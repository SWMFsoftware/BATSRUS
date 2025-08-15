!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModFaceBoundary

  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest, iVarTest, iProc
  use ModBatsrusUtility, ONLY: stop_mpi
#ifdef _OPENACC
  use ModUtilities, ONLY: norm2
#endif
  use ModMultiFluid
  use ModAdvance,    ONLY: nSpecies, nIonDensity
  use ModNumConst,   ONLY: cDegToRad
  use ModIeCoupling, ONLY: UseCpcpBc, Rho0Cpcp_I, RhoPerCpcp_I, RhoCpcp_I
  use ModBuffer,     ONLY: get_from_spher_buffer_grid
  use ModPhysics, ONLY: set_radial_state
  use ModMain

  implicit none

  SAVE

  private ! except

  ! Public methods
  public :: set_face_boundary
  public :: read_face_boundary_param

  ! Set B1_radial_ghost = B1rCoef * B1_radial_true at the inner boundary
  real, public:: B1rCoef = -1.0
  !$acc declare create( B1rCoef )

  ! Young boundary associated variables
  real,    public:: RatioOH    = 0.25
  logical, public:: UseYoungBc = .false.
  real,    public:: F107Young  = 150.0

  ! The lower bound of pe/p at inner boundary when the electron
  ! pressure equation is used.
  ! The default 1.0 / 7.8 is an empirical value from RCM, this value
  ! is also used in ModImCoupling to change the electron pressure
  real,    public :: RatioPe2P = 1.0 / 7.8

  ! Local variables

  ! Values for configuring empirical ionospheric outflow boundary conditions:
  real :: FluxAlpha=2.142E7, FluxBeta=1.265, OutflowVelocity = -1.0

  ! Polar boundary conditions are applied above this latitude only
  real :: PolarLatitude = 0.0, PolarTheta = 90.0*cDegToRad

contains
  !============================================================================
  subroutine read_face_boundary_param(NameCommand)

    use ModReadParam,  ONLY: read_var
    use ModMain,       ONLY: UseBody2, TypeFaceBc_I, body1_, body2_
    use ModMultiFluid, ONLY: nIonFluid, nFluid
    use ModPhysics,    ONLY: PolarNDim_I, PolarTDim_I, PolarUDim_I

    character(len=*), intent(in):: NameCommand

    integer:: iDensity, iFluid

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_face_boundary_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(NameCommand)
    case("#INNERBOUNDARY")
       call read_var('TypeBcInner',TypeFaceBc_I(body1_))
       if(UseBody2) call read_var('TypeBcBody2',TypeFaceBc_I(body2_))

    case("#INNERBCPE")
       call read_var('RatioPe2P', RatioPe2P)

    case("#OUTFLOWCRITERIA")
       call read_var('OutflowVelocity', OutflowVelocity)
       call read_var('FluxAlpha',       FluxAlpha)
       call read_var('FluxBeta',        FluxBeta)

    case("#POLARBOUNDARY")
       do iFluid = 1, nFluid
          call read_var('PolarNDim',  PolarNDim_I(iFluid))
          call read_var('PolarTDim',  PolarTDim_I(iFluid))
          call read_var('PolarUDim',  PolarUDim_I(iFluid))
       end do
       call read_var('PolarLatitude', PolarLatitude)
       PolarTheta = (90 - PolarLatitude)*cDegToRad

    case("#CPCPBOUNDARY")
       call read_var('UseCpcpBc', UseCpcpBc)
       if(UseCpcpBc)then
          do iDensity = 1, nIonDensity
             call read_var('Rho0Cpcp',   Rho0Cpcp_I(iDensity))
             call read_var('RhoPerCpcp', RhoPerCpcp_I(iDensity))
          end do
       end if
       !$acc update device(UseCpcpBc)

    case("#YOUNGBOUNDARY")
       call read_var('UseYoungBc', UseYoungBc)
       if(UseYoungBc) then
          call read_var('YoungF107', F107young)
       end if
       if (iProc == 0) &
            write(*,*) "Young et al IBC activated, F10.7=", F107Young

    case("#MAGNETICINNERBOUNDARY")
       call read_var('B1rCoef', B1rCoef)
       !$acc update device(B1rCoef)
    case default
       call stop_mpi(NameSub//': unknown command = '//NameCommand)
    end select

    call test_stop(NameSub, DoTest)
  end subroutine read_face_boundary_param
  !============================================================================
  subroutine set_face_boundary(iBlock, TimeBcIn, DoResChangeOnlyIn)

    use ModSize, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK
    use ModAdvance, ONLY: LeftState_VX, LeftState_VY, LeftState_VZ, &
         RightState_VX, RightState_VY, RightState_VZ
    use BATL_lib, ONLY: Used_GB
    use ModBoundaryGeometry, ONLY: iBoundary_GB, domain_

    integer, intent(in) :: iBlock
    real,    intent(in) :: TimeBcIn
    logical, intent(in) :: DoResChangeOnlyIn

    type(FaceBCType) :: FBC

    logical :: IsBodyCell_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_face_boundary'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    call timing_start(NameSub)

    ! set variables in module
    FBC%TimeBc          = TimeBcIn
    FBC%DoResChangeOnly = DoResChangeOnlyIn
    FBC%iBlockBc        = iBlock

    if(DoTest)call write_face_state('Initial')

    ! This call may be needed for moving bodies, but not in general
    ! !! call set_boundary_cells(iBlockBc)

    IsBodyCell_G(:,:,:) = &
         .not.(iBoundary_GB(:,:,:,FBC%iBlockBc) == domain_)

    call set_face_bc(FBC, IsBodyCell_G, Used_GB(:,:,:,FBC%iBlockBc) )

    if(DoTest)call write_face_state('Final')

    call timing_stop(NameSub)

    call test_stop(NameSub, DoTest, iBlock)
  contains
    !==========================================================================
    subroutine write_face_state(String)
      character(len=*), intent(in):: String

      !------------------------------------------------------------------------
      write(*,*) NameSub,' ',String,' face states:'
      write(*,*) 'VarL_x, VarR_x(iTest)  =',&
           LeftState_VX(iVarTest,  iTest, jTest, kTest),  &
           RightState_VX(iVarTest, iTest, jTest, kTest)
      write(*,*) 'VarL_x, VarR_x(iTest+1)=',&
           LeftState_VX(iVarTest,  iTest+1, jTest, kTest), &
           RightState_VX(iVarTest, iTest+1, jTest, kTest)
      write(*,*) 'VarL_y, VarR_y(jTest)  =',&
           LeftState_VY(iVarTest,  iTest, jTest, kTest),  &
           RightState_VY(iVarTest, iTest, jTest, kTest)
      write(*,*) 'VarL_y, VarR_y(jTest+1)=',&
           LeftState_VY(iVarTest,  iTest, jTest+1, kTest), &
           RightState_VY(iVarTest, iTest, jTest+1, kTest)
      write(*,*) 'VarL_z, VarR_z(kTest)  =',&
           LeftState_VZ(iVarTest,  iTest, jTest, kTest), &
           RightState_VZ(iVarTest, iTest, jTest, kTest)
      write(*,*) 'VarL_z, VarR_z(kTest+1)=',&
           LeftState_VZ(iVarTest,  iTest, jTest, kTest+1), &
           RightState_VZ(iVarTest, iTest, jTest, kTest+1)

    end subroutine write_face_state
    !==========================================================================

  end subroutine set_face_boundary
  !============================================================================
  subroutine set_face_bc(FBC, IsBodyCell_G, IsTrueCell_G)

    use ModB0,         ONLY: B0_DX, B0_DY, B0_DZ
    use ModAdvance,    ONLY: UseAnisoPressure, UseElectronPressure, &
         LeftState_VX, LeftState_VY, LeftState_VZ,    &
         RightState_VX, RightState_VY, RightState_VZ, &
         UseAnisoPe, UseMultiSpecies
    use ModParallel,   ONLY: DiLevel_EB
    use ModNumConst
    use ModPhysics,    ONLY: PolarRho_I, PolarU_I, PolarP_I, BodyNDim_I, &
         Io2No_V, No2Si_V, UnitRho_, UnitElectric_, UnitX_, BodyNSpeciesDim_I
    use ModSolarwind,  ONLY: get_solar_wind_point
    use ModIeCoupling, ONLY: logvar_ionosphere, calc_inner_bc_velocity

    use CON_axes,      ONLY: transform_matrix
    use BATL_lib,      ONLY: Xyz_DGB

    type(FaceBCType), intent(inout):: FBC
    logical, dimension(MinI:MaxI,MinJ:MaxJ,MinK:MaxK), intent(in):: &
         IsBodyCell_G, IsTrueCell_G

    integer :: i,j,k

    ! Variables used for polar wind boundary condition
    real :: GmToSmg_DD(3,3), CoordSm_D(3), Cos2PolarTheta

    ! Variables for Young et al variable mass density:
    real :: FracH, FracO

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_face_bc'
    !--------------------------------------------------------------------------
    associate( iSide => FBC%iSide, DoResChangeOnly => FBC%DoResChangeOnly, &
         iBlockBc => FBC%iBlockBc )

      call test_start(NameSub, DoTest, iBlockBc)

      if(TypeFaceBc_I(body1_) == 'polarwind') then
         GmToSmg_DD = transform_matrix(tSimulation, TypeCoordSystem, 'SMG')
         Cos2PolarTheta = cos(PolarTheta)**2
      end if

      ! Use Young et al. 1982 empirical relationship to set
      ! inner boundary density based on expected composition.
      if(UseYoungBc .and. UseIe)then
         ! Use species fractions to obtain the total mass density.
         if (UseMultiSpecies) then
            if (nIonDensity /= 2) call stop_mpi(NameSub// &
                 ': MUST be two species for Young BC.')

            if (UseCpcpBc) then
               ! assuming the first species/fluid is H+ and is determined by
               ! #CPCPBOUNDARY
               RhoCpcp_I(1) = Io2No_V(UnitRho_)*(Rho0Cpcp_I(1)           &
                    + RhoPerCpcp_I(1)*0.5*( logvar_ionosphere('cpcpn')   &
                    + logvar_ionosphere('cpcps') )                       &
                    * (No2Si_V(UnitElectric_)*No2Si_V(UnitX_))/1000.0)

               ! THe first species/fluid is H+, so the number density is the
               ! same as mass density in NO units. assuming the second
               ! species/fluid is O+, Mass is taken to be 16.
               ! use nIonDensity instead of 2 to avoid index out of range.
               RhoCpcp_I(nIonDensity) = RhoCpcp_I(1)*RatioOH*16
            else
               ! assuming the first species/fluid is H+ and determined by #BODY
               RhoCpcp_I(1) = Io2No_V(UnitRho_)*BodyNSpeciesDim_I(1)

               ! assuming the 2nd species/fluid is O+, Mass is taken to be 16
               ! use nIonDensity instead of 2 to avoid index out of range
               RhoCpcp_I(nIonDensity) = &
                    Io2No_V(UnitRho_)*BodyNSpeciesDim_I(1)*RatioOH*16
            end if
         else if (UseMultiIon) then
            if (IsMhd) call stop_mpi(NameSub//': no total fluid is supported!')
            if (UseCpcpBc) call stop_mpi(NameSub// &
                 ': CPCP should not be used '//&
                 'in combination with Young in multifluid ')
            if (nIonFluid /= 2) call stop_mpi(NameSub// &
                 ': MUST be two fluids for Young BC.')

            RhoCpcp_I(1)         = BodyNDim_I(1)*Io2No_V(UnitRho_)
            RhoCpcp_I(nIonFluid) = BodyNDim_I(1)*RatioOH*16*Io2No_V(UnitRho_)
         else
            ! Get fraction of total for H+ and O+.  Combine He+ with H+ as it
            ! is both light and very minor.
            FracH = 1.0 / (1.0 + RatioOH)
            FracO = RatioOH  * FracH

            ! fixed H+ density
            RhoCpcp_I = Io2No_V(UnitRho_)*BodyNDim_I(1)*(1+16*RatioOH)
         end if

      endif

      ! Apply face boundary conditions as required.
      FBC%B0Face_D = 0.0

      do k = kMinFace, kMaxFace
         do j = jMinFace, jMaxFace
            do i = 1, nIFace
               ! Apply BCs at X-direction faces as necessary.
               !         NUMBERING!
               !
               !     C     F     C  B  F     C
               !     +     |     +  !  |     +
               !
               !     i-1   i     i     i+1   i+1
               !
               if (IsTrueCell_G(i-1,j,k) .and. &
                    IsBodyCell_G(i,j,k) .and. &
                    (.not.DoResChangeOnly .or. &
                    ((i == nIFace .and. DiLevel_EB(2,iBlockBc)==+1) .or. &
                    (i == 1       .and. DiLevel_EB(1,iBlockBc)==+1)) )) then

                  iSide = 2

                  FBC%FaceCoords_D = 0.5*(Xyz_DGB(:,i-1,j,k,iBlockBc) &
                       +              Xyz_DGB(:,i  ,j,k,iBlockBc))

                  if(UseB0)FBC%B0Face_D = B0_DX(:,i,j,k)

                  FBC%VarsTrueFace_V= LeftState_VX(:,i,j,k)

                  call set_face(i-1,j,k,i,j,k)

                  RightState_VX(:,i,j,k) = FBC%VarsGhostFace_V

               end if

               if (IsTrueCell_G(i,j,k) .and. &
                    IsBodyCell_G(i-1,j,k)  .and. &
                    (.not.DoResChangeOnly .or. &
                    (i == 1         .and. DiLevel_EB(1,iBlockBc)==+1) .or. &
                    (i == nIFace    .and. DiLevel_EB(2,iBlockBc)==+1)  )) then

                  iSide = 1

                  FBC%FaceCoords_D = 0.5*(Xyz_DGB(:,i-1,j,k,iBlockBc) &
                       +              Xyz_DGB(:,i  ,j,k,iBlockBc))

                  if(UseB0)FBC%B0Face_D = B0_DX(:,i,j,k)

                  FBC%VarsTrueFace_V = RightState_VX(:,i,j,k)

                  call set_face(i,j,k,i-1,j,k)

                  LeftState_VX(:,i,j,k) = FBC%VarsGhostFace_V
               end if
            end do ! end i loop
         end do ! end j loop
      end do ! end k loop

      if(nDim == 1) RETURN
      do k = kMinFace, kMaxFace
         do j = 1 , nJFace
            do i = iMinFace, iMaxFace
               ! Apply BCs at Y-direction faces as necessary.
               if (IsTrueCell_G(i,j-1,k) .and. &
                    IsBodyCell_G(i,j,k)  .and. &
                    ( .not.DoResChangeOnly .or. &
                    (j == nJFace .and. DiLevel_EB(4,iBlockBc)==+1) .or. &
                    (j == 1      .and. DiLevel_EB(3,iBlockBc)==+1) )) then

                  iSide = 4

                  FBC%FaceCoords_D = 0.5*(Xyz_DGB(:,i,j-1,k,iBlockBc) &
                       +              Xyz_DGB(:,i,j  ,k,iBlockBc))

                  if(UseB0)FBC%B0Face_D = B0_DY(:,i,j,k)

                  FBC%VarsTrueFace_V = LeftState_VY(:,i,j,k)

                  call set_face(i,j-1,k,i,j,k)

                  RightState_VY(:,i,j,k) = FBC%VarsGhostFace_V
               end if

               if (IsTrueCell_G(i,j,k) .and. &
                    IsBodyCell_G(i,j-1,k)  .and. &
                    (.not.DoResChangeOnly .or. &
                    (j ==1       .and. DiLevel_EB(3,iBlockBc)==+1) .or. &
                    (j == nJFace .and. DiLevel_EB(4,iBlockBc)==+1) )) then

                  iSide = 3

                  FBC%FaceCoords_D = 0.5*(Xyz_DGB(:,i,j-1,k,iBlockBc) &
                       +              Xyz_DGB(:,i,j  ,k,iBlockBc))

                  if(UseB0)FBC%B0Face_D = B0_DY(:,i,j,k)

                  FBC%VarsTrueFace_V = RightState_VY(:,i,j,k)

                  call set_face(i,j,k,i,j-1,k)

                  LeftState_VY(:,i,j,k) = FBC%VarsGhostFace_V
               end if
            end do ! end j loop
         end do ! end i loop
      end do ! end k loop

      if(nDim == 2) RETURN
      do k = 1, nKFace
         do j = jMinFace, jMaxFace
            do i = iMinFace, iMaxFace
               ! Apply BCs at Z-direction faces as necessary.
               if (IsTrueCell_G(i,j,k-1) .and. &
                    IsBodyCell_G(i,j,k) .and. &
                    (.not.DoResChangeOnly .or. &
                    (k == nKFace .and. DiLevel_EB(6,iBlockBc)==+1) .or. &
                    (k == 1      .and. DiLevel_EB(5,iBlockBc)==+1)) ) then

                  iSide = 6

                  FBC%FaceCoords_D = 0.5*(Xyz_DGB(:,i,j,k-1,iBlockBc) &
                       +              Xyz_DGB(:,i,j,k  ,iBlockBc))

                  if(UseB0)FBC%B0Face_D = B0_DZ(:,i,j,k)

                  FBC%VarsTrueFace_V =  LeftState_VZ(:,i,j,k)

                  call set_face(i,j,k-1,i,j,k)

                  RightState_VZ(:,i,j,k) = FBC%VarsGhostFace_V
               end if

               if (IsTrueCell_G(i,j,k).and. &
                    IsBodyCell_G(i,j,k-1).and. &
                    (.not.DoResChangeOnly .or. &
                    (k == 1      .and. DiLevel_EB(5,iBlockBc)==+1) .or. &
                    (k == nKFace .and. DiLevel_EB(6,iBlockBc)==+1))  ) then

                  iSide = 5

                  FBC%FaceCoords_D = 0.5*(Xyz_DGB(:,i,j,k-1,iBlockBc) &
                       +              Xyz_DGB(:,i,j,k  ,iBlockBc))

                  if(UseB0)FBC%B0Face_D = B0_DZ(:,i,j,k)

                  FBC%VarsTrueFace_V =  RightState_VZ(:,i,j,k)

                  call set_face(i,j,k,i,j,k-1)

                  LeftState_VZ(:,i,j,k) = FBC%VarsGhostFace_V

               end if
            end do ! end i loop
         end do ! end j loop
      end do ! end k loop

      call test_stop(NameSub, DoTest, iBlockBc)
    end associate
  contains
    !==========================================================================
    subroutine set_face(iTrue, jTrue, kTrue, iGhost, jGhost, kGhost)

      use ModPhysics, ONLY: xBody2, yBody2, zBody2, vBody2_D, OmegaBody_D
      use ModAdvance, ONLY: UseMultiSpecies
      use ModPhysics, ONLY: FaceState_VI, Si2No_V, No2Si_V, UnitN_, &
           UnitU_, UnitTemperature_, UnitJ_, UnitPoynting_, &
           UseOutflowPressure, pOutflow
      use ModCurrent, ONLY: get_point_data
      use ModMain
      use ModMultiFluid
      use CON_planet_field, ONLY: get_planet_field, map_planet_field
      use ModConst,   ONLY: cElectronCharge, cBoltzmann,cProtonMass
      use ModPlanetConst, ONLY: Earth_, rPlanet_I
      use ModUtilities
      use ModBoundaryGeometry, ONLY: iBoundary_GB
      use ModNumConst, ONLY: cTwoPi
      use ModIeCoupling, ONLY: get_inner_bc_jouleheating
      use ModCoordTransform, ONLY: cross_product
      use BATL_lib, ONLY: nDim, IsCartesianGrid, FaceNormal_DDFB, CellFace_DFB

      ! indexes of the true and ghost cells on the two sides of the face
      integer, intent(in):: iTrue, jTrue, kTrue, iGhost, jGhost, kGhost

      real, parameter:: DensityJumpLimit=0.1
      real, parameter:: LatitudeCap = 55.0

      real:: uRot_D(MaxDim), uIono_D(MaxDim)
      real:: FaceState_V(nVar), State_V(Bx_:nVar+3)
      real:: bDotR, Brefl_D(MaxDim), Borig_D(MaxDim)
      real:: Ub_V(2), b, b1,b4,bFace_D(3), JouleHeating, FluxIono, FluxPw
      real:: bUnit_D(3), GseToGeo_D(3), XyzMap_D(3), SmgFaceCoords_D(3), &
           GeoFaceCoords_D(3)
      logical:: IsPolarFace
      real:: SinLatitudeCap, zCap, eCap, ePar, &
           TheTmp,DtTmp,DaTmp, Cosx, Jlocal_D(3), Jpar
      integer:: iHemisphere
      integer :: iIonSecond
      integer :: iFluid

      ! Variables for the absorbing BC
      real:: UdotR, r2Inv

      ! Variables for reflectall boundary
      integer:: iDir
      real:: Normal_D(MaxDim)

      character(len=*), parameter:: NameSubSub = 'set_face'
      !------------------------------------------------------------------------
      associate( iBoundary => FBC%iBoundary, TypeBc => FBC%TypeBc, &
           iFace => FBC%iFace, jFace => FBC%jFace, kFace => FBC%kFace, &
           TimeBc => FBC%TimeBc, iBlockBc => FBC%iBlockBc, &
           iSide => FBC%iSide)

        iBoundary = iBoundary_GB(iGhost,jGhost,kGhost,iBlockBc)
        TypeBc = TypeFaceBc_I(iBoundary)

        ! User defined boundary conditions
        if( FBC%TypeBc(1:4)=='user' )then
           iFace = i; jFace = j; kFace = k
           call user_set_face_boundary(FBC)
           RETURN
        end if

        if(iBoundary==body2_)then
           FBC%FaceCoords_D(x_)= FBC%FaceCoords_D(x_) - xBody2
           FBC%FaceCoords_D(y_)= FBC%FaceCoords_D(y_) - yBody2
           FBC%FaceCoords_D(z_)= FBC%FaceCoords_D(z_) - zBody2
        end if

        ! Default fixed/initial state for this boundary
        FaceState_V = FaceState_VI(:, iBoundary)

        select case(TypeBc)
        case('linetied','ionospherefloat')
           FBC%VarsGhostFace_V        =  FBC%VarsTrueFace_V
           FBC%VarsGhostFace_V(iUx_I) = -FBC%VarsTrueFace_V(iUx_I)
           FBC%VarsGhostFace_V(iUy_I) = -FBC%VarsTrueFace_V(iUy_I)
           FBC%VarsGhostFace_V(iUz_I) = -FBC%VarsTrueFace_V(iUz_I)

        case('radialstate')
           call set_radial_state(FBC%FaceCoords_D, FBC%VarsGhostFace_V)

        case('float')
           FBC%VarsGhostFace_V = FBC%VarsTrueFace_V

        case('outflow')
           FBC%VarsGhostFace_V = FBC%VarsTrueFace_V
           if(UseOutflowPressure) FBC%VarsGhostFace_V(p_) = pOutflow

        case('fixedB1')
           FBC%VarsGhostFace_V = FaceState_V

        case('zeroB1')
           FBC%VarsGhostFace_V = FBC%VarsTrueFace_V
           FBC%VarsGhostFace_V(Bx_:Bz_) = -FBC%VarsTrueFace_V(Bx_:Bz_)

        case('fixed')
           FBC%VarsGhostFace_V = FaceState_V
           FBC%VarsGhostFace_V(Bx_:Bz_) = &
                FBC%VarsGhostFace_V(Bx_:Bz_) - FBC%B0Face_D

        case('inflow','vary')

           call get_solar_wind_point(TimeBc, FBC%FaceCoords_D, &
                FBC%VarsGhostFace_V)
           FBC%VarsGhostFace_V(Bx_:Bz_) = FBC%VarsGhostFace_V(Bx_:Bz_) &
                - FBC%B0Face_D

        case('reflect','reflectb','reflectall')
           ! reflect the normal component of B1 (reflect/reflectall)
           ! or full B (reflectb)
           ! reflect the normal component of the velocities for reflectall
           ! reflect the full velocity vectors for reflect and reflectb

           ! Apply floating condition on densities and pressures
           FBC%VarsGhostFace_V = FBC%VarsTrueFace_V

           if(UseB)then
              Borig_D = FBC%VarsTrueFace_V(Bx_:Bz_)
              if(TypeBc == 'reflectb') Borig_D = Borig_D + FBC%B0Face_D

              select case(iBoundary)
              case(body1_, body2_)
                 bDotR   = 2*sum(Borig_D*FBC%FaceCoords_D) &
                      /sum(FBC%FaceCoords_D**2)
                 Brefl_D = FBC%FaceCoords_D*bDotR
              case default
                 if(IsCartesianGrid)then
                    select case(iSide)
                    case(1, 2)
                       Brefl_D = [ 2*Borig_D(x_), 0.0, 0.0 ]
                    case(3, 4)
                       Brefl_D = [ 0.0, 2*Borig_D(y_), 0.0 ]
                    case(5, 6)
                       Brefl_D = [ 0.0, 0.0, 2*Borig_D(z_) ]
                    end select
                 else
                    iDir = (iSide+1)/2
                    Normal_D = 0.0
                    Normal_D(1:nDim) = FaceNormal_DDFB(:,iDir,i,j,k,iBlockBc) &
                         / max(CellFace_DFB(iDir,i,j,k,iBlockBc), 1e-30)
                    Brefl_D = 2*sum(Normal_D*Borig_D)*Normal_D
                 end if
              end select
              ! Reflect B1 or full B
              FBC%VarsGhostFace_V(Bx_:Bz_) = FBC%VarsTrueFace_V(Bx_:Bz_) &
                   - BRefl_D
           end if

           if(TypeBc == 'reflectall')then
              ! Reflect the normal component of the velocity
              if(IsCartesianGrid)then
                 select case(iSide)
                 case(1, 2)
                    FBC%VarsGhostFace_V(iUx_I) = -FBC%VarsGhostFace_V(iUx_I)
                 case(3, 4)
                    FBC%VarsGhostFace_V(iUy_I) = -FBC%VarsGhostFace_V(iUy_I)
                 case(5, 6)
                    FBC%VarsGhostFace_V(iUz_I) = -FBC%VarsGhostFace_V(iUz_I)
                 end select
              else
                 iDir = (iSide+1)/2
                 Normal_D = 0.0
                 Normal_D(1:nDim) = FaceNormal_DDFB(:,iDir,i,j,k,iBlockBc) &
                      / max(CellFace_DFB(iDir,i,j,k,iBlockBc), 1e-30)
                 do iFluid = 1, nFluid
                    iUx = iUx_I(iFluid); iUz = iUz_I(iFluid)
                    FBC%VarsGhostFace_V(iUx:iUz) = FBC%VarsTrueFace_V(iUx:iUz)&
                         - 2*sum(FBC%VarsTrueFace_V(iUx:iUz)*Normal_D)*Normal_D
                 end do
              end if
           else
              ! Reflect all components of velocities (linetied)
              FBC%VarsGhostFace_V(iUx_I) = -FBC%VarsGhostFace_V(iUx_I)
              FBC%VarsGhostFace_V(iUy_I) = -FBC%VarsGhostFace_V(iUy_I)
              FBC%VarsGhostFace_V(iUz_I) = -FBC%VarsGhostFace_V(iUz_I)
           end if
        case('ionosphere', 'polarwind','ionosphereoutflow')

           ! Ionosphere type boundary conditions

           ! By default apply floating condition
           FBC%VarsGhostFace_V =  FBC%VarsTrueFace_V

           ! Use body densities but limit jump
           ! Pressure gets set too. It will be overwritten below
           where(DefaultState_V(1:nVar) > 0.0)
              FBC%VarsGhostFace_V = FBC%VarsTrueFace_V + &
                   sign(1.0, FaceState_V - FBC%VarsTrueFace_V)*   &
                   min( abs(FaceState_V - FBC%VarsTrueFace_V)     &
                   ,    DensityJumpLimit*FBC%VarsTrueFace_V   )
           end where

           ! Apply CPCP dependent density if required
           if( (UseYoungBc .or. UseCpcpBc) .and. UseIe)then
              if(UseMultiSpecies)then
                 FBC%VarsGhostFace_V(SpeciesFirst_:SpeciesLast_) = &
                      RhoCpcp_I(1:nSpecies)
                 FBC%VarsGhostFace_V(Rho_) = sum(RhoCpcp_I(1:nSpecies))
              else
                 FBC%VarsGhostFace_V(iRhoIon_I) = RhoCpcp_I(1:nIonFluid)
              end if
           end if

           ! Set pressures, including electron pressure, to float.
           FBC%VarsGhostFace_V(iP_I) = FBC%VarsTrueFace_V(iP_I)
           if(UseAnisoPressure) FBC%VarsGhostFace_V(iPparIon_I) = &
                FBC%VarsTrueFace_V(iPparIon_I)
           if(UseElectronPressure) FBC%VarsGhostFace_V(Pe_) = &
                max(FBC%VarsTrueFace_V(Pe_), &
                RatioPe2P*FBC%VarsTrueFace_V(iP_I(1)))
           if(UseAnisoPe) FBC%VarsGhostFace_V(Pepar_)      = &
                FBC%VarsTrueFace_V(Pepar_)

           ! Change sign for velocities (plasma frozen into dipole field)
           FBC%VarsGhostFace_V(iUx_I) = -FBC%VarsTrueFace_V(iUx_I)
           FBC%VarsGhostFace_V(iUy_I) = -FBC%VarsTrueFace_V(iUy_I)
           FBC%VarsGhostFace_V(iUz_I) = -FBC%VarsTrueFace_V(iUz_I)

           !---------------------------------------------------
           ! Ionosphere outflow in multifluids  --- Yiqun 2008
           ! This should be a subroutine !!!
           !---------------------------------------------------

           if(TypeBc == 'ionosphereoutflow')then

              if(.not. (UseIe .and. UseMultiIon)) call stop_mpi( &
                   'ionosphereoutflow should have IE coupled and multifluid')

              iIonSecond = min(2, nIonFluid)

              if (TypeCoordSystem /= 'SMG') then
                 SmgFaceCoords_D = matmul(transform_matrix(TimeBc, &
                      TypeCoordSystem, 'SMG'), FBC%FaceCoords_D)
              else
                 SmgFaceCoords_D = FBC%FaceCoords_D
              endif

              SinLatitudeCap = sin(LatitudeCap * cDegToRad)
              zCap = norm2(SmgFaceCoords_D)*SinLatitudeCap

              if(abs(SmgFaceCoords_D(z_)) > zCap)then
                 ! for the polar region

                 if (TypeCoordSystem /= 'GEO') then
                    GeoFaceCoords_D = matmul(transform_matrix(TimeBc, &
                         TypeCoordSystem, 'GEO'), FBC%FaceCoords_D)
                 else
                    GeoFaceCoords_D = FBC%FaceCoords_D
                 endif
                 GseToGeo_D = matmul(transform_matrix(TimeBc,'GSE','GEO'),&
                      [0,0,1])

                 ! For the cap region (refer to Tom Moore 2003?)
                 ! Get the Op flux from IE calculation
                 ! Get the Hp flux from fluxpw,
                 ! which is constant for certain solar zenith angle
                 ! Fix the velocities(V), thermal energies
                 ! Get the densities(rho), thermal pressure(P)

                 ! get the magnetic field
                 call get_planet_field(TimeBc, FBC%FaceCoords_D,&
                      TypeCoordSystem//'NORM', bFace_D)
                 b =  norm2(bFace_D)
                 bUnit_D = bFace_D / B

                 ! get the magnetic field at 4000km = 4e6m
                 call map_planet_field(TimeBc, FBC%FaceCoords_D, &
                      TypeCoordSystem//'NORM', &
                      (4e6 + rPlanet_I(Earth_))/rPlanet_I(Earth_), &
                      XyzMap_D, iHemisphere)
                 call get_planet_field(TimeBc, XyzMap_D, &
                      TypeCoordSystem//'NORM', bFace_D)

                 b4 =  norm2(bFace_D)

                 ! get the joule heating mapped from the ionosphere
                 ! (already in nomalized unit)
                 call get_inner_bc_jouleheating(SmgFaceCoords_D, &
                      JouleHeating)

                 ! get the O+ flux based on Strangeway's formula,
                 ! and scale it
                 FluxIono = FluxAlpha*(JouleHeating*No2Si_V(UnitPoynting_)&
                      * 1.0e3)**FluxBeta * 1.0e4 &
                      * Si2No_V(UnitU_) * Si2No_V(UnitN_) * (b4/b)**0.265

                 ! thermal energy = 0.1 + 1.6 * S^1.26
                 ! (S is joule heating in mW/m^2 at inner boundary)
                 ! to specify the O+ temperature
                 eCap = 0.1 + 9.2 * &
                      ((b4/b)* JouleHeating*No2Si_V(UnitPoynting_) &
                      * 1.0e3)**0.35    ! eV

                 ! Get the field aligned current at this location,
                 ! so comes the parallel energy
                 call get_point_data(1.0, FBC%FaceCoords_D, 1, nBlock, Bx_,&
                      nVar+3, State_V)

                 Jlocal_D = State_V(nVar+1:nVar+3)
                 Jpar = sum(bUnit_D * Jlocal_D)  ! in normalized unit

                 ! parallel energy
                 ! (ePar = eV=e*(1500[V/mmA/m^2] * (J//-0.33)^2 [mmA/m^2]))
                 if(abs(Jpar*No2Si_V(UnitJ_))*1.0e6 > 0.33)then
                    ePar = 1500 * (abs(Jpar*No2Si_V(UnitJ_))*1.0e6 &
                         - 0.33)**2 ! eV
                 else
                    ePar = 0.
                 end if

                 if(OutflowVelocity < 0)then
                    ! If outflowvelocity <0,
                    ! get the velocity along B, superpose the parallel
                    ! velocity and the thermal velocity
                    Ub_V(1) = (sqrt(2 * (ePar + eCap) * cElectronCharge / &
                         (MassFluid_I(1)*cProtonMass))) &
                         * Si2No_V(UnitU_)
                    Ub_V(2) = (sqrt(2 * (ePar + eCap) * cElectronCharge / &
                         (MassFluid_I(iIonSecond)*cProtonMass))) &
                         * Si2No_V(UnitU_)
                 else
                    ! .OR. Pick the constant velocities and thermal energy
                    Ub_V(2) = OutflowVelocity*Io2No_V(UnitU_)
                    Ub_V(1) = OutflowVelocity*Io2No_V(UnitU_)
                 end if

                 ! SZA x is determind by
                 ! cosx = sin(the)sin(da)+cos(the)cos(da)cos(dt)
                 ! where, the is the latitude, da is solar declination(
                 ! angle between solar ray and equatorial plane),
                 ! dt is local time angle
                 TheTmp = asin(GeoFaceCoords_D(z_)/ &
                      norm2(GeoFaceCoords_D))      ! latitutde
                 DaTmp = acos(GseToGeo_D(z_))               ! declination
                 DtTmp = acos(SmgFaceCoords_D(x_)/ &
                      sqrt(SmgFaceCoords_D(x_)**2 + &
                      SmgFaceCoords_D(y_)**2))        ! local time angle

                 if(SmgFaceCoords_D(y_)<0.0) DtTmp =  cTwoPi - DtTmp
                 Cosx = sin(TheTmp)*sin(DaTmp) + &
                      cos(TheTmp)*cos(DaTmp)*cos(DtTmp)

                 ! get the magnetic field at 1000km = 1e6m
                 call map_planet_field(TimeBc, FBC%FaceCoords_D, &
                      TypeCoordSystem//'NORM', &
                      (1e6 + rPlanet_I(Earth_))/rPlanet_I(Earth_), &
                      XyzMap_d, iHemisphere)
                 call get_planet_field(TimeBc, XyzMap_D, &
                      TypeCoordSystem//'NORM', bFace_D)
                 b1 =  norm2(bFace_D)

                 ! get the Hp flux by mapping the flux at 1000km
                 ! into the inner boudnary
                 if (acos(Cosx)*cRadToDeg < 90 .and. &
                      acos(Cosx)*cRadToDeg > 0.) then
                    FluxPw = 2.0e8 * 1.0e4 * (b/b1) &
                         * Si2No_V(UnitU_) * Si2No_V(UnitN_)
                 elseif(acos(Cosx)*cRadToDeg < 110) then
                    FluxPw = 2.0*10.**(8-(acos(Cosx)*cRadToDeg &
                         - 90.)/20.*2.5) * 1.0e4 * (b/b1) * &
                         Si2No_V(UnitU_) * Si2No_V(UnitN_)
                 else
                    FluxPw = 2.0*10.**5.5 * 1.0e4 * (b/b1) * &
                         Si2No_V(UnitU_) * Si2No_V(UnitN_)
                 endif

                 ! get the densities
                 FBC%VarsGhostFace_V(Rho_) = &
                      FluxPw/Ub_V(1)* MassFluid_I(1)
                 FBC%VarsGhostFace_V(iRho_I(iIonSecond)) = &
                      FluxIono/Ub_V(2) * MassFluid_I(iIonSecond)

                 ! Make sure it points outward
                 if(sum(bUnit_D*FBC%FaceCoords_D) < 0.0) bUnit_D = -bUnit_D

                 FBC%VarsGhostFace_V(Ux_) = Ub_V(1)*bUnit_D(x_)
                 FBC%VarsGhostFace_V(Uy_) = Ub_V(1)*bUnit_D(y_)
                 FBC%VarsGhostFace_V(Uz_) = Ub_V(1)*bUnit_D(z_)

                 FBC%VarsGhostFace_V(iUx_I(iIonSecond))= Ub_V(2)*bUnit_D(x_)
                 FBC%VarsGhostFace_V(iUy_I(iIonSecond))= Ub_V(2)*bUnit_D(y_)
                 FBC%VarsGhostFace_V(iUz_I(iIonSecond))= Ub_V(2)*bUnit_D(z_)

                 ! get the pressure
                 FBC%VarsGhostFace_V(iP_I(iIonSecond)) = 2./3.*eCap*&
                      cElectronCharge / cBoltzmann &
                      * Si2No_V(UnitTemperature_)  &
                      * FBC%VarsGhostFace_V(iRho_I(iIonSecond)) &
                      /MassFluid_I(iIonSecond)
                 FBC%VarsGhostFace_V(p_) =  2./3.*eCap*&
                      cElectronCharge / cBoltzmann &
                      * Si2No_V(UnitTemperature_)  &
                      * FBC%VarsGhostFace_V(Rho_)/MassFluid_I(1)

              end if ! polar cap region
           end if ! ionosphereoutflow type of innerboundary

           ! Overwrite faces in the polar region either using the
           ! parameters from the #POLARBOUNDARY command or from the coupled PW
           if(TypeBc == 'polarwind')then
              CoordSm_D = matmul(GmToSmg_DD, FBC%FaceCoords_D)
              IsPolarFace = CoordSm_D(z_)**2/sum(CoordSm_D**2) > Cos2PolarTheta
           else
              IsPolarFace = .false.
           end if

           if(IsPolarFace)then
              ! polarwind type conditions
              if(UsePw)then
                 ! Get density/ies and velocity/ies from polarwind code
                 call read_pw_buffer(FBC%FaceCoords_D, nVar, &  !^CMP IF PW
                      FBC%VarsGhostFace_V)                      !^CMP IF PW
              else
                 ! Use variables set in the #POLARBOUNDARY command
                 FBC%VarsGhostFace_V(iRho_I) = PolarRho_I
                 ! Align flow with the magnetic field
                 bUnit_D = FBC%B0Face_D / norm2(FBC%B0Face_D)
                 ! Make sure it points outward
                 if(sum(bUnit_D*FBC%FaceCoords_D) < 0.0) bUnit_D = -bUnit_D
                 ! Add 2*PolarU_I to the ghost face to set 0.5*(true+ghost)
                 FBC%VarsGhostFace_V(iUx_I)  = FBC%VarsGhostFace_V(iUx_I) &
                      + 2*PolarU_I*bUnit_D(x_)
                 FBC%VarsGhostFace_V(iUy_I)  = FBC%VarsGhostFace_V(iUy_I) &
                      + 2*PolarU_I*bUnit_D(y_)
                 FBC%VarsGhostFace_V(iUz_I)  = FBC%VarsGhostFace_V(iUz_I) &
                      + 2*PolarU_I*bUnit_D(z_)
                 ! Only set pressure where PolarP_I is positive
                 where(PolarP_I > 0.0) FBC%VarsGhostFace_V(iP_I) = PolarP_I
              end if
           end if ! IsPolarFace

           ! Apply boundary condition on B (radial component of B1)
           if(B1rCoef /= 1.0)then
              ! Change B_ghost so that radial component of Bghost + Btrue = 0
              ! Brefl = (1-B1rCoef)*r*(B.r)/r^2, so Bghost = Btrue - Brefl
              Brefl_D = (1-B1rCoef)*FBC%FaceCoords_D/sum(FBC%FaceCoords_D**2) &
                   *sum(FBC%VarsTrueFace_V(Bx_:Bz_)*FBC%FaceCoords_D)
              FBC%VarsGhostFace_V(Bx_:Bz_) = FBC%VarsTrueFace_V(Bx_:Bz_) &
                   - Brefl_D
           end if

        case('absorb')
           ! for inflow float everything
           FBC%VarsGhostFace_V = FBC%VarsTrueFace_V

           ! Calculate 1/r^2
           r2Inv = 1.0/sum(FBC%FaceCoords_D**2)

           ! for outflow reflect radial velocity: uG = u - 2*(u.r)*r/r^2
           do iFluid = 1, nFluid
              iUx = iUx_I(iFluid); iUz = iUz_I(iFluid)
              UdotR = sum(FBC%VarsTrueFace_V(iUx:iUz)*FBC%FaceCoords_D)
              if(UdotR > 0.0) &
                   FBC%VarsGhostFace_V(iUx:iUz) = FBC%VarsTrueFace_V(iUx:iUz) &
                   - 2*UdotR*r2Inv*FBC%FaceCoords_D
           end do

           ! Set B1rGhost according to B1rCoef
           if(B1rCoef /= 1.0) &
                FBC%VarsGhostFace_V(Bx_:Bz_) = FBC%VarsTrueFace_V(Bx_:Bz_) &
                - (1-B1rCoef) &
                *sum(FBC%VarsTrueFace_V(Bx_:Bz_)*FBC%FaceCoords_D) &
                *r2Inv*FBC%FaceCoords_D

        case('buffergrid')
           ! REVISION: June  2011 - R. Oran - generalized.
           ! Inner boundary conditions based on coupling to another BATSRUS
           ! component through a buffer grid.

           ! Allows specifying boundary conditions for additional variables
           ! that were not passed through the buffer.
           ! Coupling flags for these options are set in CON_couple_all.f90.

           ! Note: the older case 'coronatoih' is redirected to here for
           ! backwards compatability.

           ! Get interpolated values from buffer grid:
           call get_from_spher_buffer_grid( &
                FBC%FaceCoords_D, nVar, FaceState_V)

           FBC%VarsGhostFace_V = FaceState_V
           if(UseB0)FBC%VarsGhostFace_V(Bx_:Bz_) = &
                FBC%VarsGhostFace_V(Bx_:Bz_) - FBC%B0Face_D

           ! If this component is multifluid and coupled to single-fluid,
           ! stop with an error
           if(IsFullyCoupledFluid .and. nFluid > 1) call stop_mpi( &
                NameSub//': BCs for multifluid must be specified.')

           ! Only variable associated with the main MHD plasma are passed
           ! through the buffer grid. BC's for fluids must be specified
           ! somehow.

           if (DoOhNeutralBc) then
              ! Get face BCs for neutrals in the outerheliosphere
              ! (based on M. Opher)

              ! PopI flows through the boundary

              ! PopII leaves the domain at a supersonic velocity
              ! (50km/s while for their temperature 1.E5K their C_s=30km/s)
              ! For the transient case of inward flow, use fraction of ions

              ! Pop III has the velocity and temperature of ions at inner
              ! boundary, the density is taken to be a fraction of the ions
              do iFluid = nIonFluid+1, nFluid
                 if(iFluid == nIonFluid+1 .or. sum( &
                      FBC%VarsTrueFace_V(iRhoUx_I(iFluid):iRhoUz_I(iFluid))*&
                      FBC%FaceCoords_D) <= 0.0)then
                    ! Either Pop I or inflow: float BC
                    FBC%VarsGhostFace_V(iRho_I(iFluid):iP_I(iFluid)) = &
                         FBC%VarsTrueFace_V(iRho_I(iFluid):iP_I(iFluid))
                 else
                    ! Outflow for Pop II and above
                    FBC%VarsGhostFace_V(iRho_I(iFluid)) = &
                         FBC%VarsGhostFace_V(Rho_)*RhoBcFactor_I(iFluid)
                    FBC%VarsGhostFace_V(iP_I(iFluid)) = &
                         FBC%VarsGhostFace_V(p_)*RhoBcFactor_I(iFluid)
                    FBC%VarsGhostFace_V(iRhoUx_I(iFluid):iRhoUz_I(iFluid))=&
                         FBC%VarsGhostFace_V(Ux_:Uz_)*uBcFactor_I(iFluid)
                 end if
              end do

           end if

        case('body2orbit')
           FBC%VarsGhostFace_V = FaceState_V
           ! Obsolete version, assuming that the sesond body rotates
           ! in the equatorial plane
           ! Setting velocity BCs to be the second body orbital velocity:
           FBC%VarsGhostFace_V(Ux_:Uz_) = FaceState_V(Ux_:Uz_) + &
                vBody2_D(1:1+Uz_-Ux_)
        case default
           write(*,*) NameSub,': iTrue, jTrue, kTrue, iBlockBc =', &
                iTrue, jTrue, kTrue, iBlockBc
           write(*,*) NameSub,': iGhost,jGhost,kGhost,iBoundary=',&
                iGhost, jGhost, kGhost, iBoundary
           write(*,*) NameSub,': FBC%FaceCoords_D=', FBC%FaceCoords_D
           call stop_mpi(NameSub//': incorrect TypeFaceBc_I='//TypeBc)
        end select

        if (UseIe .and. iBoundary == Body1_)then
           ! Get the E x B / B^2 velocity
           call calc_inner_bc_velocity(TimeBc, FBC%FaceCoords_D, &
                FBC%VarsTrueFace_V(Bx_:Bz_) + FBC%B0Face_D, uIono_D)

           ! Subtract the radial component of the velocity (no outflow/inflow)
           uIono_D = uIono_D &
                - FBC%FaceCoords_D * sum(FBC%FaceCoords_D*uIono_D) &
                / sum(FBC%FaceCoords_D**2)

           select case(TypeBc)
           case('reflect','reflectb','linetied','polarwind','ionosphere', &
                'ionospherefloat', 'ionosphereoutflow')
              FBC%VarsGhostFace_V(iUx_I) = 2*uIono_D(x_) &
                   + FBC%VarsGhostFace_V(iUx_I)
              FBC%VarsGhostFace_V(iUy_I) = 2*uIono_D(y_) &
                   + FBC%VarsGhostFace_V(iUy_I)
              FBC%VarsGhostFace_V(iUz_I) = 2*uIono_D(z_) &
                   + FBC%VarsGhostFace_V(iUz_I)

           case default
              call stop_mpi(NameSub// &
                   ': Coupling with IE is not compatible with TypeFaceBc_I=' &
                   //TypeBc)
           end select
        end if

        if (UseRotatingBc .and. iBoundary==Body1_) then

           ! Calculate corotation velocity uRot_D at position FaceCoords
           uRot_D = cross_product(OmegaBody_D, FBC%FaceCoords_D)

           if(DoTest .and. iTrue == iTest .and. jTrue == jTest .and. &
                kTrue == kTest) write(*,*) NameSub,' UseRotatingBc, uRot_D=', &
                UseRotatingBc, uRot_D

           select case(TypeBc)
           case('reflect','reflectb','reflectall','linetied', &
                'ionosphere','ionospherefloat','polarwind','ionosphereoutflow')
              FBC%VarsGhostFace_V(iUx_I) = 2*uRot_D(x_) &
                   + FBC%VarsGhostFace_V(iUx_I)
              FBC%VarsGhostFace_V(iUy_I) = 2*uRot_D(y_) &
                   + FBC%VarsGhostFace_V(iUy_I)
              FBC%VarsGhostFace_V(iUz_I) = 2*uRot_D(z_) &
                   + FBC%VarsGhostFace_V(iUz_I)
           case default
              call stop_mpi(NameSub// &
                   ': UseRotatingBc is not compatible with TypeBc='//TypeBc)
           end select
        end if
      end associate

    end subroutine set_face
    !==========================================================================
  end subroutine set_face_bc
  !============================================================================
end module ModFaceBoundary
!==============================================================================
