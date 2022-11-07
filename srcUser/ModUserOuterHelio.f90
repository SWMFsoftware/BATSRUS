!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModUser

  ! 04/30/2007 implementing MultiFluid
  ! 06/01/2007 correcting normalization
  ! 06/08/2007 correcting source terms
  ! 08/18/2007 Implementing 3-fluids
  ! 01/01/2008 Source terms in point-implicit form - with help of Gabor Toth
  ! 01/08/2008 all source terms for PopI implicit
  ! 01/25/2008 comments added
  ! 05/02/2008 setting initial conditions to be closer to the final solution
  ! 07/28/2008 neutral fluids
  ! 09/28/2008 source terms as McNutt
  ! 02/2011 PUI fluid
  ! 04/2011 Sources for PUI
  ! 07/2012 Use sonic Mach number to define Region 1 - Region 4 boundary
  ! 11/2017 use Solar Wind Pressure and Temp for the Mach Number for REGIONS
  !   definition for inner boundary the solar pressure is iostropic (don't have
  !   any dependence with magnetic field
  !   took off inner boundary at 30AU
  ! 02/08/2018 adding possibilty of using user boundaries for all faces
  ! 02/2018  Gabor - fixed code to work with separate multi-ion velocities
  !   Tests show that the default explicit evaluation works, while the
  !   point-implicit evaluation is not robust enough.
  ! 03/2018 Opher Merav fixed calc_source routine for multi-ion
  !   and charge neutrality while assuming cold electrons
  !   source terms can be printed in tecplot
  !   use of energy heating for PUI is optional
  ! 05/2018 refining select regions criteria for multi-ion
  ! 04/2020 Marc Kornbleuth removing total fluid; adding AMPS compatibility
  ! 07/2022 Merav Opher added conditions needed for Cold Cloud

  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest, iProc

  use ModSize,     ONLY: nI, nJ, nK
  use ModMain,       ONLY: body1_,      &
       nBlock, Unused_B, tSimulation
  use ModPhysics,    ONLY: Gamma, GammaMinus1, OmegaBody, &
       UnitX_, Io2Si_V, Si2Io_V, Si2No_V, No2Io_V, No2Si_V, Io2No_V, &
       NameTecUnit_V, NameIdlUnit_V, UnitAngle_, UnitDivB_, UnitEnergyDens_, &
       UnitJ_, UnitN_, UnitRho_, UnitU_, rBody, UnitB_, UnitP_, &
       UnitTemperature_, UnitT_, UnitRhoU_, FaceState_VI

  use ModConst,    ONLY: cAU, cProtonMass, cBoltzmann
  use ModNumConst, ONLY: cRadToDeg, cPi, cTwoPi
  use ModAdvance,  ONLY: State_VGB,Source_VC, ExtraSource_ICB, Pe_, &
       UseElectronPressure
  use ModGeometry, ONLY: Xyz_DGB, r_GB, Used_GB
  use ModVarIndexes
  use ModMultiFluid
  use ModUserEmpty,                                     &
       IMPLEMENTED1  => user_read_inputs,               &
       IMPLEMENTED2  => user_set_face_boundary,         &
       IMPLEMENTED3  => user_normalization,             &
       IMPLEMENTED4  => user_set_cell_boundary,         &
       IMPLEMENTED5  => user_set_ics,                   &
       IMPLEMENTED6  => user_initial_perturbation,      &
       IMPLEMENTED7  => user_update_states,             &
       IMPLEMENTED8  => user_action,                    &
       IMPLEMENTED10 => user_set_plot_var,              &
       IMPLEMENTED12 => user_calc_sources_expl,         &
       IMPLEMENTED14 => user_init_session

  include 'user_module.h' ! list of public methods

  ! Needed for coupling with AMPS through the C wrapper
  public:: get_charge_exchange

  real,              parameter :: VersionUserModule = 1.0
  character (len=*), parameter :: NameUserFile = "ModUserOuterHelio.f90"
  character (len=*), parameter :: NameUserModule = "Outer Heliosphere"

  ! Zieger Named indexes for fluids
  integer, parameter :: SWH_ = 1, Ion_ = 1, &
       Pu3_ = min(nFluid,2), Neu_ = min(nFluid,IonLast_+1), &
       Ne2_ = min(nFluid,IonLast_+2), Ne3_ = min(nFluid,IonLast_+3), &
       Ne4_= min(nFluid,IonLast_+4)

  logical :: UseSource_I(SWH_:Ne4_) = .true.
  logical :: UsePhotoion = .false.
  logical :: UseColdCloud = .false.
  logical :: DoInitializeCloud = .false.
  logical :: UseSrcsInHelio = .true.
  logical :: DoFixChargeExchange = .false.

  ! from 173K use 0.7
  real :: HpLimit = 0.5

  real :: OmegaSun   = 0.0  ! normalized angular speed of Sun
  real :: ParkerTilt = 0.0  ! Bphi/Br at the equator at r=rBody

  logical :: UsePu3Heating = .false.
  real :: TempPu3, FactorPu3, HeatPu3

  integer :: iTableSolarWind = -1 ! initialization is needed
  integer :: iTableChargeExchange = -1

  ! SWH variables.
  real :: &
       SwhRho = 0.0, SwhRhoDim = 0.0, &
       SwhP   = 0.0, SwhTDim   = 0.0, &
       SwhPe  = 0.0, SwhTeDim  = 0.0, &
       SwhUx  = 0.0, SwhUxDim  = 0.0, &
       SwhUy  = 0.0, SwhUyDim  = 0.0, &
       SwhUz  = 0.0, SwhUzDim  = 0.0, &
       SwhBx  = 0.0, SwhBxDim  = 0.0, &
       SwhBy  = 0.0, SwhByDim  = 0.0, &
       SwhBz  = 0.0, SwhBzDim  = 0.0

  ! VLISM variables.
  real :: &
       VliswTDim  = 0.0 , VliswTeDim  = 0.0, &
       VliswRho   = 0.0 , VliswRhoDim = 0.0, &
       VliswP     = 0.0 , VliswPDim   = 0.0, &
       VliswPe    = 0.0 , VliswPeDim  = 0.0, &
       VliswUx    = 0.0 , VliswUxDim  = 0.0, &
       VliswUy    = 0.0 , VliswUyDim  = 0.0, &
       VliswUz    = 0.0 , VliswUzDim  = 0.0, &
       VliswBx    = 0.0 , VliswBxDim  = 0.0, &
       VliswBy    = 0.0 , VliswByDim  = 0.0, &
       VliswBz    = 0.0 , VliswBzDim  = 0.0

  ! Velocity, temperature, Mach number and radius limits for the populations
  real :: TempPop1LimitDim = 1e5    ! [K]
  real :: uPop1LimitDim    = 100.0  ! [km/s]
  real :: MachPop2Limit    = 0.9
  real :: MachPop3Limit    = 1.5
  real :: MachPop4Limit    = 2.0
  real :: rPop3Limit       = 50.0   ! [AU] it is all Pop3 out to rPop3Limit
  real :: MachPUIPop3      = 0.9
  real :: MachSWPop1       = 1.2

  real :: RhoPop4LimitDim  = 0.01  ! for cold cloud

  integer :: iFluidProduced_C(nI, nJ, nK)
  !$omp threadprivate( iFluidProduced_C )

  real :: &
       Pu3Rho = 0.0, Pu3RhoDim = 0.0, &
       Pu3P   = 0.0, Pu3TDim   = 0.0, &
       Pu3Ux  = 0.0, Pu3UxDim  = 0.0 , &
       Pu3Uy  = 0.0, Pu3UyDim  = 0.0 , &
       Pu3Uz  = 0.0, Pu3UzDim  = 0.0

  ! Freeze neutrals (with user_update_states)
  logical, parameter:: DoFreezeNeutral=.false.

  ! variable to hold sources from charge-exchange with neutral fluids
  real, allocatable :: FluidSource_ICB(:,:,:,:,:)

contains
  !============================================================================
  subroutine user_read_inputs

    use ModReadParam

    character (len=100) :: NameCommand

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_read_inputs'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    do
       if(.not.read_line() ) EXIT
       if(.not.read_command(NameCommand)) CYCLE
       select case(NameCommand)

       case('#USERINPUTEND')
          if(iProc==0) write(*,*)'USERINPUTEND'
          EXIT
       case("#SOLARWINDH")
          call read_var('SwhRhoDim',SwhRhoDim)
          call read_var('SwhTDim'  ,SwhTDim)
          call read_var('SwhUxDim' ,SwhUxDim)
          call read_var('SwhUyDim' ,SwhUyDim)
          call read_var('SwhUzDim' ,SwhUzDim)
          call read_var('SwhBxDim' ,SwhBxDim)
          call read_var('SwhByDim' ,SwhByDim)
          call read_var('SwhBzDim' ,SwhBzDim)
          if(UseElectronPressure)then
             call read_var('SwhTeDim'  ,SwhTeDim)
          end if
       case("#GLOBALHELIOSPHERE")
          call read_var('Rbody',Rbody)
       case("#VLISW")
          call read_var('VliswRhoDim' ,VliswRhoDim)
          call read_var('VliswTDim'  ,VliswTDim)
          call read_var('VliswUxDim' ,VliswUxDim)
          call read_var('VliswUyDim' ,VliswUyDim)
          call read_var('VliswUzDim' ,VliswUzDim)
          call read_var('VliswBxDim' ,VliswBxDim)
          call read_var('VliswByDim' ,VliswByDim)
          call read_var('VliswBzDim' ,VliswBzDim)
          if(UseElectronPressure)then
             call read_var('VliswTeDim' ,VliswTeDim)
          end if
       case("#PICKUPION3")
          call read_var('Pu3RhoDim',Pu3RhoDim)
          call read_var('Pu3TDim'  ,Pu3TDim)
          call read_var('Pu3UxDim' ,Pu3UxDim)
          call read_var('Pu3UyDim' ,Pu3UyDim)
          call read_var('Pu3UzDim' ,Pu3UzDim)

       case("#PU3HEATING")
          call read_var('UsePu3Heating', UsePu3Heating)
          call read_var('TempPu3', TempPu3)
          call read_var('FactorPu3', FactorPu3)

          ! This is a flag to define how many populations of Neutrals to run
       case("#SOURCES")
          ! Zieger
          if(IsMhd)then
             call read_var('UseIonSource', UseSource_I(Ion_))
             call read_var('UseNeuSource', UseSource_I(Neu_))
             call read_var('UseNe2Source', UseSource_I(Ne2_))
             call read_var('UseNe3Source', UseSource_I(Ne3_))
             call read_var('UseNe4Source', UseSource_I(Ne4_))
          else
             call read_var('UseSWHSource', UseSource_I(SWH_))
             call read_var('UsePu3Source', UseSource_I(Pu3_))
             call read_var('UseNeuSource', UseSource_I(Neu_))
             call read_var('UseNe2Source', UseSource_I(Ne2_))
             call read_var('UseNe3Source', UseSource_I(Ne3_))
             call read_var('UseNe4Source', UseSource_I(Ne4_))
          end if

          ! Chika. This is a flag to turn on photoionization
          ! If not specified in PARAM.in, it will be false
       case("#PHOTOIONIZATION")
          call read_var('UsePhotoionization', UsePhotoion)

       case("#COLDCLOUD")
          call read_var('UseColdCloud', UseColdCloud)
          call read_var('DoInitializeCloud', DoInitializeCloud)
          call read_var('UseSrcsInHelio', UseSrcsInHelio)
          call read_var('HpLimit', HpLimit)
          call read_var('RhoPop4LimitDim', RhoPop4LimitDim)

          ! Bair. This is a flag to use charge exchange formula 
          ! with the extra splitting terms.  
       case("#CHARGEEXCHANGE")
          call read_var("DoFixChargeExchange", DoFixChargeExchange)

       case("#REGIONS")
          call read_var('TempPop1LimitDim', TempPop1LimitDim)
          call read_var('uPop1LimitDim',    uPop1LimitDim)
          call read_var('MachPop2Limit',    MachPop2Limit)
          call read_var('MachPop3Limit',    MachPop3Limit)
          call read_var('rPop3Limit',       rPop3Limit)
          call read_var('MachPop4Limit',    MachPop4Limit)
          ! Zieger
          if(.not.IsMhd)then
             call read_var('MachPUIPop3',      MachPUIPop3)
             call read_var('MachSWPop1',       MachSWPop1)
          end if

       case default
          if(iProc==0) call stop_mpi( &
               'read_inputs: unrecognized command: '//NameCommand)
       end select
    end do

    call test_stop(NameSub, DoTest)
  end subroutine user_read_inputs
  !============================================================================
  subroutine user_set_face_boundary(FBC)

    use ModMain, ONLY: FaceBCType
    use ModCoordTransform, ONLY: rot_xyz_sph

    type(FaceBCType), intent(inout):: FBC

    ! local variables
    real:: xFace, yFace, zFace
    real:: SinTheta
    ! C.P. edited
    real:: Bsph_D(3), Vsph_D(3), VPUIsph_D(3)

    real :: pSolarWind,pPUI, Pmag, PmagEquator

    real :: XyzSph_DD(3,3)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_face_boundary'
    !--------------------------------------------------------------------------
    associate( VarsGhostFace_V => FBC%VarsGhostFace_V, &
         VarsTrueFace_V => FBC%VarsTrueFace_V, &
         FaceCoords_D => FBC%FaceCoords_D, &
         iBlockBc => FBC%iBlockBc, &
         iFace => FBC%iFace, jFace => FBC%jFace, kFace => FBC%kFace, &
         iBoundary => FBC%iBoundary )

      call test_start(NameSub, DoTest, iBlockBc)
      if(iBoundary /= body1_) &
           call stop_mpi(NameSub//' only inner BC is implemented!')

      ! Make sure that OmegaSun and ParkerTilt are set
      if(OmegaSun == 0.0) call set_omega_parker_tilt

      XyzSph_DD = rot_xyz_sph(FaceCoords_D)

      xFace = FaceCoords_D(1)
      yFace = FaceCoords_D(2)
      zFace = FaceCoords_D(3)

      SinTheta = sqrt((xFace**2 + YFace**2)/(xFace**2 + yFace**2 + zFace**2))

      Bsph_D(1) =  SwhBx                     ! Br
      Bsph_D(2) =  0.0                       ! Btheta
      Bsph_D(3) =  SwhBx*SinTheta*ParkerTilt

      Vsph_D    = [ SwhUx, 0.0, 0.0 ]        ! Vr, Vtheta, Vphi
      VPUIsph_D = [ Pu3Ux, 0.0, 0.0 ]

      ! Calculate pressur:e (equilibrium at a given inner boundary)
      Pmag = sum(Bsph_D**2) / 2.0

      ! magnetic pressure at the equator (actually wrong, neglects Br=SwhBx)
      PmagEquator = (SwhBx*ParkerTilt)**2/2

      ! this method guarantees equal pressure at inner boundary.
      ! Simpler splitting method likely

      ! Zieger
      if(.not.IsMhd) pPUI = Pu3P

      pSolarWind = SwhP

      ! Apply boundary conditions for ions
      VarsGhostFace_V(SWHRho_)    = SwhRho
      VarsGhostFace_V(SWHP_)      = pSolarWind ! SwhP
      VarsGhostFace_V(SWHUx_:SWHUz_) = matmul(XyzSph_DD, Vsph_D)
      VarsGhostFace_V(Bx_:Bz_) = matmul(XyzSph_DD, Bsph_D)

      ! Zieger
      if(.not.IsMhd)then
         VarsGhostFace_V(Pu3Rho_)    = Pu3Rho
         VarsGhostFace_V(Pu3P_)      = pPUI  ! Pu3P ???
         VarsGhostFace_V(Pu3Ux_:Pu3Uz_) = matmul(XyzSph_DD, VPUIsph_D)
         if(UseElectronPressure) VarsGhostFace_V(Pe_) = SwhPe
      end if

      if(UseNeutralFluid)then

         ! NeuRho is PopI; NeuIIRho is PopII and NeuIIIRho is PopIII
         !
         ! Pop I is going through the inner BCs

         ! soft boundary for Pop I-IV

         VarsGhostFace_V(NeuRho_:Ne4P_) = VarsTrueFace_V(NeuRho_:Ne4P_)

         ! PopII leaves the domain at a supersonic velocity
         ! (50km/s while for their temperature 1.E5K their C_s=30km/s)
         ! For the transient case when it flows inward,
         ! we use a fraction of ions

         if( sum(VarsTrueFace_V(Ne2Ux_:Ne2Uz_)*FaceCoords_D) > 0.0)then
            VarsGhostFace_V(Ne2Rho_) = &
                 VarsGhostFace_V(Rho_) * RhoBcFactor_I(Ne2_)
            VarsGhostFace_V(Ne2P_)   = &
                 VarsGhostFace_V(p_)   * RhoBcFactor_I(Ne2_)
            VarsGhostFace_V(Ne2Ux_:Ne2Uz_) = &
                 VarsGhostFace_V(Ux_:Uz_) * uBcFactor_I(Ne2_)
         else
            VarsGhostFace_V(Ne2Rho_:Ne2P_) = VarsTrueFace_V(Ne2Rho_:Ne2P_)
         end if

         ! Pop III has the velocity and temperature of ions at inner boundary
         ! the density is taken to be a fraction of the ions

         if( sum(VarsTrueFace_V(Ne3Ux_:Ne3Uz_)*FaceCoords_D) > 0.0)then
            VarsGhostFace_V(Ne3Rho_) = &
                 VarsGhostFace_V(Rho_) * RhoBcFactor_I(Ne3_)
            VarsGhostFace_V(Ne3P_)   = &
                 VarsGhostFace_V(p_)   * RhoBcFactor_I(Ne3_)
            VarsGhostFace_V(Ne3Ux_:Ne3Uz_) = &
                 VarsGhostFace_V(Ux_:Uz_) * uBcFactor_I(Ne3_)
         else
            VarsGhostFace_V(Ne3Rho_:Ne3P_) = VarsTrueFace_V(Ne3Rho_:Ne3P_)
         end if

         ! Pop IV

         if( sum(VarsTrueFace_V(Ne4Ux_:Ne4Uz_)*FaceCoords_D) > 0.0)then
            VarsGhostFace_V(Ne4Rho_) = &
                 VarsGhostFace_V(Rho_) * RhoBcFactor_I(Ne4_)
            VarsGhostFace_V(Ne4P_)   = &
                 VarsGhostFace_V(p_)   * RhoBcFactor_I(Ne4_)
            VarsGhostFace_V(Ne4Ux_:Ne4Uz_) = &
                 VarsGhostFace_V(Ux_:Uz_) * uBcFactor_I(Ne4_)
         else
            VarsGhostFace_V(Ne4Rho_:Ne4P_) = VarsTrueFace_V(Ne4Rho_:Ne4P_)
         end if
      endif

      if(DoTest)then
         write(*,*) NameSub,' FaceCoord=', FaceCoords_D
         write(*,*) NameSub,' i,j,kFace=', iFace, jFace, kFace
         write(*,*) NameSub,' SinTheta =', SinTheta
         write(*,*) NameSub,' Bsph_D   =', Bsph_D
         write(*,*) NameSub,' Vsph_D   =', Vsph_D
         write(*,*) NameSub,' Pmag, Peq=', Pmag, PmagEquator
         ! Zieger
         if(.not.IsMhd)then
            write(*,*) NameSub,' SWHRhoGhost =', VarsGhostFace_V(SWHRho_)
            write(*,*) NameSub,' SWHpGhost   =', VarsGhostFace_V(SWHP_)
            write(*,*) NameSub,' SWHUghost   =', VarsGhostFace_V(SWHUx_:SWHUz_)
         else
            write(*,*) NameSub,' RhoGhost =', VarsGhostFace_V(Rho_)
            write(*,*) NameSub,' pGhost   =', VarsGhostFace_V(p_)
            write(*,*) NameSub,' Ughost   =', VarsGhostFace_V(Ux_:Uz_)
         end if
         write(*,*) NameSub,' Bghost   =', VarsGhostFace_V(Bx_:Bz_)
         if(UseNeutralFluid)then
            write(*,*) NameSub,' Pop1     =', VarsGhostFace_V(NeuRho_:NeuP_)
            write(*,*) NameSub,' Pop2     =', VarsGhostFace_V(Ne2Rho_:Ne2P_)
            write(*,*) NameSub,' Pop3     =', VarsGhostFace_V(Ne3Rho_:Ne3P_)
            write(*,*) NameSub,' Pop4     =', VarsGhostFace_V(Ne4Rho_:Ne4P_)
         end if
         ! Zieger
         if(.not.IsMhd)then
            write(*,*) NameSub,' VPUIsph_D=', VPUIsph_D
            write(*,*) NameSub,' Pui3     =', VarsGhostFace_V(Pu3Rho_:Pu3P_)
            write(*,*) NameSub,' pPUI, PU3_p, pSolarwind, SwhP =', &
                 pPUI, Pu3P, pSolarwind, SwhP
            if(UseElectronPressure)then
               write(*,*) NameSub,' Pe     =', SwhPe
            end if
         end if
      end if

    end associate

    call test_stop(NameSub, DoTest)
  end subroutine user_set_face_boundary
  !============================================================================
  subroutine user_normalization

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_normalization'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    No2Si_V(UnitX_)  = cAU                                      ! m
    No2Si_V(UnitU_)  = sqrt(Gamma*cBoltzmann*SwhTDim/cProtonMass) ! m/s
    No2Si_V(UnitRho_)= cProtonMass*SwhRhoDim*1.0E+6           ! kg/m^3

    if(DoTest)then
       write(*,*)NameSub,' No2Si_V(UnitX_)  =',No2Si_V(UnitX_)
       write(*,*)NameSub,' No2Si_V(UnitU_)  =',No2Si_V(UnitU_)
       write(*,*)NameSub,' No2Si_V(UnitRho_)=',No2Si_V(UnitRho_)
    end if

    call test_stop(NameSub, DoTest)
  end subroutine user_normalization
  !============================================================================
  subroutine user_set_cell_boundary(iBlock, iSide, TypeBc, IsFound)

    use ModCellBoundary, ONLY: iMin, iMax, jMin, jMax, kMin, kMax

    ! The ISM enters at the east boundary (negative x)
    ! February 08, 2018 - added the possibility for using user conditions in
    ! other boundaries as well

    integer,      intent(in):: iBlock, iSide
    character(len=*), intent(in)  :: TypeBc
    logical,      intent(out):: IsFound

    integer :: iVar, i, j, k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_cell_boundary'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    IsFound = .true.

    do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
       State_VGB(SWHRho_,i,j,k,iBlock)=VliswRho
       State_VGB(SWHRhoUx_,i,j,k,iBlock)=VliswRho*VliswUx
       State_VGB(SWHRhoUy_,i,j,k,iBlock)=VliswRho*VliswUy
       State_VGB(SWHRhoUz_,i,j,k,iBlock)=VliswRho*VliswUz
       State_VGB(Bx_,i,j,k,iBlock)=VliswBx
       State_VGB(By_,i,j,k,iBlock)=VliswBy
       State_VGB(Bz_,i,j,k,iBlock)=VliswBz
       State_VGB(SWHP_,i,j,k,iBlock)=VliswP
       if(UseElectronPressure) State_VGB(Pe_,i,j,k,iBlock)=VliswPe

       if(.not.IsMhd)then
          State_VGB(Pu3Rho_,  i,j,k,iBlock) = 1E-5*VliswRho
          State_VGB(Pu3RhoUx_,i,j,k,iBlock) = 1E-5*VliswRho*VliswUx
          State_VGB(Pu3RhoUy_,i,j,k,iBlock) = 1E-5*VliswRho*VliswUy
          State_VGB(Pu3RhoUz_,i,j,k,iBlock) = 1E-5*VliswRho*VliswUz
          State_VGB(Pu3P_,    i,j,k,iBlock) = 1E-5*VliswP
       end if

    end do; end do; end do

    if(UseNeutralFluid)then
       !
       ! PopIV is the one one coming with the ISW
       ! The separation between Pop IV and Pop I is arbitrary so
       ! we took the separation as Vlad in x=-1500AU

       do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
          State_VGB(Ne4Rho_,i,j,k,iBlock) = RhoNeutralsISW
          State_VGB(Ne4RhoUx_,i,j,k,iBlock) = RhoNeutralsISW*UxNeutralsISW
          State_VGB(Ne4RhoUy_,i,j,k,iBlock) = RhoNeutralsISW*UyNeutralsISW
          State_VGB(Ne4RhoUz_,i,j,k,iBlock) = RhoNeutralsISW*UzNeutralsISW
          State_VGB(Ne4P_,i,j,k,iBlock)     = PNeutralsISW
       end do; end do; end do
       !
       ! In general you should specify as many values as many incoming
       ! characteristic waves are present. For a neutral fluid this
       ! is 0 for supersonic outflow, 1 for subsonic outflow,
       ! 4 for subsonic inflow and 5 for supersonic inflow.

       !
       ! PopII and III supersonic outflow
       do iVar = NeuRho_, Ne3P_
          select case(iSide)
          case(1)
             do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
                State_VGB(iVar,i,j,k,iBlock) =  State_VGB(iVar,1,j,k,iBlock)
             end do; end do; end do
          case(2)
             do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
                State_VGB(iVar,i,j,k,iBlock) =  State_VGB(iVar,nI,j,k,iBlock)
             end do; end do; end do
          case(3)
             do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
                State_VGB(iVar,i,j,k,iBlock) = State_VGB(iVar,i,1,k,iBlock)
             end do; end do; end do
          case(4)
             do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
                State_VGB(iVar,i,j,k,iBlock) = State_VGB(iVar,i,nJ,k,iBlock)
             end do; end do; end do
          case(5)
             do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
                State_VGB(iVar,i,j,k,iBlock) = State_VGB(iVar,i,j,1,iBlock)
             end do; end do; end do
          case(6)
             do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
                State_VGB(iVar,i,j,k,iBlock) = State_VGB(iVar,i,j,nK,iBlock)
             end do; end do; end do
          end select
       end do
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_cell_boundary
  !============================================================================
  subroutine user_set_ics(iBlock)

    use ModCoordTransform, ONLY: rot_xyz_sph

    integer, intent(in) :: iBlock

    integer :: i,j,k
    real :: x, y, z, r
    real :: b_D(3), v_D(3), bSph_D(3), vSph_D(3), vPUI_D(3), vPUISph_D(3)
    real :: SinTheta, SignZ

    real :: XyzSph_DD(3,3) ! rotation matrix Xyz_D = matmul(XyzSph_DD, Sph_D)

    logical :: DoTestCell
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_ics'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! Make sure that OmegaSun and ParkerTilt are set
    if(OmegaSun == 0.0)call set_omega_parker_tilt

    do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI

       if(.not. Used_GB(i,j,k,iBlock)) CYCLE

       DoTestCell = DoTest .and. i==iTest .and. j==jTest .and. k==kTest

       x = Xyz_DGB(x_,i,j,k,iBlock)
       y = Xyz_DGB(y_,i,j,k,iBlock)
       z = Xyz_DGB(z_,i,j,k,iBlock)
       r = r_GB(i,j,k,iBlock)

       XyzSph_DD = rot_xyz_sph(x,y,z)

       ! theta is the angle measured from the pole
       SinTheta = sqrt(x**2+y**2)/r

       ! calculating the Parker B field spherical components Bsph_D

       ! good for polarity of 1997       SignZ = sign(1.0, z)
       SignZ = sign(1.0,z)  ! good for 2005

       ! Bsph_D(1) = SignZ*SwhBx*(rBody/r)**2  ! Br
       ! Bsph_D(2) = 0.0                        ! Btheta
       ! Bsph_D(3) = -SignZ*SwhBx*SinTheta*ParkerTilt*(rBody/r) ! Bphi

       ! monopole
       Bsph_D(1) = SwhBx*(rBody/r)**2  ! Br
       Bsph_D(2) = 0.0                        ! Btheta
       Bsph_D(3) = SwhBx*SinTheta*ParkerTilt*(rBody/r) ! Bphi

       Vsph_D = [ SwhUx, 0., 0. ]
       vPUISph_D = [ Pu3Ux, 0., 0. ]

       ! magnetic field components in cartesian coordinates
       b_D = matmul(XyzSph_DD, Bsph_D)

       State_VGB(Bx_:Bz_,i,j,k,iBlock) = b_D

       ! velocity components in cartesian coordinates
       v_D = matmul(XyzSph_DD, Vsph_D)
       vPUI_D = matmul(XyzSph_DD, VPUIsph_D)
       ! density and pressure
       ! zieger
       if(.not.IsMhd)then
          State_VGB(SWHRho_,i,j,k,iBlock) = SwhRho * (rBody/r)**2
          State_VGB(SWHP_,i,j,k,iBlock)   = SwhP   * (rBody/r)**(2*Gamma)
          if(UseElectronPressure) &
               State_VGB(Pe_,i,j,k,iBlock) = SwhPe * (rBody/r)**(2*Gamma)
          State_VGB(SWHRhoUx_:SWHRhoUz_,i,j,k,iBlock) = &
               State_VGB(SWHRho_,i,j,k,iBlock)*v_D
       else
          State_VGB(Rho_,i,j,k,iBlock) = SwhRho * (rBody/r)**2
          State_VGB(P_,i,j,k,iBlock)   = SwhP   * (rBody/r)**(2*Gamma)
          State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) = &
               State_VGB(Rho_,i,j,k,iBlock)*v_D
       end if

       if(UseColdCloud) then
          !! for studies of Heliosphere in cold cloud
          if (r > rBody) then
             State_VGB(Bx_,i,j,k,iBlock) =  VliswBx
             State_VGB(By_,i,j,k,iBlock) =  VliswBy
             State_VGB(Bz_,i,j,k,iBlock) =  VliswBz

             State_VGB(Rho_,i,j,k,iBlock) = VliswRho
             State_VGB(P_,i,j,k,iBlock)   = VliswP

             State_VGB(RhoUx_,i,j,k,iBlock) = VliswUx*VliswRho
             State_VGB(RhoUy_,i,j,k,iBlock) = VliswUy*VliswRho
             State_VGB(RhoUz_,i,j,k,iBlock) = VliswUz*VliswRho
          endif
       end if

       if(UseNeutralFluid)then
          if(UseColdCloud)then
             ! PopI for studies of Heliosphere in cold cloud
             State_VGB(NeuRho_,i,j,k,iBlock)  = VliswRho
             State_VGB(NeuP_,i,j,k,iBlock) =   VliswP
             State_VGB(NeuRhoUx_,i,j,k,iBlock)= VliswUx*VliswRho
             State_VGB(NeuRhoUy_,i,j,k,iBlock)= VliswUy*VliswRho
             State_VGB(NeuRhoUz_,i,j,k,iBlock)= VliswUz*VliswRho
          else
             ! PopI
             State_VGB(NeuRho_,i,j,k,iBlock)  =  RhoNeutralsISW
             State_VGB(NeuP_,i,j,k,iBlock) =   PNeutralsISW
             State_VGB(NeuRhoUx_,i,j,k,iBlock)=RhoNeutralsISW*UxNeutralsISW
             State_VGB(NeuRhoUy_,i,j,k,iBlock)=RhoNeutralsISW*UyNeutralsISW
             State_VGB(NeuRhoUz_,i,j,k,iBlock)=RhoNeutralsISW*UzNeutralsISW
          endif
          !! This profile takes into account loss due to PUI
          !! State_VGB(NeuRho_,i,j,k,iBlock) = &
          !!    RhoNeutralsISW*Exp(-lambda*thetaN/((r**2)*SinThetaN)) &
          !!    *Exp(-lambda*thetaN/(r*SinThetaN))
          !! State_VGB(NeuP_,i,j,k,iBlock) = &
          !!    PNeutralsISW*Exp(-lambda*thetaN/(r*SinThetaN))

          ! PopII - I set it to be about 100km/s radially.
          ! The temperature is set to be 10^5K for this population.
          ! v_D is the plasma velocity, we take one quarter of that.
          State_VGB(Ne2Rho_,i,j,k,iBlock) = 1.e-3 * RhoNeutralsISW
          State_VGB(Ne2P_,i,j,k,iBlock)   = 1.e-3 * PNeutralsISW
          State_VGB(Ne2RhoUx_:Ne2RhoUz_,i,j,k,iBlock) = &
               0.25*State_VGB(Ne2Rho_,i,j,k,iBlock)*v_D
          ! PopIII
          ! Marc Kornbleuth modified to use SWH pop for Pop 3
          State_VGB(Ne3Rho_,i,j,k,iBlock) = 0.1*State_VGB(SWHRho_,i,j,k,iBlock)
          State_VGB(Ne3P_,i,j,k,iBlock)   = 0.1*State_VGB(SWHP_,i,j,k,iBlock)
          State_VGB(Ne3RhoUx_:Ne3RhoUz_,i,j,k,iBlock) = &
               State_VGB(Ne3Rho_,i,j,k,iBlock)*v_D

          ! PopIV
          State_VGB(Ne4Rho_,i,j,k,iBlock)  =RhoNeutralsISW
          State_VGB(Ne4RhoUx_,i,j,k,iBlock)=RhoNeutralsISW*UxNeutralsISW
          State_VGB(Ne4RhoUy_,i,j,k,iBlock)=RhoNeutralsISW*UyNeutralsISW
          State_VGB(Ne4RhoUz_,i,j,k,iBlock)=RhoNeutralsISW*UzNeutralsISW
          State_VGB(Ne4P_,i,j,k,iBlock) = PNeutralsISW
       end if

       ! If filling in with realistic PUI profile must do
       ! after neutrals populated
       ! neglect photoionization at >30 AU, ~10% effect
       ! must be a-dimensional
       ! start with PUI's only to 100 AU
       ! if (r <= rPop3Limit) then

       ! No production yet
       ! Zieger
       if(.not.IsMhd)then
          State_VGB(Pu3Rho_,i,j,k,iBlock) = Pu3Rho * (rBody/r)**2
          State_VGB(Pu3P_,i,j,k,iBlock)   = Pu3P   * (rBody/r)**(2*Gamma)
          State_VGB(Pu3RhoUx_:Pu3RhoUz_,i,j,k,iBlock) = &
               State_VGB(Pu3Rho_,i,j,k,iBlock)*vPUI_D
       end if

       if(DoTestCell)then
          write(*,*)NameSub,' x, y, z, r            =', x, y, z, r
          write(*,*)NameSub,' SignZ, SwhBx, SinTheta=', SignZ, SwhBx, SinTheta
          write(*,*)NameSub,' OmegaSun, rBody, SwhUx=', OmegaSun,rBody,SwhUx
          write(*,*)NameSub,' Vsph_D                =', Vsph_D
          write(*,*)NameSub,' v_D                   =', v_D
          write(*,*)NameSub,' Bsph_D                =', Bsph_D
          write(*,*)NameSub,' b_D                   =', &
               State_VGB(Bx_:Bz_,i,j,k,iBlock)
          ! Zieger
          if(.not.IsMhd)then
             write(*,*)NameSub,' SWHRhoU_D =', &
                  State_VGB(SWHRhoUx_:SWHRhoUz_,i,j,k,iBlock)
             write(*,*)NameSub,' SWHRho,   =', State_VGB(SWHRho_,i,j,k,iBlock)
             write(*,*)NameSub,' SWHp      =', State_VGB(SWHP_,i,j,k,iBlock)
             write(*,*)NameSub,' VPUIsph_D =', VPUIsph_D
             write(*,*)NameSub,' vPUI_D    =', vPUI_D
             write(*,*)NameSub,' Pu3Rho,   =', State_VGB(Pu3Rho_,i,j,k,iBlock)
             write(*,*)NameSub,' Pu3P      =', State_VGB(Pu3P_,i,j,k,iBlock)
             if(UseElectronPressure) &
                  write(*,*)NameSub,' Pe        =', State_VGB(Pe_,i,j,k,iBlock)
          else
             write(*,*)NameSub,' RhoU_D =', &
                  State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)
             write(*,*)NameSub,' Rho,   =',State_VGB(Rho_,i,j,k,iBlock)
             write(*,*)NameSub,' p      =',State_VGB(P_,i,j,k,iBlock)
          end if
       end if

    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_ics
  !============================================================================
  subroutine user_initial_perturbation

    ! Set Pop II to be the same state as the ions with smaller density
    integer:: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_initial_perturbation'
    !--------------------------------------------------------------------------
    if(.not.UseNeutralFluid) &
         call CON_stop(NameSub//': no neutral fluids present')

    call test_start(NameSub, DoTest)
    do iBlock = 1, nBlock

       if( Unused_B(iBlock) ) CYCLE
       ! Zieger
       if(.not.IsMhd)then
          State_VGB(Ne2Rho_,:,:,:,iBlock) = RhoBcFactor_I(Ne2_) * &
               State_VGB(SWHRho_,:,:,:,iBlock)
          State_VGB(Ne2RhoUx_:Ne2RhoUz_,:,:,:,iBlock) = &
               RhoBcFactor_I(Ne2_)*uBcFactor_I(Ne2_)* &
               State_VGB(SWHRhoUx_:SWHRhoUz_,:,:,:,iBlock)
          State_VGB(Ne2P_,:,:,:,iBlock) = RhoBcFactor_I(Ne2_) * &
               State_VGB(SWHp_,:,:,:,iBlock)
       else
          State_VGB(Ne2Rho_,:,:,:,iBlock) = RhoBcFactor_I(Ne2_) * &
               State_VGB(Rho_,:,:,:,iBlock)
          State_VGB(Ne2RhoUx_:Ne2RhoUz_,:,:,:,iBlock) = &
               RhoBcFactor_I(Ne2_)*uBcFactor_I(Ne2_)* &
               State_VGB(RhoUx_:RhoUz_,:,:,:,iBlock)
          State_VGB(Ne2P_,:,:,:,iBlock) = RhoBcFactor_I(Ne2_) * &
               State_VGB(p_,:,:,:,iBlock)
       end if

    end do

    call test_stop(NameSub, DoTest)
  end subroutine user_initial_perturbation
  !============================================================================
  subroutine user_update_states(iBlock)

    use ModUpdateState, ONLY: update_state_normal

    use ModAdvance, ONLY: StateOld_VGB, State_VGB
    use ModGeometry, ONLY: rMin_B

    ! merav
    ! C.P. edited

    integer,intent(in):: iBlock

    integer:: i, j, k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_update_states'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    call update_state_normal(iBlock)
    if(DoFreezeNeutral)then
       ! Set neutrals back to previous state
       do k=1,nK; do j=1,nJ; do i=1,nI
          State_VGB(NeuRho_:Ne4P_,i,j,k,iBlock) = &
               StateOld_VGB(NeuRho_:Ne4P_,i,j,k,iBlock)
       end do; end do; end do
       RETURN
    end if

    ! No need to check blocks outside:
    if(rMin_B(iBlock) > rBody) RETURN

    do k=1,nK; do j=1,nJ; do i=1,nI
       if(r_GB(i,j,k,iBlock) > rBody) CYCLE

       if(iTableSolarWind > 0)then
          ! Calculate the time dependent solar wind
          call calc_time_dep_sw(i,j,k,iBlock)
       else
          ! Retain initial condition for all ions inside body
          State_VGB(1:iP_I(nIonFluid),i,j,k,iBlock) = &
               StateOld_VGB(1:iP_I(nIonFluid),i,j,k,iBlock)
       endif

    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  contains
    !==========================================================================
    subroutine calc_time_dep_sw(i,j,k,iBlock)

      use BATL_lib,       ONLY: Xyz_DGB
      use ModCoordTransform, ONLY: rot_xyz_sph
      use ModLookupTable, ONLY: interpolate_lookup_table

      integer,intent(in):: i, j, k, iBlock

      ! variables for Solar Cycle
      real :: Rho, Ur, Temp, p, x, y, z, r, Latitude
      real :: Bsph_D(3), Vsph_D(3)
      ! merav
      real :: v_D(3),vPUI_D(3), vPUISph_D(3)
      real :: XyzSph_DD(3,3) ! rotation matrix Xyz_D = matmul(XyzSph_DD,Sph_D)

      real, parameter:: LengthCycle = 662206313.647 ! length of solar cycle

      real :: TimeCycle ! holds current time of the simulation
      real :: Value_I(3)
      real :: SinTheta
      !------------------------------------------------------------------------
      x = Xyz_DGB(1,i,j,k,iBlock)
      y = Xyz_DGB(2,i,j,k,iBlock)
      z = Xyz_DGB(3,i,j,k,iBlock)
      r = r_GB(i,j,k,iBlock)

      XyzSph_DD = rot_xyz_sph(x,y,z)

      ! calculate the latitude of the cell
      SinTheta = sqrt(x**2+y**2)/r

      ! calculating latitude of the cell
      Latitude = cRadToDeg*asin(z/r)

      ! calculating time relative to the solar cycle
      TimeCycle = modulo(tSimulation, LengthCycle)
 
      ! interpolating the value of Rho, Vr, and Temp
      ! at the cell from the lookup table
      call interpolate_lookup_table(iTableSolarWind, Latitude, TimeCycle, &
           Value_I)  

      Ur  = Value_I(1)*Io2No_V(UnitU_)
      Rho = Value_I(2)*Io2No_V(UnitRho_)
      Temp= Value_I(3)*Io2No_V(UnitTemperature_)
      p = 2.0*Rho*Temp

      ! Spherical velocity, Vr, Vtheta, Vphi constant with  radial distance
      Vsph_D    = [ Ur, 0.0, 0.0 ]

      ! monopole with By negative and a time varying B
      ! time-dependent behavior of B taken from Michael et al. 2015
      Bsph_D(1) = (SQRT(0.5)/rBody**2)*(9.27638+ &
           7.60832d-8*TimeCycle-1.91555*SIN(1.28737d-8*TimeCycle)+ &
           0.144184*SIN(2.22823d-8*TimeCycle)+ &
           47.7758*SIN(2.18788d-10*TimeCycle)+ &
           83.5522*SIN(-1.20266d-9*TimeCycle))*Io2No_V(UnitB_) ! Br
      Bsph_D(2) =  0.0                             ! Btheta
      Bsph_D(3) = Bsph_D(1)*SinTheta*ParkerTilt*SwhUx/Ur ! Bphi for vary B

      ! Scale density, pressure, and magnetic field with radial distance
      Rho = Rho*(rBody/r)**2
      p     = p*(rBody/r)**(2*Gamma)
      Bsph_D(1) = Bsph_D(1)*(rBody/r)**2
      Bsph_D(3) = Bsph_D(3)*(rBody/r)

      ! merav      ! Setting the state variables

      ! Spherical magnetic field converted to Cartesian components
      State_VGB(Bx_:Bz_,i,j,k,iBlock) = matmul(XyzSph_DD, Bsph_D)

      ! Zieger
      if(.not.IsMhd)then
         ! merav
         ! velocity components in cartesian coordinates
         v_D = matmul(XyzSph_DD, Vsph_D)
         vPUI_D = matmul(XyzSph_DD, VPUIsph_D)
         ! density and pressure
         State_VGB(SWHRho_,i,j,k,iBlock) = SwhRho * (rBody/r)**2
         State_VGB(SWHP_,i,j,k,iBlock)   = SwhP   * (rBody/r)**(2*Gamma)
         if(UseElectronPressure)then
            State_VGB(Pe_,i,j,k,iBlock)   = SwhPe   * (rBody/r)**(2*Gamma)
         end if
         ! momentum
         State_VGB(SWHRhoUx_:SWHRhoUz_,i,j,k,iBlock) = &
              State_VGB(SWHRho_,i,j,k,iBlock)*v_D
         State_VGB(Pu3Rho_,i,j,k,iBlock) = Pu3Rho * (rBody/r)**2
         State_VGB(Pu3P_,i,j,k,iBlock)   = Pu3P   * (rBody/r)**(2*Gamma)
         ! momentum
         State_VGB(Pu3RhoUx_:Pu3RhoUz_,i,j,k,iBlock) = &
              State_VGB(Pu3Rho_,i,j,k,iBlock)*vPUI_D
      else
         State_VGB(Rho_,i,j,k,iBlock) = Rho
         ! Velocity converted to 3 components of momentum in Cartesian coords
         State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) = matmul(XyzSph_DD, Rho*Vsph_D)
         ! Sets pressure and energy only for the ion fluid
         State_VGB(p_,i,j,k,iBlock) = p
      end if

    end subroutine calc_time_dep_sw
    !==========================================================================
  end subroutine user_update_states
  !============================================================================
  subroutine user_action(NameAction)

    character(len=*), intent(in):: NameAction

    character(len=*), parameter:: StringFormat = '(10X,A19,F15.6,A11,F15.6)'

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_action'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(NameAction /= 'write progress') RETURN

    write(*,StringFormat) 'SwhRhoDim [n/cc]:',SwhRhoDim,'SwhRho:',SwhRho
    write(*,StringFormat) 'SwhUxDim  [km/s]:',SwhUxDim,'SwhUx:',SwhUx
    write(*,StringFormat) 'SwhUyDim  [km/s]:',SwhUyDim,'SwhUy:',SwhUy
    write(*,StringFormat) 'SwhUzDim  [km/s]:',SwhUzDim,'SwhUz:',SwhUz
    write(*,StringFormat) 'SwhTDim   [   K]:',SwhTDim,'SwhP:',SwhP
    write(*,StringFormat) 'SwhBxDim  [  nT]:',SwhBxDim,'SwhBx:',SwhBx
    write(*,StringFormat) 'SwhByDim  [  nT]:',SwhByDim,'SwhBy:',SwhBy
    write(*,StringFormat) 'SwhBzDim  [  nT]:',SwhBzDim,'SwhBz:',SwhBz
    write(*,'(10X,A19,F15.6)')           'SwhTDim   [   K]:',SwhTDim
    write(*,*)
    write(*,*)
    write(*,StringFormat) 'VliswRhoDim[n/cc]:',VliswRhoDim,'VliswRho:',VliswRho
    write(*,StringFormat) 'VliswUxDim[km/s]: ',VliswUxDim,'VliswUx:',VliswUx
    write(*,StringFormat) 'VliswUyDim[km/s]: ',VliswUyDim,'VliswUy:',VliswUy
    write(*,StringFormat) 'VliswUzDim[km/s]: ',VliswUzDim,'VliswUz:',VliswUz
    write(*,StringFormat) 'VliswPDim [nPa]: ',VliswPDim,'VliswP:',VliswP
    write(*,StringFormat) 'VliswBxDim[nT]: ',VliswBxDim,'VliswBx:',VliswBx
    write(*,StringFormat) 'VliswByDim[nT]:',VliswByDim,'VliswBy:',VliswBy
    write(*,StringFormat) 'VliswBzDim[nT]:',VliswBzDim,'VliswBz:',VliswBz
    write(*,'(10X,A19,F15.6)') 'VliswTDim[K]: ',VliswTDim!
    if(UseNeutralFluid)then
       ! neutrals
       write(*,*)
       write(*,StringFormat) &
            'RhoNeuWindDim:',RhoNeuWindDim ,'RhoNeutralsISW:',RhoNeutralsISW
       write(*,StringFormat) &
            'UxNeuWindDim:',UxNeuWindDim,'UxNeutralsISW:',UxNeutralsISW
       write(*,StringFormat) &
            'UyNeuWindDim:',UyNeuWindDim,'UyNeutralsISW:',UyNeutralsISW
       write(*,StringFormat) &
            'UzNeuWindDim:',UzNeuWindDim,'UzNeutralsISW:',UzNeutralsISW
       write(*,StringFormat) &
            'pNeuWindDim:',pNeuWindDim,'PNeutralsISW:',PNeutralsISW
       write(*,'(10X,A19,F15.6)') 'TempNeuWindDim:',TempNeuWindDim
       write(*,*)
    end if
    ! PUIs
    ! C.P. added
    ! Zieger
    if(.not.IsMhd)then
       write(*,*)
       write(*,StringFormat) 'Pu3RhoDim [n/cc]:',Pu3RhoDim,'SwhRho:',Pu3Rho
       write(*,StringFormat) 'Pu3UxDim  [km/s]:',Pu3UxDim,'Pu3Ux:',Pu3Ux
       write(*,StringFormat) 'Pu3UyDim  [km/s]:',Pu3UyDim,'Pu3Uy:',Pu3Uy
       write(*,StringFormat) 'Pu3UzDim  [km/s]:',Pu3UzDim,'Pu3Uz:',Pu3Uz
       write(*,StringFormat) 'Pu3TDim   [   K]:',Pu3TDim,'Pu3P:',Pu3P
       write(*,'(10X,A19,F15.6)')           'Pu3TDim   [   K]:',Pu3TDim
       write(*,*)
       if(UseElectronPressure)then
          write(*,StringFormat) 'VliswPeDim [nPa]: ', &
               VliswPeDim,' VliswPe:', VliswPe
          write(*,'(10X,A19,F15.6)') 'VliswTeDim[K]: ', VliswTeDim
          write(*,StringFormat) 'SwhTeDim   [   K]:', SwhTeDim, &
               ' SwhPe:', SwhPe
       end if
    end if

    call test_stop(NameSub, DoTest)
  end subroutine user_action
  !============================================================================
  subroutine user_set_plot_var(iBlock, NameVar, IsDimensional, &
       PlotVar_G, PlotVarBody, UsePlotVarBody, &
       NameTecVar, NameTecUnit, NameIdlUnit, IsFound)

    integer,          intent(in)   :: iBlock
    character(len=*), intent(in)   :: NameVar
    logical,          intent(in)   :: IsDimensional
    real,             intent(out)  :: PlotVar_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)
    real,             intent(out)  :: PlotVarBody
    logical,          intent(out)  :: UsePlotVarBody
    character(len=*), intent(inout):: NameTecVar
    character(len=*), intent(inout):: NameTecUnit
    character(len=*), intent(inout):: NameIdlUnit
    logical,          intent(out)  :: IsFound

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_plot_var'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    UsePlotVarBody = .true.
    PlotVarBody    = 0.0

    ! Zieger
    if(.not.IsMhd)then
       IsFound = .true.
       select case(NameVar)
       case('sRho')
          NameTecVar = 'sRho'
          PlotVar_G(1:nI,1:nJ,1:nK) = FluidSource_ICB(SWHRho_,:,:,:,iBlock)
          NameIdlUnit = '1/s'
          NameTecUnit = '1/s'

       case('sRhoux')
          NameTecVar = 'sRhoUx'
          PlotVar_G(1:nI,1:nJ,1:nK) = FluidSource_ICB(SWHRhoUx_,:,:,:,iBlock)
          NameIdlUnit = '1/s'
          NameTecUnit = '1/s'

       case('sRhouy')
          NameTecVar = 'sRhoUy'
          PlotVar_G(1:nI,1:nJ,1:nK) = FluidSource_ICB(SWHRhoUy_,:,:,:,iBlock)
          NameIdlUnit = '1/s'
          NameTecUnit = '1/s'
       case('sRhouz')
          NameTecVar = 'sRhoUz'
          PlotVar_G(1:nI,1:nJ,1:nK) = FluidSource_ICB(SWHRhoUz_,:,:,:,iBlock)
          NameIdlUnit = '1/s'
          NameTecUnit = '1/s'
       case('senergy')
          NameTecVar = 'senergy'
          PlotVar_G(1:nI,1:nJ,1:nK) = FluidSource_ICB(SWHEnergy_,:,:,:,iBlock)
          NameIdlUnit = '1/s'
          NameTecUnit = '1/s'
       case('sRhopui')
          NameTecVar = 'sRhoPUI'
          PlotVar_G(1:nI,1:nJ,1:nK) = FluidSource_ICB(Pu3Rho_,:,:,:,iBlock)
          NameIdlUnit = '1/s'
          NameTecUnit = '1/s'
       case('sRhouxpui')
          NameTecVar = 'sRhoUxPUI'
          PlotVar_G(1:nI,1:nJ,1:nK) = FluidSource_ICB(Pu3RhoUx_,:,:,:,iBlock)
          NameIdlUnit = '1/s'
          NameTecUnit = '1/s'

       case('sRhouypui')
          NameTecVar = 'sRhoUyPUI'
          PlotVar_G(1:nI,1:nJ,1:nK) = FluidSource_ICB(Pu3RhoUy_,:,:,:,iBlock)
          NameIdlUnit = '1/s'
          NameTecUnit = '1/s'

       case('sRhouzpui')
          PlotVar_G(1:nI,1:nJ,1:nK) = FluidSource_ICB(Pu3RhoUz_,:,:,:,iBlock)
          NameIdlUnit = '1/s'
          NameTecUnit = '1/s'

       case('senergypui')
          PlotVar_G(1:nI,1:nJ,1:nK) = FluidSource_ICB(Pu3Energy_,:,:,:,iBlock)
          NameIdlUnit = '1/s'
          NameTecUnit = '1/s'

       case('fluid')
          if(UseNeutralFluid)then
             call select_region(iBlock)
             PlotVar_G(1:nI,1:nJ,1:nK) = iFluidProduced_C
          else
             PlotVar_G(1:nI,1:nJ,1:nK) = 0.0
          endif
       case('mach')
          ! C.P. edited to add PUI
          PlotVar_G = &
               sqrt( sum((State_VGB(SWHRhoUx_:SWHRhoUz_,:,:,:,iBlock) &
               + State_VGB(Pu3RhoUx_:Pu3RhoUz_,:,:,:,iBlock))**2, DIM=1) &
               / (Gamma *(State_VGB(SWHp_,:,:,:,iBlock) &
               + State_VGB(Pu3P_,:,:,:,iBlock)) &
               * (State_VGB(SWHRho_,:,:,:,iBlock) &
               +  State_VGB(Pu3Rho_,:,:,:,iBlock))) )
          ! merav addition
          ! edited to add PUI, could be more generic from first ion to last ion
       case('machalfven')
          PlotVar_G = &
               sqrt( sum((State_VGB(SWHRhoUx_:SWHRhoUz_,:,:,:,iBlock) &
               + State_VGB(Pu3RhoUx_:Pu3RhoUz_,:,:,:,iBlock))**2, DIM=1) &
               /   (( sum(State_VGB(Bx_:Bz_,:,:,:,iBlock)**2) &
               * (State_VGB(SWHRho_,:,:,:,iBlock) &
               +  State_VGB(Pu3Rho_,:,:,:,iBlock)) ) ) )

          ! end of merav addition

       case default
          IsFound = .false.
       end select
    else
       IsFound = .true.
       select case(NameVar)
       case('sRho')
          NameTecVar = 'SRho'
          PlotVar_G(1:nI,1:nJ,1:nK) = Source_VC(NeuRho_,:,:,:)
       case('fluid')
          if(UseNeutralFluid)then
             call select_region(iBlock)
             PlotVar_G(1:nI,1:nJ,1:nK) = iFluidProduced_C
          else
             ! This is not needed when not using the 4 nutral fluids
             call CON_stop(NameSub//': no neutral fluids present')
          endif
       case('mach')
          PlotVar_G = &
               sqrt( sum(State_VGB(RhoUx_:RhoUz_,:,:,:,iBlock)**2, DIM=1)  &
               / (Gamma*State_VGB(p_,:,:,:,iBlock) &
               *  State_VGB(Rho_,:,:,:,iBlock)) )
          ! merav addition
       case('machalfven')
          PlotVar_G = &
               sqrt( sum(State_VGB(RhoUx_:RhoUz_,:,:,:,iBlock)**2, DIM=1)  &
               /   ( sum(State_VGB(Bx_:Bz_,:,:,:,iBlock)**2, DIM=1) &
               * State_VGB(Rho_,:,:,:,iBlock)) )

          ! end of merav addition
       case default
          IsFound = .false.
       end select
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_plot_var
  !============================================================================
  subroutine user_calc_sources_expl(iBlock)

    ! Calculates the charge exchange cross sections for the neutrals
    ! This subroutine calculate the charge exchange between the ionized
    ! component and the three population of neutrals, Neu, Ne2, Ne3
    ! following the notation of McNutt (1998)
    ! The three population of neutrals are the neutrals created by
    ! charge exchange in different regions of the outer heliosphere
    !
    ! Neu are the neutrals the comes from the interstellar medium  (PopI)
    ! Ne2 are created between the Termination Shock and Heliopause (PopII)
    ! Ne3 are the neutrals created inside the Termination Shock    (PopIII)
    ! Ne4 are the neutrals created between bow shock and heliopause (PopIV)

    ! As an example for Neu the source terms inside the Termination Shock will
    ! take into account their creations and destruction;
    ! outside the Termination Shock they will be just destroyed
    ! (see more details below).
    !
    ! Fluid 1 is the SW ionized fluid,
    ! Fluid 2 is the Pick-up ion fluid and
    ! Fluids 3 to 6 are the neutral fluids
    !
    ! History of implementation:
    ! Written by Merav Opher April 21, 2002;
    ! Help From Gabor Toth
    ! *modified for the version 7.5.2 May, 2002*
    ! *revistied and modified on Nov, 2002*
    ! source terms for the plasma
    ! modified to take of the S=0 Feb 08 (initialization)
    ! modified June 08, 2007 to take into account multi-fluids
    ! modified August 18;September10, 2007 to take into account 3-fluids
    ! october 27 checking units
    ! January 01, 2008 implementing implicit source terms
    ! January 08, 2008 all source terms for PopI as implicit
    ! January 24, 2008 comments included
    ! January-June BUGS fixed
    ! September - source terms written as McNutt (1998)
    !

    integer, intent(in) :: iBlock
    integer :: iFluid

    real :: State_V(nVar)
    real :: Ux, Uy, Uz, U2, SWHUx, SWHUy, SWHUz, SWHU2, &
         Pu3Ux, Pu3Uy, Pu3Uz, Pu3U2
    real:: RhoSWH, uSWH_D(3), sRho, sRhoU_D(3), sEnergy, &
         RhoPu3, uPu3_D(3), USWH2, UPu32
    real:: Source_V(nVar + nFluid)

    real :: r
    real, dimension(nFluid) :: &
         Ux_I, Uy_I, Uz_I, U2_I, Temp_I, Rho_D, UThS_I

    real:: u_DI(3,nFluid)
    real:: SrcLookI_II(Neu_:Ne4_,5), SrcLookN_II(Neu_:Ne4_,5)

    ! For Ion/SWH
    real, dimension(Neu_:Ne4_) :: &
         URelS_I, URelSdim_I, UStar_I, Sigma_I, Rate_I, &
         UStarM_I, SigmaN_I, RateN_I, RatePh_I, &
         I0xp_I, I0px_I, I0xpPh_I, I2xp_I, I2px_I, &
         JxpUx_I, JxpUxPh_I, JxpUy_I, JxpUyPh_I,&
         JxpUz_I, JxpUzPh_I, JpxUx_I, JpxUy_I, JpxUz_I, &
         Kxp_I, Kpx_I, Qepx_I, ExpPh_I, KxpPh_I, &
         QmpxUx_I, QmpxUy_I, QmpxUz_I

    ! For PU3
    real, dimension(Neu_:Ne4_):: &
         URelSPu3_I, URelSPu3dim_I, UStarPu3_I, SigmaPu3_I, RatePu3_I, &
         UStarMPu3_I, SigmaNPu3_I, RateNPu3_I, &
         I0xpu3_I, I0pu3x_I, I2xpu3_I, I2pu3x_I, &
         Jxpu3Ux_I, Jxpu3Uy_I, Jxpu3Uz_I, Jpu3xUx_I, Jpu3xUy_I, Jpu3xUz_I, &
         Kxpu3_I, Kpu3x_I, Qepu3x_I, Qmpu3xUx_I, Qmpu3xUy_I, Qmpu3xUz_I

    real, dimension(Neu_:Ne4_):: &
         SrcLookRhoN_I, SrcLookRhoUxN_I, SrcLookRhoUyN_I, SrcLookRhoUzN_I, &
         SrcLookEnergyN_I, &
         SrcLookRhoI_I, SrcLookRhoUxI_I, SrcLookRhoUyI_I, SrcLookRhoUzI_I, &
         SrcLookEnergyI_I

    integer :: i, j, k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_calc_sources_expl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! updating sources from AMPS in PT-OH Coupling
    ! hyzhou: this is evaluated both in explicit and implicit scheme.
    ! Is it necessary to do so?
    if(.not.UseNeutralFluid)then
       if(allocated(ExtraSource_ICB))then

          do k = 1, nK; do j = 1, nJ; do i = 1, nI

             ! skipping the cells inside rBody
             if(.not.Used_GB(i,j,k,iBlock))CYCLE

             ! Extract conservative variables
             State_V = State_VGB(:,i,j,k,iBlock)

             ! Zieger
             if(.not.IsMhd)then

                ! SWH variables
                RhoSWH  = State_V(SWHRho_)
                uSWH_D  = State_V(SWHRhoUx_:SWHRhoUz_)/RhoSWH
                USWH2   = sum(uSWH_D**2)

                ! updating source terms for SWH
                sRho = ExtraSource_ICB(1,i,j,k,iBlock)*RhoSWH
                sRhoU_D = ExtraSource_ICB(2:4,i,j,k,iBlock)*RhoSWH
                sEnergy = ExtraSource_ICB(5,i,j,k,iBlock)*RhoSWH
                Source_VC(SWHRho_,i,j,k) = Source_VC(SWHRho_,i,j,k) + sRho
                Source_VC(SWHRhoUx_:RhoUz_,i,j,k) = &
                     Source_VC(SWHRhoUx_:SWHRhoUz_,i,j,k) + sRhoU_D
                Source_VC(SWHEnergy_,i,j,k) = &
                     Source_VC(SWHEnergy_,i,j,k) + sEnergy
                Source_VC(SWHP_,i,j,k) = Source_VC(SWHP_,i,j,k) &
                     + GammaMinus1* &
                     (sEnergy - sum(uSWH_D*sRhoU_D) + 0.5*USWH2*sRho)

                ! Pu3 variables
                RhoPu3  = State_V(Pu3Rho_)
                uPu3_D  = State_V(Pu3RhoUx_:Pu3RhoUz_)/RhoPu3
                UPu32   = sum(uPu3_D**2)

                ! updating source terms for Pu3
                sRho = ExtraSource_ICB(6,i,j,k,iBlock)*RhoPu3
                sRhoU_D = ExtraSource_ICB(7:9,i,j,k,iBlock)*RhoPu3
                sEnergy = ExtraSource_ICB(10,i,j,k,iBlock)*RhoPu3
                Source_VC(Pu3Rho_,i,j,k) = Source_VC(Pu3Rho_,i,j,k) + sRho
                Source_VC(Pu3RhoUx_:Pu3RhoUz_,i,j,k) = &
                     Source_VC(Pu3RhoUx_:Pu3RhoUz_,i,j,k) + sRhoU_D
                Source_VC(Pu3Energy_,i,j,k) = &
                     Source_VC(Pu3Energy_,i,j,k) + sEnergy
                Source_VC(Pu3P_,i,j,k) = Source_VC(Pu3P_,i,j,k) &
                     + GammaMinus1* &
                     (sEnergy - sum(uPu3_D*sRhoU_D) + 0.5*UPu32*sRho)

             else
                RhoSWH  = State_V(Rho_)
                uSWH_D  = State_V(RhoUx_:RhoUz_)/RhoSWH
                USWH2   = sum(uSWH_D**2)

                ! updating source terms for single-ion plasma
                sRho = ExtraSource_ICB(1,i,j,k,iBlock)*RhoSWH
                sRhoU_D = ExtraSource_ICB(2:4,i,j,k,iBlock)*RhoSWH
                sEnergy = ExtraSource_ICB(5,i,j,k,iBlock)*RhoSWH
                Source_VC(Rho_,i,j,k) = Source_VC(Rho_,i,j,k) + sRho
                Source_VC(RhoUx_:RhoUz_,i,j,k) = &
                     Source_VC(RhoUx_:RhoUz_,i,j,k) + sRhoU_D
                Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) + sEnergy
                Source_VC(p_,i,j,k) = Source_VC(p_,i,j,k) &
                     + GammaMinus1* &
                     (sEnergy - sum(uSWH_D*sRhoU_D) + 0.5*USWH2*sRho)
             end if

             if(DoTest .and. i==iTest .and. j==jTest .and. k==kTest)then
                write(*,*) 'SwhRho = ', State_V(Rho_)
                write(*,*) 'U2 =', USWH2
                write(*,*) 'SwhP   = ', State_V(p_)
                write(*,*) 'AMPS SW Density Source =', sRho
                write(*,*) 'AMPS SW MVx Source =', sRhoU_D
                write(*,*) 'AMPS SW Energy Source =', sEnergy
                write(*,*) 'Source_VC(Rho) =',Source_VC(Rho_,i,j,k)
                write(*,*) 'Source_VC(RhoUx) =',Source_VC(RhoUx_,i,j,k)
                write(*,*) 'Source_VC(Energy) =',Source_VC(Energy_,i,j,k)
                write(*,*) 'Source_VC(P) =',Source_VC(p_,i,j,k)
                write(*,*) 'Norm Rho =', No2Si_V(UnitRho_)
                write(*,*) 'Norm n =', No2Si_V(UnitN_)
                write(*,*) 'Norm U =', No2Si_V(UnitU_)
                write(*,*) 'Norm Time =', No2Si_V(UnitT_)
                ! Zieger
                if(.not.IsMhd)then
                   write(*,*) ' PUI Density Source =', &
                        ExtraSource_ICB(6,i,j,k,iBlock)
                   write(*,*) ' PUI MVx Source =', &
                        ExtraSource_ICB(7,i,j,k,iBlock)
                   write(*,*) ' PUI Energy Source =', &
                        ExtraSource_ICB(10,i,j,k,iBlock)
                end if
             endif

          end do; end do; end do
       end if

       RETURN
    end if

    ! calculating some constants cBoltzmann is J/K;
    ! cth is related to U* and the thermal speed w1
    ! see Eq (1) in McNutt et al. 1988; w1 = sqrt(cth*T1)

    ! ALL FLUIDS, including total ion

    ! Figure out which neutral population is produced at this point
    call select_region(iBlock)

    ! Initialize and set all elements
    Rate_I        = 0
    RatePu3_I     = 0
    RateN_I       = 0
    RateNPu3_I    = 0
    Ustar_I       = 0
    UStarPu3_I    = 0
    URelS_I       = 0
    URelsdim_I    = 0
    URelSPu3_I    = 0
    URelSPu3dim_I = 0
    Sigma_I       = 0
    SigmaN_I      = 0
    SigmaPu3_I    = 0
    SigmaNPu3_I   = 0
    RatePh_I      = 0
    I0xpPh_I      = 0
    JxpUxPh_I     = 0
    JxpUyPh_I     = 0
    JxpUzPh_I     = 0
    KxpPh_I       = 0

    ! Initialize with 1 to avoid division by zero
    UStarM_I      = 1
    UStarMPu3_I   = 1

    do k=1,nK; do j=1,nJ; do i=1,nI

       ! Extrxact conservative variables
       State_V = State_VGB(:, i, j, k, iBlock)
       ! Production rates of neutrals through charge exchange between
       ! sw ions and neutrals, 0 rate for sw ions with other ions

       Rho_D = State_V(iRho_I)*No2Si_V(UnitN_)

       ! This calculates the array for flow values for all populations
       Ux_I = State_V(iRhoUx_I)/State_V(iRho_I)
       Uy_I = State_V(iRhoUy_I)/State_V(iRho_I)
       Uz_I = State_V(iRhoUz_I)/State_V(iRho_I)

       ! u_DI  = State_V(iRhoUx_I:iRhoUz_I)/State_V(iRho_I)*No2Si_V(UnitU_)
       u_DI(1,:) = Ux_I*No2Si_V(UnitU_)
       u_DI(2,:) = Uy_I*No2Si_V(UnitU_)
       u_DI(3,:) = Uz_I*No2Si_V(UnitU_)

       ! Velocity square for the two ionized and four population of neutrals;
       U2_I = Ux_I**2 + Uy_I**2 + Uz_I**2

       ! Temperature for the two ionized and four population of neutrals (K)
       Temp_I = (State_V(iP_I)/State_V(iRho_I))*No2Si_V(UnitTemperature_)

       ! Zieger
       if(IsMhd)then
          ! Why?
          Temp_I(Ion_) = 0.5*Temp_I(Ion_)
       else
          Temp_I(SWH_) = (State_V(SWHP_) &
               /(2*State_V(SWHRho_) + State_V(PU3Rho_))) &
               *No2Si_V(UnitTemperature_)
       endif

       ! Thermal speed (squared) for ionized and three populations of neutrals
       ! UThS units are (m/s)^2
       UThS_I = (2*cBoltzmann/cProtonMass)*Temp_I

       ! calculus of radius for heating the PU3 population
       r = r_GB(i,j,k,iBlock)

       ! Using look-up table to find source terms
       if(iTableChargeExchange > 0) then
          do iFluid = Neu_, Ne4_
             call get_charge_exchange( &
                  Rho_D(Ion_), UthS_I(Ion_), u_DI(:,Ion_), &
                  Rho_D(iFluid), UthS_I(iFluid), u_DI(:,iFluid), &
                  SrcLookI_II(iFluid,:),SrcLookN_II(iFluid,:))
          enddo

          where(UseSource_I(Neu_:)) SrcLookRhoI_I    = SrcLookI_II(Neu_:,1)
          where(UseSource_I(Neu_:)) SrcLookRhoUxI_I  = SrcLookI_II(Neu_:,2)
          where(UseSource_I(Neu_:)) SrcLookRhoUyI_I  = SrcLookI_II(Neu_:,3)
          where(UseSource_I(Neu_:)) SrcLookRhoUzI_I  = SrcLookI_II(Neu_:,4)
          where(UseSource_I(Neu_:)) SrcLookEnergyI_I = SrcLookI_II(Neu_:,5)

          where(UseSource_I(Neu_:)) SrcLookRhoN_I    = SrcLookN_II(Neu_:,1)
          where(UseSource_I(Neu_:)) SrcLookRhoUxN_I  = SrcLookN_II(Neu_:,2)
          where(UseSource_I(Neu_:)) SrcLookRhoUyN_I  = SrcLookN_II(Neu_:,3)
          where(UseSource_I(Neu_:)) SrcLookRhoUzN_I  = SrcLookN_II(Neu_:,4)
          where(UseSource_I(Neu_:)) SrcLookEnergyN_I = SrcLookN_II(Neu_:,5)

       else

          ! Relative velocity between neutrals and ionized fluid squared

          ! Zieger For SW or Ion
          where(UseSource_I(Neu_:)) &
               URelS_I = (Ux_I(Neu_:) - Ux_I(1))**2 &
               + (Uy_I(Neu_:) - Uy_I(1))**2 &
               + (Uz_I(Neu_:) - Uz_I(1))**2

          URelSdim_I = URelS_I * No2Si_V(UnitU_)**2

          ! Zieger
          if(.not.IsMhd)then
             ! for PU3
             where(UseSource_I(Neu_:)) &
                  URelSPu3_I = (Ux_I(Neu_:) - Ux_I(PU3_))**2 &
                  + (Uy_I(Neu_:) - Uy_I(PU3_))**2 &
                  + (Uz_I(Neu_:) - Uz_I(PU3_))**2

             URelSPu3dim_I = URelSPu3_I * No2Si_V(UnitU_)**2
          end if

          ! Calculating Cross Section Sigma_I for the different neutrals
          !
          ! Incorporating units to calculate the charge exchange cross sections
          ! No2Si_V(UnitU_) has units of m/s like cstartT so UReldim and UStar
          ! has units of m/s

          !! URelSdim_I  = URelS_I * No2Si_V(UnitU_)**2

          ! Eq. (62) of McNutt et al. 1988 for U*

          ! Zieger For SW or Ion
          where(UseSource_I(Neu_:)) &
               UStar_I = sqrt(URelSdim_I &
               + (4./cPi)*(UThS_I(Neu_:) + UThS_I(1)))

          ! UStar_I has units of m/s

          !!  UStar_I = sqrt(URelSdim_I + (4./cPi)*(UThS_I +UThS_I(SWH_)))

          ! Zieger
          if(.not.IsMhd)then
             ! For PU3
             where(UseSource_I(Neu_:)) &
                  UStarPu3_I = sqrt(URelSPu3dim_I &
                  + (4./cPi)*(UThS_I(Neu_:) + UThS_I(PU3_)))
          end if

          ! Eq. (64) of McNutt et al. 1988 for UM*

          ! Zieger For SW or Ion
          where(UseSource_I(Neu_:)) &
               UStarM_I = sqrt(URelSdim_I &
               + (64./(9.*cPi))*(UThS_I(Neu_:) + UThS_I(1)))

          ! Zieger
          if(.not.IsMhd)then
             ! For PU3
             where(UseSource_I(Neu_:)) &
                  UStarMPu3_I = sqrt(URelSPu3dim_I &
                  + (64./(9.*cPi))*(UThS_I(Neu_:) + UThS_I(PU3_)))
          end if

          ! UStar has units of cm/s so the factor 100 is to convert m to cm
          ! Sigma has units of m^2

          ! Zieger for SW or Ion
          ! Cross Section from Lindsay and Stebbings, 2005
          where(UseSource_I(Neu_:)) &
               Sigma_I = ((2.2835E-7 - (1.062E-8)*log(UStarM_I*100.))**2)*1.E-4
          SigmaN_I = ((2.2835E-7 - (1.062E-8)*log(UStar_I*100.))**2)*1.E-4

          ! Zieger
          if(.not.IsMhd)then
             ! For PU3
             where(UseSource_I(Neu_:)) &
                  SigmaPu3_I = &
                  ((2.2835E-7 - (1.062E-8)*log(UStarMPu3_I*100.))**2)*(1.E-4)
             SigmaNPu3_I = &
                  ((2.2835E-7 - (1.062E-8)*log(UStarPu3_I*100.))**2)*(1.E-4)
          end if

          ! Calculate Rate = \nu * nH * mp where nH is the density of neutrals
          ! \nu = Sigma*np*u_star where np is the density of ions and
          ! For each population of neutrals there will be another Rate
          ! The charge exhange cross section 100 to change ustar to cm/s
          ! Rate has no units (m^2*m/s*s*m-3 )

          ! Zieger
          if(.not.IsMhd)then
             ! For SW
             where(UseSource_I(Neu_:)) &
                  Rate_I = Sigma_I*State_V(SWHRho_) &
                  *State_V(iRho_I(Neu_:))*UStarM_I &
                  *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)

             ! For PU3
             where(UseSource_I(Neu_:)) &
                  RatePu3_I = SigmaPu3_I*State_V(PU3Rho_) &
                  *State_V(iRho_I(Neu_:))*UStarMPu3_I &
                  *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)

          else
             where(UseSource_I(Neu_:)) &
                  Rate_I = Sigma_I*State_V(Rho_) &
                  *State_V(iRho_I(Neu_:))*UStarM_I &
                  *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)

          end if

          ! Zieger
          if(.not.IsMhd)then
             ! for SW
             where(UseSource_I(Neu_:)) &
                  RateN_I = SigmaN_I*State_V(SWHRho_)&
                  *State_V(iRho_I(Neu_:))*UStar_I&
                  *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)

             ! For PU3
             where(UseSource_I(Neu_:)) &
                  RateNPu3_I = SigmaNPu3_I*State_V(PU3Rho_)&
                  *State_V(iRho_I(Neu_:))*UStarPu3_I &
                  *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)

          else
             where(UseSource_I(Neu_:)) &
                  RateN_I =SigmaN_I*State_V(Rho_)&
                  *State_V(iRho_I(Neu_:))*UStar_I  &
                  *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)
          end if

          ! Calculating the terms that enter in the Source terms
          ! The expressions for I0, Jxp, Kxp, Qexp are taken from
          ! Zank, Pauls, Williams, and Hall 1996
          ! For example for population I; Neu:
          !
          ! Source(density) = I0xpNe2 + I0xpNe3 in region 1
          !                 = - I0pxNeu         otherwise
          !
          ! Source(momentum) = QmxpNeu + JxpNe2 + JxpNe3 in region 1
          !                  = - JpxNeu                  otherwise
          !
          ! Source(energy) = QexpNeu + KxpNe2 + KxpNe3 in region 1
          !                = -KpxNeu                   otherwise
          !
          ! 'xp' indicates neutrals-> SW protons charge exchange rates and
          ! 'px' indicates SW protons->neutrals
          ! 'xpu3' indicates neutrals -> PUI 3
          ! 'pu3x' indicates PUI3 -> neutrals
          ! charge exchange
          ! For example:
          ! I0xpNe2 is the term of creation of Neu by charge exchange p-Ne2
          ! I0xpNe3 is the term of creation of Neu by charge exchange p-Ne3

          ! Zieger For SW or Ion
          I0xp_I = RateN_I
          I0px_I = RateN_I

          I2xp_I  = RateN_I*UThS_I(1)/No2Si_V(UnitU_)**2
          I2px_I  = RateN_I(Neu_:)*UThS_I(Neu_:)/No2Si_V(UnitU_)**2

          ! Zieger
          if(.not.IsMhd)then
             ! For PUI's
             I0xpu3_I = RateNPu3_I
             I0pu3x_I = RateNPu3_I
             I2xpu3_I = RateNPu3_I*UThS_I(PU3_)/&
                  No2Si_V(UnitU_)**2
             I2pu3x_I = RateNPu3_I(Neu_:)*UThS_I(Neu_:)/No2Si_V(UnitU_)**2
             ! units are fine: (Uth2/ustar)*termxp is unitless as it should be
          end if

          ! Zieger For SW or Ion
          JxpUx_I = Ux_I(1)*Rate_I
          JxpUy_I = Uy_I(1)*Rate_I
          JxpUz_I = Uz_I(1)*Rate_I

          JpxUx_I = Ux_I(Neu_:)*Rate_I
          JpxUy_I = Uy_I(Neu_:)*Rate_I
          JpxUz_I = Uz_I(Neu_:)*Rate_I

          ! Zieger
          if(.not.IsMhd)then
             ! For PU3
             Jxpu3Ux_I = Ux_I(PU3_)*RatePu3_I
             Jxpu3Uy_I = Uy_I(PU3_)*RatePu3_I
             Jxpu3Uz_I = Uz_I(PU3_)*RatePu3_I

             Jpu3xUx_I = Ux_I(Neu_:)*RatePu3_I
             Jpu3xUy_I = Uy_I(Neu_:)*RatePu3_I
             Jpu3xUz_I = Uz_I(Neu_:)*RatePu3_I
          end if

          ! this is for neutrals, which can be created or destroyed
          ! QmpxUx_I = JpxUx_I - JxpUx_I
          ! QmpxUy_I = JpxUy_I - JxpUy_I
          ! QmpxUz_I = JpxUz_I - JxpUz_I

          ! Zieger For SW or Ion
          QmpxUx_I = (Ux_I(Neu_:) - Ux_I(1))*Rate_I(Neu_:)

          QmpxUy_I = (Uy_I(Neu_:) - Uy_I(1))*Rate_I(Neu_:)

          QmpxUz_I = (Uz_I(Neu_:) - Uz_I(1))*Rate_I(Neu_:)

          ! Zieger
          if(.not.IsMhd)then
             Qmpu3xUx_I = (Ux_I(Neu_:) - Ux_I(PU3_))*RatePu3_I(Neu_:)

             Qmpu3xUy_I = (Uy_I(Neu_:) - Uy_I(PU3_))*RatePu3_I(Neu_:)

             Qmpu3xUz_I = (Uz_I(Neu_:) - Uz_I(PU3_))*RatePu3_I(Neu_:)
          end if

          ! For SW or Ion
          Kxp_I = 0.5*U2_I(1)*Rate_I + I2xp_I
          Kpx_I = 0.5*U2_I(Neu_:)*Rate_I + I2px_I
          Qepx_I = Kpx_I - Kxp_I

          ! Zieger
          if(.not.IsMhd)then
             ! For PU3
             Kxpu3_I = 0.5*U2_I(Pu3_)*RatePu3_I + I2xpu3_I
             Kpu3x_I = 0.5*U2_I(Neu_:)*RatePu3_I + I2pu3x_I
             Qepu3x_I = Kpu3x_I - Kxpu3_I

             ! Heating Pu3
             HeatPu3 = 0.0
             if(UsePu3Heating) HeatPu3 = &
                  State_V(PU3Rho_)*(TempPu3 - Temp_I(PU3_))*&
                  Io2No_V(UnitTemperature_)*(r-rBody)*FactorPu3

          end if

       endif

       if(UsePhotoion)then
          ! Chika. Photoionization rate
          ! rate_ph = 8E-8*(r0/r)**2
          ! 8E-8 has units 1/s
          ! r0 = 1 AU
          where(UseSource_I(Neu_:)) &
               RatePh_I = 8E-8*(1/(r+1e-10))**2*State_V(iRho_I(Neu_:)) &
               * No2Si_V(UnitT_)

          ! Chika: Source terms for single ion
          ! Density source term for photoionization
          ! Fahr et al. 2000 Table 2
          I0xpPh_I = RatePh_I

          ! Momentum source term for neutrals
          ! Table 2 Fahr et al. 2000
          JxpUxPh_I = Ux_I(Neu_:)*RatePh_I
          JxpUyPh_I = Uy_I(Neu_:)*RatePh_I
          JxpUzPh_I = Uz_I(Neu_:)*RatePh_I

          ! Energy source term
          ! Defining first this "energy" term for sw/ion and neutral species.
          ! Has units of (m/s)^2.
          ! Dividing out density so that energy source terms KxpPh_I
          ! syntax is consistent with momentum and density source
          ! term definitions.
          ! Equation 4 Fahr et al. 2000

          ExpPh_I = 0.5*U2_I(Neu_:) + State_V(iP_I(Neu_:))/&
               State_V(iRho_I(Neu_:))/(GammaMinus1)

          ! Chika: Energy source term
          ! Table 2 Fahr et al. 2000
          KxpPh_I = RatePh_I*ExpPh_I

       end if

       if(UseColdCloud)then
          if(UseSrcsInHelio)then
             !! for studies of heliosphere encountering Cold Cloud
             if(r_GB(i,j,k,iBlock) < HpLimit) then
                I0xp_I(Ne4_) = 0.0
                I0px_I(Ne4_) = 0.0
                JpxUx_I(Ne4_) = 0.0
                JpxUy_I(Ne4_) = 0.0
                JpxUz_I(Ne4_) = 0.0
                JxpUx_I(Ne4_) = 0.0
                JxpUy_I(Ne4_) = 0.0
                JxpUz_I(Ne4_) = 0.0
                QmpxUx_I(Ne4_) = 0.0
                QmpxUy_I(Ne4_) = 0.0
                QmpxUz_I(Ne4_) = 0.0
                Kxp_I(Ne4_) = 0.0
                Kpx_I(Ne4_) = 0.0
                Qepx_I(Ne4_) = 0.0
                I0xp_I(Neu_) = 0.0
                I0px_I(Neu_) = 0.0
                JpxUx_I(Neu_) = 0.0
                JpxUy_I(Neu_) = 0.0
                JpxUz_I(Neu_) = 0.0
                JxpUx_I(Neu_) = 0.0
                JxpUy_I(Neu_) = 0.0
                JxpUz_I(Neu_) = 0.0
                QmpxUx_I(Neu_) = 0.0
                QmpxUy_I(Neu_) = 0.0
                QmpxUz_I(Neu_) = 0.0
                Kxp_I(Neu_) = 0.0
                Kpx_I(Neu_) = 0.0
                Qepx_I(Neu_) = 0.0
             endif
          endif
       end if

       ! Calculate the source terms for this cell
       call calc_source_cell

    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  contains
    !==========================================================================
    subroutine calc_source_cell

      ! Calculate source temrs for one cell. The pressures source is
      ! S(p) = (gamma-1)[S(e) - u.S(Rhou) + 0.5 u**2 S(Rho)]

      real :: Source_V(nVar + nFluid)
      integer :: iVar, iFluid
      !------------------------------------------------------------------------
      Source_V = 0.0

      do iFluid = Neu_, Ne4_
         if(.not.UseSource_I(iFluid)) CYCLE
         call select_fluid(iFluid)
         if (iFluid == iFluidProduced_C(i,j,k)) then
            ! iRho etc = change in density etc
            if (iTableChargeExchange < 0) then
               ! Zieger
               if(.not.IsMhd)then
                  Source_V(iRho)    = sum(I0xp_I) + sum(I0pu3x_I) &
                       - I0xp_I(iFluid) - I0xpu3_I(iFluid)
                  Source_V(iRhoUx)  = sum(JxpUx_I) + sum(Jxpu3Ux_I) &
                       - JpxUx_I(iFluid) - Jpu3xUx_I(iFluid)
                  Source_V(iRhoUy)  = sum(JxpUy_I) + sum(Jxpu3Uy_I) &
                       - JpxUy_I(iFluid) - Jpu3xUy_I(iFluid)
                  Source_V(iRhoUz)  = sum(JxpUz_I) + sum(Jxpu3Uz_I) &
                       - JpxUz_I(iFluid) - Jpu3xUz_I(iFluid)
                  Source_V(iEnergy) = sum(Kxp_I) + sum(Kxpu3_I) &
                       - Kpx_I(iFluid) - Kpu3x_I(iFluid)
               else
                  Source_V(iRho)    = &
                       sum(I0xp_I)  - I0xp_I(iFluid) - I0xpPh_I(iFluid)
                  Source_V(iRhoUx)  = &
                       sum(JxpUx_I) - JpxUx_I(iFluid) - JxpUxPh_I(iFluid)
                  Source_V(iRhoUy)  = &
                       sum(JxpUy_I) - JpxUy_I(iFluid) - JxpUyPh_I(iFluid)
                  Source_V(iRhoUz)  = &
                       sum(JxpUz_I) - JpxUz_I(iFluid) - JxpUzPh_I(iFluid)
                  Source_V(iEnergy) = &
                       sum(Kxp_I)   - Kpx_I(iFluid) - KxpPh_I(iFluid)
               endif
            else
               Source_V(iRho)    =  sum(SrcLookRhoI_I) &
                    - SrcLookRhoN_I(iFluid) - I0xpPh_I(iFluid)
               Source_V(iRhoUx)  =  sum(SrcLookRhoUxI_I) &
                    - SrcLookRhoUxN_I(iFluid) - JxpUxPh_I(iFluid)
               Source_V(iRhoUy)  =  sum(SrcLookRhoUyI_I) &
                    - SrcLookRhoUyN_I(iFluid) - JxpUyPh_I(iFluid)
               Source_V(iRhoUz)  =  sum(SrcLookRhoUzI_I) &
                    - SrcLookRhoUzN_I(iFluid) - JxpUzPh_I(iFluid)
               Source_V(iEnergy) =  sum(SrcLookEnergyI_I) &
                    - SrcLookEnergyN_I(iFluid) - KxpPh_I(iFluid)
            endif
         else
            if (iTableChargeExchange < 0) then
               ! Zieger
               if(.not.IsMhd)then
                  Source_V(iRho)    = -I0px_I(iFluid)  - I0pu3x_I(iFluid)
                  Source_V(iRhoUx)  = -JpxUx_I(iFluid) - Jpu3xUx_I(iFluid)
                  Source_V(iRhoUy)  = -JpxUy_I(iFluid) - Jpu3xUy_I(iFluid)
                  Source_V(iRhoUz)  = -JpxUz_I(iFluid) - Jpu3xUz_I(iFluid)
                  Source_V(iEnergy) = -Kpx_I(iFluid)   - Kpu3x_I(iFluid)
               else
                  Source_V(iRho)    = -I0px_I(iFluid) - I0xpPh_I(iFluid)
                  Source_V(iRhoUx)  = -JpxUx_I(iFluid) - JxpUxPh_I(iFluid)
                  Source_V(iRhoUy)  = -JpxUy_I(iFluid) - JxpUyPh_I(iFluid)
                  Source_V(iRhoUz)  = -JpxUz_I(iFluid) - JxpUzPh_I(iFluid)
                  Source_V(iEnergy) = -Kpx_I(iFluid) - KxpPh_I(iFluid)
               end if
            else
               Source_V(iRho)    = -SrcLookRhoN_I(iFluid) - I0xpPh_I(iFluid)
               Source_V(iRhoUx)  = -SrcLookRhoUxN_I(iFluid) - JxpUxPh_I(iFluid)
               Source_V(iRhoUy)  = -SrcLookRhoUyN_I(iFluid) - JxpUyPh_I(iFluid)
               Source_V(iRhoUz)  = -SrcLookRhoUzN_I(iFluid) - JxpUzPh_I(iFluid)
               Source_V(iEnergy) = -SrcLookEnergyN_I(iFluid) - KxpPh_I(iFluid)
            endif
         end if
         Source_V(iP) = (Gamma-1)* ( Source_V(iEnergy) &
              - Ux_I(iFluid)*Source_V(iRhoUx) &
              - Uy_I(iFluid)*Source_V(iRhoUy) &
              - Uz_I(iFluid)*Source_V(iRhoUz) &
              + 0.5*U2_I(iFluid)*Source_V(iRho) )
      end do

      if (iTableChargeExchange < 0) then
         ! Zieger
         if(.not.IsMhd)then
            ! Source terms for the ion populations
            if(UseSource_I(SWH_) .and. UseSource_I(Pu3_))then
               ! Region 3: only make Pu3 in region before TS
               if (Ne3_ == iFluidProduced_C(i,j,k)) then
                  ! in Pop III region
                  Source_V(SWHRho_)   = -sum(I0px_I) &
                       + I0px_I(Ne3_) + I0xpu3_I(Ne3_)
                  Source_V(SWHRhoUx_) = -sum(JxpUx_I) &
                       + JpxUx_I(Ne3_) + Jpu3xUx_I(Ne3_)
                  Source_V(SWHRhoUy_) = -sum(JxpUy_I) &
                       + JpxUy_I(Ne3_) + Jpu3xUy_I(Ne3_)
                  Source_V(SWHRhoUz_) = -sum(JxpUz_I) &
                       + JpxUz_I(Ne3_) + Jpu3xUz_I(Ne3_)
                  Source_V(SWHEnergy_)= -sum(Kxp_I) &
                       + Kpx_I(Ne3_) + Kpu3x_I(Ne3_)
                  Source_V(SWHp_) = (Gamma-1)* ( Source_V(SWHEnergy_) &
                       - Ux_I(SWH_)*Source_V(SWHRhoUx_) &
                       - Uy_I(SWH_)*Source_V(SWHRhoUy_) &
                       - Uz_I(SWH_)*Source_V(SWHRhoUz_) &
                       + 0.5*U2_I(SWH_)*Source_V(SWHRho_) )
                  Source_V(Pu3Rho_) = sum(I0px_I) &
                       - I0px_I(Ne3_) - I0xpu3_I(Ne3_)
                  Source_V(Pu3RhoUx_) = sum(Qmpu3xUx_I) + sum(JpxUx_I) &
                       - JpxUx_I(Ne3_) - Jpu3xUx_I(Ne3_)
                  Source_V(Pu3RhoUy_) = sum(Qmpu3xUy_I) + sum(JpxUy_I) &
                       - JpxUy_I(Ne3_) -Jpu3xUy_I(Ne3_)
                  Source_V(Pu3RhoUz_) = sum(Qmpu3xUz_I) + sum(JpxUz_I) &
                       - JpxUz_I(Ne3_)- Jpu3xUz_I(Ne3_)
                  Source_V(Pu3Energy_)= sum(Qepu3x_I) + sum(Kpx_I) &
                       - Kpu3x_I(Ne3_) - Kpx_I(Ne3_) &
                       + HeatPu3
                  Source_V(Pu3P_) = (Gamma-1)* ( Source_V(Pu3Energy_) &
                       - Ux_I(Pu3_)*Source_V(Pu3RhoUx_) &
                       - Uy_I(Pu3_)*Source_V(Pu3RhoUy_) &
                       - Uz_I(Pu3_)*Source_V(Pu3RhoUz_) &
                       + 0.5*U2_I(Pu3_)*Source_V(Pu3Rho_) )

                  ! end of Region 3
               else ! outside of region 3
                  Source_V(SWHRho_) = sum(I0xpu3_I)
                  Source_V(SWHRhoUx_) = sum(QmpxUx_I) + sum(Jpu3xUx_I)
                  Source_V(SWHRhoUy_) = sum(QmpxUy_I) + sum(Jpu3xUy_I)
                  Source_V(SWHRhoUz_) = sum(QmpxUz_I) + sum(Jpu3xUz_I)
                  Source_V(SWHEnergy_)= sum(Qepx_I)+ sum(Kpu3x_I)
                  Source_V(SWHp_) = (Gamma-1)* ( Source_V(SWHEnergy_) &
                       - Ux_I(SWH_)*Source_V(SWHRhoUx_) &
                       - Uy_I(SWH_)*Source_V(SWHRhoUy_) &
                       - Uz_I(SWH_)*Source_V(SWHRhoUz_) &
                       + 0.5*U2_I(SWH_)*Source_V(SWHRho_) )
                  Source_V(Pu3Rho_)   = -sum(I0xpu3_I)
                  Source_V(Pu3RhoUx_) = -sum(Jxpu3Ux_I)
                  Source_V(Pu3RhoUy_) = -sum(Jxpu3Uy_I)
                  Source_V(Pu3RhoUz_) = -sum(Jxpu3Uz_I)
                  Source_V(Pu3Energy_)= -sum(Kxpu3_I)
                  Source_V(Pu3P_) = (Gamma-1)* ( Source_V(Pu3Energy_) &
                       - Ux_I(Pu3_)*Source_V(Pu3RhoUx_) &
                       - Uy_I(Pu3_)*Source_V(Pu3RhoUy_) &
                       - Uz_I(Pu3_)*Source_V(Pu3RhoUz_) &
                       + 0.5*U2_I(Pu3_)*Source_V(Pu3Rho_))
               end if
            end if
         else
            if(UseSource_I(Ion_))then
               Source_V(Rho_)    = sum(I0xpPh_I)
               Source_V(RhoUx_) = QmpxUx_I(Neu_) + QmpxUx_I(Ne2_) &
                    + QmpxUx_I(Ne3_) + QmpxUx_I(Ne4_) + sum(JxpUxPh_I)
               Source_V(RhoUy_) = QmpxUy_I(Neu_) + QmpxUy_I(Ne2_) &
                    + QmpxUy_I(Ne3_) + QmpxUy_I(Ne4_) + sum(JxpUyPh_I)
               Source_V(RhoUz_) = QmpxUz_I(Neu_) + QmpxUz_I(Ne2_) &
                    + QmpxUz_I(Ne3_) + QmpxUz_I(Ne4_) + sum(JxpUzPh_I)

               Source_V(Energy_)= sum( Qepx_I ) + sum(KxpPh_I)

               Source_V(p_) = (Gamma-1)* ( Source_V(Energy_) &
                    - Ux_I(Ion_)*Source_V(RhoUx_) &
                    - Uy_I(Ion_)*Source_V(RhoUy_) &
                    - Uz_I(Ion_)*Source_V(RhoUz_) &
                    + 0.5*U2_I(Ion_)*Source_V(Rho_) )
            end if
         end if
      else
         if(UseSource_I(Ion_))then
            Source_V(Rho_) = sum(I0xpPh_I)
            Source_V(RhoUx_)  = sum(SrcLookRhoUxN_I) &
                 -sum(SrcLookRhoUxI_I) + sum(JxpUxPh_I)
            Source_V(RhoUy_)  = sum(SrcLookRhoUyN_I) &
                 -sum(SrcLookRhoUyI_I) + sum(JxpUyPh_I)
            Source_V(RhoUz_)  = sum(SrcLookRhoUzN_I) &
                 -sum(SrcLookRhoUzI_I) + sum(JxpUzPh_I)
            Source_V(Energy_) = sum(SrcLookEnergyN_I) &
                 -sum(SrcLookEnergyI_I) + sum(KxpPh_I)
            Source_V(p_) = (Gamma-1)* ( Source_V(Energy_) &
                 - Ux_I(Ion_)*Source_V(RhoUx_) &
                 - Uy_I(Ion_)*Source_V(RhoUy_) &
                 - Uz_I(Ion_)*Source_V(RhoUz_) &
                 + 0.5*U2_I(Ion_)*Source_V(Rho_) )
         endif
      endif

      Source_VC(:,i,j,k) = Source_VC(:,i,j,k) + Source_V

      if(.not.allocated(FluidSource_ICB)) &
           allocate(FluidSource_ICB(nVar+nFluid,nI,nJ,nK,nBlock))
      FluidSource_ICB(:,i,j,k,iBlock) = Source_V

      if(DoTest .and. i==iTest .and. j==jTest .and. k==kTest)then

         Source_V = Source_VC(:,i,j,k)

         write(*,*) NameSub, ' iFluidProduced=', iFluidProduced_C(i,j,k)
         do iVar = 1, nVar + nFLuid
            write(*,*) ' Source(',NameVar_V(iVar),')=',Source_V(iVar)
         end do
         ! Zieger
         if(.not.IsMhd)then
            write(*,*) ' Temp_I    ', Temp_I
            write(*,*) ' Rate_I    ', Rate_I
            write(*,*) ' RateN_I   ', RateN_I
            write(*,*) ' Sigma_I   ', Sigma_I
            write(*,*) ' UStar_I   ', UStar_I
            write(*,*) ' UStarM_I  ', UStarM_I
            write(*,*) ' Ux_I      ', Ux_I
            write(*,*) ' U2_I      ', U2_I
            write(*,*) ' UTh_I     ', sqrt(UThS_I)
            write(*,*) ' URelSDim_I ', sqrt(URelSdim_I)
            write(*,*) ' uDim_I    ', sqrt(U2_I)*No2Io_V(UnitU_)
            write(*,*) ' I0xp_I    ', I0xp_I
            write(*,*) ' I0px_I    ', I0px_I
            write(*,*) ' I2xp_I    ', I2xp_I
            write(*,*) ' I2px_I    ', I2px_I
            write(*,*) ' JxpUx_I   ', JxpUx_I
            write(*,*) ' JpxUx_I   ', JpxUx_I
            write(*,*) ' QmpxUx_I  ', QmpxUx_I
            write(*,*) ' Kxp_I     ', Kxp_I
            write(*,*) ' Kpx_I     ', Kpx_I
            write(*,*) ' Qepx_I    ', Qepx_I
            write(*,*) ' HeatPu3   ', HeatPu3
            !
            write(*,*) ' RatePu3_I    ', RatePu3_I
            write(*,*) ' RateNPu3_I   ', RateNPu3_I
            write(*,*) ' SigmaPu3_I   ', SigmaPu3_I
            write(*,*) ' UStarPu3_I   ', UStarPu3_I
            write(*,*) ' UStarMPu3_I  ', UStarMPu3_I
            write(*,*) ' UTh_I     ', sqrt(UThS_I)
            write(*,*) ' URelSPu3dim_I ', sqrt(URelSPu3dim_I)
            write(*,*) ' uDim_I    ', sqrt(U2_I)*No2Io_V(UnitU_)
            write(*,*) ' I2xpu3_I    ', I2xpu3_I
            write(*,*) ' I2pu3x_I    ', I2pu3x_I
            write(*,*) ' Jxpu3Ux_I   ', Jxpu3Ux_I
            write(*,*) ' Jpx3Ux_I   ', Jpu3xUx_I
            write(*,*) ' Qmpu3xUx_I  ', Qmpu3xUx_I
            write(*,*) ' Kxpu3_I     ', Kxpu3_I
            write(*,*) ' Kpu3x_I     ', Kpu3x_I
            write(*,*) ' Qepu3x_I    ', Qepu3x_I
         else
            write(*,*) ' Temp_I    ', Temp_I
            write(*,*) ' Rate_I    ', Rate_I
            write(*,*) ' RatePh_I  ', RatePh_I
            write(*,*) ' Sigma_I   ', Sigma_I
            write(*,*) ' UStar_I   ', UStar_I
            write(*,*) ' UStarM_I  ', UStarM_I
            write(*,*) ' Ux_I      ', Ux_I
            write(*,*) ' U2_I      ', U2_I
            write(*,*) ' UTh_I     ', sqrt(UThS_I)
            write(*,*) ' URelDim_I ', sqrt(URelSdim_I)
            write(*,*) ' uDim_I    ', sqrt(U2_I)*No2Io_V(UnitU_)
            write(*,*) ' I2xp_I    ', I2xp_I
            write(*,*) ' I2px_I    ', I2px_I
            write(*,*) ' JxpUx_I   ', JxpUx_I
            write(*,*) ' JxpUxPh_I ', JxpUxPh_I
            write(*,*) ' JpxUx_I   ', JpxUx_I
            write(*,*) ' QmpxUx_I  ', QmpxUx_I
            write(*,*) ' Kxp_I     ', Kxp_I
            write(*,*) ' KxpPh_I   ', KxpPh_I
            write(*,*) ' Kpx_I     ', Kpx_I
            write(*,*) ' Qepx_I    ', Qepx_I
         end if
      end if

    end subroutine calc_source_cell
    !==========================================================================
  end subroutine user_calc_sources_expl
  !============================================================================
  subroutine set_omega_parker_tilt

    ! Calculate angular velocity in normalized units
    ! Note: the rotation period is 25.38 days in ModConst.f90:
    ! OmegaSun = cTwoPi/(RotationPeriodSun*Si2No_V(UnitT_))
    !--------------------------------------------------------------------------
    OmegaSun   = cTwoPi/(26.0*24.0*3600.00*Si2No_V(UnitT_))
    ParkerTilt = OmegaSun*rBody/SwhUx

  end subroutine set_omega_parker_tilt
  !============================================================================
  subroutine select_region(iBlock)

    ! set the global variabls iFluidProduced_C
    ! to select which neutral fluid is produced in each cell of the block

    integer, intent(in):: iBlock

    integer :: i, j, k

    ! cold cloud
    real :: RhoDim
    real :: InvRho, Ux, Uy, Uz, U2, SWHU2, p, Mach2, MachSW2
    real :: RhoT, TempDim, TempSWDim, U2Dim, B2, Rho, MachAlfven2, Tave
    real :: pSW, InvRhoSW, MachPUI2, pPUI, InvRhoPUI, RhoPUI, r
    real :: MachMagneto2, RhoSw, SWHMach2

    ! Produce fluid3 at the inner boundary

    ! This subroutine is not needed when not using the 4 neutral fluids
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'select_region'
    !--------------------------------------------------------------------------
    if(.not.UseNeutralFluid) call CON_stop(NameSub// &
         ': no neutral fluids present')

    call test_start(NameSub, DoTest, iBlock)
    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       
       ! Added by Ethan for Testing Purposes only
       iFluidProduced_C(i,j,k) = 0
       CYCLE

       if(r_GB(i,j,k,iBlock) < rPop3Limit) then
          iFluidProduced_C(i,j,k) = Ne3_
          CYCLE
       end if

       ! Zieger
       if(.not.IsMhd)then
          if(UseElectronPressure) then
             p = State_VGB(SWHp_,i,j,k,iBlock) &
                  + State_VGB(Pu3P_,i,j,k,iBlock) + State_VGB(Pe_,i,j,k,iBlock)
          else
             p = State_VGB(SWHp_,i,j,k,iBlock) + State_VGB(Pu3P_,i,j,k,iBlock)
          end if
          pSW = State_VGB(SWHp_,i,j,k,iBlock)
          pPUI = State_VGB(Pu3P_,i,j,k,iBlock)
          r = r_GB(i,j,k,iBlock)
          ! Calculating speed of total fluid
          Ux = (State_VGB(RhoUx_,i,j,k,iBlock) &
               + State_VGB(Pu3RhoUx_,i,j,k,iBlock)) &
               /(State_VGB(Rho_,i,j,k,iBlock) &
               + State_VGB(Pu3Rho_,i,j,k,iBlock))
          Uy = (State_VGB(RhoUy_,i,j,k,iBlock) &
               + State_VGB(Pu3RhoUy_,i,j,k,iBlock)) &
               /(State_VGB(Rho_,i,j,k,iBlock) &
               + State_VGB(Pu3Rho_,i,j,k,iBlock))
          Uz = (State_VGB(RhoUz_,i,j,k,iBlock) &
               + State_VGB(Pu3RhoUz_,i,j,k,iBlock)) &
               /(State_VGB(Rho_,i,j,k,iBlock) &
               + State_VGB(Pu3Rho_,i,j,k,iBlock))
          U2 = (Ux**2 + Uy**2 + Uz**2)
          ! Speed of SWH population
          SWHU2 = (State_VGB(SWHRhoUx_,i,j,k,iBlock) &
               /State_VGB(SWHRho_,i,j,k,iBlock))**2 &
               + (State_VGB(SWHRhoUy_,i,j,k,iBlock) &
               /State_VGB(SWHRho_,i,j,k,iBlock))**2 &
               + (State_VGB(SWHRhoUz_,i,j,k,iBlock) &
               /State_VGB(SWHRho_,i,j,k,iBlock))**2
          U2Dim  = U2*No2Io_V(UnitU_)**2
          ! for cold cloud
          RhoDim = State_VGB(Rho_,i,j,k,iBlock)*No2Io_V(UnitRho_)
          Rho = State_VGB(SWHRho_,i,j,k,iBlock) &
               + State_VGB(Pu3Rho_,i,j,k,iBlock)
          RhoSW = State_VGB(SWHRho_,i,j,k,iBlock)
          RhoPUI = State_VGB(Pu3Rho_,i,j,k,iBlock)
          Tave = p/Rho
          InvRhoSW = 1.0/RhoSW
          InvRhoPUI = 1.0/RhoPUI
          InvRho = 1.0/Rho
          B2 = sum(State_VGB(Bx_:Bz_,i,j,k,iBlock)**2)
          ! Square of Alfven Mach Number
          MachAlfven2 = U2*Rho/(B2+1.E-30)
          MachMagneto2 = U2/((1.E-10)+(Gamma*Tave)+(B2*InvRho))
          ! Zieger Here you need the speed of the total fluid (U2)
          ! Square of Mach number
          Mach2  = U2/(Gamma*p*InvRho)
          ! Square of PUI Mach number (using total fluid speed)
          ! Zieger This Mach number has no physical meaning
          MachPUI2 = U2/(Gamma*pPUI*InvRhoPUI)
          ! Square of Solar Wind Mach number (using total fluid speed)
          ! Zieger This Mach number has no physical meaning
          MachSW2 = U2/(Gamma*pSW*InvRhoSW)
          ! Square of Solar Wind Mach number (using SWH speed)
          ! Zieger This Mach number has no physical meaning
          SWHMach2 = SWHU2/(Gamma*pSW*InvRhoSW)
          ! Temperature in Kelvins
          TempDim = InvRho*p*No2Si_V(UnitTemperature_)
          ! Temperature of solar wind in Kelvins
          TempSWDim = InvRhoSW*pSW*No2Si_V(UnitTemperature_)
          ! use sonic Mach number - good for slow Bow Shock (Zieger+ 2015)
          if (MachPop4Limit**2 < Mach2 .and. uPop1LimitDim**2 > U2Dim) then
             ! Outside the bow shock
             iFluidProduced_C(i,j,k) = Ne4_
          elseif(TempDim < TempPop1LimitDim .and. U2Dim < uPop1LimitDim**2)then
             ! Outside the heliopause
             iFluidProduced_C(i,j,k) = Neu_
          elseif( Mach2 < MachPop2Limit**2 )then
             ! Heliosheath
             iFluidProduced_C(i,j,k) = Ne2_
          elseif( Mach2 > MachPop3Limit**2 )then
             !! before 85K      elseif( MachSW2 > MachPop3Limit**2 )then
             ! Inside termination shock
             iFluidProduced_C(i,j,k) = Ne3_
          else
             ! No neutrals are produced in this region (but they are destroyed)
             iFluidProduced_C(i,j,k) = 0
          end if
          ! adding more conditions to help with the regions
          if (MachSW2 > MachPop4Limit**2 .and. MachPUI2 < MachPUIPop3**2 ) then
             iFluidProduced_C(i,j,k) = Ne2_
          end if
          if (MachSW2 < MachPop3Limit**2 .and. MachPUI2 > MachPUIPop3**2 &
               .and. r<rPop3Limit) then
             iFluidProduced_C(i,j,k) = Ne3_
          end if
          ! first 10K after heating
       else
          InvRho = 1.0/State_VGB(Rho_,i,j,k,iBlock)
          p      = State_VGB(p_,i,j,k,iBlock)
          U2     = sum(State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)**2)*InvRho**2
          U2Dim  = U2*No2Io_V(UnitU_)**2
          ! merav modifications
          B2 = sum(State_VGB(Bx_:Bz_,i,j,k,iBlock)**2)
          ! Square of Alfven Mach Number
          MachAlfven2 = U2/(B2*InvRho + 1.E-30)
          MachMagneto2 = U2/(1.E-10 + Gamma*p*InvRho + B2*InvRho)
          ! end of merav modifications
          ! Square of Mach number
          Mach2      = U2/(Gamma*p*InvRho)
          RhoDim = State_VGB(Rho_,i,j,k,iBlock)*No2Io_V(UnitRho_)
          ! Temperature in Kelvins
          TempDim = InvRho*p*No2Si_V(UnitTemperature_)
          ! Apply full source except near the boundaries between regions
          if(.not.UseColdCloud)then
             if (MachPop4Limit**2 < Mach2 .and. uPop1LimitDim**2 > U2Dim) then
                ! Outside the bow shock
                iFluidProduced_C(i,j,k) = Ne4_
             elseif( TempPop1LimitDim > TempDim &
                  .and. uPop1LimitDim**2 > U2Dim)then
                ! Outside the heliopause
                iFluidProduced_C(i,j,k) = Neu_
             elseif( MachPop2Limit**2 > Mach2 )then
                ! Heliosheath
                iFluidProduced_C(i,j,k) = Ne2_
             elseif( Mach2 > MachPop3Limit**2 )then
                ! Inside termination shock
                iFluidProduced_C(i,j,k) = Ne3_
             else
                ! No neutrals are produced in this region
                ! but they are destroyed
                iFluidProduced_C(i,j,k) = 0
             end if
          else
             if(DoInitializeCloud)then
                if (r_GB(i,j,k,iBlock) > rPop3Limit) then
                   ! Outside the bow shock
                   iFluidProduced_C(i,j,k) = Ne4_
                else
                   ! Outside the heliopause
                   iFluidProduced_C(i,j,k) = Ne3_
                end if
             else
                !! for studies of heliosphere with cold cloud
                if (TempPop1LimitDim > TempDim &
                     .and. RhoPop4LimitDim > RhoDim) then
                   ! Outside the bow shock
                   iFluidProduced_C(i,j,k) = Ne4_
                elseif (TempPop1LimitDim > TempDim &
                     .and. RhoDim > RhoPop4LimitDim) then
                   ! Outside the heliopause
                   iFluidProduced_C(i,j,k) = Neu_
                elseif(TempDim > TempPop1LimitDim &
                     .and. uPop1LimitDim**2 > U2Dim)then
                   ! Heliosheath
                   iFluidProduced_C(i,j,k) = Ne2_
                elseif(TempDim > TempPop1LimitDim .and. &
                     U2Dim > uPop1LimitDim**2)then
                   ! Inside termination shock
                   iFluidProduced_C(i,j,k) = Ne3_
                else
                   ! No neutrals are produced in this region
                   ! but they are destroyed
                   iFluidProduced_C(i,j,k) = 0
                end if
             endif
          endif

       end if
    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine select_region
  !============================================================================
  subroutine user_init_session

    use ModMain,          ONLY: UseUserUpdateStates
    use ModLookupTable,   ONLY: i_lookup_table

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_init_session'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! normalization of SWH and VLISW and Neutrals

    VliswRho = VliswRhoDim*Io2No_V(UnitRho_)
    if(IsMhd)then
       VliswP  = 2.*VliswTDim*Io2No_V(UnitTemperature_)*VliswRho
    else
       VliswP  = VliswTDim*Io2No_V(UnitTemperature_)*VliswRho
       VliswPe = VliswTeDim*Io2No_V(UnitTemperature_)*VliswRho
    end if
    VliswUx  = VliswUxDim*Io2No_V(UnitU_)
    VliswUy  = VliswUyDim*Io2No_V(UnitU_)
    VliswUz  = VliswUzDim*Io2No_V(UnitU_)
    VliswBx  = VliswBxDim*Io2No_V(UnitB_)
    VliswBy  = VliswByDim*Io2No_V(UnitB_)
    VliswBz  = VliswBzDim*Io2No_V(UnitB_)

    SwhRho = SwhRhoDim*Io2No_V(UnitRho_)

    ! Zieger
    if(IsMhd)then
       ! Pressure of plasma = 2*T_ion*Rho_ion
       SwhP   = 2.*SwhTDim*Io2No_V(UnitTemperature_)*SwhRho
    end if

    SwhUx  = SwhUxDim*Io2No_V(UnitU_)
    SwhUy  = SwhUyDim*Io2No_V(UnitU_)
    SwhUz  = SwhUzDim*Io2No_V(UnitU_)
    SwhBx  = SwhBxDim*Io2No_V(UnitB_)
    SwhBy  = SwhByDim*Io2No_V(UnitB_)
    SwhBz  = SwhBzDim*Io2No_V(UnitB_)

    ! Zieger
    if(.not.IsMhd)then
       Pu3Ux  = Pu3UxDim*Io2No_V(UnitU_)
       Pu3Uy  = Pu3UyDim*Io2No_V(UnitU_)
       Pu3Uz  = Pu3UzDim*Io2No_V(UnitU_)
       Pu3Rho = Pu3RhoDim*Io2No_V(UnitRho_)
       Pu3P   = Pu3TDim*Io2No_V(UnitTemperature_)*Pu3Rho
       ! For SwhP, see eq. 1 from Opher et al. (2020)
       SwhP = (2*SwhRho + Pu3Rho)*SwhTDim*Io2No_V(UnitTemperature_)
       if(UseElectronPressure) &
            SwhPe = SwhTeDim*Io2No_V(UnitTemperature_)*(SwhRho + Pu3Rho)
    end if

    if(UseNeutralFluid)then
       RhoNeutralsISW = RhoNeuWindDim*Io2No_V(UnitRho_)
       pNeuWindDim = No2Io_V(UnitP_)/Gamma &
            *(RhoNeuWindDim/SwhRhoDim)*(TempNeuWindDim /SwhTDim)

       PNeutralsISW  = TempNeuWindDim*Io2No_V(UnitTemperature_)*RhoNeutralsISW
       UxNeutralsISW = UxNeuWindDim*Io2No_V(UnitU_)
       UyNeutralsISW = UyNeuWindDim*Io2No_V(UnitU_)
       UzNeutralsISW = UzNeuWindDim*Io2No_V(UnitU_)
    end if

    ! Set table index based on the table name.
    ! The table can be read by BATSRUS or the AMPS Fortran wrapper.
    if(iTableChargeExchange < 0) &
         iTableChargeExchange = i_lookup_table('ChargeExchange')

    ! Set table index for iTableSolarWind with table name solarwind2d
    if(iTableSolarWind < 0 ) &
         iTableSolarWind=i_lookup_table('solarwind2d')

    call test_stop(NameSub, DoTest)

  end subroutine user_init_session
  !============================================================================
  subroutine get_charge_exchange( &
       RhoIon, Cs2Ion, uIon_D, RhoNeu, Cs2Neu, uNeu_D, &
       SourceIon_V, SourceNeu_V)

    use ModLookupTable, ONLY: interpolate_lookup_table, i_lookup_table

    real, intent(in):: RhoIon    ! ion mass density
    real, intent(in):: Cs2Ion    ! ion thermal speed squared
    real, intent(in):: uIon_D(3) ! ion bulk velocity
    real, intent(in):: RhoNeu    ! neutral mass density
    real, intent(in):: Cs2Neu    ! neutral thermal speed squared (0 for AMPS)
    real, intent(in):: uNeu_D(3) ! neutral bulk velocity
    real, intent(out):: SourceIon_V(5) ! mass,momentum,energy sources from ions
    real, intent(out):: SourceNeu_V(5) ! mass,momentum,energy sources from neu.

    real:: SqrtDuDim, SqrtCsDim  ! sqrt of relative and thermal speeds (km/s)
    real:: Integral_V(3)         ! integrals used to get rate, force and work
    real:: MassRate, ForcePerU, WorkPerCs2
    real:: U1_D(3)
    real:: U1Sqrd, Cs2Sum

    character(len=*), parameter:: NameSub = 'get_charge_exchange'
    !--------------------------------------------------------------------------
    if(iTableChargeExchange < 0)then
       iTableChargeExchange = i_lookup_table('ChargeExchange')
       if(iTableChargeExchange < 0) call CON_stop(NameSub// &
            ' : could not find lookup table ChargeExchange. Fix PARAM.in')
    end if

    ! Use square root of sound speeds and relative velocity for lookup table
    SqrtCsDim = sqrt(sqrt(Cs2Ion + Cs2Neu)/1.E3)
    SqrtDuDim = sqrt(sqrt(sum((uNeu_D - uIon_D)**2))/1.E3)

    ! Get the MassRate, Force and Work integrals frame from the lookup table
    ! in SI units
    call interpolate_lookup_table(iTableChargeExchange, &
         SqrtCsDim, SqrtDuDim, Integral_V, DoExtrapolate=.false.)

    ! Multiply with mass densities
    Integral_V = Integral_V * RhoIon * RhoNeu

    MassRate   = Integral_V(1)  ! mass density change
    ForcePerU  = Integral_V(2)  ! force per velocity
    WorkPerCs2 = Integral_V(3)  ! work per thermal speed squared

    ! Put data into the source term array
    SourceIon_V(1)    = MassRate
    SourceIon_V(2:4)  = ForcePerU*uIon_D
    SourceIon_V(5)    = WorkPerCs2*Cs2Ion + 0.5*sum(ForcePerU*uIon_D**2)

    SourceNeu_V(1)    = MassRate
    SourceNeu_V(2:4)  = ForcePerU*uNeu_D
    SourceNeu_V(5)    = WorkPerCs2*Cs2Neu + 0.5*sum(ForcePerU*uNeu_D**2)

    if(DoFixChargeExchange) then
       !Extra terms that appear when splitting McNutt
       Cs2Sum = Cs2Ion + Cs2Neu
       U1_D = (Cs2Neu*uIon_D + Cs2Ion*uNeu_D)/Cs2Sum
       U1Sqrd = sum(U1_D*U1_D)

       SourceIon_V(2:4)  = SourceIon_V(2:4) + U1_D*(MassRate - ForcePerU)
       SourceIon_V(5)    = SourceIon_V(5) &
               + Cs2Ion*Cs2Neu*(0.75*MassRate - WorkPerCs2)/Cs2Sum &
               + 0.5*U1Sqrd*(MassRate - ForcePerU)

       SourceNeu_V(2:4)  = SourceNeu_V(2:4) + U1_D*(MassRate - ForcePerU)
       SourceNeu_V(5)    = SourceNeu_V(5) &
               + Cs2Ion*Cs2Neu*(0.75*MassRate - WorkPerCs2)/Cs2Sum &
               + 0.5*U1Sqrd*(MassRate - ForcePerU)
    endif

    if(Cs2Neu > 0.0) then
       ! Convert to normalized units here for BATSRUS
       SourceIon_V(1)   = SourceIon_V(1)  *Si2No_V(UnitRho_)
       SourceIon_V(2:4) = SourceIon_V(2:4)*Si2No_V(UnitRhoU_)
       SourceIon_V(5)   = SourceIon_V(5)  *Si2No_V(UnitEnergyDens_)
       SourceIon_V      = SourceIon_V     *No2Si_V(UnitT_) ! same as /Si2No_V

       SourceNeu_V(1)   = SourceNeu_V(1)  *Si2No_V(UnitRho_)
       SourceNeu_V(2:4) = SourceNeu_V(2:4)*Si2No_V(UnitRhoU_)
       SourceNeu_V(5)   = SourceNeu_V(5)  *Si2No_V(UnitEnergyDens_)
       SourceNeu_V      = SourceNeu_V     *No2Si_V(UnitT_)
    else
       ! AMPS prefers to get rate of change for number density
       SourceIon_V(1)   = SourceIon_V(1)/cProtonMass
       SourceNeu_V(1)   = SourceNeu_V(1)/cProtonMass
    endif

  end subroutine get_charge_exchange
  !============================================================================
end module ModUser
!==============================================================================
subroutine get_charge_exchange_wrapper( &
     RhoIon, Cs2Ion, uIon_D, RhoNeu, Cs2Neu, uNeu_D, &
     SourceIon_V, SourceNeu_V) &
     bind(c, name='get_charge_exchange_wrapper')

  ! C wrapper for coupling with AMPS

  use ModUser, ONLY: get_charge_exchange

  real, intent(in):: RhoIon    ! ion mass density
  real, intent(in):: Cs2Ion    ! ion thermal speed squared
  real, intent(in):: uIon_D(3) ! ion bulk velocity
  real, intent(in):: RhoNeu    ! neutral mass density
  real, intent(in):: Cs2Neu    ! neutral thermal speed squared (0 for AMPS)
  real, intent(in):: uNeu_D(3) ! neutral bulk velocity
  real, intent(out):: SourceIon_V(5) ! mass, momentum, energy sources from ions
  real, intent(out):: SourceNeu_V(5) ! mass, momentum, energy sources from neu.
  !----------------------------------------------------------------------------
  call get_charge_exchange(RhoIon, Cs2Ion, &
       uIon_D, RhoNeu, Cs2Neu, uNeu_D, SourceIon_V, SourceNeu_V)

end subroutine get_charge_exchange_wrapper
!==============================================================================
