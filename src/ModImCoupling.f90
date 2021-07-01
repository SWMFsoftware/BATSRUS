!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModImCoupling

  use BATL_lib, ONLY: &
       test_start, test_stop
#ifdef OPENACC
  use ModUtilities, ONLY: norm2
#endif
  ! Routines related to the coupline with the Inner Magnetosphere component
  use ModMain, ONLY: DoMultiFluidIMCoupling, DoAnisoPressureIMCoupling
  use ModVarIndexes, ONLY: nFluid

  implicit none

  SAVE

  private ! except
  public:: im_pressure_init
  public:: apply_im_pressure

  ! The number of IM pressures obtained so far
  integer, public :: iNewPIm = 0

  real, public, dimension(:), allocatable   :: &
       IM_lat, IM_lon
  real, public, dimension(:,:,:), allocatable :: &
       ImP_CV, ImRho_CV, ImPpar_CV
  real, public, dimension(:,:), allocatable :: IM_bmin

  logical,public,allocatable :: IsImRho_I(:), IsImP_I(:), IsImPpar_I(:)

  ! number of passed variables (densities and pressures)
  integer, public :: nVarCouple=0

  ! indexes of passed variables
  integer, public, allocatable :: iVarCouple_V(:)

  ! Local variables

  ! The size of the IM grid
  integer :: iSize, jSize

contains
  !============================================================================
  subroutine im_pressure_init(iSizeIn,jSizeIn)
    use ModAdvance,    ONLY: UseMultiSpecies, nSpecies
    integer :: iSizeIn, jSizeIn

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'im_pressure_init'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    iSize = iSizeIn
    jSize = jSizeIn
    allocate(&
         IM_lat(iSize), &
         IM_lon(jSize), &
         ImP_CV(iSize,jSize,nFluid), &
         IsImP_I(nFluid), IsImPpar_I(nFluid))

    if (UseMultiSpecies) then
       allocate(ImRho_CV(iSize,jSize,nSpecies+1), IsImRho_I(nSpecies+1))
    else
       allocate(ImRho_CV(iSize,jSize,nFluid), IsImRho_I(nFluid))
    endif

    allocate(ImPpar_CV(iSize,jSize,nFluid), &
         IM_bmin(iSize,jSize))

    call test_stop(NameSub, DoTest)
  end subroutine im_pressure_init
  !============================================================================
  subroutine get_im_pressure(iBlock, nDensity,pIm_IC, RhoIm_IC, TauCoeffIm_C, &
       PparIm_IC)

    use ModMain,     ONLY : nI, nJ, nK, DoFixPolarRegion, rFixPolarRegion, &
         dLatSmoothIm, UseB0
    use ModFieldTrace, ONLY : ray
    use ModPhysics,  ONLY : &
         Si2No_V, UnitB_, UnitP_, UnitRho_, PolarRho_I, PolarP_I
    use ModGeometry, ONLY : R_BLK, Xyz_DGB, z_
    use ModAdvance,  ONLY : State_VGB, RhoUz_, Bx_, Bz_, UseMultiSpecies
    use ModB0,       ONLY: B0_DGB
    use ModVarIndexes, ONLY: IonFirst_, IonLast_, IsMhd

    integer, intent(in)  :: iBlock,nDensity

    real,    intent(out) :: pIm_IC(nFluid,1:nI, 1:nJ, 1:nK)
    real,    intent(out) :: RhoIm_IC(nDensity,1:nI, 1:nJ, 1:nK)
    real,    intent(out) :: TauCoeffIm_C(1:nI, 1:nJ, 1:nK)
    real,    intent(out) :: PparIm_IC(nFluid,1:nI, 1:nJ, 1:nK)

    real    :: BminIm_C(1:nI, 1:nJ, 1:nK), b_D(3)

    integer :: i,j,k, iFluid, n, iLat1,iLat2, iLon1,iLon2, iDensity

    real :: Lat,Lon, LatWeight1,LatWeight2, LonWeight1,LonWeight2
    real :: LatMaxIm, LatMinIm
    ! variables for anisotropic pressure coupling
    real :: Pperp, PperpInvPpar, Coeff

    ! Keep track if IM grid is defined in the northern of southern hemisphere
    logical :: IsSouthImGrid = .false.

    integer :: iIonSecond, nIons

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_im_pressure'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    iIonSecond = min(IonFirst_+1, IonLast_)
    ! if (DoMultiFluidIMCoupling) then
    !   nIons = iIonSecond
    ! else
    !   nIons = 1
    ! end if

    TauCoeffIm_C = 1.0

    ! Maximum latitude (ascending or descending) of the IM grid
    LatMaxIm = max(IM_lat(1), IM_lat(iSize))
    LatMinIm = min(IM_lat(1), IM_lat(iSize))

    ! Determine if IM grid is defined in the north or south
    if (LatMaxIm < 0.0) then
       IsSouthImGrid = .true.
    else
       IsSouthImGrid = .false.
    endif

    ! Check to see if cell centers are on closed fieldline
    do k=1,nK; do j=1,nJ; do i=1,nI

       ! Default is negative, which means that do not nudge GM values
       pIm_IC(:,i,j,k)   = -1.0
       RhoIm_IC(:,i,j,k) = -1.0
       PparIm_IC(:,i,j,k)   = -1.0
       BminIm_C(i,j,k)   = -1.0

       ! For closed field lines nudge towards IM pressure/density
       if(nint(ray(3,1,i,j,k,iBlock)) == 3) then

          ! Map the point down to the IM grid
          ! Note: ray values are in SM coordinates!
          if (IsSouthImGrid) then
             Lat = ray(1,2,i,j,k,iBlock)
          else
             Lat = ray(1,1,i,j,k,iBlock)
          endif

          ! write(*,*) i,j,k,iBlock,Xyz_DGB(1:3,i,j,k,iBlock),
          !    Lat,ray(1,2,i,j,k,iBlock),LatMaxIm
          ! Do not modify pressure along field lines outside the IM grid
          if(Lat > LatMaxIm .or. Lat < LatMinIm) CYCLE

          if (IsSouthImGrid) then
             Lon = ray(2,2,i,j,k,iBlock)
          else
             Lon = ray(2,1,i,j,k,iBlock)
          endif

          if (IM_lat(1) > IM_lat(2)) then
             ! IM_lat is in descending order
             do iLat1 = 2, iSize
                if(Lat > IM_lat(iLat1)) EXIT
             end do
             iLat2 = iLat1-1
             LatWeight1 = (Lat - IM_lat(iLat2))/(IM_lat(iLat1) - IM_lat(iLat2))
             LatWeight2 = 1 - LatWeight1
          else
             ! IM lat is in ascending order
             do iLat1 = 2, iSize
                if(Lat < IM_lat(iLat1)) EXIT
             end do
             iLat2 = iLat1-1
             LatWeight1 = &
                  (Lat - IM_lat(iLat2))/(IM_lat(iLat1) - IM_lat(iLat2))
             LatWeight2 = 1 - LatWeight1
          endif

          ! Note: IM_lon is in ascending order
          if(Lon < IM_lon(1)) then
             ! periodic before 1
             iLon1 = 1
             iLon2 = jSize
             LonWeight1 =     (Lon           + 360 - IM_lon(iLon2)) &
                  /           (IM_lon(iLon1) + 360 - IM_lon(iLon2))
          elseif(Lon > IM_lon(jSize)) then
             ! periodic after jSize
             iLon1 = 1
             iLon2 = jSize
             LonWeight1 = (Lon                 - IM_lon(iLon2)) &
                  /       (IM_lon(iLon1) + 360 - IM_lon(iLon2))
          else
             do iLon1 = 2, jSize
                if(Lon < IM_lon(iLon1)) EXIT
             end do
             iLon2 = iLon1-1
             LonWeight1 = (Lon           - IM_lon(iLon2)) &
                  /       (IM_lon(iLon1) - IM_lon(iLon2))
          end if
          LonWeight2 = 1 - LonWeight1

          DENSITY: do iDensity=1,nDensity
             ! check if density is available from IM, if not cycle to next
             if (.not. IsImRho_I(iDensity)) CYCLE DENSITY
             if(all( ImRho_CV( [iLat1,iLat2], &
                  [iLon1, iLon2],iDensity)&
                  > 0.0 ))then
                RhoIm_IC(iDensity,i,j,k) = Si2No_V(UnitRho_)*( &
                     LonWeight1 * ( LatWeight1*ImRho_CV(iLat1,iLon1,iDensity) &
                     +              LatWeight2*ImRho_CV(iLat2,iLon1,iDensity))+&
                     LonWeight2 * ( LatWeight1*ImRho_CV(iLat1,iLon2,iDensity) &
                     +              LatWeight2*ImRho_CV(iLat2,iLon2,iDensity)))
             end if

          end do DENSITY

          FLUID: do iFluid=1,nFluid
             ! check if fluid is available from IM, if not cycle to next
             if (.not. IsImP_I(iFluid)) CYCLE FLUID
             if(all( ImP_CV( [iLat1,iLat2], &
                  [iLon1, iLon2],iFluid ) &
                  > 0.0 ))then
                pIm_IC(iFluid,i,j,k) = Si2No_V(UnitP_)*( &
                     LonWeight1 * ( LatWeight1*ImP_CV(iLat1,iLon1,iFluid) &
                     +              LatWeight2*ImP_CV(iLat2,iLon1,iFluid) ) + &
                     LonWeight2 * ( LatWeight1*ImP_CV(iLat1,iLon2,iFluid) &
                     +              LatWeight2*ImP_CV(iLat2,iLon2,iFluid) ) )

             ! ppar at minimum B
             if(DoAnisoPressureIMCoupling .and. IsImPpar_I(iFluid) )then
                PparIm_IC(iFluid,i,j,k) = Si2No_V(UnitP_)*( &
                     LonWeight1 * ( LatWeight1*ImPpar_CV(iLat1,iLon1,iFluid) &
                     +              LatWeight2*ImPpar_CV(iLat2,iLon1,iFluid) )+&
                     LonWeight2 * ( LatWeight1*ImPpar_CV(iLat1,iLon2,iFluid) &
                     +              LatWeight2*ImPpar_CV(iLat2,iLon2,iFluid) ) )
                BminIm_C(i,j,k) = Si2No_V(UnitB_)*( &
                     LonWeight1 * ( LatWeight1*IM_bmin(iLat1,iLon1) &
                     +              LatWeight2*IM_bmin(iLat2,iLon1) ) + &
                     LonWeight2 * ( LatWeight1*IM_bmin(iLat1,iLon2) &
                     +              LatWeight2*IM_bmin(iLat2,iLon2) ) )
             end if

             if(.not. DoAnisoPressureIMCoupling &
                  .or. .not. IsImPpar_I(iFluid))then
                ! If coupled with RCM or if GM is not using anisotropic
                ! pressure then set ppar = p.
                PparIm_IC(iFluid,i,j,k) = pIm_IC(iFluid,i,j,k)
             else
                ! Anisotropic pressure coupling
                ! Pperp at minimum B
                Pperp = (3.0*pIm_IC(iFluid,i,j,k) - PparIm_IC(iFluid,i,j,k))/2.0
                PperpInvPpar = Pperp/PparIm_IC(iFluid,i,j,k)
                b_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
                if(UseB0) b_D = b_D + B0_DGB(:,i,j,k,iBlock)
                Coeff = 1/(PperpInvPpar &
                     + min(1.0, &
                     BminIm_C(i,j,k)/norm2(b_D))*(1 - PperpInvPpar))

                ! pressures and density at arbitrary location of a field line
                pIm_IC(iFluid,i,j,k) = pIm_IC(iFluid,i,j,k)*Coeff &
                     + 2.0*Pperp*(Coeff - 1)*Coeff/3.0
                PparIm_IC(iFluid,i,j,k) = PparIm_IC(iFluid,i,j,k)*Coeff
                RhoIm_IC(iFluid,i,j,k) = RhoIm_IC(iFluid,i,j,k)*Coeff
             end if

             if(dLatSmoothIm > 0.0)then
                ! Go from low to high lat and look for first unset field line
                ! !! WHAT ABOUT ASCENDING VS DESCENDING ORDER FOR LAT???
                do n = iSize,1,-1
                   if(ImP_CV(n,iLon1,iFluid) < 0.0) EXIT
                enddo
                ! Make sure n does not go below 1
                n = max(1, n)
                ! Set TauCoeff as a function of lat distance from unset lines
                ! No adjustment at the unset line, full adjustment if latitude
                ! difference exceeds dLatSmoothIm
                TauCoeffIm_C(i,j,k) = &
                     min( abs(IM_lat(n) - IM_lat(iLat1))/dLatSmoothIm, 1.0 )
             end if
             end if
          end do FLUID
       end if

       ! If the pressure is not set by IM, and DoFixPolarRegion is true
       ! and the cell is within radius rFixPolarRegion and flow points outward
       ! then nudge the pressure (and density) towards the "polarregion" values
       if(pIm_IC(1,i,j,k) < 0.0 .and. DoFixPolarRegion .and. &
            R_BLK(i,j,k,iBlock) < rFixPolarRegion .and. &
            Xyz_DGB(z_,i,j,k,iBlock)*State_VGB(RhoUz_,i,j,k,iBlock) > 0)then
          do iFluid = 1, nFluid
             pIm_IC(iFluid,i,j,k) = PolarP_I(iFluid)
          end do
          do iDensity = 1, nDensity
             RhoIm_IC(iDensity,i,j,k) = PolarRho_I(iDensity)
          end do
       end if

    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine get_im_pressure
  !============================================================================
  subroutine apply_im_pressure

    use ModMain, ONLY: nI, nJ, nK, nBlock, Unused_B, iNewGrid, TauCoupleIm, &
         time_accurate, Dt, DoCoupleImPressure, DoCoupleImDensity, RhoMinDimIm
    use ModAdvance, ONLY: State_VGB, UseAnisoPressure, UseMultiSpecies, &
         nSpecies, iStateCPU, sync_state
    use ModVarIndexes, ONLY: Rho_, SpeciesFirst_, Ppar_
    use ModPhysics, ONLY: Io2No_V, UnitT_, UnitRho_
    use ModMultiFluid, ONLY : IonFirst_, IonLast_, iRho_I, iP_I, &
         iRhoUx_I, iRhoUy_I, iRhoUz_I
    use ModFieldTraceFast, ONLY: trace_field_grid
    use ModB0, ONLY: B0_DGB

    real :: Factor

    real,allocatable :: RhoIm_IC(:,:,:,:)
    real :: RhoMinIm
    real :: pIm_IC(nFluid,nI,nJ,nK)
    real :: TauCoeffIm_C(nI,nJ,nK)
    real :: PparIm_IC(nFluid,nI,nJ,nK)

    integer :: iLastPIm = -1, iLastGrid = -1
    integer :: iIonSecond, nIons
    integer :: i, j, k, iBlock, iFluid, nDensity, iDensity
    integer, allocatable:: iDens_I(:)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'apply_im_pressure'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(iNewPIm < 1) RETURN ! No IM pressure has been obtained yet

    ! Are we coupled at all?
    if(.not.DoCoupleImPressure .and. .not.DoCoupleImDensity) RETURN

    call timing_start(NameSub)

    call sync_state

    iIonSecond = min(IonFirst_+1, IonLast_)

    ! Set density floor in normalized units
    if(DoCoupleImDensity) RhoMinIm = Io2No_V(UnitRho_)*RhoMinDimIm

    ! redo ray tracing if necessary
    ! (load_balance takes care of new decomposition)
    if(iNewPIm > iLastPIm .or. iNewGrid > iLastGrid) then
       if(DoTest)write(*,*)'GM_apply_im_pressure: call trace_field_grid ',&
            'iNewPIm,iLastPIm,iNewGrid,iLastGrid=',&
            iNewPIm,iLastPIm,iNewGrid,iLastGrid
       call trace_field_grid
    end if

    ! Remember this call
    iLastPIm = iNewPIm; iLastGrid = iNewGrid

    ! Now use the pressure from the IM to nudge the pressure in the MHD code.
    ! This will happen only on closed magnetic field lines.
    ! Determining which field lines are closed is done by using the ray
    ! tracing.

    if(time_accurate)then
       ! Ramp up is based on physical time: p' = p + min(1,dt/tau) * (pIM - p)
       ! A typical value might be 5, to get close to the IM pressure
       ! in 10 seconds. Dt/Tau is limited to 1, when p' = pIM is set

       Factor = min(1.0, Dt/(TauCoupleIM*Io2No_V(UnitT_)))

    else
       ! Ramp up is based on number of iterations: p' = (ntau*p + pIm)/(1+ntau)
       ! A typical value might be 20, to get close to the IM pressure
       ! in 20 iterations

       Factor = 1.0/(1 + TauCoupleIM)

    end if

!    if (DoMultiFluidIMCoupling)then
!       ! Number of fluids: 1 for multispecies, 2 for multiion, 3 for MHD-ions
!       nIons = iIonSecond
!    else
!       nIons = 1
!    end if

    ! Set array of density indexes:
    ! nSpecies for multispecies, nFluid for multifluid
    if(UseMultiSpecies)then
       nDensity = nSpecies+1
       allocate(iDens_I(nDensity))
       ! first density with multispecies is the total
       iDens_I(1) = Rho_
       ! subsequent densities are the species
       do iDensity=2,nDensity
          iDens_I(iDensity) = SpeciesFirst_+iDensity-2
       enddo
    else
       nDensity = nFluid ! nIons
       allocate(iDens_I(nDensity))
       iDens_I = iRho_I(1:nFluid)!(1:nIons)
    end if

    if (.not.allocated(RhoIm_IC)) allocate(RhoIm_IC(nDensity,nI,nJ,nK))

    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE

       call get_im_pressure(iBlock, nDensity, pIm_IC, RhoIm_IC, TauCoeffIm_C, &
            PparIm_IC)
       if(all(pIm_IC < 0.0)) CYCLE  ! Nothing to do

       ! Put velocity into momentum temporarily when density is changed
       if(DoCoupleImDensity)then
          do iFluid = 1, nFluid
             where(RhoIm_IC(iFluid,:,:,:) > 0.0)
                State_VGB(iRhoUx_I(iFluid),1:nI,1:nJ,1:nK,iBlock)= &
                     State_VGB(iRhoUx_I(iFluid),1:nI,1:nJ,1:nK,iBlock)/ &
                     State_VGB(iRho_I(iFluid),1:nI,1:nJ,1:nK,iBlock)
                State_VGB(iRhoUy_I(iFluid),1:nI,1:nJ,1:nK,iBlock)= &
                     State_VGB(iRhoUy_I(iFluid),1:nI,1:nJ,1:nK,iBlock)/ &
                     State_VGB(iRho_I(iFluid),1:nI,1:nJ,1:nK,iBlock)
                State_VGB(iRhoUz_I(iFluid),1:nI,1:nJ,1:nK,iBlock)= &
                     State_VGB(iRhoUz_I(iFluid),1:nI,1:nJ,1:nK,iBlock)/ &
                     State_VGB(iRho_I(iFluid),1:nI,1:nJ,1:nK,iBlock)
             end where
          end do
       end if

       if(time_accurate)then
          if(DoCoupleImPressure)then
             APPLY_P: do iFluid = 1, nFluid! nIons
                if (.not. IsImP_I(iFluid)) CYCLE APPLY_P
                where(pIm_IC(iFluid,:,:,:) > 0.0) &
                     State_VGB(iP_I(iFluid),1:nI,1:nJ,1:nK,iBlock) = &
                     State_VGB(iP_I(iFluid),1:nI,1:nJ,1:nK,iBlock)   &
                     + Factor * TauCoeffIm_C &
                     * (pIm_IC(iFluid,:,:,:) - &
                     State_VGB(iP_I(iFluid),1:nI,1:nJ,1:nK,iBlock))
                if(UseAnisoPressure)then
                   where(PparIm_IC(iFluid,:,:,:) > 0.0) &
                        State_VGB(Ppar_,1:nI,1:nJ,1:nK,iBlock) = &
                        State_VGB(Ppar_,1:nI,1:nJ,1:nK,iBlock)   &
                        + Factor * TauCoeffIm_C &
                        * (PparIm_IC(iFluid,:,:,:) - &
                        State_VGB(Ppar_,1:nI,1:nJ,1:nK,iBlock))
                end if
             end do APPLY_P
          end if
          if(DoCoupleImDensity)then
             ! Negative first fluid density signals cells not covered by IM
             APPLY_DENS: do iDensity = 1,nDensity
                if (.not. IsImRho_I(iDensity)) CYCLE APPLY_DENS
                do k = 1, nK; do j = 1, nJ; do i = 1, nI
                   if(RhoIm_IC(iDensity,i,j,k) <= 0.0 ) CYCLE
                   ! Here iDens_I can index multiple species or fluids
                   State_VGB(iDens_I(iDensity),i,j,k,iBlock) = max( RhoMinIm, &
                        State_VGB(iDens_I(iDensity),i,j,k,iBlock) &
                        + Factor * TauCoeffIm_C(i,j,k) &
                        * (RhoIm_IC(iDensity,i,j,k) &
                        - State_VGB(iDens_I(iDensity),i,j,k,iBlock)))
                end do; end do; end do
             end do APPLY_DENS
          end if
       else
          if(DoCoupleImPressure)then
             APPLY_P2: do iFluid = 1, nFluid! nIons
                if (.not. IsImP_I(iFluid)) CYCLE APPLY_P2
                where(pIm_IC(iFluid,:,:,:) > 0.0) &
                     State_VGB(iP_I(iFluid),1:nI,1:nJ,1:nK,iBlock) = Factor* &
                     (TauCoupleIM &
                     *State_VGB(iP_I(iFluid),1:nI,1:nJ,1:nK,iBlock)+&
                     pIm_IC(iFluid,:,:,:))
                if(UseAnisoPressure)then
                   where(PparIm_IC(iFluid,:,:,:) > 0.0) &
                        State_VGB(Ppar_,1:nI,1:nJ,1:nK,iBlock) = Factor* &
                        (TauCoupleIM*State_VGB(Ppar_,1:nI,1:nJ,1:nK,iBlock) + &
                        PparIm_IC(iFluid,:,:,:))
                end if
             end do APPLY_P2
          end if
          if(DoCoupleImDensity)then
             APPLY_DENS2: do iDensity = 1,nDensity
                if (.not. IsImRho_I(iDensity)) CYCLE APPLY_DENS2
                do k = 1, nK; do j = 1, nJ; do i = 1,nI
                   if(RhoIm_IC(1,i,j,k) <= 0.0) CYCLE
                   State_VGB(iDens_I,i,j,k,iBlock) = &
                        max(RhoMinIm, Factor*( &
                        TauCoupleIM*State_VGB(iDens_I,i,j,k,iBlock)&
                        + RhoIm_IC(1:nDensity,i,j,k)))
                end do; end do; end do
             end do APPLY_DENS2
          end if
       end if
       ! Convert back to momentum
       if(DoCoupleImDensity)then
          do iFluid = 1, nFluid! nIons
             where(RhoIm_IC(iFluid,:,:,:) > 0.0)
                State_VGB(iRhoUx_I(iFluid),1:nI,1:nJ,1:nK,iBlock)= &
                     State_VGB(iRhoUx_I(iFluid),1:nI,1:nJ,1:nK,iBlock)* &
                     State_VGB(iRho_I(iFluid),1:nI,1:nJ,1:nK,iBlock)
                State_VGB(iRhoUy_I(iFluid),1:nI,1:nJ,1:nK,iBlock)= &
                     State_VGB(iRhoUy_I(iFluid),1:nI,1:nJ,1:nK,iBlock)* &
                     State_VGB(iRho_I(iFluid),1:nI,1:nJ,1:nK,iBlock)
                State_VGB(iRhoUz_I(iFluid),1:nI,1:nJ,1:nK,iBlock)= &
                     State_VGB(iRhoUz_I(iFluid),1:nI,1:nJ,1:nK,iBlock)* &
                     State_VGB(iRho_I(iFluid),1:nI,1:nJ,1:nK,iBlock)
             end where
          end do
       end if

    end do

    if(allocated(iDens_I)) deallocate(iDens_I)

    iStateCPU = iStateCPU + 1
    call sync_state

    call timing_stop(NameSub)
    call test_stop(NameSub, DoTest)

  end subroutine apply_im_pressure
  !============================================================================
end module ModImCoupling
!==============================================================================
