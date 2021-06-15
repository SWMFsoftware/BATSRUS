!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModAdvance

  use BATL_lib, ONLY: &
       test_start, test_stop, iProc, nProc

  use ModSize
  use ModVarIndexes
  use ModSemiImplVar, ONLY: UseStableImplicit
  use ModMultiFluid, ONLY: UseMultiIon
  use ModMain,       ONLY: UseB, UseRotatingFrame, UseGravity, &
       iMinFace, iMaxFace, jMinFace, jMaxFace, kMinFace, kMaxFace, &
       iMinFace2, iMaxFace2, jMinFace2, jMaxFace2, kMinFace2, kMaxFace2, &
       nIFace, nJFace, nKFace
  use ModIO,         ONLY: iUnitOut, write_prefix
  use ModFaceFluxParameters
  use omp_lib

  implicit none
  save

  public:: init_mod_advance
  public:: clean_mod_advance

  ! Update method
  character(len=10):: TypeUpdate = 'orig'
  integer:: iTypeUpdate = 1
  integer, parameter:: UpdateOrig_ = 1, UpdateSlow_ = 2, UpdateFast_ = 3

  ! This should be changed to false probably
  logical:: UseDbTrick = .true., UseDbTrickNow = .true.

  ! Numerical flux type
  character(len=10) :: FluxType

  ! Total number of fluxes and sources: nVar state variables + nFluid energies
  integer, parameter:: nFlux = nVar + nFluid, nSource = nVar + nFluid

  ! indexes for other face centered variables
  integer, parameter :: &
       UnFirst_ = nFlux+1, UnLast_ = UnFirst_ + nFluid, &
       Vdt_ = UnLast_ + 1

  ! The normal components of the magnetic field is exchaned only for
  ! B_>U_ (UseB_ is true)
  integer, parameter :: BnL_ = Vdt_ + min(1, B_-U_)
  integer, parameter :: BnR_ = BnL_ + min(1, B_-U_)

  integer, parameter :: nFaceValue = BnR_

  ! For momentum conserving scheme (for hybrid or multi-fluid) Mhd flux of
  ! momentum should be saved, the condition is UseB_ (B_-U_>0) and not
  ! UseEField (Ex_>1)
  integer, parameter :: MhdRhoUx_ = BnR_ +        min(max(2-Ex_,0), B_-U_)
  integer, parameter :: MhdRhoUz_ = BnR_ + MaxDim*min(max(2-Ex_,0), B_-U_)
  integer, parameter :: nCorrectedFaceValue = MhdRhoUz_

  ! Logical and number of species for multi-species equations
  logical, parameter:: UseMultiSpecies = SpeciesFirst_ > 1
  integer, parameter:: nSpecies = SpeciesLast_ - SpeciesFirst_ + 1

  ! Named index for electron pressure and velocity
  integer, parameter:: eFluid_ = nFluid + 1

  ! Additional equations are activated by the named indices of the
  ! corresponding extra variables in the equation modules.
  ! The default values of these named indices are in ModExtraVariables.
  logical, parameter:: UseElectronPressure = Pe_ > 1
  logical, parameter:: UseAnisoPressure    = iPparIon_I(IonFirst_) > 1
  logical, parameter:: UseAnisoPe          = Pepar_ > 1
  logical, parameter:: UseIdealEos = ExtraEint_ == 1
  logical, parameter:: UseEfield = Ex_ > 1

  ! Use entropy equation for electrons instead of pressure by default
  ! This should provide more robust results near strong shocks
  logical:: UseElectronEntropy = .false. !!! UseElectronPressure

  logical:: UseWavePressure = .false.
  !$acc declare create(UseWavePressure)

  logical:: DoCalcElectricField = .false.

  ! Update check parameters
  logical :: UseUpdateCheck = .false.
  real :: percent_max_rho(2), percent_max_p(2)

  ! The percentage limit for species to be checked in update check
  real :: SpeciesPercentCheck = 1.0

  ! Replace density with sum of species densities (in multi-species plasma)
  logical :: DoReplaceDensity = .true.

  ! Enforce all ion fluids to the same velocity and/or temperature
  ! in multi-ion plasma. If both logicals are true then it should
  ! behave like multi-species MHD.
  logical :: UseSingleIonVelocity    = .false.
  logical :: UseSingleIonTemperature = .false.

  ! Block cell-centered MHD solution
  real, allocatable, target :: State_VGB(:,:,:,:,:)
  !$acc declare create(State_VGB)

  ! Block cell-centered MHD solution old state
  real, allocatable :: StateOld_VGB(:,:,:,:,:)
  !$acc declare create(StateOld_VGB)

  ! Block cell-centered intrinsic magnetic field, time, and temporary storage
  real, allocatable :: tmp1_BLK(:,:,:,:)
  real, allocatable :: tmp2_BLK(:,:,:,:)

  real, allocatable :: time_BLK(:,:,:,:)
  !$acc declare create(time_BLK)

  ! Electric field including numerical flux
  real, allocatable :: ExNum_CB(:,:,:,:)
  real, allocatable :: EyNum_CB(:,:,:,:)
  real, allocatable :: EzNum_CB(:,:,:,:)

  real, public, allocatable:: Efield_DGB(:,:,:,:,:)

  ! Local cell-centered source terms and divB.
  real, allocatable :: Source_VCI(:,:,:,:,:)
  real, allocatable :: SourceMhd_VCI(:,:,:,:,:)
  !$omp threadprivate( Source_VCI, SourceMhd_VCI )
  !$acc declare create( Source_VCI, SourceMhd_VCI )

  real, allocatable :: Source_VCB(:,:,:,:,:)

  ! Extra source terms coming from other models in the SWMF
  ! It should be allocated in the coupler
  real, allocatable :: ExtraSource_ICB(:,:,:,:,:)

  real, allocatable :: DivB1_GB(:,:,:,:)
  !$acc declare create(DivB1_GB)

  ! Switch between low and high order schemes
  logical:: UseLowOrder = .false.  ! some faces are low order
  logical, allocatable:: IsLowOrderOnly_B(:) ! Is the whole block low order?
  !$acc declare create(IsLowOrderOnly_B)
  logical:: UseLowOrderRegion = .false.
  logical:: UseAdaptiveLowOrder = .false.
  real, allocatable:: &
       LowOrderCrit_XB(:,:,:,:), &
       LowOrderCrit_YB(:,:,:,:), &
       LowOrderCrit_ZB(:,:,:,:) ! The ratio of the low order face values

  ! Cell centered velocities in ijk direction.
  real, allocatable:: Vel_IDGB(:,:,:,:,:,:)

  ! Face centered variables for the current block

  ! Primitive variables (velocity) extrapolated from left and right
  real, allocatable:: LeftState_VX(:,:,:,:), RightState_VX(:,:,:,:)
  real, allocatable:: LeftState_VY(:,:,:,:), RightState_VY(:,:,:,:)
  real, allocatable:: LeftState_VZ(:,:,:,:), RightState_VZ(:,:,:,:)
  !$omp threadprivate( LeftState_VX, RightState_VX)
  !$omp threadprivate( LeftState_VY, RightState_VY)
  !$omp threadprivate( LeftState_VZ, RightState_VZ)
  !$acc declare create(LeftState_VX, RightState_VX)
  !$acc declare create(LeftState_VY, RightState_VY)
  !$acc declare create(LeftState_VZ, RightState_VZ)

  ! primitive variables
  real, allocatable:: Primitive_VGI(:,:,:,:,:)
  !$omp threadprivate( Primitive_VGI )
  !$acc declare create(Primitive_VGI )

  ! Face centered div(U)*dl
  real, allocatable, dimension(:,:,:,:):: &
       FaceDivU_IX, FaceDivU_IY, FaceDivU_IZ
  !$omp threadprivate( FaceDivU_IX, FaceDivU_IY, FaceDivU_IZ )

  ! Fluxes are for all state variables including energies,
  ! for source terrms (div U, div B), and for time step calculatino Vmax*dt.
  real, allocatable:: &
       Flux_VXI(:,:,:,:,:), Flux_VYI(:,:,:,:,:), Flux_VZI(:,:,:,:,:)
  !$omp threadprivate( Flux_VXI, Flux_VYI, Flux_VZI )
  !$acc declare create( Flux_VXI, Flux_VYI, Flux_VZI )

  ! Cell centered fluxes
  logical:: DoInterpolateFlux = .false.
  real, allocatable:: FluxLeft_VGD(:,:,:,:,:), FluxRight_VGD(:,:,:,:,:)
  !$omp threadprivate( FluxLeft_VGD, FluxRight_VGD )

  ! Variables for ECHO scheme
  logical:: UseFDFaceFlux = .false., DoCorrectFace = .false.
  !$acc declare create(DoCorrectFace, UseFDFaceFlux)
  real, allocatable:: FluxCenter_VGD(:,:,:,:,:)
  !$omp threadprivate( FluxCenter_VGD )

  ! Magnetic field cross area vector for J x B source term in multi-ion MHD
  real, allocatable, dimension(:,:,:,:):: &
       bCrossArea_DX, bCrossArea_DY, bCrossArea_DZ
  !$omp threadprivate( bCrossArea_DX, bCrossArea_DY, bCrossArea_DZ )

  ! Mhd part of the momentum flux. May be subtracted for calculating
  ! electric field
  !
  ! Logical determining in which case the Mhd part of the momentum flux is
  ! calculated. If calculated, the electric field \nabla x B x B - \nable P_e
  ! may be calculated as the divergence of momentum fluxes, resulting in
  ! momentum-conserving schemes for hybrid and multifluids. Not calculated
  ! if UseEfield, in which case the electric field is part of the state vector.
  logical, parameter:: UseMhdMomentumFlux = UseB .and. .not.UseEfield

  real, allocatable:: MhdSource_VCI(:,:,:,:),  &
       MhdFlux_VXI(:,:,:,:,:) , MhdFlux_VYI(:,:,:,:,:) , MhdFlux_VZI(:,:,:,:,:)
  !$omp threadprivate( MhdFlux_VXI, MhdFlux_VYI, MhdFlux_VZI )
  !$acc declare create(MhdFlux_VXI, MhdFlux_VYI, MhdFlux_VZI)

  ! Merge cells around the polar axis in spherical geometry
  logical :: DoFixAxis = .false.
  real ::    rFixAxis = 0.0, r2FixAxis = 0.0

  ! Block type information
  integer, allocatable :: iTypeAdvance_B(:)
  integer, allocatable :: iTypeAdvance_BP(:,:)

  ! Named indexes for block types
  integer, parameter :: &
       SkippedBlock_=0,     & ! Blocks which were unused originally.
       SteadyBlock_=1,      & ! Blocks which do not change
       SteadyBoundBlock_=2, & ! Blocks surrounding the evolving blocks
       ExplBlock_=3,        & ! Blocks changing with the explicit scheme
       ImplBlock_=4           ! Blocks changing with the implicit scheme

contains
  !============================================================================
  subroutine init_mod_advance

    ! These arrays may need allocation depending on the parameters

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_mod_advance'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoCalcElectricField .and. .not.allocated(ExNum_CB))then
       allocate(ExNum_CB(nI,nJ,nK,MaxBlock))
       allocate(EyNum_CB(nI,nJ,nK,MaxBlock))
       allocate(EzNum_CB(nI,nJ,nK,MaxBlock))
    end if

    ! In case electric field is not a part of the state vector, it may
    ! be expressed in terms of the MhdMomentum flux and stored into
    ! Efield_DGB array
    if(UseMhdMomentumFlux &
         .and. .not.allocated(Efield_DGB))then
       allocate(Efield_DGB(3,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
       Efield_DGB = 0.0
    end if
    if(UseStableImplicit) then
       allocate(Source_VCB(nVar, nI, nJ, nK, MaxBlock))
       Source_VCB = 0.0
    endif

#ifdef OPENACC
    nGang = MaxBlock
#endif

    !$omp parallel
    if(UseB .and. (UseMultiIon .or. .not.IsMhd) &
         .and. .not. allocated(bCrossArea_DX))then
       allocate(bCrossArea_DX( &
            MaxDim,nI+1,jMinFace:jMaxFace,kMinFace:kMaxFace))
       allocate(bCrossArea_DY( &
            MaxDim,iMinFace:iMaxFace,nJ+1,kMinFace:kMaxFace))
       allocate(bCrossArea_DZ( &
            MaxDim,iMinFace:iMaxFace,jMinFace:jMaxFace,nK+1))
       bCrossArea_DX = 0.0; bCrossArea_DY = 0.0; bCrossArea_DZ = 0.0
    end if
    !$omp end parallel

    if(allocated(State_VGB)) RETURN

    ! The arrays below are allocated at the beginning (if at all)
    if(UseB)then
       allocate(DivB1_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
       DivB1_GB = 0.0
    end if
    allocate(State_VGB(nVar,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
    allocate(StateOld_VGB(nVar,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
    allocate(tmp1_BLK(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
    allocate(tmp2_BLK(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
    allocate(time_BLK(nI,nJ,nK,MaxBlock))
    allocate(iTypeAdvance_BP(MaxBlock,0:nProc-1))
    allocate(iTypeAdvance_B(MaxBlock))

    allocate(IsLowOrderOnly_B(MaxBlock))
    IsLowOrderOnly_B = .true.

    iTypeAdvance_B  = SkippedBlock_
    iTypeAdvance_BP = SkippedBlock_

    !$omp parallel
    ! The current implementation of the constrained transport scheme
    ! requires fluxes between ghost cells. Should be eliminated, and then
    ! all faces would be allocated to the usual nI+1,nJ,nK and permutations.
    allocate(LeftState_VX( &
         nVar,nI+1,jMinFace2:jMaxFace2,kMinFace2:kMaxFace2))
    allocate(RightState_VX( &
         nVar,nI+1,jMinFace2:jMaxFace2,kMinFace2:kMaxFace2))
    allocate(Flux_VXI( &
         nFaceValue,nI+1,jMinFace:jMaxFace,kMinFace:kMaxFace,nGang))
    allocate(MhdFlux_VXI( &
         RhoUx_:RhoUz_,nI+1,jMinFace:jMaxFace,kMinFace:kMaxFace,nGang))
    Flux_VXI = 0.0; MhdFlux_VXI = 0.0

    allocate(LeftState_VY( &
         nVar,iMinFace2:iMaxFace2,nJ+1,kMinFace2:kMaxFace2))
    allocate(RightState_VY( &
         nVar,iMinFace2:iMaxFace2,nJ+1,kMinFace2:kMaxFace2))
    allocate(Flux_VYI( &
         nFaceValue,iMinFace:iMaxFace,nJ+1,kMinFace:kMaxFace,nGang))
    allocate(MhdFlux_VYI( &
         RhoUx_:RhoUz_,iMinFace:iMaxFace,nJ+1,kMinFace:kMaxFace,nGang))
    Flux_VYI = 0.0; MhdFlux_VYI = 0.0

    allocate(LeftState_VZ( &
         nVar,iMinFace2:iMaxFace2,jMinFace2:jMaxFace2,nK+1))
    allocate(RightState_VZ( &
         nVar,iMinFace2:iMaxFace2,jMinFace2:jMaxFace2,nK+1))
    allocate(Flux_VZI( &
         nFaceValue,iMinFace:iMaxFace,jMinFace:jMaxFace,nK+1,nGang))
    allocate(MhdFlux_VZI( &
         RhoUx_:RhoUz_,iMinFace:iMaxFace,jMinFace:jMaxFace,nK+1,nGang))
    Flux_VZI = 0.0; MhdFlux_VZI = 0.0

    allocate(FaceDivU_IX( &
         nFluid,nIFace,jMinFace:jMaxFace,kMinFace:kMaxFace))
    allocate(FaceDivU_IY( &
         nFluid,iMinFace:iMaxFace,nJFace,kMinFace:kMaxFace))
    allocate(FaceDivU_IZ( &
         nFluid,iMinFace:iMaxFace,jMinFace:jMaxFace,nKFace))

    allocate(Primitive_VGI(nVar,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,nGang))

    allocate(Source_VCI(nSource,nI,nJ,nK,nGang))
    Source_VCI = 0.0
    allocate(SourceMhd_VCI(RhoUx_:RhoUz_,nI,nJ,nK,nGang))
    !$omp end parallel

    if(iProc==0)then
       call write_prefix
       write(iUnitOut,'(a)') 'init_mod_advance allocated arrays'
    end if

    call test_stop(NameSub, DoTest)
  end subroutine init_mod_advance
  !============================================================================
  subroutine clean_mod_advance

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'clean_mod_advance'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(allocated(State_VGB))       deallocate(State_VGB)
    if(allocated(StateOld_VGB))    deallocate(StateOld_VGB)
    if(allocated(tmp1_BLK))        deallocate(tmp1_BLK)
    if(allocated(tmp2_BLK))        deallocate(tmp2_BLK)
    if(allocated(time_BLK))        deallocate(time_BLK)
    if(allocated(DivB1_GB))        deallocate(DivB1_GB)
    if(allocated(iTypeAdvance_BP)) deallocate(iTypeAdvance_BP)
    if(allocated(iTypeAdvance_B))  deallocate(iTypeAdvance_B)
    if(allocated(ExNum_CB))        deallocate(ExNum_CB)
    if(allocated(EyNum_CB))        deallocate(EyNum_CB)
    if(allocated(EzNum_CB))        deallocate(EzNum_CB)
    if(allocated(Efield_DGB))      deallocate(Efield_DGB)
    if(allocated(Source_VCB))      deallocate(Source_VCB)
    if(allocated(FluxCenter_VGD))  deallocate(FluxCenter_VGD)
    if(allocated(IsLowOrderOnly_B))deallocate(IsLowOrderOnly_B)
    if(allocated(LowOrderCrit_XB)) deallocate(LowOrderCrit_XB)
    if(allocated(LowOrderCrit_YB)) deallocate(LowOrderCrit_YB)
    if(allocated(LowOrderCrit_ZB)) deallocate(LowOrderCrit_ZB)
    if(allocated(Vel_IDGB))        deallocate(Vel_IDGB)
    !$omp parallel
    if(allocated(LeftState_VX))    deallocate(LeftState_VX, RightState_VX)
    if(allocated(LeftState_VY))    deallocate(LeftState_VY, RightState_VY)
    if(allocated(LeftState_VZ))    deallocate(LeftState_VZ, RightState_VZ)
    if(allocated(Flux_VXI))         deallocate(Flux_VXI)
    if(allocated(Flux_VYI))         deallocate(Flux_VYI)
    if(allocated(Flux_VZI))         deallocate(Flux_VZI)
    if(allocated(bCrossArea_DX))   deallocate(bCrossArea_DX)
    if(allocated(bCrossArea_DY))   deallocate(bCrossArea_DY)
    if(allocated(bCrossArea_DZ))   deallocate(bCrossArea_DZ)
    if(allocated(FaceDivU_IX))     deallocate(FaceDivU_IX)
    if(allocated(FaceDivU_IY))     deallocate(FaceDivU_IY)
    if(allocated(FaceDivU_IZ))     deallocate(FaceDivU_IZ)
    if(allocated(MhdFlux_VXI))      deallocate(MhdFlux_VXI)
    if(allocated(MhdFlux_VYI))      deallocate(MhdFlux_VYI)
    if(allocated(MhdFlux_VZI))      deallocate(MhdFlux_VZI)
    if(allocated(Primitive_VGI))    deallocate(Primitive_VGI)
    if(allocated(Source_VCI))       deallocate(Source_VCI)
    if(allocated(SourceMhd_VCI))    deallocate(SourceMhd_VCI)
    !$omp end parallel

    if(iProc==0)then
       call write_prefix
       write(iUnitOut,'(a)') 'clean_mod_advance deallocated arrays'
    end if

    call test_stop(NameSub, DoTest)
  end subroutine clean_mod_advance
  !============================================================================
end module ModAdvance
!==============================================================================
