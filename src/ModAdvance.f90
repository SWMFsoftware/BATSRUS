!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModAdvance

  use BATL_lib, ONLY: &
       test_start, test_stop

  use ModSize
  use ModVarIndexes
  use ModSemiImplVar, ONLY: UseStableImplicit
  use ModMultiFluid, ONLY: UseMultiIon
  use ModMain,       ONLY: UseB, UseRotatingFrame, UseGravity, &
       iMinFace, iMaxFace, jMinFace, jMaxFace, kMinFace, kMaxFace, &
       iMinFace2, iMaxFace2, jMinFace2, jMaxFace2, kMinFace2, kMaxFace2, &
       nIFace, nJFace, nKFace
  use ModIO,         ONLY: iUnitOut, write_prefix
  use ModProcMH,     ONLY: iProc, nProc
  use omp_lib
  
  implicit none
  save

  public :: init_mod_advance
  public :: clean_mod_advance

  ! Numerical flux type
  character (len=10) :: FluxType

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

  logical:: DoCalcElectricField = .false.

  ! Update check parameters
  logical :: UseUpdateCheck
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

  !\
  ! Conservative/Non-conservative parameters
  !/
  logical :: UseNonConservative

  ! Number and type of criteria
  integer :: nConservCrit
  character (len=10), allocatable :: TypeConservCrit_I(:)

  ! Geometrical parameters
  real    :: rConserv, xParabolaConserv, yParabolaConserv

  ! Physics based parameters (to locate shocks)
  real    :: pCoeffConserv, GradPCoeffConserv

  ! Cells selected to be updated with conservative equations
  logical, allocatable :: IsConserv_CB(:,:,:,:)

  !\
  ! Block cell-centered MHD solution
  !/
  real, allocatable, target :: State_VGB(:,:,:,:,:)
  real, allocatable :: Energy_GBI(:,:,:,:,:)

  !\
  ! Block cell-centered MHD solution old state
  !/
  real, allocatable :: StateOld_VGB(:,:,:,:,:)
  real, allocatable :: EnergyOld_CBI(:,:,:,:,:)

  !\
  ! Block cell-centered intrinsic magnetic field, time, and temporary storage
  !/
  real, allocatable :: tmp1_BLK(:,:,:,:)
  real, allocatable :: tmp2_BLK(:,:,:,:)

  real, allocatable :: time_BLK(:,:,:,:)

  ! Electric field including numerical flux
  real, allocatable :: ExNum_CB(:,:,:,:)
  real, allocatable :: EyNum_CB(:,:,:,:)
  real, allocatable :: EzNum_CB(:,:,:,:)

  real, public, allocatable:: Efield_DGB(:,:,:,:,:)

  ! Local cell-centered source terms and divB.
  real :: Source_VC(nVar+nFluid, nI, nJ, nK)
  real :: SourceMhd_VC(RhoUx_:RhoUz_, nI, nJ, nK)
  !$omp threadprivate( Source_VC, SourceMhd_VC )
  
  real, allocatable :: Source_VCB(:,:,:,:,:)

  ! Extra source terms coming from other models in the SWMF
  ! It should be allocated in the coupler
  real, allocatable :: ExtraSource_ICB(:,:,:,:,:)

  real, allocatable :: DivB1_GB(:,:,:,:)

  ! Switch between low and high order schemes 
  logical:: UseLowOrder = .false.  ! some faces are low order
  logical, allocatable:: IsLowOrderOnly_B(:) ! Is the whole block low order?
  logical:: UseLowOrderRegion = .false.
  logical:: UseAdaptiveLowOrder = .false. 
  real, allocatable:: LowOrderCrit_XB(:,:,:,:),LowOrderCrit_YB(:,:,:,:), &
       LowOrderCrit_ZB(:,:,:,:) ! The ratio of the low order face values
  
  ! Cell centered velocities in ijk direction.
  real, allocatable:: Vel_IDGB(:,:,:,:,:,:) 

  ! Face centered variables for the current block

  ! Primitive variables (velocity) extrapolated from left and right
  real, allocatable:: LeftState_VX(:,:,:,:), RightState_VX(:,:,:,:)
  real, allocatable:: LeftState_VY(:,:,:,:), RightState_VY(:,:,:,:)
  real, allocatable:: LeftState_VZ(:,:,:,:), RightState_VZ(:,:,:,:)
  !$omp threadprivate( LeftState_VX, RightState_VX )
  !$omp threadprivate( LeftState_VY, RightState_VY )
  !$omp threadprivate( LeftState_VZ, RightState_VZ )
  
  ! Face centered div(U)*dl
  real, allocatable:: FaceDivU_IX(:,:,:,:)
  real, allocatable:: FaceDivU_IY(:,:,:,:)
  real, allocatable:: FaceDivU_IZ(:,:,:,:)
  !$omp threadprivate( FaceDivU_IX, FaceDivU_IY, FaceDivU_IZ )
  
  ! V/dt for CFL time step limit
  real, allocatable:: VdtFace_X(:,:,:), VdtFace_Y(:,:,:), VdtFace_Z(:,:,:)
  !$omp threadprivate( VdtFace_X, VdtFace_Y, VdtFace_Z )
  
  ! Fluxes are for conservative variables (momentum)
  real, allocatable:: Flux_VX(:,:,:,:), Flux_VY(:,:,:,:), Flux_VZ(:,:,:,:)
  !$omp threadprivate( Flux_VX, Flux_VY, Flux_VZ )
  
  ! Cell centered fluxes
  logical:: DoInterpolateFlux = .false.
  real, allocatable:: FluxLeft_VGD(:,:,:,:,:), FluxRight_VGD(:,:,:,:,:)
  !$omp threadprivate( FluxLeft_VGD, FluxRight_VGD )
  
  ! Variables for ECHO scheme
  logical:: UseFDFaceFlux = .false., DoCorrectFace = .false.
  real, allocatable:: FluxCenter_VGD(:,:,:,:,:)
  !$omp threadprivate( FluxCenter_VGD )
  
  ! CWENO weight used to limit flux.
  real, allocatable:: Weight_IVX(:,:,:,:,:), Weight_IVY(:,:,:,:,:), &
       Weight_IVZ(:,:,:,:,:)
  !$omp threadprivate( Weight_IVX, Weight_IVY, Weight_IVZ )
  
  ! Velocity . area vector for div(U) in various source terms. Per fluid.
  real, allocatable:: &
       uDotArea_XI(:,:,:,:), uDotArea_YI(:,:,:,:), uDotArea_ZI(:,:,:,:)
  !$omp threadprivate( uDotArea_XI, uDotArea_YI, uDotArea_ZI )
  
  ! Magnetic field cross area vector for J x B source term in multi-ion MHD
  real, allocatable:: &
       bCrossArea_DX(:,:,:,:), bCrossArea_DY(:,:,:,:), bCrossArea_DZ(:,:,:,:)
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

  real, allocatable:: MhdSource_VC(:,:,:,:),  &
       MhdFlux_VX(:,:,:,:), MhdFlux_VY(:,:,:,:), MhdFlux_VZ(:,:,:,:)
  !$omp threadprivate( MhdFlux_VX, MhdFlux_VY, MhdFlux_VZ )
  
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
    if(UseMhdMomentumFlux .and. UseMultiIon &
         .and. .not.allocated(Efield_DGB))then
       allocate(Efield_DGB(3,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
       Efield_DGB = 0.0
    end if
    if(UseStableImplicit) then
       allocate(Source_VCB(nVar, nI, nJ, nK, MaxBlock))
       Source_VCB = 0
    endif

    !$omp parallel
    if(UseB .and. (UseMultiIon .or. .not.IsMhd) &
         .and. .not. allocated(bCrossArea_DX))then
       allocate(bCrossArea_DX(MaxDim,nI+1,jMinFace:jMaxFace,kMinFace:kMaxFace))
       allocate(bCrossArea_DY(MaxDim,iMinFace:iMaxFace,nJ+1,kMinFace:kMaxFace))
       allocate(bCrossArea_DZ(MaxDim,iMinFace:iMaxFace,jMinFace:jMaxFace,nK+1))
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
    allocate(Energy_GBI(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock,nFluid))
    allocate(StateOld_VGB(nVar,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
    allocate(EnergyOld_CBI(nI,nJ,nK,MaxBlock,nFluid))
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
    allocate(LeftState_VX(nVar,nI+1,jMinFace2:jMaxFace2,kMinFace2:kMaxFace2))
    allocate(RightState_VX(nVar,nI+1,jMinFace2:jMaxFace2,kMinFace2:kMaxFace2))
    allocate(VdtFace_X(nI+1,jMinFace:jMaxFace,kMinFace:kMaxFace))
    allocate(Flux_VX(nVar+nFluid,nI+1,jMinFace:jMaxFace,kMinFace:kMaxFace))
    allocate(MhdFlux_VX(RhoUx_:RhoUz_,nI+1,jMinFace:jMaxFace,kMinFace:kMaxFace))
    allocate(uDotArea_XI(nI+1,jMinFace:jMaxFace,kMinFace:kMaxFace,nFluid+1))
    MhdFlux_VX = 0.0

    allocate(LeftState_VY(nVar,iMinFace2:iMaxFace2,nJ+1,kMinFace2:kMaxFace2))
    allocate(RightState_VY(nVar,iMinFace2:iMaxFace2,nJ+1,kMinFace2:kMaxFace2))
    allocate(VdtFace_Y(iMinFace:iMaxFace,nJ+1,kMinFace:kMaxFace))
    allocate(Flux_VY(nVar+nFluid,iMinFace:iMaxFace,nJ+1,kMinFace:kMaxFace))
    allocate(MhdFlux_VY(RhoUx_:RhoUz_,iMinFace:iMaxFace,nJ+1,kMinFace:kMaxFace))
    allocate(uDotArea_YI(iMinFace:iMaxFace,nJ+1,kMinFace:kMaxFace,nFluid+1))
    MhdFlux_VY = 0.0

    allocate(LeftState_VZ(nVar,iMinFace2:iMaxFace2,jMinFace2:jMaxFace2,nK+1))
    allocate(RightState_VZ(nVar,iMinFace2:iMaxFace2,jMinFace2:jMaxFace2,nK+1))
    allocate(VdtFace_Z(iMinFace:iMaxFace,jMinFace:jMaxFace,nK+1))
    allocate(Flux_VZ(nVar+nFluid,iMinFace:iMaxFace,jMinFace:jMaxFace,nK+1))
    allocate(MhdFlux_VZ(RhoUx_:RhoUz_,iMinFace:iMaxFace,jMinFace:jMaxFace,nK+1))
    allocate(uDotArea_ZI(iMinFace:iMaxFace,jMinFace:jMaxFace,nK+1,nFluid+1))
    MhdFlux_VZ = 0.0

    allocate(FaceDivU_IX(nFluid,1:nIFace,jMinFace:jMaxFace,kMinFace:kMaxFace))
    allocate(FaceDivU_IY(nFluid,iMinFace:iMaxFace,1:nJFace,kMinFace:kMaxFace))
    allocate(FaceDivU_IZ(nFluid,iMinFace:iMaxFace,jMinFace:jMaxFace,1:nKFace))
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
    if(allocated(Energy_GBI))      deallocate(Energy_GBI)
    if(allocated(StateOld_VGB))    deallocate(StateOld_VGB)
    if(allocated(EnergyOld_CBI))   deallocate(EnergyOld_CBI)
    if(allocated(tmp1_BLK))        deallocate(tmp1_BLK)
    if(allocated(tmp2_BLK))        deallocate(tmp2_BLK)
    if(allocated(time_BLK))        deallocate(time_BLK)
    if(allocated(DivB1_GB))        deallocate(DivB1_GB)
    if(allocated(iTypeAdvance_BP)) deallocate(iTypeAdvance_BP)
    if(allocated(iTypeAdvance_B))  deallocate(iTypeAdvance_B)
    if(allocated(LeftState_VX))    deallocate(LeftState_VX, RightState_VX)
    if(allocated(LeftState_VY))    deallocate(LeftState_VY, RightState_VY)
    if(allocated(LeftState_VZ))    deallocate(LeftState_VZ, RightState_VZ)
    if(allocated(VdtFace_X))       deallocate(VdtFace_X)
    if(allocated(VdtFace_Y))       deallocate(VdtFace_Y)
    if(allocated(VdtFace_Z))       deallocate(VdtFace_Z)
    if(allocated(Flux_VX))         deallocate(Flux_VX)
    if(allocated(Flux_VY))         deallocate(Flux_VY)
    if(allocated(Flux_VZ))         deallocate(Flux_VZ)
    if(allocated(uDotArea_XI))     deallocate(uDotArea_XI)
    if(allocated(uDotArea_YI))     deallocate(uDotArea_YI)
    if(allocated(uDotArea_ZI))     deallocate(uDotArea_ZI)
    if(allocated(bCrossArea_DX))   deallocate(bCrossArea_DX)
    if(allocated(bCrossArea_DY))   deallocate(bCrossArea_DY)
    if(allocated(bCrossArea_DZ))   deallocate(bCrossArea_DZ)
    if(allocated(ExNum_CB))        deallocate(ExNum_CB)
    if(allocated(EyNum_CB))        deallocate(EyNum_CB)
    if(allocated(EzNum_CB))        deallocate(EzNum_CB)
    if(allocated(Efield_DGB))      deallocate(Efield_DGB)
    if(allocated(Source_VCB))      deallocate(Source_VCB)
    if(allocated(FluxCenter_VGD))  deallocate(FluxCenter_VGD)
    if(allocated(Weight_IVX))      deallocate(Weight_IVX)
    if(allocated(Weight_IVY))      deallocate(Weight_IVY)
    if(allocated(Weight_IVZ))      deallocate(Weight_IVZ)
    if(allocated(IsLowOrderOnly_B))deallocate(IsLowOrderOnly_B)
    if(allocated(FaceDivU_IX))     deallocate(FaceDivU_IX)
    if(allocated(FaceDivU_IY))     deallocate(FaceDivU_IY)
    if(allocated(FaceDivU_IZ))     deallocate(FaceDivU_IZ)
    if(allocated(LowOrderCrit_XB)) deallocate(LowOrderCrit_XB)
    if(allocated(LowOrderCrit_YB)) deallocate(LowOrderCrit_YB)
    if(allocated(LowOrderCrit_ZB)) deallocate(LowOrderCrit_ZB)
    if(allocated(Vel_IDGB))        deallocate(Vel_IDGB)
    
    if(iProc==0)then
       call write_prefix
       write(iUnitOut,'(a)') 'clean_mod_advance deallocated arrays'
    end if

    call test_stop(NameSub, DoTest)
  end subroutine clean_mod_advance
  !============================================================================

end module ModAdvance
