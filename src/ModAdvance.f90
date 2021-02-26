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

  public :: init_mod_advance
  public :: clean_mod_advance
  public :: init_face_flux_var_type
  
  type, public :: FaceFluxVarType
     ! index of cell in the negative and positive directions from face
     integer :: iLeft,  jLeft, kLeft
     integer :: iRight, jRight, kRight
     ! Index of the block for this face, iBlockFace = iBlock
     integer :: iBlockFace
     ! Direction of the face iDimFace = iDim
     integer :: iDimFace
     ! Range of fluid and variable indexes for the current solver
     integer:: iFluidMin, iFluidMax
     integer:: iVarMin, iVarMax
     integer:: iEnergyMin, iEnergyMax
     ! index of the face
     integer :: iFace, jFace, kFace
     ! Maximum speed for the Courant condition
     logical :: UseHallGradPe
     logical :: IsBoundary
     logical :: DoTestCell
     ! Logicals for computation once per block
     logical :: IsNewBlockVisco
     logical :: IsNewBlockGradPe
     logical :: IsNewBlockCurrent
     logical :: IsNewBlockHeatCond
     logical :: IsNewBlockIonHeatCond
     logical :: IsNewBlockRadDiffusion
     logical :: IsNewBlockAlfven
  end type FaceFluxVarType
  
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

  ! Conservative/Non-conservative parameters
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

  ! Block cell-centered MHD solution
  real, allocatable, target :: State_VGB(:,:,:,:,:)
  real, allocatable :: Energy_GBI(:,:,:,:,:)
  !$acc declare create(State_VGB, Energy_GBI)

  ! Block cell-centered MHD solution old state
  real, allocatable :: StateOld_VGB(:,:,:,:,:)
  real, allocatable :: EnergyOld_CBI(:,:,:,:,:)
  !$acc declare create(StateOld_VGB, EnergyOld_CBI)

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
  real :: Source_VC(nVar+nFluid, nI, nJ, nK)
  real :: SourceMhd_VC(RhoUx_:RhoUz_, nI, nJ, nK)
  !$omp threadprivate( Source_VC, SourceMhd_VC )
  !$acc declare create( Source_VC, SourceMhd_VC )

  real, allocatable :: Source_VCB(:,:,:,:,:)

  ! Extra source terms coming from other models in the SWMF
  ! It should be allocated in the coupler
  real, allocatable :: ExtraSource_ICB(:,:,:,:,:)

  real, allocatable :: DivB1_GB(:,:,:,:)
  !$acc declare create(DivB1_GB)

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
  real, allocatable:: LeftState_VXI(:,:,:,:,:), RightState_VXI(:,:,:,:,:)
  real, allocatable:: LeftState_VYI(:,:,:,:,:), RightState_VYI(:,:,:,:,:)
  real, allocatable:: LeftState_VZI(:,:,:,:,:), RightState_VZI(:,:,:,:,:)
  !$omp threadprivate( LeftState_VXI, RightState_VXI)
  !$omp threadprivate( LeftState_VYI, RightState_VYI)
  !$omp threadprivate( LeftState_VZI, RightState_VZI)
  !$acc declare create(LeftState_VXI, RightState_VXI)
  !$acc declare create(LeftState_VYI, RightState_VYI)
  !$acc declare create(LeftState_VZI, RightState_VZI)

    ! primitive variables
  real, allocatable:: Primitive_VGI(:,:,:,:,:)
  !$omp threadprivate( Primitive_VGI )
  !$acc declare create(Primitive_VGI )

  ! Face centered div(U)*dl
  real, allocatable:: FaceDivU_IXI(:,:,:,:,:)
  real, allocatable:: FaceDivU_IYI(:,:,:,:,:)
  real, allocatable:: FaceDivU_IZI(:,:,:,:,:)
  !$omp threadprivate( FaceDivU_IXI, FaceDivU_IYI, FaceDivU_IZI )

  ! V/dt for CFL time step limit
  real, allocatable:: VdtFace_XI(:,:,:,:), VdtFace_YI(:,:,:,:), VdtFace_ZI(:,:,:,:)
  !$omp threadprivate( VdtFace_XI, VdtFace_YI, VdtFace_ZI )
  !$acc declare create(VdtFace_XI, VdtFace_YI, VdtFace_ZI)
  
  ! Fluxes are for conservative variables (momentum)
  real, allocatable:: Flux_VXI(:,:,:,:,:) , Flux_VYI(:,:,:,:,:) , Flux_VZI(:,:,:,:,:) 
  !$omp threadprivate( Flux_VXI, Flux_VYI, Flux_VZI )
  !$acc declare create( Flux_VXI, Flux_VYI, Flux_VZI )
  
  ! Cell centered fluxes
  logical:: DoInterpolateFlux = .false.
  real, allocatable:: FluxLeft_VGD(:,:,:,:,:), FluxRight_VGD(:,:,:,:,:)
  !$omp threadprivate( FluxLeft_VGD, FluxRight_VGD )

  ! Variables for ECHO scheme
  logical:: UseFDFaceFlux = .false., DoCorrectFace = .false.
  real, allocatable:: FluxCenter_VGD(:,:,:,:,:)
  !$omp threadprivate( FluxCenter_VGD )

  ! Velocity . area vector for div(U) in various source terms. Per fluid.
  real, allocatable:: &
       uDotArea_XII(:,:,:,:,:), uDotArea_YII(:,:,:,:,:), uDotArea_ZII(:,:,:,:,:)
  !$omp threadprivate( uDotArea_XII, uDotArea_YII, uDotArea_ZII )
  !$acc declare create( uDotArea_XII, uDotArea_YII, uDotArea_ZII )  

  ! Magnetic field cross area vector for J x B source term in multi-ion MHD
  real, allocatable:: &
       bCrossArea_DXI(:,:,:,:,:), bCrossArea_DYI(:,:,:,:,:), bCrossArea_DZI(:,:,:,:,:)
  !$omp threadprivate( bCrossArea_DXI, bCrossArea_DYI, bCrossArea_DZI )

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

    !$omp parallel
    if(UseB .and. (UseMultiIon .or. .not.IsMhd) &
         .and. .not. allocated(bCrossArea_DXI))then
       allocate(bCrossArea_DXI(MaxDim,nI+1,jMinFace:jMaxFace,kMinFace:kMaxFace,nGang))
       allocate(bCrossArea_DYI(MaxDim,iMinFace:iMaxFace,nJ+1,kMinFace:kMaxFace,nGang))
       allocate(bCrossArea_DZI(MaxDim,iMinFace:iMaxFace,jMinFace:jMaxFace,nK+1,nGang))
       bCrossArea_DXI = 0.0; bCrossArea_DYI = 0.0; bCrossArea_DZI = 0.0
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
    allocate(LeftState_VXI(nVar,nI+1,jMinFace2:jMaxFace2,kMinFace2:kMaxFace2,nGang))
    allocate(RightState_VXI(nVar,nI+1,jMinFace2:jMaxFace2,kMinFace2:kMaxFace2,nGang))
    allocate(VdtFace_XI(nI+1,jMinFace:jMaxFace,kMinFace:kMaxFace,nGang))
    allocate(Flux_VXI(nVar+nFluid,nI+1,jMinFace:jMaxFace,kMinFace:kMaxFace,nGang) )
    allocate(MhdFlux_VXI(RhoUx_:RhoUz_,nI+1,jMinFace:jMaxFace,kMinFace:kMaxFace,nGang) )
    allocate(uDotArea_XII(nI+1,jMinFace:jMaxFace,kMinFace:kMaxFace,nFluid+1,nGang))
    MhdFlux_VXI = 0.0

    allocate(LeftState_VYI(nVar,iMinFace2:iMaxFace2,nJ+1,kMinFace2:kMaxFace2,nGang))
    allocate(RightState_VYI(nVar,iMinFace2:iMaxFace2,nJ+1,kMinFace2:kMaxFace2,nGang))
    allocate(VdtFace_YI(iMinFace:iMaxFace,nJ+1,kMinFace:kMaxFace,nGang))
    allocate(Flux_VYI(nVar+nFluid,iMinFace:iMaxFace,nJ+1,kMinFace:kMaxFace,nGang) )
    allocate(MhdFlux_VYI(RhoUx_:RhoUz_,iMinFace:iMaxFace,nJ+1,kMinFace:kMaxFace,nGang) )
    allocate(uDotArea_YII(iMinFace:iMaxFace,nJ+1,kMinFace:kMaxFace,nFluid+1,nGang))
    MhdFlux_VYI = 0.0

    allocate(LeftState_VZI(nVar,iMinFace2:iMaxFace2,jMinFace2:jMaxFace2,nK+1,nGang))
    allocate(RightState_VZI(nVar,iMinFace2:iMaxFace2,jMinFace2:jMaxFace2,nK+1,nGang))
    allocate(VdtFace_ZI(iMinFace:iMaxFace,jMinFace:jMaxFace,nK+1,nGang))
    allocate(Flux_VZI(nVar+nFluid,iMinFace:iMaxFace,jMinFace:jMaxFace,nK+1,nGang) )
    allocate(MhdFlux_VZI(RhoUx_:RhoUz_,iMinFace:iMaxFace,jMinFace:jMaxFace,nK+1,nGang) )
    allocate(uDotArea_ZII(iMinFace:iMaxFace,jMinFace:jMaxFace,nK+1,nFluid+1,nGang))
    MhdFlux_VZI = 0.0

    allocate(FaceDivU_IXI(nFluid,1:nIFace,jMinFace:jMaxFace,kMinFace:kMaxFace,nGang))
    allocate(FaceDivU_IYI(nFluid,iMinFace:iMaxFace,1:nJFace,kMinFace:kMaxFace,nGang))
    allocate(FaceDivU_IZI(nFluid,iMinFace:iMaxFace,jMinFace:jMaxFace,1:nKFace,nGang))

    allocate(Primitive_VGI(nVar,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,nGang))
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
    if(allocated(LeftState_VXI))    deallocate(LeftState_VXI, RightState_VXI)
    if(allocated(LeftState_VYI))    deallocate(LeftState_VYI, RightState_VYI)
    if(allocated(LeftState_VZI))    deallocate(LeftState_VZI, RightState_VZI)
    if(allocated(VdtFace_XI))       deallocate(VdtFace_XI)
    if(allocated(VdtFace_YI))       deallocate(VdtFace_YI)
    if(allocated(VdtFace_ZI))       deallocate(VdtFace_ZI)
    if(allocated(Flux_VXI))         deallocate(Flux_VXI)
    if(allocated(Flux_VYI))         deallocate(Flux_VYI)
    if(allocated(Flux_VZI))         deallocate(Flux_VZI)
    if(allocated(uDotArea_XII))     deallocate(uDotArea_XII)
    if(allocated(uDotArea_YII))     deallocate(uDotArea_YII)
    if(allocated(uDotArea_ZII))     deallocate(uDotArea_ZII)
    if(allocated(bCrossArea_DXI))   deallocate(bCrossArea_DXI)
    if(allocated(bCrossArea_DYI))   deallocate(bCrossArea_DYI)
    if(allocated(bCrossArea_DZI))   deallocate(bCrossArea_DZI)
    if(allocated(FaceDivU_IXI))     deallocate(FaceDivU_IXI)
    if(allocated(FaceDivU_IYI))     deallocate(FaceDivU_IYI)
    if(allocated(FaceDivU_IZI))     deallocate(FaceDivU_IZI)
    if(allocated(MhdFlux_VXI))      deallocate(MhdFlux_VXI)
    if(allocated(MhdFlux_VYI))      deallocate(MhdFlux_VYI)
    if(allocated(MhdFlux_VZI))      deallocate(MhdFlux_VZI)
    !$omp end parallel

    if(iProc==0)then
       call write_prefix
       write(iUnitOut,'(a)') 'clean_mod_advance deallocated arrays'
    end if

    call test_stop(NameSub, DoTest)
  end subroutine clean_mod_advance
  !============================================================================

  subroutine init_face_flux_var_type(FFV, RealArg_I)
    !$acc routine seq
    type(FaceFluxVarType), intent(inout) :: FFV
    real, dimension(:), target, intent(inout):: RealArg_I
    real, dimension(:), pointer:: Unormal_I
    real, dimension(:), pointer:: bCrossArea_D
    !--------------------------------------------------------------------------

    ! When openacc creates a derived type on GPU, the variables are
    ! not correctly initialized. So, they are explicitly initialized
    ! here. 

    bCrossArea_D => RealArg_I(bCrossArea_:bCrossArea_+MaxDim-1)
    Unormal_I => RealArg_I(Unormal_:Unormal_+nFluid+1-1)
    
    FFV%iFluidMin = 1
    FFV%iFluidMax = nFluid
    FFV%iVarMin = 1
    FFV%iVarMax = nVar
    FFV%iEnergyMin = nVar + 1
    FFV%iEnergyMax = nVar + nFluid

    Unormal_I = 0.0
    RealArg_I(EradFlux_) = 0.0
    bCrossArea_D = 0.0
    RealArg_I(B0x_) = 0.0
    RealArg_I(B0y_) = 0.0
    RealArg_I(B0z_) = 0.0 
    
    FFV%UseHallGradPe = .false.

    FFV%DoTestCell = .false.

    FFV%IsNewBlockVisco = .true.
    FFV%IsNewBlockGradPe = .true.
    FFV%IsNewBlockCurrent = .true.
    FFV%IsNewBlockHeatCond = .true.
    FFV%IsNewBlockIonHeatCond = .true.
    FFV%IsNewBlockRadDiffusion = .true.
    FFV%IsNewBlockAlfven = .true.
        
  end subroutine init_face_flux_var_type
  !============================================================================
end module ModAdvance
!==============================================================================
