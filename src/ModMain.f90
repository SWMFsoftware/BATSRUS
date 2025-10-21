!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModMain

  use ModKind
  use ModSize
  use ModVarIndexes
  use ModNumConst, ONLY: cPi, cTwoPi

  ! Total number of used blocks on all processors
  use BATL_lib, ONLY: nBlock, nBlockAll => nNodeUsed, Unused_B, lVerbose

  implicit none

  SAVE

  ! Version of User module
  character(len=200):: NameUserFile = '???', NameUserModule = '???'

  ! Standalone and component information
  ! In stand alone mode this variable is set to true
  logical             :: IsStandAlone = .false.

  ! In the SWMF the BATSRUS may run as GM, EE, SC, IH, or OH component
  character (len=2)   :: NameThisComp='GM'
  !$acc declare create(NameThisComp)

  ! In hydro equations B_ = U_ is set.
  logical, parameter:: UseB = B_ /= U_

  ! Time stepping parameters and values.
  integer :: nStep=0, nOrder, iStage, nStage, nIteration=0, nOrderOld
  !$acc declare create(nStep, nOrder, iStage, nStage, nIteration)
  logical :: UseHalfStep = .true. ! true for the Dt/2, Dt update scheme
  !$acc declare create(UseHalfStep)

  ! FLux-In-Cell scheme, if true. (Dt/2, Dt/2, Dt) update with a special
  ! procedure to get time-centered electromagnetic fields at half time-step
  ! Maintains the Mhd environment (electromagnetic field) for hybrid.
  logical :: UseFlic     = .false.
  !$acc declare create(UseFlic)

  ! Fixed time step (only for time accurate and for implicit scheme mostly)
  logical :: UseDtFixed
  !$acc declare create(UseDtFixed)

  ! Time step and CFL number
  real :: Dt
  real :: DtFixed
  real :: DtFixedOrig
  real :: DtFixedDim   ! in IO units
  real :: Cfl
  real :: CflOrig

  ! Maximum time step for the block
  real, allocatable :: DtMax_B(:)
  !$acc declare create(DtMax_B, Dt, DtFixed, Cfl)

  ! Time accurate vs steady state, inside the time loop (or not yet)
  logical :: IsTimeAccurate = .true.,  IsTimeLoop = .false.
  !$acc declare create(IsTimeAccurate, IsTimeLoop)

  ! Limiting speed in the numerical diffusive flux (for implicit scheme only)
  real :: Climit = -1.0
  !$acc declare create(Climit)

  ! Limited time step
  logical :: UseDtLimit
  real    :: DtLimit, DtLimitOrig, DtLimitDim
  !$acc declare create(UseDtLimit, DtLimit)

  ! Part local time step
  real    :: rLocalTimeStep = -1.0
  !$acc declare create(rLocalTimeStep)

  ! Local time stepping (subcycling)
  logical:: UseLocalTimeStep    = .false.
  logical:: UseLocalTimeStepNew = .false. ! if just switched on

  ! Model coupling variables
  logical :: UseBufferGrid = .false.
  !$acc declare create(UseBufferGrid)

  ! Logical for if outer helio sphere model is used
  logical :: UseOuterHelio = .false.

  ! To split the LOS intergration span between the models
  real    :: rLowerModel = 0.0, rUpperModel = 1000.0

  ! Dimensions of the buffer grid between SC and IH

  logical :: UseIe = .false.
  !$acc declare create(UseIe)
  logical :: UsePw = .false.

  logical :: UseIm = .false.
  logical :: DoCoupleImPressure = .true.
  logical :: DoCoupleImDensity  = .false.
  logical :: DoFixPolarRegion   = .false.
  logical :: DoImSatTrace       = .false.
  logical :: DoRbSatTrace       = .false.
  real    :: rFixPolarRegion    = 5.0
  real    :: dLatSmoothIm       = -1.0
  real    :: TauCoupleIm        = 20.0
  real    :: RhoMinDimIm        = -1.0
  !$acc declare create(DoCoupleImPressure, DoCoupleImDensity)
  !$acc declare create(DoFixPolarRegion, rFixPolarRegion, dLatSmoothIm)
  !$acc declare create(TauCoupleIm)

  logical :: UseRaytrace            = UseB
  logical :: DoMultiFluidIMCoupling = &
       nIonFluid > 1 .or. SpeciesLast_ > SpeciesFirst_
  logical :: DoAnisoPressureIMCoupling = .false.
  !$acc declare create(DoAnisoPressureIMCoupling, DoMultiFluidIMCoupling)

  ! Single space separated NameVar string containing all the variable
  ! names of NameVar_V
  character(len=1000) :: NameVarCouple

  ! Intrinsic field B0 may or may not be used if UseB is true.
  logical :: UseB0        = UseB
  !$acc declare create(UseB0)

  ! Inner and outer boundaries

  ! Indexes for boundaries
  integer, parameter :: Body1_   = -1
  integer, parameter :: Body2_   = -2
  integer, parameter :: ExtraBc_ =  0
  integer, parameter :: SolidBc_ = -3
  integer, parameter :: &
       Coord1MinBc_ = 1, Coord1MaxBc_ = 2, &
       Coord2MinBc_ = 3, Coord2MaxBc_ = 4, &
       Coord3MinBc_ = 5, Coord3MaxBc_ = 6, &
       xMinBc_ = 1, xMaxBc_ = 2, &
       yMinBc_ = 3, yMaxBc_ = 4, &
       zMinBc_ = 5, zMaxBc_ = 6

  ! Inner and outer boundary conditions
  character(len=20) :: TypeCellBc_I(Coord1MinBc_:Coord3MaxBc_)='none'
  integer :: iTypeCellBc_I(Coord1MinBc_:Coord3MaxBc_)
  !$acc declare create(TypeCellBc_I, iTypeCellBc_I)
  character(len=20) :: TypeFaceBc_I(SolidBc_:zMaxBc_)='none'

  type :: FaceBCType
     ! True if only boundaries at resolution changes are updated
     logical :: DoResChangeOnly
     ! Type of the boundary
     character(len=20) :: TypeBc
     ! Index of the boundary
     ! Negative iBoundary indicates which body we are computing for.
     ! Zero corresponds to the user defined extra boundary.
     ! iBoundary=1:6  for cell boundaries set by #OUTERBOUNDARY
     ! iBoundary=7:12 for face boundaries set by #BOXBOUNDARY
     integer :: iBoundary
     ! Index of the face
     integer :: iFace, jFace, kFace
     ! The side of the cell defined with respect to the cell inside the domain
     integer :: iSide
     ! Boundary block index
     integer :: iBlockBc
     ! Simulation time
     real :: TimeBc
     ! The coordinates of the face center and the B0 field at that point
     real :: FaceCoords_D(3), B0Face_D(3)
     ! The values on the physical side and the ghost cell side of the boundary
     real :: VarsTrueFace_V(nVar), VarsGhostFace_V(nVar)
  end type FaceBCType

  ! Logicals for bodies
  logical:: UseBody  = .false.
  !$acc declare create(UseBody)
  logical:: UseBody2 = .false.

  ! Block AMR grid parameters

  ! Identifiers for the grid and decomposition changes
  integer :: iNewGrid=0, iNewDecomposition=0

  ! Number of geometric based refinements performed
  ! (needed by the CCMC user module only! )
  integer :: nRefineLevel = 0

  ! nBlockMax is a maximum block index over all processors
  integer :: nBlockMax

  ! Number of explicitly and implicitly advanced blocks
  integer :: nBlockExplAll, nBlockImplAll

  ! Index limits for the cell faces (needed for the constrained transport)
  integer, parameter :: nIFace = nI+1
  integer, parameter :: nJFace = nJ+1
  integer, parameter :: nKFace = nK+1

  ! Limits in the orthogonal directions. Default is no ghost cells,
  ! but this can be changed to 1 or 2 ghost cells depending on scheme
  integer:: &
       iMinFace = 1, iMaxFace = nI, iMinFace2 = 1, iMaxFace2 = nI, &
       jMinFace = 1, jMaxFace = nJ, jMinFace2 = 1, jMaxFace2 = nJ, &
       kMinFace = 1, kMaxFace = nK, kMinFace2 = 1, kMaxFace2 = nK
  !$acc declare create(iMinFace, iMaxFace, iMinFace2, iMaxFace2)
  !$acc declare create(jMinFace, jMaxFace, jMinFace2, jMaxFace2)
  !$acc declare create(kMinFace, kMaxFace, kMinFace2, kMaxFace2)

  ! div B control
  logical :: UseDivbSource     = UseB
  logical :: UseDivbDiffusion  = .false.
  logical :: UseProjection     = .false.
  logical :: UseConstrainB     = .false.
  logical :: UseHyperbolicDivb = .false.
  real    :: SpeedHypDim = -1.0, SpeedHyp = 1.0, HypDecay = 0.1
  !$acc declare create(UseDivbSource, UseConstrainB)
  !$acc declare create(UseHyperbolicDivb, SpeedHyp, HypDecay)

  ! More numerical scheme parameters
  ! Prolongation order
  integer           :: nOrderProlong = 1
  !$acc declare create(nOrderProlong)

  ! Message passing mode ('all' or 'allopt' ...)
  character(len=10) :: TypeMessagePass = 'all'

  ! Source terms

  ! Logicals for adding radiation diffusion and heat conduction
  logical :: UseRadDiffusion = .false.
  logical :: UseHeatConduction = .false.
  logical :: UseIonHeatConduction = .false.
  !$acc declare create(UseHeatConduction)

  ! Logical and type for gravity
  logical :: UseGravity  = .false.
  integer :: iDirGravity = 0
  real    :: GravitySi   = 0.0
  !$acc declare create(UseGravity)

  ! Logical for rotating inner boundary
  logical :: UseRotatingBc = .false.
  !$acc declare create(UseRotatingBC)

  ! Coordinate system
  character(len=3) :: TypeCoordSystem = 'GSM'
  integer :: iTypeCoordSystem
  integer,  parameter:: &
       GSM_ = 1, GSE_ = 2, HGR_ = 3, HGI_ = 4, HGC_ = 5, nCoordSystem = 5
  character(len=3), public, parameter :: NameCoordSystem_I(1:nCoordSystem) = &
       ['GSM', 'GSE', 'HGR', 'HGI', 'HGC']
  !$acc declare create(iTypeCoordSystem)

  ! Rotating frame or (at least approximately) inertial frame
  logical :: UseRotatingFrame = .false.
  !$acc declare create(UseRotatingFrame)

  ! Transform initial condition between rotating and inertial frames
  integer:: iSignRotationIC = 0

  ! Variables for debugging.

  integer :: iPixTest =-1, jPixTest = -1              ! pixel to test

  ! Shall we be strict about errors in the input parameter file
  logical :: UseStrict=.true.

  ! Debug logicals
  logical :: DoDebug=.false., DoShowGhostCells=.true.

  ! Time and timing variables
  real :: tSimulation = 0.0
  real :: tSimulationOld = 0.0
  !$acc declare create(tSimulation)

  ! This is the same default value as in the SWMF
  integer, dimension(7) :: iStartTime_I = [2000,3,21,10,45,0,0]
  real(Real8_)          :: StartTime
  !$acc declare create(StartTime)

  ! Time to end
  logical :: UseEndTime = .false.

  integer,dimension(7)  :: iEndTime_I   = [2000,3,21,10,45,0,0]
  real(Real8_)          :: EndTime

  ! Timing variables
  logical:: UseTiming = .true.
  logical:: UseTimingAll = .false.
  integer:: iUnitTiming = 6
  character(len=30):: NameTimingFile
  integer:: DnTiming = -2

  ! Optimize MPI variables
  logical :: UseOptimizeMpi = .false.

  ! Stopping conditions. These variables are only used in stand alone mode.
  ! The only exeption is tSimulationMax. It may also be used in the SWMF mode
  ! to control evolving B0 field with the use of two magnetograms,
  ! one at tSimulation=0, the other at tSimulation=tSimulationMax.
  real    :: tSimulationMax = -1.0, CpuTimeMax = -1.0
  integer :: nIter = -1
  logical :: DoCheckStopFile = .true.
  logical :: IsLastRead = .false.

  ! Controling the use of the features implemented in user files
  logical:: UseUserSourceExpl        = .false.
  logical:: UseUserSourceImpl        = .false.
  logical:: UseUserPerturbation      = .false.
  logical:: UseUserICs               = .false.
  logical:: UseUserB0                = .false.
  logical:: UseUserInitSession       = .true.
  logical:: UseUserUpdateStates      = .false.
  logical:: UseUserTimeStep          = .false.
  logical:: UseUserWriteProgress     = .false.
  !$acc declare create(UseUserUpdateStates,UseUserTimeStep)

  logical:: UseExtraBoundary         = .false.
  logical:: UseSolidState            = .false.

  ! Logical controlling the use of the laser heating.
  logical :: UseLaserHeating = .false.

  ! Logical, controlling NLTE computations and determining if
  ! these computations use ERad values OR their ratios to the the equalibrium
  ! values (ERad over B) for all energy groups.
  logical:: UseERadInput=.false.

  ! Logical for a thin heliospheric current sheet method similar to that
  ! in the ENLIL code of D. Odstril
  logical :: DoThinCurrentSheet = .false.

  ! Logicals for the use of the boundary condition at the surface
  ! well above the transition region, which in connected by the
  ! magnetic field line threads with the photosphere boundary.
  logical:: UseFieldLineThreads = .false., DoThreadRestart = .false.
  !$acc declare create(UseFieldLineThreads)
  logical, public, allocatable :: DoThreads_B(:)

  ! Use high-order accurate ghost cells.
  logical :: UseHighResChange = .false.
  !$acc declare create(UseHighResChange)

  ! Use high-order accurate refined/coarsened cells.
  logical :: UseHighOrderAMR = .false.

  ! Use resistivity planetary interior. It may be set to true in the
  ! ModUserMercury.f90.
  logical :: UseResistivePlanet = .false.
  !$acc declare create(UseResistivePlanet)

  ! Variables related to another component coupled directly with pointers
  integer           :: nVarComp2
  character(len=200):: NameVarComp2
  real, pointer     :: StateComp2_VGB(:,:,:,:,:)

  ! The lower case names of the variables and the primitive var names
  character(len=len(NameVar_V)) :: NameVarLower_V(nVar+nFluid)
  character(len=len(NameVar_V)) :: NamePrimitive_V(nVar)

  ! Neutral state variables received from Upper Atmosphere model
  logical:: IsNewUaState = .false.
  real, allocatable :: UaState_VCB(:,:,:,:,:)
  real:: rMaxUa = 3.0   ! max radius of UA influence

  ! Logical is true if compiled with OpenACC
#ifdef _OPENACC
  logical, parameter:: UseOpenACC = .true.
#else
  logical, parameter:: UseOpenACC = .false.
#endif

contains
  !============================================================================
  subroutine init_mod_main
    !--------------------------------------------------------------------------
    if(.not.allocated(DtMax_B))then
       allocate(DtMax_B(MaxBlock))
       DtMax_B = 0.0
       !$acc update device(DtMax_B)
    end if
  end subroutine init_mod_main
  !============================================================================
  subroutine clean_mod_main
    !--------------------------------------------------------------------------
    if(allocated(DtMax_B)) deallocate(DtMax_B)

  end subroutine clean_mod_main
  !============================================================================
end module ModMain
!==============================================================================
