!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModFieldLineThread
!  use ModUtilities, ONLY: norm2
  use BATL_lib,      ONLY: &
       test_start, test_stop, jTest, kTest, iBlockTest, &
       iProc, nProc, iComm, nJ, nK, jDim_, kDim_, MaxBlock
  use ModAdvance,    ONLY: UseElectronPressure
  use ModMain,       ONLY: UseFieldLineThreads, DoThreads_B
  use ModB0,         ONLY: get_b0
  use ModPhysics,    ONLY: Z => AverageIonCharge
  use ModVarIndexes, ONLY: Pe_, p_
  use ModMultiFluid, ONLY: MassIon_I
  use ModTransitionRegion

  implicit none
  SAVE

  PRIVATE ! Except
  integer, public, parameter:: jMin_ = 1 - jDim_, jMax_ = nJ + jDim_
  integer, public, parameter:: kMin_ = 1 - kDim_, kMax_ = nK + kDim_
  
  public :: init                      ! Initializes module
  public :: save_threads_for_plot     ! Get  State_VG array
  public :: interpolate_thread_state  ! Interpolate state from State_VG
  public :: set_thread_plotvar        ! Plot variables for "shell" plots
  public :: get_tr_los_image          ! Correction for TR on LOS images
  !\
  ! rBody here is set to one keeping a capability to set
  ! the face-formulated boundary condition by modifying
  ! rBody
  !/
  real, public, parameter :: rBody = 1.0

  logical, public, allocatable:: IsAllocatedThread_B(:)
  ! Named indexes for local use only

  ! In the boundary blocks the physical cells near the boundary are connected
  ! to the photosphere with these threads
  type BoundaryThreads
     !\
     ! The thread length, \int{ds}, from the photoshere to the given point
     ! Renamed and revised: in meters
     !/
     real,pointer :: LengthSi_III(:,:,:)
     !\
     ! The integral, \int{B ds}, from the photoshere to the given point
     ! Renamed and revised: only the value BDsInv_III(0,j,k) keeps this
     ! sense and used for calculating TMax_II(j,k). The values of
     ! BDsInv_III(negative iPoint,j,k) are equal to
     ! ds[m]/(B[T]*PoyntingFluxPerBSi). Are used to calculate
     ! the dimensionless heat flux as (Cons_I(iPoint)-Cons_I(iPoint+1))*BDsInv
     !/
     real,pointer :: BDsInvSi_III(:,:,:)
     !\
     ! Dimensionless TMax, such that the ingoing heat flux to the TR at this
     ! temperature equals the Poynting flux (which is not realistic and means
     ! that the input tempreture exceeding TMax assumes that something is
     ! going wrong.
     !/
     real,pointer :: TMax_II(:,:)

     !\
     ! Ds[m]/(B[T]*PoyntingFluxPerBSi)
     ! By multiplying this by 2*(3/2Pe) we obtain the internal energy in the
     ! interval between two grid points, in seconds. The physical meanin is
     ! how soon a plasma may be heated by this internal energy in the cell
     ! volume with having a BC for the Poynting flux at the left boundary
     ! and zero flux at the right boundary: In Si system
     !/
     real,pointer :: DsOverBSi_III(:,:,:)

     !\
     ! The integral, sqrt(PoyntingFluxPerB/LPerp**2)*\int{1/sqrt(B) ds}
     ! Dimensionless.
     !/
     real,pointer :: Xi_III(:,:,:)
     !\
     ! Magnetic field intensity, the inverse of heliocentric distance,
     ! and the gen coords
     ! Dimensionless.
     !/
     real,pointer :: B_III(:,:,:),RInv_III(:,:,:), Coord_DIII(:,:,:,:)
     !\
     ! The use:
     ! PeSi_I(iPoint) = PeSi_I(iPoint-1)*exp(-TGrav_III(iPoint)/TeSi_I(iPoint))
     !/
     real, pointer:: TGrav_III(:,:,:)
     !\
     ! The type of last update for the thread solution
     !/
     integer :: iAction

     !\
     !  Thread solution: temperature and pressure SI and amplitudes of waves
     !/
     real, pointer :: State_VIII(:,:,:,:)

     !\
     ! number of points
     !/
     integer,pointer :: nPoint_II(:,:)
     !\
     ! Distance between the true and ghost cell centers.
     !/
     real, pointer :: DeltaR_II(:,:)
     !\
     ! For visualization: 
     !/
     !\ Index of the last cell in radial direction
     integer :: iMin
     !\
     ! Inverse mesh size for the grid covering threaded gap
     !/
     real    :: dCoord1Inv   
     !/
     !\ PSi, TeSi amd TiSi for iMin:iMax,0:nJ+1,0:nK+1 as the first step
     real, pointer :: State_VG(:,:,:,:)
  end type BoundaryThreads
  !\
  ! Conponenets of array stored at each thread for visualization
  !/
  integer, public, parameter :: PSi_=1, A2Major_ = 2, AMajor_ = 2, &
       A2Minor_ = 3 , AMinor_ = 3, TeSi_=4, TiSi_ = TeSi_ + min(1, Pe_-1)
  
  !\
  ! To espress Te  and Ti in terms of P and rho, for ideal EOS:
  !/
  !\
  ! Te = TeFraction*State_V(iP)/State_V(Rho_)
  ! Pe = PeFraction*State_V(iP)
  ! Ti = TiFraction*State_V(p_)/State_V(Rho_)
  !/
  real, public    :: TeFraction, TiFraction, PeFraction
  integer, public :: iP

  type(BoundaryThreads), public, allocatable :: BoundaryThreads_B(:)

  !
  integer,public :: nPointThreadMax = 100
  real           :: DsThreadMin = 0.002

  real, parameter:: TeGlobalMaxSi = 1.80e7
  !\
  ! Logical from ModMain.
  !/
  public:: UseFieldLineThreads
  !\
  ! If .true., use threaded gap in plots
  !/
  logical, public :: DoPlotThreads = .true. 
  !\
  ! If .true., use triangulation
  !/
  logical, public :: UseTriangulation = .true.
  !\
  ! When the triangulation is done, interpolation may be done with
  ! two ways to find the interpolation weights: via the areas of spherical
  ! trangles, or via areas of planar trianles on a plane passing through
  ! three nodes of the interpolation stencil (the latter is the "original"
  ! interpolation mechanism by Renka, who proved its good theretical 
  ! properties, such as continuity of the interpolated valiable across the
  ! boundary of interpolation stencil). The "original" algorithm is applied
  ! if the following logical is set to .true.
  !/
  logical         :: UseInterpolationOrig = .false. 
  
  !\
  ! If .true. correct the contribution to the LOS plots from
  ! the transition region
  !/
  logical, public :: DoCorrectLosPlot4TR = .true.
  !\
  ! Number of threads, originating from physical cells
  !/
  integer :: nThread = -1
  !\
  ! Number of GhostCells in a uniform grid covering a threaded gap
  !/
  integer :: nGUniform = 10
  !\
  ! The following logical is .true., if nUniform >=1
  !/
  logical, public :: IsUniformGrid = .true.
  !\
  ! Resolution along radial direction, if the grid is uniform
  !/
  real,    public :: dCoord1Uniform  = -1.0
  real,    public :: Coord1TopThread = -1.0
  !\
  !The cell-centered grid starts at i=1, if nUniform <=0 (no uniform grid)
  !The uniform grid starts at i=0, if nUniform>=1.
  !/
  integer :: iMax = 0
  !\
  ! For the cell-centered grid,  the normalized radial gen coordinate 
  ! equals -0.5 for i=0. 
  ! The uniform grid starts at normalized coordinate equal to 0 at i=0.
  !/
  real   :: Coord1Norm0 = 0.0

  public:: BoundaryThreads
  public:: read_threads      ! Read parameters of threads
  public:: check_tr_table    ! Calculate a table for transition region
  public:: set_threads       ! (Re)Sets threads in the inner boundary blocks

  !\
  ! Called prior to different invokes of set_cell_boundary, to determine
  ! how the solution on the thread should be (or not be) advanced:
  ! after hydro stage or after the heat conduction stage etc
  !/
  public:: advance_threads
  !\
  ! Saves restart
  !/
  public :: save_thread_restart
  !\
  ! Correspondent named indexes
  !/
  integer,public,parameter:: DoInit_=-1, Done_=0, Enthalpy_=1, Heat_=2

  !\
  ! The number of grid spaces which are covered by the TR model
  ! the smaller is this number, the better the TR assumption work
  ! However, 1 is not recommended, as long as the length of the
  ! last interval is not controlled (may be too small)
  !/
  integer, parameter:: nIntervalTR = 2
  !\
  ! Number of threads on each processor
  !/
  integer, allocatable :: nThread_P(:)
  !\
  !   Hydrostatic equilibrium in an isothermal corona:
  !    d(N_i*k_B*(Z*T_e +T_i) )/dr=G*M_sun*N_I*M_i*d(1/r)/dr
  ! => N_i*Te\propto exp(cGravPot/TeSi*(M_i[amu]/(1+Z))*\Delta(R_sun/r))
  !/
  !\
  ! The plasma properties dependent coefficient needed to evaluate the
  ! eefect of gravity on the hydrostatic equilibrium
  !/
  real,public :: GravHydroStat != cGravPot*MassIon_I(1)/(AverageIonCharge + 1)

  logical :: IsInitialized = .false.
  character(len=100) :: NameRestartFile
contains
  !============================================================================
  subroutine init
    integer :: iBlock !Loop variable
    !------------
    if(IsInitialized)RETURN
    IsInitialized = .true.
    !\
    ! TeFraction is used for ideal EOS:
    !/
    if(UseElectronPressure)then
       ! Pe = ne*Te (dimensionless) and n=rho/ionmass
       ! so that Pe = ne/n *n*Te = (ne/n)*(rho/ionmass)*Te
       ! TeFraction is defined such that Te = Pe/rho * TeFraction
       TeFraction = MassIon_I(1)/Z
       ! Pi = n*Te (dimensionless) and n=rho/ionmass
       ! so that Pi = (rho/ionmass)*Ti
       ! TiFraction is defined such that Ti = Pi/rho * TiFraction
       TiFraction = MassIon_I(1)
       iP = Pe_
       PeFraction = 1.0
    else
       ! p = n*T + ne*Te (dimensionless) and n=rho/ionmass
       ! so that p=rho/massion *T*(1+ne/n Te/T)
       ! TeFraction is defined such that Te = p/rho * TeFraction
       TeFraction = MassIon_I(1)/(1 + Z)
       TiFraction = TeFraction
       iP = p_
       PeFraction = Z/(1.0 + Z)
    end if
    ! Therefore Te = TeFraction*State_V(iP)/State_V(Rho_)
    ! Pe = PeFraction*State_V(iP)
    !/
  
    allocate(        DoThreads_B(MaxBlock))
    allocate(IsAllocatedThread_B(MaxBlock))
    DoThreads_B = .false.
    IsAllocatedThread_B = .false.
    allocate(nThread_P(0:nProc - 1))
    nThread_P = 0
    allocate(BoundaryThreads_B(1:MaxBlock))
    do iBlock = 1, MaxBlock
       call nullify_thread_b(iBlock)
    end do
  end subroutine init
  !============================================================================
  subroutine clean
    integer :: iBlock !Loop variable
    !----------------------------
    if(.not.IsInitialized)RETURN
    IsInitialized = .false.
    deallocate(DoThreads_B)
    do iBlock = 1, MaxBlock
       if(IsAllocatedThread_B(iBlock))&
            call deallocate_thread_b(iBlock)
    end do
    deallocate(IsAllocatedThread_B)
    deallocate(  BoundaryThreads_B)
    deallocate(nThread_P)
  end subroutine clean
  !============================================================================
  subroutine read_threads(NameCommand, iSession)
    use ModReadParam, ONLY: read_var
    use ModMain,      ONLY: NameThisComp
    character(len=*), intent(in):: NameCommand
    integer, intent(in):: iSession
    integer :: iBlock, iError
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_threads'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(NameCommand)
    case("#FIELDLINETHREAD")
       call read_var('UseFieldLineThreads', UseFieldLineThreads)
       if(UseFieldLineThreads)then
          if(iSession/=1)call stop_mpi(&
               'UseFieldLineThreads can be set ON in the first session only')
          call read_var('nPointThreadMax', nPointThreadMax)
          call read_var('DsThreadMin', DsThreadMin)
       else
          call clean
       end if
    case('#PLOTTHREADS')
       call read_var('DoPlotThreads', DoPlotThreads)
       if(DoPlotThreads)call read_var('nGUniform', nGUniform)
       if(nGUniform > 0)then
          !\
          ! Use uniform grid
          !/
          IsUniformGrid = .true.
          iMax = 0
          Coord1Norm0 = 0.0
          call read_var('UseTriangulation', UseTriangulation)
          !\
          ! If the non-uniform grid is used extending the existing block
          ! adaptive grid, the grid normally does not even reach the 
          ! chromosphere height, so that the contribution from the TR is 
          ! not worth while quantifying. With the uniform grid, this 
          ! option is available.
          !/
          call read_var('DoTRCorrection', DoCorrectLosPlot4TR)
          !\
          ! When the triangulation is done, interpolation may be done with
          ! two ways to find the interpolation weights: via the areas of 
          ! spherical triangles, or via areas of planar trianles on a plane
          ! latter is the "original" passing through three nodes of the 
          ! interpolation stencil (the interpolation mechanism by Renka, who 
          ! proved its good theretical properties, such as continuity of
          ! the interpolated valiable across the boundary of interpolation 
          ! stencil). The "original" algorithm is applied if the following 
          ! logical is set to .true.
          !/
          call read_var('UseInterpolationOrig', UseInterpolationOrig, iError)
          if(iError /= 0)then
             UseInterpolationOrig = .false.
             if(iProc==0)write(*,'(a)')&
                  NameThisComp//': Missing UseInterpolationOrig is set to F'
             RETURN
          end if         

       else
          IsUniformGrid = .false.
          iMax = 1
          Coord1Norm0 = -0.50
       end if
    case default
       call stop_mpi(NameSub//": unknown command="//trim(NameCommand))
    end select
    call test_stop(NameSub, DoTest)
  end subroutine read_threads
  !============================================================================
  subroutine nullify_thread_b(iBlock)
    integer, intent(in) :: iBlock
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'nullify_thread_b'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    nullify(BoundaryThreads_B(iBlock) % LengthSi_III)
    nullify(BoundaryThreads_B(iBlock) % BDsInvSi_III)
    nullify(BoundaryThreads_B(iBlock) % DsOverBSi_III)
    nullify(BoundaryThreads_B(iBlock) % TMax_II)
    nullify(BoundaryThreads_B(iBlock) % Xi_III)
    nullify(BoundaryThreads_B(iBlock) % B_III)
    nullify(BoundaryThreads_B(iBlock) % RInv_III)
    nullify(BoundaryThreads_B(iBlock) % Coord_DIII)
    nullify(BoundaryThreads_B(iBlock) % TGrav_III)
    nullify(BoundaryThreads_B(iBlock) % State_VIII)
    nullify(BoundaryThreads_B(iBlock) % nPoint_II)
    nullify(BoundaryThreads_B(iBlock) % DeltaR_II)
    nullify(BoundaryThreads_B(iBlock) % State_VG)
    BoundaryThreads_B(iBlock) % iAction    = 0
    BoundaryThreads_B(iBlock) % iMin       = 0
    BoundaryThreads_B(iBlock) % DCoord1Inv = 0.0
    call test_stop(NameSub, DoTest, iBlock)
  end subroutine nullify_thread_b
  !============================================================================
  subroutine deallocate_thread_b(iBlock)
    integer, intent(in) :: iBlock
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'deallocate_thread_b'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    deallocate(BoundaryThreads_B(iBlock) % LengthSi_III)
    deallocate(BoundaryThreads_B(iBlock) % BDsInvSi_III)
    deallocate(BoundaryThreads_B(iBlock) % DsOverBSi_III)
    deallocate(BoundaryThreads_B(iBlock) % TMax_II)
    deallocate(BoundaryThreads_B(iBlock) % Xi_III)
    deallocate(BoundaryThreads_B(iBlock) % B_III)
    deallocate(BoundaryThreads_B(iBlock) % RInv_III)
    deallocate(BoundaryThreads_B(iBlock) % Coord_DIII)
    deallocate(BoundaryThreads_B(iBlock) % TGrav_III)
    deallocate(BoundaryThreads_B(iBlock) % State_VIII)
    deallocate(BoundaryThreads_B(iBlock) % nPoint_II)
    deallocate(BoundaryThreads_B(iBlock) % DeltaR_II)
    deallocate(BoundaryThreads_B(iBlock) % State_VG)
    IsAllocatedThread_B(iBlock) = .false.
    call nullify_thread_b(iBlock)
    call test_stop(NameSub, DoTest, iBlock)
  end subroutine deallocate_thread_b
  !============================================================================
  subroutine set_threads
    use BATL_lib,     ONLY: MaxBlock, Unused_B, nBlock
    use ModParallel, ONLY: NeiLev, NOBLK
    use ModMpi
    integer:: iBlock, nBlockSet, nBlockSetAll, nPointMin, nPointMinAll, j, k
    integer:: iError
    real   :: dCoord1UniformPe = -1.0
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_threads'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    nBlockSet = 0
    nPointMin = nPointThreadMax
    do iBlock = 1, MaxBlock
       if(Unused_B(iBlock))then
          DoThreads_B(iBlock) = .false.
          if(IsAllocatedThread_B(iBlock))&
               call deallocate_thread_b(iBlock)
          CYCLE
       end if
       if(.not.DoThreads_B(iBlock))CYCLE
       DoThreads_B(iBlock) = .false.
       !\
       ! Check if the block is at the inner boundary
       ! Otherwise CYCLE
       !/
       if(NeiLev(1,iBlock)/=NOBLK)then
          if(IsAllocatedThread_B(iBlock))&
               call deallocate_thread_b(iBlock)
          CYCLE
       end if
       !\
       ! Allocate threads if needed
       !/
       if(.not.IsAllocatedThread_B(iBlock))then
          allocate(BoundaryThreads_B(iBlock) % LengthSi_III(&
               -nPointThreadMax:0,jMin_:jMax_, kMin_:kMax_))
          BoundaryThreads_B(iBlock) % LengthSi_III = 0.0
          allocate(BoundaryThreads_B(iBlock) % BDsInvSi_III(&
               -nPointThreadMax:0,jMin_:jMax_, kMin_:kMax_))
          BoundaryThreads_B(iBlock) % BDsInvSi_III = 0.0
          allocate(BoundaryThreads_B(iBlock) % DsOverBSi_III(&
               -nPointThreadMax:0,jMin_:jMax_, kMin_:kMax_))
          BoundaryThreads_B(iBlock) % DsOverBSi_III = 0.0
          allocate(BoundaryThreads_B(iBlock) % TMax_II(&
               jMin_:jMax_, kMin_:kMax_))
          BoundaryThreads_B(iBlock) % TMax_II = 0.0
          allocate(BoundaryThreads_B(iBlock) % Xi_III(&
               -nPointThreadMax:0,jMin_:jMax_, kMin_:kMax_))
          BoundaryThreads_B(iBlock) % Xi_III = 0.0
          allocate(BoundaryThreads_B(iBlock) % B_III(&
               -nPointThreadMax:0,jMin_:jMax_, kMin_:kMax_))
          BoundaryThreads_B(iBlock) % B_III = 0.0
          allocate(BoundaryThreads_B(iBlock) % RInv_III(&
               -nPointThreadMax:0,jMin_:jMax_, kMin_:kMax_))
          BoundaryThreads_B(iBlock) % RInv_III = 0.0
          allocate(BoundaryThreads_B(iBlock) % Coord_DIII(3,&
               -nPointThreadMax:0,jMin_:jMax_, kMin_:kMax_))
          BoundaryThreads_B(iBlock) % Coord_DIII = 0.0
          allocate(BoundaryThreads_B(iBlock) % TGrav_III(&
               -nPointThreadMax:0,jMin_:jMax_, kMin_:kMax_))
          BoundaryThreads_B(iBlock) % TGrav_III = 0.0
          allocate(BoundaryThreads_B(iBlock) % State_VIII(PSi_:TiSi_,&
               -nPointThreadMax:0,jMin_:jMax_, kMin_:kMax_))
          BoundaryThreads_B(iBlock) % State_VIII = 0.0
          allocate(BoundaryThreads_B(iBlock) % nPoint_II(&
               jMin_:jMax_, kMin_:kMax_))
          BoundaryThreads_B(iBlock) % nPoint_II = -1
          allocate(BoundaryThreads_B(iBlock) % DeltaR_II(&
               jMin_:jMax_, kMin_:kMax_))
          BoundaryThreads_B(iBlock) % DeltaR_II = 0.0
          call set_gc_grid(iBlock, BoundaryThreads_B(iBlock) % iMin,&
               BoundaryThreads_B(iBlock) % dCoord1Inv)
          allocate(BoundaryThreads_B(iBlock) % State_VG(&
               PSi_:TiSi_, BoundaryThreads_B(iBlock) % iMin:1,&
               jMin_:jMax_, kMin_:kMax_))
          BoundaryThreads_B(iBlock) % State_VG = 0.0
          IsAllocatedThread_B(iBlock) = .true.
       end if
       !\
       ! The threads are now set in a just created block, or
       ! on updating B_0 field
       !/
       call set_threads_b(iBlock)
       nBlockSet = nBlockSet + 1
       do k = kMin_, kMax_; do j = jMin_, jMax_
          nPointMin = min(nPointMin, BoundaryThreads_B(iBlock)%nPoint_II(j,k))
       end do; end do
    end do
    !\
    ! Number of threads (originating from physical cells) at the given PE
    !/
    nThread = count(IsAllocatedThread_B(1:nBlock))*nJ*nK
    if(nProc==1)then
       nBlockSetAll = nBlockSet
       nPointMinAll = nPointMin
       nThread_P(0) = nThread
    else
       call MPI_ALLREDUCE(nBlockSet, nBlockSetAll, 1, MPI_INTEGER, MPI_SUM,&
            iComm, iError)
       call MPI_REDUCE(nPointMin, nPointMinAll, 1, MPI_INTEGER, MPI_MIN,&
            0, iComm, iError)
       call MPI_ALLGATHER(nThread, 1, MPI_INTEGER, nThread_P, 1, MPI_INTEGER,&
            iComm, iError)
       nThread = sum(nThread_P)
    end if
    if(nBlockSetAll > 0.and.iProc==0)then
       write(*,*)'Set threads in ',nBlockSetAll,' blocks'
       write(*,*)'nPointMin = ',nPointMinAll
       if(IsUniformGrid)then
          write(*,*)'dCoord1Uniform =', dCoord1Uniform
       end if
       if(nProc<=4)write(*,*)'Number of threads on different PEs: ', nThread_P
    end if
    call test_stop(NameSub, DoTest)
  contains
    !==========================================================================
    subroutine set_gc_grid(iBlock, iMin, dCoord1Inv)
      use BATL_lib, ONLY: MaxDim, xyz_to_coord, coord_to_xyz, &
           CoordMin_DB, CellSize_DB, r_
      integer, intent(in) :: iBlock
      integer, intent(out):: iMin
      real,    intent(out):: dCoord1Inv
      real :: Coord_D(MaxDim), Xyz_D(MaxDim)
      !-----------------------------------------------------------------------
      !\
      ! Gen coords for the low block corner
      !/
      call coord_to_xyz(CoordMin_DB(:, iBlock), Xyz_D)
      !\
      ! Projection onto the photosphere level
      !/
      Xyz_D = Xyz_D/norm2(Xyz_D)*rBody
      !\
      ! Generalized coords of the latter.
      !/
      call xyz_to_coord(Xyz_D, Coord_D)
      !\
      ! Minimal index of the ghost cells, in radial direction
      ! (equals 0 if there is only 1 ghost cell)
      !/
      if(nGUniform > 0)then
         iMin = -nGUniform
         if(dCoord1Uniform < 0.0)then
            dCoord1Uniform = (CoordMin_DB(r_, iBlock) - Coord_D(r_))/&
                 nGUniform  
            Coord1TopThread = CoordMin_DB(r_, iBlock)
         end if
         dCoord1Inv = 1.0/dCoord1Uniform
      else
         iMin = 1 - floor( (CoordMin_DB(r_, iBlock) - Coord_D(r_))/&
              CellSize_DB(r_, iBlock) )
         dCoord1Inv = 1.0/CellSize_DB(r_, iBlock)
      end if
    end subroutine set_gc_grid
  end subroutine set_threads
  !============================================================================
  subroutine set_threads_b(iBlock)
    use EEE_ModCommonVariables, ONLY: UseCme
    use EEE_ModMain,            ONLY: EEE_get_state_BC
    use ModMain,       ONLY: n_step, iteration_number, time_simulation, &
         DoThreadRestart
    use ModGeometry, ONLY: Xyz_DGB
    use ModPhysics,  ONLY: Si2No_V, No2Si_V,&
                           UnitTemperature_, UnitX_, UnitB_
    use ModNumConst, ONLY: cTolerance
    use ModCoronalHeating, ONLY:PoyntingFluxPerBSi, PoyntingFluxPerB, &
         LPerpTimesSqrtB
    use BATL_lib,    ONLY: MaxDim, xyz_to_coord, r_
    integer, intent(in) :: iBlock
    !\
    ! Locals:
    !/
    ! Loop variable: (j,k) enumerate the cells at which
    ! the threads starts, iPoint starts from negative
    ! values at the photospheric end and the maximal
    ! value of this index is 0 for the thread point
    ! at the physical cell center.
    integer :: j, k, iPoint, nTrial, iInterval, nPoint

    ! Length interval, !Heliocentric distance
    real :: Ds, R, RStart
    ! coordinates, field vector and modulus
    real :: Xyz_D(MaxDim), Coord_D(MaxDim), B0_D(MaxDim), B0
    ! Same stored for the starting point
    real :: XyzStart_D(MaxDim), B0Start_D(MaxDim),  B0Start
    ! 1 for ourward directed field, -1 otherwise
    real ::SignBr
    ! Coordinates and magnetic field in the midpoint
    ! within the framework of the Runge-Kutta scheme
    real :: XyzAux_D(MaxDim), B0Aux_D(MaxDim)
    !\
    ! CME parameters, if needed
    !/
    real:: RhoCme, Ucme_D(MaxDim), Bcme_D(MaxDim), pCme
    ! Aux
    real :: ROld, Aux, CoefXi
    real :: DirB_D(MaxDim), DirR_D(MaxDim), XyzOld_D(MaxDim)
    real:: CosBRMin = 1.0
    integer, parameter::nCoarseMax = 2

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_threads_b'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    !\
    ! Initialize threads
    !/
    BoundaryThreads_B(iBlock) % iAction = DoInit_
    BoundaryThreads_B(iBlock) % LengthSi_III = 0.0
    BoundaryThreads_B(iBlock) % BDsInvSi_III = 0.0
    BoundaryThreads_B(iBlock) % DsOverBSi_III = 0.0
    BoundaryThreads_B(iBlock) % TMax_II = 1.0e8*Si2No_V(UnitTemperature_)
    BoundaryThreads_B(iBlock) % Xi_III = 0.0
    BoundaryThreads_B(iBlock) % B_III = 0.0
    BoundaryThreads_B(iBlock) % RInv_III = 0.0
    BoundaryThreads_B(iBlock) % Coord_DIII = 0.0
    BoundaryThreads_B(iBlock) % TGrav_III = 0.0
    BoundaryThreads_B(iBlock) % State_VIII(TeSi_:TiSi_,:,:,:) = -1
    BoundaryThreads_B(iBlock) % State_VIII(PSi_:A2Minor_,:,:,:) = 0.0
    BoundaryThreads_B(iBlock) % nPoint_II = 0
    BoundaryThreads_B(iBlock) % DeltaR_II = 1.0
    !\
    ! sqrt(CoefXi/VAlfven[NoDim]) is dXi/ds[NoDim]
    ! While setting thread,  we calculate only the thread-dependent
    ! sqrt(CoefXi/B[NoDim]). In ModThreadedLC DXi will be multiplied
    ! by RhoNoDim**0.25
    !/
    CoefXi = PoyntingFluxPerB/LperpTimesSqrtB**2

    ! Loop over the thread starting points
    do k = kMin_, kMax_; do j = jMin_, jMax_
       !\
       ! First, take magnetic field in the ghost cell
       !/
       !\
       ! Starting points for all threads are in the centers
       ! of  physical cells near the boundary
       !/
       XyzStart_D = Xyz_DGB(:, 1, j, k, iBlock)
       !\
       ! Calculate a field in the starting point
       !/
       call get_b0(XyzStart_D, B0Start_D)
       if(UseCME)then
          call EEE_get_state_BC(XyzStart_D, RhoCme, Ucme_D, Bcme_D, pCme, &
               time_simulation, n_step, iteration_number)
          Bcme_D = Bcme_D*Si2No_V(UnitB_)
          B0Start_D = B0Start_D + Bcme_D
       end if

       SignBr = sign(1.0, sum(XyzStart_D*B0Start_D) )

       B0Start = norm2(B0Start_D)
       BoundaryThreads_B(iBlock) % B_III(0, j, k) = B0Start

       RStart = norm2(XyzStart_D)
       BoundaryThreads_B(iBlock) % RInv_III(0, j, k) = 1/RStart
       call xyz_to_coord(XyzStart_D, Coord_D)
       BoundaryThreads_B(iBlock) % Coord_DIII(:,0, j, k) = Coord_D

       Ds = 0.50*DsThreadMin ! To enter the grid coarsening loop
       COARSEN: do nTrial=1,nCoarseMax ! Ds is increased to 0.002 or 0.016
          !\
          ! Set initial Ds or increase Ds, if previous trial fails
          !/
          Ds = Ds * 2
          iPoint = 0
          Xyz_D = XyzStart_D
          B0 = B0Start
          B0_D = B0Start_D
          R = RStart
          if(nTrial==nCoarseMax)then
             CosBRMin = ( (RStart**2-rBody**2)/nPointThreadMax +Ds**2)/&
                  (2*rBody*Ds)
             if(CosBRMin>0.9)call stop_mpi('Increase nPointThreadMax')
          end if
          POINTS: do
             iPoint = iPoint + 1
             !\
             ! If the number of gridpoints in the theads is too
             ! high, coarsen the grid
             !/
             if(iPoint > nPointThreadMax)CYCLE COARSEN
             !\
             ! For the previous point given are Xyz_D, B0_D, B0
             ! R is only used near the photospheric end.
             !/
             ! Two stage Runge-Kutta
             ! 1. Point at the half of length interval:
             DirR_D = Xyz_D/R
             DirB_D = SignBr*B0_D/max(B0, cTolerance)
             if(nTrial==nCoarseMax)call limit_cosBR
             XyzAux_D = Xyz_D - 0.50*Ds*DirB_D

             ! 2. Magnetic field in this point:
             call get_b0(XyzAux_D, B0Aux_D)
             if(UseCME)then
                call EEE_get_state_BC(XyzAux_D, RhoCme, Ucme_D, Bcme_D, pCme, &
                     time_simulation, n_step, iteration_number)
                Bcme_D = Bcme_D*Si2No_V(UnitB_)
                B0Aux_D = B0Aux_D + Bcme_D
             end if
             DirB_D = SignBr*B0Aux_D/max(norm2(B0Aux_D), cTolerance**2)
             if(nTrial==nCoarseMax)call limit_cosBR
             ! 3. New grid point:
             Xyz_D = Xyz_D - Ds*DirB_D
             R = norm2(Xyz_D)
             if(R <= rBody)EXIT COARSEN
             if(R > RStart)CYCLE COARSEN
             !\
             ! Store a point
             !/
             XyzOld_D = Xyz_D
             BoundaryThreads_B(iBlock) % RInv_III(-iPoint, j, k) = 1/R
             call xyz_to_coord(Xyz_D, Coord_D)
             BoundaryThreads_B(iBlock) % Coord_DIII(:,-iPoint, j, k) = Coord_D
             call get_b0(Xyz_D, B0_D)
             if(UseCME)then
                call EEE_get_state_BC(Xyz_D, RhoCme, Ucme_D, Bcme_D, pCme, &
                     time_simulation, n_step, iteration_number)
                Bcme_D = Bcme_D*Si2No_V(UnitB_)
                B0_D = B0_D + Bcme_D
             end if
             B0 = norm2(B0_D)
             BoundaryThreads_B(iBlock) % B_III(-iPoint, j, k) = B0
          end do POINTS
       end do COARSEN
       if(R > rBody)then
          write(*,*)'iPoint, R=', iPoint, R
          call stop_mpi('Thread did not reach the photosphere!')
       end if

       ! Calculate more accurately the intersection point
       ! with the photosphere surface
       ROld = 1/BoundaryThreads_B(iBlock) % RInv_III(1-iPoint, j, k)
       Aux = (ROld - RBody) / (ROld -R)
       Xyz_D =(1 - Aux)*XyzOld_D +  Aux*Xyz_D
       !\
       ! Store the last point
       !/
       BoundaryThreads_B(iBlock) % RInv_III(-iPoint, j, k) = 1/RBody
       call get_b0(Xyz_D, B0_D)
       B0 = norm2(B0_D)
       BoundaryThreads_B(iBlock) % B_III(-iPoint, j, k) = B0
       !\
       ! Store the number of points. This will be the number of temperature
       ! nodes such that the first one is on the top of the TR, the last
       ! one is in the center of physical cell
       !/
       nPoint = iPoint + 1 - nIntervalTR
       BoundaryThreads_B(iBlock) % nPoint_II(j,k) = nPoint
       BoundaryThreads_B(iBlock)%TGrav_III(2-nPoint:0,j,k) = &
            GravHydroStat*&
            (-BoundaryThreads_B(iBlock)%RInv_III(2-nPoint:0,j,k) + &
            BoundaryThreads_B(iBlock)%RInv_III(1-nPoint:-1,j,k))

       !\
       ! Store the lengths
       !/
       BoundaryThreads_B(iBlock) % LengthSi_III(1-iPoint, j, k) = &
            Ds*Aux*No2Si_V(UnitX_)
       BoundaryThreads_B(iBlock) % BDsInvSi_III(1-iPoint, j, k) = &
            Ds*Aux*0.50*(                                         &
            BoundaryThreads_B(iBlock) % B_III(-iPoint, j, k) +    &
            BoundaryThreads_B(iBlock) % B_III(1-iPoint, j, k) )
       iPoint = iPoint - 1
       !\
       !     |           |
       !-iPoint-1      -iPoint
       !/
       iInterval = 1
       do while(iInterval < nIntervalTR)
          BoundaryThreads_B(iBlock) % LengthSi_III(1-iPoint, j, k) =     &
               BoundaryThreads_B(iBlock) % LengthSi_III(-iPoint, j, k) + &
               Ds*No2Si_V(UnitX_)
          BoundaryThreads_B(iBlock) % BDsInvSi_III(1-iPoint, j, k) =     &
               BoundaryThreads_B(iBlock) % BDsInvSi_III(-iPoint, j, k) + &
               Ds*0.50*(&
               BoundaryThreads_B(iBlock) % B_III( -iPoint, j, k) +       &
               BoundaryThreads_B(iBlock) % B_III(1-iPoint, j, k) )
          iPoint = iPoint - 1
          iInterval = iInterval + 1
       end do
       !\
       !     |            |...........|           Temperature nodes
       !-iPoint-nInterval          -iPoint
       !                        x           x            Flux nodes
       !                    -iPoint-1    -iPoint
       ! Here, iPoint is the current value, equal to nPoint-1, nPoint being the
       ! stored value. Now, LengthSi = Ds*(nInterval - 1 + Aux), as long as the
       ! first interval is shorter than Ds. Then, BDsInvSi is the
       ! dimensionless and not inverted integral of Bds over the first
       ! intervals.
       !/

       BoundaryThreads_B(iBlock) % BDsInvSi_III(-1-iPoint, j, k) = 1/  &
            (BoundaryThreads_B(iBlock) % LengthSi_III(-iPoint, j, k)*  &
            BoundaryThreads_B(iBlock) % B_III(-iPoint, j, k)*          &
            No2Si_V(UnitB_)*PoyntingFluxPerBSi)

       !\
       ! The flux node -iPoint-1=-nPoint is placed to the same position
       ! as the temperature node with the same number
       !/
       BoundaryThreads_B(iBlock) % Xi_III(-iPoint, j, k) =             &
            0.50*Ds*sqrt(CoefXi/                                       &
            BoundaryThreads_B(iBlock) % B_III(-iPoint, j, k))
       !\
       ! As long as the flux node is placed as discussed above, the
       ! first computational cell is twice shorter
       !/
       BoundaryThreads_B(iBlock) % DsOverBSi_III(-iPoint, j, k) =      &
            0.50*Ds*No2Si_V(UnitX_)/                                   &
            ( BoundaryThreads_B(iBlock) % B_III(-iPoint, j, k)&
            *PoyntingFluxPerBSi*No2Si_V(UnitB_) )

       do while(iPoint>0)
          !\
          ! Just the length of Thread in meters
          !/
          BoundaryThreads_B(iBlock) % LengthSi_III(1-iPoint, j, k) =      &
               BoundaryThreads_B(iBlock) % LengthSi_III(-iPoint, j, k ) + &
               Ds*No2Si_V(UnitX_)
          !\
          ! Sum up the integral of Bds, dimensionless, to calculate TMax
          !/
          BoundaryThreads_B(iBlock) % BDsInvSi_III(1-iPoint, j, k) =      &
               BoundaryThreads_B(iBlock) % BDsInvSi_III(-iPoint, j, k) +  &
               Ds*0.50*(&
               BoundaryThreads_B(iBlock) % B_III(-iPoint, j, k) +&
               BoundaryThreads_B(iBlock) % B_III(1-iPoint, j, k) )
          !\
          ! 1/(ds*B*Poynting-flux-over-field ratio)
          ! calculated in the flux point, so that the averaged magnetic field
          ! is used. Calculated in SI units. While multiplied by the difference
          ! in the conservative variable, 2/7*kappa*\Delta T^{7/2} gives the
          ! ratio of the heat flux to the effective Poynting flux.
          !/
          BoundaryThreads_B(iBlock) % BDsInvSi_III(-iPoint, j, k) =   1/  &
               (PoyntingFluxPerBSi*No2Si_V(UnitB_)*&
               No2Si_V(UnitX_)*Ds*0.50*(&
               BoundaryThreads_B(iBlock) % B_III(-iPoint, j, k) +&
               BoundaryThreads_B(iBlock) % B_III(1-iPoint, j, k) ) )
          !\
          ! The distance between the flux points (as well as the AW amplitude
          ! nodes multiplied by the coefficient to find dimensionless
          ! wave damping per the unit of length. Calculated in terms of
          ! the magnetic field in the midpoint. Should be multiplied by the
          ! dimensionless density powered 1/4. Dimensionless.
          !/
          BoundaryThreads_B(iBlock) % Xi_III(1-iPoint, j, k) =            &
               BoundaryThreads_B(iBlock) % Xi_III(-iPoint, j, k)        + &
               Ds*sqrt(CoefXi/               &
               BoundaryThreads_B(iBlock) % B_III(1-iPoint, j, k))
          !\
          ! Distance between the flux points (faces) divided by
          ! the magnetic field in the temperature point (or, same,
          ! multiplied by the crosssection of the flux tube)
          ! and divided by the Poynting-flux-to-field ratio.
          !/
          BoundaryThreads_B(iBlock) % DsOverBSi_III(1-iPoint, j, k) =     &
               Ds*No2Si_V(UnitX_)/                                        &
               (BoundaryThreads_B(iBlock) % B_III(1-iPoint, j, k)         &
               *PoyntingFluxPerBSi*No2Si_V(UnitB_))
          iPoint = iPoint - 1
       end do
       !\
       !          |       |           Temperature nodes
       !         -1       0
       !              x       x       Flux nodes
       !             -1       0
       !/

       !\
       ! Correct the effective length of the last flux node
       !          |       |           Temperature nodes
       !         -1       0
       !              x   <---x       Flux nodes
       !             -1       0
       !
       !/
       BoundaryThreads_B(iBlock) % Xi_III(0, j, k) =         &
            BoundaryThreads_B(iBlock) % Xi_III(0, j, k) -    &
            0.50*Ds*sqrt(CoefXi/                             &
            BoundaryThreads_B(iBlock) % B_III(0, j, k))

       BoundaryThreads_B(iBlock) % DeltaR_II(j,k) = &
            norm2(Xyz_DGB(:,1,j,k,iBlock) - Xyz_DGB(:,0,j,k,iBlock) )

       call limit_temperature(BoundaryThreads_B(iBlock) % BDsInvSi_III(&
               0, j, k), BoundaryThreads_B(iBlock) % TMax_II(j, k))

    end do; end do
    if(DoThreadRestart)call read_thread_restart(iBlock)

    if(DoTest.and.iBlock==iBlockTest)then
       write(*,'(a,3es18.10)')'Thread starting at the point  ',&
            Xyz_DGB(:,1,jTest,kTest,iBlock)
       write(*,'(a,3es18.10)')'DeltaR=  ',&
            BoundaryThreads_B(iBlock) % DeltaR_II(jTest,kTest)
       write(*,'(a)')&
            'B[NoDim] RInv[NoDim] LengthSi BDsInvSi Xi[NoDim]'
       do iPoint = 0, -BoundaryThreads_B(iBlock) % nPoint_II(jTest,kTest),-1
          write(*,'(5es18.10)')&
               BoundaryThreads_B(iBlock) % B_III(&
               iPoint, jTest,kTest),&
               BoundaryThreads_B(iBlock) % RInv_III(&
               iPoint, jTest,kTest),&
               BoundaryThreads_B(iBlock) % LengthSi_III(&
               iPoint, jTest, kTest),&
               BoundaryThreads_B(iBlock) % BDsInvSi_III(&
               iPoint, jTest, kTest),&
               BoundaryThreads_B(iBlock) % Xi_III(&
               iPoint, jTest, kTest)
       end do
    end if
    call test_stop(NameSub, DoTest, iBlock)
  contains
    !==========================================================================
    subroutine limit_cosbr
      real::CosBR
      !------------------------------------------------------------------------
      CosBR = sum(DirB_D*DirR_D)
      if(CosBR>=CosBRMin)RETURN
      DirB_D = (DirB_D - CosBR*DirR_D)*&      ! Tangential componenets
           sqrt((1 - CosBRMin**2)/(1 - CosBR**2))+& ! Reduced in magnitude
           DirR_D*CosBRMin          ! Plus increased radial comp
    end subroutine limit_cosbr
    !==========================================================================
    subroutine limit_temperature(BLength, TMax)
      use ModPhysics,      ONLY: UnitX_, Si2No_V, UnitB_
      use ModLookupTable,  ONLY: i_lookup_table, interpolate_lookup_table
      real, intent(in)  :: BLength
      real, intent(out) :: TMax
      real :: HeatFluxXLength, Value_V(LengthPAvrSi_:DLogLambdaOverDLogT_)
      !------------------------------------------------------------------------
      HeatFluxXLength = 2*PoyntingFluxPerBSi*&
           BLength*No2Si_V(UnitX_)*No2Si_V(UnitB_)
      call interpolate_lookup_table(iTable=iTableTR,&
                                    iVal=HeatFluxLength_, &
                                    ValIn=HeatFluxXLength,&
                                    Value_V=Value_V,      &
                                    Arg1Out=TMax,  &
                                    DoExtrapolate=.false.)
      !\
      ! Version Easter 2015
      ! Globally limiting the temparture is mot much
      ! physical, however, at large enough timerature
      ! the existing semi-implicit solver is unstable
      !/
      TMax = min(TMax,TeGlobalMaxSi)*Si2No_V(UnitTemperature_)
    end subroutine limit_temperature
    !==========================================================================
  end subroutine set_threads_b
  !============================================================================
  subroutine advance_threads(iAction)

    use BATL_lib, ONLY: nBlock, Unused_B

    integer, intent(in)::iAction

    integer:: iBlock
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'advance_threads'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE
       if(.not.IsAllocatedThread_B(iBlock))CYCLE
       if(BoundaryThreads_B(iBlock)%iAction /= Done_)&
            call stop_mpi('An attempt to readvance not advanced threads')
       BoundaryThreads_B(iBlock)%iAction = iAction
    end do

    call test_stop(NameSub, DoTest)
  end subroutine advance_threads
  !============================================================================
  subroutine read_thread_restart(iBlock)
    use ModMain,       ONLY: NameThisComp
    use ModIoUnit,     ONLY: UnitTmp_
    use ModUtilities,  ONLY: open_file, close_file
    integer, intent(in) :: iBlock
    !\
    ! loop variables
    !/
    integer :: j, k
    !\
    ! Misc:
    integer :: nPoint, iError
    real    :: RealNPoint
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_thread_restart'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    call get_restart_file_name(iBlock, NameThisComp//'/restartIN/')
    call open_file(file=NameRestartFile, status='old', &
         form='UNFORMATTED', NameCaller=NameSub)
    do k = kMin_,kMax_; do j = jMin_, jMax_
       read(UnitTmp_, iostat = iError)RealNPoint
       if(iError>0)then
          write(*,*)'Error in reading nPoint in Block=', iBlock
          call close_file
          RETURN
       end if
       nPoint = nint(RealNPoint)
       if(BoundaryThreads_B(iBlock) % nPoint_II(j,k)/=nPoint)then
          write(*,*)'Incorrest nPoint in Block=', iBlock
          call close_file
          RETURN
       end if
       read(UnitTmp_, iostat = iError) &
            BoundaryThreads_B(iBlock) % State_VIII(TeSi_,1-nPoint:0,j,k), &
            BoundaryThreads_B(iBlock) % State_VIII(TiSi_,1-nPoint:0,j,k), &
            BoundaryThreads_B(iBlock) % State_VIII(PSi_,1-nPoint:0,j,k)
    end do; end do
    call close_file
    BoundaryThreads_B(iBlock) % iAction = Enthalpy_
    call test_stop(NameSub, DoTest, iBlock)

  end subroutine read_thread_restart
  !============================================================================
  subroutine save_threads_for_plot

    ! For each block near the inner boundary save the density and temperature
    ! in the ghost grid extended toward the photosphere level

    use BATL_lib, ONLY: nBlock, Unused_B, &
         CoordMin_DB, CellSize_DB, r_
    use ModInterpolate, ONLY: linear
    integer :: i, j, k, iBlock, nPoint
    real    :: State_VI(PSi_:TiSi_, 1-nPointThreadMax:0), Coord1
    logical :: DoTest
    character(len=*), parameter:: NameSub = 'save_threads_for_plot'
    !--------------------------------------------------------------------------
    if(UseTriangulation)then
       call triangulate_thread_for_plot
       RETURN
    end if
    call test_start(NameSub, DoTest)
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE
       if(.not.IsAllocatedThread_B(iBlock))CYCLE
       do k = kMin_, kMax_ 
          do j = jMin_, jMax_
             nPoint = BoundaryThreads_B(iBlock) % nPoint_II(j,k)
             !\
             ! Fill in an array with NeSi and TeSi values
             State_VI(PSi_, 1 - nPoint:0) = &
                  BoundaryThreads_B(iBlock) % State_VIII(PSi_,1-nPoint:0,j,k)
             !\
             !  Use geometric average of the face value for 
             !  the wave amplitude to get the cell-centered aplitude squared
             !/
             State_VI(A2Major_, 1 - nPoint:0) = &
                  BoundaryThreads_B(iBlock) % State_VIII(A2Major_,1-nPoint:0,j,k) *&
                  BoundaryThreads_B(iBlock) % State_VIII(A2Major_,-nPoint:-1,j,k)
             State_VI(A2Minor_, 1 - nPoint:0) = &
                  BoundaryThreads_B(iBlock) % State_VIII(A2Minor_,1-nPoint:0,j,k) *&
                  BoundaryThreads_B(iBlock) % State_VIII(A2Minor_,-nPoint:-1,j,k)
             State_VI(TeSi_, 1 - nPoint:0) = &
                  BoundaryThreads_B(iBlock) % State_VIII(TeSi_,1-nPoint:0,j,k)
             if(UseElectronPressure)then
                State_VI(TiSi_, 1 - nPoint:0) = &
                     BoundaryThreads_B(iBlock) % State_VIII(TiSi_,1-nPoint:0,j,k)
             end if
             
             do i = BoundaryThreads_B(iBlock) % iMin, iMax
                !\
                ! Generalized radial coordinate of the grid point
                !/
                Coord1 = CoordMin_DB(r_, iBlock) + &
                     (real(i) + Coord1Norm0)/&
                     BoundaryThreads_B(iBlock) % dCoord1Inv
                !\
                ! interpolate Te and Ne to the ghost cell center:
                !/ 
                BoundaryThreads_B(iBlock) % State_VG(:, i, j, k)  = &
                     linear(&
                     a_VI = State_VI(:, 1 - nPoint:0),            &
                     nVar = TiSi_,                                &
                     iMin = 1 - nPoint,                           &
                     iMax = 0,                                    &
                     x = Coord1,                                  &
                     x_I = BoundaryThreads_B(iBlock) % Coord_DIII(r_,&
                     1 - nPoint:0, j, k),                         &
                     DoExtrapolate = .false.)
             end do
          end do
       end do
    end do
    
  end subroutine save_threads_for_plot
  !============================================================================
  subroutine interpolate_thread_state(Coord_D, iBlock, State_V)
    use ModAdvance,     ONLY: nVar
    use BATL_lib,       ONLY: MinIJK_D, MaxIJK_D, &
         CoordMin_DB, CellSize_DB, r_
    use ModInterpolate, ONLY: interpolate_vector
    !\
    !Coords of the point in which to interpolate
    !/
    real,    intent(in) :: Coord_D(3) 
    ! Block at which the grid is allocated
    integer, intent(in) :: iBlock
    !Interpolated state vector
    real,    intent(out):: State_V(nVar)
    real                :: StateThread_V(PSi_:TiSi_), CoordNorm_D(3)
    character(len=*), parameter:: NameSub = 'interpolate_thread_state'
    !-------------------------------------------------------------------------
    CoordNorm_D(r_+1:) = (Coord_D(r_+1:) - CoordMin_DB(r_+1:,iBlock))/&
         CellSize_DB(r_+1:,iBlock) + 0.50


    if(IsUniformGrid.and.(Coord_D(r_) > CoordMin_DB(r_,iBlock)))then
       !\
       ! The point is in between the uniform grid and the first layer pf
       ! physical cells, the width of this gap being the half cell size
       !/
       CoordNorm_D(r_) = (Coord_D(r_) - CoordMin_DB(r_,iBlock))*2/&
            CellSize_DB(r_,iBlock)
       ! Interpolate the state on threads to the given location
       StateThread_V = interpolate_vector(                        &
            a_VC=BoundaryThreads_B(iBlock)%State_VG(:,iMax:1,:,:),&
            nVar=TiSi_,                                           &
            nDim=3,                                               &
            Min_D=[iMax, jMin_, kMin_],                           &
            Max_D=[1, jMax_, kMax_],                              &
            x_D=CoordNorm_D,                                      &
            DoExtrapolate=.false.                                 )
    else
       ! Along radial coordinate the resolution and location of the grid
       ! may be different for uniform or non-uniform grid
       CoordNorm_D(r_) = (Coord_D(r_) - CoordMin_DB(r_,iBlock))*&
            BoundaryThreads_B(iBlock) % dCoord1Inv - Coord1Norm0
       ! Interpolate the state on threads to the given location
       StateThread_V = interpolate_vector(                        &
            a_VC=BoundaryThreads_B(iBlock)%State_VG(:,            &
            BoundaryThreads_B(iBlock)%iMin:iMax,:,:),             &
            nVar=TiSi_,                                           &
            nDim=3,                                               &
            Min_D=[BoundaryThreads_B(iBlock)%iMin, jMin_, kMin_], &
            Max_D=[iMax, jMax_, kMax_],                           &
            x_D=CoordNorm_D,                                      &
            DoExtrapolate=.false.                                 )
    end if
    call state_thread_to_mhd(StateThread_V, State_V)
  end subroutine interpolate_thread_state
  !============================================================================
  subroutine state_thread_to_mhd(StateThread_V, State_V)
    use ModAdvance,     ONLY: nVar, Rho_, WaveFirst_, WaveLast_
    use ModPhysics,  ONLY: Si2No_V, UnitTemperature_, UnitEnergyDens_
    !INPUT:
    real,    intent(in) :: StateThread_V(PSi_:TiSi_)
    !MHD state vector
    real,    intent(out):: State_V(nVar)
    !\
    ! Dimensionless plasma parameters
    !/
    real :: pTotal, Te, Ti
    character(len=*), parameter:: NameSub = 'state_thread_to_mhd'
    !Nullify momentum and field components of the state vector
    State_V = 0.0
    !Transform Si parameters to diminsionless ones:
    pTotal = StateThread_V(PSi_) *Si2No_V(UnitEnergyDens_ )
    Te     = StateThread_V(TeSi_)*Si2No_V(UnitTemperature_)
    if(UseElectronPressure)then
       Ti  = StateThread_V(TiSi_)*Si2No_V(UnitTemperature_)
       !\
       ! Use the following equations
       ! Te = TeFraction*State_V(iP)/State_V(Rho_)
       ! Ti = TiFraction*State_V(p_)/State_V(Rho_)
       ! and State_V(iP) + State_V(p_) = pTotal
       !/
       State_V(Rho_) = pTotal/(Te/TeFraction + Ti/TiFraction)
       State_V(p_)   = Te/TeFraction*State_V(Rho_)
       State_V(iP)   = Ti/TiFraction*State_V(Rho_)
    else
       State_V(p_)   = pTotal
       ! Use equation
       ! Te = TeFraction*State_V(iP)/State_V(Rho_)
       State_V(Rho_) = TeFraction*pTotal/Te
    end if
    State_V(WaveFirst_) = StateThread_V(A2Major_)
    State_V(WaveLast_ ) = StateThread_V(A2Minor_)
  end subroutine state_thread_to_mhd
  !============================================================================
  subroutine state_mhd_to_thread(State_V, Xyz_D, B0_D, StateThread_V)
    use ModPhysics,        ONLY: No2Si_V, UnitTemperature_, &
         UnitEnergyDens_
    use ModVarIndexes,      ONLY: Rho_, p_, Pe_, Bx_, Bz_, nVar
    use ModWaves,           ONLY: WaveFirst_, WaveLast_
    use  ModCoronalHeating, ONLY:PoyntingFluxPerB
    !INPUT:
    !MHD state vector
    real,    intent(in) :: State_V(nVar)
    !Cartesian coords and B0 field to idenify major and minor waves
    real,    intent(in) :: Xyz_D(3), B0_D(3)
    !OUTPUT:
    !Thread state vector
    real,    intent(out):: StateThread_V(PSi_:TiSi_)
    !LOCALS:
    !Total magnetic field
    real :: BTotal_D(3)
    !-----------------------
    BTotal_D = State_V(Bx_:Bz_) + B0_D
    if(sum(BTotal_D*Xyz_D) <  0.0)then
       StateThread_V(A2Major_) = State_V(WaveLast_ )
       StateThread_V(A2Minor_) = State_V(WaveFirst_)
    else
       StateThread_V(A2Major_) = State_V(WaveFirst_)
       StateThread_V(A2Minor_) = State_V(WaveLast_ )
    end if
    StateThread_V(A2Major_:A2Minor_) = StateThread_V(A2Major_:A2Minor_)/&
            (sqrt(State_V(Rho_))* PoyntingFluxPerB)
    StateThread_V(TeSi_) = TeFraction*State_V(iP)/State_V(Rho_)         &
         *No2Si_V(UnitTemperature_)
    if(useElectronPressure)then
       StateThread_V(TiSi_) = TiFraction*State_V(p_)    &
               /State_V(Rho_)*No2Si_V(UnitTemperature_)
       StateThread_V(PSi_) = (State_V(p_)+State_V(Pe_)) &
            *No2Si_V(UnitEnergyDens_)
    else
       StateThread_V(PSi_) = State_V(p_)*No2Si_V(UnitEnergyDens_)
    end if
  end subroutine state_mhd_to_thread
  !============================================================================
  subroutine set_thread_plotvar(iBlock, nPlotVar, NamePlotVar_V, Xyz_D, & 
    State_V, PlotVar_V)
    use ModMain
    use EEE_ModCommonVariables, ONLY: UseCme
    use EEE_ModMain,            ONLY: EEE_get_state_BC
    use ModVarIndexes
    use ModAdvance, ONLY : time_BLK, UseElectronPressure, &
         UseMultiSpecies
    use ModGeometry
    use ModPhysics,       ONLY: BodyRho_I, BodyP_I, OmegaBody,  &
         ElectronPressureRatio, InvGammaMinus1_I, Si2No_V, UnitB_
    use ModUtilities,     ONLY: lower_case
    use ModIO,            ONLY: NameVarUserTec_I, NameUnitUserTec_I, &
         NameUnitUserIdl_I
    use ModNumConst,      ONLY: cTiny
    use ModMultiFluid,    ONLY: extract_fluid_name,      &
         UseMultiIon, nIonFluid, iPpar, iPFluid=>iP,   &
         IsMhd, iRho, iRhoUx, iRhoUy, iRhoUz, iRhoIon_I, &
         ChargeIon_I
    use ModCoordTransform, ONLY: cross_product
    use BATL_lib,          ONLY: iNode_B, CellSize_DB
    use ModB0,             ONLY: get_b0
    use ModWaves,          ONLY: UseWavePressure
    use ModCoronalHeating, ONLY: PoyntingFluxPerB

    integer, intent(in) :: iBlock, nPlotVar
    character(LEN=20)   :: NamePlotVar_V(nPlotVar)
    real,    intent(in) :: Xyz_D(3)
    real, intent(inout) :: State_V(nVar)
    real,   intent(out) :: PlotVar_V(nPlotVar)

    !\
    ! To calculate B0 and BFull, if needed
    !/
    real :: B0_D(3) = 0.0, FullB_D(3)
    character (len=10)  :: String, NamePlotVar

    real:: tmp1Var, tmp2Var
   

    integer :: iVar, jVar, iIon, iFluid
    !\
    ! CME parameters, if needed
    !/
    real:: RhoCme, Ucme_D(MaxDim), Bcme_D(MaxDim), pCme
    !\
    ! Conversion coefficients from the squared Alfven wave amplitude to
    ! energy densities
    !/
    real :: SignBr, I0, I1
    
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_plotvar'
    !-------------------
    call test_start(NameSub, DoTest, iBlock)
    !\
    ! Calculate B0 and BFull
    !/
    B0_D = 0.0
    if(UseB0)call get_b0(Xyz_D, B0_D)
    if(UseCME)then
       call EEE_get_state_BC(Xyz_D, RhoCme, Ucme_D, Bcme_D, pCme, &
            time_simulation, n_step, iteration_number)
       Bcme_D = Bcme_D*Si2No_V(UnitB_)
       B0_D = B0_D + Bcme_D
    end if
    FullB_D = State_V(Bx_:Bz_) + B0_D
    
    !\
    ! Convert the squared Alfven wave amplitudes to
    ! energy densities
    !/
    signBr = sign(1.0, sum(FullB_D*Xyz_D))
    if(signBr < 0.0)then
       !In this case WaveLast dominates over WaveFirst
       !Currently, in the state vector WaveFirst is dominant
       !Reorder this array
       I0 = State_V(WaveLast_); I1 = State_V(WaveFirst_)
       State_V(WaveFirst_) = I0; State_V(WaveLast_) = I1
    end if
    !Convert to the energy density
    State_V(WaveFirst_:WaveLast_) = State_V(WaveFirst_:WaveLast_)*&
         PoyntingFluxPerB*sqrt(State_V(Rho_))



    PlotVar_V = 0.0
    do iVar = 1, nPlotVar
       NamePlotVar = NamePlotVar_V(iVar)

       ! Default values for TecPlot variable name and TecPlot and IDL unit names
       NameVarUserTec_I(iVar)  = NamePlotVar
       NameUnitUserTec_I(iVar) = ' '
       NameUnitUserIdl_I(iVar) = '?'

       call lower_case(NamePlotVar)
       String = NamePlotVar
       call extract_fluid_name(String,iFluid)
       select case(String)

          ! Cartesian coordinates for non-Cartesian plots
       case('x')
          PlotVar_V(iVar) = Xyz_D(1)
       case('y')
          PlotVar_V(iVar) = Xyz_D(2)
       case('z')
          PlotVar_V(iVar) = Xyz_D(3)
       case('r')
          PlotVar_V(iVar) = norm2(Xyz_D)

          ! BASIC MHD variables
       case('rho')
          PlotVar_V(iVar) = State_V(iRho)
       case('rhoux','mx')
          if (UseRotatingFrame) then
             PlotVar_V(iVar) = State_V(iRhoUx) &
                  -State_V(iRho)*OmegaBody*Xyz_D(y_)
          else
             PlotVar_V(iVar) = State_V(iRhoUx)
          end if
       case('rhouy','my')
          if (UseRotatingFrame) then
                PlotVar_V(iVar) = State_V(iRhoUy) &
                     +State_V(iRho)*OmegaBody*Xyz_D(x_)
          else
             PlotVar_V(iVar) = State_V(iRhoUy)
          end if
       case('rhouz','mz')
          PlotVar_V(iVar) = State_V(iRhoUz)
       case('bx')
          PlotVar_V(iVar) = FullB_D(x_)
       case('by')
          PlotVar_V(iVar) = FullB_D(y_)
       case('bz')
          PlotVar_V(iVar) = FullB_D(z_)
       case('e')
          !Internal plus kinetic energy
          PlotVar_V(iVar) = InvGammaMinus1_I(iFluid)*State_V(iPFluid) + &
               0.50*sum(State_V(iRhoUx:iRhoUz)**2)/State_V(iRho)
          ! Add (B0+B1)^2  
          if(iFluid == 1 .and. IsMhd.and.UseB0) &
               PlotVar_V(iVar) = PlotVar_V(iVar) + 0.5*sum(FullB_D**2)
       case('p','pth')
          PlotVar_V(iVar) = State_V(iPFluid)
       case('n','t','temp')
          ! Calculate the number density
          if(UseMultiSpecies)then
             do jVar = SpeciesFirst_, SpeciesLast_
                PlotVar_V(iVar) = PlotVar_V(iVar) + &
                     State_V(jVar)/MassSpecies_V(jVar)
             end do
          else if(iFluid == 1 .and. UseMultiIon)then
             ! Add up ion number densities
             do iIon = 1, nIonFluid
                PlotVar_V(iVar) = PlotVar_V(iVar) + &
                     State_V(iRhoIon_I(iIon))/MassIon_I(iIon)
             end do
          else
             PlotVar_V(iVar) = State_V(iRho)/MassFluid_I(iFluid)
          end if

          ! Calculate temperature from P = n*k*T + ne*k*Te = n*k*T*(1+ne/n*Te/T)
          if(String /= 'n')then
             ! t = p/n
             PlotVar_V(iVar) = State_V(iPFluid) / PlotVar_V(iVar)

             !
             if(nFluid==1 .and. .not.UseElectronPressure &
                  .and. ElectronPressureRatio > 0.0) &
                  PlotVar_V(iVar) = PlotVar_V(iVar)&
                  /(1 + ElectronPressureRatio)
          end if
       case('te')
          !\
          ! Use the following equation
          ! Te = TeFraction*State_V(iP)/State_V(Rho_)
          !/
          PlotVar_V(iVar) = TeFraction*State_V(iP)/State_V(Rho_)
       case('ti')
          !\
          ! Use the following equation
          ! Ti = TiFraction*State_V(p_)/State_V(Rho_)
          !/
          PlotVar_V(iVar) = TiFraction*State_V(p_)/State_V(Rho_)
       case('ux')
          if (UseRotatingFrame) then
             PlotVar_V(iVar) = State_V(iRhoUx)/State_V(iRho) &
                     - OmegaBody*Xyz_D(y_)
          else
             PlotVar_V(iVar) = State_V(iRhoUx)/State_V(iRho)
          end if
       case('uy')
          if (UseRotatingFrame) then
             PlotVar_V(iVar) = State_V(iRhoUy)/State_V(iRho) &
                     + OmegaBody*Xyz_D(x_)
          else
             PlotVar_V(iVar) = State_V(iRhoUy) / State_V(iRho)
          end if
       case('uxrot')
          PlotVar_V(iVar) = &
               State_V(iRhoUx)/State_V(iRho)

       case('uyrot')
          PlotVar_V(iVar) = &
               State_V(iRhoUy) / State_V(iRho)
       case('uz','uzrot')
          PlotVar_V(iVar) = &
               State_V(iRhoUz) / State_V(iRho)
       case('b1x')
          PlotVar_V(iVar) = State_V(Bx_)
       case('b1y')
          PlotVar_V(iVar) = State_V(By_)
       case('b1z')
          PlotVar_V(iVar) = State_V(Bz_)
       case('pperp')
          PlotVar_V(iVar) = (3*State_V(iPFluid) &
               -State_V(iPpar))/2.0
       case('peperp')
          PlotVar_V(iVar) = (3*State_V(Pe_) &
               -State_V(Pepar_))/2.0

       case('pvecx')
          PlotVar_V(iVar) = ( &
               ( FullB_D(x_)**2 + FullB_D(y_)**2 + FullB_D(z_)**2) * &
               State_V(iRhoUx) &
               -(FullB_D(x_)*State_V(iRhoUx) + FullB_D(y_)*State_V(iRhoUy) + &
               FullB_D(z_)* State_V(iRhoUz))*FullB_D(x_) ) /State_V(iRho)
       case('pvecy')
          PlotVar_V(iVar) = ( &
               ( FullB_D(x_)**2 + FullB_D(y_)**2 + FullB_D(z_)**2) * &
               State_V(iRhoUy) &
               -(FullB_D(x_)*State_V(iRhoUx) + FullB_D(y_)*State_V(iRhoUy) + &
               FullB_D(z_)* State_V(iRhoUz))*FullB_D(y_) ) /State_V(iRho)
       case('pvecz')
          PlotVar_V(iVar) = ( &
               ( FullB_D(x_)**2 + FullB_D(y_)**2 + FullB_D(z_)**2) * &
               State_V(iRhoUz) &
               -(FullB_D(x_)*State_V(iRhoUx) + FullB_D(y_)*State_V(iRhoUy) + &
               FullB_D(z_)* State_V(iRhoUz))*FullB_D(z_) ) /State_V(iRho)

          ! Radial component variables

       case('ur')
          PlotVar_V(iVar) = sum(State_V(iRhoUx:iRhoUz)*Xyz_D) &
                  / (State_V(iRho)*norm2(Xyz_D))
       case('rhour','mr')
          PlotVar_V(iVar) = sum(State_V(iRhoUx:iRhoUz)*Xyz_D) &
                  / norm2(Xyz_D)
       case('br')
          PlotVar_V(iVar) = sum(FullB_D*Xyz_D) /norm2(Xyz_D)
       case('b1r')
          PlotVar_V(iVar) = sum(State_V(Bx_:Bz_)*Xyz_D)/norm2(Xyz_D)
       case('er')
          PlotVar_V(iVar) =  sum( &
               Xyz_D*cross_product(FullB_D, State_V(iRhoUx:iRhoUz))) &
               / (State_V(iRho)*norm2(Xyz_D))
       case('pvecr')
          tmp1Var = sum(FullB_D**2)
          tmp2Var = sum(FullB_D*State_V(iRhoUx:iRhoUz))
          PlotVar_V(iVar) = sum ( &
               Xyz_D*( tmp1Var*State_V(iRhoUx:iRhoUz) &
               -  tmp2Var*FullB_D ) &
               ) / (State_V(iRho)*norm2(Xyz_D))
       case('b2ur')
          PlotVar_V(iVar) = 0.5*sum(FullB_D(:)**2) &
               *sum( State_V(iRhoUx:iRhoUz)*Xyz_D &
               ) / (State_V(iRho)*norm2(Xyz_D))
       case('dx')
          PlotVar_V(iVar) = CellSize_DB(x_,iBlock)
       case('dy')
          PlotVar_V(iVar) = CellSize_DB(y_,iBlock)
       case('dz')
          PlotVar_V(iVar) = CellSize_DB(z_,iBlock)
       case('dtblk')
          PlotVar_V(iVar) = dt_BLK(iBlock)
       case('proc')
          PlotVar_V(iVar) = iProc
       case('blk','block')
          PlotVar_V(iVar) = iBlock
       case('node')
          PlotVar_V(iVar) = iNode_B(iBlock)

       case('ew','erad')
          if(Ew_ == 1)then
             if(UseWavePressure)PlotVar_V(iVar) = &
                  sum(State_V(WaveFirst_:WaveLast_))
          else
             PlotVar_V(iVar) = State_V(Ew_)
          end if

       case default
          ! Check if the name is one of the state variable names
          do jVar = 1, nVar
             if(NamePlotVar /= NameVarLower_V(jVar)) CYCLE
             PlotVar_V(iVar) = State_V(jVar)
             EXIT
          end do
       end select
    end do ! iVar
    call test_stop(NameSub, DoTest, iBlock)
  end subroutine set_thread_plotvar
  !=========================
  subroutine save_thread_restart
    use ModMain,       ONLY: NameThisComp
    use BATL_lib, ONLY: nBlock, Unused_B
    use ModIoUnit,     ONLY: UnitTmp_
    use ModUtilities,  ONLY: open_file, close_file
    integer :: j, k, iBlock, nPoint
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'save_thread_restart'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE
       if(.not.IsAllocatedThread_B(iBlock))CYCLE
       call get_restart_file_name(iBlock, NameThisComp//'/restartOUT/')
       call open_file(file=NameRestartFile, form='UNFORMATTED',&
            NameCaller=NameSub)
       do k = kMin_,kMax_; do j = jMin_, jMax_
          nPoint = BoundaryThreads_B(iBlock) % nPoint_II(j,k)
          write(UnitTmp_)real(nPoint)
          write(UnitTmp_)&
               BoundaryThreads_B(iBlock) % State_VIII(TeSi_,1-nPoint:0,j,k),&
               BoundaryThreads_B(iBlock) % State_VIII(TiSi_,1-nPoint:0,j,k),&
               BoundaryThreads_B(iBlock) % State_VIII(PSi_,1-nPoint:0,j,k)
       end do; end do
       call close_file
    end do
    call test_stop(NameSub, DoTest)
  end subroutine save_thread_restart
  !============================================================================
  subroutine get_restart_file_name(iBlock, NameRestartDir)
    use BATL_lib,      ONLY: iMortonNode_A, iNode_B

    integer, intent(in)          :: iBlock
    character(len=*), intent(in) :: NameRestartDir

    integer  :: iBlockRestart
    character:: StringDigit

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_restart_file_name'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    iBlockRestart = iMortonNode_A(iNode_B(iBlock))

    write(StringDigit,'(i1)') max(5,int(1+alog10(real(iBlockRestart))))

    write(NameRestartFile,'(a,i'//StringDigit//'.'//StringDigit//',a)')&
         trim(NameRestartDir)//'thread_',iBlockRestart,'.blk'
    call test_stop(NameSub, DoTest, iBlock)
  end subroutine get_restart_file_name
  !==========================ROUTINES USED FOR TRIANGULATION===================
  subroutine broadcast_buffer(nVar, Buff_VI)
    use ModMpi
    integer, intent(in) :: nVar
    real, intent(inout) :: Buff_VI(nVar, nThread)
    integer             :: iBuff, iProcBCast, iError
    !-----------------
    iBuff = 0
    do iProcBCast = 0, nProc-1
       if(nThread_P(iProcBCast)==0)CYCLE
       call MPI_BCAST(Buff_VI(:, iBuff+1:iBuff+nThread_P(iProcBCast)),&
            nVar*nThread_P(iProcBCast), MPI_REAL, iProcBCast, iComm, iError)
       iBuff = iBuff + nThread_P(iProcBCast)
    end do
  end subroutine broadcast_buffer
  !===========================================================================
  !\
  ! Calculate coordinates (lon, lat) of the intersection point of threads
  ! with the spherical surface at the first generalized coordinate value 
  ! equal to input Coord1
  !/
  subroutine get_thread_point(Coord1, State_VI)
    use BATL_lib, ONLY: nBlock, Unused_B, &
         CoordMin_DB, CellSize_DB, r_
    use ModInterpolate, ONLY: linear
    use ModNumConst, ONLY: cHalfPi
    real,  intent(in) :: Coord1
    real, intent(out) :: State_VI(2+TiSi_, nThread)
    integer :: i, j, k, iBlock, nPoint, iBuff
    integer, parameter:: Lon_ = 1, Lat_ = 2
    real    :: StateThread_VI(2+TiSi_, 1-nPointThreadMax:0)
    logical :: DoTest
    character(len=*), parameter:: NameSub = 'get_thread_coord'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    !Start value for Buffer index numerating the points related to given PE
    if(iProc==0)then
       iBuff = 0
    else
       iBuff = sum(nThread_P(0:iProc-1))
    end if
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE
       if(.not.IsAllocatedThread_B(iBlock))CYCLE
       do k = 1, nK
          do j = 1, nJ
             iBuff = iBuff + 1
             nPoint = BoundaryThreads_B(iBlock) % nPoint_II(j,k)
             !\
             ! Fill in an array for this thread with lon,lat values
             ! of the grid points on the thread 
             StateThread_VI(Lon_:Lat_, 1 - nPoint:0) = &
                  BoundaryThreads_B(iBlock) % Coord_DIII(2:,1-nPoint:0,j,k)
             !\
             ! Fill in an array with NeSi and TeSi values
             StateThread_VI(2+PSi_:2+TiSi_, 1 - nPoint:0) = &
                  BoundaryThreads_B(iBlock) % State_VIII(:,1-nPoint:0,j,k)
             !\
             !  Use geometric average of the face value for 
             !  the wave amplitude to get the cell-centered aplitude squared
             !/
             StateThread_VI(2+A2Major_, 1 - nPoint:0) = &
                  StateThread_VI(2+AMajor_, 1 - nPoint:0)*&
                  BoundaryThreads_B(iBlock) % State_VIII(AMajor_,-nPoint:-1,j,k)
             StateThread_VI(2+A2Minor_, 1 - nPoint:0) = &
                  StateThread_VI(2+AMinor_, 1 - nPoint:0)*&
                  BoundaryThreads_B(iBlock) % State_VIII(AMinor_,-nPoint:-1,j,k)
             !\
             ! Now we find the intersection point of the thread with the
             ! spherical surface at given radial gen coordinate, Coord1
             ! and interpolate the state vector to this point
             !/ 
             State_VI(:,iBuff)  = &
                     linear(&
                     a_VI =  StateThread_VI(:, 1 - nPoint:0),     &
                     nVar = Lat_ + TiSi_,                         &
                     iMin = 1 - nPoint,                           &
                     iMax = 0,                                    &
                     x = Coord1,                                  &
                     x_I = BoundaryThreads_B(iBlock) % Coord_DIII(r_,&
                     1 - nPoint:0, j, k),                         &
                     DoExtrapolate = .false.)
          end do
       end do
    end do
    call broadcast_buffer(nVar=Lat_+TiSi_, Buff_VI=State_VI)
    call test_stop(NameSub, DoTest)
  end subroutine get_thread_point
  !====================================================================
  subroutine triangulate_thread_for_plot
    use BATL_lib,               ONLY: nBlock, Unused_B, &
         CoordMin_DB, CellSize_DB, x_, y_, z_, Xyz_DGB
    use ModB0,                  ONLY: B0_DGB
    use ModAdvance,             ONLY: State_VGB
    use ModTriangulateSpherical,ONLY:trans, trmesh, find_triangle_sph, &
         find_triangle_orig, fix_state
    use ModCoordTransform,      ONLY: rlonlat_to_xyz
    use ModConst,               ONLY: cTiny, cRadToDeg, cPi
    real    :: Coord1
    integer :: i, j, k, iBlock, nPoint
    !\
    !Coordinates and state vector at the point of intersection of thread with
    !the spherical coordinate  surface of the grid for plotting 
    !/
    integer, parameter :: Lon_ = 1, Lat_=2
    real    :: State_VI(Lat_+TiSi_,nThread+2), State_V(TiSi_), &
         Coord_D(Lon_:Lat_)
    real    :: Xyz_DI(3,nThread+2), Xyz_D(3)
    !\
    !Data for interpolation: stencil and weights
    !/
    real    :: Weight_I(3)
    integer :: iStencil_I(3), iError
    !\
    ! Triangulation data
    !/
    integer, allocatable :: list(:), lptr(:), lend(:)
    !\
    ! Local Ligicals
    !/
    logical :: DoTest, IsTriangleFound = .false.
    !\
    !Add two triangulation nodes at the poles:
    !/
    real, parameter :: North_D(3)   = [0.0, 0.0, +1.0]
    real, parameter :: South_D(3)   = [0.0, 0.0, -1.0]
    character(len=*), parameter:: NameSub = 'save_thread_for_plot'
    !-------------------------
    if(.not.IsUniformGrid)call stop_mpi(&
         'Triangulation does dot work for non-uniform grid in threaded gap')
    call test_start(NameSub, DoTest)
    allocate(list(6*nThread), lptr(6*nThread), lend(nThread+2))
    do i = -nGUniform, 0
       !\
       ! Generalized radial coordinate of the grid points
       !  This is the SC low boundary
       !/           V
       Coord1 = Coord1TopThread + real(i)*dCoord1Uniform
       
       call get_thread_point(Coord1, State_VI(:,2:nThread+1))
       !Convert lon and lat to Cartesian coordinates on a unit sphere
 
       call trans( n=nThread,&
            rlat=State_VI(2,2:nThread+1), &
            rlon=State_VI(1,2:nThread+1), &
            x= Xyz_DI(x_,2:nThread+1), &
            y= Xyz_DI(y_,2:nThread+1), &
            z=Xyz_DI(z_,2:nThread+1))
       !Add two grid nodes at the poles:
       Xyz_DI(:,1)             = South_D     ! = [0.0, 0.0, -1.0]
       Xyz_DI(:,    nThread+2) = North_D     ! = [0.0, 0.0, +1.0]
       !\
       ! Triangulate
       !/
       call trmesh(nThread+2, Xyz_DI(x_,:), Xyz_DI(y_,:), Xyz_DI(z_,:), &
            list, lptr, lend, iError)
       if(iError/=0)call stop_mpi(NameSub//': Triangilation failed')
       !Fix states at the polar nodes:
       !North:
       call fix_state(iNodeToFix =         1,&
                      nNode      = nThread+2,&
                      iList_I    =      list,&
                      iPointer_I =      lptr,&
                      iEnd_I     =      lend,&
                      Xyz_DI     =    Xyz_DI,&
                      nVar       =     TiSi_,&
                      State_VI   = State_VI(3:,:))
       !South:
       call fix_state(iNodeToFix = nThread+2,&
                      nNode      = nThread+2,&
                      iList_I    =      list,&
                      iPointer_I =      lptr,&
                      iEnd_I     =      lend,&
                      Xyz_DI     =    Xyz_DI,&
                      nVar       =     TiSi_,&
                      State_VI   = State_VI(3:,:))      
       !\
       ! Now, interpolate state vector to the points of a grid used for 
       ! plotting
       !/
       do iBlock = 1, nBlock
          if(Unused_B(iBlock))CYCLE
          if(.not.IsAllocatedThread_B(iBlock))CYCLE
          do k = kMin_, kMax_; do j = jMin_, jMax_
             Coord_D = CoordMin_DB(2:,iBlock) + &
                  CellSize_DB(2:, iBlock)*[j - 0.50, k - 0.50]
             !\
             !Transform longitude and latitude to the unit vector
             !/
             call rlonlat_to_xyz(1.0, Coord_D(Lon_), Coord_D(Lat_),&
                  Xyz_D)
             !\
             ! Find a triange into which this vector falls and the 
             ! interpolation weights
             !/
             if(UseInterpolationOrig)then
                call find_triangle_orig(Xyz_D, nThread+2, Xyz_DI ,&
                     list, lptr, lend,                            &
                     Weight_I, IsTriangleFound, iStencil_I)
             else
                call find_triangle_sph( Xyz_D, nThread+2, Xyz_DI ,&
                     list, lptr, lend,                            &
                     Weight_I(1), Weight_I(2), Weight_I(3),       &
                     IsTriangleFound,                             &
                     iStencil_I(1), iStencil_I(2), iStencil_I(3)) 
             end if
             if(.not.IsTriangleFound)then
                write(*,*)'At the location x,y,z=', Xyz_D
                call stop_mpi('Interpolation on triangulated sphere fails')
             end if
             !\
             ! interpolate  state vector to a grid point of a uniform grid
             !/ 
             BoundaryThreads_B(iBlock) % State_VG(:, i, j, k)  = &
                  State_VI(3:, iStencil_I(1))*Weight_I(1) + &
                  State_VI(3:, iStencil_I(2))*Weight_I(2) + &
                  State_VI(3:, iStencil_I(3))*Weight_I(3)
          end do; end do    !j,k
       end do       !iBlock
    end do          !i
    deallocate(list, lptr, lend)
    !\
    ! One extra layer passing through physical cell centers (i=1)
    !/
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE
       if(.not.IsAllocatedThread_B(iBlock))CYCLE
       do k = kMin_, kMax_; do j = jMin_, jMax_
          call state_mhd_to_thread(&
               State_V       = State_VGB(:, 1, j, k, iBlock), &
               Xyz_D         = Xyz_DGB(  :, 1, j, k, iBlock), &
               B0_D          = B0_DGB(   :, 1, j, k, iBlock), &
               StateThread_V = &
               BoundaryThreads_B(iBlock) % State_VG(:, 1, j, k))
       end do; end do
    end do
    call test_stop(NameSub, DoTest)
  end subroutine triangulate_thread_for_plot
  !=========================================
  subroutine get_tr_los_image(Xyz_D, DirLos_D, iBlock, nPlotVar, NamePlotVar_V,&
       iTableEuv, iTableSxr, iTableGen, PixIntensity_V)
    !\
    ! Using a tabulated analytical solution for the Transition Region (TR),
    ! the contribution to the LOS image from the TR is calculated.
    !/
    !\
    !INPUTS:
    !\
    !\
    ! An intersection point of the LOS with the chromosphere top:
    !/
    real, intent(in)    :: Xyz_D(3)
    !\
    ! A unit vector in direction of the LOS:
    !/
    real, intent(in)    :: DirLos_D(3)
    !(Extended) block ID
    integer, intent(in) :: iBlock
    !\
    !Number of the variables to plot:
    !/
    integer, intent(in) :: nPlotVar
    !\
    !Names of the variables to plot
    !/
    character(len=20), intent(in) :: NamePlotVar_V(nPlotVar)
    !\
    ! Tables, in which the emissivity in different ion lines 
    ! is tabulated:
    !/
    integer, intent(in) :: iTableEuv, iTableSxr, iTableGen
    !\
    ! Logicals specifying the use of tables
    !/
    logical :: UseEuv, UseSxr, UseGenTable
    !\
    ! Pixel intensity to be corrected
    !/
    real, intent(inout) :: PixIntensity_V(nPlotVar)
    !/
    !LOCALS:
    !/
    !\
    ! Magnetic field at the point XYZ_D, and its direction vector:
    !/
    real :: B0_D(3), DirB_D(3)
    !\
    ! Radial direction:
    !/
    real :: DirR_D(3)
    !\
    ! cosine of angles between the radial direction and the directions of
    ! LOS and magnetic field:
    !/
    real :: CosRLos, CosRB
    ! Euv intensities:
    real :: EuvValue_V(3)
    ! Sxr intensities:
    real :: SxrValue_V(2)
    ! Plasma parameters at the top of TR
    real :: TeSi, PTotSi, PeSi
    !Contribution to the image
    real :: PlotVar_V(nPlotVar)
    !-----------------------------------------------
    !\
    ! Radial direction:
    !/
    DirR_D = Xyz_D/norm2(Xyz_D)
    !\
    ! Cosine of angle between DirR and DirLos:
    !/
    CosRLos = abs(sum(DirR_D*DirLos_D))
    !\
    ! If the line is tangent to the solar surface, the intensity is too large
    ! to be corrected
    !/
    if(CosRLos <= 0.01)RETURN
    !\
    ! Magnetic field vector and its angle with the radial direction:
    !/
    call get_b0(Xyz_D, B0_D)
    DirB_D = B0_D/norm2(B0_D)
    CosRB = abs(sum(DirR_D*DirB_D))
    UseEuv = iTableEuv > 0
    UseSxr = iTableSxr > 0
    UseGenTable = iTableGen > 0
    call get_te_ptot(TeSi, PTotSi)
    !\
    ! Assiming equal electron and ion temperature
    ! Pe = Z Pi, pTotal = (Z+1)Pi
    ! So that Pi = pTotal/(Z+1) and
    PeSi = Z*PTotSi/(1 + Z)
    
    
    PlotVar_V = 0
    !Integrate plot variables
    call set_plot_var
    !Add contribution to the pixel intensity, account for
    !the geometric factor
    PixIntensity_V = PixIntensity_V + PlotVar_V *(CosRB/CosRLos)
  contains
    !=========================================
    subroutine get_te_ptot(TeSi, PTotSi)
      use BATL_lib,       ONLY: xyz_to_coord, CellSize_DB, CoordMin_DB, r_
      use ModInterpolate, ONLY: interpolate_vector
      !\
      ! OUTPUT:
      !/
      real, intent(out) :: TeSi, PTotSi
      !\
      !Coords of the point in which to interpolate
      !/
      real :: Coord_D(3), CoordNorm_D(2)
      real :: StateThread_V(PSi_:TiSi_)
      !----------------------------------------------------------------
      call xyz_to_coord(Xyz_D, Coord_D)
      CoordNorm_D = (Coord_D(r_+1:) - CoordMin_DB(r_+1:,iBlock))/&
           CellSize_DB(r_+1:,iBlock) + 0.50

      ! Along radial coordinate the resolution and location of the grid
      ! may be different:
      
      ! Interpolate the state on threads to the given location
      StateThread_V = interpolate_vector(                                 &
           a_VC=BoundaryThreads_B(iBlock)%State_VG(:,                     &
           BoundaryThreads_B(iBlock)%iMin,:,:),                           &
           nVar=TiSi_,                                                    &
           nDim=2,                                                        &
           Min_D=[jMin_, kMin_],                                          &
           Max_D=[jMax_, kMax_],                                          &
           x_D=CoordNorm_D)
      PTotSi = StateThread_V( PSi_)
      TeSi   = StateThread_V(TeSi_)
    end subroutine get_te_ptot
    !=============================
    subroutine set_plot_var
      use ModConst, ONLY: cBoltzmann
      integer ::  i, iVar !Loop variables
      !Electron density in particles per cm3:
      real    :: NeCgs
      !------------------------
      if(UseGenTable)then
         call integrate_emission(TeSi, PeSi, iTableGen, nPlotVar, &
              PlotVar_V)
      else
         if (UseEuv) then
            ! now integrate EUV response values from a lookup table
            call integrate_emission(TeSi, PeSi, iTableEuv, 3, &
                 EuvValue_V)
         end if
      
         if (UseSxr) then
            ! now integrate SXR response values from a lookup table
            call integrate_emission(TeSi, PeSi, iTableSxr, 2, &
                 SxrValue_V)
         end if
         do iVar = 1, nPlotVar
            select case(NamePlotVar_V(iVar))
            case('euv171')
               ! EUV 171
               PlotVar_V(iVar) = EuvValue_V(1)
               
            case('euv195')
               ! EUV 195
               PlotVar_V(iVar) = EuvValue_V(2)
               
            case('euv284')
               ! EUV 284
               PlotVar_V(iVar) = EuvValue_V(3)
               
            case('sxr')
               ! Soft X-Ray (Only one channel for now, can add others later)
               PlotVar_V(iVar) = SxrValue_V(1)
            case default
               call stop_mpi('Unknown NamePlotVar='//NamePlotVar_V(iVar))
            end select
         end do
      end if
    end subroutine set_plot_var
    !================================
  end subroutine get_tr_los_image
end module ModFieldLineThread
!==============================================================================
