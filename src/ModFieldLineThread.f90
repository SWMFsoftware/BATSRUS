!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModFieldLineThread

  use ModBatsrusUtility, ONLY: stop_mpi
#ifdef _OPENACC
  use ModUtilities, ONLY: norm2
#endif
  use BATL_lib, ONLY: &
       test_start, test_stop, jTest, kTest, iBlockTest, &
       iProc, nProc, iComm, nJ, nK, jDim_, kDim_, MaxBlock, CoordMin_D, r_
  use ModAdvance, ONLY: UseElectronPressure
  use ModMain, ONLY: UseFieldLineThreads, DoThreads_B
  use ModB0, ONLY: get_b0
  use ModPhysics, ONLY: Z => AverageIonCharge
  use ModVarIndexes, ONLY: Pe_, p_, nVar
  use ModMultiFluid, ONLY: MassIon_I
  use ModTransitionRegion, ONLY: nPointThreadMax=>nPointMax,           &
       DsThreadMin=>Ds0, rChromo, PavrL_, TrTable_V, &
       HeatFluxL_, iTableTr, integrate_emission

  implicit none
  SAVE

  PRIVATE ! Except
  ! Chromosphere top boundary
  public :: rChromo

  logical, public, allocatable:: IsAllocatedThread_B(:)

  ! In the boundary blocks the physical cells near the boundary are connected
  ! to the photosphere with these threads
  type BoundaryThreads
     ! The thread length, \int{ds}, from the photoshere to the given point
     ! Renamed and revised: in meters
     real,pointer :: LengthSi_III(:,:,:)

     ! Renamed and revised: only the value BDsFaceInv_III(0,j,k) keeps this
     ! sense and used for calculating TMax_II(j,k). The values of
     ! BDsFaceInv_III(negative iPoint,j,k) are equal to
     ! ds[m]/(B[T]*PoyntingFluxPerBSi). Are used to calculate
     ! the dimensionless heat flux as
     ! (Cons_I(iPoint)-Cons_I(iPoint+1))*BDsFaceInv
     real,pointer :: BDsFaceInvSi_III(:,:,:)
     ! Dimensionless TMax, such that the ingoing heat flux to the TR at this
     ! temperature equals the Poynting flux (which is not realistic and means
     ! that the input temperature exceeding TMax assumes that something is
     ! going wrong.
     real,pointer :: TMax_II(:,:)

     ! Ds[m]/(B[T]*PoyntingFluxPerBSi)
     ! By multiplying this by 2*(3/2Pe) we obtain the internal energy in the
     ! interval between two grid points, in seconds. The physical meaning is
     ! how soon a plasma may be heated by this internal energy in the cell
     ! volume with having a BC for the Poynting flux at the left boundary
     ! and zero flux at the right boundary: In Si system
     real,pointer :: DsCellOverBSi_III(:,:,:)

     ! The integral, sqrt(PoyntingFluxPerB/LPerp**2)*(1/sqrt(B)) ds
     ! Dimensionless.
     real,pointer :: Xi_III(:,:,:)
     ! Magnetic field intensity, the inverse of heliocentric distance,
     ! and the gen coords
     ! Dimensionless.
     real,pointer :: B_III(:,:,:),RInv_III(:,:,:), Coord_DIII(:,:,:,:)
     ! The use:
     ! PeSi_I(iPoint) = PeSi_I(iPoint-1)*exp(-TGrav_III(iPoint)/TeSi_I(iPoint))
     real, pointer:: TGrav_III(:,:,:)
     ! The type of last update for the thread solution
     integer :: iAction

     !  Thread solution: temperatures and pressure SI and amplitudes of waves
     !  First index - enumerates state variable
     !  Second index - number of point along a thread
     !  Third and Fourth - enumerate j and k index of threads in the block
     real, pointer :: State_VIII(:,:,:,:)

     ! number of points
     integer,pointer :: nPoint_II(:,:)
     ! Distance between the true and ghost cell centers.
     real, pointer :: DeltaR_II(:,:)
     real, pointer :: SignB_II(:,:)
     integer, pointer :: iStencil_III(:,:,:)
     real, pointer :: Weight_III(:,:,:)
     real, pointer :: DsSi_III(:,:,:)
     ! PSi, TeSi amd TiSi stored at gird iMin:iMax,0:nJ+1,0:nK+1
     real, pointer :: State_VG(:,:,:,:)
  end type BoundaryThreads
  ! For visualization:
  ! Indexes for array State_VG(PSi_:TiSi_,-nGUniform:1,jMin_:jMax_,kMin_:kMax_)
  integer, parameter:: jMin_ = 1 - jDim_, jMax_ = nJ + jDim_
  integer, parameter:: kMin_ = 1 - kDim_, kMax_ = nK + kDim_
  ! Conponenets of array stored at each thread for visualization
  integer, public, parameter :: PSi_=1, A2Major_ = 2, AMajor_ = 2, &
       A2Minor_ = 3 , AMinor_ = 3, TeSi_=4, TiSi_ = TeSi_ + min(1, Pe_-1)

  ! To espress Te  and Ti in terms of P and rho, for ideal EOS:
  ! Te = TeFraction*State_V(iPe)/State_V(Rho_)
  ! Pe = PeFraction*State_V(iPe)
  ! Ti = TiFraction*State_V(p_)/State_V(Rho_)
  real, public    :: TeFraction, TiFraction, PeFraction
  integer, public :: iPe
  ! CoefXi/sqrt(VAlfven[NoDim]) is dXi/ds[NoDim]
  ! While setting thread,  we use only the
  ! thread-dependent factor in DXi, namely,
  ! ds*CoefXi. In ModThreadedLC DXi will be divided
  ! by sqrt(vAlfven)
  real,    public :: CoefXi

  type(BoundaryThreads), public, allocatable :: BoundaryThreads_B(:)

  public :: nPointThreadMax

  real, parameter:: TeGlobalMaxSi = 1.80e7

  public:: UseFieldLineThreads

  logical, public :: DoPlotThreads = .false.

  ! Interolate with planar vs spherical triangles
  logical         :: UsePlanarTriangles = .false.

  ! Add contribution from the transition region to the LOS plots
  logical, public :: UseTRCorrection = .true.

  ! Number of all threads, originating from physical cells
  integer :: nThreadAll

  ! Number of GhostCells in a uniform grid covering a threaded gap
  integer :: nGUniform = 10

  real,    allocatable :: Weight_III(:,:,:), Xyz_DII(:,:,:)
  integer, allocatable :: iStencil_III(:,:,:), iList_II(:,:),    &
         iPointer_II(:,:), iEnd_II(:,:)

  ! Inverse mesh size for the grid covering threaded gap
  real, public :: dCoord1Inv = -1.0

  ! Public members
  public :: BoundaryThreads
  public :: init_threads      ! Initializes module
  public :: read_thread_param         ! Read parameters of threads
  public :: deallocate_thread_b
  public :: set_threads       ! (Re)Sets threads in the inner boundary blocks

  ! Called prior to different invokes of set_cell_boundary, to determine
  ! how the solution on the thread should be (or not be) advanced:
  ! after hydro stage or after the heat conduction stage etc
  public :: advance_threads
  public :: advance_threaded_block_expl
  ! Correspondent named indexes
  integer, public, parameter :: DoInit_=-1, Done_=0, Enthalpy_=1, Heat_=2, &
       Restart_=3
  public :: save_threads_for_plot     ! Get  State_VG array
  public :: interpolate_thread_state  ! Interpolate state from State_VG
  public :: set_thread_plotvar        ! Plot variables for "shell" plots
  public :: get_tr_los_image          ! Correction for TR on LOS images
  public :: is_threaded_block         ! Mark blocks near internal boundary
  public :: beta_thread               ! Accounts for grid sizes in TR and SC
  ! Saves thread state into restart
  public :: save_thread_restart
  public :: save_plot_thread

  ! Visualization, log vars
  public :: set_ur_bot_sc, set_ur_top_tr, set_u_top_tr, set_u_bot_tr
  public :: set_u_min_tr, set_u_max_tr
  ! interface procedure to easy calculate the CME field
  public :: b_cme_d

  ! The number of grid spaces which are covered by the TR model
  ! the smaller is this number, the better the TR assumption work
  ! However, 1 is not recommended, as long as the length of the
  ! last interval is not controlled (may be too small)
  integer, parameter:: nIntervalTR = 2

  ! Number of threads on each processor
  integer, allocatable :: nThread_P(:)

  !   Hydrostatic equilibrium in an isothermal corona:
  !    d(N_i*k_B*(Z*T_e +T_i) )/dr=G*M_sun*N_I*M_i*d(1/r)/dr
  ! => N_i*Te\propto exp(cGravPot/TeSi*(M_i[amu]/(1+Z))*\Delta(R_sun/r))
  ! The plasma properties dependent coefficient needed to evaluate the
  ! eefect of gravity on the hydrostatic equilibrium
  real, public :: GravHydroStat != cGravPot*MassIon_I(1)/(AverageIonCharge + 1)
  logical :: IsInitialized = .false.

  character(len=100) :: NameRestartFile

contains
  !============================================================================
  subroutine init_threads

    use ModTurbulence, ONLY: PoyntingFluxPerB, &
         LPerpTimesSqrtB
    use BATL_lib, ONLY: MaxDim, xyz_to_coord, coord_to_xyz
    integer :: iBlock ! Loop variable
    real :: CoordChromo_D(MaxDim), Xyz_D(MaxDim)
    !--------------------------------------------------------------------------
    if(IsInitialized)RETURN
    IsInitialized = .true.
    if(DoPlotThreads)then
       ! Gen coords for the low corona boundary
       call coord_to_xyz(CoordMin_D, Xyz_D)
       ! Projection onto the photosphere level
       Xyz_D = Xyz_D/norm2(Xyz_D)*rChromo
       ! Generalized coords of the latter.
       call xyz_to_coord(Xyz_D, CoordChromo_D)
       ! Inverse of the  mesh of the gap-covering grid uniform in gen coords
       dCoord1Inv = nGUniform/(CoordMin_D(r_)  - CoordChromo_D(r_))
    end if
    ! TeFraction is used for ideal EOS:
    if(UseElectronPressure)then
       ! Pe = ne*Te (dimensionless) and n=rho/ionmass
       ! so that Pe = ne/n *n*Te = (ne/n)*(rho/ionmass)*Te
       ! TeFraction is defined such that Te = Pe/rho * TeFraction
       TeFraction = MassIon_I(1)/Z
       ! Pi = n*Te (dimensionless) and n=rho/ionmass
       ! so that Pi = (rho/ionmass)*Ti
       ! TiFraction is defined such that Ti = Pi/rho * TiFraction
       TiFraction = MassIon_I(1)
       iPe = Pe_
       PeFraction = 1.0
    else
       ! p = n*T + ne*Te (dimensionless) and n=rho/ionmass
       ! so that p=rho/massion *T*(1+ne/n Te/T)
       ! TeFraction is defined such that Te = p/rho * TeFraction
       TeFraction = MassIon_I(1)/(1 + Z)
       TiFraction = TeFraction
       iPe = p_
       PeFraction = Z/(1.0 + Z)
    end if
    ! Therefore Te = TeFraction*State_V(iPe)/State_V(Rho_)
    ! Pe = PeFraction*State_V(iPe)
    !
    CoefXi = PoyntingFluxPerB/LperpTimesSqrtB**2

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
  end subroutine init_threads
  !============================================================================
  logical function is_threaded_block(iBlock)

    use ModConst, ONLY: cTiny
    use BATL_lib, ONLY: CoordMin_D, CoordMin_DB
    integer, intent(in) :: iBlock
    !--------------------------------------------------------------------------
    is_threaded_block = &
         abs(CoordMin_D(1) - CoordMin_DB(1,iBlock)) < cTiny
  end function is_threaded_block
  !============================================================================
  real function beta_thread(j,k, iBlock)

    integer, intent(in) :: j,k, iBlock
    !--------------------------------------------------------------------------
    beta_thread = 2.0  ! TBD!
  end function beta_thread
  !============================================================================
  subroutine read_thread_param(NameCommand, iSession)

    use ModReadParam, ONLY: read_var
    character(len=*), intent(in):: NameCommand
    integer, intent(in):: iSession
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_thread_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(NameCommand)
    case("#FIELDLINETHREAD")
       call read_var('UseFieldLineThreads', UseFieldLineThreads)
       if(UseFieldLineThreads)then
          DoPlotThreads = .true.
          if(iSession/=1)call stop_mpi(&
               'UseFieldLineThreads can be set ON in the first session only')
          call read_var('nPointThreadMax', nPointThreadMax)
          call read_var('DsThreadMin', DsThreadMin)
       end if
       !$acc update device(UseFieldLineThreads)
    case('#PLOTTHREADS')
       call read_var('DoPlotThreads', DoPlotThreads)
       if(.not.DoPlotThreads)RETURN
       call read_var('nGUniform', nGUniform)
       ! If the non-uniform grid is used extending the existing block
       ! adaptive grid, the grid normally does not even reach the
       ! chromosphere height, so that the contribution from the TR is
       ! not worth while quantifying. With the uniform grid, this
       ! option is available.
       call read_var('UseTRCorrection', UseTRCorrection)
       ! When the triangulation is done, interpolation may be done with
       ! two ways to find the interpolation weights: via the areas of
       ! spherical triangles, or via areas of planar triangles on a plane
       ! latter is the "original" passing through three nodes of the
       ! interpolation stencil (the interpolation mechanism by Renka, who
       ! proved its good theretical properties, such as continuity of
       ! the interpolated valiable across the boundary of interpolation
       ! stencil). The "original" algorithm is applied if the following
       ! logical is set to .true.
       call read_var('UsePlanarTriangles', UsePlanarTriangles)
    case('#CHROMOEVAPORATION')
       if(iProc==0)write(*,*)&
            'Command #CHROMOEVAPORATION is not used in this TR model'
    case default
       call stop_mpi(NameSub//": unknown command="//trim(NameCommand))
    end select
    call test_stop(NameSub, DoTest)

  end subroutine read_thread_param
  !============================================================================
  subroutine nullify_thread_b(iBlock)

    integer, intent(in) :: iBlock
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'nullify_thread_b'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    nullify(BoundaryThreads_B(iBlock)%LengthSi_III)
    nullify(BoundaryThreads_B(iBlock)%BDsFaceInvSi_III)
    nullify(BoundaryThreads_B(iBlock)%DsCellOverBSi_III)
    nullify(BoundaryThreads_B(iBlock)%TMax_II)
    nullify(BoundaryThreads_B(iBlock)%Xi_III)
    nullify(BoundaryThreads_B(iBlock)%B_III)
    nullify(BoundaryThreads_B(iBlock)%RInv_III)
    nullify(BoundaryThreads_B(iBlock)%Coord_DIII)
    nullify(BoundaryThreads_B(iBlock)%TGrav_III)
    nullify(BoundaryThreads_B(iBlock)%State_VIII)
    nullify(BoundaryThreads_B(iBlock)%nPoint_II)
    nullify(BoundaryThreads_B(iBlock)%DeltaR_II)
    nullify(BoundaryThreads_B(iBlock)%SignB_II)
    nullify(BoundaryThreads_B(iBlock)%iStencil_III)
    nullify(BoundaryThreads_B(iBlock)%Weight_III)
    nullify(BoundaryThreads_B(iBlock)%State_VG)
    BoundaryThreads_B(iBlock)%iAction    = Done_
    call test_stop(NameSub, DoTest, iBlock)

  end subroutine nullify_thread_b
  !============================================================================
  subroutine deallocate_thread_b(iBlock)

    integer, intent(in) :: iBlock
    integer :: j, k
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'deallocate_thread_b'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    deallocate(BoundaryThreads_B(iBlock)%LengthSi_III)
    deallocate(BoundaryThreads_B(iBlock)%BDsFaceInvSi_III)
    deallocate(BoundaryThreads_B(iBlock)%DsCellOverBSi_III)
    deallocate(BoundaryThreads_B(iBlock)%TMax_II)
    deallocate(BoundaryThreads_B(iBlock)%Xi_III)
    deallocate(BoundaryThreads_B(iBlock)%B_III)
    deallocate(BoundaryThreads_B(iBlock)%RInv_III)
    deallocate(BoundaryThreads_B(iBlock)%Coord_DIII)
    deallocate(BoundaryThreads_B(iBlock)%TGrav_III)
    deallocate(BoundaryThreads_B(iBlock)%State_VIII)
    deallocate(BoundaryThreads_B(iBlock)%nPoint_II)
    deallocate(BoundaryThreads_B(iBlock)%DeltaR_II)
    deallocate(BoundaryThreads_B(iBlock)%SignB_II)
    deallocate(BoundaryThreads_B(iBlock)%iStencil_III)
    deallocate(BoundaryThreads_B(iBlock)%Weight_III)
    deallocate(BoundaryThreads_B(iBlock)%State_VG)
    IsAllocatedThread_B(iBlock) = .false.
    call nullify_thread_b(iBlock)
    call test_stop(NameSub, DoTest, iBlock)

  end subroutine deallocate_thread_b
  !============================================================================
  function b_cme_d(Xyz_D)
    use EEE_ModMain, ONLY: EEE_get_state_BC
    use ModPhysics, ONLY: Si2No_V, UnitB_
    use BATL_lib, ONLY: MaxDim
    use ModMain, ONLY: nStep, nIteration, tSimulation
    real :: b_cme_d(MaxDim)
    real, intent(in) ::Xyz_D(MaxDim)
    ! CME parameters, if needed
    real:: RhoCme, Ucme_D(MaxDim), Bcme_D(MaxDim), pCme
    !--------------------------------------------------------------------------
    call EEE_get_state_BC(Xyz_D, RhoCme, Ucme_D, Bcme_D, pCme, &
         tSimulation, nStep, nIteration)
    b_cme_d = Bcme_D*Si2No_V(UnitB_)
  end function b_cme_d
  !============================================================================
  subroutine set_threads(NameCaller)

    use BATL_lib, ONLY: MaxBlock, Unused_B, nBlock, MaxDim
    use ModParallel, ONLY: DiLevel_EB, Unset_
    use ModPhysics, ONLY: Si2No_V, UnitTemperature_
    use ModMain, ONLY: nStep
    use ModMpi
    character(len=*), intent(in) :: NameCaller
    integer:: iBlock, nBlockSet, nBlockSetAll, nPointMin, nPointMinAll, j, k
    integer:: iError, nThreadLoc, nPlotPoint
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
       ! If not DoThreads_B CYCLE
       if(.not.DoThreads_B(iBlock))then
          if(IsAllocatedThread_B(iBlock).and.&
               .not.is_threaded_block(iBlock))&
               call deallocate_thread_b(iBlock)
          CYCLE
       end if
       DoThreads_B(iBlock) = .false.
       ! Check if the block is at the inner boundary
       ! Otherwise CYCLE
       if(.not.is_threaded_block(iBlock))then
          if(IsAllocatedThread_B(iBlock))&
               call deallocate_thread_b(iBlock)
          CYCLE
       end if
       ! Allocate threads if needed
       if(.not.IsAllocatedThread_B(iBlock))then
          allocate(BoundaryThreads_B(iBlock)%LengthSi_III(&
               -nPointThreadMax:0,1:nJ,1:nK))
          BoundaryThreads_B(iBlock)%LengthSi_III = 0.0

          allocate(BoundaryThreads_B(iBlock)%BDsFaceInvSi_III(&
               -nPointThreadMax:0,1:nJ, 1:nK))
          BoundaryThreads_B(iBlock)%BDsFaceInvSi_III = 0.0

          allocate(BoundaryThreads_B(iBlock)%DsCellOverBSi_III(&
               -nPointThreadMax:0,1:nJ,1:nK))
          BoundaryThreads_B(iBlock)%DsCellOverBSi_III = 0.0

          allocate(BoundaryThreads_B(iBlock)%TMax_II(&
               1:nJ, 1:nK))
          BoundaryThreads_B(iBlock)%TMax_II = &
               1.0e8*Si2No_V(UnitTemperature_)

          allocate(BoundaryThreads_B(iBlock)%Xi_III(&
               -nPointThreadMax:0,1:nJ,1:nK))
          BoundaryThreads_B(iBlock)%Xi_III = 0.0

          allocate(BoundaryThreads_B(iBlock)%B_III(&
               -nPointThreadMax:0,1:nJ,1:nK))
          BoundaryThreads_B(iBlock)%B_III = 0.0

          allocate(BoundaryThreads_B(iBlock)%RInv_III(&
               -nPointThreadMax:0,1:nJ,1:nK))
          BoundaryThreads_B(iBlock)%RInv_III = 0.0

          allocate(BoundaryThreads_B(iBlock)%Coord_DIII(3,&
               -nPointThreadMax:0,1:nJ,1:nK))
          BoundaryThreads_B(iBlock)%Coord_DIII = 0.0

          allocate(BoundaryThreads_B(iBlock)%TGrav_III(&
               -nPointThreadMax:0,1:nJ,1:nK))
          BoundaryThreads_B(iBlock)%TGrav_III = 0.0

          allocate(BoundaryThreads_B(iBlock)%State_VIII(PSi_:TiSi_,&
               -nPointThreadMax:0,1:nJ,1:nK))
          BoundaryThreads_B(iBlock)%State_VIII(TeSi_:TiSi_,:,:,:) = -1
          BoundaryThreads_B(iBlock)%State_VIII(PSi_:A2Minor_,:,:,:) = 0.0

          allocate(BoundaryThreads_B(iBlock)%nPoint_II(&
               1:nJ,1:nK))
          BoundaryThreads_B(iBlock)%nPoint_II = -1

          allocate(BoundaryThreads_B(iBlock)%DeltaR_II(&
               1:nJ,1:nK))
          BoundaryThreads_B(iBlock)%DeltaR_II = 0.0

          allocate(BoundaryThreads_B(iBlock)%SignB_II(&
               1:nJ, 1:nK))
          BoundaryThreads_B(iBlock)%SignB_II = 0.0

          allocate(BoundaryThreads_B(iBlock)%iStencil_III(2:3,&
               1:nJ, 1:nK))
          BoundaryThreads_B(iBlock)%iStencil_III = 0

          allocate(BoundaryThreads_B(iBlock)%Weight_III(MaxDim,&
               1:nJ, 1:nK))
          BoundaryThreads_B(iBlock)%Weight_III = 0.0

          allocate(BoundaryThreads_B(iBlock)%State_VG(&
               PSi_:TiSi_, -nGUniform:1,&
               jMin_:jMax_, kMin_:kMax_))
          BoundaryThreads_B(iBlock)%State_VG = 0.0
          IsAllocatedThread_B(iBlock) = .true.
       end if
       ! The threads are now set in a just created block, or
       ! on updating B_0 field
       call set_threads_b(iBlock)
       nBlockSet = nBlockSet + 1
       do k = 1, nK; do j = 1, nJ
          nPointMin = min(nPointMin, BoundaryThreads_B(iBlock)%nPoint_II(j,k))
       end do; end do
    end do
    ! Number of threads (originating from physical cells) at the given PE
    nThreadLoc = count(IsAllocatedThread_B(1:nBlock))*nJ*nK
    if(nProc==1)then
       nBlockSetAll = nBlockSet
       nPointMinAll = nPointMin
       nThread_P(0) = nThreadLoc
       nThreadAll   = nThreadLoc
    else
       call MPI_ALLREDUCE(nBlockSet, nBlockSetAll, 1, MPI_INTEGER, MPI_SUM,&
            iComm, iError)
       call MPI_REDUCE(nPointMin, nPointMinAll, 1, MPI_INTEGER, MPI_MIN,&
            0, iComm, iError)
       call MPI_ALLGATHER(nThreadLoc, 1, MPI_INTEGER, nThread_P, 1, &
            MPI_INTEGER, iComm, iError)
       nThreadAll = sum(nThread_P)
    end if
    if(nBlockSetAll > 0)then
       if(allocated(Weight_III))deallocate(Weight_III, Xyz_DII, &
            iStencil_III, iList_II, iPointer_II, iEnd_II)
       nPlotPoint = count(IsAllocatedThread_B(1:nBlock))*(jMax_ - jMin_ + 1)*&
            (kMax_ - kMin_ +1)
       allocate(Weight_III(3, nPlotPoint, -nGUniform:0))
       allocate(Xyz_DII(3,nThreadAll+2,-nGUniform:0))
       allocate(iStencil_III(3, nPlotPoint, -nGUniform:0))
       allocate(iList_II(6*nThreadAll,-nGUniform:0),    &
            iPointer_II(6*nThreadAll,-nGUniform:0),     &
            iEnd_II(nThreadAll+2,-nGUniform:0) )
       call set_triangulation
       if(nBlockSetAll > 0.and.iProc==0)then
          write(*,*)'Set threads in ',nBlockSetAll,' blocks on iteration ', &
               nStep, ' is called from '//NameCaller
          write(*,*)'nPointMin = ',nPointMinAll
          write(*,*)'dCoord1Uniform =', 1/dCoord1Inv
          if(nProc<=8)&
               write(*,*)'Number of threads on different PEs: ', nThread_P
       end if
    end if
    call test_stop(NameSub, DoTest)
  end subroutine set_threads
  !============================================================================
  subroutine set_threads_b(iBlock)

    use EEE_ModCommonVariables, ONLY: UseCme
    use ModPhysics, ONLY: No2Si_V, UnitTemperature_, UnitX_, UnitB_
    use ModMain, ONLY: DoThreadRestart
    use ModGeometry, ONLY: Xyz_DGB
    use ModNumConst, ONLY: cTolerance
    use ModTurbulence, ONLY:PoyntingFluxPerBSi
    use ModCoordTransform, ONLY: rot_xyz_rlonlat
    use BATL_lib, ONLY: MaxDim, xyz_to_coord, coord_to_xyz, CoordMin_DB, &
         CellSize_DB
    integer, intent(in) :: iBlock
    ! Locals:
    ! Loop variable: (j,k) enumerate the cells at which
    ! the threads starts, iPoint starts from negative
    ! values at the photospheric end and the maximal
    ! value of this index is 0 for the thread point
    ! at the physical cell center.
    integer :: j, k, iPoint, nTrial, iInterval, nPoint, j1, k1

    ! Length interval, ! Heliocentric distance
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
    ! Aux
    real :: ROld, Aux
    real :: DirB_D(MaxDim), DirR_D(MaxDim), XyzOld_D(MaxDim)
    ! Conversion matrix and r-lon-lat bector of direction
    real :: XyzRlonlat_DD(MaxDim,MaxDim), bRlonlat_D(MaxDim)
    ! Mesh size along r, Lon, Lat
    real :: Dr, DsLon, DsLat, Weight_I(MaxDim)
    real :: CosBRMin = 1.0
    real :: BdS,  IntegralBdS
    integer, parameter :: nCoarseMax = 2

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_threads_b'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! Initialize threads
    BoundaryThreads_B(iBlock)%iAction = DoInit_
    BoundaryThreads_B(iBlock)%Xi_III = 0.0
    BoundaryThreads_B(iBlock)%nPoint_II = 0
    BoundaryThreads_B(iBlock)%DeltaR_II = 1.0

    ! Loop over the thread starting points
    do k = 1, nK; do j = 1, nJ
       ! First, take magnetic field in the ghost cell
       ! Starting points for all threads are in the centers
       ! of  physical cells near the boundary
       XyzStart_D = Xyz_DGB(:, 1, j, k, iBlock)
       ! Calculate a field in the starting point
       call get_b0(XyzStart_D, B0Start_D)
       ! Account for the CME field as needed
       if(UseCme)B0Start_D = B0Start_D + b_cme_d(XyzStart_D)
       SignBr = sign(1.0, sum(XyzStart_D*B0Start_D) )
       BoundaryThreads_B(iBlock)%SignB_II(j, k) = SignBr
       B0Start = norm2(B0Start_D)
       BoundaryThreads_B(iBlock)%B_III(0, j, k) = B0Start

       RStart = norm2(XyzStart_D)
       BoundaryThreads_B(iBlock)%RInv_III(0, j, k) = 1/RStart
       call xyz_to_coord(XyzStart_D, Coord_D)
       BoundaryThreads_B(iBlock)%Coord_DIII(:,0, j, k) = Coord_D

       Ds = 0.50*DsThreadMin ! To enter the grid coarsening loop
       COARSEN: do nTrial=1,nCoarseMax
          ! Set initial Ds or increase Ds, if previous trial fails
          Ds = Ds * 2
          iPoint = 0
          Xyz_D = XyzStart_D
          B0 = B0Start
          B0_D = B0Start_D
          R = RStart
          if(nTrial==nCoarseMax)then
             CosBRMin = ( (RStart**2-rChromo**2)/nPointThreadMax +Ds**2)/&
                  (2*rChromo*Ds)
             if(CosBRMin>0.9)call stop_mpi('Increase nPointThreadMax')
          end if
          POINTS: do
             iPoint = iPoint + 1
             ! If the number of gridpoints in the theads is too
             ! high, coarsen the grid
             if(iPoint > nPointThreadMax)CYCLE COARSEN
             ! For the previous point given are Xyz_D, B0_D, B0
             ! R is only used near the photospheric end.
             ! Two stage Runge-Kutta
             ! 1. Point at the half of length interval:
             DirR_D = Xyz_D/R
             DirB_D = SignBr*B0_D/max(B0, cTolerance)
             if(nTrial==nCoarseMax)call limit_cosBR
             XyzAux_D = Xyz_D - 0.50*Ds*DirB_D
             ! 2. Magnetic field in this point:
             call get_b0(XyzAux_D, B0Aux_D)
             if(UseCme)B0Aux_D = B0Aux_D + b_cme_d(XyzAux_D)
             DirB_D = SignBr*B0Aux_D/max(norm2(B0Aux_D), cTolerance**2)
             if(nTrial==nCoarseMax)call limit_cosBR
             ! 3. New grid point:
             Xyz_D = Xyz_D - Ds*DirB_D
             R = norm2(Xyz_D)
             if(R <= rChromo)EXIT COARSEN
             if(R > RStart)CYCLE COARSEN
             ! Store a point
             XyzOld_D = Xyz_D
             BoundaryThreads_B(iBlock)%RInv_III(-iPoint, j, k) = 1/R
             call xyz_to_coord(Xyz_D, Coord_D)
             BoundaryThreads_B(iBlock)%Coord_DIII(:,-iPoint, j, k) = Coord_D
             call get_b0(Xyz_D, B0_D)
             if(UseCME)B0_D = B0_D + b_cme_d(Xyz_D)
             B0 = norm2(B0_D)
             BoundaryThreads_B(iBlock)%B_III(-iPoint, j, k) = B0
          end do POINTS
       end do COARSEN
       if(R > rChromo)then
          write(*,*)'iPoint, R=', iPoint, R
          call stop_mpi('Thread did not reach the photosphere!')
       end if
       !
       ! With assumed default value for nIntervalTR = 2
       ! nPoint is the total number of temperature grid points,
       ! indexed (1-nPoint:0)
       ! Point with index= -1 - nPoint is at the photosphere level
       ! Point with index= -nPoint is inside the transition region.
       ! The distance between the points -1-nPoint and -nPoint is
       ! Aux*Ds<=Ds. The distance between the points -nPoint and
       ! 1 - nPoint is Ds. The point 1 - nPoint is on top of the TR.
       !
       ! At the grid points, (1-nPoint:0) are defined:
       !
       ! Electron temperature State_VIII(TeSi_,1-nPoint:0, j,k)
       ! Ion Temperature      State_VIII(TiSi_,1-nPoint:0, j,k)
       ! Pressure             State_VIII(PSi_ ,1-nPoint:0, j,k)
       ! Gen. coords.         Coord_DIII(:    ,1-nPoint:0, j,k)
       ! Magnetic field                  B_III(1-nPoint:0, j,k)
       ! Inv. heliocentric distance   RInv_III(1-nPoint:0, j,k)
       !
       ! At by one smaller number of grid points is defined:
       !
       ! Barometric factor:          TGrav_III(1-nPoint:0, j,k)
       !
       ! At by one larger number of grid points are defined:
       !
       ! Amplitude of major wave State_VIII(AMajor_,-nPoint:0, j,k)
       ! Amplitude of minor wave State_VIII(AMajor_,-nPoint:0, j,k)
       ! The wave grid point with index -nPoint<iPoint<0 is between
       ! iPoint and iPoint+1 points of the temperature grid. The
       ! marginal points are at the top of TR and at the top of thread
       ! just as for the temperature grid, therefore, the marginal
       ! grid spaces are of the length of Ds/2
       !
       ! Differentials and integrals over length:
       ! LengthSi_III
       ! BDsFaceInvSi_III
       ! DsCellOverBSi_III
       ! Xi_III

       ! Calculate more accurately the intersection point
       ! with the photosphere surface
       ROld = 1/BoundaryThreads_B(iBlock)%RInv_III(1-iPoint, j, k)
       Aux = (ROld - rChromo) / (ROld -R)
       Xyz_D = (1 - Aux)*XyzOld_D +  Aux*Xyz_D
       ! Store the last point
       BoundaryThreads_B(iBlock)%RInv_III(-iPoint, j, k) = 1/rChromo
       call get_b0(Xyz_D, B0_D)
       B0 = norm2(B0_D)
       ! Last point for magnetic field (-nPoint-1, j, k)
       BoundaryThreads_B(iBlock)%B_III(-iPoint, j, k) = B0
       ! Store the number of points. This will be the number of temperature
       ! nodes such that the first one is on the top of the TR, the last
       ! one is in the center of physical cell
       nPoint = iPoint + 1 - nIntervalTR
       iPoint = nPoint - 1 + nIntervalTR ! nPoint + 1
       BoundaryThreads_B(iBlock)%nPoint_II(j,k) = nPoint
       BoundaryThreads_B(iBlock)%TGrav_III(2-nPoint:0,j,k) = &
            GravHydroStat*&
            (-BoundaryThreads_B(iBlock)%RInv_III(2-nPoint:0,j,k) + &
            BoundaryThreads_B(iBlock)%RInv_III(1-nPoint:-1,j,k))

       ! Store the lengths
       ! First non-zero Length at the point (-nPoint,j,k)
       BoundaryThreads_B(iBlock)%LengthSi_III(1-iPoint, j, k) = &
            Ds*Aux*No2Si_V(UnitX_)
       ! First contribution for all integrals from the (shorter) interval
       ! between points -nPoint-1 and -nPoint
       IntegralBdS = &
            Ds*Aux*0.50*(                                         &
            BoundaryThreads_B(iBlock)%B_III(-iPoint, j, k) +    &
            BoundaryThreads_B(iBlock)%B_III(1-iPoint, j, k) )
       iPoint = iPoint - 1 ! nPoint
       !     |           |
       !-iPoint-1      -iPoint
       iInterval = 1
       do while(iInterval < nIntervalTR)
          ! Contribution from the interval -nPoint, 1-nPoint
          BoundaryThreads_B(iBlock)%LengthSi_III(1-iPoint, j, k) =     &
               BoundaryThreads_B(iBlock)%LengthSi_III(-iPoint, j, k) + &
               Ds*No2Si_V(UnitX_)
          IntegralBdS = IntegralBdS + Ds*0.50*(&
               BoundaryThreads_B(iBlock)%B_III( -iPoint, j, k) +       &
               BoundaryThreads_B(iBlock)%B_III(1-iPoint, j, k) )
          iPoint = iPoint - 1  ! nPoint -1
          iInterval = iInterval + 1   ! For nInterval = 2 exit the loop
       end do
       !     |            |...........|           Temperature nodes
       !-iPoint-nInterval          -iPoint
       !                        x           x            Flux nodes
       !                    -iPoint-1    -iPoint
       ! Here, iPoint is the current value, equal to nPoint-1, nPoint being
       ! the stored value. Now, LengthSi = Ds*(nInterval - 1 + Aux), as long
       ! as the first interval is shorter than Ds. Then, BDsFaceInvSi at the
       ! face -nPoint stores the inverse of the distance to the 1-nPoint, and
       ! field at the same point

       BoundaryThreads_B(iBlock)%BDsFaceInvSi_III(-1-iPoint, j, k) = 1/  &
            (BoundaryThreads_B(iBlock)%LengthSi_III(-iPoint, j, k)*  &
            BoundaryThreads_B(iBlock)%B_III(-iPoint, j, k)*          &
            No2Si_V(UnitB_)*PoyntingFluxPerBSi)

       ! The flux node -iPoint-1=-nPoint is placed to the same position
       ! as the temperature node with the same number
       BoundaryThreads_B(iBlock)%Xi_III(-iPoint, j, k) =             &
            0.50*Ds*sqrt(CoefXi/                                       &
            BoundaryThreads_B(iBlock)%B_III(-iPoint, j, k))
       ! As long as the flux node is placed as discussed above, the
       ! first computational cell is twice shorter
       BoundaryThreads_B(iBlock)%DsCellOverBSi_III(-iPoint, j, k) =      &
            0.50*Ds*No2Si_V(UnitX_)/                                   &
            ( BoundaryThreads_B(iBlock)%B_III(-iPoint, j, k)&
            *PoyntingFluxPerBSi*No2Si_V(UnitB_) )

       do while(iPoint>0)
          ! Just the length of Thread in meters
          BoundaryThreads_B(iBlock)%LengthSi_III(1-iPoint, j, k) =      &
               BoundaryThreads_B(iBlock)%LengthSi_III(-iPoint, j, k ) + &
               Ds*No2Si_V(UnitX_)
          !
          ! Sum up the integral of Bds, dimensionless, to calculate TMax
          !
          BdS = Ds*0.50*(&
               BoundaryThreads_B(iBlock)%B_III(-iPoint, j, k) + &
               BoundaryThreads_B(iBlock)%B_III(1-iPoint, j, k) )
          IntegralBdS = IntegralBdS + BdS
          !
          ! 1/(ds*B*Poynting-flux-over-field ratio)
          ! calculated in the flux point, so that the averaged magnetic field
          ! is used. Calculated in SI units. While multiplied by the difference
          ! in the conservative variable, 2/7*kappa*\Delta T^{7/2} gives the
          ! ratio of the heat flux to the effective Poynting flux.
          BoundaryThreads_B(iBlock)%BDsFaceInvSi_III(-iPoint, j, k) =   1/  &
               (PoyntingFluxPerBSi*No2Si_V(UnitB_)*No2Si_V(UnitX_)*BdS)
          ! The distance between the flux points (as well as the AW amplitude
          ! nodes multiplied by the coefficient to find dimensionless
          ! wave damping per the unit of length. Calculated in terms of
          ! the magnetic field in the midpoint. Should be multiplied by the
          ! dimensionless density powered 1/4. Dimensionless.
          BoundaryThreads_B(iBlock)%Xi_III(1-iPoint, j, k) =            &
               BoundaryThreads_B(iBlock)%Xi_III(-iPoint, j, k)        + &
               Ds*sqrt(CoefXi/               &
               BoundaryThreads_B(iBlock)%B_III(1-iPoint, j, k))
          ! Distance between the flux points (faces) divided by
          ! the magnetic field in the temperature point (or, same,
          ! multiplied by the crosssection of the flux tube)
          ! and divided by the Poynting-flux-to-field ratio.
          BoundaryThreads_B(iBlock)%DsCellOverBSi_III(1-iPoint, j, k) =     &
               Ds*No2Si_V(UnitX_)/                                        &
               (BoundaryThreads_B(iBlock)%B_III(1-iPoint, j, k)         &
               *PoyntingFluxPerBSi*No2Si_V(UnitB_))
          iPoint = iPoint - 1
       end do
       !          |       |           Temperature nodes
       !         -1       0
       !              x       x       Flux nodes
       !             -1       0

       ! Correct the effective length of the last flux node
       !          |       |           Temperature nodes
       !         -1       0
       !              x   <---x       Flux nodes
       !             -1       0
       BoundaryThreads_B(iBlock)%Xi_III(0, j, k) =         &
            BoundaryThreads_B(iBlock)%Xi_III(0, j, k) -    &
            0.50*Ds*sqrt(CoefXi/                             &
            BoundaryThreads_B(iBlock)%B_III(0, j, k))

       BoundaryThreads_B(iBlock)%DeltaR_II(j,k) = &
            norm2(Xyz_DGB(:,1,j,k,iBlock) - Xyz_DGB(:,0,j,k,iBlock) )

       call limit_temperature(&
            IntegralBdS, BoundaryThreads_B(iBlock)%TMax_II(j, k))

    end do; end do
    if(DoThreadRestart)call read_thread_restart(iBlock)

    if(DoTest.and.iBlock==iBlockTest)then
       write(*,'(a,3es18.10)')'Thread starting at the point  ',&
            Xyz_DGB(:,1,jTest,kTest,iBlock)
       write(*,'(a,3es18.10)')'DeltaR=  ',&
            BoundaryThreads_B(iBlock)%DeltaR_II(jTest,kTest)
       write(*,'(a)')&
            'B[NoDim] RInv[NoDim] LengthSi BDsFaceInvSi Xi[NoDim]'
       do iPoint = 0, -BoundaryThreads_B(iBlock)%nPoint_II(jTest,kTest),-1
          write(*,'(5es18.10)')&
               BoundaryThreads_B(iBlock)%B_III(&
               iPoint, jTest,kTest),&
               BoundaryThreads_B(iBlock)%RInv_III(&
               iPoint, jTest,kTest),&
               BoundaryThreads_B(iBlock)%LengthSi_III(&
               iPoint, jTest, kTest),&
               BoundaryThreads_B(iBlock)%BDsFaceInvSi_III(&
               iPoint, jTest, kTest),&
               BoundaryThreads_B(iBlock)%Xi_III(&
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

      use ModPhysics, ONLY: UnitX_, Si2No_V, UnitB_
      use ModLookupTable, ONLY: interpolate_lookup_table
      real, intent(in)  :: BLength
      real, intent(out) :: TMax
      real :: HeatFluxL
      !------------------------------------------------------------------------
      HeatFluxL = 2*PoyntingFluxPerBSi*&
           BLength*No2Si_V(UnitX_)*No2Si_V(UnitB_)
      call interpolate_lookup_table(iTable=iTableTR, Arg2In=0.0, &
           iVal=HeatFluxL_, &
           ValIn=HeatFluxL, &
           Value_V=TrTable_V,      &
           Arg1Out=TMax,  &
           DoExtrapolate=.false.)
      ! Version Easter 2015
      ! Globally limiting the temparture is mot much
      ! physical, however, at large enough timerature
      ! the existing semi-implicit solver is unstable
      TMax = min(TMax,TeGlobalMaxSi)*Si2No_V(UnitTemperature_)

    end subroutine limit_temperature
    !==========================================================================
  end subroutine set_threads_b
  !============================================================================
  subroutine advance_threads(iAction)
    use ModParallel, ONLY: DiLevel_EB, Unset_
    use BATL_lib, ONLY: nBlock, Unused_B

    integer, intent(in)::iAction

    integer:: iBlock
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'advance_threads'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE
       if(DiLevel_EB(1,iBlock)/=Unset_)then
          if(.not.IsAllocatedThread_B(iBlock))then
             CYCLE
          else
             call stop_mpi('Threads are at block not near inner boundary')
          end if
       else
          if(.not.IsAllocatedThread_B(iBlock))&
               call stop_mpi('No threads at the block near inner boundary')
       end if
       if(BoundaryThreads_B(iBlock)%iAction /= Done_)&
            call stop_mpi('An attempt to readvance not advanced threads')
       BoundaryThreads_B(iBlock)%iAction = iAction
    end do

    call test_stop(NameSub, DoTest)

  end subroutine advance_threads
  !============================================================================
  subroutine advance_threaded_block_expl(iBlock, iStage, &
       RightState_VII, LeftState_VII, DtIn, Dt_II)

    integer, intent(in) :: iBlock, iStage
    real, intent(in)    :: RightState_VII(nVar, 1:nJ, 1:nK)
    real, intent(inout) :: LeftState_VII(nVar, 1:nJ, 1:nK)
    real, optional, intent(in) :: DtIn, Dt_II(1:nJ, 1:nK)

    !--------------------------------------------------------------------------
  end subroutine advance_threaded_block_expl
  !============================================================================
  subroutine read_thread_restart(iBlock)

    use ModMain, ONLY: NameThisComp
    use ModIoUnit, ONLY: UnitTmp_
    use ModUtilities, ONLY: open_file, close_file
    integer, intent(in) :: iBlock
    ! loop variables
    integer :: j, k
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
    do k = 1, nK; do j = 1, nJ
       read(UnitTmp_, iostat = iError)RealNPoint
       if(iError>0)then
          write(*,*)'Error in reading nPoint in Block=', iBlock
          call close_file
          RETURN
       end if
       nPoint = nint(RealNPoint)
       if(BoundaryThreads_B(iBlock)%nPoint_II(j,k)/=nPoint)then
          write(*,*)'Incorrect nPoint in Block=', iBlock
          call close_file
          RETURN
       end if
       read(UnitTmp_, iostat = iError) &
            BoundaryThreads_B(iBlock)%State_VIII(TeSi_,1-nPoint:0,j,k), &
            BoundaryThreads_B(iBlock)%State_VIII(TiSi_,1-nPoint:0,j,k), &
            BoundaryThreads_B(iBlock)%State_VIII(PSi_,1-nPoint:0,j,k),  &
            BoundaryThreads_B(iBlock)%State_VIII(AMajor_,-nPoint:0,j,k),&
            BoundaryThreads_B(iBlock)%State_VIII(AMinor_,-nPoint:0,j,k)
    end do; end do
    call close_file
    BoundaryThreads_B(iBlock)%iAction = Restart_
    call test_stop(NameSub, DoTest, iBlock)

  end subroutine read_thread_restart
  !============================================================================
  subroutine interpolate_thread_state(Coord_D, iBlock, State_V, DoTestIn)
    ! Interpolate the state at the point with coords Coord_D from
    ! BoundaryThreads_B(iBlock)%State_VG(:,:,:)
    ! array, then convert to MHD
    use ModAdvance, ONLY: nVar
    use BATL_lib, ONLY: CoordMin_DB, CellSize_DB
    use ModInterpolate, ONLY: interpolate_vector

    ! Coords of the point in which to interpolate
    real,    intent(in) :: Coord_D(3)

    ! Block at which the grid is allocated
    integer, intent(in) :: iBlock

    ! Interpolated state vector
    real,    intent(out):: State_V(nVar)
    Logical, optional, intent(in) :: DoTestIn
    real                :: StateThread_V(PSi_:TiSi_), CoordNorm_D(3)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'interpolate_thread_state'
    !--------------------------------------------------------------------------
    if(present(DoTestIn))then
       DoTest = DoTestIn
    else
       DoTest = .false.
    end if
    CoordNorm_D(r_+1:) = 0.5 + &
         (Coord_D(r_+1:) - CoordMin_DB(r_+1:,iBlock)) &
         /CellSize_DB(r_+1:,iBlock)

    if(Coord_D(r_) > CoordMin_DB(r_,iBlock))then
       ! The point is in between the uniform grid and the first layer pf
       ! physical cells, the width of this gap being the half cell size
       CoordNorm_D(r_) = (Coord_D(r_) - CoordMin_DB(r_,iBlock))*2/&
            CellSize_DB(r_,iBlock)
       ! Interpolate the state on threads to the given location
       StateThread_V = interpolate_vector(                        &
            a_VC=BoundaryThreads_B(iBlock)%State_VG(:,0:1,:,:),&
            nVar=TiSi_,                                           &
            nDim=3,                                               &
            Min_D=[0, jMin_, kMin_],                              &
            Max_D=[1, jMax_, kMax_],                              &
            x_D=CoordNorm_D,                                      &
            DoExtrapolate=.false.                                 )
    else
       CoordNorm_D(r_) = (Coord_D(r_) - CoordMin_DB(r_,iBlock))*&
            dCoord1Inv
       ! Interpolate the state on threads to the given location
       StateThread_V = interpolate_vector(                        &
            a_VC=BoundaryThreads_B(iBlock)%State_VG(:,            &
            -nGUniform:0,:,:),                                    &
            nVar=TiSi_,                                           &
            nDim=3,                                               &
            Min_D=[-nGUniform, jMin_, kMin_],                     &
            Max_D=[0, jMax_, kMax_],                              &
            x_D=CoordNorm_D,                                      &
            DoExtrapolate=.false.                                 )
    end if
    if(DoTest)write(*,'(a,4es14.6)')NameSub//': PSi, a2+/-, TeSi=',&
         StateThread_V(PSi_:TeSi_)
    call state_thread_to_mhd(Coord_D, StateThread_V, State_V)
  contains
    !==========================================================================
    subroutine state_thread_to_mhd(Coord_D, StateThread_V, State_V)
      ! Convert the state stored in the State_VG array to
      ! the MHD state vector
      use BATL_lib, ONLY: MaxDim, coord_to_xyz
      use ModAdvance, ONLY: nVar, Rho_, WaveFirst_, WaveLast_, Bx_
      use ModPhysics, ONLY: Si2No_V, UnitTemperature_, UnitEnergyDens_
      use ModTurbulence, ONLY: PoyntingFluxPerB
      use EEE_ModCommonVariables, ONLY: UseCme
      !INPUT:
      ! Coordinates to determine the magnetic field
      real,    intent(in) :: Coord_D(MaxDim)
      real,    intent(in) :: StateThread_V(PSi_:TiSi_)
      ! MHD state vector
      real,    intent(out):: State_V(nVar)
      ! Dimensionless plasma parameters
      real :: pTotal, Te, Ti
      real :: Xyz_D(MaxDim), B0_D(MaxDim), Aux, BCme_D(MaxDim)
      ! Nullify momentum and field components of the state vector
      character(len=*), parameter:: NameSub = 'state_thread_to_mhd'
      !------------------------------------------------------------------------
      State_V = 0.0
      ! Transform Si parameters to diminsionless ones:
      pTotal = StateThread_V(PSi_) *Si2No_V(UnitEnergyDens_ )
      Te     = StateThread_V(TeSi_)*Si2No_V(UnitTemperature_)
      if(UseElectronPressure)then
         Ti  = StateThread_V(TiSi_)*Si2No_V(UnitTemperature_)
         ! Use the following equations
         ! Te = TeFraction*State_V(iPe)/State_V(Rho_)
         ! Ti = TiFraction*State_V(p_)/State_V(Rho_)
         ! and State_V(iPe) + State_V(p_) = pTotal
         State_V(Rho_) = pTotal/(Te/TeFraction + Ti/TiFraction)
         State_V(p_)   = Ti/TiFraction*State_V(Rho_)
         State_V(iPe)   = Te/TeFraction*State_V(Rho_)
      else
         State_V(p_)   = pTotal
         ! Use equation
         ! Te = TeFraction*State_V(iPe)/State_V(Rho_)
         State_V(Rho_) = TeFraction*pTotal/Te
      end if
      Aux = PoyntingFluxPerB*sqrt(State_V(Rho_))
      call coord_to_xyz(Coord_D, Xyz_D)
      call get_b0(Xyz_D,B0_D)
      if(UseCme)then
         State_V(Bx_:Bx_+MaxDim-1)=b_cme_d(Xyz_D)
         B0_D = B0_D + State_V(Bx_:Bx_+MaxDim-1)
      end if
      if(sum(B0_D*Xyz_D) <  0.0)then
         State_V(WaveLast_ ) = StateThread_V(A2Major_)*Aux
         State_V(WaveFirst_) = StateThread_V(A2Minor_)*Aux
      else
         State_V(WaveFirst_) = StateThread_V(A2Major_)*Aux
         State_V(WaveLast_ ) = StateThread_V(A2Minor_)*Aux
      end if
    end subroutine state_thread_to_mhd
    !==========================================================================
  end subroutine interpolate_thread_state
  !============================================================================
  subroutine set_thread_plotvar(iBlock, nPlotVar, NamePlotVar_V, Xyz_D, &
       State_V, PlotVar_V)

    use ModMain
    use ModVarIndexes
    use ModAdvance, ONLY: UseElectronPressure, &
         UseMultiSpecies
    use ModGeometry
    use ModPhysics, ONLY: OmegaBody,  &
         ElectronPressureRatio, InvGammaMinus1_I, Si2No_V, UnitB_
    use ModUtilities, ONLY: lower_case
    use ModIO, ONLY: NameVarUserTec_I, NameUnitUserTec_I, &
         NameUnitUserIdl_I
    use ModMultiFluid, ONLY: extract_fluid_name,      &
         UseMultiIon, nIonFluid, iPpar, iPFluid=>iP,   &
         IsMhd, iRho, iRhoUx, iRhoUy, iRhoUz, iRhoIon_I
    use ModCoordTransform, ONLY: cross_product
    use BATL_lib, ONLY: iNode_B, CellSize_DB
    use ModB0, ONLY: get_b0
    use ModWaves, ONLY: UseWavePressure
    use ModTurbulence, ONLY: PoyntingFluxPerB

    integer, intent(in) :: iBlock, nPlotVar
    character(LEN=20)   :: NamePlotVar_V(nPlotVar)
    real,    intent(in) :: Xyz_D(3)
    real, intent(inout) :: State_V(nVar)
    real,   intent(out) :: PlotVar_V(nPlotVar)

    ! To calculate B0 and BFull, if needed
    real :: B0_D(3) = 0.0, FullB_D(3)
    character (len=10)  :: String, NamePlotVar

    real:: Tmp1Var, Tmp2Var

    integer :: iVar, jVar, iIon, iFluid
    ! Conversion coefficients from the squared Alfven wave amplitude to
    ! energy densities
    real :: SignBr, Wave0, Wave1

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_thread_plotvar'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    ! Calculate B0 and BFull
    B0_D = 0.0
    if(UseB0)call get_b0(Xyz_D, B0_D)
    FullB_D = State_V(Bx_:Bz_) + B0_D

    ! Convert the squared Alfven wave amplitudes to
    ! energy densities
    signBr = sign(1.0, sum(FullB_D*Xyz_D))
    if(signBr < 0.0)then
       ! In this case WaveLast dominates over WaveFirst
       ! Currently, in the state vector WaveFirst is dominant
       ! Reorder this array
       Wave0 = State_V(WaveLast_); Wave1 = State_V(WaveFirst_)
       State_V(WaveFirst_) = Wave0; State_V(WaveLast_) = Wave1
    end if
    ! Convert to the energy density
    State_V(WaveFirst_:WaveLast_) = State_V(WaveFirst_:WaveLast_)*&
         PoyntingFluxPerB*sqrt(State_V(Rho_))

    PlotVar_V = 0.0
    do iVar = 1, nPlotVar
       NamePlotVar = NamePlotVar_V(iVar)

       ! Default values for TecPlot variable name and
       ! TecPlot and IDL unit names
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
          ! Internal plus kinetic energy
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

          ! Calculate temperature from
          !   P = n*k*T + ne*k*Te = n*k*T*(1 + ne/n*Te/T)
          if(String /= 'n')then
             ! t = p/n
             PlotVar_V(iVar) = State_V(iPFluid) / PlotVar_V(iVar)

             if(nFluid==1 .and. .not.UseElectronPressure &
                  .and. ElectronPressureRatio > 0.0) &
                  PlotVar_V(iVar) = PlotVar_V(iVar)&
                  /(1 + ElectronPressureRatio)
          end if
       case('te')
          ! Use the following equation
          ! Te = TeFraction*State_V(iPe)/State_V(Rho_)
          PlotVar_V(iVar) = TeFraction*State_V(iPe)/State_V(Rho_)
       case('ti')
          ! Use the following equation
          ! Ti = TiFraction*State_V(p_)/State_V(Rho_)
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
          Tmp1Var = sum(FullB_D**2)
          Tmp2Var = sum(FullB_D*State_V(iRhoUx:iRhoUz))
          PlotVar_V(iVar) = sum ( &
               Xyz_D*( Tmp1Var*State_V(iRhoUx:iRhoUz) &
               -  Tmp2Var*FullB_D ) &
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
          PlotVar_V(iVar) = DtMax_B(iBlock)
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
  !============================================================================
  subroutine save_thread_restart

    use ModMain, ONLY: NameThisComp
    use BATL_lib, ONLY: nBlock, Unused_B
    use ModIoUnit, ONLY: UnitTmp_
    use ModUtilities, ONLY: open_file, close_file
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
       do k = 1, nK; do j = 1, nJ
          nPoint = BoundaryThreads_B(iBlock)%nPoint_II(j,k)
          write(UnitTmp_)real(nPoint)
          write(UnitTmp_)&
               BoundaryThreads_B(iBlock)%State_VIII(TeSi_, 1-nPoint:0,j,k),&
               BoundaryThreads_B(iBlock)%State_VIII(TiSi_, 1-nPoint:0,j,k),&
               BoundaryThreads_B(iBlock)%State_VIII(PSi_,  1-nPoint:0,j,k),&
               BoundaryThreads_B(iBlock)%State_VIII(AMajor_,-nPoint:0,j,k),&
               BoundaryThreads_B(iBlock)%State_VIII(AMinor_,-nPoint:0,j,k)
       end do; end do
       call close_file
    end do
    call test_stop(NameSub, DoTest)

  end subroutine save_thread_restart
  !============================================================================
  subroutine get_restart_file_name(iBlock, NameRestartDir)
    use BATL_lib, ONLY: iMortonNode_A, iNode_B

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
  !============================================================================
  !==========================ROUTINES USED FOR TRIANGULATION===================
  subroutine set_triangulation

    use BATL_lib, ONLY: nBlock, Unused_B, &
         CoordMin_DB, CellSize_DB, x_, y_, z_
    use ModTriangulateSpherical, ONLY:trans, trmesh, find_triangle_sph, &
         find_triangle_orig
    use ModCoordTransform, ONLY: rlonlat_to_xyz
    use ModMpi

    integer :: i, j, k, iBlock, iBuff, iError

    ! Coordinates and state vector at the point of intersection of thread with
    ! the spherical coordinate  surface of the grid for plotting
    integer, parameter :: Lon_ = 1, Lat_=2
    integer :: iPE  ! Loop variable
    integer :: iSliceFirst_P(0:nProc-1), iSliceLast_P(0:nProc-1), nSlice
    real    :: Coord_DII(Lat_,nThreadAll,-nGUniform:0),          &
         Coord_D(Lon_:Lat_)
    real    :: Xyz_D(3)
    ! Local Logicals
    logical :: IsTriangleFound = .false.

    character(len=*), parameter:: NameSub = 'set_triangulation'
    !--------------------------------------------------------------------------
    call get_thread_point(Lat_, Coord_DII, DoCoord = .true. )
    do i = -nGUniform, 0
       ! Convert lon and lat to Cartesian coordinates on a unit sphere
       call trans( n=nThreadAll,&
            rlat=Coord_DII(2,:,i), &
            rlon=Coord_DII(1,:,i), &
            x= Xyz_DII(x_,2:nThreadAll+1,i), &
            y= Xyz_DII(y_,2:nThreadAll+1,i), &
            z= Xyz_DII(z_,2:nThreadAll+1,i))

       ! Add two grid nodes at the poles:
       Xyz_DII(:,1,i)            = [0.0, 0.0, -1.0]
       Xyz_DII(:,nThreadAll+2,i) = [0.0, 0.0, +1.0]
    end do
    do iPE = 0, nProc - 1
       iSliceFirst_P(iPE) = iPE*(nGUniform + 1)/nProc - nGUniform
       iSliceLast_P(iPE) = (iPE +1)*(nGUniform + 1)/nProc - (nGUniform+1)
    end do
    iList_II = 0; iPointer_II = 0; iEnd_II = 0

    do i = iSliceFirst_P(iProc), iSliceLast_P(iProc)
    ! do i = -nGUniform, 0
       ! Triangulate
       call trmesh(nThreadAll+2, &
            Xyz_DII(x_,:,i), Xyz_DII(y_,:,i), Xyz_DII(z_,:,i), &
            iList_II(:,i), iPointer_II(:,i), iEnd_II(:,i), iError)
       if(iError/=0)call stop_mpi(NameSub//': Triangilation failed')
    end do

    do iPE = 0, nProc - 1
       nSlice = 1 -  iSliceFirst_P(iPE) + iSliceLast_P(iPE)
       if(nSlice < 1)CYCLE
       call MPI_BCAST(iList_II(:,iSliceFirst_P(iPE):iSliceLast_P(iPE)),&
            6*nThreadAll*nSlice, MPI_INTEGER, &
            iPE, iComm, iError)
       call MPI_BCAST(iPointer_II(:,iSliceFirst_P(iPE):iSliceLast_P(iPE)),&
            6*nThreadAll*nSlice, MPI_INTEGER, &
            iPE, iComm, iError)
       call MPI_BCAST(iEnd_II(:,iSliceFirst_P(iPE):iSliceLast_P(iPE)),&
            (nThreadAll + 2)*nSlice, MPI_INTEGER, &
            iPE, iComm, iError)
    end do
    ! Now, calculate interpolation weights at the points of a grid used for
    ! plotting
    iBuff = 0
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE
       if(.not.IsAllocatedThread_B(iBlock))CYCLE

       do k = kMin_, kMax_; do j = jMin_, jMax_
          iBuff = iBuff + 1
          Coord_D = CoordMin_DB(2:,iBlock) + &
               CellSize_DB(2:, iBlock)*[j - 0.50, k - 0.50]
          ! Transform longitude and latitude to the unit vector
          call rlonlat_to_xyz(1.0, Coord_D(Lon_), Coord_D(Lat_), &
               Xyz_D)
          do i = -nGUniform, 0
             ! Find a triangle into which this vector falls and the
             ! interpolation weights
             if(UsePlanarTriangles)then
                call find_triangle_orig(Xyz_D, nThreadAll+2, Xyz_DII(:,:,i),&
                     iList_II(:,i), iPointer_II(:,i), iEnd_II(:,i),         &
                     Weight_III(:,iBuff,i), IsTriangleFound,                &
                     iStencil_III(:,iBuff,i))
             else
                call find_triangle_sph( Xyz_D, nThreadAll+2, Xyz_DII(:,:,i),&
                     iList_II(:,i), iPointer_II(:,i), iEnd_II(:,i),         &
                     Weight_III(1,iBuff,i), Weight_III(2,iBuff,i),          &
                     Weight_III(3,iBuff,i), IsTriangleFound,                &
                     iStencil_III(1,iBuff,i), iStencil_III(2,iBuff,i),      &
                     iStencil_III(3,iBuff,i))
             end if
             if(.not.IsTriangleFound)then
                write(*,*)'At the location x,y,z=', Xyz_D
                call stop_mpi('Interpolation on triangulated sphere fails')
             end if
          end do          ! i
       end do; end do    ! j,k
    end do       ! iBlock
  end subroutine set_triangulation
  !============================================================================
  subroutine save_threads_for_plot

    use BATL_lib, ONLY: nBlock, Unused_B, Xyz_DGB
    use ModB0, ONLY: B0_DGB
    use ModAdvance, ONLY: State_VGB
    use ModTriangulateSpherical, ONLY:fix_state

    integer :: i, j, k, iBlock, iBuff

    ! State vector at the point of intersection of thread with
    ! the spherical coordinate  surface of the grid for plotting
    real    :: State_VII(TiSi_,nThreadAll+2,-nGUniform:0)
    character(len=*), parameter:: NameSub = 'save_threads_for_plot'
    !--------------------------------------------------------------------------

    call get_thread_point(TiSi_,State_VII(:,2:nThreadAll+1,:))
    do i = -nGUniform, 0
       ! Fix states at the polar nodes:
       ! North:
       call fix_state(iNodeToFix =   1,   &
            nNode      =  nThreadAll+2,   &
            iList_I    =    iList_II(:,i), &
            iPointer_I = iPointer_II(:,i), &
            iEnd_I     =     iEnd_II(:,i), &
            Xyz_DI     =     Xyz_DII(:,:,i), &
            nVar       =      TiSi_,      &
            State_VI   = State_VII(:,:,i))
       ! South:
       call fix_state(iNodeToFix =  nThreadAll+2,&
            nNode      =  nThreadAll+2,   &
            iList_I    =    iList_II(:,i), &
            iPointer_I = iPointer_II(:,i), &
            iEnd_I     =     iEnd_II(:,i), &
            Xyz_DI     =     Xyz_DII(:,:,i), &
            nVar       =      TiSi_,      &
            State_VI   = State_VII(:,:,i))
    end do

    ! Now, interpolate state vector to the points of a grid used for
    ! plotting
    iBuff = 0
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE
       if(.not.IsAllocatedThread_B(iBlock))CYCLE

       do k = kMin_, kMax_; do j = jMin_, jMax_
          iBuff = iBuff + 1
          do i = -nGUniform, 0
             ! interpolate  state vector to a grid point of a uniform grid
             BoundaryThreads_B(iBlock)%State_VG(:,i,j,k) = &
                  State_VII(:,iStencil_III(1,iBuff,i),i) &
                  *Weight_III(1,iBuff,i) + &
                  State_VII(:,iStencil_III(2,iBuff,i),i) &
                  *Weight_III(2,iBuff,i) + &
                  State_VII(:,iStencil_III(3,ibuff,i),i) &
                  *Weight_III(3,iBuff,i)
          end do          ! i
       end do; end do    ! j,k
    end do       ! iBlock

    ! One extra layer passing through physical cell centers (i=1)
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE
       if(.not.IsAllocatedThread_B(iBlock))CYCLE
       do k = kMin_, kMax_; do j = jMin_, jMax_
          call state_mhd_to_thread(&
               State_V       = State_VGB(:, 1, j, k, iBlock), &
               Xyz_D         = Xyz_DGB(  :, 1, j, k, iBlock), &
               B0_D          = B0_DGB(   :, 1, j, k, iBlock), &
               StateThread_V = &
               BoundaryThreads_B(iBlock)%State_VG(:, 1, j, k))
       end do; end do
    end do
  contains
    !==========================================================================
    subroutine state_mhd_to_thread(State_V, Xyz_D, B0_D, StateThread_V)

      use ModPhysics, ONLY: No2Si_V, UnitTemperature_, &
           UnitEnergyDens_
      use ModVarIndexes, ONLY: Rho_, p_, Pe_, Bx_, Bz_, nVar
      use ModWaves, ONLY: WaveFirst_, WaveLast_
      use  ModTurbulence, ONLY:PoyntingFluxPerB
      !INPUT:
      ! MHD state vector
      real,    intent(in) :: State_V(nVar)
      ! Cartesian coords and B0 field to idenify major and minor waves
      real,    intent(in) :: Xyz_D(3), B0_D(3)
      !OUTPUT:
      ! Thread state vector
      real,    intent(out):: StateThread_V(PSi_:TiSi_)
      !LOCALS:
      ! Total magnetic field
      real :: BTotal_D(3)
      !------------------------------------------------------------------------
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
      StateThread_V(TeSi_) = TeFraction*State_V(iPe)/State_V(Rho_)        &
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
    !==========================================================================
  end subroutine save_threads_for_plot
  !============================================================================
  subroutine get_thread_point(nVar,State_VII,DoCoord)

    ! Calculate coordinates (lon, lat) of the intersection point of threads
    ! with the spherical surface at the first generalized coordinate value
    ! equal to input Coord1

    use BATL_lib, ONLY: nBlock, Unused_B
    use ModInterpolate, ONLY: linear
    real :: Coord1
    integer, intent(in) :: nVar
    real, intent(out) :: State_VII(nVar, nThreadAll,-nGUniform:0)
    logical,optional, intent(in) :: DoCoord
    integer :: i, j, k, iBlock, nPoint, iBuff
    integer, parameter:: Lon_ = 1, Lat_ = 2
    real    :: StateThread_VI(nVar, 1-nPointThreadMax:0)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_thread_point'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Start value for Buffer index numerating the points related to given PE
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
             nPoint = BoundaryThreads_B(iBlock)%nPoint_II(j,k)
             ! Fill in an array for this thread with lon,lat values
             ! of the grid points on the thread
             if(present(DoCoord))then
                StateThread_VI(Lon_:Lat_, 1 - nPoint:0) = &
                     BoundaryThreads_B(iBlock)%Coord_DIII(2:,1-nPoint:0,j,k)
             else
                ! Fill in an array with NeSi and TeSi values
                StateThread_VI(:, 1 - nPoint:0) = &
                     BoundaryThreads_B(iBlock)%State_VIII(:,1-nPoint:0,j,k)
                !  Use geometric average of the face value for
                !  the wave amplitude to get the cell-centered aplitude squared
                StateThread_VI(A2Major_, 1 - nPoint:0) = &
                     StateThread_VI(AMajor_, 1 - nPoint:0)*&
                     BoundaryThreads_B(iBlock) &
                     %State_VIII(AMajor_,-nPoint:-1,j,k)
                StateThread_VI(A2Minor_, 1 - nPoint:0) = &
                     StateThread_VI(AMinor_, 1 - nPoint:0)*&
                     BoundaryThreads_B(iBlock) &
                     %State_VIII(AMinor_,-nPoint:-1,j,k)
             end if
             do i = -nGUniform, 0
                ! Generalized radial coordinate of the plot grid point
                Coord1 = CoordMin_D(r_) + real(i)/dCoord1Inv
                ! Now we find the intersection point of the thread with the
                ! spherical surface at given radial gen coordinate, Coord1
                ! and interpolate the state vector to this point
                State_VII(:,iBuff,i)  = &
                     linear(&
                     a_VI =  StateThread_VI(:, 1 - nPoint:0),     &
                     nVar = nVar,                                 &
                     iMin = 1 - nPoint,                           &
                     iMax = 0,                                    &
                     x = Coord1,                                  &
                     x_I = BoundaryThreads_B(iBlock)              &
                     %Coord_DIII(r_,1-nPoint:0,j,k),              &
                     DoExtrapolate = .false.)
             end do ! i
          end do ! j
       end do ! k
    end do ! iBlock
    call broadcast_buffer(nVar=nVar, Buff_VII=State_VII)
    call test_stop(NameSub, DoTest)
  contains
    !==========================================================================
    subroutine broadcast_buffer(nVar, Buff_VII)

      use ModMpi
      integer, intent(in) :: nVar
      real, intent(inout) :: Buff_VII(nVar, nThreadAll,-nGUniform:0)
      integer             :: iBuff, iProcBCast, iError
      !------------------------------------------------------------------------
      iBuff = 0
      do iProcBCast = 0, nProc-1
         if(nThread_P(iProcBCast)==0)CYCLE
         call MPI_BCAST(Buff_VII(:, iBuff+1:iBuff+nThread_P(iProcBCast),:),&
              (nGUniform + 1)*nVar*nThread_P(iProcBCast), MPI_REAL, &
              iProcBCast, iComm, iError)
         iBuff = iBuff + nThread_P(iProcBCast)
      end do

    end subroutine broadcast_buffer
    !==========================================================================
  end subroutine get_thread_point
  !============================================================================
  subroutine get_tr_los_image(Xyz_D, DirLos_D, iBlock, nPlotVar, &
       NamePlotVar_V, iTableEuv, iTableSxr, iTableGen, PixIntensity_V,&
       DoTestIn)

    ! Using a tabulated analytical solution for the Transition Region (TR),
    ! the contribution to the LOS image from the TR is calculated.

    ! An intersection point of the LOS with the chromosphere top:
    real, intent(in)    :: Xyz_D(3)

    ! A unit vector in direction of the LOS:
    real, intent(in)    :: DirLos_D(3)
    !(Extended) block ID
    integer, intent(in) :: iBlock

    ! Number of the variables to plot:
    integer, intent(in) :: nPlotVar

    ! Names of the variables to plot
    character(len=20), intent(in) :: NamePlotVar_V(nPlotVar)

    ! Tables, in which the emissivity in different ion lines
    ! is tabulated:
    integer, intent(in) :: iTableEuv, iTableSxr, iTableGen

    logical, optional :: DoTestIn

    ! Logicals specifying the use of tables
    logical :: UseEuv, UseSxr, UseGenTable

    ! Pixel intensity to be corrected
    real, intent(inout) :: PixIntensity_V(nPlotVar)

    ! Local variables
    ! Magnetic field at the point XYZ_D, and its direction vector:
    real :: B0_D(3), DirB_D(3)

    ! Radial direction:
    real :: DirR_D(3)

    ! cosine of angles between the radial direction and the directions of
    ! LOS and magnetic field:
    real :: CosRLos, CosRB

    ! Euv intensities:
    real :: EuvValue_V(3)

    ! Sxr intensities:
    real :: SxrValue_V(2)

    ! Plasma parameters at the top of TR
    real :: TeSi, PTotSi, PeSi

    ! Contribution to the image
    real :: PlotVar_V(nPlotVar)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_tr_los_image'
    !--------------------------------------------------------------------------
    if(present(DoTestIn))then
       DoTest = DoTestIn
    else
       DoTest = .false.
    end if
    ! Radial direction:
    DirR_D = Xyz_D/norm2(Xyz_D)

    ! Cosine of angle between DirR and DirLos:
    CosRLos = abs(sum(DirR_D*DirLos_D))

    ! If the line is tangent to the solar surface, the intensity is too large
    ! to be corrected
    if(CosRLos <= 0.01)RETURN

    ! Magnetic field vector and its angle with the radial direction:
    call get_b0(Xyz_D, B0_D)
    DirB_D = B0_D/norm2(B0_D)
    CosRB = abs(sum(DirR_D*DirB_D))
    UseEuv = iTableEuv > 0
    UseSxr = iTableSxr > 0
    UseGenTable = iTableGen > 0

    call get_te_ptot(TeSi, PTotSi)

    ! Assiming equal electron and ion temperature
    ! Pe = Z Pi, pTotal = (Z+1)Pi
    ! So that Pi = pTotal/(Z+1) and
    PeSi = Z*PTotSi/(1 + Z)

    PlotVar_V = 0

    ! Integrate plot variables
    call set_plot_var

    ! Add contribution to the pixel intensity, account for
    ! the geometric factor
    PixIntensity_V = PixIntensity_V + PlotVar_V *(CosRB/CosRLos)

  contains
    !==========================================================================
    subroutine get_te_ptot(TeSi, PTotSi)

      use BATL_lib, ONLY: xyz_to_coord, CellSize_DB, CoordMin_DB
      use ModInterpolate, ONLY: interpolate_vector
      ! OUTPUT:
      real, intent(out) :: TeSi, PTotSi
      ! Coords of the point in which to interpolate
      real :: Coord_D(3), CoordNorm_D(2)
      real :: StateThread_V(PSi_:TiSi_)
      !------------------------------------------------------------------------
      call xyz_to_coord(Xyz_D, Coord_D)
      CoordNorm_D = (Coord_D(r_+1:) - CoordMin_DB(r_+1:,iBlock))/&
           CellSize_DB(r_+1:,iBlock) + 0.50

      ! Along radial coordinate the resolution and location of the grid
      ! may be different:

      ! Interpolate the state on threads to the given location
      StateThread_V = interpolate_vector(                                 &
           a_VC=BoundaryThreads_B(iBlock)%State_VG(:,-nGUniform,:,:),     &
           nVar=TiSi_,                                                    &
           nDim=2,                                                        &
           Min_D=[jMin_, kMin_],                                          &
           Max_D=[jMax_, kMax_],                                          &
           x_D=CoordNorm_D)
      PTotSi = StateThread_V( PSi_)
      TeSi   = StateThread_V(TeSi_)

    end subroutine get_te_ptot
    !==========================================================================
    subroutine set_plot_var

      integer :: iVar ! Loop variables

      ! Electron density in particles per cm3:
      !------------------------------------------------------------------------
      if(UseGenTable)then
         call integrate_emission(TeSi, PeSi, iTableGen, nPlotVar, PlotVar_V)
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
    !==========================================================================
  end subroutine get_tr_los_image
  !============================================================================
  subroutine set_ur_bot_sc(Var_IIB)
    real, intent(out) :: Var_IIB(nJ,nK, MaxBlock)
    !--------------------------------------------------------------------------
  end subroutine set_ur_bot_sc
  !============================================================================
  subroutine set_ur_top_tr(Var_IIB)
    real, intent(out) :: Var_IIB(nJ,nK, MaxBlock)
    !--------------------------------------------------------------------------
  end subroutine set_ur_top_tr
  !============================================================================
  subroutine set_u_top_tr(Var_IIB)
    real, intent(out) :: Var_IIB(nJ,nK, MaxBlock)
    !--------------------------------------------------------------------------
  end subroutine set_u_top_tr
  !============================================================================
  subroutine set_u_bot_tr(Var_IIB)
    real, intent(out) :: Var_IIB(nJ,nK, MaxBlock)
    !--------------------------------------------------------------------------
  end subroutine set_u_bot_tr
  !============================================================================
  subroutine set_u_min_tr(Var_IIB)
    real, intent(out) :: Var_IIB(nJ,nK, MaxBlock)
    !--------------------------------------------------------------------------
  end subroutine set_u_min_tr
  !============================================================================
  subroutine set_u_max_tr(Var_IIB)
    real, intent(out) :: Var_IIB(nJ,nK, MaxBlock)
    !--------------------------------------------------------------------------
  end subroutine set_u_max_tr
  !============================================================================
  subroutine save_plot_thread(iBlock, j, k)
    integer, intent(in) :: iBlock, j, k
    !--------------------------------------------------------------------------
  end subroutine save_plot_thread
  !============================================================================
end module ModFieldLineThread
!==============================================================================
