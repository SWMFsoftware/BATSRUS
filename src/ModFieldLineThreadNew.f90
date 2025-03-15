!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModFieldLineThread

  use ModBatsrusUtility, ONLY: stop_mpi
#ifdef _OPENACC
  use ModUtilities, ONLY: norm2
#endif
  use BATL_lib,      ONLY: &
       test_start, test_stop, jTest, kTest, iBlockTest, MaxDim, &
       iProc, nProc, iComm, nJ, nK, jDim_, kDim_, MaxBlock, CoordMin_D
  use ModAdvance,    ONLY: UseElectronPressure
  use ModMain,       ONLY: UseFieldLineThreads, DoThreads_B
  use ModB0,         ONLY: get_b0
  use ModPhysics,    ONLY: Z => AverageIonCharge
  use ModVarIndexes, ONLY: Pe_, p_, nVar
  use ModMultiFluid, ONLY: MassIon_I
  use ModTransitionRegion, ONLY: OpenThread, allocate_thread_arr, &
       deallocate_thread_arr, nPointThreadMax=>nPointMax, DsThreadMin=>Ds0, &
       rChromo, set_thread, integrate_emission, read_tr_param, &
       RhoTr_=>Rho_, WminorTr_=>Wminor_, advance_thread_expl

  implicit none
  SAVE

  PRIVATE ! Except
  ! Chromosphere top boundary
  public :: rChromo
  logical, public, allocatable:: IsAllocatedThread_B(:)

  ! In the boundary blocks the physical cells near the boundary are connected
  ! to the photosphere with these threads
  type BoundaryThreads
     ! The type of last update for the thread solution
     integer :: iAction
     real, pointer :: DirR_DII(:,:,:)
     integer, pointer :: iStencil_III(:,:,:)
     real, pointer :: Weight_III(:,:,:)
     ! State_V stored on data cube iMin:iMax,0:nJ+1,0:nK+1
     real, pointer :: State_VG(:,:,:,:)
     type(OpenThread), allocatable :: Threads_II(:,:)
  end type BoundaryThreads
  ! For visualization:
  ! Indexes for array State_VG(PSi_:TiSi_,-nGUniform:1,jMin_:jMax_,kMin_:kMax_)
  integer, parameter:: jMin_ = 1 - jDim_, jMax_ = nJ + jDim_
  integer, parameter:: kMin_ = 1 - kDim_, kMax_ = nK + kDim_

  ! To espress Te  and Ti in terms of P and rho, for ideal EOS:
  ! Te = TeFraction*State_V(iPe)/State_V(Rho_)
  ! Pe = PeFraction*State_V(iPe)
  ! Ti = TiFraction*State_V(p_)/State_V(Rho_)
  real    :: TeFraction, TiFraction, PeFraction
  integer :: iPe
  public  :: iPe, PeFraction

  type(BoundaryThreads), public, allocatable :: Threads_B(:)

  public :: nPointThreadMax

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

  public :: save_threads_for_plot    ! Get  State_VG array
  public :: interpolate_thread_state ! Interpolate state from State_VG
  public :: set_thread_plotvar       ! Plot variables for "shell" plots
  public :: get_tr_los_image         ! Correction for TR on LOS images
  public :: is_threaded_block        ! Mark blocks near internal boundary
  public :: beta_thread              ! Accounts for grid sizes in TR and SC
  public :: get_bc_from_sc           ! Get State_V from SC, set State_VG(:,0)
  public :: put_bc_to_sc             ! Fill in ghost cell from thread
  public :: relax_initial_state      ! Adjust state on created thread to BCs
  public :: advance_threaded_block_expl ! In multi-stage scheme, advance HD
  public :: thread_heat_flux          ! Conserved heat flux to/from thread
  ! Correspondent named indexes
  integer, public, parameter :: DoInit_=-1, Done_=0, Enthalpy_=1, Heat_=2, &
       Restart_=3
  ! Saves thread state into restart
  public :: save_thread_restart

  ! Number of threads on each processor
  integer, allocatable :: nThread_P(:)
  ! Transformation matrices
  real  :: State2Tr_VV(nVar,RhoTr_:WminorTr_)
  real  :: Face2Tr_VV(nVar,RhoTr_:WminorTr_)
  real  :: Tr2State_VV(RhoTr_:WminorTr_,nVar)
  real  :: Tr2Face_VV(RhoTr_:WminorTr_,nVar)
  logical :: IsInitialized = .false.
  integer, parameter :: r_ = 1
  character(len=100) :: NameRestartFile

contains
  !============================================================================
  subroutine init_threads
    use ModChromosphere,     ONLY: TeChromosphereSi
    use ModTransitionRegion, ONLY:  init_tr, UseChromoEvap
    use BATL_lib, ONLY: xyz_to_coord, coord_to_xyz
    integer :: iBlock ! Loop variable
    real :: CoordChromo_D(MaxDim), Xyz_D(MaxDim)
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_threads'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(IsInitialized)RETURN
    IsInitialized = .true.

    ! Initialize transition region model:
    call init_tr(zIn=Z, TeChromoSi = TeChromosphereSi, iComm=iComm)
    UseChromoEvap = .true.
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
    allocate(        DoThreads_B(MaxBlock))
    allocate(IsAllocatedThread_B(MaxBlock))
    DoThreads_B = .false.
    IsAllocatedThread_B = .false.
    allocate(nThread_P(0:nProc - 1))
    nThread_P = 0
    allocate(Threads_B(1:MaxBlock))
    do iBlock = 1, MaxBlock
       call nullify_thread_b(iBlock)
    end do
    call set_transform_matrices
    call test_stop(NameSub, DoTest)
  contains
    !==========================================================================
    subroutine set_transform_matrices

      use ModVarIndexes, ONLY: Rho_, RhoUx_, RhoUz_, P_, Pe_, Ppar_, &
           WaveFirst_, WaveLast_, Ux_, Uz_
      use ModPhysics,    ONLY: No2Si_V, UnitRho_, UnitRhoU_, UnitP_, Si2No_V
      use ModTransitionRegion, ONLY: Utr_=>U_, RhoUtr_=>RhoU_, &
           Ptr_=>P_, PeTr_=>Pe_, PparTr_=>Ppar_, PeParTr_=>PePar_,   &
           PperpTr_=>Pperp_, PePerpTr_=>PePerp_, WmajorTr_=>Wmajor_
      use ModAdvance, ONLY: UseAnisoPressure
      use ModWaves,   ONLY: UseAwRepresentative
      real, parameter :: cThird = 1.0/3.0, cTwoThird = 2.0/3.0
      !------------------------------------------------------------------------
      State2Tr_VV(1:nVar,RhoTr_:WminorTr_) = 0.0
      Face2Tr_VV(1:nVar,RhoTr_:WminorTr_) = 0.0
      Tr2State_VV(RhoTr_:WminorTr_,1:nVar) = 0.0
      Tr2Face_VV(RhoTr_:WminorTr_,1:nVar) = 0.0
      ! Density conversion
      State2Tr_VV(Rho_,RhoTr_) = No2Si_V(UnitRho_)
      Face2Tr_VV(Rho_,RhoTr_) = No2Si_V(UnitRho_)
      Tr2State_VV(RhoTr_,Rho_) = Si2No_V(UnitRho_)
      Tr2Face_VV(RhoTr_,Rho_) = Si2No_V(UnitRho_)
      if(UseElectronPressure)then
         if(UseAnisoPressure)then
            ! Ppar conversion
            State2Tr_VV(Ppar_,PparTr_) = No2Si_V(UnitP_)
            Tr2State_VV(PparTr_,Ppar_) = Si2No_V(UnitP_)
            Face2Tr_VV(Ppar_,PparTr_)  = No2Si_V(UnitP_)
            Tr2Face_VV(PparTr_,Ppar_) = Si2No_V(UnitP_)
            ! Pperp in terms of P and Ppar
            Face2Tr_VV(Ppar_,PperpTr_) = -0.50*No2Si_V(UnitP_)
            Face2Tr_VV(P_,PperpTr_) = 1.50*No2Si_V(UnitP_)
         else
            ! Both Ppar and, if needed Pperp is expressed in terms of P
            State2Tr_VV(P_,PparTr_) = No2Si_V(UnitP_)
            Face2Tr_VV(P_,PparTr_) =  No2Si_V(UnitP_)
            Face2Tr_VV(P_,PperpTr_) =  No2Si_V(UnitP_)
         end if
         State2Tr_VV(P_,Ptr_) = No2Si_V(UnitP_)
         Tr2State_VV(Ptr_,P_) = Si2No_V(UnitP_)
         State2Tr_VV(Pe_,PeParTr_) = No2Si_V(UnitP_)
         State2Tr_VV(Pe_,PeTr_) = No2Si_V(UnitP_)
         Tr2State_VV(PeTr_,Pe_) = Si2No_V(UnitP_)
         Face2Tr_VV(Pe_,PeParTr_) = No2Si_V(UnitP_)
         Face2Tr_VV(Pe_,PePerpTr_) = No2Si_V(UnitP_)
         Tr2Face_VV(PparTr_,P_) = cThird*Si2No_V(UnitP_)
         Tr2Face_VV(PperpTr_,P_) = cTwoThird*Si2No_V(UnitP_)
         Tr2Face_VV(PeParTr_,Pe_) = cThird*Si2No_V(UnitP_)
         Tr2Face_VV(PePerpTr_,Pe_) = cTwoThird*Si2No_V(UnitP_)
      else
         State2Tr_VV(P_,PparTr_) = (1 - PeFraction)*No2Si_V(UnitP_)
         State2Tr_VV(P_,Ptr_) = (1 - PeFraction)*No2Si_V(UnitP_)
         State2Tr_VV(P_,PeParTr_) =  PeFraction*No2Si_V(UnitP_)
         State2Tr_VV(P_,PeTr_) = PeFraction*No2Si_V(UnitP_)
         Tr2State_VV(PeTr_,P_) = Si2No_V(UnitP_)
         Tr2State_VV(Ptr_,P_) = Si2No_V(UnitP_)
         Face2Tr_VV(P_,PparTr_) = (1 - PeFraction)*No2Si_V(UnitP_)
         Face2Tr_VV(P_,PperpTr_) = (1 - PeFraction)*No2Si_V(UnitP_)
         Face2Tr_VV(P_,PeParTr_) =  PeFraction*No2Si_V(UnitP_)
         Face2Tr_VV(P_,PePerpTr_) = PeFraction*No2Si_V(UnitP_)
         Tr2Face_VV(PparTr_,P_) = cThird*Si2No_V(UnitP_)
         Tr2Face_VV(PperpTr_,P_) = cTwoThird*Si2No_V(UnitP_)
         Tr2Face_VV(PeParTr_,P_) = cThird*Si2No_V(UnitP_)
         Tr2Face_VV(PePerpTr_,P_) = cTwoThird*Si2No_V(UnitP_)
      end if
      if(UseAwRepresentative)then
         ! Wave amplitudes in SC are dimensionless
         State2Tr_VV(WaveFirst_,WmajorTr_) = 1.0
         State2Tr_VV(WaveLast_,WminorTr_) = 1.0
         Face2Tr_VV(WaveFirst_,WmajorTr_) = 1.0
         Face2Tr_VV(WaveLast_,WminorTr_) = 1.0
      else
         State2Tr_VV(WaveFirst_,WmajorTr_) = No2Si_V(UnitP_)
         State2Tr_VV(WaveLast_,WminorTr_) = No2Si_V(UnitP_)
         Face2Tr_VV(WaveFirst_,WmajorTr_) = No2Si_V(UnitP_)
         Face2Tr_VV(WaveLast_,WminorTr_) = No2Si_V(UnitP_)
      end if
      Tr2State_VV(WmajorTr_,WaveFirst_) = 1.0
      Tr2State_VV(WminorTr_,WaveLast_) = 1.0
      Tr2Face_VV(WmajorTr_,WaveFirst_) = 1.0
      Tr2Face_VV(WminorTr_,WaveLast_) = 1.0

    end subroutine set_transform_matrices
    !==========================================================================
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

    ! With the actual location of the ghost cell center at
    ! (Coord_D(r_,-1) + Coord_D(r_,0))/2 the interpolated value at the face
    ! is not (True + Ghost)/2, but True + (Ghost - True)*0.5*CellSize_D(r_)/&
    ! (0.5*CellSize_D(r_) + 0.5*(Coord_D(r_,0) - Coord_D(r_,1)). To achieve
    ! the desired accuracy of interpolation, the slope Ghost - True is
    ! multiplied by 2/beta_thread. The half of thus modified slope plus True
    ! gives the second order approximation on face
    use BATL_lib, ONLY: CellSize_DB
    integer, intent(in) :: j,k, iBlock
    !--------------------------------------------------------------------------
    beta_thread = 1.0 + (&
         Threads_B(iBlock)%Threads_II(j,k)%Coord_DF(r_,0) - &
         Threads_B(iBlock)%Threads_II(j,k)%Coord_DF(r_,-1))/&
         CellSize_DB(r_,iBlock)

  end function beta_thread
  !============================================================================
  subroutine thread_heat_flux(iBlock, HeatFlux_II)

    use ModPhysics, ONLY: Si2No_V, UnitPoynting_
    use ModTransitionRegion, ONLY: HeatCondParSi
    integer, intent(in) :: iBlock
    real, intent(inout) :: HeatFlux_II(1:nJ,1:nK)
    integer :: j, k
    real, parameter :: cTwoSeventh = 2.0/7.0
    !--------------------------------------------------------------------------
    do k = 1, nK; do j = 1, nJ
       HeatFlux_II(j,k) = heat_flux(Threads_B(iBlock)%Threads_II(j,k))
    end do; end do
    ! Common multiplier
    HeatFlux_II = cTwoSeventh*HeatCondParSi*Si2No_V(UnitPoynting_)*&
         HeatFlux_II
  contains
    !==========================================================================
    real function heat_flux(Thread)
      type(OpenThread), intent(in) :: Thread
      !------------------------------------------------------------------------
      heat_flux = abs(Thread%OpenFlux)/Thread%B_F(0) *& ! Face area
           (Thread%Te_G(-1)**3.50 - Thread%Te_G(0)**3.50)/& ! Diff in cons
           (0.50*Thread%Ds_G(-1) + Thread%Ds_G(0))
    end function heat_flux
    !==========================================================================
  end subroutine thread_heat_flux
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
       call read_tr_param('#CHROMOEVAPORATION')
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
    nullify(Threads_B(iBlock)%DirR_DII)
    nullify(Threads_B(iBlock)%iStencil_III)
    nullify(Threads_B(iBlock)%Weight_III)
    nullify(Threads_B(iBlock)%State_VG)
    Threads_B(iBlock)%iAction    = Done_
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
    deallocate(Threads_B(iBlock)%DirR_DII)
    deallocate(Threads_B(iBlock)%iStencil_III)
    deallocate(Threads_B(iBlock)%Weight_III)
    deallocate(Threads_B(iBlock)%State_VG)
    call deallocate_thread_arr(&
         Threads_B(iBlock)%Threads_II, nJ, nK)
    IsAllocatedThread_B(iBlock) = .false.
    call nullify_thread_b(iBlock)
    call test_stop(NameSub, DoTest, iBlock)

  end subroutine deallocate_thread_b
  !============================================================================
  subroutine get_field(Xyz_D, B_D, B1_D)
    use EEE_ModCommonVariables, ONLY: UseCme
    use EEE_ModMain, ONLY: EEE_get_state_BC
    use ModPhysics,  ONLY: No2Si_V, UnitB_
    use ModMain,     ONLY: nStep, nIteration, tSimulation
    real, intent(in)  :: Xyz_D(MaxDim)
    real, intent(out) :: B_D(MaxDim)
    real, optional, intent(out) :: B1_D(MaxDim)
    ! CME parameters, if needed
    real:: RhoCme, Ucme_D(MaxDim), Bcme_D(MaxDim), pCme
    !--------------------------------------------------------------------------
    call get_b0(Xyz_D, B_D)
    B_D = B_D*No2Si_V(UnitB_)
    if(.not.UseCme)then
       if(present(B1_D))B1_D = 0.0
       RETURN
    end if
    call EEE_get_state_BC(Xyz_D, RhoCme, Ucme_D, Bcme_D, pCme, &
         tSimulation, nStep, nIteration)
    B_D = B_D + Bcme_D
    if(present(B1_D))B1_D = Bcme_D
  end subroutine get_field
  !============================================================================
  subroutine set_threads(NameCaller)

    use BATL_lib,     ONLY: MaxBlock, Unused_B, nBlock
    use ModMain,     ONLY: nStep
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

          allocate(Threads_B(iBlock)%DirR_DII(MaxDim,&
               1:nJ, 1:nK))

          allocate(Threads_B(iBlock)%iStencil_III(2:3,&
               1:nJ, 1:nK))

          allocate(Threads_B(iBlock)%Weight_III(MaxDim,&
               1:nJ, 1:nK))

          allocate(Threads_B(iBlock)%State_VG(&
               nVar, -nGUniform:1,jMin_:jMax_, kMin_:kMax_))

          call allocate_thread_arr(&
               Threads_B(iBlock)%Threads_II, nJ, nK)

          IsAllocatedThread_B(iBlock) = .true.
       end if
       ! The threads are now set in a just created block, or
       ! on updating B_0 field
       call set_threads_b(iBlock)
       nBlockSet = nBlockSet + 1
       do k = 1, nK; do j = 1, nJ
          nPointMin = min(nPointMin, &
               Threads_B(iBlock)%Threads_II(j,k)%nCell)
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
       if(nBlockSetAll > 0.and.iProc==0)then
          write(*,*)'Set threads in ',nBlockSetAll,' blocks on iteration ', &
               nStep, ' is called from '//NameCaller
          write(*,*)'nPointMin = ',nPointMinAll
          write(*,*)'dCoord1Uniform =', 1/dCoord1Inv
          if(nProc<=8)&
               write(*,*)'Number of threads on different PEs: ', nThread_P
       end if
       call set_triangulation
    end if
    call test_stop(NameSub, DoTest)
  end subroutine set_threads
  !============================================================================
  subroutine set_threads_b(iBlock)

    use ModPhysics,  ONLY: No2Si_V, UnitX_
    use ModMain,       ONLY: DoThreadRestart
    use ModGeometry, ONLY: Xyz_DGB
    use ModCoordTransform, ONLY: rot_xyz_rlonlat
    use BATL_lib,    ONLY: xyz_to_coord, coord_to_xyz, CoordMin_DB, &
         CellSize_DB, FaceNormal_DDFB
    integer, intent(in) :: iBlock
    ! Locals:
    ! Loop variable: (j,k) enumerate the cells at which
    ! the threads starts, iPoint starts from negative
    ! values at the photospheric end and the maximal
    ! value of this index is 0 for the thread point
    ! at the physical cell center.
    integer :: j, k, j1, k1

    ! coordinates, field vector and modulus
    real :: Xyz_D(MaxDim), Coord_D(MaxDim)
    ! Conversion matrix and r-lon-lat bector of direction
    real :: XyzRlonlat_DD(MaxDim,MaxDim), bRlonlat_D(MaxDim)
    ! Mesh size along r, Lon, Lat
    real :: Dr, DsLon, DsLat, Weight_I(MaxDim)
    ! Misc:
    real :: Aux
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_threads_b'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! Initialize threads
    Threads_B(iBlock)%iAction = DoInit_

    ! Loop over the thread starting points
    do k = 1, nK; do j = 1, nJ
       ! Face center
       Coord_D = CoordMin_DB(:,iBlock) +  [0.0, &
            (j - 0.50)*CellSize_DB(2,iBlock),    &
            (k - 0.50)*CellSize_DB(3,iBlock)]
       call coord_to_xyz(Coord_D, Xyz_D)
       call set_thread(&
            XyzIn_D = Xyz_D,&
            FaceArea = norm2(FaceNormal_DDFB(:,1,1,j,k,iBlock)), &
            OpenThread1 = Threads_B(iBlock)%Threads_II(j,k), &
            xyz_to_coord = xyz_to_coord,&
            get_field = get_field)
       XyzRlonlat_DD =  rot_xyz_rlonlat(Coord_D(2), Coord_D(3))
       bRlonlat_D = matmul(&
            Threads_B(iBlock)%Threads_II(j,k)%DirB_DG(:,0), &
            XyzRlonlat_DD)
       ! Distance to the nearest cell center
       Dr = norm2(Xyz_DGB(:, 1, j, k, iBlock) - Xyz_D)
       j1 = j + nint(sign(1.0,bRlonlat_D(2)))
       ! Distance from the first stencil point to the second one
       DsLon = norm2(Xyz_DGB(:, 1, j1, k, iBlock) - &
            Xyz_DGB(:, 1, j, k, iBlock))
       k1 = k + nint(sign(1.0,bRlonlat_D(3)))
       ! Distance from the first stencil point to the third one
       DsLat = norm2(Xyz_DGB(:, 1, j, k1, iBlock) - &
            Xyz_DGB(:, 1, j, k, iBlock))
       Weight_I(2:3) = abs(bRlonlat_D(2:3))/[DsLon, DsLat]
       Aux = max(bRlonlat_D(1)/Dr, Weight_I(2) + Weight_I(3))
       Weight_I(2:3) = Weight_I(2:3)/Aux
       Weight_I(1) = max(1 - Weight_I(2) - Weight_I(3), 0.0)
       Threads_B(iBlock)%Weight_III(:,j, k) = Weight_I
       Threads_B(iBlock)%iStencil_III(:,j, k) = [j1, k1]
       ! Assign the distance from the intersection point of the field line
       ! with the i=1 coordinate plane to the face center
       Threads_B(iBlock)%Threads_II(j,k)%Ds_G(0) = &
            norm2(Xyz_DGB(:,1,j,k,iBlock)*Weight_I(1) + &
            Xyz_DGB(:,1,j1,k,iBlock)*Weight_I(2) +      &
            Xyz_DGB(:,1,j,k1,iBlock)*Weight_I(3) - Xyz_D)*No2Si_V(UnitX_)
       Threads_B(iBlock)%DirR_DII(:,j,k) = Xyz_D/norm2(Xyz_D)
    end do; end do
    if(DoThreadRestart)call read_thread_restart(iBlock)
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
       if(.not.is_threaded_block(iBlock))then
          if(.not.IsAllocatedThread_B(iBlock))then
             CYCLE
          else
             call stop_mpi('Threads are at block not near inner boundary')
          end if
       else
          if(.not.IsAllocatedThread_B(iBlock))&
               call stop_mpi('No threads at the block near inner boundary')
       end if
       if(Threads_B(iBlock)%iAction /= Done_)&
            call stop_mpi('An attempt to readvance not advanced threads')
       Threads_B(iBlock)%iAction = iAction
    end do

    call test_stop(NameSub, DoTest)

  end subroutine advance_threads
  !============================================================================
  subroutine advance_threaded_block_expl(iBlock, iStage, &
       RightState_VII, LeftState_VII, DtIn)

    use ModWaves,        ONLY: WaveFirst_, WaveLast_, UseAwRepresentative
    use ModTransitionRegion, ONLY: Utr_=>U_, WmajorTr_=>Wmajor_
    use ModTurbulence,   ONLY: PoyntingFluxPerB, PoyntingFluxPerBsi
    use ModConst,        ONLY: cMu
    use ModPhysics,      ONLY: No2Si_V, Si2No_V, UnitU_, UnitB_
    use ModVarIndexes,   ONLY: Ux_, Uz_, Bx_, Bz_, Rho_

    integer, intent(in) :: iBlock, iStage
    real, intent(in)    :: RightState_VII(nVar, 1:nJ, 1:nK)
    real, intent(inout) :: LeftState_VII(nVar, 1:nJ, 1:nK)
    real, optional, intent(in) :: DtIn

    ! Loop variables
    integer :: j, k
    ! Direction of magnetic field at the boundary
    real :: DirB_D(MaxDim)
    ! Radial direction
    real :: DirR_D(MaxDim)
    ! Face values in the TR model
    real :: RightFace0_V(RhoTr_:WminorTr_)
    real :: LeftFace0_V(RhoTr_:WminorTr_)
    ! Reusable sign of Br and sqrt(Rho)
    real :: SignBr, SqrtRho
    real :: Elsasser_D(MaxDim)
    !--------------------------------------------------------------------------

    do k = 1, nK; do j = 1, nJ
       DirB_D = Threads_B(iBlock)%Threads_II(j,k)%DirB_DG(:,0)
       Face2Tr_VV(Ux_:Uz_,Utr_) = DirB_D*No2Si_V(UnitU_)
       SignBr = sign(1.0, &
            Threads_B(iBlock)%Threads_II(j,k)%OpenFlux)
       RightFace0_V = matmul(RightState_VII(:, j, k),Face2Tr_VV)
       if(SignBr < 0)&
            RightFace0_V(WmajorTr_:WminorTr_) = &
            RightFace0_V( [WminorTr_, WminorTr_] )
       if(.not.UseAwRepresentative)&
            RightFace0_V(WmajorTr_:WminorTr_) = &
            RightFace0_V(WmajorTr_:WminorTr_)/&
            (PoyntingFluxPerBSi*sqrt(cMu*RightFace0_V(RhoTr_)))
       if(present(DtIn))then
          call advance_thread_expl(        &
               iStage = iStage,            &
               OpenThread1 = Threads_B(iBlock)%Threads_II(j,k), &
               IsTimeAccurate = .true.,    &
               RightFace0_V = RightFace0_V,&
               LeftFace0_V = LeftFace0_V,  &
               DtIn = DtIn                 )
       else
          call advance_thread_expl(        &
               iStage = iStage,            &
               OpenThread1 = Threads_B(iBlock)%Threads_II(j,k), &
               IsTimeAccurate = .false.,   &
               RightFace0_V = RightFace0_V,&
               LeftFace0_V = LeftFace0_V   )
       end if
       if(SignBr < 0)&
            LeftFace0_V(WmajorTr_:WminorTr_) = &
            LeftFace0_V( [WminorTr_, WminorTr_] )
       Tr2Face_VV(Utr_,Ux_:Uz_) = DirB_D*Si2No_V(UnitU_)
       LeftState_VII(:,j,k) = matmul(LeftFace0_V,Tr2Face_VV)
       LeftState_VII(Bx_:Bz_,j,k) = Si2No_V(UnitB_)* &
            Threads_B(iBlock)%Threads_II(j,k)%B1_DG(:,0)
       SqrtRho = sqrt(LeftState_VII(Rho_,j,k))
       if(.not.UseAwRepresentative)&
            LeftState_VII(WaveFirst_:WaveLast_,j,k) = &
            LeftState_VII(WaveFirst_:WaveLast_,j,k)*SqrtRho*&
            PoyntingFluxPerB
       ! Characteristic boundary condition:
       ! Elsasser variable for waves propagating down
       Elsasser_D = (RightState_VII(Bx_:Bz_,j,k) - LeftState_VII(Bx_:Bz_,j,k)&
            )*SignBr/SqrtRho + &
            RightState_VII(Ux_:Uz_,j,k) - LeftState_VII(Ux_:Uz_,j,k)
       DirR_D = Threads_B(iBlock)%DirR_DII(:,j,k)
       ! Nullify the vertical component
       Elsasser_D = Elsasser_D - sum(DirR_D*Elsasser_D)*DirR_D
       ! Keep only perturbation propagating downward, nullify that one
       ! propagating upward. Solve the perturbation of the left BC:
       LeftState_VII(Ux_:Uz_,j,k) = LeftState_VII(Ux_:Uz_,j,k) + &
            0.5*Elsasser_D
       LeftState_VII(Bx_:Bz_,j,k) = LeftState_VII(Bx_:Bz_,j,k) + &
            0.5*Elsasser_D*SignBr*SqrtRho
    end do; end do
  end subroutine advance_threaded_block_expl
  !============================================================================
  subroutine relax_initial_state(OpenThread1)

    use ModTransitionRegion, ONLY: advance_thread_semi_impl
    type(OpenThread), intent(inout) :: OpenThread1
    ! Loop variable
    integer :: iIter
    !--------------------------------------------------------------------------
    do iIter = 1, OpenThread1%nCell
       call advance_thread_expl(1, OpenThread1, IsTimeAccurate=.false.)
       call advance_thread_expl(2, OpenThread1, IsTimeAccurate=.false.)
       call advance_thread_semi_impl(OpenThread1)
    end do
  end subroutine relax_initial_state
  !============================================================================
  subroutine get_bc_from_sc(State_V, OpenThread1)

    use ModWaves,        ONLY: WaveFirst_, WaveLast_, UseAwRepresentative
    use ModTransitionRegion, ONLY: Utr_=>U_, RhoUtr_=>RhoU_, &
         WmajorTr_=>Wmajor_
    use ModTurbulence,   ONLY: PoyntingFluxPerBsi
    use ModConst,        ONLY: cMu
    use ModPhysics,      ONLY: No2Si_V, UnitRhoU_, UnitTemperature_
    use ModVarIndexes,   ONLY: RhoUx_, RhoUz_, Bx_, Bz_, Rho_
    real, intent(in) :: State_V(nVar)
    type(OpenThread), intent(inout) :: OpenThread1
    real :: StateTr_V(WminorTr_)
    !--------------------------------------------------------------------------
    State2Tr_VV(RhoUx_:RhoUz_,RhoUtr_) = OpenThread1%DirB_DG(:,0)*&
         No2Si_V(UnitRhoU_)
    StateTr_V = matmul(State_V,State2Tr_VV)
    StateTr_V(Utr_) = StateTr_V(RhoUtr_)/StateTr_V(RhoTr_)
    if(OpenThread1%OpenFlux < 0)&
         StateTr_V(WmajorTr_:WminorTr_) = &
         StateTr_V( [WminorTr_, WminorTr_] )
    if(.not.UseAwRepresentative)&
         StateTr_V(WmajorTr_:WminorTr_) = StateTr_V(WmajorTr_:WminorTr_)/&
         (PoyntingFluxPerBSi*sqrt(cMu*StateTr_V(RhoTr_)))
    OpenThread1%State_VG(:, 0) = StateTr_V
    OpenThread1%Te_G(0) = TeFraction*State_V(iPe) / State_V(Rho_)*&
         No2Si_V(UnitTemperature_)
  end subroutine get_bc_from_sc
  !============================================================================
  subroutine put_bc_to_sc(TrueState_V, OpenThread1, DirR_D, GhostState_V)

    use ModWaves,        ONLY: WaveFirst_, WaveLast_, UseAwRepresentative
    use ModTransitionRegion, ONLY: Utr_=>U_
    use ModTurbulence,   ONLY: PoyntingFluxPerB
    use ModPhysics,      ONLY: Si2No_V, UnitU_, UnitB_
    use ModVarIndexes,   ONLY: RhoUx_, RhoUz_, Ux_, Uz_, Bx_, Bz_, Rho_
    real, intent(in)  :: TrueState_V(nVar)
    type(OpenThread), intent(in) :: OpenThread1
    real, intent(in)  :: DirR_D(MaxDim)
    real, intent(out) :: GhostState_V(nVar)
    ! Reusable sign of Br and sqrt(Rho)
    real :: SignBr, SqrtRho
    real :: Elsasser_D(MaxDim)
    !--------------------------------------------------------------------------
    Tr2State_VV(Utr_,Ux_:Uz_) = OpenThread1%DirB_DG(:,-1)*Si2No_V(UnitU_)
    GhostState_V = matmul(OpenThread1%State_VG(:,-1),Tr2State_VV)
    SignBr = sign(1.0, OpenThread1%OpenFlux)
    if(SignBr < 0.0)&
         GhostState_V(WaveFirst_:WaveLast_) = &
         GhostState_V(WaveLast_:WaveFirst_:-1)
    SqrtRho = sqrt(GhostState_V(Rho_))
    if(.not.UseAwRepresentative)&
         GhostState_V(WaveFirst_:WaveLast_) = &
         GhostState_V(WaveFirst_:WaveLast_)*PoyntingFluxPerB*SqrtRho
    GhostState_V(Bx_:Bz_) = OpenThread1%B1_DG(:,-1)*Si2No_V(UnitB_)
    ! Characteristic boundary condition:
    ! Elsasser variable for waves propagating down
    Elsasser_D = (TrueState_V(Bx_:Bz_) - GhostState_V(Bx_:Bz_))*    &
         SignBr/SqrtRho + TrueState_V(RhoUx_:RhoUz_)/TrueState_V(Rho_) &
         - GhostState_V(Ux_:Uz_)
    ! Nullify the vertical component
    Elsasser_D = Elsasser_D - sum(DirR_D*Elsasser_D)*DirR_D
    ! Keep only perturbation propagating downward, nullify that one
    ! propagating upward. Solve the perturbation of the left BC:
    GhostState_V(Ux_:Uz_) = GhostState_V(Ux_:Uz_) + 0.5*Elsasser_D
    GhostState_V(Bx_:Bz_) = GhostState_V(Bx_:Bz_) + 0.5*Elsasser_D*&
            SignBr*SqrtRho
    GhostState_V(RhoUx_:RhoUz_) = GhostState_V(Ux_:Uz_)*GhostState_V(Rho_)
  end subroutine put_bc_to_sc
  !============================================================================
  subroutine read_thread_restart(iBlock)

    use ModMain,       ONLY: NameThisComp
    use ModConst,      ONLY: cProtonMass, cBoltzmann
    use ModIoUnit,     ONLY: UnitTmp_
    use ModUtilities,  ONLY: open_file, close_file
    use ModVarIndexes, ONLY: Rho_
    use ModTransitionRegion, ONLY: PeTr_=>Pe_, RhoTr_=>Rho_
    integer, intent(in) :: iBlock
    ! loop variables
    integer :: j, k
    ! Misc:
    integer :: nCell, iError
    real    :: RealNCell
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_thread_restart'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    call get_restart_file_name(iBlock, NameThisComp//'/restartIN/')
    call open_file(file=NameRestartFile, status='old', &
         form='UNFORMATTED', NameCaller=NameSub)
    do k = 1, nK; do j = 1, nJ
       read(UnitTmp_, iostat = iError)RealNCell
       if(iError>0)then
          write(*,*)'Error in reading nCell in Block=', iBlock
          call close_file
          RETURN
       end if
       nCell = nint(RealNCell)
       if(Threads_B(iBlock)%Threads_II(j,k)%nCell/=nCell)then
          write(*,*)'Incorrect nCell in Block=', iBlock
          call close_file
          RETURN
       end if
       read(UnitTmp_, iostat = iError) &
            Threads_B(iBlock)%Threads_II(j,k)%State_VG(:,-nCell:-1)
       Threads_B(iBlock)%Threads_II(j,k)%Te_G(-nCell:-1) = &
            Threads_B(iBlock)%Threads_II(j,k)%State_VG(&
            PeTr_,-nCell:-1)*cProtonMass/(cBoltzmann*&
            Threads_B(iBlock)%Threads_II(j,k)%State_VG(&
            RhoTr_,-nCell:-1))
    end do; end do
    call close_file
    Threads_B(iBlock)%iAction = Restart_
    call test_stop(NameSub, DoTest, iBlock)

  end subroutine read_thread_restart
  !============================================================================
  subroutine interpolate_thread_state(Coord_D, iBlock, State_V, DoTestIn)
    ! Interpolate the state at the point with coords Coord_D from
    ! Threads_B(iBlock)%State_VG(:,:,:)
    ! array, then convert to MHD
    use ModAdvance,     ONLY: nVar
    use BATL_lib,       ONLY: CoordMin_DB, CellSize_DB
    use ModInterpolate, ONLY: interpolate_vector

    ! Coords of the point in which to interpolate
    real,    intent(in) :: Coord_D(MaxDim)

    ! Block at which the grid is allocated
    integer, intent(in) :: iBlock

    ! Interpolated state vector
    real,    intent(out):: State_V(nVar)
    Logical, optional, intent(in) :: DoTestIn
    real                :: CoordNorm_D(MaxDim)

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
       State_V = interpolate_vector(                              &
            a_VC=Threads_B(iBlock)%State_VG(:,0:1,:,:),           &
            nVar=nVar,                                            &
            nDim=3,                                               &
            Min_D=[0, jMin_, kMin_],                              &
            Max_D=[1, jMax_, kMax_],                              &
            x_D=CoordNorm_D,                                      &
            DoExtrapolate=.false.                                 )
    else
       CoordNorm_D(r_) = (Coord_D(r_) - CoordMin_DB(r_,iBlock))*&
            dCoord1Inv
       ! Interpolate the state on threads to the given location
       State_V = interpolate_vector(                              &
            a_VC=Threads_B(iBlock)%State_VG(:,            &
            -nGUniform:0,:,:),                                    &
            nVar=nVar,                                            &
            nDim=3,                                               &
            Min_D=[-nGUniform, jMin_, kMin_],                     &
            Max_D=[0, jMax_, kMax_],                              &
            x_D=CoordNorm_D,                                      &
            DoExtrapolate=.false.                                 )
    end if
    if(DoTest)write(*,'(a,100es14.6)')NameSub//': State_V=',&
         State_V(:)
  end subroutine interpolate_thread_state
  !============================================================================
  subroutine set_thread_plotvar(iBlock, nPlotVar, NamePlotVar_V, Xyz_D, &
       State_V, PlotVar_V)

    use ModMain
    use ModVarIndexes
    use ModAdvance, ONLY : UseElectronPressure, &
         UseMultiSpecies
    use ModGeometry
    use ModPhysics,       ONLY: OmegaBody,  &
         ElectronPressureRatio, InvGammaMinus1_I, Si2No_V, UnitB_
    use ModUtilities,     ONLY: lower_case
    use ModIO,            ONLY: NameVarUserTec_I, NameUnitUserTec_I, &
         NameUnitUserIdl_I
    use ModMultiFluid,    ONLY: extract_fluid_name,      &
         UseMultiIon, nIonFluid, iPpar, iPFluid=>iP,   &
         IsMhd, iRho, iRhoUx, iRhoUy, iRhoUz, iRhoIon_I
    use ModCoordTransform, ONLY: cross_product
    use BATL_lib,          ONLY: iNode_B, CellSize_DB
    use ModB0,             ONLY: get_b0
    use ModWaves,          ONLY: UseWavePressure
    use ModTurbulence,   ONLY: PoyntingFluxPerB

    integer, intent(in) :: iBlock, nPlotVar
    character(LEN=20)   :: NamePlotVar_V(nPlotVar)
    real,    intent(in) :: Xyz_D(MaxDim)
    real, intent(inout) :: State_V(nVar)
    real,   intent(out) :: PlotVar_V(nPlotVar)

    ! To calculate B0 and BFull, if needed
    real :: B0_D(MaxDim) = 0.0, FullB_D(MaxDim)
    character (len=10)  :: String, NamePlotVar

    real:: Tmp1Var, Tmp2Var

    integer :: iVar, jVar, iIon, iFluid
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_thread_plotvar'
    !--------------------------------------------------------------------------
#ifndef SCALAR
    call test_start(NameSub, DoTest, iBlock)
    ! Calculate B0 and BFull
    B0_D = 0.0
    if(UseB0)call get_b0(Xyz_D, B0_D)
    FullB_D = State_V(Bx_:Bz_) + B0_D

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
#endif
  end subroutine set_thread_plotvar
  !============================================================================
  subroutine save_thread_restart

    use ModMain,       ONLY: NameThisComp
    use BATL_lib, ONLY: nBlock, Unused_B
    use ModIoUnit,     ONLY: UnitTmp_
    use ModUtilities,  ONLY: open_file, close_file
    integer :: j, k, iBlock, nCell
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
          nCell = Threads_B(iBlock)%Threads_II(j,k)%nCell
          write(UnitTmp_)real(nCell)
          write(UnitTmp_)&
               Threads_B(iBlock)%Threads_II(j,k)%&
               State_VG(:,-nCell:-1)
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
  !============================================================================
  subroutine set_triangulation

    ! triangulation

    use BATL_lib,               ONLY: nBlock, Unused_B, &
         CoordMin_DB, CellSize_DB, x_, y_, z_
    use ModTriangulateSpherical, ONLY:trans, trmesh, find_triangle_sph, &
         find_triangle_orig
    use ModCoordTransform,      ONLY: rlonlat_to_xyz
    use ModMpi

    integer :: i, j, k, iBlock, iBuff, iError

    ! Coordinates and state vector at the point of intersection of thread with
    ! the spherical coordinate  surface of the grid for plotting
    integer, parameter :: Lon_ = 1, Lat_=2
    integer :: iPE  ! Loop variable
    integer :: iSliceFirst_P(0:nProc-1), iSliceLast_P(0:nProc-1), nSlice
    real    :: Coord_DII(Lat_,nThreadAll,-nGUniform:0), Coord_D(Lon_:Lat_)
    real    :: Xyz_D(MaxDim), Dist2_I(2+nThreadAll)
    real, parameter :: cThird = 1.0/3.0
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
       if(iError/=0)then
          write(*,'(a,i5)')NameSub//': triangulation failed with iError=',&
               iError
          write(*,'(a,i5,a,i5)')NameSub//'In the range of i from ', &
               -nGUniform,' to 0 triangulation failed at i=',i
          call stop_mpi(NameSub//': Triangulation failed')
       end if
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
                     iStencil_III(1,iBuff,i), iStencil_III(2,iBuff,i),        &
                     iStencil_III(3,iBuff,i))
             end if
             if(.not.IsTriangleFound)then
                Dist2_I = (Xyz_DII(1,:,i) - Xyz_D(1))**2 + &
                     (Xyz_DII(2,:,i) - Xyz_D(2))**2 +      &
                     (Xyz_DII(3,:,i) - Xyz_D(3))**2
                Weight_III(:,iBuff,i) = cThird
                iStencil_III(:,iBuff,i) = minloc(Dist2_I, DIM=1)
             end if
          end do          ! i
       end do; end do    ! j,k
    end do       ! iBlock

  end subroutine set_triangulation
  !============================================================================
  subroutine save_threads_for_plot

    use BATL_lib,               ONLY: nBlock, Unused_B, Xyz_DGB
    use ModB0,                  ONLY: B0_DGB
    use ModAdvance,             ONLY: State_VGB
    use ModTriangulateSpherical, ONLY:fix_state

    integer :: i, j, k, iBlock, iBuff

    ! State vector at the point of intersection of thread with
    ! the spherical coordinate  surface of the grid for plotting
    real    :: State_VII(nVar,nThreadAll+2,-nGUniform:0)
    character(len=*), parameter:: NameSub = 'save_threads_for_plot'
    !--------------------------------------------------------------------------

    call get_thread_point(nVar,State_VII(:,2:nThreadAll+1,:))
    do i = -nGUniform, 0
       ! Fix states at the polar nodes:
       ! North:
       call fix_state(iNodeToFix =   1,   &
            nNode      =  nThreadAll+2,   &
            iList_I    =    iList_II(:,i), &
            iPointer_I = iPointer_II(:,i), &
            iEnd_I     =     iEnd_II(:,i), &
            Xyz_DI     =     Xyz_DII(:,:,i), &
            nVar       =      nVar,      &
            State_VI   = State_VII(:,:,i))
       ! South:
       call fix_state(iNodeToFix =  nThreadAll+2,&
            nNode      =  nThreadAll+2,   &
            iList_I    =    iList_II(:,i), &
            iPointer_I = iPointer_II(:,i), &
            iEnd_I     =     iEnd_II(:,i), &
            Xyz_DI     =     Xyz_DII(:,:,i), &
            nVar       =      nVar,      &
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
             Threads_B(iBlock)%State_VG(:,i,j,k) = &
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
          Threads_B(iBlock)%State_VG(:, 1, j, k) = &
               State_VGB(:, 1, j, k, iBlock)
       end do; end do
    end do
  end subroutine save_threads_for_plot
  !============================================================================
  subroutine get_thread_point(nVarIn, State_VII, DoCoord)

    ! Calculate coordinates (lon, lat) of the intersection point of threads
    ! with the spherical surface at the first generalized coordinate value
    ! equal to input Coord1

    use BATL_lib, ONLY: nBlock, Unused_B, coord_to_xyz, CellSize_DB
    use ModInterpolate, ONLY: linear
    use ModTurbulence,  ONLY: PoyntingFluxPerB
    use ModWaves, ONLY: WaveFirst_, WaveLast_, UseAwRepresentative
    use ModPhysics, ONLY: Si2No_V, UnitU_, UnitB_
    use ModVarIndexes,  ONLY: RhoUx_, RhoUz_, Bx_, Bz_, Rho_
    use ModTransitionRegion, ONLY: Utr_=>U_
    real :: Coord1
    integer, intent(in) :: nVarIn
    real, intent(out) :: State_VII(nVarIn, nThreadAll,-nGUniform:0)
    logical,optional, intent(in) :: DoCoord
    integer :: i, j, k, iBlock, nCell, nPoint, iBuff, iPoint
    integer, parameter:: Lon_ = 1, Lat_ = 2
    real    :: StateThread_V(RhoTr_:WminorTr_), State_V(nVar)
    real    :: CoordFace_I(-nPointThreadMax:0), Coord_I(-nPointThreadMax:0), &
        UandB_VI(2*MaxDim,-nPointThreadMax:0), UandB_V(2*MaxDim), DsM1, Ds0
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_thread_point'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    ! Initialize output array

    ! State_VII = 0.0

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
             nCell = Threads_B(iBlock)%Threads_II(j,k)%nCell
             if(present(DoCoord))then
                nPoint = nCell+2
                ! Fill in an array for this thread with lon,lat values
                ! of the grid points on the thread
                CoordFace_I(-nPoint:0) = Threads_B(iBlock)%&
                  Threads_II(j,k)% Coord_DF(r_,-nPoint:0)
                do i = -nGUniform, 0
                   ! Generalized radial coordinate of the plot grid point
                   Coord1 = CoordMin_D(r_) + real(i)/dCoord1Inv
                   ! Now we find the intersection point of the thread with the
                   ! spherical surface at given radial gen coordinate, Coord1
                   ! and interpolate the state vector to this point
                   State_VII(:,iBuff,i)  = &
                        linear(&
                        a_VI = Threads_B(iBlock)%Threads_II(j,k)%&
                        Coord_DF(2:,-nPoint:0),                      &
                        nVar = nVarIn,                               &
                        iMin = -nPoint,                              &
                        iMax = 0,                                    &
                        x = Coord1,                                  &
                        x_I = CoordFace_I(-nPoint:0),                &
                        DoExtrapolate = .false.)
                end do ! i
             else
                ! Fill in an array for this thread with lon,lat values
             ! of the grid points on the thread
             CoordFace_I(-nCell:0) = Threads_B(iBlock)%&
                  Threads_II(j,k)% Coord_DF(r_,-nCell:0)
                Coord_I(-nCell:-1) = 0.50*&
                     (CoordFace_I(-nCell:-1) + CoordFace_I(1-nCell:0))
                CoordFace_I(-nCell:-1) = Coord_I(-nCell:-1)
                Coord_I(0) = CoordMin_D(r_) + 0.50*CellSize_DB(r_,iBlock)
                do iPoint = -nCell, -1
                   UandB_VI(1:MaxDim,iPoint) = &
                        Threads_B(iBlock)%Threads_II(j,k)%        &
                        DirB_DG(:,iPoint)*                        &
                        Threads_B(iBlock)%Threads_II(j,k)%        &
                        State_VG(Utr_,iPoint)
                   UandB_VI(MaxDim+1:2*MaxDim,iPoint) =           &
                        Threads_B(iBlock)%Threads_II(j,k)%        &
                        B1_DG(:,iPoint)
                end do
                ! Distance from -1 cell to 0 face
                DsM1 = 0.50*Threads_B(iBlock)%Threads_II(j,k)%Ds_G(-1)
                ! Distance from 0 ghost cell to 0 face
                Ds0 = Threads_B(iBlock)%Threads_II(j,k)%Ds_G(0)
                ! Interpolate velocity value to 0 face
                UandB_VI(1:MaxDim,0) = Threads_B(iBlock)%Threads_II(j,k)%  &
                     DirB_DG(:,0)*(Threads_B(iBlock)%Threads_II(j,k)%    &
                     State_VG(Utr_,-1)*Ds0 +                             &
                     Threads_B(iBlock)%Threads_II(j,k)%                  &
                     State_VG(Utr_,0)*DsM1)/(Ds0 + DsM1)
                UandB_VI(MaxDim+1:2*MaxDim,0) =                  &
                     Threads_B(iBlock)%Threads_II(j,k)%B1_DG(:,0)
                Tr2State_VV(Utr_,RhoUx_:RhoUz_) = 0
                do i = -nGUniform, 0
                   ! Generalized radial coordinate of the plot grid point
                   Coord1 = CoordMin_D(r_) + real(i)/dCoord1Inv
                   ! Now we find the intersection point of the thread with the
                   ! spherical surface at given radial gen coordinate, Coord1
                   ! and interpolate the state vector to this point
                   StateThread_V  = &
                        linear(a_VI = Threads_B(iBlock)%Threads_II(j,k)%&
                        State_VG(:,-nCell:0),                        &
                        nVar = WminorTr_,                            &
                        iMin = -nCell,                               &
                        iMax = 0,                                    &
                        x = Coord1,                                  &
                        x_I = Coord_I(-nCell:0),                     &
                        DoExtrapolate = .false.)
                   State_V = matmul(StateThread_V, Tr2State_VV)
                   UandB_V(:) = linear(a_VI = UandB_VI(:,-nCell:0),  &
                        nVar = 2*MaxDim,                             &
                        iMin = -nCell,                               &
                        iMax = 0,                                    &
                        x = Coord1,                                  &
                        x_I = CoordFace_I(-nCell:0),                 &
                        DoExtrapolate = .false.)
                   State_V(RhoUx_:RhoUz_) = UandB_V(1:MaxDim)*&
                        Si2No_V(UnitU_)*State_V(Rho_)
                   State_V(Bx_:Bz_) = UandB_V(MaxDim+1:2*MaxDim)*&
                        Si2No_V(UnitB_)
                   State_VII(:,iBuff,i) = State_V
                end do ! i
                if(Threads_B(iBlock)%Threads_II(j,k)%OpenFlux < 0.0)&
                     State_VII(WaveFirst_:WaveLast_,iBuff,:) = &
                     State_VII(WaveLast_:WaveFirst_:-1,iBuff,:)
                if(.not.UseAwRepresentative)then
                   do iPoint = -nGUniform, 0
                      State_VII(WaveFirst_:WaveLast_,iBuff,iPoint) =     &
                           State_VII(WaveFirst_:WaveLast_,iBuff,iPoint)* &
                           PoyntingFluxPerB*sqrt(State_VII(Rho_,iBuff,iPoint))
                   end do
                end if
             end if
          end do ! j
       end do ! k
    end do ! iBlock
    call broadcast_buffer(nVar=nVarIn, Buff_VII=State_VII)
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

    use ModPhysics,     ONLY: No2Si_V, UnitP_, UnitTemperature_
    use ModVarIndexes,  ONLY: Rho_
    use BATL_lib,       ONLY: xyz_to_coord, CellSize_DB, CoordMin_DB
    use ModInterpolate, ONLY: interpolate_vector
    ! Using a tabulated analytical solution for the Transition Region (TR),
    ! the contribution to the LOS image from the TR is calculated.

    ! An intersection point of the LOS with the chromosphere top:
    real, intent(in)    :: Xyz_D(MaxDim)

    ! A unit vector in direction of the LOS:
    real, intent(in)    :: DirLos_D(MaxDim)
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
    real :: B0_D(MaxDim), DirB_D(MaxDim)

    ! Coords of the point in which to interpolate
    real :: Coord_D(MaxDim), CoordNorm_D(2)
    real :: State_V(nVar)

    ! Radial direction:
    real :: DirR_D(MaxDim)

    ! cosine of angles between the radial direction and the directions of
    ! LOS and magnetic field:
    real :: CosRLos, CosRB

    ! Euv intensities:
    real :: EuvValue_V(3)

    ! Sxr intensities:
    real :: SxrValue_V(2)

    ! Plasma parameters at the top of TR
    real :: TeSi, PeSi

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

    call xyz_to_coord(Xyz_D, Coord_D)
    CoordNorm_D = (Coord_D(r_+1:) - CoordMin_DB(r_+1:,iBlock))/&
         CellSize_DB(r_+1:,iBlock) + 0.50

    ! Interpolate the state on threads to the given location
    State_V = interpolate_vector(                                       &
         a_VC=Threads_B(iBlock)%State_VG(:,-nGUniform,:,:),     &
         nVar=nVar,                                                     &
         nDim=2,                                                        &
         Min_D=[jMin_, kMin_],                                          &
         Max_D=[jMax_, kMax_],                                          &
         x_D=CoordNorm_D)

    TeSi = TeFraction*State_V(iPe)/State_V(Rho_)*No2Si_V(UnitTemperature_)
    PeSi = PeFraction*State_V(iPe)*No2Si_V(UnitP_)

    PlotVar_V = 0

    ! Integrate plot variables
    call set_plot_var

    ! Add contribution to the pixel intensity, account for
    ! the geometric factor
    PixIntensity_V = PixIntensity_V + PlotVar_V *(CosRB/CosRLos)

  contains
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
end module ModFieldLineThread
!==============================================================================
