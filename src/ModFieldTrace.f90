!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModFieldTrace

  use BATL_lib, ONLY: &
       test_start, test_stop, &
       iTest, jTest, kTest, iBlockTest, iProcTest, iProc, nProc, iComm, &
       IsNeighbor_P, nI, nJ, nK, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, &
       nBlock, MaxBlock, Unused_B, x_, y_, z_, &
       Xyz_DGB, CellSize_DB, CoordMin_DB, IsCartesianGrid, &
       find_grid_block, xyz_to_coord, message_pass_cell
  use ModB0, ONLY: B0_DGB, get_b0
  use ModBatsrusUtility, ONLY: get_time_string, stop_mpi
  use ModCoordTransform, ONLY: &
       xyz_to_rlonlat, rlonlat_to_xyz, xyz_to_sph, sph_to_xyz, cross_product
#ifdef _OPENACC
  use ModUtilities, ONLY: norm2
#endif
  use ModMain, ONLY: &
       TypeCoordSystem, nStep, tSimulation, IsTimeAccurate, UseB0, UseRayTrace
  use ModMessagePass, ONLY: exchange_messages
  use ModPhysics, ONLY: rBody
  use ModNumConst, ONLY: &
       i_DD, cPi, cTwoPi, cHalfPi, cRadToDeg, cDegToRad, cTiny
  use ModKind, ONLY: Real8_
  use ModIO, ONLY: iUnitOut, write_prefix
  use ModPlotFile, ONLY: save_plot_file
  use ModUpdateStateFast, ONLY: sync_cpu_gpu
  use ModMpi

  implicit none
  save

  private ! except
  public:: init_mod_field_trace       ! initialize module
  public:: clean_mod_field_trace      ! clean module
  public:: read_field_trace_param     ! set trace parameters
  public:: trace_field_equator        ! trace field from equatorial plane
  public:: extract_field_lines        ! extract field lines
  public:: integrate_field_from_sphere! integrate field from spherical surface
  public:: write_plot_line            ! extract field lines into plot file(s)
  public:: write_plot_ieb             !
  public:: write_plot_lcb             ! write plot with last closed B lines
  public:: write_plot_equator         !

  ! Public for ModFieldTraceFast only
  public:: trace_grid_accurate        ! trace field from 3D MHD grid cells
  public:: trace_field_sphere         ! trace field from spherical surface
  public:: xyz_to_latlonstatus        ! convert to lat, lon, status

  ! extracting variables (in SI units?) along the field trace
  logical, public:: DoExtractState   = .false.
  logical, public:: DoExtractUnitSi  = .false.
  logical, public:: DoExtractBGradB1 = .false.
  logical, public:: DoExtractEfield  = .false.

  ! mapping to the ionosphere
  real, public, allocatable:: RayMap_DSII(:,:,:,:)

  ! Trace_DSNB contains the x,y,z coordinates for the foot point of a given
  ! field line for both directions, eg.
  ! Trace_DSNB(2,1,i,j,k,iBlock) is the y coord for direction 1
  ! trace for cell center i,j,k of block iBlock

  real, public, allocatable :: Trace_DSNB(:,:,:,:,:,:)
  !$acc declare create(Trace_DSNB)

  ! Squash factor
  real, public, allocatable :: SquashFactor_II(:,:), SquashFactor_GB(:,:,:,:)
  real, public:: SquashFactorMax = 100.0

  ! Integral_I added up for all the local trace segments
  ! The fist index corresponds to the variables (index 0 shows closed vs. open)
  ! The second and third indexes correspond to the latitude and longitude of
  ! the iM/RCM grid
  real, public, allocatable :: RayIntegral_VII(:,:,:)
  real, public, allocatable :: RayResult_VII(:,:,:)

  ! map rays to the SM equatorial plane
  logical, public :: DoMapEquatorRay= .false.
  !$acc declare create(DoMapEquatorRay)

  real, public :: rIonosphere = 0.
  !$acc declare create(rIonosphere)

  ! Radius where the tracing stops
  real, public :: rTrace = 0.
  !$acc declare create(rTrace)

  ! Named indexes
  integer, public, parameter :: &
       InvB_=1, Z0x_=2, Z0y_=3, Z0b_=4, &
       RhoInvB_=5, pInvB_=6,  &
       HpRhoInvB_=5, HpPInvB_=6, OpRhoInvB_=7, OpPInvB_=8

  ! These indexes depend on multi-ion
  integer, public:: iPeInvB = 9 ! or 7
  integer, public:: iXEnd, iYEnd, iZEnd, iLength

  ! Various values indicating the end state of a trace
  real, public :: ClosedRay, OpenRay, BodyRay, LoopRay, NoRay, OutRay
  !$acc declare create(ClosedRay, OpenRay, BodyRay, LoopRay, NoRay, OutRay)

  ! Select between fast less accurate and slower but more accurate algorithms
  logical, public:: UseAccurateTrace    = .false.

  ! Logical for raytracing in IE coupling
  logical, public :: DoTraceIE = .false.

  ! Transfrom to SM coordinates?
  logical, public:: UseSmg = .true.

  ! Conversion matrix between SM and GM coordinates
  ! (to be safe initialized to unit matrix)
  real, public :: GmSm_DD(3,3) = i_DD
  !$acc declare create(GmSm_DD)

  ! Square of radii to save computation
  real, public :: rTrace2 = 0.0
  real, public :: rIonosphere2 = 0.0
  !$acc declare create(rTrace2, rIonosphere2)

  ! Inner radius to end tracing (either rTrace or rIonosphere)
  real, public :: rInner = 0.0, rInner2 = 0.0
  !$acc declare create(rInner, rInner2)

  ! The vector field to trace: B/U/J
  character, public :: NameVectorField = 'B'
  logical, public:: IsBVectorField = .true.
  !$acc declare create(IsBVectorField)

  ! Minimum number of time steps between two traces on the same grid
  integer, public :: DnRaytrace = 1

  ! Named parameters for trace status
  ! These values all must be less than 1, because 1..6 correspond to the
  ! six faces of the block. The ordering of these values is arbitrary.
  integer, public, parameter ::       &
       RayIono_    =  0,     &
       RayEquator_ = -1,     &
       RayBlock_   = -2,     &
       RayOpen_    = -3,     &
       RayLoop_    = -4,     &
       RayBody_    = -5,     &
       RayOut_     = -6

  ! Total magnetic field with second order ghost cells
  real, public, allocatable :: b_DGB(:,:,:,:,:)
  !$acc declare create(b_DGB)

  ! Local variables --------------------------------

  ! If true, calculate B0 with get_b0, otherwise interpolate
  logical, parameter:: DoGetB0 = .false.

  ! Reduce step size and tolerance with this factor
  real:: AccuracyFactor = 1.0

  ! Possible tasks
  logical :: DoTraceRay     = .false. ! trace rays from all cell centers
  logical :: DoMapRay       = .false. ! map rays down to the ionosphere
  logical :: DoMapOpen      = .false. ! map open rays to outer boundary
  logical :: DoExtractRay   = .false. ! extract info along the rays into arrays
  logical :: DoIntegrateRay = .false. ! integrate some functions along the rays

  ! Use old IJK based logic for Cartesian tracing
  logical :: UseOldMethodOfRayTrace = .true.

  ! Number of rays per dimension on the starting grid
  ! This is needed for DoExtractRay = .true. only
  integer :: nRay_D(4) = [0, 0, 0, 0]

  ! How often shall we synchronize PE-s for the accurate algorithms
  real :: DtExchangeRay = 0.1

  ! Maximum length of trace
  real :: RayLengthMax = 200.

  ! Testing
  logical :: DoTestRay = .false.

  ! Base time for timed exchanges between rays
  real(Real8_) :: CpuTimeStartRay

  ! Number of rays found to be open based on the neighbors

  ! ----------- Variables for Integral_I along the trace -----------------

  ! Number of Integral_I depends on UseMultiIon and UseAnisPressure
  integer:: nRayIntegral

  ! Number of state variables to be integrated and number of variables for
  ! a local segment
  integer :: nExtraIntegral, nLocalIntegral

  ! Flow variables to be integrated (rho and P) other than the magnetic field
  real, allocatable :: Extra_VGB(:,:,:,:,:)

  ! Integral_I for a local trace segment
  real, allocatable :: RayIntegral_V(:)

  ! Temporary array for extracting b.grad(B1) info
  real, allocatable :: bGradB1_DGB(:,:,:,:,:)

  ! Temporary array for extracting curvature of B info
  real, allocatable :: CurvatureB_GB(:,:,:,:)
  logical:: DoExtractCurvatureB = .false.

  integer :: iLatTest = 1, iLonTest = 1

  ! Variables for squash factor calculation
  integer:: nLonSquash = 360, nLatSquash = 180
  real:: AccuracyFactorSquash = 20.0

contains
  !============================================================================
  subroutine read_field_trace_param(NameCommand)

    use ModReadParam, ONLY: read_var

    character(len=*), intent(in):: NameCommand

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_field_trace_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(NameCommand)
    case("#TRACE", "#RAYTRACE")
       call read_var('UseTrace', UseRaytrace)
       if(UseRaytrace)then
          call read_var('UseAccurateTrace', UseAccurateTrace)
          call read_var('DtExchangeTrace',  DtExchangeRay)
          call read_var('DnTrace',          DnRaytrace)
       end if
    case("#TRACERADIUS")
       call read_var('rTrace',      rTrace)
       call read_var('rIonosphere', rIonosphere)
    case("#TRACELIMIT", "#RAYTRACELIMIT")
       call read_var('TraceLengthMax', RayLengthMax)
    case("#TRACEACCURACY")
       call read_var('AccuracyFactor', AccuracyFactor)
    case("#TRACEEQUATOR", "#RAYTRACEEQUATOR")
       call read_var('DoMapEquatorTrace', DoMapEquatorRay)
    case("#TRACEIE", "#IE")
       call read_var('DoTraceIE', DoTraceIE)
    case("#TRACETEST")
       call read_var("iLonTest", iLonTest)
       call read_var("iLatTest", iLatTest)
    case("#SQUASHFACTOR")
       call read_var("nLonSquash", nLonSquash)
       call read_var("nLatSquash", nLatSquash)
       call read_var("AccuracyFactorSquash", AccuracyFactorSquash)
    case default
       call stop_mpi(NameSub//': unknown command='//NameCommand)
    end select

    call test_stop(NameSub, DoTest)
  end subroutine read_field_trace_param
  !============================================================================
  subroutine xyz_to_latlon(Pos_D)
    !$acc routine seq

    ! Convert xyz coordinates to latitude and longitude (in degrees)
    ! Put the latitude and longitude into the 1st and 2nd elements
    real, intent(inout) :: Pos_D(3)

    real :: rLonLat_D(3)

    ! Check if this direction has a valid footpoint
    character(len=*), parameter:: NameSub = 'xyz_to_latlon'
    !--------------------------------------------------------------------------
    if(Pos_D(1) > ClosedRay)then

       ! Convert GM position into SM coordinates
       Pos_D = matmul(Pos_D, GmSm_DD)

       call xyz_to_rlonlat(Pos_D, rLonLat_D)

       Pos_D(1) = rLonLat_D(3)*cRadToDeg ! latitude in degrees
       Pos_D(2) = rLonLat_D(2)*cRadToDeg ! longitude in degrees
       if(rLonLat_D(1) < 1.1*rInner)then
          Pos_D(3) = rLonLat_D(1)
       else
          Pos_D(3) = OpenRay
       end if

    else
       ! Impossible values
       Pos_D(1) = -100.
       Pos_D(2) = -200.
    endif

  end subroutine xyz_to_latlon
  !============================================================================
  subroutine xyz_to_rphi(Pos_DI)
    !$acc routine seq

    ! Convert X,Y coordinates into radial distance in the XY plane
    ! and longitude (in degrees) for closed rays
    real, intent(inout) :: Pos_DI(3,2)

    real :: x, y, r, Phi

    ! index for the direction connected to the equator
    integer:: iDir

    ! Check if both directions are connected to the ionosphere
    ! or the equatorial plane

    character(len=*), parameter:: NameSub = 'xyz_to_rphi'
    !--------------------------------------------------------------------------
    if(all(Pos_DI(3,:) > ClosedRay))then

       ! Check if the first direction of the trace ends on the ionosphere
       if(Pos_DI(1,1)**2 + Pos_DI(2,1)**2 <= rIonosphere2) then
          iDir = 2
       else
          iDir = 1
       end if

       ! Convert to radius and longitude in degrees
       x   = Pos_DI(1,iDir)
       y   = Pos_DI(2,iDir)
       r   = sqrt(x**2 + y**2)
       Phi = cRadToDeg * atan2(y, x)
       ! Get rid of negative longitude angles
       if(Phi < 0.0) Phi = Phi + 360.0

       ! Put r and Phi for BOTH directions
       Pos_DI(1,:) = r
       Pos_DI(2,:) = Phi

       ! Now checking if the ray is ending is at z = 0.
       ! If not, set it as open ray. (For HEIDI)
       if (Pos_DI(3,iDir) /= 0.0) Pos_DI(3,:) = OpenRay

    else
       ! Impossible values
       Pos_DI(1,:) = -1.0
       Pos_DI(2,:) = -200.
    endif

  end subroutine xyz_to_rphi
  !============================================================================
  subroutine xyz_to_latlonstatus(Ray_DI)
    !$acc routine seq
    real, intent(inout) :: Ray_DI(3,2)

    integer :: iRay

    ! Convert 1st and 2nd elements into latitude and longitude

    character(len=*), parameter:: NameSub = 'xyz_to_latlonstatus'
    !--------------------------------------------------------------------------
    if(DoMapEquatorRay)then
       call xyz_to_rphi(Ray_DI)
    else
       do iRay = 1, 2
          call xyz_to_latlon(Ray_DI(:,iRay))
       end do
    end if

    ! Convert 3rd element into a status variable
    if(Ray_DI(3,1) > ClosedRay .and. Ray_DI(3,2) > ClosedRay)then
       Ray_DI(3,:) = 3      ! Fully closed
    elseif(Ray_DI(3,1) > ClosedRay .and. abs(Ray_DI(3,2) - OpenRay) < 0.01)then
       Ray_DI(3,:) = 2      ! Half closed in positive direction
    elseif(Ray_DI(3,2) > ClosedRay .and. abs(Ray_DI(3,1) - OpenRay) < 0.01)then
       Ray_DI(3,:) = 1      ! Half closed in negative direction
    elseif(maxval(abs(Ray_DI(3,:) - OpenRay)) < 0.01) then
       Ray_DI(3,:) = 0      ! Fully open
    elseif(abs(Ray_DI(3,1) - BodyRay) < 0.01)then
       Ray_DI(3,:) = -1     ! Cells inside body
    elseif(maxval(abs(Ray_DI(3,:) - LoopRay)) < 0.01) then
       Ray_DI(3,:) = -2     ! Loop within block
    else
       Ray_DI(3,:) = -3     ! Strange status
    end if

  end subroutine xyz_to_latlonstatus
  !============================================================================
  subroutine init_mod_field_trace

    use ModPhysics, ONLY: DipoleStrengthSi
    use ModAdvance, ONLY: UseElectronPressure
    use ModMain, ONLY: DoMultiFluidIMCoupling
    use ModGeometry, ONLY: xMinBox

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_mod_field_trace'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(rIonosphere == 0.0)then
       ! Set default ionosphere radius
       ! Currently do_follow_iono only works for a dipole
       ! This may be generalized in the future
       if(DipoleStrengthSi /= 0.0)then
          rIonosphere = 1.0
       else
          rIonosphere = -1.0
       end if
    end if

    if(rTrace == 0.0)then
       if(rBody > 0.0)then
          rTrace = max(rBody, rIonosphere)
       else
          rTrace = -1.0
       end if
    end if

    ! Radius square preserving sign
    rIonosphere2 = rIonosphere*abs(rIonosphere)
    rTrace2      = rTrace*abs(rTrace)

    if(rIonosphere < 0.0)then
       rInner = abs(rTrace)
    else
       rInner = rIonosphere
    end if
    rInner2 = rInner**2

    ClosedRay = xMinBox - 100
    OpenRay   = xMinBox - 101
    BodyRay   = xMinBox - 102
    LoopRay   = xMinBox - 103
    NoRay     = xMinBox - 104
    OutRay    = xMinBox - 105

    if(DoTest)then
       write(*,*) 'rTrace, rTrace2          =', rTrace, rTrace2
       write(*,*) 'rIonosphere, rIonosphere2=', rIonosphere, rIonosphere2
       write(*,*) 'rInner, rInner2          =', rInner, rInner2
       write(*,*) 'ClosedRay,OpenRay,OutRay =', ClosedRay, OpenRay, OutRay
    end if

    if(.not.allocated(b_DGB)) then
       allocate(b_DGB(3,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
       b_DGB = 0.0
    end if

    if(.not.allocated(Trace_DSNB))then
       allocate(Trace_DSNB(3,2,nI+1,nJ+1,nK+1,MaxBlock))
       Trace_DSNB = 0.0
    end if

    if(allocated(RayIntegral_V)) RETURN

    ! Determine number of flow variable Integral_I
    if(DoMultiFluidIMCoupling)then
       ! H+ and O+ densities pressures
       nExtraIntegral = 4
    else
       ! total density and pressure
       nExtraIntegral = 2
    end if

    ! electron pressure is the last extra integral
    if(UseElectronPressure) nExtraIntegral = nExtraIntegral + 1

    ! Number of Integral_I for a local trace segment:
    !    InvB_, Z0x_, Z0y_, Z0b_ and extras
    nLocalIntegral = nExtraIntegral + 4

    ! the electron pressure is the last local integral
    ! iPeInvB is only used GM-IM coupling, either 7 or 9
    ! notice that even iPeInvB looks overlapped with iXEnd
    ! when UseElectronPressure = .false., it is not used in this module
    ! for indexing variabls
    if(UseElectronPressure) then
       iPeInvB = nLocalIntegral
    else
       iPeInvB = nLocalIntegral + 1
    end if

    ! Indexes for the final position of the trace
    iXEnd = nLocalIntegral + 1; iYEnd = iXEnd + 1; iZEnd = iYEnd + 1
    iLength = iZEnd + 1

    ! Number of reals stored in the RayIntegral_VII and RayResult_VII arrays
    nRayIntegral = iLength

    allocate(Extra_VGB(nExtraIntegral,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
    allocate(RayIntegral_V(1:nLocalIntegral))

    if(iProc==0)then
       call write_prefix
       write(iUnitOut,'(a)') NameSub//' allocated arrays'
    end if

    UseSmg = TypeCoordSystem == 'GSM' .or. TypeCoordSystem == 'GSE'

    !$acc update device(ClosedRay, OpenRay, BodyRay, LoopRay, NoRay, OutRay)
    !$acc update device(rIonosphere, rIonosphere2, DoMapEquatorRay)
    !$acc update device(rInner, rInner2, rTrace, rTrace2)
    call test_stop(NameSub, DoTest)
  end subroutine init_mod_field_trace
  !============================================================================
  subroutine clean_mod_field_trace

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'clean_mod_field_trace'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(allocated(Trace_DSNB))    deallocate(Trace_DSNB)
    if(allocated(b_DGB))         deallocate(b_DGB)
    if(allocated(RayIntegral_V)) deallocate(RayIntegral_V)
    if(allocated(Extra_VGB))     deallocate(Extra_VGB)

    if(iProc==0)then
       call write_prefix
       write(iUnitOut,'(a)') 'clean_mod_raytrace deallocated arrays'
    end if

    call test_stop(NameSub, DoTest)
  end subroutine clean_mod_field_trace
  !============================================================================
  subroutine trace_grid_accurate

    ! Trace field lines from cell centers to the outer or inner boundaries

    use CON_ray_trace, ONLY: ray_init
    use ModMain
    use ModAdvance, ONLY: State_VGB, Bx_, Bz_
    use ModGeometry, ONLY: r_GB
    use BATL_lib, ONLY: Used_GB

    ! Indices corresponding to the starting point and directon of traces
    integer :: i, j, k, iBlock, iRay

    ! Testing and timing
    logical :: DoTime

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'trace_grid_accurate'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    DoTime = DoTest

    if (DoTest) write(*,*) NameSub,' starting'

    ! Initialize constants
    DoTraceRay     = .true.
    nRay_D         = [ nI, nJ, nK, nBlock ]
    NameVectorField = 'B'

    ! (Re)initialize CON_ray_trace
    call ray_init(iComm)

    if(DoTime) call timing_reset('ray_pass', 2)

    ! Copy magnetic field into b_DGB
    do iBlock = 1, nBlock; if(Unused_B(iBlock))CYCLE
       b_DGB(:,:,:,:,iBlock) = State_VGB(Bx_:Bz_,:,:,:,iBlock)
    end do
    ! Fill in ghost cells with first order prolongation
    call message_pass_cell(3, b_DGB, nProlongOrderIn=1)
    if(UseB0 .and. .not. DoGetB0)then
       ! Add B0
       do iBlock = 1, nBlock; if(Unused_B(iBlock))CYCLE
          b_DGB(:,:,:,:,iBlock) = &
               b_DGB(:,:,:,:,iBlock) + B0_DGB(:,:,:,:,iBlock)
       end do
    end if

    ! Initial values
    Trace_DSNB = NoRay

    if(DoTest) write(*,*) NameSub,' normalized B'
    if(DoTime .and. iProc==0)then
       write(*,'(a)',ADVANCE='NO') 'setup and normalization:'
       call timing_show('trace_field_grid', 1)
    end if

    ! This loop order seems to give optimal speed
    CpuTimeStartRay = MPI_WTIME();
    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       do iBlock = 1, nBlock
          if(Unused_B(iBlock))CYCLE

          DoTestRay = DoTest .and. &
               all( [i, j, k, iBlock, iProc] == &
               [iTest, jTest, kTest, iBlockTest, iProcTest] )

          do iRay = 1, 2
             ! Short cut for inner and false cells
             if(r_GB(i,j,k,iBlock) < rInner .or. &
                  .not.Used_GB(i,j,k,iBlock))then
                Trace_DSNB(:,:,i,j,k,iBlock)=BodyRay
                if(DoTestRay) &
                     write(*,*)'Shortcut BodyRay iProc,iRay=', iProc, iRay
                CYCLE
             end if

             if(DoTestRay) &
                  write(*,*)'calling follow_ray iProc,iRay=', iProc, iRay

             ! Trace in direction iRay
             call follow_ray(iRay, [i,j,k,iBlock], Xyz_DGB(:,i,j,k,iBlock))

          end do ! iRay
       end do    ! iBlock
    end do; end do; end do  ! i, j, k

    ! Do remaining rays passed from other PE-s
    call finish_ray

    ! Convert x, y, z to latitude and longitude, and status
    do iBlock=1,nBlock
       if(Unused_B(iBlock)) CYCLE
       do k = 1, nK; do j = 1, nJ; do i = 1, nI

          call xyz_to_latlonstatus(Trace_DSNB(:,:,i,j,k,iBlock))

          ! Comment out these statements as they spew thousands of lines
          ! with iM coupling
          ! if(Trace_DSNB(3,1,i,j,k,iBlock)==-2.) write(*,*) &
          !     'Loop Trace_DSNB found at iProc,iBlock,i,j,k,Trace_DSNB=',&
          !     iProc,iBlock,i,j,k,Trace_DSNB(:,:,i,j,k,iBlock)

          ! if(Trace_DSNB(3,1,i,j,k,iBlock)==-3.) write(*,*) &
          !     'Strange Trace_DSNB found at iProc,iBlock,i,j,k,Trace_DSNB=',&
          !     iProc,iBlock,i,j,k,Trace_DSNB(:,:,i,j,k,iBlock)
       end do; end do; end do
    end do

    if(DoTest)write(*,*)'Trace_DSNB lat, lon, status=',&
         Trace_DSNB(:,:,iTest,jTest,kTest,iBlockTest)

    if(DoTime.and.iProc==0)then
       write(*,'(a)',ADVANCE='NO') NameSub//': total tracing time:'
       call timing_show('trace_field_grid', 1)
    end if

    DoTraceRay = .false.

    if(DoTest)write(*,*) NameSub,' finished'

    call test_stop(NameSub, DoTest)

  end subroutine trace_grid_accurate
  !============================================================================
  subroutine finish_ray

    ! This subroutine is a simple interface for the last call to follow_ray
    !--------------------------------------------------------------------------
    call follow_ray(0, [0, 0, 0, 0], [ 0., 0., 0. ])

  end subroutine finish_ray
  !============================================================================
  subroutine follow_ray(iRayIn, i_D, XyzIn_D)

    ! Trace in direction iRayIn (1 is parallel with the field,
    !                            2 is anti-parallel,
    !                            0 means that nothing is traced
    ! Always follow rays received from other PE-s.
    !
    ! The passed trace is identified by the index array i_D.
    ! The meaning of i_D d depends on the context:
    !  3 cell + 1 block index for 3D tracing
    !  1 latitude + 1 longitude index for 2D integration
    !  1 linear index for 1D extraction.
    !
    ! The rays are followed until they hit the outer or inner
    ! boundary of the computational domain. The results are saved into
    ! arrays defined in ModFieldTrace or into files based on the logicals
    ! in ModFieldtrace (more than one of these can be true):
    !
    ! If DoTraceRay, trace from cell centers of the 3D grid,
    !    and save the final position into
    !    ModFieldTrace::Trace_DSNB(:,iRayIn,i_D(1),i_D(2),i_D(3),i_D(4)) on the
    !    processor that started the trace.
    !
    ! If DoMapRay, map the rays down to the ionosphere, save spherical
    !    coordinates (in SMG) into
    !    ModFieldTrace::RayMap_DSII(3,i_D(1),i_D(2),i_D(3))
    !
    ! If DoIntegrateRay, do integration along the rays and
    !    save the Integral_I into ModFieldTrace::RayIntegral_VII(i_D(1),i_D(2))
    !
    ! If DoExtractRay, extract data along the rays, collect and sort it
    !    In this case the rays are indexed with i_D(1).
    !

    use CON_ray_trace, ONLY: ray_exchange, ray_get, ray_put

    integer, intent(in) :: iRayIn     ! trace direction, 0 if no trace passed
    integer, intent(in) :: i_D(4)     ! general index array for start position
    real,    intent(in) :: XyzIn_D(3) ! coordinates of starting position

    ! local variables

    ! Cell, block and PE indexes for initial position and trace direction
    integer :: iStart, jStart, kStart, iBlockStart, iProcStart, iRay
    integer :: iStart_D(4)

    ! Current position of the trace
    integer :: iBlockRay
    real    :: XyzRay_D(3)

    ! Current length of trace
    real    :: RayLength

    ! Is the trace Done
    logical :: DoneRay

    ! Shall we get rays from other PE-s
    logical :: DoGet

    ! Did we get rays from other PE-s
    logical :: IsFound

    ! Is the trace parallel with the vector field
    logical :: IsParallel

    integer, parameter :: MaxCount = 1000
    integer :: iFace, iCount, jProc, jBlock

    logical :: DoneAll
    integer :: iCountRay = 0

    real(Real8_) :: CpuTimeNow

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'follow_ray'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(iRayIn /= 0)then

       ! Store starting indexes and trace direction
       iStart = i_D(1); jStart = i_D(2); kStart = i_D(3);
       iBlockStart = i_D(4); iProcStart = iProc
       iRay   = iRayIn

       iStart_D = i_D
       if(DoTest)call set_dotestray

       ! Current position and length
       iBlockRay = i_D(4)
       XyzRay_D  = XyzIn_D
       RayLength = 0.0

       if(DoTestRay)write(*,'(a,6i4,3es12.4)')&
            'Local trace at iProc,i_D,iRay,XyzIn_D=', iProc, i_D, iRay, XyzIn_D

    end if

    ! If iRayIn==0 there are no more local rays to follow so get from other PEs
    DoGet = iRayIn == 0
    IsFound = .true.

    RAYS: do

       if(DoGet)then
          GETRAY: do
             call ray_get(IsFound, iProcStart, iStart_D, XyzRay_D, RayLength, &
                  IsParallel, DoneRay)

             if(IsFound)then
                if(DoTest)call set_dotestray

                if(IsParallel)then
                   iRay = 1
                else
                   iRay = 2
                end if
                if(DoTestRay)write(*,*)'Recv trace iProc,iRay,Done,XyzRay_D=',&
                     iProc, iRay, DoneRay, XyzRay_D

                if(DoneRay)then
                   if(.not.DoTraceRay)then
                      write(*,*)NameSub,' WARNING ',&
                           'received DoneRay=T for DoTraceRay = .false. !'
                      CYCLE GETRAY
                   end if

                   ! Store the result into the ModFieldTrace::Trace_DSNB
                   iStart      = iStart_D(1)
                   jStart      = iStart_D(2)
                   kStart      = iStart_D(3)
                   iBlockStart = iStart_D(4)

                   Trace_DSNB(:,iRay,iStart,jStart,kStart,iBlockStart)=XyzRay_D

                   if(DoTestRay)write(*,*)&
                        'Storing recv trace iProc,iRay,i,j,k,iBlock,trace=',&
                        iProc,iRay,iStart,jStart,kStart,iBlockStart,XyzRay_D

                   ! Get another trace from the other processors
                   CYCLE GETRAY
                else
                   ! Find block for the received trace
                   call find_grid_block(XyzRay_D,jProc,iBlockRay)

                   if(jProc /= iProc)call stop_mpi(&
                        'GM_ERROR in ray_trace: Recvd trace is not in this PE')

                   if(DoTestRay) &
                        write(*,*)'Block for recv trace iProc,iBlock=',&
                        iProc, iBlockRay
                end if
             end if
             EXIT GETRAY
          end do GETRAY
       end if ! DoGet

       if(IsFound)then
          call follow_this_ray
          DoGet = .true.
       else
          if(iRayIn>0)then
             ! Stop working on received rays if there are no more
             ! but there are still local rays
             RETURN
          else
             ! Try to get more rays from others and check if everyone is Done
             call ray_exchange(.true., DoneAll, IsNeighbor_P)
             if(DoneAll)then
                EXIT RAYS
             else
                CYCLE RAYS
             end if
          end if
       end if

       iCountRay = iCountRay + 1

       if(iRayIn>0)then
          ! If there are still local rays, exchange only occasionally
          CpuTimeNow = MPI_WTIME()
          if(CpuTimeNow - CpuTimeStartRay > DtExchangeRay)then
             ! This PE is not Done yet, so pass .false.
             call ray_exchange(.false., DoneAll, IsNeighbor_P)
             CpuTimeStartRay = CpuTimeNow
          end if
       end if

    end do RAYS

    call ray_exchange(.true., DoneAll)

    GETRAYFINAL: do
       call ray_get(IsFound,iProcStart,iStart_D,XyzRay_D,RayLength,&
                    IsParallel,DoneRay, IsEnd=.true.)

       if(IsFound)then
          if(IsParallel)then
             iRay=1
          else
             iRay=2
          end if

          if(.not.DoTraceRay)then
             write(*,*)NameSub,' WARNING ',&
                       'received DoneRay=T for DoTraceRay = .false. !'
             CYCLE GETRAYFINAL
          end if

          ! Store the result into the ModFieldTrace::Trace_DSNB
          iStart      = iStart_D(1)
          jStart      = iStart_D(2)
          kStart      = iStart_D(3)
          iBlockStart = iStart_D(4)

          Trace_DSNB(:,iRay,iStart,jStart,kStart,iBlockStart) = XyzRay_D

          ! Get another trace from the others
          CYCLE GETRAYFINAL
       end if
       EXIT GETRAYFINAL
    end do GETRAYFINAL

    call test_stop(NameSub, DoTest)
  contains
    !==========================================================================
    subroutine follow_this_ray

      ! Initialize Integral_I for this segment
      !------------------------------------------------------------------------
      if(DoIntegrateRay)RayIntegral_V = 0.0

      ! Trace through the local blocks
      BLOCK: do iCount = 1, MaxCount

         if(iCount < MaxCount)then
            call follow_ray_block(iStart_D, iRay, iBlockRay, XyzRay_D, &
                 RayLength,iFace)
         else
            write(*,*) NameSub, &
                 ' WARNING trace passed through more than MaxCount=',&
                 MaxCount,' blocks:'
            write(*,*)NameSub,' iStart_D    =', iStart_D
            write(*,*)NameSub,' XyzRay_D    =', XyzRay_D
            write(*,*)NameSub,' CoordMin_DB =', CoordMin_DB(:,iBlockRay)
            iFace = RayLoop_
         end if

         select case(iFace)
         case(RayBlock_)

            ! Find the new PE and block for the current position
            call find_grid_block(XyzRay_D, jProc, jBlock)

            if(jProc /= iProc)then
               ! Send trace to the next processor and return from here
               if(DoTestRay)write(*,*)'Sending trace iProc,jProc,iRay,Xyz=',&
                    iProc,jProc,iRay,XyzRay_D

               ! Add partial results to the Integral_I.
               ! Pass .false., because this is not the final position
               if(DoIntegrateRay)call store_integral(.false.)

               call ray_put(iProcStart,iStart_D,jProc,XyzRay_D,RayLength,&
                    iRay==1,.false.)
               RETURN
            elseif(jBlock /= iBlockRay)then
               ! Continue the same trace in the next block
               iBlockRay = jBlock
               if(DoTestRay)write(*,'(a,3i4,3es12.4)')&
                    'Continuing trace iProc,jBlock,iRay,Xyz=',&
                    iProc,jBlock,iRay,XyzRay_D
               CYCLE BLOCK
            else
               write(*,*)'ERROR for follow_this_ray, iProc=',iProc
               write(*,*)'ERROR iBlockRay==jBlock    =',iBlockRay, jBlock
               write(*,*)'ERROR iStart_D, iProcStart =',iStart_D, iProcStart
               write(*,*)'ERROR for XyzRay_D, r, iRay=', &
                    XyzRay_D, norm2(XyzRay_D), iRay
               write(*,*)'CoordMin_DB, CellSize_DB  =', &
                    CoordMin_DB(:,jBlock), CellSize_DB(:,jBlock)
               ! call stop_mpi(&
               !     'GM_ERROR in follow_ray: continues in same BLOCK')
            end if
         case(RayOpen_)
            ! The trace reached the outer boundary
            ! Field line integration relies on XyzRay_D to be set to OpenRay
            if(DoIntegrateRay .or. &
                 ((DoMapRay .or. DoMapEquatorRay) .and. .not.DoMapOpen) ) &
                 XyzRay_D = OpenRay
            if(DoTestRay)write(*,*)&
                 'follow_ray finished at outer boundary, iProc,iRay,Xyz=', &
                 iProc, iRay, XyzRay_D

         case(RayLoop_)
            ! The trace did not hit the wall of the block
            XyzRay_D = LoopRay
            if(DoTestRay)write(*,*)&
                 'follow_ray finished with LoopRay, iProc,iRay=', iProc, iRay

         case(RayBody_)
            ! The trace hit a body
            XyzRay_D = BodyRay
            if(DoTestRay)write(*,*)&
                 'follow_ray finished with BodyRay, iProc,iRay=', iProc, iRay

         case(RayIono_)
            ! The trace hit the ionosphere
            if(DoTestRay)write(*,'(a,2i4,3es12.4)')&
                 'follow_this_ray finished on the ionosphere '// &
                 'at iProc,iRay,Xyz=', iProc, iRay, XyzRay_D

         case(RayEquator_)
            ! The trace hit the SM equatorial plane
            if(DoTestRay)write(*,'(a,2i4,3es12.4)')&
                 'follow_this_ray finished on the SM equator '// &
                 'at iProc,iRay,Xyz=',iProc,iRay,XyzRay_D

         case default
            write(*,*)'Impossible value for iFace=',iFace,&
                 ' at XyzRay_D,iBlockRay=',XyzRay_D,iBlockRay
            call stop_mpi('GM_ERROR in follow_ray: impossible iFace value')
         end select

         ! Store Integral_I and the final position
         if(DoIntegrateRay)call store_integral(.true.)

         if(DoMapRay)then
            if(.not.allocated(RayMap_DSII))then
               allocate(RayMap_DSII(3,nRay_D(1),nRay_D(2),nRay_D(3)))
               RayMap_DSII = 0.0
            end if
            RayMap_DSII(:,iStart_D(1),iStart_D(2),iStart_D(3)) = XyzRay_D
         end if

         ! Nothing more to do if not tracing
         if(.not.DoTraceRay) EXIT BLOCK

         ! For tracing either store results or send them back to starting PE
         if(iProcStart == iProc)then

            ! Store the result into the ModFieldTrace::Trace_DSNB
            iStart      = iStart_D(1)
            jStart      = iStart_D(2)
            kStart      = iStart_D(3)
            iBlockStart = iStart_D(4)

            Trace_DSNB(:,iRay,iStart,jStart,kStart,iBlockStart) = XyzRay_D

            if(DoTestRay)write(*,*) &
                 'Storing into iProc,iBlock,i,j,k,iRay,Xyz=',&
                 iProc,iBlockStart,iStart,jStart,kStart,iRay,XyzRay_D

         else
            ! Send back result to iProcStart.
            call ray_put(iProcStart,iStart_D,iProc,XyzRay_D,RayLength,&
                 iRay==1,.true.)

            if(DoTestRay)write(*,*) &
                 'Send result iProc,iProcStart,iRay,Xyz=',&
                 iProc,iProcStart,iRay,XyzRay_D

         end if
         EXIT BLOCK

      end do BLOCK

    end subroutine follow_this_ray
    !==========================================================================
    subroutine store_integral(DoneRay)

      ! Store Integral_I of this trace into the

      logical, intent(in) :: DoneRay

      integer :: iLat, iLon

      !------------------------------------------------------------------------
      iLat = iStart_D(1)
      iLon = iStart_D(2)

      RayIntegral_VII(InvB_:nLocalIntegral,iLat,iLon) = &
           RayIntegral_VII(InvB_:nLocalIntegral,iLat,iLon) + RayIntegral_V

      if(DoneRay)then
         RayIntegral_VII(iXEnd:iZEnd,iLat,iLon) = XyzRay_D
         RayIntegral_VII(iLength,iLat,iLon)     = RayLength
      end if
    end subroutine store_integral
    !==========================================================================
    subroutine set_dotestray
      !------------------------------------------------------------------------
      if(DoIntegrateRay)then
         ! Test the trace starting from a given Lat-Lon grid point
         DoTestRay = DoTest .and. all(iStart_D(1:2) == [iLatTest,iLonTest])
      else if(DoTraceRay)then
         ! Test the trace starting from a given grid cell
         DoTestRay = DoTest .and. iProcStart == iProcTest .and. &
              all(iStart_D == [iTest,jTest,kTest,iBlockTest])
      else
         ! Check the trace indexed in line plot files.
         DoTestRay = DoTest .and. iStart_D(1) == iTest
      end if

    end subroutine set_dotestray
    !==========================================================================
  end subroutine follow_ray
  !============================================================================
  subroutine follow_ray_block(iStart_D,iRay,iBlock,XyzInOut_D,Length,iFace)

    ! Follow trace identified by index array iStart_D,
    ! starting at initial position XyzInOut_D inside block iBlock,
    ! in direction iRay until we hit the wall of the block or the ionosphere
    ! or the SM equatorial plane (if required).
    ! Return XyzInOut_D with the final position.
    ! Integrate and/or extract values if required.
    ! Also return Length increased by the length of the trace in this block.
    !
    ! Return iFace = 1..6 if the trace hit outer domain boundary
    ! Return RayBlock_   if the trace hit the block boundary
    ! Return RayIono_    if the trace hit the ionosphere
    ! Return RayLoop_    if the trace did not hit anything
    ! Return RayBody_    if the trace goes into or is inside a body
    ! Return RayOpen_    if the trace goes outside the computational box

    use ModGeometry, ONLY: Coord111_DB, XyzMax_D, XyzMin_D, &
         rMin_B, xMinBox, xMaxBox, yMinBox, yMaxBox, zMinBox, zMaxBox
    use CON_planet, ONLY: DipoleStrength
    use ModMultiFLuid

    ! Arguments

    integer, intent(in) :: iStart_D(4)
    integer, intent(in) :: iRay
    integer, intent(in) :: iBlock
    real, intent(inout) :: XyzInOut_D(3)
    real, intent(inout) :: Length
    integer, intent(out):: iFace

    ! Local variables

    ! Block size
    real :: Dxyz_D(3)

    ! initial/mid/current points of: IJK and XYZ coordinates
    real, dimension(3) :: &
         IndIni_D, IndMid_D, IndCur_D, XyzIni_D, XyzMid_D, XyzCur_D

    ! General coordinates and reference Ijk
    real, dimension(3) :: Gen_D, Ind_D

    ! Direction of B field, true interpolated field
    real, dimension(3) :: bNormIni_D, bNormMid_D, b_D

    ! Radial distance from origin and square
    real :: rCur, r2Cur, rIni

    ! dx is the difference between 1st and 2nd order RK to estimate accuracy
    ! DxOpt is the required accuracy, DxRel=dx/DxOpt
    real :: DxRel, DxOpt

    ! trace step size, step length, next step size
    real :: Ds, Ds01, dLength, DsNext

    ! Fraction of the last step inside the ionosphere
    real :: Fraction

    ! Step size limits
    real :: DsMax, DsMin, DsTiny

    ! counter for trace integration
    integer :: nSegment, MaxSegment

    ! True if rMin_B < rTrace
    logical :: DoCheckInnerBc

    ! Counter for entering do_follow_iono
    integer :: nIono

    ! Control volume limits in local coordinates
    real:: GenMin_D(3), GenMax_D(3)

    ! Cell indices corresponding to current or final Ijk position
    integer :: i1,j1,k1,i2,j2,k2

    ! Distance between Ijk and i1,j1,k1, and i2,j2,k2
    real :: Dx1, Dy1, Dz1, Dx2, Dy2, Dz2

    ! Ds/B in physical units
    real :: InvBDl, RhoP_V(nExtraIntegral)

    ! Debugging
    logical :: DoDebug=.false.

    logical :: IsWall

    character(len=*), parameter:: NameSub = 'follow_ray_block'
    !--------------------------------------------------------------------------
    if(DoTestRay)write(*,'(a,3i4,3es12.4)')&
         'Starting follow_ray_block: me,iBlock,iRay,XyzInOut_D=',&
         iProc,iBlock,iRay,XyzInOut_D

    ! Store local block deltas
    Dxyz_D  = CellSize_DB(:,iBlock)

    ! Convert initial position to block coordinates
    XyzCur_D = XyzInOut_D
    call xyz_to_ijk(XyzCur_D, IndCur_D, iBlock, &
         XyzCur_D, Coord111_DB(:,iBlock), Dxyz_D)

    ! Set flag if checking on the ionosphere is necessary
    if(UseOldMethodOfRayTrace .and. IsCartesianGrid)then
       DoCheckInnerBc = rMin_B(iBlock) < rTrace + sum(Dxyz_D)
    else
       DoCheckInnerBc = rMin_B(iBlock) < 1.2*rTrace
    end if

    ! Set the boundaries of the control volume in block coordinates
    ! We go out to the first ghost cell centers for sake of speed and to avoid
    ! problems at the boundaries
    GenMin_D = [0.0, 0.0, 0.0]
    GenMax_D = [nI+1.0, nJ+1.0, nK+1.0]

    ! Go out to the block interface at the edges of the computational domain
    where(Coord111_DB(:,iBlock)+Dxyz_D*(GenMax_D-1.0) > XyzMax_D) &
         GenMax_D = GenMax_D - 0.5
    where(Coord111_DB(:,iBlock)+Dxyz_D*(GenMin_D-1.0) < XyzMin_D) &
         GenMin_D = GenMin_D + 0.5
    if(.not.IsCartesianGrid)then
       GenMin_D(2)=0.0;  GenMax_D(2)=nJ+1.0
       GenMin_D(3)=0.0;  GenMax_D(3)=nK+1.0
    end if

    ! Step size limits
    if(UseOldMethodOfRayTrace .and. IsCartesianGrid)then
       DsMax = 1.0
       DsMin = 0.05
       DsTiny= 1.e-6
    else
       DsMax = sum(abs(Xyz_DGB(:,nI,nJ,nK,iBlock)-Xyz_DGB(:,1,1,1,iBlock))) &
            /(nI + nJ + nK - 3)/AccuracyFactor
       DsMin = DsMax*0.05
       DsTiny= DsMax*1.e-6
    end if

    MaxSegment = AccuracyFactor*10*(nI+nJ+nK)

    ! Initial value
    DsNext=sign(DsMax, 1.5-iRay)

    ! Accuracy in terms of a kind of normalized coordinates
    DxOpt = 0.01*DsMax

    ! Reference Ijk
    Ind_D = [ nI/2, nJ/2, nK/2 ]

    ! Length and maximum length of trace within control volume
    nSegment = 0
    nIono    = 0

    IsWall=.false.

    ! Integration loop
    FOLLOW: do

       ! Integrate with 2nd order scheme
       Ds    = DsNext
       IndIni_D = IndCur_D
       XyzIni_D = XyzCur_D

       ! Half step
       call interpolate_b(XyzIni_D, IndIni_D, b_D, bNormIni_D)
       if(sum(bNormIni_D**2) < 0.5)then
          iFace = RayLoop_
          EXIT FOLLOW
       end if
       if(UseOldMethodOfRayTrace .and. IsCartesianGrid)then
          IndMid_D = IndIni_D + 0.5*Ds*bNormIni_D
          XyzMid_D = Coord111_DB(:,iBlock) + Dxyz_D*(IndMid_D - 1.)
       else
          HALF: do
             ! Try a half step in XYZ space (and get IJK from it)
             XyzMid_D = XyzIni_D + 0.5*Ds*bNormIni_D
             call xyz_to_ijk(XyzMid_D, IndMid_D, iBlock, &
                  XyzIni_D, Coord111_DB(:,iBlock), Dxyz_D)

             ! Check if it stepped too far, cut step if needed
             if(       any(IndMid_D<(GenMin_D-0.5)) &
                  .or. any(IndMid_D>(GenMax_D+0.5)) )then
                ! Step too far, reduce and try again
                Ds = 0.5*Ds

                if(abs(Ds) < DsMin)then
                   ! Cannot reduce Ds further
                   Ds = 0.0
                   ! Obtain a point outside the block by mirroring the block
                   ! center Ind_D to the starting location of this step
                   ! IndIni_D
                   IndMid_D = 2*IndIni_D - Ind_D

                   ! Reduce length of Ind_D --> IndMid_D vector to end
                   ! something like a 10th of a cell outside the block
                   Ds01 = 1.1* &
                        (1-maxval(max(GenMin_D-IndMid_D, IndMid_D-GenMax_D) &
                        /(abs(IndMid_D-IndIni_D)+DsTiny)))
                   IndMid_D = IndIni_D + Ds01*(IndMid_D - IndIni_D)

                   ! Make sure that IndMid_D is just outside the
                   ! control volume
                   IndMid_D = max(GenMin_D - 0.1, IndMid_D)
                   IndMid_D = min(GenMax_D + 0.1, IndMid_D)
                   call interpolate_xyz(IndMid_D, XyzMid_D)
                   call interpolate_b(XyzMid_D, IndMid_D, b_D, bNormMid_D)
                   IndCur_D = IndMid_D; XyzCur_D = XyzMid_D

                   ! We exited the block and have a good location to
                   ! continue from
                   IsWall = .true.
                   EXIT HALF
                end if
             else
                ! Step was OK, continue
                EXIT HALF
             end if
          end do HALF
       end if

       ! Extract trace values using around IndIni_D
       if(DoExtractRay)call ray_extract(IndIni_D,XyzIni_D)

       STEP: do
          if(IsWall)EXIT STEP

          ! Full step

          bNormMid_D = bNormIni_D
          ! In case interpolation would give zero vector
          call interpolate_b(XyzMid_D, IndMid_D, b_D, bNormMid_D)

          ! Calculate the difference between 1st and 2nd order integration
          ! and take ratio relative to DxOpt
          DxRel = abs(Ds) * maxval(abs(bNormMid_D-bNormIni_D)) / DxOpt

          if(DoTestRay.and.DoDebug)&
               write(*,*)'me,iBlock,IndMid_D,bNormMid_D,DxRel=', &
               iProc,iBlock,IndMid_D,bNormMid_D,DxRel

          ! Make sure that Ds does not change more than a factor of 2 or 0.5
          DxRel = max(0.5, min(2., DxRel))

          if(DxRel > 1.)then
             ! Not accurate enough, decrease Ds if possible

             if(abs(Ds) <= DsMin + DsTiny)then
                ! Cannot reduce Ds further
                DsNext=Ds
                EXIT STEP
             end if

             Ds = sign(max(DsMin,abs(Ds)/(DxRel+0.001)),Ds)

             ! New mid point using the reduced Ds
             if(UseOldMethodOfRayTrace .and. IsCartesianGrid)then
                IndMid_D = IndIni_D + 0.5*Ds*bNormIni_D
                XyzMid_D = Coord111_DB(:,iBlock) + Dxyz_D*(IndMid_D - 1.)
             else
                HALF2: do
                   ! Try new half step in XYZ space (and get IJK from it)
                   XyzMid_D = XyzIni_D + 0.5*Ds*bNormIni_D
                   call xyz_to_ijk(XyzMid_D, IndMid_D, iBlock, &
                        XyzIni_D, Coord111_DB(:,iBlock), Dxyz_D)

                   ! Check if it stepped too far, cut step if needed
                   if(       any(IndMid_D < (GenMin_D-0.5)) &
                        .or. any(IndMid_D > (GenMax_D+0.5)))then
                      ! Step too far, reduce and try again
                      Ds=0.5*Ds

                      if(abs(Ds) < DsMin)then
                         ! Cannot reduce Ds further
                         Ds = 0.
                         ! Obtain a point outside the block by mirroring block
                         ! center Ind_D to starting location of step IndIni_D
                         IndMid_D=2.*IndIni_D-Ind_D

                         ! Reduce length of Ind_D --> IndMid_D vector to end
                         ! something like a 10th of a cell outside the block
                         Ds01 = 1.1* &
                              (1 - maxval(max(GenMin_D - IndMid_D, &
                              IndMid_D-GenMax_D) &
                              /(abs(IndMid_D - IndIni_D) + DsTiny)))
                         IndMid_D=IndIni_D+Ds01*(IndMid_D-IndIni_D)

                         ! Make sure IndMid_D is just outside the
                         ! control volume
                         IndMid_D = max(GenMin_D-0.1, IndMid_D)
                         IndMid_D = min(GenMax_D+0.1, IndMid_D)
                         call interpolate_xyz(IndMid_D, XyzMid_D)
                         call interpolate_b(XyzMid_D, IndMid_D, b_D, &
                              bNormMid_D)
                         IndCur_D=IndMid_D; XyzCur_D=XyzMid_D

                         ! We exited block and have good location to continued
                         IsWall = .true.
                         EXIT HALF2
                      end if
                   else
                      ! Step was OK, continue
                      EXIT HALF2
                   end if
                end do HALF2
             end if

             if(DoTestRay.and.DoDebug) write(*,*) &
                  'new decreased Ds: me,iBlock,Ds=',iProc,iBlock,Ds
          else
             ! Too accurate, increase Ds if possible
             if(abs(Ds) < DsMax - DsTiny)then
                DsNext = sign(min(DsMax, abs(Ds)/sqrt(DxRel)), Ds)

                if(DoTestRay.and.DoDebug) write(*,*) &
                     'new increased DsNext: me,iBlock,DsNext=', &
                     iProc, iBlock, DsNext
             end if
             EXIT STEP
          end if
       end do STEP

       ! Update position after the full step
       if(.not.IsWall)then
          if(UseOldMethodOfRayTrace .and. IsCartesianGrid)then
             IndCur_D = IndIni_D + bNormMid_D*Ds
             XyzCur_D = Coord111_DB(:,iBlock) + Dxyz_D*(IndCur_D - 1.)
          else
             XyzCur_D = XyzIni_D + Ds*bNormMid_D
             call xyz_to_ijk(XyzCur_D, IndCur_D, iBlock, &
                  XyzIni_D, Coord111_DB(:,iBlock), Dxyz_D)

             ! Check if it stepped too far, use midpoint if it did
             if(       any(IndCur_D < (GenMin_D - 0.5)) &
                  .or. any(IndCur_D > (GenMax_D + 0.5)))then
                IndCur_D = IndMid_D; XyzCur_D = XyzMid_D
             end if
          end if
       end if  ! .not.IsWall

       ! Update number of segments
       nSegment = nSegment + 1

       ! Step size in MH units  !!! Use simpler formula for cubic cells ???
       if(UseOldMethodOfRayTrace .and. IsCartesianGrid)then
          dLength = abs(Ds)*norm2(bNormMid_D*Dxyz_D)
       else
          dLength = norm2(XyzCur_D - XyzIni_D)
       end if

       ! Update trace length
       Length  = Length + dLength

       ! Check SM equator crossing for trace integral (GM -> RCM)
       ! or if we map to the equator (HEIDI/RAM-SCB -> GM)
       ! but don't check if we map equator to ionosphere (GM -> HEIDI/RAM-SCB)
       if(DoIntegrateRay .or. (DoMapEquatorRay .and. .not.DoMapRay))then
          ! Check if we crossed the z=0 plane in the SM coordinates
          ! Stop following trace if the function returns true
          if(do_stop_at_sm_equator()) EXIT FOLLOW
       end if

       if(DoIntegrateRay)then

          ! Interpolate density and pressure
          ! Use the last indexes and distances already set in interpolate_b
          RhoP_V = &
               +Dx1*(Dy1*(Dz1*Extra_VGB(:,i2,j2,k2,iBlock)   &
               +          Dz2*Extra_VGB(:,i2,j2,k1,iBlock))  &
               +     Dy2*(Dz1*Extra_VGB(:,i2,j1,k2,iBlock)   &
               +          Dz2*Extra_VGB(:,i2,j1,k1,iBlock))) &
               +Dx2*(Dy1*(Dz1*Extra_VGB(:,i1,j2,k2,iBlock)   &
               +          Dz2*Extra_VGB(:,i1,j2,k1,iBlock))  &
               +     Dy2*(Dz1*Extra_VGB(:,i1,j1,k2,iBlock)   &
               +          Dz2*Extra_VGB(:,i1,j1,k1,iBlock)))

          ! Calculate physical step size divided by physical field strength
          InvBDl = dLength / norm2(b_D)

          ! Intgrate field line volume = \int Ds/B
          RayIntegral_V(InvB_) = RayIntegral_V(InvB_) + InvBDl

          ! Integrate density and pressure = \int Rho Ds/B and \int P Ds/B
          RayIntegral_V(RhoInvB_:nLocalIntegral) = &
               RayIntegral_V(RhoInvB_:nLocalIntegral) + InvBDl * RhoP_V

       end if

       if(DoTestRay.and.DoDebug)&
            write(*,*)'me,iBlock,nSegment,IndCur_D=', &
            iProc,iBlock,nSegment,IndCur_D

       ! Check if we got inside the ionosphere
       if(DoCheckInnerBc)then
          r2Cur = sum(XyzCur_D**2)

          if(r2Cur <= rTrace2)then

             ! If inside surface, then tracing is finished

             if(NameVectorField /= 'B' .or. r2Cur < rInner2)then
                XyzInOut_D = XyzCur_D
                iFace=RayIono_
                EXIT FOLLOW
             end if

             ! Try mapping down to rIonosphere if we haven't tried yet (a lot)
             if(nIono<5)then
                if(.not.do_follow_iono())then
                   ! We did not hit the surface of the ionosphere
                   ! continue the integration
                   nIono=nIono+1
                else
                   if(DoTestRay)write(*,'(a,a,3i4,6es12.4)')&
                        'Inside rTrace at ', &
                        'me,iBlock,nSegment,IndCur_D,XyzCur_D=',&
                        iProc, iBlock, nSegment, IndCur_D, XyzCur_D

                   rCur = sqrt(r2Cur)
                   rIni = norm2(XyzIni_D)

                   ! The fraction of last step inside body is estimated from
                   ! the radii.
                   Fraction = (rTrace - rCur) / (rIni - rCur)

                   ! Reduce trace length
                   Length = Length - Fraction * dLength

                   ! Recalculate position
                   IndCur_D = IndCur_D - Fraction*(IndCur_D-IndIni_D)
                   call interpolate_xyz(IndCur_D,XyzCur_D)

                   if(DoIntegrateRay)then
                      ! Reduce Integral_I with the fraction of the last step
                      if(DoTestRay)write(*,'(a,4es12.4)')&
                           'Before reduction InvBdl, RayIntegral_V=', InvBdl, &
                           RayIntegral_V(InvB_),RayIntegral_V(RhoInvB_:pInvB_)

                      ! Recalculate dLength/abs(B)
                      InvBDl = Fraction * InvBDl

                      ! Reduce field line volume
                      RayIntegral_V(InvB_) = RayIntegral_V(InvB_) - InvBDl

                      ! Reduce density and pressure Integral_I
                      RayIntegral_V(RhoInvB_:nLocalIntegral) = &
                           RayIntegral_V(RhoInvB_:nLocalIntegral) &
                           - InvBDl*RhoP_V

                      if(DoTestRay)then
                         write(*,'(a,4es12.4)')&
                              'After  reduction InvBdl, RayIntegral_V=', &
                              InvBdl, RayIntegral_V(InvB_), &
                              RayIntegral_V(RhoInvB_:pInvB_)

                         write(*,*)'Reduction at InvBDl,RhoP_V   =', &
                              InvBDl,RhoP_V
                         write(*,*)'Reduction rIni,rCur,rTrace =',&
                              rIni,rCur,rTrace
                      end if

                   end if

                   ! Exit integration loop. XyzInOut_D is set by do_follow_iono
                   iFace=RayIono_
                   EXIT FOLLOW
                end if
             end if
          end if
       end if

       ! Check if the trace hit the wall of the control volume
       if(any(IndCur_D<GenMin_D) .or. any(IndCur_D>GenMax_D))then
          ! Compute generalized coords without pole or edge wrapping
          call xyz_to_coord(XyzCur_D,Gen_D)

          if(any(Gen_D < XyzMin_D) .or. any(Gen_D > XyzMax_D))then
             iFace = RayOpen_
          else
             iFace = RayBlock_
          end if

          XyzInOut_D = XyzCur_D
          EXIT FOLLOW
       end if

       if(.not.IsCartesianGrid)then
          ! Can also hit wall if spherical before reaching GenMin_D,GenMax_D
          if(  XyzCur_D(1) < xMinBox .or. XyzCur_D(2) < yMinBox &
               .or. XyzCur_D(3)<zMinBox .or. &
               XyzCur_D(1) > xMaxBox .or. XyzCur_D(2) > yMaxBox &
               .or. XyzCur_D(3)>zMaxBox )then

             XyzInOut_D = XyzCur_D
             iFace = RayOpen_
             EXIT FOLLOW
          end if
       end if

       ! Check if we have integrated for too long
       if( nSegment > MaxSegment .or. Length > RayLengthMax )then
          ! Seems to be a closed loop within a block
          if(DoTestRay) &
               write(*,*)'CLOSED LOOP at me,iBlock,IndCur_D,XyzCur_D=', &
               iProc, iBlock, IndCur_D, XyzCur_D

          iFace=RayLoop_
          EXIT FOLLOW
       end if

    end do FOLLOW

    ! Extract last point if trace is Done.
    if(iFace /= RayBlock_ .and. DoExtractRay) &
         call ray_extract(IndCur_D,XyzCur_D)

    if(DoTestRay) then
       write(*,'(a,4i4)')&
            'Finished follow_ray_block at me,iBlock,nSegment,iFace=',&
            iProc,iBlock,nSegment,iFace
       write(*,'(a,i4,9es12.4)')&
            'Finished follow_ray_block at me,IndCur_D,XyzCur_D,XyzInOut_D=',&
            iProc,IndCur_D,XyzCur_D,XyzInOut_D
    end if

  contains
    !==========================================================================
    logical function do_stop_at_sm_equator()

      ! Check if we crossed the Z=0 plane in the SM coordinate system
      ! Return true if there is no reason to follow the trace further

      ! SM coordinates
      real:: XyzSMIni_D(3), XyzSMCur_D(3), XySm_D(2)

      real:: Dz1, Dz2

      !------------------------------------------------------------------------
      do_stop_at_sm_equator = .false.

      ! Convert GM position into SM frame using the transposed GmSm_DD
      XyzSMIni_D = matmul(XyzIni_D, GmSm_DD)
      XyzSMCur_D = matmul(XyzCur_D, GmSm_DD)

      ! Check if we have crossed the magnetic equator in the SM frame
      if(XyzSMCur_D(3)*XyzSMIni_D(3) > 0) RETURN

      ! Crossing the magnetic equator in opposite direction is not accepted
      if(DipoleStrength*(iRay-1.5)<0)then
         if(XyzSMIni_D(3) <= 0 .and. XyzSMCur_D(3) >= 0)then
            iFace = RayLoop_
            do_stop_at_sm_equator = .true.
            RETURN
         end if
      else
         if(XyzSMIni_D(3) >= 0 .and. XyzSMCur_D(3) <= 0)then
            iFace = RayLoop_
            do_stop_at_sm_equator = .true.
            RETURN
         end if
      end if

      ! Interpolate x and y
      Dz1 = abs(XyzSMIni_D(3))/(abs(XyzSMCur_D(3)) + abs(XyzSMIni_D(3)))
      Dz2 = 1.0 - Dz1
      XySM_D = Dz2*XyzSMIni_D(1:2) + Dz1*XyzSMCur_D(1:2)

      if(DoIntegrateRay)then
         RayIntegral_V(Z0x_:Z0y_) = XySm_D

         ! Assign Z0b_ as the middle point value of the magnetic field
         RayIntegral_V(Z0b_) = norm2(b_D)
         if(DoTestRay)then
            write(*,'(a,3es12.4)') &
                 'Found z=0 crossing at XyzSMIni_D=',XyzSMIni_D
            write(*,'(a,3es12.4)') &
                 'Found z=0 crossing at XyzSMCur_D=',XyzSMCur_D
            write(*,'(a,3es12.4)')&
                 'RayIntegral_V(Z0x_:Z0b_)=',RayIntegral_V(Z0x_:Z0b_)
         end if
      elseif(DoMapEquatorRay)then
         ! Stop at the equator and store final SM coordinates
         XyzInOut_D(1:2) = XySM_D
         XyzInOut_D(3)   = 0.0
         iFace = RayEquator_
         do_stop_at_sm_equator = .true.
      end if

    end function do_stop_at_sm_equator
    !==========================================================================
    subroutine interpolate_b(XyzIn_D, IndIn_D, b_D, bNorm_D)

      ! Interpolate the magnetic field at normalized location IndIn_D
      ! and return the result in b_D.
      ! The direction of b_D (normalized to a unit vector) is returned
      ! in bNorm_D if the magnitude of b_D is not zero.

      real, intent(in) :: XyzIn_D(3)  ! location in true coordinates
      real, intent(in) :: IndIn_D(3)  ! location in normalized coordinates
      real, intent(out):: b_D(3)      ! interpolated magnetic field
      real, intent(out):: bNorm_D(3)  ! unit magnetic field vector

      ! local variables

      real :: Dir0_D(3), b0_D(3)
      ! Determine cell indices corresponding to location IndIn_D
      !------------------------------------------------------------------------
      i1 = floor(IndIn_D(1)); i2=i1+1
      j1 = floor(IndIn_D(2)); j2=j1+1
      k1 = floor(IndIn_D(3)); k2=k1+1

      ! Distance relative to the cell centers
      Dx1 = IndIn_D(1) - i1; Dx2 = 1.0 - Dx1
      Dy1 = IndIn_D(2) - j1; Dy2 = 1.0 - Dy1
      Dz1 = IndIn_D(3) - k1; Dz2 = 1.0 - Dz1

      ! Interpolate the magnetic field
      b_D = Dx1*(Dy1*(Dz1*b_DGB(:,i2,j2,k2,iBlock)   &
           +          Dz2*b_DGB(:,i2,j2,k1,iBlock))  &
           +     Dy2*(Dz1*b_DGB(:,i2,j1,k2,iBlock)   &
           +          Dz2*b_DGB(:,i2,j1,k1,iBlock))) &
           +Dx2*(Dy1*(Dz1*b_DGB(:,i1,j2,k2,iBlock)   &
           +          Dz2*b_DGB(:,i1,j2,k1,iBlock))  &
           +     Dy2*(Dz1*b_DGB(:,i1,j1,k2,iBlock)   &
           +          Dz2*b_DGB(:,i1,j1,k1,iBlock)))

      if(UseB0 .and. DoGetB0)then
         call get_b0(XyzIn_D, b0_D)
         b_D = b_D + b0_D
      end if

      ! Set bNorm_D as a unit vector. It will be zero if b_D is zero.
      if(.not.(UseOldMethodOfRayTrace .and. IsCartesianGrid))then
         bNorm_D = b_D/max(1e-30, norm2(b_D))
      else
         ! Stretch according to normalized coordinates
         Dir0_D = b_D/Dxyz_D
         bNorm_D = Dir0_D/max(1e-30, norm2(Dir0_D))
      end if

    end subroutine interpolate_b
    !==========================================================================
    subroutine interpolate_xyz(IndIn_D,XyzOut_D)

      ! !! We should use share/Library/src/ModInterpolate !!!

      ! Interpolate X/Y/Z at normalized location IndIn_D
      ! and return the result in XyzOut_D.

      real, intent(in)   :: IndIn_D(3)  ! Ijk location
      real, intent(out)  :: XyzOut_D(3) ! Xyz location

      ! Determine cell indices corresponding to location IndIn_D

      !------------------------------------------------------------------------
      i1=floor(IndIn_D(1)); i2=i1+1
      j1=floor(IndIn_D(2)); j2=j1+1
      k1=floor(IndIn_D(3)); k2=k1+1

      ! Distance relative to the cell centers
      Dx1 = IndIn_D(1) - i1; Dx2 = 1.0 - Dx1
      Dy1 = IndIn_D(2) - j1; Dy2 = 1.0 - Dy1
      Dz1 = IndIn_D(3) - k1; Dz2 = 1.0 - Dz1

      ! Interpolate the magnetic field
      XyzOut_D(1) = &
           +Dx1*(Dy1*(Dz1*Xyz_DGB(x_,i2,j2,k2,iBlock)&
           +          Dz2*Xyz_DGB(x_,i2,j2,k1,iBlock))  &
           +     Dy2*(Dz1*Xyz_DGB(x_,i2,j1,k2,iBlock)   &
           +          Dz2*Xyz_DGB(x_,i2,j1,k1,iBlock))) &
           +Dx2*(Dy1*(Dz1*Xyz_DGB(x_,i1,j2,k2,iBlock)   &
           +          Dz2*Xyz_DGB(x_,i1,j2,k1,iBlock))  &
           +     Dy2*(Dz1*Xyz_DGB(x_,i1,j1,k2,iBlock)   &
           +          Dz2*Xyz_DGB(x_,i1,j1,k1,iBlock)))
      XyzOut_D(2) = &
           +Dx1*(Dy1*(Dz1*Xyz_DGB(y_,i2,j2,k2,iBlock)   &
           +          Dz2*Xyz_DGB(y_,i2,j2,k1,iBlock))  &
           +     Dy2*(Dz1*Xyz_DGB(y_,i2,j1,k2,iBlock)   &
           +          Dz2*Xyz_DGB(y_,i2,j1,k1,iBlock))) &
           +Dx2*(Dy1*(Dz1*Xyz_DGB(y_,i1,j2,k2,iBlock)   &
           +          Dz2*Xyz_DGB(y_,i1,j2,k1,iBlock))  &
           +     Dy2*(Dz1*Xyz_DGB(y_,i1,j1,k2,iBlock)   &
           +          Dz2*Xyz_DGB(y_,i1,j1,k1,iBlock)))
      XyzOut_D(3) = &
           +Dx1*(Dy1*(Dz1*Xyz_DGB(z_,i2,j2,k2,iBlock)   &
           +          Dz2*Xyz_DGB(z_,i2,j2,k1,iBlock))  &
           +     Dy2*(Dz1*Xyz_DGB(z_,i2,j1,k2,iBlock)   &
           +          Dz2*Xyz_DGB(z_,i2,j1,k1,iBlock))) &
           +Dx2*(Dy1*(Dz1*Xyz_DGB(z_,i1,j2,k2,iBlock)   &
           +          Dz2*Xyz_DGB(z_,i1,j2,k1,iBlock))  &
           +     Dy2*(Dz1*Xyz_DGB(z_,i1,j1,k2,iBlock)   &
           +          Dz2*Xyz_DGB(z_,i1,j1,k1,iBlock)))

    end subroutine interpolate_xyz
    !==========================================================================
    logical function do_follow_iono()

      ! Follow trace inside ionosphere starting from XyzCur_D which is given in
      ! real coordinates and use analytic mapping.
      ! On return XyzInOut_D contains the final coordinates.
      ! Return true if it was successfully integrated down to rIonosphere,
      ! return false if the trace exited rTrace or too many integration
      ! steps were Done

      use CON_planet_field, ONLY: map_planet_field
      use CON_planet, ONLY: get_planet

      integer :: iHemisphere
      real    :: x_D(3), DipoleStrength=0.0

      !------------------------------------------------------------------------
      if(DipoleStrength==0)call get_planet(DipoleStrengthOut=DipoleStrength)
      ! write(*,*) 'a',XyzCur_D
      call map_planet_field(tSimulation, XyzCur_D, TypeCoordSystem//' NORM',&
           rIonosphere, x_D, iHemisphere)

      if(iHemisphere==0)then
         write(*,*)'iHemisphere==0 for XyzCur_D=',XyzCur_D
         write(*,*)'iBlock, iRay=',iBlock,iRay
         call stop_mpi('ERROR in do_follow_iono')
      end if

      if(iHemisphere*DipoleStrength*sign(1.0,1.5-iRay) < 0.0)then
         XyzInOut_D = x_D
         do_follow_iono = .true.
      else
         do_follow_iono = .false.
      end if

    end function do_follow_iono
    !==========================================================================
    subroutine ray_extract(x_D,Xyz_D)

      use CON_line_extract, ONLY: line_put
      use ModPhysics, ONLY: No2Si_V, UnitX_, UnitB_, UnitElectric_, iUnitPrim_V
      use ModAdvance, ONLY: State_VGB, nVar, Bx_, Bz_, Efield_DGB
      use ModElectricField, ONLY: Epot_DGB
      use ModInterpolate, ONLY: trilinear

      real, intent(in) :: x_D(3)   ! normalized coordinates
      real, intent(in) :: Xyz_D(3) ! Cartesian coordinates

      real    :: State_V(nVar), B0_D(3), PlotVar_V(4+nVar+10)
      integer :: n, iLine

      !------------------------------------------------------------------------
      PlotVar_V(1)   = Length
      PlotVar_V(2:4) = Xyz_D

      if(DoExtractUnitSi) PlotVar_V(1:4) = PlotVar_V(1:4)*No2Si_V(UnitX_)

      if(DoExtractState)then

         ! Determine cell indices corresponding to location x_D
         i1=floor(x_D(1)); i2=i1+1
         j1=floor(x_D(2)); j2=j1+1
         k1=floor(x_D(3)); k2=k1+1

         ! Distance relative to the cell centers
         Dx1 = x_D(1) - i1; Dx2 = 1.0 - Dx1
         Dy1 = x_D(2) - j1; Dy2 = 1.0 - Dy1
         Dz1 = x_D(3) - k1; Dz2 = 1.0 - Dz1

         ! Interpolate state to x_D
         State_V = &
              +Dx1*(Dy1*(Dz1*State_VGB(:,i2,j2,k2,iBlock)   &
              +          Dz2*State_VGB(:,i2,j2,k1,iBlock))  &
              +     Dy2*(Dz1*State_VGB(:,i2,j1,k2,iBlock)   &
              +          Dz2*State_VGB(:,i2,j1,k1,iBlock))) &
              +Dx2*(Dy1*(Dz1*State_VGB(:,i1,j2,k2,iBlock)   &
              +          Dz2*State_VGB(:,i1,j2,k1,iBlock))  &
              +     Dy2*(Dz1*State_VGB(:,i1,j1,k2,iBlock)   &
              +          Dz2*State_VGB(:,i1,j1,k1,iBlock)))

         ! Convert momentum to velocity
         State_V(iUx_I) = State_V(iRhoUx_I)/State_V(iRho_I)
         State_V(iUy_I) = State_V(iRhoUy_I)/State_V(iRho_I)
         State_V(iUz_I) = State_V(iRhoUz_I)/State_V(iRho_I)

         ! Add B0 to the magnetic field
         if(UseB0)then
            call get_b0(Xyz_D, B0_D)
            State_V(Bx_:Bz_) = State_V(Bx_:Bz_) + B0_D
         end if

         ! Convert to SI units if required
         if(DoExtractUnitSi) State_V = State_V*No2Si_V(iUnitPrim_V)

         PlotVar_V(5:4+nVar) = State_V

         n = 4 + nVar

         if(DoExtractCurvatureB)then

            n = n + 1

            ! Interpolate curvature of the magnetic field
            PlotVar_V(n) = &
                 trilinear(CurvatureB_GB(:,:,:,iBlock), &
                 0, nI+1, 0, nJ+1, 0, nK+1, x_D, DoExtrapolate=.false.)

            if(DoExtractUnitSi) PlotVar_V(n) = &
                 PlotVar_V(n) * No2Si_V(UnitX_)

         end if

         if(DoExtractBGradB1)then

            n = n + 3

            ! Interpolate b.grad B1 into the next 3 elements
            PlotVar_V(n-2:n) = &
                 trilinear(bGradB1_DGB(:,:,:,:,iBlock), &
                 3, 0, nI+1, 0, nJ+1, 0, nK+1, x_D, DoExtrapolate=.false.)

            if(DoExtractUnitSi) PlotVar_V(n-2:n) = &
                 PlotVar_V(n-2:n) * No2Si_V(UnitB_)/No2Si_V(UnitX_)

         end if

         if(DoExtractEfield)then

            n = n + 3
            ! Interpolate Efield into next 3 elements
            PlotVar_V(n-2:n) = &
                 trilinear(Efield_DGB(:,:,:,:,iBlock), &
                 3, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, x_D, &
                 DoExtrapolate=.false.)

            n = n + 3
            ! Interpolate Epot into next 3 elements
            PlotVar_V(n-2:n) = &
                 trilinear(Epot_DGB(:,:,:,:,iBlock), &
                 3, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, x_D, &
                 DoExtrapolate=.false.)

            if(DoExtractUnitSi) PlotVar_V(n-5:n) = &
                 PlotVar_V(n-5:n) * No2Si_V(UnitElectric_)

         end if
      else
         n = 4
      end if

      ! get a unique line index based on starting indexes
      ! ignore index 4 if nRay_D(4) is zero
      iLine = &
           ((max(0, min(nRay_D(4), iStart_D(4)-1))*nRay_D(3) &
           + max(0,iStart_D(3)-1) )*nRay_D(2) &
           + max(0,iStart_D(2)-1) )*nRay_D(1) &
           + iStart_D(1)

      if(iLine < 0)then
         write(*,*)'iLine=',iLine
         write(*,*)'nRay_D  =',nRay_D
         write(*,*)'iStart_D=',iStart_D
         call stop_mpi('DEBUG')
      end if
      call line_put(iLine,n,PlotVar_V(1:n))

    end subroutine ray_extract
    !==========================================================================
  end subroutine follow_ray_block
  !============================================================================
  subroutine ray_trace_sorted

    ! This subroutine is an experiment to sort blocks and cells such that
    ! open field lines can be found very fast.
    ! It works well for simple problems,
    ! but it does not seem to improve the performance for realistic grids

    use ModPhysics, ONLY: SolarWindBx, SolarWindBy, SolarWindBz
    use ModGeometry, ONLY: XyzMin_D, XyzMax_D, Coord111_DB
    use ModSort, ONLY: sort_quick

    integer :: iStart, iEnd, iStride, jStart, jEnd, jStride, &
         kStart, kEnd, kStride

    real    :: Weight_D(3)                 ! weights for the directions
    real    :: SortFunc_B(MaxBlock)        ! sorting function
    integer :: iBlockSorted_B(MaxBlock)    ! sorted block inxdexes

    ! index order for sorted blocks
    integer :: iSort, iSortStart, iSortEnd, iSortStride

    ! Indices corresponding to the starting point and directon of the trace
    integer :: i, j, k, iBlock, iRay

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'ray_trace_sorted'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Sort blocks according to the direction of the solar wind magnetic field
    ! so that open rays are found fast from already calculated trace values.

    ! Weight X, Y and Z according to the solar wind Bx, By, Bz components
    ! The Y and Z directions are preferred to X
    Weight_D(1) = sign(1.0,SolarWindBx)

    ! Select Y or Z direction to be the slowest changing value
    ! to maximize overlap
    if(abs(SolarWindBy) > abs(SolarWindBz))then
       Weight_D(2) = sign(100.0,SolarWindBy)
       Weight_D(3) = sign( 10.0,SolarWindBz)
    else
       Weight_D(2) = sign( 10.0,SolarWindBy)
       Weight_D(3) = sign(100.0,SolarWindBz)
    end if

    do iBlock=1,nBlock
       if(Unused_B(iBlock))then
          SortFunc_B(iBlock) = -10000.0
       else
          SortFunc_B(iBlock) = sum(Weight_D*&
               (Coord111_DB(:,iBlock) - XyzMin_D)/(XyzMax_D - XyzMin_D))
       end if
    end do

    call sort_quick(nBlock,SortFunc_B,iBlockSorted_B)

    ! Assign face trace values to cell centers

    ! nOpen = 0
    CpuTimeStartRay = MPI_WTIME()
    do iRay=1,2

       if(iRay==1)then
          iSortStart=nBlock; iSortEnd=1; iSortStride=-1
       else
          iSortStart=1; iSortEnd=nBlock; iSortStride=1
       end if

       if(iRay==1 .eqv. SolarWindBx >= 0.0)then
          iStart = nI; iEnd=1; iStride=-1
       else
          iStart = 1; iEnd=nK; iStride= 1
       end if

       if(iRay==1 .eqv. SolarWindBy >= 0.0)then
          jStart = nJ; jEnd=1; jStride=-1
       else
          jStart = 1; jEnd=nJ; jStride= 1
       end if

       if(iRay==1 .eqv. SolarWindBz >= 0.0)then
          kStart = nK; kEnd=1; kStride=-1
       else
          kStart = 1; kEnd=nK; kStride= 1
       end if

       do iSort = iSortStart, iSortEnd, iSortStride
          iBlock = iBlockSorted_B(iSort)

          do k = kStart, kEnd, kStride
             do j = jStart, jEnd, jStride
                do i = iStart, iEnd, iStride

                end do
             end do
          end do
       end do

    end do

    call test_stop(NameSub, DoTest)
  end subroutine ray_trace_sorted
  !============================================================================
  subroutine integrate_field_from_sphere( &
       nLat, nLon, Lat_I, Lon_I, Radius, NameVar)

    use CON_ray_trace, ONLY: ray_init
    use CON_planet_field, ONLY: map_planet_field
    use CON_axes, ONLY: transform_matrix
    use ModAdvance, ONLY: nVar, State_VGB, Bx_, Bz_, &
         UseMultiSpecies, nSpecies, UseElectronPressure
    use CON_line_extract, ONLY: line_init, line_collect, line_clean
    use CON_planet, ONLY: DipoleStrength
    use ModMultiFluid

    integer, intent(in):: nLat, nLon
    real,    intent(in):: Lat_I(nLat), Lon_I(nLon), Radius
    character(len=*), intent(in):: NameVar

    ! Lat_I(nLat) and Lon_I(nLon) are the coordinates of a 2D spherical
    ! grid in the SM(G) coordinate system in degrees. The 2D grid is
    ! at radius Radius given in units of planet radii.
    ! NameVar lists the variables that need to be extracted and/or integrated.
    ! The subroutine can calculate the integral of various quantities
    ! and/or extract state variables along the field lines starting from the 2D
    ! spherical grid.

    real    :: Theta, Phi, Lat, Lon, XyzIono_D(3), Xyz_D(3)
    integer :: iBlock, iFluid, iLat, iLon, iHemisphere, iRay
    integer :: iProcFound, iBlockFound, i, j, k
    integer :: nStateVar
    integer :: iError

    ! Variables for multispecies coupling
    real, allocatable :: NumDens_I(:)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'integrate_field_from_sphere'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(DoTest)write(*,*)NameSub,' starting on iProc=',iProc,&
         ' with nLat, nLon, Radius=',nLat,nLon,Radius

    call timing_start(NameSub)

    call sync_cpu_gpu('update on CPU', NameSub, State_VGB, B0_DGB)

    DoTestRay = .false.

    ! Initialize some basic variables
    DoIntegrateRay = index(NameVar, 'InvB') > 0 .or. index(NameVar, 'Z0') > 0
    DoExtractRay   = index(NameVar, '_I') > 0

    if(DoTest)write(*,*)NameSub,' DoIntegrateRay,DoExtractRay,DoTraceRay=',&
         DoIntegrateRay, DoExtractRay, DoTraceRay

    if(DoExtractRay)then
       nRay_D  = [ nLat, nLon, 0, 0 ]
       DoExtractState = .true.
       DoExtractUnitSi= .true.
       nStateVar = 4 + nVar
       call line_init(nStateVar)
    end if

    NameVectorField = 'B'

    ! (Re)initialize CON_ray_trace
    call ray_init(iComm)

    ! Fill in all ghost cells without monotone restrict
    call message_pass_cell(nVar, State_VGB, nProlongOrderIn=1)

    ! Copy magnetic field into b_DGB
    do iBlock = 1, nBlock; if(Unused_B(iBlock))CYCLE
       b_DGB(:,:,:,:,iBlock) = State_VGB(Bx_:Bz_,:,:,:,iBlock)
       ! Add B0
       if(UseB0) b_DGB(:,:,:,:,iBlock) = &
            b_DGB(:,:,:,:,iBlock) + B0_DGB(:,:,:,:,iBlock)
    end do

    if(DoIntegrateRay)then
       if(UseMultiSpecies) allocate(NumDens_I(nSpecies))
       ! Copy density and pressure into Extra_VGB
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          do k = MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
             if(UseMultiSpecies)then
                ! Set the densities
                Extra_VGB(1:3:2,i,j,k,iBlock) = &
                     State_VGB(SpeciesFirst_:SpeciesFirst_+1,i,j,k,iBlock)
                ! Calculate number densities for all species
                NumDens_I = State_VGB(SpeciesFirst_:SpeciesLast_,i,j,k,iBlock)&
                     /MassSpecies_V
                ! Calculte fraction relative to the total number density
                NumDens_I = NumDens_I/sum(NumDens_I)
                ! Store the pressures of the 1st and 2nd species
                Extra_VGB(2:4:2,i,j,k,iBlock) = &
                     State_VGB(p_,i,j,k,iBlock)*NumDens_I(1:2)
             else
                do iFluid = 1, min(2,nIonFluid)
                   Extra_VGB(2*iFluid-1,i,j,k,iBlock) = &
                        State_VGB(iRhoIon_I(iFluid),i,j,k,iBlock)
                   Extra_VGB(2*iFluid  ,i,j,k,iBlock) = &
                        State_VGB(iPIon_I(iFluid),i,j,k,iBlock)
                end do
             end if
             if(UseElectronPressure) then
               ! Pe index is the nExtraIntegral
               Extra_VGB(nExtraIntegral,i,j,k,iBlock) = &
                  State_VGB(Pe_,i,j,k,iBlock)
             end if
          end do; end do; end do
       end do
       if(UseMultiSpecies) deallocate(NumDens_I)

       allocate(&
            RayIntegral_VII(nRayIntegral,nLat,nLon), &
            RayResult_VII(nRayIntegral,nLat,nLon))
       RayIntegral_VII = 0.0
       RayResult_VII   = 0.0

    end if

    ! Transformation matrix between the SM and GM coordinates
    GmSm_DD = transform_matrix(tSimulation,'SMG',TypeCoordSystem)

    ! Integrate rays starting from the latitude-longitude pairs defined
    ! by the arrays Lat_I, Lon_I
    CpuTimeStartRay = MPI_WTIME()
    do iLat = 1, nLat

       Lat = Lat_I(iLat)
       Theta = cDegToRad*(90.0 - Lat)

       do iLon = 1, nLon

          Lon = Lon_I(iLon)
          Phi = cDegToRad*Lon

          ! Convert to SMG coordinates on the surface of the ionosphere
          call sph_to_xyz(Radius, Theta, Phi, XyzIono_D)

          ! Map from the ionosphere to rBody
          call map_planet_field(tSimulation, XyzIono_D, 'SMG NORM', &
               rBody+cTiny, Xyz_D, iHemisphere)

          ! Figure out direction of tracing outward
          if(iHemisphere*DipoleStrength>0)then
             iRay = 1
          else
             iRay = 2
          end if

          ! Check if the mapping is on the north hemisphere
          if(iHemisphere == 0)then
             !  write(*,*)NameSub,' point did not map to rBody, ',&
             !   'implement analytic Integral_I here! Lat, Lon=', Lat, Lon
             CYCLE
          end if

          ! Convert SM position to GM
          if(UseSmg) Xyz_D = matmul(GmSm_DD,Xyz_D)

          ! Find processor and block for the location
          call find_grid_block(Xyz_D,iProcFound,iBlockFound)

          ! If location is on this PE, follow and integrate trace
          if(iProc == iProcFound)then

             if(DoTest .and. iLat==iLatTest .and. iLon==iLonTest)then
                write(*,'(a,2i3,a,i3,a,i4)') &
                     'start of trace iLat, iLon=',iLat, iLon,&
                     ' found on iProc=',iProc,' iBlock=',iBlockFound
                write(*,'(a,2i4,2es12.4)')'iLon, iLat, Lon, Lat=',&
                     iLon, iLat, Lon, Lat
                write(*,'(a,3es12.4)')'XyzIono_D=',XyzIono_D
                write(*,'(a,3es12.4)')'Xyz_D    =',Xyz_D
             end if

             call follow_ray(iRay, [iLat, iLon, 0, iBlockFound], Xyz_D)

          end if
       end do
    end do

    ! Do remaining rays obtained from other PE-s
    call finish_ray

    if(DoTest .and. DoIntegrateRay .and. iLatTest<=nLat .and. iLonTest<=nLon) &
         write(*,*)NameSub,' iProc, RayIntegral_VII=',&
         iProc, RayIntegral_VII(:,iLatTest,iLonTest)

    if(DoIntegrateRay) call MPI_reduce( &
         RayIntegral_VII, RayResult_VII, nLat*nLon*nRayIntegral, &
         MPI_REAL, MPI_SUM, 0, iComm, iError)

    if(DoExtractRay)then
       call line_collect(iComm,0)
       if(iProc /= 0) call line_clean
    end if

    call exchange_messages

    call timing_stop(NameSub)

    DoIntegrateRay = .false.
    DoExtractRay   = .false.
    DoExtractState = .false.
    DoExtractUnitSi= .false.

    call test_stop(NameSub, DoTest)

  end subroutine integrate_field_from_sphere
  !============================================================================
  subroutine integrate_field_from_points(nPts, XyzPt_DI, NameVar)

    use CON_ray_trace, ONLY: ray_init
    use CON_axes, ONLY: transform_matrix
    use CON_line_extract, ONLY: line_init, line_collect, line_clean
    use ModAdvance, ONLY: nVar, State_VGB, Bx_, Bz_, &
         UseMultiSpecies, nSpecies
    use ModMultiFluid

    integer, intent(in):: nPts
    real,    intent(in):: XyzPt_DI(3,nPts)
    character(len=*), intent(in):: NameVar

    ! A 1D list of points is sent in with x,y,z values in GM coordinates.
    ! NameVar lists the variables that need to be extracted and/or integrated.
    ! The subroutine can calculate the integral of various quantities
    ! and/or extract state variables along the field lines starting from the
    ! points sent in.

    real    :: Xyz_D(3)
    integer :: iPt
    integer :: iProcFound, iBlockFound, i, j, k, iBlock, iFluid
    integer :: nStateVar
    integer :: iError

    ! Variables for multispecies coupling
    real, allocatable :: NumDens_I(:)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'integrate_field_from_points'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(DoTest)write(*,*)NameSub,' starting on iProc=',iProc,' with nPts=',nPts

    call timing_start('integrate_ray_1d')

    DoTestRay = .false.

    ! Initialize some basic variables
    DoIntegrateRay = index(NameVar, 'InvB') > 0 .or. index(NameVar, 'Z0') > 0
    DoExtractRay   = index(NameVar, '_I') > 0

    if(DoTest)write(*,*)NameSub,' DoIntegrateRay,DoExtractRay,DoTraceRay=',&
         DoIntegrateRay, DoExtractRay, DoTraceRay

    if(DoExtractRay)then
       nRay_D  = [ 2, nPts, 0, 0 ]
       DoExtractState = .true.
       DoExtractUnitSi= .true.
       nStateVar = 4 + nVar
       call line_init(nStateVar)
    end if

    NameVectorField = 'B'

    ! (Re)initialize CON_ray_trace
    call ray_init(iComm)

    do iBlock = 1, nBlock; if(Unused_B(iBlock))CYCLE
       b_DGB(:,:,:,:,iBlock) = State_VGB(Bx_:Bz_,:,:,:,iBlock)
       ! Add B0
       if(UseB0) b_DGB(:,:,:,:,iBlock) = &
            b_DGB(:,:,:,:,iBlock) + B0_DGB(:,:,:,:,iBlock)
    end do

    if(DoIntegrateRay)then
       if(UseMultiSpecies) allocate(NumDens_I(nSpecies))
       ! Copy density and pressure into Extra_VGB
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          do k = MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
             if(UseMultiSpecies)then
                ! Set the densities
                Extra_VGB(1:3:2,i,j,k,iBlock) = &
                     State_VGB(SpeciesFirst_:SpeciesFirst_+1,i,j,k,iBlock)
                ! Calculate number densities for all species
                NumDens_I = State_VGB(SpeciesFirst_:SpeciesLast_,i,j,k,iBlock)&
                     /MassSpecies_V
                ! Calculte fraction relative to the total number density
                NumDens_I = NumDens_I/sum(NumDens_I)
                ! Store the pressures of the 1st and 2nd species
                Extra_VGB(2:4:2,i,j,k,iBlock) = &
                     State_VGB(p_,i,j,k,iBlock)*NumDens_I(1:2)
             else
                do iFluid = 1, min(2,nIonFluid)
                   Extra_VGB(2*iFluid-1,i,j,k,iBlock) = &
                        State_VGB(iRho_I(iFluid),i,j,k,iBlock)
                   Extra_VGB(2*iFluid  ,i,j,k,iBlock) = &
                        State_VGB(iP_I(iFluid), i,j,k,iBlock)
                end do
             end if
          end do; end do; end do
       end do
       if(UseMultiSpecies) deallocate(NumDens_I)

       allocate(&
            RayIntegral_VII(nRayIntegral,nRay_D(1),nRay_D(2)), &
            RayResult_VII(nRayIntegral,nRay_D(1),nRay_D(2)))
       RayIntegral_VII = 0.0
       RayResult_VII   = 0.0

    end if

    ! Transformation matrix between the SM and GM coordinates
    if(UseSmg) GmSm_DD = transform_matrix(tSimulation,'SMG',TypeCoordSystem)

    ! Integrate rays
    CpuTimeStartRay = MPI_WTIME()
    do iPt = 1, nPts
       Xyz_D=XyzPt_DI(:,iPt)

       ! Find processor and block for the location
       call find_grid_block(Xyz_D,iProcFound,iBlockFound)

       ! If location is on this PE, follow and integrate trace
       if(iProc == iProcFound)then
          call follow_ray(1, [1, iPt, 0, iBlockFound], Xyz_D)
          call follow_ray(2, [2, iPt, 0, iBlockFound], Xyz_D)
       end if
    end do

    ! Do remaining rays obtained from other PE-s
    call finish_ray

    if(DoIntegrateRay) call MPI_reduce( RayIntegral_VII, RayResult_VII, &
         size(RayIntegral_VII), MPI_REAL, MPI_SUM, 0, iComm, iError)

    if(DoExtractRay)then
       call line_collect(iComm,0)
       if(iProc /= 0) call line_clean
    end if

    call timing_stop('integrate_ray_1d')

    DoIntegrateRay = .false.
    DoExtractRay   = .false.
    DoExtractState = .false.
    DoExtractUnitSi= .false.

    call test_stop(NameSub, DoTest)

  end subroutine integrate_field_from_points
  !============================================================================
  subroutine write_plot_equator(iFile)

    use ModMain, ONLY: NamePrimitive_V
    use ModIo, ONLY: &
         StringDateOrTime, NamePlotDir, PlotRange_EI, TypePlot_I, TypeFile_I
    use ModAdvance, ONLY: nVar, Ux_, Uz_, Bx_, Bz_
    use ModIoUnit, ONLY: UnitTmp_
    use CON_line_extract, ONLY: line_get, line_clean
    use CON_axes, ONLY: transform_matrix
    use ModInterpolate, ONLY: fit_parabola
    use ModUtilities, ONLY: open_file, close_file

    integer, intent(in):: iFile

    ! Follow field lines starting from a 2D polar grid on the
    ! magnetic equatorial plane in the SM(G) coordinate system.
    ! The grid parameters are given by plot_rang(1:4, iFile)
    ! The subroutine extracts coordinates and state variables
    ! along the field lines going in both directions
    ! starting from the 2D polar grid.

    integer :: nRadius, nLon
    real    :: rMin, rMax, LonMin, LonMax
    integer :: iR, iLon
    integer :: iPoint, nPoint, nVarOut, nVarPlot, iVar
    real, allocatable:: Radius_I(:),Longitude_I(:),PlotVar_VI(:,:),PlotVar_V(:)
    real    :: SmGm_DD(3,3)

    ! Number of points along the Up and Down halves of the field line
    integer:: nPointDn, nPointUp, nPointAll

    ! Indexes of the start and end points of the Up and Down halves
    integer:: iPointMin, iPointMid, iPointMax

    ! State variables along a single field line (both halves)
    real, allocatable:: State_VI(:,:)

    ! Coordinates, state variables and curvature
    ! at the minimum B location indexed by r and Lon
    real, allocatable:: StateMinB_VII(:,:,:)

    ! Names of quantities in StateMin_VIIB
    character(len=12), allocatable:: Name_I(:)

    ! True for "eqb" plot area
    logical:: IsMinB

    ! Weights for interpolating to minimum location
    real:: Weight_I(3)

    integer:: iLine

    character(len=100) :: NameFile, NameFileEnd

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_plot_equator'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    IsMinB = TypePlot_I(iFile)(1:3) == 'eqb'

    DoExtractCurvatureB = IsMinB

    ! Extract grid info from PlotRange_EI
    ! See MH_set_parameters for TypePlot_I eqr and eqb
    nRadius = nint(PlotRange_EI(1,iFile))
    nLon    = nint(PlotRange_EI(2,iFile))
    rMin    = PlotRange_EI(3,iFile)
    rMax    = PlotRange_EI(4,iFile)
    LonMin  = cDegToRad*PlotRange_EI(5,iFile)
    LonMax  = cDegToRad*PlotRange_EI(6,iFile)

    allocate(Radius_I(nRadius), Longitude_I(nLon))
    do iR = 1, nRadius
       Radius_I(iR) = rMin + (iR-1)*(rMax - rMin)/(nRadius - 1)
    end do
    do iLon = 1, nLon
       Longitude_I(iLon) = LonMin + (iLon-1)*(LonMax - LonMin)/(nLon - 1)
    end do

    call trace_field_equator(nRadius, nLon, Radius_I, Longitude_I, .false.)

    deallocate(Radius_I, Longitude_I)

    if(iProc/=0) RETURN

    ! Set number of variables at each point along line; allocate accordingly.
    if(DoExtractCurvatureB)then
       ! length + coordinates + variables + rCurvature
       nVarPlot = nVar + 5
    else
       ! length + coordinates + variables
       nVarPlot = nVar + 4
    end if

    NameFileEnd = ""
    if(IsTimeAccurate)then
       call get_time_string
       NameFileEnd = "_t"//StringDateOrTime
    end if
    write(NameFileEnd,'(a,i7.7)') trim(NameFileEnd) // '_n',nStep
    if(TypeFile_I(iFile) == 'tec')then
       NameFileEnd = trim(NameFileEnd)//'.dat'
    else
       NameFileEnd = trim(NameFileEnd)//'.out'
    end if

    call line_get(nVarOut, nPoint)

    if(nVarOut /= nVarPlot)then
       write(*,*) NameSub,': nVarOut, nVarPlot=', nVarOut, nVarPlot
       call stop_mpi(NameSub//': nVarOut error')
    end if
    allocate(PlotVar_VI(0:nVarOut, nPoint))
    call line_get(nVarOut, nPoint, PlotVar_VI, DoSort=.true.)

    ! Convert vectors from BATSRUS coords to SM coords.
    if(UseSmg) SmGm_DD = transform_matrix(tSimulation, TypeCoordSystem, 'SMG')

    if(.not.IsMinB)then

       NameFile = trim(NamePlotDir)//"eqr"//NameFileEnd
       call open_file(FILE=NameFile)
       write(UnitTmp_, *) 'nRadius, nLon, nPoint=',nRadius, nLon, nPoint
       write(UnitTmp_, *) 'iLine l x y z rho ux uy uz bx by bz p rCurve'

       allocate(PlotVar_V(0:nVarPlot))
       do iPoint = 1, nPoint
          ! Convert vectors to SM coordinates
          PlotVar_V = PlotVar_VI(:, iPoint)
          if(UseSmg)then
             PlotVar_V(2:4) = matmul(SmGm_DD,PlotVar_V(2:4))
             PlotVar_V(4+Ux_:4+Uz_) = matmul(SmGm_DD,PlotVar_V(4+Ux_:4+Uz_))
             PlotVar_V(4+Bx_:4+Bz_) = matmul(SmGm_DD,PlotVar_V(4+Bx_:4+Bz_))
          end if
          ! Save into file
          write(UnitTmp_, *) PlotVar_V
       end do
       deallocate(PlotVar_V)

       call close_file
    else
       ! StateMinB: x,y,z,state variables and curvature at min B and Z=0
       allocate( &
            StateMinB_VII(2*(nVar+4),nRadius,nLon), &
            Name_I(2*(nVar+4)), &
            State_VI(0:nVarOut,nPoint))

       iPointMin = 1
       iPointMid = 0
       iPointMax = 0
       iLine  = 0
       do iLon = 1, nLon
          do iR = 1, nRadius

             iLine = iLine + 1   ! Collect info from both directions
             do
                if(nint(PlotVar_VI(0,iPointMid + 1)) > iLine) EXIT
                iPointMid = iPointMid + 1
             end do

             iLine = iLine + 1
             iPointMax = iPointMid + 1
             do
                if(iPointMax == nPoint) EXIT
                if(nint(PlotVar_VI(0,iPointMax + 1)) > iLine) EXIT
                iPointMax = iPointMax + 1
             end do

             ! Note: we skip one of the repeated starting point!
             nPointUp = iPointMid - iPointMin
             nPointDn = iPointMax - iPointMid
             nPointAll= nPointDn + nPointUp

             ! Skip all (half)open field lines
             if(any(RayMap_DSII(1,:,iR,iLon) < ClosedRay))then
                ! Set impossible values (density cannot be zero)
                StateMinB_VII(:,iR,iLon)  = 0.0
                ! Set coordinates to starting position in the SM Z=0 plane
                StateMinB_VII(1:2,iR,iLon)           = &
                     2*PlotVar_VI(2:3,iPointMin)
                StateMinB_VII(nVar+5:nVar+6,iR,iLon) = &
                     PlotVar_VI(2:3,iPointMin)
             else
                ! Put together the two halves
                State_VI(:,1:nPointDn) &
                     = PlotVar_VI(:,iPointMax:iPointMid+1:-1)
                State_VI(:,nPointDn+1:nPointAll) &
                     = PlotVar_VI(:,iPointMin+1:iPointMid)

                ! Flip the sign of the "length" variables for the Down half
                ! so that the length is a continuous function along the whole
                ! field line
                State_VI(1,1:nPointDn) = -State_VI(1,1:nPointDn)

                ! Find minimum of B^2
                iPoint = minloc( &
                     sum(State_VI(4+Bx_:4+Bz_,1:nPointAll)**2, DIM=1), DIM=1)

                ! Fit parabola around minimum B value using "length"
                ! as the coordinate
                call fit_parabola( &
                     State_VI(1,iPoint-1:iPoint+1), sqrt( &
                     sum(State_VI(4+Bx_:4+Bz_,iPoint-1:iPoint+1)**2, DIM=1)), &
                     Weight3Out_I=Weight_I)

                ! Don't save line index and length

                ! First nVar+4 variables are at minimum B
                ! Interpolate to minimum point obtained from fit_parabola
                do iVar = 1, nVar+4
                   StateMinB_VII(iVar,iR,iLon) = &
                        sum(State_VI(iVar+1,iPoint-1:iPoint+1)*Weight_I)
                end do

                ! Next nVar+4 variables are at z=0 (which is the start point)
                StateMinB_VII(nVar+5: ,iR,iLon) = State_VI(2:,nPointDn)

                if(UseSmg)then
                   ! Convert magnetic fields into SM coordinate system
                   StateMinB_VII(3+Bx_:3+Bz_,iR,iLon) = &
                        matmul(SmGm_DD, StateMinB_VII(3+Bx_:3+Bz_,iR,iLon))

                   StateMinB_VII(nVar+7+Bx_:nVar+7+Bz_,iR,iLon) = &
                        matmul(SmGm_DD, &
                        StateMinB_VII(nVar+7+Bx_:nVar+7+Bz_,iR,iLon))
                end if

             end if

             ! Prepare for the next line
             iPointMin = iPointMax + 1
             iPointMid = iPointMax

          end do
       end do

       ! Create list of variables for eqb file
       Name_I(1) = 'x'
       Name_I(2) = 'y'
       Name_I(3) = 'z'
       Name_I(4:nVar+3) = NamePrimitive_V
       do iVar = 3+Bx_, 3+Bz_
          Name_I(iVar) = trim(Name_I(iVar))//'SM'
       end do
       Name_I(nVar+4) = 'rCurve'
       do iVar = 1, nVar+4
          Name_I(iVar+nVar+4) = trim(Name_I(iVar))//'Z0'
       end do

       NameFile = trim(NamePlotDir)//"eqb"//NameFileEnd
       call save_plot_file( &
            NameFile, &
            TypeFileIn=TypeFile_I(iFile), &
            StringHeaderIn = 'Values at minimum B', &
            TimeIn  = tSimulation, &
            nStepIn = nStep, &
            NameVarIn_I= Name_I, &
            IsCartesianIn= .false., &
            CoordIn_DII  = StateMinB_VII(1:2,:,:), &
            VarIn_VII    = StateMinB_VII(3:,:,:))

       deallocate(StateMinB_VII, Name_I, State_VI)

    end if

    call line_clean
    deallocate(PlotVar_VI)

    ! Now save the mapping files
    NameFile = trim(NamePlotDir)//"map_north"//NameFileEnd
    call save_plot_file( &
         NameFile, &
         TypeFileIn=TypeFile_I(iFile), &
         StringHeaderIn = 'Mapping to northern ionosphere', &
         TimeIn       = tSimulation, &
         nStepIn      = nStep, &
         NameVarIn    = 'r Lon rIono ThetaIono PhiIono', &
         CoordMinIn_D = [rMin,   0.0], &
         CoordMaxIn_D = [rMax, 360.0], &
         VarIn_VII  = RayMap_DSII(:,1,:,:))

    NameFile = trim(NamePlotDir)//"map_south"//NameFileEnd
    call save_plot_file( &
         NameFile, &
         TypeFileIn=TypeFile_I(iFile), &
         StringHeaderIn = 'Mapping to southern ionosphere', &
         TimeIn       = tSimulation, &
         nStepIn      = nStep, &
         NameVarIn    = 'r Lon rIono ThetaIono PhiIono', &
         CoordMinIn_D = [rMin,   0.0], &
         CoordMaxIn_D = [rMax, 360.0], &
         VarIn_VII  = RayMap_DSII(:,2,:,:))

    deallocate(RayMap_DSII)

    DoExtractCurvatureB = .false.

    call test_stop(NameSub, DoTest)

  end subroutine write_plot_equator
  !============================================================================
  subroutine trace_field_equator(nRadius, nLon, Radius_I, Longitude_I, &
       DoMessagePass)

    use CON_ray_trace, ONLY: ray_init
    use CON_axes, ONLY: transform_matrix
    use ModAdvance, ONLY: nVar, State_VGB, Bx_, Bz_
    use ModGeometry, ONLY: CellSize_DB
    use CON_line_extract, ONLY: line_init, line_collect, line_clean
    use ModElectricField, ONLY: calc_inductive_e

    integer, intent(in):: nRadius, nLon
    real,    intent(in):: Radius_I(nRadius), Longitude_I(nLon)
    logical, intent(in):: DoMessagePass

    ! Follow field lines starting from a 2D polar grid on the
    ! magnetic equatorial plane in the SM(G) coordinate system.
    ! The grid parameters are given by the arguments.
    ! The subroutine extracts coordinates and state variables
    ! along the field lines going in both directions
    ! starting from the 2D equatorial grid.
    ! Fill in ghost cells if DoMessagePass is true.

    integer :: iR, iLon, iSide
    integer :: iProcFound, iBlockFound, iBlock, i, j, k, iError
    real    :: r, Phi, Xyz_D(3), b_D(3), b2

    real, allocatable:: b_DG(:,:,:,:)

    integer :: nStateVar

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'trace_field_equator'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(DoTest)then
       write(*,*)NameSub,' starting on iProc=',iProc,&
            ' with nRadius, nLon=', nRadius, nLon
       write(*,*)NameSub,' Radius_I   =',Radius_I
       write(*,*)NameSub,' Longitude_I=',Longitude_I
    end if

    call timing_start(NameSub)

    ! Fill in all ghost cells
    call message_pass_cell(nVar, State_VGB)

    DoTestRay = .false.

    ! Initialize some basic variables
    DoExtractRay   = .true.
    DoMapRay       = .true.

    if(DoExtractBGradB1)then
       allocate(bGradB1_DGB(3,0:nI+1,0:nJ+1,0:nK+1,nBlock))
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          do k = 0, nK+1; do j = 0, nJ+1; do i = 0, nI+1;
             b_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
             if(UseB0) b_D = b_D +  B0_DGB(:,i,j,k,iBlock)
             b_D = b_D/sqrt(max(1e-30,sum(b_D**2)))

             bGradB1_DGB(:,i,j,k,iBlock) = &
                  0.5*b_D(1) *  &
                  ( State_VGB(Bx_:Bz_,i+1,j,k,iBlock)     &
                  - State_VGB(Bx_:Bz_,i-1,j,k,iBlock)) / &
                  CellSize_DB(x_,iBlock) &
                  + 0.5*b_D(2) *  &
                  ( State_VGB(Bx_:Bz_,i,j+1,k,iBlock)     &
                  - State_VGB(Bx_:Bz_,i,j-1,k,iBlock)) / &
                  CellSize_DB(y_,iBlock) &
                  + 0.5*b_D(3) * &
                  ( State_VGB(Bx_:Bz_,i,j,k+1,iBlock) &
                  - State_VGB(Bx_:Bz_,i,j,k+1,iBlock)) / &
                  CellSize_DB(z_,iBlock)

          end do; end do; end do
       end do
    end if

    if(DoExtractCurvatureB)then
       allocate(CurvatureB_GB(0:nI+1,0:nJ+1,0:nK+1,nBlock), &
            b_DG(3,MinI:MaxI,MinJ:MaxJ,MinK:MaxK))
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE

          ! Calculate normalized magnetic field including B0
          do k = MinK, MaxK; do j=MinJ, MaxJ; do i = MinI,MaxI
             b_DG(:,i,j,k) = State_VGB(Bx_:Bz_,i,j,k,iBlock)
             if(UseB0) b_DG(:,i,j,k) = b_DG(:,i,j,k) + B0_DGB(:,i,j,k,iBlock)
             b2 = sum(b_DG(:,i,j,k)**2)
             if(b2 > 0)b_DG(:,i,j,k) = b_DG(:,i,j,k)/sqrt(b2)
          end do; end do; end do

          do k = 0, nK+1; do j = 0, nJ+1; do i = 0, nI+1;
             ! Calculate b.grad b
             b_D = 0.5*b_DG(1,i,j,k) *  &
                  ( b_DG(:,i+1,j,k) - b_DG(:,i-1,j,k)) / &
                  CellSize_DB(x_,iBlock) &
                  + 0.5*b_DG(2,i,j,k) *  &
                  ( b_DG(:,i,j+1,k) - b_DG(:,i,j-1,k)) / &
                  CellSize_DB(y_,iBlock) &
                  + 0.5*b_DG(3,i,j,k) * &
                  ( b_DG(:,i,j,k+1) - b_DG(:,i,j,k-1)) / &
                  CellSize_DB(z_,iBlock)

             b2 = sum(b_D**2)
             if(b2 > 0)then
                CurvatureB_GB(i,j,k,iBlock) = 1/sqrt(b2)
             else
                CurvatureB_GB(i,j,k,iBlock) = 1e30
             end if
          end do; end do; end do
       end do
    end if

    ! Calculate total, potential and inductive electric fields
    if(DoExtractEfield) call calc_inductive_e

    nRay_D  = [ 2, nRadius, nLon, 0 ]
    DoExtractState = .true.
    DoExtractUnitSi= .true.
    nStateVar = 4 + nVar
    if(DoExtractBGradB1)    nStateVar = nStateVar + 3
    if(DoExtractCurvatureB) nStateVar = nStateVar + 1
    if(DoExtractEfield)     nStateVar = nStateVar + 6
    call line_init(nStateVar)

    NameVectorField = 'B'

    ! (Re)initialize CON_ray_trace
    call ray_init(iComm)

    ! Copy magnetic field into b_DGB
    do iBlock = 1, nBlock; if(Unused_B(iBlock))CYCLE
       b_DGB(:,:,:,:,iBlock) = State_VGB(Bx_:Bz_,:,:,:,iBlock)
       ! Add B0
       if(UseB0) b_DGB(:,:,:,:,iBlock) = &
            b_DGB(:,:,:,:,iBlock) + B0_DGB(:,:,:,:,iBlock)
    end do

    ! Transformation matrix between the SM and GM coordinates
    if(UseSmg) GmSm_DD = transform_matrix(tSimulation, 'SMG', TypeCoordSystem)

    ! Integrate rays starting from the latitude-longitude pairs defined
    ! by the arrays Lat_I, Lon_I
    CpuTimeStartRay = MPI_WTIME()
    do iR = 1, nRadius

       r = Radius_I(iR)

       if(r < rBody*1.0001) CYCLE

       do iLon = 1, nLon

          Phi = Longitude_I(iLon)

          ! Convert polar coordinates to Cartesian coordinates in SM
          Xyz_D(x_) = r*cos(Phi)
          Xyz_D(y_) = r*sin(Phi)
          Xyz_D(z_) = 0.0

          ! Convert SM position to GM (Note: these are same for ideal axes)
          if(UseSmg) Xyz_D = matmul(GmSm_DD, Xyz_D)

          ! Find processor and block for the location
          call find_grid_block(Xyz_D,iProcFound,iBlockFound)

          ! If location is on this PE, follow and integrate trace
          if(iProc == iProcFound)then

             call follow_ray(1, [1, iR, iLon, iBlockFound], Xyz_D)
             call follow_ray(2, [2, iR, iLon, iBlockFound], Xyz_D)

          end if
       end do
    end do

    ! Do remaining rays obtained from other PE-s
    call finish_ray

    ! Collect all rays onto processor 0
    call line_collect(iComm,0)

    ! Clean data except on processor 0
    if(iProc /= 0)call line_clean

    ! Some procs never have their RayMap arrays allocated.
    if(.not.allocated(RayMap_DSII)) then
       allocate(RayMap_DSII(3,nRay_D(1),nRay_D(2),nRay_D(3)))
       RayMap_DSII = 0.0
    end if

    ! Collect the trace mapping info to processor 0
    call MPI_reduce_real_array( &
         RayMap_DSII, size(RayMap_DSII), MPI_SUM, 0, iComm, iError)

    if(iProc == 0)then
       do iLon = 1, nLon; do iR = 1, nRadius; do iSide = 1, 2
          if(RayMap_DSII(1,iSide,iR,iLon) < ClosedRay) CYCLE
          Xyz_D = RayMap_DSII(:,iSide,iR,iLon)
          if(UseSmg) Xyz_D = matmul(Xyz_D,GmSm_DD)
          call xyz_to_sph(Xyz_D, RayMap_DSII(:,iSide,iR,iLon))
       end do; end do; end do
    else
       deallocate(RayMap_DSII)
    end if

    if(DoExtractBGradB1) deallocate(bGradB1_DGB)
    if(DoExtractCurvatureB) deallocate(CurvatureB_GB, b_DG)
    if(DoMessagePass)call exchange_messages

    call timing_stop(NameSub)

    DoExtractRay   = .false.
    DoMapRay       = .false.
    DoExtractState = .false.
    DoExtractUnitSi= .false.

    call test_stop(NameSub, DoTest)

  end subroutine trace_field_equator
  !============================================================================
  subroutine extract_field_lines(nLine, IsParallel_I, Xyz_DI)

    ! Extract nLine trace lines parallel or anti_parallel according to
    ! IsParallel_I(nLine), starting from positions Xyz_DI(3,nLine).
    ! The results are stored by CON_line_extract.

    use CON_ray_trace, ONLY: ray_init
    use ModAdvance, ONLY: State_VGB, RhoUx_, RhoUz_, Bx_, By_, Bz_
    use ModGeometry, ONLY: CellSize_DB, x_, y_, z_

    integer, intent(in) :: nLine
    logical, intent(in) :: IsParallel_I(nLine)
    real,    intent(in) :: Xyz_DI(3, nLine)

    real    :: Xyz_D(3), Dx2Inv, Dy2Inv, Dz2Inv
    integer :: iProcFound, iBlockFound, iLine, iRay

    integer :: i, j, k, iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'extract_field_lines'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    call sync_cpu_gpu('update on CPU', NameSub, State_VGB, B0_DGB)

    ! Initialize trace parameters
    DoExtractRay  = .true.
    nRay_D = [ nLine, 0, 0, 0 ]

    ! (Re)initialize CON_ray_trace
    call ray_init(iComm)

    select case(NameVectorField)
    case('B')
       ! Copy magnetic field into b_DGB
       do iBlock = 1, nBlock; if(Unused_B(iBlock))CYCLE
          b_DGB(:,:,:,:,iBlock) = State_VGB(Bx_:Bz_,:,:,:,iBlock)
          ! Add B0
          if(UseB0) b_DGB(:,:,:,:,iBlock) = &
               b_DGB(:,:,:,:,iBlock) + B0_DGB(:,:,:,:,iBlock)
       end do
    case('U')
       ! Store momentum field (same as velocity field after normalization)
       do iBlock = 1, nBlock; if(Unused_B(iBlock))CYCLE
          b_DGB(:,:,:,:,iBlock) = State_VGB(RhoUx_:RhoUz_,:,:,:,iBlock)
       end do
    case('J')
       ! Store current
       ! !! this needs to be improved a lot:
       ! !! call get_current_D for cell centers
       ! !! call message_pass_cell(b_DGB...)
       ! !! outer boundaries???
       do iBlock = 1, nBlock; if(Unused_B(iBlock)) CYCLE
          Dx2Inv = 0.5/CellSize_DB(x_,iBlock)
          Dy2Inv = 0.5/CellSize_DB(y_,iBlock)
          Dz2Inv = 0.5/CellSize_DB(z_,iBlock)

          do k=0,nK+1; do j=0,nJ+1; do i=0,nI+1
             b_DGB(1,i,j,k,iBlock) = &
                  ( State_VGB(Bz_,i,j+1,k,iBlock) &
                  - State_VGB(Bz_,i,j-1,k,iBlock))*Dy2Inv - &
                  (State_VGB(By_,i,j,k+1,iBlock) &
                  -State_VGB(By_,i,j,k-1,iBlock))*Dz2Inv
             b_DGB(2,i,j,k,iBlock) = &
                  ( State_VGB(Bx_,i,j,k+1,iBlock) &
                  - State_VGB(Bx_,i,j,k-1,iBlock))*Dz2Inv - &
                  ( State_VGB(Bz_,i+1,j,k,iBlock) &
                  - State_VGB(Bz_,i-1,j,k,iBlock))*Dx2Inv
             b_DGB(3,i,j,k,iBlock) = &
                  ( State_VGB(By_,i+1,j,k,iBlock) &
                  - State_VGB(By_,i-1,j,k,iBlock))*Dx2Inv - &
                  ( State_VGB(Bx_,i,j+1,k,iBlock) &
                  - State_VGB(Bx_,i,j-1,k,iBlock)) &
                  * Dy2Inv
          end do; end do; end do
       end do
    case default
       call stop_mpi(NameSub//': invalid NameVectorField='//NameVectorField)
    end select

    ! Start extracting rays
    CpuTimeStartRay = MPI_WTIME()
    do iLine = 1, nLine
       Xyz_D = Xyz_DI(:,iLine)

       call find_grid_block(Xyz_D,iProcFound,iBlockFound)

       if(iProc == iProcFound)then
          if(DoTest)write(*,*)NameSub,' follows trace ',iLine,&
               ' from iProc,iBlock,i,j,k=',iProcFound, iBlockFound, i, j, k
          if(IsParallel_I(iLine))then
             iRay = 1
          else
             iRay = 2
          end if
          call follow_ray(iRay, [iLine, 0, 0, iBlockFound], Xyz_D)
       end if
    end do

    ! Do remaining rays obtained from other PE-s
    call finish_ray

    DoExtractRay = .false.

    call test_stop(NameSub, DoTest)

  end subroutine extract_field_lines
  !============================================================================
  subroutine write_plot_line(iFile)

    use ModVarIndexes, ONLY: nVar
    use ModIO, ONLY: &
         StringDateOrTime, NamePlotDir, TypePlot_I, &
         TypePlotFormat_I, IsDimensionalPlot_I, Plot_, &
         NameLine_I, nLine_I, XyzStartLine_DII, IsParallelLine_II, &
         IsSingleLine_I
    use ModWriteTecplot, ONLY: set_tecplot_var_string
    use ModMain, ONLY: NamePrimitive_V
    use ModIoUnit, ONLY: UnitTmp_
    use ModUtilities, ONLY: open_file, close_file, join_string
    use CON_line_extract, ONLY: line_init, line_collect, line_get, line_clean

    integer, intent(in) :: iFile ! The file index of the plot file

    character(len=100) :: NameFile, NameStart, StringTitle
    character(len=1500):: NameVar, StringPrimitive
    integer            :: nLineFile, nStateVar, nPlotVar
    integer            :: iPoint, nPoint, iPointNext, nPoint1

    real, pointer :: PlotVar_VI(:,:)

    integer :: iPlotFile, iLine, nLine, nVarOut

    logical :: IsSingleLine, IsIdl

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_plot_line'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Set the global ModFieldTrace variables for this plot file
    iPlotFile      = iFile - Plot_
    select case(NameLine_I(iPlotFile))
    case('A', 'B')
       NameVectorField = 'B'
    case('U','J')
       NameVectorField = NameLine_I(iPlotFile)
    case default
       write(*,*) NameSub//' WARNING invalid NameVectorField='// &
            NameVectorField//' for iPlotFile=',iPlotFile
       RETURN
    end select
    DoExtractState  = index(TypePlot_I(iFile),'pos') < 1
    DoExtractUnitSi = IsDimensionalPlot_I(iFile)

    ! Set the number lines and variables to be extracted
    nLine     = nLine_I(iPlotFile)
    nStateVar = 4
    if(DoExtractState) nStateVar = nStateVar + nVar

    ! Initialize CON_line_extract
    call line_init(nStateVar)

    ! Obtain the line data
    call extract_field_lines(nLine, IsParallelLine_II(1:nLine,iPlotFile), &
         XyzStartLine_DII(:,1:nLine,iPlotFile))

    ! Collect lines from all PE-s to Proc 0
    call line_collect(iComm,0)

    if(iProc==0)then
       call line_get(nVarOut, nPoint)
       if(nVarOut /= nStateVar)then
          write(*,*) NameSub,': nVarOut, nStateVar=', nVarOut, nStateVar
          call stop_mpi(NameSub//': nVarOut error')
       end if
       allocate(PlotVar_VI(0:nVarOut, nPoint))
       call line_get(nVarOut, nPoint, PlotVar_VI, DoSort=.true.)
    end if

    call line_clean

    ! Only iProc 0 works on writing the plot files
    if(iProc /= 0) RETURN

    ! Write the result into 1 or more plot files from processor 0

    IsSingleLine = IsSingleLine_I(iPlotFile)

    if(IsSingleLine)then
       nLineFile = nLine
    else
       nLineFile = 1
    end if

    if(iPlotFile < 10)then
       write(NameStart,'(a,i1,a)') &
            trim(NamePlotDir)//trim(TypePlot_I(iFile))//'_',iPlotFile
    else
       write(NameStart,'(a,i2,a)') &
            trim(NamePlotDir)//trim(TypePlot_I(iFile))//'_',iPlotFile
    end if
    NameStart = trim(NameStart)//'_'//NameLine_I(iPlotFile)

    if(IsTimeAccurate)call get_time_string

    ! Set the title
    if(IsSingleLine)then
       StringTitle = NameVectorField//' line'
    else
       StringTitle = NameVectorField//' lines'
    end if

    ! Add the string describing the units
    if(DoExtractUnitSi)then
       StringTitle = trim(StringTitle)//" in SI units"
    else
       StringTitle = trim(StringTitle)//" in normalized units"
    end if

    ! The Length is used as coordinate in the IDL file, so it is not a plot var
    nPlotVar = nStateVar - 1

    ! Add 1 for the Index array if it is needed in the plot file
    if(.not. IsSingleLine)nPlotVar = nPlotVar + 1

    ! Set the name of the variables
    select case(TypePlotFormat_I(iFile))
    case('idl')
       IsIdl = .true.
       NameVar = 'Length x y z'
       if(DoExtractState) then
          call join_string(nVar,NamePrimitive_V,StringPrimitive)
          NameVar = trim(NameVar)//' '//StringPrimitive
       end if
       if(IsSingleLine)then
          NameVar = trim(NameVar)//' iLine'
       else
          NameVar = trim(NameVar)//' Index nLine'
       end if
    case('tec')
       IsIdl = .false.
       if(DoExtractState) then
          call set_tecplot_var_string(iFile, nVar, NamePrimitive_V, NameVar)
       else
          NameVar = '"X", "Y", "Z"'
       end if
       if(.not.IsSingleLine)NameVar = trim(NameVar)//', "Index"'
       NameVar = trim(NameVar)//', "Length"'
    case default
       call stop_mpi(NameSub// &
            ' ERROR invalid plot form='//TypePlotFormat_I(iFile))
    end select

    ! Write out plot files
    ! If IsSingleLine is true write a new file for every line,
    ! otherwise write a single file

    iPointNext = 1
    do iLine = 1, nLineFile

       ! Set the file name
       NameFile = NameStart
       if(IsSingleLine .and. nLine > 1)then
          if(nLine < 10)then
             write(NameFile,'(a,i1)') trim(NameFile),iLine
          else
             write(NameFile,'(a,i2)') trim(NameFile),iLine
          end if
       end if
       if(IsTimeAccurate) NameFile = trim(NameFile)// "_t"//StringDateOrTime
       write(NameFile,'(a,i7.7,a)') trim(NameFile) // '_n',nStep

       if(IsIdl)then
          NameFile = trim(NameFile) // '.out'
       else
          NameFile = trim(NameFile) // '.dat'
       end if

       ! Figure out the number of points for this trace
       if(IsSingleLine) nPoint1 = count(nint(PlotVar_VI(0,1:nPoint))==iLine)

       call open_file(FILE=NameFile)
       if(IsIdl)then
          write(UnitTmp_,'(a79)') trim(StringTitle)//'_var11'
          write(UnitTmp_,'(i7,1pe13.5,3i3)') &
               nStep,tSimulation,1,1,nPlotVar
          if(IsSingleLine)then
             write(UnitTmp_,'(i6)') nPoint1
             write(UnitTmp_,'(es13.5)') real(iLine)
          else
             write(UnitTmp_,'(i6)') nPoint
             write(UnitTmp_,'(es13.5)') real(nLine)
          end if
          write(UnitTmp_,'(a79)') NameVar
       else
          write(UnitTmp_,'(a)')'TITLE ="'//trim(StringTitle)//'"'
          write(UnitTmp_,'(a)')'VARIABLES='//trim(NameVar)
          if(IsSingleLine)then
             write(UnitTmp_,'(a,i2.2,a,i6)')'ZONE T="'// &
                  NameVectorField//' line ',iLine,'", '//'I=',nPoint1
          else
             write(UnitTmp_,'(a,i2.2,a,i6)')'ZONE T="'// &
                  NameVectorField//' ',nLine,' lines", '//'I=',nPoint
          end if
       end if

       ! Write out data
       if(IsSingleLine)then
          ! Write out the part corresponding to this line
          do iPoint = iPointNext, iPointNext + nPoint1 - 1
             if(IsIdl)then
                ! Write Length as the first variable: the 1D coordinate
                write(UnitTmp_,'(50es18.10)') PlotVar_VI(1:nStateVar,iPoint)
             else
                ! Write Length as the last variable, so that
                ! x,y,z can be used as 3D coordinates
                write(UnitTmp_,'(50es18.10)') PlotVar_VI(2:nStateVar,iPoint),&
                     PlotVar_VI(1,iPoint)
             end if
          end do
          iPointNext = iPointNext + nPoint1
       else
          do iPoint = 1, nPoint
             if(IsIdl)then
                ! Write Index as the last variable
                write(UnitTmp_, '(50es18.10)') &
                     PlotVar_VI(1:nStateVar, iPoint), PlotVar_VI(0,iPoint)
             else
                ! Write Index and Length as the last 2 variables
                write(UnitTmp_, '(50es18.10)') &
                     PlotVar_VI(2:nStateVar, iPoint), PlotVar_VI(0:1,iPoint)
             end if
          end do
       end if
       call close_file
    end do

    deallocate(PlotVar_VI)

    DoExtractState  = .false.
    DoExtractUnitSi = .false.

    call test_stop(NameSub, DoTest)

  end subroutine write_plot_line
  !============================================================================
  subroutine xyz_to_ijk(XyzIn_D, IndOut_D, iBlock, XyzRef_D, GenRef_D, dGen_D)

    use BATL_lib, ONLY: Phi_, Theta_, x_, y_, &
         IsAnyAxis, IsLatitudeAxis, IsSphericalAxis, IsPeriodicCoord_D, &
         CoordMin_D, CoordMax_D, xyz_to_coord

    integer, intent(in) :: iBlock
    real,    intent(in) :: XyzIn_D(3), XyzRef_D(3), GenRef_D(3), dGen_D(3)
    real,    intent(out):: IndOut_D(3)

    real:: Gen_D(3)

    character(len=*), parameter:: NameSub = 'xyz_to_ijk'
    !--------------------------------------------------------------------------
    call xyz_to_coord(XyzIn_D, Gen_D)

    ! Did the trace cross the pole?
    if(IsAnyAxis)then
       if( (XyzIn_D(x_)*XyzRef_D(x_) + XyzIn_D(y_)*XyzRef_D(y_)) < 0.)then
          ! Shift Phi by +/-pi (tricky, but works)
          ! E.g. PhiRef=  5deg, Phi=186deg -->   6deg
          ! or   PhiRef=185deg, Phi=  6deg --> 186deg
          Gen_D(Phi_) = Gen_D(Phi_) &
               + GenRef_D(Phi_) - modulo((cPi + GenRef_D(Phi_)), cTwoPi)

          if(IsLatitudeAxis .or. IsSphericalAxis)then
             if(Gen_D(Theta_) > &
                  0.5*(CoordMax_D(Theta_) + CoordMin_D(Theta_)))then
                ! Mirror theta/latitude to maximum coordinate
                ! E.g. Lat=85 deg --> 95 deg, Theta=175 deg --> 185 deg
                Gen_D(Theta_) = 2*CoordMax_D(Theta_) - Gen_D(Theta_)
             else
                ! Mirror theta/latitude to minimum coordinate
                ! E.g. Lat=-85 deg --> -95 deg, Theta = 5 deg --> -5 deg
                Gen_D(Theta_) = 2*CoordMin_D(Theta_) - Gen_D(Theta_)
             end if
          end if
       end if
    end if

    ! Did the trace cross the periodic Phi boundary?
    if(Phi_ > 1)then
       if(IsPeriodicCoord_D(Phi_))then
          if    (Gen_D(Phi_) - GenRef_D(Phi_) > cPi)then
             ! Crossed from small phi direction, make Gen_D negative
             ! E.g. PhiRef=5deg Phi=355deg -> -5deg
             Gen_D(Phi_) = Gen_D(Phi_) - cTwoPi
          elseif(GenRef_D(Phi_) - Gen_D(Phi_) > cPi)then
             ! Crossed from phi~2pi direction, make Gen_D larger than 2pi
             ! E.g. PhiRef=355deg Phi=5deg -> 365deg
             Gen_D(Phi_) = Gen_D(Phi_) + cTwoPi
          end if
       end if
    end if

    ! Gen_D is set, now compute normalized generalized coordinate IndOut_D
    IndOut_D = (Gen_D - GenRef_D)/dGen_D + 1.

  end subroutine xyz_to_ijk
  !============================================================================
  subroutine write_plot_lcb(iFile)

    use CON_line_extract, ONLY: line_get, line_clean
    use CON_planet_field, ONLY: map_planet_field
    use CON_axes, ONLY: transform_matrix
    use ModIoUnit, ONLY: UnitTmp_
    use ModUtilities, ONLY: open_file, close_file
    use ModAdvance, ONLY: nVar
    use ModPhysics, ONLY: &
         Si2No_V, No2Si_V, UnitX_, UnitRho_, UnitP_, UnitB_
    use ModIO, ONLY: &
         StringDateOrTime, NamePlotDir, PlotRange_EI, TypePlot_I, IsPlotNameN

    integer, intent(in) :: iFile

    character (len=80) :: NameFile
    integer, parameter :: nPts=11, nD=6
    integer:: i,j,k, nLine, iStart,iMid,iEnd, jStart, jMid, jEnd
    integer:: iLon, nLon, iD, iLC
    integer :: iPoint, nPoint, nVarOut, iHemisphere, iError, nTP, iDirZ
    real :: PlotVar_V(0:4+nVar)
    real :: Radius, RadiusIono, Lon, zL,zU, zUs=40., xV,yV, Integral_I(3)
    real :: XyzIono_D(3), Xyz_D(3)
    real :: Smg2Gsm_DD(3,3) = i_DD
    real, allocatable :: PlotVar_VI(:,:), XyzPt_DI(:,:), zPt_I(:)
    logical :: DoMap1, DoMap2, IsOdd, DoSkip, DoSaveIntegral

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_plot_lcb'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*)NameSub, &
         ': starting for iFile=', iFile,' type=',TypePlot_I(iFile)

    ! Extract grid info from PlotRange_EI (see set_parameters for lcb plots)
    Radius = PlotRange_EI(1,iFile)
    nLon   = PlotRange_EI(2,iFile)

    DoSaveIntegral=.false.
    if(index(TypePlot_I(iFile),'int')>0) DoSaveIntegral=.true.

    if(DoTest)write(*,*)NameSub, 'Radius, nLon, DoSaveIntegral=', &
         Radius, nLon, DoSaveIntegral

    ! Use a value of 1. for these plots.
    RadiusIono = 1.
    nTP=int( (rBody-RadiusIono)/.1 )

    if(.not.allocated(XyzPt_DI)) allocate(XyzPt_DI(3,nPts), zPt_I(nPts))

    ! Transformation matrix from default (GM) to SM coordinates
    if(UseSmg) Smg2Gsm_DD = transform_matrix(tSimulation,'SMG','GSM')

    if(iProc == 0)then
       NameFile=trim(NamePlotDir)//'LCB-GM'
       if(iFile <  10) write(NameFile, '(a,i1)') trim(NameFile)//"_",iFile
       if(iFile >= 10) write(NameFile, '(a,i2)') trim(NameFile)//"_",iFile
       if(IsTimeAccurate)then
          call get_time_string
          NameFile = trim(NameFile) // "_t" // StringDateOrTime
       end if
       if(IsPlotNameN) write(NameFile,'(a,i7.7)') trim(NameFile)//"_n",nStep
       NameFile = trim(NameFile)//".dat"

       call open_file(FILE=trim(NameFile), STATUS="replace")
       write(UnitTmp_,'(a)')'TITLE="IE B traces (GM Coordinates)"'
       if(DoSaveIntegral)then
          write(UnitTmp_,'(a)') &
               'VARIABLES="X [R]", "Y [R]", "Z [R]", "1/B", "n", "p"'
       else
          write(UnitTmp_,'(a)') &
               'VARIABLES="X [R]", "Y [R]", "Z [R]"'
       end if
    end if

    do iDirZ = -1,1,2
       ! compute the last closed points on cylinder
       ! for positive and negative Z values

       do iLon=1,nLon
          Lon = (360./nLon)*(iLon-1)
          xV = Radius*cos(cDegToRad*Lon)
          yV = Radius*sin(cDegToRad*Lon)

          zL=0.;  zU=zUs*iDirZ
          DoSkip = .false.
          do iD = 1, nD
             if(DoSkip) CYCLE

             iLC = -9

             ! Create in SM coords
             XyzPt_DI(1,:) = xV
             XyzPt_DI(2,:) = yV
             do i=1,nPts
                XyzPt_DI(3,i) = zL + ((i-1)*((zU-zL)/(nPts-1)))
             end do
             zPt_I = XyzPt_DI(3,:)

             ! Convert to GM coords
             if(UseSmg) XyzPt_DI = matmul(Smg2Gsm_DD,XyzPt_DI)

             if(DoSaveIntegral)then
                call integrate_field_from_points(&
                     nPts, XyzPt_DI, 'InvB,RhoInvB,pInvB,extract_I')
             else
                call integrate_field_from_points(nPts, XyzPt_DI, 'extract_I')
             endif

             if(iProc == 0)then

                if(DoSaveIntegral) Integral_I = -1.

                call line_get(nVarOut, nPoint)
                if(nPoint > 0)then
                   ! plot variables = 'iLine l x y z rho ux uy uz bx by bz p'
                   allocate(PlotVar_VI(0:nVarOut, nPoint))
                   call line_get(nVarOut, nPoint, PlotVar_VI, DoSort=.true.)

                   k = 0
                   do iPoint = 1, nPoint
                      nLine = PlotVar_VI(0,iPoint)
                      if(k == nLine) CYCLE
                      IsOdd=.true.;  if( (nLine/2)*2 == nLine )IsOdd=.false.

                      ! finish previous line
                      if(k /= 0)then
                         if(IsOdd)then
                            iEnd = iPoint-1
                            DoMap2 = .false.
                            Xyz_D=PlotVar_VI(2:4,iEnd) * Si2No_V(UnitX_)
                            if(norm2(Xyz_D) < 1.5*rBody) DoMap2 = .true.

                            if(DoMap1 .and. DoMap2)then
                               iLC = k/2
                               jStart = iStart; jMid = iMid; jEnd = iEnd
                               if(DoSaveIntegral)then
                                  Integral_I(1) = &
                                       sum(RayResult_VII(   InvB_,:,iLC)) &
                                       * No2Si_V(UnitX_)/No2Si_V(UnitB_)
                                  Integral_I(2) = &
                                       sum(RayResult_VII(RhoInvB_,:,iLC))/ &
                                       sum(RayResult_VII(   InvB_,:,iLC)) &
                                       * No2Si_V(UnitRho_)
                                  Integral_I(3) = &
                                       sum(RayResult_VII(  pInvB_,:,iLC))/ &
                                       sum(RayResult_VII(   InvB_,:,iLC)) &
                                       * No2Si_V(UnitP_)
                               end if
                            end if
                         else
                            iEnd = iPoint-1
                            DoMap1 = .false.
                            Xyz_D=PlotVar_VI(2:4,iEnd) * Si2No_V(UnitX_)
                            if(norm2(Xyz_D) < 1.5*rBody) DoMap1 = .true.
                         end if
                      end if

                      ! start new line counters
                      k = nLine
                      if(IsOdd)then
                         iStart = iPoint
                      else
                         iMid = iPoint
                      end if
                   end do

                   ! finish last line
                   if(k/=0)then
                      iEnd = nPoint
                      DoMap2 = .false.
                      Xyz_D=PlotVar_VI(2:4,iEnd) * Si2No_V(UnitX_)
                      if(norm2(Xyz_D) < 1.5*rBody) DoMap2 = .true.

                      if(DoMap1 .and. DoMap2)then
                         iLC = k/2
                         jStart = iStart; jMid = iMid; jEnd = iEnd
                         if(DoSaveIntegral)then
                            Integral_I(1) = &
                                 sum(RayResult_VII(   InvB_,:,iLC)) &
                                 * No2Si_V(UnitX_)/No2Si_V(UnitB_)
                            Integral_I(2) = &
                                 sum(RayResult_VII(RhoInvB_,:,iLC))/ &
                                 sum(RayResult_VII(   InvB_,:,iLC)) &
                                 * No2Si_V(UnitRho_)
                            Integral_I(3) = &
                                 sum(RayResult_VII(  pInvB_,:,iLC))/ &
                                 sum(RayResult_VII(   InvB_,:,iLC)) &
                                 * No2Si_V(UnitP_)
                         end if
                      end if
                   end if

                   ! write only last closed
                   if(iD == nD .and. iLC /= -9)then
                      j = (jEnd-jStart)+2*nTP
                      write(UnitTmp_,'(a,f7.2,a,i8,a)') &
                           'ZONE T="LCB lon=', Lon, '", I=', j, &
                           ', J=1, K=1, ZONETYPE=ORDERED, DATAPACKING=POINT'
                      Xyz_D = PlotVar_VI(2:4,jMid-1) * Si2No_V(UnitX_)
                      do i=0,nTP-1
                         ! Map from the ionosphere to first point
                         call map_planet_field(tSimulation, Xyz_D, 'GSM NORM',&
                              RadiusIono+i*.1, XyzIono_D, iHemisphere)
                         if(DoSaveIntegral)then
                            write(UnitTmp_, *) XyzIono_D,Integral_I
                         else
                            write(UnitTmp_, *) XyzIono_D
                         end if
                      end do
                      do i = jMid-1, jStart+1, -1
                         PlotVar_V = PlotVar_VI(:, i)
                         Xyz_D = PlotVar_V(2:4) * Si2No_V(UnitX_)
                         if(DoSaveIntegral)then
                            write(UnitTmp_, *) Xyz_D,Integral_I
                         else
                            write(UnitTmp_, *) Xyz_D
                         end if
                      end do
                      do i = jMid, jEnd
                         PlotVar_V = PlotVar_VI(:, i)
                         Xyz_D = PlotVar_V(2:4) * Si2No_V(UnitX_)
                         if(DoSaveIntegral)then
                            write(UnitTmp_, *) Xyz_D,Integral_I
                         else
                            write(UnitTmp_, *) Xyz_D
                         end if
                      end do
                      do i = nTP-1, 0, -1
                         ! Map from last point to the ionosphere
                         call map_planet_field(tSimulation, Xyz_D, 'GSM NORM',&
                              RadiusIono+i*.1, XyzIono_D, iHemisphere)
                         if(DoSaveIntegral)then
                            write(UnitTmp_, *) XyzIono_D,Integral_I
                         else
                            write(UnitTmp_, *) XyzIono_D
                         end if
                      end do
                   end if

                   deallocate(PlotVar_VI)
                end if
                call line_clean
             end if  ! iProc==0

             if(allocated(RayIntegral_VII)) deallocate(RayIntegral_VII)
             if(allocated(RayResult_VII))   deallocate(RayResult_VII)

             ! set new zL and zU
             call MPI_Bcast(iLC, 1, MPI_INTEGER, 0, iComm, iError)
             if(iLC == -9)then
                DoSkip = .true.
             elseif(iLC == nPts)then
                zL =   zUs*iDirZ
                zU = 2*zUs*iDirZ
             else
                zL = zPt_I(iLC)
                zU = zPt_I(iLC+1)
             end if

          end do  ! iD loop

       end do  ! iLon loop

    end do  ! iDirZ loop

    if(iProc == 0) call close_file

    if(DoTest)write(*,*)NameSub,': finished'
    call test_stop(NameSub, DoTest)

  end subroutine write_plot_lcb
  !============================================================================
  subroutine write_plot_ieb(iFile)

    use CON_line_extract, ONLY: line_get, line_clean
    use CON_planet_field, ONLY: map_planet_field
    use CON_axes, ONLY: transform_matrix
    use ModIoUnit, ONLY: UnitTmp_
    use ModUtilities, ONLY: open_file, close_file
    use ModAdvance, ONLY: nVar
    use ModPhysics, ONLY: Si2No_V, UnitX_
    use ModIO, ONLY: StringDateOrTime, NamePlotDir

    integer, intent(in) :: iFile

    character(len=80) :: NameFile, String
    character(len=2) :: StringCoord
    character(len=1) :: StringNorS
    integer :: i, j, k, nLat, nLon, nLine, nTP, iStart,iEnd, iLat,iLon, iMap
    integer :: iPoint, nPoint, nVarOut, iHemisphere, nFile
    real :: PlotVar_V(0:4+nVar)
    real :: Radius, Lat,Lon, Theta,Phi, LonOC
    real :: XyzIono_D(3), Xyz_D(3)
    real :: Gsm2Smg_DD(3,3) = i_DD
    real :: Smg2Gsm_DD(3,3) = i_DD
    real, allocatable :: PlotVar_VI(:,:), IeLat_I(:), IeLon_I(:)
    logical :: DoMapDown

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_plot_ieb'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(DoTest)write(*,*)NameSub,': starting'

    nLat = 181
    nLon = 36
    if(.not.allocated(IeLat_I)) allocate(IeLat_I(nLat), IeLon_I(nLon))

    ! Load grid and convert to lat-lon in degrees
    do i = 1, nLat
       IeLat_I(i) = 90 - (i-1)
    end do
    do i = 1, nLon
       IeLon_I(i) = 10.*(i-1)
    end do
    Radius = (6378.+100.)/6378.
    nTP = 10*(rBody-Radius)

    call integrate_field_from_sphere(nLat, nLon, IeLat_I, IeLon_I, Radius, &
         'extract_I')

    if(iProc == 0)then

       ! Transformation matrix from default (GM) to SM coordinates
       Gsm2Smg_DD = transform_matrix(tSimulation,TypeCoordSystem,'SMG')
       Smg2Gsm_DD = transform_matrix(tSimulation,'SMG','GSM')

       call line_get(nVarOut, nPoint)
       if(nPoint>0)then
          ! PlotVar_VI variables = 'iLine l x y z rho ux uy uz bx by bz p'
          allocate(PlotVar_VI(0:nVarOut, nPoint))
          call line_get(nVarOut, nPoint, PlotVar_VI, DoSort=.true.)

          do nFile = 1, 4

             if(nFile==1)then
                StringCoord = 'SM';  StringNorS = 'N'
             elseif(nFile==2)then
                StringCoord = 'GM';  StringNorS = 'N'
             elseif(nFile==3)then
                StringCoord = 'SM';  StringNorS = 'S'
             elseif(nFile==4)then
                StringCoord = 'GM';  StringNorS = 'S'
             end if
             NameFile = trim(NamePlotDir)//'IEB-'//StringCoord//'-'//StringNorS
             if(IsTimeAccurate)then
                call get_time_string
                NameFile = trim(NameFile) // "_t" // StringDateOrTime
             end if
             write(NameFile,'(a,i7.7,a)') trim(NameFile)//"_n", nStep,".dat"

             call open_file(FILE=NameFile)
             if(StringCoord == 'GM')then
                write(UnitTmp_,'(a)')'TITLE="IE B traces (GM Coordinates)"'
             else
                write(UnitTmp_,'(a)')'TITLE="IE B traces (SM Coordinates)"'
             end if
             write(UnitTmp_,'(a)') &
                  'VARIABLES="X [R]", "Y [R]", "Z [R]", "Lat", "Lon", "iMap"'

             k = 0
             LonOC = -1
             do iPoint = 1, nPoint
                nLine = PlotVar_VI(0,iPoint)
                if(k /= nLine)then

                   ! finish previous line
                   if(k /= 0)then
                      iEnd = iPoint-1
                      DoMapDown = .false.
                      Xyz_D=PlotVar_VI(2:4,iEnd) * Si2No_V(UnitX_)
                      if(norm2(Xyz_D) < 1.5*rBody) DoMapDown = .true.
                      j = (1+iEnd-iStart) + (nTP+1)
                      if(DoMapDown) j = j + (nTP+1)
                      iMap = -1; if(DoMapDown) iMap = 2
                      if(DoMapDown .and. LonOC /= Lon) iMap = 1

                      write(UnitTmp_,'(a,2f7.2,a,a,f7.2,a,i8,a)') &
                           'ZONE T="IEB ll=',Lat,Lon,'"', &
                           ', STRANDID=1, SOLUTIONTIME=',Lon,', I=',j, &
                           ', J=1, K=1, ZONETYPE=ORDERED, DATAPACKING=POINT'
                      write(String,'(f8.2)')Lat
                      write(UnitTmp_,'(a,a,a)') &
                           'AUXDATA LAT="',trim(adjustl(String)),'"'
                      write(String,'(f8.2)')Lon
                      write(UnitTmp_,'(a,a,a)') &
                           'AUXDATA LON="',trim(adjustl(String)),'"'

                      ! Convert to SMG coordinates on the surface of ionosphere
                      Theta = cDegToRad*(90.0 - Lat)
                      Phi = cDegToRad*Lon
                      call sph_to_xyz(Radius, Theta, Phi, XyzIono_D)
                      Xyz_D=XyzIono_D
                      if(StringCoord == 'GM') Xyz_D = matmul(Smg2Gsm_DD,Xyz_D)
                      write(UnitTmp_, *) Xyz_D,Lat,Lon,iMap
                      do i = 1, nTP
                         ! Map from the ionosphere to rBody
                         call map_planet_field(tSimulation, XyzIono_D, &
                              'SMG NORM', &
                              Radius+i*.1, Xyz_D, iHemisphere)
                         if(StringCoord == 'GM') &
                              Xyz_D = matmul(Smg2Gsm_DD,Xyz_D)
                         write(UnitTmp_, *) Xyz_D,Lat,Lon,iMap
                      end do
                      do i = iStart, iEnd
                         ! Convert vectors to SM coordinates
                         PlotVar_V = PlotVar_VI(:, i)
                         PlotVar_V(2:4) = matmul(Gsm2Smg_DD,PlotVar_V(2:4))
                         PlotVar_V(2:4) = PlotVar_V(2:4) * Si2No_V(UnitX_)
                         Xyz_D = PlotVar_V(2:4)
                         if(StringCoord == 'GM') &
                              Xyz_D = matmul(Smg2Gsm_DD,Xyz_D)
                         write(UnitTmp_, *) Xyz_D,Lat,Lon,iMap
                      end do
                      if(DoMapDown)then
                         Xyz_D=PlotVar_V(2:4)
                         do i=nTP,0,-1
                            ! Map from rBody to the ionosphere
                            call map_planet_field(tSimulation, Xyz_D, &
                                 'SMG NORM', &
                                 Radius+i*.1, XyzIono_D, iHemisphere)
                            if(StringCoord == 'GM') &
                                 XyzIono_D = matmul(Smg2Gsm_DD, XyzIono_D)
                            write(UnitTmp_, *) XyzIono_D,Lat,Lon,iMap
                         end do
                      end if
                   end if

                   ! start new line counters
                   k=nLine
                   iStart = iPoint
                   iLon=1+((nLine-1)/nLat)
                   iLat=nLine-(iLon-1)*nLat
                   Lon=IeLon_I(iLon)
                   Lat=IeLat_I(iLat)
                   if(StringNorS == 'N')then
                      if(Lat<0.)k=0
                   else
                      if(Lat>0.)k=0
                   end if
                end if
             end do

             ! finish last line
             if(k /= 0)then
                iEnd = nPoint
                DoMapDown = .false.
                Xyz_D = PlotVar_VI(2:4,iEnd) * Si2No_V(UnitX_)
                if(norm2(Xyz_D) < 1.5*rBody) DoMapDown = .true.
                j = (1+iEnd-iStart)+(nTP+1)
                if(DoMapDown) j = j + (nTP+1)
                iMap = -1; if(DoMapDown) iMap = 2
                if(DoMapDown .and. LonOC /= Lon) iMap = 1
                ! write(UnitTmp_,'(a,2f7.2,a,i8,a)') &
                !     'ZONE T="IEB ll=',Lat,Lon,'" I=',j, &
                !     ', J=1, K=1, ZONETYPE=ORDERED, DATAPACKING=POINT'
                !-
                write(UnitTmp_,'(a,2f7.2,a,a,f7.2,a,i8,a)') &
                     'ZONE T="IEB ll=',Lat,Lon,'"', &
                     ', STRANDID=1, SOLUTIONTIME=',Lon, &
                     ', I=',j,', J=1, K=1, ZONETYPE=ORDERED, DATAPACKING=POINT'
                write(String,'(f8.2)')Lat
                write(UnitTmp_,'(a,a,a)') 'AUXDATA LAT="', &
                     trim(adjustl(String)),'"'
                write(String,'(f8.2)')Lon
                write(UnitTmp_,'(a,a,a)') 'AUXDATA LON="', &
                     trim(adjustl(String)),'"'

                ! Convert to SMG coordinates on the surface of the ionosphere
                Theta = cDegToRad*(90.0 - Lat)
                Phi = cDegToRad*Lon
                call sph_to_xyz(Radius, Theta, Phi, XyzIono_D)
                Xyz_D=XyzIono_D
                if(StringCoord == 'GM') Xyz_D = matmul(Smg2Gsm_DD,Xyz_D)
                write(UnitTmp_, *) Xyz_D,Lat,Lon,iMap
                do i = 1, nTP
                   ! Map from the ionosphere to rBody
                   call map_planet_field(tSimulation, XyzIono_D, &
                        'SMG NORM', &
                        Radius+i*.1, Xyz_D, iHemisphere)
                   if(StringCoord == 'GM') Xyz_D = matmul(Smg2Gsm_DD,Xyz_D)
                   write(UnitTmp_, *) Xyz_D,Lat,Lon,iMap
                end do
                do i = iStart, iEnd
                   ! Convert vectors to SM coordinates
                   PlotVar_V = PlotVar_VI(:, i)
                   PlotVar_V(2:4) = matmul(Gsm2Smg_DD,PlotVar_V(2:4))
                   PlotVar_V(2:4) = PlotVar_V(2:4) * Si2No_V(UnitX_)
                   Xyz_D = PlotVar_V(2:4)
                   if(StringCoord == 'GM') Xyz_D = matmul(Smg2Gsm_DD,Xyz_D)
                   write(UnitTmp_, *) Xyz_D,Lat,Lon,iMap
                end do
                if(DoMapDown)then
                   Xyz_D=PlotVar_V(2:4)
                   do i = nTP, 0, -1
                      ! Map from the ionosphere to rBody
                      call map_planet_field(tSimulation, Xyz_D, &
                           'SMG NORM', &
                           Radius+i*.1, XyzIono_D, iHemisphere)
                      if(StringCoord == 'GM') XyzIono_D = &
                           matmul(Smg2Gsm_DD, XyzIono_D)
                      write(UnitTmp_, *) XyzIono_D,Lat,Lon,iMap
                   end do
                end if
             end if

             call close_file
          end do

          deallocate(PlotVar_VI)
       end if
       call line_clean
    end if

    if(allocated(RayIntegral_VII)) deallocate(RayIntegral_VII)
    if(allocated(RayResult_VII))   deallocate(RayResult_VII)

    if(DoTest)write(*,*)NameSub,': finished'

    call test_stop(NameSub, DoTest)

  end subroutine write_plot_ieb
  !============================================================================
  subroutine trace_field_sphere

    ! Trace field lines from a spherical surface and
    ! calculate squashing factor.

    use ModGeometry, ONLY: RadiusMin
    use ModAdvance, ONLY: State_VGB, Bx_, Bz_
    use CON_ray_trace, ONLY: ray_init

    integer:: nLon, nLat, iLon, iLat, i, j, k, iBlock, iRay
    integer:: iProcFound, iBlockFound, iError
    real:: dLon, dLat, AccuracyFactorOrig
    real:: Xyz_D(3), rLonLat_D(3), B0_D(3)

    ! Squash factor related variables
    integer:: DiLon, iLon1, iLon2, iLat1, iLat2
    real:: Xyz1_D(3), Xyz2_D(3), Xyz3_D(3), Xyz4_D(3), Base1_D(3), Base2_D(3)
    real:: r_I(0:4), Jacobian_II(2,2), CosLat, DxyzDlon_D(3), DxyzDlat_D(3)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'trace_field_sphere'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    nLon = nLonSquash + 1 ! extra periodic cell in the Lon direction
    nLat = nLatSquash - 1 ! no poles in the Lat directoin

    ! Longitude and latitude resolution
    dLon = cTwoPi/nLonSquash
    dLat = cPi/nLatSquash

    AccuracyFactorOrig = AccuracyFactor
    AccuracyFactor     = AccuracyFactorSquash
    DoMapRay           = .true.
    DoMapOpen          = .true.
    nRay_D = [1, nLon, nLat, 0]
    NameVectorField = 'B'

    if(allocated(RayMap_DSII)) deallocate(RayMap_DSII)
    allocate(RayMap_DSII(3,nRay_D(1),nRay_D(2),nRay_D(3)))
    RayMap_DSII = 0.0

    call ray_init(iComm)

    ! Copy magnetic field into b_DGB
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          b_DGB(:,i,j,k,iBlock) = State_VGB(Bx_:Bz_,i,j,k,iBlock)
          ! Add B0
          if(UseB0) b_DGB(:,i,j,k,iBlock) = &
               b_DGB(:,i,j,k,iBlock) + B0_DGB(:,i,j,k,iBlock)
       end do; end do; end do
    end do
    call message_pass_cell(3, b_DGB)

    ! Fixed radial distance
    rLonLat_D(1) = max(1.0, rTrace, RadiusMin)
    CpuTimeStartRay = MPI_WTIME()
    do iLon = 1, nLon
       ! Longitude
       rLonLat_D(2) = (iLon - 1)*dLon
       do iLat = 1, nLat
          ! Latitude
          rLonLat_D(3) = iLat*dLat - cHalfPi

          ! Convert to Cartesian coordinates
          call rlonlat_to_xyz(rLonLat_D, Xyz_D)

          ! Set iRay=1 for the field pointing outward
          call get_b0(Xyz_D, b0_D)
          if(sum(Xyz_D*b0_D) > 0)then
             iRay = 1
          else
             iRay = 2
          end if

          ! Find processor and block for the location
          call find_grid_block(Xyz_D, iProcFound, iBlockFound)

          ! If location is on this PE, follow and integrate trace
          if(iProc == iProcFound)then

             if(DoTest .and. iLat==iLatTest .and. iLon==iLonTest)then
                write(*,'(a,3i3,a,i3,a,i4)') &
                     NameSub//': iLon, iLat, iRay=', iLon, iLat, iRay, &
                     ' found on iProc=',iProc,' iBlock=', iBlockFound
                write(*,'(a,3es12.4)') NameSub//': rLonLat_D=', rLonLat_D
                write(*,'(a,3es12.4)') NameSub//': Xyz_D    =', Xyz_D
                write(*,'(a,3es12.4)') NameSub//': B0_D     =', B0_D
             end if

             call follow_ray(iRay, [1, iLon, iLat, iBlockFound], Xyz_D)

          end if
       end do
    end do
    ! Do remaining rays obtained from other PE-s
    call finish_ray

    ! Collect the trace mapping info to processor 0
    if(nProc > 1) call MPI_allreduce(MPI_IN_PLACE, RayMap_DSII, &
         size(RayMap_DSII), MPI_REAL, MPI_SUM, iComm, iError)

    ! Calculate squash factor. Add poles for interpolation.
    if(.not.allocated(SquashFactor_II)) &
         allocate(SquashFactor_II(nLon,0:nLat+1))

    do iLon = 1, nLon; do iLat = 1, nLat
       SquashFactor_II(iLon,iLat) = 1.0
       Xyz_D = RayMap_DSII(:,1,iLon,iLat)
       if(Xyz_D(1) < ClosedRay) CYCLE
       CosLat = cos(iLat*dLat - cHalfPi)
       ! Increase DiLon as CosLat is getting smaller
       DiLon = min(nLon/8 + 1, nint(0.6/CosLat))
       iLon1 = iLon - DiLon; if(iLon1 < 1)    iLon1 = iLon1 + nLonSquash
       Xyz1_D = RayMap_DSII(:,1,iLon1,iLat)
       if(Xyz1_D(1) < ClosedRay) CYCLE
       iLon2 = iLon + DiLon; if(iLon2 > nLon) iLon2 = iLon2 - nLonSquash
       Xyz2_D = RayMap_DSII(:,1,iLon2,iLat)
       if(Xyz2_D(1) < ClosedRay) CYCLE
       iLat1 = max(iLat - 1, 1)
       Xyz3_D = RayMap_DSII(:,1,iLon,iLat1)
       if(Xyz3_D(1) < ClosedRay) CYCLE
       iLat2 = min(iLat + 1, nLat)
       Xyz4_D = RayMap_DSII(:,1,iLon,iLat2)
       if(Xyz4_D(1) < ClosedRay) CYCLE

       r_I(0) = norm2(Xyz_D)
       r_I(1) = norm2(Xyz1_D)
       r_I(2) = norm2(Xyz2_D)
       r_I(3) = norm2(Xyz3_D)
       r_I(4) = norm2(Xyz4_D)

       if(any(r_I < 1e-6)) CYCLE
       ! This is needed to avoid floating point exception in debug mode.
       r_I = max(1e-6, r_I)

       ! Check if all or no footpoints are on the inner boundary
       ! if(any(abs(r_I - rInner) > 0.1)) CYCLE
       ! if(any(abs(r_I - rInner) < 0.1) .and. any(abs(r_I - rInner) > 0.1)) &
       ! then
       !     SquashFactor_II(iLon,iLat) = 10.0
       !     CYCLE
       ! end if

       ! Normalize footpoints to the unit sphere
       Xyz_D  = Xyz_D/r_I(0)
       Xyz1_D = Xyz1_D/r_I(1)
       Xyz2_D = Xyz2_D/r_I(2)
       Xyz3_D = Xyz3_D/r_I(3)
       Xyz4_D = Xyz4_D/r_I(4)
       ! Calculate derivatives
       DxyzDlon_D = (Xyz2_D - Xyz1_D)/(2*DiLon*dLon*CosLat)
       DxyzDlat_D = (Xyz4_D - Xyz3_D)/((iLat2 - iLat1)*dLat)
       ! Remove radial part of Dxyz
       DxyzDlon_D = DxyzDlon_D - Xyz_D*sum(DxyzDlon_D*Xyz_D)
       DxyzDlat_D = DxyzDlat_D - Xyz_D*sum(DxyzDlat_D*Xyz_D)
       ! First base vector parallel with DxyzDLon_D
       Base1_D = DxyzDLon_D/norm2(DxyzDLon_D)
       ! Second base vector perpendicular to Base1_D
       Base2_D = cross_product(Xyz_D, Base1_D)
       ! Jacobian matrix
       Jacobian_II(1,1) = sum(DxyzDLon_D*Base1_D)
       Jacobian_II(2,1) = 0.0
       Jacobian_II(1,2) = sum(DxyzDLat_D*Base1_D)
       Jacobian_II(2,2) = sum(DxyzDLat_D*Base2_D)
       ! Calculate the squashing factor
       SquashFactor_II(iLon,iLat) = min(SquashFactorMax, &
            0.5*sum(Jacobian_II**2) &
            /max(1e-30, abs(Jacobian_II(1,1)*Jacobian_II(2,2))))

       if(DoTest .and. iLat==iLatTest .and. iLon==iLonTest)then
          write(*,'(a,3i4)') &
               NameSub//': iLon, iLat, DiLon=', iLon, iLat, DiLon
          write(*,'(a,3es12.4)') NameSub//': Xyz_D    =', Xyz_D
          write(*,'(a,3es12.4)') NameSub//': Xyz1_D   =', Xyz1_D
          write(*,'(a,3es12.4)') NameSub//': Xyz2_D   =', Xyz2_D
          write(*,'(a,3es12.4)') NameSub//': Xyz3_D   =', Xyz3_D
          write(*,'(a,3es12.4)') NameSub//': Xyz4_D   =', Xyz4_D
          write(*,'(a,3es12.4)') NameSub//': DxyzDlon =', DxyzDlon_D
          write(*,'(a,3es12.4)') NameSub//': DxyzDlat =', DxyzDlat_D
          write(*,'(a,3es12.4)') NameSub//': Base1_D  =', Base1_D
          write(*,'(a,3es12.4)') NameSub//': Base2_D  =', Base2_D
          write(*,'(a,4es12.4)') NameSub//': Jacobian =', Jacobian_II
          write(*,'(a,es12.4)')  NameSub//': Squash   =', &
               SquashFactor_II(iLon,iLat)
       end if

    end do; end do

    ! Average squash factor to the poles. Do not use repeated longitude.
    SquashFactor_II(:,0)      = &
         sum(SquashFactor_II(1:nLonSquash,1))/nLonSquash
    SquashFactor_II(:,nLat+1) = &
         sum(SquashFactor_II(1:nLonSquash,nLat))/nLonSquash

    if(DoTest .and. iProc ==0)then
       write(*,*) NameSub,': RayMap(1)=', RayMap_DSII(:,1,iLonTest,iLatTest)

       ! Convert footpoint coordinates to Squash-Lon-Lat
       do iLon = 1, nLon; do iLat = 1, nLat
          Xyz_D = RayMap_DSII(:,1,iLon,iLat)
          if(Xyz_D(1) < ClosedRay) then
             ! Impossible values for open field lines
             RayMap_DSII(2:3,1,iLon,iLat) = -100.0
          else
             call xyz_to_rlonlat(Xyz_D, rLonLat_D)
             RayMap_DSII(2:3,1,iLon,iLat) = rLonLat_D(2:3)*cRadToDeg
          end if
          ! Set first coordinate to the squash factor
          RayMap_DSII(1,1,iLon,iLat) = SquashFactor_II(iLon,iLat)
       end do; end do

       if(iProc == 0) call save_plot_file( &
            "map.out", &
            StringHeaderIn = "Field line mapping", &
            TimeIn  = tSimulation, &
            nStepIn = nStep, &
            ParamIn_I = [1.0], &
            NameVarIn = "Lon Lat Squash Lon1 Lat1 radius", &
            CoordMinIn_D = [0., 180/(nLat + 1.0) - 90], &
            CoordMaxIn_D = [360., 180*nLat/(nLat + 1.0) - 90], &
            VarIn_VII  = RayMap_DSII(:,1,:,:))
    end if

    DoMapRay        = .false.
    DoMapOpen       = .false.
    AccuracyFactor = AccuracyFactorOrig

    call test_stop(NameSub, DoTest)

  end subroutine trace_field_sphere
  !============================================================================
end module ModFieldTrace
!==============================================================================
