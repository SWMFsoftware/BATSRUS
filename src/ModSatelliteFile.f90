!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModSatelliteFile

  use BATL_lib, ONLY: &
       test_start, test_stop, lVerbose, iProc, iComm
#ifdef _OPENACC
  use ModUtilities, ONLY: norm2
#endif
  use ModUtilities, ONLY: open_file, close_file
  use ModBatsrusUtility, ONLY: get_time_string, stop_mpi
  use ModMain, ONLY: StartTime

  implicit none
  save
  private ! Except

  public:: read_satellite_parameters  ! read satellite file input parameters
  public:: set_satellite_file_status  ! open, open to append or close the file
  public:: read_satellite_input_files ! read satellite trajectories
  public:: set_satellite_flags        ! find the processor/block for satellite
  public:: get_satellite_ray ! map field line from satellite ^CFG IF RAYTRACE
  public:: gm_trace_sat      ! map field line from satellite ^CFG IF RAYTRACE

  public:: set_satellite_positions
  public:: i_sat_for_name, xyz_sat

  logical, public :: DoSaveSatelliteData = .false. ! save satellite data?
  integer, public :: nSatellite = 0                ! number of satellites

  integer, parameter :: MaxSatellite=300

  real, public :: TimeSatStart_I(MaxSatellite) = 0.
  real, public :: TimeSatEnd_I(MaxSatellite) = 0.

  ! Used if the local time in satellite location differs from tSimulation:
  logical, public :: UseSatelliteTimeOffset = .false.
  real, public :: TimeSatOffset_I(MaxSatellite) = 0.

  ! These variables are public for write_logfile only !!! Should be improved
  ! Names and unit numbers for satellite files
  character(len=50), public:: NameFileSat_I(MaxSatellite)
  character(len=10), public:: NameSat_I(MaxSatellite)
  integer, public:: iUnitSat_I(MaxSatellite) = -1
  logical, public:: IsFirstWriteSat_I(MaxSatellite) = .true.

  ! current positions
  real, public:: XyzSat_DI(3,MaxSatellite)

  ! variables to control time output format
  character(len=100), public :: TypeTimeSat_I(MaxSatellite)

  ! variables to write to the satellite files
  character(len=500), public :: StringSatVar_I(MaxSatellite)

  logical, public:: DoTrackSatellite_I(MaxSatellite) = .false.

  integer,public:: iPointCurrentSat_I(MaxSatellite)=1

  ! Local variables
  character(len=100) :: NameFile_I(MaxSatellite)
  logical:: IsNameFileSet_I(MaxSatellite) = .false.
  logical:: IsOpen_I(MaxSatellite) = .false.
  logical:: UseSatFile_I(MaxSatellite)   = .true.
  integer, public :: nPointTraj_I(MaxSatellite)
  integer, public :: iProcSat_I(MaxSatellite)
  integer, public :: iBlockSat_I(MaxSatellite)
  real, allocatable   :: XyzSat_DII(:,:,:)
  real, public, allocatable   :: TimeSat_II(:, :)

  character(len=3)  :: TypeSatCoord_I(MaxSatellite)

  real, public   :: StartTimeTraj_I(MaxSatellite), EndTimeTraj_I(MaxSatellite)
  real, public   :: DtTraj_I(MaxSatellite)
  character(len=5), public :: TypeTrajTimeRange_I(MaxSatellite) = 'orig'

  ! Time limits (in seconds) for satellite trajectory cut
  ! for .not. IsTimeAccurate session.
  ! If a steady-state simulation is run for a specific moment of time
  ! (set in  StartTime), the TimeSatStart_I determines the starting point of
  ! the satellite trajectory, while TimeSatEnd_I determines the trajectory
  ! ending point.
  ! Both determine the considered trajectory cut.
  ! Unlike in IsTimeAccurate sessions, after each DnOutput_I simulation
  ! steps the satellite variables for ALL the trajectory cut are
  ! saved in file.

contains
  !============================================================================
  subroutine read_satellite_parameters(NameCommand)

    use ModIO, ONLY: nFile, MaxFile, Satellite_, IsDimensionalPlot_I, &
         DnOutput_I, DtOutput_I, TypePlot_I, NamePlotDir, TypeCoordPlot_I
    use ModUtilities, ONLY: check_dir
    use ModReadParam, ONLY: read_var
    use ModIO, ONLY: NamePrimitiveVarPlot

    character(len=*), intent(in) :: NameCommand

    integer :: iSat, iFile
    character(len=100):: StringSatellite
    character (len=3) :: NameSatVar

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_satellite_parameters'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(NameCommand)
    case("#SATELLITE")
       ! reset values
       StartTimeTraj_I  = 0.
       EndTimeTraj_I    = 0.
       DtTraj_I         = 0.

       TypeTrajTimeRange_I = 'orig'

       call read_var('nSatellite', nSatellite)
       if(nSatellite <= 0) RETURN
       if(iProc==0) call check_dir(NamePlotDir)
       nFile = max(nFile, Satellite_ + nSatellite)
       if (nFile > MaxFile .or. nSatellite > MaxSatellite)&
            call stop_mpi(&
            'The number of output files is too large in #SATELLITE:'&
            //' nFile > MaxFile .or. nSatellite > MaxSatellite')

       do iSat = 1, nSatellite
          iFile = Satellite_ + iSat
          call read_var('StringSatellite', StringSatellite)
          ! Satellite output frequency
          ! Note that we broke with tradition here so that the
          ! DtOutput_I will always we read!  This may be changed
          ! in later distributions
          call read_var('DnOutput', DnOutput_I(iFile))
          call read_var('DtOutput', DtOutput_I(iFile))

          ! Satellite inputfile name or the satellite name
          call read_var('NameTrajectoryFile',&
               NameFileSat_I(iSat))
          if(index(StringSatellite,'eqn')>0 &
               .or. index(StringSatellite,'Eqn')>0 .or. &
               index(StringSatellite,'EQN')>0 ) then
             UseSatFile_I(iSat) = .false.
          else
             UseSatFile_I(iSat) = .true.
          end if

          ! time range for the satellite
          if(index(StringSatellite,'traj') >0 ) then
             if (index(StringSatellite,'range') >0) then
                TypeTrajTimeRange_I(iSat) = 'range'

                call read_var('StartTimeTraj', StartTimeTraj_I(iSat), &
                     StartTimeIn=StartTime)
                call read_var('EndTimeTraj',   EndTimeTraj_I(iSat), &
                     StartTimeIn=StartTime)
                call read_var('DtTraj',        DtTraj_I(iSat))

                ! EndTimeTraj should not be smaller then StartTimeTraj
                if (EndTimeTraj_I(iSat) < StartTimeTraj_I(iSat) .or. &
                     (EndTimeTraj_I(iSat)-StartTimeTraj_I(iSat)) &
                     /DtTraj_I(iSat) > 1e6 .or. DtTraj_I(iSat) <= 0) then
                   write(*,*) ' StartTimeTraj_I =', StartTimeTraj_I(iSat)
                   write(*,*) ' EndTimeTraj_I   =', EndTimeTraj_I(iSat)

                   call stop_mpi(NameSub//' correct #SATELLITE: '//         &
                        'EndTimeTraj < StartTimeTraj or too small dtTraj '//&
                        'or dtTraj <= 0.')
                endif
             else
                TypeTrajTimeRange_I(iSat) = 'full'
                StartTimeTraj_I(iSat) = -1e30
                EndTimeTraj_I(iSat)   = -1e30
                DtTraj_I(iSat)        = -1e30
             end if
          end if

          ! Satellite variables
          if(index(StringSatellite,'VAR')>0 .or. &
               index(StringSatellite,'var')>0 )then
             NameSatVar='var'
             IsDimensionalPlot_I(iFile) = index(StringSatellite,'VAR')>0
             TypeTimeSat_I(iSat) = 'step date'
             call read_var('NameSatelliteVars',StringSatVar_I(iSat))
          elseif(index(StringSatellite,'MHD')>0 .or. &
               index(StringSatellite,'mhd')>0)then
             NameSatVar='mhd'
             IsDimensionalPlot_I(iFile)= index(StringSatellite,'MHD')>0
             TypeTimeSat_I(iSat) = 'step date'
             StringSatVar_I(iSat)=NamePrimitiveVarPlot//' jx jy jz'
          elseif(index(StringSatellite,'FUL')>0 .or. &
               index(StringSatellite,'ful')>0)then
             NameSatVar='ful'
             IsDimensionalPlot_I(ifile)= index(StringSatellite,'FUL')>0
             TypeTimeSat_I(iSat) = 'step date'
             StringSatVar_I(iSat)=&
                  NamePrimitiveVarPlot//' b1x b1y b1z e jx jy jz'
          else
             call stop_mpi(&
                  'Variable definition (mhd,ful,var) missing' &
                  //' from StringSatellite='//StringSatellite)
          end if

          ! Change by DTW, July 2007
          ! Add Trace_DSNB-tracing variables if 'Trace_DSNB' is present.
          if (index(StringSatellite,'Trace_DSNB')>0 .or. &
               index(StringSatellite,'Trace_DSNB')>0) then
             StringSatVar_I(iSat) = trim(StringSatVar_I(iSat)) // &
                  ' theta1 phi1 status theta2 phi2'
          endif

          TypePlot_I(iFile) = "satellite"

          ! Determine the time output format to use in the
          ! satellite files.  This is loaded by default above,
          ! but can be input in the log_string line.
          if(index(StringSatellite,'none')>0) then
             TypeTimeSat_I(iSat) = 'none'
          elseif((index(StringSatellite,'step')>0) .or. &
               (index(StringSatellite,'date')>0) .or. &
               (index(StringSatellite,'time')>0)) then
             TypeTimeSat_I(iSat) = ''
             if(index(StringSatellite,'step')>0) &
                  TypeTimeSat_I(iSat) = 'step'
             if(index(StringSatellite,'date')>0) &
                  TypeTimeSat_I(iSat) = trim(TypeTimeSat_I(iSat))//' date'
             if(index(StringSatellite,'time')>0) &
                  TypeTimeSat_I(iSat) = trim(TypeTimeSat_I(iSat))//' time'
          end if

          ! Recognize coordinate system name if present
          if (index(StringSatellite,'GEO') > 0) TypeCoordPlot_I(iFile) = 'GEO'
          if (index(StringSatellite,'GSE') > 0) TypeCoordPlot_I(iFile) = 'GSE'
          if (index(StringSatellite,'GSM') > 0) TypeCoordPlot_I(iFile) = 'GSM'
          if (index(StringSatellite,'MAG') > 0) TypeCoordPlot_I(iFile) = 'MAG'
          if (index(StringSatellite,'SMG') > 0) TypeCoordPlot_I(iFile) = 'SMG'
          if (index(StringSatellite,'HGR') > 0) TypeCoordPlot_I(iFile) = 'HGR'
          if (index(StringSatellite,'HGI') > 0) TypeCoordPlot_I(iFile) = 'HGI'
          if (index(StringSatellite,'HGC') > 0) TypeCoordPlot_I(iFile) = 'HGC'
       end do

    case('#STEADYSTATESATELLITE')
       do iSat = 1, nSatellite
          call read_var('SatelliteTimeStart', TimeSatStart_I(iSat))
          call read_var('SatelliteTimeEnd',   TimeSatEnd_I(iSat))
       end do
    case('#SATELLITETIMEOFFSET')
       call read_var('UseSatelliteTimeOffset', UseSatelliteTimeOffset)
       if(.not.UseSatelliteTimeOffset)then
          TimeSatOffset_I = 0.
          RETURN
       end if
       do iSat = 1, nSatellite
          call read_var('SatelliteTimeOffset', TimeSatOffset_I(iSat))
       end do
    case default
       call stop_mpi(NameSub//' unknown command='//NameCommand)
    end select
    call test_stop(NameSub, DoTest)
  end subroutine read_satellite_parameters
  !============================================================================
  subroutine set_satellite_file_status(iSat,TypeStatus)
    use ModIoUnit, ONLY: io_unit_new

    integer, intent(in) :: iSat
    character(LEN=*),intent(in) :: TypeStatus

    character(len=*), parameter:: NameSub = 'set_satellite_file_status'
    !--------------------------------------------------------------------------
    select case(TypeStatus)
    case('open')

       call set_name_file(iSat)

       iUnitSat_I(iSat) = io_unit_new()
       call open_file(iUnitSat_I(iSat), file=NameFile_I(iSat))

       IsOpen_I(iSat) = .true.
    case('append')
       if(.not.IsNameFileSet_I(iSat)) then
          call set_name_file(iSat)
          iUnitSat_I(iSat) = io_unit_new()
          call open_file(iUnitSat_I(iSat), file=NameFile_I(iSat))
          IsOpen_I(iSat) = .true.
       end if

       if(.not.IsOpen_I(iSat))then
          iUnitSat_I(iSat) = io_unit_new()
          call open_file(iUnitSat_I(iSat), FILE=trim(NameFile_I(iSat)), &
               STATUS='old', POSITION='append')
          IsOpen_I(iSat) = .true.
       end if
    case('close')
       if (IsOpen_I(iSat)) call close_file(iUnitSat_I(iSat))
       IsOpen_I(iSat) = .false.
    case default
       call stop_mpi(NameSub//': unknown TypeStatus='//TypeStatus)
    end select
  end subroutine set_satellite_file_status
  !============================================================================
  subroutine set_name_sat(iSat)

    integer, intent(in) :: iSat
    integer :: l1,l2
    !--------------------------------------------------------------------------
    l1 = index(NameFileSat_I(iSat), '/', back=.true.) + 1
    l2 = index(NameFileSat_I(iSat), '.') - 1
    if (l1-1 <= 0) l1 = 1
    if (l2+1 <= 0) l2 = len_trim(NameFileSat_I(iSat))
    NameSat_I(iSat) = NameFileSat_I(iSat)(l1:l2)
  end subroutine set_name_sat
  !============================================================================
  subroutine set_name_file(iSat)
    use ModMain, ONLY: nStep, IsTimeAccurate
    use ModIO, ONLY: NamePlotDir, StringDateOrTime
    use ModUtilities, ONLY: lower_case

    integer, intent(in) :: iSat

    character(LEN=50) :: NameFileOutSat
    integer :: l1, l2

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_name_file'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    call set_name_sat(iSat)

    select case(TypeTrajTimeRange_I(iSat))
    case('orig')
       NameFileOutSat = 'sat_'
    case('range', 'full')
       NameFileOutSat = 'trj_'
    case default
       call stop_mpi(NameSub//': unknown TypeTraj= '// &
            TypeTrajTimeRange_I(iSat))
    end select

    if (IsTimeAccurate .and. ( TypeTrajTimeRange_I(iSat) == 'range' .or. &
         TypeTrajTimeRange_I(iSat) == 'full') ) then
       call get_time_string
       write(NameFile_I(iSat),'(a,i8.8,a)')trim(NamePlotDir) // &
            trim(NameFileOutSat)//trim(NameSat_I(iSat))// &
            '_t'//trim(StringDateOrTime)//'_n',nStep,'.sat'

    else
       write(NameFile_I(iSat),'(a,i8.8,a)')trim(NamePlotDir)//&
            trim(NameFileOutSat)//trim(NameSat_I(iSat))//&
            '_n',nStep,'.sat'
    endif

    IsNameFileSet_I(iSat) = .true.
    call lower_case(NameSat_I(iSat))
    if(DoTest) then
       write(*,*) NameSub,': satellitename:', &
            NameFileSat_I(iSat)
       write(*,*) 'iSat,l1,l2: ', iSat, l1, l2
       write(*,*) 'NameSat =', NameSat_I(iSat)
       write(*,*) 'iSat for this name:', i_sat_for_name(NameSat_I(iSat))
       write(*,*) NameSub,': NameFile_I(iSat):', trim(NameFile_I(iSat))
    end if

    call test_stop(NameSub, DoTest)
  end subroutine set_name_file
  !============================================================================
  integer function i_sat_for_name(NameSat)

    use ModUtilities, ONLY: lower_case
    ! Name of satellite for which to find iSat
    character(len=*), intent(in) :: NameSat
    ! Lower case version of input name
    character(len=10) :: NameSatLc
    ! Loop variable
    integer :: iSat
    !--------------------------------------------------------------------------
    NameSatLc = NameSat
    call lower_case(NameSatLc)
    i_sat_for_name = -1
    do iSat = 1, nSatellite
       if(trim(NameSatLc) == trim(NameSat_I(iSat)))then
          i_sat_for_name = iSat
          RETURN
       end if
    end do

  end function i_sat_for_name
  !============================================================================
  function xyz_sat(NameSat)

    use ModMain, ONLY: MaxDim
    ! Name of satellite for which to find Xyz
    character(len=*), intent(in) :: NameSat
    real :: xyz_sat(MaxDim)
    integer :: iSat
    !--------------------------------------------------------------------------

    iSat = i_sat_for_name(NameSat)
    if(iSat<=0)call stop_mpi('Trajectory is not set for satellite='//&
         trim(NameSat))
    call set_satellite_positions(iSat)
    xyz_sat = XyzSat_DI(:,iSat)
  end function xyz_sat
  !============================================================================
  subroutine read_satellite_input_files

    use ModMain, ONLY: MaxDim, TypeCoordSystem, StartTime
    use CON_axes, ONLY: transform_matrix
    use ModTimeConvert, ONLY: time_int_to_real
    use ModIoUnit, ONLY: UnitTmp_
    use ModIo, ONLY: iUnitOut, write_prefix
    use ModKind, ONLY: Real8_
    use ModMpi

    integer :: iError, i, iSat , nPoint

    ! One line of input
    character(len=100) :: StringLine

    integer      :: iTime_I(7)
    real         :: Xyz_D(MaxDim)
    real(Real8_) :: DateTime
    integer      :: MaxPoint
    real, allocatable:: Time_I(:), Xyz_DI(:,:)
    character(len=100):: NameFile

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_satellite_input_files'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Count maximum number of points by reading all satellite files
    MaxPoint = 0
    if(iProc == 0)then
       SATELLITES1: do iSat=1, nSatellite
          if(.not.UseSatFile_I(iSat)) CYCLE SATELLITES1
          NameFile = NameFileSat_I(iSat)
          call open_file(file=NameFile, status="old")
          nPoint = 0

          TypeSatCoord_I(iSat) = TypeCoordSystem
          READFILE1: do
             read(UnitTmp_,'(a)', iostat = iError ) StringLine
             if (iError /= 0) EXIT READFILE1
             if(index(StringLine,'#START')>0)then
                READPOINTS1: do
                   read(UnitTmp_,*, iostat=iError) iTime_I, Xyz_D
                   if (iError /= 0) EXIT READFILE1
                   ! Add new point
                   nPoint = nPoint + 1
                end do READPOINTS1
             end if
          end do READFILE1
          call close_file
          MaxPoint = max(MaxPoint, nPoint)
       end do SATELLITES1
    end if

    ! Tell all processors the maximum number of points
    call MPI_Bcast(MaxPoint, 1, MPI_INTEGER, 0, iComm, iError)

    ! allocate arrays depending on number of points
    allocate(Time_I(MaxPoint), Xyz_DI(MaxDim, MaxPoint))
    allocate(XyzSat_DII(3, nSatellite, MaxPoint))
    allocate(TimeSat_II(nSatellite, MaxPoint))

    ! Read the trajectories
    SATELLITES: do iSat=1, nSatellite

       if(.not.UseSatFile_I(iSat)) CYCLE SATELLITES

       ! Read file on the root processor
       if (iProc == 0) then

          NameFile = NameFileSat_I(iSat)

          if(lVerbose>0)then
             call write_prefix; write(iUnitOut,*) NameSub, &
                  " reading: ",trim(NameFile)
          end if

          call open_file(file=NameFile, status="old")
          nPoint = 0

          ! Read the file: read #COOR TypeCoord, #START and points
          ! Default coordinate system is the one used by BATSRUS (or GSM?)
          TypeSatCoord_I(iSat) = TypeCoordSystem
          READFILE: do

             read(UnitTmp_,'(a)', iostat = iError ) StringLine

             if (iError /= 0) EXIT READFILE

             if(index(StringLine,'#COOR')>0) &
                  read(UnitTmp_,'(a)') TypeSatCoord_I(iSat)

             if(index(StringLine,'#START')>0)then

                READPOINTS: do

                   read(UnitTmp_,*, iostat=iError) iTime_I, Xyz_D

                   if (iError /= 0) EXIT READFILE

                   ! Add new point
                   nPoint = nPoint + 1

                   ! Store coordinates
                   Xyz_DI(:,nPoint) = Xyz_D

                   ! Convert integer date/time to simulation time
                   call time_int_to_real(iTime_I, DateTime)
                   Time_I(nPoint) = DateTime - StartTime

                enddo READPOINTS

             endif

          enddo READFILE

          call close_file

          if(DoTest)write(*,*) NameSub,': nPoint=',nPoint

          ! Convert the coordinates if necessary
          if(TypeSatCoord_I(iSat) /= TypeCoordSystem)then
             do i = 1, nPoint
                Xyz_DI(:,i) = matmul( &
                     transform_matrix( Time_I(i), &
                     TypeSatCoord_I(iSat), TypeCoordSystem), Xyz_DI(:,i) )
             end do
          end if

       end if

       ! Tell the number of points to the other processors
       call MPI_Bcast(nPoint, 1, MPI_INTEGER, 0, iComm, iError)
       nPointTraj_I(iSat) = nPoint

       ! Tell the other processors the satellite time
       call MPI_Bcast(Time_I, nPoint, MPI_REAL, 0, iComm, iError)

       ! Tell the other processors the coordinates
       call MPI_Bcast(Xyz_DI, MaxDim*nPoint, MPI_REAL, 0, iComm, iError)

       ! Store time and positions for satellite iSat on all PE-s

       TimeSat_II(iSat, 1:nPoint) = Time_I(1:nPoint)
       do i = 1, nPoint
          XyzSat_DII(:, iSat, i) = Xyz_DI(:, i)
       end do

       if(DoTest)then
          nPoint = min(10,nPoint)
          write(*,*) NameSub,': tSat=', TimeSat_II(iSat,1:nPoint)
          write(*,*) NameSub,': xSat=', XyzSat_DII(1,iSat,1:nPoint)
          write(*,*) NameSub,': ySat=', XyzSat_DII(2,iSat,1:nPoint)
          write(*,*) NameSub,': zSat=', XyzSat_DII(3,iSat,1:nPoint)
       end if
       call set_name_sat(iSat)
    end do SATELLITES

    deallocate(Time_I, Xyz_DI)

    call test_stop(NameSub, DoTest)
  end subroutine read_satellite_input_files
  !============================================================================
  subroutine set_satellite_flags(iSat)

    use BATL_lib, ONLY: find_grid_block

    integer, intent(in) :: iSat

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_satellite_flags'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if (DoTest) write(*,*) NameSub,' starting for iSat=', iSat

    call set_satellite_positions(iSat)

    if(DoTest)write(*,*) NameSub,' DoTrackSatellite=', &
         DoTrackSatellite_I(iSat)
    if(.not.DoTrackSatellite_I(iSat)) RETURN ! Position is not defined

    call find_grid_block(XyzSat_DI(:,iSat), &
         iProcSat_I(iSat), iBlockSat_I(iSat))

    if (iProcSat_I(iSat) < 0) DoTrackSatellite_I(iSat) = .false.

    if (DoTest) write(*,*)'set_satellite_flags iPE,iBlock,TrackSatellite=', &
         iProcSat_I(iSat), iBlockSat_I(iSat), DoTrackSatellite_I(iSat)

    call test_stop(NameSub, DoTest)
  end subroutine set_satellite_flags
  !============================================================================
  subroutine set_satellite_positions(iSat)
    use ModMain, ONLY:tSimulation
    use ModNumConst

    integer, intent(in) :: iSat
    integer :: i, nPoint
    real    :: dTime

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_satellite_positions'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if (UseSatFile_I(iSat)) then

       nPoint = nPointTraj_I(iSat)
       if (nPoint > 0) then

          i = iPointCurrentSat_I(iSat)

          if(DoTest)write(*,*) NameSub,' nPoint, iPoint, TimeSim, TimeSat=',&
               nPoint, i, tSimulation, TimeSat_II(iSat,i)

          do while (i < nPoint .and. TimeSat_II(iSat,i) <= tSimulation)
             i = i + 1
          enddo

          iPointCurrentSat_I(iSat) = i

          if(DoTest)write(*,*) NameSub,' final iPoint=', i

          if ( (i == nPoint .and. tSimulation > TimeSat_II(iSat,i)) .or. &
               i == 1 ) then

             DoTrackSatellite_I(iSat) = .false.
             XyzSat_DI(:,iSat) = 0.0

          else
             DoTrackSatellite_I(iSat) = .true.

             dTime = 1.0 - (TimeSat_II(iSat,i) - tSimulation) / &
                  max((TimeSat_II(iSat,i) - TimeSat_II(iSat,i-1)), cTiny)

             XyzSat_DI(:,iSat) = dTime * XyzSat_DII(:,iSat,i) + &
                  (1.0 - dTime) * XyzSat_DII(:,iSat,i-1)

          end if

          if(DoTest) then
             write(*,*) NameSub,' DoTrackSat =', DoTrackSatellite_I(iSat)
             write(*,*) NameSub,' XyzSat     =', XyzSat_DI(:,iSat)
          end if
       end if
    else
       call satellite_trajectory_formula(iSat)
    end if

    call test_stop(NameSub, DoTest)
  end subroutine set_satellite_positions
  !============================================================================
  subroutine satellite_trajectory_formula(iSat)

    integer, intent(in) :: iSat
    real :: XyzSat_D(3)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'satellite_trajectory_formula'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    XyzSat_D(:) = XyzSat_DI(:,iSat)

    ! Case should be for a specific satellite.  The trajectories can depend
    ! on the 'real' time so that the satellite knows where it is at.  For
    ! example, Cassini could be if'd on the date so that the code knows
    ! whether the run is for a time near Earth, Jupiter or Saturn.

    ! This routine should always set the TrackSatellite(iSat) flag. When
    ! the satellite is at a useless position the time should return a
    ! do not track flag (DoTrackSatellite_I(iSat) = .false.).

    select case(NameFileSat_I(iSat))
    case ('earth')
       XyzSat_D(1) = 5.0
       XyzSat_D(2) = 5.0
       XyzSat_D(3) = 5.0
       DoTrackSatellite_I(iSat) = .true.
    case ('cassini')
       XyzSat_D(1) = 5.0
       XyzSat_D(2) = 5.0
       XyzSat_D(3) = 5.0
       DoTrackSatellite_I(iSat) = .true.
    case default
       XyzSat_D(1) = 1.0
       XyzSat_D(2) = 1.0
       XyzSat_D(3) = 1.0
       DoTrackSatellite_I(iSat) = .false.
    end select

    call test_stop(NameSub, DoTest)
  end subroutine satellite_trajectory_formula
  !============================================================================
  subroutine get_satellite_ray(iSatIn, SatRayVar_I)

    use ModFieldTrace, ONLY: Trace_DSNB
    use ModUpdateStateFast, ONLY: sync_cpu_gpu
    use BATL_size, ONLY:
    use BATL_lib, ONLY: iProc, nI, nJ, nK, IsCartesianGrid, &
         CellSize_DB, CoordMin_DB, xyz_to_coord
    use ModMpi

    integer, intent(in) :: iSatIn
    real,    intent(out):: SatRayVar_I(5)

    integer  :: iDir, iBlock, iDim
    real     :: Coord_D(3), Trace_DSC(3,2,nI,nJ,nK)
    real     :: Dx1, Dx2, Dy1, Dy2, Dz1, Dz2
    integer  :: i1, i2, j1, j2, k1, k2, iNear, jNear, kNear
    integer  :: i, j, k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_satellite_ray'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    call sync_cpu_gpu('update on CPU', NameSub, Trace_DICB=Trace_DSNB)

    ! Initialize to zero
    SatRayVar_I = 0.0

    ! Only use this if we're on the correct node.
    if (iProc /= iProcSat_I(iSatIn)) RETURN

    iBlock = iBlockSat_I(iSatIn)
    if (iBlock == 0) RETURN

    if (IsCartesianGrid) then
       Coord_D = XyzSat_DI(:,iSatIn)
    else
       call xyz_to_coord(XyzSat_DI(:,iSatIn), Coord_D)
    end if

    ! Normalize coordinates to the cell center indexes
    Coord_D = (Coord_D - CoordMin_DB(:,iBlock)) / CellSize_DB(:,iBlock) + 0.5

    ! Set location assuming point is inside block.
    i1 = floor(Coord_D(1))
    j1 = floor(Coord_D(2))
    k1 = floor(Coord_D(3))
    i2 = ceiling(Coord_D(1))
    j2 = ceiling(Coord_D(2))
    k2 = ceiling(Coord_D(3))

    ! If Coord_D is outside of block, change i,j,k in order to extrapolate.
    if(any( Coord_D < 1) .or. any(Coord_D > [nI, nJ, nK])) then
       i1 = min(nI-1, max(1, i1));   i2 = i1 + 1
       j1 = min(nJ-1, max(1, j1));   j2 = j1 + 1
       k1 = min(nK-1, max(1, k1));   k2 = k1 + 1
    endif

    ! Set interpolation weights
    Dx1 = Coord_D(1) - i1; Dx2 = 1.0 - Dx1
    Dy1 = Coord_D(2) - j1; Dy2 = 1.0 - Dy1
    Dz1 = Coord_D(3) - k1; Dz2 = 1.0 - Dz1

    ! Calculate the nearest point.
    iNear = min( nI, max(nint(Coord_D(1)),1) )
    jNear = min( nJ, max(nint(Coord_D(2)),1) )
    kNear = min( nK, max(nint(Coord_D(3)),1) )

    ! Copy Trace_DSNB tracing values to new array so allow changing of values.
    Trace_DSC = Trace_DSNB(1:3,1:2,1:nI,1:nJ,1:nK,iBlock)

    ! Use the Trace_DSNB status of the nearest point to the satellite.
    SatRayVar_I(3) = Trace_DSC(3, 1, iNear,jNear,kNear)

    ! For each direction along the Trace_DSNB, determine if all lines
    ! surrounding point are open or closed, then set SatRayVar_I accordingly.
    do iDir = 1, 2

       if ( any(Trace_DSC(3,1,i1:i2,j1:j2,k1:k2) < 1) .or. &
            any(Trace_DSC(3,1,i1:i2,j1:j2,k1:k2) == iDir) ) then
          ! One or more lines is open in direction iDir, must use nearest point
          do iDim=1,2
             SatRayVar_I(iDim + 3*(iDir-1)) = &
                  Trace_DSC(iDim,iDir,iNear,jNear,kNear)
          end do

       else   ! All lines closed in direction iDir, interpolate.

          ! If the satellite is near the 0/360 degree boundary in longitude,
          ! the result of the interpolation will be incorrect.  Adjust
          ! longitudes accordingly.
          if (any(Trace_DSC(2,iDir,i1:i2,j1:j2,k1:k2)>330.0) .AND. &
               any(Trace_DSC(2,iDir,i1:i2,j1:j2,k1:k2)<30.0)) then

             do i=i1,i2
                do j=j1,j2
                   do k=k1,k2
                      if (Trace_DSC(2,iDir,i,j,k) < 30.0) &
                           Trace_DSC(2,iDir,i,j,k) &
                           = Trace_DSC(2,iDir,i,j,k) + 360
                   enddo
                enddo
             enddo
             ! forall(i=i1:i2,j=j1:j2,k=k1:k2,Trace_DSC(2,iDir,i,j,k)<30.0)
             !   Trace_DSC(2,iDir,i,j,k) = Trace_DSC(2,iDir,i,j,k) + 360.0
             ! end forall
          endif

          do iDim=1,2
             SatRayVar_I(iDim + 3*(iDir-1)) =                         &
                  Dz2*(   Dy2*(   Dx2*Trace_DSC(iDim,iDir,i1,j1,k1)   &
                  +                Dx1*Trace_DSC(iDim,iDir,i2,j1,k1))  &
                  +        Dy1*(   Dx2*Trace_DSC(iDim,iDir,i1,j2,k1)   &
                  +                Dx1*Trace_DSC(iDim,iDir,i2,j2,k1))) &
                  +Dz1*(   Dy2*(   Dx2*Trace_DSC(iDim,iDir,i1,j1,k2)   &
                  +                Dx1*Trace_DSC(iDim,iDir,i2,j1,k2))  &
                  +        Dy1*(   Dx2*Trace_DSC(iDim,iDir,i1,j2,k2)   &
                  +                Dx1*Trace_DSC(iDim,iDir,i2,j2,k2)))
          end do
       endif

    end do

    ! Ensure that longitude wraps around 0/360 degree boundary correctly.
    if(SatRayVar_I(2) <   0.0) SatRayVar_I(2) = SatRayVar_I(2) + 360.0
    if(SatRayVar_I(5) <   0.0) SatRayVar_I(5) = SatRayVar_I(5) + 360.0
    if(SatRayVar_I(2) > 360.0) SatRayVar_I(2) = SatRayVar_I(2) - 360.0
    if(SatRayVar_I(5) > 360.0) SatRayVar_I(5) = SatRayVar_I(5) - 360.0

    call test_stop(NameSub, DoTest)
  end subroutine get_satellite_ray
  !============================================================================
  subroutine GM_trace_sat(SatXyz_D, SatRay_D)

    use ModFieldTrace, ONLY: DoExtractState, DoExtractUnitSi, &
         extract_field_lines, rIonosphere
    use ModVarIndexes, ONLY: nVar
    use ModMain, ONLY: tSimulation, TypeCoordSystem
    use CON_line_extract, ONLY: line_init, line_collect, line_get, line_clean
    use ModNumConst, ONLY: cRadToDeg
    use CON_axes, ONLY: transform_matrix
    use CON_planet_field, ONLY: map_planet_field
    use ModPhysics, ONLY: rBody

    ! Trace position SatXyz_D and return result in SatRay_D on Proc 0
    ! All other processors return 0-s

    real, intent(in) :: SatXyz_D(3) ! Satellite Position
    real, intent(out):: SatRay_D(3)
    real :: SatXyzIono_D(3), SatXyzEnd_D(3), SatXyzEnd2_D(3),B2

    integer            :: nStateVar
    integer            :: nPoint

    real, pointer :: PlotVar_VI(:,:)

    integer :: nLine, nVarOut, iHemisphere

    logical :: IsParallel = .true., IsOpen=.true.

    real    :: Rsat, Rxy2
    ! Conversion matrix between SM and GM coordinates
    ! (to be safe initialized to unit matrix)
    real :: GmSm_DD(3,3)
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'GM_trace_sat'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    DoExtractState  = .true.
    DoExtractUnitSi = .false.

    ! Set the number lines and variables to be extracted
    nLine     = 1
    nStateVar = 4
    if(DoExtractState) nStateVar = nStateVar + nVar

    if (sum(SatXyz_D(1:3)**2) > rBody**2) then
       ! Initialize CON_line_extract
       call line_init(nStateVar)

       ! Obtain the line data
       call extract_field_lines(nLine, [IsParallel], SatXyz_D)

       ! Collect lines from all PE-s to Proc 0
       call line_collect(iComm,0)

       if(iProc==0)then
          call line_get(nVarOut, nPoint)
          if(nVarOut /= nStateVar)call stop_mpi(NameSub//': nVarOut error')
          allocate(PlotVar_VI(0:nVarOut, nPoint))
          call line_get(nVarOut, nPoint, PlotVar_VI, DoSort=.true.)
       end if

       call line_clean

       ! Only iProc 0 stores result
       if(iProc == 0) then
          SatXyzEnd_D = PlotVar_VI(2:4,nPoint)
          deallocate(PlotVar_VI)
       endif

       ! Now Trace in opposite direction to make sure line is closed
       ! Initialize CON_line_extract
       call line_init(nStateVar)

       ! Obtain the line data
       call extract_field_lines(nLine, [.not.IsParallel], SatXyz_D)

       ! Collect lines from all PE-s to Proc 0
       call line_collect(iComm,0)

       if(iProc==0)then
          call line_get(nVarOut, nPoint)
          if(nVarOut /= nStateVar)call stop_mpi(NameSub//': nVarOut error')
          allocate(PlotVar_VI(0:nVarOut, nPoint))
          call line_get(nVarOut, nPoint, PlotVar_VI, DoSort=.true.)
       end if

       call line_clean
       DoExtractState  = .false.

       ! Only iProc 0 returns actual result. The rest sends back 0.
       SatRay_D = 0.0
       if(iProc /= 0) RETURN

       SatXyzEnd2_D = PlotVar_VI(2:4,nPoint)

       B2 = sum(PlotVar_VI(5:7,1)**2)

       deallocate(PlotVar_VI)

       ! Only iProc 0 works for returning line info
       !  if(iProc /= 0) RETURN

       ! Check that line is closed
       IsOpen =  sum(SatXyzEnd_D**2)  > 8*rIonosphere**2 &
            .or. sum(SatXyzEnd2_D**2) > 8*rIonosphere**2

       if (.not. IsOpen) then
          call map_planet_field(tSimulation, SatXyzEnd_D, &
               TypeCoordSystem//' NORM', rIonosphere, SatXyzIono_D, &
               iHemisphere)

          ! Transformation matrix between the SM(G) and GM coordinates
          GmSm_DD = transform_matrix(tSimulation, 'SMG', TypeCoordSystem)
          ! Convert GM position into RB position
          SatXyzIono_D = matmul(SatXyzIono_D, GmSm_DD)

          ! Convert XYZ to Lat-Lon-IsOpen
          ! Calculate  -90 < latitude = asin(z)  <  90
          SatRay_D(1) = cRadToDeg * asin(SatXyzIono_D(3)/rIonosphere)
          ! Calculate 0 < longitude = atan2(y,x) < 360
          SatRay_D(2) =  &
               modulo(cRadToDeg*atan2(SatXyzIono_D(2), SatXyzIono_D(1)), 360.0)

          ! set closed flag
          SatRay_D(3) = 3.0
       else
          SatRay_D(1) = -100.0
          SatRay_D(2) = -200.0
          SatRay_D(3) = 0.0
       endif
    else
       ! When planet is inside rBody use dipole assumption
       Rsat = norm2(SatXyz_D)
       Rxy2 = sum(SatXyz_D(1:2)**2)
       if(Rxy2 > 0.0)then
          SatRay_D(1) = acos(1/sqrt(Rsat**3/Rxy2))*cRadToDeg
       else
          SatRay_D(1) = 90.0
       endif

       SatRay_D(2) =  &
            modulo(cRadToDeg*atan2(SatXyz_D(2), SatXyz_D(1)), 360.0)
       ! set closed flag
       SatRay_D(3) = 3.0
    endif
    call test_stop(NameSub, DoTest)

  end subroutine GM_trace_sat
  !============================================================================
end module ModSatelliteFile
!==============================================================================
