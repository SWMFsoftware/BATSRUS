! 2020.08.10  Keebler
! From an input location timeseries, output interpolated values
! along the trajectory.

program interpolate_output
  
  ! Read a *.outs IDL file of type ascii/real4/real8 and a satellite file,
  ! then interpolate the *.outs to the trajectory from the satellite file.

  use ModPlotFile,       ONLY: read_plot_file
  use ModTimeConvert,    ONLY: time_int_to_real
  use ModConst,          ONLY: cRadToDeg
  use ModInterpolate,    ONLY: interpolate_vector
  use ModCoordTransform, ONLY: atan2_check
  use ModUtilities,      ONLY: open_file, close_file
  use ModIoUnit,         ONLY: UnitTmp_
  
  implicit none

  integer:: iError      ! I/O error
  integer:: iTime_I(7)  ! array for year, month ... msec
  real   :: Time        ! time in number of seconds since 00UT 1965/1/1

  ! Point position file
  character(len=100):: NameFilePoints ! name of position file
  character(len=100):: StringLine     ! single line from trajectory file
  character(len=3)  :: NameCoordPoint ! coordinate system of the points
  real, allocatable :: TrajTime_I(:)  ! simulation time from trajectory file
  character(len=3), allocatable:: NameMag_I(:) ! name of magnetic stations
  real, allocatable :: Coord_DI(:,:)  ! positions at each timestamp
  integer           :: nPoint         ! number of points for interpolation
  real              :: StartTime      ! simulation start time
  
  ! Input mulit-D file
  character(len=100):: NameFileIn     ! name of input data file
  character(len=10) :: TypeFileIn     ! ascii/real4/real8
  character(len=3)  :: NameCoordIn    ! coordinate system of input data file
  character(len=500):: StringHeader
  character(len=500):: NameVar
  integer           :: nStep, nDim, nParam, nVar
  logical           :: IsCartesian
  real              :: Param_I(100) ! parameters
  real, allocatable :: Coord_DII(:,:,:), Var_VII(:,:,:)
  integer :: n_D(0:3), n1, n2, n3

  real, allocatable :: InterpData_VI(:,:) ! fully interpolated output
  real :: InterpCoord_D(2) ! interpolated trajectory coordinates
  integer :: MaxSnapshot ! limit of snapshots in .outs file
  integer :: nSnapshots ! number of snapshots contained in .outs file
  real :: RadMin, RadMax, PhiMin, PhiMax
  integer :: Weight
  real    :: dPhi
  
  ! Interpolated file
  character(len=100):: NameFileOut      ! name of output file or directory
  real, allocatable :: TimeOut_I(:)
  real, allocatable :: InterpCoord_DI(:,:)

  logical:: IsMagStation = .false., IsTrajectory = .true.

  character(len=*), parameter:: NameProgram = 'interpolate_output'
  !-------------------------------------------------------------------------

  call read_parameters
  write(*,*) NameProgram,': finished read parameters'
  call read_positions
  write(*,*) NameProgram,': finished read position'
  
  if(IsTrajectory)then
     write(*,*) NameProgram,': start interpolate_trajectory'
     call interpolate_trajectory
     write(*,*) NameProgram,': start write_trajectory'
     call write_trajectory
  elseif(IsMagStation)then
     write(*,*) NameProgram,': start interpolate_mag_station'
     call interpolate_mag_station
     write(*,*) NameProgram,': start write_mag_station'
     call write_mag_station
  end if

contains
  !===========================================================================
  subroutine read_parameters

    ! Read  file names and types from STDIN (can be piped in from a file):
    !
    ! z=0_mhd_1_e19970517-033600-000.outs
    ! real4
    ! MARS.dat
    ! my_interpolated_output.dat
    !
    ! Input data file MUST contain the simulation start time in the filename.
    ! for accurate conversion between satellite time and simulation time.

    integer:: i
    !-------------------------------------------------------------------------
    
    write(*,'(a)', ADVANCE='NO') 'Name of multi-dimensional data file:    '
    read(*,'(a)',iostat=iError) NameFileIn

    ! Figure out if we extract magnetometer stations or trajectories
    IsMagStation = index(NameFileIn, 'mag_grid') > 0
    IsTrajectory = .not. IsMagStation

    write(*,'(a,a,l1)') trim(NameFileIn), ', IsMagStation=', IsMagStation
    write(*,'(a)', ADVANCE='NO') 'Type of input file (ascii/real4/real8): '
    read(*,'(a)',iostat=iError) TypeFileIn
    write(*,'(a)') TypeFileIn
    write(*,'(a)', ADVANCE='NO') 'Name of file containing positions:      '
    read(*,'(a)',iostat=iError) NameFilePoints
    write(*,'(a)') NameFilePoints
    write(*,'(a)', ADVANCE='NO') 'Name of output file/directory:          '
    read(*,'(a)',iostat=iError) NameFileOut
    write(*,'(a)') NameFileOut

    ! Get simulation start date-time from filename *_e(....)
    i = index(NameFileIn, '_e') + 2
    iTime_I = 0
    read(NameFileIn(i:i+14),'(i4,2i2,1x,3i2)', iostat=iError) iTime_I(1:6)
    if(iError /= 0)then
       write(*,*) NameProgram,': could not read date from file name ', &
            trim(NameFileIn)
       stop
    end if
    call time_int_to_real(iTime_I, StartTime)
    write(*,*) NameProgram,': StartTime=', iTime_I

    ! Set default coordinate systems
    if(IsMagStation)then
       NameCoordIn    = 'GEO'
       NameCoordPoint = 'GEO'
    else
       NameCoordIn    = 'HGI'
       NameCoordPoint = 'HGI'
    end if

  end subroutine read_parameters
  !============================================================================
  subroutine read_positions

    ! File should be in the same format as satellite/magnetometer station files
    ! Coordinate system is either read or assumed to be the same as the
    ! data file. Data lines follow the #START command. Examples:
    !
    ! Satellite file:
    !
    ! year mo dy hr mn sc msc x y z
    ! #START
    !  1997 5 17 3 35 00 000 -0.368 5.275 0.0
    !  ...
    !
    ! Magnetometer station file:
    ! #COORD
    ! MAG
    ! 
    ! #START
    ! YKC 68.93 299.36
    ! ...

    integer:: iPoint         ! line number in position file
    !--------------------------------------------------------------------------
    ! Open the position file
    call open_file(File=NameFilePoints, Status="old", NameCaller=NameProgram)

    ! Find the number of points in the point position file
    nPoint = 0
    COUNTPOINTS: do
       read(UnitTmp_,'(a)',iostat=iError) StringLine
       if(iError /= 0) stop NameProgram//': no #START in position file'
       if(StringLine(1:5) == '#COOR') &
            read(UnitTmp_,'(a3)') NameCoordPoint
       if(index(StringLine, '#START')>0)then
          do
             read(UnitTmp_, '(a)', iostat=iError) StringLine
             if(iError/=0) EXIT COUNTPOINTS
             nPoint = nPoint + 1
          enddo
       endif
    enddo COUNTPOINTS

    write(*,*) NameProgram, ': nPoint=', nPoint,', CoordPoint=', NameCoordPoint

    ! Allocate arrays to hold times/positions
    if(IsTrajectory) allocate(TrajTime_I(nPoint))
    if(IsMagStation) allocate(NameMag_I(nPoint))
    allocate(Coord_DI(2,nPoint))

    ! Rewind to start of file for reading times/positions.
    rewind(unit=UnitTmp_) 

    ! Skip header
    do
       read(UnitTmp_,'(a)') StringLine
       if(index(StringLine, '#START') > 0) EXIT
    end do

    ! Read point information
    do iPoint = 1, nPoint
       read(UnitTmp_,'(a)',iostat=iError) StringLine
       if(IsMagStation)then
          read(StringLine, *, iostat=iError) &
               NameMag_I(iPoint), Coord_DI(:,iPoint)
       else
          read(StringLine, *, iostat=iError) iTime_I, Coord_DI(:,iPoint)
       end if
       if(iError /= 0)then
          write(*,*) NameProgram,': error reading line ',iPoint,':'
          write(*,*) trim(StringLine)
          stop
       end if

       if(IsTrajectory)then
          ! Convert integer time to simulation time.
          call time_int_to_real(iTime_I, Time)
          TrajTime_I(iPoint) = Time - StartTime
       end if
    enddo

    ! Close the trajectory file.
    call close_file(NameCaller=NameProgram)

  end subroutine read_positions
  !============================================================================
  subroutine interpolate_trajectory

    integer:: i, iTrajTimestamp ! loop indices
    !-------------------------------------------------------------------------
    MaxSnapshot = 100000
    nSnapshots = 0
    iTrajTimestamp = 1
    do i = 1, MaxSnapshot
       ! Read header info from .outs file.
       call read_plot_file(NameFile = NameFileIn, iUnitIn = UnitTmp_, &
            TypeFileIn = TypeFileIn, &
            StringHeaderOut = StringHeader, &
            NameVarOut = NameVar, &
            nStepOut = nStep, &
            TimeOut = Time, &       ! simulation time
            nDimOut = nDim, &       ! number of dimensions
            IsCartesianOut = IsCartesian, &
            nParamOut = nParam, &   ! number of parameters
            nVarOut = nVar, &       ! number of variables
            n1Out = n1, &           ! grid sizes
            n2Out = n2, &
            n3Out = n3, &
            nOut_D = n_D, &         ! nOut_D grid size array
            ParamOut_I = Param_I)   ! parameters

       if(IsTrajectory)then
          if(Time < TrajTime_I(1)) CYCLE  ! before start of trajectory file
          if(Time > TrajTime_I(nPoint)) EXIT  ! after end of trajectory file
       end if

       ! Create array to hold interpolated data.
       if(.not.allocated(InterpData_VI))then
          allocate(TimeOut_I(MaxSnapshot), InterpData_VI(nVar,MaxSnapshot), &
               InterpCoord_DI(nDim,MaxSnapshot))
       endif

       ! Determine the shape of arrays from the header. Include ghost cells.
       !   Note: assumes no AMR/constant grid
       if(.not. allocated(Coord_DII)) &
            allocate(Coord_DII(nDim, n1, 0:n2+1), &
            Var_VII(nVar, n1, 0:n2+1))

       ! Read the data at the snapshot.
       call read_plot_file(NameFile=NameFileIn, iUnitIn=UnitTmp_, &
            TypeFileIn = TypeFileIn, &
            CoordOut_DII = Coord_DII(:,:,1:n2), &
            VarOut_VII = Var_VII(:,:,1:n2))

       ! Interpolate location of trajectory file in time to snapshot.
       do while(Time > TrajTime_I(iTrajTimestamp))
          iTrajTimestamp = iTrajTimestamp + 1
       enddo
       if(Time == TrajTime_I(iTrajTimestamp))then
          InterpCoord_D = Coord_DI(:,iTrajTimestamp)
       else
          Weight = (TrajTime_I(iTrajTimestamp) - Time)/ &
               (TrajTime_I(iTrajTimestamp)-TrajTime_I(iTrajTimestamp-1))
          InterpCoord_D = Coord_DI(:,iTrajTimestamp-1)*Weight + &
               Coord_DI(:,iTrajTimestamp)*(1-Weight)
       endif

       ! Convert coordinates to curvilinear.
       if(.not.IsCartesian)then
          ! Coordinates converted from [x,y] to [ln(r),phi]
          InterpCoord_D = [log(sqrt(sum(InterpCoord_D(1:2)**2))), &
               atan2_check(InterpCoord_D(2), InterpCoord_D(1))*cRadToDeg]

          RadMin = log(sqrt(sum(Coord_DII(1:2,1,1)**2)))
          RadMax = log(sqrt(sum(Coord_DII(1:2,n1,n2)**2)))
          PhiMin = atan2_check(Coord_DII(2,1,1),Coord_DII(1,1,1))*cRadToDeg
          PhiMax = atan2_check(Coord_DII(2,n1,n2),Coord_DII(1,n1,n2))*cRadToDeg
          dPhi   = (PhiMax - PhiMin)/(n2-1)
          PhiMin = PhiMin - dPhi ! account for ghost cells
          PhiMax = PhiMax + dPhi
       endif

       ! Fill in ghost cells in phi.
       Var_VII(:,:,0) = Var_VII(:,:,n2)
       Var_VII(:,:,n2+1) = Var_VII(:,:,1)

       ! Normalize coordinates.
       InterpCoord_D(1) = (InterpCoord_D(1)-RadMin)*(n1-1)/(RadMax-RadMin) + 1
       InterpCoord_D(2) = (InterpCoord_D(2)-PhiMin)*(n2+1)/(PhiMax-PhiMin)

       ! Interpolate snapshot to trajectory location.
       InterpData_VI(:,i) = interpolate_vector( &
            a_VC = Var_VII, & ! variable array
            nVar = nVar, &    ! number of variables (11)
            nDim = 2, &       ! number of dimensions
            Min_D = [1,0], &
            Max_D = [n1,n2+1], &
            x_D = InterpCoord_D, & ! desired position
            DoExtrapolate = .false.)

       InterpCoord_DI(:,i) = InterpCoord_D
       TimeOut_I(i) = Time

       nSnapshots = i ! Update number of snapshots in .outs file
    enddo

    call close_file(NameCaller=NameProgram)
    deallocate(Coord_DII, Var_VII)

  end subroutine interpolate_trajectory
  !============================================================================
  subroutine write_trajectory

    integer:: iPoint
    !-------------------------------------------------------------------------
    
    ! Open file for interpolated output
    call open_file(file=NameFileOut, NameCaller=NameProgram) 

    ! Include header
    write(UnitTmp_,*)'t ',NameVar

    ! Write data. For now, time is in simulation time
    do iPoint=1,nSnapshots
       write(UnitTmp_,*)TimeOut_I(iPoint), &
            InterpCoord_DI(:,iPoint), InterpData_VI(:,iPoint)
    enddo
    
    call close_file(NameCaller=NameProgram)
  end subroutine write_trajectory
  !============================================================================
  subroutine interpolate_mag_station

  end subroutine interpolate_mag_station
  !============================================================================
  subroutine write_mag_station

  end subroutine write_mag_station
  !============================================================================

end program





