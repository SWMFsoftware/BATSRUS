! 2020.08.10  Keebler
! From an input location timeseries, output interpolated values
! along the trajectory.

program interpolate_output

  ! Read a *.outs IDL file of type ascii/real4/real8 and a satellite file,
  ! then interpolate the *.outs to the trajectory from the satellite file.

  use ModPlotFile,       ONLY: read_plot_file
  use ModTimeConvert,    ONLY: time_int_to_real, time_real_to_int
  use ModNumConst,       ONLY: cRadToDeg, cDegToRad
  use ModInterpolate,    ONLY: interpolate_vector, bilinear
  use ModCoordTransform, ONLY: atan2_check
  use ModUtilities,      ONLY: open_file, close_file
  use ModIoUnit,         ONLY: UnitTmp_

  implicit none

  integer:: iError      ! I/O error
  integer:: iTime_I(7)  ! array for year, month ... msec
  real   :: Time        ! time in number of seconds since 00UT 1965/1/1
  real   :: StartTime   ! simulation start time in seconds since 00UT 1965/1/1

  ! Point position file
  character(len=100):: NameFilePoints   ! name of position file
  character(len=100):: StringLine       ! single line from trajectory file
  character(len=3)  :: NameCoordPoint   ! coordinate system of the points
  real, allocatable :: TrajTime_I(:)    ! simulation time from trajectory file
  character(len=3), allocatable:: NameMag_I(:) ! name of magnetic stations
  real, allocatable :: CoordPoint_DI(:,:) ! positions
  real, allocatable :: CoordIn_DI(:,:)    ! positions in the input file system
  real, allocatable :: CoordNorm_DI(:,:)  ! normalized positions
  integer           :: nPoint           ! number of points for interpolation

  ! Input mulit-D file
  character(len=100):: NameFileIn       ! name of input data file
  character(len=10) :: TypeFileIn       ! ascii/real4/real8
  character(len=3)  :: NameCoordIn      ! coordinate system of input data file
  character(len=500):: NameVar
  integer           :: nDim, nVar
  logical           :: IsCartesian
  real, allocatable :: Var_VII(:,:,:)
  integer           :: n1, n2, n3       ! grid size
  integer           :: Di=0, Dj=0       ! cells surrounding station
  integer           :: nSnapshot        ! number of snapshots to be read
  real              :: StartTimeFileIn  ! time of initial snapshot in data file

  ! Interpolated file
  character(len=100):: NameFileOut      ! name of output file
  character(len=100):: NameDirOut       ! name of output directory

  logical:: IsMagStation = .false., IsTrajectory = .true.

  character(len=*), parameter:: NameProgram = 'interpolate_output'
  !----------------------------------------------------------------------------
  write(*,*) NameProgram,': start read_parameters'
  call read_parameters
  write(*,*) NameProgram,': start read_positions'
  call read_positions
  write(*,*) NameProgram,': start read_through_first'
  call read_through_first

  if(IsTrajectory)then
     write(*,*) NameProgram,': start interpolate_trajectory'
     call interpolate_trajectory
  elseif(IsMagStation)then
     write(*,*) NameProgram,': start interpolate_mag_station'
     call interpolate_mag_station
  end if
  write(*,*) NameProgram,' finished'

contains
  !============================================================================
  subroutine read_parameters

    use ModUtilities, ONLY: fix_dir_name, make_dir

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
    !--------------------------------------------------------------------------
    write(*,'(a)', ADVANCE='NO') 'Name of multi-dimensional data file:    '
    read(*,'(a)',iostat=iError) NameFileIn

    ! Figure out if we extract magnetometer stations or trajectories
    IsMagStation = index(NameFileIn, 'mag_grid') > 0
    IsTrajectory = .not. IsMagStation

    write(*,'(a,a,l1)') trim(NameFileIn), ', IsMagStation=', IsMagStation

    ! Get simulation start date-time from filename *_e(....)
    i = index(NameFileIn, '_e') + 2
    iTime_I = 0
    read(NameFileIn(i:i+14),'(i4,2i2,1x,3i2)', iostat=iError) iTime_I(1:6)
    if(iError /= 0)then
       write(*,*) NameProgram,': could not read date from file name ', &
            trim(NameFileIn)
       STOP
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

    write(*,'(a)', ADVANCE='NO') 'Type of input file (ascii/real4/real8): '
    read(*,'(a)',iostat=iError) TypeFileIn
    select case(TypeFileIn)
    case('ascii','real4','real8')
       write(*,'(a)') trim(TypeFileIn)
    case default
       write(*,*) NameProgram//': incorrect TypeFileIn='//TypeFileIn
       STOP
    end select
    write(*,'(a)', ADVANCE='NO') 'Name of file containing positions:      '
    read(*,'(a)',iostat=iError) NameFilePoints
    write(*,'(a)') trim(NameFilePoints)
    if(IsMagStation)then
       write(*,'(a)', ADVANCE='NO') 'Name of output directory:               '
       read(*,'(a)',iostat=iError) NameDirOut
       if(NameDirOut == '' .or. NameDirOut == '.')then
          NameDirOut = './'
       else
          call fix_dir_name(NameDirOut)
          call make_dir(NameDirOut)
       end if
       write(*,'(a)') trim(NameDirOut)
       write(*,'(a)', ADVANCE='NO') 'Surrounding cells Di, Dj:               '
       read(*,*,iostat=iError) Di, Dj
       write(*,'(2i4)') Di, Dj
    else
       write(*,'(a)', ADVANCE='NO') 'Name of output file:                    '
       read(*,'(a)',iostat=iError) NameFileOut
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
    !
    ! #COORD
    ! MAG
    !
    ! name lat lon
    ! #START
    ! YKC 68.93 299.36
    ! ...

    integer:: iPoint         ! line number in position file
    real:: Lon, Lat
    ! Open the position file
    !--------------------------------------------------------------------------
    call open_file(File=NameFilePoints, Status="old", NameCaller=NameProgram)

    ! Find the number of points in the point position file
    ! Read the header first
    do
       read(UnitTmp_,'(a)',iostat=iError) StringLine
       if(iError /= 0)then
          write(*,*) NameProgram//': no #START in position file'
          STOP
       end if
       if(StringLine(1:5) == '#COOR') &
            read(UnitTmp_,'(a3)') NameCoordPoint
       if(index(StringLine, '#START') > 0) EXIT
    end do
    ! Count points
    nPoint = 0
    do
       read(UnitTmp_, '(a)', iostat=iError) StringLine
       if(iError /= 0) EXIT
       ! DST is not a real magnetometer station
       if(IsMagStation .and. StringLine(1:3) == 'DST') CYCLE
       nPoint = nPoint + 1
    enddo

    write(*,*) NameProgram, ': nPoint=', nPoint,', CoordPoint=', NameCoordPoint

    ! Allocate arrays to hold times/positions
    if(IsTrajectory) allocate(TrajTime_I(nPoint))
    if(IsMagStation) allocate(NameMag_I(nPoint))
    allocate(CoordPoint_DI(2,nPoint), CoordIn_DI(2,nPoint))

    ! Rewind to start of file for reading times/positions.
    rewind(unit=UnitTmp_)

    ! Skip header
    do
       read(UnitTmp_,'(a)') StringLine
       if(index(StringLine, '#START') > 0) EXIT
    end do

    ! Read point information
    do iPoint = 1, nPoint
       read(UnitTmp_,'(a)') StringLine
       if(IsMagStation)then
          ! DST is not a real station, so skip it
          if(StringLine(1:3) == 'DST') read(UnitTmp_,'(a)') StringLine

          read(StringLine, *, iostat=iError) NameMag_I(iPoint), Lat, Lon
          if(Lon < -360 .or. Lon > 360 .or. Lat < -90 .or. Lat > 90)then
             write(*,*) NameProgram// &
                  ': incorrect Lat, Lon values in '//trim(StringLine)
             STOP
          end if
          if(Lon < 0) Lon = Lon + 360
          CoordPoint_DI(:,iPoint) = [ Lon, Lat ]
       else
          read(StringLine, *, iostat=iError) iTime_I, CoordPoint_DI(:,iPoint)
       end if
       if(iError /= 0)then
          write(*,*) NameProgram,': error reading line ',iPoint,':'
          write(*,*) trim(StringLine)
          STOP
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
  subroutine read_through_first

    ! Read first snapshot to figure out start time, coordinates,
    ! coordinate system, number of snapshots

    use ModCoordTransform, ONLY: xyz_to_lonlat, lonlat_to_xyz
    use CON_planet, ONLY: init_planet_const, set_planet_defaults
    use CON_axes, ONLY: init_axes, transform_matrix

    character(len=500):: StringHeader
    integer:: iPoint
    real:: Time, CoordMax_D(2), CoordMin_D(2), dCoord_D(2)
    real:: PointToIn_DD(3,3), XyzPoint_D(3), XyzIn_D(3)
    !--------------------------------------------------------------------------
    call read_plot_file(&
         NameFile = NameFileIn,          &
         TypeFileIn = TypeFileIn,        &
         StringHeaderOut = StringHeader, &
         NameVarOut = NameVar,           &
         TimeOut = Time,                 & ! simulation time
         nDimOut = nDim,                 & ! number of dimensions
         IsCartesianOut = IsCartesian,   &
         nVarOut = nVar,                 & ! number of variables
         n1Out = n1,                     & ! grid sizes
         n2Out = n2 )

    ! Read time from header, if possible
    !    (avoids insufficient precision of real4)
    iTime_I = 0
    read(StringHeader(:19),'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)', &
         iostat=iError) iTime_I(1:6)
    if(iError == 0)then
       call time_int_to_real(iTime_I, Time)
    end if
    write(*,*) NameProgram,': initial Time=', Time,', n1=', n1, ', n2=', n2
    write(*,*) NameProgram,': nDim=', nDim, ', nVar=', nVar, ', NameVar=', &
         trim(NameVar)

    if(nDim /= 2)then
       write(*,*) NameProgram//' only works for 2D input file for now'
       STOP
    end if

    if(IsMagStation)then
       ! Extract coordinate system
       if(index(StringHeader, 'GEO') > 0) NameCoordIn = 'GEO'
       if(index(StringHeader, 'MAG') > 0) NameCoordIn = 'MAG'
       if(index(StringHeader, 'SMG') > 0) NameCoordIn = 'SMG'
       if(index(StringHeader, 'GSM') > 0) NameCoordIn = 'GSM'
       write(*,*) NameProgram,': CoordIn=', NameCoordIn
    end if

    ! Fix start time (subtract simulation time)
    StartTimeFileIn = StartTime - Time

    ! Convert point coordinates into the input coordinates
    if(NameCoordIn /= NameCoordPoint)then
       ! This works when the two coordinates systems don't move relative
       ! to each other, like GEO and MAG

       call init_planet_const
       call set_planet_defaults
       call init_axes(StartTimeFileIn)

       PointToIn_DD = transform_matrix(0.0, NameCoordPoint, NameCoordIn)
       write(*,*) NameProgram, &
            ': convert from ', NameCoordPoint, ' to ', NameCoordIn
       do iPoint = 1, nPoint
          call lonlat_to_xyz(CoordPoint_DI(:,iPoint)*cDegToRad, XyzPoint_D)
          XyzIn_D = matmul(PointToIn_DD, XyzPoint_D)
          call xyz_to_lonlat(XyzIn_D, CoordIn_DI(:,iPoint))
       end do
       CoordIn_DI = cRadToDeg*CoordIn_DI
    else
       CoordIn_DI = CoordPoint_DI
    end if

    ! Allocate variable array
    allocate(Var_VII(nVar,n1,n2))

    ! Get coordinate limits
    call read_plot_file( &
         NameFileIn, TypeFileIn=TypeFileIn, iUnitIn=UnitTmp_, &
         VarOut_VII=Var_VII, &
         CoordMinOut_D=CoordMin_D, CoordMaxOut_D=CoordMax_D, iErrorOut=iError)

    if(iError /= 0)then
       write(*,*) NameProgram// &
            ': could not read first snapshot, iError=', iError
       STOP
    end if

    dCoord_D = (CoordMax_D - CoordMin_D)/ [n1-1, n2-1]

    write(*,*) NameProgram,': CoordMin_D=', CoordMin_D
    write(*,*) NameProgram,': CoordMax_D=', CoordMax_D
    write(*,*) NameProgram,': dCoord_D  =', dCoord_D

    ! Normalize point coordinates
    allocate(CoordNorm_DI(2,nPoint))
    do iPoint = 1, nPoint
       CoordNorm_DI(:,iPoint) = &
            1 + (CoordIn_DI(:,iPoint) - CoordMin_D) / dCoord_D
    end do

    ! Read the whole file through to get the number of snapshots
    nSnapshot = 1
    ! matching initial times require one fewer snapshots
    if(IsTrajectory)then
       if(TrajTime_I(1) > -StartTimeFileIn)then
          nSnapshot = 0
       end if
    end if

    do
       call read_plot_file( &
            NameFileIn, TypeFileIn=TypeFileIn, iUnitIn=UnitTmp_, &
            StringHeaderOut=StringHeader, VarOut_VII=Var_VII, TimeOut=Time, &
            iErrorOut=iError)

       if(iError /= 0) EXIT

       if(IsTrajectory)then
          iTime_I = 0
          read(StringHeader(:19),'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)', &
               iostat=iError) iTime_I(1:6)

          if(iError /= 0)then
             write(*,*) NameProgram,&
                  ': could not read date from header line.', trim(NameFileIn)
             STOP
          end if
          call time_int_to_real(iTime_I, Time)

          Time = Time - StartTime

          if(Time < TrajTime_I(1)) CYCLE  ! before start of trajectory file
          if(Time > TrajTime_I(nPoint)) EXIT  ! after end of trajectory file
       end if

       nSnapshot = nSnapshot + 1
    end do
    close(UnitTmp_)

    write(*,*) NameProgram,': nSnapshot=', nSnapshot

  end subroutine read_through_first
  !============================================================================
  subroutine interpolate_mag_station

    ! Interpolate dB to the position of the magnetometer stations

    real,    allocatable:: Interp_VG(:,:,:) ! interpolated vars on Lon-Lat grid
    real,    allocatable:: Interp_VII(:,:,:)! vars per snapshot and station
    real,    allocatable:: InterpOld_VG(:,:,:)  ! vars per snapshot and station
    real,    allocatable:: InterpNbor_VII(:,:,:)! vars per snapshot and station
    integer, allocatable:: iTime_II(:,:) ! Date-time for each snapshot

    real:: Time,TimeOld ! simulation time
    integer:: iSnapshot, iPoint, iVar, i0, j0, iMin, iMax, jMin, jMax, Ij_D(2)
    integer:: iLocal, jLocal

    logical :: DoTest = .False.

    ! Extra cell for periodic longitudes
    !--------------------------------------------------------------------------
    allocate(Interp_VG(5,n1+1,n2), Interp_VII(5,nSnapshot,nPoint),   &
         iTime_II(6,nSnapshot), InterpNbor_VII(4,nSnapshot,nPoint),  &
         InterpOld_VG(3,n1+1,n2))

    do iSnapshot = 1, nSnapshot
       ! Read variable and time
       call read_plot_file( &
            NameFileIn, TypeFileIn=TypeFileIn, iUnitIn=UnitTmp_, &
            TimeOut = Time, VarOut_VII=Var_VII, iErrorOut=iError)

       if(iError /= 0)then
          write(*,*) NameProgram, &
               ' ERROR reading iSnapshot,iError=', iSnapshot,iError
          EXIT
       end if

       call time_real_to_int(StartTimeFileIn + Time, iTime_I)
       iTime_II(:,iSnapshot) = iTime_I(1:6)

       ! Copy the first three variables (dBn dBe dBd)
       Interp_VG(1:3,1:n1,:) = Var_VII(1:3,:,:)

       ! Copy the last two variable (LonSm LatSm)
       Interp_VG(4:5,1:n1,:) = Var_VII(nVar-1:nVar,:,:)

       ! Apply periodic longitudes
       Interp_VG(:,n1+1,:) = Interp_VG(:,1,:)

       ! Correct the 360 degree jump in LonSm
       Interp_VG(4,n1+1,:) = Interp_VG(4,1,:) + 360.0

       do iPoint = 1, nPoint
          Interp_VII(:,iSnapshot,iPoint) = &
               bilinear(Interp_VG, 5, 1, n1+1, 1, n2, &
               CoordNorm_DI(:,iPoint), DoExtrapolate=.false.)

          ! Correct longitude to be in the 0,360 range
          Interp_VII(4,iSnapshot,iPoint) = &
               modulo(Interp_VII(4,iSnapshot,iPoint), 360.0)
       end do
       ! Overwrite dBn dBe dBd with largest magnitude in nearby cells
       if(Di + Dj > 0)then
          do iPoint = 1, nPoint
             i0 = floor(CoordNorm_DI(1,iPoint))
             j0 = floor(CoordNorm_DI(2,iPoint))
             iMin = max(1, i0 - Di/2); iMax = min(n1+1, i0 + (Di+1)/2)
             jMin = max(1, j0 - Dj/2); jMax = min(n2,   j0 + (Dj+1)/2)
             do iVar = 1, 3
                Ij_D = maxloc(abs(Interp_VG(iVar,iMin:iMax,jMin:jMax)))
                iLocal = Ij_D(1)+iMin-1
                jLocal = Ij_D(2)+jMin-1
                Interp_VII(iVar,iSnapshot,iPoint) = &
                     Interp_VG(iVar,iLocal,jLocal)
             end do

             ! maximum dB and dB/dt in the neighboring cells
             ! Copy the last two variable (LonSm LatSm)
             InterpNbor_VII( 3:4,iSnapshot,iPoint) = &
                  Interp_VII(4:5,iSnapshot,iPoint)
             ! find the index with the maximum dB and set the value
             ! Only consider the North (1) and East (2) components
             Ij_D = maxloc(sum(Interp_VG(1:2,iMin:iMax,jMin:jMax)**2,DIM=1))
             iLocal = Ij_D(1)+iMin-1
             jLocal = Ij_D(2)+jMin-1
             InterpNbor_VII(1,iSnapshot,iPoint) = &
                  sqrt(sum(Interp_VG(1:2,iLocal,jLocal)**2))

             if (DoTest) then
                write(*,*) '============================================'
                write(*,*) 'Interp_VG(1,iMin:iMax,jMin:jMax) =', &
                     Interp_VG(1,iMin:iMax,jMin:jMax)
                write(*,*) 'Interp_VG(2,iMin:iMax,jMin:jMax) =', &
                     Interp_VG(2,iMin:iMax,jMin:jMax)
                write(*,*) 'dB(iMin:iMax,jMin:jMax)          =', &
                     sum(Interp_VG(1:2,iMin:iMax,jMin:jMax)**2,DIM=1)
                write(*,*) 'Ij_D  =', Ij_D
                write(*,*) 'dBn(Ij_D), dBe(Ij_D), dBn(Ij_D)  =', &
                     Interp_VG(1:2,iLocal,jLocal),               &
                     InterpNbor_VII(1,iSnapshot,iPoint)
             endif

             ! find the index with the maximum dB/dt and set the value
             if (iSnapshot > 1) then
                Ij_D = maxloc(sum(                                         &
                     ( Interp_VG(   1:2,iMin:iMax,jMin:jMax)               &
                     - InterpOld_VG(1:2,iMin:iMax,jMin:jMax))**2, DIM=1))
                iLocal = Ij_D(1)+iMin-1
                jLocal = Ij_D(2)+jMin-1
                InterpNbor_VII(2,iSnapshot,iPoint) = sqrt(sum(             &
                     ( Interp_VG(   1:2,iLocal,jLocal)                     &
                     - InterpOld_VG(1:2,iLocal,jLocal))**2 ))/(Time-TimeOld)

                if (DoTest) then
                   write(*,*) '--------------------------------------------'
                   write(*,*) 'dBndt(iMin:iMax,jMin:jMax)       =',        &
                        Interp_VG(     1,iMin:iMax,jMin:jMax)              &
                        - InterpOld_VG(1,iMin:iMax,jMin:jMax)
                   write(*,*) 'dBedt(iMin:iMax,jMin:jMax)       =',        &
                        Interp_VG(     2,iMin:iMax,jMin:jMax)              &
                        - InterpOld_VG(2,iMin:iMax,jMin:jMax)
                   write(*,*) 'dBdt(iMin:iMax,jMin:jMax)        =', sum(   &
                        ( Interp_VG(   1:2,iMin:iMax,jMin:jMax)            &
                        - InterpOld_VG(1:2,iMin:iMax,jMin:jMax))**2, DIM=1)
                   write(*,*) 'Ij_D  =', Ij_D
                   write(*,*) 'dBndt(Ij_D), dBedt(Ij_D), dBdt(Ij_D) =',    &
                        Interp_VG(1:2,iLocal,jLocal)                       &
                        -InterpOld_VG(1:2,iLocal,jLocal),                  &
                        InterpNbor_VII(2,iSnapshot,iPoint)
                endif
             else
                ! dBdt is set to 0 for the first snapshot
                InterpNbor_VII(2,iSnapshot,iPoint) = 0
             endif
          end do
          ! store for dB/dt calculation
          InterpOld_VG = Interp_VG(1:3,:,:)
          TimeOld      = Time
       end if
    end do
    close(UnitTmp_)

    do iPoint = 1, nPoint
       call open_file(FILE=trim(NameDirOut)//NameMag_I(iPoint)//'.txt')
       write(UnitTmp_, '(a)') &
            '# North, East and vertical components of magnetic field in nT'
       write(UnitTmp_, '(a)') &
            '# computed from magnetosphere and ionosphere currents'
       write(UnitTmp_, '(a,2f10.3)') &
            '# '//NameCoordPoint//' lon, lat=', CoordPoint_DI(:,iPoint)
       if(NameCoordIn /= NameCoordPoint) write(UnitTmp_, '(a,2f10.3)') &
            '# '//NameCoordIn//' lon, lat=', CoordIn_DI(:,iPoint)
       write(UnitTmp_, '(a)') &
            '# Station: '//NameMag_I(iPoint)
       write(UnitTmp_, '(a)') &
            'Year Month Day Hour Min Sec '// &
            'B_NorthGeomag B_EastGeomag B_DownGeomag SmLon SmLat'
       do iSnapshot = 1, nSnapshot
          write(UnitTmp_,'(i4,5i3,5f10.3)') &
               iTime_II(:,iSnapshot), Interp_VII(:,iSnapshot,iPoint)
       end do
       call close_file

       if(Di + Dj > 0)then
          ! the file containing the maximum dB and dB/dt in neighboring cells
          call open_file(FILE=trim(NameDirOut)//NameMag_I(iPoint)//'_nbor.txt')
          write(UnitTmp_, '(a)') &
               '# Maximum dB [nT] and dB/dt [nT/s] in neighboring cells'
          write(UnitTmp_, '(a)') &
               '# computed from magnetosphere and ionosphere currents'
          write(UnitTmp_, '(a,2f10.3)') &
               '# '//NameCoordPoint//' lon, lat=', CoordPoint_DI(:,iPoint)
          if(NameCoordIn /= NameCoordPoint) write(UnitTmp_, '(a,2f10.3)') &
               '# '//NameCoordIn//' lon, lat=', CoordIn_DI(:,iPoint)
          write(UnitTmp_, '(a, 2i4)') &
               '# Di, Dj=', Di, Dj
          write(UnitTmp_, '(a)') &
               '# Station: '//NameMag_I(iPoint)
          write(UnitTmp_, '(a)') &
               'Year Month Day Hour Min Sec '// &
               'dB dBdt SmLon SmLat'
          do iSnapshot = 1, nSnapshot
             write(UnitTmp_,'(i4,5i3,4f10.3)') &
                  iTime_II(:,iSnapshot), InterpNbor_VII(:,iSnapshot,iPoint)
          end do
          call close_file
       end if
    end do

    deallocate(iTime_II, Interp_VG, Interp_VII)

  end subroutine interpolate_mag_station
  !============================================================================
  subroutine interpolate_trajectory

    ! Interpolate trajectory position to snapshot time and
    ! then interpolate state variables to this position and write it out

    real, allocatable:: Coord_DII(:,:,:)
    real, allocatable:: InterpData_VI(:,:) ! fully interpolated output
    real, allocatable:: InterpCoord_DI(:,:)! time interpolated trajectory
    real, allocatable:: TimeOut_I(:)       ! time of snapshots
    character(len=500):: StringHeader      ! required for timestamp

    real   :: InterpCoord_D(2) ! interpolated trajectory coordinates
    real   :: RadMin, RadMax, PhiMin, PhiMax, dPhi
    real   :: Weight
    integer:: iTrajTimestamp, iSnapshot ! loop indices

    character(len=*), parameter:: NameSub = 'interpolate_trajectory'
    !--------------------------------------------------------------------------
    if(allocated(Var_VII)) deallocate(Var_VII)

    ! Create arrays to hold interpolated data.
    ! Reallocate Var_VII with ghost cells.

    allocate(                               &
         Coord_DII(nDim, n1, 0:n2+1),       &
         Var_VII(nVar, n1, 0:n2+1),         &
         TimeOut_I(nSnapshot),              &
         InterpData_VI(nVar,nSnapshot),     &
         InterpCoord_DI(nDim,nSnapshot))

    iSnapshot = 0
    iTrajTimestamp = 1
    do
       ! Read next snapshot from .outs file
       call read_plot_file( &
            NameFile = NameFileIn,              &
            TypeFileIn = TypeFileIn,            &
            iUnitIn = UnitTmp_,                 &
            StringHeaderOut=StringHeader,       &
            CoordOut_DII = Coord_DII(:,:,1:n2), &
            VarOut_VII = Var_VII(:,:,1:n2),     &
            TimeOut = Time,                     & ! real4 simulation time
            iErrorOut = iError)

       if(iError /= 0) EXIT

       ! Read time from header and convert to simulation time.
       ! This is required for long runs that exceed real4
       !   precision for simulation time.
       read(StringHeader(:19),'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)', &
            iostat=iError) iTime_I(1:6)
       if(iError /= 0)then
          write(*,*) NameSub,': could not read date from header line.'
          STOP
       end if
       call time_int_to_real(iTime_I, Time)
       Time = Time - StartTime

       if(Time < TrajTime_I(1)) CYCLE  ! before start of trajectory file
       if(Time > TrajTime_I(nPoint)) EXIT  ! after end of trajectory file

       iSnapshot = iSnapshot + 1

       ! Interpolate location of trajectory file in time to snapshot.
       do while(Time > TrajTime_I(iTrajTimestamp))
          iTrajTimestamp = iTrajTimestamp + 1
       enddo
       if(Time == TrajTime_I(iTrajTimestamp))then
          InterpCoord_D = CoordPoint_DI(:,iTrajTimestamp)
       else
          Weight = (TrajTime_I(iTrajTimestamp) - Time)/ &
               (TrajTime_I(iTrajTimestamp)-TrajTime_I(iTrajTimestamp-1))
          InterpCoord_D = &
               CoordPoint_DI(:,iTrajTimestamp-1)*Weight + &
               CoordPoint_DI(:,iTrajTimestamp)*(1-Weight)
       endif

       ! Convert (x,y) coordinates to (logr,phi) [deg]
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

       ! Fill in ghost cells in phi
       Var_VII(:,:,0)    = Var_VII(:,:,n2)
       Var_VII(:,:,n2+1) = Var_VII(:,:,1)

       ! Normalize coordinates
       InterpCoord_D(1) = (InterpCoord_D(1)-RadMin)*(n1-1)/(RadMax-RadMin) + 1
       InterpCoord_D(2) = (InterpCoord_D(2)-PhiMin)*(n2+1)/(PhiMax-PhiMin)

       ! Interpolate snapshot to trajectory location
       InterpData_VI(:,iSnapshot) = interpolate_vector( &
            a_VC = Var_VII, & ! variable array
            nVar = nVar, &    ! number of variables (11)
            nDim = 2, &       ! number of dimensions
            Min_D = [1,0], &
            Max_D = [n1,n2+1], &
            x_D = InterpCoord_D, & ! desired position
            DoExtrapolate = .false.)

       ! Undo coordinate normalization for writing to file.
       ! Convert from ln(r) to linear r
       InterpCoord_DI(1,iSnapshot) = exp((InterpCoord_D(1)-1) &
            * (RadMax-RadMin)/(n1-1)+RadMin)
       InterpCoord_DI(2,iSnapshot) = InterpCoord_D(2) &
            * (PhiMax-PhiMin)/(n2+1)+PhiMin
       TimeOut_I(iSnapshot) = Time
    enddo

    call close_file(NameCaller=NameProgram)
    deallocate(Coord_DII, Var_VII)

    ! Open file for interpolated output
    call open_file(file=NameFileOut, NameCaller=NameProgram)

    ! Include header
    write(UnitTmp_,'(a,a)')'t ', trim(NameVar)

    ! Write data. For now, time is in simulation time
    do iSnapshot = 1, nSnapshot
       write(UnitTmp_,'(100es13.6)') TimeOut_I(iSnapshot), &
            InterpCoord_DI(:,iSnapshot), InterpData_VI(:,iSnapshot)
    enddo

    call close_file(NameCaller=NameProgram)

  end subroutine interpolate_trajectory
  !============================================================================

end program
!==============================================================================

