! 2020.08.10  Keebler
! From an input location timeseries, output interpolated values
! along the trajectory.
!

program interpolate_output
  
  ! Read a *.outs IDL file of type ascii/real4/real8 and a satellite file,
  ! then interpolate the *.outs to the trajectory from the satellite file.

  use ModIoUnit,         ONLY: io_unit_new
  use ModPlotFile,       ONLY: read_plot_file
  use ModTimeConvert,    ONLY: time_int_to_real
  use ModConst,          ONLY: cRadToDeg
  use ModInterpolate,    ONLY: interpolate_vector
  use ModCoordTransform, ONLY: atan2_check
  
  implicit none

  ! Trajectory File
  character(len=100)   :: StringTraj ! single line from trajectory file
  integer, allocatable :: iTrajTime_DI(:,:) ! integer times
  real, allocatable    :: TrajTime_I(:) ! simulation time from trajectory file
  real, allocatable    :: Xy_DI(:,:) ! positions at each timestamp
  character(len=100)   :: NameTrajFile ! name of desired trajectory file
  integer :: iLine ! line number in trajectory file
  integer :: nPoints ! number of timestamps in trajectory file
  real    :: StartTime ! simulation start time
  real    :: DateTime ! timestamp in simulation time
  
  ! I/O
  integer :: iUnitTraj   ! iounit of trajectory file
  integer :: iUnitIn     ! iounit of .outs file
  integer :: iUnitInterp ! iounit of interpolated file
  integer :: iUnitQuery  ! iounit of query file
  integer :: iError      ! used for all files

  ! .outs File
  character(len=100) :: NameFileIn
  character(len=10)  :: TypeFileIn  ! ascii/real4/real8
  character(len=500) :: StringHeader
  character(len=500) :: NameVar
  integer            :: nStep, nDim, nParam, nVar
  logical            :: IsCartesian
  real              :: Time ! snapshot time
  real              :: Param_I(100) ! parameters
  real, allocatable :: Coord_DII(:,:,:), Var_VII(:,:,:)
  real, allocatable :: LogCoord_DII(:,:,:)
  integer :: n_D(0:3), n1, n2, n3
  integer :: i, iTrajTimestamp ! loop indices
  real, allocatable :: InterpData_VI(:,:) ! fully interpolated output
  real :: InterpCoord_D(2) ! interpolated trajectory coordinates
  integer :: MaxSnapshot ! limit of snapshots in .outs file
  integer :: nSnapshots ! number of snapshots contained in .outs file
  real :: RadMin,RadMax,PhiMin,PhiMax

  ! Interpolated File
  character(len=100) :: NameInterpFile ! name of desired output file
  real,allocatable :: TimeOut_D(:)
  real,allocatable :: InterpCoord_DI(:,:)
  character(len=*), parameter:: NameSub = 'interpolate_output'
  !-------------------------------------------------------------------------

  ! READ TRAJECTORY FILE OF POSITIONS AND TIMES
  !  Should be in the same format as satellite files.
  !  All commands prior to #START will be ignored.
  !  Coordinate system is assumed to be the same as the .outs file.
  ! Example:
  !             year mo dy hr mn sc msc x y z
  !             #START
  !              1997 5 17 3 35 00 000 -0.368 5.275 0.0

  iUnitQuery = io_unit_new()
  iUnitTraj = io_unit_new()
  iUnitIn = io_unit_new()
  iUnitInterp = io_unit_new()

  open(unit=iUnitQuery,file='query.txt',iostat=iError,status='old')
  read(iUnitQuery,'(a)',iostat=iError) NameFileIn
  read(iUnitQuery,'(a)',iostat=iError) TypeFileIn
  read(iUnitQuery,'(a)',iostat=iError) NameTrajFile
  read(iUnitQuery,'(a)',iostat=iError) NameInterpFile
  read(iUnitQuery,*,iostat=iError) StartTime
  close(iUnitQuery)
  
  !---------------------------------------------------------------------------
  !                   READ TRAJECTORY
  
  ! Open the trajectory file.
  open(unit=iUnitTraj, file=NameTrajFile, iostat=iError, status="old")
  
  ! Find the number of points in the trajectory file.
  nPoints = 0
  COUNTPOINTS: do
     read(iUnitTraj,'(a)',iostat=iError) StringTraj
     if(index(StringTraj, '#START')>0)then
        do
           read(iUnitTraj, '(a)', iostat=iError) StringTraj
           if(iError/=0)EXIT COUNTPOINTS
           nPoints = nPoints + 1
        enddo
     endif
  enddo COUNTPOINTS

  ! Allocate arrays to hold times/positions.
  allocate(iTrajTime_DI(7,nPoints))
  allocate(TrajTime_I(nPoints))
  allocate(Xy_DI(2,nPoints))
  
  ! Rewind to start of file for reading times/positions.
  rewind(unit=iUnitTraj) 

  ! Read in the trajectory times/positions.
  READFILE: do
     read(iUnitTraj,'(a)',iostat=iError) StringTraj
     if(index(StringTraj, '#START')>0)then
        do iLine=1, nPoints
           read(iUnitTraj, *, iostat=iError) iTrajTime_DI(:,iLine), &
                Xy_DI(:,iLine)
           if(iError/=0)EXIT READFILE

           ! Convert integer time to simulation time.
           call time_int_to_real(iTrajTime_DI(:,iLine), DateTime)
           TrajTime_I(iLine) = DateTime - StartTime
        enddo
     endif
  enddo READFILE
  
  ! Close the trajectory file.
  close(unit=iUnitTraj) 
  
  !------------------------------------------------------------------------
  !                            INTERPOLATE
  MaxSnapshot = 100000
  nSnapshots = 0
  do i=1,MaxSnapshot
     ! Read header info from .outs file.
     call read_plot_file(NameFile = NameFileIn, iUnitIn = iUnitIn, &
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

     if(Time < TrajTime_I(1)) CYCLE  ! if before start of trajectory file
     if(Time > TrajTime_I(nPoints)) EXIT  ! if after end of trajectory file

     ! Create array to hold interpolated data.
     if(.not.allocated(InterpData_VI))then
        allocate(TimeOut_D(MaxSnapshot), InterpData_VI(nVar,MaxSnapshot), &
             InterpCoord_DI(nDim,MaxSnapshot))
     endif

     ! Determine the shape of arrays from the header.
     !   Note: assumes no AMR/constant grid
     if(.not. allocated(Coord_DII)) &
          allocate(Coord_DII(nDim, n1, n2), &
          Var_VII(nVar, n1, n2), &
          LogCoord_DII(nDim,n1,n2))

     ! Read the data at the snapshot.
     call read_plot_file(NameFile=NameFileIn, iUnitIn=iUnitIn, &
          TypeFileIn = TypeFileIn, &
          CoordOut_DII = Coord_DII, &
          VarOut_VII = Var_VII)
     
     ! Interpolate location of trajectory file in time to snapshot.
     iTrajTimestamp = 1
     do while(Time > TrajTime_I(iTrajTimestamp))
        iTrajTimestamp = iTrajTimestamp + 1
     enddo
     if(Time == TrajTime_I(iTrajTimestamp))then
        InterpCoord_D = Xy_DI(:,iTrajTimestamp)
     else
        InterpCoord_D = interpolate_vector( &
             a_VC = Xy_DI(:,iTrajTimestamp-1:iTrajTimestamp), &
             nVar = nDim, &
             nDim = 1, &
             Min_D = [1], &
             Max_D = [2], &
             x_D = [Time], &
             x1_I = TrajTime_I(iTrajTimestamp-1:iTrajTimestamp), &
             DoExtrapolate = .false.)
     endif

     ! Convert coordinates to curvilinear.
     if(.not.IsCartesian)then
        ! Coordinates converted from [x,y] to [ln(r),phi]
        InterpCoord_D = [log(sqrt(InterpCoord_D(1)**2 + &
             InterpCoord_D(2)**2)), &
             atan2_check(InterpCoord_D(2), InterpCoord_D(1))*cRadToDeg]

        ! Use for non-uniform coords.
        !LogCoord_DII(1,:,:) = log(sqrt(Coord_DII(1,:,:)**2 + &
        !     Coord_DII(2,:,:)**2))
        !LogCoord_DII(2,:,:) = atan2(Coord_DII(2,:,:), &
        !     Coord_DII(1,:,:))*cRadToDeg

        RadMin = log(sqrt(Coord_DII(1,1,1)**2+Coord_DII(2,1,1)**2))
        RadMax = log(sqrt(Coord_DII(1,n1,n2)**2+Coord_DII(2,n1,n2)**2))
        PhiMin = atan2_check(Coord_DII(2,1,1),Coord_DII(1,1,1))*cRadToDeg
        PhiMax = atan2_check(Coord_DII(2,n1,n2),Coord_DII(1,n1,n2))*cRadToDeg
     endif

     ! Normalize coordinates.
     InterpCoord_D(1) = InterpCoord_D(1)/(RadMax-RadMin)*n1
     InterpCoord_D(2) = InterpCoord_D(2)/(PhiMax-PhiMin)*n2

     ! Interpolate snapshot to trajectory location.
     InterpData_VI(:,i) = interpolate_vector( &
          a_VC = Var_VII, & ! variable array
          nVar = nVar, &    ! number of variables (11)
          nDim = 2, &       ! number of dimensions
          Min_D = [1,1], &
          Max_D = [n1,n2], &
          x_D = InterpCoord_D, & ! desired position
          !x1_I = LogCoord_DII(1,:,1), & ! use for non-normalized coords
          !x2_I = LogCoord_DII(2,1,:), & ! use for non-normalized coords
          DoExtrapolate = .false.)

     InterpCoord_DI(:,i) = InterpCoord_D
     TimeOut_D(i) = Time

     nSnapshots = i ! Update number of snapshots in .outs file
  enddo
  
  close(iUnitIn)
  deallocate(Coord_DII, Var_VII)
  
  !----------------------------------------------------------------------------
  !                              OUTPUT FILE
  ! Open file for interpolated output.
  open(unit=iUnitInterp, file=NameInterpFile, iostat=iError, status="replace") 

  ! Include header.
  write(iUnitInterp,*)'t ',NameVar

  ! Write data. For now, time is in simulation time.
  do iLine=1,nSnapshots
     write(iUnitInterp,*)TimeOut_D(iLine), &
          InterpCoord_DI(:,iLine), InterpData_VI(:,iLine)
  enddo
  
  close(iUnitInterp)

  
end program interpolate_output





