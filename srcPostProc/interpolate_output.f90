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
  use ModUtilities,      ONLY: open_file, close_file
  use ModIoUnit,         ONLY: UnitTmp_
  
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
  integer :: n_D(0:3), n1, n2, n3
  integer :: i, iTrajTimestamp ! loop indices
  real, allocatable :: InterpData_VI(:,:) ! fully interpolated output
  real :: InterpCoord_D(2) ! interpolated trajectory coordinates
  integer :: MaxSnapshot ! limit of snapshots in .outs file
  integer :: nSnapshots ! number of snapshots contained in .outs file
  real :: RadMin,RadMax,PhiMin,PhiMax
  integer :: weight
  real :: dR, dPhi
  
  ! Interpolated File
  character(len=100) :: NameInterpFile ! name of desired output file
  real,allocatable :: TimeOut_D(:)
  real,allocatable :: InterpCoord_DI(:,:)
  character(len=*), parameter:: NameProgram = 'interpolate_output'
  !-------------------------------------------------------------------------

  ! READ TRAJECTORY FILE OF POSITIONS AND TIMES
  !  Should be in the same format as satellite files.
  !  All commands prior to #START will be ignored.
  !  Coordinate system is assumed to be the same as the .outs file.
  ! Example:
  !             year mo dy hr mn sc msc x y z
  !             #START
  !              1997 5 17 3 35 00 000 -0.368 5.275 0.0

  call open_file(File='INTERPOLATE.in', Status='old', NameCaller=NameProgram)
  read(UnitTmp_,'(a)',iostat=iError) NameFileIn
  read(UnitTmp_,'(a)',iostat=iError) TypeFileIn
  read(UnitTmp_,'(a)',iostat=iError) NameTrajFile
  read(UnitTmp_,'(a)',iostat=iError) NameInterpFile
  read(UnitTmp_,*,iostat=iError) StartTime
  call close_file(NameCaller=NameProgram)
  
  !---------------------------------------------------------------------------
  !                   READ TRAJECTORY
  
  ! Open the trajectory file.
  call open_file(File=NameTrajFile, Status="old", NameCaller=NameProgram)
  
  ! Find the number of points in the trajectory file.
  nPoints = 0
  COUNTPOINTS: do
     read(UnitTmp_,'(a)',iostat=iError) StringTraj
     if(index(StringTraj, '#START')>0)then
        do
           read(UnitTmp_, '(a)', iostat=iError) StringTraj
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
  rewind(unit=UnitTmp_) 

  ! Read in the trajectory times/positions.
  READFILE: do
     read(UnitTmp_,'(a)',iostat=iError) StringTraj
     if(index(StringTraj, '#START')>0)then
        do iLine=1, nPoints
           read(UnitTmp_, *, iostat=iError) iTrajTime_DI(:,iLine), &
                Xy_DI(:,iLine)
           if(iError/=0)EXIT READFILE

           ! Convert integer time to simulation time.
           call time_int_to_real(iTrajTime_DI(:,iLine), DateTime)
           TrajTime_I(iLine) = DateTime - StartTime
        enddo
     endif
  enddo READFILE
  
  ! Close the trajectory file.
  call close_file(NameCaller=NameProgram)
  
  !------------------------------------------------------------------------
  !                            INTERPOLATE
  MaxSnapshot = 100000
  nSnapshots = 0
  iTrajTimestamp = 1
  do i=1,MaxSnapshot
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
        InterpCoord_D = Xy_DI(:,iTrajTimestamp)
     else
        weight = (TrajTime_I(iTrajTimestamp) - Time)/ &
             (TrajTime_I(iTrajTimestamp)-TrajTime_I(iTrajTimestamp-1))
        InterpCoord_D = Xy_DI(:,iTrajTimestamp-1)*weight + &
             Xy_DI(:,iTrajTimestamp)*(1-weight)
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
        dR = (RadMax - RadMin)/(n1-1)
        dPhi = (PhiMax - PhiMin)/(n2-1)
        PhiMin = PhiMin - dPhi ! account for ghost cells
        PhiMax = PhiMax + dPhi
     endif

     ! Fill in ghost cells in phi.
     Var_VII(:,:,0) = Var_VII(:,:,n2)
     Var_VII(:,:,n2+1) = Var_VII(:,:,1)

     ! Normalize coordinates.
     InterpCoord_D(1) = (InterpCoord_D(1)-RadMin) * (n1-1)/(RadMax-RadMin) + 1
     InterpCoord_D(2) = (InterpCoord_D(2)-PhiMin) * (n2+1)/(PhiMax-PhiMin)

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
     TimeOut_D(i) = Time

     nSnapshots = i ! Update number of snapshots in .outs file
  enddo
  
  call close_file(NameCaller=NameProgram)
  deallocate(Coord_DII, Var_VII)

  
  !----------------------------------------------------------------------------
  !                              OUTPUT FILE
  ! Open file for interpolated output.
  call open_file(file=NameInterpFile, NameCaller=NameProgram) 

  ! Include header.
  write(UnitTmp_,*)'t ',NameVar

  ! Write data. For now, time is in simulation time.
  do iLine=1,nSnapshots
     write(UnitTmp_,*)TimeOut_D(iLine), &
          InterpCoord_DI(:,iLine), InterpData_VI(:,iLine)
  enddo
  
  call close_file(NameCaller=NameProgram)

  
end program interpolate_output





