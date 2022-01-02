!  Copyright (C) 2002 Regents of the University of Michigan
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModIO

  use ModSize
  use ModIoUnit, ONLY: UnitTmp_, STDOUT_
  use ModVarIndexes, ONLY: nVar

  implicit none
  SAVE

  ! All constants and variables related to Input/Output for GM

  ! Maximum number of output files and output variables
  ! note that:
  !     MaxFile > MaxPlotFile + MaxSatelliteFile + extras
  ! is required. MaxSatelliteFile is defined in ModSatelliteFile.f90
  integer, parameter :: MaxPlotFile=25
  integer, parameter :: MaxParcel=100
  integer, parameter :: MaxFile = 450
  integer, parameter :: nPlotvarLosMax=20
  integer, parameter :: nPlotRfrFreqMax=20
  integer, parameter :: nPlotvarMax = max(30,nVar+10) ! Max number of plot vars
  integer, parameter :: MaxLine=20          ! Max number of lines/plot file
  integer, parameter :: lNameLogVar = 20    ! Max length of NameLogVar

  ! Named indexes for output files
  integer, parameter :: &
       restart_=1, logfile_=2, magfile_=3, indexfile_=4, maggridfile_=5, &
       plot_=5, parcel_=plot_+MaxPlotfile, satellite_ = parcel_+MaxParcel

  ! I/O
  integer             :: iUnitOut = STDOUT_
  character (len=7)   :: StringPrefix=''
  character (len=100) :: NamePlotDir="GM/IO2/"

  ! Generic file name variable
  character (len=80) :: NameFile

  ! The largest time unit used in the plot file names in time-accurate runs
  character (len=20) :: NameMaxTimeUnit = 'hour'
  ! Simulation time or physical date and time used in file names
  character (len=14) :: StringDateOrTime

  logical :: IsRestart=.false.        ! read restart file
  logical :: DoRestartBface =.false.  ! Bface restarted
  logical :: IsRestartCoupler=.false. ! Informs coupler if restarted

  logical :: DoSaveInitial = .false.

  logical :: DoSaveRestart=.true., &
       DoSavePlotsAmr=.false.,DoSaveLogfile=.false.,DoSaveBinary=.true., &
       DoSaveTecBinary=.false.

  ! Unit numbers for the log file
  integer :: iUnitLogfile = -1

  ! variables for the line of sight integration plots
  character (LEN=10) :: TypeLosImage
  integer :: nPixel_I(MaxFile)
  real :: rSizeImage_I(MaxFile), xOffset_I(MaxFile), yOffset_I(MaxFile)
  real :: rOccult_I(MaxFile), MuLimbDarkening
  real :: OffsetAngle_I(MaxFile)
  real, dimension(3,MaxFile) :: ObsPos_DI
  character (LEN=20) :: NameLosTable_I(MaxFile)
  logical:: UseLosSimple = .false. !!! experiment with simple LOS algorithm

  ! variables for the line of sight plots using the instrument names
  character(LEN=10)  :: TypeSatPos_I(MaxFile)

  ! Logical variable for OBS box type
  logical  :: IsObsBox_I(MaxFile) = .false.

  ! Variables for radiowave image
  ! ObsPos_DI is borrowed from the LOS plot
  integer, dimension(MaxFile) :: nPixelX_I, nPixelY_I
  real,    dimension(MaxFile) :: xSizeImage_I, ySizeImage_I
  ! String read from PARAM.in, like '1500kHz, 11MHz, 42.7MHz, 1.08GHz':
  character(len=100), dimension(MaxFile) :: StringRadioFrequency_I
  real, dimension(MaxFile,nPlotRfrFreqMax) :: RadioFrequency_II
  logical :: UseNoRefraction = .false.

  ! Variables for field/stream/current line files
  logical :: IsSingleLine_I(MaxPlotFile)      ! One subfile for the plot file?
  integer :: nLine_I(MaxPlotFile)             ! Number of lines for a plot file
  character :: NameLine_I(MaxPlotFile)                 ! Name of vector field
  real      :: XyzStartLine_DII(3,MaxLine,MaxPlotFile) ! Starting positions
  logical   :: IsParallelLine_II(MaxLine,MaxPlotFile)  ! Parallel/anti-parallel

  ! Variable for Lagrangian parcel
  real       :: Parcel_DI(3,MaxParcel)= 0
  real       :: StartTimeParcel = -1, EndTimeParcel = -1
  integer    :: nParcel = 0, nStartParcel = -1, nEndParcel = -1
  logical    :: UseParcel = .false., UseParcelTable = .false.
  character(len=500) :: StringParcelVar

  ! Actual number of output files and plot files
  ! note that nfile is not the number of output files but rather the
  ! index of the maximum file number.  The array FileUsed contains a
  ! value that tells whether or not each file is used or not.
  integer :: nFile=0, nPlotFile=0

  ! Saving frequencies and the last saved time step and snapshot number
  real,    dimension(MaxFile) :: DtOutput_I=-1.
  integer, dimension(MaxFile) :: DnOutput_I=-1, &
       nStepOutputLast_I=-1, iTimeOutputLast_I=-1

  ! Frequency of writing progress reports in terms of time steps
  integer :: DnProgressShort=10, DnProgressLong=100

  character(LEN=20) :: TypePlot_I(MaxFile), TypePlot
  character(LEN=3)  :: TypePlotFormat_I(MaxFile)
  character(LEN=3)  :: TypeLogFormat
  character(LEN=10) :: TypeFile_I(MaxFile)

  character(LEN=3)  :: TypeCoordPlot_I(MaxFile) = '???'

  ! xMinBox, xMaxBox, yMinBox, yMaxBox, zMinBox, zMaxBox limits for plotting
  real, dimension(6,MaxFile) :: PlotRange_EI=0.
  ! plot range for the current file
  real :: PlotRange_I(6)

  ! x, y, z point for arbitrary slice plotting
  real, dimension(3,MaxFile) :: PlotPointXyz_DI=-99999.

  ! x, y, z normal vector for arbitrary slice plotting
  real, dimension(3,MaxFile) :: PlotNormal_DI

  ! dx resolution for equidistant plotting
  real, dimension(3,MaxFile) :: PlotDx_DI

  ! variables to plot
  character (len=500) :: StringPlotVar_I(MaxFile), StringPlotVar
  character (len=500) :: StringPlotParam_I(MaxFile), StringPlotParam

  ! variables to put in log file
  character (len=500) :: StringLogVar, StringLogRadius

  ! variables to control time output format
  character (len=100) :: TypeLogTime

  ! dimensionalize the output
  logical :: IsDimensionalPlot_I(MaxFile)

  ! Plot variable names and units defined in the user module
  character(len=20), dimension(nPlotVarMax) :: &
       NameVarUserTec_I, NameUnitUserTec_I, NameUnitUserIdl_I
  ! Dimensional factors for plots
  real,              dimension(nPlotVarMax) :: &
       DimFactor_V, DimFactorBody_V

  ! Plot file name string logicals.
  !  One of the three must be true
  !  Time accurate flag can override
  logical :: IsPlotNameN = .true.   !  true if time accurate false
  logical :: IsPlotNameT = .true.   ! false if time accurate false
  logical :: IsPlotNameE = .false.  ! false if time accurate false

  ! Log file name string logicals.
  !  One of the two must be true
  !  Time accurate flag false will automatically set IsLogNameN true
  !     when file is opened.
  logical :: IsLogNameN = .true.
  logical :: IsLogNameE = .false.

  logical :: DoSaveOneTecFileOrig = .false., DoSaveOneTecFile

  ! The space separated list of nVar primitive variables
  character(len=:), allocatable :: NamePrimitiveVarOrig

  ! The space separated list of primitive/conservative variables for plotting
  character(len=:), allocatable:: NamePrimitiveVarPlot, NameConservativeVarPlot

contains
  !============================================================================

  subroutine write_prefix

    use ModMain, ONLY: IsStandAlone

    !--------------------------------------------------------------------------
    if(IsStandAlone) RETURN
    if(iUnitOut==STDOUT_)write(*,'(a)',ADVANCE='NO')trim(StringPrefix)

  end subroutine write_prefix
  !============================================================================

  subroutine write_myname

    use ModMain, ONLY: NameThisComp, IsStandAlone

    !--------------------------------------------------------------------------
    if(IsStandAlone) RETURN
    if(len_trim(NameThisComp)>0) &
         write(*,'(a)',ADVANCE='NO')NameThisComp//':'

  end subroutine write_myname
  !============================================================================

end module ModIO
!==============================================================================
