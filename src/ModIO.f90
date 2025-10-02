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
  integer, parameter :: MaxMagGridFile=5
  integer, parameter :: MaxPlotFile=25
  integer, parameter :: MaxParcel=100
  integer, parameter :: MaxFile = 450
  integer, parameter :: MaxPlotvarLos=20
  integer, parameter :: MaxPlotRadioFreq=20
  integer, parameter :: MaxPlotvar = max(30,nVar+10) ! Max number of plot vars
  integer, parameter :: MaxLine=20          ! Max number of lines/plot file
  integer, parameter :: lNameLogVar = 20    ! Max length of NameLogVar

  ! Named indexes for output files
  integer, parameter :: &
       restart_=1, logfile_=2, magfile_=3, indexfile_=4, maggridfile_=5, &
       plot_=maggridfile_+MaxMagGridFile, parcel_=plot_+MaxPlotfile,     &
       satellite_ = parcel_+MaxParcel

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

  logical :: IsRestart = .false.        ! read restart file
  logical :: IsRestartCoupler = .false. ! Informs coupler if restarted
  logical :: DoRestartBface = .false.   ! Bface restarted

  logical :: DoSaveInitial = .false.

  logical :: DoSaveRestart = .true., &
       DoSavePlotsAmr = .false., DoSaveLogfile = .false., &
       DoSaveBinary = .true., DoSaveTecBinary = .false.
  !$acc declare create(DoSaveTecBinary)

  ! Unit numbers for the log file
  integer :: iUnitLogfile = -1

  ! variables for the line of sight integration plots
  character(len=10) :: TypeLosImage
  integer :: nPixel_I(MaxFile)
  real :: rSizeImage_I(MaxFile), xOffset_I(MaxFile), yOffset_I(MaxFile)
  real :: rOccult_I(MaxFile), MuLimbDarkening
  real :: OffsetAngle_I(MaxFile)
  real :: ObsPos_DI(3,MaxFile)
  character(len=20) :: NameLosTable_I(MaxFile) = '???'
  logical:: UseLosSimple = .false. !!! experiment with simple LOS algorithm

  ! variables for the line of sight plots using the instrument names
  character(len=10)  :: TypeSatPos_I(MaxFile)

  ! Logical variable for OBS box type
  logical  :: IsObsBox_I(MaxFile) = .false.

  ! Variables for radiowave image
  ! ObsPos_DI is borrowed from the LOS plot
  integer :: nPixelX_I(MaxFile), nPixelY_I(MaxFile)
  real :: xSizeImage_I(MaxFile), ySizeImage_I(MaxFile)

  ! String read from PARAM.in, like '1500kHz, 11MHz, 42.7MHz, 1.08GHz':
  character(len=100) :: StringRadioFrequency_I(MaxFile)
  real :: RadioFrequency_II(MaxFile,MaxPlotRadioFreq)
  logical :: UseNoRefraction = .false.

  ! Variables for SPECTRUM-DEM/EM calculation
  real, dimension(MaxFile) :: &
       LogTeMinDEM_I = 4.0, LogTeMaxDEM_I = 7.0, DLogTeDEM_I = 0.1

  ! Variables for SPECTRUM-FUX/PHX/NBI calculation
  character (len=200)        :: NameSpmTable_I(MaxFile), &
       NameNbiTable_I(MaxFile), NamePhxTable_I(MaxFile)
  logical                   :: UseUnobserved_I(MaxFile) = .false.,&
       UseAlfven_I(MaxFile) = .false., UseDoppler_I(MaxFile) = .false.,&
       UseIonFrac_I(MaxFile) = .false., UseIonTemp_I(MaxFile) = .false.
  real, dimension(MaxFile)  :: LambdaMin_I = 10.0, LambdaMax_I=1700.0,&
       DLambda_I = 0.001, DLambdaIns_I = 0.0, TempMin_I = 1e5

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
  real ::    DtOutput_I(MaxFile) = -1.0
  integer :: DnOutput_I(MaxFile) = -1
  integer :: nStepOutputLast_I(MaxFile) = -1
  integer :: iTimeOutputLast_I(MaxFile) = -1

  ! Frequency of writing progress reports in terms of time steps
  integer :: DnProgressShort=10, DnProgressLong=100

  character(LEN=3)  :: TypePlotFormat_I(MaxFile) = '?'
  character(LEN=3)  :: TypeLogFormat = '??'
  character(LEN=3)  :: TypeCoordPlot_I(MaxFile) = 'SYS'
  character(LEN=10) :: TypeFile_I(MaxFile) = '??????'
  character(LEN=20) :: TypePlot_I(MaxFile) = '????', TypePlot = '?????'

  ! x,y,z limits for plotting
  real :: PlotRange_EI(6,MaxFile) = 0.0

  ! plot range for the current file
  real :: PlotRange_I(6)

  ! point coordinates for arbitrary slice plotting
  real :: PlotPointXyz_DI(3,MaxFile) = -99999.0

  ! normal vector for arbitrary slice plotting
  real :: PlotNormal_DI(3,MaxFile)

  ! resolution for equidistant plotting
  real :: PlotDx_DI(3,MaxFile)

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
  character(len=20) :: NameVarUserTec_I(MaxPlotvar), &
       NameUnitUserTec_I(MaxPlotvar), NameUnitUserIdl_I(MaxPlotvar)

  ! Dimensional factors for plots
  real :: DimFactor_V(MaxPlotvar), DimFactorBody_V(MaxPlotvar)

  ! Plot file name format: n for nStep, t for TimeSimulation, e for event date
  ! One of the three must be true
  logical :: IsPlotNameN = .true.   !  true if time accurate false
  logical :: IsPlotNameT = .true.   ! false if time accurate false
  logical :: IsPlotNameE = .false.  ! false if time accurate false

  ! Log file name format: n for nStep, e for event dat
  !  One of the two must be true
  !  Time accurate flag false will automatically set IsLogNameN true
  !  when file is opened.
  logical :: IsLogNameN = .true.
  logical :: IsLogNameE = .false.

  logical :: DoSaveOneTecFileOrig = .true., DoSaveOneTecFile
  !$acc declare create(DoSaveOneTecFile)

#ifdef NOMPIIO
  logical :: DoSaveOneIdlFile = .false.
#else
  logical :: DoSaveOneIdlFile = .true.
#endif

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
