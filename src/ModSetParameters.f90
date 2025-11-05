!  Copyright (C) 2001 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModSetParameters

  use BATL_lib, ONLY: &
       test_start, test_stop, StringTest, iVarTest

  use BATL_lib, ONLY:

  implicit none

  private ! except

  public:: set_parameters

contains
  !============================================================================
  subroutine set_parameters(TypeAction)

    ! Set input parameters for BATS-R-US!

    use ModAdvance
    use ModMain
    use ModBuffer, ONLY: read_buffer_grid_param
    use ModSetInitialCondition, ONLY: read_initial_cond_param
    use ModConservative, ONLY: &
         set_non_conservative, UseNonConservative, nConservCrit
    use ModB0, ONLY: UseB0Source, UseDivFullBSource, UseCurlB0, &
         DoUpdateB0, DtUpdateB0, UseB0Wave, read_b0_param, init_mod_b0
    use ModGeometry, ONLY: init_mod_geometry, TypeGeometry, nMirror_D, &
         xMinBox, xMaxBox, yMinBox, yMaxBox, zMinBox, zMaxBox, &
         XyzMin_D, XyzMax_D, RadiusMin, RadiusMax, &
         CoordDimMin_D, CoordDimMax_D, &
         read_gen_radial_grid, set_gen_radial_grid, NameGridFile
    use ModNodes, ONLY: init_mod_nodes
    use ModImplicit
    use ModSemiImplicit, ONLY: read_semi_impl_param, init_mod_semi_impl
    use ModPartImplicit, ONLY: read_part_impl_param, init_mod_part_impl
    use ModLinearSolver, ONLY: UseAccurateSum
    use ModImplHypre, ONLY: hypre_read_param
    use ModProjectDivB, ONLY: read_project_divb_param, DivBMax
    use ModUpdateState, ONLY: read_update_param
    use ModReverseField, ONLY: read_reverse_field_param, DoReverseField, &
         init_mod_reverse_field
    use ModPhysics
    use ModTransitionRegion, ONLY: CoulombLogTr=>CoulombLog
    use ModConstrainDivB, ONLY: init_mod_ct, DoInitConstrainB
    use ModBlockData, ONLY: init_mod_block_data, clean_block_data
    use BATL_lib, ONLY: &
         read_region_param, read_test_param, NameVarTest, iVarTest, &
         UseSimpleProlongation, BetaProlong, IsCartesianGrid, IsCartesian, &
         IsRzGeometry, IsCylindrical, IsRLonLat, IsLogRadius, IsGenRadius, &
         IsRoundCube, iProc, nProc, iComm, init_mpi
    use ModAMR, ONLY: init_mod_amr, read_amr_param, fix_amr_limits, &
         AdaptGrid
    use ModFieldTrace, ONLY: init_mod_field_trace, read_field_trace_param, &
         DoMapEquatorRay
    use ModIO
    use CON_planet, ONLY: read_planet_var, check_planet_var
    use CON_star, ONLY: read_star_var
    use ModPlanetConst
    use CON_axes, ONLY: init_axes, get_axes, &
         dLongitudeHgr, dLongitudeHgrDeg, dLongitudeHgi, dLongitudeHgiDeg
    use ModUtilities, ONLY: fix_dir_name, check_dir, make_dir, DoFlush, &
         split_string, join_string, open_file, lower_case, &
         DoWriteCallSequence, StringTestSwmf => StringTest
    use CON_planet, ONLY: get_planet
    use ModTimeConvert, ONLY: time_int_to_real, time_real_to_int
    use ModReadParam
    use ModMessagePass, ONLY: DoOneCoarserLayer
    use ModFaceValue, ONLY: &
         UseTvdResChange, UseAccurateResChange, nGUsed, &
         DoLimitMomentum, LimiterBeta, TypeLimiter, read_face_value_param, &
         TypeLimiter5, UseCweno, &
         iVarSmooth_V, iVarSmoothIndex_I, &
         StringLowOrderRegion, iRegionLowOrder_I
    use ModElectricField, ONLY: UseJCrossBForce
    use ModPartSteady, ONLY: UsePartSteady, MinCheckVar, MaxCheckVar, &
         RelativeEps_V, AbsoluteEps_V
    use ModBoundaryGeometry, ONLY: init_mod_boundary_cells, &
         read_boundary_geometry_param
    use ModPointImplicit, ONLY: read_point_implicit_param, UsePointImplicit, &
         init_mod_point_impl
    use ModRestartFile, ONLY: read_restart_parameters, init_mod_restart_file, &
         DoChangeRestartVariables, nVarRestart, UseRestartWithFullB,      &
         NameRestartInDir, NameRestartOutDir, DoSpecifyRestartVarMapping, &
         nVarRestartMapping, NameVarRestartFrom_V, NameVarRestartTo_V, &
         TypeRestartOutFile
    use ModHallResist, ONLY: &
         UseHallResist, read_hall_param
    use ModParticleFieldLine, ONLY: read_particle_line_param
    use ModParticleMover, ONLY: read_charged_particle_param=>read_param, &
         normalize_particle_param=>normalize_param, &
         UseChargedParticles=>UseParticles, UseHybrid
    use ModHeatConduction, ONLY: read_heatconduction_param
    use ModHeatFluxCollisionless, ONLY: read_heatflux_param
    use ModRadDiffusion, ONLY: read_rad_diffusion_param
    use ModResistivity, ONLY: UseResistivity, &
         read_resistivity_param, init_mod_resistivity
    use ModMultiFluid, ONLY: ChargeIon_I, nIonFluid, &
         DoConserveNeutrals, DoOhNeutralBc, &
         uBcFactor_I, RhoBcFactor_I, RhoNeuWindDim, &
         UxNeuWindDim, UyNeuWindDim, UzNeuWindDim, &
         TempNeuWindDim, MassNeutralDim
    use ModMultiIon, ONLY: multi_ion_set_parameters, &
         multi_ion_init_point_impl
    use ModSolarwind, ONLY: UseSolarwindFile, read_solar_wind_param, &
         read_solar_wind_file, normalize_solar_wind_data
    use ModSatelliteFile, ONLY: nSatellite, NameFileSat_I, NameSat_I, &
         read_satellite_parameters, read_satellite_input_files
    use ModGroundMagPerturb, ONLY: read_magperturb_param, init_mod_magperturb,&
         nMagGridFile
    use ModCalcSource, ONLY: read_source_param
    use ModFaceFlux, ONLY: read_face_flux_param, init_mod_face_flux, &
         TypeFluxNeutral, UseClimit, DoBurgers
    use ModLookupTable, ONLY: read_lookup_table_param, get_lookup_table, &
         i_lookup_table, copy_lookup_table_to_gpu
    use ModIeCoupling, ONLY: read_ie_velocity_param
    use ModTimeStepControl, ONLY: read_time_step_control_param
    use ModLaserHeating, ONLY: read_laser_heating_param
    use ModLocalTimeStep, ONLY: read_localstep_param
    use ModIoUnit, ONLY: io_unit_new
    use ModNumConst, ONLY: cDegToRad, cTiny, cHalfPi
    use ModConst, ONLY: CarringtonSynodicPeriod, tStartCarringtonRotation
    use ModSort, ONLY: sort_quick
    use ModConservative, ONLY: read_conservative_param

    use ModViscosity, ONLY: UseViscosity, viscosity_read_param, viscosity_init
    use ModPIC, ONLY: pic_read_param, AdaptPic
    use ModIonElectron, ONLY: read_ion_electron_param, iVarUseCmax_I, &
         ion_electron_init_point_impl
    use ModFaceBoundary, ONLY: read_face_boundary_param, RatioPe2P
    use ModPUI, ONLY: read_pui_param, init_mod_pui
    ! BEGIN CORONA SPECIFIC
    use EEE_ModMain, ONLY: EEE_set_parameters
    use ModMagnetogram, ONLY: read_magnetogram_param
    use ModCoronalHeating, ONLY: read_coronal_heating_param, &
         init_coronal_heating, UseCoronalHeating
    use ModTurbulence, ONLY: read_turbulence_param
    use ModTurbulence, ONLY: UseAlfvenWaveDissipation
    use ModFieldLineThread, ONLY: read_thread_param
    use ModThreadedLC, ONLY: init_threaded_lc, read_threaded_bc_param
    use ModRadiativeCooling, ONLY: UseRadCooling,&
         read_cooling_param, check_cooling_param
    use ModChromosphere, ONLY: read_chromosphere_param, init_chromosphere
    use ModWaves, ONLY: read_waves_param, check_waves
    use ModLdem, ONLY: UseLdem, NameLdemFile, iRadiusLdem, read_ldem
    use ModSaMhd, ONLY: read_samhd_param
    ! END CORONA SPECIFIC
    use ModCoarseAxis, ONLY: read_coarse_axis_param
    use ModBorisCorrection, ONLY: read_boris_param, UseBorisCorrection, &
         UseBorisSimple, UseBorisRegion, init_mod_boris_correction
    use ModTimewarp, ONLY: UseTimewarp, read_timewarp_param, init_mod_timewarp
    use ModUserInterface ! user_read_inputs, user_init_session
    use ModConserveFlux, ONLY: init_mod_cons_flux, DoConserveFlux
    use ModVarIndexes, ONLY: MassSpecies_V, SpeciesFirst_, SpeciesLast_
    use ModFreq, ONLY: adjust_freq
    use BATL_lib, ONLY: Dim2_, Dim3_, &
         create_grid, set_high_geometry, get_region_indexes, &
         rRound0, rRound1, StringTest
    use ModBatsrusUtility, ONLY: get_ivar
    use ModOptimizeParam, ONLY: check_optimize_param
    use ModPlotShock, ONLY: DivuDxMin
    use ModHeatFluxCollisionless, ONLY: UseHeatFluxCollisionless, &
         UseHeatFluxRegion

    ! Arguments

    ! TypeAction determines if we read or check parameters
    character(len=*), intent(in) :: TypeAction

    ! Local variables
    integer :: iFile, i, iFluid
    logical :: IsUninitialized      = .true.

    ! local variables for the plot type 'INS'
    integer :: iFileStart, iFileRead, iFileInstrument, nPlotFileRead
    integer :: iInstrument, nInstrument, iSat
    character(LEN=10)  :: NameSat, NameInstrument, StringInstrument_I(20) = ''
    character(LEN=200) :: StringInstrument

    ! logical :: HdfUninitialized      = .true.
    logical :: DoReadSolarwindFile  = .false.
    logical :: DoReadSatelliteFiles = .false.
    logical :: IsMirrorX,  IsMirrorY,  IsMirrorZ

    ! The name of the command and an one line from the PARAM.in file
    character(len=lStringLine) :: NameCommand, StringLine
    character(len=20):: NameVarUpdate_I(nVar)

    ! Temporary variables
    logical :: DoEcho=.false.
    integer :: nVarEquationRead = 0
    character(len=lStringLine) :: NameEquationRead="?"
    logical :: IsReadNameVarRestart = .false.
    character(len=lStringLine) :: NameVarRestartRead  =''
    character(len=lStringLine) :: NameVarsRestartFrom =''
    character(len=lStringLine) :: NameVarsRestartTo   =''
    integer :: nVarRestartMappingFrom, nVarRestartMappingTo

    character(len=50) :: StringPlot,StringLog, TypeCoordObs
    character(len=3)  :: TypePlotArea, TypePlotVar
    character(len=2)  :: NameCompRead="??"
    integer :: MinBlockAll, nIJKRead_D(3), nRootRead_D(3)=1

    character(len=50) :: StringParcel
    integer           :: iParcel

    integer           :: nDepthTiming=-1
    character(len=10) :: TypeTiming='cumu'

    ! Variables for checking/reading #STARTTIME command
    real(Real8_)       :: StartTimeCheck   = -1.0_Real8_
    real               :: tSimulationCheck = -1.0

    ! Variable for #UNIFORMAXIS
    logical:: UseUniformAxis = .true.

    ! Variables for checking the user module
    character(len=lStringLine) :: NameUserModuleRead='?'

    ! Variables for user switches
    character(len=lStringLine) :: StringSwitch
    integer                    :: iSwitch, nSwitch
    character(len=25)          :: NameSwitch_I(10), NameSwitch
    logical                    :: DoSwitchOn

    ! Variables related to sessions
    logical :: IsFirstSession = .true.
    integer :: iSession, iSessionFirst = 0

    ! Indexes
    integer :: iSpecies, iPlotFile, iVar, iDim

    ! Variables for #SAVEPLOT command, to replace some common used variables
    ! in VAR string
    character(len(StringPlotVar)+2) :: StringPlotVarExt
    integer :: l1, l2

    ! Variables for #BOUNDARYSTATE command
    character(len=lStringLine) :: StringBoundary
    ! The enames of face and cell boundaries
    character(len=10) :: NameBoundary_I(zMaxBc_-SolidBc_+1+Coord3MaxBc_)
    integer :: iNameBoundary, nNameBoundary, iBoundaryState = 0
    real    :: BoundaryStateDim_V(1:nVar)

    character(len=30) :: NamePrimitiveNT_V(nVar)

    ! variables for B0 lookup table
    integer:: nParam, iTableB0 = -1
    real(Real8_):: CarringtonRotationNumber
    character(len=500):: StringHeader
    real:: Param_I(4), CRFraction

    character(len=17) :: NameSub='MH_set_parameters'
    !--------------------------------------------------------------------------
    NameSub(1:2) = NameThisComp

    iSession = i_session_read()
    ! First session when component is reading parameters
    if(iSessionFirst < 1) iSessionFirst = iSession

    ! Initialize BATL
    call init_mpi(iComm)

    if(IsUninitialized)then
       call set_namevar
       call set_user_version
       call set_defaults
       IsUninitialized=.false.
    end if

    ! restart in first session only
    if(.not.IsFirstSession) IsRestart=.false.

    select case(TypeAction)
    case('CHECK')
       if(iProc==0)write(*,*) NameSub,': CHECK iSession =',iSession

       ! Make output and check input directories
       if(iProc==0) call make_dir(NamePlotDir)
       if(iProc==0 .and. DoSaveRestart) call make_dir(NameRestartOutDir)
       if(iProc==0 .and. IsRestart) call check_dir(NameRestartInDir)

       ! Check if StartTime from PARAM.in and Carrington Map Time match
       iTableB0 = i_lookup_table('B0')
       if(iTableB0 > 0)then
          call get_lookup_table(1, StringDescription = StringHeader, &
               nParam=nParam, Param_I=Param_I, Time=CRFraction)
          if(iProc==0 .and. nParam >= 4 )then
             CarringtonRotationNumber = (StartTime &
                  - tStartCarringtonRotation)/CarringtonSynodicPeriod
             ! If the User provided StartTime is off by over half of the
             ! Carrington Rotation, it stops
             if( abs(CarringtonRotationNumber - (Param_I(4) + CRFraction) )&
                  > 0.5)then
                write(*,*)NameSub,': Carrington Rotation number from '// &
                     'PARAM.in  = ', CarringtonRotationNumber
                write(*,*)NameSub,': Carrington Rotation number from '// &
                     'input map = ',Param_I(4) + CRFraction
                write(*,*)NameSub,': WARNING! #STARTTIME in PARAM.in '// &
                     'differs from Carrington Rotation of Central Meridian '//&
                     'of Input Map !!!'
                if(UseStrict) call stop_mpi( &
                     'Fix #STARTTIME or use CORRECT Magnetogram')
             endif
          endif
       endif

       if(StartTimeCheck > 0.0 .and. tSimulationCheck > 0.0)then
          if(abs(StartTime+tSimulation - StartTimeCheck-tSimulationCheck)&
               > 0.001)then
             write(*,*)NameSub//' WARNING: '// &
                  NameThisComp//'::StartTimeCheck+tSimulationCheck=', &
                  StartTimeCheck + tSimulationCheck, &
                  ' differs from CON::StartTime+tSimulation=', &
                  StartTime + tSimulation,' !!!'
             if(UseStrict)then
                call stop_mpi('Fix #STARTTIME command in PARAM.in')
             else
                ! Fix iStartTime_I array
                call time_real_to_int(StartTime, iStartTime_I)
             end if
          end if
          StartTimeCheck   = -1.0
          tSimulationCheck = -1.0
       end if
       if(UseEndTime)then
          tSimulationMax = EndTime - StartTime
          nIter = -1
          if(IsStandAlone)then
             if(.not.IsTimeAccurate)call stop_mpi( &
                  '#ENDTIME command cannot be used in steady-state mode')
             if(.not.IsLastRead) call stop_mpi(&
                  '#ENDTIME command can be used in the last session only')
          end if
       end if

       ! Adjust frequencies
       call adjust_freq(AdaptGrid, &
            nStep+1, tSimulation+1e-6, IsTimeAccurate)
       call adjust_freq(AdaptPic, &
            nStep+1, tSimulation+1e-6, IsTimeAccurate)

       ! Initialize axes (coordinate transformation matrices)
       call init_axes(StartTime)

       if(NameThisComp == 'GM') then
          ! Set and obtain GM specific parameters from CON_planet and CON_axes
          call get_axes(tSimulation, MagAxisTiltGsmOut = ThetaTilt)
          call get_planet(DipoleStrengthOut = DipoleStrengthSi)
       end if

       if(MonopoleStrengthSi /= 0.0 .or. UseB0Wave)then
          UseB0       = .true.
          UseB0Source = UseB0Wave
          UseCurlB0   = UseB0Wave
          DoUpdateB0  = .false.
          DtUpdateB0  = -1.0
       elseif(IsStandAlone .and. NameThisComp=='GM') then
          ! Check and set some planet variables (e.g. DoUpdateB0)
          call check_planet_var(iProc==0, IsTimeAccurate)

          if(UseBody)then
             call get_planet(UseRotationOut = UseRotatingBc)
          else
             UseRotatingBc = .false.
          end if

          ! Obtain some planet parameters
          if(.not.UseB0 .or. &
               (DipoleStrengthSi == 0.0 .and. .not. UseUserB0))then
             UseB0       = .false.
             UseB0Source = .false.
             UseCurlB0   = .false.
             DoUpdateB0  = .false.
             DtUpdateB0  = -1.0
          else
             call get_planet( &
                  DoUpdateB0Out = DoUpdateB0, DtUpdateB0Out = DtUpdateB0)
          end if

       end if

       ! Set number and indexes of vector variables
       call init_vector_variables

       call correct_parameters

       ! initialize module variables
       call init_mod_main
       call init_mod_advance
       call init_mod_geometry
       call init_mod_boundary_cells
       call init_mod_nodes

       if(UseB .and. (UseBorisCorrection .or. UseBorisSimple)) &
            call init_mod_boris_correction
       if(UseB0)            call init_mod_b0
       if(DoReverseField)   call init_mod_reverse_field
       if(UseRaytrace)      call init_mod_field_trace
       if(UseConstrainB)    call init_mod_ct
       if(UseImplicit)      call init_mod_part_impl
       if(UseSemiImplicit)  call init_mod_semi_impl

       if(UsePointImplicit)then
          if(UseEfield)then
             call init_mod_point_impl(ion_electron_init_point_impl)
          elseif(UseMultiIon)then
             call init_mod_point_impl(multi_ion_init_point_impl)
          elseif(UseUserSourceImpl)then
             call init_mod_point_impl(user_init_point_implicit)
          else
             ! call init_mod_point_impl
          end if
       end if

       call init_mod_face_flux
       if(DoConserveFlux) call init_mod_cons_flux
       call init_mod_magperturb

       call get_region_indexes(StringLowOrderRegion, iRegionLowOrder_I)

       ! clean dynamic storage
       call init_mod_block_data
       call clean_block_data

       ! set physics uses dimensional solar wind data
       if (DoReadSolarwindFile) call read_solar_wind_file

       ! Read satellite file
       if(DoReadSatelliteFiles)then
          call read_satellite_input_files
          DoReadSatelliteFiles = .false.
       end if

       call set_physics_constants

       call user_action("initialize module")

       ! Normalization of solar wind data requires normalization in set_physics
       if (DoReadSolarwindFile) call normalize_solar_wind_data

       call set_extra_parameters

       ! initialize ModEqution (e.g. variable units)
       call init_mhd_variables

       if(UseResistivity)call init_mod_resistivity
       if(UseViscosity) call viscosity_init

       if(UseCoronalHeating)call init_coronal_heating
       call check_cooling_param

       call init_chromosphere

       ! Initialize threaded field line module (lower corona)
       if(UseFieldLineThreads .and. IsFirstSession)call init_threaded_lc

       ! Initialize user module and allow user to modify things
       if(UseUserInitSession)call user_init_session

       if(nPui > 1) call init_mod_pui

       if(UseTimewarp) call init_mod_timewarp

       call check_waves

       if((iProc==0 .or. UseTimingAll) .and. IsStandAlone)then
          call timing_active(UseTiming)
          if(IsFirstSession)then
             call timing_step(0)
             if(UseTimingAll)then
                iUnitTiming = io_unit_new()
                write(NameTimingFile,'(a,i6.6)') 'timing.pe', iProc
                call open_file(iUnitTiming, FILE=NameTimingFile)
                call timing_iounit(iUnitTiming)
                call timing_comp_proc("  ",iProc)
             end if
          end if
          call timing_depth(nDepthTiming)
          call timing_report_style(TypeTiming)
       end if

       if(iTypeUpdate == UpdateFast_ .and. iProc == 0) &
            call check_optimize_param

       call copy_lookup_table_to_gpu

       IsFirstSession = .false.

       RETURN
    case('read','Read','READ')
       if(iProc == 0)then
          write(*,*) NameSub,': READ iSession =',iSession,&
               ' iLine=',i_line_read(),' nLine =',n_line_read()
       end if
    case default
       call stop_mpi(NameSub//': TypeAction='//TypeAction// &
            ' must be "CHECK" or "READ"!')
    end select

    ! Read solarwindfile if #SOLARWINDFILE command is in this session
    DoReadSolarwindFile = .false.

    ! Read parameters from the text
    READPARAM: do
       if(.not.read_line(StringLine) )then
          IsLastRead = .true.
          EXIT READPARAM
       end if

       if(.not.read_command(NameCommand)) CYCLE READPARAM

       select case(NameCommand)

       case("#BATLPROLONG")
          call read_var('BetaProlong', BetaProlong )

       case("#COMPONENT")
          call read_var('NameComp', NameCompRead)
          if(NameThisComp /= NameCompRead)then
             NameThisComp = NameCompRead
             !$acc update device(NameThisComp)
             call set_defaults
          end if

       case("#DESCRIPTION")
          call check_stand_alone
          call read_var('StringDescription', StringLine)

       case("#BEGIN_COMP","#END_COMP")
          call check_stand_alone
          i = len_trim(NameCommand)
          if(StringLine(i+2:i+3) /= NameThisComp)&
               call stop_mpi(NameSub//' ERROR: the component is not '// &
               NameThisComp//'in '//trim(StringLine))

       case("#END")
          call check_stand_alone
          IslastRead=.true.
          EXIT READPARAM

       case("#RUN")
          call check_stand_alone
          IslastRead=.false.
          EXIT READPARAM

       case("#STOP")
          call check_stand_alone
          call read_var('MaxIteration', nIter)
          call read_var('tSimulationMax', tSimulationMax)

       case("#CPUTIMEMAX")
          call check_stand_alone
          call read_var('CpuTimeMax', CpuTimeMax)

       case("#CHECKSTOPFILE")
          call check_stand_alone
          call read_var('DoCheckStopfile', DoCheckStopFile)

       case("#PROGRESS")
          call check_stand_alone
          call read_var('DnProgressShort', DnProgressShort)
          call read_var('DnProgressLong', DnProgressLong)

       case("#TIMEACCURATE")
          call check_stand_alone
          call read_var('IsTimeAccurate', IsTimeAccurate)

       case("#ECHO")
          call check_stand_alone
          call read_var('DoEcho', DoEcho)
          if(iProc==0)call read_echo_set(DoEcho)

       case("#FLUSH")
          call read_var('DoFlush', DoFlush)

       case("#TESTINFO")
          call read_var('DoWriteCallSequence', DoWriteCallSequence)

       case("#VERBOSE", "#TEST", "#TESTXYZ", "#TEST2XYZ", "#TESTIJK", &
            "#TEST2IJK", "#TESTVAR", "#TESTDIM", "#TESTSIDE")
          call read_test_param(NameCommand)
          if(NameCommand == "#TESTVAR") call get_iVar(NameVarTest, iVarTest)
          if(NameCommand == "#TEST" .and. IsStandAlone) &
               StringTestSwmf = StringTest

       case("#TESTPIXEL")
          call read_var('iPixTest', iPixTest)
          call read_var('jPixTest', jPixTest)

       case("#STRICT")
          call read_var('UseStrict', UseStrict)

       case("#DEBUG")
          call read_var('DoDebug', DoDebug)
          call read_var('DoDebugGhost', DoShowGhostCells)

       case("#TIMING")
          call read_var('UseTiming', UseTiming)
          if(UseTiming)then
             call read_var('DnTiming', DnTiming)
             call read_var('nDepthTiming', nDepthTiming)
             call read_var('TypeTimingReport', TypeTiming)
             UseTimingAll = index(TypeTiming,'all') > 0
             TypeTiming  = TypeTiming(1:4)
          end if

       case("#OPTIMIZEMPI")
          call read_var('UseOptimizeMpi', UseOptimizeMpi)

       case("#OUTERBOUNDARY","#BOXBOUNDARY")
          call read_boundary_geometry_param(NameCommand)

       case("#TIMESTEPPING", "#RUNGEKUTTA", "#RK")
          call read_var('nStage',  nStage)
          call read_var('CflExpl', Cfl)
          CflOrig = Cfl
          ExplCfl = Cfl
          UseHalfStep = NameCommand == "#TIMESTEPPING" .and. nStage <= 2

       CASE('#USEFLIC')
          call read_var('UseFlic', UseFlic)

       case("#LOCALTIMESTEP", "#SUBCYCLING")
          call read_localstep_param(NameCommand, iSession)

       case("#PARTLOCALTIMESTEP")
          call read_var('rLocalTimeStep', rLocalTimeStep)

       case( "#TIMESTEPLIMIT")
          call               read_var('UseDtLimit', UseDtLimit)
          if(UseDtLimit)call read_var('DtLimitDim', DtLimitDim)

       case("#FIXEDTIMESTEP")
          call               read_var('UseDtFixed', UseDtFixed)
          if(UseDtFixed)call read_var('DtFixedDim', DtFixedDim)

       case("#PARTSTEADY")
          call read_var('UsePartSteady', UsePartSteady)

       case("#PARTSTEADYCRITERIA","#STEADYCRITERIA")
          call read_var('MinCheckVar', MinCheckVar)
          call read_var('MaxCheckVar', MaxCheckVar)
          do iVar=MinCheckVar, MaxCheckVar
             call read_var('RelativeEps', RelativeEps_V(iVar))
             call read_var('AbsoluteEps', AbsoluteEps_V(iVar))
          end do

       case("#POINTIMPLICIT")
          call read_point_implicit_param

       case("#LINEARSOLVERACCURATESUM")
          call read_var('UseAccurateSum', UseAccurateSum)

       case("#IMPLICIT", "#GRIDBLOCKIMPL", "#GRIDBLOCKIMPLALL", &
            "#IMPLCRITERIA", "#IMPLICITCRITERIA", "#STEPPINGCRITERIA", &
            "#PARTIMPL", "#PARTIMPLICIT",     &
            "#IMPLSCHEME", "#IMPLICITSCHEME", &
            "#IMPLSTEP", "#IMPLICITSTEP",     &
            "#IMPLCHECK", "#IMPLICITCHECK",   &
            "#IMPLENERGY", "#IMPLICITENERGY", &
            "#NEWTON", "#JACOBIAN", "#PRECONDITIONER", &
            "#KRYLOV", "#KRYLOVSIZE")
          call read_part_impl_param(NameCommand)

       case("#SEMIIMPL", "#SEMIIMPLICIT", &
            "#SEMICOEFF", "#SEMIIMPLCOEFF", "#SEMIIMPLICITCOEFF", &
            "#SEMIPRECOND", "#SEMIPRECONDITIONER",&
            "#SEMIKRYLOV", "#SEMIKRYLOVSIZE","#SEMIIMPLICITSTABLE")
          call read_semi_impl_param(NameCommand)

       case("#HYPRE")
          call hypre_read_param

       case("#PICGRID", '#PICGRIDROTATE', "#PICUNIT", "#PICGRIDUNIT", &
            "#PICBALANCE", "#PICADAPT", "#PICPATCH", "#PICCRITERIA", &
            "#PICPATCHEXTEND", "#PICREGIONMIN", "#PICREGIONMAX", &
            "#RESTARTPICSTATUS")
          call pic_read_param(NameCommand)

       case("#VISCOSITY", "#VISCOSITYREGION","#ARTIFICIALVISCOSITY")
          call viscosity_read_param(NameCommand)

       case("#RESISTIVITY", "#RESISTIVITYOPTIONS", &
            "#RESISTIVITYREGION", "#RESISTIVEREGION",&
            '#MESSAGEPASSRESISTIVITY', '#RESISTIVITYSCHEME')
          call read_resistivity_param(NameCommand)

       case("#HALLRESISTIVITY", "#HALLREGION", "#BIERMANNBATTERY")
          call read_hall_param(NameCommand)

       case("#MINIMUMDENSITY")
          do iFluid = 1, nFluid
             call read_var('RhoMinDim', RhoMinDim_I(iFluid))
          end do

       case("#MINIMUMPRESSURE")
          do iFluid = 1, nFluid
             call read_var('pMinDim', pMinDim_I(iFluid))
          end do
          if(UseElectronPressure) call read_var('PeMinDim', PeMinDim)

       case("#MINIMUMTEMPERATURE")
          do iFluid = 1, nFluid
             call read_var('TMinDim', TMinDim_I(iFluid))
          end do
          if(UseElectronPressure) call read_var('TeMinDim', TeMinDim)

       case("#MINIMUMRADIALSPEED")
          call    read_var('UseSpeedMin',   UseSpeedMin)
          if(UseSpeedMin)then
             call read_var('rSpeedMin',     rSpeedMin)
             call read_var('SpeedMinDim',   SpeedMinDim)
             call read_var('TauSpeedMinDim', TauSpeedMinDim)
          end if

       case("#ELECTRONPRESSURE")
          call read_var('PeMinSi', PeMinSi)

       case("#ENTROPY")
          call read_var('UseIonEntropy', UseIonEntropy)
          if(UseMultiIon)call read_var('UseTotalIonEnergy', UseTotalIonEnergy)

       case("#ELECTRONENTROPY")
          call read_var('UseElectronEntropy', UseElectronEntropy)
          call read_var('UseElectronEnergy',  UseElectronEnergy)

       case("#UPDATECHECK", "#SHOCKHEATING")
          call read_update_param(NameCommand, UseStrict)

       case("#TIMEWARP", "#WARPDIM", "#WARPCMAX", "#WARPSCHEME")
          call read_timewarp_param(NameCommand)

       case("#ANISOTROPICPRESSURE")
          do iFluid = 1, nFluid
             call read_var('UseConstantTau', UseConstantTau_I(iFluid))
             call read_var('TauInstabilitySi', TauInstabilitySi_I(iFluid))
             call read_var('TauGlobalSi', TauGlobalSi_I(iFluid))
          end do

       case("#EXTRAINTERNALENERGY")
          call read_var('ExtraEintMinSi', ExtraEintMinSi)

       case("#RADIATION", "#HEATFLUXLIMITER", "#ACCURATERADIATION")
          call read_rad_diffusion_param(NameCommand)

       case("#HEATCONDUCTION", "#WEAKFIELDCONDUCTION", "#IONHEATCONDUCTION")
          call read_heatconduction_param(NameCommand)

       case("#HEATFLUXREGION", "#HEATFLUXCOLLISIONLESS")
          call read_heatflux_param(NameCommand)

       case("#COULOMBLOG")
          call read_var('CoulombLog', CoulombLog)
          ! Synchronize with the value used in the TR model
          if(NameThisComp=='SC')CoulombLogTr = CoulombLog

       case("#PARTICLELINE","#PARTICLELINERANDOMWALK")
          call read_particle_line_param(NameCommand)

       case("#CHARGEDPARTICLES")
          call read_charged_particle_param(NameCommand)

       case("#PARCEL")
          call read_var('UseParcel', UseParcel)
          if(UseParcel)then
             call read_var('UseParcelTable', UseParcelTable)
             if(.not. UseParcelTable)then
                call read_var('nParcel', nParcel)
                nfile = max(nFile, parcel_+nParcel)
                do iParcel = 1, nParcel
                   call read_var('xParcel', Parcel_DI(1,iParcel))
                   call read_var('yParcel', Parcel_DI(2,iParcel))
                   call read_var('zParcel', Parcel_DI(3,iParcel))
                end do
             end if
             call read_var('StringParcel', StringParcel)
             call read_var('DnOutput', DnOutput_I(parcel_+1))
             DnOutput_I(parcel_+1:parcel_+nParcel)=DnOutput_I(parcel_+1)
             call read_var('DtOutput', DtOutput_I(parcel_+1))
             DtOutput_I(parcel_+1:parcel_+nParcel)=DtOutput_I(parcel_+1)
             if(DnOutput_I(parcel_+1) > 0)then
                call read_var('nStartParcel', nStartParcel)
                call read_var('nEndParcel', nEndParcel)
                if(nEndParcel < nStartParcel)then
                   write(*,*) ' StartTimeParcel =', StartTimeParcel
                   write(*,*) ' EndTimeParcel   =', EndTimeParcel
                   call stop_mpi(NameSub//' correct #PARCEL: '// &
                        'nEndParcel<nStartParcel')
                end if
             elseif(DtOutput_I(parcel_+1) > 0)then
                call read_var('StartTimeParcel', StartTimeParcel)
                call read_var('EndTimeParcel', EndTimeParcel)
                if (EndTimeParcel < StartTimeParcel .or. &
                     (EndTimeParcel-StartTimeParcel)/DtOutput_I(parcel_+1) >&
                     1e6) then
                   write(*,*) ' StartTimeParcel =', StartTimeParcel
                   write(*,*) ' EndTimeParcel   =', EndTimeParcel
                   call stop_mpi(NameSub//' correct #PARCEL: '// &
                        'EndTimeParcel<StartTimeParcel or too small DtParcel')
                endif
             else
                call stop_mpi(NameSub//' correct #PARCEL: '// &
                     'Dt or Dn must be positive')
             endif

             if(index(StringParcel,'VAR') > 0 .or. &
                  index(StringParcel,'var') > 0 )then
                TypePlotVar='var'
                IsDimensionalPlot_I(parcel_+1:parcel_+nParcel) = &
                     index(StringParcel, 'VAR') > 0
                call read_var('NameParcelVars', StringParcelVar)
             elseif(index(StringParcel, 'MHD') > 0 .or. &
                  index(StringParcel, 'mhd') > 0)then
                TypePlotVar = 'mhd'
                IsDimensionalPlot_I(parcel_+1:parcel_+nParcel) = &
                     index(StringParcel, 'MHD') > 0
                StringParcelVar = NamePrimitiveVarPlot//' jx jy jz'
             elseif(index(StringParcel, 'FUL') > 0 .or. &
                  index(StringParcel, 'ful') > 0)then
                TypePlotVar = 'ful'
                IsDimensionalPlot_I(parcel_+1:parcel_+nParcel) = &
                     index(StringParcel,'FUL') > 0
                StringParcelVar = &
                     NamePrimitiveVarPlot//' b1x b1y b1z e jx jy jz'
             else
                call stop_mpi(&
                     'Variable definition (mhd,ful,var) missing' &
                     //' from StringParcel='//StringParcel)
             endif
          endif

       case("#SAVELOGFILE")
          call read_var('DoSaveLogfile', DoSaveLogfile)
          if(DoSaveLogfile)then
             nfile=max(nfile,logfile_)
             call read_var('StringLog', StringLog)
             call read_var('DnSaveLogfile', DnOutput_I(logfile_))
             call read_var('DtSaveLogfile', DtOutput_I(logfile_))

             ! Log variables
             if(index(StringLog,'VAR')>0 .or. index(StringLog,'var')>0)then
                IsDimensionalPlot_I(logfile_) = index(StringLog,'VAR')>0
                TypeLogTime='step time'
                call read_var('StringLogVar', StringLogVar)
             elseif(index(StringLog,'RAW')>0 &
                  .or. index(StringLog,'raw')>0)then
                IsDimensionalPlot_I(logfile_) = index(StringLog,'RAW')>0
                TypeLogTime='step time'
                StringLogVar='dt '//NameConservativeVarPlot//' Pmin Pmax'
             elseif(index(StringLog,'MHD')>0 &
                  .or. index(StringLog,'mhd')>0)then
                IsDimensionalPlot_I(logfile_) = index(StringLog,'MHD')>0
                TypeLogTime='step date time'
                StringLogVar=NameConservativeVarPlot//' Pmin Pmax'
             elseif(index(StringLog,'FLX')>0 &
                  .or. index(StringLog,'flx')>0)then
                IsDimensionalPlot_I(logfile_) = index(StringLog,'FLX')>0
                TypeLogTime='step date time'
                StringLogVar='rho pmin pmax rhoflx pvecflx e2dflx'
             else
                call stop_mpi('Log variables (mhd,MHD,var,VAR,flx) missing'&
                     //' from StringLog='//StringLog)
             end if

             ! Determine the time output format to use in the logfile.
             ! This is loaded by default above, but can be input in the
             ! StringLog line.
             if(index(StringLog,'none')>0) then
                TypeLogTime = 'none'
             elseif((index(StringLog,'step')>0) .or. &
                  (index(StringLog,'date')>0) .or. &
                  (index(StringLog,'time')>0)) then
                TypeLogTime = ''
                if(index(StringLog,'step')>0) TypeLogTime = 'step'
                if(index(StringLog,'date')>0) &
                     write(TypeLogTime,'(a)') &
                     TypeLogTime(1:len_trim(TypeLogTime))//' date'
                if(index(StringLog,'time')>0) &
                     write(TypeLogTime,'(a)') &
                     TypeLogTime(1:len_trim(TypeLogTime))//' time'
             end if

             ! Recognize coordinate system names
             if (index(StringLog,'GEO') > 0) TypeCoordPlot_I(logfile_) = 'GEO'
             if (index(StringLog,'GSE') > 0) TypeCoordPlot_I(logfile_) = 'GSE'
             if (index(StringLog,'GSM') > 0) TypeCoordPlot_I(logfile_) = 'GSM'
             if (index(StringLog,'MAG') > 0) TypeCoordPlot_I(logfile_) = 'MAG'
             if (index(StringLog,'SMG') > 0) TypeCoordPlot_I(logfile_) = 'SMG'
             if (index(StringLog,'HGR') > 0) TypeCoordPlot_I(logfile_) = 'HGR'
             if (index(StringLog,'HGI') > 0) TypeCoordPlot_I(logfile_) = 'HGI'
             if (index(StringLog,'HGC') > 0) TypeCoordPlot_I(logfile_) = 'HGC'

             if (index(StringLog,'hgr') > 0) TypeCoordPlot_I(logfile_) = 'hgr'
             if (index(StringLog,'hgi') > 0) TypeCoordPlot_I(logfile_) = 'hgi'
             if (index(StringLog,'hgc') > 0) TypeCoordPlot_I(logfile_) = 'hgc'

             ! If any flux variables are used - input a list of radii
             ! at which to calculate the flux
             if (index(StringLogVar,'flx')>0) &
                  call read_var('StringLogRadius', StringLogRadius)

          end if

       case("#SAVEINITIAL")
          call read_var('DoSaveInitial', DoSaveInitial)

       case("#SAVETECPLOT")
          call read_var('DoSaveOneTecFile', DoSaveOneTecFileOrig)

       case("#SAVEONEFILE")
          call read_var('DoSaveOneFile', DoSaveOneIdlFile)

       case("#SAVEPLOT")
          call read_var('nPlotFile', nPlotFileRead)
          if(nPlotFileRead < nPlotFile)then
             ! Disable plotting files with index greater than the new nPlotFile
             DnOutput_I(plot_+nPlotFileRead+1:plot_+nPlotFile) = -1
             DtOutput_I(plot_+nPlotFileRead+1:plot_+nPlotFile) = -1.0
             nStepOutputLast_I(plot_+nPlotFileRead+1:plot_+nPlotFile) = -1
             iTimeOutputLast_I(plot_+nPlotFileRead+1:plot_+nPlotFile) = -1
          end if
          nFile = max(nFile, plot_ + nPlotFileRead)
          if (nFile > MaxFile .or. nPlotFileRead > MaxPlotFile) call stop_mpi(&
               'The number of ouput files is too large in #SAVEPLOT:'&
               //' nPlotFile > MaxPlotFile .or. nFile > MaxFile')

          nPlotFile  = nPlotFileRead
          iFileStart = Plot_

          TypeSatPos_I       = 'none'

          do iFileRead = 1, nPlotFile
             ! reset nInstrument at the beginning
             nInstrument = 0

             iFile = iFileStart + iFileRead

             call read_var('StringPlot', StringPlot)

             ! Plotting frequency
             call read_var('DnSavePlot', DnOutput_I(iFile))
             call read_var('DtSavePlot', DtOutput_I(iFile))

             ! Default resolution (original AMR grid)
             PlotDx_DI(:,iFile) = -1.0

             ! Plotting area
             if(index(StringPlot,'cut')>0)then
                TypePlotArea='cut'
                call read_var('xMinCut', PlotRange_EI(1,iFile))
                call read_var('xMaxCut', PlotRange_EI(2,iFile))
                call read_var('yMinCut', PlotRange_EI(3,iFile))
                call read_var('yMaxCut', PlotRange_EI(4,iFile))
                call read_var('zMinCut', PlotRange_EI(5,iFile))
                call read_var('zMaxCut', PlotRange_EI(6,iFile))
             elseif(index(StringPlot,'bx0')>0)then
                TypePlotArea='bx0'
                call read_var('xMinCut', PlotRange_EI(1,iFile))
                call read_var('xMaxCut', PlotRange_EI(2,iFile))
                call read_var('yMinCut', PlotRange_EI(3,iFile))
                call read_var('yMaxCut', PlotRange_EI(4,iFile))
                call read_var('zMinCut', PlotRange_EI(5,iFile))
                call read_var('zMaxCut', PlotRange_EI(6,iFile))
             elseif(index(StringPlot,'slc')>0)then
                TypePlotArea='slc'
                call read_var('xMinCut', PlotRange_EI(1,iFile))
                call read_var('xMaxCut', PlotRange_EI(2,iFile))
                call read_var('yMinCut', PlotRange_EI(3,iFile))
                call read_var('yMaxCut', PlotRange_EI(4,iFile))
                call read_var('zMinCut', PlotRange_EI(5,iFile))
                call read_var('zMaxCut', PlotRange_EI(6,iFile))
                call read_var('xPoint', PlotPointXyz_DI(1,iFile))
                call read_var('yPoint', PlotPointXyz_DI(2,iFile))
                call read_var('zPoint', PlotPointXyz_DI(3,iFile))
                call read_var('xNormal', PlotNormal_DI(1,iFile))
                call read_var('yNormal', PlotNormal_DI(2,iFile))
                call read_var('zNormal', PlotNormal_DI(3,iFile))
             elseif(index(StringPlot,'dpl')>0)then
                TypePlotArea='dpl'
                call read_var('xMinCut', PlotRange_EI(1,iFile))
                call read_var('xMaxCut', PlotRange_EI(2,iFile))
                call read_var('yMinCut', PlotRange_EI(3,iFile))
                call read_var('yMaxCut', PlotRange_EI(4,iFile))
                call read_var('zMinCut', PlotRange_EI(5,iFile))
                call read_var('zMaxCut', PlotRange_EI(6,iFile))
             elseif(index(StringPlot,'blk')>0) then
                TypePlotArea='blk'
                call read_var('xPoint', PlotPointXyz_DI(1,iFile))
                call read_var('yPoint', PlotPointXyz_DI(2,iFile))
                call read_var('zPoint', PlotPointXyz_DI(3,iFile))
             elseif(index(StringPlot,'pnt')>0) then
                TypePlotArea='pnt'
             elseif(index(StringPlot,'lin')>0)then
                iPlotFile = iFile - Plot_
                TypePlotArea='lin'
                call read_var('NameLine',  NameLine_I(iPlotFile), &
                     IsUpperCase=.true.)
                call read_var('IsSingleLine', IsSingleLine_I(iPlotFile))
                call read_var('nLine', nLine_I(iPlotFile))
                if(nLine_I(iPlotFile)==1)IsSingleLine_I(iPlotFile)=.true.
                if(nLine_I(iPlotFile) > MaxLine)then
                   if(iProc==0)then
                      write(*,*)NameSub, &
                           ' WARNING: nLine=', nLine_I(iPlotFile),&
                           ' exceeds MaxLine=', MaxLine
                      write(*,*)NameSub,' WARNING reducing nLine to MaxLine'
                   end if
                   nLine_I(iPlotFile) = MaxLine
                end if
                do i = 1, nLine_I(iPlotFile)
                   call read_var('xStartLine', XyzStartLine_DII(1,i,iPlotFile))
                   call read_var('yStartLine', XyzStartLine_DII(2,i,iPlotFile))
                   call read_var('zStartLine', XyzStartLine_DII(3,i,iPlotFile))
                   call read_var('IsParallel', IsParallelLine_II(i,iPlotFile))
                end do
             elseif(index(StringPlot,'eqr')>0)then
                TypePlotArea='eqr'
                call read_var('nRadius',   PlotRange_EI(1,iFile))
                call read_var('nLon',      PlotRange_EI(2,iFile))
                call read_var('RadiusMin', PlotRange_EI(3,iFile))
                call read_var('RadiusMax', PlotRange_EI(4,iFile))
                PlotRange_EI(5,iFile) =   0.0
                PlotRange_EI(6,iFile) = 360.0
             elseif(index(StringPlot,'eqb')>0)then
                TypePlotArea='eqb'
                call read_var('nRadius',   PlotRange_EI(1,iFile))
                call read_var('nLon',      PlotRange_EI(2,iFile))
                call read_var('RadiusMin', PlotRange_EI(3,iFile))
                call read_var('RadiusMax', PlotRange_EI(4,iFile))
                call read_var('LongitudeMin', PlotRange_EI(5,iFile))
                call read_var('LongitudeMax', PlotRange_EI(6,iFile))
             elseif(index(StringPlot,'ieb')>0)then
                TypePlotArea='ieb'
             elseif(index(StringPlot,'lcb')>0)then
                TypePlotArea='lcb'
                call read_var('Radius', PlotRange_EI(1,iFile))
                call read_var('nLon',   PlotRange_EI(2,iFile))
             elseif(index(StringPlot,'sph')>0)then
                TypePlotArea='sph'
                call read_var('Radius', PlotRange_EI(1,iFile))
             elseif(   index(StringPlot, 'shl')>0 &
                  .or. index(StringPlot, 'sln')>0 &
                  .or. index(StringPlot, 'slg')>0 &
                  .or. index(StringPlot, 'shk')>0)then
                if(index(StringPlot, 'shl')>0) then
                   TypePlotArea = 'shl'
                elseif(index(StringPlot, 'sln')>0) then
                   TypePlotArea = 'sln'
                elseif(index(StringPlot, 'slg')>0) then
                   TypePlotArea = 'slg'
                elseif(index(StringPlot, 'shk')>0) then
                   TypePlotArea = 'shk'
                endif
                if(TypePlotArea == 'shk') then
                   call read_var('DivuDxMin', DivuDxMin)
                   !$acc update device(DivuDxMin)
                else
                   call read_var('TypeCoord', TypeCoordPlot_I(iFile))
                end if
                call read_var('rMin',   PlotRange_EI(1,iFile))
                call read_var('rMax',   PlotRange_EI(2,iFile))
                if (PlotRange_EI(1, iFile) /= PlotRange_EI(2,iFile) &
                     .and. TypePlotArea /= 'shk') &
                     call read_var('dR',   PlotDx_DI(1,iFile))
                call read_var('LonMin', PlotRange_EI(3,iFile))
                call read_var('LonMax', PlotRange_EI(4,iFile))
                if (PlotRange_EI(3, iFile) /= PlotRange_EI(4,iFile)) &
                     call read_var('dLon', PlotDx_DI(2,iFile))
                call read_var('LatMin', PlotRange_EI(5,iFile))
                call read_var('LatMax', PlotRange_EI(6,iFile))
                if (PlotRange_EI(5, iFile) /= PlotRange_EI(6,iFile)) &
                     call read_var('dLat', PlotDx_DI(3,iFile))
             elseif(index(StringPlot, 'box')>0)then
                TypePlotArea = 'box'
                call read_var('TypeCoord', TypeCoordObs)
                TypeCoordPlot_I(iFile) = TypeCoordObs(1:3)
                IsObsBox_I(iFile) = index(TypeCoordObs,'OBS')>0
                call read_var('x0',   PlotRange_EI(1,iFile))
                call read_var('y0',   PlotRange_EI(2,iFile))
                call read_var('z0',   PlotRange_EI(3,iFile))
                call read_var('xLen',   PlotRange_EI(4,iFile))
                if (PlotRange_EI(4, iFile) /= 0) &
                     call read_var('dX',   PlotDx_DI(1,iFile))
                call read_var('yLen', PlotRange_EI(5,iFile))
                if (PlotRange_EI(5, iFile) /= 0) &
                     call read_var('dY', PlotDx_DI(2,iFile))
                call read_var('zLen', PlotRange_EI(6,iFile))
                if (PlotRange_EI(6, iFile) /= 0) &
                     call read_var('dZ', PlotDx_DI(3,iFile))
                if(IsObsBox_I(iFile)) then
                   call read_var('TiltAngle', PlotNormal_DI(1,iFile))
                   call read_var('ObsPosX_HGI', ObsPos_DI(1,iFile))
                   call read_var('ObsPosY_HGI', ObsPos_DI(2,iFile))
                   call read_var('ObsPosZ_HGI', ObsPos_DI(3,iFile))
                else
                   call read_var('xAngle', PlotNormal_DI(1,iFile))
                   call read_var('yAngle', PlotNormal_DI(2,iFile))
                   call read_var('zAngle', PlotNormal_DI(3,iFile))
                end if

             elseif(index(StringPlot,'los')>0)then
                TypePlotArea='los'
                ! Line of sight vector
                if(index(StringPlot, 'dem') > 0 .or. &
                     index(StringPlot, 'fux') > 0 .or. &
                     index(StringPlot, 'phx') > 0 .or. &
                     index(StringPlot, 'nbi') > 0)then
                   call read_var('TypeCoord', TypeCoordPlot_I(iFile))
                   call read_var('ObsPosX',ObsPos_DI(1,iFile))
                   call read_var('ObsPosY',ObsPos_DI(2,iFile))
                   call read_var('ObsPosZ',ObsPos_DI(3,iFile))
                   call read_var('x0',   xOffset_I(iFile))
                   call read_var('y0',   yOffset_I(iFile))
                   call read_var('xLen',   PlotRange_EI(1,iFile))
                   call read_var('dX',   PlotDx_DI(1,iFile))
                   call read_var('yLen', PlotRange_EI(2,iFile))
                   call read_var('dY', PlotDx_DI(2,iFile))
                   call read_var('TempMin', TempMin_I(iFile))
                   if (index(StringPlot, 'dem')>0)then
                      ! DEM/EM calculation
                      call read_var('LogTeMinDEM',LogTeMinDEM_I(iFile))
                      call read_var('LogTeMaxDEM',LogTeMaxDEM_I(iFile))
                      call read_var('DLogTeDEM',DLogTeDEM_I(iFile))
                   elseif(index(StringPlot, 'fux')>0)then
                      ! Spectra
                      call read_var('NameSpmTable',NameSpmTable_I(iFile))
                      call read_var('UseUnobserved',UseUnobserved_I(iFile))
                      call read_var('LambdaMin', LambdaMin_I(iFile))
                      call read_var('LambdaMax', LambdaMax_I(iFile))
                      call read_var('DLambda',   DLambda_I(iFile))
                      call read_var('UseAlfven',UseAlfven_I(iFile))
                      call read_var('UseDoppler',UseDoppler_I(iFile))
                      call read_var('DLambdaIns',DLambdaIns_I(iFile))
                      call read_var('UseIonFrac',UseIonFrac_I(iFile))
                      call read_var('UseIonTemp',UseIonTemp_I(iFile))
                      ! Individual line with photoexcitation
                   elseif(index(StringPlot, 'phx')>0)then
                      call read_var('NamePhxTable',NamePhxTable_I(iFile))
                      call read_var('LambdaMin', LambdaMin_I(iFile))
                      call read_var('LambdaMax', LambdaMax_I(iFile))
                      call read_var('DLambda',   DLambda_I(iFile))
                      call read_var('UseAlfven',UseAlfven_I(iFile))
                      call read_var('UseDoppler',UseDoppler_I(iFile))
                      call read_var('DLambdaIns',DLambdaIns_I(iFile))
                      call read_var('UseIonFrac',UseIonFrac_I(iFile))
                      call read_var('UseIonTemp',UseIonTemp_I(iFile))
                   elseif(index(StringPlot, 'nbi')>0)then
                      ! Narrowband image
                      call read_var('NameSpmTable',NameSpmTable_I(iFile))
                      call read_var('UseIonFrac',UseIonFrac_I(iFile))
                      call read_var('NameNbiTable',NameNbiTable_I(iFile))
                   end if
                elseif(index(StringPlot,'ins') == 0 .and. &
                     index(StringPlot,'INS') == 0)then
                   ! original code witout 'ins' or 'INS'
                   ! Satellite position
                   if(NameThisComp == 'GM')then
                      call read_var('ObsPosX', ObsPos_DI(1,iFile))
                      call read_var('ObsPosY', ObsPos_DI(2,iFile))
                      call read_var('ObsPosZ', ObsPos_DI(3,iFile))
                   else
                      ! Coordinates of the observation point are in HGI
                      ! system
                      call read_var('ObsPosX_HGI', ObsPos_DI(1,iFile))
                      call read_var('ObsPosY_HGI', ObsPos_DI(2,iFile))
                      call read_var('ObsPosZ_HGI', ObsPos_DI(3,iFile))
                   end if
                   ! Offset angle
                   call read_var('OffsetAngle', OffsetAngle_I(iFile))
                   OffsetAngle_I(iFile) = OffsetAngle_I(iFile)*cDegToRad
                   ! read max dimensions of the 2d image plane
                   call read_var('rSizeImage', rSizeImage_I(iFile))
                   ! read the position of image origin relative to grid origin
                   call read_var('xOffset', xOffset_I(iFile))
                   call read_var('yOffset', yOffset_I(iFile))
                   ! read the occulting radius
                   call read_var('rOccult', rOccult_I(iFile))
                   ! read the limb darkening parameter
                   call read_var('MuLimbDarkening', MuLimbDarkening)
                   ! read the number of pixels
                   call read_var('nPix', nPixel_I(iFile))
                   ! if it is an EUV plot using a long table then read in the
                   ! name of the specific lookup table (will be matched to the
                   ! name read in by the lookuptable command).
                   if (index(StringPlot,'TBL')>0&
                        .or.index(StringPlot,'tbl')>0) &
                        call read_var('NameLosTable_I', NameLosTable_I(iFile))
                else
                   ! if 'ins' or 'INS' exists
                   call read_var('StringsInstrument',  StringInstrument, &
                        IsLowerCase=.true.)

                   call split_string(StringInstrument, StringInstrument_I,   &
                        nInstrument)

                   ! each instrument adds another plot file
                   nPlotFile = nPlotFile + nInstrument - 1

                   ! adjust nFile
                   nFile = max(nFile, plot_ + nPlotFile)
                   if (nFile > MaxFile .or. nPlotFile > MaxPlotFile)          &
                        call stop_mpi(                                        &
                        'The number of ouput files is too large in #SAVEPLOT:'&
                        //' nPlotFile > MaxPlotFile .or. nFile > MaxFile')

                   ! setting each instrument info
                   do iInstrument = 1, nInstrument

                      iFileInstrument = iFile + iInstrument - 1

                      ! obtain the name of the satellite and instrument
                      i = index(StringInstrument_I(iInstrument), ':')
                      NameSat = StringInstrument_I(iInstrument)(1:i-1)
                      NameInstrument = StringInstrument_I(iInstrument)(i+1:)

                      ! plotting frequency is the same as iFile
                      DnOutput_I(iFileInstrument) = DnOutput_I(iFile)
                      DtOutput_I(iFileInstrument) = DtOutput_I(iFile)
                      TypePlot_I(iFileInstrument) = TypePlotArea//'_'// &
                           trim(NameSat)//'_'//trim(NameInstrument)
                      PlotDx_DI(:,iFileInstrument) = -1

                      ! default values, which may be overwritten below
                      ObsPos_DI(:,iFileInstrument)    = [200.0, 0.0, 0.0]
                      OffsetAngle_I(iFileInstrument)  = 0.
                      rSizeImage_I(iFileInstrument)   = 1.98
                      xOffset_I(iFileInstrument)      = 0.
                      yOffset_I(iFileInstrument)      = 0.
                      rOccult_I(iFileInstrument)      = 0.
                      MuLimbDarkening                 = 0.
                      nPixel_I(iFileInstrument)       = 512
                      NameLosTable_I(iFileInstrument) = ''

                      ! setting plot variables that may be overwritten below
                      IsDimensionalPlot_I(iFileInstrument) = &
                           index(StringPlot,'INS')>0
                      StringPlotVar_I(iFileInstrument) = 'wl pb'
                      StringPlotParam_I(iFileInstrument) = 'obsx obsy obsz'

                      select case(trim(NameSat))
                      case('sta')
                         TypeSatPos_I(iFileInstrument) = 'sta'

                         select case(trim(NameInstrument))
                         case('euvi')
                            NameLosTable_I(iFileInstrument)  = 'EuviA'
                            StringPlotVar_I(iFileInstrument) = 'tbl'
                         case('cor1')
                            rSizeImage_I(iFileInstrument) = 4.0
                            rOccult_I(iFileInstrument)    = 1.3
                            MuLimbDarkening               = 0.5
                            nPixel_I(iFileInstrument)     = 512
                         case('cor2')
                            rSizeImage_I(iFileInstrument) = 15.0
                            rOccult_I(iFileInstrument)    = 2.0
                            MuLimbDarkening               = 0.5
                            nPixel_I(iFileInstrument)     = 512
                         case default
                            call stop_mpi(NameSub//': unknown INS: '// &
                                 StringInstrument_I(iInstrument))
                         end select
                      case('stb')
                         TypeSatPos_I(iFileInstrument) = 'stb'

                         select case(trim(NameInstrument))
                         case('euvi')
                            NameLosTable_I(iFileInstrument)  = 'EuviB'
                            StringPlotVar_I(iFileInstrument) = 'tbl'
                         case('cor1')
                            rSizeImage_I(iFileInstrument)    = 4.0
                            rOccult_I(iFileInstrument)       = 1.3
                            MuLimbDarkening                  = 0.5
                            nPixel_I(iFileInstrument)        = 512
                         case('cor2')
                            rSizeImage_I(iFileInstrument)    = 15.0
                            rOccult_I(iFileInstrument)       = 2.0
                            MuLimbDarkening                  = 0.5
                            nPixel_I(iFileInstrument)        = 512
                         case default
                            call stop_mpi(NameSub//': unknown INS: '// &
                                 StringInstrument_I(iInstrument))
                         end select
                      case('sdo')
                         TypeSatPos_I(iFileInstrument) = 'earth'

                         select case(trim(NameInstrument))
                         case('aia')
                            NameLosTable_I(iFileInstrument)  = 'AiaXrt'
                            StringPlotVar_I(iFileInstrument) = 'tbl'
                         case default
                            call stop_mpi(NameSub//': unknown INS: '// &
                                 StringInstrument_I(iInstrument))
                         end select
                      case('hinode')
                         TypeSatPos_I(iFileInstrument) = 'earth'

                         select case(trim(NameInstrument))
                         case('xrt')
                            NameLosTable_I(iFileInstrument)  = 'AiaXrt'
                            StringPlotVar_I(iFileInstrument) = 'tbl'
                         case default
                            call stop_mpi(NameSub//': unknown INS: '// &
                                 StringInstrument_I(iInstrument))
                         end select
                      case('soho')
                         TypeSatPos_I(iFileInstrument) = 'earth'

                         nPixel_I(iFileInstrument)         = 300
                         MuLimbDarkening                   = 0.5

                         select case(trim(NameInstrument))
                         case('c1')
                            rSizeImage_I(iFileInstrument)  = 6.0
                            rOccult_I(iFileInstrument)     = 1.0
                            nPixel_I(iFileInstrument)      = 2048
                         case('c2')
                            rSizeImage_I(iFileInstrument)  = 6.0
                            rOccult_I(iFileInstrument)     = 2.0
                         case('c3')
                            rSizeImage_I(iFileInstrument)  = 32.0
                            rOccult_I(iFileInstrument)     = 2.5
                         case default
                            call stop_mpi(NameSub//': unknown INS: '//  &
                                 StringInstrument_I(iInstrument))
                         end select
                      case default
                         call stop_mpi(NameSub//': unknown satellite: '// &
                              StringInstrument_I(iInstrument))
                      end select

                      ! setting plot file format
                      if(index(StringPlot,'idl') > 0)then
                         TypePlotFormat_I(iFileInstrument)='idl'

                         TypeFile_I(iFileInstrument) = 'real4'
                         if(index(StringPlot,'idl_real8') > 0)      &
                              TypeFile_I(iFileInstrument) = 'real8'
                         if(index(StringPlot,'idl_ascii') > 0)      &
                              TypeFile_I(iFileInstrument) = 'ascii'
                         if(index(StringPlot,'idl_tec') > 0)        &
                              TypeFile_I(iFileInstrument) = 'tec'
                      elseif(index(StringPlot,'tec')>0) then
                         TypePlotFormat_I(iFileInstrument)  = 'tec'
                         TypeFile_I(iFileInstrument) = 'tec'
                      else
                         call stop_mpi(NameSub//' for ins/INS type, only '// &
                              'idl or tec is supported.'//StringPlot)
                      end if
                   end do

                   ! reset
                   StringInstrument_I = ''

                   ! adjust iFileStart
                   iFileStart = iFileStart + nInstrument - 1
                endif
             elseif(index(StringPlot,'rfr') > 0) then
                ! Refractive radiowave image
                TypePlotArea='rfr'
                ! Observer position
                call read_var('ObsPosX', ObsPos_DI(1,iFile))
                call read_var('ObsPosY', ObsPos_DI(2,iFile))
                call read_var('ObsPosZ', ObsPos_DI(3,iFile))
                ! read number of radiowave frequencies, i.e. # of plots
                ! call read_var('nRadioFrequency', nRadioFrequency)
                call read_var('StringRadioFrequency', &
                     StringRadioFrequency_I(iFile))
                call read_var('xSizeImage', xSizeImage_I(iFile))
                call read_var('ySizeImage', ySizeImage_I(iFile))
                ! read the number of pixels
                call read_var('nPixX', nPixelX_I(iFile))
                call read_var('nPixY', nPixelY_I(iFile))
             elseif(index(StringPlot,'buf') > 0)then
                TypePlotArea = 'buf'
             elseif(index(StringPlot,'1d') > 0)then
                TypePlotArea = '1d_'
             elseif(index(StringPlot,'2d') > 0)then
                TypePlotArea = '2d_'
             elseif(index(StringPlot,'3d') > 0)then
                TypePlotArea = '3d_'
             elseif(index(StringPlot,'3D') > 0)then
                TypePlotArea = '3D_'
             elseif(index(StringPlot,'x=0') > 0)then
                TypePlotArea = 'x=0'
             elseif(index(StringPlot,'y=0') > 0)then
                TypePlotArea = 'y=0'
             elseif(index(StringPlot,'z=0') > 0)then
                TypePlotArea = 'z=0'
             else
                call stop_mpi('Area (1d,2d,3d,x=0,y=0,z=0,cut,sph...) missing'&
                     //' from StringPlot='//StringPlot)
             end if

             ! Plot file format
             if(index(StringPlot,'idl') > 0)then
                TypePlotFormat_I(iFile)='idl'
                if (       TypePlotArea /= 'sph' &
                     .and. TypePlotArea /= 'shl' &
                     .and. TypePlotArea /= 'sln' &
                     .and. TypePlotArea /= 'slg' &
                     .and. TypePlotArea /= 'shk' &
                     .and. TypePlotArea /= 'box' &
                     .and. TypePlotArea /= 'los' &
                     .and. TypePlotArea /= 'rfr' &
                     .and. TypePlotArea /= 'lin' &
                     .and. TypePlotArea /= 'eqr' &
                     .and. TypePlotArea /= 'eqb' &
                     .and. TypePlotArea /= 'buf' &
                     ) call read_var('DxSavePlot', PlotDx_DI(1,iFile))

                ! Extract the type of idl plot file: default is real4
                TypeFile_I(iFile) = 'real4'
                if(index(StringPlot,'idl_real8') > 0) &
                     TypeFile_I(iFile) = 'real8'
                if(index(StringPlot,'idl_ascii') > 0) &
                     TypeFile_I(iFile) = 'ascii'
                if(index(StringPlot,'idl_tec') > 0) &
                     TypeFile_I(iFile) = 'tec'
             elseif(index(StringPlot, 'hdf') > 0) then
                ! With these values VisIt recognises the files as timesteps
                ! with the general defaults it does not.
                IsPlotNameN = .true.
                IsPlotNameT = .false.
                IsPlotNameE = .false.
                TypePlotFormat_I(iFile)='hdf'
                TypeFile_I(iFile) = 'hdf5'
             elseif(index(StringPlot,'tec')>0)then
                TypePlotFormat_I(iFile)  = 'tec'
                TypeFile_I(iFile) = 'tec'
             elseif(index(StringPlot,'tcp')>0)then
                if(nDim == 1)then
                   TypePlotFormat_I(iFile)  = 'idl'
                   TypeFile_I(iFile) = 'tec'
                   PlotDx_DI(1,iFile)  = -1.0
                else
                   TypePlotFormat_I(iFile)  = 'tcp'
                   TypeFile_I(iFile) = 'tcp'
                end if
             else
                call stop_mpi('Format (idl,tec) missing from StringPlot='&
                     //StringPlot)
             end if

             if(index(StringPlot,'los') == 0)then
                if(index(StringPlot, 'fux')>0)then
                   call read_var('NameSpmTable',NameSpmTable_I(iFile))
                   call read_var('UseUnobserved',UseUnobserved_I(iFile))
                   call read_var('Lambda',   LambdaMin_I(iFile))
                   call read_var('UseIonFrac',UseIonFrac_I(iFile))
                   LambdaMax_I(iFile) = LambdaMin_I(iFile)
                elseif(index(StringPlot, 'phx')>0)then
                   call read_var('NamePhxTable',NamePhxTable_I(iFile))
                   call read_var('Lambda',   LambdaMin_I(iFile))
                   call read_var('UseIonFrac',UseIonFrac_I(iFile))
                   LambdaMax_I(iFile) = LambdaMin_I(iFile)
                elseif(index(StringPlot, 'nbi')>0)then
                   call read_var('NameSpmTable',NameSpmTable_I(iFile))
                   call read_var('UseIonFrac',UseIonFrac_I(iFile))
                   call read_var('NameNbiTable',NameNbiTable_I(iFile))
                end if
             end if
             ! Plot variables
             if(       index(StringPlot,'VAR') > 0 &
                  .or. index(StringPlot,'var') > 0)then
                TypePlotVar='var'
                IsDimensionalPlot_I(iFile) = index(StringPlot,'VAR')>0
                call read_var('NameVars', StringPlotVarExt)
                call read_var('NamePars', StringPlotParam_I(iFile))
                l1 = index(StringPlotVarExt, '{')
                if (l1 > 0) then
                   l2 = index(StringPlotVarExt, '}')
                   if (l2 == 0) call stop_mpi(NameSub// &
                        ': error in #SAVEPLOT missing } in StringPlotVar='//&
                        StringPlotVarExt)

                   select case(StringPlotVarExt(l1+1:l2-1))
                   case('MHD', 'mhd')
                      StringPlotVarExt = &
                           StringPlotVarExt(:l1-1)//NamePrimitiveVarPlot//&
                           ' jx jy jz ' //trim(StringPlotVarExt(l2+1:))
                   case('HD', 'hd')
                      StringPlotVarExt = &
                           StringPlotVarExt(:l1-1)//NamePrimitiveVarPlot//&
                           trim(StringPlotVarExt(l2+1:))
                   case default
                      call stop_mpi(NameSub// &
                           ': unknown {name} ='//StringPlotVarExt(l1:l2))
                   end select
                   if(len_trim(StringPlotVarExt) > len(StringPlotVar)) &
                        call stop_mpi(NameSub// &
                        ': too long expanded variable list='//StringPlotVarExt)
                end if
                StringPlotVar_I(iFile) = StringPlotVarExt

             elseif(   index(StringPlot,'RAY') > 0 &
                  .or. index(StringPlot,'ray') > 0)then
                TypePlotVar = 'ray'
                IsDimensionalPlot_I(iFile) = index(StringPlot,'RAY')>0
                if(DoMapEquatorRay)then
                   StringPlotVar_I(iFile) = &
                        'bx by bz req1 phi1 req2 phi2 status blk'
                else
                   StringPlotVar_I(iFile) = &
                        'bx by bz lon1 lat1 lon2 lat2 status blk'
                end if
                StringPlotParam_I(iFile)='rbody'
             elseif(   index(StringPlot,'RAW') > 0 &
                  .or. index(StringPlot,'raw') > 0)then
                TypePlotVar='raw'
                IsDimensionalPlot_I(iFile)=index(StringPlot,'RAW')>0
                StringPlotVar_I(iFile) = NameConservativeVarPlot//  &
                     ' p b1x b1y b1z absdivB'
                StringPlotParam_I(iFile) = '{default}'
             elseif(   index(StringPlot,'MHD') > 0 &
                  .or. index(StringPlot,'mhd') > 0)then
                TypePlotVar='mhd'
                IsDimensionalPlot_I(iFile) = index(StringPlot,'MHD')>0
                StringPlotVar_I(iFile) = NamePrimitiveVarPlot//' jx jy jz'
                StringPlotParam_I(iFile) = '{default}'
             elseif(   index(StringPlot,'HD') > 0 &
                  .or. index(StringPlot,'hd') > 0)then
                TypePlotVar='hd'
                IsDimensionalPlot_I(iFile) = index(StringPlot,'HD')>0
                StringPlotVar_I(iFile) = NamePrimitiveVarPlot
                StringPlotParam_I(iFile) = '{default}'
             elseif(   index(StringPlot,'ALL') > 0 &
                  .or. index(StringPlot,'all') > 0)then
                ! This is intended for restart with a different dimensionality
                TypePlotVar='all'
                IsDimensionalPlot_I(iFile) = index(StringPlot,'ALL')>0
                call join_string(nVar, NameVar_V(1:nVar), &
                     StringPlotVar_I(iFile))
                StringPlotParam_I(iFile)='g'
             elseif(   index(StringPlot,'FUL') > 0 &
                  .or. index(StringPlot,'ful') > 0)then
                TypePlotVar='ful'
                IsDimensionalPlot_I(iFile) = index(StringPlot,'FUL')>0
                StringPlotVar_I(iFile) = &
                     NamePrimitiveVarPlot//' b1x b1y b1z e jx jy jz'
                StringPlotParam_I(iFile) = '{default}'
             elseif(   index(StringPlot,'FLX') > 0 &
                  .or. index(StringPlot,'flx') > 0)then
                TypePlotVar='flx'
                IsDimensionalPlot_I(iFile) = index(StringPlot,'FLX')>0
                StringPlotVar_I(iFile) = 'rho mr br p jr pvecr'
                StringPlotParam_I(iFile) = '{default}'
             elseif(   index(StringPlot,'SOL') > 0 &
                  .or. index(StringPlot,'sol') > 0)then
                TypePlotVar='sol'
                IsDimensionalPlot_I(iFile) = index(StringPlot,'SOL')>0
                StringPlotVar_I(iFile)='wl pb' ! white light
                StringPlotParam_I(iFile)='mu'
             elseif(   index(StringPlot,'EUV') > 0 &
                  .or. index(StringPlot,'euv') > 0)then
                TypePlotVar='euv'
                IsDimensionalPlot_I(iFile) = index(StringPlot,'EUV')>0
                StringPlotVar_I(iFile)='euv171 euv195 euv284' ! main euv bands
                StringPlotParam_I(iFile)='mu'
             elseif(   index(StringPlot,'SXR') > 0 &
                  .or. index(StringPlot,'sxr') > 0)then
                TypePlotVar='sxr'
                IsDimensionalPlot_I(iFile) = index(StringPlot,'SXR')>0
                StringPlotVar_I(iFile)='sxr' ! soft x-ray band
                StringPlotParam_I(iFile)='mu'
             elseif(   index(StringPlot,'TBL') > 0 &
                  .or. index(StringPlot,'tbl') > 0)then
                TypePlotVar='tbl'
                IsDimensionalPlot_I(iFile) = index(StringPlot,'TBL')>0
                ! will read a table in write_plot_los
                StringPlotVar_I(iFile)='tbl'
                StringPlotParam_I(iFile)='mu'
             elseif(   index(StringPlot,'RWI') > 0 &
                  .or. index(StringPlot,'rwi') > 0)then
                TypePlotVar='rwi'
                IsDimensionalPlot_I(iFile) = .false.
                StringPlotVar_I(iFile)='' ! Intensity
                StringPlotParam_I(iFile)=''
             elseif(   index(StringPlot,'pos') > 0 &
                  .or. index(StringPlot,'POS') > 0)then
                TypePlotVar='pos'
                IsDimensionalPlot_I(iFile) = index(StringPlot,'POS')>0
                if(TypePlotArea /= 'lin')call stop_mpi(&
                     'Variable "pos" can only be used with area "lin" !')
             elseif(index(StringPlot,'eqr')>0)then
                TypePlotVar ='eqr'
                IsDimensionalPlot_I(iFile) = .true.
             elseif(index(StringPlot,'eqb') > 0)then
                TypePlotVar ='eqb'
                IsDimensionalPlot_I(iFile) = .true.
             elseif(   index(StringPlot,'NUL') > 0 &
                  .or. index(StringPlot,'nul') > 0)then
                TypePlotVar ='nul'
                IsDimensionalPlot_I(iFile) = .true.
                StringPlotVar_I(iFile)=''
                StringPlotParam_I(iFile)=''
             elseif(   index(StringPlot,'INT') > 0 &
                  .or. index(StringPlot,'int') > 0)then
                TypePlotVar ='int'
                IsDimensionalPlot_I(iFile) = index(StringPlot,'INT')>0
                StringPlotVar_I(iFile)=''
                StringPlotParam_I(iFile)=''
             elseif(   index(StringPlot,'BBK') > 0 &
                  .or. index(StringPlot,'bbk') > 0)then
                TypePlotVar='blk'
                IsDimensionalPlot_I(iFile) = index(StringPlot,'BBK')>0
                StringPlotVar_I(iFile)='dx pe blk blkall'
             elseif(   index(StringPlot,'INS') > 0 &
                  .or. index(StringPlot,'ins') > 0)then
                ! do nothing
             elseif(   index(StringPlot,'DEM') > 0 &
                  .or. index(StringPlot,'dem') > 0)then
                TypePlotVar='dem'
                IsDimensionalPlot_I(iFile) = .true.
                StringPlotVar_I(iFile)='dem em'
                StringPlotParam_I(iFile)='rbody'
             elseif(   index(StringPlot,'FUX') > 0 &
                  .or. index(StringPlot,'fux') > 0)then
                TypePlotVar='fux'
                IsDimensionalPlot_I(iFile) = .true.
                if(TypePlotArea=='los')then
                   StringPlotVar_I(iFile)='flux'
                else
                   StringPlotVar_I(iFile)='emiss'
                end if
                StringPlotParam_I(iFile)='rbody'
             elseif(   index(StringPlot,'PHX') > 0 &
                  .or. index(StringPlot,'phx') > 0)then
                TypePlotVar='phx'
                IsDimensionalPlot_I(iFile) = .true.
                if(TypePlotArea=='los')then
                   StringPlotVar_I(iFile)='flux'
                else
                   StringPlotVar_I(iFile)='emiss'
                end if
                StringPlotParam_I(iFile)='rbody'
             elseif(   index(StringPlot,'NBI') > 0 &
                  .or. index(StringPlot,'nbi') > 0)then
                TypePlotVar='nbi'
                IsDimensionalPlot_I(iFile) = .true.
                StringPlotVar_I(iFile)='intensity'
                StringPlotParam_I(iFile)='rbody'
             elseif(   index(StringPlot,'LGQ') > 0 &
                  .or. index(StringPlot,'lgq') > 0)then
                TypePlotVar = 'lgq'
                IsDimensionalPlot_I(iFile) = index(StringPlot,'LGQ') > 0
                StringPlotVar_I(iFile) = &
                     'squash.03 squash.12 squash.2 squash-15 squash-2 squash-3'
                StringPlotParam_I(iFile)='rbody'
             else
                call stop_mpi('Variable definition missing from StringPlot=' &
                     //StringPlot)
             end if
             if (TypePlotArea == 'shk') &
                  StringPlotVar_I(iFile) = 'divudx '//StringPlotVar_I(iFile)

             ! Set equation parameters for 3D unstructured IDL files
             ! to describe block structure and the dipole. Needed by CCMC.
             if( (TypePlotArea == '3d_' .or. TypePlotArea == '3D_') &
                  .and. TypePlotFormat_I(iFile) == 'idl' &
                  .and. PlotDx_DI(1, iFile) < 0.0) &
                  StringPlotParam_I(iFile) = 'g c th p1 p2 p3 NX NY NZ R'
             if(nInstrument < 1) &
                  TypePlot_I(iFile) = TypePlotArea//'_'//TypePlotVar
          end do

          ! write out the change if ins/INS is found
          if (nPlotFile > nPlotFileRead .and. iProc == 0) then
             write(*,*) ''
             write(*,*) '----------------------------------------'
             write(*,*) ' nPlotFile    =', nPlotFile
             write(*,*) ' use sta   =', any(TypeSatPos_I == 'sta')
             write(*,*) ' use stb   =', any(TypeSatPos_I == 'sta')
             write(*,*) ' use earth =', any(TypeSatPos_I == 'earth')
             do iFile = Plot_+ 1, Plot_+ nPlotFile
                write(*,*) '----------------------------------------'
                write(*,*) ' iFile        =', iFile
                write(*,*) ' DnOutput_I    =', DnOutput_I(iFile)
                write(*,*) ' DtOutput_I    =', DtOutput_I(iFile)
                write(*,*) ' TypePlot_I    =', TypePlot_I(iFile)
                write(*,*) ' TypePlotFormat_I    =', TypePlotFormat_I(iFile)
                write(*,*) ' TypeFile     =', TypeFile_I(iFile)
                write(*,*) ' dimensional  =', IsDimensionalPlot_I(iFile)
                write(*,*) ' StringPlotVar_I   =', &
                     trim(StringPlotVar_I(iFile))
                write(*,*) ' StringPlotParam_I =', &
                     trim(StringPlotParam_I(iFile))
                if (index(TypePlot_I(iFile), 'los') >0) then
                   write(*,*) ' OffsetAngle_I =', OffsetAngle_I(iFile)
                   write(*,*) ' rSizeImage_I =', rSizeImage_I(iFile)
                   write(*,*) ' xOffset_I =', xOffset_I(iFile)
                   write(*,*) ' yOffset_I =', yOffset_I(iFile)
                   write(*,*) ' rOccult_I=', rOccult_I(iFile)
                   write(*,*) ' MuLimbDarkening =', MuLimbDarkening
                   write(*,*) ' nPixel_I =', nPixel_I(iFile)
                   write(*,*) ' NameLosTable_I =', NameLosTable_I(iFile)
                end if
             end do
             write(*,*) '----------------------------------------'
          end if

       case("#INSTRUMENT")
          call read_var('StringInstrument', StringInstrument)
          ! Replace colon with underscore
          i = index(StringInstrument, ':')
          if(i > 1) StringInstrument(i:i) = '_'
          ! Search TypePlot_I array for StringInstrument
          iFile = maxloc(index(TypePlot_I(Plot_+1:Plot_+nPlotFile), &
               trim(StringInstrument)), DIM=1) + Plot_
          ! Check if iFile is really containing the instrument name...
          if(index(TypePlot_I(iFile), trim(StringInstrument)) < 1)then
             if(iProc == 0) write(*,*) NameSub,' ERROR:', &
                  trim(StringInstrument),' was not set in prior #SAVEPLOT'
             call stop_mpi('Correct PARAM.in')
          end if
          if(iProc == 0) write(*,*)'Changing default values for iFile=', iFile
          call read_var('OffsetAngle', OffsetAngle_I(iFile))
          OffsetAngle_I(iFile) = OffsetAngle_I(iFile)*cDegToRad
          ! read max dimensions of the 2d image plane
          call read_var('rSizeImage', rSizeImage_I(iFile))
          ! read the position of image origin relative to grid origin
          call read_var('xOffset', xOffset_I(iFile))
          call read_var('yOffset', yOffset_I(iFile))
          call read_var('rOccult', rOccult_I(iFile))
          call read_var('MuLimbDarkening', MuLimbDarkening)
          ! read the number of pixels
          call read_var('nPix', nPixel_I(iFile))

       case("#NOREFRACTION")
          call read_var('UseNoRefraction', UseNoRefraction)

       case("#SAVEPLOTNAME")
          call read_var('IsPlotNameN', IsPlotNameN)
          call read_var('IsPlotNameT', IsPlotNameT)
          call read_var('IsPlotNameE', IsPlotNameE)

       case("#PLOTFILENAME")
          call read_var('NameMaxTimeUnit',  NameMaxTimeUnit)

       case("#LOSPLOT")
          call read_var('UseLosSimple', UseLosSimple)

       case("#SAVELOGNAME")
          call read_var('IsLogNameN', IsLogNameN)
          call read_var('IsLogNameE', IsLogNameE)
          ! Set _n true if _e is false.
          if(.not.IsLogNameE) IsLogNameN=.true.

       case("#SAVEPLOTSAMR")
          call read_var('DoSavePlotsAmr', DoSavePlotsAmr)

       case("#SAVEBINARY")
          call read_var('DoSaveBinary', DoSaveBinary)

       case("#SAVETECBINARY")
          call read_var('DoSaveTecBinary', DoSaveTecBinary)
          !$acc update device(DoSaveTecBinary)

       case("#GRIDRESOLUTION","#GRIDLEVEL","#REGION","#AMRREGION")
          call read_region_param(NameCommand, UseStrictIn=UseStrict)

       case("#AMR", "#DOAMR", &
            "#AMRLEVELS", "#AMRRESOLUTION", "#AMRLIMIT", "#AMRTYPE", &
            "#AMRCRITERIA", "#AMRCRITERIALEVEL","#AMRCRITERIARESOLUTION", &
            "#AMRCRITERIACELLSIZE", "#AMRPROFILE")
          call read_amr_param(NameCommand, iSession)

       case("#AMRINITPHYSICS")
          if(.not.is_first_session()) CYCLE READPARAM
          call read_amr_param(NameCommand, iSession)

       case('#CONSERVEFLUX')
          call read_var('DoConserveFlux', DoConserveFlux)

       case("#SCHEME")
          if(.not. IsFirstSession) nOrderOld = nOrder
          call read_var('nOrder', nOrder)
          ! Set default value for nStage. Can be overwritten if desired.
          nStage = nOrder
          ! Use RK3 for MP5 scheme
          if(nOrder > 4) nStage = 3
          UseHalfStep = nStage <= 2

          call read_var('TypeFlux', TypeFlux, IsUpperCase=.true.)
          ! For 5-moment equation all schemes are equivalent with Rusanov
          if(UseEfield) TypeFlux = 'RUSANOV'

          LimiterBeta = 1.0
          if(nOrder > 1 .and. TypeFlux /= "SIMPLE")then
             call read_var('TypeLimiter', TypeLimiter)
             if(TypeLimiter /= 'minmod' .and. TypeLimiter /= 'no') &
                  call read_var('LimiterBeta', LimiterBeta)
          else
             TypeLimiter = "no"
          end if

          if(nOrder == 5) then
             ! Some of the settings below can be overwritten by #SCHEME5
             UseFDFaceFlux    = .true.
             DoCorrectFace    = .true.
             UseCweno         = .false.
             ! HighResChange does not work for 1D, but works for 2D and 3D
             UseHighResChange = nDIm > 1
             UseHighOrderAMR  = .true.

             UseTvdReschange = .false.
             UseAccurateResChange = .false.
             DoConserveFlux = .false.
          endif

       case("#SCHEME5")
          if(nOrder == 5)then
             ! If UseFDFaceFlux is true, use ECHO scheme, which is based on
             ! L. Del Zanna, O. Zanotti, N. Bucciantini, P. Londrillo,&
             ! Astronomy and Astrophysics, 473 (2007), pp.11-30.
             call read_var('UseFDFaceFlux', UseFDFaceFlux)
             call read_Var('TypeLimiter5', TypeLimiter5, IsLowerCase=.true.)
             call read_var('UseHighResChange', UseHighResChange)
             call read_var('UseHighOrderAMR', UseHighOrderAMR)
             if(UseFDFaceFlux) call read_var('DoCorrectFace', DoCorrectFace)
             if(.not.UseFDFaceFlux) DoCorrectFace = .false.

             ! If it is not 'cweno', mp5 scheme will be used.
             UseCweno = TypeLimiter5 == 'cweno'

             ! The following lines are related to cweno scheme, and it needs
             ! more tests.
             ! if(UseCweno) call read_var('UsePerVarLimiter', UsePerVarLimiter)
             ! if(UseCweno .and. .not. DoInterpolateFlux) then
             !    ! Density and velocity use density as smooth indicator.
             !    ! Other variables use themselves.
             !    iVarSmooth_V(1:Uz_) = Rho_
             !    do iVar = Uz_+1, nVar
             !       iVarSmooth_V(iVar) = iVar
             !    enddo
             !    call sort_smooth_indicator
             ! endif

             if(UseFDFaceFlux) DoConserveFlux = .false.

             if(.not.UseHighResChange) nOrderProlong  = 2
          else
             if(iProc==0)write(*,*) NameSub, ' WARNING: ',&
                  ' #SCHEME5 should be used only if nOrder = 5'
          end if

       case('#BURGERSEQUATION')
          call read_var('DoBurgers', DoBurgers)

       case('#LIMITER', '#RESCHANGE', '#RESOLUTIONCHANGE', '#TVDRESCHANGE', &
            '#LIMITPTOTAL', '#LOWORDERREGION', '#ADAPTIVELOWORDER')
          call read_face_value_param(NameCommand)

       case("#LIMITMOMENTUM")
          if(.not.is_first_session()) CYCLE READPARAM
          call read_face_value_param(NameCommand)

       case("#NONCONSERVATIVE", "#CONSERVATIVECRITERIA")
          call read_conservative_param(NameCommand)

       case("#TIMESTEPCONTROL", "#CONTROLTIMESTEP", "#CHECKTIMESTEP", &
            "#CONTROLDECREASE", "#CONTROLINCREASE", &
            "#CONTROLFACTOR", "#CONTROLVAR", "#CONTROLINIT","#ENFORCECFL")
          call read_time_step_control_param(NameCommand)

       case("#PROLONGATION")
          call read_var('nOrderProlong', nOrderProlong)
          UseTvdResChange = .false.
          UseAccurateResChange = .false.

       case("#UPDATE")
          call read_var('TypeUpdate', TypeUpdate, IsLowerCase=.true.)

       case('#UPDATEVAR')
          call read_var('StringVarUpdate', StringLine, IsLowerCase=.true.)
          call split_string(StringLine, NameVarUpdate_I, nVarUpdate,&
               UseArraySyntaxIn=.true.)
          if(nVarUpdate == nVar .or. StringLine == 'all')then
             if(allocated(iVarUpdate_I)) deallocate(iVarUpdate_I)
             DoUpdate_V = .true.
          else
             allocate(iVarUpdate_I(nVarUpdate))
             do iVar = 1, nVarUpdate
                call get_ivar(NameVarUpdate_I(iVar), iVarUpdate_I(iVar))
             end do
             DoUpdate_V = .false.
             DoUpdate_V(iVarUpdate_I) = .true.
          end if
       case("#MESSAGEPASS","#OPTIMIZE")
          call read_var('TypeMessagePass', TypeMessagePass)

       case('#CLIMIT', '#CLIGHTWARNING')
          call read_face_flux_param(NameCommand)

       case('#LIGHTSPEED')
          call read_var('ClightDim', ClightDim)

       case('#BORIS', '#BORISSIMPLE', '#BORISREGION')
          if(UseB) call read_boris_param(NameCommand)

       case("#DIVB")
          if(.not.UseB)CYCLE READPARAM
          call read_var('UseDivbSource', UseDivbSource)
          call read_var('UseDivbDiffusion', UseDivbDiffusion)
          call read_var('UseProjection', UseProjection)
          call read_var('UseConstrainB', UseConstrainB)

          if (UseProjection.and.UseConstrainB.and.iProc==0) &
               call stop_mpi('Do not use projection and constrain B together!')
          if (UseProjection.and.UseDivbSource.and.iProc==0) then
             write(*,'(a)')NameSub // &
                  ' WARNING: using divbsource and projection together !!!'
             if (UseStrict) call stop_mpi('Correct PARAM.in')
          end if
          if (UseConstrainB.and.UseDivbSource.and.iProc==0) then
             write(*,'(a)')NameSub // &
                  ' WARNING: using divbsource and constrain B together !!!'
             if (UseStrict) call stop_mpi('Correct PARAM.in')
          end if

          if (iProc==0 .and. nDim > 1 &
               .and..not.UseHyperbolicDivb &
               .and..not.UseDivbSource &
               .and..not.UseProjection &
               .and..not.UseConstrainB &
               .and..not.UseDivbDiffusion) then
             write(*,'(a)') NameSub // &
                  ' WARNING: you should use some div B control method !!!'
             if (UseStrict) call stop_mpi('Correct PARAM.in!')
          end if
          ! Make sure that divbmax will be calculated
          DivbMax = -1.0
          ! reinitialize constrained transport if needed
          DoInitConstrainB = .true.

       case("#B0", "#B0SOURCE", "#CURLB0", "#FORCEFREEB0", "#MONOPOLEB0", &
            "#B0WAVE")
          if(.not.is_first_session())CYCLE READPARAM
          call read_b0_param(NameCommand)

       case("#DBTRICK")
          call read_var('UseDbTrick', UseDbTrick)

       case("#LORENTZFORCE")
          call read_var('UseJCrossBForce', UseJCrossBForce)

       case("#HYPERBOLICDIVE", "#CORRECTELECTRONFLUID", "#CORRECTEFIELD", &
            "#CMAXDIFFUSION")
          call read_ion_electron_param(NameCommand)

       case("#HYPERBOLICDIVB")
          if(.not.UseB)CYCLE READPARAM
          if(Hyp_ == 1)then
             if(iProc==0)then
                write(*,*) NameSub // 'WARNING: ',&
                     'there is no hyperbolic scalar in the equation module!'
                if (UseStrict) &
                     call stop_mpi('Correct PARAM.in or change equation!')
             end if
          else
             call read_var('UseHyperbolicDivB', UseHyperbolicDivB)
             if(UseHyperbolicDivB) then
                call read_var('SpeedHypDim', SpeedHypDim)
                call read_var('HypDecay', HypDecay)
             end if
          endif

       case("#PROJECTION")
          if(.not.UseB)CYCLE READPARAM
          call read_project_divb_param(NameCommand)

       case("#IOUNITS")
          if(.not.is_first_session())CYCLE READPARAM
          call read_var('TypeIoUnit', TypeIoUnit, IsUpperCase=.true.)

       case("#NORMALIZATION")
          if(.not.is_first_session())CYCLE READPARAM
          call read_var('TypeNormalization', TypeNormalization, &
               IsUpperCase=.true.)
          select case(TypeNormalization)
          case('NONE')
             No2Si_V = 1.0
          case('READ')
             call read_var('No2SiUnitX',   No2Si_V(UnitX_))
             call read_var('No2SiUnitU',   No2Si_V(UnitU_))
             call read_var('No2SiUnitRho', No2Si_V(UnitRho_))
          case('PLANETARY', 'SOLARWIND', 'HELIOSPHERIC', 'OUTERHELIO')
             ! Depends on other commands, defined in set_physics
          case('USER')
             ! Call user_normalization later in set_units (see set_physics.f90)
             ! to set the normalization units
          case default
             call stop_mpi(NameSub//' ERROR: unknown TypeNormalization=' &
                  //TypeNormalization)
          end select

       case("#STATEDEFINITION", "#STATEINTERFACE", "#SUBTRACTB0", &
            "#UNIFORMSTATE", "#SHOCKTUBE", "#SHOCKPOSITION", "#RADIALSTATE", &
            "#WAVE", "#WAVE2", "#WAVE4", "#WAVE6", "#BUMP", &
            "#ENTROPYCONSTANT")
          call read_initial_cond_param(NameCommand)

       case("#SOLARWINDFILE", "#UPSTREAM_INPUT_FILE", "#REFRESHSOLARWINDFILE")
          call read_solar_wind_param(NameCommand)
          DoReadSolarwindFile = UseSolarwindFile

       case("#MINIMUMSOLARWINDTEMP")
          call read_var('SwTminDim', SwTminDim)

       case("#TRACE", "#TRACELIMIT", "#TRACERADIUS", "#TRACEEQUATOR", &
            "#TRACEIE", "#TRACEACCURACY", "#TRACETEST", "#SQUASHFACTOR", &
            "#RAYTRACE", "#RAYTRACELIMIT", "#RAYTRACEEQUATOR", "#IE")
          call read_field_trace_param(NameCommand)
       case("#PWCOUPLING")
          call read_var("DoLimitRhoPw", DoLimitRhoPw)
       case("#IECOUPLING")
          call read_ie_velocity_param
       case("#IMCOUPLING","#IM")
          call read_var('TauCoupleIm', TauCoupleIm)
          if(TauCoupleIm < 1.0)then
             TauCoupleIM = 1.0/TauCoupleIM
             if(iProc==0)then
                write(*,'(a)')NameSub//' WARNING: TauCoupleIm should be >= 1'
                if(UseStrict)call stop_mpi('Correct PARAM.in!')
                write(*,*)NameSub//' using the inverse:', TauCoupleIm
             end if
          end if
          call read_var('DoImSatTrace', DoImSatTrace)
          if(NameCommand == "#IMCOUPLING")then
             call read_var('DoCoupleImPressure', DoCoupleImPressure)
             call read_var('DoCoupleImDensity',  DoCoupleImDensity)
             if(DoCoupleImDensity) &
                  call read_var('DensityCoupleFloor', RhoMinDimIm)
             call read_var('DoFixPolarRegion',   DoFixPolarRegion)
             if(DoFixPolarRegion)then
                call read_var('rFixPolarRegion', rFixPolarRegion)
                do iFluid = 1, nFluid
                   call read_var('PolarNDim',  PolarNDim_I(iFluid))
                   call read_var('PolarTDim',  PolarTDim_I(iFluid))
                end do
             end if
          end if
       case("#IMCOUPLINGSMOOTH")
          call read_var('dLatSmoothIm', dLatSmoothIm)

       case("#MULTIFLUIDIM")
          if(.not.is_first_session())CYCLE READPARAM
          ! couple GM and IM in multi-fluid (all, Hp, Op) mode
          call read_var('DoMultiFluidIMCoupling', DoMultiFluidIMCoupling)

       case('#ANISOPRESSUREIM')
          if(.not.is_first_session())CYCLE READPARAM
          ! couple GM and IM in anisotropic pressure mode
          call read_Var('DoAnisoPressureIMCoupling', DoAnisoPressureIMCoupling)

       case('#PSCOUPLING')
          call read_var('TauCouplePs', TauCoupleIm)
          if(TauCoupleIm < 1.0)then
             TauCoupleIM = 1.0/TauCoupleIM
             if(iProc==0)then
                write(*,'(a)')NameSub//' WARNING: TauCoupleIm should be >= 1'
                if(UseStrict) call stop_mpi('Correct PARAM.in!')
                write(*,*)NameSub//' using the inverse:', TauCoupleIm
             end if
          end if
          call read_var('DoCouplePsPressure', DoCoupleImPressure)
          call read_var('DoCouplePsDensity',  DoCoupleImDensity)
          if(DoCoupleImDensity) &
               call read_var('DensityCoupleFloor', RhoMinDimIm)

       case("#RBSATCOUPLING")
          call read_var('DoRbSatTrace', DoRbSatTrace)
       case("#USERSWITCH", "#USERSWITCHES")
          call read_var('StringSwitch', StringSwitch, IsLowerCase=.true.)
          call split_string(StringSwitch, NameSwitch_I, nSwitch)
          do iSwitch = 1, nSwitch
             NameSwitch = NameSwitch_I(iSwitch)
             select case(NameSwitch(1:1))
             case('+')
                DoSwitchOn = .true.
             case('-')
                DoSwitchOn = .false.
             case default
                call stop_mpi(NameSub//': user switch ='//trim(NameSwitch)// &
                     ' should start with + or -')
             end select
             select case(NameSwitch(2:len(NameSwitch)))
             case('all')
                UseUserInitSession = DoSwitchOn
                UseUserICs           = DoSwitchOn
                UseUserPerturbation  = DoSwitchOn
                UseUserB0            = DoSwitchOn
                UseUserSourceExpl    = DoSwitchOn
                UseUserSourceImpl    = DoSwitchOn
                UseUserUpdateStates  = DoSwitchOn
                UseUserTimeStep      = DoSwitchOn
                UseUserWriteProgress = DoSwitchOn
             case('init', 'init_session')
                UseUserInitSession   = DoSwitchOn
             case('ic', 'ics', 'initial_condition')
                UseUserICs = DoSwitchOn
             case('perturb', 'perturbation', 'initial_perturbation')
                UseUserPerturbation = DoSwitchOn
             case('b0', 'get_b0', 'set_b0', 'user_b0')
                UseUserB0 = DoSwitchOn
             case('source', 'calc_source', 'sources', 'calc_sources')
                UseUserSourceExpl = DoSwitchOn
                UseUserSourceImpl = DoSwitchOn
             case('sexpl', 'source_expl', 'sources_expl', 'source_explicit')
                UseUserSourceExpl = DoSwitchOn
             case('simpl', 'source_impl', 'sources_impl', 'source_implicit')
                UseUserSourceImpl = DoSwitchOn
             case('update', 'update_state', 'update_states')
                UseUserUpdateStates = DoSwitchOn
             case('ts', 'timestep', 'time_step')
                UseUserTimeStep = DoSwitchOn
             case('progress', 'write_progress')
                UseUserWriteProgress = DoSwitchOn
             case default
                call stop_mpi(NameSub// &
                     ': unknown user switch='//trim(NameSwitch))
             end select
          end do
          if(iProc==0) write(*,*) &
               'Switches: Init,IC,Perturb,B0,SExpl,SImpl,Update,TS,Progress=',&
               UseUserInitSession, UseUserICs, UseUserPerturbation, UseUserB0,&
               UseUserSourceExpl, UseUserSourceImpl, UseUserUpdateStates, &
               UseUserTimeStep, UseUserWriteProgress

       case("#USERINPUTBEGIN")
          call user_read_inputs

       case("#CODEVERSION")
          ! Only kept for backward compatibility.

       case("#CHANGEVARIABLES")
          call read_var('DoChangeRestartVariables', DoChangeRestartVariables)
          if (DoChangeRestartVariables) UseStrict = .false.

       case("#SPECIFYRESTARTVARMAPPING")
          ! If user sets restart variables mapping, DoChangeRestartVariables
          ! should be set to true.
          call read_var('DoSpecifyRestartVarMapping', &
               DoSpecifyRestartVarMapping)
          if (DoSpecifyRestartVarMapping) then
             DoChangeRestartVariables = .true.
             UseStrict = .false.
             if (allocated(NameVarRestartFrom_V))  &
                  deallocate(NameVarRestartFrom_V)
             if (allocated(NameVarRestartTo_V))    &
                  deallocate(NameVarRestartTo_V)
             allocate(NameVarRestartFrom_V(max(nVar,nVarEquationRead)))
             allocate(NameVarRestartTo_V(max(nVar,nVarEquationRead)))
             call read_var('NameVarsRestartFrom', NameVarsRestartFrom, &
                  IsLowerCase=.true.)
             call read_var('NameVarsRestartTo',   NameVarsRestartTo,   &
                  IsLowerCase=.true.)
             call split_string(NameVarsRestartFrom, NameVarRestartFrom_V, &
                  nVarRestartMappingFrom)
             call split_string(NameVarsRestartTo,   NameVarRestartTo_V,   &
                  nVarRestartMappingTo)
             nVarRestartMapping = nVarRestartMappingFrom
             if (nVarRestartMappingFrom /= nVarRestartMappingTo) then
                if (iProc == 0) write(*,*) NameSub,                     &
                     ' nVarRestartMappingFrom, nVarRestartMappingTo =', &
                     nVarRestartMappingFrom, nVarRestartMappingTo
                call stop_mpi(NameSub //  &
                     ' Error: inconsistent nVar in SPECIFYRESTARTVARMAPPING')
             end if
          end if

       case("#EQUATION")
          if(.not.is_first_session())CYCLE READPARAM
          call read_var('NameEquation', NameEquationRead)
          call read_var('nVar',         nVarEquationRead)
          if(NameEquationRead /= NameEquation .and. iProc==0 &
               .and. .not. DoChangeRestartVariables)then
             write(*,'(a)')'BATSRUS was compiled with equation '// &
                  NameEquation//' which is different from '// &
                  NameEquationRead
             call stop_mpi(NameSub//' ERROR: incompatible equation names')

          end if
          if(nVarEquationRead /= nVar .and. iProc==0 .and. &
               .not. DoChangeRestartVariables)then
             write(*,'(a,i2,a,i2)')&
                  'BATSRUS was compiled with nVar=', nVar, &
                  ' that is different from nVarEquationRead=', nVarEquationRead
             call stop_mpi(NameSub//' ERROR: Incompatible number of variables')
          end if

       case("#NEWRESTART", "#RESTARTINDIR", "#RESTARTINFILE", &
            "#PRECISION", "#BLOCKLEVELSRELOADED")
          if(.not.is_first_session())CYCLE READPARAM
          call read_restart_parameters(NameCommand)

       case("#SAVERESTART", "#RESTARTOUTDIR", "#RESTARTOUTFILE", &
            "#RESTARTBLOCKDATA", "#RESTARTWITHFULLB", "#RESTARTFULLB")
          call read_restart_parameters(NameCommand)

       case("#RESTARTVARIABLES")
          ! This reads the names of the variables saved in the input
          ! restart file.
          call read_var('NameVarRestartRead', NameVarRestartRead, &
               IsLowerCase=.true.)
          IsReadNameVarRestart = .true.

       case("#PLOTDIR")
          call read_var("NamePlotDir",NamePlotDir)
          call fix_dir_name(NamePlotDir)

       case("#SATELLITE")
          if(.not.is_first_session())CYCLE READPARAM
          call read_satellite_parameters(NameCommand)
          DoReadSatelliteFiles = nSatellite > 0

       case('#STEADYSTATESATELLITE', '#SATELLITETIMEOFFSET')
          call read_satellite_parameters(NameCommand)

       case('#MAGPERTURBINTEGRAL')
          call read_magperturb_param(NameCommand)

       case('#GEOMAGINDICES')
          call read_magperturb_param(NameCommand)
          nFile = max(nFile, indexfile_)

       case("#MAGNETOMETER")
          call read_magperturb_param(NameCommand)
          nFile = max(nFile, magfile_)

       case("#MAGNETOMETERGRID")
          call read_magperturb_param(NameCommand)
          nFile = max(nFile, maggridfile_+nMagGridFile)

       case('#SUPERMAGINDICES')
          call read_magperturb_param(NameCommand)
          nFile = max(nFile, maggridfile_+nMagGridFile)

       case("#GRIDGEOMETRY", "#GRIDGEOMETRYLIMIT")
          if(.not.is_first_session())CYCLE READPARAM
          call read_var('TypeGeometry', TypeGeometry, IsLowerCase=.true.)
          ! need to read in the general grid file
          if(TypeGeometry == 'spherical_genr') then
             call read_var('NameGridFile', NameGridFile)
             call read_gen_radial_grid(NameGridFile)
          end if
          if(TypeGeometry == 'roundcube') then
             call read_var('rRound0', rRound0)
             call read_var('rRound1', rRound1)
          end if
          if(NameCommand == '#GRIDGEOMETRYLIMIT')then
             do iDim = 1, nDim
                call read_var('CoordDimMin_D', CoordDimMin_D(iDim))
                call read_var('CoordDimMax_D', CoordDimMax_D(iDim))
             end do
             XyzMin_D = CoordDimMin_D
             XyzMax_D = CoordDimMax_D
             if(TypeGeometry(1:9) == 'spherical')then
                RadiusMin = XyzMin_D(1)
                RadiusMax = XyzMax_D(1)
                XyzMin_D(Dim2_) = XyzMin_D(Dim2_)*cDegToRad
                XyzMax_D(Dim2_) = XyzMax_D(Dim2_)*cDegToRad
                XyzMin_D(Dim3_) = XyzMin_D(Dim3_)*cDegToRad
                XyzMax_D(Dim3_) = XyzMax_D(Dim3_)*cDegToRad
             elseif(TypeGeometry(1:11) == 'cylindrical')then
                RadiusMin = XyzMin_D(1)
                RadiusMax = XyzMax_D(1)
                XyzMin_D(Dim2_) = XyzMin_D(Dim2_)*cDegToRad
                XyzMax_D(Dim2_) = XyzMax_D(Dim2_)*cDegToRad
             end if
          end if

       case("#GRIDSYMMETRY")
          nMirror_D = 1
          call read_var('IsMirrorX', IsMirrorX)
          if(IsMirrorX) nMirror_D(1) = 2
          call read_var('IsMirrorY', IsMirrorY)
          if(IsMirrorY) nMirror_D(2) = 2
          call read_var('IsMirrorZ', IsMirrorZ)
          if(IsMirrorZ) nMirror_D(3) = 2

       case("#LIMITRADIUS")
          if(.not.is_first_session())CYCLE READPARAM
          call read_var('rMin', RadiusMin)
          call read_var('rMax', RadiusMax)

       case("#UNIFORMAXIS")
          if(.not.is_first_session())CYCLE READPARAM
          call read_var('UseUniformAxis', UseUniformAxis)

       case("#FIXAXIS")
          call read_var('DoFixAxis', DoFixAxis)
          call read_var('rFixAxis', rFixAxis)
          call read_var('r2FixAxis', r2FixAxis)

       case('#COARSEAXIS')
          call read_coarse_axis_param

       case("#GRID")
          if(.not.is_first_session())CYCLE READPARAM
          call read_var('nRootBlock1', nRootRead_D(1))
          call read_var('nRootBlock2', nRootRead_D(2))
          call read_var('nRootBlock3', nRootRead_D(3))

          call read_var('xMin', xMinBox)
          call read_var('xMax', xMaxBox)
          call read_var('yMin', yMinBox)
          call read_var('yMax', yMaxBox)
          call read_var('zMin', zMinBox)
          call read_var('zMax', zMaxBox)

       case("#GRIDBLOCK", "#GRIDBLOCKALL")
          if(.not.is_first_session())CYCLE READPARAM
          call read_var('MaxBlock',     MaxBlock)
          if(NameCommand == "#GRIDBLOCKALL") &
               MaxBlock   = 1 + (MaxBlock-1)/nProc

       case("#CHECKGRIDSIZE")
          if(.not.is_first_session())CYCLE READPARAM
          call read_var('nI', nIJKRead_D(1))
          call read_var('nJ', nIJKRead_D(2))
          call read_var('nK', nIJKRead_D(3))
          if(any(nIJK_D/=nIJKRead_D).and.iProc==0)then
             write(*,*)'Code is compiled with nI,nJ,nK=', nIJK_D
             call stop_mpi('Change nI,nJ,nK with Config.pl -g and recompile!')
          end if
          call read_var('MinBlockAll', MinBlockAll)
          ! Set MaxBlock large enough.
          ! Add 1 extra block for possible load balancing
          MaxBlock = max(MaxBlock, 2 + (MinBlockAll-1)/nProc)

       case("#USERMODULE")
          if(.not.is_first_session())CYCLE READPARAM
          call read_var('NameUserModule', NameUserModuleRead)
          if(NameUserModuleRead /= NameUserModule .and. &
               .not. DoChangeRestartVariables) then
             if(iProc==0)write(*,'(3a)') NameSub, &
                  ' WARNING: code is compiled with user module ', &
                  trim(NameUserModule)
             if(UseStrict)call stop_mpi('Select the correct user module!')
          end if

       case("#GAMMA")
          if(.not.is_first_session())CYCLE READPARAM
          do iFluid = 1, nFluid
             call read_var('Gamma_I', Gamma_I(iFluid))
          end do
          ! Derived values for fluids
          GammaMinus1_I = Gamma_I - 1.0
          where(GammaMinus1_I /= 0.0)
             InvGammaMinus1_I = 1.0 / GammaMinus1_I
          elsewhere
             ! This should not be used (isothermal case)
             InvGammaMinus1_I = 1.5
          end where

          ! Isothermal case (for ions?)
          if(any(Gamma_I == 1.0))then
             call set_non_conservative
             if(iProc==0) write(*,*) NameSub, &
                  ': for gamma=1 UseNonConservative is set to TRUE'
          endif

          ! Values for the ion fluids
          GammaIon_I          = Gamma_I(1:nIonFluid)
          GammaMinus1Ion_I    = GammaMinus1_I(1:nIonFluid)
          InvGammaMinus1Ion_I = InvGammaMinus1_I(1:nIonFluid)

          ! Scalar values for the first fluid for simpler code
          Gamma          = Gamma_I(1)
          GammaMinus1    = GammaMinus1_I(1)
          InvGammaMinus1 = InvGammaMinus1_I(1)

          if(UseElectronPressure)then
             call read_var('GammaElectron',  GammaElectron)
             ! Derived values for electron
             GammaElectronMinus1    = GammaElectron - 1.0
             InvGammaElectronMinus1 = 1.0 / GammaElectronMinus1
          else
             ! Default values for electrons are the same as first fluid
             ! so ideal MHD works as expected
             GammaElectron          = Gamma
             GammaElectronMinus1    = GammaMinus1
             InvGammaElectronMinus1 = InvGammaMinus1
          end if

       case("#LOOKUPTABLE")
          call read_lookup_table_param

       case("#PLASMA", "#ELECTRONPRESSURERATIO")
          if(NameCommand == "#PLASMA")then
             if(UseMultiSpecies) then
                do iSpecies = SpeciesFirst_, SpeciesLast_
                   call read_var('MassSpecies', MassSpecies_V(iSpecies))
                enddo
                do iSpecies = SpeciesFirst_, SpeciesLast_
                   call read_var('ChargeSpecies', ChargeSpecies_I(iSpecies))
                enddo
             else
                do iFluid = 1, nFluid
                   call read_var('MassFluid', MassFluid_I(iFluid))
                end do
                do iFluid = 1, nIonFluid
                   call read_var('ChargeIon', ChargeIon_I(iFluid))
                end do
             endif
          endif

          call read_var('ElectronTemperatureRatio', ElectronTemperatureRatio)
          ElectronPressureRatio = ElectronTemperatureRatio

          ! AverageIonCharge is only useful when there is only one ion
          if(nIonFluid == 1 .and. .not. UseMultiSpecies)then
             AverageIonCharge = ChargeIon_I(1)
             ElectronPressureRatio = ElectronTemperatureRatio*AverageIonCharge
          end if

          PePerPtotal = ElectronPressureRatio/(1 + ElectronPressureRatio)

          if(NameCommand == "#ELECTRONPRESSURERATIO") &
               call read_var('InnerBcPeRatio', RatioPe2P)

       case("#MULTISPECIES")
          call read_var('DoReplaceDensity', DoReplaceDensity)
          call read_var('SpeciesPercentCheck',SpeciesPercentCheck)

       case("#NEUTRALFLUID")
          call read_var('DoConserveNeutrals', DoConserveNeutrals)
          call read_var('TypeFluxNeutral',    TypeFluxNeutral)

       case("#MULTIION", "#COLLISION", "#MULTIIONSTATE")
          call multi_ion_set_parameters(NameCommand)

       case('#USERBOUNDARY', '#EXTRABOUNDARY')
          if(.not.is_first_session())CYCLE READPARAM
          call read_var('UseExtraBoundary', UseExtraBoundary)
          if(UseExtraBoundary)then
             call read_var('TypeFaceBc_I(ExtraBc_)', TypeFaceBc_I(ExtraBc_))
          end if

       case('#SOLIDSTATE')
          call read_boundary_geometry_param(NameCommand)

       case('#FACEBOUNDARY')
          if(iProc==0) &
               call stop_mpi('#FACEBOUNDARY command is no longer used!')

       case("#SOLARWIND")
          ! if(.not.is_first_session())CYCLE READPARAM
          call read_var('SwNDim', SolarWindNDim)
          call read_var('SwTDim', SolarWindTempDim)
          call read_var('SwUxDim', SolarWindUxDim)
          call read_var('SwUyDim', SolarWindUyDim)
          call read_var('SwUzDim', SolarWindUzDim)
          call read_var('SwBxDim', SolarWindBxDim)
          call read_var('SwByDim', SolarWindByDim)
          call read_var('SwBzDim', SolarWindBzDim)

       case("#OUTFLOWPRESSURE")
          call read_var('UseOutflowPressure', UseOutflowPressure)
          if(UseOutflowPressure) call read_var('pOutflowSi', pOutflowSi)

       case("#MAGNETOSPHERE","#BODY")
          if(.not.is_first_session())CYCLE READPARAM
          call read_var('UseBody', UseBody)
          if(UseBody)then
             call read_var('rBody', rBody) ! Inner boundary radius
             if(NameThisComp=='GM') & ! for calculating field-aligned currents
                  call read_var('rCurrents', Rcurrents)
             ! Read densities and temperatures
             if(UseMultiSpecies)then
                do iSpecies = 1, nSpecies
                   call read_var('BodyNDim', BodyNSpeciesDim_I(iSpecies))
                end do
                BodyNDim_I(1) = sum(BodyNSpeciesDim_I)
                call read_var('BodyTDim', BodyTDim_I(1))
             else
                do iFluid = 1, nFluid
                   call read_var('BodyNDim', BodyNDim_I(iFluid))
                   call read_var('BodyTDim', BodyTDim_I(iFluid))
                end do
             end if
          else
             rBody = 0.0
          end if
       case("#CORONA")
          if(.not.is_first_session())CYCLE READPARAM
          call read_var('rCorona', rBody) ! Inner boundary radius
          ! Read densities and temperatures
          do iFluid = 1, nFluid
             call read_var('CoronaNDim', BodyNDim_I(iFluid))
             call read_var('CoronaTDim', BodyTDim_I(iFluid))
          end do

       case("#INNERBOUNDARY", "#INNERBCPE", &
            "#POLARBOUNDARY", "#CPCPBOUNDARY", &
            "#MAGNETICINNERBOUNDARY", "#OUTFLOWCRITERIA", "#YOUNGBOUNDARY")
          call read_face_boundary_param(NameCommand)

       case("#ADVECTION", "#FRICTION")
          call read_source_param(NameCommand)

       case("#GRAVITY")
          if(.not.is_first_session())CYCLE READPARAM
          call read_var('UseGravity', UseGravity)
          if(UseGravity)then
             call read_var('iDirGravity', iDirGravity)
             if(iDirGravity /= 0) call read_var('GravitySi', GravitySi)
          end if

       case("#SECONDBODY")
          if(.not.is_first_session())CYCLE READPARAM
          call read_var('UseBody2', UseBody2)
          if(UseBody2)then
             call read_var('rBody2', rBody2)
             call read_var('MassBody2Si',MassBody2Si)
             call read_var('Body2NDim', Body2NDim)
             call read_var('Body2TDim', Body2TDim)
             call read_var('UseBody2Orbit',  UseBody2Orbit)
             if(.not.UseBody2Orbit)then
                call read_var('xBody2', xBody2)
                call read_var('yBody2', yBody2)
                call read_var('zBody2', zBody2)
             end if
          end if

       case("#BOUNDARYSTATE", "#BOUNDARYSTATE_NT")
          ! Read boundary states for multiple boundaries.
          call read_var('StringBoundary', StringBoundary, IsLowerCase=.true.)
          do iVar = 1, nVar
             if (NameCommand == '#BOUNDARYSTATE')  then
                call read_var(NamePrimitive_V(iVar), BoundaryStateDim_V(iVar))
                iBoundaryState = 1
             else
                call read_var(NamePrimitiveNT_V(iVar),BoundaryStateDim_V(iVar))
                iBoundaryState = 2
             end if
          end do

          call split_string(StringBoundary, NameBoundary_I, nNameBoundary)

          do iNameBoundary = 1, nNameBoundary
             select case(NameBoundary_I(iNameBoundary))

             case('solid', '-3')
                iFaceBoundaryState_I(SolidBc_) = iBoundaryState
                FaceStateDim_VI(:,SolidBc_)    = BoundaryStateDim_V
             case('body2', '-2')
                iFaceBoundaryState_I(body2_) = iBoundaryState
                FaceStateDim_VI(:,body2_)    = BoundaryStateDim_V
             case('body1', '-1')
                iFaceBoundaryState_I(body1_) = iBoundaryState
                FaceStateDim_VI(:,body1_)    = BoundaryStateDim_V
             case('extra', '0')
                iFaceBoundaryState_I(ExtraBc_) = iBoundaryState
                FaceStateDim_VI(:,ExtraBc_)    = BoundaryStateDim_V
             case('xminbox')
                iFaceBoundaryState_I(xMinBc_) = iBoundaryState
                FaceStateDim_VI(:,xMinBc_)    = BoundaryStateDim_V
             case('xmaxbox')
                iFaceBoundaryState_I(xMaxBc_) = iBoundaryState
                FaceStateDim_VI(:,xMaxBc_)    = BoundaryStateDim_V
             case('yminbox')
                iFaceBoundaryState_I(yMinBc_) = iBoundaryState
                FaceStateDim_VI(:,yMinBc_)    = BoundaryStateDim_V
             case('ymaxbox')
                iFaceBoundaryState_I(yMaxBc_) = iBoundaryState
                FaceStateDim_VI(:,yMaxBc_)    = BoundaryStateDim_V
             case('zminbox')
                iFaceBoundaryState_I(zMinBc_) = iBoundaryState
                FaceStateDim_VI(:,zMinBc_)    = BoundaryStateDim_V
             case('zmaxbox')
                iFaceBoundaryState_I(zMaxBc_) = iBoundaryState
                FaceStateDim_VI(:,zMaxBc_)    = BoundaryStateDim_V

             case('coord1min', '1')
                iCellBoundaryState_I(Coord1MinBc_) = iBoundaryState
                CellStateDim_VI(:,Coord1MinBc_)    = BoundaryStateDim_V
             case('coord1max', '2')
                iCellBoundaryState_I(Coord1MaxBc_) = iBoundaryState
                CellStateDim_VI(:,Coord1MaxBc_)    = BoundaryStateDim_V
             case('coord2min', '3')
                iCellBoundaryState_I(Coord2MinBc_) = iBoundaryState
                CellStateDim_VI(:,Coord2MinBc_)    = BoundaryStateDim_V
             case('coord2max', '4')
                iCellBoundaryState_I(Coord2MaxBc_) = iBoundaryState
                CellStateDim_VI(:,Coord2MaxBc_)    = BoundaryStateDim_V
             case('coord3min', '5')
                iCellBoundaryState_I(Coord3MinBc_) = iBoundaryState
                CellStateDim_VI(:,Coord3MinBc_)    = BoundaryStateDim_V
             case('coord3max', '6')
                iCellBoundaryState_I(Coord3MaxBc_) = iBoundaryState
                CellStateDim_VI(:,Coord3MaxBc_)    = BoundaryStateDim_V

             case default
                call stop_mpi(NameSub//' ERROR: incorrect boundary name='//&
                     NameBoundary_I(iNameBoundary))
             end select
          end do

       case("#DIPOLEBODY2")
          if(.not.is_first_session())CYCLE READPARAM
          call read_var('BdpDimBody2x', BdpDimBody2_D(1))
          call read_var('BdpDimBody2y', BdpDimBody2_D(2))
          call read_var('BdpDimBody2z', BdpDimBody2_D(3))

       case('#PLANET', '#MOON', '#COMET', '#IDEALAXES', '#ROTATIONAXIS',&
            '#MAGNETICAXIS', '#MAGNETICCENTER', '#ROTATION', '#DIPOLE', &
            '#NONDIPOLE', '#UPDATEB0',  '#MULTIPOLEB0')

          call check_stand_alone
          if(.not.is_first_session())CYCLE READPARAM

          call read_planet_var(NameCommand)

       case("#ROTATEHGR")
          call check_stand_alone
          if(.not.is_first_session() .or. dLongitudeHgrDeg/=0.0)CYCLE READPARAM
          call read_var('dLongitudeHgr', dLongitudeHgrDeg)
          dLongitudeHgr = dLongitudeHgrDeg * cDegToRad

       case("#ROTATEHGI")
          call check_stand_alone
          if(.not.is_first_session() .or. dLongitudeHgiDeg/=0.0)CYCLE READPARAM
          call read_var('dLongitudeHgi', dLongitudeHgiDeg)
          dLongitudeHgi = dLongitudeHgiDeg * cDegToRad

       case("#COORDSYSTEM","#COORDINATESYSTEM")
          if(TypeCoordSystem /= 'HGC' .and. TypeCoordSystem /= 'hgc')then
             ! HGC is the only type of coordinate system
             ! which can be switched to different coordinate
             ! system in the course of run
             if(.not.is_first_session())CYCLE READPARAM
          end if
          call read_var('TypeCoordSystem', TypeCoordSystem,IsUpperCase=.true.)
          select case(NameThisComp)
          case('GM')
             if (TypeCoordSystem == 'GEO') UseRotatingFrame = .true.
             if(TypeCoordSystem /= 'GSM' .and. TypeCoordSystem /= 'GSE' &
                  .and. TypeCoordSystem /= 'GEO' ) &
                  call stop_mpi(NameSub// &
                  ' ERROR: cannot handle coordinate system '//TypeCoordSystem)
          case('IH', 'OH')
             select case(TypeCoordSystem)
             case('HGI', 'hgi')
                ! If rotating frame was on in the previous session then
                ! we need to transform from HGR/HGC to HGI system.
                ! Note: This only works if the two coordinate systems
                ! are aligned at the initial time (i.e. HGR = HGC).
                if(UseRotatingFrame) iSignRotationIC = +1
                UseRotatingFrame = .false.
             case('HGC', 'HGR', 'hgc', 'hgr')
                UseRotatingFrame = .true.
             case default
                call stop_mpi(NameSub// &
                     ' ERROR: cannot handle coordinate system '&
                     //TypeCoordSystem)
             end select
          case('SC')
             select case(TypeCoordSystem)
             case('HGR', 'HGC', 'hgr', 'hgc')
                UseRotatingFrame = .true.
                if(iProc==0) &
                     write(*,*)NameSub//' setting UseRotatingFrame = T'
             case default
                call stop_mpi(NameSub// &
                     ' ERROR: cannot handle coordinate system '&
                     //TypeCoordSystem)
             end select
          case('EE')
             select case(TypeCoordSystem)
             case('HGR', 'HGC', 'hgr', 'hgc')
                UseRotatingFrame = .true.
             case('HGI', 'hgi', 'GSM')
                UseRotatingFrame = .false.
             case default
                call stop_mpi(NameSub// &
                     ' ERROR: cannot handle coordinate system '&
                     //TypeCoordSystem)
             end select
          end select

       case("#ROTATINGINITIALCONDITION")
          call read_var('iSignRotationIC', iSignRotationIC)

       case("#NSTEP")
          if(.not.is_first_session())CYCLE READPARAM
          call read_var('nStep', nStep)

       case("#NPREVIOUS")
          if(.not.is_first_session())CYCLE READPARAM
          call read_var('nPrev', nStepPrev)
          call read_var('DtPrev', DtPrev)

       case("#STARTTIME", "#SETREALTIME")
          if(.not.is_first_session())CYCLE READPARAM
          call read_var('iYear',   iStartTime_I(1))
          call read_var('iMonth',  iStartTime_I(2))
          call read_var('iDay',    iStartTime_I(3))
          call read_var('iHour',   iStartTime_I(4))
          call read_var('iMinute', iStartTime_I(5))
          call read_var('iSecond', iStartTime_I(6))
          iStartTime_I(7) = 0
          if(IsStandAlone)then
             call time_int_to_real(iStartTime_I, StartTime)
          else
             ! Check if things work out or not
             call time_int_to_real(iStartTime_I, StartTimeCheck)
          end if

       case('#ROTPERIOD')
          call read_var('RotPeriodSi', RotPeriodSI)

       case("#TIMEEND", "#ENDTIME")
          UseEndTime = .true.
          call read_var('iYear',   iEndTime_I(1))
          call read_var('iMonth',  iEndTime_I(2))
          call read_var('iDay',    iEndTime_I(3))
          call read_var('iHour',   iEndTime_I(4))
          call read_var('iMinute', iEndTime_I(5))
          call read_var('iSecond', iEndTime_I(6))
          iEndTime_I(7) = 0
          call time_int_to_real(iEndTime_I, EndTime)

       case("#TIMESIMULATION")
          if(.not.is_first_session())CYCLE READPARAM
          if(IsStandAlone)then
             call read_var('tSimulation', tSimulation)
          else
             call read_var('tSimulation', tSimulationCheck)
          end if

       case("#HELIOUPDATEB0")
          call read_var('DtUpdateB0', DtUpdateB0)
          DoUpdateB0 = DtUpdateB0 > 0.0

       case("#HELIODIPOLE")
          if(.not.is_first_session())CYCLE READPARAM
          call read_var('HelioDipoleStrengthSi', DipoleStrengthSi)
          call read_var('HelioDipoleTilt', ThetaTilt)
          ThetaTilt = ThetaTilt*cDegToRad
          UseB0 = .true.

       case("#HELIOROTATION", "#INERTIAL")
          if(iProc==0)write(*,*) NameSub, ' WARNING: ',&
               ' #HELIOROTATION / #INERTIAL command is obsolete and ignored'

       case("#HELIOBUFFERGRID", "#BUFFERGRID", "#BUFFERBODY2", &
            "#RESTARTBUFFERGRID")
          if(is_first_session())call read_buffer_grid_param(NameCommand)

       case("#REVERSEFIELD")
          call read_reverse_field_param
          if(SignB_ > 1 .and. DoReverseField)then
             NameVar_V(SignB_) = 'SignB'
             call set_namevar
          end if

       case("#THINCURRENTSHEET")
          call read_var('DoThinCurrentSheet', DoThinCurrentSheet)
          if(SignB_ > 1 .and. DoThinCurrentSheet)then
             NameVar_V(SignB_) = 'SignB'
             call set_namevar
          end if

       case("#ALIGNBANDU")
          call read_samhd_param
          if(BperU_ > 1 .and. UseSaMhd)then
             NameVar_V(BperU_) = 'BperU'
             call set_namevar
          end if

          ! OUTERHELIOSPHERE SPECIFIC COMMANDS

       case("#OHNEUTRALS")
          call read_var('RhoNeuWindDim',  RhoNeuWindDim)
          call read_var('TempNeuWindDim', TempNeuWindDim)
          call read_var('UxNeuWindDim',   UxNeuWindDim)
          call read_var('UyNeuWindDim',   UyNeuWindDim)
          call read_var('UzNeuWindDim',   UzNeuWindDim)
          call read_var('MassNeutralDim', MassNeutralDim)

       case("#OHBOUNDARY")
          call read_var('DoOhNeutralBc', DoOhNeutralBc)
          if(DoOhNeutralBc)then
             do iFluid = nIonFluid+1, nFluid
                call read_var('RhoBcFactor', RhoBcFactor_I(iFluid))
                call read_var('uBcFactor', uBcFactor_I(iFluid))
             end do
          end if

       case("#PUIGRID")
          call read_pui_param(NameCommand)

          ! CORONA SPECIFIC COMMANDS

       case("#HARMONICSFILE", "#NEWHARMONICSFILE", "#HARMONICSGRID", &
            "#FACTORB0", "#MAGNETOGRAM") ! last one kept for compatibility only
          call read_magnetogram_param(NameCommand)

       case('#LDEM')
          call read_var('UseLdem',  UseLdem)
          if(UseLdem) then
             call read_var('NameLdemFile', NameLdemFile)
             call read_var('iRadiusLdem', iRadiusLdem)
             call read_ldem(NamePlotDir)
          end if

       case("#CORONALHEATING", "#LONGSCALEHEATING", "#ACTIVEREGIONHEATING")
          call read_coronal_heating_param(NameCommand)

       case("#HEATPARTITIONING", "#HIGHBETASTOCHASTIC",  "#ALIGNMENTANGLE", &
            "#NONLINAWDISSIPATION", "#LIMITIMBALANCE", "#POYNTINGFLUX")
          call read_turbulence_param(NameCommand)

       case("#RADIATIVECOOLING")
          call read_var('UseRadCooling', UseRadCooling)

       case("#CHROMOSPHERE")
          call read_chromosphere_param

       case("#TRANSITIONREGION")
          call read_cooling_param

       case("#FIELDLINETHREAD", '#PLOTTHREADS', '#CHROMOEVAPORATION')
          call read_thread_param(NameCommand, iSession)

       case("#THREADEDBC")
          call read_threaded_bc_param

       case('#THREADRESTART')
          call read_var('DoThreadRestart', DoThreadRestart)

       case("#ADVECTWAVES", "#ALFVENWAVES", "#WAVEPRESSURE", &
            "#FREQUENCY", "#SPECTRUM", "#WAVEREFLECTION",    &
            "#AWREPRESENTATIVE")
          call read_waves_param(NameCommand)

       case("#LASERPULSE", "#LASERBEAM", "#LASERBEAMS", "#LASERBEAMPROFILE", &
            "#LASERRAYTEST")
          call read_laser_heating_param(NameCommand)

       case("#CME", "#ARCH", "#TD99FLUXROPE", "#GL98FLUXROPE", "#SHEARFLOW", &
            "#CMS", "#CMETIME")
          call EEE_set_parameters(NameCommand)

       case("#STAR")
          call check_stand_alone
          if(.not.is_first_session())CYCLE READPARAM

          call read_star_var(NameCommand)

       case default
          if(iProc==0) then
             write(*,*) NameSub // ' WARNING: unknown #COMMAND ' // &
                  trim(NameCommand),' !!!'
             if(UseStrict)call stop_mpi('Correct PARAM.in!')
          end if
       end select
    end do READPARAM

    ! end reading parameters

  contains
    !==========================================================================
    subroutine check_stand_alone
      !------------------------------------------------------------------------
      if(IsStandAlone) RETURN
      if(iProc==0) write(*,*) NameSub,' WARNING:'// &
           ' command '//trim(NameCommand)//&
           ' is allowed in stand alone mode only !!!'
      if(UseStrict)call stop_mpi(NameSub//' Correct PARAM.in')

    end subroutine check_stand_alone
    !==========================================================================
    logical function is_first_session()
      !------------------------------------------------------------------------
      is_first_session = IsFirstSession

      if(.not.IsFirstSession .and. iProc==0)then
         write(*,*)NameSub//' WARNING: command '//trim(NameCommand)// &
              ' can be used in the first session only !!!'
         if(UseStrict)call stop_mpi('Correct PARAM.in')
      end if

    end function is_first_session
    !==========================================================================
    subroutine set_namevar

      use ModMultiFluid, ONLY: extract_fluid_name, select_fluid, &
           iRho, iRhoUx, iRhouy, iRhoUz, iP, iEnergy
      use ModUtilities, ONLY: join_string

      integer :: iWave, iPui, iMaterial, iFluid, lConservative, lPrimitive
      character(len=3)  :: NameWave, NamePui
      character(len=2)  :: NameMaterial
      character(len=50) :: NamePrimitive, NamePrimitivePlot, NameConservative
      character(len=50) :: NamePrimitiveNT
      character(len=50) :: String, NameFluid
      character(len=500):: StringConservative, StringPrimitivePlot
      character(len=500):: StringPrimitiveOrig
      integer           :: iVar, iElement, iChargeState, iIon
      character(len=4)  :: NameChargestate
      !------------------------------------------------------------------------
      ! Fix the NameVar_V string for waves
      if(WaveLast_ > 1)then
         do iWave = 1, nWave
            write(NameWave,'(a,i2.2)') 'I', iWave
            NameVar_V(WaveFirst_+iWave-1) = NameWave
         end do
      end if

      ! Fix the NameVar_V string for pickup ions
      if(PuiLast_ > 1)then
         do iPui = 1, nPui
            write(NamePui,'(a,i2.2)') 'F', iPui
            NameVar_V(PuiFirst_+iPui-1) = NamePui
         end do
      end if

      ! Fix the NameVar_V string for material levels
      if(MaterialLast_ > 1)then
         do iMaterial = 1, nMaterial
            write(NameMaterial,'(a,i1.1)') 'M', iMaterial
            NameVar_V(MaterialFirst_+iMaterial-1) = NameMaterial
         end do
      end if

      ! Fix the NameVar_V string for charge states
      if(nChargeStateAll > 1)then
         if(UseMultiIon)then
            iIon = 2
         else
            iVar = ChargeStateFirst_
         end if
         do iElement = 1, nElement
            do iChargeState = 1, nChargeState_I(iElement)
               if(nChargeState_I(iElement) < 10) then
                  write(NameChargestate,'(a,i1.1)')&
                       trim(NameElement_I(iElement)),iChargeState
               else
                  write(NameChargestate,'(a,i2.2)')&
                       trim(NameElement_I(iElement)),iChargeState
               end if
               if(UseMultiIon)then
                  call select_fluid(iIon)
                  NameVar_V(iRho)    = trim(NameChargeState)//'Rho'
                  NameVar_V(iRhoUx)  = trim(NameChargeState)//'Mx'
                  NameVar_V(iRhoUy)  = trim(NameChargeState)//'My'
                  NameVar_V(iRhoUz)  = trim(NameChargeState)//'Mz'
                  NameVar_V(iP)      = trim(NameChargeState)//'P'
                  NameVar_V(iEnergy) = trim(NameChargeState)//'E'
                  iIon = iIon + 1
               else
                  NameVar_V(iVar) = NameChargeState
                  iVar = iVar + 1
               end if
            end do
         end do
      end if

      ! space separated NameVar string containing all variable names
      call join_string(nVar, NameVar_V, NameVarCouple)

      ! convert NameVar_V to NameVarLower_V (lower case)
      do iVar = 1, nVar+nFluid
         NameVarLower_V(iVar) = NameVar_V(iVar)
         call lower_case(NameVarLower_V(iVar))
      end do

      ! initialize the two strings to be empty strings
      StringConservative  = ''
      StringPrimitivePlot = ''

      ! Only loop through nVar (e is taken care by setting NameConservative)
      do iVar = 1, nVar
         ! extract_fluid_name only checks fluid names in lower case
         String  = NameVarLower_V(iVar)
         call extract_fluid_name(String,iFluid)

         ! no need to add the fluid name for the first fluid, this is needed
         ! for non fluid variables
         if (iFluid ==1) then
            NameFluid = ''
         else
            NameFluid = NameFluid_I(iFluid)
         end if

         ! capitalize the first letter (to make the name look better)
         String(1:1) = char(ichar(String(1:1)) + ichar('A')-ichar('a'))

         ! set the three strings to NameFluid+String by default, which might be
         ! overwritten later
         NameConservative  = trim(NameFluid)//trim(String)
         NamePrimitive     = trim(NameFluid)//trim(String)
         NamePrimitivePlot = trim(NameFluid)//trim(String)
         NamePrimitiveNT   = trim(NameFluid)//trim(String)

         select case(String)
         case('Rho')
            NamePrimitiveNT   = trim(NameFluid)//'NumDens'
         case('Mx')
            NamePrimitive     = trim(NameFluid)//'Ux'
            NamePrimitivePlot = trim(NameFluid)//'Ux'
            NamePrimitiveNT   = trim(NameFluid)//'Ux'
         case('My')
            NamePrimitive     = trim(NameFluid)//'Uy'
            NamePrimitivePlot = trim(NameFluid)//'Uy'
            NamePrimitiveNT   = trim(NameFluid)//'Uy'
         case('Mz')
            NamePrimitive     = trim(NameFluid)//'Uz'
            NamePrimitivePlot = trim(NameFluid)//'Uz'
            NamePrimitiveNT   = trim(NameFluid)//'Uz'
         case('Ppar')
            ! for anisotropic pressure, the primitive var is Ppar, while
            ! typically Pperp is added for plotting purpose
            NamePrimitivePlot = trim(NameFluid)//'Ppar '// &
                 trim(NameFluid)//'Pperp'
         case('P')
            NameConservative  = trim(NameFluid)//'E'
            NamePrimitiveNT   = trim(NameFluid)//'Temperature'
         case('Pe')
            NamePrimitiveNT   = 'Te'
         case('Hype')
            ! also tries to make HypE look better.
            NameConservative  = trim(NameFluid)//'HypE'
            NamePrimitive     = trim(NameFluid)//'HypE'
            NamePrimitivePlot = trim(NameFluid)//'HypE'
            NamePrimitiveNT   = trim(NameFluid)//'HypE'
         case('Pepar')
            ! for anisotropic Pe, the primitive var is PpPpar, while
            ! typically Peperp is added for plotting purpose
            NamePrimitivePlot = trim(NameFluid)//'Pepar '// &
                 trim(NameFluid)//'Peperp '
         end select

         ! NamePrimitive is the primitive variable names without any additional
         ! variable names (i.e. Pperp) for plotting purposes
         NamePrimitive_V(iVar)   = trim(NamePrimitive)
         NamePrimitiveNT_V(iVar) = trim(NamePrimitiveNT)

         ! overwrite the wave vars, trying to be consistent with previous
         ! equation files
         if (iVar >= WaveFirst_ .and. iVar <= WaveLast_ .and. WaveLast_ >1)then
            if (iVar == WaveFirst_) then
               NameConservative = 'Ew'
               write(NamePrimitivePlot,'(a,i2.2,a)') 'I(', nWave,')'
            else
               NameConservative  = ''
               NamePrimitivePlot = ''
            end if
         end if

         ! overwrite the material vars, trying to be consistent with previous
         ! equation files
         if (iVar >= MaterialFirst_ .and. iVar <= MaterialLast_ .and. &
              MaterialLast_ > 1) then
            if (iVar == MaterialFirst_) then
               NameConservative = ''
               write(NamePrimitivePlot,'(a,i1.1,a)') 'M(', nMaterial,')'
            else
               NameConservative  = ''
               NamePrimitivePlot = ''
            end if
         end if

         ! Overwrite the charge state vars, trying to be consistent with
         ! previous equation files
         do iElement = 1, nElement
            if (iVar >= ChargeStateFirst_+sum(nChargeState_I(1:iElement-1))&
                 .and. iVar <= ChargeStateFirst_ + &
                 sum(nChargeState_I(1:iElement))-1 .and. &
                 ChargeStateLast_ > 1) then
               if(iVar == ChargeStateFirst_+sum(nChargeState_I(1:iElement-1)))&
                    then
                  if(nChargeState_I(iElement) < 10) then
                     write(NamePrimitivePlot,'(a,i1.1,a)')&
                          trim(NameElement_I(iElement))//'(', &
                          nChargeState_I(iElement),')'
                  else
                     write(NamePrimitivePlot,'(a,i2.2,a)')&
                          trim(NameElement_I(iElement))//'(',&
                          nChargeState_I(iElement),')'
                  end if
                  NameConservative = NamePrimitivePlot
               else
                  NameConservative  = ''
                  NamePrimitivePlot = ''
               end if
            end if
         end do

         ! only add to string if it is not an empty string
         if (len_trim(NameConservative) >0) &
              StringConservative  = trim(StringConservative) //' '//  &
              trim(NameConservative)
         if (len_trim(NamePrimitivePlot) > 0)   &
              StringPrimitivePlot = trim(StringPrimitivePlot) //' '// &
              trim(NamePrimitivePlot)
      end do

      ! Combine the string array into a space separated list of nVar primitive
      ! variables
      call join_string(nVar, NamePrimitive_V, StringPrimitiveOrig)

      if(allocated(NameConservativeVarPlot))deallocate(NameConservativeVarPlot)
      if(allocated(NamePrimitiveVarPlot))   deallocate(NamePrimitiveVarPlot)
      if(allocated(NamePrimitiveVarOrig))   deallocate(NamePrimitiveVarOrig)

      ! The first character of StringConservative/StringPrimitivePlot is
      ! a space...
      lConservative = len_trim(StringConservative)-1
      lPrimitive    = len_trim(StringPrimitivePlot)-1

      allocate(character(lConservative) ::NameConservativeVarPlot)
      allocate(character(lPrimitive)    ::NamePrimitiveVarPlot)

      ! The first character of StringPrimitiveOrig is not a space
      allocate(character(len_trim(StringPrimitiveOrig)):: NamePrimitiveVarOrig)

      ! Remove leading/tailing spaces
      NameConservativeVarPlot = adjustl(trim(StringConservative))
      NamePrimitiveVarPlot    = adjustl(trim(StringPrimitivePlot))

      ! Remove tailing spaces
      NamePrimitiveVarOrig    = trim(StringPrimitiveOrig)

      !$acc update device(NameVar_V)

    end subroutine set_namevar
    !==========================================================================
    subroutine set_defaults

      use ModSemiImplVar, ONLY: UseSemiImplicit, TypeSemiImplicit
      use ModSemiImplicit, ONLY: SemiParam
      !------------------------------------------------------------------------
      NamePlotDir(1:2) = NameThisComp ! fix default plot dir name

      UseOuterHelio = index(NameUserFile,'OuterHelio') > 0

      ! Set defaults for restart files
      call init_mod_restart_file

      ! Default coordinate systems
      select case(NameThisComp)
      case('IH', 'OH')
         UseRotatingFrame  = .false.
         UseRotatingBc     = .false.
         TypeCoordSystem   = 'HGI'
      case('SC', 'EE')
         UseRotatingFrame  = .true.
         UseRotatingBc     = .false.
         TypeCoordSystem   = 'HGR'
      case('GM')
         UseRotatingFrame  = .false.
         UseRotatingBc     = .true.
         TypeCoordSystem   = 'GSM'
      end select

      ! Do not set B0 field in IH and OH
      if(NameThisComp /= 'IH' .and. NameThisComp /= 'OH')then
         UseB0 = UseB
      else
         UseB0 = .false.
      end if

      ! Do not update B0 except in GM
      if(NameThisComp /= 'GM')then
         DoUpdateB0 = .false.
         DtUpdateB0 = -1.0
      end if

      ! Initialize StartTime to the default values
      ! For SWMF it is set during 'CHECK'
      if(IsStandAlone)call time_int_to_real(iStartTime_I, StartTime)

      nStage        = 2
      Cfl           = 0.8
      CflOrig       = 0.8
      UseDtFixed    = .false.
      UseDtLimit    = .false.
      Dt            = 0.0

      nOrder = 2
      TypeFlux = 'RUSANOV'

      ! Default implicit parameters
      UseImplicit      = .false.
      TypeImplCrit     = 'dt'

      UseDivbSource   =  UseB .and. nDim > 1
      UseDivbDiffusion= .false.
      UseProjection   = .false.
      UseConstrainB   = .false.

      UseB0Source     = UseB0 .and. nDim > 1

      IsDimensionalPlot_I = .true.

      IsRestart = .false.

      ! Give some "reasonable" default values

      DipoleStrengthSi = 0.0

      UseBody2 = .false.
      rBody2 =-1.0
      xBody2 = 0.0
      yBody2 = 0.0
      zBody2 = 0.0

      BdpBody2_D  = 0.0
      Body2NDim   = 1.0    ! n/cc
      Body2TDim   = 10000.0! K

      call init_mod_amr

      ! Set component dependent defaults

      iDirGravity=0

      select case(NameThisComp)
      case('SC', 'IH', 'OH', 'EE')
         ! Body parameters
         UseGravity  = .true.
         Rbody       =  1.0
         Rcurrents   = -1.0
         RotPeriodSi = RotationPeriodSun
         ! Boundary Conditions
         ! Default boundary type is 'none'.
         BodyTDim_I            = 1.5E6    ! K
         BodyNDim_I(1) = 1.5E8    ! /cc  protons
         BodyNDim_I(2:nFluid) = BodyNDim_I(1)*cTiny

         ! Normalization and I/O units
         TypeNormalization     = "SOLARWIND"
         TypeIoUnit            = "HELIOSPHERIC"

         if(NameThisComp == "OH" .or. UseOuterHelio)then
            TypeNormalization = "OUTERHELIO"
            TypeIoUnit        = "OUTERHELIO"
         end if

         ! Some defaults for AWSoM and AWSoM-R
         ! If Ehot_>1, then we assume we use these models
         if(Ehot_ > 1)then
            UseHeatFluxCollisionless = .true.
            if(NameThisComp == 'SC')then
               UseHeatFluxRegion = .true.
               UseHeatConduction = .true.
               ! defaults for semi-implicit heat conduction
               UseSemiImplicit = .true.
               TypeSemiImplicit = "parcond"
               SemiParam%TypeKrylov = "GMRES"
               SemiParam%ErrorMax = 1.0e-5
               SemiParam%MaxMatvec = 20
               SemiParam%nKrylovVector = SemiParam%MaxMatvec
            end if
         end if

         ! Set anisotropic pressure instability defaults for first ion fluid
         if(NameThisComp == 'SC' .or. NameThisComp == 'IH')then
            if(UseAnisoPressure)then
               UseConstantTau_I(1) = .false.
               TauInstabilitySi_I(1) = -1.0
               TauGlobalSi_I(1) = 1e5
            end if
         end if

      case('GM')
         ! Body Parameters
         UseGravity = .false.

         ! Boundary Conditions and Normalization
         ! Default BC type is 'none'.

         BodyTDim_I    = 25000.0          ! K
         BodyNDim_I    = 5.0              ! /cc

         ! Normalization and I/O units
         TypeNormalization = "PLANETARY"
         TypeIoUnit        = "PLANETARY"

      end select

      TypeFaceBc_I(SolidBc_) = 'reflectall'

    end subroutine set_defaults
    !==========================================================================
    subroutine correct_parameters

      use ModMultiFluid, ONLY: UseMultiIon
      use ModWaves, ONLY: UseAlfvenWaves, UseWavePressure
      use ModRestartFile, ONLY: NameVarRestart_V
      use ModFieldLineThread, ONLY: DoPlotThreads
      use ModMain, ONLY: iTypeCellBc_I
      use ModCellBoundary, ONLY: NameCellBc_I, nTypeBC, UnknownBC_
      use CON_planet, ONLY: UseOrbitElements

      ! option and module parameters
      character (len=40) :: Name
      real               :: Version
      logical            :: IsOn

      integer :: iTypeBC
      real    :: BetaProlongOrig = 0.0
      logical :: IsFirstCheck = .true.
      character(len(NameVarRestart_V)) :: NameVarTemp_V(150) = ''
      ! Check for some combinations of things that cannot be accepted as input
      !------------------------------------------------------------------------
      if (iProc==0) write (*,*) ' '

      if(IsFirstCheck)then
         call correct_grid_geometry

         if(UseConstrainB) then
            ! Extend face index range in the orthogonal direction
            ! The CT scheme needs 1 extra layers
            iMinFace = 0; iMaxFace = nI+1
            jMinFace = 1 - min(1,nJ-1); jMaxFace = nJ + min(1,nJ-1)
            kMinFace = 1 - min(1,nK-1); kMaxFace = nK + min(1,nK-1)

            iMinFace2 = iMinFace; iMaxFace2 = iMaxFace
            jMinFace2 = jMinFace; jMaxFace2 = jMaxFace
            kMinFace2 = kMinFace; kMaxFace2 = kMaxFace

         end if
      else if(nOrderOld /=5 .and. nOrder==5 .and. UseFDFaceFlux) then
         ! calculate high-order geometry coefficients
         call set_high_geometry(UseFDFaceFluxIn=UseFDFaceFlux)
         call create_grid
         nOrderOld = 5
      endif ! IsFirstCheck

      ! Get the number of used ghost cell layers
      select case(nOrder)
      case(1, 2)
         nGUsed = nOrder
      case(5)
         ! MP5 scheme needs 3 ghost cell layers
         nGUsed = 3
      case default
         if(iProc==0)then
            write(*,*)'ERROR: nOrder=', nOrder
            call stop_mpi(NameSub//': Invalid value for nOrder')
         end if
      end select

      ! Check if there are enough ghost cells
      ! We could dynamically (re)allocate State_VGB etc. with nGUsed ?!
      if(nGUsed /= nG .and. iProc==0)then
         write(*,*)'The code is configured with nG=', nG,' ghost cell layers.'
         write(*,*)'The selected scheme requires nGUsed=', nGUsed,' layers!'
         if(nGUsed > nG)then
            write(*,*)'Either change settings or reconfigure and recompile!'
            call stop_mpi(NameSub//': insufficient number of ghost cells')
         end if
      end if

      ! This depends on the grid geometry set above
      if(i_line_command("#SAVEPLOT") > 0) call correct_plot_range

      if(UseTiming)then
         call timing_version(IsOn,Name,Version)
         if(.not.IsOn)then
            if(iProc==0)then
               write(*,'(a)')NameSub//' WARNING: TIMING module is OFF !!!'
               if(UseStrict)call stop_mpi( &
                    'Correct PARAM.in or switch TIMING ON')
               write(*,*)'setting UseTiming=.false.'
            end if
            UseTiming=.false.
         end if
      end if

      ! Check flux type selection
      select case(TypeFlux)
      case('SIMPLE', 'Simple')
         TypeFlux='Simple'
      case('ROE', 'Roe')
         TypeFlux='Roe'
      case('ROEOLD', 'RoeOld')
         TypeFlux='RoeOld'
      case('RUSANOV', 'TVDLF', 'Rusanov')
         TypeFlux='Rusanov'
      case('LINDE', 'Linde', 'HLLEL', 'HLLE')
         TypeFlux='Linde'
      case('SOKOLOV', 'AW', 'Sokolov')
         TypeFlux='Sokolov'
      case('GODUNOV', 'Godunov')
         TypeFlux='Godunov'
      case('HLLD',  'HLLDW', 'LFDW', 'HLLC')
      case default
         if(iProc==0)then
            write(*,'(a)')NameSub // &
                 'WARNING: unknown value for TypeFlux=' // trim(TypeFlux)
            if(UseStrict) &
                 call stop_mpi('Correct PARAM.in!')
            write(*,*)'setting TypeFlux=Rusanov'
         end if
         TypeFlux='Rusanov'
      end select

      ! Set flux type for neutral fluids
      select case(TypeFluxNeutral)
      case('default')
         select case(TypeFlux)
         case('Rusanov', 'Linde', 'Sokolov', 'Godunov', 'HLLDW', 'LFDW', &
              'HLLC')
            TypeFluxNeutral = TypeFlux
         case default
            TypeFluxNeutral = 'Linde'
         end select
      case('RUSANOV', 'TVDLF', 'Rusanov')
         TypeFluxNeutral = 'Rusanov'
      case('LINDE', 'HLLE', 'Linde')
         TypeFluxNeutral = 'Linde'
      case('SOKOLOV', 'AW', 'Sokolov')
         TypeFluxNeutral = 'Sokolov'
      case('GODUNOV', 'Godunov')
         TypeFluxNeutral = 'Godunov'
      case('HLLDW',  'LFDW', 'HLLC')
      case default
         if(iProc==0)then
            write(*,'(a)')NameSub// &
                 ' WARNING: Unknown value for TypeFluxNeutral='// &
                 trim(TypeFluxNeutral)//' !!!'
            if(UseStrict)call stop_mpi('Correct PARAM.in!')
            write(*,*)NameSub//' setting TypeFluxNeutral=Linde'
         end if
         TypeFluxNeutral = 'Linde'
      end select

      ! Check flux type selection for implicit
      select case(TypeFluxImpl)
      case('default')
         TypeFluxImpl = TypeFlux
      case('ROE', 'Roe')
         TypeFluxImpl='Roe'
      case('ROEOLD', 'RoeOld')
         TypeFluxImpl='RoeOld'
      case('RUSANOV', 'TVDLF', 'Rusanov')
         TypeFluxImpl='Rusanov'
      case('LINDE', 'HLLEL', 'Linde')
         TypeFluxImpl='Linde'
      case('SOKOLOV', 'AW', 'Sokolov')
         TypeFluxImpl='Sokolov'
      case('GODUNOV', 'Godunov')
         TypeFluxImpl='Godunov'
      case('HLLD',  'HLLDW', 'LFDW', 'HLLC')
      case default
         if(iProc==0)then
            write(*,'(a)')NameSub// &
                 ' WARNING: Unknown value for TypeFluxImpl='// &
                 trim(TypeFluxImpl)//' !!!'
            if(UseStrict)call stop_mpi('Correct PARAM.in!')
            write(*,*)NameSub//' setting TypeFluxImpl=', trim(TypeFlux)
         end if
         TypeFluxImpl=TypeFlux
      end select

      ! Check flux types
      if( (TypeFlux(1:3)=='Roe' .or. TypeFluxImpl(1:3)=='Roe' .or. &
           TypeFlux=='HLLD' .or.  TypeFluxImpl=='HLLD') .and. &
           (UseMultiIon .or. UseAlfvenWaves .or. UseWavePressure &
           .or. .not.UseB) )then
         if (iProc == 0) then
            write(*,'(a)')NameSub//' WARNING: '// &
                 'HLLD/Roe(Old) flux only works for single fluid MHD !!!'
            if(UseStrict)call stop_mpi('Correct PARAM.in!')
            write(*,*)NameSub//' Setting TypeFlux(Impl) = Linde'
         end if
         if(TypeFlux(1:3)=='Roe' .or. TypeFlux=='HLLD') &
              TypeFlux     = 'Linde'
         if(TypeFluxImpl(1:3)=='Roe' .or. TypeFluxImpl=='HLLD') &
              TypeFluxImpl = 'Linde'
      end if

      if((TypeFlux=='Godunov' .or. TypeFluxImpl=='Godunov') &
           .and. UseB)then
         if (iProc == 0) then
            write(*,'(a)')NameSub//&
                 ' WARNING: Godunov flux is only implemented for HD !!!'
            if(UseStrict)call stop_mpi('Correct PARAM.in!')
            write(*,*)NameSub//' Setting TypeFlux(Impl) = Linde'
         end if
         if(TypeFlux=='Godunov')     TypeFlux     = 'Linde'
         if(TypeFluxImpl=='Godunov') TypeFluxImpl = 'Linde'
      end if

      if(i_line_command("#IMPLSTEP") < 0) &
           UseBdf2 = nStage > 1 .and. IsTimeAccurate

      ! Make sure periodic boundary conditions are symmetric
      do i=Coord1MinBc_,Coord3MinBc_,2
         if(any(TypeCellBc_I(i:i+1) == 'periodic')) &
              TypeCellBc_I(i:i+1) = 'periodic'
      end do

      ! Find the integer of the corresponding boundary type.
      do i = Coord1MinBc_, Coord3MaxBc_
         iTypeCellBc_I(i) = UnknownBC_
         do iTypeBC = 1, nTypeBC
            if(TypeCellBc_I(i) == trim(NameCellBc_I(iTypeBC))) then
               iTypeCellBc_I(i) = iTypeBC
               EXIT
            end if
         end do
      end do

      ! Set UseBufferGrid logical
      UseBufferGrid = any(TypeFaceBc_I=='buffergrid')

      if(UseConstrainB .and. .not.IsTimeAccurate)then
         if(iProc==0)then
            write(*,'(a)')NameSub//&
                 ' WARNING: constrain_b works for time accurate run only !!!'
            if(UseStrict)call stop_mpi('Correct PARAM.in!')
            write(*,*)NameSub//' setting UseConstrainB=F UseDivbSource=T'
         end if
         UseConstrainB=.false.
         UseDivbSource=.true.
      end if
      if(UseConstrainB .and. (UseTvdReschange .or. UseAccurateResChange))then
         if(iProc==0)write(*,'(a)')NameSub//&
              ' WARNING: cannot use TVD or accurate schemes at res. change'// &
              ' with ConstrainB'
         ! if(UseStrict)call stop_mpi('Correct PARAM.in!')
         if(iProc==0)write(*,*)NameSub// &
              ' setting UseTvdReschange=F UseAccurateResChange=F'
         UseTvdReschange      = .false.
         UseAccurateResChange = .false.
      end if
      if (UseConstrainB) TypeMessagePass = 'all'
      if (UseConstrainB .and. AdaptGrid % DoThis)then
         if(iProc==0)write(*,'(a)')NameSub//&
              ' WARNING: cannot use AMR with constrained transport'
         if(UseStrict)call stop_mpi('Correct PARAM.in!')
         if(iProc==0)write(*,*)NameSub//' setting DoAmr=F'
         AdaptGrid % DoThis = .false.
      end if

      if (UseHallResist .or. UseResistivity .or. UseViscosity) &
           TypeMessagePass = 'all'

      if (UseRadDiffusion .and. (UseFullImplicit .or. UseSemiImplicit)) &
           TypeMessagePass = 'all'

      ! Check for magnetogram

      if(nOrder == 1)then
         BetaProlongOrig = BetaProlong
         BetaProlong = 0.0
      else
         BetaProlong = max(BetaProlong, BetaProlongOrig)
      end if

      if(nK == 1 .and. UseTvdResChange) then
         ! in 1D and 2D only accurate reschange is implemented
         UseTvdResChange      = .false.
         UseAccurateResChange = .true.
      end if

      if(UseAccurateResChange) TypeMessagePass = 'all'

      if(IsRzGeometry .and. UseB)then
         if(UseMultiIon) &
              call stop_mpi('RZ geometry is not implemented for multi-ion')
         if(UseHallResist) &
              call stop_mpi('RZ geometry is not implemented for Hall-MHD')
      end if

      if(.not.IsCartesian)then
         if(UseConstrainB)call stop_mpi(&
              'Only Cartesian works with constrain B')
         if(UseDivBDiffusion)call stop_mpi(&
              'Only Cartesian works with divB diffusion')
      end if

      if(UseHyperbolicDivb)then
         if(UseDivbDiffusion .or. UseConstrainB .or. UseProjection )then
            if(iProc==0)then
               write(*,'(a)')NameSub// &
                    ' WARNING: Hyperbolic cleaning can only be combined', &
                    ' with 8-wave scheme, not any other div B scheme'
               if (UseStrict) call stop_mpi('Correct PARAM.in!')
               write(*,*)NameSub//' Setting UseHyperbolicDivb = .false.'
            end if
            UseHyperbolicDivb = .false.
         end if
      end if

      ! Check CFL number
      if(.not.IsTimeAccurate .and. iProc==0)then
         if(UseBorisCorrection)then
            if(Cfl > 0.65) then
               write(*,'(a)')NameSub// &
                    ' WARNING: CFL number above 0.65 may be unstable'// &
                    ' for local timestepping with Boris correction !!!'
               if (UseStrict) call stop_mpi('Correct PARAM.in!')
            end if
         else
            if(Cfl > 0.85)then
               write(*,'(a)')NameSub// &
                    ' WARNING: CFL number above 0.85 may be unstable'// &
                    ' for local timestepping !!!'
               if (UseStrict) call stop_mpi('Correct PARAM.in!')
            end if
         end if
      end if

      ! Boris correction checks
      if((TypeFlux(1:3)=='Roe' .or. TypeFluxImpl(1:3)=='Roe') &
           .and. UseBorisCorrection)then
         if (iProc == 0) then
            write(*,'(a)')NameSub//&
                 ' WARNING: Boris correction not available for Roe flux !!!'
            if(UseStrict)call stop_mpi('Correct PARAM.in!')
            write(*,*)NameSub//' Setting UseBoris = .false.'
         end if
         UseBorisCorrection = .false.
         ClightFactor = 1.0
      end if

      if(UseBorisCorrection .and. UseHallResist .and. &
           TypeSemiImplicit /= 'resisthall' .and. &
           TypeSemiImplicit /= 'resistivity')then
         if(iProc==0)then
            write(*,'(a)') NameSub//' WARNING: '//&
                 'Boris correction only works with semi-implicit Hall MHD!!!'
            if (UseStrict) call stop_mpi('Correct PARAM.in!')
            write(*,*)NameSub//' Setting UseBoris = .false.'
         end if
         UseBorisCorrection = .false.
         ClightFactor = 1.0
      end if

      ! Check parameters for implicit
      if(UseImplicit .and. UseSemiImplicit .and. nStage > 1)then
         if(iProc==0)write(*,'(a)') NameSub//&
              ' WARNING: Full/part implicit + semi implicit schemes with'//&
              ' nStage=2 (BDF2) scheme is inconsistent if there is overlap!!!'
         if(UseStrict)then
            if(iProc == 0)write(*,*)NameSub//' setting nStage=1'
            nStage = 1
         endif
      end if

      if(UseClimit .and. .not. UseImplicit)then
         if(iProc==0)then
            write(*,'(a)')'UseClimit=T requires (part) implicit scheme'
            call stop_mpi('Correct PARAM.in')
         end if
      end if

      if(UsePartImplicit .and. TypeImplCrit=='dt' .and.&
           (.not.IsTimeAccurate .or. .not.UseDtFixed))then
         if(iProc==0)then
            write(*,'(a)')'Part implicit scheme with TypeImplCrit=dt'
            write(*,'(a)')'requires time accurate run with fixed time step'
            call stop_mpi('Correct PARAM.in')
         end if
      end if

      if(.not.IsTimeAccurate .and. UseDtFixed)then
         if(iProc==0)then
            write(*,'(a)')'Steady state Run can not use fixed time step'
            write(*,'(a)')'Use limited time step instead'
            call stop_mpi('Correct PARAM.in')
         end if
      end if

      if(UseDtLimit .and. UseDtFixed)then
         if(iProc==0)then
            write(*,'(a)') &
                 'Limited and fixed time steps cannot be used together'
            call stop_mpi('Correct PARAM.in')
         end if
      end if

      if(.not.IsTimeAccurate.and.UseBDF2)then
         if(iProc==0)then
            write(*,'(a)') NameSub//&
                 ' WARNING: BDF2 is only available for time accurate run !!!'
            if(UseStrict)call stop_mpi('Correct PARAM.in!')
            write(*,*)NameSub//' setting UseBDF2=.false.'
         end if
         UseBDF2=.false.
      end if

      if(UseImplicit .and. UsePartSteady)then
         if(iProc==0)then
            write(*,'(a)') NameSub//&
                 ' WARNING: Implicit and part steady schemes do not'//&
                 ' work together !!!'
            if(UseStrict)call stop_mpi('Correct PARAM.in!')
            write(*,*) NameSub//' setting UsePartSteady=F'
         endif
         UsePartSteady = .false.
      end if

      if(UsePointImplicit .and. UseFullImplicit)then
         if(iProc==0)then
            write(*,'(a)') NameSub//&
                 ' WARNING: Full and point implicit schemes do not'//&
                 ' work together !!!'
            if(UseStrict)call stop_mpi('Correct PARAM.in!')
            write(*,*) NameSub//' setting UsePointImplicit=F'
         endif
         UsePointImplicit = .false.
      end if

      ! Finish checks for implicit

      ! Set min_block_level and max_block_level for #AMRRESOLUTION in session 1
      if(i_line_command("#AMRRESOLUTION", iSessionIn=iSessionFirst) > 0) &
           call fix_amr_limits( &
           (XyzMax_D(x_)-XyzMin_D(x_))/real(nRootRead_D(1)*nI))

      if(TypeGeometry == 'cartesian')then
         if(DoFixAxis)then
            DoFixAxis = .false.
            if(iProc == 0) write(*,*) NameSub// &
                 ' setting DoFixAxis to FALSE'
         end if
      end if

      if(DoFixAxis .and. .not. UseUniformAxis) &
           call stop_mpi("DoFixAxis=T only works with UseUniformAxis=T!")

      if(UseLocalTimeStep)then
         if(DoFixAxis)then
            if(iProc == 0)then
               write(*,'(a)') NameSub//&
                    ' WARNING: Subcycling and fix axis are incompatible!'
               if(UseStrict)call stop_mpi('Correct PARAM.in!')
               write(*,*) NameSub//' setting DoFixAxis to false'
            end if
            DoFixAxis = .false.
         end if

         if(nStage > 2)then
            if(iProc == 0)then
               write(*,'(a)') NameSub//' WARNING: ' &
                    //'Subcycling works with 1 and 2 stage schemes only!'
               if(UseStrict)call stop_mpi('Correct PARAM.in!')
               write(*,*) NameSub//' setting nStage=2'
            end if
            nStage = 2
         end if
      end if

      if(TypeCoordSystem == 'HGI' .and. (NameThisComp /= 'OH' &
           .and. .not.UseOuterHelio) &
           .and. .not.IsTimeAccurate)then
         if(iProc == 0)then
            write(*,'(a)') NameSub//&
                 ' WARNING: there is no steady state solution in HGI system!'
            if(UseStrict)call stop_mpi('Correct PARAM.in!')
            write(*,*) NameSub//' setting TypeCoordSystem = HGC'
         end if
         TypeCoordSystem  = 'HGC'
         iSignRotationIC = 0
         UseRotatingFrame = .true.
      end if

      ! Get the integer representation of the coordinate system
      do i = 1, nCoordSystem
         if(TypeCoordSystem == trim(NameCoordSystem_I(i))) then
            iTypeCoordSystem = i
         endif
      end do

      ! Update check does not work with Runge-Kutta explicit schemes
      ! because the final update is a linear combination of all stages.
      if(.not.UseHalfStep .and. .not.UseImplicit) UseUpdateCheck = .false.

      ! Use first order prolongation for the first stage of high
      ! resolution change.
      if(UseHighResChange) nOrderProlong = 1

      ! If the state variables are changed when restarting, the names of the
      ! state variables saved in the restart file must be specified, so they
      ! can be properly copied into State_VGB.
      if(DoChangeRestartVariables .and. .not. IsReadNameVarRestart &
           .and. NameEquationRead /= NameEquation) then
         if(iProc == 0)then
            write(*,'(a)') NameSub//&
                 ' : Could not find  #RESTARTVARIABLES command, needed '// &
                 'when changing state variables at restart.'
         end if
         call stop_mpi('Correct PARAM.in')
      end if

      ! Check that the number of variables listed in #RESTARTVARIABLES
      ! matches the number appearing in the #EQUATION command
      ! (this check is useful if the #RESTARTVARIABLES command is added
      ! manually to older restart header files, but might become redundant
      ! in the future).
      if(IsReadNameVarRestart) then
         call split_string(NameVarRestartRead, NameVarTemp_V, nVarRestart)
         if (nVarRestart /= nVarEquationRead) then
            write(*,*)'Number of variables in #EQUATION command differs'//&
                 ' from the number of variables listed in #RESTARTVARIABLES.'
            call stop_mpi(NameSub//': Correct PARAM.in file!')
         else
            ! Save array of variable names
            if (allocated(NameVarRestart_V)) deallocate(NameVarRestart_V)
            allocate(NameVarRestart_V(nVarRestart))
            NameVarRestart_V = NameVarTemp_V(1:nVarRestart)
         end if
      end if

      UseHighOrderAMR = UseHighOrderAMR .and. AdaptGrid % DoThis
      IsFirstCheck = .false.

      if (.not. allocated(iVarUseCmax_I) .and. UseEfield) then
         allocate(iVarUseCmax_I(4))
         ! The electric field should diffuse with Cmax by default in the five
         ! moment simulation.
         iVarUseCmax_I = [Ex_, Ey_, Ez_, HypE_]
      end if

      if (UseEfield .and. UseHyperbolicDivb .and. ClightDim > 0 &
           .and. SpeedHypDim > ClightDim) then
         SpeedHypDim = ClightDim
         if (iProc ==0)  write(*,*) NameSub //                &
              ' SpeedHypDim is larger than cLightDim. '//     &
              'Reduce SpeedHypDim to ClightDimSpeedHypDim =', &
              SpeedHypDim, ' ClightDim =', ClightDim, ''
      end if

      if (UseAnisoPe .and. .not. UseAnisoPressure)  call stop_mpi(NameSub//  &
           ' UseAnisoPe cannot be applied without UseAnisoPressure')

      if (UseAnisoPe .and. UseHallResist) call stop_mpi(NameSub// &
           ': UseAnisoPe is not implemented for Hall Mhd.')

      if (UseAnisoPe .and. UseAlfvenWaveDissipation) call stop_mpi(NameSub// &
           ' AnisoPe for coronal heating is not implemented yet')

      if (UseAnisoPe .and. UseRadCooling) call stop_mpi(NameSub// &
           ' AnisoPe for radiative cooling is not implemented yet')

      ! Fix NameSat_I if needed
      ! NameSat_I = 'none'  ! Disabled
      ! Another trajectory file from the
      ! database is used (venus, mars, mercury)
      do iSat = 1, nSatellite
         if (index(NameFileSat_I(iSat), 'sta') > 0 .or.  &
                 index(NameFileSat_I(iSat), 'stereoa') > 0) &
                 NameSat_I(iSat) = 'sta'
         if (index(NameFileSat_I(iSat), 'stb') > 0 .or.  &
                 index(NameFileSat_I(iSat), 'stereob') > 0) &
                 NameSat_I(iSat) = 'stb'
         if (index(NameFileSat_I(iSat), 'earth') > 0)    &
                 NameSat_I(iSat) = 'earth'
      end do

      ! stop the code if there are two stereo a/b, earth traj files
      if (sum(index(NameSat_I, 'sta')) > 1) &
           call stop_mpi(NameSub//' two stereo A traj files???')
      if (sum(index(NameSat_I, 'stb')) > 1) &
           call stop_mpi(NameSub//' two stereo B traj files???')
      if (sum(index(NameSat_I, 'earth')) > 1) &
           call stop_mpi(NameSub//' two earth traj files???')

      ! stop the code if no stereo a/b, earth traj files are found when
      ! using 'sta'/'stb'/'earth' in TypeSatPos_I
      if (.not.all(TypeSatPos_I /= 'sta') .and. all(NameSat_I /= 'sta')) &
           call stop_mpi(NameSub//' missing stereo A traj file.')
      if (.not.all(TypeSatPos_I /= 'stb') .and. all(NameSat_I /= 'stb')) &
           call stop_mpi(NameSub//' missing stereo B traj file.')
      if (.not.all(TypeSatPos_I /= 'earth') .and. all(NameSat_I /= 'earth')) &
           call stop_mpi(NameSub//' missing earth traj file.')

      ! Set coordinate system string to lower case for rotated HGR and HGI/HGC
      if(dLongitudeHgr /= 0 .and. TypeCoordSystem == 'HGR') &
           TypeCoordSystem = 'hgr'

      if(dLongitudeHgi /= 0 .and. TypeCoordSystem == 'HGI') &
           TypeCoordSystem = 'hgi'

      if(dLongitudeHgi /= 0 .and. TypeCoordSystem == 'HGC') &
           TypeCoordSystem = 'hgc'

      ! Disable thread-related logicals, if UseFieldLineThreads=.false.
      if(.not.UseFieldLineThreads)then
         DoPlotThreads = .false.; DoThreadRestart = .false.
      end if

      ! Adaptive field reversal is not compatible with flux correction (yet)
      if(DoReverseField) DoConserveFlux = .false.

#ifdef _OPENACC
      iTypeUpdate = UpdateFast_ ! Default for GPU
#else
      select case(TypeUpdate)
      case('slow')
         iTypeUpdate = UpdateSlow_
      case('fast')
         iTypeUpdate = UpdateFast_
      case default
         iTypeUpdate = UpdateOrig_
      end select
#endif
      if(iTypeUpdate /= UpdateOrig_)then
         ! These methods are not implemented in ModUpdateStateFast (yet)
         ! If fast (or compatible slow) update is used
         ! then the following features are swiched off

         UseDbTrick            = .false.
         UseDivFullBSource     = .false.
         UseBorisRegion        = .false.
         DoLimitMomentum       = .false.
         DoConserveFlux        = .false.
         UseSimpleProlongation = .true.
         if(UseTvdResChange)then
            UseTvdResChange      = .false.
            UseAccurateResChange = .true.
         end if
         ! The one type fails for nvfortran
         TypeRestartOutFile = 'proc'
      end if

      UseDbTrickNow = UseDbTrick
      if(.not.IsMhd .or. (UseNonConservative .and. nConservCrit == 0) .or. &
           (nStage == 1 .and. IsTimeAccurate) .or. .not.UseHalfStep) &
           UseDbTrickNow = .false.

      if(UseBody2Orbit)then
         if(.not.UseOrbitElements)then
            if(iProc == 0)then
               write(*,'(a)') NameSub//' WARNING: ' &
                    //'Orbit elements are not availlable for second body orbit'
               if(UseStrict)call stop_mpi('Correct PARAM.in!')
            end if
            UseBody2Orbit = .false.
         end if
      end if

      if(UseChargedParticles)then
         if(.not.IsTimeAccurate)then
            if(iProc==0)write(*,*)'To trace particles, use time-accurate!'
            call stop_mpi('Correct parameter file!!')
         end if
         if(.not.UseB)then
            if(iProc==0)write(*,*)'To trace particles use MHD!'
            call stop_mpi('Correct parameter file!!')
         end if
         if(IsMhd .and. UseHybrid)then
            if(iProc==0)write(*,*)'Single fluid MHD cannot be hybrid'
            call stop_mpi('Correct parameter file or set IsMhd=.false.')
         end if
         if(UseHybrid.and..not.UseFlic)then
            if(iProc==0) &
                 write(*,*) NameSub,': UseHybrid=.true., set UseFlic=.true.'
         end if
         call normalize_particle_param
      end if
      if(UseFlic .and. nStage /= 3)then
         if(iProc==0) &
              write(*,*) NameSub,': UseFlic=.true., set nStage to 3'
         nStage = 3
      end if

      ! No need to use MPIIO if running on 1 core
      if(nProc == 1) DoSaveOneIdlFile = .false.

      ! Update parameters on the GPU that are not done by init_mod_* routines

      !$acc update device(MaxBlock)
      !$acc update device(nOrder, nStage, nOrderProlong)
      !$acc update device(UseHalfStep, IsTimeAccurate, UseDtFixed)
      !$acc update device(DoCorrectFace, UseFDFaceFlux)
      !$acc update device(UseTvdResChange, UseAccurateResChange)
      !$acc update device(TypeLimiter, LimiterBeta)
      !$acc update device(TypeCellBc_I, iTypeCellBc_I, UseBufferGrid)
      !$acc update device(UseOutflowPressure)
      !$acc update device(UseB0, UseB0Source)
      !$acc update device(UseDivbSource, UseHyperbolicDivb, UseConstrainB)
      !$acc update device(DoConserveNeutrals, UseNonConservative, nConservCrit)
      !$acc update device(iMinFace, iMaxFace, iMinFace2, iMaxFace2)
      !$acc update device(jMinFace, jMaxFace, jMinFace2, jMaxFace2)
      !$acc update device(kMinFace, kMaxFace, kMinFace2, kMaxFace2)
      !$acc update device(UseUserUpdateStates)
      !$acc update device(Gamma_I, GammaMinus1_I, InvGammaMinus1_I)
      !$acc update device(Gamma, GammaMinus1, InvGammaMinus1)
      !$acc update device(GammaElectron, GammaElectronMinus1)
      !$acc update device(InvGammaElectronMinus1)
      !$acc update device(GammaWave)
      !$acc update device(UseBody)
      !$acc update device(UseRotatingBc)
      !$acc update device(UseGravity, UseRotatingFrame)
      !$acc update device(iTypeCoordSystem)
      !$acc update device(DipoleStrengthSi)
      !$acc update device(DoCoupleImPressure, DoCoupleImDensity, TauCoupleIM)
      !$acc update device(DoFixPolarRegion, rFixPolarRegion, dLatSmoothIm)
      !$acc update device(DoAnisoPressureIMCoupling, DoMultiFluidIMCoupling)
      !$acc update device(UseElectronEntropy, UseElectronEnergy)
      !$acc update device(DoUpdate_V)
      !$acc update device(StartTime)
      !$acc update device(AverageIonCharge, PePerPtotal)
      !$acc update device(UseHeatFluxCollisionless)
      !$acc update device(UseHeatFluxRegion, UseHeatConduction)
      !$acc update device(TypeSemiImplicit)
      !$acc update device(rLocalTimeStep)

    end subroutine correct_parameters
    !==========================================================================
    subroutine correct_grid_geometry

      use ModGeometry, ONLY: LogRGen_I
      use BATL_lib, ONLY: init_batl, CoordMin_D, CoordMax_D, &
           IsRotatedCartesian

      character(len=20):: TypeGeometryBatl
      character(len=20) :: TypeCellBcTmp_D(nDim)='none'

      character(len=*), parameter:: NameSub = 'correct_grid_geometry'
      !------------------------------------------------------------------------
      if(i_line_command("#GRID", iSessionIn=iSessionFirst) < 0) &
           call stop_mpi(NameSub // &
           ' #GRID command must be specified in the first session!')

      if(  i_line_command("#OUTERBOUNDARY", iSessionIn=iSessionFirst)<0 .and. &
           i_line_command("#BOXBOUNDARY", iSessionIn=iSessionFirst) < 0 ) &
           call stop_mpi(NameSub // &
           ' #OUTERBOUNDARY or #BOXBOUNDARY command must be specified &
           &in the first session!')

      if(product(nRootRead_D) > MaxBlock*nProc .and. iProc==0)then
         write(*,*)'Not enough grid blocks allocated for root blocks'
         write(*,*)'Number of root blocks=', product(nRootRead_D)
         write(*,*)'MaxBlock, nProc, MaxBlock*nProc=', &
              MaxBlock, nProc, MaxBlock*nProc
         call stop_mpi(NameSub//': insufficient number of grid blocks')
      end if

      ! Set XyzMin_D, XyzMax_D based on
      ! #GRID, #GRIDGEOMETRY, and #LIMITRADIUS
      ! #GRIDGEOMETRYLIMIT already sets XyzMin_D, XyzMax_D so that it does not
      ! have to be reset here
      if(.not.i_line_command("#GRIDGEOMETRYLIMIT", &
           iSessionIn=iSessionFirst) > 0) then
         select case(TypeGeometry)
         case('cartesian', 'rotatedcartesian')
            XyzMin_D = [xMinBox, yMinBox, zMinBox]
            XyzMax_D = [xMaxBox, yMaxBox, zMaxBox]
         case('rz')
            zMinBox = -0.5
            zMaxBox = +0.5
            XyzMin_D = [xMinBox, yMinBox, zMinBox]
            XyzMax_D = [xMaxBox, yMaxBox, zMaxBox]
         case('spherical', 'spherical_lnr', 'spherical_genr')
            !             R,   Phi, Latitude
            XyzMin_D = [ 0.0, 0.0, -cHalfPi]
            XyzMax_D = [ &
                 sqrt(max(xMinBox**2,xMaxBox**2)   &
                 +    max(yMinBox**2,yMaxBox**2)   &
                 +    max(zMinBox**2,zMaxBox**2)), &
                 cTwoPi, cHalfPi ]
         case('cylindrical', 'cylindrical_lnr', 'cylindrical_genr')
            !            R,   Phi, Z
            XyzMin_D = [0.0, 0.0, zMinBox]
            XyzMax_D = [sqrt(max(xMinBox**2,xMaxBox**2) &
                 +           max(yMinBox**2,yMaxBox**2)), cTwoPi, zMaxBox]
         case('roundcube')
            if(rRound0 > rRound1)then
               ! Cartesian outside, so use xMinBox..zMaxBox
               XyzMin_D = [xMinBox, yMinBox, zMinBox]
               XyzMax_D = [xMaxBox, yMaxBox, zMaxBox]
            else
               ! Round outside, so fit this inside xMinBox..zMaxBox
               if(nDim==2) XyzMax_D = min( &
                    abs(xMinBox), abs(xMaxBox), &
                    abs(yMinBox), abs(yMaxBox)) &
                    /sqrt(2.0)
               if(nDim==3) XyzMax_D = min( &
                    abs(xMinBox), abs(xMaxBox), &
                    abs(yMinBox), abs(yMaxBox), &
                    abs(zMinBox), abs(zMaxBox)) &
                    /sqrt(3.0)
               XyzMin_D = -XyzMax_D
            end if

         case default
            call stop_mpi(NameSub//': unknown TypeGeometry='//TypeGeometry)
         end select

         if(i_line_command("#LIMITRADIUS", iSessionIn=iSessionFirst) > 0) then
            XyzMin_D(1) = RadiusMin
            XyzMax_D(1) = RadiusMax
            if(TypeGeometry == 'roundcube' .and. rRound1 > rRound0) &
                 XyzMax_D = RadiusMax/sqrt(real(nDim))
         else
            if(UseBody .and. rBody > 0.0)then
               ! Set inner boundary to match rBody for spherical coordinates
               if(TypeGeometry(1:3)=='sph' .or. TypeGeometry(1:3)=='cyl') &
                    XyzMin_D(1) = rBody
            end if
            RadiusMin = XyzMin_D(1)
            RadiusMax = XyzMax_D(1)
         end if
      end if

      if(index(TypeGeometry,'_genr') < 1) call set_gen_radial_grid

      ! Initialize BATL
      if(TypeGeometry(1:9)=='spherical') then
         TypeGeometryBatl = 'rlonlat'//TypeGeometry(10:20)
      else
         TypeGeometryBatl = TypeGeometry
      end if

      ! PGI can not handle passing
      ! "TypeCellBc_I(1:2*nDim-1:2) == 'periodic' " as an argument.
      TypeCellBcTmp_D = TypeCellBc_I(1:2*nDim-1:2)

      call init_batl(XyzMin_D(1:nDim), XyzMax_D(1:nDim), MaxBlock, &
           TypeGeometryBatl, TypeCellBcTmp_D == 'periodic', &
           nRootRead_D(1:nDim), UseRadiusIn=.true., UseDegreeIn=.false.,&
           RgenIn_I = exp(LogRGen_I), UseUniformAxisIn=UseUniformAxis,&
           UseFDFaceFluxIn=UseFDFaceFlux, iVectorVarIn_I=iVectorVar_I)

      if(IsRotatedCartesian)then
         ! Fix xMinBox, xMaxBox .. zMaxBox to include the full rotated domain
         xMaxBox = sum(abs(CoordMin_D)) + sum(abs(CoordMax_D))
         yMaxBox = xMaxBox
         zMaxBox = xMaxBox
         xMinBox = -xMaxBox
         yMinBox = xMinBox
         zMinBox = xMinBox
      end if

      if(IsLogRadius .or. IsGenRadius)then
         ! Overwrite radial coordinates if necessary
         XyzMin_D(1) = CoordMin_D(1)
         XyzMax_D(1) = CoordMax_D(1)
      end if

      ! Fix grid size in ignored directions
      if(nDim == 1)then
         yMinBox = -0.5; XyzMin_D(2) = -0.5
         yMaxBox = +0.5; XyzMax_D(2) = +0.5
      end if
      if(nDim < 3)then
         zMinBox = -0.5; XyzMin_D(3) = -0.5
         zMaxBox = +0.5; XyzMax_D(3) = +0.5
      end if

    end subroutine correct_grid_geometry
    !==========================================================================
    subroutine correct_plot_range

      use BATL_lib, ONLY: radius_to_gen, Phi_, Theta_, nRoot_D, &
           CoordMin_D, CoordMax_D, nIJK_D
      use ModKind, ONLY: nByteReal
      use ModWritePlot, ONLY: adjust_plot_range
      use ModIO

      integer :: iFile
      real    :: Ratio, PlotRes_D(3)

      real    :: CellSizeMax_D(3), Cut
      real    :: SmallSize_D(3)   ! Used for 2D plot areas

      logical:: DoTest
      character(len=*), parameter:: NameSub = 'correct_plot_range'
      !------------------------------------------------------------------------
      call test_start(NameSub, DoTest)

      if(DoTest)write(*,*) NameSub,' CoordMin_D, CoordMax_D=', &
           CoordMin_D, CoordMax_D

      ! Largest cell size and a much smaller distance for 2D cuts
      CellSizeMax_D = (CoordMax_D - CoordMin_D)/(nIJK_D*nRoot_D)

      if(DoTest)write(*,*)NameSub,' CellSizeMax_D=', CellSizeMax_D

      PLOTFILELOOP: do iFile = Plot_+1, Plot_ + nPlotFile

         TypePlotArea = TypePlot_I(iFile)(1:3)

         if(TypePlotFormat_I(iFile) == 'tcp')then
            SmallSize_D = 0.0
         elseif(nByteReal == 8)then
            SmallSize_D   = 1e-9*CellSizeMax_D
         else
            SmallSize_D   = 1e-6*CellSizeMax_D
         end if

         if(DoTest)write(*,*)'iFile, TypePlotFormat_I, TypePlotArea=',&
              iFile, TypePlotFormat_I(iFile), TypePlotArea

         ! Fix plot range for various plot areas
         select case(TypePlotArea)
         case('shl', 'sln','slg','shk','box', 'eqb', 'eqr', 'lcb','los')
            ! These plot areas read all ranges from PARAM.in
            CYCLE PLOTFILELOOP
         case('cut')
            if(IsLogRadius) PlotRange_EI(1:2,iFile) = &
                 log(PlotRange_EI(1:2,iFile))
            if(IsGenRadius) then
               call radius_to_gen(PlotRange_EI(1,iFile))
               call radius_to_gen(PlotRange_EI(2,iFile))
            end if
            if(Phi_ > 0) PlotRange_EI(2*Phi_-1:2*Phi_,iFile) = &
                 cDegToRad*PlotRange_EI(2*Phi_-1:2*Phi_,iFile)
            if(Theta_ > 0) PlotRange_EI(2*Theta_-1:2*Theta_,iFile) = &
                 cDegToRad*PlotRange_EI(2*Theta_-1:2*Theta_,iFile)
            do iDim = 1, nDim
               if(  PlotRange_EI(2*iDim-1,iFile) &
                    < PlotRange_EI(2*iDim,iFile)) CYCLE
               Cut =0.5*(PlotRange_EI(2*iDim-1,iFile) &
                    +    PlotRange_EI(2*iDim,iFile))
               PlotRange_EI(2*iDim-1,iFile) = Cut - SmallSize_D(iDim)
               PlotRange_EI(2*iDim,iFile)   = Cut + SmallSize_D(iDim)
            end do
         case('sph')
            if(IsCartesianGrid)then
               PlotDx_DI(1,iFile) = 1.0    ! set to match write_plot_sph
               PlotDx_DI(2:3,iFile) = 1.0  ! angular resolution in degrees
               PlotRange_EI(2,iFile) = PlotRange_EI(1,iFile) + 1.e-4 ! so R/=0
               PlotRange_EI(3,iFile)= 0.   - 0.5*PlotDx_DI(2,iFile)
               PlotRange_EI(4,iFile)= 90.0 + 0.5*PlotDx_DI(2,iFile)
               PlotRange_EI(5,iFile)= 0.   - 0.5*PlotDx_DI(3,iFile)
               PlotRange_EI(6,iFile)= 360.0- 0.5*PlotDx_DI(3,iFile)
            elseif(IsRLonLat)then
               PlotDx_DI(1,iFile) = -1.0
               if(IsLogRadius) &
                    PlotRange_EI(1,iFile) = log(PlotRange_EI(1,iFile))
               if(IsGenRadius) call radius_to_gen(PlotRange_EI(1,iFile))
               PlotRange_EI(2,iFile)= PlotRange_EI(1,iFile) + 1.e-4 ! so R/=0
               do i=Phi_,Theta_
                  PlotRange_EI(2*i-1,iFile) = CoordMin_D(i)
                  PlotRange_EI(2*i,iFile)   = CoordMax_D(i)
               end do
               TypePlotArea='r=r' ! to disable the write_plot_sph routine
            else
               call stop_mpi(NameSub// &
                    ' Sph-plot is not implemented for geometry= '&
                    //TypeGeometry)
            end if

            ! There is nothing else to do for sph area
            CYCLE PLOTFILELOOP

         case('x=0')
            PlotRange_EI(1:5:2, iFile) = CoordMin_D
            PlotRange_EI(2:6:2, iFile) = CoordMax_D
            if( TypePlotFormat_I(iFile)=='tec' .or. &
                 IsCartesianGrid .or. IsRoundCube )then
               ! Limit plot range along x direction to be very small
               PlotRange_EI(1, iFile) = -SmallSize_D(x_)
               PlotRange_EI(2, iFile) = +SmallSize_D(x_)
               if(IsRlonLat) then
                  PlotRange_EI(1, iFile) = CoordMin_D(x_)
                  PlotRange_EI(2, iFile) = CoordMin_D(x_) + SmallSize_D(x_)
               end if
            else
               ! Limit Phi direction around cHalfPi
               PlotRange_EI(3, iFile) = cHalfPi - SmallSize_D(Phi_)
               PlotRange_EI(4, iFile) = cHalfPi + SmallSize_D(Phi_)
            end if

         case('y=0')
            PlotRange_EI(1:5:2, iFile) = CoordMin_D
            PlotRange_EI(2:6:2, iFile) = CoordMax_D
            if( (TypePlotFormat_I(iFile) == 'idl' .or. &
                 TypePlotFormat_I(iFile) == 'tcp') &
                 .and. (IsRLonLat .or. IsCylindrical) &
                 .and. CoordMin_D(2) < cPi .and. CoordMax_D(2) > cPi) then

               ! Limit plot range in Phi direction to be small around 180 degs
               PlotRange_EI(3, iFile) = cPi - SmallSize_D(y_)
               PlotRange_EI(4, iFile) = cPi + SmallSize_D(y_)
            else
               ! Limit plot range along y (or Phi) direction to be very small
               PlotRange_EI(3, iFile) = -SmallSize_D(y_)
               PlotRange_EI(4, iFile) = +SmallSize_D(y_)
            end if

         case('z=0', '2d_')
            ! Limit plot range along z direction to be very small
            PlotRange_EI(1:5:2, iFile) = CoordMin_D
            PlotRange_EI(2:6:2, iFile) = CoordMax_D
            PlotRange_EI(5, iFile) = -SmallSize_D(z_)
            PlotRange_EI(6, iFile) = +SmallSize_D(z_)

         case('1d_')
            ! Limit plot range along 2nd and 3rd directions to be very small
            PlotRange_EI(1, iFile) = CoordMin_D(1)
            PlotRange_EI(2, iFile) = CoordMax_D(1)
            PlotRange_EI(3:5:2, iFile) = -SmallSize_D(y_:z_)
            PlotRange_EI(4:6:2, iFile) = +SmallSize_D(y_:z_)

         case('3d_', '3D_')
            PlotRange_EI(1:5:2, iFile) = CoordMin_D
            PlotRange_EI(2:6:2, iFile) = CoordMax_D

         end select

         ! Reduce plot range in ignored dimensions
         if(nJ == 1) PlotRange_EI(3,iFile) = -SmallSize_D(y_)
         if(nJ == 1) PlotRange_EI(4,iFile) = +SmallSize_D(y_)
         if(nK == 1) PlotRange_EI(5,iFile) = -SmallSize_D(z_)
         if(nK == 1) PlotRange_EI(6,iFile) = +SmallSize_D(z_)

         if(DoTest)write(*,*)'For file ', iFile-plot_,&
              ' original range   =', PlotRange_EI(:,iFile)

         PlotRange_EI(1:5:2, iFile) = &
              max(PlotRange_EI(1:5:2, iFile), CoordMin_D)
         PlotRange_EI(2:6:2, iFile) = &
              min(PlotRange_EI(2:6:2, iFile), CoordMax_D)

         if(DoTest)write(*,*)'For file ', iFile-plot_,&
              ' limited range   =', PlotRange_EI(:,iFile)

         ! For PlotDx_DI = 0.0 or -1.0 there is no need to adjust cut range
         if(PlotDx_DI(1, iFile) <= cTiny)then

            PlotDx_DI(:, iFile) = PlotDx_DI(1, iFile) ! Define y and z

            CYCLE PLOTFILELOOP

         end if

         ! Make sure that plot resolution is a power of 2 fraction of cell size
         Ratio     = CellSizeMax_D(x_)/PlotDx_DI(1, iFile)
         Ratio     = 2.0**nint(log(Ratio)/log(2.0))
         PlotRes_D = CellSizeMax_D / Ratio
         PlotDx_DI(1:3, iFile) = PlotRes_D

         ! Make sure that plotting range is placed at an integer multiple of dx
         call adjust_plot_range(PlotRes_D(1), PlotRange_EI(:,iFile))
         if(DoTest)write(*,*)'For file ', iFile-plot_,&
              ' adjusted range   =', PlotRange_EI(:,iFile)

      end do PLOTFILELOOP

      call test_stop(NameSub, DoTest)
    end subroutine correct_plot_range
    !==========================================================================
    subroutine set_extra_parameters

      use ModMultiFluid, ONLY: UseMultiIon
      use ModTimeStepControl, ONLY: UseTimeStepControl,TimeStepControlInit

      integer:: l
      character(len=500):: StringParam
      !------------------------------------------------------------------------
      ! We need normalization for dt
      if(UseDtFixed)then
         DtFixed     = DtFixedDim * Io2No_V(UnitT_)
         DtFixedOrig = DtFixed   ! Store the initial setting
         Dt          = DtFixed
         Cfl         = 1.0       ! UseDtFixed only works with time accurate
      end if

      if(UseDtLimit .or. UseLocalTimeStep)then
         DtLimit     = DtLimitDim * Io2No_V(UnitT_)
         ! Store the initial setting
         DtLimitOrig = DtLimit
         ! Dt = 0 in steady state
         if(IsTimeAccurate .and. UseDtLimit) Dt  = DtLimit
      end if

      if(UseTimeStepControl)then
         ! Reduce initial time step / Cfl number.
         ! The original values are stored in DtFixedOrig and CflOrig
         if(UseDtFixed)then
            DtFixed = TimeStepControlInit*DtFixed
         else if(UseDtLimit) then
            DtLimit = TimeStepControlInit*DtLimit
         else
            Cfl     = TimeStepControlInit*Cfl
         end if
         TimeStepControlInit = 1.0
      else
         if(UseDtFixed)then
            DtFixed = DtFixedOrig
         else if(UseDtLimit) then
            DtLimit = DtLimitOrig
         else
            Cfl     = CflOrig
         end if
      end if

      DoOneCoarserLayer = .not. (nOrder > 1 .and. &
           (UseTvdResChange .or. UseAccurateResChange))
      if(UseHighResChange) DoOneCoarserLayer = .false.

      ! Set DoLimitMomentum for UpdateOrig_ unless #LIMITMOMENTUM command
      ! is present in the first session
      if(i_line_command("#LIMITMOMENTUM", iSessionIn=iSessionFirst) < 0 &
           .and. iTypeUpdate == UpdateOrig_) &
           DoLimitMomentum = UseBorisCorrection .and. DoOneCoarserLayer

      ! Momentum limiting is not available/needed for multiion MHD
      if(DoLimitMomentum .and. UseMultiIon)then
         if(iProc==0) write(*,*) NameSub,': setting DoLimitMomentum=F'
         DoLimitMomentum = .false.
      end if

      ! Set default scalar parameters for plot files
      do iFile = plot_+1, plot_+nPlotFile
         l = index(StringPlotParam_I(iFile), '{default}')
         if(l < 1) CYCLE

         ! Set the name of default scalar parameters for plotting
         StringParam = ''
         if(IsDimensionalPlot_I(iFile))then
            if(Io2Si_V(UnitX_)   /= 1)StringParam = 'xSI'
            if(Io2Si_V(UnitT_)   /= 1)StringParam = trim(StringParam)//' tSI'
         else
            if(No2Si_V(UnitX_)   /= 1)StringParam = 'xSI'
            if(No2Si_V(UnitT_)   /= 1)StringParam = trim(StringParam)//' tSI'
            if(No2Si_V(UnitRho_) /= 1)StringParam = trim(StringParam)//' rhoSI'
         end if
         if(rBody > 0) StringParam = trim(StringParam)//' r'
         if(any(Gamma_I /= Gamma))then
            do iFluid = 1, nFluid
               write(StringParam,'(a,i1)') trim(StringParam)//' g', iFluid
            end do
         else
            StringParam = trim(StringParam)//' g'
         end if
         if(GammaElectron /= Gamma) StringParam = trim(StringParam)//' ge'
         if(any(MassFluid_I /= 1))then
            do iFluid = 1, nFluid
               if(UseEfield .and. iFluid == nIonFluid)then
                  ! Last fluid is assumed to be the electrons
                  StringParam = trim(StringParam)//' me'
               else
                  write(StringParam,'(a,i1)') trim(StringParam)//' m', iFluid
               end if
            end do
         end if
         if(any(ChargeIon_I /= 1))then
            do iFluid = 1, nIonFluid
               if(UseEfield .and. iFluid == nIonFluid)then
                  ! Last fluid is assumed to be the electrons
                  StringParam = trim(StringParam)//' qe'
               elseif(iFluid <= 9)then
                  write(StringParam,'(a,i1)') trim(StringParam)//' q', iFluid
               else
                  write(StringParam,'(a,i2)') trim(StringParam)//' q', iFluid
               end if
            end do
         end if

         if (ClightFactor /=1 .or. ClightDim > 0) &
              StringParam = trim(StringParam)//' clight'

         ! Replace '{default}' with StringParam
         StringPlotParam = StringPlotParam_I(iFile)
         StringPlotParam_I(iFile) = &
              StringPlotParam(1:l-1)//trim(adjustl(StringParam))// &
              StringPlotParam(l+9:len_trim(StringPlotParam))
      end do

      !$acc update device(Dt, DtFixed, Cfl)

    end subroutine set_extra_parameters
    !==========================================================================
    subroutine sort_smooth_indicator

      ! The variables using the same smooth indicator should be
      ! calculated one by one. The smooth indicator
      ! itself is calculated first.
      ! iVarSmoothIndex_I is the calculation order.

      real:: RealIVarSmooth_V(nVar)
      integer:: iVar
      !------------------------------------------------------------------------
      do iVar = 1, nVar
         if(iVarSmooth_V(iVar) == iVar) then
            RealIVarSmooth_V(iVar) = real(iVar) - 0.5
         else
            RealIVarSmooth_V(iVar) = real(iVarSmooth_V(iVar))
         endif
      enddo

      call sort_quick(nVar,RealIVarSmooth_V,iVarSmoothIndex_I)

    end subroutine sort_smooth_indicator
    !==========================================================================
  end subroutine set_parameters
  !============================================================================
end module ModSetParameters
!==============================================================================
