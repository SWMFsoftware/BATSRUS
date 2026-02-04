!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModBatsrusMethods

  ! This module contains the top level methods for BATSRUS

  use BATL_lib, ONLY: test_start, test_stop, lVerbose,&
       iProc
  use ModUpdateStateFast, ONLY: sync_cpu_gpu
  use ModBatsrusUtility, ONLY: stop_mpi

  implicit none

  private ! except

  public:: BATS_init_session
  public:: BATS_setup
  public:: BATS_advance      ! advance solution by one time step
  public:: BATS_save_files   ! save output and/or restart files
  public:: BATS_finalize     ! final save, close files, deallocate

contains
  !============================================================================
  subroutine BATS_setup

    use ModMpi
    use ModMain
    use ModConstrainDivB, ONLY: DoInitConstrainB
    use ModIO
    use ModAMR, ONLY: nRefineLevelIC
    use ModAdvance, ONLY: iTypeAdvance_B, iTypeAdvance_BP, ExplBlock_
    use ModParallel, ONLY: init_mod_parallel
    use ModWriteProgress, ONLY: write_progress, write_runtime_values
    use BATL_lib, ONLY: find_test_cell, iProc, iComm

    ! Local variables

    integer :: iError
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'BATS_setup'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Allocate and initialize variables dependent on number of PEs
    call init_mod_parallel

    if(.not.IsStandAlone)call write_progress

    call grid_setup   ! restart reads info integer only

    call set_initial_conditions ! restart reads all real data

    call find_test_cell

    call initialize_files

    call MPI_BARRIER(iComm,iError) ! ----------- BARRIER ------

    call write_runtime_values

    IsRestartCoupler = IsRestart
    IsRestart = .false.
    DoThreadRestart = .false.
    call test_stop(NameSub, DoTest)

  contains
    !==========================================================================
    subroutine grid_setup

      ! Set up problem geometry, blocks, and grid structure.

      use ModIO, ONLY: IsRestart
      use ModRestartFile, ONLY: NameRestartInDir, &
           UseRestartInSeries, string_append_iter

      use ModMain, ONLY: nIteration, TypeCoordSystem
      use BATL_lib, ONLY: init_grid_batl, read_tree_file,set_amr_criteria,&
           set_amr_geometry, nBlock, Unused_B, init_amr_criteria, &
           nInitialAmrLevel
      use BATL_region, ONLY: Area_I, nArea
      use ModBatlInterface, ONLY: set_batsrus_grid
      ! Dummy variables, to avoid array size issues with State_VGB in
      ! set_amr_criteria
      use ModAdvance, ONLY: nVar, State_VGB
      use ModLoadBalance, ONLY: load_balance
      use ModAMR, ONLY: DoSetAmrLimits, set_amr_limits
      use CON_axes, ONLY: transform_matrix, dLongitudeHgr, dLongitudeHgi
      use ModNumConst, ONLY: cTwoPi
      use ModCoordTransform, ONLY: xyz_to_lonlat, rot_matrix_y, rot_matrix_z
      use ModSatelliteFile, ONLY: xyz_sat
      ! local variables

      character(len=*), parameter :: NameSubSub = NameSub//'::grid_setup'
      character(len=100) :: NameFile

      integer:: iBlock, iArea
      character(len=3):: TypeCoordTmp
      real :: Dir_D(3), Longitude, Latitude
      !------------------------------------------------------------------------
      call set_batsrus_grid

      ! adjust if Area_I(iArea)%TypeCoordIn == hgr/hgi/GSE
      do iArea = 1, nArea
         if(Area_I(iArea)%NameSat/='')then
            ! Rotation of the shape to align x-axis with the direction
            ! toward a satellite
            Dir_D = xyz_sat(trim(Area_I(iArea)%NameSat))
            call xyz_to_lonlat(Dir_D, Longitude, Latitude)
            Area_I(iArea)%DoRotate = .true.
            Area_I(iArea)%Rotate_DD =matmul(rot_matrix_z(-Longitude),&
                 rot_matrix_y(Latitude))
         end if
         TypeCoordTmp = Area_I(iArea)%TypeCoordIn
         if (TypeCoordTmp == '' .or. TypeCoordTmp == TypeCoordSystem) CYCLE

         if(TypeCoordTmp == 'HGR' .and. TypeCoordSystem == 'hgr') then
            ! If TypeCoordIn is hgr
            if (Area_I(iArea)%NameShape == 'brick_coord') then
               ! if NameShape is 'brick_coord' adjust the Longitude for the box
               Area_I(iArea)%Center_D(Phi_) = &
                    Area_I(iArea)%Center_D(Phi_) - dLongitudeHgr
               ! If Center_D < 0, then Center_D += 2*Pi
               if (Area_I(iArea)%Center_D(Phi_) < 0) &
                    Area_I(iArea)%Center_D(Phi_) =   &
                    Area_I(iArea)%Center_D(Phi_) + cTwoPi
               ! Center_D is within 0 and 2*Pi
               Area_I(iArea)%Center_D(Phi_) = modulo( &
                    Area_I(iArea)%Center_D(Phi_), cTwoPi)
            else
               call stop_mpi(NameSub//': HGR can only be added to brick_coord')
            end if
         elseif(TypeCoordTmp == 'HGI' .and. TypeCoordSystem == 'hgi') then
            ! If TypeCoordIn is hgi
            if (Area_I(iArea)%NameShape == 'brick_coord') then
               ! if NameShape is 'brick_coord' adjust the Longitude for the box
               Area_I(iArea)%Center_D(Phi_) = &
                    Area_I(iArea)%Center_D(Phi_) - dLongitudeHgi
               ! If Center_D < 0, then Center_D += 2*Pi
               if (Area_I(iArea)%Center_D(Phi_) < 0) &
                    Area_I(iArea)%Center_D(Phi_) =   &
                    Area_I(iArea)%Center_D(Phi_) + cTwoPi
               ! Center_D is within 0 and 2*Pi
               Area_I(iArea)%Center_D(Phi_) = modulo( &
                    Area_I(iArea)%Center_D(Phi_), cTwoPi)
            else
               call stop_mpi(NameSub//': HGI can only be added to brick_coord')
            end if
         elseif(TypeCoordTmp == 'GSE') then
            ! Rotate if TypeCoordIn == GSE
            Area_I(iArea)%DoRotate = .true.
            Area_I(iArea)%Rotate_DD = transform_matrix(tSimulation, &
                 TypeCoordSystem, TypeCoordTmp)
         else
            call stop_mpi(NameSub//': TypeCoordIn, TypeCoordSystem ='// &
                 TypeCoordTmp//' '//TypeCoordSystem)
         end if
      end do

      if (.not.IsRestart) then
         ! Create initial solution block geometry.
         ! Set all arrays for AMR
         call init_amr_criteria

         ! Perform initial refinement of mesh and solution blocks.
         do nRefineLevel = 1, nInitialAmrLevel

            if (iProc == 0 .and. lVerbose > 0) then
               call write_prefix; write (iUnitOut,*) NameSub, &
                    ' starting initial refinement level, nBlockAll =', &
                    nRefineLevel, nBlockAll
            end if
            call set_amr_criteria(nVar, State_VGB,TypeAmrIn='geo')
            call init_grid_batl
            call set_batsrus_grid
            ! need to update node information, maybe not all
            ! of load balancing
            do iBlock = 1, nBlock
               if(Unused_B(iBlock)) CYCLE
               call set_amr_geometry(iBlock)
            end do
         end do
      else
         ! Read initial solution block geometry from octree restart file.

         NameFile = trim(NameRestartInDir)//'octree.rst'
         if (UseRestartInSeries) &
              call string_append_iter(NameFile,nIteration)
         call read_tree_file(NameFile)
         call init_grid_batl
         call set_batsrus_grid

         ! Set all arrays for AMR
         call init_amr_criteria

      end if

      ! Set initial block types
      where(.not.Unused_B) iTypeAdvance_B = ExplBlock_
      call MPI_ALLGATHER(iTypeAdvance_B, MaxBlock, MPI_INTEGER, &
           iTypeAdvance_BP, MaxBlock, MPI_INTEGER, iComm, iError)

      ! Move coordinates around except for restart because the
      ! coordinate info is in the .rst files and not in the octree (1st).
      ! Do not move the data, it is not yet set. There are new blocks.
      call load_balance(DoMoveCoord=.not.IsRestart, DoMoveData=.false., &
           IsNewBlock=.true.)
      do iBlock = 1, nBlock
         if(Unused_B(iBlock)) CYCLE
         call set_amr_geometry(iBlock)
      end do

      if (iProc == 0.and.lVerbose>0)then
         call write_prefix; write (iUnitOut,*) '    total blocks = ',nBlockALL
      end if

      nRefineLevel = nInitialAmrLevel

      if(DoSetAmrLimits) call set_amr_limits
    end subroutine grid_setup
    !==========================================================================
    subroutine set_initial_conditions

      ! Set intial conditions for solution in each block.

      use ModSetInitialCondition, ONLY: set_initial_condition, &
           add_rotational_velocity
      use ModIO, ONLY: IsRestart, DoRestartBface
      use ModRestartFile, ONLY: read_restart_files
      use ModMessagePass, ONLY: exchange_messages
      use ModMain, ONLY: UseB0, iSignRotationIC
      use ModAdvance, ONLY: State_VGB
      use ModBuffer, ONLY: DoRestartBuffer
      use ModB0, ONLY: set_b0_reschange, B0_DGB
      use ModFieldLineThread, ONLY: UseFieldLineThreads, set_threads
      use ModAMR, ONLY: prepare_amr, do_amr, &
           DoSetAmrLimits, set_amr_limits
      use ModLoadBalance, ONLY: load_balance
      use ModParticleMover, ONLY: UseHybrid, get_vdf_from_state, &
           get_state_from_vdf, trace_particles

      use ModUserInterface ! user_initial_perturbation, user_action

      integer :: iLevel, iBlock
      character(len=*), parameter :: NameSubSub = &
           NameSub//'::set_initial_conditions'
      !------------------------------------------------------------------------
      if(.not.IsRestart .and. nRefineLevelIC > 0)then
         call timing_start('amr_ics')
         do iLevel=1, nRefineLevelIC
            call timing_start('amr_ics_set')
            do iBlock = 1, nBlockMax
               call set_initial_condition(iBlock)
            end do
            call timing_stop('amr_ics_set')

            ! Allow the user to add a perturbation and use that
            ! for physical refinement.
            if(UseUserPerturbation)then
               call timing_start('amr_ics_perturb')
               call user_initial_perturbation
               call timing_stop('amr_ics_perturb')
            end if

            call timing_start('amr_ics_amr')

            ! Do physics based AMR with the message passing
            call prepare_amr(DoFullMessagePass=.true., TypeAmr='phy')
            call do_amr

            call timing_stop('amr_ics_amr')
         end do

         ! Move coordinates, but not data (?), there are new blocks
         call timing_start('amr_ics_balance')
         call load_balance(DoMoveCoord=.true.,DoMoveData=.false., &
              IsNewBlock=.true.)

         call timing_stop('amr_ics_balance')

         call timing_stop('amr_ics')
      end if
      ! nRefineLevelIC has done its work, reset to zero
      nRefineLevelIC = 0

      ! Read initial data from restart files as necessary.
      if(IsRestart)then
         call user_action('reading restart files')
         call read_restart_files
         ! Transform velocities from a rotating system to the HGI system
         ! if required: add/subtract rho*(omega x r) to/from the momentum
         ! of all fluids for all blocks
         if(iSignRotationIC /= 0)call add_rotational_velocity(iSignRotationIC)
      end if

      ! Potentially useful for applying "First Touch" strategy
!!$ omp parallel do
      do iBlock = 1, nBlockMax
         ! Initialize solution blocks
         call set_initial_condition(iBlock)
         ! Initialize the VDFs for the hybrid particles
         if(UseHybrid.and.(.not.Unused_B(iBlock)))&
              call get_vdf_from_state(iBlock,DoOnScratch = .True.)
      end do
!!$ omp end parallel do

      ! If iSignRotationIC was non-zero, the rotational velocity has been
      ! added either in applying restart, or in set_initial conditions.
      ! Therefore iSignRotation is set to true for the rotational velocity
      ! not to be double-counted in init_session
      iSignRotationIC = 0

      if(UseHybrid)then
         ! Check particles and collect their moments
         call trace_particles(Dt=0.0,DoBorisStepIn=.false.)
         ! Due to finite number of particles per cell, the state vector
         ! while randomized the particles has been modified somewhat
         call get_state_from_vdf
      end if

      call user_action('initial condition done')

      ! Allow the user to add a perturbation to the initial condition.
      if(UseUserPerturbation)then
         call user_initial_perturbation
         UseUserPerturbation = .false.
      end if

      if(IsRestart)then
         if(iProc==0)then
            call write_prefix; write(iUnitOut,*)&
                 NameSub,' restarts at nStep,tSimulation=',&
                 nStep,tSimulation
         end if
         ! Load balance for the inner blocks:
         call load_balance(DoMoveCoord=.true., DoMoveData=.true., &
              IsNewBlock=.true.)

         ! Redo the AMR level constraints for fixed body level
         ! The coordinates of the blocks are only known now
         if(DoSetAmrLimits) call set_amr_limits
      end if

      ! Fix face centered B0 at resolution changes
      if(UseB0)call set_b0_reschange
      if(UseFieldLineThreads)call set_threads(NameSub)

      ! Ensure zero divergence for the CT scheme
      if(UseConstrainB)then
         if(DoRestartBface)then
            DoInitConstrainB = .false.
         else
            call BATS_init_constrain_b
         end if
      end if

      call sync_cpu_gpu('change on CPU', NameSub, State_VGB, B0_DGB)
      call sync_cpu_gpu('update on GPU', NameSub, State_VGB, B0_DGB)

      if(DoRestartBuffer)then
         ! Apply the state on the buffer grid to fill in cells
         ! within the region covered by this grid
         call exchange_messages(UseBufferIn = .true.)
         DoRestartBuffer = .false.
      else
         call exchange_messages
      end if

    end subroutine set_initial_conditions
    !==========================================================================
    subroutine initialize_files

      use ModSatelliteFile, ONLY:

      character(len=*), parameter :: NameSubSub = NameSub//'::initialize_files'
      !------------------------------------------------------------------------
      TypePlot_I(restart_)='restart'
      TypePlot_I(logfile_)='logfile'

    end subroutine initialize_files
    !==========================================================================
  end subroutine BATS_setup
  !============================================================================
  subroutine BATS_init_session

    use ModMain, ONLY: iSignRotationIC, UseUserPerturbation, &
         UseRadDiffusion, UseHeatConduction, UseIonHeatConduction, &
         UseProjection, UseConstrainB,  UseLocalTimeStepNew
    use ModAdvance, ONLY: UseElectronPressure, UseAnisoPressure, &
         State_VGB
    use ModSetInitialCondition, ONLY: add_rotational_velocity
    use ModConstrainDivB, ONLY: DoInitConstrainB
    use ModProjectDivB, ONLY: project_divb
    use ModHallResist, ONLY: &
         UseHallResist, UseBiermannBattery, init_hall_resist
    use ModResistivity, ONLY: UseResistivity, UseHeatExchange
    use ModImplicit, ONLY: UseFullImplicit, UseSemiImplicit, TypeSemiImplicit
    use ModRadDiffusion, ONLY: init_rad_diffusion
    use ModHeatConduction, ONLY: init_heat_conduction
    use ModParticleFieldLine, ONLY: UseParticles, init_particle_line
    use ModRestartFile, ONLY: UseRestartOutSeries
    use ModMessagePass, ONLY: exchange_messages
    use ModUserInterface ! user_initial_perturbation
    use ModLoadBalance, ONLY: load_balance, select_stepping
    use ModPIC, ONLY: UsePic, pic_init_region
    use BATL_lib, ONLY: init_amr_criteria, find_test_cell, iProc
    use ModTimeStepControl, ONLY: TimeSimulationOldCheck
    use ModFieldLineThread, ONLY: UseFieldLineThreads, set_threads
    use EEE_ModCommonVariables, ONLY: UseCme

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'BATS_init_session'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    TimeSimulationOldCheck = -1e10

    if(UseLocalTimeStepNew)then
       if(iProc == 0)write(*,*) &
            NameSub,': redo load balance for time accurate local timestepping'
       ! move coordinates and data, there are no new blocks
       call load_balance(DoMoveCoord=.true., DoMoveData=.true., &
            IsNewBlock=.false.)
       UseLocalTimeStepNew = .false.
    end if

    ! Find the test cell defined by #TESTIJK or #TESTXYZ commands
    call find_test_cell

    ! Transform velocities from a rotating system to the HGI system if required
    if(iSignRotationIC /= 0)then
       ! add/subtract rho*(omega x r) to/from the momentum of all fluids
       ! for all blocks
       call add_rotational_velocity(iSignRotationIC)
       iSignRotationIC = 0
    end if
    ! Allow the user to add a perturbation to the initial condition.
    if (UseUserPerturbation) then
       call sync_cpu_gpu('update on CPU', NameSub, State_VGB)
       call user_initial_perturbation
       call sync_cpu_gpu('change on CPU', NameSub, State_VGB)

       if(UseCme .and. UseFieldLineThreads) call set_threads(NameSub)
       UseUserPerturbation=.false.
    end if

    ! Set number of explicit and implicit blocks
    ! Partially implicit/local selection will be done in each time step
    call select_stepping(DoPartSelect=.false.)

    ! Ensure zero divergence for the CT scheme
    if(UseConstrainB .and. DoInitConstrainB)&
         call BATS_init_constrain_b

    if(UseHallResist .or. UseBiermannBattery)call init_hall_resist

    if(UsePic) call pic_init_region

    if(UseHeatConduction .or. UseIonHeatConduction .or. &
         UseAnisoPressure .and. UseResistivity .and. &
         UseHeatExchange .and. UseElectronPressure)&
         call init_heat_conduction

    if(UseParticles) &
         call init_particle_line

    if(UseSemiImplicit)then
       select case(TypeSemiImplicit)
       case('radiation', 'radcond', 'cond')
          call init_rad_diffusion
       end select
    elseif(UseFullImplicit.and.UseRadDiffusion)then
       call init_rad_diffusion
    end if

    ! Make sure that ghost cells are up to date
    call exchange_messages

    if(UseProjection)call project_divb

    call BATS_save_files('INITIAL')

    ! save initial restart series
    if (UseRestartOutSeries) call BATS_save_files('restart')

    ! Set all arrays for AMR
    call init_amr_criteria

    call test_stop(NameSub, DoTest)

  end subroutine BATS_init_session
  !============================================================================
  subroutine BATS_advance(TimeSimulationLimit)

    ! Advance solution with one time step

    use ModKind
    use ModMain
    use ModIO, ONLY: iUnitOut, write_prefix, DoSavePlotsAmr, UseParcel, &
         Parcel_DI, nParcel
    use ModAMR, ONLY: AdaptGrid, DoAutoRefine, prepare_amr, do_amr
    use ModPhysics, ONLY: No2Si_V, UnitT_, IO2Si_V, UseBody2Orbit
    use ModAdvance, ONLY: UseAnisoPressure, UseElectronPressure
    use ModAdvanceExplicit, ONLY: advance_explicit, update_secondbody
    use ModAdvectPoints, ONLY: advect_all_points, advect_points
    use ModPartSteady, ONLY: UsePartSteady, IsSteadyState, &
         part_steady_select, part_steady_switch
    use ModImplicit, ONLY: UseImplicit, nStepPrev, UseSemiImplicit
    use ModSemiImplicit, ONLY: advance_semi_impl
    use ModIeCoupling, ONLY: apply_ie_velocity
    use ModImCoupling, ONLY: apply_im_pressure
    use ModTimeStepControl, ONLY: UseTimeStepControl, control_time_step,     &
         set_global_timestep, DoCheckTimeStep, DnCheckTimeStep, TimeStepMin, &
         TimeSimulationOldCheck
    use ModParticleFieldLine, ONLY: UseParticles, advect_particle_line
    use ModLaserHeating, ONLY: add_laser_heating
    use ModVarIndexes, ONLY: Te0_
    use ModMessagePass, ONLY: exchange_messages
    use ModB0, ONLY: DoUpdateB0, DtUpdateB0
    use ModResistivity, ONLY: &
         UseResistivity, UseHeatExchange, calc_heat_exchange
    use ModMultiFluid, ONLY: UseMultiIon
    use ModLocalTimeStep, ONLY: advance_localstep
    use ModPartImplicit, ONLY: advance_part_impl
    use ModHeatConduction, ONLY: calc_ei_heat_exchange
    use ModFieldLineThread, ONLY: &
         UseFieldLineThreads, advance_threads, Enthalpy_
    use ModLoadBalance, ONLY: load_balance_blocks
    use ModConservative, ONLY: IsDynamicConservCrit, IsStaticConservCrit, &
         IsConserv_CB, select_conservative
    use ModBoundaryGeometry, ONLY: fix_geometry
    use ModWriteProgress, ONLY: write_timeaccurate
    use ModUpdateState, ONLY: update_b0, update_te0, fix_anisotropy
    use ModProjectDivB, ONLY: project_divb
    use ModCleanDivB, ONLY: clean_divb
    use ModFreq, ONLY: is_time_to
    use ModPIC, ONLY: AdaptPic, calc_pic_criteria, &
         pic_set_cell_status, iPicGrid, iPicDecomposition
    use BATL_region, ONLY: Area_I, nArea
    use BATL_particles, ONLY: message_pass_particles
    use ModParticleFieldLine, ONLY: UseParticleFieldLine=>UseParticles, &
         iKindReg
    use CON_axes, ONLY: transform_matrix
    use ModCoordTransform, ONLY: xyz_to_lonlat, rot_matrix_y, rot_matrix_z
    use ModSatelliteFile, ONLY: xyz_sat

    real, intent(in):: TimeSimulationLimit ! simulation time not to be exceeded

    integer:: iArea
    character(len=3):: TypeCoordTmp
    real :: Dir_D(3), Longitude, Latitude
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'BATS_advance'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    ! Check if time limit is reached (to be safe)
    if(tSimulation >= TimeSimulationLimit) RETURN

    ! Check if steady state is achieved
    if(.not.IsTimeAccurate .and. UsePartSteady .and. IsSteadyState)then
       ! Create stop condition for stand alone mode
       nIter = nIteration
       ! There is nothing to do, simply return
       RETURN
    end if

    call timing_start('advance')

    ! We are advancing in time.
    IsTimeLoop = .true.
    !$acc update device(IsTimeLoop)

    ! Some files should be saved at the beginning of the time step
    call BATS_save_files('BEGINSTEP')

    nStep = nStep + 1
    nIteration = nIteration+1

    if(IsTimeAccurate)then
       if(UseSolidState) call fix_geometry(DoSolveSolidIn=.false.)

       call set_global_timestep(TimeSimulationLimit)

       if (DoCheckTimeStep .and. mod(nStep, DnCheckTimeStep) == 0)then
          if (tSimulation - TimeSimulationOldCheck <             &
               TimeStepMin*DnCheckTimeStep*Io2SI_V(UnitT_) ) then
             if (iProc ==0) write(*,*) NameSub,                      &
                  ': TimeSimulationOldCheck, tSimulation =',     &
                  TimeSimulationOldCheck, tSimulation
             call stop_mpi(NameSub//': advance too slow in time')
          end if
          TimeSimulationOldCheck = tSimulation
       end if

       if(UseSolidState) call fix_geometry(DoSolveSolidIn=.true.)
    end if

    ! Select block types and load balance blocks
    call load_balance_blocks

    if(UseSolidState) call fix_geometry(DoSolveSolidIn=.false.)

    ! Switch off steady blocks to reduce calculation
    if(UsePartSteady) call part_steady_switch(.true.)

    ! (Re)set conservative criteria based on current solution
    if(IsDynamicConservCrit .or. &
         IsStaticConservCrit .and. .not.allocated(IsConserv_CB)) &
         call select_conservative

    if(UseImplicit.and.nBlockImplALL>0)then
       call advance_part_impl
    elseif(UseLocalTimeStep .and. nStep > 1 .and. IsTimeAccurate)then
       call advance_localstep(TimeSimulationLimit)
    else
       call advance_explicit(DoCalcTimestep=.true.)
    endif

    ! Adjust tSimulation to match TimeSimulationLimit if it is very close
    if(IsTimeAccurate .and. &
         tSimulation < TimeSimulationLimit .and. &
         TimeSimulationLimit - tSimulation <= 1e-3*Dt*No2Si_V(UnitT_))then

       if(iProc == 0 .and. lVerbose > 0)then
          call write_prefix; write(iUnitOut,*) NameSub, &
               ': adjusting tSimulation=', tSimulation,&
               ' to TimeSimulationLimit=', TimeSimulationLimit,&
               ' with Dt=', Dt*No2Si_V(UnitT_)
       end if

       tSimulation = TimeSimulationLimit
    end if

    if(UseIM)call apply_im_pressure

    if(.not.UseMultiIon)then
       if(UseHeatConduction .and. UseElectronPressure)then
          if(.not.UseSemiImplicit)call calc_ei_heat_exchange
       elseif(UseResistivity .and. UseHeatExchange &
            .and. UseElectronPressure)then
          if(UseAnisoPressure)then
             call calc_ei_heat_exchange
          else
             call calc_heat_exchange
          end if
       end if
    end if

    if(UseAnisoPressure)call fix_anisotropy

    if(UseIE)call apply_ie_velocity

    if(UseDivBDiffusion)call clean_divb

    if(UseSolidState) call fix_geometry(DoSolveSolidIn=.true.)

    if(UseLaserHeating) call add_laser_heating

    ! Calculate temperature at the end of time step
    if(Te0_>1)call update_te0

    if(UseFieldLineThreads)call advance_threads(Enthalpy_)
    call exchange_messages

    if(UseSemiImplicit .and. (Dt>0 .or. .not.IsTimeAccurate)) &
         call advance_semi_impl

    if(UseTimeStepControl .and. IsTimeAccurate .and. Dt>0)then
       if(UseSolidState) call fix_geometry(DoSolveSolidIn=.false.)

       call control_time_step

       if(UseSolidState) call fix_geometry(DoSolveSolidIn=.true.)
    end if

    if(UsePartSteady)then
       ! Select steady and unsteady blocks
       if(.not. (IsTimeAccurate .and. tSimulation == 0.0)) &
            call part_steady_select

       ! Switch on steady blocks to be included in AMR, plotting, etc.
       call part_steady_switch(.false.)
    end if

    call advect_all_points

    if(UseParcel) call advect_points(nParcel, Parcel_DI)

    call user_action('timestep done')

    if(UseParticles) call advect_particle_line

    ! Re-calculate the active PIC regions
    if(AdaptPic % DoThis) then
       if(is_time_to(AdaptPic, nStep, tSimulation, IsTimeAccurate) &
            .or. iPicGrid /= iNewGrid &
            .or. iPicDecomposition /= iNewDecomposition) then
          if(iProc==0 .and. lVerbose > 0)then
             call write_prefix; write(iUnitOut,*) NameSub, &
                  " adapt PIC region at simulation time=", tSimulation
          end if
          call calc_pic_criteria
          call pic_set_cell_status
       end if
    end if

    if(DoTest)write(*,*)NameSub,' iProc,new nStep,tSimulation=',&
         iProc,nStep,tSimulation

    if(DoUpdateB0)then
       ! dB0/dt term is added at the DtUpdateB0 frequency

       if(int(tSimulation/DtUpdateB0) >  &
            int((tSimulation - Dt*No2Si_V(UnitT_))/DtUpdateB0)) &
            call update_b0
    end if

    if(UseBody2Orbit) then
       call update_secondbody
       call update_b0
    end if

    if(UseProjection) call project_divb

    ! Adapt grid
    if(is_time_to(AdaptGrid, nStep, tSimulation, IsTimeAccurate)) then
       call timing_start(NameThisComp//'_amr')

       ! adjust if Area_I(iArea)%TypeCoordIn == GSE, so that the cone
       ! direction can be updated
       do iArea = 1, nArea
          TypeCoordTmp = Area_I(iArea)%TypeCoordIn
          if (TypeCoordTmp == 'GSE')then

             ! Rotate if TypeCoordIn == GSE
             Area_I(iArea)%DoRotate = .true.
             Area_I(iArea)%Rotate_DD = transform_matrix(tSimulation, &
                  TypeCoordSystem, TypeCoordTmp)
          elseif(Area_I(iArea)%NameSat/='')then
             ! Rotation of the shape to align x-axis with the direction
             ! toward a satellite
             Dir_D = xyz_sat(trim(Area_I(iArea)%NameSat))
             call xyz_to_lonlat(Dir_D, Longitude, Latitude)
             Area_I(iArea)%DoRotate = .true.
             Area_I(iArea)%Rotate_DD =matmul(rot_matrix_z(-Longitude),&
                  rot_matrix_y(Latitude))
          end if
       end do

       if(iProc==0 .and. lVerbose > 0)then
          call write_prefix; write(iUnitOut,*) &
               '----------------- AMR START at nStep=', nStep
          if(IsTimeAccurate) call write_timeaccurate
       end if

       ! Increase refinement level if geometric refinement is done
       if(.not.DoAutoRefine) nRefineLevel = nRefineLevel + 1

       ! BDF2 scheme should not use previous step after AMR
       nStepPrev = -100

       ! Do AMR without full initial message passing
       call prepare_amr(DoFullMessagePass=.false., TypeAmr='all')
       if(IsTimeLoop) call BATS_save_files('BEFOREAMR')
       call do_amr

       ! Output timing after AMR.
       call timing_stop(NameThisComp//'_amr')
       if(iProc == 0 .and. lVerbose > 0)then
          call timing_show(NameThisComp//'_amr',1)
          call timing_show('load_balance',1)
          call write_prefix; write(iUnitOut,*) &
               '----------------- AMR FINISHED'
       end if

       if(UseProjection) call project_divb

       ! Write plotfiles after AMR if required
       if(DoSavePlotsAmr)call BATS_save_files('AMRPLOTS')
       if(UseParticleFieldLine)call message_pass_particles(iKindReg)
    else
       ! If AMR is done, then the plotting of BATS_save_files('NORMAL')
       ! is called in ModAMR to save the AMR criteria.
       call BATS_save_files('NORMAL')

    end if

    call timing_stop('advance')
    call test_stop(NameSub, DoTest)

  end subroutine BATS_advance
  !============================================================================
  subroutine BATS_init_constrain_b

    ! Initialize for constrained transport

    use ModConstrainDivB, ONLY: DoInitConstrainB, bcenter_to_bface
    use ModProjectDivB, ONLY: proj_get_divb, project_divb
    use ModNumConst, ONLY: cTiny
    use ModAdvance, ONLY: Bx_, Bz_, State_VGB, Tmp1_GB
    use ModIO, ONLY: write_prefix, iUnitOut
    use BATL_lib, ONLY: iProc, Xyz_DGB, x_,y_,z_, nBlock, message_pass_cell, &
         maxval_grid

    integer :: iBlock
    integer :: iLoc_I(5)  ! full location index
    real    :: DivBMax
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'BATS_init_constrain_b'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    DoInitConstrainB = .false.

    call message_pass_cell(3,State_VGB(Bx_:Bz_,:,:,:,:), nWidthIn=1, &
         nProlongOrderIn=1, DoSendCornerIn=.false., DoRestrictFaceIn=.true.)

    do iBlock=1, nBlock
       ! Estimate Bface from the centered B values
       call bcenter_to_bface(iBlock)
       ! Calculate energy (it is not set in set_initial_condition)
       ! because the projection scheme will need it
       ! !! call calc_energy(iBlock)
    end do

    call proj_get_divb(Tmp1_GB)
    DivBMax = &
         maxval_grid(Tmp1_GB, UseAbs=.true., iLoc_I=iLoc_I)
    if(iProc == 0.and.lVerbose>0)then
       call write_prefix; write(iUnitOut,*)
       call write_prefix; write(iUnitOut,*) NameSub, &
            ' maximum of |div B| before projection=',DivBMax
       call write_prefix; write(iUnitOut,*)
    end if
    if(DivBMax>cTiny)then
       if(iProc==iLoc_I(5))then
          call write_prefix; write(iUnitOut,*) NameSub, &
               ' divB,loc,x,y,z=',DivBMax,iLoc_I,&
               Xyz_DGB(:,iLoc_I(x_),iLoc_I(y_),iLoc_I(z_),iLoc_I(4))
       end if

       if(iProc == 0.and.lVerbose>0)then
          call write_prefix; write(iUnitOut,*)
          call write_prefix; write(iUnitOut,*) &
               NameSub,' projecting B for CT scheme...'
       end if

       ! Do the projection with UseConstrainB true
       call project_divb

       ! Check and report the accuracy of the projection
       call proj_get_divb(Tmp1_GB)
       DivBMax = &
            maxval_grid(Tmp1_GB, UseAbs=.true., iLoc_I=iLoc_I)
       if(iProc == 0 .and. lVerbose > 0)then
          call write_prefix; write(iUnitOut,*)
          call write_prefix; write(iUnitOut,*) NameSub, &
               ' maximum of |div B| after projection=',DivBMax
          call write_prefix; write(iUnitOut,*)
       end if
       if(iProc==iLoc_I(5) .and. DivBMax > cTiny)then
          call write_prefix; write(iUnitOut,*) NameSub, &
               ' divB,loc,x,y,z=', DivBMax, iLoc_I,     &
               Xyz_DGB(:,iLoc_I(x_),iLoc_I(y_),iLoc_I(z_),iLoc_I(4))
       end if
    end if

    call test_stop(NameSub, DoTest)
  end subroutine BATS_init_constrain_b
  !============================================================================
  subroutine BATS_save_files(TypeSaveIn)

    ! Save output files (plot, log, satellite, restart, etc.)

    use ModMain
    use ModIO
    use ModUtilities, ONLY: upper_case
    use ModMessagePass, ONLY: exchange_messages
    use BATL_lib, ONLY: iProc
    ! use ModFieldLineThread, ONLY: UseFieldLineThreads
    use ModSatelliteFile, ONLY: UseSatelliteTimeOffset, TimeSatOffset_I, &
         nSatellite

    character(len=*), intent(in) :: TypeSaveIn

    character(len=len(TypeSaveIn)) :: TypeSave
    logical :: DoExchangeAgain, DoAssignNodeNumbers, IsFound, DoSaveRestartTmp
    integer :: iFile

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'BATS_save_files'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest) write(*,*) NameSub,': TypeSaveIn=', TypeSaveIn

    DoExchangeAgain     = .false.
    DoAssignNodeNumbers = .true.
    TypeSave = TypeSaveIn
    call upper_case(TypeSave)

    select case(TypeSave)
    case('INITIAL')
       ! Do not save current step or time
       where(DnOutput_I >= 0 .or. DtOutput_I > 0.0)nStepOutputLast_I = nStep

       ! Initialize last save times
       where(DtOutput_I > 0.0) &
            iTimeOutputLast_I = int(tSimulation/DtOutput_I)
       if(UseSatelliteTimeOffset)then
          where(DtOutput_I(Satellite_+1:Satellite_+nSatellite) > 0.0) &
               iTimeOutputLast_I(Satellite_+1:Satellite_+nSatellite) &
               = int((tSimulation + TimeSatOffset_I(1:nSatellite)) &
               /DtOutput_I(Satellite_+1:Satellite_+nSatellite))
       end if
       ! DoSaveInitial may be set to true in the #SAVEINITIAL command
       if(DoSaveInitial .or. (IsTimeAccurate .and. tSimulation == 0.0))then
          if(DoSaveInitial)then
             ! Save all (except restart files)
             nStepOutputLast_I = -1
             iTimeOutputLast_I = -1
          else
             ! Save only those with a positive time frequency
             where(DtOutput_I>0.)
                nStepOutputLast_I = -1
                iTimeOutputLast_I = -1
             end where
          end if
          ! Do not save restart file in any case
          if(DnOutput_I(restart_) >= 0 .or. DtOutput_I(restart_) > 0.0) &
               nStepOutputLast_I(restart_) = nStep
          call save_files
       end if
       ! Set back to default value (for next session)
       DoSaveInitial = .false.
    case('FINAL')
       DoSaveRestart = .false.
       call save_files_final
    case('FINALWITHRESTART')
       call save_files_final
    case('NORMAL', 'BEGINSTEP', 'BEFOREAMR')
       call save_files
    case('AMRPLOTS')
       do iFile=plot_+1, plot_+nPlotFile
          call save_file
       end do
       if(DoExchangeAgain) &
            call exchange_messages(DoResChangeOnlyIn=.true.)
    case('RESTART')
       DoSaveRestartTmp = DoSaveRestart
       DoSaveRestart = .true.
       iFile = restart_
       call save_file
       DoSaveRestart = DoSaveRestartTmp
    case default
       call stop_mpi(NameSub//' ERROR incorrect TypeSave='//TypeSave)
    end select

    call test_stop(NameSub, DoTest)
  contains
    !==========================================================================
    subroutine save_files

      use ModFieldLineThread, ONLY: save_threads_for_plot, DoPlotThreads
      use ModGroundMagPerturb, ONLY: nMagGridFile
      use ModSatelliteFile, ONLY: UseSatelliteTimeOffset, TimeSatOffset_I, &
           nSatellite
      real :: tSimulationBackup
      logical :: DoPlotThread
      !------------------------------------------------------------------------
      DoPlotThread = DoPlotThreads
      do iFile = 1, nFile
         ! We want to use the IE magnetic perturbations that were passed
         ! in the last coupling together with the current GM perturbations.
         if( (iFile == magfile_ .or. &
              (iFile >= maggridfile_ .and. &
              iFile < maggridfile_ + nMagGridFile)) &
              .neqv. TypeSave == 'BEGINSTEP') CYCLE
         if(UseSatelliteTimeOffset.and.iFile > Satellite_ &
              .and. iFile <= Satellite_ + nSatellite)then
            tSimulationBackup = tSimulation
            tSimulation  = tSimulation + TimeSatOffset_I(iFile-Satellite_)
         end if
         if(DnOutput_I(iFile) >= 0)then
            if(DnOutput_I(iFile) == 0)then
               if(DoPlotThread .and. iFile > plot_&
                    .and. iFile <= plot_ + nPlotFile)then
                  call save_threads_for_plot
                  DoPlotThread = .false.
               end if
               call save_file

            else if(mod(nStep,DnOutput_I(iFile)) == 0)then
               if(DoPlotThread .and. iFile > plot_&
                    .and. iFile <= plot_ + nPlotFile)then
                  call save_threads_for_plot
                  DoPlotThread = .false.
               end if
               call save_file
            end if
         else if(IsTimeAccurate .and. DtOutput_I(iFile) > 0.0)then
            if(int(tSimulation/DtOutput_I(iFile)) &
                 > iTimeOutputLast_I(iFile))then
               iTimeOutputLast_I(iFile) = int(tSimulation/DtOutput_I(iFile))
               if(DoPlotThread .and. iFile > plot_ &
                    .and. iFile <= plot_ + nPlotFile)then
                  call save_threads_for_plot
                  DoPlotThread = .false.
               end if
               call save_file
            end if
         end if
         if(UseSatelliteTimeOffset .and. iFile > Satellite_ &
              .and. iFile <= Satellite_ + nSatellite) &
              tSimulation = tSimulationBackup
      end do
      ! If message passing with corners was done in save_file
      ! then do exchange_messages over again to get expected values
      ! in ghost cells.

      if(DoExchangeAgain)then
         if(iProc == 0 .and. lVerbose > 0)then
            call write_prefix; write(iUnitOut,*)&
                 'Calling exchange_messages to reset ghost cells ...'
         end if
         call exchange_messages(DoResChangeOnlyIn=.true.)
      end if

    end subroutine save_files
    !==========================================================================
    subroutine save_file

      use ModRestartFile, ONLY: write_restart_files
      use ModSatelliteFile, ONLY: IsFirstWriteSat_I,                   &
           nSatellite, set_satellite_file_status, set_satellite_flags, &
           TimeSatStart_I, TimeSatEnd_I, iPointCurrentSat_I,  &
           TypeTrajTimeRange_I, StartTimeTraj_I, EndTimeTraj_I, DtTraj_I, &
           nPointTraj_I, TimeSat_II, iProcSat_I, iBlockSat_I
      use ModWriteLogSatFile, ONLY: write_logfile
      use ModGroundMagPerturb, ONLY: &
           DoSaveMags, DoSaveGridmag, write_magnetometers, &
           DoWriteIndices, write_geoindices, nMagGridFile
      use ModParticleFieldLine, ONLY: write_plot_particle
      use ModWritePlot, ONLY: write_plot
      use ModWritePlotLos, ONLY: write_plot_los
      use ModWritePlotRadiowave, ONLY: write_plot_radiowave
      use ModWriteTecplot, ONLY: assign_node_numbers
      use ModFieldTrace, ONLY: &
           write_plot_lcb, write_plot_ieb, write_plot_equator, write_plot_line
      use ModFieldTraceFast, ONLY: &
           trace_field_grid, Trace_DSNB, calc_squash_factor
      use ModBuffer, ONLY: plot_buffer
      use ModMessagePass, ONLY: exchange_messages
      use ModAdvance, ONLY: State_VGB, DtMax_CB
      use ModB0, ONLY: UseB0, B0_DGB

      integer :: iSat, iPointSat, iParcel

      ! tSimulation is used in steady-state runs as a loop parameter
      ! in the save_files subroutine, where set_satellite_flags and
      ! write_logfile are called with different tSimulation values
      ! spanning all the satellite trajectory cut. Old tSimulation value
      ! is saved here before and it is restored after the loop.
      real :: tSimulationBackup = 0.0
      !------------------------------------------------------------------------
      if(nStep <= nStepOutputLast_I(iFile) .and.&
           (DnOutput_I(iFile) >= 0 .or. DtOutput_I(iFile) > 0.0)) RETURN

#ifdef _OPENACC
      ! Check if this is a satellite file that requires less copying
      if(IsTimeAccurate .and. &
           iFile > Satellite_ .and. iFile <= Satellite_ + nSatellite) then
         iSat = iFile - Satellite_
         if(TypeTrajTimeRange_I(iSat) == 'orig') then
            call set_satellite_flags(iSat)
            if(iProc == iProcSat_I(iSat))then
               ! Only update the block that is required for satellite output
               !$acc update host(State_VGB(:,:,:,:,iBlockSat_I(iSat)))
               if(UseB0)then
                  !$acc update host(B0_DGB(:,:,:,:,iBlockSat_I(iSat)))
               end if
            end if
         else
            ! Get full State_VGB for trajectory range satellite
            call sync_cpu_gpu('update on CPU', NameSub, State_VGB, B0_DGB)
         end if
      else
         ! Get full State_VGB for plot files and log files
         call sync_cpu_gpu('update on CPU', NameSub, State_VGB, B0_DGB)
      end if
#endif

      if(iFile == restart_) then
         ! Case for restart file
         if(.not.DoSaveRestart)RETURN
         call write_restart_files

      elseif(iFile == logfile_) then
         ! Case for logfile

         if(.not.DoSaveLogfile)RETURN

         call timing_start('write_logfile')
         call write_logfile(0, iFile)
         call timing_stop('write_logfile')

      elseif(iFile>plot_ .and. iFile<=plot_+nplotfile) then
         ! Case for plot files
         IsFound=.false.

         if(DoExchangeAgain .and. ( &
              index(TypePlot_I(iFile),'rfr')==1 .or. &
              index(TypePlot_I(iFile),'pnt')==1))then
            if(iProc==0.and.lVerbose>0)then
               call write_prefix; write(iUnitOut,*)&
                    'Calling exchange_messages to reset ghost cells ...'
            end if
            call exchange_messages(DoResChangeOnlyIn=.true.)
            DoExchangeAgain = .false.
         end if
         if(TypeSave /= 'BEFOREAMR' .and. .not.DoExchangeAgain .and. ( &
              index(TypePlot_I(iFile),'lin')==1 .or. &
              index(TypePlot_I(iFile),'pnt')==1 .or. &
              index(TypePlot_I(iFile),'eqr')==1 .or. &
              index(TypePlot_I(iFile),'eqb')==1 .or. &
              index(TypePlot_I(iFile),'ieb')==1 .or. &
              index(TypePlot_I(iFile),'lcb')==1 .or. &
              index(TypePlot_I(iFile),'los')==1 .or. &
              index(TypePlot_I(iFile),'sph')==1 .or. &
              (TypePlotFormat_I(iFile) == 'tec'      .and. &
              index(TypePlot_I(iFile),'rfr')/=1.and. &
              index(TypePlot_I(iFile),'pnt')/=1     )))then

            if(iProc == 0 .and. lVerbose > 0)then
               call write_prefix; write(iUnitOut,*)&
                    ' Message passing for plot files ...'
            end if
            call exchange_messages(UseOrder2In=.true.)
            DoExchangeAgain = .true.
         end if

         if(index(StringPlotVar_I(iFile),'squash') > 0) &
              call calc_squash_factor

         if(index(TypePlot_I(iFile),'los') > 0) then
            IsFound = .true.
            call write_plot_los(iFile)
         end if

         if(index(TypePlot_I(iFile),'pnt') > 0) then
            IsFound = .true.
            call write_plot_particle(iFile)
         end if

         if(index(TypePlot_I(iFile),'rfr') > 0) then
            IsFound = .true.
            call write_plot_radiowave(iFile)
         end if

         if(index(TypePlot_I(iFile),'lin') > 0) then
            IsFound = .true.
            call write_plot_line(iFile)
         end if

         if(  index(TypePlot_I(iFile),'eqr') > 0 .or. &
              index(TypePlot_I(iFile),'eqb') > 0) then
            IsFound = .true.
            call write_plot_equator(iFile)
         end if

         if(index(TypePlot_I(iFile),'ieb') > 0) then
            IsFound = .true.
            call write_plot_ieb(iFile)
         end if

         if(index(TypePlot_I(iFile),'lcb') > 0) then
            IsFound = .true.
            call write_plot_lcb(iFile)
         end if

         if(index(TypePlot_I(iFile),'buf') > 0)then
            IsFound = .true.
            if(TypeSaveIn/='INITIAL')call plot_buffer(iFile)
         end if

         if(TypePlot_I(iFile) /= 'nul' .and. .not.IsFound ) then
            ! Assign node numbers for tec plots
            if( index(TypePlotFormat_I(iFile),'tec') > 0 &
                 .and. DoAssignNodeNumbers)then
               call assign_node_numbers
               DoAssignNodeNumbers = .false.
            end if

            if(  index(TypePlot_I(iFile),'ray') > 0 .or. &
                 index(StringPlotVar_I(iFile),'status') > 0)then
               call trace_field_grid
               call sync_cpu_gpu('update on CPU', NameSub, &
                    Trace_DICB=Trace_DSNB)
            end if

            if(index(StringPlotVar_I(iFile), ' dt ') > 0)then
               ! Plotting dt requires DtMax_CB on the CPU
               !$acc update host(DtMax_CB)
            end if

            call timing_start('save_plot')
            call write_plot(iFile)
            call timing_stop('save_plot')
         end if

      elseif(iFile > Parcel_ .and. iFile <= Parcel_ + nParcel) then
         iParcel = iFile - Parcel_
         call write_logfile(-iParcel,iFile)

      elseif(iFile > Satellite_ .and. iFile <= Satellite_ + nSatellite) then

         ! Case for satellite files
         iSat = iFile - Satellite_
         call timing_start('save_satellite')

         if (TypeTrajTimeRange_I(iSat) == 'range') then
            if(iProc==0)call set_satellite_file_status(iSat,'open')

            ! need this to write the header
            IsFirstWriteSat_I(iSat) = .true.

            tSimulationBackup = tSimulation
            tSimulation = StartTimeTraj_I(iSat)

            do while (tSimulation <= EndTimeTraj_I(iSat))
               call set_satellite_flags(iSat)
               ! write for ALL the points of trajectory cut
               call write_logfile(iSat, iFile, &
                    TimeSatHeaderIn=tSimulationBackup)
               tSimulation = tSimulation + DtTraj_I(iSat)
            end do

            tSimulation = tSimulationBackup    ! ... Restore
            iPointCurrentSat_I(iSat) = 1

            if(iProc==0)call set_satellite_file_status(iSat,'close')
         else if (TypeTrajTimeRange_I(iSat) == 'full') then
            if(iProc==0)call set_satellite_file_status(iSat,'open')

            ! need this to write the header
            IsFirstWriteSat_I(iSat) = .true.

            tSimulationBackup = tSimulation
            tSimulation = StartTimeTraj_I(iSat)

            do iPointSat = 1, nPointTraj_I(iSat)
               tSimulation = TimeSat_II(iSat, iPointSat)
               call set_satellite_flags(iSat)
               ! write for ALL the points of trajectory cut
               call write_logfile(iSat, iFile, &
                    TimeSatHeaderIn=tSimulationBackup)
            end do

            tSimulation = tSimulationBackup    ! ... Restore
            iPointCurrentSat_I(iSat) = 1

            if(iProc==0)call set_satellite_file_status(iSat,'close')
         else if (TypeTrajTimeRange_I(iSat) == 'orig') then
            !
            ! Distinguish between IsTimeAccurate and .not. IsTimeAccurate:
            !
            if (IsTimeAccurate) then
               if(iProc==0)call set_satellite_file_status(iSat,'append')

               call set_satellite_flags(iSat)
               ! write one line for a single trajectory point
               call write_logfile(iSat,iFile)
            else
               if(iProc==0)call set_satellite_file_status(iSat,'open')

               ! need this to write the header
               IsFirstWriteSat_I(iSat) = .true.

               tSimulationBackup = tSimulation    ! Save ...
               tSimulation = TimeSatStart_I(iSat)
               do while (tSimulation <= TimeSatEnd_I(iSat))
                  call set_satellite_flags(iSat)
                  ! write for ALL the points of trajectory cut
                  call write_logfile(iSat,iFile)
                  tSimulation = tSimulation + DtOutput_I(iSat+Satellite_)
               end do
               tSimulation = tSimulationBackup    ! ... Restore
               iPointCurrentSat_I(iSat) = 1

               if(iProc==0)call set_satellite_file_status(iSat,'close')
            end if
         else
            call stop_mpi(NameSub//' unknow TypeTrajTimeRange_I: '// &
                 TypeTrajTimeRange_I(iSat))
         end if

         call timing_stop('save_satellite')

      elseif(iFile == magfile_) then
         ! Cases for magnetometer files
         if(.not.DoSaveMags) RETURN
         if(IsTimeAccurate) then
            call timing_start('save_magnetometer')
            call write_magnetometers('stat')
            call timing_stop('save_magnetometer')
         end if

      elseif(iFile >= maggridfile_ .and. iFile < maggridfile_+nMagGridFile)then
         ! Case for grid magnetometer files
         if(.not. DoSaveGridmag) RETURN
         if(IsTimeAccurate) then
            call timing_start('grid_magnetometer')
            call write_magnetometers('grid', iFile-maggridfile_)
            call timing_stop('grid_magnetometer')
         end if

      elseif(iFile == indexfile_) then
         ! Write geomagnetic index file.
         if(IsTimeAccurate .and. DoWriteIndices) call write_geoindices
      end if

      nStepOutputLast_I(iFile) = nStep

      if(iProc==0 .and. lVerbose>0 .and. &
           (iFile == restart_ .or. &
           (iFile > plot_ .and. iFile <= plot_+MaxPlotfile))) then
         if(IsTimeAccurate)then
            call write_prefix;
            write(iUnitOut,'(a,i2,a,a,a,i8,a,i4,a,i2.2,a,i2.2,a)') &
                 'saved iFile=',iFile,' type=',trim(TypePlot_I(iFile)),&
                 ' at nStep=',nStep,' time=', &
                 int(                            tSimulation/3600.),':', &
                 int((tSimulation &
                 -(3600.*int(tSimulation/3600.)))/60.),':', &
                 int( tSimulation-(  60.*int(tSimulation/  60.))), &
                 ' h:m:s'
         else
            call write_prefix; write(iUnitOut,'(a,i2,a,a,a,i8)') &
                 'saved iFile=',iFile,' type=',TypePlot_I(iFile), &
                 ' at nStep=',nStep
         end if
      end if

    end subroutine save_file
    !==========================================================================
    subroutine save_files_final

      use ModSatelliteFile, ONLY: set_satellite_file_status, nSatellite
      use ModGroundMagPerturb, ONLY: finalize_magnetometer

      integer :: iSat
      !------------------------------------------------------------------------
      do iFile = 1, plot_ + nPlotFile
         call save_file
      end do

      ! Close files
      if (iProc==0) then
         do iSat = 1, nSatellite
            call set_satellite_file_status(iSat,'close')
         end do
      end if

      call finalize_magnetometer

      if (DoSaveLogfile.and.iProc==0.and.iUnitLogfile>0) close(iUnitLogfile)

    end subroutine save_files_final
    !==========================================================================
  end subroutine BATS_save_files
  !============================================================================
  subroutine BATS_finalize

    ! Alphabetical order
    use BATL_lib, ONLY: clean_batl
    use ModAdvance, ONLY: clean_mod_advance
    use ModBlockData, ONLY: clean_mod_block_data
    use ModBorisCorrection, ONLY: clean_mod_boris_correction
    use ModConstrainDivB, ONLY: clean_mod_ct
    use ModFaceValue, ONLY: clean_mod_face_value
    use ModFieldTrace, ONLY: clean_mod_field_trace
    use ModGeometry, ONLY: clean_mod_geometry
    use ModIeCoupling, ONLY: clean_mod_ie_coupling
    use ModMain, ONLY: clean_mod_main
    use ModNodes, ONLY: clean_mod_nodes
    use ModParallel, ONLY: clean_mod_parallel
    use ModPartImplicit, ONLY: clean_mod_part_impl
    use ModPointImplicit, ONLY: clean_mod_point_impl
    use ModReverseField, ONLY: clean_mod_reverse_field
    use ModSemiImplicit, ONLY: clean_mod_semi_impl
    use ModUserInterface, ONLY: user_action
    use ModBatsrusUtility, ONLY: error_report

    integer:: iError
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'BATS_finalize'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    call clean_batl
    call user_action("clean module")
    call clean_mod_advance
    call clean_mod_boris_correction
    call clean_mod_block_data
    call clean_mod_ct
    call clean_mod_face_value
    call clean_mod_field_trace
    call clean_mod_geometry
    call clean_mod_ie_coupling
    call clean_mod_main
    call clean_mod_nodes
    call clean_mod_parallel
    call clean_mod_part_impl
    call clean_mod_point_impl
    call clean_mod_reverse_field
    call clean_mod_semi_impl

    ! call clean_mod_boundary_cells !!! to be implemented
    ! call clean_mod_resistivity !!! to be implemented

    call error_report('PRINT',0.,iError,.true.)

    call test_stop(NameSub, DoTest)
  end subroutine BATS_finalize
  !============================================================================
end module ModBatsrusMethods
!==============================================================================
