!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
program batsrus

  use BATL_lib, ONLY: &
       test_start, test_stop, lVerbose

  use BATL_lib, ONLY: iProc, iComm, lVerbose, init_mpi, clean_mpi
  use ModBatsrusMethods, ONLY: &
       BATS_init_session, &
       BATS_setup, &
       BATS_advance, &
       BATS_save_files, &
       BATS_finalize
  use ModKind
  use ModUtilities, ONLY: remove_file, touch_file
  use ModMain, ONLY: &
       IsStandAlone, &
       IsTimeAccurate, IsTimeLoop, tSimulation, tSimulationMax, &
       nStep, nIter, nIteration, &
       IsLastRead, &
       DnTiming, UseTiming, UseTimingAll, iUnitTiming
  use ModSetParameters, ONLY: set_parameters
  use ModWriteProgress, ONLY: write_progress, write_runtime_values
  use ModRestartFile, ONLY: NameRestartInDir
  use CON_planet, ONLY: init_planet_const, set_planet_defaults
  use ModConst, ONLY: &
       cSecondPerYear, cSecondPerDay, cSecondPerHour, cSecondPerMinute

  use ModReadParam

  use ModMpi

  implicit none

  integer :: iSession=1
  logical :: IsForcedStop = .false.
  real(Real8_) :: CpuTimeStart

  ! Initialization of MPI/parallel message passing.
  !----------------------------------------------------------------------------
  call init_mpi

  ! Initialize some basic variables for the stand alone code
  IsStandAlone = .true.

  ! Initialize the planetary constant library and set Earth
  ! as the default planet.
  call init_planet_const
  call set_planet_defaults

  ! Not yet doing the computation
  IsTimeLoop = .false.
  !$acc update device(IsTimeLoop)

  ! Show git information
  if(iProc == 0)then
     include "show_git_info.h"
  endif

  ! Announce BATSRUS
  call write_progress

  ! Initialize time which is used to check CPU time
  CpuTimeStart = MPI_WTIME()

  ! Delete BATSRUS.SUCCESS, BATSRUS.DONE and BATSRUS.STOP files if found
  if(iProc == 0)then
     call remove_file('BATSRUS.SUCCESS')
     call remove_file('BATSRUS.DONE')
     call remove_file('BATSRUS.STOP')
  end if

  ! Read input parameter file. Provide the default restart file name
  call read_file('PARAM.in', iComm, trim(NameRestartInDir)//'restart.H')

  SESSIONLOOP: do
     call read_init('  ', iSessionIn=iSession)

     if(iProc == 0 .and. lVerbose >= 0)&
          write(*,*)'----- Starting Session ',iSession,' ------'
     ! Set and check input parameters for this session
     call set_parameters('READ')
     call set_parameters('CHECK')

     ! Test string is read, so set the test flags now
     ! call set_oktest('main',DoTest,DoTest)

     ! Time execution (timing parameters were set by MH_set_parameters)
     if(iSession == 1)then
        call timing_start('BATSRUS')
        call timing_start('setup')
        call BATS_setup
        call BATS_init_session
        call timing_stop('setup')
        if(DnTiming > -3)call timing_report_total
        if(iProc == 0)write(*,*)'Resetting timing counters after setup.'
        call timing_reset('#all',3)
     else
        call write_runtime_values
        call BATS_init_session
     end if

     TIMELOOP: do
        ! Stop this session if stopping conditions are fulfilled
        if(do_stop_session()) EXIT TIMELOOP
        if(do_stop_run()) EXIT SESSIONLOOP

        call timing_step(nStep+1)

        if(IsTimeAccurate .and. tSimulationMax > 0.0) then
           call BATS_advance(tSimulationMax)
        else
           call BATS_advance(huge(0.0))
        end if

        call show_progress

     end do TIMELOOP

     if(IsLastRead)then
        EXIT SESSIONLOOP
     else
        if(iProc == 0 .and. lVerbose >= 0) &
             write(*,*)'----- End of Session   ',iSession,' ------'
        iSession=iSession+1
        if (DnTiming > -2) call timing_report
        call timing_reset_all
     end if

  end do SESSIONLOOP
  IsTimeLoop = .false.
  !$acc update device(IsTimeLoop)

  if(iProc == 0 .and. lVerbose >= 0)then
     write(*,*)
     write(*,'(a)')'    Finished Numerical Simulation'
     write(*,'(a)')'    -----------------------------'
     if (IsTimeAccurate)then
        write(*, '(a,es13.5,a)') &
             '    Simulated time = ', tSimulation, ' s '
        if(tSimulation > 1e6*cSecondPerYear) then
           write(*, '(a,es13.5,a)') &
                tSimulation/cSecondPerYear
        elseif(tSimulation > cSecondPerYear) then
           write(*, '(a,f13.6,a)') '    Simulated time = ', &
                tSimulation/cSecondPerYear, ' years'
        elseif(tSimulation > cSecondPerDay) then
           write(*, '(a,f13.6,a)') '    Simulated time = ', &
                tSimulation/cSecondPerDay, ' days'
        elseif(tSimulation > cSecondPerHour) then
           write(*, '(a,f13.6,a)') '    Simulated time = ', &
                tSimulation/cSecondPerHour, ' hours'
        elseif(tSimulation > cSecondPerMinute) then
           write(*, '(a,f13.6,a)') '    Simulated time = ', &
                tSimulation/cSecondPerMinute, ' mins'
        end if
     end if
  end if
  if (DnTiming > -2) call timing_report

  call BATS_save_files('FINALWITHRESTART')

  if(iProc == 0 .and. lVerbose > 0)then
     write(*,*)
     write(*,'(a)')'    Finished Saving Output Files'
     write(*,'(a)')'    ----------------------------'
  end if

  call timing_stop('BATSRUS')

  if(DnTiming > -3)call timing_report_total

  if(UseTimingAll)close(iUnitTiming)

  call BATS_finalize

  ! Touch BATSRUS.SUCCESS
  if(iProc == 0) call touch_file('BATSRUS.SUCCESS')
  if(iProc == 0 .and. .not. IsForcedStop) call touch_file('BATSRUS.DONE')

  call clean_mpi

contains
  !============================================================================
  logical function do_stop_session()
    !--------------------------------------------------------------------------
    do_stop_session = .false.

    if(nIter >= 0 .and. nIteration >= nIter) &
         do_stop_session = .true.
    if(IsTimeAccurate .and. tSimulationMax > 0.0 &
         .and. tSimulation >= tSimulationMax) &
         do_stop_session = .true.

  end function do_stop_session
  !============================================================================
  logical function do_stop_run()

    use ModMain, ONLY: CpuTimeMax, DoCheckStopFile

    integer :: iError
    !--------------------------------------------------------------------------
    IsForcedStop = .false.

    if(iProc == 0)then
       if(CpuTimeMax > 0 .and. MPI_WTIME() - CpuTimeStart >= CpuTimeMax)then
          write(*,*)'CPU time exceeded:', CpuTimeMax, MPI_WTIME()-CpuTimeStart
          IsForcedStop=.true.
       end if
       if(.not.IsForcedStop .and. DoCheckStopFile) then
          inquire(FILE='BATSRUS.STOP', EXIST=IsForcedStop)
          if (IsForcedStop) &
               write(*,*)'BATSRUS.STOP file exists: recieved stop signal'
       end if
    end if

    call MPI_BCAST(IsForcedStop, 1, MPI_LOGICAL, 0, iComm, iError)

    do_stop_run = IsForcedStop

  end function do_stop_run
  !============================================================================
  subroutine show_progress

    use ModIo, ONLY: DnProgressShort, DnProgressLong
    use ModMain, ONLY: nI, nJ, nK, nBlock, Unused_B, nStep, Dt
    use ModPhysics, ONLY: Si2No_V, UnitT_

    ! Show timing results if required
    ! Show speed as cells/second/PE/step

    real(Real8_), external :: timing_func_d
    real(Real8_):: CpuTimeBATSRUS, CpuTimeAdvance

    integer :: nIterExpect, nIterExpectTime

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'show_progress'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if( UseTiming .and. iProc == 0 &
         .and. DnProgressShort>0 .and. mod(nStep,DnProgressShort) == 0 ) then

       CpuTimeBATSRUS=timing_func_d('sum',3,'BATSRUS','BATSRUS')
       CpuTimeAdvance=timing_func_d('sum',1,'advance','BATSRUS')
       if (.not.IsTimeAccurate) then
          write(*,'(a,f12.1,a,f9.1,a,i8)') 'Speed is',&
               nI*nJ*nK*count(.not.Unused_B(1:nBlock)) &
               /max(1.D-10,CpuTimeAdvance),&
               ' c/s/p after',&
               CpuTimeBATSRUS,&
               ' s at N =',nStep
       else
          write(*,'(a,f12.1,a,f9.1,a,i8,a,1p,e11.4,a)') 'Speed is',&
               nI*nJ*nK*count(.not.Unused_B(1:nBlock)) &
               /max(1.D-10,CpuTimeAdvance),&
               ' c/s/p after',&
               CpuTimeBATSRUS,&
               ' s at N =',nStep, ' (', tSimulation,' s)'
       endif

    endif

    ! Show timing tables
    if(DnTiming > 0 .and. mod(nIteration,DnTiming) == 0) then
       call timing_report
    else if(DnProgressLong > 0 .and. mod(nStep,DnProgressLong) == 0) then
       call timing_tree(2,2)
    end if

    ! Try to estimate the remaining length of the run
    if(UseTiming .and. iProc == 0 &
         .and. DnProgressLong>0 .and. mod(nStep,DnProgressLong) == 0)then
       nIterExpect = nITER-nIteration
       if(IsTimeAccurate .and. Dt > 0.0 .and. tSimulationMax > 0.0)then
          nIterExpectTime = min( real(huge(1)), &
               (tSimulationMax - tSimulation)/Dt*Si2No_V(UnitT_) )
          if(nIterExpect < 0)then
             nIterExpect = nIterExpectTime
          else if(nIterExpectTime > 0)then
             nIterExpect = min(nIterExpect, nIterExpectTime)
          endif
       end if
       CpuTimeAdvance=timing_func_d('sum/iter',2,'advance','BATSRUS')
       write (*,'(i8,a,i8,a,f10.2)') nIteration,' of ', &
            nIterExpect+nIteration,  &
            ' iterations done.   Expected time to completion:', &
            CpuTimeAdvance*nIterExpect
       write(*,*)
    end if

    call test_stop(NameSub, DoTest)
  end subroutine show_progress
  !============================================================================
end program batsrus
!==============================================================================
subroutine read_ih_buffer(y,z,State_V)

  use ModBatsrusUtility, ONLY: stop_mpi
  real :: y, z, State_V(8)
  !----------------------------------------------------------------------------
  call stop_mpi('ERROR: read_ih_buffer is for SWMF')

end subroutine read_ih_buffer
!==============================================================================
subroutine read_pw_buffer(FaceCoords_D,nVar,FaceState_V)

  use ModBatsrusUtility, ONLY: stop_mpi

  real, intent(in) :: FaceCoords_D(3)
  integer, intent(in) :: nVar
  real, intent(inout) :: FaceState_V(nVar)
  !----------------------------------------------------------------------------
  call stop_mpi('ERROR: read_pw_buffer is for SWMF')

end subroutine read_pw_buffer
!==============================================================================
