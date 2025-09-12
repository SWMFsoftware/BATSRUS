!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModBatsrusUtility

  ! Some simple BATSRUS specific utilities
  implicit none

  private ! except

  public:: barrier_mpi     ! Apply and time MPI barrier
  public:: barrier_mpi2    ! Apply and time MPI barrier with a name
  public:: stop_mpi        ! Stop BATSRUS. Report time and time step.
  public:: error_report    ! Report various errors. Provide final statistics.
  public:: get_date_time   ! get integer date+time for current simulation time
  public:: get_time_string ! create date+time string based on NameMaxTimeUnit
  public:: get_ivar        ! calculate variable index based on its name

contains
  !============================================================================
  subroutine barrier_mpi

    use BATL_lib, ONLY: iComm
    use ModMpi

    ! Local variables:
    integer :: iError
    !--------------------------------------------------------------------------
    call timing_start('barrier')
    call MPI_barrier(iComm, iError)
    call timing_stop('barrier')

  end subroutine barrier_mpi
  !============================================================================
  subroutine barrier_mpi2(String)

    use BATL_lib, ONLY: iComm
    use ModMpi

    character (len=*), intent(in) :: String

    ! Local variables:
    integer :: iError
    !--------------------------------------------------------------------------
    call timing_start('barrier-'//String)
    call MPI_barrier(iComm, iError)
    call timing_stop('barrier-'//String)

  end subroutine barrier_mpi2
  !============================================================================
  subroutine stop_mpi(String)
    !$acc routine seq

    use ModMain, ONLY: nIteration, iStage, tSimulation, NameThisComp
    use ModUtilities, ONLY: CON_stop_simple

    character(len=*), intent(in) :: String
    !--------------------------------------------------------------------------
    write(*,*) NameThisComp,' BATSRUS stopping at iteration=', &
         nIteration, ' stage=', iStage, ' simulation time=', tSimulation

    call CON_stop_simple(String)

  end subroutine stop_mpi
  !============================================================================
  subroutine error_report(String, Value, iErrorIn, DoShowFirst)

    use BATL_lib, ONLY: iComm, iProc, nProc
    use ModMain, ONLY: nIteration
    use ModIO, ONLY: write_myname
    use ModMpi

    ! Collect global error reports
    ! Reports are identified by an individual string String if iErrorIn<1
    !    and iErrorIn is set to a Value > 1 for later use.
    ! If iErrorIn is > 1 to start with, it is used for error identification.
    ! Make statistics of errors based on Value
    ! print statistics if String='PRINT'

    ! Parameters:

    ! Maximum number of different types of errors
    integer, parameter :: MaxError=20

    ! Arguments:

    character (LEN=*), intent(in) :: String
    real, intent(in)              :: Value
    integer, intent(inout)        :: iErrorIn
    logical, intent(in)           :: DoShowFirst

    ! Local variables:

    integer :: iError

    ! Current number of different types of errors
    integer :: nError = 0

    ! Error message
    character(LEN=60):: StringError

    ! Message, number, occurance, and statistics of errors
    character(len=60), save :: StringError_I(MaxError)
    integer, dimension(MaxError):: &
         nErrorCount_I=0, nErrorSum_I, MaxError_I,&
         nIterFirst_I=100000, nIterLast_I=-1
    real, dimension(MaxError):: &
         ErrorMin_I=1e30, ErrorMax_I=-1e30, &
         ErrorMean_I=0., ErrorLast_I=0., ErrorLastSum_I=0.

    integer, allocatable :: nError_P(:)
    character(len=60), allocatable :: StringError_IP(:,:)
    integer, allocatable:: nErrorCount_IP(:,:), nIterFirst_IP(:,:), &
         nIterLast_IP(:,:)
    real, allocatable :: ErrorMin_IP(:,:), ErrorMax_IP(:,:), &
         ErrorMean_IP(:,:), ErrorLast_IP(:,:)

    integer :: i,i0,jProc
    !--------------------------------------------------------------------------
    if(String=='PRINT')then
       ! Allocate memory in PROC 0
       allocate(&
            nError_P(nProc),&
            StringError_IP(MaxError,nProc),&
            nErrorCount_IP(MaxError,nProc),&
            nIterFirst_IP(MaxError,nProc),&
            nIterLast_IP(MaxError,nProc),&
            ErrorMin_IP(MaxError,nProc),&
            ErrorMax_IP(MaxError,nProc),&
            ErrorMean_IP(MaxError,nProc),&
            ErrorLast_IP(MaxError,nProc))

       ! Collect the error reports
       call MPI_gather(nError, 1, MPI_INTEGER, &
            nError_P, 1, MPI_INTEGER, 0, iComm, iError)
       call MPI_gather(StringError_I, 60*MaxError, MPI_CHARACTER, &
            StringError_IP, 60*MaxError, MPI_CHARACTER, 0, iComm,iError)
       call MPI_gather(nErrorCount_I, MaxError, MPI_INTEGER, &
            nErrorCount_IP, MaxError, MPI_INTEGER, 0, iComm, iError)
       call MPI_gather(nIterFirst_I, MaxError, MPI_INTEGER, &
            nIterFirst_IP, MaxError, MPI_INTEGER, 0, iComm, iError)
       call MPI_gather(nIterLast_I, MaxError, MPI_INTEGER, &
            nIterLast_IP, MaxError, MPI_INTEGER, 0, iComm, iError)
       call MPI_gather(ErrorMin_I, MaxError, MPI_REAL, &
            ErrorMin_IP, MaxError, MPI_REAL, 0, iComm, iError)
       call MPI_gather(ErrorMax_I, MaxError, MPI_REAL, &
            ErrorMax_IP, MaxError, MPI_REAL, 0, iComm, iError)
       call MPI_gather(ErrorMean_I, MaxError, MPI_REAL, &
            ErrorMean_IP, MaxError, MPI_REAL, 0, iComm, iError)
       call MPI_gather(ErrorLast_I, MaxError, MPI_REAL, &
            ErrorLast_IP, MaxError, MPI_REAL, 0, iComm, iError)

       ! Analyze errors in PROC 0
       if(iProc==0)then
          nError=0
          do jProc = 1, nProc
             do i = 1, nError_P(jProc)
                StringError = StringError_IP(i,jProc)
                i0 = 1
                do
                   if(i0 > nError)then
                      nError = i0
                      StringError_I(i0) = StringError
                      MaxError_I(i0)    = nErrorCount_IP(i,jProc)
                      nErrorSum_I(i0)   = nErrorCount_IP(i,jProc)
                      nIterFirst_I(i0)  = nIterFirst_IP(i,jProc)
                      nIterLast_I(i0)   = nIterLast_IP(i,jProc)
                      ErrorMin_I(i0)    = ErrorMin_IP(i,jProc)
                      ErrorMax_I(i0)    = ErrorMax_IP(i,jProc)
                      ErrorMean_I(i0)   = ErrorMean_IP(i,jProc)
                      ErrorLast_I(i0)   = ErrorLast_IP(i,jProc)
                      ErrorLast_I(i0)   = ErrorLast_IP(i,jProc)
                      EXIT
                   end if
                   if(StringError_I(i0) == StringError)then

                      ErrorMean_I(i0) = &
                           (ErrorMean_IP(i,jProc)*nErrorCount_IP(i,jProc)+&
                           ErrorMean_I(i0)*nErrorSum_I(i0)) &
                           /(nErrorCount_IP(i,jProc)+nErrorSum_I(i0))

                      if(nIterLast_I(i0) < nIterLast_IP(i,jProc))then
                         ErrorLast_I(i0) = ErrorLast_IP(i,jProc)
                         ErrorLast_I(i0) = ErrorLast_IP(i,jProc)
                         nIterLast_I(i0) = nIterLast_IP(i,jProc)
                      elseif(nIterLast_I(i0) == nIterLast_IP(i,jProc))then
                         ErrorLastSum_I(i0) = ErrorLastSum_I(i0) &
                              + ErrorLast_IP(i,jProc)
                      end if

                      nErrorSum_I(i0)=&
                           nErrorCount_IP(i,jProc) + nErrorSum_I(i0)
                      MaxError_I(i0)=&
                           max(nErrorCount_IP(i,jProc), MaxError_I(i0))
                      nIterFirst_I(i0)=&
                           min(nIterFirst_IP(i,jProc), nIterFirst_I(i0))
                      ErrorMin_I(i0)=&
                           min(ErrorMin_IP(i,jProc), ErrorMin_I(i0))
                      ErrorMax_I(i0)=&
                           max(ErrorMax_IP(i,jProc), ErrorMax_I(i0))
                      EXIT
                   end if
                   i0 = i0 + 1
                end do ! i0
             end do ! error types
          end do ! processors

          ! Report errors
          if(nError==0)then
             call write_myname; write(*,*)'Error report: no errors...'
          else
             do i = 1, nError
                call write_myname
                write(*,'(a,a)')'Error report for ',trim(StringError_I(i))
                call write_myname
                write(*,*)'OCCURED nIterFirst=', nIterFirst_I(i),&
                     ' Last=', nIterLast_I(i),&
                     ' MaxError=', MaxError_I(i),&
                     ' nErrorSum=', nErrorSum_I(i)
                call write_myname
                write(*,*)'VALUES Min=',ErrorMin_I(i),' Max=', ErrorMax_I(i),&
                     ' Mean=', ErrorMean_I(i),' Last=', ErrorLast_I(i),&
                     ' LastSum=', ErrorLastSum_I(i)
                call write_myname; write(*,*)
             end do
          end if

       end if ! iProc==0

       deallocate(nError_P, StringError_IP, nErrorCount_IP,&
            nIterFirst_IP, nIterLast_IP, ErrorMin_IP, ErrorMax_IP,&
            ErrorMean_IP, ErrorLast_IP)

       RETURN

    end if ! PRINT

    if(iErrorIn < 1 .or. iErrorIn > nError) then
       ! Determine iErrorIn based on String
       iErrorIn=1
       do
          if(iErrorIn > nError)then
             ! First occurance of this error type
             nError = iErrorIn
             EXIT
          end if
          if(StringError_I(iErrorIn) == String) EXIT
          iErrorIn = iErrorIn + 1
       end do
    end if

    i=iErrorIn

    nErrorCount_I(i) = nErrorCount_I(i) + 1
    nIterLast_I(i)   = nIteration
    ErrorLast_I(i)   = Value

    if(nErrorCount_I(i) == 1)then
       if(DoShowFirst)then
          call write_myname;
          write(*,*)'First error for ',String,' (PE=',iProc,&
               ') at iter=',nIteration,' with value=',Value
       end if
       StringError_I(i) = String
       nIterFirst_I(i)  = nIteration
       ErrorMin_I(i)    = Value
       ErrorMax_I(i)    = Value
       ErrorMean_I(i)   = Value
    else
       ErrorMin_I(i) = min(ErrorMin_I(i),Value)
       ErrorMax_I(i) = max(ErrorMax_I(i),Value)
       ErrorMean_I(i) = &
            (ErrorMean_I(i)*(nErrorCount_I(i)-1) + Value)/nErrorCount_I(i)
    end if

  end subroutine error_report
  !============================================================================
  subroutine test_error_report

    use BATL_lib, ONLY: iProc
    use ModMain, ONLY: nIteration

    integer:: iError1 = -1, iError2 = -1, iError3 = -1

    ! Test error_report
    !--------------------------------------------------------------------------
    select case(iProc)
    case(0)
       nIteration=1
       call error_report('negative pressure', -1., iError1, .true.)
       call error_report('negative pressure', -2., iError1, .true.)
       call error_report('energy correction', 0.1, iError2, .false.)
       nIteration=2
       call error_report('energy correction', 0.2, iError2, .false.)
       nIteration=3
       call error_report('negative pressure', -6., iError1, .true.)
       call error_report('only PE 0', 100., iError3, .true.)
       call error_report('energy correction', 0.6, iError2, .false.)
    case(1)
       nIteration=1
       call error_report('only PE 1',200., iError3, .true.)
       call error_report('energy correction',0.01, iError2, .false.)
       nIteration=2
       call error_report('energy correction', 0.02, iError2, .false.)
       nIteration=3
       call error_report('energy correction', 0.06, iError2, .false.)
    end select

    call error_report('PRINT', 0., iError1, .true.)

  end subroutine test_error_report
  !============================================================================
  subroutine get_date_time(iTime_I)

    use ModMain, ONLY: StartTime, tSimulation
    use ModTimeConvert, ONLY: time_real_to_int

    integer, intent(out) :: iTime_I(7)
    !--------------------------------------------------------------------------
    call time_real_to_int(StartTime + tSimulation, iTime_I)

  end subroutine get_date_time
  !============================================================================
  subroutine get_time_string

    use ModIO, ONLY: StringDateOrTime, NameMaxTimeUnit
    use ModMain, ONLY: StartTime, tSimulation
    use ModTimeConvert, ONLY: TimeType, time_real_to_int

    integer:: i
    type(TimeType):: Time
    !--------------------------------------------------------------------------
    StringDateOrTime = '99999999'  ! This is the value if the time is too large

    select case(NameMaxTimeUnit)
    case('hour')
       if(tSimulation < 10000.0*3600) &
            write(StringDateOrTime,'(i4.4,i2.2,i2.2)') &
            int(                            tSimulation/3600.), &
            int((tSimulation-(3600.*int(tSimulation/3600.)))/60.), &
            int( tSimulation-(  60.*int(tSimulation/  60.)))
    case('hr')
       if(tSimulation < 100.0*3600) &
            write(StringDateOrTime,'(i2.2,i2.2,f4.1)') &
            int(                            tSimulation/3600.), &
            int((tSimulation-(3600.*int(tSimulation/3600.)))/60.), &
            tSimulation-(  60.*int(tSimulation/  60.))
    case('minute')
       if(tSimulation < 100.0*60) &
            write(StringDateOrTime,'(i2.2,f6.3)') &
            int(tSimulation/60.), &
            tSimulation-(60.*int(tSimulation/60.))
    case('second')
       if(tSimulation < 100.0) &
            write(StringDateOrTime,'(f8.5)') tSimulation
    case('millisecond')
       if(tSimulation < 1.0) &
            write(StringDateOrTime,'(f8.4)') tSimulation*1e3
    case('microsecond')
       if(tSimulation < 1e-3) &
            write(StringDateOrTime,'(f8.4)') tSimulation*1e6
    case('nanosecond')
       if(tSimulation < 1e-6) &
            write(StringDateOrTime,'(f8.4)') tSimulation*1e9
    case default
       ! Could not find unit
       StringDateOrTime = ''
    end select

    if(StringDateOrTime /= '')then
       ! The time tag is 8-character long for the above cases
       ! Replaces spaces with 0-s up to 8 characters
       do i=1,8
          if(StringDateOrTime(i:i)==' ') StringDateOrTime(i:i)='0'
       end do
       RETURN
    end if

    ! Convert current date and time into string Time % String
    Time % Time = StartTime + tSimulation
    call time_real_to_int(Time)

    ! Select part of the string
    select case(NameMaxTimeUnit)
    case('date')
       StringDateOrTime = Time % String(1:14)
    case('year')
       StringDateOrTime = Time % String(1:8)
    case('yr')
       StringDateOrTime = Time % String(3:10)
    case('month')
       StringDateOrTime = Time % String(5:12)
    case('day')
       StringDateOrTime = Time % String(7:14)
    case default
       ! the unit is wrong, but what can we do? Let's write something.
       StringDateOrTime = '00000000'
    end select

  end subroutine get_time_string
  !============================================================================
  subroutine get_ivar(NameVar, iVar)

    use ModMain, ONLY: NameVarLower_V
    use ModVarIndexes, ONLY: NameFluid_I, nVar
    use ModMultiFluid, ONLY: extract_fluid_name
    use ModUtilities, ONLY: lower_case

    character(len=*), intent(inout)  :: NameVar
    integer,intent(out)              :: iVar

    integer :: iVarLoop, iError, iFluid

    ! Initialize iVar

    character(len=*), parameter:: NameSub = 'get_ivar'
    !--------------------------------------------------------------------------
    iVar = -1

    ! In case NameVar is in upper case...
    call lower_case(NameVar)

    ! Remove the fluid name first
    call extract_fluid_name(NameVar, iFluid)

    ! In case the user specifies ux/uy/uz instead of mx/my/mz
    select case(NameVar)
    case('ux')
       NameVar = 'mx'
    case('uy')
       NameVar = 'my'
    case('uz')
       NameVar = 'mz'
    end select

    ! Put back the fluid name
    if(iFluid /= 1) NameVar = trim(NameFluid_I(iFluid))//NameVar

    ! The first character in NameFluid_I is in upper case...
    call lower_case(NameVar)

    ! Find NameVar in NameVarLower_V if it is there
    do iVarLoop = 1, nVar
       if (NameVar == NameVarLower_V(iVarLoop)) then
          iVar = iVarLoop
          EXIT
       end if
    end do

    if(iVar < 0)then
       ! Try reading iVar as an index.
       read(NameVar,*,IOSTAT=iError) iVar

       ! If it is not an integer and not one of the element in NameVarLower_V
       if (iError /= 0) call stop_mpi(NameSub//': unknown NameVar =' &
            //trim(NameVar))
    end if

    ! iVar must be within 1 and nVar
    if(iVar < 0 .or. iVar > nVar) call stop_mpi(NameSub//': check NameVar, ' &
         //'iVar is not within 1 and nVar???')

  end subroutine get_ivar
  !============================================================================
end module ModBatsrusUtility
!==============================================================================
