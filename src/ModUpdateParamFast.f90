module ModUpdateParamFast

  use ModUtilities, ONLY: CON_stop
  
  use ModAdvance, ONLY: &
       UseNonConservativeOrig => UseNonConservative
  use ModFaceFlux, ONLY: &
       DoLf
  use ModFaceValue, ONLY: &
       BetaLimiter
  use ModMain, ONLY: &
       nStage, &
       iStage, &
       nOrder, &
       UseHyperBolicDivB
  use ModBorisCorrection, ONLY: &
       UseBorisCorrectionOrig => UseBorisCorrection
  use BATL_lib, ONLY: &
       IsCartesianOrig => IsCartesian, &
       IsCartesianGridOrig => IsCartesianGrid

  implicit none

  ! Fixed values
  logical, parameter:: IsCartesian = .true.
  logical, parameter:: IsCartesianGrid = .true.
  logical, parameter:: UseBorisCorrection = .false.
  logical, parameter:: UseNonConservative = .false.

contains
  !============================================================================
  subroutine check_update_param_fast

    character(len=*), parameter:: NameSub='check_update_param_fast'
    !--------------------------------------------------------------------------
    write(*,'(a)')NameSub//' checking parameters...'
    
    if(IsCartesian .neqv. IsCartesianOrig) &
         call CON_stop(NameSub//' IsCartesian=', IsCartesian)

    if(IsCartesianGrid .neqv. IsCartesianGridOrig) &
         call CON_stop(NameSub//': IsCartesianGrid=', IsCartesianGrid)

    if(UseBorisCorrection .neqv. UseBorisCorrectionOrig) &
         call CON_stop(NameSub//': UseBorisCorrection=', UseBorisCorrection)

    if(UseNonConservative .neqv. UseNonConservativeOrig) &
         call CON_stop(NameSub//': UseNonConservative=', UseNonConservative)
    
  end subroutine check_update_param_fast
  !============================================================================
end module ModUpdateParamFast
!==============================================================================
