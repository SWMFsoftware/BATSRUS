module ModUpdateParamFast

  ! Contains all variables that may be fixed
  ! so that ModUpdateStateFast runs faster

  use ModUtilities, ONLY: CON_stop

  use ModAdvance, ONLY: &
       UseNonConservative => UseNonConservative
  use ModB0, ONLY: &
       UseB0 => UseB0
  use ModFaceFlux, ONLY: &
       DoLf => DoLf
  use ModFaceValue, ONLY: &
       BetaLimiter => BetaLimiter
  use ModMain, ONLY: &
       IsTimeAccurate => time_accurate, &
       nStage => nStage, &
       iStage => iStage, &
       nOrder => nOrder, &
       UseHyperBolicDivB => UseHyperBolicDivB
  use ModBorisCorrection, ONLY: &
       UseBorisCorrection => UseBorisCorrection
  use BATL_lib, ONLY: &
       IsCartesian => IsCartesian, &
       IsCartesianGrid => IsCartesianGrid

  implicit none

  ! Fixed values

contains
  !============================================================================
  subroutine check_update_param_fast

    character(len=*), parameter:: NameSub = 'check_update_param_fast'
    !--------------------------------------------------------------------------
    write(*,'(a)')NameSub//' checking parameters...'

    ! Check fixed values

  end subroutine check_update_param_fast
  !============================================================================
end module ModUpdateParamFast
!==============================================================================
