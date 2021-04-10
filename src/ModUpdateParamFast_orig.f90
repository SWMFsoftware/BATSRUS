module ModUpdateParamFast

  ! Contains all variables that may be fixed
  ! so that ModUpdateStateFast runs faster

  use ModUtilities, ONLY: CON_stop

  use ModAdvance, ONLY: &
       UseNonConservative
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
       UseBorisCorrection
  use BATL_lib, ONLY: &
       IsCartesian, &
       IsCartesianGrid

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
