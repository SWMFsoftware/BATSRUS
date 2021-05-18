module ModOptimizeParam

  ! Optimize code performance by declaring various algorithmic choices
  ! as fixed parameters. This file is copied into ModOptimizeParam.f90
  ! and edited by GM/BATSRUS/Config.pl before compilation.

  use ModUtilities, ONLY: CON_stop

  use ModAdvance, ONLY: UseB
  use BATL_lib,   ONLY: nDim

  use ModAdvance, ONLY: &
       UseNonConservative => UseNonConservative, &
       nConservCrit => nConservCrit
  use ModB0, ONLY: &
       UseB0 => UseB0
  use ModFaceFlux, ONLY: &
       DoLf => DoLf
  use ModFaceValue, ONLY: &
       LimiterBeta => BetaLimiter
  use ModMain, ONLY: &
       IsTimeAccurate => time_accurate, &
       nStage => nStage, &
       iStage => iStage, &
       nOrder => nOrder, &
       UseDivbSource => UseDivbSource, &
       UseHyperbolicDivB => UseHyperBolicDivB, &
       UseDtFixed => UseDtFixed, &
       UseBody => Body1
  use ModBorisCorrection, ONLY: &
       UseBorisCorrection => UseBorisCorrection, &
       ClightFactor => ClightFactor
  use BATL_lib, ONLY: &
       IsCartesian => IsCartesian, &
       IsCartesianGrid => IsCartesianGrid

  implicit none

  ! Fixed values

contains
  !============================================================================
  subroutine check_optimize_param

    character(len=*), parameter:: NameSub = 'check_optimize_param'
    !--------------------------------------------------------------------------
    write(*,'(a)')NameSub//' checking parameters...'

    ! Check fixed values

  end subroutine check_optimize_param
  !============================================================================
end module ModOptimizeParam
!==============================================================================
