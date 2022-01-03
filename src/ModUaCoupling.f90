!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModUaCoupling

  use BATL_lib, ONLY: &
       test_start, test_stop

  ! Routines related to the coupline with the Upper Atmosphere component

  implicit none

  SAVE

  private ! except

  public:: gm_init_ua_array

  real, public, allocatable:: NumDenNuSpecies_CBI(:,:,:,:,:)

contains
  !============================================================================
  subroutine gm_init_ua_array(nI,nJ,nK,MaxBlock,MaxNuSpecies)

    integer, intent(in):: nI,nJ,nK,MaxBlock,MaxNuSpecies
    !--------------------------------------------------------------------------
    allocate(NumDenNuSpecies_CBI(nI,nJ,nK,MaxBlock,MaxNuSpecies))

  end subroutine gm_init_ua_array
  !============================================================================
end module ModUaCoupling
!==============================================================================
