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
  !public:: im_pressure_init
  !public:: apply_im_pressure
  public:: gm_init_ua_array

  ! The number of IM pressures obtained so far
  integer, public :: iNewPIm = 0
  integer, public :: dummyvar1 = 0
  integer, public :: dummyvar2 = 0

  integer, public :: MaxSpecies_coupler=4, MaxNuSpecies_coupler=8, MaxReactions_coupler=10

  real, public, allocatable:: nDenNuSpecies_fromUA(:,:,:,:,:)

  ! Local variables --------------------------------------------

contains
  !============================================================================

  subroutine gm_init_ua_array(nI,nJ,nK,MaxBlock,MaxNuSpecies)

    integer, intent(in):: nI,nJ,nK,MaxBlock,MaxNuSpecies
    allocate(nDenNuSpecies_fromUA(nI, nJ, nK, MaxBlock, MaxNuSpecies))

  end subroutine gm_init_ua_array

  !============================================================================

end module ModUaCoupling
!==============================================================================
