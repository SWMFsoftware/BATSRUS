!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModSingleFluid

  ! Define single fluid values for multifluid parameters

  implicit none

  integer,          parameter :: nFluid    = 1
  integer,          parameter :: nIonFluid = 1
  logical,          parameter :: IsMhd     = .true.
  character(len=1), parameter :: NameFluid_I(nFluid) = [' ']

  ! Default is proton mass, but it can be changed
  real :: MassFluid_I(nFluid) = 1.0
  !$acc declare create(MassFluid_I)

end module ModSingleFluid
!==============================================================================

