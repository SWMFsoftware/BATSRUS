!  Copyright (C) 2002 Regents of the University of Michigan
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModVarIndexes

  use ModSingleFluid, Redefine => IsMhd
  use ModExtraVariables

  implicit none

  save

  character (len=*), parameter :: &
       NameEquationFile = "ModEquationScalar.f90"

  ! This equation module contains the standard hydro equations.
  character (len=*), parameter :: &
       NameEquation = 'Scalar'

  integer, parameter :: nVar = 1

  logical, parameter :: IsMhd     = .false.

  ! Named indexes for State_VGB and other variables
  ! These indexes should go subsequently, from 1 to nVar+nFluid.
  ! The energies are handled as an extra variable, so that we can use
  ! both conservative and non-conservative scheme and switch between them.
  integer, parameter :: &
       Rho_       =  1,          &
       Energy_    = nVar+1

  ! These need to be defined
  integer, parameter :: RhoUx_=1, RhoUy_=1, RhoUz_=3
  integer, parameter :: Ux_=1, Uy_=2, Uz_=3, p_=1

  ! This allows to calculate RhoUx_ as RhoU_+x_ and so on.
  integer, parameter :: U_ = Ux_ - 1, RhoU_ = RhoUx_-1

  ! The default values for the state variables:
  ! Variables which are physically positive should be set to 1,
  ! variables that can be positive or negative should be set to 0:
  real, parameter :: DefaultState_V(nVar+nFluid) = [ &
       1.0, & ! Rho_
       1.0 ] ! Energy_

  ! The names of the variables used in i/o
  character(len=3) :: NameVar_V(nVar+nFluid) = [ &
       'Rho', & ! Rho_
       'E  ']  ! Energy_

  ! Bx_, By_, Bz_ have to be defined so that the code compiles
  ! but the Bx_ = Ux_ choice indicates that B is not used (see UseB in ModMain)
  integer, parameter :: Bx_ = Ux_, By_ = Uy_, Bz_ = Uz_, B_ = U_

  ! There are no extra scalars
  integer, parameter :: ScalarFirst_ = 2, ScalarLast_ = 1

  integer, parameter :: iRho_I(nFluid)   = [Rho_]
  integer, parameter :: iRhoUx_I(nFluid) = [RhoUx_]
  integer, parameter :: iRhoUy_I(nFluid) = [RhoUy_]
  integer, parameter :: iRhoUz_I(nFluid) = [RhoUz_]
  integer, parameter :: iP_I(nFluid)     = [p_]

end module ModVarIndexes
!==============================================================================
