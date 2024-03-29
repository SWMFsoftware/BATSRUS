!  Copyright (C) 2002 Regents of the University of Michigan
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModVarIndexes

  use ModExtraVariables

  implicit none

  save

  character (len=*), parameter :: &
       NameEquationFile = "ModEquationMhdHd.f90"

  character (len=*), parameter :: &
       NameEquation = 'MHD and HD'

  ! Number of variables without energy:
  integer, parameter :: nVar = 13

  integer, parameter :: nFluid      = 2       ! 1 ion and 1 neutral fluid
  integer, parameter :: nIonFluid   = 1
  logical, parameter :: IsMhd       = .true.  ! the first fluid obeys MHD
  real               :: MassFluid_I(nFluid) = [ 1.0, 1.0 ]

  character (len=3), parameter :: NameFluid_I(nFluid) = [ 'Ion', 'Neu' ]

  ! Named indexes for State_VGB and other variables
  ! These indexes should go subsequently, from 1 to nVar+nFluid.
  ! The energies are handled as an extra variable, so that we can use
  ! both conservative and non-conservative scheme and switch between them.
  integer, parameter :: &
       Rho_       =  1,          &
       RhoUx_     =  2, Ux_ = 2, &
       RhoUy_     =  3, Uy_ = 3, &
       RhoUz_     =  4, Uz_ = 4, &
       Bx_        =  5, &
       By_        =  6, &
       Bz_        =  7, &
       p_         =  8, &
       NeuRho_    =  9, &
       NeuRhoUx_  = 10, &
       NeuRhoUy_  = 11, &
       NeuRhoUz_  = 12, &
       NeuP_      = 13, &
       Energy_    = nVar+1, &
       NeuEnergy_ = nVar+2

  ! This allows to calculate RhoUx_ as RhoU_+x_ and so on.
  integer, parameter :: U_ = Ux_ - 1, RhoU_ = RhoUx_-1, B_ = Bx_-1

  ! These arrays are useful for multifluid
  integer, parameter :: iRho_I(nFluid)   = [Rho_,   NeuRho_]
  integer, parameter :: iRhoUx_I(nFluid) = [RhoUx_, NeuRhoUx_]
  integer, parameter :: iRhoUy_I(nFluid) = [RhoUy_, NeuRhoUy_]
  integer, parameter :: iRhoUz_I(nFluid) = [RhoUz_, NeuRhoUz_]
  integer, parameter :: iP_I(nFluid)     = [p_,     NeuP_]

  ! The default values for the state variables:
  ! Variables which are physically positive should be set to 1,
  ! variables that can be positive or negative should be set to 0:
  real, parameter :: DefaultState_V(nVar+nFluid) = [ &
       1.0, & ! Rho_
       0.0, & ! RhoUx_
       0.0, & ! RhoUy_
       0.0, & ! RhoUz_
       0.0, & ! Bx_
       0.0, & ! By_
       0.0, & ! Bz_
       1.0, & ! p_
       1.0, & ! NeuRho_
       0.0, & ! NeuRhoUx_
       0.0, & ! NeuRhoUy_
       0.0, & ! NeuRhoUz_
       1.0, & ! NeuP_
       1.0, & ! Energy_
       1.0 ] ! NeuEnergy_

  ! The names of the variables used in i/o
  character(len=6) :: NameVar_V(nVar+nFluid) = [ &
       'Rho   ', & ! Rho_
       'Mx    ', & ! RhoUx_
       'My    ', & ! RhoUy_
       'Mz    ', & ! RhoUz_
       'Bx    ', & ! Bx_
       'By    ', & ! By_
       'Bz    ', & ! Bz_
       'p     ', & ! p_
       'NeuRho', & ! NeuRho_
       'NeuMx ', & ! NeuRhoUx_
       'NeuMy ', & ! NeuRhoUy_
       'NeuMz ', & ! NeuRhoUz_
       'NeuP  ', & ! NeuP_
       'e     ', & ! Energy_
       'NeuE  ' ] ! NeuEnergy_

  ! There are no extra scalars
  integer, parameter :: ScalarFirst_ = 2, ScalarLast_ = 1

end module ModVarIndexes
!==============================================================================
