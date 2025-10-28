!  Copyright (C) 2002 Regents of the University of Michigan
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModVarIndexes

  use ModSingleFluid

  use ModExtraVariables, &
       Redefine => LevelHP_

  implicit none

  save

  character (len=*), parameter :: &
       NameEquationFile = "ModEquationSwh.f90"

  character (len=*), parameter :: &
       NameEquation='Solar wind protons with extra indexes'

  ! Number of variables without energy:
  integer, parameter :: nVar = 9

  ! Named indexes for State_VGB and other variables
  ! These indexes should go subsequently, from 1 to nVar+1.
  ! The energy is handled as an extra variable, so that we can use
  ! both conservative and non-conservative scheme and switch between them.
  integer, parameter :: &
       Rho_       =  1,          SWHRho_   = 1, &
       RhoUx_     =  2, Ux_ = 2, SWHRhoUx_ = 2, SWHUx_ = 2, &
       RhoUy_     =  3, Uy_ = 3, SWHRhoUy_ = 3, SWHUy_ = 3, &
       RhoUz_     =  4, Uz_ = 4, SWHRhoUz_ = 4, SWHUz_ = 4, &
       Bx_        =  5, &
       By_        =  6, &
       Bz_        =  7, &
       LevelHP_   =  8, &
       p_         =  9,          SWHP_ = 9, &
       Energy_    = nVar+1,      SWHEnergy_ = nVar+1

  ! Extra indexes are also declared for sake of compilation with
  ! ModUserOuterhelio written for multi-fluid neutrals
  integer, parameter :: &
       Pu3Rho_    = Rho_  , &
       Pu3RhoUx_  = RhoUx_, Pu3Ux_ = Pu3RhoUx_, &
       Pu3RhoUy_  = RhoUy_, Pu3Uy_ = Pu3RhoUy_, &
       Pu3RhoUz_  = RhoUz_, Pu3Uz_ = Pu3RhoUz_, &
       Pu3P_      = p_    , Pu3Energy_ = Energy_,  &
       NeuRho_    = nVar  , &
       NeuRhoUx_  = nVar-2, NeuUx_ = NeuRhoUx_, &
       NeuRhoUy_  = nVar-1, NeuUy_ = NeuRhoUy_, &
       NeuRhoUz_  = nVar  , NeuUz_ = NeuRhoUz_, &
       NeuP_      = nVar  , &
       Ne2Rho_    = nVar  , &
       Ne2RhoUx_  = nVar-2, Ne2Ux_ = Ne2RhoUx_, &
       Ne2RhoUy_  = nVar-1, Ne2Uy_ = Ne2RhoUy_, &
       Ne2RhoUz_  = nVar  , Ne2Uz_ = Ne2RhoUz_, &
       Ne2P_      = nVar  , &
       Ne3Rho_    = nVar  , &
       Ne3RhoUx_  = nVar-2, Ne3Ux_ = Ne3RhoUx_, &
       Ne3RhoUy_  = nVar-1, Ne3Uy_ = Ne3RhoUy_, &
       Ne3RhoUz_  = nVar  , Ne3Uz_ = Ne3RhoUz_, &
       Ne3P_      = nVar  , &
       Ne4Rho_    = nVar  , &
       Ne4RhoUx_  = nVar-2, Ne4Ux_ = Ne4RhoUx_, &
       Ne4RhoUy_  = nVar-1, Ne4Uy_ = Ne4RhoUy_, &
       Ne4RhoUz_  = nVar  , Ne4Uz_ = Ne4RhoUz_, &
       Ne4P_      = nVar

  ! This allows to calculate RhoUx_ as rhoU_+x_ and so on.
  integer, parameter :: RhoU_ = RhoUx_-1, B_ = Bx_-1

  ! These arrays are useful for multifluid
  integer, parameter :: iRho_I(nFluid)   = [Rho_]
  integer, parameter :: iRhoUx_I(nFluid) = [RhoUx_]
  integer, parameter :: iRhoUy_I(nFluid) = [RhoUy_]
  integer, parameter :: iRhoUz_I(nFluid) = [RhoUz_]
  integer, parameter :: iP_I(nFluid)     = [p_]

  ! The default values for the state variables:
  ! Variables which are physically positive should be set to 1,
  ! variables that can be positive or negative should be set to 0:
  real, parameter :: DefaultState_V(nVar+1) = [ &
       1.0, & ! SWHRho_
       0.0, & ! SWHRhoUx_
       0.0, & ! SWHRhoUy_
       0.0, & ! SWHRhoUz_
       0.0, & ! SWHBx_
       0.0, & ! SWHBy_
       0.0, & ! SWHBz_
       0.0, & ! LevelHP_
       1.0, & ! SWHp_
       1.0 ]  ! SWHEnergy_

  ! The names of the variables used in i/o
  character(len=6) :: NameVar_V(nVar+1) = [ &
       'Rho   ', & ! Rho_
       'Mx    ', & ! RhoUx_
       'My    ', & ! RhoUy_
       'Mz    ', & ! RhoUz_
       'Bx    ', & ! Bx_
       'By    ', & ! By_
       'Bz    ', & ! Bz_
       'HPLim ', & ! LevelHP_
       'p     ', & ! p_
       'e     ' ]  ! Energy_

  ! Primitive variable names
  integer, parameter :: U_ = RhoU_

  ! There are extra scalars
  integer, parameter :: ScalarFirst_ = LevelHP_, ScalarLast_ = LevelHP_

end module ModVarIndexes
!==============================================================================

