!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModVarIndexes

  use ModExtraVariables, &
       Redefine => iPparIon_I

  implicit none

  save

  ! This equation module contains the standard MHD equations.
  character (len=*), parameter :: NameEquation='Multi-ion MHD'

  integer, parameter :: nVar = 13

  ! There are two ion fluids but no total ion fluid
  integer, parameter :: nFluid    = 2
  integer, parameter :: IonFirst_ = 1
  integer, parameter :: IonLast_  = 2
  logical, parameter :: IsMhd     = .false.
  real               :: MassFluid_I(nFluid) = [ 1.0, 1.0 ]

  character (len=3), parameter :: NameFluid_I(nFluid) = [ 'SWH', 'Pu3' ]

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
       Pu3Rho_    =  9, &
       Pu3RhoUx_  = 10, Pu3Ux_ = 10, &
       Pu3RhoUy_  = 11, Pu3Uy_ = 11, &
       Pu3RhoUz_  = 12, Pu3Uz_ = 12, &
       Pu3P_      = 13, &
       Energy_    = nVar+1, &
       Pu3Energy_ = nVar+2

  ! This allows to calculate RhoUx_ as RhoU_+x_ and so on.
  integer, parameter :: U_ = Ux_ - 1, RhoU_ = RhoUx_-1, B_ = Bx_-1

  ! These arrays are useful for multifluid
  integer, parameter :: iRho_I(nFluid)   = [Rho_,   Pu3Rho_]
  integer, parameter :: iRhoUx_I(nFluid) = [RhoUx_, Pu3RhoUx_]
  integer, parameter :: iRhoUy_I(nFluid) = [RhoUy_, Pu3RhoUy_]
  integer, parameter :: iRhoUz_I(nFluid) = [RhoUz_, Pu3RhoUz_]
  integer, parameter :: iP_I(nFluid)     = [p_,     Pu3P_]

  integer, parameter :: iPparIon_I(IonFirst_:IonLast_) = [1,2]

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
       1.0, & ! Pu3Rho_
       0.0, & ! Pu3RhoUx_
       0.0, & ! Pu3RhoUy_
       0.0, & ! Pu3RhoUz_
       1.0, & ! Pu3P_
       1.0, & ! Energy_
       1.0 ]  ! Pu3Energy_

  ! The names of the variables used in i/o
  character(len=6) :: NameVar_V(nVar+nFluid) = [ &
       'Rho   ', & ! Rho_
       'Mx    ', & ! RhoUx_
       'My    ', & ! RhoUy_
       'Mz    ', & ! RhoUz_
       'Bx    ', & ! Bx_
       'By    ', & ! By_
       'Bz    ', & ! Bz_
       'P     ', & ! p_
       'Pu3Rho', & ! Pu3Rho_
       'Pu3Mx ', & ! Pu3RhoUx_
       'Pu3My ', & ! Pu3RhoUy_
       'Pu3Mz ', & ! Pu3RhoUz_
       'Pu3P  ', & ! Pu3P_
       'Pu3E  ', & ! Energy_
       'Pu3E  ' ]  ! Pu3Energy_

  ! There are no extra scalars
  integer, parameter :: ScalarFirst_ = 2, ScalarLast_ = 1


end module ModVarIndexes