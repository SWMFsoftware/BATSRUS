!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModVarIndexes

  use ModExtraVariables, &
       Redefine => iPparIon_I, &
       Redefine2 => Pe_, &
       Redefine3 => LevelHP_

  implicit none

  save

  character (len=*), parameter :: &
       NameEquationFile = "ModEquationSwh2PuiPe.f90"

  character (len=*), parameter :: &
       NameEquation = 'Multi-ion MHD + Pe'

  integer, parameter :: nVar = 20

  ! There are two ion fluids but no total ion fluid
  integer, parameter :: nFluid    = 3
  integer, parameter :: nIonFluid = 3
  logical, parameter :: IsMhd     = .false.
  real               :: MassFluid_I(nFluid) = [ 1.0, 1.0, 1.0]

  character (len=3), parameter :: NameFluid_I(nFluid) = [ 'SWH', 'Pu3', 'Pu2']

  ! Named indexes for State_VGB and other variables
  ! These indexes should go subsequently, from 1 to nVar+nFluid.
  ! The energies are handled as an extra variable, so that we can use
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
       pe_        =  9, &
       p_         =  10,          SWHP_ = 10, &
       Pu3Rho_    =  11, &
       Pu3RhoUx_  = 12, Pu3Ux_ = 12, &
       Pu3RhoUy_  = 13, Pu3Uy_ = 13, &
       Pu3RhoUz_  = 14, Pu3Uz_ = 14, &
       Pu3P_      = 15, &
       Pu2Rho_    = 16, &
       Pu2RhoUx_  = 17, Pu2Ux_ = 17, &
       Pu2RhoUy_  = 18, Pu2Uy_ = 18, &
       Pu2RhoUz_  = 19, Pu2Uz_ = 19, &
       Pu2P_      = 20, &
       Energy_    = nVar+1,      SWHEnergy_ = nVar+1, &
       Pu3Energy_ = nVar+2, &
       Pu2Energy_ = nVar+3

  ! Neutral indexes are also declared for sake of compilation with ModUser
  ! written for multi-fluid neutrals
  integer, parameter :: &
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

  ! This allows to calculate RhoUx_ as RhoU_+x_ and so on.
  integer, parameter :: U_ = Ux_ - 1, RhoU_ = RhoUx_-1, B_ = Bx_-1

  ! These arrays are useful for multifluid
  integer, parameter :: iRho_I(nFluid)   = &
       [Rho_,   Pu3Rho_, Pu2Rho_]
  integer, parameter :: iRhoUx_I(nFluid) = &
       [RhoUx_, Pu3RhoUx_, Pu2RhoUx_]
  integer, parameter :: iRhoUy_I(nFluid) = &
       [RhoUy_, Pu3RhoUy_, Pu2RhoUy_]
  integer, parameter :: iRhoUz_I(nFluid) = &
       [RhoUz_, Pu3RhoUz_, Pu2RhoUz_]
  integer, parameter :: iP_I(nFluid)     = &
       [p_, Pu3P_, Pu2P_]

  integer, parameter :: iPparIon_I(nIonFluid) = [1,2,3]

  ! The default values for the state variables:
  ! Variables which are physically positive should be set to 1,
  ! variables that can be positive or negative should be set to 0:
  real, parameter :: DefaultState_V(nVar+nFluid) = [ &
       1.0, & ! SWHRho_
       0.0, & ! SWHRhoUx_
       0.0, & ! SWHRhoUy_
       0.0, & ! SWHRhoUz_
       0.0, & ! SWHBx_
       0.0, & ! SWHBy_
       0.0, & ! SWHBz_
       0.0, & ! LevelHP_
       1.0, & ! Pe_
       1.0, & ! SWHp_
       1.0, & ! Pu3Rho_
       0.0, & ! Pu3RhoUx_
       0.0, & ! Pu3RhoUy_
       0.0, & ! Pu3RhoUz_
       1.0, & ! Pu3P_
       1.0, & ! Pu2Rho_
       0.0, & ! Pu2RhoUx_
       0.0, & ! Pu2RhoUy_
       0.0, & ! Pu2RhoUz_
       1.0, & ! Pu2P_
       1.0, & ! Energy_
       1.0, & ! Pu3Energy_
       1.0 ]  ! Pu2Energy_

  ! The names of the variables used in i/o
  character(len=6) :: NameVar_V(nVar+nFluid) = [ &
       'Rho   ', & ! SWHRho_
       'Mx    ', & ! SWHRhoUx_
       'My    ', & ! SWHRhoUy_
       'Mz    ', & ! SWHRhoUz_
       'Bx    ', & ! SWHBx_
       'By    ', & ! SWHBy_
       'Bz    ', & ! SWHBz_
       'HPLim ', & ! LevelHP_
       'Pe    ', & ! Pe_
       'P     ', & ! SWHp_
       'Pu3Rho', & ! Pu3Rho_
       'Pu3Mx ', & ! Pu3RhoUx_
       'Pu3My ', & ! Pu3RhoUy_
       'Pu3Mz ', & ! Pu3RhoUz_
       'Pu3P  ', & ! Pu3P_
       'Pu2Rho', & ! Pu2Rho_
       'Pu2Mx ', & ! Pu2RhoUx_
       'Pu2My ', & ! Pu2RhoUy_
       'Pu2Mz ', & ! Pu3RhoUz_
       'Pu2P  ', & ! Pu2P_
       'E     ', & ! Energy_
       'Pu3E  ', & ! Pu3Energy_
       'Pu2E  ' ]  ! Pu2Energy_

  ! There are extra scalars
  integer, parameter :: ScalarFirst_ = LevelHP_, ScalarLast_ = LevelHP_

end module ModVarIndexes
!==============================================================================
