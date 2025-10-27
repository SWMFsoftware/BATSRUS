!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
!#NOTPUBLIC  email:mopher@bu.edu  expires:12/31/2099
module ModVarIndexes

  use ModExtraVariables, &
       Redefine => iPparIon_I, &
       Redefine2 => Pe_, &
       Redefine3 => LevelHP_

  implicit none

  save

  character (len=*), parameter :: &
       NameEquationFile = "ModEquationOuterHelio2PUIPe.f90"

  ! This equation module contains 3 ion fluids + electron pressure + 4 neutrals
  character (len=*), parameter :: &
       NameEquation = 'SWH + 2 PUI + Pe and four neutrals'

  integer, parameter :: nVar = 40

  integer, parameter :: nFluid    = 7
  integer, parameter :: nIonFluid = 3
  logical, parameter :: IsMhd     = .false.
  real               :: MassFluid_I(nFluid) = 1.0

  ! SWH is the Solar wind hydrogen fluid,
  ! Pu3 are pick up ions produced in region 3 (see mod user),
  ! and Neu, Ne2, Ne3, Ne4 are neutrals produced in the corresponding regions

  character (len=3), parameter :: &
       NameFluid_I(nFluid) = ['SWH', 'Pu3', 'Pu2', 'Neu', 'Ne2', 'Ne3', 'Ne4']

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
       Pe_        =  9, &
       P_         =  10,          SWHP_  = 10, &
       Pu3Rho_    =  11, &
       Pu3RhoUx_  = 12, Pu3Ux_ = 12, &
       Pu3RhoUy_  = 13, Pu3Uy_ = 13, &
       Pu3RhoUz_  = 14, Pu3Uz_ = 14, &
       Pu3P_      = 15, &
       Pu2Rho_    =  16, &
       Pu2RhoUx_  = 17, Pu2Ux_ = 17, &
       Pu2RhoUy_  = 18, Pu2Uy_ = 18, &
       Pu2RhoUz_  = 19, Pu2Uz_ = 19, &
       Pu2P_      = 20

  integer, parameter:: &
       NeuRho_    = 21, &
       NeuRhoUx_  = 22, NeuUx_ = 22, &
       NeuRhoUy_  = 23, NeuUy_ = 23, &
       NeuRhoUz_  = 24, NeuUz_ = 24, &
       NeuP_      = 25, &
       Ne2Rho_    = 26, &
       Ne2RhoUx_  = 27, Ne2Ux_ = 27, &
       Ne2RhoUy_  = 28, Ne2Uy_ = 28, &
       Ne2RhoUz_  = 29, Ne2Uz_ = 29, &
       Ne2P_      = 30, &
       Ne3Rho_    = 31, &
       Ne3RhoUx_  = 32, Ne3Ux_ = 32, &
       Ne3RhoUy_  = 33, Ne3Uy_ = 33, &
       Ne3RhoUz_  = 34, Ne3Uz_ = 34, &
       Ne3P_      = 35, &
       Ne4Rho_    = 36, &
       Ne4RhoUx_  = 37, Ne4Ux_ = 37, &
       Ne4RhoUy_  = 38, Ne4Uy_ = 38, &
       Ne4RhoUz_  = 39, Ne4Uz_ = 39, &
       Ne4P_      = 40, &
       Energy_    = nVar+1, SWHEnergy_ = nVar+1, &
       Pu3Energy_ = nVar+2, &
       Pu2Energy_ = nVar+3, &
       NeuEnergy_ = nVar+4, &
       Ne2Energy_ = nVar+5, &
       Ne3Energy_ = nVar+6, &
       Ne4Energy_ = nVar+7

  ! This allows to calculate RhoUx_ as RhoU_+x_ and so on.
  integer, parameter :: U_ = Ux_ - 1, RhoU_ = RhoUx_-1, B_ = Bx_-1

  ! These arrays are needed for multifluid
  integer, parameter :: &
       iRho_I(nFluid)   &
       = [ Rho_,   Pu3Rho_,   Pu2Rho_, &
       NeuRho_,    Ne2Rho_,   Ne3Rho_,   Ne4Rho_   ], iRhoUx_I(nFluid) &
       = [ RhoUx_, Pu3RhoUx_, Pu2RhoUx_, &
       NeuRhoUx_,  Ne2RhoUx_, Ne3RhoUx_, Ne4RhoUx_ ], iRhoUy_I(nFluid) &
       = [ RhoUy_, Pu3RhoUy_, Pu2RhoUy_, &
       NeuRhoUy_,  Ne2RhoUy_, Ne3RhoUy_, Ne4RhoUy_ ], iRhoUz_I(nFluid) &
       = [ RhoUz_, Pu3RhoUz_, Pu2RhoUz_, &
       NeuRhoUz_,  Ne2RhoUz_, Ne3RhoUz_, Ne4RhoUz_ ], iP_I(nFluid) &
       = [ P_,     Pu3P_,     Pu2P_, &
       NeuP_,      Ne2P_,     Ne3P_,     Ne4P_     ]

  integer, parameter :: iPparIon_I(nIonFluid) = [1,2,3]

  ! The default values for the state variables:
  ! Variables which are physically positive should be set to 1,
  ! variables that can be positive or negative should be set to 0:
  real, parameter :: DefaultState_V(nVar+nFluid) = [ &
       1.0,           & ! SWHRho_
       0.0, 0.0, 0.0, & ! SWHRhoUx_ .. SWHRhoUz_
       0.0, 0.0, 0.0, & ! Bx_ .. Bz_
       0.0,           & ! LevelHP_
       1.0,           & ! Pe_
       1.0,           & ! SWHP_
       1.0,           & ! Pu3Rho_
       0.0, 0.0, 0.0, & ! Pu3RhoUx_ .. Pu3RhoUz_
       1.0,           & ! Pu3P_
       1.0,           & ! Pu2Rho_
       0.0, 0.0, 0.0, & ! Pu2RhoUx_ .. Pu2RhoUz_
       1.0,           & ! Pu2P_
       1.0,           & ! NeuRho_
       0.0, 0.0, 0.0, & ! NeuRhoUx_ .. NeuRhoUz_
       1.0,           & ! NeuP_
       1.0,           & ! Ne2Rho_
       0.0, 0.0, 0.0, & ! Ne2RhoUx_ .. Ne2RhoUz_
       1.0,           & ! Ne2P_
       1.0,           & ! Ne3Rho_
       0.0, 0.0, 0.0, & ! Ne3RhoUx_ .. Ne3RhoUz_
       1.0,           & ! Ne3P_
       1.0,           & ! Ne4Rho_
       0.0, 0.0, 0.0, & ! Ne4RhoUx_ .. Ne4RhoUz_
       1.0,           & ! Ne4P_
       1.0,           & ! SWHEnergy_
       1.0,           & ! Pu3Energy_
       1.0,           & ! Pu2Energy_
       1.0,           & ! NeuEnergy_
       1.0,           & ! Neu2Energy_
       1.0,           & ! Neu3Energy_
       1.0            ] ! Neu4Energy_

  ! The names of the variables used in i/o
  character(len=6):: NameVar_V(nVar+nFluid) = [ &
       'Rho   ', & ! SWHRho_
       'Mx    ', 'My    ', 'Mz    ', & ! RhoUx_ RhoUz_
       'Bx    ', 'By    ', 'Bz    ', & ! Bx_  Bz_
       'HPLim ',                     & ! LevelHP_
       'Pe    ',                     & ! Pe_
       'P     ',                     & ! p_
       'Pu3Rho',                     & ! Pu3Rho_
       'Pu3Mx ', 'Pu3My ', 'Pu3Mz ', & ! Pu3RhoUx_ Pu3RhoUz_
       'Pu3P  ',                     & ! Pu3P_
       'Pu2Rho',                     & ! Pu2Rho_
       'Pu2Mx ', 'Pu2My ', 'Pu2Mz ', & ! Pu2RhoUx_ Pu3RhoUz_
       'Pu2P  ',                     & ! Pu2P_
       'NeuRho',                     & ! NeuRho_
       'NeuMx ', 'NeuMy ', 'NeuMz ', & ! NeuRhoUx_ NeuRhoUz_
       'NeuP  ',                     & ! NeuP_
       'Ne2Rho',                     & ! Ne2Rho_
       'Ne2Mx ', 'Ne2My ', 'Ne2Mz ', & ! Ne2RhoUx_ Ne2RhoUz_
       'Ne2P  ',                     & ! Ne2P_
       'Ne3Rho',                     & ! Ne3Rho_
       'Ne3Mx ', 'Ne3My ', 'Ne3Mz ', & ! Ne3RhoUx_ Ne3RhoUz_
       'Ne3P  ',                     & ! Ne3P_
       'Ne4Rho',                     & ! Ne4Rho_
       'Ne4Mx ', 'Ne4My ', 'Ne4Mz ', & ! Ne4RhoUx_ Ne4RhoUz_
       'Ne4P  ',                     & ! Ne4P_
       'E     ',                     & ! Energy_
       'Pu3E  ',                     & ! Pu3Energy_
       'Pu2E  ',                     & ! Pu2Energy_
       'NeuE  ',                     & ! NeuEnergy_
       'Ne2E  ',                     & ! Ne2Energy_
       'Ne3E  ',                     & ! Ne3Energy_
       'Ne4E  '                      ] ! Ne4Energy_

  ! There are extra scalars
  integer, parameter :: ScalarFirst_ = LevelHP_, ScalarLast_ = LevelHP_

end module ModVarIndexes
!==============================================================================
