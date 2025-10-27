!  Copyright (C) 2002 Regents of the University of Michigan
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModVarIndexes

  use ModExtraVariables, &
       Redefine1  => Pe_, &
       Redefine2  => nPui, &
       Redefine3  => PuiFirst_, &
       Redefine4  => PuiLast_, &
       Redefine5  => iPparIon_I, &
       Redefine6  => LevelHP_

  implicit none

  save

  character (len=*), parameter :: &
       NameEquationFile = "ModEquationOuterHelioPuiBin.f90"

  ! This equation module contains two ion fluids +
  ! Alfven wave energy + electron pressure + pickup ions spectrum + 4 neutrals
  character (len=*), parameter :: &
       NameEquation = 'MHD + Pe + PUI + four neutrals'

  ! loop variable for implied do-loop over spectrum and PUI
  integer, private :: iWave, iPui

  ! Number of PUI bins
  integer, parameter :: nPui = 1

  ! Number of variables without energy:
  integer, parameter :: nVar = 35 + nPui

  ! 2 ion fluid and 4 neutral fluids
  integer, parameter :: nFluid    = 6
  integer, parameter :: nIonFluid = 2
  logical, parameter :: IsMhd     = .false.
  real               :: MassFluid_I(nFluid) = 1.0

  ! SWH is the Solar wind hydrogen fluid, Pu3
  ! are pickup ions produced in region 3 (see mod user),
  ! and Neu, Ne2, Ne3, Ne4 are neutrals produced in the corresponding regions
  character (len=3), parameter :: &
       NameFluid_I(nFluid) = ['SWH', 'Pu3', 'Neu', 'Ne2', 'Ne3', 'Ne4']

  ! Named indexes for State_VGB and other variables
  ! These indexes should go subsequently, from 1 to nVar+nFluid.
  ! The energies are handled as an extra variable, so that we can use
  ! both conservative and non-conservative scheme and switch between  them.
  integer, parameter :: &
       Rho_       = 1,                    SWHRho_   = 1, &
       RhoUx_     = 2, Ux_ = 2,           SWHRhoUx_ = 2,  SWHUx_ = 2, &
       RhoUy_     = 3, Uy_ = 3,           SWHRhoUy_ = 3,  SWHUy_ = 3, &
       RhoUz_     = 4, Uz_ = 4,           SWHRhoUz_ = 4,  SWHUz_ = 4, &
       Bx_        = 5, &
       By_        = 6, &
       Bz_        = 7, &
       PuiFirst_  = 8, &
       PuiLast_   = PuiFirst_+nPui-1, &
       LevelHP_   = PuiLast_+1, &
       Pe_        = LevelHP_+1, &
       p_         = Pe_+1,                SWHP_     = p_, &
       Pu3Rho_    = p_+1, &
       Pu3RhoUx_  = p_+2, Pu3Ux_ = p_+2, &
       Pu3RhoUy_  = p_+3, Pu3Uy_ = p_+3, &
       Pu3RhoUz_  = p_+4, Pu3Uz_ = p_+4, &
       Pu3P_      = p_+5

  integer, parameter :: &
       NeuRho_    = Pu3P_+1, &
       NeuRhoUx_  = Pu3P_+2, NeuUx_ = Pu3P_+2, &
       NeuRhoUy_  = Pu3P_+3, NeuUy_ = Pu3P_+3, &
       NeuRhoUz_  = Pu3P_+4, NeuUz_ = Pu3P_+4, &
       NeuP_      = Pu3P_+5, &
       Ne2Rho_    = NeuP_+1, &
       Ne2RhoUx_  = NeuP_+2, Ne2Ux_ = NeuP_+2, &
       Ne2RhoUy_  = NeuP_+3, Ne2Uy_ = NeuP_+3, &
       Ne2RhoUz_  = NeuP_+4, Ne2Uz_ = NeuP_+4, &
       Ne2P_      = NeuP_+5, &
       Ne3Rho_    = Ne2P_+1, &
       Ne3RhoUx_  = Ne2P_+2, Ne3Ux_ = Ne2P_+2, &
       Ne3RhoUy_  = Ne2P_+3, Ne3Uy_ = Ne2P_+3, &
       Ne3RhoUz_  = Ne2P_+4, Ne3Uz_ = Ne2P_+4, &
       Ne3P_      = Ne2P_+5, &
       Ne4Rho_    = Ne3P_+1, &
       Ne4RhoUx_  = Ne3P_+2, Ne4Ux_ = Ne3P_+2, &
       Ne4RhoUy_  = Ne3P_+3, Ne4Uy_ = Ne3P_+3, &
       Ne4RhoUz_  = Ne3P_+4, Ne4Uz_ = Ne3P_+4, &
       Ne4P_      = Ne3P_+5, &
       Energy_    = nVar+1,  SWHEnergy_ = nVar+1, &
       Pu3Energy_ = nVar+2, &
       NeuEnergy_ = nVar+3, &
       Ne2Energy_ = nVar+4, &
       Ne3Energy_ = nVar+5, &
       Ne4Energy_ = nVar+6

  ! This allows to calculate RhoUx_ as RhoU_+x_ and so on.
  integer, parameter :: U_ = Ux_ - 1, RhoU_ = RhoUx_-1, B_ = Bx_-1

  ! These arrays are useful for multifluid
  integer, parameter :: &
       iRho_I(nFluid)   &
       = [ Rho_,   Pu3Rho_,   NeuRho_,    Ne2Rho_,   Ne3Rho_,   Ne4Rho_   ], &
       iRhoUx_I(nFluid) &
       = [ RhoUx_, Pu3RhoUx_, NeuRhoUx_,  Ne2RhoUx_, Ne3RhoUx_, Ne4RhoUx_ ], &
       iRhoUy_I(nFluid) &
       = [ RhoUy_, Pu3RhoUy_, NeuRhoUy_,  Ne2RhoUy_, Ne3RhoUy_, Ne4RhoUy_ ], &
       iRhoUz_I(nFluid) &
       = [ RhoUz_, Pu3RhoUz_, NeuRhoUz_,  Ne2RhoUz_, Ne3RhoUz_, Ne4RhoUz_ ], &
       iP_I(nFluid)     &
       = [ P_,     Pu3P_,     NeuP_,      Ne2P_,     Ne3P_,     Ne4P_     ]

  integer, parameter :: iPparIon_I(nIonFluid) = [1,2]

  ! The default values for the state variables:
  ! Variables which are physically positive should be set to 1,
  ! variables that can be positive or negative should be set to 0:
  real, parameter :: DefaultState_V(nVar+nFluid) = [ &
       1.0,           & ! SWHRho_
       0.0, 0.0, 0.0, & ! SWHRhoUx_ .. SWHRhoUz_
       0.0, 0.0, 0.0, & ! Bx_ .. Bz_
       (1.0, iPui=PuiFirst_,PuiLast_), &
       0.0,           & ! LevelHP_
       1.0,           & ! Pe_
       1.0,           & ! SWHP_
       1.0,           & ! Pu3Rho_
       0.0, 0.0, 0.0, & ! Pu3RhoUx_ .. Pu3RhoUz_
       1.0,           & ! Pu3P_
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
       1.0,           & ! NeuEnergy_
       1.0,           & ! Neu2Energy_
       1.0,           & ! Neu3Energy_
       1.0            ] ! Neu4Energy_

  ! The names of the variables used in i/o
  character(len=6) :: NameVar_V(nVar+nFluid) = [ &
       'Rho   ', & ! SWHRho_
       'Mx    ', 'My    ', 'Mz    ', & ! RhoUx_ RhoUz_
       'Bx    ', 'By    ', 'Bz    ', & ! Bx_  Bz_
       ('F??   ', iPui=PuiFirst_,PuiLast_), &
       'HPLim ',                     & ! LevelHP_
       'Pe    ',                     & ! Pe_
       'P     ',                     & ! p_
       'Pu3Rho',                     & ! Pu3Rho_
       'Pu3Mx ', 'Pu3My ', 'Pu3Mz ', & ! Pu3RhoUx_ Pu3RhoUz_
       'Pu3P  ',                     & ! Pu3P_
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
       'NeuE  ',                     & ! NeuEnergy_
       'Ne2E  ',                     & ! Ne2Energy_
       'Ne3E  ',                     & ! Ne3Energy_
       'Ne4E  '                      ] ! Ne4Energy_

  ! There are extra scalars
  integer, parameter :: ScalarFirst_ = LevelHP_, ScalarLast_ = LevelHP_

end module ModVarIndexes
!==============================================================================
