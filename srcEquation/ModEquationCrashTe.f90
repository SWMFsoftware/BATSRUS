!  Copyright (C) 2002 Regents of the University of Michigan
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModVarIndexes
  use ModSingleFluid, &
       Redefine1 => IsMhd

  use ModExtraVariables, &
       Redefine2 => Erad_, &
       Redefine3 => nWave, &
       Redefine4 => WaveFirst_, &
       Redefine5 => WaveLast_, &
       Redefine6 => ExtraEint_, &
       Redefine7 => Pe_, &
       Redefine8 => nMaterial, &
       Redefine9 => MaterialFirst_, &
       Redefine10=> MaterialLast_,  &
       Redefine11=> Te0_

  implicit none

  save

  character (len=*), parameter :: &
       NameEquationFile = "ModEquationCrashTe.f90"

  ! This equation module contains the CRASH equations.
  character (len=*), parameter :: &
       NameEquation='HD+Ionization+Levels+Pe+Te0+Radiation'

  logical, parameter :: IsMhd = .false.

  ! loop variable for implied do-loop over material levels and spectrum
  integer, private :: iMaterial, iWave

  ! Number of material levels
  integer, parameter :: nMaterial = 3
  ! Number of wave bins in spectrum
  integer, parameter :: nWave = 1

  integer, parameter :: nVar = 8 + nMaterial + nWave

  ! Named indexes for State_VGB and other variables
  ! These indexes should go subsequently, from 1 to nVar+nFluid.
  ! The energies are handled as an extra variable, so that we can use
  ! both conservative and non-conservative scheme and switch between them.
  integer, parameter :: &
       Rho_           = 1,                          &
       RhoUx_         = 2, Ux_ = 2,                 &
       RhoUy_         = 3, Uy_ = 3,                 &
       RhoUz_         = 4, Uz_ = 4,                 &
       MaterialFirst_ = 5,                          &
       MaterialLast_  = MaterialFirst_+nMaterial-1, &
       Pe_            = MaterialLast_+1,            &
       Te0_           = Pe_+1,                      &
       WaveFirst_     = Te0_+1,                     &
       WaveLast_      = WaveFirst_+nWave-1,         &
       ExtraEint_     = WaveLast_+1,                &
       p_             = nVar,                       &
       Energy_        = nVar+1

  ! This is for backward compatibility with single group radiation
  integer, parameter :: Erad_ = WaveFirst_

  ! This allows to calculate RhoUx_ as RhoU_+x_ and so on.
  integer, parameter :: U_ = Ux_ - 1, RhoU_ = RhoUx_-1

  ! Bx_, By_, Bz_ have to be defined so that the code compiles
  ! but the Bx_ = Ux_ choice indicates that B is not used (see UseB in ModMain)
  integer, parameter :: Bx_ = Ux_, By_ = Uy_, Bz_ = Uz_, B_ = U_

  ! The default values for the state variables:
  ! Variables which are physically positive should be set to 1,
  ! variables that can be positive or negative should be set to 0:
  real, parameter :: DefaultState_V(nVar+nFluid) = [ &
       1.0, & ! Rho_
       0.0, & ! RhoUx_
       0.0, & ! RhoUy_
       0.0, & ! RhoUz_
       (0.0, iMaterial=MaterialFirst_,MaterialLast_), &
       1.0, & ! Pe_
       0.777770, & ! Te0_
       (1.0, iWave=WaveFirst_,WaveLast_), &
       0.0, & ! ExtraEint_
       1.0, & ! p_
       1.0 ] ! Energy_

  ! The names of the variables used in i/o
  character(len=4) :: NameVar_V(nVar+nFluid) = [ &
       'Rho ', & ! Rho_
       'Mx  ', & ! RhoUx_
       'My  ', & ! RhoUy_
       'Mz  ', & ! RhoUz_
       ('M?  ', iMaterial=MaterialFirst_,MaterialLast_), &
       'Pe  ', & ! Pe_
       'Te0 ', & ! Te0_
       ('I?? ', iWave=WaveFirst_,WaveLast_), &
       'EInt', & ! ExtraEint_
       'P   ', & ! p_
       'E   ']  ! Energy_

  ! Advected are the three level sets and the extra internal energy
  integer, parameter :: ScalarFirst_ = MaterialFirst_, ScalarLast_ = ExtraEint_

  integer, parameter :: iRho_I(nFluid)   = [Rho_]
  integer, parameter :: iRhoUx_I(nFluid) = [RhoUx_]
  integer, parameter :: iRhoUy_I(nFluid) = [RhoUy_]
  integer, parameter :: iRhoUz_I(nFluid) = [RhoUz_]
  integer, parameter :: iP_I(nFluid)     = [p_]

end module ModVarIndexes
!==============================================================================
