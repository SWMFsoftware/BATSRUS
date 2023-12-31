!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
!#NOTPUBLIC  email:judithsz@umich.edu   expires:12/31/2099
module ModVarIndexes

  use ModSingleFluid
  use ModExtraVariables, &
       Redefine1  => nWave, &
       Redefine2  => WaveFirst_, &
       Redefine3  => WaveLast_, &
       Redefine4  => Ppar_, &
       Redefine5  => iPparIon_I, &
       Redefine6  => Pe_, &
       Redefine7  => Ehot_, &
       Redefine8  => nElement, &
       Redefine9  => NameElement_I, &
       Redefine10 => nChargeState_I, &
       Redefine11 => nChargeStateAll, &
       Redefine12 => ChargeStateFirst_, &
       Redefine13 => ChargeStateLast_

  implicit none

  save

  ! This equation module contains the anisotropic ion pressure MHD equations
  ! with wave energy, electron pressure and charge states
  character (len=*), parameter :: &
       NameEquation='MHD + Alfven waves + Pe + aniso Pi + charge states'
  character (len=*), parameter :: &
       NameEquationFile='ModEquationAwsomChargeState.f90'

  ! loop variable for implied do-loop over spectrum and ions
  integer, private :: iWave, iChargeState

  ! Number of ions of element with atomic number Z
  integer, parameter          :: nElement = 1
  character(len=2), parameter :: NameElement_I(1:nElement) = ['o ']
  integer, parameter          :: nChargeState_I(1:nElement) = [9]
  integer, parameter          :: nChargeStateAll = 9

  ! Number of wave bins in spectrum
  integer, parameter :: nWave = 2
  integer, parameter :: nVar = 11 + nWave + nChargeStateAll

  ! Named indexes for State_VGB and other variables
  ! These indexes should go subsequently, from 1 to nVar+1.
  ! The energy is handled as an extra variable, so that we can use
  ! both conservative and non-conservative scheme and switch between them.
  integer, parameter :: &
       Rho_       = 1,                     &
       RhoUx_     = 2,                     &
       RhoUy_     = 3,                     &
       RhoUz_     = 4,                     &
       Bx_        = 5,                     &
       By_        = 6,                     &
       Bz_        = 7,                     &
       Ehot_      = 8,                     &
       ChargeStateFirst_ = 9,                     &
       ChargeStateLast_  = ChargeStateFirst_+nChargeStateAll-1, &
       WaveFirst_ = ChargeStateLast_+1,           &
       WaveLast_  = WaveFirst_+nWave-1,    &
       Pe_        = nVar-2,                &
       Ppar_      = nVar-1,                &
       p_         = nVar,                  &
       Energy_    = nVar+1

  ! This allows to calculate RhoUx_ as rhoU_+x_ and so on.
  integer, parameter :: RhoU_ = RhoUx_-1, B_ = Bx_-1

  ! These arrays are useful for multifluid
  integer, parameter :: iRho_I(nFluid)   = [Rho_]
  integer, parameter :: iRhoUx_I(nFluid) = [RhoUx_]
  integer, parameter :: iRhoUy_I(nFluid) = [RhoUy_]
  integer, parameter :: iRhoUz_I(nFluid) = [RhoUz_]
  integer, parameter :: iP_I(nFluid)     = [p_]
  integer, parameter :: iPparIon_I(1) = Ppar_

  ! The default values for the state variables:
  ! Variables which are physically positive should be set to 1,
  ! variables that can be positive or negative should be set to 0:
  real, parameter :: DefaultState_V(nVar+1) = [ &
       1.0, & ! Rho_
       0.0, & ! RhoUx_
       0.0, & ! RhoUy_
       0.0, & ! RhoUz_
       0.0, & ! Bx_
       0.0, & ! By_
       0.0, & ! Bz_
       0.0, & ! Ehot_
       (1.0, iChargeState=ChargeStateFirst_,ChargeStateLast_), &
       (1.0, iWave=WaveFirst_,WaveLast_), &
       1.0, & ! Pe_
       1.0, & ! Ppar_
       1.0, & ! p_
       1.0 ] ! Energy_

  ! The names of the variables used in i/o
  character(len=4) :: NameVar_V(nVar+1) = [ &
       'Rho ', & ! Rho_
       'Mx  ', & ! RhoUx_
       'My  ', & ! RhoUy_
       'Mz  ', & ! RhoUz_
       'Bx  ', & ! Bx_
       'By  ', & ! By_
       'Bz  ', & ! Bz_
       'Ehot', & ! Ehot_
       ('El??', iChargeState=ChargeStateFirst_,ChargeStateLast_), &
       ('I?? ', iWave=WaveFirst_,WaveLast_), &
       'Pe  ', & ! Pe_
       'Ppar', & ! Ppar_
       'p   ', & ! p_
       'e   ' ] ! Energy_

  ! Primitive variable names
  integer, parameter :: U_ = RhoU_, Ux_ = RhoUx_, Uy_ = RhoUy_, Uz_ = RhoUz_

  ! There are no extra scalars
  integer, parameter :: &
       ScalarFirst_ = ChargeStateFirst_, ScalarLast_ = ChargeStateLast_

end module ModVarIndexes
!==============================================================================

