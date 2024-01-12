!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModVarIndexes

  use ModExtraVariables, &
       Redefine1 => Pe_, &
       Redefine2 => iPparIon_I

  implicit none

  save

  character (len=*), parameter :: &
       NameEquationFile = "ModEquationThreeIonFluidPe.f90"

  ! This equation file declares four ion fluids: solar wind H+, ionospheric
  ! H+, and ionospheric O+,  along with ion
  ! electron pressure.  This allows for
  ! thorough investigations of each populations entry and heating mechanisms
  ! within the terrestrial magnetosphere.  Solar wind values default to
  ! first fluid; user must specify inner boundary densities using
  ! #MAGNETOSPHERE command.
  character (len=*), parameter :: &
       NameEquation = &
       'MHD with SW and Iono H+, Iono O+, and electron pressure'

  integer, parameter :: nVar = 19

  integer, parameter :: nFluid    = 3
  integer, parameter :: nIonFluid = 3
  logical, parameter :: IsMhd     = .false.
  real               :: MassFluid_I(1:3) = [ 1.0, 16.0, 1.0 ]

  character (len=5), parameter :: NameFluid_I(nFluid)= &
       [ 'HpSw ', 'Op   ', 'Hp   ']

  ! Named indexes for State_VGB and other variables
  ! These indexes should go subsequently, from 1 to nVar+nFluid.
  ! The energies are handled as an extra variable, so that we can use
  ! both conservative and non-conservative scheme and switch between them.
  integer, parameter ::   &
       Rho_     =  1,          &
       RhoUx_   =  2, Ux_ = 2, &
       RhoUy_   =  3, Uy_ = 3, &
       RhoUz_   =  4, Uz_ = 4, &
       Bx_      =  5, &
       By_      =  6, &
       Bz_      =  7, &
       Pe_      =  8, &
       p_       =  9, &
       OpRho_   = 10, &
       OpRhoUx_ = 11, &
       OpRhoUy_ = 12, &
       OpRhoUz_ = 13, &
       OpP_     = 14, &
       HpRho_   = 15, &
       HpRhoUx_ = 16, &
       HpRhoUy_ = 17, &
       HpRhoUz_ = 18, &
       HpP_     = 19, &
       Energy_  = nVar+1, &
       OpEnergy_= nVar+2, &
       HpEnergy_= nVar+3

  ! This allows to calculate RhoUx_ as RhoU_+x_ and so on.
  integer, parameter :: U_ = Ux_ - 1, RhoU_ = RhoUx_-1, B_ = Bx_-1

  ! These arrays are useful for multifluid
  integer, parameter :: &
       iRho_I(nFluid)   = [Rho_,   OpRho_,   HpRho_], &
       iRhoUx_I(nFluid) = [RhoUx_, OpRhoUx_, HpRhoUx_], &
       iRhoUy_I(nFluid) = [RhoUy_, OpRhoUy_, HpRhoUy_], &
       iRhoUz_I(nFluid) = [RhoUz_, OpRhoUz_, HpRhoUz_], &
       iP_I(nFluid)     = [p_,     OpP_,     HpP_]

  integer, parameter :: iPparIon_I(nIonFluid) = [1,2,3]

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
       1.0, & ! Pe_
       1.0, & ! p_
       1.0, & ! OpRho_
       0.0, & ! OpRhoUx_
       0.0, & ! OpRhoUy_
       0.0, & ! OpRhoUz_
       1.0, & ! OpP_
       1.0, & ! HpRho_
       0.0, & ! HpRhoUx_
       0.0, & ! HpRhoUy_
       0.0, & ! HpRhoUz_
       1.0, & ! HpP_
       1.0, & ! Energy_
       1.0, & ! OpEnergy_
       1.0  ]!  HpEnergy_

  ! The names of the variables used in i/o
  character(len=7) :: NameVar_V(nVar+nFluid) = [ &
       'HpSwRho  ', & ! Rho_
       'HpSwMx   ', & ! RhoUx_
       'HpSwMy   ', & ! RhoUy_
       'HpSwMz   ', & ! RhoUz_
       'Bx   ', & ! Bx_
       'By   ', & ! By_
       'Bz   ', & ! Bz_
       'Pe   ', & ! Pe_
       'HpSwP    ', & ! p_
       'OpRho', & ! OpRho_
       'OpMx ', & ! OpRhoUx_
       'OpMy ', & ! OpRhoUy_
       'OpMz ', & ! OpRhoUz_
       'OpP  ', & ! OpP_
       'HpRho', & ! HpRho_
       'HpMx ', & ! HpRhoUx_
       'HpMy ', & ! HpRhoUy_
       'HpMz ', & ! HpRhoUz_
       'HpP  ', & ! HpP_
       'HpSwE    ', & ! Energy_
       'OpE  ', & ! OpEnergy_
       'HpE  ' ] ! HpEnergy_

  ! There are no extra scalars
  integer, parameter :: ScalarFirst_ = 2, ScalarLast_ = 1

end module ModVarIndexes
!==============================================================================
