!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModVarIndexes

  use ModExtraVariables, &
       Redefine1 => Pe_, Redefine2 => iPparIon_I

  implicit none

  save

  character (len=*), parameter :: &
       NameEquationFile = "ModEquationMarsFluidsPe.f90"

  character (len=*), parameter :: &
       NameEquation = 'Multi-fluid with Pe for Mars'

  integer, parameter :: nVar = 23

  integer, parameter :: nFluid    = 4
  integer, parameter :: nIonFluid = 4        ! Last individual ion fluid
  logical, parameter :: IsMhd     = .false.  ! Multi-ion is not MHD
  real               :: MassFluid_I(nFluid) = [ 1.0, 32.0, 16.0, 44.0 ]

  character (len=6), parameter :: NameFluid_I(nFluid) = &
       [ 'Hp  ', 'O2p ', 'Op  ', 'CO2p' ]

  ! Named indexes for State_VGB and other variables
  ! These indexes should go subsequently, from 1 to nVar+nFluid.
  ! The energies are handled as an extra variable, so that we can use
  ! both conservative and non-conservative scheme and switch between them.
  integer, parameter :: &
       Rho_       =  1,          HpRho_   = 1, &
       RhoUx_     =  2, Ux_ = 2, HpRhoUx_ = 2, &
       RhoUy_     =  3, Uy_ = 3, HpRhoUy_ = 3, &
       RhoUz_     =  4, Uz_ = 4, HpRhoUz_ = 4, &
       Bx_        =  5, &
       By_        =  6, &
       Bz_        =  7, &
       Pe_        =  8, &
       p_         =  9, HpP_ = 9, &
       O2pRho_    = 10, &
       O2pRhoUx_  = 11, &
       O2pRhoUy_  = 12, &
       O2pRhoUz_  = 13, &
       O2pP_      = 14, &
       OpRho_     = 15, &
       OpRhoUx_   = 16, &
       OpRhoUy_   = 17, &
       OpRhoUz_   = 18, &
       OpP_       = 19, &
       CO2pRho_   = 20, &
       CO2pRhoUx_ = 21, &
       CO2pRhoUy_ = 22, &
       CO2pRhoUz_ = 23, &
       CO2pP_     = 24, &
       Energy_    = nVar+1, HpEnergy_ = nVar+1, &
       O2pEnergy_ = nVar+2, &
       OpEnergy_  = nVar+3, &
       CO2pEnergy_= nVar+4

  ! This allows to calculate RhoUx_ as RhoU_+x_ and so on.
  integer, parameter :: U_ = Ux_ - 1, RhoU_ = RhoUx_-1, B_ = Bx_-1

  ! These arrays are useful for multifluid
  integer, parameter :: &
       iRho_I(nFluid)  =[HpRho_,   O2pRho_,   OpRho_,   CO2pRho_ ] ,&
       iRhoUx_I(nFluid)=[HpRhoUx_, O2pRhoUx_, OpRhoUx_, CO2pRhoUx_],&
       iRhoUy_I(nFluid)=[HpRhoUy_, O2pRhoUy_, OpRhoUy_, CO2pRhoUy_],&
       iRhoUz_I(nFluid)=[HpRhoUz_, O2pRhoUz_, OpRhoUz_, CO2pRhoUz_],&
       iP_I(nFluid)    =[HpP_,     O2pP_,     OpP_ ,    CO2pP_ ]

  integer, parameter :: iPparIon_I(nIonFluid) = [1,2,3,4]

  ! The default values for the state variables:
  ! Variables which are physically positive should be set to 1,
  ! variables that can be positive or negative should be set to 0:
  real, parameter :: DefaultState_V(nVar+nFluid) = [ &
       1.0, & ! HpRho_
       0.0, & ! HpRhoUx_
       0.0, & ! HpRhoUy_
       0.0, & ! HpRhoUz_
       0.0, & ! Bx_
       0.0, & ! By_
       0.0, & ! Bz_
       1.0, & ! Pe_
       1.0, & ! HpP_
       1.0, & ! O2pRho_
       0.0, & ! O2pRhoUx_
       0.0, & ! O2pRhoUy_
       0.0, & ! O2pRhoUz_
       1.0, & ! O2pP_
       1.0, & ! OpRho_
       0.0, & ! OpRhoUx_
       0.0, & ! OpRhoUy_
       0.0, & ! OpRhoUz_
       1.0, & ! OpP_
       1.0, & ! CO2pRho_
       0.0, & ! CO2pRhoUx_
       0.0, & ! CO2pRhoUy_
       0.0, & ! CO2pRhoUz_
       1.0, & ! CO2pP_
       1.0, & ! HpEnergy_
       1.0, & ! O2pEnergy_
       1.0, & ! OpEnergy_
       1.0 ]  ! CO2pEnergy_

  ! The names of the variables used in i/o
  character(len=7) :: NameVar_V(nVar+nFluid) = [ &
       'Rho    ', & ! Rho_
       'Mx     ', & ! RhoUx_
       'My     ', & ! RhoUy_
       'Mz     ', & ! RhoUz_
       'Bx     ', & ! Bx_
       'By     ', & ! By_
       'Bz     ', & ! Bz_
       'Pe     ', & ! Pe_
       'P      ', & ! p_
       'O2pRho ', & ! O2pRho_
       'O2pMx  ', & ! O2pRhoUx_
       'O2pMy  ', & ! O2pRhoUy_
       'O2pMz  ', & ! O2pRhoUz_
       'O2pP   ', & ! O2pP_
       'OpRho  ', & ! OpRho_
       'OpMx   ', & ! OpRhoUx_
       'OpMy   ', & ! OpRhoUy_
       'OpMz   ', & ! OpRhoUz_
       'OpP    ', & ! OpP_
       'CO2pRho', & ! CO2pRho_
       'CO2pMx ', & ! CO2pRhoUx_
       'CO2pMy ', & ! CO2pRhoUy_
       'CO2pMz ', & ! CO2pRhoUz_
       'CO2pP  ', & ! CO2pP_
       'HpE    ', & ! HpEnergy_
       'O2pE   ', & ! O2pEnergy_
       'OpE    ', & ! OpEnergy_
       'CO2pE  ' ] ! CO2pEnergy_

  ! There are no extra scalars
  integer, parameter :: ScalarFirst_ = 2, ScalarLast_ = 1

end module ModVarIndexes
!==============================================================================
