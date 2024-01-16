!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModVarIndexes

  use ModExtraVariables, &
       Redefine => iPparIon_I

  implicit none

  save

  character (len=*), parameter :: &
       NameEquationFile = "ModEquationMarsFluidsSw.f90"

  character (len=*), parameter :: &
       NameEquation = 'Multi-fluid with SW for Mars'

  integer, parameter :: nVar = 28

  integer, parameter :: nFluid    = 5
  integer, parameter :: nIonFluid = 5        ! Last individual ion fluid
  logical, parameter :: IsMhd     = .false.  ! Multi-ion is not MHD
  real               :: MassFluid_I(nFluid) = [ 1.0, 32.0, 16.0, 44.0, 1.0 ]

  character (len=4), parameter :: NameFluid_I(nFluid) = &
       [ 'Hpsw', 'O2p ', 'Op  ', 'CO2p', 'Hp  ' ]

  ! Named indexes for State_VGB and other variables
  ! These indexes should go subsequently, from 1 to nVar+nFluid.
  ! The energies are handled as an extra variable, so that we can use
  ! both conservative and non-conservative scheme and switch between them.
  integer, parameter :: &
       Rho_       =  1,          HpswRho_   = 1, &
       RhoUx_     =  2, Ux_ = 2, HpswRhoUx_ = 2, &
       RhoUy_     =  3, Uy_ = 3, HpswRhoUy_ = 3, &
       RhoUz_     =  4, Uz_ = 4, HpswRhoUz_ = 4, &
       Bx_        =  5, &
       By_        =  6, &
       Bz_        =  7, &
       p_         =  8, HpswP_ = 8, &
       O2pRho_    =  9, &
       O2pRhoUx_  = 10, &
       O2pRhoUy_  = 11, &
       O2pRhoUz_  = 12, &
       O2pP_      = 13, &
       OpRho_     = 14, &
       OpRhoUx_   = 15, &
       OpRhoUy_   = 16, &
       OpRhoUz_   = 17, &
       OpP_       = 18, &
       CO2pRho_   = 19, &
       CO2pRhoUx_ = 20, &
       CO2pRhoUy_ = 21, &
       CO2pRhoUz_ = 22, &
       CO2pP_     = 23, &
       HpRho_     = 24, &
       HpRhoUx_   = 25, &
       HpRhoUy_   = 26, &
       HpRhoUz_   = 27, &
       HpP_       = 28, &
       Energy_    = nVar+1, HpswEnergy_= nVar+1, &
       O2pEnergy_ = nVar+2, &
       OpEnergy_  = nVar+3, &
       CO2pEnergy_= nVar+4, &
       HpEnergy_  = nVar+5

  ! This allows to calculate RhoUx_ as RhoU_+x_ and so on.
  integer, parameter :: U_ = Ux_ - 1, RhoU_ = RhoUx_-1, B_ = Bx_-1

  ! These arrays are useful for multifluid
  integer, parameter :: &
       iRho_I(nFluid)  =[Rho_,   O2pRho_,   OpRho_,   CO2pRho_,   HpRho_  ], &
       iRhoUx_I(nFluid)=[RhoUx_, O2pRhoUx_, OpRhoUx_, CO2pRhoUx_, HpRhoUx_], &
       iRhoUy_I(nFluid)=[RhoUy_, O2pRhoUy_, OpRhoUy_, CO2pRhoUy_, HpRhoUy_], &
       iRhoUz_I(nFluid)=[RhoUz_, O2pRhoUz_, OpRhoUz_, CO2pRhoUz_, HpRhoUz_], &
       iP_I(nFluid)    =[p_,     O2pP_,     OpP_ ,    CO2pP_,     HpP_    ]

  integer, parameter :: iPparIon_I(nIonFluid) = [1,2,3,4,5]

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
       1.0, & ! HpRho_
       0.0, & ! HpRhoUx_
       0.0, & ! HpRhoUy_
       0.0, & ! HpRhoUz_
       1.0, & ! HpP_
       1.0, & ! HpswEnergy_
       1.0, & ! O2pEnergy_
       1.0, & ! OpEnergy_
       1.0, & ! CO2pEnergy_
       1.0 ]  ! HpEnergy_

  ! The names of the variables used in i/o
  character(len=7) :: NameVar_V(nVar+nFluid) = [ &
       'Rho    ', & ! Rho_
       'Mx     ', & ! RhoUx_
       'My     ', & ! RhoUy_
       'Mz     ', & ! RhoUz_
       'Bx     ', & ! Bx_
       'By     ', & ! By_
       'Bz     ', & ! Bz_
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
       'HpRho  ', & ! HpRho_
       'HpMx   ', & ! HpRhoUx_
       'HpMy   ', & ! HpRhoUy_
       'HpMz   ', & ! HpRhoUz_
       'HpP    ', & ! HpP_
       'E      ', & ! Energy_
       'O2pE   ', & ! O2pEnergy_
       'OpE    ', & ! OpEnergy_
       'CO2pE  ', & ! CO2pEnergy_
       'HpswE  ' ]  ! HpEnergy_

  ! There are no extra scalars
  integer, parameter :: ScalarFirst_ = 2, ScalarLast_ = 1

end module ModVarIndexes
!==============================================================================
