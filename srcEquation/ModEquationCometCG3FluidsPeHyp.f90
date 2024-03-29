!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
!#NOTPUBLIC  email:xzjia@umich.edu  expires:12/31/2099
module ModVarIndexes

  use ModExtraVariables,        &
       Redefine1 => Pe_,        &
       Redefine2 => iPparIon_I, &
       Redefine3 => Hyp_

  implicit none

  save

  character (len=*), parameter :: &
       NameEquationFile = "ModEquationMhdCometCG3FluidsPeHyp.f90"

  ! This equation module contains the standard MHD equations
  ! with electron pressure
  character (len=*), parameter :: &
       NameEquation = &
       '2-fluid + Pe MHD + 1-Neutral for Comet CG with hyp div B cleaning'

  integer, parameter :: nVar = 20

  integer, parameter :: nFluid    = 3
  integer, parameter :: nIonFluid = 2        ! Last individual ion fluid
  logical, parameter :: IsMhd     = .false.   ! First total ion fluid obeys MHD
  real               :: MassFluid_I(1:nFluid) = [1.0, 17.0, 17.0]

  ! Fluids: total fluid, solar wind protons, cometary water ions
  character (len=4), parameter :: NameFluid_I(nFluid) = &
       [ 'Sw  ', 'H2Op', 'Neu1' ]

  ! Named indexes for State_VGB and other variables
  ! These indexes should go subsequently, from 1 to nVar+nFluid.
  ! The energy is handled as an extra variable, so that we can use
  ! both conservative and non-conservative scheme and switch between them.
  integer, parameter :: &
       Rho_        =  1, &
       RhoUx_      =  2, Ux_ = 2, &
       RhoUy_      =  3, Uy_ = 3, &
       RhoUz_      =  4, Uz_ = 4, &
       Bx_         =  5, &
       By_         =  6, &
       Bz_         =  7, &
       Pe_         =  8, &
       P_          =  9, &
       H2OpRho_    = 10, &
       H2OpRhoUx_  = 11, H2OpUx_ = 11, &
       H2OpRhoUy_  = 12, H2OpUy_ = 12, &
       H2OpRhoUz_  = 13, H2OpUz_ = 13, &
       H2OpP_      = 14, &
       Neu1Rho_    = 15, &
       Neu1RhoUx_  = 16, Neu1Ux_ = 16, &
       Neu1RhoUy_  = 17, Neu1Uy_ = 17, &
       Neu1RhoUz_  = 18, Neu1Uz_ = 18, &
       Neu1P_      = 19, &
       Hyp_        = 20, &
       Energy_     = nVar+1, &
       H2OpEnergy_ = nVar+2, &
       Neu1Energy_ = nVar+3

  ! This allows to calculate RhoUx_ as RhoU_+x_ and so on.
  integer, parameter :: U_ = Ux_ - 1, RhoU_ = RhoUx_-1, B_ = Bx_-1

  ! These arrays are useful for multifluid
  integer, parameter :: &
       iRho_I(nFluid)  =[Rho_,   H2OpRho_,   Neu1Rho_ ] ,&
       iRhoUx_I(nFluid)=[RhoUx_, H2OpRhoUx_, Neu1RhoUx_ ],&
       iRhoUy_I(nFluid)=[RhoUy_, H2OpRhoUy_, Neu1RhoUy_ ],&
       iRhoUz_I(nFluid)=[RhoUz_, H2OpRhoUz_, Neu1RhoUz_ ],&
       iP_I(nFluid)    =[P_,     H2OpP_,     Neu1P_ ]

  integer, parameter :: iPparIon_I(nIonFluid) = [1,2]

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
       1.0, & ! P_
       1.0, & ! H2OpRho_
       0.0, & ! H2OpRhoUx_
       0.0, & ! H2OpRhoUy_
       0.0, & ! H2OpRhoUz_
       1.0, & ! H2OpP_
       1.0, & ! Neu1Rho_
       0.0, & ! Neu1RhoUx_
       0.0, & ! Neu1RhoUy_
       0.0, & ! Neu1RhoUz_
       1.0, & ! Neu1P_
       0.0, & ! Hyp_
       1.0, & ! Energy_
       1.0, & ! H2OpEnergy_
       1.0]  ! Neu1Energy_

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
       'H2OpRho', & ! H2OpRho_
       'H2OpMx ', & ! H2OpRhoUx_
       'H2OpMy ', & ! H2OpRhoUy_
       'H2OpMz ', & ! H2OpRhoUz_
       'H2OpP  ', & ! H2OpP_
       'Neu1Rho', & ! Neu1Rho_
       'Neu1Mx ', & ! Neu1RhoUx_
       'Neu1My ', & ! Neu1RhoUy_
       'Neu1Mz ', & ! Neu1RhoUz_
       'Neu1P  ', & ! Neu1P_
       'Hyp    ', & ! Hyp_
       'E      ', & ! Energy_
       'H2OpE  ', & ! H2OpEnergy_
       'Neu1E  ']  ! Neu1Energy_

  ! There are no extra scalars (Pe has its own flux)
  integer, parameter :: ScalarFirst_ = 2, ScalarLast_ = 1

end module ModVarIndexes
!==============================================================================

