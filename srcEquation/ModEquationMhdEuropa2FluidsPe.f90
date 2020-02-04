!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
!#NOTPUBLIC  email:cdha@umich.edu  expires:12/31/2099
module ModVarIndexes

  use ModExtraVariables, &
       Redefine1 => Pe_, &
       Redefine2 => iPparIon_I

  implicit none

  save

  ! This equation module contains the standard MHD equations 
  ! with electron pressure
  character (len=*), parameter :: NameEquation= &
       'Two-Ion-fluid Pe MHD for Europa'

  integer, parameter :: nVar = 14

  integer, parameter :: nFluid    = 2
  integer, parameter :: IonFirst_ = 1        ! First individual ion fluid
  integer, parameter :: IonLast_  = 2        ! Last individual ion fluid
  logical, parameter :: IsMhd     = .false.  ! Does not do ideal MHD
  real               :: MassFluid_I(nFluid) = (/ 16.0, 32.0 /)

  character (len=3), parameter :: NameFluid_I(nFluid) = &
       (/ 'Op ', 'O2p' /)


  ! Named indexes for State_VGB and other variables
  ! These indexes should go subsequently, from 1 to nVar+nFluid.
  ! The energy is handled as an extra variable, so that we can use
  ! both conservative and non-conservative scheme and switch between them.
  integer, parameter :: &
       Rho_       =  1, OpRho_ = 1, &
       RhoUx_     =  2, Ux_ = 2, OpRhoUx_ = 2, OpUx_ = 2,&
       RhoUy_     =  3, Uy_ = 3, OpRhoUy_ = 3, OpUy_ = 3,&
       RhoUz_     =  4, Uz_ = 4, OpRhoUz_ = 4, OpUz_ = 4,&
       Bx_        =  5, &
       By_        =  6, &
       Bz_        =  7, &
       Pe_        =  8, &
       p_         =  9, OpP_ = 9, &
       O2pRho_    = 10, &
       O2pRhoUx_  = 11, O2pUx_ = 11,&
       O2pRhoUy_  = 12, O2pUy_ = 12,&
       O2pRhoUz_  = 13, O2pUz_ = 13,&
       O2pP_      = 14, &
       Energy_    = nVar+1, OpEnergy_  = nVar+1, &
       O2pEnergy_ = nVar+2
 
  ! This allows to calculate RhoUx_ as RhoU_+x_ and so on.
  integer, parameter :: U_ = Ux_ - 1, RhoU_ = RhoUx_-1, B_ = Bx_-1

  ! These arrays are useful for multifluid
  integer, parameter :: &
       iRho_I(nFluid)  =(/OpRho_,   O2pRho_ /) ,&
       iRhoUx_I(nFluid)=(/OpRhoUx_, O2pRhoUx_ /),&
       iRhoUy_I(nFluid)=(/OpRhoUy_, O2pRhoUy_ /),&
       iRhoUz_I(nFluid)=(/OpRhoUz_, O2pRhoUz_ /),&
       iP_I(nFluid)    =(/OpP_,     O2pP_ /),&
       iPparIon_I(IonFirst_:IonLast_) = (/1,2/)

  ! The default values for the state variables:
  ! Variables which are physically positive should be set to 1,
  ! variables that can be positive or negative should be set to 0:
  real, parameter :: DefaultState_V(nVar+nFluid) = (/ & 
       1.0, & ! OpRho_
       0.0, & ! OpRhoUx_
       0.0, & ! OpRhoUy_
       0.0, & ! OpRhoUz_
       0.0, & ! Bx_
       0.0, & ! By_
       0.0, & ! Bz_
       1.0, & ! Pe_
       1.0, & ! OpP_
       1.0, & ! O2pRho_
       0.0, & ! O2pRhoUx_
       0.0, & ! O2pRhoUy_
       0.0, & ! O2pRhoUz_
       1.0, & ! O2pP_
       1.0, & ! OpEnergy_
       1.0 /) ! O2pEnergy_
  
  ! The names of the variables used in i/o
  character(len=7) :: NameVar_V(nVar+nFluid) = (/ &
       'OpRho  ', & ! OpRho_
       'OpMx   ', & ! OpRhoUx_
       'OpMy   ', & ! OpRhoUy_
       'OpMz   ', & ! OpRhoUz_
       'Bx     ', & ! Bx_
       'By     ', & ! By_
       'Bz     ', & ! Bz_
       'Pe     ', & ! Pe_
       'OpP    ', & ! OpP_
       'O2pRho ', & ! O2pRho_
       'O2pMx  ', & ! O2pRhoUx_
       'O2pMy  ', & ! O2pRhoUy_
       'O2pMz  ', & ! O2pRhoUz_
       'O2pP   ', & ! O2pP_
       'OpE    ', & ! OpEnergy_
       'O2pE   ' /) ! O2pEnergy_

  ! The space separated list of nVar conservative variables for plotting
  character(len=*), parameter :: NameConservativeVar = &
       'OpRho OpMx OpMy OpMz Bx By Bz Pe OpE '// &
       'O2pRho O2pMx O2pMy O2pMz O2pE '

  ! The space separated list of nVar primitive variables for plotting
  character(len=*), parameter :: NamePrimitiveVar = &
       'OpRho OpUx OpUy OpUz Bx By Bz Pe OpP '// &
       'O2pRho O2pUx O2pUy O2pUz O2pP '

  ! The space separated list of nVar primitive variables for TECplot output
  character(len=*), parameter :: NamePrimitiveVarTec = &
       '"r^O^+", "U_x^O^+", "U_y^O^+", "U_z^O^+", "B_x", "B_y", "B_z", "p_e", "P^O^+"'// &
       '"r^O2^+", "U_x^O2^+", "U_y^O2^+", "U_z^O2^+", "P^O2^+"'

  ! Names of the user units for IDL and TECPlot output
  character(len=20) :: &
       NameUnitUserIdl_V(nVar+nFluid) = '', NameUnitUserTec_V(nVar+nFluid) = ''

  ! There are no extra scalars (Pe has its own flux)
  integer, parameter :: ScalarFirst_ = 2, ScalarLast_ = 1

end module ModVarIndexes

