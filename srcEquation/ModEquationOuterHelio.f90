!  Copyright (C) 2002 Regents of the University of Michigan
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModVarIndexes

   use ModExtraVariables

   implicit none

   save

  character (len=*), parameter :: &
       NameEquationFile = "ModEquationOuterHelio.f90"

   ! This equation module contains the standard MHD equations + 4 neutrals
   character (len=*), parameter :: &
       NameEquation = 'MHD and four neutrals'

   ! Number of variables without energy:
   integer, parameter :: nVar = 29

   ! 1 ion fluid and 4 neutral fluids
   integer, parameter :: nFluid    = 5
   integer, parameter :: nIonFluid = 1
   logical, parameter :: IsMhd     = .true.
   real               :: MassFluid_I(nFluid) = [ 1.0, 1.0, 1.0, 1.0, 1.0 ]

   character (len=3), parameter :: &
        NameFluid_I(nFluid) = ['Ion', 'Neu', 'Ne2', 'Ne3', 'Ne4']

   ! Named indexes for State_VGB and other variables
   ! These indexes should go subsequently, from 1 to nVar+nFluid.
   ! The energies are handled as an extra variable, so that we can use
   ! both conservative and non-conservative scheme and switch between  them.
   integer, parameter :: &
        Rho_       =  1,              SWHRho_   = 1, &
        Pu3Rho_    =  1, &
        RhoUx_     =  2, Ux_ = 2,     SWHRhoUx_ = 2, SWHUx_ = 2, &
        Pu3RhoUx_  =  2, Pu3Ux_ = 2, &
        RhoUy_     =  3, Uy_ = 3,     SWHRhoUy_ = 3, SWHUy_ = 3, &
        Pu3RhoUy_  =  3, Pu3Uy_ = 3, &
        RhoUz_     =  4, Uz_ = 4,     SWHRhoUz_ = 4, SWHUz_ = 4, &
        Pu3RhoUz_  =  4, Pu3Uz_ = 4, &
        Bx_        =  5, &
        By_        =  6, &
        Bz_        =  7, &
        LevelHP_   =  8, &
        p_         =  9,              SWHP_  = 9, &
        Pu3P_      =  9, &
        NeuRho_    = 10, &
        NeuRhoUx_  = 11, NeuUx_ = 11, &
        NeuRhoUy_  = 12, NeuUy_ = 12, &
        NeuRhoUz_  = 13, NeuUz_ = 13, &
        NeuP_      = 14, &
        Ne2Rho_    = 15, &
        Ne2RhoUx_  = 16, Ne2Ux_ = 16, &
        Ne2RhoUy_  = 17, Ne2Uy_ = 17, &
        Ne2RhoUz_  = 18, Ne2Uz_ = 18, &
        Ne2P_      = 19, &
        Ne3Rho_    = 20, &
        Ne3RhoUx_  = 21, Ne3Ux_ = 21, &
        Ne3RhoUy_  = 22, Ne3Uy_ = 22, &
        Ne3RhoUz_  = 23, Ne3Uz_ = 23, &
        Ne3P_      = 24, &
        Ne4Rho_    = 25, &
        Ne4RhoUx_  = 26, Ne4Ux_ = 26, &
        Ne4RhoUy_  = 27, Ne4Uy_ = 27, &
        Ne4RhoUz_  = 28, Ne4Uz_ = 28, &
        Ne4P_      = 29, &
        Energy_    = nVar+1,          SWHEnergy_ = nVar+1, &
        Pu3Energy_ = nVar+1, &
        NeuEnergy_ = nVar+2, &
        Ne2Energy_ = nVar+3, &
        Ne3Energy_ = nVar+4, &
        Ne4Energy_ = nVar+5

   ! This allows to calculate RhoUx_ as RhoU_+x_ and so on.
   integer, parameter :: U_ = Ux_ - 1, RhoU_ = RhoUx_-1, B_ = Bx_-1

   ! These arrays are useful for multifluid
   integer, parameter :: &
        iRho_I(nFluid)   &
        = [Rho_,   NeuRho_,    Ne2Rho_,   Ne3Rho_,   Ne4Rho_   ], &
        iRhoUx_I(nFluid) &
        = [RhoUx_, NeuRhoUx_,  Ne2RhoUx_, Ne3RhoUx_, Ne4RhoUx_ ], &
        iRhoUy_I(nFluid) &
        = [RhoUy_, NeuRhoUy_,  Ne2RhoUy_, Ne3RhoUy_, Ne4RhoUy_ ], &
        iRhoUz_I(nFluid) &
        = [RhoUz_, NeuRhoUz_,  Ne2RhoUz_, Ne3RhoUz_, Ne4RhoUz_ ], &
        iP_I(nFluid)     &
        = [p_, NeuP_, Ne2P_, Ne3P_, Ne4P_]

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
        0.0, & ! LevelHP_
        1.0, & ! p_
        1.0, & ! NeuRho_
        0.0, & ! NeuRhoUx_
        0.0, & ! NeuRhoUy_
        0.0, & ! NeuRhoUz_
        1.0, & ! NeuP_
        1.0, & ! Ne2Rho_
        0.0, & ! Ne2RhoUx_
        0.0, & ! Ne2RhoUy_
        0.0, & ! Ne2RhoUz_
        1.0, & ! Ne2P_
        1.0, & ! Ne3Rho_
        0.0, & ! Ne3RhoUx_
        0.0, & ! Ne3RhoUy_
        0.0, & ! Ne3RhoUz_
        1.0, & ! Ne3P_
        1.0, & ! Ne4Rho_
        0.0, & ! Ne4RhoUx_
        0.0, & ! Ne4RhoUy_
        0.0, & ! Ne4RhoUz_
        1.0, & ! Ne4P_
        1.0, & ! Energy_
        1.0, & ! NeuEnergy_
        1.0, & ! Neu2Energy_
        1.0, & ! Neu3Energy_
        1.0 ] ! Neu4Energy__

   ! The names of the variables used in i/o
   character(len=6) :: NameVar_V(nVar+nFluid) = [ &
        'Rho   ', & ! Rho_
        'Mx    ', & ! RhoUx_
        'My    ', & ! RhoUy_
        'Mz    ', & ! RhoUz_
        'Bx    ', & ! Bx_
        'By    ', & ! By_
        'Bz    ', & ! Bz_
        'HPLim ', & ! LevelHP_
        'p     ', & ! p_
        'NeuRho', & ! NeuRho_
        'NeuMx ', & ! NeuRhoUx_
        'NeuMy ', & ! NeuRhoUy_
        'NeuMz ', & ! NeuRhoUz_
        'NeuP  ', & ! NeuP_
        'Ne2Rho', & ! Ne2Rho_
        'Ne2Mx ', & ! Ne2RhoUx_
        'Ne2My ', & ! Ne2RhoUy_
        'Ne2Mz ', & ! Ne2RhoUz_
        'Ne2P  ', & ! Ne2P_
        'Ne3Rho', & ! Ne3Rho_
        'Ne3Mx ', & ! Ne3RhoUx_
        'Ne3My ', & ! Ne3RhoUy_
        'Ne3Mz ', & ! Ne3RhoUz_
        'Ne3P  ', & ! Ne3P_
        'Ne4Rho', & ! Ne3Rho_
        'Ne4Mx ', & ! Ne3RhoUx_
        'Ne4My ', & ! Ne3RhoUy_
        'Ne4Mz ', & ! Ne3RhoUz_
        'Ne4P  ', & ! Ne3P_
        'e     ', & ! Energy_
        'NeuE  ', & ! NeuEnergy_
        'Ne2E  ', & ! Ne2Energy_
        'Ne3E  ', & ! Ne3Energy_
        'Ne4E  ' ] ! Ne4Energy_

   ! There are no extra scalars
   integer, parameter :: ScalarFirst_ = LevelHP_, ScalarLast_ = LevelHP_

end module ModVarIndexes
!==============================================================================
