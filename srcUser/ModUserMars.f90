!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModUser

  ! This is the user module for Mars

  use ModSize
  use ModVarIndexes, ONLY: rho_, Ux_, Uz_,p_,Bx_, Bz_, &
       SpeciesFirst_, SpeciesLast_, MassSpecies_V
  use ModAdvance, ONLY: nSpecies
  use ModPhysics, ONLY: BodyRhoSpecies_I
  use ModMain, ONLY: IsNewUaState, UaState_VCB, rMaxUa
  use BATL_lib, ONLY: nI, nJ, nK, &
       test_start, test_stop, iTest, jTest, kTest, iBlockTest, iProc
  use ModUtilities, ONLY: open_file, close_file, upper_case
  use ModNumConst, ONLY: cPi, cHalfPi, cTwoPi, cDegToRad
  use ModIoUnit, ONLY: UnitTmp_
  use ModUserEmpty, &
       IMPLEMENTED1 => user_read_inputs,                &
       IMPLEMENTED2 => user_init_session,               &
       IMPLEMENTED3 => user_set_ics,                    &
       IMPLEMENTED4 => user_set_face_boundary,          &
       IMPLEMENTED5 => user_init_point_implicit,        &
       IMPLEMENTED6 => user_calc_sources_impl,          &
       IMPLEMENTED7 => user_get_b0,                     &
       IMPLEMENTED8 => user_get_log_var,                &
       IMPLEMENTED9 => user_set_boundary_cells,         &
       IMPLEMENTED10 => user_set_plot_var

  include 'user_module.h' ! list of public methods

  ! user routine Version number and descriptive string.
  character (len=*), parameter :: NameUserFile = "ModUserMars.f90"
  ! added rotation of crustal magnetic field with time,
  ! MHD model can now use MSO coordinate if needed
  character(len=*), parameter :: NameUserModule = &
       'Mars 4 species MHD code, Yingjuan Ma and Wenyi Sun'

  character(len=10) :: SolarCond='solarmax  '
  real:: F107 = 130.0   ! default value for solar med
  logical :: UsePhotoIonizationRate=.false., UseZeroU=.false.
  real:: IrateCO2, IrateO
  real:: EUVfactor = 1.0

  ! Radius within which the point implicit scheme should be used
  real :: rPointImplicit = 2.0

  ! Mars stuff
  integer, parameter :: MaxSpecies=4, MaxNuSpecies=8, MaxReactions=10

  ! number density of neutral species
  real, allocatable :: nDenNuSpecies_CBI(:,:,:,:,:)
  ! tempature of neutral species
  real, allocatable :: TempNeu_CB(:,:,:,:)
  ! production rate according to optical depth
  real, allocatable :: Productrate_CB(:,:,:,:)
  ! Ionization rate according to TGCM
  real, allocatable :: Ionizationrate_CBI(:,:,:,:,:)

  real:: ReactionRate_I(MaxReactions)
  real:: CoeffSpecies_II(MaxReactions,MaxSpecies), &
       dSdRho_II(MaxReactions,MaxSpecies) !, dLdRho_II
  real, dimension(MaxSpecies):: LossSpecies_I, &
       SiSpecies_I, LiSpecies_I, PhoIon_I, Recb_I, ImpIon_I
  !        dStndRho_I,  dLtdRho_I,  dLtndNumRho_I

  !$omp threadprivate( ReactionRate_I, CoeffSpecies_II, dSdRho_II )
  !$omp threadprivate( LossSpecies_I, SiSpecies_I, LiSpecies_I )
  !$omp threadprivate( PhoIon_I, Recb_I, ImpIon_I )

  real :: totalNumRho, totalLossRho, totalSourceRho, totalLossNumRho, &
       totalSourceNumRho, totalLossx, totalLossNumx
  real:: rInNeu=1.0294464

  !$omp threadprivate( totalNumRho, totalLossRho, totalSourceRho )
  !$omp threadprivate( totalLossNumRho )
  !$omp threadprivate( totalSourceNumRho, totalLossx, totalLossNumx )

  real, allocatable :: MaxSiSpecies_CB(:,:,:,:)
  real, allocatable :: MaxLiSpecies_CB(:,:,:,:)
  real, allocatable :: MaxSLSpecies_CB(:,:,:,:)

  ! the reactions considered:(p means ion, em means electron)
  ! the prefered order of a reaction is ions, Nus, hv and electrons
  integer, parameter :: &! reaction number
       CO2_hv__CO2p_em_=1 ,&! CO2+hv-->CO2p+em
       O_hv__Op_em_    =2 ,&! O+hv-->Op+em
       CO2p_O__O2p_CO_ =3 ,&! CO2p+O-->O2p+CO
       Op_CO2__O2p_CO_ =4 ,&! Op+CO2-->O2p+CO
       CO2p_O__Op_CO2_ =5 ,&! CO2p+O-->Op+CO2
       O2p_em__O_O_    =6 ,&! O2p+em-->O+O
       CO2p_em__CO_O_  =7 ,&! CO2p+em-->CO+O
       Hp_O__Op_H_     =8 ,&! Hp+O-->Op+H
       Op_H__Hp_O_     =9 ,&! Op+H-->Hp+O
       H_hv__Hp_em_    =10  ! H+hv-->Hp+em

  real:: Rate_I(MaxReactions)
  real:: RateDim_I(MaxReactions)= &
       [2.47e-7, 8.89e-8, 1.64e-10, 1.1e-9, &
       9.60e-11, 7.38e-8, 3.1e-7, 5.084e-10, 6.4e-10, 5.58e-8 ]  ! cm^3 s^(-1)

  integer, parameter :: &! order of ion species
       Hp_  =1, &
       O2p_ =2, &
       Op_  =3, &
       CO2p_=4

  real, parameter:: MassSpecies_I(MaxSpecies)=[1., 32., 16., 44.]

  !  MassSpecies_I(Hp_)=1	 ! atm
  !  MassSpecies_I(CO2p_)=44     ! atm
  !  MassSpecies_I(O2p_)=32      ! atm
  !  MassSpecies_I(Op_)=16       ! atm

  integer, parameter :: & ! order of Neutral species
       CO2_=1 ,&
       O_=2   ,&
       H_=3, &
       Oh_=4   ,&
       Ohx_=5 , &
       Hx_=6, &
       Ox_=7 ,&
       CO2x_=8

  real, dimension(MaxNuSpecies):: CrossSection_I
  real, dimension(MaxNuSpecies), parameter:: CrossSectionDim_I=&
       [2.6e-17,1.5e-17,0.0,1.5e-17,1.5e-17,0.0,1.5e-17,2.6e-17]

  real, dimension(MaxNuSpecies), parameter:: &
       NuMassSpecies_I=[44,16,1,16,16,1,16, 44]
  !  NuMassSpecies_I(CO2_)=44	! atm
  !  NuMassSpecies_I(O_)=16	! atm

  real, dimension(MaxNuSpecies):: HNuSpecies_I=1.0, HNuSpeciesDim_I=1.0
  ! HNuSpecies_dim_I(CO2_)=6.7e3   ! m
  ! HNuSpecies_dim_I(O_)=18.4e3    ! m

  real, dimension(MaxNuSpecies):: BodynDenNuSpecies_I,&
       BodynDenNuSpDim_I = [1.1593e12, 3.2278e9, 1.1307e7, 1.951e4, &
       1.5248e3, 9.4936e5, 5.2695e8, 2.2258e11]

  integer, parameter :: & ! other numbers
       em_=-1 ,&
       hv_=-2

  real :: TNu_body_dim=300.0, kTn  ! neutral temperature
  real :: kTi0  ! ion temperature at the body
  real :: kTp0  ! dimensionless temperature
                ! of new created ions / plasma (Tp_body=2.0*Ti_body)
  real :: Te_new_dim=1000., KTe0, T300
  real, parameter :: T300_dim = 300.0

  real, allocatable :: Nu_CB(:,:,:,:)
  real, parameter :: nu0_dim=4.0e-10
  real :: nu0

  ! coefficient of Mars magnetic field
  real, dimension(0:111,0:111) :: cmars, dmars
  integer :: NNm
  real :: SMDist = 1.52

  logical :: UseMarsB0 = .false., UseMso = .false., UseB0Old
  character(len=100):: NameFileB0 = '???'
  character(len=*), parameter:: NameFileB0Old = 'marsmgsp.txt'

  ! variables needed to be converted from MSO to GEO
  real :: RotAxisMso_D(3) ! rotation axis in MSO coordinate
  real :: u, v, w, uv, uvw, sint1, cost1, sint2, cost2

  real :: Rot = 1.0, Thetilt = 0.0
  logical :: UseHotO = .false.
  logical :: UseImpactIon = .false.
  logical :: UseChargeEx = .true.
  logical :: UseChapman = .false.

  ! Variables for Mars atmosphere lookup table generated with TGCM
  integer:: nLong, nLat, nAlt
  real, allocatable, dimension(:) :: Long_I, Lat_I, Alt_I
  real, allocatable, dimension(:,:,:):: Temp, Den_CO2, Den_O, ICO2p, IOp

  ! True if atmosphere model is coupled or read from file
  logical :: UseFileAtm = .false.
  ! Filename of atmosphere model output
  character(len=60):: NameFileAtm = "???"

  logical:: UseOldEnergy = .true.

contains
  !============================================================================
  subroutine user_read_inputs

    use ModMain
    use ModReadParam

    character (len=100) :: NameCommand

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_read_inputs'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    do
       if(.not.read_line() ) EXIT
       if(.not.read_command(NameCommand)) CYCLE
       call upper_case(NameCommand)
       select case(NameCommand)

       case('#USERINPUTEND')
          if(iProc==0) write(*,*)'USERINPUTEND'
          EXIT

       case("#USEOLDENERGY", "#OLDENERGY")
          call read_var('UseOldEnergy', UseOldEnergy)
          if(.not.UseOldEnergy)then
             call read_var('Te_new_dim',Te_new_dim)
             ! change temperature from ev to k
             Te_new_dim = Te_new_dim * 11610.0
          end if

       case("#MARSB0", "#USEMARSB0")
          ! if or not include crustal magnetic field of Mars
          call read_var('UseMarsB0',UseMarsB0)
          if(UseMarsB0) then
             call read_var('NNm', NNm)
             call read_var('Rot', Rot)
             call read_var('Thetilt', Thetilt)
             call read_var('NameFileB0', NameFileB0)
             rot= rot*cDegToRad
             thetilt= thetilt*cDegToRad
             cmars = 0.0
             dmars = 0.0
          endif

       case("#MSO", "#USEMSO")
          ! Rotate the crustal field in the MSO system
          call read_var('UseMso', UseMso)
          if(UseMso) then
             call read_var('RotAxisMsoX', RotAxisMso_D(1))
             call read_var('RotAxisMsoY', RotAxisMso_D(2))
             call read_var('RotAxisMsoZ', RotAxisMso_D(3))

             RotAxisMso_D = RotAxisMso_D/sqrt(sum(RotAxisMso_D**2))

             u = RotAxisMso_D(1)
             v = RotAxisMso_D(2)
             w = RotAxisMso_D(3)

             uv=sqrt(u**2+v**2)
             cost1=u/uv
             sint1=v/uv
             uvw=sqrt((u*w)**2+v**2)
             cost2=w*u/uvw
             sint2=v/uvw
          end if

       case ("#SMDIST")
          call read_var('SMDist', SMDist)

       case ("#PHOTOIONIZATION")
          call read_var('UsePhotoIonizationRate', UsePhotoIonizationRate)
          if(UsePhotoIonizationRate)then
             call read_var('IrateCO2', IrateCO2)
             call read_var('IrateO', IrateO)
          end if

       case ("#EUVFACTOR")
          call read_var('EUVfactor', EUVfactor)

       case("#SOLARCONDITION", "#SOLARCON")
          ! solar cycle condition
          call read_var('TypeSolarCon', SolarCond)

       case("#HOTOXYGEN", "#HOTO", "#USEHOTO")
          ! adding hot Oxygen or not
          call read_var('UseHotO', UseHotO)

       case("#NEUTRALRADIUS")
          call read_var('rInNeu', rInNeu)
          call read_var('rOutNeu', rMaxUa)

       case('#REACTIONS')
          call read_var('UseImpactIon', UseImpactIon)
          call read_var('UseChargeEx', UseChargeEx)

       case("#CHAPMAN", "#USECHAPMAN")
          call read_var('UseChapman', UseChapman)

       case("#MARSATMOSPHEREFILE", "#USEMARSATM")
          call read_var('UseFileAtm', UseFileAtm)
          if(UseFileAtm)then
             call read_var('NameFileAtm', NameFileAtm)
             ! The size should be in the file !!!
             ! Lookup table is the correct approach !!!
             call read_var('nLon', nLong)
             call read_var('nLat', nLat)
             call read_var('nAlt', nAlt)
          end if

       case('#POINTIMPLICITREGION')
          call read_var('rPointImplicit', rPointImplicit)

       case default
          if(iProc == 0) call stop_mpi( &
               'read_inputs: unrecognized command: '//NameCommand)
       end select
    end do

    call test_stop(NameSub, DoTest)
  end subroutine user_read_inputs
  !============================================================================
  subroutine user_init_point_implicit

    use ModMain, ONLY: nBlock, Unused_B
    use ModVarIndexes
    use ModPointImplicit, ONLY: iVarPointImpl_I, IsPointImplMatrixSet

    ! Allocate and set iVarPointImpl_I
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_init_point_implicit'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    allocate(iVarPointImpl_I(8))

    iVarPointImpl_I = [RhoHp_, RhoO2p_, RhoOp_, RhoCO2p_, &
         RhoUx_, RhoUy_, RhoUz_, P_]

    ! Tell the point implicit scheme if dS/dU will be set analytically
    IsPointImplMatrixSet = .false.

    call test_stop(NameSub, DoTest)
  end subroutine user_init_point_implicit
  !============================================================================
  subroutine user_calc_sources_impl(iBlock)

    use ModAdvance, ONLY: Source_VC
    use ModMain, ONLY: iNewDecomposition

    integer, intent(in) :: iBlock

    integer :: iLastDecomposition=-100
    !$omp threadprivate( iLastDecomposition )

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_calc_sources_impl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(IsNewUaState .or. iLastDecomposition /= iNewDecomposition)then

       call set_neutral_density(iBlock)

       if(iBlock == nBlock)then
          IsNewUaState = .false.
          iLastDecomposition = iNewDecomposition
       end if
    end if

    call user_sources(iBlock, Source_VC)

    call test_stop(NameSub, DoTest)
  end subroutine user_calc_sources_impl
  !============================================================================
  subroutine user_sources(iBlock, Source_VC)

    ! Set Source_VC source terms for block iBlock

    use ModAdvance, ONLY: State_VGB, Flux_VXI, Flux_VYI, Flux_VZI, Vdt_
    use ModVarIndexes, ONLY: Rho_, Ux_, Uy_, Uz_, &
         RhoUx_, RhoUy_, RhoUz_, P_, Energy_, Bx_, By_, Bz_
    use ModGeometry, ONLY: r_GB
    use ModPhysics, ONLY: Rbody, InvGammaMinus1, GammaMinus1,&
         No2Io_V, No2Si_V, UnitT_, UnitN_, UnitTemperature_
    use BATL_lib, ONLY: CellVolume_GB
    use ModPointImplicit, ONLY: UsePointImplicit

    integer, intent(in) :: iBlock
    real, intent(inout) :: Source_VC(:,:,:,:)

    ! Variables required by this user subroutine
    real, parameter :: A0=-1143.33, A1=323.767, A2=-35.0431, A3=1.69073, &
         A4=-0.0306575
    real, parameter :: B0=-1233.29, B1=347.764, B2=-37.4128, B3=1.79337, &
         B4=-0.0322777

    real :: Te_dim, Ti_dim, kTi, kTe
    real :: X1, X2, X3, X4
    real :: totalIMPNumRho
    integer:: i,j,k,iSpecies
    real :: inv_rho, inv_rho2, uu2, Productrate
    real :: temp
    real :: totalPSNumRho, totalRLNumRhox, temps
    real :: SourceLossMax, vdtmin

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_sources'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(r_GB(1,1,1,iBlock) > rMaxUa) RETURN

    do k = 1, nK; do j = 1, nJ; do i = 1, nI

       if (r_GB(i,j,k,iBlock) < Rbody) CYCLE

       inv_rho = 1/State_VGB(rho_,i,j,k,iBlock)
       inv_rho2 = inv_rho**2
       uu2 = sum(State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)**2)*inv_rho2

       ReactionRate_I  = 0.0
       CoeffSpecies_II = 0.0
       PhoIon_I        = 0.0
       Recb_I          = 0.0
       LossSpecies_I   = 0.0
       SiSpecies_I     = 0.0
       LiSpecies_I     = 0.0

       totalSourceRho    = 0.0
       totalLossRho      = 0.0
       totalLossNumRho   = 0.0
       totalSourceNumRho = 0.0
       totalLossx        = 0.0
       totalLossNumx     = 0.0
       totalPSNumRho     = 0.0
       totalRLNumRhox    = 0.0

       totalNumRho = sum(State_VGB(SpeciesFirst_:SpeciesLast_,i,j,k,iBlock) &
            /MassSpecies_I)
       MaxSLSpecies_CB(i,j,k,iBlock) = 1.0e-3

       Productrate = Productrate_CB(i,j,k,iBlock)

       kTi = State_VGB(P_,i,j,k,iBlock)/totalNumRho*0.5
       kTe = kTi
       Te_dim = kTe*No2Si_V(UnitTemperature_)
       Ti_dim = kTi*No2Si_V(UnitTemperature_)

       ! ReactionRate_I(CO2_hv__CO2p_em_)= &
       !      Rate_I(CO2_hv__CO2p_em_)&
       !      *nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)
       ! PhoIon_I(CO2p_)=ReactionRate_I(CO2_hv__CO2p_em_) &
       !      *Productrate
       ! ReactionRate_I(O_hv__Op_em_)= &
       !      Rate_I(O_hv__Op_em_)&
       !      *nDenNuSpecies_CBI(i,j,k,iBlock,O_)
       ! PhoIon_I(Op_)=ReactionRate_I(O_hv__Op_em_) &
       !      *Productrate

       ReactionRate_I(H_hv__Hp_em_) = &
            Rate_I(H_hv__Hp_em_)*nDenNuSpecies_CBI(i,j,k,iBlock,H_)
       PhoIon_I(Hp_) = ReactionRate_I(H_hv__Hp_em_)*Productrate

       PhoIon_I(CO2p_) = Ionizationrate_CBI(i,j,k,iBlock,CO2_)
       PhoIon_I(Op_) = Ionizationrate_CBI(i,j,k,iBlock,O_)

       ! IMPACT Ionization
       ImpIon_I = 0.0
       if(UseImpactIon)then
          X1 = log(Te_dim)
          X2 = X1*X1
          X3 = X2*X1
          X4 = X2*X2
          ImpIOn_I(Hp_) = exp(A0 + A1*X1 + A2*X2 + A3*X3 + A4*X4) &
               *No2Io_V(UnitT_)*No2Io_V(UnitN_)
          ImpIOn_I(Op_) = exp(B0+B1*X1+B2*X2+B3*X3+B4*X4) &
               *No2Io_V(UnitT_)*No2Io_V(UnitN_)
          ImpIon_I(Hp_) = ImpIon_I(Hp_)*nDenNuSpecies_CBI(i,j,k,iBlock,H_)
          ImpIon_I(Op_) = ImpIon_I(Op_)*nDenNuSpecies_CBI(i,j,k,iBlock,O_)
          ImpIon_I      = ImpIon_I * totalNumRho
       endif

!!!              Alt = (r_GB(i,j,k,iBlock)-1.0)*6052.0
!!!              if (Alt < 200.0 )then
!!!                 Te_dim = 300.0 + (Alt - 140.0)*3.7e3/60.0
!!!              else if( Alt < 800.0)then
!!!                 Te_dim = 4.0e3 + (Alt - 200.0)*5.0
!!!              else
!!!                 Te_dim =7.0e3
!!!              end if
!!!          Te_dim = 300.0
!!!          Ti_dim= Te_dim

       ! charge exchange
       ReactionRate_I(CO2p_O__O2p_CO_) = &
            Rate_I(CO2p_O__O2p_CO_)*nDenNuSpecies_CBI(i,j,k,iBlock,O_)
       CoeffSpecies_II(O2p_,CO2p_) = ReactionRate_I(CO2p_O__O2p_CO_)

       ReactionRate_I(Op_CO2__O2p_CO_) = &
            Rate_I(Op_CO2__O2p_CO_)*nDenNuSpecies_CBI(i,j,k,iBlock,CO2_) &
            *exp(log(800.0/Ti_dim)*0.39)
       CoeffSpecies_II(O2p_, Op_) = ReactionRate_I(Op_CO2__O2p_CO_)

       ReactionRate_I(CO2p_O__Op_CO2_) = &
            Rate_I(CO2p_O__Op_CO2_)*nDenNuSpecies_CBI(i,j,k,iBlock,O_)
       CoeffSpecies_II(Op_,CO2p_) = ReactionRate_I(CO2p_O__Op_CO2_)

       ReactionRate_I(Hp_O__Op_H_) = &
            Rate_I(Hp_O__Op_H_)*nDenNuSpecies_CBI(i,j,k,iBlock,O_)
       CoeffSpecies_II(Op_,Hp_) = ReactionRate_I(Hp_O__Op_H_)

       ReactionRate_I(Op_H__Hp_O_) = &
            Rate_I(Op_H__Hp_O_)*nDenNuSpecies_CBI(i,j,k,iBlock,H_)
       CoeffSpecies_II(Hp_,Op_)=ReactionRate_I(Op_H__Hp_O_)

       ! Recombination

       ! ReactionRate_I(O2p_em__O_O_)=Rate_I(O2p_em__O_O_)
       ! Recb_I(O2p_)=ReactionRate_I(O2p_em__O_O_)

       ! ReactionRate_I(CO2p_em__CO_O_)=Rate_I(CO2p_em__CO_O_)
       ! Recb_I(CO2p_)=ReactionRate_I(CO2p_em__CO_O_)
       ! Recombination

       ReactionRate_I(O2p_em__O_O_) = Rate_I(O2p_em__O_O_)
       ! Recb_I(O2p_)=ReactionRate_I(O2p_em__O_O_)
       !   *exp(log(TNu_body_dim/Te_dim)*0.56)
       Recb_I(O2p_)=ReactionRate_I(O2p_em__O_O_)*exp(log(1200.0/Te_dim)*0.56)

       ReactionRate_I(CO2p_em__CO_O_)=Rate_I(CO2p_em__CO_O_)
       ! Recb_I(CO2p_)=ReactionRate_I(CO2p_em__CO_O_)*sqrt(TNu_body_dim/Te_dim)
       Recb_I(CO2p_)=ReactionRate_I(CO2p_em__CO_O_)*sqrt(300.0/Te_dim)

       do iSpecies = 1, nSpecies
          LossSpecies_I = LossSpecies_I + CoeffSpecies_II(iSpecies, :)
          ! dStndRho_I=dStndRho_I  &
          !      + CoeffSpecies_II(iSpecies, :)/MassSpecies_I
          dSdRho_II(1:nSpecies, iSpecies) = &
               CoeffSpecies_II(1:nSpecies, iSpecies) &
               *MassSpecies_I(1:nSpecies)/MassSpecies_I(iSpecies)
       enddo

!!!    do iSpecies=1, nSpecies
!!!       dLdRho_II(1:nSpecies, iSpecies)=Recb_I(1:nSpecies)&
!!!            *rhoSpecies_GBI(i,j,k,iBlock,1:nSpecies) &
!!!            /MassSpecies_I(iSpecies)
!!!       dLdRho_II(iSpecies, iSpecies)=  &
!!!            dLdRho_II(iSpecies, iSpecies) &
!!!            +LossSpecies_I(iSpecies)  &
!!!            +Recb_I(iSpecies)*totalNumRho
!!!    enddo
!!!    !              dSLdRho_II=dSdRho_II-dLdRho_II
!!!
!!!    do iSpecies=1, nSpecies
!!!       dLtdRho_I=dLtdRho_I +dLdRho_II(iSpecies,:)
!!!       dLtndNumRho_I=dLtndNumRho_I &
!!!            +dLdRho_II(iSpecies,:)*MassSpecies_I/MassSpecies_I(iSpecies)
!!!    enddo

       SiSpecies_I = (PhoIon_I+ImpIon_I)*MassSpecies_I

       do iSpecies = 1, nSpecies
          SiSpecies_I(1:nSpecies) = &
               SiSpecies_I(1:nSpecies) &
               +dSdRho_II(1:nSpecies, iSpecies) &
               *State_VGB(rho_+iSpecies, i,j,k, iBlock)
          LiSpecies_I(iSpecies)= &
               LiSpecies_I(iSpecies) &
               +(LossSpecies_I(iSpecies) +Recb_I(iSpecies)*totalNumRho)&
               *State_VGB(rho_+iSpecies, i,j,k, iBlock)
       enddo

       totalIMPNumRho = sum(ImpIon_I)

       totalSourceRho = sum(SiSpecies_I(1:nSpecies))
       totalLossRho = sum(LiSpecies_I(1:nSpecies))
       ! sum of the (Loss term) of all ion species
       totalLossNumRho = sum(LiSpecies_I(1:nSpecies) &
            /MassSpecies_I(1:nSpecies))
       ! sum of the (loss term/atom mass)
       totalSourceNumRho = sum(SiSpecies_I(1:nSpecies)&
            /MassSpecies_I(1:nSpecies))
       ! sum of the (Source term/atom mass)
       totalLossx    = totalLossRho*inv_rho
       totalLossNumx = totalLossNumRho/totalNumRho
       totalPSNumRho = sum(PhoIon_I)

       ! sum of the photonionization source/atom mass
       totalRLNumRhox=sum(Recb_I &
            *State_VGB(rho_+1:rho_+nSpecies, i,j,k, iBlock)/MassSpecies_I)
       ! sum of the (loss term/atom mass) due to recombination

       MaxSLSpecies_CB(i,j,k,iBlock) = maxval(abs(SiSpecies_I(1:nSpecies) + &
            LiSpecies_I(1:nSpecies) ) / &
            (State_VGB(rho_+1:rho_+nSpecies,i,j,k,iBlock) + 1e-20)) &
            *CellVolume_GB(i,j,k,iBlock)

       ! Limit timestep for explicit scheme
       if(.not.UsePointImplicit)then
          ! sum of the (loss term/atom mass) due to recombination
          SourceLossMax = 10.0*maxval(abs(SiSpecies_I(1:nSpecies)-&
               LiSpecies_I(1:nSpecies) ) /&
               (State_VGB(rho_+1:rho_+nSpecies,i,j,k,iBlock) + 1e-20)) &
               *CellVolume_GB(i,j,k,iBlock)
          vdtmin = min(Flux_VXI(Vdt_,i,j,k,1), &
               Flux_VYI(Vdt_,i,j,k,1),Flux_VZI(Vdt_,i,j,k,1))
          if(SourceLossMax > Vdtmin) then
             Flux_VXI(Vdt_,i,j,k,1) = &
                  max(SourceLossMax, Flux_VXI(Vdt_,i,j,k,1))
             Flux_VYI(Vdt_,i,j,k,1) = &
                  max(SourceLossMax, Flux_VYI(Vdt_,i,j,k,1))
             Flux_VZI(Vdt_,i,j,k,1) = &
                  max(SourceLossMax, Flux_VZI(Vdt_,i,j,k,1))
          end if
       end if

       !  Flux_VXI(Vdt_,i,j,k,1) = max (MaxSLSpecies_CB(i,j,k,iBlock),&
       !       Flux_VXI(Vdt_,i,j,k,1) )
       !  Flux_VYI(Vdt_,i,j,k,1) = max (MaxSLSpecies_CB(i,j,k,iBlock),&
       !       Flux_VYI(Vdt_,i,j,k,1) )
       !  Flux_VZI(Vdt_,i,j,k,1) = max (MaxSLSpecies_CB(i,j,k,iBlock),&
       !       Flux_VZI(Vdt_,i,j,k,1) )

       Source_VC(rho_+1:rho_+nSpecies,i,j,k) = &
            Source_VC(rho_+1:rho_+nSpecies,i,j,k) &
            + SiSpecies_I(1:nSpecies) &
            - LiSpecies_I(1:nSpecies)

       Source_VC(rho_,i,j,k) = Source_VC(rho_,i,j,k) &
            + sum(SiSpecies_I(1:nSpecies)) &
            - sum(LiSpecies_I(1:nSpecies))

       Source_VC(rhoUx_,i,j,k) = Source_VC(rhoUx_,i,j,k) &
            - State_VGB(Ux_,i,j,k,iBlock)*totalLossx &
            - Nu_CB(i,j,k,iBlock)*State_VGB(Ux_,i,j,k,iBlock)

       Source_VC(rhoUy_,i,j,k) = Source_VC(rhoUy_,i,j,k) &
            - State_VGB(Uy_,i,j,k,iBlock)*totalLossx &
            - Nu_CB(i,j,k,iBlock)*State_VGB(Uy_,i,j,k,iBlock)

       Source_VC(rhoUz_,i,j,k) = Source_VC(rhoUz_,i,j,k) &
            -State_VGB(Uz_,i,j,k,iBlock)*totalLossx &
            -Nu_CB(i,j,k,iBlock)*State_VGB(Uz_,i,j,k,iBlock)

       !----- pressure and energy source terms
       if(UseOldEnergy)then
          temp = (totalSourceNumRho*kTp0 + totalPSNumRho*T300*20.) &
               - (totalLossNumx+totalRLNumRhox)*State_VGB(P_,i,j,k,iBlock)

          Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) &
               + (InvGammaMinus1*temp - 0.50*uu2*(totalLossRho) ) - &
               0.5*State_VGB(rho_,i,j,k,iBlock)*uu2*Nu_CB(i,j,k,iBlock)
          Source_VC(P_,i,j,k) = Source_VC(P_,i,j,k) &
               + (temp + 0.50*uu2*(totalSourceRho)*GammaMinus1) &
               + GammaMinus1*0.5*State_VGB(rho_,i,j,k,iBlock) &
               *uu2*Nu_CB(i,j,k,iBlock)

          if(kTi > kTn)then
             Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) &
                  - Nu_CB(i,j,k,iBlock)*totalNumRho*InvGammaMinus1*(kTi-kTn)
             Source_VC(P_,i,j,k) = Source_VC(P_,i,j,k) &
                  - Nu_CB(i,j,k,iBlock)*totalNumRho*(kTi-kTn)
          end if
       else
          temps = totalSourceNumRho*kTn                    &
               + totalNumRho*(kTn-KTi)*Nu_CB(i,j,k,iBlock) &
               + totalPSNumRho*kTe0                        &
               - totalLossNumRho*kTi                       &
               - totalRLNumRhox*totalNumRho*KTe

          Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) &
               - 0.5*State_VGB(rho_,i,j,k,iBlock)*uu2*Nu_CB(i,j,k,iBlock) &
               - 0.5*uu2*(totalLossRho) &
               + InvGammaMinus1*temps

          Source_VC(P_,i,j,k) = Source_VC(P_,i,j,k) &
               + GammaMinus1 &
               *State_VGB(rho_,i,j,k,iBlock)*uu2*Nu_CB(i,j,k,iBlock)  &
               + 0.5*GammaMinus1*uu2*(totalSourceRho) &
               + temps
       end if

    end do; end do; end do     ! end of the i,j,k loop

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_sources
  !============================================================================
  subroutine user_init_session

    use ModMain
    use ModPhysics
    use ModVarIndexes

    integer:: iBoundary, i, j, k, m, n
    character(len=100):: StringLine
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_init_session'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Allocate some arrays
    if(.not.allocated(TempNeu_CB))then
       allocate(TempNeu_CB(nI,nJ,nK,MaxBlock))
       allocate(Productrate_CB(nI,nJ,nK,MaxBlock))
       allocate(Ionizationrate_CBI(nI,nJ,nK,MaxBlock,2))
       allocate(MaxSiSpecies_CB(nI,nJ,nK,MaxBlock))
       allocate(MaxLiSpecies_CB(nI,nJ,nK,MaxBlock))
       allocate(MaxSLSpecies_CB(nI,nJ,nK,MaxBlock))
       allocate(Nu_CB(nI,nJ,nK,MaxBlock))
    end if

    if(UseMarsB0)then
       ! Read B0
       call open_file(FILE=NameFileB0, STATUS="OLD")
       UseB0Old = index(NameFileB0, NameFileB0Old) > 0
       if(UseB0Old)then
          do i = 0, NNm
             read(UnitTmp_,*)n,(cmars(n-1,m),m=0,n-1),(dmars(n-1,m),m=0,n-1)
          end do
       else
          read(UnitTmp_,*)StringLine
          read(UnitTmp_,*)StringLine
          read(UnitTmp_,*)StringLine
          do i = 1, NNm
             read(UnitTmp_,*)n,m, cmars(i,0)
             do j = 1, i
                read(UnitTmp_,*)n,m, cmars(i,j)
                read(UnitTmp_,*)n,m, dmars(i,j)
             end do
          end do
       end if
       call close_file
    end if

    if(UseFileAtm)then
       call open_file(FILE=NameFileAtm, STATUS="old")
       allocate(Long_I(nLong), Lat_I(nLat), Alt_I(nAlt), &
            Temp(nLong,nLat,nAlt), &
            Den_CO2(nLong,nLat,nAlt), Den_O(nLong,nLat,nAlt), &
            ICO2p(nLong,nLat,nAlt), IOp(nLong,nLat,nAlt))

       read(UnitTmp_,*) StringLine
       write(*,*) trim(StringLine), Nalt
       do k = 1, NAlt; do j=1, NLat; do i=1, NLong
          read(UnitTmp_,*) Long_I(i), Lat_I(j), Alt_I(k), &
               Temp(i,j,k), Den_CO2(i,j,k), Den_O(i,j,k), &
               ICO2p(i,j,k), IOp(i,j,k)
       end do; end do; end do
       call close_file
       write(*,*)Long_I(Nlong),Lat_I(NLat),Alt_I(Nalt)
       write(*,*)Long_I(1),Lat_I(1),Alt_I(1)
       write(*,*)'Den_O(i,j,k),ICO2p(i,j,k),IOp(i,j,k)=',&
            Den_O(Nlong,Nlat,Nalt), ICO2p(Nlong,Nlat,Nalt),&
            IOp(Nlong,Nlat,Nalt)
    end if

    ! For Outer Boundaries
    do iBoundary = xMinBc_, zMaxBc_
       FaceState_VI(rhoHp_,iBoundary)    = SolarWindRho
       FaceState_VI(rhoO2p_,iBoundary)   = 1e-10*SolarWindRho
       FaceState_VI(rhoOp_,iBoundary)    = 1e-10*SolarWindRho
       FaceState_VI(rhoCO2p_,iBoundary)  = 1e-10*SolarWindRho
    end do
    call set_multiSp_ICs
    !    Rbody = 1.0 + 140.0e3/rMars
    BodyRho_I(1) = sum(BodyRhoSpecies_I)
    BodyP_I(1)   = sum(BodyRhoSpecies_I/MassSpecies_I)*kTp0

    FaceState_VI(rho_,body1_)=BodyRho_I(1)
    FaceState_VI(rhoHp_:rhoCO2p_,body1_) = BodyRhoSpecies_I
    FaceState_VI(P_,body1_)=BodyP_I(1)

    CellState_VI(:,Coord1MinBc_:Coord3MaxBc_) = FaceState_VI(:,xMinBc_:zMaxBc_)
    ! Convert velocity to momentum (single fluid)
    CellState_VI(RhoUx_,:) = CellState_VI(Ux_,:)*CellState_VI(Rho_,:)
    CellState_VI(RhoUy_,:) = CellState_VI(Uy_,:)*CellState_VI(Rho_,:)
    CellState_VI(RhoUz_,:) = CellState_VI(Uz_,:)*CellState_VI(Rho_,:)

    UnitUser_V(rhoHp_:rhoCO2p_)   = No2Io_V(UnitRho_)/MassSpecies_V

    if(.not.allocated(nDenNuSpecies_CBI))then
       allocate(nDenNuSpecies_CBI(nI,nJ,nK,MaxBlock,MaxNuSpecies))
       nDenNuSpecies_CBI = -1.0
    end if

    call test_stop(NameSub, DoTest)
  end subroutine user_init_session
  !============================================================================
  subroutine user_set_ics(iBlock)

    use ModMain
    use ModAdvance
    use ModGeometry, ONLY: r_GB
    use BATL_lib, ONLY: Xyz_DGB, Used_GB
    use ModPhysics
    use ModNumConst
    use ModMultiFluid
    use ModPointImplicit, ONLY: UsePointImplicit, UseUserPointImplicit_B

    integer, intent(in):: iBlock

    real:: CosSZA
    integer:: i, j, k, iFluid

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_ics'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(DoTest)then
       write(*,*)'BodynDenNuSpecies_I=', BodynDenNuSpecies_I
       write(*,*)
       write(*,*)'HNuSpecies_I=', HNuSpecies_I
       write(*,*)
       write(*,*)'Rbody=', Rbody
       write(*,*)
    end if

    call set_neutral_density(iBlock)

    ! Moved here from user_init_point_implicit. This is a temporary fix.
    if(UsePointImplicit) UseUserPointImplicit_B(iBlock) = &
         r_GB(1,1,1,iBlock) <= rPointImplicit .and. &
         r_GB(nI,1,1,iBlock) > rBody

    if(DoTest)then
       write(*,*)'Ionizationrate_CBI(testcell,CO2_)=', &
            Ionizationrate_CBI(iTest,jTest,kTest,iBlockTest,CO2_)
       write(*,*)'Ionizationrate_CBI(testcell,O_)=', &
            Ionizationrate_CBI(iTest,jTest,kTest,iBlockTest,O_)
       write(*,*)
    end if

    do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
       if (r_GB(i,j,k,iBlock) < Rbody) then
          cosSZA = (0.5 + sign(0.5, Xyz_DGB(x_,i,j,k,iBlock)))*&
               Xyz_DGB(x_,i,j,k,iBlock)/max(r_GB(i,j,k,iBlock), 1e-3) &
               + 1e-3

          State_VGB(:,i,j,k,iBlock) = FaceState_VI(:,body1_)
          ! Convert velocity to momentum
          do iFluid = 1, nFluid
             call select_fluid(iFluid)
             State_VGB(iRhoUx,i,j,k,iBlock) = &
                  FaceState_VI(iUx,body1_)*FaceState_VI(iRho,body1_)
             State_VGB(iRhoUy,i,j,k,iBlock) = &
                  FaceState_VI(iUy,body1_)*FaceState_VI(iRho,body1_)
             State_VGB(iRhoUz,i,j,k,iBlock) = &
                  FaceState_VI(iUz,body1_)*FaceState_VI(iRho,body1_)
          end do
          !           State_VGB(rhoOp_,i,j,k,iBlock)= 0.0
          !           State_VGB(rhoO2p_,i,j,k,iBlock)= 0.0
          !           State_VGB(rhoCO2p_,i,j,k,iBlock)= 0.0

          State_VGB(rhoOp_,i,j,k,iBlock)= &
               FaceState_VI(rhoOp_,body1_)*cosSZA
          State_VGB(rhoO2p_,i,j,k,iBlock)= &
               FaceState_VI(rhoO2p_,body1_)*sqrt(cosSZA)
          State_VGB(rhoCO2p_,i,j,k,iBlock)= &
               FaceState_VI(rhoCO2p_,body1_)*cosSZA
          State_VGB(rho_,i,j,k,iBlock)  = &
               sum( State_VGB(rho_+1:rho_+nSpecies,i,j,k,iBlock))
          State_VGB(P_,i,j,k,iBlock) = &
               max(SolarWindP, &
               sum(State_VGB(rho_+1:rho_+nSpecies,i,j,k,iBlock) &
               /MassSpecies_I(1:nSpecies))*kTp0 )
       else
          State_VGB(:,i,j,k,iBlock) = CellState_VI(:,Coord1MinBc_)
          State_VGB(Ux_:Bz_,i,j,k,iBlock) = 0.0
       end if
    end do;end do; end do;

    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if(Used_GB(i,j,k,iBlock) .and. r_GB(i,j,k,iBlock) < 1.5*Rbody)then

          cosSZA = (0.5 + sign(0.5, Xyz_DGB(x_,i,j,k,iBlock)))* &
               Xyz_DGB(x_,i,j,k,iBlock)/max(r_GB(i,j,k,iBlock), 1e-3) &
               + 1e-3

          State_VGB(rhoCO2p_,i,j,k,iBlock)= &
               Ionizationrate_CBI(i,j,k,iBlock,CO2_) &
               /nDenNuSpecies_CBI(i,j,k,iBlock,O_)   &
               /(Rate_I(CO2p_O__O2p_CO_) + Rate_I(CO2p_O__Op_CO2_))

          if(DoTest.and.i==iTest.and.j==jTest.and.k==kTest)then
             write(*,*)'user set_ics, cosSZA=', cosSZA
             write(*,*)'user set_ics, Ionizationrate(CO2_)=', &
                  Ionizationrate_CBI(i,j,k,iBlock,CO2_)
             write(*,*)'user set_ics, nDenNuSpecies(O_)=', &
                  nDenNuSpecies_CBI(i,j,k,iBlock,O_)
             write(*,*)'user set_ics, Rate(CO2p_O__O2p_CO_,CO2p_O__Op_CO2_)=',&
                  Rate_I(CO2p_O__O2p_CO_), Rate_I(CO2p_O__Op_CO2_)
             write(*,*)'State_VGB(rhoCO2p)=', State_VGB(rhoCO2p_,i,j,k,iBlock)
          endif
          State_VGB(rhoOp_,i,j,k,iBlock)= &
               (Ionizationrate_CBI(i,j,k,iBlock,O_) &
               + Rate_I(CO2p_O__Op_CO2_)*State_VGB(rhoCO2p_,i,j,k,iBlock) &
               *nDenNuSpecies_CBI(i,j,k,iBlock,O_)) &
               /(nDenNuSpecies_CBI(i,j,k,iBlock,CO2_) + SolarWindRho*1e6) &
               /Rate_I(Op_CO2__O2p_CO_)

          State_VGB(rhoO2p_,i,j,k,iBlock)= sqrt( &
               max(1e-10*SolarwindRho, &
               (nDenNuSpecies_CBI(i,j,k,iBlock,O_)* &
               State_VGB(rhoCO2p_,i,j,k,iBlock)*Rate_I(CO2p_O__O2p_CO_) + &
               nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)* &
               State_VGB(rhoOp_,i,j,k,iBlock)*Rate_I(Op_CO2__O2p_CO_))) &
               /Rate_I(O2p_em__O_O_))

          State_VGB(rhoO2p_:rhoCO2p_,i,j,k,iBlock) = &
               State_VGB(rhoO2p_:rhoCO2p_,i,j,k,iBlock) &
               *MassSpecies_I(O2p_:CO2p_)

       end if !(Used_GB?)

    end do; end do; end do

    ! if(DoTest) then
    !   write(*,*) 'IonRate(CO2)=', &
    !        Ionizationrate_CBI(iTest,jTest,kTest,iBlock,CO2_)
    !   write(*,*) 'nDensNu(O)=', nDenNuSpecies_CBI(iTest,jTest,kTest,iBlock,O_)
    !   write(*,*) 'Rate_I(CO2p_O__O2p_CO_), Rate_I(CO2p_O__Op_CO2_)=', &
    !        Rate_I(CO2p_O__O2p_CO_), Rate_I(CO2p_O__Op_CO2_)
    !   write(*,*) 'modified Co2p=', &
    !        State_VGB(RhoCo2p_,iTest,jTest,kTest,iBlockTest)
    !   write(*,*) 'IonRate(O)=', &
    !        Ionizationrate_CBI(iTest,jTest,kTest,iBlock,O_)
    !   write(*,*) 'Rate_I(CO2p_O__Op_CO2_), Rate_I(Op_CO2__O2p_CO_)=', &
    !        Rate_I(CO2p_O__Op_CO2_), Rate_I(Op_CO2__O2p_CO_)
    !   write(*,*)'nDensNu(CO2)=', &
    !        nDenNuSpecies_CBI(iTest,jTest,kTest,iBlock,CO2_)
    !   write(*,*)'modified O2p=', &
    !        State_VGB(RhoCo2p_,iTest,jTest,kTest,iBlockTest)
    ! end if

    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if(.not.Used_GB(i,j,k,iBlock))CYCLE
       State_VGB(rho_,i,j,k,iBlock)   =&
            sum(State_VGB(rho_+1:rho_+nSpecies,i,j,k,iBlock))
       State_VGB(P_,i,j,k,iBlock)= &
            max(SolarWindP, sum(State_VGB(rho_+1:rho_+nSpecies,i,j,k,iBlock)&
            /MassSpecies_I(1:nSpecies))*kTp0)
    end do; end do; end do

    DtMax_CB(:,:,:,iBlock) = 0.0

    if(DoTest)then
       write(*,*)'initial set up'
       write(*,*)'Rate_I=',Rate_I
       write(*,*)
       write(*,*)'rhoSpecies_GBI(testcell,1:nSpecies)=',&
            State_VGB(rho_+1:rho_+nSpecies,iTest,jTest,kTest,iBlockTest)
       write(*,*)'p_BLK(iTest,jTest,kTest,iBlockTest)=',&
            State_VGB(P_,iTest,jTest,kTest,iBlockTest)
       write(*,*)
       write(*,*)'rhoSpecies_GBI(testcell+1,1:nSpecies)=',&
            State_VGB(rho_+1:rho_+nSpecies,iTest+1,jTest,kTest,iBlockTest)
       write(*,*)'p_BLK(iTest+1,jTest,kTest,iBlockTest)=',&
            State_VGB(P_,iTest+1,jTest,kTest,iBlockTest)
       write(*,*)
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_ics
  !============================================================================
  subroutine set_multisp_ics

    ! calculate the scale height of ion and neutal species and
    ! intial boundary value of ion species

    use ModMain
    use ModConst
    use ModIO
    use ModPhysics

    real :: Productrate0, OptDep, SMDist2

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_multisp_ics'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)then
       write(*,*)'in set_multisp_ICs, No2Io_V(UnitN_),t=',&
            No2Io_V(UnitN_),No2Io_V(UnitT_)
       write(*,*)'No2Si_V(UnitX_), temperature=',&
            No2Si_V(UnitX_), No2Si_V(UnitTemperature_)
       write(*,*)'kTp=',SolarWindP*Tnu_body_dim*2.0/SolarWindTempDim, &
            2.0*Tnu_body_dim/No2Si_V(UnitTemperature_)
       write(*,*)'BodynDenNuSpecies_dim_I=', BodynDenNuSpdim_I
    end if

    SMDist2 = SMDist**2/EUVfactor

    select case(SolarCond)
    case('solarmax')
       Tnu_body_dim = 134.0      ! neutral temperature
       BodynDenNuSpDim_I(CO2_)= 4.435e12
       BodynDenNuSpDim_I(O_)= 8.0283e9
       BodynDenNuSpDim_I(H_)= 1.8374e6
       BodynDenNuSpDim_I(Oh_)= 6.3119e4
       BodynDenNuSpDim_I(Ohx_)= 3.9646e3
       BodynDenNuSpDim_I(Hx_)= 7.3638e4
       BodynDenNuSpDim_I(Ox_)= 5.1736e8
       BodynDenNuSpDim_I(CO2x_)= 8.0807e10

       HNuSpeciesDim_I(O_)=13.34 ! scale height in KM
       HNuSpeciesDim_I(Ox_)=50.025
       HNuSpeciesDim_I(Oh_)=290.5
       HNuSpeciesDim_I(Ohx_)=2436.6

       HNuSpeciesDim_I(CO2_)=6.5631
       HNuSpeciesDim_I(CO2x_)=17.064

       HNuSpeciesDim_I(H_)=13.133
       HNuSpeciesDim_I(Hx_)=610.0

       RateDim_I(CO2_hv__CO2p_em_)=7.3e-7
       RateDim_I(O_hv__Op_em_) = 2.734e-7
       RateDim_I(H_hv__Hp_em_) = 8.59e-8

    case('permax')  ! for solar maximum at perihelion
       Tnu_body_dim = 137.3      ! neutral temperature
       BodynDenNuSpDim_I(CO2_)= 5.658e12
       BodynDenNuSpDim_I(O_)= 9.472e9
       BodynDenNuSpDim_I(H_)= 1.321e6
       BodynDenNuSpDim_I(Oh_)= 7.811e4
       BodynDenNuSpDim_I(Ohx_)= 4.715e3
       BodynDenNuSpDim_I(Hx_)= 4.630e4
       BodynDenNuSpDim_I(Ox_)= 5.156e8
       BodynDenNuSpDim_I(CO2x_)= 6.723e10

       HNuSpeciesDim_I(O_)=14.19 ! scale height in KM
       HNuSpeciesDim_I(Ox_)=54.74
       HNuSpeciesDim_I(Oh_)=290.5
       HNuSpeciesDim_I(Ohx_)=2436.6

       HNuSpeciesDim_I(CO2_)=6.83
       HNuSpeciesDim_I(CO2x_)=18.63

       HNuSpeciesDim_I(H_)=13.133
       HNuSpeciesDim_I(Hx_)=614.3

       RateDim_I(CO2_hv__CO2p_em_)=8.89e-7
       RateDim_I(O_hv__Op_em_) = 3.35e-7
       RateDim_I(H_hv__Hp_em_) = 9.29e-8

    case('aphmax')  ! for solar maximum at aphelion
       Tnu_body_dim = 130.5      ! neutral temperature
       BodynDenNuSpDim_I(CO2_)= 3.422e12
       BodynDenNuSpDim_I(O_)= 6.732e9
       BodynDenNuSpDim_I(H_)= 2.610e6
       BodynDenNuSpDim_I(Oh_)= 5.031e4
       BodynDenNuSpDim_I(Ohx_)= 3.296e3
       BodynDenNuSpDim_I(Hx_)= 1.207e4
       BodynDenNuSpDim_I(Ox_)= 5.192e8
       BodynDenNuSpDim_I(CO2x_)= 9.828e10

       HNuSpeciesDim_I(O_)=12.49 ! scale height in KM
       HNuSpeciesDim_I(Ox_)=45.45
       HNuSpeciesDim_I(Oh_)=290.5
       HNuSpeciesDim_I(Ohx_)=2436.6

       HNuSpeciesDim_I(CO2_)=6.29
       HNuSpeciesDim_I(CO2x_)=15.55

       HNuSpeciesDim_I(H_)=13.133
       HNuSpeciesDim_I(Hx_)=605.4

       RateDim_I(CO2_hv__CO2p_em_)=5.92e-7
       RateDim_I(O_hv__Op_em_) = 2.20e-7
       RateDim_I(H_hv__Hp_em_) = 7.90e-8

    case('solarmin')  ! for solar min condition

       Tnu_body_dim = 117.0      ! neutral temperature

       BodynDenNuSpDim_I(CO2_)= 1.1593e12
       BodynDenNuSpDim_I(O_)= 3.2278e9
       BodynDenNuSpDim_I(H_)= 1.1307e7
       BodynDenNuSpDim_I(Oh_)= 1.951e4
       BodynDenNuSpDim_I(Ohx_)= 1.5248e3
       BodynDenNuSpDim_I(Hx_)= 9.4936e5
       BodynDenNuSpDim_I(Ox_)= 5.2695e8
       BodynDenNuSpDim_I(CO2x_)= 2.2258e11

       HNuSpeciesDim_I(O_)=9.486  ! scale height in km
       HNuSpeciesDim_I(Ox_)=30.45
       HNuSpeciesDim_I(Oh_)=290.5
       HNuSpeciesDim_I(Ohx_)=2436.6

       HNuSpeciesDim_I(CO2_)=5.2667
       HNuSpeciesDim_I(CO2x_)=10.533

       HNuSpeciesDim_I(H_)=13.133
       HNuSpeciesDim_I(Hx_)=586.6

       RateDim_I(CO2_hv__CO2p_em_)=6.696e-7/SMDist2
       RateDim_I(O_hv__Op_em_) = 2.44e-7/SMDist2
       RateDim_I(H_hv__Hp_em_) = 1.289e-7/SMDist2
       ! RateDim_I(CO2_hv__Op_CO_em_) = 4.72e-8 ! In Yingjuan's code only

    case('permin')  ! for solar minimum at perihelion

       Tnu_body_dim = 119.9      ! neutral temperature

       BodynDenNuSpDim_I(CO2_)= 1.479e12
       BodynDenNuSpDim_I(O_)= 3.808e9
       BodynDenNuSpDim_I(H_)= 1.81e6
       BodynDenNuSpDim_I(Oh_)= 2.41e4
       BodynDenNuSpDim_I(Ohx_)= 1.81e3
       BodynDenNuSpDim_I(Hx_)= 5.97e5
       BodynDenNuSpDim_I(Ox_)= 5.25e8
       BodynDenNuSpDim_I(CO2x_)= 1.85e11

       HNuSpeciesDim_I(O_)=10.09  ! scale height in km
       HNuSpeciesDim_I(Ox_)=33.32
       HNuSpeciesDim_I(Oh_)=290.5
       HNuSpeciesDim_I(Ohx_)=2436.6

       HNuSpeciesDim_I(CO2_)=5.48
       HNuSpeciesDim_I(CO2x_)=11.50

       HNuSpeciesDim_I(H_)=13.133
       HNuSpeciesDim_I(Hx_)=590.8

       RateDim_I(CO2_hv__CO2p_em_)=3.01e-7
       RateDim_I(O_hv__Op_em_) = 1.09e-7
       RateDim_I(H_hv__Hp_em_) = 6.03e-8

    case('aphmin')  ! for solar minimum at aphelion

       Tnu_body_dim = 114.0      ! neutral temperature

       BodynDenNuSpDim_I(CO2_)= 8.945e11
       BodynDenNuSpDim_I(O_)= 2.707e9
       BodynDenNuSpDim_I(H_)= 1.61e7
       BodynDenNuSpDim_I(Oh_)= 1.56e4
       BodynDenNuSpDim_I(Ohx_)= 1.27e3
       BodynDenNuSpDim_I(Hx_)= 1.56e6
       BodynDenNuSpDim_I(Ox_)= 5.29e8
       BodynDenNuSpDim_I(CO2x_)= 2.71e11

       HNuSpeciesDim_I(O_)=8.88  ! scale height in km
       HNuSpeciesDim_I(Ox_)=27.66
       HNuSpeciesDim_I(Oh_)=290.5
       HNuSpeciesDim_I(Ohx_)=2436.6

       HNuSpeciesDim_I(CO2_)=5.05
       HNuSpeciesDim_I(CO2x_)=9.60

       HNuSpeciesDim_I(H_)=13.133
       HNuSpeciesDim_I(Hx_)=582.2

       RateDim_I(CO2_hv__CO2p_em_)=2.00e-7
       RateDim_I(O_hv__Op_em_) = 7.16e-8
       RateDim_I(H_hv__Hp_em_) = 5.13e-8

    case('permaxex')  ! for solar maximum at perihelion
       ! Ionization rates increased by 3 orders of magnitude

       Tnu_body_dim = 137.3      ! neutral temperature
       BodynDenNuSpDim_I(CO2_)= 5.658e12
       BodynDenNuSpDim_I(O_)= 9.472e9
       BodynDenNuSpDim_I(H_)= 1.321e6
       BodynDenNuSpDim_I(Oh_)= 7.811e4
       BodynDenNuSpDim_I(Ohx_)= 4.715e3
       BodynDenNuSpDim_I(Hx_)= 4.630e4
       BodynDenNuSpDim_I(Ox_)= 5.156e8
       BodynDenNuSpDim_I(CO2x_)= 6.723e10

       HNuSpeciesDim_I(O_)=14.19 ! scale height in KM
       HNuSpeciesDim_I(Ox_)=54.74
       HNuSpeciesDim_I(Oh_)=290.5
       HNuSpeciesDim_I(Ohx_)=2436.6

       HNuSpeciesDim_I(CO2_)=6.83
       HNuSpeciesDim_I(CO2x_)=18.63

       HNuSpeciesDim_I(H_)=13.133
       HNuSpeciesDim_I(Hx_)=614.3

       RateDim_I(CO2_hv__CO2p_em_)=8.89e-3
       RateDim_I(O_hv__Op_em_) = 3.35e-3
       RateDim_I(H_hv__Hp_em_) = 9.29e-4

    case('aphminex')  ! for solar minimum at aphelion
       ! Ionization rates decreased by nine orders of magnitude

       Tnu_body_dim = 114.0      ! neutral temperature

       BodynDenNuSpDim_I(CO2_)= 8.945e11
       BodynDenNuSpDim_I(O_)= 2.707e9
       BodynDenNuSpDim_I(H_)= 1.61e7
       BodynDenNuSpDim_I(Oh_)= 1.56e4
       BodynDenNuSpDim_I(Ohx_)= 1.27e3
       BodynDenNuSpDim_I(Hx_)= 1.56e6
       BodynDenNuSpDim_I(Ox_)= 5.29e8
       BodynDenNuSpDim_I(CO2x_)= 2.71e11

       HNuSpeciesDim_I(O_)=8.88  ! scale height in km
       HNuSpeciesDim_I(Ox_)=27.66
       HNuSpeciesDim_I(Oh_)=290.5
       HNuSpeciesDim_I(Ohx_)=2436.6

       HNuSpeciesDim_I(CO2_)=5.05
       HNuSpeciesDim_I(CO2x_)=9.60

       HNuSpeciesDim_I(H_)=13.133
       HNuSpeciesDim_I(Hx_)=582.2

       RateDim_I(CO2_hv__CO2p_em_)=2.00e-16
       RateDim_I(O_hv__Op_em_) = 7.16e-17
       RateDim_I(H_hv__Hp_em_) = 5.13e-17

    case ('earlymars1')
       Tnu_body_dim = 180.0      ! neutral temperature
       ! increase to 1000k to exobase

       RateDim_I(CO2_hv__CO2p_em_)=6.6e-7*6.0/2.25
       RateDim_I(O_hv__Op_em_) = 2.0e-7*6.0/2.25
       RateDim_I(H_hv__Hp_em_) = 8.59e-8*6.0/2.25

       BodynDenNuSpDim_I(CO2_)= 5.1e11
       BodynDenNuSpDim_I(O_)= 3.8e10
       BodynDenNuSpDim_I(H_)= 2.3e5
       BodynDenNuSpDim_I(Hx_)= 5.0e4
       BodynDenNuSpDim_I(Ox_)= 2.2e9
       BodynDenNuSpDim_I(CO2x_)= 6.8e9

       HNuSpeciesDim_I(O_)=24.5  ! scale height in km
       HNuSpeciesDim_I(Ox_)=100.
       HNuSpeciesDim_I(CO2_)=11.0
       HNuSpeciesDim_I(CO2x_)=32.0
       HNuSpeciesDim_I(H_)=24.0
       HNuSpeciesDim_I(Hx_)=650.

    case ('earlymars2')
       Tnu_body_dim = 180.0      ! neutral temperature
       ! increase to 1000k to exobase

       RateDim_I(CO2_hv__CO2p_em_)=6.6e-7*6.0/2.25
       RateDim_I(O_hv__Op_em_) = 2.0e-7*6.0/2.25
       RateDim_I(H_hv__Hp_em_) = 8.59e-8*6.0/2.25

       BodynDenNuSpDim_I(CO2_)= 1.5e14 !/cc
       BodynDenNuSpDim_I(O_)= 1.8e12
       BodynDenNuSpDim_I(H_)= 2.5e5
       BodynDenNuSpDim_I(Hx_)= 2.0e1
       BodynDenNuSpDim_I(Ox_)= 1.3e9
       BodynDenNuSpDim_I(CO2x_)= 1.0e9

       HNuSpeciesDim_I(O_)=25.5  ! scale height in km
       HNuSpeciesDim_I(Ox_)=190.
       HNuSpeciesDim_I(CO2_)=11.3
       HNuSpeciesDim_I(CO2x_)=51.0
       HNuSpeciesDim_I(H_)=15.0
       HNuSpeciesDim_I(Hx_)=850.

    case default
       call stop_mpi('unknow solar condition '//SolarCond)
    end select

    if(UsePhotoIonizationRate)then
       RateDim_I(CO2_hv__CO2p_em_) = IrateCO2
       RateDim_I(O_hv__Op_em_) = IrateO
    endif

    kTn = TNu_body_dim*Si2No_V(UnitTemperature_)
    kTi0 = kTn
    kTp0 = 2.0*kTn

    kTe0 = max(Te_new_dim, Tnu_body_dim)*Si2No_V(UnitTemperature_)

    T300 = T300_dim*Si2No_V(UnitTemperature_)

    if(DoTest)then
       write(*,*)'Tnu_body=',kTn, TNu_body_dim
       write(*,*)'T300=', T300, T300_dim
       write(*,*)'Tp_body=', kTp0
    end if

    nu0 = nu0_dim*No2Io_V(UnitN_)*No2Io_V(UnitT_)
    BodynDenNuSpecies_I = BodynDenNuSpDim_I*Io2No_V(UnitN_)
    HNuSpecies_I = HNuSpeciesDim_I*1.0e3*Si2No_V(UnitX_)

    ! normalize the reaction rate
    ! Ion-ion reaction rates have units 1/(cc * s)
    ! Rate_I(CO2p_O__O2p_CO_:Op_H__Hp_O_) = &
    !     Rate_I(CO2p_O__O2p_CO_:Op_H__Hp_O_)*No2Io_V(UnitT_)*No2Io_V(UnitN_)
    ! Photoionization rates have units 1/s
    ! Rate_I(CO2_hv__CO2p_em_) = Rate_I(CO2_hv__CO2p_em_)*No2Io_V(UnitT_)
    ! Rate_I(O_hv__Op_em_)     = Rate_I(O_hv__Op_em_)    *No2Io_V(UnitT_)
    ! Rate_I(H_hv__Hp_em_)     = Rate_I(H_hv__Hp_em_)    *No2Io_V(UnitT_)

    !!! TO BE SIMPLIFIED WITH ARRAY SYNTAX !!!
    Rate_I(CO2_hv__CO2p_em_) = RateDim_I(CO2_hv__CO2p_em_) &
         *No2Io_V(UnitT_)
    Rate_I(O_hv__Op_em_) = RateDim_I(O_hv__Op_em_) &
         *No2Io_V(UnitT_)
    Rate_I(CO2p_O__O2p_CO_) = RateDim_I(CO2p_O__O2p_CO_)  &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)
    Rate_I(Op_CO2__O2p_CO_) = RateDim_I(Op_CO2__O2p_CO_) &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)
    Rate_I(CO2p_O__Op_CO2_) = RateDim_I(CO2p_O__Op_CO2_) &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)

    Rate_I(O2p_em__O_O_) = RateDim_I(O2p_em__O_O_) &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)
    Rate_I(CO2p_em__CO_O_) = RateDim_I(CO2p_em__CO_O_) &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)

    Rate_I(H_hv__Hp_em_) = RateDim_I(H_hv__Hp_em_)*No2Io_V(UnitT_)
    Rate_I(Hp_O__Op_H_) = RateDim_I(Hp_O__Op_H_) &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)
    Rate_I(Op_H__Hp_O_) = RateDim_I(Op_H__Hp_O_) &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)

    ! Calculate reaction and photoionization rates
    ReactionRate_I(CO2_hv__CO2p_em_) = &
         Rate_I(CO2_hv__CO2p_em_)*BodynDenNuSpecies_I(CO2_)
    PhoIon_I(CO2p_) = ReactionRate_I(CO2_hv__CO2p_em_)

    ReactionRate_I(O_hv__Op_em_) = &
         Rate_I(O_hv__Op_em_)*BodynDenNuSpecies_I(O_)
    PhoIon_I(Op_) = ReactionRate_I(O_hv__Op_em_)

    ! charge exchange
    ReactionRate_I(CO2p_O__O2p_CO_) = &
         Rate_I(CO2p_O__O2p_CO_)*BodynDenNuSpecies_I(O_)
    CoeffSpecies_II(O2p_,CO2p_) = ReactionRate_I(CO2p_O__O2p_CO_)

    ReactionRate_I(Op_CO2__O2p_CO_) = &
         Rate_I(Op_CO2__O2p_CO_)*BodynDenNuSpecies_I(CO2_)
    CoeffSpecies_II(O2p_, Op_) = ReactionRate_I(Op_CO2__O2p_CO_)

    ReactionRate_I(CO2p_O__Op_CO2_) = &
         Rate_I(CO2p_O__Op_CO2_)*BodynDenNuSpecies_I(O_)
    CoeffSpecies_II(Op_,CO2p_) = ReactionRate_I(CO2p_O__Op_CO2_)

    CrossSection_I = CrossSectiondim_I*No2Io_V(unitN_)*No2Si_V(unitX_)*1e2

    Optdep = sum(BodynDenNuSpecies_I*CrossSection_I*HNuSpecies_I)
    Productrate0 = max(exp(-Optdep), 1e-5)

    if(DoTest)then
       write(*,*)'=======in set_multisp=============='
       write(*,*)'BodynDenNuSpecies_I=',BodynDenNuSpecies_I
       write(*,*)'HNuSpecies_I=',HNuSpecies_I
       write(*,*)'solar min, Procductrate=', productrate0, Optdep
       write(*,*)'CrossSection_dim_I*unitUSER_n*unitSI_x=', &
            CrossSectiondim_I, No2Io_V(unitN_), No2Si_V(unitX_)
       write(*,*)'Optdep=', Optdep
    end if

    ! ion density at the body
    BodyRhoSpecies_I(Hp_) = SolarWindRho*0.3

    BodyRhoSpecies_I(CO2p_) = Rate_I(CO2_hv__CO2p_em_)*Productrate0* &
         BodynDenNuSpecies_I(CO2_)/BodynDenNuSpecies_I(O_)/ &
         (Rate_I(CO2p_O__O2p_CO_) + Rate_I(CO2p_O__Op_CO2_))
    BodyRhoSpecies_I(Op_) = (Rate_I(O_hv__Op_em_)*Productrate0 + &
         Rate_I(CO2p_O__Op_CO2_)*BodyRhoSpecies_I(CO2p_)) &
         *BodynDenNuSpecies_I(O_)/(BodynDenNuSpecies_I(CO2_)+3.0e5)/ &
         Rate_I(Op_CO2__O2p_CO_)
    BodyRhoSpecies_I(O2p_) = sqrt((BodynDenNuSpecies_I(O_)* &
         BodyRhoSpecies_I(CO2p_)*Rate_I(CO2p_O__O2p_CO_) + &
         BodynDenNuSpecies_I(CO2_)*BodyRhoSpecies_I(Op_)* &
         Rate_I(Op_CO2__O2p_CO_))/Rate_I(O2p_em__O_O_))

    BodyRhoSpecies_I = BodyRhoSpecies_I*MassSpecies_I

    if(DoTest)then
       write(*,*)' set parameters of Mars: BodyRhoSpecies_I(i)=',&
            BodyRhoSpecies_I
       write(*,*)'neutral density=', BodynDenNuSpecies_I
       write(*,*)'nu0=',nu0
       write(*,*)'Rate_I=', Rate_I
       write(*,*)'RateDim_I=', RateDim_I
       !       call stop_mpi('end')
    end if
    call test_stop(NameSub, DoTest)

  end subroutine set_multisp_ics
  !============================================================================
  subroutine user_set_face_boundary(FBC)

    use ModMain, ONLY: UseRotatingBc, ExtraBc_,Body1_,xMinBc_, FaceBCType
    use ModVarIndexes, ONLY: nVar, RhoOp_, RhoO2p_, RhoCO2p_, RhoHp_
    use ModPhysics, ONLY: SolarWindRho, FaceState_VI, OmegaBody_D
    use ModCoordTransform, ONLY: cross_product

    type(FaceBCType), intent(inout):: FBC

    real:: XFace,YFace,ZFace,rFace,rFace2
    real:: uRot_D(3)
    real:: cosSZA
    real:: uDotR
    character(len=*), parameter:: NameSub = 'user_set_face_boundary'
    !--------------------------------------------------------------------------
    associate( VarsGhostFace_V => FBC%VarsGhostFace_V, &
               VarsTrueFace_V => FBC%VarsTrueFace_V, &
               FaceCoords_D => FBC%FaceCoords_D, &
               iBoundary => FBC%iBoundary )

    if(iBoundary == ExtraBc_)then
       VarsGhostFace_V = FaceState_VI(:,xMinBc_)
       RETURN
    elseif(iBoundary /= Body1_)then
       call stop_mpi(NameSub//' invalid iBoundary value')
    end if

    XFace = FaceCoords_D(1)
    YFace = FaceCoords_D(2)
    ZFace = FaceCoords_D(3)

    rFace2 = XFace**2 + YFace**2 + ZFace**2
    rFace  = sqrt(rFace2)

    ! Apply boundary conditions
    cosSZA=(0.5+sign(0.5,XFace)) * XFace/max(RFace,1.0e-3) + 1.0e-3

    VarsGhostFace_V(rhoOp_)  = BodyRhoSpecies_I(Op_) *cosSZA

    VarsGhostFace_V(rhoO2p_) = BodyRhoSpecies_I(O2p_)*sqrt(cosSZA)

    VarsGhostFace_V(rhoCO2p_)= BodyRhoSpecies_I(CO2p_)*cosSZA

    VarsGhostFace_V(rhoHp_)  = SolarWindRho*0.3

    VarsGhostFace_V(rho_) = sum(VarsGhostFace_V(rho_+1:rho_+nSpecies))
    VarsGhostFace_V(P_)=sum(VarsGhostFace_V(rho_+1:rho_+nSpecies)&
         /MassSpecies_I)*kTp0

    ! Reflective in radial direction
    uDotR = sum(VarsTrueFace_V(Ux_:Uz_)*FaceCoords_D)/rFace2
    ! bDotR = sum(VarsTrueFace_V(Bx_:Bz_)*FaceCoords_D)/rFace2

    VarsGhostFace_V(Ux_:Uz_) = VarsTrueFace_V(Ux_:Uz_) - 2*uDotR*FaceCoords_D
    ! VarsGhostFace_V(Bx_:Bz_) = VarsTrueFace_V(Bx_:Bz_) - 2*bDotR*FaceCoords_D
    VarsGhostFace_V(Bx_:Bz_) = 0.0

    ! Apply corotation?
    if (UseRotatingBc) then
       uRot_D = cross_product(OmegaBody_D, FaceCoords_D)
       VarsGhostFace_V(Ux_:Uz_) = VarsGhostFace_V(Ux_:Uz_) + 2*uRot_D
    end if

    end associate
  end subroutine user_set_face_boundary
  !============================================================================
  subroutine user_set_boundary_cells(iBlock)

    use ModGeometry, ONLY: ExtraBc_, Xyz_DGB, xMaxBox
    use ModBoundaryGeometry, ONLY: iBoundary_GB

    integer, intent(in):: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_boundary_cells'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    where (Xyz_DGB(x_,:,:,:,iBlock) > xMaxBox) &
         iBoundary_GB(:,:,:,iBlock) = ExtraBc_

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_boundary_cells
  !============================================================================
  subroutine user_get_b0(x1, y1, z1, B1)

    use ModMain
    use ModPhysics
    use ModNumConst
    ! use BATL_test, ONLY: xTest, yTest, zTest

    real, intent(in) :: x1, y1, z1
    real, intent(out):: B1(3)

    real :: R0, theta, phi, rr, X0, Y0, Z0, delta
    real, dimension(3) :: bb, B0, B2
    real :: sint, sinp, cost, cosp
    real :: x2, y2, z2
    !--------------------------------------------------------------------------
    if(.not. UseMarsB0) then
       B1 = 0.0
       RETURN
    end if

    call timing_start('user_get_b0')

    if(UseMso)then  ! change location from MSO to GEO
       ! rotate around Z axis to make the axis in the XZ plane
       x0 =  x1*cost1 + y1*sint1
       y0 = -x1*sint1 + y1*cost1
       ! rotate around Y axis
       x2 = X0*w - z1*uv
       y2 = Y0
       z2 = X0*uv + z1*w
       ! rotate back around Z axis so that the subsolar point is along the x
       ! axis
       x0 = x2*cost2 - y2*sint2
       z0 = z2
       y0 = x2*sint2 + y2*cost2
    else
       X0 = x1*cos(thetilt)-z1*sin(thetilt)
       Y0 = y1
       Z0 = x1*sin(thetilt)+z1*cos(thetilt)
    end if

    R0 = sqrt(X0**2 + Y0**2 + Z0**2)
    rr = max(R0, 1e-6)
    if(abs(X0) < 1e-6) then
       if(Y0 < 0) then
          phi = -cHalfPi
       else
          phi = cHalfPi
       endif
    else
       if(X0 > 0) then
          phi = atan(Y0/X0)
       else
          phi = cPi + atan(Y0/X0)
       endif
    endif

    ! RotPeriodSi=0.0 if not use rotation
    ! delta=rot-tSimulation*VRad  !(Vrad=cTwoPi/RotPeriodSi)
    delta = rot

    If(RotPeriodSi > 0.0) then
       delta=rot - tSimulation/RotPeriodSi*cTwoPi
    end If

    theta = acos(Z0/rr)

    if(UseB0Old)then
       call set_mars_b0_old(R0, theta, phi+delta, bb)
    else
       call set_mars_b0(R0, Z0/rr, phi+delta, bb)
    endif
    sint = sin(theta)
    cost = cos(theta)
    sinp = sin(phi)
    cosp = cos(phi)

    B0(1) = bb(1)*sint*cosp+bb(2)*cost*cosp-bb(3)*sinp
    B0(2) = bb(1)*sint*sinp+bb(2)*cost*sinp+bb(3)*cosp
    B0(3) = bb(1)*cost-bb(2)*sint

    if(UseMso)then  ! change from GEO to MSO
       B1(1) = B0(1)*cost2+B0(2)*sint2
       B1(2) = -B0(1)*sint2+B0(2)*cost2
       B1(3) = B0(3)

       B2(1) = w*B1(1) + uv*B1(3)
       B2(2) = B1(2)
       B2(3) = -uv*B1(1) + w*B1(3)

       B1(1) = B2(1)*cost1 - B2(2)*sint1
       B1(2) = B2(1)*sint1 + B2(2)*cost1
       B1(3) = B2(3)

    else
       B1(1) = B0(1)*cos(thetilt)+B0(3)*sin(thetilt)
       B1(2) = B0(2)
       B1(3) = -B0(1)*sin(thetilt)+B0(3)*cos(thetilt)
    end if

    ! Normalize the crustal magnetic field
    B1(1)=B1(1)*Io2No_V(UnitB_)
    B1(2)=B1(2)*Io2No_V(UnitB_)
    B1(3)=B1(3)*Io2No_V(UnitB_)

    ! if(abs(x1 - xTest) + abs(y1 - yTest) + abs(z1 - zTest) < 0.001)then
    !
    !   write(*,*)'user_get_b0 called with x1,y1,z1=',x1, y1, z1
    !   write(*,*)'user_get_b0: sint1, cost1, sint2, cost2=', &
    !        sint1, cost1, sint2, cost2
    !   write(*,*)'user_get_b0: u,v,w,uv,uvw=', u,v,w,uv,uvw
    !   write(*,*)'user_get_b0: x0,y0,z0=', x0,y0,z0
    !   write(*,*)'user_get_b0: r0, z0/rr, phi, delta=', R0, Z0/rr, phi+delta
    !   write(*,*)'user_get_b0: bb=', bb
    !   write(*,*)'user_get_b0: b0=', b0
    !   write(*,*)'user_get_b0: b1=', b1
    ! end if

    call timing_stop('user_get_b0')
  end subroutine user_get_b0
  !============================================================================
  subroutine set_mars_b0(r, xtcos, phi, bb)

    integer, parameter:: nMax=111
    real, intent(in) :: r, xtcos, phi
    real, dimension(1:3),intent(out) :: bb
    ! real :: Rlgndr, dRlgndr
    integer :: NN, n, m, im, l
    real :: dRnm, signsx, Rmm
    real :: xtsin,xtabs, xx
    real, dimension(0:nMax-1) :: xpcos, xpsin
    real :: a, arr, arrn, arrm, somx2, fact, temp
    real,dimension(0:nMax,0:nMax) :: Rnm, Pnm
    real,dimension(0:nMax+2), save  :: aorn_I
    logical :: DoSetFactor = .true., UseDebug=.false.
    !--------------------------------------------------------------------------
    NN = NNm - 1
!    xtcos=cos(theta)
!    xtsin=sin(theta)
    xtsin=sqrt(1.0-xtcos*xtcos)

    do im=0,NN
       xpcos(im)=cos(im*phi)
       xpsin(im)=sin(im*phi)
    end do

    arr=1.0/r   ! a=1.0
    aorn_I(0)=1.0
    do n=1,NN+2
       aorn_I(n)=arr*aorn_I(n-1)
    end do

    Rnm = 0.0
    Rnm(0,0) = 1.0
    Rnm(1,0)= xtcos
    do n=1, NN
       if(n == 1)then
          Rnm(n,n) = xtsin * Rnm(n-1,n-1)
       else
          Rnm(n,n) = sqrt((n-0.5)/n)*xtsin * Rnm(n-1,n-1)
       end if
       Rnm(n+1,n)= xtcos*sqrt(2.0*n+1.0)*Rnm(n,n)
    end do

    do m=0, NN
       do l =m+2, NN
          Rnm(l,m)=(xtcos*(2.0*l-1.0)*Rnm(l-1, m)-&
               Rnm(l-2,m)*sqrt((l+m-1.0)*(l-m-1.0)))/sqrt(1.0*l*l-1.0*m*m)
!          if(UseDebug)write(*,*)'l=,m= ', n, m, Rnm(l,m)
       end do
    end do

    if(UseDebug)then
       write(*,*)'x=', xtcos
       write(*,*)'Rnm()= ', Rnm(0,0),Rnm(1,1)
    endif

    bb=0.0
    drnm=0.0

    do m=0, NN
       do n=m,NN

          if(m == 0)then
             dRnm =  -sqrt((n+1.0)*n/2.0)*Rnm(n,m+1)
          else
             if(xtsin <=1.0e-6)then
                dRnm =  -sqrt((n+m+1.0)*(n-m))*Rnm(n,m+1)
             else
                dRnm = m*xtcos*Rnm(n,m)/xtsin - &
                     sqrt((n+m+1.0)*(n-m))*Rnm(n,m+1)
             endif
          endif
          bb(1)=bb(1)+(n+1)*aorn_I(n+2)*Rnm(n,m)*(cmars(n,m)*xpcos(m)&
               +dmars(n,m)*xpsin(m))
          bb(2)=bb(2)-aorn_I(n+2)*dRnm*(cmars(n,m)*&
               xpcos(m)+dmars(n,m)*xpsin(m))
          if(xtsin <= 1e-6) then
             bb(3)=0.
          else
             bb(3)=bb(3)-aorn_I(n+2)*Rnm(n,m)*m/xtsin*(-cmars(n,m)*xpsin(m)&
                  +dmars(n,m)*xpcos(m))
          endif

       end do ! n
    end do ! m

  end subroutine set_mars_b0
  !============================================================================
  subroutine set_mars_b0_old(r, theta, phi, bb)

    integer, parameter:: nMax=62
    real, intent(in) :: r, theta, phi
    real, dimension(1:3),intent(out) :: bb
    ! real :: Rlgndr, dRlgndr
    integer :: NN, n, m, im
    real :: dRnm, signsx, Rmm
    real :: xtcos,xtsin
    real, dimension(0:nMax-1) :: xpcos, xpsin
    real :: a, arr, arrn, arrm, somx2, fact, temp
    real,dimension(0:nMax,0:nMax) :: Rnm
    real,dimension(0:nMax), save  :: Factor1_I, Factor2_I, Factor3_I
    real,dimension(0:nMax,0:nMax), save :: Factor1_II, Factor2_II, Factor3_II
    logical :: DoSetFactor = .true.

    !$omp threadprivate( Factor1_I, Factor2_I, Factor3_I )
    !$omp threadprivate( Factor1_II, Factor2_II, Factor3_II )

    character(len=*), parameter:: NameSub = 'set_mars_b0_old'
    !--------------------------------------------------------------------------
    if(DoSetFactor)then
       DoSetFactor = .false.
       do m = 0, nMax
          Factor1_I(m) = sqrt((2.*m+2.)*(2.*m+1.))
          Factor2_I(m) = sqrt(4.*m+6.)
          Factor3_I(m) = sqrt(2.*m+5.)
          do n = m, nMax
             if(n>m+2)then
                temp= sqrt((n-1.-m)*(n+m+1.)/(2.*n+1.)/(n-m-2.))
                Factor1_II(n,m) = sqrt((2.*n-1.)/(n-m-2.))/temp
                Factor2_II(n,m) = sqrt((n+m)/(2.*n-3.))/temp
             end if
             Factor3_II(n,m) = sqrt((n-m)*(n+m+1.))
          end do
       end do
    end if

    a = 1.035336
    arr = a/r

    ! NNm=8

    if(r < 1.0) then
       NN = 0
    else
       NN = NNm - 1
    endif

    xtcos = cos(theta)
    xtsin = sin(theta)

    do im = 0, NN
       xpcos(im) = cos(im*phi)
       xpsin(im) = sin(im*phi)
    end do

    bb = 0.0

    !	    somx2=sqrt((1.-xtcos)*(1.+xtcos))
    somx2 = abs(xtsin)
    signsx = sign(1., xtsin)

    fact=1.
    Rmm=1.
    Rnm(0,0) = sqrt(2.)
    Rnm(1,0)=xtcos*sqrt(3.)*Rnm(0,0)
    do n=2, NN
       Rnm(n,0) = (xtcos*sqrt((2.*n-1.)*(2.*n+1.))*Rnm(n-1,0) &
            - (n-1)*sqrt((2.*n+1.)/(2.*n-3.))*Rnm(n-2, 0))/n
    enddo ! n

    arrm = 1.0

    call timing_start('crustal')

    do m = 0, NN

       Rmm=Rmm*fact*somx2/Factor1_I(m)

       Rnm(m+1,m+1)=Rmm*Factor2_I(m)
       Rnm(m, m+1)=0

       fact=fact+2.

       arrm=arr*arrm
       arrn=arrm

       do n = m, NN
          arrn = arr*arrn
          ! write(*,*) 'arrn=', arrn, ' n=', n
          if(n > (m+2)) then
             Rnm(n,m+1) = xtcos*Factor1_II(n,m)*Rnm(n-1,m+1)-&
                  Factor2_II(n,m)*Rnm(n-2,m+1)

          else if(n > (m+1)) then
             Rnm(n,m+1)=xtcos*Factor3_I(m)*Rnm(m+1,m+1)
          endif

          dRnm = m*xtcos*Rnm(n,m)/xtsin - Rnm(n,m+1)*signsx*Factor3_II(n,m)

          bb(1) = bb(1) + (n+1)*arrn*Rnm(n,m)*(cmars(n,m)*xpcos(m) &
               + dmars(n,m)*xpsin(m))
          bb(2) = bb(2) - arrn*dRnm*(cmars(n,m)*xpcos(m)+dmars(n,m)*xpsin(m))
          if(xtsin <= 1e-6) then
             bb(3) = 0.
          else
             bb(3) = bb(3) - arrn*Rnm(n,m)*m/xtsin*(-cmars(n,m)*xpsin(m)&
                  + dmars(n,m)*xpcos(m))
          endif
       end do ! n
    end do ! m

    call timing_stop('crustal')

  end subroutine set_mars_b0_old
  !============================================================================
  subroutine user_get_log_var(VarValue, NameVar, Radius)

    use ModGeometry, ONLY: Xyz_DGB,r_GB
    use ModMain, ONLY: Unused_B
    use ModVarIndexes, ONLY: &
         Rho_, rhoHp_, rhoO2p_, RhoOp_, RhoCO2p_, rhoUx_, rhoUz_
    use ModAdvance, ONLY: State_VGB,Tmp1_GB
    use ModPhysics, ONLY: No2Si_V, UnitN_, UnitX_, UnitU_
    use ModWriteLogSatFile, ONLY: calc_sphere

    real, intent(out)           :: VarValue
    character(len=*), intent(in):: NameVar
    real, intent(in), optional  :: Radius

    integer:: i, j, k, iBlock, iVar

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_get_log_var'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub,': NameVar=', NameVar
    select case(NameVar)
    case('hpflx')
       iVar = rhoHp_
    case('opflx')
       iVar = RhoOp_
    case('o2pflx')
       iVar = RhoO2p_
    case('co2pflx')
       iVar =  RhoCO2p_
    case default
       call stop_mpi(NameSub//': wrong NameVar='//NameVar)
    end select

    do iBlock = 1, nBlock
       if (Unused_B(iBlock)) CYCLE
       do k=0,nK+1; do j=0,nJ+1; do i=0,nI+1
          Tmp1_GB(i,j,k,iBlock) = State_VGB(iVar,i,j,k,iBlock)*&
               sum(State_VGB(rhoUx_:rhoUz_,i,j,k,iBlock) &
               *   Xyz_DGB(:,i,j,k,iBlock)) &
               /r_GB(i,j,k,iBlock)/State_VGB(rho_,i,j,k,iBlock)
       end do; end do; end do
    end do
    VarValue = calc_sphere('integrate', 360, Radius, Tmp1_GB) &
         /MassSpecies_V(iVar) &
         *No2Si_V(UnitN_)*No2Si_V(UnitX_)**2*No2Si_V(UnitU_)

    call test_stop(NameSub, DoTest)
  end subroutine user_get_log_var
  !============================================================================
  subroutine mars_input(iBlock)

    ! Interpolate and extrapolate various quantities from the neutral data file

    use ModMain
    use ModPhysics
    use ModConst
    use ModGeometry, ONLY: r_GB,CellSize_DB, Coord111_DB, TypeGeometry

    integer, intent(in) :: iBlock

    real, parameter :: TINY=1.0E-12

    real :: hh, theta, phi, dR, dtheta, dphi, dH, Hscale, HCO2, HO, grav
    real :: tempICO2p, tempIOp
    real :: xLat, xLong,xAlt
    real :: Dlong, Dlat, Dalt
    integer :: i,j,k
    integer:: iAlt, jLong, kLat, ip1,jp1,kp1
    !------ Interpolation/Expolation for Tn,nCO2,nO,PCO2p,POp -----

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'mars_input'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(TypeGeometry(1:9) /= 'spherical') &
         call stop_mpi('Unknown geometry type = '//TypeGeometry)

    ! the whole block is inside the body
    if (r_GB(nI,1,1,iBlock) < Rbody) RETURN

    ! the bottom of the block is outside rMaxUa
    if (r_GB(1,1,1,iBlock) >= rMaxUa) RETURN

    dR     = CellSize_DB(x_,iBlock)
    dPhi   = CellSize_DB(y_,iBlock)
    dTheta = CellSize_DB(z_,iBlock)

    Dlong = Long_I(2) - Long_I(1)
    Dlat  = Lat_I(2) - Lat_I(1)
    Dalt  = Alt_I(2) - Alt_I(1)

    do k = 1, nK
       Theta = (k-1)*dTheta  + Coord111_DB(Theta_,iBlock)
       Theta = Theta*cRadToDeg
       kLat = int((theta + 90.0 - Dlat/2)/Dlat+1.0)
       kp1 = min(kLat+1, NLat)
       kLat = max(kLat,1)

       do j = 1, nJ
          Phi = (j-1)*dPhi  + Coord111_DB(Phi_,iBlock)
          if(Long_I(1) < 0.0)then
             ! Shift to -pi to +pi range
             if(Phi > cPi) Phi = Phi - cTwoPi
             Phi = Phi*cRadToDeg ! Convert to degrees
             jLong = int((Phi+180)/Dlong + 1.0)
          else
             Phi = Phi*cRadToDeg ! Convert to degrees
             jLong = int(Phi/Dlong + 1.0)
          end if
          jp1   = min(jLong+1, nLong)
          jLong = max(jLong, 1)

          do i = 1, nI
             if (r_GB(i,j,k,iBlock) >= rMaxUa) CYCLE

             hh = (r_GB(i,j,k,iBlock) - 1)*3396.00
             !                 write(*,*)'hh=', hh, i,j,k,iBlock
             xLong = (Phi - Long_I(jLong))/Dlong
             xLat = (Theta - Lat_I(kLat))/Dlat
             if(hh <= Alt_I(NAlt))then
                iAlt = int((hh - (Alt_I(1)+Dalt/2))/Dalt+1.0)
                ip1 = min(iAlt+1,NAlt)
                if(iAlt < 1)then
                   write(*,*)'wrong ialt',iAlt
                end if
                xalt = (hh-Alt_I(iAlt))/Dalt

                ! interpolate
                TempNeu_CB(i,j,k,iBlock) =          &
                     ((Temp(jLong,kLat,iAlt)*(1-xLong)       &
                     + xLong*Temp(jp1,kLat,ialt))*(1-xLat) &
                     +(Temp(jLong,kp1,iAlt)*(1-xLong)        &
                     + xLong*Temp(jp1,kp1,ialt))*xLat)*(1-xAlt)&
                     +((Temp(jLong,kLat,ip1)*(1-xLong)       &
                     + xLong*Temp(jp1,kLat,ip1))*(1-xLat)   &
                     +(Temp(jLong,kp1,ip1)*(1-xLong)         &
                     + xLong*Temp(jp1,kp1,ip1))*xLat)*xAlt

                nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)=&
                     ((Den_CO2(jLong,kLat,iAlt)*(1-xLong) &
                     + xLong*Den_CO2(jp1,kLat,ialt))*(1-xLat) + &
                     (Den_CO2(jLong,kp1,iAlt)*(1-xLong) &
                     +xLong*Den_CO2(jp1,kp1,iAlt))*xLat)*(1-xAlt) + &
                     ((Den_CO2(jLong,kLat,ip1)*(1-xLong) &
                     + xLong*Den_CO2(jp1, kLat, ip1))*(1-xLat)+&
                     (Den_CO2(jLong,kp1,ip1)*(1-xLong) &
                     + xLong*Den_CO2(jp1, kp1, ip1))*xLat)*xAlt

                nDenNuSpecies_CBI(i,j,k,iBlock,O_)=&
                     ((Den_O(jLong,kLat,iAlt)*(1-xLong) &
                     +xLong*Den_O(jp1, kLat, ialt))*(1-xLat)+&
                     (Den_O(jLong,kp1,iAlt)*(1-xLong) &
                     +xLong*Den_O(jp1, kp1, ialt))*xLat)*(1-xAlt)+&
                     ((Den_O(jLong,kLat,ip1)*(1-xLong) &
                     +xLong*Den_O(jp1, kLat, ip1))*(1-xLat)+&
                     (Den_O(jLong,kp1,ip1)*(1-xLong) &
                     +xLong*Den_O(jp1, kp1, ip1))*xLat)*xAlt

                tempICO2p=&
                     ((ICO2p(jLong,kLat,iAlt)*(1-xLong) &
                     +xLong*ICO2p(jp1, kLat, ialt))*(1-xLat)+&
                     (ICO2p(jLong,kp1,iAlt)*(1-xLong) &
                     +xLong*ICO2p(jp1, kp1, ialt))*xLat)*(1-xAlt)+&
                     ((ICO2p(jLong,kLat,ip1)*(1-xLong) &
                     +xLong*ICO2p(jp1, kLat, ip1))*(1-xLat)+&
                     (ICO2p(jLong,kp1,ip1)*(1-xLong) &
                     +xLong*ICO2p(jp1, kp1, ip1))*xLat)*xAlt

                tempIOP=&
                     ((IOp(jLong,kLat,iAlt)*(1-xLong) + &
                     xLong*IOp(jp1,kLat,ialt))*(1-xLat)+&
                     (IOp(jLong,kp1,iAlt)*(1-xLong) &
                     +xLong*IOp(jp1,kp1,ialt))*xLat)*(1-xAlt) +&
                     ((IOp(jLong,kLat,ip1)*(1-xLong) &
                     +xLong*IOp(jp1,kLat,ip1))*(1-xLat)+&
                     (IOp(jLong,kp1,ip1)*(1-xLong) &
                     +xLong*IOp(jp1,kp1,ip1))*xLat)*xAlt

                tempICO2p = max(tempICO2p, TINY)
                tempIOP = max(tempIOp, TINY)
                ! Ionizationrate_CBI(i,j,k,iBlock,CO2_)
                !   =tempICO2p*nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)
                ! Ionizationrate_CBI(i,j,k,iBlock,O_)
                !   =tempIOP*nDenNuSpecies_CBI(i,j,k,iBlock,O_)
                Ionizationrate_CBI(i,j,k,iBlock,CO2_)=tempICO2p
                Ionizationrate_CBI(i,j,k,iBlock,O_)=tempIOP

             else  ! hh > Alt_I(NAlt)

                ! In km unit
                dH= hh - Alt_I(NAlt)

                TempNeu_CB(i,j,k,iBlock)= &
                     (Temp(jLong,kLat,NAlt)*(1-xLong) &
                     +xLong*Temp(jp1, kLat,NAlt))*(1-xLat) + &
                     (Temp(jLong,kp1,NAlt)*(1-xLong) &
                     +xLong*Temp(jp1,kp1,NAlt))*xLat

                tempICO2p = &
                     (ICO2p(jLong,kLat,NAlt)*(1-xLong) &
                     +xLong*ICO2p(jp1, kLat, NAlt))*(1-xLat)+&
                     (ICO2p(jLong,kp1,NAlt)*(1-xLong) &
                     +xLong*ICO2p(jp1, kp1, NAlt))*xLat

                tempIOP = &
                     (IOp(jLong,kLat,NAlt)*(1-xLong) &
                     +xLong*IOp(jp1, kLat, NAlt))*(1-xLat)+&
                     (IOp(jLong,kp1,NAlt)*(1-xLong) &
                     +xLong*IOp(jp1, kp1, NAlt))*xLat

                ! Constant temperature and gravity from 300km to infinity !!!
                Grav = 3.72*(1.0+300.0/3396.0)**(-2)

                ! in m unit
                Hscale = cBoltzmann* &
                     TempNeu_CB(i,j,k,iBlock)/grav/cProtonMass

                ! in km unit
                HCO2 = Hscale/NuMassSpecies_I(CO2_)/1.0e3
                HO   = Hscale/NuMassSpecies_I(O_)/1.0e3

                nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)=&
                     ((Den_CO2(jLong,kLat,NAlt)*(1-xLong) &
                     +xLong*Den_CO2(jp1, kLat,Nalt))*(1-xLat)+&
                     (Den_CO2(jLong,kp1,NAlt)*(1-xLong) &
                     +xLong*Den_CO2(jp1,kp1,Nalt))*xLat)&
                     *exp(-dH/HCO2)
                nDenNuSpecies_CBI(i,j,k,iBlock,O_)=&
                     ((Den_O(jLong,kLat,NAlt)*(1-xLong) &
                     +xLong*Den_O(jp1, kLat,Nalt))*(1-xLat)+&
                     (Den_O(jLong,kp1,NAlt)*(1-xLong) &
                     +xLong*Den_O(jp1,kp1,Nalt))*xLat)&
                     *exp(-dH/HO)

                tempICO2p = max(tempICO2p, TINY)
                tempIOP   = max(tempIOp, TINY)
                ! Ionizationrate_CBI(i,j,k,iBlock,CO2_)
                !   =tempICO2p*nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)
                ! Ionizationrate_CBI(i,j,k,iBlock,O_)
                !   =tempIOP*nDenNuSpecies_CBI(i,j,k,iBlock,O_)
                Ionizationrate_CBI(i,j,k,iBlock,CO2_) = tempICO2p
                Ionizationrate_CBI(i,j,k,iBlock,O_)   = tempIOP

             end if ! hh < or > 300km
          end do
       end do
    end do

    if(DoTest)then
       write(*,*)'Mars input end', &
            dR,dPhi,dTheta, iBlock, &
            maxval(nDenNuSpecies_CBI(nI,:,:,iBlock,CO2_)),&
            minval(nDenNuSpecies_CBI(nI,:,:,iBlock,CO2_)),&
            maxval(r_GB(nI,:,:,iBlock)),&
            minval(r_GB(1,:,:,iBlock))
       write(*,*)'Mars input end2',&
            iBlock, maxval(nDenNuSpecies_CBI(nI,:,:,iBlock,O_)),&
            minval(nDenNuSpecies_CBI(nI,:,:,iBlock,O_)),&
            maxval(Ionizationrate_CBI(nI,:,:,iBlock,CO2_)),&
            minval(Ionizationrate_CBI(nI,:,:,iBlock,O_)),&
            maxval(r_GB(nI,:,:,iBlock)),&
            minval(r_GB(1,:,:,iBlock))
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine mars_input
  !============================================================================
  subroutine ua_input(iBlock)

    use ModGeometry, ONLY: TypeGeometry, r_GB
    use ModPhysics, ONLY: rBody, No2Si_V, UnitT_, Si2No_V, UnitN_

    integer, intent(in) :: iBlock

    integer :: i, j, k

    logical:: DoTest

    character(len=*), parameter:: NameSub = 'ua_input'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(TypeGeometry(1:9) /= 'spherical') &
         call stop_mpi('Invalid TypeGeometry='//TypeGeometry)

    ! Check if block is fully outside rMaxUa
    if (r_GB(1,1,1,iBlock) > rMaxUa) RETURN

    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if(r_GB(i,j,k,iBlock) > rMaxUa) CYCLE
       TempNeu_CB(i,j,k,iBlock) &              ! in [K] Not used...
            = UaState_VCB(1,i,j,k,iBlock)
       nDenNuSpecies_CBI(i,j,k,iBlock,CO2_) &  ! convert from m^-3 to cm^-3
            = UaState_VCB(2,i,j,k,iBlock)*Si2No_V(UnitN_)
       nDenNuSpecies_CBI(i,j,k,iBlock,O_) &    ! convert from m^-3 to cm^-3
            = UaState_VCB(3,i,j,k,iBlock)*Si2No_V(UnitN_)
       Ionizationrate_CBI(i,j,k,iBlock,CO2_) & ! convert s^-1 to normalized
            = UaState_VCB(4,i,j,k,iBlock)*No2Si_V(UnitT_)
       Ionizationrate_CBI(i,j,k,iBlock,O_) &  ! convert s^-1 to normalized
            = UaState_VCB(5,i,j,k,iBlock)*No2Si_V(UnitT_)
    end do; end do; end do

  end subroutine ua_input
  !============================================================================
  subroutine set_neutral_density(iBlock)

    ! Set neutral density and some related variables

    use ModGeometry, ONLY: Xyz_DGB, r_GB
    use ModPhysics, ONLY: rBody

    integer, intent(in):: iBlock

    real :: CosSZA, Optdep
    integer :: i, j, k

    ! Variables for chapman function
    real :: Xp, chap_y, chap, sinSZA

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_neutral_density'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! calculate neutral
    do k = 1, nK; do j = 1, nJ; do i = 1, nI

       if(r_GB(i,j,k,iBlock)<= Rbody)then
          nDenNuSpecies_CBI(i,j,k,iBlock,:) = BodynDenNuSpecies_I
       else if(r_GB(i,j,k,iBlock) < rMaxUa) then
          nDenNuSpecies_CBI(i,j,k,iBlock,:) = BodynDenNuSpecies_I &
               *exp(-(r_GB(i,j,k,iBlock) - rInNeu)/HNuSpecies_I)
       else
          nDenNuSpecies_CBI(i,j,k,iBlock,:) = 0.0
       end if

    end do; end do; end do

    ! calculate optical depth and producation rate
    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if(.not.UseChapman)then
          cosSZA=(0.5+sign(0.5,Xyz_DGB(x_,i,j,k,iBlock)))*&
               Xyz_DGB(x_,i,j,k,iBlock)/max(r_GB(i,j,k,iBlock),1.0e-3)&
               +5.0e-4
          Optdep = max(sum(nDenNuSpecies_CBI(i,j,k,iBlock,1:MaxNuSpecies)* &
               CrossSection_I(1:MaxNuSpecies)*HNuSpecies_I(1:MaxNuSpecies)), &
               6.0e-3)/cosSZA
          if( Optdep < 11.5 .and. Xyz_DGB(x_,i,j,k,iBlock) > 0.0) then
             Productrate_CB(i,j,k,iBlock) = max(exp(-Optdep), 1.0e-5)
          else
             Productrate_CB(i,j,k,iBlock) = 1.0e-5
          end if
       else
          Optdep=HNuSpecies_I(CO2_)*nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)*&
               CrossSection_I(CO2_)
          cosSZA = Xyz_DGB(x_,i,j,k,iBlock) /r_GB(i,j,k,iBlock)
          if(Optdep > 13.8) then
             Productrate_CB(i,j,k,iBlock) = 1.0e-6*&
                  max(cosSZA, 1.0e-6)
          else
             Xp=r_GB(i,j,k,iBlock)/HNuSpecies_I(1)
             chap_y= sqrt(0.5*Xp)*abs(cosSZA)
             if(cosSZA > 0.0)then   ! SZA<90 deg (equation 13)
                if(chap_y < 8.0)then
                   chap = sqrt(cPi/2.0*Xp)*&
                        (1.0606963 + 0.5564383*chap_y)/&
                        (1.0619896 + 1.7245609*chap_y + chap_y**2)
                elseif(chap_y < 100.0)then
                   chap = sqrt(cPi/2.0*Xp)*&
                        0.56498823/(0.6651874 + chap_y)
                else
                   chap = 0.0
                end if
             else
                ! 180 >SZA > 90 deg (equation 15) Smith and Smith, 1972
                sinSZA = sqrt(max(1.0 - cosSZA**2, 0.0))
                if(chap_y < 8.0)then
                   chap = sqrt(2*cPi*Xp)* &
                        (sqrt(sinSZA)*exp(Xp*(1.0 - sinSZA)) &
                        - 0.5*(1.0606963 + 0.5564383*chap_y)/ &
                        (1.0619896 + 1.7245609*chap_y + chap_y**2))
                elseif(chap_y < 100.0)then
                   chap = sqrt(2*cPi*Xp)* &
                        (sqrt(sinSZA)*exp(min(100.0, Xp*(1.0 - sinSZA))) &
                        - 0.5*0.56498823/(0.6651874 + chap_y))
                else
                   chap = 0.0
                end if
             end if

             Optdep = Optdep*chap

             ! if(DoTest.and.i==iTest.and.j==jTest.and.k==kTest)then
             !   write(*,*)'!!! iNeuSpecies= ?'
             !   write(*,*)'!!! Optdep, chap, minProd=', Optdep, chap, 1e-6
             !   write(*,*)'!!! Productrate_CB=', 1.0
             ! endif

             Productrate_CB(i,j,k,iBlock) = max(exp(-Optdep), 1.0e-6)

          end if
       end if
    end do; end do; end do

    if(DoTest)then
       write(*,*)'nDenNuSpecies_CBI(iTest,jTest,kTest,iBlockTest,:)=',&
            nDenNuSpecies_CBI(iTest,jTest,kTest,iBlock,:)
    end if

    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       nDenNuSpecies_CBI(i,j,k,iBlock,O_) = &
            nDenNuSpecies_CBI(i,j,k,iBlock,O_)+ &
            nDenNuSpecies_CBI(i,j,k,iBlock,Ox_)

       nDenNuSpecies_CBI(i,j,k,iBlock,CO2_) = &
            nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)+ &
            nDenNuSpecies_CBI(i,j,k,iBlock,CO2x_)

       if(UseHotO) then
          nDenNuSpecies_CBI(i,j,k,iBlock,O_) = &
               nDenNuSpecies_CBI(i,j,k,iBlock,O_) + &
               nDenNuSpecies_CBI(i,j,k,iBlock,Oh_) + &
               nDenNuSpecies_CBI(i,j,k,iBlock,Ohx_)

          nDenNuSpecies_CBI(i,j,k,iBlock,H_) = &
               nDenNuSpecies_CBI(i,j,k,iBlock,H_)+ &
               nDenNuSpecies_CBI(i,j,k,iBlock,Hx_)

       else
          nDenNuSpecies_CBI(i,j,k,iBlock,H_) = 1e-5
       end if

       Nu_CB(i,j,k,iBlock) = (nDenNuSpecies_CBI(i,j,k,iBlock,CO2_) + &
            nDenNuSpecies_CBI(i,j,k,iBlock,O_) +  &
            nDenNuSpecies_CBI(i,j,k,iBlock,H_))*nu0
    end do; end do; end do

    if(IsNewUaState) call ua_input(iBlock)
    if(UseFileAtm) call mars_input(iBlock)

    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if( (IsNewUaState .or. UseFileAtm) &
            .and. r_GB(i,j,k,iBlock) <= rMaxUa)then
          ! Inside neutral model
          if(UseHotO) nDenNuSpecies_CBI(i,j,k,iBlock,O_) = &
               nDenNuSpecies_CBI(i,j,k,iBlock,O_) + &
               nDenNuSpecies_CBI(i,j,k,iBlock,Oh_) + &
               nDenNuSpecies_CBI(i,j,k,iBlock,Ohx_)

          Nu_CB(i,j,k,iBlock) = nu0*( &
               nDenNuSpecies_CBI(i,j,k,iBlock,CO2_) + &
               nDenNuSpecies_CBI(i,j,k,iBlock,O_))
          ! Sometime H_ is added, sometimes not !!!
          ! + nDenNuSpecies_CBI(i,j,k,iBlock,H_) )

          Ionizationrate_CBI(i,j,k,iBlock,CO2_) = &
               Ionizationrate_CBI(i,j,k,iBlock,CO2_)* &
               nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)
          Ionizationrate_CBI(i,j,k,iBlock,O_) = &
               Ionizationrate_CBI(i,j,k,iBlock,O_)* &
               nDenNuSpecies_CBI(i,j,k,iBlock,O_)
       else
          ! Outside neutral model (if any)
          Ionizationrate_CBI(i,j,k,iBlock,O_) = Rate_I(O_hv__Op_em_) &
               *nDenNuSpecies_CBI(i,j,k,iBlock,O_) &
               *Productrate_CB(i,j,k,iBlock)

          Ionizationrate_CBI(i,j,k,iBlock,CO2_) = Rate_I(CO2_hv__CO2p_em_) &
               *nDenNuSpecies_CBI(i,j,k,iBlock,CO2_) &
               *Productrate_CB(i,j,k,iBlock)
       end if
       if(DoTest .and. i==iTest .and. j==jTest .and. k==kTest)then
          write(*,*)'Rate_I(CO2_hv__CO2p_em_)=',Rate_I(CO2_hv__CO2p_em_)
          write(*,*)'nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)=', &
               nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)
          write(*,*)'Productrate_CB(i,j,k,iBlock)=', &
               Productrate_CB(i,j,k,iBlock)
          write(*,*)'Ionizationrate_CBI(i,j,k,iBlock,CO2_)=', &
               Ionizationrate_CBI(i,j,k,iBlock,CO2_)
       end if

    end do; end do; end do

    if(DoTest)then
       write(*,*)'usehoto=', UseHotO
       write(*,*)'nDenNuSpecies_CBI(iTest,jTest,kTest,iBlockTest,:)=', &
            nDenNuSpecies_CBI(iTest,jTest,kTest,iBlock,:)
       write(*,*)
       write(*,*)'nu(testcell)=', Nu_CB(iTest,jTest,kTest,iBlock)
       write(*,*)
       write(*,*)'ProductRate_CB(testcell)=', &
            ProductRate_CB(iTest,jTest,kTest,iBlock)
       write(*,*)
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine set_neutral_density
  !============================================================================
  subroutine user_set_plot_var(iBlock, NameVar, IsDimensional, &
       PlotVar_G, PlotVarBody, UsePlotVarBody, &
       NameTecVar, NameTecUnit, NameIdlUnit, IsFound)

    use ModVarIndexes, ONLY: RhoHp_, RhoCO2p_, RhoO2p_, RhoOp_
    use ModPhysics, ONLY: No2Io_V, UnitN_, UnitT_, UnitTemperature_, &
         NameTecUnit_V, NameIdlUnit_V
    use ModAdvance, ONLY: State_VGB, Rho_

    integer,          intent(in) :: iBlock
    character(len=*), intent(in) :: NameVar
    logical,          intent(in) :: IsDimensional
    real,             intent(out):: PlotVar_G(-1:nI+2, -1:nJ+2, -1:nK+2)
    real,             intent(out):: PlotVarBody
    logical,          intent(out):: UsePlotVarBody
    character(len=*), intent(out):: NameTecVar
    character(len=*), intent(out):: NameTecUnit
    character(len=*), intent(out):: NameIdlUnit
    logical,          intent(out):: IsFound

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_plot_var'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(DoTest)write(*,*) NameSub,' starting, iBlock, NameVar=', iBlock, NameVar

    IsFound = .true.
    PlotVarBody = 0.0
    NameTecUnit = NameTecUnit_V(UnitN_)
    NameIdlUnit = NameIdlUnit_V(UnitN_)

    select case(NameVar)
    case('tneu')
       PlotVar_G(1:nI,1:nJ,1:nK) = TempNeu_CB(:,:,:,iBlock)
       PlotVarBody = TNu_body_dim
       NameTecVar = 'Tneu'
       NameTecUnit = NameTecUnit_V(UnitTemperature_)
       NameIdlUnit = NameIdlUnit_V(UnitTemperature_)
    case('nco2')
       PlotVar_G(1:nI,1:nJ,1:nK) = &
            nDenNuSpecies_CBI(:,:,:,iBlock,CO2_)*No2Io_V(UnitN_)
       PlotVarBody = BodynDenNuSpDim_I(CO2_)
       NameTecVar = 'CO2'
    case('no')
       PlotVar_G(1:nI,1:nJ,1:nK) = &
            nDenNuSpecies_CBI(:,:,:,iBlock,O_)*No2Io_V(UnitN_)
       PlotVarBody = BodynDenNuSpDim_I(O_)
       NameTecVar = 'O'
    case('noh')
       PlotVar_G(1:nI,1:nJ,1:nK) = &
            nDenNuSpecies_CBI(:,:,:,iBlock,OH_)*No2Io_V(UnitN_)
       PlotVarBody = BodynDenNuSpDim_I(OH_)
       NameTecVar = 'Oh'
    case('nh')
       PlotVar_G(1:nI,1:nJ,1:nK) = &
            nDenNuSpecies_CBI(:,:,:,iBlock,H_)*No2Io_V(UnitN_)
       PlotVarBody = BodynDenNuSpDim_I(H_)
       NameTecVar = 'H'
    case('iop')
       PlotVar_G(1:nI,1:nJ,1:nK) = Ionizationrate_CBI(:,:,:,iBlock,O_) &
            *(No2Io_V(UnitN_)/No2Io_V(UnitT_))
       NameTecVar = 'IOp'
       NameTecUnit = trim(NameTecUnit)//'/s'
       NameIdlUnit = trim(NameIdlUnit)//'/s'
    case('ico2p')
       PlotVar_G(1:nI,1:nJ,1:nK) = Ionizationrate_CBI(:,:,:,iBlock,CO2_) &
            *(No2Io_V(UnitN_)/No2Io_V(UnitT_))
       NameTecVar = 'ICO2p'
       NameTecUnit = trim(NameTecUnit)//'/s'
       NameIdlUnit = trim(NameIdlUnit)//'/s'
    case('prod')
       ! This is actually the optical depth
       PlotVar_G(1:nI,1:nJ,1:nK) = Productrate_CB(:,:,:,iBlock)
       NameTecVar = 'prod'
       NameTecUnit = '1'
       NameIdlUnit = '1'
    case default
       IsFound = .false.
    end select

    UsePlotVarBody = .true.

    if(DoTest)write(*,*) NameSub, ' done: iBlock, NameVar=', iBlock, NameVar

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_plot_var
  !============================================================================
end module ModUser
!==============================================================================
