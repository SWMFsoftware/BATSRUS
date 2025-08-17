!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModUser

  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest, iBlockTest, iProc
  ! This is the user module for Mars

  use ModSize
  use ModUserEmpty,               &
       IMPLEMENTED1 => user_set_ics,                    &
       IMPLEMENTED2 => user_calc_sources_impl,          &
       IMPLEMENTED3 => user_init_point_implicit,        &
       IMPLEMENTED4 => user_init_session,               &
       IMPLEMENTED5 => user_read_inputs,                &
       IMPLEMENTED6 => user_set_face_boundary,          &
       IMPLEMENTED7 => user_set_plot_var,               &
       IMPLEMENTED8 => user_get_log_var,                &
       IMPLEMENTED9 => user_set_boundary_cells,         &
       IMPLEMENTED10=> user_get_b0

  use ModMultiFluid
  use ModAdvance, ONLY: Pe_, UseElectronPressure

  include 'user_module.h' ! list of public methods

  ! user routine Version number and descriptive string.
  character (len=*), parameter :: NameUserFile = "ModUserMarsFluids.f90"
  character (len=*), parameter :: NameUserModule = &
       'Mars 5 Fluids + Pe MHD code, Dalal Najib'

  character (len=10) :: SolarCond='solarmax  '

  ! Radius within which the point implicit scheme should be used
  real :: rPointImplicit = 4.0

  ! Mars stuff

  integer, parameter :: MaxSpecies=nIonFluid, MaxNuSpecies=9,  &
       MaxReactions=11, nNuSpecies=3
  ! Number density of neutral species
  real, allocatable:: nDenNuSpecies_CBI(:,:,:,:,:)
  ! Temperature of neutral species
  real, allocatable:: TempNuSpecies_CBI(:,:,:,:)
  ! Production rate according to optical depth
  real, allocatable:: Productrate_CB(:,:,:,:)
  ! Ionization rate
  real, allocatable:: Ionizationrate_CBI(:,:,:,:,:)
  ! Max ion species
  real, allocatable:: MaxSiSpecies_CB(:,:,:,:), MaxLiSpecies_CB(:,:,:,:),&
       MaxSLSpecies_CB(:,:,:,:)
  real, allocatable:: nu_BLK(:,:,:,:)

  real, dimension(MaxReactions):: ReactionRate_I
  real, dimension(MaxReactions,nIonFluid):: CoeffSpecies_II, &
       dSdRho_II !, dLdRho_II
  real, dimension(nIonFluid):: LossSpecies_I, &
       SiSpecies_I, LiSpecies_I, PhoIon_I, Recb_I, ImpIon_I
  !$omp threadprivate( ReactionRate_I, CoeffSpecies_II, dSdRho_II )
  !$omp threadprivate( LossSpecies_I, SiSpecies_I, LiSpecies_I )
  !$omp threadprivate( PhoIon_I, Recb_I, ImpIon_I )

  ! Reactions considered:(p means ion, em means electron)
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
       H_hv__Hp_em_    =10,&  ! H+hv-->Hp+em
       CO2_hv__Op_CO_em_=11 ! CO2+hv-->Op+CO_em

  real, dimension(MaxReactions) :: Rate_I
  real, dimension(MaxReactions) :: Ratedim_I &
       =[ 2.47e-7, 8.89e-8, 1.64e-10, 1.1e-9, 9.60e-11, 7.38e-8, 3.1e-7, &
       5.084e-10, 6.4e-10, 5.58e-8, 0.0 ] ! [cm^3 s^(-1)]

  integer, parameter :: &! order of ion fluids
       Hp_  =1, &
       O2p_ =2, &
       Op_  =3, &
       CO2p_=4

  real, dimension(nNuSpecies), parameter:: MassNeutral_I &
       =[44., 16., 1. ]  ! atm

  integer, parameter :: & ! order of Neutral species
       CO2_=1 ,&
       O_=2   ,&
       H_=3, Op2_=3, &
       Oh_=4   ,&
       Ohx_=5 , &
       Hx_=6, &
       Ox_=7 ,&
       CO2x_=8,&
       Oh2x_ =9

  real, dimension(MaxNuSpecies):: CrossSection_I ! [cm^2]
  real, dimension(MaxNuSpecies), parameter:: CrossSectionDim_I &
       =[2.6e-17,1.5e-17,0.0,1.5e-17,&
       1.5e-17,0.0,1.5e-17,2.6e-17, 1.5e-17] ! [cm^2]

  real, dimension(nIonFluid,nNuSpecies):: ReducedMassNeutral_II
  real, dimension(nIonFluid,nNuSpecies):: ReducedMassNeutral2_II
  real, dimension(nIonFluid,nIonFluid):: ReducedMassIon_II

  real, dimension(MaxNuSpecies), parameter:: NuMassSpecies_I &
       =[44,16,1,16,16,1,16, 44, 16]
  !  NuMassSpecies_I(CO2_)=44	! atm
  !  NuMassSpecies_I(O_)=16	! atm

  real, dimension(MaxNuSpecies):: HNuSpecies_I=1.0, HNuSpeciesDim_I=1.0
  ! HNuSpecies_dim_I(CO2_)=6.7e3   ! m
  ! HNuSpecies_dim_I(O_)=18.4e3    ! m

  real, dimension(MaxNuSpecies):: BodynDenNuSpecies_I,&
       BodynDenNuSpDim_I=[0.0, 0.0, 0.0, 0.0, &
       0.0, 0.0, 0.0, 0.0, 0.0]
  !       BodynDenNuSpDim_I=(/1.1593e12, 3.2278e9, 1.1307e7, 1.951e4, &
  !       1.5248e3, 9.4936e5, 5.2695e8, 2.2258e11, 3.71e4/)

  real :: BodyRhoIon_I(nIonFluid)
  integer, parameter :: & ! other numbers
       em_=-1 ,&
       hv_=-2

  real :: TNu_body_dim=300.0, TNu_body ! neutral temperature
  real :: Ti_body_dim=300.0, Ti_body   ! ion temperature at the body
  real, parameter:: T300_dim=300.0
  real, parameter:: nu0_dim=4.0e-10
  real :: T300, nu0
  real :: IonNeuCoeff_II(1:nIonFluid,1:nNuSpecies)
  real :: IonIonCoeff_II(1:nIonFluid,1:nIonFluid)

  real, parameter:: IonNeuCoeffDim_II(1:nIonFluid,1:nNuSpecies)= reshape( [ &
       41.4, 5.63, 8.95, 4., &
       30., 2.31, 4., 1.76, &
       4., 0.65, 1., 0.47 ], [4, 3] )

  real, parameter:: IonIonCoeffDim_II(1:nIonFluid,1:nIonFluid)=reshape( [ &
       0.90, 0.039, 0.077, 0.029, &
       1.25, 0.16, 0.26, 0.12, &
       1.23, 0.13, 0.22, 0.10,&
       1.26, 0.17, 0.30, 0.14 ], [4, 4] )

  ! From Schunk and Nagy
  ! nu_in=Cin*DensNeu, here we have Cin*e+10

  real :: Te_new_dim=8000., KTe0  ! 0.7 ev

  ! coefficient of Mars magnetic field
  real, dimension(0:61,0:61) :: cmars, dmars
  integer :: NNm
  real :: mars_eps=1e-4
  real :: rot = 1.0, thetilt = 0.0
  logical :: UseHotO = .false.
  ! real :: CoeffHotO =1.0
  logical :: UseTempCont=.false.
  logical :: UseMarsB0 = .false.
  logical :: UseIssiC=.false.
  logical :: UseIssiA=.false.
  logical :: UseImpactIon=.false.
  logical :: UseChargeEx=.true.
  ! logical ::InitSession= .false.

  integer, parameter:: MaxLong=73, MaxLat=36, MaxAlt=81, MaxAltD=269
  real :: Long_I(MaxLong), Lat_I(MaxLat), Alt_I(MaxAlt), Alt0
  real :: Temp(MaxLong, MaxLat, MaxAlt)
  real :: Den_CO2(MaxLong, MaxLat, MaxAlt)!,Den_CO2_dim(NLong, NLat, NAlt)
  real :: Den_O(MaxLong, MaxLat, MaxAlt)!,Den_O_dim(NLong, NLat, NAlt)
  real :: ICO2p(MaxLong, MaxLat, MaxAlt)!,ICO2p_dim(NLong, NLat, NAlt)
  real :: IOp(MaxLong, MaxLat, MaxAlt)!,IOp_dim(NLong, NLat, NAlt)
  real :: IOp2(MaxLong, MaxLat, MaxAlt)
  real :: SZA(MaxLong, MaxLat, MaxAlt)
  real :: LongD_I(MaxLong), LatD_I(MaxLat), AltD_I(MaxAltD)
  real :: Den_Oh(MaxLong, MaxLat, MaxAltD)
  logical :: UseMarsAtm=.false.
  logical :: UseDSMC=.false.
  ! integer :: NAlt=81
  integer :: NAlt, NLong, NLat, NLine
  integer :: NAltD, NLongD, NLatD, NLineD
  real :: subsolarlong, subsolarlat
  real :: longstart, latstart, altstart
  real :: dlong, dlat, dalt

  ! end mars stuff

  logical:: UseOldEnergy=.false.

contains
  !============================================================================
  subroutine user_calc_sources_impl(iBlock)

    use ModPointImplicit, ONLY: UsePointImplicit
    use ModMain,    ONLY: nI, nJ, nK, iNewGrid, iNewDecomposition
    use ModPhysics, ONLY: Rbody,UnitTemperature_, No2Io_V, No2Si_V, Io2No_V,&
         UnitT_,UnitN_,ElectronPressureRatio, &
         GammaMinus1, GammaMinus1_I, InvGammaMinus1, InvGammaMinus1_I
    use ModAdvance, ONLY: State_VGB, Source_VC, Flux_VXI,&
         Flux_VYI, Flux_VZI, Vdt_
    use ModGeometry, ONLY: r_GB,Xyz_DGB
    use ModVarIndexes, ONLY: HpRho_, O2pRho_, OpRho_, CO2pRho_, &
         HpP_, O2pP_, OpP_, CO2pP_, Bx_, By_, Bz_
    use BATL_lib, ONLY: CellVolume_GB

    integer, intent(in) :: iBlock

    integer :: i,j,k,n,iFluid
    real :: vdtmin
    real :: inv_rho,inv_rho2,uu2,Productrate,kTi,kTe
    real :: NumDens, InvNumDens
    real :: OptDep
    real, dimension(nIonFluid) :: NumDens_I, InvRho_I, uu2_I,&
         Temp_I,&
         LossNumRho_I, SourceNumRho_I, Lossx_I,LossNumx_I,&
         RLNumRhox_I, tmp_I, tmp_nu_I,col_ii_I

    real :: col_ei, col_en, col_ei_I(nIonFluid), col_en_I(3), averagemass
    real, parameter :: meovmi=5.44471e-4 ! me/mi=9.109e-31/1.673e-27
    real :: Te_dim, tmp, cosSZA
    real::  totalLossRho, totalSourceRho, totalLossNumRho, &
         totalSourceNumRho, totalLossx, totalLossNumx, SourceLossMax,&
         totalIMPNumRho
    real :: totalPSNumRho, totalRLNumRhox
    real :: xMinBox, xMaxBox, X3, X4
    real, parameter :: A0=-1143.33, A1=323.767, A2=-35.0431, A3=1.69073, &
         A4=-0.0306575
    real, parameter :: B0=-1233.29, B1=347.764, B2=-37.4128, B3=1.79337, &
         B4=-0.0322777
    real :: IonNeuRate_II(1:nIonFluid,1:nNuSpecies)

    integer :: iLastGrid=-100, iLastDecomposition=-100
    !$omp threadprivate( iLastGrid, iLastDecomposition )

    logical:: DoTestCell
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_calc_sources_impl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(nDenNuSpecies_CBI(1, 1, 1, iBlock, 1) < 0.0 &
         .or. iLastGrid /= iNewGrid &
         .or. iLastDecomposition /= iNewDecomposition)then
       call set_neutral_density(iBlock)

       if(iBlock == nBlock)then
          iLastGrid          = iNewGrid
          iLastDecomposition = iNewDecomposition
       end if
    end if

    ! Check if inside rPointImplicit
    ! hyzhou: like ModUserMars.f90, this should be using UseUserPointImplicit_B
    ! instead!
    if(r_GB(1,1,1,iBlock) > rPointImplicit) RETURN

    ! chemistry etc

    Te_dim = 300.0

    do k=1,nK; do j=1,nJ; do i=1,nI
       cosSZA = (0.5+sign(0.5,Xyz_DGB(x_,i,j,k,iBlock)))*&
            Xyz_DGB(x_,i,j,k,iBlock)/max(r_GB(i,j,k,iBlock),1.0e-3)&
            +5.0e-4
       OptDep = max( sum(nDenNuSpecies_CBI(i,j,k,iBlock,1:MaxNuSpecies)*&
            CrossSection_I(1:MaxNuSpecies)*HNuSpecies_I(1:MaxNuSpecies)),&
            6.0e-3)/cosSZA
       if( OptDep<11.5 .and. Xyz_DGB(x_,i,j,k,iBlock) > 0.0) then
          Productrate_CB(i,j,k,iBlock) = max(exp(-OptDep), 1.0e-5)
       else
          Productrate_CB(i,j,k,iBlock) = 1.0e-5
       end if
    end do; end do; end do

    do k=1,nK; do j=1,nJ; do i=1,nI

       DoTestCell = DoTest .and. i==iTest .and. j==jTest .and. k==kTest

       NumDens_I  = State_VGB(iRhoIon_I,i,j,k,iBlock)/MassIon_I
       NumDens    = sum(NumDens_I)
       InvNumDens = 1.0/NumDens

       Temp_I     = State_VGB(iPIon_I,i,j,k,iBlock)/NumDens_I
       ! Temp_I=TNu_body
       ! State_VGB(iPIon_I,i,j,k,iBlock)=2*Temp_I*NumDens_I

       if(DoTestCell)then
           write(*,*)NameSub,',i,j,k,iBlock=',iTest,jTest,kTest,iblock
        end if

       InvRho_I = 1.0/State_VGB(iRho_I,i,j,k,iBlock)

       if (r_GB(i,j,k,iBlock) < Rbody) CYCLE

       InvRho_I = 1.0/State_VGB(iRhoIon_I,i,j,k,iBlock)
       inv_rho = 1.0/sum(State_VGB(iRhoIon_I,i,j,k,iBlock))
       inv_rho2 = inv_rho**2
       uu2 = sum(State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)**2) * inv_rho2

       do iFluid = 1, nIonFluid
          uu2_I(iFluid) = &
               ( State_VGB(iRhoUxIon_I(iFluid),i,j,k,iBlock)**2  &
               + State_VGB(iRhoUyIon_I(iFluid),i,j,k,iBlock)**2  &
               + State_VGB(iRhoUzIon_I(iFluid),i,j,k,iBlock)**2) &
               * InvRho_I(iFluid)**2
       end do

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

       LossNumRho_I   = 0.0
       SourceNumRho_I = 0.0
       Lossx_I        = 0.0
       LossNumx_I     = 0.0
       RLNumRhox_I    = 0.0

       MaxSLSpecies_CB(i,j,k,iBlock) = 1.0e-3

       ! calculate optical depth and producation rate
       Productrate = Productrate_CB(i,j,k,iBlock)

       ! calculate Ion neutral collision rate: nu_in=C_in*n_n,
       ! Unit[C_in]:cm^3*s^-1
       do n=1,nNuSpecies
          IonNeuRate_II(:,n) = IonNeuCoeff_II(:,n) &
               *nDenNuSpecies_CBI(i,j,k,iBlock,n)
       end do

       ReactionRate_I(H_hv__Hp_em_) = &
            Rate_I(H_hv__Hp_em_)*nDenNuSpecies_CBI(i,j,k,iBlock,H_)
       PhoIon_I(Hp_) = ReactionRate_I(H_hv__Hp_em_)*Productrate
       PhoIon_I(CO2p_) = Ionizationrate_CBI(i,j,k,iBlock,CO2_)
       PhoIon_I(Op_) = Ionizationrate_CBI(i,j,k,iBlock,O_)

       ! IMPACT Ionization
       ImpIon_I=0.0
       if(UseImpactIon)then
          xMinBox=log(Te_dim)
          xMaxBox=xMinBox*xMinBox
          X3=xMaxBox*xMinBox
          X4=xMaxBox*xMaxBox
          ImpIOn_I(Hp_)=exp(A0+A1*xMinBox+A2*xMaxBox+A3*X3+A4*X4)&
               *No2Io_V(UnitT_)*No2Io_V(UnitN_)
          ImpIOn_I(Op_)=exp(B0+B1*xMinBox+B2*xMaxBox+B3*X3+B4*X4)&
               *No2Io_V(UnitT_)*No2Io_V(UnitN_)
          ImpIon_I(Hp_)=ImpIon_I(Hp_)*&
               nDenNuSpecies_CBI(i,j,k,iBlock,H_)
          ImpIon_I(Op_)=ImpIon_I(Op_)*&
               nDenNuSpecies_CBI(i,j,k,iBlock,O_)
          ImpIon_I=ImpIon_I*NumDens

       end if

       if(.not. UseElectronPressure)then
          State_VGB(P_,i,j,k,iBlock)=sum(State_VGB(iPIon_I,i,j,k,iBlock))*(1+ElectronPressureRatio)
       else
          State_VGB(P_,i,j,k,iBlock)=sum(State_VGB(iPIon_I,i,j,k,iBlock))
       end if

       ! charge exchange

       ReactionRate_I(CO2p_O__O2p_CO_)= &
            Rate_I(CO2p_O__O2p_CO_)&
            * nDenNuSpecies_CBI(i,j,k,iBlock,O_)
       CoeffSpecies_II(O2p_,CO2p_)=ReactionRate_I(CO2p_O__O2p_CO_)

       ReactionRate_I(Op_CO2__O2p_CO_)= &
            Rate_I(Op_CO2__O2p_CO_)&
            * nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)&
            *(Tnu_body_dim &
            /max(T300_dim, Temp_I(Op_)*No2Si_V(UnitTemperature_)))**0.39
       CoeffSpecies_II(O2p_, Op_)=ReactionRate_I(Op_CO2__O2p_CO_)

       ReactionRate_I(CO2p_O__Op_CO2_)= &
            Rate_I(CO2p_O__Op_CO2_)&
            * nDenNuSpecies_CBI(i,j,k,iBlock,O_)
       CoeffSpecies_II(Op_,CO2p_)=ReactionRate_I(CO2p_O__Op_CO2_)

       ReactionRate_I(Hp_O__Op_H_)= &
            Rate_I(Hp_O__Op_H_)* nDenNuSpecies_CBI(i,j,k,iBlock,O_)
       CoeffSpecies_II(Op_,Hp_)=ReactionRate_I(Hp_O__Op_H_)

       ReactionRate_I(Op_H__Hp_O_)= &
            Rate_I(Op_H__Hp_O_)* nDenNuSpecies_CBI(i,j,k,iBlock,H_)
       CoeffSpecies_II(Hp_,Op_)=ReactionRate_I(Op_H__Hp_O_)

       ! Recombination

       if(.not.UseElectronPressure)then
          kTi=State_VGB(P_,i,j,k,iBlock)/NumDens/(1 + ElectronPressureRatio)
       else
          kTi=State_VGB(P_,i,j,k,iBlock)/NumDens
       end if

       kTe = kTi

       if(kTi <= 0.0) &
            call stop_mpi(NameSub//'Negative ion temperature!')

       ReactionRate_I(O2p_em__O_O_) = Rate_I(O2p_em__O_O_)
       Recb_I(O2p_) = ReactionRate_I(O2p_em__O_O_)*(TNu_body/kTi)**0.56

       ! ReactionRate_I(O2p_em__O_O_)*exp(log(TNu_body/Temp_I(2))*0.56)

       ReactionRate_I(CO2p_em__CO_O_) = Rate_I(CO2p_em__CO_O_)
       Recb_I(CO2p_) = ReactionRate_I(CO2p_em__CO_O_)*sqrt(TNu_body/kTi)

       LossSpecies_I = sum( CoeffSpecies_II, DIM=1 )

       do iFluid = 1, nIonFluid
          ! LossSpecies_I = LossSpecies_I + CoeffSpecies_II(iFluid, :)
          dSdRho_II(1:nIonFluid, iFluid)= &
               CoeffSpecies_II(1:nIonFluid,iFluid)*MassIon_I/MassIon_I(iFluid)
       enddo

       SiSpecies_I = (PhoIon_I+ImpIon_I)*MassIon_I

       do iFluid = 1, nIonFluid
          SiSpecies_I(1:nIonFluid) = &
               SiSpecies_I(1:nIonFluid) + dSdRho_II(1:nIonFluid, iFluid) &
               *State_VGB(iRhoIon_I(iFluid),i,j,k,iBlock)

          LiSpecies_I(iFluid)= &
               LiSpecies_I(iFluid) &
               + (LossSpecies_I(iFluid) + Recb_I(iFluid)*NumDens) &
               *State_VGB(iRhoIon_I(iFluid),i,j,k,iBlock)
       enddo

       totalIMPNumRho = sum(ImpIon_I(:))

       ! SiSpecies_I(3)=SiSpecies_I(2)
       ! LiSpecies_I(3)=SiSpecies_I(2)

       totalSourceRho=sum(SiSpecies_I(1:nIonFluid))
       totalLossRho=sum(LiSpecies_I(1:nIonFluid))
       ! sum of the (Loss term) of all ion species
       totalLossNumRho = sum(LiSpecies_I/MassIon_I)
       LossNumRho_I = LiSpecies_I/MassIon_I
       ! sum of the (loss term/atom mass) of all ..
       totalSourceNumRho = sum(SiSpecies_I/MassIon_I)
       SourceNumRho_I    = SiSpecies_I/MassIon_I
       ! sum of the (Source term/atom mass) of all..
       totalLossx=totalLossRho*inv_rho
       Lossx_I = LiSpecies_I*invRho_I
       totalLossNumx = totalLossNumRho/NumDens
       LossNumx_I = LossNumRho_I/NumDens_I
       totalPSNumRho = sum(PhoIon_I)
       ! sum of the photonionziation source/atom mass) of all..
       totalRLNumRhox = sum(Recb_I*State_VGB(iRhoIon_I,i,j,k,iBlock)/MassIon_I)
       RLNumRhox_I    =     Recb_I*State_VGB(iRhoIon_I,i,j,k,iBlock)/MassIon_I
       ! sum of the (loss term/atom mass) due to recombination

       MaxSLSpecies_CB(i,j,k,iBlock)=maxval(abs(SiSpecies_I(1:nIonFluid)+&
            LiSpecies_I(1:nIonFluid) ) /&
            (State_VGB(iRhoIon_I(1:nIonFluid), i,j,k, iBlock)+1e-20))&
            *CellVolume_GB(i,j,k,iBlock)

       ! Limit timestep for explicit scheme
       if(.not.UsePointImplicit)then
          ! sum of the (loss term/atom mass) due to recombination
          SourceLossMax = 10.0*maxval(abs(SiSpecies_I(1:nIonFluid)-&
               LiSpecies_I(1:nIonFluid) ) /&
               (State_VGB(iRhoIon_I(1:nIonFluid), i,j,k, iBlock)+1e-20))&
               *CellVolume_GB(i,j,k,iBlock)
          vdtmin = min(Flux_VXI(Vdt_,i,j,k,1), Flux_VYI(Vdt_,i,j,k,1), &
               Flux_VZI(Vdt_,i,j,k,1))

          if(SourceLossMax > Vdtmin)then
             ! UsePointImplicit_B(iBlock)=.true.
             Flux_VXI(Vdt_,i,j,k,1) &
                  = max(SourceLossMax, Flux_VXI(Vdt_,i,j,k,1) )
             Flux_VYI(Vdt_,i,j,k,1) &
                  = max(SourceLossMax, Flux_VYI(Vdt_,i,j,k,1) )
             Flux_VZI(Vdt_,i,j,k,1) &
                  = max(SourceLossMax, Flux_VZI(Vdt_,i,j,k,1) )
          end if
       end if

       if(DoTestCell)then
          write(*,*) 'Before ',NameSub,'Source_VC(rho_)=',Source_VC(rho_,i,j,k)
       end if

       Source_VC(iRhoIon_I,i,j,k) = Source_VC(iRhoIon_I,i,j,k) &
            + SiSpecies_I - LiSpecies_I

       Source_VC(rho_     ,i,j,k) = Source_VC(rho_     ,i,j,k) &
            +sum(SiSpecies_I(1:nIonFluid))&
            -sum(LiSpecies_I(1:nIonFluid))

       Source_VC(rhoUx_     ,i,j,k) = Source_VC(rhoUx_     ,i,j,k) &
            -State_VGB(rhoUx_,i,j,k,iBlock)*totalLossx&
            -nu_BLK(i,j,k,iBlock)*State_VGB(rhoUx_,i,j,k,iBlock)

       Source_VC(iRhoUxIon_I,i,j,k) = Source_VC(iRhoUxIon_I,i,j,k) &
            -State_VGB(iRhoUxIon_I,i,j,k,iBlock)*Lossx_I &
            -sum(IonNeuRate_II,DIM = 2)*State_VGB(iRhoUxIon_I,i,j,k,iBlock)

       Source_VC(rhoUy_     ,i,j,k) = Source_VC(rhoUy_     ,i,j,k) &
            -State_VGB(rhoUy_,i,j,k,iBlock)*totalLossx&
            -nu_BLK(i,j,k,iBlock)*State_VGB(rhoUy_,i,j,k,iBlock)

       Source_VC(iRhoUyIon_I,i,j,k) = Source_VC(iRhoUyIon_I,i,j,k) &
            -State_VGB(iRhoUyIon_I,i,j,k,iBlock)*Lossx_I &
            -sum(IonNeuRate_II, DIM = 2) *State_VGB(iRhoUyIon_I,i,j,k,iBlock)

       Source_VC(rhoUz_     ,i,j,k) = Source_VC(rhoUz_     ,i,j,k) &
            -State_VGB(rhoUz_,i,j,k,iBlock)*totalLossx &
            -nu_BLK(i,j,k,iBlock)*State_VGB(rhoUz_,i,j,k,iBlock)

       Source_VC(iRhoUzIon_I,i,j,k) = Source_VC(iRhoUzIon_I,i,j,k)&
            -State_VGB(iRhoUzIon_I,i,j,k,iBlock)*Lossx_I &
            -sum(IonNeuRate_II, DIM = 2)*State_VGB(iRhoUzIon_I,i,j,k,iBlock)
       !----- pressure and energy source terms

       if(UseElectronPressure)then
          if(state_VGB(pe_,i,j,k,iBlock)<0.0)then
             write(*,*)'Pe negative at i,j,k,iBlock=',i,j,k,iBlock, &
                  state_VGB(pe_, i,j,k,iBlock)
             call stop_mpi('negative electron pressure')
          end if

          kTi = State_VGB(p_,i,j,k,iBlock)/NumDens
          kTe = State_VGB(pe_,i,j,k,iBlock)/NumDens
          Te_dim = kTe * No2Si_V(UnitTemperature_)

          col_ei = 54.0* NumDens*No2Io_V(UnitN_)/sqrt(Te_dim)/Te_dim &
               /Io2No_V(UnitT_)

          ! Ion Electron collisions Schunk and Nagy
          col_ei_I = 54.0*NumDens_I*No2Io_V(UnitN_)/sqrt(Te_dim)/Te_dim &
               /Io2No_V(UnitT_)

          ! Electron Neutral collision Schunk and Nagy
          if(Te_dim > 5000) Te_dim = 5000
          col_en_I(O_) = 8.9e-11*nDenNuSpecies_CBI(i,j,k,iBlock,O_)&
               *No2Io_V(UnitN_)*(1 + 5.7e-4*Te_dim)*sqrt(Te_dim)&
               /Io2No_V(UnitT_)
          col_en_I(H_) = 4.5e-9*nDenNuSpecies_CBI(i,j,k,iBlock,H_)&
               *No2Io_V(UnitN_)*(1 - 1.35e-4*Te_dim)*sqrt(Te_dim)&
               /Io2No_V(UnitT_)
          col_en_I(CO2_) = 3.68e-8*nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)&
               *No2Io_V(UnitN_)*(1 + 4.1e-11*abs(4500-Te_dim)**(2.93))/&
               Io2No_V(UnitT_)

          col_en_I = col_en_I/MassNeutral_I
          col_en = sum(col_en_I)*meovmi

          AverageMass = sum(State_VGB(iRhoIon_I,i,j,k,iBlock))/NumDens

          ! Ion_neutral collisions dependent terms
          tmp_nu_I = GammaMinus1_I(iFluid)*State_VGB(iRhoIon_I,i,j,k,iBlock)&
               *uu2_I*sum(IonNeuRate_II*ReducedMassNeutral_II,DIM=2) +&
               2*State_VGB(iRhoIon_I,i,j,k,iBlock)*(TNu_body-Temp_I)*&
               sum(IonNeuRate_II*ReducedMassNeutral2_II,DIM= 2)

          Source_VC(iPIon_I,i,j,k) = Source_VC(iPIon_I,i,j,k)  &
               + tmp_nu_I   &
               + SourceNumRho_I*TNu_body               &
               - LossNumRho_I*Temp_I                 &
               + 0.50*(GammaMinus1_I(iFluid))*uu2_I*SiSpecies_I     &
               - 2*col_ei_I*meovmi*NumDens*(Temp_I-kTe)/MassIon_I

          do iFluid=1,nIonFluid
             col_ii_I(iFluid)=sum(2*State_VGB(iRhoIon_I(iFluid),i,j,k,iBlock)&
                  *State_VGB(iRhoIon_I,i,j,k,iBlock)&
                  /(MassIon_I)*(Temp_I-Temp_I(iFluid))*&
                  IonIonCoeff_II(iFluid,:)*ReducedMassIon_II(:,iFluid)&
                  /(Temp_I*No2Si_V(UnitTemperature_))**(3/2))
          end do

          Source_VC(iPIon_I,i,j,k) = Source_VC(iPIon_I,i,j,k) + col_ii_I

          Source_VC(Pe_,i,j,k) = Source_VC(Pe_,i,j,k)  &
               +totalPSNumRho*kTe0         &
               -totalRLNumRhox*NumDens*kTe                  &
               -2*col_en*NumDens*(kTe-TNu_body)   &
               -2*sum(col_ei_I*meovmi*NumDens*(kTe-Temp_I)/MassIon_I)

       else
          tmp = totalSourceNumRho*TNu_body            &
               + NumDens*(TNu_body-KTi)*nu_BLK(i,j,k,iBlock) &
               + totalPSNumRho*kTe0                &
               - totalLossNumRho*kTi               &
               - totalRLNumRhox*NumDens*kTe
          tmp_I = SourceNumRho_I*TNu_body +        &
               2*State_VGB(iRhoIon_I,i,j,k,iBlock)*(TNu_body-Temp_I)*&
               sum(IonNeuRate_II*ReducedMassNeutral2_II,DIM= 2)&
               - LossNumRho_I*Temp_I        &
               + PhoIon_I*kTe0 &
               - RLNumRhox_I*NumDens*kTe

          do iFluid=1,nIonFluid
             col_ii_I(iFluid) = &
                  sum(2*State_VGB(iRhoIon_I(iFluid),i,j,k,iBlock) &
                  *State_VGB(iRhoIon_I,i,j,k,iBlock)&
                  /(MassIon_I)*(Temp_I - Temp_I(iFluid)) &
                  *IonIonCoeff_II(iFluid,:)*ReducedMassIon_II(:,iFluid)&
                  /(Temp_I*No2Si_V(UnitTemperature_))**1.5)
          end do

          tmp_I = tmp_I + col_ii_I

          ! if(DoTestCell)then
          !    write(*,*)NameSub,'SourceNumRho_I=',SourceNumRho_I
          !    write(*,*)NameSub,'NumDens_I=',NumDens_I
          !    write(*,*)NameSub,'PhoIon_I=',PhoIon_I
          !    write(*,*)NameSub,'LossNumRho_I=',LossNumRho_I
          !    write(*,*)NameSub,'RLNumRhox_I=',RLNumRhox_I
          ! end if

          do iFluid = 1, nIonFluid
             Source_VC(nVar+iFluid,i,j,k) = Source_VC(nVar+iFluid,i,j,k)&
                  -0.5*State_VGB(iRhoIon_I(iFluid),i,j,k,iBlock) &
                  *uu2_I(iFluid)*nu_BLK(i,j,k,iBlock) &
                  - 0.50*uu2_I(iFluid)*(LiSpecies_I(iFluid)) &
                  + InvGammaMinus1_I(iFluid)*tmp_I(iFluid)
          end do

          Source_VC(iPIon_I,i,j,k) = Source_VC(iPIon_I,i,j,k) &
               +GammaMinus1_I(iFluid)*State_VGB(iRhoIon_I,i,j,k,iBlock)*uu2_I*&
               sum(IonNeuRate_II*ReducedMassNeutral_II,DIM=2) &
               + 0.5*GammaMinus1_I(iFluid)*uu2_I*SiSpecies_I &
               + tmp_I
       end if

       if(DoTestCell)then
          write(*,*)'After ',NameSub,' Source_VCR(rho_)=',Source_VC(rho_,i,j,k)
       end if

    end do; end do; end do     ! end of the i,j,k loop

    ! end of chemistry

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_calc_sources_impl
  !============================================================================
  subroutine user_init_point_implicit

    use ModPointImplicit, ONLY: iVarPointImpl_I, IsPointImplMatrixSet
    logical:: DoTest
    integer :: iFluid
    character(len=*), parameter:: NameSub = 'user_init_point_implicit'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! All ion momenta are implicit
    if(UseElectronPressure)then
       allocate(iVarPointImpl_I(5*nIonFluid + 1))
    else
       allocate(iVarPointImpl_I(5*nIonFluid))
    end if
    do iFluid = 1, nIonFluid
       iVarPointImpl_I(5*iFluid-4) = iRhoIon_I(iFluid)
       iVarPointImpl_I(5*iFluid-3) = iRhoUxIon_I(iFluid)
       iVarPointImpl_I(5*iFluid-2) = iRhoUyIon_I(iFluid)
       iVarPointImpl_I(5*iFluid-1) = iRhoUzIon_I(iFluid)
       iVarPointImpl_I(5*iFluid)   = iPIon_I(iFluid)
    end do
    if(UseElectronPressure) &
         iVarPointImpl_I(5*nIonFluid + 1)   = Pe_

    IsPointImplMatrixSet = .false.
    ! IsAsymmetric= .false.
    call test_stop(NameSub, DoTest)
  end subroutine user_init_point_implicit
  !============================================================================

  subroutine user_init_session
    use ModMain
    use ModPhysics
    use ModVarIndexes

    integer :: iBoundary
    logical :: DoTest
    character(len=*), parameter:: NameSub = 'user_init_session'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    call set_multiSp_ICs

!    BodyP_I = Ti_body*(BodyRhoIon_I/MassIon_I)
!
!    FaceState_VI(iRhoIon_I,body1_) = BodyRhoIon_I
!    FaceState_VI(iPion_I,body1_)   = BodyP_I
!
!    if(UseElectronPressure)then
!       do iBoundary = xMinBc_, zMaxBc_
!          FaceState_VI(Pe_, iBoundary) = SolarWindP
!          FaceState_VI(P_, iBoundary)  = SolarWindP
!       end do
!       FaceState_VI(P_,body1_) = BodyP_I(1)
!       FaceState_VI(Pe_,body1_) = BodyP_I(1)
!    else
!       FaceState_VI(P_,body1_)=BodyP_I(1)*(1+ElectronPressureRatio)
!    end if
!
!    CellState_VI(:,Coord1MinBc_:Coord3MaxBc_)=FaceState_VI(:,xMinBc_:zMaxBc_)

    if(.not.allocated(nDenNuSpecies_CBI))then
       allocate(nDenNuSpecies_CBI(nI, nJ, nK, MaxBlock, MaxNuSpecies))
       nDenNuSpecies_CBI = -1.0

       allocate(TempNuSpecies_CBI(1:nI, 1:nJ, 1:nK, MaxBlock))
       allocate(Productrate_CB(1:nI, 1:nJ, 1:nK, MaxBlock))
       allocate(Ionizationrate_CBI(1:nI, 1:nJ, 1:nK, MaxBlock,3))
       allocate(MaxSiSpecies_CB(1:nI, 1:nJ, 1:nK, MaxBlock))
       allocate(MaxLiSpecies_CB(1:nI, 1:nJ, 1:nK, MaxBlock))
       allocate(MaxSLSpecies_CB(1:nI, 1:nJ, 1:nK, MaxBlock))
       allocate(nu_BLK(1:nI,1:nJ,1:nK,MaxBlock))
    end if

    call test_stop(NameSub, DoTest)
  end subroutine user_init_session
  !============================================================================
  subroutine user_read_inputs
    use ModMain
    use ModReadParam

    character (len=100) :: NameCommand
    integer:: i, j, k, n, m
    character (len=60):: TGCMFilename
    character (len=60):: DSMCFilename
    character (len=100) :: line

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_read_inputs'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    do
       if(.not.read_line() ) EXIT
       if(.not.read_command(NameCommand)) CYCLE
       select case(NameCommand)

       case('#USERINPUTEND')
          if(iProc==0) write(*,*)'USERINPUTEND'
          EXIT

       case('#USEOLDENERGY')
          call read_var('UseOldEnergy',UseOldEnergy)
          if(.not.UseOldEnergy)then
             call read_var('Te_new_dim',Te_new_dim)
             ! change temperature from ev to k
             Te_new_dim = Te_new_dim * 11610.0
          end if

       case("#UseMarsB0")  ! if or not include crustal magnetic field of Mars
          call read_var('UseMarsB0',UseMarsB0)
          if(UseMarsB0) then
             call read_var('NNm', NNm)
             call read_var('rot',rot)
             call read_var('thetilt', thetilt)
             rot= rot/180.0*3.141592653589793238462643383279
             thetilt= thetilt/180.0*3.141592653589793238462643383279
             cmars = 0.0
             dmars = 0.0
             open(15,file='marsmgsp.txt')
             do i=0,NNm
                read(15,*)n,(cmars(n-1,m),m=0,n-1),(dmars(n-1,m),m=0,n-1)
             end do
             close(15)
          endif

       case("#SOLARCON") ! solar cycle condition
          call read_var('SolarCon',SolarCond)

       case("#UseHotO")  ! adding hot Oxygen or not
          call read_var('UseHotO',UseHotO)
          ! call read_var('CoeffHotO',CoeffHotO)

       case("#UseTempCont") ! add hoc term of the energy source
          call read_var('UseTempCont',UseTempCont)

       case('#REACTIONS')
          call read_var('UseImpactIon',UseImpactIon)
          call read_var('UseChargeEx',UseChargeEx)
          ! open(15,file='read_in.dat')
          ! do i=1,32
          !    read(15,*)Temp_dim(i),Impact_ION_dim(i,Op_),Impact_ION_dim(i,Hp_)
          ! end do
          ! close(15)

       case("#UseMarsAtm")
          call read_var('UseMarsAtm',UseMarsAtm)
          if(UseMarsAtm)then
             call read_var('TGCMFilename',TGCMFilename)
             call read_var('NAlt', NAlt)
             call read_var('NLat', NLat)
             call read_var('NLong', NLong)
             call read_var('subsolarlong', subsolarlong)
             call read_var('subsolarlat', subsolarlat)
             call read_var('longstart', longstart)
             call read_var('latstart', latstart)
             call read_var('altstart', altstart)
             call read_var('dalt', dalt)
             call read_var('dlat', dlat)
             call read_var('dlong', dlong)
             call read_var('NLine', NLine)
             open(15,file=TGCMFilename,status="old")
             do i=1,Nline
                read(15,*)line
             end do
             ! write(*,*)line, NAlt
             do k = 1, NAlt
                do j=1, NLat
                   do i=1, NLong
                      read(15,*)Long_I(i),Lat_I(j),Alt_I(k),Temp(i,j,k),Den_CO2(i,j,k),&
                           Den_O(i,j,k),ICO2p(i,j,k),IOp(i,j,k),IOP2(i,j,k),SZA(i,j,k)
                   end do
                end do
             end do
             close(15)
             !   write(*,*)Long_I(Nlong),Lat_I(NLat),Alt_I(Nalt)
             !   write(*,*)Long_I(1),Lat_I(1),Alt_I(1)
             !   write(*,*)'Den_O(i,j,k),ICO2p(i,j,k),IOp(i,j,k)=',&
             !        Den_O(Nlong,Nlat,Nalt),ICO2p(Nlong,Nlat,Nalt),&
             !        IOp(Nlong,Nlat,Nalt)
          end if

       case("#UseDSMC")
          call read_var('UseDSMC',UseDSMC)
          if(UseDSMC)then
             call read_var('DSMCFilename',DSMCFilename)
             call read_var('NAltD', NaltD)
             call read_var('NLatD', NLatD)
             call read_var('NLongD', NLongD)
             call read_var('NLineD', NLineD)
             open(16,file=DSMCFilename,status="old")
             do i=1,NlineD
                read(16,*)line
             end do
             do k = 1, NAltD
                do j=1, NLatD
                   do i=1, NLongD
                      read(16,*) LatD_I(j),AltD_I(k),LongD_I(i),Den_Oh(i,j,k)
                   end do
                end do
             end do
             close(16)
          end if

       case('#POINTIMPLICITREGION')
          call read_var('rPointImplicit',rPointImplicit)

       case default
          if(iProc==0) call stop_mpi( &
               'read_inputs: unrecognized command: '//NameCommand)
       end select
    end do
    call test_stop(NameSub, DoTest)
  end subroutine user_read_inputs
  !============================================================================

  subroutine DSMC_Input(iBlock)

    use ModMain
    use ModPhysics
    use ModConst
    use ModGeometry, ONLY:r_GB,TypeGeometry
    use BATL_lib, ONLY: CellSize_DB, CoordMin_DB

    integer, intent(in) :: iBlock
    real, parameter :: TINY=1.0E-12
    real :: hh, theta, phi, dR, dtheta, dphi, dH, Hscale, HOh, grav
    real:: xLat, xLong,xAlt
    real:: subsolarlongD, subsolarlatD
    real:: longstartD, latstartD, altstartD
    real:: dlongD, dlatD, daltD
    integer :: i,j,k
    integer:: iAlt,jLong, kLat, ip1,jp1,kp1
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'DSMC_Input'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    !------ Interpolation/Expolation for hot Oxygen density from DSMC _-----
    dR     = CellSize_DB(r_,iBlock)
    dPhi   = CellSize_DB(Phi_,iBlock)
    dTheta = CellSize_DB(Theta_,iBlock)
    subsolarlongD=0
    subsolarlatD=0
    longstartD=0
    latstartD=-87.5
    altstartD=135.0
    dlongD=5.0
    dlatD=5.0
    daltD=50.0

    select case(TypeGeometry)
    case('cartesian')
       call stop_mpi('Unknown geometry type = '//TypeGeometry)

    case('spherical','spherical_lnr')
       if (r_GB(nI,1,1,iBlock) >= Rbody) then
          do k = 1, nK
             ! Latitude coordinate in radians
             Theta = (k - 0.5)*dTheta + CoordMin_DB(Theta_,iBlock)
             ! Convert to degrees
             Theta = cRadToDeg*Theta
             Theta=Theta+subsolarlatD
             kLat=int((Theta-latstartD)/dlatD + 1.0)
             kp1=min(kLat+1, NLatD)
             kLat = max(kLat,1)

             do j = 1, nJ
                ! Longitude coordinate in radians
                Phi = (j-0.5)*dPhi  + CoordMin_DB(Phi_,iBlock)
                ! Convert to degree
                Phi = cRadToDeg*Phi
                jLong=int((phi-longstartD)/dlongD + 1.0)
                jp1=min(jLong+1,NLongD)
                jLong=max(jLong,1)

                do i=1,nI
                   hh = (r_GB(i,j,k,iBlock)-1.00)*3396.00
                   xLong=(Phi-LongD_I(jLong))/dlongD
                   xLat=(Theta-LatD_I(kLat))/dlatD
                   if(hh <= altstartD)then  ! inside the body
                      nDenNuSpecies_CBI(i,j,k,iBlock,Oh_)= &
                           nDenNuSpecies_CBI(i+1,j,k,iBlock,Oh_)
                   elseif(hh <= AltD_I(NAltD))then
                      iAlt=int((hh-altstartD)/daltD+1.0)
                      ip1=min(iAlt+1,NAltD)
                      if(iAlt < 1)then
                         write(*,*)'wrong ialt',iAlt
                      end if
                      xAlt=(hh-AltD_I(iAlt))/daltD
                      nDenNuSpecies_CBI(i,j,k,iBlock,Oh_)=&
                           ((Den_Oh(jLong,kLat,iAlt)*(1-xLong)+xLong*Den_Oh(jp1,kLat,ialt))*(1-xLat)+&
                           (Den_Oh(jLong,kp1,iAlt)*(1-xLong)+xLong*Den_Oh(jp1,kp1,ialt))*xLat)*(1-xAlt)+&
                           ((Den_Oh(jLong,kLat,ip1)*(1-xLong)+xLong*Den_Oh(jp1,kLat,ip1))*(1-xLat)+&
                           (Den_Oh(jLong,kp1,ip1)*(1-xLong)+xLong*Den_Oh(jp1,kp1,ip1))*xLat)*xAlt
                   end if
                end do
             end do
          end do
       end if

    case default
       call stop_mpi('Unknown geometry type = '//TypeGeometry)
    end select

    if(DoTest)then
       write(*,*)'DSMC input end', &
            dR,dPhi,dTheta, iBlock, &
            maxval(nDenNuSpecies_CBI(nI,:,:,iBlock,Oh_)),&
            minval(nDenNuSpecies_CBI(nI,:,:,iBlock,Oh_)),&
            maxval(r_GB(nI,:,:,iBlock)),&
            minval(r_GB(1,:,:,iBlock))
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine DSMC_Input
  !============================================================================

  subroutine Mars_Input(iBlock)

    use ModMain
    use ModPhysics
    use ModConst
    use ModGeometry, ONLY:r_GB,CellSize_DB,&
         Coord111_DB,TypeGeometry

    integer, intent(in) :: iBlock

    real, parameter :: TINY=1.0E-12
    real :: hh, theta, phi, dR, dtheta, dphi, dH, Hscale, HCO2, HO, grav
    real:: tempICO2p, tempIOp, tempIOp2
    real:: xLat, xLong,xAlt
    integer :: i,j,k
    integer:: iAlt, jLong, kLat, ip1,jp1,kp1
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'Mars_Input'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    !------ Interpolation/Expolation for Tn,nCO2,nO,PCO2p,POp,POp2,SZA -----

    dR=CellSize_DB(x_,iBlock)
    dPhi=CellSize_DB(y_,iBlock)
    dTheta=CellSize_DB(z_,iBlock)

    select case(TypeGeometry)
    case('cartesian')
       call stop_mpi('Unknown geometry type = '//TypeGeometry)

    case('spherical','spherical_lnr')
       ! at least part of the block is outside the body
       if (r_GB(nI,1,1,iBlock) >= Rbody) then

          ! write(*,*)'we are in the spherical case of Mars input!'

          do k=1,nK
             Theta = (k-1)*dTheta  + Coord111_DB(Theta_,iBlock)
             Theta = Theta*cRadToDeg ! Convert to degrees
             !! Theta=Theta+subsolarlat
             kLat=int((theta-latstart)/5.0+1.0)
             kp1=min(kLat+1, NLat)
             kLat = max(kLat,1)

             do j=1,nJ
                Phi = (j-1)*dPhi  + Coord111_DB(Phi_,iBlock)
                ! if(phi>cPi)then
                !   phi=phi-2*cPi
                ! end if
                Phi = Phi*cRadToDeg ! Convert to degrees
                !! phi=Mod(phi+subsolarlong,360.0)
                ! phi=Mod(phi+32.5,360.)
                ! jLong=int((phi-2.5)/5.0+1.0)
                jLong=int((phi-longstart)/5.0+1.0)
                jp1=min(jLong+1,NLong)
                jLong=max(jLong,1)

                do i=nI,1,-1
                   hh = (r_GB(i,j,k,iBlock)-1.00)*3396.00
                   !                 write(*,*)'hh=', hh, i,j,k,iBlock
                   xLong=(Phi-Long_I(jLong))/dlong
                   xLat=(Theta-Lat_I(kLat))/dlat
                   if(hh <= 100.0)then  ! inside the body
                      tempNuSpecies_CBI(i,j,k,iBlock)= &
                           tempNuSpecies_CBI(i+1,j,k,iBlock)
                      nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)=&
                           nDenNuSpecies_CBI(i+1,j,k,iBlock,CO2_)
                      nDenNuSpecies_CBI(i,j,k,iBlock,O_)= &
                           nDenNuSpecies_CBI(i+1,j,k,iBlock,O_)

                      !                    tempICO2p=max(tempICO2p,TINY)
                      !                    tempIOP=max(tempIOp,TINY)
                      Ionizationrate_CBI(i,j,k,iBlock,CO2_)=&
                           Ionizationrate_CBI(i+1,j,k,iBlock,CO2_)
                      Ionizationrate_CBI(i,j,k,iBlock,O_)=&
                           Ionizationrate_CBI(i+1,j,k,iBlock,O_)
                      Ionizationrate_CBI(i,j,k,iBlock,Op2_)=&
                           Ionizationrate_CBI(i+1,j,k,iBlock,Op2_)
                   elseif(hh <= Alt_I(NAlt))then
                      iAlt=int((hh -altstart)/dalt+1.0)
                      ip1=min(iAlt+1,NAlt)
                      if(iAlt < 1)then
                         write(*,*)'wrong ialt',iAlt
                      end if
                      xalt=(hh-Alt_I(iAlt))/dalt
                      ! interpolate
                      tempNuSpecies_CBI(i,j,k,iBlock)=          &
                           ((Temp(jLong,kLat,iAlt)*(1-xLong)       &
                           + xLong*Temp(jp1, kLat, ialt))*(1-xLat) &
                           +(Temp(jLong,kp1,iAlt)*(1-xLong)        &
                           + xLong*Temp(jp1, kp1, ialt))*xLat)*(1-xAlt)&
                           +((Temp(jLong,kLat,ip1)*(1-xLong)       &
                           + xLong*Temp(jp1, kLat, ip1))*(1-xLat)   &
                           +(Temp(jLong,kp1,ip1)*(1-xLong)         &
                           + xLong*Temp(jp1, kp1, ip1))*xLat)*xAlt

                      nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)=&
                           ((Den_CO2(jLong,kLat,iAlt)*(1-xLong)+xLong*Den_CO2(jp1, kLat, ialt))*(1-xLat)+&
                           (Den_CO2(jLong,kp1,iAlt)*(1-xLong)+xLong*Den_CO2(jp1, kp1, ialt))*xLat)*(1-xAlt)+&
                           ((Den_CO2(jLong,kLat,ip1)*(1-xLong)+xLong*Den_CO2(jp1, kLat, ip1))*(1-xLat)+&
                           (Den_CO2(jLong,kp1,ip1)*(1-xLong)+xLong*Den_CO2(jp1, kp1, ip1))*xLat)*xAlt

                      nDenNuSpecies_CBI(i,j,k,iBlock,O_)=&
                           ((Den_O(jLong,kLat,iAlt)*(1-xLong)+xLong*Den_O(jp1, kLat, ialt))*(1-xLat)+&
                           (Den_O(jLong,kp1,iAlt)*(1-xLong)+xLong*Den_O(jp1, kp1, ialt))*xLat)*(1-xAlt)+&
                           ((Den_O(jLong,kLat,ip1)*(1-xLong)+xLong*Den_O(jp1, kLat, ip1))*(1-xLat)+&
                           (Den_O(jLong,kp1,ip1)*(1-xLong)+xLong*Den_O(jp1, kp1, ip1))*xLat)*xAlt

                      tempICO2p=&
                           ((ICO2p(jLong,kLat,iAlt)*(1-xLong)+xLong*ICO2p(jp1, kLat, ialt))*(1-xLat)+&
                           (ICO2p(jLong,kp1,iAlt)*(1-xLong)+xLong*ICO2p(jp1, kp1, ialt))*xLat)*(1-xAlt)+&
                           ((ICO2p(jLong,kLat,ip1)*(1-xLong)+xLong*ICO2p(jp1, kLat, ip1))*(1-xLat)+&
                           (ICO2p(jLong,kp1,ip1)*(1-xLong)+xLong*ICO2p(jp1, kp1, ip1))*xLat)*xAlt

                      tempIOP=&
                           ((IOp(jLong,kLat,iAlt)*(1-xLong)+xLong*IOp(jp1, kLat, ialt))*(1-xLat)+&
                           (IOp(jLong,kp1,iAlt)*(1-xLong)+xLong*IOp(jp1, kp1, ialt))*xLat)*(1-xAlt)+&
                           ((IOp(jLong,kLat,ip1)*(1-xLong)+xLong*IOp(jp1, kLat, ip1))*(1-xLat)+&
                           (IOp(jLong,kp1,ip1)*(1-xLong)+xLong*IOp(jp1, kp1, ip1))*xLat)*xAlt

                      tempIOP2=&
                           ((IOp2(jLong,kLat,iAlt)*(1-xLong)+xLong*IOp2(jp1, kLat, ialt))*(1-xLat)+&
                           (IOp2(jLong,kp1,iAlt)*(1-xLong)+xLong*IOp2(jp1, kp1, ialt))*xLat)*(1-xAlt)+&
                           ((IOp2(jLong,kLat,ip1)*(1-xLong)+xLong*IOp2(jp1, kLat, ip1))*(1-xLat)+&
                           (IOp2(jLong,kp1,ip1)*(1-xLong)+xLong*IOp2(jp1, kp1, ip1))*xLat)*xAlt

                      tempICO2p=max(tempICO2p,TINY)
                      tempIOP=max(tempIOp,TINY)
                      tempIOP2=max(tempIOp2,TINY)
                      Ionizationrate_CBI(i,j,k,iBlock,CO2_)=tempICO2p
                      Ionizationrate_CBI(i,j,k,iBlock,O_)=tempIOP
                      Ionizationrate_CBI(i,j,k,iBlock,Op2_)=tempIOP2
                   else  ! hh > Alt_I(NAlt)

                      dH= hh - Alt_I(NAlt)
                      tempNuSpecies_CBI(i,j,k,iBlock)= &
                           (Temp(jLong,kLat,NAlt)*(1-xLong)+xLong*Temp(jp1, kLat,NAlt))*(1-xLat)+&
                           (Temp(jLong,kp1,NAlt)*(1-xLong)+xLong*Temp(jp1,kp1,NAlt))*xLat

                      tempICO2p=&
                           (ICO2p(jLong,kLat,NAlt)*(1-xLong)+xLong*ICO2p(jp1, kLat, NAlt))*(1-xLat)+&
                           (ICO2p(jLong,kp1,NAlt)*(1-xLong)+xLong*ICO2p(jp1, kp1, NAlt))*xLat

                      tempIOP=&
                           (IOp(jLong,kLat,NAlt)*(1-xLong)+xLong*IOp(jp1, kLat, NAlt))*(1-xLat)+&
                           (IOp(jLong,kp1,NAlt)*(1-xLong)+xLong*IOp(jp1, kp1, NAlt))*xLat

                      tempIOP2=&
                           (IOp2(jLong,kLat,NAlt)*(1-xLong)+xLong*IOp2(jp1,kLat,NAlt))*(1-xLat)+&
                           (IOp2(jLong,kp1,NAlt)*(1-xLong)+xLong*IOp2(jp1, kp1,NAlt))*xLat

                      ! grav=3.72/r_GB(i,j,k,iBlock)/r_GB(i,j,k,iBlock)
                      grav=3.72/(1.0+300.0/3396.0)/(1.0+300.0/3396.0)

                      Hscale=cBoltzmann*&
                           tempNuSpecies_CBI(i,j,k,iBlock)/grav/cProtonMass! in m unit

                      HCO2= Hscale/NuMassSpecies_I(CO2_)/1.0e3
                      HO= Hscale/NuMassSpecies_I(O_)/1.0e3

                      nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)=&
                           ((Den_CO2(jLong,kLat,NAlt)*(1-xLong)+xLong*Den_CO2(jp1, kLat,Nalt))*(1-xLat)+&
                           (Den_CO2(jLong,kp1,NAlt)*(1-xLong)+xLong*Den_CO2(jp1,kp1,Nalt))*xLat)&
                           *exp(-dH/HCO2)
                      nDenNuSpecies_CBI(i,j,k,iBlock,O_)=&
                           ((Den_O(jLong,kLat,NAlt)*(1-xLong)+xLong*Den_O(jp1, kLat,Nalt))*(1-xLat)+&
                           (Den_O(jLong,kp1,NAlt)*(1-xLong)+xLong*Den_O(jp1,kp1,Nalt))*xLat)&
                           *exp(-dH/HO)

                      tempICO2p=max(tempICO2p,TINY)
                      tempIOP=max(tempIOp,TINY)
                      tempIOP2=max(tempIOp2,TINY)
                      !                    Ionizationrate_CBI(i,j,k,iBlock,CO2_)=tempICO2p*nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)
                      !                    Ionizationrate_CBI(i,j,k,iBlock,O_)=tempIOP*nDenNuSpecies_CBI(i,j,k,iBlock,O_)
                      Ionizationrate_CBI(i,j,k,iBlock,CO2_)=tempICO2p
                      Ionizationrate_CBI(i,j,k,iBlock,O_)=tempIOP
                      Ionizationrate_CBI(i,j,k,iBlock,Op2_)=tempIOP2

                   end if ! hh < or > 300km
                end do
             end do
          end do
       end if
    case default

       call stop_mpi('Unknown geometry type = '//TypeGeometry)

    end select
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
            minval(Ionizationrate_CBI(nI,:,:,iBlock,CO2_)),&
            maxval(Ionizationrate_CBI(nI,:,:,iBlock,O_)),&
            minval(Ionizationrate_CBI(nI,:,:,iBlock,O_)),&
            maxval(Ionizationrate_CBI(nI,:,:,iBlock,Op2_)),&
            minval(Ionizationrate_CBI(nI,:,:,iBlock,Op2_)),&
            maxval(r_GB(nI,:,:,iBlock)),&
            minval(r_GB(1,:,:,iBlock))
    end if
    call test_stop(NameSub, DoTest, iBlock)
  end subroutine Mars_input
  !============================================================================

  subroutine set_multiSp_ICs
    use ModMain
    use ModConst
    use ModIO
    use ModPhysics

    real :: Productrate0, Optdep
    integer :: iFluid

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_multiSp_ICs'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
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

       ! For issiA and issiC parameters, please refer to: http://www.issibern.ch/teams/martianplasma/

    case ('issiA')
       UseIssiA=.false.
       Alt0=(Rbody-1.0)*3396.
       Tnu_body_dim = -64.56*exp(-0.5*((Alt0-115.7)/20.14)**2)+196.95 ! neutral temperature
       ! increase to 1000k to exobase

       RateDim_I(CO2_hv__CO2p_em_)=2.47e-7
       RateDim_I(CO2_hv__Op_CO_em_)=2.2e-8
       RateDim_I(O_hv__Op_em_) = 8.89e-8
       RateDim_I(H_hv__Hp_em_) = 5.58e-8
       RateDim_I(CO2p_em__CO_O_) = 3.5e-7  !*sqrt(300/Te_dim)
       RateDim_I(O2p_em__O_O_) = 1.0e-7   !*exp(0.7*log(300/Te_dim))
       ! for Te<1200k
       ! RateDim_I(Hp_O__Op_H_) = 1.0e-15

       BodynDenNuSpDim_I(CO2_) = 6.04e18*exp(-Alt0/6.98) !/cc
       BodynDenNuSpDim_I(CO2x_)= 1.67e15*exp(-Alt0/11.49)
       BodynDenNuSpDim_I(O_)   = 5.85e13*exp(-Alt0/10.56)
       BodynDenNuSpDim_I(Ox_)  = 7.02e9 *exp(-Alt0/33.97) !

       BodynDenNuSpDim_I(H_)= 184383
       BodynDenNuSpDim_I(Hx_)= 20635.3

       BodynDenNuSpDim_I(Oh_)= 5.23e3*exp(-Alt0/626.2)
       BodynDenNuSpDim_I(Ohx_)=9.76e2*exp(-Alt0/2790)
       BodynDenNuSpDim_I(Oh2x_)=3.71e4*exp(-Alt0/88.47)

       HNuSpeciesDim_I(CO2_)=6.98
       HNuSpeciesDim_I(CO2x_)=11.49
       HNuSpeciesDim_I(O_)=10.56  ! scale height in km
       HNuSpeciesDim_I(Ox_)=33.97
       HNuSpeciesDim_I(H_)=100.
       HNuSpeciesDim_I(Hx_)=100.0
       HNuSpeciesDim_I(Oh_)=626.2
       HNuSpeciesDim_I(Ohx_)=2790.0
       HNuSpeciesDim_I(Oh2x_)=88.47

    case('issiC')
       UseIssiC=.true.
       Alt0=(Rbody-1.0)*3396.
       Tnu_body_dim = -161.13*exp(-0.5*((Alt0-112.6)/25.25)**2)+291.78 ! neutral temperature
       ! increase to 1000k to exobase

       RateDim_I(CO2_hv__CO2p_em_)=7.3e-7
       RateDim_I(CO2_hv__Op_CO_em_)=7.4e-8
       RateDim_I(O_hv__Op_em_) = 2.73e-7
       RateDim_I(H_hv__Hp_em_) = 8.59e-8
       RateDim_I(CO2p_em__CO_O_) = 3.5e-7  !*sqrt(300/Te_dim)
       RateDim_I(O2p_em__O_O_) = 1.0e-7   !*exp(0.7*log(300/Te_dim))
       ! for Te<1200k
       ! RateDim_I(Hp_O__Op_H_) = 1.0e-15

       BodynDenNuSpDim_I(CO2_) = 5.88e18*exp(-Alt0/7.00) !/cc
       BodynDenNuSpDim_I(CO2x_)= 3.55e13*exp(-Alt0/16.67)
       BodynDenNuSpDim_I(O_)   = 5.85e13*exp(-Alt0/10.56)
       BodynDenNuSpDim_I(Ox_)  = 7.02e09 *exp(-Alt0/33.97) !

       BodynDenNuSpDim_I(H_)= 1.58484e6
       BodynDenNuSpDim_I(Hx_)= 33753.7

       BodynDenNuSpDim_I(Oh_)= 1.56e4*exp(-Alt0/696.9)
       BodynDenNuSpDim_I(Ohx_)=2.92e3*exp(-Alt0/2891.0)
       BodynDenNuSpDim_I(Oh2x_)=5.01e4*exp(-Alt0/99.19)

       HNuSpeciesDim_I(CO2_)=7.00
       HNuSpeciesDim_I(CO2x_)=16.67
       HNuSpeciesDim_I(O_)=12.27  ! scale height in km
       HNuSpeciesDim_I(Ox_)=48.57
       HNuSpeciesDim_I(H_)=100.
       HNuSpeciesDim_I(Hx_)=100.0
       HNuSpeciesDim_I(Oh_)=696.9
       HNuSpeciesDim_I(Ohx_)=2891.0
       HNuSpeciesDim_I(Oh2x_)=99.19

    case('solarmin')

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

       RateDim_I(CO2_hv__CO2p_em_)=2.47e-7
       RateDim_I(O_hv__Op_em_) = 8.89e-8
       RateDim_I(H_hv__Hp_em_) = 5.58e-8

    case default
       call stop_mpi('unknown solar condition '//SolarCond)
    end select

    Ti_body_dim = Tnu_body_dim  ! ion temperature at the body

    TNu_body= TNu_body_dim*Si2No_V(UnitTemperature_)
    Ti_body = Ti_body_dim*Si2No_V(UnitTemperature_)
    T300 = T300_dim*Si2No_V(UnitTemperature_)

    kTe0=max(Te_new_dim, Tnu_body_dim)*Si2No_V(UnitTemperature_)

    if(DoTest)then
       write(*,*)'Tnu_body=',Tnu_body, TNu_body_dim
       write(*,*)'T300=', T300, T300_dim
    end if

    nu0=nu0_dim*No2Io_V(UnitN_)*No2Io_V(UnitT_)
    BodynDenNuSpecies_I=&
         BodynDenNuSpDim_I*1e6*Si2No_V(UnitN_)
    HNuSpecies_I=&
         HNuSpeciesDim_I*1.0e3*Si2No_V(UnitX_)

    ! normlize the reaction rate
    Rate_I(CO2_hv__CO2p_em_)= &
         Ratedim_I(CO2_hv__CO2p_em_)*No2Io_V(UnitT_)
    Rate_I(CO2_hv__Op_CO_em_)= &
         Ratedim_I(CO2_hv__Op_CO_em_)*No2Io_V(UnitT_)
    Rate_I(O_hv__Op_em_)=  &
         Ratedim_I(O_hv__Op_em_)*No2Io_V(UnitT_)
    Rate_I(CO2p_O__O2p_CO_)=  &
         Ratedim_I(CO2p_O__O2p_CO_)  &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)
    Rate_I(Op_CO2__O2p_CO_)=  &
         Ratedim_I(Op_CO2__O2p_CO_)*(8.0/3.0*T300/Tnu_body)**0.39 &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)
    Rate_I(CO2p_O__Op_CO2_)=  &
         Ratedim_I(CO2p_O__Op_CO2_) &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)

    Rate_I(O2p_em__O_O_)=  &
         Ratedim_I(O2p_em__O_O_)*(4.0*T300/TNu_body)**0.56 &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)
    Rate_I(CO2p_em__CO_O_)=  &
         Ratedim_I(CO2p_em__CO_O_)*sqrt(T300/TNu_body)&
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)

    Rate_I(H_hv__Hp_em_)=  &
         Ratedim_I(H_hv__Hp_em_)*No2Io_V(UnitT_)
    Rate_I(Hp_O__Op_H_)=  &
         Ratedim_I(Hp_O__Op_H_) &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)
    Rate_I(Op_H__Hp_O_)=  &
         Ratedim_I(Op_H__Hp_O_) &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)

    ReactionRate_I(CO2_hv__CO2p_em_)= &
         Rate_I(CO2_hv__CO2p_em_)*BodynDenNuSpecies_I(CO2_)
    PhoIon_I(CO2p_)=ReactionRate_I(CO2_hv__CO2p_em_)

    ReactionRate_I(O_hv__Op_em_)= &
         Rate_I(O_hv__Op_em_)*BodynDenNuSpecies_I(O_)
    PhoIon_I(Op_)=ReactionRate_I(O_hv__Op_em_)

    ! charge exchange
    ReactionRate_I(CO2p_O__O2p_CO_)= &
         Rate_I(CO2p_O__O2p_CO_)* BodynDenNuSpecies_I(O_)
    CoeffSpecies_II(O2p_,CO2p_)=ReactionRate_I(CO2p_O__O2p_CO_)

    ReactionRate_I(Op_CO2__O2p_CO_)= &
         Rate_I(Op_CO2__O2p_CO_)* BodynDenNuSpecies_I(CO2_)
    CoeffSpecies_II(O2p_, Op_)=ReactionRate_I(Op_CO2__O2p_CO_)

    ReactionRate_I(CO2p_O__Op_CO2_)= &
         Rate_I(CO2p_O__Op_CO2_)* BodynDenNuSpecies_I(O_)
    CoeffSpecies_II(Op_,CO2p_)=ReactionRate_I(CO2p_O__Op_CO2_)

    ! Convert cm^2 into normalized units but take into account that
    ! the cross-section will be multiplied scale height (UnitX_) and
    ! number density (UnitN_) and these normalized units are not compatible
    CrossSection_I = CrossSectiondim_I*1.0e-4 / &
         ( Si2No_V(UnitX_)*Si2No_V(UnitN_) )

    Optdep = sum(BodynDenNuSpecies_I*CrossSection_I*HNuSpecies_I)
    Productrate0 = max(exp(-Optdep), 1.0e-5)

    !    do n=1,nIonFluid
    !    write(*,*)'ionFluid=',n,'IonNeuCoeffDim_II=',IonNeuCoeffDim_II(n,:)
    !    end do
    IonNeuCoeff_II = IonNeuCoeffDim_II*No2Io_V(UnitN_)*No2Io_V(UnitT_)*1.e-10

    IonIonCoeff_II = IonIonCoeffDim_II*No2Io_V(UnitN_)*No2Io_V(UnitT_)

    if(DoTest)then
       write(*,*)'=======in set_multisp=============='
       write(*,*)'BodynDenNuSpecies_I=',BodynDenNuSpecies_I
       write(*,*)'HNuSpecies_I=',HNuSpecies_I
       write(*,*)'solar min, Procductrate=', productrate0, Optdep
       write(*,*)'CrossSection_dim_I*unitUSER_n*unitSI_x=',CrossSectiondim_I,&
            No2Io_V(unitN_),No2Si_V(unitX_)
       write(*,*)''
    end if

    ! ion density at the body
    BodyRhoIon_I(Hp_)=SolarWindRho*0.3

    BodyRhoIon_I(CO2p_)= Rate_I(CO2_hv__CO2p_em_)*Productrate0*&
         BodynDenNuSpecies_I(CO2_)/BodynDenNuSpecies_I(O_)/&
         (Rate_I(CO2p_O__O2p_CO_)+Rate_I(CO2p_O__Op_CO2_))
    ! BodyRhoIon_I(Op_)= (Rate_I(O_hv__Op_em_)*Productrate0+&
    !     Rate_I(CO2p_O__Op_CO2_)*BodyRhoIon_I(CO2p_))&
    !     *BodynDenNuSpecies_I(O_)/(BodynDenNuSpecies_I(CO2_)+3.0e5)/&
    !     Rate_I(Op_CO2__O2p_CO_)
    BodyRhoIon_I(Op_)= ((Rate_I(O_hv__Op_em_)*Productrate0+&
         Rate_I(CO2p_O__Op_CO2_)*BodyRhoIon_I(CO2p_))&
         *BodynDenNuSpecies_I(O_)+Rate_I(CO2_hv__Op_CO_em_)*&
         BodynDenNuSpecies_I(CO2_))/(BodynDenNuSpecies_I(CO2_)+3.0e5)/&
         Rate_I(Op_CO2__O2p_CO_)
    BodyRhoIon_I(O2p_)= SQRT((BodynDenNuSpecies_I(O_)*&
         BodyRhoIon_I(CO2p_)*Rate_I(CO2p_O__O2p_CO_)+ &
         BodynDenNuSpecies_I(CO2_)*BodyRhoIon_I(Op_)*&
         Rate_I(Op_CO2__O2p_CO_))/Rate_I(O2p_em__O_O_))
    BodyRhoIon_I = BodyRhoIon_I*MassIon_I

    do iFluid=1,nIonFluid
       ReducedMassNeutral2_II(iFluid,:)=1/&
            (MassIon_I(iFluid) + MassNeutral_I)
       ReducedMassNeutral_II(iFluid,:)=MassNeutral_I/&
            (MassIon_I(iFluid) + MassNeutral_I)
       ReducedMassIon_II(iFluid,:)=1/&
            (MassIon_I(iFluid) + MassIon_I)
    end do

    if(DoTest)then
       write(*,*)' set parameters of Mars: BodyRhoIon_I(i)=',&
            BodyRhoIon_I(1:nIonFluid)
       write(*,*)'neutral density=', &
            BodynDenNuSpecies_I
       write(*,*)'nu0=',nu0
       write(*,*)'Rate_I=', Rate_I
       write(*,*)'Rate_dim_I=', Ratedim_I
    end if

    call test_stop(NameSub, DoTest)
  end subroutine set_multiSp_ICs
  !============================================================================
  subroutine user_set_ics(iBlock)

    ! This subroutine allows the user to apply initial conditions to the domain
    ! which are problem specific and cannot be created using the predefined
    ! options in BATSRUS.

    use ModMain
    use ModAdvance
    use ModGeometry, ONLY: r_GB
    use BATL_lib, ONLY: Xyz_DGB, Used_GB
    use ModPhysics
    use ModNumConst

    integer, intent(in) :: iBlock

    real :: CosSZA, OptDep, Ne, p, RhoMin
    integer :: i, j, k, iFluid

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_ics'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    call set_neutral_density(iBlock)

    if(DoTest)then
       write(*,*)'in set_ics'
       write(*,*)'BodynDenNuSpecies_I=',&
            BodynDenNuSpecies_I
       WRITE(*,*)''
       write(*,*)'HNuSpecies_I(1:nNuSpecies)=',HNuSpecies_I
       WRITE(*,*)''
       write(*,*)'Rbody=', Rbody
       write(*,*)''
    end if

    ! calculate optical depth and producation rate
    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       cosSZA = (0.5+sign(0.5,Xyz_DGB(x_,i,j,k,iBlock)))*&
            Xyz_DGB(x_,i,j,k,iBlock)/max(r_GB(i,j,k,iBlock),1e-3) + 5e-4

       OptDep = max( sum(nDenNuSpecies_CBI(i,j,k,iBlock,1:MaxNuSpecies)*&
            CrossSection_I(1:MaxNuSpecies)*HNuSpecies_I(1:MaxNuSpecies)),&
            6.0e-3)/cosSZA
       if( OptDep < 11.5 .and. Xyz_DGB(x_,i,j,k,iBlock) > 0.0) then
          Productrate_CB(i,j,k,iBlock) = max(exp(-OptDep), 1e-5)
       else
          Productrate_CB(i,j,k,iBlock) = 1e-5
       end if

    end do; end do; end do

    do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
       if (r_GB(i,j,k,iBlock)< Rbody) then
          cosSZA = (0.5+sign(0.5,Xyz_DGB(x_,i,j,k,iBlock)))*&
               Xyz_DGB(x_,i,j,k,iBlock)/max(r_GB(i,j,k,iBlock), 1e-3) + 1e-3
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
          State_VGB(OpRho_,i,j,k,iBlock)= &
               FaceState_VI(OpRho_,body1_)*cosSZA
          State_VGB(O2pRho_,i,j,k,iBlock)= &
               FaceState_VI(O2pRho_,body1_)*sqrt(cosSZA)
          State_VGB(CO2pRho_,i,j,k,iBlock)= &
               FaceState_VI(CO2pRho_,body1_)*cosSZA
          State_VGB(iPIon_I,i,j,k,iBlock) = &
               Ti_body*State_VGB(iRhoIon_I,i,j,k,iBlock)/MassIon_I
       else
          State_VGB(:,i,j,k,iBlock) = CellState_VI(:,Coord1MinBc_)
          State_VGB(Ux_:bz_,i,j,k,iBlock) = 0.0
       end if
    end do;end do; end do;

    do k=1,nK; do j=1,nJ; do i=1,nI
       State_VGB(iRhoUx_I,i,j,k,iBlock) = 0.0
       State_VGB(iRhoUy_I,i,j,k,iBlock) = 0.0
       State_VGB(iRhoUz_I,i,j,k,iBlock) = 0.0

       if (.not. (Used_GB(i,j,k,iBlock).and. &
            r_GB(i,j,k,iBlock)<1.5*Rbody) ) CYCLE

       cosSZA=(0.5+sign(0.5,Xyz_DGB(x_,i,j,k,iBlock)))*&
            Xyz_DGB(x_,i,j,k,iBlock)/max(r_GB(i,j,k,iBlock),1.0e-3)+&
            1.0e-3

       State_VGB(CO2pRho_,i,j,k,iBlock)= &
            Ionizationrate_CBI(i,j,k,iBlock,CO2_) &
            /nDenNuSpecies_CBI(i,j,k,iBlock,O_)   &
            /(Rate_I(CO2p_O__O2p_CO_)+Rate_I(CO2p_O__Op_CO2_))

       ! write(*,*)'State_VGB(CO2pRho_,i,j,k,iBlock)',State_VGB(CO2pRho_,i,j,k,iBlock)

       State_VGB(OpRho_,i,j,k,iBlock)= &
            (Ionizationrate_CBI(i,j,k,iBlock,O_) &
            +Rate_I(CO2p_O__Op_CO2_)                &
            *State_VGB(CO2pRho_,i,j,k,iBlock)    &
            *nDenNuSpecies_CBI(i,j,k,iBlock,O_)) &
            /(nDenNuSpecies_CBI(i,j,k,iBlock, CO2_)+4.0e6)&
            /Rate_I(Op_CO2__O2p_CO_)

       ! write(*,*)'State_VGB(OpRho_,i,j,k,iBlock)',State_VGB(OpRho_,i,j,k,iBlock)

       State_VGB(O2pRho_,i,j,k,iBlock)= &
            SQRT((nDenNuSpecies_CBI(i,j,k,iBlock,O_)*&
            State_VGB(CO2pRho_,i,j,k,iBlock)*&
            Rate_I(CO2p_O__O2p_CO_)+&
            nDenNuSpecies_CBI(i,j,k,iBlock, CO2_)*&
            State_VGB(OpRho_,i,j,k,iBlock)*&
            Rate_I(Op_CO2__O2p_CO_)+1e-10)/Rate_I(O2p_em__O_O_))

       ! write(*,*)'State_VGB(O2pRho_,i,j,k,iBlock)',State_VGB(O2pRho_,i,j,k,iBlock)

       ! Convert to mass densities
       State_VGB(iRho_I,i,j,k,iBlock) = &
            State_VGB(iRho_I,i,j,k,iBlock)*MassIon_I

    end do; end do; end do

    !

    do k=1,nK; do j=1,nJ; do i=1,nI

       if(.not.Used_GB(i,j,k,iBlock))CYCLE

       ! IC for velocity
       State_VGB(iRhoUx_I,i,j,k,iBlock) = 0.0
       State_VGB(iRhoUy_I,i,j,k,iBlock) = 0.0
       State_VGB(iRhoUz_I,i,j,k,iBlock) = 0.0

       ! Total density
       RhoMin = LowDensityRatio*sum(State_VGB(iRhoIon_I,i,j,k,iBlock))
       State_VGB(iRhoIon_I,i,j,k,iBlock) = &
            max(RhoMin, State_VGB(iRhoIon_I,i,j,k,iBlock))

       Ne = sum(State_VGB(iRhoIon_I,i,j,k,iBlock)/(MassIon_I))
       if(.not. UseElectronPressure)then
          p = max(SolarWindP, Ne*Ti_body*(1+ElectronPressureRatio))
          State_VGB(iPIon_I,i,j,k,iBlock) = &
               p/Ne/(1 + ElectronPressureRatio) &
               *State_VGB(iRhoIon_I,i,j,k,iBlock)/MassIon_I
       else
          p = max(SolarWindP, Ne*Ti_body)
          State_VGB(iPIon_I,i,j,k,iBlock) = &
               p/Ne * State_VGB(iRhoIon_I,i,j,k,iBlock)/MassIon_I
          State_VGB(Pe_,i,j,k,iBlock) = State_VGB(P_,i,j,k,iBlock)
       end if

    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_ICs
  !============================================================================
  subroutine user_set_face_boundary(FBC)

    use ModMain, ONLY: UseRotatingBc, &
          ExtraBc_, Body1_, xMinBc_, FaceBCType
    use ModVarIndexes, ONLY: &
         nVar, OpRho_, O2pRho_, CO2pRho_, HpRho_,iRhoUx_I,iRhoUy_I,iRhoUz_I
    use ModPhysics, ONLY: SolarWindRho, ElectronPressureRatio, FaceState_VI, &
         OmegaBody_D
    use ModCoordTransform, ONLY: cross_product

    type(FaceBCType), intent(inout):: FBC

    real:: XFace,YFace,ZFace,rFace,rFace2
    real:: uRot_D(3)
    real:: cosSZA
    real:: uDotR_I(nFluid)
    logical:: DoTestCell
    character(len=*), parameter:: NameSub = 'user_set_face_boundary'
    !--------------------------------------------------------------------------
    associate( VarsGhostFace_V => FBC%VarsGhostFace_V, &
               VarsTrueFace_V => FBC%VarsTrueFace_V, &
               FaceCoords_D => FBC%FaceCoords_D, &
               iBlockBc => FBC%iBlockBc, iBoundary => FBC%iBoundary, &
               iFace => FBC%iFace, jFace => FBC%jFace, kFace => FBC%kFace )

    if(iBoundary == ExtraBc_)then
       VarsGhostFace_V = FaceState_VI(:,xMinBc_)
       RETURN
    elseif(iBoundary /= Body1_)then
       call stop_mpi(NameSub//' invalid iBoundary value')
    end if

    call test_start(NameSub, DoTestCell, iBlockBc, iFace, jFace, kFace)

    XFace = FaceCoords_D(1)
    YFace = FaceCoords_D(2)
    ZFace = FaceCoords_D(3)

    rFace2 = XFace**2 + YFace**2 + ZFace**2
    rFace  = sqrt(rFace2)

    ! Apply boundary conditions
    cosSZA=(0.5+sign(0.5,XFace)) * XFace/max(RFace,1.0e-3) + 1.0e-3

    VarsGhostFace_V(OpRho_)  = BodyRhoIon_I(Op_) *cosSZA

    VarsGhostFace_V(O2pRho_) = BodyRhoIon_I(O2p_)*sqrt(cosSZA)

    VarsGhostFace_V(CO2pRho_)= BodyRhoIon_I(CO2p_)*cosSZA

    VarsGhostFace_V(HpRho_)  = SolarWindRho*0.3
    VarsGhostFace_V(rho_) = sum(VarsGhostFace_V(iRhoIon_I))

    VarsGhostFace_V(iPIon_I)=Ti_body*VarsGhostFace_V(iRhoIon_I)/MassIon_I

    if(UseElectronPressure)then
       VarsGhostFace_V(P_)=sum(VarsGhostFace_V(iPIon_I))
       VarsGhostFace_V(Pe_)=sum(VarsGhostFace_V(iPIon_I))
    else
       VarsGhostFace_V(P_)=sum(VarsGhostFace_V(iPIon_I))*(1+ElectronPressureRatio)
    end if

    ! Reflective in radial direction
    uDotR_I = (VarsTrueFace_V(iRhoUx_I)*FaceCoords_D(1)+ &
         VarsTrueFace_V(iRhoUy_I)*FaceCoords_D(2)+ &
         VarsTrueFace_V(iRhoUz_I)*FaceCoords_D(3))/ rFace2

    ! bDotR = sum(VarsTrueFace_V(Bx_:Bz_)*FaceCoords_D)/rFace2

    VarsGhostFace_V(iRhoUx_I) = VarsTrueFace_V(iRhoUx_I) - 2*uDotR_I*FaceCoords_D(1)
    VarsGhostFace_V(iRhoUy_I) = VarsTrueFace_V(iRhoUy_I) - 2*uDotR_I*FaceCoords_D(2)
    VarsGhostFace_V(iRhoUz_I) = VarsTrueFace_V(iRhoUz_I) - 2*uDotR_I*FaceCoords_D(3)

    ! VarsGhostFace_V(Bx_:Bz_) = VarsTrueFace_V(Bx_:Bz_) - 2*bDotR*FaceCoords_D
    VarsGhostFace_V(Bx_:Bz_) = 0.0

    if(DoTestCell) then
       write(*,*)'VarsGhostFace_V(iRhoIon_I)=',VarsGhostFace_V(iRhoIon_I)
       write(*,*)'VarsGhostFace_V(iRhoUx_I(1))=',VarsGhostFace_V(iRhoUx_I(1))
       write(*,*)'VarsGhostFace_V(iRhoUy_I(1))=',VarsGhostFace_V(iRhoUy_I(1))
       write(*,*)'VarsGhostFace_V(iRhoUz_I(1))=',VarsGhostFace_V(iRhoUz_I(1))
       write(*,*)'VarsGhostFace_V(P_)=',VarsGhostFace_V(P_)
       write(*,*)'VarsGhostFace_V(Bx_ : Bz_)=',VarsGhostFace_V(Bx_: Bz_)
    end if

    ! Apply corotation?
    if (UseRotatingBc) then
       uRot_D = cross_product(OmegaBody_D, FaceCoords_D)
       VarsGhostFace_V(iRhoUx_I) = VarsGhostFace_V(iRhoUx_I) + 2*uRot_D(1)
       VarsGhostFace_V(iRhoUy_I) = VarsGhostFace_V(iRhoUy_I) + 2*uRot_D(2)
       VarsGhostFace_V(iRhoUz_I) = VarsGhostFace_V(iRhoUz_I) + 2*uRot_D(3)
    end if

    call test_stop(NameSub, DoTestCell, iBlockBc, iFace, jFace, kFace)

    end associate
  end subroutine user_set_face_boundary
  !============================================================================

  subroutine user_set_boundary_cells(iBlock)

    use ModGeometry,      ONLY: ExtraBc_, Xyz_DGB, xMaxBox
    use ModBoundaryGeometry, ONLY: iBoundary_GB

    integer, intent(in):: iBlock

    character (len=*), parameter :: Name='user_set_boundary_cells'
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_boundary_cells'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    where(Xyz_DGB(x_,:,:,:,iBlock) > xMaxBox) &
         iBoundary_GB(:,:,:,iBlock) = ExtraBc_

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_boundary_cells
  !============================================================================

  subroutine user_get_log_var(VarValue, NameVar, Radius)

    use ModGeometry,        ONLY: Xyz_DGB, r_GB
    use ModAdvance,         ONLY: State_VGB, Tmp1_GB
    use ModPhysics,         ONLY: No2Si_V, UnitN_, UnitX_, UnitU_
    use ModWriteLogSatFile, ONLY: calc_sphere
    use BATL_lib,           ONLY: Unused_B

    real, intent(out)            :: VarValue
    character (len=*), intent(in):: NameVar
    real, intent(in), optional :: Radius

    integer:: i, j, k, iBlock, iIonFluid, iSide
    integer:: iRho, iRhoUx, iRhoUz

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_get_log_var'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*)NameSub, ': NameVar=',NameVar

    iSide = 0

    select case(NameVar)
    case('hplflx')
       iIonFluid = Hp_
       iSide     = -1
    case('hprflx')
       iIonFluid = Hp_
       iSide     = +1
    case('hpflx')
       iIonFluid = Hp_
    case('oplflx')
       iIonFluid = Op_
       iSide     = -1
    case('oprflx')
       iIonFluid = Op_
       iSide     = +1
    case('opflx')
       iIonFluid = Op_
    case('o2plflx')
       iIonFluid = O2p_
       iSide     = -1
    case('o2prflx')
       iIonFluid = O2p_
       iSide     = +1
    case('o2pflx')
       iIonFluid = O2p_
    case('co2plflx')
       iIonFluid = CO2p_
       iSide     = -1
    case('co2prflx')
       iIonFluid = CO2p_
       iSide     = +1
    case('co2pflx')
       iIonFluid = CO2p_
    case default
       call stop_mpi(NameSub//': wrong NameVar='//NameVar)
    end select

    iRho   = iRhoIon_I(iIonFluid)
    iRhoUx = iRhoUxIon_I(iIonFluid)
    iRhoUz = iRhoUzIon_I(iIonFluid)

    do iBlock = 1, nBlock
       if (Unused_B(iBlock)) CYCLE
       do k=0,nK+1; do j=0,nJ+1; do i=0,nI+1
          Tmp1_GB(i,j,k,iBlock) = State_VGB(HpRho_,i,j,k,iBlock)* &
               sum(State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock) &
               *Xyz_DGB(:,i,j,k,iBlock)) &
               /(r_GB(i,j,k,iBlock)*State_VGB(iRho,i,j,k,iBlock))

          ! Exclude left side if iSide=1 or right side if iSide=-1
          if(iSide /= 0)then
             if( iSide*Xyz_DGB(x_,i,j,k,iBlock) < 0) Tmp1_GB(i,j,k,iBlock) = 0
          end if
       end do; end do; end do
    end do
    VarValue = calc_sphere('integrate', 360, Radius, Tmp1_GB) &
         /MassIon_I(iIonFluid) &
         *No2Si_V(UnitN_)*No2Si_V(UnitX_)**2*No2Si_V(UnitU_)

    call test_stop(NameSub, DoTest)
  end subroutine user_get_log_var
  !============================================================================

  subroutine user_set_plot_var(iBlock, NameVar, IsDimensional, &
       PlotVar_G, PlotVarBody, UsePlotVarBody, &
       NameTecVar, NameTecUnit, NameIdlUnit, IsFound)

    use ModPhysics, ONLY: No2Io_V, BodyRho_I,UnitN_
    use ModSize, ONLY: nI, nJ, nK
    use ModMultiFluid

    integer,          intent(in)   :: iBlock
    character(len=*), intent(in)   :: NameVar
    logical,          intent(in)   :: IsDimensional
    real,             intent(inout):: PlotVar_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)
    real,             intent(out)  :: PlotVarBody
    logical,          intent(out)  :: UsePlotVarBody
    character(len=*), intent(inout):: NameTecVar
    character(len=*), intent(inout):: NameTecUnit
    character(len=*), intent(inout):: NameIdlUnit
    logical,          intent(out)  :: IsFound

    integer :: iVar, iIon

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_plot_var'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    IsFound=.true.

    select case(NameVar)
       !  case('hp')
       !     iVar=HpRho_
       !     iIon=1
       !  case('o2p')
       !     iVar=O2pRho_
       !     iIon=2
       !  case('op')
       !     iVar=OpRho_
       !     iIon=3
       !  case('co2p')
       !     iVar=CO2pRho_
       !     iIon=4

    case('co2')
       iVar=CO2_
    case('o')
       iVar=O_
    case('h')
       iVar=H_
    case('oh')
       iVar=Oh_
    case('ohx')
       iVar=Ohx_
    case('hx')
       iVar=Hx_
    case('ox')
       iVar=Ox_
    case('co2x')
       iVar=CO2x_

    case('rateo')
       iVar=O_
    case('rateco2')
       iVar=CO2_

    case default
       IsFound= .false.
       call stop_mpi(NameSub//': unimplemented variable='//NameVar)
    end select
    ! NameTecUnit = '[amu/cm3]'
    ! NameIdlUnit = 'amu/cm3'

    NameTecUnit = '[cm-3.s-1]'
    NameIdlUnit = 'cm-3.s-1'
    ! PlotVar_G(1:nI,1:nJ,1:nK)=Ionizationrate_CBI(:,:,:,iBlock,iVar)/No2Io_V(UnitT_)*No2Io_V(UnitN_)
    PlotVar_G(1:nI,1:nJ,1:nK)= nDenNuSpecies_CBI(:,:,:,iBlock,iVar)*No2Io_V(UnitN_)
    !  PlotVar_G   = State_VGB(iVar,:,:,:,iBlock)/MassIon_I(iIon)

    PlotVarBody = BodyRho_I(iIon+1)

    ! if(IsDimensional) PlotVar_G = PlotVar_G*No2Io_V(UnitRho_)

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_plot_var
  !============================================================================
  subroutine set_neutral_density(iBlock)

    use ModMain
    use ModAdvance
    use ModGeometry, ONLY: Xyz_DGB, r_GB
    use ModPhysics
    use ModNumConst

    integer, intent(in):: iBlock

    real :: CosSZA, OptDep
    integer:: i, j, k
    logical:: DoTestCell
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_neutral_density'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! calculate neutral

    do k=1,nK; do j=1,nJ; do i=1,nI

       DoTestCell= DoTest .and. i==iTest .and. j==jTest .and. k==kTest

       ! if(DoTestCell)then
       !   write(*,*)'iProc, iBlock, iTest, jTest, kTest=',iProc, iBlock, i, j, k
       !   write(*,*)'beginning of set_neutral_density'
       !   write(*,*)'nDenNuSpecies_CBI=',nDenNuSpecies_CBI(iTest,jTest,kTest,iBlock,:)
       !   write(*,*)'Productrate_CBI=',Productrate_CB(iTest,jTest,kTest,iBlock)
       !   write(*,*)'Ionizationrate_CBI=',Ionizationrate_CBI(iTest,jTest,kTest,iBlock,:)
       ! write(*,*)' BodynDenNuSpecies_I=', BodynDenNuSpecies_I
       ! end if

       if(r_GB(i,j,k,iBlock)<= Rbody)then
          nDenNuSpecies_CBI(i,j,k,iBlock,:)=&
               BodynDenNuSpecies_I

       else if(r_GB(i,j,k,iBlock)< 5.0*Rbody) then
          nDenNuSpecies_CBI(i,j,k,iBlock,:)=&
               BodynDenNuSpecies_I*&
               exp(-(r_GB(i,j,k,iBlock)-Rbody)&
               /HNuSpecies_I)

          ! putting the ISSI parameters

          !    if(UseIssiA) then

          ! nDenNuSpecies_CBI(i,j,k,iBlock,H_)=&
          !      1.5e5*&
          !      exp(25965*(1/(r_GB(i,j,k,iBlock)*&
          !      0.001*No2Si_V(UnitX_))))/&
          !      exp((25965.0)/(3595))

          !  nDenNuSpecies_CBI(i,j,k,iBlock,O_)=&
          !       (5.85e13*&
          !      exp(-((r_GB(i,j,k,iBlock)-Rbody)*0.001*No2Si_V(UnitX_)+100)/10.56))+&
          !    (7.02e9*&
          !      exp(-((r_GB(i,j,k,iBlock)-Rbody)*0.001*No2Si_V(UnitX_)+100)/33.97))

          !  nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)=&
          !      (6.04e18*&
          !      exp(-((r_GB(i,j,k,iBlock)-Rbody)*0.001*No2Si_V(UnitX_)+100)/6.98))+&
          !    (1.67e15*&
          !      exp(-((r_GB(i,j,k,iBlock)-Rbody)*0.001*No2Si_V(UnitX_)+100)/11.49))

          !  nDenNuSpecies_CBI(i,j,k,iBlock,Oh_)=&
          !        (5.23e3*&
          !        exp(-((r_GB(i,j,k,iBlock)-Rbody)*0.001*No2Si_V(UnitX_)+100)/626.2))+&
          !        (9.76e2*&
          !        exp(-((r_GB(i,j,k,iBlock)-Rbody)*0.001*No2Si_V(UnitX_)+100)/2790.0))+&
          !        (3.71e4*&
          !        exp(-((r_GB(i,j,k,iBlock)-Rbody)*0.001*No2Si_V(UnitX_)+100)/88.47))

          !   nDenNuSpecies_CBI(i,j,k,iBlock,Ohx_)=0.0

          !   nDenNuSpecies_CBI(i,j,k,iBlock,Ox_)=0.0
          !   nDenNuSpecies_CBI(i,j,k,iBlock,Hx_)=0.0
          !   nDenNuSpecies_CBI(i,j,k,iBlock,CO2x_)=0.0

          Alt0= (r_GB(i,j,k,iBlock)-1.0)*3396.0
          if(UseIssiC)then
             nDenNuSpecies_CBI(i,j,k,iBlock,H_)=&
                  1.0e3*&
                  exp(9.25e5*(1.0/(Alt0+3393.5)-1.0/3593.5))
             nDenNuSpecies_CBI(i,j,k,iBlock,Hx_)= &
                  3.0e4*&
                  exp(1.48e4 *(1.0/(Alt0+3393.5)-1.0/3593.5))
          else if(UseIssiA)then
             nDenNuSpecies_CBI(i,j,k,iBlock,H_)=&
                  1.5e5*&
                  exp(25965*(1.0/(Alt0+3393.5)-1.0/3593.5))

             nDenNuSpecies_CBI(i,j,k,iBlock,Hx_)= &
                  1.9e4*&
                  exp(10365*(1.0/(Alt0+3393.5)-1.0/3593.5))

          end if

       else
          nDenNuSpecies_CBI(i,j,k,iBlock,:)=0.0

       end if

    end do;end do;end do

    !    call neutral_density_averages  ! calculate averaged neutral density

    ! calculate optical depth and producation rate
    do k=1,nK; do j=1,nJ; do i=1,nI
       cosSZA = (0.5+sign(0.5,Xyz_DGB(x_,i,j,k,iBlock)))*&
            Xyz_DGB(x_,i,j,k,iBlock)/max(r_GB(i,j,k,iBlock),1.0e-3)&
            +5.0e-4

       OptDep = max( sum(nDenNuSpecies_CBI(i,j,k,iBlock,1:MaxNuSpecies)*&
            CrossSection_I(1:MaxNuSpecies)*HNuSpecies_I(1:MaxNuSpecies)),&
            6.0e-3)/cosSZA
       if( OptDep<11.5 .and. Xyz_DGB(x_,i,j,k,iBlock) > 0.0) then
          Productrate_CB(i,j,k,iBlock) = max(exp(-OptDep), 1.0e-5)
       else
          Productrate_CB(i,j,k,iBlock) = 1.0e-5
       end if

    end do; end do; end do

    do k=1,nK; do j=1,nJ; do i=1,nI
       if(UseHotO) then
          nu_BLK(i,j,k,iBlock)=&
               sum(nDenNuSpecies_CBI(i,j,k,iBlock,:))*nu0
          nDenNuSpecies_CBI(i,j,k,iBlock,O_)= &
               nDenNuSpecies_CBI(i,j,k,iBlock,O_)+ &
               nDenNuSpecies_CBI(i,j,k,iBlock,Ox_)

          nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)= &
               nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)+ &
               nDenNuSpecies_CBI(i,j,k,iBlock,CO2x_)

          nDenNuSpecies_CBI(i,j,k,iBlock,O_)= &
               nDenNuSpecies_CBI(i,j,k,iBlock,O_)+ &
               nDenNuSpecies_CBI(i,j,k,iBlock,Oh_)+&
               nDenNuSpecies_CBI(i,j,k,iBlock,Ohx_)+&
               nDenNuSpecies_CBI(i,j,k,iBlock,Oh2x_)

          nDenNuSpecies_CBI(i,j,k,iBlock,H_)= &
               nDenNuSpecies_CBI(i,j,k,iBlock,H_)+ &
               nDenNuSpecies_CBI(i,j,k,iBlock,Hx_)

       else
          nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)= &
               nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)+ &
               nDenNuSpecies_CBI(i,j,k,iBlock,CO2x_)

          nDenNuSpecies_CBI(i,j,k,iBlock,O_)= &
               nDenNuSpecies_CBI(i,j,k,iBlock,O_)+ &
               nDenNuSpecies_CBI(i,j,k,iBlock,Ox_)

          nu_BLK(i,j,k,iBlock)=(nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)+&
               nDenNuSpecies_CBI(i,j,k,iBlock,O_) + &
               nDenNuSpecies_CBI(i,j,k,iBlock,H_))*nu0

       end if

!!!! doubling neutral oxygen density
       ! nDenNuSpecies_CBI(i,j,k,iBlock,O_)=2*nDenNuSpecies_CBI(i,j,k,iBlock,O_)

    end do; end do; end do

    if(UseMarsAtm)then
       if(maxval(r_GB(:,:,:,iBlock))<5.0*Rbody) call Mars_input(iBlock)

       do k=1,nK; do j=1,nJ; do i=1,nI
          if(UseHotO) then
             if(UseDSMC .and. minval(r_GB(:,:,:,iBlock))>1.+135/3396. &
                  .and. maxval(r_GB(:,:,:,iBlock))<5.0) then
                call DSMC_input(iBlock)
             else
                nDenNuSpecies_CBI(i,j,k,iBlock,Oh_)= &
                     nDenNuSpecies_CBI(i,j,k,iBlock,Oh_)+&
                     nDenNuSpecies_CBI(i,j,k,iBlock,Ohx_)+&
                     nDenNuSpecies_CBI(i,j,k,iBlock,Oh2x_)
             end if

             nDenNuSpecies_CBI(i,j,k,iBlock,O_)= &
                  nDenNuSpecies_CBI(i,j,k,iBlock,O_)+ &
                  nDenNuSpecies_CBI(i,j,k,iBlock,Oh_)

             nu_BLK(i,j,k,iBlock)=(nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)+&
                  nDenNuSpecies_CBI(i,j,k,iBlock,O_)+&
                  nDenNuSpecies_CBI(i,j,k,iBlock,H_) )*nu0
          else
             nu_BLK(i,j,k,iBlock)=(nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)+&
                  nDenNuSpecies_CBI(i,j,k,iBlock,O_)+&
                  nDenNuSpecies_CBI(i,j,k,iBlock,H_))*nu0

             !             nDenNuSpecies_CBI(i,j,k,iBlock,H_)= 1.0e-5

          end if

          Ionizationrate_CBI(i,j,k,iBlock,CO2_)=&
               Ionizationrate_CBI(i,j,k,iBlock,CO2_)*&
               nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)
          Ionizationrate_CBI(i,j,k,iBlock,O_)=&
               Ionizationrate_CBI(i,j,k,iBlock,O_)*&
               nDenNuSpecies_CBI(i,j,k,iBlock,O_)+&
               Ionizationrate_CBI(i,j,k,iBlock,Op2_)*&
               nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)

          if(Xyz_DGB(x_,i,j,k,iBlock) < -0.3 .and. &
               sqrt(Xyz_DGB(y_,i,j,k,iBlock)**2+Xyz_DGB(z_,i,j,k,iBlock)**2) < 1.09 &
               .and. r_GB(i,j,k,iBlock)>3696.0/3396.0) then
             Ionizationrate_CBI(i,j,k,iBlock,O_)=  &
                  Rate_I(O_hv__Op_em_)&
                  *nDenNuSpecies_CBI(i,j,k,iBlock,O_)&
                  *1.0e-6&
                  +Rate_I(CO2_hv__Op_CO_em_)&
                  *nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)&
                  *1.0e-6
             Ionizationrate_CBI(i,j,k,iBlock,CO2_)= &
                  Rate_I(CO2_hv__CO2p_em_)&
                  *nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)&
                  *1.0e-6
          end if
       end do; end do; end do
    else
       do k=1,nK; do j=1,nJ; do i=1,nI
          Ionizationrate_CBI(i,j,k,iBlock,O_)= &
               Rate_I(O_hv__Op_em_)&
               *nDenNuSpecies_CBI(i,j,k,iBlock,O_)&
               *Productrate_CB(i,j,k,iBlock)&
               +Rate_I(CO2_hv__Op_CO_em_)&
               *nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)&
               *Productrate_CB(i,j,k,iBlock)

          Ionizationrate_CBI(i,j,k,iBlock,CO2_)= &
               Rate_I(CO2_hv__CO2p_em_)&
               *nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)&
               *Productrate_CB(i,j,k,iBlock)

       end do; end do; end do

    end if
    call test_stop(NameSub, DoTest, iBlock)
  end subroutine set_neutral_density
  !============================================================================
  subroutine user_get_b0(xMinBox,yMinBox,zMinBox,B1)
    use ModMain
    use ModPhysics
    use ModNumConst

    real, intent(in) :: xMinBox,yMinBox,zMinBox
    real, intent(out), dimension(3) :: B1

    real :: R0, theta, phi, rr, X0, Y0, Z0
    real, dimension(3) :: bb, B0
    real :: sint, sinp, cost, cosp, uB

    !--------------------------------------------------------------------------
    X0 = xMinBox*cos(thetilt)-zMinBox*sin(thetilt)
    Y0 = yMinBox
    Z0 = xMinBox*sin(thetilt)+zMinBox*cos(thetilt)

    R0 = sqrt(X0*X0 + Y0*Y0 + Z0*Z0)
    rr = max(R0, 1.00E-6)
    if(abs(X0) < 1e-6) then
       if(Y0 < 0) then
          phi=-cPi/2.
       else
          phi=cPi/2.
       endif
    else
       if(X0 > 0) then
          phi=atan(Y0/X0)
       else
          phi=cPi+atan(Y0/X0)
       endif
    endif

    ! rot=cPi

    theta=acos(Z0/rr)

    call MarsB0(R0,theta, phi+rot, bb)

    sint=sin(theta)
    cost=cos(theta)
    sinp=sin(phi)
    cosp=cos(phi)

    B0(1) = bb(1)*sint*cosp+bb(2)*cost*cosp-bb(3)*sinp
    B0(2) = bb(1)*sint*sinp+bb(2)*cost*sinp+bb(3)*cosp
    B0(3) = bb(1)*cost-bb(2)*sint

    B1(1) = B0(1)*cos(thetilt)+B0(3)*sin(thetilt)
    B1(2) = B0(2)
    B1(3) = -B0(1)*sin(thetilt)+B0(3)*cos(thetilt)

    ! unit of magnetic field is uB=1.677600/0.263661
    ! write(*,*)'unit=',No2Io_V(UnitB_)
    uB=No2Io_V(UnitB_)
    !   write(*,*)'UB=',uB
    ! uB=6.3627
    B1(1)=B1(1)/uB
    B1(2)=B1(2)/uB
    B1(3)=B1(3)/uB

  end subroutine user_get_b0
  !============================================================================

  subroutine MarsB0(r,theta, phi, bb)

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

    character(len=*), parameter:: NameSub = 'MarsB0'
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

    a=1.035336
    arr=a/r

    ! NNm=8

    mars_eps=1e-3

    if(r < 1.0) then
       NN=0
    else
       NN=NNm-1
    endif

    xtcos=cos(theta)
    xtsin=sin(theta)

    do im=0,NN
       xpcos(im)=cos(im*phi)
       xpsin(im)=sin(im*phi)
    end do

    bb(1)=0.0
    bb(2)=0.0
    bb(3)=0.0

    !    somx2=sqrt((1.-xtcos)*(1.+xtcos))
    somx2=abs(xtsin)
    signsx=sign(1., xtsin)

    fact=1.
    Rmm=1.
    Rnm(0,0)=sqrt(2.)
    Rnm(1,0)=xtcos*sqrt(3.)*Rnm(0,0)
    do n=2, NN
       Rnm(n, 0)=(xtcos*sqrt((2.*n-1.)*(2.*n+1.))*Rnm(n-1,0)-&
            (n-1)*sqrt((2.*n+1.)/(2.*n-3.))*Rnm(n-2, 0))/n

    enddo ! n

    arrm=1.0

    call timing_start('crustal')

    do m=0, NN

       Rmm=Rmm*fact*somx2/Factor1_I(m)

       Rnm(m+1,m+1)=Rmm*Factor2_I(m)
       Rnm(m, m+1)=0

       fact=fact+2.

       arrm=arr*arrm
       arrn=arrm

       do n=m,NN
          arrn=arr*arrn
          ! write(*,*) 'arrn=', arrn, ' n=', n
          if(n> (m+2)) then
             Rnm(n,m+1) = xtcos*Factor1_II(n,m)*Rnm(n-1,m+1)-&
                  Factor2_II(n,m)*Rnm(n-2,m+1)

          else if(n > (m+1)) then
             Rnm(n,m+1)=xtcos*Factor3_I(m)*Rnm(m+1,m+1)
          endif

          dRnm=m*xtcos*Rnm(n,m)/xtsin-Rnm(n, m+1)*signsx* Factor3_II(n,m)

          bb(1)=bb(1)+(n+1)*arrn*Rnm(n,m)*(cmars(n,m)*xpcos(m)&
               +dmars(n,m)*xpsin(m))
          bb(2)=bb(2)-arrn*dRnm*(cmars(n,m)*&
               xpcos(m)+dmars(n,m)*xpsin(m))
          if(xtsin <= 1e-6) then
             bb(3)=0.
          else
             bb(3)=bb(3)-arrn*Rnm(n,m)*m/xtsin*(-cmars(n,m)*xpsin(m)&
                  +dmars(n,m)*xpcos(m))
          endif
       end do ! n
    end do ! m

    call timing_stop('crustal')

  end subroutine MarsB0
  !============================================================================

end module ModUser
!==============================================================================
