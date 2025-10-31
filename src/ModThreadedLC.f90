!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModThreadedLC

  use ModBatsrusUtility, ONLY: stop_mpi
#ifdef _OPENACC
  use ModUtilities, ONLY: norm2
#endif
  use BATL_lib, ONLY: test_start, test_stop, iProc, Xyz_DGB
  use ModChromosphere, ONLY: TeChromosphereSi
  use ModTransitionRegion, ONLY:  iTableTR, TeSiMin, SqrtZ, CoulombLog, &
       HeatCondParSi, TrTable_V, PavrL_, UHeat_, HeatFluxL_,&
       DHeatFluxLoverDcons_, Lambda_, DLogLambdaOverDLogT_, init_tr
  use ModFieldLineThread, ONLY: BoundaryThreads, BoundaryThreads_B,     &
       PSi_, TeSi_, TiSi_, AMajor_, AMinor_,                             &
       DoInit_, Done_, Enthalpy_, Heat_, Restart_
  use ModAdvance, ONLY: UseElectronPressure, UseIdealEos
  use ModTurbulence, ONLY:PoyntingFluxPerBSi, PoyntingFluxPerB,    &
       ImbalanceMax
  use ModTurbulence, ONLY: QeRatio
  use ModPhysics, ONLY: Z => AverageIonCharge
  use ModConst, ONLY: rSun, mSun, cBoltzmann, cAtomicMass, cGravitation
  use omp_lib

  !   Hydrostatic equilibrium in an isothermal corona:
  !    d(N_i*k_B*(Z*T_e +T_i) )/dr=G*M_sun*N_I*M_i*d(1/r)/dr
  ! => N_i*Te\propto exp(cGravPot/TeSi*(M_i[amu]/(1+Z))*\Delta(R_sun/r))
  ! The plasma properties dependent coefficient needed to evaluate the
  ! effect of gravity on the hydrostatic equilibrium
  use ModFieldLineThread, ONLY:  &
       GravHydroStat != cGravPot*MassIon_I(1)/(Z + 1)

  ! To espress Te  and Ti in terms of P and rho, for ideal EOS:
  ! Te = TeFraction*State_V(iPe)/State_V(Rho_)
  ! Pe = PeFraction*State_V(iPe)
  ! Ti = TiFraction*State_V(p_)/State_V(Rho_)
  use ModFieldLineThread, ONLY:  &
       TeFraction, TiFraction,  iPe, PeFraction

  implicit none
  SAVE

  ! energy flux needed to raise the mass flux rho*u to the heliocentric
  ! distance r equals: rho*u*G*Msun*(1/R_sun -1/r)=
  !=k_B*N_i*M_i(amu)u*cGravPot*(1-R_sun/r)=
  !=P_e/T_e*cGravPot*u(M_i[amu]/Z)*(1/R_sun -1/r)
  real :: GravHydroDyn ! = cGravPot*MassIon_I(1)/AverageIonCharge

  ! Temperature 3D array
  real,allocatable :: Te_G(:,:,:)
  !$omp threadprivate( Te_G )

  ! Arrays for 1D distributions
  real,allocatable,dimension(:):: ReflCoef_I, AMajor_I, AMinor_I,            &
       TeSi_I, TeSiStart_I, PSi_I, Xi_I, Cons_I,                             &
       TiSi_I, TiSiStart_I, SpecIonHeat_I, DeltaIonEnergy_I,                 &
       VaLog_I, DXi_I, ResHeating_I, ResCooling_I, DResCoolingOverDLogT_I,   &
       ResEnthalpy_I, ResHeatCond_I, ResGravity_I, SpecHeat_I, DeltaEnergy_I,&
       ExchangeRate_I

  ! We apply ADI to solve state vector, the components of the state
  ! being temperature and log pressure.
  ! The heating at constant pressure is characterized by the
  ! specific heat at constant pressure. For pressure the barometric
  ! formula is applied
  integer, parameter:: Cons_ = 1, Ti_=2, LogP_ = 3

  real, allocatable:: Res_VI(:,:), DCons_VI(:,:)
  real, allocatable:: Main_VVI(:,:,:), Lower_VVI(:,:,:), Upper_VVI(:,:,:)

  ! Two logicals with self-explained names
  logical        :: UseAlignedVelocity = .true.
  logical        :: DoConvergenceCheck = .false.

  ! Two parameters controling the choise of the order for density and
  ! pressure: first order (LimMin=LimMax=0), second order (LimMin=LimMax=1)
  ! or limited second order as a default
  real:: LimMin = 0.0, LimMax = 1

  ! Coefficient to express dimensionless density as RhoNoDimCoef*PeSi/TeSi
  real           ::  RhoNoDimCoef

  ! Misc
  real:: TeMin, ConsMin, ConsMax, TeSiMax

  ! For transforming conservative to TeSi and back
  real, parameter:: cTwoSevenths = 2.0/7.0

  ! Gravitation potential in K
  real, parameter:: cGravPot = cGravitation*mSun*cAtomicMass/&
       (cBoltzmann*rSun)

  ! In hydrogen palsma, the electron-ion heat exchange is described by
  ! the equation as follows:
  ! dTe/dt = -(Te-Ti)/(tau_{ei})
  ! dTi/dt = +(Te-Ti)/(tau_{ei})
  ! The expression for 1/tau_{ei} may be found in
  ! Lifshitz&Pitaevskii, Physical Kinetics, Eq.42.5
  ! note that in the Russian edition they denote k_B T as Te and
  ! the factor 3 is missed in the denominator:
  ! 1/tau_ei = 2* CoulombLog * sqrt{m_e} (e^2/cEps)**2* Z**2 *Ni /&
  ! ( 3 * (2\pi k_B Te)**1.5 M_p). This exchange rate scales linearly
  ! with the plasma density, therefore, we introduce its ratio to
  ! the particle concentration. We calculate the temperature exchange
  ! rate by multiplying the expression for electron-ion effective
  ! collision rate,
  ! \nu_{ei} = CoulombLog/sqrt(cElectronMass)*  &
  !            ( cElectronCharge**2 / cEps)**2 /&
  !            ( 3 *(cTwoPi*cBoltzmann)**1.50 )* Ne/Te**1.5
  !  and then multiply in by the energy exchange coefficient
  !            (2*cElectronMass/cProtonMass)
  ! The calculation of the effective electron-ion collision rate is
  ! re-usable and can be also applied to calculate the resistivity:
  ! \eta = m \nu_{ei}/(e**2 Ne)
  real :: cExchangeRateSi

  ! See above about usage of the latter constant
  integer, parameter:: Impl_=4

  integer:: nIter = 20
  real   :: cTol = 1.0e-6

contains
  !============================================================================
  subroutine init_threaded_lc

    use BATL_lib, ONLY:  MinI, MaxI, MinJ, MaxJ, MinK, MaxK, iComm
    use ModConst, ONLY: te_ti_exchange_rate! cElectronMass, &
         ! cEps, cElectronCharge, cTwoPi, cProtonMass
    use ModMultiFluid, ONLY: MassIon_I
    use ModFieldLineThread, ONLY:  nPointThreadMax, init_threads
    use ModPhysics, ONLY: &
         UnitTemperature_, Si2No_V, UnitEnergyDens_

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_threaded_lc'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    allocate(Te_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)); Te_G = 0.0

    allocate(ReflCoef_I(0:nPointThreadMax)); ReflCoef_I = 0.0
    allocate(  AMajor_I(0:nPointThreadMax));   AMajor_I = 0.0
    allocate(  AMinor_I(0:nPointThreadMax));   AMinor_I = 0.0

    allocate(   TeSi_I(nPointThreadMax));     TeSi_I = 0.0
    allocate(TeSiStart_I(nPointThreadMax));  TeSiStart_I = 0.0
    allocate(   TiSi_I(nPointThreadMax));     TiSi_I = 0.0
    allocate(TiSiStart_I(nPointThreadMax));  TiSiStart_I = 0.0
    allocate(   Cons_I(nPointThreadMax));     Cons_I = 0.0
    allocate( PSi_I(nPointThreadMax));   PSi_I = 0.0

    allocate(   SpecHeat_I(nPointThreadMax));   SpecHeat_I = 0.0
    allocate(DeltaEnergy_I(nPointThreadMax));DeltaEnergy_I = 0.0
    allocate(SpecIonHeat_I(nPointThreadMax));   SpecIonHeat_I = 0.0
    allocate(DeltaIonEnergy_I(nPointThreadMax));DeltaIonEnergy_I = 0.0
    allocate(ExchangeRate_I(nPointThreadMax));ExchangeRate_I = 0.0

    allocate( ResHeating_I(nPointThreadMax)); ResHeating_I = 0.0
    allocate( ResCooling_I(nPointThreadMax)); ResCooling_I = 0.0
    allocate(DResCoolingOverDLogT_I(nPointThreadMax))
    DResCoolingOverDLogT_I = 0.0
    allocate(ResEnthalpy_I(nPointThreadMax));ResEnthalpy_I = 0.0
    allocate(ResHeatCond_I(nPointThreadMax));ResHeatCond_I = 0.0
    allocate( ResGravity_I(nPointThreadMax)); ResGravity_I = 0.0

    allocate(     Xi_I(0:nPointThreadMax));     Xi_I = 0.0
    allocate(  VaLog_I(nPointThreadMax));    VaLog_I = 0.0
    allocate(    DXi_I(nPointThreadMax));      DXi_I = 0.0

    allocate(  Res_VI(Cons_:LogP_,nPointThreadMax));      Res_VI = 0.0
    allocate(DCons_VI(Cons_:LogP_,nPointThreadMax));    DCons_VI = 0.0

    allocate(Upper_VVI(Cons_:LogP_,Cons_:LogP_,nPointThreadMax)); Upper_VVI = 0
    allocate(Lower_VVI(Cons_:LogP_,Cons_:LogP_,nPointThreadMax)); Lower_VVI = 0
    allocate(Main_VVI(Cons_:LogP_,Cons_:LogP_,nPointThreadMax));  Main_VVI = 0

    ! Initialize transition region model:
    call init_tr(zIn=Z, TeChromoSi = TeChromosphereSi, iComm=iComm)
    !
    ! Initialize thread structure
    !
    call init_threads

    !
    ! In hydrogen palsma, the electron-ion heat exchange is described by
    ! the equation as follows:
    ! dTe/dt = -(Te-Ti)/(tau_{ei})
    ! dTi/dt = +(Te-Ti)/(tau_{ei})
    ! The expression for 1/tau_{ei} may be found in
    ! Lifshitz&Pitaevskii, Physical Kinetics, Eq.42.5
    ! note that in the Russian edition they denote k_B T as Te and
    ! the factor 3 is missed in the denominator:
    ! 1/tau_ei = 2* CoulombLog * sqrt{m_e} (e^2/cEps)**2* Z**2 *Ni /&
    ! ( 3 * (2\pi k_B Te)**1.5 M_p). This exchange rate scales linearly
    ! with the plasma density, therefore, we introduce its ratio to
    ! the particle concentration. We calculate the temperature exchange
    ! rate by multiplying the expression for electron-ion effective
    ! collision rate,
    ! \nu_{ei} = CoulombLog/sqrt(cElectronMass)*  &
    !            ( cElectronCharge**2 / cEps)**2 /&
    !            ( 3 *(cTwoPi*cBoltzmann)**1.50 )* Ne/Te**1.5
    !  and then multiply in by the energy exchange coefficient
    !            (2*cElectronMass/cProtonMass)
    ! The calculation of the effective electron-ion collision rate is
    ! re-usable and can be also applied to calculate the resistivity:
    ! \eta = m \nu_{ei}/(e**2 Ne)
    !
    cExchangeRateSi = te_ti_exchange_rate(CoulombLog)/cBoltzmann
    ! Dimensionless temperature floor
    TeMin = TeSiMin*Si2No_V(UnitTemperature_)

    ConsMin = cTwoSevenths*HeatCondParSi*TeSiMin**3.5

    !   Hydrostatic equilibrium in an isothermal corona:
    !    d(N_i*k_B*(Z*T_e +T_i) )/dr=G*M_sun*N_I*M_i*d(1/r)/dr
    ! => N_i*Te\propto exp(cGravPot/TeSi*(M_i[amu]/(1+Z))*\Delta(R_sun/r))
    GravHydroStat = cGravPot*MassIon_I(1)/(Z + 1)
    ! energy flux needed to raise the mass flux rho*u to the heliocentric
    ! distance r equals: rho*u*G*Msun*(1/R_sun -1/r)=
    !=k_B*N_i*M_i(amu)u*cGravPot*(1-R_sun/r)=
    !=P_e/T_e*cGravPot*u(M_i[amu]/Z)*(1/R_sun -1/r)
    GravHydroDyn  = cGravPot*MassIon_I(1)/Z
    ! With this constant, the dimensionless density
    ! equals RhoNoDimCoef*PeSi/TeSi
    RhoNoDimCoef = Si2No_V(UnitEnergyDens_)/PeFraction*&
         TeFraction/Si2No_V(UnitTemperature_)
    call test_stop(NameSub, DoTest)

  end subroutine init_threaded_lc
  !============================================================================
  subroutine read_threaded_bc_param

    use ModReadParam, ONLY: read_var
    character(len=7)::TypeBc = 'limited'

    character(len=*), parameter:: NameSub = 'read_threaded_bc_param'
    !--------------------------------------------------------------------------
    call read_var('UseAlignedVelocity', UseAlignedVelocity)
    call read_var('DoConvergenceCheck', DoConvergenceCheck)
    call read_var('TypeBc'            , TypeBc            )
    select case(TypeBc)
    case('first')
       LimMin = 0; LimMax = 0
    case('second')
       LimMin = 1; LimMax = 1
    case('limited')
       LimMin = 0; LimMax = 1
    case default
       if(iProc==0)write(*,'(a)')&
            'Unknown TypeBc = '//TypeBc//', reset to limited'
    end select
    call read_var('Tolerance', cTol)
    call read_var('MaxIter', nIter)

  end subroutine read_threaded_bc_param
  !============================================================================
  subroutine solve_boundary_thread(j, k, iBlock, &
       iAction, TeSiIn, TiSiIn, PeSiIn, USiIn, AMinorIn, &
       DTeOverDsSiOut, PeSiOut,PiSiOut, RhoNoDimOut, AMajorOut)

    ! solve MHD equations along thread, to link the state above the
    ! inner boundary of the global solar corona to the photosphere

    use ModAdvance, ONLY: nJ
    use ModTransitionRegion, ONLY: HeatCondParSi
    use ModPhysics, ONLY: InvGammaMinus1,&
         No2Si_V, UnitX_,Si2No_V, UnitB_, UnitTemperature_
    use ModLookupTable, ONLY: interpolate_lookup_table

    ! Cell and block indexes for the boundary point
    integer,intent(in):: j, k, iBlock, iAction

    ! Parameters of the state in the true cell near the boundary:
    ! TeSiIn: Temperature in K
    ! USiIn:  Velocity progection on the magnetic field direction.
    ! It is positive if the wind blows outward the Sun.
    ! AMinorIn: for the wave propagating toward the Sun
    !            WaveEnergyDensity = (\Pi/B)\sqrt{\rho} AMinor**2
    real,   intent(in):: TeSiIn, TiSiIn, PeSiIn, USiIn, AMinorIn

    ! DTeOverDsSiOut: Temperature derivative along the thread, at the end point
    !                Used to find the electron temperature in the ghostcell
    ! PeSiOut: The electron pressure
    ! AMajorOut: For the wave propagating outward the Sun
    !            EnergyDensity = (\Pi/B)\sqrt{\rho} AMajor**2
    real,  intent(out):: &
         DTeOverDsSiOut, PeSiOut, PiSiOut, RhoNoDimOut, AMajorOut

    ! Arrays needed to use lookup table
    ! Limited Speed
    real    :: USi
    !---------Used in 1D numerical model------------------------
    ! Number of TEMPERATURE nodes (first one is on the top of TR
    ! the last one is in the physical cell of the SC model
    integer        :: nPoint, iPoint

    ! Corrrect density and pressure values in the ghost
    real :: GhostCellCorr, BarometricFactor, DeltaTeFactor,             &
         Limiter, DensityRatio, RhoTrueCell,                            &
         FirstOrderRho, FirstOrderPeSi, SecondOrderRho, SecondOrderPeSi

    ! Electron heat condution flux from Low Corona to TR:
    real :: HeatFlux2TR
    !
    ! Constant particle flux per B times k_B
    ! k_B n_i (= P_tot/(ZTe + Ti)) U/B Normalize per Flux2B costant ratio
    real :: FluxConst

    integer :: nIterHere
    logical :: DoCheckConvHere

    character(len=12) :: NameTiming
    ! Initialize all output parameters from 0D solution
    character(len=*), parameter:: NameSub = 'solve_boundary_thread'
    !--------------------------------------------------------------------------
    write(NameTiming,'(a,i2.2)')'set_thread',j + nJ*(k - 1)
    call timing_start(NameTiming)

    nPoint = BoundaryThreads_B(iBlock)% nPoint_II(j,k)

    ! Constant particle flux per B times k_B
    ! k_B n_i (= P_tot/(ZTe + Ti)) U/B Normalize per Flux2B costant ratio
    FluxConst    = USiIn * PeSiIn/&
         (TeSiIn*PoyntingFluxPerBSi*&
         BoundaryThreads_B(iBlock)% B_III(0,j,k)*No2Si_V(UnitB_))
    call interpolate_lookup_table(iTableTR, TeSiIn, 0.0, &
         Value_V=TrTable_V, DoExtrapolate=.false.)
    ! First value is now the product of the thread length in meters times
    ! a geometric mean pressure, so that
    PeSiOut        = TrTable_V(PAvrL_)*SqrtZ/&
         BoundaryThreads_B(iBlock)% LengthSi_III(0,j,k)
    PiSiOut = PeSiOut/(Z*TeSiIn)*TiSiIn
    USi = USiIn
    if(USi>0)then
       USi = min(USi,0.1*TrTable_V(UHeat_))
    else
       USi = max(USi, -0.1*TrTable_V(HeatFluxL_)/&
            (BoundaryThreads_B(iBlock)% LengthSi_III(0,j,k)*PeSiIn))
    end if
    RhoNoDimOut    = RhoNoDimCoef*PeSiOut/TeSiIn
    AMajorOut      = 1.0
    TeSiMax        = &
         BoundaryThreads_B(iBlock) % TMax_II(j,k)*No2Si_V(UnitTemperature_)
    ConsMax = cTwoSevenths*HeatCondParSi*TeSiMax**3.5

    if(iAction/=DoInit_)then
       ! Retrieve temperature and pressure distribution
       TeSi_I(1:nPoint) &
            = BoundaryThreads_B(iBlock)%State_VIII(TeSi_,1-nPoint:0,j,k)
       TiSi_I(1:nPoint) &
            = BoundaryThreads_B(iBlock)%State_VIII(TiSi_,1-nPoint:0,j,k)
       PSi_I(1:nPoint) &
            = BoundaryThreads_B(iBlock)%State_VIII(PSi_,1-nPoint:0,j,k)
       AMajor_I(0:nPoint) &
            = BoundaryThreads_B(iBlock)%State_VIII(AMajor_,-nPoint:0,j,k)
       AMinor_I(0:nPoint) &
            = BoundaryThreads_B(iBlock)%State_VIII(AMinor_,-nPoint:0,j,k)
       if(iAction/=Enthalpy_.and.iAction/=Restart_)then
          TeSi_I(nPoint)   = TeSiIn
          TiSi_I(nPoint)   = TiSiIn
       end if
       DoCheckConvHere = .false.
       nIterHere       = 1
    else
       DoCheckConvHere = DoConvergenceCheck
       nIterHere       = nIter
    end if
    AMinor_I(nPoint)    = AMinorIn
    select case(iAction)
    case(DoInit_)
       ! As a first approximation, recover Te from the analytical solution
       TeSi_I(nPoint) = TeSiIn; TiSi_I(nPoint) = TiSiIn
       do iPoint = nPoint-1, 1, -1
          call interpolate_lookup_table(Arg2In=0.0, &
               iTable=iTableTR,         &
               iVal=PAvrL_,             &
               ValIn=PeSiOut/SqrtZ*     &
               BoundaryThreads_B(iBlock)% LengthSi_III(iPoint-nPoint,j,k), &
               Value_V=TrTable_V,       &
               Arg1Out=TeSi_I(iPoint),  &
               DoExtrapolate=.false.)
       end do
       TeSi_I(1:nPoint) = max(TeSiMin, TeSi_I(1:nPoint))
       TiSi_I(1:nPoint) = TeSi_I(1:nPoint)
       ! The analytical solution assumes constant pressure and
       ! no heating. Calculate the pressure distribution
       call set_pressure
       ! Get waves from boundary conditions
       AMajor_I(0:nPoint) = 1.0
       AMinor_I(0:nPoint) = AMinorIn
       call advance_thread(IsTimeAccurateThread=.false.)
       BoundaryThreads_B(iBlock)%State_VIII(TeSi_,1-nPoint:0,j,k) &
            = TeSi_I(1:nPoint)
       BoundaryThreads_B(iBlock)%State_VIII(TiSi_,1-nPoint:0,j,k) &
            = TiSi_I(1:nPoint)
       BoundaryThreads_B(iBlock)%State_VIII(PSi_,1-nPoint:0,j,k) &
            = PSi_I(1:nPoint)
       BoundaryThreads_B(iBlock)%State_VIII(AMajor_,-nPoint:0,j,k) &
            = AMajor_I(0:nPoint)
       BoundaryThreads_B(iBlock)%State_VIII(AMinor_,-nPoint:0,j,k) &
            = AMinor_I(0:nPoint)
    case(Enthalpy_)
       call get_res_heating(nIterIn=nIterHere)
       !
       ! Do not store temperature
       !
    case(Restart_)
       !
       ! Do not store temperature
       !
    case(Impl_)
       call advance_thread(IsTimeAccurateThread=.true.)
       AMajorOut = AMajor_I(nPoint)
       ! Output for temperature gradient, all the other outputs
       ! are meaningless
       DTeOverDsSiOut = max(0.0, &
            (TeSi_I(nPoint) - TeSi_I(nPoint-1))/&
            (BoundaryThreads_B(iBlock)% LengthSi_III(0,j,k) - &
            BoundaryThreads_B(iBlock)% LengthSi_III(-1,j,k)))
       ! Do not store temperatures
       call timing_stop(NameTiming)
       RETURN
    case(Heat_)
       call advance_thread(IsTimeAccurateThread=.true.)
       ! Calculate AWaves and store pressure and temperature
       call get_res_heating(nIterIn=nIterHere)
       BoundaryThreads_B(iBlock)%State_VIII(TeSi_,1-nPoint:0,j,k) &
            = TeSi_I(1:nPoint)
       BoundaryThreads_B(iBlock)%State_VIII(TiSi_,1-nPoint:0,j,k) &
            = TiSi_I(1:nPoint)
       BoundaryThreads_B(iBlock)%State_VIII(PSi_,1-nPoint:0,j,k) &
            = PSi_I(1:nPoint)
       BoundaryThreads_B(iBlock)%State_VIII(AMajor_,-nPoint:0,j,k) &
            = AMajor_I(0:nPoint)
       BoundaryThreads_B(iBlock)%State_VIII(AMinor_,-nPoint:0,j,k) &
            = AMinor_I(0:nPoint)
    case default
       write(*,*)'iAction=',iAction
       call stop_mpi('Unknown action in '//NameSub)
    end select
    AMajorOut = AMajor_I(nPoint)
    ! Outputs
    ! First order solution: ghost cell from the first layer are filled
    ! in with the solution of the threaded field line equation at the
    ! end point.
    FirstOrderRho   = RhoNoDimCoef*PSi_I(nPoint)*Z/&
         (Z*TeSi_I(nPoint) + TiSi_I(nPoint))
    FirstOrderPeSi  = PSi_I(nPoint)*TeSi_I(nPoint)*Z/&
         (Z*TeSi_I(nPoint) + TiSi_I(nPoint))

    ! Second order solution consists of two contributions, the first of them
    ! being the correction of true cell values. Calculate the true density:
    RhoTrueCell = RhoNoDimCoef*PeSiIn/TeSiIn
    ! The pressure in the ghost cell should be corrected corrected for
    ! a barometric scale factor, as a consequence of the hydrostatic
    ! equiliblrium condition in the physical cell. Cell corr as calculated
    ! below is the negative of DeltaR of BATSRUS / delta r of the thread. Thus,
    GhostCellCorr =  BoundaryThreads_B(iBlock)% DeltaR_II(j,k)/&
         (1/BoundaryThreads_B(iBlock)%RInv_III(-1,j,k) - &
         1/BoundaryThreads_B(iBlock)%RInv_III(0,j,k) )  ! < O!
    !
    !      ghost cell | phys.cell
    !           *<-delta R->*
    !      PGhost=exp(-dLogP/dR*DeltaR)*PPhys
    BarometricFactor = exp(&
         (log(PSi_I(nPoint)) - log(PSi_I(nPoint-1)))*GhostCellCorr )
    ! Limit Barometric factor
    BarometricFactor=min(BarometricFactor,2.0)

    SecondOrderPeSi  = PeSiIn*BarometricFactor
    ! In the ghost cell value of density in addition to the
    ! barometric factor the temperature gradient is accounted for,
    ! which is cotrolled by the heat flux derived from the TFL model:

    GhostCellCorr =  BoundaryThreads_B(iBlock)% DeltaR_II(j,k)/min(          &
         (1/BoundaryThreads_B(iBlock)%RInv_III(-1,j,k) -                     &
         1/BoundaryThreads_B(iBlock)%RInv_III(0,j,k) ),-0.7*                 &
         Si2No_V(UnitX_)*(BoundaryThreads_B(iBlock)% LengthSi_III(0,j,k) -   &
         BoundaryThreads_B(iBlock)% LengthSi_III(-1,j,k)))

    DeltaTeFactor = TeSi_I(nPoint)/max(TeSiMin, TeSi_I(nPoint) + &
         max(TeSi_I(nPoint) - TeSi_I(nPoint-1), 0.0)*GhostCellCorr)
    ! Limit DeltaTeFactor
    DeltaTeFactor = min(DeltaTeFactor,2.0)
    ! Approximately TeSiGhost = TeSiIn/DeltaTeFactor, so that:
    SecondOrderRho = RhoTrueCell*BarometricFactor*DeltaTeFactor
    ! Add numerical diffusion, which forcing the density in the true cell
    ! to approach the "first order" values predicted by the TFL model.
    SecondOrderRho  = SecondOrderRho  + (FirstOrderRho - RhoTrueCell)
    SecondOrderPeSi = SecondOrderPeSi + &
         (FirstOrderPeSi - PeSiIn)/DeltaTeFactor
    ! In the latter equation the 1/DeltaTeFactor multipler is introduced
    ! to keep the ratio of the corrected values to be equal to
    ! TeSiGhost = TeSiIn/DeltaTeFactor
    ! as predicted by the TFL model
    ! Now, limit the difference between the first and second order
    ! solutions, depending on the ratio between the true cell value
    ! density and the first order ghost cell value of density
    DensityRatio = RhoTrueCell/FirstOrderRho
    ! If PeSiIn>PSi_I(nPoint), then we apply limitation, which completely
    ! eleminates the second order correction if this ratio becomes as high
    ! as the barometric factor:
    Limiter = min(LimMax, max(LimMin, &
         (BarometricFactor - DensityRatio)/(BarometricFactor - 1)))
    RhoNoDimOut = Limiter*(SecondOrderRho - FirstOrderRho) +&
         FirstOrderRho
    PeSiOut     = Limiter*(SecondOrderPeSi - FirstOrderPeSi) + &
         FirstOrderPeSi
    PiSiOut     = PeSiOut/(Z*TeSiIn)*TiSiIn
    if(RhoNoDimOut>1e8.or.RhoNoDimOut<1e-8)then
       write(*,*)'TeSiMax=       ',TeSiMax
       write(*,*)'iAction=',iAction
       write(*,*)'Xyz_DGB(:,1,j,k)',Xyz_DGB(:,1,j,k,iBlock)
       write(*,*)'RhoNoDimOut=', RhoNoDimOut,' PeSiOut=',PeSiOut
       write(*,*)'USiIn, USi=', USiIn,' ',USi
       write(*,*)'PeSiIn=',PeSiIn
       write(*,*)'TeSiIn=',TeSiIn
       write(*,*)'First order Rho, PSi=',FirstOrderRho, FirstOrderPeSi
       write(*,*)'Second order Rho, PSi=',SecondOrderRho, SecondOrderPeSi
       write(*,*)'BarometricFactor=',BarometricFactor
       write(*,*)'DeltaTeFactor=',DeltaTeFactor
       write(*,*)&
            'iPoint TeSi TeSiStart PSi ResHeating ResEnthalpy'
       do iPoint=1,nPoint
          write(*,'(i4,5es15.6)')iPoint, TeSi_I(iPoint),&
               TeSiStart_I(iPoint),&
               PSi_I(iPoint),ResHeating_I(iPoint), ResEnthalpy_I(iPoint)
       end do
       call stop_mpi('Failure')
    end if
    call timing_stop(NameTiming)
  contains
    !==========================================================================
    subroutine set_pressure
      integer::iPoint
      !------------------------------------------------------------------------
      ! First variable in TrTable_V  is now the product of the thread length in
      ! meters times a geometric mean pressure, so that
      PSi_I(1) = TrTable_V(PAvrL_)*(1 + Z)/(SqrtZ*&
           BoundaryThreads_B(iBlock)% LengthSi_III(1-nPoint,j,k))
      !   Hydrostatic equilibrium in an isothermal corona:
      !    d(N_i*k_B*(Z*T_e +T_i) )/dr=G*M_sun*N_I*M_i*d(1/r)/dr
      ! => N_i*Te\propto exp(cGravPot/TeSi*(M_i[amu]/(1+Z))*\Delta(R_sun/r))
      ! GravHydroStat = cGravPot*MassIon_I(1)/(Z + 1)
      do iPoint = 2, nPoint
         PSi_I(iPoint) = PSi_I(iPoint-1)*&
              exp( -BoundaryThreads_B(iBlock)%TGrav_III(iPoint-nPoint,j,k)*&
              (Z + 1)/(Z*TeSi_I(iPoint) + TiSi_I(iPoint)))
      end do
    end subroutine set_pressure
    !==========================================================================
    subroutine advance_thread(IsTimeAccurateThread)

      use ModMain, ONLY: cfl, Dt, IsTimeAccurate
      use ModAdvance, ONLY: DtMax_CB, nJ, nK
      use ModPhysics, ONLY: UnitT_, No2Si_V

      ! Advances the thread solution
      ! If IsTimeAccurateThread, the solution is advanced through the time
      ! interval, DtLocal. Otherwise, it looks for the steady-state solution
      ! with the advanced boundary condition
      logical, intent(in) :: IsTimeAccurateThread

      ! Time step in the physical cell from which the thread originates
      real    :: DtLocal,DtInv

      ! Loop variable
      integer :: iPoint, iIter

      ! Enthalpy correction coefficients
      real    :: EnthalpyFlux
      real    :: ElectronEnthalpyFlux, IonEnthalpyFlux

      ! Correction accounting for the Enthlpy flux from the TR
      real    :: PressureTRCoef
      !------------------------------------------------------------------------
      if(IsTimeAccurateThread)then
         if(IsTimeAccurate)then
            DtLocal = Dt*No2Si_V(UnitT_)
         else
            DtLocal = cfl*No2Si_V(UnitT_)*&
                 DtMax_CB(1,max(min(j,nJ),1), max(min(k, nK), 1), iBlock)
         end if
         if(DtLocal==0.0)RETURN ! No time-accurate advance is needed
         DtInv = 1/DtLocal
      else
         DtInv = 0.0
      end if
      ! Initialization
      TeSiStart_I(1:nPoint) = TeSi_I(1:nPoint)
      TiSiStart_I(1:nPoint) = TiSi_I(1:nPoint)
      ! dCons = kappa(Te)dTe=> Cons = 2/7 kappa*Te
      Cons_I(1:nPoint) = cTwoSevenths*HeatCondParSi*TeSi_I(1:nPoint)**3.5

      SpecHeat_I(1:nPoint-1) = InvGammaMinus1*Z*                     &
           BoundaryThreads_B(iBlock)%DsCellOverBSi_III(1-nPoint:-1,j,k)* &
           PSi_I(1:nPoint-1)/(Z*TeSi_I(1:nPoint-1) + TiSi_I(1:nPoint-1))
      SpecIonHeat_I(1:nPoint-1) = SpecHeat_I(1:nPoint-1)/Z
      ExchangeRate_I(1:nPoint-1) = cExchangeRateSi*InvGammaMinus1*Z**3*&
           BoundaryThreads_B(iBlock)%DsCellOverBSi_III(1-nPoint:-1,j,k)* &
           PSi_I(1:nPoint-1)**2/(&
           (Z*TeSi_I(1:nPoint-1) + TiSi_I(1:nPoint-1))**2*&
           TeSi_I(1:nPoint-1)**1.5)
      DeltaEnergy_I= 0.0; Res_VI=0.0 ; ResEnthalpy_I= 0.0
      DeltaIonEnergy_I = 0.0
      PressureTRCoef = 1.0; FluxConst = 0.0
      call get_res_heating(nIterIn=nIterHere)
      ! Constant particle flux per B times k_B
      ! k_B n_i (= P_tot/(ZTe + Ti)) U/B Normalize per Flux2B costant ratio
      if(USi>0)then
         FluxConst    = USi * PSi_I(nPoint)/&
              ((Z*TeSiIn + TiSiIn)*PoyntingFluxPerBSi*&
              BoundaryThreads_B(iBlock)% B_III(0,j,k)*No2Si_V(UnitB_))
      elseif(USi<0)then
         FluxConst    = USi * PeSiIn/&
              (TeSiIn*PoyntingFluxPerBSi*&
              BoundaryThreads_B(iBlock)% B_III(0,j,k)*No2Si_V(UnitB_))
      end if
      ! 5/2*U*Pi*(Z+1)/Ti
      EnthalpyFlux = FluxConst*(InvGammaMinus1 +1)*(1 + Z)
      ! Calculate flux to TR and its temperature derivative
      call interpolate_lookup_table(iTableTR, TeSi_I(1), 0.0, &
           Value_V=TrTable_V, DoExtrapolate=.false.)

      do iIter = 1,nIterHere
         ! Iterations
         ! Shape the source.
         call get_heat_cond
         ! Add enthalpy correction
         ! Limit enthalpy flux at the TR:
         if(FluxConst/=0.0)EnthalpyFlux = sign(min(abs(EnthalpyFlux),&
              0.50*HeatFlux2TR/TeSi_I(1)), FluxConst)
         ElectronEnthalpyFlux = EnthalpyFlux*Z/(1 + Z)
         if(USi > 0)then
            ResEnthalpy_I(2:nPoint-1) = &
                 ElectronEnthalpyFlux*(TeSi_I(1:nPoint-2) - TeSi_I(2:nPoint-1))
            ResEnthalpy_I(1)   = 0.0
            Lower_VVI(Cons_,Cons_,2:nPoint-1) &
                 = Lower_VVI(Cons_,Cons_,2:nPoint-1)&
                 - ElectronEnthalpyFlux*TeSi_I(2:nPoint-1) &
                 /(3.5*Cons_I(2:nPoint-1))
            Main_VVI(Cons_,Cons_,2:nPoint-1) &
                 = Main_VVI(Cons_,Cons_,2:nPoint-1)&
                 + ElectronEnthalpyFlux*TeSi_I(2:nPoint-1) &
                 /(3.5*Cons_I(2:nPoint-1))
         elseif(USi < 0)then
            ResEnthalpy_I(1:nPoint-1) = &
                 - ElectronEnthalpyFlux*(TeSi_I(2:nPoint) - TeSi_I(1:nPoint-1))
            Upper_VVI(Cons_,Cons_,1:nPoint-1) &
                 = Upper_VVI(Cons_,Cons_,1:nPoint-1)&
                 + ElectronEnthalpyFlux*TeSi_I(1:nPoint-1) &
                 /(3.5*Cons_I(1:nPoint-1))
            Main_VVI(Cons_,Cons_,1:nPoint-1) &
                 = Main_VVI(Cons_,Cons_,1:nPoint-1)&
                 - ElectronEnthalpyFlux*TeSi_I(1:nPoint-1) &
                 /(3.5*Cons_I(1:nPoint-1))
         end if

         ! Add Gravity Source

         ! cGravPot = cGravitation*mSun*cAtomicMass/(cBoltzmann*rSun)
         ! GravHydroDyn = cGravPot*MassIon_I(1)/Z
         ! energy flux needed to raise the mass flux rho*u to the
         ! heliocentric distance r:
         !    rho*u*G*Msun*(1/R_sun -1/r)=
         !     =k_B*N_i*M_i(amu)*u*cGravPot*(1-R_sun/r)=
         !     =P_e/T_e*cGravPot*(M_ion[amu]/Z)*u*(1/R_sun -1/r)

         ResGravity_I(2:nPoint-1) = 0.5*GravHydroDyn*FluxConst*(     &
              - BoundaryThreads_B(iBlock)%RInv_III(1-nPoint:-2,j,k)  &
              + BoundaryThreads_B(iBlock)%RInv_III(3-nPoint: 0,j,k))
         ResGravity_I(1)          = 0.5*GravHydroDyn*FluxConst*(     &
              - BoundaryThreads_B(iBlock)%RInv_III(1-nPoint,j,k)     &
              + BoundaryThreads_B(iBlock)%RInv_III(2-nPoint,j,k))
         ResEnthalpy_I(1:nPoint-1) = ResEnthalpy_I(1:nPoint-1) + &
              0.5*ResGravity_I(1:nPoint-1)

         ! For the time accurate mode, account for the time derivative
         ! The contribution to the Jacobian be modified although the specific
         ! heat should not,  because the conservative variable is
         ! flux-by-length, not the temperature
         Main_VVI(Cons_,Cons_,1:nPoint-1) &
              = Main_VVI(Cons_,Cons_,1:nPoint-1) + &
              DtInv*SpecHeat_I(1:nPoint-1)*TeSi_I(1:nPoint-1)/   &
              (3.5*Cons_I(1:nPoint-1))
         Main_VVI(Ti_,Ti_,1:nPoint-1) = Main_VVI(Ti_,Ti_,1:nPoint-1) + &
              DtInv*SpecIonHeat_I(1:nPoint-1)
         PressureTRCoef = sqrt(max(&
              1 - EnthalpyFlux*TeSi_I(1)/HeatFlux2TR,1.0e-8))
         Res_VI(Cons_,1:nPoint-1) = -DeltaEnergy_I(1:nPoint-1) +      &
              ResHeating_I(1:nPoint-1)*QeRatio +  ResCooling_I(1:nPoint-1) +&
              ResEnthalpy_I(1:nPoint-1) + ResHeatCond_I(1:nPoint-1) +&
              ExchangeRate_I(1:nPoint-1)*&
              (TiSi_I(1:nPoint-1) - TeSi_I(1:nPoint-1))
         Main_VVI(Cons_,Ti_,1:nPoint-1) = Main_VVI(Cons_,Ti_,1:nPoint-1) -&
              ExchangeRate_I(1:nPoint-1)
         Main_VVI(Cons_,Cons_,1:nPoint-1) = Main_VVI(Cons_,Cons_,1:nPoint-1) +&
              ExchangeRate_I(1:nPoint-1)*TeSi_I(1:nPoint-1)/&
              (3.5*Cons_I(1:nPoint-1))
         DCons_VI = 0.0
         IonEnthalpyFlux = ElectronEnthalpyFlux/Z
         ResEnthalpy_I=0.0
         if(USi>0)then
            ResEnthalpy_I(2:nPoint-1) = &
                 IonEnthalpyFlux*(TiSi_I(1:nPoint-2) - TiSi_I(2:nPoint-1))
            ResEnthalpy_I(1)   = 0.0
            Lower_VVI(Ti_,Ti_,2:nPoint-1) =  Lower_VVI(Ti_,Ti_,2:nPoint-1)&
                 - IonEnthalpyFlux
            Main_VVI(Ti_,Ti_,2:nPoint-1) =  Main_VVI(Ti_,Ti_,2:nPoint-1)&
                 + IonEnthalpyFlux
         elseif(USi<0)then
            ResEnthalpy_I(1:nPoint-1) = &
                 - IonEnthalpyFlux*(TiSi_I(2:nPoint) - TiSi_I(1:nPoint-1))
            Upper_VVI(Ti_,Ti_,1:nPoint-1) = Upper_VVI(Ti_,Ti_,1:nPoint-1)&
                 + IonEnthalpyFlux
            Main_VVI(Ti_,Ti_,1:nPoint-1) = Main_VVI(Ti_,Ti_,1:nPoint-1)&
                 - IonEnthalpyFlux
         end if
         ResEnthalpy_I(1:nPoint-1) = ResEnthalpy_I(1:nPoint-1) + &
              0.5*ResGravity_I(1:nPoint-1)
         Res_VI(Ti_,1:nPoint-1) = -DeltaIonEnergy_I(1:nPoint-1) +      &
              ResHeating_I(1:nPoint-1)*(1-QeRatio) + ResEnthalpy_I(1:nPoint-1)&
              + ExchangeRate_I(1:nPoint-1)*&
              (TeSi_I(1:nPoint-1) - TiSi_I(1:nPoint-1))
         Main_VVI(Ti_,Ti_,1:nPoint-1) = Main_VVI(Ti_,Ti_,1:nPoint-1) +&
              ExchangeRate_I(1:nPoint-1)
         Main_VVI(Ti_,Cons_,1:nPoint-1) = Main_VVI(Ti_,Cons_,1:nPoint-1) -&
              ExchangeRate_I(1:nPoint-1)*TeSi_I(1:nPoint-1)/&
              (3.5*Cons_I(1:nPoint-1))
         call tridiag_block33(n=nPoint-1,  &
              Lower_VVI=Lower_VVI(:,:,1:nPoint-1),&
              Main_VVI=Main_VVI(:,:,1:nPoint-1),&
              Upper_VVI=Upper_VVI(:,:,1:nPoint-1),&
              Res_VI=Res_VI(:,1:nPoint-1),  &
              Weight_VI=DCons_VI(:,1:nPoint-1))
         ! limit DeltaCons
         DCons_VI(Cons_,1:nPoint-1) = max(min(DCons_VI(Cons_, 1:nPoint-1), &
              ConsMax - Cons_I(    1:nPoint-1),        Cons_I(1:nPoint-1)),&
              ConsMin - Cons_I(    1:nPoint-1),   -0.5*Cons_I(1:nPoint-1))
         DCons_VI(Ti_,1:nPoint-1) = max(min(DCons_VI(Ti_, 1:nPoint-1), &
              TeSiMax - TiSi_I(    1:nPoint-1),        TiSi_I(1:nPoint-1)),&
              TeSiMin - TiSi_I(    1:nPoint-1),   -0.5*TiSi_I(1:nPoint-1))
         ! Apply DeltaCons
         Cons_I(1:nPoint-1) = Cons_I(1:nPoint-1) + DCons_VI(Cons_,1:nPoint-1)
         ! Recover temperature
         TeSi_I(1:nPoint-1) = &
              (3.5*Cons_I(1:nPoint-1)/HeatCondParSi)**cTwoSevenths
         TiSi_I(1:nPoint-1) = TiSi_I(1:nPoint-1) + DCons_VI(Ti_,1:nPoint-1)
         ! Eliminate jump in ion temperature, to avoid an unphysical
         ! jump in the alfven speed resulting in peak reflection
         if(FluxConst>0.0)TiSi_I(nPoint) = TiSi_I(nPoint-1)
         ! Change in the internal energy (to correct the energy source
         ! for the time-accurate mode):
         DeltaEnergy_I(1:nPoint-1) = DtInv*SpecHeat_I(1:nPoint-1)* &
              (TeSi_I(1:nPoint-1) - TeSiStart_I(1:nPoint-1))
         DeltaIonEnergy_I(1:nPoint-1) = DtInv*SpecIonHeat_I(1:nPoint-1)* &
              (TiSi_I(1:nPoint-1) - TiSiStart_I(1:nPoint-1))
         ! Calculate TR pressure
         ! For next iteration calculate TR heat flux and
         ! its temperature derivative
         call interpolate_lookup_table(iTableTR, TeSi_I(1), 0.0, &
              Value_V=TrTable_V, DoExtrapolate=.false.)
         ! Set pressure for updated temperature
         TrTable_V(PAvrL_) = TrTable_V(PAvrL_)*PressureTRCoef
         call set_pressure
         call get_res_heating(nIterIn=nIterHere)
         ExchangeRate_I(1:nPoint-1) = cExchangeRateSi*InvGammaMinus1*Z**3*&
              BoundaryThreads_B(iBlock)%DsCellOverBSi_III(1-nPoint:-1,j,k)* &
              PSi_I(1:nPoint-1)**2/(&
              (Z*TeSi_I(1:nPoint-1) + TiSi_I(1:nPoint-1))**2*&
              TeSi_I(1:nPoint-1)**1.5)
         if(all(abs(DCons_VI(Cons_,1:nPoint-1))<cTol*Cons_I(1:nPoint-1)))EXIT
      end do
      if(any(abs(DCons_VI(Cons_,1:nPoint-1))>cTol*Cons_I(1:nPoint-1))&
           .and.DoCheckConvHere)then
         write(*,'(a)')'Te TeMin PSi_I Heating Enthalpy'
         do iPoint=1,nPoint
            write(*,'(i4,5es15.6)')iPoint, TeSi_I(iPoint),&
                 BoundaryThreads_B(iBlock)%TGrav_III(iPoint-nPoint,j,k),&
                 PSi_I(iPoint),ResHeating_I(iPoint), ResEnthalpy_I(iPoint)
         end do
         write(*,'(a,es15.6,a,3es15.6)')'Error =',maxval(&
              abs(DCons_VI(Cons_,1:nPoint-1)/Cons_I(1:nPoint-1))),&
              ' at the point Xyz=',Xyz_DGB(:,1,j,k,iBlock)
         write(*,'(a,5es15.6)')&
              'Input parameters: TeSiIn,USiIn,USi,PeSiIn,PCoef=',&
              TeSiIn,USiIn,USi,PeSiIn,PressureTRCoef
         call stop_mpi('Algorithm failure in advance_thread')
      end if

    end subroutine advance_thread
    !==========================================================================
    subroutine get_res_heating(nIterIn)

      use ModTurbulence, ONLY: rMinWaveReflection

      integer, intent(in)::nIterIn
      integer:: iPoint
      real    :: SqrtRho, RhoNoDim
      ! 11.12.20 Routine is backward compatible with the version of 10.30.19
      !------------------------------------------------------------------------
      ! Now prepare to calculate Alfven wave amplitude distribution
      Xi_I(0) = 0.0
      do iPoint=1,nPoint
         ! 1. Calculate sqrt(RhoNoDim)
         RhoNoDim = RhoNoDimCoef*PSi_I(iPoint)/&
              (Z*TeSi_I(iPoint) + TiSi_I(iPoint))
         if(RhoNoDim<=0.0)then
            write(*,*)'iPoint=',iPoint,' PSi_I(iPoint)=',&
                 PSi_I(iPoint),' TeSi_I(iPoint)=',TeSi_I(iPoint),&
                 ' TiSi_I(iPoint)=',TiSi_I(iPoint)
            call stop_mpi('Non-positive Density in '//NameSub)
         end if
         SqrtRho = sqrt(RhoNoDim)
         ! 2. Calculate Alfven wave speed
         VaLog_I(iPoint) = &
              log(BoundaryThreads_B(iBlock)% B_III(iPoint-nPoint,j,k)/SqrtRho)
         ! 3. Calculate dimensionless length (in terms of dissipation length)
         DXi_I(iPoint) = &
              (BoundaryThreads_B(iBlock)% Xi_III(iPoint-nPoint,j,k) - &
              BoundaryThreads_B(iBlock)% Xi_III(iPoint-1-nPoint,j,k))*&
              sqrt(SqrtRho)
         Xi_I(iPoint) = Xi_I(iPoint-1) + DXi_I(iPoint)
      end do
      if(rMinWaveReflection*BoundaryThreads_B(iBlock)%RInv_III(0,j,k) > 1.0 &
           )then
         !
         ! All thread points are below rMinWaveReflection
         !
         ReflCoef_I(0:nPoint) = 0
      else
         ! Calculate the reflection coefficient
         ReflCoef_I(1) = abs(VaLog_I(2) - VaLog_I(1))/&
              (0.50*DXi_I(2) + DXi_I(1))
         do iPoint = 2, nPoint - 2
            ReflCoef_i(iPoint) = &
                 abs(VaLog_I(iPoint+1) - VaLog_I(iPoint))/&
                 (0.50*(DXi_I(iPoint+1) +  DXi_I(iPoint)))
         end do
         ReflCoef_I(nPoint-1) = abs(VaLog_I(nPoint) - VaLog_I(nPoint-1))/&
              (0.50*DXi_I(nPoint-1) + DXi_I(nPoint))
         !  Some thread points may be below rMinWaveReflection
         if(rMinWaveReflection > 1.0)&
              ReflCoef_I(1:nPoint-1) = ReflCoef_I(1:nPoint-1)*0.5*&
              (1 + tanh(50*(1 - rMinWaveReflection*&
              BoundaryThreads_B(iBlock)%RInv_III(2-nPoint:0,j,k))))
         ReflCoef_I(0)      =  ReflCoef_I(1)
         ReflCoef_I(nPoint) =  ReflCoef_I(nPoint-1)
      end if
      call solve_a_plus_minus(&
           nI=nPoint,                      &
           ReflCoef_I=ReflCoef_I(0:nPoint),&
           Xi_I=Xi_I(0:nPoint),            &
           AMinorBC=AMinorIn,              &
           nIterIn=nIterIn,                &
           DoCheck=DoCheckConvHere)
      ResHeating_I = 0.0
      ResHeating_I(1:nPoint-1) = &
           (AMajor_I(0:nPoint-2)**2 - AMinor_I(0:nPoint-2)**2)&
           -(AMajor_I(1:nPoint-1  )**2 - AMinor_I(1:nPoint-1)**2)
    end subroutine get_res_heating
    !==========================================================================
    subroutine get_heat_cond
      !------------------------------------------------------------------------
      Main_VVI = 0.0; ResHeatCond_I = 0.0; Upper_VVI = 0.0; Lower_VVI = 0.0

      ! Contribution from heat conduction fluxes
      ! Flux linearizations over small dCons
      ! Dimensionless flux= dCons/ds(1/( (PoyntingFlux/B)B)
      Upper_VVI(Cons_,Cons_,1:nPoint-1) = &
           -BoundaryThreads_B(iBlock)% BDsFaceInvSi_III(1-nPoint:-1,j,k)
      Lower_VVI(Cons_,Cons_,2:nPoint-1) = Upper_VVI(Cons_,Cons_,1:nPoint-2)
      Main_VVI(Cons_,Cons_,2:nPoint-1) = &
           - Upper_VVI(Cons_,Cons_,2:nPoint-1) &
           - Lower_VVI(Cons_,Cons_,2:nPoint-1)
      ! Right heat fluxes
      ResHeatCond_I(1:nPoint-1) = &
           (Cons_I(1:nPoint-1) - Cons_I(2:nPoint)) &
           *Upper_VVI(Cons_,Cons_,1:nPoint-1)
      ! Add left heat flux to the TR
      HeatFlux2TR = TrTable_V(HeatFluxL_)*&
           BoundaryThreads_B(iBlock)% BDsFaceInvSi_III(-nPoint,j,k)
      ResHeatCond_I(1) = ResHeatCond_I(1) -  HeatFlux2TR
      ! Linearize left heat flux to the TR

      Main_VVI(Cons_,Cons_,1) &
           = -Upper_VVI(Cons_,Cons_,1) + TrTable_V(DHeatFluxLoverDcons_)  &
           *BoundaryThreads_B(iBlock)%BDsFaceInvSi_III(-nPoint,j,k)
      ! Add other left heat fluxes
      ResHeatCond_I(2:nPoint-1) = ResHeatCond_I(2:nPoint-1) + &
           (Cons_I(2:nPoint-1) - Cons_I(1:nPoint-2))*&
           Lower_VVI(Cons_,Cons_,2:nPoint-1)
      ! LogP_ terms
      Main_VVI(LogP_,LogP_,1:nPoint-1) =  1.0

      ! 1. At the TR:
      ! We satisfy the equation, d(LogP) = d(Cons)*(dPAvr/dCons)_{TR}/PAvr
      Main_VVI(LogP_,Cons_,1) = -1/&
           TrTable_V(HeatFluxL_) + TiSi_I(1)/&
           (3.5*Cons_I(1)*(Z*TeSi_I(1) + TiSi_I(1)))
      Main_VVI(LogP_,Ti_,1)   = -1/(Z*TeSi_I(1) + TiSi_I(1))

      ! 2:
      ! For other points we satisfy the hydrostatic equilibrium condition
      ! LogPe^{i-1}=LogPe^i+TGrav/Te^i
      ! dLogPe^i - dCons^i(TGrav/(Te^i)^2)*dTe/dCons -dLogPe^{i-1}=0
      Main_VVI(LogP_,Cons_,2:nPoint-1) = -(Z + 1)*Z*&
           BoundaryThreads_B(iBlock)% TGrav_III(2-nPoint:-1,j,k)/&
           (Z*TeSi_I(2:nPoint-1) + TiSi_I(2:nPoint-1))**2*&
           TeSi_I(2:nPoint-1)/(3.5*Cons_I(2:nPoint-1))
      Main_VVI(LogP_,Ti_,2:nPoint-1)   = -(Z + 1)*&
           BoundaryThreads_B(iBlock)% TGrav_III(2-nPoint:-1,j,k)/&
           (Z*TeSi_I(2:nPoint-1) + TiSi_I(2:nPoint-1))**2

      Lower_VVI(LogP_,LogP_,2:nPoint-1) = -1.0

      ! Cooling
      ! 1. Source term:
      call get_cooling(nLast=nPoint-1)

      ! 2. Source term derivatives:
      Main_VVI(Cons_,LogP_,1:nPoint-1) = &
           -2*ResCooling_I(1:nPoint-1) !=-dCooling/dLogPe
      Main_VVI(Cons_,Cons_,1:nPoint-1) = Main_VVI(Cons_,Cons_,1:nPoint-1) + &
           (-DResCoolingOverDLogT_I(1:nPoint-1) + 2*ResCooling_I(1:nPoint-1)/&
           (Z*TeSi_I(1:nPoint-1) + TiSi_I(1:nPoint-1))*Z*TeSi_I(1:nPoint-1))/&
           (3.5*Cons_I(1:nPoint-1))   !=-dCooling/dCons
      Main_VVI(Cons_,Ti_,1:nPoint-1) = Main_VVI(Cons_,Ti_,1:nPoint-1) + &
           2*ResCooling_I(1:nPoint-1)/&
           (Z*TeSi_I(1:nPoint-1) + TiSi_I(1:nPoint-1))   !=-dCooling/d log Ti

    end subroutine get_heat_cond
    !==========================================================================
    subroutine get_cooling(nLast)

      integer,intent(in) ::nLast
      integer ::  iPoint
      !------------------------------------------------------------------------
      ResCooling_I = 0.0;
      do iPoint = 1, nLast
         if(TeSi_I(iPoint)>1.0e8)then
            write(*,*)'Failure in heat conduction setting'
            write(*,*)'In the point Xyz=',Xyz_DGB(:,1,j,k,iBlock)
            write(*,*)'TeSiIn, PeSiIn = ', TeSiIn, PeSiIn
            write(*,*)'TeSi_I=',TeSi_I(1:nPoint)
            call stop_mpi('Stop!!!')
         end if
         call interpolate_lookup_table(iTableTR, TeSi_I(iPoint), 0.0, &
              TrTable_V, DoExtrapolate=.false.)
         ResCooling_I(iPoint) = &
              -BoundaryThreads_B(iBlock)%DsCellOverBSi_III(iPoint-nPoint,j,k)&
              *TrTable_V(Lambda_)*Z*&
              (PSi_I(iPoint)/(Z*TeSi_I(iPoint) + TiSi_I(iPoint)))**2
         DResCoolingOverDLogT_I(iPoint) = &
              ResCooling_I(iPoint)*TrTable_V(DLogLambdaOverDLogT_)
      end do

    end subroutine get_cooling
    !==========================================================================
  end subroutine solve_boundary_thread
  !============================================================================
  subroutine tridiag_block33(n,Lower_VVI,Main_VVI,Upper_VVI,Res_VI,Weight_VI)
    use ModCoordTransform, ONLY: determinant, inverse_matrix
    ! This routine solves three-diagonal system of equations:
    !
    !  ||m_1 u_1  0....        || ||w_1|| ||r_1||
    !  ||l_2 m_2 u_2...        || ||w_2|| ||r_2||
    !  || 0  l_3 m_3 u_3       ||.||w_3||=||r_3||
    !  ||...                   || ||...|| ||...||
    !  ||.............0 l_n m_n|| ||w_n|| ||r_n||
    !
    ! Prototype: Numerical Recipes, Chapter 2.6, p.40.
    ! Here each of the components w_i and r_i are 3-component vectors and
    ! m_i, l_i, u_i are 3*3 matrices                                       !

    integer, intent(in):: n
    real, intent(in):: &
         Lower_VVI(3,3,n), Main_VVI(3,3,n), Upper_VVI(3,3,n),Res_VI(3,n)
    real, intent(out):: &
         Weight_VI(3,n)

    integer:: j
    real   :: TildeM_VV(3,3), TildeMInv_VV(3,3), TildeMInvDotU_VVI(3,3,2:n)

    ! If tilde(M)+L.Inverted(\tilde(M))\dot.U = M, then the equation
    !      (M+L+U)W = R
    ! may be equivalently written as
    ! (tilde(M) +L).(I + Inverted(\tilde(M)).U).W=R
    ! 11.12.2020 - This version is backward compatible to 10.30.2019

    character(len=*), parameter:: NameSub = 'tridiag_block33'
    !--------------------------------------------------------------------------
    if (determinant(Main_VVI(:,:,1)) == 0.0) then
       call stop_mpi('Error in tridiag: M_I(1)=0')
    end if
    TildeM_VV = Main_VVI(:,:,1)
    TildeMInv_VV = inverse_matrix(TildeM_VV,DoIgnoreSingular=.true.)
    ! First 3-vector element of the vector, Inverted(tilde(M) + L).R
    Weight_VI(:,1) = matmul(TildeMInv_VV,Res_VI(:,1))
    do j=2,n
       ! Next 3*3 blok element of the matrix, Inverted(Tilde(M)).U
       TildeMInvDotU_VVI(:,:,j) = matmul(TildeMInv_VV,Upper_VVI(:,:,j-1))
       ! Next 3*3 block element of matrix tilde(M), obeying the eq.
       ! tilde(M)+L.Inverted(\tilde(M))\dot.U = M
       TildeM_VV = Main_VVI(:,:,j) - &
            matmul(Lower_VVI(:,:,j), TildeMInvDotU_VVI(:,:,j))
       if (determinant(TildeM_VV) == 0.0) then
          write(*,*)'j, M_I(j), L_I(j), TildeMInvDotU_I(j) = ',j, &
               Main_VVI(:,:,j), Lower_VVI(:,:,j), TildeMInvDotU_VVI(:,:,j)
          call stop_mpi('3*3 block Tridiag failed')
       end if
       ! Next element of inverted(Tilde(M))
       TildeMInv_VV = inverse_matrix(TildeM_VV,DoIgnoreSingular=.true.)
       ! Next 2-vector element of the vector, Inverted(tilde(M) + L).R
       ! satisfying the eq. (tilde(M) + L).W = R
       Weight_VI(:,j) = matmul(TildeMInv_VV,Res_VI(:,j) - &
            matmul(Lower_VVI(:,:,j),Weight_VI(:,j-1)))
    end do
    do j=n-1,1,-1
       ! Finally we solve equation
       ! (I + Inverted(Tilde(M)).U).W =  Inverted(tilde(M) + L).R
       Weight_VI(:,j) = Weight_VI(:,j) &
            - matmul(TildeMInvDotU_VVI(:,:,j+1),Weight_VI(:,j+1))
    end do

  end subroutine tridiag_block33
  !============================================================================
  subroutine solve_a_plus_minus(nI, ReflCoef_I, Xi_I, AMinorBC, &
       nIterIn, DoCheck)

    integer,         intent(in):: nI
    real,            intent(in):: ReflCoef_I(0:nI), Xi_I(0:nI)
    real,            intent(in):: AMinorBC         ! BC for A-
    integer,optional,intent(in):: nIterIn
    logical, intent(in) :: DoCheck

    real:: DeltaXi
    integer::iStep,iIter
    integer, parameter:: nIterMax = 10
    integer:: nIter
    real::Derivative, AOld, ADiffMax, AP, AM, APMid, AMMid
    character(len=*), parameter:: NameSub = 'solve_a_plus_minus'
    !--------------------------------------------------------------------------
    AMajor_I(0) = 1.0
    AMinor_I(nI)  = AMinorBC
    if(present(nIterIn))then
       nIter=nIterIn
    else
       nIter=nIterMax
    end if
    do iIter=1,nIter
       ! Go forward, integrate AMajor_I with given AMinor_I
       ADiffMax = 0.0
       do iStep=1, nI
          ! Predictor
          AP = AMajor_I(iStep-1); AM = AMinor_I(iStep-1)
          Derivative = derivative_major(AP, AM, &
               ReflCoef_I(iStep-1))
          AOld = AMajor_I(iStep)
          DeltaXi = Xi_I(iStep) - Xi_I(iStep-1)

          ! Corrector
          AMMid = 0.5*(AMinor_I(iStep-1) + AMinor_I(iStep))
          APMid = AP + 0.5*Derivative*DeltaXi
          Derivative = derivative_major(&
               APMid, AMMid, &
               0.50*(ReflCoef_I(iStep-1) + ReflCoef_I(iStep)))
          AMajor_I(iStep) = AP + Derivative*DeltaXi
          ADiffMax = max(ADiffMax, &
               abs(AOld - AMajor_I(iStep))/max(AOld,AMajor_I(iStep)))
       end do

       ! Go backward, integrate AMinor_I with given AMajor_I
       ! We integrate equation,
       ! 2da_-/d\xi=
       !=-[ max(1-2a_-/a_+,0)-max(1-2a_+/a_-,0)]* a_+ *
       ! *min(ReflCoef,2max(a_,a_+))-
       ! -2a_-a_+
       do iStep=nI - 1, 0, -1
          ! Predictor
          AP = AMajor_I(iStep+1); AM = AMinor_I(iStep+1)
          Derivative = derivative_minor(AP, AM, &
               ReflCoef_I(iStep+1))
          AOld = AMinor_I(iStep)
          DeltaXi = Xi_I(iStep+1) - Xi_I(iStep)

          ! Corrector
          APMid = 0.5*(AMajor_I(iStep+1) + AMajor_I(iStep))
          AMMid = AM - 0.5*Derivative*DeltaXi
          Derivative = derivative_minor(&
               APMid, AMMid, &
               0.50*(ReflCoef_I(iStep+1) + ReflCoef_I(iStep)))
          AMinor_I(iStep) = AMinor_I(iStep+1) - Derivative*DeltaXi
          ADiffMax = max(ADiffMax,&
               abs(AOld - AMinor_I(iStep))/max(AOld, AMinor_I(iStep)))
       end do
       if(ADiffMax<cTol)EXIT
       if(DoCheck.and.iIter==nIter)then
          write(*,*)'XiTot=', Xi_I(nI),' ADiffMax=', ADiffMax,&
               ' AMinorBC=',AMinorBC
          write(*,*)'iPoint AMajor TeSi TiSi PSi'
          do iStep = 1, nI
             write(*,*)iStep, AMajor_I(iStep), &
                  TeSi_I(iStep), TiSi_I(iStep), PSi_I(iStep)
          end do
          write(*,*)'iPoint AMinor DissipationMinus ReflCoef VaLog dXi'
          do iStep = 1, nI
             write(*,*)iStep, AMinor_I(iStep),  &
                  ReflCoef_I(iStep), VaLog_I(iStep), DXi_I(iStep)
          end do
          call stop_mpi('Did not reach convergence in solve_a_plus_minus')
       end if
    end do

  contains
    !==========================================================================
    real function derivative_major(AMajor, AMinor, Reflection)
      real, intent(in):: AMajor, AMinor, Reflection
      !------------------------------------------------------------------------
      derivative_major = -AMinor*&
           (max(0.0,AMajor - ImbalanceMax*AMinor)      &
           -max(0.0,AMinor - ImbalanceMax*AMajor)  )*  &
           min(0.5*Reflection/max(AMinor, AMajor), 1.0) &
           - AMinor*AMajor
    end function derivative_major
    !==========================================================================
    real function derivative_minor(AMajor, AMinor, Reflection)
      real, intent(in)         ::   AMajor, AMinor, Reflection
      !------------------------------------------------------------------------
      derivative_minor =  -AMajor*&
           (max(0.0, AMajor - ImbalanceMax*AMinor)      &
           -max(0.0, AMinor - ImbalanceMax*AMajor)  )*  &
           min(0.5*Reflection/max(AMinor, AMajor), 1.0) &
           + AMinor*AMajor

    end function derivative_minor
    !==========================================================================
  end subroutine solve_a_plus_minus
  !============================================================================
  subroutine set_field_line_thread_bc(nGhost, iBlock, nVarState, State_VG, &
       iImplBlock)

    use EEE_ModCommonVariables, ONLY: UseCme
    use ModFieldLineThread, ONLY: b_cme_d
    use ModMain, ONLY: nStep, nIteration, tSimulation
    use ModAdvance, ONLY: State_VGB
    use BATL_lib, ONLY:  MinI, MaxI, MinJ, MaxJ, MinK, MaxK, nJ, nK
    use ModPhysics, ONLY: No2Si_V, Si2No_V, UnitTemperature_, UnitRhoU_,&
         UnitEnergyDens_, UnitU_, UnitX_, UnitB_, InvGammaElectronMinus1
    use ModVarIndexes, ONLY: Rho_, p_, Bx_, Bz_, &
         RhoUx_, RhoUz_, EHot_, WDiff_, nVar
    use ModImplicit, ONLY: iTeImpl
    use ModB0, ONLY: B0_DGB
    use ModWaves, ONLY: WaveFirst_, WaveLast_, UseAwRepresentative
    use ModHeatFluxCollisionless, ONLY: UseHeatFluxCollisionless, &
         get_gamma_collisionless

    integer, intent(in):: nGhost
    integer, intent(in):: iBlock
    integer, intent(in):: nVarState
    real, intent(inout):: State_VG(nVarState,MinI:MaxI,MinJ:MaxJ,MinK:MaxK)

    ! Optional arguments when called by semi-implicit scheme
    integer, optional, intent(in):: iImplBlock
    ! Determines, which action should be done with the thread
    ! before setting the BC
    integer:: iAction

    integer :: i, j, k, iMajor, iMinor, j1, k1
    real :: TeSi, PeSi, BDir_D(3), U_D(3), U, B1_D(3), SqrtRho, DirR_D(3)
    real :: PeSiOut, AMinor, AMajor, DTeOverDsSi, DTeOverDs, GammaHere
    real :: TiSiIn, PiSiOut
    real :: RhoNoDimOut, UAbsMax
    ! CME parameters, if needed
    real:: RhoCme, Ucme_D(3), Bcme_D(3), pCme

    character(len=*), parameter:: NameSub = 'set_field_line_thread_bc'
    !--------------------------------------------------------------------------
    if(present(iImplBlock))then
       if(BoundaryThreads_B(iBlock)%iAction/=Done_)&
            call stop_mpi('Algorithm error in '//NameSub)
       iAction=Impl_
    else
       iAction=BoundaryThreads_B(iBlock)%iAction
    end if
    if(iAction==Done_)RETURN

    call timing_start('set_thread_bc')
    ! Start from floating boundary values
    do k = 1, nK; do j = 1, nJ; do i = 1 - nGhost, 0
       State_VG(:, i,j,k) = State_VG(:,1, j, k)
    end do; end do; end do

    ! Fill in the temperature array
    if(UseIdealEos)then
       do k = 1, nK; do j = 1, nJ
          Te_G(0:1,j,k) = TeFraction*State_VGB(iPe,1,j,k,iBlock) &
               /State_VGB(Rho_,1,j,k,iBlock)
       end do; end do
    else
       call stop_mpi('Generic EOS is not applicable with threads')
    end if
    do k = 1, nK; do j = 1, nJ
       B1_D = State_VGB(Bx_:Bz_,1,j,k,iBlock)
       BDir_D = B1_D + 0.50*(B0_DGB(:, 1, j, k, iBlock) + &
            B0_DGB(:, 0, j, k, iBlock))
       BDir_D = BDir_D/max(norm2(BDir_D), 1e-30)
       DirR_D = Xyz_DGB(:,1,j,k,iBlock)
       DirR_D = DirR_D/max(norm2(DirR_D), 1e-30)

       if(BoundaryThreads_B(iBlock) % SignB_II(j, k) <  0.0)then
          iMajor = WaveLast_
          iMinor = WaveFirst_
       else
          iMajor = WaveFirst_
          iMinor = WaveLast_
       end if
       if(sum(BDir_D*DirR_D) <  0.0)&
            BDir_D = -BDir_D
       ! Calculate input parameters for solving the thread
       Te_G(0, j, k) = max(TeMin, min(Te_G(0, j, k), &
            BoundaryThreads_B(iBlock) % TMax_II(j,k)))
       UAbsMax = 0.10*sqrt(Te_G(0,j,k))
       TeSi = Te_G(0, j, k)*No2Si_V(UnitTemperature_)
       SqrtRho = sqrt(State_VGB(Rho_, 1, j, k, iBlock))
       AMinor = min(1.0,&
            sqrt(  State_VGB(iMinor, 1, j, k, iBlock)/&
            (SqrtRho* PoyntingFluxPerB)  ))
       U_D = State_VGB(RhoUx_:RhoUz_, 1, j, k, iBlock)/&
            State_VGB(Rho_, 1, j, k, iBlock)
       U = sum(U_D*BDir_D)
       U = sign(min(abs(U), UAbsMax), U)

       PeSi = PeFraction*State_VGB(iPe, 1, j, k, iBlock)&
            *(Te_G(0,j,k)/Te_G(1,j,k))*No2Si_V(UnitEnergyDens_)
       TiSiIn = TiFraction*State_VGB(p_,1,j,k,iBlock) &
            /State_VGB(Rho_,1,j,k,iBlock)*No2Si_V(UnitTemperature_)
       call solve_boundary_thread(j=j, k=k, iBlock=iBlock, iAction=iAction,  &
            TeSiIn=TeSi, TiSiIn=TiSiIn, PeSiIn=PeSi, USiIn=U*No2Si_V(UnitU_),&
            AMinorIn=AMinor,DTeOverDsSiOut=DTeOverDsSi, &
            PeSiOut=PeSiOut, PiSiOut = PiSiOut, &
            RhoNoDimOut=RhoNoDimOut, AMajorOut=AMajor)
       if(present(iImplBlock))then
          DTeOverDs = DTeOverDsSi * Si2No_V(UnitTemperature_)/Si2No_V(UnitX_)
          ! Solve equation: -(TeGhost-TeTrue)/DeltaR =
          ! dTe/ds*(b . DirR)
          Te_G(0, j, k) = Te_G(0,j,k) - DTeOverDs/max(&
               sum(BDir_D*DirR_D),0.7)*&
               BoundaryThreads_B(iBlock)% DeltaR_II(j,k)
          ! Version Easter 2015 Limit TeGhost
          Te_G(0, j, k) = max(TeMin,min(Te_G(0,j,k), &
               BoundaryThreads_B(iBlock) % TMax_II(j,k)))
          State_VG(iTeImpl,0,j,k) = Te_G(0,j,k)
          CYCLE
       end if

       State_VG(iPe,0,j,k) = PeSiOut*Si2No_V(UnitEnergyDens_)/PeFraction
       ! Extrapolation of pressure
       State_VG(iPe, 1-nGhost:-1,j,k) = State_VG(iPe,0,j,k)**2/&
            State_VG(iPe,1,j,k)
       ! Assign ion pressure (if separate from electron one)
       if(iPe/=p_)then
          State_VG(p_,0,j,k) = PiSiOut*Si2No_V(UnitEnergyDens_)
          State_VG(p_,1-nGhost:-1,j,k) = State_VG(p_,0,j,k)**2/&
               State_VG(p_,1,j,k)
       end if

       State_VG(Rho_, 0, j, k) = RhoNoDimOut
       ! Extrapolation of density
       State_VG(Rho_, 1-nGhost:-1, j, k) = State_VG(Rho_, 0, j, k)**2&
            /State_VG(Rho_,1,j,k)

       do i = 1-nGhost, 0
          B1_D = State_VG(Bx_:Bz_, 1-i, j, k)
          ! Horizontal magnetic field in the physical cell equals
          ! B1_D - DirR_D*sum(DirR_D*B1_D)
          State_VG(Bx_:Bz_, i, j, k) = B1_D - DirR_D*sum(DirR_D*B1_D)
          ! CME vertical field equals:
          ! CME_vertical = sum(DirR_D*b_cme_d(Xyz_DGB(:,i,j,k,iBlock)))
          ! Horizontal CME magnetic field equals
          ! CME_horizontal = b_cme_d - DirR_D*CME_vertical
          ! Jump in horizontal MF is equal to
          ! Horizontal field in the physical cell - CME_horizontal
          if(UseCME)State_VG(Bx_:Bz_, i, j, k) = &
               State_VG(Bx_:Bz_, i, j, k) + DirR_D*sum(DirR_D*&
               b_cme_d(Xyz_DGB(:,i,j,k,iBlock)))
          ! State_VG(Bx_:Bz_, i, j, k) = b_cme_d(Xyz_DGB(:,i,j,k,iBlock))

          ! Gnost cell value of velocity: keep the velocity projection
          ! onto the magnetic field, if UseAlignedVelocity=.true.
          ! Reflect the other components.
          U_D = State_VG(RhoUx_:RhoUz_,1-i,j,k)/State_VG(Rho_,1-i,j,k)

          if(UseAlignedVelocity)then
             U   = sum(U_D*BDir_D); U_D = U_D - U*BDir_D
             U   = sign(min(abs(U), UAbsMax), U)
          else
             U = 0
          end if
          State_VG(RhoUx_:RhoUz_, i, j, k) = -U_D*State_VG(Rho_,i,j,k) &
               + U*BDir_D*State_VG(Rho_,i,j,k)

          State_VG(iMajor, i, j, k) = AMajor**2 * PoyntingFluxPerB *&
               sqrt( State_VG(Rho_, i, j, k) )
          if(WDiff_>1)State_VG(WDiff_, i, j, k) = 0.0
       end do

       if(Ehot_ > 1)then
          if(UseHeatFluxCollisionless)then
             call get_gamma_collisionless(Xyz_DGB(:,1,j,k,iBlock), GammaHere)
             State_VG(Ehot_,1-nGhost:0,j,k) = &
                  State_VG(iPe,1-nGhost:0,j,k)*&
                  (1.0/(GammaHere - 1) - InvGammaElectronMinus1)
          else
             State_VG(Ehot_,1-nGhost:0,j,k) = 0.0
          end if
       end if
    end do; end do
    BoundaryThreads_B(iBlock)%iAction = Done_
    call timing_stop('set_thread_bc')

  end subroutine set_field_line_thread_bc
  !============================================================================
end module ModThreadedLC
!==============================================================================
