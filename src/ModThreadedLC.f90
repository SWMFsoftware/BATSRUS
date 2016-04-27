!  Copyright (C) 2002 Regents of the University of Michigan, 
!  portions used with permission 
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModThreadedLC
  use ModFieldLineThread, ONLY: &
       BoundaryThreads, BoundaryThreads_B, &
       LengthPAvrSi_, UHeat_, HeatFluxLength_, DHeatFluxXOverU_, &
       LambdaSi_, DLogLambdaOverLogT_,                           &
       DoInit_, Done_, Enthalpy_, Heat_, iStage
  use ModCoronalHeating, ONLY:PoyntingFluxPerBSi, PoyntingFluxPerB
  use ModAdvance,    ONLY: UseElectronPressure, UseIdealEos
  use ModPhysics,    ONLY: Z => AverageIonCharge
  use ModConst,         ONLY: rSun, mSun, cBoltzmann, cAtomicMass, cGravitation
  use ModGeometry,   ONLY: Xyz_DGB
  !\
  !   Hydrostatic equilibrium in an isothermal corona: 
  !    d(N_i*k_B*(Z*T_e +T_i) )/dr=G*M_sun*N_I*M_i*d(1/r)/dr
  ! => N_i*Te\propto exp(cGravPot/TeSi*(M_i[amu]/(1+Z))*\Delta(R_sun/r)) 
  !/
  !\
  ! The plasma properties dependent coefficient needed to evaluate the 
  ! eefect of gravity on the hydrostatic equilibrium
  !/
  use ModFieldLineThread, ONLY:  &
       GravHydroStat != cGravPot*MassIon_I(1)/(Z + 1) 
  implicit none
  !\
  !energy flux needed to raise the mass flux rho*u to the heliocentric 
  !distance r equals: rho*u*G*Msun*(1/R_sun -1/r)=
  !=k_B*N_i*M_i(amu)u*cGravPot*(1-R_sun/r)=
  !=P_e/T_e*cGravPot*u(M_i[amu]/Z)*(1/R_sun -1/r)
  !/
  real :: GravHydroDyn ! = cGravPot*MassIon_I(1)/AverageIonCharge
  !\
  ! To expsress Te in terms of P and rho.
  !/
  real    :: TeFraction, TiFraction, PeFraction
  integer :: iP
  !\
  ! Temperature 3D array
  !/
  real,allocatable :: Te_G(:,:,:)
  !\
  ! Arrays for 1D distributions
  !/
  real,allocatable,dimension(:)::ReflCoef_I, APlus_I, AMinus_I, &
       TeSi_I, TeSiOld_I, TeSiStart_I, PeSi_I, Xi_I, Cons_I, &
       VaLog_I, DXi_I, ResHeating_I, ResCooling_I, DResCoolingOverDT_I, &
       ResEnthalpy_I, ResHeatCond_I, ResGravity_I, SpecHeat_I, DeltaEnergy_I
  !\
  ! We apply ADI to solve state vector, the components of the state
  ! being temperature and log pressure. 
  ! The heating at constant pressure is characterized by the
  ! specific heat at constant pressure. For pressure the barometric
  ! formula is applied
  !/
  integer, parameter:: Cons_ = 1, LogP_ = 2
  real, allocatable, dimension(:,:):: Res_VI, DCons_VI
  real, allocatable, dimension(:,:,:) :: M_VVI, L_VVI, U_VVI
  
  !\
  ! Table numbers needed to use lookup table
  !/ 
  integer :: iTableTR
  !\
  !Control parameters: minimum temerature  and two logicals with 
  !self-explained names
  !/
  real, parameter:: TeSiMin = 5.0e4
  logical        :: UseAlignedVelocity = .true.
  logical        :: DoConvergenceCheck = .false.
  !\
  ! Two parameters controling the choise of the order for density and
  ! pressure: first order (LimMin=LimMax=0), second order (LimMin=LimMax=1)
  ! or limited second order as a default
  !/
  real:: LimMin = 0.0, LimMax = 1
  !\
  ! Coefficient to express dimensionless density as RhoNoDimCoef*PeSi/TeSi
  !/
  real           ::  RhoNoDimCoef 
  !\
  ! Misc
  !/
  real:: TeMin, ConsMin, ConsMax, TeSiMax
  !\
  ! For transforming conservative to TeSi and back 
  !/
  real, parameter:: cTwoSevenths = 2.0/7.0
  !\
  ! Gravitation potential in K
  !/
  real, parameter:: cGravPot = cGravitation*mSun*cAtomicMass/&
       (cBoltzmann*rSun)
  !\
  ! See above for the use of the latter constant
  !/

  real :: SqrtZ
  integer, parameter:: Impl_=3

  real,parameter:: cTol=1.0e-6
contains
  !=========================================================================
  subroutine init_threaded_lc
    use BATL_lib, ONLY:  MinI, MaxI, MinJ, MaxJ, MinK, MaxK
    use ModLookupTable,  ONLY: i_lookup_table
    use ModMultiFluid,   ONLY: MassIon_I
    use ModFieldLineThread, ONLY: check_tr_table, & 
         nPointThreadMax, HeatCondParSi
    use ModPhysics,            ONLY: &
         UnitTemperature_, Si2No_V, UnitEnergyDens_
    use ModVarIndexes,         ONLY: Pe_, p_
    !-------------------
    allocate(Te_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)); Te_G = 0.0

    allocate(ReflCoef_I(0:nPointThreadMax)); ReflCoef_I = 0.0
    allocate(   APlus_I(0:nPointThreadMax));    APlus_I = 0.0
    allocate(  AMinus_I(0:nPointThreadMax));   AMinus_I = 0.0

    allocate(   TeSi_I(nPointThreadMax));     TeSi_I = 0.0
    allocate(TeSiOld_I(nPointThreadMax));  TeSiOld_I = 0.0
    allocate(TeSiStart_I(nPointThreadMax));  TeSiStart_I = 0.0
    allocate(   Cons_I(nPointThreadMax));     Cons_I = 0.0
    allocate( PeSi_I(nPointThreadMax));   PeSi_I = 0.0

    allocate(   SpecHeat_I(nPointThreadMax));   SpecHeat_I = 0.0
    allocate(DeltaEnergy_I(nPointThreadMax));DeltaEnergy_I = 0.0

    allocate( ResHeating_I(nPointThreadMax)); ResHeating_I = 0.0
    allocate( ResCooling_I(nPointThreadMax)); ResCooling_I = 0.0
    allocate(DResCoolingOverDT_I(nPointThreadMax)); DResCoolingOverDT_I = 0.0
    allocate(ResEnthalpy_I(nPointThreadMax));ResEnthalpy_I = 0.0
    allocate(ResHeatCond_I(nPointThreadMax));ResHeatCond_I = 0.0
    allocate( ResGravity_I(nPointThreadMax)); ResGravity_I = 0.0

    allocate(     Xi_I(0:nPointThreadMax));     Xi_I = 0.0
    allocate(  VaLog_I(nPointThreadMax));    VaLog_I = 0.0
    allocate(    DXi_I(nPointThreadMax));      DXi_I = 0.0

    allocate(  Res_VI(Cons_:LogP_,nPointThreadMax));      Res_VI = 0.0
    allocate(DCons_VI(Cons_:LogP_,nPointThreadMax));    DCons_VI = 0.0

    allocate(U_VVI(Cons_:LogP_,Cons_:LogP_,nPointThreadMax)); U_VVI = 0.0
    allocate(L_VVI(Cons_:LogP_,Cons_:LogP_,nPointThreadMax)); L_VVI = 0.0
    allocate(M_VVI(Cons_:LogP_,Cons_:LogP_,nPointThreadMax)); M_VVI = 0.0

    SqrtZ = sqrt(Z)

    !\
    ! TeFraction is used for ideal EOS:
    !/
    if(UseElectronPressure)then
       ! Pe = ne*Te (dimensionless) and n=rho/ionmass
       ! so that Pe = ne/n *n*Te = (ne/n)*(rho/ionmass)*Te
       ! TeFraction is defined such that Te = Pe/rho * TeFraction
       TeFraction = MassIon_I(1)/Z
       ! Pi = n*Te (dimensionless) and n=rho/ionmass
       ! so that Pi = (rho/ionmass)*Ti
       ! TiFraction is defined such that Ti = Pi/rho * TeFraction
       TiFraction = MassIon_I(1)
       iP = Pe_
       PeFraction = 1.0
    else
       ! p = n*T + ne*Te (dimensionless) and n=rho/ionmass
       ! so that p=rho/massion *T*(1+ne/n Te/T)
       ! TeFraction is defined such that Te = p/rho * TeFraction
       TeFraction = MassIon_I(1)/(1 + Z)
       TiFraction = TeFraction
       iP = p_
       PeFraction = Z/(1.0 + Z) 
    end if
    !\
    ! Therefore Te = TeFraction*State_V(iP)/State_V(Rho_)
    ! Pe = PeFraction*State_V(iP)
    !/

    call check_tr_table
    iTableTR = i_lookup_table('TR')
    if(iTableTR<=0)call CON_stop('TR table is not set')

    TeMin = TeSiMin*Si2No_V(UnitTemperature_)

    ConsMin = cTwoSevenths*HeatCondParSi*TeSiMin**3.50

    !\
    !   Hydrostatic equilibrium in an isothermal corona: 
    !    d(N_i*k_B*(Z*T_e +T_i) )/dr=G*M_sun*N_I*M_i*d(1/r)/dr
    ! => N_i*Te\propto exp(cGravPot/TeSi*(M_i[amu]/(1+Z))*\Delta(R_sun/r)) 
    !/
    GravHydroStat = cGravPot*MassIon_I(1)/(Z + 1)
    !\
    !energy flux needed to raise the mass flux rho*u to the heliocentric 
    !distance r equals: rho*u*G*Msun*(1/R_sun -1/r)=
    !=k_B*N_i*M_i(amu)u*cGravPot*(1-R_sun/r)=
    !=P_e/T_e*cGravPot*u(M_i[amu]/Z)*(1/R_sun -1/r)
    !/
    GravHydroDyn  = cGravPot*MassIon_I(1)/Z
    !\
    ! With this constant, the dimensionless density 
    ! equals RhoNoDimCoef*PeSi/TeSi
    !/
    RhoNoDimCoef = Si2No_V(UnitEnergyDens_)/PeFraction*&
         TeFraction/Si2No_V(UnitTemperature_)
  end subroutine init_threaded_lc
  !================================
  subroutine read_threaded_bc
    use ModReadParam, ONLY: read_var
    use ModProcMH,      ONLY: iProc
    character(LEN=7)::TypeBc = 'limited'
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
  end subroutine read_threaded_bc
  !\
  ! Main routine:
  ! solves MHD equations along thread, to link the state above the
  ! inner boundary of the global solar corona to the photosphere
  !/
  subroutine solve_boundary_thread(j, k, iBlock, &
       iAction, TeSiIn, PeSiIn, USiIn, AMinorIn, &
       DTeOverDsSiOut, PeSiOut, RhoNoDimOut, AMajorOut)
    !\
    ! USE:
    !/
    use ModFieldLineThread, ONLY: HeatCondParSi
    use ModPhysics,      ONLY: InvGammaMinus1, Gamma, GammaMinus1,&
         No2Si_V, UnitX_,Si2No_V, UnitB_, UnitTemperature_
    use ModLookupTable,  ONLY: interpolate_lookup_table
    use ModMain,         ONLY: BlkTest, ProcTest, jTest, kTest
    use ModProcMH,       ONLY: iProc
    !INPUT:
    !\
    !Cell and block indexes for the boundary point
    !/
    integer,intent(in):: j, k, iBlock, iAction 
    !\
    ! Parameters of the state in the true cell near the boundary:
    ! TeSiIn: Temperature in K 
    ! USiIn:  Velocity progection on the magnetic field direction.
    ! It is positive if the wind blows outward the Sun.
    ! AMinorIn: for the wave propagating toward the Sun 
    !            WaveEnergyDensity = (\Pi/B)\sqrt{\rho} AMinor**2 
    !/
    real,   intent(in):: TeSiIn, PeSiIn, USiIn, AMinorIn
    !\
    !OUTPUT:
    !DTeOverDsSiOut: Temperature derivative along the thread, at the end point
    !                Used to find the electron temperature in the ghostcell
    !PeSiOut: The electron pressure 
    !AMajorOut: For the wave propagating outward the Sun
    !            EnergyDensity = (\Pi/B)\sqrt{\rho} AMajor**2  
    !/
    real,  intent(out):: DTeOverDsSiOut, PeSiOut, RhoNoDimOut, AMajorOut

    !\
    ! Arrays needed to use lookup table
    !/ 
    real    :: Value_V(6)
    !\
    ! Limited Speed
    !/
    real    :: USi
    !\
    !---------Used in 1D numerical model------------------------
    !/
    !\
    ! Number of TEMPERATURE nodes (first one is on the top of TR
    ! the last one is in the physical cell of the SC model
    !/
    integer        :: nPoint, iPoint 
    integer        :: nIter = 10
    !\
    ! Corrrect density and pressure values in the ghost 
    !/
    real :: GhostCellCorr, BarometricFactor, DeltaTeFactor,             &
         Limiter, DensityRatio, RhoTrueCell,                            &
         FirstOrderRho, FirstOrderPeSi, SecondOrderRho, SecondOrderPeSi

    !\
    ! Electron heat condution flux from Low Corona to TR:
    !/
    real :: HeatFlux2TR 

    logical :: DoTest, DoTestMe

    character(len=*), parameter :: NameSub = 'solve_boundary_thread'
    !-------------------------------------------------------------------------
    !\
    ! Initialize all output parameters from 0D solution
    !/
    call interpolate_lookup_table(iTableTR, TeSiIn, 1.0e8, Value_V, &
           DoExtrapolate=.false.)
    !\
    ! First value is now the product of the thread length in meters times
    ! a geometric mean pressure, so that
    !/
    PeSiOut        = Value_V(LengthPAvrSi_)*SqrtZ/&
         BoundaryThreads_B(iBlock)% LengthSi_III(0,j,k)
    USi = USiIn
    if(USi>0)then
       USi = min(USi,0.1*Value_V(UHeat_))
    else
       USi = max(USi, -0.1*Value_V(HeatFluxLength_)/&
            (BoundaryThreads_B(iBlock)% LengthSi_III(0,j,k)*PeSiIn))
    end if
    RhoNoDimOut    = RhoNoDimCoef*PeSiOut/TeSiIn
    AMajorOut      = 1.0
    TeSiMax        = &
          BoundaryThreads_B(iBlock) % TMax_II(j,k)*No2Si_V(UnitTemperature_)
    ConsMax = cTwoSevenths*HeatCondParSi*TeSiMax**3.50
    if(iBlock==BLKtest.and.iProc==PROCtest.and.j==jTest.and.k==kTest)then
       call set_oktest(NameSub, DoTest, DoTestMe)
       if(DoTestMe)then
          write(*,*)'TeSiIn=       ',TeSiIn,' K '
          write(*,*)'PeSiIn = ', PeSiIn
          write(*,*)'NeSiIn = ', PeSiIn/(TeSiIn*cBoltzmann)
          write(*,*)'Dimensionless density (input)=',RhoNoDimCoef*PeSiIn/TeSiIn
          write(*,*)'AMinorIn=     ', AMinorIn
          write(*,*)'USiIn,USi=        ',USiIn,' ',USi,' m/s'
          write(*,*)'Thread Length=', &
               BoundaryThreads_B(iBlock)% LengthSi_III(0,j,k) &
               ,' m = ',  Si2No_V(UnitX_)*&
               BoundaryThreads_B(iBlock)% LengthSi_III(0,j,k),' Rs'
          write(*,*)'TeSiMax=       ',TeSiMax
          write(*,*)'iAction=',iAction
          write(*,*)'0D model results'
          write(*,*)'Pressure 0D=',PeSiOut
          write(*,*)'RhoNoDimOut=', RhoNoDimOut 
       end if
    else
       DoTest=.false.; DoTestMe=.false.
    endif

    nPoint = BoundaryThreads_B(iBlock)% nPoint_II(j,k)
    if(iAction/=DoInit_)then
       !\
       ! Retrieve temperature and pressure distribution
       !/
       TeSi_I(1:nPoint) = BoundaryThreads_B(iBlock)%TSi_III(1-nPoint:0,j,k)
       PeSi_I(1:nPoint) = BoundaryThreads_B(iBlock)%PSi_III(1-nPoint:0,j,k)
       if(iAction/=Enthalpy_)TeSi_I(nPoint)   = TeSiIn 
    end if
    select case(iAction)
    case(DoInit_)
       !\
       ! As a first approximation, recover Te from the analytical solution
       !/
       TeSi_I(nPoint) = TeSiIn
       do iPoint = nPoint-1, 1, -1
          call interpolate_lookup_table(&
               iTable=iTableTR,         &
               iVal=LengthPAvrSi_,      &
               ValIn=PeSiOut/SqrtZ*     &
               BoundaryThreads_B(iBlock)% LengthSi_III(iPoint-nPoint,j,k), &
               Arg2In=1.0e8,            &
               Value_V=Value_V,         &
               Arg1Out=TeSi_I(iPoint),  & 
               DoExtrapolate=.false.)
       end do
       TeSi_I(1:nPoint) = max(TeSiMin, TeSi_I(1:nPoint))
       !\
       ! The analytical solution assumes constant pressure and 
       ! no heating. Calculate the pressure distribution
       !/ 
       call set_pressure
       call advance_thread(IsTimeAccurate=.false.)
       call get_res_heating(nIterIn=nIter)
       BoundaryThreads_B(iBlock)%TSi_III(1-nPoint:0,j,k) = TeSi_I(1:nPoint) 
       BoundaryThreads_B(iBlock)%PSi_III(1-nPoint:0,j,k) = PeSi_I(1:nPoint)
    case(Enthalpy_)
       call get_res_heating(nIterIn=nIter)
    case(Impl_)
       call advance_thread(IsTimeAccurate=.true.)
       !\
       ! Output for temperature gradient, all the other outputs
       ! are meaningless
       !/
       DTeOverDsSiOut = max(0.0,(TeSi_I(nPoint) - TeSi_I(nPoint-1))/&
            (BoundaryThreads_B(iBlock)% LengthSi_III(0,j,k) - &
            BoundaryThreads_B(iBlock)% LengthSi_III(-1,j,k)))
       if(DoTestMe)then
          write(*,*)'Final dT/ds=  ',DTeOverDsSiOut,&
               ', dT/ds*Length=',DTeOverDsSiOut*&
               BoundaryThreads_B(iBlock)% LengthSi_III(0,j,k),' K'
       end if
       RETURN
    case(Heat_)
       call advance_thread(IsTimeAccurate=.true.)
       !\
       ! Calculate AWaves and store pressure and temperature
       !/
       call get_res_heating(nIterIn=nIter) 
       BoundaryThreads_B(iBlock)%TSi_III(1-nPoint:0,j,k) = TeSi_I(1:nPoint) 
       BoundaryThreads_B(iBlock)%PSi_III(1-nPoint:0,j,k) = PeSi_I(1:nPoint)
    case default
       write(*,*)'iAction=',iAction
       call CON_stop('Unknown action in '//NameSub)
    end select
    !\
    ! Outputs
    !/
    !\
    ! First order solution: ghost cell from the first layer are filled
    ! in with the solution of the threaded field line equation at the 
    ! end point.
    !/
    FirstOrderRho   = RhoNoDimCoef*PeSi_I(nPoint)/TeSi_I(nPoint)
    FirstOrderPeSi  = PeSi_I(nPoint)

    !\
    ! Second order solution consists of two contributions, the first of them
    ! being the correction of true cell values. Calculate the true density:
    !/
    RhoTrueCell = RhoNoDimCoef*PeSiIn/TeSiIn

    ! The pressure in the ghost cell should be corrected corrected for
    ! a barometric scale factor, as a consequence of the hydrostatic 
    ! equiliblrium condition in the physical cell. Cell corr as calculated 
    ! below is the negative of DeltaR of BATSRUS / delta r of the thread. Thus,
    !/ 
    GhostCellCorr =  BoundaryThreads_B(iBlock)% DeltaR_II(j,k)/&
         (1/BoundaryThreads_B(iBlock)%RInv_III(-1,j,k) - &
         1/BoundaryThreads_B(iBlock)%RInv_III(0,j,k) )  !< O!
    !\
    ! 
    !      ghost cell | phys.cell
    !           *<-delta R->*
    !      PGhost=exp(-dLogP/dR*DeltaR)*PPhys
    !/
    BarometricFactor = exp(&
         (log(PeSi_I(nPoint)) - log(PeSi_I(nPoint-1)))*GhostCellCorr )
    SecondOrderPeSi  = PeSiIn*BarometricFactor
    !\
    ! In the ghost cell value of density in addition to the 
    ! barometric factor the temperature gradient is accounted for,
    ! which is cotrolled by the heat flux derived from the TFL model:
    !/
    !\
    ! Solve equation: -(TeGhost-TeTrue)/DeltaR = 
    ! dTe/ds*(b . DirR)
    !/
    !TeGhost = TeSiIn - DTeOverDsSi*sum(BDir_D*DirR_D)*&
    !     BoundaryThreads_B(iBlock)% DeltaR_II(j,k)
    !\
    ! Version Easter 2015 Limit TeGhost
    !/
    !Te_G(0, j, k) = max(TeMin,min(Te_G(0, j, k), &
    !     BoundaryThreads_B(iBlock) % TMax_II(j,k)))
    !No2Si_V(UnitTemperature_)
 
    GhostCellCorr =  BoundaryThreads_B(iBlock)% DeltaR_II(j,k)/min(          &
         (1/BoundaryThreads_B(iBlock)%RInv_III(-1,j,k) -                     &
         1/BoundaryThreads_B(iBlock)%RInv_III(0,j,k) ),-0.7*                 &  
         Si2No_V(UnitX_)*(BoundaryThreads_B(iBlock)% LengthSi_III(0,j,k) -   &
         BoundaryThreads_B(iBlock)% LengthSi_III(-1,j,k)))

    DeltaTeFactor = TeSi_I(nPoint)/max(TeSiMin, TeSi_I(nPoint) + &
         max(TeSi_I(nPoint  ) - TeSi_I(nPoint-1),0.0)*GhostCellCorr)
    !\
    ! Approximately TeSiGhost = TeSiIn/DeltaTeFactor, so that:
    !/
    SecondOrderRho = RhoTrueCell*BarometricFactor*DeltaTeFactor
    !\
    ! Add numerical diffusion, which forcing the density in the true cell
    ! to approach the "first order" values predicted by the TFL model. 
    !/
    SecondOrderRho  = SecondOrderRho  + (FirstOrderRho - RhoTrueCell)
    SecondOrderPeSi = SecondOrderPeSi + &
         (FirstOrderPeSi - PeSiIn)/DeltaTeFactor
    !\
    ! In the latter equation the 1/DeltaTeFactor multipler is introduced
    ! to keep the ratio of the corrected values to be equal to
    ! TeSiGhost = TeSiIn/DeltaTeFactor
    ! as predicted by the TFL model
    !/
    !\
    ! Now, limit the difference between the first and second order
    ! solutions, depending on the ratio between the true cell value
    ! density and the first order ghost cell value of density
    !/
    DensityRatio = RhoTrueCell/FirstOrderRho
    !\
    ! If PeSiIn>PeSi_I(nPoint), then we apply limitation, which completely
    ! eleminates the second order correction if this ratio becomes as high
    ! as the barometric factor:
    !/
    Limiter = min(LimMax, max(LimMin, &
         (BarometricFactor - DensityRatio)/(BarometricFactor - 1)))
    RhoNoDimOut = Limiter*(SecondOrderRho - FirstOrderRho) +&
         FirstOrderRho
    PeSiOut     = Limiter*(SecondOrderPeSi - FirstOrderPeSi) + &
         FirstOrderPeSi
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
            'iPoint TeSiOld TeSi TeSiStart PeSi ResHeating ResEnthalpy'
       do iPoint=1,nPoint
          write(*,'(i4,6es15.6)')iPoint, TeSiOld_I(iPoint),TeSi_I(iPoint),&
               TeSiStart_I(iPoint),&
               PeSi_I(iPoint),ResHeating_I(iPoint), ResEnthalpy_I(iPoint)
       end do
       call CON_stop('Failure')
    end if
    if(DoTestMe)then
       write(*,*)'AMajorOut=    ', AMajorOut
       write(*,*)'Before correction:'
       write(*,*)'Pressure 1D (SI) = ',FirstOrderPeSi
       write(*,*)'RhoNoDimOut      = ',FirstOrderRho
       write(*,*)'Last two temperature values=', TeSi_I(nPoint-1:nPoint)
       write(*,*)'Last two pressure values=', PeSi_I(nPoint-1:nPoint)
       write(*,*)'Barometric factor=',BarometricFactor
       write(*,*)'DeltaTeFactor=',DeltaTeFactor
       write(*,*)'Corrected:'
       write(*,*)'Pressure 1D (SI) = ',PeSiOut
       write(*,*)'RhoNoDimOut      = ',RhoNoDimOut
    end if    
  contains
    !=============================
    subroutine set_pressure
      integer::iPoint
      !---------------
      !\
      ! First variable in Value_V  is now the product of the thread length in 
      ! meters times a geometric mean pressure, so that
      !/
      PeSi_I(1) = Value_V(LengthPAvrSi_)*SqrtZ/&
           BoundaryThreads_B(iBlock)% LengthSi_III(1-nPoint,j,k)
      !\
      !   Hydrostatic equilibrium in an isothermal corona: 
      !    d(N_i*k_B*(Z*T_e +T_i) )/dr=G*M_sun*N_I*M_i*d(1/r)/dr
      ! => N_i*Te\propto exp(cGravPot/TeSi*(M_i[amu]/(1+Z))*\Delta(R_sun/r)) 
      !/
      !GravHydroStat = cGravPot*MassIon_I(1)/(Z + 1)
      do iPoint = 2, nPoint
         PeSi_I(iPoint) = PeSi_I(iPoint-1)*&
            exp( -BoundaryThreads_B(iBlock)%TGrav_III(iPoint-nPoint,j,k)&
            /TeSi_I(iPoint))
      end do
    end subroutine set_pressure
 !======================
    subroutine advance_thread(IsTimeAccurate)
      use ModMain,     ONLY: cfl, Dt, time_accurate
      use ModAdvance,  ONLY: time_BLK
      use ModPhysics,  ONLY: UnitT_, No2Si_V
      !\
      ! Advances the thread solution
      ! If IsTimeAccurate, the solution is advanced through the time 
      ! interval, DtLocal. Otherwise, it looks for the steady-state solution
      ! with the advanced boundary condition
      !/ 
      logical, intent(in) :: IsTimeAccurate 
      !\
      ! Time step in the physical cell from which the thread originates
      !/
      real    :: DtLocal,DtInv
      !\
      ! Loop variable
      !/
      
      integer :: iPoint, iIter
      !\
      ! Enthalpy correction coefficient
      !/
      real    :: EnthalpyFlux, FluxConst
      !\
      ! Coreection accounting for the Enthlpy flux from the TR
      real    :: PressureTRCoef 
      !-----------------
      if(IsTimeAccurate)then
         if(time_accurate)then
            DtLocal = Dt*No2Si_V(UnitT_)
         else
            DtLocal = cfl*time_BLK(1,j,k,iBlock)*No2Si_V(UnitT_)
         end if
         if(DtLocal==0.0)RETURN !No time-accurate advance is needed
         DtInv = 1/DtLocal    
      else
         DtInv = 0.0
      end if
      !\
      ! Initialization
      !/
      TeSiStart_I(1:nPoint) = TeSi_I(1:nPoint)
      Cons_I(1:nPoint) = cTwoSevenths*HeatCondParSi*TeSi_I(1:nPoint)**3.50
      SpecHeat_I(1:nPoint-1) = InvGammaMinus1*(1 + Z)/Z*             &
           BoundaryThreads_B(iBlock)%DsOverBSi_III(1-nPoint:-1,j,k)* &
           PeSi_I(1:nPoint-1)/TeSiStart_I(1:nPoint-1)
      DeltaEnergy_I= 0.0; Res_VI=0.0 ; ResEnthalpy_I= 0.0 
      PressureTRCoef = 1.0; FluxConst = 0.0 
      !\
      ! Turbulent heating and mass flux are not iterated
      !/
      call get_res_heating(nIterIn=nIter)
      if(.not.IsTimeAccurate)ResHeating_I=0.0
      if(USi>0)then
         FluxConst    = USi * PeSi_I(nPoint)/&
              (TeSiIn*PoyntingFluxPerBSi*&
              BoundaryThreads_B(iBlock)% B_III(0,j,k)*No2Si_V(UnitB_))
      elseif(USi<0)then
         FluxConst    = USi * PeSiIn/&
              (TeSiIn*PoyntingFluxPerBSi*&
              BoundaryThreads_B(iBlock)% B_III(0,j,k)*No2Si_V(UnitB_))
         
      end if
      EnthalpyFlux = FluxConst/Z& !5/2*U*Pi*(Z+1)
           *(InvGammaMinus1 +1)*(1 + Z)
      !\
      ! Calculate flux to TR and its temperature derivative
      !/ 
      call interpolate_lookup_table(iTableTR, TeSi_I(1), 1.0e8, Value_V, &
           DoExtrapolate=.false.)

      do iIter = 1,nIter
         !\
         ! Iterations
         !/
         TeSiOld_I(1:nPoint) = TeSi_I(1:nPoint)
         !\
         !Shape the source.
         !/
         call get_heat_cond
         !\
         ! Add enthalpy correction
         !/
         if(FluxConst/=0.0)EnthalpyFlux = sign(min(abs(EnthalpyFlux),&
               0.50*HeatFlux2TR/TeSi_I(1)),FluxConst)
         if(USi>0)then
            ResEnthalpy_I(2:nPoint-1) = &
                 EnthalpyFlux*(TeSi_I(1:nPoint-2) - TeSi_I(2:nPoint-1))
            ResEnthalpy_I(1)   = 0.0
            L_VVI(Cons_,Cons_,2:nPoint-1) =  L_VVI(Cons_,Cons_,2:nPoint-1)&
                 - EnthalpyFlux*TeSi_I(2:nPoint-1)/(3.50*Cons_I(2:nPoint-1))
            M_VVI(Cons_,Cons_,2:nPoint-1) =  M_VVI(Cons_,Cons_,2:nPoint-1)&
                 + EnthalpyFlux*TeSi_I(2:nPoint-1)/(3.50*Cons_I(2:nPoint-1))
         elseif(USi<0)then
            ResEnthalpy_I(1:nPoint-1) = &
                 -EnthalpyFlux*(TeSi_I(2:nPoint) - TeSi_I(1:nPoint-1))
            U_VVI(Cons_,Cons_,1:nPoint-1) = U_VVI(Cons_,Cons_,1:nPoint-1)&
                 + EnthalpyFlux*TeSi_I(1:nPoint-1)/(3.50*Cons_I(1:nPoint-1))
            M_VVI(Cons_,Cons_,1:nPoint-1) = M_VVI(Cons_,Cons_,1:nPoint-1)&
                 - EnthalpyFlux*TeSi_I(1:nPoint-1)/(3.50*Cons_I(1:nPoint-1))
         end if
         !==========Add Gravity Source================================
         !\
         !cGravPot = cGravitation*mSun*cAtomicMass/&
         !/   (cBoltzmann*rSun)
         !GravHydroDyn = cGravPot*MassIon_I(1)/Z
         !\
         !energy flux needed to raise the mass flux rho*u to the  
         !heliocentric distance r equals: rho*u*G*Msun*(1/R_sun -1/r)=
         !=k_B*N_i*M_i(amu)*u*cGravPot*(1-R_sun/r)=
         !=P_e/T_e*cGravPot*(M_ion[amu]/Z)*u*(1/R_sun -1/r)
         !/
         
         ResGravity_I(2:nPoint-1) = 0.5*GravHydroDyn*FluxConst*(     &
              - BoundaryThreads_B(iBlock)%RInv_III(1-nPoint:-2,j,k)  &
              + BoundaryThreads_B(iBlock)%RInv_III(3-nPoint: 0,j,k))
         ResGravity_I(1)          = 0.5*GravHydroDyn*FluxConst*(     &
              - BoundaryThreads_B(iBlock)%RInv_III(1-nPoint,j,k)     &
              + BoundaryThreads_B(iBlock)%RInv_III(2-nPoint,j,k))
         ResEnthalpy_I(1:nPoint-1) = ResEnthalpy_I(1:nPoint-1) + &
                    ResGravity_I(1:nPoint-1) 
            
         !\
         ! For the time accurate mode, account for the time derivative
         ! The contribution to the Jacobian be modified although the specific
         ! heat should not,  because the conservative variable is 
         ! flux-by-length, not the temperature
         !/
         M_VVI(Cons_,Cons_,1:nPoint-1) = M_VVI(Cons_,Cons_,1:nPoint-1) + &
              DtInv*SpecHeat_I(1:nPoint-1)*TeSi_I(1:nPoint-1)/   &
              (3.50*Cons_I(1:nPoint-1))
         !   PressureTRCoef = sqrt(max(&
         !        1 - EnthalpyFlux*TeSi_I(1)/HeatFlux2TR,1.0e-8))        
         Res_VI(Cons_,1:nPoint-1) = -DeltaEnergy_I(1:nPoint-1) +      &
              ResHeating_I(1:nPoint-1) +  ResCooling_I(1:nPoint-1) +&
              ResEnthalpy_I(1:nPoint-1) + ResHeatCond_I(1:nPoint-1)
         DCons_VI = 0.0
         call tridiag_2by2_block(n=nPoint-1,  &
              L_VVI=L_VVI(:,:,1:nPoint-1),&
              M_VVI=M_VVI(:,:,1:nPoint-1),&
              U_VVI=U_VVI(:,:,1:nPoint-1),&
              R_VI=Res_VI(:,1:nPoint-1),  &
              W_VI=DCons_VI(:,1:nPoint-1))         
         !\
         ! limit DeltaCons
         !/
         DCons_VI(Cons_,1:nPoint-1) = max(min(DCons_VI(Cons_,1:nPoint-1), &
              ConsMax - Cons_I(    1:nPoint-1)),  &
              ConsMin - Cons_I(    1:nPoint-1))
         !\
         ! Apply DeltaCons
         !/
         Cons_I(1:nPoint-1) = Cons_I(1:nPoint-1) + DCons_VI(Cons_,1:nPoint-1)
         !\
         ! Recover temperature
         !/
         TeSi_I(1:nPoint-1) = &
              (3.50*Cons_I(1:nPoint-1)/HeatCondParSi)**cTwoSevenths
         !\
         ! Change in the internal energy (to correct the energy source 
         ! for the time-accurate mode):
         !/
         DeltaEnergy_I(1:nPoint-1) = DtInv*SpecHeat_I(1:nPoint-1)* &
              (TeSi_I(1:nPoint-1) - TeSiStart_I(1:nPoint-1))
         !\
         ! Calculate TR pressure
         ! For next iteration calculate TR heat flux and 
         ! its temperature derivative
         !/
         call interpolate_lookup_table(iTableTR, TeSi_I(1), 1.0e8, Value_V, &
              DoExtrapolate=.false.)
         !\
         ! Set pressure for updated temperature 
         !/
         Value_V(LengthPAvrSi_) = Value_V(LengthPAvrSi_)*PressureTRCoef
         call set_pressure
         !\
         ! Check convergence
         !/
         if(DoTestMe.and.DoConvergenceCheck)then
            write(*,'(a,i4)')&
                 'TeOld abs(DCons) Cons abs(DCons)/Cons Res_V TNew at iIter=',&
                 iIter
            do iPoint=1,nPoint
               write(*,'(i4,6es15.6)')iPoint, TeSiOld_I(iPoint),  &
                    abs(DCons_VI(Cons_,iPoint)),Cons_I(iPoint),         &
                    abs(DCons_VI(Cons_,iPoint))/Cons_I(iPoint),         &
                    Res_VI(Cons_,iPoint), TeSi_I(iPoint)
            end do
         end if
         if(all(abs(DCons_VI(Cons_,1:nPoint-1))<cTol*Cons_I(1:nPoint-1)))EXIT
      end do
      if(any(abs(DCons_VI(Cons_,1:nPoint-1))>cTol*Cons_I(1:nPoint-1))&
           .and.DoConvergenceCheck)then
         write(*,'(a)')'TeOld Te TeMin PeSi_I Heating Enthalpy'
         do iPoint=1,nPoint
            write(*,'(i4,6es15.6)')iPoint, TeSiOld_I(iPoint),TeSi_I(iPoint),&
                 BoundaryThreads_B(iBlock)%TGrav_III(iPoint-nPoint,j,k),&
                 PeSi_I(iPoint),ResHeating_I(iPoint), ResEnthalpy_I(iPoint)
         end do
         write(*,'(a,es15.6,a,3es15.6)')'Error =',maxval(&
              abs(DCons_VI(Cons_,1:nPoint-1)/Cons_I(1:nPoint-1))),&
              ' at the point Xyz=',Xyz_DGB(:,1,j,k,iBlock)
         write(*,'(a,3es15.6)')&
              'Input parameters: TeSiIn,USiIn,USi,PeSiIn,PCoef=',&
              TeSiIn,USiIn,USi,PeSiIn,PressureTRCoef
         call CON_stop('Algorithm failure in advance_thread')
      end if
    end subroutine advance_thread
    !=======================
    subroutine get_res_heating(nIterIn)
      integer, intent(in)::nIterIn
      integer:: iPoint
      real    :: SqrtRho, RhoNoDim
      !--------------------
      !\
      ! Now prepare to calculate Alfven wave amplitude distribution
      !/
      Xi_I(0) = 0.0
      do iPoint=1,nPoint
         !\
         ! 1. Calculate sqrt(RhoNoDim)
         !/
         RhoNoDim = RhoNoDimCoef*PeSi_I(iPoint)/TeSi_I(iPoint)
         if(RhoNoDim<=0.0)then
            write(*,*)'iPoint=',iPoint,' PeSi_I(iPoint)=',&
                 PeSi_I(iPoint),' TeSi_I(iPoint)=',TeSi_I(iPoint)
            call CON_stop('Non-positive Density in '//NameSub)
         end if
         SqrtRho = sqrt(RhoNoDim)
         !\
         ! 2. Calculate Alfven wave speed
         !/
         VaLog_I(iPoint) = &
            log(BoundaryThreads_B(iBlock)% B_III(iPoint-nPoint,j,k)/SqrtRho)
         !\
         ! 3. Calculate dimensionless length (in terms of the dissipation length
         !/
         DXi_I(iPoint) = &
              (BoundaryThreads_B(iBlock)% Xi_III(iPoint-nPoint,j,k) - &
              BoundaryThreads_B(iBlock)% Xi_III(iPoint-1-nPoint,j,k))*&
              sqrt(SqrtRho)
         Xi_I(iPoint) = Xi_I(iPoint-1) + DXi_I(iPoint) 
      end do
      !\
      ! 4. Calculate the reflection coefficient
      !/
      ReflCoef_I(1) = abs(VaLog_I(2) - VaLog_I(1))/(0.50*DXi_I(2) + DXi_I(1))
      ReflCoef_I(0) =  ReflCoef_I(1)
      do iPoint = 2, nPoint-2
         ReflCoef_i(iPoint) = &
              abs(VaLog_I(iPoint+1) - VaLog_I(iPoint))/&
              (0.50*(DXi_I(iPoint+1) +  DXi_I(iPoint)))
      end do
      ReflCoef_I(nPoint-1) = abs(VaLog_I(nPoint) - VaLog_I(nPoint-1))/&
           (0.50*DXi_I(nPoint-1) + DXi_I(nPoint))
      ReflCoef_I(nPoint) =     ReflCoef_I(nPoint-1)
      !\
      ! Solve amplitudes of the Alfven waves (arrays there have dimension)
      ! (0:nI)
      !/
      call solve_a_plus_minus(&
           nI=nPoint,                      & 
           ReflCoef_I=ReflCoef_I(0:nPoint),&
           Xi_I=Xi_I(0:nPoint),            &
           AMinusBC=AMinorIn,              &
           APlusBC=AMajorOut,              &
           nIterIn=nIterIn)

      ResHeating_I = 0.0
      ResHeating_I(1:nPoint-1) = &
           (APlus_I(0:nPoint-2)**2 - AMinus_I(0:nPoint-2)**2)&
           -(APlus_I(1:nPoint-1  )**2 - AMinus_I(1:nPoint-1)**2)
    end subroutine get_res_heating
    !==========================
    subroutine get_heat_cond
      M_VVI = 0.0; ResHeatCond_I = 0.0; U_VVI = 0.0; L_VVI = 0.0
      !----------------
      !\
      ! Contribution from heat conduction fluxes
      !/
      !\
      ! Flux linearizations over small dCons
      ! Dimensionless flux= dCons/ds(1/( (PoyntingFlux/B)B)
      !/
      U_VVI(Cons_,Cons_,1:nPoint-1) = &
           -BoundaryThreads_B(iBlock)% BDsInvSi_III(1-nPoint:-1,j,k)
      L_VVI(Cons_,Cons_,2:nPoint-1) = U_VVI(Cons_,Cons_,1:nPoint-2)
      M_VVI(Cons_,Cons_,2:nPoint-1) = &
           -U_VVI(Cons_,Cons_,2:nPoint-1) - L_VVI(Cons_,Cons_,2:nPoint-1)
      !\
      ! Right heat fluxes
      !/
      ResHeatCond_I(1:nPoint-1) = &
           (Cons_I(1:nPoint-1) - Cons_I(2:nPoint))*U_VVI(Cons_,Cons_,1:nPoint-1)
      !\
      ! Add left heat flux to the TR
      !/
      HeatFlux2TR = Value_V(HeatFluxLength_)*&
           BoundaryThreads_B(iBlock)% BDsInvSi_III(-nPoint,j,k)
      ResHeatCond_I(1) = ResHeatCond_I(1) -  HeatFlux2TR 
      !\
      ! Linearize left heat flux to the TR
      !/

      M_VVI(Cons_,Cons_,1) = -U_VVI(Cons_,Cons_,1) + Value_V(DHeatFluxXOverU_)*&
           BoundaryThreads_B(iBlock)% BDsInvSi_III(-nPoint,j,k)
      !\
      ! Add other left heat fluxes
      !/
      ResHeatCond_I(2:nPoint-1) = ResHeatCond_I(2:nPoint-1) + &
           (Cons_I(2:nPoint-1) - Cons_I(1:nPoint-2))*&
           L_VVI(Cons_,Cons_,2:nPoint-1)
      !\
      ! LogP_ terms
      !/
      M_VVI(LogP_,LogP_,1:nPoint-1) =  1.0
      !\
      ! We satisfy the equation, d(LogP) = d(Cons)*(dPAvr/dCons)_{TR}/PAvr
      !/
      M_VVI(LogP_,Cons_,1) = -1/&
              Value_V(HeatFluxLength_)

      !\
      ! For other points we satisfy the hydrostatic equilibrium condition
      ! LogPe^{i-1}=LogPe^i+TGrav/Te^i
      ! dLogPe^i - dCons^i(TGrav/(Te^i)^2)*dTe/dCons -dLogPe^{i-1}=0
      !/
      M_VVI(LogP_,Cons_,2:nPoint-1) = -&
           BoundaryThreads_B(iBlock)% TGrav_III(2-nPoint:-1,j,k)/&
           (TeSi_I(2:nPoint-1)*3.50*Cons_I(2:nPoint-1))
      L_VVI(LogP_,LogP_,2:nPoint-1) = -1.0

      call get_cooling(nLast=nPoint-1)
      M_VVI(Cons_,LogP_,1:nPoint-1) = &
           -2*ResCooling_I(1:nPoint-1) !=-dCooling/dLogPe
      M_VVI(Cons_,Cons_,1:nPoint-1) = M_VVI(Cons_,Cons_,1:nPoint-1)&
           -DResCoolingOverDT_I(1:nPoint-1)/&
           (3.50*Cons_I(1:nPoint-1))   !=-dCooling/dCons
           
    end subroutine get_heat_cond
    !===========================
    subroutine get_cooling(nLast)
      integer,intent(in) ::nLast
      integer ::  iPoint
      !-----------------
      ResCooling_I = 0.0;
      do iPoint = 1, nLast
         if(TeSi_I(iPoint)>1.0e8)then
            write(*,*)'Failure in heat condusction setting'
            write(*,*)'In the point Xyz=',Xyz_DGB(:,1,j,k,iBlock)
            write(*,*)'TeSiIn, PeSiIn = ', TeSiIn, PeSiIn
            write(*,*)'TeSi_I=',TeSi_I(1:nPoint)
            call CON_stop('Stop!!!')
         end if
         call interpolate_lookup_table(iTableTR, TeSi_I(iPoint), 1.0e8, &
              Value_V, &
           DoExtrapolate=.false.)
         ResCooling_I(iPoint) = &
              -BoundaryThreads_B(iBlock)%DsOverBSi_III(iPoint-nPoint,j,k)&
              *Value_V(LambdaSI_)/Z*&
              (PeSi_I(iPoint)/TeSi_I(iPoint))**2
         DResCoolingOverDT_I(iPoint) = &
              ResCooling_I(iPoint)*(Value_V(DLogLambdaOverLogT_) - 2)
      end do
    end subroutine get_cooling
    !=========================
  end subroutine solve_boundary_thread
  !=========================================================================  
  ! This routine solves three-diagonal system of equations:                    !
  !  ||m_1 u_1  0....        || ||w_1|| ||r_1||                                !
  !  ||l_2 m_2 u_2...        || ||w_2|| ||r_2||                                !
  !  || 0  l_3 m_3 u_3       ||.||w_3||=||r_3||                                !
  !  ||...                   || ||...|| ||...||                                !
  !  ||.............0 l_n m_n|| ||w_n|| ||r_n||                                !
  ! Prototype: Numerical Recipes, Chapter 2.6, p.40.
  ! Here each of the compenets w_i and r_i are 2-component states and 
  ! m_i, l_i, u_i are 2*2 matrices                                             !
  !============================================================================!
  subroutine tridiag_2by2_block(n,L_VVI,M_VVI,U_VVI,R_VI,W_VI)
    implicit none
    !--------------------------------------------------------------------------!
    integer, intent(in):: n
    real, intent(in):: L_VVI(2,2,n),M_VVI(2,2,n),U_VVI(2,2,n),R_VI(2,n)
    real, intent(out):: W_VI(2,n)
    !--------------------------------------------------------------------------!
    integer:: j
    real   :: TildeM_VV(2,2), TildeMInv_VV(2,2), TildeMInvDotU_VVI(2,2,2:n)
    !--------------------------------------------------------------------------!
    !\
    ! If tilde(M)+L.Inverted(\tilde(M))\dot.U = M, then the equation 
    !      (M+L+U)W = R
    ! may be equivalently written as 
    ! (tilde(M) +L).(I + Inverted(\tilde(M)).U).W=R
    !/
    if (det(M_VVI(:,:,1)).eq.0.0) then
       call CON_stop('Error in tridiag: M_I(1)=0')
    end if
    TildeM_VV = M_VVI(:,:,1)
    TildeMInv_VV = inverted(TildeM_VV)
    !\
    ! First 2-vector element of the vector, Inverted(tilde(M) + L).R 
    !/
    W_VI(:,1) = matmul(TildeMInv_VV,R_VI(:,1))
    do j=2,n
       !\
       ! Next 2*2 blok element of the matrix, Inverted(Tilde(M)).U
       !/
       TildeMInvDotU_VVI(:,:,j) = matmul(TildeMInv_VV,U_VVI(:,:,j-1))
       !\
       ! Next 2*2 block element of matrix tilde(M), obeying the eq.
       ! tilde(M)+L.Inverted(\tilde(M))\dot.U = M
       !/
       TildeM_VV = M_VVI(:,:,j) - &
            matmul(L_VVI(:,:,j),TildeMInvDotU_VVI(:,:,j))
       if (det(TildeM_VV).eq.0.0) then
          write(*,*)'j, M_I(j), L_I(j), TildeMInvDotU_I(j) = ',j, &
               M_VVI(:,:,j),L_VVI(:,:,j),TildeMInvDotU_VVI(:,:,j)
          call CON_stop('2*2 block Tridiag failed')
       end if
       !\
       ! Next element of inverted(Tilde(M))
       !/
       TildeMInv_VV = inverted(TildeM_VV)
       !\
       ! Next 2-vector element of the vector, Inverted(tilde(M) + L).R
       ! satisfying the eq. (tilde(M) + L).W = R
       !/
       W_VI(:,j) = matmul(TildeMInv_VV,R_VI(:,j)) - &
            matmul(TildeMInv_VV,matmul(L_VVI(:,:,j),W_VI(:,j-1)))
    end do
    do j=n-1,1,-1
       !\
       ! Finally we solve equation
       ! (I + Inverted(Tilde(M)).U).W =  Inverted(tilde(M) + L).R
       !/
       W_VI(:,j) = W_VI(:,j)-matmul(TildeMInvDotU_VVI(:,:,j+1),W_VI(:,j+1))
    end do
  end subroutine tridiag_2by2_block
  !=============
  real function det(A_II)
    real, intent(in)::A_II(2,2)
    det = A_II(1,1)*A_II(2,2) - A_II(1,2)*A_II(2,1)
  end function det
  !============
  function inverted(A_II)RESULT(B_II)
    real,intent(in)::A_II(2,2)
    real           ::B_II(2,2)
    real           ::DetInv
    !------------------
    DetInv = 1/det(A_II)
    B_II(1,1) = A_II(2,2)*DetInv
    B_II(2,2) = A_II(1,1)*DetInv
    B_II(1,2) = -A_II(1,2)*DetInv
    B_II(2,1) = -A_II(2,1)*DetInv
  end function inverted
  !====================
  subroutine solve_a_plus_minus(nI, ReflCoef_I, Xi_I, AMinusBC,&
       APlusBC, nIterIn)
    !INPUT
    integer,         intent(in):: nI
    real,            intent(in):: ReflCoef_I(0:nI), Xi_I(0:nI)
    real,            intent(in):: AMinusBC         !BC for A-
    !OUTPUT
    real,           intent(out):: APlusBC          !BC for A+
    integer,optional,intent(in):: nIterIn
    real:: DeltaXi
    integer::iStep,iIter
    integer, parameter:: nIterMax = 10
    integer:: nIter
    real::Derivative, AOld, ADiffMax, AP, AM, APMid, AMMid
    !---------------------------------------------------------------------------
    APlus_I(0:nI)  = 1.0
    AMinus_I(0:nI) = AMinusBC
    if(present(nIterIn))then
       nIter=nIterIn
    else
       nIter=nIterMax
    end if
    do iIter=1,nIter
       !\
       !Go forward, integrate APlus_I with given AMinus_I
       !/
       ADiffMax = 0.0
       do iStep=1, nI
          !Predictor
          AP = APlus_I(iStep-1); AM = AMinus_I(iStep-1)
          Derivative = -AM*(max(0.0, AP - 2*AM) - max(0.0, AM - 2*AP) )*&
               min(0.5*ReflCoef_I(iStep-1)/max(AM,AP),  1.0)- &
               AP*AM
          AOld = APlus_I(iStep)
          DeltaXi = Xi_I(iStep) - Xi_I(iStep-1)

          !Corrector
          AMMid = 0.5*(AMinus_I(iStep-1) + AMinus_I(iStep))
          APMid = AP + 0.5*Derivative*DeltaXi
          Derivative = -AMMid*&
               (max(0.0, APMid -2*AMMid) -  max(0.0,AMMid - 2*APMid))*&
               min(0.250*(ReflCoef_I(iStep-1)+ReflCoef_I(iStep))/&
               max(AMMid,APMid), 1.0) - AMMid*APMid
          APlus_I(iStep) = AP + Derivative*DeltaXi
          ADiffMax = max(ADiffMax, &
               abs(AOld - APlus_I(iStep))/max(AOld,APlus_I(iStep)))
       end do
       !Go backward, integrate APlus_I with given AMinus_I
       !We integrate equation,
       !\
       ! 2da_-/d\xi=
       !=-[ max(1-2a_-/a_+,0)-max(1-2a_+/a_-,0)]* a_+ *
       ! *min(ReflCoef,2max(a_,a_+))-
       ! -2a_-a_+
       !/
       do iStep=nI - 1, 0, -1
          !Predictor
          AP = APlus_I(iStep+1); AM = AMinus_I(iStep+1)
          Derivative = -AP*(max(0.0,AP - 2*AM) - max(0.0,AM - 2*AP)     )*&
               min(0.5*ReflCoef_I(iStep+1)/max(AM,AP), 1.0) + &
               AP*AM
          AOld = AMinus_I(iStep)
          DeltaXi = Xi_I(iStep+1) - Xi_I(iStep) 

          !Corrector
          APMid = 0.5*(APlus_I(iStep+1) + APlus_I(iStep))
          AMMid = AM - 0.5*Derivative*DeltaXi
          Derivative = -APMid*&
               ( max(0.0,APMid -2*AMMid)- max(0.0,AMMid - 2*APMid) )*&
               min(0.250*(ReflCoef_I(iStep+1) + ReflCoef_I(iStep))/&
               max(AMMid, APMid), 1.0) + AMMid*APMid
          AMinus_I(iStep) = AMinus_I(iStep+1) - Derivative*DeltaXi
          ADiffMax = max(ADiffMax,&
               abs(AOld - AMinus_I(iStep))/max(AOld, AMinus_I(iStep)))
       end do
       if(ADiffMax<cTol)EXIT
    end do
    APlusBC = APlus_I(nI)
  end subroutine solve_a_plus_minus
  !=========================================================================
  subroutine set_field_line_thread_bc(nGhost, iBlock, nVarState, State_VG, &
               iImplBlock, IsLinear)
    use EEE_ModCommonVariables, ONLY: UseCme
    use EEE_ModMain,            ONLY: EEE_get_state_BC
    use ModMain,       ONLY: n_step, iteration_number, time_simulation
    use ModAdvance,      ONLY: State_VGB
    use BATL_lib, ONLY:  MinI, MaxI, MinJ, MaxJ, MinK, MaxK
    use BATL_size,ONLY:  nJ, nK
    use ModPhysics,      ONLY: No2Si_V, Si2No_V, UnitTemperature_, &
         UnitEnergyDens_, UnitU_, UnitX_, InvGammaElectronMinus1
    use ModMultiFluid,   ONLY: UseMultiIon, MassIon_I, ChargeIon_I, iRhoIon_I
    use ModVarIndexes,   ONLY: Rho_, Pe_, p_, Bx_, Bz_, &
         RhoUx_, RhoUz_, EHot_
    use ModImplicit,     ONLY: iTeImpl
    use ModB0,           ONLY: B0_DGB
    use ModWaves,        ONLY: WaveFirst_, WaveLast_
    use ModHeatFluxCollisionless, ONLY: UseHeatFluxCollisionless, &
         get_gamma_collisionless
    use ModMain, ONLY: BlkTest, ProcTest, jTest, kTest
    use ModProcMH, ONLY: iProc
    integer, intent(in):: nGhost
    integer, intent(in):: iBlock
    integer, intent(in):: nVarState
    real, intent(inout):: State_VG(nVarState,MinI:MaxI,MinJ:MaxJ,MinK:MaxK)
 
    ! Optional arguments when called by semi-implicit scheme
    integer, optional, intent(in):: iImplBlock
    logical, optional, intent(in):: IsLinear

    !\
    ! Determines, which action should be done with the thread 
    ! before setting the BC
    !/
    integer:: iAction
    
    integer :: i, j, k, Major_, Minor_
    real :: TeSi, PeSi, BDir_D(3), U_D(3), U, B1_D(3), SqrtRho, DirR_D(3)
    real :: PeSiOut, AMinor, AMajor, DTeOverDsSi, DTeOverDs, GammaHere
    real :: RhoNoDimOut, UAbsMax
    !\
    ! CME parameters, if needed
    !/
    real:: RhoCme, Ucme_D(3), Bcme_D(3), pCme
    logical:: DoTest, DoTestMe
    character(len=*), parameter :: NameSub = 'set_thread_bc'
    !--------------------------------------------------------------------------
    if(present(iImplBlock))then
       if(BoundaryThreads_B(iBlock)%iAction/=Done_)&
            call CON_stop('Algorithm error in '//NameSub)
       iAction=Impl_
    else
       iAction=BoundaryThreads_B(iBlock)%iAction
    end if
    if(iAction==Done_)RETURN
    if(present(iImplBlock))then
       if(IsLinear)then
          !\
          !Version Easter 2015
          !/
          do k = MinK, MaxK; do j = MinJ, maxJ
             State_VG(iTeImpl,0,j,k) = 0.0
          end do; end do
          RETURN
       end if
    end if
    if(iBlock==BLKtest.and.iProc==PROCtest)then
       call set_oktest(NameSub, DoTest, DoTestMe)
    else
       DoTest=.false.; DoTestMe=.false.
    endif
    !\
    ! Start from floating boundary values
    !/
    do k = MinK, MaxK; do j = MinJ, maxJ; do i = 1 - nGhost, 0
       State_VG(:, i,j,k) = State_VG(:,1, j, k)
    end do; end do; end do
    !\
    ! Fill in the temperature array
    !/
    if(UseIdealEos)then
       do k = 1, nK; do j = 1, nJ
          Te_G(0:1,j,k) = min(&
               TeFraction*State_VGB(iP,1,j,k,iBlock), &
               TiFraction*State_VGB(p_,1,j,k,iBlock)) &
               /State_VGB(Rho_,1,j,k,iBlock)
       end do; end do
    else
       call CON_stop('Generic EOS is not applicable with threads')
    end if
    do k = 1, nK; do j = 1, nJ
       B1_D = State_VGB(Bx_:Bz_,1,j,k,iBlock)
       BDir_D = B1_D + 0.50*(B0_DGB(:, 1, j, k, iBlock) + &
            B0_DGB(:, 0, j, k, iBlock))
       BDir_D = BDir_D/max(sqrt(sum(BDir_D**2)), 1e-30)
       DirR_D = Xyz_DGB(:,1,j,k,iBlock)  
       DirR_D = DirR_D/max(sqrt(sum(DirR_D**2)),1e-30)
     
       if(sum(BDir_D*DirR_D) <  0.0)then
          BDir_D = -BDir_D
          Major_ = WaveLast_
          Minor_ = WaveFirst_
       else
          Major_ = WaveFirst_
          Minor_ = WaveLast_
       end if
       !\
       ! Calculate input parameters for solving the thread
       !/
       if(DoTestMe.and.j==jTest.and.k==kTest)then
          write(*,*)'Direction B=', BDir_D
          write(*,*)'Direction R=', DirR_D
          write(*,*)'Magnetic fields: B0True =', B0_DGB(:, 1, j, k, iBlock) 
          write(*,*)'Magnetic fields: B0Ghost=', B0_DGB(:, 0, j, k, iBlock) 
          write(*,*)'B1_D=', B1_D
          write(*,*)NameSub//': before limiting TeGhost=',&
               Te_G(0, j, k)*No2Si_V(UnitTemperature_)
       end if
       Te_G(0, j, k) = max(TeMin,min(Te_G(0, j, k), &
            BoundaryThreads_B(iBlock) % TMax_II(j,k)))
       UAbsMax = 0.10*sqrt(Te_G(0,j,k))
       TeSi = Te_G(0, j, k)*No2Si_V(UnitTemperature_)
       SqrtRho = sqrt(State_VGB(Rho_, 1, j, k, iBlock))
       AMinor = min(1.0,&
            sqrt(  State_VGB(Minor_, 1, j, k, iBlock)/&
            (SqrtRho* PoyntingFluxPerB)  ))
       U_D = State_VGB(RhoUx_:RhoUz_, 1, j, k, iBlock)/&
            State_VGB(Rho_, 1, j, k, iBlock)
       U = sum(U_D*BDir_D)
       U = sign(min(abs(U), UAbsMax), U)
       if(DoTestMe.and.j==jTest.and.k==kTest)then
          write(*,*)'U_D=',U_D
          write(*,*)'Direction U', U_D/sqrt(sum(U_D**2))
       end if

       PeSi = PeFraction*State_VGB(iP, 1, j, k, iBlock)&
            *(Te_G(0,j,k)/Te_G(1,j,k))*No2Si_V(UnitEnergyDens_)
       call solve_boundary_thread(j=j, k=k, iBlock=iBlock, iAction=iAction,    &
            TeSiIn=TeSi, PeSiIn=PeSi, USiIn=U*No2Si_V(UnitU_), AMinorIn=AMinor,&
            DTeOverDsSiOut=DTeOverDsSi, PeSiOut=PeSiOut,&
            RhoNoDimOut=RhoNoDimOut, AMajorOut=AMajor)
       if(present(iImplBlock))then
          DTeOverDs = DTeOverDsSi * Si2No_V(UnitTemperature_)/Si2No_V(UnitX_)
          !\
          ! Solve equation: -(TeGhost-TeTrue)/DeltaR = 
          ! dTe/ds*(b . DirR)
          !/
          Te_G(0, j, k) = Te_G(0, j, k) - DTeOverDs/max(&                
               sum(BDir_D*DirR_D),0.7)*&
               BoundaryThreads_B(iBlock)% DeltaR_II(j,k)
          !\
          ! Version Easter 2015 Limit TeGhost
          !/
          Te_G(0, j, k) = max(TeMin,min(Te_G(0, j, k), &
               BoundaryThreads_B(iBlock) % TMax_II(j,k)))
          if(DoTestMe.and.j==jTest.and.k==kTest)then
             write(*,*)NameSub//': reset TeGhost=',&
                  Te_G(0, j, k)*No2Si_V(UnitTemperature_)
          end if
          State_VG(iTeImpl, 0, j, k) = Te_G(0, j, k)
          CYCLE
       end if
 
       State_VG(iP,0,j,k) = PeSiOut*Si2No_V(UnitEnergyDens_)/PeFraction
       !\
       !Extrapolation of pressure
       !/
       State_VG(iP, 1-nGhost:-1, j, k) = State_VG(iP,0,j,k)**2/&
            State_VG(iP,1,j,k)
       !\
       ! Assign ion pressure (if separate from electron one)
       !/
       if(iP/=p_)State_VG(p_, 1-nGhost:0, j, k) = &
            State_VG(iP, 1-nGhost:0, j, k)/Z

       State_VG(Rho_, 0, j, k) = RhoNoDimOut
       !\
       !Extrapolation of density
       !/
       State_VG(Rho_, 1-nGhost:-1, j, k) = State_VG(Rho_, 0, j, k)**2&
            /State_VG(Rho_,1,j,k) 
   
       do i = 1-nGhost, 0
          !\
          ! Ghost cell value of the magnetic field: cancel radial B1 field 
          !/ 
          B1_D = State_VG(Bx_:Bz_, 1-i, j, k)
          State_VG(Bx_:Bz_, i, j, k) = B1_D - DirR_D*sum(DirR_D*B1_D)
          if(UseCME)then
             !\
             ! Maintain the normal component of the superimposed 
             ! CME magnetic configuration
             !/
             call EEE_get_state_BC(Xyz_DGB(:,i,j,k,iBlock), &
                  RhoCme, Ucme_D, Bcme_D, pCme, &
                  time_simulation, n_step, iteration_number)
             State_VG(Bx_:Bz_, i, j, k) = &
                  State_VG(Bx_:Bz_, i, j, k) + DirR_D*sum(DirR_D*Bcme_D)
          end if
          !\
          !Gnost cell value of velocity: keep the velocity projection
          !onto the magnetic field, if UseAlignedVelocity=.true.
          !Reflect the other components
          !/
          U_D = State_VG(RhoUx_:RhoUz_,1-i,j,k)/State_VG(Rho_,1-i,j,k)
          if(UseAlignedVelocity)then
             U   = sum(U_D*BDir_D); U_D = U_D - U*BDir_D
             U   = sign(min(abs(U), UAbsMax), U)
          else
             U = 0
          end if     
          State_VG(RhoUx_:RhoUz_, i, j, k) = -U_D*State_VG(Rho_,i,j,k) &
                + U*BDir_D*State_VG(Rho_,i,j,k)
              
          State_VG(Major_, i, j, k) = AMajor**2 * PoyntingFluxPerB *&
               sqrt( State_VG(Rho_, i, j, k) )
       end do

       if(Ehot_ > 1)then
          if(UseHeatFluxCollisionless)then
             call get_gamma_collisionless(Xyz_DGB(:,1,j,k,iBlock), GammaHere)
             State_VG(Ehot_,1-nGhost:0,j,k) = &
                  State_VG(iP,1-nGhost:0,j,k)*&
                  (1.0/(GammaHere - 1) - InvGammaElectronMinus1)
          else
             State_VG(Ehot_,1-nGhost:0,j,k) = 0.0
          end if
       end if
    end do; end do
    BoundaryThreads_B(iBlock)%iAction = Done_
  end subroutine set_field_line_thread_bc
end module ModThreadedLC
