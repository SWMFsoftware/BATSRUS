!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission For more information, see
!  http://csem.engin.umich.edu/tools/swmf
module ModUser

  use BATL_lib, ONLY: &
       test_start, test_stop, iProc, lVerbose, nI, nJ, nK

  use ModChromosphere, ONLY: tChromoSi=>TeChromosphereSi, &
       tChromo => TeChromosphere
  use ModTurbulence, ONLY: PoyntingFluxPerB, PoyntingFluxPerBSi, &
       IsOnAwRepresentative
  use ModUserEmpty,                                     &
       IMPLEMENTED1 => user_read_inputs,                &
       IMPLEMENTED2 => user_init_session,               &
       IMPLEMENTED3 => user_set_ics,                    &
       IMPLEMENTED4 => user_get_log_var,                &
       IMPLEMENTED5 => user_set_plot_var,               &
       IMPLEMENTED6 => user_set_cell_boundary,          &
       IMPLEMENTED9 => user_initial_perturbation,       &
       IMPLEMENTED11=> user_get_b0,                     &
       IMPLEMENTED12=> user_material_properties,        &
       IMPLEMENTED13=> user_calc_sources_expl

  include 'user_module.h' ! list of public methods

  character (len=*), parameter :: NameUserFile = "ModUserAwsom.f90"
  character (len=*), parameter :: NameUserModule = 'AWSoM(R) model'

  logical :: UseAwsom = .false.
  !$acc declare create(UseAwsom)

  ! Input parameters for chromospheric inner BC's
  real    :: ChromoNSi = 2e17   ! tChromoSi = 5e4
  real    :: ChromoN, RhoChromo
  !$acc declare create(ChromoN)

  ! variables for Parker initial condition
  real    :: CoronaN = 1.5e14, tCoronaSi = 1.5e6
  real    :: RhoCorona, tCorona

  ! Input parameters for two-temperature effects
  real    :: TeFraction, TiFraction

  ! Extra dipole under surface
  real    :: UserDipoleStrength = 0., UserDipoleStrengthSi = 0.
  real    :: UserDipoleDepth = 1.
  real    :: UserDipoleLatitude, UserDipoleLatDeg = 0.
  real    :: UserDipoleLongitude, UserDipoleLonDeg = 0.
  real    :: UserDipoleAxisLatitude, UserDipoleAxisLatDeg = 0.
  real    :: UserDipoleAxisLongitude, UserDipoleAxisLonDeg = 0.

  ! Uniform B0
  real    :: UniformB0Si_D(3) = 0.0

  ! STITCH
  real    :: ZetaSI = 0.0, Zeta, rStitch = 1.0
  logical :: UseStitchRegion = .false., DoStitchOneCell = .false.
  real    :: Lon0Deg, Lon1Deg, Lat0Deg, Lat1Deg
  real    :: Lon0, Lon1, Lat0, Lat1
  real    :: tStartStitch = -1.0, tStopStitch = -1.0

  logical :: UseFloatRadialVelocity = .false.
  !$acc declare create(UseFloatRadialVelocity)

  ! Rotating boundary condition
  real:: FlowSpeedJet =0.0, FlowSpeedJetSi =0.0
  real:: tBeginJet = 0.0, tEndJet = 0.0
  real:: BminJet=0.0, BminJetSi = 0.0, BmaxJet = 0.0, BmaxJetSi = 0.0
  real:: CoefBJet = 0.0
  real:: BrampJet = 1.0              ! ramp up of magnetic field?
  integer:: iBcMax = 0            ! Index up to which BC is applied
  logical:: DoRampUpJet = .false.
  logical:: IsUr0Jet = .false.
  logical:: DoUpdateParkerJet = .true.

  ! Different mechanisms for radioemission
  ! 'simplistic' - interpolation between Bremsstrahlung and contributions
  ! from non-thermal emission at critical and quarter-of-critical,
  ! densities, the different contributions being weighted quite arbitrarily.
  ! 'bremsstrahlung' - emission due to the electron-ion collisions

  character(len=20):: TypeRadioEmission = 'simplistic'
contains
  !============================================================================
  subroutine user_read_inputs

    use ModChromosphere, ONLY: NumberDensChromosphereCgs
    use ModReadParam, ONLY: read_line, read_command, read_var
    use ModIO, ONLY: write_prefix, write_myname, iUnitOut
    use ModMain, ONLY: UseRotatingFrame

    character(len=100) :: NameCommand
    integer:: iDir
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_read_inputs'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(iProc == 0 .and. lVerbose > 0)then
       call write_prefix;
       write(iUnitOut,*)'User read_input CHROMOSPHERE-CORONA starts'
    endif

    do
       if(.not.read_line() ) EXIT
       if(.not.read_command(NameCommand)) CYCLE

       select case(NameCommand)
          ! This command is used when the inner boundary is the chromosphere
       case("#CHROMOBC")
          call read_var('ChromoNSi', ChromoNSi)
          NumberDensChromosphereCgs = ChromoNSi*1.0e-6
          call read_var('tChromoSi', tChromoSi)

       case('#RADIOEMISSION')
          call read_var('TypeRadioEmission',TypeRadioEmission)

       case("#PARKERIC")
          call read_var('CoronaN', CoronaN)
          call read_var('tCoronaSi', tCoronaSi)

       case("#EXTRADIPOLE")
          call read_var('UserDipoleStrengthSi', UserDipoleStrengthSi)
          call read_var('UserDipoleDepth', UserDipoleDepth)
          call read_var('UserDipoleLatitude',UserDipoleLatDeg)
          call read_var('UserDipoleLongitude',UserDipoleLonDeg)
          call read_var('UserDipoleAxisLatitude',UserDipoleAxisLatDeg)
          call read_var('UserDipoleAxisLongitude',UserDipoleAxisLonDeg)

       case('#UNIFORMB0')
          do iDir = 1, 3
             call read_var('UniformB0Si', UniformB0Si_D(iDir))
          end do

       case('#ROTATINGFRAME')
          call read_var('UseRotatingFrame',UseRotatingFrame)

       case('#POLARJETBOUNDARY')
          call read_var('FlowSpeedJetSi',FlowSpeedJetSi)
          call read_var('tBeginJet', tBeginJet)
          call read_var('tEndJet',   tEndJet)
          call read_var('BminJetSi', BminJetSi)
          call read_var('BmaxJetSi', BmaxJetSi)
          call read_var('CoefBJet',     CoefBJet)
          call read_var('iBcMax',    iBcMax)
          call read_var('DoRampUpJet', DoRampUpJet)
          call read_var('IsUr0Jet', IsUr0Jet)
          call read_var('DoUpdateParkerJet', DoUpdateParkerJet)
          call read_var('BrampJet', BrampJet)

       case("#STITCH")
          call read_var('ZetaSI', ZetaSI)
          call read_var('rStitch', rStitch)
          call read_var('tStartStitch', tStartStitch)
          call read_var('tStopStitch', tStopStitch)
          ! For now negative rStitch means to sticth up to |rStitch|
          DoStitchOneCell = rStitch > 0.0
          rStitch = abs(rStitch)

       case("#STITCHREGION") ! input in degrees
          call read_var('UseStitchRegion', UseStitchRegion)
          if(UseStitchRegion)then
             call read_var('Longitude0', Lon0Deg)
             call read_var('Longitude1', Lon1Deg)
             call read_var('Latitude0', Lat0Deg)
             call read_var('Latitude1', Lat1Deg)
          end if

       case("#FLOATRADIALVELOCITY")
          call read_var('UseFloatRadialVelocity', UseFloatRadialVelocity)

       case('#USERINPUTEND')
          if(iProc == 0 .and. lVerbose > 0)then
             call write_prefix;
             write(iUnitOut,*)'User read_input SOLAR CORONA ends'
          endif
          EXIT

       case default
          if(iProc == 0) then
             call write_myname; write(*,*) &
                  'ERROR: Invalid user defined #COMMAND in user_read_inputs. '
             write(*,*) '--Check user_read_inputs for errors'
             write(*,*) '--Check to make sure a #USERINPUTEND command was used'
             write(*,*) '  *Unrecognized command was: '//NameCommand
             call stop_mpi('ERROR: Correct PARAM.in or user_read_inputs!')
          end if
       end select
    end do

    call test_stop(NameSub, DoTest)
  end subroutine user_read_inputs
  !============================================================================
  subroutine user_init_session

    use EEE_ModCommonVariables, ONLY: UseCme
    use EEE_ModMain, ONLY: EEE_initialize
    use BATL_lib, ONLY: iComm
    use ModMain, ONLY: tSimulation, TypeCellBc_I, TypeFaceBc_I, &
         Body1_, UseFieldLineThreads
    use ModTransitionRegion, ONLY: check_tr_param
    use ModGeometry, ONLY: RadiusMin
    use ModTurbulence, ONLY: rMinWaveReflection, QionRatio_I, QionParRatio_I,&
         UseStochasticHeating, StochasticExponent, StochasticAmplitude, &
         StochasticExponent2, StochasticAmplitude2
    use ModIO, ONLY: write_prefix, iUnitOut
    use ModWaves, ONLY: UseWavePressure, UseAlfvenWaves
    use ModAdvance, ONLY: UseElectronPressure
    use ModVarIndexes, ONLY: WaveFirst_
    use ModMultiFluid, ONLY: MassIon_I
    use ModConst, ONLY: cElectronCharge, cLightSpeed, cBoltzmann, cEps, &
         cElectronMass
    use ModNumConst, ONLY: cTwoPi, cDegToRad
    use ModPhysics, ONLY: ElectronTemperatureRatio, AverageIonCharge, &
         Si2No_V, UnitTemperature_, UnitN_, UnitB_, BodyNDim_I, BodyTDim_I, &
         UnitX_, UnitT_, Gamma, UnitEnergyDens_, UnitU_

    real, parameter :: CoulombLog = 20.0
    real :: QparPerQtotal, QperpPerQtotal
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_init_session'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(iProc == 0)then
       call write_prefix; write(iUnitOut,*) ''
       call write_prefix; write(iUnitOut,*) 'user_init_session:'
       call write_prefix; write(iUnitOut,*) ''
    end if

    UseAwsom = TypeCellBc_I(1) == 'user' .and. TypeFaceBc_I(body1_) == 'none'
    !$acc update device(UseAwsom)

    UseAlfvenWaves  = WaveFirst_ > 1
    UseWavePressure = WaveFirst_ > 1

    PoyntingFluxPerB = PoyntingFluxPerBSi &
         *Si2No_V(UnitEnergyDens_)*Si2No_V(UnitU_)/Si2No_V(UnitB_)

    ! convert to normalized units
    ChromoN = ChromoNSi*Si2No_V(UnitN_)
    RhoChromo = ChromoN*MassIon_I(1)
    tChromo = tChromoSi*Si2No_V(UnitTemperature_)

    ! Density and temperature in normalized units
    RhoCorona = CoronaN*Si2No_V(UnitN_)*MassIon_I(1)
    tCorona   = tCoronaSi*Si2No_V(UnitTemperature_)

    ! polar jet in normalized units
    UserDipoleStrength = UserDipoleStrengthSi*Si2No_V(UnitB_)
    UserDipoleLatitude = UserDipoleLatDeg*cDegToRad
    UserDipoleLongitude = UserDipoleLonDeg*cDegToRad
    UserDipoleAxisLatitude = UserDipoleAxisLatDeg*cDegToRad
    UserDipoleAxisLongitude = UserDipoleAxisLonDeg*cDegToRad

    BminJet = BminJetSi*Si2No_V(UnitB_)
    BmaxJet = BmaxJetSi*Si2No_V(UnitB_)
    FlowSpeedJet = FlowSpeedJetSi &
         * Si2No_V(UnitX_)**2 / Si2No_V(UnitT_)/Si2No_V(UnitB_)

    ! TeFraction is used for ideal EOS:
    if(UseElectronPressure)then
       ! Pe = ne*Te (dimensionless) and n=rho/ionmass
       ! so that Pe = ne/n *n*Te = (ne/n)*(rho/ionmass)*Te
       ! TeFraction is defined such that Te = Pe/rho * TeFraction
       TiFraction = MassIon_I(1)
       TeFraction = MassIon_I(1)/AverageIonCharge
    else
       ! p = n*T + ne*Te (dimensionless) and n=rho/ionmass
       ! so that p=rho/massion *T*(1+ne/n Te/T)
       ! TeFraction is defined such that Te = p/rho * TeFraction
       TiFraction = MassIon_I(1) &
            /(1 + AverageIonCharge*ElectronTemperatureRatio)
       TeFraction = TiFraction*ElectronTemperatureRatio
    end if

    if(UseCme) call EEE_initialize(BodyNDim_I(1), BodyTDim_I(1), Gamma,&
         iCommIn=iComm, TimeNow = tSimulation)

    ! ZetaSI is in SI unit of [meter^2*second^(-1)]
    ! ZetaSI = 1.4E14 [m^2/s]
    Zeta = ZetaSI*Si2No_V(UnitX_)*Si2No_V(UnitU_)

    if(UseStitchRegion)then
       Lon0 = Lon0Deg*cDegToRad
       Lon1 = Lon1Deg*cDegToRad
       Lat0 = Lat0Deg*cDegToRad
       Lat1 = Lat1Deg*cDegToRad
    end if
    if(UseFieldLineThreads)then
       if(UseStochasticHeating)then
          call check_tr_param(&
               rMaxIn = RadiusMin, & ! Max radius of TR gap = MinRadius (SC)
               rMinReflectionTrIn = rMinWaveReflection, &
               tCoronaIn = tCoronaSi,                 &
               pCoronaIn = 2*cBoltzmann*tCoronaSi*CoronaN, &
               UseStochasticHeatingIn = UseStochasticHeating, &
               StochasticExponentIn = StochasticExponent,     &
               StochasticAmplitudeIn = StochasticAmplitude,   &
               StochasticExponent2In = StochasticExponent2,   &
               StochasticAmplitude2In = StochasticAmplitude2)
       else
          if(QionParRatio_I(1)==0.0)then
             QparPerQtotal = QionRatio_I(1)/3
             QperpPerQtotal = 2*QparPerQtotal
          else
             QparPerQtotal = QionParRatio_I(1)
             QperpPerQtotal = QionRatio_I(1) - QionParRatio_I(1)
          end if
          call check_tr_param(&
               rMaxIn = RadiusMin, & ! Max radius of TR gap = MinRadius (SC)
               rMinReflectionTrIn = rMinWaveReflection, &
               tCoronaIn = tCoronaSi,                   &
               pCoronaIn = 2*cBoltzmann*tCoronaSi*CoronaN, &
               UseStochasticHeatingIn = UseStochasticHeating, &
               QparPerQtotalIn = QparPerQtotal, &
               QperpPerQtotalIn = QperpPerQtotal)
       end if
    end if
    if(iProc == 0)then
       call write_prefix; write(iUnitOut,*) ''
       call write_prefix; write(iUnitOut,*) 'user_init_session finished'
       call write_prefix; write(iUnitOut,*) ''
    end if

    !$acc update device(tChromo, UseFloatRadialVelocity, ChromoN)

    call test_stop(NameSub, DoTest)
  end subroutine user_init_session
  !============================================================================
  subroutine user_set_ics(iBlock)

    ! The isothermal parker wind solution is used as initial condition

    use ModAdvance, ONLY: State_VGB, UseElectronPressure, UseAnisoPressure
    use ModB0, ONLY: B0_DGB
    use ModTurbulence, ONLY: UseTurbulentCascade, LperpTimesSqrtB
    use ModGeometry, ONLY: r_GB
    use ModMultiFluid, ONLY: MassIon_I
    use ModPhysics, ONLY: rBody, GBody, AverageIonCharge
    use ModVarIndexes, ONLY: Rho_, RhoUx_, RhoUy_, RhoUz_, Bx_, Bz_, p_, Pe_, &
         Ppar_, WaveFirst_, WaveLast_, SignB_, Lperp_
    use ModSaMhd, ONLY: UseSaMhd
    use ModWaves, ONLY: UseAlfvenWaves
    use BATL_lib, ONLY: Xyz_DGB

    integer, intent(in) :: iBlock

    integer :: i, j, k
    real :: x, y, z, r, Rho, NumDensIon, NumDensElectron
    real :: uCorona
    real :: r_D(3), Br
    ! variables for iterative Parker solution
    integer :: nIterParker
    real :: Ur, Ur0, Ur1, dUr, rTransonic, Uescape, Usound
    real :: Coef, rParker, Temperature

    real, parameter :: Epsilon = 1.0e-6
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_ics'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    rParker = -1.0
    if(ChromoNSi > CoronaN .and. UseAwsom)then
       ! In the following, we do not generate a jump in the density,
       ! but we do connect a exponentially stratified atmosphere with
       ! the Parker solution at rParker. This avoids problems with a
       ! strong density jump at the innner boundary otherwise.
       Coef = -GBody/rBody*MassIon_I(1)/Tchromo
       rParker = rBody/(1.0 + log(CoronaN/ChromoNSi)/Coef)
    end if

    ! normalize with isothermal sound speed.
    Usound  = sqrt(tCorona*(1.0 + AverageIonCharge)/MassIon_I(1))
    Uescape = sqrt(-GBody*2.0)/Usound

    ! Initialize MHD wind with Parker's solution
    ! construct solution which obeys
    !   rho x u_r x r^2 = constant
    rTransonic = 0.25*Uescape**2
    if(.not.(rTransonic>exp(1.0)))then
       write(*,*) NameSub, 'Gbody=', Gbody
       write(*,*) NameSub,' CoronaN, RhoCorona =', CoronaN, RhoCorona
       write(*,*) NameSub,' TcoronaSi, Tcorona   =', TcoronaSi, Tcorona
       write(*,*) NameSub,' Usound    =', Usound
       write(*,*) NameSub,' Uescape   =', Uescape
       write(*,*) NameSub,' rTransonic=', rTransonic
       call stop_mpi(NameSub//'sonic point inside Sun')
    end if

    uCorona = rTransonic**2*exp(1.5 - 2.0*rTransonic)

    do k = MinK,MaxK ; do j = MinJ,MaxJ ; do i = MinI,MaxI
       x = Xyz_DGB(x_,i,j,k,iBlock)
       y = Xyz_DGB(y_,i,j,k,iBlock)
       z = Xyz_DGB(z_,i,j,k,iBlock)
       r = r_GB(i,j,k,iBlock)
       r_D = [x,y,z]

       if(r < rParker .and. UseAwsom)then
          ! Atmosphere with exponential scaleheight (AWSoM only)
          Ur = 0.0
          Rho = ChromoN*MassIon_I(1)*exp(Coef*(rBody/r - 1.0))
          Temperature = Tchromo
       else
          ! Construct 1D Parker solution
          if(r > rTransonic)then

             ! Inside supersonic region
             Ur0 = 1.0
             nIterParker = 0
             do
                nIterParker = nIterParker + 1
                Ur1 = sqrt(Uescape**2/r - 3.0 &
                     + 2.0*log(16.0*Ur0*r**2/Uescape**4))
                dUr = abs(Ur1 - Ur0)
                if(dUr < Epsilon)then
                   Ur = Ur1
                   EXIT
                elseif(nIterParker < 1000)then
                   Ur0 = Ur1
                   CYCLE
                else
                   call stop_mpi('PARKER > 1000 it.')
                end if
             end do
          else

             ! Inside subsonic region
             Ur0 = 1.0
             nIterParker = 0
             do
                nIterParker = nIterParker + 1
                Ur1 = (Uescape**2/(4.0*r))**2 &
                     *exp(0.5*(Ur0**2 + 3.0 - Uescape**2/r))
                dUr = abs(Ur1 - Ur0)
                if(dUr < Epsilon)then
                   Ur = Ur1
                   EXIT
                elseif(nIterParker < 1000)then
                   Ur0 = Ur1
                   CYCLE
                else
                   call CON_stop('PARKER > 1000 it.')
                end if
             end do
          end if

          Rho = rBody**2*RhoCorona*uCorona/(r**2*Ur)
          Temperature = tCorona
       end if

       NumDensIon = Rho/MassIon_I(1)
       NumDensElectron = NumDensIon*AverageIonCharge

       if(UseElectronPressure)then
          State_VGB(p_,i,j,k,iBlock) = NumDensIon*Temperature
          State_VGB(Pe_,i,j,k,iBlock) = NumDensElectron*Temperature
          if(UseAnisoPressure) &
               State_VGB(Ppar_,i,j,k,iBlock) = State_VGB(p_,i,j,k,iBlock)
       else
          State_VGB(p_,i,j,k,iBlock) = &
               (NumDensIon + NumDensElectron)*Temperature
       end if
       State_VGB(Rho_,i,j,k,iBlock) = Rho

       State_VGB(RhoUx_,i,j,k,iBlock) = Rho*Ur*x/r *Usound
       State_VGB(RhoUy_,i,j,k,iBlock) = Rho*Ur*y/r *Usound
       State_VGB(RhoUz_,i,j,k,iBlock) = Rho*Ur*z/r *Usound

       State_VGB(Bx_:Bz_,i,j,k,iBlock) = 0.0
       if(UseSaMhd)State_VGB(SignB_,i,j,k,iBlock) = &
            sum(B0_DGB(1:3,i,j,k,iBlock)*r_D) / (r*Usound)
       if(UseTurbulentCascade.and.Lperp_>1)&
            State_VGB(Lperp_,i,j,k,iBlock) = LperpTimesSqrtB
       if(UseAlfvenWaves)then
          Br = sum(B0_DGB(1:3,i,j,k,iBlock)*r_D)
          if (Br >= 0.0) then
             State_VGB(WaveFirst_,i,j,k,iBlock) = PoyntingFluxPerB*sqrt(Rho)
             if(UseTurbulentCascade)then
                State_VGB(WaveLast_,i,j,k,iBlock) = &
                     1e-3*State_VGB(WaveFirst_,i,j,k,iBlock)
             else
                State_VGB(WaveLast_,i,j,k,iBlock) = 1e-30
             end if
          else
             State_VGB(WaveLast_,i,j,k,iBlock) = PoyntingFluxPerB*sqrt(Rho)
             if(UseTurbulentCascade)then
                State_VGB(WaveFirst_,i,j,k,iBlock) = &
                     1e-3*State_VGB(WaveLast_,i,j,k,iBlock)
             else
                State_VGB(WaveFirst_,i,j,k,iBlock) = 1e-30
             end if
          end if
       end if

    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_ics
  !============================================================================
  subroutine user_get_log_var(VarValue, TypeVar, Radius)

    use ModAdvance, ONLY: State_VGB, Tmp1_GB, UseElectronPressure
    use ModB0, ONLY: B0_DGB
    use ModIO, ONLY: write_myname
    use ModMain, ONLY: UseB0
    use ModPhysics, ONLY: InvGammaMinus1, No2Io_V, UnitEnergydens_, &
         UnitX_, UnitT_, UnitTemperature_, GammaWave
    use ModVarIndexes, ONLY: Bx_, By_, Bz_, p_, Pe_, WaveLast_, WaveFirst_, &
         Rho_, RhoUx_, RhoUz_
    use BATL_lib, ONLY: integrate_grid, Unused_B, nBlock, x_, y_, z_, &
         Xyz_DGB, StringTest, maxval_grid, minval_grid, nProc
    use ModGeometry, ONLY: r_GB
    use ModWriteLogSatFile, ONLY: calc_sphere
    use ModCoordTransform, ONLY: cross_product
    use ModCurrent, ONLY: get_current

    real, intent(out) :: VarValue
    character(len=10), intent(in) :: TypeVar
    real, optional, intent(in) :: Radius

    integer :: iBlock, i, j, k, iLoc_I(5)
    real :: UnitEnergy, Wmajor, Wminor
    real :: FullB_D(3), SignBr, rUnit_D(3), Rho, Ur, Var, Current_D(3), b_D(3)
    logical:: DoTest

    character(len=*), parameter:: NameSub = 'user_get_log_var'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    UnitEnergy = No2Io_V(UnitEnergydens_)*No2Io_V(UnitX_)**3

    ! Define log variable to be saved::
    select case(TypeVar)
    case('eint')
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          if(UseElectronPressure)then
             Tmp1_GB(:,:,:,iBlock) = &
                  State_VGB(p_,:,:,:,iBlock) + State_VGB(Pe_,:,:,:,iBlock)
          else
             Tmp1_GB(:,:,:,iBlock) = State_VGB(p_,:,:,:,iBlock)
          end if
       end do
       VarValue = UnitEnergy*InvGammaMinus1*integrate_grid(Tmp1_GB)

    case('emag')
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          if(UseB0)then
             Tmp1_GB(:,:,:,iBlock) = &
                  ( B0_DGB(x_,:,:,:,iBlock) + State_VGB(Bx_,:,:,:,iBlock))**2 &
                  +(B0_DGB(y_,:,:,:,iBlock) + State_VGB(By_,:,:,:,iBlock))**2 &
                  +(B0_DGB(z_,:,:,:,iBlock) + State_VGB(Bz_,:,:,:,iBlock))**2
          else
             Tmp1_GB(:,:,:,iBlock) = State_VGB(Bx_,:,:,:,iBlock)**2 &
                  + State_VGB(By_,:,:,:,iBlock)**2 &
                  + State_VGB(Bz_,:,:,:,iBlock)**2
          end if
       end do
       VarValue = UnitEnergy*0.5*integrate_grid(Tmp1_GB)

    case('emag1')
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          Tmp1_GB(:,:,:,iBlock) = State_VGB(Bx_,:,:,:,iBlock)**2 &
               + State_VGB(By_,:,:,:,iBlock)**2 &
               + State_VGB(Bz_,:,:,:,iBlock)**2
       end do
       VarValue = 0.5*integrate_grid(Tmp1_GB)

    case('absjxb')
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          do k = 1,nK ; do j = 1,nJ ; do i = 1,nI

             if(UseB0) then
                b_D = B0_DGB(:,i,j,k,iBlock) + State_VGB(Bx_:Bz_,i,j,k,iBlock)
             else
                b_D =  State_VGB(Bx_:Bz_,i,j,k,iBlock)
             endif
             call get_current(i, j, k, iBlock, Current_D)
             Tmp1_GB(i,j,k,iBlock) =  norm2(cross_product(current_D, b_D))
          enddo; enddo; enddo
       end do
       VarValue = integrate_grid(Tmp1_GB)

    case('vol')
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE

          Tmp1_GB(:,:,:,iBlock) = 1.0
       end do
       VarValue = integrate_grid(Tmp1_GB)

    case('wflx')
       do iBlock=1,nBlock
          if(Unused_B(iBlock))CYCLE
          do k = 0, nK+1; do j = 0, nJ+1; do i= 0, nI+1
             FullB_D = State_VGB(Bx_:Bz_,i,j,k,iBlock) + B0_DGB(:,i,j,k,iBlock)
             SignBr = sign(1.0, sum(Xyz_DGB(:,i,j,k,iBlock)*FullB_D))
             if(SignBr < 0.0)then
                Wmajor=State_VGB(WaveLast_,i,j,k,iBlock)
                Wminor=State_VGB(WaveFirst_,i,j,k,iBlock)
             else
                Wmajor=State_VGB(WaveFirst_,i,j,k,iBlock)
                Wminor=State_VGB(WaveLast_,i,j,k,iBlock)
             end if
             rUnit_D = Xyz_DGB(:,i,j,k,iBlock)/r_GB(i,j,k,iBlock)
             Rho = State_VGB(Rho_,i,j,k,iBlock)
             Var = abs(sum(rUnit_D*FullB_D)/sqrt(Rho))
             Ur  = sum(rUnit_D*State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock))/ &
                  Rho

             Tmp1_GB(i,j,k,iBlock) =               &
                  Wmajor*(GammaWave*Ur + Var)     &
                  +  Wminor*(GammaWave*Ur - Var)
          end do; end do; end do
       end do
       VarValue = calc_sphere('integrate',360, Radius, Tmp1_GB) &
            *UnitEnergy/No2Io_V(UnitT_)

    case('wplusflx')
       do iBlock=1,nBlock
          if(Unused_B(iBlock))CYCLE
          do k = 0, nK+1; do j = 0, nJ+1; do i= 0, nI+1
             FullB_D = State_VGB(Bx_:Bz_,i,j,k,iBlock) + B0_DGB(:,i,j,k,iBlock)
             SignBr = sign(1.0, sum(Xyz_DGB(:,i,j,k,iBlock)*FullB_D))
             if(SignBr < 0.0)then
                Wmajor=State_VGB(WaveLast_,i,j,k,iBlock)
                Wminor=State_VGB(WaveFirst_,i,j,k,iBlock)
             else
                Wmajor=State_VGB(WaveFirst_,i,j,k,iBlock)
                Wminor=State_VGB(WaveLast_,i,j,k,iBlock)
             end if
             rUnit_D = Xyz_DGB(:,i,j,k,iBlock)/r_GB(i,j,k,iBlock)
             Rho = State_VGB(Rho_,i,j,k,iBlock)
             Var = abs(sum(rUnit_D*FullB_D)/sqrt(Rho))
             Ur  = sum(rUnit_D*State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock))/ &
                  Rho

             Tmp1_GB(i,j,k,iBlock) = Wmajor*(GammaWave*Ur + Var)

          end do; end do; end do
       end do
       VarValue = calc_sphere('integrate',360, Radius, Tmp1_GB) &
            *UnitEnergy/No2Io_V(UnitT_)

    case('wminusflx')
       do iBlock=1,nBlock
          if(Unused_B(iBlock))CYCLE
          do k = 0, nK+1; do j = 0, nJ+1; do i= 0, nI+1
             FullB_D = State_VGB(Bx_:Bz_,i,j,k,iBlock) + B0_DGB(:,i,j,k,iBlock)
             SignBr = sign(1.0, sum(Xyz_DGB(:,i,j,k,iBlock)*FullB_D))
             if(SignBr < 0.0)then
                Wmajor=State_VGB(WaveLast_,i,j,k,iBlock)
                Wminor=State_VGB(WaveFirst_,i,j,k,iBlock)
             else
                Wmajor=State_VGB(WaveFirst_,i,j,k,iBlock)
                Wminor=State_VGB(WaveLast_,i,j,k,iBlock)
             end if
             rUnit_D = Xyz_DGB(:,i,j,k,iBlock)/r_GB(i,j,k,iBlock)
             Rho = State_VGB(Rho_,i,j,k,iBlock)
             Var = abs(sum(rUnit_D*FullB_D)/sqrt(Rho))
             Ur  = sum(rUnit_D*State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock))/ &
                  Rho

             Tmp1_GB(i,j,k,iBlock) = Wminor*(GammaWave*Ur - Var)
          end do; end do; end do
       end do
       VarValue = calc_sphere('integrate',360, Radius, Tmp1_GB) &
            *UnitEnergy/No2Io_V(UnitT_)

    case('keflx')
       do iBlock=1,nBlock
          if(Unused_B(iBlock))CYCLE
          do k = 0, nK+1; do j = 0, nJ+1; do i= 0, nI+1
             rUnit_D = Xyz_DGB(:,i,j,k,iBlock)/r_GB(i,j,k,iBlock)
             Rho = State_VGB(Rho_,i,j,k,iBlock)
             Ur  = sum(rUnit_D*State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock))/ &
                  Rho

             Tmp1_GB(i,j,k,iBlock) = Rho*Ur**3 / 2.0
          end do; end do; end do
       end do
       VarValue = calc_sphere('integrate',360, Radius, Tmp1_GB) &
            *UnitEnergy/No2Io_V(UnitT_)
    case('temin')
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          do k = 1,nK ; do j = 1,nJ ; do i = 1,nI
             if(UseElectronPressure)then
                Tmp1_GB(i,j,k,iBlock) = State_VGB(Pe_,i,j,k,iBlock)/&
                     State_VGB(Rho_,i,j,k,iBlock)
             else
                Tmp1_GB(i,j,k,iBlock) = State_VGB(p_,i,j,k,iBlock)/&
                     State_VGB(Rho_,i,j,k,iBlock)
             end if
          enddo; enddo; enddo
       end do
       VarValue = TeFraction*minval_grid(Tmp1_GB, iLoc_I=iLoc_I)*No2Io_V(&
            UnitTemperature_)
       ! if(index(StringTest,'show_temin')>0.and.iLoc_I(5)==iProc)&
       !     write(*,*)'TeMin, x, y, z=', VarValue, &
       !     Xyz_DGB(:,iLoc_I(1),iLoc_I(2),iLoc_I(3),iLoc_I(4))
       VarValue = VarValue/nProc
    case('temax')
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          do k = 1,nK ; do j = 1,nJ ; do i = 1,nI
             if(UseElectronPressure)then
                Tmp1_GB(i,j,k,iBlock) = State_VGB(Pe_,i,j,k,iBlock)/&
                     State_VGB(Rho_,i,j,k,iBlock)
             else
                Tmp1_GB(i,j,k,iBlock) = State_VGB(p_,i,j,k,iBlock)/&
                     State_VGB(Rho_,i,j,k,iBlock)
             end if
          enddo; enddo; enddo
       end do
       VarValue = TeFraction*maxval_grid(Tmp1_GB, iLoc_I=iLoc_I)*No2Io_V(&
            UnitTemperature_)
       ! if(index(StringTest,'show_temax')>0.and.iLoc_I(5)==iProc)&
          !  write(*,*)'TeMax, x, y, z=', VarValue, &
          !  Xyz_DGB(:,iLoc_I(1),iLoc_I(2),iLoc_I(3),iLoc_I(4))
       VarValue = VarValue/nProc
    case default
       VarValue = -7777.
       call write_myname;
       write(*,*) 'Warning in set_user_logvar: unknown logvarname = ',TypeVar
    end select

    call test_stop(NameSub, DoTest)
  end subroutine user_get_log_var
  !============================================================================
  subroutine user_set_plot_var(iBlock, NameVar, IsDimensional, &
       PlotVar_G, PlotVarBody, UsePlotVarBody, &
       NameTecVar, NameTecUnit, NameIdlUnit, IsFound)

    use ModAdvance, ONLY: State_VGB, UseElectronPressure, &
         UseAnisoPressure, Source_VC, LeftState_VX, RightState_VX, &
         LeftState_VY, RightState_VY, LeftState_VZ, RightState_VZ
    use ModChromosphere, ONLY: DoExtendTransitionRegion, &
         get_tesi_c, TeSi_C
    use ModCoronalHeating, ONLY: get_block_heating
    use ModTurbulence, ONLY: CoronalHeating_C, WaveDissipationRate_VC, &
         apportion_coronal_heating, get_wave_reflection
    use ModPhysics, ONLY: No2Si_V, Si2No_V, UnitTemperature_, UnitT_, &
         UnitN_, UnitEnergyDens_, CoulombLog, InvGammaMinus1
    use ModRadiativeCooling, ONLY: RadCooling_C, get_radiative_cooling
    use ModVarIndexes, ONLY: nVar, Rho_, p_, Pe_, WaveFirst_, WaveLast_, &
         RhoUx_, RhoUz_, Ehot_
    use ModFaceValue, ONLY: calc_face_value
    use ModB0, ONLY: set_b0_face
    use ModMultiFluid, ONLY: nIonFluid
    use BATL_lib, ONLY: nDim, nG, MaxDim, FaceNormal_DDFB, CellVolume_GB, &
         Xyz_DGB, Used_GB
    use ModHeatConduction, ONLY: get_heat_flux
    use ModConst, ONLY: cBoltzmann, cElectronMass, cProtonMass, cTwoPi, &
         cElectronCharge, cEps
    use ModCellGradient, ONLY: calc_divergence
    use ModHeatFluxCollisionless, ONLY: UseHeatFluxCollisionless, &
         get_gamma_collisionless
#ifdef _OPENACC
    use ModUtilities, ONLY: norm2
#endif
    use ModUtilities, ONLY: i_gang

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

    integer :: i, j, k
    real :: QPerQtotal_I(nIonFluid)
    real :: QparPerQtotal_I(nIonFluid)
    real :: QePerQtotal
    real :: Coef

    integer :: iFace, jFace, kFace, iDir, iMax, jMax, kMax, iLeft, jLeft, kLeft
    real, allocatable :: HeatFlux_DF(:,:,:,:)
    real :: AreaX, AreaY, AreaZ, Area2, Area, Normal_D(MaxDim)
    real :: HeatCondCoefNormal, HeatFlux
    logical :: IsNewBlockHeatCond
    real :: StateLeft_V(nVar), StateRight_V(nVar)

    real :: cTeTiExchangeRate, cTeTiExchangeRateSi, HeatExchange, Te

    real, allocatable :: PeU_DG(:,:,:,:)
    real :: GammaTmp, InvGammaM1

    logical:: DoTest

    character(len=*), parameter:: NameSub = 'user_set_plot_var'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    IsFound = .true.

    select case(NameVar)
    case('te')
       NameIdlUnit = 'K'
       NameTecUnit = '[K]'
       do k = MinK,MaxK; do j = MinJ,MaxJ; do i = MinI,MaxI
          if(UseElectronPressure)then
             PlotVar_G(i,j,k) = TeFraction*State_VGB(Pe_,i,j,k,iBlock) &
                  /State_VGB(Rho_,i,j,k,iBlock)*No2Si_V(UnitTemperature_)
          else
             PlotVar_G(i,j,k) = TeFraction*State_VGB(p_,i,j,k,iBlock) &
                  /State_VGB(Rho_,i,j,k,iBlock)*No2Si_V(UnitTemperature_)
          end if
       end do; end do; end do

    case('ti')
       NameIdlUnit = 'K'
       NameTecUnit = '[K]'
       do k = MinK,MaxK; do j = MinJ,MaxJ; do i = MinI,MaxI
          PlotVar_G(i,j,k) = TiFraction*State_VGB(p_,i,j,k,iBlock) &
               /State_VGB(Rho_,i,j,k,iBlock)*No2Si_V(UnitTemperature_)
       end do; end do; end do

    case('qrad')
       call get_tesi_c(iBlock, TeSi_C)
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          call get_radiative_cooling(i, j, k, iBlock, TeSi_C(i,j,k), &
               RadCooling_C(i,j,k))
          PlotVar_G(i,j,k) = RadCooling_C(i,j,k) &
               *No2Si_V(UnitEnergyDens_)/No2Si_V(UnitT_)
       end do; end do; end do
       NameIdlUnit = 'J/m^3/s'
       NameTecUnit = 'J/m^3/s'

    case('refl')
       Source_VC(WaveFirst_:WaveLast_,:,:,:) = 0.0
       call set_b0_face(iBlock)
       call calc_face_value(iBlock, DoResChangeOnly = .false., &
            DoMonotoneRestrict = .false.)
       call get_wave_reflection(iBlock)
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          PlotVar_G(i,j,k) = Source_VC(WaveLast_,i,j,k) &
               /sqrt(State_VGB(WaveFirst_,i,j,k,iBlock) &
               *     State_VGB(WaveLast_,i,j,k,iBlock))/No2Si_V(UnitT_)
          Source_VC(WaveFirst_:WaveLast_,i,j,k) = 0.0
       end do; end do; end do
       NameIdlUnit = '1/s'
       NameTecUnit = '1/s'

    case('qheat')
       ! some of the heating terms need face values
       call set_b0_face(iBlock)
       call calc_face_value(iBlock, DoResChangeOnly = .false., &
            DoMonotoneRestrict = .false.)
       if(DoExtendTransitionRegion) call get_tesi_c(iBlock, TeSi_C)
       call get_block_heating(iBlock)
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          PlotVar_G(i,j,k) = CoronalHeating_C(i,j,k) &
               *No2Si_V(UnitEnergyDens_)/No2Si_V(UnitT_)
       end do; end do; end do
       NameIdlUnit = 'J/m^3/s'
       NameTecUnit = 'J/m^3/s'

    case('qebyq', 'qparbyq', 'qperpbyq')
       if(UseElectronPressure)then
          call set_b0_face(iBlock)
          call calc_face_value(iBlock, DoResChangeOnly = .false., &
               DoMonotoneRestrict = .false.)
          if(DoExtendTransitionRegion) call get_tesi_c(iBlock, TeSi_C)
          call get_block_heating(iBlock)
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             call apportion_coronal_heating(i, j, k, iBlock, &
                  State_VGB(:,i,j,k,iBlock), &
                  WaveDissipationRate_VC(:,i,j,k), &
                  CoronalHeating_C(i,j,k), TeSi_C(i,j,k), &
                  QPerQtotal_I, QparPerQtotal_I, QePerQtotal)
             select case(NameVar)
             case('qebyq')
                PlotVar_G(i,j,k) = QePerQtotal
             case('qparbyq')
                if(UseAnisoPressure) &
                     PlotVar_G(i,j,k) = QparPerQtotal_I(1)
             case('qperpbyq')
                PlotVar_G(i,j,k) = QPerQtotal_I(1) - QparPerQtotal_I(1)
             end select
          end do; end do; end do
       end if
       NameIdlUnit = '-'
       NameTecUnit = '-'

    case('divq')
       ! some of the heating terms need face values
       IsNewBlockHeatCond = .true.
       call set_b0_face(iBlock)
       call calc_face_value(iBlock, DoResChangeOnly = .false., &
            DoMonotoneRestrict = .false.)
       if (.not.allocated(HeatFlux_DF)) then
          allocate(HeatFlux_DF(MaxDim,nI+1,nJ+1,nK+1))
       endif
       do iDir = 1, nDim
          iMax = nI
          jMax = nJ
          kMax = nK
          select case(iDir)
          case(1)
             iMax = nI + 1
          case(2)
             jMax = nJ + 1
          case(3)
             kMax = nK + 1
          end select

          do kFace = 1, kMax; do jFace = 1, jMax; do iFace = 1, iMax
             select case(iDir)
             case(1)
                StateLeft_V  = LeftState_VX(:,iFace,jFace,kFace)
                StateRight_V = RightState_VX(:,iFace,jFace,kFace)
                iLeft = iFace - 1; jLeft = jFace; kLeft = kFace
             case(2)
                StateLeft_V  = LeftState_VY(:,iFace,jFace,kFace)
                StateRight_V = RightState_VY(:,iFace,jFace,kFace)
                iLeft = iFace; jLeft = jFace - 1; kLeft = kFace
             case(3)
                StateLeft_V  = LeftState_VZ(:,iFace,jFace,kFace)
                StateRight_V = RightState_VZ(:,iFace,jFace,kFace)
                iLeft = iFace; jLeft = jFace; kLeft = kFace - 1
             end select
             AreaX = FaceNormal_DDFB(x_,iDir,iFace,jFace,kFace,iBlock)
             AreaY = FaceNormal_DDFB(y_,iDir,iFace,jFace,kFace,iBlock)
             AreaZ = FaceNormal_DDFB(z_,iDir,iFace,jFace,kFace,iBlock)
             Area2 = AreaX**2 + AreaY**2 + AreaZ**2
             if(Area2 < 1e-30)then
                ! The face is at the pole
                Normal_D = Xyz_DGB(:,iFace,jFace,kFace,iBlock) &
                     -     Xyz_DGB(:,iLeft,jLeft,kLeft,iBlock)
                Normal_D = Normal_D/norm2(Normal_D)
                Area  = 0.0
                Area2 = 0.0
             else
                Area = sqrt(Area2)
                Normal_D = [AreaX, AreaY, AreaZ]/Area
             end if

             call get_heat_flux(iDir, iFace, jFace, kFace, iBlock, &
                  StateLeft_V, StateRight_V, Normal_D, &
                  HeatCondCoefNormal, HeatFlux, IsNewBlockHeatCond)
             HeatFlux_DF(iDir,iFace,jFace,kFace) = HeatFlux*Area
          enddo; enddo; enddo
       enddo

       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          PlotVar_G(i,j,k) = &
               HeatFlux_DF(1,i+1,j,k) - HeatFlux_DF(1,i,j,k) + &
               HeatFlux_DF(2,i,j+1,k) - HeatFlux_DF(2,i,j,k) + &
               HeatFlux_DF(3,i,j,k+1) - HeatFlux_DF(3,i,j,k)
          PlotVar_G(i,j,k) = - PlotVar_G(i,j,k) / CellVolume_GB(i,j,k,iBlock) &
               * No2Si_V(UnitEnergyDens_) / No2Si_V(UnitT_)
       enddo; enddo; enddo

       NameIdlUnit = 'J/m^3/s'
       NameTecUnit = 'J/m^3/s'

    case('teiexch')
       ! Coulomb collisional energy exchange between electrons and ions
       cTeTiExchangeRateSi = &
            CoulombLog/sqrt(cElectronMass) * &
            (cElectronCharge**2/cEps)**2 & ! effective ei collision frequency
            / (3*(cTwoPi*cBoltzmann)**1.50) &
            * (2*cElectronMass/cProtonMass) ! energy exchange per ei collision
       cTeTiExchangeRate = cTeTiExchangeRateSi * &
            (1/Si2No_V(UnitT_))*No2Si_V(UnitN_)/No2Si_V(UnitTemperature_)**1.5
       ! see ModHeatConduction for more explanations of the coefficients

       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE
          if (Ehot_>1 .and. UseHeatFluxCollisionless) then
             call get_gamma_collisionless(Xyz_DGB(:,i,j,k,iBlock), GammaTmp)
             InvGammaM1 = 1./(GammaTmp-1)
          else
             InvGammaM1 = InvGammaMinus1
          end if
          Te = TeFraction * &
               State_VGB(Pe_,i,j,k,iBlock) / State_VGB(Rho_,i,j,k,iBlock)
          ! We apply the energy exchange rate for temperature,
          ! Ni*cTeTiExchangeRate/Te_G(i,j,k)**1.5
          ! For a hydrogen only, for ideal EOS only
          HeatExchange = cTeTiExchangeRate*State_VGB(Rho_,i,j,k,iBlock)/Te**1.5
          PlotVar_G(i,j,k) = InvGammaM1 * HeatExchange * &
               (State_VGB(p_,i,j,k,iBlock) - State_VGB(Pe_,i,j,k,iBlock)) &
               * No2Si_V(UnitEnergyDens_) / No2Si_V(UnitT_)
       end do; end do; end do

       NameIdlUnit = 'J/m^3/s'
       NameTecUnit = 'J/m^3/s'

    case('advpe')
       ! Calculate div(u*Pe/(gamma-1))
       allocate(PeU_DG(3,MinI:MaxI,MinJ:MaxJ,MinK:MaxK))
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          if (Ehot_>1 .and. UseHeatFluxCollisionless) then
             call get_gamma_collisionless(Xyz_DGB(:,i,j,k,iBlock), GammaTmp)
             InvGammaM1 = 1./(GammaTmp-1)
          else
             InvGammaM1 = InvGammaMinus1
          end if
          PeU_DG(:,i,j,k) = &
               InvGammaM1 * State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) / &
               State_VGB(Rho_,i,j,k,iBlock) * State_VGB(Pe_,i,j,k,iBlock)
       end do; end do; end do
       call calc_divergence(iBlock, PeU_DG, nG, PlotVar_G, &
            UseBodyCellIn=.true.)
       deallocate(PeU_DG)

       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          PlotVar_G(i,j,k) = PlotVar_G(i,j,k) * &
               No2Si_V(UnitEnergyDens_) / No2Si_V(UnitT_)
       end do; end do; end do

       NameIdlUnit = 'J/m^3/s'
       NameTecUnit = 'J/m^3/s'

    case default
       IsFound = .false.
    end select

    UsePlotVarBody = .false.
    PlotVarBody    = 0.0

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_plot_var
  !============================================================================
  subroutine user_set_cell_boundary(iBlock, iSide, TypeBc, IsFound)
    !$acc routine vector

    ! Fill ghost cells inside body for spherical grid - this subroutine only
    ! modifies ghost cells in the r direction
    use EEE_ModCommonVariables, ONLY: UseCme
    use EEE_ModMain, ONLY: EEE_get_state_bc
    use ModAdvance, ONLY: State_VGB, UseElectronPressure, UseAnisoPressure
    use ModGeometry, ONLY: r_GB
    use ModHeatFluxCollisionless, ONLY: UseHeatFluxCollisionless, &
         get_gamma_collisionless
    use ModVarIndexes, ONLY: Rho_, p_, Pe_, Bx_, Bz_, Ehot_, &
         RhoUx_, RhoUz_, Ppar_, WaveFirst_, WaveLast_, nFluid
    use ModMultiFluid, ONLY: nIonFluid, MassIon_I, ChargeIon_I, iRho_I, &
         MassFluid_I, iRhoUx_I, iRhoUz_I, iPIon_I, iP_I, iPparIon_I, IsIon_I
    use ModImplicit, ONLY: StateSemi_VGB, iTeImpl
    use ModPhysics, ONLY: AverageIonCharge, UnitRho_, UnitB_, UnitP_, &
         Si2No_V, rBody, GBody, UnitU_, InvGammaMinus1
    use ModMain, ONLY: nStep, nIteration, tSimulation, IsTimeAccurate
    use ModB0, ONLY: B0_DGB
    use BATL_lib, ONLY: CellSize_DB, Phi_, Theta_, x_, y_, Xyz_DGB
    use ModGeometry, ONLY: TypeGeometry
    use ModCoordTransform, ONLY: rot_xyz_sph
    use ModNumConst, ONLY: cPi
    use ModIO, ONLY: IsRestart

    integer,          intent(in)  :: iBlock, iSide
    character(len=*), intent(in)  :: TypeBc
    logical,          intent(out) :: IsFound

    integer :: i, j, k
    real    :: Br1_D(3), Bt1_D(3)
    real    :: Runit_D(3)
    real    :: RhoCme, Ucme_D(3), Bcme_D(3), pCme, BrCme, BrCme_D(3)

    real    :: Br_II(MinJ:MaxJ,MinK:MaxK)
    real    :: Uphi, Ulat
    real    :: r, r1, r2Inv

    real    :: NumDensIon, NumDensElectron

    real    :: Framp = 1.0

    real    :: Dlat, Dphi, dBrDphi, dBrDlat, Br, Rho, p, Ur, rCosLat, uCoeff
    real    :: XyzSph_DD(3,3), u_D(3), Xyz1_D(3), bFace_D(3), b_D(3)

    real    :: Scale, H

    real    :: Usound, Uescape, rTransonic, Ur0, Ur1, dUr
    real    :: uCorona

    real    :: Small, Ur2

    ! Variables used in the 'heliofloat' boundary condition
    real,parameter     :: UEscapeSi = 4.0e5 ! 400 km/c
    real,dimension(3)  :: DirR_D, DirTheta_D, DirPhi_D, Coord_D, &
         UTrue_D, UGhost_D, BTrue_D, BGhost_D
    real               :: CosTheta, SinTheta, CosPhi, SinPhi, rInv
    real               :: BDotU, BPhi, UTheta

    real, parameter :: Epsilon = 1.0e-6
    integer :: nIterParker

    ! Variables related to UseAwsom
    integer :: iMajor, iMinor
    integer :: iFluid, iRho, iRhoUx, iRhoUz, iP
    real    :: FullB_D(3), SignBr
    real    :: U, Bdir_D(3)
    real    :: Gamma

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_cell_boundary'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
#ifndef _OPENACC
    if(iSide /= 1 .or. TypeGeometry(1:9) /='spherical') then
       write(*,*) NameSub, ': iSide, TypeGeometry=', iSide, TypeGeometry
       call CON_stop(NameSub)
    end if
#endif

    IsFound = .true.

    if(UseAwsom)then

#ifndef _OPENACC
       select case(TypeBc)
       case('usersemi','user_semi')
          IsFound = .true.
          StateSemi_VGB(iTeImpl,0,:,:,iBlock) = Tchromo
          RETURN
       case('usersemilinear','user_semilinear')
          IsFound = .true.
          ! Value was already set to zero in ModCellBoundary
          RETURN
          ! jet BC
       case('user')
          IsFound = .true.
       case default
          IsFound = .false.
          RETURN
       end select
#endif

       !$acc loop vector collapse(2) independent &
       !$acc private(rUnit_D, Br1_D, Bt1_D, FullB_D)
       do k = MinK, MaxK; do j = MinJ, MaxJ

          Runit_D = Xyz_DGB(:,1,j,k,iBlock) / r_GB(1,j,k,iBlock)

          Br1_D = sum(State_VGB(Bx_:Bz_,1,j,k,iBlock)*Runit_D)*Runit_D
          Bt1_D = State_VGB(Bx_:Bz_,1,j,k,iBlock) - Br1_D

          ! Set B1r=0, and B1theta = B1theta(1) and B1phi = B1phi(1)
          do i = MinI, 0
             State_VGB(Bx_:Bz_,i,j,k,iBlock) = Bt1_D
          end do

          do iFluid = 1, nFluid
             iRho = iRho_I(iFluid)

             do i = MinI, 0
                ! exponential scaleheight
                State_VGB(iRho,i,j,k,iBlock) = &
                     ChromoN*MassFluid_I(iFluid)*exp(-GBody/rBody &
                     *MassFluid_I(iFluid)/Tchromo &
                     *(rBody/r_GB(i,j,k,iBlock) - 1.0))

                ! Fix ion temperature T_s
                State_VGB(iP_I(iFluid),i,j,k,iBlock) = Tchromo &
                     *State_VGB(iRho,i,j,k,iBlock)/MassFluid_I(iFluid)

                if(UseAnisoPressure .and. IsIon_I(iFluid)) &
                     State_VGB(iPparIon_I(iFluid),i,j,k,iBlock) = &
                     State_VGB(iP_I(iFluid),i,j,k,iBlock)
             end do
          end do

          FullB_D = State_VGB(Bx_:Bz_,1,j,k,iBlock) + B0_DGB(:,1,j,k,iBlock)
          SignBr = sign(1.0, sum(Xyz_DGB(:,1,j,k,iBlock)*FullB_D))
          if(SignBr < 0.0)then
             iMajor = WaveLast_
             iMinor = WaveFirst_
          else
             iMajor = WaveFirst_
             iMinor = WaveLast_
          end if

          do i = MinI, 0
             ! Te = T_s and ne = sum(q_s n_s)
             if(UseElectronPressure) State_VGB(Pe_,i,j,k,iBlock) = &
                  sum(ChargeIon_I*State_VGB(iPIon_I,i,j,k,iBlock))
             ! Outgoing wave energy
             if(IsOnAwRepresentative)then
                State_VGB(iMajor,i,j,k,iBlock) = 1
             else
                State_VGB(iMajor,i,j,k,iBlock) = PoyntingFluxPerB &
                     *sqrt(State_VGB(iRho,i,j,k,iBlock))
             end if
             ! Ingoing wave energy
             State_VGB(iMinor,i,j,k,iBlock) = 0.0
          end do

          ! At the inner boundary this seems to be unnecessary...
          ! Ehot=0 may be sufficient?!
          if(Ehot_ > 1)then
             if(UseHeatFluxCollisionless)then
                iP = p_; if(UseElectronPressure) iP = Pe_
                do i = MinI, 0
                   call get_gamma_collisionless(Xyz_DGB(:,i,j,k,iBlock), Gamma)
                   State_VGB(Ehot_,i,j,k,iBlock) = &
                        State_VGB(iP,i,j,k,iBlock)*(1.0/(Gamma-1) &
                        - InvGammaMinus1)
                end do
             else
                State_VGB(Ehot_,MinI:0,j,k,iBlock) = 0.0
             end if
          end if
       end do; end do

       do iFluid = 1, nIonFluid
          iRho = iRho_I(iFluid)
          iRhoUx = iRhoUx_I(iFluid); iRhoUz = iRhoUz_I(iFluid)

          if(UseFloatRadialVelocity)then
#ifndef _OPENACC
             do k = MinK, MaxK; do j = MinJ, MaxJ
                ! Copy radial velocity component. Reflect the other components
                U = sum(State_VGB(iRhoUx:iRhoUz,1,j,k,iBlock)*Runit_D) &
                     /State_VGB(iRho,1,j,k,iBlock)
                do i = MinI, 0
                   U_D = State_VGB(iRhoUx:iRhoUz,1-i,j,k,iBlock) &
			/State_VGB(iRho,1-i,j,k,iBlock)
                   if(U < 0.0)then
                      State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock) = &
                           - U_D*State_VGB(iRho,i,j,k,iBlock)
                   else
                      U_D = U_D - sum(U_D*Runit_D)*Runit_D
                      State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock) = &
                           (U*Runit_D - U_D)*State_VGB(iRho,i,j,k,iBlock)
                   end if
                end do
             end do; end do
#endif
          else
             !$acc loop vector collapse(2) independent &
             !$acc private(FullB_D, Bdir_D, U_D)
             do k = MinK, MaxK; do j = MinJ, MaxJ
                ! Note that the Bdir_D calculation does not include the
                ! CME part below
                FullB_D = State_VGB(Bx_:Bz_,1,j,k,iBlock) &
                     + 0.5*(B0_DGB(:,0,j,k,iBlock) + B0_DGB(:,1,j,k,iBlock))
                Bdir_D = FullB_D/max(1e-15, norm2(FullB_D))

                ! Copy field-aligned velocity component.
                ! Reflect the other components
                do i = MinI, 0
#ifndef _OPENACC
                   U_D = State_VGB(iRhoUx:iRhoUz,1-i,j,k,iBlock) &
                        /State_VGB(iRho,1-i,j,k,iBlock)
                   U   = sum(U_D*Bdir_D); U_D = U_D - U*Bdir_D
                   State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock) = &
                        (U*Bdir_D - U_D)*State_VGB(iRho,i,j,k,iBlock)
#else
                   ! Somehow, using constants (RhoUx_...) as indices is much
                   ! faster than using regular variables (iRhoUx...) on GPU.
                   U_D = State_VGB(RhoUx_:RhoUz_,1-i,j,k,iBlock) &
                        /State_VGB(Rho_,1-i,j,k,iBlock)
                   U   = sum(U_D*Bdir_D); U_D = U_D - U*Bdir_D
                   State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) = &
                        (U*Bdir_D - U_D)*State_VGB(Rho_,i,j,k,iBlock)
#endif
                end do

             end do; end do
          end if
       end do

       ! start of CME part
       if(UseCme)then
          !$acc loop vector collapse(2) independent &
          !$acc private(Runit_D, Ucme_D, Bcme_D, BrCme_D)
          do k = MinK, MaxK; do j = MinJ, MaxJ
             Runit_D = Xyz_DGB(:,1,j,k,iBlock) / r_GB(1,j,k,iBlock)

             call EEE_get_state_bc(Runit_D, RhoCme, Ucme_D, Bcme_D, pCme, &
                  tSimulation, nStep, nIteration)

             RhoCme = RhoCme*Si2No_V(UnitRho_)
             Bcme_D = Bcme_D*Si2No_V(UnitB_)
             pCme   = pCme*Si2No_V(UnitP_)

             BrCme   = sum(Runit_D*Bcme_D)
             BrCme_D = BrCme*Runit_D
             do i = MinI, 0
                State_VGB(Rho_,i,j,k,iBlock) = State_VGB(Rho_,i,j,k,iBlock) &
                     + RhoCme
                if(UseElectronPressure)then
                   State_VGB(Pe_,i,j,k,iBlock) = State_VGB(Pe_,i,j,k,iBlock) &
                        + 0.5*pCme
                   State_VGB(p_,i,j,k,iBlock) = State_VGB(p_,i,j,k,iBlock) &
                        + 0.5*pCme

                   if(UseAnisoPressure) State_VGB(Ppar_,i,j,k,iBlock) = &
                        State_VGB(Ppar_,i,j,k,iBlock) + 0.5*pCme
                else
                   State_VGB(p_,i,j,k,iBlock) = State_VGB(p_,i,j,k,iBlock) &
                        + pCme
                end if

                ! Only the radial component is set. The tangential components
                ! float. As the flux rope leaves, the solution becomes
                ! more and more potential.
                State_VGB(Bx_:Bz_,i,j,k,iBlock) = &
                     State_VGB(Bx_:Bz_,i,j,k,iBlock) + BrCme_D

                ! If DoBqField = T, we need to modify the velocity components
                ! here
                ! Currently, with the #CME command, we have always DoBqField=F
             end do
          end do; end do
       end if
       ! End of CME part

       ! end of UseAwsom part (AWSoM)
    else
#ifndef _OPENACC
       ! start of .not.UseAwsom part (AWSoM-R)

       if(TypeBc == 'usersurfacerot')then

          if(DoTest) write(*,*) NameSub,' starting with DoUpdateParkerJet=',&
               DoUpdateParkerJet

          ! f(t) = 0                                    if      t < t1
          !      = 1                                    if t2 < t
          !      = 0.5 * ( 1- cos(Pi * (t-t1)/(t2-t1))) if t1 < t < t2

          ! Check if time accurate is set.
          if(IsTimeAccurate)then
             if(tSimulation<tBeginJet)then
                Framp = 0.0
             elseif(tSimulation>tEndJet)then
                Framp = 1.0
             else
                Framp = 0.5 * (1 - cos(cPi*(tSimulation-tBeginJet) / &
                     (tEndJet-tBeginJet)))
             endif
          else
             call stop_mpi(NameSub// &
                  'Use time accurate mode with usersurfacerot.')
          endif

          Dphi = CellSize_DB(Phi_,iBlock)
          Dlat = CellSize_DB(Theta_,iBlock)

          !
          ! B_r = X*B/r
          !
          do k = MinK, MaxK; do j = MinJ, MaxJ

             r1 = r_GB(1,j,k,iBlock)
             r2Inv = r1**(-2)
             bFace_D = 0.5*(B0_DGB(:,0,j,k,iBlock) &
                  +         B0_DGB(:,1,j,k,iBlock))

             Br_II(j,k) = sum(Xyz_DGB(:,1,j,k,iBlock)*bFace_D)/r1
          end do; end do

          do k = 0, MaxK-1; do j = 0, MaxJ-1

             ! Latitude = arccos(z/r)
             ! Longitude = arctg(y/x)
             !
             r1 = r_GB(1,j,k,iBlock)
             Xyz1_D = Xyz_DGB(:,1,j,k,iBlock)
             rCosLat = sqrt(Xyz1_D(x_)**2 + Xyz1_D(y_)**2)
             ! GradBr = (0 ,
             !           1/(r*cos(Theta)) * dB/dPhi ,
             !           1/r              * dB/dTheta )
             !
             dBrDphi = (Br_II(j+1,k) -  Br_II(j-1,k)) / (2*Dphi*rCosLat)
             dBrDlat = (Br_II(j,k+1) -  Br_II(j,k-1)) / (2*Dlat*r1)

             Br = Br_II(j,k)

             Ur = sum(Xyz1_D*State_VGB(RhoUx_:RhoUz_,1,j,k,iBlock)) / &
                  (r1*State_VGB(Rho_,1,j,k,iBlock))

             ! u_perp = 0  if Br < B1 or Br > B2
             !       = v0 * f(t) * kb * (B2-B1)/Br * tanh(kb* (Br-B1)/
             !         (B2-B1)) * (r x GradB)
             !       = ( 0 , u_Phi, u_Theta)
             !
             if(Br < BminJet .or. Br > BmaxJet)then
                Uphi = 0
                Ulat = 0
             else
                ! Rotation initiation

                if(DoRampUpJet)then
                   Framp = 1.0
                endif

                uCoeff = FlowSpeedJet * Framp * CoefBJet * (BmaxJet-BminJet)/ &
                     Br * tanh( CoefBJet * (Br - BminJet)/(BmaxJet-BminJet) )

                ! r x GradB = (1, 0, 0) x (0, GradBPhi, GradBlat) =
                !           = (0,-GradBLat,GradBPhi )
                Uphi = -dBrDlat*uCoeff
                Ulat =  dBrDphi*uCoeff
             endif

             do i = MinI, iBcMax

                r = r_GB(i,j,k,iBlock)
                ! Convert to Cartesian components
                XyzSph_DD = rot_xyz_sph(Xyz1_D)

                if(IsUr0Jet)then
                   u_D = matmul(XyzSph_DD, [max(Ur,0.0), -Ulat, Uphi] )
                else
                   u_D = matmul(XyzSph_DD, [Ur, -Ulat, Uphi] )
                endif

                ! Set velocity in first physical cell (i=1)
                if(i > 0)then
                   State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) = &
                        State_VGB(Rho_,i,j,k,iBlock) * u_D
                   CYCLE
                end if

                ! Set density and pressure in ghost cells once
                if(nIteration < 2)then
                   ! update with Parker solution
                   if(DoUpdateParkerJet)then

                      ! normalize with isothermal sound speed.
                      Usound  = sqrt(tCorona*(1.0 + AverageIonCharge) &
                           /MassIon_I(1))
                      Uescape = sqrt(-GBody*2.0)/ Usound

                      rTransonic = 0.25*Uescape**2
                      uCorona = rTransonic**2*exp(1.5 - 2.0*rTransonic)

                      Ur0 = 1.0
                      nIterParker = 0
                      do
                         nIterParker = nIterParker + 1
                         Ur1 = (Uescape**2/(4.0*r))**2 &
                              *exp(0.5*(Ur0**2 + 3.0 - Uescape**2/r))
                         dUr = abs(Ur1 - Ur0)
                         if(dUr < Epsilon)then
                            Ur2 = Ur1
                            EXIT
                         elseif(nIterParker < 1000)then
                            Ur0 = Ur1
                            CYCLE
                         else
                            call CON_stop('PARKER > 1000 it.')
                         end if
                      end do

                      Rho = rBody**2*RhoCorona*uCorona/(r**2*Ur2)

                      NumDensIon = Rho/MassIon_I(1)
                      NumDensElectron = NumDensIon*AverageIonCharge

                      p = (NumDensIon + NumDensElectron)*tCorona

                      Small = 1e-5

                   else
                      ! update with scaleheight solution
                      H = abs(tCorona * (1 + AverageIonCharge) / Gbody  )
                      Scale = exp(-(r-1) / H)
                      Rho  = RhoCorona*Scale
                      p = tCorona*Rho/MassIon_I(1) *(1 + AverageIonCharge)

                      Small = 1e-1
                   end if

                   ! Check for differences relative to the initial solution.
                   if(.not.IsRestart .and. &
                        ( p/State_VGB(p_,i,j,k,iBlock) > (1 + Small) &
                        .or. p/State_VGB(p_,i,j,k,iBlock) < (1 - Small) &
                        .or. Rho/State_VGB(Rho_,i,j,k,iBlock) > (1 + Small) &
                        .or. Rho/State_VGB(Rho_,i,j,k,iBlock) < (1 - Small)))&
                        then
                      write(*,*)'nStep=',nStep
                      write(*,*)'i,j,k,iBlock=',i,j,k,iBlock
                      write(*,*)'x,y,z=',Xyz1_D
                      write(*,*)'Rho,ParkerRho=',&
                           Rho,State_VGB(Rho_,i,j,k,iBlock),&
                           Rho/State_VGB(Rho_,i,j,k,iBlock)
                      write(*,*)'P,ParkerP=',&
                           p,State_VGB(p_,i,j,k,iBlock),&
                           p/State_VGB(p_,i,j,k,iBlock)
                      call stop_mpi(NameSub// &
                           ' BC too far from Parker solution')
                   endif
                   State_VGB(Rho_,i,j,k,iBlock)  = Rho
                   State_VGB(p_,i,j,k,iBlock)    = p
                end if

                ! Set velocity in ghost cells
                State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) = &
                     State_VGB(Rho_,i,j,k,iBlock)*u_D

                ! Set magnetic field: azimuthal component floats
                b_D = BrampJet**(1-i)*State_VGB(Bx_:Bz_,1,j,k,iBlock)
                State_VGB(Bx_:Bz_,i,j,k,iBlock) = b_D - &
                     sum(b_D*Xyz1_D)*Xyz1_D*r2Inv
             end do

          end do; end do

          if(DoUpdateParkerJet .and. iProc==0 .and. nStep <1)then
             write(*,*)'Update with Parkers solutions'
          endif
          if(.not.DoUpdateParkerJet .and. iProc==0 .and. nStep < 1)then
             write(*,*)'Update with scaleheight calculation solution'
          endif

          RETURN
       endif ! TypeBc == 'usersourcerot'

       ! The routine is only used by the solver for semi-implicit heat
       ! conduction along the magnetic field. To calculate the heat
       ! conduction coefficicients, which are averaged over the true cell
       ! and ghost cell, we need to know the magnetic field and temperature
       ! in the ghost cell. To calculate the fux, we also need to set the
       ! ghost cell temperature within the implicit solver.
       if(TypeBc == 'usersemi')then
          StateSemi_VGB(iTeImpl,0,:,:,iBlock) = tChromo
          RETURN
       elseif(TypeBc == 'usersemilinear')then
          RETURN
       end if

       ! The electron heat conduction requires the electron temperature
       ! in the ghost cells
       NumDensIon = RhoChromo/MassIon_I(1)
       NumDensElectron = NumDensIon*AverageIonCharge
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, 0
          State_VGB(Rho_,i,j,k,iBlock) = RhoChromo
          if(UseElectronPressure)then
             State_VGB(Pe_,i,j,k,iBlock) = NumDensElectron*tChromo
          else
             State_VGB(p_,i,j,k,iBlock) = &
                  (NumDensIon + NumDensElectron)*tChromo
          end if
       end do; end do; end do

       ! The following is only needed for the semi-implicit heat conduction,
       ! which averages the cell centered heat conduction coefficient towards
       ! the face
       do k = MinK,MaxK; do j = MinJ,MaxJ
          Runit_D = Xyz_DGB(:,1,j,k,iBlock) / r_GB(1,j,k,iBlock)

          Br1_D = sum(State_VGB(Bx_:Bz_,1,j,k,iBlock)*Runit_D)*Runit_D
          Bt1_D = State_VGB(Bx_:Bz_,1,j,k,iBlock) - Br1_D

          do i = MinI, 0
             State_VGB(Bx_:Bz_,i,j,k,iBlock) = Bt1_D
          end do

       end do; end do

       if(UseCme)then
          do k = MinK, MaxK; do j = MinJ, MaxJ
             Runit_D = Xyz_DGB(:,1,j,k,iBlock) / r_GB(1,j,k,iBlock)

             call EEE_get_state_bc(Runit_D, RhoCme, Ucme_D, Bcme_D, pCme, &
                  tSimulation, nStep, nIteration)

             RhoCme = RhoCme*Si2No_V(UnitRho_)
             Bcme_D = Bcme_D*Si2No_V(UnitB_)
             pCme   = pCme*Si2No_V(UnitP_)

             BrCme   = sum(Runit_D*Bcme_D)
             BrCme_D = BrCme*Runit_D

             do i = MinI, 0
                State_VGB(Rho_,i,j,k,iBlock) = State_VGB(Rho_,i,j,k,iBlock) &
                     + RhoCme
                if(UseElectronPressure)then
                   State_VGB(Pe_,i,j,k,iBlock) = State_VGB(Pe_,i,j,k,iBlock) &
                        + 0.5*pCme
                else
                   State_VGB(p_,i,j,k,iBlock) = State_VGB(p_,i,j,k,iBlock) &
                        + pCme
                end if
                State_VGB(Bx_:Bz_,i,j,k,iBlock) = &
                     State_VGB(Bx_:Bz_,i,j,k,iBlock) + BrCme_D
             end do
          end do; end do
       end if
#endif
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_cell_boundary
  !============================================================================
  subroutine user_initial_perturbation

    use EEE_ModCommonVariables, ONLY: XyzCmeCenterSi_D, XyzCmeApexSi_D, &
         bAmbientCenterSi_D, bAmbientApexSi_D
    use EEE_ModMain, ONLY: EEE_get_state_init, EEE_do_not_add_cme_again, &
         EEE_get_state_init, EEE_init_cme_parameters
    use ModB0, ONLY: get_b0
    use ModMain, ONLY: nStep, nIteration, UseFieldLineThreads
    use ModVarIndexes
    use ModAdvance, ONLY: State_VGB, UseElectronPressure, UseAnisoPressure
    use ModPhysics, ONLY: Si2No_V, UnitRho_, UnitP_, UnitB_, UnitX_, No2Si_V
    use BATL_lib, ONLY: nI, nJ, nK, nBlock, Unused_B, nDim, MaxDim, Xyz_DGB, &
         iComm, CellVolume_GB, message_pass_cell, interpolate_state_vector
    use ModMpi
    integer :: i, j, k, iBlock, iError
    real :: x_D(MaxDim), Rho, B_D(MaxDim), B0_D(MaxDim), p
    real :: Mass, MassDim, MassTotal

    logical:: DoTest, IsFound
    character(len=*), parameter:: NameSub = 'user_initial_perturbation'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(UseAwsom)then
       call EEE_init_cme_parameters
       do iBlock = 1, nBlock
          if(Unused_B(iBlock))CYCLE

          do k = 1, nK; do j = 1, nJ; do i = 1, nI

             x_D = Xyz_DGB(:,i,j,k,iBlock)

             call EEE_get_state_init(x_D, Rho, B_D, p, nStep, nIteration)

             Rho = Rho*Si2No_V(UnitRho_)
             B_D = B_D*Si2No_V(UnitB_)
             p = p*Si2No_V(UnitP_)

             ! Add the eruptive event state to the solar wind
             ! Convert momentum density to velocity
             State_VGB(Ux_:Uz_,i,j,k,iBlock) = &
                  State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)/&
                  State_VGB(Rho_,i,j,k,iBlock)

             State_VGB(Rho_,i,j,k,iBlock) = &
                  max(0.25*State_VGB(Rho_,i,j,k,iBlock), &
                  State_VGB(Rho_,i,j,k,iBlock) + Rho)

             ! Fix momentum density to correspond to the modified mass density
             State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) = &
                  State_VGB(Ux_:Uz_,i,j,k,iBlock)*State_VGB(Rho_,i,j,k,iBlock)

             State_VGB(Bx_:Bz_,i,j,k,iBlock) = &
                  State_VGB(Bx_:Bz_,i,j,k,iBlock) + B_D

             if(UseElectronPressure)then
                State_VGB(Pe_,i,j,k,iBlock) = &
                     max(0.25*State_VGB(Pe_,i,j,k,iBlock), &
                     State_VGB(Pe_,i,j,k,iBlock) + 0.5*p)
                State_VGB(p_,i,j,k,iBlock) = &
                     max(0.25*State_VGB(p_,i,j,k,iBlock), &
                     State_VGB(p_,i,j,k,iBlock) + 0.5*p)
                if(UseAnisoPressure) State_VGB(Ppar_,i,j,k,iBlock) = &
                     max(0.25*State_VGB(Ppar_,i,j,k,iBlock), &
                     State_VGB(Ppar_,i,j,k,iBlock) + 0.5*p)
             else
                State_VGB(p_,i,j,k,iBlock) = &
                     max(0.25*State_VGB(p_,i,j,k,iBlock), &
                     State_VGB(p_,i,j,k,iBlock) + p)
             endif

          end do; end do; end do
       end do
       ! End of UseAwsom
    else
       Mass = 0.0
       ! Bstrap_D should be B0_D+B1_D instead of B0_D
       ! if(DoAddTD14) call get_b0(XyzApex_D, Bstrap_D)
       !
       ! Since the ghost cells may not be filled in, call message_pass first
       if(UseFieldLineThreads) call message_pass_cell( &
            3, State_VGB(Bx_:Bz_,:,:,:,:), nProlongOrderIn=1)
       x_D = XyzCmeCenterSi_D*Si2No_V(UnitX_)
       call interpolate_state_vector( &
            x_D, 3, State_VGB(Bx_:Bz_,:,:,:,:), B_D, IsFound)
       if(IsFound)then
          call get_b0(x_D, B0_D)
          bAmbientCenterSi_D = (B0_D + B_D)*No2Si_V(UnitB_)
          if(iProc==0)then
             write(*,'(a,3es12.4)')'EEE: At the CME center at Xyz=', x_D
             write(*,'(a,3es12.4,a)') &
                  'EEE: An ambient magnetic field (prior to CME) is: ',&
                  bAmbientCenterSi_D*1e4,' [Gs]'
          end if
       end if
       x_D = XyzCmeApexSi_D*Si2No_V(UnitX_)
       call interpolate_state_vector(x_D, 3, State_VGB(Bx_:Bz_,:,:,:,:),&
            B_D, IsFound)
       if(IsFound)then
          call get_b0(x_D, B0_D)
          bAmbientApexSi_D = (B0_D + B_D)*No2Si_V(UnitB_)
          if(iProc == 0)then
             write(*,'(a,3es12.4)')'EEE: At the CME apex at Xyz=', x_D
             write(*,'(a,3es12.4,a)')&
                  'EEE: An ambient magnetic field (prior to CME) is: ',&
                  bAmbientApexSi_D*1e4,' [Gs]'
          end if
       end if

       call EEE_init_cme_parameters
       do iBlock = 1, nBlock
          if(Unused_B(iBlock))CYCLE

          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             x_D = Xyz_DGB(:,i,j,k,iBlock)
             call EEE_get_state_init(x_D, Rho, B_D, p, nStep, nIteration)
             Rho = Rho*Si2No_V(UnitRho_)
             B_D = B_D*Si2No_V(UnitB_)
             p = p*Si2No_V(UnitP_)

             ! Add the eruptive event state to the solar wind

             ! Convert momentum density to velocity
             State_VGB(Ux_:Uz_,i,j,k,iBlock) = &
                  State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)/&
                  State_VGB(Rho_,i,j,k,iBlock)
             if(State_VGB(Rho_,i,j,k,iBlock) + Rho &
                  < 0.25*State_VGB(Rho_,i,j,k,iBlock))then
                State_VGB(Rho_,i,j,k,iBlock) = &
                     0.25*State_VGB(Rho_,i,j,k,iBlock)

                ! Calculate the mass added to the eruptive event
                Mass = Mass - 3*CellVolume_GB(i,j,k,iBlock)*&
                     State_VGB(Rho_,i,j,k,iBlock)
             else

                ! Calculate the mass added to the eruptive event
                Mass = Mass + Rho*CellVolume_GB(i,j,k,iBlock)
                State_VGB(Rho_,i,j,k,iBlock) = &
                     State_VGB(Rho_,i,j,k,iBlock) + Rho
             endif

             ! Fix momentum density to correspond to the modified mass density
             State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) = &
                  State_VGB(Ux_:Uz_,i,j,k,iBlock)*&
                  State_VGB(Rho_,i,j,k,iBlock)

             State_VGB(Bx_:Bz_,i,j,k,iBlock) = &
                  State_VGB(Bx_:Bz_,i,j,k,iBlock) + B_D

             if(UseElectronPressure)then
                if(State_VGB(Pe_,i,j,k,iBlock) &
                     + 0.5*p < 0.25*State_VGB(Pe_,i,j,k,iBlock))then
                   State_VGB(Pe_,i,j,k,iBlock) = &
                        0.25*State_VGB(Pe_,i,j,k,iBlock)
                else
                   State_VGB(Pe_,i,j,k,iBlock) = &
                        State_VGB(Pe_,i,j,k,iBlock) + 0.5*p
                endif

                if(State_VGB(p_,i,j,k,iBlock)  + 0.5*p &
                     < 0.25*State_VGB(p_,i,j,k,iBlock))then
                   State_VGB(p_,i,j,k,iBlock) = 0.25*State_VGB(p_,i,j,k,iBlock)
                else
                   State_VGB(p_,i,j,k,iBlock) = &
                        State_VGB(p_,i,j,k,iBlock) + 0.5*p
                endif

             else
                if(State_VGB(p_,i,j,k,iBlock) + p &
                     < 0.25*State_VGB(p_,i,j,k,iBlock))then
                   State_VGB(p_,i,j,k,iBlock) = 0.25*State_VGB(p_,i,j,k,iBlock)
                else
                   State_VGB(p_,i,j,k,iBlock) = State_VGB(p_,i,j,k,iBlock) + p
                endif
             end if
          end do; end do; end do

       end do
       call MPI_reduce(Mass, MassTotal, 1, MPI_REAL, MPI_SUM, 0, iComm, iError)
       if(iProc == 0)then
          MassDim = MassTotal*No2Si_V(UnitRho_)*No2Si_V(UnitX_)**3 & ! in Si
               *1000                                                 ! in g
          write(*,'(a,es13.5,a)') &
               'EEE: CME is initiated, Ejected mass=', MassDim,' g'
       end if
       call EEE_do_not_add_cme_again

    end if

    call test_stop(NameSub, DoTest)
  end subroutine user_initial_perturbation
  !============================================================================
  subroutine user_get_b0(x, y, z, B0_D)

    use ModCoordTransform, ONLY: rlonlat_to_xyz
    use ModGeometry, ONLY: RadiusMin
    use ModMain, ONLY: tSimulation, UseUserB0
    use EEE_ModGetB0, ONLY: EEE_get_B0
    use EEE_ModCommonVariables, ONLY: UseTD
    use ModPhysics, ONLY: Si2No_V, UnitB_
    use BATL_lib, ONLY: IsRzGeometry, z_

    real, intent(in) :: x, y, z
    real, intent(inout):: B0_D(3)

    real :: Xyz_D(3), Dp, rInv, r2Inv, r3Inv, UserDipoleAxis_D(3), B_D(3)

    logical, save :: DoFirst = .true.
    real, save :: XyzCenter_D(3), Dipole_D(3)

    character(len=*), parameter:: NameSub = 'user_get_b0'
    !--------------------------------------------------------------------------
    if(any(UniformB0Si_D/=0.0))then
       B_D = UniformB0Si_D*Si2No_V(UnitB_)
       if(IsRzGeometry.and.B_D(z_)/=0.0)B_D(z_) = B_D(z_)/y
       B0_D = B0_D + B_D
    end if
    if(UseTD)then
       Xyz_D = [x, y, z]
       call EEE_get_B0(Xyz_D, B_D, tSimulation)
       B0_D = B0_D + B_D*Si2No_V(UnitB_)
       RETURN
    end if

    if(UserDipoleStrength == 0.0)then
       if(.not.any(UniformB0Si_D/=0.0))UseUserB0 = .false.
       RETURN
    end if

    if(DoFirst)then
       DoFirst = .false.

       ! Center of dipole shifted by UserDipoleDepth below RadiusMin
       call rlonlat_to_xyz( &
            [RadiusMin-UserDipoleDepth, &
            UserDipoleLongitude, UserDipoleLatitude], XyzCenter_D )

       call rlonlat_to_xyz(&
            [1.,UserDipoleAxisLongitude, UserDipoleAxisLatitude], &
            UserDipoleAxis_D)

       ! Compute dipole moment of the intrinsic magnetic field B0.
       Dipole_D = UserDipoleStrength * UserDipoleAxis_D
    end if

    ! Normalize with depth
    Xyz_D = ([x, y, z] - XyzCenter_D)/UserDipoleDepth

    ! Determine radial distance and powers of it
    rInv  = 1.0/sqrt(sum(Xyz_D**2))
    r2Inv = rInv**2
    r3Inv = rInv*r2Inv

    Dp = 3*sum(Dipole_D*Xyz_D)*r2Inv

    B0_D = B0_D + (Dp*Xyz_D - Dipole_D) * r3Inv

  end subroutine user_get_b0
  !============================================================================
  subroutine user_material_properties(State_V, i, j, k, iBlock, iDir, &
       EinternalIn, TeIn, NatomicOut, AverageIonChargeOut, &
       EinternalOut, TeOut, PressureOut, &
       CvOut, GammaOut, HeatCondOut, IonHeatCondOut, TeTiRelaxOut,         &
       OpacityPlanckOut_W, OpacityEmissionOut_W, OpacityRosselandOut_W,    &
       PlanckOut_W)

    use ModConst, ONLY: cBoltzmann, cPlanckH, cElectronMass, cTwoPi, cPi, &
         cLightSpeed, cElectronCharge, cElectronChargeSquaredJm
    use ModVarIndexes, ONLY: Pe_, Rho_
    use ModPhysics, ONLY: No2Si_V, UnitX_, UnitTemperature_, UnitN_

    ! The State_V vector is in normalized units, all other physical
    ! quantities are in SI.
    !
    ! If the electron energy is used, then EinternalIn, EinternalOut,
    ! PressureOut, CvOut refer to the electron internal energies,
    ! electron pressure, and electron specific heat, respectively.
    ! Otherwise they refer to the total (electron + ion) internal energies,
    ! total (electron + ion) pressure, and the total specific heat.

    use ModMain, ONLY: nI, UseERadInput
    use ModAdvance, ONLY: State_VGB, UseElectronPressure
    use ModPhysics, ONLY: No2Si_V, Si2No_V, UnitRho_, UnitP_, &
         InvGammaMinus1, UnitEnergyDens_, UnitX_
    use ModVarIndexes, ONLY: nVar, Rho_, p_, nWave, &
         WaveFirst_,WaveLast_, &
         Pe_, ExtraEint_
    use ModConst
    use ModWaves, ONLY: FrequencySi_W

    real, intent(in) :: State_V(nVar)
    integer, optional, intent(in):: i, j, k, iBlock, iDir  ! cell/face index
    real, optional, intent(in)  :: EinternalIn             ! [J/m^3]
    real, optional, intent(in)  :: TeIn                    ! [K]
    real, optional, intent(out) :: NatomicOut              ! [1/m^3]
    real, optional, intent(out) :: AverageIonChargeOut     ! dimensionless
    real, optional, intent(out) :: EinternalOut            ! [J/m^3]
    real, optional, intent(out) :: TeOut                   ! [K]
    real, optional, intent(out) :: PressureOut             ! [Pa]
    real, optional, intent(out) :: CvOut                   ! [J/(K*m^3)]
    real, optional, intent(out) :: GammaOut                ! dimensionless
    real, optional, intent(out) :: HeatCondOut             ! [J/(m*K*s)]
    real, optional, intent(out) :: IonHeatCondOut          ! [J/(m*K*s)]
    real, optional, intent(out) :: TeTiRelaxOut            ! [1/s]
    real, optional, intent(out) :: &
         OpacityPlanckOut_W(nWave)                         ! [1/m]
    real, optional, intent(out) :: &
         OpacityEmissionOut_W(nWave)                       ! [1/m]
    real, optional, intent(out) :: &
         OpacityRosselandOut_W(nWave)                      ! [1/m]

    ! Multi-group specific interface. The variables are respectively:
    !  Group Planckian spectral energy density
    real, optional, intent(out) :: PlanckOut_W(nWave)      ! [J/m^3]
    real :: FrequencySi, ElectronTemperatureSi, ElectronDensitySi
    real :: ElectronDensitySiCr, Dens2DensCr
    real, parameter :: GauntFactor = 10.0
    !--------------------------------------------------------------------------
    ! Assign frequency of radioemission
    FrequencySi = FrequencySi_W(WaveFirst_)
    !
    OpacityEmissionOut_W = 0.0
    PlanckOut_W = 0.0
    ElectronDensitySi = State_V(Rho_)*No2Si_V(UnitN_)
    ElectronTemperatureSi = State_V(Pe_)/State_V(Rho_)*&
         No2Si_V(UnitTemperature_)
    select case( trim(TypeRadioEmission) )
    case('simplistic')

       ! Calculate the critical density from the frequency
       ElectronDensitySiCr = cPi*cElectronMass&
            *FrequencySi**2/cElectronChargeSquaredJm
       Dens2DensCr = ElectronDensitySi/ElectronDensitySiCr
       PlanckOut_W = 1.0  ! Just a proxy
       OpacityEmissionOut_W(1) = (Dens2DensCr**2)*(0.50 - Dens2DensCr)**2
    case('bremsstrahlung')

       ! Bremsstrahlung spectrum for Radio satisfies well the condition:
       ! hv<<k_BT. So the B(v,T), which is indicated here are PlanckOut_W,
       ! can be written after Taylor expansion as follows.
       PlanckOut_W = 2.0*FrequencySi**2*cBoltzmann*&
            ElectronTemperatureSi/cLightSpeed**2 ! [W m^-2 sr^-1 Hz^-1]

       ! We choose absorption coefficient which is [1/m]  to be dimensionless
       ! and thus multiply it by length of segment.
       OpacityEmissionOut_W = 4.0/3.0*sqrt(2.0*cPi/3.0)*&
            (  cElectronChargeSquaredJm/&
            sqrt(cBoltzmann*ElectronTemperatureSi*cElectronMass) )**3*&
            ( ElectronDensitySi/FrequencySi  )**2&
            /cLightSpeed*GauntFactor&  ! [1/m]
            /Si2No_V(UnitX_)           ! [dimensionless]
    case default
       call CON_stop('Unknown radio emission mechanism ='&
            //TypeRadioEmission)
    end select

  end subroutine user_material_properties
  !============================================================================
  subroutine user_calc_sources_expl(iBlock)

    use ModAdvance, ONLY: Source_VC, State_VGB
    use ModB0, ONLY: B0_DGB
    use ModVarIndexes, ONLY: Bx_, Bz_, Energy_
    use ModMain, ONLY: UseB0, tSimulation
    use ModCoordTransform, ONLY: rot_xyz_rlonlat, xyz_to_lonlat
    use BATL_size, ONLY: nK, nJ
    use BATL_lib, ONLY: CellFace_DFB, Xyz_DGB
    use ModGeometry, ONLY: r_GB

    integer, intent(in) :: iBlock

    integer :: i, j, k
    real :: Lon, Lat
    real :: ZetaJLeft, ZetaJRight, ZetaKLeft, ZetaKRight
    real :: XyzSph_DD(3,3), dBSph_D(3), dBXyz_D(3)
    real :: BXyzJLeft_D(3), BXyzJRight_D(3), BXyzKLeft_D(3), BXyzKRight_D(3)
    real :: BrJLeft, BrJRight, BrKLeft, BrKRight
    !--------------------------------------------------------------------------
    ! Check STITCH time
    if(tSimulation < tStartStitch .or. tSimulation > tStopStitch) RETURN

    ! Add STITCH source term in/up to first cell above rStitch.
    ! Works only in spherical coordinates.
    if(r_GB(0,1,1,iBlock) > rStitch &
         .or. DoStitchOneCell .and. r_GB(nI,1,1,iBlock) < rStitch) &
         RETURN

    ZetaJLeft = Zeta
    ZetaJRight = Zeta
    ZetaKLeft = Zeta
    ZetaKRight = Zeta

    do i = 1, nI
       ! Check if we are above stitch radius
       if(r_GB(i-1,1,1,iBlock) > rStitch) EXIT
       ! Check if we are below stitch radius
       if(DoStitchOneCell .and. r_GB(i,1,1,iBlock) < rStitch) CYCLE
       do k = 1, nK; do j = 1, nJ

          XyzSph_DD = rot_xyz_rlonlat(Xyz_DGB(:,i,j,k,iBlock))

          BXyzJLeft_D = State_VGB(Bx_:Bz_,i,j-1,k,iBlock)
          if(UseB0) BXyzJLeft_D = BXyzJLeft_D + B0_DGB(:,i,j-1,k,iBlock)

          BXyzJRight_D = State_VGB(Bx_:Bz_,i,j+1,k,iBlock)
          if(UseB0) BXyzJRight_D = BXyzJRight_D + B0_DGB(:,i,j+1,k,iBlock)

          BXyzKLeft_D = State_VGB(Bx_:Bz_,i,j,k-1,iBlock)
          if(UseB0) BXyzKLeft_D = BXyzKLeft_D + B0_DGB(:,i,j,k-1,iBlock)

          BXyzKRight_D = State_VGB(Bx_:Bz_,i,j,k+1,iBlock)
          if(UseB0) BXyzKRight_D = BXyzKRight_D + B0_DGB(:,i,j,k+1,iBlock)

          BrJLeft  = sum(BXyzJLeft_D*Xyz_DGB(:,i,j-1,k,iBlock)) &
               /r_GB(i,j-1,k,iBlock)
          BrJRight = sum(BXyzJRight_D*Xyz_DGB(:,i,j+1,k,iBlock)) &
               /r_GB(i,j+1,k,iBlock)
          BrKLeft  = sum(BXyzKLeft_D*Xyz_DGB(:,i,j,k-1,iBlock)) &
               /r_GB(i,j,k-1,iBlock)
          BrKRight = sum(BXyzKRight_D*Xyz_DGB(:,i,j,k+1,iBlock)) &
               /r_GB(i,j,k+1,iBlock)

          if(UseStitchRegion)then
             call xyz_to_lonlat(Xyz_DGB(:,i,j-1,k,iBlock), Lon, Lat)
             ZetaJLeft = zeta_region(lon, lat)

             call xyz_to_lonlat(Xyz_DGB(:,i,j+1,k,iBlock), Lon, Lat)
             ZetaJRight = zeta_region(lon, lat)

             call xyz_to_lonlat(Xyz_DGB(:,i,j,k-1,iBlock), Lon, Lat)
             ZetaKLeft = zeta_region(lon, lat)

             call xyz_to_lonlat(Xyz_DGB(:,i,j,k+1,iBlock), Lon, Lat)
             ZetaKRight = zeta_region(lon, lat)
          end if

          dBSph_D(1) = 0
          dBSph_D(2) = -(ZetaKRight*BrKRight - ZetaKLeft*BrKLeft)/ &
               (CellFace_DFB(2,i,j,k,iBlock) + CellFace_DFB(2,i,j+1,k,iBlock))
          dBSph_D(3) =  (ZetaJRight*BrJRight - ZetaJLeft*BrJLeft)/ &
               (CellFace_DFB(3,i,j,k,iBlock) + CellFace_DFB(3,i,j,k+1,iBlock))
          dBXyz_D = matmul(XyzSph_DD, dBSph_D)

          Source_VC(Bx_:Bz_,i,j,k) = Source_VC(Bx_:Bz_,i,j,k) + dBXyz_D
          Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) &
               + sum(State_VGB(Bx_:Bz_,i,j,k,iBlock)*dBXyz_D)
       end do; end do
    end do

  contains
    !==========================================================================
    real function zeta_region(lon, lat)
      use ModNumConst, ONLY: cHalfPi, cTwoPi

      real, intent(in) :: Lon, Lat

      real :: LonLocal, LatLocal
      !------------------------------------------------------------------------
      LatLocal = cHalfPi*(Lat - Lat0)/Lat1
      if(abs(Lon-Lon0)<Lon1 .and. abs(Lat-Lat0)<Lat1)then
         LonLocal = cHalfPi*(Lon - Lon0)/Lon1
         zeta_region = Zeta*cos(LonLocal)*cos(LatLocal)
      elseif(Lon-Lon0>cTwoPi-Lon1 .and. abs(Lat-Lat0)<Lat1)then
         LonLocal = cHalfPi*(Lon - Lon0 - cTwoPi)/Lon1
         zeta_region = Zeta*cos(LonLocal)*cos(LatLocal)
      elseif(Lon-Lon0<-cTwoPi+Lon1 .and. abs(Lat-Lat0)<Lat1)then
         LonLocal = cHalfPi*(Lon - Lon0 + cTwoPi)/Lon1
         zeta_region = Zeta*cos(LonLocal)*cos(LatLocal)
      else
         zeta_region = 0.0
      end if

    end function zeta_region
    !==========================================================================
  end subroutine user_calc_sources_expl
  !============================================================================
end module ModUser
!==============================================================================
