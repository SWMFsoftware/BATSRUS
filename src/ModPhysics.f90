!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModPhysics

  use BATL_lib, ONLY: &
       test_start, test_stop, iProc
  use ModBatsrusUtility, ONLY: stop_mpi

  use ModNumConst, ONLY: cDegToRad
  use ModConst, ONLY: rSun, mSun, RotationPeriodSun, cSecondPerDay, &
       cProtonMass, cElectronCharge, cGyroElectron, cLightSpeed, &
       cRadiation, cGravitation, cAU, cBoltzmann, cMu, cRadToDeg, cEps, &
       cElectronMass
  use ModMain, ONLY: TypeCoordSystem, body2_, SolidBc_, xMinBc_, zMaxBc_, &
       Coord1MinBc_, Coord3MaxBc_ , NameVarLower_V
  use ModVarIndexes, ONLY: nVar, nFluid, SpeciesFirst_, SpeciesLast_
  use ModAdvance, ONLY: UseMultiSpecies, nSpecies, nIonDensity
  use ModMultiFluid, ONLY: nIonFluid
  use CON_star, ONLY: NameStar, RadiusStar, MassStar, RotPeriodStar

  implicit none

  SAVE

  public

  public:: set_physics_constants
  public:: init_mhd_variables
  public:: init_vector_variables
  public:: set_dimensional_factor
  public:: set_dipole
  public:: set_radial_state ! set radial state for initial/boundary conditions

  private:: set_units
  private:: set_unit_conversion_array_indices

  ! default adiabatic index value
  real, parameter:: Gamma0 = 5./3.

  ! adiabatic index (gamma) and derived values for all fluids
  real:: Gamma_I(nFluid)          = Gamma0
  real:: GammaMinus1_I(nFluid)    = Gamma0 - 1.0
  real:: InvGammaMinus1_I(nFluid) = 1.0/(Gamma0 - 1.0)
  !$acc declare create(Gamma_I, GammaMinus1_I, InvGammaMinus1_I)

  ! adiabatic index (gamma) and derived values for ion fluids
  real:: GammaIon_I(nIonFluid)          = Gamma0
  real:: GammaMinus1Ion_I(nIonFluid)    = Gamma0 - 1.0
  real:: InvGammaMinus1Ion_I(nIonFluid) = 1.0/(Gamma0 - 1.0)

  ! adiabatic index (gamma) and derived values for the first/total fluid
  real:: Gamma          = Gamma0
  real:: GammaMinus1    = Gamma0 - 1.0
  real:: InvGammaMinus1 = 1.0/(Gamma0 - 1.0)
  !$acc declare create(Gamma, GammaMinus1, InvGammaMinus1)

  ! adiabatic index (gamma) and derived values for electrons
  real:: GammaElectron          = Gamma0
  real:: GammaElectronMinus1    = Gamma0 - 1.0
  real:: InvGammaElectronMinus1 = 1.0/(Gamma0 - 1.0)
  !$acc declare create(GammaElectron, GammaElectronMinus1)
  !$acc declare create(InvGammaElectronMinus1)

  ! gamma of the waves
  real:: GammaWave = 1.5
  !$acc declare create(GammaWave)

  ! electron charge in normalized units (actually proton charge/mass)
  real:: ElectronCharge

  ! electron gyro-frequency coefficient in normalized units
  real:: ElectronGyroFreqCoef

  ! Coulomb logarithm (spatially independent)
  real:: CoulombLog = 20.0

  ! plasma parameters
  real:: AverageIonCharge         = 1.0
  real:: ElectronTemperatureRatio = 0.0
  real:: ElectronPressureRatio    = 0.0
  real:: PePerPtotal              = 0.0
  real:: IonMassPerCharge         = 1.0
  !$acc declare create(AverageIonCharge, ElectronPressureRatio, PePerPtotal)
  !$acc declare create(IonMassPerCharge)

  ! Quantities for reduced mass and collision frequencies
  real:: MassIonElectron_I(nIonFluid+1)
  real:: ReducedMass_II(nIonFluid+1,nIonFluid+1)
  real:: CollisionCoef_II(nIonFluid+1,nIonFluid+1)
  !$acc declare create(CollisionCoef_II)

  ! Ion charge for multi-species.
  real:: ChargeSpecies_I(SpeciesFirst_:SpeciesLast_) = 1.0

  ! Artificial speed of light set by #LIGHTSPEED
  real:: ClightDim = -1.0

  ! speed of light, inverse, square, inverse of square, reduction factor
  real:: Clight, InvClight, C2light, InvClight2
  real:: ClightFactor = 1.0
  !$acc declare create(C2light, InvClight, InvClight2, ClightFactor)

  ! normalized radiation constant (Erad = cRadiationNo*Trad**4)
  real:: cRadiationNo

  ! the dipole moment for body2
  real:: BdpBody2_D(3)=0.0, BdpDimBody2_D(3)=0.0

  real:: OmegaBody_D(3)
  !$acc declare create(OmegaBody_D)

  ! Monopole and dipole magnetic field
  real:: MonopoleStrength=0.0, MonopoleStrengthSi=0.0
  real:: Bdp=0.0, DipoleStrengthSi=0.0
  real:: Dipole_D(3)
  !$acc declare create(Bdp, DipoleStrengthSi, Dipole_D)

  real:: ThetaTilt=0.0, &                ! tilt angle of magnetic axis
       SinThetaTilt=0.0, CosThetaTilt=1.0 ! NOW ONLY IH SHOULD USE THIS !!!

  !$acc declare create(SinThetaTilt, CosThetaTilt)

  ! Far field solar wind solution variables.
  real:: SolarWindTempDim=0.0, &
       SolarWindRho=0.0, SolarWindRhoDim=0.0, &
       SolarWindN=0.0,   SolarWindNDim=0.0  , &
       SolarWindP=0.0  , SolarWindPDim=0.0  , &
       SolarWindUx=0.0 , SolarWindUxDim=0.0 , &
       SolarWindUy=0.0 , SolarWindUyDim=0.0 , &
       SolarWindUz=0.0 , SolarWindUzDim=0.0 , &
       SolarWindBx=0.0 , SolarWindBxDim=0.0 , &
       SolarWindBy=0.0 , SolarWindByDim=0.0 , &
       SolarWindBz=0.0 , SolarWindBzDim=0.0

  real:: SwTminDim = -1.0

  ! General Body parameters
  character(len=2):: NamePlanetRadius = 'R ' ! can be 'km' if there is no body
  real:: rPlanetSi=0.0, rBody=0.0, rCurrents=0.0
  !$acc declare create(rBody)
  real:: gBody=0.0
  real:: RotPeriodSi=0.0, OmegaBody=0.0
  !$acc declare create(OmegaBody, gBody)

  ! The dimensional quantities are given for individual ion and neutral fluids
  real, dimension(nFluid):: &
       BodyNDim_I = 1.0, BodyTDim_I = 1.0, &
       PolarNDim_I= 1.0, PolarTDim_I= 1.0, PolarUDim_I = 0.0

  ! The normalized quantities
  real, dimension(nFluid):: &
       BodyRho_I = 1.0, BodyP_I = 1.0, &
       PolarRho_I= 1.0, PolarP_I= 1.0, PolarU_I=0.0
  !$acc declare create(PolarRho_I, PolarP_I)

  ! Lower limit on densities obtained from PW model
  real:: PolarRhoMin_I(nIonDensity)

  ! Limit PW passed densities by body values?
  logical:: DoLimitRhoPw = .false.

  ! Number and mass densities for multi-species equations
  real:: BodyNSpeciesDim_I(nSpecies) = -1.0, BodyRhoSpecies_I(nSpecies) = -1.0

  ! Density ratio of major and minor ions/neutrals (e.g. in the solar wind)
  real:: LowDensityRatio = 0.0001

  ! Minimum threshold for electron pressure and extra internal energy
  real:: PeMinSi = -1.1e5, PeMinDim = -1.0, PeMin
  !$acc declare create(PeMin)
  real:: TeMinDim = -1.0, TeMin
  real:: ExtraEintMinSi = 0.0, ExtraEintMin

  ! Minimum threshold for MHD density and pressure
  real:: RhoMinDim_I(nFluid) = -1.0, RhoMin_I(nFluid)
  real:: pMinDim_I(nFluid)   = -1.0, pMin_I(nFluid)
  real:: TMinDim_I(nFluid)   = -1.0, TMin_I(nFluid)
  logical:: UseRhoMin, UsePMin
  !$acc declare create(RhoMin_I, pMin_I, UseRhoMin, UsePMin, TMin_I)

  ! Minimum threshold for radial velocity
  real:: SpeedMinDim, SpeedMin, rSpeedMin
  real:: TauSpeedMinDim, TauSpeedMin
  logical:: UseSpeedMin = .false.

  ! Boundary pressure for subsonic outflow
  logical:: UseOutflowPressure = .false.
  !$acc declare create(UseOutflowPressure)
  real:: pOutflowSi = -1.0, pOutflow = -1.0
  !$acc declare create(pOutflow)

  ! Relaxation time for anisotropic pressure
  logical:: UseConstantTau_I(nFluid)   =  nIonFluid > 1
  real:: TauInstabilitySi_I(nFluid) = -1.0, TauInstability_I(nFluid)
  real:: TauGlobalSi_I(nFluid)      = -1.0, TauGlobal_I(nFluid)

  ! General variables for the second body

  ! Radius and center coordinates
  real:: rBody2=0.0, xBody2=0.0, yBody2=0.0, zBody2=0.0, vBody2_D(3) = 0.0
  !$acc declare create(rBody2, xBody2, yBody2, zBody2, vBody2_D)

  ! Dimensional and diminsionless parameters on the seconnd body boundary
  real:: Body2NDim = 0.0, Body2TDim = 0.0, RhoBody2 = 0.0, pBody2 = 0.0
  !$acc declare create(Body2NDim, Body2TDim, RhoBody2, pBody2)

  ! Second body mass and gravity force parameter derived from it
  real:: MassBody2Si, gBody2 = 0.0
  !$acc declare create(MassBody2Si,gBody2)
  ! Logical determining if the orbit elements from CON_planet are used to
  ! trace a second body.
  logical:: UseBody2Orbit = .false.
  !$acc declare create(UseBody2Orbit)

  ! Variables for two-state shock tube problems
  logical:: UseShockTube = .false.
  real:: ShockLeftDim_V(nVar)=0.0, ShockRightDim_V(nVar)=0.0
  real:: ShockLeft_V(nVar)=0.0, ShockRight_V(nVar)=0.0 ! physical units
  real:: ShockPosition = 0.0, ShockSlope = 0.0

  ! Radial state parameters
  logical:: UseRadialState = .false.
  real:: AmplRDim_V(nVar)=0.0, AmplR_V(nVar)=0.0, ExponentR_V(nVar)=0.0

  ! Options for using Boundary State:
  ! 0 - not use boundary state (by default).
  ! 1 - use boundary sate (#BOUNDARYSTATE).
  ! 2 - use boundary state in num dens and temperature (#BOUNDARYSTATE_NT).
  integer:: iFaceBoundaryState_I(SolidBc_:zMaxBc_)          = 0
  integer:: iCellBoundaryState_I(Coord1MinBc_:Coord3MaxBc_) = 0

  ! State for the boundary conditions
  real::  FaceState_VI(nVar,SolidBc_:zMaxBc_), &
       FaceStateDim_VI(nVar,SolidBc_:zMaxBc_)
  !$acc declare create(FaceState_VI)
  real::  CellState_VI(nVar,Coord1MinBc_:Coord3MaxBc_), &
       CellStateDim_VI(nVar,Coord1MinBc_:Coord3MaxBc_)
  !$acc declare create(CellState_VI)

  ! Units for normalization of variables
  character(len=20):: TypeNormalization="PLANETARY"

  ! Type of units used for I/O (input params, log files, plot files, etc.)
  character(len=20):: TypeIoUnit = "PLANETARY"

  ! Named indexes for I/O variable units
  integer, parameter:: nIoUnit = 18

  integer, parameter:: UnitX_           = 1
  integer, parameter:: UnitU_           = 2
  integer, parameter:: UnitRho_         = 3
  integer, parameter:: UnitT_           = 4
  integer, parameter:: UnitN_           = 5
  integer, parameter:: UnitP_           = 6
  integer, parameter:: UnitB_           = 7
  integer, parameter:: UnitRhoU_        = 8
  integer, parameter:: UnitEnergyDens_  = 9
  integer, parameter:: UnitPoynting_    = 10
  integer, parameter:: UnitJ_           = 11
  integer, parameter:: UnitElectric_    = 12
  integer, parameter:: UnitTemperature_ = 13
  integer, parameter:: UnitDivB_        = 14
  integer, parameter:: UnitAngle_       = 15
  integer, parameter:: UnitMass_        = 16
  integer, parameter:: UnitCharge_      = 17
  integer, parameter:: UnitUnity_       = 18

  ! Conversion between units: e.g. VarSi = VarNo*No2Si_V(UnitVar_)
  ! The following should always be true: No2Si_V*Si2Io_V = No2Io_V
  real, dimension(nIoUnit):: &
       Io2Si_V, Si2Io_V, Io2No_V, No2Io_V, Si2No_V, No2Si_V
  !$acc declare create(Io2Si_V, Si2Io_V, Io2No_V, No2Io_V, Si2No_V, No2Si_V)

  ! Mapping between state array indices and unit conversion array indices
  integer:: iUnitCons_V(nVar), iUnitPrim_V(nVar)

  character(len=20):: &
       NameIdlUnit_V(nIoUnit), NameTecUnit_V(nIoUnit), NameSiUnit_V(nIoUnit)

  ! Names of the user (I/O) units for IDL and TECPlot output
  ! for all variables including energies
  character(len=20), allocatable:: NameUnitUserIdl_V(:), NameUnitUserTec_V(:)

  ! The user defined units for the variables including energies
  real, allocatable:: UnitUser_V(:)

  ! Some strange logical used in calc_heat_flux
  logical:: UseDefaultUnits = .false.

  ! Number and indexes of vector variables in State_VGB
  integer:: nVectorVar=0
  integer, allocatable:: iVectorVar_I(:) ! Index of first components

contains
  !============================================================================
  subroutine set_dipole

    use ModMain,  ONLY: tSimulation
    use CON_axes, ONLY: get_axes

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_dipole'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(Bdp == 0.0) RETURN

    if(TypeCoordSystem == 'GSM')then
       call get_axes(tSimulation, MagAxisGsmOut_D=Dipole_D)
    elseif(TypeCoordSystem == 'GSE')then
       call get_axes(tSimulation, MagAxisGseOut_D=Dipole_D)
    else
       Dipole_D = [-SinThetaTilt, 0.0, CosThetaTilt]
    end if
    Dipole_D = Dipole_D * Bdp
    !$acc update device(Dipole_D, SinThetaTilt, CosThetaTilt)

    if(DoTest) write(*,*) NameSub,': Dipole_D=', Dipole_D
    call test_stop(NameSub, DoTest)

  end subroutine set_dipole
  !============================================================================
  subroutine set_physics_constants

    ! set normalizations, physical constants, etc.

    use ModMain
    use CON_planet,  ONLY: get_planet, NamePlanet
    use ModVarIndexes
    use ModMultiFluid
    use ModAdvance, ONLY: UseElectronPressure, Pe_, UseAnisoPressure, Ppar_,&
         UseEfield, UseAnisoPe
    use BATL_lib, ONLY: IsCartesian

    real:: MassBodySi
    real:: pCoef
    real:: Charge_I(nIonFluid+1)
    integer:: i, iBoundary, iFluid, iIon, jIon

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_physics_constants'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    call set_unit_conversion_array_indices
    ! Load body rotation rates, masses, and radii
    NamePlanetRadius = 'R'
    select case(NameThisComp)
    case('GM')
       call get_planet( &
            RadiusPlanetOut   = rPlanetSi, &
            MassPlanetOut     = MassBodySi, &
            RotationPeriodOut = RotPeriodSi)
       if(NamePlanet == 'NONE') then
          rPlanetSi = 1000.0  ! 1 km
          NamePlanetRadius = 'km'
       end if
    case('SC', 'IH', 'OH', 'EE')
       if(NameStar/='SUN') then
          rPlanetSi   = RadiusStar
          MassBodySi  = MassStar
          RotPeriodSi = RotPeriodStar
       else
          rPlanetSi   = rSun
          MassBodySi  = mSun
          ! RotPeriodSi is set in set_defaults and may be read from PARAM.in
       endif
       SolarWindNDim = BodyNDim_I(1)    ! SOLARWIND normalization only
       SolarWindTempDim = BodyTDim_I(1) ! SOLARWIND normalization only
       if(NameThisComp == 'EE' .and. IsCartesian)then
          rPlanetSi = 1000.0  ! 1 km
          NamePlanetRadius = 'km'
       end if
    end select

    ! Second body mass is set to zero by default
    MassBody2Si = 0.0
    !$acc update device(MassBody2Si)
    ! Make sure that MassIon_I is consistent with MassFluid_I
    MassIon_I = MassFluid_I(1:nIonFluid)

    ! Call set_units, which set the quantities for converting from
    ! normalized to  dimensional quantities and vice versa.  Also
    ! sets input and output strings for the conversion quantities
    call set_units

    if(DoTest .and. iProc==0) then
       write(*,'(4a15)')'No2Io_V','NameIdlUnit_V','No2Si_V','NameSiUnit_V'
       do i=1, nIoUnit
          write(*,'(es15.6," ",a6,"        ",es15.6," ",a6)') &
               No2Io_V(i), NameIdlUnit_V(i), &
               No2Si_V(i), NameSiUnit_V(i)
       end do
    end if

    ! Average ion mass per charge used by Hall MHD and
    ! by ion-electron thermal exchange term
    if(TypeNormalization == 'NONE')then
       IonMassPerCharge = 1.0
    else
       IonMassPerCharge = cProtonMass/cElectronCharge &
            *Si2No_V(UnitMass_)/Si2No_V(UnitCharge_)
    end if

    ! Set the charge for electron fluid(s) to -1.0 if not set
    if(UseEfield .and. ChargeIon_I(ElectronFirst_) > 0.0) &
         ChargeIon_I(ElectronFirst_:) = -1.0

    ! Electron charge in normalized units (actually it is proton charge/mass)
    ! This is useful in formulas like n_s q_s (u_s - u_+) x B
    ! in the multi-ion momentum equation. It is used in some user modules,
    ! so we keep the name.
    ElectronCharge = 1.0/IonMassPerCharge

    ! Charge per mass in normalized units, This is needed in
    ! formulas like rho_s q_s/m_s (E + u_s x B)
    ! Note that ChargeIon_I is for charge state Z, which is not
    ! in normalized units.
    ChargePerMass_I = ChargeIon_I/MassIon_I*ElectronCharge

    ! Electron gyro-frequency coefficient in normalized units
    ! GyroFreq = ElectronGyroFreqCoef * |B|  [1/time]
    ElectronGyroFreqCoef = cGyroElectron*No2Si_V(UnitB_)/Si2No_V(UnitT_)

    ! For single ion fluid the average ion mass per charge is constant
    if(ChargeIon_I(1) /= 0 .and. nIonFluid == 1 .and. .not. UseMultiSpecies) &
         IonMassPerCharge = IonMassPerCharge * MassIon_I(1)/ChargeIon_I(1)

    ! arrays to have electron mass and charge available in an indexed
    ! arrary when no electron fluid present
    MassIonElectron_I(:nIonFluid)  = MassIon_I
    MassIonElectron_I(nIonFluid+1) = cElectronMass/cProtonMass
    Charge_I(:nIonFluid)  = ChargeIon_I
    Charge_I(nIonFluid+1) = -1.0

    ! Coefficient for effective ion-ion collision frequencies
    do jIon = 1, nIonFluid+1
       do iIon = 1, nIonFluid+1
          ReducedMass_II(iIon,jIon) = &
               MassIonElectron_I(iIon)*MassIonElectron_I(jIon) &
               /(MassIonElectron_I(iIon) + MassIonElectron_I(jIon))

          CollisionCoef_II(iIon,jIon) = CoulombLog &
	       *sqrt(ReducedMass_II(iIon,jIon)/cProtonMass) &
               /MassIonElectron_I(iIon) &
               *(Charge_I(iIon)*Charge_I(jIon)*cElectronCharge**2/cEps)**2 &
               /(3*(cTwoPi*cBoltzmann)**1.5)
       end do
    end do
    ! To obtain the effective ion-ion collision frequencies, the
    ! coefficients still need to be multiplied by N(jIon)/reducedTemp**1.5.
    ! Here, we already take care of the units.
    CollisionCoef_II = CollisionCoef_II &
         *(1/Si2No_V(UnitT_))*No2Si_V(UnitN_)/No2Si_V(UnitTemperature_)**1.5

    ! set the (corrected) speed of light and get normalization
    if(ClightDim > 0.0)then
       Clight   = ClightDim * Io2No_V(UnitU_)
    else
       Clight   = ClightFactor * cLightSpeed * Si2No_V(UnitU_)
    end if
    C2light     = Clight**2
    InvClight   = 1/Clight
    InvClight2  = 1/C2light

    ! normalize the radiation constant
    cRadiationNo = cRadiation &
         * Si2No_V(UnitEnergyDens_) / Si2No_V(UnitTemperature_)**4

    ! Convert rotation period to normalized angular velocity
    if (RotPeriodSi == 0.0) then
       OmegaBody = 0.0
    else
       OmegaBody = cTwoPi/RotPeriodSi * (1.0/Si2No_V(UnitT_))
    end if

    ! Convert gravity to non-dimensional values
    if(iDirGravity == 0)then
       ! Note: The mass of the body is in SI units
       gBody  = -cGravitation*MassBodySi*(Si2No_V(UnitU_)**2 * Si2No_V(UnitX_))
    else
       ! Normalize gravitational acceleration
       gBody  = GravitySi*(Si2No_V(UnitU_)**2 / Si2No_V(UnitX_))
    end if

    gBody2 = -cGravitation*MassBody2Si*(Si2No_V(UnitU_)**2 * Si2No_V(UnitX_))
    !$acc update device(gBody2)

    ! Normalize shocktube/uniform/radial state values
    ShockLeft_V  = ShockLeftDim_V * Io2No_V(iUnitPrim_V)
    ShockRight_V = ShockRightDim_V* Io2No_V(iUnitPrim_V)
    AmplR_V      = AmplRDim_V*Io2No_V(iUnitPrim_V)

    ! Normalize solar wind values. Note: the solarwind is in I/O units
    SolarWindN   = SolarWindNDim*Io2No_V(UnitN_)
    SolarWindRho = SolarWindN * MassIon_I(1)
    SolarWindP   = SolarWindN * &
         max(SolarWindTempDim, SwTminDim)*Io2No_V(UnitTemperature_)
    SolarWindUx  = SolarWindUxDim*Io2No_V(UnitU_)
    SolarWindUy  = SolarWindUyDim*Io2No_V(UnitU_)
    SolarWindUz  = SolarWindUzDim*Io2No_V(UnitU_)
    SolarWindBx  = SolarWindBxDim*Io2No_V(UnitB_)
    SolarWindBy  = SolarWindByDim*Io2No_V(UnitB_)
    SolarWindBz  = SolarWindBzDim*Io2No_V(UnitB_)

    if(.not.UseElectronPressure .and. IsMhd) &
         SolarWindP = SolarWindP*(1 + ElectronPressureRatio)

    ! These are useful for printing out values
    SolarWindRhoDim = SolarWindRho*No2Io_V(UnitRho_)
    SolarWindPDim   = SolarWindP*No2Io_V(UnitP_)

    ! No lower limit on polar wind density by default
    PolarRhoMin_I = 0.0
    if(UseMultiSpecies)then
       ! Species body densities
       BodyRhoSpecies_I = BodyNSpeciesDim_I*Io2No_V(UnitN_)*MassSpecies_V
       BodyRho_I(1) = sum(BodyRhoSpecies_I)
       BodyP_I(1)   = sum(BodyNSpeciesDim_I)*Io2No_V(UnitN_) &
            *BodyTDim_I(1)*Io2No_V(UnitTemperature_)
       if(DoLimitRhoPw) PolarRhoMin_I(1:nSpecies) = BodyRhoSpecies_I
    else
       ! The normalized quantities extend to the first MHD fluid too
       BodyRho_I = BodyNDim_I*Io2No_V(UnitN_)*MassFluid_I
       BodyP_I   = BodyNDim_I*Io2No_V(UnitN_)*BodyTDim_I &
            *Io2No_V(UnitTemperature_)
       if(DoLimitRhoPw) PolarRhoMin_I(1:nIonFluid) = BodyRho_I(1:nIonFluid)
    end if

    PolarRho_I = PolarNDim_I*Io2No_V(UnitN_)*MassFluid_I
    PolarP_I   = PolarNDim_I*Io2No_V(UnitN_)*PolarTDim_I &
         * Io2No_V(UnitTemperature_)
    PolarU_I   = PolarUDim_I*Io2No_V(UnitU_)

    if(.not.UseElectronPressure .and. IsMhd) then
       BodyP_I(1)  = BodyP_I(1)*(1 + ElectronPressureRatio)
       PolarP_I(1) = PolarP_I(1)*(1 + ElectronPressureRatio)
    end if

    RhoBody2= Body2NDim *Io2No_V(UnitN_)*MassIon_I(1)
    pBody2  = RhoBody2 * Body2TDim*Io2No_V(UnitTemperature_)
    if(UseBody2Orbit)call set_second_body_coord
    !$acc update device(RhoBody2,pBody2)
    ! Here the arrays of the FACE VALUE are formed
    ! Initialization
    do iBoundary = SolidBc_, zMaxBc_
       FaceState_VI(:,iBoundary) = DefaultState_V(1:nVar)
    end do

    ! For bodies:
    FaceState_VI(iRho_I, Body1_) = BodyRho_I
    FaceState_VI(iP_I,   Body1_) = BodyP_I

    if(UseMultiSpecies)then
       if(BodyRhoSpecies_I(1) > 0.0)then
          FaceState_VI(SpeciesFirst_:SpeciesLast_,Body1_) = BodyRhoSpecies_I
       else
          FaceState_VI(SpeciesFirst_,Body1_) = &
               BodyRho_I(1)*(1.0 - LowDensityRatio*(nSpecies - 1))
          FaceState_VI(SpeciesFirst_+1:SpeciesLast_,Body1_) = &
               LowDensityRatio*BodyRho_I(1)
       end if
    endif

    if(UseElectronPressure)then
       FaceState_VI(Pe_,Body1_) = sum(BodyP_I(1:nIonFluid))
       FaceState_VI(Pe_,Body2_) = pBody2
    end if

    if(UseAnisoPe) then
       FaceState_VI(Pepar_,Body1_) = FaceState_VI(Pe_,Body1_)
       FaceState_VI(Pepar_,Body2_) = FaceState_VI(Pe_,Body2_)
    end if

    if(UseAnisoPressure)then
       FaceState_VI(Ppar_,Body1_) = BodyP_I(1)
       FaceState_VI(Ppar_,Body2_) = pBody2
    end if

    ! The following part of the code is sensitive to a particular physical
    ! model. It should be modified in adding/deleting the physical effects
    ! or features

    FaceState_VI(rho_,body2_) = RhoBody2
    FaceState_VI(P_,body2_)   = pBody2

    ! For Outer Boundaries (if SW_* are set)
    if(SolarWindRho > 0.0)then

       FaceState_VI(Rho_,xMinBc_:zMaxBc_) = SolarWindRho
#ifndef SCALAR
       FaceState_VI(Ux_, xMinBc_:zMaxBc_) = SolarWindUx
       FaceState_VI(Uy_, xMinBc_:zMaxBc_) = SolarWindUy
       FaceState_VI(Uz_, xMinBc_:zMaxBc_) = SolarWindUz
       FaceState_VI(Bx_, xMinBc_:zMaxBc_) = SolarWindBx
       FaceState_VI(By_, xMinBc_:zMaxBc_) = SolarWindBy
       FaceState_VI(Bz_, xMinBc_:zMaxBc_) = SolarWindBz
       FaceState_VI(P_,  xMinBc_:zMaxBc_) = SolarWindP
#endif
       if(UseElectronPressure) FaceState_VI(Pe_,xMinBc_:zMaxBc_) &
            = SolarWindP*ElectronPressureRatio

       if(UseAnisoPe) FaceState_VI(Pepar_,xMinBc_:zMaxBc_) &
            = SolarWindP*ElectronPressureRatio

       if(UseAnisoPressure) FaceState_VI(Ppar_,xMinBc_:zMaxBc_) = SolarWindP

       if(UseMultiSpecies) then
          FaceState_VI(SpeciesFirst_, xMinBc_:zMaxBc_) &
               = SolarWindRho*(1 - LowDensityRatio*(nSpecies - 1))
          FaceState_VI(SpeciesFirst_+1:SpeciesLast_, xMinBc_:zMaxBc_) &
               = LowDensityRatio*SolarWindRho
       endif

       if(nFluid > 1)then
          ! Ratio of total pressure and sum of ion pressures
          ! depends on UseElectronPressure
          pCoef = 1 + ElectronPressureRatio
          if(UseElectronPressure) pCoef = 1.0

          ! The first ion fluid contains most of the mass density of the solar
          ! wind. The other ions carry the rest. The neutrals are not included.
          ! We preserve the temperature of the solar wind, so the pressure
          ! changes proportionally to the density.
          iFluid = 1
          call select_fluid(iFluid)
          FaceState_VI(iRho,xMinBc_:zMaxBc_) = &
               SolarWindRho*(1 - LowDensityRatio*(nIonFluid - 1))
          FaceState_VI( iUx,xMinBc_:zMaxBc_) = SolarWindUx
          FaceState_VI( iUy,xMinBc_:zMaxBc_) = SolarWindUy
          FaceState_VI( iUz,xMinBc_:zMaxBc_) = SolarWindUz
          ! Use solar wind temperature and reduced density to get pressure
          FaceState_VI( iP,xMinBc_:zMaxBc_) = SolarWindP/pCoef &
               *(1.0 - LowDensityRatio*(nIonFluid-1))

          do iFluid = 2, nFluid
             call select_fluid(iFluid)
             FaceState_VI(iRho,xMinBc_:zMaxBc_) = SolarWindRho*LowDensityRatio
             FaceState_VI( iUx,xMinBc_:zMaxBc_) = SolarWindUx
             FaceState_VI( iUy,xMinBc_:zMaxBc_) = SolarWindUy
             FaceState_VI( iUz,xMinBc_:zMaxBc_) = SolarWindUz
             ! Use solar wind temperature and reduced density to get pressure
             FaceState_VI( iP,xMinBc_:zMaxBc_) = SolarWindP/pCoef &
                  *LowDensityRatio*MassIon_I(1)/MassFluid_I(iFluid)
          end do
       end if
    end if

    ! Use face values from #BOUNDARYSTATE commands
    do iBoundary = SolidBc_, 2*nDim
       if (iFaceBoundaryState_I(iBoundary) == 0) CYCLE

       if (iFaceBoundaryState_I(iBoundary)  < 0 .or.                     &
            iFaceBoundaryState_I(iBoundary) > 2) call stop_mpi(NameSub// &
            ' incorrect iFaceBoundaryState_I')

       FaceState_VI(:,iBoundary) = &
            FaceStateDim_VI(:,iBoundary) * Io2No_V(iUnitPrim_V)

       ! Fix unit conversion For #BOUNDARYSTATE_NT
       if (iFaceBoundaryState_I(iBoundary) == 2) then
          ! overwrite iRho_I, the unit conversion should be UnitN_
          FaceState_VI(iRho_I,iBoundary) = FaceStateDim_VI(iRho_I,iBoundary) &
               *Io2No_V(UnitN_)

          ! overwrite iP_I, the unit conversion should be UnitTemperature_
          FaceState_VI(iP_I, iBoundary)   = FaceStateDim_VI(iP_I, iBoundary) &
               *Io2No_V(UnitTemperature_)

          ! iP_I in NO units are n*T
          FaceState_VI(iP_I, iBoundary) = FaceState_VI(iP_I, iBoundary)   &
               *FaceState_VI(iRho_I, iBoundary)

          if(UseElectronPressure) then
             FaceState_VI(Pe_,iBoundary) = FaceStateDim_VI(Pe_,iBoundary)   &
                  *Io2No_V(UnitTemperature_)
             FaceState_VI(Pe_,iBoundary) = FaceState_VI(Pe_,iBoundary)   &
                  *sum(FaceState_VI(iRhoIon_I, iBoundary))
          end if

          ! Finally convert number density to mass density
          FaceState_VI(iRho_I,iBoundary) = &
               FaceState_VI(iRho_I,iBoundary)*MassFluid_I
       end if

    end do

    ! By default use the box boundary state for the outer BC state
    CellState_VI(:,Coord1MinBc_:Coord3MaxBc_) = FaceState_VI(:,xMinBc_:zMaxBc_)

    ! Use cell values from #BOUNDARYSTATE commands given in primitive variables
    do iBoundary = 1, 2*nDim
       if (iCellBoundaryState_I(iBoundary) == 0) CYCLE

       if (iCellBoundaryState_I(iBoundary)  < 0 .or.                     &
            iCellBoundaryState_I(iBoundary) > 2) call stop_mpi(NameSub// &
            ' incorrect iCellBoundaryState_I')

       CellState_VI(:,iBoundary) = &
            CellStateDim_VI(:,iBoundary) * Io2No_V(iUnitPrim_V)

       ! Fix unit conversion For #BOUNDARYSTATE_NT
       if (iCellBoundaryState_I(iBoundary) == 2) then
          ! overwrite iRho_I, the unit conversion should be UnitN_
          CellState_VI(iRho_I,iBoundary) = CellStateDim_VI(iRho_I,iBoundary) &
               *Io2No_V(UnitN_)

          ! overwrite iP_I, the unit conversion should be UnitTemperature_
          CellState_VI(iP_I, iBoundary)   = CellStateDim_VI(iP_I, iBoundary) &
               *Io2No_V(UnitTemperature_)

          ! iP_I in NO units are n*T
          CellState_VI(iP_I, iBoundary) = CellState_VI(iP_I, iBoundary)   &
               *CellState_VI(iRho_I, iBoundary)

          if(UseElectronPressure) then
             CellState_VI(Pe_,iBoundary) = CellStateDim_VI(Pe_,iBoundary)   &
                  *Io2No_V(UnitTemperature_)
             CellState_VI(Pe_,iBoundary) = CellState_VI(Pe_,iBoundary)   &
                  *sum(CellState_VI(iRhoIon_I, iBoundary))
          end if

          ! Finally convert number density to mass density
          CellState_VI(iRho_I,iBoundary) = &
               CellState_VI(iRho_I,iBoundary)*MassFluid_I
       end if
    end do

    ! Convert velocity to momentum for all fluids and boundaries
#ifndef SCALAR
    do iFluid = 1, nFluid
       if(nFluid > 1) call select_fluid(iFluid)
       CellState_VI(iRhoUx,:) = CellState_VI(iUx,:)*CellState_VI(iRho,:)
       CellState_VI(iRhoUy,:) = CellState_VI(iUy,:)*CellState_VI(iRho,:)
       CellState_VI(iRhoUz,:) = CellState_VI(iUz,:)*CellState_VI(iRho,:)
    end do
#endif
    if(UseOutflowPressure) pOutflow = pOutflowSi*Si2No_V(UnitP_)

    ! Minimum value for densities, pressures, energy
    RhoMin_I     = RhoMinDim_I*Io2No_V(UnitRho_)
    pMin_I       = pMinDim_I*Io2No_V(UnitP_)
    PeMin        = max(PeMinSi*Si2No_V(UnitP_), PeMinDim*Io2No_V(UnitP_))
    TMin_I       = TMinDim_I*Io2No_V(UnitTemperature_)
    TeMin        = TeMinDim*Io2No_V(UnitTemperature_)
    ExtraEintMin = ExtraEintMinSi*Si2No_V(UnitEnergyDens_)
    UseRhoMin    = any(RhoMin_I > 0.0)
    UsePMin      = any(pMin_I > 0.0)

    ! Minimum value for radial speed
    if(UseSpeedMin)then
       SpeedMin    = SpeedMinDim*Io2No_V(UnitU_)
       TauSpeedMin = TauSpeedMinDim*Io2No_V(UnitT_)
    end if

    ! Relaxation parameters for anisotropic ion pressure
    TauInstability_I  = TauInstabilitySi_I*Si2No_V(UnitT_)
    TauGlobal_I  = TauGlobalSi_I*Si2No_V(UnitT_)

    ! Now do the magnetic field stuff
    ! Monopole strength (usually zero)
    if(TypeNormalization == 'NONE')then
       MonopoleStrength = MonopoleStrengthSi
    else
       MonopoleStrength = MonopoleStrengthSi*Si2No_V(UnitB_)
    end if

    ! The *ThetaTilt and Bdp variables are not needed for GM except
    ! for reporting them in write_progress.

    ! Nondimensionalize dipole strength.
    Bdp = DipoleStrengthSi*Si2No_V(UnitB_)

    BdpBody2_D = BdpDimBody2_D*Io2No_V(UnitB_)

    ! Compute dipole tilt variables
    ! For IH ThetaTilt should be set with the #HELIODIPOLETILT command
    ! For static dipole only, e.g. in 2D
    CosThetaTilt = cos(ThetaTilt)
    SinThetaTilt = sin(ThetaTilt)

    ! Initialize Dipole_D vector
    call set_dipole

    ! Hyperbolic cleaning uses SpeedHyp velocity
    if(UseHyperbolicDivb .and. SpeedHypDim > 0) &
         SpeedHyp  = SpeedHypDim*Io2No_V(UnitU_)

    !$acc update device(SpeedHyp, C2light, InvClight, InvClight2, ClightFactor)
    !$acc update device(pOutflow)

    !$acc update device(CellState_VI)
    !$acc update device(FaceState_VI)

    !$acc update device(RhoMin_I, pMin_I, UseRhoMin, UsePMin, PeMin, TMin_I)
    !$acc update device(OmegaBody, Bdp)
    !$acc update device(gBody)

    !$acc update device(PolarRho_I, PolarP_I)

    !$acc update device(MassIon_I, ChargeIon_I, ChargePerMass_I)
    !$acc update device(IonMassPerCharge)

    !$acc update device(CollisionCoef_II)

    !$acc update device(rBody)

    call test_stop(NameSub, DoTest)

  end subroutine set_physics_constants
  !============================================================================
  subroutine set_units

    use ModMain
    use ModVarIndexes
    use ModMultiFluid, ONLY: MassIon_I
    use ModUserInterface ! user_io_units, user_normalization

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_units'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! set variables for converting from normalized to SI units and back:
    !
    ! Normalized*UnitSi = Si, Si/UnitSi = Normalized
    !
    ! There are three independent variables: distance(x), velocity(u)
    ! and density(rho).  All others are built from these three.
    select case(TypeNormalization)
    case("PLANETARY")
       No2Si_V(UnitX_)   = rPlanetSi           ! rPlanet
       No2Si_V(UnitU_)   = rPlanetSi           ! rPlanet/s
       No2Si_V(UnitRho_) = 1000000*cProtonMass ! amu/cm^3
    case("HELIOSPHERIC")
       No2Si_V(UnitX_)   = rPlanetSi           ! rPlanet/rStar
       No2Si_V(UnitU_)   = 1000.0              ! km/s
       No2Si_V(UnitRho_) = 1000000*cProtonMass ! amu/cm^3
    case("OUTERHELIO")
       No2Si_V(UnitX_)   = cAU                 ! AU
       No2Si_V(UnitU_)   = 1000.0              ! km/s
       No2Si_V(UnitRho_) = 1000000*cProtonMass ! amu/cm^3
    case("SOLARWIND")
       ! rPlanet, SW sound speed, SW density in amu/cm^3
       No2Si_V(UnitX_)   = rPlanetSi
       if(NameThisComp=='OH')No2Si_V(UnitX_)=cAU
       No2Si_V(UnitU_)   = &
            sqrt(Gamma*cBoltzmann*SolarWindTempDim/cProtonMass/MassIon_I(1))
       No2Si_V(UnitRho_) = 1000000*cProtonMass*MassIon_I(1)*SolarWindNDim
    case("NONE", "READ")
       ! Already set in set_parameters
    case("USER")
       call user_normalization
    case default
       call stop_mpi(NameSub//' ERROR: unknown TypeNormalization='// &
            trim(TypeNormalization))
    end select

    ! Set other normalizing SI variables from the independent ones.
    !
    ! For sake of convenience
    !  units of B are chosen to satisfy v_A = B/sqrt(rho)       (mu0= 1)
    !  units of n are chosen to satisfy  n  = rho/(ionmass/amu) (mp = 1)
    !  units of T are chosen to satisfy  T  = p/n               (kBoltzmann=1)
    !
    ! Note that No2Si_V(UnitN_) is NOT EQUAL TO 1/No2Si_V(UnitX_)^3 !!!
    No2Si_V(UnitT_)          = No2Si_V(UnitX_)/No2Si_V(UnitU_)      ! s
    No2Si_V(UnitN_)          = No2Si_V(UnitRho_)/cProtonMass        ! #/m^3
    No2Si_V(UnitP_)          = No2Si_V(UnitRho_)*No2Si_V(UnitU_)**2 ! Pa
    No2Si_V(UnitB_)          = No2Si_V(UnitU_) &
         *sqrt(cMu*No2Si_V(UnitRho_))                               ! T
    No2Si_V(UnitRhoU_)       = No2Si_V(UnitRho_)*No2Si_V(UnitU_)    ! kg/m^2/s
    No2Si_V(UnitEnergyDens_) = No2Si_V(UnitP_)                      ! J/m^3
    No2Si_V(UnitPoynting_)   = &
         No2Si_V(UnitEnergyDens_)*No2Si_V(UnitU_)! J/m^2/s
    No2Si_V(UnitJ_)          = No2Si_V(UnitB_)/( No2Si_V(UnitX_)*cMu ) ! A/m^2
    No2Si_V(UnitElectric_)   = No2Si_V(UnitU_)*No2Si_V(UnitB_)         ! V/m
    No2Si_V(UnitTemperature_)= No2Si_V(UnitP_) &
         /( No2Si_V(UnitN_)*cBoltzmann )                               ! K
    No2Si_V(UnitDivB_)       = No2Si_V(UnitB_)/No2Si_V(UnitX_)         ! T/m
    No2Si_V(UnitAngle_)      = 1.0                                     ! radian
    No2Si_V(UnitMass_)       = No2Si_V(UnitRho_)*No2Si_V(UnitX_)**3    ! kg
    No2Si_V(UnitCharge_)     = No2Si_V(UnitJ_)/No2Si_V(UnitU_) &       ! C
         *No2Si_V(UnitX_)**3
    No2Si_V(UnitUnity_)      = 1.0   ! Fallback conversion for undefined units

    ! Set inverse conversion SI -> normalized
    Si2No_V = 1.0/No2Si_V

    ! set variables to go from Input/Output units to SI units:
    !
    ! Io*Io2Si_V = Si
    !
    ! Note that the input/output units are not necessarily consistent, e.g.
    ! units of distance divided by units of time does not necessarily
    ! coincide with the units of velocity.
    !
    ! Also load the unit name strings for IDL and TEC output

    ! As a default use SI units, so below only the differences need to be set
    Io2Si_V = 1.0
    No2Io_V = No2Si_V

    ! set string variables used for writing Tecplot output
    NameTecUnit_V(UnitX_)           = '[m]'
    NameTecUnit_V(UnitU_)           = '[m/s]'
    NameTecUnit_V(UnitRho_)         = '[kg/m^3]'
    NameTecUnit_V(UnitT_)           = '[s]'
    NameTecUnit_V(UnitN_)           = '[m^-^3]'
    NameTecUnit_V(UnitP_)           = '[Pa]'
    NameTecUnit_V(UnitB_)           = '[T]'
    NameTecUnit_V(UnitRhoU_)        = '[kg m^-^2 s^-^1]'
    NameTecUnit_V(UnitEnergydens_)  = '[J/m^3]'
    NameTecUnit_V(UnitPoynting_)    = '[J m^-^2 s^-^1]'
    NameTecUnit_V(UnitJ_)           = '[A/m^2]'
    NameTecUnit_V(UnitElectric_)    = '[V/m]'
    NameTecUnit_V(UnitTemperature_) = '[K]'
    NameTecUnit_V(UnitDivB_)        = '[T/m]'
    NameTecUnit_V(UnitAngle_)       = '[rad]'
    ! set string variables used for writing IDL output
    NameIdlUnit_V(UnitX_)           = 'm'
    NameIdlUnit_V(UnitRho_)         = 'kg/m3'
    NameIdlUnit_V(UnitU_)           = 'm/s'
    NameIdlUnit_V(UnitT_)           = 's'
    NameIdlUnit_V(UnitN_)           = '/m3'
    NameIdlUnit_V(UnitP_)           = 'Pa'
    NameIdlUnit_V(UnitB_)           = 'T'
    NameIdlUnit_V(UnitRhoU_)        = 'kg/m2s2'
    NameIdlUnit_V(UnitEnergyDens_)  = 'J/m3'
    NameIdlUnit_V(UnitPoynting_)    = 'J/m2s'
    NameIdlUnit_V(UnitJ_)           = 'A/m2'
    NameIdlUnit_V(UnitElectric_)    = 'V/m'
    NameIdlUnit_V(Unittemperature_) = 'K'
    NameIdlUnit_V(UnitDivB_)        = 'T/m'
    NameIdlUnit_V(UnitAngle_)       = 'rad'

    ! Store SI unit names for writing out variables in SI units
    NameSiUnit_V = NameIdlUnit_V

    select case(TypeIoUnit)
    case("SI")
       ! Already set above
    case("PLANETARY")
       Io2Si_V(UnitX_)        = rPlanetSi                ! planetary radii
       Io2Si_V(UnitDivB_)     = 1.0E-9/rPlanetSi         ! nT/R_planet
       Io2Si_V(UnitRho_)      = 1.0E6*cProtonMass        ! Mp/cm^3
       Io2Si_V(UnitN_)        = 1.0E6                    ! #/cm^3
       Io2Si_V(UnitU_)        = 1.0E3                    ! km/s
       Io2Si_V(UnitP_)        = 1.0E-9                   ! nPa
       Io2Si_V(UnitB_)        = 1.0E-9                   ! nT
       Io2Si_V(UnitJ_)        = 1.0E-6                   ! microA/m^2
       Io2Si_V(UnitElectric_) = 1.0E-3                   ! mV/m
       Io2Si_V(UnitAngle_)    = cRadToDeg                ! degrees
       ! set string variables used for writing output - TECPLOT
       NameTecUnit_V(UnitX_)           = '['//trim(NamePlanetRadius)//']'
       NameTecUnit_V(UnitRho_)         = '[amu/cm^3]'
       NameTecUnit_V(UnitU_)           = '[km/s]'
       NameTecUnit_V(UnitN_)           = '[cm^-^3]'
       NameTecUnit_V(UnitP_)           = '[nPa]'
       NameTecUnit_V(UnitB_)           = '[nT]'
       NameTecUnit_V(UnitJ_)           = '[`mA/m^2]'
       NameTecUnit_V(UnitElectric_)    = '[mV/m]'
       NameTecUnit_V(UnitDivB_)        = '[nT/'//trim(NamePlanetRadius)//']'
       NameTecUnit_V(UnitAngle_)       = '[deg]'

       ! set string variables used for writing output - IDL
       NameIdlUnit_V(UnitX_)           = NamePlanetRadius
       NameIdlUnit_V(UnitRho_)         = 'Mp/cc'
       NameIdlUnit_V(UnitU_)           = 'km/s'
       NameIdlUnit_V(UnitN_)           = '/cc'
       NameIdlUnit_V(UnitP_)           = 'nPa'
       NameIdlUnit_V(UnitB_)           = 'nT'
       NameIdlUnit_V(UnitJ_)           = 'uA/m2'
       NameIdlUnit_V(UnitElectric_)    = 'mV/m'
       NameIdlUnit_V(UnitDivB_)        = 'nT/'//NamePlanetRadius
       NameIdlUnit_V(UnitAngle_)       = 'deg'

    case("HELIOSPHERIC")
       Io2Si_V(UnitX_)           = rPlanetSi                 ! R
       Io2Si_V(UnitRho_)         = 1.0E+3                    ! g/cm^3
       Io2Si_V(UnitN_)           = 1.0E+6                    ! #/cm^3
       Io2Si_V(UnitU_)           = 1.0E+3                    ! km/s
       Io2Si_V(UnitP_)           = 1.0E-1                    ! dyne/cm^2
       Io2Si_V(UnitB_)           = 1.0E-4                    ! Gauss
       Io2Si_V(UnitRhoU_)        = 1.0E+1                    ! g/cm^2/s
       Io2Si_V(UnitEnergydens_)  = 1.0E-1                    ! erg/cm^3
       Io2Si_V(UnitJ_)           = 1.0E-6                    ! uA/m^2
       Io2Si_V(UnitDivB_)        = 1.0E-2                    ! Gauss/cm
       Io2Si_V(UnitAngle_)       = cRadToDeg                 ! degrees
       ! set string variables used for writing output - TECPLOT
       NameTecUnit_V(UnitX_)           = '[R]'
       NameTecUnit_V(UnitRho_)         = '[g/cm^3]'
       NameTecUnit_V(UnitU_)           = '[km/s]'
       NameTecUnit_V(UnitN_)           = '[amu/cm^3]'
       NameTecUnit_V(UnitP_)           = '[dyne/cm^2]'
       NameTecUnit_V(UnitB_)           = '[Gauss]'
       NameTecUnit_V(UnitRhoU_)        = '[g/cm^2/s]'
       NameTecUnit_V(UnitEnergyDens_)  = '[erg/cm^3]'
       NameTecUnit_V(UnitJ_)           = '[`mA/m^2]'
       NameTecUnit_V(UnitDivB_)        = '[Gauss/cm]'
       NameTecUnit_V(UnitAngle_)       = '[deg]'
       ! set string variables used for writing output - IDL
       NameIdlUnit_V(UnitX_)           = 'R'
       NameIdlUnit_V(UnitRho_)         = 'g/cm3'
       NameIdlUnit_V(UnitU_)           = 'km/s'
       NameIdlUnit_V(UnitN_)           = 'mp/cc'
       NameIdlUnit_V(UnitP_)           = 'dyne/cm^2'
       NameIdlUnit_V(UnitB_)           = 'G'
       NameIdlUnit_V(UnitRhoU_)        = 'g/cm^2/s'
       NameIdlUnit_V(UnitEnergyDens_)  = 'erg/cm3'
       NameIdlUnit_V(UnitJ_)           = 'uA/m2'
       NameIdlUnit_V(UnitDivB_)        = 'G/cm'
       NameIdlUnit_V(UnitTemperature_) = 'K'
       NameIdlUnit_V(UnitAngle_)       = 'deg'

    case("OUTERHELIO")
       Io2Si_V(UnitX_)           = cAU                       ! R
       Io2Si_V(UnitRho_)         = 1.0E+6*cProtonMass        ! Mp/cm^3
       Io2Si_V(UnitN_)           = 1.0E+6                    ! #/cm^3
       Io2Si_V(UnitU_)           = 1.0E+3                    ! km/s
       Io2Si_V(UnitP_)           = 1.0E-1                    ! dyne/cm^2
       Io2Si_V(UnitB_)           = 1.0E-9                    ! nT
       Io2Si_V(UnitRhoU_)        = 1.0E+1                    ! g/cm^2/s
       Io2Si_V(UnitEnergydens_)  = 1.0E-1                    ! erg/cm^3
       Io2Si_V(UnitJ_)           = 1.0E-6                    ! uA/m^2
       Io2Si_V(UnitDivB_)        = 1.0E-2                    ! Gauss/cm
       Io2Si_V(UnitAngle_)       = cRadToDeg                 ! degrees
       ! set string variables used for writing output - TECPLOT
       NameTecUnit_V(UnitX_)            = 'AU'
       NameTecUnit_V(UnitRho_)          = 'amu/cm3'
       NameTecUnit_V(UnitN_)            = '#/cm3'
       NameTecUnit_V(UnitU_)            = 'km/s'
       NameTecUnit_V(UnitP_)            = 'dyne/cm^2'
       NameTecUnit_V(UnitB_)            = 'nT'
       NameTecUnit_V(UnitRhoU_)         = 'g/cm^2/s'
       NameTecUnit_V(UnitEnergydens_)   = 'erg/cm^3'
       NameTecUnit_V(UnitJ_)            = 'uA/m^2'
       NameTecUnit_V(UnitDivB_)         = 'G/cm'
       NameTecUnit_V(UnitAngle_)        = 'deg'
       ! set string variables used for writing output - IDL
       NameIdlUnit_V(UnitX_)            = 'AU'
       NameIdlUnit_V(UnitRho_)          = 'amu/cc'
       NameIdlUnit_V(UnitN_)            = '/cc'
       NameIdlUnit_V(UnitU_)            = 'km/s'
       NameIdlUnit_V(UnitP_)            = 'dyne/cm^2'
       NameIdlUnit_V(UnitB_)            = 'nT'
       NameIdlUnit_V(UnitRhoU_)         = 'g/cm^2/s'
       NameIdlUnit_V(UnitEnergydens_)   = 'erg/cm^3'
       NameIdlUnit_V(UnitJ_)            = 'uA/m^2'
       NameIdlUnit_V(UnitDivB_)         = 'G/cm'
       NameIdlUnit_V(UnitAngle_)        = 'deg'

    case("NONE")
       ! I/O and normalized units are the same, so
       Io2Si_V = No2Si_V
       NameTecUnit_V = ''
       NameIdlUnit_V = ''

    case("USER")
       ! User method provides the conversion from I/O to SI units
       ! and Tecplot and IDL strings for all units differing from SI units.
       call user_io_units
    case default
       call stop_mpi(NameThisComp//': Unknown TypeIoUnit='//TypeIoUnit)
    end select

    ! Calculate the remaining unit conversions
    Si2Io_V = 1/Io2Si_V
    No2Io_V = No2Si_V*Si2Io_V
    Io2No_V = 1/No2Io_V

    !$acc update device(Io2Si_V, Si2Io_V, Io2No_V, No2Io_V, Si2No_V, No2Si_V)

    call test_stop(NameSub, DoTest)
  end subroutine set_units
  !============================================================================
  subroutine set_unit_conversion_array_indices
    use ModVarIndexes
    use ModMultiFluid

    integer:: iVar
    character(len=len(NameVar_V)):: NameVar
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_unit_conversion_array_indices'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Set mapping from state variable indices to unit conversion array indices
    do iVar = 1, nVar
       NameVar = NameVarLower_V(iVar)
       call extract_fluid_name(NameVar)
       select case(NameVar)
       case('rho')
          iUnitCons_V(iVar) = UnitRho_
          iUnitPrim_V(iVar) = UnitRho_
       case('mx','my','mz','rhoux','rhouy','rhouz')
          iUnitCons_V(iVar) = UnitRhoU_
          iUnitPrim_V(iVar) = UnitU_
       case('bx','by','bz','hyp')
          iUnitCons_V(iVar) = UnitB_
          iUnitPrim_V(iVar) = UnitB_
       case('p','pe', 'ppar')
          iUnitCons_V(iVar) = UnitP_
          iUnitPrim_V(iVar) = UnitP_
       case('e', 'ew', 'wd', 'ehot', 'eint')
          iUnitCons_V(iVar) = UnitEnergyDens_
          iUnitPrim_V(iVar) = UnitEnergyDens_
       case('ex', 'ey', 'ez','hype')
          iUnitCons_V(iVar) = UnitElectric_
          iUnitPrim_V(iVar) = UnitElectric_
       case default
          if(iVar >= SpeciesFirst_ .and. iVar <= SpeciesLast_)then
             iUnitCons_V(iVar) = UnitRho_
             iUnitPrim_V(iVar) = UnitRho_
          elseif(WaveFirst_ <= iVar .and. iVar <= WaveLast_)then
             iUnitCons_V(iVar) = UnitEnergyDens_
             iUnitPrim_V(iVar) = UnitEnergyDens_
          elseif(PuiFirst_ <= iVar .and. iVar <= PuiLast_)then
             iUnitCons_V(iVar) = UnitRho_
             iUnitPrim_V(iVar) = UnitRho_
          elseif(iVar >= ChargeStateFirst_ .and. iVar <= ChargeStateLast_)then
             iUnitCons_V(iVar) = UnitRho_
             iUnitPrim_V(iVar) = UnitRho_
          else
             if(iProc == 0) write(*,*) NameSub,': ',NameVar, &
                  ' was not found to set iUnitState_V,', &
                  ' using UnitUnity_ instead.'
             iUnitCons_V(iVar) = UnitUnity_
             iUnitPrim_V(iVar) = UnitUnity_
          end if
       end select
    end do

    call test_stop(NameSub, DoTest)
  end subroutine set_unit_conversion_array_indices
  !============================================================================
  subroutine init_mhd_variables

    ! Set default I/O units and unit names for the state variables
    ! in MHD type equations

    use ModVarIndexes
    use ModMultiFluid
    use ModAdvance, ONLY: UseElectronPressure, UseAnisoPressure, UseIdealEos, &
         UseEfield, UseAnisoPe
    use ModMain,    ONLY: UseB
    ! use ModSaMhd,    ONLY: UseSaMhd

    integer:: iVar, iFluid

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_mhd_variables'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(.not.allocated(UnitUser_V)) allocate(UnitUser_V(nVar+nFluid), &
         NameUnitUserTec_V(nVar+nFluid), NameUnitUserIdl_V(nVar+nFluid))

    UnitUser_V = 1.0
    NameUnitUserTec_V = ''
    NameUnitUserIdl_V = ''

    if(UseB)then
       UnitUser_V(Bx_:Bz_)        = No2Io_V(UnitB_)
       NameUnitUserTec_V(Bx_:Bz_) = NameTecUnit_V(UnitB_)
       NameUnitUserIdl_V(Bx_:Bz_) = NameIdlUnit_V(UnitB_)
    end if

    if(UseEfield)then
       UnitUser_V(Ex_:Ez_)        = No2Io_V(UnitElectric_)
       NameUnitUserTec_V(Ex_:Ez_) = NameTecUnit_V(UnitElectric_)
       NameUnitUserIdl_V(Ex_:Ez_) = NameIdlUnit_V(UnitElectric_)
    end if

    if(HypE_ > 1) then
       ! Scalar field Psi used in hyperbolic constraint of electric field
       UnitUser_V(HypE_)          = No2Io_V(UnitElectric_)
       NameUnitUserTec_V(HypE_)   = NameTecUnit_V(UnitElectric_)
       NameUnitUserIdl_V(HypE_)   = NameIdlUnit_V(UnitElectric_)
    end if

    do iFluid = 1, nFluid
       if(nFluid > 1) call select_fluid(iFluid)
       UnitUser_V(iRho)          = No2Io_V(UnitRho_)
       NameUnitUserTec_V(iRho)   = NameTecUnit_V(UnitRho_)
       NameUnitUserIdl_V(iRho)   = NameIdlUnit_V(UnitRho_)
#ifndef SCALAR
       UnitUser_V(iRhoUx:iRhoUz) = No2Io_V(UnitRhoU_)
       UnitUser_V(iP)            = No2Io_V(UnitP_)
       UnitUser_V(iEnergy)       = No2Io_V(UnitEnergyDens_)

       NameUnitUserTec_V(iRhoUx:iRhoUz) = NameTecUnit_V(UnitRhoU_)
       NameUnitUserTec_V(iP)            = NameTecUnit_V(UnitP_)
       NameUnitUserTec_V(iEnergy)       = NameTecUnit_V(UnitEnergyDens_)

       NameUnitUserIdl_V(iRhoUx:iRhoUz) = NameIdlUnit_V(UnitRhoU_)
       NameUnitUserIdl_V(iP)            = NameIdlUnit_V(UnitP_)
       NameUnitUserIdl_V(iEnergy)       = NameIdlUnit_V(UnitEnergyDens_)
#endif
    end do

    ! By default the scalar advected variables are assumed to be like density
    do iVar = ScalarFirst_, ScalarLast_
       UnitUser_V(iVar)        = No2Io_V(UnitRho_)
       NameUnitUserTec_V(iVar) = NameTecUnit_V(UnitRho_)
       NameUnitUserIdl_V(iVar) = NameIdlUnit_V(UnitRho_)
    end do

    if(UseElectronPressure)then
       UnitUser_V(Pe_)        = No2Io_V(UnitP_)
       NameUnitUserTec_V(Pe_) = NameTecUnit_V(UnitP_)
       NameUnitUserIdl_V(Pe_) = NameIdlUnit_V(UnitP_)
    end if

    if(UseAnisoPe) then
       UnitUser_V(Pepar_)        = No2Io_V(UnitP_)
       NameUnitUserTec_V(Pepar_) = NameTecUnit_V(UnitP_)
       NameUnitUserIdl_V(Pepar_) = NameIdlUnit_V(UnitP_)
    end if

    if(UseAnisoPressure)then
       do iFluid = 1, nIonFluid
          UnitUser_V(iPparIon_I(iFluid))        = No2Io_V(UnitP_)
          NameUnitUserTec_V(iPparIon_I(iFluid)) = NameTecUnit_V(UnitP_)
          NameUnitUserIdl_V(iPparIon_I(iFluid)) = NameIdlUnit_V(UnitP_)
       end do
    end if

    if(WaveLast_ > 1)then
       ! Set the unit and unit name for the wave energy variable
       UnitUser_V(WaveFirst_:WaveLast_)        = No2Io_V(UnitEnergyDens_)
       NameUnitUserTec_V(WaveFirst_:WaveLast_) = NameTecUnit_V(UnitEnergyDens_)
       NameUnitUserIdl_V(WaveFirst_:WaveLast_) = NameIdlUnit_V(UnitEnergyDens_)
    end if

    if(WDiff_ > 1)then
       ! Set the unit and unit name for the difference in the turbulent
       ! kinetic and magnetic energy
       UnitUser_V(WDiff_)        = No2Io_V(UnitEnergyDens_)
       NameUnitUserTec_V(WDiff_) = NameTecUnit_V(UnitEnergyDens_)
       NameUnitUserIdl_V(WDiff_) = NameIdlUnit_V(UnitEnergyDens_)
    end if

    if(Ew_ > 1)then
       ! Set the unit and unit name for the total wave energy variable
       UnitUser_V(Ew_)        = No2Io_V(UnitEnergyDens_)
       NameUnitUserTec_V(Ew_) = NameTecUnit_V(UnitEnergyDens_)
       NameUnitUserIdl_V(Ew_) = NameIdlUnit_V(UnitEnergyDens_)
    end if

    if(PuiLast_ > 1)then
       ! Set the unit and unit name for the pickup ion variable
       UnitUser_V(PuiFirst_:PuiLast_)        = No2Io_V(UnitRho_)
       NameUnitUserTec_V(PuiFirst_:PuiLast_) = NameTecUnit_V(UnitRho_)
       NameUnitUserIdl_V(PuiFirst_:PuiLast_) = NameIdlUnit_V(UnitRho_)
    end if

    if(.not.UseIdealEos)then
       UnitUser_V(ExtraEint_)        = No2Io_V(UnitEnergyDens_)
       NameUnitUserTec_V(ExtraEint_) = NameTecUnit_V(UnitEnergyDens_)
       NameUnitUserIdl_V(ExtraEint_) = NameIdlUnit_V(UnitEnergyDens_)
    end if

    if(Hyp_ > 1)then
       ! Set the scalar field Phi used in hyperbolic cleaning
       UnitUser_V(Hyp_) = No2Io_V(UnitB_)
       NameUnitUserTec_V(Hyp_) = NameTecUnit_V(UnitB_)
       NameUnitUserIdl_V(Hyp_) = NameIdlUnit_V(UnitB_)
    end if

    if(SignB_ > 1)then
       UnitUser_V(SignB_)        = No2Io_V(UnitB_)/No2Io_V(UnitU_)
       NameUnitUserTec_V(SignB_) = 'Gs*s/km'
       NameUnitUserIdl_V(SignB_) = 'Gs*s/km'
    end if

    if(Ehot_ > 1)then
       ! Set the unit and unit name for the extra internal energy due
       ! to suprathermal electrons
       UnitUser_V(Ehot_)        = No2Io_V(UnitEnergyDens_)
       NameUnitUserTec_V(Ehot_) = NameTecUnit_V(UnitEnergyDens_)
       NameUnitUserIdl_V(Ehot_) = NameIdlUnit_V(UnitEnergyDens_)
    end if

    call test_stop(NameSub, DoTest)
  end subroutine init_mhd_variables
  !============================================================================
  subroutine init_vector_variables

    ! Set number and indexes of vector variables

    use ModMain, ONLY: UseB
    use ModVarIndexes

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_vector_variables'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(nVectorVar == 0)then
       nVectorVar = nFluid
       if(UseB) nVectorVar = nVectorVar + 1
       allocate(iVectorVar_I(nVectorVar))
       iVectorVar_I(1:nFluid) = iRhoUx_I
       if(UseB) iVectorVar_I(nFluid+1) = Bx_
    end if

    call test_stop(NameSub, DoTest)
  end subroutine init_vector_variables
  !============================================================================
  subroutine update_angular_velocity

    ! Update the angular velocity OmegaBody_D

    use CON_axes,          ONLY: get_axes
    use ModMain,           ONLY: tSimulation, TypeCoordSystem

    real:: RotAxis_D(3) ! Avoid race condition in OpenMP

    character(len=*), parameter:: NameSub = 'update_angular_velocity'
    !--------------------------------------------------------------------------
    select case(TypeCoordSystem)
    case('HGI', 'hgi')
       ! In the HGI system the Solar angular velocity vector points towards +Z
       OmegaBody_D = [ 0., 0., OmegaBody ]
    case('GSE')
       call get_axes(tSimulation, RotAxisGseOut_D=RotAxis_D)
       OmegaBody_D = OmegaBody * RotAxis_D
    case('GSM')
       ! GSM system, Omega_D may be changing
       call get_axes(tSimulation, RotAxisGsmOut_D=RotAxis_D)
       OmegaBody_D = OmegaBody * RotAxis_D
    end select
    !$acc update device(OmegaBody_D)

  end subroutine update_angular_velocity
  !============================================================================
  subroutine set_dimensional_factor(nPlotVar, NameVar_V, &
       DimFactor_V, DimFactorBody_V)

    use ModVarIndexes, ONLY: DefaultState_V
    use ModUtilities,  ONLY: lower_case
    use ModMultiFluid, ONLY: extract_fluid_name

    integer, intent(in):: nPlotVar
    character(len=10), intent(in):: NameVar_V(nPlotVar)
    real,              intent(out):: DimFactor_V(nPlotVar)
    real, optional,    intent(out):: DimFactorBody_V(nPlotVar)

    character(len=10):: String, NamePlotVar

    integer:: iVar, jVar

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_dimensional_factor'
    !--------------------------------------------------------------------------
    DimFactor_V = 1.0
    if(present(DimFactorBody_V))DimFactorBody_V = 1.0
    do iVar=1,nPlotVar
       NamePlotVar = NameVar_V(iVar)
       call lower_case(NamePlotVar)
       String = NamePlotVar
       call extract_fluid_name(String)

       ! Set DimFactorBody_V to something reasonable for inside the body.
       ! Load zeros for most values - load something better for rho, p, and T
       ! We know that U,B,J are okay with zeroes, others should be changed if
       ! necessary.
       ! Note that all variables not set to 0 in set_plotvar should be
       ! loaded below. Note that this is used for tecplot corner extrapolation
       ! and for nothing else.

       select case(String)

          ! BASIC MHD variables

       case('rho')
          DimFactor_V(iVar) = No2Io_V(UnitRho_)
          if(present(DimFactorBody_V))&
               DimFactorBody_V(iVar) = No2Io_V(UnitRho_)
       case('rhoux','mx','rhouy','my','rhouz','mz','rhour','mr' )
          DimFactor_V(iVar) = No2Io_V(UnitRhoU_)
       case('bx','by','bz','br','b1x','b1y','b1z','b1r' &
            ,'bxl','bxr','byl','byr','bzl','bzr')
          DimFactor_V(iVar) = No2Io_V(UnitB_)
       case('elaser')
          DimFactor_V(iVar) = No2Io_V(UnitEnergyDens_)/No2Io_V(UnitT_)
       case('e','e1','ew','erad')
          DimFactor_V(iVar) = No2Io_V(UnitEnergyDens_)
       case('p','pth','pperp','peperp')
          DimFactor_V(iVar) =  No2Io_V(UnitP_)
          if(present(DimFactorBody_V))&
               DimFactorBody_V(iVar) = No2Io_V(UnitP_)
          ! EXTRA MHD variables
       case('n', 'qtot')
          ! Number and charge densities
          DimFactor_V(iVar) = No2Io_V(UnitN_)
       case('t','temp', 'te', 'ti')
          DimFactor_V(iVar) = &
               No2Io_V(UnitTemperature_)
       case('eta','visco')
          DimFactor_V(iVar) = &
               (No2Si_V(UnitX_)**2/No2Si_V(UnitT_))
       case('ux','uy','uz','uxrot','uyrot','uzrot','ur','clight')
          DimFactor_V(iVar) = No2Io_V(UnitU_)

       case('gradpex','gradpey','gradpez','gradper')
          DimFactor_V(iVar) = &
               No2Io_V(UnitP_)/No2Si_V(UnitX_)
       case('jx','jy','jz','jr',&
            'jxe','jye','jze','jxw','jyw','jzw', &
            'jxs','jys','jzs','jxn','jyn','jzn', &
            'jxb','jyb','jzb','jxt','jyt','jzt')
          DimFactor_V(iVar) = No2Io_V(UnitJ_)
       case('ex','ey','ez','er','enumx','enumy','enumz', &
            'expot','eypot','ezpot','exind','eyind','ezind')
          DimFactor_V(iVar) = No2Io_V(UnitElectric_)
       case('pote')
          ! Electric potential has SI units of V
          DimFactor_V(iVar) =  &
               No2Si_V(UnitElectric_)*No2Si_V(UnitX_)
       case('dive')
          ! Divergence of electric field has SI units of V/m^2
          DimFactor_V(iVar) =  &
               No2Si_V(UnitElectric_)/No2Si_V(UnitX_)
       case('pvecx','pvecy','pvecz','pvecr','b2ur')
          DimFactor_V(iVar) = No2Io_V(UnitPoynting_)
       case('divb','divb_cd','divb_ct','absdivb')
          DimFactor_V(iVar) = No2Io_V(UnitDivB_)

          ! GRID INFORMATION
       case('dt','dtblk')
          DimFactor_V(iVar) = No2Io_V(UnitT_)
       case('x','y','z','r','dx','dy','dz','req1','req2')
          DimFactor_V(iVar) = No2Io_V(UnitX_)

          ! DEFAULT CASE
       case default
          do jVar = 1, nVar
             if(NamePlotVar /= NameVarLower_V(jVar)) CYCLE
             DimFactor_V(iVar) = UnitUser_V(jVar)
             if(DefaultState_V(jVar) > 0.0 .and. present(DimFactorBody_V))&
                  DimFactorBody_V(iVar) = UnitUser_V(jVar)
             EXIT
          end do
          ! no normalization
       end select
    end do ! iVar

  end subroutine set_dimensional_factor
  !============================================================================
  subroutine set_second_body_coord

    use ModMain, ONLY:tSimulation, StartTime, TypeCoordSystem
    use CON_axes,      ONLY: transform_matrix
    use CON_planet, ONLY: orbit_in_hgi
    ! Second body location in HGI
    real:: XyzBody2Hgi_D(3), XyzBody2_D(3), vBody2Hgi_D(3)
    real:: Transform_DD(3,3)
    !--------------------------------------------------------------------------
    call orbit_in_hgi(StartTime + tSimulation,XyzBody2Hgi_D,vBody2Hgi_D)
    ! Convert to the coordinate system of the model, if needed
    Transform_DD = transform_matrix(TimeSim=tSimulation,&
         TypeCoordIn = 'HGI', TypeCoordOut = TypeCoordSystem)
    XyzBody2_D = matmul(Transform_DD, XyzBody2Hgi_D)*Si2No_V(UnitX_)
    xBody2 = XyzBody2_D(1)
    yBody2 = XyzBody2_D(2)
    zBody2 = XyzBody2_D(3)
    vBody2_D = matmul(Transform_DD, vBody2Hgi_D)*Si2No_V(UnitU_)
    !$acc update device(xBody2, yBody2, zBody2, vBody2_D)

  end subroutine set_second_body_coord
  !============================================================================
  subroutine set_radial_state(Xyz_D, State_V)

    use ModCoordTransform, ONLY: rot_xyz_rlonlat

    ! Set the radially dependent state for State_V at Xyz_D

    real, intent(in):: Xyz_D(3)
    real, intent(inout):: State_V(nVar)

    real:: r, XyzRlonlat_DD(3,3)
    integer:: iVar
    !--------------------------------------------------------------------------
    r = norm2(Xyz_D)
    XyzRlonlat_DD = rot_xyz_rlonlat(Xyz_D)
    do iVar = 1, nVar
       ! Skip Y and Z components of vector variables
       if(any(iVectorVar_I == iVar - 1)) CYCLE
       if(any(iVectorVar_I == iVar - 2)) CYCLE
       ! Check if iVar is the first component of vector variable
       if(any(iVectorVar_I == iVar))then
          if(all(AmplR_V(iVar:iVar+2) == 0.0)) CYCLE
          ! Vector variable: convert r,lon,lat to x,y,z:
          State_V(iVar:iVar+2) = matmul( XyzRlonlat_DD, &
               r**(ExponentR_V(iVar:iVar+2)) * AmplR_V(iVar:iVar+2) )
       else
          ! Scalar variable
          if(AmplR_V(iVar) == 0.0) CYCLE
          State_V(iVar) = r**ExponentR_V(iVar) * AmplR_V(iVar)
       end if
    end do

  end subroutine set_radial_state
  !============================================================================
end module ModPhysics
!==============================================================================
