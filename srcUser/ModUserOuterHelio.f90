!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModUser

  ! 04/30/2007 implementing MultiFluid
  ! 06/01/2007 correcting normalization
  ! 06/08/2007 correcting source terms
  ! 08/18/2007 Implementing 3-fluids
  ! 01/01/2008 Source terms in point-implicit form - with help of Gabor Toth
  ! 01/08/2008 all source terms for PopI implicit
  ! 01/25/2008 comments added
  ! 05/02/2008 setting initial conditions to be closer to the final solution
  ! 07/28/2008 neutral fluids
  ! 09/28/2008 source terms as McNutt
  ! 02/2011 PUI fluid
  ! 04/2011 Sources for PUI
  ! 07/2012 Use sonic Mach number to define Region 1 - Region 4 boundary
  ! 11/2017 use Solar Wind Pressure and Temp for the Mach Number for REGIONS
  !   definition for inner boundary the solar pressure is iostropic (don't have
  !   any dependence with magnetic field
  !   took off inner boundary at 30AU
  ! 02/08/2018 adding possibilty of using user boundaries for all faces
  ! 02/2018  Gabor - fixed code to work with separate multi-ion velocities
  !   Tests show that the default explicit evaluation works, while the
  !   point-implicit evaluation is not robust enough.
  ! 03/2018 Opher Merav fixed calc_source routine for multi-ion
  !   and charge neutrality while assuming cold electrons
  !   source terms can be printed in tecplot
  !   use of energy heating for PUI is optional
  ! 05/2018 refining select regions criteria for multi-ion
  ! 04/2020 Marc Kornbleuth removing total fluid; adding AMPS compatibility
  ! 07/2022 Merav Opher added conditions needed for Cold Cloud

  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest, iProc, nI, nJ, nK, &
       nBlock, MaxBlock, Unused_B, Used_GB, Xyz_DGB
  use ModMain, ONLY: &
       body1_, tSimulation, iStartTime_I
  use ModPhysics, ONLY: &
       Gamma, GammaMinus1, GammaElectronMinus1, InvGammaMinus1, &
       InvGammaElectronMinus1, GammaMinus1_I, InvGammaMinus1_I, &
       UnitX_, Si2No_V, No2Io_V, No2Si_V, Io2No_V, &
       NameTecUnit_V, NameIdlUnit_V, UnitEnergyDens_, &
       UnitN_, UnitRho_, UnitU_, rBody, UnitB_, UnitP_, &
       UnitTemperature_, UnitT_, UnitRhoU_
  use ModConst, ONLY: cAU, cProtonMass, cElectronMass, cBoltzmann, cEV, &
       cRyToEv, cSecondPerYear, iYearBase
  use ModTimeConvert, ONLY: n_day_of_year
  use ModNumConst, ONLY: cRadToDeg, cPi, cTwoPi
  use ModAdvance, ONLY: &
       State_VGB,Source_VC, ExtraSource_ICB, Pe_, UseElectronPressure
  use ModGeometry, ONLY: r_GB
  use ModVarIndexes
  use ModMultiFluid
  use ModCurrent, ONLY: get_current

  use ModUserEmpty,                                     &
       IMPLEMENTED1  => user_read_inputs,               &
       IMPLEMENTED2  => user_set_face_boundary,         &
       IMPLEMENTED4  => user_set_cell_boundary,         &
       IMPLEMENTED5  => user_set_ics,                   &
       IMPLEMENTED6  => user_initial_perturbation,      &
       IMPLEMENTED7  => user_update_states,             &
       IMPLEMENTED8  => user_action,                    &
       IMPLEMENTED10 => user_set_plot_var,              &
       IMPLEMENTED12 => user_calc_sources_expl,         &
       IMPLEMENTED14 => user_init_session,              &
       IMPLEMENTED15 => user_calc_sources_impl,         &
       IMPLEMENTED16 => user_init_point_implicit,       &
       IMPLEMENTED17 => user_set_boundary_cells

  include 'user_module.h' ! list of public methods

  ! Needed for coupling with PT through the C wrapper
  public:: get_collision
  public:: get_region
  public:: get_lat_dep_sw
  public:: get_photoionization_rate_si

  character (len=*), parameter :: NameUserFile = "ModUserOuterHelio.f90"
  character (len=*), parameter :: NameUserModule = "Outer Heliosphere"

  ! Named indexes for fluids
  integer, parameter :: SWH_ = 1, Ion_ = 1, &
       Pu3_ = nIonFluid, Neu_ = min(nFluid,nIonFluid+1), &
       Ne2_ = min(nFluid,nIonFluid+2), Ne3_ = min(nFluid,nIonFluid+3), &
       Ne4_= min(nFluid,nIonFluid+4)

  logical :: UseSource_I(SWH_:Ne4_) = .true.
  logical :: UsePhotoion = .false.
  logical :: UseColdCloud = .false.
  logical :: DoInitializeCloud = .false.
  logical :: UseSrcsInHelio = .true.
  logical :: DoFixChargeExchange = .true.
  logical :: UseElectronImpact = .false.

  ! from 173K use 0.7
  real :: HpLimit = 0.5

  real :: OmegaSun   = 0.0  ! normalized angular speed of Sun
  real :: ParkerTilt = 0.0  ! Bphi/Br at the equator at r=rBody

  ! Time-dependent variables
  ! Meant for allowing variable start time
  ! for time dependent run
  real :: SwLT_Start = 0.0
  real :: RunStart = 0.0
  real :: Offset = 0.0

  logical :: UsePu3Heating = .false.
  real :: TempPu3Si, FactorPu3, HeatPu3

  logical :: UseElectronHeating = .false.
  real :: PressureHeatElectronIo, FactorHeatElectron

  integer :: iTableSolarWind = -1 ! initialization is needed
  integer :: iTableMagIntensity = -1
  integer :: iTableChargeExchange = -1
  integer :: iTableElectronImpact = -1
  integer :: iTablePhotoionization = -1

  ! Needed for lookup table
  integer,parameter :: ChargeExchange_ = 1, ElectronImpact_ = 2

  ! SWH variables.
  real :: &
       SwhRho = 0.0, SwhRhoDim = 0.0, &
       SwhP   = 0.0, SwhTDim   = 0.0, &
       SwhPe  = 0.0, SwhTeDim  = 0.0, &
       SwhUx  = 0.0, SwhUxDim  = 0.0, &
       SwhUy  = 0.0, SwhUyDim  = 0.0, &
       SwhUz  = 0.0, SwhUzDim  = 0.0, &
       SwhBx  = 0.0, SwhBxDim  = 0.0, &
       SwhBy  = 0.0, SwhByDim  = 0.0, &
       SwhBz  = 0.0, SwhBzDim  = 0.0

  ! VLISM variables.
  real :: &
       VliswTDim  = 0.0 , VliswTeDim  = 0.0, &
       VliswRho   = 0.0 , VliswRhoDim = 0.0, &
       VliswP     = 0.0 , VliswPDim   = 0.0, &
       VliswPe    = 0.0 , VliswPeDim  = 0.0, &
       VliswUx    = 0.0 , VliswUxDim  = 0.0, &
       VliswUy    = 0.0 , VliswUyDim  = 0.0, &
       VliswUz    = 0.0 , VliswUzDim  = 0.0, &
       VliswBx    = 0.0 , VliswBxDim  = 0.0, &
       VliswBy    = 0.0 , VliswByDim  = 0.0, &
       VliswBz    = 0.0 , VliswBzDim  = 0.0

  ! Region Formulas
  character(len=20) :: NameRegionFormula
  integer, parameter:: SingleIon_ = 1, ColdCloud_ = 2
  integer, parameter:: MultiIon_=3, LevelSet_ = 4
  integer, parameter:: LevelSetMi_ = 5
  integer :: iRegionFormula = SingleIon_

  ! Cross Section Formulas
  character(len=20) :: NameCrossSection
  ! Default Cross Section is Lindsay Stebbings
  real :: CrossA1 = 2.2835E-7
  real :: CrossA2 = 1.062E-8

  ! Velocity, temperature, Mach number and radius limits for the populations
  real :: TempPop1LimitDim = 1e5    ! [K]
  real :: uPop1LimitDim    = 100.0  ! [km/s]
  real :: MachPop2Limit    = 0.9
  real :: MachPop3Limit    = 1.5
  real :: MachPop4Limit    = 2.0
  real :: rPop3Limit       = 50.0   ! [AU] it is all Pop3 out to rPop3Limit
  real :: MachPUIPop3      = 0.9
  real :: uPop3LimitDim    = 250.0  ! [km/s]
  real :: TempPop2LimitDim = 0.7e5  ! [K]
  real :: MachPop1Limit    = 1.0

  real :: RhoPop4LimitDim  = 0.01  ! for cold cloud

  ! Level Set criterion
  real :: LevelHPLimit = 0.0

  integer :: iFluidProduced_C(nI, nJ, nK)
  !$omp threadprivate( iFluidProduced_C )

  real :: &
       Pu3Rho = 0.0, Pu3RhoDim = 0.0, &
       Pu3P   = 0.0, Pu3TDim   = 0.0, &
       Pu3Ux  = 0.0, Pu3UxDim  = 0.0 , &
       Pu3Uy  = 0.0, Pu3UyDim  = 0.0 , &
       Pu3Uz  = 0.0, Pu3UzDim  = 0.0

  ! Freeze neutrals (with user_update_states)
  logical, parameter:: DoFreezeNeutral=.false.

  ! Wave turbulence
  real :: DeltaUDim = 10.0, DeltaU = 0.0
  real :: CrossHelicity = -0.8
  real :: LperpTimesSqrtBSi = 1.5e5, LperpTimesSqrtB = 0.0
  real :: TurbulencePerPu3Source = 0.25

  ! Photoionization rate at 1AU (unit is 1/s)
  ! Below is the rate of Fahr et al.
  real :: PhotoionizationRate = 8e-8

  ! Ionization energy for neutral atomic hydrogen
  ! Needed for electron impact ionization
  real :: IonizationEnergyDim = cRyToEv*cEv
  real :: IonizationEnergy = 0

  logical :: UseSingleIonVelocityRegion3 = .false.

  ! Number of subsample points of neutral distribution for PUI source terms
  integer :: nSubSample = 5

  logical :: UsePuiCxHeliosheath = .true.

contains
  !============================================================================
  subroutine user_read_inputs

    use ModReadParam

    character (len=100) :: NameCommand

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
       case("#SOLARWINDH")
          call read_var('SwhRhoDim',SwhRhoDim)
          call read_var('SwhTDim'  ,SwhTDim)
          call read_var('SwhUxDim' ,SwhUxDim)
          call read_var('SwhUyDim' ,SwhUyDim)
          call read_var('SwhUzDim' ,SwhUzDim)
          call read_var('SwhBxDim' ,SwhBxDim)
          call read_var('SwhByDim' ,SwhByDim)
          call read_var('SwhBzDim' ,SwhBzDim)
          if(UseElectronPressure)then
             call read_var('SwhTeDim'  ,SwhTeDim)
          end if
       case("#GLOBALHELIOSPHERE")
          call read_var('Rbody',Rbody)
       case("#VLISW")
          call read_var('VliswRhoDim' ,VliswRhoDim)
          call read_var('VliswTDim'  ,VliswTDim)
          call read_var('VliswUxDim' ,VliswUxDim)
          call read_var('VliswUyDim' ,VliswUyDim)
          call read_var('VliswUzDim' ,VliswUzDim)
          call read_var('VliswBxDim' ,VliswBxDim)
          call read_var('VliswByDim' ,VliswByDim)
          call read_var('VliswBzDim' ,VliswBzDim)
          if(UseElectronPressure)then
             call read_var('VliswTeDim' ,VliswTeDim)
          end if
       case("#PICKUPION3")
          call read_var('Pu3RhoDim',Pu3RhoDim)
          call read_var('Pu3TDim'  ,Pu3TDim)
          call read_var('Pu3UxDim' ,Pu3UxDim)
          call read_var('Pu3UyDim' ,Pu3UyDim)
          call read_var('Pu3UzDim' ,Pu3UzDim)

       case("#PU3HEATING")
          call read_var('UsePu3Heating', UsePu3Heating)
          call read_var('TempPu3Si', TempPu3Si)
          call read_var('FactorPu3', FactorPu3)
          ! convert from 1/[au yr] to 1/[au s]
          FactorPu3 = FactorPu3/cSecondPerYear

       case("#ELECTRONHEATING")
          call read_var('UseElectronHeating', UseElectronHeating)
          call read_var('PressureHeatElectronIo', PressureHeatElectronIo)
          call read_var('FactorHeatElectron', FactorHeatElectron)
          ! convert from 1/[au yr] to 1/[au s]
          FactorHeatElectron = FactorHeatElectron/cSecondPerYear

          ! This is a flag to define how many populations of Neutrals to run
       case("#SOURCES")
          call read_var('UseSWHSource', UseSource_I(SWH_))
          if(.not.IsMhd) call read_var('UsePu3Source', UseSource_I(Pu3_))
          call read_var('UseNeuSource', UseSource_I(Neu_))
          call read_var('UseNe2Source', UseSource_I(Ne2_))
          call read_var('UseNe3Source', UseSource_I(Ne3_))
          call read_var('UseNe4Source', UseSource_I(Ne4_))

       case("#PHOTOIONIZATION")
          ! Chika. This is a flag to turn on photoionization
          ! If not specified in PARAM.in, it will be false
          call read_var('UsePhotoionization', UsePhotoion)
          if(UsePhotoion) &
               call read_var('PhotoionizationRate', PhotoionizationRate)

       case("#COLDCLOUD")
          call read_var('UseColdCloud', UseColdCloud)
          call read_var('DoInitializeCloud', DoInitializeCloud)
          call read_var('UseSrcsInHelio', UseSrcsInHelio)
          call read_var('HpLimit', HpLimit)

       case("#CHARGEEXCHANGE")
          ! Bair. This is a flag to use charge exchange formula
          ! with the extra splitting terms.
          call read_var("DoFixChargeExchange", DoFixChargeExchange)

       case("#ELECTRONIMPACT")
          call read_var("UseElectronImpact", UseElectronImpact)

       case("#CROSSSECTION")
          call read_var('NameCrossSection', NameCrossSection)
          select case(NameCrossSection)

          case('LindsayStebbings')
             CrossA1 = 2.2835E-7
             CrossA2 = 1.062E-8

          case('MaherTinsley')
             CrossA1 = 1.64E-7
             CrossA2 = 6.95E-9

          case('UserCrossSection')
             call read_var('a1', CrossA1)
             call read_var('a2', CrossA2)

          case default
             call stop_mpi(NameSub//': unknown NameCrossSection = ' &
                  // NameCrossSection)
          end select

       case("#REGIONS")
          call read_var('NameRegionFormula', NameRegionFormula)
          select case(NameRegionFormula)

          case('SingleIon')
             iRegionFormula = SingleIon_
             call read_var('TempPop1LimitDim', TempPop1LimitDim)
             call read_var('uPop1LimitDim',    uPop1LimitDim)
             call read_var('MachPop2Limit',    MachPop2Limit)
             call read_var('MachPop3Limit',    MachPop3Limit)
             call read_var('rPop3Limit',       rPop3Limit)
             call read_var('MachPop4Limit',    MachPop4Limit)
             if(.not.IsMhd) call read_var('MachPUIPop3',   MachPUIPop3)

          case('ColdCloud')
             iRegionFormula = ColdCloud_
             call read_var('TempPop1LimitDim', TempPop1LimitDim)
             call read_var('uPopLimitDim',     uPop1LimitDim)
             call read_var('rPop3Limit',       rPop3Limit)
             call read_var('RhoPop4LimitDim',  RhoPop4LimitDim)

          case('MultiIon')
             iRegionFormula = MultiIon_
             call read_var('uPop3LimitDim', uPop3LimitDim)
             call read_var('MachPop3Limit', MachPop3Limit)
             call read_var('TempPop2LimitDim', TempPop2LimitDim)
             call read_var('uPop1LimitDim', uPop1LimitDim)
             call read_var('MachPop1Limit', MachPop1Limit)

          case('LevelSet')
             iRegionFormula = LevelSet_
             call read_var('LevelHPLimit',     LevelHPLimit)
             call read_var('MachPop2Limit',    MachPop2Limit)
             call read_var('MachPop4Limit',    MachPop4Limit)

          case('LevelSetMi')
             iRegionFormula = LevelSetMi_
             call read_var('LevelHPLimit',     LevelHPLimit)
             call read_var('uPop3LimitDim',    uPop3LimitDim)
             call read_var('MachPop4Limit',    MachPop4Limit)

          case default
             call stop_mpi(NameSub//': unknown NameRegionFormula = ' &
                  // NameRegionFormula)
          end select

       case("#TURBULENCE")
          call read_var('DeltaUDim', DeltaUDim)
          call read_var('CrossHelicity', CrossHelicity)
          call read_var('LperpTimesSqrtBSi', LperpTimesSqrtBSi)
          call read_var('TurbulencePerPu3Source', TurbulencePerPu3Source)

       case("#SINGLEIONVELOCITYREGION3")
          call read_var('UseSingleIonVelocityRegion3', &
               UseSingleIonVelocityRegion3)

       case default
          if(iProc==0) call stop_mpi( &
               'read_inputs: unrecognized command: '//NameCommand)
       end select
    end do

    call test_stop(NameSub, DoTest)
  end subroutine user_read_inputs
  !============================================================================
  subroutine user_set_face_boundary(FBC)

    use ModMain, ONLY: FaceBCType
    use ModCoordTransform, ONLY: rot_xyz_sph
    use ModWaves, ONLY: UseAlfvenWaves
    use ModTurbulence, ONLY: SigmaD
#ifdef _OPENACC
    use ModUtilities, ONLY: norm2
#endif
    use ModPUI, ONLY: set_pui_state

    type(FaceBCType), intent(inout):: FBC

    ! local variables
    real:: xFace, yFace, zFace
    real:: SinTheta
    ! C.P. edited
    real:: Bsph_D(3), Vsph_D(3), VPUIsph_D(3)

    real :: pSolarWind,pPUI, Pmag, PmagEquator, Ewave, Rho

    real :: XyzSph_DD(3,3)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_face_boundary'
    !--------------------------------------------------------------------------
    associate( VarsGhostFace_V => FBC%VarsGhostFace_V, &
         VarsTrueFace_V => FBC%VarsTrueFace_V, &
         FaceCoords_D => FBC%FaceCoords_D, &
         iBlockBc => FBC%iBlockBc, &
         iFace => FBC%iFace, jFace => FBC%jFace, kFace => FBC%kFace, &
         iBoundary => FBC%iBoundary )

      call test_start(NameSub, DoTest, iBlockBc)
      if(iBoundary /= body1_) &
           call stop_mpi(NameSub//' only inner BC is implemented!')

      ! Make sure that OmegaSun and ParkerTilt are set
      if(OmegaSun == 0.0) call set_omega_parker_tilt

      XyzSph_DD = rot_xyz_sph(FaceCoords_D)

      xFace = FaceCoords_D(1)
      yFace = FaceCoords_D(2)
      zFace = FaceCoords_D(3)

      SinTheta = sqrt((xFace**2 + YFace**2)/(xFace**2 + yFace**2 + zFace**2))

      Bsph_D(1) =  SwhBx                     ! Br
      Bsph_D(2) =  0.0                       ! Btheta
      Bsph_D(3) =  SwhBx*SinTheta*ParkerTilt

      Vsph_D    = [ SwhUx, 0.0, 0.0 ]        ! Vr, Vtheta, Vphi
      VPUIsph_D = [ Pu3Ux, 0.0, 0.0 ]

      ! Calculate pressur:e (equilibrium at a given inner boundary)
      Pmag = sum(Bsph_D**2) / 2.0

      ! magnetic pressure at the equator (actually wrong, neglects Br=SwhBx)
      PmagEquator = (SwhBx*ParkerTilt)**2/2

      ! this method guarantees equal pressure at inner boundary.
      ! Simpler splitting method likely

      if(.not.IsMhd) pPUI = Pu3P

      pSolarWind = SwhP

      ! Apply boundary conditions for ions
      VarsGhostFace_V(SWHRho_)    = SwhRho
      VarsGhostFace_V(SWHP_)      = pSolarWind ! SwhP
      VarsGhostFace_V(SWHUx_:SWHUz_) = matmul(XyzSph_DD, Vsph_D)
      VarsGhostFace_V(Bx_:Bz_) = matmul(XyzSph_DD, Bsph_D)

      VarsGhostFace_V(LevelHP_) = SwhRho

      if(UseElectronPressure) VarsGhostFace_V(Pe_) = SwhPe

      if(.not.IsMhd)then
         VarsGhostFace_V(Pu3Rho_)    = Pu3Rho
         VarsGhostFace_V(Pu3P_)      = pPUI  ! Pu3P ???
         VarsGhostFace_V(Pu3Ux_:Pu3Uz_) = matmul(XyzSph_DD, VPUIsph_D)
         VarsGhostFace_V(LevelHP_) = SwhRho + Pu3Rho
      end if

      if(PuiFirst_ > 1) call set_pui_state(VarsGhostFace_V)

      if(UseAlfvenWaves)then
         Rho = VarsGhostFace_V(Rho_)

         Ewave = Rho*DeltaU**2/(1.0 +  SigmaD)
         if(Bsph_D(1) > 0.0)then
            VarsGhostFace_V(WaveFirst_) = Ewave*0.5*(1.0 - CrossHelicity)
            VarsGhostFace_V(WaveLast_)  = Ewave*0.5*(1.0 + CrossHelicity)
         else
            VarsGhostFace_V(WaveFirst_) = Ewave*0.5*(1.0 + CrossHelicity)
            VarsGhostFace_V(WaveLast_)  = Ewave*0.5*(1.0 - CrossHelicity)
         end if

         if(Lperp_ > 1) VarsGhostFace_V(Lperp_) = &
              Rho*LperpTimesSqrtB/sqrt(norm2(VarsGhostFace_V(Bx_:Bz_)))
      end if

      if(UseNeutralFluid)then

         ! NeuRho is PopI; NeuIIRho is PopII and NeuIIIRho is PopIII
         !
         ! Pop I is going through the inner BCs

         ! soft boundary for Pop I-IV

         VarsGhostFace_V(NeuRho_:Ne4P_) = VarsTrueFace_V(NeuRho_:Ne4P_)

         ! PopII leaves the domain at a supersonic velocity
         ! (50km/s while for their temperature 1.E5K their C_s=30km/s)
         ! For the transient case when it flows inward,
         ! we use a fraction of ions

         if( sum(VarsTrueFace_V(Ne2Ux_:Ne2Uz_)*FaceCoords_D) > 0.0)then
            VarsGhostFace_V(Ne2Rho_) = &
                 VarsGhostFace_V(Rho_) * RhoBcFactor_I(Ne2_)
            VarsGhostFace_V(Ne2P_)   = &
                 VarsGhostFace_V(p_)   * RhoBcFactor_I(Ne2_)
            VarsGhostFace_V(Ne2Ux_:Ne2Uz_) = &
                 VarsGhostFace_V(Ux_:Uz_) * uBcFactor_I(Ne2_)
         else
            VarsGhostFace_V(Ne2Rho_:Ne2P_) = VarsTrueFace_V(Ne2Rho_:Ne2P_)
         end if

         ! Pop III has the velocity and temperature of ions at inner boundary
         ! the density is taken to be a fraction of the ions

         if( sum(VarsTrueFace_V(Ne3Ux_:Ne3Uz_)*FaceCoords_D) > 0.0)then
            VarsGhostFace_V(Ne3Rho_) = &
                 VarsGhostFace_V(Rho_) * RhoBcFactor_I(Ne3_)
            VarsGhostFace_V(Ne3P_)   = &
                 VarsGhostFace_V(p_)   * RhoBcFactor_I(Ne3_)
            VarsGhostFace_V(Ne3Ux_:Ne3Uz_) = &
                 VarsGhostFace_V(Ux_:Uz_) * uBcFactor_I(Ne3_)
         else
            VarsGhostFace_V(Ne3Rho_:Ne3P_) = VarsTrueFace_V(Ne3Rho_:Ne3P_)
         end if

         ! Pop IV

         if( sum(VarsTrueFace_V(Ne4Ux_:Ne4Uz_)*FaceCoords_D) > 0.0)then
            VarsGhostFace_V(Ne4Rho_) = &
                 VarsGhostFace_V(Rho_) * RhoBcFactor_I(Ne4_)
            VarsGhostFace_V(Ne4P_)   = &
                 VarsGhostFace_V(p_)   * RhoBcFactor_I(Ne4_)
            VarsGhostFace_V(Ne4Ux_:Ne4Uz_) = &
                 VarsGhostFace_V(Ux_:Uz_) * uBcFactor_I(Ne4_)
         else
            VarsGhostFace_V(Ne4Rho_:Ne4P_) = VarsTrueFace_V(Ne4Rho_:Ne4P_)
         end if
      endif

      if(DoTest)then
         write(*,*) NameSub,' FaceCoord=', FaceCoords_D
         write(*,*) NameSub,' i,j,kFace=', iFace, jFace, kFace
         write(*,*) NameSub,' SinTheta =', SinTheta
         write(*,*) NameSub,' Bsph_D   =', Bsph_D
         write(*,*) NameSub,' Vsph_D   =', Vsph_D
         write(*,*) NameSub,' Pmag, Peq=', Pmag, PmagEquator
         write(*,*) NameSub,' SWHRhoGhost =', VarsGhostFace_V(SWHRho_)
         write(*,*) NameSub,' SWHpGhost   =', VarsGhostFace_V(SWHP_)
         write(*,*) NameSub,' SWHUghost   =', VarsGhostFace_V(SWHUx_:SWHUz_)
         write(*,*) NameSub,' Bghost   =', VarsGhostFace_V(Bx_:Bz_)
         if(UseNeutralFluid)then
            write(*,*) NameSub,' Pop1     =', VarsGhostFace_V(NeuRho_:NeuP_)
            write(*,*) NameSub,' Pop2     =', VarsGhostFace_V(Ne2Rho_:Ne2P_)
            write(*,*) NameSub,' Pop3     =', VarsGhostFace_V(Ne3Rho_:Ne3P_)
            write(*,*) NameSub,' Pop4     =', VarsGhostFace_V(Ne4Rho_:Ne4P_)
         end if
         if(.not.IsMhd)then
            write(*,*) NameSub,' VPUIsph_D=', VPUIsph_D
            write(*,*) NameSub,' Pui3     =', VarsGhostFace_V(Pu3Rho_:Pu3P_)
            write(*,*) NameSub,' pPUI, PU3_p, pSolarwind, SwhP =', &
                 pPUI, Pu3P, pSolarwind, SwhP
         end if
         if(UseElectronPressure)then
            write(*,*) NameSub,' Pe     =', SwhPe
         end if
      end if

    end associate

    call test_stop(NameSub, DoTest)
  end subroutine user_set_face_boundary
  !============================================================================
  subroutine user_set_cell_boundary(iBlock, iSide, TypeBc, IsFound)

    use ModCellBoundary, ONLY: iMin, iMax, jMin, jMax, kMin, kMax
    use ModTurbulence, ONLY: KarmanTaylorAlpha
    use ModWaves, ONLY: UseAlfvenWaves
    use ModPUI, ONLY: set_pui_state

    ! The ISM enters at the east boundary (negative x)
    ! February 08, 2018 - added the possibility for using user conditions in
    ! other boundaries as well

    integer,      intent(in):: iBlock, iSide
    character(len=*), intent(in)  :: TypeBc
    logical,      intent(out):: IsFound

    integer :: iVar, i, j, k
    real :: Rho

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_cell_boundary'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    IsFound = .true.

    do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
       State_VGB(SWHRho_,i,j,k,iBlock)=VliswRho
       State_VGB(LevelHP_,i,j,k,iBlock)=-VliswRho
       State_VGB(SWHRhoUx_,i,j,k,iBlock)=VliswRho*VliswUx
       State_VGB(SWHRhoUy_,i,j,k,iBlock)=VliswRho*VliswUy
       State_VGB(SWHRhoUz_,i,j,k,iBlock)=VliswRho*VliswUz
       State_VGB(Bx_,i,j,k,iBlock)=VliswBx
       State_VGB(By_,i,j,k,iBlock)=VliswBy
       State_VGB(Bz_,i,j,k,iBlock)=VliswBz
       State_VGB(SWHP_,i,j,k,iBlock)=VliswP
       if(UseElectronPressure) State_VGB(Pe_,i,j,k,iBlock)=VliswPe

       if(.not.IsMhd)then
          State_VGB(Pu3Rho_,  i,j,k,iBlock) = 1E-5*VliswRho
          State_VGB(Pu3RhoUx_,i,j,k,iBlock) = 1E-5*VliswRho*VliswUx
          State_VGB(Pu3RhoUy_,i,j,k,iBlock) = 1E-5*VliswRho*VliswUy
          State_VGB(Pu3RhoUz_,i,j,k,iBlock) = 1E-5*VliswRho*VliswUz
          State_VGB(Pu3P_,    i,j,k,iBlock) = 1E-5*VliswP
       end if

       if(PuiFirst_ > 1) call set_pui_state(State_VGB(:,i,j,k,iBlock))

       if(UseAlfvenWaves)then
          State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock) = 0.0

          ! Set Lperp to a large value of 1 AU in ISW
          if(Lperp_ > 1) &
               State_VGB(Lperp_,i,j,k,iBlock) = &
               State_VGB(Rho_,i,j,k,iBlock)/KarmanTaylorAlpha
       end if
    end do; end do; end do

    if(UseNeutralFluid)then

       ! PopIV is the one one coming with the ISW
       ! The separation between Pop IV and Pop I is arbitrary so
       ! we took the separation as Vlad in x=-1500AU

       do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
          State_VGB(Ne4Rho_,i,j,k,iBlock)   = RhoNeutralsISW
          State_VGB(Ne4RhoUx_,i,j,k,iBlock) = RhoNeutralsISW*UxNeutralsISW
          State_VGB(Ne4RhoUy_,i,j,k,iBlock) = RhoNeutralsISW*UyNeutralsISW
          State_VGB(Ne4RhoUz_,i,j,k,iBlock) = RhoNeutralsISW*UzNeutralsISW
          State_VGB(Ne4P_,i,j,k,iBlock)     = PNeutralsISW
       end do; end do; end do

       ! In general you should specify as many values as many incoming
       ! characteristic waves are present. For a neutral fluid this
       ! is 0 for supersonic outflow, 1 for subsonic outflow,
       ! 4 for subsonic inflow and 5 for supersonic inflow.

       ! PopII and III supersonic outflow
       do iVar = NeuRho_, Ne3P_
          select case(iSide)
          case(1)
             do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
                State_VGB(iVar,i,j,k,iBlock) = State_VGB(iVar,1,j,k,iBlock)
             end do; end do; end do
          case(2)
             do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
                State_VGB(iVar,i,j,k,iBlock) = State_VGB(iVar,nI,j,k,iBlock)
             end do; end do; end do
          case(3)
             do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
                State_VGB(iVar,i,j,k,iBlock) = State_VGB(iVar,i,1,k,iBlock)
             end do; end do; end do
          case(4)
             do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
                State_VGB(iVar,i,j,k,iBlock) = State_VGB(iVar,i,nJ,k,iBlock)
             end do; end do; end do
          case(5)
             do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
                State_VGB(iVar,i,j,k,iBlock) = State_VGB(iVar,i,j,1,iBlock)
             end do; end do; end do
          case(6)
             do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
                State_VGB(iVar,i,j,k,iBlock) = State_VGB(iVar,i,j,nK,iBlock)
             end do; end do; end do
          end select
       end do
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_cell_boundary
  !============================================================================
  subroutine user_set_ics(iBlock)

    use ModCoordTransform, ONLY: rot_xyz_sph
    use ModWaves, ONLY: UseAlfvenWaves
    use ModTurbulence, ONLY: SigmaD, KarmanTaylorAlpha
#ifdef _OPENACC
    use ModUtilities, ONLY: norm2
#endif
    use ModPUI, ONLY: set_pui_state

    integer, intent(in) :: iBlock

    integer :: i,j,k, iP
    real :: x, y, z, r
    real :: b_D(3), v_D(3), bSph_D(3), vSph_D(3), vPUI_D(3), vPUISph_D(3)
    real :: SinTheta, SignZ
    real :: Ewave, Rho, RhoBody, GammaTmp
    real :: XyzSph_DD(3,3) ! rotation matrix Xyz_D = matmul(XyzSph_DD, Sph_D)

    logical :: DoTestCell
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_ics'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! Make sure that OmegaSun and ParkerTilt are set
    if(OmegaSun == 0.0)call set_omega_parker_tilt

    do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
       if(.not. Used_GB(i,j,k,iBlock)) CYCLE

       DoTestCell = DoTest .and. i==iTest .and. j==jTest .and. k==kTest

       x = Xyz_DGB(x_,i,j,k,iBlock)
       y = Xyz_DGB(y_,i,j,k,iBlock)
       z = Xyz_DGB(z_,i,j,k,iBlock)
       r = r_GB(i,j,k,iBlock)

       XyzSph_DD = rot_xyz_sph(x,y,z)

       ! theta is the angle measured from the pole
       SinTheta = sqrt(x**2+y**2)/r

       ! calculating the Parker B field spherical components Bsph_D

       ! good for polarity of 1997       SignZ = sign(1.0, z)
       SignZ = sign(1.0,z)  ! good for 2005

       ! Bsph_D(1) = SignZ*SwhBx*(rBody/r)**2  ! Br
       ! Bsph_D(2) = 0.0                        ! Btheta
       ! Bsph_D(3) = -SignZ*SwhBx*SinTheta*ParkerTilt*(rBody/r) ! Bphi

       ! monopole
       Bsph_D(1) = SwhBx*(rBody/r)**2  ! Br
       Bsph_D(2) = 0.0                        ! Btheta
       Bsph_D(3) = SwhBx*SinTheta*ParkerTilt*(rBody/r) ! Bphi

       Vsph_D = [ SwhUx, 0., 0. ]
       vPUISph_D = [ Pu3Ux, 0., 0. ]

       ! magnetic field components in cartesian coordinates
       b_D = matmul(XyzSph_DD, Bsph_D)

       State_VGB(Bx_:Bz_,i,j,k,iBlock) = b_D

       ! velocity components in cartesian coordinates
       v_D    = matmul(XyzSph_DD, Vsph_D)
       vPUI_D = matmul(XyzSph_DD, VPUIsph_D)

       ! density and pressure
       State_VGB(SWHRho_,i,j,k,iBlock) = SwhRho * (rBody/r)**2
       State_VGB(LevelHP_,i,j,k,iBlock) = State_VGB(SWHRho_,i,j,k,iBlock)
       State_VGB(SWHP_,i,j,k,iBlock) = SwhP   * (rBody/r)**2
       State_VGB(SWHRhoUx_:SWHRhoUz_,i,j,k,iBlock) = &
            State_VGB(SWHRho_,i,j,k,iBlock)*v_D

       if(UseElectronPressure) &
            State_VGB(Pe_,i,j,k,iBlock) = SwhPe * (rBody/r)**2

       if(UseColdCloud .and. r > rBody .or. r > 100.0) then
          State_VGB(Bx_,i,j,k,iBlock) =  VliswBx
          State_VGB(By_,i,j,k,iBlock) =  VliswBy
          State_VGB(Bz_,i,j,k,iBlock) =  VliswBz

          State_VGB(Rho_,i,j,k,iBlock) = VliswRho
          State_VGB(LevelHP_,i,j,k,iBlock) = -VliswRho
          State_VGB(P_,i,j,k,iBlock)   = VliswP
          if(UseElectronPressure) State_VGB(Pe_,i,j,k,iBlock) = VliswP

          State_VGB(RhoUx_,i,j,k,iBlock) = VliswUx*VliswRho
          State_VGB(RhoUy_,i,j,k,iBlock) = VliswUy*VliswRho
          State_VGB(RhoUz_,i,j,k,iBlock) = VliswUz*VliswRho
       end if

       if(UseNeutralFluid)then
          if(UseColdCloud)then
             ! PopI for studies of Heliosphere in cold cloud
             State_VGB(NeuRho_,i,j,k,iBlock)   = VliswRho
             State_VGB(NeuP_,i,j,k,iBlock)     = VliswP
             State_VGB(NeuRhoUx_,i,j,k,iBlock) = VliswUx*VliswRho
             State_VGB(NeuRhoUy_,i,j,k,iBlock) = VliswUy*VliswRho
             State_VGB(NeuRhoUz_,i,j,k,iBlock) = VliswUz*VliswRho
          else
             ! PopI
             ! Set to small fraction of incoming ISW density and pressure
             State_VGB(NeuRho_,i,j,k,iBlock)   = 0.01*RhoNeutralsISW
             State_VGB(NeuP_,i,j,k,iBlock)     = 0.01*PNeutralsISW
             State_VGB(NeuRhoUx_,i,j,k,iBlock) = 0.01*RhoNeutralsISW &
                  * UxNeutralsISW
             State_VGB(NeuRhoUy_,i,j,k,iBlock) = 0.01*RhoNeutralsISW &
                  * UyNeutralsISW
             State_VGB(NeuRhoUz_,i,j,k,iBlock) = 0.01*RhoNeutralsISW &
                  * UzNeutralsISW
          endif
          !! This profile takes into account loss due to PUI
          !! State_VGB(NeuRho_,i,j,k,iBlock) = &
          !!    RhoNeutralsISW*Exp(-lambda*thetaN/((r**2)*SinThetaN)) &
          !!    *Exp(-lambda*thetaN/(r*SinThetaN))
          !! State_VGB(NeuP_,i,j,k,iBlock) = &
          !!    PNeutralsISW*Exp(-lambda*thetaN/(r*SinThetaN))

          ! PopII - I set it to be about 100km/s radially.
          ! The temperature is set to be 10^5K for this population.
          ! v_D is the plasma velocity, we take one quarter of that.
          State_VGB(Ne2Rho_,i,j,k,iBlock) = 1.e-3 * RhoNeutralsISW
          State_VGB(Ne2P_,i,j,k,iBlock)   = 1.e-3 * PNeutralsISW
          State_VGB(Ne2RhoUx_:Ne2RhoUz_,i,j,k,iBlock) = &
               0.25*State_VGB(Ne2Rho_,i,j,k,iBlock)*v_D
          ! PopIII
          ! Marc Kornbleuth modified to use SWH pop for Pop 3
          State_VGB(Ne3Rho_,i,j,k,iBlock) = 0.1*State_VGB(SWHRho_,i,j,k,iBlock)
          State_VGB(Ne3P_,i,j,k,iBlock)   = 0.1*State_VGB(SWHP_,i,j,k,iBlock)
          State_VGB(Ne3RhoUx_:Ne3RhoUz_,i,j,k,iBlock) = &
               State_VGB(Ne3Rho_,i,j,k,iBlock)*v_D

          ! PopIV
          State_VGB(Ne4Rho_,i,j,k,iBlock)  =RhoNeutralsISW
          State_VGB(Ne4RhoUx_,i,j,k,iBlock)=RhoNeutralsISW*UxNeutralsISW
          State_VGB(Ne4RhoUy_,i,j,k,iBlock)=RhoNeutralsISW*UyNeutralsISW
          State_VGB(Ne4RhoUz_,i,j,k,iBlock)=RhoNeutralsISW*UzNeutralsISW
          State_VGB(Ne4P_,i,j,k,iBlock) = PNeutralsISW
       end if

       ! If filling in with realistic PUI profile must do
       ! after neutrals populated
       ! neglect photoionization at >30 AU, ~10% effect
       ! must be a-dimensional
       ! start with PUI's only to 100 AU
       ! if (r <= rPop3Limit) then

       if(.not.IsMhd)then
          if(r < rBody)then
             State_VGB(Pu3Rho_,i,j,k,iBlock) = Pu3Rho
             State_VGB(Pu3P_,i,j,k,iBlock)   = Pu3P
          else
             ! No production yet
             State_VGB(Pu3Rho_,i,j,k,iBlock) = Pu3Rho * (rBody/r)**2
             State_VGB(Pu3P_,i,j,k,iBlock)   = Pu3P   * (rBody/r)**2
          end if
          State_VGB(Pu3RhoUx_:Pu3RhoUz_,i,j,k,iBlock) = &
               State_VGB(Pu3Rho_,i,j,k,iBlock)*vPUI_D
          if(UseColdCloud .and. r < rBody .or. r < 100.0) &
               State_VGB(LevelHP_,i,j,k,iBlock) = &
               State_VGB(SWHRho_,i,j,k,iBlock) &
               + State_VGB(Pu3Rho_,i,j,k,iBlock)
       end if

       if(PuiFirst_ > 1) call set_pui_state(State_VGB(:,i,j,k,iBlock))

       if(UseAlfvenWaves)then
          if(r>100)then
             State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock) = 0.0
             if(Lperp_ > 1) State_VGB(Lperp_,i,j,k,iBlock) = &
                  VliswRho/KarmanTaylorAlpha
          else
             Rho = State_VGB(Rho_,i,j,k,iBlock)
             RhoBody = SwhRho

             Ewave = Rho*DeltaU**2/(1.0 + SigmaD)*sqrt(Rho/RhoBody)

             if(sum(State_VGB(Bx_:Bz_,i,j,k,iBlock) &
                  * Xyz_DGB(x_:z_,i,j,k,iBlock))>0.0)then
                ! Positive propagating waves
                State_VGB(WaveFirst_,i,j,k,iBlock) = &
                     Ewave*0.5*(1.0-CrossHelicity)
                State_VGB(WaveLast_,i,j,k,iBlock)  = &
                     Ewave*0.5*(1.0+CrossHelicity)
             else
                ! Negative propagating waves
                State_VGB(WaveFirst_,i,j,k,iBlock) = &
                     Ewave*0.5*(1.0+CrossHelicity)
	        State_VGB(WaveLast_,i,j,k,iBlock)  = &
                     Ewave*0.5*(1.0-CrossHelicity)
             end if

             if(Lperp_ > 1) State_VGB(Lperp_,i,j,k,iBlock) = &
                  Rho*LperpTimesSqrtB/sqrt(norm2(B_D))
          end if
       end if

       if(DoTestCell)then
          write(*,*)NameSub,' x, y, z, r            =', x, y, z, r
          write(*,*)NameSub,' SignZ, SwhBx, SinTheta=', SignZ, SwhBx, SinTheta
          write(*,*)NameSub,' OmegaSun, rBody, SwhUx=', OmegaSun,rBody,SwhUx
          write(*,*)NameSub,' Vsph_D                =', Vsph_D
          write(*,*)NameSub,' v_D                   =', v_D
          write(*,*)NameSub,' Bsph_D                =', Bsph_D
          write(*,*)NameSub,' b_D                   =', &
               State_VGB(Bx_:Bz_,i,j,k,iBlock)
          write(*,*)NameSub,' SWHRhoU_D =', &
               State_VGB(SWHRhoUx_:SWHRhoUz_,i,j,k,iBlock)
          write(*,*)NameSub,' SWHRho,   =', State_VGB(SWHRho_,i,j,k,iBlock)
          write(*,*)NameSub,' SWHp      =', State_VGB(SWHP_,i,j,k,iBlock)
          if(.not.IsMhd)then
             write(*,*)NameSub,' VPUIsph_D =', VPUIsph_D
             write(*,*)NameSub,' vPUI_D    =', vPUI_D
             write(*,*)NameSub,' Pu3Rho,   =', State_VGB(Pu3Rho_,i,j,k,iBlock)
             write(*,*)NameSub,' Pu3P      =', State_VGB(Pu3P_,i,j,k,iBlock)
          end if
          if(UseElectronPressure) &
               write(*,*)NameSub,' Pe        =', State_VGB(Pe_,i,j,k,iBlock)
       end if

    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_ics
  !============================================================================
  subroutine user_initial_perturbation

    ! Set Pop II to be the same state as the ions with smaller density
    integer:: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_initial_perturbation'
    !--------------------------------------------------------------------------
    if(.not.UseNeutralFluid) &
         call CON_stop(NameSub//': no neutral fluids present')

    call test_start(NameSub, DoTest)
    do iBlock = 1, nBlock
       if( Unused_B(iBlock) ) CYCLE
       State_VGB(Ne2Rho_,:,:,:,iBlock) = RhoBcFactor_I(Ne2_) * &
            State_VGB(SWHRho_,:,:,:,iBlock)
       State_VGB(Ne2RhoUx_:Ne2RhoUz_,:,:,:,iBlock) = &
            RhoBcFactor_I(Ne2_)*uBcFactor_I(Ne2_)* &
            State_VGB(SWHRhoUx_:SWHRhoUz_,:,:,:,iBlock)
       State_VGB(Ne2P_,:,:,:,iBlock) = RhoBcFactor_I(Ne2_) * &
            State_VGB(SWHp_,:,:,:,iBlock)
    end do

    call test_stop(NameSub, DoTest)
  end subroutine user_initial_perturbation
  !============================================================================
  subroutine user_update_states(iBlock)

    use ModUpdateState, ONLY: update_state_normal

    use ModAdvance, ONLY: StateOld_VGB, State_VGB
    use ModGeometry, ONLY: rMin_B
    use ModPui, ONLY: Vpui_I, DeltaVpui_I

    ! merav
    ! C.P. edited

    integer,intent(in):: iBlock

    integer:: i, j, k
    real :: RhoInv, Ux, Uy, Uz

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_update_states'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    call update_state_normal(iBlock)

    if(UseSingleIonVelocityRegion3)then
       call select_region(iBlock)
       do k=1, nK; do j=1, nJ; do i=1, nI
          if(Ne3_ == iFluidProduced_C(i,j,k))then

             ! Calcualate average velocity from total momentum and density
             RhoInv= 1/sum(State_VGB(iRhoIon_I,i,j,k,iBlock))
             Ux = RhoInv*sum(State_VGB(iRhoUxIon_I,i,j,k,iBlock))
             Uy = RhoInv*sum(State_VGB(iRhoUyIon_I,i,j,k,iBlock))
             Uz = RhoInv*sum(State_VGB(iRhoUzIon_I,i,j,k,iBlock))

             ! Reset the momentum of all ion fluids
             State_VGB(iRhoUxIon_I,i,j,k,iBlock) = &
                  Ux*State_VGB(iRhoIon_I,i,j,k,iBlock)
             State_VGB(iRhoUyIon_I,i,j,k,iBlock) = &
                  Uy*State_VGB(iRhoIon_I,i,j,k,iBlock)
             State_VGB(iRhoUzIon_I,i,j,k,iBlock) = &
                  Uz*State_VGB(iRhoIon_I,i,j,k,iBlock)
          end if
       end do; end do; end do
    end if

    if(DoFreezeNeutral)then
       ! Set neutrals back to previous state
       do k=1,nK; do j=1,nJ; do i=1,nI
          State_VGB(NeuRho_:Ne4P_,i,j,k,iBlock) = &
               StateOld_VGB(NeuRho_:Ne4P_,i,j,k,iBlock)
       end do; end do; end do
       RETURN
    end if

    if(nPui > 1)then
       do k=1,nk; do j=1,nJ; do i=1,nI
          ! Correct PUI density for full velocity distribution
          State_VGB(Pu3Rho_,i,j,k,iBlock) = &
               4*cPi*sum(State_VGB(PuiFirst_:PuiLast_,i,j,k,iBlock) &
               *Vpui_I**2*DeltaVpui_I)
          ! Correct PUI pressure for full velocity distribution
          State_VGB(Pu3P_,i,j,k,iBlock) = &
               4*cPi/3*sum(State_VGB(PuiFirst_:PuiLast_,i,j,k,iBlock) &
               *Vpui_I**4*DeltaVpui_I)
       end do; end do; end do
    end if

    ! No need to check blocks outside:
    if(rMin_B(iBlock) > rBody) RETURN

    do k=1,nK; do j=1,nJ; do i=1,nI
       if(r_GB(i,j,k,iBlock) > rBody) CYCLE

       if(iTableSolarWind > 0)then
          ! Calculate the time dependent solar wind
          call calc_time_dep_sw(i, j, k, iBlock)
       else
          ! Retain initial condition for all ions inside body
          State_VGB(1:iP_I(nIonFluid),i,j,k,iBlock) = &
               StateOld_VGB(1:iP_I(nIonFluid),i,j,k,iBlock)
       endif

    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  contains
    !==========================================================================
    subroutine calc_time_dep_sw(i, j, k, iBlock)

      ! Time dependent solar wind from a lookup table

      use BATL_lib, ONLY: Xyz_DGB
      use ModCoordTransform, ONLY: rot_xyz_sph

      integer,intent(in):: i, j, k, iBlock

      ! variables for Solar Cycle
      real :: Rho, Ur, Temp, p, x, y, z, r
      real :: Bsph_D(3), Vsph_D(3)
      ! merav
      real :: v_D(3),vPUI_D(3), vPUISph_D(3)
      real :: XyzSph_DD(3,3) ! rotation matrix Xyz_D = matmul(XyzSph_DD,Sph_D)
      real :: SinTheta
      !------------------------------------------------------------------------
      x = Xyz_DGB(1,i,j,k,iBlock)
      y = Xyz_DGB(2,i,j,k,iBlock)
      z = Xyz_DGB(3,i,j,k,iBlock)

      call get_lat_dep_sw(x, y, z, Rho, Ur, Temp, Bsph_D)

      ! Include electron pressure (!?)
      p = 2*Rho*Temp

      XyzSph_DD = rot_xyz_sph(x,y,z)

      ! Spherical velocity, Vr, Vtheta, Vphi constant with  radial distance
      Vsph_D = [ Ur, 0.0, 0.0 ]

      ! momentum
      v_D = matmul(XyzSph_DD, Vsph_D)
      State_VGB(SWHRhoUx_:SWHRhoUz_,i,j,k,iBlock) = Rho*v_D

      ! Spherical magnetic field converted to Cartesian components
      State_VGB(Bx_:Bz_,i,j,k,iBlock) = matmul(XyzSph_DD, Bsph_D)

      ! density
      State_VGB(SWHRho_,i,j,k,iBlock) = Rho
      ! Level set function
      State_VGB(LevelHP_,i,j,k,iBlock) = Rho
      ! pressures
      if(UseElectronPressure)then
         State_VGB(SWHP_,i,j,k,iBlock) = 0.5*p
         State_VGB(Pe_,i,j,k,iBlock)   = 0.5*p
      else
         State_VGB(SWHP_,i,j,k,iBlock) = p
      end if
      if(.not.IsMhd)then
         ! PUI density
         State_VGB(Pu3Rho_,i,j,k,iBlock) = Pu3Rho * (rBody/r)**2
         ! PUI momentum
         vPUI_D = matmul(XyzSph_DD, VPUIsph_D)
         State_VGB(Pu3RhoUx_:Pu3RhoUz_,i,j,k,iBlock) = &
              State_VGB(Pu3Rho_,i,j,k,iBlock)*vPUI_D
         ! PUI pressure
         State_VGB(Pu3P_,i,j,k,iBlock) = Pu3P * (rBody/r)**(2*Gamma)
         State_VGB(LevelHP_,i,j,k,iBlock) = &
               State_VGB(SWHRho_,i,j,k,iBlock) &
               + State_VGB(Pu3Rho_,i,j,k,iBlock)
      end if

    end subroutine calc_time_dep_sw
    !==========================================================================
  end subroutine user_update_states
  !============================================================================
  subroutine user_action(NameAction)

    character(len=*), intent(in):: NameAction

    character(len=*), parameter:: StringFormat = '(10X,A19,F15.6,A11,F15.6)'

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_action'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(NameAction /= 'write progress') RETURN

    write(*,StringFormat) 'SwhRhoDim [n/cc]:',SwhRhoDim,'SwhRho:',SwhRho
    write(*,StringFormat) 'SwhUxDim  [km/s]:',SwhUxDim,'SwhUx:',SwhUx
    write(*,StringFormat) 'SwhUyDim  [km/s]:',SwhUyDim,'SwhUy:',SwhUy
    write(*,StringFormat) 'SwhUzDim  [km/s]:',SwhUzDim,'SwhUz:',SwhUz
    write(*,StringFormat) 'SwhTDim   [   K]:',SwhTDim,'SwhP:',SwhP
    write(*,StringFormat) 'SwhBxDim  [  nT]:',SwhBxDim,'SwhBx:',SwhBx
    write(*,StringFormat) 'SwhByDim  [  nT]:',SwhByDim,'SwhBy:',SwhBy
    write(*,StringFormat) 'SwhBzDim  [  nT]:',SwhBzDim,'SwhBz:',SwhBz
    write(*,'(10X,A19,F15.6)')           'SwhTDim   [   K]:',SwhTDim
    write(*,*)
    write(*,*)
    write(*,StringFormat) 'VliswRhoDim[n/cc]:',VliswRhoDim,'VliswRho:',VliswRho
    write(*,StringFormat) 'VliswUxDim[km/s]: ',VliswUxDim,'VliswUx:',VliswUx
    write(*,StringFormat) 'VliswUyDim[km/s]: ',VliswUyDim,'VliswUy:',VliswUy
    write(*,StringFormat) 'VliswUzDim[km/s]: ',VliswUzDim,'VliswUz:',VliswUz
    write(*,StringFormat) 'VliswPDim [nPa]: ',VliswPDim,'VliswP:',VliswP
    write(*,StringFormat) 'VliswBxDim[nT]: ',VliswBxDim,'VliswBx:',VliswBx
    write(*,StringFormat) 'VliswByDim[nT]:',VliswByDim,'VliswBy:',VliswBy
    write(*,StringFormat) 'VliswBzDim[nT]:',VliswBzDim,'VliswBz:',VliswBz
    write(*,'(10X,A19,F15.6)') 'VliswTDim[K]: ',VliswTDim!

    if(iTableSolarWind > 0)then
       write(*,*)
       write(*,'(10X,A19,F15.6)') 'RunStart[yr]:', RunStart
       write(*,'(10X,A19,F15.6)') 'Offset[yr]:',Offset
    end if

    if(UseNeutralFluid)then
       ! neutrals
       write(*,*)
       write(*,StringFormat) &
            'RhoNeuWindDim:',RhoNeuWindDim ,'RhoNeutralsISW:',RhoNeutralsISW
       write(*,StringFormat) &
            'UxNeuWindDim:',UxNeuWindDim,'UxNeutralsISW:',UxNeutralsISW
       write(*,StringFormat) &
            'UyNeuWindDim:',UyNeuWindDim,'UyNeutralsISW:',UyNeutralsISW
       write(*,StringFormat) &
            'UzNeuWindDim:',UzNeuWindDim,'UzNeutralsISW:',UzNeutralsISW
       write(*,StringFormat) &
            'pNeuWindDim:',pNeuWindDim,'PNeutralsISW:',PNeutralsISW
       write(*,'(10X,A19,F15.6)') 'TempNeuWindDim:',TempNeuWindDim
       write(*,*)
    end if
    if(.not.IsMhd)then
       write(*,*)
       write(*,StringFormat) 'Pu3RhoDim [n/cc]:',Pu3RhoDim,'SwhRho:',Pu3Rho
       write(*,StringFormat) 'Pu3UxDim  [km/s]:',Pu3UxDim,'Pu3Ux:',Pu3Ux
       write(*,StringFormat) 'Pu3UyDim  [km/s]:',Pu3UyDim,'Pu3Uy:',Pu3Uy
       write(*,StringFormat) 'Pu3UzDim  [km/s]:',Pu3UzDim,'Pu3Uz:',Pu3Uz
       write(*,StringFormat) 'Pu3TDim   [   K]:',Pu3TDim,'Pu3P:',Pu3P
       write(*,'(10X,A19,F15.6)')           'Pu3TDim   [   K]:',Pu3TDim
       write(*,*)
    end if
    if(UseElectronPressure)then
       write(*,StringFormat) 'VliswPeDim [nPa]: ', &
            VliswPeDim,' VliswPe:', VliswPe
       write(*,'(10X,A19,F15.6)') 'VliswTeDim[K]: ', VliswTeDim
       write(*,StringFormat) 'SwhTeDim   [   K]:', SwhTeDim, &
            ' SwhPe:', SwhPe
    end if

    call test_stop(NameSub, DoTest)
  end subroutine user_action
  !============================================================================
  subroutine user_set_plot_var(iBlock, NameVar, IsDimensional, &
       PlotVar_G, PlotVarBody, UsePlotVarBody, &
       NameTecVar, NameTecUnit, NameIdlUnit, IsFound)

    integer,          intent(in)   :: iBlock
    character(len=*), intent(in)   :: NameVar
    logical,          intent(in)   :: IsDimensional
    real,             intent(out)  :: PlotVar_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)
    real,             intent(out)  :: PlotVarBody
    logical,          intent(out)  :: UsePlotVarBody
    character(len=*), intent(inout):: NameTecVar
    character(len=*), intent(inout):: NameTecUnit
    character(len=*), intent(inout):: NameIdlUnit
    logical,          intent(out)  :: IsFound

    integer :: i,j,k
    real:: Source_V(nVar+nFluid)
    real:: U2_I(nFluid), TempSi_I(nFluid), NumDensSi_I(nFluid), UThS_I(nFluid)
    real :: U_DI(3,nFluid)
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_plot_var'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    UsePlotVarBody = .true.
    PlotVarBody    = 0.0

    if(.not.IsMhd)then
       IsFound = .true.
       select case(NameVar)
       case('fluid')
          if(UseNeutralFluid)then
             call select_region(iBlock)
             PlotVar_G(1:nI,1:nJ,1:nK) = iFluidProduced_C
          else
             PlotVar_G(1:nI,1:nJ,1:nK) = 0.0
          endif
       case('mach')
          ! C.P. edited to add PUI
          PlotVar_G = &
               sqrt( sum((State_VGB(SWHRhoUx_:SWHRhoUz_,:,:,:,iBlock) &
               + State_VGB(Pu3RhoUx_:Pu3RhoUz_,:,:,:,iBlock))**2, DIM=1) &
               / (Gamma *(State_VGB(SWHp_,:,:,:,iBlock) &
               + State_VGB(Pu3P_,:,:,:,iBlock)) &
               * (State_VGB(SWHRho_,:,:,:,iBlock) &
               +  State_VGB(Pu3Rho_,:,:,:,iBlock))) )
          ! merav addition
          ! edited to add PUI, could be more generic from first ion to last ion
       case('machalfven')
          PlotVar_G = &
               sqrt( sum((State_VGB(SWHRhoUx_:SWHRhoUz_,:,:,:,iBlock) &
               + State_VGB(Pu3RhoUx_:Pu3RhoUz_,:,:,:,iBlock))**2, DIM=1) &
               /   (( sum(State_VGB(Bx_:Bz_,:,:,:,iBlock)**2) &
               * (State_VGB(SWHRho_,:,:,:,iBlock) &
               +  State_VGB(Pu3Rho_,:,:,:,iBlock)) ) ) )

          ! end of merav addition

       case('srswhcx','sruxswhcx','sruyswhcx', &
            'sruzswhcx','spswhcx','seswhcx')
          if(UseNeutralFluid)then
             call select_region(iBlock)
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                call calc_source_inputs( &
                     i,j,k,iBlock,NumDensSi_I,U_DI,U2_I,TempSi_I,UThS_I)
                call calc_charge_exchange_source( &
                     i,j,k,iBlock,NumDensSi_I,U_DI,U2_I,UThS_I,Source_V)
                select case(NameVar)
                case('srswhcx')
                   PlotVar_G(i,j,k) = Source_V(SwhRho_)*No2Io_V(UnitRho_)
                case('sruxswhcx')
                   PlotVar_G(i,j,k) = Source_V(SwhRhoUx_)*No2Io_V(UnitRhoU_)
                case('sruyswhcx')
                   PlotVar_G(i,j,k) = Source_V(SwhRhoUy_)*No2Io_V(UnitRhoU_)
                case('sruzswhcx')
                   PlotVar_G(i,j,k) = Source_V(SwhRhoUz_)*No2Io_V(UnitRhoU_)
                case('spswhcx')
                   PlotVar_G(i,j,k) = Source_V(SwhP_)*No2Io_V(UnitP_)
                case('seswhcx')
                   PlotVar_G(i,j,k) = Source_V(SwhEnergy_)&
                        *No2Io_V(UnitEnergydens_)
                end select
                PlotVar_G(i,j,k) = PlotVar_G(i,j,k)/No2Io_V(UnitT_)
             end do; end do; end do
          else
             ! For kinetic, sources in ExtraSource_ICB, or get from FLEKS
             PlotVar_G = 0.0
          end if
          select case(NameVar)
          case('srswhcx')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRho_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRho_)) // '/s'
          case('sruxswhcx')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRhoU_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRhoU_)) // '/s'
          case('sruyswhcx')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRhoU_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRhoU_)) // '/s'
          case('sruzswhcx')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRhoU_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRhoU_)) // '/s'
          case('spswhcx')
             NameIdlUnit = trim(NameIdlUnit_V(UnitP_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitP_)) // '/s'
          case('seswhcx')
             NameIdlUnit = trim(NameIdlUnit_V(UnitEnergydens_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitEnergydens_)) // '/s'
          end select

       case('srpuicx','sruxpuicx','sruypuicx', &
            'sruzpuicx','sppuicx','sepuicx')
          if(UseNeutralFluid)then
             call select_region(iBlock)
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                call calc_source_inputs( &
                     i,j,k,iBlock,NumDensSi_I,U_DI,U2_I,TempSi_I,UThS_I)
                call calc_charge_exchange_source( &
                     i,j,k,iBlock,NumDensSi_I,U_DI,U2_I,UThS_I,Source_V)
                select case(NameVar)
                case('srpuicx')
                   PlotVar_G(i,j,k) = Source_V(Pu3Rho_)*No2Io_V(UnitRho_)
                case('sruxpuicx')
                   PlotVar_G(i,j,k) = Source_V(Pu3RhoUx_)*No2Io_V(UnitRhoU_)
                case('sruypuicx')
                   PlotVar_G(i,j,k) = Source_V(Pu3RhoUy_)*No2Io_V(UnitRhoU_)
                case('sruzpuicx')
                   PlotVar_G(i,j,k) = Source_V(Pu3RhoUz_)*No2Io_V(UnitRhoU_)
                case('sppuicx')
                   PlotVar_G(i,j,k) = Source_V(Pu3P_)*No2Io_V(UnitP_)
                case('sepuicx')
                   PlotVar_G(i,j,k) = Source_V(Pu3Energy_)&
                        *No2Io_V(UnitEnergydens_)
                end select
                PlotVar_G(i,j,k) = PlotVar_G(i,j,k)/No2Io_V(UnitT_)
             end do; end do; end do
          else
             ! For kinetic, sources in ExtraSource_ICB, or get from FLEKS
             PlotVar_G = 0.0
          end if
          select case(NameVar)
          case('srpuicx')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRho_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRho_)) // '/s'
          case('sruxpuicx')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRhoU_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRhoU_)) // '/s'
          case('sruypuicx')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRhoU_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRhoU_)) // '/s'
          case('sruzpuicx')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRhoU_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRhoU_)) // '/s'
          case('sppuicx')
             NameIdlUnit = trim(NameIdlUnit_V(UnitP_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitP_)) // '/s'
          case('sepuicx')
             NameIdlUnit = trim(NameIdlUnit_V(UnitEnergydens_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitEnergydens_)) // '/s'
          end select

       case('srswhimp','sruxswhimp','sruyswhimp', &
            'sruzswhimp','spswhimp','seswhimp')
          if(UseNeutralFluid)then
             call select_region(iBlock)
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                call calc_source_inputs( &
                     i,j,k,iBlock,NumDensSi_I,U_DI,U2_I,TempSi_I,UThS_I)
                call calc_electron_impact_source( &
                     i,j,k,iBlock,NumDensSi_I,U_DI,U2_I,UThS_I,Source_V)
                select case(NameVar)
                case('srswhimp')
                   PlotVar_G(i,j,k) = Source_V(SwhRho_)*No2Io_V(UnitRho_)
                case('sruxswhimp')
                   PlotVar_G(i,j,k) = Source_V(SwhRhoUx_)*No2Io_V(UnitRhoU_)
                case('sruyswhimp')
                   PlotVar_G(i,j,k) = Source_V(SwhRhoUy_)*No2Io_V(UnitRhoU_)
                case('sruzswhimp')
                   PlotVar_G(i,j,k) = Source_V(SwhRhoUz_)*No2Io_V(UnitRhoU_)
                case('spswhimp')
                   PlotVar_G(i,j,k) = Source_V(SwhP_)*No2Io_V(UnitP_)
                case('seswhimp')
                   PlotVar_G(i,j,k) = Source_V(SwhEnergy_)&
                        *No2Io_V(UnitEnergydens_)
                end select
                PlotVar_G(i,j,k) = PlotVar_G(i,j,k)/No2Io_V(UnitT_)
             end do; end do; end do
          else
             ! For kinetic, sources in ExtraSource_ICB, or get from FLEKS
             PlotVar_G = 0.0
          end if
          select case(NameVar)
          case('srswhimp')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRho_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRho_)) // '/s'
          case('sruxswhimp')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRhoU_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRhoU_)) // '/s'
          case('sruyswhimp')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRhoU_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRhoU_)) // '/s'
          case('sruzswhimp')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRhoU_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRhoU_)) // '/s'
          case('spswhimp')
             NameIdlUnit = trim(NameIdlUnit_V(UnitP_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitP_)) // '/s'
          case('seswhimp')
             NameIdlUnit = trim(NameIdlUnit_V(UnitEnergydens_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitEnergydens_)) // '/s'
          end select

       case('srpuiimp','sruxpuiimp','sruypuiimp', &
            'sruzpuiimp','sppuiimp','sepuiimp')
          if(UseNeutralFluid)then
             call select_region(iBlock)
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                call calc_source_inputs( &
                     i,j,k,iBlock,NumDensSi_I,U_DI,U2_I,TempSi_I,UThS_I)
                call calc_electron_impact_source( &
                     i,j,k,iBlock,NumDensSi_I,U_DI,U2_I,UThS_I,Source_V)
                select case(NameVar)
                case('srpuiimp')
                   PlotVar_G(i,j,k) = Source_V(Pu3Rho_)*No2Io_V(UnitRho_)
                case('sruxpuiimp')
                   PlotVar_G(i,j,k) = Source_V(Pu3RhoUx_)*No2Io_V(UnitRhoU_)
                case('sruypuiimp')
                   PlotVar_G(i,j,k) = Source_V(Pu3RhoUy_)*No2Io_V(UnitRhoU_)
                case('sruzpuiimp')
                   PlotVar_G(i,j,k) = Source_V(Pu3RhoUz_)*No2Io_V(UnitRhoU_)
                case('sppuiimp')
                   PlotVar_G(i,j,k) = Source_V(Pu3P_)*No2Io_V(UnitP_)
                case('sepuiimp')
                   PlotVar_G(i,j,k) = Source_V(Pu3Energy_)&
                        *No2Io_V(UnitEnergydens_)
                end select
                PlotVar_G(i,j,k) = PlotVar_G(i,j,k)/No2Io_V(UnitT_)
             end do; end do; end do
          else
             ! For kinetic, sources in ExtraSource_ICB, or get from FLEKS
             PlotVar_G = 0.0
          end if
          select case(NameVar)
          case('srpuiimp')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRho_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRho_)) // '/s'
          case('sruxpuiimp')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRhoU_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRhoU_)) // '/s'
          case('sruypuiimp')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRhoU_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRhoU_)) // '/s'
          case('sruzpuiimp')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRhoU_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRhoU_)) // '/s'
          case('sppuiimp')
             NameIdlUnit = trim(NameIdlUnit_V(UnitP_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitP_)) // '/s'
          case('sepuiimp')
             NameIdlUnit = trim(NameIdlUnit_V(UnitEnergydens_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitEnergydens_)) // '/s'
          end select

       case default
          IsFound = .false.
       end select
    else
       IsFound = .true.
       select case(NameVar)
       case('fluid')
          if(UseNeutralFluid)then
             call select_region(iBlock)
             PlotVar_G(1:nI,1:nJ,1:nK) = iFluidProduced_C
          else
             ! This is not needed when not using the 4 nutral fluids
             call CON_stop(NameSub//': no neutral fluids present')
          endif
       case('mach')
          PlotVar_G = &
               sqrt( sum(State_VGB(RhoUx_:RhoUz_,:,:,:,iBlock)**2, DIM=1)  &
               / (Gamma*State_VGB(p_,:,:,:,iBlock) &
               *  State_VGB(Rho_,:,:,:,iBlock)) )
          ! merav addition
       case('machalfven')
          PlotVar_G = &
               sqrt( sum(State_VGB(RhoUx_:RhoUz_,:,:,:,iBlock)**2, DIM=1)  &
               /   ( sum(State_VGB(Bx_:Bz_,:,:,:,iBlock)**2, DIM=1) &
               * State_VGB(Rho_,:,:,:,iBlock)) )

          ! end of merav addition

       case('srcx','sruxcx','sruycx', &
            'sruzcx','spcx','secx')
          if(UseNeutralFluid)then
             call select_region(iBlock)
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                call calc_source_inputs( &
                     i,j,k,iBlock,NumDensSi_I,U_DI,U2_I,TempSi_I,UThS_I)
                call calc_charge_exchange_source( &
                     i,j,k,iBlock,NumDensSi_I,U_DI,U2_I,UThS_I,Source_V)
                select case(NameVar)
                case('srcx')
                   PlotVar_G(i,j,k) = Source_V(Rho_)*No2Io_V(UnitRho_)
                case('sruxcx')
                   PlotVar_G(i,j,k) = Source_V(RhoUx_)*No2Io_V(UnitRhoU_)
                case('sruycx')
                   PlotVar_G(i,j,k) = Source_V(RhoUy_)*No2Io_V(UnitRhoU_)
                case('sruzcx')
                   PlotVar_G(i,j,k) = Source_V(RhoUz_)*No2Io_V(UnitRhoU_)
                case('spcx')
                   PlotVar_G(i,j,k) = Source_V(P_)*No2Io_V(UnitP_)
                case('secx')
                   PlotVar_G(i,j,k) = Source_V(Energy_)&
                        *No2Io_V(UnitEnergydens_)
                end select
                PlotVar_G(i,j,k) = PlotVar_G(i,j,k)/No2Io_V(UnitT_)
             end do; end do; end do
          else
             ! For kinetic, sources in ExtraSource_ICB, or get from FLEKS
             PlotVar_G = 0.0
          end if
          select case(NameVar)
          case('srcx')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRho_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRho_)) // '/s'
          case('sruxcx')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRhoU_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRhoU_)) // '/s'
          case('sruycx')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRhoU_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRhoU_)) // '/s'
          case('sruzcx')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRhoU_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRhoU_)) // '/s'
          case('spcx')
             NameIdlUnit = trim(NameIdlUnit_V(UnitP_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitP_)) // '/s'
          case('secx')
             NameIdlUnit = trim(NameIdlUnit_V(UnitEnergydens_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitEnergydens_)) // '/s'
          end select

       case('srph','sruxph','sruyph','sruzph','spph','seph')
          if(UseNeutralFluid .and. UsePhotoion)then
             call select_region(iBlock)
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                call calc_source_inputs( &
                     i,j,k,iBlock,NumDensSi_I,U_DI,U2_I,TempSi_I,UThS_I)
                call calc_photoion_source( &
                     i,j,k,iBlock,U_DI,U2_I,Source_V)
                select case(NameVar)
                case('srph')
                   PlotVar_G(i,j,k) = Source_V(Rho_)*No2Io_V(UnitRho_)
                case('sruxph')
                   PlotVar_G(i,j,k) = Source_V(RhoUx_)*No2Io_V(UnitRhoU_)
                case('sruyph')
                   PlotVar_G(i,j,k) = Source_V(RhoUy_)*No2Io_V(UnitRhoU_)
                case('sruzph')
                   PlotVar_G(i,j,k) = Source_V(RhoUz_)*No2Io_V(UnitRhoU_)
                case('spph')
                   PlotVar_G(i,j,k) = Source_V(P_)*No2Io_V(UnitP_)
                case('seph')
                   PlotVar_G(i,j,k) = Source_V(Energy_)&
                        *No2Io_V(UnitEnergydens_)
                end select
                PlotVar_G(i,j,k) = PlotVar_G(i,j,k)/No2Io_V(UnitT_)
             end do; end do; end do
          else
             PlotVar_G = 0.0
          end if
          select case(NameVar)
          case('srph')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRho_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRho_)) // '/s'
          case('sruxph')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRhoU_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRhoU_)) // '/s'
          case('sruyph')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRhoU_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRhoU_)) // '/s'
          case('sruzph')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRhoU_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRhoU_)) // '/s'
          case('spph')
             NameIdlUnit = trim(NameIdlUnit_V(UnitP_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitP_)) // '/s'
          case('seph')
             NameIdlUnit = trim(NameIdlUnit_V(UnitEnergydens_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitEnergydens_)) // '/s'
          end select

       case('srimp','sruximp','sruyimp','sruzimp','spimp','seimp')
          if(UseNeutralFluid)then
             call select_region(iBlock)
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                call calc_source_inputs( &
                     i,j,k,iBlock,NumDensSi_I,U_DI,U2_I,TempSi_I,UThS_I)
                call calc_electron_impact_source( &
                     i,j,k,iBlock,NumDensSi_I,U_DI,U2_I,UThS_I,Source_V)
                select case(NameVar)
                case('srimp')
                   PlotVar_G(i,j,k) = Source_V(Rho_)*No2Io_V(UnitRho_)
                case('sruximp')
                   PlotVar_G(i,j,k) = Source_V(RhoUx_)*No2Io_V(UnitRhoU_)
                case('sruyimp')
                   PlotVar_G(i,j,k) = Source_V(RhoUy_)*No2Io_V(UnitRhoU_)
                case('sruzimp')
                   PlotVar_G(i,j,k) = Source_V(RhoUz_)*No2Io_V(UnitRhoU_)
                case('spimp')
                   PlotVar_G(i,j,k) = Source_V(P_)*No2Io_V(UnitP_)
                case('seimp')
                   PlotVar_G(i,j,k) = Source_V(Energy_)&
                        *No2Io_V(UnitEnergydens_)
                end select
                PlotVar_G(i,j,k) = PlotVar_G(i,j,k)/No2Io_V(UnitT_)
             end do; end do; end do
          else
             ! For kinetic, sources in ExtraSource_ICB, or get from FLEKS
             PlotVar_G = 0.0
          end if
          select case(NameVar)
          case('srimp')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRho_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRho_)) // '/s'
          case('sruximp')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRhoU_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRhoU_)) // '/s'
          case('sruyimp')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRhoU_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRhoU_)) // '/s'
          case('sruzimp')
             NameIdlUnit = trim(NameIdlUnit_V(UnitRhoU_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitRhoU_)) // '/s'
          case('spimp')
             NameIdlUnit = trim(NameIdlUnit_V(UnitP_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitP_)) // '/s'
          case('seimp')
             NameIdlUnit = trim(NameIdlUnit_V(UnitEnergydens_)) // '/s'
             NameTecUnit = trim(NameTecUnit_V(UnitEnergydens_)) // '/s'
          end select

       case default
          IsFound = .false.
       end select
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_plot_var
  !============================================================================
  subroutine user_calc_sources_expl(iBlock)

    ! Calculates the charge exchange cross sections for the neutrals
    ! This subroutine calculate the charge exchange between the ionized
    ! component and the three population of neutrals, Neu, Ne2, Ne3
    ! following the notation of McNutt (1998)
    ! The three population of neutrals are the neutrals created by
    ! charge exchange in different regions of the outer heliosphere
    !
    ! Neu are the neutrals the comes from the interstellar medium  (PopI)
    ! Ne2 are created between the Termination Shock and Heliopause (PopII)
    ! Ne3 are the neutrals created inside the Termination Shock    (PopIII)
    ! Ne4 are the neutrals created between bow shock and heliopause (PopIV)

    ! As an example for Neu the source terms inside the Termination Shock will
    ! take into account their creations and destruction;
    ! outside the Termination Shock they will be just destroyed
    ! (see more details below).
    !
    ! Fluid 1 is the SW ionized fluid,
    ! Fluid 2 is the Pick-up ion fluid and
    ! Fluids 3 to 6 are the neutral fluids
    !
    ! History of implementation:
    ! Written by Merav Opher April 21, 2002;
    ! Help From Gabor Toth
    ! *modified for the version 7.5.2 May, 2002*
    ! *revistied and modified on Nov, 2002*
    ! source terms for the plasma
    ! modified to take of the S=0 Feb 08 (initialization)
    ! modified June 08, 2007 to take into account multi-fluids
    ! modified August 18;September10, 2007 to take into account 3-fluids
    ! october 27 checking units
    ! January 01, 2008 implementing implicit source terms
    ! January 08, 2008 all source terms for PopI as implicit
    ! January 24, 2008 comments included
    ! January-June BUGS fixed
    ! September - source terms written as McNutt (1998)
    !

    integer, intent(in) :: iBlock

    real :: State_V(nVar)
    real:: RhoSWH, uSWH_D(3), sRho, sRhoU_D(3), sEnergy, &
         RhoPu3, uPu3_D(3), USWH2, UPu32
    real, dimension(nVar + nFluid):: SourceCx_V, SourcePh_V, &
         SourceImp_V

    real :: U2_I(nFluid), TempSi_I(nFluid)
    real :: NumDensSi_I(nFluid), UTh2Si_I(nFluid)
    real :: HeatElectron

    real :: U_DI(3,nFluid)

    integer :: i, j, k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_calc_sources_expl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    call update_photoionization_rate

    ! updating sources from AMPS in PT-OH Coupling
    ! hyzhou: this is evaluated both in explicit and implicit scheme.
    ! Is it necessary to do so?
    if(.not.UseNeutralFluid)then
       if(allocated(ExtraSource_ICB))then

          do k = 1, nK; do j = 1, nJ; do i = 1, nI

             ! skipping the cells inside rBody
             if(.not.Used_GB(i,j,k,iBlock))CYCLE

             ! Extract conservative variables
             State_V = State_VGB(:,i,j,k,iBlock)

             if(.not.IsMhd)then

                ! SWH variables
                RhoSWH  = State_V(SWHRho_)
                uSWH_D  = State_V(SWHRhoUx_:SWHRhoUz_)/RhoSWH
                USWH2   = sum(uSWH_D**2)

                ! updating source terms for SWH
                sRho = ExtraSource_ICB(1,i,j,k,iBlock)*RhoSWH
                sRhoU_D = ExtraSource_ICB(2:4,i,j,k,iBlock)*RhoSWH
                sEnergy = ExtraSource_ICB(5,i,j,k,iBlock)*RhoSWH
                Source_VC(SWHRho_,i,j,k) = Source_VC(SWHRho_,i,j,k) + sRho
                Source_VC(SWHRhoUx_:RhoUz_,i,j,k) = &
                     Source_VC(SWHRhoUx_:SWHRhoUz_,i,j,k) + sRhoU_D
                Source_VC(SWHEnergy_,i,j,k) = &
                     Source_VC(SWHEnergy_,i,j,k) + sEnergy
                Source_VC(SWHP_,i,j,k) = Source_VC(SWHP_,i,j,k) &
                     + GammaMinus1* &
                     (sEnergy - sum(uSWH_D*sRhoU_D) + 0.5*USWH2*sRho)

                ! Pu3 variables
                RhoPu3  = State_V(Pu3Rho_)
                uPu3_D  = State_V(Pu3RhoUx_:Pu3RhoUz_)/RhoPu3
                UPu32   = sum(uPu3_D**2)

                ! updating source terms for Pu3
                sRho = ExtraSource_ICB(6,i,j,k,iBlock)*RhoPu3
                sRhoU_D = ExtraSource_ICB(7:9,i,j,k,iBlock)*RhoPu3
                sEnergy = ExtraSource_ICB(10,i,j,k,iBlock)*RhoPu3
                Source_VC(Pu3Rho_,i,j,k) = Source_VC(Pu3Rho_,i,j,k) + sRho
                Source_VC(Pu3RhoUx_:Pu3RhoUz_,i,j,k) = &
                     Source_VC(Pu3RhoUx_:Pu3RhoUz_,i,j,k) + sRhoU_D
                Source_VC(Pu3Energy_,i,j,k) = &
                     Source_VC(Pu3Energy_,i,j,k) + sEnergy
                Source_VC(Pu3P_,i,j,k) = Source_VC(Pu3P_,i,j,k) &
                     + GammaMinus1* &
                     (sEnergy - sum(uPu3_D*sRhoU_D) + 0.5*UPu32*sRho)

             else
                RhoSWH  = State_V(Rho_)
                uSWH_D  = State_V(RhoUx_:RhoUz_)/RhoSWH
                USWH2   = sum(uSWH_D**2)

                ! updating source terms for single-ion plasma
                sRho = ExtraSource_ICB(1,i,j,k,iBlock)*RhoSWH
                sRhoU_D = ExtraSource_ICB(2:4,i,j,k,iBlock)*RhoSWH
                sEnergy = ExtraSource_ICB(5,i,j,k,iBlock)*RhoSWH
                Source_VC(Rho_,i,j,k) = Source_VC(Rho_,i,j,k) + sRho
                Source_VC(RhoUx_:RhoUz_,i,j,k) = &
                     Source_VC(RhoUx_:RhoUz_,i,j,k) + sRhoU_D
                Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) + sEnergy
                Source_VC(p_,i,j,k) = Source_VC(p_,i,j,k) &
                     + GammaMinus1* &
                     (sEnergy - sum(uSWH_D*sRhoU_D) + 0.5*USWH2*sRho)
             end if

             if(DoTest .and. i==iTest .and. j==jTest .and. k==kTest)then
                write(*,*) 'SwhRho = ', State_V(Rho_)
                write(*,*) 'U2 =', USWH2
                write(*,*) 'SwhP   = ', State_V(p_)
                write(*,*) 'AMPS SW Density Source =', sRho
                write(*,*) 'AMPS SW MVx Source =', sRhoU_D
                write(*,*) 'AMPS SW Energy Source =', sEnergy
                write(*,*) 'Source_VC(Rho) =',Source_VC(Rho_,i,j,k)
                write(*,*) 'Source_VC(RhoUx) =',Source_VC(RhoUx_,i,j,k)
                write(*,*) 'Source_VC(Energy) =',Source_VC(Energy_,i,j,k)
                write(*,*) 'Source_VC(P) =',Source_VC(p_,i,j,k)
                write(*,*) 'Norm Rho =', No2Si_V(UnitRho_)
                write(*,*) 'Norm n =', No2Si_V(UnitN_)
                write(*,*) 'Norm U =', No2Si_V(UnitU_)
                write(*,*) 'Norm Time =', No2Si_V(UnitT_)
                if(.not.IsMhd)then
                   write(*,*) ' PUI Density Source =', &
                        ExtraSource_ICB(6,i,j,k,iBlock)
                   write(*,*) ' PUI MVx Source =', &
                        ExtraSource_ICB(7,i,j,k,iBlock)
                   write(*,*) ' PUI Energy Source =', &
                        ExtraSource_ICB(10,i,j,k,iBlock)
                end if
             endif

          end do; end do; end do
       end if

       RETURN
    end if

    ! Figure out which neutral population is produced at this point
    call select_region(iBlock)

    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       ! Extract conservative variables
       State_V = State_VGB(:,i,j,k,iBlock)

       ! Calculate Rho, U, U^2, Temp, and UTh^2 for source terms
       call calc_source_inputs( &
            i,j,k,iBlock,NumDensSi_I,U_DI,U2_I,TempSi_I,UTh2Si_I)

       ! Charge Exchange
       if (nPui>1 .and. UsePuiCxHeliosheath) then
          call calc_charge_exchange_source_pui( &
            i,j,k,iBlock,NumDensSi_I,U_DI,U2_I,Uth2Si_I,SourceCx_V)
       else
          call calc_charge_exchange_source( &
            i,j,k,iBlock,NumDensSi_I,U_DI,U2_I,UTh2Si_I,SourceCx_V)
       endif

       ! Photoionization
       if(UsePhotoion)then
          call calc_photoion_source( &
               i,j,k,iBlock,U_DI,U2_I,SourcePh_V)
       else
          SourcePh_V = 0.0
       end if

       ! Electron Impact Ionization
       if(UseElectronImpact)then
          call calc_electron_impact_source( &
               i,j,k,iBlock,NumDensSi_I,U_DI,U2_I,UTh2Si_I,SourceImp_V)
       else
          SourceImp_V = 0.0
       end if

       if(.not.IsMhd)then
          ! Heating Pu3
          if(UsePu3Heating) then
             ! HeatPu3 = n*(TempPu3 - T)/(gamma-1) * (r-rBody)*FactorPu3
             ! in normalized units, so FactorPu3 has units of 1/(AU s)
             HeatPu3 = &
                  InvGammaMinus1*State_V(PU3Rho_)/MassFluid_I(Pu3_) &
                  *(TempPu3Si - TempSi_I(PU3_))*Si2No_V(UnitTemperature_) &
                  *(r_GB(i,j,k,iBlock) - rBody)*FactorPu3/Si2No_V(UnitT_)
          else
             HeatPu3 = 0.0
          end if
       end if

       if(UseElectronPressure .and. UseElectronHeating)then
          ! Heating electrons

          ! HeatElectron = (PeHeat - Pe)*(r-rBody)*FactorHeatE
          ! in normalized units, so FactorHeatE has units of 1/(AU s)
          HeatElectron = &
               (PressureHeatElectronIo*Io2No_V(UnitP_) - State_V(Pe_))*&
               (r_GB(i,j,k,iBlock) - rBody)*FactorHeatElectron/Si2No_V(UnitT_)

       else
          HeatElectron = 0.0
       end if

       if(PuiFirst_ > 1 .and. .not.UsePuiCxHeliosheath) call add_pui_source( &
         i, j, k, iBlock, NumDensSi_I, U_DI, U2_I, UTh2Si_I)

       ! Calculate the source terms for this cell
       call calc_source_cell
    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  contains
    !==========================================================================
    subroutine calc_source_cell

      ! Calculate source temrs for one cell. The pressures source is
      ! S(p) = (gamma-1)[S(e) - u.S(Rhou) + 0.5 u**2 S(Rho)]

      real :: Source_V(nVar + nFluid)
      integer :: iVar
      !------------------------------------------------------------------------
      Source_V = 0.0

      if(.not.IsMhd)then
         ! Source terms for the ion populations
         if(UseSource_I(SWH_) .and. UseSource_I(Pu3_))then
            ! Region 3: only make Pu3 in region before TS
            if (Ne3_ == iFluidProduced_C(i,j,k)) then
               ! in Pop III region
               Source_V(Pu3Energy_)= HeatPu3
               Source_V(Pu3P_) = (Gamma-1)* ( Source_V(Pu3Energy_) &
                    - sum(U_DI(:,Pu3_)*Source_V(Pu3RhoUx_:Pu3RhoUz_)) &
                    + 0.5*U2_I(Pu3_)*Source_V(Pu3Rho_) )
               Source_V(Pe_) = HeatElectron
            end if
         end if
      end if

      Source_V = Source_V + SourceCx_V + SourcePh_V + SourceImp_V

      Source_VC(:,i,j,k) = Source_VC(:,i,j,k) + Source_V

      if(DoTest .and. i==iTest .and. j==jTest .and. k==kTest)then

         Source_V = Source_VC(:,i,j,k)

         write(*,*) NameSub, ' iFluidProduced=', iFluidProduced_C(i,j,k)
         do iVar = 1, nVar + nFLuid
            write(*,*) ' Source(',NameVar_V(iVar),')=',Source_V(iVar)
         end do

         do iVar = 1, nVar + nFLuid
            write(*,*) ' SourceCx(',NameVar_V(iVar),')=',SourceCx_V(iVar)
         end do

         do iVar = 1, nVar + nFLuid
            write(*,*) ' SourcePh(',NameVar_V(iVar),')=',SourcePh_V(iVar)
         end do

         do iVar = 1, nVar + nFLuid
            write(*,*) ' SourceImp(',NameVar_V(iVar),')=',SourceImp_V(iVar)
         end do

         if(.not.IsMhd) write(*,*) ' HeatPu3=', HeatPu3

         if(UseElectronPressure)  write(*,*) ' HeatElectron=', HeatElectron
      end if

    end subroutine calc_source_cell
    !==========================================================================
  end subroutine user_calc_sources_expl
  !============================================================================
  subroutine add_pui_source(i, j, k, iBlock, NumDensSi_I, U_DI, U2_I, UTh2Si_I)

    use ModPUI, ONLY: Vpui_I

    integer, intent(in) :: i, j, k, iBlock
    real, intent(out) :: NumDensSi_I(nFluid), U_DI(3,nFLuid), U2_I(nFluid)
    real, intent(out) :: UTh2Si_I(nFluid)

    integer :: iVar, iPui
    real, dimension(nFluid) :: NumDens_I, UTh_I
    real, dimension(Neu_:Ne4_) :: URelS_I, Alpha_I, Rate_I, Source_I
    real :: Xpui, DeltaVpXpui, DeltaVpXpuiSi, Sigma
    real :: PhotoIonRate

    ! Region 3: only make Pu3 in region before TS
    !--------------------------------------------------------------------------
    if (Ne3_ == iFluidProduced_C(i,j,k)) then
       UTh_I = sqrt(UTh2Si_I)*Si2No_V(UnitU_)
       NumDens_I = NumDensSi_I*Si2No_V(UnitN_)

       URelS_I = sqrt((U_DI(x_,Neu_:) - U_DI(x_,Ion_))**2 &
            +         (U_DI(y_,Neu_:) - U_DI(y_,Ion_))**2 &
            +         (U_DI(z_,Neu_:) - U_DI(z_,Ion_))**2)/UTh_I(Neu_:)

       Alpha_I = UTh_I(Ion_)/UTh_I(Neu_:)

       do iPui = 1, nPui
          Xpui = Vpui_I(iPui)/UTh_I(Ion_)

          DeltaVpXpui = UTh_I(Ion_)*(1./sqrt(cPi)*exp(-Xpui**2) &
               + (Xpui + 0.5/Xpui)*Erf(Xpui))

          DeltaVpXpuiSi = DeltaVpXpui*No2Si_V(UnitU_)

          Sigma = ((CrossA1 - CrossA2*log(DeltaVpXpuiSi*100.))**2)*1.E-4

          Rate_I = State_VGB(iRho_I(SWHRho_),i,j,k,iBlock) &
               *Sigma*DeltaVpXpuiSi*No2Si_V(UnitN_)

          if(UsePhotoion) then
             call get_photoionization_rate_si(r_GB(i,j,k,iBlock), &
                  PhotoIonRate)
             Rate_I = Rate_I  + PhotoIonRate
          end if

          Source_I = Rate_I*No2Si_V(UnitT_) &
               *NumDens_I(Neu_:)/(4.*cPi**1.5*UTh_I(Ion_)*UTh_I(Neu_:)**2) &
               *(exp(-(Alpha_I*Xpui - URelS_I)**2) &
               - exp(-(Alpha_I*Xpui + URelS_I)**2)) &
	       /(Xpui*URelS_I)

          iVar = PuiFirst_ - 1 + iPui
          Source_VC(iVar,i,j,k) = Source_VC(iVar,i,j,k) &
               + sum(Source_I) - Source_I(Ne3_)
       end do
    end if

  end subroutine add_pui_source
  !============================================================================
  subroutine calc_source_inputs( &
       i, j, k, iBlock, NumDensSi_I, U_DI, U2_I, TempSi_I, UTh2Si_I)

    ! Calculate parameters to pass to source terms

    integer, intent(in):: i, j, k, iBlock
    real,  intent(out) :: NumDensSi_I(nFluid), U2_I(nFluid)
    real, intent(out) :: TempSi_I(nFluid), UTh2Si_I(nFluid)
    real, intent(out) :: U_DI(3,nFLuid)

    real :: State_V(nVar)
    real :: NumDens_I(nFluid)
    !--------------------------------------------------------------------------
    State_V = State_VGB(:,i,j,k,iBlock)

    ! Numberd densities
    NumDens_I = State_V(iRho_I)/MassFluid_I

    ! Number densities in #/m^3
    NumDensSi_I = NumDens_I*No2Si_V(UnitN_)

    ! Velocities per component
    U_DI(x_,:) = State_V(iRhoUx_I)/State_V(iRho_I)
    U_DI(y_,:) = State_V(iRhoUy_I)/State_V(iRho_I)
    U_DI(z_,:) = State_V(iRhoUz_I)/State_V(iRho_I)

    ! Velocity square in normalized units
    U2_I = sum(U_DI**2, 1)

    ! Temperature (K)
    TempSi_I = State_V(iP_I)/NumDens_I*No2Si_V(UnitTemperature_)

    ! If not using Pe, electron pressure is grouped with SWH
    if(.not.UseElectronPressure)then
       if(IsMhd)then
          TempSi_I(Ion_) = 0.5*TempSi_I(Ion_)
       else
          TempSi_I(SWH_) = (State_V(SWHP_) &
               /(2*State_V(SWHRho_) + State_V(PU3Rho_))) &
               *No2Si_V(UnitTemperature_)
       endif
    end if

    ! Thermal speed squared in (m/s)^2
    UTh2Si_I = (2*cBoltzmann/cProtonMass)*TempSi_I

  end subroutine calc_source_inputs
  !============================================================================
  subroutine calc_charge_exchange_source_pui( &
       i,j,k,iBlock,NumDensSi_I,U_DI,U2_I,UTh2Si_I,SourceCx_V)

    use ModPui, ONLY: Vpui_I, DeltaVpui_I, DeltaLogVpui

    ! Calculate the charge exchange source terms for one cell with PUI bins
    ! Source terms described in Bair et al. (2026)

    integer, intent(in):: i,j,k,iBlock
    real, dimension(nFluid), intent(in) :: NumDensSi_I, U2_I, UTh2Si_I
    real, intent(in) :: U_DI(3,nFluid)
    real, intent(out):: SourceCx_V(nVar + nFluid)

    integer :: iDim, iNeu, iPui, iSubSample, iFluidProduced
    real :: State_V(nVar)

    ! Fluid parameters
    real:: NumDensSwh, NumDensPui, USwh, UPui, PPui, &
         UThSwh, UThPui
    real, dimension(3):: USwh_D, UPui_D
    real, dimension(Neu_:Ne4_,3):: UNeu_ID, URel_ID, URelPu3_ID, &
         UMean_ID, UMeanPu3_ID
    real, dimension(Neu_:Ne4_):: NumDensNeu_I, UNeu_I, PNeu_I, &
         UThNeu_I, URel2_I, URel_I, &
         UTh2Sum_I, InvUTh2Sum_I, URel2Pu3_I, URelPu3_I, &
         UTh2SumPu3_I, InvUTh2SumPu3_I, UMean2_I, UMean2Pu3_I, &
         XpPui_I, XmPui_I, Xswh_I

    ! Distribution function parameters
    real, dimension(nPui):: FStarPui_I
    real:: FStarPui, Vpui, DeltaVpui, &
         VsubBot, VsubTop, DeltaVsub, NormalizedWidth
    real, dimension(nSubSample):: &
         Vsub_I, XpSwhSub_I, XmSwhSub_I, XpPuiSub_I, XmPuiSub_I
    real, dimension(Neu_:Ne4_):: Xp_I, Xm_I, XpSwh_I, XmSwh_I, FStarNeu_I, &
         FStarNeuVNeu2_I
    real:: CumSumFpuiV1, CumSumFpuiV2, CumSumFpuiV3, CumSumFpuiV4

    ! Helper functions: h(x)
    real, dimension(Neu_:Ne4_):: H1Xp_I, H1Xm_I, H2Xp_I, H2Xm_I, &
         H5Xp_I, H5Xm_I, H6Xp_I, H6Xm_I, H7Xp_I, &
         H7Xm_I, H8XSwh_I, H9XSwh_I, H10XSwh_I, H11XSwh_I
    real:: H8Sub
    real:: H8Sub_I(nSubSample)

    ! For SWH
    real, dimension(Neu_:Ne4_) :: &
         I0xp_I, I0px_I, Kxp_I, Kpx_I,  &
         UmeanDotURel_I, UNeuDotJxp_I
    real, dimension(Neu_:Ne4_):: IntegralpxRho_I, IntegralpxRho1_I, &
         IntegralpxRho2_I, &
         IntegralpxU_I, IntegralpxU1_I, IntegralpxU2_I, &
         IntegralpxP_I, IntegralpxP1_I, IntegralpxP2_I, &
         g0pxRhoSi_I, g0pxUSi_I, g0pxPSi_I, &
         SourceRhoxp_I, SourcePxp_I
    real:: g0xpFSi
    real:: g0xpFSiSub_I(nSubSample)
    real, dimension(Neu_:Ne4_,3):: Jxp_ID, Jpx_ID
    real, dimension(nSubSample):: &
         SourceFxpSub_I, SourceFV2xpSub_I, FStarNeuSub_I, FStarNeuVNeu2Sub_I
    real, dimension(Neu_:Ne4_, nPui):: &
         SourceFxp_II, SourceFV2xp_II

    ! For PU3
    real, dimension(Neu_:Ne4_):: &
         I0xpu3_I, I0pu3x_I, Kxpu3_I, Kpu3x_I, &
         UPuiDotSourceUpu3x_I, UNeuDotSourceUxpu3_I
    real, dimension(Neu_:Ne4_):: Integralpu3xU1_I, Integralpu3xU2_I, &
         Integralxpu3U1_I, Integralxpu3U2_I, &
         g0pu3xFSi_I, g0pu3xUSi_I, g0xpu3USi_I, &
         SourceRhopu3x_I, SourceRhoxpu3_I, SourcePpu3x_I, SourcePxpu3_I
    real:: g0xpu3FSi
    real, dimension(Neu_:Ne4_,3):: SourceUpu3x_ID, SourceUxpu3_ID
    real, dimension(Neu_:Ne4_,3) :: Jxpu3_ID, Jpu3x_ID
    real, dimension(Neu_:Ne4_,nPui):: &
         SourceFpu3x_II, SourceFxpu3_II, SourceFxpu3P_II

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_charge_exchange_source_pui'
    !--------------------------------------------------------------------------
    State_V = State_VGB(:,i,j,k,iBlock)

    ! PUI distribution function (Star for pitch-angle averaged)
    FStarPui_I = State_V(PuiFirst_:PuiLast_)

    NumDensPui = NumDensSi_I(Pu3_)*Si2No_V(UnitN_)
    PPui = State_V(iP_I(Pu3_))

    NumDensSwh = NumDensSi_I(Swh_)*Si2No_V(UnitN_)

    UPui_D = U_DI(:,Pu3_)
    UPui = sqrt(U2_I(Pu3_))
    USwh_D = U_DI(:,Swh_)
    USwh = sqrt(U2_I(Swh_))

    UThSwh = sqrt(UTh2Si_I(Swh_))*Si2No_V(UnitU_)
    UThPui = sqrt(UTh2Si_I(Pu3_))*Si2No_V(UnitU_)

    ! Sum of thermal speeds squared for SW fluid and neutral fluid
    UTh2Sum_I = (UTh2Si_I(SWH_) + UTh2Si_I(Neu_:Ne4_)) &
         *Si2No_V(UnitU_)**2
    InvUTh2Sum_I = 1./UTh2Sum_I

    ! Sum of thermal speeds for PUI fluid and neutral fluid
    UTh2SumPu3_I = (UTh2Si_I(Pu3_) + UTh2Si_I(Neu_:Ne4_)) &
         *Si2No_V(UnitU_)**2
    InvUTh2SumPu3_I = 1./UTh2SumPu3_I

    ! Relative velocity between neutrals and ion fluids squared
    do iDim = x_,z_
       URel_ID(:,iDim) = (U_DI(iDim,Swh_) - U_DI(iDim,Neu_:Ne4_))
       URelPu3_ID(:,iDim) = (U_DI(iDim,Pu3_) - U_DI(iDim,Neu_:Ne4_))
    end do
    URel2_I = sum(URel_ID**2, 2)
    URel_I = sqrt(URel2_I)
    URel2Pu3_I = sum(URelPu3_ID**2, 2)
    URelPu3_I = sqrt(URel2Pu3_I)

    ! Thermal speed weighted average velocity
    do iDim = x_,z_
       Umean_ID(:,iDim) = (U_DI(iDim,Swh_)*UTh2Si_I(Neu_:Ne4_) &
          + U_DI(iDim,Neu_:Ne4_)*UTh2Si_I(Swh_))*InvUTh2Sum_I &
          *Si2No_V(UnitU_)**2

       UmeanPu3_ID(:,iDim) = (U_DI(iDim,Pu3_)*UTh2Si_I(Neu_:Ne4_) &
          + U_DI(iDim, Neu_:Ne4_)*UTh2Si_I(Pu3_))*InvUTh2SumPu3_I &
          *Si2No_V(UnitU_)**2
    end do
    Umean2_I = sum(UMean_ID**2, 2)
    Umean2Pu3_I = sum(UmeanPu3_ID**2, 2)

    UThNeu_I = sqrt(UTh2Si_I(Neu_:Ne4_)) * Si2No_V(UnitU_)
    NumDensNeu_I = NumDensSi_I(Neu_:Ne4_) * Si2No_V(UnitN_)
    do iDim = x_,z_
      UNeu_ID(:,iDim) = U_DI(iDim,Neu_:Ne4_)
    end do
    UNeu_I = norm2(UNeu_ID, 2)
    PNeu_I = State_V(iP_I(Neu_:Ne4_))

    CumSumFpuiV1 = 0
    CumSumFpuiV2 = 0
    CumSumFpuiV3 = 0
    CumSumFpuiV4 = 0
    SourceRhopu3x_I = 0
    SourceRhoxpu3_I = 0
    SourcePpu3x_I = 0
    SourcePxpu3_I = 0
    Integralpu3xU1_I = 0
    Integralpu3xU2_I = 0
    Integralxpu3U1_I = 0
    SourceRhoxp_I = 0
    SourcePxp_I = 0

    ! Iterate over pui bins
    ! We go in reverse order for the CumSumFpui terms
    do iPui = nPui, 1, -1
      Vpui = Vpui_I(iPui)
      DeltaVpui = DeltaVpui_I(iPui)
      FStarPui = FStarPui_I(iPui)

      Xp_I = abs((Vpui+URelPu3_I)/UThNeu_I)
      Xm_I = abs((Vpui-URelPu3_I)/UThNeu_I)
      XpSwh_I = abs((Vpui+URel_I)/UThNeu_I)
      XmSwh_I = abs((Vpui-URel_I)/UThNeu_I)

      call h1(Xp_I,H1Xp_I)
      call h1(Xm_I,H1Xm_I)
      call h2(Xp_I,H2Xp_I)
      call h2(Xm_I,H2Xm_I)
      call h5(Xp_I,H5Xp_I)
      call h5(Xm_I,H5Xm_I)
      call h6(Xp_I,H6Xp_I)
      call h6(Xm_I,H6Xm_I)
      call h7(Xp_I,H7Xp_I)
      call h7(Xm_I,H7Xm_I)

      g0pu3xFSi_I = UthNeu_I**3/12/Vpui/URelPu3_I*max(1E-30,H1Xp_I-H1Xm_I) &
         *No2Si_V(UnitU_)

      SourceFpu3x_II(:,iPui) = FStarPui*NumDensNeu_I &
         *sigma_cx_array(g0pu3xFSi_I)*g0pu3xFSi_I &
         /Si2No_V(UnitN_)/Si2No_V(UnitT_)

      CumSumFpuiV1 = CumSumFpuiV1 + FStarPui*DeltaVpui*Vpui
      CumSumFpuiV2 = CumSumFpuiV2 + FStarPui*DeltaVpui*Vpui**2
      CumSumFpuiV3 = CumSumFpuiV3 + FStarPui*DeltaVpui*Vpui**3
      CumSumFpuiV4 = CumSumFpuiV4 + FStarPui*DeltaVpui*Vpui**4

      g0xpu3FSi = Vpui/NumDensPui*(PPui/MassFluid_I(Pu3_)/Vpui**2 &
         + NumDensPui + 4*cPi*( CumSumFpuiV3/Vpui - CumSumFpuiV2 &
         + (Vpui*CumSumFpuiV1 - CumSumFpuiV4/Vpui**2)/3 )) &
         *No2Si_V(UnitU_)

      ! Subsample neutrals over the width of each bin
      ! Only subsample near peak of neutral distribution
      ! Note: bin is centered on Vpui in log space, not linear space
      VsubBot = Vpui*exp(-0.5*DeltaLogVpui)
      VsubTop = Vpui*exp(+0.5*DeltaLogVpui)
      DeltaVsub = (VsubTop-VsubBot)/nSubSample
      do iNeu = Neu_,Ne4_
      ! If the sample spacing is still larger than thermal speed,
      ! Scale each point to the width of the distribution
      NormalizedWidth = max(1.0, DeltaVsub/UThNeu_I(iNeu))

        if (3*UThNeu_I(iNeu)+0.5*DeltaVpui-abs(Vpui-URel_I(iNeu))> 0) then
          do iSubSample = 1, nSubSample
            Vsub_I(iSubSample) = VsubBot+DeltaVsub*(iSubSample-0.5)
          end do
          XpSwhSub_I = (Vsub_I+URel_I(iNeu))/UthNeu_I(iNeu)
          XmSwhSub_I = (Vsub_I-URel_I(iNeu))/UthNeu_I(iNeu)

          call h8_sub(Vsub_I/UThSwh, H8Sub_I)
          g0xpFSiSub_I = UthSwh*H8Sub_I * No2Si_V(UnitU_)

          SourceFxpSub_I = &
               0.25*NumDensSwh*NumDensNeu_I(iNeu) &
               /cPi**1.5/Vsub_I/URel_I(iNeu)/UThNeu_I(iNeu) &
               *sigma_cx_sub(g0xpFSiSub_I)*g0xpFSiSub_I &
               *(exp(-XmSwhSub_I**2)-exp(-XpSwhSub_I**2)) &
               /Si2No_V(UnitN_)/Si2No_V(UnitT_)
          SourceFxp_II(iNeu,iPui) = sum(SourceFxpSub_I*Vsub_I**2)/Vpui**2 &
               /nSubSample/NormalizedWidth

          SourceFV2xpSub_I = &
               0.25*NumDensSwh*NumDensNeu_I(iNeu) &
               /cPi**1.5/UThNeu_I(iNeu) &
               *sigma_cx_sub(g0xpFSiSub_I)*g0xpFSiSub_I &
               *( (UThNeu_I(iNeu)**2/Vsub_I/URel_I(iNeu) &
               +URel_I(iNeu)/Vsub_I + Vsub_I/URel_I(iNeu)) &
               *(exp(-XmSwhSub_I**2)-exp(-XpSwhSub_I**2)) &
               - 2.*(exp(-XmSwhSub_I**2)-exp(-XpSwhSub_I**2))) &
               /Si2No_V(UnitN_)/Si2No_V(UnitT_)
          SourceFV2xp_II(iNeu,iPui) = &
               sum(SourceFV2xpSub_I*Vsub_I**2)/Vpui**2 &
               /nSubSample/NormalizedWidth
        else
          call h8_scalar(Vpui/UThSwh, H8Sub)
          g0xpFSi = UthSwh*H8Sub * No2Si_V(UnitU_)

          SourceFxp_II(iNeu,iPui) = 0.25*NumDensSwh*NumDensNeu_I(iNeu) &
                /cPi**1.5/Vpui/URel_I(iNeu)/UThNeu_I(iNeu)*sigma_cx(g0xpFSi) &
                *g0xpFSi*(exp(-XmSwh_I(iNeu)**2)-exp(-XpSwh_I(iNeu)**2)) &
                /Si2No_V(UnitN_)/Si2No_V(UnitT_)

          SourceFV2xp_II(iNeu,iPui) = &
               0.25*NumDensSwh*NumDensNeu_I(iNeu) &
               /cPi**1.5/UThNeu_I(iNeu) &
               *sigma_cx(g0xpFSi)*g0xpFSi &
               *( (UThNeu_I(iNeu)**2/Vpui/URel_I(iNeu) &
               +URel_I(iNeu)/Vpui + Vpui/URel_I(iNeu)) &
               *(exp(-XmSwh_I(iNeu)**2)-exp(-XpSwh_I(iNeu)**2)) &
               - 2.*(exp(-XmSwh_I(iNeu)**2)-exp(-XpSwh_I(iNeu)**2))) &
               /Si2No_V(UnitN_)/Si2No_V(UnitT_)
        end if
        if (3*UThNeu_I(iNeu)+0.5*DeltaVpui-abs(Vpui-URelPu3_I(iNeu))> 0) then
          do iSubSample = 1, nSubSample
            Vsub_I(iSubSample) = VsubBot+DeltaVsub*(iSubSample-0.5)
          end do
          XpPuiSub_I = abs((Vsub_I+URelPu3_I(iNeu))/UthNeu_I(iNeu))
          XmPuiSub_I = abs((Vsub_I-URelPu3_I(iNeu))/UthNeu_I(iNeu))

          ! Neutral distribution, pitch angle averaged in PUI frame
          FStarNeuSub_I = 0.25*NumDensNeu_I(iNeu)/cPi**1.5/Vsub_I &
              /URelPu3_I(iNeu)/UThNeu_I(iNeu) &
              *(exp(-XmPuiSub_I**2) - exp(-XpPuiSub_I**2))
          FStarNeu_I(iNeu) = sum(FStarNeuSub_I*Vsub_I**2)/Vpui**2 &
               /nSubSample/NormalizedWidth

          FStarNeuVneu2Sub_I = 0.25*NumDensNeu_I(iNeu) &
               /cPi**1.5/UThNeu_I(iNeu) &
               *( (UThNeu_I(iNeu)**2/Vsub_I/URelPu3_I(iNeu) &
               +URelPu3_I(iNeu)/Vsub_I + Vsub_I/URelPu3_I(iNeu)) &
               *(exp(-XmPuiSub_I**2)-exp(-XpPuiSub_I**2)) &
               - 2.*(exp(-XmPuiSub_I**2)-exp(-XpPuiSub_I**2)))

          FStarNeuVneu2_I(iNeu) = sum(FStarNeuVneu2Sub_I*Vsub_I**2)/Vpui**2 &
               /nSubSample/NormalizedWidth

        else
          FStarNeu_I(iNeu) = 0.25*NumDensNeu_I(iNeu)/cPi**1.5/Vpui &
                /URelPu3_I(iNeu)/UThNeu_I(iNeu) &
                *(exp(-Xm_I(iNeu)**2) - exp(-Xp_I(iNeu)**2))

          FStarNeuVneu2_I(iNeu) = 0.25*NumDensNeu_I(iNeu) &
               /cPi**1.5/UThNeu_I(iNeu) &
               *( (UThNeu_I(iNeu)**2/Vpui/URelPu3_I(iNeu) &
               +URelPu3_I(iNeu)/Vpui + Vpui/URelPu3_I(iNeu)) &
               *(exp(-Xm_I(iNeu)**2)-exp(-Xp_I(iNeu)**2)) &
               - 2.*(exp(-Xm_I(iNeu)**2)-exp(-Xp_I(iNeu)**2)))
        end if
      end do

      SourceFxpu3_II(:,iPui) = NumDensPui*FStarNeu_I &
           *sigma_cx(g0xpu3FSi)*g0xpu3FSi &
           /Si2No_V(UnitN_)/Si2No_V(UnitT_)

      SourceFxpu3P_II(:,iPui) = NumDensPui*FStarNeuVNeu2_I &
           *sigma_cx(g0xpu3FSi)*g0xpu3FSi &
           /Si2No_V(UnitN_)/Si2No_V(UnitT_)

      SourceRhopu3x_I = SourceRhopu3x_I &
           + SourceFpu3x_II(:,iPui)*Vpui**2*DeltaVpui

      SourceRhoxpu3_I = SourceRhoxpu3_I &
           + SourceFxpu3_II(:,iPui)*Vpui**2*DeltaVpui

      Integralpu3xU1_I = Integralpu3xU1_I &
           + FStarPui*Vpui*DeltaVpui &
           *max(UThNeu_I**2*1E-30, 0.2*UThNeu_I**2*(H2Xp_I-H2Xm_I) &
           - (URel2Pu3_I+Vpui**2)*(H1Xp_I-H1Xm_I))

      Integralxpu3U1_I = Integralxpu3U1_I &
           + FStarPui*Vpui*DeltaVpui &
           *min(-UThNeu_I**2*1E-30, UThNeu_I**2/3*(H6Xp_I-H6Xm_I) &
           +(Vpui**2-URel2Pu3_I)*(H5Xp_I-H5Xm_I))

      SourcePpu3x_I = SourcePpu3x_I &
           + SourceFpu3x_II(:,iPui)*Vpui**4*DeltaVpui

      SourcePxpu3_I = SourcePxpu3_I &
           + SourceFxpu3P_II(:,iPui)*Vpui**2*DeltaVpui

      SourceRhoxp_I = SourceRhoxp_I &
           + SourceFxp_II(:,iPui)*Vpui**2*DeltaVpui

      SourcePxp_I = SourcePxp_I &
           + SourceFV2xp_II(:,iPui)*Vpui**2*DeltaVpui
    end do

    SourceRhopu3x_I = 4*cPi*SourceRhopu3x_I
    SourceRhoxpu3_I = 4*cPi*SourceRhoxpu3_I

    SourcePpu3x_I = 4*cPi/3*SourcePpu3x_I
    SourcePxpu3_I = 4*cPi/3*SourcePxpu3_I

    Integralpu3xU1_I = Integralpu3xU1_I &
         *cPi*UThNeu_I**3/6/NumDensPui/URelPu3_I**3

    Integralpu3xU2_I = UThPui**2

    g0pu3xUSi_I = Integralpu3xU2_I/Integralpu3xU1_I * No2Si_V(UnitU_)

    Integralxpu3U1_I = Integralxpu3U1_I &
         *0.25*cPi*UThNeu_I**3/NumDensPui/URelPu3_I**3

    Integralxpu3U2_I = -UThNeu_I**2

    g0xpu3USi_I = Integralxpu3U2_I/Integralxpu3U1_I * No2Si_V(UnitU_)

    SourceRhoxp_I = 4*cPi*SourceRhoxp_I
    SourcePxp_I = 4*cPi/3*SourcePxp_I

    I0pu3x_I = SourceRhoxpu3_I
    I0xpu3_I = SourceRhoxpu3_I

    do iNeu = Neu_, Ne4_
      SourceFpu3x_II(iNeu,:) = SourceFpu3x_II(iNeu,:) &
           *SourceRhoxpu3_I(iNeu)/SourceRhopu3x_I(iNeu)
    end do

    SourcePpu3x_I = SourcePpu3x_I*SourceRhoxpu3_I/SourceRhopu3x_I

    UPuiDotSourceUpu3x_I = 0
    UNeuDotSourceUxpu3_I = 0
    do iDim=X_,Z_
      SourceUpu3x_ID(:,iDim) =  0.785*NumDensPui*NumDensNeu_I &
          *Integralpu3xU1_I*URelPu3_ID(:,iDim)*sigma_cx_array(g0pu3xUSi_I) &
           *No2Si_V(UnitU_)/Si2No_V(UnitN_)/Si2No_V(UnitT_)
      SourceUxpu3_ID(:,iDim) = NumDensPui*NumDensNeu_I &
           *Integralxpu3U1_I*URelPu3_ID(:,iDim) *sigma_cx_array(g0xpu3USi_I)&
           *No2Si_V(UnitU_)/Si2No_V(UnitN_)/Si2No_V(UnitT_)

      Jpu3x_ID(:,iDim) = SourceRhopu3x_I*UPui_D(iDim) &
           + SourceUpu3x_ID(:,iDim)
      Jxpu3_ID(:,iDim) = SourceRhoxpu3_I*UNeu_ID(:,iDim) &
           + SourceUxpu3_ID(:,iDim)

      UPuiDotSourceUpu3x_I = UPuiDotSourceUpu3x_I &
           + UPui_D(iDim)*SourceUpu3x_ID(:,iDim)
      UNeuDotSourceUxpu3_I = UNeuDotSourceUxpu3_I &
           + UNeu_ID(:,iDim)*SourceUxpu3_ID(:,iDim)
    end do

    Kpu3x_I = InvGammaMinus1*SourcePpu3x_I &
         + UPuiDotSourceUpu3x_I &
         + 0.5*UPui**2*SourceRhopu3x_I
    Kxpu3_I = InvGammaMinus1*SourcePxpu3_I &
         + UNeuDotSourceUxpu3_I &
         + 0.5*UNeu_I**2*SourceRhoxpu3_I

    XSwh_I = URel_I/sqrt(UTh2Sum_I)

    call h8(XSwh_I,H8XSwh_I)
    IntegralpxRho1_I = 0.5*sqrt(cPi*UTh2Sum_I)*URel_I
    IntegralpxRho2_I = 0.5*sqrt(cPi)*UTh2Sum_I*URel_I*H8XSwh_I
    g0pxRhoSi_I = IntegralpxRho2_I/IntegralpxRho1_I * No2Si_V(UnitU_)

    IntegralpxRho_I = &
         IntegralpxRho1_I*sigma_cx_array(g0pxRhoSi_I)*g0pxRhoSi_I &
         /Si2No_V(UnitN_)/Si2No_V(UnitT_)

    call h9(XSwh_I,H9XSwh_I)
    IntegralpxU1_I = sqrt(cPi*InvUTh2Sum_I)*URel_I**3
    IntegralpxU2_I = sqrt(cPi)*URel_I**3*H9XSwh_I
    g0pxUSi_I = IntegralpxU2_I/IntegralpxU1_I * No2Si_V(UnitU_)

    IntegralpxU_I = IntegralpxU1_I*sigma_cx_array(g0pxUSi_I)*g0pxUSi_I &
            /Si2No_V(UnitN_)/Si2No_V(UnitT_)

    call h10(XSwh_I,H10XSwh_I)
    call h11(XSwh_I,H11XSwh_I)
    IntegralpxP1_I = &
            0.25*sqrt(cPi*UTh2Sum_I**3)*URel_I*H10XSwh_I
    IntegralpxP2_I = &
            0.25*sqrt(cPi)*UTh2Sum_I**2*URel_I*H11XSwh_I
    g0pxPSi_I = IntegralpxP2_I/IntegralpxP1_I * No2Si_V(UnitU_)

    IntegralpxP_I = IntegralpxP1_I*sigma_cx_array(g0pxPSi_I)*g0pxPSi_I &
            /Si2No_V(UnitN_)/Si2No_V(UnitT_)

    I0xp_I = SourceRhoxp_I
    I0px_I = SourceRhoxp_I

    UNeuDotJxp_I = 0
    do iDim = X_, Z_
      Jpx_ID(:,iDim) = NumDensSwh*NumDensNeu_I &
            *sqrt(InvUTh2Sum_I/cPi)/URel_I &
            *( 2*UMean_ID(:,iDim)*IntegralpxRho_I &
            +UThSwh**2*URel_ID(:,iDim)/URel2_I*IntegralpxU_I)

      Jxp_ID(:,iDim) = NumDensSwh*NumDensNeu_I &
            *sqrt(InvUTh2Sum_I/cPi)/URel_I &
            *( 2*UMean_ID(:,iDim)*IntegralpxRho_I &
            -UThNeu_I**2*URel_ID(:,iDim)/URel2_I*IntegralpxU_I)

      UNeuDotJxp_I = UNeuDotJxp_I &
           + UNeu_ID(:,iDim)*Jxp_ID(:,iDim)
    end do

    UmeanDotUrel_I = sum(Umean_ID*URel_ID, 2)

    Kpx_I = NumDensSwh*NumDensNeu_I &
            *sqrt(InvUTh2Sum_I/cPi)/URel_I*( &
            (InvGammaMinus1*InvUTh2Sum_I &
            *UThSwh**2*UThNeu_I**2 + Umean2_I)*IntegralpxRho_I &
            + UThSwh**2/URel2_I &
            *UmeanDotUrel_I*IntegralpxU_I &
            + InvUTh2Sum_I**2*UThSwh**4*IntegralpxP_I)

    Kxp_I = InvGammaMinus1*SourcePxp_I &
         + UNeuDotJxp_I - 0.5*UNeu_I**2*SourceRhoxp_I

    ! Only use the source terms requested by the user
    if (UseSource_I(Swh_)) then
      do iNeu = Neu_,Ne4_
        if (.not.UseSource_I(iNeu))then
          I0px_I(iNeu) = 0
          I0xp_I(iNeu) = 0
          Jpx_ID(iNeu,:) = 0
          Jxp_ID(iNeu,:) = 0
          Kpx_I(iNeu) = 0
          Kxp_I(iNeu) = 0
          SourceFxp_II(iNeu,:) = 0
        end if
      end do
    else
      I0px_I = 0
      I0xp_I = 0
      Jpx_ID = 0
      Jxp_ID = 0
      Kpx_I = 0
      Kxp_I = 0
      SourceFxp_II = 0
    end if

    if (UseSource_I(Pu3_)) then
        do iNeu = Neu_,Ne4_
          if (.not.UseSource_I(iNeu))then
            I0pu3x_I(iNeu) = 0
            I0xpu3_I(iNeu) = 0
            Jpu3x_ID(iNeu, :) = 0
            Jxpu3_ID(iNeu, :) = 0
            Kpu3x_I(iNeu) = 0
            Kxpu3_I(iNeu) = 0
            SourceFxpu3_II(iNeu,:) = 0
            SourceFpu3x_II(iNeu,:) = 0
          end if
        end do
    else
      I0pu3x_I = 0
      I0xpu3_I = 0
      Jpu3x_ID = 0
      Jxpu3_ID = 0
      Kpu3x_I = 0
      Kxpu3_I = 0
      SourceFxpu3_II = 0
      SourceFpu3x_II = 0
    end if

    ! PUIs are created in the solar wind (regions 2,3)
    ! and destroyed in the ISM (regions 1,4)
    ! in region i, cx with neutral fluid i never produces PUIs
    SourceCx_V = 0

    ! Ion source terms
    iFluidProduced = iFluidProduced_C(i,j,k)
    if (iFluidProduced == Ne2_ .or. iFluidProduced == Ne3_)then
        ! Solar wind: PUIs are created
        SourceCx_V(SwhRho_) = -sum(I0px_I) + I0xp_I(iFluidProduced) &
            + I0xpu3_I(iFluidProduced)
        SourceCx_V(SwhRhoUx_:SwhRhoUz_) = -sum(Jpx_ID,1) &
            + Jxp_ID(iFluidProduced,:) &
            + Jxpu3_ID(iFluidProduced,:)
        SourceCx_V(SwhEnergy_) = -sum(Kpx_I) + Kxpu3_I(iFLuidProduced) &
            + Kxp_I(iFluidProduced)
        SourceCx_V(SwhP_) = GammaMinus1*( SourceCx_V(SwhEnergy_) &
            -sum(USwh_D*SourceCx_V(SwhRhoUx_:SwhRhoUz_)) &
            +0.5*USwh**2*SourceCx_V(SwhRho_) )

        SourceCx_V(Pu3Rho_) = sum(I0xp_I) - I0xp_I(iFluidProduced) &
            +sum(I0xpu3_I) - I0xpu3_I(iFluidProduced) &
            -sum(I0pu3x_I)
        SourceCx_V(Pu3RhoUx_:Pu3RhoUz_) = &
            +sum(Jxp_ID,1) - Jxp_ID(iFluidProduced,:) &
            +sum(Jxpu3_ID,1) - Jxpu3_ID(iFluidProduced,:) &
            -sum(Jpu3x_ID,1)
        SourceCx_V(Pu3Energy_) = sum(Kxp_I) - Kxp_I(iFluidProduced) &
            +sum(Kxpu3_I) - Kxpu3_I(iFluidProduced) &
            -sum(Kpu3x_I)
        SourceCx_V(Pu3P_) = GammaMinus1*( SourceCx_V(Pu3Energy_) &
            -sum(UPui_D*SourceCx_V(Pu3RhoUx_:Pu3RhoUz_)) &
            +0.5*UPui**2*SourceCx_V(Pu3Rho_) )
        SourceCx_V(PuiFirst_:PuiLast_) = sum(SourceFxp_II,1) &
            -SourceFxp_II(iFluidProduced,:) &
            +sum(SourceFxpu3_II,1) - SourceFxpu3_II(iFluidProduced,:) &
            -sum(SourceFpu3x_II,1)
    else
        ! ISM: PUIs destroyed here
        SourceCx_V(SwhRho_) = -sum(I0px_I) + sum(I0xp_I) + sum(I0xpu3_I)
        SourceCx_V(SwhRhoUx_:SwhRhoUz_) = -sum(Jpx_ID,1) &
            + sum(Jxp_ID,1) + sum(Jxpu3_ID,1)
        SourceCx_V(SwhEnergy_) = -sum(Kpx_I) + sum(Kxpu3_I) + sum(Kxp_I)
        SourceCx_V(SwhP_) = GammaMinus1*( SourceCx_V(SwhEnergy_) &
            -sum(USwh_D*SourceCx_V(SwhRhoUx_:SwhRhoUz_)) &
            +0.5*USwh**2*SourceCx_V(SwhRho_) )

        SourceCx_V(Pu3Rho_) =  -sum(I0pu3x_I)
        SourceCx_V(Pu3RhoUx_:Pu3RhoUz_) = -sum(Jpu3x_ID,1)
        SourceCx_V(Pu3Energy_) = -sum(Kpu3x_I)
        SourceCx_V(Pu3P_) = GammaMinus1*( SourceCx_V(Pu3Energy_) &
            -sum(UPui_D*SourceCx_V(Pu3RhoUx_:Pu3RhoUz_)) &
            +0.5*UPui**2*SourceCx_V(Pu3Rho_) )
        SourceCx_V(PuiFirst_:PuiLast_) = -sum(SourceFpu3x_II,1)

    end if

    ! Neutral source terms
    ! Neutrals are lossed from each populations
    do iNeu = Neu_,Ne4_
        call select_fluid(iNeu)
        SourceCx_V(iRho)    = -I0xp_I(iNeu) - I0xpu3_I(iNeu)
        SourceCx_V(iRhoUx:iRhoUz)  = -Jxp_ID(iNeu,:) - Jxpu3_ID(iNeu,:)
        SourceCx_V(iEnergy) = -Kxp_I(iNeu) - Kxpu3_I(iNeu)
    end do

    ! Created neutrals all go to same population
    call select_fluid(iFluidProduced)
    SourceCx_V(iRho)    = &
          SourceCx_V(iRho) &
          +sum(I0px_I) + sum(I0pu3x_I)
    SourceCx_V(iRhoUx:iRhoUz)  = &
          SourceCx_V(iRhoUx:iRhoUz) &
          +sum(Jpx_ID,1) + sum(Jpu3x_ID,1)
    SourceCx_V(iEnergy) = &
          SourceCx_V(iEnergy) &
          +sum(Kpx_I) + sum(Kpu3x_I)

    do iNeu = Neu_,Ne4_
        call select_fluid(iNeu)
        SourceCx_V(iP) = GammaMinus1*(SourceCx_V(iEnergy) &
            -sum(U_DI(:,iNeu)*SourceCx_V(iRhoUx:iRhoUz)) &
            +0.5*UNeu_I(iNeu)**2*SourceCx_V(iRho) )
    end do
  contains
    !==========================================================================
    real function sigma_cx(URelSi)
      ! Calculate the charge exchange cross section
      real, intent(in):: URelSi
      !------------------------------------------------------------------------
      ! URelSi is in SI units (m/s), but needs to have units cm/s
      sigma_cx = &
           ((CrossA1 - CrossA2*log(URelSi*100.))**2)/1.E4
    end function sigma_cx
    !==========================================================================
    function sigma_cx_array(URelSi_I)
      ! Calculate the charge exchange cross section
      real, intent(in):: URelSi_I(Neu_:Ne4_)
      real:: sigma_cx_array(Neu_:Ne4_)
      !------------------------------------------------------------------------
      ! URelSi is in SI units (m/s), but needs to have units cm/s
      sigma_cx_array = &
           ((CrossA1 - CrossA2*log(URelSi_I*100.))**2)/1.E4
    end function sigma_cx_array
    !==========================================================================
    function sigma_cx_sub(URelSi_I)
      ! Calculate the charge exchange cross section
      real, intent(in):: URelSi_I(nSubSample)
      real:: sigma_cx_sub(nSubSample)
      !------------------------------------------------------------------------
      ! URelSi is in SI units (m/s), but needs to have units cm/s
      sigma_cx_sub = &
           ((CrossA1 - CrossA2*log(URelSi_I*100.))**2)/1.E4
    end function sigma_cx_sub
    !==========================================================================
    subroutine h1(X_I, H1_I)
      real, intent(in):: X_I(Neu_:Ne4_)
      real, intent(out):: H1_I(Neu_:Ne4_)
      !------------------------------------------------------------------------
      H1_I = 2./sqrt(cPi)*(1.+X_I**2)*exp(-X_I**2) &
           + (3.*X_I+2.*X_I**3)*erf(X_I)
    end subroutine
    !==========================================================================
    subroutine h2(X_I, H2_I)
      real, intent(in):: X_I(Neu_:Ne4_)
      real, intent(out):: H2_I(Neu_:Ne4_)
      !------------------------------------------------------------------------
      H2_I = 2/sqrt(cPi)*(1.+X_I**2+3*X_I**4)*exp(-X_I**2) &
           + (5.*X_I**3+6.*X_I**5)*erf(X_I)
    end subroutine
    !==========================================================================
    subroutine h5(X_I, H5_I)
      real, intent(in):: X_I(Neu_:Ne4_)
      real, intent(out):: H5_I(Neu_:Ne4_)
      !------------------------------------------------------------------------
      H5_I = 2./sqrt(cPi)*exp(-X_I**2) + (1./X_I + 2.*X_I)*erf(X_I)
    end subroutine
    !==========================================================================
    subroutine h6(X_I, H6_I)
      real, intent(in):: X_I(Neu_:Ne4_)
      real, intent(out):: H6_I(Neu_:Ne4_)
      !------------------------------------------------------------------------
      H6_I = 2./sqrt(cPi)*(2.-X_I**2)*exp(-X_I**2) &
           + (3*X_I-2.*X_I**3)*erf(X_I)
    end subroutine
    !==========================================================================
    subroutine h7(X_I, H7_I)
      real, intent(in):: X_I(Neu_:Ne4_)
      real, intent(out):: H7_I(Neu_:Ne4_)
      !------------------------------------------------------------------------
      H7_I = 2./sqrt(cPi)*(2.+X_I**2)*exp(-X_I**2) &
           + (5.*X_I+2.*X_I**3)*erf(X_I)
    end subroutine
    !==========================================================================
    subroutine h8(X_I, H8X_I)
      real, intent(in):: X_I(Neu_:Ne4_)
      real, intent(out):: H8X_I(Neu_:Ne4_)
      !------------------------------------------------------------------------
      H8X_I = exp(-X_I**2)/sqrt(cPi) + (0.5/X_I+X_I)*erf(X_I)
    end subroutine
    !==========================================================================
    subroutine h8_sub(X_I, H8X_I)
      real, intent(in):: X_I(nSubSample)
      real, intent(out):: H8X_I(nSubSample)
      !------------------------------------------------------------------------
      H8X_I = exp(-X_I**2)/sqrt(cPi) + (0.5/X_I+X_I)*erf(X_I)
    end subroutine
    !==========================================================================
    subroutine h9(X_I, H9X_I)
      real, intent(in):: X_I(Neu_:Ne4_)
      real, intent(out):: H9X_I(Neu_:Ne4_)
      !------------------------------------------------------------------------
      ! This is an approximate expression to avoid singularity at x=0
      H9X_I = sqrt(64./9./cPi + X_I**2)
    end subroutine
    !==========================================================================
    subroutine h10(X_I, H10X_I)
      real, intent(in):: X_I(Neu_:Ne4_)
      real, intent(out):: H10X_I(Neu_:Ne4_)
      !------------------------------------------------------------------------
      H10X_I = 3. + 2.*X_I**2
    end subroutine
    !==========================================================================
    subroutine h11(X_I, H11X_I)
      real, intent(in):: X_I(Neu_:Ne4_)
      real, intent(out):: H11X_I(Neu_:Ne4_)
      !------------------------------------------------------------------------
      H11X_I = (5.+2.*X_I**2)/sqrt(cPi)*exp(-X_I**2) &
           + (1.5/X_I+6.*X_I+2.*X_I**3)*erf(X_I)
    end subroutine
    !==========================================================================
    subroutine h8_scalar(X, H8X)
      real, intent(in):: X
      real, intent(out):: H8X
      !------------------------------------------------------------------------
      H8X = exp(-X**2)/sqrt(cPi) + (0.5/X+X)*erf(X)
    end subroutine
    !==========================================================================
  end subroutine calc_charge_exchange_source_pui
  !============================================================================
  subroutine calc_charge_exchange_source( &
       i,j,k,iBlock,NumDensSi_I,U_DI,U2_I,UTh2Si_I,SourceCx_V)

    use ModTurbulence, ONLY: KarmanTaylorBeta2AlphaRatio
    use ModWaves, ONLY: UseAlfvenWaves

    ! Calculate the charge exchange source terms for one cell.

    integer, intent(in):: i,j,k,iBlock
    real, dimension(nFluid), intent(in) :: NumDensSi_I, U2_I, UTh2Si_I
    real, intent(in) :: U_DI(3,nFluid)
    real, intent(out):: SourceCx_V(nVar + nFluid)

    integer :: iFluid
    real :: State_V(nVar)

    real:: uDim_DI(3,nFLuid)
    real, dimension(Neu_:Ne4_,5):: SrcLookI_II, SrcLookN_II, &
         SrcLookPu3_II, SrcLookNPu3_II

    ! For Ion/SWH
    real, dimension(Neu_:Ne4_) :: &
         URelS_I, URelSdim_I, UTh2Sum_I, UStar_I, Sigma_I, Rate_I, &
         UStarM_I, SigmaN_I, RateN_I, RateE_I, &
         I0xp_I, I0px_I, I2xp_I, I2px_I, &
         JxpUx_I, JxpUy_I, JxpUz_I, JpxUx_I, JpxUy_I, JpxUz_I, &
         Kxp_I, Kpx_I, Qepx_I, QmpxUx_I, QmpxUy_I, QmpxUz_I

    ! For PU3
    real, dimension(Neu_:Ne4_):: &
         URelSPu3_I, URelSPu3dim_I, UTh2SumPu3_I, UStarPu3_I, SigmaPu3_I, &
         RatePu3_I, UStarMPu3_I, SigmaNPu3_I, RateNPu3_I, RateEPu3_I, &
         I0xpu3_I, I0pu3x_I, I2xpu3_I, I2pu3x_I, &
         Jxpu3Ux_I, Jxpu3Uy_I, Jxpu3Uz_I, Jpu3xUx_I, Jpu3xUy_I, Jpu3xUz_I, &
         Kxpu3_I, Kpu3x_I, Qepu3x_I, Qmpu3xUx_I, Qmpu3xUy_I, Qmpu3xUz_I

    real, dimension(3,Neu_:Ne4_):: Umean_DI, UMeanPu3_DI

    real :: AlfvenSpeed, SourceTurbulence

    !--------------------------------------------------------------------------
    SourceCx_V = 0.0

    ! Initialize and set all elements
    Rate_I        = 0
    RatePu3_I     = 0
    RateN_I       = 0
    RateNPu3_I    = 0
    RateE_I       = 0
    RateEPu3_I    = 0
    Ustar_I       = 0
    UStarPu3_I    = 0
    URelS_I       = 0
    URelsdim_I    = 0
    URelSPu3_I    = 0
    URelSPu3dim_I = 0
    Sigma_I       = 0
    SigmaN_I      = 0
    SigmaPu3_I    = 0
    SigmaNPu3_I   = 0
    I0px_I        = 0
    I0xp_I        = 0
    JpxUx_I       = 0
    JpxUy_I       = 0
    JpxUz_I       = 0
    JxpUx_I       = 0
    JxpUy_I       = 0
    JxpUz_I       = 0
    Kpx_I         = 0
    Kxp_I         = 0
    I0pu3x_I        = 0
    I0xpu3_I        = 0
    Jpu3xUx_I       = 0
    Jpu3xUy_I       = 0
    Jpu3xUz_I       = 0
    Jxpu3Ux_I       = 0
    Jxpu3Uy_I       = 0
    Jxpu3Uz_I       = 0
    Kpu3x_I         = 0
    Kxpu3_I         = 0

    ! Initialize with 1 to avoid division by zero
    UStarM_I      = 1
    UStarMPu3_I   = 1
    UTh2Sum_I     = 1
    UTh2SumPu3_I  = 1

    ! Calculating the terms that enter in the Source terms
    ! I0, Jxp, Kxp, Qexp are used as in Zank, Pauls, Williams, and Hall 1996
    ! However the expressions for each are taken from McNutt et al. 1998
    ! For example for population I; Neu:
    !
    ! Source(density) = I0xpNe2 + I0xpNe3 + I0xoNe4 in region 1
    !                 = - I0pxNeu         otherwise
    !
    ! Source(momentum) = QmxpNeu + JxpNe2 + JxpNe3 + JxpNe4 in region 1
    !                  = - JpxNeu                  otherwise
    !
    ! Source(energy) = QexpNeu + KxpNe2 + KxpNe3 + KxpNe4 in region 1
    !                = -KpxNeu                   otherwise
    !
    ! 'xp' indicates neutrals-> SW protons charge exchange rates and
    ! 'px' indicates SW protons->neutrals
    ! 'xpu3' indicates neutrals -> PUI 3
    ! 'pu3x' indicates PUI3 -> neutrals
    ! charge exchange
    ! For example:
    ! I0xpNe2 is the term of creation of Neu by charge exchange p-Ne2
    ! I0xpNe3 is the term of creation of Neu by charge exchange p-Ne3

    State_V = State_VGB(:,i,j,k,iBlock)

    uDim_DI = U_DI * No2Si_V(UnitU_)

    ! Using look-up table to find source terms
    if(iTableChargeExchange > 0) then
       do iFluid = Neu_, Ne4_
          call get_collision( &
               ChargeExchange_, &
               NumDensSi_I(Ion_),UTh2Si_I(Ion_), uDIm_DI(:,Ion_), &
               NumDensSi_I(iFluid), UTh2Si_I(iFluid), uDim_DI(:,iFluid), &
               SrcLookI_II(iFluid,:),SrcLookN_II(iFluid,:))
       end do

       where(UseSource_I(Neu_:)) I0xp_I  = SrcLookI_II(Neu_:,1)
       where(UseSource_I(Neu_:)) JxpUx_I = SrcLookI_II(Neu_:,2)
       where(UseSource_I(Neu_:)) JxpUy_I = SrcLookI_II(Neu_:,3)
       where(UseSource_I(Neu_:)) JxpUz_I = SrcLookI_II(Neu_:,4)
       where(UseSource_I(Neu_:)) Kxp_I   = SrcLookI_II(Neu_:,5)

       where(UseSource_I(Neu_:)) I0px_I  = SrcLookN_II(Neu_:,1)
       where(UseSource_I(Neu_:)) JpxUx_I = SrcLookN_II(Neu_:,2)
       where(UseSource_I(Neu_:)) JpxUy_I = SrcLookN_II(Neu_:,3)
       where(UseSource_I(Neu_:)) JpxUz_I = SrcLookN_II(Neu_:,4)
       where(UseSource_I(Neu_:)) Kpx_I   = SrcLookN_II(Neu_:,5)

       QmpxUx_I = JpxUx_I - JxpUx_I
       QmpxUy_I = JpxUy_I - JxpUy_I
       QmpxUz_I = JpxUz_I - JxpUz_I

       Qepx_I = Kpx_I - Kxp_I

       if(.not.IsMhd) then
          do iFluid = Neu_, Ne4_
             call get_collision( &
                  ChargeExchange_, &
                  NumDensSi_I(Pu3_),UTh2Si_I(Pu3_), uDim_DI(:,Pu3_), &
                  NumDensSi_I(iFluid), UTh2Si_I(iFluid), uDIm_DI(:,iFluid), &
                  SrcLookPu3_II(iFluid,:),SrcLookNPu3_II(iFluid,:))
          end do

          where(UseSource_I(Neu_:)) I0xpu3_I  = SrcLookPu3_II(Neu_:,1)
          where(UseSource_I(Neu_:)) Jxpu3Ux_I = SrcLookPu3_II(Neu_:,2)
          where(UseSource_I(Neu_:)) Jxpu3Uy_I = SrcLookPu3_II(Neu_:,3)
          where(UseSource_I(Neu_:)) Jxpu3Uz_I = SrcLookPu3_II(Neu_:,4)
          where(UseSource_I(Neu_:)) Kxpu3_I   = SrcLookPu3_II(Neu_:,5)

          where(UseSource_I(Neu_:)) I0pu3x_I  = SrcLookNPu3_II(Neu_:,1)
          where(UseSource_I(Neu_:)) Jpu3xUx_I = SrcLookNPu3_II(Neu_:,2)
          where(UseSource_I(Neu_:)) Jpu3xUy_I = SrcLookNPu3_II(Neu_:,3)
          where(UseSource_I(Neu_:)) Jpu3xUz_I = SrcLookNPu3_II(Neu_:,4)
          where(UseSource_I(Neu_:)) Kpu3x_I   = SrcLookNPu3_II(Neu_:,5)

          Qmpu3xUx_I = Jpu3xUx_I - Jxpu3Ux_I
          Qmpu3xUy_I = Jpu3xUy_I - Jxpu3Uy_I
          Qmpu3xUz_I = Jpu3xUz_I - Jxpu3Uz_I

          Qepu3x_I = Kpu3x_I - Kxpu3_I

       end if

    else
       ! Using analytic formulas to find source terms

       ! Relative velocity between neutrals and SW fluid squared
       where(UseSource_I(Neu_:)) &
            URelS_I = (U_DI(x_,Neu_:) - U_DI(x_,Ion_))**2 &
            + (U_DI(y_,Neu_:) - U_DI(y_,Ion_))**2 &
            + (U_DI(z_,Neu_:) - U_DI(z_,Ion_))**2
       URelSDim_I = URelS_I * No2Si_V(UnitU_)**2

       ! Sum of thermal speeds squared for SW fluid and neutral fluid
       where(UseSource_I(Neu_:)) &
            UTh2Sum_I = UTh2Si_I(SWH_) + UTh2Si_I(Neu_:)

       if(.not.IsMhd)then
          ! for Pu3
          ! Relative velocity between neutrals and PUI fluid squared
          where(UseSource_I(Neu_:)) &
               URelSPu3_I = (U_DI(x_,Neu_:) - U_DI(x_,Pu3_))**2 &
               + (U_DI(y_,Neu_:) - U_DI(y_,Pu3_))**2 &
               + (U_DI(z_,Neu_:) - U_DI(z_,Pu3_))**2
          URelSPu3Dim_I = URelSPu3_I * No2Si_V(UNitU_)**2

          ! Sum of thermal speeds for PUI fluid and neutral fluid
          where(UseSource_I(Neu_:)) &
               UTh2SumPu3_I = UTh2Si_I(Pu3_) + UTh2Si_I(Neu_:)
       end if

       ! Calculating Cross Section Sigma_I for the different neutrals

       ! Eq. (62) of McNutt et al. 1998 for U* [m/s]

       ! For SW
       where(UseSource_I(Neu_:)) &
            UStar_I = sqrt(URelSdim_I &
            + (4./cPi)*UTh2Sum_I)

       if(.not.IsMhd)then
          ! For Pu3
          where(UseSource_I(Neu_:)) &
               UStarPu3_I = sqrt(UrelSPu3dim_I &
               + (4./cPi)*UTh2SumPu3_I)
       end if

       ! Eq. (64) of McNutt et al. 1998 for UM*

       ! For SW
       where(UseSource_I(Neu_:)) &
            UStarM_I = sqrt(URelSdim_I &
            + (64./(9.*cPi))*UTh2Sum_I)

       if(.not.IsMhd)then
          ! For Pu3
          where(UseSource_I(Neu_:)) &
               UStarMPu3_I = sqrt(UrelSPu3dim_I &
               + (64./(9.*cPi))*UTh2SumPu3_I)
       end if

       ! Ustar has units of cm/s so the factor 100 si to convert m to cm
       ! Sigma has units of m^2

       ! For SW
       ! Cross Section is by default Lindsay Stebbings but can be changed
       where(UseSource_I(Neu_:)) &
            Sigma_I = ((CrossA1 - CrossA2*log(UStarM_I*100.))**2)*1.E-4
       where(UseSource_I(Neu_:)) &
            SigmaN_I = ((CrossA1 - CrossA2*log(UStar_I*100.))**2)*1.E-4

       if(.not.IsMhd)then
          ! For Pu3
          where(UseSource_I(Neu_:)) &
               SigmaPu3_I = &
               ((CrossA1 - CrossA2*log(UStarMPu3_I*100.))**2)*(1.E-4)
          where(UseSource_I(Neu_:)) &
               SigmaNPu3_I = &
               ((CrossA1 - CrossA2*log(UStarPu3_I*100.))**2)*(1.E-4)
       end if

       ! Calculate Rate = \nu * nH * mp where nH is the neutral density
       ! \nu = Sigma*np*u_star where np is the ion density and
       ! For each population of neutrals there will be another Rate
       ! The charge xchange cross section 100 to change ustar to cm/s
       ! Rate has no units (m^2*m/s*s*m-3 )

       ! For SW
       where(UseSource_I(Neu_:)) &
            Rate_I = Sigma_I*State_V(SWHRho_) &
            *State_V(iRho_I(Neu_:))*UStarM_I &
            *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)

       if(.not.IsMhd)then
          ! For PU3
          where(UseSource_I(Neu_:)) &
               RatePu3_I = SigmaPu3_I*State_V(PU3Rho_) &
               *State_V(iRho_I(Neu_:))*UStarMPu3_I &
               *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)
       end if

       ! for SW
       where(UseSource_I(Neu_:)) &
            RateN_I = SigmaN_I*State_V(SWHRho_)&
            *State_V(iRho_I(Neu_:))*UStar_I&
            *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)

       if(.not.IsMhd)then
          ! For PU3
          where(UseSource_I(Neu_:)) &
               RateNPu3_I = SigmaNPu3_I*State_V(PU3Rho_)&
               *State_V(iRho_I(Neu_:))*UStarPu3_I &
               *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)
       end if

       ! for SW
       where(UseSource_I(Neu_:)) &
            RateE_I = Sigma_I*State_V(SWHRho_)&
            *State_V(iRho_I(Neu_:))*UStar_I&
            *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)

       if(.not.IsMhd)then
          ! For PU3
          where(UseSource_I(Neu_:)) &
               RateEPu3_I = SigmaPu3_I*State_V(PU3Rho_)&
               *State_V(iRho_I(Neu_:))*UStarPu3_I &
               *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)
       end if

       ! For SW
       I0xp_I = RateN_I
       I0px_I = RateN_I

       I2xp_I  = RateE_I*UTh2Si_I(Ion_)/No2Si_V(UnitU_)**2
       I2px_I  = RateE_I(Neu_:)*UTh2Si_I(Neu_:)/No2Si_V(UnitU_)**2

       if(.not.IsMhd)then
          ! For PUI's
          I0xpu3_I = RateNPu3_I
          I0pu3x_I = RateNPu3_I
          I2xpu3_I = RateEPu3_I*UTh2Si_I(PU3_)/&
               No2Si_V(UnitU_)**2
          I2pu3x_I = RateEPu3_I(Neu_:)*UTh2Si_I(Neu_:)/No2Si_V(UnitU_)**2
          ! units are fine: (Uth2/ustar)*termxp is unitless as it should be
       end if

       ! For SW
       JxpUx_I = U_DI(x_,Ion_)*Rate_I
       JxpUy_I = U_DI(y_,Ion_)*Rate_I
       JxpUz_I = U_DI(z_,Ion_)*Rate_I

       JpxUx_I = U_DI(x_,Neu_:)*Rate_I
       JpxUy_I = U_DI(y_,Neu_:)*Rate_I
       JpxUz_I = U_DI(z_,Neu_:)*Rate_I

       if(.not.IsMhd)then
          ! For PU3
          Jxpu3Ux_I = U_DI(x_,PU3_)*RatePu3_I
          Jxpu3Uy_I = U_DI(y_,PU3_)*RatePu3_I
          Jxpu3Uz_I = U_DI(z_,PU3_)*RatePu3_I

          Jpu3xUx_I = U_DI(x_,Neu_:)*RatePu3_I
          Jpu3xUy_I = U_DI(y_,Neu_:)*RatePu3_I
          Jpu3xUz_I = U_DI(z_,Neu_:)*RatePu3_I
       end if

       ! this is for neutrals, which can be created or destroyed
       ! QmpxUx_I = JpxUx_I - JxpUx_I
       ! QmpxUy_I = JpxUy_I - JxpUy_I
       ! QmpxUz_I = JpxUz_I - JxpUz_I

       ! For SW
       QmpxUx_I = JpxUx_I - JxpUx_I
       QmpxUy_I = JpxUy_I - JxpUy_I
       QmpxUz_I = JpxUz_I - JxpUz_I

       if(.not.IsMhd)then
          Qmpu3xUx_I = Jpu3xUx_I - Jxpu3Ux_I
          Qmpu3xUy_I = Jpu3xUy_I - Jxpu3Uy_I
          Qmpu3xUz_I = Jpu3xUz_I - Jxpu3Uz_I
       end if

       ! For SW or Ion
       Kxp_I = 0.5*U2_I(Ion_)*Rate_I + I2xp_I
       Kpx_I = 0.5*U2_I(Neu_:)*Rate_I + I2px_I
       Qepx_I = Kpx_I - Kxp_I

       if(.not.IsMhd)then
          ! For PU3
          Kxpu3_I = 0.5*U2_I(Pu3_)*RatePu3_I + I2xpu3_I
          Kpu3x_I = 0.5*U2_I(Neu_:)*RatePu3_I + I2pu3x_I
          Qepu3x_I = Kpu3x_I - Kxpu3_I
       end if

       ! Additional energy and momentum terms from having multiple fluids.
       ! The Qm and Qe variables do not need to be corrected because the
       ! additional terms cancel.
       if(DoFixChargeExchange)then
          ! Mean ion and neutral velocity weighted by sound speed squared
          UMean_DI(x_,:) = (UTh2Si_I(SWH_)*U_DI(x_,Neu_:) &
               + UTh2Si_I(Neu_:)*U_DI(x_,SWH_))/UTh2Sum_I
          UMean_DI(y_,:) = (UTh2Si_I(SWH_)*U_DI(y_,Neu_:) &
               + UTh2Si_I(Neu_:)*U_DI(y_,SWH_))/UTh2Sum_I
          UMean_DI(z_,:) = (UTh2Si_I(SWH_)*U_DI(z_,Neu_:) &
               + UTh2Si_I(Neu_:)*U_DI(z_,SWH_))/UTh2Sum_I

          ! Momentum
          JxpUx_I = JxpUx_I + UMean_DI(x_,:)*(RateN_I - Rate_I)
          JxpUy_I = JxpUy_I + UMean_DI(y_,:)*(RateN_I - Rate_I)
          JxpUz_I = JxpUz_I + UMean_DI(z_,:)*(RateN_I - Rate_I)

          JpxUx_I = JpxUx_I + UMean_DI(x_,:)*(RateN_I - Rate_I)
          JpxUy_I = JpxUy_I + UMean_DI(y_,:)*(RateN_I - Rate_I)
          JpxUz_I = JpxUz_I + UMean_DI(z_,:)*(RateN_I - Rate_I)

          ! Energy
          Kxp_I = Kxp_I + 0.5*Sum(UMean_DI**2, 1)*(RateN_I - Rate_I)&
               + (0.75*RateN_I - RateE_I)*UTh2Si_I(SWH_)*UTh2Si_I(Neu_:)/&
               UTh2Sum_I/No2Si_V(UnitU_)**2
          Kpx_I = Kpx_I + 0.5*Sum(UMean_DI**2, 1)*(RateN_I - Rate_I)&
               + (0.75*RateN_I - RateE_I)*UTh2Si_I(SWH_)*UTh2Si_I(Neu_:)/&
               UTh2Sum_I/No2Si_V(UnitU_)**2

          ! For PUIs
          if(.not.IsMhd)then
             UMeanPu3_DI(x_,:) = &
                  ( UTh2Si_I(Pu3_)*U_DI(x_,Neu_:) &
                  + UTh2Si_I(Neu_:)*U_DI(x_,Pu3_) )/UTh2SumPu3_I
             UMeanPu3_DI(y_,:) = &
                  ( UTh2Si_I(Pu3_)*U_DI(y_,Neu_:) &
                  + UTh2Si_I(Neu_:)*U_DI(y_,Pu3_) )/UTh2SumPu3_I
             UMeanPu3_DI(z_,:) = &
                  ( UTh2Si_I(Pu3_)*U_DI(z_,Neu_:) &
                  + UTh2Si_I(Neu_:)*U_DI(z_,Pu3_) )/UTh2SumPu3_I

             ! Momentum
             Jxpu3Ux_I = Jxpu3Ux_I + UMeanPu3_DI(x_,:)&
                  *(RateNPu3_I - RatePu3_I)
             Jxpu3Uy_I = Jxpu3Uy_I + UMeanPu3_DI(y_,:)&
                  *(RateNPu3_I - RatePu3_I)
             Jxpu3Uz_I = Jxpu3Uz_I + UMeanPu3_DI(z_,:)&
                  *(RateNPu3_I - RatePu3_I)

             Jpu3xUx_I = Jpu3xUx_I + UMeanPu3_DI(x_,:)&
                  *(RateNPu3_I - RatePu3_I)
             Jpu3xUy_I = Jpu3xUy_I + UMeanPu3_DI(y_,:)&
                  *(RateNPu3_I - RatePu3_I)
             Jpu3xUz_I = Jpu3xUz_I + UMeanPu3_DI(z_,:)&
                  *(RateNPu3_I - RatePu3_I)

             ! Energy
             Kxpu3_I = Kxpu3_I &
                  + 0.5*Sum(UMeanPu3_DI**2, 1)&
                  *(RateNPu3_I - RatePu3_I)&
                  + (0.75*RateNPu3_I - RateEPu3_I)&
                  *UTh2Si_I(Pu3_)*UTh2Si_I(Neu_:)/&
                  UTh2SumPu3_I/No2Si_V(UnitU_)**2
             Kpu3x_I = Kpu3x_I &
                  + 0.5*Sum(UMeanPu3_DI**2, 1)&
                  *(RateNPu3_I - RatePu3_I)&
                  + (0.75*RateNPu3_I - RateEPu3_I)*&
                  UTh2Si_I(Pu3_)*UTh2Si_I(Neu_:)/&
                  UTh2SumPu3_I/No2Si_V(UnitU_)**2
          end if
       end if
    end if

    if(UseColdCloud)then
       if(UseSrcsInHelio)then
          !! for studies of heliosphere encountering Cold Cloud
          if(r_GB(i,j,k,iBlock) < HpLimit) then
             I0xp_I(Ne4_) = 0.0
             I0px_I(Ne4_) = 0.0
             JpxUx_I(Ne4_) = 0.0
             JpxUy_I(Ne4_) = 0.0
             JpxUz_I(Ne4_) = 0.0
             JxpUx_I(Ne4_) = 0.0
             JxpUy_I(Ne4_) = 0.0
             JxpUz_I(Ne4_) = 0.0
             QmpxUx_I(Ne4_) = 0.0
             QmpxUy_I(Ne4_) = 0.0
             QmpxUz_I(Ne4_) = 0.0
             Kxp_I(Ne4_) = 0.0
             Kpx_I(Ne4_) = 0.0
             Qepx_I(Ne4_) = 0.0
             I0xp_I(Neu_) = 0.0
             I0px_I(Neu_) = 0.0
             JpxUx_I(Neu_) = 0.0
             JpxUy_I(Neu_) = 0.0
             JpxUz_I(Neu_) = 0.0
             JxpUx_I(Neu_) = 0.0
             JxpUy_I(Neu_) = 0.0
             JxpUz_I(Neu_) = 0.0
             QmpxUx_I(Neu_) = 0.0
             QmpxUy_I(Neu_) = 0.0
             QmpxUz_I(Neu_) = 0.0
             Kxp_I(Neu_) = 0.0
             Kpx_I(Neu_) = 0.0
             Qepx_I(Neu_) = 0.0
          endif
       endif
    end if

    ! Neutral source terms
    do iFluid = Neu_, Ne4_
       if(.not.UseSource_I(iFluid)) CYCLE
       call select_fluid(iFluid)
       if (iFluid == iFluidProduced_C(i,j,k)) then
          ! iRho etc = change in density etc
          if(IsMhd)then
             ! Single Ion
             SourceCx_V(iRho)    = sum(I0xp_I)  - I0px_I(iFluid)
             SourceCx_V(iRhoUx)  = sum(JxpUx_I) - JpxUx_I(iFluid)
             SourceCx_V(iRhoUy)  = sum(JxpUy_I) - JpxUy_I(iFluid)
             SourceCx_V(iRhoUz)  = sum(JxpUz_I) - JpxUz_I(iFluid)
             SourceCx_V(iEnergy) = sum(Kxp_I)   - Kpx_I(iFluid)
          else
             ! Multi ion
             SourceCx_V(iRho)    = sum(I0xp_I) + sum(I0pu3x_I) &
                  - I0xp_I(iFluid) - I0xpu3_I(iFluid)
             SourceCx_V(iRhoUx)  = sum(JxpUx_I) + sum(Jxpu3Ux_I) &
                  - JpxUx_I(iFluid) - Jpu3xUx_I(iFluid)
             SourceCx_V(iRhoUy)  = sum(JxpUy_I) + sum(Jxpu3Uy_I) &
                  - JpxUy_I(iFluid) - Jpu3xUy_I(iFluid)
             SourceCx_V(iRhoUz)  = sum(JxpUz_I) + sum(Jxpu3Uz_I) &
                  - JpxUz_I(iFluid) - Jpu3xUz_I(iFluid)
             SourceCx_V(iEnergy) = sum(Kxp_I) + sum(Kxpu3_I) &
                  - Kpx_I(iFluid) - Kpu3x_I(iFluid)
          end if
       else
          if(IsMhd)then
             ! Single Ion
             SourceCx_V(iRho)    = -I0px_I(iFluid)
             SourceCx_V(iRhoUx)  = -JpxUx_I(iFluid)
             SourceCx_V(iRhoUy)  = -JpxUy_I(iFluid)
             SourceCx_V(iRhoUz)  = -JpxUz_I(iFluid)
             SourceCx_V(iEnergy) = -Kpx_I(iFluid)
          else
             ! Multi Ion
             SourceCx_V(iRho)    = -I0px_I(iFluid)  - I0pu3x_I(iFluid)
             SourceCx_V(iRhoUx)  = -JpxUx_I(iFluid) - Jpu3xUx_I(iFluid)
             SourceCx_V(iRhoUy)  = -JpxUy_I(iFluid) - Jpu3xUy_I(iFluid)
             SourceCx_V(iRhoUz)  = -JpxUz_I(iFluid) - Jpu3xUz_I(iFluid)
             SourceCx_V(iEnergy) = -Kpx_I(iFluid)   - Kpu3x_I(iFluid)
          end if
       end if
       SourceCx_V(iP) = (Gamma-1)* ( SourceCx_V(iEnergy) &
            - sum(U_DI(:,iFluid)*SourceCx_V(iRhoUx:iRhoUz)) &
            + 0.5*U2_I(iFluid)*SourceCx_V(iRho) )
    end do

    ! Ion source terms
    if(IsMhd)then
       ! Single Ion
       if(UseSource_I(Ion_)) then
          SourceCx_V(RhoUx_)  = sum(QmpxUx_I)
          SourceCx_V(RhoUy_)  = sum(QmpxUy_I)
          SourceCx_V(RhoUz_)  = sum(QmpxUz_I)
          SourceCx_V(Energy_) = sum(Qepx_I)
          SourceCx_V(p_) = (Gamma-1)* ( SourceCx_V(Energy_) &
               - sum(U_DI(:,Ion_)*SourceCx_V(RhoUx_:RhoUz_)) &
               + 0.5*U2_I(Ion_)*SourceCx_V(Rho_) )
       end if
    else
       ! Multi Ion
       if(UseSource_I(SWH_) .and. UseSource_I(Pu3_))then
          ! Region 3: only make Pu3 in region before TS
          if (Ne3_ == iFluidProduced_C(i,j,k)) then
             ! in Pop III region
             SourceCx_V(SWHRho_)    = -sum(I0px_I) &
                  + I0px_I(Ne3_) + I0xpu3_I(Ne3_)
             SourceCx_V(SWHRhoUx_)  = -sum(JxpUx_I) &
                  + JpxUx_I(Ne3_) + Jpu3xUx_I(Ne3_)
             SourceCx_V(SWHRhoUy_)  = -sum(JxpUy_I) &
                  + JpxUy_I(Ne3_) + Jpu3xUy_I(Ne3_)
             SourceCx_V(SWHRhoUz_)  = -sum(JxpUz_I) &
                  + JpxUz_I(Ne3_) + Jpu3xUz_I(Ne3_)
             SourceCx_V(SWHEnergy_) = -sum(Kxp_I) &
                  + Kpx_I(Ne3_) + Kpu3x_I(Ne3_)
             SourceCx_V(SWHp_) = (Gamma-1)* ( SourceCx_V(SWHEnergy_) &
                  - sum(U_DI(:,SWH_)*SourceCx_V(SWHRhoUx_:SWHRhoUz_)) &
                  + 0.5*U2_I(SWH_)*SourceCx_V(SWHRho_) )

             SourceCx_V(Pu3Rho_) = sum(I0px_I) &
                  - I0px_I(Ne3_) - I0xpu3_I(Ne3_)
             SourceCx_V(Pu3RhoUx_) = sum(Qmpu3xUx_I) + sum(JpxUx_I) &
                  - JpxUx_I(Ne3_) - Jpu3xUx_I(Ne3_)
             SourceCx_V(Pu3RhoUy_) = sum(Qmpu3xUy_I) + sum(JpxUy_I) &
                  - JpxUy_I(Ne3_) -Jpu3xUy_I(Ne3_)
             SourceCx_V(Pu3RhoUz_) = sum(Qmpu3xUz_I) + sum(JpxUz_I) &
                  - JpxUz_I(Ne3_)- Jpu3xUz_I(Ne3_)
             SourceCx_V(Pu3Energy_)= sum(Qepu3x_I) + sum(Kpx_I) &
                  - Kpu3x_I(Ne3_) - Kpx_I(Ne3_)

             if(UseAlfvenWaves)then
                if(iTableChargeExchange > 0) then
                   ! relative velocities are not yet calculated
                   where(UseSource_I(Neu_:)) &
			URelS_I = (U_DI(x_,Neu_:) - U_DI(x_,Ion_))**2 &
                        + (U_DI(y_,Neu_:) - U_DI(y_,Ion_))**2 &
                        + (U_DI(z_,Neu_:) - U_DI(z_,Ion_))**2
		end if

                AlfvenSpeed = sqrt(sum(State_V(Bx_:Bz_)**2)/State_V(Rho_))
                SourceTurbulence = 0.5*TurbulencePerPu3Source*AlfvenSpeed*( &
                     (I0px_I(Neu_) + I0xpu3_I(Neu_))*sqrt(URelS_I(Neu_)) + &
                     (I0px_I(Ne2_) + I0xpu3_I(Ne2_))*sqrt(URelS_I(Ne2_)) + &
                     (I0px_I(Ne4_) + I0xpu3_I(Ne4_))*sqrt(URelS_I(Ne4_)) )

                SourceCx_V(Pu3Energy_) = SourceCx_V(Pu3Energy_) &
                     - SourceTurbulence
                SourceCx_V(WaveFirst_:WaveLast_) = 0.5*SourceTurbulence

                if(Lperp_ > 1) SourceCx_V(Lperp_) = &
                     -KarmanTaylorBeta2AlphaRatio &
                     *State_V(Lperp_)*SourceTurbulence &
                     /max(1e-30,sum(State_V(WaveFirst_:WaveLast_)))

                ! PUI-turbulence sinks for L_- and L_+ only
                if(LcorrFirst_ > 1) &
                     SourceCx_V(LcorrFirst_:LcorrFirst_+nWave-1) = &
                     -KarmanTaylorBeta2AlphaRatio &
                     *State_V(LcorrFirst_:LcorrFirst_+nWave-1) &
                     *0.5*SourceTurbulence &
                     /max(1e-30,State_V(WaveFirst_:WaveLast_))
             end if

             SourceCx_V(Pu3P_) = (Gamma-1)* ( SourceCx_V(Pu3Energy_) &
                  - sum(U_DI(:,Pu3_)*SourceCx_V(Pu3RhoUx_:Pu3RhoUz_)) &
                  + 0.5*U2_I(Pu3_)*SourceCx_V(Pu3Rho_) )

             ! end of Region 3
          else
             SourceCx_V(SWHRho_) = sum(I0xpu3_I)
             SourceCx_V(SWHRhoUx_) = sum(QmpxUx_I) + sum(Jpu3xUx_I)
             SourceCx_V(SWHRhoUy_) = sum(QmpxUy_I) + sum(Jpu3xUy_I)
             SourceCx_V(SWHRhoUz_) = sum(QmpxUz_I) + sum(Jpu3xUz_I)
             SourceCx_V(SWHEnergy_)= sum(Qepx_I)+ sum(Kpu3x_I)
             SourceCx_V(SWHp_) = (Gamma-1)* ( SourceCx_V(SWHEnergy_) &
                  - sum(U_DI(:,SWH_)*SourceCx_V(SWHRhoUx_:SWHRhoUz_)) &
                  + 0.5*U2_I(SWH_)*SourceCx_V(SWHRho_) )
             SourceCx_V(Pu3Rho_)   = -sum(I0xpu3_I)
             SourceCx_V(Pu3RhoUx_) = -sum(Jxpu3Ux_I)
             SourceCx_V(Pu3RhoUy_) = -sum(Jxpu3Uy_I)
             SourceCx_V(Pu3RhoUz_) = -sum(Jxpu3Uz_I)
             SourceCx_V(Pu3Energy_)= -sum(Kxpu3_I)
             SourceCx_V(Pu3P_) = (Gamma-1)* ( SourceCx_V(Pu3Energy_) &
                  - sum(U_DI(:,Pu3_)*SourceCx_V(Pu3RhoUx_:Pu3RhoUz_)) &
                  + 0.5*U2_I(Pu3_)*SourceCx_V(Pu3Rho_))
          end if
       end if
    end if

  end subroutine calc_charge_exchange_source
  !============================================================================
  subroutine calc_photoion_source( &
       i,j,k,iBlock,U_DI,U2_I,SourcePh_V)

    use ModTurbulence, ONLY: KarmanTaylorBeta2AlphaRatio
    use ModWaves, ONLY: UseAlfvenWaves

    ! Calculate the photoionization source terms for one cell.

    integer, intent(in):: i,j,k,iBlock
    real, intent(in):: U_DI(3,nFluid)
    real, intent(in) :: U2_I(nFluid)
    real, intent(out):: SourcePh_V(nVar + nFluid)

    integer :: iFluid
    real :: r
    real, dimension(Neu_:Ne4_):: RatePh_I, I0xpPh_I, JxpUxPh_I, JxpUyPh_I, &
         JxpUzPh_I, ExpPh_I, KxpPh_I, URelS_I
    real :: State_V(nVar)

    real :: AlfvenSpeed, SourceTurbulence

    real :: PhotoIonRate

    ! Chika. Photoionization rate
    character(len=*), parameter:: NameSub = 'calc_photoion_source'
    !--------------------------------------------------------------------------
    SourcePh_V = 0.0

    RatePh_I = 0.0

    State_V = State_VGB(:,i,j,k,iBlock)
    r = r_GB(i,j,k,iBlock)
    call get_photoionization_rate_si(r, PhotoIonRate)

    ! rate_ph = 8E-8*(r0/r)**2
    ! 8E-8 has units 1/s
    ! r0 = 1 AU
    where(UseSource_I(Neu_:)) &
         RatePh_I = PhotoIonRate*State_V(iRho_I(Neu_:))*No2Si_V(UnitT_)

    ! Chika: Source terms for single ion
    ! Density source term for photoionization
    ! Fahr et al. 2000 Table 2
    I0xpPh_I = RatePh_I

    ! Momentum source term for neutrals
    ! Table 2 Fahr et al. 2000
    JxpUxPh_I = U_DI(x_,Neu_:)*RatePh_I
    JxpUyPh_I = U_DI(y_,Neu_:)*RatePh_I
    JxpUzPh_I = U_DI(z_,Neu_:)*RatePh_I

    ! Energy source term
    ! Defining first this "energy" term for sw/ion and neutral species.
    ! Has units of (m/s)^2.
    ! Dividing out density so that energy source terms KxpPh_I
    ! syntax is consistent with momentum and density source
    ! term definitions.
    ! Equation 4 Fahr et al. 2000

    ExpPh_I = 0.5*U2_I(Neu_:) + State_V(iP_I(Neu_:))/&
         State_V(iRho_I(Neu_:))*InvGammaMinus1_I(Neu_:)

    ! Chika: Energy source term
    ! Table 2 Fahr et al. 2000
    KxpPh_I = RatePh_I*ExpPh_I

    if(UseNeutralFluid)then
       do iFluid = Neu_, Ne4_
          if(.not.UseSource_I(iFluid)) CYCLE
          call select_fluid(iFluid)
          SourcePh_V(iRho)    = -I0xpPh_I(iFluid)
          SourcePh_V(iRhoUx)  = -JxpUxPh_I(iFluid)
          SourcePh_V(iRhoUy)  = -JxpUyPh_I(iFluid)
          SourcePh_V(iRhoUz)  = -JxpUzPh_I(iFluid)
          SourcePh_V(iEnergy) = -KxpPh_I(iFluid)

          SourcePh_V(iP) = GammaMinus1_I(iFluid)* ( SourcePh_V(iEnergy) &
               - sum(U_DI(:,iFluid)*SourcePh_V(iRhoUx:iRhoUz)) &
               + 0.5*U2_I(iFluid)*SourcePh_V(iRho) )
       end do
    end if

    ! Ion source terms
    if(IsMhd)then
       ! Single Ion
       if(UseSource_I(Ion_))then
          SourcePh_V(Rho_)    = sum(I0xpPh_I)
          SourcePh_V(RhoUx_)  = sum(JxpUxPh_I)
          SourcePh_V(RhoUy_)  = sum(JxpUyPh_I)
          SourcePh_V(RhoUz_)  = sum(JxpUzPh_I)
          SourcePh_V(Energy_) = sum(KxpPh_I)
          SourcePh_V(p_) = GammaMinus1* ( SourcePh_V(Energy_) &
               - sum(U_DI(:,Ion_)*SourcePh_V(RhoUx_:RhoUz_)) &
               + 0.5*U2_I(Ion_)*SourcePh_V(Rho_) )
       end if
    else
       ! Multi Ion
       if(UseSource_I(SWH_) .and. UseSource_I(Pu3_))then
          ! Region 3: only make Pu3 in region before TS
          if(Ne3_ == iFluidProduced_C(i,j,k))then
             ! in Pop III region
             ! a source gamma*pe/ne*qphoto seems to be missing in
             ! electron pressure equation. Probably, need to be removed from SW
	     SourcePh_V(SWHRho_)    = I0xpPh_I(Ne3_)
             SourcePh_V(SWHRhoUx_)  = JxpUxPh_I(Ne3_)
             SourcePh_V(SWHRhoUy_)  = JxpUyPh_I(Ne3_)
             SourcePh_V(SWHRhoUz_)  = JxpUzPh_I(Ne3_)
             SourcePh_V(SWHEnergy_) = KxpPh_I(Ne3_)
             SourcePh_V(SWHp_) = GammaMinus1_I(SWH_)*( SourcePh_V(SWHEnergy_) &
                  - sum(U_DI(:,SWH_)*SourcePh_V(SWHRhoUx_:SWHRhoUz_)) &
                  + 0.5*U2_I(SWH_)*SourcePh_V(SWHRho_) )

             SourcePh_V(Pu3Rho_)   = sum(I0xpPh_I) - I0xpPh_I(Ne3_)
             SourcePh_V(Pu3RhoUx_) = sum(JxpUxPh_I) - JxpUxPh_I(Ne3_)
             SourcePh_V(Pu3RhoUy_) = sum(JxpUyPh_I) - JxpUyPh_I(Ne3_)
             SourcePh_V(Pu3RhoUz_) = sum(JxpUzPh_I) - JxpUzPh_I(Ne3_)
             SourcePh_V(Pu3Energy_)= sum(KxpPh_I) - KxpPh_I(Ne3_)

             if(UseAlfvenWaves)then
                URelS_I = 0
                where(UseSource_I(Neu_:)) &
                     URelS_I = (U_DI(x_,Neu_:) - U_DI(x_,Ion_))**2 &
                     + (U_DI(y_,Neu_:) - U_DI(y_,Ion_))**2 &
                     + (U_DI(z_,Neu_:) - U_DI(z_,Ion_))**2

                AlfvenSpeed = sqrt(sum(State_V(Bx_:Bz_)**2)/State_V(Rho_))

                SourceTurbulence = 0.5*TurbulencePerPu3Source*AlfvenSpeed*( &
                     I0xpPh_I(Neu_)*sqrt(URelS_I(Neu_)) + &
                     I0xpPh_I(Ne2_)*sqrt(URelS_I(Ne2_)) + &
                     I0xpPh_I(Ne4_)*sqrt(URelS_I(Ne4_)) )

                SourcePh_V(Pu3Energy_) = SourcePh_V(Pu3Energy_) &
                     - SourceTurbulence
                SourcePh_V(WaveFirst_:WaveLast_) = 0.5*SourceTurbulence

                if(Lperp_ > 1) SourcePh_V(Lperp_) = &
                     -KarmanTaylorBeta2AlphaRatio &
                     *State_V(Lperp_)*SourceTurbulence &
                     /max(1e-30,sum(State_V(WaveFirst_:WaveLast_)))

                ! PUI-turbulence sinks for L_- and L_+ only
                if(LcorrFirst_ > 1) &
                     SourcePh_V(LcorrFirst_:LcorrFirst_+nWave-1) = &
                     -KarmanTaylorBeta2AlphaRatio &
                     *State_V(LcorrFirst_:LcorrFirst_+nWave-1) &
                     *0.5*SourceTurbulence &
                     /max(1e-30,State_V(WaveFirst_:WaveLast_))
             end if

             SourcePh_V(Pu3P_) = GammaMinus1_I(Pu3_)*( SourcePh_V(Pu3Energy_) &
                  - sum(U_DI(:,Pu3_)*SourcePh_V(Pu3RhoUx_:Pu3RhoUz_)) &
                  + 0.5*U2_I(Pu3_)*SourcePh_V(Pu3Rho_) )

             ! end of Region 3
          else
             SourcePh_V(SwhRho_)    = sum(I0xpPh_I)
             SourcePh_V(SwhRhoUx_)  = sum(JxpUxPh_I)
             SourcePh_V(SwhRhoUy_)  = sum(JxpUyPh_I)
             SourcePh_V(SwhRhoUz_)  = sum(JxpUzPh_I)
             SourcePh_V(SwhEnergy_) = sum(KxpPh_I)

             SourcePh_V(SWHp_) = GammaMinus1_I(SWH_)*( SourcePh_V(SWHEnergy_) &
                  - sum(U_DI(:,SWH_)*SourcePh_V(SWHRhoUx_:SWHRhoUz_)) &
                  + 0.5*U2_I(SWH_)*SourcePh_V(SWHRho_) )
          end if
       end if
    end if

  end subroutine calc_photoion_source
  !============================================================================
  subroutine calc_electron_impact_source( &
       i,j,k,iBlock,Rho_I,U_DI,U2_I,UThS_I,SourceImp_V)

    use ModTurbulence, ONLY: KarmanTaylorBeta2AlphaRatio
    use ModWaves, ONLY: UseAlfvenWaves

    ! Calculate the electron impact source terms for one cell.
    ! Requires a separate electron pressure

    integer, intent(in):: i,j,k,iBlock
    real, intent(in):: Rho_I(nFluid), U2_I(nFluid), UThS_I(nFluid)
    real, intent(in) :: U_DI(3,nFluid)
    real, intent(out):: SourceImp_V(nVar + nFluid)

    integer :: iFluid
    real, dimension(Neu_:Ne4_):: SrcImpRho_I, SrcImpRhoUx_I, &
         SrcImpRhoUy_I, SrcImpRhoUz_I, SrcImpEnergy_I, URelS_I
    real :: uSi_DI(3,nFluid)
    real :: State_V(nVar)
    real :: RhoEl, NumDensEl, UthSElSi, RhoIonTot, NumDensElSi
    real :: UIonMean_D(3), UEl_D(3), UElSi_D(3), Current_D(3)
    real :: SrcImp_II(Neu_:Ne4_,5)

    real :: AlfvenSpeed, SourceTurbulence

    ! Requires a separate electron pressure
    character(len=*), parameter:: NameSub = 'calc_electron_impact_source'
    !--------------------------------------------------------------------------
    if (.not.UseElectronPressure) call CON_stop(NameSub// &
         ': Electron impact requires a separate electron pressure')

    SrcImpRho_I = 0.0
    SrcImpRhoUx_I = 0.0
    SrcImpRhoUy_I = 0.0
    SrcImpRhoUz_I = 0.0
    SrcImpEnergy_I = 0.0
    SourceImp_V = 0.0

    State_V = State_VGB(:,i,j,k,iBlock)

    ! uDIm_DI  = State_V(iRhoUx_I:iRhoUz_I)/State_V(iRho_I)*No2Si_V(UnitU_)
    uSi_DI = U_DI*No2Si_V(UnitU_)

    ! Total ion mass density
    RhoIonTot = sum(State_V(iRhoIon_I))

    ! Electron number density
    NumDensEl = sum(State_V(iRhoIon_I)/MassIon_I)

    ! Electron number density in SI units (#/m^3)
    NumDensElSi = NumDensEl*No2Si_V(UnitN_)

    ! Electron mass density
    RhoEl = cElectronMass/cProtonMass*NumDensEl

    ! Average ion velocity
    UIonMean_D(x_) = sum(State_V(iRhoUxIon_I))/RhoIonTot
    UIonMean_D(y_) = sum(State_V(iRhoUyIon_I))/RhoIonTot
    UIonMean_D(z_) = sum(State_V(iRhoUzIon_I))/RhoIonTot

    ! Get electron velocity from the current and positive ion velocities
    call get_current(i, j, k, iBlock, Current_D)
    UEl_D = (UIonMean_D - Current_D/NumDensEl)

    UElSi_D = UEl_D*No2Si_V(UnitU_)

    ! Electron thermal speed squared in SI units
    UThSElSi = 2*State_V(Pe_)/RhoEl*No2Si_V(UnitU_)**2

    if(iTableElectronImpact > 0)then
       do iFluid = Neu_, Ne4_
          call get_collision( &
               ElectronImpact_, &
               Rho_I(iFluid), UThS_I(iFluid), uSi_DI(:,iFluid), &
               NumDensElSi, UThSElSi, UElSi_D, &
               SrcImp_II(iFluid,:))
       enddo
    end if

    where(UseSource_I(Neu_:)) SrcImpRho_I    = SrcImp_II(Neu_:,1)
    where(UseSource_I(Neu_:)) SrcImpRhoUx_I  = SrcImp_II(Neu_:,2)
    where(UseSource_I(Neu_:)) SrcImpRhoUy_I  = SrcImp_II(Neu_:,3)
    where(UseSource_I(Neu_:)) SrcImpRhoUz_I  = SrcImp_II(Neu_:,4)
    where(UseSource_I(Neu_:)) SrcImpEnergy_I = SrcImp_II(Neu_:,5)

    ! Neutral source terms
    do iFluid = Neu_, Ne4_
       if(.not.UseSource_I(iFluid)) CYCLE
       call select_fluid(iFLuid)
       SourceImp_V(iRho)    = -SrcImpRho_I(iFluid)
       SourceImp_V(iRhoUx)  = -SrcImpRhoUx_I(iFluid)
       SourceImp_V(iRhoUy)  = -SrcImpRhoUy_I(iFluid)
       SourceImp_V(iRhoUz)  = -SrcImpRhoUz_I(iFluid)
       SourceImp_V(iEnergy) = -SrcImpEnergy_I(iFluid)

       SourceImp_V(iP) = (Gamma-1)* ( SourceImp_V(iEnergy) &
            - sum(U_DI(:,iFluid)*SourceImp_V(iRhoUx:iRhoUz)) &
            + 0.5*U2_I(iFluid)*SourceImp_V(iRho) )
    end do

    ! Plasma source terms
    if(IsMhd) then
       ! Single Ion
       if (UseSource_I(Ion_)) then
          SourceImp_V(Rho_)    = sum(SrcImpRho_I)
          SourceImp_V(RhoUx_)  = sum(SrcImpRhoUx_I)
          SourceImp_V(RhoUy_)  = sum(SrcImpRhoUy_I)
          SourceImp_V(RhoUz_)  = sum(SrcImpRhoUz_I)
          SourceImp_V(Energy_) = sum(SrcImpEnergy_I)

          SourceImp_V(P_) = GammaMinus1*( SourceImp_V(Energy_) &
               - sum(U_DI(:,Ion_)*SourceImp_V(RhoUx_:RhoUz_)) &
               + 0.5*U2_I(Ion_)*SourceImp_V(Rho_) )

          ! Electron pressure source term
          ! Ionization Energy * total ionization rate
          ! + thermal energy from new electron
          SourceImp_V(Pe_) = GammaElectronMinus1*( &
               -SourceImp_V(Rho_)*IonizationEnergy &
               + (cElectronMass/cProtonMass)* ( SourceImp_V(Energy_) &
               - sum(UEl_D*SourceImp_V(RhoUx_:RhoUz_)) &
               + 0.5*sum(UEl_D**2)*SourceImp_V(Rho_) ) )
       end if
    else
       ! Multi Ion
       if(UseSource_I(SWH_) .and. UseSource_I(Pu3_)) then
          if (Ne3_ == iFluidProduced_C(i,j,k)) then
             ! inside region 3

             ! Pop 3 still creates SWH because it is already
             ! moving as 400 km/s and does not get picked up
             SourceImp_V(SwhRho_)    = SrcImpRho_I(Ne3_)
             SourceImp_V(SwhRhoUx_)  = SrcImpRhoUx_I(Ne3_)
             SourceImp_V(SwhRhoUy_)  = SrcImpRhoUy_I(Ne3_)
             SourceImp_V(SwhRhoUz_)  = SrcImpRhoUz_I(Ne3_)
             SourceImp_V(SwhEnergy_) = SrcImpEnergy_I(Ne3_)

             SourceImp_V(SwhP_) = GammaMinus1*( SourceImp_V(SwhEnergy_) &
                  - sum(U_DI(:,Swh_)*SourceImp_V(SwhRhoUx_:SwhRhoUz_)) &
                  + 0.5*U2_I(Swh_)*SourceImp_V(SwhRho_))

             ! Pu3 is created here
             SourceImp_V(Pu3Rho_)    = sum(SrcImpRho_I) &
                  - SrcImpRho_I(Ne3_)
             SourceImp_V(Pu3RhoUx_)  = sum(SrcImpRhoUx_I) &
                  - SrcImpRhoUx_I(Ne3_)
             SourceImp_V(Pu3RhoUy_)  = sum(SrcImpRhoUy_I) &
                  - SrcImpRhoUy_I(Ne3_)
             SourceImp_V(Pu3RhoUz_)  = sum(SrcImpRhoUz_I) &
                  - SrcImpRhoUz_I(Ne3_)
             SourceImp_V(Pu3Energy_) = sum(SrcImpEnergy_I) &
                  - SrcImpEnergy_I(Ne3_)

             if(UseAlfvenWaves)then
                URelS_I = 0
                where(UseSource_I(Neu_:)) &
                     URelS_I = (U_DI(x_,Neu_:) - U_DI(x_,Ion_))**2 &
                     + (U_DI(y_,Neu_:) - U_DI(y_,Ion_))**2 &
                     + (U_DI(z_,Neu_:) - U_DI(z_,Ion_))**2

                AlfvenSpeed = sqrt(sum(State_V(Bx_:Bz_)**2)/State_V(Rho_))

                SourceTurbulence = 0.5*TurbulencePerPu3Source*AlfvenSpeed*( &
                     SrcImpRho_I(Neu_)*sqrt(URelS_I(Neu_)) + &
                     SrcImpRho_I(Ne2_)*sqrt(URelS_I(Ne2_)) + &
                     SrcImpRho_I(Ne4_)*sqrt(URelS_I(Ne4_)) )

                SourceImp_V(Pu3Energy_) = SourceImp_V(Pu3Energy_) &
                     - SourceTurbulence
                SourceImp_V(WaveFirst_:WaveLast_) = 0.5*SourceTurbulence

                if(Lperp_ > 1) SourceImp_V(Lperp_) = &
                     -KarmanTaylorBeta2AlphaRatio &
                     *State_V(Lperp_)*SourceTurbulence &
                     /max(1e-30,sum(State_V(WaveFirst_:WaveLast_)))

                ! PUI-turbulence sinks for L_- and L_+ only
                if(LcorrFirst_ > 1) &
                     SourceImp_V(LcorrFirst_:LcorrFirst_+nWave-1) = &
                     -KarmanTaylorBeta2AlphaRatio &
                     *State_V(LcorrFirst_:LcorrFirst_+nWave-1) &
                     *0.5*SourceTurbulence &
                     /max(1e-30,State_V(WaveFirst_:WaveLast_))
             end if

             SourceImp_V(Pu3P_) = GammaMinus1*( SourceImp_V(Pu3Energy_) &
                  - sum(U_DI(:,Pu3_)*SourceImp_V(Pu3RhoUx_:Pu3RhoUz_)) &
                  + 0.5*U2_I(Pu3_)*SourceImp_V(Pu3Rho_))
          else
             ! outside region 3
             ! SWH is created
             SourceImp_V(SwhRho_)    = sum(SrcImpRho_I)
             SourceImp_V(SwhRhoUx_)  = sum(SrcImpRhoUx_I)
             SourceImp_V(SwhRhoUy_)  = sum(SrcImpRhoUy_I)
             SourceImp_V(SwhRhoUz_)  = sum(SrcImpRhoUz_I)
             SourceImp_V(SwhEnergy_) = sum(SrcImpEnergy_I)

             SourceImp_V(SWHp_) = GammaMinus1*( SourceImp_V(SWHEnergy_) &
                  - sum(U_DI(:,SWH_)*SourceImp_V(SWHRhoUx_:SWHRhoUz_)) &
                  + 0.5*U2_I(SWH_)*SourceImp_V(SWHRho_) )
          end if

          ! Electron pressure source term
          ! ionization Energy * total ionization rate
          ! + thermal energy from new electron.
          ! (nVar+1:nVar+nIonFluid) corresponds to the ion energy terms
          SourceImp_V(Pe_) = GammaElectronMinus1*( &
               -sum(SourceImp_V(iRhoIon_I))*IonizationEnergy &
               + (cElectronMass/cProtonMass)&
               *( sum(SourceImp_V(nVar+1:nVar+nIonFluid)) &
               - UEl_D(x_)*sum(SourceImp_V(iRhoUxIon_I)) &
               - UEl_D(y_)*sum(SourceImp_V(iRhoUyIon_I)) &
               - UEl_D(z_)*sum(SourceImp_V(iRhoUzIon_I)) &
               + 0.5*sum(UEl_D**2)*sum(SourceImp_V(iRhoIon_I)) ) )

       end if
    end if

  end subroutine calc_electron_impact_source
  !============================================================================
  subroutine set_omega_parker_tilt

    ! Calculate angular velocity in normalized units
    ! Note: the rotation period is 25.38 days in ModConst.f90:
    ! OmegaSun = cTwoPi/(RotationPeriodSun*Si2No_V(UnitT_))
    !--------------------------------------------------------------------------
    OmegaSun   = cTwoPi/(26.0*24.0*3600.00*Si2No_V(UnitT_))
    ParkerTilt = OmegaSun*rBody/SwhUx

  end subroutine set_omega_parker_tilt
  !============================================================================
  subroutine get_region(iRegion, r, RhoDim, U2Dim, TempDim, Mach2, &
       MachPUI2, MachSW2, LevelHP, DoReIndexIn)

    ! set the global variabls iFluidProduced_C
    ! to select which neutral fluid is produced in each cell of the block

    integer,          intent(out) :: iRegion
    real,             intent(in)  :: r       ! unit: cAU
    real,             intent(in)  :: RhoDim  ! unit: amu/cm^3
    real,             intent(in)  :: U2Dim   ! unit: (km/s)^2
    real,             intent(in)  :: TempDim ! unit: K
    real,             intent(in)  :: Mach2, MachPUI2, MachSW2
    real,             intent(in)  :: LevelHP

    ! Force the indices of neutral species to 1 ... 4. It is used by FLEKS
    ! for OH-PT coupling
    logical, optional, intent(in) :: DoReIndexIn

    logical :: DoReIndex

    integer :: iNe1, iNe2, iNe3, iNe4

    character(len=*), parameter:: NameSub = 'get_region'
    !--------------------------------------------------------------------------
    DoReIndex = .false.
    if(present(DoReIndexIn)) DoReIndex = DoReIndexIn

    if(DoReIndex) then
       iNe1 = 1
       iNe2 = 2
       iNe3 = 3
       iNe4 = 4
    else
       iNe1 = Neu_
       iNe2 = Ne2_
       iNe3 = Ne3_
       iNe4 = Ne4_
    endif

    ! Apply region formulas
    select case(iRegionFormula)

    case(SingleIon_)
       if(r < rPop3Limit) then
          iRegion = iNe3
          RETURN
       end if

       if (MachPop4Limit**2 < Mach2 &
            .and. uPop1LimitDim**2 > U2Dim) then
          ! Outside the bow shock
          iRegion = iNe4
       elseif(TempDim < TempPop1LimitDim &
            .and. U2Dim < uPop1LimitDim**2)then
          ! Outside the heliopause
          iRegion = iNe1
       elseif( Mach2 < MachPop2Limit**2 )then
          ! Heliosheath
          iRegion = iNe2
       elseif( Mach2 > MachPop3Limit**2 )then
          !! before 85K      elseif( MachSW2 > MachPop3Limit**2 )then
          ! Inside termination shock
          iRegion = iNe3
       else
          ! No neutrals are produced in this region
          ! but they are destroyed
          iRegion = 0
       end if

       if (.not.IsMhd) then
          ! adding more conditions to help with the regions
          if (MachSW2 > MachPop4Limit**2 &
               .and. MachPUI2 < MachPUIPop3**2 ) then
             iRegion = iNe2
          end if
          if (MachSW2 < MachPop3Limit**2 &
               .and. MachPUI2 > MachPUIPop3**2 &
               .and. r < rPop3Limit) then
             iRegion = iNe3
          end if
       endif

    case(ColdCloud_)
       if(DoInitializeCloud)then
          if(r > rPop3Limit) then
             ! Outside the bow shock
             iRegion = iNe4
          else
             ! Outside the heliopause
             iRegion = iNe3
          endif
       else
          !! for studies of heliosphere with cold cloud
          if(r < rPop3Limit) then
             iRegion = iNe3
             RETURN
          end if

          if (TempPop1LimitDim > TempDim &
               .and. RhoPop4LimitDim > RhoDim) then
             ! Outside the bow shock
             iRegion = iNe4
          elseif (TempPop1LimitDim > TempDim &
               .and. RhoDim > RhoPop4LimitDim) then
             ! Outside the heliopause
             iRegion = iNe1
          elseif(TempDim > TempPop1LimitDim &
               .and. uPop1LimitDim**2 > U2Dim)then
             ! Heliosheath
             iRegion = iNe2
          elseif(TempDim > TempPop1LimitDim .and. &
               U2Dim > uPop1LimitDim**2)then
             ! Inside termination shock
             iRegion = iNe3
          else
             ! No neutrals are produced in this region
             ! but they are destroyed
             iRegion = 0
          end if
       endif

    case(MultiIon_)
       if (r < rBody) then
          ! inside inner boundary (inside termination shock)
          iRegion = iNe3
       elseif (U2Dim > uPop3LimitDim**2 &
            .and. Mach2 > MachPop3Limit**2) then
          ! inside termination shock
          iRegion = iNe3
       elseif (TempDim > TempPop2LimitDim) then
          ! inside heliosheath
          iRegion = iNe2
       elseif (U2Dim < uPop1LimitDim**2 &
            .and. Mach2 < MachPop1Limit**2) then
          ! inside bowshock
          iRegion = iNe1
       else
          ! outside bowshock
          iRegion = iNe4
       endif

    case(LevelSet_)
       if (r < rBody) then
          ! inside inner boundary (inside termination shock)
          iRegion = iNe3
          RETURN
       end if

       if (LevelHP < LevelHPLimit) then
          ! Outside Heliopause
          if (Mach2 > MachPop4Limit**2) then
             ! Outside bow shock
             iRegion = iNe4
          else
             ! Inside bow shock
             iRegion = iNe1
          end if
       else
          ! Inside Heliopause
          if (Mach2 < MachPop2Limit**2) then
             ! Outside termination Shock
             iRegion = iNe2
          else
             ! Inside termination Shock
             iRegion = iNe3
          end if
       end if

    case(LevelSetMi_)
       if (r < rBody) then
          ! inside inner boundary (inside termination shock)
          iRegion = iNe3
          RETURN
       end if

       if (LevelHP < LevelHPLimit) then
          ! Outside Heliopause
          if (Mach2 > MachPop4Limit**2) then
             ! Outside bow shock
             iRegion = iNe4
          else
             ! Inside bow shock
             iRegion = iNe1
          end if
       else
          ! Inside Heliopause
          if (U2Dim < uPop3LimitDim**2) then
             ! Outside termination shock
             iRegion = iNe2
          else
             ! Inside termination shock
             iRegion = iNe3
          end if
       end if

    case default
       call CON_stop(NameSub// &
            ' : unexpected region formulas.')
    end select
  end subroutine get_region
  !============================================================================
  subroutine select_region(iBlock)

    ! set the global variabls iFluidProduced_C
    ! to select which neutral fluid is produced in each cell of the block

    integer, intent(in):: iBlock

    integer :: i, j, k, iRegion

    ! cold cloud
    real :: RhoDim, InvRho, U2, p, Mach2, MachSW2, TempDim,  U2Dim, Rho
    real :: pSW, InvRhoSW, MachPUI2, pPUI, InvRhoPUI, RhoPUI, RhoSw
    real :: LevelHP

    ! Produce fluid3 at the inner boundary

    ! This subroutine is not needed when not using the 4 neutral fluids
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'select_region'
    !--------------------------------------------------------------------------
    if(.not.UseNeutralFluid) call CON_stop(NameSub// &
         ': no neutral fluids present')

    call test_start(NameSub, DoTest, iBlock)
    do k = 1, nK; do j = 1, nJ; do i = 1, nI

       if(IsMhd) then
          ! Single ion
          Rho = State_VGB(Rho_,i,j,k,iBlock)
          InvRho = 1.0/Rho
          U2 = sum(State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)**2)*InvRho**2
          if(UseElectronPressure) then
             p = State_VGB(P_,i,j,k,iBlock) + State_VGB(Pe_,i,j,k,iBlock)
          else
             p = State_VGB(P_,i,j,k,iBlock)
          endif
       else
          ! Multi ion
          Rho    = State_VGB(SWHRho_,i,j,k,iBlock) &
               +   State_VGB(Pu3Rho_,i,j,k,iBlock)
          RhoSW  = State_VGB(SWHRho_,i,j,k,iBlock)
          RhoPui = State_VGB(Pu3Rho_,i,j,k,iBLock)
          InvRho    = 1.0/Rho
          InvRhoSW  = 1.0/RhoSW
          InvRhoPUI = 1.0/RhoPui
          U2 = sum((State_VGB(SWHRhoUx_:SWHRhoUz_,i,j,k,iBlock) &
               +State_VGB(Pu3RhoUx_:Pu3RhoUz_,i,j,k,iBlock))**2) &
               *InvRho**2
          if (UseElectronPressure) then
             p = State_VGB(SWHP_,i,j,k,iBlock) &
                  + State_VGB(Pu3P_,i,j,k,iBlock) &
                  + State_VGB(Pe_,i,j,k,iBlock)
          else
             p = State_VGB(SWHP_,i,j,k,iBlock) &
                  + State_VGB(Pu3P_,i,j,k,iBlock)
          endif
          pSW  = State_VGB(SWHP_,i,j,k,iBlock)
          pPUI = State_VGB(Pu3P_,i,j,k,iBlock)

          ! Square of PUI Mach number (using total fluid speed)
          ! Zieger This Mach number has no physical meaning
          MachPUI2 = U2/(Gamma*pPUI*InvRhoPUI)

          ! Square of Solar Wind Mach number (using total fluid speed)
          ! Zieger This Mach number has no physical meaning
          MachSW2 = U2/(Gamma*pSW*InvRhoSW)
       endif

       RhoDim = Rho*No2Io_V(UnitRho_)
       U2Dim = U2*No2Io_V(UnitU_)**2
       TempDim = 0.5*p*InvRho*No2Si_V(UnitTemperature_)

       ! Square of Mach number
       Mach2 = U2/(Gamma*p*InvRho)

       ! Level Set function
       LevelHP = State_VGB(LevelHP_,i,j,k,iBlock)/Rho

       call get_region(iRegion, r_GB(i,j,k,iBlock), RhoDim, &
            U2Dim, TempDim, Mach2, MachPUI2, MachSW2, LevelHP)
       iFluidProduced_C(i,j,k) = iRegion
    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine select_region
  !============================================================================
  subroutine user_init_session

    use ModLookupTable, ONLY: i_lookup_table, get_lookup_table
    use ModWaves, ONLY: UseWavePressure, UseAlfvenWaves
    use ModVarIndexes, ONLY: WaveFirst_
    use ModTurbulence, ONLY: KarmanTaylorAlpha

    real:: IndexMax_I(2)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_init_session'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    UseAlfvenWaves  = WaveFirst_ > 1
    UseWavePressure = WaveFirst_ > 1

    DeltaU = DeltaUDim*Io2No_V(UnitU_)
    ! Our LperpTimesSqrtBSi is L_\perp*\sqrt(B)/KarmanTaylorAlpha
    LperpTimesSqrtB = LperpTimesSqrtBSi/KarmanTaylorAlpha &
         *Si2No_V(UnitX_)*sqrt(Si2No_V(UnitB_))

    ! normalization of SWH and VLISW and Neutrals

    VliswRho = VliswRhoDim*Io2No_V(UnitRho_)
    if(UseElectronPressure)then
       VliswP  = VliswTDim*Io2No_V(UnitTemperature_)*VliswRho
       VliswPe = VliswTeDim*Io2No_V(UnitTemperature_)*VliswRho
    else
       VliswP  = 2.*VliswTDim*Io2No_V(UnitTemperature_)*VliswRho
    end if
    VliswUx  = VliswUxDim*Io2No_V(UnitU_)
    VliswUy  = VliswUyDim*Io2No_V(UnitU_)
    VliswUz  = VliswUzDim*Io2No_V(UnitU_)
    VliswBx  = VliswBxDim*Io2No_V(UnitB_)
    VliswBy  = VliswByDim*Io2No_V(UnitB_)
    VliswBz  = VliswBzDim*Io2No_V(UnitB_)

    SwhRho = SwhRhoDim*Io2No_V(UnitRho_)

    SwhUx  = SwhUxDim*Io2No_V(UnitU_)
    SwhUy  = SwhUyDim*Io2No_V(UnitU_)
    SwhUz  = SwhUzDim*Io2No_V(UnitU_)
    SwhBx  = SwhBxDim*Io2No_V(UnitB_)
    SwhBy  = SwhByDim*Io2No_V(UnitB_)
    SwhBz  = SwhBzDim*Io2No_V(UnitB_)

    if(IsMhd)then
       if(UseElectronPressure)then
          SwhP   = SwhTDim*Io2No_V(UnitTemperature_)*SwhRho
          SwhPe  = SwhTeDim*Io2No_V(UnitTemperature_)*SwhRho
       else
          ! Pressure of plasma = 2*T_ion*Rho_ion
          SwhP   = 2.*SwhTDim*Io2No_V(UnitTemperature_)*SwhRho
       end if
    else
       Pu3Ux  = Pu3UxDim*Io2No_V(UnitU_)
       Pu3Uy  = Pu3UyDim*Io2No_V(UnitU_)
       Pu3Uz  = Pu3UzDim*Io2No_V(UnitU_)
       Pu3Rho = Pu3RhoDim*Io2No_V(UnitRho_)
       Pu3P   = Pu3TDim*Io2No_V(UnitTemperature_)*Pu3Rho
       if(UseElectronPressure)then
          SwhP   = SwhTDim*Io2No_V(UnitTemperature_)*SwhRho
          SwhPe  = SwhTeDim*Io2No_V(UnitTemperature_)*(SwhRho + Pu3Rho)
       else
          ! For SwhP, see eq. 1 from Opher et al. (2020)
          SwhP = (2*SwhRho + Pu3Rho)*SwhTDim*Io2No_V(UnitTemperature_)
       end if
    end if

    if(UseNeutralFluid)then
       RhoNeutralsISW = RhoNeuWindDim*Io2No_V(UnitRho_)
       pNeuWindDim = No2Io_V(UnitP_)/Gamma &
            *(RhoNeuWindDim/SwhRhoDim)*(TempNeuWindDim /SwhTDim)

       PNeutralsISW  = TempNeuWindDim*Io2No_V(UnitTemperature_)*RhoNeutralsISW
       UxNeutralsISW = UxNeuWindDim*Io2No_V(UnitU_)
       UyNeutralsISW = UyNeuWindDim*Io2No_V(UnitU_)
       UzNeutralsISW = UzNeuWindDim*Io2No_V(UnitU_)
    end if

    ! Set table index based on the table name.
    ! The table can be read by BATSRUS or the AMPS Fortran wrapper.
    if(iTableChargeExchange < 0) &
         iTableChargeExchange = i_lookup_table('ChargeExchange')

    ! Set table index for iTableSolarWind with table name solarwind2d
    if(iTableSolarWind < 0)then
       iTableSolarWind=i_lookup_table('solarwind2d')

       if(iTableSolarWind > 0)then
          call get_lookup_table(iTableSolarWind,Time=SwLT_Start, &
               IndexMax_I=IndexMax_I)

          ! Calculate start of run in unit [years]
          ! based on #STARTTIME command  !!! to be improved
          RunStart = iStartTime_I(1) + &
               n_day_of_year(iStartTime_I(1), iStartTime_I(2), &
               iStartTime_I(3))*2.74E-3 + iStartTime_I(4)*1.14E-4 &
               + iStartTime_I(5)*1.9E-6 + iStartTime_I(6)/cSecondPerYear

          !(1) Find the offset between the #STARTTIME of the run and
          ! start time of the lookup table.
          !(2) If RunStart is out of range of the lookup table time range
          ! Offset defaults to zero.
          if(RunStart >= SwLT_Start .and. RunStart <= &
               SwLT_Start+(IndexMax_I(2)/cSecondPerYear))then
             Offset = Runstart - SwLT_Start ! years
          end if
       end if
    end if

    if(iTableElectronImpact < 0) &
         iTableElectronImpact=i_lookup_table('ElectronImpact')

    ! Normalize ionization energy for electron impact
    IonizationEnergy = IonizationEnergyDim*Si2No_V(UnitEnergyDens_) &
         /Si2No_V(UnitN_)

    if(iTablePhotoionization < 0) then
       iTablePhotoionization = i_lookup_table('Photoionization')
       call update_photoionization_rate
    end if

    call test_stop(NameSub, DoTest)

  end subroutine user_init_session
  !============================================================================
  subroutine get_lat_dep_sw(x, y, z, Rho, Ur, Temp, Bsph_D)

    use ModLookupTable, ONLY: interpolate_lookup_table, i_lookup_table, &
         get_lookup_table

    ! Calculate latitude and time dependent solar wind and IMF at x,y,z

    real, intent(in):: x     ! X position to sample
    real, intent(in):: y     ! Y position to sample
    real, intent(in):: z     ! Z position to sample
    real, intent(out):: Rho  ! Rho at X,Y,Z according to table
    real, intent(out):: Ur   ! Radial U at X,Y,Z according to table
    real, intent(out):: Temp ! Temp at X,Y,Z according to table
    real, intent(out):: Bsph_D(3)   ! B at X,Y,Z according to table

    real ::  Latitude, SinTheta, MagLT_Start, MagLT_dt
    real ::  TimeCycleSW, TimeCycleB

    real :: IndexMax_I(2), Param_I(1)
    real :: ValueSW_I(3), ValueB_I(1)
    real :: r

    character(len=*), parameter:: NameSub = 'get_lat_dep_sw'
    !--------------------------------------------------------------------------
    r = sqrt(x**2 + y**2 + z**2)

    ! calculating latitude of the cell
    Latitude = cRadToDeg*asin(z/r)

    ! calculate the latitude of the cell
    SinTheta = sqrt(x**2+y**2)/r

    ! Make sure that OmegaSun and ParkerTilt are set
    if(OmegaSun == 0.0) call set_omega_parker_tilt

    if(iTableSolarWind < 0)then
       iTableSolarWind = i_lookup_table('solarwind2d')
       if(iTableSolarWind < 0) call CON_stop(NameSub// &
            ' : could not find lookup table solarwind2d.')
    end if

    ! calculating time relative to the solar cycle
    call get_lookup_table(iTableSolarWind, IndexMax_I=IndexMax_I, &
         Param_I = Param_I)

    TimeCycleSW = modulo(tSimulation + (Offset*cSecondPerYear) , IndexMax_I(2))

    ! interpolating the value of Rho, Vr, and Temp
    ! at the cell from the lookup table
    call interpolate_lookup_table(iTableSolarWind, Latitude, TimeCycleSW, &
         ValueSW_I, DoExtrapolate=.false.)

    Ur  = ValueSW_I(1)*Io2No_V(UnitU_)
    Rho = ValueSw_I(2)*Io2No_V(UnitRho_)
    Temp= ValueSW_I(3)*Io2No_V(UnitTemperature_)

    ! Chika. If TD magnetic lookup table is present
    ! read in values from the table
    iTableMagIntensity = i_lookup_table('MagIntensity')

    if(iTableMagIntensity > 0)then ! Use time-dependent magnetic field
       ! Make sure lookup tables are sampling the same times

       call get_lookup_table(iTableMagIntensity,Time=MagLT_Start)
       MagLT_dt = (SwLT_Start - MagLT_Start)*cSecondPerYear
       TimeCycleB = TimeCycleSW + MagLT_dt

       call interpolate_lookup_table(iTableMagIntensity, TimeCycleB, &
            ValueB_I, DoExtrapolate=.false.)

       Bsph_D(1) = (SQRT(0.5)*ValueB_I(1))/rBody**2 &
            *Io2No_V(UnitB_)                     ! Br scaled from 1 AU
       Bsph_D(2) =  0.0                                 ! Btheta
       Bsph_D(3) =  Bsph_D(1)*SinTheta*ParkerTilt &
            *SwhUx/Ur                            ! Bphi scaled from 1 AU
    else
       Bsph_D(1) = SwhBx ! Br
       Bsph_D(2) =  0.0                             ! Btheta
       Bsph_D(3) = SwhBx*SinTheta*ParkerTilt*SwhUx/Ur ! Bphi for vary B

    end if

    ! Scale density, pressure, and magnetic field with radial distance
    Rho       = Rho*(rBody/r)**2
    Temp      = Temp*(rBody/r)**(2*Gamma-2)
    Bsph_D(1) = Bsph_D(1)*(rBody/r)**2
    Bsph_D(3) = Bsph_D(3)*(rBody/r)

  end subroutine get_lat_dep_sw
  !============================================================================
  subroutine get_collision( iTypeCollision, &
       NumDensA, Cs2A, uA_D, NumDensB, Cs2B, uB_D, &
       SourceA_V, SourceB_V)

    use ModLookupTable, ONLY: interpolate_lookup_table, i_lookup_table

    integer, intent(in):: iTypeCollision
    real, intent(in):: NumDensA  ! fluid A number density
    real, intent(in):: Cs2A      ! fluid A thermal speed squared
    real, intent(in):: uA_D(3)   ! fluid A bulk velocity
    real, intent(in):: NumDensB  ! fluid B number density
    real, intent(in):: Cs2B      ! fluid B thermal speed squared
    real, intent(in):: uB_D(3)   ! fluid B bulk velocity
    real, intent(out):: SourceA_V(5) ! mass,momentum,energy sources for fluid A
    real, optional, intent(out):: SourceB_V(5) ! sources for fluid B

    integer:: iTableCollision
    real:: SqrtDuDim, SqrtCsDim  ! sqrt of relative and thermal speeds (km/s)
    real:: Integral_V(3)         ! integrals used to get rate, force and work
    real:: Cs2Sum                ! Cs2A + Cs2B
    real:: Umean_D(3), Umean2    ! Cs2 weighted mean velocity, squared
    real:: UDiff_D(3)            ! relative velocity
    real:: MassRate, ForcePerU, WorkPerCs2

    character(len=*), parameter:: NameSub = 'get_collision'
    !--------------------------------------------------------------------------
    select case(iTypeCollision)

    case(ChargeExchange_)
       if(iTableChargeExchange < 0)then
          iTableChargeExchange = i_lookup_table('ChargeExchange')
          if(iTableChargeExchange < 0) call CON_stop(NameSub// &
               ' : could not find lookup table ChargeExchange. Fix PARAM.in')
       end if
       iTableCollision = iTableChargeExchange
    case(ElectronImpact_)
       if(iTableElectronImpact < 0)then
          iTableElectronImpact = i_lookup_table('ElectronImpact')
          if(iTableElectronImpact < 0) call CON_stop(NameSub// &
               ' : could not find lookup table ElectronImpact. Fix PARAM.in')
       end if
       iTableCollision = iTableElectronImpact
    case default
       call CON_stop(NameSub// &
            ' : unexpected collision type.')
    end select

    Cs2Sum = Cs2A + CS2B        ! Sum of square thermal speeds
    UDiff_D = uA_D - uB_D       ! Velocity difference

    ! Use square root of sound speeds and relative velocity for lookup table
    SqrtCsDim = sqrt(sqrt(Cs2Sum)/1.E3)
    SqrtDuDim = sqrt(sqrt(sum(UDiff_D*UDiff_D))/1.E3)

    ! Get the MassRate, Force and Work integrals frame from the lookup table
    ! in SI units
    call interpolate_lookup_table(iTableCollision, &
         SqrtCsDim, SqrtDuDim, Integral_V, DoExtrapolate=.false.)

    ! Multiply with mass densities
    Integral_V = Integral_V * NumDensA * NumDensB

    MassRate   = Integral_V(1)         ! mass density change
    ForcePerU  = Integral_V(2)         ! force per velocity
    WorkPerCs2 = Integral_V(3)         ! work per thermal speed squared

    SourceA_V(1)    = MassRate
    SourceA_V(2:4)  = ForcePerU*uA_D
    SourceA_V(5)    = WorkPerCs2*Cs2A + 0.5*sum(ForcePerU*uA_D**2)

    if(present(SourceB_V)) then
       SourceB_V(1)    = MassRate
       SourceB_V(2:4)  = ForcePerU*uB_D
       SourceB_V(5)    = WorkPerCs2*Cs2B + 0.5*sum(ForcePerU*uB_D**2)
    endif

    if(iTypeCollision /= ChargeExchange_ .or. DoFixChargeExchange) then
       ! Extra terms that appear when splitting McNutt's formulas
       Umean_D = (Cs2A*uB_D + Cs2B*uA_D)/Cs2Sum
       Umean2  = sum(Umean_D**2)

       SourceA_V(2:4)  = SourceA_V(2:4) + Umean_D*(MassRate - ForcePerU)
       SourceA_V(5)    = SourceA_V(5) &
            + Cs2A*Cs2B*(0.75*MassRate - WorkPerCs2)/Cs2Sum &
            + 0.5*Umean2*(MassRate - ForcePerU)

       if (present(SourceB_V)) then
          SourceB_V(2:4)  = SourceB_V(2:4) + Umean_D*(MassRate - ForcePerU)
          SourceB_V(5)    = SourceB_V(5) &
               + Cs2A*Cs2B*(0.75*MassRate - WorkPerCs2)/Cs2Sum &
               + 0.5*Umean2*(MassRate - ForcePerU)
       endif
    endif

    ! Particles from AMPS treated as zero temperature fluids
    if(Cs2A > 0.0 .and. Cs2B > 0.0) then
       ! Convert to normalized units here for BATSRUS
       SourceA_V(1)   = SourceA_V(1)  *Si2No_V(UnitRho_)
       SourceA_V(2:4) = SourceA_V(2:4)*Si2No_V(UnitRhoU_)
       SourceA_V(5)   = SourceA_V(5)  *Si2No_V(UnitEnergyDens_)
       SourceA_V      = SourceA_V     *No2Si_V(UnitT_) ! same as /Si2No_V

       if (present(SourceB_V)) then
          SourceB_V(1)   = SourceB_V(1)  *Si2No_V(UnitRho_)
          SourceB_V(2:4) = SourceB_V(2:4)*Si2No_V(UnitRhoU_)
          SourceB_V(5)   = SourceB_V(5)  *Si2No_V(UnitEnergyDens_)
          SourceB_V      = SourceB_V     *No2Si_V(UnitT_)
       endif
    else
       ! AMPS prefers to get rate of change for number density
       SourceA_V(1)   = SourceA_V(1)/cProtonMass
       if (present(SourceB_V)) &
            SourceB_V(1) = SourceB_V(1)/cProtonMass
    endif

  end subroutine get_collision
  !============================================================================
  subroutine get_photoionization_rate_si(r, rate)
    real, intent(in):: r
    real, intent(out):: rate

    character(len=*), parameter:: NameSub = 'get_photoionization_rate_si'
    !--------------------------------------------------------------------------
    rate = 0

    ! Time-dependent photoionization rate can be implemented here
    if(UsePhotoion) &
         rate = PhotoionizationRate*(1/(r+1e-10))**2
  end subroutine get_photoionization_rate_si
  !============================================================================
  subroutine update_photoionization_rate()
    use ModMain, ONLY: StartTime
    use ModLookupTable, ONLY: interpolate_lookup_table

    real :: year, rate(1)

    character(len=*), parameter:: NameSub = 'update_photoionization_rate'
    !--------------------------------------------------------------------------

    if(iTablePhotoionization > 0)then
       year = (StartTime + tSimulation)/cSecondPerYear + iYearBase
       ! Read in the photoionization rate from the lookup table
       call interpolate_lookup_table(iTablePhotoionization, year, &
            rate, DoExtrapolate=.false.)
       PhotoionizationRate = rate(1)
    end if

  end subroutine update_photoionization_rate
  !============================================================================
  subroutine user_calc_sources_impl(iBlock)

    use ModPhysics, ONLY: ReducedMass_II, CollisionCoef_II, MassIonElectron_I

    integer, intent(in) :: iBlock

    integer :: i, j, k

    real :: State_V(nVar), Source_V(nVar+nFluid)

    integer :: iIon, jIon, iIonLast, iRhoUx, iRhoUz, iP, iEnergy
    real :: ReducedTemp, CollisionRate, Coef
    real :: Du2
    real, dimension(nIonFluid) :: RhoIon_I, ChargeDensIon_I
    real, dimension(nIonFluid+1) :: Ux_I, Uy_I, Uz_I, P_I, NumDens_I, T_I
    real :: U_D(3), Du_D(3), Me_D(3)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_calc_sources_impl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    iIonLast = nIonFluid
    if(UseElectronPressure) iIonLast = nIonFluid+1

    do k = 1, nK; do j = 1, nJ; do i = 1, nI

       State_V = State_VGB(:,i,j,k,iBlock)

       RhoIon_I = State_V(iRhoIon_I)
       Ux_I(:nIonFluid) = State_V(iRhoUxIon_I)/RhoIon_I
       Uy_I(:nIonFluid) = State_V(iRhoUyIon_I)/RhoIon_I
       Uz_I(:nIonFluid) = State_V(iRhoUzIon_I)/RhoIon_I
       P_I(:nIonFluid)  = State_V(iPIon_I)
       NumDens_I(:nIonFluid)  = RhoIon_I/MassIon_I
       T_I(:nIonFluid)  = P_I(:nIonFluid)/NumDens_I(:nIonFluid)

       if(UseElectronPressure)then
          ChargeDensIon_I = ChargeIon_I*NumDens_I(:nIonFluid)
          P_I(nIonFluid+1) = State_V(Pe_)
          NumDens_I(nIonFluid+1) = sum(ChargeDensIon_I)
          T_I(nIonFluid+1) = P_I(nIonFluid+1)/NumDens_I(nIonFluid+1)
          Ux_I(nIonFluid+1) = sum(ChargeDensIon_I*Ux_I(:nIonFluid)) &
               /NumDens_I(nIonFluid+1)
          Uy_I(nIonFluid+1) = sum(ChargeDensIon_I*Uy_I(:nIonFluid)) &
               /NumDens_I(nIonFluid+1)
          Uz_I(nIonFluid+1) = sum(ChargeDensIon_I*Uz_I(:nIonFluid)) &
               /NumDens_I(nIonFluid+1)
       end if

       Source_V = 0.0

       do iIon = 1, iIonLast

          if(iIon == nIonFluid+1)then
             Me_D = 0.0
             iP = Pe_
          else
             iRhoUx = iRhoUxIon_I(iIon); iRhoUz = iRhoUzIon_I(iIon)
             iP = iPIon_I(iIon)
          end if

          do jIon = 1, iIonLast
             if(iIon == jIon) CYCLE

             ReducedTemp = (MassIonElectron_I(jIon)*T_I(iIon) &
                  + MassIonElectron_I(iIon)*T_I(jIon)) &
                  /(MassIonElectron_I(iIon) + MassIonElectron_I(jIon))

             ! Turbulence modifies the collision rate, but we do not
             ! incorporate that here
             CollisionRate = CollisionCoef_II(iIon,jIon) &
                  *NumDens_I(jIon)/(ReducedTemp*sqrt(ReducedTemp))

             Du_D = [ Ux_I(jIon) - Ux_I(iIon), Uy_I(jIon) - Uy_I(iIon), &
                  Uz_I(jIon) - Uz_I(iIon) ]

             Du2 = sum(Du_D**2)

             Coef = NumDens_I(iIon)*ReducedMass_II(iIon,jIon)*CollisionRate

             if(iIon == nIonFluid+1)then
                Me_D = Me_D + MassIonElectron_I(nIonFluid+1) &
                     *NumDens_I(nIonFluid+1)*CollisionRate*Du_D
             else
                Source_V(iRhoUx:iRhoUz) = Source_V(iRhoUx:iRhoUz) &
                     + RhoIon_I(iIon)*CollisionRate*Du_D
             end if

             Source_V(iP) = Source_V(iP) &
                  + Coef*(2.0*(T_I(jIon) - T_I(iIon)) &
                  /MassIonElectron_I(jIon) + (2.0/3.0)*Du2)
          end do
       end do

       do iIon = 1, nIonFluid
          iRhoUx = iRhoUxIon_I(iIon); iRhoUz = iRhoUzIon_I(iIon)
          iP = iPIon_I(iIon)
          iEnergy = Energy_ - 1 + iIon

          if(UseElectronPressure) Source_V(iRhoUx:iRhoUz) &
               = Source_V(iRhoUx:iRhoUz) + ChargeDensIon_I(iIon) &
               /NumDens_I(nIonFluid+1)*Me_D

          U_D = [ Ux_I(iIon), Uy_I(iIon), Uz_I(iIon) ]
          Source_V(iEnergy) = Source_V(iEnergy) &
               + InvGammaMinus1*Source_V(iP) &
               + sum(U_D*Source_V(iRhoUx:iRhoUz))
       end do

       Source_VC(:,i,j,k) = Source_VC(:,i,j,k) + Source_V

    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_calc_sources_impl
  !============================================================================
  subroutine user_init_point_implicit

    use ModMultiFluid, ONLY: iRhoUxIon_I, iRhoUyIon_I, iRhoUzIon_I, iPIon_I
    use ModPointImplicit, ONLY: iVarPointImpl_I, IsPointImplMatrixSet

    logical :: IsPointImpl_V(nVar)
    integer :: iVar, iPointImplVar, nPointImplVar

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_init_point_implicit'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    IsPointImpl_V = .false.

    IsPointImpl_V(iRhoUxIon_I) = .true.
    IsPointImpl_V(iRhoUyIon_I) = .true.
    IsPointImpl_V(iRhoUzIon_I) = .true.
    IsPointImpl_V(iPIon_I)     = .true.
    if(UseElectronPressure) IsPointImpl_V(Pe_) = .true.

    ! Tell the point implicit scheme if dS/dU will be set analytically
    ! If this is set to true the DsDu_VVC matrix has to be set below.
    IsPointImplMatrixSet = .false.

    nPointImplVar = count(IsPointImpl_V)

    allocate(iVarPointImpl_I(nPointImplVar))

    iPointImplVar = 0
    do iVar = 1, nVar
       if(.not. IsPointImpl_V(iVar)) CYCLE
       iPointImplVar = iPointImplVar + 1
       iVarPointImpl_I(iPointImplVar) = iVar
    end do

  end subroutine user_init_point_implicit
  !============================================================================
  subroutine user_set_boundary_cells(iBlock)

    use ModMain, ONLY: NameThisComp, ExtraBc_
    use ModBuffer, ONLY: BufferMin_D, BufferMax_D, BuffR_
    use ModBoundaryGeometry, ONLY: iBoundary_GB

    ! Set "false" cells that are surrounded by the "extra" face boundary

    integer,intent(in)::iBlock

    character(len=*), parameter:: NameSub = 'user_set_boundary_cells'
    !--------------------------------------------------------------------------
    if(NameThisComp == 'IH') then
       where(r_GB(:,:,:,iBlock) > BufferMax_D(BuffR_)) &
            iBoundary_GB(:,:,:,iBlock) = ExtraBc_
    end if
    if(NameThisComp == 'OH') then
       where(r_GB(:,:,:,iBlock) < BufferMin_D(BuffR_)) &
            iBoundary_GB(:,:,:,iBlock) = ExtraBc_
    end if

  end subroutine user_set_boundary_cells
  !============================================================================

end module ModUser
!==============================================================================
subroutine get_charge_exchange_wrapper( &
     RhoIon, Cs2Ion, uIon_D, RhoNeu, Cs2Neu, uNeu_D, &
     SourceIon_V, SourceNeu_V) &
     bind(c, name='get_charge_exchange_wrapper')

  ! C wrapper for coupling with AMPS

  use ModUser, ONLY: get_collision

  real, intent(in):: RhoIon    ! ion mass density
  real, intent(in):: Cs2Ion    ! ion thermal speed squared
  real, intent(in):: uIon_D(3) ! ion bulk velocity
  real, intent(in):: RhoNeu    ! neutral mass density
  real, intent(in):: Cs2Neu    ! neutral thermal speed squared (0 for AMPS)
  real, intent(in):: uNeu_D(3) ! neutral bulk velocity
  real, intent(out):: SourceIon_V(5) ! mass, momentum, energy sources from ions
  real, intent(out):: SourceNeu_V(5) ! mass, momentum, energy sources from neu.
  !----------------------------------------------------------------------------
  call get_collision(1, RhoIon, Cs2Ion, &
       uIon_D, RhoNeu, Cs2Neu, uNeu_D, SourceIon_V, SourceNeu_V)

end subroutine get_charge_exchange_wrapper
!==============================================================================
subroutine get_charge_exchange_region( &
     iRegion, r, RhoDim, U2Dim, USW2Dim, TempDim, TempPu2Dim, &
     Mach2, MachPUI2, MachSW2, LevelHP) &
     bind(c, name='get_charge_exchange_region')

  use ModUser, ONLY: get_region

  integer, intent(out):: iRegion
  real, intent(in)    :: r, RhoDim, U2Dim, USW2Dim, TempDim, TempPu2Dim,&
       Mach2, MachSW2, MachPUI2, LevelHP

  logical :: DoReIndex
  !----------------------------------------------------------------------------
  DoReIndex = .true.

  call get_region(iRegion, r, RhoDim, U2Dim, TempDim, &
       Mach2, MachPUI2, MachSW2, LevelHP, DoReIndex)

  ! For convenience, reduce iRegion by 1 since the c/c++ array
  ! index starts from 0
  iRegion = iRegion - 1

end subroutine get_charge_exchange_region
!==============================================================================
subroutine get_solar_wind(x, y, z, NumDen, Ur, Temp, B_D) &
     bind(c, name='get_solar_wind')

  ! C wrapper for coupling with AMPS

  use ModUser, ONLY: get_lat_dep_sw
  use ModPhysics, ONLY: No2Si_V, Si2No_V, UnitX_, UnitN_, UnitU_, &
       UnitTemperature_, UnitB_

  real, intent(in)::  x, y, z   ! Position in SI unit. [m]
  real, intent(out):: NumDen    ! Solar wind density [#/m^3]
  real, intent(out):: Ur        ! Solar wind speed [m/s]
  real, intent(out):: Temp      ! Solar wind temperature [K]
  real, intent(out):: B_D(3)    ! Solar wind B field [T]

  real :: x0, y0, z0
  !----------------------------------------------------------------------------
  ! Convert from SI to normalized units
  x0 = x*Si2No_V(UnitX_)
  y0 = y*Si2No_V(UnitX_)
  z0 = z*Si2No_V(UnitX_)

  call get_lat_dep_sw(x0, y0, z0, NumDen, Ur, Temp, B_D)

  ! Convert to SI units
  NumDen = NumDen*No2Si_V(UnitN_)
  Ur     = Ur    *No2Si_V(UnitU_)
  Temp   = Temp  *No2Si_V(UnitTemperature_)
  B_D    = B_D   *No2Si_V(UnitB_)

end subroutine get_solar_wind
!==============================================================================
subroutine photoionization_rate_wrapper(rSI, rateSI) &
     bind(c, name='photoionization_rate_wrapper')

  use ModUser, ONLY: get_photoionization_rate_si
  use ModPhysics, ONLY: Si2No_V, UnitX_

  real, intent(in):: rSI
  real, intent(out):: rateSI

  real :: r
  !----------------------------------------------------------------------------
  r = rSI*Si2No_V(UnitX_)

  call get_photoionization_rate_si(r, rateSI)

end subroutine photoionization_rate_wrapper
!==============================================================================
