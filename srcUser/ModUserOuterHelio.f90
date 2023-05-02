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
       test_start, test_stop, iTest, jTest, kTest, iProc

  use ModSize,     ONLY: nI, nJ, nK
  use ModMain,       ONLY: body1_,      &
       nBlock, Unused_B, tSimulation, IsTimeAccurate, iStartTime_I
  use ModPhysics,  ONLY: Gamma, GammaMinus1, GammaElectronMinus1, OmegaBody, &
       UnitX_, Io2Si_V, Si2Io_V, Si2No_V, No2Io_V, No2Si_V, Io2No_V, &
       NameTecUnit_V, NameIdlUnit_V, UnitAngle_, UnitDivB_, UnitEnergyDens_, &
       UnitJ_, UnitN_, UnitRho_, UnitU_, rBody, UnitB_, UnitP_, &
       UnitTemperature_, UnitT_, UnitRhoU_, FaceState_VI, IonMassPerCharge

  use ModConst,    ONLY: cAU, cProtonMass, cElectronMass, cBoltzmann, cEV, &
          cRyToEv, cElectronCharge, cSecondPerYear
  use ModTimeConvert, ONLY: n_day_of_year
  use ModNumConst, ONLY: cRadToDeg, cPi, cTwoPi
  use ModAdvance,  ONLY: State_VGB,Source_VC, ExtraSource_ICB, Pe_, &
       UseElectronPressure
  use ModGeometry, ONLY: Xyz_DGB, r_GB, Used_GB
  use ModVarIndexes
  use ModMultiFluid
  use ModCurrent, ONLY: get_current
  use ModUserEmpty,                                     &
       IMPLEMENTED1  => user_read_inputs,               &
       IMPLEMENTED2  => user_set_face_boundary,         &
       IMPLEMENTED3  => user_normalization,             &
       IMPLEMENTED4  => user_set_cell_boundary,         &
       IMPLEMENTED5  => user_set_ics,                   &
       IMPLEMENTED6  => user_initial_perturbation,      &
       IMPLEMENTED7  => user_update_states,             &
       IMPLEMENTED8  => user_action,                    &
       IMPLEMENTED10 => user_set_plot_var,              &
       IMPLEMENTED12 => user_calc_sources_expl,         &
       IMPLEMENTED14 => user_init_session

  include 'user_module.h' ! list of public methods

  ! Needed for coupling with AMPS through the C wrapper
  public:: get_collision

  real,              parameter :: VersionUserModule = 1.0
  character (len=*), parameter :: NameUserFile = "ModUserOuterHelio.f90"
  character (len=*), parameter :: NameUserModule = "Outer Heliosphere"

  ! Named indexes for fluids
  integer, parameter :: SWH_ = 1, Ion_ = 1, &
       Pu3_ = min(nFluid,2), Neu_ = min(nFluid,IonLast_+1), &
       Ne2_ = min(nFluid,IonLast_+2), Ne3_ = min(nFluid,IonLast_+3), &
       Ne4_= min(nFluid,IonLast_+4)

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
  real :: TableStart = 0.0
  real :: RunStart = 0.0
  real :: Offset = 0.0

  logical :: UsePu3Heating = .false.
  real :: TempPu3, FactorPu3, HeatPu3

  integer :: iTableSolarWind = -1 ! initialization is needed
  integer :: iTableChargeExchange = -1
  integer :: iTableElectronImpact = -1

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
  integer,parameter :: SingleIon_ = 1, ColdCloud_ = 2, MultiIon_=3
  integer :: iRegionFormula = SingleIon_

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

  ! variable to hold sources from charge-exchange with neutral fluids
  real, allocatable :: FluidSource_ICB(:,:,:,:,:)

  ! Wave turbulence
  real :: DeltaUSi = 10.0, DeltaU = 0.0
  real :: LperpTimesSqrtBSi = 1.5e5, LperpTimesSqrtB = 0.0

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
          call read_var('TempPu3', TempPu3)
          call read_var('FactorPu3', FactorPu3)

          ! This is a flag to define how many populations of Neutrals to run
       case("#SOURCES")
          if(IsMhd)then
             call read_var('UseIonSource', UseSource_I(Ion_))
             call read_var('UseNeuSource', UseSource_I(Neu_))
             call read_var('UseNe2Source', UseSource_I(Ne2_))
             call read_var('UseNe3Source', UseSource_I(Ne3_))
             call read_var('UseNe4Source', UseSource_I(Ne4_))
          else
             call read_var('UseSWHSource', UseSource_I(SWH_))
             call read_var('UsePu3Source', UseSource_I(Pu3_))
             call read_var('UseNeuSource', UseSource_I(Neu_))
             call read_var('UseNe2Source', UseSource_I(Ne2_))
             call read_var('UseNe3Source', UseSource_I(Ne3_))
             call read_var('UseNe4Source', UseSource_I(Ne4_))
          end if

          ! Chika. This is a flag to turn on photoionization
          ! If not specified in PARAM.in, it will be false
       case("#PHOTOIONIZATION")
          call read_var('UsePhotoionization', UsePhotoion)

       case("#COLDCLOUD")
          call read_var('UseColdCloud', UseColdCloud)
          call read_var('DoInitializeCloud', DoInitializeCloud)
          call read_var('UseSrcsInHelio', UseSrcsInHelio)
          call read_var('HpLimit', HpLimit)

          ! Bair. This is a flag to use charge exchange formula
          ! with the extra splitting terms.
       case("#CHARGEEXCHANGE")
          call read_var("DoFixChargeExchange", DoFixChargeExchange)

       case("#ELECTRONIMPACT")
          call read_var("UseElectronImpact", UseElectronImpact)

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

          case default
             call stop_mpi(NameSub//': unknown NameRegionFormula = ' &
                     // NameRegionFormula)
          end select

       case("#TURBULENCE")
          call read_var('DeltaUSi', DeltaUSi)
          call read_var('LperpTimesSqrtBSi', LperpTimesSqrtBSi)

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
    use ModWaves,          ONLY: UseAlfvenWaves

    type(FaceBCType), intent(inout):: FBC

    ! local variables
    real:: xFace, yFace, zFace
    real:: SinTheta
    ! C.P. edited
    real:: Bsph_D(3), Vsph_D(3), VPUIsph_D(3)

    real :: pSolarWind,pPUI, Pmag, PmagEquator, Ewave, B

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

      if(UseAlfvenWaves)then
         Ewave = SwhRho*DeltaU**2
         if(Bsph_D(1) > 0.0)then
            VarsGhostFace_V(WaveFirst_) = Ewave
            VarsGhostFace_V(WaveLast_) = 0.0
         else
            VarsGhostFace_V(WaveFirst_) = 0.0
            VarsGhostFace_V(WaveLast_) = Ewave
         end if

         if(Lperp_>1)then
            B = sqrt(max(sum(VarsGhostFace_V(Bx_:Bz_)), 1e-30))
            VarsGhostFace_V(Lperp_) = SwhRho*LperpTimesSqrtB/sqrt(B)
         end if
      end if

      if(UseElectronPressure) VarsGhostFace_V(Pe_) = SwhPe

      if(.not.IsMhd)then
         VarsGhostFace_V(Pu3Rho_)    = Pu3Rho
         VarsGhostFace_V(Pu3P_)      = pPUI  ! Pu3P ???
         VarsGhostFace_V(Pu3Ux_:Pu3Uz_) = matmul(XyzSph_DD, VPUIsph_D)
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

         if(.not.IsMhd)then
            write(*,*) NameSub,' SWHRhoGhost =', VarsGhostFace_V(SWHRho_)
            write(*,*) NameSub,' SWHpGhost   =', VarsGhostFace_V(SWHP_)
            write(*,*) NameSub,' SWHUghost   =', VarsGhostFace_V(SWHUx_:SWHUz_)
         else
            write(*,*) NameSub,' RhoGhost =', VarsGhostFace_V(Rho_)
            write(*,*) NameSub,' pGhost   =', VarsGhostFace_V(p_)
            write(*,*) NameSub,' Ughost   =', VarsGhostFace_V(Ux_:Uz_)
         end if
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
  subroutine user_normalization

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_normalization'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    No2Si_V(UnitX_)  = cAU                                      ! m
    No2Si_V(UnitU_)  = sqrt(Gamma*cBoltzmann*SwhTDim/cProtonMass) ! m/s
    No2Si_V(UnitRho_)= cProtonMass*SwhRhoDim*1.0E+6           ! kg/m^3

    if(DoTest)then
       write(*,*)NameSub,' No2Si_V(UnitX_)  =',No2Si_V(UnitX_)
       write(*,*)NameSub,' No2Si_V(UnitU_)  =',No2Si_V(UnitU_)
       write(*,*)NameSub,' No2Si_V(UnitRho_)=',No2Si_V(UnitRho_)
    end if

    call test_stop(NameSub, DoTest)
  end subroutine user_normalization
  !============================================================================
  subroutine user_set_cell_boundary(iBlock, iSide, TypeBc, IsFound)

    use ModCellBoundary, ONLY: iMin, iMax, jMin, jMax, kMin, kMax
    use ModWaves,        ONLY: UseAlfvenWaves

    ! The ISM enters at the east boundary (negative x)
    ! February 08, 2018 - added the possibility for using user conditions in
    ! other boundaries as well

    integer,      intent(in):: iBlock, iSide
    character(len=*), intent(in)  :: TypeBc
    logical,      intent(out):: IsFound

    integer :: iVar, i, j, k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_cell_boundary'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    IsFound = .true.

    do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
       State_VGB(SWHRho_,i,j,k,iBlock)=VliswRho
       State_VGB(SWHRhoUx_,i,j,k,iBlock)=VliswRho*VliswUx
       State_VGB(SWHRhoUy_,i,j,k,iBlock)=VliswRho*VliswUy
       State_VGB(SWHRhoUz_,i,j,k,iBlock)=VliswRho*VliswUz
       State_VGB(Bx_,i,j,k,iBlock)=VliswBx
       State_VGB(By_,i,j,k,iBlock)=VliswBy
       State_VGB(Bz_,i,j,k,iBlock)=VliswBz
       if(UseAlfvenWaves) State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock) = 0.0
       State_VGB(SWHP_,i,j,k,iBlock)=VliswP
       if(UseElectronPressure) State_VGB(Pe_,i,j,k,iBlock)=VliswPe

       if(.not.IsMhd)then
          State_VGB(Pu3Rho_,  i,j,k,iBlock) = 1E-5*VliswRho
          State_VGB(Pu3RhoUx_,i,j,k,iBlock) = 1E-5*VliswRho*VliswUx
          State_VGB(Pu3RhoUy_,i,j,k,iBlock) = 1E-5*VliswRho*VliswUy
          State_VGB(Pu3RhoUz_,i,j,k,iBlock) = 1E-5*VliswRho*VliswUz
          State_VGB(Pu3P_,    i,j,k,iBlock) = 1E-5*VliswP
       end if

    end do; end do; end do

    if(UseNeutralFluid)then
       !
       ! PopIV is the one one coming with the ISW
       ! The separation between Pop IV and Pop I is arbitrary so
       ! we took the separation as Vlad in x=-1500AU

       do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
          State_VGB(Ne4Rho_,i,j,k,iBlock) = RhoNeutralsISW
          State_VGB(Ne4RhoUx_,i,j,k,iBlock) = RhoNeutralsISW*UxNeutralsISW
          State_VGB(Ne4RhoUy_,i,j,k,iBlock) = RhoNeutralsISW*UyNeutralsISW
          State_VGB(Ne4RhoUz_,i,j,k,iBlock) = RhoNeutralsISW*UzNeutralsISW
          State_VGB(Ne4P_,i,j,k,iBlock)     = PNeutralsISW
       end do; end do; end do
       !
       ! In general you should specify as many values as many incoming
       ! characteristic waves are present. For a neutral fluid this
       ! is 0 for supersonic outflow, 1 for subsonic outflow,
       ! 4 for subsonic inflow and 5 for supersonic inflow.

       !
       ! PopII and III supersonic outflow
       do iVar = Lperp_, Ne3P_
          if(iVar>=NeuRho_.and.iVar<=Ne3P_ .or. Lperp_>1.and.iVar==Lperp_)then
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
          end if
       end do
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_cell_boundary
  !============================================================================
  subroutine user_set_ics(iBlock)

    use ModCoordTransform, ONLY: rot_xyz_sph
    use ModWaves,          ONLY: UseAlfvenWaves

    integer, intent(in) :: iBlock

    integer :: i,j,k
    real :: x, y, z, r
    real :: b_D(3), v_D(3), bSph_D(3), vSph_D(3), vPUI_D(3), vPUISph_D(3)
    real :: SinTheta, SignZ
    real :: Ewave, B
    real :: XyzSph_DD(3,3) ! rotation matrix Xyz_D = matmul(XyzSph_DD, Sph_D)

    logical :: DoTestCell
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_ics'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! Make sure that OmegaSun and ParkerTilt are set
    if(OmegaSun == 0.0)call set_omega_parker_tilt

    do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI

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
       v_D = matmul(XyzSph_DD, Vsph_D)
       vPUI_D = matmul(XyzSph_DD, VPUIsph_D)

       ! density and pressure
       if(.not.IsMhd)then
          State_VGB(SWHRho_,i,j,k,iBlock) = SwhRho * (rBody/r)**2
          State_VGB(SWHP_,i,j,k,iBlock)   = SwhP   * (rBody/r)**(2*Gamma)
          State_VGB(SWHRhoUx_:SWHRhoUz_,i,j,k,iBlock) = &
               State_VGB(SWHRho_,i,j,k,iBlock)*v_D
       else
          State_VGB(Rho_,i,j,k,iBlock) = SwhRho * (rBody/r)**2
          State_VGB(P_,i,j,k,iBlock)   = SwhP   * (rBody/r)**(2*Gamma)
          State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) = &
               State_VGB(Rho_,i,j,k,iBlock)*v_D
       end if
       if(UseElectronPressure) &
            State_VGB(Pe_,i,j,k,iBlock) = SwhPe * (rBody/r)**(2*Gamma)

       if(UseColdCloud) then
          !! for studies of Heliosphere in cold cloud
          if (r > rBody) then
             State_VGB(Bx_,i,j,k,iBlock) =  VliswBx
             State_VGB(By_,i,j,k,iBlock) =  VliswBy
             State_VGB(Bz_,i,j,k,iBlock) =  VliswBz

             State_VGB(Rho_,i,j,k,iBlock) = VliswRho
             State_VGB(P_,i,j,k,iBlock)   = VliswP

             State_VGB(RhoUx_,i,j,k,iBlock) = VliswUx*VliswRho
             State_VGB(RhoUy_,i,j,k,iBlock) = VliswUy*VliswRho
             State_VGB(RhoUz_,i,j,k,iBlock) = VliswUz*VliswRho
          endif
       end if

       if(UseAlfvenWaves)then
          ! Energy density as specified in van der Holst et al. 2014
          Ewave = State_VGB(Rho_,i,j,k,iBlock)*DeltaU**2 &
                  *sqrt(State_VGB(Rho_,i,j,k,iBlock)/SwhRho)

          ! Positive propagating waves
          if(sum(State_VGB(Bx_:Bz_,i,j,k,iBlock) &
               * Xyz_DGB(x_:z_,i,j,k,iBlock))>0.0)then

             State_VGB(WaveFirst_,i,j,k,iBlock) = Ewave
             State_VGB(WaveLast_,i,j,k,iBlock) = &
                  1e-3*State_VGB(WaveFirst_,i,j,k,iBlock)

          ! Negative propagating waves
          else
             State_VGB(WaveLast_,i,j,k,iBlock) = Ewave
             State_VGB(WaveFirst_,i,j,k,iBlock) = &
	          1e-3*State_VGB(WaveLast_,i,j,k,iBlock)
          end if

          if(Lperp_ > 1)then
             B = sqrt(max(sum(B_D**2), 1e-30))
             State_VGB(Lperp_,i,j,k,iBlock) = &
                  State_VGB(Rho_,i,j,k,iBlock)*LperpTimesSqrtB/sqrt(B)
	  end if
       end if

       if(UseNeutralFluid)then
          if(UseColdCloud)then
             ! PopI for studies of Heliosphere in cold cloud
             State_VGB(NeuRho_,i,j,k,iBlock)  = VliswRho
             State_VGB(NeuP_,i,j,k,iBlock) =   VliswP
             State_VGB(NeuRhoUx_,i,j,k,iBlock)= VliswUx*VliswRho
             State_VGB(NeuRhoUy_,i,j,k,iBlock)= VliswUy*VliswRho
             State_VGB(NeuRhoUz_,i,j,k,iBlock)= VliswUz*VliswRho
          else
             ! PopI
             State_VGB(NeuRho_,i,j,k,iBlock)  =  RhoNeutralsISW
             State_VGB(NeuP_,i,j,k,iBlock) =   PNeutralsISW
             State_VGB(NeuRhoUx_,i,j,k,iBlock)=RhoNeutralsISW*UxNeutralsISW
             State_VGB(NeuRhoUy_,i,j,k,iBlock)=RhoNeutralsISW*UyNeutralsISW
             State_VGB(NeuRhoUz_,i,j,k,iBlock)=RhoNeutralsISW*UzNeutralsISW
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

       ! No production yet
       if(.not.IsMhd)then
          State_VGB(Pu3Rho_,i,j,k,iBlock) = Pu3Rho * (rBody/r)**2
          State_VGB(Pu3P_,i,j,k,iBlock)   = Pu3P   * (rBody/r)**(2*Gamma)
          State_VGB(Pu3RhoUx_:Pu3RhoUz_,i,j,k,iBlock) = &
               State_VGB(Pu3Rho_,i,j,k,iBlock)*vPUI_D
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
          if(.not.IsMhd)then
             write(*,*)NameSub,' SWHRhoU_D =', &
                  State_VGB(SWHRhoUx_:SWHRhoUz_,i,j,k,iBlock)
             write(*,*)NameSub,' SWHRho,   =', State_VGB(SWHRho_,i,j,k,iBlock)
             write(*,*)NameSub,' SWHp      =', State_VGB(SWHP_,i,j,k,iBlock)
             write(*,*)NameSub,' VPUIsph_D =', VPUIsph_D
             write(*,*)NameSub,' vPUI_D    =', vPUI_D
             write(*,*)NameSub,' Pu3Rho,   =', State_VGB(Pu3Rho_,i,j,k,iBlock)
             write(*,*)NameSub,' Pu3P      =', State_VGB(Pu3P_,i,j,k,iBlock)
          else
             write(*,*)NameSub,' RhoU_D =', &
                  State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)
             write(*,*)NameSub,' Rho,   =',State_VGB(Rho_,i,j,k,iBlock)
             write(*,*)NameSub,' p      =',State_VGB(P_,i,j,k,iBlock)
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
       if(.not.IsMhd)then
          State_VGB(Ne2Rho_,:,:,:,iBlock) = RhoBcFactor_I(Ne2_) * &
               State_VGB(SWHRho_,:,:,:,iBlock)
          State_VGB(Ne2RhoUx_:Ne2RhoUz_,:,:,:,iBlock) = &
               RhoBcFactor_I(Ne2_)*uBcFactor_I(Ne2_)* &
               State_VGB(SWHRhoUx_:SWHRhoUz_,:,:,:,iBlock)
          State_VGB(Ne2P_,:,:,:,iBlock) = RhoBcFactor_I(Ne2_) * &
               State_VGB(SWHp_,:,:,:,iBlock)
       else
          State_VGB(Ne2Rho_,:,:,:,iBlock) = RhoBcFactor_I(Ne2_) * &
               State_VGB(Rho_,:,:,:,iBlock)
          State_VGB(Ne2RhoUx_:Ne2RhoUz_,:,:,:,iBlock) = &
               RhoBcFactor_I(Ne2_)*uBcFactor_I(Ne2_)* &
               State_VGB(RhoUx_:RhoUz_,:,:,:,iBlock)
          State_VGB(Ne2P_,:,:,:,iBlock) = RhoBcFactor_I(Ne2_) * &
               State_VGB(p_,:,:,:,iBlock)
       end if

    end do

    call test_stop(NameSub, DoTest)
  end subroutine user_initial_perturbation
  !============================================================================
  subroutine user_update_states(iBlock)

    use ModUpdateState, ONLY: update_state_normal

    use ModAdvance, ONLY: StateOld_VGB, State_VGB
    use ModGeometry, ONLY: rMin_B

    ! merav
    ! C.P. edited

    integer,intent(in):: iBlock

    integer:: i, j, k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_update_states'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    call update_state_normal(iBlock)
    if(DoFreezeNeutral)then
       ! Set neutrals back to previous state
       do k=1,nK; do j=1,nJ; do i=1,nI
          State_VGB(NeuRho_:Ne4P_,i,j,k,iBlock) = &
               StateOld_VGB(NeuRho_:Ne4P_,i,j,k,iBlock)
       end do; end do; end do
       RETURN
    end if

    ! No need to check blocks outside:
    if(rMin_B(iBlock) > rBody) RETURN

    do k=1,nK; do j=1,nJ; do i=1,nI
       if(r_GB(i,j,k,iBlock) > rBody) CYCLE

       if(iTableSolarWind > 0)then
          ! Calculate the time dependent solar wind
          call calc_time_dep_sw(i,j,k,iBlock)
       else
          ! Retain initial condition for all ions inside body
          State_VGB(1:iP_I(nIonFluid),i,j,k,iBlock) = &
               StateOld_VGB(1:iP_I(nIonFluid),i,j,k,iBlock)
       endif

    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  contains
    !==========================================================================
    subroutine calc_time_dep_sw(i,j,k,iBlock)

      use BATL_lib,       ONLY: Xyz_DGB
      use ModCoordTransform, ONLY: rot_xyz_sph
      use ModLookupTable, ONLY: interpolate_lookup_table, i_lookup_table, &
                                get_lookup_table

      integer,intent(in):: i, j, k, iBlock

      ! variables for Solar Cycle
      real :: Rho, Ur, Temp, p, x, y, z, r, Latitude
      real :: Bsph_D(3), Vsph_D(3)
      ! merav
      real :: v_D(3),vPUI_D(3), vPUISph_D(3)
      real :: XyzSph_DD(3,3) ! rotation matrix Xyz_D = matmul(XyzSph_DD,Sph_D)

      real :: TimeCycle ! holds the time [s] from the start time of run
      real :: Value_I(3)
      real :: SinTheta
      real :: IndexMax_I(2)
      !------------------------------------------------------------------------
      if(iTableSolarWind < 0)then
          iTableSolarWind = i_lookup_table('solarwind2d')
          if(iTableSolarWind < 0) call CON_stop(NameSub// &
             ' : could not find lookup table solarwind2d.')
      end if

      x = Xyz_DGB(1,i,j,k,iBlock)
      y = Xyz_DGB(2,i,j,k,iBlock)
      z = Xyz_DGB(3,i,j,k,iBlock)
      r = r_GB(i,j,k,iBlock)

      XyzSph_DD = rot_xyz_sph(x,y,z)

      ! calculate the latitude of the cell
      SinTheta = sqrt(x**2+y**2)/r

      ! calculating latitude of the cell
      Latitude = cRadToDeg*asin(z/r)

      ! calculating time relative to the solar cycle
      call get_lookup_table(iTableSolarWind,IndexMax_I=IndexMax_I)
      TimeCycle = modulo(tSimulation + (Offset*cSecondPerYear) , IndexMax_I(2))

      ! interpolating the value of Rho, Vr, and Temp
      ! at the cell from the lookup table
      call interpolate_lookup_table(iTableSolarWind, Latitude, TimeCycle, &
           Value_I)

      Ur  = Value_I(1)*Io2No_V(UnitU_)
      Rho = Value_I(2)*Io2No_V(UnitRho_)
      Temp= Value_I(3)*Io2No_V(UnitTemperature_)
      p = 2.0*Rho*Temp

      ! Spherical velocity, Vr, Vtheta, Vphi constant with  radial distance
      Vsph_D    = [ Ur, 0.0, 0.0 ]

      ! monopole with By negative and a time varying B
      ! time-dependent behavior of B taken from Michael et al. 2015
      Bsph_D(1) = (SQRT(0.5)/rBody**2)*(9.27638+ &
           7.60832d-8*TimeCycle-1.91555*SIN(1.28737d-8*TimeCycle)+ &
           0.144184*SIN(2.22823d-8*TimeCycle)+ &
           47.7758*SIN(2.18788d-10*TimeCycle)+ &
           83.5522*SIN(-1.20266d-9*TimeCycle))*Io2No_V(UnitB_) ! Br
      Bsph_D(2) =  0.0                             ! Btheta
      Bsph_D(3) = Bsph_D(1)*SinTheta*ParkerTilt*SwhUx/Ur ! Bphi for vary B

      ! Scale density, pressure, and magnetic field with radial distance
      Rho = Rho*(rBody/r)**2
      p     = p*(rBody/r)**(2*Gamma)
      Bsph_D(1) = Bsph_D(1)*(rBody/r)**2
      Bsph_D(3) = Bsph_D(3)*(rBody/r)

      ! merav      ! Setting the state variables

      ! Spherical magnetic field converted to Cartesian components
      State_VGB(Bx_:Bz_,i,j,k,iBlock) = matmul(XyzSph_DD, Bsph_D)

      if(.not.IsMhd)then
         ! merav
         ! velocity components in cartesian coordinates
         v_D = matmul(XyzSph_DD, Vsph_D)
         vPUI_D = matmul(XyzSph_DD, VPUIsph_D)
         ! density and pressure
         State_VGB(SWHRho_,i,j,k,iBlock) = SwhRho * (rBody/r)**2
         State_VGB(SWHP_,i,j,k,iBlock)   = SwhP   * (rBody/r)**(2*Gamma)
         if(UseElectronPressure)then
            State_VGB(Pe_,i,j,k,iBlock)   = SwhPe   * (rBody/r)**(2*Gamma)
         end if
         ! momentum
         State_VGB(SWHRhoUx_:SWHRhoUz_,i,j,k,iBlock) = &
              State_VGB(SWHRho_,i,j,k,iBlock)*v_D
         State_VGB(Pu3Rho_,i,j,k,iBlock) = Pu3Rho * (rBody/r)**2
         State_VGB(Pu3P_,i,j,k,iBlock)   = Pu3P   * (rBody/r)**(2*Gamma)
         ! momentum
         State_VGB(Pu3RhoUx_:Pu3RhoUz_,i,j,k,iBlock) = &
              State_VGB(Pu3Rho_,i,j,k,iBlock)*vPUI_D
      else
         State_VGB(Rho_,i,j,k,iBlock) = Rho
         ! Velocity converted to 3 components of momentum in Cartesian coords
         State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) = matmul(XyzSph_DD, Rho*Vsph_D)
         ! Sets pressure and energy only for the ion fluid
         State_VGB(p_,i,j,k,iBlock) = p
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
       if(UseElectronPressure)then
          write(*,StringFormat) 'VliswPeDim [nPa]: ', &
               VliswPeDim,' VliswPe:', VliswPe
          write(*,'(10X,A19,F15.6)') 'VliswTeDim[K]: ', VliswTeDim
          write(*,StringFormat) 'SwhTeDim   [   K]:', SwhTeDim, &
               ' SwhPe:', SwhPe
       end if
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

    real, dimension(nVar + nFluid):: Source_V

    real, dimension(nFluid) :: U2_I, Temp_I, Rho_I, UThS_I

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
       case('sRho')
          NameTecVar = 'sRho'
          PlotVar_G(1:nI,1:nJ,1:nK) = FluidSource_ICB(SWHRho_,:,:,:,iBlock)
          NameIdlUnit = '1/s'
          NameTecUnit = '1/s'

       case('sRhoux')
          NameTecVar = 'sRhoUx'
          PlotVar_G(1:nI,1:nJ,1:nK) = FluidSource_ICB(SWHRhoUx_,:,:,:,iBlock)
          NameIdlUnit = '1/s'
          NameTecUnit = '1/s'

       case('sRhouy')
          NameTecVar = 'sRhoUy'
          PlotVar_G(1:nI,1:nJ,1:nK) = FluidSource_ICB(SWHRhoUy_,:,:,:,iBlock)
          NameIdlUnit = '1/s'
          NameTecUnit = '1/s'
       case('sRhouz')
          NameTecVar = 'sRhoUz'
          PlotVar_G(1:nI,1:nJ,1:nK) = FluidSource_ICB(SWHRhoUz_,:,:,:,iBlock)
          NameIdlUnit = '1/s'
          NameTecUnit = '1/s'
       case('senergy')
          NameTecVar = 'senergy'
          PlotVar_G(1:nI,1:nJ,1:nK) = FluidSource_ICB(SWHEnergy_,:,:,:,iBlock)
          NameIdlUnit = '1/s'
          NameTecUnit = '1/s'
       case('sRhopui')
          NameTecVar = 'sRhoPUI'
          PlotVar_G(1:nI,1:nJ,1:nK) = FluidSource_ICB(Pu3Rho_,:,:,:,iBlock)
          NameIdlUnit = '1/s'
          NameTecUnit = '1/s'
       case('sRhouxpui')
          NameTecVar = 'sRhoUxPUI'
          PlotVar_G(1:nI,1:nJ,1:nK) = FluidSource_ICB(Pu3RhoUx_,:,:,:,iBlock)
          NameIdlUnit = '1/s'
          NameTecUnit = '1/s'

       case('sRhouypui')
          NameTecVar = 'sRhoUyPUI'
          PlotVar_G(1:nI,1:nJ,1:nK) = FluidSource_ICB(Pu3RhoUy_,:,:,:,iBlock)
          NameIdlUnit = '1/s'
          NameTecUnit = '1/s'

       case('sRhouzpui')
          PlotVar_G(1:nI,1:nJ,1:nK) = FluidSource_ICB(Pu3RhoUz_,:,:,:,iBlock)
          NameIdlUnit = '1/s'
          NameTecUnit = '1/s'

       case('senergypui')
          PlotVar_G(1:nI,1:nJ,1:nK) = FluidSource_ICB(Pu3Energy_,:,:,:,iBlock)
          NameIdlUnit = '1/s'
          NameTecUnit = '1/s'

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

       case( 'srswhcx','sruxswhcx','sruyswhcx' &
                  ,'sruzswhcx','spswhcx','seswhcx')
          if(UseNeutralFluid)then
             call select_region(iBlock)
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                call calc_source_inputs( &
                   i,j,k,iBlock,Rho_I,U_DI,U2_I,Temp_I,UThS_I)
                call calc_charge_exchange_source( &
                   i,j,k,iBlock,Rho_I,U_DI,U2_I,UThS_I,Source_V)
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

       case( 'srpuicx','sruxpuicx','sruypuicx' &
                  ,'sruzpuicx','sppuicx','sepuicx')
          if(UseNeutralFluid)then
             call select_region(iBlock)
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                call calc_source_inputs( &
                   i,j,k,iBlock,Rho_I,U_DI,U2_I,Temp_I,UThS_I)
                call calc_charge_exchange_source( &
                   i,j,k,iBlock,Rho_I,U_DI,U2_I,UThS_I,Source_V)
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

       case( 'srswhimp','sruxswhimp','sruyswhimp' &
                  ,'sruzswhimp','spswhimp','seswhimp')
          if(UseNeutralFluid)then
             call select_region(iBlock)
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                call calc_source_inputs( &
                   i,j,k,iBlock,Rho_I,U_DI,U2_I,Temp_I,UThS_I)
                call calc_electron_impact_source( &
                   i,j,k,iBlock,Rho_I,U_DI,U2_I,UThS_I,Source_V)
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

       case( 'srpuiimp','sruxpuiimp','sruypuiimp' &
                  ,'sruzpuiimp','sppuiimp','sepuiimp')
          if(UseNeutralFluid)then
             call select_region(iBlock)
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                call calc_source_inputs( &
                   i,j,k,iBlock,Rho_I,U_DI,U2_I,Temp_I,UThS_I)
                call calc_electron_impact_source( &
                   i,j,k,iBlock,Rho_I,U_DI,U2_I,UThS_I,Source_V)
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
       case('sRho')
          NameTecVar = 'SRho'
          PlotVar_G(1:nI,1:nJ,1:nK) = Source_VC(NeuRho_,:,:,:)
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

       case( 'srcx','sruxcx','sruycx' &
                  ,'sruzcx','spcx','secx')
          if(UseNeutralFluid)then
             call select_region(iBlock)
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                call calc_source_inputs( &
                   i,j,k,iBlock,Rho_I,U_DI,U2_I,Temp_I,UThS_I)
                call calc_charge_exchange_source( &
                   i,j,k,iBlock,Rho_I,U_DI,U2_I,UThS_I,Source_V)
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

       case( 'srph','sruxph','sruyph' &
                  ,'sruzph','spph','seph')
          if(UseNeutralFluid .and. UsePhotoion)then
             call select_region(iBlock)
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                call calc_source_inputs( &
                   i,j,k,iBlock,Rho_I,U_DI,U2_I,Temp_I,UThS_I)
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

       case( 'srimp','sruximp','sruyimp' &
                  ,'sruzimp','spimp','seimp')
          if(UseNeutralFluid)then
             call select_region(iBlock)
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                call calc_source_inputs( &
                   i,j,k,iBlock,Rho_I,U_DI,U2_I,Temp_I,UThS_I)
                call calc_electron_impact_source( &
                   i,j,k,iBlock,Rho_I,U_DI,U2_I,UThS_I,Source_V)
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
    integer :: iFluid

    real :: State_V(nVar)
    real :: Ux, Uy, Uz, U2, SWHUx, SWHUy, SWHUz, SWHU2, &
         Pu3Ux, Pu3Uy, Pu3Uz, Pu3U2
    real:: RhoSWH, uSWH_D(3), sRho, sRhoU_D(3), sEnergy, &
         RhoPu3, uPu3_D(3), USWH2, UPu32
    real, dimension(nVar + nFluid):: Source_V, SourceCx_V, SourcePh_V, &
         SourceImp_V

    real, dimension(nFluid) :: U2_I, Temp_I, Rho_I, UThS_I

    real :: U_DI(3,nFluid)

    integer :: i, j, k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_calc_sources_expl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

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
       ! Calculate Rho, U, U^2, Temp, and UTh^2 for source terms
       call calc_source_inputs( &
          i,j,k,iBlock,Rho_I,U_DI,U2_I,Temp_I,UThS_I)

       ! Charge Exchange
       call calc_charge_exchange_source( &
          i,j,k,iBlock,Rho_I,U_DI,U2_I,UThS_I,SourceCx_V)

       ! Photoionization
       if(UsePhotoion)then
          call calc_photoion_source( &
             i,j,k,iBlock,U_DI,U2_I,SourcePh_V)
       else
          SourcePh_V = 0.0
       end if

       ! Electron Impact Ionization
       if(UseElectronImpact .and. UseElectronPressure)then
          call calc_electron_impact_source( &
             i,j,k,iBlock,Rho_I,U_DI,U2_I,UThS_I,SourceImp_V)
       else
          SourceImp_V = 0.0
       end if

       if(.not.IsMhd)then
          ! Heating Pu3
          HeatPu3 = 0.0
          if(UsePu3Heating) HeatPu3 = &
             State_V(PU3Rho_)*(TempPu3 - Temp_I(PU3_))*&
             Io2No_V(UnitTemperature_)*(r_GB(i,j,k,iBlock)-rBody)*FactorPu3

       end if

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
      integer :: iVar, iFluid
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
            end if
         end if
      end if

      Source_V = Source_V + SourceCx_V + SourcePh_V + SourceImp_V

      Source_VC(:,i,j,k) = Source_VC(:,i,j,k) + Source_V

      if(.not.allocated(FluidSource_ICB)) &
           allocate(FluidSource_ICB(nVar+nFluid,nI,nJ,nK,nBlock))
      FluidSource_ICB(:,i,j,k,iBlock) = Source_V

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

         if(.not.IsMhd)then
            write(*,*) ' HeatPu3   ', HeatPu3
         end if
      end if

    end subroutine calc_source_cell
    !==========================================================================
  end subroutine user_calc_sources_expl
  !============================================================================
  subroutine calc_source_inputs( &
     i,j,k,iBlock,Rho_I,U_DI,U2_I,Temp_I,UThS_I)

    ! Calculate parameters to pass to source terms

    integer, intent(in):: i,j,k,iBlock
    real, dimension(nFLuid), intent(out) :: Rho_I, U2_I, Temp_I, UThS_I
    real, intent(out) :: U_DI(3,nFLuid)

    real:: State_V(nVar)

    !--------------------------------------------------------------------------
    State_V = State_VGB(:,i,j,k,iBlock)

    ! Densities
    Rho_I = State_V(iRho_I)*No2Si_V(UnitN_)

    ! Velocities per component
    U_DI(x_,:) = State_V(iRhoUx_I)/State_V(iRho_I)
    U_DI(y_,:) = State_V(iRhoUy_I)/State_V(iRho_I)
    U_DI(z_,:) = State_V(iRhoUz_I)/State_V(iRho_I)

    ! Velocity square for the two ionized and four population of neutrals;
    U2_I = sum(U_DI**2, 1)

    ! Temperature for the two ionized and four population of neutrals (K)
    Temp_I = (State_V(iP_I)/State_V(iRho_I))*No2Si_V(UnitTemperature_)

    ! If not using Pe, electron pressure is grouped with SWH
    if(.not.UseElectronPressure)then
       if(IsMhd)then
          Temp_I(Ion_) = 0.5*Temp_I(Ion_)
       else
          Temp_I(SWH_) = (State_V(SWHP_) &
               /(2*State_V(SWHRho_) + State_V(PU3Rho_))) &
               *No2Si_V(UnitTemperature_)
       endif
    end if

    ! Thermal speed (squared) for ionized and three populations of neutrals
    ! UThS units are (m/s)^2
    UThS_I = (2*cBoltzmann/cProtonMass)*Temp_I

  end subroutine calc_source_inputs
  !============================================================================
  subroutine calc_charge_exchange_source( &
     i,j,k,iBlock,Rho_I,U_DI,U2_I,UThS_I,SourceCx_V)

    ! Calculate the charge exchange source terms for one cell.

    integer, intent(in):: i,j,k,iBlock
    real, dimension(nFluid), intent(in) :: Rho_I, U2_I, UThS_I
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
               Rho_I(Ion_),UThS_I(Ion_), uDIm_DI(:,Ion_), &
               Rho_I(iFluid), UThS_I(iFluid), uDim_DI(:,iFluid), &
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
                  Rho_I(Pu3_),UThS_I(Pu3_), uDim_DI(:,Pu3_), &
                  Rho_I(iFluid), UThs_I(iFluid), uDIm_DI(:,iFluid), &
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
            UTh2Sum_I = UThS_I(SWH_) + UThS_I(Neu_:)

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
               UTh2SumPu3_I = UThS_I(Pu3_) + UThS_I(Neu_:)
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
       ! Cross Section from Lindsay and Stebbings, 2005
       where(UseSource_I(Neu_:)) &
             Sigma_I = ((2.2835E-7 - (1.062E-8)*log(UStarM_I*100.))**2)*1.E-4
       where(UseSource_I(Neu_:)) &
             SigmaN_I = ((2.2835E-7 - (1.062E-8)*log(UStar_I*100.))**2)*1.E-4

       if(.not.IsMhd)then
          ! For Pu3
          where(UseSource_I(Neu_:)) &
                SigmaPu3_I = &
                ((2.2835E-7 - (1.062E-8)*log(UStarMPu3_I*100.))**2)*(1.E-4)
          where(UseSource_I(Neu_:)) &
                SigmaNPu3_I = &
                ((2.2835E-7 - (1.062E-8)*log(UStarPu3_I*100.))**2)*(1.E-4)
       end if

       ! Calculate Rate = \nu * nH * mp where nH is the neutral density
       ! \nu = Sigma*np*u_star where np is the ion density and
       ! For each population of neutrals there will be another Rate
       ! The charge xchange cross section 100 to change ustar to cm/s
       ! Rate has no units (m^2*m/s*s*m-3 )

       if(.not.IsMhd)then
          ! For SW
          where(UseSource_I(Neu_:)) &
               Rate_I = Sigma_I*State_V(SWHRho_) &
               *State_V(iRho_I(Neu_:))*UStarM_I &
               *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)

          ! For PU3
          where(UseSource_I(Neu_:)) &
               RatePu3_I = SigmaPu3_I*State_V(PU3Rho_) &
               *State_V(iRho_I(Neu_:))*UStarMPu3_I &
               *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)

       else
          where(UseSource_I(Neu_:)) &
               Rate_I = Sigma_I*State_V(Rho_) &
               *State_V(iRho_I(Neu_:))*UStarM_I &
               *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)

       end if

       if(.not.IsMhd)then
          ! for SW
          where(UseSource_I(Neu_:)) &
               RateN_I = SigmaN_I*State_V(SWHRho_)&
               *State_V(iRho_I(Neu_:))*UStar_I&
               *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)

          ! For PU3
          where(UseSource_I(Neu_:)) &
               RateNPu3_I = SigmaNPu3_I*State_V(PU3Rho_)&
               *State_V(iRho_I(Neu_:))*UStarPu3_I &
               *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)

       else
          where(UseSource_I(Neu_:)) &
               RateN_I =SigmaN_I*State_V(Rho_)&
               *State_V(iRho_I(Neu_:))*UStar_I  &
               *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)
       end if

       if(.not.IsMhd)then
          ! for SW
          where(UseSource_I(Neu_:)) &
               RateE_I = Sigma_I*State_V(SWHRho_)&
               *State_V(iRho_I(Neu_:))*UStar_I&
               *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)

          ! For PU3
          where(UseSource_I(Neu_:)) &
               RateEPu3_I = SigmaPu3_I*State_V(PU3Rho_)&
               *State_V(iRho_I(Neu_:))*UStarPu3_I &
               *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)

       else
          where(UseSource_I(Neu_:)) &
               RateE_I =Sigma_I*State_V(Rho_)&
               *State_V(iRho_I(Neu_:))*UStar_I  &
               *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)
       end if

       ! For SW
       I0xp_I = RateN_I
       I0px_I = RateN_I

       I2xp_I  = RateE_I*UThS_I(Ion_)/No2Si_V(UnitU_)**2
       I2px_I  = RateE_I(Neu_:)*UThS_I(Neu_:)/No2Si_V(UnitU_)**2

       if(.not.IsMhd)then
          ! For PUI's
          I0xpu3_I = RateNPu3_I
          I0pu3x_I = RateNPu3_I
          I2xpu3_I = RateEPu3_I*UThS_I(PU3_)/&
               No2Si_V(UnitU_)**2
          I2pu3x_I = RateEPu3_I(Neu_:)*UThS_I(Neu_:)/No2Si_V(UnitU_)**2
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
          UMean_DI(x_,:) = (UThS_I(SWH_)*U_DI(x_,Neu_:) &
               + UThS_I(Neu_:)*U_DI(x_,SWH_))/UTh2Sum_I
          UMean_DI(y_,:) = (UThS_I(SWH_)*U_DI(y_,Neu_:) &
               + UThS_I(Neu_:)*U_DI(y_,SWH_))/UTh2Sum_I
          UMean_DI(z_,:) = (UThS_I(SWH_)*U_DI(z_,Neu_:) &
               + UThS_I(Neu_:)*U_DI(z_,SWH_))/UTh2Sum_I

          ! Momentum
          JxpUx_I = JxpUx_I + UMean_DI(x_,:)*(RateN_I - Rate_I)
          JxpUy_I = JxpUy_I + UMean_DI(y_,:)*(RateN_I - Rate_I)
          JxpUz_I = JxpUz_I + UMean_DI(z_,:)*(RateN_I - Rate_I)

          JpxUx_I = JpxUx_I + UMean_DI(x_,:)*(RateN_I - Rate_I)
          JpxUy_I = JpxUy_I + UMean_DI(y_,:)*(RateN_I - Rate_I)
          JpxUz_I = JpxUz_I + UMean_DI(z_,:)*(RateN_I - Rate_I)

          ! Energy
          Kxp_I = Kxp_I + 0.5*Sum(UMean_DI**2, 1)*(RateN_I - Rate_I)&
               + (0.75*RateN_I - RateE_I)*UThS_I(SWH_)*UThS_I(Neu_:)/&
               UTh2Sum_I/No2Si_V(UnitU_)**2
          Kpx_I = Kpx_I + 0.5*Sum(UMean_DI**2, 1)*(RateN_I - Rate_I)&
               + (0.75*RateN_I - RateE_I)*UThS_I(SWH_)*UThS_I(Neu_:)/&
               UTh2Sum_I/No2Si_V(UnitU_)**2

          ! For PUIs
          if(.not.IsMhd)then
             UMeanPu3_DI(x_,:) = &
                  (UThS_I(Pu3_)*U_DI(x_,Neu_:) + UThS_I(Neu_:)*U_DI(x_,Pu3_))/&
                  UTh2SumPu3_I
             UMeanPu3_DI(y_,:) = &
                  (UThS_I(Pu3_)*U_DI(y_,Neu_:) + UThS_I(Neu_:)*U_DI(y_,Pu3_))/&
                  UTh2SumPu3_I
             UMeanPu3_DI(z_,:) = &
                  (UThS_I(Pu3_)*U_DI(z_,Neu_:) + UThS_I(Neu_:)*U_DI(z_,Pu3_))/&
                  UTh2SumPu3_I

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
                  *UThS_I(Pu3_)*UThS_I(Neu_:)/&
                  UTh2SumPu3_I/No2Si_V(UnitU_)**2
             Kpu3x_I = Kpu3x_I &
                  + 0.5*Sum(UMeanPu3_DI**2, 1)&
                  *(RateNPu3_I - RatePu3_I)&
                  + (0.75*RateNPu3_I - RateEPu3_I)*&
                  UThS_I(Pu3_)*UThS_I(Neu_:)/&
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

    ! Calculate the photoionization source terms for one cell.

    integer, intent(in):: i,j,k,iBlock
    real, intent(in):: U_DI(3,nFluid)
    real, intent(in) :: U2_I(nFluid)
    real, intent(out):: SourcePh_V(nVar + nFluid)

    integer :: iFluid
    real :: r
    real, dimension(Neu_:Ne4_):: RatePh_I, I0xpPh_I, JxpUxPh_I, JxpUyPh_I, &
            JxpUzPh_I, ExpPh_I, KxpPh_I
    real :: State_V(nVar)

    ! Chika. Photoionization rate
    character(len=*), parameter:: NameSub = 'calc_photoion_source'
    !--------------------------------------------------------------------------
    SourcePh_V = 0.0

    ! Only defined for Single Ion (for now)
    if(.not.IsMhd) RETURN

    State_V = State_VGB(:,i,j,k,iBlock)
    r = r_GB(i,j,k,iBlock)

    ! rate_ph = 8E-8*(r0/r)**2
    ! 8E-8 has units 1/s
    ! r0 = 1 AU
    where(UseSource_I(Neu_:)) &
         RatePh_I = 8E-8*(1/(r+1e-10))**2*State_V(iRho_I(Neu_:)) &
         * No2Si_V(UnitT_)

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
               State_V(iRho_I(Neu_:))/(GammaMinus1)

    ! Chika: Energy source term
    ! Table 2 Fahr et al. 2000
    KxpPh_I = RatePh_I*ExpPh_I

    do iFluid = Neu_, Ne4_
        if(.not.UseSource_I(iFluid)) CYCLE
        call select_fluid(iFluid)
        SourcePh_V(iRho)    = -I0xpPh_I(iFluid)
        SourcePh_V(iRhoUx)  = -JxpUxPh_I(iFluid)
        SourcePh_V(iRhoUy)  = -JxpUyPh_I(iFluid)
        SourcePh_V(iRhoUz)  = -JxpUzPh_I(iFluid)
        SourcePh_V(iEnergy) = -KxpPh_I(iFluid)

        SourcePh_V(iP) = (Gamma-1)* ( SourcePh_V(iEnergy) &
             - sum(U_DI(:,iFluid)*SourcePh_V(iRhoUx:iRhoUz)) &
             + 0.5*U2_I(iFluid)*SourcePh_V(iRho) )
    end do

    if(UseSource_I(Ion_))then
        SourcePh_V(Rho_)    = sum(I0xpPh_I)
        SourcePh_V(RhoUx_)  = sum(JxpUxPh_I)
        SourcePh_V(RhoUy_)  = sum(JxpUyPh_I)
        SourcePh_V(RhoUz_)  = sum(JxpUzPh_I)
        SourcePh_V(Energy_) = sum(KxpPh_I)

        SourcePh_V(P_) = (Gamma-1)* ( SourcePh_V(Energy_) &
             - sum(U_DI(:,Ion_)*SourcePh_V(RhoUx_:RhoUz_)) &
             + 0.5*U2_I(Ion_)*SourcePh_V(Rho_) )
    end if

  end subroutine calc_photoion_source
  !============================================================================
  subroutine calc_electron_impact_source( &
     i,j,k,iBlock,Rho_I,U_DI,U2_I,UThS_I,SourceImp_V)

    ! Calculate the electron impact source terms for one cell.
    ! Requires a separate electron pressure

    integer, intent(in):: i,j,k,iBlock
    real, dimension(nFluid), intent(in):: Rho_I, U2_I, UThS_I
    real, intent(in) :: U_DI(3,nFluid)
    real, intent(out):: SourceImp_V(nVar + nFluid)

    integer :: iFluid
    real, dimension(Neu_:Ne4_):: SrcImpRho_I, &
         SrcImpRhoUx_I, SrcImpRhoUy_I, SrcImpRhoUz_I, &
         SrcImpEnergy_I
    real, dimension(3,nFluid):: uDim_DI
    real :: State_V(nVar)
    real :: RhoEleNo, RhoEleSi, TempEle, UthSEle, IonizationEnergy, SrcImpPe
    real, dimension(3):: Uion_D, UEle_D, Current_D
    real :: SrcImp_II(Neu_:Ne4_,5)

    character(len=*), parameter:: NameSub = 'calc_electron_impact_source'
    !--------------------------------------------------------------------------

    SourceImp_V = 0.0

    ! Requires a separate electron pressure
    if (.not.UseElectronPressure) RETURN

    State_V = State_VGB(:,i,j,k,iBlock)

    ! uDIm_DI  = State_V(iRhoUx_I:iRhoUz_I)/State_V(iRho_I)*No2Si_V(UnitU_)
    uDim_DI = U_DI*No2Si_V(UnitU_)

    if (IsMhd) then
       ! Electron density in normalized units
       RhoEleNo = State_V(Ion_)

       ! Average Ion Velocity
       UIon_D = State_V(RhoUx_:RhoUz_)/RhoEleNo
    else
       ! Electron density in normalized units
       RhoEleNo = State_V(SWHRho_) + State_V(Pu3Rho_)

       ! Average Ion Velocity
       UIon_D = (State_V(SWHRhoUx_:SWHRhoUz_) + &
               State_V(Pu3RhoUx_:Pu3RhoUz_))/RhoEleNo
    end if

    ! Electron density in SI units
    RhoEleSi = RhoEleNo*No2Si_V(UnitN_)

    ! Get electron velocity from the current and positive ion velocities
    call get_current(i, j, k, iBlock, Current_D)
    UEle_D = (UIon_D - Current_D*IonMassPerCharge/RhoEleNo) &
               *No2Si_V(UnitU_)

    ! Electron temperature
    TempEle = State_V(Pe_)/RhoEleNo*No2Si_V(UnitTemperature_)

    ! Electron thermal speed squared
    UThSEle = (2*cBoltzmann/cElectronMass)*TempEle

    ! Electron Ionization Energy
    IonizationEnergy = cRyToEv*cEv

    if(iTableElectronImpact > 0)then
       do iFluid = Neu_, Ne4_
          call get_collision( &
             ElectronImpact_, &
             Rho_I(iFluid), UThS_I(iFluid), uDim_DI(:,iFLuid), &
             RhoEleSi, UThSEle, UEle_D, &
             SrcImp_II(iFluid,:))
       enddo
    end if

    where(UseSource_I(Neu_:)) SrcImpRho_I    = SrcImp_II(Neu_:,1)
    where(UseSource_I(Neu_:)) SrcImpRhoUx_I  = SrcImp_II(Neu_:,2)
    where(UseSource_I(Neu_:)) SrcImpRhoUy_I  = SrcImp_II(Neu_:,3)
    where(UseSource_I(Neu_:)) SrcImpRhoUz_I  = SrcImp_II(Neu_:,4)
    where(UseSource_I(Neu_:)) SrcImpEnergy_I = SrcImp_II(Neu_:,5)
    ! Pressure source term for the electrons
    ! Ionization Energy * total ionization rate
    SrcImpPe = -sum(SrcImp_II(Neu_:,1), MASK=UseSource_I(Neu_:)) &
             *IonizationEnergy*GammaElectronMinus1/cProtonMass &
             *No2Si_V(UnitRho_)/No2Si_V(UnitP_)

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

    ! Ion source terms
    if(IsMhd) then
       ! Single Ion
       if (UseSource_I(Ion_)) then
          SourceImp_V(Rho_)    = sum(SrcImpRho_I)
          SourceImp_V(RhoUx_)  = sum(SrcImpRhoUx_I)
          SourceImp_V(RhoUy_)  = sum(SrcImpRhoUy_I)
          SourceImp_V(RhoUz_)  = sum(SrcImpRhoUz_I)
          SourceImp_V(Energy_) = sum(SrcImpEnergy_I)

          SourceImp_V(P_) = (Gamma-1)* ( SourceImp_V(Energy_) &
             - sum(U_DI(:,Ion_)*SourceImp_V(RhoUx_:RhoUz_)) &
             + 0.5*U2_I(Ion_)*SourceImp_V(Rho_) )
       end if
    else
       ! Multi Ion
       if(UseSource_I(SWH_) .and. UseSource_I(Pu3_)) then
          if (Ne3_ == iFluidProduced_C(i,j,k)) then
             ! inside region 3
             ! Pu3 is created here
             SourceImp_V(Pu3Rho_)    = sum(SrcImpRho_I)
             SourceImp_V(Pu3RhoUx_)  = sum(SrcImpRhoUx_I)
             SourceImp_V(Pu3RhoUy_)  = sum(SrcImpRhoUy_I)
             SourceImp_V(Pu3RhoUz_)  = sum(SrcImpRhoUz_I)
             SourceImp_V(Pu3Energy_) = sum(SrcImpEnergy_I)

             SourceImp_V(Pu3P_) = (Gamma-1)* ( SourceImp_V(Pu3Energy_) &
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

             SourceImp_V(SWHp_) = (Gamma-1)* ( SourceImp_V(SWHEnergy_) &
                       - sum(U_DI(:,SWH_)*SourceImp_V(SWHRhoUx_:SWHRhoUz_)) &
                       + 0.5*U2_I(SWH_)*SourceImp_V(SWHRho_) )
          end if
       end if
    end if

    ! Electron Pressure source term
    SourceImp_V(Pe_) = SrcImpPe

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
  subroutine select_region(iBlock)

    ! set the global variabls iFluidProduced_C
    ! to select which neutral fluid is produced in each cell of the block

    integer, intent(in):: iBlock

    integer :: i, j, k

    ! cold cloud
    real :: RhoDim, InvRho, U2, p, Mach2, MachSW2, TempDim,  U2Dim, Rho
    real :: pSW, InvRhoSW, MachPUI2, pPUI, InvRhoPUI, RhoPUI, RhoSw

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
                  + State_VGB(Pu3Rho_,i,j,k,iBlock)
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

       RhoDim = Rho*No2Si_V(UnitRho_)
       U2Dim = U2*No2Io_V(UnitU_)**2
       TempDim = 0.5*p*InvRho*No2Si_V(UnitTemperature_)

       ! Square of Mach number
       Mach2 = U2/(Gamma*p*InvRho)

       ! Apply region formulas
       select case(iRegionFormula)

       case(SingleIon_)
           if(r_GB(i,j,k,iBlock) < rPop3Limit) then
              iFluidProduced_C(i,j,k) = Ne3_
              CYCLE
           end if

           if (MachPop4Limit**2 < Mach2 &
                   .and. uPop1LimitDim**2 > U2Dim) then
              ! Outside the bow shock
              iFluidProduced_C(i,j,k) = Ne4_
           elseif(TempDim < TempPop1LimitDim &
                           .and. U2Dim < uPop1LimitDim**2)then
              ! Outside the heliopause
              iFluidProduced_C(i,j,k) = Neu_
           elseif( Mach2 < MachPop2Limit**2 )then
              ! Heliosheath
              iFluidProduced_C(i,j,k) = Ne2_
           elseif( Mach2 > MachPop3Limit**2 )then
              !! before 85K      elseif( MachSW2 > MachPop3Limit**2 )then
              ! Inside termination shock
              iFluidProduced_C(i,j,k) = Ne3_
           else
              ! No neutrals are produced in this region
              ! but they are destroyed
              iFluidProduced_C(i,j,k) = 0
           end if

           if (.not.IsMhd) then
              ! adding more conditions to help with the regions
              if (MachSW2 > MachPop4Limit**2 &
                      .and. MachPUI2 < MachPUIPop3**2 ) then
                 iFluidProduced_C(i,j,k) = Ne2_
              end if
              if (MachSW2 < MachPop3Limit**2 &
                      .and. MachPUI2 > MachPUIPop3**2 &
                   .and. r_GB(i,j,k,iBlock)<rPop3Limit) then
                 iFluidProduced_C(i,j,k) = Ne3_
              end if
           endif

       case(ColdCloud_)
           if(DoInitializeCloud)then
              if(r_GB(i,j,k,iBlock) > rPop3Limit) then
                 ! Outside the bow shock
                 iFluidProduced_C(i,j,k) = Ne4_
              else
                 ! Outside the heliopause
                 iFluidProduced_C(i,j,k) = Ne3_
              endif
           else
              !! for studies of heliosphere with cold cloud
              if(r_GB(i,j,k,iBlock) < rPop3Limit) then
                 iFluidProduced_C(i,j,k) = Ne3_
                 CYCLE
              end if

              if (TempPop1LimitDim > TempDim &
                   .and. RhoPop4LimitDim > RhoDim) then
                 ! Outside the bow shock
                 iFluidProduced_C(i,j,k) = Ne4_
              elseif (TempPop1LimitDim > TempDim &
                   .and. RhoDim > RhoPop4LimitDim) then
                 ! Outside the heliopause
                 iFluidProduced_C(i,j,k) = Neu_
              elseif(TempDim > TempPop1LimitDim &
                   .and. uPop1LimitDim**2 > U2Dim)then
                 ! Heliosheath
                 iFluidProduced_C(i,j,k) = Ne2_
              elseif(TempDim > TempPop1LimitDim .and. &
                   U2Dim > uPop1LimitDim**2)then
                 ! Inside termination shock
                 iFluidProduced_C(i,j,k) = Ne3_
              else
                 ! No neutrals are produced in this region
                 ! but they are destroyed
                 iFluidProduced_C(i,j,k) = 0
              end if
           endif

       case(MultiIon_)
           if (r_GB(i,j,k,iBlock) < rBody) then
              ! inside inner boundary (inside termination shock)
              iFluidProduced_C(i,j,k) = Ne3_
           elseif (U2Dim > uPop3LimitDim**2 &
                           .and. Mach2 > MachPop3Limit**2) then
              ! inside termination shock
              iFluidProduced_C(i,j,k) = Ne3_
           elseif (TempDim > TempPop2LimitDim) then
              ! inside heliosheath
              iFluidProduced_C(i,j,k) = Ne2_
           elseif (U2Dim < uPop1LimitDim**2 &
                           .and. Mach2 < MachPop1Limit**2) then
              ! inside bowshock
              iFluidProduced_C(i,j,k) = Neu_
           else
              ! outside bowshock
              iFluidProduced_C(i,j,k) = Ne4_
           endif

       case default
           call CON_stop(NameSub// &
                   ' : unexpected region formulas.')
       end select
    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine select_region
  !============================================================================
  subroutine user_init_session

    use ModMain,          ONLY: UseUserUpdateStates
    use ModLookupTable,   ONLY: i_lookup_table, get_lookup_table
    use ModWaves,         ONLY: UseWavePressure, UseAlfvenWaves
    use ModVarIndexes,    ONLY: WaveFirst_

    real:: IndexMax_I(2)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_init_session'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    UseAlfvenWaves  = WaveFirst_ > 1
    UseWavePressure = WaveFirst_ > 1

    DeltaU = DeltaUSi*Si2No_V(UnitU_)
    LperpTimesSqrtB = LperpTimesSqrtBSi*Si2No_V(UnitX_)*sqrt(Si2No_V(UnitB_))

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
            call get_lookup_table(iTableSolarWind,Time=TableStart, &
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
            if(RunStart >= TableStart .and. RunStart <= &
            TableStart+(IndexMax_I(2)/cSecondPerYear))then
               Offset = Runstart - TableStart ! years
            end if
         end if
    end if

    if(iTableElectronImpact < 0) &
         iTableElectronImpact=i_lookup_table('ElectronImpact')

    call test_stop(NameSub, DoTest)

  end subroutine user_init_session
  !============================================================================
  subroutine get_collision( iTypeCollision, &
     RhoA, Cs2A, uA_D, RhoB, Cs2B, uB_D, &
     SourceA_V, SourceB_V)

     use ModLookupTable, ONLY: interpolate_lookup_table, i_lookup_table, &
        Table_I

    integer, intent(in):: iTypeCollision
    real, intent(in):: RhoA      ! fluid A mass density
    real, intent(in):: Cs2A      ! fluid A thermal speed squared
    real, intent(in):: uA_D(3)   ! fluid A bulk velocity
    real, intent(in):: RhoB      ! fluid B mass density
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
    Integral_V = Integral_V * RhoA * RhoB

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
