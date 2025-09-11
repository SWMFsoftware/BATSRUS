!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
!#NOTPUBLIC  email:zghuang@umich.edu  expires:12/31/2099
module ModUser

  use ModUserEmpty,               &
       IMPLEMENTED1  => user_set_boundary_cells,         &
       IMPLEMENTED2  => user_read_inputs,                &
       IMPLEMENTED3  => user_init_session,               &
       IMPLEMENTED4  => user_set_face_boundary,          &
       IMPLEMENTED5  => user_calc_sources_expl,          &
       IMPLEMENTED6  => user_calc_sources_impl,          &
       IMPLEMENTED7  => user_update_states,              &
       IMPLEMENTED8  => user_set_resistivity,            &
       IMPLEMENTED9  => user_material_properties,        &
       IMPLEMENTED10 => user_init_point_implicit,        &
       IMPLEMENTED11 => user_set_plot_var,               &
       IMPLEMENTED12 => user_set_ICs,                    &
       IMPLEMENTED13 => user_get_log_var,                &
       IMPLEMENTED14 => user_set_cell_boundary,          &
       IMPLEMENTED15 => user_amr_criteria,               &
       IMPLEMENTED16 => user_initial_perturbation,       &
       IMPLEMENTED17 => user_action

  use BATL_lib, ONLY: &
       test_start, test_stop, &
       iTest, jTest, kTest, iBlockTest, iProcTest, iVarTest, iProc
  use ModSize
  use ModNumConst, ONLY: cPi, cTiny
  use ModAdvance, ONLY: Pe_, UseElectronPressure
  use ModMultiFluid

  include 'user_module.h' ! list of public methods

  ! Here you must define a user routine Version number and a
  ! descriptive string.
  character (len=*), parameter :: NameUserFile = "ModUserCometCGfluids.f90"
  character (len=*), parameter :: NameUserModule = &
       'CG Comet, Z. Huang and G. Toth'

  character (len=100) :: NameShapeFile

  ! Use the CG shape or not. If not, then use a spherical body.
  logical :: DoUseCGShape = .true.
  real    :: rSphericalBodySi = 2.0e3
  real    :: rSphericalBody

  integer:: nTriangle
  real, allocatable:: XyzTriangle_DII(:,:,:), Normal_DI(:,:)
  real :: rMinShape = 0.0, rMaxShape = 0.0

  ! Position of the sun at the start time
  real :: LatSun=43.5, LonSun=0.0
  real :: NormalSun_D(3)

  ! Rotation matrix to rotate the comet so that the Sun is in the +x direction.
  real:: Rot_DD(3,3) = 0.

  ! Rotation of the comet (changes the direction of the Sun)
  real:: RotationCometHour = 12.0

  ! Angular velocity
  real:: OmegaComet

  ! Maximum change in longitude before updating the boundary conditions
  real:: AngleUpdateDeg = 10.0

  ! The time between updates
  real:: DtUpdateSi

  ! minimum and maximum temperature
  real :: TempCometMinDim, TempCometMaxDim, TempCometMin, TempCometMax

  ! Temperature at 75.5 degree to determin the temperature slope
  real :: TempComet75Dim, TempComet75

  ! Minimum and maximum production rate
  real :: ProductionRateMaxSi, ProductionRateMinSi
  real :: ProductionRateMax, ProductionRateMin

  ! Maximum solar zenith angle for dayside production rate
  real :: SolarAngleMaxDim, SolarAngleMax

  ! Parameters for y=ax+b to mimic the production rate and
  ! temperature distribution
  real :: SlopeProduction, bProduction, SlopeTemp, bTemp

  ! Constant parameters to calculate uNormal and temperature
  ! from TempCometLocal
  real :: TempToUnormal
  real :: TempToPressure

  ! Inner boundary condition for ions
  character (len=10) :: TypeBodyBC = 'default'
  logical :: UseSwBC        = .false.
  logical :: UseReflectedBC = .false.

  ! Inner boundary condition for magnetic field
  character (len=10) :: TypeBfieldBody = 'zero'

  ! Perturbed initial condition parameters:
  real :: R0PerturbedSi  = 1e6, R0Perturbed
  real :: R1PerturbedSi  = 2e6, R1Perturbed
  real :: ratioPerturbed = 1e-10

  ! Parameters to increase the neutral density in the source term to
  ! approach the steady state solution more easily (hopefully)
  logical :: DoEnhanceNeu    = .false.
  real    :: EnhancedRatio   = 20
  integer :: DecadeDn        = 10000
  integer :: nStepEnhanceNeu = -100

  ! minimum temperature for neutral
  real :: TempNeuMinSi = 60.0, TempNeuMin

  ! FaceCoordsTest_D
  real :: FaceCoordsX=0.0, FaceCoordsY=0.0, FaceCoordsZ=0.0
  real :: FaceCoordsTest_D(3) = [0.0, 0.0, 0.0]

  ! Last step and time the inner boundary values were saved for each block
  integer, allocatable :: nStepSave_B(:)

  real, allocatable :: TimeSimulationSave_B(:)

  integer, allocatable :: nStepSaveCalcRates_B(:)
  integer :: nStepPritSetFace = -100

  ! If this variable is set .true., then use the Haser neutral background
  ! Only for testing purpose
  logical :: DoUseHaserBackground = .false.

  ! If this variable is set .true., then use the uniform neutral background
  ! Only for testing purpose
  logical :: DoUseUniformNeuBackground = .false.
  real :: nNeuUniformSi, UxNeuUniformSi, UyNeuUniformSi, UzNeuUniformSi, &
       TNeuUniformSi
  real :: nNeuUniform, UxNeuUniform, UyNeuUniform, UzNeuUniform, &
       pNeuUniform

  ! Increase ionization near a field line
  logical :: DoUseFieldlineFile = .false.
  character (len=100) :: NameFieldlineFile
  integer :: nVarFieldlineFile
  real    :: RadiusTubeSI, RadiusTube, SPeAdditionalSi=5.0e-9, SPeAdditional
  real, allocatable::  XyzFieldline_DI(:,:)
  real    :: TimeEnhanceStartSI = 180.0, TimeEnhanceEndSI = 240.0
  real    :: TimeEnhanceStart, TimeEnhanceEnd

  integer, parameter, public :: nNeuFluid = 1
  integer, parameter :: Neu1_  =  1

  !! Ion species names
  integer, parameter :: SW_   =  1
  integer, parameter :: H2Op_ =  2

  real :: Qprod = 1e22
  real :: TminSi, Tmin, rHelio, vHI, uHaser

  real, allocatable :: ne20eV_GB(:,:,:,:)

  !! logical variable to determine whether to use an artifical perturbed solar
  !! wind at beyond a certain xPerturbedSwIO
  logical :: UsePerturbedSW = .false.
  real :: xPerturbedSwMinIO = 1.5e4, xPerturbedSwMaxIO = 1.6e4
  real :: PerturbedSwNIO = 10.0
  real :: PerturbedSwUxIO = 400.0, PerturbedSwUyIO = 0.0, PerturbedSwUzIO = 0.0
  real :: PerturbedSwTIO = 1.5e5, PerturbedSwTeIO = 1.5e5
  real :: PerturbedSwBxIO = 5.0, PerturbedSwByIO = 0.0, PerturbedSwBzIO = 0.0
  real :: xPerturbedSwMin, xPerturbedSwMax, PerturbedSwN, PerturbedSwRho
  real :: PerturbedSwT,  PerturbedSwTe
  real :: PerturbedSwUx, PerturbedSwUy, PerturbedSwUz
  real :: PerturbedSwBx, PerturbedSwBy, PerturbedSwBz

  !! Make the photo- and electron impact ionization rate global arrays for
  !! user_set_plot_var
  real, allocatable :: v_IIGB(:,:,:,:,:,:), ve_IIGB(:,:,:,:,:,:)

  logical            :: DoSaveSource = .false.
  character (len=10) :: NameSource   = 'none'
  real, allocatable :: TestArray_IGB(:,:,:,:,:)

  real, allocatable :: SPeAdditional_GB(:,:,:,:)

  character (len=6), parameter, public :: NameNeutral_I(nNeuFluid) = &
       [ 'Neu1  ' ]

contains
  !============================================================================
  subroutine user_action(NameAction)

    character(len=*), intent(in):: NameAction

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_action'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if (NameAction /= 'initialize module' .and. NameAction /= 'clean module') &
         RETURN
    if(iProc==0)write(*,*) NameSub,' called with action ',NameAction
    select case(NameAction)
    case('initialize module')
       if(.not.allocated(nStepSave_B)) then
          allocate(nStepSave_B(MaxBlock))
          nStepSave_B = -100
          allocate(TimeSimulationSave_B(MaxBlock))
          TimeSimulationSave_B = -1e30
          allocate(nStepSaveCalcRates_B(MaxBlock))
          nStepSaveCalcRates_B = -100
          allocate(ne20eV_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
          ne20eV_GB = 0.
          allocate(SPeAdditional_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
          SPeAdditional_GB =0.0
          allocate(v_IIGB(nNeuFluid,nIonFluid,&
               MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
          allocate(ve_IIGB(1:nNeuFluid,1:nIonFluid, &
               MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
          allocate(TestArray_IGB(8, MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
          TestArray_IGB = 0.0
       end if
    case('clean module')
       if(allocated(nStepSave_B)) deallocate(nStepSave_B)
       if(allocated(TimeSimulationSave_B)) deallocate(TimeSimulationSave_B)
       if(allocated(nStepSaveCalcRates_B)) deallocate(nStepSaveCalcRates_B)
       if(allocated(ne20eV_GB)) deallocate(ne20eV_GB)
       if(allocated(SPeAdditional_GB)) deallocate(SPeAdditional_GB)
       if(allocated(v_IIGB)) deallocate(v_IIGB)
       if(allocated(ve_IIGB)) deallocate(ve_IIGB)
       if(allocated(TestArray_IGB)) deallocate(TestArray_IGB)
    end select
    call test_stop(NameSub, DoTest)
  end subroutine user_action
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

       case("#SHAPEFILE")
          call read_var('NameShapeFile' ,NameShapeFile)
       case("#USECGSHAPE")
          call read_var('DoUseCGShape',     DoUseCGShape)
          call read_var('rSphericalBodySi', rSphericalBodySi)
       case("#USEHASERBACKGROUND")
          call read_var('DoUseHaserBackground', DoUseHaserBackground)
       case("#USEUNIFORMNEUBACKGROUND")
          call read_var('DoUseUniformNeuBackground', DoUseUniformNeuBackground)
          if (DoUseUniformNeuBackground) then
             call read_var('nNeuUniformSi',  nNeuUniformSi)
             call read_var('UxNeuUniformSi', UxNeuUniformSi)
             call read_var('UyNeuUniformSi', UyNeuUniformSi)
             call read_var('UzNeuUniformSi', UzNeuUniformSi)
             call read_var('TNeuUniformSi',  TNeuUniformSi)
          end if
       case("#SUNDIRECTION")
          call read_var('LatSun', LatSun)
          call read_var('LonSun', LonSun)
       case("#COMETSTATE")
          call read_var('ProductionRateMinSi', ProductionRateMinSi)
          call read_var('ProductionRateMaxSi', ProductionRateMaxSi)
          call read_var('SolarAngleMaxDim',    SolarAngleMaxDim)
          call read_var('TempCometMinDim',     TempCometMinDim)
          call read_var('TempCometMaxDim',     TempCometMaxDim)
          call read_var('TempComet75',         TempComet75Dim)
       case("#IONIZATIONPARAM")
          ! Heliocentric distance [AU]
          call read_var('rHelio', rHelio)

          ! Ionization frequency for cometary heavy ions
          ! (1/lifetime of cometary heavy neutrals)
          call read_var('vHI', vHI)

          ! Minimum ion temperature (enforced in update states)
          call read_var('TminSi', TminSi)

          ! Total production rate
          call read_var('Qprod', Qprod)

          ! velocity for Haser model used in preset conditions
          call read_var('uHaser', uHaser)
       case("#COMETROTATION")
          call read_var('RotationCometHour', RotationCometHour)
          call read_var('AngleUpdateDeg',    AngleUpdateDeg)
       case("#BODYBC")
          call read_var('TypeBodyBC', TypeBodyBC)
          select case(TypeBodyBC)
          case('solarwind')
             UseSwBC        = .true.
             UseReflectedBC = .false.
          case('reflected')
             UseSwBC        = .false.
             UseReflectedBC = .true.
          case('default')
             UseSwBC        = .false.
             UseReflectedBC = .false.
          case default
             if(iProc==0) call stop_mpi( &
                  NameSub//' invalid body type='//trim(NameCommand))
          end select
       case("#TYPEBFIELDBODY")
          call read_var('TypeBfieldBody', TypeBfieldBody)
       case("#TESTFACECOORDS")
          call read_var('FaceCoordsX', FaceCoordsX)
          call read_var('FaceCoordsY', FaceCoordsY)
          call read_var('FaceCoordsZ', FaceCoordsZ)
       case("#PERTURBEDCONDITIONS")
          call read_var('R0PerturbedSi' , R0PerturbedSi)
          call read_var('R1PerturbedSi' , R1PerturbedSi)
          call read_var('ratioPerturbed', ratioPerturbed)
       case("#MINIMUMNEUTEMPERATURE")
          call read_var('TempNeuMinSi', TempNeuMinSi)
       case('#ENHANCENEU')
          call read_var('DoEnhanceNeu',  DoEnhanceNeu)
          call read_var('EnhancedRatio', EnhancedRatio)
          call read_var('DecadeDn',      DecadeDn)
       Case('#USEPERTURBEDSW')
          call read_var('UsePerturbedSW',    UsePerturbedSW)
          call read_var('xPerturbedSwMinIO', xPerturbedSwMinIO)
          call read_var('xPerturbedSwMaxIO', xPerturbedSwMaxIO)
          call read_var('PerturbedSwNIO',    PerturbedSwNIO)
          call read_var('PerturbedSwTIO',    PerturbedSwTIO)
          call read_var('PerturbedSwTeIO',   PerturbedSwTeIO)
          call read_var('PerturbedSwUxIO',   PerturbedSwUxIO)
          call read_var('PerturbedSwUyIO',   PerturbedSwUyIO)
          call read_var('PerturbedSwUzIO',   PerturbedSwUzIO)
          call read_var('PerturbedSwBxIO',   PerturbedSwBxIO)
          call read_var('PerturbedSwByIO',   PerturbedSwByIO)
          call read_var('PerturbedSwBzIO',   PerturbedSwBzIO)
       case('#USEFIELDLINEFILE')
          call read_var('DoUseFieldlineFile', DoUseFieldlineFile)
          call read_var('NameFieldlineFile',  NameFieldlineFile)
          call read_var('nVarFieldlineFile',  nVarFieldlineFile)
          call read_var('RadiusTubeSI',       RadiusTubeSI)
          call read_var('SPeAdditionalSi',    SPeAdditionalSi)
          call read_var('TimeEnhanceStartSI', TimeEnhanceStartSI)
          call read_var('TimeEnhanceEndSI',   TimeEnhanceEndSI)
       case('#SAVESOURCE')
          call read_var('DoSaveSource',       DoSaveSource)
          call read_var('NameSource',         NameSource)
       case('#USERINPUTEND')
          EXIT
       case default
          if(iProc==0) call stop_mpi( &
               NameSub//' invalid command='//trim(NameCommand))
       end select
    end do

    call test_stop(NameSub, DoTest)
  end subroutine user_read_inputs
  !============================================================================
  subroutine user_init_session

    ! Read shape file and convert units

    use ModMain, ONLY: tSimulation, nStep
    use ModPhysics, ONLY: Io2No_V, Si2No_V, No2Si_V, &
         UnitU_, UnitTemperature_, UnitT_, UnitP_,   &
         UnitN_, UnitX_, UnitB_, UnitEnergyDens_
    use ModNumConst, ONLY: cTwoPi, cDegToRad
    use ModCoordTransform, ONLY: dir_to_xyz
    use ModConst, ONLY: cBoltzmann, cAtomicMass
    use ModVarIndexes, ONLY: MassFluid_I
    use ModBlockData, ONLY: MaxBlockData
    use ModIO, ONLY: IsRestart
    use ModCoordTransform, ONLY: rot_matrix_y, rot_matrix_z

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_init_session'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Obtained the rotation matrix from LatSun and LonSun
    Rot_DD = matmul( rot_matrix_z(-LonSun*cDegToRad), &
         rot_matrix_y(LatSun*cDegToRad) )

    ! Now the Sun is in the +x direction
    LatSun = 0
    LonSun = 0

    ! Test statement, need to remove later
    if (iProc == 0) then
       write(*,*) '-LonSun*cDegToRad =', -LonSun*cDegToRad
       write(*,*) '-LatSun*cDegToRad =', -LatSun*cDegToRad
       write(*,*)         'rot_matrix_z = '
       write(*,'(3f7.3)') rot_matrix_z(-LonSun*cDegToRad)
       write(*,*)         'rot_matrix_y = '
       write(*,'(3f7.3)') rot_matrix_y(-LatSun*cDegToRad)
       write(*,*)         'Rot_DD       = '
       write(*,'(3f7.3)')  Rot_DD
    end if

    if (DoUseCGShape) then
       ! We need to have unit conversions before reading the shape file
       ! which contains everything in SI units
       call read_shape_file
       if (iProc ==0) &
            write(*,*) NameSub, ': reading CG shape file.'
    else
       if (iProc ==0) &
            write(*,*) NameSub, ': using spherical body: rSphericalBodySi =', &
            rSphericalBodySi
    end if

    if (DoUseFieldlineFile) then
       TimeEnhanceStart = TimeEnhanceStartSI*SI2NO_V(UnitT_)
       TimeEnhanceEnd   = TimeEnhanceEndSI*SI2NO_V(UnitT_)
       RadiusTube       = RadiusTubeSI*SI2NO_V(UnitX_)
       SPeAdditional    = SPeAdditionalSi* &
            Si2No_V(UnitEnergyDens_)/Si2No_V(UnitT_)
       call read_fieldline_file

       if (iProc == 0) then
          write (*,*) NameSub, ': reading field line data from ', &
               NameFieldlineFile
          write(*,*) 'TimeEnhanceStartSI, TimeEnhanceStart = ', &
               TimeEnhanceStartSI, TimeEnhanceStart
          write(*,*) 'TimeEnhanceEndSI, TimeEnhanceEnd=    = ', &
               TimeEnhanceEndSI, TimeEnhanceEnd
          write(*,*) 'RadiusTubeSI, RadiusTube = ', RadiusTubeSI, RadiusTube
          write(*,*) 'SPeAdditional   = ', SPeAdditional, &
               SPeAdditionalSi*Si2No_V(UnitP_)/Si2No_V(UnitT_)
          write(*,*) 'SPeAdditionalSi = ', SPeAdditionalSi
       end if
    end if

    rSphericalBody = rSphericalBodySi*Si2No_V(UnitX_)

    ! Production rate = number density * velocity, the unit is in [m^-2 s^-1]
    ! But, applying 1 / Si2No_V(UnitX_)**2 / Io2No_V(UnitT_) is not correct
    ! because Si2No_V(UnitN_) /= 1/Si2No_V(UnitX_)**3
    ProductionRateMax = &
         ProductionRateMaxSi * Si2No_V(UnitN_) * Si2No_V(UnitU_)
    ProductionRateMin = &
         ProductionRateMinSi * Si2No_V(UnitN_) * Si2No_V(UnitU_)

    SolarAngleMax = SolarAngleMaxDim * cDegToRad
    TempCometMin = TempCometMinDim * Io2No_V(UnitTemperature_)
    TempCometMax = TempCometMaxDim * Io2No_V(UnitTemperature_)
    TempComet75  = TempComet75Dim  * Io2No_V(UnitTemperature_)

    ! From Huebner and Markiwitz u_0 = sqrt(8kT/m pi)*0.8257 for f_rv=3 (H2O)
    ! 0.8345 for f_rv=2 (OH) and 0.8728 for f_rv=0 (H)
    TempToUnormal = 1.31763*sqrt(cBoltzmann/(MassFluid_I(nFluid)*cAtomicMass) &
         * No2Si_V(UnitTemperature_))*Si2No_V(UnitU_)

    ! T' = C*T so and p = n*T' = rho*C*T/m so TempToPressure = C/m
    ! where C = 0.8097 for f_rv=0, 0.8858 for f_rv=2, 0.9049 for f_rv=3 (H20)
    TempToPressure = 0.9049/MassFluid_I(nFluid)

    ! Calculate the parameters for production rate (y = a*cos(theta)+b)
    SlopeProduction = &
         (ProductionRateMax - ProductionRateMin) / (1-cos(SolarAngleMax))
    bProduction     = &
         (ProductionRateMin - ProductionRateMax*cos(SolarAngleMax)) / &
         (1-cos(SolarAngleMax))

    ! Calculate the parameters for temperature (y = a/cos(theta)+b)
    SlopeTemp = (TempCometMax - TempComet75)/(1 - 1/cos(75.5*cDegToRad))
    bTemp     = TempCometMax - SlopeTemp

    ! Angular velocity of the comet
    OmegaComet = cTwoPi / (RotationCometHour*3600 * Si2No_V(UnitT_))

    ! Frequency of boundary condition updates
    DtUpdateSi = AngleUpdateDeg*cDegToRad / abs(OmegaComet) * No2Si_V(UnitT_)

    call dir_to_xyz((90-LatSun)*cDegToRad, LonSun*cDegToRad, NormalSun_D)

    ! Maximum amount of data to be stored in ModBlockData
    ! In practice this is a rather generous overestimate.
    ! The first 8 variables: 5 are needed to store the neu1 face B.C. and
    ! 3 are used to store the face normal. The last second is for the shading
    ! in the photoionization. The last one is for the enhancement of the
    ! photo ionization rate at the ring.
    MaxBlockData = 11*(nI+1)*(nJ+1)*(nK+1) + nI*nJ*nK + nI*nJ*nK

    R0Perturbed = R0PerturbedSi*Si2NO_V(UnitX_)
    R1Perturbed = R1PerturbedSi*Si2NO_V(UnitX_)

    Tmin       = TminSi*Si2NO_V(UnitTemperature_)
    TempNeuMin = TempNeuMinSi*Si2NO_V(UnitTemperature_)

    xPerturbedSwMin   = xPerturbedSwMinIO*Io2NO_V(UnitX_)
    xPerturbedSwMax   = xPerturbedSwMaxIO*Io2NO_V(UnitX_)
    PerturbedSwN      = PerturbedSwNIO*Io2NO_V(UnitN_)
    PerturbedSwUx     = PerturbedSwUxIO*Io2NO_V(UnitU_)
    PerturbedSwUy     = PerturbedSwUyIO*Io2NO_V(UnitU_)
    PerturbedSwUz     = PerturbedSwUzIO*Io2NO_V(UnitU_)
    PerturbedSwT      = PerturbedSwTIO*Io2NO_V(UnitTemperature_)
    PerturbedSwTe     = PerturbedSwTeIO*Io2NO_V(UnitTemperature_)
    PerturbedSwBx     = PerturbedSwBxIO*Io2NO_V(UnitB_)
    PerturbedSwBy     = PerturbedSwByIO*Io2NO_V(UnitB_)
    PerturbedSwBz     = PerturbedSwBzIO*Io2NO_V(UnitB_)
    PerturbedSwRho    = PerturbedSwN*MassIon_I(1)

    if (IsRestart) then
       nStepSave_B          = nStep
       TimeSimulationSave_B = tSimulation
       nStepSaveCalcRates_B = nStep
       nStepPritSetFace     = nStep
       FaceCoordsTest_D     = [FaceCoordsX, FaceCoordsY, FaceCoordsZ]
    end if

    if (DoEnhanceNeu) then
       nStepEnhanceNeu = nStep
    end if

    if (DoUseUniformNeuBackground) then
       nNeuUniform  = nNeuUniformSi*Si2No_V(UnitN_)
       UxNeuUniform = UxNeuUniformSi*Si2No_V(UnitU_)
       UyNeuUniform = UyNeuUniformSi*Si2No_V(UnitU_)
       UzNeuUniform = UzNeuUniformSi*Si2No_V(UnitU_)
       pNeuUniform  = nNeuUniform*TNeuUniformSi*Si2No_V(UnitTemperature_)
    end if

    if(iProc==0)then
       write(*,*) 'rSphericalBodySi, rSphericalBody       =', &
            rSphericalBodySi, rSphericalBody
       write(*,*) 'R0PerturbedSi, R0Perturbed             =', &
            R0PerturbedSi, R0Perturbed
       write(*,*) 'R1PerturbedSi, R1Perturbed             =', &
            R1PerturbedSi, R1Perturbed
       write(*,*) 'ratioPerturbed                         =', ratioPerturbed
       write(*,*) 'ProductionRateMaxSi, ProductionRateMax =', &
            ProductionRateMaxSi, ProductionRateMax
       write(*,*) 'ProductionRateMinSi, ProductionRateMin =', &
            ProductionRateMinSi, ProductionRateMin
       write(*,*) 'SolarAngleMaxDim, SolarAngleMax        =', &
            SolarAngleMaxDim, SolarAngleMax
       write(*,*) 'TempCometMinDim, TempCometMin =', &
            TempCometMinDim, TempCometMin
       write(*,*) 'TempCometMaxDim, TempCometMax =', &
            TempCometMaxDim, TempCometMax
       write(*,*) 'TempComet75Dim,  TempComet75  =', &
            TempComet75Dim,  TempComet75
       write(*,*) 'TempToUnormal, TempToPressure =', &
            TempToUnormal, TempToPressure
       write(*,*) 'MassFluid_I =', MassFluid_I
       write(*,*) 'TempToUn*sqrt(TempCometMax)   =', &
            TempToUnormal*sqrt(TempCometMax)
       write(*,*) 'SlopeProduction, bProduction  =', &
            SlopeProduction, bProduction
       write(*,*) 'SlopeTemp, bTemp              =', &
            SlopeTemp/Io2No_V(UnitTemperature_),&
            bTemp/Io2No_V(UnitTemperature_)
       write(*,*)'RotationComet, Omega       =', RotationCometHour, OmegaComet
       write(*,*)'AngleUpdateDeg, DtUpdateSi =', AngleUpdateDeg, DtUpdateSi
       write(*,*)'LatSun, LonSun             =', LatSun, LonSun
       write(*,*)'NormalSun_D                =', NormalSun_D
       write(*,*)'TminSi, Tmin               =', TminSi, Tmin
       write(*,*)'nStepEnhanceNeu            =', nStepEnhanceNeu
       if (UsePerturbedSW) then
          write(*,*) 'xPerturbedSwMinIO, xPerturbedSwMin   =', &
               xPerturbedSwMinIO, xPerturbedSwMin
          write(*,*) 'xPerturbedSwMaxIO, xPerturbedSwMax   =', &
               xPerturbedSwMaxIO, xPerturbedSwMax
          write(*,*) 'PerturbedSwNIO, PerturbedSwN   =', &
               PerturbedSwNIO, PerturbedSwN
          write(*,*) 'PerturbedSwTIO, PerturbedSwT   =', &
               PerturbedSwTIO, PerturbedSwT
          write(*,*) 'PerturbedSwTeIO,PerturbedSwTe  =', &
               PerturbedSwTeIO, PerturbedSwTe
          write(*,*) 'PerturbedSwUxIO, PerturbedSwUx =', &
               PerturbedSwUxIO, PerturbedSwUx
          write(*,*) 'PerturbedSwUyIO, PerturbedSwUy =', &
               PerturbedSwUyIO, PerturbedSwUy
          write(*,*) 'PerturbedSwUzIO, PerturbedSwUz =', &
               PerturbedSwUzIO, PerturbedSwUz
          write(*,*) 'PerturbedSwBxIO, PerturbedSwBx =', &
               PerturbedSwUzIO, PerturbedSwUz
          write(*,*) 'PerturbedSwByIO, PerturbedSwBy =', &
               PerturbedSwByIO, PerturbedSwBy
          write(*,*) 'PerturbedSwBzIO, PerturbedSwBz =', &
               PerturbedSwBzIO, PerturbedSwBz
       end if
       if (DoUseUniformNeuBackground) then
          write(*,*) 'DoUseUniformNeuBackground    =', DoUseUniformNeuBackground
          write(*,*) 'nNeuUniformSi, nNeuUniform   =', &
               nNeuUniformSi, nNeuUniform
          write(*,*) 'UxNeuUniformSi, UxNeuUniform =', &
               UxNeuUniformSi, UxNeuUniform
          write(*,*) 'UyNeuUniformSi, UyNeuUniform =', &
               UyNeuUniformSi, UyNeuUniform
          write(*,*) 'UzNeuUniformSi, UzNeuUniform =', &
               UzNeuUniformSi, UzNeuUniform
          write(*,*) 'TNeuUniformSi, pNeuUniform =', &
               TNeuUniformSi, pNeuUniform
       end if
    end if
    call test_stop(NameSub, DoTest)
  contains
    !==========================================================================
    subroutine read_shape_file

      use ModPhysics, ONLY: Si2No_V, UnitX_
      use ModIoUnit, ONLY: UnitTmp_
      use ModCoordTransform, ONLY: cross_product
      use ModRandomNumber, ONLY: random_real

      logical :: DoReadShapeFile = .true.

      integer :: nPoint, i, j, iPoint, iTriangle, iPoint1, iPoint2, iPoint3
      integer :: iSeed = 7

      real, allocatable:: Xyz_DI(:,:)

      character(len=100):: String1, String2

      logical:: DoTest
      character(len=*), parameter:: NameSub = 'read_shape_file'
      !------------------------------------------------------------------------
      call test_start(NameSub, DoTest)
      if(.not.DoReadShapeFile) RETURN
      DoReadShapeFile = .false.

      if(iProc==0)write(*,*) NameSub,' reading shape file ',trim(NameShapeFile)

      open(UnitTmp_, file=NameShapeFile)

      read(UnitTmp_, '(a)') String1
      read(UnitTmp_, *) String1, nPoint, String2
      if(String2 /= 'POINTS')call stop_mpi(NameSub//&
           ' no POINTS in '//trim(String2)//' of '//trim(NameShapeFile))
      read(UnitTmp_, *) String1, nTriangle, String2
      if(String2 /= 'TRIANGLES')call stop_mpi(NameSub//&
           ' no TRIANGLES in '//trim(String2)//' of '//trim(NameShapeFile))

      if(iProc==0)write(*,*) NameSub,' nPoint=', nPoint,' nTriangle=',nTriangle

      allocate(Xyz_DI(3,nPoint), &
           XyzTriangle_DII(3,3,nTriangle), Normal_DI(3,nTriangle))

      read(UnitTmp_, '(a)') String1
      do iPoint=1,nPoint
         read(UnitTmp_,*) String1, i, j, Xyz_DI(:,iPoint)

         ! Perturb vertices of all triangles to avoid the the situation that
         ! a line segment is parallel to a triangle plane in
         ! is_segment_intersected

         Xyz_DI(1, iPoint) = Xyz_DI(1, iPoint) + random_real(iSeed)*1e-5
         Xyz_DI(2, iPoint) = Xyz_DI(2, iPoint) + random_real(iSeed)*1e-5
         Xyz_DI(3, iPoint) = Xyz_DI(3, iPoint) + random_real(iSeed)*1e-5

         ! Convert from SI units to normalized unit
         Xyz_DI(:,iPoint) = Xyz_DI(:,iPoint) * Si2No_V(UnitX_)

         ! Rotate the comet, so that the Sun is in the +x direction
         Xyz_DI(:,iPoint) = matmul(Rot_DD, Xyz_DI(:,iPoint))
      end do
      do iTriangle=1,nTriangle
         read(UnitTmp_,*) String1, i, j, iPoint1, iPoint2, iPoint3
         XyzTriangle_DII(:,1,iTriangle) = Xyz_DI(:,iPoint1)
         XyzTriangle_DII(:,2,iTriangle) = Xyz_DI(:,iPoint2)
         XyzTriangle_DII(:,3,iTriangle) = Xyz_DI(:,iPoint3)

         Normal_DI(:,iTriangle) = cross_product( &
              Xyz_DI(:,iPoint2) - Xyz_DI(:,iPoint1), &
              Xyz_DI(:,iPoint3) - Xyz_DI(:,iPoint1))
         Normal_DI(:,iTriangle) = Normal_DI(:,iTriangle) / &
              sqrt(sum(Normal_DI(:,iTriangle)**2))
      end do

      rMinShape = sqrt(minval(sum(Xyz_DI**2,DIM=1)))
      rMaxShape = sqrt(maxval(sum(Xyz_DI**2,DIM=1)))

      if(iProc==0)write(*,*) NameSub,' rMinShape, rMaxShape=', &
           rMinShape, rMaxShape

      deallocate(Xyz_DI)

      close(UnitTmp_)

      call test_stop(NameSub, DoTest)
    end subroutine read_shape_file
    !==========================================================================
    subroutine read_fieldline_file

      use ModPhysics, ONLY: Si2No_V, UnitX_
      use ModIoUnit, ONLY: UnitTmp_

      logical :: DoReadFieldlineFile = .true.

      integer:: nPoint, iPoint, iFileHeader, i

      real, allocatable:: State_VI(:,:)

      character(len=100):: String1, String2, String3, String4

      logical:: DoTest
      character(len=*), parameter:: NameSub = 'read_fieldline_file'
      !------------------------------------------------------------------------
      call test_start(NameSub, DoTest)
      if(.not.DoReadFieldlineFile) RETURN
      DoReadFieldlineFile = .false.

      open(UnitTmp_, file=NameFieldlineFile)

      ! Read the header info for the whole file
      do iFileHeader = 1, nVarFieldlineFile+3+1
         read(UnitTmp_,'(a)') String1
      end do

      !! Read the header for each zone
      !! Read 'ZONE T="Streamtrace"'
      read(UnitTmp_,'(a)') String1

      !! Read ' STRANDID=0, SOLUTIONTIME=0'
      read(UnitTmp_,'(a)') String1
      !! Read ' I=*, J=1, K=1, ZONETYPE=Ordered'
      read(UnitTmp_,*)     String1, String2, String3, String4
      i = index(String1,'=')
      String1 = String1(i+1:100)
      read (string1,*) nPoint

      !! Read ' DATAPACKING=POINT'
      read(UnitTmp_,'(a)') String1
      !! Read ' DT=(SINGLE SINGLE SINGLE ...)'
      read(UnitTmp_,'(a)') String1

      allocate(XyzFieldline_DI(3,nPoint), State_VI(nVarFieldlineFile,nPoint))

      ! Read the points along a field line
      do iPoint = 1, nPoint
         read(UnitTmp_,*) XyzFieldline_DI(:,iPoint), State_VI(:,iPoint)
      end do

      XyzFieldline_DI = XyzFieldline_DI*1e3*Si2NO_V(UnitX_)

      ! Temporarily set XyzFieldline_DI(3,*) = 0 for testing purpose
      XyzFieldline_DI(3,:) = 0.0

      deallocate(State_VI)

      call test_stop(NameSub, DoTest)
    end subroutine read_fieldline_file
    !==========================================================================
  end subroutine user_init_session
  !============================================================================
  subroutine user_set_boundary_cells(iBlock)

    use ModGeometry, ONLY: ExtraBc_, Xyz_DGB, r_GB
    use ModBoundaryGeometry, ONLY: iBoundary_GB, domain_

    integer, intent(in):: iBlock

    integer :: i, j, k
    real    :: XyzInside_D(3)
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_boundary_cells'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if (DoUseCGShape) then
       ! Place a point inside rMinShape sphere with transcendent coordinates
       ! to reduce chances of hitting the edge or corner of triangles

       XyzInside_D = rMinShape*[cPi/10,cPi**2/50,cPi**3/700]

       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          ! Check if we are close enough
          if(r_GB(i,j,k,iBlock) > rMaxShape) then
             iBoundary_GB(i,j,k,iBlock) = domain_
          elseif(r_GB(i,j,k,iBlock) < rMinShape) then
             iBoundary_GB(i,j,k,iBlock) = ExtraBc_
          else
             ! Connect cell center with a point inside.
             ! If the line segment does not intersect the shape
             ! or it intersects even times then the point is inside the shape.
             if(.not. is_segment_intersected(XyzInside_D, &
                  Xyz_DGB(:,i,j,k,iBlock),IsOddIn=.true.)) &
                  iBoundary_GB(i,j,k,iBlock) = ExtraBc_
          end if
       end do; end do; end do

    else
       where(r_GB(:,:,:,iBlock) <= rSphericalBody) &
            iBoundary_GB(:,:,:,iBlock) = ExtraBc_
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_boundary_cells
  !============================================================================
  subroutine user_set_face_boundary(FBC)

    use ModMain, ONLY: nStep, tSimulation, FaceBCType
    use ModVarIndexes, ONLY: nVar, Rho_, p_, Bx_, Bz_
    use ModGeometry, ONLY: ExtraBc_, Xyz_DGB
    use ModPhysics, ONLY: UnitRho_, BodyRho_I, BodyP_I, Si2No_V, UnitN_, &
         UnitTemperature_
    use ModSolarwind, ONLY: get_solar_wind_point
    use ModBlockData, ONLY: use_block_data, clean_block_data, &
         get_block_data, put_block_data

    type(FaceBCType), intent(inout):: FBC

    integer:: iTrue, jTrue, kTrue, iBody, jBody, kBody

    real :: XyzIntersect_D(3), XyzStart_D(3), XyzEnd_D(3)
    real :: XyzTrueCell_D(3), XyzBodyCell_D(3)
    real :: Normal_D(3), CosAngle, NormalFace_D(3)
    real :: TempCometLocal, uNormal, ProductionRateLocal

    logical :: IsIlluminated = .false.
    logical :: DoWriteOnce = .true.
    !$omp threadprivate( IsIlluminated, DoWriteOnce )

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_face_boundary'
    !--------------------------------------------------------------------------
    associate( VarsGhostFace_V => FBC%VarsGhostFace_V, &
         VarsTrueFace_V => FBC%VarsTrueFace_V, &
         B0Face_D => FBC%B0Face_D, &
         iBoundary => FBC%iBoundary, TimeBc => FBC%TimeBc, &
         iSide => FBC%iSide, FaceCoords_D => FBC%FaceCoords_D, &
         iFace => FBC%iFace, jFace => FBC%jFace, kFace =>FBC%kFace, &
         iBlock => FBC%iBlockBc)

      call test_start(NameSub, DoTest, iBlock)

      ! Outer boundaries
      if(iBoundary >0) then
         call get_solar_wind_point(TimeBc, FaceCoords_D, VarsGhostFace_V)

         ! Float condition for neu1
         VarsGhostFace_V(Neu1Rho_:Neu1P_) = VarsTrueFace_V(Neu1Rho_:Neu1P_)
         RETURN
      end if

      if (iBoundary /= ExtraBc_) call stop_mpi(NameSub//' bad iBoundary value')

      ! Body boundaries
      ! Default body boundary conditions
      if (UseSwBC) then
         ! Boundary condition with solar wind values
         if (DoWriteOnce) then
            write(*,*) NameSub, ': solar wind body conditions.'
            DoWriteOnce = .false.
         end if

         call get_solar_wind_point(TimeBc, FaceCoords_D, VarsGhostFace_V)
      else
         if (DoWriteOnce .and. .not. UseReflectedBC) then
            ! write(*,*) NameSub, ': floating body conditions.'
            DoWriteOnce = .false.
         end if

         ! Floating boundary condition by default, will be overwritten below
         VarsGhostFace_V = VarsTrueFace_V
      end if

      ! Neutral boundary conditions -----------------------------------------
      if (DoUseCGShape) then

         ! We can use the saved values if no AMR is done
         if(use_block_data(iBlock)) then

            call get_block_data(iBlock, 5, VarsGhostFace_V(Neu1Rho_:Neu1P_))
            call get_block_data(iBlock, 3, Normal_D)
            call get_block_data(iBlock, 3, NormalFace_D)

            if (.not. UseSwBC) call set_ion_face_boundary

            if ((nStep <= nStepPritSetFace+2) .and. &
                 sum(abs(FaceCoords_D - FaceCoordsTest_D)) < 1e-8 ) then
               write(*,*) '============= nStep ', nStep, '===================='
               write(*,*) 'FaceCoords_D  =', FaceCoords_D
               write(*,*) 'Normal_D      =', Normal_D
               write(*,*) 'NormalFace_D  =', NormalFace_D
               write(*,*) 'P             =', VarsGhostFace_V(P_)
               write(*,*) 'Pe            =', VarsGhostFace_V(Pe_)
               write(*,*) 'Rho           =', VarsGhostFace_V(Rho_)
               write(*,*) 'U_D           =', VarsGhostFace_V(RhoUx_:RhoUz_)
               write(*,*) 'P             =', VarsGhostFace_V(P_)
               write(*,*) 'H2OpRho       =', VarsGhostFace_V(H2OpRho_)
               write(*,*) 'H2OpU_D       =', &
                    VarsGhostFace_V(H2OpRhoUx_:H2OpRhoUz_)
               write(*,*) 'H2OpUT_D      =', &
                    VarsTrueFace_V(H2OpRhoUx_:H2OpRhoUz_)
               write(*,*) 'H2OpP         =', VarsGhostFace_V(H2OpP_)
               write(*,*) 'Neu1Rho       =', VarsGhostFace_V(Neu1Rho_)
               write(*,*) 'Neu1u_D       =', VarsGhostFace_V(Neu1Ux_:Neu1Uz_)
               write(*,*) 'Neu1p         =', VarsGhostFace_V(Neu1p_)
            end if
            RETURN
         end if

         ! Empty the block storage if we redo the calculation
         if(use_block_data(iBlock)) call clean_block_data(iBlock)

         ! Save step and simulation time info
         nStepSave_B(iBlock)          = nStep
         TimeSimulationSave_B(iBlock) = tSimulation

         ! Default indexes for the true and body cells
         iTrue = iFace; jTrue = jFace; kTrue = kFace
         iBody = iFace; jBody = jFace; kBody = kFace

         select case(iSide)
         case(1)
            iBody = iFace - 1
            NormalFace_D = [ 1.0, 0.0, 0.0]
         case(2)
            iTrue = iFace - 1
            NormalFace_D = [-1.0, 0.0, 0.0]
         case(3)
            jBody = jFace - 1
            NormalFace_D = [ 0.0, 1.0, 0.0]
         case(4)
            jTrue = jFace - 1
            NormalFace_D = [ 0.0,-1.0, 0.0]
         case(5)
            kBody = kFace -1
            NormalFace_D = [ 0.0, 0.0, 1.0]
         case(6)
            kTrue = kFace -1
            NormalFace_D = [ 0.0, 0.0,-1.0]
         end select

         XyzBodyCell_D = Xyz_DGB(:,iBody,jBody,kBody,iBlock)
         XyzTrueCell_D = Xyz_DGB(:,iTrue,jTrue,kTrue,iBlock)

         ! Find the intersection point between the true cell and the body cell
         ! that is closest to the true cell
         if (.not. is_segment_intersected( &
              XyzTrueCell_D, XyzBodyCell_D, IsOddIn = .true., &
              XyzIntersectOut_D=XyzIntersect_D, NormalOut_D = Normal_D))then
            write(*,*) 'XyzTrueCell_D =', XyzTrueCell_D
            write(*,*) 'XyzBodyCell_D =', XyzBodyCell_D
            write(*,*) NameSub,' error for face =', iFace, jFace, kFace
            write(*,*) NameSub,' error for iside, iBlock=', iSide, iBlock
            call stop_mpi(NameSub// &
                 ': No intersection points between true and the body cells')
         end if

         ! Fix the normal direction if it is not pointing outward
         if (sum(Normal_D*(XyzTrueCell_D - XyzBodyCell_D)) < 0.0) &
              Normal_D = -Normal_D
      else
         ! Use a spherical body instead of a real CG shape
         ! Normal is in the r direction
         Normal_D = FaceCoords_D/sqrt(sum(FaceCoords_D*FaceCoords_D))
      end if

      ! Calculate the cos angle between surface normal and sun direction
      CosAngle = sum(Normal_D*NormalSun_D)

      ! Set local outflow parameters as default that may be overwritten
      ! if illuminated
      TempCometLocal      = TempCometMin
      ProductionRateLocal = ProductionRateMin

      if (CosAngle > 0.0) then
         if (DoUseCGShape) then
            ! See whether the intersection point is in the shade  by going
            ! towards the Sun and checking for intersection with the shape
            XyzStart_D = XyzIntersect_D + 1e-9*rMaxShape*NormalSun_D
            XyzEnd_D   = XyzIntersect_D +    2*rMaxShape*NormalSun_D
            if(.not.is_segment_intersected(XyzStart_D, XyzEnd_D)) &
                 IsIlluminated = .true.
         else
            IsIlluminated = .true.
         end if
      end if

      if (IsIlluminated) then
         ! Increase temperature of the face if it is illuminated
         TempCometLocal      = max( TempCometMin, &
              SlopeTemp / CosAngle + bTemp)
         ! Increase neutral production rate
         ProductionRateLocal = max( ProductionRateMin, &
              SlopeProduction * CosAngle + bProduction )
      end if

      ! Calculate the normal velocity
      uNormal = sqrt(TempCometLocal)*TempToUnormal

      if (.not. DoUseHaserBackground) then
         ! illumniated case if the Haser background is not applied
         VarsGhostFace_V(Neu1Ux_:Neu1Uz_) = Normal_D*uNormal
         VarsGhostFace_V(Neu1Rho_)        = ProductionRateLocal/uNormal  &
              *MassFluid_I(nFluid)
         VarsGhostFace_V(Neu1P_)          =                              &
              VarsGhostFace_V(Neu1Rho_)*TempCometLocal*TempToPressure
      else
         ! Haser background
         VarsGhostFace_V(Neu1Rho_)        =  Qprod/                 &
              (4.*cPi*rSphericalBodySi**2*uHaser )*                 &
              exp(-vHI*rSphericalBodySi/uHaser)*                    &
              Si2No_V(UnitN_) * MassFluid_I(nFluid)
         VarsGhostFace_V(Neu1Ux_:Neu1Uz_) = 0.0
         VarsGhostFace_V(Neu1P_)          = VarsGhostFace_V(Neu1Rho_)/   &
              MassFluid_I(nFluid)*TempNeuMinSi*Si2No_V(UnitTemperature_)
      end if

      if (.not. UseSwBC) call set_ion_face_boundary

      IsIlluminated = .false.

      ! Store for future time steps
      if (DoUseCGShape)  then
         call put_block_data(iBlock, 5, VarsGhostFace_V(Neu1Rho_:Neu1P_))
         call put_block_data(iBlock, 3, Normal_D)
         call put_block_data(iBlock, 3, NormalFace_D)
      end if

      if (DoTest .and. IsIlluminated .and. CosAngle > 0.5) then
         FaceCoordsTest_D = FaceCoords_D

         write(*,*) 'FaceCoords_D: ', FaceCoords_D
         if (DoUseCGShape) then
            write(*,*) 'TestFace_D: ', (XyzBodyCell_D + XyzTrueCell_D)/2
            write(*,*) 'XyzTrueCell_D =', XyzTrueCell_D
            write(*,*) 'XyzBodyCell_D =', XyzBodyCell_D
            write(*,*) 'XyzIntersect_D=', XyzIntersect_D
            write(*,*) 'XyzStart_D    =', XyzStart_D
            write(*,*) 'XyzEnd_D      =', XyzEnd_D
         end if
         write(*,*) 'Normal_D      =', Normal_D
         write(*,*) 'NormalFace_D  =', NormalFace_D
         write(*,*) 'CosAngle      =', CosAngle
         write(*,*) 'P             =', VarsGhostFace_V(P_)
         write(*,*) 'Pe            =', VarsGhostFace_V(Pe_)
         write(*,*) 'Rho           =', VarsGhostFace_V(Rho_)
         write(*,*) 'U_D           =', VarsGhostFace_V(RhoUx_:RhoUz_)
         write(*,*) 'P             =', VarsGhostFace_V(P_)
         write(*,*) 'H2OpRho       =', VarsGhostFace_V(H2OpRho_)
         write(*,*) 'H2OpU_D       =', VarsGhostFace_V(H2OpRhoUx_:H2OpRhoUz_)
         write(*,*) 'H2OpUT_D      =', VarsTrueFace_V(H2OpRhoUx_:H2OpRhoUz_)
         write(*,*) 'H2OpP         =', VarsGhostFace_V(H2OpP_)
         write(*,*) 'Neu1Rho       =', VarsGhostFace_V(Neu1Rho_)
         write(*,*) 'Neu1u_D       =', VarsGhostFace_V(Neu1Ux_:Neu1Uz_)
         write(*,*) 'Neu1uNormal   =', uNormal
         write(*,*) 'Neu1p         =', VarsGhostFace_V(Neu1p_)

         nStepPritSetFace = nStep
      end if

    end associate

    call test_stop(NameSub, DoTest)
  contains
    !==========================================================================
    subroutine set_ion_face_boundary

      integer :: iUx, iUz, iIonFluid
      real    :: uNormalIon_I(nIonFluid), nIon_I(nIonFluid)
      real    :: uIonMean_D(3)
      real    :: nElec, uIonMeanNormal

      ! Projection length of U_ on the local surface normal vector
      !------------------------------------------------------------------------
      associate( VarsGhostFace_V => FBC%VarsGhostFace_V, &
           VarsTrueFace_V => FBC%VarsTrueFace_V, &
           FaceCoords_D => FBC%FaceCoords_D )

        do iIonFluid=1,nIonFluid
           iUx = iRhoUxIon_I(iIonFluid)
           iUz = iRhoUzIon_I(iIonFluid)
           uNormalIon_I(iIonFluid) = sum(VarsTrueFace_V(iUx:iUz) * Normal_D)
        end do

        nIon_I = VarsTrueFace_V(iRhoIon_I)/MassIon_I
        nElec  = sum(nIon_I*ChargeIon_I)

        uIonMean_D = 0.0
        do iIonFluid=1,nIonFluid
           iUx = iRhoUxIon_I(iIonFluid)
           iUz = iRhoUzIon_I(iIonFluid)
           uIonMean_D = uIonMean_D + &
                VarsTrueFace_V(iUx:iUz) &
                *nIon_I(iIonFluid)*ChargeIon_I(iIonFluid)/nElec
        end do

        uIonMeanNormal = sum(uIonMean_D*Normal_D)

        ! Projection length of U_ on the local cartesian surface normal vector
        ! do iIonFluid=1,nIonFluid
        !   uNormalIon_I(iIonFluid) = sum(&
        !        VarsTrueFace_V(iRhoUxIon_I(iIonFluid):iRhoUzIon_I(iIonFluid))* &
        !        NormalFace_D)
        ! end do

        !  BdotR = dot_product(VarsTrueFace_V(Bx_:Bz_),FaceCoords_D)/ &
        !  dot_product(FaceCoords_D,FaceCoords_D)

        ! Projection vectors
        !  BRefl_D = BdotR*FaceCoords_D

        ! Bz component propagated through moon, Bx and By didn't
        !  VarsGhostFace_V(Bx_:By_) = 0.0
        !  VarsGhostFace_V(Bz_)     = SolarWindBz

        do iIonFluid=1,nIonFluid
           if (UseReflectedBC) then
              ! Reflected raidal velocity uG = uT - 2*u_normal
              if (DoWriteOnce) then
                 ! write(*,*) NameSub, ': reflected boundary condition'
                 DoWriteOnce = .false.
              end if

              if (uNormalIon_I(iIonFluid) > 0.0) then
                 iUx = iRhoUxIon_I(iIonFluid)
                 iUz = iRhoUzIon_I(iIonFluid)
                 VarsGhostFace_V(iUx:iUz) = VarsTrueFace_V(iUx:iUz) - &
                      2.0*uNormalIon_I(iIonFluid)*Normal_D
              end if
           else
              ! set outward flux body value
              ! (Comet's surface not considered as plasma source)
              ! leave inward flux untouched
              if (uNormalIon_I(iIonFluid) > 0.0) then
                 iUx = iRhoUxIon_I(iIonFluid)
                 iUz = iRhoUzIon_I(iIonFluid)

                 VarsGhostFace_V(iUx:iUz) = 0.0
                 VarsGhostFace_V(iRhoIon_I(iIonFluid)) = BodyRho_I(iIonFluid)
                 VarsGhostFace_V(iPIon_I(iIonFluid))   = BodyP_I(iIonFluid)
              endif
           end if
        end do

        ! Set a vacuum cleaner for the solar wind at the body
        ! if the solar wind is inward, let the ghost face rho and P value to
        ! be half of the true face value.
        if (uNormalIon_I(Sw_) <= 0.0) then
           VarsGhostFace_V(iRhoIon_I(Sw_)) = 0.5* VarsTrueFace_V(iRhoIon_I(Sw_))
           VarsGhostFace_V(iPIon_I(Sw_))   = 0.5* VarsTrueFace_V(iPIon_I(Sw_))
        end if

        do iIonFluid=1,nIonFluid
           if (uNormalIon_I(iIonFluid) > 0.0 .and. &
                any(VarsGhostFace_V(iRhoIon_I) > 1e-2*Si2NO_V(UnitRho_))) then
              write(*,*) 'nStep, iIonFluid, FaceCoords_D =', &
                   nStep, iIonFluid, FaceCoords_D
              write(*,*) 'uNormalIon_I(iIonFluid)                =', &
                   uNormalIon_I(iIonFluid)
              write(*,*) 'BodyRho_I(iIonFluid), BodyP_I(iIonFluid) =', &
                   BodyRho_I(iIonFluid), BodyP_I(iIonFluid)
              ! call stop_mpi('Plasma source at the surface????????')
           end if
        end do

        ! magnetic field on the comet body should be 0
        if (TypeBfieldBody == 'zero') then
           VarsGhostFace_V(Bx_:Bz_) = 0.0
        else if (TypeBfieldBody == 'absorbed') then
           if (sum(VarsGhostFace_V(Bx_:Bz_)*Normal_D) > 0) &
                VarsGhostFace_V(Bx_:Bz_) = 0.0
        else
           call stop_mpi(NameSub//' unknow TypeBfieldBody')
        end if

        if(UseElectronPressure .and. uIonMeanNormal > 0.0) then
           ! Assume that electron velocity is the same as the mean ion velocity.
           ! Use the minimum neutral temperature as the electron temperature
           ! at the body if outflow, float otherwise
           VarsGhostFace_V(Pe_) = nElec*TempNeuMin
        end if

        ! fixed bc for Hyp_
        if(Hyp_>1) VarsGhostFace_V(Hyp_) = 0

      end associate

    end subroutine set_ion_face_boundary
    !==========================================================================
  end subroutine user_set_face_boundary
  !============================================================================
  logical function is_segment_intersected(Xyz1_D, Xyz2_D, &
       IsOddIn, XyzIntersectOut_D, NormalOut_D)

    ! Check if a line segment connecting Xyz1_D and Xyz2_D intersects
    ! the shape. If IsEvenIn is present and true, check if the number
    ! of intersections is odd or even.

    ! Using algorithm: http://geomalgorithms.com/a06-_intersect-2.html

    real, intent(in):: Xyz1_D(3) ! segment start coordinates
    real, intent(in):: Xyz2_D(3) ! segment end   coordinates
    logical, optional:: IsOddIn  ! check for odd number of intersects
    real,    optional :: XyzIntersectOut_D(3)
    real,    optional :: NormalOut_D(3)

    logical:: IsOdd

    integer:: iTriangle, nIntersect, iMinRatio

    integer, parameter:: MaxIntersect = 15
    real:: Ratio, Ratio1, Ratio2, Ratio_I(MaxIntersect)
    integer:: iTriangle_I(MaxIntersect)
    real, dimension(3):: v1_D, Xyz_D, u_D, v_D, w_D
    real:: nDotP2P1, nDotV1P1, u2, v2, uDotV, wDotU, wDotV, InvDenom

    ! Default is to check for first intersection only
    ! (for shadows and convex shapes)
    character(len=*), parameter:: NameSub = 'is_segment_intersected'
    !--------------------------------------------------------------------------
    IsOdd = .false.
    if(present(IsOddIn))   IsOdd = IsOddIn

    nIntersect = 0
    do iTriangle=1,nTriangle
       ! Find intersection of line segment with the plane of the triangle
       nDotP2P1 = sum(Normal_DI(:,iTriangle)*(Xyz2_D - Xyz1_D))

       ! Check if the segment is parallel to the plane
       if(abs(nDotP2P1) < 1e-12) then
          if (abs(sum(Normal_DI(:,iTriangle)* &
               (Xyz1_D - XyzTriangle_DII(:,1,iTriangle)))) < 1e-12) then
             write(*,*) 'segment lies in the same plane: iTriangle: ', Xyz2_D
             write(*,*) 'Test:', sum( &
                  Normal_DI(:,iTriangle)* &
                  (Xyz1_D - XyzTriangle_DII(:,1,iTriangle)) )
             CYCLE
          else
             CYCLE
          end if
       end if

       ! Vertex 1 of triangle
       v1_D = XyzTriangle_DII(:,1,iTriangle)

       nDotV1P1 = sum(Normal_DI(:,iTriangle)*(v1_D -  Xyz1_D))

       ! Intersection is at P1 + Ratio * (P2 - P1)
       Ratio = nDotV1P1 / nDotP2P1

       ! Check if point is inside the segment
       if(Ratio <    -1e-12) CYCLE
       if(Ratio > 1.0+1e-12) CYCLE

       ! Intersection point
       Xyz_D =  Xyz1_D + Ratio*(Xyz2_D -  Xyz1_D)

       ! Calculate the barycentric coordinates Ratio1 and Ratio2
       ! The intersection point is inside if the conditions
       ! 0 < Ratio1, Ratio2 and Ratio1 + Ratio2 < 1 both hold.

       ! Vectors relative to the first vertex
       u_D = XyzTriangle_DII(:,2,iTriangle) - v1_D
       v_D = XyzTriangle_DII(:,3,iTriangle) - v1_D
       w_D = Xyz_D                          - v1_D

       u2 = sum(u_D**2)
       v2 = sum(v_D**2)
       uDotV = sum(u_D*v_D)
       wDotU = sum(w_D*u_D)
       wDotV = sum(w_D*v_D)

       InvDenom = 1.0/(uDotV**2 - u2*v2)

       ! First barycentric coordinate
       Ratio1 = (uDotV*wDotV - v2*wDotU)*InvDenom
       if(Ratio1 <    -1e-12) CYCLE
       if(Ratio1 > 1.0+1e-12) CYCLE

       ! Second barycentric coordinate
       Ratio2 = (uDotV*wDotU - u2*wDotV)*InvDenom

       ! if (abs(Xyz2_D(1) -xTest) <= 1e-4 .and. &
       !     abs(Xyz2_D(2) - yTest) <= 1e-4 .and. &
       !     abs(Xyz2_D(3) - zTest) <= 1e-4) then
       !   if (abs(Ratio2) < 1e-10 .or. abs(Ratio2 - 1.0) < 1e-10) then
       !      write(*,*) 'iTriangle: ', iTriangle, 'Ratio1: ', Ratio1, &
       !           'Ratio2: ', Ratio2
       !   end if
       ! end if

       if(         Ratio2 <    -1e-12) CYCLE
       if(Ratio1 + Ratio2 > 1.0+1e-12) CYCLE

       if(.not. IsOdd)then
          ! The line segment intersected the triangle
          is_segment_intersected = .true.
          RETURN
       end if

       ! Check if this intersection is different from previous ones
       if(nIntersect > 0)then
          if( any(abs(Ratio - Ratio_I(1:nIntersect)) < 1e-12) ) CYCLE
       end if

       ! New intersection was found
       nIntersect =  nIntersect + 1

       if(nIntersect > MaxIntersect) then
          write(*,*) 'nIntersect, MaxIntersect =', nIntersect, MaxIntersect
          call stop_mpi(NameSub// &
               ': too many intersections, increase MaxIntersect')
       end if

       ! Store the position along the segment into Ratio_I
       Ratio_I(nIntersect) = Ratio
       iTriangle_I(nIntersect) = iTriangle
    end do

    ! Only record the closeset intersection to the true cell
    if(present(NormalOut_D)) then
       iMinRatio = minloc(Ratio_I(1:nIntersect), 1)
       iTriangle = iTriangle_I(iMinRatio)
       NormalOut_D = Normal_DI(:,iTriangle)

       if (nIntersect > 1) then
          ! write(*,*) 'nIntersect: ', nIntersect
          ! write(*,*) 'Ratio_I: ', Ratio_I(1:nIntersect)
          ! write(*,*) 'Xyz1_D: ', Xyz1_D
          ! write(*,*) 'Xyz2_D: ', Xyz2_D
          ! do iIntersect = 1, nIntersect
          !   iTriangle = Triangle_I(iIntersect)
          !   write(*,*) XyzTriangle_DII(:,1,iTriangle)
          !   write(*,*) XyzTriangle_DII(:,2,iTriangle)
          !   write(*,*) XyzTriangle_DII(:,3,iTriangle)
          ! end do
          write(*,*)  'Ratio_I(iMinRatio)            = ', Ratio_I(iMinRatio)
          write(*,*)  'minval(Ratio_I(1:nIntersect)) = ', &
               minval(Ratio_I(1:nIntersect))
          write(*,*)  'Ratio_I(1:nIntersect)', Ratio_I(1:nIntersect)
       end if
    end if
    if(present(XyzIntersectOut_D)) then
       XyzIntersectOut_D = Xyz1_D + &
            minval(Ratio_I(1:nIntersect))*(Xyz2_D -  Xyz1_D)
    end if

    if(.not. IsOdd)then
       ! The line segment was not intersected by any triangle
       is_segment_intersected = .false.
       RETURN
    end if

    ! We got to the other side of the surface
    ! if there were odd number of intersections
    is_segment_intersected = modulo(nIntersect, 2) == 1

  end function is_segment_intersected
  !============================================================================
  subroutine calc_electron_collision_rates(Te,nElec,i,j,k,iBlock,fen_I,fei_I)

    ! calculate all collision rates for electrons (fen, fei)
    ! (used for sources & resistivity)

    use ModMain, ONLY: nStep
    use ModAdvance, ONLY: State_VGB
    use ModPhysics, ONLY: No2SI_V, UnitN_
    use ModMultiFluid, ONLY: MassIon_I, ChargeIon_I
    use ModGeometry, ONLY: Xyz_DGB

    integer,intent(in) :: i,j,k,iBlock
    real,intent(in)    :: Te
    real,intent(in)    :: nElec
    real,intent(out)   :: fen_I(nNeuFluid)
    real,intent(out)   :: fei_I(nIonFluid)

    real :: sqrtTe

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_electron_collision_rates'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    !! electron-neutral and electron-ion collision rates
    !! provide all rates in SI units

    if (Te < 0) then
       write(*,*) NameSub,': i,j,k,iBlock =', i,j,k,iBlock
       write(*,*) NameSub,': State_VGB    =', State_VGB(:,i,j,k,iBlock)
       write(*,*) NameSub,': Xyz_DGB      =', Xyz_DGB(:,i,j,k,iBlock)
       call stop_mpi(NameSub//': negative Te')
    end if

    ! reduced temperature ~ Te
    sqrtTe = sqrt(Te)

    ! initialize all collision rates with zero
    fei_I = 0. ; fen_I = 0.

    ! Electron - neutral collision rates
    ! e - H2O,  Itikawa, Planet. Space Sci., 1971
    ! and Itikawa, Phys. Fluids 1983
    !  fen_I(H2O_) = 2.745E-5*NnNeu_IG(H2O_,i-MinI+1,j-MinJ+1,k-MinK+1)
    !               /1e6*Te**(-0.62)      !! rate in [1/s]
    if (DoEnhanceNeu) then
       fen_I(Neu1_) = 2.745E-5*State_VGB(Neu1Rho_, i, j, k, iBlock)/1e6 &
            *Te**(-0.62)/MassFluid_I(nFluid)*No2SI_V(UnitN_) &
            *max((EnhancedRatio*(DecadeDn+nStepEnhanceNeu-nStep))/DecadeDn,&
            1.0)
    else
       fen_I(Neu1_) = 2.745E-5*State_VGB(Neu1Rho_, i, j, k, iBlock)/1e6 &
            *Te**(-0.62)/MassFluid_I(nFluid)*No2SI_V(UnitN_)
    end if

    ! Electron - ion collision rates
    ! e - H2Op, Schunk and Nagy, Ionospheres, Cambridge University Press, 2000
    ! rate in [1/s]
    fei_I(H2Op_) = 54.5*ChargeIon_I(H2Op_)**2* &
         State_VGB(H2OpRho_,i,j,k,iBlock)/MassIon_I(H2Op_)* &
         No2SI_V(UnitN_)/1E6/(Te*sqrtTe)
    ! e - Hp, Schunk and Nagy, Ionospheres, Cambridge University Press, 2000
    ! rate in [1/s]
    fei_I(SW_) = 54.5*ChargeIon_I(SW_)**2* &
         State_VGB(Rho_,i,j,k,iBlock)/MassIon_I(SW_)* &
         No2SI_V(UnitN_)/1E6/(Te*sqrtTe)

    if(DoTest)then
       write(*,*) 'fen_I  =', fen_I
       write(*,*) 'fei_I  =', fei_I
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine calc_electron_collision_rates
  !============================================================================
  subroutine user_calc_rates(Ti_I, Te, i, j, k, iBlock, nElec, nIon_I, &
       fin_II, fii_II, fie_I, Alpha_I, kin_IIII, v_II, ve_II, &
       uElec_D, uIon_DI, Qexc_II, Qion_II, &
       DoCalcShading, IsIntersectedShapeR, &
       DoCalcDistance2Fieldline, IsWithinTheRingR)

    ! calculate all rates not involving electron collisions

    use ModConst, ONLY: cElectronMass, cProtonMass
    use ModMain, ONLY: nStep, tSimulation
    use ModGeometry, ONLY: r_GB, Xyz_DGB

    integer,intent(in) :: i, j, k, iBlock
    real,intent(in)    :: Ti_I(nIonFluid)
    real,intent(in)    :: Te
    real,intent(in)    :: nElec
    real,intent(in)    :: nIon_I(nIonFluid)
    real,intent(in)    :: uElec_D(3)
    real,intent(in)    :: uIon_DI(3,nIonFluid)
    logical,intent(in) :: DoCalcShading
    logical,intent(in) :: DoCalcDistance2Fieldline
    real,intent(out)   :: fin_II(nIonFluid,nNeuFluid)
    real,intent(out)   :: fii_II(nIonFluid,nIonFluid)
    real,intent(out)   :: fie_I(nIonFluid)
    real,intent(out)   :: Alpha_I(nIonFluid)
    real,intent(out)   :: kin_IIII(nIonFluid,nNeuFluid,nNeuFluid,nIonFluid)
    real,intent(out)   :: v_II(nNeuFluid,nIonFluid)
    real,intent(out)   :: ve_II(nNeuFluid,nIonFluid)
    real,intent(out)   :: Qexc_II(nNeuFluid,nIonFluid)
    real,intent(out)   :: Qion_II(nNeuFluid,nIonFluid)
    real,intent(inout) :: IsIntersectedShapeR
    real,intent(inout) :: IsWithinTheRingR

    real :: Tred, Mred
    real :: DistProjection2, CosAngleTmp, NCol, sigma, J3, log10Te, sqrtTe
    real :: Sigma_e(nNeuFluid,nIonFluid)
    integer :: n
    real, save :: ElImpRate_I(nNeuFluid,61)!, ElCrossSect_I(61)

    ! Ionization cross section for 20 eV electrons [m^2]
    real, save :: sigmaeh2o = 4.53E-21

    ! Speed of 20 eV electrons [m/s]
    real, save :: ve = 2.65E6

    !$omp threadprivate( ElImpRate_I,sigmaeh2o,ve )

    logical :: IsIntersectedShape

    real    :: nTmp

    real    :: Distance2Fieldline
    logical :: DoWriteVeIncreaseOnce = .true.
    !$omp threadprivate( DoWriteVeIncreaseOnce )

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_calc_rates'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    if(iBlock==iBlockTest .and. i==iTest .and. j==jTest .and. &
         k==kTest .and. iProc==iProcTest) then
    else
       DoTest = .false.
       DoTest = .false.
    end if

    ! H2O and H electron impact rate depending on electron temperature
    ! (Cravens et al. 1987)
    ElImpRate_I(Neu1_,1:61) = &
         [ 0.00E+00, 1.14E-16, 2.03E-16, 3.04E-16, 4.37E-16, 6.34E-16, &
         9.07E-16, 1.28E-15, 1.79E-15, 2.34E-15, 3.15E-15, 4.35E-15, &
         5.54E-15, 6.90E-15, 8.47E-15, 1.05E-14, 1.25E-14, 1.51E-14, &
         1.80E-14, 2.09E-14, 2.41E-14, 2.74E-14, 3.09E-14, 3.46E-14, &
         3.90E-14, 4.34E-14, 4.80E-14, 5.24E-14, 5.70E-14, 6.15E-14, &
         6.63E-14, 7.15E-14, 7.61E-14, 8.03E-14, 8.47E-14, 8.90E-14, &
         9.30E-14, 9.72E-14, 1.01E-13, 1.05E-13, 1.08E-13, 1.11E-13, &
         1.14E-13, 1.16E-13, 1.18E-13, 1.21E-13, 1.23E-13, 1.24E-13, &
         1.25E-13, 1.25E-13, 1.25E-13, 1.23E-13, 1.23E-13, 1.23E-13, &
         1.21E-13, 1.20E-13, 1.17E-13, 1.15E-13, 1.12E-13, 1.10E-13, &
         1.07E-13 ]

    ! Hydrogen-electron impact ionization cross section
    ! depending on electron energy
    ! ElCrossSect_I(1:61) = &
    !    (/ 0.000E-00, 0.114E-20, 0.189E-20, 0.257E-20, 0.320E-20, 0.377E-20, &
    !      0.429E-20, 0.473E-20, 0.510E-20, 0.542E-20, 0.568E-20, 0.587E-20, &
    !      0.600E-20, 0.609E-20, 0.613E-20, 0.613E-20, 0.608E-20, 0.600E-20, &
    !      0.588E-20, 0.575E-20, 0.559E-20, 0.541E-20, 0.521E-20, 0.501E-20, &
    !      0.480E-20, 0.457E-20, 0.435E-20, 0.414E-20, 0.392E-20, 0.369E-20, &
    !      0.348E-20, 0.328E-20, 0.307E-20, 0.288E-20, 0.270E-20, 0.252E-20, &
    !      0.235E-20, 0.219E-20, 0.204E-20, 0.190E-20, 0.176E-20, 0.163E-20, &
    !      0.152E-20, 0.141E-20, 0.130E-20, 0.120E-20, 0.111E-20, 0.103E-20, &
    !      0.095E-20, 0.088E-20, 0.081E-20, 0.075E-20, 0.069E-20, 0.063E-20, &
    !      0.059E-20, 0.054E-20, 0.050E-20, 0.045E-20, 0.042E-20, 0.039E-20, &
    !      0.036E-20 /)

    ! provide all rates in SI units

    ! initialize all collision rates with zero
    fin_II  = 0. ; fii_II  = 0. ; fie_I   = 0. ; kin_IIII = 0.
    Alpha_I = 0. ; v_II    = 0. ; ve_II   = 0.
    sigma_e = 0. ; Qexc_II = 0. ; Qion_II = 0.

    ! Ionization rates (add opacity correction/shadowing when needed)
    ! from PARAM.in
    v_II(Neu1_,H2Op_) = vHI
    ! to avoid too low densities
    v_II(Neu1_,SW_) = vHI*1e-10

    ! Electron excess energies from ionization (increases electron pressure)
    ! 12.0 eV, Huebner 1992
    Qexc_II(Neu1_,H2Op_) = 1.9226E-18

    ! Ionization potential for electron impact ionization
    ! (needs to be delivered by the ionizing electron)
    ! 12.6 eV Joshipura et al. (2007)
    Qion_II(Neu1_,H2Op_) = 2.02e-18

    ! UV opacity
    ! J3 = 4.5E14 [m^-2*s^-1]: lambda < 984A solar flux @ 1 AU, Marconi, 1982)
    J3 = 4.5E14/(rHelio**2)
    sigma = (v_II(Neu1_,H2Op_))/J3

    ! Alternative:
    ! Cross section of J3 flux for ionization (lambda < 984A) [m^2],
    ! from Marconi, 1982
    ! sigma_13=2.4E-18 cm^2: H2O + hv -> OH + H
    ! sigma_23=1.0E-18 cm^2: H2O + hv -> H2 + O(1D)
    ! sigma_33=8.4E-18 cm^2: H2O + hv -> H2Op + e
    ! sigma = 1.18E-21 ! sum(sigma_i3)

    CosAngleTmp     = sum(Xyz_DGB(:,i,j,k,iBlock)*NormalSun_D)
    DistProjection2 = r_GB(i,j,k,iBlock)**2 - CosAngleTmp**2

    ! New Block, need to check whether the cell is in the shade
    if(DoCalcShading .and. DoUseCGShape) then

       if (i == 1 .and. j == 1 .and. k ==1 .and. iBlock ==1) then
          write(*,*) NameSub, ': doing calculations. nStep, iProc =', &
               nStep, iProc
       end if

       if (DistProjection2 < rMinShape**2 .and. CosAngleTmp < 0) then
          IsIntersectedShapeR = 1.0
       else if (DistProjection2 < rMinShape**2 .and. CosAngleTmp > 0) then
          IsIntersectedShapeR = 0.0
       else if (DistProjection2 > rMaxShape**2) then
          IsIntersectedShapeR = 0.0
       else
          if (is_segment_intersected(Xyz_DGB(:,i,j,k,iBlock), &
               Xyz_DGB(:,i,j,k,iBlock)+5*rMaxShape*NormalSun_D)) then
             IsIntersectedShapeR = 1.0
          else
             IsIntersectedShapeR = 0.0
          end if
       end if
    end if

    if (.not.DoUseCGShape) then
       if (DistProjection2 < rSphericalBody**2 .and. CosAngleTmp < 0) then
          IsIntersectedShapeR = 1.0
       else
          IsIntersectedShapeR = 0.0
       end if
    end if

    if (IsIntersectedShapeR == 1.0) then
       IsIntersectedShape = .true.
    else if (IsIntersectedShapeR == 0.0) then
       IsIntersectedShape = .false.
    else
       write(*,*) 'iProc, iBlock =', iProc, iBlock
       write(*,*) 'IsIntersectedShapeR =',&
            IsIntersectedShapeR
       call stop_mpi('IsIntersectedShapeR /= 0.0 or 1.0')
    end if

    if (IsIntersectedShape) then
       v_II = v_II*1e-9
    else
       ! opacity corrections based on Haser, still need some fecth fectors
       !       NCol = Qprod/4.0/(uHaser)/(sqrt(DistProjection2)*NO2SI_V(UnitX_)) * &
       !            (0.5-1.0/cPi*atan(Xyz_DGB(x_,i,j,k,iBlock)/sqrt(DistProjection2)))
       NCol = 0.0
       v_II = v_II*exp(-sigma*NCol) + v_II*1e-9
    end if

    if (Te <= 0.0) then
       write(*,*) NameSub,': Te =', Te
       call stop_mpi('Te<0')
    end if

    log10Te = log10(max(Te,1.0))
    nTmp = (log10Te-4.45053516)/0.0415 + 1.0

    if (abs(nTmp) > 1e6) then
       write(*,*) 'iProc, Te, log10Te, nTmp   =', iProc, Te, log10Te, nTmp
    end if

    ! H2O electron impact ionization cross section after Cravens et al. 1987:
    ! H2O & e -> H2Op + 2e
    nTmp = min(60.0, max(1.0,  nTmp))
    n = int(nTmp)
    if (n > 60) n = 60
    if (n < 1) n = 1

    ! linear interpolation
    ve_II(Neu1_,H2Op_) = max(nElec*( &
         (log10Te-((n-1.0)*0.0415+4.45053516))/0.0415* &
         (ElImpRate_I(Neu1_,n+1)-ElImpRate_I(Neu1_,n))+ElImpRate_I(Neu1_,n) ),&
         0.0)

    if (DoCalcDistance2Fieldline .and. DoUseFieldlineFile) then
       Distance2Fieldline = minval( sqrt( &
            (Xyz_DGB(1,i,j,k,iBlock)-XyzFieldline_DI(1,:))**2 + &
            (Xyz_DGB(2,i,j,k,iBlock)-XyzFieldline_DI(2,:))**2 + &
            (Xyz_DGB(3,i,j,k,iBlock)-XyzFieldline_DI(3,:))**2 ))
       if (Distance2Fieldline < RadiusTube) then
          IsWithinTheRingR = 1.0
       else
          IsWithinTheRingR = -1.0
       end if
    end if

    if (.not. DoUseFieldlineFile)  IsWithinTheRingR = -1.0

    If (IsWithinTheRingR == 1.0 .and. tSimulation >  TimeEnhanceStart &
         .and. tSimulation < TimeEnhanceEnd) then
       SPeAdditional_GB(i,j,k,iBlock) = SPeAdditional
       if (DoWriteVeIncreaseOnce) then
          write(*,*) 'Additional Pe of  ', SPeAdditionalSi, ' [nPa/s] at ', &
               Xyz_DGB(:,i,j,k,iBlock)
          DoWriteVeIncreaseOnce = .false.
       end if
       if (DoTest) then
          write(*,*) 'tSimulation          = ', tSimulation
          write(*,*) 'DoCalcDistance2Fieldline = ', DoCalcDistance2Fieldline
          write(*,*) 'Distance2Fieldline       = ', Distance2Fieldline
          write(*,*) 'IsWithinTheRingR         = ', IsWithinTheRingR
          write(*,*) 'SPeAdditionalSi          = ', SPeAdditionalSi
          write(*,*) 'SPeAdditional_GB(i,j,k,iBlock) =', &
               SPeAdditional_GB(i,j,k,iBlock)
       end if
    else
       SPeAdditional_GB(i,j,k,iBlock) = 0.0
    end If

    ! Number of energetic electrons (number of equivalent 20 eV electrons)
    ne20eV_GB(i,j,k,iBlock) = ve_II(Neu1_,H2Op_)/(sigmaeh2o*ve)

    ! ********** Ion-neutral collision/charge exchange rates **********
    ! Example(s)
    ! resonant H+ & O -> O+ & H  subtracts H+ and adds O+, rate in [m^3/s]
    ! kin_IIII(Hp_,O_,Op_,H_) = &
    ! 6.61E-11/1E6*sqrt(Ti_I(Hp_))*(1.0-0.047*log10(Ti_I(Hp)))**2

    ! resonant O+ & H -> H+ & O  subtracts O+ and adds H+, rate in [m^3/s]
    ! kin_IIII(Op_,H_,Hp_,O_) = &
    ! 4.63E-12/1E6*sqrt(TnNeuFluid(H_,i-MinI+1,j-MinJ+1,k-MinK+1)+TOp_/16.)

    ! H2Op & H2O -> H2Op & H2O    ! non-resonant
    ! fin_II(H2Op_,H2O_) = 0.
    ! H2Op & H2O -> H2O & H2Op    ! resonant

    ! Gombosi et al., J. Geophys. Res., (1996)
    kin_IIII(H2Op_,Neu1_,Neu1_,H2Op_) = 1E-6*1.7E-9

    ! Hp & H2O -> H & H2Op
    ! resonant, estimate to get the same drag on SW-protons,
    ! neutral H2O product is just placeholder for fast neutral hydrogen
    !! (unused here)
    ! uSWBulk2 = &
    !    sum(State_VGB(iRhoUxIon_I(SW_):iRhoUzIon_I(SW_),i,j,k,iBlock)**2) / &
    !      State_VGB(iRhoIon_I(SW_),i,j,k,iBlock)**2*No2SI_V(UnitU_)**2
    ! uSWTherm2 = 8.*cBoltzmann*Ti_I(SW_)/(MassIon_I(SW_)*cProtonMass*cPi)
    ! sigma=3e-15 cm^2, Cometopause of comet Halley,
    ! Ip, ApJ, 343, 956-952, 1989
    ! kin_IIII(SW_,H2O_,H2O_,H2Op_) = 1E-4*3.0E-15*sqrt(uSWBulk2 + uSWTherm2)

    ! Hp & H2O -> H2O & H2Op
    ! resonant, estimate to get the same drag on SW-protons,
    ! neutral H2O product
    ! is just placeholder for fast neutral hydrogen (unused here)
    ! Gombosi et al., J. Geophys. Res., (1996), estimated
    kin_IIII(SW_,Neu1_,Neu1_,H2Op_) = 1E-6*1.7E-9

    ! ********** Ion-ion collision rates **********
    ! SWp - SWp is left zero because the they do not result in a
    ! change in the source terms
    ! Hp - Hp is left zero because the they do not result in a
    ! change in the source terms
    ! H2Op - H2Op is left zero because the they do not result in a change
    ! in the source terms

    ! H2Op - SWp, Coulomb collision
    ! Schunk and Nagy, Ionospheres,Cambridge University Press, 2000
    ! reduced temp
    Tred = (MassIon_I(H2Op_)*Ti_I(SW_)+MassIon_I(SW_)*Ti_I(H2Op_))/ &
         (MassIon_I(H2Op_)+MassIon_I(SW_))
    ! reduced mass
    Mred = MassIon_I(H2Op_)*MassIon_I(SW_)/(MassIon_I(H2Op_)+MassIon_I(SW_))

    fii_II(H2Op_,SW_) = 1.27*ChargeIon_I(H2Op_)**2 * &
         ChargeIon_I(SW_)**2/MassIon_I(H2Op_) * &
         sqrt(Mred)*1e-6*nIon_I(SW_)/(Tred*sqrt(Tred))
    ! SWp - H2Op, Coulomb collision
    ! Schunk and Nagy, Ionospheres,Cambridge University Press, 2000
    fii_II(SW_,H2Op_) = 1.27*ChargeIon_I(H2Op_)**2*ChargeIon_I(SW_)**2 &
         /MassIon_I(SW_)*sqrt(Mred)*1e-6*nIon_I(H2Op_)/(Tred*sqrt(Tred))

    ! Ion - electron collision rates,
    ! reduced mass=~me and reduced temperature=~Te
    sqrtTe = sqrt(Te)
    ! H2Op - e, Schunk and Nagy, Ionospheres, Cambridge University Press, 2000
    ! rate in [1/s]
    fie_I(H2Op_) = 1.27*sqrt(cElectronMass/cProtonMass)/MassIon_I(H2Op_) &
         *ChargeIon_I(H2Op_)**2*nElec/1E6/(Te*sqrtTe)
    ! Hp - e, Schunk and Nagy, Ionospheres, Cambridge University Press, 2000
    ! rate in [1/s]
    fie_I(SW_) = 1.27*sqrt(cElectronMass/cProtonMass)/MassIon_I(SW_)* &
         ChargeIon_I(SW_)**2*nElec/1E6/(Te*sqrtTe)

    ! ********** Ion-electron recombination rates **********

    ! Schunk and Nagy, Ionospheres,Cambridge University Press, 2000
    ! rate in [m^3/s]
    if (Te < 800.) then
       Alpha_I(H2Op_) = 1E-6*1.57E-5*Te**(-0.569)
    elseif (Te<4000) then
       Alpha_I(H2Op_) = 1E-6*4.73E-5*Te**(-0.74)
    else
       Alpha_I(H2Op_) = 1E-6*1.03E-3*Te**(-1.111)
    end if

    ! rate in [m^3/s]
    ! if (Te < 200.) then
    !    Alpha_I(H2Op_) = 1E-6*7E-7*sqrt(300./Te)
    ! else
    !    Alpha_I(H2Op_) = 2.342*1E-6*7E-7*Te**(0.2553-0.1633*log10(Te))
    ! end if

    ! Schunk and Nagy, Ionospheres,Cambridge University Press, 2000
    Alpha_I(SW_)   = 1E-6*4.8E-12*(250/Te)**0.7

    ! Schmidt et al., Comput. Phys. Commun. (1988)
    ! Alpha_I(SW_)   = 1E-6*3.5E-12*(Te/300)**(-0.7)

    if (DoTest .and. .false.) then
       write(*,*) NameSub
       write(*,*) ' fin_II   =', fin_II
       write(*,*) ' fii_II   =', fii_II
       write(*,*) ' fie_I    =', fie_I
       write(*,*) ' Alpha_I  =', Alpha_I
       write(*,*) ' kin_IIII =', kin_IIII
       write(*,*) ' v_II     =', v_II
       write(*,*) ' ve_II    =', ve_II
       write(*,*) ' Qexc_II  =', Qexc_II
       write(*,*) ' Qion_II  =', Qion_II
       write(*,*) ' IsIntersectedShapeR =', &
            IsIntersectedShapeR
       write(*,*) ' IsIntersectedShape  =', IsIntersectedShape
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_calc_rates
  !============================================================================
  subroutine user_calc_sources_impl(iBlock)

    use ModMain, ONLY: nI, nJ, nK, nStep, tSimulation, iNewDecomposition
    use ModAdvance, ONLY: State_VGB, Source_VC, Bx_,By_,Bz_, P_
    use ModConst, ONLY: cBoltzmann, cElectronMass, cProtonMass, cEV
    use ModGeometry, ONLY: r_GB, Xyz_DGB
    use ModCurrent, ONLY: get_current
    use ModPhysics, ONLY: Si2No_V, No2Si_V, &
         UnitEnergyDens_, UnitN_, UnitRho_, UnitU_, UnitP_, UnitT_, &
         UnitRhoU_, UnitTemperature_, UnitX_, &
         ElectronPressureRatio, ElectronCharge
    use ModVarIndexes, ONLY: MassFluid_I
    use ModBlockData, ONLY: use_block_data, get_block_data, put_block_data

    integer, intent(in) :: iBlock

    integer, parameter:: nRhoTerm=5, nRhoUTerm=7, nPTerm=11, nPeTerm =10

    real, dimension(nI,nJ,nK):: nElec_C, Te_C, SBx_C, SBy_C, SBz_C, &
         SPe_C, TempNeu1_C, nNeu1_C
    real, dimension(nRhoTerm, nIonFluid,nI,nJ,nK):: SRhoTerm_IIC
    real, dimension(nRhoUTerm,nIonFluid,nI,nJ,nK):: SRhoUxTerm_IIC, &
         SRhoUyTerm_IIC, SRhoUzTerm_IIC
    real, dimension(nPTerm,nIonFluid,nI,nJ,nK):: SPTerm_IIC
    real, dimension(nPeTerm,nI,nJ,nK):: SPeTerm_IC
    real, dimension(3,nIonFluid,nI,nJ,nK):: uIon_DIC
    real, dimension(3,nI,nJ,nK):: Current_DC, uIonMean_DC, uElec_DC, &
         uNeu1_DC
    real, dimension(nIonFluid,nIonFluid,nI,nJ,nK):: fii_IIC, uIonIon2_IIC
    real, dimension(nIonFluid,nI,nJ,nK):: &
         Ti_IC, uIonElec2_IC, fei_IC, fie_IC, &
         nIon_IC, SRho_IC, SRhoUx_IC, SRhoUy_IC, SRhoUz_IC, SP_IC
    real, dimension(nIonFluid,nI,nJ,nK):: Alpha_IC
    real, dimension(nIonFluid,nNeuFluid,nI,nJ,nK):: fin_IIC, uIonNeu2_IIC
    real, dimension(nNeuFluid,nI,nJ,nK):: fen_IC, uNeuElec2_IC
    real, dimension(nNeuFluid,nIonFluid) :: Qexc_II, Qion_II
    real, dimension(nIonFluid) :: fiiTot_I, finTot_I, vAdd_I, veAdd_I, &
         kinAdd_I, kinSub_I

    real:: kin_IIIIC(nIonFluid,nNeuFluid,nNeuFluid,nIonFluid,nI,nJ,nK)

    real    :: fenTot, feiTot
    integer :: i,j,k,iNeuFluid,jNeutral,iIonFluid,jIonFluid,iTerm

    logical :: DoTestCell
    logical :: DoCalcShading = .false.
    integer, save :: iBlockLast = -100
    integer, save :: iLastDecomposition = -100
    real,    save :: IsIntersectedShapeR_III(nI,nJ,nK) = -1.0

    logical :: DoCalcDistance2Fieldline = .false.
    real,    save :: IsWithinTheRingR_III(nI,nJ,nK) = -1.0

    ! OpenMP declarations
    !$omp threadprivate( DoCalcShading )
    !$omp threadprivate( iBlockLast,iLastDecomposition )
    !$omp threadprivate( IsIntersectedShapeR_III )
    !$omp threadprivate( IsWithinTheRingR_III )
    !$omp threadprivate( DoCalcDistance2Fieldline )

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_calc_sources_impl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if (iNewDecomposition /= iLastDecomposition) then
       iBlockLast         = -100
       iLastDecomposition = iNewDecomposition
    end if

    !! Set the source arrays for this block to zero
    SRho_IC        = 0.
    SRhoTerm_IIC   = 0.
    SRhoUx_IC      = 0.
    SRhoUxTerm_IIC = 0.
    SRhoUy_IC      = 0.
    SRhoUyTerm_IIC = 0.
    SRhoUz_IC      = 0.
    SRhoUzTerm_IIC = 0.
    SBx_C          = 0.
    SBy_C          = 0.
    SBz_C          = 0.
    SP_IC          = 0.
    SPTerm_IIC     = 0.
    SPe_C          = 0.
    SPeTerm_IC     = 0.

    do k=1,nK; do j=1,nJ; do i=1,nI
       if (DoUseHaserBackground) then
          if (r_GB(i,j,k,iBlock) > rSphericalBody) then
             State_VGB(Neu1Rho_,i,j,k,iBlock) = Qprod/                        &
                  (4.*cPi*(r_GB(i,j,k,iBlock)*No2Si_V(UnitX_))**2*uHaser ) * &
                  exp(-vHI*r_GB(i,j,k,iBlock)*No2Si_V(UnitX_)/uHaser)*       &
                  Si2No_V(UnitN_) * MassFluid_I(nFluid)
             State_VGB(Neu1Ux_:Neu1Uz_,i,j,k,iBlock) =                     &
                  State_VGB(Neu1Rho_,i,j,k,iBlock)*uHaser*Si2No_V(UnitU_)* &
                  Xyz_DGB(:,i,j,k,iBlock)/r_GB(i,j,k,iBlock)
             State_VGB(Neu1P_,i,j,k,iBlock)     =                          &
                  State_VGB(Neu1Rho_,i,j,k,iBlock)/MassFluid_I(nFluid)     &
                  *TempNeuMinSi*Si2No_V(UnitTemperature_)
          else
             State_VGB(Neu1Rho_,i,j,k,iBlock) = Qprod/                  &
                  (4.*cPi*rSphericalBodySi**2*uHaser )*                 &
                  exp(-vHI*rSphericalBodySi/uHaser)*                    &
                  Si2No_V(UnitN_) * MassFluid_I(nFluid)
             State_VGB(Neu1Ux_:Neu1Uz_,i,j,k,iBlock) = 0.0
             State_VGB(Neu1P_,i,j,k,iBlock)     =                       &
                  State_VGB(Neu1Rho_,i,j,k,iBlock)/MassFluid_I(nFluid)  &
                  *TempNeuMinSi*Si2No_V(UnitTemperature_)
          end if
       else if (DoUseUniformNeuBackground) then
          State_VGB(Neu1Rho_,i,j,k,iBlock) = nNeuUniform*MassFluid_I(nFluid)
          State_VGB(Neu1Ux_, i,j,k,iBlock) = UxNeuUniform
          State_VGB(Neu1Uy_, i,j,k,iBlock) = UyNeuUniform
          State_VGB(Neu1Uz_, i,j,k,iBlock) = UzNeuUniform
          State_VGB(Neu1P_,  i,j,k,iBlock) = pNeuUniform
       end if
    end do; end do; end do

    ! nElec_C is the electron/ion density in SI units ( n_e=sum(n_i*Zi) )
    do k=1,nK; do j=1,nJ; do i=1,nI
       nIon_IC(1:nIonFluid,i,j,k) = &
            State_VGB(iRhoIon_I,i,j,k,iBlock)/MassIon_I*No2SI_V(UnitN_)
       nElec_C(i,j,k) = &
            sum(nIon_IC(1:nIonFluid,i,j,k)*ChargeIon_I(1:nIonFluid))
    end do; end do; end do

    ! ion velocity components in SI
    uIon_DIC(1,1:nIonFluid,1:nI,1:nJ,1:nK) = &
         State_VGB(iRhoUxIon_I,1:nI,1:nJ,1:nK,iBlock) / &
         State_VGB(iRhoIon_I,  1:nI,1:nJ,1:nK,iBlock) *No2SI_V(UnitU_)
    uIon_DIC(2,1:nIonFluid,1:nI,1:nJ,1:nK) = &
         State_VGB(iRhoUyIon_I,1:nI,1:nJ,1:nK,iBlock) / &
         State_VGB(iRhoIon_I,  1:nI,1:nJ,1:nK,iBlock) *No2SI_V(UnitU_)
    uIon_DIC(3,1:nIonFluid,1:nI,1:nJ,1:nK) = &
         State_VGB(iRhoUzIon_I,1:nI,1:nJ,1:nK,iBlock) / &
         State_VGB(iRhoIon_I,  1:nI,1:nJ,1:nK,iBlock) *No2SI_V(UnitU_)
    uIonMean_DC(1:3,1:nI,1:nJ,1:nK) = 0.

    do iIonFluid=1,nIonFluid
       uIonMean_DC(1,1:nI,1:nJ,1:nK) = uIonMean_DC(1,1:nI,1:nJ,1:nK) + &
            nIon_IC(   iIonFluid,1:nI,1:nJ,1:nK) * &
            uIon_DIC(1,iIonFluid,1:nI,1:nJ,1:nK) * ChargeIon_I(iIonFluid) / &
            nElec_C(1:nI,1:nJ,1:nK)
       uIonMean_DC(2,1:nI,1:nJ,1:nK) = uIonMean_DC(2,1:nI,1:nJ,1:nK) + &
            nIon_IC(   iIonFluid,1:nI,1:nJ,1:nK)* &
            uIon_DIC(2,iIonFluid,1:nI,1:nJ,1:nK) * ChargeIon_I(iIonFluid) / &
            nElec_C(1:nI,1:nJ,1:nK)
       uIonMean_DC(3,1:nI,1:nJ,1:nK) = uIonMean_DC(3,1:nI,1:nJ,1:nK) + &
            nIon_IC(   iIonFluid,1:nI,1:nJ,1:nK)* &
            uIon_DIC(3,iIonFluid,1:nI,1:nJ,1:nK) * ChargeIon_I(iIonFluid) / &
            nElec_C(1:nI,1:nJ,1:nK)
    end do

    ! Neu1 velocity componet in SI
    uNeu1_DC(1, 1:nI,1:nJ,1:nK) = &
         State_VGB(Neu1RhoUx_,1:nI,1:nJ,1:nK,iBlock) / &
         State_VGB(Neu1Rho_,  1:nI,1:nJ,1:nK,iBlock) *No2SI_V(UnitU_)
    uNeu1_DC(2, 1:nI,1:nJ,1:nK) = &
         State_VGB(Neu1RhoUy_,1:nI,1:nJ,1:nK,iBlock) / &
         State_VGB(Neu1Rho_,  1:nI,1:nJ,1:nK,iBlock) *No2SI_V(UnitU_)
    uNeu1_DC(3, 1:nI,1:nJ,1:nK) = &
         State_VGB(Neu1RhoUz_,1:nI,1:nJ,1:nK,iBlock) / &
         State_VGB(Neu1Rho_,  1:nI,1:nJ,1:nK,iBlock) *No2SI_V(UnitU_)

    ! Neu1 temperature in SI
    TempNeu1_C = State_VGB(Neu1P_,1:nI,1:nJ,1:nK,iBlock)* &
         MassFluid_I(nFluid)/State_VGB(Neu1Rho_,1:nI,1:nJ,1:nK,iBlock) * &
         No2SI_V(UnitTemperature_)

    ! Neu1 density in SI
    if (DoEnhanceNeu) then
       nNeu1_C  = State_VGB(Neu1Rho_,1:nI,1:nJ,1:nK,iBlock) &
            / MassFluid_I(nFluid)*No2SI_V(UnitN_) &
            * max((EnhancedRatio*(DecadeDn+nStepEnhanceNeu-nStep))/DecadeDn,&
            1.0)
    else
       nNeu1_C  = State_VGB(Neu1Rho_,1:nI,1:nJ,1:nK,iBlock) &
            / MassFluid_I(nFluid)*No2SI_V(UnitN_)
    end if

    ! (u_i-u_n)^2 in SI
    do iIonFluid=1,nIonFluid
       do iNeuFluid=1,nNeuFluid
          uIonNeu2_IIC(iIonFluid,iNeuFluid,1:nI,1:nJ,1:nK) = &
               (uIon_DIC(1,iIonFluid,1:nI,1:nJ,1:nK) - &
               uNeu1_DC( 1,          1:nI,1:nJ,1:nK))**2 + &
               (uIon_DIC(2,iIonFluid,1:nI,1:nJ,1:nK) - &
               uNeu1_DC( 2,          1:nI,1:nJ,1:nK))**2 + &
               (uIon_DIC(3,iIonFluid,1:nI,1:nJ,1:nK) - &
               uNeu1_DC( 3,          1:nI,1:nJ,1:nK))**2
       end do
    end do

    !! (u_i1-u_i2)^2 in SI
    do iIonFluid=1,nIonFluid
       do jIonFluid=1,nIonFluid
          uIonIon2_IIC(iIonFluid,jIonFluid,:,:,:) = &
               (uIon_DIC(1,iIonFluid,1:nI,1:nJ,1:nK) - &
               uIon_DIC( 1,jIonFluid,1:nI,1:nJ,1:nK))**2+&
               (uIon_DIC(2,iIonFluid,1:nI,1:nJ,1:nK) - &
               uIon_DIC( 2,jIonFluid,1:nI,1:nJ,1:nK))**2+&
               (uIon_DIC(3,iIonFluid,1:nI,1:nJ,1:nK) - &
               uIon_DIC( 3,jIonFluid,1:nI,1:nJ,1:nK))**2
       end do
    end do

    if (UseElectronPressure) then
       ! Electron temperature calculated from electron pressure
       ! Ion temperature is calculated from ion pressure
       do k=1,nK; do j=1,nJ; do i=1,nI
          Ti_IC(1:nIonFluid,i,j,k) = &
               State_VGB(iPIon_I,i,j,k,iBlock)*NO2SI_V(UnitP_) / &
               (cBoltzmann* &
               State_VGB(iRhoIon_I,i,j,k,iBlock)/MassIon_I*NO2SI_V(UnitN_))
          Te_C(i,j,k) = State_VGB(Pe_,i,j,k,iBlock)*NO2SI_V(UnitP_) / &
               (cBoltzmann*nElec_C(i,j,k))
       end do; end do; end do
    else
       ! Electron temperature calculated from pressure
       ! assuming Te_C=Ti_IC*ElectronTemperatureRatio:
       ! p=nkT with n_e=n_i*Z_i (quasi-neutrality),
       ! n=n_e+n_i and p=p_e+p_i=p_i*(1+ElectronPressureRatio)
       do k=1,nK; do j=1,nJ; do i=1,nI
          Ti_IC(1:nIonFluid,i,j,k) = &
               State_VGB(iPIon_I,i,j,k,iBlock)*NO2SI_V(UnitP_)/ &
               (cBoltzmann* &
               State_VGB(iRhoIon_I,i,j,k,iBlock)/MassIon_I*NO2SI_V(UnitN_))
          Te_C(i,j,k) = State_VGB(P_,i,j,k,iBlock)*ElectronPressureRatio / &
               (1.+ElectronPressureRatio)*NO2SI_V(UnitP_) / &
               (cBoltzmann*nElec_C(i,j,k))
       end do; end do; end do
    end if

    if (iBlock /= iBlockLast) then
       iBlocklast = iBlock
       if (use_block_data(iBlock) .and. .not. DoCalcShading) then
          call get_block_data(iBlock,nI,nJ,nK, IsIntersectedShapeR_III)
          !          write(*,*) 'iProc, iBlock get block data: ', iProc, iBlock, nStep
       end if

       if (use_block_data(iBlock) .and. .not.  DoCalcDistance2Fieldline) then
          call get_block_data(iBlock,nI,nJ,nK, IsWithinTheRingR_III)
          ! write(*,*) 'getting block data for IsWithinTheRingR_III'
       end if

       if (.not. use_block_data(iBlock)) then
          DoCalcShading = .true.
          DoCalcDistance2Fieldline = .true.
          if (iProc == 1 .and. iBlock ==1) &
               write(*,*) 'Calculating the distance to the field line.'
       end if
    end if

    do k=1,nK; do j=1,nJ; do i=1,nI

       DoTestCell = DoTest .and. i == iTest .and. j == jTest .and. k == kTest

       call get_current(i,j,k,iBlock,Current_DC(:,i,j,k))

       ! calculate uElec_DC from Hall velocity -J/(e*n) [m/s]
       uElec_DC(1:3,i,j,k) = uIonMean_DC(1:3,i,j,k) - &
            Current_DC(1:3,i,j,k)/(nElec_C(i,j,k)*Si2No_V(UnitN_) * &
            ElectronCharge)*No2SI_V(UnitU_)

       call calc_electron_collision_rates( &
            Te_C(i,j,k),nElec_C(i,j,k),i,j,k,iBlock,fen_IC(1:nNeuFluid,i,j,k),&
            fei_IC(1:nIonFluid,i,j,k))
       call user_calc_rates( &
            Ti_IC(1:nIonFluid,i,j,k), Te_C(i,j,k),i,j,k,iBlock, &
            nElec_C(i,j,k), nIon_IC(1:nIonFluid,i,j,k),&
            fin_IIC(1:nIonFluid,1:nNeuFluid,i,j,k), &
            fii_IIC(1:nIonFluid,1:nIonFluid,i,j,k),fie_IC(1:nIonFluid,i,j,k),&
            Alpha_IC(1:nIonFluid,i,j,k), &
            kin_IIIIC(1:nIonFluid,1:nNeuFluid,1:nNeuFluid,1:nIonFluid,i,j,k),&
            v_IIGB(1:nNeuFluid,1:nIonFluid,i,j,k,iBlock), &
            ve_IIGB(1:nNeuFluid,1:nIonFluid,i,j,k,iBlock),&
            uElec_DC(1:3,i,j,k),uIon_DIC(1:3,1:nIonFluid,i,j,k),&
            Qexc_II(1:nNeuFluid,1:nIonFluid),Qion_II(1:nNeuFluid,1:nIonFluid),&
            DoCalcShading, IsIntersectedShapeR_III(i,j,k), &
            DoCalcDistance2Fieldline,IsWithinTheRingR_III(i,j,k))

       ! Zeroth moment
       ! Sources separated into the terms by
       ! Tamas' "Transport Equations for Multifluid Magnetized Plasmas"
       kinAdd_I = 0. ; kinSub_I = 0.
       do iIonFluid=1,nIonFluid
          do jIonFluid=1,nIonFluid
             do iNeuFluid=1,nNeuFluid
                do jNeutral=1,nNeuFluid
                   ! addition to individual fluid from charge exchange
                   ! unit in [1/(m^3*s)]
                   kinAdd_I(jIonFluid) = kinAdd_I(jIonFluid) + &
                        nIon_IC(iIonFluid,i,j,k)* &
                        kin_IIIIC(iIonFluid,iNeuFluid,jNeutral,jIonFluid, &
                        i,j,k) * nNeu1_C(i,j,k)
                   ! subtraction to individual fluid from charge exchange
                   ! unit in [1/(m^3*s)]
                   kinSub_I(iIonFluid) = kinSub_I(iIonFluid) + &
                        nIon_IC(iIonFluid,i,j,k)* &
                        kin_IIIIC(iIonFluid,iNeuFluid,jNeutral,jIonFluid, &
                        i,j,k) * nNeu1_C(i,j,k)
                end do
             end do
          end do
       end do

       ! Sources divideded into the terms
       ! by Tamas' "Transport Equations for Multifluid Magnetized Plasmas"
       vAdd_I = 0. ; veAdd_I = 0.
       do iNeuFluid=1,nNeuFluid
          vAdd_I(1:nIonFluid)  = vAdd_I(1:nIonFluid)  + &
               v_IIGB(iNeuFluid, 1:nIonFluid,i,j,k,iBlock)*nNeu1_C(i,j,k)
          veAdd_I(1:nIonFluid) = veAdd_I(1:nIonFluid) + &
               ve_IIGB(iNeuFluid,1:nIonFluid,i,j,k,iBlock)*nNeu1_C(i,j,k)
       end do

       ! photo-ionization
       SRhoTerm_IIC(1,1:nIonFluid,i,j,k) = &
            vAdd_I(1:nIonFluid)*Si2No_V(UnitN_)/Si2No_V(UnitT_)*MassIon_I

       ! electron impact ionization
       SRhoTerm_IIC(2,1:nIonFluid,i,j,k) = &
            veAdd_I(1:nIonFluid)*Si2No_V(UnitN_)/Si2No_V(UnitT_)*MassIon_I

       ! ion-neutral charge exchange
       SRhoTerm_IIC(3,1:nIonFluid,i,j,k) = &
            kinAdd_I(1:nIonFluid)*Si2No_V(UnitN_)/Si2No_V(UnitT_)*MassIon_I

       ! ion-neutral charge exchange
       SRhoTerm_IIC(4,1:nIonFluid,i,j,k) = &
            -kinSub_I(1:nIonFluid)*Si2No_V(UnitN_)/Si2No_V(UnitT_)*MassIon_I

       ! loss due to recombination
       SRhoTerm_IIC(5,1:nIonFluid,i,j,k) = &
            -Alpha_IC(1:nIonFluid,i,j,k)*(nElec_C(i,j,k)* &
            nIon_IC(1:nIonFluid,i,j,k)*Si2No_V(UnitN_)/Si2No_V(UnitT_)) * &
            MassIon_I

       ! First moment, x component
       ! d(rho_s*u_s)/dt = rho_s*du_s/dt + u_s*drho_s/dt
       ! combined from zeroth and first moment
       ! by Tamas' "Transport Equations for Multifluid Magnetized Plasmas"
       fiiTot_I = 0. ; finTot_I = 0. ; vAdd_I = 0. ; veAdd_I = 0.

       ! ion-ion collisions
       do iIonFluid=1,nIonFluid
          fiiTot_I(1:nIonFluid) = fiiTot_I(1:nIonFluid) + &
               fii_IIC(1:nIonFluid,iIonFluid,i,j,k)     * &
               (uIon_DIC(1,iIonFluid,i,j,k) - uIon_DIC(1,1:nIonFluid,i,j,k))
       end do

       ! momentum transfer by ion-neutral collisions
       do iNeuFluid=1,nNeuFluid
          finTot_I(1:nIonFluid) = finTot_I(1:nIonFluid)                   + &
               fin_IIC(1:nIonFluid,iNeuFluid,i,j,k)                       * &
               (uNeu1_DC(1,i,j,k) - uIon_DIC(1,1:nIonFluid,i,j,k))
          vAdd_I(1:nIonFluid)  = vAdd_I(1:nIonFluid)                      + &
               v_IIGB(iNeuFluid,1:nIonFluid,i,j,k,iBlock)*nNeu1_C(i,j,k)  * &
               (uNeu1_DC(1,i,j,k) - uIon_DIC(1,1:nIonFluid,i,j,k))
          veAdd_I(1:nIonFluid) = veAdd_I(1:nIonFluid)                     + &
               ve_IIGB(iNeuFluid,1:nIonFluid,i,j,k,iBlock)*nNeu1_C(i,j,k) * &
               (uNeu1_DC(1,i,j,k) - uIon_DIC(1,1:nIonFluid,i,j,k))
       end do

       kinAdd_I = 0.
       do iIonFluid=1,nIonFluid
          do jIonFluid=1,nIonFluid
             do iNeuFluid=1,nNeuFluid
                do jNeutral=1,nNeuFluid
                   ! addition to individual fluid from charge exchange
                   ! in [1/(m^3*s)]
                   kinAdd_I(jIonFluid) = kinAdd_I(jIonFluid) + &
                        nIon_IC(iIonFluid,i,j,k)* &
                        kin_IIIIC(iIonFluid,iNeuFluid,jNeutral,jIonFluid, &
                        i,j,k)*nNeu1_C(i,j,k)*&
                        (uNeu1_DC(1,i,j,k) - uIon_DIC(1,jIonFluid,i,j,k))
                end do
             end do
          end do
       end do

       ! newly photoionized neutrals
       SRhoUxTerm_IIC(1,1:nIonFluid,i,j,k) =    &
            vAdd_I(1:nIonFluid)/Si2No_V(UnitT_) &
            *Si2No_V(UnitN_)*Si2No_V(UnitU_)*MassIon_I

       ! newly electron-impact
       SRhoUxTerm_IIC(2,1:nIonFluid,i,j,k) =     &
            veAdd_I(1:nIonFluid)/Si2No_V(UnitT_) &
            *Si2No_V(UnitN_)*Si2No_V(UnitU_)*MassIon_I

       ! charge exchange
       SRhoUxTerm_IIC(3,1:nIonFluid,i,j,k) =      &
            kinAdd_I(1:nIonFluid)/Si2No_V(UnitT_) &
            *Si2No_V(UnitN_)*MassIon_I*Si2No_V(UnitU_)

       ! ion-electron
       SRhoUxTerm_IIC(4,1:nIonFluid,i,j,k) = -fie_IC(1:nIonFluid,i,j,k) / &
            Si2No_V(UnitT_)*MassIon_I*nIon_IC(1:nIonFluid,i,j,k)* &
            Si2No_V(UnitN_)* &
            (uIon_DIC(1,1:nIonFluid,i,j,k)-uElec_DC(1,i,j,k)) * &
            Si2No_V(UnitU_)

       ! ion-ion collisions
       SRhoUxTerm_IIC(5,1:nIonFluid,i,j,k) = &
            nIon_IC(1:nIonFluid,i,j,k)*Si2No_V(UnitRho_) * &
            fiiTot_I(1:nIonFluid)* &
            Si2No_V(UnitU_)/Si2No_V(UnitT_)*cProtonMass*MassIon_I

       ! ion neutral collisions
       SRhoUxTerm_IIC(6,1:nIonFluid,i,j,k) = &
            nIon_IC(1:nIonFluid,i,j,k)*Si2No_V(UnitRho_) * &
            finTot_I(1:nIonFluid)* &
            Si2No_V(UnitU_)/Si2No_V(UnitT_)*cProtonMass*MassIon_I

       ! Add u_s*drho_s/dt for d(rho_s*u_s)/dt = rho_s*du_s/dt + u_s*drho_s/dt
       do iIonFluid=1,nIonFluid
          SRhoUxTerm_IIC(7,iIonFluid,i,j,k) = &
               sum(SRhoTerm_IIC(1:5,iIonFluid,i,j,k))&
               *uIon_DIC(1,iIonFluid,i,j,k)*Si2No_V(UnitU_)
       end do

       ! First moment, y component
       ! Sources separated into the terms by
       ! Tamas' "Transport Equations for Multifluid Magnetized Plasmas"
       fiiTot_I = 0. ; finTot_I = 0. ; vAdd_I = 0. ; veAdd_I = 0.

       ! momentum transfer by ion-ion collisions
       do iIonFluid=1,nIonFluid
          fiiTot_I(1:nIonFluid) = fiiTot_I(1:nIonFluid) + &
               fii_IIC(1:nIonFluid,iIonFluid,i,j,k)     * &
               (uIon_DIC(2,iIonFluid,i,j,k) - uIon_DIC(2,1:nIonFluid,i,j,k))
       end do

       ! ion-neutral collisions
       do iNeuFluid=1,nNeuFluid
          finTot_I(1:nIonFluid) = finTot_I(1:nIonFluid)                   + &
               fin_IIC(1:nIonFluid,iNeuFluid,i,j,k)                       * &
               (uNeu1_DC(2,i,j,k) - uIon_DIC(2,1:nIonFluid,i,j,k))
          vAdd_I(1:nIonFluid)  = vAdd_I(1:nIonFluid)                      + &
               v_IIGB(iNeuFluid,1:nIonFluid,i,j,k,iBlock)*nNeu1_C(i,j,k)  * &
               (uNeu1_DC(2,i,j,k) - uIon_DIC(2,1:nIonFluid,i,j,k))
          veAdd_I(1:nIonFluid) = veAdd_I(1:nIonFluid)                     + &
               ve_IIGB(iNeuFluid,1:nIonFluid,i,j,k,iBlock)*nNeu1_C(i,j,k) * &
               (uNeu1_DC(2,i,j,k) - uIon_DIC(2,1:nIonFluid,i,j,k))
       end do

       ! addition to individual fluid from charge exchange [1/(m^3*s)]
       kinAdd_I = 0.
       do iIonFluid = 1, nIonFluid
          do jIonFluid = 1, nIonFluid
             do iNeuFluid = 1, nNeuFluid
                do jNeutral = 1, nNeuFluid
                   kinAdd_I(jIonFluid) = kinAdd_I(jIonFluid) + &
                        nIon_IC(iIonFluid,i,j,k)* &
                        kin_IIIIC(iIonFluid,iNeuFluid,jNeutral,jIonFluid, &
                        i,j,k)*nNeu1_C(i,j,k)*&
                        (uNeu1_DC(2,i,j,k) - uIon_DIC(2,jIonFluid,i,j,k))
                end do
             end do
          end do
       end do

       ! newly photoionized neutrals
       SRhoUyTerm_IIC(1,1:nIonFluid,i,j,k) =    &
            vAdd_I(1:nIonFluid)/Si2No_V(UnitT_) &
            *Si2No_V(UnitN_)*Si2No_V(UnitU_)*MassIon_I

       ! newly electron-impact
       SRhoUyTerm_IIC(2,1:nIonFluid,i,j,k) =     &
            veAdd_I(1:nIonFluid)/Si2No_V(UnitT_) &
            *Si2No_V(UnitN_)*Si2No_V(UnitU_)*MassIon_I

       ! charge exchange
       SRhoUyTerm_IIC(3,1:nIonFluid,i,j,k) =      &
            kinAdd_I(1:nIonFluid)/Si2No_V(UnitT_) &
            *Si2No_V(UnitN_)*MassIon_I*Si2No_V(UnitU_)

       ! ion-electron
       SRhoUyTerm_IIC(4,1:nIonFluid,i,j,k) = &
            -fie_IC(1:nIonFluid,i,j,k)/Si2No_V(UnitT_)*MassIon_I*&
            nIon_IC(1:nIonFluid,i,j,k)*Si2No_V(UnitN_)* &
            (uIon_DIC(2,1:nIonFluid,i,j,k) - uElec_DC(2,i,j,k))* &
            Si2No_V(UnitU_)

       ! ion-ion collisions
       SRhoUyTerm_IIC(5,1:nIonFluid,i,j,k) = &
            nIon_IC(1:nIonFluid,i,j,k)*Si2No_V(UnitRho_)* &
            fiiTot_I(1:nIonFluid)* &
            Si2No_V(UnitU_)/Si2No_V(UnitT_)*cProtonMass*MassIon_I

       ! ion neutral collisions
       SRhoUyTerm_IIC(6,1:nIonFluid,i,j,k) = &
            nIon_IC(1:nIonFluid,i,j,k)*Si2No_V(UnitRho_)* &
            finTot_I(1:nIonFluid)* &
            Si2No_V(UnitU_)/Si2No_V(UnitT_)*cProtonMass*MassIon_I

       ! Add u_s*drho_s/dt for d(rho_s*u_s)/dt = rho_s*du_s/dt + u_s*drho_s/dt
       do iIonFluid=1,nIonFluid
          SRhoUyTerm_IIC(7,iIonFluid,i,j,k) = &
               sum(SRhoTerm_IIC(1:5,iIonFluid,i,j,k))&
               *uIon_DIC(2,iIonFluid,i,j,k)*Si2No_V(UnitU_)
       end do

       ! First moment, z component
       ! Sources separated into the terms by
       ! Tamas' "Transport Equations for Multifluid Magnetized Plasmas"
       fiiTot_I = 0. ; finTot_I = 0. ; vAdd_I = 0. ; veAdd_I = 0.

       ! momentum transfer by ion-ion collisions
       do iIonFluid=1,nIonFluid
          fiiTot_I(1:nIonFluid) = fiiTot_I(1:nIonFluid) + &
               fii_IIC(1:nIonFluid,iIonFluid,i,j,k)*&
               (uIon_DIC(3,iIonFluid,i,j,k) - uIon_DIC(3,1:nIonFluid,i,j,k))
       end do

       ! ion-neutral collisions
       do iNeuFluid=1,nNeuFluid
          finTot_I(1:nIonFluid) = finTot_I(1:nIonFluid)                    + &
               fin_IIC(1:nIonFluid,iNeuFluid,i,j,k)                        * &
               (uNeu1_DC(3,i,j,k) - uIon_DIC(3,1:nIonFluid,i,j,k))
          vAdd_I(1:nIonFluid)  = vAdd_I(1:nIonFluid)                       + &
               v_IIGB(iNeuFluid,1:nIonFluid,i,j,k,iBlock) * nNeu1_C(i,j,k) * &
               (uNeu1_DC(3,i,j,k) - uIon_DIC(3,1:nIonFluid,i,j,k))
          veAdd_I(1:nIonFluid) = veAdd_I(1:nIonFluid)                      + &
               ve_IIGB(iNeuFluid,1:nIonFluid,i,j,k,iBlock)* nNeu1_C(i,j,k) * &
               (uNeu1_DC(3,i,j,k) - uIon_DIC(3,1:nIonFluid,i,j,k))
       end do

       kinAdd_I = 0.
       ! addition to individual fluid from charge exchange [1/(m^3*s)]
       do iIonFluid = 1, nIonFluid
          do jIonFluid = 1, nIonFluid
             do iNeuFluid = 1, nNeuFluid
                do jNeutral = 1, nNeuFluid
                   kinAdd_I(jIonFluid) = kinAdd_I(jIonFluid) + &
                        nIon_IC(iIonFluid,i,j,k)* &
                        kin_IIIIC(iIonFluid,iNeuFluid,jNeutral,jIonFluid, &
                        i,j,k) * nNeu1_C(i,j,k)*&
                        (uNeu1_DC(3,i,j,k) - uIon_DIC(3,jIonFluid,i,j,k))
                end do
             end do
          end do
       end do

       ! newly photoionized neutrals
       SRhoUzTerm_IIC(1,1:nIonFluid,i,j,k) =    &
            vAdd_I(1:nIonFluid)/Si2No_V(UnitT_) &
            *Si2No_V(UnitN_)*Si2No_V(UnitU_)*MassIon_I

       ! newly electron-impact
       SRhoUzTerm_IIC(2,1:nIonFluid,i,j,k) =    &
            veAdd_I(1:nIonFluid)/Si2No_V(UnitT_) &
            *Si2No_V(UnitN_)*Si2No_V(UnitU_)*MassIon_I

       ! charge exchange
       SRhoUzTerm_IIC(3,1:nIonFluid,i,j,k) =      &
            kinAdd_I(1:nIonFluid)/Si2No_V(UnitT_) &
            *Si2No_V(UnitN_)*MassIon_I*Si2No_V(UnitU_)

       ! ion-electron
       SRhoUzTerm_IIC(4,1:nIonFluid,i,j,k) = &
            -fie_IC(1:nIonFluid,i,j,k)/Si2No_V(UnitT_)*MassIon_I*&
            nIon_IC(1:nIonFluid,i,j,k)*Si2No_V(UnitN_)* &
            (uIon_DIC(3,1:nIonFluid,i,j,k) - uElec_DC(3,i,j,k))* &
            Si2No_V(UnitU_)

       ! ion-ion collisions
       SRhoUzTerm_IIC(5,1:nIonFluid,i,j,k) = nIon_IC(1:nIonFluid,i,j,k) * &
            Si2No_V(UnitRho_)*fiiTot_I(1:nIonFluid)* &
            Si2No_V(UnitU_)/Si2No_V(UnitT_)*cProtonMass*MassIon_I

       ! ion neutral collisions
       SRhoUzTerm_IIC(6,1:nIonFluid,i,j,k) = nIon_IC(1:nIonFluid,i,j,k) * &
            Si2No_V(UnitRho_)*finTot_I(1:nIonFluid)* &
            Si2No_V(UnitU_)/Si2No_V(UnitT_)*cProtonMass*MassIon_I

       ! Add u_s*drho_s/dt for d(rho_s*u_s)/dt = rho_s*du_s/dt + u_s*drho_s/dt
       do iIonFluid=1,nIonFluid
          SRhoUzTerm_IIC(7,iIonFluid,i,j,k) = &
               sum(SRhoTerm_IIC(1:5,iIonFluid,i,j,k)) &
               *uIon_DIC(3,iIonFluid,i,j,k)*Si2No_V(UnitU_)
       end do

       ! Second moment
       ! ------------------------------------------------------------------
       ! (u_n-u_e)^2 difference in neutral and electron speeds qubed [m^2/s^2]
       do iNeuFluid=1,nNeuFluid
          uNeuElec2_IC(iNeuFluid,i,j,k) =  &
               (uNeu1_DC(1,i,j,k)-uElec_DC(1,i,j,k))**2 + &
               (uNeu1_DC(2,i,j,k)-uElec_DC(2,i,j,k))**2 + &
               (uNeu1_DC(3,i,j,k)-uElec_DC(3,i,j,k))**2
       end do

       ! (u_i-u_e)^2 difference in ion and electron speeds qubed [m^2/s^2]
       do iIonFluid=1,nIonFluid
          uIonElec2_IC(iIonFluid,i,j,k) = &
               (uIon_DIC(1,iIonFluid,i,j,k) - uElec_DC(1,i,j,k))**2 +&
               (uIon_DIC(2,iIonFluid,i,j,k) - uElec_DC(2,i,j,k))**2+&
               (uIon_DIC(3,iIonFluid,i,j,k) - uElec_DC(3,i,j,k))**2
       end do

       ! Sources separated into the terms by
       ! Tamas' "Transport Equations for Multifluid Magnetized Plasmas"
       ! subtraction to individual fluid from charge exchange [1/(m^3*s)]
       kinSub_I = 0.
       do iIonFluid=1,nIonFluid
          do jIonFluid=1,nIonFluid
             do iNeuFluid=1,nNeuFluid
                do jNeutral=1,nNeuFluid
                   kinSub_I(iIonFluid) = kinSub_I(iIonFluid) + &
                        kin_IIIIC(iIonFluid,iNeuFluid,jNeutral,jIonFluid, &
                        i,j,k)* nNeu1_C(i,j,k)
                end do
             end do
          end do
       end do

       ! lost ions through charge exchange
       SPTerm_IIC(1,1:nIonFluid,i,j,k) = &
            -kinSub_I(1:nIonFluid)/Si2No_V(UnitT_)/Si2No_V(UnitT_)* &
            State_VGB(iPIon_I,i,j,k,iBlock)

       ! lost ions through recombination
       SPTerm_IIC(2,1:nIonFluid,i,j,k) = &
            -Alpha_IC(1:nIonFluid,i,j,k)*nElec_C(i,j,k)/Si2No_V(UnitT_)*&
            State_VGB(iPIon_I,i,j,k,iBlock)

       ! ion-ion collisions, temperature
       fiiTot_I(1:nIonFluid) = 0.
       do iIonFluid=1,nIonFluid
          fiiTot_I(1:nIonFluid) = fiiTot_I(1:nIonFluid) + &
               fii_IIC(1:nIonFluid,iIonFluid,i,j,k) * &
               nIon_IC(1:nIonFluid,i,j,k) * MassIon_I(1:nIonFluid) / &
               (MassIon_I(1:nIonFluid)+MassIon_I(iIonFluid))*&
               cBoltzmann*(Ti_IC(iIonFluid,i,j,k)-Ti_IC(1:nIonFluid,i,j,k))
       end do

       SPTerm_IIC(3,1:nIonFluid,i,j,k) = &
            2.*fiiTot_I(1:nIonFluid)/Si2No_V(UnitT_)*Si2No_V(UnitEnergyDens_)

       ! ion-ion collisions, velocity
       fiiTot_I(1:nIonFluid) = 0.
       do iIonFluid=1,nIonFluid
          fiiTot_I(1:nIonFluid) = fiiTot_I(1:nIonFluid)+ &
               fii_IIC(1:nIonFluid,iIonFluid,i,j,k)* &
               nIon_IC(1:nIonFluid,i,j,k)* &
               MassIon_I(1:nIonFluid)*MassIon_I(iIonFluid) / &
               (MassIon_I(1:nIonFluid)+MassIon_I(iIonFluid))*&
               uIonIon2_IIC(1:nIonFluid,iIonFluid,i,j,k)
       end do

       SPTerm_IIC(4,1:nIonFluid,i,j,k) = 2./3.*fiiTot_I(1:nIonFluid) / &
            Si2No_V(UnitT_)*Si2No_V(UnitN_)*Si2No_V(UnitU_)**2

       ! ion-neutral collisions, temperature
       finTot_I(1:nIonFluid) = 0.
       do iNeuFluid=1,nNeuFluid
          finTot_I(1:nIonFluid) = finTot_I(1:nIonFluid)+ &
               fin_IIC(1:nIonFluid,iNeuFluid,i,j,k)* &
               nIon_IC(1:nIonFluid,i,j,k)*MassIon_I(1:nIonFluid) / &
               (MassIon_I(1:nIonFluid)+MassFluid_I(nFluid))*&
               cBoltzmann*(TempNeu1_C(i,j,k)-Ti_IC(1:nIonFluid,i,j,k))
       end do

       SPTerm_IIC(5,1:nIonFluid,i,j,k) = 2.*finTot_I(1:nIonFluid) / &
            Si2No_V(UnitT_)*Si2No_V(UnitEnergyDens_)

       ! ion-neutral collisions, velocity
       finTot_I(1:nIonFluid) = 0.
       do iNeuFluid=1,nNeuFluid
          finTot_I(1:nIonFluid) = finTot_I(1:nIonFluid) + &
               fin_IIC(1:nIonFluid,iNeuFluid,i,j,k) * &
               nIon_IC(1:nIonFluid,i,j,k)*&
               MassIon_I(1:nIonFluid)*MassFluid_I(nFluid) / &
               (MassIon_I(1:nIonFluid)+MassFluid_I(nFluid))*&
               uIonNeu2_IIC(1:nIonFluid,iNeuFluid,i,j,k)
       end do

       SPTerm_IIC(6,1:nIonFluid,i,j,k) = 2./3.*finTot_I(1:nIonFluid) / &
            Si2No_V(UnitT_)*Si2No_V(UnitN_)*Si2No_V(UnitU_)**2

       ! ion-electron, temperature
       SPTerm_IIC(7,1:nIonFluid,i,j,k) = 2.*fie_IC(1:nIonFluid,i,j,k) / &
            Si2No_V(UnitT_)*nIon_IC(1:nIonFluid,i,j,k)*&
            cBoltzmann*(Te_C(i,j,k)-Ti_IC(1:nIonFluid,i,j,k)) * &
            Si2No_V(UnitEnergyDens_)

       ! ion-electron collisional exchange (due to Hall velocity)
       SPTerm_IIC(8,1:nIonFluid,i,j,k) = 2./3.*fie_IC(1:nIonFluid,i,j,k) / &
            Si2No_V(UnitT_)*cElectronMass*&
            nIon_IC(1:nIonFluid,i,j,k)*Si2No_V(UnitRho_)* &
            uIonElec2_IC(1:nIonFluid,i,j,k)*Si2No_V(UnitU_)**2

       ! photoionization
       vAdd_I = 0.
       do iNeuFluid=1,nNeuFluid
          vAdd_I(1:nIonFluid) = vAdd_I(1:nIonFluid) + &
               v_IIGB(iNeuFluid,1:nIonFluid,i,j,k,iBlock)*nNeu1_C(i,j,k)* &
               uIonNeu2_IIC(1:nIonFluid,iNeuFluid,i,j,k)
       end do

       SPTerm_IIC(9,1:nIonFluid,i,j,k) = 1./3.*vAdd_I(1:nIonFluid)   / &
            Si2No_V(UnitT_) * MassIon_I(1:nIonFluid)*Si2No_V(UnitN_)   &
            *Si2No_V(UnitU_)**2

       ! electron-impact
       veAdd_I = 0.
       do iNeuFluid=1,nNeuFluid
          veAdd_I(1:nIonFluid) = veAdd_I(1:nIonFluid) + &
               ve_IIGB(iNeuFluid,1:nIonFluid,i,j,k,iBlock)*nNeu1_C(i,j,k)* &
               uIonNeu2_IIC(1:nIonFluid,iNeuFluid,i,j,k)
       end do

       SPTerm_IIC(10,1:nIonFluid,i,j,k) = 1./3.*veAdd_I(1:nIonFluid) / &
            Si2No_V(UnitT_) * MassIon_I(1:nIonFluid)*Si2No_V(UnitN_)   &
            *Si2No_V(UnitU_)**2

       ! addition to individual fluid from charge exchange [1/(m*s^2)]
       kinAdd_I = 0.
       do iIonFluid=1,nIonFluid
          do jIonFluid=1,nIonFluid
             do iNeuFluid=1,nNeuFluid
                do jNeutral=1,nNeuFluid
                   kinAdd_I(jIonFluid) = kinAdd_I(jIonFluid) + &
                        nIon_IC(iIonFluid,i,j,k)*&
                        kin_IIIIC(iIonFluid,iNeuFluid,jNeutral,jIonFluid, &
                        i,j,k) * nNeu1_C(i,j,k)* &
                        uIonNeu2_IIC(jIonFluid,iNeuFluid,i,j,k)
                end do
             end do
          end do
       end do

       SPTerm_IIC(11,1:nIonFluid,i,j,k) = 1./3.*kinAdd_I(1:nIonFluid) &
            /Si2No_V(UnitT_)*MassIon_I(1:nIonFluid)*Si2No_V(UnitN_)   &
            *Si2No_V(UnitU_)**2

       if (UseElectronPressure) then
          ! lost electrons through recombination
          SPeTerm_IC(1,i,j,k) = -sum( &
               Alpha_IC(1:nIonFluid,i,j,k)*nIon_IC(1:nIonFluid,i,j,k)) / &
               Si2No_V(UnitT_)*State_VGB(Pe_,i,j,k,iBlock)

          vAdd_I(1:nIonFluid) = 0.
          do iNeuFluid=1,nNeuFluid
             vAdd_I(1:nIonFluid) = vAdd_I(1:nIonFluid) + &
                  v_IIGB(iNeuFluid,1:nIonFluid,i,j,k,iBlock)*nNeu1_C(i,j,k)* &
                  uNeuElec2_IC(iNeuFluid,i,j,k)
          end do

          ! new electrons through photoionized neutrals
          SPeTerm_IC(2,i,j,k) = 1./3.*cElectronMass*sum(vAdd_I) * &
               Si2No_V(UnitRho_)/Si2No_V(UnitT_)*Si2No_V(UnitU_)**2

          veAdd_I(1:nIonFluid) = 0.
          do iNeuFluid=1,nNeuFluid
             veAdd_I(1:nIonFluid) = veAdd_I(1:nIonFluid) + &
                  ve_IIGB(iNeuFluid,1:nIonFluid,i,j,k,iBlock)*nNeu1_C(i,j,k)* &
                  uNeuElec2_IC(iNeuFluid,i,j,k)
          end do

          ! new electrons through electron-impact
          SPeTerm_IC(3,i,j,k) = 1./3.*cElectronMass*sum(veAdd_I) * &
               Si2No_V(UnitRho_)/Si2No_V(UnitT_)*Si2No_V(UnitU_)**2

          feiTot = 0.
          do iIonFluid=1,nIonFluid
             feiTot = feiTot+fei_IC(iIonFluid,i,j,k)/MassIon_I(iIonFluid)*&
                  (Ti_IC(iIonFluid,i,j,k)-Te_C(i,j,k))
          end do

          ! ion-electron collisional exchange (thermal motion)
          SPeTerm_IC(4,i,j,k) = 2.*cElectronMass* &
               Si2No_V(UnitRho_)/Si2No_V(UnitN_)*&
               nElec_C(i,j,k)*cBoltzmann*Si2No_V(UnitEnergyDens_) * &
               feiTot/Si2No_V(UnitT_)

          fenTot = 0.
          do iNeuFluid=1,nNeuFluid
             fenTot = fenTot + &
                  fen_IC(iNeuFluid,i,j,k)/MassFluid_I(nFluid)/cProtonMass*&
                  (TempNeu1_C(i,j,k)-Te_C(i,j,k))
          end do

          ! electron-neutral collisional exchange (thermal motion)
          SPeTerm_IC(5,i,j,k) = 2.*cElectronMass*nElec_C(i,j,k)*cBoltzmann*&
               Si2No_V(UnitEnergyDens_)*fenTot/Si2No_V(UnitT_)

          ! ion-electron collisional exchange (due to Hall velocity)
          SPeTerm_IC(6,i,j,k) = 2./3. * sum( &
               fei_IC(1:nIonFluid,i,j,k)*uIonElec2_IC(1:nIonFluid,i,j,k) ) / &
               Si2No_V(UnitT_)*cElectronMass*nElec_C(i,j,k) * &
               Si2No_V(UnitRho_)*Si2No_V(UnitU_)**2

          ! electron-neutral collisional exchange (bulk motion)
          SPeTerm_IC(7,i,j,k) = 2./3.*sum( &
               fen_IC(1:nNeuFluid,i,j,k)*uNeuElec2_IC(1:nNeuFluid,i,j,k)) / &
               Si2No_V(UnitT_)*cElectronMass*nElec_C(i,j,k) * &
               Si2No_V(UnitRho_)*Si2No_V(UnitU_)**2

          vAdd_I(1:nIonFluid) = 0.
          do iNeuFluid=1,nNeuFluid
             vAdd_I(1:nIonFluid) = vAdd_I(1:nIonFluid) + &
                  (v_IIGB(iNeuFluid,1:nIonFluid,i,j,k,iBlock) * &
                  Qexc_II(iNeuFluid,1:nIonFluid) - &
                  ve_IIGB(iNeuFluid,1:nIonFluid,i,j,k,iBlock) * &
                  Qion_II(iNeuFluid,1:nIonFluid))*ChargeIon_I(1:nIonFluid) * &
                  nNeu1_C(i,j,k)
          end do

          ! heating of electrons due to ionization excess energy
          SPeTerm_IC(8,i,j,k) = 2./3.*sum(vAdd_I) * &
               Si2No_V(UnitEnergyDens_)/Si2No_V(UnitT_)

          ! electron cooling due to collisions w/ water vapor

          ! logTe = log(Te_C(i,j,k))
          ! SPeTerm_IC(8,i,j,k) = &
          !     exp(-188.4701+33.2547*logTe-2.0792*logTe**2+0.0425*logTe**3)
          !
          ! if(Te_C(i,j,k)<1.5*TempNeu1_C(i,j,k)) then
          !   SPeTerm_IC(8,i,j,k)=4.5e-9/(0.5*TempNeu1_C(i,j,k))* &
          !        (Te_C(i,j,k)-TempNeu1_C(i,j,k))
          ! else
          !   SPeTerm_IC(8,i,j,k)=SPeTerm_IC(8,i,j,k)+4.5e-9
          ! end if

          SPeTerm_IC(9,i,j,k) = 4e-9* &
               (1-exp(-cBoltzmann*(Te_C(i,j,k)-TempNeu1_C(i,j,k)) &
               /0.033/cEV))

          if (Te_C(i,j,k) > 2181.65) then
             SPeTerm_IC(9,i,j,k) = SPeTerm_IC(9,i,j,k) + &
                  6.5e-9*(0.415-exp(-(cBoltzmann*Te_C(i,j,k)-0.1*cEV)/&
                  (0.1*cEV)))
          end if

          SPeTerm_IC(9,i,j,k) = min(-2./3.*nNeu1_C(i,j,k)*nElec_C(i,j,k)* &
               SPeTerm_IC(9,i,j,k)/1e6*cEV* &
               Si2No_V(UnitEnergyDens_)/Si2No_V(UnitT_), 0.0)

          ! SPeTerm_IC(8,i,j,k) = -2./3.*nNeu1_C(i,j,k)*nElec_C(i,j,k)* &
          !     SPeTerm_IC(8,i,j,k)/1e6*1.60217733e-19* &
          !     Si2No_V(UnitEnergyDens_)/Si2No_V(UnitT_)

          If (IsWithinTheRingR_III(i,j,k) == 1.0    .and. &
               tSimulation >  TimeEnhanceStart  .and. &
               tSimulation < TimeEnhanceEnd) then
             SPeTerm_IC(10,i,j,k) =  SPeAdditional_GB(i,j,k,iBlock)
          end If

       end if

       ! sum up individual terms
       do iTerm=1,nRhoTerm
          SRho_IC(1:nIonFluid,i,j,k) = SRho_IC(1:nIonFluid,i,j,k) + &
               SRhoTerm_IIC(iTerm,1:nIonFluid,i,j,k)
       end do

       do iTerm=1,nRhoUTerm
          SRhoUx_IC(1:nIonFluid,i,j,k) = SRhoUx_IC(1:nIonFluid,i,j,k) + &
               SRhoUxTerm_IIC(iTerm,1:nIonFluid,i,j,k)
          SRhoUy_IC(1:nIonFluid,i,j,k) = SRhoUy_IC(1:nIonFluid,i,j,k) + &
               SRhoUyTerm_IIC(iTerm,1:nIonFluid,i,j,k)
          SRhoUz_IC(1:nIonFluid,i,j,k) = SRhoUz_IC(1:nIonFluid,i,j,k) + &
               SRhoUzTerm_IIC(iTerm,1:nIonFluid,i,j,k)
       end do

       do iTerm=1,nPTerm
          SP_IC(1:nIonFluid,i,j,k) = SP_IC(1:nIonFluid,i,j,k) + &
               SPTerm_IIC(iTerm,1:nIonFluid,i,j,k)
       end do

       if(UseElectronPressure) then
          SPe_C(i,j,k) = sum(SPeTerm_IC(1:nPeTerm,i,j,k))
       end if

       Source_VC(iRhoIon_I   ,i,j,k) = SRho_IC(1:nIonFluid,i,j,k)    + &
            Source_VC(iRhoIon_I   ,i,j,k)
       Source_VC(iRhoUxIon_I ,i,j,k) = SRhoUx_IC(1:nIonFluid,i,j,k)  + &
            Source_VC(iRhoUxIon_I ,i,j,k)
       Source_VC(iRhoUyIon_I ,i,j,k) = SRhoUy_IC(1:nIonFluid,i,j,k)  + &
            Source_VC(iRhoUyIon_I ,i,j,k)
       Source_VC(iRhoUzIon_I ,i,j,k) = SRhoUz_IC(1:nIonFluid,i,j,k)  + &
            Source_VC(iRhoUzIon_I ,i,j,k)
       Source_VC(iPIon_I     ,i,j,k) = SP_IC(1:nIonFluid,i,j,k)      + &
            Source_VC(iPIon_I     ,i,j,k)

       Source_VC(Bx_    ,i,j,k) = SBx_C(i,j,k)                       + &
            Source_VC(Bx_    ,i,j,k)
       Source_VC(By_    ,i,j,k) = SBy_C(i,j,k)                       + &
            Source_VC(By_    ,i,j,k)
       Source_VC(Bz_    ,i,j,k) = SBz_C(i,j,k)                       + &
            Source_VC(Bz_    ,i,j,k)

       if(UseElectronPressure) then
          Source_VC(Pe_    ,i,j,k) = SPe_C(i,j,k)                    + &
               Source_VC(Pe_    ,i,j,k)
       end if

       if (DoSaveSource) call save_user_source
    end do;  end do;  end do

    if (DoCalcShading) then
       call put_block_data(iBlock,nI,nJ,nK,IsIntersectedShapeR_III)
       DoCalcShading = .false.
    end if
    if (DoCalcDistance2Fieldline) then
       call put_block_data(iBlock,nI,nJ,nK,IsWithinTheRingR_III)
       DoCalcDistance2Fieldline = .false.
    end if

    if(DoTest) call print_test

    call test_stop(NameSub, DoTest, iBlock)
  contains
    !==========================================================================
    subroutine save_user_source
      !------------------------------------------------------------------------
      select case(NameSource)
      case('H2OpRho')
         do iTerm = 1, nRhoTerm
            TestArray_IGB(iTerm,i,j,k,iBlock) =  &
                 SRhoTerm_IIC(iTerm,H2Op_,i,j,k) &
                 *No2SI_V(UnitRho_)/No2SI_V(UnitT_)
         end do
      case('H2OpUx')
         do iTerm = 1, nRhoUTerm
            TestArray_IGB(iTerm,i,j,k,iBlock) =    &
                 SRhoUxTerm_IIC(iTerm,H2Op_,i,j,k) &
                 *No2SI_V(UnitRhoU_)/No2SI_V(UnitT_)
         end do
      case('H2OpUy')
         do iTerm = 1, nRhoUTerm
            TestArray_IGB(iTerm,i,j,k,iBlock) =    &
                 SRhoUyTerm_IIC(iTerm,H2Op_,i,j,k) &
                 *No2SI_V(UnitRhoU_)/No2SI_V(UnitT_)
         end do
      case('H2OpUz')
         do iTerm = 1, nRhoUTerm
            TestArray_IGB(iTerm,i,j,k,iBlock) =    &
                 SRhoUzTerm_IIC(iTerm,H2Op_,i,j,k) &
                 *No2SI_V(UnitRhoU_)/No2SI_V(UnitT_)
         end do
      case('H2OpP')
         do iTerm = 1, nPTerm
            TestArray_IGB(iTerm,i,j,k,iBlock) = &
                 SPTerm_IIC(iTerm,H2Op_,i,j,k)  &
                 *No2SI_V(UnitP_)/No2SI_V(UnitT_)
         end do
      case('SwRho')
         do iTerm = 1, nRhoTerm
            TestArray_IGB(iTerm,i,j,k,iBlock) = &
                 SRhoTerm_IIC(iTerm,Sw_,i,j,k)  &
                 *No2SI_V(UnitRho_)/No2SI_V(UnitT_)
         end do
      case('SwUx')
         do iTerm = 1, nRhoUTerm
            TestArray_IGB(iTerm,i,j,k,iBlock) =  &
                 SRhoUxTerm_IIC(iTerm,Sw_,i,j,k) &
                 *No2SI_V(UnitRhoU_)/No2SI_V(UnitT_)
         end do
      case('SwUy')
         do iTerm = 1, nRhoUTerm
            TestArray_IGB(iTerm,i,j,k,iBlock) =  &
                 SRhoUyTerm_IIC(iTerm,Sw_,i,j,k) &
                 *No2SI_V(UnitRhoU_)/No2SI_V(UnitT_)
         end do
      case('SwUz')
         do iTerm = 1, nRhoUTerm
            TestArray_IGB(iTerm,i,j,k,iBlock) =  &
                 SRhoUzTerm_IIC(iTerm,Sw_,i,j,k) &
                 *No2SI_V(UnitRhoU_)/No2SI_V(UnitT_)
         end do
      case('SwP')
         do iTerm = 1, nPTerm
            TestArray_IGB(iTerm,i,j,k,iBlock) = &
                 SPTerm_IIC(iTerm,Sw_,i,j,k)    &
                 *No2SI_V(UnitP_)/No2SI_V(UnitT_)
         end do

      case('Pe')
         do iTerm = 1, nPeTerm
            TestArray_IGB(iTerm,i,j,k,iBlock) = &
                 SPeTerm_IC(iTerm,i,j,k)        &
                 *No2SI_V(UnitP_)/No2SI_V(UnitT_)
         end do
      case default
         if(iProc==0) call stop_mpi( &
              NameSub//' invalid NameSource='//trim(NameSource))
      end select
    end subroutine save_user_source
    !==========================================================================
    subroutine print_test
      character(len=100) :: TestFmt1, TestFmt2, TestFmt3

      !------------------------------------------------------------------------
      i=iTest;j=jTest;k=kTest

      !---------------------------------------------------------------------
      TestFmt1 = '(a, 100es22.15)'
      TestFmt2 = '(a, 2es22.15, 2x, f7.2)'
      TestFmt3 = '(a, i2, a, 2es22.15, 2x, f7.2)'
      write(*,*) '========================================================'
      write(*,*) 'Neutral background:'
      write(*,TestFmt1) '  n, u_D, T (SI)  =', &
           nNeu1_C(i,j,k), uNeu1_DC(:,i,j,k),  &
           TempNeu1_C(i,j,k)
      write(*,TestFmt1) '  n, u_D, T (NO)  =', &
           nNeu1_C(i,j,k)   *Si2No_V(UnitN_),  &
           uNeu1_DC(:,i,j,k)*Si2No_V(UnitU_),  &
           TempNeu1_C(i,j,k)*Si2No_V(UnitTemperature_)
      write(*,*) '-----------------------------------------------------'
      write(*,'(a,i2)') ' Ion information: nIonFluid =', nIonFluid
      write(*,*) ' MassIon_I   =',  MassIon_I
      write(*,*) ' ChargeIon_I =',ChargeIon_I
      do iIonFluid = 1, nIonFluid
         write(*,'(a,i1,100es22.15)')                        &
              '  iFluid, n, u_D, T  (SI) = ', iIonFluid,    &
              nIon_IC(iIonFluid,i,j,k),         &
              uIon_DIC(:,iIonFluid,i,j,k),      &
              Ti_IC(iIonFluid,i,j,k)
         write(*,'(a,i1,100es22.15)') &
              '  iFluid, n, u_D, T  (NO) = ', iIonFluid,    &
              nIon_IC(iIonFluid,i,j,k)    *Si2NO_V(UnitN_), &
              uIon_DIC(:,iIonFluid,i,j,k) *Si2NO_V(UnitU_), &
              Ti_IC(iIonFluid,i,j,k)      *Si2NO_V(UnitTemperature_)
      end do
      write(*,*) '-----------------------------------------------------'
      write(*,'(a,i2)') ' Electron information:'
      write(*,'(a,100es22.15)') &
           '  Te  (SI) = ', Te_C(i,j,k)
      write(*,'(a,100es22.15)') &
           '  Te  (SI) = ', Te_C(i,j,k)*Si2NO_V(UnitTemperature_)

      do iIonFluid = 1, nIonFluid
         write(*,*) '-----------------------------------------------------'
         write(*,'(a4,i2,a)') ' Ion ', iIonFluid, ':'
         write(*,'(a, f7.2)') ' MassIon_I(iIonFluid) =', &
              MassIon_I(iIonFluid)
         write(*,TestFmt1) ' vIonizationSi  =', &
              v_IIGB(1,iIonFluid,i,j,k,iBlockTest)
         write(*,TestFmt1) ' veSi_III       =', &
              ve_IIGB(1,iIonFluid,i,j,k,iBlockTest)
         write(*,TestFmt1) ' kinSi_IIII     =', &
              kin_IIIIC(iIonFluid,1,1,:,i,j,k)
         write(*,TestFmt1) ' finSi_II       =', &
              fin_IIC(iIonFluid, 1, i,j,k)
         write(*,TestFmt1) ' fiiSi_II       =', &
              fii_IIC(iIonFluid, :, i,j,k)
         write(*,TestFmt1) ' fieSi_I        =', &
              fie_IC(iIonFluid, i,j,k)
         write(*,TestFmt1) ' AlphaSi_II     =', &
              Alpha_IC(iIonFluid,i,j,k)
         write(*,*) '*****************************************************'
         write(*,TestFmt2) ' Source_VC  (NO, SI, rate)            =', &
              Source_VC(iRhoIon_I(iIonFluid),i,j,k),      &
              Source_VC(iRhoIon_I(iIonFluid),i,j,k)       &
              *No2SI_V(UnitRho_)/No2Si_V(UnitT_),         &
              Source_VC(iRhoIon_I(iIonFluid),i,j,k)*100   &
              /State_VGB(iRhoIon_I(iIonFluid),i,j,k,iBlock)
         write(*,TestFmt2) ' SRho_IC  (NO, SI, rate)              =', &
              SRho_IC(iIonFluid,i,j,k),             &
              SRho_IC(iIonFluid,i,j,k)              &
              *No2SI_V(UnitRho_)/No2Si_V(UnitT_),   &
              SRho_IC(iIonFluid,i,j,k)*100          &
              /State_VGB(iRhoIon_I(iIonFluid),i,j,k,iBlock)
         do iTerm =1,nRhoTerm
            write(*,TestFmt3) &
                 '  SRhoTerms_II(',iTerm, ')   (NO, SI, rate)   =', &
                 SRhoTerm_IIC(iTerm,iIonFluid,i,j,k),   &
                 SRhoTerm_IIC(iTerm,iIonFluid,i,j,k)    &
                 *No2SI_V(UnitRho_)/No2Si_V(UnitT_),    &
                 SRhoTerm_IIC(iTerm,iIonFluid,i,j,k)    &
                 /State_VGB(iRhoIon_I(iIonFluid),i,j,k,iBlock)*100
         end do
         write(*,*) '*****************************************************'
         write(*,'(a,i1)') ' iDir = ', 1
         write(*,TestFmt2) ' Source_VC  (NO, SI, rate)              =', &
              Source_VC(iRhoUxIon_I(iIonFluid),i,j,k),    &
              Source_VC(iRhoUxIon_I(iIonFluid),i,j,k)     &
              *No2SI_V(UnitRhoU_)/No2Si_V(UnitT_),        &
              Source_VC(iRhoUxIon_I(iIonFluid),i,j,k)*100 &
              /max(abs(State_VGB(iRhoUxIon_I(iIonFluid),i,j,k,iBlock)),cTiny)
         write(*,TestFmt2) ' SRhoU_IC  (NO, SI, rate)               =', &
              SRhoUx_IC(iIonFluid,i,j,k),             &
              SRhoUx_IC(iIonFluid,i,j,k)              &
              *No2SI_V(UnitRhoU_)/No2Si_V(UnitT_),    &
              SRhoUx_IC(iIonFluid,i,j,k)*100          &
              /max(abs(State_VGB(iRhoUxIon_I(iIonFluid),i,j,k,iBlock)),cTiny)
         do iTerm = 1,nRhoUTerm
            write(*,TestFmt3) &
                 '  SRhoUTerms_IID(',iTerm, ')   (NO, SI, rate)   =',  &
                 SRhoUxTerm_IIC(iTerm,iIonFluid,i,j,k),    &
                 SRhoUxTerm_IIC(iTerm,iIonFluid,i,j,k)     &
                 *No2SI_V(UnitRhoU_)/No2Si_V(UnitT_),      &
                 SRhoUxTerm_IIC(iTerm,iIonFluid,i,j,k)*100 &
                 /max(cTiny, &
                 abs(State_VGB(iRhoUxIon_I(iIonFluid),i,j,k,iBlock)))
         end do
         write(*,'(a,i1)') ' iDir = ', 2
         write(*,TestFmt2) ' Source_VC  (NO, SI, rate)              =', &
              Source_VC(iRhoUyIon_I(iIonFluid),i,j,k),    &
              Source_VC(iRhoUyIon_I(iIonFluid),i,j,k)     &
              *No2SI_V(UnitRhoU_)/No2Si_V(UnitT_),        &
              Source_VC(iRhoUyIon_I(iIonFluid),i,j,k)*100 &
              /max(abs(State_VGB(iRhoUyIon_I(iIonFluid),i,j,k,iBlock)),cTiny)
         write(*,TestFmt2) ' SRhoU_IC  (NO, SI, rate)               =', &
              SRhoUy_IC(iIonFluid,i,j,k),            &
              SRhoUy_IC(iIonFluid,i,j,k)             &
              *No2SI_V(UnitRhoU_)/No2Si_V(UnitT_),   &
              SRhoUy_IC(iIonFluid,i,j,k)*100         &
              /max(abs(State_VGB(iRhoUyIon_I(iIonFluid),i,j,k,iBlock)),cTiny)
         do iTerm = 1,nRhoUTerm
            write(*,TestFmt3) &
                 '  SRhoUTerms_IID(',iTerm, ')   (NO, SI, rate)   =',  &
                 SRhoUyTerm_IIC(iTerm,iIonFluid,i,j,k),                &
                 SRhoUyTerm_IIC(iTerm,iIonFluid,i,j,k)                 &
                 *No2SI_V(UnitRhoU_)/No2Si_V(UnitT_),                  &
                 SRhoUyTerm_IIC(iTerm,iIonFluid,i,j,k)*100             &
                 /max(cTiny, &
                 abs(State_VGB(iRhoUyIon_I(iIonFluid),i,j,k,iBlock)))
         end do
         write(*,'(a,i1)') ' iDir = ', 3
         write(*,TestFmt2) ' Source_VC  (NO, SI, rate)              =', &
              Source_VC(iRhoUzIon_I(iIonFluid),i,j,k),    &
              Source_VC(iRhoUzIon_I(iIonFluid),i,j,k)     &
              *No2SI_V(UnitRhoU_)/No2Si_V(UnitT_),        &
              Source_VC(iRhoUzIon_I(iIonFluid),i,j,k)*100 &
              /max(abs(State_VGB(iRhoUzIon_I(iIonFluid),i,j,k,iBlock)),cTiny)
         write(*,TestFmt2) ' SRhoU_IC  (NO, SI, rate)               =', &
              SRhoUz_IC(iIonFluid,i,j,k),           &
              SRhoUz_IC(iIonFluid,i,j,k)            &
              *No2SI_V(UnitRhoU_)/No2Si_V(UnitT_),  &
              SRhoUz_IC(iIonFluid,i,j,k)*100        &
              /max(abs(State_VGB(iRhoUzIon_I(iIonFluid),i,j,k,iBlock)),cTiny)
         do iTerm = 1,nRhoUTerm
            write(*,TestFmt3) &
                 '  SRhoUTerms_IID(',iTerm, ')   (NO, SI, rate)   =',  &
                 SRhoUzTerm_IIC(iTerm,iIonFluid,i,j,k),                &
                 SRhoUzTerm_IIC(iTerm,iIonFluid,i,j,k)                 &
                 *No2SI_V(UnitRhoU_)/No2Si_V(UnitT_),                  &
                 SRhoUzTerm_IIC(iTerm,iIonFluid,i,j,k)*100             &
                 /max(cTIny, &
                 abs(State_VGB(iRhoUzIon_I(iIonFluid),i,j,k,iBlock)))
         end do
         write(*,*) '*****************************************************'
         write(*,TestFmt2) ' Source_VC  (NO, SI, rate)          =',  &
              Source_VC(iPIon_I(iIonFluid),i,j,k),       &
              Source_VC(iPIon_I(iIonFluid),i,j,k)        &
              *No2SI_V(UnitEnergyDens_)/No2Si_V(UnitT_), &
              Source_VC(iPIon_I(iIonFluid),i,j,k)*100    &
              /max(abs(State_VGB(iPIon_I(iIonFluid),i,j,k,iBlock)),cTiny)
         write(*,TestFmt2) ' SP_IC  (NO, SI, rate)              =',  &
              SP_IC(iIonFluid,i,j,k),                    &
              SP_IC(iIonFluid,i,j,k)                     &
              *No2SI_V(UnitEnergyDens_)/No2Si_V(UnitT_), &
              SP_IC(iIonFluid,i,j,k)*100                 &
              /max(abs(State_VGB(iPIon_I(iIonFluid),i,j,k,iBlock)),cTiny)
         do iTerm = 1,nPTerm
            write(*,TestFmt3) &
                 '  SPTerms_II(',iTerm, ')   (NO, SI, rate)   =',  &
                 SPTerm_IIC(iTerm,iIonFluid,i,j,k),                &
                 SPTerm_IIC(iTerm,iIonFluid,i,j,k)                 &
                 *No2SI_V(UnitEnergyDens_)/No2Si_V(UnitT_),        &
                 SPTerm_IIC(iTerm,iIonFluid,i,j,k)*100             &
                 /max(abs(State_VGB(iPIon_I(iIonFluid),i,j,k,iBlock)),cTiny)
         end do
      end do

      write(*,*) '-----------------------------------------------------'
      write(*,'(a10,i2,a)') ' Electron:'
      write(*,TestFmt1) ' fenSi_II       =', fen_IC(1, i,j,k)
      write(*,TestFmt1) ' feiSi_II       =', fei_IC(:, i,j,k)
      write(*,TestFmt1) ' AlphaSi_II     =', Alpha_IC(:,i,j,k)
      write(*,TestFmt2) ' Source_VC  (NO, SI, rate)           =', &
           Source_VC(Pe_,i,j,k),                      &
           Source_VC(Pe_,i,j,k)                       &
           *No2SI_V(UnitEnergyDens_)/No2Si_V(UnitT_), &
           Source_VC(Pe_,i,j,k)*100                   &
           /max(abs(State_VGB(Pe_,i,j,k,iBlock)),cTiny)
      write(*,TestFmt2) ' SPe_IC  (NO, SI, rate)              =', &
           SPe_C(i,j,k),                              &
           SPe_C(i,j,k)                               &
           *No2SI_V(UnitEnergyDens_)/No2Si_V(UnitT_), &
           SPe_C(i,j,k)*100                           &
           /max(abs(State_VGB(Pe_,i,j,k,iBlock)),cTiny)
      do iTerm = 1,nPeTerm
         write(*,TestFmt3) &
              '  SPeTerms_II(',iTerm, ')   (NO, SI, rate)   =', &
              SPeTerm_IC(iTerm,i,j,k),                          &
              SPeTerm_IC(iTerm,i,j,k)                           &
              *No2SI_V(UnitEnergyDens_)/No2Si_V(UnitT_),        &
              SPeTerm_IC(iTerm,i,j,k)*100                       &
              /max(abs(State_VGB(Pe_,i,j,k,iBlock)),cTiny)
      end do
      write(*,*) '========================================================'
    end subroutine print_test
    !==========================================================================
  end subroutine user_calc_sources_impl
  !============================================================================
  subroutine user_calc_sources_expl(iBlock)

    integer, intent(in) :: iBlock
    !--------------------------------------------------------------------------
  end subroutine user_calc_sources_expl
  !============================================================================
  subroutine user_update_states(iBlock)

    use ModUpdateState, ONLY: update_state_normal
    use ModAdvance, ONLY: State_VGB
    use ModPhysics, ONLY: LowDensityRatio, &
         Si2No_V, No2Si_V, UnitN_, UnitTemperature_, UnitX_, UnitU_
    use ModGeometry, ONLY: r_GB
    use BATL_lib, ONLY: Used_GB, Xyz_DGB

    integer,intent(in) :: iBlock

    integer :: i,j,k
    real    :: Ux, Uy, Uz, Ti

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_update_states'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    call update_state_normal(iBlock)

    if (DoTest) &
         write(*,*) NameSub, ' before user term =', &
         State_VGB(iVarTest, iTest, jTest, kTest, iBlock)

    do k=1,nK; do j=1,nJ; do i=1,nI
       if (.not. Used_GB(i,j,k,iBlock)) CYCLE

       if (DoUseHaserBackground) then
          if (r_GB(i,j,k,iBlock) > rSphericalBody) then
             State_VGB(Neu1Rho_,i,j,k,iBlock) = Qprod/                        &
                  (4.*cPi*(r_GB(i,j,k,iBlock)*No2Si_V(UnitX_))**2*uHaser ) * &
                  exp(-vHI*r_GB(i,j,k,iBlock)*No2Si_V(UnitX_)/uHaser)*       &
                  Si2No_V(UnitN_) * MassFluid_I(nFluid)
             State_VGB(Neu1Ux_:Neu1Uz_,i,j,k,iBlock) =                     &
                  State_VGB(Neu1Rho_,i,j,k,iBlock)*uHaser*Si2No_V(UnitU_)* &
                  Xyz_DGB(:,i,j,k,iBlock)/r_GB(i,j,k,iBlock)
             State_VGB(Neu1P_,i,j,k,iBlock)     =                          &
                  State_VGB(Neu1Rho_,i,j,k,iBlock)/MassFluid_I(nFluid)     &
                  *TempNeuMinSi*Si2No_V(UnitTemperature_)
          else
             State_VGB(Neu1Rho_,i,j,k,iBlock) = Qprod/                  &
                  (4.*cPi*rSphericalBodySi**2*uHaser )*                 &
                  exp(-vHI*rSphericalBodySi/uHaser)*                    &
                  Si2No_V(UnitN_) * MassFluid_I(nFluid)
             State_VGB(Neu1Ux_:Neu1Uz_,i,j,k,iBlock) = 0.0
             State_VGB(Neu1P_,i,j,k,iBlock)     =                       &
                  State_VGB(Neu1Rho_,i,j,k,iBlock)/MassFluid_I(nFluid)  &
                  *TempNeuMinSi*Si2No_V(UnitTemperature_)
          end if
       else if (DoUseUniformNeuBackground) then
          State_VGB(Neu1Rho_,i,j,k,iBlock) = nNeuUniform*MassFluid_I(nFluid)
          State_VGB(Neu1Ux_, i,j,k,iBlock) = UxNeuUniform
          State_VGB(Neu1Uy_, i,j,k,iBlock) = UyNeuUniform
          State_VGB(Neu1Uz_, i,j,k,iBlock) = UzNeuUniform
          State_VGB(Neu1P_,  i,j,k,iBlock) = pNeuUniform
       end if

       ! If the cometary ion fluid mass density is less than the solar wind
       ! density * LowDensityRatio, then set the cometary ion fluid mass
       ! density is solar wind density * LowDensityRatio,
       ! and its velocity/temperature the same as the solar wind
       if(State_VGB(H2OpRho_,i,j,k,iBlock) < &
            State_VGB(Rho_,i,j,k,iBlock)*LowDensityRatio) then

          ! Velocity of the solar wind
          Ux = State_VGB(RhoUx_,i,j,k,iBlock)/State_VGB(Rho_,i,j,k,iBlock)
          Uy = State_VGB(RhoUy_,i,j,k,iBlock)/State_VGB(Rho_,i,j,k,iBlock)
          Uz = State_VGB(RhoUz_,i,j,k,iBlock)/State_VGB(Rho_,i,j,k,iBlock)

          ! Temperature of the solar wind
          Ti = State_VGB(P_,i,j,k,iBlock) / &
               State_VGB(Rho_,i,j,k,iBlock)*MassIon_I(Sw_)

          State_VGB(H2OpRho_,i,j,k,iBlock) = &
               State_VGB(Rho_,i,j,k,iBlock)*LowDensityRatio

          State_VGB(H2OpRhoUx_,i,j,k,iBlock) =  &
               State_VGB(H2OpRho_,i,j,k,iBlock) * Ux
          State_VGB(H2OpRhoUy_,i,j,k,iBlock) =  &
               State_VGB(H2OpRho_,i,j,k,iBlock) * Uy
          State_VGB(H2OpRhoUz_,i,j,k,iBlock) =  &
               State_VGB(H2OpRho_,i,j,k,iBlock) * Uz

          State_VGB(H2OpP_,i,j,k,iBlock) =  &
               State_VGB(H2OpRho_,i,j,k,iBlock)/MassIon_I(H2Op_)*Ti
       end if

       ! If the solar wind density is less than 1e-4, then set solar wind
       ! density to 1e-4, and its velocity/temperature the same as the
       ! cometary ion fluid
       if(State_VGB(Rho_,i,j,k,iBlock) < &
            1.0e-4*Si2No_V(UnitN_)*MassIon_I(Sw_)) then

          ! Velocity of the cometary ion
          Ux = State_VGB(H2OpRhoUx_,i,j,k,iBlock) / &
               State_VGB(H2OpRho_,  i,j,k,iBlock)
          Uy = State_VGB(H2OpRhoUy_,i,j,k,iBlock) / &
               State_VGB(H2OpRho_,  i,j,k,iBlock)
          Uz = State_VGB(H2OpRhoUz_,i,j,k,iBlock) / &
               State_VGB(H2OpRho_,  i,j,k,iBlock)

          ! Temperature of the cometary ion
          Ti = State_VGB(H2OpP_,  i,j,k,iBlock) / &
               State_VGB(H2OpRho_,i,j,k,iBlock)*MassIon_I(H2Op_)

          State_VGB(Rho_,i,j,k,iBlock) = &
               1.0e-4*Si2No_V(UnitN_)*MassIon_I(Sw_)

          State_VGB(RhoUx_,i,j,k,iBlock) = State_VGB(Rho_,i,j,k,iBlock)*Ux
          State_VGB(RhoUy_,i,j,k,iBlock) = State_VGB(Rho_,i,j,k,iBlock)*Uy
          State_VGB(RhoUz_,i,j,k,iBlock) = State_VGB(Rho_,i,j,k,iBlock)*Uz

          State_VGB(P_,i,j,k,iBlock) =  &
               State_VGB(Rho_,i,j,k,iBlock)/MassIon_I(Sw_)*Ti
       end if

    end do; end do; end do   ! do k=1,nK; do j=1,nJ; do i=1,nI

    if (DoTest) &
         write(*,*) NameSub, ' after user term =', &
         State_VGB(iVarTest, iTest, jTest, kTest, iBlock)

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_update_states
  !============================================================================
  subroutine derive_cell_diffusivity( &
       iBlock, i, j, k, TeSI, nIon_I, nElec, EtaSi)
    use ModResistivity, ONLY: Eta0SI
    use ModConst, ONLY: cElectronMass, cElectronCharge, cMu

    integer, intent(in)  :: iBlock, i, j, k
    real,    intent(in)  :: TeSI
    real,    intent(in)  :: nIon_I(1:nIonFluid)
    real,    intent(in)  :: nElec
    real,    intent(out) :: EtaSi

    real :: EtaSiColl!, EtaSiSpitzer, lnL
    !    real, save :: SpitzerCoef, EtaPerpSpitzerSi
    real :: eeSigma!, B0_D(3)
    real, dimension(nIonFluid):: fei_I, eiSigma_I
    real, dimension(nNeuFluid):: fen_I, enSigma_I
    integer :: iIonFluid, iNeuFluid

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'derive_cell_diffusivity'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(iBlock==iBlockTest.and.i==iTest.and.j==jTest.and.k==kTest &
         .and.iProc==iProcTest) then
    else
       DoTest=.false.; DoTest=.false.
    end if

    ! Spitzer formulation from Stoecker "Taschenbuch der Physik",
    ! Verlag "Harri Deutsch"
    ! lnL = log(1e7*TeSI**1.5/sqrt(nElec))
    ! EtaSiSpitzer = cElectronCharge**2*lnL/ &
    !  (32.*sqrt(2*cPi/cElectronMass*(cBoltzmann*TeSI)**3)*cEps**2)/cMu

    !! Collisional type resisitivity/diffusivity
    call calc_electron_collision_rates( &
         TeSI,nElec,i,j,k,iBlock,fen_I(1:nNeuFluid),fei_I(1:nIonFluid))

    eiSigma_I(1:nIonFluid) = &
         cElectronCharge**2*max(nElec, 1e-15)/ &
         ((fei_I(1:nIonFluid)+1E-20)*cElectronMass)
    enSigma_I(1:nNeuFluid) = &
         cElectronCharge**2*max(nElec, 1e-15)/ &
         ((fen_I(1:nNeuFluid)+1E-20)*cElectronMass)

    ! Eta_G is calculated from both conductivities using Kirchhoff's rule:
    ! 1/sigma_tot = 1/eiSigma_I+1/enSigma_I
    ! The resulting conductivity is close to Spitzer conductivity far from
    ! the comet and decreases due to abundant electron-neutral collisions
    ! close to the nucleus
    ! EtaSiColl = 1/(sigma_tot*mu_0) magnetic diffusivity [m^2/s]

    EtaSiColl = &
         (sum(1/eiSigma_I(1:nIonFluid))+sum(1/enSigma_I(1:nNeuFluid)))/cMu

    ! Total diffusivity [m^2/s]
    EtaSi = Eta0SI + EtaSiColl

    ! TestArray(1,i,j,k,iBlock) = EtaSiColl
    ! TestArray(2,i,j,k,iBlock) = EtaSiSpitzer
    ! TestArray(3,i,j,k,iBlock) = EtaSiSpitzer/EtaSiColl

    if(DoTest) then
       write(*,*)'derive_cell_diffusivity:'
       write(*,*)'n_e    = ',nElec," [1/m^3]"
       write(*,*)'Te     = ',TeSI," [K]"
       do iIonFluid=1,nIonFluid
          write(*,*)'e & ',NameFluid_I(iIonFluid+1),':'
          write(*,*)'s_ei  = ',eiSigma_I(iIonFluid)," [1/(Ohm*m)]"
       end do
       do iNeuFluid=1,nNeuFluid
          write(*,*)'e & ',NameNeutral_I(iNeuFluid),':'
          write(*,*)'s_en  = ',enSigma_I(iNeuFluid)," [1/(Ohm*m)]"
       end do
       write(*,*)'e & e:'
       write(*,*)'s_ee  = ',eeSigma," [1/(Ohm*m)]"
       write(*,*)''
       write(*,*)'Eta0   = ',Eta0Si," [m^2/s]"
       write(*,*)'Eta_en = ',sum(1/enSigma_I(1:nNeuFluid))/cMu," [m^2/s]"
       write(*,*)'Eta_ei = ',sum(1/eiSigma_I(1:nIonFluid))/cMu," [m^2/s]"
       write(*,*)'Eta_ee = ',1/eeSigma/cMu," [m^2/s]"
       write(*,*)'Eta_eX = ',EtaSiColl," [m^2/s]"
       write(*,*)'EtaTot = ',EtaSi," [m^2/s]"
       write(*,*)''
    end if

    call test_stop(NameSub, DoTest)
  end subroutine derive_cell_diffusivity
  !============================================================================
  subroutine user_set_resistivity(iBlock, Eta_G)

    use ModPhysics, ONLY: No2Si_V, Si2No_V, &
         UnitN_, UnitX_, UnitT_, UnitP_, ElectronPressureRatio
    use ModAdvance, ONLY: State_VGB
    use ModVarIndexes, ONLY: Pe_, P_
    use ModConst, ONLY: cBoltzmann
    use ModMultiFluid, ONLY: MassIon_I

    integer, intent(in) :: iBlock
    real, intent(out) :: Eta_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)

    integer :: i, j, k
    real:: nIon_IG(nIonFluid,MinI:MaxI,MinJ:MaxJ,MinK:MaxK)
    real, dimension(MinI:MaxI,MinJ:MaxJ,MinK:MaxK) :: Te_G, nElec_G
    real :: EtaSi

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_resistivity'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! nElec_G is the electron/ion density in SI units (n_e=n_itot)
    nElec_G = 0.
    do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
       nIon_IG(1:nIonFluid,i,j,k) = &
            State_VGB(iRhoIon_I,i,j,k,iBlock)/MassIon_I*NO2SI_V(UnitN_)
       nElec_G(i,j,k) = &
            sum(nIon_IG(1:nIonFluid,i,j,k)*ChargeIon_I(1:nIonFluid))
    end do; end do; end do

    if (UseElectronPressure) then
       Te_G = State_VGB(Pe_,:,:,:,iBlock)*NO2SI_V(UnitP_)/(cBoltzmann* &
            nElec_G)
    else
       Te_G(:,:,:) = State_VGB(P_,:,:,:,iBlock)* &
            ElectronPressureRatio/(1.+ElectronPressureRatio)*&
            NO2SI_V(UnitP_)/(cBoltzmann*nElec_G(:,:,:))
    end if

    do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
       call derive_cell_diffusivity( &
            iBlock, i, j, k, Te_G(i,j,k), &
            nIon_IG(1:nIonFluid,i,j,k), nElec_G(i,j,k), EtaSi)

       Eta_G(i,j,k) = EtaSi*SI2No_V(UnitX_)**2/SI2No_V(UnitT_)
    end do; end do; end do

    if(DoTest) then
       write(*,*)'user_set_resistivity:'
       write(*,*)'Te    = ',Te_G(iTest,jTest,kTest)," [K]"
       write(*,*)'n_e   = ',nElec_G(iTest,jTest,kTest)," [m^-3]"
       write(*,*)'Eta   = ', &
            Eta_G(iTest,jTest,kTest)*No2SI_V(UnitX_)**2/No2SI_V(UnitT_), &
            " [m^2/s]"
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_resistivity
  !============================================================================
  subroutine user_material_properties(State_V, i,j,k,iBlock,iDir, &
       EinternalIn, TeIn, NatomicOut, AverageIonChargeOut, &
       EinternalOut, TeOut, PressureOut,   &
       CvOut, GammaOut, HeatCondOut, IonHeatCondOut, TeTiRelaxOut, &
       OpacityPlanckOut_W, OpacityEmissionOut_W, OpacityRosselandOut_W, &
       PlanckOut_W, EntropyOut)

    use ModPhysics, ONLY: No2Si_V, Si2No_V, UnitP_, UnitN_, UnitX_, &
         ElectronPressureRatio, InvGammaElectronMinus1
    use ModVarIndexes, ONLY: nVar, p_
    use ModConst, ONLY: cElectronCharge, cBoltzmann, cMu, cElectronMass
    use ModAdvance, ONLY: State_VGB
    use ModGeometry, ONLY: Xyz_DGB

    ! The State_V vector is in normalized units
    real, intent(in) :: State_V(nVar)
    integer, optional, intent(in) :: i, j, k, iBlock, iDir
    real, optional, intent(in)  :: EinternalIn                  ! [J/m^3]
    real, optional, intent(in)  :: TeIn                         ! [K]
    real, optional, intent(out) :: NatomicOut                   ! [1/m^3]
    real, optional, intent(out) :: AverageIonChargeOut          ! dimensionless
    real, optional, intent(out) :: EinternalOut                 ! [J/m^3]
    real, optional, intent(out) :: TeOut                        ! [K]
    real, optional, intent(out) :: PressureOut                  ! [Pa]
    real, optional, intent(out) :: CvOut                        ! [J/(K*m^3)]
    real, optional, intent(out) :: GammaOut                     ! dimensionless
    real, optional, intent(out) :: HeatCondOut                  ! [W/(m*K)]
    real, optional, intent(out) :: IonHeatCondOut               ! [W/(m*K)]
    real, optional, intent(out) :: TeTiRelaxOut                 ! [1/s]
    real, optional, intent(out) :: OpacityPlanckOut_W(nWave)    ! [1/m]
    real, optional, intent(out) :: OpacityEmissionOut_W(nWave)  ! [1/m]
    real, optional, intent(out) :: OpacityRosselandOut_W(nWave) ! [1/m]
    real, optional, intent(out) :: PlanckOut_W(nWave)           ! [J/m^3]
    real, optional, intent(out) :: EntropyOut

    real, save :: KappaCoeffSI = (cBoltzmann/cElectronCharge)**2/cMu
    real :: nElec, EtaSI, TeSI
    real :: nIon_I(nIonFluid)

    real :: xmin, xmax, HeatCondFactor, widthmax, widthmin, xMaxyz, xMinyz

    ! OpenMP declaration
    !$omp threadprivate( KappaCoeffSI )

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_material_properties'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock, i, j, k)

    nIon_I(1:nIonFluid) = &
         State_VGB(iRhoIon_I,i,j,k,iBlock)/MassIon_I*NO2SI_V(UnitN_)
    nElec = sum(nIon_I(1:nIonFluid)*ChargeIon_I(1:nIonFluid))
    if (UseElectronPressure) then
       TeSI = State_VGB(Pe_,i,j,k,iBlock)*NO2SI_V(UnitP_)/(cBoltzmann*nElec)
    else
       TeSI = State_VGB(P_,i,j,k,iBlock)* &
            ElectronPressureRatio/(1.+ElectronPressureRatio)*&
            NO2SI_V(UnitP_)/(cBoltzmann*nElec)
    end if

    if(present(CvOut)) CvOut = cBoltzmann*nElec*InvGammaElectronMinus1
    if(present(TeOut)) TeOut = TeSI
    if(present(AverageIonChargeOut).or.present(NatomicOut)) &
         AverageIonChargeOut = nElec/sum(nIon_I(1:nIonFluid))
    if(present(NatomicOut)) NatomicOut = nElec/AverageIonChargeOut

    if(present(HeatCondOut)) then
       xmin =  75000e3*Si2No_V(UnitX_) ! cometopause 75'000 km
       xmax = 100000e3*Si2No_V(UnitX_) ! cometopause max
       widthmin = 2.*xmin              ! flaring ratio 2 (Cravens 1989)
       widthmax = 2.*xmax              ! flaring ratio 2 (Cravens 1989)

       xMaxyz = -(Xyz_DGB(y_,i,j,k,iBlock)**2+Xyz_DGB(z_,i,j,k,iBlock)**2) / &
            widthmax**2*xmax+xmax

       if(Xyz_DGB(x_,i,j,k,iBlock) > xMaxyz) then
          HeatCondFactor = 0.0
          HeatCondOut = 0.0
       else
          call derive_cell_diffusivity( &
               iBlock, i, j, k, TeSI, nIon_I(1:nIonFluid), nElec, EtaSi)

          xMinyz = -(Xyz_DGB(y_,i,j,k,iBlock)**2+Xyz_DGB(z_,i,j,k,iBlock)**2)/&
               widthmin**2*xmin+xmin
          if (Xyz_DGB(x_,i,j,k,iBlock) < xMinyz) then
             HeatCondFactor = 1.0
          else
             HeatCondFactor=(xMaxyz-Xyz_DGB(x_,i,j,k,iBlock))/(xMaxyz-xMinyz)
          end if
          HeatCondOut = TeSI/EtaSI*KappaCoeffSI*HeatCondFactor
       end if

    end if

    if(DoTest) then
       write(*,*)'user_material_properties:'
       write(*,*)'n_e    = ',nElec," [1/m^3]"
       write(*,*)'Te     = ',TeSI," [K]"
       if(present(CvOut)) write(*,*)'Cv     = ',CvOut,' [J/(K*m^3)]'
       if(present(HeatCondOut)) then
          write(*,*)'Eta    = ',EtaSI," [m^2/s]"
          write(*,*)'Kappa  = ',HeatCondOut," [W/(m*K)]"
          write(*,*)'Ffree  = ',&
               nElec*sqrt(cBoltzmann*TeSi/cElectronMass)*cBoltzmann*TeSI, &
               " [W/m^2]"
       end if
       write(*,*)''
    end if
    call test_stop(NameSub, DoTest, iBlock)

  end subroutine user_material_properties
  !============================================================================
  subroutine user_init_point_implicit

    use ModPointImplicit, ONLY: iVarPointImpl_I, IsPointImplMatrixSet
    logical:: DoTest
    integer :: iFluid
    character(len=*), parameter:: NameSub = 'user_init_point_implicit'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    !! Source terms are evaluated explicitly!
    ! RETURN

    ! All ion momenta are implicit
    if(UseElectronPressure)then
       allocate(iVarPointImpl_I(5*nIonFluid + 1))
       iVarPointImpl_I(5*nIonFluid + 1) = Pe_
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

    IsPointImplMatrixSet = .false.
    ! IsAsymmetric= .false.

    call test_stop(NameSub, DoTest)
  end subroutine user_init_point_implicit
  !============================================================================
  subroutine user_set_plot_var(iBlock, NameVar, IsDimensional,&
       PlotVar_G, PlotVarBody, UsePlotVarBody,&
       NameTecVar, NameTecUnit, NameIdlUnit, IsFound)

    use ModAdvance, ONLY: State_VGB
    use ModPhysics, ONLY: No2Si_V, UnitP_, UnitN_, UnitU_, UnitT_,&
         ElectronPressureRatio, &
         UnitTemperature_, UnitEnergyDens_, UnitX_
    use ModVarIndexes, ONLY: P_, Pe_
    use ModConst, ONLY: cBoltzmann
    use ModMultiFluid, ONLY: MassIon_I
    use ModMain, ONLY: nStep
    use ModCellGradient, ONLY: calc_gradient

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
    real :: nElec
    real :: nIon_I(nIonFluid)
    real :: uIon_DI(3,nIonFluid)

    real, allocatable :: &
         GradXVarX_C(:,:,:), GradXVarY_C(:,:,:), GradXVarZ_C(:,:,:), &
         GradYVarX_C(:,:,:), GradYVarY_C(:,:,:), GradYVarZ_C(:,:,:), &
         GradZVarX_C(:,:,:), GradZVarY_C(:,:,:), GradZVarZ_C(:,:,:)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_plot_var'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    IsFound = .true.

    select case(NameVar)
    case('nn1')
       NameIdlUnit = '1/cm^3'
       NameTecUnit = '[1/cm^3]'
       ! do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          if (DoEnhanceNeu) then
             PlotVar_G(i,j,k) = State_VGB(Neu1Rho_,i,j,k,iBlock)/ &
                  MassFluid_I(nFluid)*No2Si_V(UnitN_) * &
                  max( &
                  (EnhancedRatio*(DecadeDn+nStepEnhanceNeu-nStep))/DecadeDn, &
                  1.0)
          else
             PlotVar_G(i,j,k) = State_VGB(Neu1Rho_,i,j,k,iBlock)/ &
                  MassFluid_I(nFluid)*No2Si_V(UnitN_)
          end if
       end do; end do; end do

    case('tn1')
       NameIdlUnit = 'K'
       NameTecUnit = '[K]'
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          PlotVar_G(i,j,k) = State_VGB(Neu1P_,i,j,k,iBlock)* &
               MassFluid_I(nFluid)/State_VGB(Neu1Rho_,i,j,k,iBlock) * &
               No2SI_V(UnitTemperature_)
       end do; end do; end do

    case('te')
       NameIdlUnit = 'K'
       NameTecUnit = '[K]'
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          nIon_I(1:nIonFluid) = State_VGB(iRhoIon_I,i,j,k,iBlock)/ &
               MassIon_I*No2SI_V(UnitN_)
          nElec = sum(nIon_I(1:nIonFluid)*ChargeIon_I(1:nIonFluid))
          if(UseElectronPressure)then
             PlotVar_G(i,j,k) = State_VGB(Pe_,i,j,k,iBlock)*No2SI_V(UnitP_)/&
                  (cBoltzmann*nElec)
          else
             PlotVar_G(i,j,k) = State_VGB(P_,i,j,k,iBlock)*No2SI_V(UnitP_)* &
                  ElectronPressureRatio/(1.+ElectronPressureRatio)/ &
                  (cBoltzmann*nElec)
          end if
       end do; end do; end do

    case('tisw')
       NameIdlUnit = 'K'
       NameTecUnit = '[K]'
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          PlotVar_G(i,j,k) = State_VGB(iPIon_I(SW_),i,j,k,iBlock) / &
               State_VGB(iRhoIon_I(Sw_),i,j,k,iBlock) * &
               MassIon_I(Sw_)* NO2SI_V(UnitTemperature_)
       end do; end do; end do

    case('tih2op')
       NameIdlUnit = 'K'
       NameTecUnit = '[K]'
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          PlotVar_G(i,j,k) = State_VGB(iPIon_I(H2Op_),i,j,k,iBlock) / &
               State_VGB(iRhoIon_I(H2Op_),i,j,k,iBlock) * &
               MassIon_I(H2Op_)* NO2SI_V(UnitTemperature_)
       end do; end do; end do

    case('uplusx')
       NameIdlUnit = 'km/s'
       NameTecUnit = '[km/s]'
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          nIon_I(1:nIonFluid) = &
               State_VGB(iRhoIon_I,i,j,k,iBlock)/MassIon_I
          uIon_DI(1,1:nIonFluid) = State_VGB(iRhoUxIon_I,i,j,k,iBlock) / &
               State_VGB(iRhoIon_I,i,j,k,iBlock)
          nElec = sum(nIon_I(1:nIonFluid)*ChargeIon_I(1:nIonFluid))
          PlotVar_G(i,j,k) = &
               sum(nIon_I*uIon_DI(1,1:nIonFluid)*ChargeIon_I(1:nIonFluid)) &
               /nElec
       end do; end do; end do

    case('uplusy')
       NameIdlUnit = 'km/s'
       NameTecUnit = '[km/s]'
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          nIon_I(1:nIonFluid) = &
               State_VGB(iRhoIon_I,i,j,k,iBlock)/MassIon_I*No2SI_V(UnitN_)
          uIon_DI(2,1:nIonFluid) = State_VGB(iRhoUyIon_I,i,j,k,iBlock) / &
               State_VGB(iRhoIon_I,i,j,k,iBlock)*No2SI_V(UnitU_)
          nElec = sum(nIon_I(1:nIonFluid)*ChargeIon_I(1:nIonFluid))
          PlotVar_G(i,j,k) = &
               sum(nIon_I*uIon_DI(2,1:nIonFluid)*ChargeIon_I(1:nIonFluid)) &
               /nElec
       end do; end do; end do

    case('uplusz')
       NameIdlUnit = 'km/s'
       NameTecUnit = '[km/s]'
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          nIon_I(1:nIonFluid) = &
               State_VGB(iRhoIon_I,i,j,k,iBlock)/MassIon_I*No2SI_V(UnitN_)
          uIon_DI(3,1:nIonFluid) = State_VGB(iRhoUzIon_I,i,j,k,iBlock) / &
               State_VGB(iRhoIon_I,i,j,k,iBlock)*No2SI_V(UnitU_)
          nElec = sum(nIon_I(1:nIonFluid)*ChargeIon_I(1:nIonFluid))
          PlotVar_G(i,j,k) = &
               sum(nIon_I*uIon_DI(3,1:nIonFluid)*ChargeIon_I(1:nIonFluid)) &
               /nElec
       end do; end do; end do

    case('ns')
       NameIdlUnit = ' '
       NameTecUnit = '[ ]'
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          PlotVar_G(i,j,k) = ne20eV_GB(i,j,k,iBlock)
       end do; end do; end do

    case('v11')
       NameIdlUnit = '1/s'
       NameTecUnit = '[1/s]'
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          PlotVar_G(i,j,k) = v_IIGB(Neu1_,H2Op_,i,j,k,iBlock)
       end do; end do; end do

    case('v12')
       NameIdlUnit = '1/s'
       NameTecUnit = '[1/s]'
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          PlotVar_G(i,j,k) = v_IIGB(Neu1_,SW_,i,j,k,iBlock)
       end do; end do; end do

    case('v21')
       NameIdlUnit = '1/s'
       NameTecUnit = '[1/s]'
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          PlotVar_G(i,j,k) = ve_IIGB(Neu1_,H2Op_,i,j,k,iBlock)
       end do; end do; end do

    case('v22')
       NameIdlUnit = '1/s'
       NameTecUnit = '[1/s]'
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          PlotVar_G(i,j,k) = ve_IIGB(Neu1_,SW_,i,j,k,iBlock)
       end do; end do; end do

    case('v3')
       NameIdlUnit = ' '
       NameTecUnit = ' '
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          PlotVar_G(i,j,k) = SPeAdditional_GB(i,j,k,iBlock) &
               * No2Si_V(UnitEnergyDens_)/No2Si_V(UnitT_)
       end do; end do; end do

    case('s1')
       NameIdlUnit = ' '
       NameTecUnit = ' '
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          PlotVar_G(i,j,k) = TestArray_IGB(1,i,j,k,iBlock)
       end do; end do; end do

    case('s2')
       NameIdlUnit = ' '
       NameTecUnit = ' '
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          PlotVar_G(i,j,k) = TestArray_IGB(2,i,j,k,iBlock)
       end do; end do; end do

    case('s3')
       NameIdlUnit = ' '
       NameTecUnit = ' '
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          PlotVar_G(i,j,k) = TestArray_IGB(3,i,j,k,iBlock)
       end do; end do; end do

    case('s4')
       NameIdlUnit = ' '
       NameTecUnit = ' '
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          PlotVar_G(i,j,k) = TestArray_IGB(4,i,j,k,iBlock)
       end do; end do; end do

    case('s5')
       NameIdlUnit = ' '
       NameTecUnit = ' '
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          PlotVar_G(i,j,k) = TestArray_IGB(5,i,j,k,iBlock)
       end do; end do; end do

    case('s6')
       NameIdlUnit = ' '
       NameTecUnit = ' '
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          PlotVar_G(i,j,k) = TestArray_IGB(6,i,j,k,iBlock)
       end do; end do; end do

    case('s7')
       NameIdlUnit = ' '
       NameTecUnit = ' '
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          PlotVar_G(i,j,k) = TestArray_IGB(7,i,j,k,iBlock)
       end do; end do; end do

    case('s8')
       NameIdlUnit = ' '
       NameTecUnit = ' '
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          PlotVar_G(i,j,k) = TestArray_IGB(8,i,j,k,iBlock)
       end do; end do; end do

    case('divuh2op')
       PlotVar_G = 0.0
       NameIdlUnit = ' '
       NameTecUnit = ' '
       allocate(GradXVarX_C(1:nI,1:nJ,1:nK), GradXVarY_C(1:nI,1:nJ,1:nK), &
            GradXVarZ_C(1:nI,1:nJ,1:nK), GradYVarX_C(1:nI,1:nJ,1:nK),     &
            GradYVarY_C(1:nI,1:nJ,1:nK), GradYVarZ_C(1:nI,1:nJ,1:nK),     &
            GradZVarX_C(1:nI,1:nJ,1:nK), GradZVarY_C(1:nI,1:nJ,1:nK),     &
            GradZVarZ_C(1:nI,1:nJ,1:nK))
       call calc_gradient(iBlock, &
            State_VGB(H2OpRhoUx_,:,:,:,iBlock)/ &
            State_VGB(H2OpRho_, :,:,:,iBlock),  &
            GradXVarX_C, GradYVarX_C, GradZVarX_C)
       call calc_gradient(iBlock, &
            State_VGB(H2OpRhoUy_,:,:,:,iBlock)/ &
            State_VGB(H2OpRho_, :,:,:,iBlock),  &
            GradXVarY_C, GradYVarY_C, GradZVarY_C)
       call calc_gradient(iBlock, &
            State_VGB(H2OpRhoUz_,:,:,:,iBlock)/ &
            State_VGB(H2OpRho_, :,:,:,iBlock),  &
            GradXVarZ_C, GradYVarZ_C, GradZVarZ_C)
       PlotVar_G(1:nI,1:nJ,1:nK) = (GradXVarX_C + GradYVarY_C + GradZVarZ_C)*&
            NO2SI_V(UnitU_)/NO2SI_V(UnitX_)

    case default
       IsFound = .false.
    end select

    UsePlotVarBody = .false.
    PlotVarBody    = 0.0

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_plot_var
  !============================================================================
  subroutine user_preset_conditions(i,j,k,iBlock)
    ! This is applied as initial conditions and in the upstream boundary
    ! for the semi-implicit heat conduction
    use ModAdvance, ONLY: P_, Pe_, State_VGB
    use ModPhysics, ONLY: &
         SolarWindRho, SolarWindUx, SolarWindUy, SolarWindUz, SolarWindP, &
         LowDensityRatio, ElectronPressureRatio, Io2No_V, UnitTemperature_, &
         SolarWindBx, SolarWindBy, SolarWindBz, UnitX_, UnitU_, UnitN_, &
         Si2No_V, No2Si_V
    use ModGeometry, ONLY: Xyz_DGB, r_GB
    use ModVarIndexes, ONLY: MassFluid_I

    integer,intent(in) :: i, j, k, iBlock
    real:: RhoSw, RhoNeu1

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_preset_conditions'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    RhoSw = SolarWindRho*(1.0 - LowDensityRatio*(nIonFluid - 1))
    State_VGB(Rho_,i,j,k,iBlock)     = RhoSw
    State_VGB(RhoUx_,i,j,k,iBlock)   = RhoSw*SolarWindUx
    State_VGB(RhoUy_,i,j,k,iBlock)   = RhoSw*SolarWindUy
    State_VGB(RhoUz_,i,j,k,iBlock)   = RhoSw*SolarWindUz
    State_VGB(P_,i,j,k,iBlock)       = &
         SolarWindP*(1.0-LowDensityRatio*(nIonFluid - 1))

    State_VGB(H2OpRho_,i,j,k,iBlock)   = SolarWindRho*LowDensityRatio
    State_VGB(H2OpRhoUx_,i,j,k,iBlock) = SolarWindRho*LowDensityRatio*SolarWindUx
    State_VGB(H2OpRhoUy_,i,j,k,iBlock) = SolarWindRho*LowDensityRatio*SolarWindUy
    State_VGB(H2OpRhoUz_,i,j,k,iBlock) = SolarWindRho*LowDensityRatio*SolarWindUz
    State_VGB(H2OpP_,i,j,k,iBlock)     = SolarWindP*LowDensityRatio

    ! Neutral values are pre-set by Haser model
    RhoNeu1 = Qprod/ &
         ( 4.*cPi*(r_GB(i,j,k,iBlock)*No2Si_V(UnitX_))**2*uHaser ) * &
         exp(-vHI*r_GB(i,j,k,iBlock)*No2Si_V(UnitX_)/uHaser)*Si2No_V(UnitN_)*&
         MassFluid_I(nFluid)
    State_VGB(Neu1Rho_,i,j,k,iBlock)   = RhoNeu1
    State_VGB(Neu1Ux_:Neu1Uz_,i,j,k,iBlock) = RhoNeu1*uHaser*Si2No_V(UnitU_)*&
         Xyz_DGB(:,i,j,k,iBlock)/r_GB(i,j,k,iBlock)
    State_VGB(Neu1P_,i,j,k,iBlock)     = &
         State_VGB(Neu1Rho_,i,j,k,iBlock)/MassFluid_I(nFluid) &
         *50*Io2No_V(UnitTemperature_)

    State_VGB(Bx_,i,j,k,iBlock) = SolarWindBx
    State_VGB(By_,i,j,k,iBlock) = SolarWindBy
    State_VGB(Bz_,i,j,k,iBlock) = SolarWindBz

    if(UseElectronPressure) then
       State_VGB(Pe_,i,j,k,iBlock)     = &
            sum(State_VGB(iPIon_I,i,j,k,iBlock))*ElectronPressureRatio
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_preset_conditions
  !============================================================================
  subroutine user_set_ICs(iBlock)

    use ModMain, ONLY: Body1_
    use ModAdvance, ONLY: Pe_, State_VGB
    use ModPhysics, ONLY: &
         No2Si_V, UnitRho_, UnitRhoU_, UnitP_, FaceState_VI, UnitU_
    use BATL_lib, ONLY: Used_GB

    integer, intent(in) :: iBlock

    integer :: i, j, k, iIonFluid, iFluid
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_ICs'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
       if (.not.Used_GB(i,j,k,iBlock)) then
          State_VGB(:,i,j,k,iBlock) = FaceState_VI(:,Body1_)
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
       else
          call user_preset_conditions(i,j,k,iBlock)
       end if

    end do; end do ; end do

    if(DoTest) then
       i=iTest ; j=jTest ; k=kTest
123    format (A13,ES25.16,A15)
       do iIonFluid=1,nIonFluid
          write(*,*)'Ion species #',iIonFluid,': ',NameFluid_I(iIonFluid+1)
          write(*,123)'Rho       = ', &
               State_VGB(iRhoIon_I(iIonFluid),i,j,k,iBlock)*No2SI_V(UnitRho_),&
               " [kg/m^3]"
          write(*,123)'Ux        = ', &
               State_VGB(iRhoUxIon_I(iIonFluid),i,j,k,iBlock) / &
               State_VGB(iRhoIon_I(iIonFluid),i,j,k,iBlock)*No2SI_V(UnitU_), &
               " [km/s]"
          write(*,123)'rhoUx     = ', &
               State_VGB(iRhoUxIon_I(iIonFluid),i,j,k,iBlock)* &
               No2SI_V(UnitRhoU_), " [kg/(m^2*s)]"
          write(*,123)'rhoUy     = ', &
               State_VGB(iRhoUyIon_I(iIonFluid),i,j,k,iBlock)* &
               No2SI_V(UnitRhoU_), " [kg/(m^2*s)]"
          write(*,123)'rhoUz     = ', &
               State_VGB(iRhoUzIon_I(iIonFluid),i,j,k,iBlock)* &
               No2SI_V(UnitRhoU_), " [kg/(m^2*s)]"
          write(*,123)'Pi        = ', &
               State_VGB(iPIon_I(iIonFluid),i,j,k,iBlock)*No2SI_V(UnitP_), &
               " [Pa]"
       end do
       write(*,*)''
       write(*,*)'Total:'
       if (UseElectronPressure) then
          write(*,123)'Pe        = ', &
               State_VGB(Pe_,i,j,k,iBlock)*No2SI_V(UnitP_)," [Pa]"
       end if

    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_ICs
  !============================================================================
  subroutine user_set_cell_boundary(iBlock, iSide, TypeBc, IsFound)

    use ModAdvance, ONLY: State_VGB
    use ModImplicit, ONLY: StateSemi_VGB, iTeImpl
    use ModSize, ONLY: nI, MaxI, MinJ, MaxJ, MinK, MaxK
    use BATL_lib, ONLY: nG
    use ModPhysics, ONLY: Si2No_V, UnitTemperature_
    use ModCellBoundary, ONLY: set_cell_boundary
    use ModVarIndexes, ONLY: NameVar_V

    integer,          intent(in)  :: iBlock, iSide
    character(len=*), intent(in)  :: TypeBc
    logical,          intent(out) :: IsFound

    integer:: i, j, k, iVar
    real :: TeSi

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_cell_boundary'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    IsFound = .true.

    if (TypeBc == 'userfixed' .and. iSide /= 2)  &
         call stop_mpi(NameSub  &
         //' iSide should be 2 for userfixed, correct PARAM.in')

    if (TypeBc == 'userfixed') then
       call stop_mpi(NameSub  &
         //' Please switch from userfixed to fixed, correct PARAM.in')
       ! call set_cell_boundary(nG,iBlock,nVar,State_VGB(:,:,:,:,iBlock), &
       !     iSideIn = iSide, TypeBcIn = 'fixed')

       if(DoTest)then
          do iVar=1,nVar
             write(*,'(a,a,a,3es22.15)') 'initial ',            &
                  NameVar_V(iVar), '  cell,ghost,ghost2 =',    &
                  State_VGB(iVar, iTest,  jTest,kTest,iBlockTest),&
                  State_VGB(iVar, iTest+1,jTest,kTest,iBlockTest),&
                  State_VGB(iVar, iTest+2,jTest,kTest,iBlockTest)
          end do
       end if

       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = nI+1, MaxI
          State_VGB(Neu1Rho_:Neu1P_,i,j,k,iBlock) = &
               State_VGB(Neu1Rho_:Neu1P_,nI,j,k,iBlock)
       end do; end do; end do

       if(DoTest)then
          do iVar=1,nVar
             write(*,'(a,a,a,3es22.15)') 'final ',              &
                  NameVar_V(iVar), '  cell,ghost,ghost2 =',    &
                  State_VGB(iVar, iTest,  jTest,kTest,iBlockTest),&
                  State_VGB(iVar, iTest+1,jTest,kTest,iBlockTest),&
                  State_VGB(iVar, iTest+2,jTest,kTest,iBlockTest)
          end do
       end if
    end if

    if(TypeBc == 'usersemi')then
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = nI+1, MaxI
          call user_preset_conditions(i,j,k,iBlock)
          call user_material_properties(State_VGB(:,i,j,k,iBlock), &
               i, j, k, iBlock, TeOut=TeSi)
          StateSemi_VGB(iTeImpl,i,j,k,iBlock) = TeSi*Si2No_V(UnitTemperature_)
       end do; end do; end do
       RETURN
    elseif(TypeBc == 'usersemilinear')then
       RETURN
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_cell_boundary
  !============================================================================
  subroutine user_get_log_var(VarValue, TypeVar, Radius)

    use ModPhysics, ONLY: No2SI_V, UnitT_
    use ModAdvance, ONLY: DtMax_CB

    real, intent(out)             :: VarValue
    character (len=*), intent(in) :: TypeVar
    real, intent(in), optional    :: Radius

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_get_log_var'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(TypeVar)
    case('dtpnt')
       if (iProc == iProcTest) then
          VarValue = DtMax_CB(iTest,jTest,kTest,iBlockTest)*No2SI_V(UnitT_)
       else
          VarValue = 0.0
       end if
    case default
       VarValue = -7777.0
    end select

    call test_stop(NameSub, DoTest)
  end subroutine user_get_log_var
  !============================================================================
  subroutine user_amr_criteria(iBlock, UserCriteria, TypeCriteria, IsFound)

    ! Set UserCriteria = 1.0 for refinement, 0.0 for coarsening.

    use BATL_lib, ONLY: iNode_B, iTree_IA, Level_
    use ModAdvance, ONLY: State_VGB, H2OpRho_
    use ModPhysics, ONLY: No2SI_V, UnitN_
    use ModGeometry, ONLY: Xyz_DGB

    ! Variables required by this user subroutine

    integer, intent(in)          :: iBlock
    real, intent(out)            :: UserCriteria
    character (len=*),intent(in) :: TypeCriteria
    logical ,intent(inout)       :: IsFound

    integer:: i, j, k, nLevel, iNode
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_amr_criteria'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    IsFound = .true.

    UserCriteria = 0.0 ! only refinement

    iNode = iNode_B(iBlock)
    nLevel = iTree_IA(Level_,iNode)

    do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI

       if ((Xyz_DGB(x_,i,j,k,iBlock) < 0.1).and. &
            (Xyz_DGB(x_,i,j,k,iBlock) > -0.4).and. &
            (Xyz_DGB(y_,i,j,k,iBlock) <  0.05).and. &
            (Xyz_DGB(y_,i,j,k,iBlock) > -0.05).and. &
            (Xyz_DGB(z_,i,j,k,iBlock) <  0.2).and. &
            (Xyz_DGB(z_,i,j,k,iBlock) > -0.1)) then

          if (State_VGB(H2OpRho_,i,j,k,iBlock)/MassIon_I(H2Op_)* &
               No2SI_V(UnitN_)>1.0) then
             UserCriteria = 1.0
             RETURN
          end if
       end if
    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_amr_criteria
  !============================================================================
  subroutine user_initial_perturbation
    ! This is applied to reset all ions after the neutral background
    ! is fully developed

    use ModMain, ONLY: Unused_B
    use ModAdvance, ONLY: P_, Pe_, State_VGB
    use ModPhysics, ONLY: &
         SolarWindRho, SolarWindUx, SolarWindUy, SolarWindUz, SolarWindP, &
         LowDensityRatio, ElectronPressureRatio, SolarWindBx, SolarWindBy, &
         SolarWindBz
    use ModGeometry, ONLY: r_GB
    use BATL_lib, ONLY: Used_GB, Xyz_DGB

    integer :: i, j, k, iBlock
    real    :: RhoSw, Alpha, Beta
    real, dimension(nI,nJ,nK):: TempNeu1_C, nNeu1_C

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_initial_perturbation'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! The ration to obtained the perturbed H2OpRho from the Neu1Rho
    ! ratioPerturbed = vHi*LowDensityRatio

    do iBlock=1,nBlock

       if(Unused_B(iBlock)) CYCLE

       RhoSw = SolarWindRho*(1.0 - LowDensityRatio*(nIonFluid - 1))

       TempNeu1_C = State_VGB(Neu1P_,1:nI,1:nJ,1:nK,iBlock)* &
            MassFluid_I(nFluid)/State_VGB(Neu1Rho_,1:nI,1:nJ,1:nK,iBlock)
       nNeu1_C    = State_VGB(Neu1Rho_,1:nI,1:nJ,1:nK,iBlock) / &
            MassFluid_I(nFluid)

       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not. Used_GB(i,j,k,iBlock)) CYCLE

          if (.not. UsePerturbedSW) then
             if (r_GB(i,j,k,iBlock) <= R0Perturbed) then
                ! Near the comet, set the ions density = ratioPerturbed * nNeu1
                ! and the u = uNeu1, Ti = Tneu

                ! Cometary heavy ion
                State_VGB(H2OpRho_,i,j,k,iBlock)    = &
                     State_VGB(Neu1Rho_,i,j,k,iBlock)*ratioPerturbed
                State_VGB(H2OpRhoUx_,i,j,k,iBlock) = &
                     State_VGB(Neu1RhoUx_,i,j,k,iBlock)*ratioPerturbed
                State_VGB(H2OpRhoUy_,i,j,k,iBlock) = &
                     State_VGB(Neu1RhoUy_,i,j,k,iBlock)*ratioPerturbed
                State_VGB(H2OpRhoUz_,i,j,k,iBlock) = &
                     State_VGB(Neu1RhoUz_,i,j,k,iBlock)*ratioPerturbed
                State_VGB(H2OpP_,i,j,k,iBlock)   = &
                     State_VGB(Neu1P_,i,j,k,iBlock)*ratioPerturbed

             else if (r_GB(i,j,k,iBlock) <= R1Perturbed) then
                Alpha = (r_GB(i,j,k,iBlock)-R0Perturbed) / &
                     (R1Perturbed-R0Perturbed)
                Beta  = 1 - Alpha
                ! Transition region from near the comet to far away from the
                ! comet.
                ! The ion profile is set to be a linear combination between
                ! the near comet region and far away region.
                State_VGB(H2OpRho_,i,j,k,iBlock)    = &
                     State_VGB(Neu1Rho_,i,j,k,iBlock)*ratioPerturbed*Beta + &
                     SolarWindRho*LowDensityRatio*Alpha
                State_VGB(H2OpRhoUx_,i,j,k,iBlock) = &
                     State_VGB(Neu1RhoUx_,i,j,k,iBlock)*ratioPerturbed*Beta + &
                     SolarWindRho*LowDensityRatio*SolarWindUx*Alpha
                State_VGB(H2OpRhoUy_,i,j,k,iBlock) = &
                     State_VGB(Neu1RhoUy_,i,j,k,iBlock)*ratioPerturbed*Beta + &
                     SolarWindRho*LowDensityRatio*SolarWindUy*Alpha
                State_VGB(H2OpRhoUz_,i,j,k,iBlock) = &
                     State_VGB(Neu1RhoUz_,i,j,k,iBlock)*ratioPerturbed*Beta + &
                     SolarWindRho*LowDensityRatio*SolarWindUz*Alpha
                State_VGB(H2OpP_,i,j,k,iBlock) = &
                     State_VGB(Neu1P_,i,j,k,iBlock)*ratioPerturbed*Beta + &
                     SolarWindP*LowDensityRatio*Alpha

             else
                ! Far away from the comet, both ions are the same as the
                ! solar wind conditions
                State_VGB(Rho_,i,j,k,iBlock)     = RhoSw
                State_VGB(RhoUx_,i,j,k,iBlock)   = RhoSw*SolarWindUx
                State_VGB(RhoUy_,i,j,k,iBlock)   = RhoSw*SolarWindUy
                State_VGB(RhoUz_,i,j,k,iBlock)   = RhoSw*SolarWindUz
                State_VGB(P_,i,j,k,iBlock)       = &
                     SolarWindP*(1.0-LowDensityRatio*(nIonFluid - 1))

                State_VGB(H2OpRho_,i,j,k,iBlock)  =SolarWindRho*LowDensityRatio
                State_VGB(H2OpRhoUx_,i,j,k,iBlock)=SolarWindRho*LowDensityRatio*SolarWindUx
                State_VGB(H2OpRhoUy_,i,j,k,iBlock)=SolarWindRho*LowDensityRatio*SolarWindUy
                State_VGB(H2OpRhoUz_,i,j,k,iBlock)=SolarWindRho*LowDensityRatio*SolarWindUz
                State_VGB(H2OpP_,i,j,k,iBlock)    =SolarWindP  *LowDensityRatio

                State_VGB(Bx_,i,j,k,iBlock) = SolarWindBx
                State_VGB(By_,i,j,k,iBlock) = SolarWindBy
                State_VGB(Bz_,i,j,k,iBlock) = SolarWindBz
             end if

             ! Electron pressure
             if(UseElectronPressure) then
                State_VGB(Pe_,i,j,k,iBlock)     = &
                     sum(State_VGB(iPIon_I,i,j,k,iBlock))*ElectronPressureRatio
             end if
          else
             if (Xyz_DGB(x_,i,j,k,iBlock) >= xPerturbedSwMin .and. &
                  Xyz_DGB(x_,i,j,k,iBlock) <= xPerturbedSwMax) then
                State_VGB(Rho_,i,j,k,iBlock)   = PerturbedSwRho
                State_VGB(RhoUx_,i,j,k,iBlock) = PerturbedSwUx*PerturbedSwRho
                State_VGB(RhoUy_,i,j,k,iBlock) = PerturbedSwUy*PerturbedSwRho
                State_VGB(RhoUz_,i,j,k,iBlock) = PerturbedSwUz*PerturbedSwRho
                State_VGB(P_,i,j,k,iBlock)     = PerturbedSwN*PerturbedSwT
                State_VGB(Bx_,i,j,k,iBlock) = PerturbedSwBx
                State_VGB(By_,i,j,k,iBlock) = PerturbedSwBy
                State_VGB(Bz_,i,j,k,iBlock) = PerturbedSwBz

                if(UseElectronPressure) then
                   State_VGB(Pe_,i,j,k,iBlock) = PerturbedSwN*PerturbedSwTe
                end if

             end if
          end if

       end do; end do; end do

    end do

    call test_stop(NameSub, DoTest)
  end subroutine user_initial_perturbation
  !============================================================================
end module ModUser
!==============================================================================
