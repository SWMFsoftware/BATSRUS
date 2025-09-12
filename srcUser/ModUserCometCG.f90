!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModUser

  use ModUserEmpty,               &
       IMPLEMENTED1 => user_set_boundary_cells,         &
       IMPLEMENTED2 => user_read_inputs,                &
       IMPLEMENTED3 => user_init_session,               &
       IMPLEMENTED4 => user_set_face_boundary,           &
       IMPLEMENTED5 => user_action

  use ModSize
  use ModVarIndexes, ONLY: nVar
  use ModNumConst, ONLY: cPi
  use ModPhysics, ONLY: Io2No_V, Si2No_V, No2Si_V, &
       UnitRho_, UnitU_, UnitTemperature_, UnitT_, &
       UnitP_, UnitN_, UnitX_, Gamma
  use BATL_lib, ONLY: &
       test_start, test_stop, xTest, yTest, zTest, iProc

  include 'user_module.h' ! list of public methods

  ! Here you must define a user routine Version number and a
  ! descriptive string.
  character (len=*), parameter :: NameUserFile = "ModUserCometCG.f90"
  character (len=*), parameter :: NameUserModule = &
       'CG Comet, G. Toth & Z. Huang, 2014'

  character (len=100) :: NameShapeFile

  integer:: nTriangle
  real, allocatable:: XyzTriangle_DII(:,:,:), Normal_DI(:,:)
  real :: rMinShape = 0.0, rMaxShape = 0.0

  ! Position of the sun at the start time
  real :: LatSun=43.5, LonSun=0.0

  ! Rotation of the comet (changes the direction of the Sun)
  real:: RotationCometHour = 12.0

  ! Angular velocity
  real:: OmegaCometSi

  ! Maximum change in longitude before updating the boundary conditions
  real:: AngleUpdateDeg = 10.0

  ! The time between updates
  real:: DtUpdateSi
  integer:: DnUpdate = 0, nStepStart

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
  real :: SlopeProduction, bProduction, SlopeTemp, bTemp, cos75

  ! Coefficients to calculate uNormal and temperature from TempCometLocal
  real :: TempToUnormal
  real :: TempToPressure

  ! Last step and time the inner boundary values were saved for each block
  integer, allocatable :: nStepSave_B(:)

  real, allocatable :: TimeSimulationSave_B(:)

contains
  !============================================================================
  subroutine user_action(NameAction)

    character(len=*), intent(in):: NameAction

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_action'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(iProc==0)write(*,*) NameSub,' called with action ',NameAction

    select case(NameAction)
    case('initialize module')
       if(.not.allocated(nStepSave_B)) then
          allocate(nStepSave_B(MaxBlock))
          nStepSave_B = -100
          allocate(TimeSimulationSave_B(MaxBlock))
          TimeSimulationSave_B = -1e30
       end if
    case('clean module')
       if(allocated(nStepSave_B)) deallocate(nStepSave_B)
       if(allocated(TimeSimulationSave_B)) deallocate(TimeSimulationSave_B)
    end select
    call test_stop(NameSub, DoTest)
  end subroutine user_action
  !============================================================================

  subroutine user_read_inputs

    use ModMain, ONLY: UseExtraBoundary, nStep
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
       case("#SUNDIRECTION")
          call read_var('LatSun', LatSun)
          call read_var('LonSun', LonSun)

          ! If the Sun's position is changed, recalculate illumination
          call user_action('initialize module')
          nStepSave_B = -100
          TimeSimulationSave_B = -1e30

       case("#COMETSTATE")
          call read_var('ProductionRateMinSi', ProductionRateMinSi)
          call read_var('ProductionRateMaxSi', ProductionRateMaxSi)
          call read_var('SolarAngleMaxDim',    SolarAngleMaxDim)
          call read_var('TempCometMinDim',     TempCometMinDim)
          call read_var('TempCometMaxDim',     TempCometMaxDim)
          call read_var('TempComet75',         TempComet75Dim)
       case("#COMETROTATION")
          call read_var('RotationCometHour', RotationCometHour)
          call read_var('AngleUpdateDeg',    AngleUpdateDeg)
          call read_var('DnUpdate',          DnUpdate)
          nStepStart = nStep
       case('#USERINPUTEND')
          EXIT
       case default
          if(iProc==0) call stop_mpi( &
               NameSub//' invalid command='//trim(NameCommand))
       end select
    end do

    UseExtraBoundary   = .true.

    call test_stop(NameSub, DoTest)
  end subroutine user_read_inputs
  !============================================================================
  subroutine user_init_session

    ! Read shape file and convert units

    use ModMain, ONLY: tSimulation
    use ModNumConst, ONLY: cTwoPi, cDegToRad
    use ModCoordTransform, ONLY: dir_to_xyz
    use ModConst, ONLY: cBoltzmann, cAtomicMass
    use ModVarIndexes, ONLY: MassFluid_I
    use ModBlockData, ONLY: MaxBlockData
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_init_session'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    ! We need to have unit conversions before reading the shape file
    ! which contains everything in SI units
    call read_shape_file

    ! Production rate = number density * velocity, the unit is in [m^-2 s^-1]
    ! But, applying 1 / Si2No_V(UnitX_)**2 / Io2No_V(UnitT_) is not correct
    ! because Si2No_V(UnitN_) /= 1/Si2No_V(UnitX_)**3
    ProductionRateMax = &
         ProductionRateMaxSi * Si2No_V(UnitN_) * Si2No_V(UnitU_)
    ProductionRateMin = &
         ProductionRateMinSi * Si2No_V(UnitN_) * Si2No_V(UnitU_)

    SolarAngleMax= SolarAngleMaxDim * cDegToRad
    TempCometMin = TempCometMinDim * Io2No_V(UnitTemperature_)
    TempCometMax = TempCometMaxDim * Io2No_V(UnitTemperature_)
    TempComet75  = TempComet75Dim  * Io2No_V(UnitTemperature_)

    ! From Huebner and Markiwitz u_0 = sqrt(8kT/m pi)*0.8257 for f_rv=3 (H2O)
    ! 0.8345 for f_rv=2 (OH) and 0.8728 for f_rv=0 (H)
    TempToUnormal = 1.31763*sqrt(cBoltzmann/(MassFluid_I(1)*cAtomicMass) * &
         No2Si_V(UnitTemperature_))*Si2No_V(UnitU_)

    ! T' = C*T so and p = n*T' = rho*C*T/m so TempToPressure = C/m
    ! where C = 0.8097 for f_rv=0, 0.8858 for f_rv=2, 0.9049 for f_rv=3 (H20)
    TempToPressure = 0.9049/MassFluid_I(1)

    ! From Gamma. Toth's derivations
    ! uNormal = sqrt( kT / m ), so TempToUnormal = sqrt( k/ m )
    ! and also unit conversions of temperature to SI, and velocity from SI
    ! TempToUnormal = sqrt(cBoltzmann/(MassFluid_I(1)*cAtomicMass) * &
    !     No2Si_V(UnitTemperature_))*Si2No_V(UnitU_)
    !
    ! From Gamma. Toth's derivations
    ! T' = T/Gamma so and p = n*T' = rho*T/(Gamma*m) so TempToPressure = 1/(Gamma*m)
    ! TempToPressure = 1/(Gamma*MassFluid_I(1))

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
    OmegaCometSi = cTwoPi / (RotationCometHour*3600)

    ! Frequency of boundary condition updates
    DtUpdateSi = AngleUpdateDeg*cDegToRad / abs(OmegaCometSi)

    ! Maximum amount of data to be stored in ModBlockData
    ! This is for the inner boundary conditions.
    ! In practice this is a rather generous overestimate.
    MaxBlockData = nVar*(nI+1)*(nJ+1)*(nK+1)

    if(iProc==0)then
       write(*,*) 'ProductionRateMaxSi, ProductionRateMax =', &
            ProductionRateMaxSi, ProductionRateMax
       write(*,*) 'ProductionRateMinSi, ProductionRateMin =', &
            ProductionRateMinSi, ProductionRateMin
       write(*,*) 'SolarAngleMaxDim, SolarAngleMax =', &
            SolarAngleMaxDim, SolarAngleMax
       write(*,*) 'TempCometMinDim, TempCometMin   =', &
            TempCometMinDim, TempCometMin
       write(*,*) 'TempCometMaxDim, TempCometMax   =', &
            TempCometMaxDim, TempCometMax
       write(*,*) 'TempComet75Dim,  TempComet75    =', &
            TempComet75Dim,  TempComet75
       write(*,*) 'TempToUnormal, TempToPressure   =', &
            TempToUnormal, TempToPressure
       write(*,*) 'MassFluid_I,                    =', MassFluid_I
       write(*,*) 'TempToUn*sqrt(TempCometMax)     =', &
            TempToUnormal*sqrt(TempCometMax)
       write(*,*) 'SlopeProduction, bProduction    =', &
            SlopeProduction, bProduction
       write(*,*) 'SlopeTemp, bTemp                =', &
            SlopeTemp/Io2No_V(UnitTemperature_),bTemp/Io2No_V(UnitTemperature_)
       write(*,*)'LatSun, LonSun initial           =', &
            LatSun, LonSun
       write(*,*)'RotationCometHour, OmegaCometSi  =', &
            RotationCometHour, OmegaCometSi
       write(*,*)'AngleUpdateDeg, DtUpdateSi       =', &
            AngleUpdateDeg, DtUpdateSi
    end if

    call test_stop(NameSub, DoTest)
  end subroutine user_init_session
  !============================================================================
  subroutine user_set_boundary_cells(iBlock)

    use ModGeometry, ONLY: ExtraBc_, Xyz_DGB, r_GB
    use ModBoundaryGeometry, ONLY: iBoundary_GB
    integer, intent(in):: iBlock

    integer:: i, j, k
    real:: XyzInside_D(3)

    ! Place a point inside rMinShape sphere with transcendent coordinates
    ! to reduce chances of hitting the edge or corner of triangles

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_boundary_cells'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    XyzInside_D = rMinShape*[cPi/10,cPi**2/50,cPi**3/700]

    do k = MinK, MaxK; do j = MinJ, MaxJ; do i=MinI, MaxI
       ! Check if we are close enough
       if(r_GB(i,j,k,iBlock) > rMaxShape) then

       elseif(r_GB(i,j,k,iBlock) < rMinShape) then
          iBoundary_GB(i,j,k,iBlock) = ExtraBc_
       else
          ! Connect cell center with a point inside.
          ! If the line segment does not intersect the shape or it intersects
          ! even times then the point is inside the shape.
          if(.not. is_segment_intersected( &
               XyzInside_D, Xyz_DGB(:,i,j,k,iBlock), IsOddIn=.true.)) &
               iBoundary_GB(i,j,k,iBlock) = ExtraBc_
       end if

    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_boundary_cells
  !============================================================================

  subroutine read_shape_file

    use ModIoUnit, ONLY: UnitTmp_
    use ModCoordTransform, ONLY: cross_product
    use ModRandomNumber, ONLY: random_real

    logical:: DoReadShapeFile = .true.

    integer:: nPoint, i, j, iPoint, iTriangle, iPoint1, iPoint2, iPoint3
    integer:: iSeed = 7

    real, allocatable:: Xyz_DI(:,:)
    real :: RandomNumber

    character(len=100):: String1, String2

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_shape_file'
    !--------------------------------------------------------------------------
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
    do iPoint = 1, nPoint
       read(UnitTmp_,*) String1, i, j, Xyz_DI(:,iPoint)

       ! Perturb vertices of all triangles to avoid the the situation that
       ! a line segment is parallel to a triangle plane in
       ! is_segment_intersected

       Xyz_DI(1, iPoint) = Xyz_DI(1, iPoint) + random_real(iSeed)*1e-5
       Xyz_DI(2, iPoint) = Xyz_DI(2, iPoint) + random_real(iSeed)*1e-5
       Xyz_DI(3, iPoint) = Xyz_DI(3, iPoint) + random_real(iSeed)*1e-5

       ! Convert from SI units to normalized unit
       Xyz_DI(:,iPoint) = Xyz_DI(:,iPoint) * Si2No_V(UnitX_)
    end do
    do iTriangle = 1, nTriangle
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

    ! write(*,*)'!!! XyzTriangle_DII(:,:,1)=',XyzTriangle_DII(:,:,1)
    ! write(*,*)'!!! XyzTriangle_DII(:,:,n)=',XyzTriangle_DII(:,:,nTriangle)

    rMinShape = sqrt(minval(sum(Xyz_DI**2,DIM=1)))
    rMaxShape = sqrt(maxval(sum(Xyz_DI**2,DIM=1)))

    if(iProc==0)write(*,*) NameSub,' rMinShape, rMaxShape=', &
         rMinShape, rMaxShape

    deallocate(Xyz_DI)

    close(UnitTmp_)

    call test_stop(NameSub, DoTest)
  end subroutine read_shape_file
  !============================================================================

  subroutine user_set_face_boundary(FBC)

    use ModMain, ONLY: nStep, tSimulation, IsTimeAccurate, Dt, FaceBCType
    use ModVarIndexes, ONLY: nVar, Rho_, p_, Ux_, Uz_, MassFluid_I
    use ModGeometry, ONLY: ExtraBc_
    use BATL_lib, ONLY: CellSize_DB
    use ModGeometry, ONLY: Xyz_DGB
    use ModNumConst, ONLY: cDegToRad, cRadToDeg
    use ModCoordTransform, ONLY: dir_to_xyz
    use ModBlockData, ONLY: use_block_data, clean_block_data, &
         get_block_data, put_block_data

    logical :: DoTestHere=.true., IsIlluminated = .false.

    type(FaceBCType), intent(inout):: FBC

    integer:: iTrue, jTrue, kTrue, iBody, jBody, kBody

    real :: XyzIntersect_D(3), XyzStart_D(3), XyzEnd_D(3)
    real :: TestFace_D(3)
    real :: XyzTrueCell_D(3), XyzBodyCell_D(3)
    real :: Normal_D(3), CosAngle
    real :: TempCometLocal, uNormal, ProductionRateLocal
    real :: LonSunNow
    real :: FaceCoordsTest_D(3) = 0.0

    integer:: nStepLonSun = -1
    real, save :: NormalSun_D(3)
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_face_boundary'
    !--------------------------------------------------------------------------
    associate( VarsGhostFace_V => FBC%VarsGhostFace_V, &
               VarsTrueFace_V => FBC%VarsTrueFace_V, &
               FaceCoords_D => FBC%FaceCoords_D, &
               iBoundary => FBC%iBoundary, iBlock => FBC%iBlockBc, &
               iFace => FBC%iFace, jFace => FBC%jFace, kFace => FBC%kFace, &
               iSide => FBC%iSide )

    call test_start(NameSub, DoTest, iBlock)

    ! We can use the saved values if
    ! not too much time or time step has passed since the last save
    if(  use_block_data(iBlock)                                      .and. &
         tSimulation < TimeSimulationSave_B(iBlock) + DtUpdateSi .and. &
         (DnUpdate <= 0 .or. nStep < nStepSave_B(iBlock) + DnUpdate)) then
       call get_block_data(iBlock, nVar, VarsGhostFace_V)
       RETURN
    end if

    ! Empty the block storage if we redo the calculation
    if(use_block_data(iBlock)) call clean_block_data(iBlock)

    if (iBoundary /= ExtraBc_) &
         call stop_mpi(NameSub//' is implemented for extra BC only')

    ! Recalculate LonSunNow if needed. Note that we use tSimulation
    ! that only changes after the full time step is done
    ! and not ModFaceBoundary::TimeBc that can change during subcycling.
    if(nStepLonSun < nStep)then

       nStepLonSun = nStep

       if(IsTimeAccurate)then
          ! The sun is moving clockwise in the rotating frame of the comet
          LonSunNow = LonSun - OmegaCometSi*tSimulation*cRadToDeg
          if (DoTest) write(*,*) NameSub, &
               ': iProc, Time, LonSunNow=', iProc, tSimulation, LonSunNow

       elseif(DnUpdate > 0)then
          ! Move the sun direction by AngleUpdateDeg every DnUpdate step
          LonSunNow = LonSun + (AngleUpdateDeg*(nStep - nStepStart))/DnUpdate
          if (DoTest) write(*,*) NameSub, &
               ': iProc, Step, LonSunNow=', iProc, nStep, LonSunNow

       else
          ! Use input direction
          LonSunNow = LonSun
          if (DoTest) write(*,*) NameSub, &
               ': iProc, LonSun, LonSunNow=', iProc, LonSun, LonSunNow
       end if

       ! Get the direction vector to the Sun
       call dir_to_xyz((90-LatSun)*cDegToRad, LonSunNow*cDegToRad, NormalSun_D)

    end if

    ! Save step and simulation time info
    nStepSave_B(iBlock)          = nStep
    TimeSimulationSave_B(iBlock) = tSimulation

    ! Floating boundary condition by default
    VarsGhostFace_V = VarsTrueFace_V

    ! Default indexes for the true and body cells
    iTrue = iFace; jTrue = jFace; kTrue = kFace
    iBody = iFace; jBody = jFace; kBody = kFace

    select case(iSide)
    case(1)
       iBody = iFace - 1
    case(2)
       iTrue = iFace - 1
    case(3)
       jBody = jFace - 1
    case(4)
       jTrue = jFace - 1
    case(5)
       kBody = kFace -1
    case(6)
       kTrue = kFace -1
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
            ': No intersection points are found between true and the body cells')
    end if

    ! Fix the normal direction if it is not pointing outward
    if (sum(Normal_D*(XyzTrueCell_D - XyzBodyCell_D)) < 0.0) &
         Normal_D = -Normal_D

    ! Set local outflow parameters as default that may be overwritten if illuminated
    TempCometLocal      = TempCometMin
    ProductionRateLocal = ProductionRateMin

    ! Check if Sun light hits the shape
    CosAngle = sum(Normal_D*NormalSun_D)

    if (CosAngle > 0.0) then

       ! See whether the intersection point is in the shade by going towards the Sun
       ! and checking for intersection with the shape
       XyzStart_D = XyzIntersect_D + 1e-9*rMaxShape*NormalSun_D
       XyzEnd_D   = XyzIntersect_D +    2*rMaxShape*NormalSun_D
       if(.not.is_segment_intersected(XyzStart_D, XyzEnd_D)) then
          IsIlluminated = .true.

          ! Increase temperature of the face if it is illuminated
          TempCometLocal      = max( TempCometMin, &
               SlopeTemp / CosAngle + bTemp)
          ! Increase neutral production rate
          ProductionRateLocal = max( ProductionRateMin, &
               SlopeProduction * CosAngle + bProduction )
       end if
    end if

    ! Calculate the normal velocity
    uNormal = sqrt(TempCometLocal)*TempToUnormal

    VarsGhostFace_V(Ux_:Uz_) = Normal_D*uNormal
    VarsGhostFace_V(Rho_)    = ProductionRateLocal/uNormal*MassFluid_I(1)
    VarsGhostFace_V(P_)      = &
         VarsGhostFace_V(Rho_)*TempCometLocal*TempToPressure

    if (.false. .and. DoTestHere .and. IsIlluminated .and. CosAngle > 0.5) then
       FaceCoordsTest_D = FaceCoords_D

       write(*,*) 'FaceCoords_D  =', FaceCoords_D
       write(*,*) 'XyzTrueCell_D =', XyzTrueCell_D
       write(*,*) 'XyzBodyCell_D =', XyzBodyCell_D
       write(*,*) 'XyzIntersect_D=', XyzIntersect_D
       write(*,*) 'XyzStart_D    =', XyzStart_D
       write(*,*) 'XyzEnd_D      =', XyzEnd_D
       write(*,*) 'Normal_D      =', Normal_D
       write(*,*) 'CosAngle      =', CosAngle
       write(*,*) 'ProductionRate=', ProductionRateLocal
       write(*,*) 'TempLocal [K] =', No2Si_V(UnitTemperature_)*TempCometLocal
       write(*,*) 'Temperature[K]=', No2Si_V(UnitTemperature_)* &
            VarsGhostFace_V(p_)*MassFluid_I(1)/VarsGhostFace_V(Rho_)
       write(*,*) 'Rho           =', VarsGhostFace_V(Rho_)
       write(*,*) 'u_D           =', VarsGhostFace_V(Ux_:Uz_)
       write(*,*) 'uNormal       =', uNormal
       write(*,*) 'p             =', VarsGhostFace_V(p_)
       write(*,*) 'Mach number   =', &
            uNormal/sqrt(Gamma*VarsGhostFace_V(p_)/VarsGhostFace_V(Rho_))
       DoTestHere=.false.
    end if

    IsIlluminated = .false.

    ! Store for future time steps
    call put_block_data(iBlock, nVar, VarsGhostFace_V)

    call test_stop(NameSub, DoTest, iBlock)

    end associate
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

    integer:: iTriangle, nIntersect, iIntersect, iMinRatio

    integer, parameter:: MaxIntersect = 10
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
    do iTriangle = 1, nTriangle
       ! Find intersection of line segment with the plane of the triangle
       nDotP2P1 = sum(Normal_DI(:,iTriangle)*(Xyz2_D - Xyz1_D))

       ! Check if the segment is parallel to the plane
       if(abs(nDotP2P1) < 1e-12) then
          if (abs(sum(Normal_DI(:,iTriangle)*(Xyz1_D - XyzTriangle_DII(:,1,iTriangle)))) &
               < 1e-12) then
             write(*,*) 'segment lies in the same plane: iTriangle: ', Xyz2_D
             write(*,*) 'Test:', &
                  sum(Normal_DI(:,iTriangle)*(Xyz1_D - XyzTriangle_DII(:,1,iTriangle)))
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

!       if (abs(Xyz2_D(1) -xTest) <= 1e-4 .and. &
!            abs(Xyz2_D(2) - yTest) <= 1e-4 .and. &
!            abs(Xyz2_D(3) - zTest) <= 1e-4) then
!          if (abs(Ratio2) < 1e-10 .or. abs(Ratio2 - 1.0) < 1e-10) then
!             write(*,*) 'iTriangle: ', iTriangle, 'Ratio1: ', Ratio1, 'Ratio2: ', Ratio2
!          end if
!       end if

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

       if(nIntersect > MaxIntersect) call stop_mpi(NameSub// &
            ': too many intersections, increase MaxIntersect')

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
!          write(*,*) 'nIntersect: ', nIntersect, 'Ratio_I: ', Ratio_I(1:nIntersect)
!          write(*,*) 'Xyz1_D: ', Xyz1_D
!          write(*,*) 'Xyz2_D: ', Xyz2_D
!          do iIntersect = 1, nIntersect
!             iTriangle = Triangle_I(iIntersect)
!             write(*,*) XyzTriangle_DII(:,1,iTriangle)
!             write(*,*) XyzTriangle_DII(:,2,iTriangle)
!             write(*,*) XyzTriangle_DII(:,3,iTriangle)
!          end do
          write(*,*)  'Ratio_I(iMinRatio)            = ', Ratio_I(iMinRatio)
          write(*,*)  'minval(Ratio_I(1:nIntersect)) = ', minval(Ratio_I(1:nIntersect))
          write(*,*)  'Ratio_I(1:nIntersect)', Ratio_I(1:nIntersect)
       end if
    end if
    if(present(XyzIntersectOut_D)) then
       XyzIntersectOut_D = Xyz1_D + minval(Ratio_I(1:nIntersect))*(Xyz2_D -  Xyz1_D)
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

end module ModUser
!==============================================================================
