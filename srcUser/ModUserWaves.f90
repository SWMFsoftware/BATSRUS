!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModUser

  ! Revision history:
  ! Nov. 2010 Rona Oran -
  !   1 . Added user problem AdvectSphere and comparison to exact solution for
  !   this test problem.
  !   2. Allow user to choose the unit of length used in input commands #WAVE,
  !   #WAVE2 and #ADVECTSPHERE, without affecting the actual normalization in
  !   BATSRUS.
  !   This simplifies the way IC's are set up in the input file. This option is
  !   useful in case this user module is used by two coupled components with
  !   different normalizations.
  !
  !   USAGE:
  !   #USERINPUTUNITX
  !   T            UseUserInputUnitX
  !   String       TypeInputUnitX
  !   real         UnitXSi
  !
  !   If UseUSerInputUnitX = T the String is read. String specifies the input
  !   unit of length.
  !   Options are: rPlanet, rBody, rSun, cAU, Si.
  !   In case String='Si', the third parameter is read, allowing any value in
  !   Si units to be chosen.

  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest, iBlockTest, iProc, &
       IsCartesian

  use ModUserEmpty,               &
       IMPLEMENTED1 => user_read_inputs,                &
       IMPLEMENTED2 => user_set_ics,                    &
       IMPLEMENTED3 => user_get_log_var,                &
       IMPLEMENTED4 => user_get_b0,                     &
       IMPLEMENTED5 => user_set_face_boundary,          &
       IMPLEMENTED6 => user_set_cell_boundary,          &
       IMPLEMENTED7 => user_amr_criteria,               &
       IMPLEMENTED8 => user_set_plot_var,               &
       IMPLEMENTED9 => user_update_states

  use ModSize, ONLY: x_, y_, z_
  use ModVarIndexes

  include 'user_module.h' ! list of public methods

  ! user module info
  character(len=*), parameter :: NameUserFile = "ModUserWaves.f90"
  character(len=*), parameter :: NameUserModule = "Waves"

  character(len=20):: NameProblem='wave'
  real :: Width, Amplitude, AmplitudeRho, Phase, LambdaX, LambdaY, LambdaZ
  real, dimension(nVar):: Width_V=0.0, Ampl_V=0.0, Phase_V=0.0, &
       x_V=0.0, y_V=0.0, z_V=0.0, KxWave_V=0.0, KyWave_V=0.0, KzWave_V=0.0
  integer :: iPower_V(nVar) = 1
  logical :: DoInitialize=.true.

  ! GEM challenge parameters
  real, parameter :: Lambda0=0.5, Tp=0.5 , B0=1.0
  real :: GemEps=0.0, Az=0.1

  ! The (rotated) unperturbed initial state with primitive variables
  real :: PrimInit_V(nVar)

  ! Velocity of wave (default is set for right going whistler wave test)
  real :: Velocity=169.344

  ! Entropy constant for isentropic initial condition. Only used if positive.
  real :: EntropyConstant=-1.0

  ! Integrate sin / cos wave over cell if true
  logical :: DoIntegrateWave=.false.

  ! Variables used by the user problem AdvectSphere
  real :: pBackgrndIo, uBackgrndIo, FlowAngleTheta, FlowAnglePhi
  real :: NumDensBackgrndIo, NumDensMaxIo
  real :: rSphere, rSphereIo, RhoBackgrndNo, RhoMaxNo, UxNo, UyNo, UzNo
  real :: xSphereCenterInitIo, xSphereCenterInit
  real :: ySphereCenterInitIo, ySphereCenterInit
  real :: zSphereCenterInitIo, zSphereCenterInit
  logical :: DoCalcAnalytic=.false., DoInitSphere=.false.

  ! Variables for the generalized power profile
  logical :: IsPowerProfile_V(nVar)=.false.
  integer :: nPowerX_V(nVar)=1, nPowerY_V(nVar)=1, nPowerZ_V(nVar)=1
  real    :: CoeffX_V(nVar)=0.0, CoeffY_V(nVar)=0.0, CoeffZ_V(nVar)=0.0

  ! Enable user units of length in input file
  logical           :: UseUserInputUnitx=.false.
  character(len=20) :: TypeInputUnitX
  real              :: InputUnitXSi=0.0

  ! aux. flags for problem types
  logical :: DoAdvectSphere, DoWave, DoPipeFlow=.false., &
       DoResistivityGaussian=.false.

  logical :: UseInitialStateDefinition=.false.

  ! Variables for shockramp problem
  logical :: DoShockramp=.false.

  ! Variables for incompressible flow problem: Hill vortex
  real:: xCenter, zCenter, xWidth, zWidth
  logical:: IsSmooth
  real, allocatable:: Stream_DG(:,:,:,:), &
       Un_XB(:,:,:,:),  Un_YB(:,:,:,:),  Un_ZB(:,:,:,:)

contains
  !============================================================================
  subroutine user_read_inputs
    use ModMain
    use ModReadParam
    ! use ModPhysics,  ONLY: Si2No_V, Io2Si_V,Io2No_V,&
    !      UnitRho_, UnitU_, UnitP_, UnitN_, UnitX_
    use ModNumConst, ONLY: cTwoPi,cDegToRad
    use ModUtilities, ONLY: split_string, join_string
    use ModInitialState, ONLY: init_initial_state, read_initial_state_param

    character(len=100) :: NameCommand
    character(len=500) :: StringVar
    character(len=20)  :: NameVar
    integer :: iVar
    logical:: DoTest

    character(len=*), parameter:: NameSub = 'user_read_inputs'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    UseUserIcs = .true.

    do
       if(.not.read_line() ) EXIT
       if(.not.read_command(NameCommand)) CYCLE
       select case(NameCommand)

       case('#USERPROBLEM')
          call read_var('NameProblem',NameProblem)

       case('#GEM')
          call read_var('Amplitude',Az)

       case('#GEMPERTURB')
          call read_var('GemEps',GemEps)

       case('#HILL')
          NameProblem = 'HILL'
          call read_var('xWidth',  xWidth)
          call read_var('zWidth',  zWidth)
          call read_var('xCenter', xCenter)
          call read_var('zCenter', zCenter)
          call read_var('IsSmooth', IsSmooth)

       case('#SPHALFVEN')
          NameProblem = 'SPHALFVEN'
       case('#RT')
          NameProblem = 'RT'
          call read_var('AmplitudeUx', Amplitude)
          call read_var('WidthXUx'   , Width)
          call read_var('LambdaYUx'  , LambdaY)
          call read_var('IsSmoothRho', IsSmooth)
          if(IsSmooth)then
             call read_var('WidthRho',     Width)
             call read_var('AmplitudeRho', AmplitudeRho)
          end if

       case('#WAVESPEED')
          call read_var('Velocity',Velocity)

       case('#SHOCKRAMP')
          call read_var('DoShockramp', DoShockramp)

       case('#USERINPUTUNITX')
          ! This option controls the normalization of the unit of length
          ! in input commands #WAVE, #WAVE2, #ADVECTSPHERE only.
          ! This will not affect the normalization of state variables.
          ! Designed to allow simple input in case two coupled components
          ! use this user module
          call read_var('UseUserInputUnitX', UseUserInputUnitx)
          if (UseUserInputUnitX) then
             call read_var('TypeInputUnitX', TypeInputUnitX)
             if(TypeInputUnitX=='Si') then
                call read_var('InputUnitXSi',InputUnitXSi)
                if(InputUnitXSi <= 0.0) &
                     call CON_stop('InputUnitXSi <= 0 . Correct PARAM.in')
             end if
          end if

       case('#SHEARSQUARE')
          NameProblem = 'ShearSquare'
          call read_var('Amplitude', Amplitude)
          call read_var('Side', Width)

       case('#ADVECTSPHERE')
          NameProblem = 'AdvectSphere'
          call read_var('DoInitSphere',      DoInitSphere     )
          call read_var('NumDensBackgrndIo', NumDensBackgrndIo)
          call read_var('pBackgrndIo',       pBackgrndIo      )
          call read_var('uBackgrndIo',       uBackgrndIo      )
          call read_var('FlowAngleTheta',    FlowAngleTheta   )
          call read_var('FlowAnglePhi',      FlowAnglePhi     )
          call read_var('rSphereIo',         rSphereIo        )
          call read_var('NumDensMaxIo',      NumDensMaxIo     )
          call read_var('xSphereCenterInitIo', xSphereCenterInitIo)
          call read_var('ySphereCenterInitIo', ySphereCenterInitIo)
          call read_var('zSphereCenterInitIo', zSphereCenterInitIo)

       case('#ANALYTIC')
          call read_var('DoCalcAnalytic', DoCalcAnalytic)

       case('#POWERPROFILE')
          ! Read parameters for a power profile. Power is a postive or
          ! negative integer or zero (linear profile).
          call read_var('NameVar',   NameVar)
          call get_iVar(NameVar, iVar)
          call read_var('CoeffX',  CoeffX_V(iVar))
          call read_var('nPowerX', nPowerX_V(iVar))
          if(nDim > 1) call read_var('CoeffY',  CoeffY_V(iVar))
          if(nDim > 1) call read_var('nPowerY', nPowerY_V(iVar))
          if(nDim > 2) call read_var('CoeffZ',  CoeffZ_V(iVar))
          if(nDim > 2) call read_var('nPowerZ', nPowerZ_V(iVar))

          NameProblem = 'PowerProfile'
          IsPowerProfile_V(iVar) = .true.

          if(iProc == 0) write(*,*) 'Setting POWERPROFILE for iVar =', &
               iVar, ', NameVar =', NameVar_V(iVar)

       case('#PIPEFLOW')
          call read_var('DoPipeFlow',DoPipeFlow)
          if(DoPipeFlow) NameProblem = 'PipeFlow'

       case('#USERINPUTEND')
          if(iProc==0) write(*,*)'USERINPUTEND'
          EXIT

       case default
          if(iProc==0) call stop_mpi( &
               'read_inputs: unrecognized command: '//NameCommand)
       end select
    end do
    call test_stop(NameSub, DoTest)
  end subroutine user_read_inputs
  !============================================================================
  subroutine user_set_ics(iBlock)

    use ModMain, ONLY: TypeCoordSystem, GravitySi
    use ModCalcSource, ONLY: FrictionSi, FrictionUDim_D
    use ModGeometry, ONLY: &
         xMinBox, xMaxBox, yMinBox, yMaxBox, zMinBox, zMaxBox
    use ModAdvance, ONLY: &
         State_VGB, RhoUx_, RhoUy_, RhoUz_, Ux_, Uy_, Uz_, &
         Bx_, By_, Bz_, rho_, Ppar_, p_, Pe_, &
         UseElectronPressure, UseAnisoPressure, UseEfield, UseAnisoPe
    use ModMultiFluid, ONLY: &
         iRho_I, iUx_I, iUy_I, iUz_I, iRhoUx_I, iRhoUy_I, iRhoUz_I, iP_I
    use ModPhysics, ONLY: &
         ShockSlope, ShockLeft_V, ShockRight_V, &
         Si2No_V, Io2Si_V, Io2No_V, UnitRho_, UnitU_, UnitP_,UnitX_, UnitN_,&
         rPlanetSi, rBody, UnitT_, Gamma_I, nVectorVar, iVectorVar_I
    use ModNumconst, ONLY: cHalfPi, cPi, cTwoPi, cDegToRad
    use ModSize, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK,nI,nJ,nK
    use ModConst, ONLY: cProtonMass, rSun, cAu, RotationPeriodSun
    use BATL_lib, ONLY: &
         nDim, CoordMax_D, CoordMin_D, IsPeriodic_D, CellSize_DB, Xyz_DGB
    use ModInitialState, ONLY: get_initial_state
    use ModIonElectron, ONLY: &
         correct_electronfluid_efield , DoCorrectElectronFluid, DoCorrectEfield

    integer, intent(in) :: iBlock

    real:: State_V(nVar), KxTemp_V(nVar), KyTemp_V(nVar)
    real:: SinSlope, CosSlope, Input2SiUnitX, OmegaSun
    real:: x, y, z, r, r2, Lx, Ly, HalfWidth
    integer:: i, j, k, iVectorVar, iVar, iVarX, iVarY

    real :: Rho, RhoLeft, RhoRight, pLeft, Bx, By, Bz, B2
    real :: ViscoCoeff

    real :: Acceleration

    ! For 4th order scheme
    real :: Laplace
    real, allocatable:: State_G(:,:,:)

    logical:: DoTest

    character(len=*), parameter:: NameSub = 'user_set_ics'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    if(UseInitialStateDefinition)then
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          x = Xyz_DGB(x_,i,j,k,iBlock)
          y = Xyz_DGB(y_,i,j,k,iBlock)
          call get_initial_state( [x, y], State_VGB(:,i,j,k,iBlock) )
       end do; end do; end do
    end if

    if(UseUserInputUnitX) then
       select case(TypeInputUnitX)
       case('rPlanet')
          Input2SiUnitX = rPlanetSi
       case('rBody')
          Input2SiUnitX = rBody
       case('rSun')
          Input2SiUnitX = rSun
       case('cAU')
          Input2SiUnitX = cAu
       case('Si')
          Input2SiUnitX = InputUnitXSi
       case default
          call CON_stop('TypeInputUnitX is not set, correct PARAM.in')
       end select
    end if

    select case(NameProblem)
    case('HILL')
       ! set density
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          if(IsCartesian)then
             x = Xyz_DGB(x_,i,j,k,iBlock) - xCenter
             z = Xyz_DGB(y_,i,j,k,iBlock) - zCenter
          else
             x = sqrt(sum(Xyz_DGB(x_:y_,i,j,k,iBlock)**2)) - xCenter
             z = Xyz_DGB(z_,i,j,k,iBlock) - zCenter
          end if
          Rho = ShockLeft_V(Rho_)
          if(IsSmooth)then
             if(abs(x) < xWidth/2 .and. abs(z) < zWidth/2) &
                  Rho = Rho + cos(cPi*x/xWidth)**4*cos(cPi*z/zWidth)**4
          else
             if((x/xWidth)**2 + (z/zWidth)**2 < 0.25) &
                  Rho = Rho + 1.0
          end if
          State_VGB(Rho_,i,j,k,iBlock)   = Rho
       end do; end do; end do
       call set_hill_velocity(iBlock)
    case('SPHALFVEN')
       ! set u=B for a spherical Alfven wave inside |x|,|y| < pi
       ! Outside this square B = ShockLeft_V(Bx_:Bz_)]

       ! Calculate minimum Bz value and adjust ShockLeft(Bz_) if needed
       Bx = ShockLeft_V(Bx_)
       By = ShockLeft_V(By_)
       Bz = sqrt( (2 + abs(Bx))**2 - Bx**2 + (2 + abs(By))**2 - By**2)
       if(abs(ShockLeft_V(Bz_)) < Bz)then
          if(ShockLeft_V(Bz_) >= 0)then
             ShockLeft_V(Bz_) = Bz
          else
             ShockLeft_V(Bz_) = -Bz
          end if
       endif
       B2 = sum(ShockLeft_V(Bx_:Bz_)**2)
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          x = Xyz_DGB(x_,i,j,k,iBlock)
          y = Xyz_DGB(y_,i,j,k,iBlock)
          if(abs(x) < cPi .and. abs(y) < cPi) then
             Bx = ShockLeft_V(Bx_) - (1 + cos(x))*sin(y)
             By = ShockLeft_V(By_) + (1 + cos(y))*sin(x)
             Bz = sqrt(B2 - Bx**2 - By**2)
             State_VGB(Bx_,i,j,k,iBlock) = Bx
             State_VGB(By_,i,j,k,iBlock) = By
             State_VGB(Bz_,i,j,k,iBlock) = Bz
          else
             ! in case it was modified above
             State_VGB(Bz_,i,j,k,iBlock) = ShockLeft_V(Bz_)
          end if
          ! Modify velocities to include Alfven wave perturbations
          State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) = &
               State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) &
               + State_VGB(Bx_:Bz_,i,j,k,iBlock)*sqrt(ShockLeft_V(Rho_))
       end do; end do; end do

    case('RT')
       ! Initialize Rayleigh-Taylor instability

       Acceleration = max(GravitySi, FrictionSi*FrictionUDim_D(x_))

       ! Set pressure gradient balancing Gravity/Friction

       ! pressure = pLeft + integral_x1^xMaxBox rho*Gamma dx

       RhoLeft  = ShockLeft_V(Rho_)
       RhoRight = ShockRight_V(Rho_)
       pLeft    = ShockLeft_V(p_)

       if(IsSmooth)then
          ! Gaussian density profile. Integral from -infty to x is 1+erf(x)
          ! Integral scales with amplitude and 1/KxWave
          do i = MinI, MaxI
             x = Xyz_DGB(x_,i,1,1,iBlock)
             State_VGB(Rho_,i,:,:,iBlock) = RhoLeft + &
                  AmplitudeRho*exp(-(x/Width)**2)

             State_VGB(p_,i,:,:,iBlock) = pLeft + Acceleration* &
                  ( RhoLeft*(x - xMinBox) &
                  + AmplitudeRho*Width &
                  *0.5*sqrt(cPi)*(1.0 + erf(x/Width)))
          end do
       else
          ! rho = Rholeft for x < 0
          !     = Rhoright for x > 0
          !     = pLeft + (x-xMinBox)*RhoLeft*Gamma                 for x < 0
          !     = pLeft - xMinBox*RhoLeft*Gamma + x*RhoRight*Gamma  for x > 0

          where(Xyz_DGB(x_,:,:,:,iBlock) <= 0.0)
             State_VGB(p_,:,:,:,iBlock) = pLeft + Acceleration &
                  *(Xyz_DGB(x_,:,:,:,iBlock) - xMinBox)*RhoLeft
          elsewhere
             State_VGB(p_,:,:,:,iBlock) = pLeft + Acceleration &
                  *(Xyz_DGB(x_,:,:,:,iBlock)*RhoRight - xMinBox*RhoLeft)
          end where
       end if

       ! Perturb Ux velocity
       where(abs(Xyz_DGB(x_,:,:,:,iBlock)) < Width)
          State_VGB(RhoUx_,:,:,:,iBlock) = State_VGB(Rho_,:,:,:,iBlock) &
               * Amplitude * cos(cHalfPi*Xyz_DGB(x_,:,:,:,iBlock)/Width)**2 &
               * sin(cTwoPi*(Xyz_DGB(y_,:,:,:,iBlock))/LambdaY)
       endwhere

    case('ShearSquare')
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          x = Xyz_DGB(x_,i,j,k,iBlock)
          y = Xyz_DGB(y_,i,j,k,iBlock)
          z = Xyz_DGB(z_,i,j,k,iBlock)
          if(abs(x) < Width/2 .and. abs(y) < Width/2) &
               State_VGB(Rho_,i,j,k,iBlock) = State_VGB(Rho_,i,j,k,iBlock) &
               + Amplitude
          State_VGB(RhoUx_,i,j,k,iBlock) = State_VGB(Rho_,i,j,k,iBlock)*abs(y)
          ! State_VGB(RhoUy_,i,j,k,iBlock)=State_VGB(Rho_,i,j,k,iBlock)*abs(x)
       end do; end do; end do

    case('AdvectSphere')
       DoAdvectSphere = .true.
       ! This case describes an IC with uniform 1D flow of plasma in a fixed
       ! direction, with no density, pressure gradients, or magnetic field.
       ! A sphere with higher density is embedded in the flow, initially at
       ! xSphereCenterInit,  ySphereCenterInit, zSphereCenterInit.
       ! The density profile within the sphere is given by:
       ! rho = (RhoMax-RhoBackgrnd)* cos^2(pi*r/2 rSphere) + RhoBackgrnd

       ! Convert to normalized units and separate flow components
       UxNo = uBackgrndIo*sin(cDegToRad*FlowAngleTheta)* &
            cos(cDegToRad*FlowAnglePhi)*Io2No_V(UnitU_)
       UyNo = uBackgrndIo*sin(cDegToRad*FlowAngleTheta)* &
            sin(cDegToRad*FlowAnglePhi)*Io2No_V(UnitU_)
       UzNo = uBackgrndIo*cos(cDegToRad*FlowAngleTheta)*Io2No_V(UnitU_)

       RhoBackgrndNo = NumDensBackgrndIo*Io2Si_V(UnitN_)* &
            cProtonMass*Si2No_V(UnitRho_)
       RhoMaxNo       = NumDensMaxIo*Io2Si_V(UnitN_)* &
            cProtonMass*Si2No_V(UnitRho_)
       if (UseUserInputUnitX) then
          rSphere = rSphereIo*Input2SiUnitX*Si2No_V(UnitX_)
          xSphereCenterInit = xSphereCenterInitIo* &
               Input2SiUnitX*Si2No_V(UnitX_)
          ySphereCenterInit = ySphereCenterInitIo* &
               Input2SiUnitX*Si2No_V(UnitX_)
          zSphereCenterInit = zSphereCenterInitIo* &
               Input2SiUnitX*Si2No_V(UnitX_)
       else
          rSphere = rSphereIo
          xSphereCenterInit = xSphereCenterInitIo
          ySphereCenterInit = ySphereCenterInitIo
          zSphereCenterInit = zSphereCenterInitIo
       end if

       ! Start filling in cells (including ghost cells)

       if(DoInitSphere)then
          do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
             x = Xyz_DGB(x_,i,j,k,iBlock)
             y = Xyz_DGB(y_,i,j,k,iBlock)
             z = Xyz_DGB(z_,i,j,k,iBlock)
             r = sqrt((x - xSphereCenterInit)**2 + &
                  (y - ySphereCenterInit)**2 + &
                  (z - zSphereCenterInit)**2)
             if(r <= rSphere)then
                ! inside the sphere
                ! State_VGB(rho_,i,j,k,iBlock) = RhoMaxNo ! for tophat
                State_VGB(rho_,i,j,k,iBlock) = RhoBackgrndNo + &
                     (RhoMaxNo - RhoBackgrndNo)* &
                     (cos(cHalfPi*r/rSphere))**2
             else
                ! in background flow
                State_VGB(rho_,i,j,k,iBlock) = RhoBackgrndNo
             end if
          end do; end do ; end do
       else
          State_VGB(rho_,:,:,:,iBlock) = RhoBackgrndNo
       end if

       ! velocity
       State_VGB(RhoUx_,:,:,:,iBlock) = UxNo*State_VGB(rho_,:,:,:,iBlock)
       State_VGB(RhoUy_,:,:,:,iBlock) = UyNo*State_VGB(rho_,:,:,:,iBlock)
       State_VGB(RhoUz_,:,:,:,iBlock) = UzNo*State_VGB(rho_,:,:,:,iBlock)

       ! pressure
       State_VGB(p_,    :,:,:,iBlock) = pBackgrndIo*Io2No_V(UnitP_)

       if(TypeCoordSystem =='HGC')then
          ! Transform to HGC frame - initially aligned with HGI, only velocity
          ! and/or momentum in X-Y plane should be transformed

          OmegaSun = cTwoPi/(RotationPeriodSun*Si2No_V(UnitT_))
          State_VGB(RhoUx_,:,:,:,iBlock) = State_VGB(RhoUx_,:,:,:,iBlock) &
               + State_VGB(Rho_,:,:,:,iBlock)*OmegaSun*Xyz_DGB(y_,:,:,:,iBlock)

          State_VGB(RhoUy_,:,:,:,iBlock) = State_VGB(RhoUy_,:,:,:,iBlock) &
               - State_VGB(Rho_,:,:,:,iBlock)*OmegaSun*Xyz_DGB(x_,:,:,:,iBlock)

       end if

    case('wave')

       if(DoInitialize)then
          DoWave = .true.
          DoInitialize=.false.

          if(DoIntegrateWave)then
             ! Calculate the finite volume integral of the wave over the cell
             do iVar=1,nVar
                if(iPower_V(iVar) == 1)then
                   if(KxWave_V(iVar) > 0) then
                      HalfWidth = 0.5*KxWave_V(iVar)*CellSize_DB(x_,iBlock)
                      Ampl_V(iVar) = Ampl_V(iVar)*sin(HalfWidth) / HalfWidth
                   end if
                   if(KyWave_V(iVar) > 0) then
                      HalfWidth = 0.5*KyWave_V(iVar)*CellSize_DB(y_,iBlock)
                      Ampl_V(iVar) = Ampl_V(iVar)*sin(HalfWidth) / HalfWidth
                   end if
                   if(KzWave_V(iVar) > 0) then
                      HalfWidth = 0.5*KzWave_V(iVar)*CellSize_DB(z_,iBlock)
                      Ampl_V(iVar) = Ampl_V(iVar)*sin(HalfWidth) / HalfWidth
                   end if
                end if
             end do
          end if

          PrimInit_V = ShockLeft_V

          if(UseUserInputUnitX)then
             ! Convert to normalized units of length
             Width_V  = Width_V*rSun*Si2No_V(UnitX_)
             KxWave_V = KxWave_V/(Input2SiUnitX*Si2No_V(UnitX_))
             KyWave_V = KyWave_V/(Input2SiUnitX*Si2No_V(UnitX_))
             KzWave_V = KzWave_V/(Input2SiUnitX*Si2No_V(UnitX_))
          end if

          if(ShockSlope /= 0.0)then

             CosSlope = 1.0/sqrt(1+ShockSlope**2)
             SinSlope = ShockSlope*CosSlope

             State_V = Ampl_V

             do iVectorVar=1,nVectorVar
                iVarX = iVectorVar_I(iVectorVar)
                iVarY = iVarX + 1
                ! Make sure that the X and Y components of vector variables
                ! have consistent wave parameters
                if(Width_V(iVarX) > 0.0 .and. Width_V(iVarY) == 0.0) &
                     call copy_wave(iVarX, iVarY)
                if(Width_V(iVarY) > 0.0 .and. Width_V(iVarX) == 0.0) &
                     call copy_wave(iVarY, iVarX)

                ! Rotate amplitudes with the angle of the shock slope
                Ampl_V(iVarX) = &
                     CosSlope*State_V(iVarX) - SinSlope*State_V(iVarY)
                Ampl_V(iVarY) = &
                     SinSlope*State_V(iVarX) + CosSlope*State_V(iVarY)
             end do

             KxTemp_V= KxWave_V
             KyTemp_V= KyWave_V
             KxWave_V= CosSlope*KxTemp_V - SinSlope*KyTemp_V
             KyWave_V= SinSlope*KxTemp_V + CosSlope*KyTemp_V

             State_V = ShockLeft_V
             PrimInit_V(Ux_) = CosSlope*State_V(Ux_) - SinSlope*State_V(Uy_)
             PrimInit_V(Uy_) = SinSlope*State_V(Ux_) + CosSlope*State_V(Uy_)
             PrimInit_V(Bx_) = CosSlope*State_V(Bx_) - SinSlope*State_V(By_)
             PrimInit_V(By_) = SinSlope*State_V(Bx_) + CosSlope*State_V(By_)

             ! write(*,*) &
             !    'KxWave_V(Bx_:Bz_),KyWave_V(Bx_:Bz_),KzWave_V(Bx_:Bz_)=',&
             !     KxWave_V(Bx_:Bz_),KyWave_V(Bx_:Bz_),KzWave_V(Bx_:Bz_)
             ! write(*,*)'       Ampl_V(Bx_:Bz_) =',       Ampl_V(Bx_:Bz_)
             ! write(*,*)'      Phase_V(Bx_:Bz_) =',       Phase_V(Bx_:Bz_)

          end if
       end if

       ! Convert momentum to velocity
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          State_VGB(iUx_I,i,j,k,iBlock) = State_VGB(iRhoUx_I,i,j,k,iBlock) &
               / State_VGB(iRho_I,i,j,k,iBlock)
          State_VGB(iUy_I,i,j,k,iBlock) = State_VGB(iRhoUy_I,i,j,k,iBlock) &
               / State_VGB(iRho_I,i,j,k,iBlock)
          State_VGB(iUz_I,i,j,k,iBlock) = State_VGB(iRhoUz_I,i,j,k,iBlock) &
               / State_VGB(iRho_I,i,j,k,iBlock)
       end do; end do; end do

       do iVar=1,nVar
          if(iPower_V(iVar) <= 0)then
             ! iPower==0: Tophat
             ! iPower< 0: Gaussian profile multiplied by smoother:
             !    ampl*exp(-(r/d)^2)*(cos(0.25*pi*r/d))^6 for r/d < 2
             do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
                x = Xyz_DGB(x_,i,j,k,iBlock) - x_V(iVar)
                y = Xyz_DGB(y_,i,j,k,iBlock) - y_V(iVar)
                z = Xyz_DGB(z_,i,j,k,iBlock) - z_V(iVar)
                if(IsPeriodic_D(1))then
                   if(x > +(xMaxBox-xMinBox)/2) x = x - (xMaxBox-xMinBox)
                   if(x < -(xMaxBox-xMinBox)/2) x = x + (xMaxBox-xMinBox)
                end if
                if(IsPeriodic_D(2))then
                   if(y > +(yMaxBox-yMinBox)/2) y = y - (yMaxBox-yMinBox)
                   if(y < -(yMaxBox-yMinBox)/2) y = y + (yMaxBox-yMinBox)
                end if
                if(IsPeriodic_D(3))then
                   if(z > +(zMaxBox-zMinBox)/2) z = z - (zMaxBox-zMinBox)
                   if(z < -(zMaxBox-zMinBox)/2) z = z + (zMaxBox-zMinBox)
                end if
                r2 =   (KxWave_V(iVar)*x)**2 + (KyWave_V(iVar)*y)**2 &
                     + (KzWave_V(iVar)*z)**2

                if(iPower_V(iVar) == 0)then
                   ! Top hat
                   if(r2 > 1.0) CYCLE
                   State_VGB(iVar,i,j,k,iBlock) = State_VGB(iVar,i,j,k,iBlock)&
                        + Ampl_V(iVar)
                else
                   ! Gaussian smoothed with cos^6
                   if(r2 > 4.0) CYCLE
                   r  = sqrt(r2)
                   State_VGB(iVar,i,j,k,iBlock) = State_VGB(iVar,i,j,k,iBlock)&
                        + Ampl_V(iVar)*cos(cPi*0.25*r)**6*exp(-r2)
                end if
             end do; end do; end do
          else
             ! cos^n profile
             do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
                if(KxWave_V(iVar) > 0.0)then
                   if(abs(Xyz_DGB(x_,i,j,k,iBlock) &
                        + ShockSlope*Xyz_DGB(y_,i,j,k,iBlock)) &
                        > Width_V(iVar) ) CYCLE
                elseif(KyWave_V(iVar) > 0.0)then
                   if(abs(Xyz_DGB(y_,i,j,k,iBlock)) > Width_V(iVar) ) CYCLE
                elseif(KzWave_V(iVar) > 0.0)then
                   if(abs(Xyz_DGB(z_,i,j,k,iBlock)) > Width_V(iVar) ) CYCLE
                end if

                State_VGB(iVar,i,j,k,iBlock) =        &
                     State_VGB(iVar,i,j,k,iBlock)          &
                     + Ampl_V(iVar)*cos(Phase_V(iVar)      &
                     + KxWave_V(iVar)*Xyz_DGB(x_,i,j,k,iBlock)  &
                     + KyWave_V(iVar)*Xyz_DGB(y_,i,j,k,iBlock)  &
                     + KzWave_V(iVar)*Xyz_DGB(z_,i,j,k,iBlock))**iPower_V(iVar)
             end do; end do; end do
          end if
       end do

       ! Convert velocity to momentum
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          State_VGB(iRhoUx_I,i,j,k,iBlock) = State_VGB(iUx_I,i,j,k,iBlock) &
               *State_VGB(iRho_I,i,j,k,iBlock)
          State_VGB(iRhoUy_I,i,j,k,iBlock) = State_VGB(iUy_I,i,j,k,iBlock) &
               *State_VGB(iRho_I,i,j,k,iBlock)
          State_VGB(iRhoUz_I,i,j,k,iBlock) = State_VGB(iUz_I,i,j,k,iBlock) &
               *State_VGB(iRho_I,i,j,k,iBlock)
       end do; end do; end do

       if(EntropyConstant > 0.0)then
          do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
             State_VGB(iP_I,i,j,k,iBlock) = &
                  EntropyConstant*State_VGB(iRho_I,i,j,k,iBlock)**Gamma_I
          end do; end do; end do
          ! Make sure the pressure gets integrated below
          ! This only works if the velocity is zero, so e = p/(Gamma-1)
          iPower_V(iP_I) = 2*iPower_V(iRho_I)
       end if

       if(DoIntegrateWave)then
          ! Convert to cell averages
          allocate(State_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK))
          do iVar=1,nVar
             ! Tophat should not be integrated.
             ! Pure cosine can be integrated analytically
             if(abs(iPower_V(iVar)) <= 1) CYCLE

             State_G = State_VGB(iVar,:,:,:,iBlock)
             do k=1,nK; do j=1,nJ; do i=1,nI
                Laplace =                   State_G(i-1,j,k) + State_G(i+1,j,k)
                if(nJ > 1)Laplace=Laplace + State_G(i,j-1,k) + State_G(i,j+1,k)
                if(nK > 1)Laplace=Laplace + State_G(i,j,k-1) + State_G(i,j,k+1)
                State_VGB(iVar,i,j,k,iBlock) = &
                     (1 - nDim/12.0)*State_VGB(iVar,i,j,k,iBlock) + Laplace/24
             end do; end do; end do
          end do
          deallocate(State_G)
       end if

       if(UseEfield .and. DoCorrectElectronFluid) &
            call correct_electronfluid_efield(State_VGB(:,:,:,:,iBlock), &
            1, nI, 1, nJ, 1, nK, iBlock, DoHallCurrentIn=.true.,         &
            DoGradPeIn=.false., DoCorrectEfieldIn=DoCorrectEfield)

    case('GEM')
       ! write(*,*)'GEM problem set up'
       State_VGB(Bx_,:,:,:,iBlock) = B0*tanh(Xyz_DGB(y_,:,:,:,iBlock)/Lambda0)

       ! Modify pressure(s) to balance magnetic pressure
       if(UseElectronPressure) then
          ! Distribute the correction proportionally between electrons and ions
          State_VGB(Pe_,:,:,:,iBlock) = ShockLeft_V(Pe_)*(1.0 &
               + 0.5*(B0**2 - State_VGB(Bx_,:,:,:,iBlock)**2) &
               /(ShockLeft_V(Pe_) + ShockLeft_V(p_)))

          State_VGB(p_,:,:,:,iBlock) = ShockLeft_V(p_)*(1.0 &
               + 0.5*(B0**2 - State_VGB(Bx_,:,:,:,iBlock)**2) &
               /(ShockLeft_V(Pe_) + ShockLeft_V(p_)))
       else
          State_VGB(p_,:,:,:,iBlock)  = ShockLeft_V(p_) &
               + 0.5*(B0**2 - State_VGB(Bx_,:,:,:,iBlock)**2)
       end if

       if(UseAnisoPressure) &
                                ! parallel pressure
            State_VGB(Ppar_,:,:,:,iBlock) = ShockLeft_V(Ppar_)*(1.0 &
            + 0.5*(B0**2 - State_VGB(Bx_,:,:,:,iBlock)**2) &
            /ShockLeft_V(p_))

       if(UseAnisoPe) &
                                ! parallel pressure
            State_VGB(Pepar_,:,:,:,iBlock) = ShockLeft_V(Pepar_)*(1.0 &
            + 0.5*(B0**2 - State_VGB(Bx_,:,:,:,iBlock)**2) &
            /ShockLeft_V(Pe_))

       State_VGB(rho_,:,:,:,iBlock)= State_VGB(p_,:,:,:,iBlock)/Tp

       ! Size of the box
       Lx = xMaxBox - xMinBox
       Ly = yMaxBox - yMinBox
       ! set intial perturbation
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          x = Xyz_DGB(x_,i,j,k,iBlock)
          y = Xyz_DGB(y_,i,j,k,iBlock)
          ! Apply perturbation on x
          x = x + GemEps*(x-xMinBox)*(x-xMaxBox)
          State_VGB(Bx_,i,j,k,iBlock) = State_VGB(Bx_,i,j,k,iBlock) &
               - Az* cPi/Ly *cos(cTwoPi*x/Lx) * sin(cPi*y/Ly) * (1+2*GemEps*x)
          State_VGB(By_,i,j,k,iBlock) = State_VGB(By_,i,j,k,iBlock) &
               + Az* cTwoPi/Lx * sin(cTwoPi*x/Lx) * cos(cPi*y/Ly)
       end do; end do; end do

    case('PowerProfile')
       ! Generalized power profile:
       ! state = shockleft + c1*x^p1 + c2*y^p2 + c3*z^p3
       do iVar=1,nVar
          if(.not.IsPowerProfile_V(iVar)) CYCLE
          ! set up the power profile for iVar
          do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI

             ! Convert momentum to velocity
             State_VGB(iUx_I,i,j,k,iBlock) = State_VGB(iRhoUx_I,i,j,k,iBlock) &
                  /State_VGB(iRho_I,i,j,k,iBlock)
             State_VGB(iUy_I,i,j,k,iBlock) = State_VGB(iRhoUy_I,i,j,k,iBlock) &
                  /State_VGB(iRho_I,i,j,k,iBlock)
             State_VGB(iUz_I,i,j,k,iBlock) = State_VGB(iRhoUz_I,i,j,k,iBlock) &
                  /State_VGB(iRho_I,i,j,k,iBlock)

             State_VGB(iVar,i,j,k,iBlock) = ShockLeft_V(iVar) &
                  + CoeffX_V(iVar)*Xyz_DGB(x_,i,j,k,iBlock)**nPowerX_V(iVar) &
                  + CoeffY_V(iVar)*Xyz_DGB(y_,i,j,k,iBlock)**nPowerY_V(iVar) &
                  + CoeffZ_V(iVar)*Xyz_DGB(z_,i,j,k,iBlock)**nPowerZ_V(iVar)

             ! Convert velocity to momentum
             State_VGB(iRhoUx_I,i,j,k,iBlock) = State_VGB(iUx_I,i,j,k,iBlock) &
                  *State_VGB(iRho_I,i,j,k,iBlock)
             State_VGB(iRhoUy_I,i,j,k,iBlock) = State_VGB(iUy_I,i,j,k,iBlock) &
                  *State_VGB(iRho_I,i,j,k,iBlock)
             State_VGB(iRhoUz_I,i,j,k,iBlock) = State_VGB(iUz_I,i,j,k,iBlock) &
                  *State_VGB(iRho_I,i,j,k,iBlock)

          end do; end do; end do
       end do

    case('PipeFlow')
       State_VGB(:,:,:,:,iBlock)      = 0.0
       State_VGB(Rho_,:,:,:,iBlock)   = 1.0
       State_VGB(p_,:,:,:,iBlock)     = 1.0 &
            - 0.1*(Xyz_DGB(x_,:,:,:,iBlock)-CoordMin_D(x_))/CoordMax_D(x_)
       do k=1,nK; do j=1,nJ; do i=MinI,MaxI
          ViscoCoeff = 1.0! Viscosity_factor(0,i,j,k,iBlock)
          if(ViscoCoeff > 0.0) then
             State_VGB(RhoUx_,i,j,k,iBlock) = &
                  0.5*(Xyz_DGB(y_,i,j,k,iBlock)**2 -CoordMax_D(y_)**2)*&
                  (State_VGB(p_,i,j,k,iBlock) -1.0) &
                  /(ViscoCoeff*(Xyz_DGB(x_,i,j,k,iBlock)-CoordMin_D(x_)))
          else
             State_VGB(RhoUx_,i,j,k,iBlock) = 0.0025
          end if
       end do; end do; end do

    case('ResistivityGaussian')
       DoResistivityGaussian = .true.

       State_VGB(Rho_,:,:,:,iBlock) = 1.0
       State_VGB(p_,:,:,:,iBlock)   = 1.0
       State_VGB(RhoUx_:RhoUz_,:,:,:,iBlock) = 0.0
       do k=1,nK; do j=1,nJ; do i=1,nI
          call get_gaussian_field(i, j, k, iBlock, &
               State_VGB(Bx_:Bz_,i,j,k,iBlock))
       end do; end do; end do

    case default
       if(iProc==0) call stop_mpi( &
            'user_set_ics: undefined user problem='//NameProblem)

    end select

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_ics
  !============================================================================
  subroutine copy_wave(iVar, jVar)

    ! Copy wave parameters from iVar to jVar for rotated problems

    integer, intent(in):: iVar, jVar
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'copy_wave'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    Width_V(jVar)  = Width_V(iVar)
    KxWave_V(jVar) = KxWave_V(iVar)
    KyWave_V(jVar) = KyWave_V(iVar)
    KzWave_V(jVar) = KzWave_V(iVar)
    Phase_V(jVar)  = Phase_V(iVar)
    iPower_V(jVar) = iPower_V(iVar)

    call test_stop(NameSub, DoTest)
  end subroutine copy_wave
  !============================================================================
  subroutine user_set_plot_var(iBlock,NameVar,IsDimensional,&
       PlotVar_G, PlotVarBody, UsePlotVarBody,&
       NameTecVar, NameTecUnit, NameIdlUnit, IsFound)

    use ModMain, ONLY: TypeCoordSystem
    use ModPhysics, ONLY: NameTecUnit_V, NameIdlUnit_V, No2Io_V, No2Si_V, &
         Si2No_V, UnitRho_, UnitP_, UnitU_,  UnitT_, Gamma0
    use ModAdvance, ONLY: State_VGB
    use ModVarIndexes, ONLY: RhoUx_, RhoUy_, RhoUz_, p_, Rho_
    use ModConst, ONLY: RotationPeriodSun
    use ModNumConst, ONLY: cTwoPi
    use BATL_lib, ONLY: &
         nI, nJ, nK, Xyz_DGB, CellFace_DB, CellFace_DFB, FaceNormal_DDFB

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

    real:: &
         RhoExact_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK), &
         RhoError_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)
    real    :: FlowSpeedCell, Pressure, Density, OmegaSun
    real    :: RhoU_D(3), B_D(3), u_D(3)
    integer :: i, j, k, iDir
    logical:: DoTest

    character(len=*), parameter:: NameSub = 'user_set_plot_var'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    IsFound = .true.

    if(DoCalcAnalytic .and. NameProblem == 'AdvectSphere' ) &
         call calc_analytic_sln_sphere(iBlock,RhoExact_G,RhoError_G)

    select case(NameVar)
    case('rhoexact')
       if (.not. DoCalcAnalytic) then
          write(*,*) NameSub,': cannot calculate ',NameVar
          call CON_stop('Set #ANALYTIC to T on PARAM.in file')
       end if
       PlotVar_G = RhoExact_G*No2Io_V(UnitRho_)
       NameTecVar = 'RhoExact'
       NameTecUnit = NameTecUnit_V(UnitRho_)
       NameIdlUnit = NameIdlUnit_V(UnitRho_)
    case('rhoerr')
       if (.not. DoCalcAnalytic) then
          write(*,*) NameSub,': cannot calculate ',NameVar
          call CON_stop('Set #ANALYTIC to T on PARAM.in file')
       end if
       PlotVar_G = RhoError_G*No2Io_V(UnitRho_)
       NameTecVar = 'RhoError'
       NameTecUnit = NameTecUnit_V(UnitRho_)
       NameIdlUnit = NameIdlUnit_V(UnitRho_)

    case('mach')
       ! plot Mach number
       OmegaSun = cTwoPi/(RotationPeriodSun*Si2No_V(UnitT_))

       do k=MinK,MaxK ; do j=MinJ,MaxJ ; do i=MinI,MaxI
          Pressure = State_VGB(p_,i,j,k,iBlock)*No2Si_V(UnitP_)
          Density = State_VGB(Rho_,i,j,k,iBlock)*No2Si_V(UnitRho_)

          if (TypeCoordSystem =='HGC' .or. TypeCoordSystem =='hgc') then
             RhoU_D(1) = State_VGB(RhoUx_,i,j,k,iBlock) &
                  - State_VGB(Rho_,i,j,k,iBlock) &
                  *OmegaSun*Xyz_DGB(y_,i,j,k,iBlock)

             RhoU_D(2) = State_VGB(RhoUy_,i,j,k,iBlock) &
                  + State_VGB(Rho_,i,j,k,iBlock) &
                  *OmegaSun*Xyz_DGB(x_,i,j,k,iBlock)

          elseif (TypeCoordSystem == 'HGI' .or. TypeCoordSystem == 'hgi') then
             RhoU_D(1) = State_VGB(RhoUx_,i,j,k,iBlock)
             RhoU_D(2) = State_VGB(RhoUy_,i,j,k,iBlock)
          end if

          RhoU_D(3) = State_VGB(RhoUz_,i,j,k,iBlock)
          FlowSpeedCell = No2Si_V(UnitU_)*sqrt(sum(RhoU_D**2)) &
               /State_VGB(Rho_,i,j,k,iBlock)

          PlotVar_G(i,j,k) = FlowSpeedCell/sqrt(Gamma0*Pressure/Density)
       end do; end do ; end do
       NameTecVar = 'Mach'
       NameTecUnit = '--'
       NameIdlUnit = '--'

    case('bxexact')
       if(DoResistivityGaussian)then
          do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
             call get_gaussian_field(i, j, k, iBlock, B_D)
             PlotVar_G(i,j,k) = B_D(1)
          end do; end do ; end do
       end if

    case('divun')
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          PlotVar_G(i,j,k) = &
               Un_XB(i+1,j,k,iBlock) - Un_XB(i,j,k,iBlock) + &
               Un_YB(i,j+1,k,iBlock) - Un_YB(i,j,k,iBlock) + &
               Un_ZB(i,j,k+1,iBlock) - Un_ZB(i,j,k,iBlock)
       end do; end do; end do
    case('unx', 'uny', 'unz')
       select case(NameVar)
       case('unx')
          iDir = 1
       case('uny')
          iDir = 2
       case('unz')
          iDir = 3
       end select
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(IsCartesian)then
             ! Average 2 faces to cell center.
             ! Divide by face area, because Un_*B has area in it.
             u_D(1) = 0.5*(Un_XB(i+1,j,k,iBlock) + Un_XB(i,j,k,iBlock)) &
                  /max(1e-30, CellFace_DB(1,iBlock))
             u_D(2) = 0.5*(Un_YB(i,j+1,k,iBlock) + Un_YB(i,j,k,iBlock)) &
                  /max(1e-30, CellFace_DB(2,iBlock))
             u_D(3) = 0.5*(Un_ZB(i,j,k+1,iBlock) + Un_ZB(i,j,k,iBlock)) &
                  /max(1e-30, CellFace_DB(3,iBlock))
          else
             ! Add up 6 face normal velocities averaged to the cell center
             ! Divide by face squared, because of FaceNormal and Un definition
             u_D = 0.5* &
                  Un_XB(i+1,j,k,iBlock)*FaceNormal_DDFB(:,1,i+1,j,k,iBlock) &
                  /max(1e-30, CellFace_DFB(1,i+1,j,k,iBlock)**2)
             u_D = u_D + 0.5* &
                  Un_XB(i,j,k,iBlock)*FaceNormal_DDFB(:,1,i,j,k,iBlock) &
                  /max(1e-30, CellFace_DFB(1,i,j,k,iBlock)**2)

             u_D = u_D + 0.5* &
                  Un_YB(i,j+1,k,iBlock)*FaceNormal_DDFB(:,2,i,j+1,k,iBlock) &
                  /max(1e-30, CellFace_DFB(2,i,j+1,k,iBlock)**2)
             u_D = u_D + 0.5* &
                  Un_YB(i,j,k,iBlock)*FaceNormal_DDFB(:,2,i,j,k,iBlock) &
                  /max(1e-30, CellFace_DFB(2,i,j,k,iBlock)**2)

             u_D = u_D + 0.5* &
                  Un_ZB(i,j,k+1,iBlock)*FaceNormal_DDFB(:,3,i,j,k+1,iBlock) &
                  /max(1e-30, CellFace_DFB(3,i,j,k+1,iBlock)**2)
             u_D = u_D + 0.5* &
                  Un_ZB(i,j,k,iBlock)*FaceNormal_DDFB(:,3,i,j,k,iBlock) &
                  /max(1e-30, CellFace_DFB(3,i,j,k,iBlock)**2)
          end if
          PlotVar_G(i,j,k) = u_D(iDir)
       end do; end do; end do

    case default
       IsFound = .false.
    end select

    call test_stop(NameSub, DoTest, iBlock)
  contains
    !==========================================================================
    subroutine calc_analytic_sln_sphere(iBlock,RhoExact_G,RhoError_G)

      use ModMain, ONLY: tSimulation, TypeCoordSystem
      use ModGeometry, ONLY: Xyz_DGB
      use ModNumConst, ONLY: cHalfPi, cTwoPi
      use ModConst, ONLY: RotationPeriodSun
      use ModAdvance, ONLY: State_VGB
      use ModVarIndexes, ONLY: Rho_
      use ModPhysics, ONLY: Si2No_V,UnitT_

      integer, intent(in)  :: iBlock
      real,    intent(out) :: &
           RhoExact_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK), &
           RhoError_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)

      real    :: x, y, z, t
      real    :: xSphereCenter, ySphereCenter, zSphereCenter
      real    :: rFromCenter, rSphereCenter
      real    :: PhiSphereCenterInertial, PhiSphereCenterRotating
      integer :: i, j, k

      ! Find current location of sphere center
      character(len=*), parameter:: NameSub = 'calc_analytic_sln_sphere'
      !------------------------------------------------------------------------
      t = tSimulation*Si2No_V(UnitT_)
      xSphereCenter = xSphereCenterInit + UxNo*t
      ySphereCenter = ySphereCenterInit + UyNo*t
      zSphereCenter = zSphereCenterInit + UzNo*t

      ! transform if rotating frame
      if (TypeCoordSystem =='HGC') then
         rSphereCenter = sqrt(xSphereCenter**2 + ySphereCenter**2)
         PhiSphereCenterInertial = atan2(ySphereCenter, xSphereCenter)
         PhiSphereCenterRotating = PhiSphereCenterInertial - &
              tSimulation*cTwoPi/RotationPeriodSun
         xSphereCenter = rSphereCenter*cos(PhiSphereCenterRotating)
         ySphereCenter = rSphereCenter*sin(PhiSphereCenterRotating)
      end if

      do k=MinK,MaxK ; do j= MinJ,MaxJ ; do i= MinI,MaxI

         x = Xyz_DGB(x_,i,j,k,iBlock)
         y = Xyz_DGB(y_,i,j,k,iBlock)
         z = Xyz_DGB(z_,i,j,k,iBlock)

         ! Chcek if this cell is inside the sphere
         rFromCenter = sqrt((x-xSphereCenter)**2 + (y-ySphereCenter)**2 + &
              (z-zSphereCenter)**2)
         if (rFromCenter <= rSphere) then
            RhoExact_G(i,j,k) = RhoBackgrndNo + (RhoMaxNo - RhoBackgrndNo)* &
                 cos(cHalfPi*rFromCenter/rSphere)**2
         else
            RhoExact_G(i,j,k) = RhoBackgrndNo
         end if
      end do; end do ; end do

      RhoError_G = RhoExact_G - State_VGB(Rho_,:,:,:,iBlock)

    end subroutine calc_analytic_sln_sphere
    !==========================================================================
  end subroutine user_set_plot_var
  !============================================================================
  subroutine user_get_log_var(VarValue, TypeVar, Radius)

    use ModMain, ONLY: nI, nJ, nK, nBlock, Unused_B
    use ModAdvance, ONLY: By_, State_VGB
    use ModGeometry, ONLY: zMaxBox, zMinBox
    use BATL_lib, ONLY: CellFace_DB, CellSize_DB, Xyz_DGB

    real, intent(out)            :: VarValue
    character (len=*), intent(in):: TypeVar
    real, intent(in), optional :: Radius

    character (len=*), parameter :: Name='user_get_log_var'

    integer :: k1, k2, iBlock
    real:: yMinBox, yMaxBox, Dy1, Dy2, HalfInvWidth, Flux
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_get_log_var'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    HalfInvWidth = 0.5/(zMaxBox-zMinBox)
    VarValue=0.0
    select case(TypeVar)
    case('byflux')
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          yMinBox = Xyz_DGB(y_,1,0,1,iBlock)
          yMaxBox = Xyz_DGB(y_,1,nJ+1,1,iBlock)

          if(yMinBox*yMaxBox > 0) CYCLE
          k1 = -yMinBox/CellSize_DB(y_,iBlock)
          k2 = k1 + 1
          Dy1 = abs(Xyz_DGB(y_,1,k1,1,iBlock))/CellSize_DB(y_,iBlock)
          Dy2 = 1.0 - Dy1
          Flux = CellFace_DB(2,iBlock)*HalfInvWidth* &
               ( Dy2*sum(abs(State_VGB(By_,1:nI,k1,1:nK,iBlock))) &
               + Dy1*sum(abs(State_VGB(By_,1:nI,k2,1:nK,iBlock))))
          if(k1 == 0 .or. k2 == nJ + 1) Flux = 0.5*Flux
          VarValue = VarValue + Flux
       end do
    case default
       call stop_mpi('Unknown user logvar='//TypeVar)
    end select
    call test_stop(NameSub, DoTest)
  end subroutine user_get_log_var
  !============================================================================
  subroutine user_get_b0(x, y, z, B0_D)

    real, intent(in) :: x, y, z
    real, intent(out):: B0_D(3)

    !--------------------------------------------------------------------------
    B0_D = [0.2, 0.3, 0.4]

  end subroutine user_get_b0
  !============================================================================
  subroutine user_set_face_boundary(FBC)

    use ModMain, ONLY: x_, y_, z_, FaceBCType

    type(FaceBCType) :: FBC

    integer :: iVar
    real :: Dx
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_face_boundary'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, FBC%iBlockBc)

    Dx = Velocity*FBC%TimeBc

    do iVar=1,nVar
       ! Both of these are primitive variables
       FBC%VarsGhostFace_V(iVar) = PrimInit_V(iVar)         &
            + Ampl_V(iVar)*cos(Phase_V(iVar)            &
            + KxWave_V(iVar)*(FBC%FaceCoords_D(x_) - Dx)    &
            + KyWave_V(iVar)*FBC%FaceCoords_D(y_)           &
            + KzWave_V(iVar)*FBC%FaceCoords_D(z_))
    end do

    call test_stop(NameSub, DoTest, FBC%iBlockBc)

  end subroutine user_set_face_boundary
  !============================================================================
  subroutine user_set_cell_boundary(iBlock, iSide, TypeBc, IsFound)

    use ModImplicit, ONLY: StateSemi_VGB
    use ModSize, ONLY: nI, nJ, nK, x_, y_, z_
    use ModPhysics, ONLY: Si2No_V, Io2Si_V,&
         Io2No_V, UnitRho_, UnitU_, UnitP_, UnitN_,&
         UnitT_, ShockLeft_V, ShockRight_V,&
         ShockSlope, ShockPosition
    use ModNumconst, ONLY: cTwoPi, cDegToRad
    use ModConst, ONLY: cProtonMass, RotationPeriodSun
    use ModMain, ONLY: tSimulation, TypeCoordSystem
    use ModAdvance, ONLY: nVar, Rho_, Ux_, Uz_, RhoUx_, RhoUz_, State_VGB,p_
    use ModGeometry, ONLY: TypeGeometry, &
         xMinBox, xMaxBox, yMinBox, yMaxBox, zMinBox, zMaxBox, r_GB
    use ModVarIndexes
    use BATL_lib, ONLY: CoordMax_D, CoordMin_D, Xyz_DGB, CoordMin_D, CoordMax_D

    integer, intent(in)          :: iBlock, iSide
    character(len=*), intent(in) :: TypeBc
    logical, intent(out)         :: IsFound

    integer :: i, j, k, iVar
    real    :: Dx, x, y, z, r, rMin, rMax
    real    :: OmegaSun, Phi, UxAligned, UyAligned
    real    :: ViscoCoeff

    ! variables for shockramp
    real   :: x0
    real   :: SinSlope, CosSlope
    real   :: ShockRampLeft_I(Rho_: p_), ShockRampRight_I(Rho_: p_)
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_cell_boundary'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    !    DoTest = iBlock == iBlockTest
    IsFound = .true.

    if(DoShockramp)then
       SinSlope = ShockSlope/sqrt(1 + ShockSlope**2)
       CosSlope =         1/sqrt(1 + ShockSlope**2)

       ShockRampLeft_I = ShockLeft_V(Rho_:p_)
       ! Project the velocity in the shock front reference frame into the frame
       ! for computation.
       ShockRampLeft_I(Ux_) = ShockLeft_V(Ux_)*CosSlope
       ShockRampLeft_I(Uy_) = ShockLeft_V(Ux_)*SinSlope

       ! The velocity is zero in the right of shock.
       ShockRampRight_I = ShockRight_V(Rho_:p_)
       select case (iSide)
       case(1)
          ! inflow BC for x=0
          do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, 0
             State_VGB(:,i,j,k,iBlock) = ShockRampLeft_I
             ! convert velocity into momentum.
             State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) = &
                  State_VGB(Ux_:Uz_,i,j,k,iBlock)&
                  *State_VGB(Rho_,i,j,k,iBlock)
          end do; end do; end do
       case(3)
          do k = MinK, MaxK; do j = MinJ, 0; do i = MinI, MaxI
             if (Xyz_DGB(x_,i,j,k,iBlock) <=  ShockPosition) then
                ! upstream (fixed) BC for the bottom ahead of "ShockPosition"
                State_VGB(:,i,j,k,iBlock) = ShockRampLeft_I
                State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) = &
                     State_VGB(Ux_:Uz_,i,j,k,iBlock) &
                     *State_VGB(Rho_,i,j,k,iBlock)
             else
                ! reflective BC beyond ShockPosition for the bottom
                State_VGB(:,i,j,k,iBlock) = State_VGB(:,i,1-j,k,iBlock)
                State_VGB(RhoUy_,i,j,k,iBlock) = &
                     -State_VGB(RhoUy_,i,j,k,iBlock)
             end if

          end do; end do; end do
       case(4)
          ! x0 is the shock position at y=1 at the current simulation time.
          ! The analytic shock speed is 20 along X.
          x0 = ShockPosition - ShockSlope*(1 + 20*tSimulation)
          do k = MinK, MaxK; do j = nJ+1, MaxJ; do i = MinI, MaxI
             if (Xyz_DGB(x_,i,j,k,iBlock) <= x0) then
                ! Upstream condition
                State_VGB(:,i,j,k,iBlock) = ShockRampLeft_I
             else
                ! Downstream condition
                State_VGB(:,i,j,k,iBlock) = ShockRampRight_I
             end if
             State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) = &
                  State_VGB(Ux_:Uz_,i,j,k,iBlock)&
                  *State_VGB(Rho_,i,j,k,iBlock)
          end do; end do; end do
       end select
    end if

    if(DoResistivityGaussian)then
       select case(TypeBc)
       case('usersemi')
          select case(iSide)
          case(2)
             do j=MinJ,MaxJ
                call get_gaussian_field(nI+1, j, 1, iBlock, &
                     StateSemi_VGB(1:3,nI+1,j,1,iBlock))
             end do
          end select
       end select

       RETURN
    end if

    Dx = Velocity*tSimulation

    if(TypeGeometry=='spherical_lnr')then
       rMin = exp(CoordMin_D(1)); rMax = exp(CoordMax_D(1));
    else
       rMin = CoordMin_D(1); rMax = CoordMax_D(1);
    end if

    if (DoWave) then
       do i = MinI, MaxI; do j = MinJ, MaxJ; do k = MinK, MaxK
          x = Xyz_DGB(x_,i,j,k,iBlock)
          y = Xyz_DGB(y_,i,j,k,iBlock)
          z = Xyz_DGB(z_,i,j,k,iBlock)
          r = r_GB(i,j,k,iBlock)
          r = alog(r)

          if(  xMinBox < x .and. x < xMaxBox .and. &
               yMinBox < y .and. y < yMaxBox .and. &
               zMinBox < z .and. z < zMaxBox .and. &
               r > rMin .and. r < rMax) CYCLE

          do iVar=1,nVar
             ! Both of these are primitive variables
             State_VGB(iVar,i,j,k,iBlock) = PrimInit_V(iVar) &
                  + Ampl_V(iVar)*cos(Phase_V(iVar)               &
                  + KxWave_V(iVar)*(x - Dx)                      &
                  + KyWave_V(iVar)*y                             &
                  + KzWave_V(iVar)*z)
          end do
          State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) = &
               State_VGB(Ux_:Uz_,i,j,k,iBlock)*&
               State_VGB(Rho_,i,j,k,iBlock)

       end do; end do; end do
    end if

    if(DoPipeFlow) then

       select case(iSide)
       case(1)
          State_VGB(:,MinK:0,:,:,iBlock)    = 0.0
          State_VGB(Rho_,MinK:0,:,:,iBlock) = 1.0
          State_VGB(p_,MinI:0,:,:,iBlock)   = 1.0 - 0.1*&
               (Xyz_DGB(x_,MinI:0,:,:,iBlock) - CoordMin_D(x_))/CoordMax_D(x_)
          do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,0
             ViscoCoeff = 1.0 ! Viscosity_factor(0,i,j,k,iBlock)
             if( ViscoCoeff > 0.0 )then
                State_VGB(RhoUx_,i,j,k,iBlock) = 0.5*&
                     (Xyz_DGB(y_,i,j,k,iBlock)**2 - CoordMax_D(y_)**2)*&
                     (State_VGB(p_,i,j,k,iBlock) - 1.0)/&
                     (ViscoCoeff*(Xyz_DGB(x_,i,j,k,iBlock) - CoordMin_D(x_)))
             else
                State_VGB(RhoUx_,i,j,k,iBlock) = 0.0025
             end if

          end do; end do; end do
       case(2)
          State_VGB(:,nI+1,:,:,iBlock) = State_VGB(:,nI,:,:,iBlock)
          State_VGB(:,nI+2,:,:,iBlock) = State_VGB(:,nI,:,:,iBlock)
          State_VGB(p_,nI+1:MaxI,:,:,iBlock)     = 1.0 - 0.1*&
               (Xyz_DGB(x_,nI+1:MaxI,:,:,iBlock)-CoordMin_D(x_))/CoordMax_D(x_)
       case(3)
          State_VGB(:,:,0,:,iBlock) = State_VGB(:,:,1,:,iBlock)
          State_VGB(:,:,-1,:,iBlock) = State_VGB(:,:,1,:,iBlock)
          State_VGB(RhoUx_:RhoUz_,:,-1:0,:,iBlock) = 0.0
       case(4)
          State_VGB(:,:,nJ+1,:,iBlock) = State_VGB(:,:,nJ,:,iBlock)
          State_VGB(:,:,nJ+2,:,iBlock) = State_VGB(:,:,nJ,:,iBlock)
          State_VGB(RhoUx_:RhoUz_,:,nJ+1:MaxJ,:,iBlock) = 0.0
       case(5)
          State_VGB(:,:,:,0,iBlock) = State_VGB(:,:,:,1,iBlock)
          State_VGB(:,:,:,-1,iBlock) = State_VGB(:,:,:,1,iBlock)
          State_VGB(RhoUx_:RhoUz_,:,:,-1:0,iBlock) = 0.0
       case(6)
          State_VGB(:,:,:,nK+1,iBlock) = State_VGB(:,:,:,nK,iBlock)
          State_VGB(:,:,:,nK+2,iBlock) = State_VGB(:,:,:,nK,iBlock)
          State_VGB(RhoUx_:RhoUz_,:,:,nK+1:MaxK,iBlock) = 0.0
       case default
          if(iProc==0) call stop_mpi( &
               'read_inputs: unrecognized command: '//NameSub)
       end select
    end if

    if (DoAdvectSphere) then

       ! Convert to normalized units and separate velocity components
       UxNo = uBackgrndIo*sin(cDegToRad*FlowAngleTheta)* &
            cos(cDegToRad*FlowAnglePhi)*Io2No_V(UnitU_)
       UyNo = uBackgrndIo*sin(cDegToRad*FlowAngleTheta)* &
            sin(cDegToRad*FlowAnglePhi)*Io2No_V(UnitU_)
       UzNo = uBackgrndIo*cos(cDegToRad*FlowAngleTheta)*Io2No_V(UnitU_)

       RhoBackgrndNo = NumDensBackgrndIo*Io2Si_V(UnitN_)* &
            cProtonMass*Si2No_V(UnitRho_)

       ! Start filling in cells (including ghost cells)
       State_VGB(rho_,:,:,:,iBlock) = RhoBackgrndNo

       ! Transform to HGC frame
       ! only velocity and/ or momentum in X-Y plane should be transformed

       if(TypeCoordSystem =='HGC')then
          OmegaSun = cTwoPi/(RotationPeriodSun*Si2No_V(UnitT_))
          Phi = OmegaSun*tSimulation*Si2No_V(UnitT_)
          ! calculate the uniform flow in a fixed frame that is aligned with
          ! the HGC frame at this time
          UxAligned =  UxNo*cos(Phi) + UyNo*sin(Phi)
          UyAligned = -UxNo*sin(Phi) + UyNo*cos(Phi)

          State_VGB(RhoUx_,:,:,:,iBlock) = UxAligned*&
               State_VGB(rho_,:,:,:,iBlock)
          State_VGB(RhoUy_,:,:,:,iBlock) = UyAligned*&
               State_VGB(rho_,:,:,:,iBlock)

          ! Now transform velocity field to a rotating frame
          State_VGB(RhoUx_,:,:,:,iBlock) = State_VGB(RhoUx_,:,:,:,iBlock) &
               + State_VGB(Rho_,:,:,:,iBlock)*OmegaSun*Xyz_DGB(y_,:,:,:,iBlock)

          State_VGB(RhoUy_,:,:,:,iBlock) = State_VGB(RhoUy_,:,:,:,iBlock) &
               - State_VGB(Rho_,:,:,:,iBlock)*OmegaSun*Xyz_DGB(x_,:,:,:,iBlock)

          ! set the rest of state variables
          State_VGB(RhoUz_, :,:,:,iBlock) = UzNo
          State_VGB(Bx_:Bz_,:,:,:,iBlock) = 0.0
          State_VGB(p_,     :,:,:,iBlock) = pBackgrndIo*Io2No_V(UnitP_)

       else

          call CON_stop(&
               'You can only use user_outerbcs for ADVECTSPHERE in HGC frame')
       end if
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_cell_boundary
  !============================================================================
  subroutine user_amr_criteria(iBlock, UserCriteria, TypeCriteria, IsFound)

    use ModSize, ONLY: nI, nJ, nK
    use ModAdvance, ONLY: State_VGB, Rho_

    ! Variables required by this user subroutine
    integer, intent(in)          :: iBlock
    real, intent(out)            :: UserCriteria
    character (len=*),intent(in) :: TypeCriteria
    logical ,intent(inout)       :: IsFound

    real, parameter:: RhoMin = 2.0

    integer :: i, j, k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_amr_criteria'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    IsFound = .true.

    ! If density exceeds RhoMin, refine
    UserCriteria = 1.0
    do k = 1, nK; do j= 1, nJ; do i = 1, nI
       if(State_VGB(Rho_,i,j,k,iBlock) > RhoMin) RETURN
    end do; end do; end do

    ! No need to refine
    UserCriteria = 0.0

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_amr_criteria
  !============================================================================
  subroutine user_update_states(iBlock)

    use ModMain, ONLY: UseUserUpdateStates
    use ModUpdateState, ONLY: update_state_normal
    use ModAdvance, ONLY: nVar, Flux_VXI, Flux_VYI, Flux_VZI, &
         LeftState_VX, RightState_VX, LeftState_VY, RightState_VY, &
         LeftState_VZ, RightState_VZ
    use BATL_lib, ONLY: nDim, nI, nJ, nK
    use ModVarIndexes
    use ModUtilities, ONLY: i_gang

    integer,intent(in)::iBlock

    integer:: iGang, i, j, k
    real:: Un

    logical:: DoTest

    character(len=*), parameter:: NameSub = 'user_update_states'
    !--------------------------------------------------------------------------
    if(NameProblem /= 'HILL') RETURN

    call test_start(NameSub, DoTest, iBlock)

    iGang = i_gang(iBlock)

    ! Calculate upwind flux (same as Rusanov scheme)
    do k = 1, nK; do j = 1, nJ; do i = 1, nI + 1
       Un = Un_XB(i,j,k,iBlock)
       if(Un > 0)then
          Flux_VXI(Rho_,i,j,k,iGang) = Un*LeftState_VX(Rho_,i,j,k)
       else
          Flux_VXI(Rho_,i,j,k,iGang) = Un*RightState_VX(Rho_,i,j,k)
       end if
    end do; end do; end do

    do k = 1, nK; do j = 1, nJ+1; do i = 1, nI
       Un = Un_YB(i,j,k,iBlock)
       if(Un > 0)then
          Flux_VYI(Rho_,i,j,k,iGang) = Un*LeftState_VY(Rho_,i,j,k)
       else
          Flux_VYI(Rho_,i,j,k,iGang) = Un*RightState_VY(Rho_,i,j,k)
       end if
    end do; end do; end do

    ! if(iBlock == iBlockTest)then
    !   write(*,*) 'UnY,Left,Right=',Un_YB(iTest,jTest,kTest,iBlockTest), &
    !        LeftState_VY(Rho_,iTest,jTest,kTest), &
    !        RightState_VY(Rho_,iTest,jTest,kTest)
    ! end if

    if(nDim > 2)then
       do k = 1, nK+1; do j = 1, nJ; do i = 1, nI
          Un = Un_ZB(i,j,k,iBlock)
          if(Un > 0)then
             Flux_VZI(Rho_,i,j,k,iGang) = Un*LeftState_VZ(Rho_,i,j,k)
          else
             Flux_VZI(Rho_,i,j,k,iGang) = Un*RightState_VZ(Rho_,i,j,k)
          end if
       end do; end do; end do
    end if

    call update_state_normal(iBlock)

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_update_states
  !============================================================================
  subroutine get_gaussian_field(i, j, k, iBlock, B_D)

    use ModGeometry, ONLY: Xyz_DGB
    use ModMain, ONLY: tSimulation
    use ModNumConst, ONLY: cPi
    use ModResistivity, ONLY: Eta0

    integer, intent(in) :: i, j, k, iBlock
    real,    intent(out):: B_D(3)

    real :: Spread, Field
    real, parameter :: AmplitudeGaussian=10.0
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_gaussian_field'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    Spread = 4.0*Eta0*tSimulation
    Field = AmplitudeGaussian/(sqrt(cPi*Spread)) &
         *exp(-Xyz_DGB(y_,i,j,k,iBlock)**2/Spread)

    B_D = [ Field, 0.0, 0.0 ]

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine get_gaussian_field
  !============================================================================
  real function stream(iDir, Xyz_D)

    ! Calculate iDir component of the stream function at location Xyz_D

    integer, intent(in):: iDir
    real,    intent(in):: Xyz_D(3)

    real:: x, y, z, rCyl, rSph, StreamPhi
    !--------------------------------------------------------------------------

    ! Pure rotation:
    ! if(iDir == 3)then
    !   stream = sum(Xyz_D(1:2)**2)/2.0
    ! else
    !   stream = 0.0
    ! end if

    ! Hill vortex stream function is rCyl times the "Stoke stream function" in
    ! https://en.wikipedia.org/wiki/Hill%27s_spherical_vortex
    ! with a=1 (radius of vortex) and U=1 (far away flow speed)
    ! Only the Phi component is non-zero, which is in the X-Y plane.

    if(iDir == 3)then
       ! The Z or Latitude components are zero
       stream = 0.0
       RETURN
    end if

    if(iDir == 1 .and. .not.IsCartesian)then
       ! The R component is zero too (spherical or cylindrical)
       stream = 0.0
       RETURN
    end if

    x = Xyz_D(1); y = Xyz_D(2); z = Xyz_D(3)

    rSph = norm2(Xyz_D)
    rCyl = sqrt(x**2 + y**2)

    ! Calculate the phi component of the Stream function/rCyl
    if(rSph < 1.0)then
       StreamPhi = 0.75*(rSph**2 - 1)
    else
       StreamPhi = 0.5*(1 - 1/rSph**3)
    endif

    if(IsCartesian)then
       ! Calculate the x and y components in Cartesian coordinates
       if(iDir == 1)then
          stream = -y*StreamPhi
       else
          stream =  x*StreamPhi
       end if
    else
       ! Phi component in spherical or cylindrical coordinates
       stream = StreamPhi*rCyl
    endif

  end function stream
  !============================================================================
  subroutine set_hill_velocity(iBlock)

    use ModAdvance, ONLY: State_VGB
    use BATL_lib, ONLY: Xyz_DNB, nI, nJ, nK, nDim, MaxBlock, Xyz_DGB

    integer, intent(in):: iBlock

    integer:: i, j, k
    real:: Rho, rCyl, rSph, x, y, z
    !--------------------------------------------------------------------------
    if(.not.allocated(Stream_DG))then
       allocate( &
            Stream_DG(3,MinI:MaxI,MinJ:MaxJ,MinK:MaxK), &
            Un_XB(nI+1,nJ,nK,MaxBlock), &
            Un_YB(nI,nJ+1,nK,MaxBlock), &
            Un_ZB(nI,nJ,nK+1,MaxBlock) )
    endif

    Stream_DG = 0.0

    ! Calculate edge centered stream function
    if(nDim == 2)then
       ! In 2D, only the 3rd component is needed
       do j = 1, nJ+1; do i = 1, nI+1
          ! write(*,*)'i,j,iBlock,Xyz=',i,j,iBlock,Xyz_DNB(:,i,j,1,iBlock)
          Stream_DG(3,i,j,1) = stream(3, Xyz_DNB(:,i,j,1,iBlock))
       end do; end do

       ! Take curl of Stream function on each face
       do j = 1, nJ; do i = 1, nI+1
          ! ux = dSz/dy
          Un_XB(i,j,1,iBlock) = -Stream_DG(3,i,j,1) + Stream_DG(3,i,j+1,1)
       end do; end do
       do j = 1, nJ+1; do i = 1, nI
          ! uy = -dSz/dx
          Un_YB(i,j,1,iBlock) = Stream_DG(3,i,j,1) - Stream_DG(3,i+1,j,1)
       end do; end do
    else
       ! 3D: approximate the integral with the midedge value times edge length
       do k = 1, nK+1; do j = 1, nJ+1; do i = 1, nI
          Stream_DG(1,i,j,k) = stream(1, &
               0.5*  (Xyz_DNB(:,i+1,j,k,iBlock) + Xyz_DNB(:,i,j,k,iBlock))) &
               *norm2(Xyz_DNB(:,i+1,j,k,iBlock) - Xyz_DNB(:,i,j,k,iBlock))
       end do; end do; end do
       do k = 1, nK+1; do j = 1, nJ; do i = 1, nI+1
          Stream_DG(2,i,j,k) = stream(2, &
               0.5*  (Xyz_DNB(:,i,j+1,k,iBlock) + Xyz_DNB(:,i,j,k,iBlock))) &
               *norm2(Xyz_DNB(:,i,j+1,k,iBlock) - Xyz_DNB(:,i,j,k,iBlock))
       end do; end do; end do
       do k = 1, nK; do j = 1, nJ+1; do i = 1, nI+1
          Stream_DG(3,i,j,k) = stream(3, &
               0.5*  (Xyz_DNB(:,i,j,k+1,iBlock) + Xyz_DNB(:,i,j,k,iBlock))) &
               *norm2(Xyz_DNB(:,i,j,k+1,iBlock) - Xyz_DNB(:,i,j,k,iBlock))
       end do; end do; end do

       ! Take curl of Stream function on each face multiplied by the area
       do k = 1, nK; do j = 1, nJ; do i = 1, nI+1
          ! ux = dSz/dy - dSy/dz
          Un_XB(i,j,k,iBlock) = &
               (Stream_DG(3,i,j+1,k) - Stream_DG(3,i,j,k)) - &
               (Stream_DG(2,i,j,k+1) - Stream_DG(2,i,j,k))
       end do; end do; end do
       do k = 1, nK; do j = 1, nJ+1; do i = 1, nI
          ! uy = dSx/dz - dSz/dx
          Un_YB(i,j,k,iBlock) = &
               (Stream_DG(1,i,j,k+1) - Stream_DG(1,i,j,k)) - &
               (Stream_DG(3,i+1,j,k) - Stream_DG(3,i,j,k))
       end do; end do; end do
       do k = 1, nK+1; do j = 1, nJ; do i = 1, nI
          ! uz = dSy/dx - dSx/dy
          Un_ZB(i,j,k,iBlock) = &
               (Stream_DG(2,i+1,j,k) - Stream_DG(2,i,j,k)) - &
               (Stream_DG(1,i,j+1,k) - Stream_DG(1,i,j,k))
       end do; end do; end do
    end if

    if(nVar > 1)then
       ! Set velocity at cell centers (these are the face normal components)
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          Rho = State_VGB(Rho_,i,j,k,iBlock)

          x = Xyz_DGB(x_,i,j,k,iBlock)
          y = Xyz_DGB(y_,i,j,k,iBlock)
          z = Xyz_DGB(z_,i,j,k,iBlock)
          rCyl = sqrt(x**2    + y**2)
          rSph = sqrt(rCyl**2 + z**2)

          Rho = State_VGB(Rho_,i,j,k,iBlock)
          if(rSph < 1)then
             State_VGB(RhoUz_,i,j,k,iBlock) = 1.5*Rho*(-1 + rCyl**2 + rSph**2)
             State_VGB(RhoUx_:RhoUy_,i,j,k,iBlock) = -1.5*Rho*[x, y]*z
          else
             State_VGB(RhoUz_,i,j,k,iBlock) = &
                  Rho*(1 - 1/rSph**3 + 1.5*rCyl**2/rSph**5)
             State_VGB(RhoUx_:RhoUy_,i,j,k,iBlock) = -1.5*Rho*[x, y]*z/rSph**5
          end if

       end do; end do; end do
    end if

  end subroutine set_hill_velocity
  !============================================================================
end module ModUser
!==============================================================================
