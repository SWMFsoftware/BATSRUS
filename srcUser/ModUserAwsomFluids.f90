!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
!#NOTPUBLIC  email:bartvand@umich.edu  expires:12/31/2099
module ModUser

  use BATL_lib, ONLY: &
       test_start, test_stop, iProc

  use ModVarIndexes, ONLY: IonFirst_, nFluid
  use ModMultiFluid, ONLY: nIonFluid
  use ModMain, ONLY: nI, nJ,nK
  use ModCoronalHeating, ONLY: PoyntingFluxPerB
  use ModUserEmpty,                                     &
       IMPLEMENTED1 => user_read_inputs,                &
       IMPLEMENTED2 => user_init_session,               &
       IMPLEMENTED3 => user_set_ics,                    &
       IMPLEMENTED4 => user_get_log_var,                &
       IMPLEMENTED5 => user_set_plot_var,               &
       IMPLEMENTED6 => user_set_cell_boundary,          &
       IMPLEMENTED7 => user_set_resistivity,            &
       IMPLEMENTED8 => user_get_b0,                     &
       IMPLEMENTED9 => user_initial_perturbation

  include 'user_module.h' ! list of public methods

  real, parameter :: VersionUserModule = 1.0
  character (len=*), parameter :: NameUserFile = "ModUserAwsomFluids.f90"
  character (len=*), parameter :: NameUserModule = &
       'AWSoM multi-fluid model'

  ! Input parameters for chromospheric inner BC's
  real    :: NchromoSi_I(IonFirst_:nFluid) = 2e17, TchromoSi = 5e4
  real    :: Nchromo_I(IonFirst_:nFluid), Tchromo

  ! variables for Parker initial condition
  real    :: nCoronaSi = 1.5e14, tCoronaSi = 1.5e6

  ! Input parameters for two-temperature effects
  real    :: TeFraction, TiFraction
  real    :: EtaPerpSi

  ! variables for polar jet application
  ! Dipole under surface
  real    :: UserDipoleStrength, UserDipoleStrengthSi
  real    :: UserDipoleDepth
  real    :: UserDipoleLatitude
  real    :: UserDipoleLongitude
  real    :: UserDipoleAxisLatitude
  real    :: UserDipoleAxisLongitude

  ! Rotating boundary condition
  real    :: TbeginJet, TendJet
  logical :: IsRamping
  logical :: IsPolarDipole=.false., IsJetBC
  real    :: DistMinJet
  real    :: DistMaxJet
  real    :: LocationMaxJet
  real    :: UmaxJet
  real    :: ProfileExponentJet
  real    :: LinearCoeffJet
  real    :: PowerCoeffJet

  ! Multiply the collisionfrequencies by CollisionFactor
  real :: CollisionFactor = 1.0

contains
  !============================================================================

  subroutine user_read_inputs

    use ModMain,       ONLY: lVerbose
    use ModReadParam,  ONLY: read_line, read_command, read_var
    use ModIO,         ONLY: write_prefix, write_myname, iUnitOut

    integer :: iFluid
    character (len=100) :: NameCommand

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
       case("#CHROMOBC")
          do iFluid = IonFirst_, nFluid
             call read_var('NchromoSi', NchromoSi_I(iFluid))
          end do
          call read_var('TchromoSi', TchromoSi)

       case("#PARKERIC")
          call read_var('nCoronaSi', nCoronaSi)
          call read_var('tCoronaSi', tCoronaSi)

       case("#POLARJETDIPOLE")
          call read_var('UserDipoleStrengthSi', UserDipoleStrengthSi)
          call read_var('UserDipoleDepth', UserDipoleDepth)
          call read_var('UserDipoleLatitude',UserDipoleLatitude)
          call read_var('UserDipoleLongitude',UserDipoleLongitude)
          call read_var('UserDipoleAxisLatitude',UserDipoleAxisLatitude)
          call read_var('UserDipoleAxisLongitude',UserDipoleAxisLongitude)
          IsPolarDipole = .true.

       case("#POLARJETBC")
          call read_var('IsRamping', IsRamping)
          if(IsRamping)then
             call read_var('TbeginJet', TBeginJet)
             call read_var('TendJet',   TEndJet)
          endif
          call read_var('DistMinJet',DistMinJet)
          call read_var('DistMaxJet',DistMaxJet)
          call read_var('ProfileExponentJet',ProfileExponentJet)
          call read_var('UmaxJet',UmaxJet)
          IsJetBC = .true.

       case("#COLLISIONFACTOR")
          call read_var('CollisionFactor', CollisionFactor)

       case('#USERINPUTEND')
          if(iProc == 0 .and. lVerbose > 0)then
             call write_prefix;
             write(iUnitOut,*)'User read_input SOLAR CORONA ends'
          endif
          EXIT

       case default
          if(iProc == 0) then
             call write_myname; write(*,*) &
                  'ERROR: Invalid user defined #COMMAND in user_read_inputs.'
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

    use ModIO,         ONLY: write_prefix, iUnitOut
    use ModWaves,      ONLY: UseWavePressure, UseAlfvenWaves
    use ModAdvance,    ONLY: UseElectronPressure
    use ModMultiFluid, ONLY: MassIon_I, ChargeIon_I
    use ModConst,      ONLY: cElectronCharge, cLightSpeed, cBoltzmann, cEps, &
         cElectronMass, cProtonMass
    use ModNumConst,   ONLY: cTwoPi, cDegToRad
    use ModPhysics,    ONLY: ElectronTemperatureRatio, AverageIonCharge, &
         Si2No_V, UnitTemperature_, UnitN_, UnitX_, No2Si_V, UnitT_, UnitB_, &
         UnitU_, Io2No_V, BodyNDim_I, BodyTDim_I, Gamma
    use EEE_ModCommonVariables, ONLY: UseCme
    use EEE_ModMain,   ONLY: EEE_initialize

    real, parameter :: CoulombLog = 20.0

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_init_session'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(iProc == 0)then
       call write_prefix; write(iUnitOut,*) ''
       call write_prefix; write(iUnitOut,*) 'user_init_session:'
       call write_prefix; write(iUnitOut,*) ''
    end if

    UseAlfvenWaves = .true.
    UseWavePressure = .true.

    ! convert to normalized units
    Nchromo_I   = NchromoSi_I*Si2No_V(UnitN_)
    Tchromo     = TchromoSi*Si2No_V(UnitTemperature_)

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

    ! perpendicular resistivity, used for temperature relaxation
    ! Note EtaPerpSi is divided by cMu.
    EtaPerpSi = sqrt(cElectronMass)*CoulombLog &
         *(cElectronCharge*cLightSpeed)**2/(3*(cTwoPi*cBoltzmann)**1.5*cEps)

    ! dipole (jet) parameter converted to normalized units
    if(IsPolarDipole)then
       UserDipoleStrength = UserDipoleStrengthSi*Si2No_V(UnitB_)
       UserDipoleLatitude = UserDipoleLatitude*cDegToRad
       UserDipoleLongitude = UserDipoleLongitude*cDegToRad
       UserDipoleAxisLatitude = UserDipoleAxisLatitude*cDegToRad
       UserDipoleAxisLongitude = UserDipoleAxisLongitude*cDegToRad
    endif

    ! the rotation's velocity profile is the following:
    ! u(z) = Ax-Bx^C
    ! LinearCoeff =     A = umax/xmax * rmax^(C-1)/(rmax^(C-1)-xmax^(C-1))
    ! PowerCoeff  =     B = umax/xmax             /(rmax^(C-1)-xmax^(C-1))
    !                   B = A * rmax^(C-1)
    ! ProfileExponent = C = 5.14
    ! A = 3600.
    ! B = 164.^1./5.14 = 2.4226857e11
    ! xmax = rmax*C^(1/1-C) = LocationMaxJet
    ! for:
    !     DistMminJet = 2.13e-3 Rs = rmin
    !     DistMaxJet = 1.28582e-2 Rs = rmax
    !     LocationMaxJet = 8.658e-3 Rs = xmax
    !     UmaxJet = 25.1067 km/s = umax
    if(IsJetBC) then

       LocationMaxJet = DistMaxJet*ProfileExponentJet** &
            (1./(1.-ProfileExponentJet))

       UmaxJet = UmaxJet * Si2No_V(UnitU_) * 1e3

       LinearCoeffJet = UmaxJet/LocationMaxJet &
            * DistMaxJet**(ProfileExponentJet-1.) &
            /(DistMaxJet**(ProfileExponentJet-1.) &
            - LocationMaxJet**(ProfileExponentJet-1.))

       PowerCoeffJet = LinearCoeffJet* DistMaxJet** &
            (1.-ProfileExponentJet)

    endif

    ! Initialize CME
    if(UseCme) call EEE_initialize(BodyNDim_I(1), BodyTDim_I(1), Gamma)

    if(iProc == 0)then
       call write_prefix; write(iUnitOut,*) ''
       call write_prefix; write(iUnitOut,*) 'user_init_session finished'
       call write_prefix; write(iUnitOut,*) ''
    end if

    call test_stop(NameSub, DoTest)
  end subroutine user_init_session
  !============================================================================

  subroutine user_set_ics(iBlock)

    ! The isothermal parker wind solution is used as initial condition

    use ModAdvance,    ONLY: State_VGB, UseAnisoPressure
    use ModB0,         ONLY: B0_DGB
    use ModGeometry,   ONLY: Xyz_DGB, r_Blk
    use ModPhysics,    ONLY: Si2No_V, UnitTemperature_, rBody, GBody, UnitN_
    use ModVarIndexes, ONLY: Rho_, Bx_, Bz_, p_, Pe_, WaveFirst_, WaveLast_, &
         Ew_
    use ModMultiFluid, ONLY: iRho_I, iRhoUx_I, iRhoUy_I, iRhoUz_I, iP_I, &
         MassFluid_I, iRhoIon_I, iPIon_I, MassIon_I, ChargeIon_I, IonLast_, &
         UseMultiIon, iPparIon_I, IsIon_I
    use ModWaves, ONLY: UseWavePressureLtd

    integer, intent(in) :: iBlock

    integer :: i, j, k, iFluid
    real :: x, y, z, r, Rho, NumDens_I(IonFirst_:nFluid)
    real :: RhoCorona, tCorona, uCorona
    real :: r_D(3), Br
    ! variables for iterative Parker solution
    integer :: IterCount
    real :: Ur, Ur0, Ur1, del, rTransonic, Uescape, Usound

    real, parameter :: Epsilon = 1.0e-6

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_ics'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! Initially, density, electron and ion temperature are at coronal
    ! values starting from just above the boundary
    RhoCorona = nCoronaSi*Si2No_V(UnitN_)*MassIon_I(1)
    tCorona   = tCoronaSi*Si2No_V(UnitTemperature_)

    ! normalize with isothermal sound speed.
    Usound = sqrt(tCorona*(1.0+ChargeIon_I(1))/MassIon_I(1))
    Uescape = sqrt(-GBody*2.0)/Usound

    !\
    ! Initialize MHD wind with Parker's solution
    ! construct solution which obeys
    !   rho x u_r x r^2 = constant
    !/
    rTransonic = 0.25*Uescape**2
    if(.not.(rTransonic>exp(1.0))) call stop_mpi('sonic point inside Sun')

    uCorona = rTransonic**2*exp(1.5 - 2.0*rTransonic)

    do k = MinK, MaxK ; do j = MinJ, MaxJ ; do i = MinI, MaxI
       x = Xyz_DGB(x_,i,j,k,iBlock)
       y = Xyz_DGB(y_,i,j,k,iBlock)
       z = Xyz_DGB(z_,i,j,k,iBlock)
       r = r_BLK(i,j,k,iBlock)
       r_D = (/x,y,z/)

       if(r > rTransonic)then
          !\
          ! Inside supersonic region
          !/
          Ur0 = 1.0
          IterCount = 0
          do
             IterCount = IterCount + 1
             Ur1 = sqrt(Uescape**2/r - 3.0 + 2.0*log(16.0*Ur0*r**2/Uescape**4))
             del = abs(Ur1 - Ur0)
             if(del < Epsilon)then
                Ur = Ur1
                EXIT
             elseif(IterCount < 1000)then
                Ur0 = Ur1
                CYCLE
             else
                call stop_mpi('PARKER > 1000 it.')
             end if
          end do
       else
          !\
          ! Inside subsonic region
          !/
          Ur0 = 1.0
          IterCount = 0
          do
             IterCount = IterCount + 1
             Ur1 = (Uescape**2/(4.0*r))**2 &
                  *exp(0.5*(Ur0**2 + 3.0 - Uescape**2/r))
             del = abs(Ur1 - Ur0)
             if(del < Epsilon)then
                Ur = Ur1
                EXIT
             elseif(IterCount < 1000)then
                Ur0 = Ur1
                CYCLE
             else
                call CON_stop('PARKER > 1000 it.')
             end if
          end do
       end if

       Rho = rBody**2*RhoCorona*uCorona/(r**2*Ur)

       NumDens_I = Rho/MassIon_I(1)*Nchromo_I/Nchromo_I(IonFirst_)

       do iFluid = IonFirst_, nFluid
          State_VGB(iRho_I(iFluid),i,j,k,iBlock) = &
               NumDens_I(iFluid)*MassFluid_I(iFluid)
       end do

       State_VGB(iP_I(IonFirst_:),i,j,k,iBlock) = NumDens_I*Tcorona
       if(UseAnisoPressure)then
          do iFluid = IonFirst_, IonLast_
             State_VGB(iPparIon_I(iFluid),i,j,k,iBlock) = &
                  State_VGB(iP_I(iFluid),i,j,k,iBlock)
          end do
       end if
       State_VGB(Pe_,i,j,k,iBlock) = &
            sum(ChargeIon_I*State_VGB(iPIon_I,i,j,k,iBlock))

       State_VGB(iRhoUx_I,i,j,k,iBlock) = &
            State_VGB(iRho_I,i,j,k,iBlock)*Ur*x/r *Usound
       State_VGB(iRhoUy_I,i,j,k,iBlock) = &
            State_VGB(iRho_I,i,j,k,iBlock)*Ur*y/r *Usound
       State_VGB(iRhoUz_I,i,j,k,iBlock) = &
            State_VGB(iRho_I,i,j,k,iBlock)*Ur*z/r *Usound

       State_VGB(Bx_:Bz_,i,j,k,iBlock) = 0.0
       Br = sum(B0_DGB(1:3,i,j,k,iBlock)*r_D)

       if (Br >= 0.0) then
          State_VGB(WaveFirst_,i,j,k,iBlock) =  &
               PoyntingFluxPerB*sqrt(State_VGB(iRho_I(IonFirst_),i,j,k,iBlock))
          State_VGB(WaveLast_,i,j,k,iBlock) = &
               1e-4*State_VGB(WaveFirst_,i,j,k,iBlock)
       else
          State_VGB(WaveLast_,i,j,k,iBlock) =  &
               PoyntingFluxPerB*sqrt(State_VGB(iRho_I(IonFirst_),i,j,k,iBlock))
          State_VGB(WaveFirst_,i,j,k,iBlock) = &
               1e-4*State_VGB(WaveLast_,i,j,k,iBlock)
       end if
       if(UseWavePressureLtd) State_VGB(Ew_,i,j,k,iBlock) = &
            sum(State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock))
    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_ics
  !============================================================================

  subroutine user_get_log_var(VarValue, TypeVar, Radius)

    use ModAdvance,    ONLY: State_VGB, tmp1_BLK
    use ModB0,         ONLY: B0_DGB
    use ModIO,         ONLY: write_myname
    use ModMain,       ONLY: Unused_B, nBlock, x_, y_, z_, UseB0
    use ModPhysics,    ONLY: InvGammaMinus1, No2Io_V, No2Si_V, &
         UnitEnergydens_, UnitX_, UnitRho_
    use ModVarIndexes, ONLY: Bx_, By_, Bz_, Pe_, iP_I, Ew_, &
         rho_, rhoUx_, rhoUy_, rhoUz_
    use ModGeometry,   ONLY: R_BLK
    use BATL_lib,      ONLY: integrate_grid

    real, intent(out) :: VarValue
    character(len=10), intent(in) :: TypeVar
    real, optional, intent(in) :: Radius

    integer :: i, j, k, iBlock
    real :: unit_energy, unit_mass

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_get_log_var'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    unit_energy = No2Io_V(UnitEnergydens_)*No2Io_V(UnitX_)**3
    unit_mass   = 1.0e3*No2Si_V(UnitRho_)*No2Si_V(UnitX_)**3

    !\
    ! Define log variable to be saved::
    !/
    select case(TypeVar)

    case('eint')
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             tmp1_BLK(i,j,k,iBlock) = &
                  sum(State_VGB(iP_I(IonFirst_:),i,j,k,iBlock)) &
                  + State_VGB(Pe_,i,j,k,iBlock)
          end do; end do; end do
       end do
       VarValue = unit_energy*InvGammaMinus1*integrate_grid(tmp1_BLK)

    case('emag')
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          if(UseB0)then
             tmp1_BLK(:,:,:,iBlock) = &
                  ( B0_DGB(x_,:,:,:,iBlock) + State_VGB(Bx_,:,:,:,iBlock))**2 &
                  +(B0_DGB(y_,:,:,:,iBlock) + State_VGB(By_,:,:,:,iBlock))**2 &
                  +(B0_DGB(z_,:,:,:,iBlock) + State_VGB(Bz_,:,:,:,iBlock))**2
          else
             tmp1_BLK(:,:,:,iBlock) = State_VGB(Bx_,:,:,:,iBlock)**2 &
                  + State_VGB(By_,:,:,:,iBlock)**2 &
                  + State_VGB(Bz_,:,:,:,iBlock)**2
          end if
       end do
       VarValue = unit_energy*0.5*integrate_grid(tmp1_BLK)

    case('ekin')
       do iBlock=1,nBlock
          if (Unused_B(iBlock)) CYCLE
          tmp1_BLK(:,:,:,iBlock) = &
               (State_VGB(rhoUx_,:,:,:,iBlock)**2 +&
               State_VGB(rhoUy_,:,:,:,iBlock)**2 +&
               State_VGB(rhoUz_,:,:,:,iBlock)**2)/&
               State_VGB(rho_  ,:,:,:,iBlock)
       end do
       VarValue = unit_energy*0.5*integrate_grid(tmp1_BLK)

    case('ew')
       do iBlock=1,nBlock
          if (Unused_B(iBlock)) CYCLE
          tmp1_BLK(:,:,:,iBlock) = State_VGB(Ew_,:,:,:,iBlock)
       end do
       VarValue = unit_energy*integrate_grid(tmp1_BLK)

    case('mass')
       do iBlock=1,nBlock
          if (Unused_B(iBlock)) CYCLE
          tmp1_BLK(:,:,:,iBlock) = &
               State_VGB(rho_,:,:,:,iBlock)/R_BLK(:,:,:,iBlock)
       end do
       VarValue = unit_mass*integrate_grid(tmp1_BLK)

    case('vol')
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE

          tmp1_BLK(:,:,:,iBlock) = 1.0
       end do
       VarValue = integrate_grid(tmp1_BLK)

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

    use ModAdvance,        ONLY: UseElectronPressure, UseAnisoPressure, &
         State_VGB, Source_VC
    use ModB0,             ONLY: set_b0_face
    use ModChromosphere,   ONLY: DoExtendTransitionRegion, extension_factor, &
         TeSi_C, get_tesi_c
    use ModCoronalHeating, ONLY: CoronalHeating_C, &
         apportion_coronal_heating, get_block_heating, get_wave_reflection, &
         WaveDissipation_VC, UseAlignmentAngle, Cdiss_C
    use ModRadiativeCooling, ONLY: RadCooling_C, get_radiative_cooling
    use ModFaceValue,      ONLY: calc_face_value
    use ModMultiFluid,     ONLY: IonLast_, IonFirst_
    use ModPhysics,        ONLY: No2Si_V, UnitT_, UnitEnergyDens_, UnitT_, &
         UnitTemperature_
    use ModVarIndexes,     ONLY: WaveFirst_, WaveLast_, Rho_, Pe_, p_

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

    integer :: i, j, k
    real :: QPerQtotal_I(IonFirst_:IonLast_)
    real :: QparPerQtotal_I(IonFirst_:IonLast_)
    real :: QePerQtotal
    logical :: IsNewBlockAlfven

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
       IsNewBlockAlfven = .true.
       call get_wave_reflection(iBlock, IsNewBlockAlfven)
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          PlotVar_G(i,j,k) = Source_VC(WaveLast_,i,j,k) &
               /sqrt(State_VGB(WaveFirst_,i,j,k,iBlock) &
               *     State_VGB(WaveLast_,i,j,k,iBlock))/No2Si_V(UnitT_)
          Source_VC(WaveFirst_:WaveLast_,i,j,k) = 0.0
       end do; end do; end do
       NameIdlUnit = '1/s'
       NameTecUnit = '1/s'

    case('sintheta')
       Source_VC(WaveFirst_:WaveLast_,:,:,:) = 0.0
       call set_b0_face(iBlock)
       call calc_face_value(iBlock, DoResChangeOnly = .false., &
            DoMonotoneRestrict = .false.)
       IsNewBlockAlfven = .true.
       call get_wave_reflection(iBlock, IsNewBlockAlfven)

       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          PlotVar_G(i,j,k) = Cdiss_C(i,j,k)
          Source_VC(WaveFirst_:WaveLast_,i,j,k) = 0.0
       end do; end do; end do
       NameIdlUnit = '-'
       NameTecUnit = '-'

    case('qheat')
       if(UseAlignmentAngle)then
          Source_VC(WaveFirst_:WaveLast_,:,:,:) = 0.0
          call set_b0_face(iBlock)
          call calc_face_value(iBlock, DoResChangeOnly = .false., &
               DoMonotoneRestrict = .false.)
          IsNewBlockAlfven = .true.
          call get_wave_reflection(iBlock, IsNewBlockAlfven)
       end if

       call get_block_heating(iBlock)

       if(DoExtendTransitionRegion) call get_tesi_c(iBlock, TeSi_C)
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(DoExtendTransitionRegion) CoronalHeating_C(i,j,k) = &
               CoronalHeating_C(i,j,k)/extension_factor(TeSi_C(i,j,k))
          if(UseAlignmentAngle)then
             CoronalHeating_C(i,j,k) = CoronalHeating_C(i,j,k)*Cdiss_C(i,j,k)
             Source_VC(WaveFirst_:WaveLast_,i,j,k) = 0.0
          end if
          PlotVar_G(i,j,k) = CoronalHeating_C(i,j,k) &
               *No2Si_V(UnitEnergyDens_)/No2Si_V(UnitT_)
       end do; end do; end do
       NameIdlUnit = 'J/m^3/s'
       NameTecUnit = 'J/m^3/s'

    case('qebyq', 'qparbyq', 'qperpbyq', 'qparbyqa', 'qperpbyqa')
       if(UseElectronPressure)then
          call set_b0_face(iBlock)
          call calc_face_value(iBlock, DoResChangeOnly = .false., &
               DoMonotoneRestrict = .false.)

          if(UseAlignmentAngle)then
             Source_VC(WaveFirst_:WaveLast_,:,:,:) = 0.0
             IsNewBlockAlfven = .true.
             call get_wave_reflection(iBlock, IsNewBlockAlfven)
          end if

          call get_block_heating(iBlock)
          if(DoExtendTransitionRegion) call get_tesi_c(iBlock, TeSi_C)
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             if(DoExtendTransitionRegion) CoronalHeating_C(i,j,k) = &
                  CoronalHeating_C(i,j,k)/extension_factor(TeSi_C(i,j,k))
             if(UseAlignmentAngle)then
                CoronalHeating_C(i,j,k) = CoronalHeating_C(i,j,k) &
                     *Cdiss_C(i,j,k)
                WaveDissipation_VC(:,i,j,k) = WaveDissipation_VC(:,i,j,k) &
                     *Cdiss_C(i,j,k)
                Source_VC(WaveFirst_:WaveLast_,i,j,k) = 0.0
             end if
             call apportion_coronal_heating(i, j, k, iBlock, &
                  WaveDissipation_VC(:,i,j,k), CoronalHeating_C(i,j,k), &
                  QPerQtotal_I, QparPerQtotal_I, QePerQtotal)
             select case(NameVar)
             case('qebyq')
                PlotVar_G(i,j,k) = QePerQtotal
             case('qparbyq')
                PlotVar_G(i,j,k) = QparPerQtotal_I(IonFirst_)
             case('qperpbyq')
                PlotVar_G(i,j,k) = &
                     QPerQtotal_I(IonFirst_) - QparPerQtotal_I(IonFirst_)
             case('qparbyqa')
                PlotVar_G(i,j,k) = QparPerQtotal_I(IonLast_)
             case('qperpbyqa')
                PlotVar_G(i,j,k) = &
                     QPerQtotal_I(IonLast_) - QparPerQtotal_I(IonLast_)
             end select
          end do; end do; end do
       else
          PlotVar_G(i,j,k) = 0.0
       end if
       NameIdlUnit = '-'
       NameTecUnit = '-'

    case default
       IsFound = .false.
    end select

    UsePlotVarBody = .false.
    PlotVarBody    = 0.0

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_plot_var
  !============================================================================

  subroutine user_set_cell_boundary(iBlock, iSide, CBC, IsFound)

    ! Fill ghost cells inside body for spherical grid - this subroutine only
    ! modifies ghost cells in the r direction

    use EEE_ModCommonVariables, ONLY: UseCme
    use EEE_ModMain,   ONLY: EEE_get_state_BC
    use BATL_lib,      ONLY: CellSize_DB, x_, y_, z_
    use ModAdvance,    ONLY: State_VGB, UseElectronPressure, UseAnisoPressure
    use ModB0,         ONLY: B0_DGB
    use ModGeometry,   ONLY: TypeGeometry, Xyz_DGB, r_BLK
    use ModHeatFluxCollisionless, ONLY: UseHeatFluxCollisionless, &
         get_gamma_collisionless
    use ModPhysics,    ONLY: InvGammaMinus1, GBody, rBody, &
         UnitU_, UnitRho_, UnitB_, UnitP_, Si2No_V
    use ModVarIndexes, ONLY: Pe_, Bx_, Bz_, WaveFirst_, WaveLast_, Ew_, &
         Ehot_, p_, Rho_, Ppar_
    use ModMultiFluid, ONLY: MassIon_I, iRhoIon_I, ChargeIon_I, IonLast_, &
         iRho_I, MassFluid_I, iUx_I, iUz_I, iRhoUx_I, iRhoUz_I, iPIon_I, &
         iP_I, iPparIon_I, IsIon_I
    use ModImplicit,   ONLY: StateSemi_VGB, iTeImpl
    use ModWaves,      ONLY: UseWavePressureLtd
    ! Below is for jet only
    use ModMain,       ONLY: time_simulation, time_accurate, CellBCType, &
         n_step, iteration_number
    use ModConst,      ONLY: cPi
    use ModCoordTransform, ONLY: rlonlat_to_xyz, cross_product
    ! Above is for jet only
    integer,          intent(in)  :: iBlock, iSide
    type(CellBCType), intent(in)  :: CBC
    logical,          intent(out) :: IsFound

    ! For CME
    real    :: RhoCme, Ucme_D(3), Bcme_D(3), pCme, BrCme, BrCme_D(3)

    integer :: Minor_, Major_
    integer :: i, j, k, iFluid, iRho, iRhoUx, iRhoUz, iP
    real    :: Br1_D(3), Bt1_D(3), Runit_D(3)
    real    :: FullB_D(3), SignBr
    real    :: Gamma
    real    :: U, U_D(3), Bdir_D(3)

    ! Below is for jet only
    real    :: Framp
    real    :: Xyz_D(3), JetCenter_D(3)
    real    :: CrossProduct_D(3), Urot_D(3)
    real    :: DistanceJet
    real    :: Ucoeff
    real    :: r
    ! Above is for jet only

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_cell_boundary'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(iSide /= 1 .or. TypeGeometry(1:9) /='spherical') &
         call CON_stop('Wrong iSide in user_set_cell_boundary')

    IsFound = .true.

    select case(CBC%TypeBc)
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

    ! This part is for jet only: rotation starts immediately or gradually
    if(IsJetBC)then
       ! Check if time accurate is set.
       if(time_accurate .and. IsRamping)then
          if(time_simulation<TbeginJet)then
             Framp = 0.0
          elseif(time_simulation>TendJet)then
             Framp = 1.0
          else
             Framp = 0.5 * (1 - cos(2*cPi*(time_simulation-TbeginJet) / &
                  (TendJet-TbeginJet)))
          endif
       elseif(.not. IsRamping)then
          Framp = 1.0
       else
          Framp = 0.0
       endif
    endif
    ! This part for jet only ends here.

    do k = MinK, MaxK; do j = MinJ, MaxJ

       Runit_D = Xyz_DGB(:,1,j,k,iBlock) / r_BLK(1,j,k,iBlock)

       Br1_D = sum(State_VGB(Bx_:Bz_,1,j,k,iBlock)*Runit_D)*Runit_D
       Bt1_D = State_VGB(Bx_:Bz_,1,j,k,iBlock) - Br1_D

       ! Set B1r=0, and B1theta = B1theta(1) and B1phi = B1phi(1)
       do i = MinI, 0
          State_VGB(Bx_:Bz_,i,j,k,iBlock) = Bt1_D
       end do

       do iFluid = IonFirst_, nFluid
          iRho = iRho_I(iFluid)

          do i = MinI, 0
             ! exponential scaleheight
             State_VGB(iRho,i,j,k,iBlock) = &
                  Nchromo_I(iFluid)*MassFluid_I(iFluid)*exp(-GBody/rBody &
                  *MassFluid_I(iFluid)/Tchromo &
                  *(rBody/r_BLK(i,j,k,iBlock) - 1.0))

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
          Major_ = WaveLast_
          Minor_ = WaveFirst_
       else
          Major_ = WaveFirst_
          Minor_ = WaveLast_
       end if

       do i = MinI, 0
          ! Te = T_s and ne = sum(q_s n_s)
          if(UseElectronPressure) State_VGB(Pe_,i,j,k,iBlock) = &
               sum(ChargeIon_I*State_VGB(iPIon_I,i,j,k,iBlock))

          ! Outgoing wave energy
          State_VGB(Major_,i,j,k,iBlock) = PoyntingFluxPerB &
               *sqrt(State_VGB(iRho,i,j,k,iBlock))

          ! Ingoing wave energy
          State_VGB(Minor_,i,j,k,iBlock) = 0.0

          if(UseWavePressureLtd) State_VGB(Ew_,i,j,k,iBlock) &
               = sum(State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock))
       end do

       ! At the inner boundary this seems to be unnecessary...
       ! Ehot=0 may be sufficient?!
       if(Ehot_ > 1)then
          if(UseHeatFluxCollisionless)then
             iP = p_; if(UseElectronPressure) iP = Pe_
             do i = MinI, 0
                call get_gamma_collisionless(Xyz_DGB(:,i,j,k,iBlock), Gamma)
                State_VGB(Ehot_,i,j,k,iBlock) = &
                     State_VGB(iP,i,j,k,iBlock)*(1.0/(Gamma-1) -InvGammaMinus1)
             end do
          else
             State_VGB(Ehot_,MinI:0,j,k,iBlock) = 0.0
          end if
       end if

    end do; end do

    ! BEGINNING of rotating BC (jet) part
    if(IsJetBC)then
       do k = MinK, MaxK; do j = MinJ, MaxJ

          ! Get total B direction at surface
          FullB_D = State_VGB(Bx_:Bz_,1,j,k,iBlock) &
               + 0.5*(B0_DGB(:,0,j,k,iBlock) + B0_DGB(:,1,j,k,iBlock))
          Bdir_D = FullB_D/sqrt(max(sum(FullB_D**2), 1e-30))

          Xyz_D = Xyz_DGB(:,1,j,k,iBlock)
          r  = sqrt(sum(Xyz_D**2))

          ! Center is in the direction of the jet's rotation vector
          call rlonlat_to_xyz( &
               (/r, UserDipoleLongitude, UserDipoleLatitude/), &
               JetCenter_D &
               )
          DistanceJet = sqrt(sum((Xyz_D-JetCenter_D)**2))

          ! Calculate the rotation speed profile and velocity vector
          if(DistanceJet < DistMinJet .or. DistanceJet > DistMaxJet)then
             Urot_D = 0.0
          else
             Ucoeff = LinearCoeffJet*DistanceJet &
                  - PowerCoeffJet*DistanceJet**ProfileExponentJet

             CrossProduct_D = cross_product(JetCenter_D,Xyz_D)
             Urot_D = Ucoeff * CrossProduct_D / sqrt(sum(CrossProduct_D**2))
          endif

          do iFluid = IonFirst_, IonLast_
             iRho = iRho_I(iFluid)
             iRhoUx = iRhoUx_I(iFluid); iRhoUz = iRhoUz_I(iFluid)

             ! BC with reflected velocity for each cell
             ! (relative to the surface)
             do i = MinI, 0

                ! Calculate velocity in cell 1-i
                u_D = State_VGB(iRhoUx:iRhoUz,1-i,j,k,iBlock) &
                     /State_VGB(iRho,1-i,j,k,iBlock)
                u_D = u_D - Urot_D

                ! u_ghost = 2*u_par - u_cell
                u_D = 2*(sum(u_D*Bdir_D))*Bdir_D - u_D
                u_D = u_D + Urot_D

                State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock) = &
                     u_D*State_VGB(iRho,i,j,k,iBlock)
             end do
          end do
       end do;end do
       ! END of rotating BC (jet) part

    else
       do iFluid = IonFirst_, IonLast_
          iRho = iRho_I(iFluid)
          iRhoUx = iRhoUx_I(iFluid); iRhoUz = iRhoUz_I(iFluid)

          do k = MinK, MaxK; do j = MinJ, MaxJ
             ! Note that the Bdir_D calculation does not include the 
             ! CME part below
             FullB_D = State_VGB(Bx_:Bz_,1,j,k,iBlock) &
                  + 0.5*(B0_DGB(:,0,j,k,iBlock) + B0_DGB(:,1,j,k,iBlock))
             Bdir_D = FullB_D/sqrt(max(sum(FullB_D**2), 1e-30))

             ! Copy field-aligned velocity component.
             ! Reflect the other components
             do i = MinI, 0
                U_D = State_VGB(iRhoUx:iRhoUz,1-i,j,k,iBlock) &
                     /State_VGB(iRho,1-i,j,k,iBlock)
                U   = sum(U_D*Bdir_D); U_D = U_D - U*Bdir_D
                State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock) = &
                     (U*Bdir_D - U_D)*State_VGB(iRho,i,j,k,iBlock)
             end do
          end do; end do
       end do
    endif

    ! start of CME part
    if(UseCme)then
       do k = MinK, MaxK; do j = MinJ, MaxJ
          Runit_D = Xyz_DGB(:,1,j,k,iBlock) / r_BLK(1,j,k,iBlock)

          call EEE_get_state_BC(Runit_D, RhoCme, Ucme_D, Bcme_D, pCme, &
               time_simulation, n_step, iteration_number)

          RhoCme = RhoCme*Si2No_V(UnitRho_)
          Bcme_D = Bcme_D*Si2No_V(UnitB_)
          pCme   = pCme*Si2No_V(UnitP_)

          BrCme   = sum(Runit_D*Bcme_D)
          BrCme_D = BrCme*Runit_D
          do i = MinI, 0
             State_VGB(Rho_,i,j,k,iBlock) = State_VGB(Rho_,i,j,k,iBlock)+RhoCme
             if(UseElectronPressure)then
                State_VGB(Pe_,i,j,k,iBlock) = State_VGB(Pe_,i,j,k,iBlock) &
                     + 0.5*pCme
                State_VGB(p_,i,j,k,iBlock) = State_VGB(p_,i,j,k,iBlock) &
                     + 0.5*pCme

                if(UseAnisoPressure) State_VGB(Ppar_,i,j,k,iBlock) = &
                     State_VGB(Ppar_,i,j,k,iBlock) + 0.5*pCme
             else
                State_VGB(p_,i,j,k,iBlock) = State_VGB(p_,i,j,k,iBlock) + pCme
             end if
             State_VGB(Bx_:Bz_,i,j,k,iBlock) = &
                  State_VGB(Bx_:Bz_,i,j,k,iBlock) + BrCme_D

             ! If DoBqField = T, we need to modify the velocity components here 
             ! Currently, with the #CME command, we have always DoBqField = F
          end do
       end do; end do
    end if
    ! End of CME part

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_cell_boundary
  !============================================================================

  subroutine user_set_resistivity(iBlock, Eta_G)

    use ModAdvance,    ONLY: State_VGB
    use ModPhysics,    ONLY: No2Si_V, Si2No_V, UnitTemperature_, UnitX_, UnitT_
    use ModVarIndexes, ONLY: Rho_, Pe_

    integer, intent(in) :: iBlock
    real,    intent(out):: Eta_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)

    integer :: i, j, k
    real :: Te, TeSi

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_resistivity'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    do k = MinK,MaxK; do j = MinJ,MaxJ; do i = MinI,MaxI
       Te = TeFraction*State_VGB(Pe_,i,j,k,iBlock)/State_VGB(Rho_,i,j,k,iBlock)
       TeSi = Te*No2Si_V(UnitTemperature_)

       Eta_G(i,j,k) = EtaPerpSi/TeSi**1.5 *Si2No_V(UnitX_)**2/Si2No_V(UnitT_)
    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_resistivity
  !======================================================================

  subroutine user_get_b0(x, y, z, B0_D)

    use ModCoordTransform, ONLY: rlonlat_to_xyz
    use ModGeometry, ONLY: RadiusMin

    real, intent(in)   :: x, y, z
    real, intent(inout):: B0_D(3)

    real :: Xyz_D(3), Dp, rInv, r2Inv, r3Inv, Dipole_D(3)
    real :: UserDipoleAxis_D(3)

    character(len=*), parameter:: NameSub = 'user_get_b0'
    !--------------------------------------------------------------------------
    if(.not.IsPolarDipole)then
       UseUserB0 = .false.
       RETURN
    end if

    ! Center of dipole shifted by UserDipoleDepth below RadiusMin
    call rlonlat_to_xyz( &
         (/RadiusMin-UserDipoleDepth, &
         UserDipoleLongitude, UserDipoleLatitude/), &
         Xyz_D &
         )
    ! Normalize with depth
    Xyz_D = ((/x, y, z/) - Xyz_D)/UserDipoleDepth

    call rlonlat_to_xyz(&
         (/1.,UserDipoleAxisLongitude, UserDipoleAxisLatitude/), &
         UserDipoleAxis_D)

    ! Determine radial distance and powers of it
    rInv  = 1.0/sqrt(sum(Xyz_D**2))
    r2Inv = rInv**2
    r3Inv = rInv*r2Inv

    ! Compute dipole moment of the intrinsic magnetic field B0.
    Dipole_D = UserDipoleStrength * UserDipoleAxis_D

    Dp = 3*sum(Dipole_D*Xyz_D)*r2Inv

    B0_D = B0_D + (Dp*Xyz_D - Dipole_D) * r3Inv
  end subroutine user_get_b0
  !============================================================================

  subroutine user_initial_perturbation

    use EEE_ModMain,  ONLY: EEE_get_state_init
    use ModMain, ONLY: nI, nJ, nK, MaxBlock, unused_B, n_step, iteration_number
    use ModVarIndexes
    use ModAdvance,   ONLY: State_VGB, UseElectronPressure, UseAnisoPressure
    use ModPhysics,   ONLY: Si2No_V, UnitRho_, UnitP_, UnitB_
    use ModGeometry,  ONLY: Xyz_DGB
    use ModEnergy,    ONLY: calc_energy_cell
    use BATL_lib,     ONLY: nDim, MaxDim

    integer :: i, j, k, iBlock
    real :: x_D(nDim), Rho, B_D(MaxDim), p

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_initial_perturbation'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    do iBlock = 1, MaxBlock
       if(unused_B(iBlock))CYCLE

       do k = 1, nK; do j = 1, nJ; do i = 1, nI

          x_D = Xyz_DGB(:,i,j,k,iBlock)

          call EEE_get_state_init(x_D, Rho, B_D, p, n_step, iteration_number)

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
       call calc_energy_cell(iBlock)

    end do

    call test_stop(NameSub, DoTest)
  end subroutine user_initial_perturbation
  !============================================================================

end module ModUser
!==============================================================================
