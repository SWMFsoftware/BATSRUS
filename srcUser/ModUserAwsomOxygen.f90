!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
!#NOTPUBLIC  email:judithsz@umich.edu   expires:12/31/2099
module ModUser

  use BATL_lib, ONLY: &
       test_start, test_stop, iProc

  use ModVarIndexes, ONLY: IonFirst_, nFluid
  use ModMultiFluid, ONLY: nIonFluid
  use ModMain, ONLY: nI, nJ,nK
  use ModCoronalHeating, ONLY: PoyntingFluxPerB
  use ModUserEmpty,                                      &
       IMPLEMENTED1  => user_action,                     &
       IMPLEMENTED2  => user_read_inputs,                &
       IMPLEMENTED3  => user_init_session,               &
       IMPLEMENTED4  => user_set_ics,                    &
       IMPLEMENTED5  => user_get_log_var,                &
       IMPLEMENTED6  => user_set_plot_var,               &
       IMPLEMENTED7  => user_set_cell_boundary,          &
       IMPLEMENTED8  => user_set_resistivity,            &
       IMPLEMENTED9  => user_calc_sources_impl,          &
       IMPLEMENTED10 => user_init_point_implicit
  
  include 'user_module.h' ! list of public methods

  real, parameter :: VersionUserModule = 1.0
  character (len=*), parameter :: NameUserFile = &
       'ModUserAwsomOxygen.f90'
  character (len=*), parameter :: NameUserModule = &
       'AWSoM model with Oxygen charge states'

  ! Input parameters for chromospheric inner BC's
  real    :: NchromoSi_I(IonFirst_:nFluid) = 2e17, TchromoSi = 5e4
  real    :: Nchromo_I(IonFirst_:nFluid), Tchromo

  ! variables for Parker initial condition
  real    :: nCoronaSi = 1.5e14, tCoronaSi = 1.5e6

  ! Input parameters for two-temperature effects
  real    :: TeFraction, TiFraction
  real    :: EtaPerpSi

  real    :: Mass_I(0:nIonFluid), Charge_I(0:nIonFluid)
  real    :: ReducedMass_II(0:nIonFluid,0:nIonFluid)
  real    :: CollisionCoef_II(0:nIonFluid,0:nIonFluid)
  
  ! Multiply the collisionfrequencies by CollisionFactor
  real :: CollisionFactor = 1.0

  ! Charge state calculation variables
 ! For H:He 10:1 fully ionized plasma the proton:electron ratio is
    ! 1/(1+2*0.1)
  real                           :: ProtonElectronRatio = 0.83
  integer, allocatable           :: iTableElement_I(:)
  
contains
  !============================================================================
  subroutine user_action(NameAction)
    ! Initial condition for charge state calculation
    ! Can be used for both start and restart

    use ModVarIndexes,  ONLY: ScalarFirst_, nElement, Pe_, P_, Rho_
    use ModLookupTable, ONLY: Table_I,interpolate_lookup_table    
    use ModPhysics,     ONLY: UnitT_, No2Si_V, UnitTemperature_, Si2No_V, &
         UnitN_ 
    use ModAdvance,     ONLY: UseElectronPressure, State_VGB
    use BATL_lib,       ONLY: nBlock, nI, nJ, nK, Unused_B
    
    character(len=*),intent(in):: NameAction
    character(len=*), parameter:: NameSub = 'user_action'
    
    real, allocatable          :: Ioniz_I(:), Recomb_I(:)
    real                       :: Value_I(2), Te, TeSi
    integer                    :: iVar, iElement, iCharge, nCharge
    integer                    :: iBlock, i, j, k
    real                       :: AbundanceElement, MassElement
    !-------------------------------------------------------------------------- 
    select case(NameAction)
    case('initial condition done')
       ! Charge state initial condition is ionization equilibrium 
       do iBlock = 1, nBlock
          if(Unused_B(iBlock))CYCLE
          do k = 1, nK ; do j = 1, nJ ; do i = 1, nI
             if(UseElectronPressure)then
                Te = State_VGB(Pe_,i,j,k,iBlock)/State_VGB(Rho_,i,j,k,iBlock)
             else
                Te = State_VGB(p_,i,j,k,iBlock)/State_VGB(Rho_,i,j,k,iBlock)
             endif

             TeSi = Te * No2Si_V(UnitTemperature_)

             iVar = ScalarFirst_
             do iElement = 1, nElement
                nCharge = nint(Table_I(iTableElement_I(iElement))%Param_I(1))
                MassElement = Table_I(iTableElement_I(iElement))%Param_I(2)
                AbundanceElement = Table_I(iTableElement_I(iElement))%Param_I(3)
                allocate(Ioniz_I(0:nCharge),Recomb_I(0:nCharge))
                do iCharge = 0, nCharge
                   call interpolate_lookup_table(iTableElement_I(iElement), &
                        real(iCharge), TeSi, Value_I, DoExtrapolate = .false.)
                   Ioniz_I(iCharge) =&
                        Value_I(1)*1e-6/Si2No_V(UnitN_)/Si2No_V(UnitT_)
                   Recomb_I(iCharge) = &
                        Value_I(2)*1e-6/Si2No_V(UnitN_)/Si2No_V(UnitT_)
                end do
                ! initial guess for y_{0}
                State_VGB(iVar,i,j,k,iBlock) = 1.
                ! y_1*R_1 = y_0*I_0          
                State_VGB(iVar+1,i,j,k,iBlock) = &
                     State_VGB(ScalarFirst_,i,j,k,iBlock) * &
                     Ioniz_I(0)/Recomb_I(1)
                ! 0 = y_{m-1} * I_{m-1} + y_m*(I_m+R_m) - y_{m+1}*R_{m+1} 
                do iCharge = 2, nCharge
                   State_VGB(iVar+iCharge,i,j,k,iBlock) = &
                        State_VGB(iVar+iCharge-1,i,j,k,iBlock) * &
                        (Recomb_I(iCharge-1)+Ioniz_I(iCharge-1))/&
                        Recomb_I(iCharge) - &
                        State_VGB(iVar+iCharge-2,i,j,k,iBlock) * &
                        Ioniz_I(iCharge-2)/Recomb_I(iCharge)
                end do
                ! normalize to 1 and multiply by proton mass density and
                ! element mass relative to proton mass and element abundance
                State_VGB(iVar:iVar+nCharge,i,j,k,iBlock) = &
                     State_VGB(iVar:iVar+nCharge,i,j,k,iBlock) / &
                     sum(State_VGB(iVar:iVar+nCharge,i,j,k,iBlock)) &
                     *AbundanceElement*MassElement &
                     *State_VGB(Rho_,i,j,k,iBlock)

                iVar = iVar + nCharge + 1
                deallocate(Ioniz_I,Recomb_I)
             end do
          end do; end do; end do
       end do
    end select

  end subroutine user_action
  !============================================================================
  subroutine user_read_inputs

    use ModMain,       ONLY: lVerbose
    use ModReadParam,  ONLY: read_line, read_command, read_var
    use ModIO,         ONLY: write_prefix, write_myname, iUnitOut

    integer :: iFluid
    character (len=100) :: NameCommand

    integer :: iElement
    
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

    use ModIO,          ONLY: write_prefix, iUnitOut
    use ModWaves,       ONLY: UseWavePressure, UseAlfvenWaves
    use ModAdvance,     ONLY: UseElectronPressure
    use ModMultiFluid,  ONLY: MassIon_I, ChargeIon_I
    use ModConst,       ONLY: cElectronCharge, cLightSpeed, cBoltzmann, cEps, &
         cElectronMass, cProtonMass
    use ModNumConst,    ONLY: cTwoPi, cDegToRad
    use ModPhysics,     ONLY: ElectronTemperatureRatio, AverageIonCharge, &
         Si2No_V, UnitTemperature_, UnitN_, UnitX_, No2Si_V, UnitT_
    use ModVarIndexes,  ONLY: ScalarFirst_, ScalarLast_, NameVar_V, &
         NameElement_I, nElement 
    use ModLookupTable, ONLY: i_lookup_table, Table_I
    
    integer :: iIon, jIon, iVar
    real, parameter :: CoulombLog = 20.0

    integer :: iElement, iCharge, nCharge
    character(len=2) :: NameChSt
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
    Nchromo_I = NchromoSi_I*Si2No_V(UnitN_)
    Tchromo = TchromoSi*Si2No_V(UnitTemperature_)

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

    Mass_I(0) = cElectronMass/cProtonMass
    Mass_I(1:) = MassIon_I
    Charge_I(0) = 1.0
    Charge_I(1:) = ChargeIon_I

    ! Coefficient for effective ion-ion collision frequencies
    do jIon = 0, nIonFluid
       do iIon = 0, nIonFluid
          ReducedMass_II(iIon,jIon) = Mass_I(iIon)*Mass_I(jIon) &
               /(Mass_I(iIon) + Mass_I(jIon))

          CollisionCoef_II(iIon,jIon) = CollisionFactor*CoulombLog &
               *sqrt(ReducedMass_II(iIon,jIon)/cProtonMass)/Mass_I(iIon) &
               *(Charge_I(iIon)*Charge_I(jIon)*cElectronCharge**2/cEps)**2 &
               /(3*(cTwoPi*cBoltzmann)**1.5)
       end do
    end do
    ! To obtain the effective ion-ion collision frequencies, the
    ! coefficients still need to be multiplied by Nion(jIon)/reducedTemp**1.5.
    ! Here, we already take care of the units.
    CollisionCoef_II = CollisionCoef_II &
         *(1/Si2No_V(UnitT_))*No2Si_V(UnitN_)/No2Si_V(UnitTemperature_)**1.5

    ! Read in charge state lookup table(s)
    if(nElement > 0)then
       if(allocated(iTableElement_I)) deallocate(iTableElement_I)
       allocate(iTableElement_I(nElement))
       do iElement = 1, nElement
          iTableElement_I(iElement) = &
               i_lookup_table('ionization_'//trim(NameElement_I(iElement)))
          if(.not. iTableElement_I(iElement) > 0)&
               call stop_mpi('table requited for element '// &
               NameElement_I(iElement))
       end do
    endif
    
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

    use ModAdvance,    ONLY: State_VGB, UseAnisoPressure, UseElectronPressure
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
    use ModFaceValue,      ONLY: calc_face_value
    use ModMultiFluid,     ONLY: IonLast_
    use ModPhysics,        ONLY: No2Si_V, UnitT_, UnitEnergyDens_
    use ModVarIndexes,     ONLY: WaveFirst_, WaveLast_, ScalarFirst_

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

    logical                        :: IsNewBlockAlfven
    
    integer :: i, j, k
    real :: QPerQtotal_I(IonFirst_:IonLast_)
    real :: QparPerQtotal_I(IonFirst_:IonLast_)
    real :: QePerQtotal

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_plot_var'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    IsFound = .true.

    select case(NameVar)
    case('refl')
       Source_VC(WaveFirst_:WaveLast_,:,:,:) = 0.0
       call set_b0_face(iBlock)
       call calc_face_value(iBlock, DoResChangeOnly = .false., DoMonotoneRestrict = .false.)
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
       call calc_face_value(iBlock, DoResChangeOnly = .false., DoMonotoneRestrict = .false.)
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
          call calc_face_value(iBlock, DoResChangeOnly = .false., DoMonotoneRestrict = .false.)
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
       ! Not yet generalized to multi-fluid
       if(UseElectronPressure)then
          call set_b0_face(iBlock)
          call calc_face_value(iBlock, DoResChangeOnly = .false., DoMonotoneRestrict = .false.)

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

  subroutine user_set_cell_boundary(iBlock,iSide, CBC, IsFound)

    ! Fill ghost cells inside body for spherical grid - this subroutine only
    ! modifies ghost cells in the r direction

    use BATL_lib,       ONLY: CellSize_DB, x_, y_, z_
    use ModAdvance,     ONLY: State_VGB, UseElectronPressure, UseAnisoPressure
    use ModB0,          ONLY: B0_DGB
    use ModGeometry,    ONLY: TypeGeometry, Xyz_DGB, r_BLK
    use ModHeatFluxCollisionless, ONLY: UseHeatFluxCollisionless, &
         get_gamma_collisionless
    use ModPhysics,     ONLY: InvGammaMinus1, GBody, rBody, No2Si_V, Si2No_V, &
         UnitTemperature_, UnitX_, UnitT_, UnitN_
    use ModVarIndexes,  ONLY: Pe_, Bx_, Bz_, WaveFirst_, WaveLast_, Ew_, &
         Ehot_, p_, ScalarFirst_, Rho_, nElement
    use ModMultiFluid,  ONLY: MassIon_I, iRhoIon_I, ChargeIon_I, IonLast_, &
         iRho_I, MassFluid_I, iUx_I, iUz_I, iRhoUx_I, iRhoUz_I, iPIon_I, &
         iP_I, iPparIon_I, IsIon_I
    use ModImplicit,    ONLY: StateSemi_VGB, iTeImpl
    use ModWaves,       ONLY: UseWavePressureLtd
    use ModLookupTable, ONLY: Table_I,interpolate_lookup_table
    use ModMain,        ONLY: CellBCType
    
    integer,          intent(in)  :: iBlock, iSide
    type(CellBCType), intent(in)  :: CBC
    logical,          intent(out) :: IsFound

    integer :: Minor_, Major_
    integer :: i, j, k, iFluid, iRho, iRhoUx, iRhoUz, iP
    real    :: Br1_D(3), Bt1_D(3), Runit_D(3)
    real    :: FullB_D(3), SignBr
    real    :: Gamma
    real    :: U, U_D(3), Bdir_D(3)

    ! Charge state variables
    real, allocatable          :: Ioniz_I(:), Recomb_I(:)
    real                       :: Value_I(2),Te, TeSi
    integer                    :: iVar, iElement, iCharge, nCharge
    real                       :: AbundanceElement, MassElement
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
    case('user')
       IsFound = .true.
    case default
       IsFound = .false.
       RETURN
    end select

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

    do iFluid = IonFirst_, IonLast_
       iRho = iRho_I(iFluid)
       iRhoUx = iRhoUx_I(iFluid); iRhoUz = iRhoUz_I(iFluid)

       do k = MinK, MaxK; do j = MinJ, MaxJ
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

    ! Charge state boundary conditions
    do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, 0
       
       if(UseElectronPressure)then
          Te = State_VGB(Pe_,i,j,k,iBlock)/State_VGB(Rho_,i,j,k,iBlock)
       else
          Te = State_VGB(p_,i,j,k,iBlock)/State_VGB(Rho_,i,j,k,iBlock)
       endif

       TeSi = Te * No2Si_V(UnitTemperature_)

       iVar = ScalarFirst_
       do iElement = 1, nElement
          nCharge = nint(Table_I(iTableElement_I(iElement))%Param_I(1))
          MassElement = Table_I(iTableElement_I(iElement))%Param_I(2)
          AbundanceElement = Table_I(iTableElement_I(iElement))%Param_I(3)

          allocate(Ioniz_I(0:nCharge),Recomb_I(0:nCharge))
          do iCharge = 0, nCharge
             call interpolate_lookup_table(iTableElement_I(iElement), &
                  real(iCharge), TeSi, Value_I, DoExtrapolate = .false.)
             Ioniz_I(iCharge) = Value_I(1)*1e-6/Si2No_V(UnitN_)/Si2No_V(UnitT_)
             Recomb_I(iCharge) = Value_I(2)*1e-6/Si2No_V(UnitN_)/Si2No_V(UnitT_)
          end do
          ! initial guess for y_{0}
          State_VGB(iVar,i,j,k,iBlock) = 1.
          ! y_1*R_1 = y_0*I_0          
          State_VGB(iVar+1,i,j,k,iBlock) = &
               State_VGB(ScalarFirst_,i,j,k,iBlock) * Ioniz_I(0)/Recomb_I(1)
          ! 0 = y_{m-1} * I_{m-1} + y_m*(I_m+R_m) - y_{m+1}*R_{m+1} 
          do iCharge = 2, nCharge
             State_VGB(iVar+iCharge,i,j,k,iBlock) = &
                  State_VGB(iVar+iCharge-1,i,j,k,iBlock) * &
                  (Recomb_I(iCharge-1)+Ioniz_I(iCharge-1))/Recomb_I(iCharge) - &
                  State_VGB(iVar+iCharge-2,i,j,k,iBlock) * &
                  Ioniz_I(iCharge-2)/Recomb_I(iCharge)
          end do

          ! normalize to 1 and multiply by proton mass density and
          ! element mass relative to proton mass and element abundance
          State_VGB(iVar:iVar+nCharge,i,j,k,iBlock) = &
               State_VGB(iVar:iVar+nCharge,i,j,k,iBlock) / &
               sum(State_VGB(iVar:iVar+nCharge,i,j,k,iBlock)) &
               *AbundanceElement*MassElement &
               *State_VGB(Rho_,i,j,k,iBlock)

          iVar = iVar + nCharge + 1
          deallocate(Ioniz_I,Recomb_I)
       end do
    end do; end do; end do

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
  !============================================================================

  subroutine user_calc_sources_impl(iBlock)

    use ModAdvance,     ONLY: State_VGB, Source_VC, UseElectronPressure
    use ModVarIndexes,  ONLY: nVar, Rho_, p_, Pe_, ScalarFirst_, ScalarLast_, &
         nElement
    use ModPhysics,     ONLY: No2Si_V, Si2No_V, UnitTemperature_,UnitT_,UnitN_
    use ModLookupTable, ONLY: Table_I, interpolate_lookup_table
    integer, intent(in) :: iBlock

    integer :: i, j, k

    real :: State_V(nVar), Source_V(nVar+1)

    real                       :: Value_I(2)
    real                       :: Ne, Te, TeSi, Source
    integer                    :: iCharge, nCharge, iElement, iVar

    real, allocatable          :: Ioniz_I(:), Recomb_I(:)
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_calc_sources_impl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    do k = 1, nK; do j = 1, nJ; do i = 1, nI

       State_V = State_VGB(:,i,j,k,iBlock)
       Source_V = 0.0

       Ne = State_V(Rho_) * ProtonElectronRatio
       if(UseElectronPressure)then
          Te = State_V(Pe_)/State_V(Rho_)
       else
          Te = State_V(p_)/State_V(Rho_)
       endif

       TeSi = Te * No2Si_V(UnitTemperature_)

       iVar = ScalarFirst_
       do iElement = 1, nElement
          nCharge = nint(Table_I(iTableElement_I(iElement))%Param_I(1))
          allocate(Ioniz_I(0:nCharge),Recomb_I(0:nCharge))
          do iCharge = 0, nCharge
             call interpolate_lookup_table(iTableElement_I(iElement), &
                  real(iCharge), TeSi, Value_I, DoExtrapolate = .false.)
             Ioniz_I(iCharge) = Value_I(1)*1e-6/Si2No_V(UnitN_)/Si2No_V(UnitT_)
             Recomb_I(iCharge) = Value_I(2)*1e-6/Si2No_V(UnitN_)/Si2No_V(UnitT_)
          end do
          
          do iCharge = 0, nCharge
             Source = 0
             if(iCharge /= 0)Source = State_V(iVar-1)*Ioniz_I(iCharge-1)
             Source = Source-State_V(iVar)*(Ioniz_I(iCharge)+Recomb_I(iCharge)) 
             if(iCharge /= nCharge)Source = Source + &
                  State_V(iVar+1)*Recomb_I(iCharge+1)
             Source_V(iVar) = Ne*Source
             iVar = iVar + 1
          end do
          deallocate(Ioniz_I,Recomb_I)
       end do

       Source_VC(:,i,j,k) = Source_VC(:,i,j,k) + Source_V

    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_calc_sources_impl
  !============================================================================
  subroutine user_init_point_implicit

    use ModPointImplicit, ONLY: iVarPointImpl_I, IsPointImplMatrixSet
    use ModVarIndexes, ONLY: nVar, ScalarFirst_, ScalarLast_

    logical :: IsPointImpl_V(nVar)
    integer :: iVar, iPointImplVar, nPointImplVar

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_init_point_implicit'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    IsPointImpl_V = .false.

    ! Charge state variables
    IsPointImpl_V(ScalarFirst_:ScalarLast_) = .true.
    
    nPointImplVar = count(IsPointImpl_V)

    allocate(iVarPointImpl_I(nPointImplVar))

    ! Shifts indices to start from 1
    iPointImplVar = 0
    do iVar = 1, nVar
       if(.not. IsPointImpl_V(iVar)) CYCLE
       iPointImplVar = iPointImplVar + 1
       iVarPointImpl_I(iPointImplVar) = iVar
    end do

    ! Tell the point implicit scheme if dS/dU will be set analytically
    ! If this is set to true the DsDu_VVC matrix has to be set below.
    IsPointImplMatrixSet = .false.

    call test_stop(NameSub, DoTest)
  end subroutine user_init_point_implicit
  !============================================================================
  
end module ModUser
!==============================================================================
