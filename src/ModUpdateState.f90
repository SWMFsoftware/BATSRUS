!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModUpdateState

  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest, iBlockTest, &
       iVarTest, iComm, Used_GB, CellVolume_GB, Xyz_DGB
  use ModConservative, ONLY: IsConserv_CB, UseNonConservative, nConservCrit

  implicit none

  private ! except

  public:: update_state         ! call user_update_state or update_state_normal
  public:: update_state_normal  ! normal update of state variables
  public:: update_b0            ! update time varying B0 field
  public:: update_te0           ! update Te0 variable
  public:: update_check         ! check and correct update if necessary
  public:: fix_anisotropy       ! fix pressure anisotropy after update
  public:: check_nan            ! Check State_VGB for NaNs

contains
  !============================================================================
  subroutine update_state(iBlock)

    use ModMain, ONLY: nStep, iStage, Cfl, UseUserUpdateStates, UseBufferGrid
    use ModVarIndexes, ONLY: nVar, Rho_, RhoUx_, RhoUz_, Ehot_, SignB_, &
         NameVar_V, nFluid
    use ModAdvance, ONLY: State_VGB, StateOld_VGB, DTMAX_CB, &
         Flux_VXI, Flux_VYI, Flux_VZI, Source_VC, &
         nVarUpdate, iVarUpdate_I, DoUpdate_V
    use ModPhysics, ONLY: &
         No2Si_V, No2Io_V, UnitT_, UnitU_, UnitRhoU_, iUnitCons_V
    use ModChGL, ONLY: UseChGL, update_chgl
    use ModEnergy, ONLY: limit_pressure
    use ModHeatFluxCollisionless, ONLY: UseHeatFluxCollisionless, &
         update_heatflux_collisionless
    use ModBuffer, ONLY: fix_buffer_grid
    use BATL_lib, ONLY: nI, nJ, nK
    use ModUserInterface ! user_update_states

    integer, intent(in) :: iBlock
    integer :: iVar, i, j, k
    integer, parameter:: iGang=1
    real :: Rho

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'update_state'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(DoTest)then
       write(*,*)NameSub,' nStep=', nStep,' iStage=', iStage,     &
            ' dt=',DtMax_CB(iTest,jTest,kTest,iBlock)*Cfl, &
            ' dtSI=',DtMax_CB(iTest,jTest,kTest,iBlock)*Cfl*No2Si_V(UnitT_)
       if(allocated(IsConserv_CB)) write(*,*)NameSub,' IsConserv=', &
            IsConserv_CB(iTest,jTest,kTest,iBlock)
       write(*,*)
       do iVar = 1, nVar
          write(*,'(2x,2a,2es23.15)')NameVar_V(iVar), '(TestCell)  =',&
               State_VGB(iVar,iTest,jTest,kTest,iBlockTest), &
               State_VGB(iVar,iTest,jTest,kTest,iBlockTest) &
               *No2Io_V(iUnitCons_V(iVar))
          if(iVar >= RhoUx_ .and. iVar <= RhoUz_)then
             Rho = State_VGB(Rho_,iTest,jTest,kTest,iBlockTest)
             write(*,'(2x,a,2es23.15)') &
                  'Velocity (TestCell)  =',&
                  State_VGB(iVar,iTest,jTest,kTest,iBlockTest)/Rho, &
                  State_VGB(iVar,iTest,jTest,kTest,iBlockTest)/Rho &
                  *No2Io_V(UnitU_)
          end if
       end do
       write(*,*)'Fluxes and sources for ', NameVar_V(iVarTest)
       write(*,'(2x,a,2es23.15)') &
            'X fluxes L,R =',Flux_VXI(iVarTest,iTest,jTest,kTest,iGang) ,&
            Flux_VXI(iVarTest,iTest+1,jTest,kTest,iGang)
       write(*,'(2x,a,2es23.15)') &
            'Y fluxes L,R =',Flux_VYI(iVarTest,iTest,jTest,kTest,iGang) ,&
            Flux_VYI(iVarTest,iTest,jTest+1,kTest,iGang)
       write(*,'(2x,a,2es23.15)') &
            'Z fluxes L,R =',Flux_VZI(iVarTest,iTest,jTest,kTest,iGang) ,&
            Flux_VZI(iVarTest,iTest,jTest,kTest+1,iGang)
       write(*,'(2x,a,es23.15)')'source=',&
            Source_VC(iVarTest,iTest,jTest,kTest)
       write(*,'(2x,a,es23.15)')'fluxes=', &
            +(Flux_VXI(iVarTest,iTest,jTest,kTest,iGang)    &
            -Flux_VXI(iVarTest,iTest+1,jTest,kTest,iGang)   &
            +Flux_VYI(iVarTest,iTest,jTest,kTest,iGang)     &
            -Flux_VYI(iVarTest,iTest,jTest+1,kTest,iGang)   &
            +Flux_VZI(iVarTest,iTest,jTest,kTest,iGang)     &
            -Flux_VZI(iVarTest,iTest,jTest,kTest+1,iGang) ) &
            /CellVolume_GB(iTest,jTest,kTest,iBlockTest)
    end if

    ! Note must copy state to old state only if iStage is 1.
    if(iStage==1) &
         StateOld_VGB(:,:,:,:,iBlock) = State_VGB(:,:,:,:,iBlock)

    ! The first call may set UseUserUpdateStates to false
    if(UseUserUpdateStates)       call user_update_states(iBlock)
    if(.not. UseUserUpdateStates) call update_state_normal(iBlock)

    call limit_pressure(1, nI, 1, nJ, 1, nK, iBlock, 1, nFluid)

    if(Ehot_ > 1 .and. UseHeatFluxCollisionless) then
       call update_heatflux_collisionless(iBlock)
       if(UseBufferGrid) call fix_buffer_grid(iBlock)
    end if
    if(SignB_ > 1 .and. UseChGL)call update_chgl(iBlock, iStage)

    if(DoTest)then
       write(*,*)NameSub,' final for nStep =', nStep
       do iVar=1,nVar
          write(*,'(2x,2a,es23.15)')NameVar_V(iVar),'(TestCell)  =',&
               State_VGB(iVar,iTest,jTest,kTest,iBlockTest)
       end do
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine update_state
  !============================================================================
  subroutine update_state_normal(iBlock)

    use ModAdvance
    use ModMain, ONLY: &
         IsTimeAccurate, iStage, nStage, Dt, Cfl, UseBufferGrid, &
         UseHalfStep, UseFlic, UseUserSourceImpl, UseHyperbolicDivB, HypDecay
    use ModPhysics, ONLY: InvGammaElectron, GammaElectron, RhoMin_I
    use ModSemiImplVar, ONLY: UseStableImplicit
    use ModVarIndexes, ONLY: pe_, p_
    use ModPointImplicit, ONLY: UsePointImplicit, UseUserPointImplicit_B, &
         IsDynamicPointImplicit, update_point_implicit
    use ModMultiIon, ONLY: multi_ion_source_impl, &
         multi_ion_set_restrict, DoRestrictMultiIon
    use ModEnergy, ONLY: energy_to_pressure, pressure_to_energy, limit_pressure
    use ModWaves, ONLY: nWave, WaveFirst_,WaveLast_, &
         UseWavePressure, UseWavePressureLtd, UseAlfvenWaves, DoAdvectWaves, &
         update_wave_group_advection
    use ModResistivity, ONLY: UseResistivity, UseResistiveFlux, &
         calc_resistivity_source
    use ModUserInterface
    use ModBuffer,      ONLY: fix_buffer_grid
    use ModIonElectron, ONLY: ion_electron_source_impl, &
         HypEDecay
    use ModMultiFluid,  ONLY: ChargePerMass_I, iRhoUxIon_I, iRhoUyIon_I, &
         iRhoUzIon_I, iPIon_I, nIonFluid, UseNeutralFluid, DoConserveNeutrals

    integer, intent(in) :: iBlock

    integer :: i, j, k, iVar
    integer, parameter:: iGang=1

    ! These variables have to be double precision for accurate Boris scheme
    real:: DtLocal, DtFactor, SourceIonEnergy_I(nIonFluid)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'update_state_normal'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! Nothing to do if time step is zero
    if(IsTimeAccurate .and. Dt == 0.0) RETURN

    ! Add Joule heating: dPe/dt or dP/dt += (gamma-1)*eta*j**2
    ! also dE/dt += eta*j**2 for semi-implicit scheme (UseResistiveFlux=F)
    ! and heat exchange between electrons and ions (mult-ion is not coded).

    if(.not.UseMultiIon .and. UseResistivity .and. &
         (UseElectronPressure .or. UseNonConservative .or. &
         .not.UseResistiveFlux)) then

       call calc_resistivity_source(iBlock)
       if(DoTest)write(*,'(2x,2a,15es20.12)') &
            NameSub, ' after add_resistive_source          =', &
            State_VGB(iVarTest,iTest,jTest,kTest,iBlock)
    end if

    ! Calculate partial step size compared to largest stable time step
    if(nStage==4.or.UseFlic)then
       ! Classical 4th order Runge-Kutta scheme
       select case(iStage)
       case(1)
          DtFactor = Cfl/2
       case(2)
          DtFactor = Cfl/2
       case(3)
          DtFactor = Cfl
       case(4)
          DtFactor = Cfl/6
       end select
    elseif(UseHalfStep)then
       DtFactor = (Cfl*iStage)/nStage
    else
       DtFactor = Cfl
    end if

    ! Modify electron pressure source term to electron entropy if necessary
    ! d(Se)/d(Pe) = Pe^(1/gammaE-1)/gammaE
    if(UseElectronPressure .and. UseElectronEntropy)then
       do k = 1,nK; do j = 1,nJ; do i = 1,nI
          Source_VC(Pe_,i,j,k) = Source_VC(Pe_,i,j,k)*InvGammaElectron &
               * State_VGB(Pe_,i,j,k,iBlock)**(InvGammaElectron - 1.0)
       end do; end do; end do
    end if

    do k = 1,nK; do j = 1,nJ; do i = 1,nI; do iVar = 1, nVar+nFluid
       DtLocal = DtFactor*DtMax_CB(i,j,k,iBlock)
       Source_VC(iVar,i,j,k) = &
            DtLocal* (Source_VC(iVar,i,j,k) + &
            ( Flux_VXI(iVar,i,j,k,iGang)  - Flux_VXI(iVar,i+1,j,k,iGang)  &
            + Flux_VYI(iVar,i,j,k,iGang)  - Flux_VYI(iVar,i,j+1,k,iGang)  &
            + Flux_VZI(iVar,i,j,k,iGang)  - Flux_VZI(iVar,i,j,k+1,iGang)  ) &
            /CellVolume_GB(i,j,k,iBlock) )
    end do; end do; end do; end do

    if(UseMultiIon .and. DoRestrictMultiIon)call multi_ion_set_restrict(iBlock)

    if(DoTest)write(*,'(2x,2a,15es20.12)') &
         NameSub, ' original testvar and energy         =', &
         State_VGB(iVarTest,iTest,jTest,kTest,iBlock)

    call update_explicit(iBlock, DoTest)

    if(UseMultiIon .and. &
         (UseSingleIonVelocity .or. UseSingleIonTemperature)) then

       call fix_multi_ion_update(iBlock)
       call limit_pressure(1, nI, 1, nJ, 1, nK, iBlock, 1, nFluid)

       if(DoTest)write(*,'(2x,2a,15es20.12)') &
            NameSub, ' after fix multiion update           =', &
            State_VGB(iVarTest,iTest,jTest,kTest,iBlock)

    end if

    ! The point implicit update and other stuff below are only done in
    ! the last stage except for ion-electron equations where the point
    ! implicit has to be done in every stage.
    if(.not.UseEfield .and. iStage < nStage)then
       if(UseBufferGrid) call fix_buffer_grid(iBlock)
       RETURN
    end if

    ! Add point implicit user or multi-ion source terms
    if(UsePointImplicit)then
       if(IsDynamicPointImplicit .or. UseUserPointImplicit_B(iBlock)) then
          if(UseEfield)then
             call update_point_implicit(iBlock, ion_electron_source_impl)
          elseif(UseMultiIon .and. .not.UseSingleIonVelocity)then
             call update_point_implicit(iBlock, multi_ion_source_impl)
          elseif(UseUserSourceImpl) then
             call update_point_implicit(iBlock, user_calc_sources_impl)
          end if

          ! Make ion temperatures equal if requested
          if(UseMultiIon .and. &
               (UseSingleIonVelocity .or. UseSingleIonTemperature)) &
               call fix_multi_ion_update(iBlock)

          ! Make sure that energy is consistent
          if(UseEfield)then
             if(.not. UseNonconservative) then

                call pressure_to_energy(iBlock, State_VGB)

                ! Add q/m rhou.E to ion energy source terms for the
                ! energy equation, q/m*rho*(E dot u) if UseEfield.
                ! Tests show that putting the energy source terms after
                ! the point implicit update is more stable than putting
                ! the source terms in ModCalcSource.
                do k=1,nK; do j=1,nJ; do i=1,nI
                   DtLocal = DtFactor*DtMax_CB(i,j,k,iBlock)

                   SourceIonEnergy_I = ChargePerMass_I* (         &
                        State_VGB(Ex_,i,j,k,iBlock)               &
                        *State_VGB(iRhoUxIon_I,i,j,k,iBlock)    + &
                        State_VGB(Ey_,i,j,k,iBlock)               &
                        *State_VGB(iRhoUyIon_I,i,j,k,iBlock)    + &
                        State_VGB(Ez_,i,j,k,iBlock)               &
                        *State_VGB(iRhoUzIon_I,i,j,k,iBlock) )

                   SourceIonEnergy_I = SourceIonEnergy_I*DtLocal

                   State_VGB(iPIon_I,i,j,k,iBlock) =     &
                        State_VGB(iPIon_I,i,j,k,iBlock)  &
                        + SourceIonEnergy_I
                end do; end do; end do

                ! Re-calculate the pressure from the energy.
                ! In this case, the pressure source terms in the user file
                ! would not contribute to the pressure. It requires that
                ! the user provide the corresponding source terms for
                ! the energy equation in the user file.
                call energy_to_pressure(iBlock, State_VGB)
             else
                ! write(*,*) NameSub,' !!! call limit_pressure'
                call limit_pressure(1, nI, 1, nJ, 1, nK, iBlock, 1, nFluid)
             end if
          else
             ! write(*,*) NameSub,' !!! call limit_pressure'
             call limit_pressure(1, nI, 1, nJ, 1, nK, iBlock, 1, nFluid)
          end if

          if(DoTest)write(*,'(2x,2a,15es20.12)') &
               NameSub, ' after point impl state              =', &
               State_VGB(iVarTest, iTest,jTest,kTest,iBlock)
       end if
    end if

    ! The parabolic div B decay is only done in the last stage.
    if(UseHyperbolicDivb .and. HypDecay > 0 .and. iStage == nStage) &
         State_VGB(Hyp_,1:nI,1:nJ,1:nK,iBlock) = &
         State_VGB(Hyp_,1:nI,1:nJ,1:nK,iBlock)*(1 - HypDecay)

    if(UseEfield .and. HypEDecay > 0 .and. iStage == nStage) &
         State_VGB(HypE_,1:nI,1:nJ,1:nK,iBlock) = &
         State_VGB(HypE_,1:nI,1:nJ,1:nK,iBlock)*(1 - HypEDecay)

    if(UseStableImplicit) call deduct_expl_source

    if(UseBufferGrid) call fix_buffer_grid(iBlock)

    if(DoTest)write(*,'(2x,2a,15es20.12)') &
         NameSub, ' final state                         =', &
         State_VGB(iVarTest,iTest,jTest,kTest,iBlock)

    call test_stop(NameSub, DoTest, iBlock)
  contains
    !==========================================================================
    subroutine update_explicit(iBlock, DoTest)
      use ModBorisCorrection, ONLY: UseBorisCorrection, UseBorisSimple, &
           mhd_to_boris, boris_to_mhd

      integer, intent(in):: iBlock
      logical, intent(in):: DoTest

      ! Allocatable storage for classical 4th order Runge-Kutta scheme
      real, allocatable, save:: Rk4_VCB(:,:,:,:,:)

      real, parameter:: cThird = 1./3.
      real:: Coeff1, Coeff2
      integer:: iFluid, iRho
      integer:: i, j, k, iVar
      !------------------------------------------------------------------------
      ! Convert pressure to energy for the conservative scheme
      call pressure_to_energy(iBlock, StateOld_VGB)

      if(.not.(UseHalfStep .or. nStage == 1 .or. nStage == 4)) &
         call pressure_to_energy(iBlock, State_VGB)

      if(UseBorisCorrection .or. UseBorisSimple .and. IsMhd) then
         ! Convert classical momentum and energy to relativistic
         call mhd_to_boris(iBlock)

         if(DoTest)write(*,'(2x,2a,15es20.12)') &
              NameSub, ' after mhd_to_boris                  =', &
              State_VGB(iVarTest,iTest,jTest,kTest,iBlock)
      endif

      if(UseElectronPressure .and. UseElectronEntropy)then
         ! Convert electron pressure to entropy
         ! Se = Pe^(1/GammaE)
         do k=1,nK; do j=1,nJ; do i=1,nI
            if(.not.Used_GB(i,j,k,iBlock)) CYCLE

            StateOld_VGB(Pe_,i,j,k,iBlock) = &
                 StateOld_VGB(Pe_,i,j,k,iBlock)**(1/GammaElectron)
            ! State_VGB is not used in 1-stage and HalfStep schemes
            if(.not.UseHalfStep .and. nStage > 1) &
                 State_VGB(Pe_,i,j,k,iBlock) = &
                 State_VGB(Pe_,i,j,k,iBlock)**(1/GammaElectron)
         end do; end do; end do
      end if

      ! Move energy source terms to pressure index as needed
      ! Ions first
      if(.not.UseNonConservative)then
         do k = 1, nK; do j = 1, nJ; do i =1, nI
            if(.not.Used_GB(i,j,k,iBlock)) CYCLE
            Source_VC(iP_I(1:IonLast_),i,j,k) = &
                 Source_VC(Energy_:Energy_+IonLast_-1,i,j,k)
         end do; end do; end do
      elseif(nConservCrit > 0)then
         do k = 1, nK; do j = 1, nJ; do i = 1, nI
            if(.not.Used_GB(i,j,k,iBlock)) CYCLE
            if(.not.IsConserv_CB(i,j,k,iBlock)) CYCLE
            Source_VC(iP_I(1:IonLast_),i,j,k) = &
                 Source_VC(Energy_:Energy_+IonLast_-1,i,j,k)
         end do; end do; end do
      end if
      ! Neutrals next
      if(UseNeutralFluid .and. DoConserveNeutrals)then
         do k = 1, nK; do j = 1, nJ; do i = 1, nI
            if(.not.Used_GB(i,j,k,iBlock)) CYCLE
            Source_VC(iP_I(IonLast_+1:),i,j,k) = &
                 Source_VC(Energy_+IonLast_:,i,j,k)
         end do; end do; end do
      end if

      if(nVarUpdate /= nVar .and. DoUpdate_V(Rho_))then
         ! Convert to velocity when momentum is not updated
         do iVar = RhoUx_, RhoUz_
            if(DoUpdate_V(iVar)) CYCLE
            do k = 1, nK; do j = 1, nJ; do i = 1, nI
               State_VGB(iVar,i,j,k,iBlock) &
                    = State_VGB(iVar,i,j,k,iBlock)/State_VGB(Rho_,i,j,k,iBlock)
            end do; end do; end do
         end do
      endif

      ! Now update State_VGB
      if(UseHalfStep .or. nStage == 1 .or. nStage == 4)then
         ! Update state variables starting from level n (=old) state
         if(nVarUpdate == nVar)then
            do k = 1, nK; do j = 1, nJ; do i = 1, nI
               State_VGB(:,i,j,k,iBlock) = &
                    StateOld_VGB(:,i,j,k,iBlock) + Source_VC(1:nVar,i,j,k)
            end do; end do; end do
         else
            do k = 1, nK; do j = 1, nJ; do i = 1, nI
               State_VGB(iVarUpdate_I,i,j,k,iBlock) = &
                    StateOld_VGB(iVarUpdate_I,i,j,k,iBlock) &
                    + Source_VC(iVarUpdate_I,i,j,k)
            end do; end do; end do
         end if
      else
         ! Update state variables starting from previous stage (RK schemes)
         if(nVarUpdate == nVar)then
            do k = 1, nK; do j = 1, nJ; do i = 1, nI
               State_VGB(:,i,j,k,iBlock) = &
                    State_VGB(:,i,j,k,iBlock) + Source_VC(1:nVar,i,j,k)
            end do; end do; end do
         else
            do k = 1, nK; do j = 1, nJ; do i = 1, nI
               State_VGB(iVarUpdate_I,i,j,k,iBlock) = &
                    State_VGB(iVarUpdate_I,i,j,k,iBlock) &
                    + Source_VC(iVarUpdate_I,i,j,k)
            end do; end do; end do
         end if
      end if

      if(nStage == 4)then
         ! Classical 4th order Runge-Kutta scheme. Requires extra storage.
         if(.not.allocated(Rk4_VCB)) allocate(Rk4_VCB(nVar,nI,nJ,nK,MaxBlock))

         select case(iStage)
         case(1)
            ! Rk4 = U1 = Un + Dt/2*Rn
            do k=1,nK; do j=1,nJ; do i=1,nI
               Rk4_VCB(:,i,j,k,iBlock) = State_VGB(:,i,j,k,iBlock)
            end do; end do; end do
         case(2)
            ! U2 = Un + Dt/2*R1
            ! Rk4 = Rk4 + 2*U2 = 3*Un + Dt/2*Rn + Dt*R1
            do k=1,nK; do j=1,nJ; do i=1,nI
               Rk4_VCB(:,i,j,k,iBlock) = Rk4_VCB(:,i,j,k,iBlock) &
                    + 2*State_VGB(:,i,j,k,iBlock)
            end do; end do; end do
         case(3)
            ! U3 = Un + Dt*R2
            ! Rk4 = Rk4 + U3 - 4Un = Dt/2*Rn + Dt*R1 + Dt*R2
            do k=1,nK; do j=1,nJ; do i=1,nI
               Rk4_VCB(:,i,j,k,iBlock) = Rk4_VCB(:,i,j,k,iBlock) &
                    + State_VGB(:,i,j,k,iBlock) &
                    - 4*StateOld_VGB(:,i,j,k,iBlock)
            end do; end do; end do
         case(4)
            ! U4 = Un + Dt/6*R3
            ! Un+1 = U4 + Rk4/3 = Un + Dt/6*(Rn + 2*R1 + 2*R2 + R3)
            do k=1,nK; do j=1,nJ; do i=1,nI
               State_VGB(:,i,j,k,iBlock) = &
                    + State_VGB(:,i,j,k,iBlock) &
                    + cThird*Rk4_VCB(:,i,j,k,iBlock)
            end do; end do; end do
         end select
      elseif(.not.UseHalfStep .and. iStage > 1)then
         ! Interpolation step for 2nd and 3rd order Runge-Kutta schemes

         ! Runge-Kutta scheme coefficients
         if(nStage==2)then
            Coeff1 = 0.5
         elseif(nStage==3)then
            if(iStage==2)then
               Coeff1 = 0.75
            elseif(iStage==3)then
               Coeff1 = 1./3.
            end if
         end if
         Coeff2 = 1 - Coeff1

         ! Interpolate state variables
         if(nVarUpdate == nVar)then
            do k = 1, nK; do j = 1, nJ; do i = 1, nI
               State_VGB(:,i,j,k,iBlock) = &
                    Coeff1*StateOld_VGB(:,i,j,k,iBlock) + &
                    Coeff2*State_VGB(:,i,j,k,iBlock)
            end do; end do; end do
         else
            do k = 1, nK; do j = 1, nJ; do i = 1, nI
               State_VGB(iVarUpdate_I,i,j,k,iBlock) = &
                    Coeff1*StateOld_VGB(iVarUpdate_I,i,j,k,iBlock) + &
                    Coeff2*State_VGB(iVarUpdate_I,i,j,k,iBlock)
            end do; end do; end do
         end if
      endif

      if(nVarUpdate /= nVar .and. DoUpdate_V(Rho_))then
         ! Convert back to momentum with preserved velocity
         do iVar = RhoUx_, RhoUz_
            if(DoUpdate_V(iVar)) CYCLE
            do k = 1, nK; do j = 1, nJ; do i = 1, nI
               State_VGB(iVar,i,j,k,iBlock) &
                    = State_VGB(iVar,i,j,k,iBlock)*State_VGB(Rho_,i,j,k,iBlock)
            end do; end do; end do
         end do
      endif

      if(DoTest)write(*,'(2x,2a,15es20.12)') &
           NameSub, ' after flux/source                   =', &
           State_VGB(iVarTest,iTest,jTest,kTest,iBlock)

      if(UseBorisCorrection .or. UseBorisSimple .and. IsMhd) then
         ! Convert relativistic momentum/energy back to classical
         call boris_to_mhd(iBlock)

         if(DoTest)write(*,'(2x,2a,15es20.12)') &
              NameSub, ' after boris_to_mhd                  =', &
              State_VGB(iVarTest,iTest,jTest,kTest,iBlock)
      endif

      if(UseElectronPressure .and. UseElectronEntropy)then
         ! Convert electron entropy back to pressure
         ! Pe = Se^GammaE
         do k = 1, nK; do j = 1, nJ; do i = 1, nI
            if(.not.Used_GB(i,j,k,iBlock)) CYCLE

            StateOld_VGB(Pe_,i,j,k,iBlock) = &
                 StateOld_VGB(Pe_,i,j,k,iBlock)**GammaElectron
            if(State_VGB(Pe_,i,j,k,iBlock) > 0.0) &
                 State_VGB(Pe_,i,j,k,iBlock) = &
                 State_VGB(Pe_,i,j,k,iBlock)**GammaElectron
         end do; end do; end do
      end if

      if(UseMultiSpecies)then
         ! Fix negative species densities
         State_VGB(SpeciesFirst_:SpeciesLast_,1:nI,1:nJ,1:nK,iBlock) = &
              max(0.0,&
              State_VGB(SpeciesFirst_:SpeciesLast_,1:nI,1:nJ,1:nK,iBlock))

         if(DoReplaceDensity)then
            ! Add up species densities to total density
            do k = 1, nK; do j = 1, nJ; do i = 1, nI
               State_VGB(Rho_,i,j,k,iBlock) = &
                    sum(State_VGB(SpeciesFirst_:SpeciesLast_,i,j,k,iBlock))
            end do; end do; end do
         end if

         if(DoTest)write(*,'(2x,2a,15es20.12)') &
              NameSub, ' after multispecies correct          =', &
              State_VGB(iVarTest,iTest,jTest,kTest,iBlock)

      end if

      ! Check minimum density
      if(any(RhoMin_I > 0.0))then
         do iFluid = 1, nFluid
            if(RhoMin_I(iFluid) < 0) CYCLE
            iRho = iRho_I(iFluid)
            do k=1,nK; do j=1,nJ; do i=1,nI
               State_VGB(iRho,i,j,k,iBlock) = max(RhoMin_I(iFluid), &
                    State_VGB(iRho,i,j,k,iBlock))
            end do; end do; end do
         end do

         if(DoTest)write(*,'(2x,2a,15es20.12)') &
              NameSub, ' after min density correct densities =', &
              State_VGB(iRho_I,iTest,jTest,kTest,iBlock)
      end if

      if(DoUpdate_V(p_) .and. &
           (UseDbTrickNow .and. (nStage==2 .and. iStage==1 &
           .or.               nStage==1 .and. .not.IsTimeAccurate))) then

         ! A desparate attempt to maintain positivity by adding dB^2/2 to the
         ! energy. This is fine for steady state, and is 2nd order accurate
         ! for half+full step method. But it cannot be used for RK schemes!

         do k = 1, nK; do j = 1, nJ; do i = 1, nI
            if(.not.Used_GB(i,j,k,iBlock)) CYCLE
            if(UseNonConservative)then
               if(.not.IsConserv_CB(i,j,k,iBlock)) CYCLE
            end if
            State_VGB(p_,i,j,k,iBlock) = State_VGB(p_,i,j,k,iBlock) &
                 + 0.5*sum(Source_VC(Bx_:Bz_,i,j,k)**2)
         end do; end do; end do

         if(DoTest)write(*,'(2x,2a,15es20.12)') &
              NameSub, ' after energy dB correct             =', &
              State_VGB(iVarTest,iTest,jTest,kTest,iBlock)
      end if

      if(UseWavePressure)then
         if(DoAdvectWaves .and. iStage==nStage .and. nWave>2)&
              call update_wave_group_advection(iBlock)
         if(UseWavePressureLtd .and. DoUpdate_V(Ew_))then
            do k = 1, nK; do j = 1, nJ; do i = 1, nI
               State_VGB(Ew_,i,j,k,iBlock) = &
                    sum(State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock))
            end do; end do; end do
         end if
         ! Avoid negative wave pressure
         if(UseAlfvenWaves)then
            do k = 1, nK; do j = 1, nJ; do i = 1, nI
               State_VGB(WaveFirst_:WaveLast_,i,j,k, iBlock) = &
                    max(State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock), 0.0)
            end do; end do; end do
         end if
      end if

      ! Convert energy back to pressure as needed
      call energy_to_pressure(iBlock, State_VGB)
      call energy_to_pressure(iBlock, StateOld_VGB, IsOld=.true.)

      if(DoTest)write(*,'(2x,2a,15es20.12)') &
           NameSub, ' after pressure/energy update        =', &
           State_VGB(iVarTest,iTest,jTest,kTest,iBlock)

    end subroutine update_explicit
    !==========================================================================
    subroutine deduct_expl_source()
      integer:: iVarSemi, i, j, k

      character(len=*), parameter:: NameSub = 'deduct_expl_source'
      !------------------------------------------------------------------------
      if(UseElectronPressure) then
         iVarSemi  = pe_
      else
         iVarSemi = p_
      endif

      do k=1,nK; do j=1,nJ; do i=1,nI
         ! DtLocal = Cfl*DtMax_CB(i,j,k,iBlock)

         ! For the first iteration, dt = 0;
         ! if(DtLocal < 1e-15) CYCLE
         Source_VCB(iVarSemi,i,j,k,iBlock) = &
              State_VGB(iVarSemi,i,j,k,iBlock) - &
              StateOld_VGB(iVarSemi,i,j,k,iBlock)
         State_VGB(iVarSemi,i,j,k,iBlock) = &
              StateOld_VGB(iVarSemi,i,j,k,iBlock)
      end do; end do; end do

    end subroutine deduct_expl_source
    !==========================================================================
  end subroutine update_state_normal
  !============================================================================
  subroutine update_te0

    use ModPhysics, ONLY: UnitTemperature_,Si2No_V
    use ModAdvance, ONLY: State_VGB,  nI, nJ, nK
    use ModMain,    ONLY: nBlock, Unused_B, UseERadInput
    use ModVarIndexes, ONLY: Te0_
    use ModUserInterface ! user_material_properties

    real:: Te0Si
    integer:: i, j, k, iBlock
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'update_te0'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    ! At the end of time step just calculated values of ERad are used to
    ! calculate Te (and accordingly B(Te)).
    UseERadInput = .true.

    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE
       do k=1,nK; do j=1,nJ; do i=1,nI
          call user_material_properties(State_VGB(:,i,j,k,iBlock), &
               i,j,k,iBlock, TeOut=Te0SI)
          State_VGB(Te0_,i,j,k,iBlock) = Te0SI * Si2No_V(UnitTemperature_)
       end do; end do; end do
    end do

    ! Reset UseERadInput
    UseERadInput = .false.

    call test_stop(NameSub, DoTest)
  end subroutine update_te0
  !============================================================================
  subroutine update_check

    ! Check updated values for allowed change in density or pressure

    use ModMain, ONLY: IsTimeAccurate, Dt, DtFixed, DtFixedOrig, &
         iStage, nStage, nStep
    use ModImplicit, ONLY: UsePartImplicit
    use ModVarIndexes, ONLY: p_, Rho_, nVar, SpeciesFirst_, SpeciesLast_, &
         NameVar_V, DefaultState_V
    use ModAdvance, ONLY: State_VGB, StateOld_VGB, DtMax_CB, &
         UseMultiIon, UseMultiSpecies, SpeciesPercentCheck, &
         PercentPLimit_I, PercentRhoLimit_I
    use ModNumConst, ONLY: cTiny
    use ModMultiIon, ONLY: DoRestrictMultiIon, IsMultiIon_CB
    use ModBatsrusUtility, ONLY: error_report, stop_mpi
    use BATL_lib, ONLY: iProc, nProc, nI, nJ, nK, nBlock, Unused_B
    use ModMpi

    integer, parameter :: MaxCheck=25, RhoDn_=1, RhoUp_=2, pDn_=3, pUp_=4
    integer :: i,j,k, iVar, iBlock, nCheck, iError
    real :: TimeFractionRho, TimeFractionRhoMin
    real :: TimeFractionP,   TimeFractionPMin
    real :: TimeFraction, TimeFractionCell, TimeFractionReport
    real :: RhoChangeLimit_S(RhoDn_:RhoUp_)
    real :: pChangeLimit_S(pDn_:pUp_), ChangeLimit_I(RhoDn_:pUp_)

    real :: Value
    integer :: i_D(3)
    logical :: DoneUpdateCheck, IsNegative, DoStop
    logical :: DoTest1
    logical :: DoTest2
    logical :: DoTest3

    real    :: RhoChangeMax_I(2), pChangeMax_I(2)
    character(len=*), parameter:: StringFormat="(a,i5,a,f6.1,a,3es11.3)"

    integer :: iError1=-1

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'update_check'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    call test_start('convergence_history', DoTest1)
    call test_start('update_check_detail', DoTest2)
    call test_start('locations',           DoTest3)

    ! Check for allowable percentage changed from update
    if(IsTimeAccurate) then
       TimeFractionReport = 1.
       do nCheck = 1, MaxCheck
          RhoChangeLimit_S = 0.1
          pChangeLimit_S   = 0.1
          !$omp parallel do private(iVar, i, j, k) &
          !$omp reduction(max:RhoChangeLimit_S) reduction(max:pChangeLimit_S)
          do iBlock = 1, nBlock
             if (Unused_B(iBlock)) CYCLE
             if (nCheck == 1) then
                do k = 1, nK; do j = 1, nJ; do i = 1, nI
                   do iVar = 1, nVar
                      if(DefaultState_V(iVar) == 0.0) CYCLE

                      ! For sake of backward compatibility
                      if(iVar == P_) CYCLE

                      ! Do not check multi-ion variables in regions that are
                      ! not truely multi-ion
                      if(UseMultiIon .and. iVar > p_ &
                           .and. DoRestrictMultiIon)then
                         if(.not.IsMultiIon_CB(i,j,k,iBlock)) CYCLE
                      end if

                      ! Do not check minor species if not necessary
                      if(UseMultiSpecies .and. &
                           iVar >= SpeciesFirst_ .and. iVar <= SpeciesLast_ &
                           .and. StateOld_VGB(iVar,i,j,k,iBlock) &
                           < SpeciesPercentCheck*0.01*&
                           StateOld_VGB(Rho_,i,j,k,iBlock)) CYCLE

                      RhoChangeLimit_S(RhoDn_) = &
                           max(RhoChangeLimit_S(RhoDn_), 100*abs( min(0.,&
                           (State_VGB(iVar,i,j,k,iBlock)- &
                           StateOld_VGB(iVar,i,j,k,iBlock)) &
                           /StateOld_VGB(iVar,i,j,k,iBlock) ) ) )
                      RhoChangeLimit_S(RhoUp_) = &
                           max(RhoChangeLimit_S(RhoUp_), 100*abs( max(0.,&
                           (State_VGB(iVar,i,j,k,iBlock)- &
                           StateOld_VGB(iVar,i,j,k,iBlock)) &
                           /StateOld_VGB(iVar,i,j,k,iBlock) ) ) )
                   end do
                end do; end do; end do
             end if
             pChangeLimit_S(pDn_) = &
                  max(pChangeLimit_S(pDn_), 100. * abs( min( 0., minval( &
                  (State_VGB(P_,1:nI,1:nJ,1:nK,iBlock)- &
                  StateOld_VGB(P_,1:nI,1:nJ,1:nK,iBlock)) &
                  /StateOld_VGB(P_,1:nI,1:nJ,1:nK,iBlock) ) ) ) )
             pChangeLimit_S(pUp_) = &
                  max(pChangeLimit_S(pUp_), 100. * abs( max( 0., maxval( &
                  (State_VGB(P_,1:nI,1:nJ,1:nK,iBlock)- &
                  StateOld_VGB(P_,1:nI,1:nJ,1:nK,iBlock)) &
                  /StateOld_VGB(P_,1:nI,1:nJ,1:nK,iBlock) ) ) ) )
          end do
          !$omp end parallel do

          if(DoTest)then
             ! Find location of maximum change
             call MPI_allreduce(RhoChangeLimit_S, RhoChangeMax_I, 2, &
                  MPI_REAL, MPI_MAX, iComm, iError)
             call MPI_allreduce(pChangeLimit_S, pChangeMax_I, 2, &
                  MPI_REAL, MPI_MAX, iComm, iError)

             !$omp parallel do &
             !$omp      reduction(max:RhoChangeLimit_S) &
             !$omp      reduction(max:pChangeLimit_S)
             do iBlock = 1, nBlock
                if(Unused_B(iBlock)) CYCLE
                do k=1,nK; do j=1,nJ; do i=1,nI
                   do iVar = 1, nVar
                      if(DefaultState_V(iVar) == 0.0) CYCLE

                      if(UseMultiSpecies .and. &
                           iVar >= SpeciesFirst_ .and. iVar <= SpeciesLast_ &
                           .and. StateOld_VGB(iVar,i,j,k,iBlock) &
                           < SpeciesPercentCheck*0.01*&
                           StateOld_VGB(Rho_,i,j,k,iBlock)) CYCLE

                      if(iVar == p_)then
                         if(pChangeMax_I(1) > PercentPLimit_I(1) .and. &
                              1e-4 > abs(pChangeMax_I(1) - 100. * abs( &
                              (   State_VGB(P_,i,j,k,iBlock)- &
                              StateOld_VGB (P_,i,j,k,iBlock)) &
                              /StateOld_VGB(P_,i,j,k,iBlock) ))) &
                              write(*,StringFormat) NameSub//' nStep=',nStep,&
                              ' max p drop=',pChangeMax_I(1),&
                              '% at x,y,z=',&
                              Xyz_DGB(:,i,j,k,iBlock)

                         if(pChangeMax_I(2) > PercentPLimit_I(2) .and. &
                              1e-4 > abs(pChangeMax_I(2) - 100. * abs( &
                              (   State_VGB(P_,i,j,k,iBlock)- &
                              StateOld_VGB (P_,i,j,k,iBlock)) &
                              /StateOld_VGB(P_,i,j,k,iBlock)  ))) &
                              write(*,StringFormat) NameSub//' nStep=',nStep,&
                              ' max p increase=',&
                              pChangeMax_I(2), &
                              '% at x,y,z=',&
                              Xyz_DGB(:,i,j,k,iBlock)

                         CYCLE
                      end if

                      if(RhoChangeMax_I(1) > PercentRhoLimit_I(1) .and. &
                           1e-4 > abs(RhoChangeMax_I(1) - 100*abs( &
                           (State_VGB(iVar,i,j,k,iBlock)- &
                           StateOld_VGB(iVar,i,j,k,iBlock)) &
                           /StateOld_VGB(iVar,i,j,k,iBlock) ))) &
                           write(*,StringFormat) NameSub//' nStep=',nStep,&
                           ' max '//trim(NameVar_V(iVar))//' drop=', &
                           RhoChangeMax_I(1), &
                           '% at x,y,z=',&
                           Xyz_DGB(:,i,j,k,iBlock)

                      if(RhoChangeMax_I(2) > PercentRhoLimit_I(2) .and. &
                           1e-4 > abs(RhoChangeMax_I(2) - 100*abs( &
                           (State_VGB(iVar,i,j,k,iBlock)- &
                           StateOld_VGB(iVar,i,j,k,iBlock)) &
                           /StateOld_VGB(iVar,i,j,k,iBlock) ))) &
                           write(*,StringFormat) NameSub//' nStep=',nStep,&
                           ' max '//trim(NameVar_V(iVar))//' increase=',&
                           RhoChangeMax_I(2), &
                           '% at x,y,z=',&
                           Xyz_DGB(:,i,j,k,iBlock)
                   end do
                end do; end do; end do
             end do
             !$omp end parallel do
          end if ! DoTest
          TimeFractionRho = 1.0 / maxval(RhoChangeLimit_S/PercentRhoLimit_I)
          call MPI_allreduce(TimeFractionRho, TimeFractionRhoMin, 1, &
               MPI_REAL, MPI_MIN, iComm, iError)
          TimeFractionP   = 1.0 / maxval(pChangeLimit_S/PercentPLimit_I  )
          call MPI_allreduce(TimeFractionP, TimeFractionPMin, 1, &
               MPI_REAL, MPI_MIN, iComm, iError)
          if(TimeFractionRhoMin >= 1. .and. TimeFractionPMin >= 1.) EXIT

          if(nCheck == 1)then
             TimeFraction = 1.
             if (TimeFractionRhoMin < 1.) &
                  TimeFraction = 0.9*TimeFractionRhoMin
             if (TimeFractionPMin   < 1.) &
                  TimeFraction = min(TimeFraction, 0.75)
          else
             TimeFraction = 0.5
          end if
          dt = dt*TimeFraction
          TimeFractionReport = TimeFractionReport*TimeFraction

          !$omp parallel do private(i,j,k)
          do iBlock = 1, nBlock
             if(Unused_B(iBlock)) CYCLE

             ! Fix the update in the cells
             do k=1,nK; do j=1,nJ; do i=1,nI
                call fix_update(i,j,k,iBlock,TimeFraction)
             end do; end do; end do
          end do
          !$omp end parallel do
       end do

       ChangeLimit_I(RhoDn_:RhoUp_) =  RhoChangeLimit_S - 0.1
       ChangeLimit_I(pDn_:pUp_) =  pChangeLimit_S   - 0.1

       ! The part implicit scheme can get here if all blocks become explicit
       ! due to time step reductions. To be able to recover the time step,
       ! increase fixed time step if there was no time step reduction above.
       if(UsePartImplicit .and. dt == DtFixed) &
            DtFixed = min(DtFixedOrig, DtFixed*1.05)

       if(DoTest) then
          if (iProc == 0 .and. TimeFractionReport < 1.) &
               write(*,'(a,a,i6,a,f12.8,a,f12.8)') NameSub//' TA:', &
               ' nStep=', nStep,'     dt reduction=', TimeFractionReport, &
               ' dt=', dt
       end if
    else
       ! LOCAL TIMESTEPPING
       TimeFractionReport = 1.
       ChangeLimit_I = 0.
       !$omp parallel do private(i,j,k,nCheck,iVar,DoneUpdateCheck) &
       !$omp private(TimeFractionRho,TimeFractionP,TimeFractionCell) &
       !$omp private(TimeFraction, RhoChangeLimit_S, pChangeLimit_S) &
       !$omp reduction(max:ChangeLimit_I) &
       !$omp reduction(min:TimeFractionReport)
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          do k=1,nK; do j=1,nJ; do i=1,nI
             TimeFractionCell = 1.
             do nCheck = 1, MaxCheck
                DoneUpdateCheck = .true.
                RhoChangeLimit_S = 0.1
                pChangeLimit_S   = 0.1
                if (nCheck == 1) then
                   do iVar = 1, nVar
                      if (DefaultState_V(iVar) == 0.0) CYCLE

                      ! This is for backwards compatibility
                      if(iVar == P_) CYCLE

                      if(UseMultiSpecies .and. &
                           iVar >= SpeciesFirst_ .and. iVar <= SpeciesLast_ &
                           .and. StateOld_VGB(iVar,i,j,k,iBlock) < &
                           SpeciesPercentCheck*0.01*&
                           StateOld_VGB(Rho_,i,j,k,iBlock)) CYCLE

                      RhoChangeLimit_S(RhoDn_) = &
                           max(RhoChangeLimit_S(RhoDn_), &
                           0.1 + 100. * abs( min( 0., &
                           (State_VGB(iVar,i,j,k,iBlock)-&
                           StateOld_VGB(iVar,i,j,k,iBlock)) &
                           /StateOld_VGB(iVar,i,j,k,iBlock) ) ) )

                      RhoChangeLimit_S(RhoUp_) = &
                           max(RhoChangeLimit_S(RhoUp_), &
                           0.1 + 100. * abs( max( 0., &
                           (State_VGB(iVar,i,j,k,iBlock)-&
                           StateOld_VGB(iVar,i,j,k,iBlock)) &
                           /StateOld_VGB(iVar,i,j,k,iBlock) ) ) )
                   end do
                end if
                pChangeLimit_S(pDn_) = 0.1 + 100. * abs( min( 0., &
                     (State_VGB(P_,i,j,k,iBlock)-&
                     StateOld_VGB(P_,i,j,k,iBlock)) &
                     /StateOld_VGB(P_,i,j,k,iBlock) ) )
                pChangeLimit_S(pUp_) = 0.1 + 100. * abs( max( 0., &
                     (State_VGB(P_,i,j,k,iBlock)-&
                     StateOld_VGB(P_,i,j,k,iBlock)) &
                     /StateOld_VGB(P_,i,j,k,iBlock) ) )
                TimeFractionRho = 1/maxval(RhoChangeLimit_S/PercentRhoLimit_I)
                TimeFractionP   = 1/maxval(pChangeLimit_S  /PercentPLimit_I  )
                if (TimeFractionRho < 1. .or. TimeFractionP < 1.) then
                   if(nCheck == 1) then
                      TimeFraction = 1.
                      if (TimeFractionRho < 1.) &
                           TimeFraction = 0.9*TimeFractionRho
                      if (TimeFractionP   < 1.) &
                           TimeFraction = min(TimeFraction, 0.75)
                   else
                      TimeFraction = 0.5
                   end if
                   DoneUpdateCheck = .false.
                   TimeFractionCell = TimeFractionCell * TimeFraction
                   if(DoTest2) then
                      write(*,*) &
                           NameSub,' LT: changing cell value, PE=',iProc, &
                           ' BLK=',iBlock,' i,j,k=',i,' ',j,' ',k, &
                           '  TimeFraction=',TimeFraction
                      write(*,*) &
                           iProc,' ',iBlock,' ',i,' ',j,' ',k,' OLD:  ', &
                           NameVar_V(1),'=',StateOld_VGB(1,i,j,k,iBlock),&
                           '    ',&
                           NameVar_V(nVar),' ', StateOld_VGB(nVar,i,j,k,iBlock)
                      write(*,*) &
                           iProc,' ',iBlock,' ',i,' ',j,' ',k,' BAD: ', &
                           NameVar_V(1),'=',State_VGB(1,i,j,k,iBlock), &
                           '   ', NameVar_V(nVar),State_VGB(nVar,i,j,k,iBlock)
                   end if
                   call fix_update(i,j,k,iblock,TimeFraction)
                   if(DoTest2) then
                      write(*,*) &
                           iProc,' ',iBlock,' ',i,' ',j,' ',k,' NEW: ', &
                           NameVar_V(Rho_),'=',State_VGB(Rho_,i,j,k,iBlock),&
                           '   ',NameVar_V(p_),'=', State_VGB(p_,i,j,k,iBlock)
                   end if
                end if
                if(DoneUpdateCheck) EXIT
             end do
             ChangeLimit_I(RhoDn_:RhoUp_) = &
                  max(RhoChangeLimit_S(1:2) - 0.1, &
                  ChangeLimit_I(RhoDn_:RhoUp_))
             ChangeLimit_I(pDn_:pUp_) = &
                  max(pChangeLimit_S(pDn_:pUp_) - 0.1, &
                  ChangeLimit_I(pDn_:pUp_))
             TimeFractionReport = min(TimeFractionReport, TimeFractionCell)
          end do; end do; end do
       end do
       !$omp end parallel do

       if(nProc > 1) call MPI_allreduce(MPI_IN_PLACE, TimeFractionReport, 1, &
            MPI_REAL, MPI_MIN, iComm, iError)

       if(DoTest) then
          if (iProc == 0 .and. TimeFractionReport < 1.) &
               write(*,'(a,a,i6,a,f12.8)') NameSub//' LT:', &
               ' nStep=',nStep,' max dt reduction=',TimeFractionReport
       end if
    end if

    if(DoTest1 .and. iStage == nStage) then
       if(nProc > 1) call MPI_allreduce(MPI_IN_PLACE, ChangeLimit_I, &
            4, MPI_REAL, MPI_MAX, iComm, iError)
       if(iProc == 0) then
          write(*,*)'Maximum change in pressure on proc 0:',&
               - ChangeLimit_I(pDn_),' %,   ',  ChangeLimit_I(pUp_),' %'
          write(*,*) 'Maximum change in other positive variables:', &
               - ChangeLimit_I(RhoDn_),' %,   ',  ChangeLimit_I(RhoUp_),' %'
       end if

       if(DoTest3)then
          !$omp parallel do private(i,j,k)
          do iBlock = 1,nBlock
             if(Unused_B(iBlock))CYCLE
             do k=1,nK; do j=1,nJ; do i=1,nI
                if(.not.Used_GB(i,j,k,iBlock))CYCLE
                if(abs(100. * abs( min( 0., &
                     (State_VGB(Rho_,i,j,k,iBlock)-&
                     StateOld_VGB(Rho_,i,j,k,iBlock)) &
                     /StateOld_VGB(Rho_,i,j,k,iBlock) ) )-&
                     ChangeLimit_I(RhoDn_)) < cTiny*ChangeLimit_I(RhoDn_))&
                     write(*,*)'Maximum decrease in density at X Y Z=',&
                     Xyz_DGB(:,i,j,k,iBlock),&
                     ': rho_old = ',StateOld_VGB(Rho_,i,j,k,iBlock),&
                     ' rho_new = ',State_VGB(Rho_,i,j,k,iBlock)

                if(abs(100. * abs( max( 0., &
                     (State_VGB(Rho_,i,j,k,iBlock)-&
                     StateOld_VGB(Rho_,i,j,k,iBlock)) &
                     /StateOld_VGB(Rho_,i,j,k,iBlock) ) )-&
                     ChangeLimit_I(RhoUp_)) < cTiny*ChangeLimit_I(RhoUp_))&
                     write(*,*)'Maximum increase in density at the point',&
                     Xyz_DGB(:,i,j,k,iBlock),&
                     'is: rho_old = ',&
                     StateOld_VGB(Rho_,i,j,k,iBlock),&
                     'rho_new=',State_VGB(Rho_,i,j,k,iBlock)

                if(abs(100. * abs( min( 0., &
                     (State_VGB(p_,i,j,k,iBlock)-&
                     StateOld_VGB(p_,i,j,k,iBlock)) &
                     /StateOld_VGB(p_,i,j,k,iBlock) ) )-&
                     ChangeLimit_I(pDn_)) < cTiny*ChangeLimit_I(pDn_))&
                     write(*,*)'Maximum decrease in',NameVar_V(p_), &
                     'at the point',&
                     Xyz_DGB(:,i,j,k,iBlock),&
                     'is: valeu_old = ',StateOld_VGB(P_,i,j,k,iBlock),&
                     'value_new=',State_VGB(p_,i,j,k,iBlock)
                if(abs(100. * abs( max( 0., &
                     (State_VGB(p_,i,j,k,iBlock)-&
                     StateOld_VGB(p_,i,j,k,iBlock)) &
                     /StateOld_VGB(p_,i,j,k,iBlock) ) )-&
                     ChangeLimit_I(pUp_)) < cTiny*ChangeLimit_I(pUp_))&
                     write(*,*)'Maximum increase in', NameVar_V(p_), &
                     'at the point',&
                     Xyz_DGB(:,i,j,k,iBlock),&
                     'is: value_old = ',StateOld_VGB(p_,i,j,k,iBlock),&
                     'value_new=',State_VGB(p_,i,j,k,iBlock)
             end do; end do; end do
          end do
          !$omp end parallel do
       end if
    end if

    if(iProc == 0 .and. TimeFractionReport < 1.0)&
         call error_report('Time step reduction, min(factor)',&
         TimeFractionReport,iError1,.true.)

    ! Check for positivity of variables
    IsNegative = .false.
    !$omp parallel do private(Value,i_D,IsNegative,iVar,i,j,k)
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       do iVar = 1, nVar
          ! Ignore variables that do not have to be positive
          if(DefaultState_V(iVar) == 0.0) CYCLE

          ! Do not check species densities if check threshold is positive
          ! (i.e. minor species densities are allowed to go negative,
          !       and they are fixed to be zero in update_state)
          if(UseMultiSpecies .and. SpeciesPercentCheck > 0.0 &
               .and. iVar >= SpeciesFirst_ .and. iVar <= SpeciesLast_) CYCLE

          Value = minval(State_VGB(iVar,1:nI,1:nJ,1:nK,iBlock))

          if(Value < 0.0)then
             i_D = minloc(State_VGB(iVar,1:nI,1:nJ,1:nK,iBlock))
             i = i_D(1); j = i_D(2); k = i_D(3)
             write (*,'(a,3i3,2i5,i3,a,3f12.4,/,5x,a,a,es12.4)') &
                  ' I J K iBlock iProc iVar=',i_D,iBlock,iProc,iVar, &
                  ' X Y Z=', &
                  Xyz_DGB(:,i,j,k,iBlock), &
                  ' Var='//trim(NameVar_V(iVar)), &
                  ' Value=', State_VGB(iVar,i,j,k,iBlock)
             IsNegative = .true.
          end if
       end do
    end do
    !$omp end parallel do
    if(IsNegative)then
       if(IsTimeAccurate)then
          write(*,'(a,i4,a,a,i6,a,f12.8,a,f12.8)') &
               'Negative updated value: PE=', iProc, &
               NameSub//' TA:',' nStep=', nStep, &
               '     dt reduction=',TimeFractionReport,' dt=',dt
       else
          write(*,'(a,i4,a,a,i6,a,f12.8)') &
               'Negative updated value: PE=', iProc, &
               NameSub//' LT:',' nStep=', nStep, &
               ' max dt reduction=', TimeFractionReport
       end if
    end if

    call MPI_allreduce(IsNegative, DoStop,1,MPI_LOGICAL,MPI_LOR,iComm,iError)
    if(DoStop) call stop_mpi('Stopping, negative density or pressure')

    call test_stop(NameSub, DoTest)
  contains
    !==========================================================================
    subroutine fix_update(i,j,k,iBlock,TimeFraction)

      integer, intent(in):: i,j,k,iBlock
      real,    intent(in):: TimeFraction

      logical, parameter :: DoTestCell = .false.

      ! DoTestCell = DoTestMe .and. iBlock==iBlockTest .and. &
      !     i==iTest .and. j==jTest .and. k==kTest

      character(len=*), parameter:: NameSub = 'fix_update'
      !------------------------------------------------------------------------
      State_VGB(:,i,j,k,iBlock) = &
           (    TimeFraction) *    State_VGB(:,i,j,k,iBlock) + &
           (1.0-TimeFraction) * StateOld_VGB(:,i,j,k,iBlock)

      DtMax_CB(i,j,k,iBlock) = DtMax_CB(i,j,k,iBlock)*TimeFraction

      if(DoTestCell)write(*,*)NameSub,' final state=',State_VGB(:,i,j,k,iBlock)

    end subroutine fix_update
    !==========================================================================
  end subroutine update_check
  !============================================================================
  subroutine fix_anisotropy

    ! Calculate the pressure anisotropy relaxation term for anisotropic MHD.
    ! Correct the parallel pressure based on the firehose, mirror and proton
    ! cyclotron instability criteria in unstable regions and the global
    ! relaxation, if present, in the whole domain.
    ! The instability that changes Ppar most is applied.
    !
    ! If UseConstantTau = true, use TauInstability read from PARAM.in as the
    ! contant relaxation time, same for different instabilities.
    ! TauGlobal is a constant read from PARAM.in.

    use ModVarIndexes, ONLY: Bx_, Bz_
    use ModMain,    ONLY: nI, nJ, nK, nBlock, Unused_B, UseB0, &
         IsTimeAccurate, Cfl, dt
    use ModB0,      ONLY: B0_DGB
    use ModAdvance, ONLY: State_VGB, DtMax_CB
    use ModPhysics, ONLY: UseConstantTau_I, TauInstability_I, &
         IonMassPerCharge, TauGlobal_I
    use ModMultiFluid, ONLY: select_fluid, iP, iPpar
    use ModVarIndexes, ONLY: nFluid

    ! Variables for anisotropic pressure
    real:: B_D(3), B2, p, Ppar, Pperp, Dp, DtCell
    real:: InvGyroFreq, PparOverLimit, Deltapf, Deltapm

    integer:: i, j, k, iBlock, iFluid

    logical :: UseConstantTau
    real    :: TauInstability, TauGlobal

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'fix_anisotropy'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       do k=1,nK; do j=1,nJ; do i=1,nI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE

          do iFluid = 1, nFluid
             if(nFluid > 1) call select_fluid(iFluid)

             UseConstantTau = UseConstantTau_I(iFluid)
             TauInstability = TauInstability_I(iFluid)
             TauGlobal      = TauGlobal_I(iFluid)

             ! Avoid Pperp < 0
             State_VGB(iPpar,i,j,k,iBlock) = &
                  min(3*State_VGB(iP,i,j,k,iBlock),&
                  State_VGB(iPpar,i,j,k,iBlock))

             ! Do not apply the relaxation term in this case
             if(UseConstantTau .and. TauInstability < 0.0) CYCLE

             B_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
             if(UseB0) B_D = B_D + B0_DGB(:,i,j,k,iBlock)
             B2     = sum(B_D**2)
             Ppar   = State_VGB(iPpar,i,j,k,iBlock)
             p = State_VGB(ip,i,j,k,iBlock)
             Pperp  = (3*p - Ppar)/2.
             if(.not. IsTimeAccurate)then
                DtCell = Cfl*DtMax_CB(i,j,k,iBlock)
             else
                DtCell = dt
             end if

             InvGyroFreq = IonMassPerCharge/max(1e-8, sqrt(B2))

             ! Find the instability that changes ppar the most
             Dp = 0.0

             ! This is what global relaxation would do
             if(TauGlobal > 0) Dp = DtCell*(p - Ppar)/(DtCell + TauGlobal)

             ! Check for the firehose, mirror and ion cyclotron instabilities
             ! Limit anisotropy to instability criteria in unstable regions
             if(Ppar - Pperp > B2)then
                ! firehose
                ! by how much the instability limit is exceeded:
                ! ppar - ppar_marginalstable
                PparOverLimit = Ppar - p - 2/3.*B2

                ! Calc firehose relaxation time based on the maximum
                ! growth rate calculated from eqn (2) of Hall [1981]
                ! with theta = 0 and ppar < 4*pperp
                ! MaxGrowthRate =
                !    0.5*GyroFreq*Delta pf/sqrt(ppar*(pperp-ppar/4))
                ! where Delta pf = ppar-pperp-B^2 = 3/2*PparOverLimit
                if(.not. UseConstantTau)then
                   Deltapf = 3/2.*PparOverLimit
                   TauInstability = 2.0*InvGyroFreq* &
                        sqrt(max(3.0*Ppar*(Pperp-0.25*Ppar),1e-8))/Deltapf
                end if
                Dp = min(Dp, -DtCell*PparOverLimit/(DtCell + TauInstability))

             else
                if(Pperp**2 > Ppar*Pperp + 0.5*B2*Ppar)then
                   ! mirror
                   ! ppar_marginalstable - ppar
                   PparOverLimit = (B2 + 6.0*p &
                        - sqrt(B2**2 + 12.0*B2*p + 9.0*p**2))/3. - Ppar

                   ! Calc mirror relaxation time based on the maximum
                   ! growth rate from eqn (7) of Southwood [1993],
                   ! with the wavelength at maximum growth from eqn (21)
                   ! of Hall [1980]
                   ! MaxGrowthRate =
                   !    4/3/sqrt(5)*GyroFreq*sqrt(2*Delta pm/ppar)
                   ! where Delta pm = pperp-ppar-B^2*ppar/(2*pperp)
                   if(.not. UseConstantTau)then
                      Deltapm = Pperp - Ppar - 0.5*B2*Ppar/Pperp
                      TauInstability = 0.75*InvGyroFreq*sqrt(2.5*Ppar/Deltapm)
                   end if
                   Dp = max(Dp, DtCell*PparOverLimit/(DtCell + TauInstability))
                end if
                if(Pperp > Ppar + 0.3*sqrt(0.5*B2*Ppar))then
                   ! ion cyclotron
                   ! ppar_marginalstable - ppar
                   PparOverLimit = (sqrt(0.01*B2 + 2.0*p) &
                        - 0.1*sqrt(B2))**2/2. - Ppar

                   ! Estimate ion cyclotron relaxation time from
                   ! observations in the magnetosphere and theories
                   if(.not. UseConstantTau) &
                        TauInstability = 100*InvGyroFreq

                   Dp = max(Dp, DtCell*PparOverLimit/(DtCell + TauInstability))
                end if
             end if
             State_VGB(iPpar,i,j,k,iBlock) = Ppar + Dp

          end do
       end do; end do; end do
    end do

    call test_stop(NameSub, DoTest)
  end subroutine fix_anisotropy
  !============================================================================
  subroutine update_b0

    use ModMain,          ONLY: nBlock, Unused_B,      &
         tSimulation, NameThisComp, IsTimeAccurate, DoThreads_B
    use ModPhysics,       ONLY: ThetaTilt, UseBody2Orbit
    use ModAdvance,       ONLY: Bx_, By_, Bz_, State_VGB, &
         iTypeUpdate, UpdateFast_
    use ModUpdateStateFast, ONLY: update_b0_fast
    use ModGeometry,      ONLY: IsBody_B
    use CON_axes,         ONLY: get_axes
    use ModNumConst,      ONLY: cRadToDeg
    use ModIO,            ONLY: iUnitOut, write_prefix
    use ModB0,            ONLY: B0_DGB, set_b0_cell, set_b0_reschange
    use ModFieldLineThread, ONLY: UseFieldLineThreads, set_threads
    use ModMessagePass,    ONLY: exchange_messages

    integer :: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'update_b0'
    !--------------------------------------------------------------------------
    if(iTypeUpdate >= UpdateFast_)then
       call update_b0_fast
       if (IsTimeAccurate) call exchange_messages(DoResChangeOnlyIn=.true.)
       RETURN
    endif

    call test_start(NameSub, DoTest)
    call timing_start(NameSub)

    ! Update ThetaTilt
    if(NameThisComp=='GM') &
         call get_axes(tSimulation, MagAxisTiltGsmOut=ThetaTilt)

    if (DoTest) then
       if(NameThisComp=='GM')then
          call write_prefix; write(iUnitOut,*) &
               "update_b0 at tSimulation, TiltGsm=", &
               tSimulation, ThetaTilt*cRadToDeg
       else
          call write_prefix; write(iUnitOut,*) &
               "update_b0 at tSimulation=",tSimulation
       end if
    end if

    do iBlock=1,nBlock
       if(Unused_B(iBlock)) CYCLE

       ! Save total magnetic field into Bx, By ,Bz
       State_VGB(Bx_:Bz_,:,:,:,iBlock) = State_VGB(Bx_:Bz_,:,:,:,iBlock) &
            + B0_DGB(:,:,:,:,iBlock)

       call set_b0_cell(iBlock)

       ! Split total B again using new B0
       State_VGB(Bx_:Bz_,:,:,:,iBlock) = State_VGB(Bx_:Bz_,:,:,:,iBlock) &
            - B0_DGB(:,:,:,:,iBlock)
    end do

    if (IsTimeAccurate) call exchange_messages(DoResChangeOnlyIn=.true.)

    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       ! Set B1 to 0 inside bodies
       if(IsBody_B(iBlock))then
          where(.not.Used_GB(:,:,:,iBlock))
             State_VGB(Bx_,:,:,:,iBlock)=0.0
             State_VGB(By_,:,:,:,iBlock)=0.0
             State_VGB(Bz_,:,:,:,iBlock)=0.0
          end where
       end if
    end do

    ! Recalculate B0 face values at resolution changes
    call set_b0_reschange
    if(UseFieldLineThreads)then
       if(UseBody2Orbit)then
          ! Nullify DoThread array, set in set_b0
          DoThreads_B = .false.
       else
          call set_threads(NameSub)
          call exchange_messages
       end if
    end if
    call timing_stop(NameSub)
    call test_stop(NameSub, DoTest)

  end subroutine update_b0
  !============================================================================
  subroutine fix_multi_ion_update(iBlock)

    ! This subroutine sets the ion velocities of the individual fluids equal to
    ! their mass density weighted average, and/or the ion temperature of the
    ! individual fluids equal to their number density weighted average.

    use ModSize,       ONLY: nI, nJ, nK
    use ModAdvance,    ONLY: State_VGB, &
         UseSingleIonVelocity, UseSingleIonTemperature
    use ModMultiFluid, ONLY: nIonFluid, IonFirst_, IonLast_, &
         iRho_I, iRhoUx_I, iRhoUy_I, iRhoUz_I, iP_I, MassIon_I, MassFluid_I

    integer, intent(in) :: iBlock

    integer :: i, j, k, iFluid
    real :: RhoInv, Ux, Uy, Uz, Temp, NumDens_I(nIonFluid)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'fix_multi_ion_update'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(DoTest)then
       write(*,*) NameSub,': ion fluid velocities and temperatures before fix:'
       do iFluid = IonFirst_, IonLast_
          write(*,*)'iIonFluid,ux,uy,uz,T=', &
               State_VGB(iRhoUx_I(iFluid):iRhoUz_I(iFluid),&
               iTest,jTest,kTest,iBlock) &
               / State_VGB(iRho_I(iFluid),iTest,jTest,kTest,iBlock), &
               State_VGB(iP_I(iFluid),iTest,jTest,kTest,iBlock)    &
               *MassFluid_I(iFluid) &
               /State_VGB(iRho_I(iFluid),iTest,jTest,kTest,iBlock)
       end do
    end if

    if(UseSingleIonVelocity) then
       do k=1, nK; do j=1, nJ; do i=1, nI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE

          ! Calcualate average velocity from total momentum and density
          RhoInv= 1/sum(State_VGB(iRho_I(IonFirst_:IonLast_),i,j,k,iBlock))
          Ux = RhoInv*sum(State_VGB(iRhoUx_I(IonFirst_:IonLast_),i,j,k,iBlock))
          Uy = RhoInv*sum(State_VGB(iRhoUy_I(IonFirst_:IonLast_),i,j,k,iBlock))
          Uz = RhoInv*sum(State_VGB(iRhoUz_I(IonFirst_:IonLast_),i,j,k,iBlock))

          ! Reset the momentum of all ion fluids
          State_VGB(iRhoUx_I(IonFirst_:IonLast_),i,j,k,iBlock) = &
               Ux*State_VGB(iRho_I(IonFirst_:IonLast_),i,j,k,iBlock)
          State_VGB(iRhoUy_I(IonFirst_:IonLast_),i,j,k,iBlock) = &
               Uy*State_VGB(iRho_I(IonFirst_:IonLast_),i,j,k,iBlock)
          State_VGB(iRhoUz_I(IonFirst_:IonLast_),i,j,k,iBlock) = &
               Uz*State_VGB(iRho_I(IonFirst_:IonLast_),i,j,k,iBlock)

       end do; end do; end do
    end if

    if(UseSingleIonTemperature) then
       do k=1, nK; do j=1, nJ; do i=1, nI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE

          ! Number density
          NumDens_I = &
               State_VGB(iRho_I(IonFirst_:IonLast_),i,j,k,iBlock)/MassIon_I

          ! Average temperature = sum(p)/sum(n) = sum(p)/sum(rho/M)
          Temp = sum(State_VGB(iP_I(IonFirst_:IonLast_),i,j,k,iBlock)) &
               / sum(NumDens_I)

          ! Reset the pressure of all ion fluids
          State_VGB(iP_I(IonFirst_:IonLast_),i,j,k,iBlock) = Temp*NumDens_I

       end do; end do; end do
    end if

    if(DoTest)then
       write(*,*) NameSub,': ion fluid velocities and temperatures after fix:'
       do iFluid = IonFirst_, IonLast_
          write(*,*)'iIonFluid,ux,uy,uz,T=', &
               State_VGB(iRhoUx_I(iFluid):iRhoUz_I(iFluid),&
               iTest,jTest,kTest,iBlock) &
               / State_VGB(iRho_I(iFluid),iTest,jTest,kTest,iBlock), &
               State_VGB(iP_I(iFluid),iTest,jTest,kTest,iBlock)    &
               *MassFluid_I(iFluid) &
               /State_VGB(iRho_I(iFluid),iTest,jTest,kTest,iBlock)
       end do
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine fix_multi_ion_update
  !============================================================================
  subroutine check_nan(NameSub, iError)

    use ModVarIndexes, ONLY: nVar, NameVar_V
    use ModAdvance, ONLY: State_VGB
    use ModGeometry, ONLY: r_GB
    use BATL_lib, ONLY: nI, nJ, nK, nBlock, Unused_B, Xyz_DGB, iProc
    use ModBatsrusUtility, ONLY: stop_mpi
    use, intrinsic :: ieee_arithmetic

    character(len=*), intent(in):: NameSub
    integer, intent(out), optional:: iError

    integer:: iVar, iBlock, i, j, k
    real:: Value
    !--------------------------------------------------------------------------
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          do iVar = 1, nVar
             Value = State_VGB(iVar,i,j,k,iBlock)
             if (ieee_is_nan(Value)) then
                write(*,*) 'iProc=', iProc, &
                     ': NaN in State_V=', State_VGB(:,i,j,k,iBlock)
                write(*,*) 'NameVar_V =', NameVar_V
                write(*,*) 'iProc=', iProc, &
                     ': NaN at i,j,k,iBlock= ', i, j, k, iBlock,  &
                     ', x,y,z,r= ', Xyz_DGB(:,i,j,k,iBlock), r_GB(i,j,k,iBlock)

                if(present(iError))then
                   iError = 1
                   RETURN
                else
                   call stop_mpi('ERROR: NaN from '//NameSub)
                end if
             end if
          end do
       end do; end do; end do
    end do

  end subroutine check_nan
  !============================================================================
end module ModUpdateState
!==============================================================================
