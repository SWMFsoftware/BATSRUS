!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModEnergy

  use BATL_lib, ONLY: &
       test_start, test_stop
  use ModExtraVariables, ONLY: Pepar_
  use ModVarIndexes, ONLY: &
       nVar, Rho_, RhoUx_, RhoUz_, Bx_, Bz_, Hyp_, p_, Pe_, IsMhd
  use ModMultiFluid, ONLY: &
       nFluid, nIonFluid, iRho, iRhoUx, iRhoUz, &
       iRhoUxIon_I, iRhoUyIon_I, iRhoUzIon_I, iP, iP_I, iPIon_I, &
       UseMultiIon, UseNeutralFluid, DoConserveNeutrals, &
       select_fluid, MassFluid_I, iRho_I, iRhoIon_I, MassIon_I, ChargeIon_I, &
       iRhoUxIon_I, iRhoUyIon_I, iRhoUzIon_I
  use ModAdvance, ONLY: &
       State_VGB, UseElectronPressure, UseElectronEnergy, UseTotalIonEnergy
  use ModConservative, ONLY: is_conserv, UseNonConservative, nConservCrit
  use ModPhysics, ONLY: &
       GammaMinus1_I, InvGammaMinus1_I, GammaMinus1, InvGammaMinus1, &
       InvGammaElectronMinus1, pMin_I, PeMin, Tmin_I, TeMin
  use BATL_lib, ONLY: &
       nI, nJ, nK, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, MaxBlock, Used_GB
  use ModSaMhd,     ONLY: UseSaMhd, rMinSaMhd
  use ModGeometry, ONLY: r_GB

  implicit none

  private ! except

  public:: energy_to_pressure       ! e -> p conditionally for explicit
  public:: energy_to_pressure_cell  ! e -> p in physical cells for implicit
  public:: pressure_to_energy       ! p -> e conditionally for explicit
  public:: pressure_to_energy_block ! p -> e in a block for part implicit
  public:: energy_i                 ! energy of fluid iFluid from State_V
  public:: pressure_i               ! pressure of fluid iFluid from Cons_V
  public:: get_fluid_energy_block   ! energy of fluid in a grid block
  public:: limit_pressure           ! Enforce minimum pressure and temperature

  ! It is possible to add the hyperbolic scalar energy to the total energy
  ! (Tricco & Price 2012, J. Comput. Phys. 231, 7214).
  ! Experiments so far do not show any benefit, but the code is preserved.
  logical, parameter:: UseHypEnergy = .false.

contains
  !============================================================================
  subroutine pressure_to_energy(iBlock, State_VGB)

    ! Calculate energy from pressure depending on
    ! the value of UseNonConservative and IsConserv_CB

    integer, intent(in):: iBlock
    real, intent(inout):: &
         State_VGB(nVar,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)

    integer:: i,j,k,iFluid
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'pressure_to_energy'
    !--------------------------------------------------------------------------
#ifndef SCALAR
    ! Make sure pressure is larger than floor value
    ! write(*,*) NameSub,' !!! call limit_pressure'
    call limit_pressure(1, nI, 1, nJ, 1, nK, iBlock, 1, nFluid)

    ! Fully non-conservative scheme
    if(UseNonConservative .and. nConservCrit <= 0 .and. &
         .not. (UseNeutralFluid .and. DoConserveNeutrals)) RETURN

    call test_start(NameSub, DoTest, iBlock)

    if(DoTest)write(*,*)NameSub, &
         ': UseNonConservative, DoConserveNeutrals, nConservCrit=', &
         UseNonConservative, DoConserveNeutrals, nConservCrit

    ! A mix of conservative and non-conservative scheme (at least for the ions)
    FLUIDLOOP: do iFluid = 1, nFluid

       ! If all neutrals are non-conservative exit from the loop
       if(iFluid > nIonFluid .and. .not. DoConserveNeutrals) EXIT FLUIDLOOP

       if(nFluid > 1) call select_fluid(iFluid)

       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE
          if(iFluid <= nIonFluid .and. .not.is_conserv(i, j, k, iBlock)) CYCLE
          ! Convert to hydro energy density
          State_VGB(iP,i,j,k,iBlock) =                             &
               InvGammaMinus1_I(iFluid)*State_VGB(iP,i,j,k,iBlock) &
               + 0.5*sum(State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock)**2) &
               /State_VGB(iRho,i,j,k,iBlock)

          ! Done with all fluids except first MHD fluid
          if(iFluid > 1 .or. .not.(UseTotalIonEnergy .or. IsMhd)) CYCLE
          if(UseSaMhd .and. r_GB(i,j,k,iBlock) > rMinSaMhd) CYCLE
          ! Add magnetic energy density for fluid 1
          State_VGB(iP,i,j,k,iBlock) = State_VGB(iP,i,j,k,iBlock) &
               + 0.5*sum(State_VGB(Bx_:Bz_,i,j,k,iBlock)**2)

          ! Add electron energy density if needed
          if(UseElectronPressure .and. UseElectronEnergy) &
               State_VGB(iP,i,j,k,iBlock) = State_VGB(iP,i,j,k,iBlock) &
               + State_VGB(Pe_,i,j,k,iBlock)*InvGammaElectronMinus1

          ! Add hyperbolic scalar energy density (idea of Daniel Price)
          if(Hyp_ > 1 .and. UseHypEnergy) &
               State_VGB(iP,i,j,k,iBlock) = State_VGB(iP,i,j,k,iBlock) &
               + 0.5*State_VGB(Hyp_,i,j,k,iBlock)**2

       end do; end do; end do

    end do FLUIDLOOP

    if(UseMultiIon .and. UseTotalIonEnergy)then
       ! Add up all ion energy densities into the first energy
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE
          if(.not. is_conserv(i, j, k, iBlock)) CYCLE
          State_VGB(p_,i,j,k,iBlock) = sum(State_VGB(iPIon_I,i,j,k,iBlock))
       end do; end do; end do
    end if

    call test_stop(NameSub, DoTest, iBlock)
#endif
  end subroutine pressure_to_energy
  !============================================================================
  real function energy_i(State_V, iFluidIn)

    ! Return energy of fluid iFluid from primitive State_V
    ! Default fluid is iFluid = 1.

    real, intent(in):: State_V(nVar)
    integer, intent(in), optional:: iFluidIn

    integer:: iFluid
#ifndef SCALAR
    !--------------------------------------------------------------------------
    iFluid = 1
    if(present(iFluidIn)) iFluid = iFluidIn

    ! The input State_V has pressures
    if(iFluid == 1 .and. UseMultiIon .and. UseTotalIonEnergy)then
       energy_i = sum(InvGammaMinus1_I(1:nIonFluid)*State_V(iPIon_I) + 0.5* &
            ( State_V(iRhoUxIon_I)**2 + State_V(iRhoUyIon_I)**2 &
            + State_V(iRhoUzIon_I)**2 &
            )/State_V(iRhoIon_I)) + 0.5*sum(State_V(Bx_:Bz_)**2)
    elseif(iFluid == 1 .and. IsMhd) then
       ! MHD energy density
       energy_i = InvGammaMinus1*State_V(p_) + 0.5* &
            ( sum(State_V(RhoUx_:RhoUz_)**2)/State_V(Rho_) &
            + sum(State_V(Bx_:Bz_)**2) )
    else
       ! Hydro energy density
       if(nFluid > 1) call select_fluid(iFluid)
       energy_i = InvGammaMinus1_I(iFluid)*State_V(iP) &
            + 0.5*sum(State_V(iRhoUx:iRhoUz)**2)/State_V(iRho)
    end if
    if(UseElectronPressure .and. UseElectronEnergy .and. iFluid == 1) &
         energy_i = energy_i + State_V(Pe_)*InvGammaElectronMinus1
#endif
  end function energy_i
  !============================================================================
  real function pressure_i(State_V, iFluidIn)

    ! Return pressure of fluid iFluid from conservative State_V.
    ! Default fluid is iFluid = 1.

    real, intent(in):: State_V(nVar)
    integer, intent(in), optional:: iFluidIn

    integer:: iFluid
#ifndef SCALAR
    !--------------------------------------------------------------------------
    iFluid = 1
    if(present(iFluidIn)) iFluid = iFluidIn

    ! The input State_V has energies (total ion energy is not handled yet!)
    if(iFluid == 1 .and. IsMhd) then
       ! Start from energy
       pressure_i = State_V(p_)
       ! Subtract electron energy
       if(UseElectronPressure .and. UseElectronEnergy) &
            pressure_i = pressure_i - State_V(Pe_)*InvGammaElectronMinus1
       ! Subtract kinetic and magnetic energies and conver to pressure
       pressure_i = GammaMinus1*(pressure_i - 0.5* &
            ( sum(State_V(RhoUx_:RhoUz_)**2)/State_V(Rho_) &
            - sum(State_V(Bx_:Bz_)**2) ) )
    else
       ! Hydro energy density
       if(nFluid > 1) call select_fluid(iFluid)
       pressure_i = GammaMinus1_I(iFluid)*(State_V(iP) &
            - 0.5*sum(State_V(iRhoUx:iRhoUz)**2)/State_V(iRho))
    end if
#endif
  end function pressure_i
  !============================================================================
  subroutine get_fluid_energy_block(iBlock, iFluid, Energy_G)

    integer, intent(in):: iBlock, iFluid
    real, intent(inout):: Energy_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)

    ! calculate the energy of fluid iFluid in physical cells of block iBlock

    integer:: i, j, k
    !--------------------------------------------------------------------------
    if(nFluid > 1) call select_fluid(iFluid)

    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if(State_VGB(iRho,i,j,k,iBlock) <= 0.0)then
          Energy_G(i,j,k) = 0.0
       elseif(UseMultiIon .and. UseTotalIonEnergy .and. iFluid == 1)then
          ! Add up all ion fluid hydro energies
          Energy_G(i,j,k) = sum( &
               InvGammaMinus1_I(1:nIonFluid)*State_VGB(iPIon_I,i,j,k,iBlock) &
               + 0.5*(State_VGB(iRhoUxIon_I,i,j,k,iBlock)**2  &
               +      State_VGB(iRhoUyIon_I,i,j,k,iBlock)**2  &
               +      State_VGB(iRhoUzIon_I,i,j,k,iBlock)**2) &
               /State_VGB(iRhoIon_I,i,j,k,iBlock))
       else
          ! Hydro energy of a single fluid
          Energy_G(i,j,k) = &
               InvGammaMinus1_I(iFluid)*State_VGB(iP,i,j,k,iBlock) &
               + 0.5*sum(State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock)**2) &
               /State_VGB(iRho,i,j,k,iBlock)
       end if
       ! Add magnetic energy if needed
       if(iFluid == 1 .and. (IsMhd .or. UseTotalIonEnergy)             &
            .and..not.(UseSaMhd .and. r_GB(i,j,k,iBlock) > rMinSaMhd)) &
            Energy_G(i,j,k) = Energy_G(i,j,k) &
            + 0.5*sum(State_VGB(Bx_:Bz_,i,j,k,iBlock)**2)
       ! Add electron energy density if needed
       if(UseElectronPressure .and. UseElectronEnergy .and. iFluid == 1) &
            Energy_G(i,j,k) = Energy_G(i,j,k) &
            + State_VGB(Pe_,i,j,k,iBlock)*InvGammaElectronMinus1

    end do; end do; end do

  end subroutine get_fluid_energy_block
  !============================================================================
  subroutine pressure_to_energy_block(State_VG, &
       iMin, iMax, jMin, jMax, kMin, kMax, iBlock)

    ! Convert pressure to energy in all cells of State_VG
    ! This is only called from the (part)implicit scheme.
    integer, intent(in)::  iMin, iMax, jMin, jMax, kMin, kMax, iBlock
    real, intent(inout):: State_VG(nVar,iMin:iMax,jMin:jMax,kMin:kMax)

    integer:: i, j, k, iFluid
    !--------------------------------------------------------------------------
#ifndef SCALAR
    do iFluid = 1, nFluid

       if(nFluid > 1) call select_fluid(iFluid)

       do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
          ! Convert to hydro energy density
          State_VG(iP,i,j,k) =                             &
               InvGammaMinus1_I(iFluid)*State_VG(iP,i,j,k) &
               + 0.5*sum(State_VG(iRhoUx:iRhoUz,i,j,k)**2) &
               /State_VG(iRho,i,j,k)

          ! Add magnetic energy density
          if(iFluid == 1 .and. (IsMhd .or. UseTotalIonEnergy) &
               .and. .not.(UseSaMhd.and.r_GB(i,j,k,iBlock) > rMinSaMhd) ) &
               State_VG(iP,i,j,k) = State_VG(iP,i,j,k) &
               + 0.5*sum(State_VG(Bx_:Bz_,i,j,k)**2)

          ! Add electron thermal energy if needed
          if(UseElectronPressure .and. UseElectronEnergy .and. iFluid == 1) &
               State_VG(iP,i,j,k) = State_VG(iP,i,j,k) &
               + State_VG(Pe_,i,j,k)*InvGammaElectronMinus1
       end do; end do; end do
    end do

    if(UseMultiIon .and. UseTotalIonEnergy)then
       ! Add up ion energy densities
       do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
          State_VG(p_,i,j,k) = sum(State_VG(iPIon_I,i,j,k))
       end do; end do; end do
    end if
#endif
  end subroutine pressure_to_energy_block
  !============================================================================
  subroutine energy_to_pressure(iBlock, State_VGB)

    ! Convert energy to pressure in State_VGB depending on
    ! the value of UseNonConservative and IsConserv_CB

    integer, intent(in):: iBlock
    real, intent(inout):: &
         State_VGB(nVar,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)

    integer:: i, j, k, iFluid
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'energy_to_pressure'
    !--------------------------------------------------------------------------
#ifndef SCALAR
    ! Fully non-conservative scheme
    if(UseNonConservative .and. nConservCrit <= 0 .and. &
         .not. (UseNeutralFluid .and. DoConserveNeutrals))then

       ! Make sure pressure is larger than floor value
       call limit_pressure(1, nI, 1, nJ, 1, nK, iBlock, 1, nFluid)

       RETURN
    end if

    call test_start(NameSub, DoTest, iBlock)

    if(DoTest)write(*,*)NameSub, &
         ': UseNonConservative, DoConserveNeutrals, nConservCrit=', &
         UseNonConservative, DoConserveNeutrals, nConservCrit

    FLUIDLOOP: do iFluid = 1, nFluid

       ! If all neutrals are non-conservative exit from the loop
       if(iFluid > nIonFluid .and. .not. DoConserveNeutrals) EXIT FLUIDLOOP

       if(nFluid > 1) call select_fluid(iFluid)

       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE
          if(iFluid <= nIonFluid .and..not.is_conserv(i, j, k, iBlock)) CYCLE

          if(iFluid == 1 .and. (IsMhd .or. UseTotalIonEnergy) &
               .and. .not.(UseSaMhd .and. r_GB(i,j,k,iBlock) > rMinSaMhd))then

             ! Subtract the magnetic energy density
             State_VGB(iP,i,j,k,iBlock) = State_VGB(iP,i,j,k,iBlock) &
                  - 0.5*sum(State_VGB(Bx_:Bz_,i,j,k,iBlock)**2)

             ! Subtract hyperbolic scalar energy density (from Daniel Price)
             if(Hyp_ > 1 .and. UseHypEnergy) &
                  State_VGB(iP,i,j,k,iBlock) = State_VGB(iP,i,j,k,iBlock) &
                  - 0.5*State_VGB(Hyp_,i,j,k,iBlock)**2
          end if

          ! Subtract electron thermal energy if needed
          if(UseElectronPressure .and. UseElectronEnergy .and. iFluid == 1) &
               State_VGB(iP,i,j,k,iBlock) = State_VGB(iP,i,j,k,iBlock) &
               - State_VGB(Pe_,i,j,k,iBlock)*InvGammaElectronMinus1

          ! Subtract hydro energy densities of fluids 2..nIonFluid if needed
          if(UseMultiIon .and. UseTotalIonEnergy .and. iFluid == 1) &
               State_VGB(iP,i,j,k,iBlock) = State_VGB(iP,i,j,k,iBlock) &
               - sum(State_VGB(iPIon_I(2:),i,j,k,iBlock))

          ! Convert from hydro energy density to pressure
          State_VGB(iP,i,j,k,iBlock) =                             &
               GammaMinus1_I(iFluid)*(State_VGB(iP,i,j,k,iBlock) &
               - 0.5*sum(State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock)**2) &
               /State_VGB(iRho,i,j,k,iBlock))
       end do; end do; end do

    end do FLUIDLOOP

    ! Make sure final pressure is larger than floor value
    call limit_pressure(1, nI, 1, nJ, 1, nK, iBlock, 1, nFluid)

    call test_stop(NameSub, DoTest, iBlock)
#endif
  end subroutine energy_to_pressure
  !============================================================================
  subroutine energy_to_pressure_cell(iBlock, State_VGB)

    ! Convert energy to pressure in physical cells of block iBlock of State_VGB

    integer, intent(in):: iBlock
    real, intent(inout):: &
         State_VGB(nVar,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)

    integer:: i, j, k, iFluid
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'energy_to_pressure_cell'
    !--------------------------------------------------------------------------
#ifndef SCALAR
    call test_start(NameSub, DoTest, iBlock)

    FLUIDLOOP: do iFluid = 1, nFluid

       if(nFluid > 1) call select_fluid(iFluid)

       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          ! Leave the body cells alone
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE

          ! Subtract the magnetic energy density
          if(iFluid == 1 .and. (IsMhd .or. UseTotalIonEnergy) &
               .and. .not.(UseSaMhd .and. r_GB(i,j,k,iBlock) > rMinSaMhd)) &
               State_VGB(iP,i,j,k,iBlock) = State_VGB(iP,i,j,k,iBlock) &
               - 0.5*sum(State_VGB(Bx_:Bz_,i,j,k,iBlock)**2)

          ! Subtract electron energy if needed
          if(UseElectronPressure .and. UseElectronEnergy .and. iFluid == 1) &
               State_VGB(iP,i,j,k,iBlock) = State_VGB(iP,i,j,k,iBlock) &
               - State_VGB(Pe_,i,j,k,iBlock)*InvGammaElectronMinus1

          ! Subtract hydro energy densities of fluids 2..nIonFluid if needed
          if(UseMultiIon .and. UseTotalIonEnergy .and. iFluid == 1) &
               State_VGB(iP,i,j,k,iBlock) = State_VGB(iP,i,j,k,iBlock) &
               - sum(State_VGB(iPIon_I(2:),i,j,k,iBlock))

          ! Convert from hydro energy density to pressure
          State_VGB(iP,i,j,k,iBlock) =                             &
               GammaMinus1_I(iFluid)*(State_VGB(iP,i,j,k,iBlock)   &
               - 0.5*sum(State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock)**2) &
               /State_VGB(iRho,i,j,k,iBlock))
       end do; end do; end do

    end do FLUIDLOOP

    call test_stop(NameSub, DoTest, iBlock)
#endif
  end subroutine energy_to_pressure_cell
  !============================================================================
  subroutine limit_pressure(iMin, iMax, jMin, jMax, kMin, kMax, iBlock, &
       iFluidMin, iFluidMax)
    !$acc routine vector

    ! Keep pressure(s) in State_VGB above pMin_I limit

    use ModAdvance, ONLY: UseAnisoPressure, UseAnisoPe
    use ModMultiFluid, ONLY: IsIon_I, iPparIon_I

    integer, intent(in) :: iMin, iMax, jMin, jMax, kMin, kMax, iBlock
    integer, intent(in) :: iFluidMin, iFluidMax

    integer:: i, j, k, iFluid
    real :: NumDens, pMin, Ne

    character(len=*), parameter:: NameSub = 'limit_pressure'
    !--------------------------------------------------------------------------
    do iFluid = iFluidMin, iFluidMax
       if(pMin_I(iFluid) < 0.0) CYCLE
       pMin = pMin_I(iFluid)
       iP = iP_I(iFluid)
       !$acc loop vector collapse(3) independent
       do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
          State_VGB(iP,i,j,k,iBlock) = max(pMin, State_VGB(iP,i,j,k,iBlock))
          if(UseAnisoPressure .and. IsIon_I(iFluid))&
               State_VGB(iPparIon_I(iFluid),i,j,k,iBlock) = &
               min(max(pMin, State_VGB(iPparIon_I(iFluid),i,j,k,iBlock)), &
               (3*State_VGB(iP_I(iFluid),i,j,k,iBlock) - 2*pMin))
       end do; end do; end do
    end do

    do iFluid = iFluidMin, iFluidMax
       if(Tmin_I(iFluid) < 0.0) CYCLE
       iP = iP_I(iFluid)
       !$acc loop vector collapse(3) independent
       do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
          NumDens=State_VGB(iRho_I(iFluid),i,j,k,iBlock)/MassFluid_I(iFluid)
          pMin = NumDens*Tmin_I(iFluid)
          State_VGB(iP,i,j,k,iBlock) = max(pMin, State_VGB(iP,i,j,k,iBlock))
          if(UseAnisoPressure .and. IsIon_I(iFluid))&
               State_VGB(iPparIon_I(iFluid),i,j,k,iBlock) = &
               min(max(pMin, State_VGB(iPparIon_I(iFluid),i,j,k,iBlock)), &
               (3*State_VGB(iP_I(iFluid),i,j,k,iBlock) - 2*pMin))
       end do; end do; end do
    end do

    if(UseElectronPressure .and. iFluidMin == 1)then
       if(PeMin > 0.0)then
          !$acc loop vector collapse(3) independent
          do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
             State_VGB(Pe_,i,j,k,iBlock) = &
                  max(PeMin, State_VGB(Pe_,i,j,k,iBlock))
             if(UseAnisoPe) State_VGB(Pepar_,i,j,k,iBlock)  = &
                  max(peMin, State_VGB(Pepar_,i,j,k,iBlock))
          end do; end do; end do
       end if
       if(TeMin > 0.0)then
          !$acc loop vector collapse(3) independent
          do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
             Ne = sum(ChargeIon_I*State_VGB(iRhoIon_I,i,j,k,iBlock)/MassIon_I)
             State_VGB(Pe_,i,j,k,iBlock) = &
                  max(Ne*TeMin, State_VGB(Pe_,i,j,k,iBlock))
             if(UseAnisoPe) State_VGB(Pepar_,i,j,k,iBlock)  = &
                  max(Ne*TeMin, State_VGB(Pepar_,i,j,k,iBlock))
          end do; end do; end do
       end if
    end if

  end subroutine limit_pressure
  !============================================================================
end module ModEnergy
!==============================================================================

