!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModEnergy

  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest, iBlockTest
  use ModExtraVariables, ONLY: Pepar_
  use ModVarIndexes, ONLY: nVar, Rho_, RhoUx_, RhoUy_, RhoUz_, Bx_, By_, Bz_, &
       Hyp_, p_, Pe_, IsMhd
  use ModMultiFluid, ONLY: nFluid, IonLast_, &
       iRho, iRhoUx, iRhoUy, iRhoUz, iP, iP_I, &
       UseNeutralFluid, DoConserveNeutrals, &
       select_fluid, MassFluid_I, iRho_I, iRhoIon_I, MassIon_I, ChargeIon_I
  use ModAdvance,    ONLY: State_VGB, Energy_GBI, StateOld_VGB, &
       UseNonConservative, nConservCrit, IsConserv_CB, UseElectronPressure
  use ModPhysics,    ONLY: GammaMinus1_I, InvGammaMinus1_I, InvGammaMinus1, &
       pMin_I, PeMin, Tmin_I, TeMin
  use BATL_lib, ONLY: nI, nJ, nK, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, &
       MaxBlock, Used_GB
  
  implicit none

  private ! except

  public:: energy_to_pressure       ! e -> p conditionally for explicit
  public:: energy_to_pressure_cell  ! e -> p in physical cells for implicit
  public:: pressure_to_energy       ! p -> e conditionally for explicit
  public:: pressure_to_energy_block ! p -> e in a block for part implicit
  public:: calc_energy              ! p -> e in a range of cells/fluids
  public:: calc_energy_point        ! p -> e in one cell for all fluids
  public:: calc_energy_cell         ! p -> e in physical cells
  public:: calc_energy_ghost        ! p -> e in ghost cells
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
       if(iFluid > IonLast_ .and. .not. DoConserveNeutrals) EXIT FLUIDLOOP

       if(nFluid > 1) call select_fluid(iFluid)
       
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE
          if(UseNonConservative .and. iFluid <= IonLast_)then
             if(.not.IsConserv_CB(i,j,k,iBlock)) CYCLE
          end if
          ! Convert to hydro energy density
          State_VGB(iP,i,j,k,iBlock) =                             &
               InvGammaMinus1_I(iFluid)*State_VGB(iP,i,j,k,iBlock) &
               + 0.5*sum(State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock)**2) &
               /State_VGB(iRho,i,j,k,iBlock)

          ! Done with all fluids except first MHD fluid
          if(iFluid > 1 .or. .not. IsMhd) CYCLE

          ! Add magnetic energy density
          State_VGB(iP,i,j,k,iBlock) = State_VGB(iP,i,j,k,iBlock) &
               + 0.5*sum(State_VGB(Bx_:Bz_,i,j,k,iBlock)**2)

          if(Hyp_ <=1 .or. .not. UseHypEnergy) CYCLE
          ! Add hyperbolic scalar energy density (idea of Daniel Price)
          State_VGB(iP,i,j,k,iBlock) = State_VGB(iP,i,j,k,iBlock) &
               + 0.5*State_VGB(Hyp_,i,j,k,iBlock)**2

       end do; end do; end do

    end do FLUIDLOOP

    call test_stop(NameSub, DoTest, iBlock)

  end subroutine pressure_to_energy
  !============================================================================
  subroutine pressure_to_energy_block(State_VG, &
       iMin, iMax, jMin, jMax, kMin, kMax)

    ! Convert pressure to energy in all cells of State_VG
    integer, intent(in)::  iMin, iMax, jMin, jMax, kMin, kMax
    real, intent(inout):: State_VG(nVar,iMin:iMax,jMin:jMax,kMin:kMax)

    integer:: i, j, k, iFluid
    !--------------------------------------------------------------------------
    do iFluid = 1, nFluid
       
       if(nFluid > 1) call select_fluid(iFluid)
       
       do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax

          ! Convert to hydro energy density
          State_VG(iP,i,j,k) =                             &
               InvGammaMinus1_I(iFluid)*State_VG(iP,i,j,k) &
               + 0.5*sum(State_VG(iRhoUx:iRhoUz,i,j,k)**2) &
               /State_VG(iRho,i,j,k)

          ! Add magnetic energy density
          if(iFluid == 1 .and. IsMhd) State_VG(iP,i,j,k) = State_VG(iP,i,j,k) &
               + 0.5*sum(State_VG(Bx_:Bz_,i,j,k)**2)

       end do; end do; end do
    end do

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
    ! Fully non-conservative scheme
    if(UseNonConservative .and. nConservCrit <= 0 .and. &
         .not. (UseNeutralFluid .and. DoConserveNeutrals))then

       ! Make sure pressure is larger than floor value
       ! write(*,*) NameSub,' !!! call limit_pressure'
       call limit_pressure(1, nI, 1, nJ, 1, nK, iBlock, 1, nFluid)

       RETURN
    end if
    
    call test_start(NameSub, DoTest, iBlock)

    if(DoTest)write(*,*)NameSub, &
         ': UseNonConservative, DoConserveNeutrals, nConservCrit=', &
         UseNonConservative, DoConserveNeutrals, nConservCrit

    FLUIDLOOP: do iFluid = 1, nFluid
       
       ! If all neutrals are non-conservative exit from the loop
       if(iFluid > IonLast_ .and. .not. DoConserveNeutrals) EXIT FLUIDLOOP

       if(nFluid > 1) call select_fluid(iFluid)
       
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE
          if(UseNonConservative .and. iFluid <= IonLast_)then
             ! Apply conservative criteria for the ions
             if(.not.IsConserv_CB(i,j,k,iBlock)) CYCLE
          end if

          if(iFluid == 1 .and. IsMhd) then
             ! Deal with first MHD fluid

             ! Subtract the magnetic energy density
             State_VGB(iP,i,j,k,iBlock) = State_VGB(iP,i,j,k,iBlock) &
                  - 0.5*sum(State_VGB(Bx_:Bz_,i,j,k,iBlock)**2)

             ! Subtract hyperbolic scalar energy density (from Daniel Price)
             if(Hyp_ > 1 .and. UseHypEnergy) &
                  State_VGB(iP,i,j,k,iBlock) = State_VGB(iP,i,j,k,iBlock) &
                  - 0.5*State_VGB(Hyp_,i,j,k,iBlock)**2
          end if
          
          ! Convert from hydro energy density to pressure
          State_VGB(iP,i,j,k,iBlock) =                             &
               GammaMinus1_I(iFluid)*(State_VGB(iP,i,j,k,iBlock) &
               - 0.5*sum(State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock)**2) &
               /State_VGB(iRho,i,j,k,iBlock))
       end do; end do; end do

    end do FLUIDLOOP

    ! Make sure final pressure is larger than floor value
    ! write(*,*) NameSub,' !!! call limit_pressure'
    call limit_pressure(1, nI, 1, nJ, 1, nK, iBlock, 1, nFluid)

    call test_stop(NameSub, DoTest, iBlock)

  end subroutine energy_to_pressure
  !============================================================================
  subroutine energy_to_pressure_cell(iBlock, State_VGB)

    ! Convert energy to pressure in physical cells of block iBlock of State_VGB

    integer, intent(in):: iBlock
    real, intent(inout):: &
         State_VGB(nVar,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)

    integer:: i,j,k,iFluid
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'pressure_to_energy'
    !--------------------------------------------------------------------------
    ! Fully non-conservative scheme
    if(UseNonConservative .and. nConservCrit <= 0 .and. &
         .not. (UseNeutralFluid .and. DoConserveNeutrals)) RETURN

    call test_start(NameSub, DoTest, iBlock)

    if(DoTest)write(*,*)NameSub, &
         ': UseNonConservative, DoConserveNeutrals, nConservCrit=', &
         UseNonConservative, DoConserveNeutrals, nConservCrit

    FLUIDLOOP: do iFluid = 1, nFluid
       
       if(nFluid > 1) call select_fluid(iFluid)
       
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          ! Leave the body cells alone
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE
          
          ! Subtract the magnetic energy density
          if(iFluid == 1 .and. IsMhd) then
             State_VGB(iP,i,j,k,iBlock) = State_VGB(iP,i,j,k,iBlock) &
                  - 0.5*sum(State_VGB(Bx_:Bz_,i,j,k,iBlock)**2)
          end if
          
          ! Convert from hydro energy density to pressure
          State_VGB(iP,i,j,k,iBlock) =                             &
               GammaMinus1_I(iFluid)*(State_VGB(iP,i,j,k,iBlock) &
               - 0.5*sum(State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock)**2) &
               /State_VGB(iRho,i,j,k,iBlock))
       end do; end do; end do

    end do FLUIDLOOP

    call test_stop(NameSub, DoTest, iBlock)

  end subroutine energy_to_pressure_cell
  !============================================================================
  subroutine calc_energy(iMin, iMax, jMin, jMax, kMin, kMax, iBlock, &
       iFluidMin, iFluidMax)

    ! Calculate total energy (excluding B0):
    !
    !   E = p/(gamma-1) + 0.5*rho*u^2 + 0.5*b1^2
    !

    integer, intent(in) :: iMin, iMax, jMin, jMax, kMin, kMax, iBlock
    integer, intent(in) :: iFluidMin, iFluidMax
    integer:: i,j,k,iFluid

#ifndef OPENACC
    character(len=*), parameter:: NameSub = 'calc_energy'
    !--------------------------------------------------------------------------
    ! write(*,*) NameSub,' !!! call limit_pressure'
    call limit_pressure(iMin, iMax, jMin, jMax, kMin, kMax, &
         iBlock, iFluidMin, iFluidMax)

    do iFluid = iFluidMin, iFluidMax
       call select_fluid(iFluid)
       ! Calculate thermal plus kinetic energy
       do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
          if (State_VGB(iRho,i,j,k,iBlock) <= 0.0)then
             Energy_GBI(i,j,k,iBlock,iFluid) = 0.0
          else
             Energy_GBI(i,j,k,iBlock,iFluid) = &
                  InvGammaMinus1_I(iFluid)*State_VGB(iP,i,j,k,iBlock) &
                  +0.5*(sum(State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock)**2)/&
                  State_VGB(iRho,i,j,k,iBlock))
          end if
       end do; end do; end do

       if(iFluid > 1 .or. .not. IsMhd) CYCLE

       ! Add magnetic energy for ion fluid
       do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
          Energy_GBI(i,j,k,iBlock,iFluid) = &
               Energy_GBI(i,j,k,iBlock,iFluid) + &
               0.5*sum(State_VGB(Bx_:Bz_,i,j,k,iBlock)**2)
       end do; end do; end do

       if(Hyp_ > 1 .and. UseHypEnergy)then
          do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
             Energy_GBI(i,j,k,iBlock,iFluid) = &
                  Energy_GBI(i,j,k,iBlock,iFluid) + &
                  0.5*State_VGB(Hyp_,i,j,k,iBlock)**2
          end do; end do; end do
       end if

    end do
#endif
  end subroutine calc_energy
  !============================================================================
  subroutine calc_energy_cell(iBlock)
    integer, intent(in) :: iBlock
    !--------------------------------------------------------------------------
    call calc_energy(1,nI,1,nJ,1,nK,iBlock,1,nFluid)
  end subroutine calc_energy_cell
  !============================================================================

  subroutine calc_energy_ghost(iBlock, DoResChangeOnlyIn, UseOpenACCIn)
    use BATL_lib, ONLY: DiLevelNei_IIIB

    integer, intent(in) :: iBlock
    logical, optional, intent(in) :: DoResChangeOnlyIn

    ! TOD0: This command is introduced as a temporary solution. It needs
    ! to be reoved when more code is ported to GPU.
    logical, optional, intent(in) :: UseOpenACCIn

    integer :: i, j, k, iFluid
    logical :: DoResChangeOnly, UseOpenACC

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_energy_ghost'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    DoResChangeOnly =.false.
    if(present(DoResChangeOnlyIn)) DoResChangeOnly = DoResChangeOnlyIn

    UseOpenACC = .false.
    if(present(UseOpenACCIn)) UseOpenACC = UseOpenACCIn

    if( DoResChangeOnly ) then
       if( .not.any(abs(DiLevelNei_IIIB(-1:1,-1:1,-1:1,iBlock)) == 1) ) RETURN
    end if
    ! write(*,*) NameSub,' !!! call limit_pressure'
    call limit_pressure(MinI, MaxI, MinJ, MaxJ, MinK, MaxK, iBlock, 1, nFluid)

    do iFluid = 1, nFluid
       if(nFluid > 1) call select_fluid(iFluid)

       if(IsMhd .and. iFluid == 1) then
          ! MHD energy

          if(UseOpenACC) then
             do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
                if(State_VGB(Rho_,i,j,k,iBlock) <= 0.0)then
                   Energy_GBI(i,j,k,iBlock,iFluid) = 0.0
                else
                   Energy_GBI(i,j,k,iBlock,iFluid) = &
                        InvGammaMinus1*State_VGB(p_,i,j,k,iBlock) + 0.5*(&
                        State_VGB(RhoUx_,i,j,k,iBlock)**2 + &
                        State_VGB(RhoUy_,i,j,k,iBlock)**2 + &
                        State_VGB(RhoUz_,i,j,k,iBlock)**2) &
                        /State_VGB(Rho_,i,j,k,iBlock) + &
                        0.5*(&
                        State_VGB(Bx_,i,j,k,iBlock)**2 + &
                        State_VGB(By_,i,j,k,iBlock)**2 + &
                        State_VGB(Bz_,i,j,k,iBlock)**2)

                   if(Hyp_ > 1 .and. UseHypEnergy) &
                        Energy_GBI(i,j,k,iBlock,iFluid) = &
                        Energy_GBI(i,j,k,iBlock,iFluid) &
                        + 0.5*State_VGB(Hyp_,i,j,k,iBlock)**2

                end if
             end do; end do; end do
          else
             do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
                if(State_VGB(Rho_,i,j,k,iBlock) <= 0.0)then
                   Energy_GBI(i,j,k,iBlock,iFluid) = 0.0
                else
                   Energy_GBI(i,j,k,iBlock,iFluid) = &
                        InvGammaMinus1*State_VGB(p_,i,j,k,iBlock) &
                        + 0.5*sum(State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)**2) &
                        /State_VGB(Rho_,i,j,k,iBlock) + &
                        0.5*sum(State_VGB(Bx_:Bz_,i,j,k,iBlock)**2)

                   if(Hyp_ > 1 .and. UseHypEnergy) &
                        Energy_GBI(i,j,k,iBlock,iFluid) = &
                        Energy_GBI(i,j,k,iBlock,iFluid) &
                        + 0.5*State_VGB(Hyp_,i,j,k,iBlock)**2

                end if
             end do; end do; end do
          endif

       else
          ! HD energy
          do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
             if(State_VGB(iRho,i,j,k,iBlock) <= 0.0)then
                Energy_GBI(i,j,k,iBlock,iFluid) = 0.0
             else
                Energy_GBI(i,j,k,iBlock,iFluid) = &
                     InvGammaMinus1_I(iFluid)*State_VGB(iP,i,j,k,iBlock) &
                     + 0.5*sum(State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock)**2) &
                     /State_VGB(iRho,i,j,k,iBlock)
             end if
          end do; end do; end do
       end if
    end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine calc_energy_ghost
  !============================================================================
  subroutine calc_energy_point(i, j, k, iBlock)
    integer, intent(in) :: i, j, k, iBlock
    !--------------------------------------------------------------------------
    call calc_energy(i,i,j,j,k,k,iBlock,1,nFluid)
  end subroutine calc_energy_point
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
    real :: NumDens, p, pMin, Ne

    character(len=*), parameter:: NameSub = 'limit_pressure'
    !--------------------------------------------------------------------------
#ifndef OPENACC
    do iFluid = iFluidMin, iFluidMax
       if(pMin_I(iFluid) < 0.0) CYCLE
       pMin = pMin_I(iFluid)
       iP = iP_I(iFluid)
       do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
          State_VGB(iP,i,j,k,iBlock) = max(pMin, State_VGB(iP,i,j,k,iBlock))
          if(UseAnisoPressure .and. IsIon_I(iFluid))&
               State_VGB(iPparIon_I(iFluid),i,j,k,iBlock) = &
               min(max(pMin, State_VGB(iPparIon_I(iFluid),i,j,k,iBlock)),(3*State_VGB(iP_I(iFluid),i,j,k,iBlock) - 2*pMin))
       end do; end do; end do
    end do

    do iFluid = iFluidMin, iFluidMax
       if(Tmin_I(iFluid) < 0.0) CYCLE
       iP = iP_I(iFluid)
       do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
          NumDens=State_VGB(iRho_I(iFluid),i,j,k,iBlock)/MassFluid_I(iFluid)
          pMin = NumDens*Tmin_I(iFluid)
          State_VGB(iP,i,j,k,iBlock) = max(pMin, State_VGB(iP,i,j,k,iBlock))
          if(UseAnisoPressure .and. IsIon_I(iFluid))&
               State_VGB(iPparIon_I(iFluid),i,j,k,iBlock) = &
               min(max(pMin, State_VGB(iPparIon_I(iFluid),i,j,k,iBlock)),(3*State_VGB(iP_I(iFluid),i,j,k,iBlock) - 2*pMin))
       end do; end do; end do
    end do

    if(UseElectronPressure .and. iFluidMin == 1)then
       if(PeMin > 0.0)then
          do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
             State_VGB(Pe_,i,j,k,iBlock) = &
                  max(PeMin, State_VGB(Pe_,i,j,k,iBlock))
             if(UseAnisoPe) State_VGB(Pepar_,i,j,k,iBlock)  = &
                  max(peMin, State_VGB(Pepar_,i,j,k,iBlock))
          end do; end do; end do
       end if
       if(TeMin > 0.0)then
          do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
             Ne = sum(ChargeIon_I*State_VGB(iRhoIon_I,i,j,k,iBlock)/MassIon_I)
             State_VGB(Pe_,i,j,k,iBlock) = &
                  max(Ne*TeMin, State_VGB(Pe_,i,j,k,iBlock))
             if(UseAnisoPe) State_VGB(Pepar_,i,j,k,iBlock)  = &
                  max(Ne*TeMin, State_VGB(Pepar_,i,j,k,iBlock))
          end do; end do; end do
       end if
    end if
#endif
  end subroutine limit_pressure
  !============================================================================

end module ModEnergy
!==============================================================================

