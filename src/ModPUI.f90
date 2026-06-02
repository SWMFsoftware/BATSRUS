!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModPUI

  use BATL_lib, ONLY: test_start, test_stop
  use BATL_size, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK
  use ModVarIndexes, ONLY: nPui, PuiFirst_, PuiLast_, nIonFluid
  use ModBatsrusUtility, ONLY: stop_mpi
  use ModSize, ONLY: nI, nJ, nK

  implicit none
  save

  private ! except

  public :: read_pui_param
  public :: init_mod_pui
  public :: set_pui_state
  public :: pui_advection_diffusion
  public :: get_pui_flux

  integer, parameter, public :: Pu3_ = nIonFluid

  real :: VpuiMinSi = 1e4, VpuiMin
  real :: VpuiMaxSi = 6e6, VpuiMax

  ! Logarithmic velocity grid for the PUIs.
  ! The bin centered velocities are:
  real, public :: Vpui_I(nPui)
  real, public :: DeltaVpui_I(nPui)

  real, public :: DeltaLogVpui

  real, public :: DivUpui_C(nI,nJ,nK) = 0.0

  ! For spatial diffusion at the termination shock
  logical, public :: UsePuiDiffusion = .false.
  logical, allocatable, public :: DoPuiDiffusion_B(:)
  real, public :: PuiDiffCoefSi = 1.E17   ! m^2/s
  real, public :: PuiDiffCoef = 0.0
  real, public :: PuiDiffV0Si
  real, public :: PuiDiffV0
  real, public :: PuiDiffSlope
  real, allocatable :: Fpui_IG(:,:,:,:)

contains
  !============================================================================
  subroutine read_pui_param(NameCommand)

    use ModReadParam, ONLY: read_var

    character(len=*), intent(in):: NameCommand

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_pui_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    select case(NameCommand)
    case("#PUIGRID")
       call read_var('VpuiMinSi', VpuiMinSi)
       call read_var('VpuiMaxSi', VpuiMaxSi)
    case("#PUIDIFFUSION")
       call read_var('UsePuiDiffusion', UsePuiDiffusion)
       if(UsePuiDiffusion) then
          call read_var('PuiDiffCoefSi', PuiDiffCoefSi)
          call read_var('PuiDiffV0Si', PuiDiffV0Si)
          call read_var('PuiDiffSlope', PuiDiffSlope)
       end if
    case default
       call stop_mpi(NameSub//": unknown command="//trim(NameCommand))
    end select

    call test_stop(NameSub, DoTest)
  end subroutine read_pui_param
  !============================================================================
  subroutine init_mod_pui

    use ModWaves, ONLY: WaveFirst_
    use ModPhysics, ONLY: Si2No_V, UnitU_, UnitX_
    use BATL_size, ONLY: MaxBlock

    integer ::  iPui

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_mod_pui'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    VpuiMin = VpuiMinSI*Si2No_V(UnitU_)
    VpuiMax = VpuiMaxSi*Si2No_V(UnitU_)

    DeltaLogVpui = log(VpuiMax/VpuiMin)/nPui

    ! Bin centered PUI velocities on log grid
    Vpui_I(1) = VpuiMin*exp(0.5*DeltaLogVpui)
    do iPui = 2, nPui
       Vpui_I(iPui) = Vpui_I(iPui-1)*exp(DeltaLogVpui)
    end do

    DeltaVpui_I(1) = VpuiMin*(exp(DeltaLogVpui) - 1.0)
    do iPui = 2, nPui
       DeltaVpui_I(iPui) = DeltaVpui_I(iPui-1)*exp(DeltaLogVpui)
    end do

    if (UsePuiDiffusion)then
       PuiDiffCoef = PuiDiffCoefSi * Si2No_V(UnitU_)*Si2No_V(UnitX_)
       PuiDiffV0 = PuiDiffV0Si *Si2No_V(UnitU_)
       if (.not. allocated(DoPuiDiffusion_B)) &
            allocate(DoPuiDiffusion_B(MaxBlock))
       if (.not. allocated(Fpui_IG)) &
            allocate(Fpui_IG(PuiFirst_:PuiLast_,MinI:MaxI,MinJ:MaxJ,MinK:MaxK))
    else
       if(allocated(DoPuiDiffusion_B)) &
            deallocate(DoPuiDiffusion_B)
       if(allocated(Fpui_IG)) &
            deallocate(Fpui_IG)
    end if

    call test_stop(NameSub, DoTest)
  end subroutine init_mod_pui
  !============================================================================
  subroutine set_pui_state(State_V, StateRead_V, iVarMatch_V)

    use ModNumConst, ONLY: cPi
    use ModMultiFluid, ONLY: iRho_I, iRhoUx_I, iRhoUy_I, iRhoUz_I, iP_I
    use ModVarIndexes, ONLY: nVar

    real, intent(inout) :: State_V(nVar)
    real, optional, intent(in) :: StateRead_V(:)
    integer, optional, intent(in) :: iVarMatch_V(nVar)

    real :: RhoPui, Ppui, Vpui
    integer :: iPui1, iPui2

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_pui_state'
    !--------------------------------------------------------------------------
    if(PuiFirst_ > 1 .and. nPui == 1)then
       State_V(PuiFirst_:PuiLast_) = 0.0
       RETURN
    end if

    if(present(StateRead_V))then
       RhoPui = StateRead_V(iVarMatch_V(iRho_I(Pu3_)))
       Ppui = StateRead_V(iVarMatch_V(iP_I(Pu3_)))
    else
       RhoPui = State_V(iRho_I(Pu3_))
       Ppui = State_V(iP_I(Pu3_))
    end if

    Vpui = sqrt(3*Ppui/RhoPui)

    iPui1 = floor(log(Vpui/Vpui_I(1))/DeltaLogVpui) +1
    iPui2 = iPui1+1

    ! Set all bins to zero except for a shell
    State_V(PuiFirst_:PuiLast_) = 0
    if (iPui1 < 1) then
       State_V(PuiFirst_) = RhoPui/(4*cPi*Vpui_I(1)**2*DeltaVpui_I(1))
    else if (iPui2 > nPui) then
       State_V(PuiLast_) = RhoPui/(4*cPi*Vpui_I(nPui)**2*DeltaVpui_I(nPui))
    else
       State_V(PuiFirst_+iPui1-1) = &
            (3*Ppui - RhoPui*Vpui_I(iPui2)**2) &
            /(Vpui_I(iPui1)**2 - Vpui_I(iPui2)**2) &
            /(4*cPi*Vpui_I(iPui1)**2*DeltaVpui_I(iPui1))

       State_V(PuiFirst_+iPui2-1) = &
            (3*Ppui - RhoPui*Vpui_I(iPui1)**2) &
            /(Vpui_I(iPui2)**2 - Vpui_I(iPui1)**2) &
            /(4*cPi*Vpui_I(iPui2)**2*DeltaVpui_I(iPui2))
    endif

    ! Make sure that density and pressure match the new distribution
    State_V(iRho_I(Pu3_)) = &
         4*cPi*sum(State_V(PuiFirst_:PuiLast_)*Vpui_I**2*DeltaVpui_I)
    State_V(iP_I(Pu3_)) = &
         4*cPi/3*sum(State_V(PuiFirst_:PuiLast_)*Vpui_I**4*DeltaVpui_I)

  end subroutine set_pui_state
  !============================================================================
  subroutine pui_advection_diffusion(iBlock)

    ! advection+diffusion in PUI velocity space

    use ModAdvance, ONLY: State_VGB, DtMax_CB
    use BATL_lib, ONLY: Used_GB
    use ModLinearAdvection, ONLY: advance_lin_advection_plus, &
         advance_lin_advection_minus
    use ModMain, ONLY: Cfl

    integer, intent(in) :: iBlock

    real :: Cfl_I(nPui), F_I(0:nPui+1)

    integer :: i, j, k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'pui_advection_diffusion'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Boundary conditions
    F_I(0) = 0.0; F_I(nPui+1) = 0.0

    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if(.not. Used_GB(i,j,k,iBlock)) CYCLE

       Cfl_I = abs(DivUpui_C(i,j,k))/3/DeltaLogVpui*Cfl*DtMax_CB(i,j,k,iBlock)

       F_I(1:nPui) = max(State_VGB(PuiFirst_:PuiLast_,i,j,k,iBlock), 1e-30)

       if(DivUpui_C(i,j,k) > 0.0)then
          F_I(nPui + 1) = F_I(nPui)
          call advance_lin_advection_minus( Cfl_I, nPui, 1, 1, F_I, &
               UseConservativeBC= .true.)
       else
          F_I(0) = F_I(1)
          call advance_lin_advection_plus( Cfl_I, nPui, 1, 1, F_I, &
               UseConservativeBC= .true.)
       end if

       State_VGB(PuiFirst_:PuiLast_,i,j,k,iBlock) = F_I(1:nPui)

    end do; end do; end do

    call test_stop(NameSub, DoTest)
  end subroutine pui_advection_diffusion
  !============================================================================
  subroutine get_pui_flux(iDir, i, j, k, iBlock, &
       StateLeft_V, StateRight_V, Normal_D, &
       PuiDiffCoefOut, FpuiFlux_I, IsNewBlockPuiDiffusion)

    use ModAdvance, ONLY: State_VGB, Erad_
    use ModFaceGradient, ONLY: get_face_gradient
    use ModVarIndexes, ONLY: nVar
    use BATL_size, ONLY: MaxDim

    integer, intent(in):: iDir, i, j, k, iBlock
    real, intent(in):: StateLeft_V(nVar)
    real, intent(in):: StateRight_V(nVar)
    real, intent(in):: Normal_D(MaxDim)
    real, intent(out):: PuiDiffCoefOut, FpuiFlux_I(PuiFirst_:PuiLast_)
    logical, intent(inout):: IsNewBlockPuiDiffusion

    integer :: iPui
    real :: FaceGrad_D(3)
    logical:: IsRegion2, IsRegion3
    logical:: IsNewBlockPuiDiffusion_I(PuiFirst_:PuiLast_)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_pui_flux'
    !--------------------------------------------------------------------------

    if(.not. DoPuiDiffusion_B(iBlock))then
       IsNewBlockPuiDiffusion = .false.
       PuiDiffCoefOut = 0.0
       FpuiFlux_I = 0
       RETURN
    end if

    if(IsNewBlockPuiDiffusion) &
         Fpui_IG = State_VGB(PuiFirst_:PuiLast_,:,:,:,iBlock)
    IsNewBlockPuiDiffusion_I = IsNewBlockPuiDiffusion
    do iPui = PuiFirst_,PuiLast_
       call get_face_gradient(iDir, i, j, k, iBlock, &
            IsNewBlockPuiDiffusion_I(iPui), Fpui_IG(iPui,:,:,:), FaceGrad_D)
       PuiDiffCoefOut = &
            PuiDiffCoef*(Vpui_I(iPui-PuiFirst_+1)/PuiDiffV0)**(PuiDiffSlope)
       FpuiFlux_I(iPui) = -PuiDiffCoefOut*sum(Normal_D*FaceGrad_D)
    end do
    IsNewBlockPuiDiffusion = .false.
  end subroutine get_pui_flux
  !============================================================================

end module ModPUI
!==============================================================================
