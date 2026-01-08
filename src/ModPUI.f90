!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModPUI

  use BATL_lib, ONLY: test_start, test_stop
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

  integer, parameter, public :: Pu3_ = nIonFluid

  real :: VpuiMinSi = 1e4, VpuiMin
  real :: VpuiMaxSi = 6e6, VpuiMax

  ! Logarithmic velocity grid for the PUIs.
  ! The bin centered velocities are:
  real, public :: Vpui_I(nPui)
  real, public :: DeltaVpui_I(nPui)

  real, public :: DeltaLogVpui

  real, public :: DivUpui_C(nI,nJ,nK) = 0.0

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
    case default
       call stop_mpi(NameSub//": unknown command="//trim(NameCommand))
    end select

    call test_stop(NameSub, DoTest)
  end subroutine read_pui_param
  !============================================================================
  subroutine init_mod_pui

    use ModWaves, ONLY: WaveFirst_
    use ModPhysics, ONLY: Si2No_V, UnitU_

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
end module ModPUI
!==============================================================================
