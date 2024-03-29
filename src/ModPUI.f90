!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModPUI

  use BATL_lib, ONLY: test_start, test_stop
  use ModVarIndexes, ONLY: nPui, PuiFirst_, PuiLast_
  use ModBatsrusUtility, ONLY: stop_mpi
  implicit none
  save

  private ! except

  public :: read_pui_param
  public :: init_mod_pui
  public :: pui_advection_diffusion
  public :: add_pui_source

  logical, parameter, public :: UsePui = PuiFirst_ > 1

  real :: VpuiMinSi = 10.0, VpuiMin
  real :: VpuiMaxSi = 6000.0, VpuiMax

  ! Logarithmic velocity grid for the PUIs.
  ! The bin centered velocities are:
  real :: Vpui_I(PuiFirst_:PuiLast_)

  real :: DeltaLogVpui

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

    use ModWaves,   ONLY: WaveFirst_
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
    Vpui_I(PuiFirst_) = VpuiMinSi*exp(0.5*DeltaLogVpui)

    do iPui = PuiFirst_ + 1, PuiLast_
       Vpui_I(iPui) = Vpui_I(iPui-1)*exp(DeltaLogVpui)
    end do

    if(UsePui .and. WaveFirst_==1) &
         call stop_mpi(NameSub//": PUI can only run together with turbulence")

    call test_stop(NameSub, DoTest)
  end subroutine init_mod_pui
  !============================================================================
  subroutine pui_advection_diffusion(iBlock)

    ! advection+diffusion in PUI velocity space

    integer, intent(in) :: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'pui_advection_diffusion'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    call test_stop(NameSub, DoTest)
  end subroutine pui_advection_diffusion
  !============================================================================
  subroutine add_pui_source(iBlock)

    ! PUI source terms due to charge exchange and photoionization

    integer, intent(in) :: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'add_pui_source'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    call test_stop(NameSub, DoTest)
  end subroutine add_pui_source
  !============================================================================
end module ModPUI
!==============================================================================
