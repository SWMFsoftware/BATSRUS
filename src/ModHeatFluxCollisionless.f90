!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModHeatFluxCollisionless

  use BATL_lib,      ONLY: test_start, test_stop
  use ModBatsrusUtility, ONLY: stop_mpi
#ifdef _OPENACC
  use ModUtilities, ONLY: norm2
#endif

  implicit none
  save

  private ! except

  ! Public methods
  public :: read_heatflux_param
  public :: get_gamma_collisionless
  public :: update_heatflux_collisionless

  ! Parameters for heat flux region
  logical, public :: UseHeatFluxRegion = .false.
  real, public :: rCollisional = 5.0, rCollisionless = -8.0
  !$acc declare create(UseHeatFluxRegion, rCollisional, rCollisionless)

  ! Parameters for collisionless heat conduction
  logical, public :: UseHeatFluxCollisionless = .false.
  real :: CollisionlessAlpha = 1.05
  !$acc declare create(UseHeatFluxCollisionless, CollisionlessAlpha)

contains
  !============================================================================

  subroutine read_heatflux_param(NameCommand)

    use ModReadParam, ONLY: read_var

    character(len=*), intent(in) :: NameCommand

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_heatflux_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    select case(NameCommand)

    case("#HEATFLUXREGION")
       call read_var('UseHeatFluxRegion', UseHeatFluxRegion)
       if(UseHeatFluxRegion)then
          call read_var('rCollisional', rCollisional)
          call read_var('rCollisionless', rCollisionless)
       end if
       !$acc update device(UseHeatFluxRegion, rCollisional, rCollisionless)

    case("#HEATFLUXCOLLISIONLESS")
       call read_var('UseHeatFluxCollisionless', UseHeatFluxCollisionless)
       if(UseHeatFluxCollisionless)then
          call read_var('CollisionlessAlpha', CollisionlessAlpha)
       endif
       !$acc update device(UseHeatFluxCollisionless, CollisionlessAlpha)

    case default
       call stop_mpi(NameSub//' invalid NameCommand='//NameCommand)
    end select

    call test_stop(NameSub, DoTest)
  end subroutine read_heatflux_param
  !============================================================================

  subroutine update_heatflux_collisionless(iBlock)

    use BATL_lib,      ONLY: Xyz_DGB
    use ModVarIndexes, ONLY: Pe_, P_, Ehot_
    use ModSize
    use ModAdvance,    ONLY: State_VGB, UseElectronPressure
    use ModPhysics,    ONLY: InvGammaElectronMinus1
    use ModEnergy,     ONLY: limit_pressure

    integer, intent(in) :: iBlock

    integer:: i, j, k, iP
    real:: GammaHere

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'update_heatflux_collisionless'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! We use a varying gamma for the electrons to parameterize the
    ! collisionless heat flux of Hollweg (1976).

    iP = p_
    if(UseElectronPressure) iP = Pe_

    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       call get_gamma_collisionless(Xyz_DGB(:,i,j,k,iBlock), GammaHere)

       State_VGB(iP,i,j,k,iBlock) = (GammaHere - 1) &
            *(InvGammaElectronMinus1*State_VGB(iP,i,j,k,iBlock) &
            + State_VGB(Ehot_,i,j,k,iBlock))
       State_VGB(Ehot_,i,j,k,iBlock) = State_VGB(iP,i,j,k,iBlock) &
            *(1.0/(GammaHere - 1) - InvGammaElectronMinus1)
    end do; end do; end do

    ! write(*,*)NameSub,' !!! call limit_pressure'
    call limit_pressure(1, nI, 1, nJ, 1, nK, iBlock, 1, 1)

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine update_heatflux_collisionless
  !============================================================================

  subroutine get_gamma_collisionless(x_D, GammaOut)
    !$acc routine seq
    use BATL_lib,   ONLY: MaxDim
    use ModPhysics, ONLY: GammaElectron, InvGammaElectronMinus1
    use ModAdvance, ONLY: UseElectronPressure
    use ModMain,    ONLY: UseHeatConduction

    real, intent(in) :: x_D(MaxDim)
    real, intent(out) :: GammaOut

    real :: r
    real :: GammaCollisionless

    character(len=*), parameter:: NameSub = 'get_gamma_collisionless'
    !--------------------------------------------------------------------------
    r = norm2(x_D)

    if(UseElectronPressure)then
       GammaCollisionless = &
            (InvGammaElectronMinus1*GammaElectron + 1.5*CollisionlessAlpha) &
            /(InvGammaElectronMinus1 + 1.5*CollisionlessAlpha)
    else
       GammaCollisionless = &
            (InvGammaElectronMinus1*GammaElectron + 0.75*CollisionlessAlpha) &
            /(InvGammaElectronMinus1 + 0.75*CollisionlessAlpha)
    end if

    if(.not.UseHeatConduction)then
       GammaOut = GammaCollisionless
    elseif(rCollisionless < 0.0)then
       GammaOut = GammaCollisionless &
            + (GammaElectron-GammaCollisionless)/((r/rCollisional)**2 + 1)
    elseif(r <= rCollisional)then
       GammaOut = GammaElectron
    else
       GammaOut = GammaCollisionless + (GammaElectron-GammaCollisionless)* &
            exp(-((r-rCollisional)/(rCollisionless-rCollisional))**2)
    end if

  end subroutine get_gamma_collisionless
  !============================================================================

end module ModHeatFluxCollisionless
!==============================================================================
