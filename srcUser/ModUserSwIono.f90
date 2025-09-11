!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModUser

  use BATL_lib, ONLY: &
       test_start, test_stop
  use ModSize, ONLY: nI,nJ,nK
  use ModUserEmpty,               &
       IMPLEMENTED1 => user_init_session,               &
       IMPLEMENTED2 => user_set_ics,                    &
       IMPLEMENTED3 => user_set_cell_boundary

  include 'user_module.h' ! list of public methods

  character (len=*), parameter :: NameUserFile = "ModUserSwIono.f90"
  character (len=*), parameter :: &
       NameUserModule = 'Magnetosphere 2 Species (SwIono), Hansen, May, 2006'

contains
  !============================================================================
  subroutine user_init_session

    use ModVarIndexes
    use ModPhysics, ONLY: FaceState_VI, CellState_VI, SolarWindRho, BodyRho_I
    use ModMain, ONLY: body1_, xMinBc_, zMaxBc_
    integer :: iBoundary
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_init_session'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! We are using this routine to initialize the arrays that control the
    ! default value for the inner body and hence the boundary condition.
    ! Note these values are typically set in set_physics and they are set to
    ! the BodyRho_I value read from the PARAM.in file.  We want to use
    ! this same strategy for multi-species but have to do it here to avoid
    ! modifying the core source code.

    ! FaceState_VI is used to set the inner boundary condition.  Setting
    ! the correct values here for the extra species will assure that
    ! the inner boundary is done correctly.
    FaceState_VI(rhosw_,body1_) =1e-6*BodyRho_I(1)
    FaceState_VI(rhoion_,body1_)=BodyRho_I(1)

    ! We set the following array for the outer boundaries.  Although
    ! only CellState_VI is used we set both.  Not that these are
    ! used for only some outerboundary cases (fixed, for example) and
    ! are ignored for vary and other types.  We code them as in set_physics
    ! just to be safe.
    do iBoundary = xMinBc_, zMaxBc_
       FaceState_VI(rhosw_, iBoundary)  = SolarWindRho
       FaceState_VI(rhoion_, iBoundary) = 1e-6*SolarWindRho
    end do

    CellState_VI=FaceState_VI(:,xMinBc_:zMaxBc_)
    ! Convert velocity to momentum
    do iBoundary=1,zMaxBc_
       CellState_VI(rhoUx_:rhoUz_,iBoundary) = &
            FaceState_VI(Ux_:Uz_,iBoundary)*FaceState_VI(rho_,iBoundary)
    end do

    call test_stop(NameSub, DoTest)
  end subroutine user_init_session
  !============================================================================

  subroutine user_set_ics(iBlock)

    use ModGeometry, ONLY: r_GB
    use ModAdvance, ONLY: State_VGB, rhoion_, rhosw_
    use ModPhysics, ONLY: BodyRho_I, SolarWindRho, rBody

    integer, intent(in) :: iBlock
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_ics'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    where(r_GB(:,:,:,iBlock)<2.0*Rbody)
       State_VGB(rhoion_,:,:,:,iBlock) = BodyRho_I(1)
       State_VGB(rhosw_,:,:,:,iBlock)  = 1e-6*SolarWindRho
    elsewhere
       State_VGB(rhoion_,:,:,:,iBlock) = 1e-6*BodyRho_I(1)
       State_VGB(rhosw_,:,:,:,iBlock)  = SolarWindRho
    end where

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_ics
  !============================================================================

  subroutine user_set_cell_boundary(iBlock, iSide, TypeBc, found)

    use ModMain, ONLY: tSimulation, IsTimeAccurate
    use ModVarIndexes
    use ModAdvance, ONLY: State_VGB
    use ModPhysics, ONLY: CellState_VI

    integer,      intent(in) :: iBlock, iSide
    character(len=*), intent(in)  :: TypeBc
    logical,      intent(out):: found

    real :: time_now
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_cell_boundary'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    time_now = tSimulation

    if(TypeBc=='vary' .and. IsTimeAccurate)then
       call BC_solar_wind(time_now)
    else
       call BC_fixed(1,nVar,CellState_VI(:,iSide))
       call BC_fixed_B
    end if

    ! Note that the above code does not set the extra density species (vary)
    ! or sets them to undefined values (fixed).
    !
    ! The solar wind species is the only one at the upstream boundary.
    ! The ionosphere species is zero.
    State_VGB(rhosw_,:,:,:,iBlock)   = State_VGB(rho_,:,:,:,iBlock)
    State_VGB(rhoion_,:,:,:,iBlock)  = 1e-6*State_VGB(rho_,:,:,:,iBlock)

    found = .true.

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_cell_boundary
  !============================================================================

end module ModUser
!==============================================================================

