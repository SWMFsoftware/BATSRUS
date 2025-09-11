!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
! Note that for some reason that I don't understand yet, this user file
! will only work with the normalization type set to "solarwind".  You
! should set this in the PARAM.in file.

module ModUser

  use BATL_lib, ONLY: &
       test_start, test_stop, iProc

  use ModSize, ONLY: nI,nJ,nK,MaxBlock
  use ModVarIndexes, ONLY: nVar
  use ModMain, ONLY:  nStep
  use ModUserEmpty,               &
       IMPLEMENTED1 => user_init_session,               &
       IMPLEMENTED2 => user_set_ics,                    &
       IMPLEMENTED3 => user_set_cell_boundary,          &
       IMPLEMENTED4 => user_set_plot_var,               &
       IMPLEMENTED5 => user_update_states,              &
       IMPLEMENTED6 => user_action

  include 'user_module.h' ! list of public methods

  ! summed MHD quantities
  integer, parameter, public :: MaxSumMhdVar=nVar+2 !(8+2+2)
  integer, parameter, public :: Umag_=nVar+1
  integer, parameter, public :: Tsw_ =nVar+2
  real :: tSumStart, tSumEnd
  real :: StateSum_VC(MaxSumMhdVar,nI, nJ, nK)

  logical, allocatable :: IsRestartSum(:)

  character (len=*), parameter :: NameUserFile = "ModUserEarthXray.f90"
  character (len=*), parameter :: &
       NameUserModule = 'Earth Mag X-ray (EarthXray), Hansen, Jan, 2008'

contains
  !============================================================================
  subroutine user_action(NameAction)
    character(len=*), intent(in):: NameAction

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_action'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(iProc==0)write(*,*) NameSub,' called with action ',NameAction
    select case(NameAction)
    case('initialize module')
      if(.not.allocated(IsRestartSum)) &
         allocate(IsRestartSum(MaxBlock))
    case('clean module')
      if(allocated(IsRestartSum)) &
         deallocate(IsRestartSum)
    end select
    call test_stop(NameSub, DoTest)
  end subroutine user_action
  !============================================================================

  subroutine user_init_session

    use ModVarIndexes
    use ModPhysics, ONLY: FaceState_VI, CellState_VI, SolarWindRho, BodyRho_I
    use ModMain, ONLY: body1_
    use BATL_size, ONLY: nIJK,nDim
    use ModBlockData, ONLY: MaxBlockData
    integer :: iBoundary

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_init_session'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Maximum numer of reals stored as extra block data
    MaxBlockData = MaxSumMhdVar*nIJK

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
    do iBoundary=1,2*nDim
       FaceState_VI(rhosw_, iBoundary)  = SolarWindRho
       FaceState_VI(rhoion_, iBoundary) = 1e-6*SolarWindRho
    end do

    CellState_VI = FaceState_VI(:,xMinBc_:zMaxBc_)
    ! Convert velocity to momentum
    do iBoundary=1,2*nDim
       CellState_VI(rhoUx_:rhoUz_,iBoundary) = &
            FaceState_VI(Ux_:Uz_,iBoundary)*FaceState_VI(rho_,iBoundary)
    end do

    call test_stop(NameSub, DoTest)
  end subroutine user_init_session
  !============================================================================

  subroutine user_set_ics(iBlock)

    use ModMain, ONLY: tSimulation, dt
    use ModGeometry, ONLY: r_GB
    use ModAdvance, ONLY: State_VGB, rhoion_, rhosw_
    use ModPhysics, ONLY: BodyRho_I, SolarWindRho, rBody
    use ModBlockData, ONLY: put_block_data

    integer, intent(in) :: iBlock

    integer :: iBlockLast = -1
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

!
!    ! Initiallize the arrays that contain the time average of the State
!    ! variables with the initial conditions of the cells
!
!    tSumStart = tSimulation
!    StateSum_VC = 0.0
!    if(iBlock /= iBlockLast)then
!       iBlockLast = iBlock
!       call put_block_data(iBlock, MaxSumMhdVar, nI, nJ, nK, StateSum_VC)
!    end if
!
    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_ics
  !============================================================================

  subroutine user_set_cell_boundary(iBlock, iSide, TypeBc, found)

    use ModMain, ONLY: tSimulation, IsTimeAccurate
    use ModVarIndexes
    use ModAdvance, ONLY: State_VGB
    use ModPhysics, ONLY: CellState_VI

    integer,      intent(in) ::iBlock, iSide
    character(len=*), intent(in)  :: TypeBc
    logical,      intent(out):: found

    real :: time_now

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_cell_boundary'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    time_now = tSimulation

    if(TypeBc=='uservary' .and. IsTimeAccurate)then
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

  subroutine user_update_states(iBlock)

    use ModUpdateState, ONLY: update_state_normal
    use ModVarIndexes
    use ModSize
    use ModAdvance, ONLY: State_VGB
    use ModMain, ONLY: nStage, tSimulation, dt
    use ModBlockData, ONLY: get_block_data, put_block_data, use_block_data

    integer,intent(in):: iBlock
    integer :: iBlockLast = -1

    ! do the normal update states
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_update_states'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    call update_state_normal(iBlock)

    ! Now compute the sum of the state variables (we will divide by the number of
    ! time steps later to get the average state value
    tSumEnd = tSimulation
    if(iBlock /= iBlockLast)then
       iBlockLast = iBlock
       if(use_block_data(iBlock))then
          call get_block_data(iBlock, MaxSumMhdVar, nI, nJ, nK, &
               StateSum_VC, DoNotAdvance=.true.)

          ! reset the block data for summing over the next period if it was just printed
          if (IsRestartSum(iBlock)) then
             IsRestartSum(iBlock) = .false.
             StateSum_VC = 0.0
             tSumStart = tSimulation
             tSumEnd = tSumStart
          else
             StateSum_VC(rho_,:,:,:) = StateSum_VC(rho_,:,:,:) + &
                  State_VGB(rho_,1:nI,1:nJ,1:nK,iBlock)*dt
             StateSum_VC(rhosw_,:,:,:) = StateSum_VC(rhosw_,:,:,:) + &
                  State_VGB(rhosw_,1:nI,1:nJ,1:nK,iBlock)*dt
             StateSum_VC(rhoion_,:,:,:) = StateSum_VC(rhoion_,:,:,:) + &
                  State_VGB(rhoion_,1:nI,1:nJ,1:nK,iBlock)*dt
             StateSum_VC(Bx_,:,:,:) = StateSum_VC(Bx_,:,:,:) + &
                  State_VGB(Bx_,1:nI,1:nJ,1:nK,iBlock)*dt
             StateSum_VC(By_,:,:,:) = StateSum_VC(By_,:,:,:) + &
                  State_VGB(By_,1:nI,1:nJ,1:nK,iBlock)*dt
             StateSum_VC(Bz_,:,:,:) = StateSum_VC(Bz_,:,:,:) + &
                  State_VGB(Bz_,1:nI,1:nJ,1:nK,iBlock)*dt
             StateSum_VC(p_,:,:,:) = StateSum_VC(p_,:,:,:) + &
                  State_VGB(p_,1:nI,1:nJ,1:nK,iBlock)*dt
             StateSum_VC(Ux_,:,:,:) = StateSum_VC(Ux_,:,:,:) + &
                  State_VGB(rhoux_,1:nI,1:nJ,1:nK,iBlock) /   &
                  State_VGB(rho_,1:nI,1:nJ,1:nK,iBlock)*dt
             StateSum_VC(Uy_,:,:,:) = StateSum_VC(Uy_,:,:,:) + &
                  State_VGB(rhouy_,1:nI,1:nJ,1:nK,iBlock) /   &
                  State_VGB(rho_,1:nI,1:nJ,1:nK,iBlock)*dt
             StateSum_VC(Uz_,:,:,:) = StateSum_VC(Uz_,:,:,:) + &
                  State_VGB(rhouz_,1:nI,1:nJ,1:nK,iBlock) /   &
                  State_VGB(rho_,1:nI,1:nJ,1:nK,iBlock)*dt
             StateSum_VC(Umag_,:,:,:) = StateSum_VC(Umag_,:,:,:) + &
                  sqrt(State_VGB(RhoUx_,1:nI,1:nJ,1:nK,iBlock)**2 + &
                       State_VGB(RhoUy_,1:nI,1:nJ,1:nK,iBlock)**2 + &
                       State_VGB(RhoUz_,1:nI,1:nJ,1:nK,iBlock)**2 )/ &
                  State_VGB(rho_,1:nI,1:nJ,1:nK,iBlock)*dt
             ! Note that the temperature of the solar wind plasma is only
             ! going to work here for cells were the ionospheric plasma is
             ! small since we are using the full pressure and the partial
             ! density.
             StateSum_VC(Tsw_,:,:,:) = StateSum_VC(Tsw_,:,:,:) + &
                  State_VGB(p_,1:nI,1:nJ,1:nK,iBlock)/           &
                  State_VGB(rhosw_,1:nI,1:nJ,1:nK,iBlock)*dt

          end if

          call put_block_data(iBlock, MaxSumMhdVar, nI, nJ, nK, &
               StateSum_VC, DoAllowReplace=.true.)

       else

          IsRestartSum(iBlock) = .false.
          tSumStart = tSimulation
          StateSum_VC = 0.0
          call put_block_data(iBlock, MaxSumMhdVar, nI, nJ, nK, StateSum_VC)

       end if
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_update_states
  !============================================================================

  subroutine user_set_plot_var(iBlock, NameVar, IsDimensional, &
       PlotVar_G, PlotVarBody, UsePlotVarBody, &
       NameTecVar, NameTecUnit, NameIdlUnit, IsFound)

    use ModPhysics
    use ModMain, ONLY: Body1_, tSimulation,x_,y_,z_
    use ModAdvance
    use ModGeometry, ONLY: Xyz_DGB, r_GB
    use ModBlockData, ONLY: get_block_data, put_block_data, use_block_data

    integer,          intent(in) :: iBlock
    character(len=*), intent(in) :: NameVar
    logical,          intent(in) :: IsDimensional
    real,             intent(inout):: PlotVar_G(MinI:MaxI, MinJ:MaxJ, MinK:MaxK)
    real,             intent(out):: PlotVarBody
    logical,          intent(out):: UsePlotVarBody
    character(len=*), intent(out):: NameTecVar
    character(len=*), intent(out):: NameTecUnit
    character(len=*), intent(out):: NameIdlUnit
    logical,          intent(out):: IsFound

    integer :: iUnitVar
    integer :: iBlockLast = -1

    character (len=*), parameter :: Name='user_set_plot_var'

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_plot_var'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! If we are in this routine then reset the logical that tell the sum to start over

    IsRestartSum = .true.

    ! Get the averaged data from the Block storage arrays if this block has not been read yet
    ! if it has just use the already saved StateSum_VC which should have the right info in it
    if(iBlock /= iBlockLast) then
       iBlockLast = iBlock
       if(use_block_data(iBlock)) &
          call get_block_data(iBlock, MaxSumMhdVar, nI, nJ, nK, StateSum_VC)
    end if

    ! Now load the plot arrays
    if(use_block_data(iBlock))then

       isFound = .true.

       select case(NameVar)
       case('rhoave')
          NameTecVar = '<`r>'
          iUnitVar   = UnitRho_
          PlotVar_G(1:nI,1:nJ,1:nK)  = StateSum_VC(rho_,:,:,:)

       case('rhoswave')
          NameTecVar = '<`rsw>'
          iUnitVar   = UnitRho_
          PlotVar_G(1:nI,1:nJ,1:nK)  = StateSum_VC(rhosw_,:,:,:)
       case('rhoionave')
          NameTecVar = '<`rion>'
          iUnitVar   = UnitRho_
          PlotVar_G(1:nI,1:nJ,1:nK)  = StateSum_VC(rhoion_,:,:,:)
       case('bxave')
          NameTecVar = '<B_x>'
          iUnitVar   = UnitB_
          ! Note: here we add B0x to the summed B1.  We have to multiply by the time because the
          ! summed variables have been multiplied by dt in the summation.
          PlotVar_G(1:nI,1:nJ,1:nK)  = StateSum_VC(Bx_,:,:,:)+ &
               B0_DGB(x_,1:nI,1:nJ,1:nK,iBlock)*(tSumEnd-tSumStart)
       case('byave')
          NameTecVar = '<B_y>'
          iUnitVar   = UnitB_
          ! Note: here we add B0y to the summed B1.  We have to multiply by the time because the
          ! summed variables have been multiplied by dt in the summation.
          PlotVar_G(1:nI,1:nJ,1:nK)  = StateSum_VC(By_,:,:,:)+ &
               B0_DGB(y_,1:nI,1:nJ,1:nK,iBlock)*(tSumEnd-tSumStart)
       case('bzave')
          NameTecVar = '<B_z>'
          iUnitVar   = UnitB_
          ! Note: here we add B0z to the summed B1.  We have to multiply by the time because the
          ! summed variables have been multiplied by dt in the summation.
          PlotVar_G(1:nI,1:nJ,1:nK)  = StateSum_VC(Bz_,:,:,:)+ &
               B0_DGB(z_,1:nI,1:nJ,1:nK,iBlock)*(tSumEnd-tSumStart)
       case('uxave')
          NameTecVar = '<U_x>'
          iUnitVar   = UnitU_
          PlotVar_G(1:nI,1:nJ,1:nK)  = StateSum_VC(RhoUx_,:,:,:)
       case('uyave')
          NameTecVar = '<U_y>'
          iUnitVar   = UnitU_
          PlotVar_G(1:nI,1:nJ,1:nK)  = StateSum_VC(RhoUy_,:,:,:)
       case('uzave')
          NameTecVar = '<U_z>'
          iUnitVar   = UnitU_
          PlotVar_G(1:nI,1:nJ,1:nK)  = StateSum_VC(RhoUz_,:,:,:)
       case('uave')
          NameTecVar = '<|U|>'
          iUnitVar   = UnitU_
          PlotVar_G(1:nI,1:nJ,1:nK)  = StateSum_VC(Umag_,:,:,:)
       case('pave','pthave')
          NameTecVar = '<p>'
          iUnitVar   = UnitP_
          PlotVar_G(1:nI,1:nJ,1:nK)  = StateSum_VC(p_,:,:,:)
       case('tswave')
          NameTecVar = '<Tsw>'
          iUnitVar   = UnitTemperature_
          PlotVar_G(1:nI,1:nJ,1:nK)  = StateSum_VC(Tsw_,:,:,:)
       case default
          IsFound = .false.
          call stop_mpi(Name//': unimplemented variable='//NameVar)
       end select

       UsePlotVarBody = .true.
       PlotVarBody    = 0.0

       ! The Sum variables store the summation over time.  We want averages so divide by
       ! the elapsed time.
       PlotVar_G = PlotVar_G/((tSumEnd-tSumStart)*Io2No_V(UnitT_))

       if(IsDimensional) PlotVar_G = PlotVar_G*No2Io_V(iUnitVar)

    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_plot_var
  !============================================================================

end module ModUser
!==============================================================================

