!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModSetInitialCondition

  use BATL_lib, ONLY: &
       test_start, test_stop, iBlockTest, iTest, jTest, kTest

  use ModVarIndexes, ONLY: nVar
  use ModMain, ONLY: NamePrimitive_V
  use ModPhysics, ONLY: UseShocktube, ShockLeftState_V, ShockRightState_V, &
       ShockPosition, ShockSlope
  use ModBatsrusUtility, ONLY: stop_mpi
  
  implicit none

  private ! except

  public:: read_initial_cond_param ! read parameters for initial condition
  public:: set_initial_condition   ! set initial condition for one block
  public:: add_rotational_velocity ! transform between rotating/inertial frames

contains
  !============================================================================
  subroutine read_initial_cond_param(NameCommand)

    use ModReadParam, ONLY: read_var

    character(len=*), intent(in):: NameCommand

    integer:: iVar
    
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_face_flux_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    select case(NameCommand)
    case("#UNIFORMSTATE")
       UseShockTube = .true.
       do iVar = 1, nVar
          call read_var(NamePrimitive_V(iVar), ShockLeftState_V(iVar))
       end do
       ShockRightState_V = ShockLeftState_V

    case("#SHOCKTUBE")
       UseShockTube = .true.
       do iVar = 1, nVar
          call read_var(NamePrimitive_V(iVar)//' left', ShockLeftState_V(iVar))
       end do
       do iVar = 1, nVar
          call read_var(NamePrimitive_V(iVar)//' right', &
               ShockRightState_V(iVar))
       end do
       
    case("#SHOCKPOSITION")
       call read_var('ShockPosition', ShockPosition)
       call read_var('ShockSlope', ShockSlope)

    case("#WAVE")
    case("#GAUSSIAN", "#TOPHAT")
    case default
       call stop_mpi(NameSub//': unknown command='//NameCommand)
    end select

    call test_stop(NameSub, DoTest)
  end subroutine read_initial_cond_param
  !============================================================================
  subroutine set_initial_condition(iBlock)

    use ModMain
    use ModAdvance
    use ModB0, ONLY: B0_DGB, set_b0_cell, subtract_b0
    use ModGeometry, ONLY: Used_GB
    use ModIO, ONLY : IsRestart
    use ModPhysics, ONLY: FaceState_VI, CellState_VI, ShockSlope, &
         UseShockTube, UnitUser_V, ShockLeftState_V, ShockRightState_V, &
         ShockPosition, UnitU_, Io2No_V
    use ModUserInterface ! user_set_ics
    use ModChGL,          ONLY: UseChGL, init_chgl
    use ModConstrainDivB, ONLY: constrain_ics
    use ModMultiFluid
    use ModRestartFile, ONLY: UseRestartWithFullB
    use ModBoundaryGeometry, ONLY: iBoundary_GB
    use BATL_lib, ONLY: Xyz_DGB

    integer, intent(in) :: iBlock

    real   :: SinSlope, CosSlope, Rot_II(2,2)
    real   :: ShockLeft_V(nVar), ShockRight_V(nVar)
    integer:: i, j, k, iVar, iBoundary, iFluid, iGang

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_initial_condition'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    iGang = 1
#ifdef _OPENACC
    iGang = iBlock
#endif

    DtMax_CB(:,:,:,iBlock) = 0.0

    Flux_VXI(:,:,:,:,iGang) = 0.0
    Flux_VYI(:,:,:,:,iGang) = 0.0
    Flux_VZI(:,:,:,:,iGang) = 0.0

    if(Unused_B(iBlock))then
       do iVar = 1, nVar
          State_VGB(iVar,:,:,:,iBlock) = DefaultState_V(iVar)
       end do
    else
       ! If used, initialize solution variables and parameters.
       if(UseB0) call set_b0_cell(iBlock)

       ! Subtract B0 from Full B0+B1 from restart to obtain B1
       if(UseB0 .and. IsRestart .and. UseRestartWithFullB) &
            call subtract_b0(iBlock)

       if(.not.IsRestart)then

          if(UseShockTube)then
             ! Calculate sin and cos from the tangent = ShockSlope
             SinSlope=ShockSlope/sqrt(1.0 + ShockSlope**2)
             CosSlope=       1.0/sqrt(1.0 + ShockSlope**2)
             ! Set rotational matrix
             Rot_II = reshape([CosSlope, SinSlope, -SinSlope, CosSlope],[2,2])
             ! calculate normalized left and right states
             ShockLeft_V  = ShockLeftState_V /UnitUser_V(1:nVar)
             ShockRight_V = ShockRightState_V/UnitUser_V(1:nVar)

             ! fix the units for the velocities
             do iFluid = 1, nFluid
                if(nFluid > 1) call select_fluid(iFluid)
                ShockLeft_V(iUx:iUz) = ShockLeftState_V(iUx:iUz) *&
                     Io2No_V(UnitU_)
                ShockRight_V(iUx:iUz)= ShockRightState_V(iUx:iUz)*&
                     Io2No_V(UnitU_)
             end do

          end if  ! UseShockTube

          ! Loop through all the cells
          do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
             if(.not.Used_GB(i,j,k,iBlock))then
                iBoundary = iBoundary_GB(i,j,k,iBlock)

                State_VGB(1:nVar,i,j,k,iBlock) = FaceState_VI(1:nVar,iBoundary)

                ! Convert velocity to momentum
                do iFluid = 1, nFluid
                   if(nFluid > 1) call select_fluid(iFluid)
                   State_VGB(iRhoUx,i,j,k,iBlock) = &
                        FaceState_VI(iUx,iBoundary) &
                        *FaceState_VI(iRho,iBoundary)
                   State_VGB(iRhoUy,i,j,k,iBlock) = &
                        FaceState_VI(iUy,iBoundary) &
                        *FaceState_VI(iRho,iBoundary)
                   State_VGB(iRhoUz,i,j,k,iBlock) = &
                        FaceState_VI(iUz,iBoundary) &
                        *FaceState_VI(iRho,iBoundary)
                end do

             elseif(.not.UseShockTube)then
                State_VGB(:,i,j,k,iBlock) = CellState_VI(:,Coord1MaxBc_)
             else
                if( (Xyz_DGB(x_,i,j,k,iBlock)-ShockPosition) &
                     < -ShockSlope*Xyz_DGB(y_,i,j,k,iBlock))then
                   ! Set all variables first
                   State_VGB(:,i,j,k,iBlock) = ShockLeft_V

                   ! Rotate vector variables
                   do iFluid = 1, nFluid
                      if(nFluid > 1) call select_fluid(iFluid)
                      State_VGB(iUx:iUy,i,j,k,iBlock) = &
                           matmul(Rot_II,ShockLeft_V(iUx:iUy))
                   end do
                   if(UseB) State_VGB(Bx_:By_,i,j,k,iBlock) = &
                        matmul(Rot_II,ShockLeft_V(Bx_:By_))
                else
                   ! Set all variables first
                   State_VGB(:,i,j,k,iBlock) = ShockRight_V
                   ! Set vector variables
                   do iFluid = 1, nFluid
                      if(nFluid > 1) call select_fluid(iFluid)
                      State_VGB(iUx:iUy,i,j,k,iBlock) = &
                           matmul(Rot_II,ShockRight_V(iUx:iUy))
                   end do
                   if(UseB) State_VGB(Bx_:By_,i,j,k,iBlock) = &
                        matmul(Rot_II,ShockRight_V(Bx_:By_))
                end if
                ! Convert velocity to momentum
                do iFluid = 1, nFluid
                   if(nFluid > 1) call select_fluid(iFluid)
                   State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock) = &
                        State_VGB(iRho,i,j,k,iBlock) * &
                        State_VGB(iUx:iUz,i,j,k,iBlock)
                end do
                if(.not.UseB0)CYCLE
                ! Remove B0 from B (if any)
                State_VGB(Bx_:Bz_,i,j,k,iBlock) = &
                     State_VGB(Bx_:Bz_,i,j,k,iBlock) - B0_DGB(:,i,j,k,iBlock)
             end if ! UseShockTube

          end do; end do; end do

          if(UseConstrainB) call constrain_ics(iBlock)
          if(UseUserICs) call user_set_ics(iBlock)

          if(iSignRotationIC /= 0) &
               call add_rotational_velocity(iSignRotationIC, iBlock)

          if(UseChGL)call init_chgl(iBlock)
       end if ! not IsRestart

    end if ! Unused_B

    if(DoTest)write(*,*) &
         NameSub, 'State(test)=',State_VGB(:,iTest,jTest,kTest,iBlockTest)

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine set_initial_condition
  !============================================================================

  subroutine add_rotational_velocity(iSign, iBlockIn)

    use ModSize,     ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK, nBlock, x_, y_
    use ModMain,     ONLY: Unused_B, NameThisComp
    use ModAdvance,  ONLY: State_VGB
    use ModGeometry, ONLY: Used_GB
    use ModPhysics,  ONLY: OmegaBody
    use ModMultiFluid, ONLY: iRho_I, iRhoUx_I, iRhoUy_I
    use BATL_lib,    ONLY: Xyz_DGB, iProc

    integer, intent(in):: iSign
    integer, optional, intent(in):: iBlockIn

    ! Transform velocities between inertial and rotating frames
    ! where Omega is the angular velocity of the rotating frame
    ! Since Omega = (0,0,OmegaBody)
    ! ux = ux - iSign*OmegaBody*y
    ! uy = uy + iSign*OmegaBody*x
    ! iSign=+1: from rotating to inertial frame
    ! iSign=-1: from inertial to rotating frame
    !
    ! If iBlockIn is present, do that block, otherwise do all blocks.

    integer :: i, j, k, iBlock, iBlockFirst, iBlockLast
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'add_rotational_velocity'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(present(iBlockIn))then
       iBlockFirst = iBlockIn; iBlockLast = iBlockIn
    else
       iBlockFirst = 1; iBlockLast = nBlock
       if(iProc==0)write(*,'(a)')&
            NameThisComp//': add rotational velocity to convert coords'
    end if

    do iBlock = iBlockFirst, iBlockLast
       if(Unused_B(iBlock))CYCLE
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE
          State_VGB(iRhoUx_I,i,j,k,iBlock) = State_VGB(iRhoUx_I,i,j,k,iBlock) &
               - iSign*State_VGB(iRho_I,i,j,k,iBlock) &
	       *OmegaBody*Xyz_DGB(y_,i,j,k,iBlock)

          State_VGB(iRhoUy_I,i,j,k,iBlock) = State_VGB(iRhoUy_I,i,j,k,iBlock) &
	       + iSign*State_VGB(iRho_I,i,j,k,iBlock) &
	       *OmegaBody*Xyz_DGB(x_,i,j,k,iBlock)

       end do; end do; end do
    end do

    call test_stop(NameSub, DoTest)
  end subroutine add_rotational_velocity
  !============================================================================

end module ModSetInitialCondition
!==============================================================================
