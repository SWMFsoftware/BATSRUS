!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModReverseField

  ! Reverse direction of magnetic field where it helps suppressing
  ! numerical reconnection

  use BATL_lib, ONLY: test_start, test_stop, nDim, x_, y_, &
       nI, nJ, nK, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, MaxBlock, &
       Xyz_DGB
  use ModNumConst, ONLY: cDegToRad
  use ModAdvance, ONLY: State_VGB, StateOld_VGB
  use ModB0, ONLY: UseB0, B0_DGB
  use ModGeometry, ONLY: rMin_B
  use ModMain, ONLY: UseRotatingFrame
  use ModPhysics, ONLY: OmegaBody
  use ModVarIndexes, ONLY: Rho_, RhoUx_, RhoUz_, Bx_, Bz_, SignB_, &
       WaveFirst_, WaveLast_
  use ModWaves, ONLY: UseAlfvenWaves

  implicit none

  SAVE

  private ! except

  public:: init_mod_reverse_field    ! initialize module
  public:: clean_mod_reverse_field   ! clean module
  public:: read_reverse_field_param  ! read parameters
  public:: set_sign_field            ! set the SignB variable in a block
  public:: do_reverse_block          ! check if field should be reversed
  public:: reverse_field             ! reverse magnetic field in a block

  logical, public:: DoReverseField = .false.         ! Use the scheme
  logical, allocatable, public:: DoReverseField_B(:) ! Use the scheme per block

  ! Local variables
  real:: rMin = -1.0, CosAngleMin = -1.0

contains
  !============================================================================
  subroutine read_reverse_field_param

    use ModReadParam, ONLY: read_var

    real:: AngleMax
    !--------------------------------------------------------------------------
    call read_var('DoReverseField', DoReverseField)
    call read_var('rMinReverse', rMin)
    call read_var('AngleMaxReverse', AngleMax)
    CosAngleMin = abs(cos(AngleMax*cDegToRad))

  end subroutine read_reverse_field_param
  !============================================================================
  subroutine init_mod_reverse_field

    !--------------------------------------------------------------------------
    if(.not.allocated(DoReverseField_B)) allocate(DoReverseField_B(MaxBlock))

  end subroutine init_mod_reverse_field
  !============================================================================
  subroutine clean_mod_reverse_field

    !--------------------------------------------------------------------------
    if(allocated(DoReverseField_B)) deallocate(DoReverseField_B)

  end subroutine clean_mod_reverse_field
  !============================================================================
  subroutine set_sign_field(iBlock)
    integer, intent(in):: iBlock

    ! Set SignB for block iBlock based on the radial component of full B

    integer:: i, j, k
    real:: FullB_D(3)
    !--------------------------------------------------------------------------
    do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
       FullB_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
       if(UseB0) FullB_D = FullB_D + B0_DGB(:,i,j,k,iBlock)
       if(nDim < 3)then
          ! Use sign of By for test purposes
          State_VGB(SignB_,i,j,k,iBlock) = sign(1.0, FullB_D(2))
       elseif(sum(FullB_D*Xyz_DGB(:,i,j,k,iBlock)) > 0)then
          ! Br > 0
          State_VGB(SignB_,i,j,k,iBlock) = 1.0
       else
          ! Br < 0
          State_VGB(SignB_,i,j,k,iBlock) = -1.0
       end if
    end do; end do; end do

  end subroutine set_sign_field
  !============================================================================
  logical function do_reverse_block(iBlock)
    use BATL_lib, ONLY: Used_GB
    ! Return true is magnetic field in block iBlock should be reversed

    integer, intent(in):: iBlock

    integer:: i, j, k
    real:: u_D(3), b_D(3), CosAngle

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'do_reverse_block'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    do_reverse_block = .false.
    ! Check if block is within rMin
    if(rMin_B(iBlock) <= rMin) RETURN

    if(CosAngleMin < 0.001)then
       ! No need to check angle
       do_reverse_block = .true.
       RETURN
    end if

    ! Check angle between velocity (in corotating frame)  and magnetic field
    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if(.not. Used_GB(i,j,k,iBlock)) CYCLE
       u_D = State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)/State_VGB(Rho_,i,j,k,iBlock)
       if(.not.UseRotatingFrame .and. OmegaBody /= 0.0)then
          ! Add rotational velocity
          u_D(1) = u_D(1) + OmegaBody*Xyz_DGB(y_,i,j,k,iBlock)
          u_D(2) = u_D(2) - OmegaBody*Xyz_DGB(x_,i,j,k,iBlock)
       end if
       b_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
       if(UseB0) b_D = b_D + B0_DGB(:,i,j,k,iBlock)
       CosAngle = abs(sum(u_D*b_D))/(norm2(u_D)*norm2(b_D) + 1e-30)
       ! Check if cos(angle) is large enough
       if(CosAngle < CosAngleMin) RETURN
    end do; end do; end do

    do_reverse_block = .true.

    call test_stop(NameSub, DoTest, iBlock)
  end function do_reverse_block
  !============================================================================
  subroutine reverse_field(iBlock, DoStateOld)

    ! Reverse magnetic field in block iBlock (including ghost cells)

    integer, intent(in):: iBlock
    logical, intent(in),  optional:: DoStateOld

    integer:: i, j, k
    real:: Ewave
    logical:: DoOld

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'reverse_field'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! Should we reverse StateOld_VGB
    DoOld = .false.
    if(present(DoStateOld)) DoOld = DoStateOld

    do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
       if(State_VGB(SignB_,i,j,k,iBlock) < 0)then
          if(UseB0)then
             ! Swap sign of total field (-B1-B0) and subtract B0: -B1-2*B0
             State_VGB(Bx_:Bz_,i,j,k,iBlock) = &
                  -State_VGB(Bx_:Bz_,i,j,k,iBlock) &
                  -2*B0_DGB(:,i,j,k,iBlock)
          else
             ! Swap sign of B1
             State_VGB(Bx_:Bz_,i,j,k,iBlock) = -State_VGB(Bx_:Bz_,i,j,k,iBlock)
          end if
          if(UseAlfvenWaves)then
             ! Swap incoming and outgoing waves
             Ewave = State_VGB(WaveFirst_,i,j,k,iBlock)
             State_VGB(WaveFirst_,i,j,k,iBlock) = &
                  State_VGB(WaveLast_,i,j,k,iBlock)
             State_VGB(WaveLast_,i,j,k,iBlock) = Ewave
          end if
       end if
       if(.not.DoOld) CYCLE
       if(StateOld_VGB(SignB_,i,j,k,iBlock) >= 0) CYCLE
       if(UseB0)then
          ! Swap sign of total field (-B1-B0) and subtract B0: -B1-2*B0
          StateOld_VGB(Bx_:Bz_,i,j,k,iBlock) = &
               -StateOld_VGB(Bx_:Bz_,i,j,k,iBlock) &
               -2*B0_DGB(:,i,j,k,iBlock)
       else
          ! Swap sign of B1
          StateOld_VGB(Bx_:Bz_,i,j,k,iBlock) = &
               -StateOld_VGB(Bx_:Bz_,i,j,k,iBlock)
       end if
    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine reverse_field
  !============================================================================
end module ModReverseField
!==============================================================================
