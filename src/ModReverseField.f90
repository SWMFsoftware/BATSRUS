!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModReverseField

  ! Reverse direction of magnetic field where it helps suppressing
  ! numerical reconnection

  use BATL_lib, ONLY: test_start, test_stop, &
       nDim, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, Xyz_DGB
  use ModAdvance, ONLY: State_VGB, StateOld_VGB
  use ModB0, ONLY: UseB0, B0_DGB
  use ModGeometry, ONLY: rMin_B
  use ModVarIndexes, ONLY: Bx_, Bz_, SignB_, &
       WaveFirst_, WaveLast_
  use ModWaves, ONLY: UseAlfvenWaves

  implicit none

  SAVE

  private ! except

  public:: read_reverse_field_param  ! read parameters
  public:: set_sign_field            ! set the SignB variable in a block
  public:: do_reverse_block          ! check if field should be reversed
  public:: reverse_field             ! reverse magnetic field in a block

  logical, public:: DoReverseField = .false. ! Use the scheme
  logical, public:: DoReverseBlock = .false. ! Use the scheme in this block

  ! Local variables
  real:: rMin = -1.0

contains
  !============================================================================
  subroutine read_reverse_field_param

    use ModReadParam, ONLY: read_var
    !--------------------------------------------------------------------------
    call read_var('DoReverseField', DoReverseField)
    call read_var('rMinReverse', rMin)

  end subroutine read_reverse_field_param
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

    ! Return true is magnetic field in block iBlock should be reversed

    integer, intent(in):: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'do_reverse_block'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    do_reverse_block = rMin_B(iBlock) > rMin

    call test_stop(NameSub, DoTest, iBlock)
  end function do_reverse_block
  !============================================================================
  subroutine reverse_field(iBlock, DoStateOld, DoReverse)

    ! Reverse magnetic field in block iBlock (including ghost cells)

    integer, intent(in):: iBlock
    logical, intent(in),  optional:: DoStateOld
    logical, intent(out), optional:: DoReverse

    integer:: i, j, k
    real:: Ewave
    logical:: DoOld

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'reverse_field'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! Should we reverse this block at all?
    if(present(DoReverse))then
       ! Check if magnetic field should be reversed
       DoReverse = do_reverse_block(iBlock)
       if(.not.DoReverse) RETURN
    end if

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
          if(UseAlfvenWaves .and. .not.DoReverseField)then
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
