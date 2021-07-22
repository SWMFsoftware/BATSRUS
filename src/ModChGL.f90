!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModChGL
  ! In steady state MHD the magnetic field and mass density flux are
  ! related, according to the Chew-Golberger-Low theory  with the scalar
  ! coefficient, \mathbf{B} = s \rho\mathbf{U}, which is constant along
  ! the magnetic field line,
  ! \partial s/\partial t + \mathbf{U}\cdot\nabla s = 0
  ! which may be solved as the conservation law:
  ! \partial (\rho s)/\partial t + \nabla\cdot(\rho s  \mathbf{U}) = 0
  ! Once \rho s is solved, the magnetic field may be found locally as
  ! \mathbf{B} = (\rho s)\mathbf{U}
  use ModVarIndexes, ONLY: Bx_, Bz_, RhoUx_, RhoUz_, SignB_, Rho_, &
       nVar, Ux_, Uz_
  use BATL_lib, ONLY: MaxDim
  implicit none
  PRIVATE ! Except
  logical, public :: UseChGL = .false.
  ! For R < RSourceChGL the ChGL ratio is calculated in terms of U, B
  real :: RSourceChGL = 0.0
  ! For R > RMinChGL the magnetic field is solved as
  ! \mathbf{B} = (\rho s)\mathbf{U}:
  real, public :: RMinChGL = -1.0
  public :: read_chgl_param ! Read model parameters
  public :: init_chgl
  public :: update_chgl ! Assign ChGL density or express B = (\rho s)\mathbf{U}
  public :: get_chgl_state ! Do same in a single point
contains
  !============================================================================
  subroutine read_chgl_param
    use ModReadParam, ONLY: read_var
    !--------------------------------------------------------------------------
    call read_var('UseChGL', UseChGL)
    if(.not.UseChGL)RETURN
    if(SignB_==1)call stop_mpi(&
            'Reconfigure the code with setting meaningful value for SignB_')
    call read_var('RSourceChGL', RSourceChGL)
    call read_var('RMinChGL', RMinChGL)
    if(RMinChGL < 0)RMinChGL = huge(1.0)
  end subroutine read_chgl_param
  !============================================================================
  subroutine init_chgl(iBlock)
    use ModAdvance,  ONLY: State_VGB, nI, nJ, nK
    use ModGeometry, ONLY: r_BLK, true_cell
    use ModB0,       ONLY: UseB0, B0_DGB
    integer, intent(in) :: iBlock
    integer :: i, j, k
    real    :: RhoU2, B_D(MaxDim)
    !--------------------------------------------------------------------------
    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if(.not.true_cell(i,j,k,iBlock))CYCLE
       ! The ChGL ratio is calculated in terms of U, B
       RhoU2 = sum(State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)**2)
       if(RhoU2 ==0.0)then
          State_VGB(SignB_,i,j,k,iBlock) = 0
       else
          B_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
          if(UseB0)B_D = B_D + B0_DGB(:,i,j,k,iBlock)
          State_VGB(SignB_,i,j,k,iBlock) =                   &
               (State_VGB(Rho_,i,j,k,iBlock)/RhoU2)*         &
               sum(State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)*B_D)
       end if
    end do; end do; end do
  end subroutine init_chgl
  !============================================================================
  subroutine update_chgl(iBlock)
    use ModAdvance,  ONLY: State_VGB, nI, nJ, nK
    use ModGeometry, ONLY: r_BLK, true_cell
    use ModB0,       ONLY: UseB0, B0_DGB
    integer, intent(in) :: iBlock
    integer :: i, j, k
    real    :: RhoU2, B_D(MaxDim)
    !--------------------------------------------------------------------------
    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if(.not.true_cell(i,j,k,iBlock))CYCLE
       if(R_BLK(i,j,k,iBlock) < RSourceChGL)then
          ! The ChGL ratio is calculated in terms of U, B
          RhoU2 = sum(State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)**2)
          if(RhoU2 ==0.0)then
             State_VGB(SignB_,i,j,k,iBlock) = 0
          else
             B_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
             if(UseB0)B_D = B_D + B0_DGB(:,i,j,k,iBlock)
             State_VGB(SignB_,i,j,k,iBlock) =                   &
                  (State_VGB(Rho_,i,j,k,iBlock)/RhoU2)*         &
                  sum(State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)*B_D)
          end if
       end if
       if(R_BLK(i,j,k,iBlock) > RMinChGL)then
          ! the magnetic field is solved as
          ! \mathbf{B} = (\rho s)\mathbf{U}
          B_D = State_VGB(SignB_,i,j,k,iBlock)*       &
               State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)/ &
               State_VGB(Rho_,i,j,k,iBlock)
          if(UseB0)B_D = B_D - B0_DGB(:,i,j,k,iBlock)
          State_VGB(Bx_:Bz_,i,j,k,iBlock) =  B_D
       end if
    end do; end do; end do
  end subroutine update_chgl
  !============================================================================
  subroutine get_chgl_state(Xyz_D, State_V)
    use ModB0, ONLY: UseB0, get_b0
    real, intent(in)   :: Xyz_D(MaxDim)
    real, intent(inout):: State_V(nVar)
    ! Radial distance and the velocity squared
    real :: R, U2, B0_D(MaxDim)
    !--------------------------------------------------------------------------
    R = sqrt(sum(Xyz_D**2))
    if(R < RSourceChGL)then
       ! The ChGL ratio is calculated in terms of U, B
       U2 = sum(State_V(Ux_:Uz_)**2)
       if(U2 ==0.0)then
          State_V(SignB_) = 0
       else
          State_V(SignB_) = sum(State_V(Bx_:Bz_)*State_V(Ux_:Uz_))/U2
       end if
    end if
    if(R > RMinChGL)then
       ! the magnetic field is solved as
       ! \mathbf{B} = (\rho s)\mathbf{U}
       State_V(Bx_:Bz_) = State_V(SignB_)*State_V(Ux_:Uz_)
       if(UseB0)then
          call get_b0(Xyz_D, B0_D)
          State_V(Bx_:Bz_) = State_V(Bx_:Bz_) - B0_D
       end if
    end if
  end subroutine get_chgl_state
  !============================================================================
end module ModChGL
!==============================================================================

