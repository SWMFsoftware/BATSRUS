!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModChGL
  ! In steady state MHD the magnetic field and mass density flux are
  ! related, according to the Chou-Goldberg-Low theory  with the scalar
  ! coefficient, \mathbf{B} = s \rho\mathbf{U}, which is constant along
  ! the magnetic field line,
  ! \partial s/\partial t + \mathbf{U}\cdot\nabla s = 0
  ! which may be solved as the conservation law:
  ! \partial (\rho s)/\partial t + \nabla\cdot(\rho s  \mathbf{U}) = 0
  ! Once \rho s is solved, the magnetic field may be found locally as
  ! \mathbf{B} = (\rho s)\mathbf{U}
  use ModVarIndexes, ONLY: Bx_, Bz_, RhoUx_, RhoUz_, SignB_,  Rho_
  implicit none
  PRIVATE ! Except
  logical, public :: UseChGL = .false.
  ! For R < RSourceChGL the ChGL ratio is calculated in terms of U, B
  real :: RSourceChGL = -1.0
  ! For R > RMinChGL the magnetic field is solved as
  ! \mathbf{B} = (\rho s)\mathbf{U}:
  real :: RMinChGL = -1.0
  public :: update_chgl ! Assign ChGL density or express B = (\rho s)\mathbf{U}
contains
  !============================================================================
  subroutine update_chgl(iBlock)
    use ModAdvance,  ONLY: State_VGB, nI, nJ, nK, MaxDim
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
          RhoU2 = sum(State_VGB(RhoUx_:Rhouz_,i,j,k,iBlock)**2)
          if(RhoU2 ==0.0)then
             State_VGB(SignB_,i,j,k,iBlock) = 0
          else
             B_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
             if(UseB0)B_D = B_D + B0_DGB(:,i,j,k,iBlock)
             State_VGB(SignB_,i,j,k,iBlock) =                   &
                  (State_VGB(Rho_,i,j,k,iBlock)/RhoU2)*         &
                  sum(State_VGB(RhoUx_:Rhouz_,i,j,k,iBlock)*B_D)
          end if
       end if
       if(R_BLK(i,j,k,iBlock) > RMinChGL)then
          ! the magnetic field is solved as
          ! \mathbf{B} = (\rho s)\mathbf{U}
          B_D = State_VGB(SignB_,i,j,k,iBlock)*       &
               State_VGB(RhoUx_:Rhouz_,i,j,k,iBlock)/ &
               State_VGB(Rho_,i,j,k,iBlock)
          if(UseB0)B_D = B_D - B0_DGB(:,i,j,k,iBlock)
          State_VGB(Bx_:Bz_,i,j,k,iBlock) =  B_D
       end if
    end do; end do; end do
  end subroutine update_chgl
  !============================================================================
end module ModChGL
!==============================================================================

