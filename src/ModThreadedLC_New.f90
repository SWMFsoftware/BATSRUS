!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModThreadedLC

  use ModBatsrusUtility, ONLY: stop_mpi
#ifdef _OPENACC
  use ModUtilities, ONLY: norm2
#endif
  use BATL_lib, ONLY: test_start, test_stop, iProc

  use ModFieldLineThread,  ONLY: BoundaryThreads, Threads_B,             &
       PeFraction, iPe, DoInit_, Done_, Heat_, put_bc_to_sc,             &
       init_threaded_lc=>init_threads, relax_initial_state, get_bc_from_sc
  use omp_lib

  implicit none
  SAVE


  ! Redundant
  logical        :: UseAlignedVelocity = .true.
  logical        :: DoConvergenceCheck = .false.

  integer:: nIter = 20
  real   :: cTol = 1.0e-6

contains
  !============================================================================
  subroutine read_threaded_bc_param

    use ModReadParam, ONLY: read_var
    character(len=7)::TypeBc = 'limited'

    character(len=*), parameter:: NameSub = 'read_threaded_bc_param'
    !--------------------------------------------------------------------------
    call read_var('UseAlignedVelocity', UseAlignedVelocity)
    call read_var('DoConvergenceCheck', DoConvergenceCheck)
    call read_var('TypeBc'            , TypeBc            )
    select case(TypeBc)
    case('first', 'second', 'limited')
      ! Do nothing
    case default
       if(iProc==0)write(*,'(a)')&
            'Unknown TypeBc = '//TypeBc//', reset to limited'
    end select
    call read_var('Tolerance', cTol)
    call read_var('MaxIter', nIter)

  end subroutine read_threaded_bc_param
  !============================================================================
  subroutine set_field_line_thread_bc(nGhost, iBlock, nVarState, State_VG, &
       iImplBlock)

    use ModAdvance,      ONLY: State_VGB
    use ModGeometry, ONLY: Xyz_DGB
    use BATL_lib, ONLY:  MinI, MaxI, MinJ, MaxJ, MinK, MaxK, nJ, nK
    use ModPhysics,      ONLY: No2Si_V, Si2No_V, UnitTemperature_, &
         InvGammaElectronMinus1
    use ModVarIndexes,   ONLY: Rho_, Bx_, Bz_, &
         RhoUx_, RhoUz_, Ux_, Uz_, EHot_, WDiff_, nVar
    use ModImplicit,     ONLY: iTeImpl
    use ModHeatFluxCollisionless, ONLY: UseHeatFluxCollisionless, &
         get_gamma_collisionless
    use ModTransitionRegion, ONLY: advance_thread_semi_impl
    integer, intent(in):: nGhost
    integer, intent(in):: iBlock
    integer, intent(in):: nVarState
    real, intent(inout):: State_VG(nVarState,MinI:MaxI,MinJ:MaxJ,MinK:MaxK)

    ! Optional arguments when called by semi-implicit scheme
    integer, optional, intent(in):: iImplBlock
    ! Determines, which action should be done with the thread
    integer :: i, j, k
    real :: State_V(nVarState)
    real :: BDir_D(3), U_D(3), B1_D(3), SqrtRho, DirR_D(3)
    real :: GammaHere

    character(len=*), parameter:: NameSub = 'set_field_line_thread_bc'
    !--------------------------------------------------------------------------

    call timing_start('set_thread_bc')
    if(present(iImplBlock))then
       if(Threads_B(iBlock)%iAction/=Done_)&
            call stop_mpi('Algorithm error in '//NameSub)
       do k = 1, nK; do j = 1, nJ
          call prolong_state(j,k)
          Threads_B(iBlock)%Threads_II(j,k)%Te_G(0) = &
               State_V(iTeImpl)*No2Si_V(UnitTemperature_)
          State_VG(iTeImpl,1-nGhost:0,j,k) =  Si2No_V(UnitTemperature_)*&
               Threads_B(iBlock)%Threads_II(j,k)%Te_G(-1)
       end do; end do
       call timing_stop('set_thread_bc')
       RETURN
    end if
    select case(Threads_B(iBlock)%iAction)
    case(Done_)
       call timing_stop('set_thread_bc')
       RETURN
    case(Heat_)
       do k = 1, nK; do j = 1, nJ
          call prolong_state(j,k)
          call get_bc_from_sc(&
               State_V,Threads_B(iBlock)%Threads_II(j, k))
          call advance_thread_semi_impl(&
               Threads_B(iBlock)%Threads_II(j, k))
          call put_bc_to_sc(State_VG(:, 1, j, k),                    &
               Threads_B(iBlock)%Threads_II(j, k), Threads_B(iBlock)%&
               DirR_DII(:, j, k), State_VG(:, 0, j, k))
          do i = 1 - nGhost, -1
             State_VG(:, i,j,k) = State_VG(:,0, j, k)
          end do
       end do; end do
    case(DoInit_)
       do k = 1, nK; do j = 1, nJ
          call get_bc_from_sc(&
               State_VG(:, 1, j, k), Threads_B(iBlock)%Threads_II(j, k))
          call relax_initial_state(Threads_B(iBlock)%Threads_II(j, k))
          call prolong_state(j,k)
          call get_bc_from_sc(&
               State_V,Threads_B(iBlock)%Threads_II(j, k))
          call put_bc_to_sc(State_VG(:, 1, j, k),                    &
               Threads_B(iBlock)%Threads_II(j, k), Threads_B(iBlock)%&
               DirR_DII(:, j, k), State_VG(:, 0, j, k))
          do i = 1 - nGhost, -1
             State_VG(:, i,j,k) = State_VG(:,0, j, k)
          end do
       end do; end do
    case default
       do k = 1, nK; do j = 1, nJ
          call prolong_state(j,k)
          call get_bc_from_sc(&
               State_V,Threads_B(iBlock)%Threads_II(j, k))
          call put_bc_to_sc(State_VG(:, 1, j, k),                    &
               Threads_B(iBlock)%Threads_II(j, k), Threads_B(iBlock)%&
               DirR_DII(:, j, k), State_VG(:, 0, j, k))
          do i = 1 - nGhost, -1
             State_VG(:, i,j,k) = State_VG(:,0, j, k)
          end do
       end do; end do
    end select
    if(WDiff_>1)State_VG(WDiff_,1-nGhost:0, 1:nJ, 1:nK) = 0.0
    if(Ehot_ > 1)then
       if(UseHeatFluxCollisionless)then
          do k = 1, nK; do j = 1, nJ
             call get_gamma_collisionless(Xyz_DGB(:,1,j,k,iBlock), GammaHere)
             State_VG(Ehot_,1-nGhost:0,j,k) = &
                  PeFraction*State_VG(iPe,1-nGhost:0,j,k)*&
                  (1.0/(GammaHere - 1) - InvGammaElectronMinus1)
          end do; end do
       else
          State_VG(Ehot_,1-nGhost:0,1:nJ,1:nK) = 0.0
       end if
    end if
    Threads_B(iBlock)%iAction = Done_
    call timing_stop('set_thread_bc')
  contains
    !==========================================================================
    subroutine prolong_state(j,k)

      ! Get state on the prolonged magnetic field line passing through
      ! the face center
      integer, intent(in) :: j, k
      integer :: j1, k1
      real :: Weight_I(3)
      !------------------------------------------------------------------------
      j1 = Threads_B(iBlock) % iStencil_III(2,j,k)
      k1 = Threads_B(iBlock) % iStencil_III(3,j,k)
      Weight_I = Threads_B(iBlock) % Weight_III(:,j,k)
      State_V = Weight_I(1)*State_VG(:,1,j,k) + &
           Weight_I(2)*State_VG(:,1,j1,k)     + &
           Weight_I(3)*State_VG(:,1,j,k1)
    end subroutine prolong_state
    !==========================================================================
  end subroutine set_field_line_thread_bc
  !============================================================================
end module ModThreadedLC
!==============================================================================
