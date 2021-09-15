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
  use ModAdvance,  ONLY: State_VGB, nI, nJ, nK
  use ModGeometry, ONLY: r_BLK, true_cell
  use ModB0,       ONLY: UseB0, B0_DGB
  implicit none
  PRIVATE ! Except
  logical, public :: UseChGL = .false.
  ! For R < RSourceChGL the ChGL ratio is calculated in terms of U, B
  real, public :: RSourceChGL = 0.0
  ! For R > RMinChGL the magnetic field is solved as
  ! \mathbf{B} = (\rho s)\mathbf{U}:
  real, public :: RMinChGL = -1.0
  public :: read_chgl_param ! Read model parameters
  public :: init_chgl
  public :: update_chgl ! Assign ChGL density or express B = (\rho s)\mathbf{U}
  public :: get_chgl_state ! Do same in a single point
  public :: correct_chgl_face_value ! Calculate magnetic field face values
  public :: aligning_bc             ! Align field and stream from the MHD side
  public :: calc_aligning_region_timestep ! Use global timestep in the region
  ! If the below logical is true, between rSourceChGL and rMinChGL
  ! the aligning source is applied
  logical, public   :: UseAligningSource  = .false.
  ! The aligning source, in addition to the geometric factor is limited, if
  ! the local Alvenic Mach number is less than sqrt(MA2Limiter)
  real, parameter   :: MA2Limiter = 0.01
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
    UseAligningSource = RMinChGL > RSourceChGL + 0.1
    ! if(RMinChGL < 0)RMinChGL = huge(1.0)
  end subroutine read_chgl_param
  !============================================================================
  subroutine init_chgl(iBlock)
    use ModMain, ONLY: Dt, IsTimeAccurate=>time_accurate
    integer, intent(in) :: iBlock
    integer :: i, j, k
    real    :: RhoU2, B_D(MaxDim)
    !--------------------------------------------------------------------------
    if(.not.IsTimeAccurate.and.UseAligningSource)Dt = max(Dt,0.0)
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
  subroutine update_chgl(iBlock, iStage)
    use ModMain,     ONLY: nStage
    use ModAdvance,  ONLY: StateOld_VGB
    integer, intent(in) :: iBlock, iStage
    integer :: i, j, k
    real    :: RhoU2, B_D(MaxDim), Rho, DeltaU_D(3), MomentumSource_D(3), B2
    !--------------------------------------------------------------------------
    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if(.not.true_cell(i,j,k,iBlock))CYCLE
       if(R_BLK(i,j,k,iBlock) < RSourceChGL)then
          ! The ChGL ratio is calculated in terms of U, B
          RhoU2 = sum(State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)**2)
          if(RhoU2 ==0.0.or.UseAligningSource)then
             State_VGB(SignB_,i,j,k,iBlock) = 0
          else
             B_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
             if(UseB0)B_D = B_D + B0_DGB(:,i,j,k,iBlock)
             State_VGB(SignB_,i,j,k,iBlock) =                 &
                  (State_VGB(Rho_,i,j,k,iBlock)/RhoU2)*       &
                  sum(State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)*B_D)
          end if
       elseif(UseAligningSource.and.r_BLK(i,j,k,iBlock) < RMinChGL       &
            .and.iStage==nStage)then
          RhoU2 = sum(State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)**2)
          if(RhoU2 ==0.0)then
             State_VGB(SignB_,i,j,k,iBlock) = 0
          else
             ! The ChGL ratio is calculated in terms of U, B
             Rho = State_VGB(Rho_,i,j,k,iBlock)
             B_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
             if(UseB0)B_D = B_D + B0_DGB(:,i,j,k,iBlock)
             State_VGB(SignB_,i,j,k,iBlock) = Rho/RhoU2*                &
                  sum(State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)*B_D)*       &
                  ! Geometric interpolation factor
                  (R_BLK(i,j,k,iBlock) - RSourceChGL) / &
                  (RMinChGL - RSourceChGL)
             ! Increment in velocity
             DeltaU_D = State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)/&
                  State_VGB(Rho_,i,j,k,iBlock) -        &
                  StateOld_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)/&
                  StateOld_VGB(Rho_,i,j,k,iBlock)
             B2 = sum(B_D**2)
             ! Check if the stream is energetic enough
             if(B2*Rho > RhoU2)then
                ! The increment in the velcity should be reduced
                MomentumSource_D = (1 - RhoU2/(Rho*B2))*(&
                     sum(DeltaU_D*B_D)*B_D/B2 - DeltaU_D)
                State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) = &
                     State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) + &
                     MomentumSource_D*Rho
                DeltaU_D = DeltaU_D + MomentumSource_D
             end if
             State_VGB(Bx_:Bz_,i,j,k,iBlock) =          &
                  State_VGB(Bx_:Bz_,i,j,k,iBlock) +     &
                  ! Field-to-stream-speed ratio
                  State_VGB(SignB_,i,j,k,iBlock) *      &
                  ! Increment in velocity
                  DeltaU_D
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
  subroutine correct_chgl_face_value(iBlock, DoResChangeOnly)
    use ModSize, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK
    use ModMain,     ONLY: nIFace, nJFace, nKFace, &
         iMinFace, iMaxFace, jMinFace, jMaxFace, kMinFace, kMaxFace
    use ModParallel, ONLY: &
         neiLtop, neiLbot, neiLeast, neiLwest, neiLnorth, neiLsouth
    use BATL_lib,      ONLY: IsCartesian, Xyz_DGB, FaceNormal_DDFB

    integer, intent(in) :: iBlock
    logical, intent(in) :: DoResChangeOnly
    ! Logical is true in the points of ChGL model
    logical             :: IsChGL_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)
    !--------------------------------------------------------------------------
    IsChGL_G = r_BLK(:,:,:,iBlock) > rMinChGL
    if (.not.DoResChangeOnly)then
       call correct_faceX(&
            1,nIFace,jMinFace,jMaxFace,kMinFace,kMaxFace)
       if(nJ > 1) call correct_faceY(&
            iMinFace,iMaxFace,1,nJFace,kMinFace,kMaxFace)
       if(nK > 1) call correct_faceZ(&
            iMinFace,iMaxFace,jMinFace,jMaxFace,1,nKFace)
    else
       if(neiLeast(iBlock)==+1)&
            call correct_faceX(1,1,1,nJ,1,nK)
       if(neiLwest(iBlock)==+1)&
            call correct_faceX(nIFace,nIFace,1,nJ,1,nK)
       if(nJ > 1 .and. neiLsouth(iBlock)==+1) &
            call correct_faceY(1,nI,1,1,1,nK)
       if(nJ > 1 .and. neiLnorth(iBlock)==+1) &
            call correct_faceY(1,nI,nJFace,nJFace,1,nK)
       if(nK > 1 .and. neiLbot(iBlock)==+1) &
            call correct_faceZ(1,nI,1,nJ,1,1)
       if(nK > 1 .and. neiLtop(iBlock)==+1) &
            call correct_faceZ(1,nI,1,nJ,nKFace,nKFace)
    end if
  contains
    !==========================================================================
    subroutine correct_facex(iMin,iMax,jMin,jMax,kMin,kMax)
      use ModB0,       ONLY: B0_DX
      use ModAdvance,  ONLY: LeftState_VX, RightState_VX
      integer,intent(in):: iMin,iMax,jMin,jMax,kMin,kMax
      ! Loop variables
      integer :: i, j, k
      ! B0_DX field or zero
      real :: B0_DIII(MaxDim,iMin:iMax,jMin:jMax,kMin:kMax)
      !------------------------------------------------------------------------
      if(.not.any(IsChGL_G(iMin-1:iMax,jMin:jMax,kMin:kMax)))RETURN
      if(UseB0)then
         B0_DIII = B0_DX(:,iMin:iMax,jMin:jMax,kMin:kMax)
      else
         B0_DIII = 0.0
      end if
      do k = kMin, kMax; do j = jMin, jMax
         i = iMin - 1
         if(IsChGL_G(i,j,k))LeftState_VX(Bx_:Bz_,i+1,j,k) =              &
              LeftState_VX(Ux_:Uz_,i+1,j,k)*LeftState_VX(SignB_,i+1,j,k)-&
              B0_DIII(:,i+1,j,k)
         do i = iMin, iMax -1
            if(.not.IsChGL_G(i,j,k))CYCLE
            LeftState_VX(Bx_:Bz_,i+1,j,k) =                              &
                 LeftState_VX(Ux_:Uz_,i+1,j,k)*LeftState_VX(SignB_,i+1,j,k)-&
                 B0_DIII(:,i+1,j,k)
            RightState_VX(Bx_:Bz_,i,j,k) =                               &
                 RightState_VX(Ux_:Uz_,i,j,k)*RightState_VX(SignB_,i,j,k)-  &
                 B0_DIII(:,i,j,k)
         end do
         i = iMax
         if(IsChGL_G(i,j,k))RightState_VX(Bx_:Bz_,i,j,k) =               &
              RightState_VX(Ux_:Uz_,i,j,k)*RightState_VX(SignB_,i,j,k)-  &
              B0_DIII(:,i,j,k)
      end do; end do
    end subroutine correct_facex
    !==========================================================================
    subroutine correct_facey(iMin,iMax,jMin,jMax,kMin,kMax)
      use ModB0,       ONLY: B0_DY
      use ModAdvance,  ONLY: LeftState_VY, RightState_VY
      integer,intent(in):: iMin,iMax,jMin,jMax,kMin,kMax
      ! Loop variables
      integer :: i, j, k
      ! B0_DY field or zero
      real :: B0_DIII(MaxDim,iMin:iMax,jMin:jMax,kMin:kMax)
      !------------------------------------------------------------------------
      if(.not.any(IsChGL_G(iMin:iMax,jMin-1:jMax,kMin:kMax)))RETURN
      if(UseB0)then
         B0_DIII = B0_DY(:,iMin:iMax,jMin:jMax,kMin:kMax)
      else
         B0_DIII = 0.0
      end if
      do k = kMin, kMax
         j = jMin - 1
         do i = iMin, iMax
            if(IsChGL_G(i,j,k))LeftState_VY(Bx_:Bz_,i,j+1,k) =              &
                 LeftState_VY(Ux_:Uz_,i,j+1,k)*LeftState_VY(SignB_,i,j+1,k)-&
                 B0_DIII(:,i,j+1,k)
         end do
         do j = jMin, jMax - 1; do i = iMin, iMax
            if(.not.IsChGL_G(i,j,k))CYCLE
            LeftState_VY(Bx_:Bz_,i,j+1,k) =                                 &
                 LeftState_VY(Ux_:Uz_,i,j+1,k)*LeftState_VY(SignB_,i,j+1,k)-&
              B0_DIII(:,i,j+1,k)
            RightState_VY(Bx_:Bz_,i,j,k) =                                  &
              RightState_VY(Ux_:Uz_,i,j,k)*RightState_VY(SignB_,i,j,k)-     &
              B0_DIII(:,i,j,k)
         end do; end do
         j = jMax
         do i = iMin, iMax
            if(IsChGL_G(i,j,k))RightState_VY(Bx_:Bz_,i,j,k) =               &
              RightState_VY(Ux_:Uz_,i,j,k)*RightState_VY(SignB_,i,j,k)-     &
              B0_DIII(:,i,j,k)
         end do
      end do
    end subroutine correct_facey
    !==========================================================================
    subroutine correct_facez(iMin,iMax,jMin,jMax,kMin,kMax)
      use ModB0,       ONLY: B0_DZ
      use ModAdvance,  ONLY: LeftState_VZ, RightState_VZ
      integer,intent(in):: iMin,iMax,jMin,jMax,kMin,kMax
      ! Loop variables
      integer :: i, j, k
      ! B0_DZ field or zero
      real :: B0_DIII(MaxDim,iMin:iMax,jMin:jMax,kMin:kMax)
      !------------------------------------------------------------------------
      if(.not.any(IsChGL_G(iMin:iMax,jMin:jMax,kMin-1:kMax)))RETURN
      if(UseB0)then
         B0_DIII = B0_DZ(:,iMin:iMax,jMin:jMax,kMin:kMax)
      else
         B0_DIII = 0.0
      end if
      k = kMin - 1
      do  j = jMin, jMax; do i = iMin, iMax
         if(IsChGL_G(i,j,k))LeftState_VZ(Bx_:Bz_,i,j,k+1) =              &
              LeftState_VZ(Ux_:Uz_,i,j,k+1)*LeftState_VZ(SignB_,i,j,k+1)-&
              B0_DIII(:,i,j,k+1)
      end do; end do
      do k = kMin, kMax - 1; do j = jMin, jMax; do i = iMin, iMax
         if(.not.IsChGL_G(i,j,k))CYCLE
         LeftState_VZ(Bx_:Bz_,i,j,k+1) =                                 &
              LeftState_VZ(Ux_:Uz_,i,j,k+1)*LeftState_VZ(SignB_,i,j,k+1)-&
              B0_DIII(:,i,j,k+1)
         RightState_VZ(Bx_:Bz_,i,j,k) =                                  &
              RightState_VZ(Ux_:Uz_,i,j,k)*RightState_VZ(SignB_,i,j,k) - &
              B0_DIII(:,i,j,k)
      end do; end do; end do
      k = kMax
      do  j = jMin, jMax; do i = iMin, iMax
         if(IsChGL_G(i,j,k))RightState_VZ(Bx_:Bz_,i,j,k) =               &
              RightState_VZ(Ux_:Uz_,i,j,k)*RightState_VZ(SignB_,i,j,k) - &
              B0_DIII(:,i,j,k)
      end do; end do
    end subroutine correct_facez
    !==========================================================================
  end subroutine correct_chgl_face_value
  !============================================================================
  subroutine aligning_bc(iFace,jFace,kFace, iBlockFace,                &
         iLeft, jLeft, kLeft, Normal_D, B0x, B0y, B0z,                 &
         StateLeft_V, StateRight_V)
    integer, intent(in) :: iFace,jFace,kFace, iBlockFace,              &
         iLeft, jLeft, kLeft
    real, intent(in)    :: Normal_D(3), B0x, B0y, B0z
    real, intent(inout) :: StateLeft_V(nVar), StateRight_V(nVar)
    !--------------------------------------------------------------------------
    call stop_mpi('Work in progresss')
  end subroutine aligning_bc
  !============================================================================
  subroutine calc_aligning_region_timestep(iBlock)
    use ModMain, ONLY: IsTimeAccurate=>time_accurate, dt_BLK, dt, cfl
    use ModAdvance, ONLY: time_BLK
    integer, intent(in) :: iBlock
    logical :: Mask_C(1:nI,1:nJ,1:nK) = .false.
    !--------------------------------------------------------------------------
    ! Mask for aligning source region
    Mask_C = true_cell(1:nI,1:nJ,1:nK,iBlock).and.       &
         r_blk(1:nI,1:nJ,1:nK,iBlock) > RSourceChGL.and.&
         r_blk(1:nI,1:nJ,1:nK,iBlock) < RMinChGL
    ! Do nothing, if no point is in the aligning source region
    if(.not.any(Mask_C))RETURN
    ! Store the minimum time step for the aligning source region
    Dt_BLK(iBlock) = minval(time_blk(1:nI,1:nJ,1:nK,iBlock), MASK = Mask_C)
    ! Set the global time step throughout this region
    where(Mask_C)time_blk(1:nI,1:nJ,1:nK,iBlock) = Dt/CFL
  end subroutine calc_aligning_region_timestep
  !============================================================================
end module ModChGL
!==============================================================================

