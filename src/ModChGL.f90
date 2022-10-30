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

  use ModBatsrusUtility, ONLY: stop_mpi
  use ModVarIndexes, ONLY: Bx_, Bz_, RhoUx_, RhoUz_, SignB_, Rho_, &
       nVar, Ux_, Uz_
  use BATL_lib, ONLY: MaxDim
  use ModAdvance,  ONLY: State_VGB, nI, nJ, nK, Uz__, Bz__
  use ModGeometry, ONLY: r_GB, Used_GB
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
  end subroutine read_chgl_param
  !============================================================================
  subroutine init_chgl(iBlock)

    integer, intent(in) :: iBlock
    integer :: i, j, k
    real    :: RhoU2, B_D(MaxDim)
    !--------------------------------------------------------------------------
    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if(.not.Used_GB(i,j,k,iBlock))CYCLE
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
    integer, intent(in) :: iBlock, iStage
    integer :: i, j, k
    real    :: RhoU2, B_D(MaxDim), Rho, B2
    real    :: VA2, UDotB, UPar2, GeometricFactor, U_D(3), Ut_D(3)
    !--------------------------------------------------------------------------
    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if(.not.Used_GB(i,j,k,iBlock))CYCLE
       if(r_GB(i,j,k,iBlock) < RSourceChGL)then
          ! The ChGL ratio is calculated in terms of U, B
          RhoU2 = sum(State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)**2)
          if(RhoU2 ==0.0)then
             State_VGB(SignB_,i,j,k,iBlock) = 0
          else
             B_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
             if(UseB0)B_D = B_D + B0_DGB(:,i,j,k,iBlock)
             State_VGB(SignB_,i,j,k,iBlock) =                 &
                  (State_VGB(Rho_,i,j,k,iBlock)/RhoU2)*       &
                  sum(State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)*B_D)
          end if
       elseif(r_GB(i,j,k,iBlock) < RMinChGL)then
          if(iStage/=nStage)CYCLE
          RhoU2 = sum(State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)**2)
          if(RhoU2 ==0.0)then
             State_VGB(SignB_,i,j,k,iBlock) = 0
             CYCLE
          end if
          ! The Leontowich BC (see L. D. Landau and E. M. Lifshits,
          ! Electrrodynamics of Continuous Media, Chapter 87 Surface impedance
          ! of metals). Near the surface with concentrated impedance
          ! (surface of metal with pronounced skin-effect) the tangential
          ! electric and magnetic field vectors are related with the boundary
          ! condition:
          ! \delta B_t \propto n x E_t, where E_t = Bn n x U_t - Un n x B_t
          ! Note: the unit vetor of normal is directed toward the metal, i.e.
          ! from the MHD domain toward the ChGL domain.
          !
          ! Hence, \delta B = (-Bn U_t + Un B_t)/Impedance
          ! (-FullBn*Ut_D + Un*FullBt_D)/ &
          ! The estimate for impedance is as follows:
          ! sqrt(max(1.0e-30, Un**2 + FullBn**2/StateLeft_V(Rho_)))

          Rho = State_VGB(Rho_,i,j,k,iBlock)
          B_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
          if(UseB0)B_D = B_D + B0_DGB(:,i,j,k,iBlock)
          ! Geometric interpolation factor
          GeometricFactor = (r_GB(i,j,k,iBlock) - RSourceChGL) / &
               (RMinChGL - RSourceChGL)
          B2 = max(sum(B_D**2), 1e-30)
          U_D = State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)/Rho
          UDotB = sum(U_D*B_D); UPar2 = UDotB**2/B2
          Ut_D = U_D - UDotB*B_D/B2
          VA2 = B2/Rho
          U_D = U_D - GeometricFactor*Ut_D*VA2/(VA2 + UPar2)
          B_D = B_D + GeometricFactor*Ut_D*UDotB/(VA2 + UPar2)
          State_VGB(SignB_,i,j,k,iBlock) = sum(U_D*B_D)/max(sum(U_D**2), 1e-30)
          State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) = Rho*U_D
          State_VGB(Bx_:Bz_,i,j,k,iBlock) = &
               State_VGB(Bx_:Bz_,i,j,k,iBlock) + &
               GeometricFactor*Ut_D*UDotB/(VA2 + UPar2)
       else
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
#ifndef SCALAR
    R = sqrt(sum(Xyz_D**2))
    if(R < RSourceChGL)then
       ! The ChGL ratio is calculated in terms of U, B
       U2 = sum(State_V(Ux_:Uz__)**2)
       if(U2 ==0.0)then
          State_V(SignB_) = 0
       else
          State_V(SignB_) = sum(State_V(Bx_:Bz__)*State_V(Ux_:Uz__))/U2
       end if
    end if
    if(R > RMinChGL)then
       ! the magnetic field is solved as
       ! \mathbf{B} = (\rho s)\mathbf{U}
       State_V(Bx_:Bz__) = State_V(SignB_)*State_V(Ux_:Uz__)
       if(UseB0)then
          call get_b0(Xyz_D, B0_D)
          State_V(Bx_:Bz_) = State_V(Bx_:Bz_) - B0_D
       end if
    end if
#endif

  end subroutine get_chgl_state
  !============================================================================
  subroutine correct_chgl_face_value(iBlock, DoResChangeOnly)

    use ModSize, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK
    use ModMain,     ONLY: nIFace, nJFace, nKFace, &
         iMinFace, iMaxFace, jMinFace, jMaxFace, kMinFace, kMaxFace
    use ModParallel, ONLY: DiLevel_EB

    integer, intent(in) :: iBlock
    logical, intent(in) :: DoResChangeOnly

    ! Logical is true in the points of ChGL model
    logical             :: IsChGL_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)
    !--------------------------------------------------------------------------
    IsChGL_G = r_GB(:,:,:,iBlock) > rMinChGL
    if (.not.DoResChangeOnly)then
       call correct_faceX(&
            1,nIFace,jMinFace,jMaxFace,kMinFace,kMaxFace)
       if(nJ > 1) call correct_faceY(&
            iMinFace,iMaxFace,1,nJFace,kMinFace,kMaxFace)
       if(nK > 1) call correct_faceZ(&
            iMinFace,iMaxFace,jMinFace,jMaxFace,1,nKFace)
    else
       if(DiLevel_EB(1,iBlock)==+1)&
            call correct_facex(1,1,1,nJ,1,nK)
       if(DiLevel_EB(2,iBlock)==+1)&
            call correct_facex(nIFace,nIFace,1,nJ,1,nK)
       if(nJ > 1 .and. DiLevel_EB(3,iBlock)==+1) &
            call correct_facey(1,nI,1,1,1,nK)
       if(nJ > 1 .and. DiLevel_EB(4,iBlock)==+1) &
            call correct_facey(1,nI,nJFace,nJFace,1,nK)
       if(nK > 1 .and. DiLevel_EB(5,iBlock)==+1) &
            call correct_facez(1,nI,1,nJ,1,1)
       if(nK > 1 .and. DiLevel_EB(6,iBlock)==+1) &
            call correct_facez(1,nI,1,nJ,nKFace,nKFace)
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
       iLeft, jLeft, kLeft, Normal_D, B0x, B0y, B0z,                &
       StateLeft_V, StateRight_V)
    integer, intent(in) :: iFace,jFace,kFace, iBlockFace,              &
         iLeft, jLeft, kLeft
    real, intent(in)    :: Normal_D(3), B0x, B0y, B0z
    real, intent(inout) :: StateLeft_V(nVar), StateRight_V(nVar)
    real :: FullB_D(3), U2
    !--------------------------------------------------------------------------
#ifndef SCALAR
    U2 = max(sum(StateLeft_V(Ux_:Uz__)**2), &
         sum(StateRight_V(Ux_:Uz__)**2), 1e-30)
    if(r_GB(iLeft,jLeft,kLeft,iBlockFace) < RMinChGL.and.&
         r_GB(iFace,jFace,kFace,iBlockFace) >= RMinChGL)then
       FullB_D  = StateLeft_V(Bx_:Bz_) + [B0x, B0y, B0z]
       StateLeft_V(SignB_) = sum(StateLeft_V(Ux_:Uz_)*FullB_D)/U2
    elseif(r_GB(iFace,jFace,kFace,iBlockFace) < RMinChGL.and.&
         r_GB(iLeft,jLeft,kLeft,iBlockFace) >= RMinChGL)then
       FullB_D  = StateRight_V(Bx_:Bz_) + [B0x, B0y, B0z]
       StateRight_V(SignB_) = sum(StateRight_V(Ux_:Uz_)*FullB_D)/U2
    end if
#endif
  end subroutine aligning_bc
  !============================================================================
end module ModChGL
!==============================================================================
