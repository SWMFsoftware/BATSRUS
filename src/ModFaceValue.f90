!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModFaceValue

  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest, iBlockTest, iVarTest, &
       iDimTest
  use ModBatsrusUtility, ONLY: stop_mpi
  use ModSize, ONLY: nI, nJ, nK, nG, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, &
       x_, y_, z_, nDim
  use ModVarIndexes
  use ModGeometry, ONLY: Used_GB, IsBody_B
  use ModAdvance, ONLY: UseFDFaceFlux, UseLowOrder, &
       UseLowOrderRegion, IsLowOrderOnly_B, UseAdaptiveLowOrder, &
       State_VGB, Primitive_VG,  &
       LeftState_VX,  &  ! Face Left  X
       RightState_VX, &  ! Face Right X
       LeftState_VY,  &  ! Face Left  Y
       RightState_VY, &  ! Face Right Y
       LeftState_VZ,  &  ! Face Left  Z
       RightState_VZ     ! Face Right Z
  use ModTurbulence, ONLY: PoyntingFluxPerB, IsOnAwRepresentative
  use ModBorisCorrection, ONLY: &
       boris_to_mhd_x, boris_to_mhd_y, boris_to_mhd_z, &
       UseBorisRegion, set_clight_cell, set_clight_face, Clight_G
  use omp_lib

  implicit none

  private ! except

  public:: read_face_value_param
  public:: calc_face_value
  public:: correct_monotone_restrict
  public:: get_face_accurate1d, get_face_accurate2d, get_face_accurate3d
  public:: accurate_reschange2d, accurate_reschange3d
  public:: get_face_tvd
  public:: set_low_order_face
  public:: calc_cell_norm_velocity

  public:: clean_mod_face_value

  public:: get_log_limiter_var

  logical, public :: UseAccurateResChange = .false.
  logical, public :: UseTvdResChange      = .true.
  logical, public :: DoLimitMomentum      = .false.
  integer, public :: nGUsed               = nG
  !$acc declare create(UseAccurateResChange, DoLimitMomentum)
  !$acc declare create(UseTvdResChange)

  real,             public :: LimiterBeta = 1.0
  character(len=6), public :: TypeLimiter = 'minmod'
  character(len=6), public :: TypeLimiter5= 'mp'
  !$acc declare create(LimiterBeta, TypeLimiter)

  logical, public :: UseAccurateExtremum = .true.
  integer:: nLowOrder = 2
  real:: VelCrit, pCritLow, pCritHigh

  ! Logical switch for 5th order scheme: use cweno5 or mp5 scheme.
  logical, public ::  UseCweno = .false.

  logical, public:: UsePerVarLimiter = .false. ! Variable for CWENO5
  integer, public:: iVarSmooth_V(nVar), iVarSmoothIndex_I(nVar)

  ! Region parameters for low order scheme
  character(len=200), public:: StringLowOrderRegion = 'none'
  integer, allocatable, public:: iRegionLowOrder_I(:)
  !$omp threadprivate( iRegionLowOrder_I )

  ! Local variables -----------------

  ! Parameters for the limiter applied near resolution changes
  ! Scheme has no effect if BetaLimierReschange is larger than LimiterBeta
  real    :: BetaLimiterResChange  = 2.0
  integer :: nFaceLimiterResChange = 2
  !$acc declare create(BetaLimiterResChange, nFaceLimiterResChange)

  ! Parameters for limiting the logarithm of variables
  logical :: UseLogRhoLimiter = .false.
  logical :: UseLogPLimiter   = .false.
  !$acc declare create(UseLogRhoLimiter, UseLogPLimiter)

  ! Logicals for limiting the logarithm of variables
  logical, public :: UseLogLimiter, UseLogLimiter_V(nVar)
  !$acc declare create(UseLogLimiter, UseLogLimiter_V)

  ! Logicals for limiting the total pressure
  logical :: UsePtotalLimiter
  !$acc declare create(UsePtotalLimiter)

  ! Parameters for limiting the total pressure (p + p_e + p_wave)
  logical :: UsePtotalLtd     = .false.
  !$acc declare create(UsePtotalLtd)

  ! Parameters for limiting the variable divided by density
  logical :: UseRhoRatioLimiter = .false.
  !$acc declare  create(UseRhoRatioLimiter)
  integer :: nVarLimitRatio
  integer, allocatable, save:: iVarLimitRatio_I(:)
  !$omp threadprivate( iVarLimitRatio_I )
  !$acc declare create(iVarLimitRatio_I)

  ! Maximum length of the stencil in 1D
  integer, parameter:: MaxIJK = max(nI,nJ,nK)

  ! index ranges for optimized slope limiter calculations
  integer, parameter:: Lo2_=nVar+1, Hi2_=nVar+nVar, Lo3_=Hi2_+1, Hi3_=nVar+Hi2_

  ! local constants
  real, parameter:: cThird = 1./3., cTwoThird = 2./3., cSixth=1./6.
  real, parameter:: c7over12 = 7.0/12.0, c1over12 = 1.0/12.0

  ! Variables for "body" blocks with masked cells
  logical:: UseTrueCell
  logical:: IsTrueCell_I(1-nG:MaxIJK+nG)
  !$omp threadprivate( UseTrueCell, IsTrueCell_I )

  ! FIXME: The following variables should be private!!!
  !$acc declare create(UseTrueCell, IsTrueCell_I)

  ! Low order switch for 1D stencil
  logical:: UseLowOrder_I(1:MaxIJK+1)

  ! variables for the PPM4 limiter
  real:: Cell_I(1-nG:MaxIJK+nG)
  real:: Cell2_I(1-nG:MaxIJK+nG)

  real, allocatable:: FaceL_I(:), FaceR_I(:)
  !$omp threadprivate( Cell_I, Cell2_I, FaceL_I, FaceR_I)
  !$acc declare create(Cell_I, Cell2_I, FaceL_I, FaceR_I)

  real:: LowOrderCrit_I(1:MaxIJK+1)
  !$omp threadprivate( LowOrderCrit_I )

  ! The weight of the four low order polynomials of cweno5
  real, allocatable:: WeightL_II(:,:), WeightR_II(:,:)
  !$omp threadprivate( WeightL_II, WeightR_II)

contains
  !============================================================================
  subroutine clean_mod_face_value
    !--------------------------------------------------------------------------
    if(allocated(iVarLimitRatio_I))  deallocate(iVarLimitRatio_I)
    if(allocated(iRegionLowOrder_I)) deallocate(iRegionLowOrder_I)
    if(allocated(FaceL_I))           deallocate( &
         FaceL_I, FaceR_I, WeightL_II, WeightR_II)

  end subroutine clean_mod_face_value
  !============================================================================
  subroutine read_face_value_param(NameCommand)

    use ModReadParam,  ONLY: read_var, lStringLine
    use ModUtilities,  ONLY: split_string
    use ModVarIndexes, ONLY: NameVar_V

    character(len=*), intent(in) :: NameCommand

    integer :: i, iVar
    character(len=10) :: NameVar_I(nVar)
    character(len=lStringLine) :: NameVarLimitRatio

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_face_value_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(NameCommand)
    case('#RESOLUTIONCHANGE')
       call read_var('UseAccurateResChange',  UseAccurateResChange)
       call read_var('UseTvdResChange',       UseTvdResChange)
       call read_var('BetaLimiterResChange',  BetaLimiterResChange)
       call read_var('nFaceLimiterResChange', nFaceLimiterResChange)

       ! Make sure only one of UseAccurateResChange and UseTvdResChange is true
       if(UseAccurateResChange) UseTvdResChange=.false.
       !$acc update  device(BetaLimiterResChange, nFaceLimiterResChange)
    case('#RESCHANGE')
       call read_var('UseAccurateResChange', UseAccurateResChange)
       if(UseAccurateResChange) UseTvdResChange=.false.

    case('#TVDRESCHANGE')
       call read_var('UseTvdResChange', UseTvdResChange)
       if(UseTvdResChange) UseAccurateResChange = .false.

    case("#LIMITER")
       call read_var('UseLogRhoLimiter',   UseLogRhoLimiter)
       call read_var('UseLogPLimiter',     UseLogPLimiter)
       call read_var('UseRhoRatioLimiter', UseRhoRatioLimiter)
       if(.not. UseRhoRatioLimiter)RETURN
       call read_var('NameVarLimitRatio', NameVarLimitRatio)
       call split_string(NameVarLimitRatio, nVar, NameVar_I, nVarLimitRatio, &
            UseArraySyntaxIn=.true.)
       if(allocated(iVarLimitRatio_I)) deallocate(iVarLimitRatio_I)
       allocate(iVarLimitRatio_I(nVarLimitRatio))
       do i = 1, nVarLimitRatio
          do iVar = 1, nVar
             if(NameVar_V(iVar) == NameVar_I(i))then
                iVarLimitRatio_I(i) = iVar
                EXIT
             end if
          end do
          if(iVar > nVar) call stop_mpi(NameSub// &
               ' could not find NameVarLimitRatio='//NameVar_I(i))
       end do
       !$acc update device(iVarLimitRatio_I)
    case("#LIMITPTOTAL")
       call read_var('DoLimitPtotal', UsePtotalLtd)

    case("#LIMITMOMENTUM")
       call read_var('DoLimitMomentum', DoLimitMomentum)

    case("#LOWORDERREGION")
       call read_var('StringLowOrderRegion', StringLowOrderRegion, &
            IsLowerCase=.true.)
       UseLowOrderRegion = .true.
    case("#ADAPTIVELOWORDER")
       call read_var('UseAdaptiveLowOrder', UseAdaptiveLowOrder)
       if(UseAdaptiveLowOrder) then
          call read_var('nLowOrder', nLowOrder)
          call read_var('pCritLow',  pCritLow)
          call read_var('pCritHigh', pCritHigh)
          call read_var('VelCrit',   VelCrit)
       endif
       if(.not.(nLowOrder==1 .or. nLowOrder==2)) &
            call stop_mpi(NameSub//' nLowOrder should be 1 or 2!!')
       if(pCritLow < (pCritHigh-1e-15)) &
            call stop_mpi(NameSub//' pCritLow should be >= pCritHigh')
    case default
       call stop_mpi(NameSub//' invalid command='//trim(NameCommand))
    end select

    call test_stop(NameSub, DoTest)
  end subroutine read_face_value_param
  !============================================================================
  subroutine correct_monotone_restrict(iBlock)
    !$acc routine vector

    ! Correct the result of the first order monotone restriction that
    ! simply averages the fine cells in the tangential direction by modifying
    ! the coarse ghost cell values such that the slope remains the same
    ! while the cell center is moved from the fine cell distance to the
    ! coarse cell distance.
    !
    ! This operation should be performed at most once per exchange messages.
    ! To avoid multiple modifications in schemes which switch off some blocks,
    ! the correction is not done if any of the finer block neighbors are unused

    use ModSize
    use ModVarIndexes, ONLY: DefaultState_V, nVar, &
         iRho_I, iRhoUx_I, iRhoUy_I, iRhoUz_I
    use ModParallel,   ONLY: DiLevel_EB, jBlock_IEB, jProc_IEB
    use BATL_lib,  ONLY: Unused_BP

    integer, intent(in) :: iBlock

    real, parameter:: cFourThird = 4.0/3.0
    integer:: i, j, k, iVar
    real:: Rho_I(nFluid), InvRho_I(nFluid), Slope1, Slope2

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'correct_monotone_restrict'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(all(DiLevel_EB(:,iBlock) /= -1))RETURN

#ifndef _OPENACC
    if(DoTest)write(*,*)NameSub, ' state before: ',&
         State_VGB(iVarTest, nI:nI+2, jTest, kTest, iBlock)
#endif

    if(.not.DoLimitMomentum)then
       ! Convert momenta to velocities (that will be limited)
       !$acc loop vector collapse(3) private(InvRho_I)
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          InvRho_I = 1.0/State_VGB(iRho_I,i,j,k,iBlock)
          State_VGB(iRhoUx_I,i,j,k,iBlock)=State_VGB(iRhoUx_I,i,j,k,iBlock) &
               *InvRho_I
          State_VGB(iRhoUy_I,i,j,k,iBlock)=State_VGB(iRhoUy_I,i,j,k,iBlock) &
               *InvRho_I
          State_VGB(iRhoUz_I,i,j,k,iBlock)=State_VGB(iRhoUz_I,i,j,k,iBlock) &
               *InvRho_I
       end do; end do; end do
    end if

    ! Extrapolation in log variable seems to produce bad results
    ! if(UseLogLimiter) then
    !   ! Convert to log variables
    !   do iVar = 1, nVar
    !      if(UseLogLimiter_V(iVar)) then
    !         !$ acc loop vector collapse(3) independent
    !         do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
    !            State_VGB(iVar,i,j,k,iBlock) = &
    !                 log(State_VGB(iVar,i,j,k,iBlock))
    !         end do; end do; end do
    !      end if
    !   end do
    ! end if

    if(DiLevel_EB(1,iBlock) == -1)then
       if(       .not.Unused_BP(jBlock_IEB(1,1,iBlock),jProc_IEB(1,1,iBlock)) &
            .and..not.Unused_BP(jBlock_IEB(2,1,iBlock),jProc_IEB(2,1,iBlock)) &
            .and..not.Unused_BP(jBlock_IEB(3,1,iBlock),jProc_IEB(3,1,iBlock)) &
            .and..not.Unused_BP(jBlock_IEB(4,1,iBlock),jProc_IEB(4,1,iBlock)) &
            ) then
          !$acc loop vector collapse(3) private(Slope1, Slope2) independent
          do k = 1, nK; do j = 1, nJ; do iVar = 1, nVar
             ! Slope to first ghost cell over 0.75 dx
             Slope1 = State_VGB(iVar, 0,j,k,iBlock) &
                  -   State_VGB(iVar, 1,j,k,iBlock)
             ! Slope to second ghost cell over 0.5 dx
             Slope2 = State_VGB(iVar,-1,j,k,iBlock) &
                  -   State_VGB(iVar, 0,j,k,iBlock)
             ! Extrapolate to first ghost cell
             State_VGB(iVar,0,j,k,iBlock) = &
                  State_VGB(iVar,1,j,k,iBlock) + cFourThird*Slope1
             ! Extrapolate to second ghost cell
             State_VGB(iVar,-1,j,k,iBlock) = &
                  State_VGB(iVar,0,j,k,iBlock) + 2*Slope2

             if(DefaultState_V(iVar) > 0.0) &
                  State_VGB(iVar,-1:0,j,k,iBlock) = &
                  max(State_VGB(iVar,-1:0,j,k,iBlock), 1e-30)
          end do; end do; end do
       end if
    end if
    if(DiLevel_EB(2,iBlock) == -1)then
       if(       .not.Unused_BP(jBlock_IEB(1,2,iBlock),jProc_IEB(1,2,iBlock)) &
            .and..not.Unused_BP(jBlock_IEB(2,2,iBlock),jProc_IEB(2,2,iBlock)) &
            .and..not.Unused_BP(jBlock_IEB(3,2,iBlock),jProc_IEB(3,2,iBlock)) &
            .and..not.Unused_BP(jBlock_IEB(4,2,iBlock),jProc_IEB(4,2,iBlock)) &
            )then
          !$acc loop vector collapse(3) private(Slope1, Slope2) independent
          do k = 1, nK; do j = 1, nJ; do iVar = 1, nVar
             ! Slope to first ghost cell over 0.75 dx
             Slope1 = State_VGB(iVar,nI+1,j,k,iBlock) &
                  -   State_VGB(iVar,  nI,j,k,iBlock)
             ! Slope to second ghost cell over 0.5 dx
             Slope2 = State_VGB(iVar,nI+2,j,k,iBlock) &
                  -   State_VGB(iVar,nI+1,j,k,iBlock)
             ! Extrapolate to first ghost cell
             State_VGB(iVar,nI+1,j,k,iBlock) = &
                  State_VGB(iVar,nI,j,k,iBlock) + cFourThird*Slope1
             ! Extrapolate to second ghost cell
             State_VGB(iVar,nI+2,j,k,iBlock) = &
                  State_VGB(iVar,nI+1,j,k,iBlock) + 2*Slope2

             if(DefaultState_V(iVar) > 0.0) &
                  State_VGB(iVar,nI+1:nI+2,j,k,iBlock) = &
                  max(State_VGB(iVar,nI+1:nI+2,j,k,iBlock), 1e-30)
          end do; end do; end do
       end if
    end if
    if(DiLevel_EB(3,iBlock) == -1 .and. nJ > 1)then
       if(       .not.Unused_BP(jBlock_IEB(1,3,iBlock),jProc_IEB(1,3,iBlock)) &
            .and..not.Unused_BP(jBlock_IEB(2,3,iBlock),jProc_IEB(2,3,iBlock)) &
            .and..not.Unused_BP(jBlock_IEB(3,3,iBlock),jProc_IEB(3,3,iBlock)) &
            .and..not.Unused_BP(jBlock_IEB(4,3,iBlock),jProc_IEB(4,3,iBlock)) &
            )then
          !$acc loop vector collapse(3) private(Slope1, Slope2) independent
          do k = 1, nK; do i = 1, nI; do iVar = 1, nVar
             ! Slope to first ghost cell over 0.75 dy
             Slope1 = State_VGB(iVar,i, 0,k,iBlock) &
                  -   State_VGB(iVar,i, 1,k,iBlock)
             ! Slope to second ghost cell over 0.5 dy
             Slope2 = State_VGB(iVar,i,-1,k,iBlock) &
                  -   State_VGB(iVar,i, 0,k,iBlock)
             ! Extrapolate to first ghost cell
             State_VGB(iVar,i,0,k,iBlock) = &
                  State_VGB(iVar,i,1,k,iBlock) + cFourThird*Slope1
             ! Extrapolate to second ghost cell
             State_VGB(iVar,i,-1,k,iBlock) = &
                  State_VGB(iVar,i,0,k,iBlock) + 2*Slope2

             if(DefaultState_V(iVar) > 0.0) &
                  State_VGB(iVar,i,-1:0,k,iBlock) = &
                  max(State_VGB(iVar,i,-1:0,k,iBlock), 1e-30)
          end do; end do; end do
       end if
    end if
    if(DiLevel_EB(4,iBlock) == -1 .and. nJ > 1)then
       if(       .not.Unused_BP(jBlock_IEB(1,4,iBlock),jProc_IEB(1,4,iBlock)) &
            .and..not.Unused_BP(jBlock_IEB(2,4,iBlock),jProc_IEB(2,4,iBlock)) &
            .and..not.Unused_BP(jBlock_IEB(3,4,iBlock),jProc_IEB(3,4,iBlock)) &
            .and..not.Unused_BP(jBlock_IEB(4,4,iBlock),jProc_IEB(4,4,iBlock)) &
            )then
          !$acc loop vector collapse(3) private(Slope1, Slope2) independent
          do k = 1, nK; do i = 1, nI; do iVar = 1, nVar
             ! Slope to first ghost cell over 0.75 dy
             Slope1 = State_VGB(iVar,i,nJ+1,k,iBlock) &
                  -   State_VGB(iVar,i,  nJ,k,iBlock)
             ! Slope to second ghost cell over 0.5 dy
             Slope2 = State_VGB(iVar,i,nJ+2,k,iBlock) &
                  -   State_VGB(iVar,i,nJ+1,k,iBlock)
             ! Extrapolate to first ghost cell
             State_VGB(iVar,i,nJ+1,k,iBlock) = &
                  State_VGB(iVar,i,nJ,k,iBlock) + cFourThird*Slope1
             ! Extrapolate to second ghost cell
             State_VGB(iVar,i,nJ+2,k,iBlock) = &
                  State_VGB(iVar,i,nJ+1,k,iBlock) + 2*Slope2

             if(DefaultState_V(iVar) > 0.0) &
                  State_VGB(iVar,i,nJ+1:nJ+2,k,iBlock) = &
                  max(State_VGB(iVar,i,nJ+1:nJ+2,k,iBlock), 1e-30)
          end do; end do; end do
       end if
    end if
    if(DiLevel_EB(5,iBlock) == -1 .and. nK > 1)then
       if(       .not.Unused_BP(jBlock_IEB(1,5,iBlock),jProc_IEB(1,5,iBlock)) &
            .and..not.Unused_BP(jBlock_IEB(2,5,iBlock),jProc_IEB(2,5,iBlock)) &
            .and..not.Unused_BP(jBlock_IEB(3,5,iBlock),jProc_IEB(3,5,iBlock)) &
            .and..not.Unused_BP(jBlock_IEB(4,5,iBlock),jProc_IEB(4,5,iBlock)) &
            )then
          !$acc loop vector collapse(3) private(Slope1, Slope2) independent
          do j = 1, nJ; do i = 1, nI; do iVar = 1, nVar
             ! Slope to first ghost cell over 0.75 dz
             Slope1 = State_VGB(iVar,i,j, 0,iBlock) &
                  -   State_VGB(iVar,i,j, 1,iBlock)
             ! Slope to second ghost cell over 0.5 dz
             Slope2 = State_VGB(iVar,i,j,-1,iBlock) &
                  -   State_VGB(iVar,i,j, 0,iBlock)
             ! Extrapolate to first ghost cell
             State_VGB(iVar,i,j,0,iBlock) = &
                  State_VGB(iVar,i,j,1,iBlock) + cFourThird*Slope1
             ! Extrapolate to second ghost cell
             State_VGB(iVar,i,j,-1,iBlock) = &
                  State_VGB(iVar,i,j,0,iBlock) + 2*Slope2

             if(DefaultState_V(iVar) > 0.0) &
                  State_VGB(iVar,i,j,-1:0,iBlock) = &
                  max(State_VGB(iVar,i,j,-1:0,iBlock), 1e-30)
          end do; end do; end do
       end if
    end if
    if(DiLevel_EB(6,iBlock) == -1 .and. nK > 1)then
       if(       .not.Unused_BP(jBlock_IEB(1,6,iBlock),jProc_IEB(1,6,iBlock)) &
            .and..not.Unused_BP(jBlock_IEB(2,6,iBlock),jProc_IEB(2,6,iBlock)) &
            .and..not.Unused_BP(jBlock_IEB(3,6,iBlock),jProc_IEB(3,6,iBlock)) &
            .and..not.Unused_BP(jBlock_IEB(4,6,iBlock),jProc_IEB(4,6,iBlock)) &
            )then
          !$acc loop vector collapse(3) private(Slope1, Slope2) independent
          do j = 1, nJ; do i = 1, nI; do iVar = 1, nVar
             ! Slope to first ghost cell over 0.75 dz
             Slope1 = State_VGB(iVar,i,j,nK+1,iBlock) &
                  -   State_VGB(iVar,i,j,nK  ,iBlock)
             ! Slope to second ghost cell over 0.5 dz
             Slope2 = State_VGB(iVar,i,j,nK+2,iBlock) &
                  -   State_VGB(iVar,i,j,nK+1,iBlock)
             ! Extrapolate to first ghost cell
             State_VGB(iVar,i,j,nK+1,iBlock) = &
                  State_VGB(iVar,i,j,nK,iBlock) + cFourThird*Slope1
             ! Extrapolate to second ghost cell
             State_VGB(iVar,i,j,nK+2,iBlock) = &
                  State_VGB(iVar,i,j,nK+1,iBlock) + 2*Slope2

             if(DefaultState_V(iVar) > 0.0) &
                  State_VGB(iVar,i,j,nK+1:nK+2,iBlock) = &
                  max(State_VGB(iVar,i,j,nK+1:nK+2,iBlock), 1e-30)
          end do; end do; end do
       end if
    end if

    ! if(UseLogLimiter) then
    !   ! Convert back from log variables
    !   do iVar = 1, nVar
    !      if(UseLogLimiter_V(iVar)) then
    !         !$ acc loop vector collapse(3) independent
    !         do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
    !            State_VGB(iVar,i,j,k,iBlock) = &
    !                 exp(State_VGB(iVar,i,j,k,iBlock))
    !         end do; end do; end do
    !      end if
    !   end do
    ! end if

    if(.not.DoLimitMomentum)then
       ! Convert velocities back to momenta
       !$acc loop vector collapse(3) private(Rho_I)
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          Rho_I = State_VGB(iRho_I,i,j,k,iBlock)
          State_VGB(iRhoUx_I,i,j,k,iBlock)=State_VGB(iRhoUx_I,i,j,k,iBlock) &
               * Rho_I
          State_VGB(iRhoUy_I,i,j,k,iBlock)=State_VGB(iRhoUy_I,i,j,k,iBlock) &
               * Rho_I
          State_VGB(iRhoUz_I,i,j,k,iBlock)=State_VGB(iRhoUz_I,i,j,k,iBlock) &
               * Rho_I
       end do; end do; end do
    end if

#ifndef _OPENACC
    if(DoTest)write(*,*)NameSub, ' state after: ',&
         State_VGB(iVarTest, nI:nI+2, jTest, kTest, iBlock)
#endif

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine correct_monotone_restrict
  !============================================================================
  subroutine get_log_limiter_var

    use ModMain, ONLY: nOrder
    use ModAdvance, ONLY: UseElectronPressure, UseAnisoPe, UseAnisoPressure

    integer:: iFluid
    !--------------------------------------------------------------------------

    UseLogLimiter   = nOrder > 1 .and. (UseLogRhoLimiter .or. UseLogPLimiter)
    UseLogLimiter_V = .false.
    if(UseLogLimiter)then
       if(UseLogRhoLimiter)then
          do iFluid = 1, nFluid
             UseLogLimiter_V(iRho_I(iFluid)) = .true.
          end do
       end if
       if(UseLogPLimiter)then
          do iFluid = 1, nFluid
             UseLogLimiter_V(iP_I(iFluid))   = .true.
          end do
          if(UseAnisoPressure)then
             do iFluid = 1, nIonFluid
                UseLogLimiter_V(iPparIon_I(iFluid)) = .true.
             end do
          end if
          if(UseElectronPressure) UseLogLimiter_V(Pe_) = .true.
          if(UseAnisoPe)          UseLogLimiter_V(Pepar_) = .true.
       end if
    end if

    !$acc update device(UseLogLimiter_V, UseLogLimiter)
  end subroutine get_log_limiter_var
  !============================================================================
  subroutine get_face_accurate3d(iSideIn,  iBlock)

    integer, intent(in):: iSideIn, iBlock
    integer:: i, j, k
    !--------------------------------------------------------------------------
    select case(iSideIn)
    case(1)
       do k = 1, nK, 2; do j = 1, nJ, 2
          if(  all(Used_GB(-1:2,j:j+1,k:k+1,iBlock)) .and. &
               all(Used_GB(0,j-2:j+3,k-2:k+3,iBlock)) ) then
             call accurate_reschange3d(&
                  Coarse2_V    = Primitive_VG(:,-1,j,k)            ,&
                  Coarse1_VII  = Primitive_VG(:, 0,j-2:j+3,k-2:k+3),&
                  Fine1_VII    = Primitive_VG(:, 1,j:j+1,k:k+1)    ,&
                  Fine2_VII    = Primitive_VG(:, 2,j:j+1,k:k+1)    ,&
                  CoarseToFineF_VII= LeftState_VX(:,1,j:j+1,k:k+1) ,&
                  FineToCoarseF_VII=RightState_VX(:,1,j:j+1,k:k+1) ,&
                  FineF_VII        = LeftState_VX(:,2,j:j+1,k:k+1))
          else
             call tvd_reschange_body(                                 &
                  Coarse2_V    =    Primitive_VG(:,-1,j,k)           ,&
                  Coarse1_V    =    Primitive_VG(:, 0,j,k)           ,&
                  Fine1_VII    =    Primitive_VG(:, 1,j:j+1,k:k+1)   ,&
                  Fine2_VII    =    Primitive_VG(:, 2,j:j+1,k:k+1)   ,&
                  CoarseToFineF_VII= LeftState_VX(:,1,j:j+1,k:k+1)   ,&
                  FineToCoarseF_VII=RightState_VX(:,1,j:j+1,k:k+1)   ,&
                  FineF_VII        = LeftState_VX(:,2,j:j+1,k:k+1)   ,&
                  IsTrueCoarse2    = Used_GB(-1,j,k,iBlock)        ,&
                  IsTrueCoarse1    = Used_GB( 0,j,k,iBlock)        ,&
                  IsTrueFine1  =all(Used_GB( 1,j:j+1,k:k+1,iBlock)),&
                  IsTrueFine2_II   = Used_GB( 2,j:j+1,k:k+1,iBlock))
          end if
       end do; end do
    case(2)
       do k = 1, nK, 2; do j = 1, nJ, 2
          if(  all(Used_GB(nI-1:nI+2,j:j+1,k:k+1,iBlock)).and. &
               all(Used_GB(nI+1,j-2:j+3,k-2:k+3,iBlock)) ) then
             call accurate_reschange3d( &
                  Coarse2_V    = Primitive_VG(:,nI+2,j,k)            ,&
                  Coarse1_VII  = Primitive_VG(:,nI+1,j-2:j+3,k-2:k+3),&
                  Fine1_VII    = Primitive_VG(:,nI,j:j+1,k:k+1)      ,&
                  Fine2_VII    = Primitive_VG(:,nI-1,j:j+1,k:k+1)    ,&
                  CoarseToFineF_VII=RightState_VX(:,nI+1,j:j+1,k:k+1),&
                  FineToCoarseF_VII=LeftState_VX(:,nI+1,j:j+1,k:k+1) ,&
                  FineF_VII       =RightState_VX(:,nI,j:j+1,k:k+1))
          else
             call tvd_reschange_body( &
                  Coarse2_V    =    Primitive_VG(:,nI+2,j,k)         ,&
                  Coarse1_V    =    Primitive_VG(:,nI+1,j,k)         ,&
                  Fine1_VII    =    Primitive_VG(:,nI,j:j+1,k:k+1)   ,&
                  Fine2_VII    =    Primitive_VG(:,nI-1,j:j+1,k:k+1) ,&
                  CoarseToFineF_VII=RightState_VX(:,nI+1,j:j+1,k:k+1),&
                  FineToCoarseF_VII=LeftState_VX(:,nI+1,j:j+1,k:k+1) ,&
                  FineF_VII        =RightState_VX(:,nI,j:j+1,k:k+1)  ,&
                  IsTrueCoarse2    = Used_GB(nI+2,j,k,iBlock)        ,&
                  IsTrueCoarse1    = Used_GB(nI+1,j,k,iBlock)        ,&
                  IsTrueFine1      =all(Used_GB(nI,j:j+1,k:k+1,iBlock)),&
                  IsTrueFine2_II   =Used_GB(nI-1,j:j+1,k:k+1,iBlock))
          end if
       end do; end do
    case(3)
       do k = 1, nK, 2; do i = 1, nI, 2
          if(  all(Used_GB(i:i+1,-1:2,k:k+1,iBlock)) .and. &
               all(Used_GB(i-2:i+3,0,k-2:k+3,iBlock)) ) then
             call accurate_reschange3d( &
                  Coarse2_V    =    Primitive_VG(:,i,-1,k)           ,&
                  Coarse1_VII  =    Primitive_VG(:,i-2:i+3,0,k-2:k+3),&
                  Fine1_VII    =    Primitive_VG(:,i:i+1, 1,k:k+1)   ,&
                  Fine2_VII    =    Primitive_VG(:,i:i+1, 2,k:k+1)   ,&
                  CoarseToFineF_VII= LeftState_VY(:,i:i+1,1,k:k+1)   ,&
                  FineToCoarseF_VII=RightState_VY(:,i:i+1,1,k:k+1)   ,&
                  FineF_VII       = LeftState_VY(:,i:i+1,2,k:k+1))
          else
             call tvd_reschange_body(&
                  Coarse2_V    =    Primitive_VG(:,i,-1,k)           ,&
                  Coarse1_V    =    Primitive_VG(:,i, 0,k)           ,&
                  Fine1_VII    =    Primitive_VG(:,i:i+1, 1,k:k+1)   ,&
                  Fine2_VII    =    Primitive_VG(:,i:i+1, 2,k:k+1)   ,&
                  CoarseToFineF_VII= LeftState_VY(:,i:i+1,1,k:k+1)   ,&
                  FineToCoarseF_VII=RightState_VY(:,i:i+1,1,k:k+1)   ,&
                  FineF_VII        = LeftState_VY(:,i:i+1,2,k:k+1)   ,&
                  IsTrueCoarse2    = Used_GB(i,-1,k,iBlock)        ,&
                  IsTrueCoarse1    = Used_GB(i, 0,k,iBlock)        ,&
                  IsTrueFine1  =all(Used_GB(i:i+1, 1,k:k+1,iBlock)),&
                  IsTrueFine2_II      =Used_GB(i:i+1, 2,k:k+1,iBlock))
          end if
       end do; end do
    case(4)
       do k = 1, nK, 2; do i = 1, nI, 2
          if(  all(Used_GB(i:i+1,nJ-1:nJ+2,k:k+1,iBlock)) .and. &
               all(Used_GB(i-2:i+3,nJ+1,k-2:k+3,iBlock)) ) then
             call accurate_reschange3d(&
                  Coarse2_V    = Primitive_VG(:,i,nJ+2,k)            ,&
                  Coarse1_VII  = Primitive_VG(:,i-2:i+3,nJ+1,k-2:k+3),&
                  Fine1_VII    = Primitive_VG(:,i:i+1, nJ,k:k+1)     ,&
                  Fine2_VII    = Primitive_VG(:,i:i+1, nJ-1,k:k+1)   ,&
                  CoarseToFineF_VII=RightState_VY(:,i:i+1,nJ+1,k:k+1),&
                  FineToCoarseF_VII=LeftState_VY(:,i:i+1,nJ+1,k:k+1) ,&
                  FineF_VII        =RightState_VY(:,i:i+1,nJ,k:k+1))
          else
             call tvd_reschange_body(&
                  Coarse2_V    = Primitive_VG(:,i,nJ+2,k)            ,&
                  Coarse1_V    = Primitive_VG(:,i,nJ+1,k)            ,&
                  Fine1_VII    = Primitive_VG(:,i:i+1, nJ,k:k+1)     ,&
                  Fine2_VII    = Primitive_VG(:,i:i+1, nJ-1,k:k+1)   ,&
                  CoarseToFineF_VII=RightState_VY(:,i:i+1,nJ+1,k:k+1),&
                  FineToCoarseF_VII=LeftState_VY(:,i:i+1,nJ+1,k:k+1) ,&
                  FineF_VII        = RightState_VY(:,i:i+1,nJ,k:k+1) ,&
                  IsTrueCoarse2    = Used_GB(i,nJ+2,k,iBlock)      ,&
                  IsTrueCoarse1    = Used_GB(i,nJ+1,k,iBlock)      ,&
                  IsTrueFine1  =all(Used_GB(i:i+1,nJ,k:k+1,iBlock)),&
                  IsTrueFine2_II      =Used_GB(i:i+1,nJ-1,k:k+1,iBlock))
          end if
       end do; end do
    case(5)
       do j = 1, nJ, 2; do i = 1, nI, 2
          if(  all(Used_GB(i:i+1,j:j+1,-1:2,iBlock)) .and. &
               all(Used_GB(i-2:i+3,j-2:j+3,0,iBlock)) ) then
             call accurate_reschange3d(&
                  Coarse2_V    =    Primitive_VG(:,i,j,-1)           ,&
                  Coarse1_VII  =    Primitive_VG(:,i-2:i+3,j-2:j+3,0),&
                  Fine1_VII    =    Primitive_VG(:,i:i+1,j:j+1, 1)   ,&
                  Fine2_VII    =    Primitive_VG(:,i:i+1,j:j+1, 2)   ,&
                  CoarseToFineF_VII= LeftState_VZ(:,i:i+1,j:j+1,1)   ,&
                  FineToCoarseF_VII=RightState_VZ(:,i:i+1,j:j+1,1)   ,&
                  FineF_VII        = LeftState_VZ(:,i:i+1,j:j+1,2))
          else
             call tvd_reschange_body(&
                  Coarse2_V    =    Primitive_VG(:,i,j,-1)           ,&
                  Coarse1_V    =    Primitive_VG(:,i,j, 0)           ,&
                  Fine1_VII    =    Primitive_VG(:,i:i+1,j:j+1, 1)   ,&
                  Fine2_VII    =    Primitive_VG(:,i:i+1,j:j+1, 2)   ,&
                  CoarseToFineF_VII= LeftState_VZ(:,i:i+1,j:j+1,1)   ,&
                  FineToCoarseF_VII=RightState_VZ(:,i:i+1,j:j+1,1)   ,&
                  FineF_VII        = LeftState_VZ(:,i:i+1,j:j+1,2)   ,&
                  IsTrueCoarse2    = Used_GB(i,j,-1,iBlock)        ,&
                  IsTrueCoarse1    = Used_GB(i,j, 0,iBlock)        ,&
                  IsTrueFine1 =all(Used_GB(i:i+1,j:j+1, 1,iBlock)),&
                  IsTrueFine2_II      =Used_GB(i:i+1,j:j+1, 2,iBlock))
          end if
       end do; end do
    case(6)
       do j = 1, nJ, 2; do i = 1, nI, 2
          if(  all(Used_GB(i:i+1,j:j+1,nK-1:nK+2,iBlock)) .and. &
               all(Used_GB(i-2:i+3,j-2:j+3,nK+1,iBlock)) ) then
             call accurate_reschange3d(&
                  Coarse2_V    = Primitive_VG(:,i,j,nK+2)         ,   &
                  Coarse1_VII  = Primitive_VG(:,i-2:i+3,j-2:j+3,nK+1),&
                  Fine1_VII    = Primitive_VG(:,i:i+1,j:j+1, nK)  ,   &
                  Fine2_VII    = Primitive_VG(:,i:i+1,j:j+1, nK-1),   &
                  CoarseToFineF_VII=RightState_VZ(:,i:i+1,j:j+1,nK+1),&
                  FineToCoarseF_VII=LeftState_VZ(:,i:i+1,j:j+1,nK+1) ,&
                  FineF_VII    = RightState_VZ(:,i:i+1,j:j+1,nK))
          else
             call tvd_reschange_body(&
                  Coarse2_V    =    Primitive_VG(:,i,j,nK+2)         ,&
                  Coarse1_V    =    Primitive_VG(:,i,j,nK+1)         ,&
                  Fine1_VII    =    Primitive_VG(:,i:i+1,j:j+1, nK)  ,&
                  Fine2_VII    =    Primitive_VG(:,i:i+1,j:j+1, nK-1),&
                  CoarseToFineF_VII=RightState_VZ(:,i:i+1,j:j+1,nK+1),&
                  FineToCoarseF_VII=LeftState_VZ(:,i:i+1,j:j+1,nK+1) ,&
                  FineF_VII        =RightState_VZ(:,i:i+1,j:j+1,nK)  ,&
                  IsTrueCoarse2    =Used_GB(i,j,nK+2,iBlock)       ,&
                  IsTrueCoarse1    =Used_GB(i,j,nK+1,iBlock)       ,&
                  IsTrueFine1  =all(Used_GB(i:i+1,j:j+1,nK,iBlock)),&
                  IsTrueFine2_II  =Used_GB(i:i+1,j:j+1,nK-1,iBlock))
          end if
       end do; end do
    end select
  end subroutine get_face_accurate3d
  !============================================================================
  subroutine get_face_accurate1d(iSideIn, iBlock)

    integer, intent(in):: iSideIn, iBlock
    !--------------------------------------------------------------------------
    select case(iSideIn)
    case(1)
       call accurate_reschange1d(&
            Coarse2_V       = Primitive_VG(:,-1,1,1)     ,&
            Coarse1_V       = Primitive_VG(:, 0,1,1)     ,&
            Fine1_V         = Primitive_VG(:, 1,1,1)     ,&
            Fine2_V         = Primitive_VG(:, 2,1,1)     ,&
            CoarseToFineF_V = LeftState_VX(:, 1,1,1)     ,&
            FineToCoarseF_V =RightState_VX(:, 1,1,1)     ,&
            FineF_V         = LeftState_VX(:, 2,1,1))
    case(2)
       call accurate_reschange1d(&
            Coarse2_V       = Primitive_VG(:,nI+2,1,1)   ,&
            Coarse1_V       = Primitive_VG(:,nI+1,1,1)   ,&
            Fine1_V         = Primitive_VG(:,nI  ,1,1)   ,&
            Fine2_V         = Primitive_VG(:,nI-1,1,1)   ,&
            CoarseToFineF_V =RightState_VX(:,nI+1,1,1)   ,&
            FineToCoarseF_V = LeftState_VX(:,nI+1,1,1)   ,&
            FineF_V         =RightState_VX(:,nI  ,1,1))
    end select

  end subroutine get_face_accurate1d
  !============================================================================
  subroutine get_face_accurate2d(iSideIn, iBlock)

    integer, intent(in):: iSideIn, iBlock
    integer:: i, j
    !--------------------------------------------------------------------------
    select case(iSideIn)
    case(1)
       do j = 1, nJ, 2
          call accurate_reschange2d( &
               Coarse2_V       = Primitive_VG(:,-1,j,1), &
               Coarse1_VI      = Primitive_VG(:, 0,j-2:j+3,1), &
               Fine1_VI        = Primitive_VG(:, 1,j:j+1,1), &
               Fine2_VI        = Primitive_VG(:, 2,j:j+1,1), &
               CoarseToFineF_VI= LeftState_VX(:, 1,j:j+1,1), &
               FineToCoarseF_VI=RightState_VX(:, 1,j:j+1,1), &
               FineF_VI        = LeftState_VX(:, 2,j:j+1,1))
       end do
    case(2)
       do j = 1, nJ, 2
          call accurate_reschange2d( &
               Coarse2_V       = Primitive_VG(:,nI+2,j,1)       ,&
               Coarse1_VI      = Primitive_VG(:,nI+1,j-2:j+3,1) ,&
               Fine1_VI        = Primitive_VG(:,nI  ,j:j+1,1)   ,&
               Fine2_VI        = Primitive_VG(:,nI-1,j:j+1,1)   ,&
               CoarseToFineF_VI=RightState_VX(:,nI+1,j:j+1,1)   ,&
               FineToCoarseF_VI= LeftState_VX(:,nI+1,j:j+1,1)   ,&
               FineF_VI        =RightState_VX(:,nI  ,j:j+1,1))
       end do
    case(3)
       do i = 1, nI, 2
          call accurate_reschange2d( &
               Coarse2_V       = Primitive_VG(:,i,-1,1)         ,&
               Coarse1_VI      = Primitive_VG(:,i-2:i+3,0,1)    ,&
               Fine1_VI        = Primitive_VG(:,i:i+1,1,1)      ,&
               Fine2_VI        = Primitive_VG(:,i:i+1,2,1)      ,&
               CoarseToFineF_VI= LeftState_VY(:,i:i+1,1,1)      ,&
               FineToCoarseF_VI=RightState_VY(:,i:i+1,1,1)      ,&
               FineF_VI        = LeftState_VY(:,i:i+1,2,1))
       end do
    case(4)
       do i = 1, nI, 2
          call accurate_reschange2d( &
               Coarse2_V       = Primitive_VG(:,i,nJ+2,1)       ,&
               Coarse1_VI      = Primitive_VG(:,i-2:i+3,nJ+1,1) ,&
               Fine1_VI        = Primitive_VG(:,i:i+1,nJ,1)     ,&
               Fine2_VI        = Primitive_VG(:,i:i+1,nJ-1,1)   ,&
               CoarseToFineF_VI=RightState_VY(:,i:i+1,nJ+1,1)   ,&
               FineToCoarseF_VI= LeftState_VY(:,i:i+1,nJ+1,1)   ,&
               FineF_VI        =RightState_VY(:,i:i+1,nJ,1))
       end do
    end select
  end subroutine get_face_accurate2d
  !============================================================================
  subroutine get_face_tvd(iSideIn, iBlock)

    integer,intent(in)::iSideIn, iBlock
    integer:: i, j, k
    !--------------------------------------------------------------------------
    select case(iSideIn)
    case(1)
       do k = 1, nK, 2; do j = 1, nJ, 2
          if(.not.all(Used_GB(-1:2,j:j+1,k:k+1,iBlock)))then
             call tvd_reschange_body(&
                  Coarse2_V    =    Primitive_VG(:,-1,j,k)           ,&
                  Coarse1_V    =    Primitive_VG(:, 0,j,k)           ,&
                  Fine1_VII    =    Primitive_VG(:, 1,j:j+1,k:k+1)   ,&
                  Fine2_VII    =    Primitive_VG(:, 2,j:j+1,k:k+1)   ,&
                  CoarseToFineF_VII= LeftState_VX(:,1,j:j+1,k:k+1)   ,&
                  FineToCoarseF_VII=RightState_VX(:,1,j:j+1,k:k+1)   ,&
                  FineF_VII        = LeftState_VX(:,2,j:j+1,k:k+1)   ,&
                  IsTrueCoarse2    = Used_GB(-1,j,k,iBlock)        ,&
                  IsTrueCoarse1    = Used_GB( 0,j,k,iBlock)        ,&
                  IsTrueFine1  =all(Used_GB( 1,j:j+1,k:k+1,iBlock)),&
                  IsTrueFine2_II      =Used_GB( 2,j:j+1,k:k+1,iBlock))
          else
             call tvd_reschange(&
                  Coarse2_V    =    Primitive_VG(:,-1,j,k)           ,&
                  Coarse1_V    =    Primitive_VG(:, 0,j,k)           ,&
                  Fine1_VII    =    Primitive_VG(:, 1,j:j+1,k:k+1)   ,&
                  Fine2_VII    =    Primitive_VG(:, 2,j:j+1,k:k+1)   ,&
                  CoarseToFineF_VII= LeftState_VX(:,1,j:j+1,k:k+1)   ,&
                  FineToCoarseF_VII=RightState_VX(:,1,j:j+1,k:k+1)   ,&
                  FineF_VII        = LeftState_VX(:,2,j:j+1,k:k+1))
          end if
       end do; end do
    case(2)
       do k = 1, nK, 2; do j = 1, nJ, 2
          if(.not.all(Used_GB(nI-1:nI+2,j:j+1,k:k+1,iBlock)))then
             call tvd_reschange_body(&
                  Coarse2_V    =    Primitive_VG(:,nI+2,j,k)         ,&
                  Coarse1_V    =    Primitive_VG(:, nI+1,j,k)        ,&
                  Fine1_VII    =    Primitive_VG(:, nI,j:j+1,k:k+1)  ,&
                  Fine2_VII    =    Primitive_VG(:, nI-1,j:j+1,k:k+1),&
                  CoarseToFineF_VII=RightState_VX(:,nI+1,j:j+1,k:k+1),&
                  FineToCoarseF_VII=LeftState_VX(:,nI+1,j:j+1,k:k+1) ,&
                  FineF_VII        =RightState_VX(:,nI,j:j+1,k:k+1)  ,&
                  IsTrueCoarse2    = Used_GB(nI+2,j,k,iBlock)      ,&
                  IsTrueCoarse1    = Used_GB(nI+1,j,k,iBlock)      ,&
                  IsTrueFine1  =all(Used_GB(nI,j:j+1,k:k+1,iBlock)),&
                  IsTrueFine2_II      =Used_GB(nI-1,j:j+1,k:k+1,iBlock))
          else
             call tvd_reschange(&
                  Coarse2_V    =    Primitive_VG(:,nI+2,j,k)         ,&
                  Coarse1_V    =    Primitive_VG(:, nI+1,j,k)        ,&
                  Fine1_VII    =    Primitive_VG(:, nI,j:j+1,k:k+1)  ,&
                  Fine2_VII    =    Primitive_VG(:, nI-1,j:j+1,k:k+1),&
                  CoarseToFineF_VII=RightState_VX(:,nI+1,j:j+1,k:k+1),&
                  FineToCoarseF_VII=LeftState_VX(:,nI+1,j:j+1,k:k+1) ,&
                  FineF_VII       =RightState_VX(:,nI,j:j+1,k:k+1))
          end if
       end do; end do
    case(3)
       do k = 1, nK, 2; do i = 1, nI, 2
          if(.not.all(Used_GB(i:i+1,-1:2,k:k+1,iBlock)))then
             call tvd_reschange_body(&
                  Coarse2_V    =    Primitive_VG(:,i,-1,k)           ,&
                  Coarse1_V    =    Primitive_VG(:,i, 0,k)           ,&
                  Fine1_VII    =    Primitive_VG(:,i:i+1, 1,k:k+1)   ,&
                  Fine2_VII    =    Primitive_VG(:,i:i+1, 2,k:k+1)   ,&
                  CoarseToFineF_VII= LeftState_VY(:,i:i+1,1,k:k+1)   ,&
                  FineToCoarseF_VII=RightState_VY(:,i:i+1,1,k:k+1)   ,&
                  FineF_VII        = LeftState_VY(:,i:i+1,2,k:k+1)   ,&
                  IsTrueCoarse2    = Used_GB(i,-1,k,iBlock)        ,&
                  IsTrueCoarse1    = Used_GB(i, 0,k,iBlock)        ,&
                  IsTrueFine1  =all(Used_GB(i:i+1, 1,k:k+1,iBlock)),&
                  IsTrueFine2_II      =Used_GB(i:i+1, 2,k:k+1,iBlock))
          else
             call tvd_reschange(&
                  Coarse2_V    =    Primitive_VG(:,i,-1,k)           ,&
                  Coarse1_V    =    Primitive_VG(:,i, 0,k)           ,&
                  Fine1_VII    =    Primitive_VG(:,i:i+1, 1,k:k+1)   ,&
                  Fine2_VII    =    Primitive_VG(:,i:i+1, 2,k:k+1)   ,&
                  CoarseToFineF_VII= LeftState_VY(:,i:i+1,1,k:k+1)   ,&
                  FineToCoarseF_VII=RightState_VY(:,i:i+1,1,k:k+1)   ,&
                  FineF_VII       = LeftState_VY(:,i:i+1,2,k:k+1))
          end if
       end do; end do
    case(4)
       do k = 1, nK, 2; do i = 1, nI, 2
          if(.not.all(Used_GB(i:i+1,nJ-1:nJ+2,k:k+1,iBlock)))then
             call tvd_reschange_body(&
                  Coarse2_V    =    Primitive_VG(:,i,nJ+2,k)         ,&
                  Coarse1_V    =    Primitive_VG(:,i,nJ+1,k)         ,&
                  Fine1_VII    =    Primitive_VG(:,i:i+1, nJ,k:k+1)  ,&
                  Fine2_VII    =    Primitive_VG(:,i:i+1, nJ-1,k:k+1),&
                  CoarseToFineF_VII=RightState_VY(:,i:i+1,nJ+1,k:k+1),&
                  FineToCoarseF_VII=LeftState_VY(:,i:i+1,nJ+1,k:k+1) ,&
                  FineF_VII        =RightState_VY(:,i:i+1,nJ,k:k+1)  ,&
                  IsTrueCoarse2    = Used_GB(i,nJ+2,k,iBlock)      ,&
                  IsTrueCoarse1    = Used_GB(i,nJ+1,k,iBlock)      ,&
                  IsTrueFine1  =all(Used_GB(i:i+1,nJ,k:k+1,iBlock)),&
                  IsTrueFine2_II      =Used_GB(i:i+1,nJ-1,k:k+1,iBlock))
          else
             call tvd_reschange(&
                  Coarse2_V    =    Primitive_VG(:,i,nJ+2,k)         ,&
                  Coarse1_V    =    Primitive_VG(:,i,nJ+1,k)         ,&
                  Fine1_VII    =    Primitive_VG(:,i:i+1, nJ,k:k+1)  ,&
                  Fine2_VII    =    Primitive_VG(:,i:i+1, nJ-1,k:k+1),&
                  CoarseToFineF_VII=RightState_VY(:,i:i+1,nJ+1,k:k+1),&
                  FineToCoarseF_VII=LeftState_VY(:,i:i+1,nJ+1,k:k+1) ,&
                  FineF_VII        =RightState_VY(:,i:i+1,nJ,k:k+1))
          end if
       end do; end do
    case(5)
       do j = 1, nJ, 2; do i = 1, nI, 2
          if(.not.all(Used_GB(i:i+1,j:j+1,-1:2,iBlock)))then
             call tvd_reschange_body(&
                  Coarse2_V    =    Primitive_VG(:,i,j,-1)           ,&
                  Coarse1_V    =    Primitive_VG(:,i,j, 0)           ,&
                  Fine1_VII    =    Primitive_VG(:,i:i+1,j:j+1, 1)   ,&
                  Fine2_VII    =    Primitive_VG(:,i:i+1,j:j+1, 2)   ,&
                  CoarseToFineF_VII= LeftState_VZ(:,i:i+1,j:j+1,1)   ,&
                  FineToCoarseF_VII=RightState_VZ(:,i:i+1,j:j+1,1)   ,&
                  FineF_VII        = LeftState_VZ(:,i:i+1,j:j+1,2)   ,&
                  IsTrueCoarse2    = Used_GB(i,j,-1,iBlock)        ,&
                  IsTrueCoarse1    = Used_GB(i,j, 0,iBlock)        ,&
                  IsTrueFine1 =all(Used_GB(i:i+1,j:j+1, 1,iBlock)),&
                  IsTrueFine2_II      =Used_GB(i:i+1,j:j+1, 2,iBlock))
          else
             call tvd_reschange(&
                  Coarse2_V    =    Primitive_VG(:,i,j,-1)           ,&
                  Coarse1_V    =    Primitive_VG(:,i,j, 0)           ,&
                  Fine1_VII    =    Primitive_VG(:,i:i+1,j:j+1, 1)   ,&
                  Fine2_VII    =    Primitive_VG(:,i:i+1,j:j+1, 2)   ,&
                  CoarseToFineF_VII= LeftState_VZ(:,i:i+1,j:j+1,1)   ,&
                  FineToCoarseF_VII=RightState_VZ(:,i:i+1,j:j+1,1)   ,&
                  FineF_VII        = LeftState_VZ(:,i:i+1,j:j+1,2))
          end if
       end do; end do
    case(6)
       do j = 1, nJ, 2; do i = 1, nI, 2
          if(.not.all(Used_GB(i:i+1,j:j+1,nK-1:nK+2,iBlock)))then
             call tvd_reschange_body(&
                  Coarse2_V    =    Primitive_VG(:,i,j,nK+2)         ,&
                  Coarse1_V    =    Primitive_VG(:,i,j,nK+1)         ,&
                  Fine1_VII    =    Primitive_VG(:,i:i+1,j:j+1, nK)  ,&
                  Fine2_VII    =    Primitive_VG(:,i:i+1,j:j+1, nK-1),&
                  CoarseToFineF_VII=RightState_VZ(:,i:i+1,j:j+1,nK+1),&
                  FineToCoarseF_VII=LeftState_VZ(:,i:i+1,j:j+1,nK+1) ,&
                  FineF_VII        =RightState_VZ(:,i:i+1,j:j+1,nK)  ,&
                  IsTrueCoarse2    =Used_GB(i,j,nK+2,iBlock)       ,&
                  IsTrueCoarse1    =Used_GB(i,j,nK+1,iBlock)       ,&
                  IsTrueFine1  =all(Used_GB(i:i+1,j:j+1,nK,iBlock)),&
                  IsTrueFine2_II  =Used_GB(i:i+1,j:j+1,nK-1,iBlock))
          else
             call tvd_reschange(&
                  Coarse2_V    =    Primitive_VG(:,i,j,nK+2)         ,&
                  Coarse1_V    =    Primitive_VG(:,i,j,nK+1)         ,&
                  Fine1_VII    =    Primitive_VG(:,i:i+1,j:j+1, nK)  ,&
                  Fine2_VII    =    Primitive_VG(:,i:i+1,j:j+1, nK-1),&
                  CoarseToFineF_VII=RightState_VZ(:,i:i+1,j:j+1,nK+1),&
                  FineToCoarseF_VII=LeftState_VZ(:,i:i+1,j:j+1,nK+1) ,&
                  FineF_VII        =RightState_VZ(:,i:i+1,j:j+1,nK))
          end if
       end do; end do
    end select

  end subroutine get_face_tvd
  !============================================================================
  subroutine calc_face_value(iBlock, DoResChangeOnly, DoMonotoneRestrict)

    ! Calculate right and left face values (primitive variables)
    ! LeftState_VX.. RightState_VZfor block iBlock from
    ! the cell centered State_VGB.
    !
    ! If DoResChangeOnly is true, only facevalues next to a coarser
    ! neighbor block are calculated.
    !
    ! If the optional DoMonotoneRestrict is present (and false) then
    ! do not call correct_monotone_restrict. This is only used from
    ! some user plot functions. By default correct_monotone_restrict
    ! is called for TVD/Accurate reschange true.

    use ModMultiFluid, ONLY: nIonFluid, iRho, iUx, iUz, iUx_I, iUz_I
    use ModMain, ONLY: nOrder, nOrderProlong, UseB0, &
         UseConstrainB, nIFace, nJFace, nKFace, &
         iMinFace, iMaxFace, jMinFace, jMaxFace, kMinFace, kMaxFace, &
         iMinFace2, iMaxFace2, jMinFace2, jMaxFace2, kMinFace2, kMaxFace2, &
         UseHighResChange
    use ModPhysics, ONLY: GammaWave
    use ModB0
    use ModAdvance, ONLY: UseElectronPressure, UseWavePressure, &
         LowOrderCrit_XB, LowOrderCrit_YB, LowOrderCrit_ZB
    use ModParallel, ONLY : DiLevel_EB
    use ModViscosity, ONLY: UseArtificialVisco
    use ModSaMhd,  ONLY: UseSaMhd, correct_samhd_face_value

    integer, intent(in):: iBlock
    logical, intent(in):: DoResChangeOnly
    logical, intent(in), optional:: DoMonotoneRestrict

    integer:: i, j, k, iSide

    ! Number of cells needed to get the face values
    integer:: nStencil

    integer:: iVarSmoothLast, iVarSmooth

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_face_value'
    !--------------------------------------------------------------------------
    if(.not. DoResChangeOnly)then
       call test_start(NameSub, DoTest, iBlock)
    else
       DoTest=.false.
    end if

    if(DoTest)then
       write(*,*) NameSub,' starting with DoResChangeOnly=', DoResChangeOnly
       if(iDimTest==0 .or. iDimTest==1)then
          write(*,*)'TestVar(iTest-nG:iTest+nG)=', &
               State_VGB(iVarTest,iTest-nG:iTest+nG,jTest,kTest,iBlockTest)
          if(.not.all(Used_GB(iTest-nG:iTest+nG,jTest,kTest,iBlockTest))) &
               write(*,*)'Used_GB(iTest-nG:iTest+nG)=',&
               Used_GB(iTest-nG:iTest+nG,jTest,kTest,iBlockTest)
       end if
       if(nDim > 1 .and. (iDimTest==0 .or. iDimTest==2))then
          write(*,*)'TestVar(jTest-nG:jTest+nG)=', &
               State_VGB(iVarTest,iTest,jTest-nG:jTest+nG,kTest,iBlockTest)
          if(.not.all(Used_GB(iTest,jTest-nG:jTest+nG,kTest,iBlockTest))) &
               write(*,*)'Used_GB(jTest-nG:jTest+nG)=',&
               Used_GB(iTest,jTest-nG:jTest+nG,kTest,iBlockTest)
       end if
       if(nDim > 2 .and. (iDimTest==0 .or. iDimTest==3)) then
          write(*,*)'TestVar(kTest-nG:kTest+nG)=', &
               State_VGB(iVarTest,iTest,jTest,kTest-nG:kTest+nG,iBlockTest)
          if(.not.all(Used_GB(iTest,jTest,kTest-nG:kTest+nG,iBlockTest))) &
               write(*,*)'Used_GB(kTest-nG:kTest+nG)=', &
               Used_GB(iTest,jTest,kTest-nG:kTest+nG,iBlockTest)
       end if
    end if

    if(.not.allocated(FaceL_I)) then
       allocate(FaceL_I(1:MaxIJK+2))
       allocate(FaceR_I(0:MaxIJK+1))
       allocate(WeightL_II(-2:2,0:MaxIJK+1))
       allocate(WeightR_II(-2:2,0:MaxIJK+1))
    endif

    UseTrueCell = IsBody_B(iBlock)

    call get_log_limiter_var

    UsePtotalLimiter = nOrder > 1 .and. nIonFluid == 1 .and. UsePtotalLtd

    if(.not.DoResChangeOnly & ! In order not to call it twice
         .and. nOrder > 1   & ! Is not needed for nOrder=1
         .and. (UseAccurateResChange .or. UseTvdResChange) &
         .and. .not.present(DoMonotoneRestrict)) &
         call correct_monotone_restrict(iBlock)

    ! first, calculate the CELL values for the variables to be limited
    ! for non-boris corrections they are: density, velocity, pressure
    ! for boris correction momentum is used instead of the velocity

    ! Number of cells away from the cell center
    if(nOrder == 5)then
       nStencil = 3
    else
       nStencil = nOrder
    end if

    if(DoLimitMomentum)then
       if(UseBorisRegion)then
          call set_clight_cell(iBlock)
          call set_clight_face(iBlock)
       end if
    end if

    do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
       Primitive_VG(:,i,j,k) = State_VGB(:,i,j,k,iBlock)
    end do; end do; end do

    if(UseAccurateResChange)then
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          call calc_primitives(i, j, k, iBlock)        ! all cells
       end do; end do; end do
    else
       do k = kMinFace, kMaxFace
          do j = jMinFace, jMaxFace
             do i = 1-nStencil, nI+nStencil
                call calc_primitives(i, j, k, iBlock)   ! for x-faces
             end do
          end do
       end do

       if(nJ > 1)then
          do k = kMinFace, kMaxFace; do i = iMinFace, iMaxFace
             do j = 1-nStencil, jMinFace-1
                call calc_primitives(i, j, k, iBlock)  ! for lower y-faces
             end do
             do j = jMaxFace+1, nJ+nStencil
                call calc_primitives(i, j, k, iBlock)  ! for upper y-faces
             end do
          end do; end do
       end if
       if(nK > 1)then
          do j = jMinFace, jMaxFace; do i = iMinFace, iMaxFace
             do k = 1-nStencil, kMinFace-1
                call calc_primitives(i, j, k, iBlock)  ! for lower z-faces
             end do
             do k = kMaxFace+1, nK+nStencil
                call calc_primitives(i, j, k, iBlock)  ! for upper z-faces
             end do
          end do; end do
       end if
    end if

    if(UseArtificialVisco) call calc_face_div_u(iBlock)

    ! Now the first or second order face values are calculated
    select case(nOrder)
    case(1)
       ! First order reconstruction
       if (.not.DoResChangeOnly) then
          call get_faceX_first(&
               1, nIFace, jMinFace, jMaxFace, kMinFace, kMaxFace, iBlock)
          if(nJ > 1) call get_faceY_first(&
               iMinFace, iMaxFace, 1, nJFace, kMinFace, kMaxFace, iBlock)
          if(nK > 1) call get_faceZ_first(&
               iMinFace, iMaxFace, jMinFace, jMaxFace, 1, nKFace, iBlock)
       else
          if(DiLevel_EB(1,iBlock) == +1)&
               call get_faceX_first(1,1,1,nJ,1,nK,iBlock)
          if(DiLevel_EB(2,iBlock) == +1)&
               call get_faceX_first(nIFace,nIFace,1,nJ,1,nK,iBlock)
          if(nJ > 1 .and. DiLevel_EB(3,iBlock) == +1) &
               call get_faceY_first(1,nI,1,1,1,nK,iBlock)
          if(nJ > 1 .and. DiLevel_EB(4,iBlock) == +1) &
               call get_faceY_first(1,nI,nJFace,nJFace,1,nK,iBlock)
          if(nK > 1 .and. DiLevel_EB(5,iBlock) == +1) &
               call get_faceZ_first(1,nI,1,nJ,1,1,iBlock)
          if(nK > 1 .and. DiLevel_EB(6,iBlock) == +1) &
               call get_faceZ_first(1,nI,1,nJ,nKFace,nKFace,iBlock)
       end if
    case default
       if (.not.DoResChangeOnly)then
          ! Calculate all face values with high order scheme
          if(nOrder==2 .or. IsLowOrderOnly_B(iBlock))then
             ! Second order scheme
             call get_faceX_second( &
                  1, nIFace, jMinFace, jMaxFace, kMinFace, kMaxFace, iBlock)
             if(nJ > 1) call get_faceY_second( &
                  iMinFace, iMaxFace, 1, nJFace, kMinFace, kMaxFace, iBlock)
             if(nK > 1) call get_faceZ_second(&
                  iMinFace, iMaxFace, jMinFace, jMaxFace, 1, nKFace, iBlock)
          else
             ! High order scheme
             call get_facex_high(1, nIFace, &
                  jMinFace2, jMaxFace2, kMinFace2, kMaxFace2, iBlock)
             if(nJ > 1) call get_facey_high(iMinFace2, iMaxFace2, &
                  1,nJFace,kMinFace2,kMaxFace2,iBlock)
             if(nK > 1) call get_facez_high(iMinFace2, iMaxFace2, &
                  jMinFace2, jMaxFace2, 1, nKFace, iBlock)
          end if
       end if

       ! Now take care of faces at resolution changes
       if(nOrderProlong==1 .and..not.UseConstrainB &
            .and. .not.UseHighResChange)then
          ! If nOrderProlong is 1 then use TVD reschange or accurate reschange
          ! scheme and overwrite face values at resolution changes

          if(nJ == 1 .and. (UseAccurateResChange .or. UseTvdResChange))then
             do iSide = 1, 2
                if(DiLevel_EB(iSide,iBlock) == 1) &
                     call get_face_accurate1d(iSide, iBlock)
             end do
          elseif (UseAccurateResChange)then
             if(nK == 1)then
                do iSide = 1, 4
                   if(DiLevel_EB(iSide,iBlock) == 1) &
                        call get_face_accurate2d(iSide, iBlock)
                end do
             else
                do iSide = 1, 6
                   if(DiLevel_EB(iSide,iBlock) == 1) &
                        call get_face_accurate3d(iSide, iBlock)
                end do
             end if
          else if(UseTvdResChange)then
             do iSide = 1, 6
                if(DiLevel_EB(iSide,iBlock) == +1) &
                     call get_face_tvd(iSide, iBlock)
             end do
          else
             ! First order facevalues at resolution change
             if(DiLevel_EB(1,iBlock) == +1)&
                  call get_faceX_first(1, 1, 1, nJ, 1, nK, iBlock)
             if(DiLevel_EB(2,iBlock)==+1)&
                  call get_faceX_first(nIFace, nIFace, 1, nJ, 1, nK, iBlock)
             if(nJ > 1 .and. DiLevel_EB(3,iBlock) == +1) &
                  call get_faceY_first(1, nI, 1, 1, 1, nK, iBlock)
             if(nJ > 1 .and. DiLevel_EB(4,iBlock) == +1) &
                  call get_faceY_first(1, nI, nJFace, nJFace, 1, nK, iBlock)
             if(nK > 1 .and. DiLevel_EB(5,iBlock) == +1) &
                  call get_faceZ_first(1, nI, 1, nJ, 1, 1, iBlock)
             if(nK > 1 .and. DiLevel_EB(6,iBlock) == +1) &
                  call get_faceZ_first(1, nI, 1, nJ, nKFace, nKFace, iBlock)
          end if
       else if(DoResChangeOnly) then
          if(nOrder==2 .or. IsLowOrderOnly_B(iBlock))then
             ! Second order face values at resolution changes
             if(DiLevel_EB(1,iBlock)==+1)&
                  call get_faceX_second(1,1,1,nJ,1,nK,iBlock)
             if(DiLevel_EB(2,iBlock)==+1)&
                  call get_faceX_second(nIFace,nIFace,1,nJ,1,nK,iBlock)
             if(nJ > 1 .and. DiLevel_EB(3,iBlock)==+1) &
                  call get_faceY_second(1,nI,1,1,1,nK,iBlock)
             if(nJ > 1 .and. DiLevel_EB(4,iBlock)==+1) &
                  call get_faceY_second(1,nI,nJFace,nJFace,1,nK,iBlock)
             if(nK > 1 .and. DiLevel_EB(5,iBlock)==+1) &
                  call get_faceZ_second(1,nI,1,nJ,1,1,iBlock)
             if(nK > 1 .and. DiLevel_EB(6,iBlock)==+1) &
                  call get_faceZ_second(1,nI,1,nJ,nKFace,nKFace,iBlock)
          else
             ! High order face values at resolution changes
             if(DiLevel_EB(1,iBlock)==+1)&
                  call get_faceX_high(1,1,1,nJ,1,nK,iBlock)
             if(DiLevel_EB(2,iBlock)==+1)&
                  call get_faceX_high(nIFace,nIFace,1,nJ,1,nK,iBlock)
             if(nJ > 1 .and. DiLevel_EB(3,iBlock)==+1) &
                  call get_faceY_high(1,nI,1,1,1,nK,iBlock)
             if(nJ > 1 .and. DiLevel_EB(4,iBlock)==+1) &
                  call get_faceY_high(1,nI,nJFace,nJFace,1,nK,iBlock)
             if(nK > 1 .and. DiLevel_EB(5,iBlock)==+1) &
                  call get_faceZ_high(1,nI,1,nJ,1,1,iBlock)
             if(nK > 1 .and. DiLevel_EB(6,iBlock)==+1) &
                  call get_faceZ_high(1,nI,1,nJ,nKFace,nKFace,iBlock)
          end if
       endif

       if(UseLogLimiter .and. .not.DoLimitMomentum)then
          if(DoResChangeOnly)then
             if(DiLevel_EB(1,iBlock)==+1) &
                  call logfaceX_to_faceX(1,1, 1,nJ, 1,nK)
             if(DiLevel_EB(2,iBlock)==+1) &
                  call logfaceX_to_faceX(nIFace,nIFace, 1,nJ, 1,nK)
             if(nJ > 1 .and. DiLevel_EB(3,iBlock)==+1) &
                  call logfaceY_to_faceY(1,nI,1,1,1,nK)
             if(nJ > 1 .and. DiLevel_EB(4,iBlock)==+1) &
                  call logfaceY_to_faceY(1,nI, nJFace,nJFace, 1,nK)
             if(nK > 1 .and. DiLevel_EB(5,iBlock)==+1) &
                  call logfaceZ_to_faceZ(1,nI,1,nJ,1,1)
             if(nK > 1 .and. DiLevel_EB(6,iBlock)==+1) &
                  call logfaceZ_to_faceZ(1,nI, 1,nJ, nKFace,nKFace)
          else
             call logfaceX_to_faceX( &
                  1,nIFace, jMinFace,jMaxFace, kMinFace,kMaxFace)
             if(nJ > 1) call logfaceY_to_faceY( &
                  iMinFace,iMaxFace, 1,nJFace, kMinFace,kMaxFace)
             if(nK > 1) call logfaceZ_to_faceZ( &
                  iMinFace,iMaxFace, jMinFace,jMaxFace, 1,nKFace)
          end if
       end if

       if(UseRhoRatioLimiter)then
          if(DoResChangeOnly)then
             if(DiLevel_EB(1,iBlock)==+1) &
                  call ratio_to_scalar_faceX(1, 1, 1, nJ, 1, nK)
             if(DiLevel_EB(2,iBlock)==+1) &
                  call ratio_to_scalar_faceX(nIFace, nIFace, 1, nJ, 1, nK)
             if(nJ > 1 .and. DiLevel_EB(3,iBlock)==+1) &
                  call ratio_to_scalar_faceY(1, nI, 1, 1, 1, nK)
             if(nJ > 1 .and. DiLevel_EB(4,iBlock)==+1) &
                  call ratio_to_scalar_faceY(1, nI, nJFace, nJFace, 1, nK)
             if(nK > 1 .and. DiLevel_EB(5,iBlock)==+1) &
                  call ratio_to_scalar_faceZ(1, nI, 1, nJ, 1, 1)
             if(nK > 1 .and. DiLevel_EB(6,iBlock)==+1) &
                  call ratio_to_scalar_faceZ(1, nI, 1, nJ, nKFace, nKFace)
          else
             call ratio_to_scalar_faceX( &
                  1, nIFace, jMinFace, jMaxFace, kMinFace, kMaxFace)
             if(nJ > 1) call ratio_to_scalar_faceY( &
                  iMinFace, iMaxFace, 1, nJFace, kMinFace, kMaxFace)
             if(nK > 1) call ratio_to_scalar_faceZ( &
                  iMinFace, iMaxFace, jMinFace, jMaxFace, 1, nKFace)
          end if
       end if

       if(UsePtotalLimiter)then
          if(DoResChangeOnly)then
             if(DiLevel_EB(1,iBlock)==+1) &
                  call ptotal_to_p_faceX(1,1,1,nJ,1,nK)
             if(DiLevel_EB(2,iBlock)==+1) &
                  call ptotal_to_p_faceX(nIFace,nIFace,1,nJ,1,nK)
             if(nJ > 1 .and. DiLevel_EB(3,iBlock)==+1) &
                  call ptotal_to_p_faceY(1,nI,1,1,1,nK)
             if(nJ > 1 .and. DiLevel_EB(4,iBlock)==+1) &
                  call ptotal_to_p_faceY(1,nI,nJFace,nJFace,1,nK)
             if(nK > 1 .and. DiLevel_EB(5,iBlock)==+1) &
                  call ptotal_to_p_faceZ(1,nI,1,nJ,1,1)
             if(nK > 1 .and. DiLevel_EB(6,iBlock)==+1) &
                  call ptotal_to_p_faceZ(1,nI,1,nJ,nKFace,nKFace)
          else
             call ptotal_to_p_faceX(1,nIFace,jMinFace,jMaxFace, &
                  kMinFace,kMaxFace)
             if(nJ > 1) call ptotal_to_p_faceY(iMinFace,iMaxFace,1,nJFace, &
                  kMinFace,kMaxFace)
             if(nK > 1) call ptotal_to_p_faceZ(iMinFace,iMaxFace, &
                  jMinFace,jMaxFace,1,nKFace)
          end if
       end if
    end select  ! nOrder
    if(UseSaMhd)call correct_samhd_face_value(iBlock, DoResChangeOnly)
    if(DoTest)then
       write(*,*) NameSub,' finishing with DoResChangeOnly=', DoResChangeOnly
       if(iDimTest==0 .or. iDimTest==1) &
            write(*,*)'Left,Right(i-1/2),Left,Right(i+1/2)=', &
            LeftState_VX(iVarTest,iTest,jTest,kTest), &
            RightState_VX(iVarTest,iTest,jTest,kTest), &
            LeftState_VX(iVarTest,iTest+1,jTest,kTest), &
            RightState_VX(iVarTest,iTest+1,jTest,kTest)

       if(nDim > 1 .and. (iDimTest==0 .or. iDimTest==2)) &
            write(*,*)'Left,Right(j-1/2),Left,Right(j+1/2)=', &
            LeftState_VY(iVarTest,iTest,jTest,kTest), &
            RightState_VY(iVarTest,iTest,jTest,kTest), &
            LeftState_VY(iVarTest,iTest,jTest+1,kTest), &
            RightState_VY(iVarTest,iTest,jTest+1,kTest)

       if(nDim > 2 .and. (iDimTest==0 .or. iDimTest==3)) &
            write(*,*)'Left,Right(k-1/2),Left,Right(k+1/2)=', &
            LeftState_VZ(iVarTest,iTest,jTest,kTest), &
            RightState_VZ(iVarTest,iTest,jTest,kTest), &
            LeftState_VZ(iVarTest,iTest,jTest,kTest+1), &
            RightState_VZ(iVarTest,iTest,jTest,kTest+1)

    end if

    call test_stop(NameSub, DoTest, iBlock)

  contains
    !==========================================================================
    subroutine limit_var(lMin, lMax, iVar, DoCalcWeightIn)

      ! Switch between various possibilities for high order variable limiter

      integer, intent(in):: lMin, lMax, iVar
      logical, optional, intent(in):: DoCalcWeightIn
      logical:: DoCalcWeight
      !------------------------------------------------------------------------
      DoCalcWeight = .false.
      if(present(DoCalcWeightIn)) DoCalcWeight = DoCalcWeightIn

      if(UseCweno) then
         if (UsePerVarLimiter .or. DoCalcWeight) &
              call calc_cweno_weight(lMin, lMax)
         call limiter_cweno5(lMin, lMax, Cell_I, Cell_I, iVar)
      else
         call limiter_mp(lMin, lMax, Cell_I, Cell_I, iVar)
      end if

    end subroutine limit_var
    !==========================================================================
    subroutine limit_flux(lMin, lMax)

      ! Switch between various possibilities for 5th order flux limiter

      integer, intent(in):: lMin, lMax
      !------------------------------------------------------------------------
      if(UseCweno) then
         call limiter_cweno5(lMin, lMax, Cell_I, Cell2_I)
      else
         call limiter_mp(lMin, lMax, Cell_I, Cell2_I)
      end if

    end subroutine limit_flux
    !==========================================================================
    subroutine logfacex_to_facex(iMin, iMax, jMin, jMax, kMin, kMax)

      integer, intent(in) :: iMin, iMax, jMin, jMax, kMin, kMax
      integer:: iVar
      !------------------------------------------------------------------------
      do iVar = 1, nVar
         if(.not.UseLogLimiter_V(iVar))CYCLE

         LeftState_VX(iVar,iMin:iMax,jMin:jMax,kMin:kMax) = &
              exp(LeftState_VX(iVar,iMin:iMax,jMin:jMax,kMin:kMax))
         RightState_VX(iVar,iMin:iMax,jMin:jMax,kMin:kMax) = &
              exp(RightState_VX(iVar,iMin:iMax,jMin:jMax,kMin:kMax))
      end do

    end subroutine logfacex_to_facex
    !==========================================================================
    subroutine logfacey_to_facey(iMin, iMax, jMin, jMax, kMin, kMax)

      integer, intent(in) :: iMin, iMax, jMin, jMax, kMin, kMax
      integer:: iVar
      !------------------------------------------------------------------------
      do iVar = 1, nVar
         if(.not.UseLogLimiter_V(iVar))CYCLE

         LeftState_VY(iVar,iMin:iMax,jMin:jMax,kMin:kMax) = &
              exp(LeftState_VY(iVar,iMin:iMax,jMin:jMax,kMin:kMax))
         RightState_VY(iVar,iMin:iMax,jMin:jMax,kMin:kMax) = &
              exp(RightState_VY(iVar,iMin:iMax,jMin:jMax,kMin:kMax))
      end do

    end subroutine logfacey_to_facey
    !==========================================================================
    subroutine logfacez_to_facez(iMin, iMax, jMin, jMax, kMin, kMax)

      integer, intent(in) :: iMin, iMax, jMin, jMax, kMin, kMax
      integer:: iVar
      !------------------------------------------------------------------------
      do iVar = 1, nVar
         if(.not.UseLogLimiter_V(iVar))CYCLE

         LeftState_VZ(iVar,iMin:iMax,jMin:jMax,kMin:kMax) = &
              exp(LeftState_VZ(iVar,iMin:iMax,jMin:jMax,kMin:kMax))
         RightState_VZ(iVar,iMin:iMax,jMin:jMax,kMin:kMax) = &
              exp(RightState_VZ(iVar,iMin:iMax,jMin:jMax,kMin:kMax))
      end do

    end subroutine logfacez_to_facez
    !==========================================================================
    subroutine ratio_to_scalar_facex(iMin, iMax, jMin, jMax, kMin, kMax)

      integer, intent(in) :: iMin, iMax, jMin, jMax, kMin, kMax
      integer :: i, j, k
      !------------------------------------------------------------------------
      do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
         LeftState_VX(iVarLimitRatio_I,i,j,k) = &
              LeftState_VX(iVarLimitRatio_I,i,j,k) &
              *LeftState_VX(Rho_,i,j,k)
         RightState_VX(iVarLimitRatio_I,i,j,k) = &
              RightState_VX(iVarLimitRatio_I,i,j,k) &
              *RightState_VX(Rho_,i,j,k)
      end do; end do; end do

    end subroutine ratio_to_scalar_facex
    !==========================================================================
    subroutine ratio_to_scalar_facey(iMin, iMax, jMin, jMax, kMin, kMax)

      integer, intent(in) :: iMin, iMax, jMin, jMax, kMin, kMax
      integer :: i, j, k
      !------------------------------------------------------------------------
      do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
         LeftState_VY(iVarLimitRatio_I,i,j,k) = &
              LeftState_VY(iVarLimitRatio_I,i,j,k) &
              *LeftState_VY(Rho_,i,j,k)
         RightState_VY(iVarLimitRatio_I,i,j,k) = &
              RightState_VY(iVarLimitRatio_I,i,j,k) &
              *RightState_VY(Rho_,i,j,k)
      end do; end do; end do

    end subroutine ratio_to_scalar_facey
    !==========================================================================
    subroutine ratio_to_scalar_facez(iMin, iMax, jMin, jMax, kMin, kMax)

      integer, intent(in) :: iMin, iMax, jMin, jMax, kMin, kMax
      integer :: i, j, k
      !------------------------------------------------------------------------
      do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
         LeftState_VZ(iVarLimitRatio_I,i,j,k) = &
              LeftState_VZ(iVarLimitRatio_I,i,j,k) &
              *LeftState_VZ(Rho_,i,j,k)
         RightState_VZ(iVarLimitRatio_I,i,j,k) = &
              RightState_VZ(iVarLimitRatio_I,i,j,k) &
              *RightState_VZ(Rho_,i,j,k)
      end do; end do; end do

    end subroutine ratio_to_scalar_facez
    !==========================================================================
    subroutine ptotal_to_p_facex(iMin, iMax, jMin, jMax, kMin, kMax)

      integer, intent(in) :: iMin, iMax, jMin, jMax, kMin, kMax
      integer :: i, j, k
      !------------------------------------------------------------------------
      if(UseElectronPressure)then
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            LeftState_VX(p_,i,j,k) = LeftState_VX(p_,i,j,k) &
                 - LeftState_VX(Pe_,i,j,k)
            RightState_VX(p_,i,j,k) = RightState_VX(p_,i,j,k) &
                 - RightState_VX(Pe_,i,j,k)
         end do; end do; end do
      end if
      if(UseWavePressure)then
         if(IsOnAwRepresentative)then
            do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
               LeftState_VX(p_,i,j,k) = LeftState_VX(p_,i,j,k) &
                    - (GammaWave-1)*PoyntingFluxPerB*&
                    sqrt(LeftState_VX(Rho_,i,j,k)) &
                    *sum(LeftState_VX(WaveFirst_:WaveLast_,i,j,k))
               RightState_VX(p_,i,j,k) = RightState_VX(p_,i,j,k) &
                    - (GammaWave-1)*PoyntingFluxPerB*&
                    sqrt(RightState_VX(Rho_,i,j,k)) &
                    *sum(RightState_VX(WaveFirst_:WaveLast_,i,j,k))
            end do; end do; end do
         else
            do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
               LeftState_VX(p_,i,j,k) = LeftState_VX(p_,i,j,k) &
                    - (GammaWave-1) &
                    *sum(LeftState_VX(WaveFirst_:WaveLast_,i,j,k))
               RightState_VX(p_,i,j,k) = RightState_VX(p_,i,j,k) &
                    - (GammaWave-1) &
                    *sum(RightState_VX(WaveFirst_:WaveLast_,i,j,k))
            end do; end do; end do
         end if
      end if

    end subroutine ptotal_to_p_facex
    !==========================================================================
    subroutine ptotal_to_p_facey(iMin, iMax, jMin, jMax, kMin, kMax)

      integer, intent(in) :: iMin, iMax, jMin, jMax, kMin, kMax
      integer :: i, j, k
      !------------------------------------------------------------------------
      if(UseElectronPressure)then
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            LeftState_VY(p_,i,j,k) = LeftState_VY(p_,i,j,k) &
                 - LeftState_VY(Pe_,i,j,k)
            RightState_VY(p_,i,j,k) = RightState_VY(p_,i,j,k) &
                 - RightState_VY(Pe_,i,j,k)
         end do; end do; end do
      end if
      if(UseWavePressure)then
         if(IsOnAwRepresentative)then
            do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
               LeftState_VY(p_,i,j,k) = LeftState_VY(p_,i,j,k) &
                    - (GammaWave-1)*PoyntingFluxPerB*&
                    sqrt(LeftState_VY(Rho_,i,j,k)) &
                    *sum(LeftState_VY(WaveFirst_:WaveLast_,i,j,k))
               RightState_VY(p_,i,j,k) = RightState_VY(p_,i,j,k) &
                    - (GammaWave-1)*PoyntingFluxPerB*&
                    sqrt(RightState_VY(Rho_,i,j,k)) &
                    *sum(RightState_VY(WaveFirst_:WaveLast_,i,j,k))
            end do; end do; end do
         else
            do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
               LeftState_VY(p_,i,j,k) = LeftState_VY(p_,i,j,k) &
                    - (GammaWave-1) &
                    *sum(LeftState_VY(WaveFirst_:WaveLast_,i,j,k))
               RightState_VY(p_,i,j,k) = RightState_VY(p_,i,j,k) &
                    - (GammaWave-1) &
                    *sum(RightState_VY(WaveFirst_:WaveLast_,i,j,k))
            end do; end do; end do
         end if
      end if

    end subroutine ptotal_to_p_facey
    !==========================================================================
    subroutine ptotal_to_p_facez(iMin, iMax, jMin, jMax, kMin, kMax)

      integer, intent(in) :: iMin, iMax, jMin, jMax, kMin, kMax
      integer :: i, j, k
      !------------------------------------------------------------------------
      if(UseElectronPressure)then
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            LeftState_VZ(p_,i,j,k) = LeftState_VZ(p_,i,j,k) &
                 - LeftState_VZ(Pe_,i,j,k)
            RightState_VZ(p_,i,j,k) = RightState_VZ(p_,i,j,k) &
                 - RightState_VZ(Pe_,i,j,k)
         end do; end do; end do
      end if
      if(UseWavePressure)then
         if(IsOnAwRepresentative)then
            do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
               LeftState_VZ(p_,i,j,k) = LeftState_VZ(p_,i,j,k) &
                    - (GammaWave-1)*PoyntingFluxPerB*&
                    sqrt(LeftState_VZ(Rho_,i,j,k)) &
                    *sum(LeftState_VZ(WaveFirst_:WaveLast_,i,j,k))
               RightState_VZ(p_,i,j,k) = RightState_VZ(p_,i,j,k) &
                    - (GammaWave-1)*PoyntingFluxPerB*&
                    sqrt(RightState_VZ(Rho_,i,j,k)) &
                    *sum(RightState_VZ(WaveFirst_:WaveLast_,i,j,k))
            end do; end do; end do
         else
            do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
               LeftState_VZ(p_,i,j,k) = LeftState_VZ(p_,i,j,k) &
                    - (GammaWave-1) &
                    *sum(LeftState_VZ(WaveFirst_:WaveLast_,i,j,k))
               RightState_VZ(p_,i,j,k) = RightState_VZ(p_,i,j,k) &
                    - (GammaWave-1) &
                    *sum(RightState_VZ(WaveFirst_:WaveLast_,i,j,k))
            end do; end do; end do
         end if
      end if

    end subroutine ptotal_to_p_facez
    !==========================================================================
    subroutine calc_primitives(i, j, k, iBlock)

      ! Convert Primitive_VG from conservative to primitive variables

      use ModPhysics, ONLY: InvClight2

      integer, intent(in):: i, j, k, iBlock

      real:: RhoInv, RhoC2Inv, BxFull, ByFull, BzFull, B2Full, uBC2Inv, &
           Ga2Boris
      integer:: iVar, iFluid
      !------------------------------------------------------------------------
      RhoInv = 1/Primitive_VG(Rho_,i,j,k)

      if(DoLimitMomentum)then
         ! momentum is limited

         ! rhoU_Boris = rhoU - ((U x B) x B)/c^2
         !            = rhoU + (U B^2 - B U.B)/c^2
         !            = rhoU*(1+BB/(rho*c2)) - B UdotB/c^2
         BxFull = Primitive_VG(Bx_,i,j,k)
         ByFull = Primitive_VG(By_,i,j,k)
         BzFull = Primitive_VG(Bz_,i,j,k)

         if(UseB0) then
            BxFull = B0_DGB(x_,i,j,k,iBlock) + BxFull
            ByFull = B0_DGB(y_,i,j,k,iBlock) + ByFull
            BzFull = B0_DGB(z_,i,j,k,iBlock) + BzFull
         endif

         B2Full = BxFull**2 + ByFull**2 + BzFull**2
         if(UseBorisRegion)then
            RhoC2Inv = RhoInv/Clight_G(i,j,k)**2
         else
            RhoC2Inv = RhoInv*InvClight2
         end if
         uBC2Inv= (Primitive_VG(rhoUx_,i,j,k)*BxFull + &
              Primitive_VG(rhoUy_,i,j,k)*ByFull + &
              Primitive_VG(rhoUz_,i,j,k)*BzFull)*RhoC2Inv
         Ga2Boris= 1 + B2Full*RhoC2Inv

         Primitive_VG(Ux_,i,j,k)= Primitive_VG(rhoUx_,i,j,k)*&
              Ga2Boris - BxFull*uBC2Inv
         Primitive_VG(Uy_,i,j,k)= Primitive_VG(rhoUy_,i,j,k)*&
              Ga2Boris - ByFull*uBC2Inv
         Primitive_VG(Uz_,i,j,k)= Primitive_VG(rhoUz_,i,j,k)*&
              Ga2Boris - BzFull*uBC2Inv
      else
         Primitive_VG(Ux_:Uz_,i,j,k) = RhoInv*Primitive_VG(RhoUx_:RhoUz_,i,j,k)
         do iFluid = 2, nFluid
            iRho = iRho_I(iFluid); iUx = iUx_I(iFluid); iUz = iUz_I(iFluid)
            RhoInv = 1/Primitive_VG(iRho,i,j,k)
            Primitive_VG(iUx:iUz,i,j,k) = &
                 RhoInv*Primitive_VG(iUx:iUz,i,j,k)
         end do
      end if

      ! Transform p to Ptotal
      if(UsePtotalLimiter)then
         if(UseElectronPressure)then
            Primitive_VG(p_,i,j,k) = Primitive_VG(p_,i,j,k) &
                 + Primitive_VG(Pe_,i,j,k)
         end if
         if(UseWavePressure)then
            if(IsOnAwRepresentative)then
               Primitive_VG(p_,i,j,k) = Primitive_VG(p_,i,j,k) &
                    + (GammaWave-1)*PoyntingFluxPerB*&
                    Primitive_VG(Rho_,i,j,k)&
                    *sum(Primitive_VG(WaveFirst_:WaveLast_,i,j,k))
            else
               Primitive_VG(p_,i,j,k) = Primitive_VG(p_,i,j,k) &
                    + (GammaWave-1) &
                    *sum(Primitive_VG(WaveFirst_:WaveLast_,i,j,k))
            end if
         end if
      end if

      if(UseRhoRatioLimiter) Primitive_VG(iVarLimitRatio_I,i,j,k) &
           = RhoInv*Primitive_VG(iVarLimitRatio_I,i,j,k)

      if(UseLogLimiter)then
         do iVar = 1, nVar
            if(UseLogLimiter_V(iVar)) Primitive_VG(iVar,i,j,k) &
                 = log(Primitive_VG(iVar,i,j,k))
         end do
      end if

    end subroutine calc_primitives
    !==========================================================================
    subroutine get_facex_high(iMin, iMax, jMin, jMax, kMin, kMax, iBlock)

      integer,intent(in):: iMin, iMax, jMin, jMax, kMin, kMax, iBlock
      integer:: iVar, iSort
      logical:: IsSmoothIndictor
      real:: Primitive_VI(1:nVar,1-nG:MaxIJK+nG)
      ! variables used for TVD limiters
      real:: dVarLimR_VI(1:nVar,0:MaxIJK+1) ! limited slope for right state
      real:: dVarLimL_VI(1:nVar,0:MaxIJK+1) ! limited slope for left state
      !------------------------------------------------------------------------
      if(TypeLimiter == 'no')then
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            LeftState_VX(:,i,j,k) &
                 = c7over12 *(Primitive_VG(:,i-1,j,k)  &
                 +          Primitive_VG(:,i,j,k)  )   &
                 - c1over12*(Primitive_VG(:,i-2,j,k)   &
                 +         Primitive_VG(:,i+1,j,k) )
            RightState_VX(:,i,j,k) = LeftState_VX(:,i,j,k)
         end do; end do; end do
      else
         do k = kMin, kMax; do j = jMin, jMax
            if(UseLowOrder) then
               Primitive_VI(:,iMin-2:iMax+1) &
                    = Primitive_VG(:,iMin-2:iMax+1,j,k)
               if(nLowOrder==2) then
                  ! IsTrueCell needed by limiter_body
                  IsTrueCell_I(iMin-nG:iMax-1+nG) = &
                       Used_GB(iMin-nG:iMax-1+nG,j,k,iBlock)
                  ! Get 2nd order limited slopes
                  if(UseTrueCell)then
                     call limiter_body(iMin, iMax, LimiterBeta,&
                          Primitive_VI, dVarLimL_VI, dVarLimR_VI)
                  else
                     call limiter(iMin, iMax, LimiterBeta,&
                          Primitive_VI, dVarLimL_VI, dVarLimR_VI)
                  end if
               else
                  dVarLimL_VI = 0; dVarLimR_VI = 0
               endif
               ! Store 1st/2nd order accurate face values
               do i = iMin, iMax
                  ! if(UseLowOrder_I(i))then...
                  LeftState_VX(:,i,j,k) &
                       = Primitive_VI(:,i-1) + dVarLimL_VI(:,i-1)
                  RightState_VX(:,i,j,k) &
                       = Primitive_VI(:,i)   - dVarLimR_VI(:,i)
               end do
            endif

            if(UseLowOrder) LowOrderCrit_I(iMin:iMax) = &
                    LowOrderCrit_XB(iMin:iMax,j,k,iBlock)

            iVarSmoothLast = 0
            do iSort = 1, nVar
               if(UseCweno) then
                  iVar = iVarSmoothIndex_I(iSort)
                  iVarSmooth = iVarSmooth_V(iVar)
                  if(iVarSmooth /= iVarSmoothLast) then
                     IsSmoothIndictor = .true.
                     iVarSmoothLast = iVarSmooth
                  else
                     IsSmoothIndictor = .false.
                  endif
               else
                  iVar = iSort
               endif

               ! Copy points along i direction into 1D array
               Cell_I(iMin-nG:iMax-1+nG) = &
                    Primitive_VG(iVar,iMin-nG:iMax-1+nG,j,k)

               if(UseLowOrder)then
                  ! Use 2nd order face values where high order is skipped
                  ! where(UseLowOrder_I(iMin:iMax)...
                  FaceL_I(iMin:iMax) = LeftState_VX(iVar,iMin:iMax,j,k)
                  FaceR_I(iMin:iMax) = RightState_VX(iVar,iMin:iMax,j,k)
               end if

               call limit_var(iMin, iMax, iVar, &
                    DoCalcWeightIn = IsSmoothIndictor)

               ! Copy back the results into the 3D arrays
               LeftState_VX(iVar,iMin:iMax,j,k)  = FaceL_I(iMin:iMax)
               RightState_VX(iVar,iMin:iMax,j,k) = FaceR_I(iMin:iMax)
            end do
         end do; end do
      end if

      if(DoLimitMomentum) &
           call boris_to_mhd_x(iMin, iMax, jMin, jMax, kMin, kMax)

      if(UseRhoRatioLimiter) &
           call ratio_to_scalar_faceX(iMin, iMax, jMin, jMax, kMin, kMax)

    end subroutine get_facex_high
    !==========================================================================
    subroutine get_facey_high(iMin, iMax, jMin, jMax, kMin, kMax, iBlock)

      integer,intent(in):: iMin, iMax, jMin, jMax, kMin, kMax, iBlock

      integer:: iVar, iSort
      logical:: IsSmoothIndictor
      real:: Primitive_VI(1:nVar,1-nG:MaxIJK+nG)

      ! variables used for TVD limiters
      real:: dVarLimR_VI(1:nVar,0:MaxIJK+1) ! limited slope for right state
      real:: dVarLimL_VI(1:nVar,0:MaxIJK+1) ! limited slope for left state
      !------------------------------------------------------------------------
      if(TypeLimiter == 'no')then
         do k=kMin, kMax; do j=jMin, jMax; do i=iMin,iMax
            LeftState_VY(:,i,j,k) &
                 = c7over12*(Primitive_VG(:,i,j-1,k) &
                 +           Primitive_VG(:,i,j,k))  &
                 - c1over12*(Primitive_VG(:,i,j-2,k) &
                 +           Primitive_VG(:,i,j+1,k))
            RightState_VY(:,i,j,k) = LeftState_VY(:,i,j,k)
         end do; end do; end do
      else
         do k = kMin, kMax; do i = iMin, iMax
            if(UseLowOrder) then
               Primitive_VI(:,jMin-2:jMax+1) &
                    = Primitive_VG(:,i,jMin-2:jMax+1,k)

               if(nLowOrder==2) then
                  IsTrueCell_I(jMin-nG:jMax-1+nG) = &
                       Used_GB(i,jMin-nG:jMax-1+nG,k,iBlock)
                  if(UseTrueCell)then
                     call limiter_body(jMin, jMax, LimiterBeta,&
                          Primitive_VI, dVarLimL_VI, dVarLimR_VI)
                  else
                     call limiter(jMin, jMax, LimiterBeta,&
                          Primitive_VI, dVarLimL_VI, dVarLimR_VI)
                  end if
               else
                  dVarLimL_VI = 0; dVarLimR_VI = 0
               endif
               do j = jMin, jMax
                  LeftState_VY(:,i,j,k) &
                       = Primitive_VI(:,j-1) + dVarLimL_VI(:,j-1)
                  RightState_VY(:,i,j,k) &
                       = Primitive_VI(:,j)   - dVarLimR_VI(:,j)
               end do
            endif

            if(UseLowOrder) LowOrderCrit_I(jMin:jMax) = &
                 LowOrderCrit_YB(i,jMin:jMax,k,iBlock)

            iVarSmoothLast = 0
            do iSort = 1, nVar
               if(UseCweno) then
                  ! The variables use the same smooth indicator are
                  ! calculated one by one. And the smooth indicator
                  ! itself is calculated first.
                  iVar = iVarSmoothIndex_I(iSort)
                  iVarSmooth = iVarSmooth_V(iVar)
                  if(iVarSmooth /= iVarSmoothLast) then
                     IsSmoothIndictor = .true.
                     iVarSmoothLast = iVarSmooth
                  else
                     IsSmoothIndictor = .false.
                  endif
               else
                  iVar = iSort
               endif

               ! Copy points along j direction into 1D array
               Cell_I(jMin-nG:jMax-1+nG) = &
                    Primitive_VG(iVar,i,jMin-nG:jMax-1+nG,k)

               if(UseLowOrder)then
                  ! Use 2nd order face values where high order is skipped
                  FaceL_I(jMin:jMax) = LeftState_VY(iVar,i,jMin:jMax,k)
                  FaceR_I(jMin:jMax) = RightState_VY(iVar,i,jMin:jMax,k)
               end if

               call limit_var(jMin, jMax, iVar, &
                    DoCalcWeightIn = IsSmoothIndictor)

               ! Copy back the results into the 3D arrays
               LeftState_VY(iVar,i,jMin:jMax,k)  = FaceL_I(jMin:jMax)
               RightState_VY(iVar,i,jMin:jMax,k) = FaceR_I(jMin:jMax)
            end do
         end do; end do
      end if

      if(DoLimitMomentum) &
           call boris_to_mhd_y(iMin, iMax, jMin, jMax, kMin, kMax)

      if(UseRhoRatioLimiter) &
           call ratio_to_scalar_faceY(iMin, iMax, jMin, jMax, kMin, kMax)

    end subroutine get_facey_high
    !==========================================================================
    subroutine get_facez_high(iMin, iMax, jMin, jMax, kMin, kMax, iBlock)

      integer,intent(in):: iMin, iMax, jMin, jMax, kMin, kMax, iBlock

      integer:: iVar, iSort
      logical:: IsSmoothIndictor
      real:: Primitive_VI(1:nVar,1-nG:MaxIJK+nG)
      ! variables used for TVD limiters
      real:: dVarLimR_VI(1:nVar,0:MaxIJK+1) ! limited slope for right state
      real:: dVarLimL_VI(1:nVar,0:MaxIJK+1) ! limited slope for left state
      !------------------------------------------------------------------------
      if(TypeLimiter == 'no')then
         do k=kMin, kMax; do j=jMin, jMax; do i=iMin,iMax
            LeftState_VZ(:,i,j,k) &
                 = c7over12*(Primitive_VG(:,i,j,k-1) &
                 +           Primitive_VG(:,i,j,k))  &
                 - c1over12*(Primitive_VG(:,i,j,k-2) &
                 +           Primitive_VG(:,i,j,k+1))

            RightState_VZ(:,i,j,k) = LeftState_VZ(:,i,j,k)
         end do; end do; end do
      else
         do j = jMin, jMax; do i = iMin, iMax

            if(UseLowOrder)then
               Primitive_VI(:,kMin-2:kMax+1) &
                    = Primitive_VG(:,i,j,kMin-2:kMax+1)

               if(nLowOrder==2) then
                  IsTrueCell_I(kMin-nG:kMax-1+nG) = &
                       Used_GB(i,j,kMin-nG:kMax-1+nG,iBlock)
                  if(UseTrueCell)then
                     call limiter_body(kMin, kMax, LimiterBeta,&
                          Primitive_VI, dVarLimL_VI, dVarLimR_VI)
                  else
                     call limiter(kMin, kMax, LimiterBeta,&
                          Primitive_VI, dVarLimL_VI, dVarLimR_VI)
                  end if
               else
                  dVarLimL_VI = 0; dVarLimR_VI = 0
               endif
               do k = kMin, kMax
                  LeftState_VZ(:,i,j,k) &
                       = Primitive_VI(:,k-1) + dVarLimL_VI(:,k-1)
                  RightState_VZ(:,i,j,k) &
                       = Primitive_VI(:,k)   - dVarLimR_VI(:,k)
               end do
            end if

            if(UseLowOrder) LowOrderCrit_I(kMin:kMax) = &
                 LowOrderCrit_ZB(i,j,kMin:kMax,iBlock)

            iVarSmoothLast = 0
            do iSort = 1, nVar
               if(UseCweno) then
                  iVar = iVarSmoothIndex_I(iSort)
                  iVarSmooth = iVarSmooth_V(iVar)
                  if(iVarSmooth /= iVarSmoothLast) then
                     IsSmoothIndictor = .true.
                     iVarSmoothLast = iVarSmooth
                  else
                     IsSmoothIndictor = .false.
                  endif
               else
                  iVar = iSort
               endif

               ! Copy points along k direction into 1D array
               Cell_I(kMin-nG:kMax-1+nG) = &
                    Primitive_VG(iVar,i,j,kMin-nG:kMax-1+nG)

               if(UseLowOrder)then
                  ! Use 2nd order face values where high order is skipped
                  FaceL_I(kMin:kMax) = LeftState_VZ(iVar,i,j,kMin:kMax)
                  FaceR_I(kMin:kMax) = RightState_VZ(iVar,i,j,kMin:kMax)
               end if

               call limit_var(kMin, kMax, iVar, &
                    DoCalcWeightIn = IsSmoothIndictor)

               ! Copy back the results into the 3D arrays
               LeftState_VZ(iVar,i,j,kMin:kMax)  = FaceL_I(kMin:kMax)
               RightState_VZ(iVar,i,j,kMin:kMax) = FaceR_I(kMin:kMax)
            end do
         end do; end do
      end if

      if(DoLimitMomentum) &
           call boris_to_mhd_z(iMin, iMax, jMin, jMax, kMin, kMax)

      if(UseRhoRatioLimiter) &
           call ratio_to_scalar_faceZ(iMin, iMax, jMin, jMax, kMin, kMax)

    end subroutine get_facez_high
    !==========================================================================
    subroutine get_facex_first(iMin, iMax, jMin, jMax, kMin, kMax, iBlock)
      integer,intent(in):: iMin, iMax, jMin, jMax, kMin, kMax, iBlock
      integer:: i, j, k
      !------------------------------------------------------------------------
      do k=kMin, kMax; do j=jMin, jMax; do i=iMin,iMax
         LeftState_VX(:,i,j,k)  = Primitive_VG(:,i-1,j,k)
         RightState_VX(:,i,j,k) = Primitive_VG(:,i,j,k)
      end do; end do; end do

      if(DoLimitMomentum) &
           call boris_to_mhd_x(iMin, iMax, jMin, jMax, kMin, kMax)

      if(UseRhoRatioLimiter) &
           call ratio_to_scalar_faceX(iMin, iMax, jMin, jMax, kMin, kMax)
    end subroutine get_facex_first
    !==========================================================================
    subroutine get_facey_first(iMin, iMax, jMin, jMax, kMin, kMax, iBlock)
      integer,intent(in):: iMin, iMax, jMin, jMax, kMin, kMax, iBlock
      integer:: i, j, k
      !------------------------------------------------------------------------
      do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
         LeftState_VY(:,i,j,k)  = Primitive_VG(:,i,j-1,k)
         RightState_VY(:,i,j,k) = Primitive_VG(:,i,j,k)
      end do; end do; end do

      if(DoLimitMomentum) &
           call boris_to_mhd_y(iMin, iMax, jMin, jMax, kMin, kMax)

      if(UseRhoRatioLimiter) &
           call ratio_to_scalar_faceY(iMin, iMax, jMin, jMax, kMin, kMax)

    end subroutine get_facey_first
    !==========================================================================
    subroutine get_facez_first(iMin, iMax, jMin, jMax, kMin, kMax, iBlock)
      integer,intent(in):: iMin, iMax, jMin, jMax, kMin, kMax, iBlock
      integer:: i, j, k
      !------------------------------------------------------------------------
      do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
         LeftState_VZ(:,i,j,k)  = Primitive_VG(:,i,j,k-1)
         RightState_VZ(:,i,j,k) = Primitive_VG(:,i,j,k)
      end do; end do; end do

      if(DoLimitMomentum) &
           call boris_to_mhd_z(iMin, iMax, jMin, jMax, kMin, kMax)

      if(UseRhoRatioLimiter) &
           call ratio_to_scalar_faceZ(iMin, iMax, jMin, jMax, kMin, kMax)

    end subroutine get_facez_first
    !==========================================================================
    subroutine get_facex_second(iMin, iMax, jMin, jMax, kMin, kMax, iBlock)

      use ModFieldLineThread, ONLY: UseFieldLineThreads, &
           beta_thread, is_threaded_block

      integer,intent(in):: iMin, iMax, jMin, jMax, kMin, kMax, iBlock
      integer::i1, iMinSharp, iMaxSharp
      real:: Primitive_VI(1:nVar,1-nG:MaxIJK+nG)
      ! variables used for TVD limiters
      real:: dVarLimR_VI(1:nVar,0:MaxIJK+1) ! limited slope for right state
      real:: dVarLimL_VI(1:nVar,0:MaxIJK+1) ! limited slope for left state
      integer:: i, j, k
      real :: BetaThread
      !------------------------------------------------------------------------
      iMinSharp = iMin
      iMaxSharp = iMax
      if(LimiterBeta > BetaLimiterResChange)then
         if(DiLevel_EB(1,iBlock) /= 0) iMinSharp = &
              max(iMin, min(iMax + 1,      1 + nFaceLimiterResChange))
         if(DiLevel_EB(2,iBlock) /= 0) iMaxSharp = &
              min(iMax, max(iMin - 1, nI + 1 - nFaceLimiterResChange))
      endif

      do k=kMin, kMax; do j=jMin, jMax
         Primitive_VI(:,iMin-2:iMax+1) = Primitive_VG(:,iMin-2:iMax+1,j,k)
         if(UseTrueCell)then
            IsTrueCell_I(iMin-2:iMax+1) = Used_GB(iMin-2:iMax+1,j,k,iBlock)
            if(iMinSharp <= iMaxSharp) &
                 call limiter_body(iMinSharp, iMaxSharp, LimiterBeta,&
                 Primitive_VI, dVarLimL_VI, dVarLimR_VI)
            if(iMin < iMinSharp) &
                 call limiter_body(iMin, iMinSharp-1, BetaLimiterResChange,&
                 Primitive_VI, dVarLimL_VI, dVarLimR_VI)
            if(iMax > iMaxSharp) &
                 call limiter_body(iMaxSharp+1, iMax, BetaLimiterResChange,&
                 Primitive_VI, dVarLimL_VI, dVarLimR_VI)
         else
            if(iMinSharp <= iMaxSharp) &
                 call limiter(iMinSharp, iMaxSharp, LimiterBeta,&
                 Primitive_VI, dVarLimL_VI, dVarLimR_VI)
            if(iMin < iMinSharp) &
                 call limiter(iMin, iMinSharp-1, BetaLimiterResChange,&
                 Primitive_VI, dVarLimL_VI, dVarLimR_VI)
            if(iMax > iMaxSharp) &
                 call limiter(iMaxSharp+1, iMax, BetaLimiterResChange,&
                 Primitive_VI, dVarLimL_VI, dVarLimR_VI)
         end if

         LowOrderCrit_I = 0
         if(UseLowOrder .and. nLowOrder==1) LowOrderCrit_I(iMin:iMax) = &
               LowOrderCrit_XB(iMin:iMax,j,k,iBlock)

         do i=iMin,iMax
            i1 = i - 1
            LeftState_VX(:,i,j,k)  &
                 = Primitive_VI(:,i1) + dVarLimL_VI(:,i1)*(1-LowOrderCrit_I(i))
            RightState_VX(:,i,j,k) &
                 = Primitive_VI(:,i ) - dVarLimR_VI(:,i )*(1-LowOrderCrit_I(i))
         end do
         if(.not. UseFieldLineThreads) CYCLE
         if(is_threaded_block(iBlock))then
            ! The cell center on thread may be closer/farther to the boundary
            ! face then the true cell center in SC. The correction factor
            ! acconts for this difference in such a way, that the half of
            ! difference, Primitive_VI(:,0) - Primitive_VI(:,1) with the
            ! corrected Primitive_VI(:,0) gives the second order coorection
            ! for the face values.
            BetaThread = beta_thread(j, k, iBlock)
            Primitive_VI(:,0) = Primitive_VI(:,1) + &
                 (2/BetaThread)*(Primitive_VI(:,0) - Primitive_VI(:,1))
            call limiter(2, 1, min(LimiterBeta,BetaThread),&
                 Primitive_VI, dVarLimL_VI, dVarLimR_VI)
            RightState_VX(:,1,j,k) &
                 = Primitive_VI(:,1 ) - dVarLimR_VI(:,1 )
         end if
      end do; end do

      if(DoLimitMomentum) &
           call boris_to_mhd_x(iMin, iMax, jMin, jMax, kMin, kMax)

    end subroutine get_facex_second
    !==========================================================================
    subroutine get_facey_second(iMin, iMax, jMin, jMax, kMin, kMax, iBlock)
      integer,intent(in):: iMin, iMax, jMin, jMax, kMin, kMax, iBlock
      integer::j1, jMinSharp, jMaxSharp
      real:: Primitive_VI(1:nVar,1-nG:MaxIJK+nG)
      ! variables used for TVD limiters
      real:: dVarLimR_VI(1:nVar,0:MaxIJK+1) ! limited slope for right state
      real:: dVarLimL_VI(1:nVar,0:MaxIJK+1) ! limited slope for left state
      integer:: i, j, k
      !------------------------------------------------------------------------
      jMinSharp = jMin
      jMaxSharp = jMax
      if(LimiterBeta > BetaLimiterResChange)then
         if(DiLevel_EB(3,iBlock) /= 0) jMinSharp = &
              max(jMin, min(jMax + 1,      1 + nFaceLimiterResChange))
         if(DiLevel_EB(4,iBlock) /= 0) jMaxSharp = &
              min(jMax, max(jMin - 1, nJ + 1 - nFaceLimiterResChange))
      endif

      do k=kMin, kMax; do i=iMin,iMax
         Primitive_VI(:,jMin-2:jMax+1) = Primitive_VG(:,i,jMin-2:jMax+1,k)
         if(UseTrueCell)then
            IsTrueCell_I(jMin-2:jMax+1) = Used_GB(i,jMin-2:jMax+1,k,iBlock)
            if(jMinSharp <= jMaxSharp) &
                 call limiter_body(jMinSharp, jMaxSharp, LimiterBeta,&
                 Primitive_VI, dVarLimL_VI, dVarLimR_VI)
            if(jMin < jMinSharp) &
                 call limiter_body(jMin, jMinSharp-1, BetaLimiterResChange,&
                 Primitive_VI, dVarLimL_VI, dVarLimR_VI)
            if(jMax > jMaxSharp) &
                 call limiter_body(jMaxSharp+1, jMax, BetaLimiterResChange,&
                 Primitive_VI, dVarLimL_VI, dVarLimR_VI)
         else
            if(jMinSharp <= jMaxSharp) &
                 call limiter(jMinSharp, jMaxSharp, LimiterBeta,&
                 Primitive_VI, dVarLimL_VI, dVarLimR_VI)
            if(jMin < jMinSharp) &
                 call limiter(jMin, jMinSharp-1, BetaLimiterResChange,&
                 Primitive_VI, dVarLimL_VI, dVarLimR_VI)
            if(jMax > jMaxSharp) &
                 call limiter(jMaxSharp+1, jMax, BetaLimiterResChange,&
                 Primitive_VI, dVarLimL_VI, dVarLimR_VI)
         end if

         LowOrderCrit_I = 0
         if(UseLowOrder .and. nLowOrder==1) LowOrderCrit_I(jMin:jMax) = &
              LowOrderCrit_YB(i,jMin:jMax,k,iBlock)

         do j=jMin, jMax
            j1 = j - 1
            LeftState_VY(:,i,j,k)  &
                 = Primitive_VI(:,j1) + dVarLimL_VI(:,j1)*(1-LowOrderCrit_I(j))
            RightState_VY(:,i,j,k) &
                 = Primitive_VI(:,j ) - dVarLimR_VI(:,j )*(1-LowOrderCrit_I(j))
         end do
      end do; end do

      if(DoLimitMomentum) &
           call boris_to_mhd_y(iMin, iMax, jMin, jMax, kMin, kMax)

    end subroutine get_facey_second
    !==========================================================================
    subroutine get_facez_second(iMin, iMax, jMin, jMax, kMin, kMax, iBlock)
      integer,intent(in) :: iMin, iMax, jMin, jMax, kMin, kMax, iBlock
      integer::k1, kMinSharp, kMaxSharp
      real:: Primitive_VI(1:nVar,1-nG:MaxIJK+nG)
      ! variables used for TVD limiters
      real:: dVarLimR_VI(1:nVar,0:MaxIJK+1) ! limited slope for right state
      real:: dVarLimL_VI(1:nVar,0:MaxIJK+1) ! limited slope for left state
      integer:: i, j, k
      !------------------------------------------------------------------------
      kMinSharp = kMin
      kMaxSharp = kMax
      if(LimiterBeta > BetaLimiterResChange)then
         if(DiLevel_EB(5,iBlock) /= 0) kMinSharp = &
              max(kMin, min(kMax + 1,      1 + nFaceLimiterResChange))
         if(DiLevel_EB(6,iBlock) /= 0) kMaxSharp = &
              min(kMax, max(kMin - 1, nK + 1 - nFaceLimiterResChange))
      endif

      do j=jMin,jMax; do i=iMin,iMax;
         Primitive_VI(:,kMin-2:kMax+1) = Primitive_VG(:,i,j,kMin-2:kMax+1)
         if(UseTrueCell)then
            IsTrueCell_I(kMin-2:kMax+1) = Used_GB(i,j,kMin-2:kMax+1,iBlock)
            if(kMinSharp <= kMaxSharp) &
                 call limiter_body(kMinSharp, kMaxSharp, LimiterBeta,&
                 Primitive_VI, dVarLimL_VI, dVarLimR_VI)
            if(kMin < kMinSharp) &
                 call limiter_body(kMin, kMinSharp-1, BetaLimiterResChange,&
                 Primitive_VI, dVarLimL_VI, dVarLimR_VI)
            if(kMax > kMaxSharp) &
                 call limiter_body(kMaxSharp+1, kMax, BetaLimiterResChange,&
                 Primitive_VI, dVarLimL_VI, dVarLimR_VI)
         else
            if(kMinSharp <= kMaxSharp) &
                 call limiter(kMinSharp, kMaxSharp, LimiterBeta,&
                 Primitive_VI, dVarLimL_VI, dVarLimR_VI)
            if(kMin < kMinSharp) &
                 call limiter(kMin, kMinSharp-1, BetaLimiterResChange,&
                 Primitive_VI, dVarLimL_VI, dVarLimR_VI)
            if(kMax > kMaxSharp) &
                 call limiter(kMaxSharp+1, kMax, BetaLimiterResChange,&
                 Primitive_VI, dVarLimL_VI, dVarLimR_VI)
         end if

         LowOrderCrit_I = 0
         if(UseLowOrder .and. nLowOrder==1) LowOrderCrit_I(kMin:kMax) = &
              LowOrderCrit_ZB(i,j,kMin:kMax,iBlock)

         do k=kMin,kMax
            k1 = k - 1
            LeftState_VZ(:,i,j,k)  &
                 = Primitive_VI(:,k1) + dVarLimL_VI(:,k1)*(1-LowOrderCrit_I(k))
            RightState_VZ(:,i,j,k) &
                 = Primitive_VI(:,k ) - dVarLimR_VI(:,k )*(1-LowOrderCrit_I(k))
         end do
      end do; end do

      if(DoLimitMomentum) &
           call boris_to_mhd_z(iMin, iMax, jMin, jMax, kMin, kMax)

    end subroutine get_facez_second
    !==========================================================================
  end subroutine calc_face_value
  !============================================================================
  subroutine tvd_reschange_body(&
       Coarse2_V         ,& ! State in the coarser ghostcell, 2nd layer
       Coarse1_V         ,& ! State in the coarser ghostcell, 1st layer
       Fine1_VII         ,& ! States in 4 fine physical cells,1st layer
       Fine2_VII         ,& ! States in 4 fine physical cells,2nd layer
       CoarseToFineF_VII ,& ! Values at subfaces, in the coarse ghostcell
       FineToCoarseF_VII ,& ! Facevalues in phys. cell looking at coarser cell
       FineF_VII         ,& ! Facevalues in phys. cell looking at phys. cell
       IsTrueCoarse2     ,& ! True if coarser ghostcell of 2nd layer is true
       IsTrueCoarse1     ,& ! True if coarser ghostcell of 1st layer is true
       IsTrueFine1       ,& ! True if all physical cells of 1st layer are true
       IsTrueFine2_II)      ! True for true physical cell of the 2nd layer

    ! _____________|_____________|_______|_______|_
    !              |         CToF|FToC FF|       |
    !  C2_V        | C1_V       _|_F1_V__|__F2_V_|_
    !              |         CToF|FToC FF|       |
    ! _____________|_____________|_F1_V__|__F2_V_|_
    !              |             |       |       |

    real, intent(in):: Coarse2_V(:)              ! dimension(nVar)
    real, intent(in):: Coarse1_V(:)              ! dimension(nVar)
    real, intent(in):: Fine1_VII(:,:,:)          ! dimension(nVar,2,2)
    real, intent(in):: Fine2_VII(:,:,:)          ! dimension(nVar,2,2)
    real, intent(out):: CoarseToFineF_VII(:,:,:) ! dimension(nVar,2,2)
    real, intent(out):: FineToCoarseF_VII(:,:,:) ! dimension(nVar,2,2)
    real, intent(out):: FineF_VII(:,:,:)         ! dimension(nVar,2,2)
    logical, intent(in):: IsTrueCoarse2, IsTrueCoarse1, IsTrueFine1
    logical, intent(in):: IsTrueFine2_II(:,:)    ! dimension(2,2)

    integer::iVar, i2, j2
    real,dimension(nVar):: AveragedFine1_V,GradNormal_V,SignGradNormal_V
    real,dimension(nVar):: GradNormalLtd_V  ! Ltd stands for "Limited"

    real:: Beta

    character(len=*), parameter:: NameSub = 'tvd_reschange_body'
    !--------------------------------------------------------------------------

    ! Calculate averaged Fine1_VII
    do iVar=1,nVar
       AveragedFine1_V(iVar) = 0.25*sum(Fine1_VII(iVar,:,:))
    end do
    GradNormal_V = AveragedFine1_V - Coarse1_V

    ! Save gradients squared
    SignGradNormal_V = sign(1.0,GradNormal_V)

    Beta = min(BetaLimiterResChange, LimiterBeta)

    if(IsTrueCoarse2.and.IsTrueCoarse1.and.IsTrueFine1)then
       ! Limit gradient in the first coarser cell
       GradNormalLtd_V = SignGradNormal_V*&
            max(0.0,&
            min(Beta*cTwoThird*abs(GradNormal_V),&
            Beta*0.5*SignGradNormal_V*(Coarse1_V-Coarse2_V),&
            cThird*abs(GradNormal_V)+&
            0.25*SignGradNormal_V*(Coarse1_V-Coarse2_V)))

       do j2=1,2;do i2=1,2
          ! Limit transverse gradients, if they are larger than the normal one
          ! The unlimited transverse gradients are Fine1_VII-AveragedFine1_V
          CoarseToFineF_VII(:,i2,j2) = Coarse1_V + GradNormalLtd_V +&
               sign(min(abs(GradNormalLtd_V), &
               abs(Fine1_VII(:,i2,j2)-AveragedFine1_V)),&
               Fine1_VII(:,i2,j2)-AveragedFine1_V)
       end do;end do
    else
       do j2=1,2;do i2=1,2
          ! First order scheme
          CoarseToFineF_VII(:,i2,j2) = Coarse1_V
       end do;end do
    end if
    if(.not.(IsTrueCoarse1.and.IsTrueFine1))then
       do j2=1,2;do i2=1,2
          ! First order scheme
          FineToCoarseF_VII(:,i2,j2) = Fine1_VII(:,i2,j2)
          FineF_VII(:,i2,j2) = Fine1_VII(:,i2,j2)
       end do;end do
    else
       do j2=1,2;do i2=1,2
          if(IsTrueFine2_II(i2,j2))then
             ! Limit gradient in the first layer of finer cells
             GradNormalLtd_V = SignGradNormal_V*&
                  max(0.0,&
                  min(Beta*cThird*abs(GradNormal_V),&
                  SignGradNormal_V*(Fine1_VII(:,i2,j2)-Coarse1_V),&
                  Beta*0.5*SignGradNormal_V*&
                  (Fine2_VII(:,i2,j2)-Fine1_VII(:,i2,j2)),&
                  cSixth*abs(GradNormal_V) + 0.25*SignGradNormal_V*&
                  (Fine2_VII(:,i2,j2)-Fine1_VII(:,i2,j2))))
          else
             ! First order scheme
             GradNormalLtd_V = 0.0
          end if
          FineToCoarseF_VII(:,i2,j2) = Fine1_VII(:,i2,j2) - GradNormalLtd_V
          FineF_VII(:,i2,j2) = Fine1_VII(:,i2,j2) + GradNormalLtd_V
       end do;end do
    end if

  end subroutine tvd_reschange_body
  !============================================================================
  subroutine tvd_reschange(&
       Coarse2_V         ,& ! State in the coarser ghostcell, 2nd layer
       Coarse1_V         ,& ! State in the coarser ghostcell, 1st layer
       Fine1_VII         ,& ! States in 4 fine physical cells,1st layer
       Fine2_VII         ,& ! States in 4 fine physical cells,2nd layer
       CoarseToFineF_VII ,& ! Values at subfaces, in the coarse ghostcell
       FineToCoarseF_VII ,& ! Values in phys. cell looking at coarser cell
       FineF_VII)           ! Facevalues in the physical cell,
    !                         looking at another physical cell

    ! _____________|____________|________! _______|_
    !              |        CToF|FToC FF !       |
    !  C2_V        |C1_V       _|__F1_V__|__F2_V_|_
    !              |        CToF|FToC FF |       |
    ! _____________|____________|__F1_V__! __F2_V_|_
    !              |            |        |       |

    real, intent(in):: Coarse2_V(:), Coarse1_V(:)         ! dimension(nVar)
    real, intent(in):: Fine1_VII(:,:,:), Fine2_VII(:,:,:) ! dimension(nVar,2,2)
    real, intent(inout):: CoarseToFineF_VII(:,:,:)        ! dimension(nVar,2,2)
    real, intent(inout):: FineToCoarseF_VII(:,:,:)        ! dimension(nVar,2,2)
    real, intent(inout):: FineF_VII(:,:,:)                ! dimension(nVar,2,2)

    integer:: iVar, i2, j2
    real,dimension(nVar):: AveragedFine1_V
    real,dimension(nVar):: GradNormal_V,SignGradNormal_V
    real,dimension(nVar):: GradNormalLtd_V  ! Ltd stands for "Limited"

    real :: Beta
    character(len=*), parameter:: NameSub = 'tvd_reschange'
    !--------------------------------------------------------------------------
    ! Calculate averaged Fine1_VII
    do iVar=1,nVar
       AveragedFine1_V(iVar) = 0.25*sum(Fine1_VII(iVar,:,:))
    end do
    GradNormal_V = AveragedFine1_V - Coarse1_V

    ! Save gradients squared
    SignGradNormal_V = sign(1.0,GradNormal_V)

    Beta = min(BetaLimiterResChange, LimiterBeta)

    ! Limit gradient in the first coarser cell
    GradNormalLtd_V = SignGradNormal_V*max(0.0, min( &
         Beta*cTwoThird*abs(GradNormal_V),&
         Beta*0.5*SignGradNormal_V*(Coarse1_V - Coarse2_V), &
         cThird*abs(GradNormal_V) + 0.25*&
         SignGradNormal_V*(Coarse1_V - Coarse2_V)))

    do j2=1,2; do i2=1,2
       ! Limit transverse gradients, if they are larger than the normal one
       ! Before limiting the transverse gradients are equal to
       ! Fine1_VII-AveragedFine1V
       CoarseToFineF_VII(:,i2,j2) = Coarse1_V + GradNormalLtd_V + &
            sign(min(abs(GradNormalLtd_V),&
            abs(Fine1_VII(:,i2,j2) - AveragedFine1_V)),&
            Fine1_VII(:,i2,j2) - AveragedFine1_V)
    end do;end do

    do j2=1,2; do i2=1,2
       ! Limit gradient in the first layer of finer cells
       GradNormalLtd_V = SignGradNormal_V*max(0.0,min( &
            SignGradNormal_V*(Fine1_VII(:,i2,j2)-Coarse1_V),&
            Beta*cThird*abs(GradNormal_V),&
            Beta*0.5*SignGradNormal_V &
            *(Fine2_VII(:,i2,j2)-Fine1_VII(:,i2,j2)), &
            cSixth*abs(GradNormal_V)+0.25*SignGradNormal_V&
            *(Fine2_VII(:,i2,j2)-Fine1_VII(:,i2,j2)) &
            ))
       FineToCoarseF_VII(:,i2,j2) = Fine1_VII(:,i2,j2)-GradNormalLtd_V
       FineF_VII(:,i2,j2)         = Fine1_VII(:,i2,j2)+GradNormalLtd_V

    end do;end do

  end subroutine tvd_reschange
  !============================================================================
  subroutine accurate_reschange3d( &
       Coarse2_V         ,& ! State in the coarser ghostcell,  2nd layer
       Coarse1_VII       ,& ! State in the coarser ghostcells, 1st layer
       Fine1_VII         ,& ! States in 4 fine physical cells, 1st layer
       Fine2_VII         ,& ! States in 4 fine physical cells, 2nd layer
       CoarseToFineF_VII ,& ! Values at subfaces, in the coarse ghostcell
       FineToCoarseF_VII ,& ! Values in phys. cell looking at coarser cell
       FineF_VII,         & ! Values in phys. cell looking at fine cell
       DoConvert)
    !$acc routine seq

    !              |C1_V        |        |       |
    ! _____________|____________|________|_______|_
    !              |        CToF|FToC FF |       |
    !  C2_V        |C1_V       _|__F1_V__|__F2_V_|_
    !              |        CToF|FToC FF |       |
    ! _____________|____________|__F1_V__|__F2_V_|_
    !              |            |        |       |
    !              | C1_V       |        |       |

    real, intent(in) :: Coarse2_V(:)               ! dimension(nVar)
    real, intent(in) :: Coarse1_VII(:,:,:)         ! dimension(nVar,6,6)
    real, intent(in) :: Fine1_VII(:,:,:)           ! dimension(nVar,2,2)
    real, intent(in) :: Fine2_VII(:,:,:)           ! dimension(nVar,2,2)
    real, intent(inout):: CoarseToFineF_VII(:,:,:) ! dimension(nVar,2,2)
    real, intent(inout):: FineToCoarseF_VII(:,:,:) ! dimension(nVar,2,2)
    real, intent(inout):: FineF_VII(:,:,:)         ! dimension(nVar,2,2)

    logical, optional, intent(in):: DoConvert

    integer :: iVar, i2, j2
    real, dimension(nVar):: AveragedFine1_V, Slope1_V, Slope2_V
    real, dimension(nVar):: GradNormal_V, SignGradNormal_V
    real, dimension(nVar):: GradNormalLtd_V, FaceMiddle_V, Transverse_V
    real, dimension(nVar, 2, 2):: Coarse1Max_VII, Coarse1Min_VII

    real :: AverageOrig_V(nVar), AverageOrig
    real :: Coarse, Middle, FaceAverage, FaceTmp_II(2,2), Alpha, Alpha1
    real :: Beta
    real :: Denominator

    ! Calculate averaged Fine1_VII
    character(len=*), parameter:: NameSub = 'accurate_reschange3d'
    !--------------------------------------------------------------------------
    do iVar=1,nVar
       AveragedFine1_V(iVar) = 0.25*sum(Fine1_VII(iVar,:,:))
    end do
    GradNormal_V = AveragedFine1_V - Coarse1_VII(:,3,3)

    ! Save sign of the gradient
    SignGradNormal_V = sign(1.0,GradNormal_V)

    ! Limit gradient in the first coarser cell
    Slope1_V = cTwoThird*abs(GradNormal_V)
    Slope2_V = 0.5*SignGradNormal_V*(Coarse1_VII(:,3,3) - Coarse2_V)

    Beta = min(BetaLimiterResChange, LimiterBeta)

    GradNormalLtd_V= SignGradNormal_V*max(0.0,min( &
         Beta*Slope1_V, Beta*Slope2_V, 0.5*(Slope1_V+Slope2_V)))

    ! Add limited normal gradient to obtain the middle value for the fine face
    FaceMiddle_V = Coarse1_VII(:,3,3) + GradNormalLtd_V

    do j2=1,2; do i2=1,2
       ! Calculate transverse gradient between coarse cells
       do iVar = 1, nVar
          ! TransverseSlope = ( (Cside1 - Ccenter) + (Cside2 - Ccenter) ) / 4
          Transverse_V(iVar) = 0.0625* &
               ( sum(Coarse1_VII(iVar,4*i2-3:4*i2-2,3:4)) &
               + sum(Coarse1_VII(iVar,3:4,4*j2-3:4*j2-2)) &
               ) - 0.5*Coarse1_VII(iVar,3,3)
       end do

       ! Bound the face value by Coarse1, Coarse1+Transverse and Fine1
       Coarse1Max_VII(:,i2,j2) = Coarse1_VII(:,3,3) + max(0.0, Transverse_V)
       Coarse1Min_VII(:,i2,j2) = Coarse1_VII(:,3,3) + min(0.0, Transverse_V)

       ! Add transverse gradient and limit it
       CoarseToFineF_VII(:,i2,j2) = &
            max( min(Coarse1Min_VII(:,i2,j2), Fine1_VII(:,i2,j2)), &
            min( max(Coarse1Max_VII(:,i2,j2), Fine1_VII(:,i2,j2)), &
            FaceMiddle_V + Transverse_V) )

    end do; end do

    ! The average face value
    AverageOrig_V = 0.25* &
         ( CoarseToFineF_VII(:,1,1) + CoarseToFineF_VII(:,1,2) &
         + CoarseToFineF_VII(:,2,1) + CoarseToFineF_VII(:,2,2) )

    ! For each variable fix the face values if necessary
    do iVar = 1, nVar

       AverageOrig = AverageOrig_V(iVar)
       Coarse      = Coarse1_VII(iVar, 3, 3)
       Middle      = FaceMiddle_V(iVar)

       ! Check if the |L-M| <= |M-C| condition is satisfied
       if(abs(AverageOrig - Middle) <=  abs(Coarse - Middle) ) CYCLE

       ! Calculate the fixed average value L = Lorig + sgn(Lorig-M)*|M-C|
       FaceAverage = Middle + &
            sign( abs(Middle - Coarse), AverageOrig - Middle )

       ! Correct face values either upward or downward
       if(AverageOrig < FaceAverage)then
          FaceTmp_II = &
               max(Coarse1Max_VII(iVar,:,:), CoarseToFineF_VII(iVar,:,:))
       else
          FaceTmp_II = &
               min(Coarse1Min_VII(iVar,:,:), CoarseToFineF_VII(iVar,:,:))
       end if

       ! Calculate interpolation coefficient needed to satisfy the condition
       ! Avoid zero denominator.
       Denominator = 0.25*sum(FaceTmp_II) - AverageOrig
       if(abs(Denominator) < 1e-30) then
          CoarseToFineF_VII(iVar,:,:) = FaceTmp_II
       else
          Alpha = (FaceAverage - AverageOrig) / Denominator
          Alpha1 = 1.0 - Alpha

          ! Interpolate
          CoarseToFineF_VII(iVar,:,:) = &
               Alpha*FaceTmp_II + Alpha1*CoarseToFineF_VII(iVar,:,:)
       endif
    end do

    ! The face is half the distance in the fine cell
    Slope1_V = 0.5*Slope1_V
    do j2 = 1, 2; do i2 = 1, 2
       ! Limit gradient in the first layer of finer cells
       Slope2_V = 0.5*SignGradNormal_V*(Fine2_VII(:,i2,j2)-Fine1_VII(:,i2,j2))

       ! The first limiting ensures that the FineToCoarse face value
       ! remains between the Fine1 and Coarse values
       GradNormalLtd_V = SignGradNormal_V*max(0.0,min( &
            SignGradNormal_V*(Fine1_VII(:,i2,j2) - Coarse1_VII(:,3,3)), &
            Beta*Slope1_V, Beta*Slope2_V, 0.5*(Slope1_V + Slope2_V)))

       FineToCoarseF_VII(:,i2,j2) = Fine1_VII(:,i2,j2) - GradNormalLtd_V
       FineF_VII(:,i2,j2) = Fine1_VII(:,i2,j2) + GradNormalLtd_V

    end do; end do

    if(present(DoConvert))then
       do iVar = 1, nVar
          if(UseLogLimiter_V(iVar)) then
             CoarseToFineF_VII(iVar,:,:) = exp(CoarseToFineF_VII(iVar,:,:))
             FineToCoarseF_VII(iVar,:,:) = exp(FineToCoarseF_VII(iVar,:,:))
             FineF_VII(iVar,:,:)         = exp(FineF_VII(iVar,:,:))
          end if
       end do
    end if
  end subroutine accurate_reschange3d
  !============================================================================
  subroutine accurate_reschange2d(&
       Coarse2_V         ,& ! State in the coarser ghostcell,  2nd layer
       Coarse1_VI        ,& ! State in the coarser ghostcells, 1st layer
       Fine1_VI          ,& ! States in 2 fine physical cells, 1st layer
       Fine2_VI          ,& ! States in 2 fine physical cells, 2nd layer
       CoarseToFineF_VI  ,& ! Values at subfaces, in the coarse ghostcell
       FineToCoarseF_VI  ,& ! Values in phys. cell looking at coarser cell
       FineF_VI          ,& ! Values in phys. cell,looking at fine cell
       DoConvert)
    !$acc routine seq

    !              |C1_V        |        |       |
    ! _____________|____________|________|_______|_
    !              |        CToF|FToC FF |       |
    !  C2_V        |C1_V       _|__F1_V__|__F2_V_|_
    !              |        CToF|FToC FF |       |
    ! _____________|____________|__F1_V__|__F2_V_|_
    !              |            |        |       |
    !              | C1_V       |        |       |

    real, intent(in) :: Coarse2_V(:)            ! dimension(nVar)
    real, intent(in) :: Coarse1_VI(:,:)         ! dimension(nVar,6)
    real, intent(in) :: Fine1_VI(:,:)           ! dimension(nVar,2)
    real, intent(in) :: Fine2_VI(:,:)           ! dimension(nVar,2)

    real, intent(inout):: CoarseToFineF_VI(:,:) ! dimension(nVar,2)
    real, intent(inout):: FineToCoarseF_VI(:,:) ! dimension(nVar,2)
    real, intent(inout):: FineF_VI(:,:)         ! dimension(nVar,2)

    ! If DoConvert=.true, it assumes some variables of input have
    ! been converted to log, and the output will be converted back
    ! at the end of this subroutine.
    logical, optional, intent(in):: DoConvert

    integer:: iVar, i2
    real, dimension(nVar):: AveragedFine1_V, Slope1_V, Slope2_V
    real, dimension(nVar):: GradNormal_V, SignGradNormal_V
    real, dimension(nVar):: GradNormalLtd_V, FaceMiddle_V, Transverse_V
    real, dimension(nVar,2):: Coarse1Max_VI, Coarse1Min_VI

    real :: AverageOrig_V(nVar), AverageOrig
    real :: Coarse, Middle, FaceAverage, FaceTmp_I(2), Alpha, Alpha1
    real :: Beta
    real :: Denominator

    ! Calculate averaged Fine1_VI
    character(len=*), parameter:: NameSub = 'accurate_reschange2d'
    !--------------------------------------------------------------------------
    do iVar = 1, nVar
       AveragedFine1_V(iVar) = 0.5*sum(Fine1_VI(iVar,:))
    end do
    GradNormal_V = AveragedFine1_V - Coarse1_VI(:,3)

    ! Save sign of the gradient
    SignGradNormal_V = sign(1.0,GradNormal_V)

    ! Limit gradient in the first coarser cell
    Slope1_V = cTwoThird*abs(GradNormal_V)
    Slope2_V = 0.5*SignGradNormal_V*(Coarse1_VI(:,3) - Coarse2_V)

    Beta = min(BetaLimiterResChange, LimiterBeta)

    GradNormalLtd_V= SignGradNormal_V*max(0.0,min( &
         Beta*Slope1_V, Beta*Slope2_V, 0.5*(Slope1_V+Slope2_V)))

    ! Add limited normal gradient to obtain the middle value for the fine face
    FaceMiddle_V = Coarse1_VI(:,3) + GradNormalLtd_V

    do i2 = 1, 2
       ! Calculate transverse gradient between coarse cells
       do iVar = 1, nVar
          ! TransverseSlope = (Cside1 - Ccenter) / 4
          Transverse_V(iVar) = &
               0.125*sum(Coarse1_VI(iVar,4*i2-3:4*i2-2)) &
               - 0.25*Coarse1_VI(iVar,3)
       end do

       ! Bound the face value by Coarse1, Coarse1+Transverse and Fine1
       Coarse1Max_VI(:,i2) = Coarse1_VI(:,3) + max(0.0, Transverse_V)
       Coarse1Min_VI(:,i2) = Coarse1_VI(:,3) + min(0.0, Transverse_V)

       ! Add transverse gradient and limit it
       CoarseToFineF_VI(:,i2) = &
            max( min(Coarse1Min_VI(:,i2), Fine1_VI(:,i2)), &
            min( max(Coarse1Max_VI(:,i2), Fine1_VI(:,i2)), &
            FaceMiddle_V + Transverse_V) )

    end do

    ! The average face value
    AverageOrig_V = 0.5*( CoarseToFineF_VI(:,1) + CoarseToFineF_VI(:,2) )

    ! For each variable fix the face values if necessary
    do iVar = 1, nVar

       AverageOrig = AverageOrig_V(iVar)
       Coarse      = Coarse1_VI(iVar,3)
       Middle      = FaceMiddle_V(iVar)

       ! Check if the |L-M| <= |M-C| condition is satisfied
       if(abs(AverageOrig - Middle) <=  abs(Coarse - Middle) ) CYCLE

       ! Calculate the fixed average value L = Lorig + sgn(Lorig-M)*|M-C|
       FaceAverage = Middle + &
            sign( abs(Middle - Coarse), AverageOrig - Middle )

       ! Correct face values either upward or downward
       if(AverageOrig < FaceAverage)then
          FaceTmp_I = max(Coarse1Max_VI(iVar,:), CoarseToFineF_VI(iVar,:))
       else
          FaceTmp_I = min(Coarse1Min_VI(iVar,:), CoarseToFineF_VI(iVar,:))
       end if

       ! Calculate interpolation coefficient needed to satisfy the condition
       ! Avoid zero denominator.
       Denominator = 0.5*sum(FaceTmp_I) - AverageOrig
       if(abs(Denominator) < 1e-30) then
          CoarseToFineF_VI(iVar,:) = FaceTmp_I
       else
          Alpha = (FaceAverage - AverageOrig) / Denominator
          Alpha1 = 1.0 - Alpha
          ! Interpolate
          CoarseToFineF_VI(iVar,:) = &
               Alpha*FaceTmp_I + Alpha1*CoarseToFineF_VI(iVar,:)
       endif

    end do

    ! The face is half the distance in the fine cell
    Slope1_V = 0.5*Slope1_V
    do i2 = 1, 2
       ! Limit gradient in the first layer of finer cells
       Slope2_V = 0.5*SignGradNormal_V*(Fine2_VI(:,i2) - Fine1_VI(:,i2))

       ! The first limiting ensures that the FineToCoarse face value
       ! remains between the Fine1 and Coarse values
       GradNormalLtd_V = SignGradNormal_V*max(0.0, min( &
            SignGradNormal_V*(Fine1_VI(:,i2) - Coarse1_VI(:,3)), &
            Beta*Slope1_V, Beta*Slope2_V, 0.5*(Slope1_V + Slope2_V)))

       FineToCoarseF_VI(:,i2) = Fine1_VI(:,i2) - GradNormalLtd_V
       FineF_VI(:,i2) = Fine1_VI(:,i2) + GradNormalLtd_V

    end do

    if(present(DoConvert))then
       do iVar = 1, nVar
          if(UseLogLimiter_V(iVar)) then
             CoarseToFineF_VI(iVar,:) = exp(CoarseToFineF_VI(iVar,:))
             FineToCoarseF_VI(iVar,:) = exp(FineToCoarseF_VI(iVar,:))
             FineF_VI(iVar,:)         = exp(FineF_VI(iVar,:))
          end if
       end do
    end if
  end subroutine accurate_reschange2d
  !============================================================================
  subroutine accurate_reschange1d(&
       Coarse2_V         ,& ! State in the coarser ghostcell,  2nd layer
       Coarse1_V         ,& ! State in the coarser ghostcells, 1st layer
       Fine1_V           ,& ! States in 2 fine physical cells, 1st layer
       Fine2_V           ,& ! States in 2 fine physical cells, 2nd layer
       CoarseToFineF_V   ,& ! Values at face, in the coarse ghostcell
       FineToCoarseF_V   ,& ! Values in the phys. cell looking at coarser cell
       FineF_V)             ! Facevalues in the physical cell,
    !                         looking at another physical cell

    ! _____________|_____________|_______|_______|_
    !              |        CToF |FToC FF|       |
    !  C2_V        |C1_V         |F1_V   | F2_V  |
    ! _____________|_____________|_______|_______|_

    real, intent(in) :: Coarse2_V(:)         ! dimension(nVar)
    real, intent(in) :: Coarse1_V(:)         ! dimension(nVar)
    real, intent(in) :: Fine1_V(:)           ! dimension(nVar)
    real, intent(in) :: Fine2_V(:)           ! dimension(nVar)

    real, intent(inout):: CoarseToFineF_V(:) ! dimension(nVar)
    real, intent(inout):: FineToCoarseF_V(:) ! dimension(nVar)
    real, intent(inout):: FineF_V(:)         ! dimension(nVar)

    real, dimension(nVar):: Slope1_V, Slope2_V
    real, dimension(nVar):: GradNormal_V, SignGradNormal_V, GradNormalLtd_V

    real :: Beta

    character(len=*), parameter:: NameSub = 'accurate_reschange1d'
    !--------------------------------------------------------------------------

    ! Calculate averaged Fine1_VI
    GradNormal_V = Fine1_V - Coarse1_V

    ! Save sign of the gradient
    SignGradNormal_V = sign(1.0,GradNormal_V)

    ! Limit gradient in the first coarser cell
    Slope1_V = cTwoThird*abs(GradNormal_V)
    Slope2_V = 0.5*SignGradNormal_V*(Coarse1_V - Coarse2_V)

    Beta = min(BetaLimiterResChange, LimiterBeta)

    GradNormalLtd_V = SignGradNormal_V*max(0.0,min( &
         Beta*Slope1_V, Beta*Slope2_V, 0.5*(Slope1_V+Slope2_V)))

    ! Add limited normal gradient to obtain the middle value for the fine face
    CoarseToFineF_V = Coarse1_V + GradNormalLtd_V

    ! The face is half the distance in the fine cell
    Slope1_V = 0.5*Slope1_V
    ! Limit gradient in the first layer of finer cells
    Slope2_V = 0.5*SignGradNormal_V*(Fine2_V - Fine1_V)

    ! The first limiting ensures that the FineToCoarse face value
    ! remains between the Fine1 and Coarse values
    GradNormalLtd_V = SignGradNormal_V*max(0.0,min( &
         SignGradNormal_V*(Fine1_V - Coarse1_V), &
         Beta*Slope1_V, Beta*Slope2_V, 0.5*(Slope1_V + Slope2_V)))

    FineToCoarseF_V = Fine1_V - GradNormalLtd_V
    FineF_V         = Fine1_V + GradNormalLtd_V

  end subroutine accurate_reschange1d
  !============================================================================
  subroutine set_low_order_face

    use ModMain,  ONLY: MaxBlock, iMinFace, iMaxFace, jMinFace, jMaxFace, &
         kMinFace, kMaxFace, nIFace, nJFace, nKFace, nOrder, nBlock
    use ModB0
    use ModPhysics, ONLY: Gamma_I
    use ModAdvance, ONLY: LowOrderCrit_XB, LowOrderCrit_YB, LowOrderCrit_ZB, &
         State_VGB
    use BATL_lib, ONLY: block_inside_regions, Unused_B

    ! Set which faces should use low (up to second) order scheme
    ! Set logicals for the current block

    integer:: iBlock
    logical:: UseLowOrderOnly

    integer:: i, j, k
    real:: LowOrderCrit_X(nIFace,nJ,nK)
    real:: LowOrderCrit_Y(nI,nJFace,nK)
    real:: LowOrderCrit_Z(nI,nJ,nKFace)
    real, parameter:: cLowOrder = 1
    real, parameter:: cSmall = 1e-6

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_low_order_face'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(nOrder<2) then
       IsLowOrderOnly_B = .true.
    else
       IsLowOrderOnly_B = .false.
       UseLowOrder = &
            UseTrueCell .or. UseLowOrderRegion .or. UseAdaptiveLowOrder
       if(UseLowOrder) then
          do iBlock = 1, nBlock
             if (Unused_B(iBlock)) CYCLE
             UseLowOrderOnly = .false.

             if(.not. allocated(LowOrderCrit_XB)) then
                allocate(LowOrderCrit_XB( &
                     nI+1,jMinFace:jMaxFace,kMinFace:kMaxFace,MaxBlock))
                allocate(LowOrderCrit_YB( &
                     iMinFace:iMaxFace,nJ+1,kMinFace:kMaxFace,MaxBlock))
                allocate(LowOrderCrit_ZB( &
                     iMinFace:iMaxFace,jMinFace:jMaxFace,nK+1,MaxBlock))
             endif

             LowOrderCrit_XB(:,:,:,iBlock) = 0
             LowOrderCrit_YB(:,:,:,iBlock) = 0
             LowOrderCrit_ZB(:,:,:,iBlock) = 0

             ! Get the LowOrderCrit based on physical criteria. The value of
             ! 'LowOrderCrit_*B' is the ratio of the low order face values.
             if(UseAdaptiveLowOrder) call set_phys_based_low_order_face

             if(allocated(iRegionLowOrder_I)) then

                call block_inside_regions(iRegionLowOrder_I, iBlock, &
                     size(LowOrderCrit_X), 'xface',Value_I=LowOrderCrit_X)
                LowOrderCrit_XB(:,:,:,iBlock) = &
                     max(LowOrderCrit_X,LowOrderCrit_XB(:,:,:,iBlock))
                UseLowOrderOnly = all(LowOrderCrit_XB(:,:,:,iBlock) &
                     >= cLowOrder-cSmall)

                if(nDim > 1)then
                   call block_inside_regions(iRegionLowOrder_I, iBlock, &
                        size(LowOrderCrit_Y), 'yface', Value_I=LowOrderCrit_Y)
                   ! Use low order if the face is inside the low order region
                   ! OR satisfies the physical condition.
                   LowOrderCrit_YB(:,:,:,iBlock) = &
                        max(LowOrderCrit_Y,LowOrderCrit_YB(:,:,:,iBlock))
                   UseLowOrderOnly = UseLowOrderOnly .and. &
                        all(LowOrderCrit_YB(:,:,:,iBlock) >= cLowOrder-cSmall)
                end if

                if(nDim > 2)then
                   call block_inside_regions(iRegionLowOrder_I, iBlock, &
                        size(LowOrderCrit_Z), 'zface', Value_I=LowOrderCrit_Z)
                   LowOrderCrit_ZB(:,:,:,iBlock) = &
                        max(LowOrderCrit_Z,LowOrderCrit_ZB(:,:,:,iBlock))
                   UseLowOrderOnly = UseLowOrderOnly .and. &
                        all(LowOrderCrit_ZB(:,:,:,iBlock) >= cLowOrder-cSmall)
                end if

             else if(nOrder > 2 .and. UseTrueCell)then
                ! 1. When high-order scheme is used, change to low order for
                !    cells near the boundary.
                ! 2. 5th order scheme needs 3 cells on both sides of the face
                do k=1, nK; do j=1, nJ; do i = 1, nI+1
                   if(.not.all(Used_GB(i-3:i+2,j,k,iBlock))) &
                        LowOrderCrit_XB(i,j,k,iBlock) = cLowOrder
                end do; end do; end do
                if(nDim > 1)then
                   do k=1, nK; do j=1, nJ+1; do i = 1, nI
                      if(.not.all(Used_GB(i,j-3:j+2,j,iBlock))) &
                           LowOrderCrit_YB(i,j,k,iBlock) = cLowOrder
                   end do; end do; end do
                end if
                if(nDim > 2)then
                   do k=1, nK+1; do j=1, nJ; do i = 1, nI
                      if(.not.all(Used_GB(i,j,k-3:k+2,iBlock))) &
                           LowOrderCrit_ZB(i,j,k,iBlock) = cLowOrder
                   end do; end do; end do
                end if
             end if

             if(UseLowOrderOnly) IsLowOrderOnly_B(iBlock) = .true.

          enddo ! iBlock
       endif ! UseLowOrder
    endif ! nOrder

    !!! acc update device(IsLowOrderOnly_B)
    call test_stop(NameSub, DoTest)

  contains
    !==========================================================================
    subroutine set_phys_based_low_order_face

      ! Set criteria for low order scheme

      use ModAdvance, ONLY: Vel_IDGB

      integer :: iFace, jFace, kFace
      real:: State_VI(nVar,-3:2)
      integer :: iL, iR

      logical:: DoTest
      character(len=*), parameter:: NameSub = 'set_phys_based_low_order_face'
      !------------------------------------------------------------------------
      call test_start(NameSub, DoTest)

      if(nOrder > 2) then
         ! Use 6 neighbors to calculate the low order criteria
         iL = -3
         iR = 2
      else
         ! 2nd order is 'high order'. Use 4 neighbors to calculate
         ! the low order criteria
         iL = -2
         iR = 1
      endif

      State_VI = 0
      ! Face along x-direction
      do kFace = kMinFace, kMaxFace
         do jFace = jMinFace, jMaxFace
            do iFace = 1, nIFace
               State_VI(:,iL:iR) = &
                  State_VGB(:,iFace+iL:iFace+iR,jFace,kFace,iBlock)

               if(UseB0) State_VI(Bx_:Bz_,iL:iR) = &
                    State_VI(Bx_:Bz_,iL:iR) + &
                    B0_DGB(:,iFace+iL:iFace+iR,jFace,kFace,iBlock)

               LowOrderCrit_XB(iFace,jFace,kFace,iBlock) = &
                    low_order_face_criteria( &
                    State_VI(:,iL:iR), &
                    Vel_IDGB(:,x_,iFace+iL:iFace+iR,jFace,kFace,iBlock),&
                    iL,iR)
            enddo
         enddo
      enddo

      if(nDim>1) then
         ! Face along y-direction
         do kFace = kMinFace, kMaxFace
            do jFace = 1, nJFace
               do iFace=iMinFace,iMaxFace
                  State_VI(:,iL:iR) = &
                       State_VGB(:,iFace,jFace+iL:jFace+iR,kFace,iBlock)

                  if(UseB0) State_VI(Bx_:Bz_,iL:iR) = &
                       State_VI(Bx_:Bz_,iL:iR) + &
                       B0_DGB(:,iFace,jFace+iL:jFace+iR,kFace,iBlock)

                  LowOrderCrit_YB(iFace,jFace,kFace,iBlock) = &
                       low_order_face_criteria( &
                       State_VI(:,iL:iR), &
                       Vel_IDGB(:,y_,iFace,jFace+iL:jFace+iR,kFace,iBlock),&
                       iL,iR)
               enddo
            enddo
         enddo
      endif

      if(nDim>2) then
         ! Face along z-direction
         do kFace = 1, nKFace
            do jFace = jMinFace, jMaxFace
               do iFace = iMinFace, iMaxFace
                  State_VI(:,iL:iR) = &
                       State_VGB(:,iFace,jFace,kFace+iL:kFace+iR,iBlock)

                  if(UseB0) State_VI(Bx_:Bz_,iL:iR) = &
                       State_VI(Bx_:Bz_,iL:iR) + &
                       B0_DGB(:,iFace,jFace,kFace+iL:kFace+iR,iBlock)

                  LowOrderCrit_ZB(iFace,jFace,kFace,iBlock) = &
                       low_order_face_criteria( &
                       State_VI(:,iL:iR), &
                       Vel_IDGB(:,z_,iFace,jFace,kFace+iL:kFace+iR,iBlock),&
                       iL,iR)
               enddo
            enddo
         enddo
      endif

      call test_stop(NameSub, DoTest)

    end subroutine set_phys_based_low_order_face
    !==========================================================================
    real function low_order_face_criteria(State_VI, Vel_II, iL, iR)

      use ModMain, ONLY: UseB
      use ModMultiFluid, ONLY: iRho, iP, select_fluid

      real,    intent(in):: State_VI(:,:)
      real,    intent(in):: Vel_II(:,:)
      integer, intent(in):: iL, iR

      integer:: iFluid
      real:: pTotal_I(iL:iR), Sound_I(iL:iR)
      real:: Crit, pRatio, VelRatio, SoundMin
      !------------------------------------------------------------------------
      pTotal_I = 0
      VelRatio = 0
      do iFluid = 1, nFluid
         if(nFluid > 1) call select_fluid(iFluid)

         pTotal_I = pTotal_I + State_VI(iP,:)

         Sound_I  = sqrt(State_VI(iP,:)/State_VI(iRho,:)*Gamma_I(iFluid))
         SoundMin = minval(Sound_I)
         VelRatio = max(VelRatio, &
              (maxval(Vel_II(iFluid,:))-minval(Vel_II(iFluid,:)))/SoundMin)
      enddo

      Crit = 0
      if(VelRatio > VelCrit) then
         if(UseB) then
            pTotal_I = pTotal_I + 0.5*(State_VI(Bx_,:)**2 + &
                 State_VI(By_,:)**2 + State_VI(Bz_,:)**2)
         endif

         pRatio = maxval(pTotal_I)/minval(pTotal_I)
         ! pRatio >= pCritLow    -> low order
         ! pRatio < pCritHigh    -> high order
         ! otherwise             -> linear combination of low and high order
         ! This function returns the ratio of the low order face value
         if(pRatio >= pCritLow) then
            Crit = 1
         elseif(pRatio < pCritHigh) then
            Crit = 0
         else
            Crit = (pRatio - pCritHigh)/(pCritLow - pCritHigh)
         endif
      endif

      low_order_face_criteria = Crit

    end function low_order_face_criteria
    !==========================================================================
  end subroutine set_low_order_face
  !============================================================================
  subroutine calc_cell_norm_velocity

    use ModMain, ONLY: MaxBlock, nBlock
    use BATL_lib, ONLY: Xyz_DGB, IsCartesian, Unused_B, MaxDim, &
         iDim_, jDim_, kDim_
    use ModAdvance, ONLY: Vel_IDGB, State_VGB
    use ModMultiFluid, ONLY: nFluid

    integer:: iBlock

    integer :: iMin, iMax, jMin, jMax, kMin, kMax, i, j, k, iDim
    real :: GenXyz_DD(MaxDim, MaxDim)

    integer :: iFluid, iRho, iRhoUx, iRhoUy, iRhoUz

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_cell_norm_velocity'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(.not. allocated(Vel_IDGB))  allocate( &
         Vel_IDGB(nFluid,MaxDim,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))

    iMin = 1 - nG;       iMax = nI + nG
    jMin = 1 - nG*jDim_; jMax = nJ + nG*jDim_
    kMin = 1 - nG*kDim_; kMax = nK + nG*kDim_

    do iBlock = 1, nBlock
       if (Unused_B(iBlock)) CYCLE
       do iFluid = 1, nFluid
          iRho = iRho_I(iFluid)
          iRhoUx = iRhoUx_I(iFluid)
          iRhoUy = iRhoUy_I(iFluid)
          iRhoUz = iRhoUz_I(iFluid)

          if(IsCartesian) then
             Vel_IDGB(iFluid,x_,:,:,:,iBlock) = &
                  State_VGB(iRhoUx,:,:,:,iBlock)/State_VGB(iRho,:,:,:,iBlock)
             Vel_IDGB(iFluid,y_,:,:,:,iBlock) = &
                  State_VGB(iRhoUy,:,:,:,iBlock)/State_VGB(iRho,:,:,:,iBlock)
             Vel_IDGB(iFluid,z_,:,:,:,iBlock) = &
                  State_VGB(iRhoUz,:,:,:,iBlock)/State_VGB(iRho,:,:,:,iBlock)
          else
             ! Convert cell center velocity in xyz direction into grid
             ! aligned direction.
             do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
                GenXyz_DD = 0

                GenXyz_DD(:,x_) = Xyz_DGB(:,min(i+iDim_,iMax),j,k,iBlock) &
                     -            Xyz_DGB(:,max(i-iDim_,iMin),j,k,iBlock)
                GenXyz_DD(:,x_) = GenXyz_DD(:,x_)/norm2(GenXyz_DD(:,x_))

                if(nDim>1) then
                   GenXyz_DD(:,y_) = Xyz_DGB(:,i,min(j+jDim_,jMax),k,iBlock) &
                        -            Xyz_DGB(:,i,max(j-jDim_,jMin),k,iBlock)
                   GenXyz_DD(:,y_) = GenXyz_DD(:,y_)/norm2(GenXyz_DD(:,y_))
                endif

                if(nDim>2) then
                   GenXyz_DD(:,z_) = Xyz_DGB(:,i,j,min(k+kDim_,kMax),iBlock) &
                        -            Xyz_DGB(:,i,j,max(k-kDim_,kMin),iBlock)
                   GenXyz_DD(:,z_) = GenXyz_DD(:,z_)/norm2(GenXyz_DD(:,z_))
                endif

                do iDim = 1, MaxDim
                   Vel_IDGB(iFluid,iDim,i,j,k,iBlock) =  &
                        sum(State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock)* &
                        GenXyz_DD(:,iDim))/State_VGB(iRho,i,j,k,iBlock)
                enddo

             enddo; enddo; enddo

          endif ! IsCartesian
       enddo ! iFluid
    enddo ! iBlock

    call test_stop(NameSub, DoTest)
  end subroutine calc_cell_norm_velocity
  !============================================================================
  subroutine calc_face_div_u(iBlock)
    ! This subroutine 'estimates' div(V)*dl at the cell faces, where
    ! 'dl' is the cell size.
    ! The algorithm is implemented based on the paper of
    ! P. McCorquodale and P. Colella (2010). See section 2.52 of this paper
    ! for more details.

    use ModAdvance, ONLY: FaceDivU_IX, FaceDivU_IY, FaceDivU_IZ, &
         Vel_IDGB
    use BATL_size,   ONLY: nDim, jDim_, kDim_
    use ModMain,  ONLY: iMinFace, iMaxFace, jMinFace, jMaxFace, kMinFace, &
         kMaxFace, nIFace, nJFace, nKFace
    integer, intent(in)::iBlock

    integer :: iRho, iRhoUx, iRhoUy, iRhoUz
    integer :: iFluid, iFace, jFace, kFace
    integer :: iMin, iMax, jMin, jMax, kMin, kMax
    real:: Vel_DG(x_:z_,MinI:MaxI,MinJ:MaxJ,MinK:MaxK)

    character(len=*), parameter:: NameSub = 'calc_face_div_u'
    !--------------------------------------------------------------------------
    call timing_start(NameSub)

    iMin = 1 - nG;       iMax = nI + nG
    jMin = 1 - nG*jDim_; jMax = nJ + nG*jDim_
    kMin = 1 - nG*kDim_; kMax = nK + nG*kDim_

    do iFluid = 1, nFluid
       iRho = iRho_I(iFluid)
       iRhoUx = iRhoUx_I(iFluid)
       iRhoUy = iRhoUy_I(iFluid)
       iRhoUz = iRhoUz_I(iFluid)

       Vel_DG = Vel_IDGB(iFluid,:,:,:,:,iBlock)

       ! Assume dl ~ dx ~ dy ~ dz, then FaceDivU_IX/Y/Z = div(U)*dl
       ! See eq(35) of P. McCorquodale and P. Colella (2010)
       do kFace=kMinFace,kMaxFace
          do jFace=jMinFace,jMaxFace
             do iFace=1,nIFace
                FaceDivU_IX(iFluid,iFace,jFace,kFace) = &
                     (Vel_DG(x_,iFace,jFace,kFace) - &
                     Vel_DG(x_,iFace-1,jFace,kFace))

                if(nDim>1) FaceDivU_IX(iFluid,iFace,jFace,kFace) = &
                     FaceDivU_IX(iFluid,iFace,jFace,kFace)+ &
                     (Vel_DG(y_,iFace,jFace+1,kFace) - &
                     Vel_DG(y_,iFace,jFace-1,kFace) + &
                     Vel_DG(y_,iFace-1,jFace+1,kFace) - &
                     Vel_DG(y_,iFace-1,jFace-1,kFace))/4

                if(nDim>2) FaceDivU_IX(iFluid,iFace,jFace,kFace) = &
                     FaceDivU_IX(iFluid,iFace,jFace,kFace)+ &
                     (Vel_DG(z_,iFace,jFace,kFace+1) - &
                     Vel_DG(z_,iFace,jFace,kFace-1) + &
                     Vel_DG(z_,iFace-1,jFace,kFace+1) - &
                     Vel_DG(z_,iFace-1,jFace,kFace-1))/4

             enddo
          enddo
       enddo

       If(nDim>1) then
          do kFace=kMinFace,kMaxFace
             do jFace=1,nJFace
                do iFace=iMinFace,iMaxFace
                   FaceDivU_IY(iFluid,iFace,jFace,kFace) = &
                        (Vel_DG(y_,iFace,jFace,kFace) - &
                        Vel_DG(y_,iFace,jFace-1,kFace)) + &
                        (Vel_DG(x_,iFace+1,jFace,kFace) - &
                        Vel_DG(x_,iFace-1,jFace,kFace) + &
                        Vel_DG(x_,iFace+1,jFace-1,kFace) - &
                        Vel_DG(x_,iFace-1,jFace-1,kFace))/4

                   if(nDim>2) FaceDivU_IY(iFluid,iFace,jFace,kFace) = &
                        FaceDivU_IY(iFluid,iFace,jFace,kFace) + &
                        (Vel_DG(z_,iFace,jFace,kFace+1) - &
                        Vel_DG(z_,iFace,jFace,kFace-1) + &
                        Vel_DG(z_,iFace,jFace-1,kFace+1) - &
                        Vel_DG(z_,iFace,jFace-1,kFace-1))/4
                enddo
             enddo
          enddo
       endif

       if(nDim>2) then
          do kFace=1,nKFace
             do jFace=jMinFace,jMaxFace
                do iFace=iMinFace,iMaxFace
                   FaceDivU_IZ(iFluid,iFace,jFace,kFace) = &
                        (Vel_DG(z_,iFace,jFace,kFace) - &
                        Vel_DG(z_,iFace,jFace,kFace-1)) + &
                        (Vel_DG(x_,iFace+1,jFace,kFace) - &
                        Vel_DG(x_,iFace-1,jFace,kFace) + &
                        Vel_DG(x_,iFace+1,jFace,kFace-1) - &
                        Vel_DG(x_,iFace-1,jFace,kFace-1))/4 + &
                        (Vel_DG(y_,iFace,jFace+1,kFace) - &
                        Vel_DG(y_,iFace,jFace-1,kFace) + &
                        Vel_DG(y_,iFace,jFace+1,kFace-1) - &
                        Vel_DG(y_,iFace,jFace-1,kFace-1))/4
                enddo
             enddo
          enddo
       endif

    enddo ! iFluid

    call timing_stop(NameSub)
  end subroutine calc_face_div_u
  !============================================================================
  subroutine limiter_mp(lMin, lMax, Cell_I, Cell2_I, iVar)

    integer, intent(in):: lMin, lMax  ! face index range, e.g. 1...nI+1
    real,    intent(in):: Cell_I(1-nG:MaxIJK+nG)
    real,    intent(in):: Cell2_I(1-nG:MaxIJK+nG)

    integer, optional, intent(in):: iVar        ! variable index

    ! Apply 5th order MP limiter to calculate face values.
    ! Use Cell_I to calculate FaceL_I and Cell2_I for FaceR_I.
    ! If iVar is present, apply the positivity check

    ! Coefficient for 5th order accurate interpolation
    real, parameter:: &
         c1 = 2/60., c2 = -13/60., c3 = 47/60., c4 = 27/60., c5 = -3/60.

    ! If the cell center value is used, the interpolatio for the face value
    ! is implemented with the coefficients below
    real, parameter:: &
         d1 = 3./128, d2 = -20./128, d3 = 90./128, d4=60./128, d5=-5./128

    real, parameter:: cFourThird = 4./3., c6 = 0.6

    ! Cell centered values at l, l+1, l+2, l-1, l-2
    real:: Cell, Cellp, Cellpp, Cellm, Cellmm

    ! Second derivatives
    real:: D2_I(-3:MaxIJK+4)

    ! Limited second derivatives at l+1/2 and l-1/2
    real:: D2p, D2m

    ! Various face values
    real:: FaceOrig, FaceMp, UpperLimit, Average, Median, LargeCurve
    real:: FaceMin, FaceMax

    integer:: l

    real:: Diff1, Diff2_I(3)

    real:: FaceLowOrder

    character(len=*), parameter:: NameSub = 'limiter_mp'
    !--------------------------------------------------------------------------

    ! Second derivative based on cell values (3 ghost cells are needed)
    do l = lMin-2, lMax+1
       D2_I(l) = Cell_I(l+1) - 2*Cell_I(l) + Cell_I(l-1)
    end do

    ! Limit left face first. Loop index l is for cell center, and face l+1/2
    do l = lMin-1, lMax-1

       Cellmm = Cell_I(l-2)
       Cellm  = Cell_I(l-1)
       Cell   = Cell_I(l)
       Cellp  = Cell_I(l+1)
       Cellpp = Cell_I(l+2)

       if(UseLowOrder) then
          if(LowOrderCrit_I(l+1) >=1) then
             CYCLE
          else
             FaceLowOrder = FaceL_I(l+1)
          endif

       endif

       if(UseFDFaceFlux) then
          ! Get the 5th order face value.
          FaceOrig = d1*Cellmm + d2*Cellm + d3*Cell + d4*Cellp + d5*Cellpp
       else
          ! 5th order interpolation
          FaceOrig = c1*Cellmm + c2*Cellm + c3*Cell + c4*Cellp + c5*Cellpp
       end if

       ! This is a quick check if there is a need to do any limiting
       FaceMp = Cell + minmod(Cellp - Cell, 4*(Cell - Cellm))

       if( (FaceOrig - Cell)*(FaceOrig - FaceMp) <= 1e-12)then
          FaceL_I(l+1) = FaceOrig
       else

          UpperLimit = Cell + 4*(Cell - Cellm)
          Average    = 0.5*(Cell + Cellp)

          Diff1 = max(abs(Cell2_I(l)-Cell2_I(l-1)),1e-14)
          Diff2_I(1) = abs(0.5*(Cell2_I(l-2) + Cell2_I(l) - 2*Cell2_I(l-1)))
          Diff2_I(2) = abs(0.5*(Cell2_I(l-1) + Cell2_I(l+1) - 2*Cell2_I(l)))
          Diff2_I(3) = abs(0.5*(Cell2_I(l) + Cell2_I(l+2) - 2*Cell2_I(l+1)))

          if(UseAccurateExtremum .or. maxval(Diff2_I)/Diff1 <0.5) then
             D2p = minmod4(4*D2_I(l) - D2_I(l+1),4*D2_I(l+1) - D2_I(l), &
                  D2_I(l), D2_I(l+1))
             D2m = minmod4(4*D2_I(l) - D2_I(l-1),4*D2_I(l-1) - D2_I(l), &
                  D2_I(l), D2_I(l-1))

             Median     = Average - 0.5*D2p
             LargeCurve = Cell + 0.5*(Cell - Cellm) + cFourThird*D2m
          else
             Median     = Cell
             LargeCurve = UpperLimit
          endif

          ! Note: FaceMin <= Cell, FaceMax >= Cell, so FaceMin <= FaceMax
          FaceMin = max(min(Cell, Cellp, Median), &
               min(Cell, UpperLimit, LargeCurve))

          FaceMax = min(max(Cell, Cellp, Median), &
               max(Cell, UpperLimit, LargeCurve))

          ! FaceL = median(FaceOrig, FaceMin, FaceMax)
          FaceL_I(l+1) = min(FaceMax, max(FaceMin, FaceOrig))

       end if

       if(UseLowOrder) then
          FaceL_I(l+1) = (1-LowOrderCrit_I(l+1))*FaceL_I(l+1) + &
               LowOrderCrit_I(l+1)*FaceLowOrder
       endif

       ! If the face value is a very small fraction of the cell
       ! then switch to first order scheme. This can occur at
       ! a shock or a smooth minimum very close to zero.
       if(present(iVar))then
          if(DefaultState_V(iVar) > 0.0 .and. &
               FaceL_I(l+1) < c6*Cell) FaceL_I(l+1) = Cell
       end if

    end do

    do l = lMin-2, lMax+1
       D2_I(l) = Cell2_I(l+1) - 2*Cell2_I(l) + Cell2_I(l-1)
    end do

    ! Limit right face. Loop index l is for cell center, and face l-1/2
    do l = lMin, lMax

       Cellmm = Cell2_I(l-2)
       Cellm  = Cell2_I(l-1)
       Cell   = Cell2_I(l)
       Cellp  = Cell2_I(l+1)
       Cellpp = Cell2_I(l+2)

       if(UseLowOrder) then
          if(LowOrderCrit_I(l) >=1) then
             CYCLE
          else
             FaceLowOrder = FaceR_I(l)
          endif
       endif

       if(UseFDFaceFlux) then
          ! FaceOrig
          FaceOrig = d1*Cellpp + d2*Cellp + d3*Cell + d4* Cellm + d5*Cellmm
       else
          ! 5th order interpolation
          FaceOrig = c1*Cellpp + c2*Cellp + c3*Cell + c4*Cellm + c5*Cellmm
       end if

       ! This is a quick check if there is a need to do any limiting
       FaceMp = Cell + minmod(Cellm - Cell, 4*(Cell - Cellp))

       if( (FaceOrig - Cell)*(FaceOrig - FaceMp) <= 1e-12)then
          FaceR_I(l) = FaceOrig
       else

          UpperLimit = Cell + 4*(Cell - Cellp)
          Average    = 0.5*(Cell + Cellm)

          Diff1 = max(abs(Cell2_I(l)-Cell2_I(l-1)),1e-14)
          Diff2_I(1) = abs(0.5*(Cell2_I(l-2) + Cell2_I(l) - 2*Cell2_I(l-1)))
          Diff2_I(2) = abs(0.5*(Cell2_I(l-1) + Cell2_I(l+1) - 2*Cell2_I(l)))
          Diff2_I(3) = abs(0.5*(Cell2_I(l) + Cell2_I(l+2) - 2*Cell2_I(l+1)))

          if(UseAccurateExtremum .or. maxval(Diff2_I)/Diff1 <0.5 ) then
             D2p = minmod4(4*D2_I(l) - D2_I(l+1),4*D2_I(l+1) - D2_I(l), &
                  D2_I(l), D2_I(l+1))
             D2m = minmod4(4*D2_I(l) - D2_I(l-1),4*D2_I(l-1) - D2_I(l), &
                  D2_I(l), D2_I(l-1))

             Median     = Average - 0.5*D2m
             LargeCurve = Cell + 0.5*(Cell - Cellp) + cFourThird*D2p
          else
             Median     = Cell
             LargeCurve = UpperLimit
          endif

          ! Note: FaceMin <= Cell, FaceMax >= Cell, so FaceMin <= FaceMax
          FaceMin = max(min(Cell, Cellm, Median), &
               min(Cell, UpperLimit, LargeCurve))

          FaceMax = min(max(Cell, Cellm, Median), &
               max(Cell, UpperLimit, LargeCurve))

          ! FaceR = median(FaceOrig, FaceMin, FaceMax)
          FaceR_I(l) = min(FaceMax, max(FaceMin, FaceOrig))

       end if

       if(UseLowOrder) then
          FaceR_I(l) = (1-LowOrderCrit_I(l))*FaceR_I(l) + &
               LowOrderCrit_I(l)*FaceLowOrder
       endif

       ! Check fraction limit
       if(present(iVar))then
          if(DefaultState_V(iVar) > 0.0 .and. &
               FaceR_I(l) < c6*Cell) FaceR_I(l) = Cell
       end if

    end do

  contains
    !==========================================================================
    real function minmod(a, b)

      real, intent(in):: a, b
      !------------------------------------------------------------------------
      minmod = (sign(0.5,a) + sign(0.5,b))*min(abs(a), abs(b))

    end function minmod
    !==========================================================================
    real function minmod4(a, b, c, d)

      real, intent(in):: a, b, c, d
      real:: SignSum
      !------------------------------------------------------------------------
      SignSum = sign(0.25,a) + sign(0.25,b) + sign(0.25,c) + sign(0.25,d)
      if(abs(SignSum) < 0.9)then
         minmod4 = 0.0
      else
         minmod4 = SignSum*min(abs(a), abs(b), abs(c), abs(d))
      end if
    end function minmod4
    !==========================================================================
  end subroutine limiter_mp
  !============================================================================
  subroutine calc_cweno_weight(lMin, lMax)

    integer, intent(in):: lMin, lMax

    ! Constants in eq(24)
    real, parameter:: c7over120 = 7./120, c1over6 = 1./6, c1over3 = 1./3
    real, parameter:: c5over6 = 5./6, c21over40 = 21./40, c11over6 = 11./6
    real, parameter:: c73over120 = 73./120, c7over6 = 7./6, c1over60 = 1./60
    real, parameter:: c13over3 = 13./3

    ! Constants for ECHO scheme(UseFDFaceFlux)
    real, parameter:: c3over8 = 3./8, c1over8 = 1./8, c6over8 = 3./4
    real, parameter:: c10over8 = 10./8, c15over8 = 15./8
    real, parameter:: c3over64 = 3./64, c1over16 = 1./16
    real, parameter:: c15over32 = 15./32, c9over16 = 9./16

    ! Generic cell index
    integer:: l

    ! Scalars for Cell_I(l-2:l+2)
    real:: Cellmm, Cellm, Cell, Cellp, Cellpp

    ! Polynomial coefficients for the linear and quadratic terms for the
    ! three second order polynomals using the l-2:l, l-1:l, l:l+2 stencils
    real:: a1_I(3), a2_I(3)

    ! linear coefficients of four low order polynamials. eq (13)
    real, parameter:: LinearCoeff_I(4) = [0.125, 0.25, 0.125, 0.5]

    ! Smoothness indicators. eq (19)
    real:: SmoothLocal_I(4)
    ! eq (24)
    real:: AlphaIS_I(4), Weight_I(4), w1, w2, w3, w4
    real:: SmoothMin, SmoothMax, Epsilon

    character(len=*), parameter:: NameSub = 'calc_cweno_weight'
    !--------------------------------------------------------------------------
    do l = lMin - 1, lMax

       Cellmm = Cell_I(l-2)
       Cellm  = Cell_I(l-1)
       Cell   = Cell_I(l)
       Cellp  = Cell_I(l+1)
       Cellpp = Cell_I(l+2)

       ! eq(8)
       a1_I(1) = 1.5*Cell - 2*Cellm + 0.5*Cellmm
       a2_I(1) = 0.5*Cell -   Cellm + 0.5*Cellmm

       ! eq(9)
       a1_I(2) = 0.5*Cellp - 0.5*Cellm
       a2_I(2) = 0.5*Cellp - Cell + 0.5*Cellm

       ! eq(10)
       a1_I(3) = -1.5*Cell + 2*Cellp - 0.5*Cellpp
       a2_I(3) =  0.5*Cell -   Cellp + 0.5*Cellpp

       ! Calculate indicator of smoothness
       ! eq(20)
       SmoothLocal_I(1:3) = a1_I**2 + c13over3*a2_I**2

       ! Based on G. Capedeville's code sent to us on Sept 2 2013
       SmoothMin = minval(SmoothLocal_I(1:3))
       SmoothMax = maxval(SmoothLocal_I(1:3))
       SmoothLocal_I(4) = SmoothMax

       ! This expression is from G. Capdeville's code
       Epsilon = sqrt(((SmoothMin + 1e-12)/(SmoothMax + 1e-12))**3)

       ! the following is just as fast with NAG 5.1
       ! Epsilon = ((SmoothMin + 1e-12)/(SmoothMax + 1e-12))**1.5

       ! This alternative expression comes from:
       ! A.A.I. pEER, et al., Appl. Math. Lett. 22 (2009) 1730-1733
       ! Epsilon = 1e-6*min(1.0, ((SmoothMin+1e-28)
       !                          /(SmoothMax-SmoothMin+1e-30))) + 1e-99

       ! eq(24)
       AlphaIS_I = LinearCoeff_I/(Epsilon + SmoothLocal_I)**2
       ! eq(23)
       Weight_I = AlphaIS_I/sum(AlphaIS_I)
       w1 = Weight_I(1)
       w2 = Weight_I(2)
       w3 = Weight_I(3)
       w4 = Weight_I(4)

       if(UseFDFaceFlux) then
          WeightL_II(-2,l) = c3over8*w1 - c3over64*w4
          WeightL_II(-1,l) = -(c10over8*w1 + c1over8*w2 - c1over16*w4)
          WeightL_II( 0,l) = c15over8*w1 + c6over8*w2 + c3over8*w3 + &
               c15over32*w4
          WeightL_II(+1,l) = c3over8*w2 + c6over8*w3 + c9over16*w4
          WeightL_II(+2,l) = -(c1over8*w3 + c3over64*w4)

          WeightR_II(-2,l) = -(c1over8*w1 + c3over64*w4)
          WeightR_II(-1,l) = c6over8*w1 + c3over8*w2 + c9over16*w4
          WeightR_II( 0,l) = c3over8*w1 + c6over8*w2 + c15over8*w3 + &
               c15over32*w4
          WeightR_II(+1,l) = -(c1over8*w2 + c10over8*w3 - c1over16*w4)
          WeightR_II(+2,l) = c3over8*w3 - c3over64*w4

       else
          ! Calculate interpolation weights used in eq (34) to obtain
          ! left and right face values
          WeightL_II(-2,l) = c1over3*w1 - c1over60*w4
          WeightL_II(-1,l) = -(c1over6*w2 + c7over6*w1 + c7over120*w4)
          WeightL_II( 0,l) = c5over6*w2 + c1over3*w3 + c11over6*w1 + &
               c73over120*w4
          WeightL_II(+1,l) = c1over3*w2 + c5over6*w3 + c21over40*w4
          WeightL_II(+2,l) = -(c1over6*w3 + c7over120*w4)

          WeightR_II(+2,l) = c1over3*w3 -c1over60*w4
          WeightR_II(+1,l) = -(c1over6*w2 + c7over6*w3 + c7over120*w4)
          WeightR_II( 0,l) = c5over6*w2 + c1over3*w1 + c11over6*w3 + &
               c73over120*w4
          WeightR_II(-1,l) = c1over3*w2 + c5over6*w1 + c21over40*w4
          WeightR_II(-2,l) = -(c1over6*w1 + c7over120*w4)
       endif
    end do

  end subroutine calc_cweno_weight
  !============================================================================
  subroutine limiter_cweno5(lMin, lMax, Cell_I, Cell2_I, iVar)

    ! G. Capdeville, A central WENO scheme for solving hyperbolic conservation
    ! laws on non-uniform meshes, J. Comput. Phys. 227 (2008) 2977-3014

    integer, intent(in):: lMin, lMax
    real,    intent(in):: Cell_I(1-nG:MaxIJK+nG)
    real,    intent(in):: Cell2_I(1-nG:MaxIJK+nG)
    integer, optional, intent(in):: iVar

    integer:: l

    character(len=*), parameter:: NameSub = 'limiter_cweno5'
    !--------------------------------------------------------------------------
    do l = lMin-1, lMax-1
       if(UseLowOrder)then
          if(UseLowOrder_I(l+1)) CYCLE
       end if

       ! eq (34)
       FaceL_I(l+1) = sum(WeightL_II(-2:2,l)*Cell_I(l-2:l+2))
    end do

    do l = lMin, lMax
       if(UseLowOrder)then
          if(UseLowOrder_I(l)) CYCLE
       end if

       ! eq (34)
       FaceR_I(l)   = sum(WeightR_II(-2:2,l)*Cell2_I(l-2:l+2))
    end do

    if(present(iVar))then
       if (DefaultState_V(iVar) > 0.0) then
          do l = lMin-1, lMax-1
             ! Make sure positive variables remain positive
             if(FaceL_I(l+1) < 0)&
                  FaceL_I(l+1) = 0.5*(Cell_I(l+1) + Cell_I(l))
          end do

          do l = lMin, lMax
             if(FaceR_I(l) < 0) &
                  FaceR_I(l) = 0.5*(Cell_I(l-1) + Cell_I(l))
          end do
       end if
    endif

  end subroutine limiter_cweno5
  !============================================================================
  subroutine limiter_body(lMin, lMax, Beta, Primitive_VI, &
       dVarLimL_VI, dVarLimR_VI)

    ! TVD limiters:
    ! mimod limiter:
    !                slim = minmod(s1,s2)
    !
    ! generalized MC (monotonized central) limiter:
    !                slim = minmod(beta*s1, beta*s2, (s1+s2)/2)
    !
    ! Koren limiter (mc3):
    !                slimL = minmod(beta*s1, beta*s2, (s1+2*s2)/3)
    !                slimR = minmod(beta*s1, beta*s2, (2*s1+s2)/3)
    !
    ! beta-limiter   slim = maxmod(minmod(beta*s1,s2), minmod(beta*s2,s1))
    !
    !                (see C.Hirsch, Numerical computation of
    !                 internal and external flows, Volume 2, page 544-545.)
    !
    ! where s1 and s2 are the unlimited slopes, the minmod function
    ! is zero if the arguments have different signs, otherwise it
    ! select the argument with the smallest absolute value, while
    ! the maxmod function selects the argument with the largest absolute value.
    !
    ! For beta=1.0 the MC and the beta limiters coincide with minmod.
    ! For beta=2.0 the beta limiter becomes the superbee limiter.
    !
    ! Note: the subroutines limiter() and limiter_body() calculate the
    !       HALF of the limited difference, so it can be applied simply as
    !
    !       left_face  = central_value - limited_slope_left
    !       right_face = central_value + limited_slope_right

    integer, intent(in):: lMin, lMax
    real,    intent(in):: Beta
    real, intent(inout):: Primitive_VI(1:nVar,1-nG:MaxIJK+nG)
    real, intent(inout):: dVarLimR_VI(1:nVar,0:MaxIJK+1)
    real, intent(inout):: dVarLimL_VI(1:nVar,0:MaxIJK+1)

    real,dimension(Hi3_):: dVar1_I, dVar2_I !
    real,dimension(nVar):: dVar1_V, dVar2_V ! unlimited left and right slopes
    integer :: l

    character(len=*), parameter:: NameSub = 'limiter_body'
    !--------------------------------------------------------------------------
    select case(TypeLimiter)
    case('beta')
       dVar1_I(1:nVar)=Primitive_VI(:,lMax+1)-Primitive_VI(:,lMax)
       dVar1_I(Lo2_:Hi2_)=abs(dVar1_I(1:nVar))
       dVar1_I(Lo3_:Hi3_)=Beta*dVar1_I(Lo2_:Hi2_)
       dVar1_I(1:nVar)=sign(0.25, dVar1_I(1:nVar))
       do l=lMax,lMin-1,-1
          dVar2_I=dVar1_I
          dVar1_I(1:nVar)=Primitive_VI(:,l)-Primitive_VI(:,l-1)
          dVar1_I(Lo2_:Hi2_)=abs(dVar1_I(1:nVar))
          dVar1_I(Lo3_:Hi3_)=Beta*dVar1_I(Lo2_:Hi2_)
          dVar1_I(1:nVar)=sign(0.25, dVar1_I(1:nVar))
          if(all(IsTrueCell_I(l-1:l+1)))then
             dVar2_I(1:nVar)=dVar2_I(1:nVar)+dVar1_I(1:nVar)
             dVar2_I(Lo2_:Hi2_)=min(dVar2_I(Lo2_:Hi2_), dVar1_I(Lo3_:Hi3_))
             dVar2_I(Lo3_:Hi3_)=min(dVar1_I(Lo2_:Hi2_), dVar2_I(Lo3_:Hi3_))
             dVar2_I(Lo2_:Hi2_)=max(dVar2_I(Lo2_:Hi2_), dVar2_I(Lo3_:Hi3_))
             dVarLimR_VI(:,l) = dVar2_I(1:nVar)*dVar2_I(Lo2_:Hi2_)
          else
             dVarLimR_VI(:,l) = 0.0
          end if
          dVarLimL_VI(:,l) = dVarLimR_VI(:,l)
       end do
    case('minmod')
       dVar1_I(1:nVar)=Primitive_VI(:,lMax+1)-Primitive_VI(:,lMax)
       dVar1_I(Lo2_:Hi2_)=abs(dVar1_I(1:nVar))
       dVar1_I(1:nVar)=sign(0.25, dVar1_I(1:nVar))
       do l=lMax,lMin-1,-1
          dVar2_I(1:Hi2_)=dVar1_I(1:Hi2_)
          dVar1_I(1:nVar)=Primitive_VI(:,l)-Primitive_VI(:,l-1)
          dVar1_I(Lo2_:Hi2_)=abs(dVar1_I(1:nVar))
          dVar1_I(1:nVar)=sign(0.25, dVar1_I(1:nVar))
          if(all(IsTrueCell_I(l-1:l+1)))then
             dVar2_I(1:nVar)=dVar2_I(1:nVar)+dVar1_I(1:nVar)
             dVar2_I(Lo2_:Hi2_)=min(dVar2_I(Lo2_:Hi2_), dVar1_I(Lo2_:Hi2_))
             dVarLimR_VI(:,l) = dVar2_I(1:nVar)*dVar2_I(Lo2_:Hi2_)
          else
             dVarLimR_VI(:,l) = 0.0
          end if
          dVarLimL_VI(:,l) = dVarLimR_VI(:,l)
       end do
    case('mc')
       dVar1_V = Primitive_VI(:,lMax+1) - Primitive_VI(:,lMax)
       do l=lMax,lMin-1,-1
          dVar2_V = dVar1_V
          dVar1_V = Primitive_VI(:,l) - Primitive_VI(:,l-1)
          if(all(IsTrueCell_I(l-1:l+1)))then
             dVarLimR_VI(:,l) = &
                  (sign(0.25, dVar1_V)+sign(0.25, dVar2_V))*&
                  min(Beta*abs(dVar1_V), Beta*abs(dVar2_V), &
                  0.5*abs(dVar1_V+dVar2_V))
          else
             dVarLimR_VI(:,l) = 0.0
          end if
          dVarLimL_VI(:,l) = dVarLimR_VI(:,l)
       end do
    case('mc3')
       dVar1_V = Primitive_VI(:,lMax+1) - Primitive_VI(:,lMax)
       do l=lMax,lMin-1,-1
          dVar2_V = dVar1_V
          dVar1_V = Primitive_VI(:,l) - Primitive_VI(:,l-1)
          if(all(IsTrueCell_I(l-1:l+1)))then
             dVarLimR_VI(:,l) = &
                  (sign(0.25, dVar1_V)+sign(0.25, dVar2_V))*&
                  min(Beta*abs(dVar1_V), Beta*abs(dVar2_V), &
                  cThird*abs(2*dVar1_V+dVar2_V))
             dVarLimL_VI(:,l) = &
                  (sign(0.25, dVar1_V)+sign(0.25, dVar2_V))*&
                  min(Beta*abs(dVar1_V), Beta*abs(dVar2_V), &
                  cThird*abs(dVar1_V+2*dVar2_V))
          else
             dVarLimR_VI(:,l) = 0.0
             dVarLimL_VI(:,l) = 0.0
          end if
       end do
    case default
       call stop_mpi('limiter_body: unknown TypeLimiter='//TypeLimiter)
    end select

  end subroutine limiter_body
  !============================================================================
  subroutine limiter(lMin, lMax, Beta, Primitive_VI, dVarLimL_VI, dVarLimR_VI)

    ! Calculate left and right slopes from the primitive variables Primitive_VI
    ! for indexes lMin-1 to lMax.
    ! These are actually just differences (as if the cell size was one)
    ! multiplied by 0.5 for sake of simplifying the formulas.
    ! For all limiters except mc3 (Koren's limiter with adjustable Beta),
    ! dVarLimL_VI and dVarLimR_VI are equal.
    ! The Beta parameter is used in all limiters except for 'no' and 'minmod'
    ! to set the maximum slope ratio.
    ! The 'no' limiter does not apply any limiter at all, just
    ! calculates the second order accurate average slope.

    integer, intent(in):: lMin, lMax
    real,    intent(in):: Beta
    real, intent(inout):: Primitive_VI(1:nVar,1-nG:MaxIJK+nG)
    real, intent(inout):: dVarLimR_VI(1:nVar,0:MaxIJK+1)
    real, intent(inout):: dVarLimL_VI(1:nVar,0:MaxIJK+1)

    real,dimension(Hi3_):: dVar1_I, dVar2_I
    real,dimension(nVar):: dVar1_V, dVar2_V
    integer::l

    character(len=*), parameter:: NameSub = 'limiter'
    !--------------------------------------------------------------------------
    select case(TypeLimiter)
    case('no')
       do l = lMin-1, lMax
          ! Calculate the unlimited slope
          dVarLimR_VI(:,l) = 0.25*(Primitive_VI(:,l+1) - Primitive_VI(:,l-1))
          dVarLimL_VI(:,l) = dVarLimR_VI(:,l)
       end do

    case('beta')
       dVar1_I(1:nVar) = Primitive_VI(:,lMax+1) - Primitive_VI(:,lMax)
       dVar1_I(Lo2_:Hi2_) = abs(dVar1_I(1:nVar))
       dVar1_I(Lo3_:Hi3_) = Beta*dVar1_I(Lo2_:Hi2_)
       dVar1_I(1:nVar) = sign(0.25, dVar1_I(1:nVar))
       do l = lMax, lMin-1, -1
          dVar2_I = dVar1_I
          dVar1_I(1:nVar) = Primitive_VI(:,l) - Primitive_VI(:,l-1)
          dVar1_I(Lo2_:Hi2_) = abs(dVar1_I(1:nVar))
          dVar1_I(Lo3_:Hi3_) = Beta*dVar1_I(Lo2_:Hi2_)
          dVar1_I(1:nVar) = sign(0.25, dVar1_I(1:nVar))
          dVar2_I(1:nVar) = dVar2_I(1:nVar)+dVar1_I(1:nVar)
          dVar2_I(Lo2_:Hi2_) = min(dVar2_I(Lo2_:Hi2_), dVar1_I(Lo3_:Hi3_))
          dVar2_I(Lo3_:Hi3_) = min(dVar1_I(Lo2_:Hi2_), dVar2_I(Lo3_:Hi3_))
          dVar2_I(Lo2_:Hi2_) = max(dVar2_I(Lo2_:Hi2_), dVar2_I(Lo3_:Hi3_))
          dVarLimR_VI(:,l) = dVar2_I(1:nVar)*dVar2_I(Lo2_:Hi2_)

          dVarLimL_VI(:,l) = dVarLimR_VI(:,l)
       end do
    case('minmod')
       dVar1_I(1:nVar) = Primitive_VI(:,lMax+1) - Primitive_VI(:,lMax)
       dVar1_I(Lo2_:Hi2_) = abs(dVar1_I(1:nVar))
       dVar1_I(1:nVar) = sign(0.25, dVar1_I(1:nVar))
       do l = lMax, lMin-1, -1
          dVar2_I(1:Hi2_) = dVar1_I(1:Hi2_)
          dVar1_I(1:nVar) = Primitive_VI(:,l) - Primitive_VI(:,l-1)
          dVar1_I(Lo2_:Hi2_) = abs(dVar1_I(1:nVar))
          dVar1_I(1:nVar) = sign(0.25, dVar1_I(1:nVar))
          dVar2_I(1:nVar) = dVar2_I(1:nVar) + dVar1_I(1:nVar)
          dVar2_I(Lo2_:Hi2_) = min(dVar2_I(Lo2_:Hi2_), dVar1_I(Lo2_:Hi2_))
          dVarLimR_VI(:,l) = dVar2_I(1:nVar)*dVar2_I(Lo2_:Hi2_)

          dVarLimL_VI(:,l) = dVarLimR_VI(:,l)
       end do
    case('mc')
       ! Calculate rightmost unlimited slope
       dVar1_V = Primitive_VI(:,lMax+1) - Primitive_VI(:,lMax)
       do l = lMax, lMin-1, -1
          ! Propagate old left slope to become the right slope
          dVar2_V = dVar1_V
          ! Calculate left slope
          dVar1_V = Primitive_VI(:,l) - Primitive_VI(:,l-1)
          ! Calculate the limited slope
          dVarLimR_VI(:,l) = (sign(0.25, dVar1_V) + sign(0.25, dVar2_V))* &
               min(Beta*abs(dVar1_V), Beta*abs(dVar2_V), &
               0.5*abs(dVar1_V+dVar2_V))

          dVarLimL_VI(:,l) = dVarLimR_VI(:,l)
       end do
    case('mc3')
       ! Calculate rightmost unlimited slope
       dVar1_V = Primitive_VI(:,lMax+1) - Primitive_VI(:,lMax)
       do l = lMax, lMin-1, -1
          ! Propagate old left slope to become the right slope
          dVar2_V = dVar1_V
          ! Calculate left slope
          dVar1_V = Primitive_VI(:,l) - Primitive_VI(:,l-1)
          ! Calculate the limited slopes
          dVarLimR_VI(:,l) = (sign(0.25, dVar1_V) + sign(0.25, dVar2_V))* &
               min(Beta*abs(dVar1_V), Beta*abs(dVar2_V), &
               cThird*abs(2*dVar1_V+dVar2_V))
          dVarLimL_VI(:,l) = (sign(0.25, dVar1_V) + sign(0.25, dVar2_V))* &
               min(Beta*abs(dVar1_V), Beta*abs(dVar2_V), &
               cThird*abs(dVar1_V+2*dVar2_V))
       end do
    case default
       call stop_mpi('limiter: unknown TypeLimiter='//TypeLimiter)
    end select

  end subroutine limiter
  !============================================================================
end module ModFaceValue
!==============================================================================

