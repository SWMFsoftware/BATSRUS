!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModConserveFlux

  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest

  use BATL_size, ONLY: nDim
  use ModSize, ONLY: nI, nJ, nK, MaxBlock, MaxDim
  use ModVarIndexes, ONLY: Bx_, By_, Bz_,B_,U_, Ex_, Rho_

  use ModMain, ONLY: UseB, UseB0
  use ModAdvance, ONLY: &
       Flux_VXI, Flux_VYI, Flux_VZI, &
       UnFirst_, UnLast_,  Vdt_, BnL_, BnR_, LogAlfven_, nFaceValue, &
       LeftState_VX, LeftState_VY, LeftState_VZ, &
       RightState_VX, RightState_VY, RightState_VZ

  !    UseMhdMomentumFlux, MhdFlux_VX, MhdFlux_VY, MhdFlux_VZ

  use ModGeometry,  ONLY: Used_GB
  use ModParallel, ONLY : DiLevel_EB, jBlock_IEB, jProc_IEB
  use BATL_lib, ONLY: &
       IsCartesianGrid, IsCartesian, IsRzGeometry, &
       CellFace_DFB, FaceNormal_DDFB, Unused_BP

  implicit none

  SAVE

  private ! except

  public:: init_mod_cons_flux
  public:: save_cons_flux
  public:: apply_cons_flux

  ! Correct face flux near resolution change.
  logical, public :: DoConserveFlux = .true.

  ! Face conservative or corrected flux.
  real, public, allocatable, dimension(:,:,:,:,:) :: &
       CorrectedFlux_VXB, CorrectedFlux_VYB, CorrectedFlux_VZB

  ! For momentum conserving scheme (for hybrid or multi-fluid) Mhd flux of
  ! momentum should be saved, the condition is UseB_ (B_-U_>0) and not
  ! UseEField (Ex_>1)
  ! integer, parameter :: MhdRhoUx_ = BnR_ +        min(max(2-Ex_,0), B_-U_)
  ! integer, parameter :: MhdRhoUz_ = BnR_ + MaxDim*min(max(2-Ex_,0), B_-U_)
  ! integer, public, parameter:: nCorrectedFaceValues = MhdRhoUz_

  real, parameter :: FaceRatio = 1.0/2**(nDim-1)

contains
  !============================================================================
  subroutine init_mod_cons_flux

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_mod_cons_flux'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(.not.allocated(CorrectedFlux_VXB)) allocate( &
         CorrectedFlux_VXB(nFaceValue,nJ,nK,2,MaxBlock), &
         CorrectedFlux_VYB(nFaceValue,nI,nK,2,MaxBlock), &
         CorrectedFlux_VZB(nFaceValue,nI,nJ,2,MaxBlock)  )

    CorrectedFlux_VXB = 0.0
    CorrectedFlux_VYB = 0.0
    CorrectedFlux_VZB = 0.0

    call test_stop(NameSub, DoTest)
  end subroutine init_mod_cons_flux
  !============================================================================
  subroutine save_cons_flux(iBlock)

    integer, intent(in) :: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'save_cons_flux'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if (DiLevel_EB(1,iBlock) == 1) &
         call save_corrected_flux_x(1, 1)
    if (DiLevel_EB(2,iBlock) == 1) &
         call save_corrected_flux_x(nI+1, 2)
    if(nDim > 1 .and. DiLevel_EB(3,iBlock) == 1) &
         call save_corrected_flux_y(1,1)
    if(nDim > 1 .and. DiLevel_EB(4,iBlock) == 1) &
         call save_corrected_flux_y(nJ+1,2)
    if (nDim > 2 .and. DiLevel_EB(5,iBlock) == 1) &
         call save_corrected_flux_z(1, 1)
    if (nDim > 2 .and. DiLevel_EB(6,iBlock) == 1) &
         call save_corrected_flux_z(nK+1, 2)

    call test_stop(NameSub, DoTest, iBlock)

  contains
    !==========================================================================
    subroutine save_corrected_flux_x(lFaceFrom, lFaceTo)

      integer, intent(in):: lFaceFrom, lFaceTo

      integer :: j, k, iVar
      !------------------------------------------------------------------------
      do k = 1, nK; do j = 1, nJ
         CorrectedFlux_VXB(1:Vdt_,j,k,lFaceTo,iBlock)  &
              = Flux_VXI(1:Vdt_,lFaceFrom,j,k,1)
      end do; end do

      if(.not.UseB)RETURN

      if(IsCartesian)then
         do k = 1, nK; do j = 1, nJ
            CorrectedFlux_VXB(BnL_,j,k,lFaceTo,iBlock) = &
                 LeftState_VX(Bx_,lFaceFrom,j,k)*FaceRatio
            CorrectedFlux_VXB(BnR_,j,k,lFaceTo,iBlock) = &
                 RightState_VX(Bx_,lFaceFrom,j,k)*FaceRatio
         end do; end do
      elseif(IsRzGeometry)then
         ! X face areas vary in RZ geometry
         do k = 1, nK; do j = 1, nJ
            CorrectedFlux_VXB(BnL_,j,k,lFaceTo,iBlock) = &
                 LeftState_VX(Bx_, lFaceFrom,j,k) * &
                 CellFace_DFB(1,lFaceFrom,j,k,iBlock)
            CorrectedFlux_VXB(BnR_,j,k,lFaceTo,iBlock) = &
                 RightState_VX(Bx_,lFaceFrom,j,k) * &
                 CellFace_DFB(1,lFaceFrom,j,k,iBlock)
         end do; end do
      else
         ! Dot product with face normal
         do k = 1, nK; do j = 1, nJ
            CorrectedFlux_VXB(BnL_,j,k,lFaceTo,iBlock) = &
                 dot_product(LeftState_VX(Bx_:B_+nDim,lFaceFrom,j,k), &
                 FaceNormal_DDFB(:,1,lFaceFrom, j, k, iBlock))
            CorrectedFlux_VXB(BnR_, j, k, lFaceTo, iBlock) = &
                 dot_product(RightState_VX(Bx_:B_+nDim,lFaceFrom,j,k), &
                 FaceNormal_DDFB(:,1,lFaceFrom,j,k,iBlock))
         end do; end do
      end if
      ! if(UseMhdMomentumFlux)then
      !    do k = 1, nK; do j = 1, nJ
      !       CorrectedFlux_VXB(MhdRhoUx_:MhdRhoUz_,j,k,lFaceTo,iBlock) = &
      !            MhdFlux_VX(:,lFaceFrom,j,k)
      !    end do; end do
      ! end if

      if(DoTest)then
         write(*,*)NameSub,' lFaceFrom, lFaceTo=',lFaceFrom, lFaceTo
         do iVar = UnFirst_, UnLast_
            write(*,*)NameSub,' iVar, uDotA=', &
                 Flux_VXI(iVar,lFaceFrom,jTest,kTest,1)
         end do
         do iVar = 1, nFaceValue
            write(*,*)NameSub,' iVar, flux=', iVar, &
                 CorrectedFlux_VXB(iVar,iTest,jTest,kTest,iBlock)
         end do
      end if

    end subroutine save_corrected_flux_x
    !==========================================================================
    subroutine save_corrected_flux_y(lFaceFrom, lFaceTo)

      integer, intent(in):: lFaceFrom, lFaceTo

      integer :: i, k
      !------------------------------------------------------------------------
      do k = 1, nK; do i = 1, nI
         CorrectedFlux_VYB(1:Vdt_,i,k,lFaceTo,iBlock)    &
              = Flux_VYI(1:Vdt_,i,lFaceFrom,k,1)
      end do; end do

      if(.not.UseB)RETURN

      if(IsCartesianGrid)then
         do k = 1, nK; do i = 1, nI
            CorrectedFlux_VYB(BnL_,i,k,lFaceTo,iBlock) = &
                 LeftState_VY( By_,i,lFaceFrom,k)*FaceRatio
            CorrectedFlux_VYB(BnR_,i,k,lFaceTo,iBlock) = &
                 RightState_VY(By_,i,lFaceFrom,k)*FaceRatio
         end do; end do
      else
         do k = 1, nK; do i = 1, nI
            CorrectedFlux_VYB(BnL_,i,k,lFaceTo,iBlock) = &
                 dot_product(LeftState_VY(Bx_:B_+nDim,i,lFaceFrom,k), &
                 FaceNormal_DDFB(:,2,i,lFaceFrom,k,iBlock))
            CorrectedFlux_VYB(BnR_,i,k,lFaceTo,iBlock) = &
                 dot_product(RightState_VY(Bx_:B_+nDim,i,lFaceFrom,k), &
                 FaceNormal_DDFB(:,2,i,lFaceFrom,k,iBlock))
         end do; end do
      end if
      ! if(UseMhdMomentumFlux)then
      !    do k = 1, nK; do i = 1, nI
      !       CorrectedFlux_VYB(MhdRhoUx_:MhdRhoUz_,i,k,lFaceTo,iBlock) = &
      !            MhdFlux_VY(:,i,lFaceFrom,k)
      !    end do; end do
      ! end if

    end subroutine save_corrected_flux_y
    !==========================================================================
    subroutine save_corrected_flux_z(lFaceFrom, lFaceTo)

      integer, intent(in):: lFaceFrom, lFaceTo

      integer :: i, j
      !------------------------------------------------------------------------
      do j = 1, nJ; do i = 1, nI
         CorrectedFlux_VZB(1:Vdt_,  i,j,lFaceTo,iBlock)    &
              = Flux_VZI(1:Vdt_,i,j,lFaceFrom,1)
      end do; end do

      if(.not.UseB)RETURN

      if(IsCartesianGrid)then
         do j = 1, nJ; do i = 1, nI
            CorrectedFlux_VZB(BnL_,i,j,lFaceTo,iBlock)= &
                 LeftState_VZ( Bz_,i,j,lFaceFrom)*FaceRatio
            CorrectedFlux_VZB(BnR_,i,j,lFaceTo,iBlock)= &
                 RightState_VZ(Bz_,i,j,lFaceFrom)*FaceRatio
         end do; end do
      else
         do j = 1, nJ; do i = 1, nI
            CorrectedFlux_VZB(BnL_,i,j,lFaceTo,iBlock) = &
                 dot_product(LeftState_VZ(Bx_:B_+nDim,i,j,lFaceFrom), &
                 FaceNormal_DDFB(:,3,i,j,lFaceFrom,iBlock))
            CorrectedFlux_VZB(BnR_,i,j,lFaceTo, iBlock) = &
                 dot_product(RightState_VZ(Bx_:B_+nDim,i,j,lFaceFrom), &
                 FaceNormal_DDFB(:,3,i,j,lFaceFrom,iBlock))
         end do; end do
      end if
      ! if(UseMhdMomentumFlux)then
      !    do j = 1, nJ; do i = 1, nI
      !       CorrectedFlux_VZB(MhdRhoUx_:MhdRhoUz_,i,j,lFaceTo,iBlock) = &
      !            MhdFlux_VZ(:,i,j,lFaceFrom)
      !    end do; end do
      ! end if

    end subroutine save_corrected_flux_z
    !==========================================================================
  end subroutine save_cons_flux
  !============================================================================
  subroutine apply_cons_flux(iBlock)
    use ModB0, ONLY: B0_DX, B0_DY, B0_DZ
    use ModTurbulence, ONLY: UseTurbulentCascade

    integer, intent(in):: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'apply_cons_flux'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if (DiLevel_EB(1,iBlock) == -1)then
       if(  .not.Unused_BP(jBlock_IEB(1,1,iBlock),jProc_IEB(1,1,iBlock)).and. &
            .not.Unused_BP(jBlock_IEB(2,1,iBlock),jProc_IEB(2,1,iBlock)).and. &
            .not.Unused_BP(jBlock_IEB(3,1,iBlock),jProc_IEB(3,1,iBlock)).and. &
            .not.Unused_BP(jBlock_IEB(4,1,iBlock),jProc_IEB(4,1,iBlock))) &
            call apply_corrected_flux_x(1, 1)
    end if

    if (DiLevel_EB(2,iBlock) == -1)then
       if(.not.Unused_BP(jBlock_IEB(1,2,iBlock),jProc_IEB(1,2,iBlock)).and. &
            .not.Unused_BP(jBlock_IEB(2,2,iBlock),jProc_IEB(2,2,iBlock)).and. &
            .not.Unused_BP(jBlock_IEB(3,2,iBlock),jProc_IEB(3,2,iBlock)).and. &
            .not.Unused_BP(jBlock_IEB(4,2,iBlock),jProc_IEB(4,2,iBlock))) &
            call apply_corrected_flux_x(2, nI+1)
    end if

    if (nDim > 1 .and. DiLevel_EB(3,iBlock) == -1)then
       if(.not.Unused_BP(jBlock_IEB(1,3,iBlock),jProc_IEB(1,3,iBlock)).and. &
            .not.Unused_BP(jBlock_IEB(2,3,iBlock),jProc_IEB(2,3,iBlock)).and. &
            .not.Unused_BP(jBlock_IEB(3,3,iBlock),jProc_IEB(3,3,iBlock)).and. &
            .not.Unused_BP(jBlock_IEB(4,3,iBlock),jProc_IEB(4,3,iBlock))) &
            call apply_corrected_flux_y(1, 1)
    end if

    if (nDim > 1 .and. DiLevel_EB(4,iBlock) == -1)then
       if(.not.Unused_BP(jBlock_IEB(1,4,iBlock),jProc_IEB(1,4,iBlock)).and. &
            .not.Unused_BP(jBlock_IEB(2,4,iBlock),jProc_IEB(2,4,iBlock)).and. &
            .not.Unused_BP(jBlock_IEB(3,4,iBlock),jProc_IEB(3,4,iBlock)).and. &
            .not.Unused_BP(jBlock_IEB(4,4,iBlock),jProc_IEB(4,4,iBlock))) &
            call apply_corrected_flux_y(2, nJ+1)
    end if
    if (nDim > 2 .and. DiLevel_EB(5,iBlock) == -1)then
       if(.not.Unused_BP(jBlock_IEB(1,5,iBlock),jProc_IEB(1,5,iBlock)).and. &
            .not.Unused_BP(jBlock_IEB(2,5,iBlock),jProc_IEB(2,5,iBlock)).and. &
            .not.Unused_BP(jBlock_IEB(3,5,iBlock),jProc_IEB(3,5,iBlock)).and. &
            .not.Unused_BP(jBlock_IEB(4,5,iBlock),jProc_IEB(4,5,iBlock))) &
            call apply_corrected_flux_z(1, 1)
    end if

    if (nDim > 2 .and. DiLevel_EB(6,iBlock) == -1)then
       if(.not.Unused_BP(jBlock_IEB(1,6,iBlock),jProc_IEB(1,6,iBlock)).and. &
            .not.Unused_BP(jBlock_IEB(2,6,iBlock),jProc_IEB(2,6,iBlock)).and. &
            .not.Unused_BP(jBlock_IEB(3,6,iBlock),jProc_IEB(3,6,iBlock)).and. &
            .not.Unused_BP(jBlock_IEB(4,6,iBlock),jProc_IEB(4,6,iBlock))) &
            call apply_corrected_flux_z(2, nK+1)
    end if
    call test_stop(NameSub, DoTest, iBlock)
  contains
    !==========================================================================
    subroutine apply_corrected_flux_x(lFaceFrom, lFaceTo)

      integer, intent(in):: lFaceFrom, lFaceTo

      integer:: j, k

      real :: Rho, FullB_D(3)
      !------------------------------------------------------------------------
      do k = 1, nK; do j = 1, nJ
         if (.not.Used_GB(lFaceTo-1, j, k, iBlock)) CYCLE
         if (.not.Used_GB(lFaceTo  , j, k, iBlock)) CYCLE

         Flux_VXI(1:Vdt_,lFaceTo,j,k,1)  = &
              CorrectedFlux_VXB(1:Vdt_,j,k,lFaceFrom,iBlock)

         ! if(UseMhdMomentumFlux) &
         !     MhdFlux_VX(:,lFaceTo,j,k)  = CorrectedFlux_VXB(&
         !     MhdRhoUx_:MhdRhoUz_,j,k,lFaceFrom,iBlock)
         if(.not.(UseB .and. IsCartesianGrid))CYCLE

         if(IsCartesian)then
            LeftState_VX(Bx_,lFaceTo,j,k) = &
                 CorrectedFlux_VXB(BnL_,j,k,lFaceFrom,iBlock)
            RightState_VX(Bx_,lFaceTo,j,k) = &
                 CorrectedFlux_VXB(BnR_,j,k,lFaceFrom,iBlock)
         else
            ! RZ geometry
            LeftState_VX(Bx_,lFaceTo,j,k) = &
                 CorrectedFlux_VXB(BnL_,j,k,lFaceFrom,iBlock) &
                 / CellFace_DFB(1,lFaceTo,j,k,iBlock)
            RightState_VX(Bx_,lFaceTo,j,k) = &
                 CorrectedFlux_VXB(BnR_,j,k,lFaceFrom,iBlock) &
                 / CellFace_DFB(1,lFaceTo,j,k,iBlock)
         end if

      end do; end do
      if(UseB .and. .not.IsCartesianGrid) &
           call apply_bn_face_i(lFaceFrom, lFaceTo, iBlock)

      if(UseTurbulentCascade) then
         ! Since face B changed, recalculate log(Alfven_speed)
         do k = 1, nK; do j = 1, nJ
            if (.not.Used_GB(lFaceTo-1, j, k, iBlock)) CYCLE
            if (.not.Used_GB(lFaceTo  , j, k, iBlock)) CYCLE

            FullB_D = 0.5*(LeftState_VX(Bx_:Bz_,lFaceTo,j,k) &
                 + RightState_VX(Bx_:Bz_,lFaceTo,j,k))
            if(UseB0) FullB_D = FullB_D + B0_DX(:,lFaceTo,j,k)
            Rho = 0.5*(LeftState_VX(Rho_,lFaceTo,j,k) &
                 +     RightState_VX(Rho_,lFaceTo,j,k))
            Flux_VXI(LogAlfven_,lFaceTo,j,k,1) = &
                 0.5*log(max(sum(FullB_D**2), 1e-30)/Rho)
         end do; end do
      end if

    end subroutine apply_corrected_flux_x
    !==========================================================================
    subroutine apply_corrected_flux_y(lFaceFrom, lFaceTo)

      integer, intent(in):: lFaceFrom, lFaceTo

      integer:: i, k

      real :: Rho, FullB_D(3)
      !------------------------------------------------------------------------
      do k = 1, nK; do i = 1, nI
         if (.not.Used_GB(i, lFaceTo-1, k, iBlock))CYCLE
         if (.not.Used_GB(i, lFaceTo  , k, iBlock))CYCLE

         Flux_VYI(1:Vdt_,i,lFaceTo,k,1)  = &
              CorrectedFlux_VYB(1:Vdt_,i,k,lFaceFrom,iBlock)

         ! if(UseMhdMomentumFlux)&
         !     MhdFlux_VY(:,i,lFaceTo,k)  = CorrectedFlux_VYB(&
         !     MhdRhoUx_:MhdRhoUz_,i,k,lFaceFrom,iBlock)
         if(IsCartesianGrid .and. UseB)then
            LeftState_VY(By_,i,lFaceTo,k) = &
                 CorrectedFlux_VYB(BnL_,i,k,lFaceFrom,iBlock)
            RightState_VY(By_,i,lFaceTo,k) = &
                 CorrectedFlux_VYB(BnR_,i,k,lFaceFrom,iBlock)
         end if
      end do; end do
      if(.not.IsCartesianGrid .and. UseB) &
           call apply_bn_face_j(lFaceFrom, lFaceTo, iBlock)

      if(UseTurbulentCascade) then
         ! Since face B changed, recalculate log(Alfven_speed)
         do k = 1, nK; do i = 1, nI
            if (.not.Used_GB(i, lFaceTo-1, k, iBlock))CYCLE
            if (.not.Used_GB(i, lFaceTo  , k, iBlock))CYCLE
            FullB_D = 0.5*(LeftState_VY(Bx_:Bz_,i,lFaceTo,k) &
                 + RightState_VY(Bx_:Bz_,i,lFaceTo,k))
            if(UseB0) FullB_D = FullB_D + B0_DY(:,i,lFaceTo,k)
            Rho = 0.5*(LeftState_VY(Rho_,i,lFaceTo,k) &
                 +     RightState_VY(Rho_,i,lFaceTo,k))
            Flux_VYI(LogAlfven_,i,lFaceTo,k,1) = &
                 0.5*log(max(sum(FullB_D**2), 1e-30)/Rho)
         end do; end do
      end if

    end subroutine apply_corrected_flux_y
    !==========================================================================
    subroutine apply_corrected_flux_z(lFaceFrom, lFaceTo)

      integer, intent(in):: lFaceFrom, lFaceTo

      integer:: i, j

      real :: Rho, FullB_D(3)
      !------------------------------------------------------------------------
      do j = 1, nJ; do i = 1, nI
         if(.not.Used_GB(i, j, lFaceTo-1, iBlock)) CYCLE
         if(.not.Used_GB(i, j, lFaceTo  , iBlock)) CYCLE

         Flux_VZI(1:Vdt_,i,j,lFaceTo,1)  = &
              CorrectedFlux_VZB(1:Vdt_,i,j,lFaceFrom,iBlock)

         ! if(UseMhdMomentumFlux)&
         !     MhdFlux_VZ(:,i,j,lFaceTo)  = CorrectedFlux_VZB(&
         !     MhdRhoUx_:MhdRhoUz_,i,j,lFaceFrom,iBlock)
         if(IsCartesianGrid .and. UseB)then
            LeftState_VZ(Bz_,i,j,lFaceTo) = &
                 CorrectedFlux_VZB(BnL_,i,j,lFaceFrom,iBlock)
            RightState_VZ(Bz_,i,j,lFaceTo) = &
                 CorrectedFlux_VZB(BnR_,i,j,lFaceFrom,iBlock)
         end if
      end do; end do
      if(.not.IsCartesianGrid .and. UseB) &
           call apply_bn_face_k(lFaceFrom, lFaceTo, iBlock)

      if(UseTurbulentCascade) then
         ! Since face B changed, recalculate log(Alfven_speed)
         do j = 1, nJ; do i = 1, nI
            if(.not.Used_GB(i, j, lFaceTo-1, iBlock)) CYCLE
            if(.not.Used_GB(i, j, lFaceTo  , iBlock)) CYCLE
            FullB_D = 0.5*(LeftState_VZ(Bx_:Bz_,i,j,lFaceTo) &
                 + RightState_VZ(Bx_:Bz_,i,j,lFaceTo))
            if(UseB0) FullB_D = FullB_D + B0_DZ(:,i,j,lFaceTo)
            Rho = 0.5*(LeftState_VZ(Rho_,i,j,lFaceTo) &
                 +     RightState_VZ(Rho_,i,j,lFaceTo))
            Flux_VZI(LogAlfven_,i,j,lFaceTo,1) = &
                 0.5*log(max(sum(FullB_D**2), 1e-30)/Rho)
         end do; end do
      end if

    end subroutine apply_corrected_flux_z
    !==========================================================================
  end subroutine apply_cons_flux
  !============================================================================
  subroutine apply_bn_face_i(iFaceIn, iFaceOut, iBlock)

    ! subroutines for applying Bn in generalized coordinates

    integer, intent(in):: iFaceIn, iFaceOut, iBlock

    integer:: j, k
    real:: B_D(nDim), FaceArea_D(nDim)
    real:: FaceArea2, DeltaBDotFA
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'apply_bn_face_i'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    do k = 1, nK; do j = 1, nJ
       if(.not.all(Used_GB(iFaceOut-1:iFaceOut,j,k,iBlock))) CYCLE

       FaceArea_D = FaceNormal_DDFB(:,1,iFaceOut,j,k,iBlock)
       FaceArea2  = sum(FaceArea_D**2)

       if(FaceArea2 == 0.0) CYCLE

       B_D = LeftState_VX(Bx_:B_+nDim,iFaceOut,j,k)

       DeltaBDotFA = (CorrectedFlux_VXB(BnL_,j,k,iFaceIn,iBlock) - &
            sum(B_D*FaceArea_D))/FaceArea2

       LeftState_VX(Bx_:B_+nDim,iFaceOut,j,k) = B_D + DeltaBDotFA*FaceArea_D

       B_D = RightState_VX(Bx_:B_+nDim,iFaceOut,j,k)

       DeltaBDotFA = (CorrectedFlux_VXB(BnR_,j,k,iFaceIn,iBlock) - &
            sum(B_D*FaceArea_D))/FaceArea2

       RightState_VX(Bx_:B_+nDim,iFaceOut,j,k) = B_D + DeltaBDotFA*FaceArea_D
    end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine apply_bn_face_i
  !============================================================================
  subroutine apply_bn_face_j(jFaceIn, jFaceOut, iBlock)

    integer, intent(in):: jFaceIn, jFaceOut, iBlock

    integer:: i, k
    real:: B_D(nDim), FaceArea_D(nDim)
    real:: FaceArea2, DeltaBDotFA
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'apply_bn_face_j'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    do k = 1, nK; do i = 1, nI
       if(.not.all(Used_GB(i,jFaceOut-1:jFaceOut,k,iBlock)))CYCLE

       FaceArea_D = FaceNormal_DDFB(:,2,i,jFaceOut,k,iBlock)
       FaceArea2  = sum(FaceArea_D**2)

       if(FaceArea2 == 0.0) CYCLE

       B_D = LeftState_VY(Bx_:B_+nDim,i,jFaceOut,k)

       DeltaBDotFA = (CorrectedFlux_VYB(BnL_,i,k,jFaceIn,iBlock) - &
            sum(B_D*FaceArea_D))/FaceArea2

       LeftState_VY(Bx_:B_+nDim,i,jFaceOut,k) = B_D + DeltaBDotFA*FaceArea_D

       B_D = RightState_VY(Bx_:B_+nDim,i,jFaceOut,k)

       DeltaBDotFA = (CorrectedFlux_VYB(BnR_,i,k,jFaceIn,iBlock) - &
            sum(B_D*FaceArea_D))/FaceArea2

       RightState_VY(Bx_:B_+nDim,i,jFaceOut,k) = B_D + DeltaBDotFA*FaceArea_D

    end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine apply_bn_face_j
  !============================================================================
  subroutine apply_bn_face_k(kFaceIn, kFaceOut, iBlock)

    integer, intent(in):: kFaceIn, kFaceOut, iBlock

    integer:: i, j
    real:: B_D(nDim), FaceArea_D(nDim)
    real:: FaceArea2, DeltaBDotFA
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'apply_bn_face_k'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    do j = 1, nJ; do i = 1, nI
       if(.not.all(Used_GB(i,j,kFaceOut-1:kFaceOut,iBlock)))CYCLE

       FaceArea_D = FaceNormal_DDFB(:,3,i,j,kFaceOut,iBlock)
       FaceArea2  = sum(FaceArea_D**2)

       if(FaceArea2 == 0.0) CYCLE

       B_D = LeftState_VZ(Bx_:B_+nDim,i,j,kFaceOut)

       DeltaBDotFA = ( CorrectedFlux_VZB(BnL_,i,j,kFaceIn,iBlock) - &
            sum(B_D*FaceArea_D))/FaceArea2

       LeftState_VZ(Bx_:B_+nDim,i,j,kFaceOut) = B_D + DeltaBDotFA*FaceArea_D

       B_D = RightState_VZ(Bx_:B_+nDim,i,j,kFaceOut)

       DeltaBDotFA = (CorrectedFlux_VZB(BnR_,i,j,kFaceIn,iBlock) - &
            sum(B_D*FaceArea_D))/FaceArea2

       RightState_VZ(Bx_:B_+nDim,i,j,kFaceOut) = B_D + DeltaBDotFA*FaceArea_D

    end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine apply_bn_face_k
  !============================================================================
end module ModConserveFlux
!==============================================================================
