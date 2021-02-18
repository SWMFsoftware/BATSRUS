!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModConserveFlux

  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest

  use BATL_size, ONLY: nDim
  use ModSize, ONLY: nI, nJ, nK, MaxBlock, MaxDim
  use ModVarIndexes, ONLY: nFluid, nVar, Bx_, By_, Bz_,B_,U_, Ex_,&
       RhoUx_, RhoUz_
  
  use ModMain, ONLY:     UseB, &
       iMinFace, iMaxFace, jMinFace, jMaxFace, kMinFace, kMaxFace, &
       iMinFace2, iMaxFace2, jMinFace2, jMaxFace2, kMinFace2, kMaxFace2
  use ModAdvance, ONLY: &
       Flux_VXI, Flux_VYI, Flux_VZI, &
       VdtFace_XI, VdtFace_YI, VdtFace_ZI, &
       LeftState_VXI, LeftState_VYI, LeftState_VZI, &
       RightState_VXI, RightState_VYI, RightState_VZI, &
       uDotArea_XII, uDotArea_YII, uDotArea_ZII, UseMhdMomentumFlux, &
       MhdFlux_VXI, MhdFlux_VYI, MhdFlux_VZI

  use ModGeometry,  ONLY: true_cell
  use ModParallel, ONLY : &
       neiLtop, neiLbot, neiLeast, neiLwest, neiLnorth, neiLsouth, &
       neiPtop, neiPbot, neiPeast, neiPwest, neiPnorth, neiPsouth, &
       neiBtop, neiBbot, neiBeast, neiBwest, neiBnorth, neiBsouth
  use BATL_lib, ONLY: &
       IsCartesianGrid, IsCartesian, IsRzGeometry, &
       CellFace_DFB, FaceNormal_DDFB, Unused_BP

  implicit none
  SAVE
  private ! except

  public:: &
       init_mod_cons_flux, &
       save_cons_flux, &
       apply_cons_flux, &
       nCorrectedFaceValues, &
       CorrectedFlux_VXB, &
       CorrectedFlux_VYB, &
       CorrectedFlux_VZB, &
       DoConserveFlux

  ! Correct face flux near resolution change.
  logical :: DoConserveFlux = .true.

  integer, parameter :: &
       FluxLast_ = nVar + nFluid, &
       UnFirst_ = FluxLast_+1, UnLast_ = UnFirst_ + nFluid, &
       Vdt_ = UnLast_ + 1
  ! The normal components of the magnetic field is exchaned only for
  ! B_>U_ (UseB_ is true)
  integer,parameter :: BnL_ = Vdt_ + min(1, B_-U_)
  integer,parameter :: BnR_ = BnL_ + min(1, B_-U_)

  ! For momentum conserving scheme (for hybrid or multi-fluid) Mhd flux of
  ! momentum should be saved, the condition is UseB_ (B_-U_>0) and not
  ! UseEField (Ex_>1)
  integer,parameter :: MhdRhoUx_ = BnR_ +        min(max(2-Ex_,0), B_-U_)
  integer,parameter :: MhdRhoUz_ = BnR_ + MaxDim*min(max(2-Ex_,0), B_-U_)
  integer,parameter :: nCorrectedFaceValues = MhdRhoUz_

  ! Face conservative or corrected flux.
  real, allocatable, dimension(:,:,:,:,:) :: &
       CorrectedFlux_VXB, CorrectedFlux_VYB, CorrectedFlux_VZB

  real, parameter :: FaceRatio = 1.0/2**(nDim-1)

contains
  !============================================================================

  subroutine init_mod_cons_flux
    
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_mod_cons_flux'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(.not.allocated(CorrectedFlux_VXB)) allocate( &
         CorrectedFlux_VXB(nCorrectedFaceValues,nJ,nK,2,MaxBlock), &
         CorrectedFlux_VYB(nCorrectedFaceValues,nI,nK,2,MaxBlock), &
         CorrectedFlux_VZB(nCorrectedFaceValues,nI,nJ,2,MaxBlock)  )

    CorrectedFlux_VXB = 0.0
    CorrectedFlux_VYB = 0.0
    CorrectedFlux_VZB = 0.0

    call test_stop(NameSub, DoTest)
  end subroutine init_mod_cons_flux
  !============================================================================

  subroutine save_cons_flux(iBlock)

    integer, intent(in) :: iBlock

    integer :: lFaceFrom,lFaceTo,i,j,k
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'save_cons_flux'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if (neiLeast(iBlock)==+1) then
       lFaceFrom=1
       lFaceTo=1
       call save_corrected_flux_x
    end if

    if (neiLwest(iBlock)==+1) then
       lFaceFrom=1+nI
       lFaceTo=2
       call save_corrected_flux_x
    end if

    if (nDim > 1 .and. neiLsouth(iBlock)==+1) then
       lFaceFrom=1
       lFaceTo=1
       call save_corrected_flux_y
    end if

    if (nDim > 1 .and. neiLnorth(iBlock)==+1) then
       lFaceFrom=1+nJ
       lFaceTo=2
       call save_corrected_flux_y
    end if

    if (nDim > 2 .and. neiLbot(iBlock)==+1) then
       lFaceFrom=1
       lFaceTo=1
       call save_corrected_flux_z
    end if

    if (nDim > 2 .and. neiLtop(iBlock)==+1) then
       lFaceFrom=nK+1
       lFaceTo=2
       call save_corrected_flux_z
    end if

    call test_stop(NameSub, DoTest, iBlock)
  contains
    !==========================================================================

    subroutine save_corrected_flux_x
      real, pointer:: uDotArea_XI(:,:,:,:)
      real, pointer:: Flux_VX(:,:,:,:)
      real, pointer:: LeftState_VX(:,:,:,:)
      real, pointer:: RightState_VX(:,:,:,:)
      real, pointer:: VdtFace_X(:,:,:)
      real, pointer:: MhdFlux_VX(:,:,:,:)

      !------------------------------------------------------------------------
      MhdFlux_VX(RhoUx_:RhoUz_,1:nI+1,jMinFace:jMaxFace,kMinFace:kMaxFace) => &
           MhdFlux_VXI(:,:,:,:,1) 
      VdtFace_X(1:nI+1,jMinFace:jMaxFace,kMinFace:kMaxFace) => &
           VdtFace_XI(:,:,:,1) 
      RightState_VX(1:nVar,1:nI+1,jMinFace2:jMaxFace2,kMinFace2:kMaxFace2) => &
           RightState_VXI(:,:,:,:,1) 
      LeftState_VX(1:nVar,1:nI+1,jMinFace2:jMaxFace2,kMinFace2:kMaxFace2) => &
           LeftState_VXI(:,:,:,:,1) 
      Flux_VX(1:nVar+nFluid,1:nI+1,jMinFace:jMaxFace,kMinFace:kMaxFace) => &
           Flux_VXI(:,:,:,:,1) 
      uDotArea_XI(1:nI+1,jMinFace:jMaxFace,kMinFace:kMaxFace,1:nFluid+1) => &
           uDotArea_XII(:,:,:,:,1)
      
      do k=1,nK; do j=1,nJ
         CorrectedFlux_VXB(1:FluxLast_,j,k,lFaceTo,iBlock)  &
              = Flux_VX(1:nVar+nFluid, lFaceFrom,j,k)

         CorrectedFlux_VXB(UnFirst_:UnLast_,j,k,lFaceTo,iBlock) &
              = uDotArea_XI(lFaceFrom,j,k,:)

         CorrectedFlux_VXB(Vdt_,j,k,lFaceTo,iBlock)          &
              = VdtFace_X(lFaceFrom,j,k)
      end do; end do

      if(.not.UseB)RETURN

      if(IsCartesian)then
         do k=1,nK; do j=1,nJ
            CorrectedFlux_VXB(BnL_,j,k,lFaceTo,iBlock)= &
                 LeftState_VX(Bx_,lFaceFrom,j,k)*FaceRatio
            CorrectedFlux_VXB(BnR_,j,k,lFaceTo,iBlock)= &
                 RightState_VX(Bx_,lFaceFrom,j,k)*FaceRatio
         end do; end do
      elseif(IsRzGeometry)then
         ! X face areas vary in RZ geometry
         do k=1,nK; do j=1,nJ
            CorrectedFlux_VXB(BnL_,j,k,lFaceTo,iBlock) = &
                 LeftState_VX(Bx_, lFaceFrom,j,k) * &
                 CellFace_DFB(1,lFaceFrom,j,k,iBlock)
            CorrectedFlux_VXB(BnR_,j,k,lFaceTo,iBlock) = &
                 RightState_VX(Bx_,lFaceFrom,j,k) * &
                 CellFace_DFB(1,lFaceFrom,j,k,iBlock)
         end do; end do
      else
         ! Dot product with face normal
         do k=1,nK; do j=1,nJ
            CorrectedFlux_VXB(BnL_,j,k,lFaceTo,iBlock) = &
                 dot_product(LeftState_VX(Bx_:B_+nDim,lFaceFrom,j,k), &
                 FaceNormal_DDFB(:,1,lFaceFrom, j, k, iBlock))
            CorrectedFlux_VXB(BnR_, j, k, lFaceTo, iBlock) = &
                 dot_product(RightState_VX(Bx_:B_+nDim,lFaceFrom,j,k), &
                 FaceNormal_DDFB(:,1,lFaceFrom,j,k,iBlock))
         end do; end do
      end if
      if(UseMhdMomentumFlux)then
         do k=1,nK; do j=1,nJ
            CorrectedFlux_VXB(MhdRhoUx_:MhdRhoUz_,j,k,lFaceTo,iBlock) = &
                 MhdFlux_VX(:,lFaceFrom,j,k)
         end do; end do
      end if

      if(DoTest)then
         write(*,*)NameSub,' lFaceFrom, lFaceTo=',lFaceFrom, lFaceTo
         do i = 1, nFluid+1
            write(*,*)NameSub,' iVar, uDotA=', &
                 uDotArea_XI(lFaceFrom,jTest,kTest,i)
         end do
         do i = 1, nCorrectedFaceValues
            write(*,*)NameSub,' iVar, flux=', i, &
                 CorrectedFlux_VXB(i, iTest, jTest, kTest, iBlock)
         end do
      end if

    end subroutine save_corrected_flux_x
    !==========================================================================

    subroutine save_corrected_flux_y
      real, pointer:: uDotArea_YI(:,:,:,:)
      real, pointer:: Flux_VY(:,:,:,:)
      real, pointer:: LeftState_VY(:,:,:,:)
      real, pointer:: RightState_VY(:,:,:,:)
      real, pointer:: VdtFace_Y(:,:,:)
      real, pointer:: MhdFlux_VY(:,:,:,:)

      !------------------------------------------------------------------------
      MhdFlux_VY(RhoUx_:RhoUz_,iMinFace:iMaxFace,1:nJ+1,kMinFace:kMaxFace) => &
           MhdFlux_VYI(:,:,:,:,1) 
      VdtFace_Y(iMinFace:iMaxFace,1:nJ+1,kMinFace:kMaxFace) => &
           VdtFace_YI(:,:,:,1) 
      RightState_VY(1:nVar,iMinFace2:iMaxFace2,1:nJ+1,kMinFace2:kMaxFace2) => &
           RightState_VYI(:,:,:,:,1) 
      LeftState_VY(1:nVar,iMinFace2:iMaxFace2,1:nJ+1,kMinFace2:kMaxFace2) => &
           LeftState_VYI(:,:,:,:,1) 
      Flux_VY(1:nVar+nFluid,iMinFace:iMaxFace,1:nJ+1,kMinFace:kMaxFace) => &
           Flux_VYI(:,:,:,:,1) 
      uDotArea_YI(iMinFace:iMaxFace,1:nJ+1,kMinFace:kMaxFace,1:nFluid+1) => &
           uDotArea_YII(:,:,:,:,1)
      
      do k=1,nK;do i=1,nI
         CorrectedFlux_VYB(1:FluxLast_,i,k,lFaceTo,iBlock)    &
              = Flux_VY(1:FluxLast_,i,lFaceFrom,k)

         CorrectedFlux_VYB(UnFirst_:UnLast_,i,k,lFaceTo,iBlock) &
              = uDotArea_YI(i,lFaceFrom,k,:)

         CorrectedFlux_VYB(Vdt_,i,k,lFaceTo,iBlock)         &
              = VdtFace_Y(i,lFaceFrom,k)
      end do; end do

      if(.not.UseB)RETURN

      if(IsCartesianGrid)then
         do k=1,nK; do i=1,nI
            CorrectedFlux_VYB(BnL_,i,k,lFaceTo,iBlock)= &
                 LeftState_VY( By_,i,lFaceFrom,k)*FaceRatio
            CorrectedFlux_VYB(BnR_,i,k,lFaceTo,iBlock)= &
                 RightState_VY(By_,i,lFaceFrom,k)*FaceRatio
         end do; end do
      else
         do k=1,nK; do i=1,nI
            CorrectedFlux_VYB(BnL_,i,k,lFaceTo,iBlock) = &
                 dot_product(LeftState_VY(Bx_:B_+nDim,i,lFaceFrom,k),&
                 FaceNormal_DDFB(:,2,i,lFaceFrom,k,iBlock))
            CorrectedFlux_VYB(BnR_,i,k,lFaceTo,iBlock) = &
                 dot_product(RightState_VY(Bx_:B_+nDim,i,lFaceFrom,k),&
                 FaceNormal_DDFB(:,2,i,lFaceFrom,k,iBlock))
         end do; end do
      end if
      if(UseMhdMomentumFlux)then
         do k=1,nK; do i=1,nI
            CorrectedFlux_VYB(MhdRhoUx_:MhdRhoUz_,i,k,lFaceTo,iBlock) = &
                 MhdFlux_VY(:,i,lFaceFrom,k)
         end do; end do
      end if
    end subroutine save_corrected_flux_y
    !==========================================================================

    subroutine save_corrected_flux_z
      real, pointer:: uDotArea_ZI(:,:,:,:)
      real, pointer:: Flux_VZ(:,:,:,:)
      real, pointer:: LeftState_VZ(:,:,:,:)
      real, pointer:: RightState_VZ(:,:,:,:)
      real, pointer:: VdtFace_Z(:,:,:)
      real, pointer:: MhdFlux_VZ(:,:,:,:)

      !------------------------------------------------------------------------
      MhdFlux_VZ(RhoUx_:RhoUz_,iMinFace:iMaxFace,jMinFace:jMaxFace,1:nK+1) => &
           MhdFlux_VZI(:,:,:,:,1) 
      VdtFace_Z(iMinFace:iMaxFace,jMinFace:jMaxFace,1:nK+1) => &
           VdtFace_ZI(:,:,:,1) 
      RightState_VZ(1:nVar,iMinFace2:iMaxFace2,jMinFace2:jMaxFace2,1:nK+1) => &
           RightState_VZI(:,:,:,:,1) 
      LeftState_VZ(1:nVar,iMinFace2:iMaxFace2,jMinFace2:jMaxFace2,1:nK+1) => &
           LeftState_VZI(:,:,:,:,1) 
      Flux_VZ(1:nVar+nFluid,iMinFace:iMaxFace,jMinFace:jMaxFace,1:nK+1) => &
           Flux_VZI(:,:,:,:,1) 
      uDotArea_ZI(iMinFace:iMaxFace,jMinFace:jMaxFace,1:nK+1,1:nFluid+1) => &
           uDotArea_ZII(:,:,:,:,1)
      
      do j=1,nJ;do i=1,nI
         CorrectedFlux_VZB(1:FluxLast_,  i,j,lFaceTo,iBlock)    &
              = Flux_VZ(1:FluxLast_,i,j,lFaceFrom)

         CorrectedFlux_VZB(UnFirst_:UnLast_,i,j,lFaceTo,iBlock) &
              = uDotArea_ZI(i,j,lFaceFrom,:)

         CorrectedFlux_VZB(Vdt_,  i,j,lFaceTo,iBlock)           &
              = VdtFace_Z(i,j,lFaceFrom)
      end do; end do

      if(.not.UseB)RETURN

      if(IsCartesianGrid)then
         do j=1,nJ; do i=1,nI
            CorrectedFlux_VZB(BnL_,i,j,lFaceTo,iBlock)= &
                 LeftState_VZ( Bz_,i,j,lFaceFrom)*FaceRatio
            CorrectedFlux_VZB(BnR_,i,j,lFaceTo,iBlock)= &
                 RightState_VZ(Bz_,i,j,lFaceFrom)*FaceRatio
         end do; end do
      else
         do j=1,nJ; do i=1,nI
            CorrectedFlux_VZB(BnL_,i,j,lFaceTo,iBlock) = &
                 dot_product(LeftState_VZ(Bx_:B_+nDim,i,j,lFaceFrom),&
                 FaceNormal_DDFB(:,3,i,j,lFaceFrom,iBlock))
            CorrectedFlux_VZB(BnR_,i,j,lFaceTo, iBlock) =&
                 dot_product(RightState_VZ(Bx_:B_+nDim,i,j,lFaceFrom),&
                 FaceNormal_DDFB(:,3,i,j,lFaceFrom,iBlock))
         end do; end do
      end if
      if(UseMhdMomentumFlux)then
         do j=1,nJ; do i=1,nI
            CorrectedFlux_VZB(MhdRhoUx_:MhdRhoUz_,i,j,lFaceTo,iBlock) = &
                 MhdFlux_VZ(:,i,j,lFaceFrom)
         end do; end do
      end if
    end subroutine save_corrected_flux_z
    !==========================================================================

  end subroutine save_cons_flux
  !============================================================================

  subroutine apply_cons_flux(iBlock)

    integer, intent(in):: iBlock

    integer :: i, j, k
    integer :: lFaceFrom, lFaceTo

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'apply_cons_flux'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if (neiLeast(iBlock)==-1)then
       if ( .not.Unused_BP(neiBeast(1,iBlock),neiPeast(1,iBlock)).and.&
            .not.Unused_BP(neiBeast(2,iBlock),neiPeast(2,iBlock)).and.&
            .not.Unused_BP(neiBeast(3,iBlock),neiPeast(3,iBlock)).and.&
            .not.Unused_BP(neiBeast(4,iBlock),neiPeast(4,iBlock)))then
          lFaceTo=1
          lFaceFrom=1
          call apply_corrected_flux_x
       end if
    end if

    if (neiLwest(iBlock)==-1) then
       if ( .not.Unused_BP(neiBwest(1,iBlock),neiPwest(1,iBlock)).and.&
            .not.Unused_BP(neiBwest(2,iBlock),neiPwest(2,iBlock)).and.&
            .not.Unused_BP(neiBwest(3,iBlock),neiPwest(3,iBlock)).and.&
            .not.Unused_BP(neiBwest(4,iBlock),neiPwest(4,iBlock)))then
          lFaceTo=nI+1
          lFaceFrom=2
          call apply_corrected_flux_x
       end if
    end if

    if (nDim > 1 .and. neiLsouth(iBlock)==-1) then
       if(.not.Unused_BP(neiBsouth(1,iBlock),neiPsouth(1,iBlock)).and.&
            .not.Unused_BP(neiBsouth(2,iBlock),neiPsouth(2,iBlock)).and.&
            .not.Unused_BP(neiBsouth(3,iBlock),neiPsouth(3,iBlock)).and.&
            .not.Unused_BP(neiBsouth(4,iBlock),neiPsouth(4,iBlock)))then
          lFaceTo=1
          lFaceFrom=1
          call apply_corrected_flux_y
       end if
    end if

    if (nDim > 1 .and. neiLnorth(iBlock)==-1) then
       if ( .not.Unused_BP(neiBnorth(1,iBlock),neiPnorth(1,iBlock)).and.&
            .not.Unused_BP(neiBnorth(2,iBlock),neiPnorth(2,iBlock)).and.&
            .not.Unused_BP(neiBnorth(3,iBlock),neiPnorth(3,iBlock)).and.&
            .not.Unused_BP(neiBnorth(4,iBlock),neiPnorth(4,iBlock)))then
          lFaceTo=nJ+1
          lFaceFrom=2
          call apply_corrected_flux_y
       end if
    end if

    if (nDim > 2 .and. neiLbot(iBlock)==-1) then
       if ( .not.Unused_BP(neiBbot(1,iBlock),neiPbot(1,iBlock)).and. &
            .not.Unused_BP(neiBbot(2,iBlock),neiPbot(2,iBlock)).and. &
            .not.Unused_BP(neiBbot(3,iBlock),neiPbot(3,iBlock)).and. &
            .not.Unused_BP(neiBbot(4,iBlock),neiPbot(4,iBlock)))then
          lFaceTo=1
          lFaceFrom=1
          call apply_corrected_flux_z
       end if
    end if

    if (nDim > 2 .and. neiLtop(iBlock)==-1) then
       if ( .not.Unused_BP(neiBtop(1,iBlock),neiPtop(1,iBlock)).and. &
            .not.Unused_BP(neiBtop(2,iBlock),neiPtop(2,iBlock)).and. &
            .not.Unused_BP(neiBtop(3,iBlock),neiPtop(3,iBlock)).and. &
            .not.Unused_BP(neiBtop(4,iBlock),neiPtop(4,iBlock))) then
          lFaceTo=nK+1
          lFaceFrom=2
          call apply_corrected_flux_z
       end if
    end if

    call test_stop(NameSub, DoTest, iBlock)
  contains
    !==========================================================================

    subroutine apply_corrected_flux_x
      real, pointer:: uDotArea_XI(:,:,:,:)
      real, pointer:: Flux_VX(:,:,:,:)
      real, pointer:: LeftState_VX(:,:,:,:)
      real, pointer:: RightState_VX(:,:,:,:)
      real, pointer:: VdtFace_X(:,:,:)

      !------------------------------------------------------------------------
      VdtFace_X(1:nI+1,jMinFace:jMaxFace,kMinFace:kMaxFace) => &
           VdtFace_XI(:,:,:,1) 
      RightState_VX(1:nVar,1:nI+1,jMinFace2:jMaxFace2,kMinFace2:kMaxFace2) => &
           RightState_VXI(:,:,:,:,1) 
      LeftState_VX(1:nVar,1:nI+1,jMinFace2:jMaxFace2,kMinFace2:kMaxFace2) => &
           LeftState_VXI(:,:,:,:,1) 
      Flux_VX(1:nVar+nFluid,1:nI+1,jMinFace:jMaxFace,kMinFace:kMaxFace) => &
           Flux_VXI(:,:,:,:,1) 
      uDotArea_XI(1:nI+1,jMinFace:jMaxFace,kMinFace:kMaxFace,1:nFluid+1) => &
           uDotArea_XII(:,:,:,:,1)

      do k = 1, nK; do j = 1, nJ
         if (.not.true_cell(lFaceTo-1, j, k, iBlock)) CYCLE
         if (.not.true_cell(lFaceTo  , j, k, iBlock)) CYCLE

         Flux_VX(1:FluxLast_,lFaceTo,j,k) = &
              CorrectedFlux_VXB(1:FluxLast_,j,k,lFaceFrom,iBlock)
         uDotArea_XI(lFaceTo,j,k,:) = &
              CorrectedFlux_VXB(UnFirst_:UnLast_,j,k,lFaceFrom,iBlock)
         VdtFace_X(lFaceTo,j,k) = &
              CorrectedFlux_VXB(Vdt_,j,k,lFaceFrom,iBlock)
         ! if(UseMhdMomentumFlux) &
         !     MhdFlux_VX(:,lFaceTo,j,k) = CorrectedFlux_VXB(&
         !     MhdRhoUx_:MhdRhoUz_,j,k,lFaceFrom,iBlock)
         if(.not.(UseB .and. IsCartesianGrid))CYCLE

         if(IsCartesian)then
            LeftState_VX(Bx_,lFaceTo,j,k) = &
                 CorrectedFlux_VXB(BnL_,j,k,lFaceFrom,iBlock)
            RightState_VX(Bx_,lFaceTo,j,k) = &
                 CorrectedFlux_VXB(BnR_,j,k,lFaceFrom,iBlock)
         else
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

    end subroutine apply_corrected_flux_x
    !==========================================================================

    subroutine apply_corrected_flux_y
      real, pointer:: uDotArea_YI(:,:,:,:)
      real, pointer:: Flux_VY(:,:,:,:)
      real, pointer:: LeftState_VY(:,:,:,:)
      real, pointer:: RightState_VY(:,:,:,:)
      real, pointer:: VdtFace_Y(:,:,:)
      !------------------------------------------------------------------------
      VdtFace_Y(iMinFace:iMaxFace,1:nJ+1,kMinFace:kMaxFace) => &
           VdtFace_YI(:,:,:,1) 
      RightState_VY(1:nVar,iMinFace2:iMaxFace2,1:nJ+1,kMinFace2:kMaxFace2) => &
           RightState_VYI(:,:,:,:,1) 
      LeftState_VY(1:nVar,iMinFace2:iMaxFace2,1:nJ+1,kMinFace2:kMaxFace2) => &
           LeftState_VYI(:,:,:,:,1) 
      Flux_VY(1:nVar+nFluid,iMinFace:iMaxFace,1:nJ+1,kMinFace:kMaxFace) => &
           Flux_VYI(:,:,:,:,1) 
      uDotArea_YI(iMinFace:iMaxFace,1:nJ+1,kMinFace:kMaxFace,1:nFluid+1) => &
           uDotArea_YII(:,:,:,:,1)

      do k = 1, nK; do i = 1, nI
         if (.not.true_cell(i, lFaceTo-1, k, iBlock))CYCLE
         if (.not.true_cell(i, lFaceTo  , k, iBlock))CYCLE

         Flux_VY(1:FluxLast_,i,lFaceTo,k) = &
              CorrectedFlux_VYB(1:FluxLast_,i,k,lFaceFrom,iBlock)
         uDotArea_YI(i,lFaceTo,k,:) = &
              CorrectedFlux_VYB(UnFirst_:UnLast_,i,k,lFaceFrom,iBlock)
         VdtFace_Y(i,lFaceTo,k)= &
              CorrectedFlux_VYB(Vdt_,i,k,lFaceFrom,iBlock)
         ! if(UseMhdMomentumFlux)&
         !     MhdFlux_VY(:,i,lFaceTo,k) = CorrectedFlux_VYB(&
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

    end subroutine apply_corrected_flux_y
    !==========================================================================

    subroutine apply_corrected_flux_z
      real, pointer:: uDotArea_ZI(:,:,:,:)
      real, pointer:: Flux_VZ(:,:,:,:)
      real, pointer:: LeftState_VZ(:,:,:,:)
      real, pointer:: RightState_VZ(:,:,:,:)
      real, pointer:: VdtFace_Z(:,:,:)

      !------------------------------------------------------------------------
      VdtFace_Z(iMinFace:iMaxFace,jMinFace:jMaxFace,1:nK+1) => &
           VdtFace_ZI(:,:,:,1) 
      RightState_VZ(1:nVar,iMinFace2:iMaxFace2,jMinFace2:jMaxFace2,1:nK+1) => &
           RightState_VZI(:,:,:,:,1) 
      LeftState_VZ(1:nVar,iMinFace2:iMaxFace2,jMinFace2:jMaxFace2,1:nK+1) => &
           LeftState_VZI(:,:,:,:,1) 
      Flux_VZ(1:nVar+nFluid,iMinFace:iMaxFace,jMinFace:jMaxFace,1:nK+1) => &
           Flux_VZI(:,:,:,:,1) 
      uDotArea_ZI(iMinFace:iMaxFace,jMinFace:jMaxFace,1:nK+1,1:nFluid+1) => &
           uDotArea_ZII(:,:,:,:,1)

      do j = 1, nJ; do i = 1, nI
         if(.not.true_cell(i, j, lFaceTo-1, iBlock)) CYCLE
         if(.not.true_cell(i, j, lFaceTo  , iBlock)) CYCLE

         Flux_VZ(1:FluxLast_,i,j,lFaceTo) = &
              CorrectedFlux_VZB(1:FluxLast_,i,j,lFaceFrom,iBlock)
         uDotArea_ZI(i,j,lFaceTo,:) = &
              CorrectedFlux_VZB(UnFirst_:UnLast_,i,j,lFaceFrom,iBlock)
         VdtFace_Z(i,j,lFaceTo) = &
              CorrectedFlux_VZB(Vdt_,i,j,lFaceFrom,iBlock)
         ! if(UseMhdMomentumFlux)&
         !     MhdFlux_VZ(:,i,j,lFaceTo) = CorrectedFlux_VZB(&
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

    end subroutine apply_corrected_flux_z
    !==========================================================================

  end subroutine apply_cons_flux
  !============================================================================

  ! subroutines for applying Bn in generalized coordinates

  subroutine apply_bn_face_i(iFaceIn, iFaceOut, iBlock)

    integer, intent(in):: iFaceIn, iFaceOut, iBlock

    integer:: j, k
    real:: B_D(nDim), FaceArea_D(nDim)
    real:: FaceArea2, DeltaBDotFA
    logical:: DoTest    
    real, pointer:: LeftState_VX(:,:,:,:)
    real, pointer:: RightState_VX(:,:,:,:)
    character(len=*), parameter:: NameSub = 'apply_bn_face_i'
    !--------------------------------------------------------------------------
    RightState_VX(1:nVar,1:nI+1,jMinFace2:jMaxFace2,kMinFace2:kMaxFace2) => &
         RightState_VXI(:,:,:,:,1) 
    LeftState_VX(1:nVar,1:nI+1,jMinFace2:jMaxFace2,kMinFace2:kMaxFace2) => &
         LeftState_VXI(:,:,:,:,1)

    call test_start(NameSub, DoTest, iBlock)
    do k=1,nK; do j=1,nJ
       if(.not.all(true_cell(iFaceOut-1:iFaceOut,j,k,iBlock)))CYCLE

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
    real, pointer:: LeftState_VY(:,:,:,:)
    real, pointer:: RightState_VY(:,:,:,:)
    character(len=*), parameter:: NameSub = 'apply_bn_face_j'
    !--------------------------------------------------------------------------
    RightState_VY(1:nVar,iMinFace2:iMaxFace2,1:nJ+1,kMinFace2:kMaxFace2) => &
         RightState_VYI(:,:,:,:,1) 
    LeftState_VY(1:nVar,iMinFace2:iMaxFace2,1:nJ+1,kMinFace2:kMaxFace2) => &
         LeftState_VYI(:,:,:,:,1)

    call test_start(NameSub, DoTest, iBlock)

    do k=1,nK; do i=1,nI
       if(.not.all(true_cell(i,jFaceOut-1:jFaceOut,k,iBlock)))CYCLE

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

    integer:: i,j
    real:: B_D(nDim), FaceArea_D(nDim)
    real:: FaceArea2, DeltaBDotFA
    logical:: DoTest
    real, pointer:: LeftState_VZ(:,:,:,:)
    real, pointer:: RightState_VZ(:,:,:,:)
    character(len=*), parameter:: NameSub = 'apply_bn_face_k'
    !--------------------------------------------------------------------------
    RightState_VZ(1:nVar,iMinFace2:iMaxFace2,jMinFace2:jMaxFace2,1:nK+1) => &
         RightState_VZI(:,:,:,:,1) 
    LeftState_VZ(1:nVar,iMinFace2:iMaxFace2,jMinFace2:jMaxFace2,1:nK+1) => &
         LeftState_VZI(:,:,:,:,1)

    call test_start(NameSub, DoTest, iBlock)
    do j=1,nJ; do i=1,nI
       if(.not.all(true_cell(i,j,kFaceOut-1:kFaceOut,iBlock)))CYCLE

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
