!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModCleanDivB

  use BATL_lib, ONLY: &
       test_start, test_stop

  implicit none

  private ! except

  public:: clean_divb

  ! Local variables

  real:: OneDotMDotOneInv
  real, allocatable:: Prec_CB(:,:,:,:)
  real:: BoundaryCoef = 1.0
  integer:: nCleanDivb = 0

contains
  !============================================================================

  subroutine clean_divb

    use ModNumConst, ONLY: cTiny
    use ModMain, ONLY: iNewGrid, iNewDecomposition
    use ModAdvance, ONLY: nVar,State_VGB, Bx_, By_, Bz_,Tmp1_GB,Tmp2_GB,&
         Residual_GB=>Tmp1_GB,Dir_GB=>Tmp2_GB
    use ModAdvance, ONLY:tmp3_blk=>divB1_GB
    use ModGeometry, ONLY: IsNoBody_B, IsBody_B
    use ModParallel, ONLY : Unset_, DiLevel_EB
    use ModMpi
    use BATL_lib, ONLY: nI, nJ, nK, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, &
         nBlock, MaxBlock, x_, y_, z_, k0_, j0_, nJp1_, nKp1_, &
         Unused_B, CellFace_DB, CellVolume_GB, message_pass_cell, &
         iProc, nProc, iComm, Used_GB

    ! Loop variables
    integer::i,j,k,iBlock

    real:: DivBV_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)
    real:: DivBInt_I(2), DivBTemp_I(2)
    integer, parameter:: ResDotM1DotRes_=2, ResDotOne_=1

    ! Conjugated gradients algorithm, see
    ! http://www.netlib.org/linalg/html_templates/node20.html
    ! Notations
    ! M^(-1)=diag(Prec_CB) - the Jacobi preconditioner
    ! Residual_GB=M^{-1}r^(i)/rho_(i-1) (temporal)
    ! Dir_GB=p^(i)/rho_(i-1)

    real::DirDotDir,DirDotDirInv
    ! DirDotDirInv=1/(Dir.A.Dir)

    real::    Tolerance=cTiny
    integer:: iError
    integer:: nIteration

    real:: VDotGrad_DC(3,nI,nJ,nK)
    ! For convenience, what is stored in these arrays are the gradients
    ! multiplied by the cell volume
    ! While calculating the magnetic field, we use vInv*vDotGrad Phi
    ! While calculating the sum V_i grad^2 Phi, we use vInv vDotGrad^2 Phi

    integer :: iLastGrid=-100, iLastDecomposition=-100

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'clean_divb'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(.not.allocated(Prec_CB)) allocate(Prec_CB(nI,nJ,nK,MaxBlock))

    if(iLastGrid /= iNewGrid .or. iLastDecomposition /= iNewDecomposition)then
       call init_divb_cleanup
       iLastGrid          = iNewGrid
       iLastDecomposition = iNewDecomposition
    endif
    call timing_start(NameSub)

    nIteration=1

    do
       call message_pass_cell(nVar, State_VGB, nWidthIn=1, &
            DoSendCornerIn=.false., nProlongOrderIn=1, DoRestrictFaceIn=.true.)

       ! Get the ghostcell values for MF
       DivBInt_I = 0.0;DivBTemp_I = 0.0
       do iBlock=1,nBlock
          Residual_GB(:,:,:,iBlock) = 0.0
          if(nIteration==1) Dir_GB(:,:,:,iBlock) = 0.0
          ! Initialize the vector "Dir"

          if(Unused_B(iBlock))CYCLE

          call div3d_b1(iBlock,&
               State_VGB(Bx_,:,:,:,iBlock), &
               State_VGB(By_,:,:,:,iBlock), &
               State_VGB(Bz_,:,:,:,iBlock), &
               DivBV_G)

          ! Multiply residual by M^{-1}
          Residual_GB(1:nI,1:nJ,1:nK,iBlock) = &
               DivBV_G(1:nI,1:nJ,1:nK)*Prec_CB(:,:,:,iBlock)

          ! DivBAbsMax=max(DivBAbsMax,vInv_CB(iBlock)*abs(&
          !                DivBV_G(1:nI,1:nJ,1:nK))))

          ! Integrate divB by the whole volume
          DivBInt_I(ResDotOne_)=DivBInt_I(ResDotOne_)&
               +sum(DivBV_G(1:nI,1:nJ,1:nK))

          ! Calculate Resudual.M.Residual
          DivBInt_I(ResDotM1DotRes_)=DivBInt_I(ResDotM1DotRes_)&
               +sum(DivBV_G(1:nI,1:nJ,1:nK)*Residual_GB(1:nI,1:nJ,1:nK,iBlock))
       end do

       if(nProc>1) call MPI_allreduce(MPI_IN_PLACE, DivBInt_I, &
            2,  MPI_REAL, MPI_SUM, iComm, iError)

       DivBTemp_I = DivBInt_I

       ! Now evaluate the norm only for that part of the residual,
       ! which is orthogobal to 1:
       ! (Res.M^{-1}.Res)_{mod} + (Res.M^{-1}.(1/sqrt(1.M^{-1}.1)))^2
       ! = Res.M^{-1}.Res

       DivBInt_I(ResDotM1DotRes_) = DivBTemp_I(ResDotM1DotRes_)&
            -OneDotMDotOneInv*DivBTemp_I(ResDotOne_)**2

       if(iProc==0.and.DoTest)&
            write(*,*) NameSub,': iteration=',nIteration,&
            'SigmaRes,SigmaRes^2,integral error:',&
            DivBTemp_I,DivBInt_I(ResDotM1DotRes_)

       if(DivBInt_I(ResDotM1DotRes_) < Tolerance)EXIT

       ! This is what we should add to the Residual to make it ortogonal
       ! to the constant solution
       DivBInt_I(ResDotOne_) = -OneDotMDotOneInv*DivBTemp_I(ResDotOne_)

       ! Below we divide per Res.M.Res, so now we get inverse of it
       DivBInt_I(ResDotM1DotRes_) = 1.0/DivBInt_I(ResDotM1DotRes_)
       do iBlock=1,nBlock
          if(Unused_B(iBlock))CYCLE
          if(IsNoBody_B(iBlock))then
             Dir_GB(1:nI,1:nJ,1:nK,iBlock)=Dir_GB(1:nI,1:nJ,1:nK,iBlock)+&
                  DivBInt_I(ResDotM1DotRes_) &
                  *(Residual_GB(1:nI,1:nJ,1:nK,iBlock) + DivBInt_I(ResDotOne_))
          else
             do k=1,nK;do j=1,nJ;do i=1,nI
                if(.not.Used_GB(i,j,k,iBlock))CYCLE
                Tmp2_GB(i,j,k,iBlock)=Tmp2_GB(i,j,k,iBlock)+&
                     DivBInt_I(ResDotM1DotRes_)*(Tmp1_GB(i,j,k,iBlock)+&
                     DivBInt_I(ResDotOne_))
             end do;end do;end do
          end if
       end do

       call message_pass_cell(Tmp2_GB, nWidthIn=1, DoSendCornerIn=.false. ,&
            nProlongOrderIn=1, DoRestrictFaceIn=.true.)

       ! Calculate Dir.A.Dir
       DirDotDir = 0.0
       do iBlock=1,nBlock
          if(Unused_B(iBlock))CYCLE
          call v_grad_phi(Tmp2_GB,iBlock)
          DirDotDir = DirDotDir + &
               sum( (VDotGrad_DC(1,:,:,:)**2 + &
               VDotGrad_DC(2,:,:,:)**2 + &
               VDotGrad_DC(3,:,:,:)**2 ) &
               /CellVolume_GB(1:nI,1:nJ,1:nK,iBlock) )
       end do
       if(nProc>1) call MPI_allreduce(MPI_IN_PLACE, DirDotDir, &
            1,  MPI_REAL, MPI_SUM, iComm, iError)

       DirDotDirInv = 1.0/DirDotDir

       if(DoTest .and. iProc==0) &
            write(*,*) NameSub,': effective diffusion coefficient = ',&
            DirDotDirInv*DivBInt_I(ResDotM1DotRes_)

       ! If iterations are not used, the diffusion coefficient
       ! should be less than 1 to ensure the convergence in the r.M^{-1}.r norm
       if(nCleanDivb==0) &
            DirDotDirInv = min(DirDotDirInv, 0.99/DivBInt_I(ResDotM1DotRes_))

       do iBlock=1,nBlock
          if (Unused_B(iBlock)) CYCLE
          call v_grad_phi(Dir_GB,iBlock)
          do k=1,nK; do j=1,nJ; do i=1,nI
             State_VGB(Bx_:Bz_,i,j,k,iBlock) =    &
                  State_VGB(Bx_:Bz_,i,j,k,iBlock)-&
                  VDotGrad_DC(x_:z_,i,j,k)*DirDotDirInv/   &
                  CellVolume_GB(i,j,k,iBlock)
          end do; end do; end do
       end do
       nIteration = nIteration + 1
       if(nIteration > nCleanDivb) EXIT
    end do

    call timing_stop(NameSub)

    call test_stop(NameSub, DoTest)
  contains
    !==========================================================================
    subroutine init_divb_cleanup

      integer::i,j,k,iBlock,iError,iLimit
      real,dimension(0:nI+1,0:nJ+1,0:nK+1)::Q_G
      real::EstimateForMAMNorm,OneDotMDotOne
      real:: DivBDiffCoeff, VInvHalf
      !------------------------------------------------------------------------
      Tmp1_GB = 0.0; Prec_CB = 0.0

      do iBlock=1,nBlock
         if(Unused_B(iBlock))CYCLE
         Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) = &
              1/CellVolume_GB(1:nI,1:nJ,1:nK,iBlock)
      end do

      ! tmp1 is equal to the inverse of the volume, including the ghostcells
      call message_pass_cell(Tmp1_GB, DoSendCornerIn=.false. ,&
           nProlongOrderIn=1, DoRestrictFaceIn=.true.)

      do iBlock=1,nBlock
         if (Unused_B(iBlock)) CYCLE

         Q_G = 1.0

         if(any(DiLevel_EB(:,iBlock)/=0))then
            if(DiLevel_EB(1,iBlock)==Unset_)then
               Tmp1_GB(0,1:nJ,1:nK,iBlock) = &
                    1.0/CellVolume_GB(1,1:nJ,1:nK,iBlock)
               ! to define somehow tmp1 in the outer ghostcells

            elseif(abs(DiLevel_EB(1,iBlock))==1)then
               Q_G(0,:,:)=4.0**DiLevel_EB(1,iBlock)
               ! if the neighboring block is coarser,
               !  the input from FA^2 should be multipled by four
               ! If the neighborig block is finer,
               !  the input from FA^2  should be multiplied by 1/4
            end if
            if(DiLevel_EB(2,iBlock)==Unset_)then
               Tmp1_GB(nI+1,1:nJ,1:nK,iBlock) = &
                    1.0/CellVolume_GB(nI,1:nJ,1:nK,iBlock)
            elseif(abs(DiLevel_EB(2,iBlock))==1)then
               Q_G(nI+1,:,:)=4.0**DiLevel_EB(2,iBlock)
            end if
            if(DiLevel_EB(3,iBlock)==Unset_)then
               Tmp1_GB(1:nI,0,1:nK,iBlock) = &
                    1.0/CellVolume_GB(1:nI,1,1:nK,iBlock)
            elseif(abs(DiLevel_EB(3,iBlock))==1)then
               Q_G(:,0,:)=4.0**DiLevel_EB(3,iBlock)
            end if
            if(DiLevel_EB(4,iBlock)==Unset_)then
               Tmp1_GB(1:nI,nJ+1,1:nK,iBlock) = &
                    1.0/CellVolume_GB(1:nI,nJ,1:nK,iBlock)
            elseif(abs(DiLevel_EB(4,iBlock))==1)then
               Q_G(:,nJ+1,:)=4.0**DiLevel_EB(4,iBlock)
            end if
            if(DiLevel_EB(5,iBlock)==Unset_)then
               Tmp1_GB(1:nI,1:nJ,0,iBlock) = &
                    1.0/CellVolume_GB(1:nI,1:nJ,1,iBlock)
            elseif(abs(DiLevel_EB(5,iBlock))==1)then
               Q_G(:,:,0)=4.0**DiLevel_EB(5,iBlock)
            end if
            if(DiLevel_EB(6,iBlock)==Unset_)then
               Tmp1_GB(1:nI,1:nJ,nK+1,iBlock) = &
                    1.0/CellVolume_GB(1:nI,1:nJ,nK,iBlock)
            elseif(abs(DiLevel_EB(6,iBlock))==1)then
               Q_G(:,:,nK+1)=4.0**DiLevel_EB(6,iBlock)
            end if
         end if
         Prec_CB(:,:,:,iBlock)=4.0/(&
              (Q_G(2:nI+1,1:nJ,1:nK)*Tmp1_GB(2:nI+1,1:nJ,1:nK,iBlock)+&
              Q_G(0:nI-1,1:nJ,1:nK)*Tmp1_GB(0:nI-1,1:nJ,1:nK,iBlock))&
              *CellFace_DB(x_,iBlock)**2+&
              (Q_G(1:nI,2:nJ+1,1:nK)*Tmp1_GB(1:nI,2:nJ+1,1:nK,iBlock)+&
              Q_G(1:nI,0:nJ-1,1:nK)*Tmp1_GB(1:nI,0:nJ-1,1:nK,iBlock))&
              *CellFace_DB(y_,iBlock)**2+&
              (Q_G(1:nI,1:nJ,2:nK+1)*Tmp1_GB(1:nI,1:nJ,2:nK+1,iBlock)+&
              Q_G(1:nI,1:nJ,0:nK-1)*Tmp1_GB(1:nI,1:nJ,0:nK-1,iBlock))&
              *CellFace_DB(z_,iBlock)**2)
      end do
      do iLimit = 1, 2
         do iBlock=1,nBlock
            if(Unused_B(iBlock))CYCLE
            if(any(DiLevel_EB(:,iBlock)==Unset_))then
               if(DiLevel_EB(1,iBlock)==Unset_)&
                    Tmp1_GB(0,1:nJ,1:nK,iBlock)=Prec_CB(1,:,:,iBlock)
               if(DiLevel_EB(2,iBlock)==Unset_)&
                    Tmp1_GB(nI+1,1:nJ,1:nK,iBlock)=Prec_CB(nI,:,:,iBlock)
               if(DiLevel_EB(3,iBlock)==Unset_)&
                    Tmp1_GB(1:nI,0,1:nK,iBlock)=Prec_CB(:,1,:,iBlock)
               if(DiLevel_EB(4,iBlock)==Unset_)&
                    Tmp1_GB(1:nI,nJ+1,1:nK,iBlock)=Prec_CB(:,nJ,:,iBlock)
               if(DiLevel_EB(5,iBlock)==Unset_)&
                    Tmp1_GB(1:nI,1:nJ,0,iBlock)=Prec_CB(:,:,1,iBlock)
               if(DiLevel_EB(6,iBlock)==Unset_)&
                    Tmp1_GB(1:nI,1:nJ,nK+1,iBlock)=Prec_CB(:,:,nK,iBlock)
            end if
            Tmp1_GB(1:nI,1:nJ,1:nK,iBlock)=Prec_CB(:,:,:,iBlock)
         end do

         call message_pass_cell(Tmp1_GB,DoSendCornerIn=.false. ,&
              nProlongOrderIn=1, DoRestrictFaceIn=.true.)

         do iBlock=1,nBlock
            if (Unused_B(iBlock)) CYCLE

            do k=1,nK;do j=1,nJ;do i=1,nI
               Prec_CB(i,j,k,iBlock)=min(&
                    minval(Tmp1_GB(i-1:i+1,j,k,iBlock)),&
                    minval(Tmp1_GB(i,j-1:j+1:2,k,iBlock)),&
                    minval(Tmp1_GB(i,j,k-1:k+1:2,iBlock)))
            end do;end do; end do
         end do
      end do

      ! With the preconditioner, defined in the manner like this
      ! the upper estimate of the spectral radius fo the matrix
      ! ||Prec^{1/2}.A.Prec^1/2 is 2, according to the Frobenius inequality

      ! Herewith Prec is also denoted as M^{-1}
      ! Calculate the divB diffusion coefficient =2/||M^(-1/2).A.M^(-1/2)||
      ! For the grid, which is at least partially uniform cartesian,
      ! this coefficient equals 1
      do iBlock=1,nBlock
         if (Unused_B(iBlock)) CYCLE
         if(any(DiLevel_EB(:,iBlock)==Unset_))then
            if(DiLevel_EB(1,iBlock)==Unset_)&
                 Tmp1_GB(0,1:nJ,1:nK,iBlock)=sqrt(Prec_CB(1,:,:,iBlock))
            if(DiLevel_EB(2,iBlock)==Unset_)&
                 Tmp1_GB(nI+1,1:nJ,1:nK,iBlock)=sqrt(Prec_CB(nI,:,:,iBlock))
            if(DiLevel_EB(3,iBlock)==Unset_)&
                 Tmp1_GB(1:nI,0,1:nK,iBlock)=sqrt(Prec_CB(:,1,:,iBlock))
            if(DiLevel_EB(4,iBlock)==Unset_)&
                 Tmp1_GB(1:nI,nJ+1,1:nK,iBlock)=sqrt(Prec_CB(:,nJ,:,iBlock))
            if(DiLevel_EB(5,iBlock)==Unset_)&
                 Tmp1_GB(1:nI,1:nJ,0,iBlock)=sqrt(Prec_CB(:,:,1,iBlock))
            if(DiLevel_EB(6,iBlock)==Unset_)&
                 Tmp1_GB(1:nI,1:nJ,nK+1,iBlock)=sqrt(Prec_CB(:,:,nK,iBlock))
         end if
         Tmp1_GB(1:nI,1:nJ,1:nK,iBlock)=sqrt(Prec_CB(:,:,:,iBlock))
      end do
      !    if(iProc==0)write(*,*)' Cleanup Initialization second message pass'

      call message_pass_cell(Tmp1_GB,DoSendCornerIn=.false. ,&
           nProlongOrderIn=1, DoRestrictFaceIn=.true.)

      ! Now the elements of diag(Prec_CB)^{1/2} are in Tmp1_GB

      do iBlock=1,nBlock
         if (Unused_B(iBlock)) CYCLE

         Q_G=Tmp1_GB(0:nI+1,0:nJ+1,0:nK+1,iBlock)
         Tmp1_GB(:,:,:,iBlock) = 0.0
         Tmp2_GB(:,:,:,iBlock) = 0.0
         tmp3_BLK(:,:,:,iBlock) = 0.0

         do k=1,nK;do j=1,nJ;do i=1,nI
            VInvHalf = 0.5/CellVolume_GB(i,j,k,iBlock)
            Tmp1_GB(i,j,k,iBlock) = &
                 CellFace_DB(x_,iBlock)*VInvHalf*(&
                 Q_G(i+1,j,k)+&
                 Q_G(i-1,j,k))
            Tmp2_GB(i,j,k,iBlock) = &
                 CellFace_DB(y_,iBlock)*VInvHalf*(&
                 Q_G(i,j+1,k)+&
                 Q_G(i,j-1,k))
            tmp3_BLK(i,j,k,iBlock) = &
                 CellFace_DB(z_,iBlock)*VInvHalf*(&
                 Q_G(i,j,k+1)+&
                 Q_G(i,j,k-1))
         end do;end do; end do
      end do
      ! In tmp1,tmp2 and divB1 are the estimates of gradX, gradY,gradZ
      ! correspondenyly

      call message_pass_cell(Tmp1_GB,DoSendCornerIn=.false. ,&
           nProlongOrderIn=1, DoRestrictFaceIn=.true.)
      call message_pass_cell(Tmp2_GB,DoSendCornerIn=.false. ,&
           nProlongOrderIn=1, DoRestrictFaceIn=.true.)
      call message_pass_cell(tmp3_blk,DoSendCornerIn=.false. ,&
           nProlongOrderIn=1, DoRestrictFaceIn=.true.)

      EstimateForMAMNorm = 0.0
      do iBlock=1,nBlock
         if(Unused_B(iBlock))CYCLE
         EstimateForMAMNorm=max(EstimateForMAMNorm,&
              maxval(sqrt(Prec_CB(:,:,:,iBlock))*&
              0.5*( &
              CellFace_DB(x_,iBlock)*&
              (Tmp1_GB(2:nI+1, 1:nJ, 1:nK,iBlock)+&
              Tmp1_GB(0:nI-1, 1:nJ, 1:nK,iBlock))&
              +CellFace_DB(y_,iBlock)*&
              (Tmp2_GB(1:nI, 2:nJ+1, 1:nK,iBlock)+&
              Tmp2_GB(1:nI, 0:nJ-1, 1:nK,iBlock))&
              +CellFace_DB(z_,iBlock)*&
              (tmp3_blk(1:nI, 1:nJ, 2:nK+1,iBlock)+&
              tmp3_blk(1:nI, 1:nJ, 0:nK-1,iBlock)) ) ))
      end do

      if(nProc>1) call MPI_allreduce(MPI_IN_PLACE, EstimateForMAMNorm,  &
           1, MPI_REAL, MPI_MAX, iComm, iError)

      DivBDiffCoeff = 2/EstimateForMAMNorm

      if(iProc==0)write(*,*)"Divb diffusion coefficient is: ",DivBDiffCoeff

      ! Compute 1/sum(M_i)
      OneDotMDotOne = 0.0
      do iBlock=1,nBlock
         if (Unused_B(iBlock)) CYCLE
         !     Prec_CB(:,:,:,iBlock)=Prec_CB(:,:,:,iBlock)*DivBDiffCoeff
         if(IsNoBody_B(iBlock))then
            OneDotMDotOne=OneDotMDotOne+sum(1.0/Prec_CB(:,:,:,iBlock))
         elseif(any(Used_GB(1:nI,1:nJ,1:nK,iBlock)))then
            OneDotMDotOne=OneDotMDotOne+sum(1.0/Prec_CB(:,:,:,iBlock)&
                 ,MASK=Used_GB(1:nI,1:nJ,1:nK,iBlock))
         end if
      end do
      if(nProc>1) call MPI_allreduce(MPI_IN_PLACE, OneDotMDotOne, &
           1, MPI_REAL, MPI_SUM, iComm, iError)

      OneDotMDotOneInv = 1.0/OneDotMDotOne

      if(iProc==0)write(*,*) NameSub, &
           ' finishes with OneDotMDotOneInv=',OneDotMDotOneInv

    end subroutine init_divb_cleanup
    !==========================================================================
    subroutine v_grad_phi(Phi_GB,iBlock)

      integer,intent(in) :: iBlock
      real, intent(inout):: Phi_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)

      !------------------------------------------------------------------------
      VDotGrad_DC = 0.0

      if (DiLevel_EB(1,iBlock) == Unset_) Phi_GB(0,1:nJ,1:nK,iBlock) = &
           -BoundaryCoef*Phi_GB(1 ,1:nJ,1:nK,iBlock)
      if (DiLevel_EB(2,iBlock) == Unset_) Phi_GB(nI+1,1:nJ,1:nK,iBlock) = &
           -BoundaryCoef*Phi_GB(nI,1:nJ,1:nK,iBlock)
      if (DiLevel_EB(3,iBlock) == Unset_) Phi_GB(1:nI,j0_ ,1:nK,iBlock) = &
           -BoundaryCoef*Phi_GB(1:nI,1 ,1:nK,iBlock)
      if (DiLevel_EB(4,iBlock) == Unset_) Phi_GB(1:nI,nJp1_,1:nK,iBlock) = &
           -BoundaryCoef*Phi_GB(1:nI,nJ,1:nK,iBlock)
      if (DiLevel_EB(5,iBlock) == Unset_ .and. nK>1) &
           Phi_GB(1:nI,1:nJ,k0_,iBlock) = &
           -BoundaryCoef*Phi_GB(1:nI,1:nJ,1 ,iBlock)
      if (DiLevel_EB(6,iBlock) == Unset_ .and. nK>1) &
           Phi_GB(1:nI,1:nJ,nKp1_,iBlock) = &
           -BoundaryCoef*Phi_GB(1:nI,1:nJ,nK,iBlock)

      if(IsBody_B(iBlock))then
         do k = 1, nK; do j = 1, nJ;do i = 1, nI
            if(.not.Used_GB(i,j,k,iBlock))CYCLE
            where(.not.Used_GB(i-1:i+1:2,j,k,iBlock))&
                 Phi_GB(i-1:i+1:2,j,k,iBlock) = &
                 -BoundaryCoef*Phi_GB(i,j,k,iBlock)
            where(.not.Used_GB(i,j-1:j+1:2,k,iBlock))&
                 Phi_GB(i,j-1:j+1:2,k,iBlock) = &
                 -BoundaryCoef*Phi_GB(i,j,k,iBlock)
            where(.not.Used_GB(i,j,k-1:k+1:2,iBlock))&
                 Phi_GB(i,j,k-1:k+1:2,iBlock) = &
                 -BoundaryCoef*Phi_GB(i,j,k,iBlock)
            VDotGrad_DC(x_,i,j,k)=&
                 0.5*CellFace_DB(x_,iBlock)*(&
                 Phi_GB(i+1,j,k,iBlock)-&
                 Phi_GB(i-1,j,k,iBlock))
            VDotGrad_DC(y_,i,j,k)=&
                 0.5*CellFace_DB(y_,iBlock)*(&
                 Phi_GB(i,j+1,k,iBlock)-&
                 Phi_GB(i,j-1,k,iBlock))
            VDotGrad_DC(z_,i,j,k)=&
                 0.5*CellFace_DB(z_,iBlock)*(&
                 Phi_GB(i,j,k+1,iBlock)-&
                 Phi_GB(i,j,k-1,iBlock))
         end do;end do;end do
      else
         do k=1,nK;do j=1,nJ;do i=1,nI
            VDotGrad_DC(x_,i,j,k)=&
                 0.5*CellFace_DB(x_,iBlock)*(&
                 Phi_GB(i+1,j,k,iBlock)-&
                 Phi_GB(i-1,j,k,iBlock))
            VDotGrad_DC(y_,i,j,k)=&
                 0.5*CellFace_DB(y_,iBlock)*(&
                 Phi_GB(i,j+1,k,iBlock)-&
                 Phi_GB(i,j-1,k,iBlock))
            VDotGrad_DC(z_,i,j,k)=&
                 0.5*CellFace_DB(z_,iBlock)*(&
                 Phi_GB(i,j,k+1,iBlock)-&
                 Phi_GB(i,j,k-1,iBlock))
         end do;end do;end do
      end if
    end subroutine v_grad_phi
    !==========================================================================

  end subroutine clean_divb
  !============================================================================

  subroutine div3d_b1(iBlock,VecX_G,VecY_G,VecZ_G,Out_G)

    ! Can only be used for divB diffusion and projection scheme!!!!
    ! DivB is multiplied by -V_Cell!!!!
    ! With this modification DivB[grad Phi] is a symmetric positive definite
    ! operator!

    use ModSize, ONLY: nI, nJ, nK, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, &
         x_, y_, z_
    use ModGeometry, ONLY: IsBody_B
    use ModParallel, ONLY: DiLevel_EB, Unset_
    use BATL_lib, ONLY: CellFace_DB, Used_GB

    integer,intent(in) :: iBlock
    real,dimension(MinI:MaxI,MinJ:MaxJ,MinK:MaxK),intent(in)::&
         VecX_G,VecY_G,VecZ_G
    real,dimension(MinI:MaxI,MinJ:MaxJ,MinK:MaxK),intent(out)::Out_G

    real, dimension(0:nI+1, 0:nJ+1, 0:nK+1) :: OneTrue_G

    integer :: i, j, k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'div3d_b1'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    Out_G = 0.0
    if(.not.(IsBody_B(iBlock).or.any(DiLevel_EB(:,iBlock)==Unset_)))then
       do k=1,nK; do j=1,nJ; do i=1,nI
          Out_G(i,j,k) = - 0.5*(&
               CellFace_DB(x_,iBlock)*&
               (VecX_G(i+1, j, k)-VecX_G(i-1,j,k))&
               +CellFace_DB(y_,iBlock)*&
               (VecY_G(i ,j+1, k)-VecY_G(i,j-1,k))&
               +CellFace_DB(z_,iBlock)*&
               (VecZ_G(i, j, k+1)-VecZ_G(i,j,k-1)) )
       end do; end do; end do
    else
       where(Used_GB(0:nI+1, 0:nJ+1, 0:nK+1,iBlock))
          OneTrue_G = 1.0
       elsewhere
          OneTrue_G = 0.0
       end where
       if(DiLevel_EB(1 ,iBlock)==Unset_) OneTrue_G(0   ,:,:) = 0.0
       if(DiLevel_EB(2 ,iBlock)==Unset_) OneTrue_G(nI+1,:,:) = 0.0
       if(DiLevel_EB(3,iBlock)==Unset_) OneTrue_G(:,0   ,:) = 0.0
       if(DiLevel_EB(4,iBlock)==Unset_) OneTrue_G(:,nJ+1,:) = 0.0
       if(DiLevel_EB(5  ,iBlock)==Unset_) OneTrue_G(:,:,0   ) = 0.0
       if(DiLevel_EB(6  ,iBlock)==Unset_) OneTrue_G(:,:,nK+1) = 0.0

       ! Where .not.Used_GB, all the gradients are zero
       ! In Used_GB the input to gradient from the face neighbor
       ! is ignored, if the face neighbor is .not.Used_GB

       do k=1,nK; do j=1,nJ; do i=1,nI
          Out_G(i,j,k) = - 0.5*OneTrue_G(i,j,k)*(+&
               CellFace_DB(x_,iBlock)*&
               (VecX_G(i+1,j,k)-&
               BoundaryCoef*(VecX_G(i+1,j,k)-VecX_G(i,j,k))*&
               (1.0-OneTrue_G(i+1,j,k))-&
               VecX_G(i-1,j,k)-&
               BoundaryCoef*(VecX_G(i,j,k)-VecX_G(i-1,j,k))*&
               (1.0-OneTrue_G(i-1,j,k)))+&
               CellFace_DB(y_,iBlock)*&
               (VecY_G(i,j+1,k)-&
               BoundaryCoef*(VecY_G(i,j+1,k)-VecY_G(i,j,k))*&
               (1.0-OneTrue_G(i,j+1,k))-&
               VecY_G(i,j-1,k)-&
               BoundaryCoef*(VecY_G(i,j,k)-VecY_G(i,j-1,k))*&
               (1.0-OneTrue_G(i,j-1,k)))+&
               CellFace_DB(z_,iBlock)*&
               (VecZ_G(i,j,k+1)-&
               BoundaryCoef*(VecZ_G(i,j,k+1)-VecZ_G(i,j,k))*&
               (1.0-OneTrue_G(i,j,k+1))-&
               VecZ_G(i,j,k-1)-&
               BoundaryCoef*(VecZ_G(i,j,k)-VecZ_G(i,j,k-1))*&
               (1.0-OneTrue_G(i,j,k-1))) &
               )
       end do; end do; end do
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine div3d_b1
  !============================================================================

end module ModCleanDivB
!==============================================================================
