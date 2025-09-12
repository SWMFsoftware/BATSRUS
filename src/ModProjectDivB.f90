!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModProjectDivB

  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest, iBlockTest, &
       iProc, nProc, iComm, minval_grid, maxval_grid
  use ModBatsrusUtility, ONLY: error_report, stop_mpi

  ! Parameters for projection scheme:
  !
  ! TypeProjectIter determines the iterative scheme to be used
  ! if TypeProjectIter='cg'       then Conjugate Gradient method is used
  !                           (this is only good for symmetric matrices! )
  !    TypeProjectIter='bicgstab' then BiConjugate Gradient method is used
  !
  ! TypeProjectStop determines the stopping condition for the iterations.
  ! if TypeProjectStop='rel' then sum((div B)**2) is reduced by a factor
  !    RelativeLimit
  ! if TypeProjectStop='max' then |div B| is kept below a limit determeined by
  !    RelativeLimit and
  !    AbsoluteLimit
  !    if RelativeLimit>0 then
  !       |div B|< RelativeLimit * divbmax_0
  !          where divbmax_0 is the max(|div B|) after the first time step
  !    otherwise
  !       |div B|< AbsoluteLimit
  !
  ! MaxMatvec is an upper limit on matrix-vector products for iterations

  use ModSize, ONLY: nI, nJ, nK, nBlock, &
       MinI, MaxI, MinJ, MaxJ, MinK, MaxK, MaxBlock
  use ModMain, ONLY: Unused_B
  use ModGeometry, ONLY: IsNoBody_B
  use ModMpi

  implicit none
  SAVE

  private ! except

  public:: project_divb            ! project to div B=0 with Poisson solve
  public:: read_project_divb_param ! read parameters for projection scheme
  public:: proj_get_divb           ! Calculate div B for projection or CT

  ! Maximum value for divB (absolute or relative)
  real, public :: DivbMax = -1.0

  ! Local variables
  character (len=10):: TypeProjectIter   ='cg        '
  character (len=3)::  TypeProjectStop ='rel'
  real ::              RelativeLimit=0.1
  real ::              AbsoluteLimit=0.0
  integer ::           MaxMatvec=50

  real, allocatable:: GradPhi_DGB(:,:,:,:,:)

  ! Minimum value for divB (a small number)
  real, parameter :: DivBTiny=1E-10

contains
  !============================================================================
  subroutine read_project_divb_param(NameCommand)

    use ModReadParam, ONLY: read_var

    character(len=*), intent(in):: NameCommand

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_project_divb_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(NameCommand)
    case("#PROJECTION")
       call read_var('TypeProjectIter' ,TypeProjectIter)
       call read_var('TypeProjectStop' ,TypeProjectStop)
       call read_var('RelativeLimit'   ,RelativeLimit)
       call read_var('AbsoluteLimit'   ,AbsoluteLimit)
       call read_var('MaxMatvec'       ,MaxMatvec)
       ! Make sure that DivbMax is recalculated
       DivbMax = -1.0
    case default
       call stop_mpi(NameSub//': unknown command='//NameCommand)
    end select

    call test_stop(NameSub, DoTest)
  end subroutine read_project_divb_param
  !============================================================================
  subroutine project_divb

    ! Project B according to B'=B-grad Phi_GB, where Laplace Phi_GB=div B
    ! Solve the Poisson equation with an iterative scheme
    !
    ! See G. Toth, 2000, Journal of Computational Physics, 161, 605-652

    use ModVarIndexes, ONLY: Bx_,Bz_,P_
    use ModAdvance, ONLY: State_VGB
    use ModMain, ONLY: UseConstrainB
    use ModConstrainDivB, ONLY: BxFace_GB, ByFace_GB, BzFace_GB
    use ModMessagePass, ONLY: exchange_messages
    use BATL_lib, ONLY: Xyz_DGB, Used_GB

    ! Local variables
    ! DivB_GB: original error to be projected out
    ! Phi_GB:       Scalar field in the Poisson problem
    real, allocatable, save:: DivB_GB(:,:,:,:), Phi_GB(:,:,:,:)

    integer :: iInfo      ! error status of the iterative scheme
    integer :: nMatvec   ! number of matvex operations performed
    real ::    Resid     ! residual after iterative solver stopped

    real :: DivBMaxNow  ! current value of max(|div B|)
    real :: pMinOld, pMinNew     ! original and projected value of min(p)

    integer :: iLoc_I(5)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'project_divb'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    call timing_start(NameSub)

    if(.not.allocated(DivB_GB)) allocate( &
         DivB_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock), &
         Phi_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))

    if(DoTest)then
       write(*,*) NameSub, ': old B:', &
            State_VGB(Bx_:Bz_,iTest,jTest,kTest,iBlockTest),  &
            Used_GB(iTest,jTest,kTest,iBlockTest)
       if(UseConstrainB)then
          write(*,*) NameSub, ': Bxface,Used_GB:',    &
               BxFace_GB(iTest,jTest,kTest,iBlockTest),  &
               BxFace_GB(iTest+1,jTest,kTest,iBlockTest),&
               Used_GB(iTest-1,jTest,kTest,iBlockTest), &
               Used_GB(iTest+1,jTest,kTest,iBlockTest)

          write(*,*) NameSub, ': Byface,Used_GB:',    &
               ByFace_GB(iTest,jTest,kTest,iBlockTest),  &
               ByFace_GB(iTest,jTest+1,kTest,iBlockTest),&
               Used_GB(iTest,jTest-1,kTest,iBlockTest), &
               Used_GB(iTest,jTest+1,kTest,iBlockTest)

          write(*,*) NameSub, ': Bzface,Used_GB:',    &
               BzFace_GB(iTest,jTest,kTest,iBlockTest),  &
               BzFace_GB(iTest,jTest,kTest+1,iBlockTest),&
               Used_GB(iTest,jTest,kTest-1,iBlockTest), &
               Used_GB(iTest,jTest,kTest+1,iBlockTest)

       end if
    end if

    ! Determine the div B error
    call proj_get_divb(DivB_GB)

    if(DoTest)write(*,*)'DivB_GB=',DivB_GB(iTest,jTest,kTest,iBlockTest)

    ! DEBUG
    ! call proj_bound(DivB_GB)
    ! call show_BLK('divb',DivB_GB)
    ! DEBUG
    ! call stop_mpi('debug')

    if(DoTest)then
       pMinOld = minval_grid(State_VGB(P_,:,:,:,:))
       DivBMaxNow = maxval_grid(DivB_GB, UseAbs=.true.)
       if(DoTest) write(*,*) NameSub, &
            ': old max(abs(divB)), min(p)=',DivBMaxNow,pMinOld
    endif

    ! Calculate and save divbmax, the maximum div B allowed
    if(divbmax < DivBTiny)then
       ! Default values if projection parameters were not set
       if(RelativeLimit<0.0)  RelativeLimit=0.1
       if(AbsoluteLimit<0.0)  AbsoluteLimit=0.0
       if(MaxMatvec<0)MaxMatvec=1000
       select case(TypeProjectStop)
       case('rel')
          divbmax=RelativeLimit
       case('max')
          if(RelativeLimit > 0.0)then
             divbmax = RelativeLimit &
                  *maxval_grid(DivB_GB, UseAbs=.true.)
          else
             divbmax = AbsoluteLimit
          endif
       end select
       if(DoTest)write(*,*)'Allowed maximum for div B:',divbmax
       if(divbmax<DivBTiny)&
            call stop_mpi(NameSub//': Too small value for divbmax')
    endif

    ! Solve the Poisson equation Laplace Phi_GB = div B
    call proj_poisson(DivB_GB,divbmax,TypeProjectStop,MaxMatvec, &
         iInfo,nMatvec,Resid,Phi_GB)

    if(DoTest)write(*,*) NameSub, ': Poisson solver iInfo, nMatvec, Resid:',&
         iInfo, nMatvec, Resid

    ! Do not do anything if the initial guess satisfied the stopping criterion
    if(iInfo==3)GOTO 100

    ! Do not subtract grad(Phi_GB) from B if the iterations
    ! did not reduce the error
    if (iInfo < 0) GOTO 100

    ! Get the ghost cell values for the solution Phi_GB
    call proj_boundphi(Phi_GB)

    ! Correct B field: B'=B-dB where dB=grad(Phi_GB)
    call proj_correction(Phi_GB)

    ! Recalculate boundary conditions
    call exchange_messages

    ! Testing div B'=0
    if(DoTest)then
       call proj_get_divb(DivB_GB)

       if(DoTest)write(*,*)'after DivB_GB=',&
            DivB_GB(iTest,jTest,kTest,iBlockTest)

       DivBMaxNow = &
            maxval_grid(DivB_GB, UseAbs=.true., iLoc_I=iLoc_I)
       if(abs(Resid-DivBMaxNow)/(DivBMaxNow+1.e-12) > 0.1)then
          if(iProc==iLoc_I(5))write(*,*)&
                NameSub, ': Resid,max(abs(divB)),iLoc_I,X,Y,Z=',&
               Resid, DivBMaxNow, iLoc_I, &
               Xyz_DGB(:,iLoc_I(1),iLoc_I(2),iLoc_I(3),iLoc_I(4))
       else
          if(DoTest)write(*,*) NameSub, ': Resid,max(abs(divB))',&
               Resid, DivBMaxNow
       end if
       pMinNew = minval_grid(State_VGB(P_,:,:,:,:), iLoc_I=iLoc_I)
       if(pMinNew<0.5*pMinOld)then
          if(iProc==iLoc_I(5))write(*,*) NameSub, ': min(p),iLoc_I,X,Y,Z=',&
               pMinNew, iLoc_I, &
               Xyz_DGB(:,iLoc_I(1),iLoc_I(2),iLoc_I(3),iLoc_I(4))
       else
          if(DoTest)write(*,*) NameSub, ': new min(p)',pMinNew
       endif

       if(DoTest)then
          write(*,*) NameSub, ': new B, Used_GB:', &
               State_VGB(Bx_:Bz_,iTest,jTest,kTest,iBlockTest),  &
               Used_GB(iTest,jTest,kTest,iBlockTest)
          if(UseConstrainB)then
             write(*,*) NameSub, ': Bxface,Used_GB:',    &
                  BxFace_GB(iTest,jTest,kTest,iBlockTest),  &
                  BxFace_GB(iTest+1,jTest,kTest,iBlockTest),&
                  Used_GB(iTest-1,jTest,kTest,iBlockTest), &
                  Used_GB(iTest+1,jTest,kTest,iBlockTest)

             write(*,*) NameSub, ': Byface,Used_GB:',    &
                  ByFace_GB(iTest,jTest,kTest,iBlockTest),  &
                  ByFace_GB(iTest,jTest+1,kTest,iBlockTest),&
                  Used_GB(iTest,jTest-1,kTest,iBlockTest), &
                  Used_GB(iTest,jTest+1,kTest,iBlockTest)

             write(*,*) NameSub, ': Bzface,Used_GB:',    &
                  BzFace_GB(iTest,jTest,kTest,iBlockTest),  &
                  BzFace_GB(iTest,jTest,kTest+1,iBlockTest),&
                  Used_GB(iTest,jTest,kTest-1,iBlockTest), &
                  Used_GB(iTest,jTest,kTest+1,iBlockTest)

          end if
       end if
    endif

    ! if(DoTest)write(*,*)'final DivB_GB=',&
    !        DivB_GB(iTest,jTest,kTest,iBlockTest)

100 call timing_stop(NameSub)

    call test_stop(NameSub, DoTest)
  end subroutine project_divb
  !============================================================================

  subroutine proj_get_divb(DivB_GB)

    ! Calculate div B using simple finite differences
    ! Do corrections for mesh refinement

    use ModVarIndexes, ONLY: Bx_, Bz_
    use ModAdvance, ONLY: State_VGB
    use ModMain, ONLY: UseConstrainB
    use ModConstrainDivB, ONLY: BxFace_GB, ByFace_GB, BzFace_GB
    use BATL_lib, ONLY:  nI,nJ,nK, nG, MaxBlock, nBlock, Unused_B, &
         x_, y_, z_, CellSize_DB
    use ModCellGradient, ONLY: calc_divergence
    ! Argument

    real, intent(out) :: DivB_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)

    ! Local variables
    integer :: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'proj_get_divb'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    DivB_GB=0.0

    if(UseConstrainB)then
       do iBlock=1,nBlock
          if(Unused_B(iBlock)) CYCLE

          DivB_GB(1:nI,1:nJ,1:nK,iBlock)= &
               (BxFace_GB(2:nI+1,1:nJ  ,1:nK  ,iBlock)                      &
               -BxFace_GB(1:nI  ,1:nJ  ,1:nK  ,iBlock))/CellSize_DB(x_,iBlock)&
               +(ByFace_GB(1:nI  ,2:nJ+1,1:nK  ,iBlock)                      &
               -ByFace_GB(1:nI  ,1:nJ  ,1:nK  ,iBlock))/CellSize_DB(y_,iBlock)&
               +(BzFace_GB(1:nI  ,1:nJ  ,2:nK+1,iBlock)                      &
               -BzFace_GB(1:nI  ,1:nJ  ,1:nK  ,iBlock))/CellSize_DB(z_,iBlock)
       end do
    else
       do iBlock=1,nBlock
          if(Unused_B(iBlock))CYCLE
          call calc_divergence(iBlock, State_VGB(Bx_:Bz_,:,:,:,iBlock), nG, &
               DivB_GB(:,:,:,iBlock), UseBodyCellIn=.true.)
       end do
    end if

    call test_stop(NameSub, DoTest)
  end subroutine proj_get_divb
  !============================================================================

  ! This is just an interface to the various iterative schemes to
  ! Solve grad div Phi_GB = Rhs_GB
  subroutine proj_poisson(Rhs_GB,Tolerance,TypeStop,MaxMatvec,&
       iInfo,nMatvec,Resid,Phi_GB)
    use ModMain, ONLY: MaxBlock

    ! Arguments

    real, intent(inout):: Rhs_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    !    on input: the right hand side of the Poisson equation
    !    on output: the residual

    character (len=3), intent(in) :: TypeStop
    real, intent(in) :: Tolerance
    ! The required accuracy of the solution is given by Tolerance and TypeStop:
    !    TypeStop='max': Resid=max(abs(Laplace(Phi)-Rhs)) < Tolerance
    !    TypeStop='abs': Resid=sum((Laplace(Phs)-Rhs)**2) < Tolerance
    !    TypeStop='rel': Resid=sum((Laplace(Phi)-Rhs)**2) < Tolerance*Resid0
    !       where Resid0 = sum(Rhs**2) is the initial residual

    integer, intent(in) :: MaxMatvec
    ! At most MaxMatvec matrix vector multiplications can be performed

    integer, intent(out) :: iInfo
    ! The success of the iteration is given by returned value of iInfo:
    !     abs(iInfo)= 0 - solution found satisfying given Tolerance.
    !                 1 - iteration aborted due to division by very small value
    !                 2 - no convergence within maximum number of iterations.
    !                 3 - initial guess satisfies the stopping criterion.
    !    sign(iInfo)= + - residual decreased
    !                 - - residual did not reduce

    integer, intent(out) :: nMatvec
    ! The total number of matvec operations done during the iterations

    real, intent(out)    :: Resid
    ! The residual after the iterations

    real, intent(out):: Phi_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    !    The solution

    ! Local variables

    integer :: iError=-1

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'proj_poisson'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(DoTest)write(*,*)'Proj_Poisson solver called with ',TypeProjectIter

    ! Initialize parameters and Phi_GB for the iterative solvers
    nMatvec =MaxMatvec
    Resid=Tolerance

    ! Solve the Poisson equation
    select case(TypeProjectIter)
    case('cg')
       call proj_cg(Rhs_GB,Phi_GB,nMatvec,Resid,TypeStop,iInfo)
    case('bicgstab')
       call proj_bicgstab(Rhs_GB,Phi_GB,nMatvec,Resid,TypeStop,iInfo)
    case default
       call stop_mpi('Error in Proj_Poisson: Unknown type of iterative method')
    end select

    if(DoTest)write(*,*)'Poisson iInfo,nMatvec,Resid',iInfo,nMatvec,Resid

    if(iInfo/=0.and.iInfo/=3)then
       if(iProc==0)then
          if(iError<1)then
             ! Print additional information
             write(*,"(a,i2,a,i5)")'iInfo=',iInfo,' nMatvec=',nMatvec
             select case(abs(iInfo))
             case(1)
                write(*,*)'Breakdown due to division by a small value'
             case(2)
                write(*,*)'No convergence within maximum number of iterations'
             end select
             if(iInfo>0)write(*,*)'The residual decreased'
             if(iInfo<0)write(*,*)'The residual did not decrease'
          end if
          call error_report('Poisson solver failed, Resid',Resid,iError,.true.)
       end if
    end if

    call test_stop(NameSub, DoTest)
  end subroutine proj_poisson
  !============================================================================

  ! Calculate Laplace Phi_GB
  subroutine proj_matvec(Phi_GB,LaplacePhi_GB)
    use ModMain, ONLY: MaxBlock,nBlock,Unused_B,nI,nJ,nK, x_, y_, z_
    use ModGeometry, ONLY: IsBody_B
    use ModMain, ONLY: UseConstrainB
    use BATL_lib, ONLY: CellSize_DB, nG, message_pass_cell, Used_GB
    use ModCellGradient, ONLY: calc_gradient, calc_divergence

    ! Arguments

    real, intent(inout) :: Phi_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    real, intent(out) :: LaplacePhi_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)

    ! Local variables
    integer :: iBlock
    integer :: i,j,k
    real :: Phi_III(-1:1,-1:1,-1:1), InvDx2, InvDy2, InvDz2
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'proj_matvec'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    call proj_boundphi(Phi_GB)

    if(UseConstrainB)then
       do iBlock=1,nBlock
          if(Unused_B(iBlock))CYCLE

          if(IsBody_B(iBlock))then
             ! If some cells are inside the body, Phi_GB should have 0 gradient
             ! accross the face between the body and the physical cell so that
             ! there is correction done for Bface on that face
             do k = 1, nK; do j = 1, nJ; do i = 1, nI

                if(.not.Used_GB(i,j,k,iBlock)) CYCLE

                where(Used_GB(i-1:i+1,j-1:j+1,k-1:k+1,iBlock))
                   Phi_III=Phi_GB(i-1:i+1,j-1:j+1,k-1:k+1,iBlock)
                elsewhere
                   Phi_III=Phi_GB(i,j,k,iBlock)
                end where

                LaplacePhi_GB(i,j,k,iBlock)= &
                     (Phi_III(1,0,0)+Phi_III(-1,0,0)-2*Phi_III(0,0,0)) &
                     /CellSize_DB(x_,iBlock)**2 + &
                     (Phi_III(0,1,0)+Phi_III(0,-1,0)-2*Phi_III(0,0,0)) &
                     /CellSize_DB(y_,iBlock)**2 + &
                     (Phi_III(0,0,1)+Phi_III(0,0,-1)-2*Phi_III(0,0,0)) &
                     /CellSize_DB(z_,iBlock)**2
             end do; end do; end do
          else
             InvDx2 = 1/CellSize_DB(x_,iBlock)**2
             InvDy2 = 1/CellSize_DB(y_,iBlock)**2
             InvDz2 = 1/CellSize_DB(z_,iBlock)**2
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                LaplacePhi_GB(i,j,k,iBlock)= &
                     (Phi_GB(i+1,j,k,iBlock) + Phi_GB(i-1,j,k,iBlock) &
                     -2*Phi_GB(i,j,k,iBlock))*InvDx2 + &
                     (Phi_GB(i,j+1,k,iBlock) + Phi_GB(i,j-1,k,iBlock) &
                     -2*Phi_GB(i,j,k,iBlock))*InvDy2 + &
                     (Phi_GB(i,j,k+1,iBlock) + Phi_GB(i,j,k-1,iBlock) &
                     -2*Phi_GB(i,j,k,iBlock))*InvDz2
             end do; end do; end do
          endif

       end do

       RETURN
    end if

    if(.not.allocated(GradPhi_DGB)) &
         allocate(GradPhi_DGB(3,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
    GradPhi_DGB = 0.0

    ! Calculate gradient of Phi
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       call calc_gradient(iBlock, Phi_GB(:,:,:,iBlock), &
            nG, GradPhi_DGB(:,:,:,:,iBlock), UseBodyCellIn=.true.)
    end do

    ! Fill in ghost cells
    call message_pass_cell(3,GradPhi_DGB)

    ! Calculate Laplace(Phi)
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE

       call calc_divergence(iBlock, GradPhi_DGB(:,:,:,:,iBlock), nG, &
            LaplacePhi_GB(:,:,:,iBlock), UseBodyCellIn=.true.)
    end do

    ! call proj_gradient(1,Phi_GB,dPhi_GB)

    ! call proj_boundphi(dPhi_GB)

    ! call proj_gradient(1,dPhi_GB,LaplacePhi_GB)

    ! do iDim=2,3
    !    call proj_gradient(iDim,Phi_GB,dPhi_GB)

    !    call proj_boundphi(dPhi_GB)

    !    call proj_gradient(iDim,dPhi_GB,DdPhi_GB)

    !    call add_block(LaplacePhi_GB,DdPhi_GB)

    ! end do

    call test_stop(NameSub, DoTest)
  end subroutine proj_matvec
  !============================================================================

  subroutine proj_gradient(iDim,Phi_GB,dPhi_GB)

    ! Calculate gradient of Phi_GB in direction iDim

    use ModSize, ONLY: x_, y_, z_, &
         MinI, MaxI, MinJ, MaxJ, MinK, MaxK, nI, nJ, nK, nBlock, MaxBlock
    use ModMain, ONLY: Unused_B
    use BATL_lib, ONLY: CellSize_DB

    ! Arguments
    integer, intent(in) :: iDim
    real, intent(in) :: Phi_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)

    real, intent(out) :: dPhi_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)

    ! Local variables
    integer :: i, j, k, iBlock
    real:: Coef

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'proj_gradient'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(DoTest)write(*,*)'Gradient, iDim=',iDim
    ! call show_BLK('Phi_GB',Phi_GB)

    ! dPhi_GB=0.0

    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE
       select case(iDim)
       case(1)
          Coef = 0.5/CellSize_DB(x_,iBlock)
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             dPhi_GB(i,j,k,iBlock) = &
                  Coef*(Phi_GB(i+1,j,k,iBlock) - Phi_GB(i-1,j,k,iBlock))
          end do; end do; end do
       case(2)
          Coef = 0.5/CellSize_DB(y_,iBlock)
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             dPhi_GB(i,j,k,iBlock) = &
                  Coef*(Phi_GB(i,j+1,k,iBlock) - Phi_GB(i,j-1,k,iBlock))
          end do; end do; end do
       case(3)
          Coef = 0.5/CellSize_DB(z_,iBlock)
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             dPhi_GB(i,j,k,iBlock) = &
                  Coef*(Phi_GB(i,j,k+1,iBlock) - Phi_GB(i,j,k-1,iBlock))
          end do; end do; end do
       end select
    end do ! All blocks are done

    ! call show_BLK('dPhi_GB',dPhi_GB)

    ! !! CORRECT dPhi_GB FOR MESH REFINEMENT EFFECTS

    call test_stop(NameSub, DoTest)
  end subroutine proj_gradient
  !============================================================================

  subroutine proj_boundphi(Phi_GB)

    ! Calculate boundary values for Phi_GB for dimensions

    use ModMain, ONLY: MaxBlock, nBlock, Unused_B
    use ModGeometry, ONLY: IsBody_B
    use ModParallel, ONLY: Unset_, DiLevel_EB
    use BATL_lib, ONLY: message_pass_cell, &
         MinI, MaxI, j0_, nJp1_, MinJ, MaxJ, k0_, nKp1_, MinK, MaxK, Used_GB

    ! Arguments
    real,    intent(inout) :: Phi_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)

    ! Local variables
    integer :: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'proj_boundphi'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    call message_pass_cell(Phi_GB, nWidthIn=1, nProlongOrderIn=1, &
         DoSendCornerIn=.false., DoRestrictFaceIn = .true.)

    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE

       if(DiLevel_EB(1,iBlock) ==Unset_) Phi_GB(MinI:0   ,:,:,iBlock) = 0.0
       if(DiLevel_EB(2,iBlock) ==Unset_) Phi_GB(nI+1:MaxI,:,:,iBlock) = 0.0
       if(DiLevel_EB(3,iBlock)==Unset_) Phi_GB(:,MinJ :j0_ ,:,iBlock) = 0.0
       if(DiLevel_EB(4,iBlock)==Unset_) Phi_GB(:,nJp1_:MaxJ,:,iBlock) = 0.0

       if(nK>1)then
          if(DiLevel_EB(5,iBlock)==Unset_) Phi_GB(:,:,MinK :k0_ ,iBlock) = 0.0
          if(DiLevel_EB(6,iBlock)==Unset_) Phi_GB(:,:,nKp1_:MaxK,iBlock) = 0.0
       end if

       if(IsBody_B(iBlock))then
          where(.not.Used_GB(:,:,:,iBlock)) Phi_GB(:,:,:,iBlock)=0.0
       end if

       ! if(UseConstrainB)then
       !   ! Correct ghost cells at Res. changes to get consistent gradphi
       !   if(DiLevel_EB(1,iBlock) ==-1) &
       !        Phi_GB(0   ,1:nJ,1:nK,iBlock)=2*Phi_GB(0   ,1:nJ,1:nK,iBlock) &
       !        -Phi_GB(1   ,1:nJ,1:nK,iBlock)
       !   if(DiLevel_EB(2,iBlock) ==-1) &
       !        Phi_GB(nI+1,1:nJ,1:nK,iBlock)=2*Phi_GB(nI+1,1:nJ,1:nK,iBlock) &
       !        -Phi_GB(nI  ,1:nJ,1:nK,iBlock)
       !   if(DiLevel_EB(3,iBlock)==-1) &
       !        Phi_GB(1:nI,0   ,1:nK,iBlock)=2*Phi_GB(1:nI,0   ,1:nK,iBlock) &
       !        -Phi_GB(1:nI,1   ,1:nK,iBlock)
       !   if(DiLevel_EB(4,iBlock)==-1) &
       !        Phi_GB(1:nI,nJ+1,1:nK,iBlock)=2*Phi_GB(1:nI,nJ+1,1:nK,iBlock) &
       !        -Phi_GB(1:nI,nJ  ,1:nK,iBlock)
       !   if(DiLevel_EB(5,iBlock)  ==-1) &
       !        Phi_GB(1:nI,1:nJ,0   ,iBlock)=2*Phi_GB(1:nI,1:nJ,0   ,iBlock) &
       !        -Phi_GB(1:nI,1:nJ,1   ,iBlock)
       !   if(DiLevel_EB(6,iBlock)  ==-1) &
       !        Phi_GB(1:nI,1:nJ,nK+1,iBlock)=2*Phi_GB(1:nI,1:nJ,nK+1,iBlock) &
       !        -Phi_GB(1:nI,1:nJ,nK  ,iBlock)
       ! end if

    end do

    call test_stop(NameSub, DoTest)
  end subroutine proj_boundphi
  !============================================================================

  subroutine proj_correction(Phi_GB)

    ! Correct B field by gradient of Phi_GB

    use ModMain, ONLY: nI,nJ,nK
    use ModVarIndexes, ONLY: Bx_,Bz_
    use ModAdvance, ONLY: State_VGB
    use ModMain, ONLY: UseConstrainB
    use ModConstrainDivB, ONLY: BxFace_GB, ByFace_GB, BzFace_GB, &
         bface_to_bcenter, bound_bface
    use BATL_lib, ONLY: &
         CellSize_DB, x_, y_, z_, nI, nJ, nK, nG, nBlock, Unused_B, Used_GB
    use ModCellGradient, ONLY: calc_gradient

    ! Arguments
    real, intent(inout) :: Phi_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)

    ! Local variables
    integer :: iBlock, i, j, k
    real    :: DxInv, DyInv, DzInv
    real    :: dB_D(3), Ratio, dBRatioMax

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'proj_correction'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(UseConstrainB)then

       if(DoTest)write(*,*)'proj_correction old Bzface=',&
            BzFace_GB(iTest,jTest,kTest,iBlockTest), &
            BzFace_GB(iTest,jTest,kTest+1,iBlockTest), &
            Used_GB(iTest,jTest,kTest-1,iBlockTest), &
            Used_GB(iTest,jTest,kTest+1,iBlockTest)

       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE

          DxInv = 1/CellSize_DB(x_,iBlock)
          do k = 1, nK; do j = 1, nJ; do i = 1, nI+1
             BxFace_GB(i,j,k,iBlock) = BxFace_GB(i,j,k,iBlock) &
                  - DxInv*(Phi_GB(i,j,k,iBlock) - Phi_GB(i-1,j,k,iBlock))
          end do; end do; end do

          DyInv = 1/CellSize_DB(y_,iBlock)
          do k = 1, nK; do j = 1, nJ+1; do i = 1, nI
             ByFace_GB(i,j,k,iBlock) = ByFace_GB(i,j,k,iBlock) &
                  - DyInv*(Phi_GB(i,j+1,k,iBlock) - Phi_GB(i,j-1,k,iBlock))
          end do; end do; end do

          if(nK > 1)then
             DzInv = 1/CellSize_DB(z_,iBlock)
             do k = 1, nK+1; do j = 1, nJ; do i = 1, nI
                BzFace_GB(i,j,k,iBlock) = BzFace_GB(i,j,k,iBlock) &
                     - DzInv*(Phi_GB(i,j,k+1,iBlock) - Phi_GB(i,j,k-1,iBlock))
             end do; end do; end do
          end if

          if(DoTest.and.iBlockTest==iBlock) &
               write(*,*)'before bound_bface Bzface=',&
               BzFace_GB(iTest,jTest,kTest,iBlockTest), &
               BzFace_GB(iTest,jTest,kTest+1,iBlockTest)

          ! Make sure that the correction is NOT applied to the body boundaries
          call bound_bface(iBlock)
          ! Recalculate the cell centered B
          call bface_to_bcenter(iBlock)

       end do

       if(DoTest)write(*,*)'proj_correction new Bzface=',&
            BzFace_GB(iTest,jTest,kTest,iBlockTest), &
            BzFace_GB(iTest,jTest,kTest+1,iBlockTest)

    else
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE

          call calc_gradient(iBlock, Phi_GB(:,:,:,iBlock), &
               nG, GradPhi_DGB(:,:,:,:,iBlock), UseBodyCellIn=.true.)

          do k=1,nK; do j=1,nJ; do i=1,nI
             if(.not.Used_GB(i,j,k,iBlock)) CYCLE

             dB_D = GradPhi_DGB(:,i,j,k,iBlock)

             if(.false.) then
                ! In a simulation with large magnetic field gradient, the
                ! correction dB may be too large for the small B side.
                ! The following lines limit the correction ratio.

                Ratio = sqrt(sum(dB_D**2)/ &
                     (sum(State_VGB(Bx_:Bz_,i,j,k,iBlock)**2)+1e-99))

                dBRatioMax = 0.1
                if(Ratio > dBRatioMax) then
                   dB_D = dB_D/Ratio*dBRatioMax
                endif
             endif

             State_VGB(Bx_:Bz_,i,j,k,iBlock) = &
                  State_VGB(Bx_:Bz_,i,j,k,iBlock) - dB_D
          end do; end do; end do
       end do
    end if

    call test_stop(NameSub, DoTest)
  end subroutine proj_correction
  !============================================================================

  ! The CG-algorithm is implemented as shown on page 12 of the thesis
  ! "Preconditioning for sparse matrices with applications."
  ! Auke van der Ploeg, University of Groningen, 1994.
  ! Rewritten to F90 by G. Toth based on the F77 subroutine in src/conjgrad.f
  ! Rewritten by G. Toth for BATS-R-US code to be used with MPI, 2000.
  !
  ! This subroutine determines the solution of A.X=Rhs_GB, where
  ! the matrix-vector multiplication with A is performed by
  ! the subroutine 'proj_matvec'.
  !
  ! !! If the matrix is not symmetric positive definite, CG is likely to fail.
  !
  subroutine proj_cg(Rhs_GB,x_GB,nIter,Tol,TypeStop,iInfo)
    use ModMain, ONLY:MaxBlock
    use ModAdvance, ONLY: Tmp1_GB, Tmp2_GB

    ! Arguments

    real, intent(inout) :: Rhs_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    !        on input:  right-hand side vector.
    !        on output: residual vector.

    real, intent(out):: x_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    !        on output: solution vector.

    integer, intent(inout) :: nIter
    !       on input:  maximum number of iterations to be performed.
    !       on output: actual  number of iterations done.

    real, intent(inout) :: Tol
    !       on input:  required (relative) 2-norm or maximum norm of residual
    !       on output: achieved (relative) 2-norm or maximum norm of residual

    character (len=3), intent(in) :: TypeStop
    !       Determine stopping criterion (||.|| denotes the 2-norm):
    !       TypeStop='rel'    -- relative stop crit.: ||Res|| <= Tol*||Res0||
    !       TypeStop='abs'    -- absolute stop crit.: ||Res|| <= Tol
    !       TypeStop='max'    -- maximum  stop crit.: max(abs(Res)) <= Tol

    integer, intent(out)   :: iInfo
    !       Gives reason for returning:
    !     abs(iInfo)= 0 - solution found satisfying given Tolerance.
    !                 1 - aborted due to division by very small value.
    !                 2 - no convergence within maximum number of iterations.
    !                 3 - initial guess satisfies the stopping criterion.
    !    sign(iInfo)= + - residual decreased
    !                 - - residual did not reduce

    ! Local variables

    integer ::iIter,nMatvec
    real :: Rho,RhoNew,Res,Res0,Bet,Alf,AssumedZero

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'proj_cg'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(DoTest)write(*,*)'Proj_CG: Tol, nIter=',Tol,nIter

    ! Debug
    ! call show_BLK('Rhs_GB',Rhs_GB)

    AssumedZero=1.E-16; iIter=0; nMatvec=0
    call set_block_scalar(x_GB,0.0)

    if (TypeStop/='rel'.and.TypeStop/='abs'.and.TypeStop/='max') &
         call stop_mpi('Error in CG: TypeStop='//TypeStop// &
         ' should be one of rel/abs/max.')

    if(DoTest) write(*,*)'n gives the number of CG-iterations.'

    ! Rho=||Rhs_GB||
    Rho=sqrt(dot_product_block(Rhs_GB,Rhs_GB))

    Res0=Rho
    if (TypeStop=='max') Res0 = maxval_grid(Rhs_GB, UseAbs=.true.)

    ! DEBUG
    ! write(*,*)'Res0:',Res0,' Rhs_GB:',Rhs_GB

    Res=Res0

    AssumedZero = AssumedZero*Res0

    if (DoTest) then
       if (TypeStop=='max') then
          write(*,*)'n:',iIter,' Maximum norm initial residual:',Res0
       else
          write(*,*)'n:',iIter,' 2-norm initial residual:',Res0
       end IF
    end if

    if (Res0<DivBTiny .or. (TypeStop/='rel'.and.Res0<=Tol)) then
       iInfo = 3
    else
       ! Initialize Rho and Tmp1_GB=Z
       Rho=Rho*Rho
       call set_block_array(Tmp1_GB,Rhs_GB)

       ! Do iteration
       do
          ! AZ=A.Z
          call proj_matvec(Tmp1_GB,Tmp2_GB)
          nMatvec=nMatvec+1

          ! Debug
          ! write(*,*)'Z =Tmp1_GB=',Tmp1_GB(:,:,:,1:2)
          ! write(*,*)'AZ=Tmp2_GB=',Tmp2_GB(:,:,:,1:2)
          ! call stop_mpi('Debug')

          ! Alf=A.AZ
          Alf=dot_product_block(Tmp1_GB,Tmp2_GB)

          if(DoTest)write(*,*)'Alf=',Alf
          ! Debug
          ! call show_BLK('Z',Tmp1_GB)
          ! call show_BLK('AZ',Tmp2_GB)
          ! call stop_mpi('Debug')
          if (abs(Alf)<=AssumedZero**2) then
             iInfo = 1
             EXIT
          end if
          Alf=Rho/Alf
          if(DoTest)write(*,*)'Alf=',Alf

          call add_times_block(x_GB,Alf,Tmp1_GB)
          call add_times_block(Rhs_GB,-Alf,Tmp2_GB)

          ! RhoNew=||Rhs_GB||
          RhoNew=sqrt(dot_product_block(Rhs_GB,Rhs_GB))
          if(DoTest)write(*,*)'RhoNew=',RhoNew

          select case(TypeStop)
          case('max')
             Res = maxval_grid(Rhs_GB, UseAbs=.true.)

          case('rel')
             Res = RhoNew/Res0

          case('abs')
             Res=RhoNew
          end select
          RhoNew=RhoNew*RhoNew

          iIter=iIter+1
          if (DoTest) &
               write(*,*)'n:',iIter,' ',TypeStop,'. norm of residual:',Res

          if (Res<=Tol) then
             iInfo = 0
             EXIT
          end if
          if (iIter>=nIter) then
             iInfo = 2
             EXIT
          end if
          if (Rho<=AssumedZero**2) then
             iInfo = 1
             EXIT
          end if

          Bet=RhoNew/Rho
          if(DoTest)write(*,*)'Bet=',Bet

          call add2_times_block(Tmp1_GB,Rhs_GB,Bet,Tmp1_GB)

          if (DoTest) write(*,*)'Alf,Bet,Rho,RhoNew:',Alf,Bet,Rho,RhoNew
          Rho=RhoNew
       end do
    end if

    ! return number of iterations and achieved residual
    nIter=iIter
    Tol =Res
    if((TypeStop == 'rel' .and. Res > 1.0) &
         .or. (TypeStop /= 'rel' .and. Res > Res0)) iInfo = -iInfo

    ! report results
    if(DoTest)then
       write(*,*)'Total Number of CG-iterations:',iIter
       write(*,*)'Number of matrix-vector mult.:',nMatvec
       select case(abs(iInfo))
       case(0)
          write(*,*)'Successful iteration, norm of Res. is:',Tol
       case(1)
          write(*,*)'Iteration aborted due to division by a'
          write(*,*)'very small value.'
       case(2)
          write(*,*)'Stopping crit. not fulfilled within given'
          write(*,*)'maximum number of iterations.'
       case(3)
          write(*,*)'Initial guess for the solution satisfies'
          write(*,*)'given stopping criterion.'
       case default
          write(*,*)'Impossible value for iInfo:',iInfo
       end select
       if(iInfo<0)write(*,*)'The residual did not reduce'
    endif

    call test_stop(NameSub, DoTest)
  end subroutine proj_cg
  !============================================================================

  ! Simple BiCGstab(\ell=1) iterative method
  ! Modified by G.Toth from the \ell<=2 version written
  ! by M.A.Botchev, Jan.'98.
  ! Further modified for the BATS-R-US code (2000) by G. Toth
  !
  ! This is the "vanilla" version of BiCGstab(\ell) as described
  ! in PhD thesis of D.R.Fokkema, Chapter 3.  It includes two enhancements
  ! to BiCGstab(\ell) proposed by G.Sleijpen and H.van der Vorst in
  ! 1) G.Sleijpen and H.van der Vorst "Maintaining convergence
  !    properties of BiCGstab methods in finite precision arithmetic",
  !    Numerical Algorithms, 10, 1995, pp.203-223
  ! 2) G.Sleijpen and H.van der Vorst "Reliable updated residuals in
  !    hybrid BiCG methods", Computing, 56, 1996, 141-163
  !
  ! {{ This code is based on:
  ! subroutine bistbl v1.0 1995
  !
  ! Copyright (c) 1995 by D.R. Fokkema.
  ! Permission to copy all or part of this work is granted,
  ! provided that the copies are not made or distributed
  ! for resale, and that the copyright notice and this
  ! notice are retained.  }}
  !
  ! This subroutine determines the solution of A.x_GB=Rhs_GB, where
  ! the matrix-vector multiplication with A is performed by
  ! the subroutine 'proj_matvec'. For symmetric matrix use the more efficient
  ! proj_cg algorithm!
  !
  subroutine proj_bicgstab(Rhs_GB,x_GB,nIter,Tol,TypeStop,iInfo)
    use ModMain, ONLY:MaxBlock
    use ModAdvance, ONLY: Tmp1_GB,Tmp2_GB

    ! Arguments

    real, intent(inout) :: Rhs_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    !        on input:  right-hand side vector.
    !        on output: residual vector.

    real, intent(out):: x_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    !       on output: solution vector.

    integer, intent(inout) :: nIter
    !       on input:  maximum number of iterations to be performed.
    !       on output: actual  number of iterations done.

    real, intent(inout) :: Tol
    !       on input:  required (relative) 2-norm or maximum norm of residual
    !       on output: achieved (relative) 2-norm or maximum norm of residual

    character (len=3), intent(in) :: TypeStop
    !      Determine stopping criterion (||.|| denotes the 2-norm):
    !      TypeStop='rel'    -- relative stop crit.: ||Res|| <= Tol*||Res0||
    !      TypeStop='abs'    -- absolute stop crit.: ||Res|| <= Tol
    !      TypeStop='max'    -- maximum  stop crit.: max(abs(Res)) <= Tol

    ! NOTE for TypeStop='rel' and 'abs':
    !            To save computational work, the value of
    !            residual norm used to check the convergence inside the main
    !            iterative loop is computed from projections,
    !            i.e. it can be smaller than the true residual norm
    !            (it may happen when e.g. the 'matrix-free' approach is used).
    !            Thus, it is possible that the true residual does NOT satisfy
    !            the stopping criterion ('rel' or 'abs').
    !            The true residual norm (or residual reduction) is reported on
    !            output in parameter Tol (this can be changed to save 1 MATVEC)
    !            (see comments at the end of the subroutine)

    integer, intent(out)   :: iInfo
    !       Gives reason for returning:
    !     abs(iInfo)=  0 - solution found satisfying given Tolerance.
    !                  1 - aborted due to division by very small value.
    !                  2 - no convergence within maximum number of iterations.
    !                  3 - initial guess satisfies the stopping criterion.
    !    sign(iInfo)=  + - residual decreased
    !                  - - residual did not reduce

    ! Local parameters

    integer, parameter :: qz_=1,zz_=3,y0_=5,yl_=6,qy_=7

    ! Local variables (2 vectors are needed in addition to
    ! Tmp1_GB=r and Tmp2_GB=u:
    real, dimension(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock):: r1_GB, u1_GB

    real :: Work_II(2,7)

    logical:: DoGoOn
    integer:: nMv
    real::    Alpha, Beta, Omega, Rho0, Rho1, Sigma
    real::    VarRho, HatGamma
    real::    AssumedZero, rNorm0, rNorm, rNormMax0, rNormMax
    real::    Kappa0, KappaL

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'proj_bicgstab'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(DoTest)write(*,*)'Proj_BiCGSTAB Tol,nIter:',Tol,nIter

    iInfo = 0

    if (Tol<=0.0) call stop_mpi('Error in Proj_BiCGSTAB: Tolerance < 0')
    if (nIter<=1)  call stop_mpi('Error in Proj_BiCGSTAB: maxmatvec < 2')

    !
    !     --- Initialize first residual
    !
    AssumedZero = 1.e-16
    call set_block_array(Tmp1_GB,Rhs_GB)
    call set_block_scalar(x_GB,0.0)

    nMv = 0
    !
    !     --- Initialize iteration loop
    !

    rNorm0 = sqrt( dot_product_block(Tmp1_GB,Tmp1_GB))

    rNorm = rNorm0
    if(DoTest) print *,'initial rNorm:',rNorm

    Alpha = 0.0
    Omega = 1.0
    Sigma = 1.0
    Rho0 =  1.0
    !
    !     --- Iterate
    !
    select case(TypeStop)
    case('rel')
       DoGoOn = rNorm > Tol*rNorm0 .and. nMv < nIter
       AssumedZero = AssumedZero*rNorm0
       rNormMax = 0
       rNormMax0 = 0

    case('abs')
       DoGoOn = rNorm > Tol .and. nMv < nIter
       AssumedZero = AssumedZero*rNorm0
       rNormMax = 0
       rNormMax0 = 0

    case('max')
       rNormMax0 = maxval_grid(Tmp1_GB, UseAbs=.true.)
       rNormMax  = rNormMax0
       if(DoTest) print *,'initial rNormMax:',rNormMax
       DoGoOn = rNormMax>Tol .and. nMv < nIter
       AssumedZero = AssumedZero*rNormMax
    case default
       call stop_mpi('Error in Proj_BiCGSTAB: unknown TypeStop value')
    end select

    if (.not.DoGoOn) then
       if(DoTest) print *,'Proj_BiCGSTAB: nothing to do. iInfo = ',iInfo
       nIter = nMv
       iInfo = 3
       RETURN
    end if

    do while (DoGoOn)
       !
       !     =====================
       !     --- The BiCG part ---
       !     =====================
       !
       Rho0 = -Omega*Rho0

       Rho1 = dot_product_block(Rhs_GB,Tmp1_GB)

       if (abs(Rho0)<AssumedZero**2) then
          iInfo = 1
          RETURN
       endif
       Beta = Alpha*(Rho1/Rho0)
       Rho0 = Rho1
       call add2_times_block(Tmp2_GB,Tmp1_GB,-Beta,Tmp2_GB)

       call proj_matvec(Tmp2_GB,u1_GB)
       nMv = nMv + 1

       ! DEBUG
       ! write(*,*)'u =',Tmp2_GB(1:nI,1:nJ,1:nK,iBlockTest)
       ! write(*,*)'u1=',u1_GB(1:nI,1:nJ,1:nK,iBlockTest)

       Sigma=dot_product_block(Rhs_GB,u1_GB)

       if (abs(Sigma)<AssumedZero**2) then
          iInfo = 1
          RETURN
       endif

       Alpha = Rho1/Sigma
       call add_times_block(x_GB,Alpha,Tmp2_GB)

       call add_times_block(Tmp1_GB,-Alpha,u1_GB)

       call proj_matvec(Tmp1_GB,r1_GB)
       nMv = nMv + 1

       rNorm = sqrt( dot_product_block(Tmp1_GB,Tmp1_GB) )

       ! DEBUG
       ! write(*,*)'Rho0, Rho1, Beta, Sigma, Alpha, rNorm:',&
       !     Rho0, Rho1, Beta, Sigma, Alpha, rNorm

       !
       !  ==================================
       !  --- The convex polynomial part ---
       !  ==================================
       !
       !    --- Z = R'R a 2 by 2 matrix
       ! i=1,j=0
       Work_II(1,1) = dot_product_block(Tmp1_GB,Tmp1_GB)

       ! i=1,j=1
       Work_II(2,1) = dot_product_block(r1_GB,Tmp1_GB)
       Work_II(1,2) = Work_II(2,1)

       ! i=2,j=1
       Work_II(2,2) = dot_product_block(r1_GB,r1_GB)

       !
       !   --- tilde r0 and tilde rl (small vectors)
       !
       Work_II(1:2,zz_:zz_+1)   = Work_II(1:2,qz_:qz_+1)
       Work_II(1,y0_) = -1.0
       Work_II(2,y0_) = 0.0

       Work_II(1,yl_) = 0.0
       Work_II(2,yl_) = -1.0
       !
       !   --- Convex combination
       !
       Work_II(1:2,qy_) = Work_II(1,yl_)*Work_II(1:2,qz_) + &
            Work_II(2,yl_)*Work_II(1:2,qz_+1)

       KappaL = sqrt( sum( Work_II(1:2,yl_)*Work_II(1:2,qy_) ) )

       Work_II(1:2,qy_) = Work_II(1,y0_)*Work_II(1:2,qz_) + &
            Work_II(2,y0_)*Work_II(1:2,qz_+1)

       Kappa0 = sqrt( sum( Work_II(1:2,y0_)*Work_II(1:2,qy_) ) )

       VarRho = sum( Work_II(1:2,yl_)*Work_II(1:2,qy_) )
       VarRho = VarRho / (Kappa0*KappaL)

       HatGamma = sign(1.0,VarRho)*max(abs(VarRho),0.7) * (Kappa0/KappaL)

       Work_II(1:2,y0_) = -HatGamma*Work_II(1:2,yl_) + Work_II(1:2,y0_)

       !
       !    --- Update
       !
       Omega = Work_II(2,y0_)

       call add_times_block(Tmp2_GB,-Omega,u1_GB)

       call add_times_block(x_GB,Omega,Tmp1_GB)

       call add_times_block(Tmp1_GB,-Omega,r1_GB)

       Work_II(1:2,qy_) = Work_II(1,y0_)*Work_II(1:2,qz_) + &
            Work_II(2,y0_)*Work_II(1:2,qz_+1)

       rNorm = sqrt( sum( Work_II(1:2,y0_)*Work_II(1:2,qy_) ) )

       select case(TypeStop)
       case('rel')
          DoGoOn = rNorm>Tol*rNorm0 .and. nMv < nIter
          if(DoTest) print *, nMv,' matvecs, ', ' ||rn||/||r0|| =',rNorm/rNorm0

       case('abs')
          DoGoOn = rNorm>Tol       .and. nMv < nIter
          if(DoTest) print *, nMv,' matvecs, ||rn|| =',rNorm

       case('max')
          rNormMax = maxval_grid(Tmp1_GB, UseAbs=.true.)
          DoGoOn = rNormMax > Tol  .and. nMv < nIter
          if(DoTest) print *, nMv,' matvecs, max(rn) =',rNormMax
       end select

    end do
    !
    !     =========================
    !     --- End of iterations ---
    !     =========================

    select case(TypeStop)
    case('rel')
       if (rNorm>Tol*rNorm0) iInfo = 2
       Tol = rNorm/rNorm0

    case('abs')
       if (rNorm>Tol) iInfo = 2
       Tol = rNorm

    case('max')
       if (rNormMax>Tol) iInfo = 2
       Tol = rNormMax
    end select

    if((TypeStop/='max'.and.rNorm>rNorm0).or.(TypeStop=='max'.and.rNormMax&
         >rNormMax0)) iInfo=-iInfo

    nIter = nMv

    call test_stop(NameSub, DoTest)
  end subroutine proj_bicgstab
  !============================================================================
  subroutine set_block_scalar(a_GB, b)
    use BATL_lib, ONLY: Used_GB

    ! Set a_GB = b for all used blocks, where b is a scalar

    ! Arguments

    real, intent(out):: a_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    real, intent(in) :: b

    ! Local variables:
    integer:: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_block_scalar'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    do iBlock=1,nBlock
       if(.not.Unused_B(iBlock)) then
          if(IsNoBody_B(iBlock))then
             a_GB(1:nI,1:nJ,1:nK,iBlock)=b
          else
             where(Used_GB(1:nI,1:nJ,1:nK,iBlock)) &
                  a_GB(1:nI,1:nJ,1:nK,iBlock)=b
          end if
       end if
    end do

    call test_stop(NameSub, DoTest)
  end subroutine set_block_scalar
  !============================================================================
  subroutine set_block_array(a_GB, b_GB)
    use BATL_lib, ONLY: Used_GB

    ! Do a_GB=b_GB for all used blocks

    ! Arguments
    real, intent(out) :: a_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    real, intent(in)  :: b_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)

    ! Local variables:
    integer:: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_block_array'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    do iBlock=1,nBlock
       if(.not.Unused_B(iBlock)) then
          if(IsNoBody_B(iBlock))then
             a_GB(1:nI,1:nJ,1:nK,iBlock)= b_GB(1:nI,1:nJ,1:nK,iBlock)
          else
             where(Used_GB(1:nI,1:nJ,1:nK,iBlock)) &
                  a_GB(1:nI,1:nJ,1:nK,iBlock)= b_GB(1:nI,1:nJ,1:nK,iBlock)
          end if

       end if
    end do

    call test_stop(NameSub, DoTest)
  end subroutine set_block_array
  !============================================================================
  subroutine add_block(a_GB, b_GB)
    use BATL_lib, ONLY: Used_GB

    ! Do a_GB=a_GB+b_GB for all used blocks

    ! Arguments

    real, intent(inout) :: a_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    real, intent(in)    :: b_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)

    ! Local variables:
    integer:: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'add_block'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    do iBlock=1,nBlock
       if(.not.Unused_B(iBlock)) then
          if(IsNoBody_B(iBlock))then
             a_GB(1:nI,1:nJ,1:nK,iBlock)= &
                  a_GB(1:nI,1:nJ,1:nK,iBlock)+b_GB(1:nI,1:nJ,1:nK,iBlock)
          else
             where(Used_GB(1:nI,1:nJ,1:nK,iBlock)) &
                  a_GB(1:nI,1:nJ,1:nK,iBlock)= &
                  a_GB(1:nI,1:nJ,1:nK,iBlock)+b_GB(1:nI,1:nJ,1:nK,iBlock)
          end if
       end if
    end do

    call test_stop(NameSub, DoTest)
  end subroutine add_block
  !============================================================================
  subroutine sub_block(a_GB, b_GB)
    use BATL_lib, ONLY: Used_GB

    ! Do a_GB=a_GB-b_GB for all used blocks

    ! Arguments

    real, intent(inout):: a_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    real, intent(in)   :: b_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)

    ! Local variables:
    integer:: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'sub_block'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    do iBlock=1,nBlock
       if(.not.Unused_B(iBlock)) then
          if(IsNoBody_B(iBlock))then
             a_GB(1:nI,1:nJ,1:nK,iBlock)= &
                  a_GB(1:nI,1:nJ,1:nK,iBlock)-b_GB(1:nI,1:nJ,1:nK,iBlock)
          else
             where(Used_GB(1:nI,1:nJ,1:nK,iBlock)) &
                  a_GB(1:nI,1:nJ,1:nK,iBlock)= &
                  a_GB(1:nI,1:nJ,1:nK,iBlock)-b_GB(1:nI,1:nJ,1:nK,iBlock)
          end if
       end if
    end do

    call test_stop(NameSub, DoTest)
  end subroutine sub_block
  !============================================================================
  subroutine add2_block(a_GB, b_GB, c_GB)
    use BATL_lib, ONLY: Used_GB

    ! Do a_GB=b_GB+c_GB for all used blocks

    ! Arguments

    real, intent(out):: a_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    real, intent(in) :: b_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    real, intent(in) :: c_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)

    ! Local variables:
    integer:: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'add2_block'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    do iBlock=1,nBlock
       if(.not.Unused_B(iBlock)) then
          if(IsNoBody_B(iBlock))then
             a_GB(1:nI,1:nJ,1:nK,iBlock)= &
                  b_GB(1:nI,1:nJ,1:nK,iBlock)+c_GB(1:nI,1:nJ,1:nK,iBlock)
          else
             where(Used_GB(1:nI,1:nJ,1:nK,iBlock)) &
                  a_GB(1:nI,1:nJ,1:nK,iBlock)= &
                  b_GB(1:nI,1:nJ,1:nK,iBlock)+c_GB(1:nI,1:nJ,1:nK,iBlock)
          end if
       end if
    end do

    call test_stop(NameSub, DoTest)
  end subroutine add2_block
  !============================================================================
  subroutine add_times_block(a_GB, b, c_GB)
    use BATL_lib, ONLY: Used_GB

    ! a_GB = a_GB + b_GB*c_GB for all used blocks, where b_GB is a scalar

    ! Arguments
    real, intent(inout):: a_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    real, intent(in)   :: c_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    real, intent(in)   :: b

    ! Local variables:
    integer:: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'add_times_block'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    do iBlock=1,nBlock
       if(.not.Unused_B(iBlock)) then
          if(IsNoBody_B(iBlock))then
             a_GB(1:nI,1:nJ,1:nK,iBlock)= &
                  a_GB(1:nI,1:nJ,1:nK,iBlock)+b*c_GB(1:nI,1:nJ,1:nK,iBlock)
          else
             where(Used_GB(1:nI,1:nJ,1:nK,iBlock)) &
                  a_GB(1:nI,1:nJ,1:nK,iBlock)= &
                  a_GB(1:nI,1:nJ,1:nK,iBlock)+b*c_GB(1:nI,1:nJ,1:nK,iBlock)
          end if
       end if
    end do

    call test_stop(NameSub, DoTest)
  end subroutine add_times_block
  !============================================================================
  subroutine add2_times_block(a_GB, b_GB, c, d_GB)
    use BATL_lib, ONLY: Used_GB

    ! a_GB = b_GB + c*d_GB for all used blocks, where c is a scalar

    ! Arguments

    real, intent(inout):: a_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    real, intent(in)   :: b_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    real, intent(inout):: d_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    real, intent(in)   :: c

    ! Local variables:
    integer:: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'add2_times_block'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    do iBlock=1,nBlock
       if(.not.Unused_B(iBlock)) then
          if(IsNoBody_B(iBlock))then
             a_GB(1:nI,1:nJ,1:nK,iBlock)= &
                  b_GB(1:nI,1:nJ,1:nK,iBlock) + c*d_GB(1:nI,1:nJ,1:nK,iBlock)
          else
             where(Used_GB(1:nI,1:nJ,1:nK,iBlock)) &
                  a_GB(1:nI,1:nJ,1:nK,iBlock)= &
                  b_GB(1:nI,1:nJ,1:nK,iBlock) + c*d_GB(1:nI,1:nJ,1:nK,iBlock)
          end if
       end if
    end do

    call test_stop(NameSub, DoTest)
  end subroutine add2_times_block
  !============================================================================
  real function dot_product_block(a_GB, b_GB)
    use BATL_lib, ONLY: Used_GB

    ! Return a_GB.b_GB = sum(a_GB*b_GB) for all used blocks

    ! Arguments
    real, intent(in):: a_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    real, intent(in):: b_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)

    ! Local variables:
    real    :: BlockProduct
    integer :: iBlock, iError

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'dot_product_block'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    BlockProduct=0.0

    do iBlock=1,nBlock
       if(.not.Unused_B(iBlock)) then
          if(IsNoBody_B(iBlock)) then
             BlockProduct=BlockProduct + &
                  sum(a_GB(1:nI,1:nJ,1:nK,iBlock)*b_GB(1:nI,1:nJ,1:nK,iBlock))
          else
             BlockProduct=BlockProduct + &
                  sum(a_GB(1:nI,1:nJ,1:nK,iBlock)*b_GB(1:nI,1:nJ,1:nK,iBlock),&
                  MASK=Used_GB(1:nI,1:nJ,1:nK,iBlock))
          end if
       end if
    end do

    if(nProc>1) call MPI_allreduce(MPI_IN_PLACE, BlockProduct, &
         1,  MPI_REAL, MPI_SUM, iComm, iError)

    dot_product_block = BlockProduct
    if(DoTest)write(*,*) NameSub,': me, product=', iProc, BlockProduct

    call test_stop(NameSub, DoTest)
  end function dot_product_block
  !============================================================================
  real function sum_block(nPe, a_GB)
    use BATL_lib, ONLY: Used_GB

    ! Return sum(a_GB) for all used blocks and true cells
    ! Do for each processor separately if nPe=1, otherwise add them all

    ! Arguments

    integer, intent(in) :: nPe
    real, dimension(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock), intent(in) :: a_GB

    ! Local variables:
    real    :: BlockSum
    integer :: iBlock, iError

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'sum_block'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    BlockSum=0.0

    do iBlock=1,nBlock
       if(.not.Unused_B(iBlock)) then
          if(IsNoBody_B(iBlock)) then
             BlockSum=BlockSum + sum(a_GB(1:nI,1:nJ,1:nK,iBlock))
          else
             BlockSum=BlockSum + sum(a_GB(1:nI,1:nJ,1:nK,iBlock), &
                  MASK=Used_GB(1:nI,1:nJ,1:nK,iBlock))
          end if
       end if
    end do

    if(nPe > 1) call MPI_allreduce(MPI_IN_PLACE, BlockSum, &
         1,  MPI_REAL, MPI_SUM, iComm, iError)
    sum_block=BlockSum
    if(DoTest)write(*,*) NameSub,': me,BlockSum=',iProc,BlockSum

    call test_stop(NameSub, DoTest)
  end function sum_block
  !============================================================================
end module ModProjectDivB
!==============================================================================
