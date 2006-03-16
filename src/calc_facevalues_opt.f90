!^CFG COPYRIGHT UM
module ModLimiter
  use ModMain, ONLY: VarTest, BetaLimiter
  use ModSize
  use ModVarIndexes,ONLY:nVar
  use ModNumConst

  implicit none

  real, parameter :: cFifth = 0.2, cTwoSeventh = 2.0/7.0

  ! Maximum length of the stencil in 1D
  integer,parameter:: MaxIJK= max(nI,nJ,nK)

  ! index ranges for optimized slope limiter calculations
  integer, parameter:: Lo2_=nVar+1, Hi2_=nVar+nVar, Lo3_=Hi2_+1, Hi3_=nVar+Hi2_

  ! local constant
  real, parameter   :: cTwoThird = cTwo*cThird

  ! Globally set variables
  logical:: UseTVDAtResChange = .false.
  logical:: DoLimitMomentum   = .false.  !^CFG IF BORISCORR

  ! Locally set variables
  real   :: dVarLimR_VI(1:nVar,0:MaxIJK+1) ! limited slope for right state
  real   :: dVarLimL_VI(1:nVar,0:MaxIJK+1) ! limited slope for left state
  real   :: Primitive_VI(1:nVar,-1:MaxIJK+2)
  logical:: IsTrueCell_I(-1:MaxIJK+2)


  !logical :: DoTestLimiter = .false.

contains
  !===========================================================================
  subroutine tvd_reschange_body(& 
       Coarse2_V         ,& !State in the coarser ghostcell, 2nd layer
       Coarse1_V         ,& !State in the coarser ghostcell, 1st layer
       Fine1_VII         ,& !States in 4 fine physical cells,1st layer
       Fine2_VII         ,& !States in 4 fine physical cells,2nd layer
       CoarseToFineF_VII ,& !Values at subfaces, in the coarse ghostcell
       FineToCoarseF_VII ,& !Facevalues in the physical cell,looking at the coarser cell 
       FineF_VII         ,& !Facevalues in the physical cell,looking at the physical cell  
       IsTrueCoarse2     ,& !True if the coarser ghostcell of the 2nd layer is true
       IsTrueCoarse1     ,& !True if the coarser ghostcell of the 1st layer is true
       IsTrueFine1       ,& !True if all physical cells of the 1st layer are true
       IsTrusFine2_II)      !True for true physical cell of the 2nd layer
    !_____________!_____________!_______!_______!_
    !             !         CToF!FToC FF!
    ! C2_V        ! C1_V       _!_F1_V__!__F2_V_!_
    !             !         CToF!FToC FF!      
    !_____________!_____________!_F1_V__!__F2_V_!_
    !             !             !       !       !
    real,dimension(nVar),intent(in):: Coarse2_V,Coarse1_V
    real,dimension(nVar,2,2),intent(in):: Fine1_VII,Fine2_VII
    real,dimension(nVar,2,2),intent(out)::&
         CoarseToFineF_VII ,FineToCoarseF_VII , FineF_VII
    logical,intent(in)::IsTrueCoarse2,IsTrueCoarse1,IsTrueFine1
    logical,dimension(2,2),intent(in)::IsTrusFine2_II
    integer::iVar,i2,j2
    real,dimension(nVar)::AveragedFine1_V,GradNormal_V,SignGradNormal_V
    real,dimension(nVar)::GradNormalLtd_V  !Ltd stands for "Limited"
    !-------------------------------------------------------------------------
    !Calculate averaged Fine1_VII 
    do iVar=1,nVar
       AveragedFine1_V(iVar)=cQuarter*sum(Fine1_VII(iVar,:,:))
    end do
    GradNormal_V= AveragedFine1_V-Coarse1_V
    !Save gradients squared
    SignGradNormal_V=sign(1.0,GradNormal_V)
    if(IsTrueCoarse2.and.IsTrueCoarse1.and.IsTrueFine1)then
       !Limit gradient in the first coarser cell
       GradNormalLtd_V= SignGradNormal_V*&
            max(cZero,&
            min(cTwoThird*abs(GradNormal_V),&
            cHalf*SignGradNormal_V*(Coarse1_V-Coarse2_V)))

       do j2=1,2;do i2=1,2
          ! ??? This seems to be an unnecessary/first order limiting ???
          ! ??? What about the normal gradient=0 and a constant gradient in
          ! ??? the transverse direction ???
          !Limit transverse gradients, if they are larger than the normal one
          !The unlimited transverse gradients are Fine1_VII-AveragedFine1_V
          CoarseToFineF_VII(:,i2,j2)=Coarse1_V+GradNormalLtd_V+&
               !sign(min(abs(GradNormalLtd_V), &
               !abs(Fine1_VII(:,i2,j2)-AveragedFine1_V)),&
               Fine1_VII(:,i2,j2)-AveragedFine1_V !!!)
       end do;end do
    else
       do j2=1,2;do i2=1,2
          !First order scheme
          CoarseToFineF_VII(:,i2,j2)=Coarse1_V
       end do;end do
    end if
    if(.not.(IsTrueCoarse1.and.IsTrueFine1))then
       do j2=1,2;do i2=1,2
          !First order scheme
          FineToCoarseF_VII(:,i2,j2)=Fine1_VII(:,i2,j2)
          FineF_VII(:,i2,j2)=Fine1_VII(:,i2,j2)
       end do;end do
    else
       do j2=1,2;do i2=1,2
          if(IsTrusFine2_II(i2,j2))then
             !Limit gradient in the first layer of finer cells
             GradNormalLtd_V = SignGradNormal_V*&
                  max(cZero,&
                  min(cThird*abs(GradNormal_V),&
                  SignGradNormal_V*(Fine1_VII(:,i2,j2)-Coarse1_V),&
                  cHalf*SignGradNormal_V*&
                  (Fine2_VII(:,i2,j2)-Fine1_VII(:,i2,j2))))
          else
             !First order scheme
             GradNormalLtd_V=cZero
          end if
          FineToCoarseF_VII(:,i2,j2)=Fine1_VII(:,i2,j2)-GradNormalLtd_V
          FineF_VII(:,i2,j2)=Fine1_VII(:,i2,j2)+GradNormalLtd_V
       end do;end do
    end if
  end subroutine tvd_reschange_body
  !===========================================================================
  subroutine tvd_reschange(&
       Coarse2_V         ,& !State in the coarser ghostcell, 2nd layer
       Coarse1_V         ,& !State in the coarser ghostcell, 1st layer
       Fine1_VII         ,& !States in 4 fine physical cells,1st layer
       Fine2_VII         ,& !States in 4 fine physical cells,2nd layer
       CoarseToFineF_VII ,& !Values at subfaces, in the coarse ghostcell
       FineToCoarseF_VII ,& !Facevalues in the physical cell,
                            !   looking at the coarser cell 
       FineF_VII)           !Facevalues in the physical cell,
                            !   looking at another physical cell

    !_____________!_____________!_______!_______!_
    !             !         CToF!FToC FF!
    ! C2_V        ! C1_V       _!_F1_V__!__F2_V_!_
    !             !         CToF!FToC FF!      
    !_____________!_____________!_F1_V__!__F2_V_!_
    !             !             !       !       !

    real,dimension(nVar),intent(in):: Coarse2_V,Coarse1_V
    real,dimension(nVar,2,2),intent(in):: Fine1_VII,Fine2_VII
    real,dimension(nVar,2,2),intent(inout)::&
         CoarseToFineF_VII ,FineToCoarseF_VII , FineF_VII
    integer::iVar,i2,j2
    real,dimension(nVar):: AveragedFine1_V,AveragedFine2_V
    real,dimension(nVar):: GradNormal_V,SignGradNormal_V
    real,dimension(nVar):: GradNormalLtd_V  !Ltd stands for "Limited"
    !-------------------------------------------------------------------------

    !Calculate averaged Fine1_VII 
    do iVar=1,nVar
       AveragedFine1_V(iVar)=cQuarter*sum(Fine1_VII(iVar,:,:))
       AveragedFine2_V(iVar)=cQuarter*sum(Fine2_VII(iVar,:,:))
    end do
    GradNormal_V= AveragedFine1_V-Coarse1_V

    !if(DoTestLimiter)then
    !   write(*,*)'AveragedFine1=',AveragedFine1_V(VarTest)
    !   write(*,*)'GradNormal_V =',GradNormal_V(VarTest)
    !end if

    !Save gradients squared
    SignGradNormal_V=sign(1.0,GradNormal_V)
    !Limit gradient in the first coarser cell
    GradNormalLtd_V= SignGradNormal_V*max(cZero,min( &
         BetaLimiter*cTwoThird*abs(GradNormal_V),&
         BetaLimiter*cHalf*SignGradNormal_V*(Coarse1_V-Coarse2_V), &
         cTwoSeventh*SignGradNormal_V*(AveragedFine1_V-Coarse2_V) ))

    do j2=1,2;do i2=1,2
       !??? This limiting seems to be an incorrect part of the algorithm ???
       !Limit transverse gradients, if they are larger than the normal one
       !Before limiting the transverse gradients are equal to
       !Fine1_VII-AveragedFine1V
       CoarseToFineF_VII(:,i2,j2)=Coarse1_V+GradNormalLtd_V+&
            !sign(min(abs(GradNormalLtd_V),&
            !abs(Fine1_VII(:,i2,j2)-AveragedFine1_V)),&
            Fine1_VII(:,i2,j2)-AveragedFine1_V!)
    end do;end do

    do j2=1,2;do i2=1,2
       !Limit gradient in the first layer of finer cells
       GradNormalLtd_V = SignGradNormal_V*max(cZero,min( &
            SignGradNormal_V*(Fine1_VII(:,i2,j2)-Coarse1_V),&
            BetaLimiter*cThird*abs(GradNormal_V),&
            BetaLimiter*cHalf*SignGradNormal_V &
            *(Fine2_VII(:,i2,j2)-Fine1_VII(:,i2,j2)), &
            cFifth*SignGradNormal_V*(AveragedFine2_V-Coarse1_V) &
            ))
       FineToCoarseF_VII(:,i2,j2)=Fine1_VII(:,i2,j2)-GradNormalLtd_V
       FineF_VII(:,i2,j2)=Fine1_VII(:,i2,j2)+GradNormalLtd_V

       !if(DoTestLimiter)then
       !   write(*,*)'GradNormal_V   =',GradNormal_V(VarTest)
       !   write(*,*)'SignGradNormal =',SignGradNormal_V(VarTest)
       !   write(*,*)'Fine1-Coarse1  =',Fine1_VII(VarTest,i2,j2)-Coarse1_V(VarTest)
       !   write(*,*)'Fine2-Fine1    =',Fine2_VII(VarTest,i2,j2)-Fine1_VII(VarTest,i2,j2)
       !   write(*,*)'GradNormalLtd_V=',GradNormalLtd_V(VarTest)
       !end if

    end do;end do

  end subroutine tvd_reschange

end module ModLimiter

!=============================================================================

subroutine calc_facevalues(DoResChangeOnly)

  ! The subroutine calculates right and left face values.
  ! If DoResChangeOnly is true, only facevalues next to a coarser 
  ! neighbor block are calculated.
  ! To improve the code stability, ONLY those values are calculated, which
  ! are actually used by the calc_facefluxes.

  use ModMain
  use ModVarIndexes
  use ModLimiter
  use ModGeometry, ONLY : true_cell,body_BLK
  use ModNumConst
  use ModPhysics, ONLY: c2LIGHT,inv_c2LIGHT  !^CFG IF BORISCORR BEGIN
  use ModAdvance, ONLY: B0xCell_BLK, B0yCell_BLK, B0zCell_BLK, &
       B0xFace_x_BLK, B0yFace_x_BLK, B0zFace_x_BLK,&
       B0xFace_y_BLK, B0yFace_y_BLK, B0zFace_y_BLK,&
       B0xFace_z_BLK, B0yFace_z_BLK, B0zFace_z_BLK    !^CFG END BORISCORR
  use ModAdvance, ONLY: State_VGB,&
       LeftState_VX,      &  ! Face Left  X
       RightState_VX,     &  ! Face Right X
       LeftState_VY,      &  ! Face Left  Y
       RightState_VY,     &  ! Face Right Y
       LeftState_VZ,      &  ! Face Left  Z
       RightState_VZ         ! Face Right Z

  use ModParallel, ONLY : &
       neiLEV,neiLtop,neiLbot,neiLeast,neiLwest,neiLnorth,neiLsouth

  implicit none

  logical, intent(in):: DoResChangeOnly
  logical::DoTest,DoTestMe
  integer:: i,j,k,m,l,iSide,iBlock
  real:: RhoInv

  real:: RhoC2Inv, BxFull, ByFull, BzFull, B2Full,& !^CFG IF BORISCORR
         uBC2Inv,Ga2Boris                           !^CFG IF BORISCORR

  !primitive variables
  real, dimension(nVar,&
       -1:nI+2,-1:nJ+2,-1:nK+2)::Primitive_VG
  !---------------------------------------------------------------------------

  iBlock=globalBLK

  if(iBlock==BLKtest)then !!! .and. .not. DoResChangeOnly )then
     call set_oktest('calc_facevalues',DoTest,DoTestMe)
  else
     DoTest=.false.; DoTestMe=.false.
  end if

  if(.not.DoResChangeOnly & !In order not to call it twice
       .and.nOrder==2     & !Is not needed for nOrder=1
       .and.UseTVDAtResChange)call correct_monotone_restrict(iBlock)

  ! first, calculate the CELL values for the variables to be limited
  ! for non-boris corrections they are: density, velocity, pressure
  ! for boris correction momentum is used instead of the velocity
  !
  if(DoLimitMomentum)then                     !^CFG IF BORISCORR BEGIN 
     do k=kMinFaceX,kMaxFaceX
        do j=jMinFaceX,jMaxFaceX
           do i=1-nOrder,nI+nOrder
              call calc_primitives_boris       !needed for x-faces
           end do
        end do
     end do
     do k=kMinFaceY,kMaxFaceY
        do i=iMinFaceY,iMaxFaceY
           do j=1-nOrder,jMinFaceX-1
              call calc_primitives_boris       ! additional calculations for 
           end do                              ! y -faces
           do j=jMaxFaceX+1,nJ+nOrder
              call calc_primitives_boris       ! additional calculations for 
           end do	                       ! y-faces
        end do
     end do
     do j=jMinFaceZ,jMaxFaceZ
        do i=iMinFaceZ,iMaxFaceZ
           do k=1-nOrder,kMinFaceX-1
              call calc_primitives_boris       ! additional calculations for 
           end do                              ! z-faces
           do k=kMaxFaceX+1,nK+nOrder
              call calc_primitives_boris       ! additional calculations for 
           end do	                       ! z-faces
        end do
     end do
  else                                         !^CFG END BORISCORR
     do k=kMinFaceX,kMaxFaceX
        do j=jMinFaceX,jMaxFaceX
           do i=1-nOrder,nI+nOrder
              call calc_primitives_MHD         !needed for x-faces
           end do
        end do
     end do
     do k=kMinFaceY,kMaxFaceY
        do i=iMinFaceY,iMaxFaceY
           do j=1-nOrder,jMinFaceX-1
              call calc_primitives_MHD         ! additional calculations for 
           end do                              ! y -faces
           do j=jMaxFaceX+1,nJ+nOrder
              call calc_primitives_MHD         ! additional calculations for 
           end do	                       ! y-faces
        end do
     end do
     do j=jMinFaceZ,jMaxFaceZ
        do i=iMinFaceZ,iMaxFaceZ
           do k=1-nOrder,kMinFaceX-1
              call calc_primitives_MHD         ! additional calculations for 
           end do                              ! z-faces
           do k=kMaxFaceX+1,nK+nOrder
              call calc_primitives_MHD         ! additional calculations for 
           end do	                       ! z-faces
        end do
     end do
  end if                                       !^CFG IF BORISCORR

  !\
  ! Now the first or second order face values are calcuted
  !/
  select case(nOrder)
  case(1)
     !\
     ! First order reconstruction
     !/
     if (.not.DoResChangeOnly) then
        call get_faceX_first(1,nIFace,jMinFaceX,jMaxFaceX,kMinFaceX,kMaxFaceX)
        call get_faceY_first(iMinFaceY,iMaxFaceY,1,nJFace,kMinFaceY,kMaxFaceY)
        call get_faceZ_first(iMinFaceZ,iMaxFaceZ,jMinFaceZ,jMaxFaceZ,1,nKFace)
     else
        if(neiLeast(iBlock)==+1)&
             call get_faceX_first(1,1,1,nJ,1,nK)
        if(neiLwest(iBlock)==+1)&
             call get_faceX_first(nIFace,nIFace,1,nJ,1,nK)
        if(neiLsouth(iBlock)==+1) &
             call get_faceY_first(1,nI,1,1,1,nK)
        if(neiLnorth(iBlock)==+1) &
             call get_faceY_first(1,nI,nJFace,nJFace,1,nK)
        if(neiLbot(iBlock)==+1) &
             call get_faceZ_first(1,nI,1,nJ,1,1)
        if(neiLtop(iBlock)==+1) &
             call get_faceZ_first(1,nI,1,nJ,nKFace,nKFace)
     end if
  case(2)
     ! For second order scheme (nOrder==2)   
     ! use second order limited reconstruction.
     ! When prolong_order==1 we can use first order reconstruction 
     ! at resolution changes.
     ! However, constrained transport requires facevalues !^CFG IF CONSTRAINB
     ! to be independent of the resolution changes.       !^CFG IF CONSTRAINB


     if (.not.DoResChangeOnly)then
        call get_faceX_second(1,nIFace,jMinFaceX,jMaxFaceX,kMinFaceX,kMaxFaceX)
        call get_faceY_second(iMinFaceY,iMaxFaceY,1,nJFace,kMinFaceY,kMaxFaceY)
        call get_faceZ_second(iMinFaceZ,iMaxFaceZ,jMinFaceZ,jMaxFaceZ,1,nKFace)
     end if

     if(prolong_order==1 &
          .and..not.UseConstrainB & !^CFG IF CONSTRAINB
        )then

        if(.not.UseTVDAtResChange)then
           ! First order facevalues at resolution change
           if(neiLeast(iBlock)==+1)&
                call get_faceX_first(1,1,1,nJ,1,nK)
           if(neiLwest(iBlock)==+1)&
                call get_faceX_first(nIFace,nIFace,1,nJ,1,nK)
           if(neiLsouth(iBlock)==+1) &
                call get_faceY_first(1,nI,1,1,1,nK)
           if(neiLnorth(iBlock)==+1) &
                call get_faceY_first(1,nI,nJFace,nJFace,1,nK)
           if(neiLbot(iBlock)==+1) &
                call get_faceZ_first(1,nI,1,nJ,1,1)
           if(neiLtop(iBlock)==+1) &
                call get_faceZ_first(1,nI,1,nJ,nKFace,nKFace)
        else
           do iSide=east_,top_
              if(neilev(iSide,iBlock) ==+1)call get_face_tvd(iSide)
           end do
        end if
     else if(DoResChangeOnly) then
        ! Second order face values at resolution changes
        if(neiLeast(iBlock)==+1)&
             call get_faceX_second(1,1,1,nJ,1,nK)
        if(neiLwest(iBlock)==+1)&
             call get_faceX_second(nIFace,nIFace,1,nJ,1,nK)
        if(neiLsouth(iBlock)==+1) &
             call get_faceY_second(1,nI,1,1,1,nK)
        if(neiLnorth(iBlock)==+1) &
             call get_faceY_second(1,nI,nJFace,nJFace,1,nK)
        if(neiLbot(iBlock)==+1) &
             call get_faceZ_second(1,nI,1,nJ,1,1)
        if(neiLtop(iBlock)==+1) &
             call get_faceZ_second(1,nI,1,nJ,nKFace,nKFace)
     endif
  end select  !end second order

  if(DoTestMe)write(*,*)&
       'calc_facevalues_opt: final UyFaceR_y=',&
       RightState_VY(Uy_,Itest,Jtest,Ktest)

contains
  !========================================================
  subroutine calc_primitives_MHD
    Primitive_VG(:,i,j,k) = &
         State_VGB(1:nVar,i,j,k,iBlock)
    RhoInv=cOne/Primitive_VG(rho_,i,j,k)
    Primitive_VG(Ux_:Uz_,i,j,k)=RhoInv*&
         Primitive_VG(rhoUx_:rhoUz_,i,j,k)
  end subroutine calc_primitives_MHD
  !======================================================
  !^CFG IF BORISCORR BEGIN
  subroutine calc_primitives_boris                                       
    !momentum is limited

    ! rhoU_Boris = rhoU - ((U x B) x B)/c^2
    !            = rhoU + (U B^2 - B U.B)/c^2
    !            = rhoU*(1+BB/(rho*c2)) - B UdotB/c^2
    Primitive_VG(:,i,j,k) = &
         State_VGB(1:nVar,i,j,k,iBlock)
    BxFull = B0xCell_BLK(i,j,k,iBlock) + Primitive_VG(Bx_,i,j,k)
    ByFull = B0yCell_BLK(i,j,k,iBlock) + Primitive_VG(By_,i,j,k)
    BzFull = B0zCell_BLK(i,j,k,iBlock) + Primitive_VG(Bz_,i,j,k)
    B2Full = BxFull**2 + ByFull**2 + BzFull**2
    RhoC2Inv  =cOne/(Primitive_VG(rho_,i,j,k)*c2LIGHT)
    uBC2Inv= (Primitive_VG(rhoUx_,i,j,k)*BxFull + &
         Primitive_VG(rhoUy_,i,j,k)*ByFull + &
         Primitive_VG(rhoUz_,i,j,k)*BzFull)*RhoC2Inv
    Ga2Boris=cOne+B2Full*RhoC2Inv

    Primitive_VG(Ux_,i,j,k)= Primitive_VG(rhoUx_,i,j,k)*&
         Ga2Boris - BxFull*uBC2Inv
    Primitive_VG(Uy_,i,j,k)= Primitive_VG(rhoUy_,i,j,k)*&
         Ga2Boris - ByFull*uBC2Inv
    Primitive_VG(Uz_,i,j,k)= Primitive_VG(rhoUz_,i,j,k)*&
         Ga2Boris - BzFull*uBC2Inv
  end subroutine calc_primitives_boris
  !======================================================                  
  !^CFG END BORISCORR
  subroutine get_faceX_first(iMin,iMax,jMin,jMax,kMin,kMax)
    integer,intent(in):: iMin,iMax,jMin,jMax,kMin,kMax
    do k=kMin, kMax; do j=jMin, jMax; do i=iMin,iMax
       LeftState_VX(:,i,j,k)=Primitive_VG(:,i-1,j,k)
       RightState_VX(:,i,j,k)=Primitive_VG(:,i,j,k)              
    end do; end do; end do
    if(DoLimitMomentum)&                                     !^CFG IF BORISCORR
         call BorisFaceXtoMHD(iMin,iMax,jMin,jMax,kMin,kMax) !^CFG IF BORISCORR
  end subroutine get_faceX_first
  !========================================================================
  subroutine get_faceY_first(iMin,iMax,jMin,jMax,kMin,kMax)
    integer,intent(in):: iMin,iMax,jMin,jMax,kMin,kMax
    do k=kMin, kMax; do j=jMin, jMax; do i=iMin,iMax
       LeftState_VY(:,i,j,k)=Primitive_VG(:,i,j-1,k)
       RightState_VY(:,i,j,k)=Primitive_VG(:,i,j,k)              
    end do; end do; end do
    if(DoLimitMomentum)&                                     !^CFG IF BORISCORR
         call BorisFaceYtoMHD(iMin,iMax,jMin,jMax,kMin,kMax) !^CFG IF BORISCORR
  end subroutine get_faceY_first
  !========================================================================
  subroutine get_faceZ_first(iMin,iMax,jMin,jMax,kMin,kMax)
    integer,intent(in):: iMin,iMax,jMin,jMax,kMin,kMax
    do k=kMin, kMax; do j=jMin, jMax; do i=iMin,iMax
       LeftState_VZ(:,i,j,k)=Primitive_VG(:,i,j,k-1)
       RightState_VZ(:,i,j,k)=Primitive_VG(:,i,j,k)              
    end do; end do; end do
    if(DoLimitMomentum)&                                     !^CFG IF BORISCORR
         call BorisFaceZtoMHD(iMin,iMax,jMin,jMax,kMin,kMax) !^CFG IF BORISCORR
  end subroutine get_faceZ_first
  !============================================================================
  !^CFG IF BORISCORR BEGIN
  subroutine BorisFaceXtoMHD(iMin,iMax,jMin,jMax,kMin,kMax)

    ! Convert face centered Boris momenta to MHD velocities

    integer, intent(in) :: iMin,iMax,jMin,jMax,kMin,kMax
    !--------------------------------------------------------------------------
    ! U_Boris=rhoU_Boris/rho
    ! U = 1/[1+BB/(rho c^2)]* (U_Boris + (UBorisdotB/(rho c^2) * B)

    do k=kMin, kMax; do j=jMin, jMax; do i=iMin,iMax

       ! Left face values
       RhoInv=cOne/LeftState_VX(rho_,i,j,k)
       BxFull = B0xFace_x_BLK(i,j,k,iBlock) + LeftState_VX(Bx_,i,j,k)
       ByFull = B0yFace_x_BLK(i,j,k,iBlock) + LeftState_VX(By_,i,j,k)
       BzFull = B0zFace_x_BLK(i,j,k,iBlock) + LeftState_VX(Bz_,i,j,k)
       B2Full = BxFull**2 + ByFull**2 + BzFull**2
       RhoC2Inv  = inv_c2LIGHT*RhoInv
       LeftState_VX(Ux_,i,j,k)=LeftState_VX(Ux_,i,j,k)*RhoInv
       LeftState_VX(Uy_,i,j,k)=LeftState_VX(Uy_,i,j,k)*RhoInv
       LeftState_VX(Uz_,i,j,k)=LeftState_VX(Uz_,i,j,k)*RhoInv
       uBC2Inv= (LeftState_VX(Ux_,i,j,k)*BxFull + &
                 LeftState_VX(Uy_,i,j,k)*ByFull + &
                 LeftState_VX(Uz_,i,j,k)*BzFull)*RhoC2Inv

       ! gammaA^2 = 1/[1+BB/(rho c^2)]
       Ga2Boris=cOne/(cOne+ B2Full*RhoC2Inv)

       LeftState_VX(Ux_,i,j,k) = &
            Ga2Boris * (LeftState_VX(Ux_,i,j,k)+uBC2Inv*BxFull)
       LeftState_VX(Uy_,i,j,k) = &
            Ga2Boris * (LeftState_VX(Uy_,i,j,k)+uBC2Inv*ByFull)
       LeftState_VX(Uz_,i,j,k) = &
            Ga2Boris * (LeftState_VX(Uz_,i,j,k)+uBC2Inv*BzFull)

       ! Right face values
       RhoInv=cOne/RightState_VX(rho_,i,j,k)
       BxFull = B0xFace_x_BLK(i,j,k,iBlock) + RightState_VX(Bx_,i,j,k)
       ByFull = B0yFace_x_BLK(i,j,k,iBlock) + RightState_VX(By_,i,j,k)
       BzFull = B0zFace_x_BLK(i,j,k,iBlock) + RightState_VX(Bz_,i,j,k)
       B2Full = BxFull**2 + ByFull**2 + BzFull**2
       RhoC2Inv  =inv_c2LIGHT*RhoInv
       RightState_VX(Ux_,i,j,k)=RightState_VX(Ux_,i,j,k)*RhoInv
       RightState_VX(Uy_,i,j,k)=RightState_VX(Uy_,i,j,k)*RhoInv
       RightState_VX(Uz_,i,j,k)=RightState_VX(Uz_,i,j,k)*RhoInv
       uBC2Inv= (RightState_VX(Ux_,i,j,k)*BxFull + &
                 RightState_VX(Uy_,i,j,k)*ByFull + &
                 RightState_VX(Uz_,i,j,k)*BzFull)*RhoC2Inv

       ! gammaA^2 = 1/[1+BB/(rho c^2)]
       Ga2Boris=cOne/(cOne+B2Full*RhoC2Inv)

       RightState_VX(Ux_,i,j,k) = &
            Ga2Boris * (RightState_VX(Ux_,i,j,k)+uBC2Inv*BxFull)
       RightState_VX(Uy_,i,j,k) = &
            Ga2Boris * (RightState_VX(Uy_,i,j,k)+uBC2Inv*ByFull)
       RightState_VX(Uz_,i,j,k) = &
            Ga2Boris * (RightState_VX(Uz_,i,j,k)+uBC2Inv*BzFull)

    end do; end do; end do

  end subroutine BorisFaceXtoMHD
  !======================================================

  subroutine BorisFaceYtoMHD(iMin,iMax,jMin,jMax,kMin,kMax)
    integer, intent(in) :: iMin,iMax,jMin,jMax,kMin,kMax

    ! U_Boris=rhoU_Boris/rho
    ! U = 1/[1+BB/(rho c^2)]* (U_Boris + (UBorisdotB/(rho c^2) * B)

    do k=kMin, kMax; do j=jMin, jMax; do i=iMin,iMax

       ! Left face values
       RhoInv=cOne/LeftState_VY(rho_,i,j,k)
       BxFull = B0xFace_y_BLK(i,j,k,iBlock) + LeftState_VY(Bx_,i,j,k)
       ByFull = B0yFace_y_BLK(i,j,k,iBlock) + LeftState_VY(By_,i,j,k)
       BzFull = B0zFace_y_BLK(i,j,k,iBlock) + LeftState_VY(Bz_,i,j,k)
       B2Full = BxFull**2 + ByFull**2 + BzFull**2
       RhoC2Inv  =inv_c2LIGHT*RhoInv
       LeftState_VY(Ux_,i,j,k)=LeftState_VY(Ux_,i,j,k)*RhoInv
       LeftState_VY(Uy_,i,j,k)=LeftState_VY(Uy_,i,j,k)*RhoInv
       LeftState_VY(Uz_,i,j,k)=LeftState_VY(Uz_,i,j,k)*RhoInv
       uBC2Inv= (LeftState_VY(Ux_,i,j,k)*BxFull + &
                 LeftState_VY(Uy_,i,j,k)*ByFull + &
                 LeftState_VY(Uz_,i,j,k)*BzFull)*RhoC2Inv

       ! gammaA^2 = 1/[1+BB/(rho c^2)]
       Ga2Boris=cOne/(cOne+ B2Full*RhoC2Inv)

        LeftState_VY(Ux_,i,j,k) = &
             Ga2Boris * (LeftState_VY(Ux_,i,j,k)+uBC2Inv*BxFull)
        LeftState_VY(Uy_,i,j,k) = &
             Ga2Boris * (LeftState_VY(Uy_,i,j,k)+uBC2Inv*ByFull)
        LeftState_VY(Uz_,i,j,k) = &
             Ga2Boris * (LeftState_VY(Uz_,i,j,k)+uBC2Inv*BzFull)

       ! Right face values
       RhoInv=cOne/RightState_VY(rho_,i,j,k) 
       BxFull = B0xFace_y_BLK(i,j,k,iBlock) + RightState_VY(Bx_,i,j,k)
       ByFull = B0yFace_y_BLK(i,j,k,iBlock) + RightState_VY(By_,i,j,k)
       BzFull = B0zFace_y_BLK(i,j,k,iBlock) + RightState_VY(Bz_,i,j,k)
       B2Full = BxFull**2 + ByFull**2 + BzFull**2
       RhoC2Inv=inv_c2LIGHT*RhoInv
       RightState_VY(Ux_,i,j,k)=RightState_VY(Ux_,i,j,k)*RhoInv
       RightState_VY(Uy_,i,j,k)=RightState_VY(Uy_,i,j,k)*RhoInv
       RightState_VY(Uz_,i,j,k)=RightState_VY(Uz_,i,j,k)*RhoInv
       uBC2Inv= (RightState_VY(Ux_,i,j,k)*BxFull + &
                 RightState_VY(Uy_,i,j,k)*ByFull + &
                 RightState_VY(Uz_,i,j,k)*BzFull)*RhoC2Inv

       ! gammaA^2 = 1/[1+BB/(rho c^2)]
       Ga2Boris=cOne/(cOne + B2Full*RhoC2Inv)

       RightState_VY(Ux_,i,j,k) = &
            Ga2Boris * (RightState_VY(Ux_,i,j,k)+uBC2Inv*BxFull)
       RightState_VY(Uy_,i,j,k) = &
            Ga2Boris * (RightState_VY(Uy_,i,j,k)+uBC2Inv*ByFull)
       RightState_VY(Uz_,i,j,k) = &
            Ga2Boris * (RightState_VY(Uz_,i,j,k)+uBC2Inv*BzFull)
    end do; end do; end do

  end subroutine BorisFaceYtoMHD
  !============================================================================
  subroutine BorisFaceZtoMHD(iMin,iMax,jMin,jMax,kMin,kMax)
    integer, intent(in) :: iMin,iMax,jMin,jMax,kMin,kMax

    ! Convert face centered Boris momenta/rho to MHD velocities
    !--------------------------------------------------------------------------
    ! U_Boris=rhoU_Boris/rho
    ! U = 1/[1+BB/(rho c^2)]* (U_Boris + (UBorisdotB/(rho c^2) * B)

    do k=kMin, kMax; do j=jMin, jMax; do i=iMin,iMax

       ! Left face values
       RhoInv=cOne/LeftState_VZ(rho_,i,j,k)
       BxFull = B0xFace_z_BLK(i,j,k,iBlock) + LeftState_VZ(Bx_,i,j,k)
       ByFull = B0yFace_z_BLK(i,j,k,iBlock) + LeftState_VZ(By_,i,j,k)
       BzFull = B0zFace_z_BLK(i,j,k,iBlock) + LeftState_VZ(Bz_,i,j,k)
       B2Full = BxFull**2 + ByFull**2 + BzFull**2
       RhoC2Inv  =inv_c2LIGHT*RhoInv
       LeftState_VZ(Ux_,i,j,k)=LeftState_VZ(Ux_,i,j,k)*RhoInv
       LeftState_VZ(Uy_,i,j,k)=LeftState_VZ(Uy_,i,j,k)*RhoInv
       LeftState_VZ(Uz_,i,j,k)=LeftState_VZ(Uz_,i,j,k)*RhoInv
       uBC2Inv= (LeftState_VZ(Ux_,i,j,k)*BxFull + &
                 LeftState_VZ(Uy_,i,j,k)*ByFull + &
                 LeftState_VZ(Uz_,i,j,k)*BzFull)*RhoC2Inv

       ! gammaA^2 = 1/[1+BB/(rho c^2)]
       Ga2Boris=cOne/(cOne + B2Full*RhoC2Inv)

       LeftState_VZ(Ux_,i,j,k) = &
            Ga2Boris * (LeftState_VZ(Ux_,i,j,k)+uBC2Inv*BxFull)
       LeftState_VZ(Uy_,i,j,k) = &
            Ga2Boris * (LeftState_VZ(Uy_,i,j,k)+uBC2Inv*ByFull)
       LeftState_VZ(Uz_,i,j,k) = &
            Ga2Boris * (LeftState_VZ(Uz_,i,j,k)+uBC2Inv*BzFull)

       ! Right face values
       RhoInv=cOne/RightState_VZ(rho_,i,j,k)
       BxFull = B0xFace_z_BLK(i,j,k,iBlock) + RightState_VZ(Bx_,i,j,k)
       ByFull = B0yFace_z_BLK(i,j,k,iBlock) + RightState_VZ(By_,i,j,k)
       BzFull = B0zFace_z_BLK(i,j,k,iBlock) + RightState_VZ(Bz_,i,j,k)
       B2Full = BxFull**2 + ByFull**2 + BzFull**2
       RhoC2Inv  =inv_c2LIGHT*RhoInv
       RightState_VZ(Ux_,i,j,k)=RightState_VZ(Ux_,i,j,k)*RhoInv
       RightState_VZ(Uy_,i,j,k)=RightState_VZ(Uy_,i,j,k)*RhoInv
       RightState_VZ(Uz_,i,j,k)=RightState_VZ(Uz_,i,j,k)*RhoInv
       uBC2Inv= (RightState_VZ(Ux_,i,j,k)*BxFull + &
                 RightState_VZ(Uy_,i,j,k)*ByFull + &
                 RightState_VZ(Uz_,i,j,k)*BzFull)*RhoC2Inv

       ! gammaA^2 = 1/[1+BB/(rho c^2)]
       Ga2Boris=cOne/(cOne + B2Full*RhoC2Inv)

       RightState_VZ(Ux_,i,j,k) = &
            Ga2Boris * (RightState_VZ(Ux_,i,j,k)+uBC2Inv*BxFull)
       RightState_VZ(Uy_,i,j,k) = &
            Ga2Boris * (RightState_VZ(Uy_,i,j,k)+uBC2Inv*ByFull)
       RightState_VZ(Uz_,i,j,k) = &
            Ga2Boris * (RightState_VZ(Uz_,i,j,k)+uBC2Inv*BzFull)
    end do; end do; end do

  end subroutine BorisFaceZtoMHD
  !============================================================================
  !^CFG END BORISCORR
  !=======================================================================
  subroutine get_face_tvd(iSideIn)
    integer,intent(in)::iSideIn

    select case(iSideIn)
    case(east_)
       do k=1,nK,2; do j=1,nJ,2
          if(.not.all(true_cell(-1:2,j:j+1,k:k+1,iBlock)))then
             call tvd_reschange_body(& 
                  Coarse2_V    =    Primitive_VG(:,-1,j,k)           ,&
                  Coarse1_V    =    Primitive_VG(:, 0,j,k)           ,&
                  Fine1_VII    =    Primitive_VG(:, 1,j:j+1,k:k+1)   ,&
                  Fine2_VII    =    Primitive_VG(:, 2,j:j+1,k:k+1)   ,&
                  CoarseToFineF_VII= LeftState_VX(:,1,j:j+1,k:k+1)   ,&
                  FineToCoarseF_VII=RightState_VX(:,1,j:j+1,k:k+1)   ,&
                  FineF_VII        = LeftState_VX(:,2,j:j+1,k:k+1)   ,&
                  IsTrueCoarse2    = true_cell(-1,j,k,iBlock)        ,&
                  IsTrueCoarse1    = true_cell( 0,j,k,iBlock)        ,&
                  IsTrueFine1  =all(true_cell( 1,j:j+1,k:k+1,iBlock)),&
                  IsTrusFine2_II      =true_cell( 2,j:j+1,k:k+1,iBlock))
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
    case(west_)
       do k=1,nK,2; do j=1,nJ,2
          if(.not.all(true_cell(nI-1:nI+2,j:j+1,k:k+1,iBlock)))then
             call tvd_reschange_body(& 
                  Coarse2_V    =    Primitive_VG(:,nI+2,j,k)         ,&
                  Coarse1_V    =    Primitive_VG(:, nI+1,j,k)        ,&
                  Fine1_VII    =    Primitive_VG(:, nI,j:j+1,k:k+1)  ,&
                  Fine2_VII    =    Primitive_VG(:, nI-1,j:j+1,k:k+1),&
                  CoarseToFineF_VII=RightState_VX(:,nI+1,j:j+1,k:k+1),&
                  FineToCoarseF_VII=LeftState_VX(:,nI+1,j:j+1,k:k+1) ,&
                  FineF_VII        =RightState_VX(:,nI,j:j+1,k:k+1)  ,&
                  IsTrueCoarse2    = true_cell(nI+2,j,k,iBlock)      ,&
                  IsTrueCoarse1    = true_cell(nI+1,j,k,iBlock)      ,&
                  IsTrueFine1  =all(true_cell(nI,j:j+1,k:k+1,iBlock)),&
                  IsTrusFine2_II      =true_cell(nI-1,j:j+1,k:k+1,iBlock))
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
    case(south_)
       do k=1,nK,2; do i=1,nI,2
          if(.not.all(true_cell(i:i+1,-1:2,k:k+1,iBlock)))then
             call tvd_reschange_body(& 
                  Coarse2_V    =    Primitive_VG(:,i,-1,k)           ,&
                  Coarse1_V    =    Primitive_VG(:,i, 0,k)           ,&
                  Fine1_VII    =    Primitive_VG(:,i:i+1, 1,k:k+1)   ,&
                  Fine2_VII    =    Primitive_VG(:,i:i+1, 2,k:k+1)   ,&
                  CoarseToFineF_VII= LeftState_VY(:,i:i+1,1,k:k+1)   ,&
                  FineToCoarseF_VII=RightState_VY(:,i:i+1,1,k:k+1)   ,&
                  FineF_VII        = LeftState_VY(:,i:i+1,2,k:k+1)   ,&
                  IsTrueCoarse2    = true_cell(i,-1,k,iBlock)        ,&
                  IsTrueCoarse1    = true_cell(i, 0,k,iBlock)        ,&
                  IsTrueFine1  =all(true_cell(i:i+1, 1,k:k+1,iBlock)),&
                  IsTrusFine2_II      =true_cell(i:i+1, 2,k:k+1,iBlock))
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
    case(north_)
       do k=1,nK,2; do i=1,nI,2
          if(.not.all(true_cell(i:i+1,nJ-1:nJ+2,k:k+1,iBlock)))then
             call tvd_reschange_body(& 
                  Coarse2_V    =    Primitive_VG(:,i,nJ+2,k)         ,&
                  Coarse1_V    =    Primitive_VG(:,i,nJ+1,k)         ,&
                  Fine1_VII    =    Primitive_VG(:,i:i+1, nJ,k:k+1)  ,&
                  Fine2_VII    =    Primitive_VG(:,i:i+1, nJ-1,k:k+1),&
                  CoarseToFineF_VII=RightState_VY(:,i:i+1,nJ+1,k:k+1),&
                  FineToCoarseF_VII=LeftState_VY(:,i:i+1,nJ+1,k:k+1) ,&
                  FineF_VII        =RightState_VY(:,i:i+1,nJ,k:k+1)  ,&
                  IsTrueCoarse2    = true_cell(i,nJ+2,k,iBlock)      ,&
                  IsTrueCoarse1    = true_cell(i,nJ+1,k,iBlock)      ,&
                  IsTrueFine1  =all(true_cell(i:i+1,nJ,k:k+1,iBlock)),&
                  IsTrusFine2_II      =true_cell(i:i+1,nJ-1,k:k+1,iBlock))
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
    case(bot_)
       do j=1,nJ,2; do i=1,nI,2
          if(.not.all(true_cell(i:i+1,j:j+1,-1:2,iBlock)))then
             call tvd_reschange_body(& 
                  Coarse2_V    =    Primitive_VG(:,i,j,-1)           ,&
                  Coarse1_V    =    Primitive_VG(:,i,j, 0)           ,&
                  Fine1_VII    =    Primitive_VG(:,i:i+1,j:j+1, 1)   ,&
                  Fine2_VII    =    Primitive_VG(:,i:i+1,j:j+1, 2)   ,&
                  CoarseToFineF_VII= LeftState_VZ(:,i:i+1,j:j+1,1)   ,&
                  FineToCoarseF_VII=RightState_VZ(:,i:i+1,j:j+1,1)   ,&
                  FineF_VII        = LeftState_VZ(:,i:i+1,j:j+1,2)   ,&
                  IsTrueCoarse2    = true_cell(i,j,-1,iBlock)        ,&
                  IsTrueCoarse1    = true_cell(i,j, 0,iBlock)        ,&
                  IsTrueFine1 =all(true_cell(i:i+1,j:j+1, 1,iBlock)),&
                  IsTrusFine2_II      =true_cell(i:i+1,j:j+1, 2,iBlock))
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
    case(top_)
       do j=1,nJ,2; do i=1,nI,2
          if(.not.all(true_cell(i:i+1,j:j+1,nK-1:nK+2,iBlock)))then
             call tvd_reschange_body(& 
                  Coarse2_V    =    Primitive_VG(:,i,j,nK+2)         ,&
                  Coarse1_V    =    Primitive_VG(:,i,j,nK+1)         ,&
                  Fine1_VII    =    Primitive_VG(:,i:i+1,j:j+1, nK)  ,&
                  Fine2_VII    =    Primitive_VG(:,i:i+1,j:j+1, nK-1),&
                  CoarseToFineF_VII=RightState_VZ(:,i:i+1,j:j+1,nK+1),&
                  FineToCoarseF_VII=LeftState_VZ(:,i:i+1,j:j+1,nK+1) ,&
                  FineF_VII        =RightState_VZ(:,i:i+1,j:j+1,nK)  ,&
                  IsTrueCoarse2    =true_cell(i,j,nK+2,iBlock)       ,&
                  IsTrueCoarse1    =true_cell(i,j,nK+1,iBlock)       ,&
                  IsTrueFine1  =all(true_cell(i:i+1,j:j+1,nK,iBlock)),&
                  IsTrusFine2_II  =true_cell(i:i+1,j:j+1,nK-1,iBlock))
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
  !===========================================================================
  subroutine get_faceX_second(iMin,iMax,jMin,jMax,kMin,kMax)
    integer,intent(in):: iMin,iMax,jMin,jMax,kMin,kMax
    integer::i1
    do k=kMin, kMax; do j=jMin, jMax; 
       Primitive_VI(:,iMin-2:iMax+1)=Primitive_VG(:,iMin-2:iMax+1,j,k)
       if(body_BLK(iBlock))then
          IsTrueCell_I(iMin-2:iMax+1)=&
               true_cell(iMin-2:iMax+1,j,k,iBlock)
          call limiter_body(iMin,iMax)
       else
          call limiter(iMin,iMax)
       end if
       do i=iMin,iMax
          i1=i-1
          LeftState_VX(:,i,j,k)  = Primitive_VI(:,i1) + dVarLimL_VI(:,i1)
          RightState_VX(:,i,j,k) = Primitive_VI(:,i ) - dVarLimR_VI(:,i )

       end do
    end do; end do
    if(DoLimitMomentum) &                                    !^CFG IF BORISCORR
         call BorisFaceXtoMHD(iMin,iMax,jMin,jMax,kMin,kMax) !^CFG IF BORISCORR
  end subroutine get_faceX_second
  !============================================================================
  subroutine get_faceY_second(iMin,iMax,jMin,jMax,kMin,kMax)
    integer,intent(in):: iMin,iMax,jMin,jMax,kMin,kMax
    integer::j1
    do k=kMin, kMax; do i=iMin,iMax
       Primitive_VI(:,jMin-2:jMax+1)=Primitive_VG(:,i,jMin-2:jMax+1,k)
       if(body_BLK(iBlock))then
          IsTrueCell_I(jMin-2:jMax+1)=&
               true_cell(i,jMin-2:jMax+1,k,iBlock)
          call limiter_body(jMin,jMax)
       else
          call limiter(jMin,jMax)
       end if
       do j=jMin, jMax
          j1=j-1
          LeftState_VY(:,i,j,k)  = Primitive_VI(:,j1) + dVarLimL_VI(:,j1)
          RightState_VY(:,i,j,k) = Primitive_VI(:,j ) - dVarLimR_VI(:,j )
       end do
    end do; end do
    if(DoLimitMomentum) &                                    !^CFG IF BORISCORR
         call BorisFaceYtoMHD(iMin,iMax,jMin,jMax,kMin,kMax) !^CFG IF BORISCORR
  end subroutine get_faceY_second
  !============================================================================
  subroutine get_faceZ_second(iMin,iMax,jMin,jMax,kMin,kMax)
    integer,intent(in) :: iMin,iMax,jMin,jMax,kMin,kMax
    integer::k1
    do j=jMin,jMax; do i=iMin,iMax; 
       Primitive_VI(:,kMin-2:kMax+1)=Primitive_VG(:,i,j,kMin-2:kMax+1)
       if(body_BLK(iBlock))then
          IsTrueCell_I(kMin-2:kMax+1)=&
               true_cell(i,j,kMin-2:kMax+1,iBlock)
          call limiter_body(kMin,kMax)
       else
          call limiter(kMin,kMax)
       end if
       do k=kMin,kMax
          k1=k-1
          LeftState_VZ(:,i,j,k)  = Primitive_VI(:,k1) + dVarLimL_VI(:,k1)
          RightState_VZ(:,i,j,k) = Primitive_VI(:,k ) - dVarLimR_VI(:,k )
       end do
    end do; end do
    if(DoLimitMomentum)&                                     !^CFG IF BORISCORR
         call BorisFaceZtoMHD(iMin,iMax,jMin,jMax,kMin,kMax) !^CFG IF BORISCORR
  end subroutine get_faceZ_second
end subroutine calc_facevalues

!============================================================================
! Limiters:
! mimod limiter:
!                slim = minmod(s1,s2)
!
! generalized MC (monotonized central) limiter:
!                slim = minmod(beta*s1, beta*s2, (s1+s2)/2)
!
! beta-limiter   slim = maxmod(minmod(beta*s1,s2), minmod(beta*s2,s1))
!
!                (see C.Hirsch, Numerical Computation of
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
!       left_face  = central_value - limited_slope
!       right_face = central_value + limited_slope
!===========================================================================
subroutine limiter_body(lMin,lMax)
  use ModLimiter
  use ModNumConst
  use ModMain, ONLY: limiter_type, BetaLimiter
  implicit none
  integer, intent(in)::lMin,lMax
  real,dimension(Hi3_):: dVar1_I, dVar2_I ! 
  real,dimension(nVar):: dVar1_V, dVar2_V ! unlimited left and right slopes
  integer::l
  !---------------------------------------------------------------------------
  select case(limiter_type)
  case('beta')
     dVar1_I(1:nVar)=Primitive_VI(:,lMax+1)-Primitive_VI(:,lMax)
     dVar1_I(Lo2_:Hi2_)=abs(dVar1_I(1:nVar))
     dVar1_I(Lo3_:Hi3_)=BetaLimiter*&
          dVar1_I(Lo2_:Hi2_)
     dVar1_I(1:nVar)=sign(cQuarter,dVar1_I(1:nVar))
     do l=lMax,lMin-1,-1
        dVar2_I=dVar1_I
        dVar1_I(1:nVar)=Primitive_VI(:,l)-Primitive_VI(:,l-1)
        dVar1_I(Lo2_:Hi2_)=abs(dVar1_I(1:nVar))
        dVar1_I(Lo3_:Hi3_)=BetaLimiter*&
             dVar1_I(Lo2_:Hi2_)
        dVar1_I(1:nVar)=sign(cQuarter,dVar1_I(1:nVar))
        if(all(IsTrueCell_I(l-1:l+1)))then
           dVar2_I(1:nVar)=dVar2_I(1:nVar)+dVar1_I(1:nVar)
           dVar2_I(Lo2_:Hi2_)=min(dVar2_I(Lo2_:Hi2_),dVar1_I(Lo3_:Hi3_))
           dVar2_I(Lo3_:Hi3_)=min(dVar1_I(Lo2_:Hi2_),dVar2_I(Lo3_:Hi3_))
           dVar2_I(Lo2_:Hi2_)=max(dVar2_I(Lo2_:Hi2_),dVar2_I(Lo3_:Hi3_))
           dVarLimR_VI(:,l) = dVar2_I(1:nVar)*dVar2_I(Lo2_:Hi2_)
        else
           dVarLimR_VI(:,l) = cZero
        end if
        dVarLimL_VI(:,l) = dVarLimR_VI(:,l)
     end do
  case('minmod')
     dVar1_I(1:nVar)=Primitive_VI(:,lMax+1)-Primitive_VI(:,lMax)
     dVar1_I(Lo2_:Hi2_)=abs(dVar1_I(1:nVar))
     dVar1_I(1:nVar)=sign(cQuarter,dVar1_I(1:nVar))
     do l=lMax,lMin-1,-1
        dVar2_I(1:Hi2_)=dVar1_I(1:Hi2_)
        dVar1_I(1:nVar)=Primitive_VI(:,l)-Primitive_VI(:,l-1)
        dVar1_I(Lo2_:Hi2_)=abs(dVar1_I(1:nVar))
        dVar1_I(1:nVar)=sign(cQuarter,dVar1_I(1:nVar))
        if(all(IsTrueCell_I(l-1:l+1)))then
           dVar2_I(1:nVar)=dVar2_I(1:nVar)+dVar1_I(1:nVar)
           dVar2_I(Lo2_:Hi2_)=min(dVar2_I(Lo2_:Hi2_),dVar1_I(Lo2_:Hi2_))
           dVarLimR_VI(:,l) = dVar2_I(1:nVar)*dVar2_I(Lo2_:Hi2_)
        else
           dVarLimR_VI(:,l) = cZero
        end if
        dVarLimL_VI(:,l) = dVarLimR_VI(:,l)
     end do
  case('mc')
     dVar1_V = Primitive_VI(:,lMax+1) - Primitive_VI(:,lMax)
     do l=lMax,lMin-1,-1
        dVar2_V = dVar1_V
        dVar1_V = Primitive_VI(:,l) - Primitive_VI(:,l-1)
        if(all(IsTrueCell_I(l-1:l+1)))then
           dVarLimR_VI(:,l) = (sign(cQuarter,dVar1_V)+sign(cQuarter,dVar2_V))* &
                min(BetaLimiter*abs(dVar1_V), BetaLimiter*abs(dVar2_V), &
                cHalf*abs(dVar1_V+dVar2_V))
        else
           dVarLimR_VI(:,l) = cZero
        end if
        dVarLimL_VI(:,l) = dVarLimR_VI(:,l)
     end do
  case('mc3')
     dVar1_V = Primitive_VI(:,lMax+1) - Primitive_VI(:,lMax)
     do l=lMax,lMin-1,-1
        dVar2_V = dVar1_V
        dVar1_V = Primitive_VI(:,l) - Primitive_VI(:,l-1)
        if(all(IsTrueCell_I(l-1:l+1)))then
           dVarLimR_VI(:,l) = (sign(cQuarter,dVar1_V)+sign(cQuarter,dVar2_V))* &
                min(BetaLimiter*abs(dVar1_V), BetaLimiter*abs(dVar2_V), &
                cThird*abs(2*dVar1_V+dVar2_V))
           dVarLimL_VI(:,l) = (sign(cQuarter,dVar1_V)+sign(cQuarter,dVar2_V))* &
                min(BetaLimiter*abs(dVar1_V), BetaLimiter*abs(dVar2_V), &
                cThird*abs(dVar1_V+2*dVar2_V))
        else
           dVarLimR_VI(:,l) = cZero
           dVarLimL_VI(:,l) = cZero
        end if
     end do
  case default
     call stop_mpi('limiter_body: unknown TypeLimiter='//limiter_type)
  end select

end subroutine limiter_body

!===========================================================================

subroutine limiter(lMin,lMax)
  use ModLimiter
  use ModNumConst
  use ModMain, ONLY: limiter_type, BetaLimiter
  implicit none
  integer, intent(in)::lMin,lMax
  real,dimension(Hi3_):: dVar1_I, dVar2_I
  real,dimension(nVar):: dVar1_V, dVar2_V
  integer::l

  select case(limiter_type)
  case('beta')
     dVar1_I(1:nVar)=Primitive_VI(:,lMax+1)-Primitive_VI(:,lMax)
     dVar1_I(Lo2_:Hi2_)=abs(dVar1_I(1:nVar))
     dVar1_I(Lo3_:Hi3_)=BetaLimiter*&
          dVar1_I(Lo2_:Hi2_)
     dVar1_I(1:nVar)=sign(cQuarter,dVar1_I(1:nVar))
     do l=lMax,lMin-1,-1
        dVar2_I=dVar1_I
        dVar1_I(1:nVar)=Primitive_VI(:,l)-Primitive_VI(:,l-1)
        dVar1_I(Lo2_:Hi2_)=abs(dVar1_I(1:nVar))
        dVar1_I(Lo3_:Hi3_)=BetaLimiter*dVar1_I(Lo2_:Hi2_)
        dVar1_I(1:nVar)=sign(cQuarter,dVar1_I(1:nVar))
        dVar2_I(1:nVar)=dVar2_I(1:nVar)+dVar1_I(1:nVar)
        dVar2_I(Lo2_:Hi2_)=min(dVar2_I(Lo2_:Hi2_),dVar1_I(Lo3_:Hi3_))
        dVar2_I(Lo3_:Hi3_)=min(dVar1_I(Lo2_:Hi2_),dVar2_I(Lo3_:Hi3_))
        dVar2_I(Lo2_:Hi2_)=max(dVar2_I(Lo2_:Hi2_),dVar2_I(Lo3_:Hi3_))
        dVarLimR_VI(:,l) = dVar2_I(1:nVar)*dVar2_I(Lo2_:Hi2_)

        dVarLimL_VI(:,l) = dVarLimR_VI(:,l)
     end do
  case('minmod')
     dVar1_I(1:nVar)=Primitive_VI(:,lMax+1)-Primitive_VI(:,lMax)
     dVar1_I(Lo2_:Hi2_)=abs(dVar1_I(1:nVar))
     dVar1_I(1:nVar)=sign(cQuarter,dVar1_I(1:nVar))
     do l=lMax,lMin-1,-1
        dVar2_I(1:Hi2_)=dVar1_I(1:Hi2_)
        dVar1_I(1:nVar)=Primitive_VI(:,l)-Primitive_VI(:,l-1)
        dVar1_I(Lo2_:Hi2_)=abs(dVar1_I(1:nVar))
        dVar1_I(1:nVar)=sign(cQuarter,dVar1_I(1:nVar))
        dVar2_I(1:nVar)=dVar2_I(1:nVar)+dVar1_I(1:nVar)
        dVar2_I(Lo2_:Hi2_)=min(dVar2_I(Lo2_:Hi2_),dVar1_I(Lo2_:Hi2_))
        dVarLimR_VI(:,l) = dVar2_I(1:nVar)*dVar2_I(Lo2_:Hi2_)

        dVarLimL_VI(:,l) = dVarLimR_VI(:,l)
     end do
  case('mc')
     ! Calculate right most unlimited slope
     dVar1_V = Primitive_VI(:,lMax+1) - Primitive_VI(:,lMax)
     do l=lMax,lMin-1,-1
        ! Propagate old left slope to become the right slope
        dVar2_V = dVar1_V
        ! Calculate left slope
        dVar1_V = Primitive_VI(:,l) - Primitive_VI(:,l-1)
        ! Calculate the limited slope
        dVarLimR_VI(:,l) = (sign(cQuarter,dVar1_V)+sign(cQuarter,dVar2_V)) * &
             min(BetaLimiter*abs(dVar1_V), BetaLimiter*abs(dVar2_V), &
             cHalf*abs(dVar1_V+dVar2_V))

        dVarLimL_VI(:,l) = dVarLimR_VI(:,l)
     end do
  case('mc3')
     ! Calculate right most unlimited slope
     dVar1_V = Primitive_VI(:,lMax+1) - Primitive_VI(:,lMax)
     do l=lMax,lMin-1,-1
        ! Propagate old left slope to become the right slope
        dVar2_V = dVar1_V
        ! Calculate left slope
        dVar1_V = Primitive_VI(:,l) - Primitive_VI(:,l-1)
        ! Calculate the limited slopes
        dVarLimR_VI(:,l) = (sign(cQuarter,dVar1_V)+sign(cQuarter,dVar2_V))* &
             min(BetaLimiter*abs(dVar1_V), BetaLimiter*abs(dVar2_V), &
             cThird*abs(2*dVar1_V+dVar2_V))
        dVarLimL_VI(:,l) = (sign(cQuarter,dVar1_V)+sign(cQuarter,dVar2_V))* &
             min(BetaLimiter*abs(dVar1_V), BetaLimiter*abs(dVar2_V), &
             cThird*abs(dVar1_V+2*dVar2_V))
     end do
  case default
     call stop_mpi('limiter: unknown TypeLimiter='//limiter_type)
  end select

end subroutine limiter

!=======================================================================

subroutine correct_monotone_restrict(iBLK)

  ! Correct the result of the first order monotone restriction by modifying 
  ! the coarse ghost cell values such that the slope remains the same 
  ! while the cell center is moved from the fine cell distance to the
  ! coarse cell distance. 
  !
  ! This operation should be performed at most once per exchange messages.
  ! To avoid multiple modifications in schemes which switch off some blocks,
  ! the correction is not done if any of the finer block neighbors are unused.

  use ModSize
  use ModVarIndexes, ONLY: DefaultState_V, nVar
  use ModAdvance,    ONLY: State_VGB
  use ModAMR,        ONLY: unusedBlock_BP
  use ModParallel,   ONLY: neiLEV, &
       neiLtop, neiLbot, neiLeast, neiLwest, neiLnorth, neiLsouth, &
       neiBtop, neiBbot, neiBeast, neiBwest, neiBnorth, neiBsouth, &
       neiPtop, neiPbot, neiPeast, neiPwest, neiPnorth, neiPsouth
  use ModNumConst

  implicit none

  integer, intent(in) :: iBLK
  integer             :: i, j, k
  !---------------------------------------------------------------------------

  if(all(neiLEV(:,iBLK) /= -1))RETURN
  if(neiLnorth(iBLK) == -1)then
     if(     .not.unusedBlock_BP(neiBnorth(1,iBLK),neiPnorth(1,iBLK)) &
       .and. .not.unusedBlock_BP(neiBnorth(2,iBLK),neiPnorth(2,iBLK)) &
       .and. .not.unusedBlock_BP(neiBnorth(3,iBLK),neiPnorth(3,iBLK)) &
       .and. .not.unusedBlock_BP(neiBnorth(4,iBLK),neiPnorth(4,iBLK))) then
        do k=1,nK;do i=1,nI
           State_VGB(1:nVar,i,nJ+1,k,iBLK) =&
                State_VGB(1:nVar,i,nJ+1,k,iBLK)+ cThird*(&
                State_VGB(1:nVar,i,nJ+1,k,iBLK)-&
                State_VGB(1:nVar,i,nJ,k,iBLK))
           where(DefaultState_V(1:nVar)>cTiny)
              State_VGB(1:nVar,i,nJ+1,k,iBLK) = max(&
                   State_VGB(1:nVar,i,nJ+1,k,iBLK),cTiny)
           end where
        end do;end do
     end if
  end if
  if(neiLsouth(iBLK) == -1)then
     if(        .not.unusedBlock_BP(neiBsouth(1,iBLK),neiPsouth(1,iBLK)) & 
          .and. .not.unusedBlock_BP(neiBsouth(2,iBLK),neiPsouth(2,iBLK)) & 
          .and. .not.unusedBlock_BP(neiBsouth(3,iBLK),neiPsouth(3,iBLK)) & 
          .and. .not.unusedBlock_BP(neiBsouth(4,iBLK),neiPsouth(4,iBLK))) then
        do k=1,nK;do i=1,nI
           State_VGB(1:nVar,i,0,k,iBLK) = &
                State_VGB(1:nVar,i,0,k,iBLK)+ cThird*(&
                State_VGB(1:nVar,i,0,k,iBLK)- &
                State_VGB(1:nVar,i,1,k,iBLK))
           where(DefaultState_V(1:nVar)>cTiny)
              State_VGB(1:nVar,i,0,k,iBLK) =max(&
                   State_VGB(1:nVar,i,0,k,iBLK),cTiny)
           end where
        end do;end do
     end if
  end if
  if(neiLeast(iBLK) == -1)then
     if(     .not.unusedBlock_BP(neiBeast(1,iBLK),neiPeast(1,iBLK)) &
       .and. .not.unusedBlock_BP(neiBeast(2,iBLK),neiPeast(2,iBLK)) &
       .and. .not.unusedBlock_BP(neiBeast(3,iBLK),neiPeast(3,iBLK)) &
       .and. .not.unusedBlock_BP(neiBeast(4,iBLK),neiPeast(4,iBLK))) then
        do k=1,nK;do j=1,nJ
           State_VGB(1:nVar,0,j,k,iBLK) = &
                State_VGB(1:nVar,0,j,k,iBLK)+ cThird*(&
                State_VGB(1:nVar,0,j,k,iBLK)- &
                State_VGB(1:nVar,1,j,k,iBLK))
           where(DefaultState_V(1:nVar)>cTiny)
              State_VGB(1:nVar,0,j,k,iBLK) =max(&
                   State_VGB(1:nVar,0,j,k,iBLK),cTiny)
           end where
        end do;end do
     end if
  end if
  if(neiLwest(iBLK) == -1)then
     if(        .not.unusedBlock_BP(neiBwest(1,iBLK),neiPwest(1,iBLK)) &
          .and. .not.unusedBlock_BP(neiBwest(2,iBLK),neiPwest(2,iBLK)) &
          .and. .not.unusedBlock_BP(neiBwest(3,iBLK),neiPwest(3,iBLK)) &
          .and. .not.unusedBlock_BP(neiBwest(4,iBLK),neiPwest(4,iBLK))) then
        do k=1,nK;do j=1,nJ
           State_VGB(1:nVar,nI+1,j,k,iBLK) = &
                State_VGB(1:nVar,nI+1,j,k,iBLK) + cThird*(&
                State_VGB(1:nVar,nI+1,j,k,iBLK) - &
                State_VGB(1:nVar,nI,  j,k,iBLK))
           where(DefaultState_V(1:nVar)>cTiny)
              State_VGB(1:nVar,nI+1,j,k,iBLK) = max(&
                   State_VGB(1:nVar,nI+1,j,k,iBLK),cTiny)
           end where
        end do;end do
     end if
  end if
  if(neiLtop(iBLK) == -1)then
     if(        .not.unusedBlock_BP(neiBtop(1,iBLK),neiPtop(1,iBLK)) &
          .and. .not.unusedBlock_BP(neiBtop(2,iBLK),neiPtop(2,iBLK)) &
          .and. .not.unusedBlock_BP(neiBtop(3,iBLK),neiPtop(3,iBLK)) &
          .and. .not.unusedBlock_BP(neiBtop(4,iBLK),neiPtop(4,iBLK))) then
        do j=1,nJ;do i=1,nI
           State_VGB(1:nVar,i,j,nK+1,iBLK) = &
                State_VGB(1:nVar,i,j,nK+1,iBLK) + cThird*(&
                State_VGB(1:nVar,i,j,nK+1,iBLK) - &
                State_VGB(1:nVar,i,j,nK,iBLK))
           where(DefaultState_V(1:nVar)>cTiny)
              State_VGB(1:nVar,i,j,nK+1,iBLK) =max( &
                   State_VGB(1:nVar,i,j,nK+1,iBLK),cTiny)
           end where
        end do;end do
     end if
  end if
  if(neiLbot(iBLK) == -1)then
     if(        .not.unusedBlock_BP(neiBbot(1,iBLK),neiPbot(1,iBLK)) &
          .and. .not.unusedBlock_BP(neiBbot(2,iBLK),neiPbot(2,iBLK)) &
          .and. .not.unusedBlock_BP(neiBbot(3,iBLK),neiPbot(3,iBLK)) &
          .and. .not.unusedBlock_BP(neiBbot(4,iBLK),neiPbot(4,iBLK))) then
        do j=1,nJ;do i=1,nI
           State_VGB(1:nVar,i,j,0,iBLK) = &
                State_VGB(1:nVar,i,j,0,iBLK) + cThird*(&
                State_VGB(1:nVar,i,j,0,iBLK) - &
                State_VGB(1:nVar,i,j,1,iBLK))
           where(DefaultState_V(1:nVar)>cTiny)
              State_VGB(1:nVar,i,j,0,iBLK) =max( &
                   State_VGB(1:nVar,i,j,0,iBLK),cTiny)
           end where
        end do;end do
     end if
  end if

end subroutine correct_monotone_restrict
