 !^CFG COPYRIGHT UM
!//////////////////////////////////////////////////////////////////////////////
!
!    North                      8--------------7
!    y,j    Top               / |            / |
!     ^     z,k             /   |          /   |
!     |   /               /     |        /     |
!     | /     West      5-------+------6       |
!     |-----> x,i       |       |      |       |
!                       |       1------+-------2
!                       |     /        |     /
!                       |   /          |   /
!                       | /            | /
!                       4--------------3 --(front, right, and bottom 3D box corner)
!
!  Point 7 is:  +x, +y, +z, West, North, Top
!
!  Point 4 is:  -x, -y, -z, East, South, Bottom
!

subroutine find_neighbors
  use ModProcMH, ONLY: iProc
  use ModMain,   ONLY: nBlock, UnusedBLK
  use ModParallel
  use ModPolarNeighbor
  implicit none

  integer :: i, j, k, iBlock, iLevelOut
  integer, dimension(4) :: iProcOut_I, iBlockOut_I, iChildOut_I
  logical::DoTest=.false.,DoTestMe=.false.,DoCallOKTest=.true.
  character(LEN=*),parameter::NameSub='find_neighbors'
    !--------------------------------------------------------!
  if(DoCallOKTest)then
     call set_oktest(NameSub,DoTest,DoTestMe)
     DoCallOKTest=.false.
  end if
  !---------------------------------------------------------------------------
  !\
  ! Loop and fill variables with neighbor information for all blocks on this PE
  !/
  do iBlock = 1, nBlock
     if (UnusedBLK(iBlock)) CYCLE

     do k=-1,1; do j=-1,1; do i=-1,1
        call treeNeighbor(iProc, iBlock, i, j, k, &
             iProcOut_I, iBlockOut_I, iChildOut_I, iLevelOut)
        BLKneighborPE   (i,j,k,:,iBlock) = iProcOut_I
        BLKneighborBLK  (i,j,k,:,iBlock) = iBlockOut_I
        BLKneighborCHILD(i,j,k,:,iBlock) = iChildOut_I
        BLKneighborLEV  (i,j,k,  iBlock) = iLevelOut
     end do; end do; end do
  end do

  ! Fill up arrays which contain information about face neighbors only

  neiLeast(1:nBlock)    = BLKneighborLEV(-1,0,0,1:nBlock)
  neiLwest(1:nBlock)    = BLKneighborLEV(+1,0,0,1:nBlock)
  neiLsouth(1:nBlock)   = BLKneighborLEV(0,-1,0,1:nBlock)
  neiLnorth(1:nBlock)   = BLKneighborLEV(0,+1,0,1:nBlock)
  neiLbot(1:nBlock)     = BLKneighborLEV(0,0,-1,1:nBlock)
  neiLtop(1:nBlock)     = BLKneighborLEV(0,0,+1,1:nBlock)

  neiPeast(:,1:nBlock)  = BLKneighborPE(-1,0,0,:,1:nBlock)
  neiPwest(:,1:nBlock)  = BLKneighborPE(+1,0,0,:,1:nBlock)
  neiPsouth(:,1:nBlock) = BLKneighborPE(0,-1,0,:,1:nBlock)
  neiPnorth(:,1:nBlock) = BLKneighborPE(0,+1,0,:,1:nBlock)
  neiPbot(:,1:nBlock)   = BLKneighborPE(0,0,-1,:,1:nBlock)
  neiPtop(:,1:nBlock)   = BLKneighborPE(0,0,+1,:,1:nBlock)

  neiBeast(:,1:nBlock)  = BLKneighborBLK(-1,0,0,:,1:nBlock)
  neiBwest(:,1:nBlock)  = BLKneighborBLK(+1,0,0,:,1:nBlock)
  neiBsouth(:,1:nBlock) = BLKneighborBLK(0,-1,0,:,1:nBlock)
  neiBnorth(:,1:nBlock) = BLKneighborBLK(0,+1,0,:,1:nBlock)
  neiBbot(:,1:nBlock)   = BLKneighborBLK(0,0,-1,:,1:nBlock)
  neiBtop(:,1:nBlock)   = BLKneighborBLK(0,0,+1,:,1:nBlock)

  neiLEV(1,:) = neiLeast
  neiLEV(2,:) = neiLwest
  neiLEV(3,:) = neiLsouth
  neiLEV(4,:) = neiLnorth
  neiLEV(5,:) = neiLbot
  neiLEV(6,:) = neiLtop

  neiPE(:,1,:) = neiPeast
  neiPE(:,2,:) = neiPwest
  neiPE(:,3,:) = neiPsouth
  neiPE(:,4,:) = neiPnorth
  neiPE(:,5,:) = neiPbot
  neiPE(:,6,:) = neiPtop

  neiBLK(:,2,:) = neiBwest
  neiBLK(:,1,:) = neiBeast
  neiBLK(:,3,:) = neiBsouth
  neiBLK(:,4,:) = neiBnorth
  neiBLK(:,5,:) = neiBbot
  neiBLK(:,6,:) = neiBtop

end subroutine find_neighbors
!=============================================================================
subroutine treeNeighbor(inPE, inBLK, ix, iy, iz, &
     outPE, outBLK, outCHILD, outLEV)
  use ModParallel, ONLY : NOBLK
  use ModOctree
  use ModCube
  implicit none

  integer, intent(in) :: inPE,inBLK,ix,iy,iz
  integer, intent(out) :: outLEV
  integer, intent(out), dimension(4) :: outPE, outBLK, outCHILD
  integer :: i,idir,iSubFace
  logical :: noNeighbor
  type (adaptive_block_ptr) :: inBlockPtr,outBlockPtr,tmpBlockPtr

  if ( ix .lt. -1 .or. ix .gt. 1 .or. &
       iy .lt. -1 .or. iy .gt. 1 .or. &
       iz .lt. -1 .or. iz .gt. 1 ) then
     write (*,*) 'ERROR: treeNeighbor: 0: invalid direction given  ',ix,iy,iz
     !     stop
  end if
  outLEV   = NOBLK
  outPE    = NOBLK
  outBLK   = NOBLK
  outCHILD = NOBLK

  idir = 1+ (iz+1) + 3*(iy+1) + 9*(ix+1)
  inBlockPtr%ptr => global_block_ptrs(inBLK,inPE+1)%ptr
  if (associated(inBlockPtr%ptr)) then ! ensure block is allocated
     !While applied near the pole the present subroutine is often called
     !with ix=0,  iy=0,  iz=0. To improve an efficiency, consider
     !this case now.
     if(iDir==14)then !ix=0  iy=0  iz=0
        OutChild(1)=InBlockPtr%ptr%child_number
        OutPe(1)=inPE
        OutBlk(1)=inBLK
        OutLev=0
        return
     end if
     call findTreeNeighbor(inBlockPtr,outBlockPtr,idir,noNeighbor)
     if (noNeighbor) then
        outLEV   = NOBLK
        outPE    = NOBLK
        outBLK   = NOBLK
        outCHILD = NOBLK
     else
        if (associated(outBlockPtr%ptr)) then ! ensure block is allocated
           if (.not. associated(outBlockPtr%ptr%child(1)%ptr)) then
              if (inBlockPtr%ptr%LEV == outBlockPtr%ptr%LEV) then
                 outLEV = 0
              else
                 outLEV = 1
              end if
              outPE(1)    = outBlockPtr%ptr%PE
              outBLK(1)   = outBlockPtr%ptr%BLK
              outCHILD(1) = outBlockPtr%ptr%child_number
           else
              outLEV = -1
              !Set values for refined neighbor
              outPE  = NOBLK
              outBLK = NOBLK
              call get_children_list(iX,iY,iZ,OutCHILD)
              do iSubFace=1,4
                 if(outCHILD(iSubface)==NOBLK)EXIT
                 outPE (iSubFace)   = outBlockPtr%ptr%child(&
                      outCHILD(iSubFace))%ptr%PE
                 outBLK(iSubFace)   = outBlockPtr%ptr%child(&
                      outCHILD(iSubFace))%ptr%BLK
              end do
           end if
           !Error check
           do i=1,4
              if (outPE(i) .ge. 0) then
                 tmpBlockPtr%ptr => global_block_ptrs(outBLK(i),outPE(i)+1)%ptr
                 if (associated(tmpBlockPtr%ptr)) then ! ensure block is allocated
                    if (.not. tmpBlockPtr%ptr%used) then
                       write (*,*) 'ERROR: treeNeighbor: 4: block not used ',inPE,inBLK
                       !                       stop
                    end if
                 else
                    write (*,*) 'ERROR: treeNeighbor: 3: not associated ',inPE,inBLK
                    !                    stop
                 end if
              end if
           end do
        else
           write (*,*) 'ERROR: treeNeighbor: 2: not associated ',inPE,inBLK
           !           stop
        end if
     end if
  else
     write (*,*) 'ERROR: treeNeighbor: 1: not associated ',inPE,inBLK
     !     stop
  end if
contains
  subroutine get_children_list_obsolete(iX,iY,iZ,OutCHILD)
    integer,intent(in)::iX,iY,iZ
    integer,intent(out)::OutCHILD(4)
    integer::iDirHere
    outCHILD=NOBLK
    idirHere = 1+ (iz+1) + 3*(iy+1) + 9*(ix+1)
    select case (idirhere)
    case (1)  ! -X -Y -Z    ESB
       outCHILD(1) = 7 
    case (2)  ! -X -Y       ES 
       outCHILD(1:2) =(/ 6,7/)
    case (3)  ! -X -Y +Z    EST
       outCHILD(1) = 6
    case (4)  ! -X    -Z    E B
       outCHILD(1:2) =(/ 2,7/)
    case (5)  ! -X          E  
       outCHILD(1:4) =(/ 3,2,6,7/)
    case (6)  ! -X    +Z    E T
       outCHILD(1:2) =(/ 3,6/)
    case (7)  ! -X +Y -Z    ENB
       outCHILD(1) = 2
    case (8)  ! -X +Y       EN
       outCHILD(1:2) =(/ 3,2/)
    case (9)  ! -X +Y +Z    ENT
       outCHILD(1) = 3
    case (10) !    -Y -Z     SB
       outCHILD(1:2) =(/8,7/)
    case (11) !    -Y        S 
       outCHILD(1:4) =(/5,8,6,7/)
    case (12) !    -Y +Z     ST
       outCHILD(1:2) = (/5,6/)
    case (13) !       -Z      B
       outCHILD(1:4) =(/1,2,8,7/)
    case (14) !         
    case (15) !       +Z      T
       outCHILD(1:4) =(/4,3,5,6/)
    case (16) !    +Y -Z     NB
       outCHILD(1:2) =(/1,2/)
    case (17) !    +Y        N 
       outCHILD(1:4) =(/4,1,3,2/)
    case (18) !    +Y +Z     NT
       outCHILD(1:2) =(/4,3/)
    case (19) ! +X -Y -Z    WSB
       outCHILD(1) = 8
    case (20) ! +X -Y       WS 
       outCHILD(1:2) = (/5,8/)
    case (21) ! +X -Y +Z    WST
       outCHILD(1) = 5
    case (22) ! +X    -Z    W B 
       outCHILD(1:2) = (/1,8/)
    case (23) ! +X          W  
       outCHILD(1:4) =(/4,1,5,8/)
    case (24) ! +X    +Z    W T
       outCHILD(1:2) =(/4,5/)
    case (25) ! +X +Y -Z     WNB
       outCHILD(1) = 1
    case (26) ! +X +Y        WN 
       outCHILD(1:2) = (/4,1/)
    case (27) ! +X +Y +Z     WNT
       outCHILD(1) = 4
    end select
  end subroutine get_children_list_obsolete
end subroutine treeNeighbor

recursive subroutine findTreeNeighbor(inblkptr,outblkptr,idir,noNeighbor)
  use ModParallel, ONLY : proc_dims,periodic3D
  use ModOctree
  implicit none

  integer, intent(in) :: idir
  type (adaptive_block_ptr) :: inblkptr,outblkptr,tmp1blkptr,tmp2blkptr
  logical, intent(inout) :: noNeighbor

  integer :: i,j,k,iFoundIt,idirNext,idirDown,iRoot,jRoot,kRoot,ii,ix,iy,iz
  integer, dimension(27,8) :: isFound,isNext,isDown

  !idir=
  !    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
  !
  !   -X -X -X -X -X -X -X -X -X                            +X +X +X +X +X +X +X +X +X
  !   -Y -Y -Y          +Y +Y +Y -Y -Y -Y          +Y +Y +Y -Y -Y -Y          +Y +Y +Y
  !   -Z    +Z -Z    +Z -Z    +Z -Z    +Z -Z    +Z -Z    +Z -Z    +Z -Z    +Z -Z    +Z
  !
  data isFound / &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 1, 0, 5, 8, 0, 0, 0, 0, 3, 2, 0, 6, 7, 0, &
       0, 0, 0, 4, 1, 0, 5, 8, 0, 0, 0, 0, 3, 2, 0, 6, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 4, 1, 0, 5, 8, 0, 0, 0, 0, 3, 2, 0, 6, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 1, 0, 5, 8, 0, 0, 0, 0, 3, 2, 0, 6, 7, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 1, 0, 5, 8, 0, 0, 0, 0, 3, 2, 0, 6, 7, 0, 0, 0, &
       0, 4, 1, 0, 5, 8, 0, 0, 0, 0, 3, 2, 0, 6, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       4, 1, 0, 5, 8, 0, 0, 0, 0, 3, 2, 0, 6, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 1, 0, 5, 8, 0, 0, 0, 0, 3, 2, 0, 6, 7, 0, 0, 0, 0   /
  data isNext / &
       2, 2, 3, 5, 5, 6, 5, 5, 6,11,11,12, 0, 0,15, 0, 0,15,11,11,12, 0, 0,15, 0, 0,15, &
      11,11,12, 0, 0,15, 0, 0,15,11,11,12, 0, 0,15, 0, 0,15,20,20,21,23,23,24,23,23,24, &
      10,11,11,13, 0, 0,13, 0, 0,10,11,11,13, 0, 0,13, 0, 0,19,20,20,22,23,23,22,23,23, &
       1, 2, 2, 4, 5, 5, 4, 5, 5,10,11,11,13, 0, 0,13, 0, 0,10,11,11,13, 0, 0,13, 0, 0, &
       4, 5, 5, 4, 5, 5, 7, 8, 8,13, 0, 0,13, 0, 0,16,17,17,13, 0, 0,13, 0, 0,16,17,17, &
      13, 0, 0,13, 0, 0,16,17,17,13, 0, 0,13, 0, 0,16,17,17,22,23,23,22,23,23,25,26,26, &
       0, 0,15, 0, 0,15,17,17,18, 0, 0,15, 0, 0,15,17,17,18,23,23,24,23,23,24,26,26,27, &
       5, 5, 6, 5, 5, 6, 8, 8, 9, 0, 0,15, 0, 0,15,17,17,18, 0, 0,15, 0, 0,15,17,17,18   /
  data isDown / &
       6, 7, 6, 3, 2, 3, 6, 7, 6, 5, 8, 5, 0, 0, 4, 0, 0, 5, 6, 7, 6, 0, 0, 3, 0, 0, 6, &
       5, 8, 5, 0, 0, 4, 0, 0, 5, 6, 7, 6, 0, 0, 3, 0, 0, 6, 5, 8, 5, 4, 1, 4, 5, 8, 5, &
       8, 5, 8, 1, 0, 0, 8, 0, 0, 7, 6, 7, 2, 0, 0, 7, 0, 0, 8, 5, 8, 1, 4, 1, 8, 5, 8, &
       7, 6, 7, 2, 3, 2, 7, 6, 7, 8, 5, 8, 1, 0, 0, 8, 0, 0, 7, 6, 7, 2, 0, 0, 7, 0, 0, &
       2, 3, 2, 7, 6, 7, 2, 3, 2, 1, 0, 0, 8, 0, 0, 1, 4, 1, 2, 0, 0, 7, 0, 0, 2, 3, 2, &
       1, 0, 0, 8, 0, 0, 1, 4, 1, 2, 0, 0, 7, 0, 0, 2, 3, 2, 1, 4, 1, 8, 5, 8, 1, 4, 1, &
       0, 0, 4, 0, 0, 5, 4, 1, 4, 0, 0, 3, 0, 0, 6, 3, 2, 3, 4, 1, 4, 5, 8, 5, 4, 1, 4, &
       3, 2, 3, 6, 7, 6, 3, 2, 3, 0, 0, 4, 0, 0, 5, 4, 1, 4, 0, 0, 3, 0, 0, 6, 3, 2, 3   /

  noNeighbor = .false.

  if (inblkptr%ptr%LEV .eq.0) then
     !Found root cell, find neighbor root cell
     do i=1,proc_dims(1)
        do j=1,proc_dims(2)
           do k=1,proc_dims(3)
              if (associated(inblkptr%ptr,octree_roots(i,j,k)%ptr)) then
                 iRoot=i
                 jRoot=j
                 kRoot=k
              end if
           end do
        end do
     end do
     !Break idir into components
     ii=idir-1
     ix=-1
     iy=-1
     iz=-1
     do i=1,2
        if (ii .ge. 9) then
           ix = ix + 1
           ii = ii - 9
        end if
     end do
     do i=1,2
        if (ii .ge. 3) then
           iy = iy + 1
           ii = ii - 3
        end if
     end do
     do i=1,2
        if (ii .ge. 1) then
           iz = iz + 1
           ii = ii - 1
        end if
     end do
     ix = ix + iRoot
     iy = iy + jRoot
     iz = iz + kRoot

     !\
     ! periodic boudary conditions
     !/
     if(periodic3D(1).and.ix==0)              ix=proc_dims(1)
     if(periodic3D(1).and.ix==proc_dims(1)+1) ix=1
     if(periodic3D(2).and.iy==0)              iy=proc_dims(2)
     if(periodic3D(2).and.iy==proc_dims(2)+1) iy=1
     if(periodic3D(3).and.iz==0)              iz=proc_dims(3)
     if(periodic3D(3).and.iz==proc_dims(3)+1) iz=1

     if ( ix .ge. 1 .and. ix .le.proc_dims(1) .and. &
          iy .ge. 1 .and. iy .le.proc_dims(2) .and. &
          iz .ge. 1 .and. iz .le.proc_dims(3) ) then
        outblkptr%ptr => octree_roots(ix,iy,iz)%ptr
     else
        noNeighbor = .true.
     end if
  else
     iFoundIt = isFound(idir,inblkptr%ptr%child_number)
     select case (iFoundIt)
     case (1,2,3,4,5,6,7,8)
        tmp1blkptr%ptr => inblkptr%ptr%parent%ptr
        
        outblkptr%ptr => tmp1blkptr%ptr%child(iFoundIt)%ptr
     case default
        idirNext = isNext(idir,inblkptr%ptr%child_number)
        idirDown = isDown(idir,inblkptr%ptr%child_number)
        tmp1blkptr%ptr => inblkptr%ptr%parent%ptr
        if (associated(tmp1blkptr%ptr)) then ! ensure block is allocated
           call findTreeNeighbor(tmp1blkptr,tmp2blkptr,idirNext,noNeighbor)
           if (.not. noNeighbor) then
              if (associated(tmp2blkptr%ptr)) then ! ensure block is allocated
                 select case (idirDown)
                 case (1,2,3,4,5,6,7,8)
                    
                    outblkptr%ptr => tmp2blkptr%ptr%child(iDirDown)%ptr
                    if (.not. associated(outblkptr%ptr)) then
                       !neighbor must be at coarser level keep previous block
                       outblkptr%ptr => tmp2blkptr%ptr
                    end if
                 case default
                    write (*,*) 'ERROR: findTreeNeighbor: 4: no direction down tree '
                    !                    stop
                 end select
              else
                 write (*,*) 'ERROR: findTreeNeighbor: 3: not associated '
                 !                 stop
              end if
           end if
        else
           write (*,*) 'ERROR: findTreeNeighbor: 2: not associated - parent%ptr'
           !           stop
        end if
     end select
     if (.not. noNeighbor) then
        if (.not. associated(outblkptr%ptr)) then
           write (*,*) 'ERROR: findTreeNeighbor: 1: not associated '
           !           stop
        end if
     end if
  end if
end subroutine findTreeNeighbor

subroutine fixRefinementLevels
  use ModProcMH
  use ModMain, ONLY : nBLK,lVerbose
  use ModAMR, ONLY : refine_list
  use ModGeometry,ONLY: is_axial_geometry                
  use ModOctree
  implicit none

  integer :: i,j,k, idir, inPE,inBLK, iLEV1,iLEV2, iCount, maxLev, curLev
  logical :: noNeighbor
  type (adaptive_block_ptr) :: inBlockPtr,outBlockPtr,tmpBlockPtr

  call set_body_flag
  do
     iCount = 0
     refine_list = .false.
     !find max level
     maxLev=0
     do inPE = 1,nProc
        do inBLK = 1,nBLK
           inBlockPtr%ptr => global_block_ptrs(inBLK,inPE)%ptr
           if (associated(inBlockPtr%ptr)) &
                maxLev = max(maxLev, inBlockPtr%ptr%LEV)
        end do
     end do
     !loop from max level down to zero
     do curLev = maxLev,1,-1
        do inPE=1,nProc; do inBLK=1,nBLK
           inBlockPtr%ptr => global_block_ptrs(inBLK,inPE)%ptr
           if (associated(inBlockPtr%ptr)) then
              if (inBlockPtr%ptr%LEV == curLev) then
                 iLEV1 = inBlockPtr%ptr%LEV
                 if (refine_list(inBlockPtr%ptr%BLK,inBlockPtr%ptr%PE+1)) &
                      iLEV1 = iLEV1 + 1
                 do i=-1,1; do j=-1,1; do k=-1,1
                    idir = 1+ (k+1) + 3*(j+1) + 9*(i+1)
                    call findTreeNeighbor(inBlockPtr,outBlockPtr,idir,noNeighbor)
                    if (.not. noNeighbor)then
                       if(associated(outBlockPtr%ptr)) then
                          if (.not. associated(outBlockPtr%ptr%child(1)%ptr)) then
                             iLEV2 = outBlockPtr%ptr%LEV
                             if (refine_list(outBlockPtr%ptr%BLK,outBlockPtr%ptr%PE+1)) &
                                  iLEV2 = iLEV2 + 1
                             if ( (iLEV1-iLEV2 > 1) .or. &
                                  (iLEV1-iLEV2 > 0 .and. &
                                  ((inBlockPtr%ptr%body .and. outBlockPtr%ptr%body)&
                                  .or.(inBlockPtr%ptr%IsOuterBoundary .and. &    
                                  outBlockPtr%ptr%IsOuterBoundary)&
                                  .or.(inBlockPtr%ptr%IsExtraBoundaryOrPole .and.& 
                                  outBlockPtr%ptr%IsExtraBoundaryOrPole &
                                  .and.( (i==0.and.k==0).or.(.not.is_axial_geometry()))& 
                                  )&          
                                  ))) then
                                refine_list(outBlockPtr%ptr%BLK,outBlockPtr%ptr%PE+1) = .true.
                                iCount = iCount +1
                             end if
                          end if
                       end if
                    end if
                 end do; end do; end do
              end if
           end if
        end do; end do
     end do
     if (iCount == 0) EXIT
     if (iProc == 0.and.lVerbose>0)&
          write (*,*) '    FixRefinementLevels found ',iCount,' blocks to refine'
     call parallel_refine
     call set_body_flag
  end do

end subroutine fixRefinementLevels
