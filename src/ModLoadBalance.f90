!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModLoadBalance

  use BATL_lib, ONLY: &
       test_start, test_stop, StringTest, iBlockTest, iProcTest, &
       iProc, iComm, sync_cpu_gpu_amr
  use ModBatsrusUtility, ONLY: stop_mpi

  use ModMain, ONLY: UseConstrainB, UseB0, UseIM
  use BATL_size, ONLY: nI, nJ, nK, nIJK, &
       MinI, MaxI, MinJ, MaxJ, MinK, MaxK, MaxIJK
  use ModBlockData, ONLY: MaxBlockData, get_block_data, put_block_data, &
       n_block_data, use_block_data, set_block_data, clean_block_data
  use ModImplicit, ONLY: UseImplicit, UseBDF2, nStepPrev, ImplOld_VCB
  use ModPointImplicit, ONLY: UseUserPointImplicit_B, &
       DoBalancePointImplicit, IsDynamicPointImplicit
  use ModConstrainDivB, ONLY: BxFace_GB, ByFace_GB, BzFace_GB
  use ModFieldTrace, ONLY: Trace_DSNB
  use ModAdvance, ONLY: nVar
  use ModB0, ONLY: B0_DGB
  use ModIO, ONLY: StringLogVar

  implicit none

  SAVE

  private ! except

  public:: init_load_balance
  public:: load_balance
  public:: load_balance_blocks
  public:: select_stepping

  ! Upper estimate on the size of buffer to be sent or received
  integer, public:: nBuffer

  ! We should switch to these variables instead of _BP indexes !!!
  integer, allocatable, public:: iTypeBalance_A(:)

  integer, allocatable:: iTypeAdvance_A(:)

  ! Local variables

  ! This could be public, so one can tell if we need to recalculate stuff
  logical, parameter :: DoMoveExtraData = .false.

  ! Number of scalars passed in the buffer
  integer, parameter :: nScalarData = 2

  ! Send field trace info?
  logical:: DoSendTrace

  ! The index of block type features.
  integer, parameter:: &
       iNotSkippedBlock     =  1, & ! Skipped block or not.
       iTrueBlock           =  2    ! True block or not.
  integer :: &
       iPartImplBlock       = -1, & ! Implicit or explicit.
       iSemiImplBlock       = -1, & ! Semi-implicit or not.
       iPointImplBlock      = -1, & ! Point-implicit or not.
       iFieldLineThreadBlock= -1, & ! Threaded file line BC or not.
       iPicBlock            = -1, & ! Overlaped with PIC or not.
       iSteadyBlock         = -1, & ! Steady block or not.
       iHighOrderBlock      = -1, & ! Use high-order scheme or not.
       iUserTypeBlock       = -1, & ! User defined block types
       iSubCycleBlock       = -1    ! First bit of time level info

contains
  !============================================================================
  subroutine init_load_balance

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_load_balance'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    ! to be improved?
    DoSendTrace = UseIM .or. index(StringLogVar, 'status') > 0

    nBuffer = nScalarData

    if(UseConstrainB) nBuffer = nBuffer + 3*MaxIJK
    if(DoSendTrace) &
         nBuffer = nBuffer + 6*nIJK
    if(UseImplicit .and. UseBDF2 .and. nStepPrev > 0) &
         nBuffer = nBuffer + nVar*nIJK
    if(DoMoveExtraData)then
       if(UseB0) nBuffer = nBuffer + 3*MaxIJK
    end if
    if(MaxBlockData > 0) &
         nBuffer = nBuffer + MaxBlockData

    call test_stop(NameSub, DoTest)
  end subroutine init_load_balance
  !============================================================================
  subroutine pack_load_balance(iBlock, nBuffer, Buffer_I)

    integer, intent(in) :: iBlock
    integer, intent(in) :: nBuffer
    real,    intent(out):: Buffer_I(nBuffer)

    integer:: i, j, k, i1, i2, iVar, iData, nDynamicData

    ! Amount of user defined data for this block
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'pack_load_balance'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    nDynamicData = 0
    if(use_block_data(iBlock)) nDynamicData = n_block_data(iBlock)
    if(nDynamicData > MaxBlockData)then
       write(*,*)NameSub,' ERROR: iBlock, nDynamicData=', iBlock, nDynamicData
       write(*,*)NameSub,' MaxBlockData=',MaxBlockData,' is too small!'
       call stop_mpi(NameSub//': MaxBlockData has to be set in ModUser')
    end if

    Buffer_I(1) = real(nDynamicData)

    if(DoBalancePointImplicit)then
       if(UseUserPointImplicit_B(iBlock))then
          Buffer_I(2) = 1.0
       else
          Buffer_I(2) = 0.0
       end if
    end if

    iData = nScalarData

    if (UseConstrainB) then

       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          iData = iData + 1
          Buffer_I(iData) = BxFace_GB(i,j,k,iBlock)
          iData = iData + 1
          Buffer_I(iData) = ByFace_GB(i,j,k,iBlock)
          iData = iData + 1
          Buffer_I(iData) = BzFace_GB(i,j,k,iBlock)
       end do; end do; end do

    endif

    if(DoMoveExtraData)then
       if(UseB0)then
          ! Cell centered B0_DGB
          do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
             Buffer_I(iData+1:iData+3) = B0_DGB(:,i,j,k,iBlock)
             iData = iData + 3
          end do; end do; end do
       end if

    end if ! DoMoveExtraData

    if(UseImplicit .and. UseBDF2 .and. nStepPrev > 0)then
       do k = 1, nK; do j = 1, nJ; do i = 1, nI; do iVar = 1, nVar
          iData = iData + 1
          Buffer_I(iData) = ImplOld_VCB(iVar,i,j,k,iBlock)
       end do; end do; end do; end do
    end if

    if(DoSendTrace)then
       do k = 1, nK; do j = 1, nJ; do i = 1, nI; do i2 = 1, 2; do i1 = 1, 3
          iData = iData + 1
          Buffer_I(iData) = Trace_DSNB(i1,i2,i,j,k,iBlock)
       end do; end do; end do; end do; end do
    end if

    if(nDynamicData > 0)then
       call get_block_data(iBlock, nDynamicData, &
            Buffer_I(iData+1:iData+nDynamicData))
       iData = iData + nDynamicData
       call clean_block_data(iBlock)
    endif

    if(iData > nBuffer)then
       write(*,*)'ERROR in load_balance: iData=',iData,&
            ' > nBuffer=',nBuffer
       call stop_mpi(NameSub//'load_balnce: increase nBuffer')
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine pack_load_balance
  !============================================================================
  subroutine unpack_load_balance(iBlock, nBuffer, Buffer_I)

    integer, intent(in):: iBlock
    integer, intent(in):: nBuffer
    real,    intent(in):: Buffer_I(nBuffer)

    integer:: i, j, k, i1, i2, iVar, iData, nDynamicData

    ! Amount of user defined data for this block
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'unpack_load_balance'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    nDynamicData = nint(Buffer_I(1))

    if(DoBalancePointImplicit) &
         UseUserPointImplicit_B(iBlock) = Buffer_I(2) > 0.5

    ! Read rest of the blockData buffer
    iData = nScalarData

    if (UseConstrainB) then
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          iData = iData + 1
          BxFace_GB(i,j,k,iBlock) = Buffer_I(iData)
          iData = iData + 1
          ByFace_GB(i,j,k,iBlock) = Buffer_I(iData)
          iData = iData + 1
          BzFace_GB(i,j,k,iBlock) = Buffer_I(iData)
       end do; end do; end do
    end if

    if(DoMoveExtraData)then
       if(UseB0)then
          ! Cell centered B0_DGB
          do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
             B0_DGB(:,i,j,k,iBlock) = Buffer_I(iData+1:iData+3)
             iData = iData + 3
          end do; end do; end do
       end if
    end if ! DoMoveExtraData

    if(UseImplicit .and. UseBDF2 .and. nStepPrev > 0)then
       do k = 1, nK; do j = 1, nJ; do i = 1, nI; do iVar = 1, nVar
          iData = iData + 1
          ImplOld_VCB(iVar,i,j,k,iBlock) = Buffer_I(iData)
       end do; end do; end do; end do
    end if

    if(DoSendTrace)then
       do k = 1, nK; do j = 1, nJ; do i = 1, nI; do i2 = 1, 2; do i1 = 1, 3
          iData = iData + 1
          Trace_DSNB(i1,i2,i,j,k,iBlock) = Buffer_I(iData)
       end do; end do; end do; end do; end do
    end if

    if(nDynamicData > 0)then
       call put_block_data(iBlock, nDynamicData, &
            Buffer_I(iData+1:iData+nDynamicData))
       call set_block_data(iBlock)
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine unpack_load_balance
  !============================================================================
  subroutine load_balance(DoMoveCoord, DoMoveData, IsNewBlock)

    use ModMain, ONLY: nBlockMax, UseFieldLineThreads, UseLocalTimeStep, &
         DtMax_B
    use ModImplicit, ONLY: UsePartImplicit, UseSemiImplicit, &
         TypeSemiImplicit, iBlockFromSemi_B, nBlockSemi
    use ModAdvance, ONLY: &
         State_VGB, iTypeAdvance_B, iTypeAdvance_BP, &
         SkippedBlock_, ImplBlock_, SteadyBlock_, &
         UseLowOrderRegion, IsLowOrderOnly_B
    use ModGeometry, ONLY: IsNoBody_B, IsBoundary_B
    use ModBoundaryGeometry, ONLY: fix_boundary_ghost_cells
    use ModPartSteady, ONLY: UsePartSteady
    use ModParallel
    use ModMpi
    use ModPIC, ONLY: UsePic, pic_find_active_node, IsActivePicNode_A, &
         DoBalanceActivePicBlock, pic_find_node, IsPicNode_A, DoBalancePicBlock
    use BATL_lib, ONLY: MaxNode, nNode, iTree_IA, Status_, Proc_, Block_, &
         Used_, nTimeLevel, iTimeLevel_A, &
         nBlock, iNode_B, Unused_BP, Used_GB, &
         regrid_batl, find_test_cell
    use ModBatlInterface, ONLY: set_batsrus_grid, set_batsrus_state
    use ModTimeStepControl, ONLY: UseMaxTimeStep, DtMax, DtMin
    use ModUserInterface ! user_action, i_type_block_user

    ! Load balance grid using space filling (Morton) ordering of blocks
    ! Coordinates are moved if DoMoveCoord is true.
    ! Data is moved with the blocks if DoMoveData is true.
    ! There are new blocks (due to initial refinement, restart or AMR)
    ! when IsNewBlock is true (so update neighbors etc).

    logical, optional, intent(in) :: DoMoveCoord, DoMoveData, IsNewBlock

    ! Number of different block types
    integer :: nType, nTypeUser

    integer :: iError
    integer:: iNode, iBlock

    ! Index for block types
    integer:: iType, iCrit, iTypeMax, nCount

    ! Conversion from iTypeBalance to iType
    integer, allocatable:: iType_I(:)

    logical, allocatable:: IsTypeExist_I(:)

    integer, allocatable:: iTypeConvert_I(:) ! reverse conversion

    logical :: IsSemiImplBlock_B(MaxBlock)
    integer :: i
    integer, allocatable:: nTypeProc_PI(:,:)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'load_balance'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    call timing_start(NameSub)
    if(DoTest)write(*,*) NameSub, &
         ' starting with DoMoveCoord,DoMoveData,IsNewBlock=', &
         DoMoveCoord, DoMoveData, IsNewBlock

    call select_stepping(DoMoveCoord)

    if (nProc > 1 .and. index(StringTest,'NOLOADBALANCE') < 1) then
       if(.not.allocated(iTypeBalance_A)) allocate(iTypeBalance_A(MaxNode))
       iTypeBalance_A = 0

       if(DoMoveCoord)then
          IsSemiImplBlock_B = .false.
          if(TypeSemiImplicit(1:6) == 'resist' .and. nBlockSemi >= 0) &
               IsSemiImplBlock_B(iBlockFromSemi_B(1:nBlockSemi)) = .true.

          if(UsePic) then
             if(DoBalancePicBlock .and. IsNewBlock) then
                call pic_find_node
             else if(DoBalanceActivePicBlock) then
                call pic_find_active_node
             endif
          endif

          ! N criteria are used to decide the type of a block.
          ! Each criterion determines the value of one bit (0 or 1).
          ! For N criteria, the iType of a block could be -1 or a value
          ! between 1 and 2^N-1 (the first bit SkippedBlock is special).
          ! In a typical case only some of the possible combinations occur.
          !

          ! 1st  bit: NotSkippedBlock   -> 1, otherwise -> iType will be -1.
          ! 2nd  bit: IsNoBody_B          -> 1, otherwise -> 0
          iCrit = 2

          ! next bit: implicit          -> 1, explicit  -> 0
          if(UsePartImplicit)then
             iCrit = 2*iCrit
             iPartImplBlock = iCrit
          end if

          ! next bit: semi-implicit     -> 1, otherwise -> 0
          if(UseSemiImplicit)then
             iCrit = 2*iCrit
             iSemiImplBlock = iCrit
          end if

          ! next bit: point-implicit     -> 1, otherwise -> 0
          if(DoBalancePointImplicit)then
             iCrit = 2*iCrit
             iPointImplBlock = iCrit
          end if

          ! next bit: threaded field BC -> 1, otherwise -> 0
          if(UseFieldLineThreads)then
             iCrit = 2*iCrit
             iFieldLineThreadBlock = iCrit
          end if

          ! next bit: PIC-block         -> 1, otherwise -> 0
          if(UsePic .and. &
               (DoBalancePicBlock .or. DoBalanceActivePicBlock))then
             iCrit = 2*iCrit
             iPicBlock = iCrit
          endif

          ! next bit: steady block      -> 1, otherwise -> 0
          if(UsePartSteady)then
             iCrit = 2*iCrit
             iSteadyBlock = iCrit
          end if

          ! next bit: high-order scheme -> 1, otherwise -> 0
          if(UseLowOrderRegion)then
             ! High-order scheme block: some faces use high-order scheme
             iCrit = 2*iCrit
             iHighOrderBlock = iCrit
          end if

          ! Set number of user defined block types (no iBlock argument)
          nTypeUser = i_type_block_user()
          if(nTypeUser > 0)then
             ! User defined block types
             iUserTypeBlock = 2*iCrit    ! first bit of the user types
             iCrit = 2**nTypeUser*iCrit  ! max bit of the user types
          end if

          ! Maximum possible value is when all the bits are 1 from 0 to iCrit.
          iTypeMax = max(1, 2*iCrit - 1)

          ! Time level uses the last nTimeLevel bits
          if(UseMaxTimeStep .and. DtMax > DtMin .and. DtMin > 0)then

             ! First bit used by time levels
             iSubCycleBlock = 2*iCrit

             ! Maximum type
             iTypeMax = iTypeMax + iSubCycleBlock*nTimeLevel

          end if

          ! Arrays to store indexes for different block types
          allocate(iType_I(0:iTypeMax), IsTypeExist_I(0:iTypeMax))

          do iBlock = 1, nBlock

             if(iTypeAdvance_B(iBlock) == SkippedBlock_) CYCLE

             ! used block has 1st bit set
             iType = iNotSkippedBlock

             ! true block has second bit set
             if(IsNoBody_B(iBlock)) iType = iType + iTrueBlock

             if(UsePartImplicit)then
                if(iTypeAdvance_B(iBlock)==ImplBlock_) &
                     iType = iType + iPartImplBlock
             end if

             if(UseSemiImplicit)then
                if(IsSemiImplBlock_B(iBlock)) iType = iType + iSemiImplBlock
             end if

             if(DoBalancePointImplicit)then
                if(UseUserPointImplicit_B(iBlock)) &
                     iType = iType + iPointImplBlock
             end if

             if(UseFieldLineThreads)then
                ! Field line threads are at the 1st boundary (minimum r)
                if(IsBoundary_B(iBlock) .and. DiLevel_EB(1,iBlock)==Unset_)&
                     iType = iType + iFieldLineThreadBlock
             end if

             if(UsePic)then
                if(DoBalancePicBlock) then
                   if(IsPicNode_A(iNode_B(iBlock))) &
                        iType = iType + iPicBlock
                else if(DoBalanceActivePicBlock) then
                   if(IsActivePicNode_A(iNode_B(iBlock))) &
                        iType = iType + iPicBlock
                end if
             endif

             if(UsePartSteady)then
                if(iTypeAdvance_B(iBlock)==SteadyBlock_) &
                     iType = iType + iSteadyBlock
             end if

             if(UseLowOrderRegion)then
                ! High-order scheme block: some faces use high-order scheme
                if(.not.IsLowOrderOnly_B(iBlock)) &
                     iType = iType + iHighOrderBlock
             end if

             if(nTypeUser > 0)then
                ! Add user type bit(s)
                iType = iType + iUserTypeBlock*i_type_block_user(iBlock)
             end if

             if(UseMaxTimeStep .and. DtMax > DtMin .and. DtMin > 0)then
                ! For subcycling the time step of the block determines
                ! which time level it belongs to. It can only belong
                ! to one time level, so the type is the time level
                ! times a constant iSubCycleBlock that leaves enough bits
                ! for the other block type criteria.
                iType = iType + iSubCycleBlock*iTimeLevel_A(iNode_B(iBlock))

                if(DtMax_B(iBlock) < DtMin .or. iType > iTypeMax)then
                   write(*,*) NameSub,' ERROR for iBlock, iProc=', &
                        iBlock, iProc
                   write(*,*) NameSub,'iType, iTypeMax =', iType, iTypeMax
                   write(*,*) NameSub,' iSubCycleBlock =', iSubCycleBlock
                   write(*,*) NameSub,' DtMin, DtMax   =', DtMin, DtMax
                   write(*,*) NameSub,' DtMax_B         =', DtMax_B(iBlock)
                   write(*,*) NameSub,' time level     =', &
                        iTimeLevel_A(iNode_B(iBlock))
                end if
             end if

             iTypeBalance_A(iNode_B(iBlock)) = iType
          enddo
          ! Update iTypeAdvance_BP
          ! CHEATING: only indicate first index to circumvent ModMpiInterfaces
          ! Set displacement equal to MaxBlock so we get same behavior
          ! as MPI_allgather. Use nBlockMax for maximum receive data for speed
          nBlockMax_P = nBlockMax
          call MPI_allgatherv(iTypeAdvance_B(1), nBlockMax, MPI_INTEGER, &
               iTypeAdvance_BP(1,0), nBlockMax_P, MaxBlockDisp_P,&
               MPI_INTEGER, iComm, iError)

          ! In order to do MPI_allreduce with MPI_SUM, the default value of
          ! iTypeBalance_A=0. The skipped blocks also have 0 type value.
          if(nProc > 1)&
               call MPI_allreduce(MPI_IN_PLACE, iTypeBalance_A, MaxNode, &
               MPI_INTEGER, MPI_SUM, iComm, iError)

          ! Find all different block types that occur
          IsTypeExist_I = .false.
          do iNode = 1, nNode
             if(iTypeBalance_A(iNode) >= 0)&
                  IsTypeExist_I(iTypeBalance_A(iNode)) = .true.
          enddo

          ! Count the number of different block types that occur and
          ! create an indirect index from the full range of possible types.
          iType_I = -1
          nType = 0
          do iType = 1, iTypeMax
             if(IsTypeExist_I(iType)) then
                nType = nType + 1
                iType_I(iType) = nType
             endif
          enddo
       end if

       ! Convert from block/proc index to node index.
       allocate(iTypeAdvance_A(MaxNode))
       if(DoMoveCoord) then
          do iNode = 1, nNode
             if(iTree_IA(Status_,iNode) /= Used_) CYCLE
             iTypeAdvance_A(iNode) = &
                  iTypeAdvance_BP(iTree_IA(Block_,iNode),iTree_IA(Proc_,iNode))
             iTypeBalance_A(iNode) = iType_I(iTypeBalance_A(iNode))
          end do
       else
          iTypeBalance_A = 1
          iTypeAdvance_A = 1
       endif

       ! load balance depending on block types
       if(DoMoveData)then
          call init_load_balance

          call regrid_batl(nVar, State_VGB, DtMax_B,  &
               DoBalanceEachLevelIn= &
               (UseLocalTimeStep .and. .not.UseMaxTimeStep), &
               iTypeBalance_A=iTypeBalance_A,        &
               iTypeNode_A=iTypeAdvance_A,           &
               DoBalanceOnlyIn=.true.,               &
               nExtraData=nBuffer,                   &
               pack_extra_data=pack_load_balance,    &
               unpack_extra_data=unpack_load_balance,&
               DoTestIn=DoTest)

          call set_batsrus_grid
          call set_batsrus_state
       else
          call regrid_batl(nVar, State_VGB, DtMax_B, Used_GB=Used_GB, &
               iTypeBalance_A=iTypeBalance_A, iTypeNode_A=iTypeAdvance_A,&
               DoTestIn=DoTest)
          call set_batsrus_grid
       end if

       if(.not.IsNewBlock) call fix_boundary_ghost_cells

       ! Go up to nBlockMax instead of nBlock (! ) to clean up beyond nBlock
       iTypeAdvance_BP(1:nBlockMax,:) = SkippedBlock_

       do iNode = 1, nNode
          if(iTree_IA(Status_,iNode) /= Used_) CYCLE
          iTypeAdvance_BP(iTree_IA(Block_,iNode),iTree_IA(Proc_,iNode)) = &
               iTypeAdvance_A(iNode)
       end do
       iTypeAdvance_B(1:nBlockMax)  = iTypeAdvance_BP(1:nBlockMax,iProc)

       if(DoTest .and. nType > 0 .and. allocated(iType_I)) then
          ! Create the table so that we can check the status of a block.
          allocate(iTypeConvert_I(nType), nTypeProc_PI(0:nProc-1,nType))
          nCount = 1
          do iType = 1, iTypeMax
             if(iType_I(iType) > 0) then
                iTypeConvert_I(nCount) = iType
                nCount = nCount + 1
             endif
          enddo

          nTypeProc_PI = 0
          do iNode = 1, nNode
             if(iTree_IA(Status_,iNode) /= Used_) CYCLE
             nTypeProc_PI(iTree_IA(Proc_,iNode),iTypeBalance_A(iNode)) = &
                  nTypeProc_PI(iTree_IA(Proc_,iNode),iTypeBalance_A(iNode)) &
                  + 1
          end do
          write(*,*) NameSub,' nType, iSubCycleBlock=', nType, iSubCycleBlock
          do i = 1, nType
             write(*,*) 'iType= ',iTypeConvert_I(i), &
                  ' nCount= ',sum(nTypeProc_PI(:,i))
          enddo
          deallocate(nTypeProc_PI, iTypeConvert_I)
       endif ! DoTest

       deallocate(iTypeAdvance_A)
    end if

    ! When load balancing is done Skipped and Unused blocks coincide
    Unused_BP(1:nBlockMax,:) = &
         iTypeAdvance_BP(1:nBlockMax,:) == SkippedBlock_

    ! Update variables on the GPU
    call sync_cpu_gpu_amr
    !$acc update device(DtMax_B)

    call find_test_cell

    if(allocated(iType_I)) deallocate(iType_I, IsTypeExist_I)

    call timing_stop(NameSub)

    ! Allow the user to do something after load balancing was done
    if (nProc>1 .and. index(StringTest,'NOLOADBALANCE') < 1 .and. DoMoveData)&
         call user_action('load balance done')

    call test_stop(NameSub, DoTest)
  end subroutine load_balance
  !============================================================================
  subroutine select_stepping(DoPartSelect)

    ! Set logical arrays for implicit blocks,
    ! set number of implicit and explicit blocks,
    ! and if DoPartSelect is true then select explicit and implicit blocks
    ! based on the stepping selection criteria.

    use ModMain
    use ModFaceFlux, ONLY: calc_face_flux
    use ModFaceValue, ONLY: calc_face_value
    use ModAdvance, ONLY: iTypeAdvance_B, iTypeAdvance_BP, &
         SkippedBlock_, ExplBlock_, ImplBlock_
    use ModGeometry, ONLY: rMin_B
    use ModImplicit, ONLY: UseImplicit, UseFullImplicit, UsePartImplicit, &
         TypeImplCrit, ExplCFL, rImplicit
    use ModIO, ONLY: write_prefix, iUnitOut
    use ModB0, ONLY: set_b0_face
    use ModMpi
    use ModParallel, ONLY: nBlockMax_P, MaxBlockDisp_P
    use ModTimeStepControl, ONLY: calc_timestep

    logical, intent(in) :: DoPartSelect

    integer :: iBlock, iError

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'select_stepping'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(DoTest)then
       write(*,*) 'select_stepping starting with iProc ',&
            'UseFullImplicit, UsePartImplicit, DoPartSelect=',&
            iProc, UseFullImplicit, UsePartImplicit, DoPartSelect
       write(*,*)'select_stepping starting with ',&
            'iProc,nBlockExpl, nBlockImpl, nBlockSkipped=', iProc, &
            count(iTypeAdvance_B == ExplBlock_),&
            count(iTypeAdvance_B == ImplBlock_),&
            count(iTypeAdvance_B == SkippedBlock_)
       write(*,*)'select_stepping starting with ',&
            'iProc,nBlockExplALL, nBlockImplALL, nBlockSkippedALL=', iProc, &
            count(iTypeAdvance_BP == ExplBlock_),&
            count(iTypeAdvance_BP == ImplBlock_),&
            count(iTypeAdvance_BP == SkippedBlock_)
       write(*,*)'select_stepping starting with ',&
            'iProc,min(advance),max(advance)=',iProc, &
            minval(iTypeAdvance_B),maxval(iTypeAdvance_B)
       write(*,*)'select_stepping starting with ',&
            'iProc,min(advanceall),max(advanceall)=',iProc, &
            minval(iTypeAdvance_BP),maxval(iTypeAdvance_BP)
    end if

    if(UseFullImplicit)then
       nBlockExplALL = 0
       nBlockImplALL = nBlockALL
       where(iTypeAdvance_BP(1:nBlockMax,:) /= SkippedBlock_) &
            iTypeAdvance_BP(1:nBlockMax,:) = ImplBlock_
       iTypeAdvance_B(1:nBlockMax) = iTypeAdvance_BP(1:nBlockMax,iProc)

    elseif((UsePartImplicit .and. .not. DoPartSelect) &
         .or. .not. UseImplicit)then
       nBlockExplALL    = nBlockALL
       nBlockImplALL    = 0
       where(iTypeAdvance_BP(1:nBlockMax,:) == ImplBlock_) &
            iTypeAdvance_BP(1:nBlockMax,:) = ExplBlock_
       iTypeAdvance_B(1:nBlockMax) = iTypeAdvance_BP(1:nBlockMax,iProc)

    else
       ! First set all blocks to be explicit
       where(iTypeAdvance_B(1:nBlockMax) /= SkippedBlock_) &
            iTypeAdvance_B(1:nBlockMax) = ExplBlock_

       ! Select implicitly treated blocks
       select case(TypeImplCrit)
       case('dt')
          ! Just checking
          if(.not.IsTimeAccurate)call stop_mpi(&
               'TypeImplCrit=dt is only valid in IsTimeAccurate mode')

          ! Set implicitBLK based on the time step.
          do iBlock = 1, nBlockMax
             if(Unused_B(iBlock)) CYCLE

             ! Obtain the time step based on CFL condition

             ! For first iteration calculate DtMax_B when inside time loop,
             ! otherwise use the available DtMax_B from previous time step,
             ! or from the restart file, or simply 0 set in read_inputs.
             ! The latter two choices will be overruled later anyways.
             if(nStep == 1 .and. IsTimeLoop)then
                ! For first iteration in the time loop
                ! calculate stable time step
                call set_b0_face(iBlock)
                call calc_face_value(iBlock, DoResChangeOnly=.false.)
                call calc_face_flux(.false., iBlock)
                call calc_timestep(iBlock)
             end if

             ! If the smallest allowed timestep is below the fixed DtFixed
             ! then only implicit scheme will work
             if(DtMax_B(iBlock)*explCFL <= DtFixed) &
                  iTypeAdvance_B(iBlock) = ImplBlock_
          end do

          if(DoTest)write(*,*)&
               'SELECT: advancetype,DtMax_B,explCFL,dt=',&
               iTypeAdvance_B(iBlockTest), DtMax_B(iBlockTest), ExplCFL, Dt

       case('r','R')
          ! implicitly treated blocks are within rImplicit and not Unused
          where(rMin_B(1:nBlockMax) <= rImplicit .and. &
               .not.Unused_B(1:nBlockMax)) &
               iTypeAdvance_B(1:nBlockMax) = ImplBlock_
       case('test')
          if(iProc==iProcTest) iTypeAdvance_B(iBlockTest) = ImplBlock_
       end select

       ! Gather global information
       ! CHEATING: only indicate first index to circumvent ModMpiInterfaces.
       ! Set displacement equal to MaxBlock so we get same behavior
       ! as MPI_allgather. Use nBlockMax for maximum receive data for speed!
       nBlockMax_P = nBlockMax
       call MPI_allgatherv(iTypeAdvance_B(1), nBlockMax, MPI_INTEGER, &
            iTypeAdvance_BP(1,0), nBlockMax_P, MaxBlockDisp_P,&
            MPI_INTEGER, iComm, iError)

       nBlockImplALL = count(iTypeAdvance_BP(1:nBlockMax,:) == ImplBlock_)
       nBlockExplALL = count(iTypeAdvance_BP(1:nBlockMax,:) == ExplBlock_)

       if(iProc==0.and.lVerbose>0)then
          call write_prefix;
          write(iUnitOut,*)'select_stepping: nBlockExplALL, nBlockImplALL=',&
               nBlockExplALL,nBlockImplALL
       end if

    end if
    if(DoTest)then
       write(*,*)'select_stepping finished with ',&
            'iProc,nBlockExpl, nBlockImpl, nBlockSkipped=', iProc, &
            count(iTypeAdvance_B == ExplBlock_),&
            count(iTypeAdvance_B == ImplBlock_),&
            count(iTypeAdvance_B == SkippedBlock_)
       write(*,*)'select_stepping finished with ',&
            'iProc,nBlockExplALL, nBlockImplALL, nBlockSkippedALL=', iProc, &
            count(iTypeAdvance_BP == ExplBlock_),&
            count(iTypeAdvance_BP == ImplBlock_),&
            count(iTypeAdvance_BP == SkippedBlock_)
       write(*,*)'select_stepping finished with ',&
            'iProc,min(advance),max(advance)=',iProc, &
            minval(iTypeAdvance_B),maxval(iTypeAdvance_B)
       write(*,*)'select_stepping finished with ',&
            'iProc,min(advanceall),max(advanceall)=',iProc, &
            minval(iTypeAdvance_BP),maxval(iTypeAdvance_BP)
    end if
    call test_stop(NameSub, DoTest)
  end subroutine select_stepping
  !============================================================================
  subroutine load_balance_blocks

    use ModMain, ONLY: nIteration
    use ModImplicit, ONLY: UsePartImplicit, nBlockSemi, IsDynamicSemiImpl
    use ModPartSteady, ONLY: UsePartSteady, IsNewSteadySelect
    use ModTimeStepControl, ONLY: UseMaxTimeStep
    use ModPIC, ONLY: DoBalanceActivePicBlock, UsePic

    ! Local variables
    logical:: DoBalanceSemiImpl = .true.

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'load_balance_blocks'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Select and load balance blocks
    if(  UseMaxTimeStep .or.                              &! subcycling scheme
         UsePartImplicit .or.                             &! part implicit
         UsePartSteady .and. IsNewSteadySelect .or.       &! part steady scheme
         nBlockSemi >= 0 .and. DoBalanceSemiImpl .or.     &! semi-implicit
         DoBalancePointImplicit .and. nIteration>1 .or.   &! point-implicit
         UsePic .and. DoBalanceActivePicBlock             &! PIC region
         ) then

       ! Redo load balancing
       call load_balance(DoMoveCoord=.true., DoMoveData=.true., &
            IsNewBlock=.false.)

       IsNewSteadySelect = .false.

       ! Repeated semi implicit load balancing is only needed if the
       ! semi-implicit condition is changing dynamically.
       DoBalanceSemiImpl = IsDynamicSemiImpl

       ! Repeated point implicit load balancing is only needed if the
       ! semi-implicit condition is changing dynamically.
       DoBalancePointImplicit = IsDynamicPointImplicit
    end if

    call test_stop(NameSub, DoTest)
  end subroutine load_balance_blocks
  !============================================================================
end module ModLoadBalance
!==============================================================================
