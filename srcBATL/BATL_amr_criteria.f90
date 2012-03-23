
!^CFG COPYRIGHT UM

module BATL_amr_criteria


  use BATL_tree, ONLY: nDesiredRefine,nNodeRefine,nDesiredCoarsen, &
       nNodeCoarsen, DoStrictAmr, iRank_A, Rank_A, nNodeSort


  implicit none

  SAVE

  private ! except

  public set_amr_criteria
  public clean_amr_criteria
  public read_amr_criteria
  public test_amr_criteria
  public calc_error_amr_criteria
  public restrict_amr_criteria
  public init_amr_criteria

  ! Choosing with blocks we want to refine is based on a list of criteria 
  ! and a set of upper (refine)  and lower (coarsen) limits. The criteria 
  ! can be external or calculated internally by estimating the 
  ! numerical errors (calc_error_amr_criteria) based on the state variables.
  ! 
  integer, public            :: nAmrCrit = 0
  integer                    :: nAmrCritUsed = 0
  real, public, allocatable  :: AmrCrit_IB(:,:)

  ! DoSortAmrCrit : true/false for generating the priority list iRank_A
  ! DoSoftAmrCrit : standard behavior (false) is that criteria has to be 
  ! refined/coarsened or we will stop the program. If true we will 
  ! refine/coarsen them as much as we can, this is the default behavior for
  ! amr by percentage.
  logical, public :: &
       DoSortAmrCrit = .false., &
       DoSoftAmrCrit = .false., &
       DoAutoAmr     = .false.

  ! Try to make geometric dependence for refinement/coarsening
  logical,public :: &
       DoGeometryAmr = .false., &
       DoCritAmr     = .false.

  ! Percentage with want refined and coarsen
  real ::  PercentRefine=0.0, PercentCoarsen=0.0

  ! Threshold limits for refine or unrefined the grid (length nCrit) 
  real, allocatable, dimension(:)    :: &
       CoarsenCrit_I, RefineCrit_I,&
       CoarsenCritAll_I, RefineCritAll_I

  ! Gives the max level for a criteria
  integer, allocatable:: MaxLevelCrit_I(:)

  integer, allocatable:: iVarCrit_I(:),iStateVarCrit_I(:) ! Index to variables or criterias

  ! Parameters used by calc_error_amr_criteria to estimate the errors
  real :: cAmrWavefilter = 1.0e-2
  real, parameter :: cEpsilon = 1.0d-8 ! avoid zero in denominator

  integer :: nExtCrit = 4 ! Number of External criteria, =4 from BATSRUS
  integer :: nExtCritUsed = 0 ! Number of External criteria actually used, 
  !4 :symmetry (BATSRUS)
  integer :: nIntCrit = 0 ! Number of internal criteria, 2nd order err estimate
  integer :: nAmrCritOld = -1

  ! How large the relative discrepancy in the Criteria for 
  ! symmetry points can be and still be refined in the same 
  ! way and the same for geometric variation
  real, parameter :: DeltaSymCrit = 1.0e-2
  real, parameter :: DeltaSymGeo  = 1.0e-6

  ! The max differens in the criteia to say thay have the same value,
  ! taking care of numerical fluctations
  real :: DeltaCritera = 1.0e-8

  ! Converting index form running block and proc number to node numbers
  integer, allocatable :: iNode_I(:)

  ! Maksing of areas which we do not want to use for deciding refinment.
  ! It will not hinder areas to be refinded if blocks neighbirs need to
  ! be refined.
  logical, public, allocatable :: DoAmr_GB (:,:,:,:)
  logical, public, allocatable :: DoAmr_B (:)
  logical, public :: UseAmrMask = .false.
  ! Gemetric cordinates for areas where we want dto do AMR, only rectagular
  ! AmrBox_DII([x,y,z],[min,max],[nAmrBox])
  integer :: nAmrBox=0
  real,allocatable :: AmrBox_DII(:,:,:)
contains
  !============================================================================

  subroutine init_amr_criteria
    
    use BATL_size, ONLY: MaxBlock,MinI, MaxI,MinJ, MaxJ, MinK, MaxK, nDim
    use BATL_tree, ONLY: Unused_B
    use BATL_grid, ONLY: CoordMin_D, CoordMax_D
    integer :: iBlock
    !-------------------------------------------------------------------------
  

    if(UseAmrMask) then
       if(allocated(DoAmr_GB)) deallocate(DoAmr_GB)
       allocate(DoAmr_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
       if(allocated(DoAmr_B)) deallocate(DoAmr_B)
       allocate(DoAmr_B(MaxBlock))
       DoAmr_GB = .false.
       DoAmr_B  = .false.

       ! Fill in for not used dimentions
       if(nDim < 2) then
          AmrBox_DII(2,1,:) = CoordMin_D(2)
          AmrBox_DII(2,2,:) = CoordMax_D(2)  
       end if

       if(nDim < 3) then
          AmrBox_DII(3,1,:) = CoordMin_D(3) 
          AmrBox_DII(3,2,:) = CoordMax_D(3)
       end if

       do iBlock=1,MaxBlock
          if(Unused_B(iBlock)) CYCLE
          call restrict_amr_criteria(iBlock)
       end do
    end if

  end subroutine init_amr_criteria
  !============================================================================

  subroutine set_amr_criteria(nVar, State_VGB, nInCritExtUsed, CritExt_IB, &
       CoarsenCritExt_I, RefineCritExt_I, Used_GB)

    use BATL_size, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK,&
         MaxBlock,nBlock
    use BATL_tree, ONLY: Unused_B
    use BATL_mpi, ONLY: iProc

    integer,  intent(in)  :: nVar
    real,    intent(in):: &                 ! state variables
         State_VGB(nVar,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    integer, intent(in), optional :: nInCritExtUsed
    real, intent(in), optional :: CritExt_IB(nExtCrit, nBlock)
    real, intent(in), optional :: CoarsenCritExt_I(nExtCrit),&
         RefineCritExt_I(nExtCrit)
    logical, intent(in), optional:: &
         Used_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)

    integer :: iCrit, iBlock
    !-----------------------------------------------------------------------
    if(DoGeometryAmr) &
         call CON_stop("set_amr_criteria :: DoGeometryAmr not implemented")


    !-------------- Setting number of criteria we are working with ----------
    nExtCritUsed = 0
    if(present(nInCritExtUsed)) &
         nExtCritUsed = nInCritExtUsed

    ! Number of criteria variables
    nAmrCrit =  nIntCrit + nExtCritUsed

    !if(iProc == 0) &
    !     write(*,'(a50,4(i5))') &
    !     " nAmrCrit, nIntCrit, nExtCritUsed, nAmrCritUsed =", &
    !     nAmrCrit, nIntCrit, nExtCritUsed, nAmrCritUsed

    !------------ Collect all criteria external and internals -------------

    if(nAmrCrit /= nAmrCritOld) then
       if(allocated(AmrCrit_IB)) deallocate(AmrCrit_IB)
       allocate(AmrCrit_IB(nAmrCrit,MaxBlock))
       AmrCrit_IB = -1.0
       nAmrCritOld = nAmrCrit
    end if

    AmrCrit_IB(:,1:nBlock) = 0.0

    ! add external criteria into the list of all criteria
    if(present(CritExt_IB)) then
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
              do iCrit = 1, nExtCritUsed
                AmrCrit_IB(nIntCrit+iCrit,iBlock) = CritExt_IB(iCrit,iBlock)
             end do
       end do

       ! add external refinement and coarsening thresholds to internals
       !   do iCrit = 1, nExtCritUsed
       !      RefineCritAll_I(nIntCrit+iCrit)  = RefineCritExt_I(iCrit)
       !      CoarsenCritAll_I(nIntCrit+iCrit) = CoarsenCritExt_I(iCrit)
       !   end do
    end if

    ! Estimation of the numerical error
    if(nIntCrit >0) &
         call calc_error_amr_criteria(nVar, State_VGB, Used_GB=Used_GB)

    if(DoSortAmrCrit .or. .not.DoStrictAmr) then
       ! we make an AMR priority list
       call sort_amr_criteria
    else
       ! refine only based on criteria
       call apply_unsorted_criteria
    end if


  end subroutine set_amr_criteria
  !===========================================================================
  subroutine sort_amr_criteria

    ! the routine will find the candidate for refinement or coursing based
    ! upon the percentage of refinement and the percentage wanted refined and
    ! and coarsened. It will ONLY modify the criteria the values in the 
    ! criteria list itself. The refinement criteria will overrule the 
    ! percentage refinement.

    use BATL_mpi, ONLY: iComm, iProc, nProc
    use ModMpi
    use BATL_size, ONLY: nBlock
    use ModSort, ONLY: sort_quick
    use BATL_tree, ONLY: Unused_BP,iStatusNew_A, Refine_, Coarsen_, &
         iNode_B, nNode, nChild, nNodeUsed, Unset_, diffRange

    ! Array containing all criteria for all blocks gathered
    real, allocatable :: AllCrit_II(:,:)

    ! CritSort_II contains all criteria for all blocks
    ! iIdxSort_II gives the new indexes after sorting
    ! and reused for other functions.
    integer, allocatable :: iIdxSort_II(:,:)
    real, allocatable :: CritSort_II(:,:)
    integer, allocatable :: iRank_I(:)
    real, allocatable :: Rank_I(:)
    integer :: iCrit, iBlock, iProces, k
    integer :: iCritSort, iSort
    integer :: iError
    real    :: Diff, Coeff, Crit

    integer :: nTotBlocks, nBlockMax, iTotalCrit, iHelpCrit
    integer, allocatable :: nBlock_P(:), nReciveCont_P(:), nRecivDisp_P(:)
    !-----------------------------------------------------------------------

    ! need extra array space to store temperal data
    iTotalCrit = 2
    iHelpCrit  = 3

    ! COMMENTS FOR FUTURE : we can use the Unused_B to only pick and send
    ! the data that is actively used, reducing communication


    !------------ Get information on all the blocks on all process -----------


    ! collect nBlock from each process 
    allocate(nBlock_P(nProc),nReciveCont_P(nProc),nRecivDisp_P(nProc))
    call MPI_Allgather(nBlock, 1,MPI_INTEGER, nBlock_P, 1, &
         MPI_INTEGER, iComm, iError)
    nBlockMax = maxval(nBlock_P)
    nTotBlocks = sum(nBlock_P)

    !------------ Collect all criteria for sorting and ranking ------------

    ! collect all criteria for the total grid
    allocate(AllCrit_II(nAmrCrit,nTotBlocks))
    AllCrit_II = -77

    ! store the Node indexing iNode_B for all processes
    if(allocated(iNode_I)) deallocate(iNode_I)
    allocate(iNode_I(nBlockMax*nProc))
    iNode_I = 0

    ! Set up the displacement and size for collecting the data for 
    ! AllCrit_II
    nReciveCont_P = nBlock_P*nAmrCrit
    nRecivDisp_P(1) = 0
    do iProces=2,nProc 
       nRecivDisp_P(iProces) = &
            nRecivDisp_P(iProces-1)+nBlock_P(iProces-1)*nAmrCrit
    end do

    ! gathering the criteria 
    call MPI_allgatherv(AmrCrit_IB, nAmrCrit*nBlock, MPI_REAL, &
         AllCrit_II, nReciveCont_P, nRecivDisp_P, &
         MPI_REAL, iComm, iError)


    ! Set up the displacement and size for collecting the data for 
    ! iNode_I
    nReciveCont_P = nBlock_P
    nRecivDisp_P(1) = 0
    do iProces=2,nProc 
       nRecivDisp_P(iProces) = nRecivDisp_P(iProces-1)+nBlock_P(iProces-1)
    end do

    ! gathering the node indexes
    call MPI_allgatherv(iNode_B, nBlock,MPI_INTEGER, &
         iNode_I, nReciveCont_P, nRecivDisp_P, &
         MPI_INTEGER, iComm, iError)

    ! Setting up arrays for sorting and index handling
    allocate( &
         CritSort_II(nTotBlocks,1), &
         iIdxSort_II(nTotBlocks,iHelpCrit)  )

    ! iRank_A is public, so deallocate it if necessary
    if(allocated(iRank_A)) deallocate(iRank_A,Rank_A)
    allocate(iRank_I(nTotBlocks), Rank_I(nTotBlocks),&
         iRank_A(nTotBlocks), Rank_A(nTotBlocks))

    iRank_A      = 0
    Rank_A       = 0.0
    iRank_I      = 0
    Rank_I       = 0.0
    iIdxSort_II  = 0
    CritSort_II  = 0.0

    Coeff = 1./real(nAmrCritUsed*nTotBlocks) ! normalization for secondary rank
    iRank_I = nNode+1 ! larger than any possible rank
    iIdxSort_II(:,iHelpCrit) = 0 ! store info about if a block is already 
    !                               marked for refinement(+1) or coarsening(-1)
    nNodeRefine  = 0
    nNodeCoarsen = 0

    do iCrit=1,nAmrCritUsed

       ! copy criteria data into the sorting arrays
       iSort = 0
       do iProces = 1, nProc
          do iBlock = 1, nBlock_P(iProces)
             if(Unused_BP(iBlock,iProces-1)) CYCLE
             iSort = iSort +1
             CritSort_II(iSort,1) = &
                  AllCrit_II(iCrit,iBlock+nRecivDisp_P(iProces))
             iIdxSort_II(iSort, 1) = iSort
          end do
       end do
       nNodeSort = iSort

       ! Sort each criteria. Pass in first index of array arguments
       call sort_quick(nNodeSort, &
            CritSort_II(1:nNodeSort,1), iIdxSort_II(1:nNodeSort,1))

       iIdxSort_II(:,iTotalCrit) = iIdxSort_II(:,1)
       k = 1
       iSort = iIdxSort_II(1,iTotalCrit)
       !Value of first element (min)
       iIdxSort_II(iSort,1) = k
       Crit = CritSort_II(iSort,1)


       ! group together criteria which has a diffrence of 
       ! less then DeltaCritera by giving them the same
       ! sort ranking
       do iCritSort = 2,nNodeSort
          iSort = iIdxSort_II(iCritSort,iTotalCrit)
          if((CritSort_II(iSort,1)- Crit) < DeltaCritera) then
             iIdxSort_II(iSort,1) = k
          else
             k = k + 1
             iIdxSort_II(iSort,1) = k
             Crit = CritSort_II(iSort,1)
          end if
       end do

       do iSort = 1,nNodeSort
          iCritSort = iIdxSort_II(iSort,iTotalCrit)

          ! Use the minimum position (=rank) of the node in sorted criteria 
          ! array to set up the new ranking order for each node when doing amr
          iRank_I(iCritSort) = min(iRank_I(iCritSort),iIdxSort_II(iCritSort,1))

          ! if some of the nodes have the same minimal rank we will use the 
          ! average rank of all criteria as a secondary priority.
          Rank_I(iCritSort) = Rank_I(iCritSort)+Coeff*iIdxSort_II(iCritSort,1)
       end do



       ! To make sure that blocks that are marked for refinement/coarsening 
       ! by thresholds will come at the top/bottom of the ranking list 
       ! we will shift the values with the total number of blocks used.
       ! COMMENTS FOR FUTURE : the loop may be split for one that goes 
       ! form the top and one that goes form bottom so it do not need to go 
       ! though the whole list of elements

       !print *," iCrit =", iCrit
       !print *," RefineCritAll_I(iCrit) : ", RefineCritAll_I(iCrit),size(RefineCritAll_I)
       !print *," CoarsenCritAll_I(iCrit): ",CoarsenCritAll_I(iCrit),size(CoarsenCritAll_I) 
       ! Only if Criterias is in use
       if(RefineCritAll_I(iCrit) > -0.5 .and. CoarsenCritAll_I(iCrit) > -0.5) then
          do iSort = nNodeSort,1,-1
             iCritSort = iIdxSort_II(iSort,iTotalCrit)
             ! Block has already ben marked
             !if(iIdxSort_II(iCritSort,iHelpCrit) /= 0 ) CYCLE

             if(CritSort_II(iCritSort,1) > RefineCritAll_I(iCrit) &
                  .and. iIdxSort_II(iCritSort,iHelpCrit) /= 1) then
                ! Satisfies refinement threshold and not yet marked for refinement

                ! Shift up the rank above sorted values                
                Rank_I(iCritSort) = Rank_I(iCritSort) + nTotBlocks+1

                ! If node was originally marked for coarsening, 
                ! now there is one fewer node to be coarsened
                if(iIdxSort_II(iCritSort,iHelpCrit) == -1) &
                     nNodeCoarsen = nNodeCoarsen - 1

                ! Noe node is marked fore refinement
                nNodeRefine = nNodeRefine +1
                iIdxSort_II(iCritSort,iHelpCrit) = 1

             else if(CritSort_II(iCritSort,1) < CoarsenCritAll_I(iCrit) &
                  .and. iIdxSort_II(iCritSort,iHelpCrit) == 0) then
                ! Satisfies coarsening threshold and not yet marked at all !

                ! Shift down the rank below sorted values
                Rank_I(iCritSort) = Rank_I(iCritSort)-nTotBlocks-1

                ! Now node is marked for coarsening
                nNodeCoarsen = nNodeCoarsen +1
                iIdxSort_II(iCritSort,iHelpCrit) = -1 
             end if
          end do
       end if

    end do


    ! the final rank is a sum of the primary minimum rank
    ! and secondary average rank:
    Rank_I(1:nNodeSort) = iRank_I(1:nNodeSort) + Rank_I(1:nNodeSort)

    ! Get the number of elements to refine by percentage
    ! taking into account that each refinements generate nChild-1 
    ! new blocks
    nDesiredRefine  = floor(PercentRefine*nNodeUsed/(100.0))
    nDesiredCoarsen = floor(PercentCoarsen*nNodeUsed/100.0)

    ! make sure thay do not take more then thay can
    nDesiredRefine  = min(nNodeSort,nDesiredRefine)
    nDesiredCoarsen = min(nNodeSort,nDesiredCoarsen)

    ! nNodeRefine and nNodeCoarsen are based on thresholds.
    ! If DoSoftAmrCrit is false, these have to be refined/coarsened,
    ! or else the code stops.
    ! If DoSoftAmrCrit is true, we change the threshold based 
    ! values into "desired" values, so the code behaves as if 
    ! the criteria were based on percentages.
    if(DoSoftAmrCrit .or. .not.DoStrictAmr) then
       nDesiredRefine  = max(nDesiredRefine,  nNodeRefine)
       nDesiredCoarsen = max(nDesiredCoarsen, nNodeCoarsen)
       nNodeRefine  = 0
       nNodeCoarsen = 0
    end if

    !if(iProc == 0) then
    !   write(*,'(2(a30, i10))') " BATL nDesiredRefine  = ", &
    !        nDesiredRefine,",    nNodeRefine  = ",  nNodeRefine
    !   write(*,'(2(a30, i10))') " BATL nDesiredCoarsen = ", &
    !        nDesiredCoarsen,",    nNodeCoarsen = ", nNodeCoarsen
    !end if


    ! store the sorted indexing list for Rank_A
    iIdxSort_II(:,iTotalCrit) = 0 

    ! The quick sort algorithm assume a continuous indexing order,
    ! this will not necessary be true so we need to make a 
    ! mapping between the block numbers and the node numbers when
    ! there are unused blocks. This is stored in iIdxSort_II(iSort,1)
    k = 0
    iSort = 0
    do iProces = 1,nProc
       do iBlock = 1, nBlock_P(iProces)
          k = k +1
          if(Unused_BP(iBlock,iProces-1)) CYCLE
          iSort = iSort+1
          iIdxSort_II(iSort,1) = iNode_I(k)
       end do
    end do

    ! Finding final rank ordering
    call sort_quick(nNodeSort, Rank_I, iIdxSort_II(1,iTotalCrit))

    ! map local indexing in this subroutine to the node index
    do iSort = 1, nNodeSort
       iCritSort = iIdxSort_II(iSort,iTotalCrit)
       iRank_A(iSort) =iIdxSort_II(iCritSort,1)
       Rank_A(iSort) = Rank_I(iCritSort)
    end do
    
    ! making sure that the refinment and coarsning are starting from
    ! a position with have a transition in its criteria larger then
    ! diffRange
    if(nDesiredRefine > nNodeRefine .and.  nDesiredRefine < nNodeSort )then
       k=-1
       do iSort = nNodeSort-nDesiredRefine, nNodeSort-nNodeRefine
          diff =  abs(Rank_A(iSort) - Rank_A(nNodeSort- nDesiredRefine+1)) 
          if( diff > diffRange) EXIT
          k=k+1
       end do
       nDesiredRefine = nDesiredRefine - max(0,k)
    end if

    if(nDesiredCoarsen > nNodeCoarsen .and. nDesiredCoarsen < nNodeSort )then
       k=-1
       do iSort= nDesiredCoarsen+1, max(nNodeCoarsen,1), -1
          diff = abs(Rank_A(iSort) - Rank_A(nDesiredCoarsen))
          if(diff >diffRange) EXIT
          k=k+1
       end do
       nDesiredCoarsen = nDesiredCoarsen - max(0,k)
    end if

    ! Give iStatusNew_A based on the iRank_A and number of blocks we want for
    ! refinment and coarsning
    do iSort = nNodeSort, nNodeSort-max(nDesiredRefine,nNodeRefine)+1, -1
       iStatusNew_A(iRank_A(iSort)) = Refine_
    end do

    do iSort = 1, max(nDesiredCoarsen, nNodeCoarsen)
       if(iStatusNew_A(iRank_A(iSort)) /= Refine_) &
            iStatusNew_A(iRank_A(iSort)) =  Coarsen_
    end do

    deallocate(AllCrit_II)
    deallocate(CritSort_II, iIdxSort_II)
    deallocate(nBlock_P, nReciveCont_P, nRecivDisp_P)
    deallocate(iRank_I, Rank_I)

  end subroutine sort_amr_criteria


  !============================================================================

  ! The AMR criteria method used here are adapted from L{\"o}hner (1987).
  ! The flash3 user guide have also a description used.
  ! The method use the relative error ,E (Factor), of the approximation to 
  ! the differential equation:
  !
  !  dU      DU         d^2 U
  ! ----  = ----  + DX  ------
  !  dx      DX          dx^2
  ! 
  ! Giving a relative error
  !
  !             d^2 U
  !            -------
  !              dx^2
  !    E  =  -------------
  !           1     dU
  !          ---  ------
  !          DX     dx
  !
  ! To avoid problems with small linear gradient fluctuations in the 
  ! denominator we add it with a fraction of its average value.
  ! And to avoid zero in the denominator we add it with cEpsilon.
  ! The final expression is:
  !
  !                               d^2 U
  !                              -------
  !                               dx^2 
  !    E  =  ----------------------------------------------
  !           1     dU        1
  !          ---  ------  + ------ cAmrWavefilter*<U> + cEpsilon 
  !          DX     dx       DX^2
  !
  ! We use a wide stencil to do the refinement in time and 
  ! neglected the diagonal terms. 
  ! 
  ! The error factor for each variable will be normalized before
  ! threshold evaluation and also the sum of all the factors over
  ! all variables will be evaluated for deciding the refinement.
  !
  ! The subroutine only set iStatusNew_A array.
  !
  ! OPTIONAL:
  ! The user can sent in additional factors  used for deciding refinement
  ! together with there refinements criteria. This will be then used
  ! in addition to the error estimate described earlier.

  subroutine calc_error_amr_criteria(nVar, State_VGB,Used_GB )

    use BATL_size, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK,&
	 nI, nJ, nK, MaxDim, nDim, MaxBlock, nBlock
    use BATL_tree, ONLY: Unused_B
    use BATL_mpi, ONLY: iProc

    integer,  intent(in)   :: nVar
    real,    intent(in) :: &                            ! state variables
         State_VGB(nVar,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    logical, intent(in), optional:: &
         Used_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)  ! used cells
    !integer, intent(in), optional :: nCritExt ! num of external criteria
    !real, intent(inout), optional :: CritExt_IB(:,:)
    !real, intent(in),    optional :: CoarsenCritExt_I(:), RefineCritExt_I(:)

    real :: Crit, Crit_D(MaxDim)
    real, parameter :: InvNdim = 1.0/nDim
    real :: Numerator, Denominator
    integer:: iBlock, iCrit, i, j, k, iVar
    ! number of blocks set out for refining and coarsning
    !------------------------------------------------------------------------
    Crit        = 1.0
    Crit_D      = 0.0
    Numerator   = 0.0
    Denominator = 0.0

    !if(iproc == 0) write(*,*) "calc_error_amr_criteria"

    if(nIntCrit > nVar) &
         call CON_stop("calc_error_amr_criteria :: More criteria then variables")

    !--------------- initilizing AMR masking ----------------------------
    if(UseAmrMask .and. .not.allocated(DoAmr_GB)) then

       allocate(DoAmr_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
       allocate(DoAmr_B(MaxBlock))
       
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE

          call restrict_amr_criteria(iBlock)
       end do
    end if

    ! Calculate error estimates (1..nIntCrit)
    BLOCK: do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       if(UseAmrMask)then
          if(.not.DoAmr_B(iBlock)) CYCLE
       end if

       ! Check refinement first
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          ! Only variables indexed in iVarCrit_I will decide the refinement

          if(present(Used_GB)) then
             if(nDim == 1) then
                if(.not.all(Used_GB((i-2):(i+2),j, k,iBlock))) CYCLE
             end if
             if(nDim == 2) then
                if(.not.all(Used_GB((i-2):(i+2),(j-2):(j+2),k,iBlock))) CYCLE
             end if
             if(nDim == 3) then
                if(.not.all(Used_GB((i-2):(i+2),(j-2):(j+2),(k-2):(k+2),iBlock))) CYCLE
             end if
          end if

          if(UseAmrMask) then
             if(.not.DoAmr_GB(i,j,k,iBlock) ) CYCLE
          end if

          do iCrit = 1, nIntCrit 
             iVar = iStateVarCrit_I(iCrit)

             Crit_D =0.0

             Numerator = abs( &
                  State_VGB(iVar,i-2,j,k,iBlock)   - &
                  2.0*State_VGB(iVar,i,j,k,iBlock) + &
                  State_VGB(iVar,i+2,j,k,iBlock) )
             Denominator =  (&
                  abs(State_VGB(iVar,i+2,j,k,iBlock) - &
                  State_VGB(iVar,i,j,k,iBlock)) + &
                  abs(State_VGB(iVar,i,j,k,iBlock) - &
                  State_VGB(iVar,i-2,j,k,iBlock))) + &
                  cAmrWavefilter * (&
                  abs(State_VGB(iVar,i+2,j,k,iBlock)) + &
                  abs(2.0*State_VGB(iVar,i,j,k,iBlock)) + &
                  abs(State_VGB(iVar,i-2,j,k,iBlock))) 
             Crit_D(1) = (Numerator/max(Denominator,cEpsilon)) 
             Crit = Crit_D(1)**2
             if(nDim >= 2) then
                Numerator =  abs( &
                     State_VGB(iVar,i,j-2,k,iBlock)   - &
                     2.0*State_VGB(iVar,i,j,k,iBlock) + &
                     State_VGB(iVar,i,j+2,k,iBlock) )
                Denominator = (&
                     abs(State_VGB(iVar,i,j+2,k,iBlock) - &
                     State_VGB(iVar,i,j,k,iBlock)) + &
                     abs(State_VGB(iVar,i,j,k,iBlock) - &
                     State_VGB(iVar,i,j-2,k,iBlock))) + &
                     cAmrWavefilter * (&
                     abs(State_VGB(iVar,i,j+2,k,iBlock)) + &
                     abs(2.0*State_VGB(iVar,i,j,k,iBlock)) + &
                     abs(State_VGB(iVar,i,j-2,k,iBlock)))
                Crit_D(2) = (Numerator/max(Denominator,cEpsilon))
                Crit = Crit + Crit_D(2)**2
             end if

             if(nDim >= 3) then
                Numerator = abs( &
                     State_VGB(iVar,i,j,k-2,iBlock)   - &
                     2.0*State_VGB(iVar,i,j,k,iBlock) + &
                     State_VGB(iVar,i,j,k+2,iBlock))
                Denominator = (&
                     abs(State_VGB(iVar,i,j,k+2,iBlock) - &
                     State_VGB(iVar,i,j,k,iBlock)) + &
                     abs(State_VGB(iVar,i,j,k,iBlock) - &
                     State_VGB(iVar,i,j,k-2,iBlock)) + &
                     cAmrWavefilter * (&
                     abs(State_VGB(iVar,i,j,k+2,iBlock)) + &
                     abs(2.0*State_VGB(iVar,i,j,k,iBlock)) + &
                     abs(State_VGB(iVar,i,j,k-2,iBlock))))
                Crit_D(3) = (Numerator/max(Denominator,cEpsilon))
                Crit = Crit + Crit_D(3)**2
             end if


             !Crit = (sum(Crit_D(1:nDim)**2))*InvNdim
             Crit = Crit *InvNdim
             !AmrCrit_IB(iCrit,iBlock) = &
             !     max(AmrCrit_IB(iCrit,iBlock),Crit)
             if( Crit >  AmrCrit_IB(iCrit,iBlock)) &
                  AmrCrit_IB(iCrit,iBlock) = Crit

          end do !end nVar
       end do; end do; end do

    end do BLOCK

  end subroutine calc_error_amr_criteria

  !============================================================================

  subroutine apply_unsorted_criteria

    use BATL_mpi,  ONLY: iProc 
    use BATL_size, ONLY: nBlock
    use BATL_tree, ONLY: iStatusNew_A, Refine_, Coarsen_, &
         Unused_B, iNode_B, iTree_IA, Level_

    integer:: iBlock, iCrit, iVarCrit
    ! number of blocks set out for refining and coarsning
    logical :: DoCoarsen
    !----------------------------------------------------------------------

    !if(iProc == 0) write(*,*) "apply_unsorted_criteria"

       BLOCK3:do iBlock = 1, nBlock

          if(Unused_B(iBlock)) CYCLE

          if(UseAmrMask) then 
             if(.not.DoAmr_B(iBlock)) CYCLE
          end if

          DoCoarsen = .true.

             
          do iCrit = 1, nAmrCritUsed

             iVarCrit = iVarCrit_I(iCrit)


             if(iTree_IA(Level_,iNode_B(iBlock)) > MaxLevelCrit_I(iCrit)) &
                  CYCLE
             
             if(AmrCrit_IB(iVarCrit,iBlock) >= RefineCritAll_I(iCrit) .and. &
                  iTree_IA(Level_,iNode_B(iBlock)) < MaxLevelCrit_I(iCrit))then
                iStatusNew_A(iNode_B(iBlock)) = Refine_
                CYCLE BLOCK3
             else if(AmrCrit_IB(iVarCrit,iBlock) > CoarsenCritAll_I(iCrit))then
                DoCoarsen = .false.
             end if

          end do

          if(DoCoarsen) iStatusNew_A(iNode_B(iBlock)) =  Coarsen_ 

       end do BLOCK3

  end subroutine apply_unsorted_criteria

  !============================================================================
  subroutine read_amr_criteria(NameCommand, nCritOut, NameCritOut_I,&
       NameStatVarIn_V, nStateVarIn, ReadExtraOut)

    use ModReadParam, ONLY: read_var
    use ModUtilities, ONLY: lower_case
    use BATL_tree, ONLY: MaxTotalBlock, Unused_B,iTree_IA,MaxLevel_,&
         MaxLevel
    use BATL_size, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK,&
         MaxBlock,nBlock, MaxDim, nDim

    character(len=*), intent(in) :: NameCommand
    character (len=20) :: TypeAmr
    integer :: iCrit,iBlock,iAmrBox,iCritName,nCrit,iIntCrit,iStatVar

    ! Number and name of criteria to be used by BATSRUS (at most 3)
    integer,           optional, intent(out):: nCritOut
    character(len=20), optional, intent(out):: NameCritOut_I(3)
    logical,           optional, intent(out):: ReadExtraOut
    logical :: IsUniqueCritName, UseErrorCrit, ReadExtra

    integer,           optional, intent(in):: nStateVarIn
    character(len=*),  optional, dimension(:), intent(in):: NameStatVarIn_V

    character(len=20) :: CritName
    character(len=20) :: NameStatVar
    logical :: IsLevel
    logical :: DoAmr
    integer :: DnAmr
    real    :: DtAmr
    real    :: tmp

    character(len=*), parameter:: NameSub='BATL_tree::read_amr_criteria'
    !-------------------------------------------------------------------------
    ReadExtra = .false.

    DoSortAmrCrit = .not. DoStrictAmr
    select case(NameCommand)
    case("#AMRCRITERIA", "#AMRCRITERIALEVEL")
       IsLevel = NameCommand == "#AMRCRITERIALEVEL"
       
       call read_var('nCrit', nCrit)

       nAmrCrit = nCrit

       ! deallocate,if they are already allocated
       if(allocated(CoarsenCritAll_I)) &
            deallocate(CoarsenCritAll_I, RefineCritAll_I, iVarCrit_I)
       if(allocated(MaxLevelCrit_I)) deallocate(MaxLevelCrit_I)
       
       ! allocate all arrays
       allocate(CoarsenCritAll_I(nCrit), RefineCritAll_I(nCrit), &
            iVarCrit_I(nCrit), MaxLevelCrit_I(nCrit))

       if(present(nStateVarIn) .and. .not. allocated(iStateVarCrit_I))&
            allocate(iStateVarCrit_I(nCrit))

       nIntCrit = 1
       nCritOut = 1
       NameCritOut_I = "NULL"
       iStateVarCrit_I = -777
       UseErrorCrit = .false.

       do iCrit = 1, nCrit
          IsUniqueCritName = .true.
          !find index of the criteria from its name
          call read_var('CritName', CritName, IsLowerCase=.true.)

          if(CritName(1:5) == 'error') then
             UseErrorCrit = .true.

             if(.not. present(nStateVarIn) &
                  .and. .not. present(NameStatVarIn_V))&
                  call CON_stop(NameCommand//' ERROR: Need a name table')

             ! find index associated with name => iStatVar
             do iStatVar = 1, nStateVarIn
                NameStatVar = NameStatVarIn_V(iStatVar)
                call lower_case(NameStatVar)
                if(NameStatVar == CritName(7:6+len_trim(NameStatVar)) ) EXIT
             end do
             !find if index iStatVar exsit in list before
             do iIntCrit = 1, nIntCrit
                if(iStateVarCrit_I(iIntCrit) == iStatVar) then
                   IsUniqueCritName = .false.
                end if
             end do
             ! if unique it will be added to list
             if(IsUniqueCritName .and. iStatVar <= nStateVarIn) then
                iStateVarCrit_I(nIntCrit) = iStatVar
                iVarCrit_I(iCrit) = -nIntCrit
                nIntCrit = nIntCrit+1
             end if
          elseif(CritName(1:9) == 'transient') then
             if(.not. present(ReadExtraOut)) call CON_stop(NameCommand//&
                  ' ERROR: BATSRUS need flag to read more data')
             ReadExtra = .true.

             ! Find out it the name has bin used before
             do iCritName = 1, 3
                if(CritName(11:20) == NameCritOut_I(iCritName)) then
                   iVarCrit_I(iCrit) = iCritName
                   IsUniqueCritName = .false.
                   EXIT
                end if
             end do

             ! Add it to the list if its unique
             if(IsUniqueCritName) then
                do iCritName = 1, min(nCritOut+1,3)
                   if(NameCritOut_I(iCritName) == "NULL" )then
                      iVarCrit_I(iCrit) = iCritName
                      NameCritOut_I(iCritName) = CritName(11:20)
                      EXIT
                   end if
                end do
                nCritOut = iVarCrit_I(iCrit)
             end if
          else
             if(.not. present(NameCritOut_I) &
                  .and. .not. present(nCritOut))&
                  call CON_stop(NameCommand//' ERROR: Need a name table')

             ! Find out it the name has bin used before
             do iCritName = 1, 3
                if(trim(CritName) == trim(NameCritOut_I(iCritName))) then
                   iVarCrit_I(iCrit) = iCritName
                   IsUniqueCritName = .false.
                   EXIT
                end if
             end do

             ! Add it to the list if its unique
             if(IsUniqueCritName) then
                do iCritName = 1, min(nCritOut+1,3)
                   if(NameCritOut_I(iCritName) == "NULL" )then
                      iVarCrit_I(iCrit) = iCritName
                      NameCritOut_I(iCritName) = CritName 
                      EXIT
                   end if
                end do
                nCritOut = iVarCrit_I(iCrit)
             end if
          end if
          call read_var('CoarsenCrit',CoarsenCritAll_I(iCrit))
          call read_var('RefineCrit',RefineCritAll_I(iCrit))
          if(IsLevel) call read_var('MaxLevelCrit',MaxLevelCrit_I(iCrit))
       end do

       if(UseErrorCrit)&
            call read_var('AmrWavefilter',cAmrWavefilter)
       if(ReadExtra) &
            call read_var('ReadExtraOut',ReadExtraOut)

       ! UseMultiCrit = .true.
       DoCritAmr = .true.
       DoAutoAmr = .true.      
       nExtCritUsed = nCritOut
       nIntCrit = nIntCrit-1
       nAmrCritUsed = nCrit
       where(iVarCrit_I > 0)
          iVarCrit_I = iVarCrit_I + nIntCrit
       end where
       iVarCrit_I = abs(iVarCrit_I)

       ! Extra stuff for #AMRCRITERIA
       if(.not.IsLevel)then
          if(allocated(iTree_IA)) then
             MaxLevelCrit_I(1:nAmrCritUsed) = maxval(iTree_IA(MaxLevel_,:))
          else
             ! if the grid is not initzilised at this point, we set it to max
             ! value and lett propper threading take care of it
             MaxLevelCrit_I(1:nAmrCritUsed) = MaxLevel 
          end if
       end if

       !print *," nCritOut        = ", nCritOut
       !print *," nIntCrit        = ", nIntCrit
       !print *," NameCritOut_I   = ", NameCritOut_I
       !print *," iStateVarCrit_I = ", iStateVarCrit_I
       !print *," iVarCrit_I      = ", iVarCrit_I
       !print *," CoarsenCritAll_I   = ", CoarsenCritAll_I
       !print *," RefineCritAll_I    = ", RefineCritAll_I
       !print *," MaxLevelCrit_I  = ", MaxLevelCrit_I
       !print *," AmrWavefilter   = ",cAmrWavefilter

    case("#AMR") ! compatibilety with old system with BATSRUS amr options
       call read_var('PercentCoarsen', PercentCoarsen)
       call read_var('PercentRefine' , PercentRefine)
       call read_var('MaxTotalBlock',  MaxTotalBlock) 
       DoSortAmrCrit = PercentCoarsen > 0.0 .or. PercentRefine > 0.0       
    case("#AMRLIMIT")
       if(UseAmrMask) &
            call CON_stop(NameCommand//' ERROR: AMR masking not supported')
       call read_var('PercentCoarsen', PercentCoarsen)
       call read_var('PercentRefine' , PercentRefine)
       call read_var('MaxTotalBlock',  MaxTotalBlock) 
       call read_var('DiffCriteriaLevel',  DeltaCritera)
       DoSortAmrCrit = PercentCoarsen > 0.0 .or. PercentRefine > 0.0
    case("#DOAMR") 
       call read_var('DoAmr',DoAmr)
       if(DoAmr) then
          call read_var('DnAmr',DnAmr)
          call read_var('DtAmr',DtAmr)
          call read_var('IsStrictAmr'  ,DoStrictAmr)
       end if
       if(.not. DoStrictAmr) DoSortAmrCrit = .true.
    case("#AMRAREA")
       if(DoSortAmrCrit) &
            call CON_stop(NameCommand//' ERROR: Sorting not supported')
       print *," #AMRAREA :: ",MaxBlock
       if(allocated(DoAmr_GB)) deallocate(DoAmr_GB)
       allocate(DoAmr_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
       if(allocated(DoAmr_B)) deallocate(DoAmr_B)
       allocate(DoAmr_B(MaxBlock))
       DoAmr_GB = .false.
       DoAmr_B  = .false.
       UseAmrMask = .true.
     
       call read_var('nAmrBox', nAmrBox)
       if(nAmrBox > 0) then
          allocate(AmrBox_DII(MaxDim,2,nAmrBox))
          do iAmrBox=1,nAmrBox
             call read_var('minX',AmrBox_DII(1,1,iAmrBox))
             call read_var('maxX',AmrBox_DII(1,2,iAmrBox))
             if( nDim <  2) CYCLE 
             call read_var('minY',AmrBox_DII(2,1,iAmrBox))
             call read_var('maxY',AmrBox_DII(2,2,iAmrBox))
             if( nDim <  3) CYCLE 
             call read_var('minZ',AmrBox_DII(3,1,iAmrBox))
             call read_var('maxZ',AmrBox_DII(3,2,iAmrBox))
          end do

       end if
    case default
       call CON_stop(NameSub//'incorect PARAM.in!')
    end select

    DoStrictAmr = .not. DoSortAmrCrit 

  end subroutine read_amr_criteria

  !============================================================================
  subroutine restrict_amr_criteria(iBlock)

    use BATL_grid, ONLY : Xyz_DGB
    use BATL_size, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK,nDim
    integer, intent(in) :: iBlock

    integer :: i,j,k,iDim,iAmrBox
    logical :: IsAmrCell
    real    :: Xyz_D(nDim) 
    !--------------------------------------------------------

    if(.not.UseAmrMask) RETURN

    DoAmr_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,iBlock) = .false.
    !If any cell in the block is true, the hole block need to be tested
    DoAmr_B(iBlock) = .false.
    
    BLOCK4:do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
       Xyz_D = Xyz_DGB(1:nDim,i,j,k,iBlock)
       do iAmrBox=1,nAmrBox 
          if(all(Xyz_D > AmrBox_DII(1:nDim,1,iAmrBox)) .and. &
               all(Xyz_D < AmrBox_DII(1:nDim,2,iAmrBox))) then
             DoAmr_GB(i,j,k,iBlock) = .true.
             DoAmr_B(iBlock) = .true.
             CYCLE BLOCK4
          end if
       end do
    end do; end do; end do BLOCK4

  end subroutine restrict_amr_criteria
  !============================================================================

  subroutine clean_amr_criteria

    if(allocated(DoAmr_GB))&
         deallocate(DoAmr_GB)
    if(allocated(AmrBox_DII))&
         deallocate(AmrBox_DII)
    if(allocated(DoAmr_B))&
         deallocate(DoAmr_B)
    
    if(.not.allocated(CoarsenCrit_I)) RETURN
    deallocate(CoarsenCrit_I, RefineCrit_I, iVarCrit_I)
    deallocate(CoarsenCritAll_I, RefineCritAll_I)
    if(allocated(AmrCrit_IB)) deallocate(AmrCrit_IB)
    if(allocated(iNode_I)) deallocate(iNode_I)
    if(allocated(MaxLevelCrit_I)) deallocate(MaxLevelCrit_I)
    nAmrCrit     = 0
    nAmrCritUsed = 0
    nAmrCritOld  = -1
    nIntCrit     = 0

  end subroutine clean_amr_criteria

  !============================================================================
  subroutine test_amr_criteria()
    use BATL_size, ONLY : MaxBlock,nBlock, iRatio, jRatio, kRatio
    use BATL_mpi, ONLY: iProc, nProc
    use BATL_tree, ONLY: init_tree, set_tree_root, find_tree_node, &
         refine_tree_node, distribute_tree, clean_tree, Unused_B, iNode_B, &
         nNode, show_tree, iStatusNew_A, &
         Coarsen_, Unset_, adapt_tree, move_tree, distribute_tree
    use BATL_grid, ONLY: init_grid, create_grid, clean_grid, CellSize_DB
    use BATL_geometry, ONLY: init_geometry
    use BATL_amr, ONLY: do_amr, init_amr
    use BATL_size, ONLY: MaxDim, nDim, MinI, MaxI, MinJ, MaxJ, MinK, MaxK,&
         nI, nJ, nK 
    ! For Random generation
    integer :: jSeed
    logical :: IsFirst
    integer, parameter :: iMPLIER=16807, &
         iMODLUS=2147483647, &
         iMOBYMP=127773, &
         iMOMDMP=2836

    integer, parameter:: MaxBlockTest            = 50
    integer, parameter:: nRootTest_D(MaxDim)     = (/3,3,3/)
    logical, parameter:: IsPeriodicTest_D(MaxDim)= .false.
    real, parameter:: DomainMin_D(MaxDim) = (/ -24.0, -24.0, -24.0 /)
    real, parameter:: DomainMax_D(MaxDim) = (/ 24.0, 24.0, 24.0 /)
    integer :: iNode, iBlock

    real, allocatable :: Criterias_IB(:,:),AllCriterias_IBP(:,:,:)
    real, allocatable :: PreCriterias_IB(:,:)
    real, allocatable :: RefineLevel_I(:), CoursenLevel_I(:)
    logical, allocatable :: Used_GB(:,:,:,:)

    integer :: nCritExt = 4
    integer, allocatable :: iA_I(:)
    real, allocatable :: TestState_VGB(:,:,:,:,:)
    integer :: nVar =3
    integer :: i,j,k,iVar
    logical:: DoTestMe
    character(len=*), parameter :: NameSub = 'test_amr_criteria'
    !-----------------------------------------------------------------------
    DoTestMe = iProc == 0

    if(DoTestMe) write(*,*) 'Starting ',NameSub

    call init_tree(MaxBlockTest)
    call init_geometry( IsPeriodicIn_D = IsPeriodicTest_D(1:nDim) )
    call init_grid( DomainMin_D(1:nDim), DomainMax_D(1:nDim) )
    call set_tree_root( nRootTest_D(1:nDim))

    call find_tree_node( (/0.5,0.5,0.5/), iNode)
    call refine_tree_node(iNode)
    call distribute_tree(.true.)
    call create_grid
    call init_amr

    call srand(123456789+iProc)

    allocate(Criterias_IB(nCritExt,nBlock), &
         AllCriterias_IBP(nCritExt,nBlock,nProc),&
         PreCriterias_IB(nCritExt,nBlock))
    allocate(RefineLevel_I(nCritExt),CoursenLevel_I(nCritExt))


    !===================== Begin Test  =======================
    DoSortAmrCrit = .true.
    !--------------------- internal --------------------------
    nIntCrit = 1
    nExtCritUsed = 0
    allocate(CoarsenCritAll_I(nVar), &
         RefineCritAll_I(nVar),iVarCrit_I(nVar),iStateVarCrit_I(nVar))

    allocate(TestState_VGB(nVar,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
    CoarsenCritAll_I = -1.0
    RefineCritAll_I  =  1.0
    TestState_VGB = 1.0
    iVarCrit_I(nIntCrit)=1
    iStateVarCrit_I(nIntCrit)=1
    nAmrCritUsed = 1
    nAmrCrit = nIntCrit + nExtCritUsed
    !allocate(AmrCrit_IB(nIntCrit+nExtCritUsed,nBlock))
    !AmrCrit_IB = 0.0

    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       do k = MinK, MaxK
          do j = MinJ, MaxJ
             do i = MinI,MaxI
                do iVar=1,nVar
                   TestState_VGB(iVar,i,j,k,iBlock) = &
                        dexp(0.1*(i*(iNode_B(iBlock)+1)))
                end do
             end do
          end do
       end do
    end do


    call set_amr_criteria(nVar,TestState_VGB)

!!$    do iBlock = 1, nBlock
!!$       if(Unused_B(iBlock)) CYCLE
!!$       write(*,'(i6,f16.12)') iNode_B(iBlock),AmrCrit_IB(1,iBlock)
!!$    end do
!!$
!!$    do iNode = 1, nNode-1
!!$       print *,iNode, iRank_A(iNode)
!!$    end do

    do iNode = 2, nNode-1
       if(iRank_A(iNode-1) > iRank_A(iNode)) then
          write(*,*) " ERROR in ",NameSub, " Internal test"
       end if
    end do

    !deallocate(AmrCrit_IB)

   
    
    !-------------------- external --------------------------
    Criterias_IB = 0.0
    RefineLevel_I =  1.0
    CoursenLevel_I = -1.0
    nExtCritUsed = 1
    nIntCrit = 0
    nCritExt = nExtCritUsed
    nAmrCritUsed = nIntCrit + nExtCritUsed 
    
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) then
          Criterias_IB(1,iBlock) = 10.0
       else
          Criterias_IB(1,iBlock) = AmrCrit_IB(1,iBlock)
       end if
    end do

    call set_amr_criteria(nVar,TestState_VGB, &
         nCritExt, Criterias_IB, CoursenLevel_I, RefineLevel_I)

    do iNode = 2, nNode-1
       if(iRank_A(iNode-1)>iRank_A(iNode)) then
          write(*,*) " ERROR in ",NameSub, " External=Intenal test"
       end if
    end do
    
    !--------------------- internal with masked cells --------
    nIntCrit = 1
    nExtCritUsed = 0
    nAmrCritUsed = nIntCrit + nExtCritUsed 

    CoarsenCritAll_I = -1.0
    RefineCritAll_I  =  1.0
    TestState_VGB = 1.0
    iVarCrit_I(nIntCrit)=1
    iStateVarCrit_I(nIntCrit)=1

    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       do k = MinK, MaxK
          do j = MinJ, MaxJ
             do i = MinI,MaxI
                do iVar=1,nVar
                   TestState_VGB(iVar,i,j,k,iBlock) = &
                        dexp(0.1*(i*(iNode_B(iBlock)+1)))
                end do
             end do
          end do
       end do
    end do

    UseAmrMask = .true.
    nAmrBox = 1
    allocate(AmrBox_DII(3,2,nAmrBox))
    AmrBox_DII(1,1,1) = DomainMin_D(1) 
    AmrBox_DII(1,2,1) = DomainMin_D(1) + CellSize_DB(1,1)*nI
    AmrBox_DII(2,1,1) = DomainMin_D(2) 
    AmrBox_DII(2,2,1) = DomainMin_D(2) + CellSize_DB(2,1)*nJ
    AmrBox_DII(3,1,1) = DomainMin_D(3) 
    AmrBox_DII(3,2,1) = DomainMin_D(3) + CellSize_DB(3,1)*nK

    call set_amr_criteria(nVar,TestState_VGB)

    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       if( iNode_B(iBlock) == 1) then
          if( AmrCrit_IB(1,iBlock)  == 0.0) &
               write (*,*) " ERROR in ",NameSub, &
               " in  Internal test masked cells", &
               " AmrCrit_IB of Node == 1 shoud be none zero"
       else
          if( AmrCrit_IB(1,iBlock)  /= 0.0) &
               write (*,*) " ERROR in ",NameSub, &
               " in  Internal test masked cells", &
               " AmrCrit_IB of Node /= 1 shoud be zero"
       end if
    end do

!!$   ! Using any becouse of the ghost cells
!!$    do iBlock = 1, nBlock
!!$       if(Unused_B(iBlock)) CYCLE
!!$       write(*,*) iNode_B(iBlock), &
!!$            any(DoAmr_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,iBlock))
!!$    end do
!!$

    UseAmrMask = .false.
    deallocate(AmrBox_DII)
    deallocate(DoAmr_GB)
    !--------------------- internal with masked cells --------

    !--------------------- internal with masked body -------
    nIntCrit = 1
    nExtCritUsed = 0
    nAmrCritUsed = nIntCrit + nExtCritUsed 

    allocate(Used_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
    Used_GB = .true.

    CoarsenCritAll_I = -1.0
    RefineCritAll_I  =  1.0
    TestState_VGB = 1.0
    iVarCrit_I(nIntCrit)=1
    iStateVarCrit_I(nIntCrit)=1

    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       do k = MinK, MaxK
          do j = MinJ, MaxJ
             do i = MinI,MaxI
                do iVar=1,nVar
                   TestState_VGB(iVar,i,j,k,iBlock) = &
                        dexp(0.1*(i*(iNode_B(iBlock)+1)))
                end do
             end do
          end do
       end do
    end do

    do iBlock = 1, nBlock
       if(iNode_B(iBlock) /= nNode)  CYCLE

       Used_GB(1,1,1,iBlock) = .false.

       do k = MinK, MaxK
          do j = MinJ, MaxJ
             do i = MinI,MaxI
                do iVar=1,nVar
                   TestState_VGB(iVar,i,j,k,iBlock) = dexp(0.1*(i*0.5))
                end do
             end do
          end do
       end do

       TestState_VGB(:,1,1,1,iBlock) = 1.0e18
    end do


    call set_amr_criteria(nVar,TestState_VGB,Used_GB=Used_GB)

    if(iRank_A(1) /= nNode) &
         write(*,*) " ERROR in ",NameSub, " Internal test masked body"

    do iNode = 3, nNode-1
       if(iRank_A(iNode-1) > iRank_A(iNode)) then
          write(*,*) " ERROR in ",NameSub, " Internal test masked body"
       end if
    end do



    deallocate(Used_GB)
    !-------------------- internal x 2 -------------------

    if(iRatio > 1 .and. jRatio > 1 .and. kRatio >1 ) then

       nCritExt = 0
       nIntCrit = 2
       nExtCritUsed = nCritExt
       nAmrCritUsed = nIntCrit + nExtCritUsed 
       iVarCrit_I(1:nIntCrit)=(/ 1,2 /)
       iStateVarCrit_I =  iVarCrit_I

       do iBlock=1,nBlock
          if(Unused_B(iBlock)) CYCLE
          do k = MinK, MaxK
             do j = MinJ, MaxJ
                do i = MinI,MaxI
                   TestState_VGB(1,i,j,k,iBlock) = &
                        dexp(0.1*(i*(35-iNode_B(iBlock)+1)))

                   TestState_VGB(2,i,j,k,iBlock) = &
                        dexp(0.1*(i*(iNode_B(iBlock)+1)))
                end do
             end do
          end do
       end do

       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) then
             Criterias_IB(1,iBlock) = 10.0
          else
             Criterias_IB(1,iBlock) = AmrCrit_IB(1,nBlock-iBlock+1)
          end if

       end do


       call set_amr_criteria(nVar,TestState_VGB, &
            nCritExt, Criterias_IB, CoursenLevel_I, RefineLevel_I)

       allocate(iA_I(nNode-1))

       iA_I =(/ 1,35, 2, 34, 33,  3,  4, 32, 31,  5,  6, 30, 29,  7, &
            8, 28, 27,  9, 26, 10, 25, 11, 12, 24, 13, 23, 22, 15, 21, &
            16, 17, 20, 19, 18 /)

!!$       do iBlock = 1, nBlock
!!$          if(Unused_B(iBlock)) CYCLE
!!$          print *,"Criterias_IB(1,iBlock) = ", &
!!$               AmrCrit_IB(1:2,iBlock), iNode_B(iBlock)
!!$       end do
!!$
!!$       do iNode = 1, nNode-1
!!$          print *,iRank_A(iNode)," :: ", iA_I(iNode) 
!!$       end do

       do iNode = 1, nNode-1, 2
          if(iRank_A(iNode) /= iA_I(iNode)) &
               write(*,*) " ERROR in ",NameSub, "2 x Intenal test"
       end do

       deallocate(iA_I)
    end if
    !-------------------- testing levels ---------------------
    !intenals
    CoarsenCritAll_I(1) = -1.0
    RefineCritAll_I(1)  =  1.0
    !externals
    RefineLevel_I(1) =  0.030
    CoursenLevel_I(1) = 0.020

    PercentRefine  = 30.0
    PercentCoarsen = 10.0
    nCritExt = 1
    nIntCrit = 1
  
    nExtCritUsed = nCritExt
    nAmrCritUsed = nIntCrit + nExtCritUsed 

    ! init Internals
    iVarCrit_I(nIntCrit) = 1
    iStateVarCrit_I(nIntCrit) = 1
    do iBlock=1,nBlock
       if(Unused_B(iBlock)) CYCLE
       do k = MinK, MaxK
          do j = MinJ, MaxJ
             do i = MinI,MaxI
                !do iVar=1,nVar
                !print *,rand()
                TestState_VGB(2,i,j,k,iBlock) = &
                     dexp(0.1*(i*(iNode_B(iBlock)+1)))
                !end do
             end do
          end do
       end do
    end do

    !init Externals
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       Criterias_IB(1,iBlock) = 0.001*(nNode - iNode_B(iBlock)+1)
    end do

    call set_amr_criteria(nVar,TestState_VGB, &
         nCritExt, Criterias_IB, CoursenLevel_I, RefineLevel_I)

    do k = nNodeCoarsen+1, nNode-1-nNodeRefine
       if( iRank_A(k) < nNodeRefine .and. & 
            iRank_A(k) > nNode-1 - nNodeCoarsen ) &
            write(*,*) "Error in seting refinment by criteria"
    end do

    ! test unused block
    Criterias_IB = 0.0
    RefineLevel_I =  1.0
    CoursenLevel_I = -1.0
    nCritExt = 0
    nIntCrit = 1
    
    iStatusNew_A = Unset_
    nExtCritUsed = nCritExt
    nAmrCritUsed = nIntCrit + nExtCritUsed 

    do iBlock=1,nBlock
       if(iNode_B(iBlock) > 3**nDim) then 
          iStatusNew_A(iNode_B(iBlock)) =  Coarsen_
       end if
    end do
    !call show_tree(NameSub,.true.)
    call adapt_tree
    call distribute_tree(DoMove=.false.)
    call do_amr(nVar,TestState_VGB)
    call move_tree
    !call show_tree(NameSub,.true.)

    do iBlock=1,nBlock
       if(Unused_B(iBlock)) CYCLE
       do k = MinK, MaxK
          do j = MinJ, MaxJ
             do i = MinI,MaxI
                do iVar=1,nVar
                   TestState_VGB(iVar,i,j,k,iBlock) = &
                        dexp(0.1*(i*(iNode_B(iBlock)+1)))
                end do
             end do
          end do
       end do
    end do

    call set_amr_criteria(nVar,TestState_VGB)

    do iNode = 2, nNode
       if(iRank_A(iNode-1)>iRank_A(iNode)) then
          write(*,*) " ERROR in ",NameSub, " unused  test"
       end if
    end do


    !===================== End Test  =======================
    deallocate(CoarsenCritAll_I,RefineCritAll_I,iVarCrit_I,iStateVarCrit_I)
    deallocate(TestState_VGB)

    deallocate(Criterias_IB,PreCriterias_IB,AllCriterias_IBP)
    deallocate(RefineLevel_I,CoursenLevel_I)
    call clean_grid
    call clean_tree

  contains

    ! The saudo random number generator is for testing performense in
    ! parallel sorting
    subroutine srand(iSeed)
      integer, intent(in) :: iSeed
      jSeed = iSeed
      IsFirst = .true.
    end subroutine srand

    real function rand()
      !  A pseudo-random number generator implemented to make sure that 
      !  all platform reproduce the same sequence for testing and compering.
      !  The algorithm is based on "Integer Version 2" given in :
      !
      !       Park, Steven K. and Miller, Keith W., "Random Number Generators: 
      !       Good Ones are Hard to Find", Communications of the ACM, 
      !       October, 1988.

      integer :: nHvalue,nLvalue,nTestv
      integer, save :: nExtn

      if(IsFirst) then
         nExtn=jSeed
         IsFirst = .false.
      end if

      nHvalue = nExtn/iMOBYMP
      nLvalue = mod(nExtn,iMOBYMP)
      nTestv = iMPLIER*nLvalue - iMOMDMP*nHvalue
      if(nTestv > 0) then
         nExtn = nTestv
      else
         nExtn = nTestv + iMODLUS
      end if

      rand = real(nExtn)/real(iMODLUS)

    end function rand

  end subroutine test_amr_criteria

end module BATL_amr_criteria
