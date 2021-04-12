!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
! MHD grid in BATSRUS
module MH_domain_decomposition

  use CON_grid_storage
  use CON_domain_decomposition, ONLY: ProcToolkit_ => Pe_, &
       BLK_, GlobalBlock_, Parent_, MyNumberAsAChild_, &
       FirstChild_, None_, check_octree_allocation, complete

  implicit none

  SAVE

  interface MH_get_root_decomposition
     module procedure MH_get_roots_id
     module procedure MH_get_roots_dd
  end interface

  public:: MH_get_root_decomposition
  public:: MH_update_local_decomposition

  type(DomainType), public:: MH_Domain
  type(DomainType), public:: MH_LineDecomposition

  ! Local variables and constants
  integer, private:: iLastGrid = -1, iLastDecomposition = -1

  ! Position of children relative to the parent block
  ! in the Morton ordering
  integer, parameter, private:: iShiftMorton_DI(3,8)= reshape( [ &
       0,0,0, &
       1,0,0, &
       0,1,0, &
       1,1,0, &
       0,0,1, &
       1,0,1, &
       0,1,1, &
       1,1,1 ], [3,8])

  private:: get_batl_tree, &
       MH_get_roots_dd, MH_get_roots_id

contains
  !============================================================================

  subroutine show_domain_decomp(Dd)
    use BATL_lib, ONLY: iProc, DomainSize_D, get_tree_position, MaxDim, &
         CoordMin_D

    type(DomainType),intent(in):: Dd
    real    :: PositionMin_D(MaxDim), PositionMax_D(MaxDim)
    integer :: iNode, iChild
    !--------------------------------------------------------------------------
    if(iProc /= 0) RETURN

    write(*,*)'!!! Starting show_domain_decomp'
    write(*,*)'!!! CompID_            =', Dd%CompID_
    write(*,*)'!!! nDim               =', Dd%nDim
    write(*,*)'!!! CoordMin_D         =', Dd%CoordMin_D
    write(*,*)'!!! CoordMax_D         =', Dd%CoordMax_D
    write(*,*)'!!! iRootMapDim_D      =', Dd%iRootMapDim_D
    write(*,*)'!!! IsTreeDD           =', Dd%IsTreeDD
    write(*,*)'!!! nChildren          =', Dd%nChildren
    write(*,*)'!!! nDim               =', Dd%nDim
    write(*,*)'!!! nTreeNode         =', Dd%nTreeNode
    write(*,*)'!!! nAllocatedNodes    =', Dd%nAllocatedNodes
    write(*,*)'!!! IsPeriodic_D       =', Dd%IsPeriodic_D
    write(*,*)'!!! DoGlueMargins      =', Dd%DoGlueMargins
    write(*,*)'!!! iRealization       =', Dd%iRealization
    write(*,*)'!!! IsLocal            =', Dd%IsLocal
    write(*,*)'!!! nBlockAll          =', Dd%nBlockAll
    if(Dd%IsTreeDD)then
       write(*,*)'!!! iChild, iShift_DI'
       do iChild = 1, Dd%nChildren
          write(*,*) iChild, Dd%iShift_DI(:,iChild)
       end do
    end if
    write(*,*)'!!! iNode, iDD_II'
    do iNode = 1, Dd%nTreeNode
       write(*,*) iNode, Dd%iDD_II(:,iNode)
    end do

    write(*,*)&
         '!!! iNode, CoordBlock_DI,  DCoord_DI BATL/CoordMin_DB BATL/DCoordDB'
    do iNode = 1, Dd%nTreeNode
       call get_tree_position(iNode, PositionMin_D, PositionMax_D)
       PositionMin_D = CoordMin_D + (DomainSize_D)*PositionMin_D
       PositionMax_D = CoordMin_D + (DomainSize_D)*PositionMax_D
       write(*,'(i6,12es13.5)') iNode, &
            Dd%CoordBlock_DI(:,iNode), Dd%DCoordCell_DI(:,iNode), &
            PositionMin_D, (PositionMax_D - PositionMin_D)/Dd%nCell_D
    enddo

    write(*,*)'!!! Done with show_domain_decomp'

  end subroutine show_domain_decomp
  !============================================================================
  subroutine get_batl_tree(Domain)

    ! Avoid name conflict with Parent_ in the SWMF coupling toolkit
    use BATL_tree, ParentBatl_ => Parent_

    type(DomainType),intent(inout)::Domain

    integer:: iNode, iNodeParent, iChild
    !--------------------------------------------------------------------------

    ! Allocate arrays for nNode sized tree
    Domain%nTreeNode = nNode
    call check_octree_allocation(Domain)

    ! Here we assume that there are no holes in the BATL tree
    do iNode = 1, nNode

       iNodeParent = iTree_IA(ParentBatl_,iNode)
       if(iNodeParent == Unset_)then
          ! For root blocks coupling toolkit seems to set parent to itself
          Domain%iDD_II(Parent_,iNode) = iNode
          ! For root blocks coupling toolkit seems to set child index to 0
          Domain%iDD_II(MyNumberAsAChild_,iNode) = 0
       else
          Domain%iDD_II(Parent_,iNode) = iNodeParent
          ! Find child index
          do iChild = 1, nChild
             if(iTree_IA(Child0_+iChild,iNodeParent) == iNode) then
                Domain%iDD_II(MyNumberAsAChild_,iNode)&
                     =iChild
                EXIT
             end if
          end do
       end if

       if(iTree_IA(Status_,iNode) == Unused_)then
          do iChild = 1, nChild
             ! iChildOrder_II may be required here !!!
             Domain%iDD_II(iChild,iNode) = &
                  iTree_IA(Child0_+iChild,iNode)
          end do
       else
          Domain%iDD_II(FirstChild_,iNode)  = None_
          Domain%iDD_II(GlobalBlock_,iNode) = iMortonNode_A(iNode)
          Domain%iDD_II(ProcToolkit_,iNode) = iTree_IA(Proc_,iNode)
          Domain%iDD_II(BLK_,iNode) = iTree_IA(Block_,iNode)
       end if
    end do
    ! Check 04/11/2021: routine works as it should. Igor.
    ! call complete(Domain)
    ! call show_domain_decomp(Domain)
    ! call CON_stop('After domain decomposition')
  end subroutine get_batl_tree
  !============================================================================
  subroutine MH_get_roots_dd(Domain)

    use BATL_lib, ONLY: nIJK_D, IsPeriodic_D, nRoot_D, CoordMin_D, CoordMax_D
    use BATL_geometry, ONLY: IsAnyAxis, IsCylindricalAxis, r_, Theta_, Phi_
    type(DomainType),intent(inout)::Domain
    logical :: DoGlueMargins
    integer :: iDirMinusGlue, iDirPlusGlue, iDirCycle
    !--------------------------------------------------------------------------
    DoGlueMargins = IsAnyAxis
    iDirMinusGlue = 0; iDirPlusGlue = 0; iDirCycle = 0
    if(IsAnyAxis) iDirCycle = Phi_
    if(IsCylindricalAxis)then
       ! IsCylindricalAxis = CoordMin_D(r_) == 0.0
       ! r_ = 1; Phi_ = 2; z_=3
       iDirMinusGlue = r_
    else
       ! IsSphericalAxis = CoordMin_D(Theta_) <   0.01*Unit &
       !         .and.     CoordMax_D(Theta_) > 179.99*Unit
       ! r_ = 1; Theta_ = 2; Phi_ = 3
       ! IsLatitudeAxis  = CoordMin_D(Lat_)   < -89.99*Unit &
       !                   CoordMax_D(Lat_)   >  89.99*Unit
       ! r_ = 1; Phi_ = 2; Theta_ = Lat_ = 3
       iDirMinusGlue = Theta_; iDirPlusGlue = Theta_
    end if
    call get_root_decomposition_dd(&
         Domain,       & ! DD to be constructed
         nRoot_D,                   & ! As in DomainType
         CoordMin_D,                & ! As in DomainType
         CoordMax_D,                & ! As in DomainType
         nIJK_D,                    & ! As in DomainType
         IsPeriodic_D=IsPeriodic_D, &
         iShift_DI=iShiftMorton_DI, &
         DoGlueMargins= DoGlueMargins,&
         iDirMinusGlue= iDirMinusGlue,&
         iDirPlusGlue = iDirPlusGlue ,&
         iDirCycle   = iDirCycle)

  end subroutine MH_get_roots_dd
  !============================================================================
  subroutine MH_get_roots_id(GridID_)

    use BATL_lib, ONLY: nIJK_D, IsPeriodic_D, nRoot_D, CoordMin_D, CoordMax_D
    use BATL_geometry, ONLY: IsAnyAxis, IsCylindricalAxis, r_, Theta_, Phi_
    integer, intent(in):: GridID_
    logical :: DoGlueMargins
    integer :: iDirMinusGlue, iDirPlusGlue, iDirCycle
    !--------------------------------------------------------------------------
    DoGlueMargins = IsAnyAxis
    iDirMinusGlue = 0; iDirPlusGlue = 0; iDirCycle = 0
    if(IsAnyAxis) iDirCycle = Phi_
    if(IsCylindricalAxis)then
       ! IsCylindricalAxis = CoordMin_D(r_) == 0.0
       ! r_ = 1; Phi_ = 2; z_=3
       iDirMinusGlue = r_
    else
       ! IsSphericalAxis = CoordMin_D(Theta_) <   0.01*Unit &
       !         .and.     CoordMax_D(Theta_) > 179.99*Unit
       ! r_ = 1; Theta_ = 2; Phi_ = 3
       ! IsLatitudeAxis  = CoordMin_D(Lat_)   < -89.99*Unit &
       !                   CoordMax_D(Lat_)   >  89.99*Unit
       ! r_ = 1; Phi_ = 2; Theta_ = Lat_ = 3
       iDirMinusGlue = Theta_; iDirPlusGlue = Theta_
    end if
    call get_root_decomposition_id(&
         GridID_,                   & ! DD to be constructed
         nRoot_D,                   & ! As in DomainType
         CoordMin_D,                & ! As in DomainType
         CoordMax_D,                & ! As in DomainType
         nIJK_D,                    & ! As in DomainType
         IsPeriodic_D=IsPeriodic_D, &
         iShift_DI=iShiftMorton_DI, &
         DoGlueMargins= DoGlueMargins,&
         iDirMinusGlue= iDirMinusGlue,&
         iDirPlusGlue = iDirPlusGlue ,&
         iDirCycle   = iDirCycle)

  end subroutine MH_get_roots_id
  !============================================================================

  subroutine MH_update_local_decomposition(Domain)
    use ModMain, ONLY: iNewGrid, iNewDecomposition

    type(DomainType), intent(inout):: Domain

    !--------------------------------------------------------------------------
    if(iNewGrid==iLastGrid .and. iNewDecomposition == iLastDecomposition &
         .and. Domain%iRealization /= 0) &
         RETURN

    call get_batl_tree(Domain)

    Domain%iRealization = mod(Domain%iRealization + 1, 1000)
    iLastDecomposition  = iNewDecomposition
    iLastGrid           = iNewGrid
    call complete(Domain)

  end subroutine MH_update_local_decomposition
  !============================================================================

end module MH_domain_decomposition
!==============================================================================
