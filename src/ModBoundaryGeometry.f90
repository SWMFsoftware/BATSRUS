!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModBoundaryGeometry

  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest, iBlockTest
  use ModBatsrusUtility, ONLY: stop_mpi
#ifdef _OPENACC
  use ModUtilities, ONLY: norm2
#endif

  implicit none
  SAVE

  private ! except

  public:: init_mod_boundary_cells     ! initialize module
  public:: read_boundary_geometry_param! read parameters
  public:: fix_geometry                ! set geometry variables
  public:: fix_block_geometry          ! set geometry variables for a block
  public:: fix_boundary_ghost_cells    ! recalculate "Used_GB" in ghost cells

  ! iBoundary_GB contains the index of the boundary that the cell belongs to.
  integer, allocatable, public :: iBoundary_GB(:,:,:,:)
  !$acc declare create(iBoundary_GB)

  ! Cells inside domain have index domain_ that is smaller than smallest
  ! boundary index (SolidBc_ = -3)
  integer, parameter, public :: domain_ = -10

  ! Local variables -------------------------------

  ! Boundary parameter for SolidState
  character(len=15) :: TypeSolidGeometry = 'none'
  real :: rSolid = -1
  real :: SolidLimitDt = 0

contains
  !============================================================================

  subroutine init_mod_boundary_cells

    use ModSize, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK, MaxBlock
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_mod_boundary_cells'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(.not. allocated(iBoundary_GB)) then
       allocate(iBoundary_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
       iBoundary_GB = domain_
    end if

    call test_stop(NameSub, DoTest)
  end subroutine init_mod_boundary_cells
  !============================================================================

  subroutine read_boundary_geometry_param(NameCommand)

    use ModMain, ONLY: UseSolidState, TypeCellBc_I, TypeFaceBc_I
    use ModMain, ONLY: Coord1MinBc_, xMinBc_, solidBc_
    use ModReadParam, ONLY: read_var
    use BATL_size, ONLY: nDim

    integer :: i

    character(len=*), intent(in):: NameCommand

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_boundary_geometry_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    select case(NameCommand)
    case("#SOLIDSTATE")
       call read_var('UseSolidState', UseSolidState)
       if(UseSolidState)then
          call read_var('TypeBcSolid',TypeFaceBc_I(solidBc_))
          call read_var('TypeSolidGeometry',TypeSolidGeometry)

          select case(TypeSolidGeometry)
          case("sphere")
             call read_var('rSolid',rSolid)
          case("user")
             ! ?
          case default
             call stop_mpi(NameSub//': Command='//NameCommand//&
                  ', Geometry type='//TypeSolidGeometry//&
                  ' has not been implemented!')
          end select

          call read_var('SolidLimitDt',SolidLimitDt)
       end if
    case("#OUTERBOUNDARY")
       do i = Coord1MinBc_, 2*nDim
          call read_var('TypeCellBc', TypeCellBc_I(i))
       end do
    case("#BOXBOUNDARY")
       do i = xMinBc_, xMinBc_-1+2*nDim
          call read_var('TypeFaceBc', TypeFaceBc_I(i))
       end do

    case default
       call stop_mpi(NameSub//' unknown command='//NameCommand)
    end select

    call test_stop(NameSub, DoTest)
  end subroutine read_boundary_geometry_param
  !============================================================================

  subroutine fix_block_geometry(iBlock, DoSolveSolidIn)

    use ModMain, ONLY: UseBody, body1_, body2_, &
         UseBody2, UseExtraBoundary, UseSolidState, &
              TypeFaceBc_I, &
         xMinBc_, xMaxBc_, yMinBc_, yMaxBc_, zMinBc_, zMaxBc_, solidBc_
    use ModGeometry, ONLY: &
         r_GB, rBody2_GB, rMinBody2_B, IsBody_B, &
         IsBoundary_B, IsNoBody_B, &
         xMinBox, xMaxBox, yMinBox, yMaxBox, zMinBox, zMaxBox
    use ModPhysics, ONLY: xBody2,yBody2,zBody2, rbody, rBody2
    use BATL_lib, ONLY: &
         MinI, MaxI, MinJ, MaxJ, MinK, MaxK, nI, nJ, nK, nG, &
         nBlock, Xyz_DGB, CellSize_DB, x_, y_, z_, Used_GB,&
         CoordMin_DB, CoordMax_DB, CoordMin_D, CoordMax_D, IsPeriodic_D
    use ModAdvance, ONLY: DtMax_CB
    use ModUserInterface ! user_set_boundary_cells

    integer, intent(in) :: iBlock
    logical, intent(in), optional :: DoSolveSolidIn

    integer :: i, j, k
    logical :: DoSolveSolid

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'fix_block_geometry'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    DoSolveSolid = .true.
    if(present(DoSolveSolidIn)) DoSolveSolid = DoSolveSolidIn

    if(UseBody2)then
       ! calculate the radius as measured from the second body
       ! Note that the second body can move
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          rBody2_GB(i,j,k,iBlock) = norm2( Xyz_DGB(:,i,j,k,iBlock) - &
               [xBody2, yBody2, zBody2])
       end do; end do; end do
       rMinBody2_B(iBlock) = minval(rBody2_GB(:,:,:,iBlock))
    end if

    IsBoundary_B(iBlock) = any( .not. IsPeriodic_D .and.   &
         (    CoordMin_DB(:,iBlock) <= CoordMin_D + 1e-12       &
         .or. CoordMax_DB(:,iBlock) >= CoordMax_D - 1e-12) )

    if(DoTest)then
       write(*,*)NameSub,': IsBoundary_B=',IsBoundary_B(iBlock)
       write(*,*)NameSub,': CoordMin_DB=', CoordMin_DB(:,iBlock)
       write(*,*)NameSub,': CellSize_D = ',CellSize_DB(:,iBlock)
       write(*,*)NameSub,': CoordMin_D =', CoordMin_D
       write(*,*)NameSub,': CoordMax_D =', CoordMax_D
    end if

    ! Used_GB: if not inside a body or outside the outer face boundary
    ! so true cells are those where iBoundary_GB==domain_
    Used_GB(:,:,:,iBlock)=.true.

    ! Reset for every level of refinement
    iBoundary_GB(:,:,:,iBlock) = domain_

    ! User solid boundary or extra boundary
    if((.not.DoSolveSolid .and.UseSolidState .and.TypeSolidGeometry=='user') &
         .or. UseExtraBoundary) call user_set_boundary_cells(iBlock)

    ! Set boundary type and timestep inside body for solidBC
    if(UseSolidState) then
       if(.not. DoSolveSolid) then
          select case(TypeSolidGeometry)
          case('sphere')
             where( r_GB(:,:,:,iBlock) < rSolid)&
                  iBoundary_GB(:,:,:,iBlock) = solidBc_
             where( r_GB(1:nI,1:nJ,1:nK,iBlock) < rSolid)&
                  DtMax_CB(:,:,:,iBlock) = 0.0
          end select
       else ! DoSolveSolid=.TRUE.
          select case(TypeSolidGeometry)
          case('sphere')
             where( r_GB(1:nI,1:nJ,1:nK,iBlock) < rSolid)&
                  DtMax_CB(:,:,:,iBlock) = SolidLimitDt
          end select
       end if
    end if

    ! Set iBoundary_GB for UseBody and body2 (always face boundary)
    if(UseBody2) then
       where( rBody2_GB(:,:,:,iBlock) < rbody2) &
            iBoundary_GB(:,:,:,iBlock) = body2_
    end if
    if(UseBody) then
       where( r_GB(:,:,:,iBlock) < rbody ) &
            iBoundary_GB(:,:,:,iBlock) = body1_
    end if

    ! No face BC is applied for TypeFaceBc_I(1:6) if not set in PARAM.in
    if(.not. all(TypeFaceBc_I(xMinBc_:zMaxBc_)=='none') )then
       where( Xyz_DGB(x_,:,:,:,iBlock) < xMinBox ) &
            iBoundary_GB(:,:,:,iBlock) = xMinBc_
       where( Xyz_DGB(x_,:,:,:,iBlock) > xMaxBox ) &
            iBoundary_GB(:,:,:,iBlock) = xMaxBc_
       where( Xyz_DGB(y_,:,:,:,iBlock) < yMinBox ) &
            iBoundary_GB(:,:,:,iBlock) = yMinBc_
       where( Xyz_DGB(y_,:,:,:,iBlock) > yMaxBox ) &
            iBoundary_GB(:,:,:,iBlock) = yMaxBc_
       where( Xyz_DGB(z_,:,:,:,iBlock) < zMinBox ) &
            iBoundary_GB(:,:,:,iBlock) = zMinBc_
       where( Xyz_DGB(z_,:,:,:,iBlock) > zMaxBox ) &
            iBoundary_GB(:,:,:,iBlock) = zMaxBc_
    end if

    Used_GB(:,:,:,iBlock) = (iBoundary_GB(:,:,:,iBlock)==domain_)

    ! IsBody_B: if any cell INCLUDING ghost cells is inside body(ies)
    IsBody_B(iBlock) = .not. all(Used_GB(:,:,:,iBlock))

    ! IsNoBody_B: if all cells EXCLUDING ghost cells are outside body(ies)
    IsNoBody_B(iBlock) = all(Used_GB(1:nI,1:nJ,1:nK,iBlock))

    if(DoTest)then
       write(*,*) NameSub,&
            ' finished with iBoundary_GB(iTest-nG:iTest+nG)=',&
            iBoundary_GB(iTest-nG:iTest+nG,jTest,kTest,iBlock)

       write(*,*) NameSub,&
            ' finished with Used_GB(iTest-nG:iTest+nG)=',&
            Used_GB(iTest-nG:iTest+nG,jTest,kTest,iBlock)
    end if

    if(iBlock == nBlock) then
       !$acc update device(Used_GB, IsBody_B, IsNoBody_B, iBoundary_GB)
       !$acc update device(IsBoundary_B)
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine fix_block_geometry
  !============================================================================

  subroutine fix_geometry(DoSolveSolidIn)

    use BATL_lib, ONLY: nBlock

    logical, intent(in) :: DoSolveSolidIn

    integer :: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'fix_geometry'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    do iBlock = 1, nBlock
       call fix_block_geometry(iBlock, DoSolveSolidIn)
    end do

    call test_stop(NameSub, DoTest)
  end subroutine fix_geometry
  !============================================================================

  subroutine fix_boundary_ghost_cells

    ! Recalculate Used_GB information in ghost cells if grid changed.

    use ModMain, ONLY: nBlock, Unused_B, iNewGrid, iNewDecomposition, &
             nIteration, nOrderProlong, UseOpenACC
    use ModGeometry, ONLY: IsBody_B
    use BATL_lib, ONLY: message_pass_cell, Used_GB

    integer:: iBlock
    integer:: iGridHere = -1, iDecompositionHere = -1

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'fix_boundary_ghost_cells'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(iGridHere==iNewGrid .and. iDecompositionHere==iNewDecomposition) RETURN

    iGridHere = iNewGrid; iDecompositionHere = iNewDecomposition

    if(DoTest) write(*,*)NameSub,' starting with Used_GB(i-2:i+2)=', &
         Used_GB(iTest-2:iTest+2,jTest,kTest,iBlockTest)

    ! DoResChangeOnly=true works as long as the ghost cells are correctly set
    ! away from resolution changes. This usually holds, but not if the
    ! boundary cells are set based on the state variables read from a restart
    ! file that has no ghost cell information saved. This can only happen
    ! at the very beginning of a run when nIteration == 0.
    if(nOrderProlong > 1)then
       call message_pass_cell(iBoundary_GB, &
            DoResChangeOnlyIn=nIteration>0, NameOperatorIn='max',&
            UseOpenACCIn=UseOpenACC)
    else
       call message_pass_cell(iBoundary_GB, &
            nProlongOrderIn=1, nCoarseLayerIn=2, &
            DoSendCornerIn=.true., DoRestrictFaceIn=.true., &
            DoResChangeOnlyIn=nIteration>0, NameOperatorIn='max',&
            UseOpenACCIn=.true.)
    end if
    !$acc update device(iBoundary_GB)
    ! We probably need to update iBoundary_GB after message passing on CPU.
    ! The only other place iBoundary_GB is updated on GPU is in fix_block_
    ! geometry. There is at least one instance (in ModLoadBalance) where
    ! fix_boundary_ghost_cell is called AFTER fix_block_geometry.

    if(DoTest) write(*,*) NameSub,': iBoundary_GB(i-2:i+2)=', &
         iBoundary_GB(iTest-2:iTest+2,jTest,kTest,iBlockTest)

    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE

       Used_GB(:,:,:,iBlock) = Used_GB(:,:,:,iBlock)  &
            .and. (iBoundary_GB(:,:,:,iBlock) == domain_)

       IsBody_B(iBlock) = .not. all(Used_GB(:,:,:,iBlock))
    end do

    if(DoTest) write(*,*) NameSub,' finished with Used_GB(i-2:i+2)=', &
         Used_GB(iTest-2:iTest+2,jTest,kTest,iBlockTest)

    call test_stop(NameSub, DoTest)
  end subroutine fix_boundary_ghost_cells
  !============================================================================

end module ModBoundaryGeometry
!==============================================================================

