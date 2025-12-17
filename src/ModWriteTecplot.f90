!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModWriteTecplot

  use BATL_lib, ONLY: &
       test_start, test_stop, lVerbose, iProc, nProc, iComm, &
       MinI, MaxI, MinJ, MaxJ, MinK, MaxK, nIjk_D, MaxBlock, &
       nI, nJ, nK, Unused_B
  use ModBatsrusUtility, ONLY: get_date_time, get_time_string, stop_mpi

  use ModKind, ONLY: Int1_
  use ModMpi

  ! Save cell centered data into Tecplot files
  !
  ! Save cell centers (same as 3D IDL plots, but no cell size).
  ! For single data file:
  !     record size is nDim+nPlotVar reals + 1 newline
  !     record index is going from 1 to nPointAll ordered per processor
  ! For single connectivity file:
  !     record size is 2**nDim integers (global cell indexes) + 1 newline
  !     record index is going from 1 to nBrickAll ordered per processor
  ! Connectivity at block boundaries with no resolution change
  !     is written by the side towards the negative direction
  ! Connectivity at resolution changes is written by the finer side.
  ! The connectivity brick contains i:i+iPlotDim,j:j+jPlotDim,k:k+kPlotDim
  !     cell centers.
  !     Indexes belonging to ghost cells are taken from the neighbor.
  !     These are obtained by message passing the cell indexes.

  use BATL_size, ONLY: nDim

  implicit none

  SAVE

  private ! except

  public:: write_tecplot_init      ! initialize variables
  public:: write_tecplot_count     ! count number of cells written
  public:: write_tecplot_set_mark  ! set the starting position for each cell
  public:: write_tecplot_get_data  ! get data for one block
  public:: write_tecplot_write_data ! write data
  public:: write_tecplot_head      ! write header information
  public:: write_tecplot_connect   ! write connectivity file(s)
  public:: write_tecplot_setinfo   ! set some variables to be written as AUX
  public:: write_tecplot_auxdata   ! write auxiliary information
  public:: write_tecplot_node_data ! write node averaged tecplot output
  public:: assign_node_numbers     ! assign node numbers for node averaged plot
  public:: set_tecplot_var_string  ! set variable and unit names

  character(len=23), public:: StringDateTime
  character(len=22), public:: StringNandT
  character, public, parameter:: CharNewLine = new_line('a')
  integer,   public:: lRecConnect = 2**nDim*11+1

  ! Dimensionality of plot
  integer, public:: nPlotDim = nDim
  !$acc declare create(nPlotDim)

  ! Local variables
  character(len=23):: StringDateTime0

  ! Cuts
  logical:: DoCut
  !$acc declare create(DoCut)

  ! Generalized coordinate limits of the cut box/slice
  real:: CutMin_D(3), CutMax_D(3)
  !$acc declare create(CutMin_D, CutMax_D)

  ! The dimension normal to the cut plane (=0 for no cut or 3D cut box)
  integer:: iCutDim
  !$acc declare create(iCutDim)

  ! iPlot_D is 1 if the plot has non-zero width in that dimension
  integer:: iPlot_D(3)

  ! Global cell index. It is a real array for sake of message passing only.
  real, allocatable:: CellIndex_GB(:,:,:,:)
  !$acc declare create(CellIndex_GB)

  ! Total number of cells written into file
  integer:: nPointAll

  ! Total number of connectivity bricks
  integer:: nBrickAll

  character(len=80):: StringFormat

  ! One byte integer array that stores the ascii codes of the
  ! data to be written
  integer(Int1_), allocatable:: iAscii_I(:)
  !$acc declare create(iAscii_I)

  ! Maximum size allocated for iAscii_I
  integer:: nCharMax
  ! Size of used portion of iAscii_I
  integer:: nChar

  integer:: nCharPerLine
  !$acc declare create(nCharPerLine)

  ! The starting position of each cell's data in iAscii_I
  integer, allocatable:: iMark_GI(:,:,:,:)
  !$acc declare create(iMark_GI)

  ! Corresponding to the format (ES14.6)
  integer, parameter:: nWidthReal = 14, nFrac = 6

  ! Corresponding to the format (i11)
  integer, parameter:: nWidthInt = 11

  integer(MPI_OFFSET_KIND) :: nOffset

  integer:: nBrickStart

  integer, public, parameter:: nBlockPerPatch = 128

contains
  !============================================================================
  subroutine write_tecplot_init(nRealIn, nIntegerIn, nCellOffset)
    integer, optional, intent(in):: nRealIn, nIntegerIn, nCellOffset

    integer:: nReal, nInteger
    integer:: nCharPerLineNew

    !--------------------------------------------------------------------------
    if(.not.allocated(iMark_GI))then
       allocate(iMark_GI(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,nBlockPerPatch))
    end if

    if(.not.allocated(iAscii_I))then
       nCharPerLine = 0
       nCharMax = 0
    end if

    nReal = 0
    if(present(nRealIn)) nReal = nRealIn

    nInteger = 0
    if(present(nIntegerIn)) nInteger = nIntegerIn

    ! We add 1 for the new line character
    nCharPerLineNew = nReal*nWidthReal + nInteger*nWidthInt + 1

    if(nCharPerLineNew /= nCharPerLine)then
       if(nCharPerLineNew > nCharPerLine) then
          ! Reallocate iAscii_I if needed
          nCharMax = (MaxI - MinI + 1)*(MaxJ - MinJ + 1)*(MaxK - MinK + 1)*&
               nCharPerLineNew*nBlockPerPatch

          if(allocated(iAscii_I)) deallocate(iAscii_I)
          allocate(iAscii_I(nCharMax))
       end if
       nCharPerLine = nCharPerLineNew
       !$acc update device(nCharPerLine)
    end if

    nOffset = 0
    if(present(nCellOffset)) then
       ! Note: Since nCellOffset is int4 while nOffset is int8, the following
       ! multiplication can be overflowed when nCellOffset is large.
       ! nOffset = nCellOffset*nCharPerLine
       ! To avoid the overflow, we do it in two steps:
       nOffset = nCellOffset
       nOffset = nOffset*nCharPerLine
    end if
  end subroutine write_tecplot_init
  !============================================================================
  subroutine write_tecplot_count(nCell)
    use BATL_lib, ONLY: nBlock

    integer, optional, intent(out):: nCell

    integer:: i, j, k, iBlock
    !--------------------------------------------------------------------------

    nCell = 0
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(CellIndex_GB(i,j,k,iBlock) == 0) CYCLE
          nCell = nCell + 1
       end do; end do; end do
    end do
  end subroutine write_tecplot_count
  !============================================================================
  subroutine write_tecplot_set_mark(iBlockMin, iBlockMax)
    integer, intent(in):: iBlockMin, iBlockMax

    integer:: i, j, k, iCount, iBlock

    !--------------------------------------------------------------------------
    iMark_GI = 0
    iCount = 1
    do iBlock = iBlockMin, iBlockMax
       if(Unused_B(iBlock)) CYCLE
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(CellIndex_GB(i,j,k,iBlock) == 0) CYCLE
          iMark_GI(i,j,k,iBlock-iBlockMin+1) = iCount
          iCount = iCount + nCharPerLine
       end do; end do; end do
    end do
    nChar = iCount - 1
    !$acc update device(iMark_GI)

  end subroutine write_tecplot_set_mark
  !============================================================================

  subroutine write_tecplot_get_data(iBlock, iBlockMin, nPlotVar, PlotVar_VGB)
    !$acc routine vector

    use BATL_lib, ONLY: MaxDim, nDim, nJ, nK, Xyz_DGB, &
         r_, Phi_, Theta_, Lat_, CoordMin_DB, CoordMax_DB, &
         IsAnyAxis, IsLatitudeAxis, IsSphericalAxis, IsCylindricalAxis
    use ModUtilities, ONLY: real_to_ascii_code
    use ModNumConst, ONLY: cPi, cHalfPi
    use ModIO, ONLY: MaxPlotvar, DoSaveOneTecFile, DoSaveTecBinary
    use ModIoUnit, ONLY: UnitTmp_
    use ModKind, ONLY: Real4_

    integer, intent(in):: iBlock, iBlockMin, nPlotVar
    real,    intent(in):: PlotVar_VGB(&
         nPlotVar,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)

    integer:: i, j, k
    integer:: iRecData, iVar

    real(Real4_):: Xyz_D(MaxDim)
    real(Real4_):: PlotVar_V(MaxPlotvar)

    integer:: iLoc

    ! Index limits inside the block based on the cut
    integer:: IjkMin_D(3), IjkMax_D(3)

    integer:: iMin, iMax, jMin, jMax, kMin, kMax

    integer(int1_), parameter:: iCharNewLine = ichar(CharNewLine)

    ! Interpolation
    integer:: Di, Dj, Dk
    real:: CoefL, CoefR

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_tecplot_get_data'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    IjkMin_D = 1
    IjkMax_D = nIjk_D

    Di = 0; Dj = 0; Dk = 0
    CoefL = 1.0; CoefR = 0.0
    if(DoCut)then
       ! Check if block is inside cut and set interpolation info if needed
       if(do_skip_block(iBlock, Di, Dj, Dk, &
            CoefL, CoefR, IjkMin_D, IjkMax_D)) RETURN
    endif

    iMin = IjkMin_D(1); iMax = IjkMax_D(1)
    jMin = IjkMin_D(2); jMax = IjkMax_D(2)
    kMin = IjkMin_D(3); kMax = IjkMax_D(3)

    if(DoSaveTecBinary)then
#ifndef _OPENACC
       do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
          ! Skip points outside the cut
          if(CellIndex_GB(i,j,k,iBlock) == 0) CYCLE
          call set_xyz_state(iBlock, i, j, k, Di, Dj, Dk, nPlotVar, &
               Xyz_D, PlotVar_V(1:nPlotVar), PlotVar_VGB(:,:,:,:,iBlock), &
               CoefL, CoefR)
          write(UnitTmp_) Xyz_D(1:nDim), PlotVar_V(1:nPlotVar)
       end do; end do; end do
#endif
    else
       !$acc loop vector collapse(3) private(Xyz_D, PlotVar_V)
       do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
          ! Skip points outside the cut
          if(CellIndex_GB(i,j,k,iBlock) == 0) CYCLE
          call set_xyz_state(iBlock, i, j, k, Di, Dj, Dk, nPlotVar, &
               Xyz_D, PlotVar_V(1:nPlotVar), &
               PlotVar_VGB(:,:,:,:,iBlock), CoefL, CoefR)

          iLoc = iMark_GI(i,j,k,iBlock-iBlockMin+1)
          do iVar = 1, nDim
             call real_to_ascii_code(real(Xyz_D(iVar)), nFrac, &
                  nWidthReal, iAscii_I(iLoc:iLoc+nWidthReal-1))
             iLoc = iLoc + nWidthReal
          end do

          do iVar = 1, nPlotVar
             call real_to_ascii_code(real(PlotVar_V(iVar)), nFrac, &
                  nWidthReal, iAscii_I(iLoc:iLoc+nWidthReal-1))
             iLoc = iLoc + nWidthReal
          end do

          iAscii_I(iLoc) = iCharNewLine
       end do; end do; end do
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine write_tecplot_get_data
  !============================================================================
  subroutine write_tecplot_write_data(iUnit)
    use ModIoUnit, ONLY: UnitTmp_

    integer, intent(in):: iUnit

    integer :: iStatus_I(MPI_STATUS_SIZE), iError
    !--------------------------------------------------------------------------

    !$acc update host(iAscii_I)
    call MPI_file_write_at(iUnit, nOffset, iAscii_I, nChar, &
         MPI_INT8_T, iStatus_I, iError)
    nOffset = nOffset + nChar
  end subroutine write_tecplot_write_data
  !============================================================================
  subroutine set_xyz_state(iBlock, i, j, k, Di, Dj, Dk, nPlotVar, &
       Xyz_D, PlotVar_V, PlotVar_VG, CoefL, CoefR)
    !$acc routine seq
    use ModKind, ONLY: Real4_
    use BATL_lib, ONLY: MaxDim, Phi_, Theta_, Lat_, nJ, nK, &
         Xyz_DGB, r_, CoordMin_DB, CoordMax_DB, IsAnyAxis, &
         IsLatitudeAxis, IsSphericalAxis, IsCylindricalAxis
    use ModNumConst, ONLY: cPi, cHalfPi
    use ModIO, ONLY: MaxPlotvar

    ! Interpolate variables and coordinates to cut planes
    ! Set the coordinates and fix them to fill the hole at the pole
    integer, intent(in):: iBlock, i, j, k, Di, Dj, Dk, nPlotVar
    real(Real4_), intent(out):: Xyz_D(MaxDim)
    real(Real4_), intent(out):: PlotVar_V(nPlotVar)
    real, intent(in):: PlotVar_VG(nPlotVar,MinI:MaxI,MinJ:MaxJ,MinK:MaxK)
    real, intent(in):: CoefL, CoefR

    !--------------------------------------------------------------------------
    if(iCutDim == 0)then
       Xyz_D = Xyz_DGB(:,i,j,k,iBlock)
       PlotVar_V = PlotVar_VG(:,i,j,k)
    else
       ! Interpolate using precalculated coefficients
       Xyz_D     = CoefL*Xyz_DGB(:,i  ,j,k,iBlock) &
            +      CoefR*Xyz_DGB(:,i+Di,j+Dj,k+Dk,iBlock)
       PlotVar_V = CoefL*PlotVar_VG(:,i  ,j,k) &
            +      CoefR*PlotVar_VG(:,i+Di,j+Dj,k+Dk)
    end if

    ! No need to push points to the pole if there is no axis
    ! or the 2D cut is along the Phi direction
    if(.not.IsAnyAxis .or. iCutDim == Phi_) RETURN

    if(IsLatitudeAxis)then
       if(  k == 1  .and. CoordMin_DB(Lat_,iBlock) <= -cHalfPi .or. &
            k == nK .and. CoordMax_DB(Lat_,iBlock) >= +cHalfPi) &
            Xyz_D(1:2) = 0
    elseif(IsSphericalAxis)then
       if(  j == 1  .and. CoordMin_DB(Theta_,iBlock) <= 0 .or. &
            j == nJ .and. CoordMax_DB(Theta_,iBlock) >= cPi) &
            Xyz_D(1:2) = 0
    elseif(IsCylindricalAxis)then
       if(  i == 1 .and. CoordMin_DB(r_,iBlock) <= 0) &
            Xyz_D(1:2) = 0
    end if
  end subroutine set_xyz_state
  !============================================================================
  function int2str(i) result(String)

    ! Convert integer to string of the same length

    integer, intent(in):: i
    character(:), allocatable:: String ! return value

    character(40):: StringTmp
    !--------------------------------------------------------------------------
    write(StringTmp,'(i0)') i
    String = trim(StringTmp)

  end function int2str
  !============================================================================
  function real2str(r, StringFormat) result(String)

    ! Convert real to string using StringFormat.
    ! StringFormat should not contain the outside parentheses.

    real, intent(in):: r
    character(len=*):: StringFormat

    character(:), allocatable:: String ! return value

    character(40):: StringTmp
    !--------------------------------------------------------------------------
    write(StringTmp, "("//StringFormat//")") r
    String = trim(StringTmp)

  end function real2str
  !============================================================================
  subroutine write_tecplot_connect(iFile, NameFile)

    use ModAdvance, ONLY: iTypeAdvance_BP, SkippedBlock_
    use ModIO, ONLY: DoSaveOneTecFile, DoSaveTecBinary, &
         TypePlot, PlotRange_EI
    use ModIoUnit, ONLY: UnitTmp_
    use ModUtilities, ONLY: open_file, close_file, int_to_ascii_code, &
         iCharSpace
    use ModMain, ONLY: nBlockMax
    use ModNumConst, ONLY: cHalfPi, cPi
    use BATL_lib, ONLY: nI, nJ, nK, nIJK, k0_, nKp1_, &
         MaxBlock, nBlock, Unused_B, nNodeUsed, &
         CoordMin_DB, CoordMax_DB, Xyz_DGB, &
         IsAnyAxis, IsLatitudeAxis, IsSphericalAxis, IsCylindricalAxis, &
         r_, Phi_, Theta_, Lat_, &
         DiLevelNei_IIIB, message_pass_cell, set_tree_periodic

    ! Write out connectivity file

    integer,          intent(in):: iFile     ! index of plot file
    character(len=*), intent(in):: NameFile  ! name of connectivity file

    integer:: nBlockBefore, iCell, iBlock, iError

    integer:: iBlockMin, iBlockMax, iPatch, nPatch

    integer:: i0, i1, j0, j1, k0, k1, i, j, k

    integer:: IjkMin_D(3), IjkMax_D(3)

    ! Cell index to be written into the connectivity file
    integer :: iCell_G(0:nI+1,0:nJ+1,k0_:nKp1_)
    !$acc declare create(iCell_G)

    ! Number of bricks for this and all processors
    integer:: nBrick
    integer, allocatable:: nBrick_P(:)

    ! Multiple stages may be needed
    integer:: iStage, nStage

    integer:: iPass, nPass

    ! Cut related variables
    integer:: iPlotDim, jPlotDim, kPlotDim
    logical:: IsPlotDim1, IsPlotDim2, IsPlotDim3

    integer, allocatable:: nCell_P(:)

    integer:: iLoc

    integer:: iUnit

    integer :: iStatus_I(MPI_STATUS_SIZE)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_tecplot_connect'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    DoCut = nDim == 3 .and. TypePlot(1:2) /= '3d'
    !$acc update device(DoCut)

    ! Set iPlotDim_D to 0 in ignored dimensions
    iPlot_D = 0
    iPlot_D(1:nDim) = 1
    nPlotDim = nDim

    ! Set the normal direction of a 2D cut plane to 0 as default (no cut)
    iCutDim = 0

    !$acc update device(iCutDim, nPlotDim)

    if(DoTest)write(*,*) NameSub,' starting with NameFile, DoCut=', &
         NameFile, DoCut

    allocate( &
         CellIndex_GB(0:nI+1,0:nJ+1,k0_:nKp1_,MaxBlock))

    if(DoCut)then
       CutMin_D = PlotRange_EI(1:5:2,iFile)
       CutMax_D = PlotRange_EI(2:6:2,iFile)
       !$acc update device(CutMin_D, CutMax_D)

       ! Set iPlot_D to 0 in 0 width cut direction
       where(CutMin_D == CutMax_D) iPlot_D = 0
       nPlotDim = sum(iPlot_D)

       ! Index of the dimension normal to the slice
       if(nPlotDim < nDim)then
          do iCutDim = 1, nDim
             if(iPlot_D(iCutDim) == 0) EXIT
          end do
       end if
       !$acc update device(iCutDim, nPlotDim)

       if(DoTest .or. nPlotDim == 1)then
          write(*,*) NameSub,' CutMin_D=',  CutMin_D
          write(*,*) NameSub,' CutMax_D=',  CutMax_D
          write(*,*) NameSub,' iPlot_D =', iPlot_D
          write(*,*) NameSub,' iCutDim =', iCutDim

          if(nPlotDim == 1) &
               call stop_mpi(NameSub//': only 2D cuts are possible here')
       end if

       ! For more than 1 processors:
       ! in stage 1 count number of cells written by this processor
       ! In stage 2 set global cell index
       nStage = min(2, nProc)

       iCell = 0
       do iStage = 1, nStage
          do iBlock = 1, nBlock
             if(iStage == nStage) CellIndex_GB(:,:,:,iBlock) = 0

             ! Check if block is inside cut and set IjkMin_D, IjkMax_D...
             if(do_skip_block(iBlock, &
                  IjkMin_D=IjkMin_D, IjkMax_D=IjkMax_D)) CYCLE

             if(iStage < nStage)then
                ! Just count the number of cells inside the cut
                iCell = iCell + product(max(0, IjkMax_D - IjkMin_D + 1))
                CYCLE
             end if

             ! Set global cell index for each cell inside the cut
             do k = IjkMin_D(3), IjkMax_D(3)
                do j = IjkMin_D(2), IjkMax_D(2)
                   do i = IjkMin_D(1), IjkMax_D(1)
                      iCell = iCell + 1
                      CellIndex_GB(i,j,k,iBlock) = iCell
                   end do
                end do
             end do
          end do

          if(iStage < nStage)then
             ! Collect the number of cells written by other processors
             allocate(nCell_P(0:nProc-1))
             call MPI_allgather(iCell, 1, MPI_INTEGER, &
                  nCell_P, 1, MPI_INTEGER, iComm, iError)
             if(iProc == 0)then
                iCell = 0
                ! Store total number of points saved
                nPointAll = sum(nCell_P)
             else
                ! Add up number of cells written by the previous processors
                iCell = sum(nCell_P(0:iProc-1))
             end if
             deallocate(nCell_P)
          end if
       end do
       ! Store total number of points saved when running on 1 processor
       if(nStage == 1) nPointAll = iCell
    else
       ! Full 2D or 3D plot
       ! count number of cells written by processors before this one
       nBlockBefore = 0
       if(iProc > 0) nBlockBefore = &
            count(iTypeAdvance_BP(1:nBlockMax,0:iProc-1) /= SkippedBlock_)

       if(DoTest)write(*,*) NameSub,' nBlockBefore=', nBlockBefore

       ! initial cell index
       iCell = nIJK*nBlockBefore

       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          CellIndex_GB(:,:,:,iBlock) = 0
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             iCell = iCell + 1
             CellIndex_GB(i,j,k,iBlock) = iCell
          end do; end do; end do
       end do
    end if

    ! Set the global cell indexes for the ghost cells. First order prolongation
    ! is used, so fine ghost cells are set to the index of the coarse cell.
    ! Note that the coarse ghost cells are not going to be used.
    ! Just to be safe, we use the minimum index which is better than the
    ! average index.
    call message_pass_cell(1, CellIndex_GB, nProlongOrderIn=1, &
         NameOperatorIn='min')

    !$acc update device(CellIndex_GB)

    ! switch off "fake" periodicity so there are no connections
    call set_tree_periodic(.false.)

    ! Some useful scalars
    iPlotDim = iPlot_D(1); jPlotDim = iPlot_D(2); kPlotDim = iPlot_D(3)
    IsPlotDim1 = iPlotDim>0; IsPlotDim2 = jPlotDim>0; IsPlotDim3 = kPlotDim>0

    if(DoSaveOneTecFile)then
       ! Two stages are needed to figure out the global brick indexes
       nStage = 2
       allocate(nBrick_P(0:nProc-1))

       call open_file(File=NameFile, iComm=iComm, &
            NameCaller=NameSub//'_direct_connect', iUnitMpi=iUnit)
    else
       nStage = 1
       ! Open connectivity file
       if(DoSaveTecBinary) then
          call open_file(FILE=NameFile, NameCaller=NameSub//&
               '_connect',access='stream', form='unformatted')
       else
          call open_file(File=NameFile, iComm=MPI_COMM_SELF, &
               NameCaller=NameSub//'_connect', iUnitMpi=iUnit)
       end if
    end if

    nBrick = 0
    do iStage = 1, nStage
       ! For single file, two passes are needed:
       ! In pass 1, we only count the number of bricks and set nOffset for
       ! each processor
       ! In pass 2, write data.

       nOffset = 0
       if(nStage==2 .and. iStage==2) then
          ! Offset in the file for this processor
          ! Note: Since nBrickStart is int4 while nOffset is int8, using
          ! nOffset = nBrickStart*lRecConnect can be overflowed when
          ! nBrickStart is large.
          ! To avoid the overflow, we do it in two steps:
          nOffset = nBrickStart
          nOffset = nOffset*lRecConnect
       end if

       nPass = 2
       if(iStage < nStage) nPass = 1

       ! For each patch, all the data are formatted and stored in iAscii_I
       ! first. To reduce memory usage, we only do nBlockPerPatch blocks
       ! at a time.

       ! For each patch, we do two passes (unless it is the counting stage):
       ! In pass 1, we only count the number of bricks and set iMark_GI
       ! In pass 2, we write the data into iAscii_I using iMark_GI
       nPatch = ceiling(real(nBlock)/real(nBlockPerPatch))
       do iPatch = 1, nPatch; do iPass = 1, nPass
          iBlockMin = (iPatch-1)*nBlockPerPatch + 1
          iBlockMax = min(iPatch*nBlockPerPatch, nBlock)

          if(iPass == 2) then
             !$acc update device(iMark_GI)
          end if

          iLoc = 1
          !$acc parallel loop gang if(iPass==2) private(iCell_G)
          do iBlock = iBlockMin, iBlockMax
             ! Check if block is used and inside cut
             if(do_skip_block(iBlock)) CYCLE

             call set_connect(i0, i1, j0, j1, k0, k1, iBlock, &
                  iCell_G, IsPlotDim1, IsPlotDim2, IsPlotDim3)

             if(DoSaveTecBinary) then
#ifndef _OPENACC
                ! Right now binary format only works for 3D.
                ! Loop over the "lower-left" corner of the bricks
                do k = k0, k1; do j = j0, j1; do i = i0, i1
                   ! Skip bricks that are not fully inside/usable
                   if(any(iCell_G(i:i+iPlotDim,j:j+jPlotDim,k:k+kPlotDim)==0))&
                        CYCLE
                   if(iPass==1) nBrick = nBrick + 1

                   ! In stage 1 only count bricks
                   if(iStage < nStage) CYCLE

                   write(UnitTmp_) &
                        iCell_G(i  ,j  ,k  ), &
                        iCell_G(i+1,j  ,k  ), &
                        iCell_G(i+1,j+1,k  ), &
                        iCell_G(i  ,j+1,k  ), &
                        iCell_G(i  ,j  ,k+1), &
                        iCell_G(i+1,j  ,k+1), &
                        iCell_G(i+1,j+1,k+1), &
                        iCell_G(i  ,j+1,k+1)
                end do; end do; end do
#endif
             else ! ASCII format
                ! Loop over the "lower-left" corner of the bricks
                !$acc loop vector collapse(3)
                do k = k0, k1; do j = j0, j1; do i = i0, i1
                   ! Skip bricks that are not fully inside/usable
                   if(any(iCell_G(i:i+iPlotDim,j:j+jPlotDim,k:k+kPlotDim)==0))&
                        CYCLE
                   if(iPass==1) nBrick = nBrick + 1

                   if(nPass == 2) then
                      if(iPass < nPass) then
                         iMark_GI(i,j,k,iBlock-iBlockMin+1) = iLoc
                         iLoc = iLoc + (2**nPlotDim)*nWidthInt + 1
                         CYCLE
                      else
                         iLoc = iMark_GI(i,j,k,iBlock-iBlockMin+1)
                      end if
                   end if

                   ! In stage 1 only count bricks
                   if(iStage < nStage) CYCLE

                   if(nPlotDim == 3)then
                      call int_to_ascii_code(iCell_G(i  ,j  ,k  ), &
                           nWidthInt, iAscii_I(iLoc:iLoc+nWidthInt-1), .true.)
                      iLoc = iLoc + nWidthInt
                      call int_to_ascii_code(iCell_G(i+1,j  ,k  ), &
                           nWidthInt, iAscii_I(iLoc:iLoc+nWidthInt-1), .true.)
                      iLoc = iLoc + nWidthInt
                      call int_to_ascii_code(iCell_G(i+1,j+1,k  ), &
                           nWidthInt, iAscii_I(iLoc:iLoc+nWidthInt-1), .true.)
                      iLoc = iLoc + nWidthInt
                      call int_to_ascii_code(iCell_G(i  ,j+1,k  ), &
                           nWidthInt, iAscii_I(iLoc:iLoc+nWidthInt-1), .true.)
                      iLoc = iLoc + nWidthInt
                      call int_to_ascii_code(iCell_G(i  ,j  ,k+1), &
                           nWidthInt, iAscii_I(iLoc:iLoc+nWidthInt-1), .true.)
                      iLoc = iLoc + nWidthInt
                      call int_to_ascii_code(iCell_G(i+1,j  ,k+1), &
                           nWidthInt, iAscii_I(iLoc:iLoc+nWidthInt-1), .true.)
                      iLoc = iLoc + nWidthInt
                      call int_to_ascii_code(iCell_G(i+1,j+1,k+1), &
                           nWidthInt, iAscii_I(iLoc:iLoc+nWidthInt-1), .true.)
                      iLoc = iLoc + nWidthInt
                      call int_to_ascii_code(iCell_G(i  ,j+1,k+1), &
                           nWidthInt, iAscii_I(iLoc:iLoc+nWidthInt-1), .true.)
                      iLoc = iLoc + nWidthInt
                      iAscii_I(iLoc) = ichar(CharNewLine)
                      iLoc = iLoc + 1
                   elseif(.not.IsPlotDim3)then
                      call int_to_ascii_code(iCell_G(i  ,j  ,k), &
                           nWidthInt, iAscii_I(iLoc:iLoc+nWidthInt-1), .true.)
                      iLoc = iLoc + nWidthInt
                      call int_to_ascii_code(iCell_G(i+1,j  ,k), &
                           nWidthInt, iAscii_I(iLoc:iLoc+nWidthInt-1), .true.)
                      iLoc = iLoc + nWidthInt
                      call int_to_ascii_code(iCell_G(i+1,j+1,k), &
                           nWidthInt, iAscii_I(iLoc:iLoc+nWidthInt-1), .true.)
                      iLoc = iLoc + nWidthInt
                      call int_to_ascii_code(iCell_G(i  ,j+1,k), &
                           nWidthInt, iAscii_I(iLoc:iLoc+nWidthInt-1), .true.)
                      iLoc = iLoc + nWidthInt
                      iAscii_I(iLoc) = ichar(CharNewLine)
                      iLoc = iLoc + 1
                   elseif(.not.IsPlotDim2)then
                      call int_to_ascii_code(iCell_G(i  ,j  ,k  ), &
                           nWidthInt, iAscii_I(iLoc:iLoc+nWidthInt-1), .true.)
                      iLoc = iLoc + nWidthInt
                      call int_to_ascii_code(iCell_G(i+1,j  ,k  ), &
                           nWidthInt, iAscii_I(iLoc:iLoc+nWidthInt-1), .true.)
                      iLoc = iLoc + nWidthInt
                      call int_to_ascii_code(iCell_G(i+1,j  ,k+1), &
                           nWidthInt, iAscii_I(iLoc:iLoc+nWidthInt-1), .true.)
                      iLoc = iLoc + nWidthInt
                      call int_to_ascii_code(iCell_G(i  ,j  ,k+1), &
                           nWidthInt, iAscii_I(iLoc:iLoc+nWidthInt-1), .true.)
                      iLoc = iLoc + nWidthInt
                      iAscii_I(iLoc) = ichar(CharNewLine)
                      iLoc = iLoc + 1
                   elseif(.not.IsPlotDim1)then
                      call int_to_ascii_code(iCell_G(i,j  ,k  ), &
                           nWidthInt, iAscii_I(iLoc:iLoc+nWidthInt-1), .true.)
                      iLoc = iLoc + nWidthInt
                      call int_to_ascii_code(iCell_G(i,j+1,k  ), &
                           nWidthInt, iAscii_I(iLoc:iLoc+nWidthInt-1), .true.)
                      iLoc = iLoc + nWidthInt
                      call int_to_ascii_code(iCell_G(i,j+1,k+1), &
                           nWidthInt, iAscii_I(iLoc:iLoc+nWidthInt-1), .true.)
                      iLoc = iLoc + nWidthInt
                      call int_to_ascii_code(iCell_G(i,j  ,k+1), &
                           nWidthInt, iAscii_I(iLoc:iLoc+nWidthInt-1), .true.)
                      iLoc = iLoc + nWidthInt
                      iAscii_I(iLoc) = ichar(CharNewLine)
                      iLoc = iLoc + 1
                   end if
                end do; end do; end do

             end if

          end do ! iBlock

          if(iPass == 1) nChar = iLoc - 1

          if(iPass == nPass) then
             !$acc update host(iAscii_I(1:nChar))
             call MPI_file_write_at(iUnit, nOffset, iAscii_I, nChar, &
                  MPI_INT8_T, MPI_STATUS_IGNORE, iError)
             nOffset = nOffset + nChar
          end if
       end do; end do ! iPatch, iPass

       if(iStage < nStage)then
          ! Collect number of bricks from all processors
          call MPI_allgather(nBrick, 1, MPI_INTEGER, &
               nBrick_P, 1, MPI_INTEGER, iComm, iError)
          ! Add up number of bricks on previous prorecssors
          if(iProc > 0) nBrickStart = sum(nBrick_P(0:iProc-1))
       end if
    end do ! iStage

    if(DoSaveTecBinary) then
       call close_file
    else
       call MPI_file_close(iUnit, iError)
    end if

    ! Reset periodicity as it was
    call set_tree_periodic(.true.)

    ! Calculate and store total number of bricks for header file
    if(DoSaveOneTecFile)then
       if(iProc == 0) nBrickAll = sum(nBrick_P)
       deallocate(nBrick_P)
    else
       nBrickAll = nBrick
       if(nProc > 1) call mpi_reduce_integer_scalar(nBrickAll, &
            MPI_SUM, 0, iComm, iError)
    end if

    ! Calculate and store total number of cells for header file
    if(.not. DoCut .and. iProc == 0) nPointAll = nNodeUsed*nIJK

    if(DoTest)write(*,*) NameSub,' done with nPointAll, nBrickAll=', &
         nPointAll, nBrickAll

    call test_stop(NameSub, DoTest)
  end subroutine write_tecplot_connect
  !============================================================================
  subroutine set_connect(i0, i1, j0, j1, k0, k1, iBlock, iCell_G, &
       IsPlotDim1, IsPlotDim2, IsPlotDim3)
    !$acc routine seq
    use ModNumConst, ONLY: cPi, cHalfPi
    use BATL_lib, ONLY: nI, nJ, k0_, nKp1_, &
         DiLevelNei_IIIB, &
         Xyz_DGB, Lat_, Theta_, r_, Phi_, &
         CoordMin_DB, CoordMax_DB, &
         IsAnyAxis, IsLatitudeAxis, IsSphericalAxis, IsCylindricalAxis

    integer, intent(out):: i0, i1, j0, j1, k0, k1
    integer, intent(in)::  iBlock
    integer, intent(inout):: iCell_G(0:nI+1,0:nJ+1,k0_:nKp1_)
    logical, intent(in):: IsPlotDim1, IsPlotDim2, IsPlotDim3

    ! Get the index limits for the lower left corner
    ! of the connectivity bricks.

    ! Start connectivity brick from index 1 unless coarser left neighbor
    !--------------------------------------------------------------------------
    i0 = 1
    if(IsPlotDim1 .and. DiLevelNei_IIIB(-1,0,0,iBlock) == 1) i0 = 0
    j0 = 1
    if(IsPlotDim2 .and. DiLevelNei_IIIB(0,-1,0,iBlock) == 1) j0 = 0
    k0 = 1
    if(IsPlotDim3 .and. DiLevelNei_IIIB(0,0,-1,iBlock) == 1) k0 = 0

    ! Finish at nI unless there is no block on the right or it is finer
    i1 = nI
    if(IsPlotDim1 .and. DiLevelNei_IIIB(1,0,0,iBlock) < 0) i1 = nI-1
    j1 = nJ
    if(IsPlotDim2 .and. DiLevelNei_IIIB(0,1,0,iBlock) < 0) j1 = nJ-1
    k1 = nK
    if(IsPlotDim3 .and. DiLevelNei_IIIB(0,0,1,iBlock) < 0) k1 = nK-1

    ! For cuts in the Phi direction we want connectivity across poles
    ! Both sides are at either minimum or maximum so we use the
    ! Y coordinate to select which side takes care of the connection
    ! We assume no resolution change across the poles for simplicity
    if(iCutDim == Phi_ .and. IsAnyAxis &
         .and. Xyz_DGB(2,1,1,1,iBlock) > 0)then
       if(IsLatitudeAxis)then
          if(CoordMin_DB(Lat_,iBlock) <= -cHalfPi) k0 = 0
          if(CoordMax_DB(Lat_,iBlock) >= +cHalfPi) k1 = nK - 1
       elseif(IsSphericalAxis)then
          if(CoordMin_DB(Theta_,iBlock) <= 0)   j0 = 0
          if(CoordMax_DB(Theta_,iBlock) >= cPi) j1 = nJ - 1
       elseif(IsCylindricalAxis)then
          if(CoordMin_DB(r_,iBlock) <= 0) i0 = 0
       end if
    end if

    iCell_G = nint(CellIndex_GB(:,:,:,iBlock))

    ! Coarse ghost cells should not be used.
    ! The face ghost cells are surely not used due to above settings
    ! The "left" edge and corner ghost cells are only used if the
    ! left face neighbor is coarser, so these cannot be prolonged cells.
    ! So only "right" edge and corner ghost cells have to be zeroed out.

    if(IsPlotDim1 .and. IsPlotDim2 .and. &
         DiLevelNei_IIIB(1,1,0,iBlock) < 0) iCell_G(nI+1,nJ+1,:) = 0
    if(IsPlotDim1 .and. IsPlotDim3 .and. &
         DiLevelNei_IIIB(1,0,1,iBlock) < 0) iCell_G(nI+1,:,nKp1_) = 0
    if(IsPlotDim2 .and. IsPlotDim3 .and. &
         DiLevelNei_IIIB(0,1,1,iBlock) < 0) iCell_G(:,nJ+1,nKp1_) = 0
    if(nPlotDim == 3 .and. &
         DiLevelNei_IIIB(1,1,1,iBlock) < 0) iCell_G(nI+1,nJ+1,nKp1_) = 0
  end subroutine set_connect
  !============================================================================

  subroutine write_tecplot_head(NameFile, StringUnit)

    use ModIoUnit, ONLY: UnitTmp_
    use ModUtilities, ONLY: open_file, close_file
    use ModIO, ONLY: DoSaveTecBinary

    ! Write out tecplot header file

    character(len=*), intent(in):: NameFile
    character(len=*), intent(in):: StringUnit

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_tecplot_head'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(allocated(CellIndex_GB)) deallocate(CellIndex_GB)

    call write_tecplot_setinfo
    if(iProc /= 0) RETURN

    if(DoSaveTecBinary)then
       ! Actually only works for 3D right now.
       ! hyzhou: This is supposed to be cell-centered data, but why is the node
       ! number and element number different from expected?
       call open_file(FILE=NameFile, access='stream', form='unformatted')
       if(DoCut)then
          write(UnitTmp_) 'TITLE="BATSRUS: cut Data, '//StringDateTime//'"',&
               CharNewLine
       else
          write(UnitTmp_) 'TITLE="BATSRUS: ', int2str(nDim),&
               'D Data,'//StringDateTime//'"', CharNewLine
       end if
       write(UnitTmp_) trim(StringUnit), CharNewLine
       select case(nPlotDim)
       case(2)
          write(UnitTmp_) &
               'ZONE T="2D   '//StringNandT//'"', &
               ', N=', int2str(nPointAll), &
               ', E=', int2str(nBrickAll), &
               ', F=FEPOINT, ET=QUADRILATERAL', CharNewLine
       case(3)
          write(UnitTmp_) &
               'ZONE T="3D   '//StringNandT//'"', &
               ', N=', int2str(nPointAll), &
               ', E=', int2str(nBrickAll), &
               ', F=FEPOINT, ET=BRICK', CharNewLine
       end select
    else
       call open_file(File=NameFile)

       if(DoCut)then
          write(UnitTmp_,'(a)')'TITLE="BATSRUS: cut Data, ' &
               //StringDateTime//'"'
       else
          write(UnitTmp_,'(a,i1,a)') &
               'TITLE="BATSRUS: ', nDim,'D Data,'//StringDateTime//'"'
       end if
       write(UnitTmp_,'(a)') trim(StringUnit)
       select case(nPlotDim)
       case(2)
          write(UnitTmp_,'(a,a,i12,a,i12,a)') &
               'ZONE T="2D   '//StringNandT//'"', &
               ', N=', nPointAll, &
               ', E=', nBrickAll, &
               ', F=FEPOINT, ET=QUADRILATERAL'
       case(3)
          write(UnitTmp_,'(a,a,i12,a,i12,a)') &
               'ZONE T="3D   '//StringNandT//'"', &
               ', N=', nPointAll, &
               ', E=', nBrickAll, &
               ', F=FEPOINT, ET=BRICK'
       end select
    end if

    call write_tecplot_auxdata
    call close_file

    call test_stop(NameSub, DoTest)
  end subroutine write_tecplot_head
  !============================================================================
  subroutine write_tecplot_auxdata(iUnitIn)

    use ModMain, ONLY: IsTimeAccurate, nStep, nOrder, UseRotatingBc, &
         TypeCoordSystem
    use ModGeometry, ONLY: nUsedCell
    use ModFaceValue, ONLY: TypeLimiter, LimiterBeta
    use ModPhysics, ONLY: &
         ThetaTilt, Rbody, ClightFactor, BodyNDim_I, Gamma_I
    use ModBorisCorrection, ONLY: UseBorisCorrection, UseBorisSimple
    use ModAdvance, ONLY: TypeFlux
    use ModIoUnit, ONLY: UnitTmp_
    use ModIO, ONLY: StringDateOrTime, DoSaveTecBinary
    use ModNumConst, ONLY: cRadToDeg
    use BATL_lib, ONLY: nProc, nI, nJ, nK, nIJK, nNodeUsed

    integer, intent(in), optional:: iUnitIn

    character(len=8) :: StringDate
    character(len=10):: StringTime
    integer:: iUnitHere

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_tecplot_auxdata'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if (present(iUnitIn)) then
       iUnitHere = iUnitIn
    else
       iUnitHere = UnitTmp_
    end if

    if(DoSaveTecBinary) then
       ! DATATYPE
       write(iUnitHere) 'AUXDATA TYPE="SINGLE"', CharNewline

       ! BLOCKS
       write(iUnitHere) &
            'AUXDATA BLOCKS="',int2str(nNodeUsed),'  ',int2str(nI),' x',&
            int2str(nJ),' x',int2str(nK),'"', CharNewLine

       ! BODYDENSITY
       write(iUnitHere) 'AUXDATA BODYNUMDENSITY="',&
            real2str(BodyNDim_I(1), "f8.2"), '"', CharNewLine

       ! BORIS
       if(UseBorisCorrection .or. UseBorisSimple) then
          write(iUnitHere) 'AUXDATA BORIS="T ',real2str(ClightFactor, "f8.4"),&
               CharNewLine
       else
          write(iUnitHere) 'AUXDATA BORIS="F"', CharNewLine
       end if

       ! BTHETATILT
       write(iUnitHere) 'AUXDATA BTHETATILT="', &
            real2str(ThetaTilt*cRadToDeg, "f12.4"), '"', CharNewLine

       ! CELLS
       write(iUnitHere) 'AUXDATA CELLS="',int2str(nNodeUsed*nIJK),&
            '"', CharNewLine

       ! CELLSUSED
       write(iUnitHere) 'AUXDATA CELLSUSED="', int2str(nUsedCell),'"', &
            CharNewLine

       ! CODEVERSION
       write(iUnitHere) 'AUXDATA CODEVERSION="BATSRUS"', CharNewLine

       ! COORDSYSTEM
       write(iUnitHere) &
            'AUXDATA COORDSYSTEM="',TypeCoordSystem,'"', CharNewLine

       ! COROTATION
       if(UseRotatingBc)then
          write(iUnitHere) 'AUXDATA COROTATION="T"', CharNewLine
       else
          write(iUnitHere) 'AUXDATA COROTATION="F"', CharNewLine
       end if

       ! TypeFlux
       write(iUnitHere) 'AUXDATA TypeFlux="',TypeFlux,'"', CharNewLine

       ! GAMMA
       write(iUnitHere) 'AUXDATA GAMMA="', real2str(Gamma_I(1), "f14.6"), &
            '"', CharNewLine

       ! ITER
       write(iUnitHere) 'AUXDATA ITER="',int2str(nStep),'"', CharNewLine

       ! NPROC
       write(iUnitHere) 'AUXDATA NPROC="',int2str(nProc),'"', CharNewLine

       ! ORDER
       if(nOrder > 1)then
          write(iUnitHere) 'AUXDATA ORDER="', int2str(nOrder),' ', &
               trim(TypeLimiter),', beta=', real2str(LimiterBeta, "f8.5"), &
               '"', CharNewLine
       else
          write(iUnitHere) 'AUXDATA ORDER="',int2str(nOrder),'"', CharNewLine
       end if

       ! RBODY
       write(iUnitHere) 'AUXDATA RBODY="', real2str(rBody, "f12.2"), &
            '"', CharNewLine

       ! SAVEDATE
       call Date_and_time(StringDate, StringTime)
       write(iUnitHere) &
            'AUXDATA SAVEDATE="','Save Date: ', StringDate(1:4),'/',&
            StringDate(5:6),'/', StringDate(7:8), &
            ' at ',  StringTime(1:2),':',StringTime(3:4),':',StringTime(5:6),&
            '"', CharNewLine

       ! TIMEEVENT
       write(iUnitHere) 'AUXDATA TIMEEVENT="',StringDateTime,'"', CharNewLine

       ! TIMEEVENTSTART
       write(iUnitHere) 'AUXDATA TIMEEVENTSTART="',StringDateTime0,'"', &
            CharNewLine

       ! TIMESIM
       if(IsTimeAccurate)then
          write(iUnitHere) 'AUXDATA TIMESIM="T=',&
               StringDateOrTime(1:4)//":"// &
               StringDateOrTime(5:6)//":"// &
               StringDateOrTime(7:8),        &
               '"', CharNewLine
       else
          write(iUnitHere) 'AUXDATA TIMESIM="T= N/A"', CharNewLine
       end if

       ! TIMESIMSHORT
       if(IsTimeAccurate)then
          write(iUnitHere) 'AUXDATA TIMESIMSHORT="T=',&
               StringDateOrTime(1:4)//":"// &
               StringDateOrTime(5:6),'"', CharNewLine
       else
          write(iUnitHere) 'AUXDATA TIMESIMSHORT="T= SS"', CharNewLine
       end if
    else
       ! BLOCKS
       write(iUnitHere,'(a,i12,3(a,i2),a)') 'AUXDATA BLOCKS="',nNodeUsed,'  ',&
            nI,' x',nJ,' x',nK,'"'

       ! BODYDENSITY
       write(iUnitHere,'(a,(f12.2),a)') &
            'AUXDATA BODYNUMDENSITY="',BodyNDim_I(1),'"'

       ! BORIS
       if(UseBorisCorrection .or. UseBorisSimple)then
          write(iUnitHere,'(a,f8.4,a)') 'AUXDATA BORIS="T ',ClightFactor,'"'
       else
          write(iUnitHere,'(a,a,a)') 'AUXDATA BORIS="F"'
       end if

       ! BTHETATILT
       write(iUnitHere,'(a,f12.4,a)') &
            'AUXDATA BTHETATILT="',ThetaTilt*cRadToDeg,'"'

       ! CELLS
       write(iUnitHere,'(a,i12,a)') 'AUXDATA CELLS="',nNodeUsed*nIJK,'"'

       ! CELLSUSED
       write(iUnitHere,'(a,i12,a)') 'AUXDATA CELLSUSED="',nUsedCell,'"'

       ! CODEVERSION
       write(iUnitHere,'(a)') 'AUXDATA CODEVERSION="BATSRUS"'

       ! COORDSYSTEM
       write(iUnitHere,'(a,a,a)') &
            'AUXDATA COORDSYSTEM="',TypeCoordSystem,'"'

       ! COROTATION
       if(UseRotatingBc)then
          write(iUnitHere,'(a)') 'AUXDATA COROTATION="T"'
       else
          write(iUnitHere,'(a)') 'AUXDATA COROTATION="F"'
       end if

       ! TypeFlux
       write(iUnitHere,'(a,a,a)') 'AUXDATA TypeFlux="', TypeFlux,'"'

       ! GAMMA
       write(iUnitHere,'(a,f14.6,a)') 'AUXDATA GAMMA="', Gamma_I(1),'"'

       ! ITER
       write(iUnitHere,'(a,i12,a)') 'AUXDATA ITER="', nStep,'"'

       ! NPROC
       write(iUnitHere,'(a,i12,a)') 'AUXDATA NPROC="', nProc,'"'

       ! ORDER
       if(nOrder > 1)then
          write(iUnitHere,'(a,i12,a,f8.5,a)') 'AUXDATA ORDER="', nOrder,&
               ' '//trim(TypeLimiter)//', beta=',LimiterBeta,'"'
       else
          write(iUnitHere,'(a,i12,a)') 'AUXDATA ORDER="',nOrder,'"'
       end if

       ! RBODY
       write(iUnitHere,'(a,f12.2,a)') 'AUXDATA RBODY="',rBody,'"'

       ! SAVEDATE
       call Date_and_time(StringDate, StringTime)
       write(iUnitHere,'(a,a11,a4,a1,a2,a1,a2,a4,a2,a1,a2,a1,a2,a)')&
            'AUXDATA SAVEDATE="','Save Date: ', StringDate(1:4),'/',&
            StringDate(5:6),'/', StringDate(7:8), ' at ',  &
            StringTime(1:2),':',StringTime(3:4),':',StringTime(5:6),'"'

       ! TIMEEVENT
       write(iUnitHere,'(a,a,a)') 'AUXDATA TIMEEVENT="', StringDateTime,'"'

       ! TIMEEVENTSTART
       write(iUnitHere,'(a,a,a)') &
            'AUXDATA TIMEEVENTSTART="',StringDateTime0,'"'

       ! TIMESIM
       if(IsTimeAccurate)then
          write(iUnitHere,'(a,a,a)') 'AUXDATA TIMESIM="',&
               'T='// &
               StringDateOrTime(1:4)//":"// &
               StringDateOrTime(5:6)//":"// &
               StringDateOrTime(7:8),'"'
       else
          write(iUnitHere,'(a)') 'AUXDATA TIMESIM="T= N/A"'
       end if

       ! TIMESIMSHORT
       if(IsTimeAccurate)then
          write(iUnitHere,'(a,a,a)') &
               'AUXDATA TIMESIMSHORT="',&
               'T='// &
               StringDateOrTime(1:4)//":"// &
               StringDateOrTime(5:6),'"'
       else
          write(iUnitHere,'(a)') 'AUXDATA TIMESIMSHORT=" SS"'
       end if
    endif

    call test_stop(NameSub, DoTest)
  end subroutine write_tecplot_auxdata
  !============================================================================
  subroutine write_tecplot_setinfo

    use ModMain, ONLY: nStep, IsTimeAccurate, iStartTime_I
    use ModIO, ONLY: StringDateOrTime
    use ModGeometry, ONLY: count_true_cells

    integer:: iTime_I(7)
    character(len=80):: StringFormat
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_tecplot_setinfo'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    call count_true_cells

    if(iProc /= 0) RETURN

    ! Create text string for zone name like 'N=0002000 T=0000:05:00'
    if(IsTimeAccurate)then
       call get_time_string
       write(StringNandT,'(a,i7.7,a)') "N=",nStep," T="// &
            StringDateOrTime(1:4)//":"// &
            StringDateOrTime(5:6)//":"// &
            StringDateOrTime(7:8)
    else
       write(StringNandT,'(a,i7.7)') &
            "N=",nStep
    end if

    StringFormat='(i4.4,"/",i2.2,"/",i2.2," ",i2.2,":",i2.2,":",i2.2,".",i3.3)'
    call get_date_time(iTime_I)
    write(StringDateTime0,StringFormat) iStartTime_I
    write(StringDateTime ,StringFormat) iTime_I

    call test_stop(NameSub, DoTest)
  end subroutine write_tecplot_setinfo
  !============================================================================
  logical function do_skip_block(iBlock, Di, Dj, Dk, CoefL, CoefR, &
       IjkMin_D, IjkMax_D)
    !$acc routine seq

    ! Set the logical value to false if the block is unused
    ! or if it is outside the cut.
    ! If the block intersects the cut, then set IjkMin_D and IjkMax_D
    ! index limits.
    ! If the index shifts Di, Dj, Dk and the interpolation coefficients
    ! CoefL and CoefR are present
    ! and iCutDim is not zero, set the index shifts and
    ! interpolation coefficients to interpolate onto the cut plane.

    use BATL_lib, ONLY: Unused_B, CoordMin_DB, CoordMax_DB, CellSize_DB, &
         Phi_
    use ModNumConst, ONLY: i_DD, cPi, cHalfPi

    integer, intent(in):: iBlock

    integer, intent(out), optional:: Di, Dj, Dk
    real,    intent(out), optional:: CoefL, CoefR
    integer, intent(out), optional:: IjkMin_D(3), IjkMax_D(3)

    ! Arrays describing block geometry
    real:: BlockMin_D(3), BlockMax_D(3), CellSize_D(3)

    real:: PhiCut, PhiBlock, CutCoordNorm
    !--------------------------------------------------------------------------
    do_skip_block = UnUsed_B(iBlock)
    if(do_skip_block) RETURN

    ! There is nothing else to check if there is no cut
    if(.not.DoCut) RETURN

    ! Block coordinates
    BlockMin_D = CoordMin_DB(:,iBlock)
    BlockMax_D = CoordMax_DB(:,iBlock)

    ! Shift Phi coordinate for 2D cuts along the Phi coordinate
    ! e.g. x=0 and y=0 cuts of a spherical grid.
    if(iCutDim == Phi_)then
       ! Blocks "far" from the phi cut are shifted 180 degrees towards cut
       ! This will capture both sides of the sphere/cylinder and is the
       ! expected behavior for x=0 and y=0 cuts. But also works for other.
       PhiCut = CutMin_D(Phi_)
       PhiBlock = 0.5*(BlockMin_D(Phi_)+BlockMax_D(Phi_))

       if(PhiBlock - PhiCut > cHalfPi)then
          BlockMin_D(Phi_) = BlockMin_D(Phi_) - cPi
          BlockMax_D(Phi_) = BlockMax_D(Phi_) - cPi
       elseif(PhiCut - PhiBlock > cHalfPi)then
          BlockMin_D(Phi_) = BlockMin_D(Phi_) + cPi
          BlockMax_D(Phi_) = BlockMax_D(Phi_) + cPi
       end if

    end if

    ! Check if block is inside the cut
    do_skip_block = any(BlockMin_D > CutMax_D) .or. any(BlockMax_D < CutMin_D)
    if(do_skip_block) RETURN

    ! Calculate start and ending indexes of cut
    ! The +-0.01 is used to avoid rounding errors and make sure
    ! that there are at least 1 cells in zero-width directions,
    ! and 2 cells in non-zero width directions.
    CellSize_D = CellSize_DB(:,iBlock)
    if(present(IjkMin_D)) then
       IjkMin_D = max(1, &
            nint(-0.01 +          (CutMin_D - BlockMin_D)/CellSize_D))
    end if

    if(present(IjkMax_D)) then
       IjkMax_D = min(nIJK_D, &
            nint(0.01 + iPlot_D + (CutMax_D - BlockMin_D)/CellSize_D))
    end if

    ! Nothing else to do if there is no 2D cut or Di is not present
    if(iCutDim == 0 .or. .not. present(Di)) RETURN

    ! Cell index shift in the normal direction for interpolation
    Di = i_DD(1,iCutDim); Dj = i_DD(2,iCutDim); Dk = i_DD(3,iCutDim)

    ! Normalized (to block index) coordinate of the cut
    CutCoordNorm = 0.5 + &
         (CutMin_D(iCutDim) - BlockMin_D(iCutDim))/CellSize_D(iCutDim)
    CoefR = CutCoordNorm - IjkMin_D(iCutDim)
    CoefL = 1 - CoefR

  end function do_skip_block
  !============================================================================
  subroutine write_tecplot_node_data( &
       iFile, nPlotVar, PlotVarTec_GV, PlotVarNodes_VNB, PlotXYZNodes_DNB, &
       StringUnitTec, xMin, xMax, yMin, yMax, zMin, zMax, iUnit)

    ! NOTE:
    ! This routine assumes that the blocks are sorted on PEs by their global
    ! block number, ie blocks 1 to n on PE 0, blocks n+1 to n+m on PE 1,
    ! etc.

    use ModPhysics, ONLY: No2Io_V, UnitX_, &
         ThetaTilt
    use ModAdvance, ONLY: iTypeAdvance_B, SkippedBlock_
    use ModIO
    use ModIoUnit, ONLY: UnitTmp_, UnitTmp2_
    use ModNodes, ONLY: nNodeAll, iNodeGlobal_NB, IsNodeUnique_NB
    use BATL_lib, ONLY: IsCartesianGrid, IsRLonLat,                    &
         nI, nJ, nK, nBlock, nNodeUsed, iNodeMorton_I,                 &
         iTree_IA, Block_, Proc_,                                      &
         Xyz_DGB, MinI, MaxI, MinJ, MaxJ, MinK, MaxK,                  &
         find_grid_block, iMortonNode_A, iNode_B
    use ModKind, ONLY: Real4_

    ! Arguments
    integer, intent(in):: iFile, nPlotVar
    character(len=1000), intent(in):: StringUnitTec
    real, intent(in):: PlotVarTec_GV(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxPlotvar)
    real, intent(in):: PlotVarNodes_VNB(MaxPlotvar,nI+1,nJ+1,nK+1,nBlock)
    real, intent(in):: PlotXYZNodes_DNB(3,nI+1,nJ+1,nK+1,nBlock)
    real, intent(in):: xMin, xMax, yMin, yMax, zMin, zMax
    integer, intent(in):: iUnit

    ! Note: the final output data is in single precision, so in fact it is a
    ! waste of memory to pass in double precision arrays.

    ! Local Variables
    integer:: i, j, k, Ijk1, Ijk2, iPE, iBlock, iBlockAll, iNode
    integer:: nBlockCut, iError
    real:: CutValue, Factor1,Factor2
    integer, allocatable:: iBlockCut_A(:)

    ! parameters for saving 3d tecplot in a single file
    character(len=80):: StringFormat
    integer:: iRec

    integer:: i1, i2, j1, j2, k1, k2, nCuts, nCutsTotal
    real:: XarbP,YarbP,ZarbP, XarbNormal,YarbNormal,ZarbNormal, Xp,Yp,Zp
    real(Real4_):: NodeXYZ_DN(3,nI+1,nJ+1,nK+1)
    logical:: DoDebug

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_tecplot_node_data'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) TypePlot,TypePlot(1:3)

    call write_tecplot_setinfo

    select case(TypePlot(1:3))
    case('blk')
       call find_grid_block(PlotPointXyz_DI(:,iFile), iPE, iBlock)
       if(iPE /= iProc) RETURN

       write(UnitTmp_,'(a)')'TITLE="BATSRUS: BLK Only, '//StringDateTime//'"'
       write(UnitTmp_,'(a)')trim(StringUnitTec)
       write(UnitTmp_,'(a,i8,a,i8,a,i8,a)') &
            'ZONE T="BLK Only '//StringNandT//'", I=',nI+4,&
            ', J=',nJ+4,', K=',nK+4,', F=POINT'
       call write_tecplot_auxdata
       ! DEBUGBLK
       write(UnitTmp_,'(a,i0,a)') 'AUXDATA DEBUGBLK="', iBlock, '"'
       ! DEBUGPROC
       write(UnitTmp_,'(a,i0,a)') 'AUXDATA DEBUGPROC="', iProc, '"'
       ! Write cell values
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          if (IsDimensionalPlot_I(iFile)) then
             write(UnitTmp_,fmt="(50(ES14.6))") &
                  Xyz_DGB(:,i,j,k,iBlock)*No2Io_V(UnitX_), &
                  PlotVarTec_GV(i,j,k,1:nPlotVar)
          else
             write(UnitTmp_,fmt="(50(ES14.6))") &
                  Xyz_DGB(:,i,j,k,iBlock), &
                  PlotVarTec_GV(i,j,k,1:nPlotVar)
          end if
       end do; end do; end do
    case('1d_')
       if(iProc==0)then
          ! Write file header
          write(UnitTmp_,'(a)') &
               'TITLE="BATSRUS: 1D Block Data, '//StringDateTime//'"'
          write(UnitTmp_,'(a)')trim(StringUnitTec)
          write(UnitTmp_,'(a,a,i12,a,a)') &
               'ZONE T="1D   '//StringNandT//'"', &
               ', I=',nNodeUsed,', J=1, K=1,', &
               ', ZONETYPE=ORDERED, DATAPACKING=POINT'
          call write_tecplot_auxdata
       end if
       ! 1d
       do iBlockAll = 1, nNodeUsed
          iNode = iNodeMorton_I(iBlockAll)
          iBlock = iTree_IA(Block_,iNode)
          iPE = iTree_IA(Proc_,iNode)
          if(iProc == iPE)then
             ! Write point values
             call fill_node_xyz
             write(UnitTmp_,fmt="(50(ES14.6))") &
                  (NodeXYZ_DN(1,1,1,1) + NodeXYZ_DN(1,nI+1,1,1))/2, &
                  (NodeXYZ_DN(2,1,1,1) + NodeXYZ_DN(2,1,nJ+1,1))/2, &
                  (NodeXYZ_DN(3,1,1,1) + NodeXYZ_DN(3,1,1,nK+1))/2, &
                  PlotVarNodes_VNB(1:nPlotVar,2,2,2,iBlock)
          end if
       end do
    case('3d_')
       if(iProc==0)then
          ! For DoSaveOneTecFile = T, iUnit is assoicated with the header
          ! file. iUnit = UnitTMp_ for DoSaveOneTecFile = F
          ! Write file header
          if(.not.DoSaveTecBinary) then
             write(iUnit,'(a)')'TITLE="BATSRUS: 3D Data, '//StringDateTime//'"'
             write(iUnit,'(a)')trim(StringUnitTec)
             write(iUnit,'(a,a,i12,a,i12,a)') &
                  'ZONE T="3D   '//StringNandT//'"', &
                  ', N=',nNodeALL, &
                  ', E=',nNodeUsed*nIJK, &
                  ', F=FEPOINT, ET=BRICK'
          else
             write(iUnit)'TITLE="BATSRUS: 3D Data, '//StringDateTime//'"', &
                  CharNewLine
             write(iUnit)trim(StringUnitTec), CharNewLine
             write(iUnit) &
                  'ZONE T="3D   '//StringNandT//'"', &
                  ', N=', int2str(nNodeALL), &
                  ', E=', int2str(nNodeUsed*nIJK), &
                  ', F=FEPOINT, ET=BRICK', CharNewLine
          end if

          call write_tecplot_auxdata(iUnit)
       end if
       ! 3d
       if (DoSaveOneTecFile) then
          ! output format for tecplot data: coordinate info + nPlotVar
          write(StringFormat, '(a,i2.2,a)') "(", nPlotVar+3, "(ES14.6), a)"
          if(DoTest)write(*,*) 'StringFormat =', StringFormat
          do iBlock = 1, nBlock
             if(iTypeAdvance_B(iBlock) == SkippedBlock_) CYCLE
             call fill_node_xyz

             ! Write point values
             do k = 1, nK+1; do j = 1, nJ+1; do i = 1, nI+1
                if(IsNodeUnique_NB(i,j,k,iBlock))then
                   iRec = iNodeGlobal_NB(i,j,k,iBlock)
                   write(UnitTmp_, StringFormat, REC=iRec) &
                        NodeXYZ_DN(1:3,i,j,k),                  &
                        PlotVarNodes_VNB(1:nPlotVar,i,j,k,iBlock),&
                        CharNewLine
                end if
             end do; end do; end do

             ! Write point connectivity.
             ! Initalize record index based on Morton ordering
             iRec = nIJK*(iMortonNode_A(iNode_B(iBlock)) - 1)
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                iRec = iRec + 1
                write(UnitTmp2_,'(8i11,a)', REC=iRec) &
                     iNodeGlobal_NB(i  ,j  ,k  ,iBlock), &
                     iNodeGlobal_NB(i+1,j  ,k  ,iBlock), &
                     iNodeGlobal_NB(i+1,j+1,k  ,iBlock), &
                     iNodeGlobal_NB(i  ,j+1,k  ,iBlock), &
                     iNodeGlobal_NB(i  ,j  ,k+1,iBlock), &
                     iNodeGlobal_NB(i+1,j  ,k+1,iBlock), &
                     iNodeGlobal_NB(i+1,j+1,k+1,iBlock), &
                     iNodeGlobal_NB(i  ,j+1,k+1,iBlock), CharNewLine
             end do; end do; end do
          end do
       else
          if(DoSaveTecBinary) then
             do iBlock = 1, nBlock
                if(iTypeAdvance_B(iBlock) == SkippedBlock_) CYCLE
                ! Write point values
                call fill_node_xyz
                do k = 1, nK + 1; do j = 1, nJ + 1; do i = 1, nI + 1
                   if(IsNodeUnique_NB(i,j,k,iBlock))then
                      write(UnitTmp_) &
                           NodeXYZ_DN(1:3,i,j,k),       &
                           real(PlotVarNodes_VNB(1:nPlotVar,i,j,k,iBlock),&
                           Real4_)
                   end if
                end do; end do; end do
                ! Write point connectivity
                do k = 1, nK; do j = 1, nJ; do i = 1, nI
                   write(UnitTmp2_) &
                        iNodeGlobal_NB(i  ,j  ,k  ,iBlock), &
                        iNodeGlobal_NB(i+1,j  ,k  ,iBlock), &
                        iNodeGlobal_NB(i+1,j+1,k  ,iBlock), &
                        iNodeGlobal_NB(i  ,j+1,k  ,iBlock), &
                        iNodeGlobal_NB(i  ,j  ,k+1,iBlock), &
                        iNodeGlobal_NB(i+1,j  ,k+1,iBlock), &
                        iNodeGlobal_NB(i+1,j+1,k+1,iBlock), &
                        iNodeGlobal_NB(i  ,j+1,k+1,iBlock)
                end do; end do; end do
             end do
          else
             do iBlock = 1,nBlock
                if(iTypeAdvance_B(iBlock) == SkippedBlock_) CYCLE
                ! Write point values
                call fill_node_xyz
                do k = 1, nK+1; do j = 1, nJ+1; do i = 1, nI+1
                   if(IsNodeUnique_NB(i,j,k,iBlock))then
                      write(UnitTmp_,fmt="(50(ES14.6))") &
                           NodeXYZ_DN(1:3,i,j,k),       &
                           PlotVarNodes_VNB(1:nPlotVar,i,j,k,iBlock)
                   end if
                end do; end do; end do
                ! Write point connectivity
                do k = 1, nK; do j = 1, nJ; do i = 1, nI
                   write(UnitTmp2_,'(8i11)') &
                        iNodeGlobal_NB(i  ,j  ,k  ,iBlock), &
                        iNodeGlobal_NB(i+1,j  ,k  ,iBlock), &
                        iNodeGlobal_NB(i+1,j+1,k  ,iBlock), &
                        iNodeGlobal_NB(i  ,j+1,k  ,iBlock), &
                        iNodeGlobal_NB(i  ,j  ,k+1,iBlock), &
                        iNodeGlobal_NB(i+1,j  ,k+1,iBlock), &
                        iNodeGlobal_NB(i+1,j+1,k+1,iBlock), &
                        iNodeGlobal_NB(i  ,j+1,k+1,iBlock)
                end do; end do; end do
             end do
          end if
       end if
    case('cut','x=0','y=0','z=0')
       ! Allocate memory for storing the blocks that are cut
       allocate(iBlockCut_A(nNodeUsed))
       iBlockCut_A = 0
       nBlockCut = 0

       if(  (xMax - xMin) < (yMax - yMin) .and. &
            (xMax - xMin) < (zMax - zMin)) then
          ! X Slice
          CutValue = 0.5*(xMin + xMax)
          if(TypePlot(1:3) == 'x=0') CutValue = 0
          if(IsCartesianGrid)then
             ! First loop to count nodes and cells
             do iBlockAll = 1, nNodeUsed
                iNode = iNodeMorton_I(iBlockAll)
                iBlock = iTree_IA(Block_,iNode)
                iPE = iTree_IA(Proc_,iNode)
                if(iProc == iPE)then
                   if ( CutValue> PlotXYZNodes_DNB(1,1   ,1,1,iBlock) .and. &
                        CutValue<=PlotXYZNodes_DNB(1,1+nI,1,1,iBlock)  )then
                      nBlockCut = nBlockCut + 1
                      iBlockCut_A(iBlockALL) = nBlockCut
                   end if
                end if
                call MPI_Bcast(nBlockCut,1,MPI_Integer,iPE,iComm,iError)
             end do
             if(iProc == 0)then
                ! Write file header
                write(UnitTmp_,'(a)')'TITLE="BATSRUS: Cut X Data, ' &
                     //StringDateTime//'"'
                write(UnitTmp_,'(a)')trim(StringUnitTec)
                write(UnitTmp_,'(a,a,i12,a,i12,a)') &
                     'ZONE T="2D X '//StringNandT//'"', &
                     ', N=',nBlockCut*((nJ+1)*(nK+1)), &
                     ', E=',nBlockCut*((nJ  )*(nK  )), &
                     ', F=FEPOINT, ET=QUADRILATERAL'
                call write_tecplot_auxdata
             end if
             ! Now loop to write values
             do iBlockAll = 1, nNodeUsed
                iNode = iNodeMorton_I(iBlockAll)
                iBlock = iTree_IA(Block_,iNode)
                iPE = iTree_IA(Proc_,iNode)
                if(iProc==iPE)then
                   if ( CutValue> PlotXYZNodes_DNB(1,1   ,1,1,iBlock) .and. &
                        CutValue<=PlotXYZNodes_DNB(1,1+nI,1,1,iBlock) )then
                      ! Find cut interpolation factors
                      do i = 1, nI
                         if ( CutValue > PlotXYZNodes_DNB(1,i  ,1,1,iBlock) &
                              .and. &
                              CutValue <= PlotXYZNodes_DNB(1,i+1,1,1,iBlock)) &
                              then
                            Ijk1=i
                            Ijk2=i+1
                            Factor2 = &
                                 (CutValue - PlotXYZNodes_DNB(1,i,1,1,iBlock))&
                                 /( PlotXYZNodes_DNB(1,i+1,1,1,iBlock) &
                                 -  PlotXYZNodes_DNB(1,i,1,1,iBlock))
                            Factor1 = 1 - Factor2
                            EXIT
                         end if
                      end do
                      ! Write point values
                      call fill_node_xyz
                      do k = 1, 1+nK; do j = 1, 1+nJ
                         write(UnitTmp_,fmt="(50(ES14.6))") &
                              (Factor1*NodeXYZ_DN(1:3,Ijk1,j,k)+ &
                              Factor2*NodeXYZ_DN(1:3,Ijk2,j,k)), &
                              (Factor1 &
                              *PlotVarNodes_VNB(1:nPlotVar,Ijk1,j,k,iBlock) + &
                              Factor2 &
                              *PlotVarNodes_VNB(1:nPlotVar,Ijk2,j,k,iBlock))
                      end do; end do
                      ! Write point connectivity
                      do k = 1, nK; do j = 1, nJ
                         write(UnitTmp2_,'(4(i8,1x))') &
                              ((iBlockCut_A(iBlockALL) - 1)*(nJ+1)*(nK+1)) &
                              + (k-1)*(nJ+1) + j, &
                              ((iBlockCut_A(iBlockALL) - 1)*(nJ+1)*(nK+1)) &
                              + (k-1)*(nJ+1) + j+1, &
                              ((iBlockCut_A(iBlockALL) - 1)*(nJ+1)*(nK+1)) &
                              + k*(nJ+1) + j+1, &
                              ((iBlockCut_A(iBlockALL) - 1)*(nJ+1)*(nK+1)) &
                              + k*(nJ+1) + j
                      end do; end do
                   end if
                end if
             end do
          elseif(IsRLonLat)then
             ! First loop to count nodes and cells
             do iBlockAll = 1, nNodeUsed
                iNode = iNodeMorton_I(iBlockAll)
                iBlock  = iTree_IA(Block_,iNode)
                iPE = iTree_IA(Proc_,iNode)
                if(iProc == iPE)then
                   if((CutValue - PlotXYZNodes_DNB(1,1,1,2,iBlock))* &
                        (CutValue - PlotXYZNodes_DNB(1,1,1+nJ,2,iBlock)) &
                        <= 0)then
                      nBlockCut = nBlockCut + 1
                      iBlockCut_A(iBlockALL) = nBlockCut
                   end if
                end if
                call MPI_bcast(nBlockCut,1,MPI_Integer,iPE,iComm,iError)
             end do
             if(iProc==0)then
                ! Write file header
                write(UnitTmp_,'(a)')'TITLE="BATSRUS: Cut X Data, ' &
                     //StringDateTime//'"'
                write(UnitTmp_,'(a)')trim(StringUnitTec)
                write(UnitTmp_,'(a,a,i12,a,i12,a)')       &
                     'ZONE T="2D X '//StringNandT//'"',   &
                     ', N=', nBlockCut*(nI+1)*(nK+1), &
                     ', E=', nBlockCut*nI*nK, &
                     ', F=FEPOINT, ET=QUADRILATERAL'
                call write_tecplot_auxdata
             end if
             ! Now loop to write values
             do iBlockAll = 1, nNodeUsed
                iNode = iNodeMorton_I(iBlockAll)
                iBlock = iTree_IA(Block_,iNode)
                iPE = iTree_IA(Proc_,iNode)
                if(iProc == iPE)then
                   if(  (CutValue - PlotXYZNodes_DNB(1,1,1,2,iBlock))* &
                        (CutValue - PlotXYZNodes_DNB(1,1,1+nJ,2,iBlock)) &
                        <= 0)then
                      ! Find cut interpolation factors
                      do j = 1, nJ
                         if ((CutValue - PlotXYZNodes_DNB(1,1,j,2,iBlock))* &
                              (CutValue - PlotXYZNodes_DNB(1,1,j+1,2,iBlock)) &
                              <= 0) then
                            Ijk1 = j
                            Ijk2 = j + 1
                            Factor2 = &
                                 (CutValue - PlotXYZNodes_DNB(1,1,j,2,iBlock))&
                                 /(PlotXYZNodes_DNB(1,1,j+1,2,iBlock) &
                                 - PlotXYZNodes_DNB(1,1,j,2,iBlock))
                            Factor1 = 1 - Factor2
                            EXIT
                         end if
                      end do
                      ! Write point values
                      call fill_node_xyz
                      do k = 1, nK+1; do i = 1, nI+1
                         write(UnitTmp_, "(50(ES14.6))") &
                              (Factor1*NodeXYZ_DN(1:3,i,Ijk1,k)+ &
                              Factor2*NodeXYZ_DN(1:3,i,Ijk2,k)), &
                              (Factor1 &
                              *PlotVarNodes_VNB(1:nPlotVar,i,Ijk1,k,iBlock)+ &
                              Factor2 &
                              *PlotVarNodes_VNB(1:nPlotVar,i,Ijk2,k,iBlock))
                      end do; end do
                      ! Write point connectivity
                      do k = 1, nK; do i = 1, nI
                         write(UnitTmp2_, '(4(i8,1x))') &
                              ((iBlockCut_A(iBlockALL)-1)*(nI+1)*(nK+1)) &
                              + (k-1)*(nI+1) + i, &
                              ((iBlockCut_A(iBlockALL)-1)*(nI+1)*(nK+1)) &
                              + (k-1)*(nI+1) + i+1, &
                              ((iBlockCut_A(iBlockALL)-1)*(nI+1)*(nK+1)) &
                              + k*(nI+1) + i+1, &
                              ((iBlockCut_A(iBlockALL)-1)*(nI+1)*(nK+1)) &
                              + k*(nI+1) + i
                      end do; end do
                   end if
                end if
             end do
          end if

       elseif((yMax - yMin) < (zMax - zMin))then
          ! Y Slice
          CutValue = 0.5*(yMin + yMax)
          if(TypePlot(1:3) == 'y=0') CutValue = 0
          if(IsCartesianGrid)then
             ! First loop to count nodes and cells
             do iBlockAll = 1, nNodeUsed
                iNode = iNodeMorton_I(iBlockAll)
                iBlock = iTree_IA(Block_,iNode)
                iPE = iTree_IA(Proc_,iNode)
                if(iProc == iPE)then
                   if ( CutValue >  PlotXYZNodes_DNB(2,1,1,1,iBlock) .and. &
                        CutValue <= PlotXYZNodes_DNB(2,1,1+nJ,1,iBlock))then
                      nBlockCut = nBlockCut + 1
                      iBlockCut_A(iBlockALL) = nBlockCut
                   end if
                end if
                call MPI_Bcast(nBlockCut, 1, MPI_Integer, iPE, iComm, iError)
             end do
             if(iProc==0)then
                ! Write file header
                write(UnitTmp_,'(a)')'TITLE="BATSRUS: Cut Y Data, ' &
                     //StringDateTime//'"'
                write(UnitTmp_,'(a)') trim(StringUnitTec)
                write(UnitTmp_,'(a,a,i12,a,i12,a)') &
                     'ZONE T="2D Y '//StringNandT//'"', &
                     ', N=',nBlockCut*((nI+1)*(nK+1)), &
                     ', E=',nBlockCut*((nI  )*(nK  )), &
                     ', F=FEPOINT, ET=QUADRILATERAL'
                call write_tecplot_auxdata
             end if
             ! Now loop to write values
             do iBlockAll = 1, nNodeUsed
                iNode = iNodeMorton_I(iBlockAll)
                iBlock  = iTree_IA(Block_,iNode)
                iPE = iTree_IA(Proc_,iNode)
                if(iProc == iPE)then
                   if ( CutValue> PlotXYZNodes_DNB(2,1,1,1,iBlock) .and. &
                        CutValue <= PlotXYZNodes_DNB(2,1,1+nJ,1,iBlock))then
                      ! Find cut interpolation factors
                      do j = 1, nJ
                         if(CutValue > PlotXYZNodes_DNB(2,1,j,1,iBlock) .and.&
                              CutValue <= PlotXYZNodes_DNB(2,1,j+1,1,iBlock) &
                              )then
                            Ijk1 = j
                            Ijk2 = j + 1
                            Factor2 = (CutValue &
                                 - PlotXYZNodes_DNB(2,1,j,1,iBlock))/&
                                 ( PlotXYZNodes_DNB(2,1,j+1,1,iBlock) &
                                 - PlotXYZNodes_DNB(2,1,j,1,iBlock))
                            Factor1 = 1 - Factor2
                            EXIT
                         end if
                      end do
                      ! Write point values
                      call fill_node_xyz
                      do k = 1, nK+1; do i = 1, nI+1
                         write(UnitTmp_,fmt="(50(ES14.6))") &
                              (Factor1*NodeXYZ_DN(1:3,i,Ijk1,k) + &
                              Factor2*NodeXYZ_DN(1:3,i,Ijk2,k)), &
                              (Factor1 &
                              *PlotVarNodes_VNB(1:nPlotVar,i,Ijk1,k,iBlock) + &
                              Factor2 &
                              *PlotVarNodes_VNB(1:nPlotVar,i,Ijk2,k,iBlock))
                      end do; end do
                      ! Write point connectivity
                      do k = 1, nK; do i = 1, nI
                         write(UnitTmp2_,'(4(i8,1x))') &
                              ((iBlockCut_A(iBlockALL)-1)*(nI+1)*(nK+1)) &
                              + (k-1)*(nI+1)+i, &
                              ((iBlockCut_A(iBlockALL)-1)*(nI+1)*(nK+1)) &
                              + (k-1)*(nI+1)+i+1, &
                              ((iBlockCut_A(iBlockALL)-1)*(nI+1)*(nK+1)) &
                              + (k  )*(nI+1)+i+1, &
                              ((iBlockCut_A(iBlockALL)-1)*(nI+1)*(nK+1)) &
                              + (k  )*(nI+1)+i
                      end do; end do
                   end if
                end if
             end do
          else if(IsRLonLat) then
             ! First loop to count nodes and cells
             do iBlockAll = 1, nNodeUsed
                iNode = iNodeMorton_I(iBlockAll)
                iBlock = iTree_IA(Block_,iNode)
                iPE = iTree_IA(Proc_,iNode)
                if(iProc == iPE)then
                   if(  (CutValue - PlotXYZNodes_DNB(2,1,1,2,iBlock))* &
                        (CutValue - PlotXYZNodes_DNB(2,1,1+nJ,2,iBlock)) &
                        <= 0)then
                      nBlockCut = nBlockCut + 1
                      iBlockCut_A(iBlockALL) = nBlockCut
                   end if
                end if
                call MPI_Bcast(nBlockCut, 1, MPI_Integer, iPE, iComm, iError)
             end do
             if(iProc == 0)then
                ! Write file header
                write(UnitTmp_,'(a)')'TITLE="BATSRUS: Cut Y Data, ' &
                     //StringDateTime//'"'
                write(UnitTmp_,'(a)') trim(StringUnitTec)
                write(UnitTmp_,'(a,a,i12,a,i12,a)') &
                     'ZONE T="2D Y '//StringNandT//'"', &
                     ', N=',nBlockCut*((nI+1)*(nK+1)), &
                     ', E=',nBlockCut*((nI  )*(nK  )), &
                     ', F=FEPOINT, ET=QUADRILATERAL'
                call write_tecplot_auxdata
             end if
             ! Now loop to write values
             do iBlockAll = 1, nNodeUsed
                iNode = iNodeMorton_I(iBlockAll)
                iBlock = iTree_IA(Block_,iNode)
                iPE = iTree_IA(Proc_,iNode)
                if(iProc == iPE)then
                   if(  (CutValue - PlotXYZNodes_DNB(2,1,1,2,iBlock))* &
                        (CutValue - PlotXYZNodes_DNB(2,1,1+nJ,2,iBlock)) &
                        <= 0)then
                      ! Find cut interpolation factors
                      do j = 1, nJ
                         if(  (CutValue - PlotXYZNodes_DNB(2,1,j,2,iBlock))* &
                              (CutValue - PlotXYZNodes_DNB(2,1,j+1,2,iBlock)) &
                              <= 0)then
                            Ijk1 = j
                            Ijk2 = j + 1
                            Factor2 = (CutValue &
                                 - PlotXYZNodes_DNB(2,1,j,2,iBlock)) &
                                 /( PlotXYZNodes_DNB(2,1,j+1,2,iBlock) &
                                 -  PlotXYZNodes_DNB(2,1,j,2,iBlock))
                            Factor1 = 1 - Factor2
                            EXIT
                         end if
                      end do
                      ! Write point values
                      call fill_node_xyz
                      do k = 1, nK+1; do i = 1, nI+1
                         write(UnitTmp_,fmt="(50(ES14.6))") &
                              (Factor1*NodeXYZ_DN(1:3,i,Ijk1,k)+ &
                              Factor2*NodeXYZ_DN(1:3,i,Ijk2,k)), &
                              (Factor1 &
                              *PlotVarNodes_VNB(1:nPlotVar,i,Ijk1,k,iBlock)+ &
                              Factor2 &
                              *PlotVarNodes_VNB(1:nPlotVar,i,Ijk2,k,iBlock))
                      end do; end do
                      ! Write point connectivity
                      do k = 1, nK; do i = 1, nI
                         write(UnitTmp2_,'(4(i8,1x))') &
                              ((iBlockCut_A(iBlockALL)-1)*(nI+1)*(nK+1)) &
                              + (k-1)*(nI+1)+i, &
                              ((iBlockCut_A(iBlockALL)-1)*(nI+1)*(nK+1)) &
                              + (k-1)*(nI+1)+i+1, &
                              ((iBlockCut_A(iBlockALL)-1)*(nI+1)*(nK+1)) &
                              + (k  )*(nI+1)+i+1, &
                              ((iBlockCut_A(iBlockALL)-1)*(nI+1)*(nK+1)) &
                              + (k  )*(nI+1)+i
                      end do; end do
                   end if
                end if
             end do
          end if

       else
          ! Z Slice
          CutValue = 0.5*(zMin + zMax)
          if(TypePlot(1:3) == 'z=0') CutValue = 0
          ! First loop to count nodes and cells
          do iBlockAll = 1, nNodeUsed
             iNode = iNodeMorton_I(iBlockAll)
             iBlock = iTree_IA(Block_,iNode)
             iPE = iTree_IA(Proc_,iNode)
             if(iProc==iPE)then
                if ( CutValue >  PlotXYZNodes_DNB(3,1,1,1,iBlock) .and. &
                     CutValue <= PlotXYZNodes_DNB(3,1,1,1+nK,iBlock))then
                   nBlockCut = nBlockCut+1
                   iBlockCut_A(iBlockALL) = nBlockCut
                end if
             end if
             call MPI_Bcast(nBlockCut, 1, MPI_Integer, iPE, iComm, iError)
          end do
          if(iProc == 0)then
             ! Write file header
             write(UnitTmp_,'(a)')'TITLE="BATSRUS: Cut Z Data, ' &
                  //StringDateTime//'"'
             write(UnitTmp_,'(a)') trim(StringUnitTec)
             write(UnitTmp_,'(a,a,i12,a,i12,a)') &
                  'ZONE T="2D Z '//StringNandT//'"', &
                  ', N=', nBlockCut*(nI+1)*(nJ+1), &
                  ', E=', nBlockCut*nI*nJ, &
                  ', F=FEPOINT, ET=QUADRILATERAL'
             call write_tecplot_auxdata
          end if
          ! Now loop to write values
          do iBlockAll = 1, nNodeUsed
             iNode = iNodeMorton_I(iBlockAll)
             iBlock  = iTree_IA(Block_,iNode)
             iPE   = iTree_IA(Proc_,iNode)
             if(iProc==iPE)then
                if ( CutValue> PlotXYZNodes_DNB(3,1,1,1   ,iBlock) .and. &
                     CutValue<=PlotXYZNodes_DNB(3,1,1,1+nK,iBlock)  )then
                   ! Find cut interpolation factors
                   do k = 1, nK
                      if ( CutValue > PlotXYZNodes_DNB(3,1,1,k,iBlock) .and. &
                           CutValue <= PlotXYZNodes_DNB(3,1,1,k+1,iBlock))then
                         Ijk1=k
                         Ijk2=k+1
                         Factor2=(CutValue-PlotXYZNodes_DNB(3,1,1,k,iBlock))/ &
                              ( PlotXYZNodes_DNB(3,1,1,k+1,iBlock) &
                              - PlotXYZNodes_DNB(3,1,1,k,iBlock))
                         Factor1 = 1 - Factor2
                         EXIT
                      end if
                   end do
                   ! Write point values
                   call fill_node_xyz
                   do j = 1, nJ+1; do i = 1, nI+1
                      write(UnitTmp_,fmt="(50(ES14.6))") &
                           (Factor1*NodeXYZ_DN(1:3,i,j,Ijk1) + &
                           Factor2*NodeXYZ_DN(1:3,i,j,Ijk2)), &
                           (Factor1 &
                           *PlotVarNodes_VNB(1:nPlotVar,i,j,Ijk1,iBlock) + &
                           Factor2 &
                           *PlotVarNodes_VNB(1:nPlotVar,i,j,Ijk2,iBlock))
                   end do; end do
                   ! Write point connectivity
                   do j = 1, nJ; do i = 1, nI
                      write(UnitTmp2_,'(4(i8,1x))') &
                           ((iBlockCut_A(iBlockALL)-1)*(nI+1)*(nJ+1)) &
                           + (j-1)*(nI+1)+i, &
                           ((iBlockCut_A(iBlockALL)-1)*(nI+1)*(nJ+1)) &
                           + (j-1)*(nI+1)+i+1, &
                           ((iBlockCut_A(iBlockALL)-1)*(nI+1)*(nJ+1)) &
                           + (j  )*(nI+1)+i+1, &
                           ((iBlockCut_A(iBlockALL)-1)*(nI+1)*(nJ+1)) &
                           + (j  )*(nI+1)+i
                   end do; end do
                end if
             end if
          end do
       end if
       deallocate(iBlockCut_A)
    case('slc','dpl')
       ! arbitrary slices
       DoDebug = .false.

       ! XarbP,YarbP,ZarbP                    point on plane
       ! XarbNormal,YarbNormal,ZarbNormal     normal for cut
       ! i1,j1,k1,i2,j2,k2              two opposite corner indices

       if (TypePlot(1:3)=='slc')then
          ! Point-Normal cut plot
          XarbP=PlotPointXyz_DI(1,iFile); XarbNormal=PlotNormal_DI(1,iFile)
          YarbP=PlotPointXyz_DI(2,iFile); YarbNormal=PlotNormal_DI(2,iFile)
          ZarbP=PlotPointXyz_DI(3,iFile); ZarbNormal=PlotNormal_DI(3,iFile)
       else
          ! Dipole cut plot
          XarbP=0.; XarbNormal=-sin(ThetaTilt)
          YarbP=0.; YarbNormal=0.
          ZarbP=0.; ZarbNormal= cos(ThetaTilt)
       end if

       ! First loop to count cuts
       nBlockCut=0
       do iBlock = 1, nBlock
          if(iTypeAdvance_B(iBlock) == SkippedBlock_) CYCLE
          i1 = 1; i2 = 1+nI
          j1 = 1; j2 = 1+nJ
          k1 = 1; k2 = 1+nK
          call find_cuts(-1)
          if (nCuts > 0)then
             ! count up number of cuts
             do i = 1, nI; do j = 1, nJ; do k = 1, nK
                i1 = i; i2 = i + 1
                j1 = j; j2 = j + 1
                k1 = k; k2 = k + 1
                call find_cuts(0)
                nBlockCut=nBlockCut+nCuts
             end do; end do; end do
          end if
       end do
       call MPI_reduce(nBlockCut, nCutsTotal, 1, MPI_INTEGER, MPI_SUM, 0, &
            iComm, iError)

       ! Write file header
       if(iProc==0)then
          if (TypePlot(1:3)=='slc')then
             write(UnitTmp_,'(a)')'TITLE="BATSRUS: Slice, ' &
                  //StringDateTime//'"'
             write(UnitTmp_,'(a)')trim(StringUnitTec)
             write(UnitTmp_,'(a,i8,a)') &
                  'ZONE T="Slice '//StringNandT//'", I=', nCutsTotal,&
                  ', J=1, K=1, F=POINT'
          else
             write(UnitTmp_,'(a)')'TITLE="BATSRUS: Dipole Cut, '// &
                  StringDateTime//'"'
             write(UnitTmp_,'(a)')trim(StringUnitTec)
             write(UnitTmp_,'(a,i8,a)') &
                  'ZONE T="Dipole Cut '//StringNandT//'", I=', nCutsTotal,&
                  ', J=1, K=1, F=POINT'
          end if
          call write_tecplot_auxdata
       end if

       ! Now loop to write values
       do iBlock = 1, nBlock
          if(iTypeAdvance_B(iBlock) == SkippedBlock_) CYCLE
          i1 = 1; i2 = 1 + nI
          j1 = 1; j2 = 1 + nJ
          k1 = 1; k2 = 1 + nK
          call find_cuts(-1)
          if (nCuts > 0)then
             ! write the cuts
             call fill_node_xyz
             do i = 1, nI; do j = 1, nJ; do k = 1, nK
                i1 = i; i2 = i + 1
                j1 = j; j2 = j + 1
                k1 = k; k2 = k + 1
                call find_cuts(1)
             end do; end do; end do
          end if
       end do
    case default
       write(*,*) NameSub,': ERROR: Unknown TypePlot_I='//TypePlot
    end select

    call test_stop(NameSub, DoTest)

  contains
    !==========================================================================
    subroutine fill_node_xyz

      ! Fill array with position (optionally dimensioned)
      ! Assumes iBlock value is correct
      !------------------------------------------------------------------------
      if (IsDimensionalPlot_I(iFile)) then
         NodeXYZ_DN(1:3,:,:,:)=PlotXYZNodes_DNB(1:3,:,:,:,iBlock) &
              *No2Io_V(UnitX_)
      else
         NodeXYZ_DN(1:3,:,:,:)=PlotXYZNodes_DNB(1:3,:,:,:,iBlock)
      end if

    end subroutine fill_node_xyz
    !==========================================================================
    subroutine find_cuts(iOption)

      integer, intent(in):: iOption
      integer:: i, j, k

      ! iOption =-1 check all edges to see if cut
      !      = 0 count cuts only
      !      = 1 find cuts and write to disk
      !------------------------------------------------------------------------
      nCuts=0

      ! Check edges.
      ! X edges
      if (XarbNormal > 0.01) then
         i = i1; j = j1; k = k1
         do j = j1, j2, j2-j1; do k = k1, k2, k2-k1
            if(iOption>-1 .and. (j==j1 .or. k==k1) .and. (j/=0 .and. k/=0))&
                 CYCLE
            Yp = PlotXYZNodes_DNB(2,i,j,k,iBlock)
            Zp = PlotXYZNodes_DNB(3,i,j,k,iBlock)
            Xp = XarbP &
                 - (YarbNormal*(Yp-YarbP) + ZarbNormal*(Zp-ZarbP))/XarbNormal
            if ( Xp> PlotXYZNodes_DNB(1,i1,j,k,iBlock) .and. &
                 Xp<=PlotXYZNodes_DNB(1,i2,j,k,iBlock) )then
               if(DoDebug)write(*,*)'x-cut:',iOption,Xp,Yp,Zp
               if(iOption == -1)then
                  nCuts = 1; RETURN
               end if
               ! Cycle if outside of clipping box
               if ( Xp<xMin .or. Yp<yMin .or. Zp<zMin .or. &
                    Xp>xMax .or. Yp>yMax .or. Zp>zMax) CYCLE
               nCuts = nCuts + 1
               if (iOption>0) then
                  ! Write point values
                  Factor2 = (Xp-PlotXYZNodes_DNB(1,i1,j,k,iBlock))/ &
                       ( PlotXYZNodes_DNB(1,i2,j,k,iBlock) &
                       - PlotXYZNodes_DNB(1,i1,j,k,iBlock))
                  Factor1 = 1 - Factor2
                  write(UnitTmp_,fmt="(50(ES14.6))") &
                       (Factor1*NodeXYZ_DN(:, i1,j,k)+ &
                       Factor2*NodeXYZ_DN(:, i2,j,k)), &
                       (Factor1*PlotVarNodes_VNB(1:nPlotVar,i1,j,k,iBlock)+ &
                       Factor2*PlotVarNodes_VNB(1:nPlotVar,i2,j,k,iBlock))
                  if(DoDebug)write(*,*)'  i=',i1,'-',i2,' j=',j,' k=',k
               end if
            end if
         end do; end do
      end if
      ! Y edges
      if (YarbNormal > 0.01) then
         i = i1; j = j1; k = k1
         do i = i1, i2, i2-i1; do k = k1, k2, k2-k1
            if(iOption>-1 .and. (i==i1 .or. k==k1) .and. (i/=0 .and. k/=0))&
                 CYCLE
            Xp = PlotXYZNodes_DNB(1,i,j,k,iBlock)
            Zp = PlotXYZNodes_DNB(3,i,j,k,iBlock)
            Yp = YarbP &
                 - (XarbNormal*(Xp-XarbP) + ZarbNormal*(Zp-ZarbP))/YarbNormal
            if ( Yp> PlotXYZNodes_DNB(2,i,j1,k,iBlock) .and. &
                 Yp<=PlotXYZNodes_DNB(2,i,j2,k,iBlock) )then
               if(DoDebug)write(*,*)'y-cut:',iOption,Xp,Yp,Zp
               if(iOption==-1)then
                  nCuts = 1; RETURN
               end if
               ! Cycle if outside of clipping box
               if ( Xp < xMin .or. Yp < yMin .or. Zp < zMin .or. &
                    Xp > xMax .or. Yp > yMax .or. Zp > zMax) CYCLE
               nCuts = nCuts + 1
               if (iOption > 0) then
                  ! Write point values
                  Factor2=(Yp-PlotXYZNodes_DNB(2,i,j1,k,iBlock))/ &
                       ( PlotXYZNodes_DNB(2,i,j2,k,iBlock) &
                       - PlotXYZNodes_DNB(2,i,j1,k,iBlock))
                  Factor1 = 1 - Factor2
                  write(UnitTmp_,fmt="(50(ES14.6))") &
                       (Factor1*NodeXYZ_DN(:, i,j1,k)+ &
                       Factor2*NodeXYZ_DN(:, i,j2,k)), &
                       (Factor1*PlotVarNodes_VNB(1:nPlotVar,i,j1,k,iBlock)+ &
                       Factor2*PlotVarNodes_VNB(1:nPlotVar,i,j2,k,iBlock))
                  if(DoDebug)write(*,*)'  i=',i,' j=',j1,'-',j2,' k=',k
               end if
            end if
         end do; end do
      end if
      ! Z edges
      if (ZarbNormal > 0.01) then
         i = i1; j = j1; k = k1
         do i = i1, i2, i2-i1; do j = j1, j2, j2-j1
            if(iOption>-1 .and. (i==i1 .or. j==j1) .and. (i/=0 .and. j/=0))&
                 CYCLE
            Xp = PlotXYZNodes_DNB(1,i,j,k,iBlock)
            Yp = PlotXYZNodes_DNB(2,i,j,k,iBlock)
            Zp = ZarbP &
                 - (XarbNormal*(Xp-XarbP) + YarbNormal*(Yp-YarbP))/ZarbNormal
            if ( Zp> PlotXYZNodes_DNB(3,i,j,k1,iBlock) .and. &
                 Zp<=PlotXYZNodes_DNB(3,i,j,k2,iBlock) )then
               if(DoDebug)write(*,*)'z-cut:',iOption,Xp,Yp,Zp
               if(iOption == -1)then
                  nCuts = 1; RETURN
               end if
               ! Cycle if outside of clipping box
               if ( Xp<xMin .or. Yp<yMin .or. Zp<zMin .or. &
                    Xp>xMax .or. Yp>yMax .or. Zp>zMax) CYCLE
               nCuts=nCuts+1
               if (iOption>0) then
                  ! Write point values
                  Factor2 = (Zp - PlotXYZNodes_DNB(3,i,j,k1,iBlock))/ &
                       ( PlotXYZNodes_DNB(3,i,j,k2,iBlock) &
                       - PlotXYZNodes_DNB(3,i,j,k1,iBlock))
                  Factor1 = 1 - Factor2
                  write(UnitTmp_,fmt="(50(ES14.6))") &
                       (Factor1*NodeXYZ_DN(:, i,j,k1)+ &
                       Factor2*NodeXYZ_DN(:, i,j,k2)), &
                       (Factor1*PlotVarNodes_VNB(1:nPlotVar,i,j,k1,iBlock)+ &
                       Factor2*PlotVarNodes_VNB(1:nPlotVar,i,j,k2,iBlock))
                  if(DoDebug)write(*,*)'  i=',i,' j=',j,' k=',k1,'-',k2
               end if
            end if
         end do; end do
      end if

    end subroutine find_cuts
    !==========================================================================
  end subroutine write_tecplot_node_data
  !============================================================================
  subroutine assign_node_numbers

    use ModIO, ONLY: write_prefix, iUnitOut
    use ModAdvance, ONLY: iTypeAdvance_B, iTypeAdvance_BP, SkippedBlock_
    use ModNodes
    use ModMain, ONLY: nBlockMax
    use BATL_lib, ONLY: nBlock, nNodeUsed, message_pass_node

    integer, parameter:: nNodeBlock=(nI+1)*(nJ+1)*(nK+1)
    integer:: iBlockStart
    integer:: i, j, k, iNode, iBlock, iError, iPE, iTag
    integer:: nOffset, nOffsetPrevious
    integer, allocatable:: nNodeOffset_P(:), MaxNodeOffset_P(:), nOffset_P(:)
    real,    allocatable:: IndexNode_VNB(:,:,:,:,:)
    logical:: DoAllReduce=.true.
    integer:: iStatus_I(MPI_STATUS_SIZE)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'assign_node_numbers'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Write information to the screen
    if(iProc==0.and.lVerbose>0)then
       call write_prefix; write(iUnitOut,*)'Starting assign_node_numbers ...'
    end if

    ! Initialize all node numbers to zero
    iNodeLocal_NB=0

    ! Number of nodes on each block (maximum)
    nNodeALL=nNodeUsed*nNodeBlock

    ! Count number of used blocks on all processors with rank below this one
    iBlockStart = 0
    if(iProc > 0) iBlockStart = &
         count(iTypeAdvance_BP(1:nBlockMax,0:iProc-1) /= SkippedBlock_)

    iNode = iBlockStart*nNodeBlock

    ! Loop to assign local and global node numbers
    TREE1: do iBlock  = 1, nBlock
       if(iTypeAdvance_B(iBlock) == SkippedBlock_) CYCLE
       do k = 1, nK+1; do j = 1, nJ+1; do i = 1, nI+1
          iNode = iNode + 1
          iNodeLocal_NB(i,j,k,iBlock) = iNode
       end do; end do; end do
    end do TREE1
    iNodeGlobal_NB = iNodeLocal_NB

    ! Set logical array
    IsNodeUnique_NB = iNodeGlobal_NB>0

    ! Assign value to internal passing variable and do message pass
    !  NOTE: convert integer to real for message pass first

    ! Done a evel one, with allocate and dealocate. NEED to be fixed
    allocate(IndexNode_VNB(1,nI+1,nJ+1,nK+1,nBlock))
    IndexNode_VNB(1,:,:,:,:) = real(iNodeGlobal_NB(:,:,:,1:nBlock))
    call message_pass_node(1,IndexNode_VNB, &
         NameOperatorIn='Min', UsePeriodicCoordIn = .true.)
    iNodeGlobal_NB(:,:,:,1:nBlock) = nint(IndexNode_VNB(1,:,:,:,:))
    deallocate(IndexNode_VNB)

    ! Allocate memory for storing the node offsets
    allocate( nNodeOffset_P   (nNodeUsed*nNodeBlock))
    allocate( MaxNodeOffset_P(nNodeUsed*nNodeBlock))
    nNodeOffset_P=0

    ! Loop to compute node offsets
    nOffset=0
    TREE2: do iBlock = 1, nBlock
       if(iTypeAdvance_B(iBlock) == SkippedBlock_) CYCLE
       do k = 1, nK+1; do j = 1, nJ+1; do i = 1, nI+1
          if(  iNodeLocal_NB(i,j,k,iBlock) > &
               iNodeGlobal_NB(i,j,k,iBlock))then
             nOffset = nOffset + 1
             IsNodeUnique_NB(i,j,k,iBlock) = .false.
          end if
          nNodeOffset_P(iNodeLocal_NB(i,j,k,iBlock)) = nOffset
       end do; end do; end do
    end do TREE2

    ! Collect offsets from all the PEs
    allocate(nOffset_P(0:nProc-1))
    call MPI_allgather(nOffset, 1, MPI_INTEGER, nOffset_P, 1, MPI_INTEGER, &
         iComm, iError)

    ! Add up the offsets on processors with lower rank
    nOffsetPrevious = 0
    if(iProc > 0) nOffsetPrevious = sum(nOffset_P(0:iProc-1))

    ! Increase the offset on this processor by nOffsetPrevious
    do iBlock  = 1, nBlock
       if(iTypeAdvance_B(iBlock) == SkippedBlock_) CYCLE
       do k = 1, nK+1; do j = 1, nJ+1; do i = 1, nI+1
          iNode = iNodeLocal_NB(i,j,k,iBlock)
          nNodeOffset_P(iNode) = nNodeOffset_P(iNode) + nOffsetPrevious
       end do; end do; end do
    end do

    ! Gather offsets from all PE-s.
    ! nNodeOffset_P was initialized to 0 so MPI_MAX works.
    if(DoAllReduce)then
       call MPI_allreduce(nNodeOffset_P,MaxNodeOffset_P,nNodeUsed*nNodeBlock, &
            MPI_INTEGER,MPI_MAX,iComm,iError)
       nNodeOffset_P = MaxNodeOffset_P
       nNodeALL   = nNodeALL - sum(nOffset_P)
    else
       if(iProc == 0) then
          do iPE = 1, nProc-1
             iTag = iPE
             call MPI_recv(MaxNodeOffset_P, nNodeUsed*nNodeBlock, &
                  MPI_INTEGER, iPE, iTag, iComm, iStatus_I, iError)
             nNodeOffset_P = max(nNodeOffset_P,MaxNodeOffset_P)
          end do
       else
          iTag = iProc
          call MPI_send(nNodeOffset_P, nNodeUsed*nNodeBlock, &
               MPI_INTEGER, 0, iTag, iComm, iError)
       end if
       call MPI_Bcast(nNodeOffset_P, nNodeUsed*nNodeBlock, MPI_INTEGER, 0, &
            iComm, iError)
    end if

    ! Loop to fix iNodeGlobal_NB for offset
    TREE3: do iBlock  = 1, nBlock
       if(iTypeAdvance_B(iBlock) == SkippedBlock_) CYCLE
       do k = 1, nK+1; do j = 1, nJ+1; do i = 1, nI+1
          iNodeGlobal_NB(i,j,k,iBlock) = iNodeGlobal_NB(i,j,k,iBlock) &
               - nNodeOffset_P(iNodeGlobal_NB(i,j,k,iBlock))
          if(iNodeGlobal_NB(i,j,k,iBlock)>nNodeALL &
               .or. iNodeGlobal_NB(i,j,k,iBlock)<1)then
             ! Error in numbering, report values and stop.
             write(*,*)'ERROR: Global node numbering problem.', &
                  ' PE=',iProc,' BLK=',iBlock,' ijk=',i,j,k
             write(*,*)'  iNodeGlobal_NB=',&
                  iNodeGlobal_NB(i,j,k,iBlock)
             write(*,*)'  nNodeOffset_P           =',&
                  nNodeOffset_P(iNodeGlobal_NB(i,j,k,iBlock))
             write(*,*)'  nNodeUsed=',nNodeUsed,&
                  ' nNodeBlock=',nNodeBlock,&
                  ' unreduced total=',nNodeUsed*nNodeBlock,&
                  ' nNodeALL=',nNodeALL
             call stop_mpi('message_pass_nodes: error in numbering')
          end if
       end do; end do; end do
    end do TREE3

    ! Deallocate memory when done with it
    deallocate(nNodeOffset_P, MaxNodeOffset_P, nOffset_P)

    ! Write information to the screen
    if(iProc==0)then
       call write_prefix; write(iUnitOUt,*) &
            ' nNodeUsed=',nNodeUsed,' nNodeBlock=',nNodeBlock, &
            ' unreduced total=',nNodeUsed*nNodeBlock,' nNodeALL=',nNodeALL
    end if

    call test_stop(NameSub, DoTest)
  end subroutine assign_node_numbers
  !============================================================================
  subroutine set_tecplot_var_string( &
       iFile, nPlotVar, NamePlotVar_V, StringVarTec)

    ! Set StringVarTec with variable names and units
    ! based on NamePlotVar_V array

    use ModPhysics
    use ModUtilities, ONLY: lower_case
    use ModIO, ONLY: IsDimensionalPlot_I, TypePlot
    use ModVarIndexes, ONLY: IsMhd
    use ModIO, ONLY: NameVarUserTec_I, NameUnitUserTec_I
    use ModMultiFluid, ONLY: extract_fluid_name, NameFluid
    use BATL_lib, ONLY: nDim

    ! Arguments

    integer, intent(in):: nPlotVar, iFile
    character(len=*), intent(in):: NamePlotVar_V(nPlotVar)
    character(len=*), intent(out):: StringVarTec

    character(len=20):: NameTecFluid
    character(len=20):: String, NamePlotVar, NameTecVar, NameUnit
    integer:: iPlotVar, iVar, iFluid

    ! Coordinate names and units
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_tecplot_var_string'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(TypePlot(1:3) == 'box')then
       if (IsDimensionalPlot_I(iFile)) then
          StringVarTec = 'VARIABLES ="x '// trim(NameTecUnit_V(UnitX_)) // &
               '", " y '                // trim(NameTecUnit_V(UnitX_)) // &
               '", " z '               // NameTecUnit_V(UnitX_)
       else
          StringVarTec = 'VARIABLES ="x", "y", "z'
       end if

    elseif(TypePlot(1:3) == 'shl')then
       if (IsDimensionalPlot_I(iFile)) then
          StringVarTec = 'VARIABLES ="R '// trim(NameTecUnit_V(UnitX_)) &
               // '", "Lon [deg]", "Lat [deg]'
       else
          StringVarTec = 'VARIABLES ="R", "Lon", "Lat'
       end if

    elseif(TypePlot(1:3) == 'shk')then
       if (IsDimensionalPlot_I(iFile)) then
          StringVarTec = 'VARIABLES =", "Lon [deg]", "Lat [deg]", "R ' &
               // NameTecUnit_V(UnitX_)
       else
          StringVarTec = 'VARIABLES ="Lon", "Lat", "R'
       end if

    else
       if (IsDimensionalPlot_I(iFile)) then
          StringVarTec = 'VARIABLES ="X ' // trim(NameTecUnit_V(UnitX_)) &
               // '", "Y ' // NameTecUnit_V(UnitX_)
          if(nDim==3) StringVarTec = trim(StringVarTec) &
               // '", "Z ' // NameTecUnit_V(UnitX_)
       else
          if(nDim==2) StringVarTec = 'VARIABLES = "X", "Y'
          if(nDim==3) StringVarTec = 'VARIABLES = "X", "Y", "Z'
       end if

    end if

    do iPlotVar = 1, nPlotVar

       NamePlotVar = NamePlotVar_V(iPlotVar)
       call lower_case(NamePlotVar)
       String = NamePlotVar
       call extract_fluid_name(String,iFluid)
       NameTecFluid = ''
       if(iFluid > 1 .or. .not. IsMhd) NameTecFluid = '^'//NameFluid

       ! Default value for NameUnit is empty string
       NameUnit = ''
       ! Default value for the variable name is the plot variable name.
       NameTecVar = NamePlotVar

       select case(String)
       case('rho')
          NameTecVar = 'Rho'//NameTecFluid
          NameUnit   = NameTecUnit_V(UnitRho_)
       case('rhoux','mx')
          NameTecVar = 'Rho U_x'//NameTecFluid
          NameUnit   = NameTecUnit_V(UnitRhoU_)
       case('rhouy','my')
          NameTecVar = 'Rho U_y'//NameTecFluid
          NameUnit   = NameTecUnit_V(UnitRhoU_)
       case('rhouz','mz')
          NameTecVar = 'Rho U_z'//NameTecFluid
          NameUnit   = NameTecUnit_V(UnitRhoU_)
       case('bx')
          NameTecVar = 'B_x'
          NameUnit   = NameTecUnit_V(UnitB_)
       case('by')
          NameTecVar = 'B_y'
          NameUnit   = NameTecUnit_V(UnitB_)
       case('bz')
          NameTecVar = 'B_z'
          NameUnit   = NameTecUnit_V(UnitB_)
          ! face centered magnetic field
       case('bxl') ! east
          NameTecVar = 'B_e'
          NameUnit   = NameTecUnit_V(UnitB_)
       case('bxr') ! west
          NameTecVar = 'B_w'
          NameUnit   = NameTecUnit_V(UnitB_)
       case('byl') ! south
          NameTecVar = 'B_s'
          NameUnit   = NameTecUnit_V(UnitB_)
       case('byr') ! north
          NameTecVar = 'B_n'
          NameUnit   = NameTecUnit_V(UnitB_)
       case('bzl') ! bottom
          NameTecVar = 'B_b'
          NameUnit   = NameTecUnit_V(UnitB_)
       case('bzr') ! top
          NameTecVar = 'B_t'
          NameUnit   = NameTecUnit_V(UnitB_)
          !
       case('hyp')
          NameTecVar = 'Hyp'
          NameUnit   = NameTecUnit_V(UnitB_)
       case('e')
          NameTecVar = 'E'//NameTecFluid
          NameUnit   = NameTecUnit_V(UnitEnergydens_)
       case('p','pth')
          NameTecVar = 'P'//NameTecFluid
          NameUnit   = NameTecUnit_V(UnitP_)
       case('ppar')
          NameTecVar = 'P_par'//NameTecFluid
          NameUnit   = NameTecUnit_V(UnitP_)
       case('pperp')
          NameTecVar = 'P_perp'//NameTecFluid
          NameUnit   = NameTecUnit_V(UnitP_)
       case('n')
          NameTecVar = 'n'//NameTecFluid
          NameUnit   = NameTecUnit_V(UnitN_)
       case('t','temp')
          NameTecVar = 'T'//NameTecFluid
          NameUnit   = NameTecUnit_V(UnitTemperature_)
       case('ux')
          NameTecVar = 'U_x'//NameTecFluid
          NameUnit   = NameTecUnit_V(UnitU_)
       case('uy')
          NameTecVar = 'U_y'//NameTecFluid
          NameUnit   = NameTecUnit_V(UnitU_)
       case('uz', 'uzrot')
          NameTecVar = 'U_z'//NameTecFluid
          NameUnit   = NameTecUnit_V(UnitU_)
       case('uxrot')
          NameTecVar = 'U_x_rot'//NameTecFluid
          NameUnit   = NameTecUnit_V(UnitU_)
       case('uyrot')
          NameTecVar = 'U_y_rot'//NameTecFluid
          NameUnit   = NameTecUnit_V(UnitU_)
       case('ur')
          NameTecVar = 'U_r'//NameTecFluid
          NameUnit   = NameTecUnit_V(UnitU_)
       case('rhour','mr')
          NameTecVar = 'Rho U_r'//NameTecFluid
          NameUnit   = NameTecUnit_V(UnitRhoU_)
       case('rhounup')
          NameTecVar = 'Rho U_n_up'//NameTecFluid
          NameUnit   = NameTecUnit_V(UnitRhoU_)
       case('rhoundn')
          NameTecVar = 'Rho U_n_dn'//NameTecFluid
          NameUnit   = NameTecUnit_V(UnitRhoU_)
       case('ushk')
          NameTecVar = 'U_shk'//NameTecFluid
          NameUnit   = NameTecUnit_V(UnitU_)
       case('br')
          NameTecVar = 'B_r'
          NameUnit   = NameTecUnit_V(UnitB_)
       case('b1x')
          NameTecVar = 'B1_x'
          NameUnit   = NameTecUnit_V(UnitB_)
       case('b1y')
          NameTecVar = 'B1_y'
          NameUnit   = NameTecUnit_V(UnitB_)
       case('b1z')
          NameTecVar = 'B1_z'
          NameUnit   = NameTecUnit_V(UnitB_)
       case('b1r')
          NameTecVar = 'B1_r'
          NameUnit   = NameTecUnit_V(UnitB_)
       case('jx')
          NameTecVar = 'J_x'
          NameUnit   = NameTecUnit_V(UnitJ_)
       case('jy')
          NameTecVar = 'J_y'
          NameUnit   = NameTecUnit_V(UnitJ_)
       case('jz')
          NameTecVar = 'J_z'
          NameUnit   = NameTecUnit_V(UnitJ_)
       case('jr')
          NameTecVar = 'J_r'
          NameUnit   = NameTecUnit_V(UnitJ_)
       case('ex')
          NameTecVar = 'E_x'
          NameUnit   = NameTecUnit_V(UnitElectric_)
       case('ey')
          NameTecVar = 'E_y'
          NameUnit   = NameTecUnit_V(UnitElectric_)
       case('ez')
          NameTecVar = 'E_z'
          NameUnit   = NameTecUnit_V(UnitElectric_)
       case('expot')
          NameTecVar = 'Epot_x'
          NameUnit   = NameTecUnit_V(UnitElectric_)
       case('eypot')
          NameTecVar = 'Epot_y'
          NameUnit   = NameTecUnit_V(UnitElectric_)
       case('ezpot')
          NameTecVar = 'Epot_z'
          NameUnit   = NameTecUnit_V(UnitElectric_)
       case('exind')
          NameTecVar = 'Eind_x'
          NameUnit   = NameTecUnit_V(UnitElectric_)
       case('eyind')
          NameTecVar = 'Eind_y'
          NameUnit   = NameTecUnit_V(UnitElectric_)
       case('ezind')
          NameTecVar = 'Eind_z'
          NameUnit   = NameTecUnit_V(UnitElectric_)
       case('hype')
          NameTecVar = 'HypE'
          NameUnit   = NameTecUnit_V(UnitElectric_)
       case('gradpex')
          NameTecVar = 'GradPe_x'
          NameUnit   = 'nPa/m'
       case('gradpey')
          NameTecVar = 'GradPe_y'
          NameUnit   = 'nPa/m'
       case('gradpez')
          NameTecVar = 'GradPe_z'
          NameUnit   = 'nPa/m'
       case('gradper')
          NameTecVar = 'GradPe_r'
          NameUnit   = 'nPa/m'
       case('pote')
          NameTecVar = 'PotE'
          NameUnit   = 'V'
       case('dive')
          NameTecVar = 'div(E)'
          NameUnit   = 'V/m^2'
       case('er')
          NameTecVar = 'E_r'
          NameUnit   = NameTecUnit_V(UnitElectric_)
       case('pvecx')
          NameTecVar = 'S_x'
          NameUnit   = NameTecUnit_V(UnitPoynting_)
       case('pvecy')
          NameTecVar = 'S_y'
          NameUnit   = NameTecUnit_V(UnitPoynting_)
       case('pvecz')
          NameTecVar = 'S_z'
          NameUnit   = NameTecUnit_V(UnitPoynting_)
       case('pvecr')
          NameTecVar = 'S_r'
          NameUnit   = NameTecUnit_V(UnitPoynting_)
       case('b2ur')
          NameTecVar = 'B^2/mu_0 U_r'
          NameUnit   = NameTecUnit_V(UnitPoynting_)
       case('divb', 'divb_cd', 'divb_ct', 'absdivb')
          NameTecVar = 'div B'
          NameUnit   = NameTecUnit_V(UnitDivB_)
       case('theta1')
          NameTecVar = 'theta_1'
          NameUnit   = NameTecUnit_V(UnitAngle_)
       case('phi1')
          NameTecVar = 'phi_1'
          NameUnit   = NameTecUnit_V(UnitAngle_)
       case('theta2')
          NameTecVar = 'theta_2'
          NameUnit   = NameTecUnit_V(UnitAngle_)
       case('phi2')
          NameTecVar = 'phi_2'
          NameUnit   = NameTecUnit_V(UnitAngle_)
       case('status')
          NameTecVar = 'Status'
       case('f1x','f1y','f1z','f2x','f2y','f2z')
          NameTecVar = NamePlotVar
       case('x','y','z','r','dx','dy','dz','req1','req2')
          NameTecVar = String
          NameUnit   = NameTecUnit_V(UnitX_)
       case('dvol')
          NameTecVar = 'dvol'
          NameUnit   = trim(NameTecUnit_V(UnitX_))//'^3'
       case('dt')
          NameTecVar = 'dt'
          NameUnit   = NameTecUnit_V(UnitT_)
       case('dtblk')
          NameTecVar = 'dtblk'
          NameUnit   = NameTecUnit_V(UnitT_)
       case('impl')
          NameTecVar = 'impl'
       case('proc')
          NameTecVar = 'PE #'
       case('blk')
          NameTecVar = 'Block #'
       case('node')
          NameTecVar = 'Node #'
       case('eta')
          NameTecVar = 'eta'
       case('cons')
          NameTecVar = 'cons'
       case('hall')
          NameTecVar = 'hall'
       case('pe')
          NameTecVar = 'pe'
          NameUnit   = NameTecUnit_V(UnitP_)
       case('pic')
          NameTecVar = 'pic'
       case('ew','erad')
          NameTecVar = String
          NameUnit   = NameTecUnit_V(UnitEnergydens_)
       case('emiss')
          NameTecVar = 'emiss'
          NameUnit = '[erg s^-1 cm^-3]'
       case('intensity')
          NameTecVar = 'intensity'
          NameUnit = '[DN s^-1 cm^-3]'
       case default
          ! Set the default or user defined values
          NameTecVar = NameVarUserTec_I(iPlotVar)
          NameUnit   = NameUnitUserTec_I(iPlotVar)

          ! Try to find the plot variable among the basic variables
          do iVar = 1, nVar
             if(NamePlotVar /= NameVarLower_V(iVar)) CYCLE
             NameUnit = NameUnitUserTec_V(iVar)
             EXIT
          end do
       end select

       StringVarTec = trim(StringVarTec) // '", "' // NameTecVar

       if (IsDimensionalPlot_I(iFile)) &
            StringVarTec = trim(StringVarTec) // ' ' //NameUnit

    end do

    ! Append a closing double quote
    StringVarTec = trim(StringVarTec) // '"'

    call test_stop(NameSub, DoTest)
  end subroutine set_tecplot_var_string
  !============================================================================
end module ModWriteTecplot
!==============================================================================
