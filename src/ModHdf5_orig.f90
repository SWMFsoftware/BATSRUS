!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModHdf5

  use BATL_lib, ONLY: &
       test_start, test_stop, iProc, iComm

  use ModSize, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK
  use hdf5
  use ModHdf5Utils
  use ModNumConst
  use BATL_size, ONLY: nDim
  use ModMpi
  implicit none
  SAVE

  private ! except

  public:: write_var_hdf5
  public:: write_plot_hdf5
  public:: init_hdf5_plot

  real, parameter:: cHalfMinusTiny = 0.5*(1.0 - 1e-6)

  real, allocatable :: PlotVar_CBV(:,:,:,:,:)
  real, allocatable :: Xyz_DNB(:,:,:,:,:)

  integer, allocatable :: iBlockUsed_B(:)
  integer, allocatable :: iNodeUsed_B(:)
  integer, allocatable :: nBlock_P(:)
  integer, allocatable :: iCoord_DB(:,:)

  integer(HID_T) :: iFileID
  character (len=lNameH5), allocatable :: NameUnknown_V(:)

  integer :: iBlock
  integer :: iSizeGlobal, jSizeGlobal, kSizeGlobal, nPlotDim, iDimPlot_D(3)
  integer :: nMessagesNeeded, nMessagesTotal, iProcOffset, nBlocksUsed
  integer(HSIZE_T) :: nCellsLocal,nCellBlock_D(3), nBlkUsedGlobal, nPlotDim8
  integer(HSIZE_T) :: nNodeBlock_D(3)

  ! one comunicator for each plot
  integer :: iCommPlot, nProcPlot, iProcPlot

contains
  !============================================================================
  subroutine stop_hdf5(String)

    character(len=*), intent(in):: String
    integer:: iError, nError
    !--------------------------------------------------------------------------
    write(*,*)'iError in ModHdf5: ', String
    call MPI_abort(MPI_COMM_WORLD, nError, iError)
    stop

  end subroutine stop_hdf5
  !============================================================================
  subroutine init_hdf5_plot(iFile, TypePlot, nPlotVar, &
       xMin, xMax, yMin, yMax, zMin, zMax, DxBlock, DyBlock, DzBlock,&
       IsNonCartesian, IsNotACut)

    ! Save all cells within plotting range, for each processor

    use ModMain, ONLY: nI, nJ, nK, &
         x_, y_, z_, Phi_, nBlockMax, Unused_B, nBlock
    use ModGeometry, ONLY: CellSize_DB,&
         Coord111_DB
    use ModIO, ONLY: PlotDx_DI
    use BATL_lib, ONLY: Xyz_DNB, IsRLonLat, IsCylindrical, CoordMin_DB,&
         CoordMax_DB, CoordMin_D, CoordMax_D, DiLevelNei_IIIB

    ! Arguments

    integer, intent(in) :: iFile
    integer, intent(in) :: nPlotVar
    real,    intent(in) :: xMin,xMax,yMin,yMax,zMin,zMax
    logical, intent(in) :: IsNonCartesian, IsNotACut
    character(len=*), intent(in) :: TypePlot
    real,    intent(inout):: DxBlock,DyBlock,DzBlock

    ! Local variables
    ! Indices and Coord_DB
    integer :: i, j, k, iMin, iMax, jMin, jMax, kMin, kMax
    integer :: iMin1, iMax1, jMin1, jMax1, kMin1, kMax1
    integer :: iMaxN, jMaxN, kMaxN, iMaxN1, jMaxN1, kMaxN1
    integer :: i2, j2, k2, iSize, jSize, kSize
    integer :: iError,i1, j1, k1,i3, j3, k3
    real    :: Dx
    real    :: xMin1, xMax1, yMin1, yMax1, zMin1, zMax1
    real    :: ySqueezed

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_hdf5_plot'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    iMin = huge(iMin)
    iMax = -huge(iMax)
    jMin = huge(jMin)
    jMax = -huge(jMax)
    kMin = huge(kMin)
    kMax = -huge(kMax)
    iSize = 0
    jSize = 0
    kSize = 0
    nBlocksUsed = 0
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE

       xMin1 = xMin - cHalfMinusTiny*CellSize_DB(x_,iBlock)
       xMax1 = xMax + cHalfMinusTiny*CellSize_DB(x_,iBlock)
       yMin1 = yMin - cHalfMinusTiny*CellSize_DB(y_,iBlock)
       yMax1 = yMax + cHalfMinusTiny*CellSize_DB(y_,iBlock)
       zMin1 = zMin - cHalfMinusTiny*CellSize_DB(z_,iBlock)
       zMax1 = zMax + cHalfMinusTiny*CellSize_DB(z_,iBlock)

       if(IsRLonLat .or. IsCylindrical)then
          ! Make sure that angles around 3Pi/2 are moved to Pi/2 for x=0 cut
          ySqueezed = mod(Coord111_DB(Phi_,iBlock), cPi)
          ! Make sure that small angles are moved to Pi degrees for y=0 cut
          if(ySqueezed < 0.25*cPi .and. &
               abs(yMin+yMax-cTwoPi) < cTiny .and. yMax-yMin < 0.01) &
               ySqueezed = ySqueezed + cPi
       else
          ySqueezed = Coord111_DB(y_,iBlock)
       end if

       if( Coord111_DB(x_,iBlock) > xMax1.or.&
            Coord111_DB(x_,iBlock)+(nI-1)*CellSize_DB(x_,iBlock) < xMin1.or.&
            ySqueezed > yMax1.or.&
            ySqueezed+(nJ-1)*CellSize_DB(y_,iBlock) < yMin1.or.&
            Coord111_DB(z_,iBlock) > zMax1.or.&
            Coord111_DB(z_,iBlock)+(nK-1)*CellSize_DB(z_,iBlock) < zMin1) CYCLE
       if (IsNotACut) then
          iMin = 1
          iMax = nI
          jMin = 1
          jMax = nJ
          kMin = 1
          kMax = nK
          nBlocksUsed = nBlocksUsed + 1
       else

          Dx = PlotDx_DI(1,iFile)
          DxBlock = CellSize_DB(x_,iBlock)
          DyBlock = CellSize_DB(y_,iBlock)
          DzBlock = CellSize_DB(z_,iBlock)

          ! Calculate index limits of cells inside cut
          i = max(1 ,floor((xMin1-Coord111_DB(x_,iBlock))/DxBlock)+2)
          i2 = min(nI,floor((xMax1-Coord111_DB(x_,iBlock))/DxBlock)+1)

          j = max(1 ,floor((yMin1-ySqueezed)/DyBlock)+2)
          j2 = min(nJ,floor((yMax1-ySqueezed)/DyBlock)+1)

          k = max(1 ,floor((zMin1-Coord111_DB(z_,iBlock))/DzBlock)+2)
          k2 = min(nK,floor((zMax1-Coord111_DB(z_,iBlock))/DzBlock)+1)

          i1 = max(0 ,floor((xMin1-Coord111_DB(x_,iBlock))/DxBlock)+2)
          i3 = min(nI+1,floor((xMax1-Coord111_DB(x_,iBlock))/DxBlock)+1)

          j1 = max(0 ,floor((yMin1-ySqueezed)/DyBlock)+2)
          j3 = min(nJ+1,floor((yMax1-ySqueezed)/DyBlock)+1)

          k1 = max(0,floor((zMin1-Coord111_DB(z_,iBlock))/DzBlock)+2)
          k3 = min(nK+1,floor((zMax1-Coord111_DB(z_,iBlock))/DzBlock)+1)

          !         ! Cut falls between iMinCell and iMinCell - 1
          !         if (i1==0 .and.  i==1 .and. i2==1) then
          !
          !             if (DiLevelNei_IIIB(-1, 0, 0,iBlock) == -1) cycle
          !             ! Neighbor is more refined so it writes
          !         end if
          !
          !         if (j1==0 .and. j==1 .and. j2 == 1) then
          !             ! Neighbor is more refined so it writes
          !             if (DiLevelNei_IIIB(0, -1, 0,iBlock) == -1) cycle
          !         end if
          !
          !         if (k1==0 .and. k==1 .and. k2 == 1) then
          !             ! Neighbor is more refined so it writes
          !             if (DiLevelNei_IIIB(0, 0, -1,iBlock) == -1) cycle
          !         end if
          !
          !         ! Cut falls between iMaxCell and iMaxCell 1
          !         if (i3==nI+1 .and.  i2==nI .and. i == nI) then
          !             if (DiLevelNei_IIIB(1, 0, 0,iBlock) == -1) then
          !                 cycle
          !             else if (CoordMax_DB(1, iBlock) /= CoordMax_D(1)) then
          !                 cycle
          !
          !             end if
          !         end if
          !
          !         if (j3==nJ+1 .and. j2==nJ .and. j == nJ) then
          !             if (DiLevelNei_IIIB(0, 1, 0,iBlock) == -1) then
          !                 cycle
          !             else if (CoordMax_DB(2, iBlock) /= CoordMax_D(2)) then
          !                 cycle
          !
          !             end if
          !         end if
          !
          !         if (k3==nK+1 .and. k2==nK .and. k==nK) then
          !             if (DiLevelNei_IIIB(0, 0, 1,iBlock) == -1) then
          !                 cycle
          !             else if (CoordMax_DB(3, iBlock) /= CoordMax_D(3)) then
          !                 cycle
          !
          !             end if
          !         end if
          !
          ! Neighbor is more refined so it writes

          if (i < iMin)&
               iMin = i
          if (i2 > iMax)&
               iMax = i2
          if ((i2-i + 1) > iSize)&
               iSize = i2-i+1

          if (j < jMin)&
               jMin = j
          if (j2 > jMax)&
               jMax = j2
          if ((j2-j+1) > jSize)&
               jSize = j2-j+1

          if (k < kMin)&
               kMin = k
          if (k2 > kMax)&
               kMax = k2
          if ((k2-k+1) > kSize)&
               kSize = k2-k+1

          nBlocksUsed = nBlocksUsed + 1
       end if
    end do

    ! Make a comincator for only procs which has data to save
    if(nBlock == 0) then
       call MPI_Comm_split(iComm,MPI_UNDEFINED,iProc,iCommPlot,iError)
    else
       call MPI_Comm_split(iComm,1,iProc,iCommPlot,iError)
    endif

    if(MPI_COMM_NULL == iCommPlot ) RETURN
    call MPI_COMM_RANK (iCommPlot, iProcPlot, iError)
    call MPI_COMM_SIZE (iCommPlot, nProcPlot, iError)

    allocate(nBlock_P(0:nProcPlot-1))
    call MPI_Allgather(nBlocksUsed, 1, MPI_INTEGER, nBlock_P, &
         1, MPI_INTEGER, iCommPlot, iError)
    if (iProcPlot == 0) then
       iProcOffset = 0
    else
       iProcOffset = sum(nBlock_P(0:iProcPlot-1))
    end if
    nBlkUsedGlobal = sum(nBlock_P(0:nProcPlot-1))
    deallocate(nBlock_P)

    allocate(iBlockUsed_B(nBlocksUsed))
    if(IsNotACut) then
       iSizeGlobal = nI
       jSizeGlobal = nJ
       kSizeGlobal = nK
    else
       call MPI_AllReduce(iSize, iSizeGlobal, 1,&
            MPI_INTEGER, MPI_MAX, iCommPlot, iError)
       call MPI_AllReduce(jSize, jSizeGlobal, 1,&
            MPI_INTEGER, MPI_MAX, iCommPlot, iError)
       call MPI_AllReduce(kSize, kSizeGlobal, 1,&
            MPI_INTEGER, MPI_MAX, iCommPlot, iError)
    end if

    if (IsNonCartesian) then
       if (IsNotACut) then
          nPlotDim = nDim
          iDimPlot_D = 0
          nCellBlock_D = 1
          nNodeBlock_D = 1
          do i = 1, nPlotDim
             iDimPlot_D(i) = i
             if (i == 1) then
                nCellBlock_D(i) = iSizeGlobal
                nNodeBlock_D(i) = iSizeGlobal+1
             else if (i == 2) then
                nCellBlock_D(i) = jSizeGlobal
                nNodeBlock_D(i) = jSizeGlobal+1
             else if (i == 3) then
                nCellBlock_D(i) = kSizeGlobal
                nNodeBlock_D(i) = kSizeGlobal+1
             end if
          end do
       else if (TypePlot=='x=0') then
          nPlotDim = 2
          iDimPlot_D = [2,3,0]
          nCellBlock_D = [jSizeGlobal, kSizeGlobal, 1]
          nNodeBlock_D = [jSizeGlobal+1, kSizeGlobal+1, 1]
       else if(TypePlot=="y=0)") then
          nPlotDim = 2
          iDimPlot_D = [1,3,0]
          nCellBlock_D = [iSizeGlobal, kSizeGlobal, 1]
          nNodeBlock_D = [iSizeGlobal+1, kSizeGlobal+1, 1]
       else if (TypePlot=='z=0' )then
          !              write (*,*) "allocated z=0"
          nPlotDim = 2
          iDimPlot_D = [1,2,0]
          nCellBlock_D = [iSizeGlobal, jSizeGlobal, 1]
          nNodeBlock_D = [iSizeGlobal+1, jSizeGlobal+1, 1]
       end if
    else
       if (IsNotACut) then
          nPlotDim = nDim
          iDimPlot_D = 0
          nCellBlock_D = 1
          nNodeBlock_D = 1
          do i = 1, nPlotDim
             iDimPlot_D(i) = i
             if (i == 1) then
                nCellBlock_D(i) = iSizeGlobal
                nNodeBlock_D(i) = iSizeGlobal+1
             else if (i == 2) then
                nCellBlock_D(i) = jSizeGlobal
                nNodeBlock_D(i) = jSizeGlobal+1
             else if (i == 3) then
                nCellBlock_D(i) = kSizeGlobal
                nNodeBlock_D(i) = kSizeGlobal+1
             end if
          end do

       else if (iSizeGlobal == 1 .and. jSizeGlobal == 1) then
          nPlotDim = 1
          iDimPlot_D = [3,0,0]
          nCellBlock_D = [kSizeGlobal,1,1]
          nNodeBlock_D = [kSizeGlobal+1,1,1]
       else if (iSizeGlobal == 1 .and. kSizeGlobal == 1) then
          nPlotDim = 1
          iDimPlot_D = [2,0,0]
          nCellBlock_D = [jSizeGlobal,1,1]
          nNodeBlock_D = [jSizeGlobal+1,1,1]
       else if (jSizeGlobal == 1 .and. kSizeGlobal == 1) then
          nPlotDim = 1
          iDimPlot_D = [1,0,0]
          nCellBlock_D = [iSizeGlobal,1,1]
          nNodeBlock_D = [iSizeGlobal+1,1,1]
       else if (iSizeGlobal == 1) then
          nPlotDim = 2
          iDimPlot_D = [2,3,0]
          nCellBlock_D = [jSizeGlobal,kSizeGlobal,1]
          nNodeBlock_D = [jSizeGlobal+1,kSizeGlobal+1,1]
       else if (jSizeGlobal == 1) then
          nPlotDim = 2
          iDimPlot_D = [1,3,0]
          nCellBlock_D = [iSizeGlobal,kSizeGlobal,1]
          nNodeBlock_D = [iSizeGlobal+1,kSizeGlobal+1,1]
       else if (kSizeGlobal == 1) then
          nPlotDim = 2
          iDimPlot_D = [1,2,0]
          nCellBlock_D = [iSizeGlobal,jSizeGlobal,1]
          nNodeBlock_D = [iSizeGlobal+1,jSizeGlobal+1,1]
       end if
    end if

    allocate(PlotVar_CBV(nCellBlock_D(1),nCellBlock_D(2),nCellBlock_D(3), &
         nBlocksUsed,nPlotVar))
    PlotVar_CBV=-50
    if (IsNonCartesian) allocate(Xyz_DNB(nPlotDim, &
         nNodeBlock_D(1),nNodeBlock_D(2),nNodeBlock_D(3),nBlocksUsed))

    call test_stop(NameSub, DoTest)

  end subroutine init_hdf5_plot
  !============================================================================
  subroutine write_var_hdf5(iFile, TypePlot, iBlock, iH5Index, nPlotVar, &
       PlotVar_GV, &
       xMin, xMax, yMin, yMax, zMin, zMax, DxBlock, DyBlock, DzBlock,&
       IsNonCartesian, IsNotACut, nCell,DoH5Advance)

    ! Save all cells within plotting range, for each processor

    use ModMain, ONLY: nI, nJ, nK, &
         x_, y_, z_, Phi_, nBlockMax, Unused_B
    use ModGeometry, ONLY: CellSize_DB,&
         Coord111_DB
    use ModIO, ONLY: PlotDx_DI
    use ModMpi
    use BATL_lib, ONLY : Xyz_DNB, IsRLonLat, IsCylindrical, CoordMin_DB,&
         CoordMax_DB, CoordMin_D,CoordMax_D,DiLevelNei_IIIB

    ! Arguments

    integer, intent(in)   :: iFile, iBlock, iH5Index
    integer, intent(in)   :: nPlotVar
    real,    intent(in)   :: PlotVar_GV(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,nPlotVar)
    real,    intent(in)   :: xMin,xMax,yMin,yMax,zMin,zMax
    logical, intent(in) :: IsNonCartesian, IsNotACut
    character (len=*), intent(in) :: TypePlot
    real,    intent(inout):: DxBlock,DyBlock,DzBlock
    integer, intent(out)  :: nCell
    logical, intent(out) :: DoH5Advance

    ! Local variables
    ! Indices and Coord_DB
    integer :: i, j, k, iMin, iMax, jMin, jMax, kMin, kMax
    integer :: iMin1, iMax1, jMin1, jMax1, kMin1, kMax1
    integer ::  iMaxN,jMaxN, kMaxN,iMaxN1,jMaxN1, kMaxN1
    integer :: i2, j2, k2, iSize, jSize, kSize
    integer :: iError,i1, j1, k1,i3, j3, k3
    real ::Dx
    real :: xMin1,xMax1,yMin1,yMax1,zMin1,zMax1

    real :: ySqueezed

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_var_hdf5'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(MPI_COMM_NULL == iCommPlot ) RETURN

    xMin1 = xMin - cHalfMinusTiny*CellSize_DB(x_,iBlock)
    xMax1 = xMax + cHalfMinusTiny*CellSize_DB(x_,iBlock)
    yMin1 = yMin - cHalfMinusTiny*CellSize_DB(y_,iBlock)
    yMax1 = yMax + cHalfMinusTiny*CellSize_DB(y_,iBlock)
    zMin1 = zMin - cHalfMinusTiny*CellSize_DB(z_,iBlock)
    zMax1 = zMax + cHalfMinusTiny*CellSize_DB(z_,iBlock)

    nCell = 0
    if(IsNonCartesian)then
       ! Make sure that angles around 3Pi/2 are moved to Pi/2 for x=0 cut
       ySqueezed = mod(Coord111_DB(Phi_,iBlock),cPi)
       ! Make sure that small angles are moved to Pi degrees for y=0 cut
       if(ySqueezed < 0.25*cPi .and. &
            abs(yMin+yMax-cTwoPi) < cTiny .and. yMax-yMin < 0.01) &
            ySqueezed = ySqueezed + cPi
    else
       ySqueezed = Coord111_DB(y_,iBlock)
    end if
    !
    !   if(DoTest)then
    !      write(*,*) NameSub, 'xMin1,xMax1,yMin1,yMax1,zMin1,zMax1=',&
    !           xMin1,xMax1,yMin1,yMax1,zMin1,zMax1
    !      write(*,*) NameSub, 'Coord111_DB=',iBlock,Coord111_DB(:,iBlock)
    !      write(*,*) NameSub, 'ySqueezed =',ySqueezed
    !      write(*,*) NameSub, 'xyzEnd=', &
    !           Coord111_DB(x_,iBlock)+(nI-1)*CellSize_DB(x_,iBlock),&
    !           ySqueezed+(nJ-1)*CellSize_DB(y_,iBlock),&
    !           Coord111_DB(z_,iBlock)+(nK-1)*CellSize_DB(z_,iBlock)
    !   end if
    !
    ! If block is fully outside of cut then cycle
    if(  Coord111_DB(x_,iBlock) > xMax1.or.&
         Coord111_DB(x_,iBlock)+(nI-1)*CellSize_DB(x_,iBlock) < xMin1.or.&
         ySqueezed > yMax1.or.&
         ySqueezed+(nJ-1)*CellSize_DB(y_,iBlock) < yMin1.or.&
         Coord111_DB(z_,iBlock) > zMax1.or.&
         Coord111_DB(z_,iBlock)+(nK-1)*CellSize_DB(z_,iBlock) < zMin1) then
       DoH5Advance = .false.
       RETURN
    end if

    Dx = PlotDx_DI(1,iFile)
    DxBlock = CellSize_DB(x_,iBlock)
    DyBlock = CellSize_DB(y_,iBlock)
    DzBlock = CellSize_DB(z_,iBlock)

    ! Calculate index limits of cells inside cut

    iMin = max(1 ,floor((xMin1-Coord111_DB(x_,iBlock))/DxBlock)+2)
    iMax = min(nI,floor((xMax1-Coord111_DB(x_,iBlock))/DxBlock)+1)

    jMin = max(1 ,floor((yMin1-ySqueezed)/DyBlock)+2)
    jMax = min(nJ,floor((yMax1-ySqueezed)/DyBlock)+1)

    kMin = max(1,floor((zMin1-Coord111_DB(z_,iBlock))/DzBlock)+2)
    kMax = min(nK,floor((zMax1-Coord111_DB(z_,iBlock))/DzBlock)+1)

    iMin1 = max(-1 ,floor((xMin1-Coord111_DB(x_,iBlock))/DxBlock)+2)
    iMax1 = min(nI+1,floor((xMax1-Coord111_DB(x_,iBlock))/DxBlock)+1)

    jMin1 = max(-1 ,floor((yMin1-ySqueezed)/DyBlock)+2)
    jMax1 = min(nJ+1,floor((yMax1-ySqueezed)/DyBlock)+1)

    kMin1 = max(-1,floor((zMin1-Coord111_DB(z_,iBlock))/DzBlock)+2)
    kMax1 = min(nK+1,floor((zMax1-Coord111_DB(z_,iBlock))/DzBlock)+1)

    !    ! Cut falls between iMinCell and iMinCell - 1
    !    if (iMin1==0 .and.  iMin==1 .and. iMax==1) then
    !         if (DiLevelNei_IIIB(-1, 0, 0,iBlock) == -1) then
    !             DoH5Advance = .false.
    !             return
    !         end if
    !         ! Neighbor is more refined so it writes
    !     end if
    !
    !     if (jMin1==0 .and. jMin==1 .and. jMax == 1) then
    !         ! Neighbor is more refined so it writes
    !         if (DiLevelNei_IIIB(0, -1, 0,iBlock) == -1) then
    !             DoH5Advance = .false.
    !             return
    !         end if
    !     end if
    !     if (kMin1==0 .and. kMin==1 .and. kMax == 1) then
    !         ! Neighbor is more refined so it writes
    !         if (DiLevelNei_IIIB(0, 0, -1,iBlock) == -1) then
    !             DoH5Advance = .false.
    !             return
    !         end if
    !     end if
    !     ! Cut falls between iMaxCell and iMaxCell 1
    !     if (iMax1==nI+1 .and.  iMax==nI .and. iMin == nI) then
    !         if (DiLevelNei_IIIB(1, 0, 0,iBlock) == -1) then
    !             DoH5Advance = .false.
    !             return
    !         else if (CoordMax_DB(1, iBlock) /= CoordMax_D(1)) then
    !             DoH5Advance = .false.
    !             return
    !         end if
    !     end if
    !
    !     if (jMax1==nJ+1 .and. jMax==nJ .and. jMin == nJ) then
    !         if (DiLevelNei_IIIB(0, 1, 0,iBlock) == -1) then
    !             DoH5Advance = .false.
    !             return
    !         else if (CoordMax_DB(2, iBlock) /= CoordMax_D(2)) then
    !             DoH5Advance = .false.
    !             return
    !         end if
    !     end if
    !
    !     if (kMax1==nK+1 .and. kMax==nK .and. kMin==nK) then
    !         if (DiLevelNei_IIIB(0, 0, 1,iBlock) == -1) then
    !             DoH5Advance = .false.
    !             return
    !         else if (CoordMax_DB(3, iBlock) /= CoordMax_D(3)) then
    !             DoH5Advance = .false.
    !             return
    !         end if
    !     end if

    iMaxN = iMax
    jMaxN = jMax
    kMaxN = kMax

    !   if(DoTest)then
    !      write(*,*) NameSub, 'iMin,iMax,jMin,jMax,kMin,kMax=',&
    !           iMin,iMax,jMin,jMax,kMin,kMax
    !      write(*,*) NameSub, 'DxBlock,xMinBox,yMinBox,zMinBox', &
    !           DxBlock,Coord111_DB(:,iBlock)
    !      write(*,*) NameSub, 'ySqueezed  =',ySqueezed
    !      write(*,*) NameSub, 'xMin1,xMax1=',xMin1,xMax1
    !      write(*,*) NameSub, 'yMin1,yMax1=',yMin1,yMax1
    !      write(*,*) NameSub, 'zMin1,zMax1=',zMin1,zMax1
    !   end if
    !

    if (IsNonCartesian) then
       if ( TypePlot=='x=0' .or. TypePlot=='y=0') then
          iMaxN = iMax + 1
          kMaxN = kMax + 1

          iMaxN1 = iMax1 + 1
          kMaxN1 = kMax1 + 1

       else if ( TypePlot=='z=0') then
          iMaxN = iMax + 1
          jMaxN = jmax + 1
          iMaxN1 = iMax1 + 1
          jMaxN1 = jmax1 + 1
       else if (IsNotACut) then
          do i2 = 1, nPlotDim
             if (iDimPlot_D(i2) == 1) then
                iMaxN = iMax + 1
             else if (iDimPlot_D(i2) == 2) then
                jMaxN = jMax + 1
             else if (iDimPlot_D(i2) == 3) then
                kMaxN = kMax + 1
             end if
          end do
       end if
    else
       do i2 = 1, nPlotDim
          if (iDimPlot_D(i2) == 1) then
             iMaxN = iMax + 1
          else if (iDimPlot_D(i2) == 2) then
             jMaxN = jMax + 1
          else if (iDimPlot_D(i2) == 3) then
             kMaxN = kMax + 1
          end if
       end do
    end if

    if (IsNonCartesian) then
       do k2=kMin,kMaxN; do j2=jMin,jMaxN; do i2=iMin,iMaxN
          if (nPlotDim == 3) then
             i = i2 - iMin + 1
             j = j2 - jMin + 1
             k = k2 - kMin + 1

             Xyz_DNB(1,i, j, k, iH5Index) = &
                  Xyz_DNB(1,i2, j2, k2, iBlock)
             Xyz_DNB(2,i, j, k, iH5Index) = &
                  Xyz_DNB(2,i2, j2, k2, iBlock)
             Xyz_DNB(3,i, j, k, iH5Index) = &
                  Xyz_DNB(3,i2, j2, k2, iBlock)

          else if (TypePlot=='x=0') then
             i = i2 - iMin + 1
             j = k2 - kMin + 1
             k = j2 - jMin + 1

             Xyz_DNB(1,i, j, k, iH5Index) = &
                  Xyz_DNB(2,i2, j2, k2, iBlock)

             Xyz_DNB(2,i, j, k, iH5Index) = &
                  Xyz_DNB(3,i2, j2, k2, iBlock)
          else if (TypePlot=='y=0') then
             i = i2 - iMin + 1
             j = k2 - kMin + 1
             k = j2 - jMin + 1

             Xyz_DNB(1,i, j, k, iH5Index) = &
                  xyz_dnb(1,i2, j2, k2, iblock)
             Xyz_DNB(2,i, j, k, iH5Index) = &
                  Xyz_DNB(3,i2, j2, k2, iBlock)
             !
          else if (TypePlot=='z=0') then
             i = i2 - iMin + 1
             j = j2 - jMin + 1
             k = k2 - kMin + 1

             Xyz_DNB(1,i, j, k, iH5Index) = &
                  Xyz_DNB(1,i2, j2, k2, iBlock)
             Xyz_DNB(2,i, j, k, iH5Index) = &
                  Xyz_DNB(2,i2, j2, k2, iBlock)

          else
             i = i2 - iMin + 1
             j = j2 - jMin + 1
             k = k2 - kMin + 1

             Xyz_DNB(1,i, j, k, iH5Index) = &
                  Xyz_DNB(1,i2, j2, k2, iBlock)
             Xyz_DNB(2,i, j, k, iH5Index) = &
                  Xyz_DNB(2,i2, j2, k2, iBlock)
             Xyz_DNB(3,i, j, k, iH5Index) = &
                  Xyz_DNB(3,i2, j2, k2, iBlock)

          end if
          where(abs(Xyz_DNB(:,:,:,:,iH5Index))<1.e-10) &
               Xyz_DNB(:,:,:,:,iH5Index) = 0.
       end do; end do; end do
       do k2=kMin,kMax; do j2=jMin,jMax; do i2=iMin,iMax
          if (nPlotDim == 3 .or. IsNotACut) then
             i = i2 - iMin + 1
             j = j2 - jMin + 1
             k = k2 - kMin + 1
             PlotVar_CBV(i, j, k, iH5Index, 1:nPlotVar) = &
                  PlotVar_GV(i2, j2, k2, 1:nPlotVar)
          else if ( TypePlot=='x=0' .or. TypePlot=='y=0') then
             !              write (*,*) "x=0 y=0"
             i = i2 - iMin + 1
             j = k2 - kMin + 1
             k = j2 - jMin + 1
             PlotVar_CBV(i, j, k, iH5Index, 1:nPlotVar) = &
                  0.5*(PlotVar_GV(i2,jMin1,k2,1:nPlotVar) &
                  +    PlotVar_GV(i2,jMax1,k2,1:nPlotVar))
          else if ( TypePlot=='z=0') then
             !              write (*,*) 'z=0'
             i = i2 - iMin + 1
             j = j2 - jMin + 1
             k = k2 - kMin + 1
             PlotVar_CBV(i, j, k, iH5Index, 1:nPlotVar) = &
                  0.5*(PlotVar_GV(i2,j2,kMin1,1:nPlotVar) &
                  +    PlotVar_GV(i2,j2,kMax1,1:nPlotVar))
          end if
          nCell = nCell+1
       end do; end do; end do
    else
       do k2=kMin,kMax; do j2=jMin,jMax; do i2=iMin,iMax

          if (nPlotDim < 3) then
             if (TypePlot == 'z=0') then
                i = i2 - iMin + 1
                j = j2 - jMin + 1
                k = k2 - kMin + 1
                PlotVar_CBV(i, j, k, iH5Index, 1:nPlotVar) = &
                     0.5*(PlotVar_GV(i2,j2,kmin1,1:nPlotVar) &
                     +    PlotVar_GV(i2,j2,kmax1,1:nPlotVar))
             else if (TypePlot == 'y=0') then
                i = i2 - iMin + 1
                j = k2 - kMin + 1
                k = j2 - jMin + 1
                PlotVar_CBV(i, j, k, iH5Index, 1:nPlotVar) = &
                     0.5*(PlotVar_GV(i2, jMin1 ,k2, 1:nPlotVar) &
                     +    PlotVar_GV(i2, jMax1, k2, 1:nPlotVar))
             else if (TypePlot == 'x=0') then
                i = j2 - jMin + 1
                j = k2 - kMin + 1
                k = i2 - iMin + 1
                PlotVar_CBV(i, j, k, iH5Index, 1:nPlotVar) = &
                     0.5*(PlotVar_GV(iMin1,j2,k2,1:nPlotVar)  &
                     +    PlotVar_GV(iMax1,j2,k2,1:nPlotVar))
             end if
          else
             i = i2 - iMin + 1
             j = j2 - jMin + 1
             k = k2 - kMin + 1
             PlotVar_CBV(i, j, k, iH5Index, 1:nPlotVar) = &
                  PlotVar_GV(i2, j2 ,k2, 1:nPlotVar)
          end if

          nCell = nCell+1
       end do; end do; end do
    end if
    DoH5Advance = .true.
    iBlockUsed_B(iH5Index) = iBlock
    call test_stop(NameSub, DoTest, iBlock)

  end subroutine write_var_hdf5
  !============================================================================
  subroutine write_plot_hdf5( &
       NameFile, TypePlot, NamePlotVar_V, NamePlotUnit_V,&
       nPlotVar, IsNotACut, IsNonCartesian, IsSphPlot, IsDimensionalPlot, &
       xmin, xmax, ymin, ymax, zmin, zmax)

    use ModNumConst
    use ModMpi
    use BATL_lib,  ONLY: CoordMin_DB, CoordMax_DB, MaxDim, &
         iNode_B, iTree_IA, Coord0_, Level_, iMortonNode_A
    use BATL_mpi,  ONLY: barrier_mpi

    integer,                 intent(in):: nPlotVar
    character(len=80),       intent(in):: NameFile
    character(len=lnamevar), intent(in):: NamePlotVar_V(nPlotVar)
    character(len=lNameVar),  intent(in):: NamePlotUnit_V(nPlotVar)
    character (len=*), intent(in) :: TypePlot
    real, intent(in)  ::  xMin, xMax, yMin, yMax, zMin, zMax
    logical, intent(in) :: &
         IsNotACut, IsDimensionalPlot, IsNonCartesian, IsSphPlot
    integer :: iError, iLen, iVar, nBlockUsedMax

    integer :: lLabel, i, i2, nBlocksUsedMin
    integer(HID_T) :: iIntHIDT

    real, allocatable :: Coord_DB(:,:), CoordLimit_SDB(:,:,:),DataLimit_SB(:,:)
    integer, allocatable :: iProcPlot_B(:)
    real :: VarMin, VarMax
    real :: GlobalVarMin_V(nPlotVar), GlobalVarMax_V(nPlotVar)
    character(len=lnamevar+4) :: NameDataTmp

    logical :: IsCutFile

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_plot_hdf5'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(MPI_COMM_NULL == iCommPlot ) RETURN

    if (IsNotACut) then
       IsCutFile = .false.
    else
       IsCutFile = .true.
    end if

    if (nBlocksUsed > 0) then
       allocate(iNodeUsed_B(nBlocksUsed))
       iNodeUsed_B(1:nBlocksUsed) = iNode_B(iBlockUsed_B(1:nBlocksUsed))
    else
       allocate(iNodeUsed_B(1))
       iNodeUsed_B(1) = 0
    end if
    do iVar = 1, nPlotVar
       if (nBlocksUsed > 0) then
          VarMin = minval(PlotVar_CBV(:,:,:,:,iVar))
          VarMax = maxval(PlotVar_CBV(:,:,:,:,iVar))
       else
          VarMin = huge(VarMin)
          VarMax = -huge(VarMax)
       end if
       call MPI_allreduce(VarMin, GlobalVarMin_V(iVar), 1, MPI_REAL,&
            MPI_MIN, iCommPlot, iError)

       call MPI_allreduce(VarMax, GlobalVarMax_V(iVar), 1, MPI_REAL,&
            MPI_MAX, iCommPlot, iError)

    end do

    ! Open the hdf5 file
    call open_hdf5_file(iFileID, NameFile, iCommPlot)
    if(iFileID == -1) then
       if (iProcPlot == 0) write (*,*)  "iError: unable to initialize file"
       call stop_hdf5("unable to initialize hdf5 file")
    end if

    allocate(NameUnknown_V(nPlotVar))

    call pad_string_with_null( &
         nPlotVar, lNameH5, NamePlotVar_V, NameUnknown_V)
    call write_hdf5_data(iFileID, "NamePlotVar_V", 1, [int(nPlotVar,HSIZE_T)],&
         CharacterData=NameUnknown_V, nStringChars=int(lNameH5,HSIZE_T))

    ! The plugn doesn't need these. However it does use this info to speed some
    ! things up if it is there.  Perhaps this could be an option in PARAM.in
    allocate (DataLimit_SB(2,nBlocksUsed))
    do iVar = 1, nPlotVar
       do iBlock = 1, nBlocksUsed
          DataLimit_SB(1, iBlock) = minval(PlotVar_CBV(:,:,:,iBlock,iVar))
          DataLimit_SB(2, iBlock) = maxval(PlotVar_CBV(:,:,:,iBlock,iVar))
       end do
       NameDataTmp = trim(NamePlotVar_V(iVar))//"_Ext"
       lLabel = len_trim(NameUnknown_V(iVar)) + 4
       do iLen = lLabel + 1, lNameH5 + 3
          NameDataTmp(iLen:iLen) = CHAR(0)
       end do
       if (nBlocksUsed == 0 ) then
          call write_hdf5_data(iFileID, NameDataTmp, 2, &
               [2_HSIZE_T, nBlkUsedGlobal],&
               nOffsetLocal=iProcOffset, nBlocksLocalIn=nBlocksUsed,&
               Rank2RealData=reshape([0.0,0.0,0.0,0.0],[2,2]))
       else
          call write_hdf5_data(iFileID, NameDataTmp, 2, &
               [2_HSIZE_T, nBlkUsedGlobal],&
               nOffsetLocal=iProcOffset, nBlocksLocalIn=nBlocksUsed,&
               Rank2RealData=DataLimit_SB)
       end if

    end do

    deallocate (DataLimit_SB)

    do iVar = 1, nPlotVar
       call write_hdf5_data(iFileID, NameUnknown_V(iVar), 4, &
            [nCellBlock_D(1), nCellBlock_D(2), nCellBlock_D(3), &
            nBlkUsedGlobal], &
            nOffsetLocal=iProcOffset, nBlocksLocalIn=nBlocksUsed,&
            Rank4RealData=PlotVar_CBV(:,:,:,:,iVar),&
            RealAttribute1=GlobalVarMin_V(iVar), &
            RealAttribute2=GlobalVarMax_V(iVar), &
            NameRealAttribute1="minimum", NameRealAttribute2="maximum")
    end do

    allocate(CoordLimit_SDB(2,nPlotDim,nBlocksUsed))
    if (IsNonCartesian) then
       call write_hdf5_data( iFileID, "NodesX", 4, &
            [nNodeBlock_D(1), nNodeBlock_D(2), nNodeBlock_D(3), &
            nBlkUsedGlobal], nOffsetLocal=iProcOffset, &
            nBlocksLocalIn=nBlocksUsed,&
            Rank4RealData=Xyz_DNB(1,:,:,:,:))
       if (nPlotDim >= 2) then
          call write_hdf5_data(iFileID, "NodesY", 4, &
               [nNodeBlock_D(1), nNodeBlock_D(2), nNodeBlock_D(3), &
               nBlkUsedGlobal], nOffsetLocal=iProcOffset, &
               nBlocksLocalIn=nBlocksUsed,&
               Rank4RealData=Xyz_DNB(2,:,:,:,:))
          if (nPlotDim == 3) &
               call write_hdf5_data(iFileID, "NodesZ", 4, &
               [nNodeBlock_D(1), nNodeBlock_D(2), nNodeBlock_D(3), &
               nBlkUsedGlobal], nOffsetLocal=iProcOffset, &
               nBlocksLocalIn=nBlocksUsed,&
               Rank4RealData=Xyz_DNB(3,:,:,:,:))
       end if
       do iBlock = 1, nBlocksUsed
          CoordLimit_SDB(1,1,iBlock) = minval(Xyz_DNB(1,:,:,:,iBlock))
          CoordLimit_SDB(2,1,iBlock) = maxval(Xyz_DNB(1,:,:,:,iBlock))
          if (nPlotDim >= 2) then
             CoordLimit_SDB(1,2,iBlock) = minval(Xyz_DNB(2,:,:,:,iBlock))
             CoordLimit_SDB(2,2,iBlock) = maxval(Xyz_DNB(2,:,:,:,iBlock))
          end if
          if (nPlotDim == 3) then
             CoordLimit_SDB(1,3,iBlock) = minval(Xyz_DNB(3,:,:,:,iBlock))
             CoordLimit_SDB(2,3,iBlock) = maxval(Xyz_DNB(3,:,:,:,iBlock))
          end if
       end do
       deallocate(Xyz_DNB)

    else
       CoordLimit_SDB(1,:,:) = CoordMin_DB(iDimPlot_D(1:nPlotDim), &
            iBlockUsed_B)
       CoordLimit_SDB(2,:,:) = CoordMax_DB(iDimPlot_D(1:nPlotDim), &
            iBlockUsed_B)
    end if
    nPlotDim8 = nPlotDim
    call write_hdf5_data(iFileID, "bounding box", 3, &
         [2_HSIZE_T, nPlotDim8, nBlkUsedGlobal],&
         nOffsetLocal=iProcOffset, nBlocksLocalIn=nBlocksUsed,&
         Rank3RealData=CoordLimit_SDB)
    allocate(Coord_DB(nPlotDim, nBlocksUsed))
    do iBlock = 1, nBlocksUsed
       do i = 1, nPlotDim
          Coord_DB(i, iBlock) = &
               0.5*(CoordLimit_SDB(1,i,iBlock) + CoordLimit_SDB(2,i,iBlock))
       end do
    end do
    deallocate(CoordLimit_SDB)
    call MPI_Barrier(iCommPlot,iError)
    call write_hdf5_data(iFileID, "Coord_DB", 2, &
         [nPlotDim8, nBlkUsedGlobal],&
         nOffsetLocal=iProcOffset, nBlocksLocalIn=nBlocksUsed,&
         Rank2RealData=Coord_DB)
    deallocate(Coord_DB)

    call MPI_Barrier(iCommPlot,iError)
    if (nBlocksUsed > 0) then
       allocate(iCoord_DB(nPlotDim, nBlocksUsed))
       do iVar = 1,nPlotDim
          iCoord_DB(:,:) = &
               iTree_IA(iDimPlot_D(1:nPlotDim)+Coord0_, iNodeUsed_B) &
               *nCellBlock_D(iVar)
       end do
    else
       allocate(iCoord_DB(nPlotDim, 1))
       iCoord_DB = huge(iCoord_DB(1,1))
    end if

    do iVar = 1,nPlotDim
       i = minval(iCoord_DB(iVar,:))
       call MPI_allreduce(i, i2, 1, MPI_INTEGER,&
            MPI_MIN, iCommPlot, iError)
       do iBlock = 1, nBlocksUsed
          iCoord_DB(iVar,iBlock) = iCoord_DB(iVar,iBlock) - i2
       end do
    end do

    call  write_hdf5_data(iFileID, "iCoord_DB", 2, &
         [nPlotDim8, nBlkUsedGlobal],&
         nOffsetLocal=iProcOffset, nBlocksLocalIn=nBlocksUsed,&
         Rank2IntegerData=iCoord_DB)
    deallocate(iCoord_DB)

    call pad_string_with_null(nPlotVar, lNameH5, NamePlotUnit_V, &
         NameUnknown_V)

    call write_hdf5_data( &
         iFileID, "NamePlotUnit_V", 1, [int(nPlotVar,HSIZE_T)],&
         CharacterData=NameUnknown_V, nStringChars=int(lNameH5,HSIZE_T))

    call MPI_Barrier(iCommPlot,iError)
    deallocate(PlotVar_CBV)
    deallocate(NameUnknown_V)

    call MPI_Barrier(iCommPlot,iError)
    if (IsNotACut .and. nBlocksUsed > 0) then
       call  write_hdf5_data(iFileID, "iMortonNode_A", 1, [nBlkUsedGlobal],&
            nOffsetLocal=iProcOffset, nBlocksLocalIn=nBlocksUsed,&
            Rank1IntegerData=iMortonNode_A(iNodeUsed_B))
    else if (IsNotACut) then
       call  write_hdf5_data(iFileID, "iMortonNode_A", 1, [nBlkUsedGlobal],&
            nOffsetLocal=iProcOffset, nBlocksLocalIn=nBlocksUsed,&
            Rank1IntegerData=[0])
    end if
    !
    call MPI_Barrier(iCommPlot,iError)
    if (nBlocksUsed > 0 ) then
       call  write_hdf5_data(iFileID, "refine level", 1, [nBlkUsedGlobal],&
            nOffsetLocal=iProcOffset, nBlocksLocalIn=nBlocksUsed,&
            Rank1IntegerData=iTree_IA(Level_,iNodeUsed_B))
       deallocate(iNodeUsed_B)
    else
       call  write_hdf5_data(iFileID, "refine level", 1, [nBlkUsedGlobal],&
            nOffsetLocal=iProcOffset, nBlocksLocalIn=nBlocksUsed,&
            Rank1IntegerData=[0])
       deallocate(iNodeUsed_B)
    end if

    call MPI_Barrier(iCommPlot,iError)
    allocate(iProcPlot_B(nBlocksUsed))
    iProcPlot_B = iProcPlot
    call  write_hdf5_data(iFileID, "Processor Number", 1, [nBlkUsedGlobal],&
         nOffsetLocal=iProcOffset, nBlocksLocalIn=nBlocksUsed,&
         Rank1IntegerData=iProcPlot_B)
    deallocate(iProcPlot_B)
    deallocate(iBlockUsed_B)
    allocate(NameUnknown_V(nPlotDim))

    call MPI_Barrier(iCommPlot,iError)
    if (TypePlot=='x=0') then
       NameUnknown_V(1) = "Y-Axis"
       if (nPlotDim == 2) &
            NameUnknown_V(2) = "Z-Axis"
    else
       do iVar = 1, nPlotDim

          if (iDimPlot_D(iVar) == 1) then
             NameUnknown_V(iVar) = "X-Axis"
          else if (iDimPlot_D(iVar) == 2) then
             NameUnknown_V(iVar) = "Y-Axis"
          else if (iDimPlot_D(iVar) == 3) then
             NameUnknown_V(iVar) = "Z-Axis"
          end if
       end do
    end if

    call pad_string_with_null( &
         nPlotDim, lNameH5, NameUnknown_V, NameUnknown_V)
    call write_hdf5_data(iFileID, "Axis Labels", 1, [nPlotDim8],&
         CharacterData=NameUnknown_V, nStringChars=int(lNameH5, HSIZE_T))

    deallocate(NameUnknown_V)
    iIntHIDT = nPlotVar ! Convert to correct data type.
    call write_integer_sim_metadata(iFileID, iIntHIDT)
    call write_real_sim_metadata(iFileID,IsDimensionalPlot)
    call write_integer_plot_metadata(iFileID, iIntHIDT, IsCutFile)
    if (TypePlot=='x=0') then
       call write_real_plot_metadata(iFileID,IsDimensionalPlot, .true.)
    else
       call write_real_plot_metadata(iFileID,IsDimensionalPlot, .false.)
    end if

    call MPI_Barrier(iCommPlot,iError)
    call close_hdf5_file(iFileID)

    call MPI_Comm_Free(iCommPlot,iError)

    call test_stop(NameSub, DoTest)

  end subroutine write_plot_hdf5
  !============================================================================
  subroutine write_real_plot_metadata(iFileID,IsDimensionalPlot, IsXZero)

    use ModMain, ONLY : tSimulation
    use BATL_lib, ONLY : nLevelMax, nDim
    use ModGeometry, ONLY : xMinBox,xMaxBox,yMinBox,yMaxBox,zMinBox,zMaxBox
    use ModPhysics, ONLY : No2Io_V, UnitX_

    integer(HID_T), intent(in) :: iFileID
    integer(HSIZE_T) :: iData
    integer :: i
    logical, intent (in) :: IsDimensionalPlot, IsXZero

    real :: MetaData_I(7)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_real_plot_metadata'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(MPI_COMM_NULL == iCommPlot ) RETURN

    iData = 1
    !    attName(1) = 'Simulation Time'
    MetaData_I(iData) = tSimulation
    iData = iData + 1
    if (IsXZero) then
       MetaData_I(iData) = yMinBox
       iData = iData + 1
       MetaData_I(iData) = yMaxBox
       iData = iData + 1
       if (nPlotDim == 1) then
          MetaData_I(iData) = 0.0
          iData = iData + 1
          MetaData_I(iData) = 1.0
       else
          MetaData_I(iData) = zMinBox
          iData = iData + 1
          MetaData_I(iData) = zMaxBox
       end if
       iData = iData + 1
       MetaData_I(iData) = 0.0
       iData = iData + 1
       MetaData_I(iData) = 0.0
       iData = iData + 1
    else
       do i=1,3
          if (iDimPlot_D(i) == 1) then
             MetaData_I(iData) = xMinBox
             iData = iData + 1
             MetaData_I(iData) = xMaxBox
          else if(iDimPlot_D(i) == 2) then
             MetaData_I(iData) = yMinBox
             iData = iData + 1
             MetaData_I(iData) = yMaxBox
          else if(iDimPlot_D(i) == 3) then
             MetaData_I(iData) = zMinBox
             iData = iData + 1
             MetaData_I(iData) = zMaxBox
          else if (i == 2 .and. nPlotDim == 1) then
             MetaData_I(iData) = 0.0
             iData = iData + 1
             MetaData_I(iData) = 1.0
          else if (iDimPlot_D(i) == 0) then
             MetaData_I(iData) = 0.0
             iData = iData + 1
             MetaData_I(iData) = 0.0
          end if
          iData = iData + 1
       end do
    end if
    iData = iData -1

    ! write the real Metadata
    call  write_hdf5_data(iFileID, "Real Plot Metadata", 1, [iData],&
         Rank1RealData=MetaData_I)

    call test_stop(NameSub, DoTest)

  end subroutine write_real_plot_metadata
  !============================================================================
  subroutine write_real_sim_metadata(iFileID,IsDimensionalPlot)

    use ModMain, only : tSimulation
    use BATL_lib, only : nLevelMax, nDim
    use ModGeometry, only : xMinBox,xMaxBox,yMinBox,yMaxBox,zMinBox,zMaxBox
    use ModPhysics, ONLY : No2Io_V, UnitX_

    integer (HID_T), intent(in) :: iFileID
    logical, intent (in) :: IsDimensionalPlot
    integer(HSIZE_T) :: iData

    real :: MetaData_I(7)
    !    allocate(attName(nAtts))

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_real_sim_metadata'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(MPI_COMM_NULL == iCommPlot ) RETURN

    iData = 1
    !    attName(1) = 'Simulation Time'
    MetaData_I(iData) = tSimulation
    iData = iData + 1
    !    attName(2) = 'xmin'

    if (IsDimensionalPlot) then
       MetaData_I(iData) = xMinBox*No2Io_V(UnitX_)
       iData = iData + 1
       !    attName(3) = 'xmax'
       MetaData_I(iData) = xMaxBox*No2Io_V(UnitX_)
       iData = iData + 1
       !    attName(4) = 'ymin'
       MetaData_I(iData) = yMinBox*No2Io_V(UnitX_)
       iData = iData + 1
       !    attName(3) = 'ymax'
       MetaData_I(iData) = yMaxBox*No2Io_V(UnitX_)
       iData = iData + 1
       !    attName(6) = 'zmin'
       MetaData_I(iData) = zMinBox*No2Io_V(UnitX_)
       iData = iData + 1
       !    attName(3) = 'zmax'
       MetaData_I(iData) = zMaxBox*No2Io_V(UnitX_)

    else
       !   write (*,*) "xMinBox,xMaxBox,yMinBox,yMaxBox,zMinBox,zMaxBox", &
       !        xMinBox,xMaxBox,yMinBox,yMaxBox,zMinBox,zMaxBox
       MetaData_I(iData) = xMinBox
       iData = iData + 1
       !    attName(3) = 'xmax'
       MetaData_I(iData) = xMaxBox
       iData = iData + 1
       !    attName(4) = 'ymin'
       MetaData_I(iData) = yMinBox
       iData = iData + 1
       !    attName(3) = 'ymax'
       MetaData_I(iData) = yMaxBox
       iData = iData + 1
       !    attName(6) = 'zmin'
       MetaData_I(iData) = zMinBox
       iData = iData + 1
       !    attName(3) = 'zmax'
       MetaData_I(iData) = zMaxBox
    end if

    ! write the real Metadata
    call  write_hdf5_data(iFileID, "Real Simulation Metadata", 1, [iData],&
         Rank1RealData=MetaData_I)

    call test_stop(NameSub, DoTest)

  end subroutine write_real_sim_metadata
  !============================================================================
  subroutine write_integer_plot_metadata(iFileID, nPlotVar, IsCutFile)

    use BATL_lib, ONLY : nDimAmr, nLevelMax, IsPeriodic_D
    use ModMain, ONLY : nStep
    use ModGeometry, ONLY : TypeGeometry

    integer (HID_T), intent(in) :: iFileID, nPlotVar
    logical, intent(in) :: IsCutFile

    integer(HSIZE_T) ::  iData
    integer :: i, IntMetaData_I(16)
    integer, parameter  :: Ffv_ = 1 ! file format version

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_integer_plot_metadata'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(MPI_COMM_NULL == iCommPlot ) RETURN

    iData = 1

    IntMetaData_I(iData) = Ffv_
    !    attName(1) = 'File Format Version'
    iData = iData + 1
    IntMetaData_I(iData) = nStep
    !    attName(2) = "Time Step"
    iData = iData + 1
    IntMetaData_I(iData) = nPlotDim
    !    attName(3) = 'nDim'
    iData = iData + 1
    IntMetaData_I(iData) = nDimAmr
    !    attName(3) = 'nDim'
    iData = iData + 1
    IntMetaData_I(iData) = nBlkUsedGlobal
    !    attName(4) = 'globalNumBlocks'
    iData = iData + 1
    IntMetaData_I(iData) = nProcPlot
    !    attName(5) = 'numProcessors'
    iData = iData + 1

    IntMetaData_I(iData) = nLevelMax
    !    attName(7) = 'nLevelMax'
    iData = iData + 1
    do i = 1, 3
       if (iDimPlot_D(i) == 1) then
          IntMetaData_I(iData) = iSizeGlobal
       else if (iDimPlot_D(i) == 2) then
          IntMetaData_I(iData) = jSizeGlobal
       else if (iDimPlot_D(i) == 3) then
          IntMetaData_I(iData) = kSizeGlobal
       else if (iDimPlot_D(i) == 0) then
          IntMetaData_I(iData) = 0
       end if
       iData = iData + 1
    end do

    if(TypeGeometry=='cartesian') then
       IntMetaData_I(iData) = CartesianPlot_
    elseif(TypeGeometry=='rz') then
       IntMetaData_I(iData) = RzPlot_
    elseif(TypeGeometry=='roundcube') then
       IntMetaData_I(iData) = RoundCubePlot_
    elseif(TypeGeometry=='cylindrical') then
       IntMetaData_I(iData) = CylindricalPlot_
    elseif(TypeGeometry=='cylindrical_lnr') then
       IntMetaData_I(iData) = CylindricalLnrPlot_
    elseif(TypeGeometry=='cylindrical_genr') then
       IntMetaData_I(iData) = CylindricalGenrPlot_
    elseif(TypeGeometry=='spherical') then
       IntMetaData_I(iData) = SphericalPlot_
    elseif(TypeGeometry=='spherical_lnr') then
       IntMetaData_I(iData) = SphericalLnrPlot_
    elseif(TypeGeometry=='spherical_genr') then
       IntMetaData_I(iData) = SphericalGenrPlot_
    elseif(TypeGeometry=='rlonlat') then
       IntMetaData_I(iData) = rLonLatPlot_
    elseif(TypeGeometry=='rlonlat_lnr') then
       IntMetaData_I(iData) = rLonLatLnrPlot_
    elseif(TypeGeometry=='rlonlat_genr') then
       IntMetaData_I(iData) = rLonLatGenrPlot_
    endif
    iData = iData + 1
    ! as of 2/3/2012 this is not implimented in the plugin but it probably
    ! should be in the future
    do i = 1, 3
       if(IsPeriodic_D(i)) then
          IntMetaData_I(iData) = 1
       else
          IntMetaData_I(iData) = 0
       endif
       iData = iData + 1
    end do

    if (IsCutFile) then
       IntMetaData_I(iData) = 1
    else
       IntMetaData_I(iData) = 0
    end if
    iData = iData + 1
    IntMetaData_I(iData) = nPlotVar

    ! write the integer Metadata
    call  write_hdf5_data(iFileID, "Integer Plot Metadata", 1, [iData],&
         Rank1IntegerData=IntMetaData_I)

    call test_stop(NameSub, DoTest)

  end subroutine write_integer_plot_metadata
  !============================================================================
  subroutine write_integer_sim_metadata(iFileID, nPlotVar)

    ! Not read by plugin at this time  Only exists so that one looking at
    ! the file may know something about the simulation that created it

    use BATL_lib, only : nDim, nDimAmr, nLevelMax
    use ModMain, only : nStep, nI, nJ, nK
    integer (HID_T), intent(in) :: iFileID, nPlotVar

    integer(HSIZE_T) :: iData
    integer :: IntMetaData_I(8)
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_integer_sim_metadata'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(MPI_COMM_NULL == iCommPlot ) RETURN

    iData = 1

    IntMetaData_I(iData) = nI
    !    attName(8) = 'nxb'
    iData = iData + 1
    IntMetaData_I(iData) = nJ
    !    attName(9) = 'nyb'
    iData = iData + 1
    IntMetaData_I(iData) = nK
    !    attName(10) = 'nzb'
    iData = iData + 1
    IntMetaData_I(iData) = nDim
    !    attName(3) = 'nDim'
    iData = iData + 1
    IntMetaData_I(iData) = nDimAmr
    !    attName(3) = 'nDim'
    iData = iData + 1
    IntMetaData_I(iData) = nProcPlot
    !    attName(5) = 'numProcessors'
    iData = iData + 1

    IntMetaData_I(iData) = nLevelMax
    !    attName(7) = 'nLevelMax'
    iData = iData + 1

    IntMetaData_I(iData) = nStep
    !    attName(2) = "Time Step"
    !    iData = iData + 1

    ! write the integer Metadata
    call  write_hdf5_data(iFileID, "Integer Sim Metadata", 1, [iData],&
         Rank1IntegerData=IntMetaData_I)
    call test_stop(NameSub, DoTest)

  end subroutine write_integer_sim_metadata
  !============================================================================
  real function minmod(x,y)

    real, intent(in) :: x,y
    !--------------------------------------------------------------------------
    minmod = max(0.0, min(abs(x), sign(1.0,x)*y))

  end function minmod
  !============================================================================
end module ModHdf5
!==============================================================================
