!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModWritePlotIdl

  use BATL_lib, ONLY: &
       test_start, test_stop, StringTest

  implicit none

  private ! except

  public:: write_plot_idl
  public:: nCharPerLine

  ! Maximum number of variables to be written in one line
  integer, parameter :: nVarMax = 100
  ! The width of the real number format
  integer, parameter :: nWidthReal = 13
  integer, parameter :: nCharPerLineMax = nWidthReal*(nVarMax+4) + 1
  character(len=nCharPerLineMax) :: StringLine
  integer :: nCharPerLine = -1

contains
  !============================================================================
  subroutine write_plot_idl(iUnit, iFile, iBlock, nPlotVar, PlotVar_GV, &
       DoSaveGenCoord, xUnit, xMin, xMax, yMin, yMax, zMin, zMax, &
       CellSize1, CellSize2, CellSize3, nCell, nOffset, UseMpiIOIn, &
       DoCountOnlyIn)

    ! Save all cells within plotting range, for each processor

    use ModGeometry, ONLY: &
         xMinBox, xMaxBox, yMinBox, yMaxBox, zMinBox, zMaxBox, Coord111_DB
    use ModIO, ONLY: &
         DoSaveBinary, TypePlot, PlotDx_DI, PlotRange_EI
    use ModNumConst, ONLY: cPi, cTwoPi
    use ModKind, ONLY: nByteReal
    use ModAdvance, ONLY: State_VGB, Bx_
    use ModB0, ONLY: B0_DGB
    use ModMain, ONLY: UseB0
    use ModMpi, ONLY: MPI_OFFSET_KIND
    use BATL_size, ONLY: nGI, nGJ, nGK, nDim
    use BATL_lib, ONLY: IsRLonLat, IsCylindrical, &
         CoordMin_D, CoordMax_D, CoordMin_DB, CellSize_DB, &
         nI, nJ, nK, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, &
         Xyz_DGB, x_, y_, z_, Phi_
    use ModUtilities, ONLY: greatest_common_divisor
    use ModBatsrusUtility, ONLY: stop_mpi

    ! Arguments

    integer, intent(in)   :: iUnit, iFile, iBlock
    integer, intent(in)   :: nPlotVar
    real,    intent(in)   :: PlotVar_GV(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,nPlotVar)
    logical, intent(in)   :: DoSaveGenCoord      ! save gen. or x,y,z coords
    real,    intent(in)   :: xUnit               ! unit for coordinates
    real,    intent(in)   :: xMin, xMax, yMin, yMax, zMin, zMax
    real,    intent(inout):: CellSize1, CellSize2, CellSize3
    integer, intent(out)  :: nCell
    integer(MPI_OFFSET_KIND), intent(inout) :: nOffset
    logical, optional, intent(in) :: UseMpiIOIn
    ! Only count the number of cells that will be written.
    logical, optional, intent(in) :: DoCountOnlyIn

    ! Local variables
    ! Indices and coordinates
    integer :: iVar, i, j, k, i2, j2, k2, iMin, iMax, jMin, jMax, kMin, kMax
    integer :: nRestrict, nRestrictX, nRestrictY, nRestrictZ
    real :: Coord_D(3), x, y, z, ySqueezed, Dx, Restrict
    real :: xMin1, xMax1, yMin1, yMax1, zMin1, zMax1
    real :: Plot_V(nPlotVar)
    logical:: IsBinary

    real:: cHalfMinusTiny

    logical:: UseMpiIO
    logical:: DoCountOnly

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_plot_idl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(nByteReal == 8)then
       cHalfMinusTiny = 0.5*(1.0 - 1e-9)
    else
       cHalfMinusTiny = 0.5*(1.0 - 1e-6)
    end if

    UseMpiIO = .false.
    if(present(UseMpiIOIn)) UseMpiIO = UseMpiIOIn

    DoCountOnly = .false.
    if(present(DoCountOnlyIn)) DoCountOnly = DoCountOnlyIn

    IsBinary = DoSaveBinary .and. TypePlot /= 'cut_pic'

    if(.not. IsBinary) then
       if(nPlotVar > nVarMax) &
            call stop_mpi(NameSub//': nPlotVar > nVarMax')

       nCharPerLine = nWidthReal*(nPlotVar+4) + 1
    end if

    ! Initialize number of cells saved from this block
    ! Note that if this is moved inside the if statement
    ! the NAG compiler with optimization on fails !
    nCell = 0

    if(index(StringTest,'SAVEPLOTALL')>0)then

       if(.not. DoCountOnly) then
          ! Save all cells of block including ghost cells
          CellSize1 = CellSize_DB(x_,iBlock)
          CellSize2 = CellSize_DB(y_,iBlock)
          CellSize3 = CellSize_DB(z_,iBlock)

          PlotRange_EI(1,iFile) = CoordMin_D(1) - nGI*CellSize1
          PlotRange_EI(2,iFile) = CoordMax_D(1) + nGI*CellSize1
          PlotRange_EI(3,iFile) = CoordMin_D(2) - nGJ*CellSize2
          PlotRange_EI(4,iFile) = CoordMax_D(2) + nGJ*CellSize2
          PlotRange_EI(5,iFile) = CoordMin_D(3) - nGK*CellSize3
          PlotRange_EI(6,iFile) = CoordMax_D(3) + nGK*CellSize3
          PlotDx_DI(1,iFile) = CellSize1
          PlotDx_DI(2,iFile) = CellSize2
          PlotDx_DI(3,iFile) = CellSize3
       end if

       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          nCell = nCell + 1
          if(DoCountOnly) CYCLE

          if(DoSaveGenCoord)then
             Coord_D = CoordMin_DB(:,iBlock) &
                  + ([i,j,k] - 0.5)*CellSize_DB(:,iBlock)
          else
             Coord_D = Xyz_DGB(:,i,j,k,iBlock)*xUnit
          end if

          Plot_V = PlotVar_GV(i,j,k,1:nPlotVar)

          call write_data(iUnit, CellSize1, xUnit, Coord_D, Plot_V, &
               nPlotVar, IsBinary, UseMpiIO, nOffset)
       end do; end do; end do

       RETURN

    end if

    ! The range for the cell centers is Dx/2 wider
    xMin1 = xMin - cHalfMinusTiny*CellSize_DB(x_,iBlock)
    xMax1 = xMax + cHalfMinusTiny*CellSize_DB(x_,iBlock)
    yMin1 = yMin - cHalfMinusTiny*CellSize_DB(y_,iBlock)
    yMax1 = yMax + cHalfMinusTiny*CellSize_DB(y_,iBlock)
    zMin1 = zMin - cHalfMinusTiny*CellSize_DB(z_,iBlock)
    zMax1 = zMax + cHalfMinusTiny*CellSize_DB(z_,iBlock)

    if((IsRLonLat .or. IsCylindrical) .and. .not.DoSaveGenCoord)then
       ! Make sure that angles around 3Pi/2 are moved to Pi/2 for x=0 cut
       ySqueezed = mod(Coord111_DB(Phi_,iBlock),cPi)
       ! Make sure that small angles are moved to Pi degrees for y=0 cut
       if(ySqueezed < 0.25*cPi .and. &
            abs(yMin+yMax-cTwoPi) < 1e-6 .and. yMax-yMin < 0.01) &
            ySqueezed = ySqueezed + cPi
    else
       ySqueezed = Coord111_DB(y_,iBlock)
    end if

    if(DoTest)then
       write(*,*) NameSub, 'xMin1,xMax1,yMin1,yMax1,zMin1,zMax1=',&
            xMin1,xMax1,yMin1,yMax1,zMin1,zMax1
       write(*,*) NameSub, 'Coord111_DB=',iBlock,Coord111_DB(:,iBlock)
       write(*,*) NameSub, 'ySqueezed =',ySqueezed
       write(*,*) NameSub, 'xyzEnd=', &
            Coord111_DB(x_,iBlock)+(nI-1)*CellSize_DB(x_,iBlock),&
            ySqueezed + (nJ-1)*CellSize_DB(y_,iBlock),&
            Coord111_DB(z_,iBlock)+(nK-1)*CellSize_DB(z_,iBlock)
    end if

    ! If block is fully outside of cut then cycle
    if(  Coord111_DB(x_,iBlock) > xMax1.or.&
         Coord111_DB(x_,iBlock)+(nI-1)*CellSize_DB(x_,iBlock) < xMin1.or.&
         ySqueezed > yMax1.or.&
         ySqueezed+(nJ-1)*CellSize_DB(y_,iBlock) < yMin1.or.&
         Coord111_DB(z_,iBlock) > zMax1.or.&
         Coord111_DB(z_,iBlock)+(nK-1)*CellSize_DB(z_,iBlock) < zMin1)&
         RETURN

    Dx = PlotDx_DI(1,iFile)
    CellSize1 = CellSize_DB(x_,iBlock)
    CellSize2 = CellSize_DB(y_,iBlock)
    CellSize3 = CellSize_DB(z_,iBlock)

    ! Calculate index limits of cells inside cut
    iMin = max(1 ,floor((xMin1-Coord111_DB(x_,iBlock))/CellSize1)+2)
    iMax = min(nI,floor((xMax1-Coord111_DB(x_,iBlock))/CellSize1)+1)

    jMin = max(1 ,floor((yMin1-ySqueezed)/CellSize2)+2)
    jMax = min(nJ,floor((yMax1-ySqueezed)/CellSize2)+1)

    kMin = max(1 ,floor((zMin1-Coord111_DB(z_,iBlock))/CellSize3)+2)
    kMax = min(nK,floor((zMax1-Coord111_DB(z_,iBlock))/CellSize3)+1)

    if(DoTest)then
       write(*,*) NameSub, 'iMin,iMax,jMin,jMax,kMin,kMax=',&
            iMin,iMax,jMin,jMax,kMin,kMax
       write(*,*) NameSub, 'CellSize1, Coord111=', &
            CellSize1, Coord111_DB(:,iBlock)
       write(*,*) NameSub, 'ySqueezed  =',ySqueezed
       write(*,*) NameSub, 'xMin1,xMax1=',xMin1,xMax1
       write(*,*) NameSub, 'yMin1,yMax1=',yMin1,yMax1
       write(*,*) NameSub, 'zMin1,zMax1=',zMin1,zMax1
    end if

    if(CellSize1 >= Dx)then
       ! Cell is equal or coarser than Dx, save all cells in cut
       do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
          x = Xyz_DGB(x_,i,j,k,iBlock)
          y = Xyz_DGB(y_,i,j,k,iBlock)
          z = Xyz_DGB(z_,i,j,k,iBlock)

          ! Check if we are inside the Cartesian box
          if(TypePlot(1:3) /= '3D_' .and. ( &
               x < xMinBox .or. x > xMaxBox .or. &
               y < yMinBox .or. y > yMaxBox .or. &
               z < zMinBox .or. z > zMaxBox)) CYCLE

          ! if plot type is bx0
          if(index(TypePlot, 'bx0') > 0) then
             ! check if bx are the same sign in this block
             if(UseB0) then
                if( all(B0_DGB(x_,i,j,k-1:k+1,iBlock) &
                     +  State_VGB(Bx_,i,j,k-1:k+1,iBlock)>0) .or. &
                     all(B0_DGB(x_,i,j,k-1:k+1,iBlock) &
                     +   State_VGB(Bx_,i,j,k-1:k+1,iBlock)<0)) CYCLE
             else
                if( all(State_VGB(Bx_,i,j,k-1:k+1,iBlock)>0) .or.&
                     all(State_VGB(Bx_,i,j,k-1:k+1,iBlock)<0)) CYCLE
             end if
             ! exclude the edge points at the plot range boundary
             if( abs(Xyz_DGB(z_,i,j,k,iBlock) - PlotRange_EI(5,iFile)) &
                  /CellSize3 <= 3 .or.&
                  abs(Xyz_DGB(z_,i,j,k,iBlock) - PlotRange_EI(6,iFile)) &
                  /CellSize3 <= 3) CYCLE
          end if

          nCell = nCell + 1
          if(DoCountOnly) CYCLE

          if(DoSaveGenCoord)then
             Coord_D = CoordMin_DB(:,iBlock) &
                  + ([i, j, k] - 0.5)*CellSize_DB(:,iBlock)
          else
             Coord_D = Xyz_DGB(:,i,j,k,iBlock)*xUnit
          end if

          Plot_V = PlotVar_GV(i,j,k,1:nPlotVar)

          call write_data(iUnit, CellSize1, xUnit, Coord_D, Plot_V, &
               nPlotVar, IsBinary, UseMpiIO, nOffset)
       end do; end do; end do
    else
       ! Block is finer then required resolution
       ! Calculate restriction factor
       nRestrict = greatest_common_divisor(nint(Dx/CellSize1), iMax-iMin+1)
       if(nDim > 1) nRestrict = greatest_common_divisor(nRestrict, jMax-jMin+1)
       if(nDim > 2) nRestrict = greatest_common_divisor(nRestrict, kMax-kMin+1)

       nRestrictX = nRestrict
       nRestrictY = 1; if(nDim > 1) nRestrictY = nRestrict
       nRestrictZ = 1; if(nDim > 2) nRestrictZ = nRestrict

       ! Calculate restricted cell size
       CellSize1 = nRestrictX*CellSize1
       CellSize2 = nRestrictY*CellSize2
       CellSize3 = nRestrictZ*CellSize3

       ! Factor for taking the average
       Restrict = 1./(nRestrict**nDim)

       if(DoTest) write(*,*) NameSub,': nRestrict, X, Y, Z, Restrict=',&
            nRestrict, nRestrictX, nRestrictY, nRestrictZ, Restrict

       ! Loop for the nRestrictX*nRestrictY*nRestrictZ bricks inside the cut
       do k = kMin, kMax, nRestrictZ
          k2 = k + nRestrictZ - 1
          do j = jMin, jMax, nRestrictY
             j2 = j + nRestrictY - 1
             do i = iMin, iMax, nRestrictX
                i2 = i + nRestrictX - 1
                x =0.5*(Xyz_DGB(x_,i,j,k,iBlock) + Xyz_DGB(x_,i2,j2,k2,iBlock))
                y =0.5*(Xyz_DGB(y_,i,j,k,iBlock) + Xyz_DGB(y_,i2,j2,k2,iBlock))
                z =0.5*(Xyz_DGB(z_,i,j,k,iBlock) + Xyz_DGB(z_,i2,j2,k2,iBlock))

                if(TypePlot(1:3) /= '3D_' .and. ( &
                     x < xMinBox .or. x > xMaxBox .or. &
                     y < yMinBox .or. y > yMaxBox .or. &
                     z < zMinBox .or. z > zMaxBox)) CYCLE

                if(DoSaveGenCoord)then
                   Coord_D = CoordMin_DB(:,iBlock) &
                        + (0.5*[i+i2,j+j2,k+k2] - 0.5)*CellSize_DB(:,iBlock)
                else
                   Coord_D = [x, y, z]*xUnit
                end if

                nCell = nCell + 1
                if(DoCountOnly) CYCLE

                do iVar=1,nPlotVar
                   Plot_V(iVar) = Restrict* &
                        sum(PlotVar_GV(i:i2,j:j2,k:k2,iVar))
                end do
                call write_data(iUnit, CellSize1, xUnit, Coord_D, Plot_V, &
                     nPlotVar, IsBinary, UseMpiIO, nOffset)
             end do
          end do
       end do
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine write_plot_idl
  !============================================================================
  subroutine write_record_mpi_binary(iUnit, nOffset, nReal, Buff_I)

    ! Write a record of integers to the file using MPI IO

    use ModMpi
    use ModKind, ONLY: nByteReal

    integer, intent(in) :: iUnit
    integer(MPI_OFFSET_KIND), intent(inout) :: nOffset
    integer, intent(in) :: nReal
    real, intent(in) :: Buff_I(nReal)

    integer :: iStatus_I(MPI_STATUS_SIZE)
    integer :: nRecord, iError

    !--------------------------------------------------------------------------
    nRecord = nReal*nByteReal
    call MPI_file_write_at(iUnit, nOffset, nRecord, 1, MPI_INTEGER, &
         iStatus_I, iError)
    nOffset = nOffset + 4

    call MPI_file_write_at(iUnit, nOffset, Buff_I, nReal, MPI_REAL, &
         iStatus_I, iError)
    nOffset = nOffset + nReal*nByteReal

    call MPI_file_write_at(iUnit, nOffset, nRecord, 1, MPI_INTEGER, &
         iStatus_I, iError)
    nOffset = nOffset + 4

  end subroutine write_record_mpi_binary
  !============================================================================
  subroutine write_record_mpi_ascii(iUnit, nOffset, nReal, Buff_I)

    ! Write a record in ASCII format using MPI IO
    ! Each value uses nWidthReal=13 characters in es13.5 format
    ! The line ends with a newline character

    use ModMpi
    use ModKind, ONLY: nByteReal

    integer, intent(in) :: iUnit
    integer(MPI_OFFSET_KIND), intent(inout) :: nOffset
    integer, intent(in) :: nReal
    real, intent(in) :: Buff_I(nReal)

    integer :: iError, i, iStart
    integer(MPI_OFFSET_KIND) :: nChar
    integer :: iStatus_I(MPI_STATUS_SIZE)
    integer(MPI_OFFSET_KIND) :: nCharOffset

    !--------------------------------------------------------------------------
    ! Format each value into the string buffer
    do i = 1, nReal
       iStart = (i-1)*nWidthReal + 1
       write(StringLine(iStart:iStart+12), '(es13.5)') Buff_I(i)
    end do

    ! New line character
    StringLine(nCharPerLine:nCharPerLine) = char(10)

    call MPI_file_write_at(iUnit, nOffset, StringLine(1:nCharPerLine), &
         nCharPerLine, MPI_CHAR, iStatus_I, iError)

    nOffset = nOffset + nCharPerLine

  end subroutine write_record_mpi_ascii
  !============================================================================
  subroutine write_data(iUnit, CellSize1, xUnit, Coord_D, Plot_V, nPlotVar, &
       IsBinary, UseMpiIO, nOffset)

    use ModMpi, ONLY: MPI_OFFSET_KIND

    integer, intent(in)              :: iUnit, nPlotVar
    real,    intent(in)              :: CellSize1, xUnit, Coord_D(3)
    real,    intent(inout)           :: Plot_V(nPlotVar)
    logical, intent(in)              :: IsBinary, UseMpiIO
    integer(MPI_OFFSET_KIND), intent(inout), optional :: nOffset

    real :: Data_I(4+nPlotVar)

    !--------------------------------------------------------------------------
    if(.not.IsBinary) then
       where(abs(Plot_V) < 1d-99) Plot_V = 0.0
    end if

    Data_I(1) = CellSize1*xUnit
    Data_I(2:4) = Coord_D
    Data_I(5:4+nPlotVar) = Plot_V

    if(UseMpiIO) then
       if(IsBinary) then
          call write_record_mpi_binary(iUnit, nOffset, size(Data_I), Data_I)
       else
          call write_record_mpi_ascii(iUnit, nOffset, size(Data_I), Data_I)
       end if
    else
       if(IsBinary) then
          write(iUnit) Data_I
       else
          write(iUnit,'(50es13.5)') Data_I
       end if
    end if

  end subroutine write_data
  !============================================================================

end module ModWritePlotIdl
!==============================================================================
