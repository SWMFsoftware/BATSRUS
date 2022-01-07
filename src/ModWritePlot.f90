!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModWritePlot

  use BATL_lib, ONLY: &
       test_start, test_stop, iProc, nProc, iComm
  use ModNumConst, ONLY: cRadToDeg
  use ModBatsrusUtility, ONLY: get_date_time, get_time_string

  implicit none

  private ! except

  public:: write_plot         ! write IDL, Tecplot, shell or box plot
  public:: adjust_plot_range  ! adjust the range of cut plots
  public:: set_plot_scalars   !
  public:: reverse_field

contains
  !============================================================================
  subroutine write_plot(iFile)

    ! Loops over all blocks per processor and write the appropriate plot files
    ! for file iFile.

    use ModMain
    use ModGeometry, ONLY: &
         XyzMin_D, XyzMax_D, Used_GB, TypeGeometry, LogRGen_I, CellSize1Min
    use ModPhysics, ONLY: No2Io_V, UnitX_, rBody, ThetaTilt, &
         set_dimensional_factor
    use ModFieldLineThread, ONLY: DoPlotThreads
    use ModIO
    use ModHdf5, ONLY: write_plot_hdf5, write_var_hdf5, init_hdf5_plot
    use ModIoUnit, ONLY: UnitTmp_, UnitTmp2_, io_unit_new
    use ModMpi
    use ModUtilities, ONLY: split_string, join_string, open_file, close_file
    use ModAdvance, ONLY : State_VGB
    use ModB0, ONLY: B0_DGB
    use ModVarIndexes, ONLY: SignB_
    use ModPlotShell, ONLY: init_plot_shell, set_plot_shell, write_plot_shell
    use ModPlotBox, ONLY: init_plot_box, set_plot_box, write_plot_box
    use ModWritePlotIdl, ONLY: write_plot_idl
    use ModWriteTecplot, ONLY: lRecConnect, nPlotDim, &
         write_tecplot_head, write_tecplot_data, write_tecplot_connect, &
         write_tecplot_node_data, set_tecplot_var_string

    use BATL_lib, ONLY: calc_error_amr_criteria, write_tree_file, &
         message_pass_node, message_pass_cell, average_grid_node, &
         find_grid_block, IsCartesianGrid, Xyz_DNB, nRoot_D, IsPeriodic_D, &
         nDim, rRound0, rRound1, SqrtNDim

    ! Arguments

    integer, intent(in) :: iFile

    ! Local variables

    integer :: iError

    integer, parameter:: lNameVar = 10

    ! Plot variables
    real :: PlotVar_GV(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxPlotvar)
    real :: PlotVarTec_GV(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxPlotvar)
    real :: PlotVarBody_V(MaxPlotvar)
    logical :: UsePlotVarBody_V(MaxPlotvar)
    real, allocatable :: PlotVarNodes_VNB(:,:,:,:,:)
    real, allocatable :: PlotXYZNodes_DNB(:,:,:,:,:)
    real, allocatable :: PlotVar_VGB(:,:,:,:,:)

    character (len=lNameVar) :: NamePlotVar_V(MaxPlotvar) = ''
    integer :: nPlotVar
    character(len=lNameVar) :: NamePlotUnit_V(MaxPlotvar)

    ! True for shell / box plots and tcp cuts
    logical:: DoPlotShell, DoPlotBox, DoPassPlotVar

    ! Equation parameters
    integer, parameter :: MaxParam = 100
    real               :: Param_I(MaxParam)
    character (len=10) :: NameParam_I(MaxParam)
    integer :: nParam

    character (Len=500)  :: NameAllVar
    character (Len=1500) :: StringUnitTec
    character (Len=500)  :: StringUnitIdl
    character (len=80)   :: NameFileNorth, NameFileSouth, NameFileHeader
    character (len=80)   :: NameSnapshot, NameProc
    character (len=20)   :: TypeForm

    logical:: IsBinary

    ! If DoSaveGenCoord is true, save generalized coordinates (e.g. r,phi,lat)
    ! In this case the coordinates are saved in normalized units (CoordUnit=1)
    ! If DoSaveGenCoord is false, save x,y,z coordinates with either normalized
    ! or I/O units.
    logical:: DoSaveGenCoord
    real   :: CoordUnit

    ! Indices and coordinates
    integer :: iBlock, i, j, k, iVar, iH5Index, iProcFound, iBlockFound
    real :: Coord1Min, Coord1Max, Coord2Min, Coord2Max, Coord3Min, Coord3Max
    real :: CellSize1, CellSize2, CellSize3

    real :: CellSizeMin_D(3)
    integer :: nCellProc, nCellBlock, nCellAll

    integer :: iTime_I(7), iDim, iParam
    integer :: iDefaultStartTime_I(7) = [2000,3,21,10,45,0,0]

    character (len=10) :: NamePlotVar

    ! Event date for NameFile
    character (len=3)  :: NameExt
    character (len=19) :: StringDateTime

    ! Parameters for saving a single 3D tecplot file (DoSaveOneTecFile = T)
    integer :: lRecData
    integer :: iUnit

    logical :: IsNotCut, DoH5Advance,IsNonCartesianPlot

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_plot'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Initialize stuff

    PlotVarBody_V = 0.0
    UsePlotVarBody_V = .false.

    StringUnitTec = ''
    StringUnitIdl = ''

    TypePlot = TypePlot_I(iFile)
    StringPlotVar = StringPlotVar_I(iFile)
    PlotRange_I = PlotRange_EI(:,iFile)

    ! DoSaveOneTecFile = T only works for 3D tecplot file right now
    DoSaveOneTecFile = DoSaveOneTecFileOrig .and. &
         (TypePlot(1:3)=='3d_' .and. TypePlotFormat_I(iFile)=='tec' &
         .or. TypePlotFormat_I(iFile)=='tcp')

    call split_string(StringPlotVar, MaxPlotvar, NamePlotVar_V, nPlotVar,    &
         UseArraySyntaxIn=.true.)

    call set_plot_scalars(iFile, MaxParam, nParam, NameParam_I, Param_I)

    call join_string(NamePlotVar_V(1:nPlotVar), NameAllVar)
    NameAllVar=trim(NameAllVar)//' '//trim(StringPlotParam_I(iFile))

    if(TypePlotFormat_I(iFile) == 'idl')then
       ! Adjust plotting range with unspecified plot resolution
       ! so that it is aligned with the current grid resolution
       if(TypePlot(1:3) == 'cut' .and. PlotDx_DI(1,iFile) <= 0.0) &
            call adjust_plot_range(CellSize1Min, PlotRange_I)

       ! Save generalized coordinates for cuts out of non-Cartesian grids
       DoSaveGenCoord = TypePlot(1:3) == 'cut' .and. .not. IsCartesianGrid
       if(DoSaveGenCoord .or. .not.IsDimensionalPlot_I(iFile))then
          CoordUnit = 1.0
       else
          CoordUnit = No2Io_V(UnitX_)
       end if
    end if

    if(DoTest) then
       write(*,*) NameSub
       write(*,*) 'iFile =', iFile
       write(*,*) 'plot_var =', StringPlotVar
       write(*,*) 'nPlotVar = ', nPlotVar
       write(*,*) 'varnames = ', NamePlotVar_V(1:nPlotVar)
       write(*,*) 'PlotDx_DI = ', PlotDx_DI(:,iFile)
       write(*,*) 'PlotRange_EI = ', PlotRange_I
       write(*,*) 'TypePlot_I =  ', TypePlot
       write(*,*) 'form =  ', TypePlotFormat_I(iFile)
       write(*,*) 'DoSaveOneTecFile =', DoSaveOneTecFile
    end if

    ! Construct the file name
    ! Plotfile names start with the plot directory and the type infor
    NameSnapshot = trim(NamePlotDir) // trim(TypePlot) // "_"

    ! add index of the plot file
    if (iFile-Plot_ < 10) then
       write(NameSnapshot, '(a,i1)') trim(NameSnapshot), iFile - Plot_
    else
       write(NameSnapshot, '(a,i2)') trim(NameSnapshot), iFile - Plot_
    end if

    if(.not.IsTimeAccurate)then
       ! Add time step information
       write(NameSnapshot,'(a,i8.8)') trim(NameSnapshot)//"_n", nStep
    else
       if(IsPlotNameE)then
          ! Event date
          call get_date_time(iTime_I)
          write(StringDateTime, &
               '(i4.4,i2.2,i2.2,"-",i2.2,i2.2,i2.2,"-",i3.3)') iTime_I
          NameSnapshot = trim(NameSnapshot) // "_e" // trim(StringDateTime)
       end if
       if(IsPlotNameT)then
          ! The file name will contain the StringDateOrTime
          call get_time_string
          NameSnapshot = trim(NameSnapshot) // "_t" // StringDateOrTime
       end if
       if(IsPlotNameN)then
          ! Add time step information
          write(NameSnapshot,'(a,i8.8)') trim(NameSnapshot)//"_n", nStep
       end if
    end if

    ! String containing the processor index and file extension
    NameExt = TypePlotFormat_I(iFile)
    if(NameExt == 'tcp') NameExt = 'tec'
    if (DoSaveOneTecFile) then
       write(NameProc, '(a)') "."//NameExt
    elseif(nProc < 10000) then
       write(NameProc, '(a,i4.4,a)') "_pe", iProc, "."//NameExt
    elseif(nProc < 100000) then
       write(NameProc, '(a,i5.5,a)') "_pe", iProc, "."//NameExt
    else
       write(NameProc, '(a,i6.6,a)') "_pe", iProc, "."//NameExt
    end if

    ! Determine if output file is formatted or unformatted
    IsBinary = DoSaveBinary .and. TypePlotFormat_I(iFile)=='idl'

    if(IsBinary)then
       TypeForm = "unformatted"
    else
       TypeForm = "formatted"
    end if

    ! Spherical slices are special cases:
    DoPlotShell = TypePlot(1:3) == 'shl'
    DoPlotBox   = TypePlot(1:3) == 'box'

    ! If threaded gap is used, the dimensional factors should be calculated,
    ! which are needed to convert a point state vector to a dimensional form
    if(DoPlotThreads .and. (DoPlotShell .or. DoPlotBox))then
       if(IsDimensionalPlot_I(iFile))then
          call set_dimensional_factor(nPlotVar, NamePlotVar_V(1:nPlotVar), &
               DimFactor_V(1:nPlotVar) , DimFactorBody_V(1:nPlotVar))
       else
          DimFactor_V = 1.0;  DimFactorBody_V = 1.0
       end if
    end if

    if(DoSaveOneTecFile) then
       iUnit = io_unit_new()
    else
       iUnit = UnitTmp_
    end if

    ! Calculate the record length for direct access data files
    ! The output format for data is ES14.6, so each cell has
    ! (nDim + nPlotVar)*14 data, plus a new line character
    lRecData = (nDim + nPlotVar)*14 + 1

    if(TypePlotFormat_I(iFile)=='tcp')then
       ! Calculate and write connectivity file
       call write_tecplot_connect(iFile, &
            trim(NameSnapshot)//"_2"//trim(NameProc))
       ! Open one file for data
       NameFileNorth = trim(NameSnapshot)//"_1"//trim(NameProc)

       if(DoSaveOneTecFile)then
          call open_file(&
               FILE=NameFileNorth, ACCESS='DIRECT', RECL=lRecData, &
               iComm=iComm, NameCaller=NameSub//'_tcp_direct_data')
       else
          if(DoSaveTecBinary) then
             call open_file(FILE=NameFileNorth, &
                  NameCaller=NameSub//'_tcp_data', &
                  access='stream', form='unformatted')
          else
             call open_file(FILE=NameFileNorth, &
                  NameCaller=NameSub//'_tcp_data')
          end if
       end if
    elseif(DoSaveOneTecFile) then
       ! NameFileHeader stores the header, NameFileNorth stores the data and
       ! NameFileSouth stores the connectivity
       NameFileHeader = trim(NameSnapshot)//"_0.tec"
       NameFileNorth = trim(NameSnapshot)//"_1.tec"
       NameFileSouth = trim(NameSnapshot)//"_2.tec"

       if(DoTest) then
          write(*,*) 'NameFileHeader =', NameFileHeader
          write(*,*) 'NameFileNorth =', NameFileNorth
          write(*,*) 'NameFileSouth =', NameFileSouth
          write(*,*) 'nPlotVar, nI, nJ, nK       =', nPlotVar, nI, nJ, nK
          write(*,*) 'lRecData, lRecConnect      =', lRecData, lRecConnect
          write(*,*) 'UnitTmp_, UnitTmp2_, iUnit =', &
               UnitTmp_, UnitTmp2_, iUnit
       end if

       ! only iProc == 0 opens the header file
       ! we should do this at the end (?)
       if(iProc == 0) call open_file(iUnit, FILE=NameFileHeader, &
            NameCaller=NameSub//'_tec_head')
       call open_file(FILE=NameFileNorth, &
            ACCESS='DIRECT', RECL=lRecData, iComm=iComm, &
            NameCaller=NameSub//'_tec_direct_data')
       call open_file(UnitTmp2_, FILE=NameFileSouth, &
            ACCESS='DIRECT', RECL=lRecConnect, iComm=iComm, &
            NameCaller=NameSub//'_tec_direct_connect')
    elseif(DoPlotBox)then
       ! Initialize the box grid for this file
       call init_plot_box(iFile, nPlotVar)
    elseif(DoPlotShell)then
       ! Initialize the shell grid for this file
       call init_plot_shell(iFile, nPlotVar)
    elseif(TypePlotFormat_I(iFile)=='tec')then
       ! Open two files for connectivity and data
       NameFileNorth = trim(NameSnapshot)//"_1"//trim(NameProc)
       NameFileSouth = trim(NameSnapshot)//"_2"//trim(NameProc)

       if(.not.DoSaveTecBinary) then
          call open_file(UnitTmp_,  FILE=NameFileNorth)
          call open_file(UnitTmp2_, FILE=NameFileSouth)
       else
          call open_file(UnitTmp_,  FILE=NameFileNorth, &
               access='stream', form='unformatted')
          call open_file(UnitTmp2_, FILE=NameFileSouth, &
               access='stream', form='unformatted')
       end if
    elseif(TypePlotFormat_I(iFile)=='hdf') then
       ! Only one plotfile will be generated, so do not include PE number
       ! in NameFile. ModHdf5 will handle opening the file.
       NameFile = trim(NameSnapshot)//".batl"
    else
       ! For IDL just open one file
       NameFile = trim(NameSnapshot)//trim(NameProc)
       call open_file(FILE=NameFile, form=TypeForm)
    end if

    IsNonCartesianPlot = .not.IsCartesianGrid

    if (DoPlotShell) IsNonCartesianPlot = .true.
    if (DoPlotBox) IsNonCartesianPlot = .false.

    ! Logical for hdf plots
    IsNotCut = TypePlot(1:3)=='3d_' .or. nDim == 1 .or. &
         (nDim==2 .and. (TypePlot(1:3) == '2d_' &
         .or.            TypePlot(1:3) == 'z=0'))

    ! START IDL
    ! initialize values used in the plotting
    Coord1Min = PlotRange_I(1)
    Coord1Max = PlotRange_I(2)
    Coord2Min = PlotRange_I(3)
    Coord2Max = PlotRange_I(4)
    Coord3Min = PlotRange_I(5)
    Coord3Max = PlotRange_I(6)

    CellSizeMin_D(:) = XyzMax_D(:) - XyzMin_D(:)

    CellSize1 = XyzMax_D(1) - XyzMin_D(1)
    CellSize2 = XyzMax_D(2) - XyzMin_D(2)
    CellSize3 = XyzMax_D(3) - XyzMin_D(3)
    nCellProc = 0
    nCellBlock = 0
    ! END IDL

    ! To plot the criteria used for AMR we need to
    ! recalulate them for the existing grid.
    do iVar = 1, nPlotVar
       NamePlotVar = NamePlotVar_V(iVar)
       if(NamePlotVar(1:4) == 'crit') then
          call calc_error_amr_criteria(nVar, State_VGB)
          EXIT
       end if
    end do

    ! plot index for hdf5 plots
    iH5Index = 1
    ! Compute the plot variables and write them to the disk
    PlotVarTec_GV=0

    ! Find the processor and block indexes for the 'blk' plot
    if(TypePlot(1:3) == 'blk') call find_grid_block( &
         PlotPointXyz_DI(:,iFile), iProcFound, iBlockFound)

    if(TypePlotFormat_I(iFile) == 'hdf') then
       call init_hdf5_plot(iFile, TypePlot(1:3), nPlotVar, &
            Coord1Min, Coord1Max, Coord2Min, Coord2Max, Coord3Min, Coord3Max, &
            CellSize1, CellSize2, CellSize3, IsNonCartesianPlot, IsNotCut)
    end if

    ! True if message passing is needed for interpolating non-primitive
    ! variables using the ghost cells.
    DoPassPlotVar = DoPlotShell .or. DoPlotBox .or. &
         TypePlotFormat_I(iFile)=='tcp' .and. nPlotDim < nDim

    if(DoPassPlotVar)then
       ! Calculate plot variables for all blocks and store them into
       ! PlotVar_VGB which will be message passed to fill in ghost cells.
       allocate(PlotVar_VGB(nPlotVar,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))

       ! Block loop stage I:
       ! copy PlotVar_GV to Plotvar_VGB and do message pass to
       ! fill in the ghost cell values
       do iBlock = 1, nBlock
          if(Unused_B(iBlock))CYCLE

          ! Use true signed magnetic field in plots
          if(SignB_>1 .and. DoThinCurrentSheet) call reverse_field(iBlock)

          call set_plotvar(iBlock, iFile - plot_, nPlotVar, NamePlotVar_V, &
               PlotVar_GV, PlotVarBody_V, UsePlotVarBody_V)

          if(IsDimensionalPlot_I(iFile)) call dimensionalize_plotvar(iBlock, &
               iFile-plot_,nPlotVar,NamePlotVar_V,PlotVar_GV,PlotVarBody_V)

          ! Copy PlotVar_GV for each block into a single array
          ! for message passing
          do iVar = 1 , nPlotVar
             PlotVar_VGB(iVar,:,:,:,iBlock) = PlotVar_GV(:,:,:,iVar)
          end do

       end do

       ! Pass plotting variables to fill ghost cell values
       call message_pass_cell(nPlotVar, PlotVar_VGB)
    end if

    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE

       if(DoPassPlotVar) then
          ! Copy precalculated plot variables including ghost cells
          do iVar = 1, nPlotVar
             PlotVar_GV(:,:,:,iVar) = PlotVar_VGB(iVar,:,:,:,iBlock)
          end do
       else
          ! Use signed magnetic field in plots
          if(SignB_>1 .and. DoThinCurrentSheet) call reverse_field(iBlock)

          ! Set plot variable for this block
          call set_plotvar(iBlock, iFile-plot_, nPlotVar, NamePlotVar_V, &
               PlotVar_GV, PlotVarBody_V, UsePlotVarBody_V)

          ! Dimensionalize plot variables
          if(IsDimensionalPlot_I(iFile)) call dimensionalize_plotvar(iBlock, &
               iFile-plot_, nPlotVar, NamePlotVar_V, PlotVar_GV, PlotVarBody_V)
       end if

       if(DoPlotShell) then
          call set_plot_shell(iBlock, nPlotVar, PlotVar_GV)
       else if (DoPlotBox) then
          call set_plot_box(iBlock, nPlotVar, PlotVar_GV)
       else
          select case(TypePlotFormat_I(iFile))
          case('tec')
             call plotvar_to_plotvarnodes
             if(TypePlot(1:3)=='blk' &
                  .and. iProc == iProcFound .and. iBlock==iBlockFound) &
                  PlotVarTec_GV = PlotVar_GV
          case('tcp')
             call write_tecplot_data(iBlock, nPlotVar, PlotVar_GV)
          case('idl')
             call write_plot_idl(iFile, iBlock, nPlotVar, PlotVar_GV, &
                  DoSaveGenCoord, CoordUnit, Coord1Min, Coord1Max, &
                  Coord2Min, Coord2Max, Coord3Min, Coord3Max, &
                  CellSize1, CellSize2, CellSize3, nCellBlock)
          case('hdf')
             call write_var_hdf5(iFile, TypePlot(1:3), iBlock, iH5Index, &
                  nPlotVar, PlotVar_GV, Coord1Min, Coord1Max, &
                  Coord2Min, Coord2Max, Coord3Min, Coord3Max, &
                  CellSize1, CellSize2, CellSize3, IsNonCartesianPlot, &
                  IsNotCut, nCellBlock, DoH5Advance)
             if (DoH5Advance) iH5Index = iH5Index + 1
          end select
       end if

       if (TypePlotFormat_I(iFile) == 'idl' .and. &
            .not.DoPlotShell .and. .not.DoPlotBox) then
          ! Update number of cells per processor
          nCellProc = nCellProc + nCellBlock

          ! Find smallest cell size in the plotting region
          CellSizeMin_D(1) = min(CellSizeMin_D(1), CellSize1)
          CellSizeMin_D(2) = min(CellSizeMin_D(2), CellSize2)
          CellSizeMin_D(3) = min(CellSizeMin_D(3), CellSize3)
       end if

       if(SignB_ > 1 .and. DoThinCurrentSheet) call reverse_field(iBlock)

    end do ! Block loop stage i2

    if(allocated(PlotVar_VGB)) deallocate(PlotVar_VGB)

    ! Write the HDF5 output file and return
    select case(TypePlotFormat_I(iFile))
    case('hdf')

       call get_idl_units(iFile, nPlotVar,NamePlotVar_V, NamePlotUnit_V, &
            StringUnitIdl)
       call write_plot_hdf5(NameFile, TypePlot(1:3), NamePlotVar_V, &
            NamePlotUnit_V, nPlotVar, IsNotCut, IsNonCartesianPlot, &
            .false., IsDimensionalPlot_I(iFile), &
            Coord1Min, Coord1Max, Coord2Min, Coord2Max, Coord3Min, Coord3Max)

       RETURN
    case('tec','tcp')
       call set_tecplot_var_string(iFile, nPlotVar, NamePlotVar_V, &
            StringUnitTec)
       if(DoTest .and. iProc==0) write(*,*) NameSub,' StringUnitTec:', &
            trim(StringUnitTec)
       if(DoPlotShell) call write_plot_shell(iFile, nPlotVar, &
            NamePlotVar_V, StringUnitTec, trim(NameSnapshot)//'.dat')
       if(DoPlotBox) call write_plot_box(iFile, nPlotVar, &
            NamePlotVar_V, StringUnitTec, trim(NameSnapshot)//'.dat')
    case('idl')
       call get_idl_units(iFile, nPlotVar, NamePlotVar_V, NamePlotUnit_V, &
            StringUnitIdl)
       if(DoTest .and. iProc==0) write(*,*) StringUnitIdl
       if(DoPlotShell) call write_plot_shell(iFile, nPlotVar, &
            NamePlotVar_V, StringUnitIdl, trim(NameSnapshot)//'.out')
       if(DoPlotBox) call write_plot_box(iFile, nPlotVar, &
            NamePlotVar_V, StringUnitIdl, trim(NameSnapshot)//'.out')
    end select

    ! Done writing shell plot
    if(DoPlotShell) RETURN
    if(DoPlotBox) RETURN

    ! Write files for tecplot format
    if(TypePlotFormat_I(iFile)=='tec')then

       if(.not.allocated(PlotVarNodes_VNB)) then
          allocate(PlotVarNodes_VNB(MaxPlotvar,nI+1,nJ+1,nK+1,nBlock))
          PlotVarNodes_VNB = 0.0
       end if

       ! Pass and average the plot variables
       ! Do not average at pseudo-periodic boundaries
       call message_pass_node(MaxPlotvar, PlotVarNodes_VNB, &
            NameOperatorIn='Mean', UsePeriodicCoordIn = .not.IsCartesianGrid)

       do iBlock = 1, nBlock; if(Unused_B(iBlock)) CYCLE
          call average_grid_node(iBlock, MaxPlotvar, &
               PlotVarNodes_VNB(:,:,:,:,iBlock))
       end do

       if(IsCartesianGrid)then
          call write_tecplot_node_data(&
               iFile, nPlotVar, PlotVarTec_GV, PlotVarNodes_VNB, &
               Xyz_DNB, StringUnitTec, Coord1Min, Coord1Max, &
               Coord2Min, Coord2Max, Coord3Min, Coord3Max, iUnit)
       else
          ! Fix "hanging" nodes so they lie precisely on the same plane
          ! as "non-hanging" nodes. This is needed for non-Cartesian grids.

          allocate(PlotXYZNodes_DNB(3,nINode,nJNode,nKNode,nBlock))
          PlotXYZNodes_DNB(:,:,:,:,1:nBlock) = Xyz_DNB(:,:,:,:,1:nBlock)

          do iBlock = 1, nBlock; if(Unused_B(iBlock)) CYCLE
             ! Fixing hanging nodes at resolution change
             call  average_grid_node(iBlock, 3, &
                  PlotXYZNodes_DNB(:,:,:,:,iBlock))
             ! Make near zero values exactly zero
             where(abs(PlotXYZNodes_DNB(:,:,:,:,iBlock)) < 1e-10) &
                  PlotXYZNodes_DNB(:,:,:,:,iBlock) = 0.
          end do
          call write_tecplot_node_data(iFile, nPlotVar, PlotVarTec_GV, &
               PlotVarNodes_VNB, PlotXYZNodes_DNB, &
               StringUnitTec, Coord1Min, Coord1Max, Coord2Min, Coord2Max, &
               Coord3Min, Coord3Max, iUnit)

          deallocate(PlotXYZNodes_DNB)
       end if

       deallocate(PlotVarNodes_VNB)
    end if

    if(TypePlotFormat_I(iFile) == 'idl' .and. nCellProc == 0) then
       call close_file(status = 'DELETE')
    else
       call close_file
    end if

    ! Write out header file for tcp format
    if(TypePlotFormat_I(iFile)=='tcp') &
         call write_tecplot_head(trim(NameSnapshot)//"_0.tec", StringUnitTec)

    if(TypePlotFormat_I(iFile)=='tec') call close_file(UnitTmp2_)

    if(DoSaveOneTecFile) call close_file(iUnit)

    ! START IDL
    if (TypePlotFormat_I(iFile)=='idl')then
       ! Find smallest cell size and total number of cells
       nCellAll = nCellProc
       if(nProc > 1)then
          call MPI_reduce_real_array(CellSizeMin_D, 3, MPI_MIN, 0, &
               iComm, iError)
          call MPI_reduce_integer_scalar(nCellAll, MPI_SUM, 0, &
               iComm, iError)
       end if

       if(DoTest) write(*,*)NameSub,' DxyzMin_D, nCellProc=', &
            CellSizeMin_D, nCellProc
    end if
    ! END IDL

    ! write header file
    if(iProc==0)then

       select case(TypePlotFormat_I(iFile))
       case('tec','tcp')
          NameFile = trim(NameSnapshot) // ".T"
       case('idl')
          NameFile = trim(NameSnapshot) // ".h"
       end select

       call open_file(FILE=NameFile)

       select case(TypePlotFormat_I(iFile))
       case('tec','tcp')
          write(UnitTmp_,'(a)')NameFile
          write(UnitTmp_,'(i8,a)')nProc,' nProc'
          write(UnitTmp_,'(i8,a)')nStep,' nStep'
          write(UnitTmp_,'(1pe18.10,a)')tSimulation,' t'
          write(UnitTmp_,'(a)')trim(StringUnitTec)
          call get_date_time(iTime_I)
          write(UnitTmp_,*) iTime_I(1:7),' year mo dy hr mn sc msc'
          write(UnitTmp_,'(2(1pe13.5),a)') thetaTilt*cRadToDeg, 0.0,  &
               ' thetatilt[deg] phitilt[deg]'
       case('idl')
          write(UnitTmp_,'(a)') '#HEADFILE'
          write(UnitTmp_,'(a)') NameFile
          write(UnitTmp_,'(i8,16x,a)') nProc, 'nProc'
          write(UnitTmp_,'(l8,16x,a)') IsBinary, 'IsBinary'
          if(IsBinary) &
               write(UnitTmp_,'(i8,16x,a)')nByteReal, 'nByteReal'
          write(UnitTmp_,*)

          write(UnitTmp_,'(a)') '#NDIM'
          write(UnitTmp_,'(i8,16x,a)') nDim, 'nDim'
          write(UnitTmp_,*)

          write(UnitTmp_,'(a)') '#GRIDBLOCKSIZE'
          do iDim = 1, nDim
             write(UnitTmp_,'(i8,16x,a,i1)') nIJK_D(iDim), 'BlockSize', iDim
          enddo
          write(UnitTmp_,*)

          write(UnitTmp_,'(a)') '#ROOTBLOCK'
          do iDim = 1, nDim
             write(UnitTmp_,'(i8,16x,a,i1)') nRoot_D(iDim), 'nRootBlock', iDim
          enddo
          write(UnitTmp_,*)

          write(UnitTmp_,'(a)') '#GRIDGEOMETRYLIMIT'
          write(UnitTmp_,'(a,4x,a)') TypeGeometry, 'TypeGeometry'
          if(index(TypeGeometry,'genr') > 0)then
             write(UnitTmp_,'(i8,16x,a)') size(LogRGen_I), 'nRgen'
             write(UnitTmp_,'(es13.5,"           LogRgen")') LogRGen_I
          end if
          if(TypeGeometry == 'roundcube')then
             write(UnitTmp_,'(es13.5,11x,a)') rRound0,  'rRound0'
             write(UnitTmp_,'(es13.5,11x,a)') rRound1,  'rRound1'
             write(UnitTmp_,'(es13.5,11x,a)') SqrtNDim, 'SqrtNDim'
          end if
          do iDim = 1, nDim
             write(UnitTmp_,'(es13.5,11x,a,i1)') XyzMin_D(iDim),'XyzMin',iDim
             write(UnitTmp_,'(es13.5,11x,a,i1)') XyzMax_D(iDim),'XyzMax',iDim
          enddo
          write(UnitTmp_,*)

          write(UnitTmp_,'(a)') '#PERIODIC'
          do iDim = 1, nDim
             write(UnitTmp_,'(l8,16x,a,i1)') &
                  IsPeriodic_D(iDim), 'IsPeriodic', iDim
          enddo
          write(UnitTmp_,*)

          write(UnitTmp_,'(a)') '#NSTEP'
          write(UnitTmp_,'(i8,16x,a)')nStep, 'nStep'
          write(UnitTmp_,*)

          write(UnitTmp_,'(a)') '#TIMESIMULATION'
          write(UnitTmp_,'(1pe18.10,6x,a)')tSimulation, 'TimeSimulation'
          write(UnitTmp_,*)

          write(UnitTmp_,'(a)') '#NCELL'
          write(UnitTmp_,'(i10,14x,a)') nCellAll, 'nCellPlot'
          write(UnitTmp_,*)

          write(UnitTmp_,'(a)') '#CELLSIZE'
          do iDim = 1, nDim
             write(UnitTmp_,'(1pe18.10,6x,a,i1)') &
                  CellSizeMin_D(iDim)*CoordUnit, 'CellSizeMin',iDim
          enddo
          write(UnitTmp_,*)

          write(UnitTmp_,'(a)') '#PLOTRANGE'
          do iDim = 1, nDim
             write(UnitTmp_,'(1pe18.10,6x,a,i1)') &
                  PlotRange_I(2*iDim-1)*CoordUnit, 'CoordMin', iDim
             write(UnitTmp_,'(1pe18.10,6x,a,i1)') &
                  PlotRange_I(2*iDim)*CoordUnit, 'CoordMax', iDim
          enddo
          write(UnitTmp_,*)

          write(UnitTmp_,'(a)') '#PLOTRESOLUTION'
          do iDim = 1, nDim
             write(UnitTmp_,'(1pe18.10,6x,a,i1)') &
                  PlotDx_DI(iDim,iFile)*CoordUnit, 'DxSavePlot', iDim
          enddo
          write(UnitTmp_,*)

          write(UnitTmp_,'(a)') '#SCALARPARAM'
          write(UnitTmp_,'(i8,16x,a)') nParam, 'nParam'
          do iParam = 1, nParam
             write(UnitTmp_,'(es13.5,11x,a)') &
                  Param_I(iParam), NameParam_I(iParam)
          enddo
          write(UnitTmp_,*)

          write(UnitTmp_,'(a)') '#PLOTVARIABLE'
          write(UnitTmp_,'(i8,16x,a)') nPlotVar, 'nPlotVar'
          write(UnitTmp_,'(a)')trim(NameAllVar)
          if(TypeFile_I(iFile) == 'tec' .or. TypeFile_I(iFile) == 'tcp')then
             call set_tecplot_var_string(iFile, nPlotVar, NamePlotVar_V, &
                  StringUnitTec)
             write(UnitTmp_,'(a)')trim(StringUnitTec)
          elseif( StringUnitIdl == '') then
             write(UnitTmp_,'(a)')'normalized units'
          elseif(all(iDefaultStartTime_I == iStartTime_I))then
             write(UnitTmp_,'(a)')trim(StringUnitIdl)
          else
             call get_date_time(iTime_I)
             write(StringDateTime, &
                  '(i4.4,"-",i2.2,"-",i2.2,"T",i2.2,":",i2.2,":",i2.2)')&
                  iTime_I(1:6)
             write(UnitTmp_,'(a)')StringDateTime//'; '//trim(StringUnitIdl)
          endif
          write(UnitTmp_,*)

          write(UnitTmp_,'(a)') '#OUTPUTFORMAT'
          write(UnitTmp_,'(a)')TypeFile_I(iFile)
          write(UnitTmp_,*)

       end select
       call close_file

    end if

    ! Save tree information for nDim dimensional IDL file
    if(TypePlotFormat_I(iFile) == 'idl' .and.               &
         (    TypePlot(1:3) == '3d_'               &
         .or. TypePlot(1:3) == '2d_' .and. nDim<=2 &
         .or. TypePlot(1:3) == '1d_' .and. nDim==1 ) )then
       NameFile = trim(NameSnapshot)//'.tree'
       call write_tree_file(NameFile)
    end if

    if(DoTest)write(*,*) NameSub,' finished'

    call test_stop(NameSub, DoTest)
  contains
    !==========================================================================
    subroutine plotvar_to_plotvarnodes

      integer :: i2,j2,k2
      integer :: nCell_NV(nI+1,nJ+1,nK+1,MaxPlotvar)
      real    :: PlotVar_NV(nI+1,nJ+1,nK+1,MaxPlotvar)
      real    :: r2, r2Min
      !------------------------------------------------------------------------
      if(.not.allocated(PlotVarNodes_VNB)) then
         allocate(PlotVarNodes_VNB(MaxPlotvar,nI+1,nJ+1,nK+1,nBlock))
         PlotVarNodes_VNB = 0.0
      end if

      ! Initialize values
      nCell_NV = 0; PlotVar_NV = 0.0

      ! Add physical cell values in neighboring nodes (ignore ghost cells).
      ! Count the number of cells contributing to the node value.
      ! Then message_pass_node will do the averaging at block boundaries.
      do k=1,nK; do j=1,nJ; do i=1,nI  ! Cell loop
         do iVar = 1, nPlotVar
            if ( Used_GB(i,j,k,iBlock) .or. UsePlotVarBody_V(iVar) )then
               do k2=0,1; do j2=0,1; do i2=0,1
                  nCell_NV(i+i2,j+j2,k+k2,iVar) = &
                       nCell_NV(i+i2,j+j2,k+k2,iVar) + 1
                  PlotVar_NV(i+i2,j+j2,k+k2,iVar) = &
                       PlotVar_NV(i+i2,j+j2,k+k2,iVar) + PlotVar_GV(i,j,k,iVar)
               end do; end do; end do
            end if
         end do
      end do; end do; end do

      if(UseBody) r2Min = (0.51*min(1.0, Rbody))**2

      ! Store PlotVar_NV (per block info) into PlotVarNodes_VNB
      do k=1,nK+1; do j=1,nJ+1; do i=1,nI+1  ! Node loop

         if(UseBody) r2 = sum(Xyz_DNB(:,i,j,k,iBlock)**2)

         do iVar = 1, nPlotVar
            if (nCell_NV(i,j,k,iVar) > 0) then
               PlotVarNodes_VNB(iVar,i,j,k,iBlock) = &
                    PlotVar_NV(i,j,k,iVar)/nCell_NV(i,j,k,iVar)

               ! This will zero out values otherwise true with UsePlotVarBody_V
               ! The intent of UsePlotVarBody_V is to fill nodes inside of the
               ! body with values for plotting. However, when allowed to go all
               ! the way to the origin, B traces will continuously loop through
               ! the body and out. Setting the values to 0 inside 0.51 fixes it
               if(UsePlotVarBody_V(iVar) .and. UseBody)then
                  if(r2 < r2Min) PlotVarNodes_VNB(iVar,i,j,k,iBlock) = 0.0
               end if
            else
               PlotVarNodes_VNB(iVar,i,j,k,iBlock) = PlotVarBody_V(iVar)
            end if
         end do
      end do; end do; end do

    end subroutine plotvar_to_plotvarnodes
    !==========================================================================

  end subroutine write_plot
  !============================================================================

  subroutine set_plot_scalars(iFile, MaxParam, nParam, NameParam_I, Param_I)

    ! For file iPlotFile set Param_I based on NameParam_I
    ! Extend array of scalar parameters with useful information

    use ModPhysics, ONLY: Gamma, Gamma_I, GammaElectron, &
         cLight, rBody, ThetaTilt, &
         No2Io_V, No2Si_V, Io2Si_V, UnitX_, UnitT_, UnitU_, UnitRho_
    use ModFieldTrace, ONLY: rTrace
    use ModResistivity, ONLY: Eta0Si
    use ModIO
    use ModMain, ONLY: dt
    use ModMultiFluid, ONLY: nFluid, nIonFluid, IonFirst_, &
         MassFluid_I, ChargeIon_I
    use BATL_lib, ONLY: nRoot_D, nI, nJ, nK
    use ModUtilities, ONLY: split_string, lower_case

    integer,           intent(in) :: iFile
    integer,           intent(in) :: MaxParam
    integer,           intent(out):: nParam
    character(len=10), intent(out):: NameParam_I(MaxParam)
    real,              intent(out):: Param_I(MaxParam)

    character(len=500):: NameParam
    integer :: iPar
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_plot_scalars'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    NameParam = StringPlotParam_I(iFile)

    call lower_case(NameParam)
    call split_string(NameParam, MaxParam, NameParam_I, nParam, &
         UseArraySyntaxIn=.true.)

    do iPar = 1, nParam
       select case(NameParam_I(iPar))
       case('g', 'g1', 'gamma')
          Param_I(iPar) = Gamma
       case('g2')
          Param_I(iPar) = Gamma_I(min(IonFirst_+1,nFluid))
       case('g3')
          Param_I(iPar) = Gamma_I(min(IonFirst_+2,nFluid))
       case('g4')
          Param_I(iPar) = Gamma_I(min(IonFirst_+3,nFluid))
       case('g5')
          Param_I(iPar) = Gamma_I(min(IonFirst_+4,nFluid))
       case('g6')
          Param_I(iPar) = Gamma_I(min(IonFirst_+5,nFluid))
       case('g7')
          Param_I(iPar) = Gamma_I(min(IonFirst_+6,nFluid))
       case('g8')
          Param_I(iPar) = Gamma_I(min(IonFirst_+7,nFluid))
       case('g9')
          Param_I(iPar) = Gamma_I(min(IonFirst_+8,nFluid))
       case('ge')
          Param_I(iPar) = GammaElectron
       case('c','clight')
          if(IsDimensionalPlot_I(iFile)) then
             Param_I(iPar) = Clight*No2Io_V(UnitU_)
          else
             Param_I(iPar) = Clight
          end if
       case('r','rbody')
          Param_I(iPar) = rBody
          if(IsDimensionalPlot_I(iFile)) &
               Param_I(iPar) = Param_I(iPar)*No2Io_V(UnitX_)
          ! BEGIN CCMC REQUESTED PARAMETERS to describe block structure
       case('p1')
          Param_I(iPar) = nRoot_D(1)
       case('p2')
          Param_I(iPar) = nRoot_D(2)
       case('p3')
          Param_I(iPar) = nRoot_D(3)
       case('nx')
          Param_I(iPar) = nI
       case('ny')
          Param_I(iPar) = nJ
       case('nz')
          Param_I(iPar) = nK
       case('th')
          ! CCMC needs the dipole tilt in radians
          Param_I(iPar) = ThetaTilt
          ! END OF CCMC requested parameters
       case('tilt')
          Param_I(iPar) = ThetaTilt*cRadToDeg
       case('eta')
          Param_I(iPar) = Eta0Si
       case('mu')
          Param_I(iPar) = MuLimbDarkening
       case('obsx')
          Param_I(iPar) = ObsPos_DI(1,iFile)
       case('obsy')
          Param_I(iPar) = ObsPos_DI(2,iFile)
       case('obsz')
          Param_I(iPar) = ObsPos_DI(3,iFile)
       case('rTrace')
          Param_I(iPar) = rTrace
       case('dt')
          if(IsDimensionalPlot_I(iFile))then
             Param_I(iPar) = dt*No2Io_V(UnitT_)
          else
             Param_I(iPar) = dt
          end if
       case('xsi')
          if(IsDimensionalPlot_I(iFile))then
             Param_I(iPar) = Io2Si_V(UnitX_)
          else
             Param_I(iPar) = No2Si_V(UnitX_)
          end if
       case('tsi')
          if(IsDimensionalPlot_I(iFile))then
             Param_I(iPar) = Io2Si_V(UnitT_)
          else
             Param_I(iPar) = No2Si_V(UnitT_)
          end if
       case('usi')
          if(IsDimensionalPlot_I(iFile))then
             Param_I(iPar) = Io2Si_V(UnitU_)
          else
             Param_I(iPar) = No2Si_V(UnitU_)
          end if
       case('rhosi')
          if(IsDimensionalPlot_I(iFile))then
             Param_I(iPar) = Io2Si_V(UnitRho_)
          else
             Param_I(iPar) = No2Si_V(UnitRho_)
          end if
       case('mi','m1')
          Param_I(iPar) = MassFluid_I(IonFirst_)
       case('m2')
          Param_I(iPar) = MassFluid_I(min(IonFirst_+1,nFluid))
       case('m3')
          Param_I(iPar) = MassFluid_I(min(IonFirst_+2,nFluid))
       case('m4')
          Param_I(iPar) = MassFluid_I(min(IonFirst_+3,nFluid))
       case('m5')
          Param_I(iPar) = MassFluid_I(min(IonFirst_+4,nFluid))
       case('m6')
          Param_I(iPar) = MassFluid_I(min(IonFirst_+5,nFluid))
       case('m7')
          Param_I(iPar) = MassFluid_I(min(IonFirst_+6,nFluid))
       case('m8')
          Param_I(iPar) = MassFluid_I(min(IonFirst_+7,nFluid))
       case('m9')
          Param_I(iPar) = MassFluid_I(min(IonFirst_+8,nFluid))
       case('me')
          Param_I(iPar) = MassFluid_I(nIonFluid)
       case('q1')
          Param_I(iPar) = ChargeIon_I(1)
       case('q2')
          Param_I(iPar) = ChargeIon_I(min(2,nIonFluid))
       case('q3')
          Param_I(iPar) = ChargeIon_I(min(3,nIonFluid))
       case('q4')
          Param_I(iPar) = ChargeIon_I(min(4,nIonFluid))
       case('q5')
          Param_I(iPar) = ChargeIon_I(min(5,nIonFluid))
       case('q6')
          Param_I(iPar) = ChargeIon_I(min(6,nIonFluid))
       case('q7')
          Param_I(iPar) = ChargeIon_I(min(7,nIonFluid))
       case('q8')
          Param_I(iPar) = ChargeIon_I(min(8,nIonFluid))
       case('q9')
          Param_I(iPar) = ChargeIon_I(min(9,nIonFluid))
       case('qe')
          Param_I(iPar) = ChargeIon_I(nIonFluid)
       case default
          Param_I(iPar) = -7777.
          if(iProc==0)write(*,*) NameSub, ' Error: unknown parameter name=',&
               NameParam_I(iPar),' for iFile=',iFile
       end select
    end do

    call test_stop(NameSub, DoTest)
  end subroutine set_plot_scalars
  !============================================================================
  subroutine set_plotvar(iBlock, iPlotFile, nPlotVar, NamePlotVar_V, &
       PlotVar_GV, PlotVarBody_V, UsePlotVarBody_V)

    use ModMain
    use ModVarIndexes
    use ModAdvance, ONLY : DtMax_CB, State_VGB, DivB1_GB, &
         ExNum_CB, EyNum_CB, EzNum_CB, iTypeAdvance_B, UseElectronPressure, &
         UseMultiSpecies, LowOrderCrit_XB, LowOrderCrit_YB, LowOrderCrit_ZB
    use ModConservative, ONLY: IsConserv_CB, UseNonconservative
    use ModLoadBalance, ONLY: iTypeBalance_A
    use ModB0, ONLY: B0_DGB
    use ModGeometry
    use ModBoundaryGeometry, ONLY: iBoundary_GB
    use ModPhysics, ONLY: BodyRho_I, BodyP_I, OmegaBody, FaceState_VI, &
         ElectronPressureRatio, RhoBody2, pBody2, rBody2
    use ModConstrainDivB, ONLY: BxFace_GB, ByFace_GB, BzFace_GB
    use ModFieldTrace, ONLY: Trace_DSNB
    use ModUtilities, ONLY: lower_case
    use ModIO, ONLY: NameVarUserTec_I, NameUnitUserTec_I, NameUnitUserIdl_I, &
         IsDimensionalPlot_I, Plot_
    use ModHallResist, ONLY: UseHallResist, &
         set_hall_factor_cell, HallFactor_C, IsHallBlock
    use ModResistivity, ONLY: Eta_GB, Eta0
    use ModFaceGradient, ONLY: get_face_curl
    use ModCellGradient, ONLY: calc_divergence, calc_gradient
    use ModPointImplicit, ONLY: UsePointImplicit, UseUserPointImplicit_B
    use ModMultiFluid, ONLY: extract_fluid_name,   &
         UseMultiIon, nIonFluid, MassIon_I, iPpar, &
         IsMhd, iRho, iRhoUx, iRhoUy, iRhoUz, iP, iRhoIon_I, &
         ChargeIon_I
    use ModWaves, ONLY: UseWavePressure
    use ModLaserHeating, ONLY: LaserHeating_CB
    use ModCurrent, ONLY: get_current
    use ModEnergy, ONLY: get_fluid_energy_block
    use ModElectricField, ONLY: Efield_DGB, DivE_CB, Potential_GB, &
         Epot_DGB, Eind_DGB, &
         get_electric_field_block, get_electric_field, &
         calc_div_e, calc_inductive_e
    use ModCoordTransform, ONLY: cross_product
    use ModViscosity, ONLY: UseViscosity, set_visco_factor_cell, ViscoFactor_C
    use ModFaceValue, ONLY: iRegionLowOrder_I
    use ModPIC, ONLY: i_status_pic_region, i_status_pic_active, &
         i_status_pic_criteria, iStatusPicCrit_CB, calc_crit_entropy,&
         calc_crit_jb, calc_crit_jbperp, CriteriaB1, DivCurvature_CB
    use ModBorisCorrection, ONLY: set_clight_cell, Clight_G
    use BATL_lib, ONLY: block_inside_regions, iTree_IA, Level_, iNode_B, &
         iTimeLevel_A, AmrCrit_IB, nAmrCrit, &
         Xyz_DGB, iNode_B, CellSize_DB, CellVolume_GB

    use ModUserInterface ! user_set_plot_var

    integer, intent(in) :: iBlock,iPlotFile,nPlotVar
    character (LEN=10), intent(in) :: NamePlotVar_V(nPlotVar)
    real, intent(out)   :: PlotVar_GV(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,nPlotVar)
    real, intent(out)   :: PlotVarBody_V(nPlotVar)
    logical, intent(out):: UsePlotVarBody_V(nPlotVar)

    character (len=10)  :: String, NamePlotVar

    real:: FullB2, FullBRhoU
    real, allocatable :: Current_DC(:,:,:,:)
    real, allocatable :: GradPe_DG(:,:,:,:), Var_G(:,:,:), u_DG(:,:,:,:)

    integer :: iVar, i3, j3, jVar, iIon, iFluid
    integer :: i,j,k

    integer:: iDir, nShiftI, nShiftJ, nShiftK
    real:: Current_D(3)
    real:: FullB_DG(3,MinI:MaxI,MinJ:MaxJ,MinK:MaxK)

    logical :: IsFound

    ! ModCurrent with get_current calculate jx,jy and jz at the same time,
    ! but we write them separately.
    ! DoCurrent used to make sure we only calculate the currents once per block
    logical :: DoCurrent

    ! Passed to and set by get_face_curl
    logical:: IsNewBlockCurrent

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_plotvar'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    if(.not.UseB)then
       FullB_DG = 0.00
    elseif(UseB0)then
       FullB_DG(:,:,:,:) = State_VGB(Bx_:Bz_,:,:,:,iBlock) &
            + B0_DGB(:,:,:,:,iBlock)
    else
       FullB_DG = State_VGB(Bx_:Bz_,:,:,:,iBlock)
    end if

    ! Calculate current if needed
    DoCurrent = .true.

    ! Recalculate magnetic field in block for face currents (if needed)
    IsNewBlockCurrent = .true.

    ! Set default value to zero
    PlotVar_GV = 0.0

    do iVar = 1, nPlotVar
       NamePlotVar = NamePlotVar_V(iVar)

       ! Default values for TecPlot variable name
       ! and TecPlot and IDL unit names
       NameVarUserTec_I(iVar)  = NamePlotVar
       NameUnitUserTec_I(iVar) = ' '
       NameUnitUserIdl_I(iVar) = '?'

       call lower_case(NamePlotVar)
       String = NamePlotVar
       call extract_fluid_name(String,iFluid)

       ! Set PlotVarBody_V to something reasonable for inside the body.
       ! Load zeros (0) for most values - load something better for
       ! rho, p, and T. We know that U, B, J are okay with zeroes,
       ! others should be changed if necessary.
       ! Note that all variables not set to 0 should be loaded below.
       ! Note that this is used for tecplot corner extrapolation only.
       PlotVarBody_V(iVar) = 0.0

       ! Set UsePlotVarBody_V to false unless cell values inside of the body
       ! are to be used for plotting.
       UsePlotVarBody_V(iVar) = .false.

       select case(String)

          ! Cartesian coordinates for non-Cartesian plots
       case('x')
          PlotVar_GV(:,:,:,iVar) = Xyz_DGB(1,:,:,:,iBlock)
       case('y')
          PlotVar_GV(:,:,:,iVar) = Xyz_DGB(2,:,:,:,iBlock)
       case('z')
          PlotVar_GV(:,:,:,iVar) = Xyz_DGB(3,:,:,:,iBlock)
       case('r')
          PlotVar_GV(:,:,:,iVar) = r_GB(:,:,:,iBlock)

          ! BASIC MHD variables
       case('rho')
          PlotVar_GV(:,:,:,iVar) = State_VGB(iRho,:,:,:,iBlock)
          PlotVarBody_V(iVar) = BodyRho_I(iFluid)
          ! If Body2 is used, set Rho=RhoBody2 inside it
          if(UseBody2)then
             if(rMinBody2_B(iBlock) < rBody2) PlotVarBody_V(iVar) = RhoBody2
          end if
       case('rhoux','mx')
          if (UseRotatingFrame) then
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                PlotVar_GV(i,j,k,iVar) = State_VGB(iRhoUx,i,j,k,iBlock) &
                     - State_VGB(iRho,i,j,k,iBlock) &
                     *OmegaBody*Xyz_DGB(y_,i,j,k,iBlock)
             end do; end do; end do
          else
             PlotVar_GV(:,:,:,iVar) = State_VGB(iRhoUx,:,:,:,iBlock)
          end if
       case('rhouy','my')
          if (UseRotatingFrame) then
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                PlotVar_GV(i,j,k,iVar) = State_VGB(iRhoUy,i,j,k,iBlock) &
                     +State_VGB(iRho,i,j,k,iBlock) &
                     *OmegaBody*Xyz_DGB(x_,i,j,k,iBlock)
             end do; end do; end do
          else
             PlotVar_GV(:,:,:,iVar) = State_VGB(iRhoUy,:,:,:,iBlock)
          end if
       case('rhouz','mz')
          PlotVar_GV(:,:,:,iVar) = State_VGB(iRhoUz,:,:,:,iBlock)
       case('bx')
          UsePlotVarBody_V(iVar) = NameThisComp/='SC'
          PlotVar_GV(:,:,:,iVar) = FullB_DG(x_,:,:,:)
       case('by')
          UsePlotVarBody_V(iVar) = NameThisComp/='SC'
          PlotVar_GV(:,:,:,iVar) = FullB_DG(y_,:,:,:)
       case('bz')
          UsePlotVarBody_V(iVar) = NameThisComp/='SC'
          PlotVar_GV(:,:,:,iVar) = FullB_DG(z_,:,:,:)
       case('bxl')
          PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = BxFace_GB(1:nI,1:nJ,1:nK,iBlock)
       case('bxr')
          PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = BxFace_GB(2:nI+1,1:nJ,1:nK,iBlock)
       case('byl')
          PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = ByFace_GB(1:nI,1:nJ,1:nK,iBlock)
       case('byr')
          PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = ByFace_GB(1:nI,2:nJ+1,1:nK,iBlock)
       case('bzl')
          PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = BzFace_GB(1:nI,1:nJ,1:nK,iBlock)
       case('bzr')
          PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = BzFace_GB(1:nI,1:nJ,2:nK+1,iBlock)
          !
       case('e')
          call get_fluid_energy_block(iBlock, iFluid, PlotVar_GV(:,:,:,iVar))
          ! Add 0.5*( (B0+B1)^2 - B1^2 ) so the energy contains B0
          if(iFluid == 1 .and. IsMhd .and. UseB0)then
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                PlotVar_GV(i,j,k,iVar) = PlotVar_GV(i,j,k,iVar) + 0.5*( &
                     sum(( State_VGB(Bx_:Bz_,i,j,k,iBlock)      &
                     +     B0_DGB(:,i,j,k,iBlock))**2      ) -  &
                     sum(State_VGB(Bx_:Bz_,i,j,k,iBlock)**2)    )
             end do; end do; end do
          end if
       case('p','pth')
          PlotVar_GV(:,:,:,iVar) = State_VGB(iP,:,:,:,iBlock)
          PlotVarBody_V(iVar) = BodyP_I(iFluid)
          ! If Body2 is used, set p=pBody2 inside it
          if(UseBody2)then
             if(rMinBody2_B(iBlock) < rBody2) PlotVarBody_V(iVar) = pBody2
          end if

          ! EXTRA MHD variables
       case('eta')
          if(allocated(Eta_GB))then
             PlotVar_GV(:,:,:,iVar) = Eta_GB(:,:,:,iBlock)
          else
             PlotVar_GV(:,:,:,iVar) = Eta0
          end if
       case('n','t','temp')
          ! Calculate the number density
          if(UseMultiSpecies)then
             do jVar = SpeciesFirst_, SpeciesLast_
                PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) + &
                     State_VGB(jVar,:,:,:,iBlock)/MassSpecies_V(jVar)
             end do
          else if(iFluid == 1 .and. UseMultiIon)then
             ! Add up ion number densities
             do iIon = 1, nIonFluid
                PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) + &
                     State_VGB(iRhoIon_I(iIon),:,:,:,iBlock)/MassIon_I(iIon)
             end do
          else
             PlotVar_GV(:,:,:,iVar) &
                  = State_VGB(iRho,:,:,:,iBlock)/MassFluid_I(iFluid)
          end if

          ! Calculate temperature from P= n*k*T+ne*k*Te = n*k*T*(1+ne/n*Te/T)
          if(String /= 'n')then
             ! t = p/n
             PlotVar_GV(:,:,:,iVar) = &
                  State_VGB(iP,:,:,:,iBlock) / PlotVar_GV(:,:,:,iVar)

             !
             if(nFluid==1 .and. .not.UseElectronPressure &
                  .and. ElectronPressureRatio > 0.0) &
                  PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar)&
                  /(1 + ElectronPressureRatio)
          end if
       case('ux')
          if (UseRotatingFrame) then
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                PlotVar_GV(i,j,k,iVar) = &
                     State_VGB(iRhoUx,i,j,k,iBlock) &
                     /State_VGB(iRho,i,j,k,iBlock) &
                     - OmegaBody*Xyz_DGB(y_,i,j,k,iBlock)
             end do; end do; end do
          else
             PlotVar_GV(:,:,:,iVar) = &
                  State_VGB(iRhoUx,:,:,:,iBlock)/State_VGB(iRho,:,:,:,iBlock)
          end if
       case('uy')
          if (UseRotatingFrame) then
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                PlotVar_GV(i,j,k,iVar) = &
                     State_VGB(iRhoUy,i,j,k,iBlock) &
                     /State_VGB(iRho,i,j,k,iBlock) &
                     + OmegaBody*Xyz_DGB(x_,i,j,k,iBlock)
             end do; end do; end do
          else
             PlotVar_GV(:,:,:,iVar) = &
                  State_VGB(iRhoUy,:,:,:,iBlock)/State_VGB(iRho,:,:,:,iBlock)
          end if
       case('uxrot')
          PlotVar_GV(:,:,:,iVar) = &
               State_VGB(iRhoUx,:,:,:,iBlock)/State_VGB(iRho,:,:,:,iBlock)
       case('uyrot')
          PlotVar_GV(:,:,:,iVar) = &
               State_VGB(iRhoUy,:,:,:,iBlock)/State_VGB(iRho,:,:,:,iBlock)
       case('uz','uzrot')
          PlotVar_GV(:,:,:,iVar) = &
               State_VGB(iRhoUz,:,:,:,iBlock)/State_VGB(iRho,:,:,:,iBlock)
       case('divu')
          allocate(u_DG(3,MinI:MaxI,MinJ:MaxJ,MinK:MaxK))
          ! Calculate velocity
          do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
             u_DG(:,i,j,k) = State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock)/ &
                  State_VGB(iRho,i,j,k,iBlock)
          end do; end do; end do
          ! Calculate div(u)
          call calc_divergence(iBlock, u_DG, &
               nG, PlotVar_GV(:,:,:,iVar), UseBodyCellIn=.true.)
          deallocate(u_DG)
       case('gradlogp')
          if(.not. allocated(GradPe_DG)) &
               allocate(GradPe_DG(3,MinI:MaxI,MinJ:MaxJ,MinK:MaxK))
          if(.not. allocated(Var_G)) &
               allocate(Var_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK))
          ! Log of pressure gradient
          Var_G = log10(State_VGB(P_,:,:,:,iBlock))
          call calc_gradient(iBlock, Var_G, nG, GradPe_DG)
          PlotVar_GV(:,:,:,iVar) = sqrt(sum(GradPe_DG**2, DIM=1))
       case('b1x')
          PlotVar_GV(:,:,:,iVar) = State_VGB(Bx_,:,:,:,iBlock)
       case('b1y')
          PlotVar_GV(:,:,:,iVar) = State_VGB(By_,:,:,:,iBlock)
       case('b1z')
          PlotVar_GV(:,:,:,iVar) = State_VGB(Bz_,:,:,:,iBlock)
       case('pperp')
          PlotVar_GV(:,:,:,iVar) = (3*State_VGB(iP,:,:,:,iBlock) &
               -State_VGB(iPpar,:,:,:,iBlock))/2.0
       case('peperp')
          PlotVar_GV(:,:,:,iVar) = (3*State_VGB(Pe_,:,:,:,iBlock) &
               -State_VGB(Pepar_,:,:,:,iBlock))/2.0

       case('gradpex','gradpey', 'gradpez', 'gradper')

          if(.not. allocated(GradPe_DG)) &
               allocate(GradPe_DG(3,MinI:MaxI,MinJ:MaxJ,MinK:MaxK))
          if(.not. allocated(Var_G)) &
               allocate(Var_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK))
          if(UseElectronPressure)then
             Var_G= State_VGB(Pe_,:,:,:,iBlock)
          else
             Var_G= State_VGB(P_,:,:,:,iBlock)*ElectronPressureRatio
          endif
          call calc_gradient(iBlock, Var_G, nG, GradPe_DG)

          select case(String)
          case('gradpex')
             PlotVar_GV(:,:,:,iVar) = GradPe_DG(1,:,:,:)
          case('gradpey')
             PlotVar_GV(:,:,:,iVar) = GradPe_DG(2,:,:,:)
          case('gradpez')
             PlotVar_GV(:,:,:,iVar) = GradPe_DG(3,:,:,:)
          case('gradper')
             do k = Mink,MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
                PlotVar_GV(i,j,k,iVar) = &
                     sum(GradPe_DG(:,i,j,k)*Xyz_DGB(:,i,j,k,iBlock)) &
                     / max(1e-30, r_GB(i,j,k,iBlock))
             end do; end do; end do
          end select

       case('jx', 'jy', 'jz', 'jr')

          if(.not. allocated(Current_DC)) allocate(Current_DC(3,nI,nJ,nK))

          ! Calculationg all the currents only once per block
          if(DoCurrent) then
             ! Note that the current in the ghost cells are not
             ! needed for Tecplot output. Maybe needed for HDF (! ).
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                call  get_current(i, j, k, iBlock, Current_DC(:,i,j,k))
             end do; end do; end do
             DoCurrent = .false.
          end if

          select case(String)
          case('jx')
             PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = Current_DC(1,:,:,:)
          case('jy')
             PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = Current_DC(2,:,:,:)
          case('jz')
             PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = Current_DC(3,:,:,:)
          case('jr')
             do k = 1,nK; do j = 1, nJ; do i = 1, nI
                PlotVar_GV(i,j,k,iVar) = &
                     sum(Current_DC(:,i,j,k)*Xyz_DGB(:,i,j,k,iBlock)) &
                     / max(1e-30, r_GB(i,j,k,iBlock))
             end do; end do; end do
          end select

       case('jx1','jy1','jz1','jx2','jy2','jz2', &
            'jx3','jy3','jz3','jx4','jy4','jz4', &
            'jx5','jy5','jz5','jx6','jy6','jz6')
          nShiftI=0; nShiftJ=0; nShiftK=0
          select case(String(3:3))
          case('1')
             iDir=1
          case('2')
             iDir=1; nShiftI=1
          case('3')
             iDir=2
          case('4')
             iDir=2; nShiftJ=1
          case('5')
             iDir=3
          case('6')
             iDir=3; nShiftK=1
          end select
          do k=1,nK; do j=1,nJ; do i=1,nI
             call get_face_curl(iDir, i+nShiftI, j+nShiftJ, k+nShiftK, iBlock, &
                  IsNewBlockCurrent, &
                  FullB_DG, Current_D)
             select case(String(2:2))
             case('x')
                PlotVar_GV(i,j,k,iVar) = Current_D(x_)
             case('y')
                PlotVar_GV(i,j,k,iVar) = Current_D(y_)
             case('z')
                PlotVar_GV(i,j,k,iVar) = Current_D(z_)
             end select
          end do; end do; end do
       case('enumx')
          PlotVar_GV(1:nI,1:nJ,1:nK,iVar) =  ExNum_CB(:,:,:,iBlock)
       case('enumy')
          PlotVar_GV(1:nI,1:nJ,1:nK,iVar) =  EyNum_CB(:,:,:,iBlock)
       case('enumz')
          PlotVar_GV(1:nI,1:nJ,1:nK,iVar) =  EzNum_CB(:,:,:,iBlock)
       case('ex')
          call get_electric_field_block(iBlock, DoHallCurrentIn = .true.)
          PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = Efield_DGB(1,1:nI,1:nJ,1:nK,iBlock)
       case('ey')
          PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = Efield_DGB(2,1:nI,1:nJ,1:nK,iBlock)
       case('ez')
          PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = Efield_DGB(3,1:nI,1:nJ,1:nK,iBlock)
       case('dive')
          call get_electric_field
          call calc_div_e
          PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = DivE_CB(:,:,:,iBlock)
       case('pote')
          call calc_inductive_e
          PlotVar_GV(:,:,:,iVar) = Potential_GB(:,:,:,iBlock)
       case('expot')
          call calc_inductive_e
          PlotVar_GV(:,:,:,iVar) = Epot_DGB(1,:,:,:,iBlock)
       case('eypot')
          call calc_inductive_e
          PlotVar_GV(:,:,:,iVar) = Epot_DGB(2,:,:,:,iBlock)
       case('ezpot')
          call calc_inductive_e
          PlotVar_GV(:,:,:,iVar) = Epot_DGB(3,:,:,:,iBlock)
       case('exind')
          call calc_inductive_e
          PlotVar_GV(:,:,:,iVar) = Eind_DGB(1,:,:,:,iBlock)
       case('eyind')
          call calc_inductive_e
          PlotVar_GV(:,:,:,iVar) = Eind_DGB(2,:,:,:,iBlock)
       case('ezind')
          call calc_inductive_e
          PlotVar_GV(:,:,:,iVar) = Eind_DGB(3,:,:,:,iBlock)
       case('pvecx')
          PlotVar_GV(:,:,:,iVar) = ( &
               ( FullB_DG(x_,:,:,:)**2  &
               + FullB_DG(y_,:,:,:)**2  &
               + FullB_DG(z_,:,:,:)**2) * &
               State_VGB(iRhoUx,:,:,:,iBlock) &
               -(FullB_DG(x_,:,:,:)* &
               State_VGB(iRhoUx,:,:,:,iBlock) + &
               FullB_DG(y_,:,:,:)* &
               State_VGB(iRhoUy,:,:,:,iBlock) + &
               FullB_DG(z_,:,:,:)* &
               State_VGB(iRhoUz,:,:,:,iBlock)) * &
               FullB_DG(x_,:,:,:) ) &
               / State_VGB(iRho,:,:,:,iBlock)
       case('pvecy')
          PlotVar_GV(:,:,:,iVar) = ( &
               (FullB_DG(x_,:,:,:)**2 + &
               FullB_DG(y_,:,:,:)**2 + &
               FullB_DG(z_,:,:,:)**2) * &
               State_VGB(iRhoUy,:,:,:,iBlock) &
               -(FullB_DG(x_,:,:,:)* &
               State_VGB(iRhoUx,:,:,:,iBlock) + &
               FullB_DG(y_,:,:,:)* &
               State_VGB(iRhoUy,:,:,:,iBlock) + &
               FullB_DG(z_,:,:,:)* &
               State_VGB(iRhoUz,:,:,:,iBlock)) * &
               FullB_DG(y_,:,:,:) ) &
               / State_VGB(iRho,:,:,:,iBlock)
       case('pvecz')
          PlotVar_GV(:,:,:,iVar) = ( &
               (FullB_DG(x_,:,:,:)**2 + &
               FullB_DG(y_,:,:,:)**2 + &
               FullB_DG(z_,:,:,:)**2) * &
               State_VGB(iRhoUz,:,:,:,iBlock) &
               -(FullB_DG(x_,:,:,:)* &
               State_VGB(iRhoUx,:,:,:,iBlock) + &
               FullB_DG(y_,:,:,:)* &
               State_VGB(iRhoUy,:,:,:,iBlock) + &
               FullB_DG(z_,:,:,:)* &
               State_VGB(iRhoUz,:,:,:,iBlock)) * &
               FullB_DG(z_,:,:,:) ) &
               / State_VGB(iRho,:,:,:,iBlock)

          ! Radial component variables

       case('ur')
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             PlotVar_GV(i,j,k,iVar) = sum( &
                  State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock) &
                  *Xyz_DGB(:,i,j,k,iBlock)) &
                  / (State_VGB(iRho,i,j,k,iBlock)*r_GB(i,j,k,iBlock))
          end do; end do; end do
       case('rhour','mr')
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             PlotVar_GV(i,j,k,iVar) = sum( &
                  State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock) &
                  *Xyz_DGB(:,i,j,k,iBlock)) / r_GB(i,j,k,iBlock)
          end do; end do; end do
       case('br')
          UsePlotVarBody_V(iVar) = .true.
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             PlotVar_GV(i,j,k,iVar) = sum( &
                  FullB_DG(:,i,j,k)*Xyz_DGB(:,i,j,k,iBlock) &
                  ) / r_GB(i,j,k,iBlock)
          end do; end do; end do
       case('b1r')
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             PlotVar_GV(i,j,k,iVar) = sum( &
                  State_VGB(Bx_:Bz_,i,j,k,iBlock)*Xyz_DGB(:,i,j,k,iBlock) &
                  ) / r_GB(i,j,k,iBlock)
          end do; end do; end do
       case('er')
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             PlotVar_GV(i,j,k,iVar) =  sum( &
                  Xyz_DGB(:,i,j,k,iBlock) &
                  *cross_product(FullB_DG(:,i,j,k), &
                  State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock))) &
                  / (State_VGB(iRho,i,j,k,iBlock)*r_GB(i,j,k,iBlock))
          end do; end do; end do
       case('pvecr')
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             FullB2 = sum(FullB_DG(:,i,j,k)**2)
             FullBRhoU = sum(FullB_DG(:,i,j,k)* &
                  State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock))
             PlotVar_GV(i,j,k,iVar) = sum ( &
                  Xyz_DGB(:,i,j,k,iBlock) &
                  *( FullB2*State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock) &
                  -  FullBRhoU*FullB_DG(:,i,j,k) ) &
                  ) / (State_VGB(iRho,i,j,k,iBlock)*r_GB(i,j,k,iBlock))
          end do; end do; end do
       case('b2ur')
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             PlotVar_GV(i,j,k,iVar) = 0.5*sum(FullB_DG(:,i,j,k)**2) &
                  *sum( State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock) &
                  *     Xyz_DGB(:,i,j,k,iBlock) &
                  ) / (State_VGB(iRho,i,j,k,iBlock)*r_GB(i,j,k,iBlock))
          end do; end do; end do
       case('visco')
          if (UseViscosity) then
             call set_visco_factor_cell(iBlock)
             PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = ViscoFactor_C
          end if
       case('divb')
          if(.not.UseConstrainB)then
             call calc_divergence(iBlock, State_VGB(Bx_:Bz_,:,:,:,iBlock), &
                  nG, PlotVar_GV(:,:,:,iVar), UseBodyCellIn=.true.)
          else
             ! Div B from face fluxes
             do k = 1, nK; do j = 1, nJ; do i =1, nI
                if(.not. Used_GB(i,j,k,iBlock)) CYCLE
                PlotVar_GV(i,j,k,iVar) = &
                     (BxFace_GB(i+1,j,k,iBlock)                         &
                     -BxFace_GB(i  ,j,k,iBlock))/CellSize_DB(x_,iBlock) + &
                     (ByFace_GB(i,j+1,k,iBlock)                         &
                     -ByFace_GB(i,j  ,k,iBlock))/CellSize_DB(y_,iBlock)
                if(nK > 1) PlotVar_GV(i,j,k,iVar) = PlotVar_GV(i,j,k,iVar) + &
                     (BzFace_GB(i,j,k+1,iBlock)                         &
                     -BzFace_GB(i,j,k  ,iBlock))/CellSize_DB(z_,iBlock)
             end do; end do; end do
          end if

       case('absdivb')
          if(UseB) PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = &
               abs(DivB1_GB(1:nI,1:nJ,1:nK,iBlock))
          if(.not.IsNoBody_B(iBlock))then
             where(.not.Used_GB(:,:,:,iBlock)) PlotVar_GV(:,:,:,iVar) = 0.0
          endif

       case('theta1','req1','theta2','req2','phi1','phi2','status')
          ! BASIC RAYTRACE variables
          select case(String)
          case ('theta1', 'req1')
             i3 = 1 ; j3 = 1
          case ('theta2', 'req2')
             i3 = 1 ; j3 = 2
          case ('phi1')
             i3 = 2 ; j3 = 1
          case ('phi2')
             i3 = 2 ; j3 = 2
          case ('status')
             i3 = 3 ; j3 = 1
          end select

          PlotVar_GV(1:nI,1:nJ,1:nK,iVar) &
               = Trace_DSNB(i3,j3,1:nI,1:nJ,1:nK,iBlock)

          ! Now load the face ghost cells with the first computation
          ! cell on each face.  This is a bad approximation but is
          ! needed for Tecplot.  It will be fixed later using message
          ! passing
          PlotVar_GV(1:nI,1:nJ,1:nK,iVar) &
               = Trace_DSNB(i3,j3,1:nI,1:nJ,1:nK,iBlock)

          ! GRID INFORMATION
       case('crit1')
          ! call calc_error_amr_criteria(nVar, State_VGB)
          if(allocated(AmrCrit_IB) .and. nAmrCrit >= 1) &
               PlotVar_GV(:,:,:,iVar) = AmrCrit_IB(1,iBlock)
       case('crit2')
          if(allocated(AmrCrit_IB) .and. nAmrCrit >= 2) &
               PlotVar_GV(:,:,:,iVar) = AmrCrit_IB(2,iBlock)
       case('crit3')
          if(allocated(AmrCrit_IB) .and. nAmrCrit >= 3) &
               PlotVar_GV(:,:,:,iVar) = AmrCrit_IB(3,iBlock)
       case('crit4')
          if(allocated(AmrCrit_IB) .and. nAmrCrit >= 4) &
               PlotVar_GV(:,:,:,iVar) = AmrCrit_IB(4,iBlock)
       case('crit5')
          if(allocated(AmrCrit_IB) .and. nAmrCrit >= 5) &
               PlotVar_GV(:,:,:,iVar) = AmrCrit_IB(5,iBlock)
       case('crit6')
          if(allocated(AmrCrit_IB) .and. nAmrCrit >= 6) &
               PlotVar_GV(:,:,:,iVar) = AmrCrit_IB(6,iBlock)
       case('crit7')
          if(allocated(AmrCrit_IB) .and. nAmrCrit >= 7) &
               PlotVar_GV(:,:,:,iVar) = AmrCrit_IB(7,iBlock)
       case('crit8')
          if(allocated(AmrCrit_IB) .and. nAmrCrit >= 8) &
               PlotVar_GV(:,:,:,iVar) = AmrCrit_IB(8,iBlock)
       case('crit9')
          if(allocated(AmrCrit_IB) .and. nAmrCrit >= 9) &
               PlotVar_GV(:,:,:,iVar) = AmrCrit_IB(9,iBlock)
       case('amrlevel')
          PlotVar_GV(:,:,:,iVar) = iTree_IA(Level_,iNode_B(iBlock))
       case('timelevel')
          if(allocated(iTimeLevel_A)) &
               PlotVar_GV(:,:,:,iVar) = iTimeLevel_A(iNode_B(iBlock))
       case('dvol')
          PlotVar_GV(:,:,:,iVar) = CellVolume_GB(:,:,:,iBlock)
       case('dx','dr')
          PlotVar_GV(:,:,:,iVar) = CellSize_DB(x_,iBlock)
       case('dy','dphi','dlon')
          PlotVar_GV(:,:,:,iVar) = CellSize_DB(y_,iBlock)
       case('dz','dlat')
          PlotVar_GV(:,:,:,iVar) = CellSize_DB(z_,iBlock)
       case('dt')
          PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = DtMax_CB(1:nI,1:nJ,1:nK,iBlock)
       case('dtblk')
          PlotVar_GV(:,:,:,iVar) = DtMax_B(iBlock)
          ! if(.not.IsNoBody_B(iBlock))then
          !   if(.not.any(Used_GB(:,:,:,iBlock)))&
          !        PlotVar_GV(:,:,:,iVar) = 0.0
          ! end if
       case('cons')
          if(allocated(IsConserv_CB))then
             where(IsConserv_CB(:,:,:,iBlock)) &
                  PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = 1
          else if(.not.UseNonConservative)then
             PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = 1.
          end if
       case('ibound')
          PlotVar_GV(:,:,:,iVar) = iBoundary_GB(:,:,:,iBlock)
       case('evolve','impl')
          PlotVar_GV(:,:,:,iVar) = iTypeAdvance_B(iBlock)
          if(UsePointImplicit)then
             if(UseUserPointImplicit_B(iBlock))&
                  PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar)+0.5
          end if
       case('balance')
          if(allocated(iTypeBalance_A)) &
               PlotVar_GV(:,:,:,iVar) = iTypeBalance_A(iNode_B(iBlock))
       case('clight')
          call set_clight_cell(iBlock)
          PlotVar_GV(:,:,:,iVar) = Clight_G
       case('loworder')
          if(allocated(iRegionLowOrder_I)) call block_inside_regions( &
               iRegionLowOrder_I, iBlock, size(PlotVar_GV(:,:,:,iVar)), &
               'ghost', Value_I=PlotVar_GV(MinI,MinJ,MinK,iVar))
       case('lowcritx')
          PlotVar_GV(:,:,:,iVar) = 0
          ! LowOrderCrit_XB is actually face based.
          if(allocated(LowOrderCrit_XB)) &
               PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = &
               LowOrderCrit_XB(1:nI,1:nJ,1:nK,iBlock)
       case('lowcrity')
          PlotVar_GV(:,:,:,iVar) = 0
          if(allocated(LowOrderCrit_YB)) &
               PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = &
               LowOrderCrit_YB(1:nI,1:nJ,1:nK,iBlock)
       case('lowcritz')
          PlotVar_GV(:,:,:,iVar) = 0
          if(allocated(LowOrderCrit_ZB)) &
               PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = &
               LowOrderCrit_ZB(1:nI,1:nJ,1:nK,iBlock)
       case('proc')
          PlotVar_GV(:,:,:,iVar) = iProc
       case('blk','block')
          PlotVar_GV(:,:,:,iVar) = iBlock
       case('node')
          PlotVar_GV(:,:,:,iVar) = iNode_B(iBlock)
       case('hall')
          if(UseHallResist)then
             call set_hall_factor_cell(iBlock)
             PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = HallFactor_C
          end if
       case('hallfactor')
          if(UseHallResist)then
             call set_hall_factor_cell(iBlock, .false.)
             PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = HallFactor_C
          end if
       case('hallblock')
          if(UseHallResist)then
             call set_hall_factor_cell(iBlock, .false.)
             if(IsHallBlock)PlotVar_GV(1:nI,1:nJ,1:nK,iVar) = 1.0
          end if
       case('elaser')
          if(UseLaserHeating)then
             if(allocated(LaserHeating_CB)) PlotVar_GV(1:nI,1:nJ,1:nK,iVar) &
                  = LaserHeating_CB(:,:,:,iBlock)
          end if
       case('ew','erad')
          if(Ew_ == 1)then
             if(UseWavePressure)then
                do k = 1, nK; do j = 1, nJ; do i = 1, nI
                   PlotVar_GV(i,j,k,iVar) = &
                        sum(State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock))
                end do; end do; end do
             end if
          else
             PlotVar_GV(:,:,:,iVar) = State_VGB(Ew_,:,:,:,iBlock)
          end if
       case('pic')
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             PlotVar_GV(i,j,k,iVar) = i_status_pic_region(iBlock,i,j,k)
          end do; end do; end do
       case('pic_active')
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             PlotVar_GV(i,j,k,iVar) = i_status_pic_active(iBlock,i,j,k)
          end do; end do; end do
       case('pic_crit')
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             PlotVar_GV(i,j,k,iVar) = iStatusPicCrit_CB(i,j,k,iBlock)
          end do; end do; end do
       case('jb')
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             call calc_crit_jb(i, j, k, iBlock, FullB_DG,&
                  CriteriaB1, PlotVar_GV(i,j,k,iVar))
          end do; end do; end do
       case('jbperp')
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             call calc_crit_jbperp(i, j, k, iBlock, FullB_DG,&
                  CriteriaB1, PlotVar_GV(i,j,k,iVar))
          end do; end do; end do
       case('divcurv')
          if(allocated(DivCurvature_CB)) then
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                PlotVar_GV(i,j,k,iVar) = DivCurvature_CB(i,j,k,iBlock)
             end do; end do; end do
          else
             PlotVar_GV(:,:,:,iVar) = -777.0
          end if
       case('entropy')
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             call calc_crit_entropy(i, j, k, iBlock, State_VGB, &
                  PlotVar_GV(i,j,k,iVar))
          end do; end do; end do
       case('qtot')
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             PlotVar_GV(i,j,k,iVar) = &
                  sum(State_VGB(iRhoIon_I,i,j,k,iBlock)*ChargeIon_I/MassIon_I)
          end do; end do; end do

       case default
          ! Check if the name is one of the state variable names
          do jVar = 1, nVar
             if(NamePlotVar /= NameVarLower_V(jVar)) CYCLE
             PlotVar_GV(:,:,:,iVar) = State_VGB(jVar,:,:,:,iBlock)
             if(DefaultState_V(jVar) > 0.0) &
                  PlotVarBody_V(iVar) = FaceState_VI(jVar,body1_)
             EXIT
          end do
          if(jVar > nVar) then
             call user_set_plot_var(iBlock, &
                  NamePlotVar, IsDimensionalPlot_I(Plot_+iPlotFile), &
                  PlotVar_GV(:,:,:,iVar), &
                  PlotVarBody_V(iVar), UsePlotVarBody_V(iVar), &
                  NameVarUserTec_I(iVar), NameUnitUserTec_I(iVar), &
                  NameUnitUserIdl_I(iVar), IsFound)
             if(.not. IsFound) then
                PlotVar_GV(:,:,:,iVar) = -7777.
                if(iProc==0 .and. iBlock==1)write(*,*) &
                     'Warning in set_plotvar: unknown plotvarname=',&
                     NamePlotVar_V(iVar),' for iPlotFile=',iPlotFile
             end if
          end if
       end select
    end do ! iVar

    if(allocated(Current_DC)) deallocate(Current_DC)
    if(allocated(Var_G)) deallocate(Var_G)
    if(allocated(GradPe_DG)) deallocate(GradPe_DG)

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine set_plotvar
  !============================================================================
  subroutine dimensionalize_plotvar(iBlock, iPlotFile, nPlotVar, &
       NamePlotVar_V, PlotVar_GV, PlotVarBody_V)

    use ModPhysics, ONLY: nVar, UnitX_, UnitTemperature_, UnitN_, UnitRho_, &
       UnitP_, UnitU_, UnitB_, UnitT_, UnitMass_, UnitDivB_, UnitRhoU_, &
       UnitElectric_, UnitJ_, UnitPoynting_, UnitCharge_, UnitEnergyDens_, &
       No2Io_V, No2Si_V, UnitUser_V, NameVarLower_V
    use ModVarIndexes, ONLY: DefaultState_V
    use ModUtilities,  ONLY: lower_case
    use ModMultiFluid, ONLY: extract_fluid_name
    use BATL_lib, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK

    integer, intent(in) :: iBlock,iPlotFile,nPlotVar
    character (LEN=10), intent(in) :: NamePlotVar_V(nPlotVar)
    real, intent(inout) :: PlotVar_GV(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,nPlotVar)
    real, intent(inout) :: PlotVarBody_V(nPlotVar)

    character (len=10)  :: String, NamePlotVar

    integer :: iVar, jVar
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'dimensionalize_plotvar'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    do iVar=1,nPlotVar
       NamePlotVar = NamePlotVar_V(iVar)
       call lower_case(NamePlotVar)
       String = NamePlotVar
       call extract_fluid_name(String)

       ! Set PlotVarBody_V to something reasonable for inside the body.
       ! Load zeros for most values - load something better for rho, p, and T
       ! We know that U,B,J are okay with zeroes, others should be changed if
       ! necessary.
       ! Note that all variables not set to 0 in set_plotvar should be
       ! loaded below. Note that this is used for tecplot corner extrapolation
       ! and for nothing else.

       select case(String)

          ! BASIC MHD variables

       case('rho')
          PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) &
               *No2Io_V(UnitRho_)
          PlotVarBody_V(iVar) = PlotVarBody_V(iVar) &
               *No2Io_V(UnitRho_)
       case('rhoux','mx','rhouy','my','rhouz','mz','rhour','mr' )
          PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) &
               *No2Io_V(UnitRhoU_)
       case('bx','by','bz','br','b1x','b1y','b1z','b1r' &
            ,'bxl','bxr','byl','byr','bzl','bzr' &
            )
          PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) &
               *No2Io_V(UnitB_)
       case('elaser')
          PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) &
               *No2Io_V(UnitEnergyDens_)/No2Io_V(UnitT_)
       case('e','e1','ew','erad')
          PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) &
               *No2Io_V(UnitEnergyDens_)
       case('p','pth','pperp','peperp')
          PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) &
               *No2Io_V(UnitP_)
          PlotVarBody_V(iVar) = PlotVarBody_V(iVar) &
               *No2Io_V(UnitP_)

          ! EXTRA MHD variables
       case('n', 'qtot')
          ! Number and charge densities
          PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) &
               *No2Io_V(UnitN_)
       case('t','temp')
          PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) &
               *No2Io_V(UnitTemperature_)
       case('eta','visco')
          PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) &
               *(No2Si_V(UnitX_)**2/No2Si_V(UnitT_))
       case('ux','uy','uz','uxrot','uyrot','uzrot','ur','clight')
          PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) &
               *No2Io_V(UnitU_)
       case('divu')
          PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) &
               /No2Io_V(UnitT_)
       case('gradpex','gradpey','gradpez','gradper')
          PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) &
               *No2Io_V(UnitP_)/No2Si_V(UnitX_)
       case('jx','jy','jz','jr',&
            'jx1','jy1','jz1','jx2','jy2','jz2', &
            'jx3','jy3','jz3','jx4','jy4','jz4', &
            'jx5','jy5','jz5','jx6','jy6','jz6')
          PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) &
               *No2Io_V(UnitJ_)
       case('ex','ey','ez','er','enumx','enumy','enumz', &
            'expot','eypot','ezpot','exind','eyind','ezind')
          PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) &
               *No2Io_V(UnitElectric_)
       case('pote')
          ! Electric potential has SI units of V
          PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) &
               *No2Si_V(UnitElectric_)*No2Si_V(UnitX_)
       case('dive')
          ! Divergence of electric field has SI units of V/m^2
          PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) &
               *No2Si_V(UnitElectric_)/No2Si_V(UnitX_)
       case('pvecx','pvecy','pvecz','pvecr','b2ur')
          PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) &
               *No2Io_V(UnitPoynting_)
       case('divb','divb_cd','divb_ct','absdivb')
          PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) &
               *No2Io_V(UnitDivB_)

          ! GRID INFORMATION
       case('dt','dtblk')
          PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) &
               *No2Io_V(UnitT_)
       case('x','y','z','r','dx','dr','dy','dz','req1','req2')
          PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar) &
               *No2Io_V(UnitX_)
       case('dphi','dlon','dlat')
          PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar)*cRadToDeg

          ! DEFAULT CASE
       case default
          do jVar = 1, nVar
             if(NamePlotVar /= NameVarLower_V(jVar)) CYCLE
             PlotVar_GV(:,:,:,iVar) = PlotVar_GV(:,:,:,iVar)*UnitUser_V(jVar)
             if(DefaultState_V(jVar) > 0.0)&
                  PlotVarBody_V(iVar) = PlotVarBody_V(iVar)*UnitUser_V(jVar)
             EXIT
          end do
          ! no normalization
       end select
    end do ! iVar

    call test_stop(NameSub, DoTest, iBlock)

  end subroutine dimensionalize_plotvar
  !============================================================================

  subroutine get_idl_units(iFile, nPlotVar, NamePlotVar_V, NamePlotUnit_V, &
       StringUnitIdl)

    use ModPhysics, ONLY: nVar, UnitX_, UnitTemperature_, UnitN_, UnitRho_, &
       UnitP_, UnitU_, UnitB_, UnitT_, UnitMass_, UnitDivB_, UnitRhoU_, &
       UnitElectric_, UnitJ_, UnitPoynting_, UnitCharge_, UnitEnergyDens_, &
       NameIdlUnit_V, NameUnitUserIdl_V, UnitAngle_, NameVarLower_V
    use ModUtilities,  ONLY: lower_case
    use ModIO,         ONLY: TypePlot, IsDimensionalPlot_I, NameUnitUserIdl_I
    use ModMultiFluid, ONLY: extract_fluid_name
    use BATL_lib,      ONLY: IsRLonLat, IsCylindrical

    ! Arguments

    integer, intent(in)             :: iFile, nPlotVar
    character (len=10), intent(in)  :: NamePlotVar_V(nPlotVar)
    character (len=10), intent(out) :: NamePlotUnit_V(nPlotVar)
    character (len=500),intent(out) :: StringUnitIdl

    ! set the unit description strings based on the plot variable names
    ! for plot  file iFile

    character (len=10) :: String, NamePlotVar, NameUnit
    integer            :: iPlotVar, iVar

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_idl_units'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(.not.IsDimensionalPlot_I(iFile))then
       NamePlotUnit_V = 'normalized'
       StringUnitIdl = 'normalized variables'
       RETURN
    end if

    if(TypePlot(1:3) == 'shl' .or. &
         (TypePlot(1:3) == 'cut' .and. IsRLonLat)) then
       StringUnitIdl = trim(NameIdlUnit_V(UnitX_))//' deg deg'
    elseif(TypePlot(1:3) == 'cut' .and. IsCylindrical)then
          StringUnitIdl = trim(NameIdlUnit_V(UnitX_))//' '//&
               trim(NameIdlUnit_V(UnitX_))//' deg'
    else
       StringUnitIdl = trim(NameIdlUnit_V(UnitX_))//' '//&
            trim(NameIdlUnit_V(UnitX_))//' '//trim(NameIdlUnit_V(UnitX_))
    end if

    do iPlotVar = 1, nPlotVar

       NamePlotVar = NamePlotVar_V(iPlotVar)
       call lower_case(NamePlotVar)
       String = NamePlotVar
       call extract_fluid_name(String)

       select case(String)
       case('rho')
          NameUnit = NameIdlUnit_V(UnitRho_)
       case('rhoux','mx','rhouy','rhoUz','rhouz','mz','rhour','mr')
          NameUnit = NameIdlUnit_V(UnitRhoU_)
       case('bx','by','bz','b1x','b1y','b1z','br','b1r')
          NameUnit = NameIdlUnit_V(UnitB_)
       case('e','ew','erad')
          NameUnit = NameIdlUnit_V(UnitEnergydens_)
       case('p','pth','pperp','peperp')
          NameUnit = NameIdlUnit_V(UnitP_)
       case('n')
          NameUnit = NameIdlUnit_V(UnitN_)
       case('t','temp')
          NameUnit = NameIdlUnit_V(UnitTemperature_)
       case('ux','uy','uz','ur','uxrot','uyrot','uzrot')
          NameUnit = NameIdlUnit_V(UnitU_)
       case('gradpex','gradpey','gradpez','gradper')
          NameUnit = 'nPa/m'
       case('jx','jy','jz','jr',&
            'jx1','jy1','jz1','jx2','jy2','jz2', &
            'jx3','jy3','jz3','jx4','jy4','jz4', &
            'jx5','jy5','jz5','jx6','jy6','jz6')
          NameUnit = NameIdlUnit_V(UnitJ_)
       case('ex','ey','ez','er','enumx','enumy','enumz', &
            'expot', 'eypot', 'ezpot', 'exind', 'eyind', 'ezind')
          NameUnit = NameIdlUnit_V(UnitElectric_)
       case('pote')
          NameUnit = 'V'
       case('dive')
          NameUnit = 'V/m2'
       case('pvecx','pvecy','pvecz','pvecr','b2ur')
          NameUnit = NameIdlUnit_V(UnitPoynting_)
       case('divb','divb_cd','divb_ct','absdivb')
          NameUnit = NameIdlUnit_V(UnitDivB_)
       case('theta1','phi1','theta2','phi2')
          NameUnit = NameIdlUnit_V(UnitAngle_)
       case('status','f1x','f1y','f1z','f2x','f2y','f2z')
          NameUnit = '--'
          ! GRID INFORMATION
       case('proc','blk','node','impl','evolve')
          NameUnit = '1'
       case('dt', 'dtblk')
          NameUnit = NameIdlUnit_V(UnitT_)
       case('x','y','z','r','dx','dr','dy','dz','req1','req2')
          NameUnit = NameIdlUnit_V(UnitX_)
       case('dphi','dlon','dlat')
          NameUnit = NameIdlUnit_V(UnitAngle_)
       case('dvol')
          NameUnit = trim(NameIdlUnit_V(UnitX_))//'3'
       case('eta','visco')
          NameUnit = 'm2/s'
       case('qtot')
          NameUnit = 'e'//NameIdlUnit_V(UnitN_)
       case default
          ! Set default or user defined unit
          NameUnit = NameUnitUserIdl_I(iPlotVar)

          ! Try to find the plot variable among the basic variables
          do iVar = 1, nVar
             if(NamePlotVar /= NameVarLower_V(iVar)) CYCLE
             NameUnit = NameUnitUserIdl_V(iVar)
             EXIT
          end do
       end select
       ! Append the unit string for this variable to the output string
       NamePlotUnit_V(iPlotVar) = NameUnit
       StringUnitIdl = trim(StringUnitIdl)//' '//trim(NameUnit)
    end do

    call test_stop(NameSub, DoTest)
  end subroutine get_idl_units
  !============================================================================

  subroutine reverse_field(iBlock)

    use ModAdvance,    ONLY: State_VGB
    use BATL_size,     ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK
    use ModVarIndexes, ONLY: Bx_, Bz_, SignB_, WaveFirst_, WaveLast_
    use ModWaves,      ONLY: UseAlfvenWaves

    integer, intent(in) :: iBlock

    integer :: i, j, k
    real :: Ewave

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'reverse_field'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
       if(State_VGB(SignB_,i,j,k,iBlock) < 0.0)then
          State_VGB(Bx_:Bz_,i,j,k,iBlock) = -State_VGB(Bx_:Bz_,i,j,k,iBlock)
          if(UseAlfvenWaves)then
             Ewave = State_VGB(WaveFirst_,i,j,k,iBlock)
             State_VGB(WaveFirst_,i,j,k,iBlock) = &
                  State_VGB(WaveLast_,i,j,k,iBlock)
             State_VGB(WaveLast_,i,j,k,iBlock) = Ewave
          end if
       end if
    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine reverse_field
  !============================================================================
  subroutine adjust_plot_range(PlotRes1, PlotRange_I)

    ! Adjust plot range so it coincides with the cell boundaries
    ! of the cells of size PlotRes1 in the first dimension

    use ModKind,  ONLY: nByteReal
    use BATL_lib, ONLY: nDim, DomainSize_D, CoordMin_D, nIJK_D, nRoot_D

    real, intent(in)   :: PlotRes1       ! smallest cell size in coord1 to plot
    real, intent(inout):: PlotRange_I(6) ! plot range to be adjusted

    real:: CellSizeMax_D(nDim), SmallSize_D(nDim), PlotRes_D(nDim)
    integer:: iDim, iMin, iMax

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'adjust_plot_range'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    ! Largest cell size and a much smaller distance for 2D cuts
    CellSizeMax_D = DomainSize_D(1:nDim)/(nIJK_D(1:nDim)*nRoot_D(1:nDim))

    ! Calculate plot cell size in all directions
    PlotRes_D = PlotRes1/CellSizeMax_D(1) * CellSizeMax_D

    if(nByteReal == 8)then
       SmallSize_D   = 1e-9*CellSizeMax_D
    else
       SmallSize_D   = 1e-6*CellSizeMax_D
    end if

    do iDim = 1, nDim
       iMin = 2*iDim - 1; iMax = iMin+1

       ! DoSkip ignored dimensions of 2D and 1D cuts
       if(PlotRange_I(iMax) - PlotRange_I(iMin) <= 1.5*PlotRes_D(iDim)) CYCLE

       ! Shift plot range slightly outward
       PlotRange_I(iMin) = PlotRange_I(iMin) - SmallSize_D(iDim)
       PlotRange_I(iMax) = PlotRange_I(iMax) + SmallSize_D(iDim)

       ! Round plot range to multiple of plot resolution
       Plotrange_I(iMin:iMax) = CoordMin_D(iDim) + PlotRes_D(iDim)* &
            nint( (PlotRange_I(iMin:iMax) - CoordMin_D(iDim))/PlotRes_D(iDim) )
    end do

    call test_stop(NameSub, DoTest)
  end subroutine adjust_plot_range
  !============================================================================

end module ModWritePlot
!==============================================================================
