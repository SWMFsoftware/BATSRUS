!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModHdf5

  use BATL_lib, ONLY: &
       test_start, test_stop

  ! This empty module is used when HDF5 plotting is not enabled

  implicit none

  integer, parameter:: lNameVar = 10

contains
  !============================================================================
  subroutine init_hdf5_plot(iFile, TypePlot, nPlotVar, &
       xMin, xMax, yMin, yMax, zMin, zMax, DxBlock, DyBlock, DzBlock,&
       IsNonCartesian, IsNotACut)

    ! Arguments

    integer, intent(in) :: iFile
    integer, intent(in) :: nPlotVar
    real,    intent(in) :: xMin, xMax, yMin, yMax, zMin, zMax
    logical, intent(in) :: IsNonCartesian, IsNotACut
    character (len=*), intent(in) :: TypePlot
    real,    intent(inout):: DxBlock, DyBlock, DzBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_hdf5_plot'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    call test_stop(NameSub, DoTest)
  end subroutine init_hdf5_plot
  !============================================================================
  subroutine write_plot_hdf5( &
       NameFile, TypePlot, NamePlotVar_V, NamePlotUnit_V, &
       nPlotVar, IsNotACut, IsNonCartesian, IsSphPlot, IsDimensionalPlot, &
       xMin, xMax, yMin, yMax, zMin, zMax)

    use BATL_lib, ONLY: iProc

    integer,                 intent(in):: nPlotVar
    character(len=80),       intent(in):: NameFile
    character(len=lnamevar), intent(in):: NamePlotVar_V(nplotvar)
    character(len=lNameVar), intent(in):: NamePlotUnit_V(nPlotVar)
    character(len=*),        intent(in):: TypePlot
    real,    intent(in):: xMin, xMax, yMin, yMax, zMin, zMax
    logical, intent(in):: &
         IsNotACut, IsDimensionalPlot, IsNonCartesian, IsSphPlot
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_plot_hdf5'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(iProc==0) write (*,*) "iError: HDF5 plotting is not enabled!"

    call test_stop(NameSub, DoTest)
  end subroutine write_plot_hdf5
  !============================================================================
  subroutine write_var_hdf5( &
       iFile, TypePlot, iBlock, iH5Index, nPlotVar, PlotVar_GV, &
       xMin, xMax, yMin, yMax, zMin, zMax, DxBlock, DyBlock, DzBlock,&
       IsNonCartesian, IsNotACut, nCell,DoH5Advance)

    ! Save all cells within plotting range, for each processor

    use ModSize, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK

    ! Arguments

    integer, intent(in)   :: iFile, iBlock, iH5Index
    integer, intent(in)   :: nPlotVar
    real,    intent(in)   :: PlotVar_GV(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,nPlotVar)
    real,    intent(in)   :: xMin,xMax,yMin,yMax,zMin,zMax
    logical, intent(in)   :: IsNonCartesian, IsNotACut
    character (len=*), intent(in) :: TypePlot
    real,    intent(inout):: DxBlock,DyBlock,DzBlock
    integer, intent(out)  :: nCell
    logical, intent(out)  :: DoH5Advance

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_var_hdf5'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    nCell = 0
    DoH5Advance = .false.

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine write_var_hdf5
  !============================================================================

end module ModHdf5
!==============================================================================
