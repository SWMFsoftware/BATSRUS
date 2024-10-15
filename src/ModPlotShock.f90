!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModPlotShock

  use BATL_lib, ONLY: &
       test_start, test_stop, iProc, nProc, iComm
  use ModBatsrusUtility, ONLY: stop_mpi

  use ModIO
  use ModNumConst,  ONLY: cRadtoDeg, cDegToRad

  implicit none

  SAVE

  private   ! except
  public:: init_plot_shock
  public:: set_plot_shock
  public:: write_plot_shock

  ! Threshold for Divu*Dx
  real, public :: DivuDxMin = 0.0

  ! Local variables

  ! Size of current plot:
  integer :: nR, nLon, nLat

  ! Ranges for current plot:
  real :: dR, dLat, dLon
  real :: rMinPlot, rMaxPlot, LonMin, LonMax, LatMin, LatMax

  integer, parameter :: RadiusTransformLinear_ = 1
  integer, parameter :: RadiusTransformLog_ = 2
  integer, parameter :: RadiusTransformLog10_ = 3

  ! Local results container:
  ! Array of values written to file:
  real, allocatable :: PlotVar_VII(:,:,:)

contains
  !============================================================================
  subroutine init_plot_shock(iFile, nPlotVar)

    ! set up the shock grid for this plot file
    integer, intent(in):: iFile, nPlotVar

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_plot_shock'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Allocate results array and set up the spherical grid
    if(allocated(PlotVar_VII)) RETURN

    ! Get plot area info from ModIO arrays:
    dLon     = PlotDx_DI(2,iFile) * cDegtoRad
    dLat     = PlotDx_DI(3,iFile) * cDegtoRad
    rMinPlot = PlotRange_EI(1,iFile)
    rMaxPlot = PlotRange_EI(2,iFile)
    LonMin = PlotRange_EI(3,iFile) * cDegtoRad
    LonMax = PlotRange_EI(4,iFile) * cDegtoRad
    LatMin = PlotRange_EI(5,iFile) * cDegtoRad
    LatMax = PlotRange_EI(6,iFile) * cDegtoRad

    ! Set number of points:
    nLon = nint((LonMax - LonMin)/dLon) + 1
    nLat = nint((LatMax - LatMin)/dLat) + 1

    ! Ensure dR, dLon and dLat are compatible with the ranges
    dLon = (LonMax - LonMin)/max(1, nLon - 1)
    dLat = (LatMax - LatMin)/max(1, nLat - 1)

    ! The 0 element is for the radius.
    allocate(PlotVar_VII(0:nPlotVar,nLon,nLat))
    PlotVar_VII = 0.0

    if (DoTest) then
       write(*,*) NameSub//' iFile, nPlotVar= ', iFile, nPlotVar
       write(*,*) NameSub//' Raw PlotDx_DI=   ', PlotDx_DI(:,iFile)
       write(*,*) NameSub//' Raw PlotRange_EI=', PlotRange_EI(:,iFile)
       write(*,*) NameSub//' dLon, dLat =     ', dLon, dLat
       write(*,*) NameSub//' r, Lon, Lat range = ',  &
            rMinPlot, rMaxPlot, LonMin,LonMax,LatMin,LatMax
       write(*,*) NameSub,' nLon, nLat, dLon, dLat = ', &
            nLon, nLat, dLon, dLat
    end if

    call test_stop(NameSub, DoTest)

  end subroutine init_plot_shock
  !============================================================================
  subroutine set_plot_shock(iBlock, nPlotvar, Plotvar_GV)

    ! Interpolate the plot variables for block iBlock
    ! onto the shock surface of the plot area.
    use ModGeometry,    ONLY: rMin_B, r_GB
    use ModInterpolate, ONLY: trilinear
    use BATL_lib,       ONLY: CoordMin_DB, nIjk_D, CellSize_DB, &
         xyz_to_coord, IsCartesianGrid
    use ModCoordTransform, ONLY: rlonlat_to_xyz

    ! Arguments
    integer, intent(in) :: iBlock
    integer, intent(in) :: nPlotvar
    real,    intent(in) :: PlotVar_GV(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,nPlotVar)

    ! Local variables
    integer :: i, j, k, iVar

    real :: r, Lon, Lat
    real :: XyzPlot_D(3)
    real :: Coord_D(3), CoordNorm_D(3)

    ! Interpolated plot variables
    real :: PlotVar_V(nPlotVar)
    real :: rMin, rMax

    ! Check testing for block
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_plot_shock'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    rMax = maxval(r_GB(:,:,:,iBlock))
    ! Return if block is below the PlotRange
    if(rMax < rMinPlot) RETURN
    ! Return if block is above the PlotRange
    rMin = rMin_B(iBlock)
    if(rMin > rMaxPlot) RETURN

    ! Limit radial range
    ! rMin = max(rMin, rMinPlot)
    ! rMax = min(rMax, rMaxPlot)

    ! Loop through shock points and interpolate PlotVar
    if(IsCartesianGrid)then
       nR = nI + nJ + nK
    else
       nR = nI*3
    end if
    dR = (maxval(r_GB(:,:,:,iBlock)) - rMin)/nR

    ! skip blocks with all DivuDx >= DivuDxMin
    if(all(PlotVar_GV(:,:,:,1) >= DivuDxMin)) RETURN

    do k = 1, nLat
       Lat = LatMin + (k-1)*dLat
       do j = 1, nLon
          Lon = LonMin + (j-1)*dLon
          do i = 1, nR
             r = rMin + (i-0.5)*dR
             if(r < rMinPlot .or. r > rMaxPlot) CYCLE
             ! Convert to Cartesian coordinates
             call rlonlat_to_xyz(r, Lon, Lat, XyzPlot_D)

             ! Convert to generalized coordinates
             call xyz_to_coord(XyzPlot_D, Coord_D)

             ! Normalize to block coordinates
             CoordNorm_D = &
                  (Coord_D - CoordMin_DB(:,iBlock))/CellSize_DB(:,iBlock) + 0.5

             ! Check if point is inside block
             if(any(CoordNorm_D < 0.4999)) CYCLE
             if(any(CoordNorm_D > nIjk_D + 0.5001)) CYCLE

             do iVar = 1, nPlotVar
                ! Interpolate up to ghost cells.
                ! compute the interpolated values at the current location
                PlotVar_V(iVar) = trilinear(PlotVar_GV(:,:,:,iVar),&
                     MinI, MaxI, MinJ, MaxJ, MinK, MaxK, CoordNorm_D)
             end do

             ! 0th plot variable is radius, and next plot variable is DivuDx
             if(PlotVar_V(1) < PlotVar_VII(1,j,k))then
                PlotVar_VII(0,j,k) = r
                PlotVar_VII(1:,j,k) = PlotVar_V
             endif
          end do ! r loop
       end do    ! lon loop
    end do       ! lat loop

    call test_stop(NameSub, DoTest, iBlock)

  end subroutine set_plot_shock
  !============================================================================
  subroutine write_plot_shock(iFile, nPlotVar, NameVar_V, NameUnit, NameFile)

    ! Collect results from all blocks and write to single output file.
    use ModMpi
    use ModMain,     ONLY: tSimulation, nStep
    use ModPlotFile, ONLY: save_plot_file

    integer,          intent(in) :: iFile, nPlotvar
    character(len=*), intent(in) :: NameFile, NameVar_V(nPlotVar), NameUnit

    integer :: iVar, iLon, iLat, iError
    character(len=500) :: NameVar

    real, allocatable :: PlotVarWeight_VII(:,:,:)
    real, allocatable :: DivuDx_II(:,:), DivuDxMin_II(:,:)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_plot_shock'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! This subroutine does not support HDF output.
    if(TypePlotFormat_I(iFile) == 'hdf') call stop_mpi(NameSub// &
         ': HDF file type not supported for Geo Sphere output.')

    ! Collect results to head node
    ! Allocate variable

    if(nProc > 1)then
       allocate(DivuDx_II(nLon,nLat), DivuDxMin_II(nLon,nLat), &
            PlotVarWeight_VII(0:nPlotVar+1,nLon,nLat))
       PlotVarWeight_VII = 0.0

       ! Find smallest DivuDx for each lon-lat index
       DivuDx_II = PlotVar_VII(1,:,:)
       call MPI_allreduce(DivuDx_II, DivuDxMin_II, nLon*nLat, MPI_REAL, &
            MPI_MIN, &
            iComm, iError)

       ! Assign weight 1 to the plot variables at the minimum value
       do iLat = 1, nLat; do iLon = 1, nLon
          if(DivuDxMin_II(iLon,iLat) == DivuDx_II(iLon,iLat))then
             PlotVarWeight_VII(0:nPlotVar,iLon,iLat) = PlotVar_VII(:,iLon,iLat)
             PlotVarWeight_VII(nPlotVar+1,iLon,iLat) = 1.0
          end if
       end do; end do

       ! Add up weighted plot variables and the weights
       call MPI_reduce_real_array(PlotVarWeight_VII, size(PlotVarWeight_VII), &
            MPI_SUM, 0, iComm, iError)

       if(iProc ==0) then
          do iLat = 1, nLat; do iLon = 1, nLon
             ! Divide by total weight (usually 1)
             PlotVar_VII(:,iLon,iLat) = &
                  PlotVarWeight_VII(0:nPlotVar,iLon,iLat) &
                  /PlotVarWeight_VII(nPlotVar+1,iLon,iLat)
          enddo; enddo
       endif
       deallocate(PlotVarWeight_VII, DivuDx_II, DivuDxMin_II)
    endif

    ! Save results to disk
    if(iProc==0) then
       ! zero out all variables if DivuDx > DivuDxMin
       do iLat = 1, nLat; do iLon = 1, nLon
          if(PlotVar_VII(1,iLon,iLat) > DivuDxMin) &
               PlotVar_VII(:,iLon,iLat) = 0.0
       enddo; enddo

       ! Build a single-line list of variable names.
       NameVar = 'lon lat r'
       do iVar = 1, nPlotVar
          NameVar = trim(NameVar)  // ' ' // trim(NameVar_V(iVar))
       end do

       ! Call save_plot_file to write data to disk.
       call save_plot_file(NameFile, &
            TypeFileIn=TypeFile_I(iFile), &
            StringHeaderIn=NameUnit, &
            nStepIn=nStep, &
            TimeIn=tSimulation, &
            NameVarIn = NameVar, &
            CoordMinIn_D = [cRadtoDeg*LonMin, cRadtoDeg*LatMin], &
            CoordMaxIn_D = [cRadtoDeg*LonMax, cRadtoDeg*LatMax], &
            VarIn_VII = PlotVar_VII)
    end if

    ! Deallocate results arrays:.
    deallocate(PlotVar_VII)

    call test_stop(NameSub, DoTest)

  end subroutine write_plot_shock
  !============================================================================
end module ModPlotShock
!==============================================================================
