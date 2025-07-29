!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModPlotShell

  use BATL_lib, ONLY: &
       test_start, test_stop, iProc, nProc, iComm
  use ModBatsrusUtility, ONLY: stop_mpi

  use ModIO
  use ModNumConst,        ONLY: cRadtoDeg, cDegToRad
  implicit none

  SAVE

  private   ! except
  public:: init_plot_shell
  public:: set_plot_shell
  public:: write_plot_shell

  ! Size of current plot:
  integer :: nR, nLon, nLat

  ! Ranges for current plot:
  real :: dR, dLat, dLon
  real :: rMin, rMax, LonMin, LonMax, LatMin, LatMax
  integer, parameter :: RadiusTransformLinear_ = 1
  integer, parameter :: RadiusTransformLog_ = 2
  integer, parameter :: RadiusTransformLog10_ = 3
  integer :: iRadiusTransform = RadiusTransformLinear_

  ! Local results container:
  ! Array of values written to file:
  real, allocatable :: PlotVar_VIII(:,:,:,:)
  ! Same, but for a single grid point
  real :: PlotVar_V(MaxPlotvar)

  ! Coordinate conversion matrix
  real:: PlotToGm_DD(3,3)

  character (len=20) :: NamePlotVar_V(MaxPlotvar) = ''

contains
  !============================================================================
  subroutine init_plot_shell(iFile)

    ! set up the shell grid for this plot file

    use ModMain,           ONLY: tSimulation, TypeCoordSystem
    use CON_axes,          ONLY: transform_matrix
    use ModCoordTransform, ONLY: show_rot_matrix
    use ModUtilities,      ONLY: split_string

    integer, intent(in):: iFile

    integer :: nPlotVar

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_plot_shell'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Allocate results array and set up all the spherical shell
    if(allocated(PlotVar_VIII)) RETURN

    StringPlotVar = StringPlotVar_I(iFile)
    call split_string(StringPlotVar, MaxPlotvar, NamePlotVar_V, nPlotVar, &
         UseArraySyntaxIn=.true.)
    if(index(TypePlot_I(iFile),'shl')>0) then
       iRadiusTransform = RadiusTransformLinear_
    elseif(index(TypePlot_I(iFile),'sln')>0) then
       iRadiusTransform = RadiusTransformLog_
    elseif(index(TypePlot_I(iFile),'slg')>0) then
       iRadiusTransform = RadiusTransformLog10_
    endif

    ! Get plot area info from ModIO arrays:
    dR     = abs(PlotDx_DI(1, iFile))
    dLon   = PlotDx_DI(2,iFile) * cDegtoRad
    dLat   = PlotDx_DI(3,iFile) * cDegtoRad
    rMin   = PlotRange_EI(1,iFile)
    rMax   = PlotRange_EI(2,iFile)
    LonMin = PlotRange_EI(3,iFile) * cDegtoRad
    LonMax = PlotRange_EI(4,iFile) * cDegtoRad
    LatMin = PlotRange_EI(5,iFile) * cDegtoRad
    LatMax = PlotRange_EI(6,iFile) * cDegtoRad
    if(iRadiusTransform == RadiusTransformLog_) then
       dR   = log((rMin + dR)/rMin)
       rMin = log(rMin)
       rMax = log(rMax)
    elseif(iRadiusTransform == RadiusTransformLog10_) then
       dR   = log10((rMin + dR)/rMin)
       rMin = log10(rMin)
       rMax = log10(rMax)
    endif

    ! Set number of points:
    nR   = nint((rMax - rMin)/dR)       + 1
    nLon = nint((LonMax - LonMin)/dLon) + 1
    nLat = nint((LatMax - LatMin)/dLat) + 1

    ! Ensure dR, dLon and dLat are compatible with the ranges
    dR   = (rMax - rMin)/max(1, nR - 1)
    dLon = (LonMax - LonMin)/max(1, nLon - 1)
    dLat = (LatMax - LatMin)/max(1, nLat - 1)

    ! The 0 element is to count the number of blocks that
    ! contribute to a plot variable.
    allocate(PlotVar_VIII(0:nPlotVar,nR,nLon,nLat))
    PlotVar_VIII = 0.0

    ! Get coordinate transformation matrix:
    PlotToGm_DD = transform_matrix(tSimulation, &
         TypeCoordPlot_I(iFile), TypeCoordSystem)

    if (DoTest) then
       write(*,*) NameSub//' iFile, nPlotVar=      ', iFile, nPlotVar
       write(*,*) NameSub//' Raw PlotDx_DI=          ', PlotDx_DI(:,iFile)
       write(*,*) NameSub//' Raw PlotRange_EI=       ', PlotRange_EI(:,iFile)
       write(*,*) NameSub//' dR, dLon, dLat =      ', dR, dLon, dLat
       write(*,*) NameSub//' r, Lon, Lat range = ',  &
            rMin, rMax, LonMin,LonMax,LatMin,LatMax
       write(*,*) NameSub,' nR, nLon, nLat, dR, dLon, dLat = ', &
            nR, nLon, nLat, dR, dLon, dLat
       write(*,*) NameSub,' PlotToGm_DD:'
       call show_rot_matrix(PlotToGm_DD)
    end if

    call test_stop(NameSub, DoTest)
  end subroutine init_plot_shell
  !============================================================================
  subroutine set_plot_shell(iBlock, nPlotvar, Plotvar_GV)
    ! Interpolate the plot variables for block iBlock
    ! onto the spherical shell of the plot area.
    use ModFieldLineThread, ONLY: DoPlotThreads, rChromo, &
         interpolate_thread_state, set_thread_plotvar
    use ModGeometry,    ONLY: rMin_B
    use ModInterpolate, ONLY: trilinear
    use BATL_lib,       ONLY: CoordMin_DB, nIjk_D, CellSize_DB, &
         xyz_to_coord, r_
    use ModCoordTransform, ONLY: rlonlat_to_xyz
    use ModParallel,       ONLY: DiLevel_EB, Unset_

    ! Arguments
    integer, intent(in) :: iBlock
    integer, intent(in) :: nPlotvar
    real,    intent(in) :: PlotVar_GV(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,nPlotVar)

    ! Local variables
    integer :: i, j, k, iVar, iDirMin

    real :: r, Lon, Lat
    real :: XyzPlot_D(3), XyzGm_D(3)
    real :: Coord_D(3), CoordNorm_D(3)

    ! State at the grid point in the threaded gap
    real :: State_V(nVar)

    ! If .true., this block includes grid points in the threaded gap
    logical :: IsThreadedBlock

    ! Check testing for block
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_plot_shell'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! Does this block include grid points in the threaded gap?
    IsThreadedBlock = DoPlotThreads .and. DiLevel_EB(1,iBlock) == Unset_
    if(IsThreadedBlock)then
       ! Don't check radial coordinate to see if the point is outside the block
       iDirMin = r_ + 1
    else
       iDirMin = 1
    end if

    ! Loop through shell points and interpolate PlotVar
    do i = 1, nR
       r = rMin + (i-1)*dR
       if(iRadiusTransform == RadiusTransformLog_)then
          r = exp(r)
       elseif(iRadiusTransform == RadiusTransformLog10_)then
          r = 10**r
       endif

       if(IsThreadedBlock)then
          ! The point should be skipped, if below the chromosphere
          if(r < rChromo) CYCLE
       else
          ! The point should be skipped, if outside the block
          if(r < rMin_B(iBlock)) CYCLE
       end if
       ! if(r > rMax_B(iBlock)) CYCLE

       do j = 1, nLon
          Lon = LonMin + (j-1)*dLon
          do k = 1, nLat
             Lat = LatMin + (k-1)*dLat

             ! Convert to Cartesian coordinates
             call rlonlat_to_xyz(r, Lon, Lat, XyzPlot_D)

             ! Convert from plot coordinates to BATSRUS grid:
             XyzGm_D = matmul(PlotToGm_DD, XyzPlot_D)

             ! Convert to generalized coordinates
             call xyz_to_coord(XyzGm_D, Coord_D)

             ! Normalize to block coordinates
             CoordNorm_D = &
                  (Coord_D - CoordMin_DB(:,iBlock))/CellSize_DB(:,iBlock) + 0.5

             ! Check if point is inside block
             if(any(CoordNorm_D(iDirMin:) < 0.4999)) CYCLE
             if(any(CoordNorm_D > nIjk_D + 0.5001)) CYCLE

             ! compute the interpolated values at the current location
             PlotVar_VIII(0,i,j,k) = 1.0
             if(IsThreadedBlock .and. Coord_D(1) < &
                  CoordMin_DB(1,iBlock) + 0.5*CellSize_DB(1,iBlock))then
                ! The threaded gap is used and the point is below
                ! the first layer of grid cell centers
                ! Interpolate using the solution on threads
                call interpolate_thread_state(Coord_D, iBlock, State_V)
                call set_thread_plotvar(iBlock, nPlotVar, NamePlotVar_V(&
                     1:nPlotVar), XyzGm_D, State_V, PlotVar_V(1:nPlotVar))
                PlotVar_VIII(1:,i,j,k) = PlotVar_V(1:nPlotVar)*&
                     DimFactor_V(1:nPlotVar)
             else
                do iVar=1, nPlotVar
                   ! Interpolate up to ghost cells.
                   PlotVar_VIII(iVar,i,j,k) = &
                        trilinear(PlotVar_GV(:,:,:,iVar),&
                        MinI, MaxI, MinJ, MaxJ, MinK, MaxK, CoordNorm_D)
                end do
             end if

          end do ! lon loop
       end do    ! lat loop
    end do       ! r loop

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine set_plot_shell
  !============================================================================
  subroutine write_plot_shell(iFile, nPlotVar, NameVar_V, &
       NameUnit, NameFile)

    ! Collect results from all blocks and write to single output file.
    use ModMpi
    use ModMain,     ONLY: tSimulation, nStep
    use ModPlotFile, ONLY: save_plot_file

    integer,          intent(in) :: iFile, nPlotvar
    character(len=*), intent(in) :: NameFile, NameVar_V(nPlotVar), NameUnit

    integer :: iVar, iR, iLon, iLat, iError
    character(len=500) :: NameVar

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_plot_shell'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! This subroutine does not support HDF output.
    if(TypePlotFormat_I(iFile) == 'hdf') call stop_mpi(NameSub// &
         ': HDF file type not supported for Geo Sphere output.')

    ! Collect results to head node
    if(nProc > 1) call MPI_reduce_real_array(PlotVar_VIII, size(PlotVar_VIII),&
         MPI_SUM, 0, iComm, iError)

    ! Save results to disk
    if(iProc==0) then
       ! Build a single-line list of variable names.
       if(nR==1)then
          NameVar = 'lon lat'
       elseif(nLon==1)then
          NameVar = 'r lat'
       elseif(nLat==1)then
          NameVar = 'r lon'
       else
          NameVar = 'r lon lat'
       end if
       if(NameVar(1:1)=='r') then
          if(iRadiusTransform == RadiusTransformLog_) then
             NameVar='ln'//trim(NameVar)
          elseif(iRadiusTransform == RadiusTransformLog10_) then
             NameVar='log'//trim(NameVar)
          endif
       endif
       do iVar=1, nPlotVar
          NameVar = trim(NameVar)  // ' ' // trim(NameVar_V(iVar))
       end do

       do iR = 1, nR; do iLon = 1, nLon; do iLat = 1, nLat
          if(PlotVar_VIII(0,iR,iLon,iLat) <= 1.0) CYCLE
          PlotVar_VIII(1:nPlotVar,iR,iLon,iLat) = &
               PlotVar_VIII(1:nPlotVar,iR,iLon,iLat) &
               / PlotVar_VIII(0,iR,iLon,iLat)
       end do; end do; end do

       ! Call save_plot_file to write data to disk
       if(nR == 1)then
          call save_plot_file(NameFile, &
               TypeFileIn=TypeFile_I(iFile), &
               StringHeaderIn=NameUnit, &
               nStepIn=nStep, &
               TimeIn=tSimulation, &
               NameVarIn = NameVar, &
               CoordMinIn_D = [cRadtoDeg*LonMin, cRadtoDeg*LatMin], &
               CoordMaxIn_D = [cRadtoDeg*LonMax, cRadtoDeg*LatMax], &
               VarIn_VII = PlotVar_VIII(1:,1,:,:))
       elseif(nLon == 1)then
          call save_plot_file(NameFile, &
               TypeFileIn=TypeFile_I(iFile), &
               StringHeaderIn=NameUnit, &
               nStepIn=nStep, &
               TimeIn=tSimulation, &
               NameVarIn = NameVar, &
               CoordMinIn_D = [rMin, cRadtoDeg*LatMin], &
               CoordMaxIn_D = [rMax, cRadtoDeg*LatMax], &
               VarIn_VII = PlotVar_VIII(1:,:,1,:))
       elseif(nLat == 1)then
          call save_plot_file(NameFile, &
               TypeFileIn=TypeFile_I(iFile), &
               StringHeaderIn=NameUnit, &
               nStepIn=nStep, &
               TimeIn=tSimulation, &
               NameVarIn = NameVar, &
               CoordMinIn_D = [rMin, cRadtoDeg*LonMin], &
               CoordMaxIn_D = [rMax, cRadtoDeg*LonMax], &
               VarIn_VII = PlotVar_VIII(1:,:,:,1))
       else
          call save_plot_file(NameFile, &
               TypeFileIn=TypeFile_I(iFile), &
               StringHeaderIn=NameUnit, &
               nStepIn=nStep, &
               TimeIn=tSimulation, &
               NameVarIn = NameVar, &
               CoordMinIn_D = [rMin, cRadtoDeg*LonMin, cRadtoDeg*LatMin], &
               CoordMaxIn_D = [rMax, cRadtoDeg*LonMax, cRadtoDeg*LatMax], &
               VarIn_VIII = PlotVar_VIII(1:,:,:,:))
       end if
    end if

    ! Deallocate results arrays:.
    deallocate(PlotVar_VIII)

    call test_stop(NameSub, DoTest)
  end subroutine write_plot_shell
  !============================================================================
end module ModPlotShell
!==============================================================================
