!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

! This is a modified version of the line-of-sight (LOS) segment calculation.
! See ModWritePlot for the original code, written by Chip Manchester, KC Hansen;
! and improved by Gabor Toth, Noe Lugaz, Cooper Downs. The code integrates 
! along several lines of sight and create a 2D image of the integrated quantities.
! This modified version only outputs the LOS segments (calculated from spherical
! grids), so that the Spectrum code can do optimized parallelization to
! integrate spectral lines.

module ModSpectrumLos
  use BATL_lib, ONLY: test_start, test_stop, iProc, nProc, iComm
  use ModVarIndexes, ONLY: nVar
  use ModMpi

  implicit none
  private
  save

  public:: init_los_spectrum
  public:: get_los_data_cube


  ! here simple as: solar radius, outmost radius to calculate
  real :: rInner, rOuter
  ! location of the observer in Batsrus (SC) coordiantes
  real :: ObsPos_D(3), ObsDistance
  ! LOS unit vector towards the observer
  real :: LosUnit_D(3)
  ! rotation matrix to transform vectors from Batsrus to image coordinates
  ! after rotation x-axis points towards observer, y -> a, z -> b
  real :: RotBatsLos_DD(3,3)


  integer :: nPixelA, nPixelB    ! pixels in image a and b directions
  real :: PixelSizeA, PixelSizeB    ! size of pixel in solar radii unit
  real :: ImageCenter_D(3)   ! center of the image
  real :: ImageA_D(3), ImageB_D(3)   ! unit vector of a, b direction of image

  integer :: nPixelProc     ! number of image pixels to store in iProc
  integer :: MaxLosSeg   ! maximum number of LOS segments; dynamic allocated
  integer :: nLosSegInc   ! increment of array size if nLOS is too small

  ! integer, pointer :: iPixelProcInfo_I(:,:)

  integer, public :: LosX_ = nVar+1, LosY_ = nVar+2, LosZ_ = nVar+3, Ds_ = nVar+4


contains

  ! Initialize this module. Setup variables and geometric transformations.
  ! Information of image and observer should be already calculated elsewhere.
  ! rInner: radius of the Sun, or lower cutoff for calculation
  ! rOuter: radius of the domain, or infinity, or upper cutoff (maybe 3 Rs)
  ! ImageCenter: center of the image in Rs units, as viewed from observer
  subroutine init_los_spectrum(rInnerIn, rOuterIn, nPixelIn_I, &
      ImageCenterIn_I, ImageSizeIn_I, ObsPosIn_D, PixelPosA_I, PixelPosB_I, &
      nPixelProcOut, iPixelProcInfo_II, nLosSeg_I, StatePixelSegProc_VII)
    use ModUtilities, ONLY: norm2
    use ModCoordTransform, ONLY: cross_product
    use ModConst, ONLY: cTiny

    real, intent(in) :: rInnerIn, rOuterIn
    integer, intent(in) :: nPixelIn_I(2)
    real, intent(in) :: ImageCenterIn_I(2), ImageSizeIn_I(2), ObsPosIn_D(3)
    real, intent(out) :: PixelPosA_I(nPixelIn_I(1)), PixelPosB_I(nPixelIn_I(2))
    integer, intent(out) :: nPixelProcOut
    integer, intent(inout) :: iPixelProcInfo_II(2,nPixelIn_I(1)*nPixelIn_I(2))
    integer, allocatable, intent(inout) :: nLosSeg_I(:)
    ! The interpolated state vector for each processor. It contains:
    ! [number of variables, number of pixels, number of LOS segments].
    real, allocatable, intent(inout) :: StatePixelSegProc_VII(:,:,:)

    integer :: i, nPixelProc_P(0:nProc-1), jProc
    real :: PixelPos, PixelCenterA, PixelCenterB

    ! initialize vectors and geometric stuff within this module
    rInner = rInnerIn
    rOuter = rOuterIn
    nPixelA = nPixelIn_I(1)
    nPixelB = nPixelIn_I(2)

    ObsPos_D = ObsPosIn_D
    where(ObsPos_D == 0.0) ObsPos_D = cTiny
    ObsDistance = norm2(ObsPos_D)
    LosUnit_D = ObsPos_D/ObsDistance

    PixelSizeA = ImageSizeIn_I(1)/nPixelA
    PixelSizeB = ImageSizeIn_I(2)/nPixelB
    PixelCenterA = (nPixelA+1.)/2.
    PixelCenterB = (nPixelB+1.)/2.

    do i = 1, nPixelA
      PixelPos = (i - PixelCenterA) * PixelSizeA
      PixelPosA_I(i) = PixelPos + ImageCenterIn_I(1)
    enddo
    do i = 1, nPixelB
      PixelPos = (i - PixelCenterB) * PixelSizeB
      PixelPosB_I(i) = PixelPos + ImageCenterIn_I(2)
    enddo

    ! Unit vectors ImageA_D and ImageB_D orthogonal to the LOS to setup 
    ! coordinate system in the image plane. In case the observer is closer to 
    ! equtorial plane, ImageB_D points roughly along +Z.
    if (abs(LosUnit_D(3)) < maxval(abs(LosUnit_D(1:2)))) then
      ImageA_D = cross_product([0.,0.,1.], LosUnit_D)
    else
      ! For viewing from above north/south pole, ImageB_D is roughly along +Y.
      ImageA_D = cross_product([0.,1.,0.], LosUnit_D)
    end if
    ImageA_D = ImageA_D/norm2(ImageA_D)
    ImageB_D = cross_product(LosUnit_D, ImageA_D)
    ! ImageB_D = ImageB_D/norm2(ImageB_D)

    ! Possibly add image tilt/rotation here, just need to rotate A and B axis

    ! 3D vector pointing from the origin to the image center
    ! Here I assume image plane always pass the origin
    ImageCenter_D = ImageCenterIn_I(1)*ImageA_D + ImageCenterIn_I(2)*ImageB_D

    ! get the rotation matrix to transform magnetic and velocity field
    ! LosUnit -> x', ImageA -> y', ImageB -> z'
    RotBatsLos_DD(1,:) = LosUnit_D
    RotBatsLos_DD(2,:) = ImageA_D
    RotBatsLos_DD(3,:) = ImageB_D

    ! distribute pixels to processors
    nPixelProc_P = 0
    do i = 1, nPixelA*nPixelB
      jProc = mod(i-1,nProc)
      iPixelProcInfo_II(1,i) = jProc
      nPixelProc_P(jProc) = nPixelProc_P(jProc) + 1
      iPixelProcInfo_II(2,i) = nPixelProc_P(jProc)
    enddo
    nPixelProc = nPixelProc_P(iProc)
    nPixelProcOut = nPixelProc

    ! allocate array for each processor to store state variables
    MaxLosSeg = 100
    nLosSegInc = 20
    if (allocated(nLosSeg_I)) deallocate(nLosSeg_I)
    allocate(nLosSeg_I(nPixelProc))
    nLosSeg_I = 0
    if (allocated(StatePixelSegProc_VII)) deallocate(StatePixelSegProc_VII)
    allocate(StatePixelSegProc_VII(Ds_,MaxLosSeg,nPixelProc))

    ! initialize to 0
    StatePixelSegProc_VII = 0.0
    
  end subroutine init_los_spectrum

  !============================================================================
  subroutine get_los_data_cube(iPixelProcInfo_II, StatePixelSegProc_VII, nLosSeg_I, State_VGB)
    use ModNumConst, ONLY : cTiny, cHuge
    use ModCoordTransform, ONLY : cross_product

    integer, intent(in) :: iPixelProcInfo_II(:,:)
    real, allocatable, intent(inout) :: StatePixelSegProc_VII(:,:,:)
    integer, intent(inout) :: nLosSeg_I(:)
    real, intent(in) :: State_VGB(:,:,:,:,:)

    integer :: iError
    integer :: iPix, jPix             ! indexes of the pixel 
    integer :: iPixelTotal, iProcRecv, iPixelProcLoc
    real :: PixelCenterA, PixelCenterB, PixelPosA, PixelPosB
    real :: PixelPos_D(3), IntersectPos_D(3)
    real :: PixPosDotPixLos, PixPos2, Discriminant, IntersectD

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_los_data_cube'
    !--------------------------------------------------------------------------

    call test_start(NameSub, DoTest)
    call timing_start(NameSub)

    PixelCenterA = (nPixelA+1.)/2.
    PixelCenterB = (nPixelB+1.)/2.

    iPixelTotal = 0
    do jPix = 1, nPixelB
      PixelPosB = (jPix - PixelCenterB) * PixelSizeB
      do iPix = 1, nPixelA
        PixelPosA = (iPix - PixelCenterA) * PixelSizeA

        iPixelTotal = iPixelTotal + 1
        iProcRecv = iPixelProcInfo_II(1,iPixelTotal)
        iPixelProcLoc = iPixelProcInfo_II(2,iPixelTotal)

        ! Get the 3D location of the pixel on image plane
        PixelPos_D = ImageCenter_D + PixelPosA*ImageA_D + PixelPosB*ImageB_D

        ! This part considers large viewing angles from image center
        ! ! get unit vector from pixel center to observer
        ! Pix2LosUnit_D = ObsPos_D - PixelPos_D
        ! ObsDistance = norm2(Pix2LosUnit_D)
        ! Pix2LosUnit_D = Pix2LosUnit_D/ObsDistance

        ! Calculate whether there are intersections with the rInner sphere
        ! If LOS line, XyzLine_D = PixelPos_D + d*LosUnit_D, intersects 
        ! with the sphere of radius rInner+cTiny, then
        ! (rInner+cTiny)^2 = (PixelPos_D + IntersectD*LosUnit_D)^2
        ! d^2 + 2*(PixelPos.Pix2Los) * d + PixelPos^2-(rInner+cTiny)^2 = 0
        ! Solve and only the positive root is needed.
        ! If intersect, starting point: PixelPos_D + IntersectD * LosUnit_D

        ! The discriminant of the equation
        PixPosDotPixLos = dot_product(LosUnit_D,PixelPos_D) 
        PixPos2 = sum(PixelPos_D**2)
        Discriminant = PixPosDotPixLos**2 - PixPos2  + (rInner + cTiny)**2

        if (DoTest) then
          write(*,'(A,I0,2(A,I0),A,I0)') '----------', iPixelTotal, '  i:', iPix, ' j:', jPix, '  recv:', iProcRecv
          write(*,'(A12,4A8,A2,5A10,A2,7A10)') 'StartLen','blk','size1','size2','size3','','step','ds','move1','move2','move3','','coordI','coordJ','coordK','ds','move1','move2','move3'
        endif

        if (Discriminant > 0) then
          IntersectD = - PixPosDotPixLos + sqrt(Discriminant) 
          IntersectPos_D = PixelPos_D + IntersectD*LosUnit_D
          call get_los_segments(IntersectPos_D, LosUnit_D, ObsDistance-IntersectD, &
              iProcRecv, iPixelProcLoc, State_VGB, StatePixelSegProc_VII, nLosSeg_I, &
              DoTestIn=DoTest)
        else
          call get_los_segments(PixelPos_D, LosUnit_D, ObsDistance, &
              iProcRecv, iPixelProcLoc, State_VGB, StatePixelSegProc_VII, nLosSeg_I, &
              DoTestIn=DoTest)
          call get_los_segments(PixelPos_D, -LosUnit_D, cHuge, &
              iProcRecv, iPixelProcLoc, State_VGB, StatePixelSegProc_VII, nLosSeg_I, &
              DoTestIn=DoTest)
        end if

      end do ! iPix loop
    end do    ! jPix loop

    call timing_stop(NameSub)
    call test_stop(NameSub, DoTest)

  end subroutine get_los_data_cube


  ! Integrate variables from XyzStartIn_D in the direction ObsDirUnit_D
  ! LengthMax: maximum length along LOS for integration. For Spectrum it is
  !     not very likely LengthMax < rOuter, but include it here just in case 
  !     future mission has spacecraft inside corona (solar orbiter?)
  subroutine get_los_segments(XyzStartIn_D, ObsDirUnit_D, LengthMax, &
      iProcRecv, iPixelProcLoc, State_VGB, StatePixelSegProc_VII, &
      nLosSeg_I, DoTestIn)
    use ModGeometry, ONLY: x1, x2, y1, y2, z1, z2
    use BATL_size,ONLY: nDim, nIJK_D, MaxDim, MinIJK_D, MaxIJK_D
    use BATL_geometry, ONLY: r_
    use BATL_lib, ONLY: xyz_to_coord, find_grid_block, get_tree_position, &
        CoordMin_D, CoordMax_D
    use ModVarIndexes, ONLY: Bx_, Bz_, RhoUx_, RhoUz_
    use ModInterpolate, ONLY: interpolate_vector
    use ModConst, ONLY: cTiny

    real, intent(in) :: XyzStartIn_D(3), ObsDirUnit_D(3), LengthMax
    integer, intent(in) :: iProcRecv, iPixelProcLoc
    real, intent(in) :: State_VGB(:,:,:,:,:)
    real, allocatable, intent(inout) :: StatePixelSegProc_VII(:,:,:)
    integer, intent(inout) :: nLosSeg_I(:)
    logical, intent(in), optional :: DoTestIn

    ! prefered los segment sizes
    ! real, parameter:: StepMax = 1., StepMin = 0.5, StepGood = 0.75
    real, parameter:: StepMax = 0.5, StepMin = 0.2, StepGood = 0.35

    real :: Length     ! Total length of integral
    real :: Ds, Step, DsTiny       ! Length of line segment
    integer:: iNode, iDimMin = 1, nStateVar, nSizeVar
    real, dimension(MaxDim):: DomainSize_D, PositionMin_D, PositionMax_D, &
        CoordBlockMin_D, CoordBlockMax_D, CoordBlockCntr_D, CoordBlockSize_D, &
        CellSize_D, XyzLos_D, CoordLos_D, XyzLosNew_D, CoordLosNew_D, &
        CoordNorm_D, dCoord_D
    logical:: DoTest, IsEdge, IsCycle, DoAdjStep
    integer :: iBlock, iProcSend, iStatus_I(mpi_status_size), iError
    real, allocatable :: State_V(:), TmpStateArray_VII(:,:,:)

    integer :: i, iCell_D(3)

    character(len=*), parameter:: NameSub = 'get_los_segments'
    !------------------------------------------------------------------------
    call timing_start(NameSub)

    if (present(DoTestIn)) then
      DoTest = DoTestIn
    else
      DoTest = .false.
    endif

    if (allocated(State_V)) deallocate(State_V)
    nStateVar = nVar
    nSizeVar = Ds_
    allocate(State_V(nSizeVar))


    iDimMin = r_

    DomainSize_D = CoordMax_D - CoordMin_D
    DsTiny = cTiny

    ! Initial length of segment
    Ds = DsTiny

    ! Initialize "new" position as the starting point
    XyzLosNew_D = XyzStartIn_D
    call xyz_to_coord(XyzLosNew_D, CoordLosNew_D)

    ! Initialize block boundaries so that point is surely outside
    CoordBlockMin_D = CoordMax_D
    CoordBlockMax_D = CoordMin_D

    Length = - Ds
    LOOPLINE: do
      if (Ds <= 0.0) then
        write(*,*)'ds=', Ds
        call stop_mpi(NameSub//': Algorithm failed: zero integration step')
      end if

      ! Total length integrated so far
      Length  = Length + Ds
      if(DoTest) write(*,'(F12.5)',advance='no') Length

      ! Stop if reached maximum length
      if(Length >= LengthMax) EXIT LOOPLINE
      ! Stop integration if we reached the edge of the domain
      if ( any(CoordLosNew_D > CoordMax_D) .or. &
          any(CoordLosNew_D < CoordMin_D)) EXIT LOOPLINE
      if (norm2(XyzLosNew_D) > rOuter) EXIT LOOPLINE

      ! Move to new position
      XyzLos_D   = XyzLosNew_D
      CoordLos_D = CoordLosNew_D

      ! Check if we are still in the same block or not
      if( any(CoordLos_D(iDimMin:) < CoordBlockMin_D(iDimMin:)) .or. &
          any(CoordLos_D(iDimMin:) > CoordBlockMax_D(iDimMin:)))then
        ! Find new block/node, increase the radial coordinate to 
        ! put the point above the inner boundary
        call find_grid_block(XyzLos_D, iProcSend, iBlock, iNodeOut=iNode)
        ! Set block coordinates and the cell size on all processors
        call get_tree_position(iNode, PositionMin_D, PositionMax_D)
        CoordBlockMin_D = CoordMin_D + DomainSize_D*PositionMin_D  ! Start
        CoordBlockMax_D = CoordMin_D + DomainSize_D*PositionMax_D  ! End
        CoordBlockCntr_D = 0.5*(CoordBlockMax_D + CoordBlockMin_D) ! Center
        CoordBlockSize_D = CoordBlockMax_D - CoordBlockMin_D    ! Block size
        CellSize_D      = CoordBlockSize_D / nIJK_D            ! Cell size
        if (DoTest) then
         write(*,'(I8,3F8.3)',advance='no') iBlock, CellSize_D
        end if
      elseif(DoTest) then
        write(*,'(A32)',advance='no') ''
      endif

      ! Ds = 0.0024
      call adjust_segment_length
      if (DoTest) then
        if (IsEdge) then
          write(*,'(A2)',advance='no') 'e'
        else
          write(*,'(A2)',advance='no') ''
        endif
      endif
      if (IsCycle) then
        if (DoTest) write(*,*) ' Cycle'
        CYCLE LOOPLINE
      endif

      ! Check how big the largest change is in the generalized coordinates
      Step = maxval(abs(CoordLosNew_D - CoordLos_D)/CellSize_D)
      DoAdjStep = .false.
      if (DoTest) write(*,'(5F10.5)',advance='no') Step, Ds, (CoordLosNew_D-CoordLos_D)/CellSize_D


      ! If change is too large or too small adjust the step size
      if (Step > StepMax .or. (Step < StepMin .and. .not. IsEdge)) then
        DoAdjStep = .true.
        ! New interval size corresponds to a StepGood
        ! in generalized coordinates instead of Step
        Ds = Ds*StepGood/Step
        call adjust_segment_length
        if (DoTest) then
          if (IsEdge) then
            write(*,'(A2)',advance='no') 'E'
          else
            write(*,'(A2)',advance='no') ''
          endif
        endif
        if (IsCycle) then
          if (DoTest) write(*,*) ' Cycle'
          CYCLE LOOPLINE
        endif
      else
        if (DoTest) write(*,'(A2)',advance='no') ''
      endif

      ! Now prepare to send interpolated variables to appropriate processor
      if (iProc == iProcSend) then
        ! call find_grid_block(XyzLosNew_D, iProcSend, iBlock, iCellOut_D=iCell_D)
        ! State_V(1:nVar) = State_VGB(:,iCell_D(1),iCell_D(2),iCell_D(3),iBlock)

        ! Get normalized coordinates (to cell index)
        CoordNorm_D = (CoordLosNew_D - CoordBlockMin_D)/CellSize_D + 0.5
        ! interpolate vector
        State_V(1:nStateVar) = interpolate_vector(State_VGB(:,:,:,:,iBlock), &
            nStateVar, nDim, MinIJK_D, MaxIJK_D, CoordNorm_D)

        ! Rotate magnetic and velocity field to LOS coordinates
        State_V(Bx_:Bz_) = matmul(RotBatsLos_DD,State_V(Bx_:Bz_))
        State_V(RhoUx_:RhoUz_) = matmul(RotBatsLos_DD,State_V(RhoUx_:RhoUz_))
        State_V(LosX_:LosZ_) = XyzLosNew_D
        State_V(Ds_) = Ds

        if (iProcSend /= iProcRecv) then
          call mpi_send(State_V, nSizeVar, MPI_REAL, iProcRecv, 0, iComm, iError)
        endif
      endif

      if (iProc == iProcRecv) then
        if (iProcRecv /= iProcSend) then
          call mpi_recv(State_V, nSizeVar, MPI_REAL, iProcSend, 0, iComm, iStatus_I, iError)
        endif
        nLosSeg_I(iPixelProcLoc) = nLosSeg_I(iPixelProcLoc) + 1
        if (nLosSeg_I(iPixelProcLoc) > MaxLosSeg) then
          if (allocated(TmpStateArray_VII)) deallocate(TmpStateArray_VII)
          allocate(TmpStateArray_VII(nSizeVar,MaxLosSeg+nLosSegInc,nPixelProc))
          TmpStateArray_VII(:,1:MaxLosSeg,:) = StatePixelSegProc_VII
          call move_alloc(TmpStateArray_VII, StatePixelSegProc_VII)
          MaxLosSeg = MaxLosSeg + nLosSegInc
        endif
        StatePixelSegProc_VII(:,nLosSeg_I(iPixelProcLoc),iPixelProcLoc) = State_V
      endif

      if (DoTest) then
        if (iProc==iProcSend) then
          write(*,'(3F10.3)',advance='no') CoordNorm_D
          if (DoAdjStep) write(*,'(4F10.5)',advance='no') Ds, (CoordLosNew_D-CoordLos_D)/CellSize_D
        endif
        write(*,*)
      endif

      ! Move XyzLosNew to the end of the segment
      XyzLosNew_D = XyzLos_D + Ds*ObsDirUnit_D
      call xyz_to_coord(XyzLosNew_D, CoordLosNew_D)

    end do LOOPLINE
    if (DoTest) write(*,*)
    
    call timing_stop(NameSub)

  contains
    subroutine adjust_segment_length
      IsEdge = .false.
      IsCycle = .false.
      do
        ! Move to the middle of the segment
        XyzLosNew_D = XyzLos_D + 0.5*Ds*ObsDirUnit_D
        call xyz_to_coord(XyzLosNew_D, CoordLosNew_D)

        ! Check if midpoint is inside block + 1 cell size
        dCoord_D = abs(CoordLosNew_D - CoordBlockCntr_D)
        if (all(2*dCoord_D(iDimMin:) <= CoordBlockSize_D(iDimMin:))) EXIT

        ! Reduce Ds but make sure that 2*Ds is still outside.
        Ds = Ds*0.5

        ! Don't integrate this segment if it is very small
        ! Since we took half of Ds, XyzLosNew is at the end
        if(Ds < DsTiny*0.01) then
          IsCycle = .true.
          EXIT
        else
          ! Make sure we don't try to increase the step below
          IsEdge = .true.
        endif
      end do
    end subroutine adjust_segment_length

  end subroutine get_los_segments
  !==========================================================================


end module ModSpectrumLos
