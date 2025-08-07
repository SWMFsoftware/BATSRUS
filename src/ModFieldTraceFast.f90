!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModFieldTraceFast

  use ModKind
  use ModFieldTrace
  use ModMain, ONLY: UseB0, TypeCoordSystem, tSimulation, nStep
  use ModB0, ONLY: B0_DGB, get_b0
  use ModAdvance, ONLY: State_VGB, Bx_, Bz_, iTypeUpdate, UpdateSlow_
  use BATL_lib, ONLY: &
       test_start, test_stop, StringTest, xTest, yTest, zTest, &
       iTest, jTest, kTest, iBlockTest, iProc, iComm, nProc, &
       nDim, nI, nJ, nK, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, &
       nBlock, MaxBlock, Unused_B, IsCartesianGrid, x_, y_, z_, &
       iNode_B, iTree_IA, Coord0_, Xyz_DGB, CellSize_DB, &
       message_pass_cell, message_pass_node
  use ModBatsrusUtility, ONLY: barrier_mpi, error_report, stop_mpi
  use BATL_size, ONLY: nGang
  use ModUtilities, ONLY: i_gang
#ifdef _OPENACC
  use ModUtilities, ONLY: norm2
#endif

  implicit none

  SAVE

  private ! except
  public:: trace_field_grid           ! trace field from 3D MHD grid cells
  public:: Trace_DSNB                 ! inherited from ModFieldTrace
  public:: calc_squash_factor         ! calculate squashing factor
  public:: SquashFactor_GB            ! squashing factor

  ! Local variables
  logical, parameter:: DoDebug = .false.

  integer, parameter :: nFaceMax=max(nI+1,nJ+1,nK+1)

  ! Trace_DINB contains the x,y,z coordinates for the foot point of a given
  ! field line for both directions, eg.
  ! Trace_DINB(2,1,i,j,k,iBlock) is the y coordinate for direction 1
  ! for node i,j,k of block iBlock.

  real, allocatable :: Trace_DINB(:,:,:,:,:,:)
  !$acc declare create(Trace_DINB)

  real, allocatable :: Trace_DIIII(:,:,:,:,:)
  !$acc declare create(Trace_DIIII)

  integer, allocatable :: IjkTrace_DIII(:,:,:,:)
  !$acc declare create(IjkTrace_DIII)

  ! Stored face and cell indices of the 2 traces starting from face of a block
  integer(Int1_), allocatable :: I_DINB(:,:,:,:,:,:)
  !$acc declare create(I_DINB)

  ! Stored weights for the 2 traces starting from a face of a block
  real, allocatable :: WeightTrace_DINB(:,:,:,:,:,:)
  !$acc declare create(WeightTrace_DINB)

  ! Node interpolated magnetic field components without B0
  real, allocatable :: b_DNB(:,:,:,:,:)
  !$acc declare create(b_DNB)

  ! Prefer open and closed field lines in interpolation ?!
  logical :: UsePreferredInterpolation
  !$acc declare create(UsePreferredInterpolation)

  ! Control volume limits in local coordinates
  real, parameter :: GenMin_D(3) = [   0.5,   0.5,     0.5]
  real, parameter :: GenMax_D(3) = [nI+0.5, nJ+0.5, nK+0.5]

  ! Testing
#ifdef _OPENACC
  logical, parameter :: DoTestTrace = .false.
#else
  logical :: DoTestTrace = .false.
#endif

contains
  !============================================================================
  subroutine init_mod_trace_fast
    !--------------------------------------------------------------------------
    if(.not.allocated(Trace_DINB)) &
         allocate(Trace_DINB(3,2,nI+1,nJ+1,nK+1,MaxBlock))
    if(.not.allocated(I_DINB)) &
         allocate(I_DINB(3,2,nI+1,nJ+1,nK+1,MaxBlock))
    if(.not.allocated(WeightTrace_DINB)) &
         allocate(WeightTrace_DINB(4,2,nI+1,nJ+1,nK+1,MaxBlock))
    if(.not.allocated(b_DNB)) &
         allocate(b_DNB(3,nI+1,nJ+1,nK+1,MaxBlock))
    if(.not.allocated(b_DGB)) &
         allocate(b_DGB(3,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
    if(.not.allocated(Trace_DSNB))then
       allocate(Trace_DSNB(3,2,nI+1,nJ+1,nK+1,MaxBlock))
       Trace_DSNB = 0.0
    end if
    if(.not.allocated(Trace_DIIII)) &
         allocate(Trace_DIIII(3,2,nFaceMax,nFaceMax,nGang))
    if(.not.allocated(IjkTrace_DIII)) &
         allocate(IjkTrace_DIII(2,nFaceMax,nFaceMax,nGang))
  end subroutine init_mod_trace_fast
  !============================================================================
  subroutine clean_mod_trace_fast
    !--------------------------------------------------------------------------
    if(allocated(Trace_DINB))       deallocate(Trace_DINB)
    if(allocated(I_DINB))           deallocate(I_DINB)
    if(allocated(WeightTrace_DINB)) deallocate(WeightTrace_DINB)
    if(allocated(b_DNB))            deallocate(b_DNB)
    if(allocated(b_DGB))            deallocate(b_DGB)
    if(allocated(Trace_DSNB))       deallocate(Trace_DSNB)
    if(allocated(Trace_DIIII))      deallocate(Trace_DIIII)
    if(allocated(IjkTrace_DIII))    deallocate(IjkTrace_DIII)
  end subroutine clean_mod_trace_fast
  !============================================================================
  subroutine trace_field_grid

    ! This parallel tracing algorithm was developed at the U.of M.
    ! by G. Toth and D. De Zeeuw. An overview of the scheme can be found in
    !
    ! D. L. De Zeeuw, S. Sazykin, R. A. Wolf, T. I. Gombosi,
    ! A. J. Ridley, G. T\'oth, 2004,
    ! Journal of Geophysical Research, 109, 12219,
    !
    ! Details of the algorithm are to be published later

    use ModMain,     ONLY: iNewGrid, iNewDecomposition, tSimulation
    use ModPhysics,  ONLY: set_dipole
    use CON_axes,    ONLY: transform_matrix
    use ModUpdateStateFast, ONLY: sync_cpu_gpu

    ! remember last call and the last grid number
    integer :: nStepLast=-1, iLastGrid=-1, iLastDecomposition=-1

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'trace_field_grid'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    call sync_cpu_gpu('update on CPU', NameSub, State_VGB, B0_DGB)

    call init_mod_trace_fast

    if(DoTest)then
       write(*,*)NameSub,': nStepLast,nStep       =',nStepLast,nStep
       write(*,*)NameSub,': iLastGrid,iNewGrid    =',iLastGrid,iNewGrid
       write(*,*)NameSub,': iLastDecomp,iNewDecomp=',&
            iLastDecomposition,iNewDecomposition
    end if

    if(  nStepLast + DnRaytrace > nStep   .and. &
         iLastGrid          == iNewGrid .and. &
         iLastDecomposition == iNewDecomposition) RETURN

    ! Remember this call
    nStepLast = nStep
    iLastGrid = iNewGrid
    iLastDecomposition = iNewDecomposition

    call timing_start(NameSub)

    call init_mod_field_trace

    call set_dipole

    ! Transformation matrix between the SM(G) and GM coordinates
    if(UseSmg) &
         GmSm_DD = transform_matrix(tSimulation,'SMG',TypeCoordSystem)
    !$acc update device(GmSm_DD)

    if(UseAccurateTrace .or. .not.IsCartesianGrid)then
       call trace_grid_accurate
       call sync_cpu_gpu('change on CPU', NameSub, Trace_DICB=Trace_DSNB)
    else
       call trace_grid_fast
       call sync_cpu_gpu('change on GPU', NameSub, Trace_DICB=Trace_DSNB)
    end if

    call timing_stop(NameSub)

    call test_stop(NameSub, DoTest)

  end subroutine trace_field_grid
  !============================================================================
  subroutine trace_grid_fast

    use ModParallel, ONLY: Unset_, DiLevel_EB
    use ModGeometry, ONLY: r_GB, rMin_B, Used_GB
    use ModMpi

    ! Iteration parameters
    integer, parameter :: MaxIterTrace = 150
    integer :: nIterTrace
    ! check for changes and loops in the iterative scheme
    logical :: DoneChange, DoneLoop, Done_I(2)

    real    :: dTraceMin
    !$acc declare create(nIterTrace)

    ! Small arrays to pass as arguments
    real :: Trace_DI(3,4)

    ! Minimum value of B for which integration of field lines makes any sense
    real, parameter :: SmallB = 1e-8

    ! True if rMin_B < rTrace
    logical :: DoCheckInside

    ! Face index for the final point of the Trace_DSNB
    integer :: iFace

    ! Current position of tracing in normalized and physical coordinates
    real :: Gen_D(3)

    ! Cell indices corresponding to current or final Gen_D position
    integer :: i1, j1, k1, i2, j2, k2

    ! Distance between Gen_D and i1,j1,k1, and i2,j2,k2

    ! Weights for surface interpolation
    real :: Weight_I(4)

    ! Cell indices
    integer :: i, j, k, i0, j0, k0, iCount

    ! Indices corresponding to the starting point of the trace
    integer :: iX, iY, iZ

    ! Current block and direction indices
    integer :: iBlock, iTrace, iDim

    ! Testing and timing
    logical, parameter :: DoTime = .false.
    integer :: Ijk_D(3)
    real:: GenIni_D(3)

    integer :: iError, iError1 = -1

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'trace_grid_fast'
    !--------------------------------------------------------------------------
    call timing_start(NameSub)

    call test_start(NameSub, DoTest)

    call timing_start('trace_grid_fast1')

    if(DoTime)call timing_reset('pass_trace',2)

    IsBVectorField = NameVectorField == 'B'
    !$acc update device(IsBVectorField)

#ifndef _OPENACC
    DoTestTrace = .false.
#endif

    !$acc parallel loop gang
    do iBlock = 1, nBlock
       !$acc loop vector collapse(3)
       do k = MinK, MaxK ; do j = MinJ, MaxJ; do i = MinI, MaxI
          b_DGB(:,i,j,k,iBlock) = State_VGB(Bx_:Bz_,i,j,k,iBlock)
       end do; end do; end do
    end do

    ! Fill in ghost cells
    call message_pass_cell(3, b_DGB, UseOpenACCIn=.true.)

    !$acc parallel loop gang
    do iBlock = 1, nBlock
       ! Q: Should we have i,j,k loops here?
       ! A: Since this kernel is already short and fast, replacing the
       !    following array operations with loops does not improve speed.
       if(Unused_B(iBlock))then
          ! Trace_DINB in unused blocks is assigned to NoRay-1.
          Trace_DINB(:,:,:,:,:,iBlock) = NoRay-1.
          CYCLE
       else
          ! Initial values !!! Maybe LoopRay would be better??
          Trace_DINB(:,:,:,:,:,iBlock) = NoRay
          Trace_DSNB(:,:,:,:,:,iBlock) = NoRay
       endif
       ! Inner points of Trace_DINB should never be used, assign them to OPEN
       ! so that checking for blocks with fully open traces becomes easy
       Trace_DINB(:,:,2:nI,2:nJ,2:nK,iBlock)=OPENRAY

       ! Set Trace_DINB=OPENRAY at outer boundaries
       if(DiLevel_EB(1,iBlock)==Unset_) &
            Trace_DINB(:,:,   1,:,:,iBlock) = OPENRAY
       if(DiLevel_EB(2,iBlock)==Unset_) &
            Trace_DINB(:,:,nI+1,:,:,iBlock) = OPENRAY
       if(DiLevel_EB(3,iBlock)==Unset_) &
            Trace_DINB(:,:,:,   1,:,iBlock) = OPENRAY
       if(DiLevel_EB(4,iBlock)==Unset_) &
            Trace_DINB(:,:,:,nJ+1,:,iBlock) = OPENRAY
       if(DiLevel_EB(5,iBlock)==Unset_) &
            Trace_DINB(:,:,:,:,   1,iBlock) = OPENRAY
       if(DiLevel_EB(6,iBlock)==Unset_) &
            Trace_DINB(:,:,:,:,nK+1,iBlock) = OPENRAY
    end do

#ifndef _OPENACC
    if(DoTest)write(*,*)NameSub,' initialized Trace_DSNB and Trace_DINB arrays'
#endif

    ! Interpolate the B1 field to the nodes
    !$acc parallel loop gang present(b_DNB)
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE

       !$acc loop vector collapse(3)
       do k=1, nK+1; do j=1, nJ+1; do i=1, nI+1
          do iDim = 1, 3
             b_DNB(iDim,i,j,k,iBlock) = &
                  sum(b_DGB(iDim,i-1:i,j-1:j,k-1:k,iBlock))*0.125
          end do
       end do; end do; end do

    end do ! iBlock

    !$acc update host(b_DNB)
    ! Average node values between shared faces
    call message_pass_node(3,b_DNB)
    !$acc update device(b_DNB)

#ifndef _OPENACC
    if(DoTest)write(*,*) NameSub,' normalized B'
    if(DoTime .and. iProc==0)then
       write(*,'(a)', ADVANCE='NO') NameSub//' setup and normalization:'
       call timing_show('trace_field_grid', 1)
    end if

    if(DoTest)write(*,*)NameSub,' starting iterations to obtain Trace_DINB'
#endif

    ! Iterate
    dTraceMin = rIonosphere*1e-6
    UsePreferredInterpolation = .false.
    nIterTrace = 0
    !$acc update device(UsePreferredInterpolation, nIterTrace)
    call timing_stop('trace_grid_fast1')

    call timing_start("trace_iter")
    do
       if(DoTest)write(*,*)'nIterTrace=',nIterTrace

       if(nIterTrace>=MaxIterTrace)EXIT

       call timing_start("trace_iter1")
       !$acc parallel loop gang
       do iBlock = 1, nBlock
          !$acc loop vector collapse(3)
          do iZ = 1, nK+1; do iY = 1, nJ+1; do iX = 1, nI+1
             ! Store Trace_DINB into Trace_DSNB
             ! so we can see if there is any change
             Trace_DSNB(:,:,iX,iY,iZ,iBlock) = Trace_DINB(:,:,iX,iY,iZ,iBlock)
          end do; end do; end do

          if(Unused_B(iBlock)) CYCLE

          ! Flag cells inside the ionosphere if necessary
          DoCheckInside=rMin_B(iBlock)<rTrace

          !$acc loop vector collapse(3) independent &
          !$acc private(Gen_D, GenIni_D, Weight_I, Trace_DI, &
          !$acc         i0, j0, k0, iCount)
          ! i1, j1, k1, i2, j2, k2, iFace)
          do iZ = 1, nK+1; do iY = 1, nJ+1; do iX = 1, nI+1
             ! Exclude inside points !!! inefficient ???
             if(iX>1 .and. iX<=nI .and. iY>1 .and. iY<=nJ &
                  .and. iZ>1 .and. iZ<=nK ) CYCLE

             ! Exclude outer boundaries
             if(DiLevel_EB(1,iBlock)==Unset_ .and. iX==   1) CYCLE
             if(DiLevel_EB(2,iBlock)==Unset_ .and. iX==nI+1) CYCLE
             if(DiLevel_EB(5,iBlock)==Unset_ .and. iZ==   1) CYCLE
             if(DiLevel_EB(6,iBlock)==Unset_ .and. iZ==nK+1) CYCLE
             if(DiLevel_EB(3,iBlock)==Unset_ .and. iY==   1) CYCLE
             if(DiLevel_EB(4,iBlock)==Unset_ .and. iY==nJ+1) CYCLE

             if(DoTestTrace)write(*,*) &
                  'TESTING Trace_DSNB: me,iBlock,iX,iY,iZ,Xyz_D',&
                  iProc, iBlock, iX, iY, iZ,&
                  Xyz_DGB(:,iX,iY,iZ,iBlock) - 0.5*CellSize_DB(:,iBlock)

             if(nIterTrace == 0)then
                do iTrace = 1, 2
                   ! Follow Trace_DSNB in direction iTrace
                   GenIni_D = [iX-0.5, iY-0.5, iZ-0.5]

                   iFace = follow_fast(.true., Gen_D, GenIni_D, &
                        DoCheckInside, iTrace, iBlock)

                   ! Assign value to Trace_DINB
                   call assign_trace(iFace, iTrace, iBlock, iX, iY, iZ, &
                        i1, j1, k1, i2, j2, k2, &
                        .true., Trace_DINB(:,iTrace,iX,iY,iZ,iBlock), Gen_D,&
                        Weight_I, .true.)

                   ! Memorize Trace_DSNB integration results
                   I_DINB(1,iTrace,iX,iY,iZ,iBlock) = iFace
                   if(iFace > 0)then
                      select case(iFace)
                      case(1,2)
                         I_DINB(2:3,iTrace,iX,iY,iZ,iBlock) = [j1,k1]
                      case(3,4)
                         I_DINB(2:3,iTrace,iX,iY,iZ,iBlock) = [i1,k1]
                      case(6,5)
                         I_DINB(2:3,iTrace,iX,iY,iZ,iBlock) = [i1,j1]
                      end select
                      WeightTrace_DINB(:,iTrace,iX,iY,iZ,iBlock) = Weight_I
                   end if
                end do
             else
                do iTrace = 1, 2
                   ! Use stored values
                   iFace = I_DINB(1,iTrace,iX,iY,iZ,iBlock)
                   if(iFace > 0)then
                      select case(iFace)
                      case(1)
                         i1 = 1; i2 = 1
                         j1 = I_DINB(2,iTrace,iX,iY,iZ,iBlock); j2 = j1 + 1
                         k1 = I_DINB(3,iTrace,iX,iY,iZ,iBlock); k2 = k1 + 1
                      case(2)
                         i1 = nI+1; i2 = i1
                         j1 = I_DINB(2,iTrace,iX,iY,iZ,iBlock); j2 = j1 + 1
                         k1 = I_DINB(3,iTrace,iX,iY,iZ,iBlock); k2 = k1 + 1
                      case(3)
                         j1 = 1; j2=1
                         i1 = I_DINB(2,iTrace,iX,iY,iZ,iBlock); i2 = i1 + 1
                         k1 = I_DINB(3,iTrace,iX,iY,iZ,iBlock); k2 = k1 + 1
                      case(4)
                         j1 = nJ + 1; j2 = nJ + 1
                         i1 = I_DINB(2,iTrace,iX,iY,iZ,iBlock); i2 = i1 + 1
                         k1 = I_DINB(3,iTrace,iX,iY,iZ,iBlock); k2 = k1 + 1
                      case(5)
                         k1 = 1; k2 = 1
                         i1 = I_DINB(2,iTrace,iX,iY,iZ,iBlock); i2 = i1 + 1
                         j1 = I_DINB(3,iTrace,iX,iY,iZ,iBlock); j2 = j1 + 1
                      case(6)
                         k1 = nK + 1; k2 = k1
                         i1 = I_DINB(2,iTrace,iX,iY,iZ,iBlock); i2 = i1 + 1
                         j1 = I_DINB(3,iTrace,iX,iY,iZ,iBlock); j2 = j1 + 1
                      end select

                      ! Put interpolation values into a small array
                      iCount = 1
                      !$acc loop seq collapse(3)
                      do k0 = k1, k2; do j0 = j1, j2; do i0 = i1, i2
                         Trace_DI(:,iCount) = &
                              Trace_DSNB(:,iTrace,i0,j0,k0,iBlock)
                         iCount = iCount + 1
                      end do; end do; end do
                      call interpolate_trace_face(Trace_DI, &
                           WeightTrace_DINB(:,iTrace,iX,iY,iZ,iBlock), 4, &
                           Trace_DINB(:,iTrace,iX,iY,iZ,iBlock))
                   end if
                end do
             end if ! nIterTrace==0
          end do; end do; end do ! iZ, iY, iX
       end do ! iBlock

       call timing_stop("trace_iter1")

       ! Exchange Trace_DINB information

       call timing_start('pass_trace')
       call pass_trace
       call timing_stop('pass_trace')

       nIterTrace = nIterTrace + 1
       !$acc update device(nIterTrace)

       if(DoTime .and. iProc == 0 .and. nIterTrace == 1)then
          write(*,'(a)',ADVANCE='NO') 'first iteration:'
          call timing_show('trace_field_grid', 1)
       end if

       ! Check for significant changes and loops in Trace_DINB
       call timing_start('trace_check')

       DoneChange = .true.; DoneLoop = .true.
       !$acc parallel loop gang reduction(.and.:DoneChange,DoneLoop)
       do iBlock = 1, nBlock
          if(Unused_B(iBlock))CYCLE
          !$acc loop vector collapse(3) reduction(.and.:DoneChange,DoneLoop)
          do k = 1,nK+1; do j = 1,nJ+1; do i = 1,nI+1
             DoneChange = DoneChange .and. &
                  all(abs(Trace_DSNB(:,:,i,j,k,iBlock) &
                  -       Trace_DINB(:,:,i,j,k,iBlock)) < dTraceMin)
             DoneLoop = DoneLoop .and. &
                  Trace_DINB(1,1,i,j,k,iBlock) > LoopRay .and. &
                  Trace_DINB(1,2,i,j,k,iBlock) > LoopRay
          end do; end do; end do
       end do

       if(nProc > 1)then
          Done_I = [DoneChange, DoneLoop]
          call MPI_allreduce(MPI_IN_PLACE, Done_I, 2, MPI_LOGICAL, &
               MPI_LAND, iComm, iError)
          DoneChange = Done_I(1); DoneLoop = Done_I(2)
       end if

       call timing_stop('trace_check')

       ! Check for change
       if(DoneChange) then
          ! Check for loops
          if(DoneLoop) EXIT
          if(UsePreferredInterpolation)then
             if(iProc == 0)call error_report( &
                  'Trace_DSNB tracing, nIterTrace=', &
                  nIterTrace+0.0, iError1, .true.)
             EXIT
          endif
          if(DoTest)write(*,*)'Switching to UsePreferredInterpolation=.true.'
          UsePreferredInterpolation = .true.
          !$acc update device(UsePreferredInterpolation)
       end if

    end do ! trace iteration

    call timing_stop("trace_iter")

#ifndef _OPENACC
    ! Check for unassigned Trace_DINB in every used block
    if(DoTest)then
       write(*,*)NameSub,' finished after ',nIterTrace,' iterations'
       do iBlock = 1, nBlock
          if(Unused_B(iBlock))CYCLE
          do iTrace=1,2
             if(any(Trace_DINB(1,iTrace,1:nI,1:nJ,1:nK,iBlock)<BODYRAY))then
                Ijk_D = minloc(Trace_DINB(1,iTrace,1:nI,1:nJ,1:nK,iBlock))
                write(*,*)'LOOPRAYFACE: iTrace,me,Ijk_D,value,Xyz_D=',&
                     iTrace, iProc, Ijk_D, iBlock, &
                     minval(Trace_DINB(1,iTrace,1:nI,1:nJ,1:nK,iBlock)),&
                     Xyz_DGB(:,Ijk_D(1),Ijk_D(2),Ijk_D(3),iBlock) &
                     -0.5*CellSize_DB(:,iBlock)
             end if
          end do
       end do
       if(index(StringTest,'trace_debugger') > 0)call trace_debugger
    end if

    if(DoTime.and.iProc==0)then
       write(*,'(i5,a)') nIterTrace,' iterations:'
       call timing_show('trace_field_grid',1)
       call timing_show('pass_trace',2)
    end if

    if(DoTest)write(*,*)NameSub,' starting cell center assignments'
#endif

    call timing_start('trace_grid_fast2')

    ! Assign face Trace_DSNB values to cell centers
    !$acc parallel loop gang private(DoCheckInside, iTrace)
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE

       ! Set flag if checking on the ionosphere is necessary
       DoCheckInside = rMin_B(iBlock) < rTrace

       do iTrace = 1, 2
#ifndef _OPENACC
          ! Some optimization for fully open blocks
          if(.not.DoCheckInside)then
             if(all(Trace_DINB(1,iTrace,:,:,:,iBlock) == OPENRAY))then
                Trace_DSNB(:,iTrace,:,:,:,iBlock) = OPENRAY
                CYCLE
             end if
          end if
#endif
          !$acc loop vector collapse(3) private(GenIni_D,Gen_D,Weight_I,iFace)
          do iZ = 1, nK; do iY = 1, nJ; do iX = 1, nI

             ! Shortcuts for inner and false cells
             if(r_GB(iX,iY,iZ,iBlock) < rInner .or. &
                  .not.Used_GB(iX,iY,iZ,iBlock))then
                Trace_DSNB(:,iTrace,iX,iY,iZ,iBlock) = BODYRAY
                if(DoTestTrace)write(*,*)'BODYRAY'
                CYCLE
             end if
             if(DoTestTrace)write(*,*)'calling follow_fast'

             ! Follow Trace_DSNB in direction iTrace
             GenIni_D = [real(iX), real(iY), real(iZ)]
             iFace = follow_fast(.false., Gen_D, GenIni_D, DoCheckInside, &
                  iTrace, iBlock)

             if(DoTestTrace)write(*,*)'calling assign_trace'

             ! Assign value to Trace_DSNB
             call assign_trace(iFace, iTrace, iBlock, iX, iY, iZ, &
                  i1, j1, k1, i2, j2, k2, .false., &
                  Trace_DSNB(:,iTrace,iX,iY,iZ,iBlock), Gen_D, Weight_I)

          end do; end do; end do ! iX, iY, iZ
       end do ! iTrace
    end do ! iBlock

    call timing_stop('trace_grid_fast2')

#ifndef _OPENACC
    if(DoTest)then
       write(*,*)NameSub,' finished with Trace_DSNB=',&
            Trace_DSNB(:,:,iTest,jTest,kTest,iBlockTest)

       ! Check for unassigned cell centers
       do iBlock = 1, nBlock
          if(Unused_B(iBlock))CYCLE
          do iTrace = 1, 2
             if(any(Trace_DSNB(1,iTrace,1:nI,1:nJ,1:nK,iBlock)<BODYRAY))then
                Ijk_D = minloc(Trace_DSNB(1,iTrace,1:nI,1:nJ,1:nK,iBlock))
                write(*,*)'LoopRay: iTrace,me,Ijk_D,value,Xyz_D=',&
                     iTrace,iProc,Ijk_D,iBlock,&
                     minval(Trace_DSNB(1,iTrace,1:nI,1:nJ,1:nK,iBlock)),&
                     Xyz_DGB(:,Ijk_D(1),Ijk_D(2),Ijk_D(3),iBlock)
             end if
          end do
       end do

       write(*,*)NameSub,' starting conversion to lat/lon'
    end if
#endif
    call timing_start('trace_grid_fast3')

    ! Convert end position to co-latitude, longitude, and status
    !$acc parallel loop gang
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       !$acc loop vector collapse(3)
       do k=1,nK; do j=1,nJ; do i=1,nI
          call xyz_to_latlonstatus(Trace_DSNB(:,:,i,j,k,iBlock))
       end do; end do; end do
    end do

    call timing_stop('trace_grid_fast3')

    if(DoTime .and. iProc==0)then
       write(*,'(a)',ADVANCE='NO') NameSub//' total tracing time:'
       call timing_show('trace_field_grid', 1)
    end if
    call barrier_mpi
    call test_stop(NameSub, DoTest)

    call timing_stop(NameSub)

  contains
    !==========================================================================
#ifndef _OPENACC
    subroutine trace_debugger

      ! Debug Trace_DINB values

      integer :: iPos_D(3), jX, kX, jY, kY, jZ, kZ
      !------------------------------------------------------------------------
      do
         ! Read position
         write(*,'(a)',ADVANCE='NO')'Trace_DINB x,y,z,iTrace:'
         read(*,*) xTest, yTest, zTest, iTrace
         if(xTest == 0.0 .and. yTest == 0.0 .and. zTest == 0.0) EXIT

         ! Find position
         do iBlock = 1, nBlock
            if(Unused_B(iBlock))CYCLE
            do iX = 1, nI+1
               if(abs(Xyz_DGB(x_,iX,1,1,iBlock) &
                    -0.5*CellSize_DB(x_,iBlock)-xTest) > 0.01) CYCLE
               do iY = 1, nJ+1
                  if(abs(Xyz_DGB(y_,1,iY,1,iBlock) &
                       -0.5*CellSize_DB(y_,iBlock)-yTest) > 0.01) CYCLE
                  do iZ = 1, nK+1
                     if(abs(Xyz_DGB(z_,1,1,iZ,iBlock) &
                          -0.5*CellSize_DB(z_,iBlock)-zTest) > 0.01) CYCLE

                     ! Print information
                     write(*,*)'iProc,iBlock,iX,iY,iZ=', &
                          iProc, iBlock, iX, iY, iZ
                     write(*,*)'Xyz_DGB(z_,1,1,1),dx=',&
                          Xyz_DGB(:,1,1,1,iBlock), CellSize_DB(x_,iBlock)

                     iPos_D=I_DINB(:,iTrace,iX,iY,iZ,iBlock)
                     if(iPos_D(1)>0)then
                        write(*,*)' Trace_DINB   =', &
                             Trace_DINB(:,iTrace,iX,iY,iZ,iBlock)
                        write(*,*)' I_DINB=', &
                             I_DINB(:,iTrace,iX,iY,iZ,iBlock)
                        write(*,*)' WeightTrace_DINB=', &
                             WeightTrace_DINB(:,iTrace,iX,iY,iZ,iBlock)
                        select case(iPos_D(1))
                        case(1,2)
                           jX = 1+nI*(iPos_D(1)-1);
                           jY = iPos_D(2); jZ = iPos_D(3)
                           kX = jX; kY = jY+1; kZ = jZ+1
                        case(3,4)
                           jY = 1+nJ*(iPos_D(1)-3);
                           jX = iPos_D(2); jZ = iPos_D(3)
                           kX = jX+1; kY = jY; kZ = jZ+1
                        case(5,6)
                           jZ = 1+nK*(iPos_D(1)-5);
                           jX = iPos_D(2);
                           jY = iPos_D(3)
                           kX = jX+1; kY = jY+1; kZ = jZ
                        end select
                        write(*,*)' Trace_DINB(1,end)=',&
                             Trace_DINB(1,iTrace,jX:kX,jY:kY,jZ:kZ,iBlock)
                        write(*,*)' jX,kX,jY,kY,jZ,kZ=', jX, kX, jY, kY, jZ, kZ
                        write(*,*)' Xyz(End)=',&
                             Xyz_DGB(:,jx,jy,jz,iBlock) &
                             -0.5*CellSize_DB(:,iBlock)
                     else
                        write(*,*)' I_DINB=', iPos_D
                     end if
                  end do
               end do
            end do
         end do
      end do

    end subroutine trace_debugger
    !==========================================================================
#endif
    subroutine interpolate_bb_node(Gen_D, b_D, iBlock)
      !$acc routine seq

      ! Interpolate normalized field b_D at normalized location Gen_D
      ! Interpolate B1 from nodes, take B0 from analytic expression

#ifdef _OPENACC
      use ModB0, ONLY: get_b0_dipole
#endif

      real, intent(in) :: Gen_D(3)
      real, intent(out):: b_D(3)
      integer, intent(in):: iBlock
      real :: b

      integer :: i1, j1, k1, i2, j2, k2

      ! Distance between Gen_D and i1,j1,k1, and i2,j2,k2
      real :: Dx1, Dy1, Dz1, Dx2, Dy2, Dz2

      real :: Xyz_D(3)

      ! Determine cell indices corresponding to location Gen_D
      !------------------------------------------------------------------------
      i1 = floor(Gen_D(1)+0.5); i2 = i1 + 1
      j1 = floor(Gen_D(2)+0.5); j2 = j1 + 1
      k1 = floor(Gen_D(3)+0.5); k2 = k1 + 1

#ifndef _OPENACC
      if(i1<0 .or. i2>nI+2 .or. j1<0 .or. j2>nJ+2 .or. k1<0 .or. k2>nK+2)then
         write(*,*)'interpolate_bb_node: iProc, iBlock, Gen_D=', &
              iProc, iBlock, Gen_D
         call stop_mpi('ERROR in interpolate_bb_node: location out of bounds')
      endif
#endif
      ! Get B0 values for location
      Xyz_D = Xyz_DGB(:,1,1,1,iBlock) + CellSize_DB(:,iBlock)*(Gen_D - 1)

      if(UseB0)then
#ifdef _OPENACC
         call get_b0_dipole(Xyz_D, b_D)
#else
         call get_b0(Xyz_D, b_D)
#endif
      else
         b_D = 0.00
      end if

      ! Make sure that the interpolation uses inside indexes only
      i1 = max(1, i1)   ; j1 = max(1, j1);    k1 = max(1, k1)
      i2 = min(nI+1, i2); j2 = min(nJ+1, j2); k2 = min(nK+1, k2)

      ! Distances relative to the nodes
      Dx1 = Gen_D(1) + 0.5 - i1; Dx2 = 1.-Dx1
      Dy1 = Gen_D(2) + 0.5 - j1; Dy2 = 1.-Dy1
      Dz1 = Gen_D(3) + 0.5 - k1; Dz2 = 1.-Dz1

      ! Add in node interpolated B1 values and take aspect ratios into account
      b_D = b_D &
           + Dx1*(   Dy1*(   Dz1*b_DNB(:,i2,j2,k2,iBlock)   &
           +                 Dz2*b_DNB(:,i2,j2,k1,iBlock))  &
           +         Dy2*(   Dz1*b_DNB(:,i2,j1,k2,iBlock)   &
           +                 Dz2*b_DNB(:,i2,j1,k1,iBlock))) &
           + Dx2*(   Dy1*(   Dz1*b_DNB(:,i1,j2,k2,iBlock)   &
           +                 Dz2*b_DNB(:,i1,j2,k1,iBlock))  &
           + Dy2*(           Dz1*b_DNB(:,i1,j1,k2,iBlock)   &
           +                 Dz2*b_DNB(:,i1,j1,k1,iBlock)))

      b_D = b_D/CellSize_DB(:,iBlock)

      ! Normalize
      b = norm2(b_D)

      if(b > SmallB)then
         b_D = b_D/b
      else
         b_D = 0.
      end if

    end subroutine interpolate_bb_node
    !==========================================================================
    logical function do_follow_iono(Xyz_D, iTrace)
      !$acc routine seq

      ! Trace inside ionosphere starting from Xyz_D which
      ! is given in real coordinates and use analytic mapping.
      ! On return Xyz_D contains the final coordinates.
      ! Return true if it was successfully integrated down to rIonosphere,
      ! return false if the trace exited rTrace or too many integration
      ! steps were Done

      use ModPhysics,  ONLY: DipoleStrengthSi ! only the sign is needed
      use CON_planet_field, ONLY: map_planet_field, map_planet_field_fast

      real, intent(inout):: Xyz_D(3)
      integer, intent(in):: iTrace

      integer :: iHemisphere
      real    :: x_D(3)
      !------------------------------------------------------------------------
#ifndef _OPENACC
      if(iTypeUpdate <= UpdateSlow_)then
         call map_planet_field(tSimulation, Xyz_D, &
              TypeCoordSystem//' NORM', rIonosphere, x_D, iHemisphere)
      else
#endif
         call map_planet_field_fast(Xyz_D, rIonosphere, x_D, iHemisphere, &
              UseGsmIn=.true.)
#ifndef _OPENACC
      end if

      if(iHemisphere==0)then
         write(*,*)'iHemisphere==0 for Xyz_D=',Xyz_D
         write(*,*)'iBlock, iTrace=',iBlock,iTrace
         call stop_mpi('ERROR in do_follow_iono')
      end if
#endif

      if(iHemisphere*DipoleStrengthSi*sign(1.0,1.5-iTrace) < 0.0)then
         Xyz_D = x_D
         do_follow_iono = .true.
      else
         do_follow_iono = .false.
      end if

    end function do_follow_iono
    !==========================================================================
    function follow_fast(IsSurfacePoint, Gen_D, GenIn_D, DoCheckInside, &
         iTrace, iBlock) result(iFaceOut)
      !$acc routine seq

      ! Trace starting at initial position GenIn_D in direction
      ! iTrace until we hit the wall of the control volume or the ionosphere.
      ! Return 1,2,3,4,5,6 if the trace hit the block faces
      ! Return RayIono_   if the trace hit the ionosphere
      ! Return RayLoop_   if the trace did not hit anything
      ! Return RayOut_    if the trace goes out of the box immediately
      ! Return RayBody_   if the trace goes into or is inside a body

      ! Arguments

      logical, intent(in):: IsSurfacePoint
      real, intent(inout):: Gen_D(3)
      real, intent(in)   :: GenIn_D(3)
      logical, intent(in):: DoCheckInside
      integer, intent(in):: iTrace, iBlock

      ! Result

      integer :: iFaceOut

      ! Local variables

      ! Initial and mid point coordinates and bb field
      real :: GenIni_D(3), GenMid_D(3), bIni_D(3), bMid_D(3), XyzIni_D(3)
      real :: r, rIni

      ! Radial distance and square of it: r2=sum(Xyz_D**2)
      real :: r2

      real :: Xyz_D(3)

      ! dx is the difference between 1st and 2nd order RK to estimate accuracy
      ! DxOpt is the required accuracy, DxRel=dx/DxOpt
      real :: DxRel, DxOpt

      ! Line length, max, step size, limits, next step size, back to surface
      real :: s, sMax, Ds, DsMax, DsMin, DsNext, DsTiny, DsBack

      ! counter for integration
      integer :: nSegment
      integer, parameter:: MaxSegment = 10*(nI+nJ+nK)

      integer:: iOuter, iInner

      ! Counter for entering do_follow_iono
      integer :: nIono

      character(len=*), parameter:: NameSub = 'follow_fast'
      !------------------------------------------------------------------------
      if(DoTestTrace)&
           write(*,*) NameSub,': me,iBlock,IsSurfacePoint,GenIn_D,iTrace=',&
           iProc, iBlock, IsSurfacePoint, GenIn_D, iTrace

      ! Step size limits
      DsMax=1.0
      DsMin=0.05
      DsTiny=1.e-6

      ! Initial value
      DsNext=sign(DsMax,1.5-iTrace)

      ! Accuracy in terms of Gen_D in normalized coordinates
      DxOpt=0.01

      ! Length and maximum length of trace within control volume
      s = 0
      sMax = 10*maxval(GenMax_D - GenMin_D)
      nSegment = 0
      nIono = 0

      ! Initial position
      Gen_D = GenIn_D

      ! It seems the implementation assumes the default of iFaceOut is 0.
      iFaceOut = 0

      ! FIXME: test_swpc_pc fails with upper limit MaxSegment
      ! Integration loop
      do iOuter = 1, 9999999 ! MaxSegment
         ! Check if we are inside the ionosphere
         if(DoCheckInside)then
            ! Convert Gen_D to real coordinates Xyz_D
            Xyz_D = Xyz_DGB(:,1,1,1,iBlock) + CellSize_DB(:,iBlock)*(Gen_D - 1)

            r2 = sum(Xyz_D**2)

            if(r2 <= rTrace2)then

               if(DoTestTrace)write(*,*)&
                    'Inside rTrace at me,iBlock,nSegment,Gen_D,Xyz_D=',&
                    iProc,iBlock,nSegment,Gen_D,Xyz_D

               if((.not. IsBVectorField) .or. r2 <= rInner2)then

                  if(nSegment==0)then
                     iFaceOut = RayBody_

                     if(DoTestTrace)write(*,*)&
                          'Initial point inside rInner at me,iBlock,Xyz_D=',&
                          iProc,iBlock,Xyz_D
                  else
                     r = sqrt(r2)
                     XyzIni_D = Xyz_DGB(:,1,1,1,iBlock) + &
                          CellSize_DB(:,iBlock)*(GenIni_D - 1)

                     rIni = norm2(XyzIni_D)
                     ! Interpolate to the surface linearly along last segment
                     Xyz_D = (Xyz_D*(rIni-rInner)+XyzIni_D*(rInner-r)) &
                          /(rIni-r)
                     ! Normalize Xyz_D in radial direction
                     Xyz_D = rInner*Xyz_D/norm2(Xyz_D)
                     Gen_D = Xyz_D
                     iFaceOut = RayIono_
                  end if
                  EXIT
               end if

               ! Try mapping down to rIonosphere if we haven't tried yet
               if(nIono<1)then
                  if(do_follow_iono(Xyz_D, iTrace))then
                     Gen_D=Xyz_D
                     iFaceOut=RayIono_
                     EXIT
                  else
                     ! We did not hit the surface of the ionosphere
                     ! continue the integration
                     nIono=nIono+1
                  end if
               end if
            end if
         end if

         ! Integrate with 2nd order scheme
         Ds = DsNext
         GenIni_D = Gen_D

         ! Half step
         call interpolate_bb_node(GenIni_D, bIni_D, iBlock)

         GenMid_D = GenIni_D + 0.5*Ds*bIni_D

         ! Check if the trace is pointing outwards
         if(nSegment==0.and.IsSurfacePoint)then
            if(DoTestTrace)write(*,*)'me,iBlock,GenIni_D,bIni_D=', &
                 iProc, iBlock, GenIni_D, bIni_D

            if(any(GenMid_D < GenMin_D) .or. any(GenMid_D > GenMax_D))then
               iFaceOut = RayOut_

               if(DoTestTrace)then
                  write(*,*)'me,iBlock,GenMid_D=', iProc, iBlock, GenMid_D
                  write(*,*)'trace points outwards: me,iBlock,Ds,Xyz_D=',&
                       iProc, iBlock, Ds,&
                       Xyz_DGB(:,1,1,1,iBlock) &
                       + CellSize_DB(:,iBlock)*(GenMid_D - 1)
               end if

               RETURN
            end if
         end if

         do iInner = 1, 10
            ! Full step
            call interpolate_bb_node(GenMid_D, bMid_D, iBlock)

            ! Calculate the difference between 1st and 2nd order integration
            ! and take ratio relative to DxOpt
            DxRel = abs(Ds)*maxval(abs(bMid_D - bIni_D))/DxOpt

            if(DoTestTrace .and. .false.) &
                 write(*,*)'me,iBlock,GenMid_D,bMid_D,DxRel=', &
                 iProc, iBlock, GenMid_D, bMid_D, DxRel

            ! Make sure that Ds does not change more than a factor of 2 or 0.5
            DxRel = max(0.5, min(2., DxRel))

            if(DxRel > 1.)then
               ! Not accurate enough, decrease Ds if possible

               if(abs(Ds)<=DsMin+DsTiny)then
                  ! Cannot reduce Ds further
                  DsNext=Ds
                  EXIT
               end if

               Ds = sign(max(DsMin,abs(Ds)/(DxRel+0.001)),Ds)

               ! New mid point using the reduced Ds
               GenMid_D = GenIni_D + 0.5*Ds*bIni_D

               if(DoTestTrace .and. DoDebug)&
                    write(*,*)'new decreased Ds: me,iBlock,Ds=', &
                    iProc,iBlock,Ds

            else
               ! Too accurate, increase Ds if possible
               if(abs(Ds) < DsMax - DsTiny)then

                  DsNext = sign(min(DsMax, abs(Ds)/sqrt(DxRel)), Ds)

                  if(DoTestTrace .and. DoDebug)&
                       write(*,*)'new increased DsNext: me,iBlock,DsNext=', &
                       iProc, iBlock, DsNext

               end if

               EXIT
            end if
         end do
         Gen_D = GenIni_D + bMid_D*Ds

         nSegment = nSegment+1
         s = s + abs(Ds)

         if(DoTestTrace .and. DoDebug)&
              write(*,*)'me,iBlock,nSegment,s,Gen_D=', &
              iProc, iBlock, nSegment, s, Gen_D

         ! Check if the trace hit the wall of the control volume
         if(any(Gen_D < GenMin_D) .or. any(Gen_D > GenMax_D))then

            ! Hit the wall, backup so that Gen_D is almost exactly on the wall
            ! just a little bit outside. Only if nSegment is more than 1!
            if(nSegment > 1)then
               DsBack = Ds*maxval(max(GenMin_D-Gen_D,Gen_D-GenMax_D) &
                    /(abs(Gen_D - GenIni_D) + DsTiny))
               Gen_D = Gen_D - DsBack*bMid_D
            end if

            ! Find out which wall was hit
            if    (Gen_D(1) <= GenMin_D(1))then; iFaceOut=1
            elseif(Gen_D(2) <= GenMin_D(2))then; iFaceOut=3
            elseif(Gen_D(3) <= GenMin_D(3))then; iFaceOut=5
            elseif(Gen_D(1) >= GenMax_D(1))then; iFaceOut=2
            elseif(Gen_D(2) >= GenMax_D(2))then; iFaceOut=4
            elseif(Gen_D(3) >= GenMax_D(3))then; iFaceOut=6
#ifndef _OPENACC
            else
               write(*,*) NameSub, ': Error at me,iBlock,iX,iY,iZ=', &
                    iProc, iBlock, iX, iY, iZ
               write(*,*)'nSegment,Gen_D,Ds,DsBack=', &
                    nSegment, Gen_D, Ds, DsBack
               call stop_mpi('GM_follow_fast: Hit wall but which one?')
#endif
            end if

            ! Make sure that Gen_D is not outside the control volume
            Gen_D = max(GenMin_D+DsTiny, Gen_D)
            Gen_D = min(GenMax_D-DsTiny, Gen_D)

            EXIT
         end if

         ! Check if we have integrated for too long
         if(s>sMax)then
            ! Seems to be a closed loop within a block

            if(DoTestTrace)then
               write(*,*)'CLOSED LOOP at me,iBlock,iX,iY,iZ,Gen_D,Xyz_D=',&
                    iProc,iBlock,iX,iY,iZ,Gen_D,&
                    Xyz_DGB(:,1,1,1,iBlock) + CellSize_DB(:,iBlock)*(Gen_D - 1)
            end if

            iFaceOut=RayLoop_
            EXIT
         end if

      end do
      if(DoTestTrace)write(*,*) NameSub, &
           ' finished at me,iBlock,nSegment,iFaceOut,Gen_D,Xyz_D=',&
           iProc,iBlock,nSegment,iFaceOut,Gen_D,&
           Xyz_DGB(:,1,1,1,iBlock) + CellSize_DB(:,iBlock)*(Gen_D - 1)

    end function follow_fast
    !==========================================================================
    subroutine assign_trace(iFace, iTrace, iBlock, iX, iY, iZ, i1, j1, k1, &
         i2, j2, k2, IsSurfacePoint, Trace_D, Gen_D, Weight_I, UseRay)
      !$acc routine seq

      ! Assign value to Trace_D(3) based on trace intersection
      ! given by the global variables iFace and position Gen_D(3)
      !
      ! iTrace is 1 if trace points in positive B direction and 2 otherwise
      !
      ! IsSurfacePoint is true if the tracing was started from the block
      ! face and false if it was started from a cell center

      integer, intent(in)    :: iFace, iTrace, iBlock, iX, iY, iZ
      integer, intent(inout) :: i1,j1,k1,i2,j2,k2
      logical, intent(in)    :: IsSurfacePoint

      ! Called with a segment of Trace_DINB array and it is used here
      ! to get Trace_D
      real, intent(inout)    :: Trace_D(3), Gen_D(3)
      real, intent(inout)    :: Weight_I(4)
      logical, optional, intent(in):: UseRay

      real :: Trace_DI(3,4)
      integer :: iCount, i0, j0, k0

      ! Distances between Gen_D and the 4 grid points used for interpolation
      real :: d1, e1, d2, e2

      character(len=*), parameter:: NameSub = 'assign_trace'
      !------------------------------------------------------------------------
      if(DoTestTrace)write(*,*)&
           NameSub,' starting with IsSurfacePoint, iTrace, iFace=',&
           IsSurfacePoint,iTrace,iFace

      select case(iFace)
      case(RayOut_)
         ! The trace points outward
         Trace_D = OUTRAY

         if(DoTestTrace)write(*,*)NameSub,' finished with Trace_D=OUTRAY'

         RETURN
      case(RayLoop_)
         ! The trace did not hit the wall of the block
         Trace_D = LoopRay

         if(DoTestTrace)write(*,*)NameSub,' finished with Trace_D=LoopRay'

         RETURN
      case(RayBody_)
         ! The trace hit a body
         Trace_D = BODYRAY

         if(DoTestTrace)write(*,*)NameSub,' finished with Trace_D=BODYRAY'

         RETURN
      case(RayIono_)
         ! The trace hit the ionosphere
         Trace_D = Gen_D

         if(DoTestTrace)write(*,*) NameSub, &
              ' finished with Trace_D on ionosphere, Trace_D=',Trace_D

         RETURN
      case(1, 2)
         if(iFace == 1)then
            i1 = 1
         else
            i1 = nI + 1
         endif
         i2 = i1
         j1 = floor(Gen_D(2) - GenMin_D(2)) + 1; j2 = j1 + 1
         k1 = floor(Gen_D(3) - GenMin_D(3)) + 1; k2 = k1 + 1
         d1 = Gen_D(2) - j1 + 0.5
         e1 = Gen_D(3) - k1 + 0.5

      case(3, 4)
         if(iFace == 3)then
            j1 = 1
         else
            j1 = nJ + 1
         endif
         j2 = j1
         i1 = floor(Gen_D(1) - GenMin_D(1)) + 1; i2 = i1 + 1
         k1 = floor(Gen_D(3) - GenMin_D(3)) + 1; k2 = k1 + 1
         d1 = Gen_D(1) - i1 + 0.5
         e1 = Gen_D(3) - k1 + 0.5

      case(5, 6)
         ! The trace hit the bottom or top wall
         if(iFace == 5)then
            k1 = 1
         else
            k1 = nK + 1
         endif
         k2 = k1
         i1 = floor(Gen_D(1) - GenMin_D(1)) + 1; i2 = i1 + 1
         j1 = floor(Gen_D(2) - GenMin_D(2)) + 1; j2 = j1 + 1
         d1 = Gen_D(1) - i1 + 0.5
         e1 = Gen_D(2) - j1 + 0.5

#ifndef _OPENACC
      case default
         write(*,*)'Impossible value for iFace=',iFace,' at iX,iY,iZ,iBlock=',&
              iX,iY,iZ,iBlock
         call stop_mpi('assign_trace')
#endif
      end select

      ! Calculate bilinear interpolation weights
      d2 = 1 - d1; e2 = 1 - e1
      Weight_I(1) = d2*e2
      Weight_I(2) = d1*e2
      Weight_I(3) = d2*e1
      Weight_I(4) = d1*e1

      if(DoTestTrace)write(*,*)'Weight_I=',Weight_I

      ! Exclude the starting point if its among the 4 interpolated cells
      if(IsSurfacePoint)then
         if((iX==i1.or.iX==i2) .and. (iY==j1.or.iY==j2) .and. &
              (iZ==k1.or.iZ==k2))then
            select case(iFace)
            case(1,2)
               Weight_I(iY-j1+2*(iZ-k1)+1) = 0.
            case(3,4)
               Weight_I(iX-i1+2*(iZ-k1)+1) = 0.
            case(5,6)
               Weight_I(iX-i1+2*(iY-j1)+1) = 0.
            end select
            ! Normalize weights
            Weight_I = Weight_I/sum(Weight_I)

            if(DoTestTrace)write(*,*) &
                 'Excluded point: me,iBlock,iX,iY,iZ,Weight_I=',&
                 iProc, iBlock, iX, iY, iZ, Weight_I
         end if
      end if

      if(DoTestTrace) write(*,*)'i1,j1,k1,i2,j2,k2,d1,e1=', &
           i1, j1, k1, i2, j2, k2, d1, e1

      iCount = 1
      do k0 = k1, k2; do j0 = j1, j2; do i0 = i1, i2
         if(present(UseRay)) then
            ! Use Trace_DSNB instead of Trace_DINB to avoid GPU race condition
            if(UseRay) &
                 Trace_DI(:,iCount) = Trace_DSNB(:,iTrace,i0,j0,k0,iBlock)
         else
            Trace_DI(:,iCount) = Trace_DINB(:,iTrace,i0,j0,k0,iBlock)
         endif
         iCount = iCount + 1
      end do; end do; end do

      call interpolate_trace_face(Trace_DI, Weight_I, 4, Trace_D)

      if(DoTestTrace)write(*,*)NameSub,' finished Trace_D=',Trace_D

    end subroutine assign_trace
    !==========================================================================
  end subroutine trace_grid_fast
  !============================================================================
  subroutine interpolate_trace_face(Trace_DI, Weight_I, nValue, Trace_D)
    !$acc routine seq

    ! Collect weights for Trace_DI values that differ less than dTraceMax
    ! and interpolate the values corresponding to the largest Weight_I
    ! The result is returned in Trace_D.
    ! Note that Trace_D and Trace_DI may overlap, so their intent must be inout

    integer, intent(in)    :: nValue
    real,    intent(inout) :: Trace_DI(3,nValue)
    real,    intent(in)    :: Weight_I(nValue)
    real,    intent(inout) :: Trace_D(3)

    ! Local variables

    ! Cumulated weights corresponding to various kinds of Trace_DI values
    real :: WeightTmp_I(4), WeightSum_I(4)
    real :: TraceFirst_DI(3,4), TraceSum_DI(3,4)

    ! Difference between Trace_DI values, maximum for interpolation
    real :: dTrace, dTraceMax, ValueMax

    ! Number and indices of (cummulated) Trace_DI values, max location
    integer :: n, i, j

    integer :: iCount
    real :: WeightMax
    integer:: iWeightMax

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'interpolate_trace_face'
    !--------------------------------------------------------------------------
    if(.not.UsePreferredInterpolation .and. &
         maxval(Weight_I) - minval(Weight_I) > 0.0001)then
       WeightTmp_I(1:nValue) = Weight_I
    else
       ValueMax = maxval(Trace_DI(1,:), MASK = Weight_I > 0.0) ! 0.01)

       if(ValueMax < CLOSEDRAY)then
          Trace_D = ValueMax
          RETURN
       end if

       where(Trace_DI(1,:) >= OPENRAY - 0.01)
          WeightTmp_I(1:nValue) = Weight_I
       elsewhere
          WeightTmp_I(1:nValue) = 0.
       endwhere
    end if

    if(DoTestTrace)then
       write(*,*) NameSub
       write(*,*)'Trace_DI(1,:)=',Trace_DI(1,:)
       write(*,*)'Trace_DI(2,:)=',Trace_DI(2,:)
       write(*,*)'Trace_DI(3,:)=',Trace_DI(3,:)
       write(*,*)'Weight_I       =',Weight_I
       write(*,*)'WeightTmp_I      =',WeightTmp_I
    end if

    ! Short cuts
    if(all(Trace_DI(1,:) == OPENRAY))then
       ! all surrounding rays are open
       Trace_D = OPENRAY
       if(DoTestTrace)write(*,*) NameSub,' finished with fully OPENRAY'
       RETURN
    end if

    if(all(Trace_DI(1,:) == NoRay))then
       ! all surrounding rays are unknown
       Trace_D = NoRay
       if(DoTestTrace)write(*,*) NameSub,' finished with fully NoRay'
       RETURN
    end if

    dTraceMax = 0.2*rIonosphere
    n = 0
    do j = 1, nValue
       i = 1
       do iCount = 1, 9999999
          if(i > n)then
             ! New type of trace
             n = i
             TraceFirst_DI(:,i) = Trace_DI(:,j)
             WeightSum_I(i) = WeightTmp_I(j)
             if(TraceFirst_DI(1,i) > CLOSEDRAY)&
                  TraceSum_DI(:,i) = WeightTmp_I(j)*Trace_DI(:,j)
             EXIT
          end if

          ! Calculate difference between Trace_DI(:,j) and TraceFirst_DI(:,i)
          dTrace=sum(abs(Trace_DI(:,j)-TraceFirst_DI(:,i)))

          if(dTrace<dTraceMax)then
             ! Same type of trace, cummulate it

             WeightSum_I(i) = WeightSum_I(i) + WeightTmp_I(j)
             if(TraceFirst_DI(1,i) > CLOSEDRAY)&
                  TraceSum_DI(:,i) = TraceSum_DI(:,i) &
                  + WeightTmp_I(j)*Trace_DI(:,j)
             EXIT
          end if
          ! Try next type
          i = i + 1

#ifndef _OPENACC
          if(i > nValue)call stop_mpi(NameSub//': Impossible value for i')
#endif
       end do ! i
    end do ! j

    if(n == 1)then
       ! Only one type of trace is interpolated
       if(TraceFirst_DI(1,1)>CLOSEDRAY)then
          ! get result (WeightSum_I can be less than 1! )
          Trace_D = TraceSum_DI(:,1)/WeightSum_I(1)
       else
          ! identical Trace_DINB values, no need to average
          Trace_D = TraceFirst_DI(:,1)
       end if
    else
       ! Take the values corresponding to the largest cummulated Weight_I
       iWeightMax = 1
       WeightMax = WeightSum_I(1)
       do i = 2, n
          if(WeightSum_I(i) > WeightMax) then
             WeightMax = WeightSum_I(i)
             iWeightMax = i
          end if
       end do

       if(TraceFirst_DI(1,iWeightMax) > CLOSEDRAY)then
          ! take average
          Trace_D = TraceSum_DI(:,iWeightMax)/WeightSum_I(iWeightMax)
       else
          ! identical Trace_DINB values, no need to average
          Trace_D = TraceFirst_DI(:,iWeightMax)
       end if
    end if

    if(DoTestTrace)then
       write(*,*) NameSub,': WeightSum_I=',WeightSum_I(1:n)
       write(*,*) NameSub,' finished with Trace_D=',Trace_D
    end if

  end subroutine interpolate_trace_face
  !============================================================================
  subroutine pass_trace

    ! Exchange and update Trace_DINB values between blocks direction
    ! by direction

    ! Notation: O out        (cells to be sent for equal blocks)
    !           G get        (cells to be received)
    !           R restricted (to be sent to a coarser block)
    !           S subface    (one quarter of a face)

    use ModParallel, ONLY: DiLevel_EB

    ! Local variables

    ! iDir=1,2,3 correspond to east-west, south-north, bot-top.
    integer :: iDir

    integer :: iFace, iBlock

#ifndef _OPENACC
    ! Array ranges for outgoing, incoming, restricted and subfaces
    integer :: iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO
    integer :: iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG
    integer :: iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR
    integer :: iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS
#endif

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'pass_trace'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    do iDir = 1, 3
       ! Send messages for both faces
#ifdef _OPENACC
       call pass_face_gpu(iDir, 2*iDir-1, 2*iDir)
#else
       call pass_face(iDir, 2*iDir-1, 2*iDir)
#endif
    end do ! iDir

    if(DoTest)write(*,*) NameSub, ' starting prolongation'
    call timing_start('trace_prolong')
    !$acc parallel loop gang
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE

       do iFace = 1, 6
          if(DiLevel_EB(iFace,iBlock) == 1)call prolong_trace(iFace, iBlock)
       end do
    end do
    call timing_stop('trace_prolong')

    call test_stop(NameSub, DoTest)
#ifndef _OPENACC
  contains
    !==========================================================================
    subroutine buf_to_trace_face( &
         Buffer_DIN, iMin, iMax, jMin, jMax, kMin, kMax)

      ! Take maximum of Trace_DINB and buf (more positive values are more real)
      ! for the full face

      integer, intent(in) :: iMin, iMax, jMin, jMax, kMin, kMax
      real, intent(inout):: Buffer_DIN(3,2,iMin:iMax,jMin:jMax,kMin:kMax)
      !------------------------------------------------------------------------
      Trace_DINB(:,:,iMinG:iMaxG,jMinG:jMaxG,kMinG:kMaxG,iBlock) &
           = max(Buffer_DIN, &
           Trace_DINB(:,:,iMinG:iMaxG,jMinG:jMaxG,kMinG:kMaxG,iBlock))

    end subroutine buf_to_trace_face
    !==========================================================================
    subroutine buf_to_sparse_trace_face( &
         Buffer_DIN, iMin, iMax, jMin, jMax, kMin, kMax)

      ! Take maximum of Trace_DINB and buf (more positive values are more real)
      ! for a factor of 2 coarser grid

      integer, intent(in) :: iMin, iMax, jMin, jMax, kMin, kMax
      real, intent(inout):: Buffer_DIN(3,2,iMin:iMax,jMin:jMax,kMin:kMax)
      !------------------------------------------------------------------------
      Trace_DINB(:,:,iMinG:iMaxG:2,jMinG:jMaxG:2,kMinG:kMaxG:2,iBlock) = &
           max(Buffer_DIN,&
           Trace_DINB(:,:,iMinG:iMaxG:2,jMinG:jMaxG:2,kMinG:kMaxG:2,iBlock))

    end subroutine buf_to_sparse_trace_face
    !==========================================================================
    subroutine buf_to_trace_subface( &
         Buffer_DIN, iFace, iSubFace, iMin, iMax, jMin, jMax, kMin, kMax)

      ! Set subface range to write into

      integer, intent(in) :: iFace, iSubFace, iMin,iMax,jMin,jMax,kMin,kMax
      real, intent(inout) :: Buffer_DIN(3,2,iMin:iMax,jMin:jMax,kMin:kMax)
      !------------------------------------------------------------------------
      call set_subface_range(.false., iFace, iSubFace, &
           iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
           iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
           iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR, &
           iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS)

      ! Take maximum of Trace_DINB and buf (more positive values are more real)
      ! for the appropriate subface

      Trace_DINB(:,:,iMinS:iMaxS,jMinS:jMaxS,kMinS:kMaxS,iBlock) = &
           max(Buffer_DIN, &
           Trace_DINB(:,:,iMinS:iMaxS,jMinS:jMaxS,kMinS:kMaxS,iBlock))

    end subroutine buf_to_trace_subface
    !==========================================================================
    subroutine pass_face(iDir, iFaceMin, iFaceMax)

      use ModParallel, ONLY : Unset_, DiLevel_EB, jBlock_IEB, jProc_IEB
      use ModMpi

      integer, intent(in):: iDir, iFaceMin,iFaceMax

      ! Face, neighbor's face and side indexes Note: iFace is on CPU
      integer :: iFace, jFace, iSide

      ! number of subfaces (1 or 4), subface (1..nSubFace)
      integer ::  nSubFace, iSubFace

      ! Descriptors for neighbor
      integer :: jProc, jBlock, DiLevel

      ! Maximum size of the RESTRICTED Trace_DINB layer to be received
      ! for the 6 trace variables (3 coordinates * 2 trace directions)
      integer, parameter :: MaxSizeR = &
           6*max((nI/2+1)*(nJ/2+1),(nI/2+1)*(nK/2+1),(nJ/2+1)*(nK/2+1))

      ! MPI variables
      integer :: iTag, iRequest, nRecvRequest, iRecvRequest_I(MaxBlock*6), &
           iError

      ! Receive Buffer_IIBI to hold 4 incoming RESTRICTED Trace_DINB values
      ! for all blocks and for both sides
      real :: Buffer_IIBI(MaxSizeR,4,MaxBlock,2)

      ! Equal and restricted values to be sent are stored in these buffers
      real, allocatable :: BufEqual_DIC(:,:,:,:,:), BufRestrict_DIC(:,:,:,:,:)

      ! Actual size of messages: full, restricted/sparse and actual face
      integer :: iSize, iSizeR, iSize1

      ! BATL related
      integer:: iNode, iDim, iSideFace

      character(len=*), parameter:: NameSub = 'pass_face'
      !------------------------------------------------------------------------
      if(DoTest)write(*,*) NameSub, &
           ':me,iFaceMin,iFaceMax,do_eq,do_re,do_pr=', &
           iProc, iFaceMin, iFaceMax

      ! Debug
      if(DoDebug)Buffer_IIBI = 0.0

      nRecvRequest = 0
      iRecvRequest_I = MPI_REQUEST_NULL

      do iFace=iFaceMin,iFaceMax
         if(DoDebug .and. DoTest)then
            write(*,*) NameSub,': me,iFace,iSize,iSizeR',&
                 iProc, iFace, iSize, iSizeR
            write(*,*)'_o=', iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO
            write(*,*)'_g=', iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG
            write(*,*)'_r=', iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR
         end if

         do iBlock = 1, nBlock
            if(Unused_B(iBlock))CYCLE

            ! Set index ranges for the face
            call set_ranges_trace(iFace, iDir, jFace, iSide, iSize, iSizeR, &
                 iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
                 iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
                 iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR)

            ! Post non-blocking receive for opposite face of neighbor block
            DiLevel = DiLevel_EB(jFace,iBlock)
            select case(DiLevel)
            case(0)
               nSubFace=1
               iSize1=iSize
            case(1)
               nSubFace=1
               iSize1=iSizeR
            case(-1)
               nSubFace=4
               iSize1=iSizeR
            case(Unset_)
               ! Do nothing
               CYCLE
            case default
               write(*,*)'me,iBlock,jFace,DiLevel=',&
                    iProc, iBlock, jFace, DiLevel
               call stop_mpi(&
                    'Error in message pass: Invalid value for DiLevel_EB')
            end select

            if(DoDebug.and.DoTest)write(*,*)&
                 'receive: me, DiLevel, nSubFace, iSize1',&
                 iProc, DiLevel, nSubFace, iSize1

            do iSubFace=1,nSubFace
               jProc = jProc_IEB(iSubFace,jFace,iBlock)
               if(jProc /= iProc)then
                  ! Remote receive
                  iTag = 100*iBlock+10*iFace+iSubFace

                  if(DoTest.and.DoDebug)write(*,*)&
                       'Remote receive, me,iTag,DiLevel,jProc=',&
                       iProc, iTag, DiLevel, jProc

                  call MPI_irecv(Buffer_IIBI(1,iSubFace,iBlock,iSide),&
                       iSize1,MPI_REAL,jProc,iTag,iComm,iRequest,iError)

                  nRecvRequest = nRecvRequest + 1
                  iRecvRequest_I(nRecvRequest) = iRequest
               end if
            end do ! iSubFace
         end do ! iBlock
      end do ! iFace

      ! Wait for all receive commands to be posted for all processors
      call barrier_mpi

      if(DoTest)write(*,*)'receives posted: me=',iProc

      ! Send blocking messages with Rsend (ready to receive)
      do iFace = iFaceMin, iFaceMax

         ! Set index ranges for the face
         call set_ranges_trace(iFace, iDir, jFace, iSide, iSize, iSizeR, &
              iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
              iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
              iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR)

         if(DoDebug.and.DoTest)write(*,*)&
              'set_ranges_trace for send Done: me, iFace=',iProc, iFace

         allocate(BufEqual_DIC(3,2,iMinO:iMaxO,jMinO:jMaxO,kMinO:kMaxO))
         allocate( &
              BufRestrict_DIC(3,2,iMinR:iMaxR,jMinR:jMaxR,kMinR:kMaxR))

         if(DoDebug)write(*,*)'allocation Done, me,iFace=', iProc, iFace

         do iBlock = 1, nBlock
            if(Unused_B(iBlock))CYCLE
            DiLevel = DiLevel_EB(iFace,iBlock)

            if(DoDebug.and.DoTest)write(*,*)&
                 'sending: me, iFace,iBlock,DiLevel=', &
                 iProc, iFace, iBlock, DiLevel

            select case(DiLevel)
            case(0)
               jProc=jProc_IEB(1,iFace,iBlock)
               jBlock=jBlock_IEB(1,iFace,iBlock)
               if(jProc==iProc)then
                  ! Local copy
                  if(DoDebug.and.DoTest)write(*,*)&
                       'local equal copy: me,iFace,iBlock=',iProc,iFace,iBlock

                  Trace_DINB(:,:,iMinG:iMaxG,jMinG:jMaxG,kMinG:kMaxG,jBlock) &
                       = max( &
                       Trace_DINB(:,:,iMinG:iMaxG,jMinG:jMaxG,kMinG:kMaxG, &
                       jBlock), &
                       Trace_DINB(:,:,iMinO:iMaxO,jMinO:jMaxO,kMinO:kMaxO, &
                       iBlock))
               else
                  ! Remote send
                  iTag = 100*jBlock+10*iFace+1
                  if(DoTest.and.DoDebug)write(*,*)&
                       'Remote equal send, me,iTag,jProc=',iProc,iTag,jProc

                  BufEqual_DIC = &
                       Trace_DINB(:,:,iMinO:iMaxO,jMinO:jMaxO,kMinO:kMaxO, &
                       iBlock)

                  call MPI_Rsend(BufEqual_DIC,&
                       iSize,MPI_REAL,jProc,iTag,iComm,iError)
               end if
            case(1)
               ! Restrict Trace_DINB in _o range into _r
               BufRestrict_DIC = &
                    Trace_DINB(:,:,iMinO:iMaxO:2,jMinO:jMaxO:2,kMinO:kMaxO:2, &
                    iBlock)

               jProc = jProc_IEB(1,iFace,iBlock)
               jBlock = jBlock_IEB(1,iFace,iBlock)
               ! Subface index =1,2,3, or 4 with respect to the coarse neighbor

               ! iSubFace = iSubFace_IA(iFace,iNode_B(iBlock))
               iNode = iNode_B(iBlock)
               iSubFace = 0
               do iDim = 1, nDim
                  iSideFace = modulo(iTree_IA(Coord0_+iDim,iNode) - 1,2)

                  if(iDim == (iFace+1)/2) CYCLE

                  if(iSubFace == 0) then
                     iSubFace = iSideFace + 1
                  else
                     iSubFace = iSubFace + 2*iSideFace
                  end if

               end do

               ! Swap subface 2 and 3 for iFace = 1..4 for BATSRUS tradition...
               if(iFace <= 4 .and. iSubFace >= 2 .and. iSubFace <= 3) &
                    iSubFace = 5 - iSubFace

               if(jProc==iProc)then
                  ! Local copy into appropriate subface
                  call set_subface_range(.false., iFace, iSubFace, &
                       iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
                       iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
                       iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR, &
                       iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS)

                  if(DoDebug.and.DoTest)write(*,*)&
                       'local restricted copy: me,iFace,iBlock,_s=', &
                       iProc, iFace, iBlock,&
                       iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS

                  Trace_DINB(:,:,iMinS:iMaxS,jMinS:jMaxS,kMinS:kMaxS,jBlock) &
                       = max(BufRestrict_DIC, &
                       Trace_DINB(:,:,iMinS:iMaxS,jMinS:jMaxS,kMinS:kMaxS, &
                       jBlock))
               else
                  ! Restrict Trace_DINB in _o range into _r
                  BufRestrict_DIC=&
                       Trace_DINB(:,:,iMinO:iMaxO:2,jMinO:jMaxO:2, &
                       kMinO:kMaxO:2,iBlock)

                  ! Remote send
                  iTag = 100*jBlock+10*iFace+iSubFace
                  if(DoTest.and.DoDebug)write(*,*)&
                       'Remote restricted send, me,iFace,iTag=',&
                       iProc,iFace,iTag
                  call MPI_Rsend(BufRestrict_DIC,iSizeR,&
                       MPI_REAL,jProc,iTag,iComm,iError)
               end if
            case(-1)
               do iSubFace = 1, 4
                  jProc = jProc_IEB(iSubFace,iFace,iBlock)
                  jBlock = jBlock_IEB(iSubFace,iFace,iBlock)

                  call set_subface_range(.true., iFace, iSubFace, &
                       iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
                       iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
                       iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR, &
                       iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS)

                  if(jProc == iProc)then
                     ! Local copy of appropriate subface
                     if(DoDebug.and.DoTest)write(*,*)&
                          'local prolonged copy: me,iSubFace,iFace,iBlock,S=',&
                          iProc,iSubFace,iFace,iBlock,&
                          iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS

                     Trace_DINB(:,:,iMinG:iMaxG:2,&
                          jMinG:jMaxG:2,&
                          kMinG:kMaxG:2,jBlock)=max(&
                          Trace_DINB(:,:,iMinG:iMaxG:2,&
                          jMinG:jMaxG:2,&
                          kMinG:kMaxG:2,jBlock),&
                          Trace_DINB(:,:,iMinS:iMaxS,jMinS:jMaxS,kMinS:kMaxS, &
                          iBlock))
                  else
                     ! Remote send
                     BufRestrict_DIC = &
                          Trace_DINB(:,:,iMinS:iMaxS,jMinS:jMaxS,kMinS:kMaxS, &
                          iBlock)

                     iTag = 100*jBlock + 10*iFace + 1
                     if(DoTest.and.DoDebug)write(*,*)&
                          'Remote prolong send, me,iFace,iTag=',&
                          iProc,iFace,iTag

                     call MPI_Rsend(BufRestrict_DIC,iSizeR,&
                          MPI_REAL,jProc,iTag,iComm,iError)
                  end if
               end do ! iSubFace

            case(Unset_)
               ! There is no neighbor, do nothing
               CYCLE
            case default
               write(*,*)'me,iBlock,iFace,DiLevel=',&
                    iProc, iBlock, iFace, DiLevel
               call stop_mpi(NameSub//': Invalid value for DiLevel_EB')
            end select ! DiLevel
         end do ! iBlock

         deallocate(BufEqual_DIC)
         deallocate(BufRestrict_DIC)

         if(DoTest) write(*,*)'messages sent, me, iFace=', iProc, iFace
      end do ! iFace

      ! wait for all messages to be received
      if (nRecvRequest > 0) call MPI_waitall(nRecvRequest, iRecvRequest_I, &
           MPI_STATUSES_IGNORE, iError)

      if(DoTest) write(*,*)'messages received, me, iDir=',iProc, iDir

      ! Copy ghost cells received from non-local neigbors
      ! and stored in the Buffer_IIBI into sol_BLK

      do iFace = iFaceMin, iFaceMax

         ! Set index ranges for the face
         call set_ranges_trace(iFace, iDir, jFace, iSide, iSize, iSizeR, &
              iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
              iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
              iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR)

         if(DoDebug.and.DoTest)write(*,*)&
              'set_ranges_trace Done: me, iFace=',iProc, iFace

         do iBlock = 1, nBlock
            if(Unused_B(iBlock))CYCLE
            select case(DiLevel_EB(jFace,iBlock))
            case(0)
               if(DoDebug) write(*,*) &
                    'call buf_to_trace_face: me, iBlock=',iProc,iBlock
               if(jProc_IEB(1,jFace,iBlock) /= iProc)&
                    call buf_to_trace_face(Buffer_IIBI(1,1,iBlock,iSide),&
                    iMinG,iMaxG,jMinG,jMaxG,kMinG,kMaxG)
            case(1)
               if(DoDebug.and.DoTest)&
                    write(*,*)'call buf_to_sparse_trace_face: me, iBlock=', &
                    iProc, iBlock
               if(jProc_IEB(1,jFace,iBlock) /= iProc)&
                    call buf_to_sparse_trace_face( &
                    Buffer_IIBI(1,1,iBlock,iSide),&
                    iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR)
            case(-1)
               do iSubFace = 1, 4
                  if(DoDebug) write(*,*) &
                       'call buf_to_trace_subface: me, iSubFace, iBlock=',&
                       iProc, iSubFace, iBlock
                  if(jProc_IEB(iSubFace,jFace,iBlock)/=iProc)&
                       call buf_to_trace_subface(&
                       Buffer_IIBI(1,iSubFace,iBlock,iSide),&
                       iFace,iSubFace,iMinR,iMaxR,jMinR,jMaxR,kMinR,kMaxR)
               end do
            end select ! DiLevel
         end do ! iBlock
      end do ! iFace

      if(DoTest)write(*,*) NameSub, ' finished: me,iFaceMin,iFaceMax=', &
           iProc, iFaceMin, iFaceMax

    end subroutine pass_face
    !==========================================================================
#endif
  end subroutine pass_trace
  !============================================================================
  subroutine set_ranges_trace(iFace, iDir, jFace, iSide, iSize, iSizeR, &
       iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
       iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
       iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR)
    !$acc routine seq

    integer, intent(in):: iFace, iDir
    integer, intent(out):: jFace, iSide, iSize, iSizeR
    integer, intent(out) :: iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO
    integer, intent(out) :: iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG
    integer, intent(out) :: iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR

    ! Set ranges orthogonal to iDir based on the value of iDir
    !--------------------------------------------------------------------------
    if(iDir /= 1)then
       iMinG = 1;  iMaxG = nI + 1
       iMinO = 1;  iMaxO = nI + 1
       iMinR = 1;  iMaxR = nI/2 + 1
    end if

    if(iDir /= 2)then
       jMinG = 1;  jMaxG = nJ + 1
       jMinO = 1;  jMaxO = nJ + 1
       jMinR = 1;  jMaxR = nJ/2 + 1
    endif

    if(iDir /= 3)then
       kMinG = 1;  kMaxG = nK + 1
       kMinO = 1;  kMaxO = nK + 1
       kMinR = 1;  kMaxR = nK/2 + 1
    end if

    ! Set ranges in direction of iDir based on the value of iFace

    select case(iFace)
    case(1)
       jFace = 2;    iSide = 1
       iMinO = 1;    iMaxO = 1
       iMinG = nI+1; iMaxG = nI+1
       iMinR = 1;    iMaxR = 1
    case(3)
       jFace = 4;    iSide = 1
       jMinO = 1;    jMaxO = 1
       jMinG = nJ+1; jMaxG = nJ+1
       jMinR = 1;    jMaxR = 1
    case(5)
       jFace = 6;    iSide = 1
       kMinO = 1;    kMaxO = 1
       kMinG = nK+1; kMaxG = nK+1
       kMinR = 1;    kMaxR = 1
    case(2)
       jFace = 1;    iSide = 2
       iMinO = nI+1; iMaxO = nI+1
       iMinG = 1;    iMaxG = 1
       iMinR = 1;    iMaxR = 1
    case(4)
       jFace = 3;    iSide = 2
       jMinO = nJ+1; jMaxO = nJ+1
       jMinG = 1;    jMaxG = 1
       jMinR = 1;    jMaxR = 1
    case(6)
       jFace = 5;    iSide = 2
       kMinO = nK+1; kMaxO = nK+1
       kMinG = 1;    kMaxG = 1
       kMinR = 1;    kMaxR = 1
    end select

    ! Size of full and restricted cell layers
    iSize  = 6*(iMaxG - iMinG + 1)*(jMaxG - jMinG + 1)*(kMaxG - kMinG + 1)
    iSizeR = 6*(iMaxR - iMinR + 1)*(jMaxR - jMinR + 1)*(kMaxR - kMinR + 1)

  end subroutine set_ranges_trace
  !============================================================================
  subroutine set_subface_range(DoSend, iFace, iSubFace, &
       iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
       iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
       iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR, &
       iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS)
    !$acc routine seq

    logical, intent(in) :: DoSend
    integer, intent(in) :: iFace, iSubFace
    integer, intent(in) :: iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO
    integer, intent(in) :: iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG
    integer, intent(in) :: iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR
    integer, intent(out) :: iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS

    ! Select appropriate quarter of ghost cell layer
    !--------------------------------------------------------------------------
    select case(iFace)
    case(1, 2)
       if(DoSend)then
          iMinS = iMinO; iMaxS = iMaxO
       else
          iMinS = iMinG; iMaxS = iMaxG
       end if

       select case(iSubFace)
          ! Beware, case(2) and case(3) are swapped
       case(1)
          jMinS = jMinR; jMaxS = jMaxR
          kMinS = kMinR; kMaxS = kMaxR
       case(3)
          jMinS = jMinR + nJ/2; jMaxS = jMaxR + nJ/2;
          kMinS = kMinR; kMaxS = kMaxR
       case(2)
          jMinS = jMinR; jMaxS = jMaxR;
          kMinS = kMinR + nK/2; kMaxS = kMaxR + nK/2;
       case(4)
          jMinS = jMinR + nJ/2; jMaxS = jMaxR + nJ/2;
          kMinS = kMinR + nK/2; kMaxS = kMaxR + nK/2;
       end select
    case(3, 4)
       if(DoSend)then
          jMinS = jMinO; jMaxS = jMaxO
       else
          jMinS = jMinG; jMaxS = jMaxG
       end if
       select case(iSubFace)
          ! Beware, case(2) and case(3) are swapped
       case(1)
          iMinS = iMinR;        iMaxS = iMaxR;
          kMinS = kMinR;        kMaxS = kMaxR
       case(3)
          iMinS = iMinR + nI/2; iMaxS = iMaxR + nI/2;
          kMinS = kMinR;        kMaxS = kMaxR
       case(2)
          iMinS = iMinR;        iMaxS = iMaxR;
          kMinS = kMinR + nK/2; kMaxS = kMaxR + nK/2;
       case(4)
          iMinS = iMinR + nI/2; iMaxS = iMaxR + nI/2;
          kMinS = kMinR + nK/2; kMaxS = kMaxR + nK/2;
       end select
    case(5, 6)
       if(DoSend)then
          kMinS = kMinO; kMaxS = kMaxO
       else
          kMinS = kMinG; kMaxS = kMaxG
       end if
       select case(iSubFace)
          ! Beware, case(2) and case(3) are not swapped
       case(1)
          iMinS = iMinR;        iMaxS = iMaxR;
          jMinS = jMinR;        jMaxS = jMaxR
       case(2)
          iMinS = iMinR + nI/2; iMaxS = iMaxR + nI/2;
          jMinS = jMinR;        jMaxS = jMaxR
       case(3)
          iMinS = iMinR;        iMaxS = iMaxR;
          jMinS = jMinR + nJ/2; jMaxS = jMaxR + nJ/2;
       case(4)
          iMinS = iMinR + nI/2; iMaxS = iMaxR + nI/2;
          jMinS = jMinR + nJ/2; jMaxS = jMaxR + nJ/2;
       end select
    end select

  end subroutine set_subface_range
  !============================================================================
  subroutine trace_face_to_buf(Trace_DINB, &
       iMin, iMax, jMin, jMax, kMin, kMax, &
       iBlock, iSide, jProc, MaxSizeR, BufferS_IBIP)
    !$acc routine vector

    ! put trace into buf for equal resolution, or put prolonged trace into buf

    integer, intent(in) :: iMin,iMax,jMin,jMax,kMin,kMax
    integer, intent(in) :: iBlock, iSide, jProc, MaxSizeR
    real, intent(in) ::Trace_DINB(3,2,nI+1,nJ+1,nK+1,MaxBlock)
    real, intent(inout):: BufferS_IBIP(MaxSizeR*4,MaxBlock,2,0:nProc-1)

    integer :: iTraceLine, i, j, k, iTrace, iDim
    !--------------------------------------------------------------------------
    !$acc loop vector collapse(5) private(iTraceLine)
    do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
       do iTrace = 1, 2; do iDim=1,3
          iTraceLine = (k-kMin)*(jMax-jMin+1)*(iMax-iMin+1)*6 +&
               (j-jMin)*(iMax-iMin+1)*6 + (i-iMin)*6 + &
               (iTrace-1)*3 + iDim

          BufferS_IBIP(iTraceLine,iBlock,iSide,jProc) = &
               Trace_DINB(iDim,iTrace,i,j,k,iBlock)
       end do; end do
    end do; end do; end do

  end subroutine trace_face_to_buf
  !============================================================================
  subroutine trace_subface_to_buf(Trace_DINB, &
       iMin, iMax, jMin, jMax, kMin, kMax, &
       iBlock, iSide, jProc, MaxSizeR, BufferS_IBIP, iSubFaceIn)
    !$acc routine vector

    ! put restrited trace into buffer

    integer, intent(in) :: iMin, iMax, jMin, jMax, kMin, kMax
    integer, intent(in) :: iBlock, iSide, jProc, MaxSizeR
    real, intent(in) ::Trace_DINB(3,2,nI+1,nJ+1,nK+1,MaxBlock)
    real, intent(inout):: BufferS_IBIP(MaxSizeR*4,MaxBlock,2,0:nProc-1)

    integer, intent(in):: iSubFaceIn

    integer :: iTraceLine, i, j, k, iTrace, iDim
    !--------------------------------------------------------------------------
    !$acc loop vector collapse(5) private(iTraceLine)
    do k = kMin, kMax, 2; do j = jMin, jMax, 2; do i = iMin, iMax, 2
       do iTrace = 1, 2; do iDim = 1, 3
          iTraceLine = (k-kMin)/2*((jMax-jMin)/2+1)*((iMax-iMin)/2+1)*6 + &
               (j-jMin)/2*((iMax-iMin)/2+1)*6 + (i-iMin)/2*6 + &
               (iTrace-1)*3 + iDim + (iSubFaceIn-1)*MaxSizeR

          BufferS_IBIP(iTraceLine,iBlock,iSide,jProc) = &
               Trace_DINB(iDim,iTrace,i,j,k,iBlock)
       end do; end do
    end do; end do; end do

  end subroutine trace_subface_to_buf
  !============================================================================
  subroutine prolong_trace(iFace, iBlock)
    !$acc routine vector

    ! For faces that are shared with a coarser neighbor, interpolate for all
    ! points which are not coinciding and where the trace is going out.
    !
    ! a at odd  j and even k requires interpolation in direction k
    ! b at even j and odd  k requires interpolation in direction j
    ! c at even j and even k requires interpolation in both directions

    ! (1,5)           (5,5)
    !   O-- b --O-- b --O
    !   |   |   |   |   |
    !   |   |   |   |   |
    !   a - c - a - c - a
    !   |   |   |   |   |
    !   |   |   |   |   |
    !   O-- b --O-- b --O
    !   |   |   |   |   |
    !   |   |   |   |   |
    !   a - c - a - c - a
    !   |   |   |   |   |
    !   |   |   |   |   |
    !   O-- b --O-- b --O
    ! (1,1)           (5,1)

    integer, intent(in):: iFace, iBlock

    integer :: i, j, k, iTrace, iGang

    ! Q: why not parameter arrays / scalars ???
    ! A: If Weight4_I/Weight2_I are defined as 'parameter', nvfortran+openacc
    !    will produce compilation error.
    ! Interpolation weights
    real :: Weight4_I(4) != 0.25
    real :: Weight2_I(2) != 0.5

    real :: Trace_DI(3,2), Trace4_DI(3,4)

#ifndef _OPENACC
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'prolong_trace'
    !--------------------------------------------------------------------------
    if(DoTest)write(*,*) NameSub,': me, iBlock, iFace=',iProc, iBlock, iFace
#endif
    iGang = i_gang(iBlock)

    ! Extract Trace_DIII and IjkTrace_DII for the appropriate face
    ! NOTE: IjkTrace_DII assignment split to two lines to avoid
    ! reshaping compiler bug!
    select case(iFace)
    case(1)
       !$acc loop vector collapse(2)
       do k = 1, nK+1; do j = 1, nJ+1
          Trace_DIIII(:,:,j,k,iGang) = Trace_DINB(:,:,1,j,k,iBlock)
          IjkTrace_DIII(1,j,k,iGang) = I_DINB(1,1,1,j,k,iBlock)
          IjkTrace_DIII(2,j,k,iGang) = I_DINB(1,2,1,j,k,iBlock)
       end do; end do
    case(2)
       !$acc loop vector collapse(2)
       do k = 1, nK+1; do j = 1, nJ+1
          Trace_DIIII(:,:,j,k,iGang) = Trace_DINB(:,:,nI+1,j,k,iBlock)
          IjkTrace_DIII(1,j,k,iGang) = I_DINB(1,1,nI+1,j,k,iBlock)
          IjkTrace_DIII(2,j,k,iGang) = I_DINB(1,2,nI+1,j,k,iBlock)
       end do; end do
    case(3)
       !$acc loop vector collapse(2)
       do k = 1, nK+1; do i = 1, nI+1
          Trace_DIIII(:,:,i,k,iGang) = Trace_DINB(:,:,i,1,k,iBlock)
          IjkTrace_DIII(1,i,k,iGang) = I_DINB(1,1,i,1,k,iBlock)
          IjkTrace_DIII(2,i,k,iGang) = I_DINB(1,2,i,1,k,iBlock)
       end do; end do
    case(4)
       !$acc loop vector collapse(2)
       do k = 1, nK+1; do i = 1, nI+1
          Trace_DIIII(:,:,i,k,iGang) = Trace_DINB(:,:,i,nJ+1,k,iBlock)
          IjkTrace_DIII(1,i,k,iGang) = I_DINB(1,1,i,nJ+1,k,iBlock)
          IjkTrace_DIII(2,i,k,iGang) = I_DINB(1,2,i,nJ+1,k,iBlock)
       end do; end do
    case(5)
       !$acc loop vector collapse(2)
       do j = 1, nJ+1; do i = 1, nI+1
          Trace_DIIII(:,:,i,j,iGang) = Trace_DINB(:,:,i,j,1,iBlock)
          IjkTrace_DIII(1,i,j,iGang) = I_DINB(1,1,i,j,1,iBlock)
          IjkTrace_DIII(2,i,j,iGang) = I_DINB(1,2,i,j,1,iBlock)
       end do; end do
    case(6)
       !$acc loop vector collapse(2)
       do j = 1, nJ+1; do i = 1, nI+1
          Trace_DIIII(:,:,i,j,iGang) = Trace_DINB(:,:,i,j,nK+1,iBlock)
          IjkTrace_DIII(1,i,j,iGang) = I_DINB(1,1,i,j,nK+1,iBlock)
          IjkTrace_DIII(2,i,j,iGang) = I_DINB(1,2,i,j,nK+1,iBlock)
       end do; end do
    case default
#ifndef _OPENACC
       call stop_mpi(NameSub//': Impossible value for iFace')
#endif
    end select

    !$acc loop vector collapse(2) &
    !$acc private(Trace_DI, Trace4_DI, Weight2_I, Weight4_I)
    do iTrace = 1, 2
       do k = 1, nK+1
          Weight2_I = 0.5
          Weight4_I = 0.25

          if(mod(k, 2) == 1)then
             do j = 2, nJ+1, 2
                ! Case b: even j and odd k

                if(IjkTrace_DIII(iTrace,j,k,iGang) /= RayOut_)CYCLE

                Trace_DI(:,1) = Trace_DIIII(:,iTrace,j-1,k,iGang)
                Trace_DI(:,2) = Trace_DIIII(:,iTrace,j+1,k,iGang)

                call interpolate_trace_face(Trace_DI, Weight2_I, 2, &
                     Trace_DIIII(:,iTrace,j,k,iGang))
             end do
          else
             do j = 1, nJ+1, 2
                ! Case a: odd j and even k

                if(IjkTrace_DIII(iTrace,j,k,iGang) /= RayOut_)CYCLE

                Trace_DI(:,1) = Trace_DIIII(:,iTrace,j,k-1,iGang)
                Trace_DI(:,2) = Trace_DIIII(:,iTrace,j,k+1,iGang)
                call interpolate_trace_face(Trace_DI, Weight2_I, 2,&
                     Trace_DIIII(:,iTrace,j,k,iGang))
             end do
             do j = 2, nJ, 2
                ! Case c: even j and even k

                if(IjkTrace_DIII(iTrace,j,k,iGang) /= RayOut_)CYCLE

                Trace4_DI(:,1) = Trace_DIIII(:,iTrace,j-1,k-1,iGang)
                Trace4_DI(:,2) = Trace_DIIII(:,iTrace,j+1,k-1,iGang)
                Trace4_DI(:,3) = Trace_DIIII(:,iTrace,j-1,k+1,iGang)
                Trace4_DI(:,4) = Trace_DIIII(:,iTrace,j+1,k+1,iGang)
                call interpolate_trace_face(Trace4_DI, Weight4_I, 4,&
                     Trace_DIIII(:,iTrace,j,k,iGang))

             end do ! j
          end if ! mod(k,2)
       end do ! k
    end do ! iTrace

    ! Put back result into Trace_DINB
    select case(iFace)
    case(1)
       !$acc loop vector collapse(2)
       do k = 1, nK+1; do j = 1, nJ+1
          Trace_DINB(:,:,1,j,k,iBlock) = Trace_DIIII(:,:,j,k,iGang)
       end do; end do
    case(2)
       !$acc loop vector collapse(2)
       do k = 1, nK+1; do j = 1, nJ+1
          Trace_DINB(:,:,nI+1,j,k,iBlock) = Trace_DIIII(:,:,j,k,iGang)
       end do; end do
    case(3)
       !$acc loop vector collapse(2)
       do k = 1, nK+1; do i = 1, nI+1
          Trace_DINB(:,:,i,1,k,iBlock) = Trace_DIIII(:,:,i,k,iGang)
       end do; end do
    case(4)
       !$acc loop vector collapse(2)
       do k = 1, nK+1; do i = 1, nI+1
          Trace_DINB(:,:,i,nJ+1,k,iBlock) = Trace_DIIII(:,:,i,k,iGang)
       end do; end do
    case(5)
       !$acc loop vector collapse(2)
       do j = 1, nJ+1; do i = 1, nJ+1
          Trace_DINB(:,:,i,j,1,iBlock) = Trace_DIIII(:,:,i,j,iGang)
       end do; end do
    case(6)
       !$acc loop vector collapse(2)
       do j = 1, nJ+1; do i = 1, nI+1
          Trace_DINB(:,:,i,j,nK+1,iBlock) = Trace_DIIII(:,:,i,j,iGang)
       end do; end do
    end select

  end subroutine prolong_trace
  !============================================================================
#ifdef _OPENACC
  subroutine buf_to_trace_face_gpu(BufferR_IBIP, Trace_DINB, MaxSizeR, &
       iMin, iMax, jMin, jMax, kMin, kMax, iBlock, jBlock, jSide, jProc)
    !$acc routine vector

    ! Take maximum of Trace_DINB and buf (more positive values are more real)
    ! for the full face

    integer, intent(in):: iMin, iMax, jMin, jMax, kMin, kMax, MaxSizeR
    integer, intent(in):: iBlock, jBlock, jSide, jProc
    real,    intent(in):: BufferR_IBIP(MaxSizeR*4,MaxBlock,2,0:nProc-1)
    real, intent(inout):: Trace_DINB(3,2,nI+1,nJ+1,nK+1,MaxBlock)

    integer :: i, j, k, iDim, iTrace, iTraceLine
    !--------------------------------------------------------------------------
    !$acc loop vector collapse(5) private(iTraceLine)
    do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
       do iTrace = 1, 2; do iDim = 1, 3
          iTraceLine = (k-kMin)*(jMax-jMin+1)*(iMax-iMin+1)*6 +&
               (j-jMin)*(iMax-iMin+1)*6 + (i-iMin)*6 + (iTrace-1)*3 + iDim

          Trace_DINB(iDim,iTrace,i,j,k,iBlock) = &
               max(BufferR_IBIP(iTraceLine,jBlock,jSide,jProc), &
               Trace_DINB(iDim,iTrace,i,j,k,iBlock))
       end do; end do
    end do; end do; end do

  end subroutine buf_to_trace_face_gpu
  !============================================================================
  subroutine buf_to_sparse_trace_face_gpu( &
       BufferR_IBIP, Trace_DINB, MaxSizeR, &
       iMin, iMax, jMin, jMax, kMin, kMax, iBlock, jBlock, jSide, jProc)
    !$acc routine vector

    ! Take maximum of Trace_DINB and buf (more positive values are more real)
    ! for a factor of 2 coarser grid

    integer, intent(in) :: iMin, iMax, jMin, jMax, kMin, kMax, MaxSizeR
    integer, intent(in) :: iBlock,jBlock,jSide,jProc
    real, intent(in) :: BufferR_IBIP(MaxSizeR*4,MaxBlock,2,0:nProc-1)
    real, intent(inout):: Trace_DINB(3,2,nI+1,nJ+1,nK+1,MaxBlock)

    integer :: i, j, k, iDim, iTrace, iTraceLine
    !--------------------------------------------------------------------------
    !$acc loop vector collapse(5) private(iTraceLine)
    do k = kMin, kMax, 2; do j = jMin, jMax, 2; do i = iMin, iMax, 2
       do iTrace = 1, 2; do iDim = 1, 3
          iTraceLine = (k-kMin)/2*((jMax-jMin)/2+1)*((iMax-iMin)/2+1)*6 +&
               (j-jMin)/2*((iMax-iMin)/2+1)*6 + (i-iMin)/2*6 + &
               (iTrace-1)*3 + iDim

          Trace_DINB(iDim,iTrace,i,j,k,iBlock) = &
               max(BufferR_IBIP(iTraceLine,jBlock,jSide,jProc), &
               Trace_DINB(iDim,iTrace,i,j,k,iBlock))
       end do; end do
    end do; end do; end do

  end subroutine buf_to_sparse_trace_face_gpu
  !============================================================================
  subroutine buf_to_trace_subface_gpu(BufferR_IBIP, Trace_DINB, MaxSizeR, &
       iMin, iMax, jMin, jMax, kMin, kMax, iBlock, jBlock, jSide, jProc, &
       iSubFaceIn)
    !$acc routine vector

    ! Take maximum of Trace_DINB and buf (more positive values are more real)
    ! buf is from a factor of 2 finer grid

    integer, intent(in) :: iMin, iMax, jMin, jMax, kMin, kMax, MaxSizeR
    integer, intent(in) :: iBlock, jBlock, jSide, jProc, iSubFaceIn
    real, intent(in) :: BufferR_IBIP(MaxSizeR*4,MaxBlock,2,0:nProc-1)
    real, intent(inout):: Trace_DINB(3,2,nI+1,nJ+1,nK+1,MaxBlock)

    integer :: i, j, k, iDim, iTrace, iTraceLine
    !--------------------------------------------------------------------------
    !$acc loop vector collapse(5) private(iTraceLine)
    do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
       do iTrace = 1, 2; do iDim = 1, 3
          iTraceLine = (k-kMin)*((jMax-jMin)+1)*((iMax-iMin)+1)*6 + &
               (j-jMin)*((iMax-iMin)+1)*6 + (i-iMin)*6 + &
               (iTrace-1)*3 + iDim + (iSubFaceIn-1)*MaxSizeR

          Trace_DINB(iDim,iTrace,i,j,k,iBlock) = &
               max(BufferR_IBIP(iTraceLine,jBlock,jSide,jProc), &
               Trace_DINB(iDim,iTrace,i,j,k,iBlock))
       end do; end do
    end do; end do; end do;

  end subroutine buf_to_trace_subface_gpu
  !============================================================================
  subroutine pass_face_gpu(iDir, iFaceMin, iFaceMax)

    ! Avoid double-contained subroutine for GPU code (nvfortran 25 bug)
    ! On 1 gpu: finish local copy; on >1 gpus: fill in send buffer

    use ModParallel, ONLY : Unset_, DiLevel_EB, jBlock_IEB, jProc_IEB
    use ModMpi

    integer, intent(in):: iDir, iFaceMin,iFaceMax

    ! Face, neighbor's face and side indexes Note: iFace is on CPU
    integer :: iFace, jFace, iSide, iBlock

    ! number of subfaces (1 or 4), subface (1..nSubFace)
    integer ::  nSubFace, iSubFace

    ! Array ranges for outgoing, incoming, restricted and subfaces
    integer :: iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO
    integer :: iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG
    integer :: iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR
    integer :: iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS

    integer:: iS, jS, kS, iR, jR, kR, iTrace

    ! Descriptors for neighbor
    integer :: jProc, jBlock, DiLevel

    ! Maximum size of the RESTRICTED Trace_DINB layer to be received
    ! for the 6 trace variables (3 coordinates * 2 trace directions)
    integer, parameter :: MaxSizeR = &
         6*max((nI/2+1)*(nJ/2+1),(nI/2+1)*(nK/2+1),(nJ/2+1)*(nK/2+1))

    ! MPI variables
    integer :: iRequestR, iRequestS, iProcSend, iError
    integer, allocatable :: iRequestR_I(:), iRequestS_I(:)

    ! Receive Buffer_IIBI to hold 4 incoming RESTRICTED Trace_DINB values
    ! for all blocks and for both sides
    real, allocatable :: BufferS_IBIP(:,:,:,:)
    real, allocatable :: BufferR_IBIP(:,:,:,:)
    !$acc declare create(BufferS_IBIP, BufferR_IBIP)

    ! Actual size of messages: full, restricted/sparse and actual face
    integer :: iSize, iSizeR, iSize1

    ! BATL related
    integer:: iNode, iDim, iSideFace
    !--------------------------------------------------------------------------
    if(nProc > 1)then
       if (.not. allocated(BufferS_IBIP))&
            allocate(BufferS_IBIP(MaxSizeR*4,MaxBlock,2,0:nProc-1))
       if (.not. allocated(BufferR_IBIP))&
            allocate(BufferR_IBIP(MaxSizeR*4,MaxBlock,2,0:nProc-1))
       if (.not. allocated(iRequestS_I)) &
            allocate(iRequestS_I(1:nProc-1))
       if (.not. allocated(iRequestR_I)) &
            allocate(iRequestR_I(1:nProc-1))
    end if

    call timing_start('pass_trace1')
    do iFace = iFaceMin, iFaceMax

       call set_ranges_trace(iFace, iDir, jFace, iSide, iSize, iSizeR, &
            iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
            iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
            iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR)

       !$acc parallel loop gang independent
       do iBlock = 1, nBlock
          if(Unused_B(iBlock))CYCLE
          DiLevel = DiLevel_EB(iFace,iBlock)

          select case(DiLevel)
          case(0)
             jProc = jProc_IEB(1,iFace,iBlock)
             jBlock = jBlock_IEB(1,iFace,iBlock)
             iSize1 = iSize

             if(jProc == iProc)then
                ! Local copy

                !$acc loop vector collapse(3) private(iS, jS, kS)
                do kR = kMinG,kMaxG; do jR = jMinG,jMaxG; do iR = iMinG,iMaxG
                   iS = iR - iMinG + iMinO
                   jS = jR - jMinG + jMinO
                   kS = kR - kMinG + kMinO

                   Trace_DINB(:,:,iR,jR,kR,jBlock) = max( &
                        Trace_DINB(:,:,iR,jR,kR,jBlock), &
                        Trace_DINB(:,:,iS,jS,kS,iBlock))
                end do; end do; end do
             else
                call trace_face_to_buf(Trace_DINB, &
                     iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
                     iBlock, iSide, jProc, MaxSizeR, BufferS_IBIP)
             end if
          case(1)
             jProc = jProc_IEB(1,iFace,iBlock)
             jBlock = jBlock_IEB(1,iFace,iBlock)
             iSize1 = iSizeR
             ! Subface index =1,2,3, or 4 with respect to the coarse neighbor

             ! iSubFace = iSubFace_IA(iFace,iNode_B(iBlock))
             iNode = iNode_B(iBlock)
             iSubFace = 0

             !$acc loop seq
             do iDim = 1, nDim
                iSideFace = modulo(iTree_IA(Coord0_+iDim,iNode) - 1, 2)

                if(iDim == (iFace+1)/2) CYCLE

                if(iSubFace == 0) then
                   iSubFace = iSideFace + 1
                else
                   iSubFace = iSubFace + 2*iSideFace
                end if

             end do

             ! Swap subface 2 and 3 for iFace = 1..4 for BATSRUS tradition...
             if(iFace <= 4 .and. iSubFace >= 2 .and. iSubFace <= 3) &
                  iSubFace = 5 - iSubFace

             if(jProc == iProc)then
                ! Local copy into appropriate subface
                call set_subface_range(.false., iFace, iSubFace, &
                     iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
                     iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
                     iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR, &
                     iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS)

                !$acc loop vector collapse(3) private(iS, jS, kS)
                do kR = kMinS,kMaxS; do jR = jMinS,jMaxS; do iR = iMinS,iMaxS
                   iS = 2*(iR - iMinS) + iMinO
                   jS = 2*(jR - jMinS) + jMinO
                   kS = 2*(kR - kMinS) + kMinO

                   do iTrace = 1, 2; do iDim = 1, 3
                      !$acc atomic update
                      Trace_DINB(iDim,iTrace,iR,jR,kR,jBlock) = max( &
                           Trace_DINB(iDim,iTrace,iS,jS,kS,iBlock), &
                           Trace_DINB(iDim,iTrace,iR,jR,kR,jBlock))
                   end do; end do
                end do; end do; end do
             else
                call trace_subface_to_buf(Trace_DINB, &
                     iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
                     iBlock, iSide, jProc, MaxSizeR, BufferS_IBIP, iSubFace)
             end if
          case(-1)
             do iSubFace = 1, 4
                jProc = jProc_IEB(iSubFace,iFace,iBlock)
                jBlock = jBlock_IEB(iSubFace,iFace,iBlock)
                iSize1 = iSizeR

                call set_subface_range(.true., iFace, iSubFace, &
                     iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
                     iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
                     iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR, &
                     iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS)

                if(jProc == iProc)then
                   ! Local copy of appropriate subface

                   !$acc loop vector collapse(3) private(iS, jS, kS)
                   do kR = kMinG, kMaxG, 2
                      do jR = jMinG, jMaxG, 2
                         do iR = iMinG, iMaxG, 2
                            iS = 0.5*(iR - iMinG) + iMinS
                            jS = 0.5*(jR - jMinG) + jMinS
                            kS = 0.5*(kR - kMinG) + kMinS

                            Trace_DINB(:,:,iR,jR,kR,jBlock) = max(&
                                 Trace_DINB(:,:,iR,jR,kR,jBlock),&
                                 Trace_DINB(:,:,iS,jS,kS,iBlock))
                         end do
                      end do
                   end do
                else
                   call trace_face_to_buf(Trace_DINB, &
                        iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS, &
                        iBlock, iSide, jProc, MaxSizeR, BufferS_IBIP)
                end if
             end do ! iSubFace

          case(Unset_)
             ! There is no neighbor, do nothing
             CYCLE
          case default
             ! pass
          end select ! DiLevel
       end do ! iBlock
    end do ! iFace
    call timing_stop('pass_trace1')

    if(nProc > 1)then
       iRequestS = 0
       iSize1 = MaxSizeR * 4 * MaxBlock * 2

       !$acc host_data use_device(BufferS_IBIP)
       do iProcSend = 0, nProc - 1
          if(iProcSend == iProc) CYCLE
          iRequestS = iRequestS + 1
          call MPI_isend(BufferS_IBIP(1,1,1,iProcSend), iSize1, MPI_REAL, &
               iProcSend, 10, iComm, iRequestS_I(iRequestS), iError)
       end do
       !$acc end host_data

       iRequestR = 0
       !$acc host_data use_device(BufferR_IBIP)
       do iProcSend = 0, nProc - 1
          if(iProcSend == iProc) CYCLE
          iRequestR = iRequestR + 1
          call MPI_irecv(BufferR_IBIP(1,1,1,iProcSend), iSize1, MPI_REAL, &
               iProcSend, 10, iComm, iRequestR_I(iRequestR), iError)
       end do
       !$acc end host_data

       if(iRequestS > 0) then
          call MPI_waitall(iRequestS, iRequestS_I, MPI_STATUSES_IGNORE, &
               iError)
       end if
       if(iRequestR > 0) then
          call MPI_waitall(iRequestR, iRequestR_I, MPI_STATUSES_IGNORE, &
               iError)
       end if

       call timing_start('pass_trace2')

       ! retrieve traces from buffer
       ! on the receiving buffer, iBlock -> jBlock, but jSide = iSide
       do iFace = iFaceMin, iFaceMax

          ! Set index ranges for the face
          call set_ranges_trace(iFace, iDir, jFace, iSide, iSize, iSizeR, &
               iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
               iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
               iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR)

          if(DoDebug)write(*,*) &
               'set_ranges_trace for buf_to_race Done: me, iFace=', &
               iProc, iFace

          !$acc parallel loop gang independent
          do iBlock = 1, nBlock
             if(Unused_B(iBlock))CYCLE

             select case(DiLevel_EB(jFace,iBlock))
             case(0)
                jProc = jProc_IEB(1,jFace,iBlock)
                jBlock = jBlock_IEB(1,jFace,iBlock)
                iSize1 = iSize
                if(DoDebug)&
                     write(*,*)'buf_to_trace_face: me, iBlock, jBlock=',&
                     iProc, iBlock, jBlock
                if(jProc /= iProc) then
                   call buf_to_trace_face_gpu( &
                        BufferR_IBIP, Trace_DINB, MaxSizeR, &
                        iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
                        iBlock, jBlock, iSide, jProc)
                end if
             case(1) ! neighbour is coarser
                jProc = jProc_IEB(1,jFace,iBlock)
                jBlock = jBlock_IEB(1,jFace,iBlock)
                if(DoDebug) write(*,*)'buf_to_sparse_trace_face: me,iBlock=', &
                     iProc,iBlock
                if(jProc /= iProc)then
                   call buf_to_sparse_trace_face_gpu( &
                        BufferR_IBIP, Trace_DINB, MaxSizeR, &
                        iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
                        iBlock, jBlock, iSide, jProc)
                end if
             case(-1)
                do iSubFace = 1, 4
                   jProc = jProc_IEB(iSubFace,jFace,iBlock)
                   jBlock = jBlock_IEB(iSubFace,jFace,iBlock)
                   if(DoDebug) &
                        write(*,*)'buf_to_trace_subface: me,iSubFace,iBlock=',&
                        iProc, iSubFace, iBlock
                   if(jProc /= iProc)then
                      call set_subface_range(.false., iFace, iSubFace, &
                           iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
                           iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
                           iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR, &
                           iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS)
                      call buf_to_trace_subface_gpu(BufferR_IBIP, &
                           Trace_DINB, MaxSizeR, &
                           iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS, &
                           iBlock, jBlock, iSide, jProc, iSubFace)
                   end if
                end do
             end select ! DiLevel
          end do ! iBlock
       end do ! iFace
       call timing_stop('pass_trace2')
    end if ! nProc > 1

  end subroutine pass_face_gpu
  !============================================================================
#endif
  subroutine calc_squash_factor
    ! Calculatte squashing factor

    use ModInterpolate, ONLY: bilinear

    ! Last time the squashing factor has been calculated
    integer :: nStepLast = -1
    integer :: nLonSquash = 360, nLatSquash = 180

    integer:: i, j, k, iSide, iBlock, iStatus
    real:: Lon, Lat, Squash, SquashFactor

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_squash_factor'
    !--------------------------------------------------------------------------
    if(nStep == nStepLast) RETURN
    nStepLast = nStep

    call test_start(NameSub, DoTest)

    call trace_field_sphere
    if(DoTest)then
       write(*,*) NameSub,' minval(SquashFactor_II)=', minval(SquashFactor_II)
       write(*,*) NameSub,' maxval(SquashFactor_II)=', maxval(SquashFactor_II)
    end if

    call trace_field_grid

    if(.not.allocated(SquashFactor_GB)) then
       allocate(SquashFactor_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
       SquashFactor_GB = 1.0
    end if

    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          iStatus = nint(Trace_DSNB(3,1,i,j,k,iBlock))
          SquashFactor = 0.0
          do iSide = 1, 2
             ! Take footpoints at the inner boundary only
             if(iSide /= iStatus .and. iStatus /= 3) CYCLE
             ! Normalized longitude and latitude
             ! 1...nLonSquash+1
             Lon = Trace_DSNB(2,iSide,i,j,k,iBlock)*nLonSquash/360.0 + 1
             ! 0...nLatSquash
             Lat = (Trace_DSNB(1,iSide,i,j,k,iBlock) + 90)*nLatSquash/180.0
             Squash = bilinear(SquashFactor_II, &
                  1, nLonSquash+1, 0, nLatSquash, [Lon, Lat])
             if(iStatus == 3) Squash = 0.5*Squash ! Average two sides
             SquashFactor = SquashFactor + Squash
          end do
          if(SquashFactor > 0) SquashFactor_GB(i,j,k,iBlock) = SquashFactor
       end do; end do; end do
    end do

    ! Fill in ghost cells
    call message_pass_cell(SquashFactor_GB)

    if(DoTest)then
       write(*,*) NameSub,' minval(SquashFactor_GB)=',minval(SquashFactor_GB)
       write(*,*) NameSub,' maxval(SquashFactor_GB)=',maxval(SquashFactor_GB)
    end if
    call test_stop(NameSub, DoTest)

  end subroutine calc_squash_factor
  !============================================================================
end module ModFieldTraceFast
!==============================================================================
