!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModFieldTraceFast

  use ModFieldTrace

  use BATL_lib, ONLY: &
       test_start, test_stop, StringTest, xTest, yTest, zTest, &
       iTest, jTest, kTest, iBlockTest, iProc, iComm, nProc, &
       nDim, nI, nJ, nK, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, &
       nBlock, MaxBlock, Unused_B, IsCartesianGrid
  use ModMain, ONLY: TypeCoordSystem
#ifdef OPENACC
  use ModUtilities, ONLY: norm2
#endif

  implicit none

  SAVE

  private ! except
  public:: trace_field_grid           ! trace field from 3D MHD grid cells

  ! Local variables

  ! Stored face and cell indices of the 2 rays starting from a face of a block
  integer, allocatable :: IjkTrace_DINB(:,:,:,:,:,:)
  !$acc declare create(IjkTrace_DINB)

  ! Stored weights for the 2 rays starting from a face of a block
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
  logical :: DoTestRay = .false.

contains
  !============================================================================
  subroutine init_mod_trace_fast
    !--------------------------------------------------------------------------
    if(.not.allocated(Trace_DINB)) &
         allocate(Trace_DINB(3,2,nI+1,nJ+1,nK+1,MaxBlock))
    if(.not.allocated(IjkTrace_DINB)) &
         allocate(IjkTrace_DINB(3,2,nI+1,nJ+1,nK+1,MaxBlock))
    if(.not.allocated(WeightTrace_DINB)) &
         allocate(WeightTrace_DINB(4,2,nI+1,nJ+1,nK+1,MaxBlock))
    if(.not.allocated(b_DNB)) &
         allocate(b_DNB(3,nI+1,nJ+1,nK+1,MaxBlock))
    if(.not.allocated(b_DGB)) &
         allocate(b_DGB(3,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
    if(.not.allocated(ray))then
       allocate(ray(3,2,nI+1,nJ+1,nK+1,MaxBlock))
       ray = 0.0
    end if

  end subroutine init_mod_trace_fast
  !============================================================================
  subroutine clean_mod_trace_fast
    !--------------------------------------------------------------------------
    if(allocated(Trace_DINB))    deallocate(Trace_DINB)
    if(allocated(IjkTrace_DINB)) deallocate(IjkTrace_DINB)
    if(allocated(WeightTrace_DINB))    deallocate(WeightTrace_DINB)
    if(allocated(b_DNB))         deallocate(b_DNB)
    if(allocated(b_DGB))         deallocate(b_DGB)
    if(allocated(ray))           deallocate(ray)

  end subroutine clean_mod_trace_fast
  !============================================================================
  subroutine trace_field_grid

    ! This parallel ray tracing algorithm was developed at the U.of M.
    ! by G. Toth and D. De Zeeuw. An overview of the scheme can be found in
    !
    ! D. L. De Zeeuw, S. Sazykin, R. A. Wolf, T. I. Gombosi,
    ! A. J. Ridley, G. T\'oth, 2004,
    ! Journal of Geophysical Research, 109, 12219,
    !
    ! Details of the algorithm are to be published later

    use ModMain,     ONLY: n_step, iNewGrid, iNewDecomposition, time_simulation
    use ModPhysics,  ONLY: set_dipole
    use CON_axes,    ONLY: transform_matrix
    use ModUpdateStateFast, ONLY: sync_cpu_gpu

    ! remember last call and the last grid number
    integer :: nStepLast=-1, iLastGrid=-1, iLastDecomposition=-1

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'trace_field_grid'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    call sync_cpu_gpu('update State_VGB, B0_DGB on CPU', NameSub)

    call init_mod_trace_fast

    if(DoTest)then
       write(*,*)'GM ray_trace: nStepLast,n_step         =',nStepLast,n_step
       write(*,*)'GM ray_trace: iLastGrid,iNewGrid    =',iLastGrid,iNewGrid
       write(*,*)'GM ray_trace: iLastDecomp,iNewDecomp=',&
            iLastDecomposition,iNewDecomposition
    end if

    if(  nStepLast + DnRaytrace > n_step   .and. &
         iLastGrid          == iNewGrid .and. &
         iLastDecomposition == iNewDecomposition) RETURN

    ! Remember this call
    nStepLast = n_step
    iLastGrid = iNewGrid
    iLastDecomposition = iNewDecomposition

    call timing_start(NameSub)

    call init_mod_field_trace

    call set_dipole

    ! Transformation matrix between the SM(G) and GM coordinates
    if(UseSmg) &
         GmSm_DD = transform_matrix(time_simulation,'SMG',TypeCoordSystem)
    !$acc update device(GmSm_DD)

    if(UseAccurateTrace .or. .not.IsCartesianGrid)then
       call trace_grid_accurate
    else
       call trace_grid_fast
    end if

    call timing_stop(NameSub)

    call test_stop(NameSub, DoTest)

  end subroutine trace_field_grid
  !============================================================================
  subroutine trace_grid_fast

    use ModMain
    use ModAdvance,  ONLY: Bx_, Bz_, State_VGB, iTypeUpdate, UpdateSlow_
    use ModB0,       ONLY: get_b0
    use ModUpdateStateFast, ONLY: get_b0_dipole_fast
    use ModParallel, ONLY: NOBLK, neiLEV
    use ModGeometry, ONLY: R_BLK, Rmin_BLK, true_cell
    use BATL_lib, ONLY: Xyz_DGB, CellSize_DB, &
         message_pass_cell, message_pass_node
    use ModMpi

    ! Iteration parameters
    integer, parameter :: MaxIterTrace = 150
    integer :: nIterTrace
    real    :: dTraceMin, dTraceMax, NegTraceMax ! check for changes and loops
    !$acc declare create(nIterTrace)

    real :: TraceTmp_D(3)

    ! Minimum value of B for which integration of field lines makes any sense
    real, parameter :: SmallB = 1e-8

    ! True if Rmin_BLK < rTrace
    logical :: DoCheckInside

    ! Face index for the final point of the ray
    integer :: iFace

    ! Current position of ray in normalized and physical coordinates
    real :: Gen_D(3)

    ! Cell indices corresponding to current or final Gen_D position
    integer :: i1,j1,k1,i2,j2,k2

    ! Distance between Gen_D and i1,j1,k1, and i2,j2,k2
    real :: Dx1, Dy1, Dz1, Dx2, Dy2, Dz2

    ! Weights for surface interpolation
    real :: Weight_I(4)

    ! Cell indices
    integer :: i, j, k

    ! Indices corresponding to the starting point of the ray
    integer :: iX, iY, iZ

    ! Current block and direction indices
    integer :: iBlock, iRay, iDim

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

    write(*,*) NameSub,' starting'

    if(DoTime)call timing_reset('ray_pass',2)

    IsBVectorField = NameVectorField == 'B'
    !$acc update device(IsBVectorField)

    DoTestRay = .false.

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

!!! should we have i,j,k loops here???
       if(Unused_B(iBlock))then
          ! Trace_DINB in unused blocks is assigned to NORAY-1.
          Trace_DINB(:,:,:,:,:,iBlock) = NORAY-1.
          CYCLE
       else
          ! Initial values !!! Maybe LOOPRAY would be better??
          Trace_DINB(:,:,:,:,:,iBlock) = NORAY
          ray(:,:,:,:,:,iBlock) = NORAY
       endif
       ! Inner points of Trace_DINB should never be used, assign them to OPEN
       ! so that checking for blocks with fully open rays becomes easy
       Trace_DINB(:,:,2:nI,2:nJ,2:nK,iBlock)=OPENRAY

       ! Set Trace_DINB=OPENRAY at outer boundaries
       if(neiLEV(1,iBlock)==NOBLK) Trace_DINB(:,:,   1,:,:,iBlock) = OPENRAY
       if(neiLEV(2,iBlock)==NOBLK) Trace_DINB(:,:,nI+1,:,:,iBlock) = OPENRAY
       if(neiLEV(3,iBlock)==NOBLK) Trace_DINB(:,:,:,   1,:,iBlock) = OPENRAY
       if(neiLEV(4,iBlock)==NOBLK) Trace_DINB(:,:,:,nJ+1,:,iBlock) = OPENRAY
       if(neiLEV(5,iBlock)==NOBLK) Trace_DINB(:,:,:,:,   1,iBlock) = OPENRAY
       if(neiLEV(6,iBlock)==NOBLK) Trace_DINB(:,:,:,:,nK+1,iBlock) = OPENRAY

    end do

#ifndef OPENACC
    if(DoTest)write(*,*)NameSub,' initialized ray and Trace_DINB arrays'
#endif

    ! Interpolate the B1 field to the nodes
    !$acc parallel loop gang present(b_DNB)
    do iBlock=1, nBlock
       if(Unused_B(iBlock))CYCLE

       !$acc loop vector collapse(3)
       do k=1,nK+1; do j=1,nJ+1; do i=1,nI+1
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

#ifndef OPENACC
    if(DoTest)write(*,*)'Trace_DINB normalized B'
    if(DoTime.and.iProc==0)then
       write(*,'(a)',ADVANCE='NO') 'setup and normalization:'
       call timing_show('ray_trace',1)
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
          do iZ=1,nK+1; do iY=1,nJ+1; do iX=1,nI+1
             ! Store Trace_DINB into ray so we can see if there is any change
             ray(:,:,iX,iY,iZ,iBlock) = Trace_DINB(:,:,iX,iY,iZ,iBlock)
          end do; end do; end do

          if(Unused_B(iBlock)) CYCLE

          ! Flag cells inside the ionosphere if necessary
          DoCheckInside=Rmin_BLK(iBlock)<rTrace

          !$acc loop vector collapse(3) independent &
          !$acc private(Gen_D, GenIni_D,Weight_I, TraceTmp_D)
          do iZ=1,nK+1; do iY=1,nJ+1; do iX=1,nI+1
             ! Exclude inside points
             if(iX>1 .and. iX<=nI .and. iY>1 .and. iY<=nJ &
                  .and. iZ>1 .and. iZ<=nK ) CYCLE

             ! Exclude outer boundaries
             if(neiLEV(1,iBlock)==NOBLK .and. iX==   1) CYCLE
             if(neiLEV(2,iBlock)==NOBLK .and. iX==nI+1) CYCLE
             if(neiLEV(5,iBlock)==NOBLK .and. iZ==   1) CYCLE
             if(neiLEV(6,iBlock)==NOBLK .and. iZ==nK+1) CYCLE
             if(neiLEV(3,iBlock)==NOBLK .and. iY==   1) CYCLE
             if(neiLEV(4,iBlock)==NOBLK .and. iY==nJ+1) CYCLE

#ifndef OPENACC
             if(DoTestRay)write(*,*)'TESTING RAY: me,iBlock,iX,iY,iZ,Xyz_D',&
                  iProc,iBlock,iX,iY,iZ,&
                  Xyz_DGB(:,iX,iY,iZ,iBlock)-0.5*CellSize_DB(:,iBlock)
#endif

             if(nIterTrace==0)then
                do iRay = 1, 2
                   ! Follow ray in direction iRay
                   GenIni_D = [iX-0.5, iY-0.5, iZ-0.5]
                   iFace = follow_fast(.true., Gen_D, GenIni_D, DoCheckInside,&
                        iRay, iBlock)

                   ! Assign value to Trace_DINB
                   call assign_ray(iFace, iRay, iBlock, iX, iY, iZ, &
                        i1, j1, k1, i2, j2, k2, &
                        .true.,Trace_DINB(:,iRay,iX,iY,iZ,iBlock), Gen_D,&
                        Weight_I, .true.)

                   ! Memorize ray integration results
                   IjkTrace_DINB(1,iRay,iX,iY,iZ,iBlock) = iFace
                   if(iFace>0)then
                      select case(iFace)
                      case(1,2)
                         IjkTrace_DINB(2:3,iRay,iX,iY,iZ,iBlock) = [j1,k1]
                      case(3,4)
                         IjkTrace_DINB(2:3,iRay,iX,iY,iZ,iBlock) = [i1,k1]
                      case(6,5)
                         IjkTrace_DINB(2:3,iRay,iX,iY,iZ,iBlock) = [i1,j1]
                      end select
                      WeightTrace_DINB(:,iRay,iX,iY,iZ,iBlock) = Weight_I
                   end if
                end do
             else
                do iRay = 1, 2
                   ! Use stored values
                   iFace=IjkTrace_DINB(1,iRay,iX,iY,iZ,iBlock)
                   if(iFace>0)then
                      select case(iFace)
                      case(1)
                         i1=1; i2=1
                         j1=IjkTrace_DINB(2,iRay,iX,iY,iZ,iBlock); j2=j1+1
                         k1=IjkTrace_DINB(3,iRay,iX,iY,iZ,iBlock); k2=k1+1
                      case(2)
                         i1=nI+1; i2=i1
                         j1=IjkTrace_DINB(2,iRay,iX,iY,iZ,iBlock); j2=j1+1
                         k1=IjkTrace_DINB(3,iRay,iX,iY,iZ,iBlock); k2=k1+1
                      case(3)
                         j1=1; j2=1
                         i1=IjkTrace_DINB(2,iRay,iX,iY,iZ,iBlock); i2=i1+1
                         k1=IjkTrace_DINB(3,iRay,iX,iY,iZ,iBlock); k2=k1+1
                      case(4)
                         j1=nJ+1; j2=nJ+1
                         i1=IjkTrace_DINB(2,iRay,iX,iY,iZ,iBlock); i2=i1+1
                         k1=IjkTrace_DINB(3,iRay,iX,iY,iZ,iBlock); k2=k1+1
                      case(5)
                         k1=1; k2=1
                         i1=IjkTrace_DINB(2,iRay,iX,iY,iZ,iBlock); i2=i1+1
                         j1=IjkTrace_DINB(3,iRay,iX,iY,iZ,iBlock); j2=j1+1
                      case(6)
                         k1=nK+1; k2=k1
                         i1=IjkTrace_DINB(2,iRay,iX,iY,iZ,iBlock); i2=i1+1
                         j1=IjkTrace_DINB(3,iRay,iX,iY,iZ,iBlock); j2=j1+1
                      end select

!!! We should not pass an array segment
                      ! ray instead of Trace_DINB is used for the first argument
                      ! to avoid GPU race condition.
                      call rayface_interpolate(&
                           ray(:,iRay,i1:i2,j1:j2,k1:k2,iBlock),&
                           WeightTrace_DINB(:,iRay,iX,iY,iZ,iBlock),4,&
                           TraceTmp_D)

                      Trace_DINB(:,iRay,iX,iY,iZ,iBlock) = TraceTmp_D

                   end if
                end do
             end if ! nIterTrace==0
          end do; end do; end do ! iZ, iY, iX
       end do ! iBlock
       call timing_stop("trace_iter1")

       ! Exchange Trace_DINB information

       call timing_start('trace_pass')
       call ray_pass
       call timing_stop('trace_pass')

       nIterTrace = nIterTrace + 1
       !$acc update device(nIterTrace)

       if(DoTime .and. iProc == 0 .and. nIterTrace == 1)then
          write(*,'(a)',ADVANCE='NO') 'first iteration:'
          call timing_show('ray_trace',1)
       end if

       ! Check for significant changes and loop rays in Trace_DINB
       call timing_start('trace_check')
       dTraceMax = 0.0; NegTraceMax = 0.0  ! note that LOOPRAY is negative
       !$acc parallel loop gang reduction(max:dTraceMax,NegTracemax)
       do iBlock = 1, nBlock
          if(Unused_B(iBlock))CYCLE
          !$acc loop vector collapse(3) reduction(max:dTraceMax,NegTraceMax)
          do k = 1,nK+1; do j = 1,nJ+1; do i = 1,nI+1
             dTraceMax = max(dTraceMax, &
                  maxval(abs(ray(:,:,i,j,k,iBlock) &
                  -          Trace_DINB(:,:,i,j,k,iBlock))))
             NegTraceMax = max(NegTraceMax, &
                  -Trace_DINB(1,1,i,j,k,iBlock), -Trace_DINB(1,2,i,j,k,iBlock))
          end do; end do; end do
       end do

       if(nProc > 1)then
          call MPI_allreduce(MPI_IN_PLACE, dTraceMax, 1, MPI_REAL,&
               MPI_MAX, iComm, iError)
          call MPI_allreduce(MPI_IN_PLACE, NegTraceMax, 1, MPI_REAL,&
               MPI_MAX, iComm, iError)
       end if

       call timing_stop('trace_check')

       ! Check for change
       if(dTraceMax < dTraceMin) then
          ! Check for loops
          if(NegTraceMax < -LOOPRAY) EXIT
          if(UsePreferredInterpolation)then
             if(iProc==0)call error_report('field tracing, nIterTrace=',&
                  nIterTrace+0.0, iError1, .true.)
             EXIT
          endif
          if(DoTest)write(*,*)'Switching to UsePreferredInterpolation=.true.'
          UsePreferredInterpolation = .true.
          !$acc update device(UsePreferredInterpolation)
       end if

    end do ! ray iteration

    call timing_stop("trace_iter")

#ifndef OPENACC
    ! Check for unassigned Trace_DINB in every used block
    if(DoTest)then
       write(*,*)NameSub,' finished after ',nIterTrace,' iterations'
       do iBlock = 1, nBlock
          if(Unused_B(iBlock))CYCLE
          do iRay=1,2
             if(any(Trace_DINB(1,iRay,1:nI,1:nJ,1:nK,iBlock)<BODYRAY))then
                Ijk_D=minloc(Trace_DINB(1,iRay,1:nI,1:nJ,1:nK,iBlock))
                write(*,*)'LOOPRAYFACE: iRay,me,Ijk_D,value,Xyz_D=',&
                     iRay,iProc,Ijk_D,iBlock,&
                     minval(Trace_DINB(1,iRay,1:nI,1:nJ,1:nK,iBlock)),&
                     Xyz_DGB(:,Ijk_D(1),Ijk_D(2),Ijk_D(3),iBlock) &
                     -0.5*CellSize_DB(:,iBlock)
             end if
          end do
       end do
       if(index(StringTest,'ray_debugger') > 0)call ray_debugger
    end if

    if(DoTime.and.iProc==0)then
       write(*,'(i5,a)') nIterTrace,' iterations:'
       call timing_show('ray_trace',1)
       call timing_show('ray_pass',2)
    end if

    if(DoTest)write(*,*)NameSub,' starting cell center assignments'
#endif

    call timing_start('trace_grid_fast2')

    ! Assign face ray values to cell centers
    !$acc parallel loop gang
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE

       ! Set flag if checking on the ionosphere is necessary
       DoCheckInside = Rmin_BLK(iBlock) < rTrace

       do iRay=1,2
#ifndef OPENACC
          ! Some optimization for fully open blocks
          if(.not.DoCheckInside)then
             if(all(Trace_DINB(1,iRay,:,:,:,iBlock)==OPENRAY))then
                ray(:,iRay,:,:,:,iBlock)=OPENRAY
                CYCLE
             end if
          end if
#endif
          !$acc loop vector collapse(3) private(GenIni_D, Gen_D, Weight_I)
          do iZ=1,nK; do iY=1,nJ; do iX=1,nI

             ! Shortcuts for inner and false cells
             if(R_BLK(iX,iY,iZ,iBlock) < rInner .or. &
                  .not.true_cell(iX,iY,iZ,iBlock))then
                ray(:,iRay,iX,iY,iZ,iBlock)=BODYRAY
#ifndef OPENACC
                if(DoTestRay)write(*,*)'BODYRAY'
#endif
                CYCLE
             end if

#ifndef OPENACC
             if(DoTestRay)write(*,*)'calling follow_fast'
#endif

             ! Follow ray in direction iRay
             GenIni_D = [real(iX), real(iY), real(iZ)]
             iFace = follow_fast(.false., Gen_D, GenIni_D, DoCheckInside, iRay, iBlock)

#ifndef OPENACC
             if(DoTestRay)write(*,*)'calling assign_ray'
#endif

             ! Assign value to ray
             call assign_ray(iFace, iRay, iBlock, iX, iY, iZ, &
                  i1, j1, k1, i2, j2, k2, &
                  .false., ray(:,iRay,iX,iY,iZ,iBlock), Gen_D, Weight_I)

          end do; end do; end do ! iX, iY, iZ
       end do ! iRay
    end do ! iBlock

    call timing_stop('trace_grid_fast2')

    if(DoTest)write(*,*)NameSub,' finished with ray=',&
         ray(:,:,iTest,jTest,kTest,iBlockTest)

    if(DoTest)then
       ! Check for unassigned cell centers
       do iBlock = 1, nBlock
          if(Unused_B(iBlock))CYCLE
          do iRay=1,2
             if(any(ray(1,iRay,1:nI,1:nJ,1:nK,iBlock)<BODYRAY))then
                Ijk_D=minloc(ray(1,iRay,1:nI,1:nJ,1:nK,iBlock))
                write(*,*)'LOOPRAY: iRay,me,Ijk_D,value,Xyz_D=',&
                     iRay,iProc,Ijk_D,iBlock,&
                     minval(ray(1,iRay,1:nI,1:nJ,1:nK,iBlock)),&
                     Xyz_DGB(:,Ijk_D(1),Ijk_D(2),Ijk_D(3),iBlock)
             end if
          end do
       end do
    end if

    if(DoTest)write(*,*)NameSub,' starting conversion to lat/lon'

    call timing_start('trace_grid_fast3')

    ! Convert end position to co-latitude, longitude, and status
    !$acc parallel loop gang
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       !$acc loop vector collapse(3)
       do k=1,nK; do j=1,nJ; do i=1,nI
          call xyz_to_latlonstatus(ray(:,:,i,j,k,iBlock))
       end do; end do; end do
    end do

!!! If ray tracing is done more often than plotting, this is not optimal.
    !$acc update host(ray)

    call timing_stop('trace_grid_fast3')

    if(DoTime.and.iProc==0)then
       write(*,'(a)',ADVANCE='NO') 'Total ray tracing time:'
       call timing_show('ray_trace',1)
    end if
    call barrier_mpi
    call test_stop(NameSub, DoTest)

    call timing_stop(NameSub)
  contains
    !==========================================================================
#ifndef OPENACC
    subroutine ray_debugger

      ! Debug Trace_DINB values

      integer :: iPos_D(3), jX, kX, jY, kY, jZ, kZ
      !------------------------------------------------------------------------
      do
         ! Read position
         write(*,'(a)',ADVANCE='NO')'Trace_DINB x,y,z,iRay:'
         read(*,*) xTest,yTest,zTest,iRay
         if(xTest==0.0 .and. yTest==0.0 .and. zTest==0.0) EXIT

         ! Find position
         do iBlock = 1, nBlock
            if(Unused_B(iBlock))CYCLE
            do iX=1,nI+1
               if(abs(Xyz_DGB(x_,iX,1,1,iBlock) &
                    -0.5*CellSize_DB(x_,iBlock)-xTest) > 0.01) CYCLE
               do iY=1,nJ+1
                  if(abs(Xyz_DGB(y_,1,iY,1,iBlock) &
                       -0.5*CellSize_DB(y_,iBlock)-yTest) > 0.01) CYCLE
                  do iZ=1,nK+1
                     if(abs(Xyz_DGB(z_,1,1,iZ,iBlock) &
                          -0.5*CellSize_DB(z_,iBlock)-zTest) > 0.01) CYCLE

                     ! Print information

                     write(*,*)'iProc,iBlock,iX,iY,iZ=',iProc,iBlock,iX,iY,iZ
                     write(*,*)'Xyz_DGB(z_,1,1,1),dx=',&
                          Xyz_DGB(:,1,1,1,iBlock), CellSize_DB(x_,iBlock)

                     iPos_D=IjkTrace_DINB(:,iRay,iX,iY,iZ,iBlock)
                     if(iPos_D(1)>0)then
                        write(*,*)' Trace_DINB   =', &
                             Trace_DINB(:,iRay,iX,iY,iZ,iBlock)
                        write(*,*)' IjkTrace_DINB=', &
                             IjkTrace_DINB(:,iRay,iX,iY,iZ,iBlock)
                        write(*,*)' WeightTrace_DINB=', &
                             WeightTrace_DINB(:,iRay,iX,iY,iZ,iBlock)
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
                             Trace_DINB(1,iRay,jX:kX,jY:kY,jZ:kZ,iBlock)
                        write(*,*)' jX,kX,jY,kY,jZ,kZ=',jX,kX,jY,kY,jZ,kZ
                        write(*,*)' Xyz(End)=',&
                             Xyz_DGB(:,jx,jy,jz,iBlock) &
                             -0.5*CellSize_DB(:,iBlock)
                     else
                        write(*,*)' IjkTrace_DINB=',iPos_D
                     end if
                  end do
               end do
            end do
         end do
      end do

    end subroutine ray_debugger
    !==========================================================================
#endif
    subroutine interpolate_bb_node(Gen_D, b_D, iBlock)
      !$acc routine seq

      ! Interpolate normalized field b_D at normalized location Gen_D
      ! Interpolate B1 from nodes, take B0 from analytic expression

      real, intent(in) :: Gen_D(3)
      real, intent(out):: b_D(3)
      integer, intent(in):: iBlock
      real :: b

      integer :: i1,j1,k1,i2,j2,k2

      ! Distance between Gen_D and i1,j1,k1, and i2,j2,k2
      real :: Dx1, Dy1, Dz1, Dx2, Dy2, Dz2

      real :: Xyz_D(3)
      !------------------------------------------------------------------------

      ! Determine cell indices corresponding to location Gen_D
      i1 = floor(Gen_D(1)+0.5); i2 = i1 + 1
      j1 = floor(Gen_D(2)+0.5); j2 = j1 + 1
      k1 = floor(Gen_D(3)+0.5); k2 = k1 + 1

#ifndef OPENACC
      if(i1<0 .or. i2>nI+2 .or. j1<0 .or. j2>nJ+2 .or. k1<0 .or. k2>nK+2)then
         write(*,*)'interpolate_bb_node: iProc, iBlock, Gen_D=', &
              iProc, iBlock, Gen_D
         call stop_mpi('ERROR in interpolate_bb_node: location out of bounds')
      endif
#endif

      ! Get B0 values for location
      Xyz_D = Xyz_DGB(:,1,1,1,iBlock) + CellSize_DB(:,iBlock)*(Gen_D - 1)

      if(UseB0)then
#ifdef OPENACC
         call get_b0_dipole_fast(Xyz_D, b_D)
#else
         call get_b0(Xyz_D, b_D)
#endif
      else
         b_D = 0.00
      end if

      ! Make sure that the interpolation uses inside indexes only
      i1 = max(1,i1)   ; j1 = max(1,j1);    k1 = max(1,k1)
      i2 = min(nI+1,i2); j2 = min(nJ+1,j2); k2 = min(nK+1,k2)

      ! Distances relative to the nodes
      Dx1 = Gen_D(1)+0.5-i1; Dx2 = 1.-Dx1
      Dy1 = Gen_D(2)+0.5-j1; Dy2 = 1.-Dy1
      Dz1 = Gen_D(3)+0.5-k1; Dz2 = 1.-Dz1

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
    logical function do_follow_iono(Xyz_D, iRay)
      !$acc routine seq

      ! Follow ray inside ionosphere starting from Xyz_D which is given in
      ! real coordinates and use analytic mapping.
      ! On return Xyz_D contains the final coordinates.
      ! Return true if it was successfully integrated down to rIonosphere,
      ! return false if the ray exited rTrace or too many integration
      ! steps were Done

      use ModPhysics,  ONLY: DipoleStrengthSi ! only the sign is needed
      use ModUpdateStateFast, ONLY: map_planet_field_fast
      use CON_planet_field, ONLY: map_planet_field

      real, intent(inout):: Xyz_D(3)
      integer, intent(in):: iRay

      integer :: iHemisphere
      real    :: x_D(3)
      !------------------------------------------------------------------------
#ifndef OPENACC
      if(iTypeUpdate <= UpdateSlow_)then
         call map_planet_field(Time_Simulation, Xyz_D, &
              TypeCoordSystem//' NORM', rIonosphere, x_D, iHemisphere)
      else
#endif
         call map_planet_field_fast(Xyz_D, rIonosphere, x_D, iHemisphere, &
              UseGsm=.true.)
#ifndef OPENACC
      end if

      if(iHemisphere==0)then
         write(*,*)'iHemisphere==0 for Xyz_D=',Xyz_D
         write(*,*)'iBlock, iRay=',iBlock,iRay
         call stop_mpi('ERROR in do_follow_iono')
      end if
#endif

      if(iHemisphere*DipoleStrengthSi*sign(1.0,1.5-iRay) < 0.0)then
         Xyz_D = x_D
         do_follow_iono = .true.
      else
         do_follow_iono = .false.
      end if

    end function do_follow_iono
    !==========================================================================

    function follow_fast(IsSurfacePoint, Gen_D, GenIn_D, DoCheckInside, &
         iRay, iBlock) result(iFaceOut)
      !$acc routine seq

      ! Follow ray starting at initial position GenIn_D in direction iRay
      ! until we hit the wall of the control volume or the ionosphere.
      ! Return 1,2,3,4,5,6 if the ray hit the block faces
      ! Return ray_iono_   if the ray hit the ionosphere
      ! Return ray_loop_   if the ray did not hit anything
      ! Return ray_out_    if the ray goes out of the box immediately
      ! Return ray_body_   if the ray goes into or is inside a body

      ! Arguments

      logical, intent(in):: IsSurfacePoint
      real, intent(inout):: Gen_D(3)
      real, intent(in)   :: GenIn_D(3)
      logical, intent(in):: DoCheckInside
      integer, intent(in):: iRay, iBlock

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

      ! counter for ray integration
      integer :: nSegment
      integer, parameter:: MaxSegment = 10*(nI+nJ+nK)

      integer:: iOuter, iInner

      ! Counter for entering do_follow_iono
      integer :: nIono
#ifndef OPENACC
      !------------------------------------------------------------------------
      if(DoTestRay)&
           write(*,*)'follow_fast: me,iBlock,IsSurfacePoint,GenIn_D,iRay=',&
           iProc, iBlock, IsSurfacePoint, GenIn_D, iRay
#endif

      ! Step size limits
      DsMax=1.0
      DsMin=0.05
      DsTiny=1.e-6

      ! Initial value
      DsNext=sign(DsMax,1.5-iRay)

      ! Accuracy in terms of Gen_D in normalized coordinates
      DxOpt=0.01

      ! Length and maximum length of ray within control volume
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

#ifndef OPENACC
               if(DoTestRay)write(*,*)&
                    'Inside rTrace at me,iBlock,nSegment,Gen_D,Xyz_D=',&
                    iProc,iBlock,nSegment,Gen_D,Xyz_D
#endif

               if((.not. IsBVectorField) .or. r2 <= rInner2)then

                  if(nSegment==0)then
                     iFaceOut = ray_body_
#ifndef OPENACC
                     if(DoTestRay)write(*,*)&
                          'Initial point inside rInner at me,iBlock,Xyz_D=',&
                          iProc,iBlock,Xyz_D
#endif
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
                     iFaceOut = ray_iono_
                  end if
                  EXIT
               end if

               ! Try mapping down to rIonosphere if we haven't tried yet
               if(nIono<1)then
                  if(do_follow_iono(Xyz_D, iRay))then
                     Gen_D=Xyz_D
                     iFaceOut=ray_iono_
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

         ! Check if the ray is pointing outwards
         if(nSegment==0.and.IsSurfacePoint)then
#ifndef OPENACC
            if(DoTestRay)write(*,*)'me,iBlock,GenIni_D,bIni_D=', &
                 iProc, iBlock, GenIni_D, bIni_D
#endif

            if(any(GenMid_D < GenMin_D) .or. any(GenMid_D > GenMax_D))then
               iFaceOut = ray_out_
#ifndef OPENACC
               if(DoTestRay)then
                  write(*,*)'me,iBlock,GenMid_D=', iProc, iBlock, GenMid_D
                  write(*,*)'ray points outwards: me,iBlock,Ds,Xyz_D=', &
                       iProc, iBlock, Ds,&
                       Xyz_DGB(:,1,1,1,iBlock) &
                       + CellSize_DB(:,iBlock)*(GenMid_D - 1)
               end if
#endif
               RETURN
            end if
         end if

         do iInner = 1, 10
            ! Full step
            call interpolate_bb_node(GenMid_D, bMid_D, iBlock)

            ! Calculate the difference between 1st and 2nd order integration
            ! and take ratio relative to DxOpt
            DxRel = abs(Ds)*maxval(abs(bMid_D - bIni_D))/DxOpt

#ifndef OPENACC
            if(DoTestRay.and.okdebug)&
                 write(*,*)'me,iBlock,GenMid_D,bMid_D,DxRel=', &
                 iProc, iBlock, GenMid_D, bMid_D, DxRel
#endif

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
#ifndef OPENACC
               if(DoTestRay.and.okdebug)&
                    write(*,*)'new decreased Ds: me,iBlock,Ds=', &
                    iProc,iBlock,Ds
#endif
            else
               ! Too accurate, increase Ds if possible
               if(abs(Ds) < DsMax - DsTiny)then

                  DsNext = sign(min(DsMax, abs(Ds)/sqrt(DxRel)), Ds)

#ifndef OPENACC
                  if(DoTestRay.and.okdebug)&
                       write(*,*)'new increased DsNext: me,iBlock,DsNext=', &
                       iProc, iBlock, DsNext
#endif

               end if

               EXIT
            end if
         end do

         Gen_D = GenIni_D + bMid_D*Ds

         nSegment = nSegment+1
         s = s + abs(Ds)

#ifndef OPENACC
         if(DoTestRay.and.okdebug)&
              write(*,*)'me,iBlock,nSegment,s,Gen_D=', &
              iProc, iBlock, nSegment, s, Gen_D
#endif

         ! Check if the ray hit the wall of the control volume
         if(any(Gen_D < GenMin_D) .or. any(Gen_D > GenMax_D))then

            ! Hit the wall, backup so that Gen_D is almost exactly on the wall
            ! just a little bit outside. Only if nSegment is more than 1!
            if(nSegment > 1)then
               DsBack = Ds*maxval(max(GenMin_D-Gen_D,Gen_D-GenMax_D) &
                    /(abs(Gen_D - GenIni_D) + DsTiny))
               Gen_D = Gen_D - DsBack*bMid_D
            end if

            ! Find out which wall the ray hit
            if    (Gen_D(1)<=GenMin_D(1))then; iFaceOut=1
            elseif(Gen_D(2)<=GenMin_D(2))then; iFaceOut=3
            elseif(Gen_D(3)<=GenMin_D(3))then; iFaceOut=5
            elseif(Gen_D(1)>=GenMax_D(1))then; iFaceOut=2
            elseif(Gen_D(2)>=GenMax_D(2))then; iFaceOut=4
            elseif(Gen_D(3)>=GenMax_D(3))then; iFaceOut=6
            else
#ifndef OPENACC
               write(*,*)'Error in follow_fast for me,iBlock,iX,iY,iZ=',&
                    iProc,iBlock,iX,iY,iZ
               write(*,*)'nSegment,Gen_D,Ds,DsBack=', &
                    nSegment, Gen_D, Ds, DsBack
               call stop_mpi('GM_follow_fast: Hit wall but which one?')
#endif
            end if

            ! Make sure that Gen_D is not outside the control volume
            Gen_D=max(GenMin_D+DsTiny,Gen_D)
            Gen_D=min(GenMax_D-DsTiny,Gen_D)

            EXIT
         end if

         ! Check if we have integrated for too long
         if(s>sMax)then
            ! Seems to be a closed loop within a block
#ifndef OPENACC
            if(DoTestRay)then
               write(*,*)'CLOSED LOOP at me,iBlock,iX,iY,iZ,Gen_D,Xyz_D=',&
                    iProc,iBlock,iX,iY,iZ,Gen_D,&
                    Xyz_DGB(:,1,1,1,iBlock) + CellSize_DB(:,iBlock)*(Gen_D - 1)
            end if
#endif

            iFaceOut=ray_loop_
            EXIT
         end if

      end do

#ifndef OPENACC
      if(DoTestRay)write(*,*) &
           'Finished follow_fast at me,iBlock,nSegment,iFaceOut,Gen_D,Xyz_D=',&
           iProc,iBlock,nSegment,iFaceOut,Gen_D,&
           Xyz_DGB(:,1,1,1,iBlock) + CellSize_DB(:,iBlock)*(Gen_D - 1)
#endif

    end function follow_fast
    !==========================================================================
    subroutine assign_ray(iFace, iRay, iBlock, iX, iY, iZ, &
         i1, j1, k1, i2, j2, k2, IsSurfacePoint, Trace_D, Gen_D, Weight_I, UseRay)
      !$acc routine seq

      ! Assign value to Trace_D(3) based on ray intersection
      ! given by the global variables iFace and position Gen_D(3)
      !
      ! iRay is 1 if ray points in positive B direction and 2 otherwise
      !
      ! IsSurfacePoint is true if the ray was started from the block face
      ! and false if it was started from a cell center

      integer, intent(in)    :: iFace, iRay, iBlock, iX, iY, iZ
      integer, intent(inout) :: i1,j1,k1,i2,j2,k2
      logical, intent(in)    :: IsSurfacePoint
      ! Called with a segment of Trace_DINB array and it is used here
      ! to get Trace_D
      real, intent(inout)    :: Trace_D(3), Gen_D(3)
      real, intent(inout)    :: Weight_I(4)
      logical, optional, intent(in):: UseRay

      real :: TraceTmp_D(3)
      real :: Trace_DI(3,4)
      integer :: iCount, ii, jj, kk

      ! Distances between Gen_D and the 4 grid points used for interpolation
      real :: d1, e1, d2, e2

      character(len=*), parameter:: NameSub = 'assign_ray'
      !------------------------------------------------------------------------
#ifndef OPENACC
      if(DoTestRay)write(*,*)&
           NameSub,' starting with IsSurfacePoint, iRay, iFace=',&
           IsSurfacePoint,iRay,iFace
#endif

      select case(iFace)
      case(ray_out_)
         ! The ray points outward
         Trace_D = OUTRAY
#ifndef OPENACC
         if(DoTestRay)write(*,*)NameSub,' finished with Trace_D=OUTRAY'
#endif
         RETURN
      case(ray_loop_)
         ! The ray did not hit the wall of the block
         Trace_D = LOOPRAY
#ifndef OPENACC
         if(DoTestRay)write(*,*)NameSub,' finished with Trace_D=LOOPRAY'
#endif
         RETURN
      case(ray_body_)
         ! The ray hit a body
         Trace_D = BODYRAY
#ifndef OPENACC
         if(DoTestRay)write(*,*)NameSub,' finished with Trace_D=BODYRAY'
#endif
         RETURN
      case(ray_iono_)
         ! The ray hit the ionosphere
         Trace_D = Gen_D
#ifndef OPENACC
         if(DoTestRay)write(*,*)&
              NameSub,' finished with Trace_D on ionosphere, Trace_D=',Trace_D
#endif
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
         ! The ray hit the bot or top wall
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

      case default
#ifndef OPENACC
         write(*,*)'Impossible value for iFace=',iFace,' at iX,iY,iZ,iBlock=',&
              iX,iY,iZ,iBlock
         call stop_mpi('assign_ray')
#endif
      end select

      ! Calculate bilinear interpolation weights
      d2 = 1 - d1; e2 = 1 - e1
      Weight_I(1) = d2*e2
      Weight_I(2) = d1*e2
      Weight_I(3) = d2*e1
      Weight_I(4) = d1*e1

#ifndef OPENACC
      if(DoTestRay)write(*,*)'Weight_I=',Weight_I
#endif

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
#ifndef OPENACC
            if(DoTestRay)write(*,*) &
                 'Excluded point: me,iBlock,iX,iY,iZ,Weight_I=',&
                 iProc, iBlock, iX, iY, iZ, Weight_I
#endif
         end if
      end if

#ifndef OPENACC
      if(DoTestRay) write(*,*)'i1,j1,k1,i2,j2,k2,d1,e1=', &
           i1, j1, k1, i2, j2, k2, d1, e1
#endif

      iCount = 1
      do kk = k1, k2; do jj = j1, j2; do ii = i1, i2
         if(present(UseRay)) then
            ! Use ray instead of Trace_DINB to avoid GPU race condition
            if(UseRay) &
                 Trace_DI(:,iCount) = ray(:,iRay,ii,jj,kk,iBlock)
         else
            Trace_DI(:,iCount) = Trace_DINB(:,iRay,ii,jj,kk,iBlock)
         endif
         iCount = iCount + 1
      end do; end do; end do

      call rayface_interpolate(Trace_DI, Weight_I, 4, TraceTmp_D)

      Trace_D = TraceTmp_D

#ifndef OPENACC
      if(DoTestRay)write(*,*)NameSub,' finished Trace_D=',Trace_D
#endif

    end subroutine assign_ray
    !==========================================================================
  end subroutine trace_grid_fast
  !============================================================================
  subroutine rayface_interpolate(Trace_DI, Weight_I, nValue, Trace_D)
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
    character(len=*), parameter:: NameSub = 'rayface_interpolate'
    !--------------------------------------------------------------------------
    ! call test_start(NameSub, DoTest)

    if(.not.UsePreferredInterpolation .and. &
         maxval(Weight_I)-minval(Weight_I) > 0.0001)then
       WeightTmp_I(1:nValue)=Weight_I
    else
       ValueMax = maxval(Trace_DI(1,:), MASK = Weight_I > 0.0) ! 0.01)

       if(ValueMax < CLOSEDRAY)then
          !     if(ValueMax < OPENRAY-0.01)then
          Trace_D = ValueMax
          RETURN
       end if

       where(Trace_DI(1,:)>=OPENRAY-0.01)
          WeightTmp_I(1:nValue)=Weight_I
       elsewhere
          WeightTmp_I(1:nValue)=0.
       endwhere
    end if

#ifndef OPENACC
    if(DoTestRay)then
       write(*,*) NameSub
       write(*,*)'Trace_DI(1,:)=',Trace_DI(1,:)
       write(*,*)'Trace_DI(2,:)=',Trace_DI(2,:)
       write(*,*)'Trace_DI(3,:)=',Trace_DI(3,:)
       write(*,*)'Weight_I       =',Weight_I
       write(*,*)'WeightTmp_I      =',WeightTmp_I
    end if
#endif

    ! Short cuts
    if(all(Trace_DI(1,:)==OPENRAY))then
       ! all surrounding rays are open
       Trace_D=OPENRAY
#ifndef OPENACC
       if(DoTestRay)write(*,*) NameSub,' finished with fully OPENRAY'
#endif
       RETURN
    end if

    if(all(Trace_DI(1,:)==NORAY))then
       ! all surrounding rays are unknown
       Trace_D=NORAY
#ifndef OPENACC
       if(DoTestRay)write(*,*) NameSub,' finished with fully NORAY'
#endif
       RETURN
    end if

    dTraceMax=0.2*rIonosphere
    n=0
    do j=1,nValue
       i=1
       do iCount = 1, 9999999
          if(i>n)then
             ! New type of ray
             n=i
             TraceFirst_DI(:,i)=Trace_DI(:,j)
             WeightSum_I(i) =WeightTmp_I(j)
             if(TraceFirst_DI(1,i)>CLOSEDRAY)&
                  TraceSum_DI(:,i)=WeightTmp_I(j)*Trace_DI(:,j)
             EXIT
          end if

          ! Calculate difference between Trace_DI(:,j) and TraceFirst_DI(:,i)
          dTrace=sum(abs(Trace_DI(:,j)-TraceFirst_DI(:,i)))

          if(dTrace<dTraceMax)then
             ! Same type of ray, cummulate it

             WeightSum_I(i)=WeightSum_I(i)+WeightTmp_I(j)
             if(TraceFirst_DI(1,i)>CLOSEDRAY)&
                  TraceSum_DI(:,i) = TraceSum_DI(:,i) &
                  + WeightTmp_I(j)*Trace_DI(:,j)
             EXIT
          end if
          ! Try next type
          i = i + 1

#ifndef OPENACC
          if(i>nValue)call stop_mpi(NameSub//': Impossible value for i')
#endif
       end do ! i
    end do ! j

    if(n == 1)then
       ! Only one type of ray is interpolated
       if(TraceFirst_DI(1,1)>CLOSEDRAY)then
          ! get result (WeightSum_I can be less than 1! )
          Trace_D=TraceSum_DI(:,1)/WeightSum_I(1)
       else
          ! identical Trace_DINB values, no need to average
          Trace_D=TraceFirst_DI(:,1)
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

#ifndef OPENACC
    if(DoTestRay)then
       write(*,*) NameSub,': WeightSum_I=',WeightSum_I(1:n)
       write(*,*) NameSub,' finished with Trace_D=',Trace_D
    end if
#endif

    ! call test_stop(NameSub, DoTest)
  end subroutine rayface_interpolate
  !============================================================================
  subroutine ray_pass

    !  call ray_pass_new
    !--------------------------------------------------------------------------
    call ray_pass_old

  end subroutine ray_pass
  !============================================================================
  subroutine ray_pass_new

    use ModParallel, ONLY : neiLEV
    use BATL_lib, ONLY: message_pass_node

    integer :: iBlock, iFace

    !  do i=1,3; do j=1,2

    ! !     call pass_and_max_nodes(.false.,Trace_DINB(i,j,:,:,:,:))
    ! !     call pass_and_max_nodes(.true.,Trace_DINB(i,j,:,:,:,:))
    !  end do; end do

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'ray_pass_new'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    call message_pass_node(6, Trace_DINB, 'max')

    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE
       do iFace = 1, 6
          if(neiLEV(iFace,iBlock)==1)call prolong_ray_after_pass(iFace,iBlock)
       end do
    end do

    call test_stop(NameSub, DoTest)
  end subroutine ray_pass_new
  !============================================================================
  subroutine prolong_ray_after_pass(iFace,iBlock)
    !! acc routine vector

    ! For faces that are shared with a coarser neighbor, interpolate
    ! for all points which are not coinciding and where the ray is going out.
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

    integer, intent(in) :: iFace,iBlock
    integer :: iRay
    integer :: j, k, nFaceJ, nFaceK
    integer, parameter :: nFaceMax=max(nI+1,nJ+1,nK+1)
    real    :: Trace_DIII(3,2,nFaceMax,nFaceMax)
    integer :: IjkTrace_DII(2,nFaceMax,nFaceMax)

    ! Interpolation weights
    real, parameter:: Weight4_I(4)=0.25, Weight2_I(2)=0.5

    ! Extract Trace_DIII and IjkTrace_DII for the appropriate face
    ! NOTE: IjkTrace_DII assignment split to two lines to avoid
    !       reshaping compiler bug!
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'prolong_ray_after_pass'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    select case(iFace)
    case(1)
       nFaceJ=nJ+1; nFaceK=nK+1
       Trace_DIII( :,:,1:nFaceJ,1:nFaceK)=Trace_DINB(:,:,1,:,:,iBlock)
       IjkTrace_DII(1,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,1,1,:,:,iBlock)
       IjkTrace_DII(2,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,2,1,:,:,iBlock)
    case(2)
       nFaceJ=nJ+1; nFaceK=nK+1
       Trace_DIII( :,:,1:nFaceJ,1:nFaceK)=Trace_DINB(:,:,nI+1,:,:,iBlock)
       IjkTrace_DII(1,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,1,nI+1,:,:,iBlock)
       IjkTrace_DII(2,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,2,nI+1,:,:,iBlock)
    case(3)
       nFaceJ=nI+1; nFaceK=nK+1
       Trace_DIII( :,:,1:nFaceJ,1:nFaceK)=Trace_DINB(:,:,:,1,:,iBlock)
       IjkTrace_DII(1,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,1,:,1,:,iBlock)
       IjkTrace_DII(2,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,2,:,1,:,iBlock)
    case(4)
       nFaceJ=nI+1; nFaceK=nK+1
       Trace_DIII( :,:,1:nFaceJ,1:nFaceK)=Trace_DINB(:,:,:,nJ+1,:,iBlock)
       IjkTrace_DII(1,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,1,:,nJ+1,:,iBlock)
       IjkTrace_DII(2,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,2,:,nJ+1,:,iBlock)
    case(5)
       nFaceJ=nI+1; nFaceK=nJ+1
       Trace_DIII( :,:,1:nFaceJ,1:nFaceK)=Trace_DINB(:,:,:,:,1,iBlock)
       IjkTrace_DII(1,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,1,:,:,1,iBlock)
       IjkTrace_DII(2,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,2,:,:,1,iBlock)
    case(6)
       nFaceJ=nI+1; nFaceK=nJ+1
       Trace_DIII( :,:,1:nFaceJ,1:nFaceK)=Trace_DINB(:,:,:,:,nK+1,iBlock)
       IjkTrace_DII(1,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,1,:,:,nK+1,iBlock)
       IjkTrace_DII(2,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,2,:,:,nK+1,iBlock)
    case default
       call stop_mpi('Impossible value for iFace in prolong_ray')
    end select

    do iRay=1,2
       do k=1,nfaceK
          if(mod(k,2)==1)then
             do j=2,nfaceJ,2
                ! Case b: even j and odd k

                if(IjkTrace_DII(iRay,j,k) /= ray_out_) CYCLE

                call rayface_interpolate(&
                     Trace_DIII(:,iRay,j-1:j+1:2,k), Weight2_I, 2,&
                     Trace_DIII(:,iRay,j,k))
             end do
          else
             do j=1,nJ+1,2
                ! Case a: odd j and even k

                if(IjkTrace_DII(iRay,j,k) /= ray_out_) CYCLE

                call rayface_interpolate(&
                     Trace_DIII(:,iRay,j,k-1:k+1:2), Weight2_I, 2,&
                     Trace_DIII(:,iRay,j,k))
             end do
             do j=2,nJ,2
                ! Case c: even j and even k

                if(IjkTrace_DII(iRay,j,k) /= ray_out_) CYCLE

                call rayface_interpolate(&
                     Trace_DIII(:,iRay,j-1:j+1:2,k-1:k+1:2), Weight4_I, 4,&
                     Trace_DIII(:,iRay,j,k))
             end do ! j
          end if ! mod(k,2)
       end do ! k
    end do ! iRay

    ! Put back result into Trace_DINB
    select case(iFace)
    case(1)
       Trace_DINB(:,:,   1,:,:,iBlock) = Trace_DIII(:,:,1:nFaceJ,1:nFaceK)
    case(2)
       Trace_DINB(:,:,nI+1,:,:,iBlock) = Trace_DIII(:,:,1:nFaceJ,1:nFaceK)
    case(3)
       Trace_DINB(:,:,:,   1,:,iBlock) = Trace_DIII(:,:,1:nFaceJ,1:nFaceK)
    case(4)
       Trace_DINB(:,:,:,nJ+1,:,iBlock) = Trace_DIII(:,:,1:nFaceJ,1:nFaceK)
    case(5)
       Trace_DINB(:,:,:,:,   1,iBlock) = Trace_DIII(:,:,1:nFaceJ,1:nFaceK)
    case(6)
       Trace_DINB(:,:,:,:,nK+1,iBlock) = Trace_DIII(:,:,1:nFaceJ,1:nFaceK)
    end select

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine prolong_ray_after_pass
  !============================================================================
  subroutine ray_pass_old

    ! Exchange and update Trace_DINB values between blocks direction
    ! by direction

    ! Notation: O out        (cells to be sent for equal blocks)
    !           G get        (cells to be received)
    !           R restricted (to be sent to a coarser block)
    !           S subface    (one quarter of a face)

    use ModMain, ONLY: Unused_B
    use ModParallel, ONLY: neiLEV

    ! Local variables

    ! iDir=1,2,3 correspond to east-west, south-north, bot-top.
    integer :: iDir

    integer :: iFace, iBlock

#ifndef OPENACC
      ! Array ranges for outgoing, incoming, restricted and subfaces
      integer :: iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO
      integer :: iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG
      integer :: iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR
      integer :: iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS
#endif

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'ray_pass_old'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    do iDir = 1, 3
       ! Send messages for both faces
       call ray_pass_faces(iDir,2*iDir-1,2*iDir,.true.,.true.,.true.)
    end do ! iDir

    if(DoTest)write(*,*)'ray_pass starting prolongation'

    !$acc parallel loop gang
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE

       do iFace = 1, 6
          if(neiLEV(iFace,iBlock)==1)call prolong_ray(iFace, iBlock)
       end do
    end do

    call test_stop(NameSub, DoTest)
  contains
    !==========================================================================
    subroutine setranges_ray(iFace, iDir, jFace, iSide, iSize, iSizeR, &
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
      !------------------------------------------------------------------------
      if(iDir /= 1)then
         iMinG = 1;  iMaxG = nI+1
         iMinO = 1;  iMaxO = nI+1
         iMinR = 1;  iMaxR = nI/2+1
      end if

      if(iDir /= 2)then
         jMinG = 1;  jMaxG = nJ+1
         jMinO = 1;  jMaxO = nJ+1
         jMinR = 1;  jMaxR = nJ/2+1
      endif

      if(iDir /= 3)then
         kMinG = 1;  kMaxG = nK+1
         kMinO = 1;  kMaxO = nK+1
         kMinR = 1;  kMaxR = nK/2+1
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
      iSize  = 6*(iMaxG-iMinG+1)*(jMaxG-jMinG+1)*(kMaxG-kMinG+1)
      iSizeR = 6*(iMaxR-iMinR+1)*(jMaxR-jMinR+1)*(kMaxR-kMinR+1)

    end subroutine setranges_ray
    !==========================================================================
    subroutine setsubrange_ray(DoSend, iFace, iSubFace, &
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
      !------------------------------------------------------------------------
      select case(iFace)
      case(1,2)
         if(DoSend)then
            iMinS=iMinO; iMaxS=iMaxO
         else
            iMinS=iMinG; iMaxS=iMaxG
         end if

         select case(iSubFace)
            ! Beware, case(2) and case(3) are swapped
         case(1)
            jMinS=jMinR; jMaxS=jMaxR;
            kMinS=kMinR; kMaxS=kMaxR
         case(3)
            jMinS=jMinR+nJ/2; jMaxS=jMaxR+nJ/2;
            kMinS=kMinR; kMaxS=kMaxR
         case(2)
            jMinS=jMinR; jMaxS=jMaxR;
            kMinS=kMinR+nK/2; kMaxS=kMaxR+nK/2;
         case(4)
            jMinS=jMinR+nJ/2; jMaxS=jMaxR+nJ/2;
            kMinS=kMinR+nK/2; kMaxS=kMaxR+nK/2;
         end select
      case(3,4)
         if(DoSend)then
            jMinS=jMinO; jMaxS=jMaxO
         else
            jMinS=jMinG; jMaxS=jMaxG
         end if
         select case(iSubFace)
            ! Beware, case(2) and case(3) are swapped
         case(1)
            iMinS=iMinR;      iMaxS=iMaxR;
            kMinS=kMinR;      kMaxS=kMaxR
         case(3)
            iMinS=iMinR+nI/2; iMaxS=iMaxR+nI/2;
            kMinS=kMinR;      kMaxS=kMaxR
         case(2)
            iMinS=iMinR;      iMaxS=iMaxR;
            kMinS=kMinR+nK/2; kMaxS=kMaxR+nK/2;
         case(4)
            iMinS=iMinR+nI/2; iMaxS=iMaxR+nI/2;
            kMinS=kMinR+nK/2; kMaxS=kMaxR+nK/2;
         end select
      case(5,6)
         if(DoSend)then
            kMinS=kMinO; kMaxS = kMaxO
         else
            kMinS=kMinG; kMaxS = kMaxG
         end if
         select case(iSubFace)
            ! Beware, case(2) and case(3) are not swapped
         case(1)
            iMinS=iMinR;      iMaxS=iMaxR;
            jMinS=jMinR;      jMaxS=jMaxR
         case(2)
            iMinS=iMinR+nI/2; iMaxS=iMaxR+nI/2;
            jMinS=jMinR;      jMaxS=jMaxR
         case(3)
            iMinS=iMinR;      iMaxS=iMaxR;
            jMinS=jMinR+nJ/2; jMaxS=jMaxR+nJ/2;
         case(4)
            iMinS=iMinR+nI/2; iMaxS=iMaxR+nI/2;
            jMinS=jMinR+nJ/2; jMaxS=jMaxR+nJ/2;
         end select
      end select

    end subroutine setsubrange_ray
    !==========================================================================
    subroutine ray_pass_faces(&
         iDir, iFaceMin, iFaceMax, DoEqual, DoRestrict, DoProlong)

      use ModMain, ONLY : okdebug, Unused_B
      use BATL_lib, ONLY: iNode_B, iTree_IA, Coord0_
      use ModParallel, ONLY : NOBLK, neiLEV, neiBLK, neiPE
      use ModMpi

      integer, intent(in):: iDir, iFaceMin,iFaceMax
      logical, intent(in):: DoEqual,DoRestrict,DoProlong

      ! Face, neighbor's face and side indexes
      integer :: iFace, jFace, iSide

      ! number of subfaces (1 or 4), subface (1..nSubFace) and child (1..8) index
      integer ::  nSubFace, iSubFace

#ifdef OPENACC
      ! Array ranges for outgoing, incoming, restricted and subfaces
      integer :: iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO
      integer :: iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG
      integer :: iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR
      integer :: iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS
#endif

      ! Descriptors for neighbor
      integer :: jProc, jBlock, DiLevel

#ifndef OPENACC
      ! MPI variables
      integer :: iTag, iRequest, nRecvRequest, iRecvRequest_I(MaxBlock*6), iError

      ! Maximum size of the RESTRICTED Trace_DINB layer to be received
      ! for the 6 ray variables (3 coord*2 ray dir.)
      integer, parameter :: MaxSizeR = &
           6*max((nI/2+1)*(nJ/2+1),(nI/2+1)*(nK/2+1),(nJ/2+1)*(nK/2+1))

      ! Receive Buffer_IIBI to hold 4 incoming RESTRICTED Trace_DINB values
      ! for all blocks and for both sides
      real :: Buffer_IIBI(MaxSizeR,4,MaxBlock,2)

      ! Equal and restricted values to be sent are stored in these buffers
      real, allocatable :: BufEqual_DIC(:,:,:,:,:), BufRestrict_DIC(:,:,:,:,:)

#endif

      ! Actual size of messages: full, restricted/sparse and actual face
      integer :: iSize, iSizeR, iSize1

      ! BATL related
      integer:: iNode, iDim, iSideFace

      integer:: iS, jS, kS, iR, jR, kR, iRay
      !------------------------------------------------------------------------
#ifdef OPENACC
      do iFace=iFaceMin,iFaceMax
         !$acc parallel loop gang
         do iBlock = 1, nBlock
            if(Unused_B(iBlock))CYCLE
            DiLevel = neiLEV(iFace,iBlock)

            call setranges_ray(iFace, iDir, jFace, iSide, iSize, iSizeR, &
                 iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
                 iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
                 iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR)

            select case(DiLevel)
            case(0)
               if(.not.DoEqual)CYCLE

               jProc=neiPE(1,iFace,iBlock)
               jBlock=neiBLK(1,iFace,iBlock)
               if(jProc==iProc)then
                  ! Local copy

                  !$acc loop vector collapse(3)
                  do kR = kMinG, kMaxG; do jR = jMinG, jMaxG; do iR = iMinG,iMaxG
                     iS = iR - iMinG + iMinO
                     jS = jR - jMinG + jMinO
                     kS = kR - kMinG + kMinO

                     Trace_DINB(:,:,iR,jR,kR,jBlock)=max(&
                          Trace_DINB(:,:,iR,jR,kR,jBlock),&
                          Trace_DINB(:,:,iS,jS,kS,iBlock))
                  end do; end do; end do
               else
                  ! To be implemented
               end if
            case(1)
               if(.not.DoRestrict)CYCLE

               jProc=neiPE(1,iFace,iBlock)
               jBlock=neiBLK(1,iFace,iBlock)
               ! Subface index =1,2,3, or 4 with respect to the coarse neighbor

               ! iSubFace = iSubFace_IA(iFace,iNode_B(iBlock))
               iNode = iNode_B(iBlock)
               iSubFace = 0

               !$acc loop seq
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
                  call setsubrange_ray(.false., iFace, iSubFace, &
                       iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
                       iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
                       iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR, &
                       iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS)

                  !$acc loop vector collapse(3)
                  do kR = kMinS, kMaxS; do jR = jMinS, jMaxS; do iR = iMinS,iMaxS
                     iS = 2*(iR - iMinS) + iMinO
                     jS = 2*(jR - jMinS) + jMinO
                     kS = 2*(kR - kMinS) + kMinO

                     do iRay = 1, 2; do iDim = 1, 3
                        !$acc atomic update
                        Trace_DINB(iDim,iRay,iR,jR,kR,jBlock) = max(&
                             Trace_DINB(iDim,iRay,iS,jS,kS,iBlock),&
                             Trace_DINB(iDim,iRay,iR,jR,kR,jBlock))
                        !$acc end atomic
                     end do; end do
                  end do; end do; end do

               else
                  ! To be implemented
               end if
            case(-1)
               if(.not.DoProlong)CYCLE

               do iSubFace = 1, 4
                  jProc = neiPE(iSubFace,iFace,iBlock)
                  jBlock = neiBLK(iSubFace,iFace,iBlock)

                  call setsubrange_ray(.true., iFace, iSubFace, &
                       iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
                       iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
                       iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR, &
                       iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS)

                  if(jProc==iProc)then
                     ! Local copy of appropriate subface

                     !$acc loop vector collapse(3)
                     do kR=kMinG,kMaxG,2; do jR=jMinG,jMaxG,2; do iR=iMinG,iMaxG,2
                        iS = 0.5*(iR - iMinG) + iMinS
                        jS = 0.5*(jR - jMinG) + jMinS
                        kS = 0.5*(kR - kMinG) + kMinS

                        Trace_DINB(:,:,iR,jR,kR,jBlock)=max(&
                             Trace_DINB(:,:,iR,jR,kR,jBlock),&
                             Trace_DINB(:,:,iS,jS,kS,iBlock))
                     end do; end do; end do

                  else
                     ! To be implemented
                  end if
               end do ! iSubFace

            case(NOBLK)
               ! There is no neighbor, do nothing
               CYCLE
            case default
               ! pass
            end select ! DiLevel
         end do ! iBlock
      end do ! iFace

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#else
      ! CPU version

      if(DoTest)write(*,*)&
           'ray_pass_faces:me,iFaceMin,iFaceMax,do_eq,do_re,do_pr=',&
           iProc,iFaceMin,iFaceMax,DoEqual,DoRestrict,DoProlong

      ! Debug
      if(okdebug)Buffer_IIBI  =0.00

      nRecvRequest = 0
      iRecvRequest_I = MPI_REQUEST_NULL

      do iFace=iFaceMin,iFaceMax
         if(okdebug.and.DoTest)then
            write(*,*)&
                 'setranges_ray for receive Done: me,iFace,iSize,iSizeR',&
                 iProc, iFace, iSize, iSizeR
            write(*,*)'_o=',iMinO,iMaxO,jMinO,jMaxO,kMinO,kMaxO
            write(*,*)'_g=',iMinG,iMaxG,jMinG,jMaxG,kMinG,kMaxG
            write(*,*)'_r=',iMinR,iMaxR,jMinR,jMaxR,kMinR,kMaxR
         end if

         do iBlock = 1, nBlock
            if(Unused_B(iBlock))CYCLE

            ! Set index ranges for the face
            call setranges_ray(iFace, iDir, jFace, iSide, iSize, iSizeR, &
                 iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
                 iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
                 iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR)

            ! Post non-blocking receive for opposite face of neighbor block
            DiLevel = neiLEV(jFace,iBlock)
            select case(DiLevel)
            case(0)
               if(.not.DoEqual)CYCLE
               nSubFace=1
               iSize1=iSize
            case(1)
               if(.not.DoProlong)CYCLE
               nSubFace=1
               iSize1=iSizeR
            case(-1)
               if(.not.DoRestrict)CYCLE
               nSubFace=4
               iSize1=iSizeR
            case(NOBLK)
               ! Do nothing
               CYCLE
            case default
               write(*,*)'me,iBlock,jFace,DiLevel=',&
                    iProc, iBlock, jFace, DiLevel
               call stop_mpi(&
                    'Error in message pass: Invalid value for neiLEV')
            end select

            if(okdebug.and.DoTest)write(*,*)&
                 'receive: me, DiLevel, nSubFace, iSize1',&
                 iProc, DiLevel, nSubFace, iSize1

            do iSubFace=1,nSubFace
               jProc = neiPE(iSubFace,jFace,iBlock)
               if(jProc /= iProc)then
                  ! Remote receive
                  iTag = 100*iBlock+10*iFace+iSubFace

                  if(DoTest.and.okdebug)write(*,*)&
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
      do iFace=iFaceMin,iFaceMax

         ! Set index ranges for the face
         call setranges_ray(iFace, iDir, jFace, iSide, iSize, iSizeR, &
              iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
              iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
              iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR)

         if(okdebug.and.DoTest)write(*,*)&
              'setranges_ray for send Done: me, iFace=',iProc, iFace

         if(DoEqual) allocate(BufEqual_DIC( &
              3,2,iMinO:iMaxO,jMinO:jMaxO,kMinO:kMaxO))
         if(DoRestrict.or.DoProlong) allocate( &
              BufRestrict_DIC(3,2,iMinR:iMaxR,jMinR:jMaxR,kMinR:kMaxR))

         if(okdebug.and.DoTest)write(*,*)'allocation Done, me,iFace=',&
              iProc, iFace

         do iBlock = 1, nBlock
            if(Unused_B(iBlock))CYCLE
            DiLevel = neiLEV(iFace,iBlock)

            if(okdebug.and.DoTest)write(*,*)&
                 'sending: me, iFace,iBlock,DiLevel=', &
                 iProc, iFace, iBlock, DiLevel

            select case(DiLevel)
            case(0)
               if(.not.DoEqual)CYCLE

               jProc=neiPE(1,iFace,iBlock)
               jBlock=neiBLK(1,iFace,iBlock)
               if(jProc==iProc)then
                  ! Local copy
                  if(okdebug.and.DoTest)write(*,*)&
                       'local equal copy: me,iFace,iBlock=',iProc,iFace,iBlock

                  ! Debug
                  ! write(*,*)'Trace_DINB(_o,iBlock)=',&
                  !  Trace_DINB(:,:,iMinO:iMaxO,jMinO:jMaxO,kMinO:kMaxO,iBlock)
                  ! write(*,*)'before: Trace_DINB(_g,jBlock)=',&
                  !  Trace_DINB(:,:,iMinG:iMaxG,jMinG:jMaxG,kMinG:kMaxG,jBlock)

                  Trace_DINB(:,:,iMinG:iMaxG,jMinG:jMaxG,kMinG:kMaxG,jBlock)=&
                       max(&
                       Trace_DINB(:,:,iMinG:iMaxG,jMinG:jMaxG,kMinG:kMaxG, &
                       jBlock),&
                       Trace_DINB(:,:,iMinO:iMaxO,jMinO:jMaxO,kMinO:kMaxO, &
                       iBlock))

                  ! Debug
                  ! write(*,*)'after: Trace_DINB(_g,jBlock)=',&
                  !  Trace_DINB(:,:,iMinG:iMaxG,jMinG:jMaxG,kMinG:kMaxG,jBlock)

               else
                  ! Remote send
                  iTag = 100*jBlock+10*iFace+1
                  if(DoTest.and.okdebug)write(*,*)&
                       'Remote equal send, me,iTag,jProc=',iProc,iTag,jProc

                  BufEqual_DIC=&
                       Trace_DINB(:,:,iMinO:iMaxO,jMinO:jMaxO,kMinO:kMaxO, &
                       iBlock)

                  call MPI_Rsend(BufEqual_DIC,&
                       iSize,MPI_REAL,jProc,iTag,iComm,iError)
               end if
            case(1)
               if(.not.DoRestrict)CYCLE

               ! Restrict Trace_DINB in _o range into _r
               BufRestrict_DIC=&
                    Trace_DINB(:,:,iMinO:iMaxO:2,jMinO:jMaxO:2,kMinO:kMaxO:2, &
                    iBlock)

               jProc=neiPE(1,iFace,iBlock)
               jBlock=neiBLK(1,iFace,iBlock)
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
                  call setsubrange_ray(.false., iFace, iSubFace, &
                       iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
                       iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
                       iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR, &
                       iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS)

                  if(okdebug.and.DoTest)write(*,*)&
                       'local restricted copy: me,iFace,iBlock,_s=',&
                       iProc, iFace, iBlock,&
                       iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS

                  Trace_DINB(:,:,iMinS:iMaxS,jMinS:jMaxS,kMinS:kMaxS,jBlock)=&
                       max(BufRestrict_DIC,&
                       Trace_DINB(:,:,iMinS:iMaxS,jMinS:jMaxS,kMinS:kMaxS, &
                       jBlock))
               else
                  ! Restrict Trace_DINB in _o range into _r
                  BufRestrict_DIC=&
                       Trace_DINB(:,:,iMinO:iMaxO:2,jMinO:jMaxO:2,kMinO:kMaxO:2, &
                       iBlock)

                  ! Remote send
                  iTag = 100*jBlock+10*iFace+iSubFace
                  if(DoTest.and.okdebug)write(*,*)&
                       'Remote restricted send, me,iFace,iTag=',&
                       iProc,iFace,iTag
                  call MPI_Rsend(BufRestrict_DIC,iSizeR,&
                       MPI_REAL,jProc,iTag,iComm,iError)
               end if
            case(-1)
               if(.not.DoProlong)CYCLE

               do iSubFace = 1, 4
                  jProc = neiPE(iSubFace,iFace,iBlock)
                  jBlock = neiBLK(iSubFace,iFace,iBlock)

                  call setsubrange_ray(.true., iFace, iSubFace, &
                       iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
                       iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
                       iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR, &
                       iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS)

                  if(jProc==iProc)then
                     ! Local copy of appropriate subface
                     if(okdebug.and.DoTest)write(*,*)&
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
                     if(DoTest.and.okdebug)write(*,*)&
                          'Remote prolong send, me,iFace,iTag=',&
                          iProc,iFace,iTag

                     call MPI_Rsend(BufRestrict_DIC,iSizeR,&
                          MPI_REAL,jProc,iTag,iComm,iError)
                  end if
               end do ! iSubFace

            case(NOBLK)
               ! There is no neighbor, do nothing
               CYCLE
            case default
               write(*,*)'me,iBlock,iFace,DiLevel=',&
                    iProc, iBlock, iFace, DiLevel
               call stop_mpi('Error in message pass: Invalid value for neiLEV')
            end select ! DiLevel
         end do ! iBlock

         if(DoEqual)deallocate(BufEqual_DIC)
         if(DoRestrict.or.DoProlong)deallocate(BufRestrict_DIC)

         if(DoTest)write(*,*)'messages sent, me, iFace=',iProc,iFace
      end do ! iFace

      ! WAIT FOR ALL MESSAGES TO BE RECEIVED
      if (nRecvRequest > 0) call MPI_waitall(nRecvRequest, iRecvRequest_I, &
           MPI_STATUS_IGNORE, iError)

      if(DoTest)write(*,*)'messages received, me, iDir=',iProc, iDir

      ! Copy ghost cells received from non-local neigbors
      ! and stored in the Buffer_IIBI into sol_BLK

      do iFace = iFaceMin, iFaceMax

         ! Set index ranges for the face
         call setranges_ray(iFace, iDir, jFace, iSide, iSize, iSizeR, &
              iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
              iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
              iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR)

         if(okdebug.and.DoTest)write(*,*)&
              'setranges_ray for buf2ray Done: me, iFace=',iProc, iFace

         do iBlock = 1, nBlock
            if(Unused_B(iBlock))CYCLE
            select case(neiLEV(jFace,iBlock))
            case(0)
               if(okdebug.and.DoTest)&
                    write(*,*)'buf2rayface: me, iBlock=',iProc,iBlock
               if(DoEqual.and.neiPE(1,jFace,iBlock)/=iProc)&
                    call buf2rayface(Buffer_IIBI(1,1,iBlock,iSide),&
                    iMinG,iMaxG,jMinG,jMaxG,kMinG,kMaxG)
            case(1)
               if(okdebug.and.DoTest)&
                    write(*,*)'buf2sparserayface: me, iBlock=',iProc,iBlock
               if(DoProlong.and.neiPE(1,jFace,iBlock)/=iProc)&
                    call buf2sparserayface(Buffer_IIBI(1,1,iBlock,iSide),&
                    iMinR,iMaxR,jMinR,jMaxR,kMinR,kMaxR)
            case(-1)
               if(DoRestrict)then
                  do iSubFace=1,4
                     if(okdebug.and.DoTest)&
                          write(*,*)'buf2subrayface: me, iSubFace, iBlock=',&
                          iProc,iSubFace,iBlock
                     if(neiPE(iSubFace,jFace,iBlock)/=iProc)&
                          call buf2subrayface(&
                          Buffer_IIBI(1,iSubFace,iBlock,iSide),&
                          iFace,iSubFace,iMinR,iMaxR,jMinR,jMaxR,kMinR,kMaxR)
                  end do
               end if
            end select ! DiLevel
         end do ! iBlock
      end do ! iFace

      if(DoTest)write(*,*)'ray_pass_faces finished: me, iFaceMin, iFaceMax=',&
           iProc, iFaceMin, iFaceMax
#endif

    end subroutine ray_pass_faces
    !==========================================================================
#ifndef OPENACC
    subroutine buf2rayface(Buffer_DIN,iMin,iMax,jMin,jMax,kMin,kMax)

      integer, intent(in) :: iMin, iMax, jMin, jMax, kMin, kMax
      real, intent(inout):: Buffer_DIN(3,2,iMin:iMax,jMin:jMax,kMin:kMax)
      ! Take maximum of Trace_DINB and buf (more positive values are more real)
      ! for the full face

      !------------------------------------------------------------------------
      Trace_DINB(:,:,iMinG:iMaxG,jMinG:jMaxG,kMinG:kMaxG,iBlock) &
           = max(Buffer_DIN, &
           Trace_DINB(:,:,iMinG:iMaxG,jMinG:jMaxG,kMinG:kMaxG,iBlock))

    end subroutine buf2rayface
    !==========================================================================
    subroutine buf2sparserayface( &
         Buffer_DIN, iMin, iMax, jMin, jMax, kMin, kMax)

      integer, intent(in) :: iMin,iMax,jMin,jMax,kMin,kMax
      real, intent(inout):: Buffer_DIN(3,2,iMin:iMax,jMin:jMax,kMin:kMax)

      ! Take maximum of Trace_DINB and buf (more positive values are more real)
      ! for a factor of 2 coarser grid
      !------------------------------------------------------------------------
      Trace_DINB(:,:,iMinG:iMaxG:2,jMinG:jMaxG:2,kMinG:kMaxG:2,iBlock) = &
           max(Buffer_DIN,&
           Trace_DINB(:,:,iMinG:iMaxG:2,jMinG:jMaxG:2,kMinG:kMaxG:2,iBlock))

    end subroutine buf2sparserayface
    !==========================================================================
    subroutine buf2subrayface(Buffer_DIN, iFace, iSubFace, iMin,iMax,jMin,jMax,kMin,kMax)

      integer, intent(in) :: iFace, iSubFace, iMin,iMax,jMin,jMax,kMin,kMax
      real, intent(inout) :: Buffer_DIN(3,2,iMin:iMax,jMin:jMax,kMin:kMax)

      ! Set subface range to write into
      !------------------------------------------------------------------------
      call setsubrange_ray(.false., iFace, iSubFace, &
           iMinO, iMaxO, jMinO, jMaxO, kMinO, kMaxO, &
           iMinG, iMaxG, jMinG, jMaxG, kMinG, kMaxG, &
           iMinR, iMaxR, jMinR, jMaxR, kMinR, kMaxR, &
           iMinS, iMaxS, jMinS, jMaxS, kMinS, kMaxS)

      ! Take maximum of Trace_DINB and buf (more positive values are more real)
      ! for the appropriate subface

      Trace_DINB(:,:,iMinS:iMaxS,jMinS:jMaxS,kMinS:kMaxS,iBlock) = &
           max(Buffer_DIN, &
           Trace_DINB(:,:,iMinS:iMaxS,jMinS:jMaxS,kMinS:kMaxS,iBlock))

    end subroutine buf2subrayface
    !==========================================================================
#endif
    subroutine prolong_ray(iFace,iBlock)
      !$acc routine vector

      ! For faces that are shared with a coarser neighbor, interpolate
      ! for all points which are not coinciding and where the ray is going out.
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

      integer, intent(in)::iFace, iBlock

      integer :: iRay
      integer :: i, j, k, nFaceJ, nFaceK

      integer, parameter   :: nFaceMax=max(nI+1,nJ+1,nK+1)

      real    :: Trace_DIII(3,2,nFaceMax,nFaceMax)
      integer :: IjkTrace_DII(2,nFaceMax,nFaceMax)

      ! Q: why not parameter arrays / scalars ???
      ! A: If Weight4_I/Weight2_I are defined as 'parameter', nvfortran+openacc
      !    will produce compilation error.
      ! Interpolation weights
      real :: Weight4_I(4) != 0.25
      real :: Weight2_I(2) != 0.5

      real :: Trace_DI(3,2), Trace4_DI(3,4)
      real :: tmp(3)

#ifndef OPENACC
      character(len=*), parameter:: NameSub = 'prolong_ray'
      !------------------------------------------------------------------------
      if(DoTest)write(*,*) NameSub,': me, iBlock, iFace=',iProc, iBlock, iFace
#endif

      ! Extract Trace_DIII and IjkTrace_DII for the appropriate face
      ! NOTE: IjkTrace_DII assignment split to two lines to avoid
      ! reshaping compiler bug!
      select case(iFace)
      case(1)
         nFaceJ=nJ+1; nFaceK=nK+1
         !$acc loop vector collapse(2)
         do j = 1, nFaceJ ; do k = 1, nFaceK
            Trace_DIII( :,:,j,k)=Trace_DINB(   :,:,1,j,k,iBlock)
            IjkTrace_DII(1,j,k)=IjkTrace_DINB(1,1,1,j,k,iBlock)
            IjkTrace_DII(2,j,k)=IjkTrace_DINB(1,2,1,j,k,iBlock)
         end do; end do
      case(2)
         nFaceJ=nJ+1; nFaceK=nK+1
         !$acc loop vector collapse(2)
         do j = 1, nFaceJ; do k = 1, nFaceK
            Trace_DIII( :,:,j,k)=Trace_DINB(   :,:,nI+1,j,k,iBlock)
            IjkTrace_DII(1,j,k)=IjkTrace_DINB(1,1,nI+1,j,k,iBlock)
            IjkTrace_DII(2,j,k)=IjkTrace_DINB(1,2,nI+1,j,k,iBlock)
         end do; end do
      case(3)
         nFaceJ=nI+1; nFaceK=nK+1
         !$acc loop vector collapse(2)
         do i = 1, nFaceJ; do k = 1, nFaceK
            Trace_DIII( :,:,i,k)=Trace_DINB(   :,:,i,1,k,iBlock)
            IjkTrace_DII(1,i,k)=IjkTrace_DINB(1,1,i,1,k,iBlock)
            IjkTrace_DII(2,i,k)=IjkTrace_DINB(1,2,i,1,k,iBlock)
         end do; end do
      case(4)
         nFaceJ=nI+1; nFaceK=nK+1
         !$acc loop vector collapse(2)
         do i = 1, nFaceJ; do k = 1, nFaceK
            Trace_DIII( :,:,i,k)=Trace_DINB(   :,:,i,nJ+1,k,iBlock)
            IjkTrace_DII(1,i,k)=IjkTrace_DINB(1,1,i,nJ+1,k,iBlock)
            IjkTrace_DII(2,i,k)=IjkTrace_DINB(1,2,i,nJ+1,k,iBlock)
         end do; end do
      case(5)
         nFaceJ=nI+1; nFaceK=nJ+1
         !$acc loop vector collapse(2)
         do i = 1, nFaceJ; do j = 1, nFaceK
            Trace_DIII(:,:,i,j)=Trace_DINB(   :,:,i,j,1,iBlock)
            IjkTrace_DII(1,i,j)=IjkTrace_DINB(1,1,i,j,1,iBlock)
            IjkTrace_DII(2,i,j)=IjkTrace_DINB(1,2,i,j,1,iBlock)
         end do; end do
      case(6)
         nFaceJ=nI+1; nFaceK=nJ+1
         !$acc loop vector collapse(2)
         do i = 1, nFaceJ; do j = 1, nFaceK
            Trace_DIII(:,:,i,j)=Trace_DINB(   :,:,i,j,nK+1,iBlock)
            IjkTrace_DII(1,i,j)=IjkTrace_DINB(1,1,i,j,nK+1,iBlock)
            IjkTrace_DII(2,i,j)=IjkTrace_DINB(1,2,i,j,nK+1,iBlock)
         end do; end do
      case default
#ifndef OPENACC
         call stop_mpi(NameSub//': Impossible value for iFace')
#endif
      end select

      !$acc loop vector collapse(2) private(Trace_DI, Weight2_I, Weight4_I)
      do iRay=1,2
         do k=1,nfaceK
            Weight2_I = 0.5
            Weight4_I = 0.25

            if(mod(k,2)==1)then
               do j=2,nfaceJ,2
                  ! Case b: even j and odd k

                  if(IjkTrace_DII(iRay,j,k)/=ray_out_)CYCLE

                  Trace_DI(:,1) = Trace_DIII(:,iRay,j-1,k)
                  Trace_DI(:,2) = Trace_DIII(:,iRay,j+1,k)

                  call rayface_interpolate(Trace_DI, Weight2_I, 2,&
                       Trace_DIII(:,iRay,j,k))
               end do
            else
               do j=1,nJ+1,2
                  ! Case a: odd j and even k

                  if(IjkTrace_DII(iRay,j,k)/=ray_out_)CYCLE

                  Trace_DI(:,1) = Trace_DIII(:,iRay,j,k-1)
                  Trace_DI(:,2) = Trace_DIII(:,iRay,j,k+1)
                  call rayface_interpolate(Trace_DI, Weight2_I, 2,&
                       Trace_DIII(:,iRay,j,k))
               end do
               do j=2,nJ,2
                  ! Case c: even j and even k

                  if(IjkTrace_DII(iRay,j,k)/=ray_out_)CYCLE

                  Trace4_DI(:,1) = Trace_DIII(:,iRay,j-1,k-1)
                  Trace4_DI(:,2) = Trace_DIII(:,iRay,j+1,k-1)
                  Trace4_DI(:,3) = Trace_DIII(:,iRay,j-1,k+1)
                  Trace4_DI(:,4) = Trace_DIII(:,iRay,j+1,k+1)
                  call rayface_interpolate(Trace4_DI, Weight4_I, 4,&
                       Trace_DIII(:,iRay,j,k))

               end do ! j
            end if ! mod(k,2)
         end do ! k
      end do ! iRay

      ! Put back result into Trace_DINB
      select case(iFace)
      case(1)
         !$acc loop vector collapse(2)
         do k = 1, nFaceK; do j = 1, nFaceJ
            Trace_DINB(:,:,1,j,k,iBlock) = Trace_DIII(:,:,j,k)
         end do; end do
      case(2)
         !$acc loop vector collapse(2)
         do k = 1, nFaceK; do j = 1, nFaceJ
            Trace_DINB(:,:,nI+1,j,k,iBlock) = Trace_DIII(:,:,j,k)
         end do; end do
      case(3)
         !$acc loop vector collapse(2)
         do k = 1, nFaceK; do i = 1, nFaceJ
            Trace_DINB(:,:,i,1,k,iBlock) = Trace_DIII(:,:,i,k)
         end do; end do
      case(4)
         !$acc loop vector collapse(2)
         do k = 1, nFaceK; do i = 1, nFaceJ
            Trace_DINB(:,:,i,nJ+1,k,iBlock) = Trace_DIII(:,:,i,k)
         end do; end do
      case(5)
         !$acc loop vector collapse(2)
         do j = 1, nFaceK; do i = 1, nFaceJ
            Trace_DINB(:,:,i,j,1,iBlock) = Trace_DIII(:,:,i,j)
         end do; end do
      case(6)
         !$acc loop vector collapse(2)
         do j = 1, nFaceK; do i = 1, nFaceJ
            Trace_DINB(:,:,i,j,nK+1,iBlock) = Trace_DIII(:,:,i,j)
         end do; end do
      end select

    end subroutine prolong_ray
    !==========================================================================

  end subroutine ray_pass_old
  !============================================================================
end module ModFieldTraceFast
!==============================================================================
