!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModFieldTraceFast

  use ModFieldTrace

  use BATL_lib, ONLY: &
       test_start, test_stop, StringTest, xTest, yTest, zTest, &
       iTest, jTest, kTest, iBlockTest, iProcTest, iProc, iComm, nProc, &
       IsNeighbor_P, nDim, nI, nJ, nK, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, &
       MaxBlock, x_, y_, z_, IsCartesianGrid
  use ModMain, ONLY: iNewDecomposition, TypeCoordSystem
  use ModPhysics, ONLY: rBody
#ifdef OPENACC
  use ModUtilities, ONLY: norm2
#endif
  use ModNumConst, ONLY: i_DD
  use ModKind, ONLY: Real8_
  use ModIO,   ONLY: iUnitOut, write_prefix

  implicit none
  save

  private ! except
  public:: trace_field_grid           ! trace field from 3D MHD grid cells

  ! Local variables --------------------------------

  ! Stored face and cell indices of the 2 rays starting from a face of a block
  integer, allocatable :: IjkTrace_DINB(:,:,:,:,:,:)

  ! Stored weights for the 2 rays starting from a face of a block
  real, allocatable :: WeightTrace_DINB(:,:,:,:,:,:)

  ! Node interpolated magnetic field components without B0
  real, allocatable :: b_DNB(:,:,:,:,:)

  ! Prefer open and closed field lines in interpolation ?!
  logical :: UsePreferredInterpolation

  ! Testing
  logical :: DoTestRay = .false.

contains
  !============================================================================
  subroutine init_mod_trace_fast

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
    use CON_axes,    ONLY: transform_matrix

    ! remember last call and the last grid number
    integer :: nStepLast=-1, iLastGrid=-1, iLastDecomposition=-1

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'trace_field_grid'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

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
    nStepLast=n_step; iLastGrid = iNewGrid; iLastDecomposition = iNewDecomposition

    call timing_start(NameSub)

    call init_mod_field_trace

    ! Transformation matrix between the SM(G) and GM coordinates
    if(UseSmg) &
         GmSm_DD = transform_matrix(time_simulation,'SMG',TypeCoordSystem)

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
    use ModAdvance,  ONLY: Bx_, Bz_, State_VGB
    use ModB0,       ONLY: get_b0, B0_DGB
    use ModParallel, ONLY: NOBLK, neiLEV
    use ModGeometry, ONLY: R_BLK, Rmin_BLK, true_cell
    use BATL_lib, ONLY: Xyz_DGB, CellSize_DB
    use ModMpi

    use BATL_lib, ONLY: message_pass_cell, message_pass_node

    ! Iteration parameters
    integer, parameter :: MaxIterTrace = 150
    integer :: nIterTrace
    logical :: Done
    real    :: dTraceMin

    real :: TraceTmp_D(3)

    ! Minimum value of B for which integration of field lines makes any sense
    real, parameter :: SmallB = 1e-8

    ! True if Rmin_BLK < rTrace
    logical :: DoCheckInside

    ! Face index for the final point of the ray
    integer :: iFace

    ! Control volume limits in local coordinates
    real, dimension(3), parameter :: &
         GenMin_D=[   0.5,   0.5,   0.5],&
         GenMax_D=[nI+0.5,nJ+0.5,nK+0.5]

    ! Current position of ray in normalized and physical coordinates
    real, dimension(3) :: x, Xyz_D

    ! Radial distance and square of it: r2=sum(Xyz_D**2)
    real :: r2

    ! Cell indices corresponding to current or final x position
    integer :: i1,j1,k1,i2,j2,k2

    ! Distance between x and i1,j1,k1, and i2,j2,k2
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
    call test_start(NameSub, DoTest)
    if(DoTime)call timing_reset('ray_pass',2)

    !$acc update host(State_VGB, B0_DGB)

    DoTestRay = .false.

    b_DGB(:,:,:,:,1:nBlock) = State_VGB(Bx_:Bz_,:,:,:,1:nBlock)
    ! Fill in ghost cells
    call message_pass_cell(3, b_DGB)

    ! Initial values !!! Maybe LOOPRAY would be better??

    Trace_DINB=NORAY
    ray=NORAY

    do iBlock = 1, nBlockMax
       if(Unused_B(iBlock))then
          ! Trace_DINB in unused blocks is assigned to NORAY-1.
          Trace_DINB(:,:,:,:,:,iBlock)=NORAY-1.
          CYCLE
       end if
       ! Inner points of Trace_DINB should never be used, assign them to OPEN
       ! so that checking for blocks with fully open rays becomes easy
       Trace_DINB(:,:,2:nI,2:nJ,2:nK,iBlock)=OPENRAY

       ! Set Trace_DINB=OPENRAY at outer boundaries
       if(neiLEV(1,iBlock)==NOBLK)Trace_DINB(:,:,   1,:,:,iBlock)=OPENRAY
       if(neiLEV(2,iBlock)==NOBLK)Trace_DINB(:,:,nI+1,:,:,iBlock)=OPENRAY
       if(neiLEV(3,iBlock)==NOBLK)Trace_DINB(:,:,:,   1,:,iBlock)=OPENRAY
       if(neiLEV(4,iBlock)==NOBLK)Trace_DINB(:,:,:,nJ+1,:,iBlock)=OPENRAY
       if(neiLEV(5,iBlock)==NOBLK)Trace_DINB(:,:,:,:,   1,iBlock)=OPENRAY
       if(neiLEV(6,iBlock)==NOBLK)Trace_DINB(:,:,:,:,nK+1,iBlock)=OPENRAY

    end do
    if(DoTest)write(*,*)'ray_trace initialized ray and Trace_DINB arrays'

    ! Interpolate the B1 field to the nodes
    do iBlock=1, nBlock
       if(Unused_B(iBlock))CYCLE

       do k=1,nK+1; do j=1,nJ+1; do i=1,nI+1; do iDim = 1, 3
          b_DNB(iDim,i,j,k,iBlock) = &
               sum(b_DGB(iDim,i-1:i,j-1:j,k-1:k,iBlock))*0.125
       end do; end do; end do; end do

    end do ! iBlock

    ! Average node values between shared faces
    call message_pass_node(3,b_DNB)

    if(DoTest)write(*,*)'Trace_DINB normalized B'
    if(DoTime.and.iProc==0)then
       write(*,'(a)',ADVANCE='NO') 'setup and normalization:'
       call timing_show('ray_trace',1)
    end if

    if(DoTest)write(*,*)'ray_trace starting iterations to obtain Trace_DINB'

    ! Iterate
    dTraceMin=rIonosphere*1.0e-6
    UsePreferredInterpolation = .false.
    nIterTrace=0
    do

       if(DoTest)write(*,*)'nIterTrace=',nIterTrace

       if(nIterTrace>=MaxIterTrace)EXIT

       ! Store Trace_DINB into ray so we can see if there is any change
       ray(:,:,:,:,:,1:nBlockMax) = Trace_DINB(:,:,:,:,:,1:nBlockMax)

       !! acc parallel loop gang independent
       do iBlock = 1, nBlock
          if(Unused_B(iBlock))CYCLE

          ! Flag cells inside the ionosphere if necessary
          DoCheckInside=Rmin_BLK(iBlock)<rTrace

          !! acc loop vector collapse(3) independent
          do iZ=1,nK+1; do iY=1,nJ+1; do iX=1,nI+1
             ! Exclude inside points
             if(iX>1 .and. iX<=nI .and. iY>1 .and. iY<=nJ &
                  .and. iZ>1 .and. iZ<=nK ) CYCLE

             ! Exclude outer boundaries
             if(neiLEV(1,iBlock)==NOBLK .and. iX==   1)CYCLE
             if(neiLEV(2,iBlock)==NOBLK .and. iX==nI+1)CYCLE
             if(neiLEV(5,iBlock)==NOBLK .and. iZ==   1)CYCLE
             if(neiLEV(6,iBlock)==NOBLK .and. iZ==nK+1)CYCLE
             if(neiLEV(3,iBlock)==NOBLK .and. iY==   1)CYCLE
             if(neiLEV(4,iBlock)==NOBLK .and. iY==nJ+1)CYCLE

             if(DoTestRay)write(*,*)'TESTING RAY: me,iBlock,iX,iY,iZ,Xyz_D',&
                  iProc,iBlock,iX,iY,iZ,&
                  Xyz_DGB(:,iX,iY,iZ,iBlock)-0.5*CellSize_DB(:,iBlock)

             if(nIterTrace==0)then
                do iRay=1,2
                   ! Follow ray in direction iRay
                   GenIni_D = [iX-0.5, iY-0.5, iZ-0.5]
                   iFace = follow_fast(.true.,GenIni_D)

                   ! Assign value to Trace_DINB
                   call assign_ray(.true.,Trace_DINB(:,iRay,iX,iY,iZ,iBlock))

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
                do iRay=1,2
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

                      call rayface_interpolate(&
                           Trace_DINB(:,iRay,i1:i2,j1:j2,k1:k2,iBlock),&
                           WeightTrace_DINB(:,iRay,iX,iY,iZ,iBlock),4,&
                           TraceTmp_D)

                      Trace_DINB(:,iRay,iX,iY,iZ,iBlock) = TraceTmp_D

                   end if
                end do
             end if ! nIterTrace==0
          end do; end do; end do ! iZ, iY, iX
       end do ! iBlock

       ! Exchange Trace_DINB information

       call timing_start('ray_pass')
       call ray_pass
       call timing_stop('ray_pass')

       nIterTrace = nIterTrace + 1

       if(DoTime .and. iProc == 0 .and. nIterTrace == 1)then
          write(*,'(a)',ADVANCE='NO') 'first iteration:'
          call timing_show('ray_trace',1)
       end if

       !! acc serial
       ! Check if we are Done by checking for significant changes in Trace_DINB
       Done = all(abs(ray(:,:,:,:,:,1:nBlock) - &
            Trace_DINB(:,:,:,:,:,1:nBlock)) < dTraceMin)
       !! acc end serial

       if(nProc > 1)call MPI_allreduce(MPI_IN_PLACE, Done, 1, MPI_LOGICAL, &
            MPI_LAND, iComm, iError)

       if(Done)then
          do iBlock = 1, nBlock
             if(Unused_B(iBlock))CYCLE
             Done = all(Trace_DINB(1,:,:,:,:,iBlock) > LOOPRAY) ! !! NORAY)
             if(.not.Done)EXIT
          end do
          call MPI_allreduce(MPI_IN_PLACE, Done, 1, MPI_LOGICAL, &
               MPI_LAND,iComm,iError)
          if(Done) EXIT
          if(UsePreferredInterpolation)then
             if(iProc==0)call error_report('ray tracing, nIterTrace=',&
                  nIterTrace+0.0,iError1,.true.)
             EXIT
          endif
          if(DoTest)write(*,*)'Switching to UsePreferredInterpolation=.true.'
          UsePreferredInterpolation = .true.
       end if

    end do ! ray iteration

#ifndef OPENACC
    ! Check for unassigned Trace_DINB in every used block
    if(DoTest)then
       write(*,*)'ray_trace finished after ',nIterTrace,' iterations'
       do iBlock = 1, nBlock
          if(Unused_B(iBlock))CYCLE
          do iRay=1,2
             if(any(Trace_DINB(1,iRay,1:nI,1:nJ,1:nK,iBlock)<BODYRAY))then
                Ijk_D=minloc(Trace_DINB(1,iRay,1:nI,1:nJ,1:nK,iBlock))
                write(*,*)'LOOPRAYFACE: iRay,me,Ijk_D,value,x,y,z=',&
                     iRay,iProc,Ijk_D,iBlock,&
                     minval(Trace_DINB(1,iRay,1:nI,1:nJ,1:nK,iBlock)),&
                     Xyz_DGB(:,Ijk_D(1),Ijk_D(2),Ijk_D(3),iBlock)-0.5*CellSize_DB(:,iBlock)
             end if
          end do
       end do
       if(index(StringTest,'ray_debugger')>0)call ray_debugger
    end if

    if(DoTime.and.iProc==0)then
       write(*,'(i5,a)') nIterTrace,' iterations:'
       call timing_show('ray_trace',1)
       call timing_show('ray_pass',2)
    end if

    if(DoTest)write(*,*)'ray_trace starting cell center assignments'
#endif

    ! Assign face ray values to cell centers
    !! acc parallel loop gang independent private(DoCheckInside)
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
          !! acc loop vector collapse(3) independent
          do iZ=1,nK; do iY=1,nJ; do iX=1,nI

             ! Short cuts for inner and false cells
             if(R_BLK(iX,iY,iZ,iBlock) < rInner .or. &
                  .not.true_cell(iX,iY,iZ,iBlock))then
                ray(:,iRay,iX,iY,iZ,iBlock)=BODYRAY
                if(DoTestRay)write(*,*)'BODYRAY'
                CYCLE
             end if

             if(DoTestRay)write(*,*)'calling follow_fast'

             ! Follow ray in direction iRay
             GenIni_D = [real(iX), real(iY), real(iZ)]
             iFace = follow_fast(.false., GenIni_D)

             if(DoTestRay)write(*,*)'calling assign_ray'

             ! Assign value to ray
             call assign_ray(.false., ray(:,iRay,iX,iY,iZ,iBlock))

          end do; end do; end do ! iX, iY, iZ
       end do ! iRay
    end do ! iBlock

    if(DoTest)write(*,*)'ray_trace finished with ray=',&
         ray(:,:,iTest,jTest,kTest,iBlockTest)

    if(DoTest)then
       ! Check for unassigned cell centers
       do iBlock = 1, nBlock
          if(Unused_B(iBlock))CYCLE
          do iRay=1,2
             if(any(ray(1,iRay,1:nI,1:nJ,1:nK,iBlock)<BODYRAY))then
                Ijk_D=minloc(ray(1,iRay,1:nI,1:nJ,1:nK,iBlock))
                write(*,*)'LOOPRAY: iRay,me,Ijk_D,value,x,y,z=',&
                     iRay,iProc,Ijk_D,iBlock,&
                     minval(ray(1,iRay,1:nI,1:nJ,1:nK,iBlock)),&
                     Xyz_DGB(:,Ijk_D(1),Ijk_D(2),Ijk_D(3),iBlock)
             end if
          end do
       end do
    end if

    if(DoTest)write(*,*)'ray_trace starting conversion to lat/lon'

    ! Convert x, y, z to latitude and longitude, and status
    !! acc parallel loop gang independent
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       !! acc loop vector collapse(3) independent
       do k=1,nK; do j=1,nJ; do i=1,nI
          call xyz_to_latlonstatus(ray(:,:,i,j,k,iBlock))
       end do; end do; end do
    end do

    if(DoTime.and.iProc==0)then
       write(*,'(a)',ADVANCE='NO') 'Total ray tracing time:'
       call timing_show('ray_trace',1)
    end if
    call barrier_mpi
    if(DoTest)write(*,*)'ray_trace completed.'

    call test_stop(NameSub, DoTest)
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
         if(xTest==0.0.and.yTest==0.0.and.zTest==0.0) EXIT

         ! Find position
         do iBlock = 1, nBlock
            if(Unused_B(iBlock))CYCLE
            do iX=1,nI+1
               if(abs(Xyz_DGB(x_,iX,1,1,iBlock)-0.5*CellSize_DB(x_,iBlock)-xTest)>0.01)CYCLE
               do iY=1,nJ+1
                  if(abs(Xyz_DGB(y_,1,iY,1,iBlock)-0.5*CellSize_DB(y_,iBlock)-yTest)>0.01)CYCLE
                  do iZ=1,nK+1
                     if(abs(Xyz_DGB(z_,1,1,iZ,iBlock)-0.5*CellSize_DB(z_,iBlock)-zTest)>0.01)&
                          CYCLE

                     ! Print information

                     write(*,*)'iProc,iBlock,iX,iY,iZ=',iProc,iBlock,iX,iY,iZ
                     write(*,*)' x,y,Xyz_DGB(z_,1,1,1),dx=',&
                          Xyz_DGB(:,1,1,1,iBlock), CellSize_DB(x_,iBlock)

                     iPos_D=IjkTrace_DINB(:,iRay,iX,iY,iZ,iBlock)
                     if(iPos_D(1)>0)then
                        write(*,*)' Trace_DINB   =',Trace_DINB(:,iRay,iX,iY,iZ,iBlock)
                        write(*,*)' IjkTrace_DINB=',IjkTrace_DINB(:,iRay,iX,iY,iZ,iBlock)
                        write(*,*)' WeightTrace_DINB=',WeightTrace_DINB(:,iRay,iX,iY,iZ,iBlock)
                        select case(iPos_D(1))
                        case(1,2)
                           jX = 1+nI*(iPos_D(1)-1); jY = iPos_D(2); jZ = iPos_D(3)
                           kX = jX; kY = jY+1; kZ = jZ+1
                        case(3,4)
                           jY = 1+nJ*(iPos_D(1)-3); jX = iPos_D(2); jZ = iPos_D(3)
                           kX = jX+1; kY = jY; kZ = jZ+1
                        case(5,6)
                           jZ = 1+nK*(iPos_D(1)-5); jX = iPos_D(2); jY = iPos_D(3)
                           kX = jX+1; kY = jY+1; kZ = jZ
                        end select
                        write(*,*)' Trace_DINB(1,end)=',&
                             Trace_DINB(1,iRay,jX:kX,jY:kY,jZ:kZ,iBlock)
                        write(*,*)' jX,kX,jY,kY,jZ,kZ=',jX,kX,jY,kY,jZ,kZ
                        write(*,*)' x,y,z(End)=',&
                             Xyz_DGB(:,jx,jy,jz,iBlock)-0.5*CellSize_DB(:,iBlock)
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
    function follow_fast(IsSurfacePoint, GenIn_D) result(iFaceOut)
      !! acc routine seq

      ! Follow ray starting at initial position GenIn_D in direction iRay
      ! until we hit the wall of the control volume or the ionosphere.
      ! Return 1,2,3,4,5,6 if the ray hit the block faces
      ! Return ray_iono_   if the ray hit the ionosphere
      ! Return ray_loop_   if the ray did not hit anything
      ! Return ray_out_    if the ray goes out of the box immediately
      ! Return ray_body_   if the ray goes into or is inside a body

      ! Arguments

      logical, intent(in):: IsSurfacePoint
      real, intent(in)   :: GenIn_D(3)

      ! Result

      integer :: iFaceOut

      ! Local variables

      ! Initial and mid point coordinates and bb field
      real, dimension(3) :: xIni_D, xMid_D, bIni_D, bMid_D, GenIni_D
      real :: r, rIni

      ! dx is the difference between 1st and 2nd order RK to estimate accuracy
      ! DxOpt is the required accuracy, DxRel=dx/DxOpt
      real :: DxRel, DxOpt

      ! Line length, max, step size, limits, next step size, back to surface
      real :: l, sMax, Ds, DsMax, DsMin, DsNext, DsTiny, DsBack

      ! counter for ray integration
      integer :: nSegment

      ! Counter for entering follow_fast_iono
      integer :: nIono
      !------------------------------------------------------------------------
      if(DoTestRay)&
           write(*,*)'follow_fast: me,iBlock,IsSurfacePoint,GenIn_D,iRay=',&
           iProc, iBlock, IsSurfacePoint, GenIn_D, iRay

      ! Step size limits
      DsMax=1.0
      DsMin=0.05
      DsTiny=1.e-6

      ! Initial value
      DsNext=sign(DsMax,1.5-iRay)

      ! Accuracy in terms of x in normalized coordinates
      DxOpt=0.01

      ! Length and maximum length of ray within control volume
      l=0
      sMax=10*maxval(GenMax_D-GenMin_D)
      nSegment=0
      nIono=0

      ! Initial position
      x = GenIn_D

      ! Integration loop
      do
         ! Check if we are inside the ionosphere
         if(DoCheckInside)then
            ! Convert x to real coordinates Xyz_D

            Xyz_D = Xyz_DGB(:,1,1,1,iBlock) + CellSize_DB(:,iBlock)*(x - 1.)

            r2 = sum(Xyz_D**2)

            if(r2 <= rTrace2)then

               if(DoTestRay)write(*,*)&
                    'Inside rTrace at me,iBlock,nSegment,x,Xyz_D=',&
                    iProc,iBlock,nSegment,x,Xyz_D

               if(NameVectorField /= 'B' .or. r2 <= rInner2)then
                  if(nSegment==0)then
                     iFaceOut = ray_body_
                     if(DoTestRay)write(*,*)&
                          'Initial point inside rInner at me,iBlock,Xyz_D=',&
                          iProc,iBlock,Xyz_D
                  else
                     r = sqrt(r2)
                     GenIni_D = Xyz_DGB(:,1,1,1,iBlock) + &
                          CellSize_DB(:,iBlock)*(xIni_D-1.)

                     rIni = norm2(GenIni_D)
                     ! Interpolate to the surface linearly along last segment
                     Xyz_D = (Xyz_D*(rIni-rInner)+GenIni_D*(rInner-r)) &
                          /(rIni-r)
                     ! Normalize Xyz_D in radial direction
                     Xyz_D = rInner*Xyz_D/norm2(Xyz_D)
                     x = Xyz_D
                     iFaceOut = ray_iono_
                  end if
                  EXIT
               end if

               ! Try mapping down to rIonosphere if we haven't tried yet
               if(nIono<1)then
                  if(follow_fast_iono())then
                     x=Xyz_D
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
         Ds=DsNext
         xIni_D=x

         ! Half step
         call interpolate_bb_node(xIni_D,bIni_D)
         xMid_D=xIni_D+0.5*Ds*bIni_D

         ! Check if the ray is pointing outwards
         if(nSegment==0.and.IsSurfacePoint)then
            if(DoTestRay)write(*,*)'me,iBlock,xIni_D,bIni_D=', &
                 iProc,iBlock,xIni_D,bIni_D

            if(any(xMid_D<GenMin_D) .or. any(xMid_D>GenMax_D))then
               iFaceOut=ray_out_
               if(DoTestRay)then
                  write(*,*)'me,iBlock,xMid_D=',iProc,iBlock,xMid_D
                  write(*,*)'ray points outwards: me,iBlock,Ds,Xyz_D=', &
                       iProc,iBlock,Ds,&
                       Xyz_DGB(:,1,1,1,iBlock) + CellSize_DB(:,iBlock)*(xMid_D - 1.)
               end if
               RETURN
            end if
         end if

         do
            ! Full step
            call interpolate_bb_node(xMid_D,bMid_D)

            ! Calculate the difference between 1st and 2nd order integration
            ! and take ratio relative to DxOpt
            DxRel=abs(Ds)*maxval(abs(bMid_D-bIni_D))/DxOpt

            if(DoTestRay.and.okdebug)&
                 write(*,*)'me,iBlock,xMid_D,bMid_D,DxRel=', &
                 iProc,iBlock,xMid_D,bMid_D,DxRel

            ! Make sure that Ds does not change more than a factor of 2 or 0.5
            DxRel=max(0.5,min(2.,DxRel))

            if(DxRel>1.)then
               ! Not accurate enough, decrease Ds if possible

               if(abs(Ds)<=DsMin+DsTiny)then
                  ! Cannot reduce Ds further
                  DsNext=Ds
                  EXIT
               end if

               Ds = sign(max(DsMin,abs(Ds)/(DxRel+0.001)),Ds)

               ! New mid point using the reduced Ds
               xMid_D=xIni_D+0.5*Ds*bIni_D

               if(DoTestRay.and.okdebug)&
                    write(*,*)'new decreased Ds: me,iBlock,Ds=', &
                    iProc,iBlock,Ds
            else
               ! Too accurate, increase Ds if possible
               if(abs(Ds)<DsMax-DsTiny)then

                  DsNext = sign(min(DsMax,abs(Ds)/sqrt(DxRel)),Ds)

                  if(DoTestRay.and.okdebug)&
                       write(*,*)'new increased DsNext: me,iBlock,DsNext=', &
                       iProc,iBlock,DsNext

               end if

               EXIT
            end if
         end do

         x=xIni_D+bMid_D*Ds

         nSegment = nSegment+1
         l = l + abs(Ds)

         if(DoTestRay.and.okdebug)&
              write(*,*)'me,iBlock,nSegment,l,x=', &
              iProc, iBlock, nSegment, l, x

         ! Check if the ray hit the wall of the control volume
         if(any(x<GenMin_D) .or. any(x>GenMax_D))then

            ! Hit the wall, backup so that x is almost exactly on the wall
            ! just a little bit outside. Only if nSegment is more than 1!
            if(nSegment > 1)then
               DsBack = Ds*maxval(max(GenMin_D-x,x-GenMax_D) &
                    /(abs(x-xIni_D)+DsTiny))
               x = x-DsBack*bMid_D
            end if

            ! Find out which wall the ray hit
            if    (x(1)<=GenMin_D(1))then; iFaceOut=1
            elseif(x(2)<=GenMin_D(2))then; iFaceOut=3
            elseif(x(3)<=GenMin_D(3))then; iFaceOut=5
            elseif(x(1)>=GenMax_D(1))then; iFaceOut=2
            elseif(x(2)>=GenMax_D(2))then; iFaceOut=4
            elseif(x(3)>=GenMax_D(3))then; iFaceOut=6
            else
               write(*,*)'Error in follow_fast for me,iBlock,iX,iY,iZ=',&
                    iProc,iBlock,iX,iY,iZ
               write(*,*)'nSegment,x,Ds,DsBack=',nSegment,x,Ds,DsBack
               call stop_mpi('GM_follow_fast: Hit wall but which one?')
            end if

            ! Make sure that x is not outside the control volume
            x=max(GenMin_D+DsTiny,x)
            x=min(GenMax_D-DsTiny,x)

            EXIT
         end if

         ! Check if we have integrated for too long
         if(l>sMax)then
            ! Seems to be a closed loop within a block
            if(DoTestRay)then
               write(*,*)'CLOSED LOOP at me,iBlock,iX,iY,iZ,x,Xyz_D=',&
                    iProc,iBlock,iX,iY,iZ,x,&
                    Xyz_DGB(:,1,1,1,iBlock) + CellSize_DB(:,iBlock)*(x - 1.)
            end if

            iFaceOut=ray_loop_
            EXIT
         end if

      end do

      if(DoTestRay)write(*,*) &
           'Finished follow_fast at me,iBlock,nSegment,iFaceOut,x,Xyz_D=',&
           iProc,iBlock,nSegment,iFaceOut,x,&
           Xyz_DGB(:,1,1,1,iBlock) + CellSize_DB(:,iBlock)*(x - 1.)

    end function follow_fast
    !==========================================================================
    subroutine interpolate_bb_node(Gen_D, b_D)
      !! acc routine seq

      ! Interpolate normalized field b_D at normalized location Gen_D
      ! Interpolate B1 from nodes, take B0 from analytic expression

      real, intent(in) :: Gen_D(3)
      real, intent(out):: b_D(3)
      real :: b
      !------------------------------------------------------------------------

      ! Determine cell indices corresponding to location Gen_D
      i1 = floor(Gen_D(1)+0.5); i2 = i1 + 1
      j1 = floor(Gen_D(2)+0.5); j2 = j1 + 1
      k1 = floor(Gen_D(3)+0.5); k2 = k1 + 1

      if(i1<0 .or. i2>nI+2 .or. j1<0 .or. j2>nJ+2 .or. k1<0 .or. k2>nK+2)then
         write(*,*)'interpolate_bb_node: iProc, iBlock, Gen_D=', &
              iProc, iBlock, Gen_D
         call stop_mpi('ERROR in interpolate_bb_node: location out of bounds')
      endif

      ! Get B0 values for location
      Xyz_D = Xyz_DGB(:,1,1,1,iBlock) + CellSize_DB(:,iBlock)*(Gen_D - 1.)

      if(UseB0)then
         call get_b0(Xyz_D, b_D)
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
    logical function follow_fast_iono()
      !! acc routine seq

      ! Follow ray inside ionosphere starting from Xyz_D which is given in
      ! real coordinates and use analytic mapping.
      ! On return Xyz_D contains the final coordinates.
      ! Return true if it was successfully integrated down to rIonosphere,
      ! return false if the ray exited rTrace or too many integration
      ! steps were Done

      use ModMain,     ONLY: Time_Simulation
      use ModPhysics,  ONLY: DipoleStrengthSi ! only the sign of dipole is needed
      use CON_planet_field, ONLY: map_planet_field

      integer :: iHemisphere
      real    :: x_D(3)
      !------------------------------------------------------------------------
      call map_planet_field(Time_Simulation, Xyz_D, TypeCoordSystem//' NORM', &
           rIonosphere, x_D, iHemisphere)

      if(iHemisphere==0)then
         write(*,*)'iHemisphere==0 for Xyz_D=',Xyz_D
         write(*,*)'iBlock, iRay=',iBlock,iRay
         call stop_mpi('ERROR in follow_fast_iono')
      end if

      if(iHemisphere*DipoleStrengthSi*sign(1.0,1.5-iRay) < 0.0)then
         Xyz_D = x_D
         follow_fast_iono = .true.
      else
         follow_fast_iono = .false.
      end if

    end function follow_fast_iono
    !==========================================================================
    subroutine assign_ray(IsSurfacePoint,Trace_D)
      !! acc routine seq

      ! Assign value to Trace_D(3) based on ray intersection
      ! given by the global variables iFace and position x(3)
      !
      ! iRay is 1 if ray points in positive B direction and 2 otherwise
      !
      ! IsSurfacePoint is true if the ray was started from the block face
      ! and false if it was started from a cell center

      logical, intent(in) :: IsSurfacePoint
      ! Called with a segment of Trace_DINB array and it is used here to get Trace_D
      real, intent(inout) :: Trace_D(3)

      real :: TraceTmp_D(3)

      ! Local variables

      ! Distances between x and the 4 grid points used for interpolation
      real :: d1,e1,d2,e2
      !------------------------------------------------------------------------
      if(DoTestRay)write(*,*)&
           'assign_ray starting with IsSurfacePoint, iRay, iFace=',&
           IsSurfacePoint,iRay,iFace

      select case(iFace)
      case(ray_out_)
         ! The ray points outward
         Trace_D=OUTRAY
         if(DoTestRay)write(*,*)'assign_ray finished with Trace_D=OUTRAY'
         RETURN
      case(ray_loop_)
         ! The ray did not hit the wall of the block
         Trace_D=LOOPRAY
         if(DoTestRay)write(*,*)'assign_ray finished with Trace_D=LOOPRAY'
         RETURN
      case(ray_body_)
         ! The ray hit a body
         Trace_D=BODYRAY
         if(DoTestRay)write(*,*)'assign_ray finished with Trace_D=BODYRAY'
         RETURN
      case(ray_iono_)
         ! The ray hit the ionosphere
         Trace_D=x
         if(DoTestRay)write(*,*)&
              'assign_ray finished with Trace_D on ionosphere, Trace_D=',Trace_D
         RETURN
      case(1,2)
         if(iFace==1)then
            i1=1
         else
            i1=nI+1
         endif
         i2=i1
         j1=floor(x(2)-GenMin_D(2))+1; j2=j1+1
         k1=floor(x(3)-GenMin_D(3))+1; k2=k1+1
         d1=x(2)-j1+0.5
         e1=x(3)-k1+0.5

      case(3,4)
         if(iFace==3)then
            j1=1
         else
            j1=nJ+1
         endif
         j2=j1
         i1=floor(x(1)-GenMin_D(1))+1; i2=i1+1
         k1=floor(x(3)-GenMin_D(3))+1; k2=k1+1
         d1=x(1)-i1+0.5
         e1=x(3)-k1+0.5

      case(5,6)
         ! The ray hit the bot or top wall
         if(iFace==5)then
            k1=1
         else
            k1=nK+1
         endif
         k2=k1
         i1=floor(x(1)-GenMin_D(1))+1; i2=i1+1
         j1=floor(x(2)-GenMin_D(2))+1; j2=j1+1
         d1=x(1)-i1+0.5
         e1=x(2)-j1+0.5

      case default
         write(*,*)'Impossible value for iFace=',iFace,' at iX,iY,iZ,iBlock=',&
              iX,iY,iZ,iBlock
         call stop_mpi('assign_ray')
      end select

      ! Calculate bilinear interpolation weights
      d2 = 1 - d1; e2 = 1 - e1
      Weight_I(1) = d2*e2
      Weight_I(2) = d1*e2
      Weight_I(3) = d2*e1
      Weight_I(4) = d1*e1

      if(DoTestRay)write(*,*)'Weight_I=',Weight_I

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
            if(DoTestRay)write(*,*) &
                 'Excluded point: me,iBlock,iX,iY,iZ,Weight_I=',&
                 iProc, iBlock, iX, iY, iZ, Weight_I
         end if
      end if

      if(DoTestRay) write(*,*)'i1,j1,k1,i2,j2,k2,d1,e1=', &
           i1, j1, k1, i2, j2, k2, d1, e1

      call rayface_interpolate(Trace_DINB(:,iRay,i1:i2,j1:j2,k1:k2,iBlock),&
           Weight_I, 4, TraceTmp_D)

      Trace_D = TraceTmp_D

      if(DoTestRay)write(*,*)'assign_ray finished Trace_D=',Trace_D

    end subroutine assign_ray
    !==========================================================================
  end subroutine trace_grid_fast
  !============================================================================
  subroutine rayface_interpolate(qrayface,Weight_I,nValue,Trace_D)
    !! acc routine seq

    ! Collect weights for qrayface values that differ less than dTraceMax
    ! and interpolate the values corresponding to the largest Weight_I
    ! The result is returned in Trace_D.
    ! Note that Trace_D and qrayface may overlap, so their intent must be inout!

    integer, intent(in)    :: nValue
    real,    intent(inout) :: qrayface(3,nValue)
    real,    intent(in)    :: Weight_I(nValue)
    real,    intent(inout) :: Trace_D(3)

    ! Local variables

    ! Cumulated weights corresponding to various kinds of qrayface values
    real :: WeightTmp_I(4), WeightSum_I(4)
    real :: TraceFirst_DI(3,4), TraceSum_DI(3,4)

    ! Difference between qrayface values, maximum for interpolation
    real :: dTrace, dTraceMax, ValueMax

    ! Number and indices of (cummulated) qrayface values, max location
    integer :: n, i, j

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'rayface_interpolate'
    !--------------------------------------------------------------------------
    ! call test_start(NameSub, DoTest)

    if(.not.UsePreferredInterpolation .and. &
         maxval(Weight_I)-minval(Weight_I) > 0.0001)then
       WeightTmp_I(1:nValue)=Weight_I
    else
       ValueMax = maxval(qrayface(1,:), MASK = Weight_I > 0.0) ! 0.01)

       if(ValueMax < CLOSEDRAY)then
          !     if(ValueMax < OPENRAY-0.01)then
          Trace_D = ValueMax
          RETURN
       end if

       where(qrayface(1,:)>=OPENRAY-0.01)
          WeightTmp_I(1:nValue)=Weight_I
       elsewhere
          WeightTmp_I(1:nValue)=0.
       endwhere
    end if

    if(DoTestRay)then
       write(*,*) NameSub
       write(*,*)'qrayface(1,:)=',qrayface(1,:)
       write(*,*)'qrayface(2,:)=',qrayface(2,:)
       write(*,*)'qrayface(3,:)=',qrayface(3,:)
       write(*,*)'Weight_I       =',Weight_I
       write(*,*)'WeightTmp_I      =',WeightTmp_I
    end if

    ! Short cuts
    if(all(qrayface(1,:)==OPENRAY))then
       ! all surrounding rays are open
       Trace_D=OPENRAY
       if(DoTestRay)write(*,*) NameSub,' finished with fully OPENRAY'
       RETURN
    end if

    if(all(qrayface(1,:)==NORAY))then
       ! all surrounding rays are unknown
       Trace_D=NORAY
       if(DoTestRay)write(*,*) NameSub,' finished with fully NORAY'
       RETURN
    end if

    dTraceMax=0.2*rIonosphere
    n=0
    do j=1,nValue
       i=1
       do
          if(i>n)then
             ! New type of ray
             n=i
             TraceFirst_DI(:,i)=qrayface(:,j)
             WeightSum_I(i) =WeightTmp_I(j)
             if(TraceFirst_DI(1,i)>CLOSEDRAY)&
                  TraceSum_DI(:,i)=WeightTmp_I(j)*qrayface(:,j)
             EXIT
          end if

          ! Calculate difference between qrayface(:,j) and TraceFirst_DI(:,i)
          dTrace=sum(abs(qrayface(:,j)-TraceFirst_DI(:,i)))

          if(dTrace<dTraceMax)then
             ! Same type of ray, cummulate it

             WeightSum_I(i)=WeightSum_I(i)+WeightTmp_I(j)
             if(TraceFirst_DI(1,i)>CLOSEDRAY)&
                  TraceSum_DI(:,i)=TraceSum_DI(:,i)+WeightTmp_I(j)*qrayface(:,j)
             EXIT
          end if
          ! Try next type
          i=i+1

          if(i>nValue)call stop_mpi(NameSub//': Impossible value for i')
       end do ! i
    end do ! j

    if(n==1)then
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
       i = maxloc(WeightSum_I(1:n),DIM=1)
       if(TraceFirst_DI(1,i)>CLOSEDRAY)then
          ! take average
          Trace_D=TraceSum_DI(:,i)/WeightSum_I(i)
       else
          ! identical Trace_DINB values, no need to average
          Trace_D=TraceFirst_DI(:,i)
       end if
    end if

    if(DoTestRay)then
       write(*,*) NameSub,': WeightSum_I=',WeightSum_I(1:n)
       write(*,*) NameSub,' finished with Trace_D=',Trace_D
    end if

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

    use ModMain, ONLY : nBlock,Unused_B
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

    do iBlock=1,nBlock
       if(Unused_B(iBlock))CYCLE
       do iFace=1,6
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
    real    :: qrayface(3,2,nFaceMax,nFaceMax)
    integer :: IjkTrace_DII(2,nFaceMax,nFaceMax)

    ! Interpolation weights
    real, dimension(4), parameter:: Weight4=0.25
    real, dimension(2), parameter:: Weight2=0.5

    ! Extract qrayface and IjkTrace_DII for the appropriate face
    ! NOTE: IjkTrace_DII assignment split to two lines to avoid reshaping compiler bug!
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'prolong_ray_after_pass'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    select case(iFace)
    case(1)
       nFaceJ=nJ+1; nFaceK=nK+1
       qrayface( :,:,1:nFaceJ,1:nFaceK)=Trace_DINB(   :,:,1,1:nJ+1,1:nK+1,iBlock)
       IjkTrace_DII(1,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,1,1,1:nJ+1,1:nK+1,iBlock)
       IjkTrace_DII(2,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,2,1,1:nJ+1,1:nK+1,iBlock)
    case(2)
       nFaceJ=nJ+1; nFaceK=nK+1
       qrayface( :,:,1:nFaceJ,1:nFaceK)=Trace_DINB(   :,:,nI+1,1:nJ+1,1:nK+1,iBlock)
       IjkTrace_DII(1,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,1,nI+1,1:nJ+1,1:nK+1,iBlock)
       IjkTrace_DII(2,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,2,nI+1,1:nJ+1,1:nK+1,iBlock)
    case(3)
       nFaceJ=nI+1; nFaceK=nK+1
       qrayface( :,:,1:nFaceJ,1:nFaceK)=Trace_DINB(   :,:,1:nI+1,1,1:nK+1,iBlock)
       IjkTrace_DII(1,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,1,1:nI+1,1,1:nK+1,iBlock)
       IjkTrace_DII(2,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,2,1:nI+1,1,1:nK+1,iBlock)
    case(4)
       nFaceJ=nI+1; nFaceK=nK+1
       qrayface( :,:,1:nFaceJ,1:nFaceK)=Trace_DINB(   :,:,1:nI+1,nJ+1,1:nK+1,iBlock)
       IjkTrace_DII(1,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,1,1:nI+1,nJ+1,1:nK+1,iBlock)
       IjkTrace_DII(2,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,2,1:nI+1,nJ+1,1:nK+1,iBlock)
    case(5)
       nFaceJ=nI+1; nFaceK=nJ+1
       qrayface( :,:,1:nFaceJ,1:nFaceK)=Trace_DINB(   :,:,1:nI+1,1:nJ+1,1,iBlock)
       IjkTrace_DII(1,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,1,1:nI+1,1:nJ+1,1,iBlock)
       IjkTrace_DII(2,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,2,1:nI+1,1:nJ+1,1,iBlock)
    case(6)
       nFaceJ=nI+1; nFaceK=nJ+1
       qrayface( :,:,1:nFaceJ,1:nFaceK)=Trace_DINB(   :,:,1:nI+1,1:nJ+1,nK+1,iBlock)
       IjkTrace_DII(1,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,1,1:nI+1,1:nJ+1,nK+1,iBlock)
       IjkTrace_DII(2,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,2,1:nI+1,1:nJ+1,nK+1,iBlock)
    case default
       call stop_mpi('Impossible value for iFace in prolong_ray')
    end select

    do iRay=1,2
       do k=1,nfaceK
          if(mod(k,2)==1)then
             do j=2,nfaceJ,2
                ! Case b: even j and odd k

                if(IjkTrace_DII(iRay,j,k)/=ray_out_)CYCLE

                call rayface_interpolate(&
                     qrayface(:,iRay,j-1:j+1:2,k),Weight2,2,&
                     qrayface(:,iRay,j,k))
             end do
          else
             do j=1,nJ+1,2
                ! Case a: odd j and even k

                if(IjkTrace_DII(iRay,j,k)/=ray_out_)CYCLE

                call rayface_interpolate(&
                     qrayface(:,iRay,j,k-1:k+1:2),Weight2,2,&
                     qrayface(:,iRay,j,k))
             end do
             do j=2,nJ,2
                ! Case c: even j and even k

                if(IjkTrace_DII(iRay,j,k)/=ray_out_)CYCLE

                call rayface_interpolate(&
                     qrayface(:,iRay,j-1:j+1:2,k-1:k+1:2),Weight4,4,&
                     qrayface(:,iRay,j,k))
             end do ! j
          end if ! mod(k,2)
       end do ! k
    end do ! iRay

    ! Put back result into Trace_DINB
    select case(iFace)
    case(1)
       Trace_DINB(:,:,     1,1:nJ+1,1:nK+1,iBlock)=qrayface(:,:,1:nFaceJ,1:nFaceK)
    case(2)
       Trace_DINB(:,:,  nI+1,1:nJ+1,1:nK+1,iBlock)=qrayface(:,:,1:nFaceJ,1:nFaceK)
    case(3)
       Trace_DINB(:,:,1:nI+1,     1,1:nK+1,iBlock)=qrayface(:,:,1:nFaceJ,1:nFaceK)
    case(4)
       Trace_DINB(:,:,1:nI+1,  nJ+1,1:nK+1,iBlock)=qrayface(:,:,1:nFaceJ,1:nFaceK)
    case(5)
       Trace_DINB(:,:,1:nI+1,1:nJ+1,     1,iBlock)=qrayface(:,:,1:nFaceJ,1:nFaceK)
    case(6)
       Trace_DINB(:,:,1:nI+1,1:nJ+1,  nK+1,iBlock)=qrayface(:,:,1:nFaceJ,1:nFaceK)
    end select

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine prolong_ray_after_pass
  !============================================================================
  subroutine ray_pass_old

    ! Exchange and update Trace_DINB values between blocks direction by direction

    ! Notation: _o out        (cells to be sent for equal blocks)
    !           _g get        (cells to be received)
    !           _r restricted (to be sent to a coarser block)
    !           _s subface    (one quarter of a face)

    use ModMain, ONLY : nblockMax,okdebug,Unused_B,optimize_message_pass
    use BATL_lib, ONLY: iNode_B, iTree_IA, Coord0_
    use ModParallel, ONLY : NOBLK,neiLEV,neiBLK,neiPE
    use ModMpi

    ! Local variables

    ! iDir=1,2,3 correspond to east-west, south-north, bot-top.
    integer :: iDir, iSweep

    ! Face (east..top), side (1 for east,south,bot, 2 for others)
    integer :: iFace, otherface, iSide

    ! number of subfaces (1 or 4), subface (1..nSubFace) and child (1..8) index
    integer ::  nSubFace, iSubFace

    ! Array ranges for outgoing, incoming, restricted and subfaces
    integer :: imin_o,imax_o,jmin_o,jmax_o,kmin_o,kmax_o
    integer :: imin_g,imax_g,jmin_g,jmax_g,kmin_g,kmax_g
    integer :: imin_r,imax_r,jmin_r,jmax_r,kmin_r,kmax_r
    integer :: imin_s,imax_s,jmin_s,jmax_s,kmin_s,kmax_s

    ! Block index
    integer :: iBlock

    ! Descriptors for neighbor
    integer :: neiP, neiB, neiL

    ! MPI variables
    integer :: iTag, iRequest, nRecvRequest, iRecvRequest_I(MaxBlock*6)
    integer :: status(MPI_STATUS_SIZE, MaxBlock*6)

    ! Maximum size of the RESTRICTED Trace_DINB layer to be received
    ! for the 6 ray variables (3 coord*2 ray dir.)
    integer, parameter :: MaxSizeR = &
         6*max((nI/2+1)*(nJ/2+1),(nI/2+1)*(nK/2+1),(nJ/2+1)*(nK/2+1))

    ! Receive Buffer_IIBI to hold 4 incoming RESTRICTED Trace_DINB values
    ! for all blocks and for both sides
    real, dimension(MaxSizeR,4,MaxBlock,2) :: Buffer_IIBI

    ! Actual size of messages: full, restricted/sparse and actual face
    integer :: iSize, iSizeR, iSize1

    ! Equal and restricted values to be sent are stored in these buffers
    real, dimension(:,:,:,:,:), allocatable :: eq_buf, re_buf

    integer :: iError

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'ray_pass_old'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*)'ray_pass me=',iProc

    do iDir=1,3
       select case(optimize_message_pass)
       case('face')
          ! Send messages face by face
          call ray_pass_faces(2*iDir-1,2*iDir-1,.true.,.true.,.true.)
          call ray_pass_faces(2*iDir  ,2*iDir  ,.true.,.true.,.true.)
       case('min')
          ! Send messages face by face and kind by kind
          do iSweep=2*iDir-1,2*iDir
             ! Send equal
             call ray_pass_faces(iSweep,iSweep,.true.,.false.,.false.)
             ! Send restricted
             call ray_pass_faces(iSweep,iSweep,.false.,.true.,.false.)
             ! Send prolonged
             call ray_pass_faces(iSweep,iSweep,.false.,.false.,.true.)
          end do
       case default
          ! Send messages for both faces
          call ray_pass_faces(2*iDir-1,2*iDir,.true.,.true.,.true.)
       end select
    end do ! iDir

    if(DoTest)write(*,*)'ray_pass starting prolongation'

    !! acc parallel loop gang independent
    do iBlock=1,nBlockMax
       if(Unused_B(iBlock))CYCLE

       do iFace=1,6
          if(neiLEV(iFace,iBlock)==1)call prolong_ray
       end do
    end do

    if(DoTest)write(*,*)'ray_pass finished'

    call test_stop(NameSub, DoTest)
  contains
    !==========================================================================
    subroutine ray_pass_faces(&
         iFaceMin,iFaceMax,DoEqual,DoRestrict,DoProlong)

      integer, intent(in):: iFaceMin,iFaceMax
      logical, intent(in):: DoEqual,DoRestrict,DoProlong

      ! BATL related
      integer:: iNode, iDim, iSideFace
      !------------------------------------------------------------------------

      if(DoTest)write(*,*)&
           'ray_pass_faces:me,iFaceMin,iFaceMax,do_eq,do_re,do_pr=',&
           iProc,iFaceMin,iFaceMax,DoEqual,DoRestrict,DoProlong

      ! Debug
      if(okdebug)Buffer_IIBI  =0.00

      nRecvRequest = 0
      iRecvRequest_I = MPI_REQUEST_NULL

      do iFace=iFaceMin,iFaceMax

         ! Set index ranges for the face
         call setranges_ray

         if(okdebug.and.DoTest)then
            write(*,*)&
                 'setranges_ray for receive Done: me,iFace,iSize,iSizeR',&
                 iProc, iFace, iSize, iSizeR
            write(*,*)'_o=',imin_o,imax_o,jmin_o,jmax_o,kmin_o,kmax_o
            write(*,*)'_g=',imin_g,imax_g,jmin_g,jmax_g,kmin_g,kmax_g
            write(*,*)'_r=',imin_r,imax_r,jmin_r,jmax_r,kmin_r,kmax_r
         end if

         do iBlock = 1,nBlockMax
            if(Unused_B(iBlock))CYCLE
            ! Post non-blocking receive for opposite face of neighbor block
            neiL=neiLEV(otherface,iBlock)
            select case(neiL)
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
               write(*,*)'me,iBlock,otherface,neiL=',&
                    iProc,iBlock,otherface,neiL
               call stop_mpi(&
                    'Error in message pass: Invalid value for neiLEV')
            end select

            if(okdebug.and.DoTest)write(*,*)&
                 'receive: me,neiL,nSubFace,iSize1',&
                 iProc,neiL,nSubFace,iSize1

            do iSubFace=1,nSubFace
               neiP=neiPE(iSubFace,otherface,iBlock)
               if(neiP/=iProc)then
                  ! Remote receive
                  iTag = 100*iBlock+10*iFace+iSubFace
                  if(DoTest.and.okdebug)write(*,*)&
                       'Remote receive, me,iTag,neiL,neiP=',&
                       iProc,iTag,neiL,neiP

                  call MPI_irecv(Buffer_IIBI(1,iSubFace,iBlock,iSide),&
                       iSize1,MPI_REAL,neiP,iTag,iComm,iRequest,iError)
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
         call setranges_ray

         if(okdebug.and.DoTest)write(*,*)&
              'setranges_ray for send Done: me, iFace=',iProc, iFace

         if(DoEqual)&
              allocate(eq_buf(3,2,imin_o:imax_o,jmin_o:jmax_o,kmin_o:kmax_o))
         if(DoRestrict.or.DoProlong)&
              allocate(re_buf(3,2,imin_r:imax_r,jmin_r:jmax_r,kmin_r:kmax_r))

         if(okdebug.and.DoTest)write(*,*)'allocation Done, me,iFace=',&
              iProc,iFace

         do iBlock=1,nBlockMax
            if(Unused_B(iBlock))CYCLE
            neiL=neiLEV(iFace,iBlock)

            if(okdebug.and.DoTest)write(*,*)&
                 'sending: me, iFace,iBlock,neiL=',iProc,iFace,iBlock,neiL
            select case(neiL)
            case(0)
               if(.not.DoEqual)CYCLE

               neiP=neiPE(1,iFace,iBlock)
               neiB=neiBLK(1,iFace,iBlock)
               if(neiP==iProc)then
                  ! Local copy
                  if(okdebug.and.DoTest)write(*,*)&
                       'local equal copy: me,iFace,iBlock=',iProc,iFace,iBlock

                  ! Debug
                  ! write(*,*)'Trace_DINB(_o,iBlock)=',&
                  !  Trace_DINB(:,:,imin_o:imax_o,jmin_o:jmax_o,kmin_o:kmax_o,iBlock)
                  ! write(*,*)'before: Trace_DINB(_g,neiB)=',&
                  !  Trace_DINB(:,:,imin_g:imax_g,jmin_g:jmax_g,kmin_g:kmax_g,neiB)

                  Trace_DINB(:,:,imin_g:imax_g,jmin_g:jmax_g,kmin_g:kmax_g,neiB)=&
                       max(&
                       Trace_DINB(:,:,imin_g:imax_g,jmin_g:jmax_g,kmin_g:kmax_g,neiB),&
                       Trace_DINB(:,:,imin_o:imax_o,jmin_o:jmax_o,kmin_o:kmax_o,iBlock))

                  ! Debug
                  ! write(*,*)'after: Trace_DINB(_g,neiB)=',&
                  !  Trace_DINB(:,:,imin_g:imax_g,jmin_g:jmax_g,kmin_g:kmax_g,neiB)

               else
                  ! Remote send
                  iTag = 100*neiB+10*iFace+1
                  if(DoTest.and.okdebug)write(*,*)&
                       'Remote equal send, me,iTag,neiP=',iProc,iTag,neiP

                  eq_buf=&
                       Trace_DINB(:,:,imin_o:imax_o,jmin_o:jmax_o,kmin_o:kmax_o,iBlock)

                  call MPI_Rsend(eq_buf,&
                       iSize,MPI_REAL,neiP,iTag,iComm,iError)
               end if
            case(1)
               if(.not.DoRestrict)CYCLE

               ! Restrict Trace_DINB in _o range into _r
               re_buf=&
                    Trace_DINB(:,:,imin_o:imax_o:2,jmin_o:jmax_o:2,kmin_o:kmax_o:2,iBlock)

               neiP=neiPE(1,iFace,iBlock)
               neiB=neiBLK(1,iFace,iBlock)
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

               if(neiP==iProc)then
                  ! Local copy into appropriate subface
                  call setsubrange_ray(.false.)
                  if(okdebug.and.DoTest)write(*,*)&
                       'local restricted copy: me,iFace,iBlock,_s=',&
                       iProc,iFace,iBlock,&
                       imin_s,imax_s,jmin_s,jmax_s,kmin_s,kmax_s

                  Trace_DINB(:,:,imin_s:imax_s,jmin_s:jmax_s,kmin_s:kmax_s,neiB)=&
                       max(re_buf,&
                       Trace_DINB(:,:,imin_s:imax_s,jmin_s:jmax_s,kmin_s:kmax_s,neiB))
               else
                  ! Remote send
                  iTag = 100*neiB+10*iFace+iSubFace
                  if(DoTest.and.okdebug)write(*,*)&
                       'Remote restricted send, me,iFace,iTag=',&
                       iProc,iFace,iTag
                  call MPI_Rsend(re_buf,iSizeR,&
                       MPI_REAL,neiP,iTag,iComm,iError)
               end if
            case(-1)
               if(.not.DoProlong)CYCLE

               do iSubFace=1,4
                  neiP=neiPE(iSubFace,iFace,iBlock)
                  neiB=neiBLK(iSubFace,iFace,iBlock)

                  call setsubrange_ray(.true.)

                  if(neiP==iProc)then
                     ! Local copy of appropriate subface

                     if(okdebug.and.DoTest)write(*,*)&
                          'local prolonged copy: me,iSubFace,iFace,iBlock,_s=',&
                          iProc,iSubFace,iFace,iBlock,&
                          imin_s,imax_s,jmin_s,jmax_s,kmin_s,kmax_s

                     Trace_DINB(:,:,imin_g:imax_g:2,&
                          jmin_g:jmax_g:2,&
                          kmin_g:kmax_g:2,neiB)=max(&
                          Trace_DINB(:,:,imin_g:imax_g:2,&
                          jmin_g:jmax_g:2,&
                          kmin_g:kmax_g:2,neiB),&
                          Trace_DINB(:,:,imin_s:imax_s,jmin_s:jmax_s,kmin_s:kmax_s,iBlock))
                  else
                     ! Remote send
                     re_buf=&
                          Trace_DINB(:,:,imin_s:imax_s,jmin_s:jmax_s,kmin_s:kmax_s,iBlock)

                     iTag = 100*neiB+10*iFace+1
                     if(DoTest.and.okdebug)write(*,*)&
                          'Remote prolong send, me,iFace,iTag=',&
                          iProc,iFace,iTag

                     call MPI_Rsend(re_buf,iSizeR,&
                          MPI_REAL,neiP,iTag,iComm,iError)
                  end if
               end do ! iSubFace

            case(NOBLK)
               ! There is no neighbor, do nothing
               CYCLE
            case default
               write(*,*)'me,iBlock,iFace,neiL=',&
                    iProc,iBlock,iFace,neiL
               call stop_mpi('Error in message pass: Invalid value for neiLEV')
            end select ! neiL
         end do ! iBlock

         if(DoEqual)deallocate(eq_buf)
         if(DoRestrict.or.DoProlong)deallocate(re_buf)

         if(DoTest)write(*,*)'messages sent, me, iFace=',iProc,iFace
      end do ! iFace

      ! WAIT FOR ALL MESSAGES TO BE RECEIVED
      if (nRecvRequest > 0) &
           call MPI_waitall(nRecvRequest,iRecvRequest_I,status,iError)

      if(DoTest)write(*,*)'messages received, me, iDir=',iProc, iDir

      ! Copy ghost cells received from non-local neigbors
      ! and stored in the Buffer_IIBI into sol_BLK

      do iFace=iFaceMin,iFaceMax

         ! Set index ranges for the face
         call setranges_ray

         if(okdebug.and.DoTest)write(*,*)&
              'setranges_ray for buf2ray Done: me, iFace=',iProc, iFace

         do iBlock = 1,nBlockMax
            if(Unused_B(iBlock))CYCLE
            select case(neiLEV(otherface,iBlock))
            case(0)
               if(okdebug.and.DoTest)&
                    write(*,*)'buf2rayface: me, iBlock=',iProc,iBlock
               if(DoEqual.and.neiPE(1,otherface,iBlock)/=iProc)&
                    call buf2rayface(Buffer_IIBI(1,1,iBlock,iSide),&
                    imin_g,imax_g,jmin_g,jmax_g,kmin_g,kmax_g)
            case(1)
               if(okdebug.and.DoTest)&
                    write(*,*)'buf2sparserayface: me, iBlock=',iProc,iBlock
               if(DoProlong.and.neiPE(1,otherface,iBlock)/=iProc)&
                    call buf2sparserayface(Buffer_IIBI(1,1,iBlock,iSide),&
                    imin_r,imax_r,jmin_r,jmax_r,kmin_r,kmax_r)
            case(-1)
               if(DoRestrict)then
                  do iSubFace=1,4
                     if(okdebug.and.DoTest)&
                          write(*,*)'buf2subrayface: me, iSubFace, iBlock=',&
                          iProc,iSubFace,iBlock
                     if(neiPE(iSubFace,otherface,iBlock)/=iProc)&
                          call buf2subrayface(&
                          Buffer_IIBI(1,iSubFace,iBlock,iSide),&
                          imin_r,imax_r,jmin_r,jmax_r,kmin_r,kmax_r)
                  end do
               end if
            end select ! neiL
         end do ! iBlock
      end do ! iFace

      if(DoTest)write(*,*)'ray_pass_faces finished: me, iFaceMin, iFaceMax=',&
           iProc,iFaceMin,iFaceMax

    end subroutine ray_pass_faces
    !==========================================================================
    subroutine setranges_ray

      ! Set ranges orthogonal to iDir based on the value of iDir

      !------------------------------------------------------------------------
      if(iDir/=1)then
         imin_g=1;  imax_g=nI+1
         imin_o=1;  imax_o=nI+1
         imin_r=1;  imax_r=nI/2+1
      end if

      if(iDir/=2)then
         jmin_g=1;  jmax_g=nJ+1
         jmin_o=1;  jmax_o=nJ+1
         jmin_r=1;  jmax_r=nJ/2+1
      endif

      if(iDir/=3)then
         kmin_g=1;  kmax_g=nK+1
         kmin_o=1;  kmax_o=nK+1
         kmin_r=1;  kmax_r=nK/2+1
      end if

      ! Set ranges in direction of iDir based on the value of iFace

      select case(iFace)
      case(1)
         otherface=2; iSide=1
         imin_o=1;    imax_o=1
         imin_g=nI+1; imax_g=nI+1
         imin_r=1;    imax_r=1
      case(3)
         otherface=4; iSide=1
         jmin_o=1;    jmax_o=1
         jmin_g=nJ+1; jmax_g=nJ+1
         jmin_r=1;    jmax_r=1
      case(5)
         otherface=6; iSide=1
         kmin_o=1;    kmax_o=1
         kmin_g=nK+1; kmax_g=nK+1
         kmin_r=1;    kmax_r=1
      case(2)
         otherface=1; iSide=2
         imin_o=nI+1; imax_o=nI+1
         imin_g=1;    imax_g=1
         imin_r=1;    imax_r=1
      case(4)
         otherface=3; iSide=2
         jmin_o=nJ+1; jmax_o=nJ+1
         jmin_g=1;    jmax_g=1
         jmin_r=1;    jmax_r=1
      case(6)
         otherface=5; iSide=2
         kmin_o=nK+1; kmax_o=nK+1
         kmin_g=1;    kmax_g=1
         kmin_r=1;    kmax_r=1
      end select

      ! Size of full and restricted cell layers
      iSize  =6*(imax_g-imin_g+1)*(jmax_g-jmin_g+1)*(kmax_g-kmin_g+1)
      iSizeR=6*(imax_r-imin_r+1)*(jmax_r-jmin_r+1)*(kmax_r-kmin_r+1)

    end subroutine setranges_ray
    !==========================================================================
    subroutine setsubrange_ray(DoSend)

      logical, intent(in) :: DoSend

      ! Select appropriate quarter of ghost cell layer
      !------------------------------------------------------------------------
      select case(iFace)
      case(1,2)
         if(DoSend)then
            imin_s=imin_o; imax_s=imax_o
         else
            imin_s=imin_g; imax_s=imax_g
         end if

         select case(iSubFace)
            ! Beware, case(2) and case(3) are swapped
         case(1)
            jmin_s=jmin_r; jmax_s=jmax_r;
            kmin_s=kmin_r; kmax_s=kmax_r
         case(3)
            jmin_s=jmin_r+nJ/2; jmax_s=jmax_r+nJ/2;
            kmin_s=kmin_r; kmax_s=kmax_r
         case(2)
            jmin_s=jmin_r; jmax_s=jmax_r;
            kmin_s=kmin_r+nK/2; kmax_s=kmax_r+nK/2;
         case(4)
            jmin_s=jmin_r+nJ/2; jmax_s=jmax_r+nJ/2;
            kmin_s=kmin_r+nK/2; kmax_s=kmax_r+nK/2;
         end select
      case(3,4)
         if(DoSend)then
            jmin_s=jmin_o; jmax_s=jmax_o
         else
            jmin_s=jmin_g; jmax_s=jmax_g
         end if
         select case(iSubFace)
            ! Beware, case(2) and case(3) are swapped
         case(1)
            imin_s=imin_r;      imax_s=imax_r;
            kmin_s=kmin_r;      kmax_s=kmax_r
         case(3)
            imin_s=imin_r+nI/2; imax_s=imax_r+nI/2;
            kmin_s=kmin_r;      kmax_s=kmax_r
         case(2)
            imin_s=imin_r;      imax_s=imax_r;
            kmin_s=kmin_r+nK/2; kmax_s=kmax_r+nK/2;
         case(4)
            imin_s=imin_r+nI/2; imax_s=imax_r+nI/2;
            kmin_s=kmin_r+nK/2; kmax_s=kmax_r+nK/2;
         end select
      case(5,6)
         if(DoSend)then
            kmin_s=kmin_o; kmax_s=kmax_o
         else
            kmin_s=kmin_g; kmax_s=kmax_g
         end if
         select case(iSubFace)
            ! Beware, case(2) and case(3) are not swapped
         case(1)
            imin_s=imin_r;      imax_s=imax_r;
            jmin_s=jmin_r;      jmax_s=jmax_r
         case(2)
            imin_s=imin_r+nI/2; imax_s=imax_r+nI/2;
            jmin_s=jmin_r;      jmax_s=jmax_r
         case(3)
            imin_s=imin_r;      imax_s=imax_r;
            jmin_s=jmin_r+nJ/2; jmax_s=jmax_r+nJ/2;
         case(4)
            imin_s=imin_r+nI/2; imax_s=imax_r+nI/2;
            jmin_s=jmin_r+nJ/2; jmax_s=jmax_r+nJ/2;
         end select
      end select

    end subroutine setsubrange_ray
    !==========================================================================
    subroutine buf2rayface(&
         buf,iMin,iMax,jMin,jMax,kMin,kMax)

      integer, intent(in) :: iMin,iMax,jMin,jMax,kMin,kMax
      real, dimension(3,2,iMin:iMax,jMin:jMax,kMin:kMax),intent(inout) :: buf
      !------------------------------------------------------------------------

      ! Take maximum of Trace_DINB and buf (more positive values are more real)
      ! for the full face

      Trace_DINB(:,:,imin_g:imax_g,jmin_g:jmax_g,kmin_g:kmax_g,iBlock)=max(buf,&
           Trace_DINB(:,:,imin_g:imax_g,jmin_g:jmax_g,kmin_g:kmax_g,iBlock))

    end subroutine buf2rayface
    !==========================================================================
    subroutine buf2sparserayface(buf,iMin,iMax,jMin,jMax,kMin,kMax)

      integer, intent(in) :: iMin,iMax,jMin,jMax,kMin,kMax
      real, dimension(3,2,iMin:iMax,jMin:jMax,kMin:kMax),intent(inout) :: buf

      ! Take maximum of Trace_DINB and buf (more positive values are more real)
      ! for a factor of 2 coarser grid

      !------------------------------------------------------------------------
      Trace_DINB(:,:,imin_g:imax_g:2,jmin_g:jmax_g:2,kmin_g:kmax_g:2,iBlock)=max(buf,&
           Trace_DINB(:,:,imin_g:imax_g:2,jmin_g:jmax_g:2,kmin_g:kmax_g:2,iBlock))

    end subroutine buf2sparserayface
    !==========================================================================
    subroutine buf2subrayface(buf,iMin,iMax,jMin,jMax,kMin,kMax)

      integer, intent(in) :: iMin,iMax,jMin,jMax,kMin,kMax
      real, dimension(3,2,iMin:iMax,jMin:jMax,kMin:kMax),intent(inout) :: buf

      ! Set subface range to write into

      !------------------------------------------------------------------------
      call setsubrange_ray(.false.)

      ! Take maximum of Trace_DINB and buf (more positive values are more real)
      ! for the appropriate subface

      Trace_DINB(:,:,imin_s:imax_s,jmin_s:jmax_s,kmin_s:kmax_s,iBlock)=max(buf,&
           Trace_DINB(:,:,imin_s:imax_s,jmin_s:jmax_s,kmin_s:kmax_s,iBlock))

    end subroutine buf2subrayface
    !==========================================================================
    subroutine prolong_ray
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

      integer :: iRay
      integer :: j, k, nFaceJ, nFaceK
      integer, parameter :: nFaceMax=max(nI+1,nJ+1,nK+1)
      real    :: qrayface(3,2,nFaceMax,nFaceMax)
      integer :: IjkTrace_DII(2,nFaceMax,nFaceMax)

      ! Interpolation weights
      real, dimension(4), parameter:: Weight4=0.25
      real, dimension(2), parameter:: Weight2=0.5
      !------------------------------------------------------------------------
      if(DoTest)write(*,*)'Prolong_ray, me, iBlock, iFace=',iProc, iBlock, iFace

      ! Extract qrayface and IjkTrace_DII for the appropriate face
      ! NOTE: IjkTrace_DII assignment split to two lines to avoid reshaping compiler bug!
      select case(iFace)
      case(1)
         nFaceJ=nJ+1; nFaceK=nK+1
         qrayface( :,:,1:nFaceJ,1:nFaceK)=Trace_DINB(   :,:,1,1:nJ+1,1:nK+1,iBlock)
         IjkTrace_DII(1,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,1,1,1:nJ+1,1:nK+1,iBlock)
         IjkTrace_DII(2,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,2,1,1:nJ+1,1:nK+1,iBlock)
      case(2)
         nFaceJ=nJ+1; nFaceK=nK+1
         qrayface( :,:,1:nFaceJ,1:nFaceK)=Trace_DINB(   :,:,nI+1,1:nJ+1,1:nK+1,iBlock)
         IjkTrace_DII(1,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,1,nI+1,1:nJ+1,1:nK+1,iBlock)
         IjkTrace_DII(2,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,2,nI+1,1:nJ+1,1:nK+1,iBlock)
      case(3)
         nFaceJ=nI+1; nFaceK=nK+1
         qrayface( :,:,1:nFaceJ,1:nFaceK)=Trace_DINB(   :,:,1:nI+1,1,1:nK+1,iBlock)
         IjkTrace_DII(1,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,1,1:nI+1,1,1:nK+1,iBlock)
         IjkTrace_DII(2,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,2,1:nI+1,1,1:nK+1,iBlock)
      case(4)
         nFaceJ=nI+1; nFaceK=nK+1
         qrayface( :,:,1:nFaceJ,1:nFaceK)=Trace_DINB(   :,:,1:nI+1,nJ+1,1:nK+1,iBlock)
         IjkTrace_DII(1,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,1,1:nI+1,nJ+1,1:nK+1,iBlock)
         IjkTrace_DII(2,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,2,1:nI+1,nJ+1,1:nK+1,iBlock)
      case(5)
         nFaceJ=nI+1; nFaceK=nJ+1
         qrayface( :,:,1:nFaceJ,1:nFaceK)=Trace_DINB(   :,:,1:nI+1,1:nJ+1,1,iBlock)
         IjkTrace_DII(1,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,1,1:nI+1,1:nJ+1,1,iBlock)
         IjkTrace_DII(2,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,2,1:nI+1,1:nJ+1,1,iBlock)
      case(6)
         nFaceJ=nI+1; nFaceK=nJ+1
         qrayface( :,:,1:nFaceJ,1:nFaceK)=Trace_DINB(   :,:,1:nI+1,1:nJ+1,nK+1,iBlock)
         IjkTrace_DII(1,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,1,1:nI+1,1:nJ+1,nK+1,iBlock)
         IjkTrace_DII(2,1:nFaceJ,1:nFaceK)=IjkTrace_DINB(1,2,1:nI+1,1:nJ+1,nK+1,iBlock)
      case default
         call stop_mpi('Impossible value for iFace in prolong_ray')
      end select

      do iRay=1,2
         do k=1,nfaceK
            if(mod(k,2)==1)then
               do j=2,nfaceJ,2
                  ! Case b: even j and odd k

                  if(IjkTrace_DII(iRay,j,k)/=ray_out_)CYCLE

                  call rayface_interpolate(&
                       qrayface(:,iRay,j-1:j+1:2,k),Weight2,2,&
                       qrayface(:,iRay,j,k))
               end do
            else
               do j=1,nJ+1,2
                  ! Case a: odd j and even k

                  if(IjkTrace_DII(iRay,j,k)/=ray_out_)CYCLE

                  call rayface_interpolate(&
                       qrayface(:,iRay,j,k-1:k+1:2),Weight2,2,&
                       qrayface(:,iRay,j,k))
               end do
               do j=2,nJ,2
                  ! Case c: even j and even k

                  if(IjkTrace_DII(iRay,j,k)/=ray_out_)CYCLE

                  call rayface_interpolate(&
                       qrayface(:,iRay,j-1:j+1:2,k-1:k+1:2),Weight4,4,&
                       qrayface(:,iRay,j,k))
               end do ! j
            end if ! mod(k,2)
         end do ! k
      end do ! iRay

      ! Put back result into Trace_DINB
      select case(iFace)
      case(1)
         Trace_DINB(:,:,     1,1:nJ+1,1:nK+1,iBlock)=qrayface(:,:,1:nFaceJ,1:nFaceK)
      case(2)
         Trace_DINB(:,:,  nI+1,1:nJ+1,1:nK+1,iBlock)=qrayface(:,:,1:nFaceJ,1:nFaceK)
      case(3)
         Trace_DINB(:,:,1:nI+1,     1,1:nK+1,iBlock)=qrayface(:,:,1:nFaceJ,1:nFaceK)
      case(4)
         Trace_DINB(:,:,1:nI+1,  nJ+1,1:nK+1,iBlock)=qrayface(:,:,1:nFaceJ,1:nFaceK)
      case(5)
         Trace_DINB(:,:,1:nI+1,1:nJ+1,     1,iBlock)=qrayface(:,:,1:nFaceJ,1:nFaceK)
      case(6)
         Trace_DINB(:,:,1:nI+1,1:nJ+1,  nK+1,iBlock)=qrayface(:,:,1:nFaceJ,1:nFaceK)
      end select

    end subroutine prolong_ray
    !==========================================================================
  end subroutine ray_pass_old
  !============================================================================
end module ModFieldTraceFast
!==============================================================================
