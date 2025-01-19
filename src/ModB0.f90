!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModB0

  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest, iBlockTest
  use ModBatsrusUtility, ONLY: stop_mpi
#ifdef _OPENACC
  use ModUtilities, ONLY: norm2
#endif

  ! The magnetic field can be split into an analytic and numeric part.
  ! The analytic part is B0. It should satisfy div(B0) = 0,
  ! and usually curl(B0) = 0 except if UseCurlB0 = .true.
  ! Even if curl(B0) = 0 analytically, the numerical representation
  ! may have a finite curl.
  ! B0 field is often constant in time dB0/dt = 0, but not necessarily.
  ! This module provides data and methods for B0.

  use BATL_size, ONLY: MaxDim, nDim, MaxBlock, nI, nJ, nK, &
       MinI, MaxI, MinJ, MaxJ, MinK, MaxK
  use ModMain, ONLY: UseB, UseB0, UseConstrainB
  use ModAdvance, ONLY: iTypeUpdate, UpdateOrig_, State_VGB
  use ModVarIndexes, ONLY: Bx_, Bz_

  use omp_lib

  implicit none
  SAVE

  private ! except

  public:: UseB0            ! so one can use it from ModB0 too

  public:: init_mod_b0      ! initialize B0 module
  public:: clean_mod_b0     ! clean B0 module
  public:: read_b0_param    ! read UseB0, UseB0Source, UseCurlB0
  public:: set_b0_cell      ! set cell centered B0_DGB
  public:: set_b0_reschange ! make face centered B0 consistent at res.change
  public:: set_b0_face      ! set face centered B0_DX,Y,Z
  public:: set_b0_source    ! set DivB0 and CurlB0
  public:: get_b0           ! get B0 at an arbitrary point
  public:: get_b0_dipole    ! get B0 field for a dipole
  public:: add_b0           ! add B0 to B1 for given block
  public:: subtract_b0      ! subtract B0 from B0+B1 for given block

  ! If B0 varies with time it should be update at some frequency
  logical, public:: DoUpdateB0  = UseB
  real,    public:: DtUpdateB0  = 0.0001

  ! Use source terms related to finite div(B0) and curl(B0)
  logical, public:: UseB0Source = UseB

  ! The extra source teerms in the indiction equation help to clean div B
  ! The logical choses if one wants to clean div B1 or div (B1 + B0)
  logical, public:: UseDivFullBSource = .false.

  ! Use source terms related to finite curl(B0) for non-force-free B0 field
  ! If UseCurlB0 is true, then UseB0Source must be true!
  logical, public:: UseCurlB0 = .false.
  !$acc declare create(UseCurlB0)

  ! Force free B0 means that J0 x B0 = 0 is assumed
  logical, public:: UseForceFreeB0 = .false.
  !$acc declare create(UseForceFreeB0)

  ! maximum radius of the force free B0
  real, public:: rMaxForceFreeB0 = -1.0

  ! The momentum source term for non-current-free B0 field, curl B0 x B0
  ! may be alternatively calculated as div (B0 B0) - grad B0^2/2 -B0 div B0
  logical, public :: UseB0MomentumFlux = .false.

  ! Radius within which the B0 field is curl free (analytically)
  real, public:: rCurrentFreeB0 = -1.0
  !$acc declare create(rCurrentFreeB0)

  ! Cell-centered B0 field vector
  real, public, allocatable:: B0_DGB(:,:,:,:,:)
  !$acc declare create(B0_DGB)

  ! Face-centered B0 field arrays for one block
  real, public, allocatable:: B0_DX(:,:,:,:), B0_DY(:,:,:,:), B0_DZ(:,:,:,:)
  !$omp threadprivate( B0_DX, B0_DY, B0_DZ )

  ! The numerical curl, curl B0 x B0 and divergence of B0 for one block
  real, public, allocatable :: CurlB0_DC(:,:,:,:)
  real, public, allocatable :: B0MomentumSource_DC(:,:,:,:)
  real, public, allocatable :: DivB0_C(:,:,:)
  !$omp threadprivate( CurlB0_DC, DivB0_C, B0MomentumSource_DC )

  ! Local variables

  ! B0 field at the resolution change interfaces
  real, public, allocatable :: B0ResChangeX_DIIEB(:,:,:,:,:)
  real, public, allocatable :: B0ResChangeY_DIIEB(:,:,:,:,:)
  real, public, allocatable :: B0ResChangeZ_DIIEB(:,:,:,:,:)

contains
  !============================================================================
  subroutine read_b0_param(NameCommand)

    use ModReadParam, ONLY: read_var
    use ModPhysics,   ONLY: MonopoleStrengthSi

    character(len=*), intent(in):: NameCommand

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_b0_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    select case(NameCommand)
    case("#B0")
       call read_var('UseB0', UseB0)
       if(.not.UseB0)then
          UseB0Source = .false.
          UseCurlB0   = .false.
          DoUpdateB0  = .false.
          DtUpdateB0  = -1.0
          UseDivFullBSource = .false.
       end if
    case("#B0SOURCE")
       call read_var('UseB0Source', UseB0Source)
       if(UseB0Source)then
          call read_var('UseDivFullBSource', UseDivFullBSource)
       else
          UseDivFullBSource = .false.
       end if

    case("#CURLB0")
       call read_var('UseCurlB0', UseCurlB0)
       if(UseCurlB0)then
          call read_var('rCurrentFreeB0', rCurrentFreeB0)
          call read_var('UseB0MomentumFlux', UseB0MomentumFlux)
       end if

    case("#FORCEFREEB0")
       call read_var('UseForceFreeB0', UseForceFreeB0)
       if(UseForceFreeB0) then
          call read_var('rMaxForceFreeB0', rMaxForceFreeB0)
       endif

    case("#MONOPOLEB0")
       call read_var('MonopoleStrengthSi', MonopoleStrengthSi)

    case default
       call stop_mpi(NameSub//': unknown command='//NameCommand)
    end select

    if(UseCurlB0) UseB0Source = .true.

    call test_stop(NameSub, DoTest)
  end subroutine read_b0_param
  !============================================================================
  subroutine init_mod_b0

    use BATL_lib, ONLY: iComm, iProc
    use ModMagnetogram, ONLY: init_magnetogram_lookup_table, rMaxB0, iTableB0
    use ModGeometry,    ONLY: RadiusMax

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_mod_b0'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    !$omp parallel
    !$omp single
    if(.not.allocated(B0_DGB))then
       allocate(B0_DGB(MaxDim,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
       B0_DGB = 0.0
       if(iTypeUpdate == UpdateOrig_)then
          allocate( &
               B0ResChangeX_DIIEB(MaxDim,nJ,nK,1:2,MaxBlock),    &
               B0ResChangeY_DIIEB(MaxDim,nI,nK,3:4,MaxBlock),    &
               B0ResChangeZ_DIIEB(MaxDim,nI,nJ,5:6,MaxBlock))
          B0ResChangeX_DIIEB = 0.0
          B0ResChangeY_DIIEB = 0.0
          B0ResChangeZ_DIIEB = 0.0
       end if
    end if
    !$omp end single

    if( .not.allocated(B0_DX) ) then
       if(UseConstrainB)then
          ! The current implementation of CT requires fluxes
          ! between ghost cells. Not a good solution.
          allocate( &
               B0_DX(MaxDim,nI+1,0:nJ+1,0:nK+1), &
               B0_DY(MaxDim,0:nI+1,nJ+1,0:nK+1), &
               B0_DZ(MaxDim,0:nI+1,0:nJ+1,nK+1))
       else
          allocate( &
               B0_DX(MaxDim,nI+1,nJ,nK), &
               B0_DY(MaxDim,nI,nJ+1,nK), &
               B0_DZ(MaxDim,nI,nJ,nK+1))
       end if
    end if

    !$omp end parallel

    call init_magnetogram_lookup_table(iComm)

    if(iTableB0 > 0) then
       if(iProc==0)write(*,*)NameSub,&
            ' Input: UseCurlB0, rCurrentFreeB0, rSourceSurface = ',&
            UseCurlB0, rCurrentFreeB0, rMaxB0
       if(UseForceFreeB0) then
          ! J0 is not zero, only J0 x B0 = 0
          UseCurlB0 = .true.
          rCurrentFreeB0 = 1.0
          UseB0Source = .true.
          if(iProc==0)write(*,*)NameSub, &
               ' ForceFreeB0, so UseCurlB0=T, rCurrentFreeB0= 1, ', &
               ' rMaxForceFreeB0=', rMaxForceFreeB0
       else if(rMaxB0 < RadiusMax)then
          ! J0 is finite above rMaxB0
          UseCurlB0 = .true.
          rCurrentFreeB0 = rMaxB0
          UseB0Source = .true.
          if(iProc==0)write(*,*)NameSub,&
                  ' UseCurlB0 is switched ON, rCurrentFreeB0 = ',rCurrentFreeB0
       else if(UseCurlB0)then
          ! if rSourceSurface > SC boundary then UseCurlB0 is NOT required
          UseCurlB0 = .false.
          rCurrentFreeB0 = -1.0
          if(iProc==0)write(*,*)NameSub,&
               ' NOTE: UseCurlB0 is switched OFF as source surface = ', rMaxB0
       end if
    end if
    !$acc update device (UseCurlB0, rCurrentFreeB0)

    !$omp parallel

    if(UseB0Source .and. .not.allocated(DivB0_C)) &
         allocate(DivB0_C(nI,nJ,nK))

    if((UseCurlB0 .or. UseB0Source) .and. .not.allocated(CurlB0_DC)) &
         allocate(CurlB0_DC(3,nI,nJ,nK), B0MomentumSource_DC(3,nI,nJ,nK))

    !$omp end parallel

    call test_stop(NameSub, DoTest)

  end subroutine init_mod_b0
  !============================================================================
  subroutine clean_mod_b0
    !--------------------------------------------------------------------------
    if(allocated(B0_DGB)) deallocate(B0_DGB, &
         B0ResChangeX_DIIEB, B0ResChangeY_DIIEB, B0ResChangeZ_DIIEB)

    !$omp parallel
    if(allocated(DivB0_C))   deallocate(DivB0_C)
    if(allocated(CurlB0_DC)) deallocate(CurlB0_DC, B0MomentumSource_DC)
    if(allocated(B0_DX)) deallocate(B0_DX,B0_DY,B0_DZ)
    !$omp end parallel

  end subroutine clean_mod_b0
  !============================================================================
  subroutine set_b0_cell(iBlock)

    ! Calculate the cell centered B0 for block iBlock
    use ModConst, ONLY: cTiny
    use ModMain,  ONLY: UseFieldLineThreads, DoThreads_B
    use BATL_lib, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK, Xyz_DGB, &
         CoordMin_D, CoordMin_DB

    integer, intent(in) :: iBlock

    integer :: i, j, k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_b0_cell'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
       call get_b0(Xyz_DGB(:,i,j,k,iBlock), B0_DGB(:,i,j,k,iBlock))
    end do; end do; end do

    ! If use field line threads, then in the block with newly
    ! calculated B0 the threads may or may not need to be calculated
    ! depending on the block proximity to the Sun
    if(UseFieldLineThreads)DoThreads_B(iBlock) = &
         abs(CoordMin_D(1) - CoordMin_DB(1,iBlock)) < cTiny
    if(DoTest)write(*,*)'B0*Cell_BLK=',&
         B0_DGB(:,iTest,jTest,kTest,iBlockTest)

    call test_stop(NameSub, DoTest, iBlock)

  end subroutine set_b0_cell
  !============================================================================
  subroutine set_b0_face(iBlock)

    ! Calculate the face centered B0 for block iBlock

    use ModMain,  ONLY: UseFieldLineThreads
    use ModParallel, ONLY: DiLevel_EB, Unset_
    use BATL_lib,    ONLY: nDim, CoordMin_DB, CellSize_DB, coord_to_xyz

    integer,intent(in)::iBlock
    ! Coords and Xyz for
    real :: Coord_D(3), Xyz_D(3)
    ! Loop variables
    integer :: j, k
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_b0_face'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    if(.not.UseB0) RETURN

    ! Average cell centered B0_DGB to the face centered B0_DX,Y,Z arrays
    if(UseConstrainB)then
       B0_DX = 0.5*(B0_DGB(:,0:nI  ,0:nJ+1,0:nK+1,iBlock) &
            +       B0_DGB(:,1:nI+1,0:nJ+1,0:nK+1,iBlock))

       B0_DY = 0.5*(B0_DGB(:,0:nI+1,0:nJ  ,0:nK+1,iBlock) &
            +       B0_DGB(:,0:nI+1,1:nJ+1,0:nK+1,iBlock))

       B0_DZ = 0.5*(B0_DGB(:,0:nI+1,0:nJ+1,0:nK  ,iBlock) &
            +       B0_DGB(:,0:nI+1,0:nJ+1,1:nK+1,iBlock))

    else
       B0_DX = 0.5*(B0_DGB(:,0:nI  ,1:nJ,1:nK,iBlock) &
            +       B0_DGB(:,1:nI+1,1:nJ,1:nK,iBlock))

       if(nDim >= 2) &
            B0_DY = 0.5*(B0_DGB(:,1:nI,0:nJ  ,1:nK,iBlock) &
            +            B0_DGB(:,1:nI,1:nJ+1,1:nK,iBlock))

       if(nDim >= 3) &
            B0_DZ = 0.5*(B0_DGB(:,1:nI,1:nJ,0:nK  ,iBlock) &
            +         B0_DGB(:,1:nI,1:nJ,1:nK+1,iBlock))
    end if

    if(iTypeUpdate == UpdateOrig_)then
       ! Correct B0 at resolution changes
       if(DiLevel_EB(1,iBlock) == -1) &
            B0_DX(:,1   ,1:nJ,1:nK) = B0ResChangeX_DIIEB(:,:,:,1,iBlock)
       if(DiLevel_EB(2,iBlock) == -1) &
            B0_DX(:,nI+1,1:nJ,1:nK) = B0ResChangeX_DIIEB(:,:,:,2,iBlock)
       if(DiLevel_EB(3,iBlock) == -1) &
            B0_DY(:,1:nI,1   ,1:nK) = B0ResChangeY_DIIEB(:,:,:,3,iBlock)
       if(DiLevel_EB(4,iBlock) == -1) &
            B0_DY(:,1:nI,1+nJ,1:nK) = B0ResChangeY_DIIEB(:,:,:,4,iBlock)
       if(DiLevel_EB(5,iBlock) == -1) &
            B0_DZ(:,1:nI,1:nJ,1   ) = B0ResChangeZ_DIIEB(:,:,:,5,iBlock)
       if(DiLevel_EB(6,iBlock) == -1) &
            B0_DZ(:,1:nI,1:nJ,1+nK) = B0ResChangeZ_DIIEB(:,:,:,6,iBlock)
    end if
    if(DiLevel_EB(1,iBlock) == Unset_ .and. UseFieldLineThreads)then
       do k = 1, nK; do j = 1, nJ
          ! Face center
          Coord_D = CoordMin_DB(:,iBlock) +  [0.0, &
               (j - 0.50)*CellSize_DB(2,iBlock),    &
               (k - 0.50)*CellSize_DB(3,iBlock)]
          call coord_to_xyz(Coord_D, Xyz_D)
          call get_b0(Xyz_D, B0_DX(:,1 ,j,k))
       end do; end do
    end if
    call test_stop(NameSub, DoTest, iBlock)

  end subroutine set_b0_face
  !============================================================================
  subroutine set_b0_reschange

    ! Set face centered B0 at resolution changes. Use the face area weighted
    ! average of the fine side B0 for the coarse face. This works for
    ! non-Cartesian grids too, because all the fine and coarse face normal
    ! vectors are parallel with each other (see algorithm in BATL_grid).

    use BATL_lib, ONLY: nDim, nBlock, Unused_B, DiLevelNei_IIIB, &
         IsCartesian, CellFace_DFB, message_pass_face

    integer:: i, j, k, iBlock
    real:: Coef

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_b0_reschange'
    !--------------------------------------------------------------------------

    if(iTypeUpdate /= UpdateOrig_) RETURN ! don't do this for fast update

    ! There are no resolution changes in 1D
    if(nDim == 1) RETURN

    if(.not.UseB0) RETURN

    call test_start(NameSub, DoTest)

    ! For Cartesian grid take 1/8-th of the contributing fine B0 values.
    ! For non-Cartesian grid the averaged cell values are weighted by face area
    if(IsCartesian .and. nDim==2) Coef = 0.25
    if(IsCartesian .and. nDim==3) Coef = 0.125

    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       if(DiLevelNei_IIIB(-1,0,0,iBlock) == +1) then
          do k = 1, nK; do j = 1, nJ
             if(.not.IsCartesian) Coef = 0.5*CellFace_DFB(1,1,j,k,iBlock)
             B0ResChangeX_DIIEB(:,j,k,1,iBlock) =     &
                  Coef*( B0_DGB(:,0,j,k,iBlock) &
                  +      B0_DGB(:,1,j,k,iBlock) )
          end do; end do
       end if

       if(DiLevelNei_IIIB(+1,0,0,iBlock) == +1) then
          do k = 1, nK; do j = 1, nJ
             if(.not.IsCartesian) Coef = 0.5*CellFace_DFB(1,nI+1,j,k,iBlock)
             B0ResChangeX_DIIEB(:,j,k,2,iBlock) =    &
                  Coef*( B0_DGB(:,nI  ,j,k,iBlock) &
                  +      B0_DGB(:,nI+1,j,k,iBlock) )
          end do; end do
       end if

       if(DiLevelNei_IIIB(0,-1,0,iBlock) == +1) then
          do k = 1, nK; do i = 1, nI
             if(.not.IsCartesian) Coef = 0.5*CellFace_DFB(2,i,1,k,iBlock)
             B0ResChangeY_DIIEB(:,i,k,3,iBlock) = &
                  Coef*( B0_DGB(:,i,0,k,iBlock) &
                  +      B0_DGB(:,i,1,k,iBlock) )
          end do; end do
       end if

       if(DiLevelNei_IIIB(0,+1,0,iBlock) == +1) then
          do k = 1, nK; do i = 1, nI
             if(.not.IsCartesian) Coef = 0.5*CellFace_DFB(2,i,nJ+1,k,iBlock)
             B0ResChangeY_DIIEB(:,i,k,4,iBlock) =    &
                  Coef*( B0_DGB(:,i,nJ  ,k,iBlock) &
                  +      B0_DGB(:,i,nJ+1,k,iBlock) )
          end do; end do
       end if

       if(nDim < 3) CYCLE

       if(DiLevelNei_IIIB(0,0,-1,iBlock) == +1) then
          do j = 1, nJ; do i = 1, nI
             if(.not.IsCartesian) Coef = 0.5*CellFace_DFB(3,i,j,1,iBlock)
             B0ResChangeZ_DIIEB(:,i,j,5,iBlock) = &
                  Coef*( B0_DGB(:,i,j,0,iBlock) &
                  +      B0_DGB(:,i,j,1,iBlock) )
          end do; end do
       end if

       if(DiLevelNei_IIIB(0,0,+1,iBlock) == +1)  then
          do j = 1, nJ; do i = 1, nI
             if(.not.IsCartesian) Coef = 0.5*CellFace_DFB(3,i,j,nK+1,iBlock)
             B0ResChangeZ_DIIEB(:,i,j,6,iBlock) =    &
                  Coef*( B0_DGB(:,i,j,nK  ,iBlock) &
                  +      B0_DGB(:,i,j,nK+1,iBlock) )
          end do; end do
       end if
    end do

    call message_pass_face(                                       &
         3, B0ResChangeX_DIIEB, B0ResChangeY_DIIEB, B0ResChangeZ_DIIEB, &
         DoSubtractIn=.false.)

    if(.not. IsCartesian) then
       ! For non-Cartesian grid divide B0ResChange by the coarse face area
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          if(DiLevelNei_IIIB(-1,0,0,iBlock) == -1) then
             do k = 1, nK; do j = 1, nJ
                B0ResChangeX_DIIEB(:,j,k,1,iBlock) =    &
                     B0ResChangeX_DIIEB(:,j,k,1,iBlock) &
                     /max(1e-30, CellFace_DFB(1,1,j,k,iBlock))
             end do; end do
          end if

          if(DiLevelNei_IIIB(+1,0,0,iBlock) == -1) then
             do k = 1, nK; do j = 1, nJ
                B0ResChangeX_DIIEB(:,j,k,2,iBlock) =    &
                     B0ResChangeX_DIIEB(:,j,k,2,iBlock) &
                     /max(1e-30, CellFace_DFB(1,nI+1,j,k,iBlock))
             end do; end do
          end if

          if(DiLevelNei_IIIB(0,-1,0,iBlock) == -1) then
             do k = 1, nK; do i = 1, nI
                B0ResChangeY_DIIEB(:,i,k,3,iBlock) =    &
                     B0ResChangeY_DIIEB(:,i,k,3,iBlock) &
                     /max(1e-30, CellFace_DFB(2,i,1,k,iBlock))
             end do; end do
          end if

          if(DiLevelNei_IIIB(0,+1,0,iBlock) == -1) then
             do k = 1, nK; do i = 1, nI
                B0ResChangeY_DIIEB(:,i,k,4,iBlock) =    &
                     B0ResChangeY_DIIEB(:,i,k,4,iBlock) &
                     /max(1e-30, CellFace_DFB(2,i,nJ+1,k,iBlock))
             end do; end do
          end if

          if(nDim < 3) CYCLE

          if(DiLevelNei_IIIB(0,0,-1,iBlock) == -1) then
             do j = 1, nJ; do i = 1, nI
                B0ResChangeZ_DIIEB(:,i,j,5,iBlock) =    &
                     B0ResChangeZ_DIIEB(:,i,j,5,iBlock) &
                     /max(1e-30, CellFace_DFB(3,i,j,1,iBlock))
             end do; end do
          end if

          if(DiLevelNei_IIIB(0,0,+1,iBlock) == -1) then
             do j = 1, nJ; do i = 1, nI
                B0ResChangeZ_DIIEB(:,i,j,6,iBlock) =    &
                     B0ResChangeZ_DIIEB(:,i,j,6,iBlock) &
                     /max(1e-30, CellFace_DFB(3,i,j,nK+1,iBlock))
             end do; end do
          end if
       end do
    endif
    call test_stop(NameSub, DoTest)

  end subroutine set_b0_reschange
  !============================================================================
  subroutine set_b0_source(iBlock, DoSkipSetB0Face)

    ! Calculate div(B0) and curl(B0) for block iBlock

    use ModSize,  ONLY: x_, y_, z_
    use BATL_lib, ONLY: IsCartesian, IsRzGeometry, &
         CellSize_DB, CellFace_DFB, FaceNormal_DDFB, CellVolume_GB, Xyz_DGB
    use ModCoordTransform, ONLY: cross_product

    integer, intent(in):: iBlock
    logical, optional, intent(in) :: DoSkipSetB0Face

    integer:: i, j, k
    real:: DxInv, DyInv, DzInv

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_b0_source'
    !--------------------------------------------------------------------------
    if(.not.(UseB0 .and. UseB0Source)) RETURN
    call test_start(NameSub, DoTest, iBlock)

    ! set B0_DX, B0_DY, B0_DZ for this block.
    ! Skip, if calculated before
    if(.not.present(DoSkipSetB0Face))call set_b0_face(iBlock)

    if(IsCartesian)then
       if(nDim == 2)then
          DxInv = 1/CellSize_DB(x_,iBlock)
          DyInv = 1/CellSize_DB(y_,iBlock)

          k = 1
          do j = 1, nJ; do i = 1, nI
             DivB0_C(i,j,k)= &
                  DxInv*(B0_DX(x_,i+1,j,k) - B0_DX(x_,i,j,k)) + &
                  DyInv*(B0_DY(y_,i,j+1,k) - B0_DY(y_,i,j,k))

             CurlB0_DC(x_,i,j,k) = &
                  +DyInv*(B0_DY(z_,i,j+1,k) - B0_DY(z_,i,j,k))

             CurlB0_DC(y_,i,j,k) = &
                  -DxInv*(B0_DX(z_,i+1,j,k) - B0_DX(z_,i,j,k))

             CurlB0_DC(z_,i,j,k) = &
                  DxInv*(B0_DX(y_,i+1,j,k) - B0_DX(y_,i,j,k)) - &
                  DyInv*(B0_DY(x_,i,j+1,k) - B0_DY(x_,i,j,k))

          end do; end do
       else
          DxInv = 1/CellSize_DB(x_,iBlock)
          DyInv = 1/CellSize_DB(y_,iBlock)
          DzInv = 1/CellSize_DB(z_,iBlock)

          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             DivB0_C(i,j,k)= &
                  DxInv*(B0_DX(x_,i+1,j,k) - B0_DX(x_,i,j,k)) + &
                  DyInv*(B0_DY(y_,i,j+1,k) - B0_DY(y_,i,j,k)) + &
                  DzInv*(B0_DZ(z_,i,j,k+1) - B0_DZ(z_,i,j,k))

             CurlB0_DC(x_,i,j,k) = &
                  DyInv*(B0_DY(z_,i,j+1,k) - B0_DY(z_,i,j,k)) - &
                  DzInv*(B0_DZ(y_,i,j,k+1) - B0_DZ(y_,i,j,k))

             CurlB0_DC(y_,i,j,k) = &
                  DzInv*(B0_DZ(x_,i,j,k+1) - B0_DZ(x_,i,j,k)) - &
                  DxInv*(B0_DX(z_,i+1,j,k) - B0_DX(z_,i,j,k))

             CurlB0_DC(z_,i,j,k) = &
                  DxInv*(B0_DX(y_,i+1,j,k) - B0_DX(y_,i,j,k)) - &
                  DyInv*(B0_DY(x_,i,j+1,k) - B0_DY(x_,i,j,k))

          end do; end do; end do
       end if
    elseif(IsRzGeometry)then
       DxInv = 1/CellSize_DB(x_,iBlock)

       k = 1
       do j = 1, nJ; do i = 1, nI
          DivB0_C(i,j,k)= ( &
               + CellFace_DFB(x_,i+1,j,k,iBlock)*B0_DX(x_,i+1,j,k)   &
               - CellFace_DFB(x_,i  ,j,k,iBlock)*B0_DX(x_,i  ,j,k)   &
               + CellFace_DFB(y_,i,j+1,k,iBlock)*B0_DY(y_,i,j+1,k)   &
               - CellFace_DFB(y_,i,j  ,k,iBlock)*B0_DY(y_,i,j  ,k) ) &
               /CellVolume_GB(i,j,k,iBlock)

          CurlB0_DC(x_,i,j,k) = ( &
               + CellFace_DFB(y_,i,j+1,k,iBlock)*B0_DY(z_,i,j+1,k)   &
               - CellFace_DFB(y_,i,j  ,k,iBlock)*B0_DY(z_,i,j  ,k) ) &
               /CellVolume_GB(i,j,k,iBlock)

          CurlB0_DC(y_,i,j,k) = &
               -DxInv*(B0_DX(z_,i+1,j,k) - B0_DX(z_,i,j,k))

          CurlB0_DC(z_,i,j,k) = &
               DxInv*(B0_DX(y_,i+1,j,k) - B0_DX(y_,i,j,k)) - ( &
               + CellFace_DFB(y_,i,j+1,k,iBlock)*B0_DY(x_,i,j+1,k)   &
               - CellFace_DFB(y_,i,j  ,k,iBlock)*B0_DY(x_,i,j  ,k) ) &
               /CellVolume_GB(i,j,k,iBlock) &
               + B0_DGB(x_,i,j,k,iBlock)/Xyz_DGB(y_,i,j,k,iBlock)
       end do; end do
    else
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          DivB0_C(i,j,k) = &
               + sum(FaceNormal_DDFB(:,1,i+1,j,k,iBlock)*B0_DX(:,i+1,j,k)) &
               - sum(FaceNormal_DDFB(:,1,i  ,j,k,iBlock)*B0_DX(:,i  ,j,k)) &
               + sum(FaceNormal_DDFB(:,2,i,j+1,k,iBlock)*B0_DY(:,i,j+1,k)) &
               - sum(FaceNormal_DDFB(:,2,i,j  ,k,iBlock)*B0_DY(:,i,j  ,k))

          if(nDim == 3) DivB0_C(i,j,k) = DivB0_C(i,j,k) &
               + sum(FaceNormal_DDFB(:,3,i,j,k+1,iBlock)*B0_DZ(:,i,j,k+1)) &
               - sum(FaceNormal_DDFB(:,3,i,j,k  ,iBlock)*B0_DZ(:,i,j,k  ))

          DivB0_C(i,j,k) = DivB0_C(i,j,k)/CellVolume_GB(i,j,k,iBlock)
       end do; end do; end do

       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          CurlB0_DC(:,i,j,k) =                                          &
               + cross_product(                                         &
               FaceNormal_DDFB(:,1,i+1,j,k,iBlock), B0_DX(:,i+1,j,k))   &
               - cross_product(                                         &
               FaceNormal_DDFB(:,1,i  ,j,k,iBlock), B0_DX(:,i  ,j,k))   &
               + cross_product(                                         &
               FaceNormal_DDFB(:,2,i,j+1,k,iBlock), B0_DY(:,i,j+1,k))   &
               - cross_product(                                         &
               FaceNormal_DDFB(:,2,i,j  ,k,iBlock), B0_DY(:,i,j  ,k))

          if(nDim == 3) CurlB0_DC(:,i,j,k) = CurlB0_DC(:,i,j,k)         &
               + cross_product(                                         &
               FaceNormal_DDFB(:,3,i,j,k+1,iBlock), B0_DZ(:,i,j,k+1))   &
               - cross_product(                                         &
               FaceNormal_DDFB(:,3,i,j,k  ,iBlock), B0_DZ(:,i,j,k  ))
          CurlB0_DC(:,i,j,k) = CurlB0_DC(:,i,j,k)/CellVolume_GB(i,j,k,iBlock)
       end do; end do; end do
    endif
    if(UseB0MomentumFlux)then
       if(IsCartesian.or.IsRzGeometry)call stop_mpi(&
            'B0MomentumFlux is not implemented in RZ or Cartesian Geometry')
       ! Divergence of Maxwell tensor : div(B0 B0 - B0^2/2)  = J0 x B0
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          B0MomentumSource_DC(:,i,j,k) =                                &
               + B0_DX(:,i+1,j,k)*sum(B0_DX(:,i+1,j,k)*                 &
               FaceNormal_DDFB(:,1,i+1,j,k,iBlock))                     &
               - FaceNormal_DDFB(:,1,i+1,j,k,iBlock)*                   &
               sum(B0_DX(:,i+1,j,k)**2)*0.50                            &
               - B0_DX(:,i  ,j,k)*sum(B0_DX(:,i  ,j,k)*                 &
               FaceNormal_DDFB(:,1,i,j,k,iBlock))                       &
               + FaceNormal_DDFB(:,1,i,j,k,iBlock)*                     &
               sum(B0_DX(:,i  ,j,k)**2)*0.50                            &
               + B0_DY(:,i,j+1,k)*sum(B0_DY(:,i,j+1,k)*                 &
               FaceNormal_DDFB(:,2,i,j+1,k,iBlock) )                    &
               - FaceNormal_DDFB(:,2,i,j+1,k,iBlock)*                   &
               sum(B0_DY(:,i,j+1,k)**2)*0.50                            &
               - B0_DY(:,i,j  ,k)*sum(B0_DY(:,i,j  ,k)*                 &
               FaceNormal_DDFB(:,2,i,j  ,k,iBlock) )                    &
               + FaceNormal_DDFB(:,2,i,j  ,k,iBlock)*                   &
               sum(B0_DY(:,i,j  ,k)**2)*0.50

          if(nDim == 3) B0MomentumSource_DC(:,i,j,k) =                  &
               B0MomentumSource_DC(:,i,j,k)                             &
               + B0_DZ(:,i,j,k+1)*sum(B0_DZ(:,i,j,k+1)*                 &
               FaceNormal_DDFB(:,3,i,j,k+1,iBlock))                     &
               - FaceNormal_DDFB(:,3,i,j,k+1,iBlock)*                   &
               sum(B0_DZ(:,i,j,k+1)**2)*0.50                            &
               - B0_DZ(:,i,j,k  )*sum(B0_DZ(:,i,j,k  )*                 &
               FaceNormal_DDFB(:,3,i,j,k  ,iBlock) )                    &
               + FaceNormal_DDFB(:,3,i,j,k  ,iBlock)*                   &
               sum(B0_DZ(:,i,j,k  )**2)*0.50

          ! Remove contribution from finite div(B0)
          B0MomentumSource_DC(:,i,j,k) =                                &
               B0MomentumSource_DC(:,i,j,k)/CellVolume_GB(i,j,k,iBlock) &
               - B0_DGB(:,i,j,k,iBlock)*DivB0_C(i,j,k)
       end do; end do; end do
    else
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          B0MomentumSource_DC(:,i,j,k) = cross_product(CurlB0_DC(:,i,j,k), &
               B0_DGB(:,i,j,k,iBlock))
       end do; end do; end do
    end if
    call test_stop(NameSub, DoTest, iBlock)

  end subroutine set_b0_source
  !============================================================================
  subroutine get_b0(Xyz_D, B0_D)
    !$acc routine seq

    ! Get B0 at location Xyz_D

    use ModMain,           ONLY : tSimulation, NameThisComp, &
         TypeCoordSystem
    use ModPhysics,        ONLY: Si2No_V, UnitB_, DipoleStrengthSi, &
         MonopoleStrength
    use CON_planet_field,  ONLY: get_planet_field
    use ModMain,           ONLY: UseBody2, UseUserB0, IsTimeAccurate, StartTime
    use ModMagnetogram,    ONLY: iTableB0, iTableB0New, get_magnetogram_field
    use ModConst, ONLY: CarringtonSynodicPeriod, tStartCarringtonRotation

    real, intent(in) :: Xyz_D(3)
    real, intent(out):: B0_D(3)

    real:: r, CarringtonNumber

    character(len=*), parameter:: NameSub = 'get_b0'
    !--------------------------------------------------------------------------
    if(iTableB0 > 0)then
       if(iTableB0New > 0 .and. IsTimeAccurate)then
          ! Interpolate to the current time expressed as CarringtonNumber
          CarringtonNumber = &
               (StartTime + tSimulation - tStartCarringtonRotation) &
               /CarringtonSynodicPeriod
          call get_magnetogram_field(Xyz_D, B0_D, CarringtonNumber)
       else
          call get_magnetogram_field(Xyz_D, B0_D)
       end if

       ! Convert from Tesla to normalized units.
       B0_D = B0_D*Si2No_V(UnitB_)
#ifndef _OPENACC
    elseif(MonopoleStrength /= 0.0)then
       r = norm2(Xyz_D(1:nDim))
       B0_D = MonopoleStrength*Xyz_D/r**nDim
    elseif(DipoleStrengthSi==0.0)then
       B0_D = 0.0
    elseif(NameThisComp=='GM' .and. nDim == 3)then
       call get_planet_field(tSimulation, Xyz_D, TypeCoordSystem//' NORM',&
            B0_D)
       B0_D = B0_D*Si2No_V(UnitB_)
#endif
    else
       ! dipole field
       call get_b0_dipole(Xyz_D, B0_D)
    end if

#ifndef _OPENACC
    if(UseBody2)call add_b0_body2(Xyz_D, B0_D)

    if(UseUserB0)call user_get_b0(Xyz_D(1), Xyz_D(2), Xyz_D(3), B0_D)
#endif

  end subroutine get_b0
  !============================================================================
  subroutine get_b0_dipole(Xyz_D, B0_D, IsAligned)
    !$acc routine seq

    ! Provide magnetic field B0_D in normalized unit at location Xyz_D
    ! By default the coordinate system of BATSRUS is used.
    ! If IsAligned is present, then MAG/SMG coordinate system is assumed.

    use ModPhysics, ONLY: DipoleStrength => Bdp, Dipole_D, &
         CosThetaTilt, SinThetaTilt
    use BATL_lib, ONLY: x_, y_, z_

    real, intent(in) :: Xyz_D(3)
    real, intent(out):: B0_D(3)
    logical, optional, intent(in):: IsAligned

    real :: r2, r3Inv, Term1
    real :: x, y, Bx, By ! Using Dipole_D in 2D would be better

    character(len=*), parameter:: NameSub = 'get_b0_dipole'
    !--------------------------------------------------------------------------
    r2 = sum(Xyz_D(1:nDim)**2)

    ! Avoid calculating B0 near the origin
    if(r2 <= 1e-12)then
       B0_D = 0.0
    elseif(nDim == 2) then

       ! 2D magnetic dipole is implemented from F.R. Cardoso et.al.'s paper
       ! "2D MHD simulation of the magnetic dipole tilt and IMF influence
       ! on the magnetosphere"
       ! except that they define the dipole pointing down, while this one
       ! points upward.

       x = Xyz_D(x_)
       y = Xyz_D(y_)
       Term1 = DipoleStrength/r2**2

       ! Dipole aligned with the +Y axis
       Bx = Term1*2*x*y
       By = Term1*(y**2 - x**2)

       ! Rotate the field with -ThetaTilt around the Z axis
       B0_D(x_) =  CosThetaTilt*Bx + SinThetaTilt*By
       B0_D(y_) = -SinThetaTilt*Bx + CosThetaTilt*By
       B0_D(3) = 0.0
    else
       r3Inv = 1/(r2*sqrt(r2))

       ! Compute intrinsic magnetic field B0 as a 3D dipole
       if(present(IsAligned))then
          Term1       = DipoleStrength*Xyz_D(3)*3/r2
          B0_D(x_:y_) = Term1*Xyz_D(x_:y_)*r3Inv
          B0_D(z_)    = (Term1*Xyz_D(z_) - DipoleStrength)*r3Inv
       else
          Term1 = sum(Dipole_D*Xyz_D)*3/r2
          B0_D  = (Term1*Xyz_D - Dipole_D)*r3Inv
       end if

    end if

  end subroutine get_b0_dipole
  !============================================================================
  subroutine add_b0_body2(XyzIn_D, B0_D)

    ! If there is a second body that has a magnetic field, the contribution
    ! to the field from the second body is computed here.

    use ModPhysics, ONLY: BdpBody2_D, rBody2, xBody2, yBody2, zBody2
    use ModNumConst, ONLY: cTiny

    real, intent(in)   :: XyzIn_D(3)
    real, intent(inout):: B0_D(3)

    real :: Xyz_D(3),R0,rInv,r2Inv,r3Inv,Dp

    ! Determine normalized relative coordinates and radial distance from body 2
    character(len=*), parameter:: NameSub = 'add_b0_body2'
    !--------------------------------------------------------------------------
    Xyz_D = (XyzIn_D - [xBody2, yBody2, zBody2])/rBody2

    R0 = norm2(Xyz_D)

    ! Avoid calculating B0 inside a critical normalized radius = cTiny
    if(R0 <= cTiny) RETURN

    rInv  = 1/R0
    r2Inv = rInv**2
    r3Inv = rInv*r2Inv

    ! Add dipole field of the second body
    Dp = sum(BdpBody2_D*Xyz_D)*3*r2Inv

    B0_D = B0_D + (Dp*Xyz_D - BdpBody2_D)*r3Inv

  end subroutine add_b0_body2
  !============================================================================
  subroutine add_b0(iBlock)

    ! add B0 to B1 to obtain full B=B0+B1

    integer, intent(in) :: iBlock

    integer :: i, j, k
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'add_b0'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
       State_VGB(Bx_:Bz_,i,j,k,iBlock) = State_VGB(Bx_:Bz_,i,j,k,iBlock) &
            + B0_DGB(:,i,j,k,iBlock)
    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine add_b0
  !============================================================================
  subroutine subtract_b0(iBlock)

    ! subtract B0 from full B0+B1 to obtain B1

    integer, intent(in) :: iBlock

    integer :: i, j, k
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'subtract_b0'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
       State_VGB(Bx_:Bz_,i,j,k,iBlock) = State_VGB(Bx_:Bz_,i,j,k,iBlock) &
            - B0_DGB(:,i,j,k,iBlock)
    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine subtract_b0
  !============================================================================
end module ModB0
!==============================================================================
