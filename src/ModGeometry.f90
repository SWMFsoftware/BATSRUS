!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModGeometry

  use BATL_lib, ONLY: &
       test_start, test_stop,&
       iProc, Xyz_DGB, CellSize_DB, Used_GB
  use ModBatsrusUtility, ONLY: stop_mpi

  use ModSize
  use ModMain,   ONLY: UseBody2, ExtraBc_, SolidBc_
  use ModIO,     ONLY: iUnitOut, write_prefix

  implicit none
  SAVE

  ! Grid geometry description
  character(len=20):: TypeGeometry = 'cartesian'

  ! Cartesian box limits that may be cut out of a non-Cartesian grid
  real :: xMinBox, xMaxBox, yMinBox, yMaxBox, zMinBox, zMaxBox

  ! Total volume of used (true) cells
  real :: DomainVolume = -1.0

  ! Coordinates of 1,1,1 cell. Same as BATL_lib::CoordMin_DB + 0.5*CellSize_DB
  real, allocatable :: Coord111_DB(:,:)
  !$acc declare create(Coord111_DB)

  ! Obsolete variables. Same as BATL_lib::CoordMin_D and CoordMax_D
  real :: XyzMin_D(3), XyzMax_D(3)

  ! Coodinate limits in true radius and degrees (used in restart.H)
  real :: CoordDimMin_D(3) = 0.0, CoordDimMax_D(3) = 0.0
  real :: RadiusMin = -1.0, RadiusMax = -1.0

  ! Mirror symmetry in the 3 directions
  integer:: nMirror_D(3) = 1

  ! Smallest and largest cell sizes in the first dimension
  real:: CellSize1Min, CellSize1Max

  ! Smallest and largest cell sizes in either the first direction
  ! or the Phi direction in degrees if IsLogRadius or IsGenRadius is true
  real:: CellSizeMin, CellSizeMax

  ! Variables describing cells inside boundaries

  ! true when at least one cell in the block (including ghost cells)
  ! is not used
  logical, allocatable :: IsBody_B(:)
  !$acc declare create(IsBody_B)

  ! true when all cells in block (not including ghost cells) are true_cells
  logical, allocatable :: IsNoBody_B(:)
  !$acc declare create(IsNoBody_B)

  ! Number of true cells (collected for processor 0)
  integer :: nUsedCell = -1

  ! True for blocks next to the cell based boundaries
  logical, allocatable :: IsBoundary_B(:)
  !$acc declare create(IsBoundary_B)

  ! Radial distance from origin and second body
  real, allocatable :: r_GB(:,:,:,:)
  !$acc declare create(r_GB)
  real, allocatable :: rBody2_GB(:,:,:,:)

  ! Smallest value of r_GB and rBody2_GB within a block
  real, allocatable :: rMin_B(:)
  !$acc declare create(rMin_B)
  real, allocatable :: rMinBody2_B(:)

  ! Added for general r grid in spherical geometry!
  ! Main idea is to have a tabulated function that maps
  ! a general coordinate to log(r). This way, r resolution can
  ! be arbitrarily defined. Gen. coord is taken to be 0 to 1.0
  ! but will be linearly extrapolated outside of this domain
  real, allocatable :: LogRGen_I(:)
  character(len=100):: NameGridFile=''

  ! Jacobian matrix for general grid: Dgencoord/Dcartesian
  ! This can be set with call set_block_jacobian_cell(iBlock)
  real, public :: DgenDxyz_DDC(MaxDim,MaxDim,nI,nJ,nK)
  !$omp threadprivate( DgenDxyz_DDC )

contains
  !============================================================================
  subroutine init_mod_geometry

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_mod_geometry'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(allocated(Used_GB)) RETURN

    allocate(Used_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
    allocate(IsBody_B(MaxBlock))
    allocate(IsNoBody_B(MaxBlock))
    allocate(IsBoundary_B(MaxBlock))
    allocate(r_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
    allocate(rMin_B(MaxBlock))
    allocate(Coord111_DB(3,MaxBlock))

    if(UseBody2)then
       allocate(rBody2_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
       allocate(rMinBody2_B(MaxBlock))
    end if

    if(iProc==0)then
       call write_prefix
       write(iUnitOut,'(a)') 'init_mod_geometry allocated arrays'
    end if

    call test_stop(NameSub, DoTest)
  end subroutine init_mod_geometry
  !============================================================================
  subroutine clean_mod_geometry

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'clean_mod_geometry'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(allocated(Used_GB))    deallocate(Used_GB)
    if(allocated(IsBody_B))     deallocate(IsBody_B)
    if(allocated(IsNoBody_B))   deallocate(IsNoBody_B)
    if(allocated(IsBoundary_B)) deallocate(IsBoundary_B)
    if(allocated(r_GB))         deallocate(r_GB)
    if(allocated(rBody2_GB))    deallocate(rBody2_GB)
    if(allocated(rMin_B))       deallocate(rMin_B)
    if(allocated(rMinBody2_B))  deallocate(rMinBody2_B)
    if(allocated(LogRGen_I))    deallocate(LogRGen_I)
    if(allocated(Coord111_DB))  deallocate(Coord111_DB)

    if(iProc==0)then
       call write_prefix
       write(iUnitOut,'(a)') 'clean_mod_geometry deallocated arrays'
    end if

    call test_stop(NameSub, DoTest)
  end subroutine clean_mod_geometry
  !============================================================================
  subroutine read_gen_radial_grid(NameFile)

    use ModIoUnit, ONLY: UnitTmp_
    use ModUtilities, ONLY: open_file, close_file

    character(len=*), intent(in) :: NameFile

    integer :: i, iError, nGrid
    real :: LogR

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_gen_radial_grid'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    ! This function is for reading in the general grid function.
    ! For simplicity the files Must be in a fixed format that is read by this
    ! routine.

    ! The formate is a single column with number of points in the
    ! first row, and the folling rows the log(r) value from gen = 0 to 1

    call open_file(FILE=NameFile, STATUS='old')

    ! read in nGrid
    read(UnitTmp_,*,iostat=iError) nGrid

    if(iError /= 0) call stop_mpi(NameSub// &
         ' could not read nGrid from file ' // trim(NameFile))

    ! Allocate LogRGen_I
    if(allocated(LogRGen_I)) deallocate(LogRGen_I)
    allocate(LogRGen_I(nGrid))
    LogRGen_I = 0.0

    ! read in tabulated log(radius)
    do i=1, nGrid
       read(UnitTmp_, *, IOSTAT=iError) LogR
       if(iError /= 0) then
          write(*,*) NameSub,': ERROR at i=',i
          call stop_mpi(NameSub//' could not read logR from '//trim(NameFile))
       end if
       LogRGen_I(i) = LogR
    enddo

    call close_file

    call test_stop(NameSub, DoTest)
  end subroutine read_gen_radial_grid
  !============================================================================
  subroutine set_gen_radial_grid

    ! Set a single element array when generalized radial coordinate
    ! is not used, but still should be set.

    !--------------------------------------------------------------------------
    if(allocated(LogRGen_I)) deallocate(LogRGen_I)
    allocate(LogRGen_I(1))
    LogRGen_I = 0.0

  end subroutine set_gen_radial_grid
  !============================================================================

  subroutine set_block_jacobian_cell(iBlock)

    use BATL_lib, ONLY: nDim
    use ModNumConst, ONLY: i_DD
    use ModCoordTransform, ONLY: inverse_matrix

    integer, intent(in):: iBlock
    real:: InvDx1Half, InvDx2Half, InvDx3Half
    real:: DxyzDgen_DD(MaxDim, MaxDim)
    integer:: i,j,k
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_block_jacobian_cell'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! Calculate the dCartesian/dGencoord matrix

    InvDx1Half = 0.5/CellSize_DB(1,iBlock)
    InvDx2Half = 0.5/CellSize_DB(2,iBlock)
    InvDx3Half = 0.5/CellSize_DB(3,iBlock)

    ! Get the dCartesian/dGencoord matrix with finite differences
    do k=1,nK; do j=1,nJ; do i=1,nI
       DxyzDgen_DD(:,1) = InvDx1Half &
            *(Xyz_DGB(:,i+1,j,k,iBlock) - Xyz_DGB(:,i-1,j,k,iBlock))

       DxyzDgen_DD(:,2) = InvDx2Half &
            *(Xyz_DGB(:,i,j+1,k,iBlock) - Xyz_DGB(:,i,j-1,k,iBlock))

       if(nDim==3)then
          DxyzDgen_DD(:,3) = InvDx3Half &
               *(Xyz_DGB(:,i,j,k+1,iBlock) - Xyz_DGB(:,i,j,k-1,iBlock))
       else
          DxyzDgen_DD(:,3) = i_DD(:,3)
       end if

       DgenDxyz_DDC(:,:,i,j,k) = &
            inverse_matrix(DxyzDgen_DD, DoIgnoreSingular=.true.)
    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine set_block_jacobian_cell
  !============================================================================

  subroutine count_true_cells

    use BATL_lib, ONLY: nI, nJ, nK, nBlock, Unused_B, iComm
    use ModMpi

    integer :: iBlock, iError

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'count_true_cells'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    nUsedCell=0
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       nUsedCell = nUsedCell + count(Used_GB(1:nI,1:nJ,1:nK,iBlock))
    end do
    call MPI_reduce_integer_scalar(nUsedCell, MPI_SUM, 0, iComm, iError)

    call test_stop(NameSub, DoTest)
  end subroutine count_true_cells
  !============================================================================

end module ModGeometry
!==============================================================================
