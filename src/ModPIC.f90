!  Copyright (C) 2002 Regents of the University of Michigan
!  Portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModPIC

  use BATL_lib, ONLY: &
       test_start, test_stop, iProc, iComm

  use ModGridInfo, ONLY: iPicOn_, iPicOff_, get_point_status, set_point_status

  ! Variables and methods for coupling BATSRUS with a PIC code

  implicit none

  SAVE

  private ! except

  public:: pic_read_param
  public:: pic_init_region
  public:: pic_find_node
  public:: pic_find_region
  public:: pic_set_cell_status
  public:: mhd_to_pic_vec
  public:: is_inside_active_pic_region

  logical, public:: UsePic = .false.
  logical, public:: UseAdaptivePic = .false. 

  logical, public:: DoBalancePicBlock=.true.

  ! Local variables

  ! Coupling parameters
  integer, public :: nGhostPic   = 2  ! Number of ghost cells around PIC region

  ! Conversion to PIC units  
  ! If UseSamePicUnit is true, use the same units for all PIC regions. 
  logical, public:: UseSamePicUnit = .true. 
  real, public :: xUnitPicSi = 1, uUnitPicSi = 1, mUnitPicSi = 1
  real, public, allocatable  :: xUnitPicSi_I(:), uUnitPicSi_I(:), &
       mUnitPicSi_I(:), ScalingFactor_I(:)

  ! File sent by the PIC code

  ! PIC regions
  integer, public :: nRegionPic = 0


  ! R_DDI: mhd coordinates to pic coordinates.
  real, public, allocatable:: XyzMinPic_DI(:,:), XyzMaxPic_DI(:,:), &
       LenPic_DI(:,:), DxyzPic_DI(:,:), R_DDI(:,:,:)

  ! The patch size of each region
  integer, public, allocatable:: PatchSize_DI(:,:)

  ! Each patch uses 1 bit to record its status, on or off. The second
  ! index is the region number. 
  integer, public, allocatable:: Status_I(:)
  integer, public::  nSizeStatus

  integer, public:: nCellPerPatch = 2

  ! The PIC regon can rotate around x, y and z axis. The rotation rule is the
  ! same as the rotation for #REGION command.
  ! In the rotated coordinates, XyzMinPic_DI instead of
  ! XyzPic0_DI is the origin point.
  real, public, allocatable:: XyzPic0_DI(:,:)
  logical, public :: DoRotatePIC = .false.

  ! Is the node overlaped with PIC domain?
  logical, public,allocatable:: IsPicNode_A(:)

  logical :: IsPicRegionInitialized = .false.
contains
  !============================================================================
  subroutine pic_read_param(NameCommand)

    use ModReadParam, ONLY: read_var
    use BATL_lib,     ONLY: x_, y_, z_, nDim
    use ModNumConst,  ONLY: cDegToRad
    use ModCoordTransform, ONLY: rot_matrix_x, rot_matrix_y, rot_matrix_z, &
         rot_matrix

    character(len=*), intent(in):: NameCommand

    integer:: iRegion, nRegionPicTmp

    real :: xRotate, yRotate, zRotate

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'pic_read_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(NameCommand)
    case ("#PICGHOST")
       call read_var('nGhostPic',nGhostPic)
    case ("#PICADAPTIVE")
       call read_var('UseAdaptivePic', UseAdaptivePic)
       call read_var('PatchSize', nCellPerPatch)
    case("#PICUNIT")
       call read_var('xUnitPicSi', xUnitPicSi)
       call read_var('uUnitPicSi', uUnitPicSi)
    case("#PICREGIONUNIT")
       UseSamePicUnit = .false. 
       call read_var('nPicRegion', nRegionPicTmp)

       if(nRegionPic > 0 .and. nRegionPicTmp /= nRegionPic ) then
          if(iProc==0) call stop_mpi(NameSub// &
               ': the input nPicRegion conflicts with the existing nPicRegion')
       endif
       nRegionPic = nRegionPicTmp

       if(allocated(xUnitPicSi_I)) deallocate( &
            xUnitPicSi_I, uUnitPicSi_I, mUnitPicSi_I, ScalingFactor_I)
       allocate( &
            xUnitPicSi_I(nRegionPic),       &
            uUnitPicSi_I(nRegionPic),       &
            mUnitPicSi_I(nRegionPic),       &
            ScalingFactor_I(nRegionPic)     )

       do iRegion = 1, nRegionPic
          call read_var('xUnitPicSi', xUnitPicSi_I(iRegion))
          call read_var('uUnitPicSi', uUnitPicSi_I(iRegion))
          call read_var('ScalingFactor', ScalingFactor_I(iRegion))          
       enddo

    case("#PICBALANCE")
       call read_var('DoBalancePicBlock', DoBalancePicBlock)
    case("#PICREGION")
       call read_var('nPicRegion', nRegionPicTmp)
       if(nRegionPic > 0 .and. nRegionPicTmp /= nRegionPic ) then
          if(iProc==0) call stop_mpi(NameSub// &
               ': the input nPicRegion conflicts with the existing nPicRegion')
       endif
       nRegionPic = nRegionPicTmp

       UsePic = nRegionPic > 0
       if(allocated(XyzMinPic_DI)) deallocate( &
            XyzMinPic_DI, XyzMaxPic_DI, DxyzPic_DI,XyzPic0_DI, PatchSize_DI)
       allocate( &
            XyzMinPic_DI(nDim,nRegionPic), &
            XyzMaxPic_DI(nDim,nRegionPic), &
            XyzPic0_DI(nDim,nRegionPic), &
            LenPic_DI(nDim,nRegionPic), &
            R_DDI(3,3,nRegionPic),       &
            DxyzPic_DI(nDim,nRegionPic), &
            PatchSize_DI(3, nRegionPic))
       PatchSize_DI = 1
       do iRegion = 1, nRegionPic
          call              read_var('xMinPic', XyzMinPic_DI(x_,iRegion))
          call              read_var('xMaxPic', XyzMaxPic_DI(x_,iRegion))
          if(nDim > 1) call read_var('yMinPic', XyzMinPic_DI(y_,iRegion))
          if(nDim > 1) call read_var('yMaxPic', XyzMaxPic_DI(y_,iRegion))
          if(nDim > 2) call read_var('zMinPic', XyzMinPic_DI(z_,iRegion))
          if(nDim > 2) call read_var('zMaxPic', XyzMaxPic_DI(z_,iRegion))
          call              read_var('DxPic',   DxyzPic_DI(x_,iRegion))
          if(nDim > 1) call read_var('DyPic',   DxyzPic_DI(y_,iRegion))
          if(nDim > 2) call read_var('DzPic',   DxyzPic_DI(z_,iRegion))

          ! Origo point for the IPIC3D grid for this region
          XyzPic0_DI(1:nDim,iRegion) = XyzMinPic_DI(1:nDim,iRegion)

          LenPic_DI(1:nDim, iRegion) = XyzMaxPic_DI(1:nDim,iRegion) - &
               XyzMinPic_DI(1:nDim,iRegion)
          R_DDI(:,:,iRegion) = 0
          R_DDI(1,1,iRegion) = 1
          R_DDI(2,2,iRegion) = 1
          R_DDI(3,3,iRegion) = 1

       end do
    case("#PICREGIONROTATE")       
       DoRotatePIC = .true.

       call read_var('nPicRegion', nRegionPicTmp)
       if(nRegionPic > 0 .and. nRegionPicTmp /= nRegionPic ) then
          if(iProc==0) call stop_mpi(NameSub// &
               ': the input nPicRegion conflicts with the existing nPicRegion')
       endif
       nRegionPic = nRegionPicTmp

       UsePic = nRegionPic > 0
       if(allocated(XyzMinPic_DI)) deallocate( &
            XyzMinPic_DI, XyzMaxPic_DI, LenPic_DI, DxyzPic_DI,XyzPic0_DI)
       allocate( &
            XyzMinPic_DI(nDim,nRegionPic), &
            XyzMaxPic_DI(nDim,nRegionPic), &
            XyzPic0_DI(nDim,nRegionPic), &
            DxyzPic_DI(nDim,nRegionPic), &
            LenPic_DI(nDim,nRegionPic),  &
            R_DDI(3,3,nRegionPic))
       XyzMinPic_DI = 0
       XyzMaxPic_DI = 0
       XyzPic0_DI = 0
       DxyzPic_DI = 0
       LenPic_DI = 0
       R_DDI=0
       do iRegion = 1, nRegionPic
          call              read_var('xMinPic', XyzMinPic_DI(x_,iRegion))
          call              read_var('xLenPic', LenPic_DI(x_,iRegion))
          if(nDim > 1) call read_var('yMinPic', XyzMinPic_DI(y_,iRegion))
          if(nDim > 1) call read_var('yLenPic', LenPic_DI(y_,iRegion))
          if(nDim > 2) call read_var('zMinPic', XyzMinPic_DI(z_,iRegion))
          if(nDim > 2) call read_var('zLenPic', LenPic_DI(z_,iRegion))
          call              read_var('DxPic',   DxyzPic_DI(x_,iRegion))
          if(nDim > 1) call read_var('DyPic',   DxyzPic_DI(y_,iRegion))
          if(nDim > 2) call read_var('DzPic',   DxyzPic_DI(z_,iRegion))
          if(nDim==2 ) then
             xRotate = 0; yRotate = 0
             call read_var('zRotate', zRotate)
             R_DDI(1:2,1:2,iRegion) = rot_matrix(-zRotate*cDegToRad)
          elseif(nDim==3) then
             call              read_var('xRotate', xRotate)
             call              read_var('yRotate', yRotate)
             call              read_var('zRotate', zRotate)
          endif

          XyzPic0_DI(1:nDim,iRegion) = XyzMinPic_DI(1:nDim,iRegion)

          ! Rotation matrix rotates around X, Y and Z axes in this order
          R_DDI(:,:,iRegion) = matmul( matmul( &
               rot_matrix_z(-zRotate*cDegToRad),&
               rot_matrix_y(-yRotate*cDegToRad)),&
               rot_matrix_x(-xRotate*cDegToRad))
       end do
    case default
       if(iProc==0) call stop_mpi(NameSub//': unknown command='//NameCommand)
    end select

    call test_stop(NameSub, DoTest)
  end subroutine pic_read_param
  !============================================================================

  subroutine pic_init_region

    use BATL_lib,     ONLY: nDim, find_grid_block, MaxDim, x_, y_, z_
    use ModPhysics,   ONLY: No2Si_V, UnitMass_, UnitCharge_
    use ModHallResist, ONLY: HallFactorMax, UseHallResist, HallFactor_C, set_hall_factor_cell
    use ModPhysics,   ONLY: IonMassPerCharge
    use ModMultiFluid,ONLY: nIonFLuid, MassIon_I
    use ModVarIndexes,ONLY: IsMhd
    use ModMpi,       ONLY: MPI_REAL

    ! PIC grid indexes
    integer:: iRegion

    ! MHD grid indexes
    integer:: i

    integer:: nCell

    ! mass per charge SI
    real:: IonMassPerChargeSi

    integer :: iProcPic, iBlockPic, iCell_D(MaxDim), iError
    real:: PicMiddle_D(MaxDim)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'pic_init_region'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(nIonFluid > 1 .and. IsMhd) then
       if(iProc == 0) then
          write(*,*) ' '
          write(*,*) "Error!!!!!"
          write(*,*) 'Multi-fluid MHD with total density and total ',&
               'momentum variables is used for  the current simulation ', &
               '(see ModEquation.f90). ', &
               'MHD-EPIC does not support this case now. Please eliminate ', &
               'the total density and total momentum variables and try again.'
          write(*,*) ' '
       endif
       call stop_mpi(NameSub)
    endif

    ! Normalizing the system so q/(mc) == 1 in IPIC3D.
    !
    ! In CGS units the Hall speed is uH_CGS = j/(nq) = c/4pi curlB m/(q rho)
    !
    ! In SI  units the Hall speed is uH_SI  = j/nq = curlB/mu0 m/(q rho)
    !
    ! The CGS dimension of curlB/(uH rho) is
    !   1/[L] * [U]sqrt[RHO]/([U][RHO]) = 1/([L] sqrt[RHO]) = sqrt[L]/sqrt[M]
    ! which can be used to scale the PIC units to true CGS units.
    !
    ! CGS->SI: 1kg=1000g, 1m=100cm, 1T=10000G and mu0 = 4pi*1e-7
    !
    ! 1  = q_PIC/(m_PIC c_pic)
    !    = curlB_PIC /  (4pi uH_PIC rho_PIC)
    !    = curlB_CGS /  (4pi uH_CGS rho_CGS) * sqrt( [M]_CGS / [L]_CGS )
    !    = curlB_SI*100/(4pi uH_SI*100 rho_SI*0.001)
    !                                        * sqrt{ [M]_SI*1000 / ([L]_SI*100)
    !    = 10^3.5/4pi * curlB_SI/(uH_SI rho_SI) * sqrt( [M]_SI / [L]_SI )
    !    = 10^3.5/4pi * mu0_SI*q_SI/m_SI        * sqrt( [M]_SI / [L]_SI )
    !    = 10^(-3.5)  *        q_SI/m_SI        * sqrt( [M]_SI / [L]_SI )
    !
    ! Then we can solve for the mass unit
    !
    !   [M]_SI = 10^7 * [L]_SI * (m_SI/q_SI)^2

    ! iPIC3D does not suppor multi-sessions now. If the pic region related
    ! parameters has been initialized, then skip this subroutine in
    ! the following sessions.
    if(IsPicRegionInitialized) RETURN
    IsPicRegionInitialized = .true.

    if(UseSamePicUnit) then
       if(allocated(xUnitPicSi_I)) deallocate( &
            xUnitPicSi_I, uUnitPicSi_I, mUnitPicSi_I, ScalingFactor_I)
       allocate( &
            xUnitPicSi_I(nRegionPic),       &
            uUnitPicSi_I(nRegionPic),       &
            mUnitPicSi_I(nRegionPic),       &
            ScalingFactor_I(nRegionPic)     )
       xUnitPicSi_I = xUnitPicSi
       uUnitPicSi_I = uUnitPicSi
       ScalingFactor_I = HallFactorMax
    endif

    IonMassPerChargeSi = IonMassPerCharge* &
         No2Si_V(UnitMass_)/No2Si_V(UnitCharge_)
    if(nIonFluid == 1) IonMassPerChargeSi = IonMassPerChargeSi/MassIon_I(1)

    do iRegion = 1, nRegionPic      
       do i=1, nDim
          ! Fix the PIC domain range.
          nCell = nint(LenPic_DI(i,iRegion)/DxyzPic_DI(i,iRegion))
          LenPic_DI(i,iRegion) =  nCell*DxyzPic_DI(i,iRegion)
          if(UseAdaptivePic) PatchSize_DI(i,iRegion) = nCell/nCellPerPatch
       end do

       if(UseAdaptivePic) then
          if(iRegion==1) then
             ! Assume the first PIC region is the largest. A temporary solution.
             nSizeStatus = ceiling(real(product(PatchSize_DI(1:nDim,iRegion))) &
                  /(sizeof(nSizeStatus) * 8) )
             allocate(Status_I(nSizeStatus))
             Status_I = 0
          endif
       endif


       XyzMaxPic_DI(:,iRegion) = XyzMinPic_DI(:,iRegion) + LenPic_DI(:,iRegion)      

       if(UseHallResist) then
          ! If UseHallResist is true, find out the Hall factor in the middle
          ! of this PIC region, and use this Hall factor as the scaling factor.
          PicMiddle_D = 0
          PicMiddle_D(1:nDim) = XyzMinPic_DI(:,iRegion) + 0.5*LenPic_DI(:,iRegion)
          call find_grid_block(PicMiddle_D, iProcPic,iBlockPic, &
               iCellOut_D=iCell_D)

          if(iProcPic == iProc) then
             call set_hall_factor_cell(iBlockPic, .false.)
             ScalingFactor_I(iRegion) = HallFactor_C( & 
                  iCell_D(x_), iCell_D(y_), iCell_D(z_))
             ! If Hall is not used around this PIC region, then use 
             ! HallFactorMax as the scaling factor.
             if(ScalingFactor_I(iRegion) == 0) &
                  ScalingFactor_I(iRegion) = HallFactorMax
          endif
          call MPI_Bcast(ScalingFactor_I(iRegion),1,MPI_REAL, &
               iProcPic,iComm,iError)          
       endif

       mUnitPicSi = 1e7*xUnitPicSi_I(iRegion) * &
            (IonMassPerChargeSi*ScalingFactor_I(iRegion))**2
       mUnitPicSi_I(iRegion) = mUnitPicSi

       if(iProc==0)then
          write(*,*) NameSub,': iRegion            = ', iRegion
          write(*,*) NameSub,': IonMassPerChargeSi = ', IonMassPerChargeSi
          write(*,*) NameSub,': xUnitPicSi         = ', xUnitPicSi_I(iRegion)
          write(*,*) NameSub,': uUnitPicSi         = ', uUnitPicSi_I(iRegion)
          write(*,*) NameSub,': ScalingFactor      = ', ScalingFactor_I(iRegion)
          write(*,*) NameSub,': mUnitPicSi         = ', mUnitPicSi_I(iRegion)
       end if

    end do

    if(iProc == 0) then
       write(*,*) "Corrected IPIC3D  regions"
       do iRegion = 1, nRegionPic
          write(*,*) "  Region : ", iRegion
          write(*,*) "     Min Cordinate : ", &
               XyzMinPic_DI(1:nDim,iRegion)
          write(*,*) "     Max Cordinate : ", &
               XyzMaxPic_DI(1:nDim,iRegion)
       end do
    end if
    call test_stop(NameSub, DoTest)
  end subroutine pic_init_region
  !============================================================================

  subroutine pic_find_node()
    ! Find out the blocks that overlaped with PIC region(s).
    use BATL_lib,  ONLY: nDim, MaxDim, find_grid_block, &
         x_, y_, z_, MaxNode
    integer:: nIJK_D(1:MaxDim), ijk_D(1:MaxDim)
    real:: PIC_D(1:MaxDim), MHD_D(1:MaxDim)
    integer:: iRegion, iBlock, i, j, k, iProcFound, iNode

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'pic_find_node'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub,' is called'

    if(.not.allocated(IsPicNode_A)) allocate(IsPicNode_A(MaxNode))
    IsPicNode_A = .false.

    nIJK_D = 1; PIC_D = 0; MHD_D=0

    do iRegion = 1, nRegionPic
       nIJK_D(1:nDim) = int(&
            LenPic_DI(1:nDim,iRegion)/DxyzPic_DI(1:nDim,iRegion) + 0.5)

       if(DoTest) write(*,*) NameSub,' iRegion = ',iRegion, &
            ' nIJK_D = ',nIJK_D(1:nDim)

       do k=1, nIJK_D(z_); do j=1, nIJK_D(y_); do i=1, nIJK_D(x_)
          ! Loop through all the PIC node points.
          ijk_D(x_) = i - 1; ijk_D(y_) = j - 1; ijk_D(z_) = k - 1
          PIC_D(1:nDim) = ijk_D(1:nDim)*DxyzPic_DI(1:nDim,iRegion)
          call pic_to_mhd_vec(iRegion,PIC_D,MHD_D)
          call find_grid_block(MHD_D, iProcFound, iBlock, iNodeOut=iNode)
          IsPicNode_A(iNode) = .true.
       enddo; enddo; enddo

    enddo

    if(DoTest) write(*,*)'IsPicNode= ', IsPicNode_A(:)
    call test_stop(NameSub, DoTest)
  end subroutine pic_find_node
  !============================================================================

  subroutine pic_set_cell_status
    use BATL_lib, ONLY: x_, y_, z_
    use ModMain, ONLY: iteration_number

    integer:: iStatus
    integer::nx, ny, nz, i, j, k, iRegion
    real:: r
    !--------------------------------------------------------------------

    ! Global MPI reduction is needed for real simulations. Needs to be implemented. 

    Status_I = iPicOff_
    do iRegion = 1, nRegionPic
       nx = PatchSize_DI(x_, iRegion)
       ny = PatchSize_DI(y_, iRegion)
       nz = PatchSize_DI(z_, iRegion)
       do i = 0, nx-1; do j = 0, ny-1; do k = 0, nz-1

          !write(*,*)"iteration_number = ", iteration_number

          ! if(j-ny/2 < ny/4.0 + 2*ny/5.0*mod(iteration_number,10)/10.0 + (i - nx/2)*0.5 .and. &
          !j-ny/2 > -ny/4.0 - 2*ny/5.0*mod(iteration_number,10)/10.0 + (i - nx/2)*0.5 ) then

          !    call set_point_status(Status_I,nx, ny, nz, i, j, k, iPicOn_)
          ! endif

          !r = sqrt(real(j-ny/2 + 0.5)**2 + real(i-nx/2 + 0.5)**2)
          !if(.true.) then
          !if(r < nx/2) then
          !if(i >nx/2 + nx/4*mod(iteration_number,10)/10.0 .and. j > ny/2) then
          ! if((r < nx/4 + nx/4*mod(iteration_number,10)/10.0 .and. &
          !       r > nx/8 + nx/10*mod(iteration_number,10)/10.0) .or. &
          !       r < -nx/10) then
          ! j-ny/2 < ny/4.0 + 2*ny/5.0*mod(iteration_number,10)/10.0 + (i - nx/2)*0.5 .and. &
          !   j-ny/2 > -ny/4.0 - 2*ny/5.0*mod(iteration_number,10)/10.0 + (i - nx/2)*0.5 ) then

          call set_point_status(Status_I,nx, ny, nz, i, j, k, iPicOn_)
          !endif


          ! if(iteration_number <= 2  ) then
          !    !.and. (j<ny/2 .or. i > nx/2)
          !    call set_point_status(Status_I,nx, ny, nz, i, j, k, iPicOn_)
          !  else
          !     if(i>nx/2) then
          !        call set_point_status(Status_I,nx, ny, nz, i, j, k, iPicOn_)
          !     endif
          !  endif


       enddo; enddo; enddo
    enddo

    ! do iRegion = 1, nRegionPic
    !    nx = PatchSize_DI(x_, iRegion)
    !    ny = PatchSize_DI(y_, iRegion)
    !    nz = PatchSize_DI(z_, iRegion)
    !    do i = 0, nx-1; do j = 0, ny-1; do k = 0, nz-1
    !       call get_point_status(Status_I,nx, ny, nz, i, j, k, iStatus)
    !       write(*,*) "pic_set_cell_status: i, j, k, status = ", i,j,k,iStatus               
    !    enddo; enddo; enddo
    ! enddo

  end subroutine pic_set_cell_status

  !============================================================================
  subroutine is_inside_active_pic_region(xyz_D, IsInside)
    ! It should be a function instead of a subroutine. --Yuxi
    use BATL_lib, ONLY: nDim, x_, y_, z_

    real, intent(in) :: xyz_D(nDim)
    logical, intent(out):: IsInside
    real:: dshift_D(3)

    integer:: iRegion, iStatus, nx, ny, nz
    integer:: Index_D(3) = 0
    integer:: di, dj, dk
    !-----------------------------------------------
    iRegion = 1

    IsInside = .false. 

    if(any(xyz_D < XyzMinPic_DI(1:nDim, iRegion) + (nGhostPic - 0.1)*DxyzPic_DI(:,iRegion) )) return 
    if(any(xyz_D > XyzMaxPic_DI(1:nDim, iRegion) - (nGhostPic - 0.1)*DxyzPic_DI(:,iRegion) )) return

    nx = PatchSize_DI(x_, iRegion)
    ny = PatchSize_DI(y_, iRegion)
    nz = PatchSize_DI(z_, iRegion)       

    do di = -1, 1; do dj = -1, 1; do dk = -1, 1

       dshift_D(x_) = di
       dshift_D(y_) = dj
       dshift_D(z_) = dk 


       ! Patch cell index
       Index_D(1:nDim) = floor((xyz_D + dshift_D(1:nDim)*DxyzPic_DI(1:nDim, iRegion) - XyzMinPic_DI(1:nDim,iRegion))/ &
            (DxyzPic_DI(1:nDim,iRegion)*nCellPerPatch))

       call get_point_status(Status_I,nx, ny, nz, Index_D(x_), Index_D(y_), &
            Index_D(z_), iStatus)

       if(iStatus==iPicOff_) return   

    enddo; enddo; enddo
    IsInside = .true.
  end subroutine is_inside_active_pic_region
  
  !============================================================================
  integer function pic_find_region(iBlock,i,j,k)
    ! If a cell is inside the PIC region, return 1;
    ! otherwise, return 0;
    use BATL_lib, ONLY: nDim, Xyz_DGB

    integer, intent(in) :: iBlock,i,j,k

    integer:: iStatus
    integer:: iRegion
    real:: xyz_D(nDim), PIC_D(nDim)

    character(len=*), parameter:: NameSub = 'pic_find_region'
    !--------------------------------------------------------------------------
    xyz_D = Xyz_DGB(1:nDim,i,j,k,iBlock)

    iStatus=0
    do iRegion = 1, nRegionPic
       call mhd_to_pic_vec(iRegion, xyz_D, PIC_D)

       if(  all(PIC_D > 0 ).and.&
            all(PIC_D < LenPic_DI(:,iRegion))) & ! Not accurate here. --Yuxi
            iStatus = 1
    enddo

    pic_find_region=iStatus
  end function pic_find_region
  !============================================================================

  subroutine pic_to_mhd_vec(iRegion, CoordIn_D, CoordOut_D, OriginIn_D)
    ! Transfer Pic coordinates to Mhd coordinates. Origin_D
    ! is the origin of the PIC coordinates.

    use BATL_lib, ONLY: nDim

    integer, intent(in) :: iRegion
    real, intent(in)    :: CoordIn_D(nDim)
    real, intent(out)   :: CoordOut_D(nDim)
    real, intent(in), optional :: OriginIn_D(nDim)
    real :: Origin_D(nDim), coord_D(nDim), R_DD(3, 3)

    integer:: iDim, jDim

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'pic_to_mhd_vec'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub,' is called'

    Origin_D = XyzMinPic_DI(:,iRegion)
    if(present(OriginIn_D)) Origin_D = OriginIn_D

    Coord_D = 0

    ! R_pic2mhd = transpose(R_mhd2pic)
    coord_D = CoordIn_D
    do iDim = 1, nDim; do jDim = 1, nDim
       R_DD(iDim,jDim) = R_DDI(jDim,iDim,iRegion)
    enddo; enddo

    CoordOut_D = 0
    do iDim = 1, nDim
       CoordOut_D(iDim) = sum(R_DD(iDim,1:nDim)*coord_D)
    enddo

    CoordOut_D = CoordOut_D + Origin_D

    if(DoTest) then
       write(*,*) 'Origin_D   = ', Origin_D
       write(*,*) 'CoordIn_D  = ', CoordIn_D
       write(*,*) 'CoordOut_D = ', CoordOut_D
       do iDim = 1, nDim
          write(*,*) 'iDim = ', iDim, 'R = ', R_DD(iDim,:)
       enddo
    endif

    call test_stop(NameSub, DoTest)
  end subroutine pic_to_mhd_vec
  !============================================================================

  subroutine mhd_to_pic_vec(iRegion, CoordIn_D, CoordOut_D, OriginIn_D)
    ! DoMhd2Pic == true: transfer Mhd coordinates to a coordinates
    !   that is parallel to the PIC coordinates but the origin point is
    !   defined by Origin_D.

    use BATL_lib, ONLY: nDim

    integer, intent(in) :: iRegion
    real, intent(in)    :: CoordIn_D(nDim)
    real, intent(out)   :: CoordOut_D(nDim)
    real, intent(in), optional :: OriginIn_D(nDim)
    real :: Origin_D(nDim), coord_D(nDim), R_DD(3, 3)

    integer :: iDim

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'mhd_to_pic_vec'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub,' is called'

    Origin_D = XyzMinPic_DI(:,iRegion)
    if(present(OriginIn_D)) Origin_D = OriginIn_D

    Coord_D = CoordIn_D - Origin_D

    CoordOut_D = 0
    do iDim = 1, nDim
       CoordOut_D(iDim) = sum(R_DDI(iDim,1:nDim,iRegion)*coord_D)
    enddo

    if(DoTest) then
       write(*,*) 'Origin_D   = ', Origin_D
       write(*,*) 'CoordIn_D  = ', CoordIn_D
       write(*,*) 'CoordOut_D = ', CoordOut_D
       do iDim = 1, nDim
          write(*,*) 'iDim = ', iDim, 'R = ', R_DD(iDim,:)
       enddo
    endif

    call test_stop(NameSub, DoTest)
  end subroutine mhd_to_pic_vec
  !============================================================================

end module ModPIC
!==============================================================================
