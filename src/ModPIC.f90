!  Copyright (C) 2002 Regents of the University of Michigan
!  Portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModPIC

  use BATL_lib,     ONLY: &
       test_start, test_stop, iProc, iComm

  use ModGridInfo,  ONLY: iPicOn_, iPicOff_, &
       get_point_status, &
       set_point_status
  use ModFreq
  ! Variables and methods for coupling BATSRUS with a PIC code

  implicit none

  SAVE

  private ! except

  public:: pic_read_param
  public:: pic_init_region
  public:: pic_find_node
  public:: pic_find_region
  public:: pic_find_region_active
  public:: pic_find_region_criteria
  public:: pic_set_cell_status
  public:: mhd_to_pic_vec
  public:: is_inside_active_pic_region
  public:: calc_pic_criteria

  logical, public:: UsePic = .false.
  logical, public:: UseAdaptivePic = .false.

  ! The viriables for adaptive PIC criterias
  integer, public :: nCriteriaPic=0
  character (len=10), public, allocatable :: NameCriteriaPic_I(:)
  real, public, allocatable :: CriteriaMinPic_I(:), CriteriaMaxPic_I(:)
  integer, public:: nPatchExtend_D(3)=0

  ! Description of the region where the pic region is fixed there
  character(len=200):: StringPicRegion = 'none'
  character(len=200):: StringPicRegionLimit = 'none'

  type(FreqType), public :: &
       AdaptPic = FreqType(.true.,100000,huge(1.0),-1,-1.0)

  logical, public:: DoBalancePicBlock=.true.

  ! The cell status related to get PIC involved, -1 is the default
  integer, public, allocatable:: IsPicCrit_CB(:, :, :, :)

  ! Vars of regions defined with the #REGION commands
  integer, allocatable:: iRegionPic_I(:)
  integer, allocatable:: iRegionPicLimit_I(:)
  real, allocatable:: InsidePicRegion_C(:,:,:)
  real, allocatable:: InsidePicRegionLimit_C(:,:,:)

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
       LenPic_DI(:,:), DxyzPic_DI(:,:), r_DDI(:,:,:)

  ! The number of patches of each region
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
    use BATL_lib,     ONLY: x_, y_, z_, nDim, get_region_indexes
    use ModNumConst,  ONLY: cDegToRad
    use ModCoordTransform, ONLY: rot_matrix_x, rot_matrix_y, rot_matrix_z, &
         rot_matrix

    character(len=*), intent(in):: NameCommand

    integer:: iRegion, nRegionPicTmp, iCriteria, iDim
    real :: xRotate, yRotate, zRotate

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'pic_read_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(NameCommand)
    case ("#PICGHOST")
       call read_var('nGhostPic',nGhostPic)

    case ("#PICADAPT")
       call read_var('UseAdaptivePic', UseAdaptivePic)
       if(UseAdaptivePic) then
          call read_var('DoAdaptPic', AdaptPic % DoThis)
          if(AdaptPic % DoThis) then
             call read_var('DnAdaptPic', AdaptPic % Dn)
             call read_var('DtAdaptPic', AdaptPic % Dt)
             AdaptPic % nNext = AdaptPic % Dn
             AdaptPic % tNext = AdaptPic % Dt
          end if
       end if

    case ('#PICPATCH')
       call read_var('PatchSize', nCellPerPatch)

    case ('#PICCRITERIA')
       call read_var('nCriteriaPic', nCriteriaPic)
       if(.not. allocated(NameCriteriaPic_I)) &
            allocate(NameCriteriaPic_I(nCriteriaPic))
       if(.not. allocated(CriteriaMaxPic_I)) &
            allocate(CriteriaMaxPic_I(nCriteriaPic))
       if(.not. allocated(CriteriaMinPic_I)) &
            allocate(CriteriaMinPic_I(nCriteriaPic))
       do iCriteria=1, nCriteriaPic
          call read_var('NameCriteriaPic_I', NameCriteriaPic_I(iCriteria))
          call read_var('CriteriaMinPic_I', CriteriaMinPic_I(iCriteria))
          call read_var('CriteriaMaxPic_I', CriteriaMaxPic_I(iCriteria))
       end do

    case("#PICPATCHEXTEND")
       !       if(.not. allocated(nPatchExtend_D)) allocate(nPatchExtend_D(1:nDim))
       do iDim = 1, nDim
          call read_var('nPatchExtend_D', nPatchExtend_D(iDim))
       end do

    case("#PICREGIONLIMIT")
       call read_var('StringPicRegionLimit', StringPicRegionLimit)
       if (StringPicRegionLimit /= 'none') &
            call get_region_indexes(StringPicRegionLimit, &
            iRegionPicLimit_I)


    case("#PICREGION")
       call read_var('StringPicRegion', StringPicRegion)
       if (StringPicRegion /= 'none') &
            call get_region_indexes(StringPicRegion, iRegionPic_I)

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

    case("#PICGRID")
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
            r_DDI(3,3,nRegionPic),       &
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
          r_DDI(:,:,iRegion) = 0
          r_DDI(1,1,iRegion) = 1
          r_DDI(2,2,iRegion) = 1
          r_DDI(3,3,iRegion) = 1

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
            r_DDI(3,3,nRegionPic))
       XyzMinPic_DI = 0
       XyzMaxPic_DI = 0
       XyzPic0_DI = 0
       DxyzPic_DI = 0
       LenPic_DI = 0
       r_DDI=0
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
             r_DDI(1:2,1:2,iRegion) = rot_matrix(-zRotate*cDegToRad)
          elseif(nDim==3) then
             call              read_var('xRotate', xRotate)
             call              read_var('yRotate', yRotate)
             call              read_var('zRotate', zRotate)
          endif

          XyzPic0_DI(1:nDim,iRegion) = XyzMinPic_DI(1:nDim,iRegion)

          ! Rotation matrix rotates around X, Y and Z axes in this order
          r_DDI(:,:,iRegion) = matmul( matmul( &
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

    use BATL_lib,     ONLY: nDim, find_grid_block, MaxDim, &
         x_, y_, z_, nI, nJ, nK, nBlock, MaxNode, Unused_B
    use ModPhysics,   ONLY: No2Si_V, UnitMass_, UnitCharge_
    use ModHallResist, ONLY: HallFactorMax, UseHallResist, &
         HallFactor_C, set_hall_factor_cell
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
    integer :: nByteInt, nByteChar
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

    if(UseAdaptivePic) then
       if(allocated(IsPicCrit_CB)) deallocate(IsPicCrit_CB)
       allocate(IsPicCrit_CB(1:nI,1:nJ,1:nK,1:nBlock))
       IsPicCrit_CB = iPicOff_
    end if

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
             ! storage_size returns the size in bits. 
             nSizeStatus = ceiling(real(product(PatchSize_DI(1:nDim,iRegion))) &
                  /storage_size(nSizeStatus))
             if(allocated(Status_I)) deallocate(Status_I)             
             allocate(Status_I(nSizeStatus))
             Status_I = iPicOff_
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

       ! Set active PIC cells for adaptive PIC
       if(UseAdaptivePic) then

          ! Set PIC region limit
          allocate(InsidePicRegionLimit_C(1:nI,1:nJ,1:nK))
          InsidePicRegionLimit_C = 1.0 ! initialized as inside
          ! Set user defined fixed pic regions
          allocate(InsidePicRegion_C(1:nI,1:nJ,1:nK))
          InsidePicRegion_C = 1.0

          call pic_find_node
          ! Calculate the pic region criteria
          call calc_pic_criteria
          call pic_set_cell_status
       end if

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

  subroutine pic_find_node
    ! Find out the blocks that overlaped with PIC region(s).
    use BATL_lib,  ONLY: nDim, MaxDim, find_grid_block, &
         x_, y_, z_, MaxNode
    integer:: nIjk_D(1:MaxDim), Ijk_D(1:MaxDim)
    real:: Pic_D(1:MaxDim), Mhd_D(1:MaxDim)
    integer:: iRegion, iBlock, i, j, k, iProcFound, iNode

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'pic_find_node'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub,' is called'

    if(.not.allocated(IsPicNode_A)) allocate(IsPicNode_A(MaxNode))
    IsPicNode_A = .false.

    nIjk_D = 1; PIC_D = 0; MHD_D=0

    do iRegion = 1, nRegionPic
       nIjk_D(1:nDim) = int(&
            LenPic_DI(1:nDim,iRegion)/DxyzPic_DI(1:nDim,iRegion) + 0.5)

       if(DoTest) write(*,*) NameSub,' iRegion = ',iRegion, &
            ' nIjk_D = ',nIjk_D(1:nDim)

       do k=1, nIjk_D(z_); do j=1, nIjk_D(y_); do i=1, nIjk_D(x_)
          ! Loop through all the PIC node points.
          Ijk_D(x_) = i - 1; Ijk_D(y_) = j - 1; Ijk_D(z_) = k - 1
          PIC_D(1:nDim) = Ijk_D(1:nDim)*DxyzPic_DI(1:nDim,iRegion)
          call pic_to_mhd_vec(iRegion,Pic_D,MHD_D)
          call find_grid_block(MHD_D, iProcFound, iBlock, iNodeOut=iNode)
          IsPicNode_A(iNode) = .true.
       enddo; enddo; enddo

    enddo

    if(DoTest) write(*,*)'IsPicNode= ', IsPicNode_A(:)
    call test_stop(NameSub, DoTest)
  end subroutine pic_find_node
  !============================================================================

  subroutine pic_set_cell_status

    use BATL_lib, ONLY: &
         nDim, x_, y_, z_, find_grid_block, iNode_B, &
         nI, nJ, nK, Xyz_DGB, block_inside_regions
    use BATL_Region, ONLY: points_inside_region, &
         point_inside_regions
    use ModAdvance, ONLY: State_VGB, Bx_, By_, Bz_
    use ModMain
    use ModMpi

    integer:: iStatus, iError
    integer:: nX, nY, nZ, i, j, k, iRegion, nExtend, iDim, &
         iP, jP, kP

    real:: r
    real:: XyzPic_D(nDim), XyzMhd_D(nDim), XyzMhdExtend_D(nDim)
    integer:: iBlock
    integer:: IndexPatch_D(3) = 0, IndexCenterPatch_D(3) = 0
    logical:: IsPointInside = .false.
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'pic_set_cell_status'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub,' is called'

    Status_I = iPicOff_

    do iRegion = 1, nRegionPic
       nX = PatchSize_DI(x_, iRegion)
       nY = PatchSize_DI(y_, iRegion)
       nZ = PatchSize_DI(z_, iRegion)

       ! Loop through the BATSRUS grid, find the cells need PIC involved
       do iBlock=1,nBlock

          if(allocated(iRegionPic_I)) &
               call block_inside_regions(iRegionPic_I, iBlock, &
               size(InsidePicRegion_C), 'center', &
               Value_I=InsidePicRegion_C)
          if(allocated(iRegionPicLimit_I)) &
               call block_inside_regions(iRegionPicLimit_I, iBlock, &
               size(InsidePicRegionLimit_C), 'center', &
               Value_I=InsidePicRegionLimit_C)

          do k=1,nK; do j=1,nJ; do i=1,nI

             if(.not. IsPicNode_A(iNode_B(iBlock))) CYCLE
             if(Unused_B(iBlock)) CYCLE

             ! Get the MHD coordinate for this cell
             XyzMhd_D = Xyz_DGB(1:nDim,i,j,k,iBlock)
             ! Get the patch id from the MHD coordinates
             call coord_to_patch_index(iRegion, XyzMhd_D, IndexCenterPatch_D)
             ! turn on the pic in user_defined region
             if(InsidePicRegion_C(i,j,k)>0.0) then

                IndexPatch_D = IndexCenterPatch_D
                ! set the point status without extension on the edge
                call set_point_status(Status_I, nX, nY, nZ, &
                     IndexPatch_D(x_), &
                     IndexPatch_D(y_), &
                     IndexPatch_D(z_), iPicOn_)

             else if(IsPicCrit_CB(i,j,k,iBlock)>0.0) then
                ! if(InsidePicRegionLimit_C(i,j,k)<=0.0) CYCLE
                ! Generate all extended patch indices
                do iP=-nPatchExtend_D(x_),nPatchExtend_D(x_)
                   do jP=-nPatchExtend_D(y_),nPatchExtend_D(y_)
                      do kP=-nPatchExtend_D(z_),nPatchExtend_D(z_)

                         IndexPatch_D(x_) = IndexCenterPatch_D(x_) + iP
                         IndexPatch_D(y_) = IndexCenterPatch_D(y_) + jP
                         IndexPatch_D(z_) = IndexCenterPatch_D(z_) + kP

                         if(.not. is_inside_pic_grid(IndexPatch_D, iRegion)) CYCLE
                         ! convert patch index to MHD Coordinates
                         call patch_index_to_coord(iRegion, IndexPatch_D, 'Mhd', &
                              XyzMhdExtend_D)
                         call point_inside_regions(iRegionPicLimit_I, XyzMhdExtend_D, &
                              IsPointInside)
                         ! set the point status
                         if(IsPointInside) then
                            call set_point_status(Status_I, nX, nY, nZ, &
                                 IndexPatch_D(x_), &
                                 IndexPatch_D(y_), &
                                 IndexPatch_D(z_), iPicOn_)
                         end if
                      end do
                   end do
                end do ! end loop extention on the pic domain

             end if
          end do
       end do; end do; end do

    end do ! end loop thorugh regions

    ! Global MPI reduction for Status_I array
    call MPI_Allreduce(MPI_IN_PLACE, Status_I, nSizeStatus, MPI_INT, MPI_BOR, &
         iComm, iError)

    ! if(iProc==0) print*, "!!!! ", sum(Status_I)

    ! do iRegion = 1, nRegionPic
    !    nx = PatchSize_DI(x_, iRegion)
    !    ny = PatchSize_DI(y_, iRegion)
    !    nz = PatchSize_DI(z_, iRegion)
    !    do i = 0, nx-1; do j = 0, ny-1; do k = 0, nz-1
    !        r = sqrt(real(j-ny/2 + 0.5)**2 + real(i-nx/2 + 0.5)**2)

    !        if((r < nx/4 + nx/4*mod(Time_Simulation,10.0)/10.0 .and. &
    !             r > nx/8 + nx/10*mod(Time_Simulation,10.0)/10.0) .or. &
    !             r < nx/10) then
    !           ! Setting PIC region for tests only. 
    !           call set_point_status(Status_I,nx, ny, nz, i, j, k, iPicOn_)
    !        endif

    !    enddo; enddo; enddo
    ! enddo

  end subroutine pic_set_cell_status

  logical function is_inside_pic_grid(IndexPatch_D, iRegion)

    integer, intent(in) :: IndexPatch_D(3)
    integer, intent(in) :: iRegion

    if(any(IndexPatch_D < 0) .or. &
         any(IndexPatch_D >= PatchSize_DI(:, iRegion))) then
       is_inside_pic_grid = .false.
    else 
       is_inside_pic_grid = .true.
    end if

  end function is_inside_pic_grid

  !============================================================================
  subroutine is_inside_active_pic_region(xyz_D, IsInside)
    ! It should be a function instead of a subroutine. --Yuxi
    use BATL_lib, ONLY: nDim, x_, y_, z_

    real, intent(in) :: Xyz_D(nDim)
    logical, intent(out):: IsInside
    real:: dshift_D(3)

    integer:: iRegion, iStatus, nX, nY, nZ
    integer:: Index_D(3) = 0
    integer:: dI = 0, dJ = 0, dK = 0
    !-----------------------------------------------
    iRegion = 1

    IsInside = .false. 

    if(any(Xyz_D < XyzMinPic_DI(1:nDim, iRegion) + &
         (nGhostPic - 0.1)*DxyzPic_DI(:,iRegion) )) return 
    if(any(Xyz_D > XyzMaxPic_DI(1:nDim, iRegion) - &
         (nGhostPic - 0.1)*DxyzPic_DI(:,iRegion) )) return

    nX = PatchSize_DI(x_, iRegion)
    nY = PatchSize_DI(y_, iRegion)
    nZ = PatchSize_DI(z_, iRegion)       

    do dI = -1, 1; do dJ = -1, 1; do dK = -1, 1

       dshift_D(x_) = dI
       dshift_D(y_) = dJ
       dshift_D(z_) = dK

       ! Patch cell index
       Index_D(1:nDim) = floor((Xyz_D + dshift_D(1:nDim)*DxyzPic_DI(1:nDim, iRegion) - XyzMinPic_DI(1:nDim,iRegion))/ &
            (DxyzPic_DI(1:nDim,iRegion)*nCellPerPatch))

       call get_point_status(Status_I, nx, ny, nz, Index_D(x_), Index_D(y_), &
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
    real:: Xyz_D(nDim), Pic_D(nDim)

    character(len=*), parameter:: NameSub = 'pic_find_region'
    !--------------------------------------------------------------------------
    Xyz_D = Xyz_DGB(1:nDim,i,j,k,iBlock)

    iStatus=0
    do iRegion = 1, nRegionPic
       call mhd_to_pic_vec(iRegion, Xyz_D, Pic_D)

       if(all(Pic_D > 0 ).and.&
            all(Pic_D < LenPic_DI(:,iRegion))) & ! Not accurate here. --Yuxi
            iStatus = 1
    enddo

    pic_find_region=iStatus
  end function pic_find_region

  !============================================================================
  integer function pic_find_region_active(iBlock,i,j,k)
    ! If a cell is inside the PIC region, return 1;
    ! otherwise, return 0;
    use BATL_lib, ONLY: nDim, Xyz_DGB

    integer, intent(in) :: iBlock,i,j,k

    integer:: iStatus
    integer:: iRegion
    real:: Xyz_D(nDim)
    logical :: IsInside

    character(len=*), parameter:: NameSub = 'pic_find_region_active'
    !--------------------------------------------------------------------------

    iStatus=0

    Xyz_D = Xyz_DGB(1:nDim,i,j,k,iBlock)

    call is_inside_active_pic_region(Xyz_D, IsInside)

    if (IsInside) iStatus=1
    pic_find_region_active=iStatus

  end function pic_find_region_active

  !============================================================================
  integer function pic_find_region_criteria(iBlock,i,j,k)
    ! If a cell is inside the PIC region, return 1;
    ! otherwise, return 0;
    use BATL_lib, ONLY: nDim, Xyz_DGB

    integer, intent(in) :: iBlock,i,j,k

    integer:: iStatus
    integer:: iRegion
    real:: Xyz_D(nDim)
    logical :: IsInside

    character(len=*), parameter:: NameSub = 'pic_find_region_criteria'
    !--------------------------------------------------------------------------

    iStatus=0

    if (IsPicCrit_CB(i, j, k, iBlock) >= iPicOn_) iStatus = 1

    pic_find_region_criteria=iStatus

  end function pic_find_region_criteria


  !============================================================================
  subroutine pic_to_mhd_vec(iRegion, CoordIn_D, CoordOut_D, OriginIn_D)
    ! Transfer Pic coordinates to Mhd coordinates. Origin_D
    ! is the origin of the PIC coordinates.

    use BATL_lib, ONLY: nDim

    integer, intent(in) :: iRegion
    real, intent(in)    :: CoordIn_D(nDim)
    real, intent(out)   :: CoordOut_D(nDim)
    real, intent(in), optional :: OriginIn_D(nDim)
    real :: Origin_D(nDim), Coord_D(nDim), r_DD(3, 3)

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
    Coord_D = CoordIn_D
    do iDim = 1, nDim; do jDim = 1, nDim
       R_DD(iDim,jDim) = r_DDI(jDim,iDim,iRegion)
    enddo; enddo

    CoordOut_D = 0
    do iDim = 1, nDim
       CoordOut_D(iDim) = sum(R_DD(iDim,1:nDim)*Coord_D)
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
    real :: Origin_D(nDim), Coord_D(nDim), r_DD(3, 3)

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
       CoordOut_D(iDim) = sum(r_DDI(iDim,1:nDim,iRegion)*Coord_D)
    enddo

    if(DoTest) then
       write(*,*) 'Origin_D   = ', Origin_D
       write(*,*) 'CoordIn_D  = ', CoordIn_D
       write(*,*) 'CoordOut_D = ', CoordOut_D
       Do iDim = 1, nDim
          write(*,*) 'iDim = ', iDim, 'R = ', r_DD(iDim,:)
       enddo
    endif

    call test_stop(NameSub, DoTest)
  end subroutine mhd_to_pic_vec
  !============================================================================

  subroutine coord_to_patch_index(iRegion, CoordPicIn_D, IndexPatchOut_D)

    ! This subroutine takes the PIC xyz coordinate and output
    ! the patch index in IndexPatchOut_D

    use BATL_lib, ONLY: nDim

    integer, intent(in) :: iRegion
    real, intent(in) :: CoordPicIn_D(nDim) ! xyz in PIC coordinates
    integer:: IndexPatchOut_D(nDim)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'coord_to_patch_index'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub,' is called'

    IndexPatchOut_D(1:nDim) = floor( (CoordPicIn_D - XyzMinPic_DI(1:nDim, iRegion))/ &
         (DxyzPic_DI(1:nDim, iRegion) * nCellPerPatch) )

    call test_stop(NameSub, DoTest)
  end subroutine coord_to_patch_index
  !============================================================================

  subroutine patch_index_to_coord(iRegion, IndexPatchIn_D, NameCoord, &
       CoordOut_D)

    ! This subroutine takes the adaptive PIC patch index as input, and
    ! output the corresponding MHD or PIC coordinates depends on NameCoord

    use BATL_lib, ONLY: nDim

    integer, intent(in) :: iRegion
    integer, intent(in) :: IndexPatchIn_D(nDim)
    character(len=3), intent(in) :: NameCoord
    real, intent(out) :: CoordOut_D(nDim)

    real :: CoordPic_D(nDim), CoordMhd_D(nDim)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'patch_index_to_coord'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub,' is called'

    CoordMhd_D(1:nDim) = IndexPatchIn_D(1:nDim)*(DxyzPic_DI(1:nDim, iRegion)*nCellPerPatch) + &
         XyzMinPic_DI(1:nDim, iRegion)

    if(NameCoord=='Mhd') then
       CoordOut_D = CoordMhd_D
    else if(NameCoord=='Pic') then
       call mhd_to_pic_vec(iRegion,CoordMhd_D,CoordPic_D)
       CoordOut_D = CoordPic_D
    else 
       if(iProc==0) call stop_mpi(NameSub// &
            ': NameCoord ' // NameCoord // ' not defined!')
    end if

    call test_stop(NameSub, DoTest)
  end subroutine patch_index_to_coord
  !============================================================================


  subroutine calc_pic_criteria

    ! This subroutine takes the PIC xyz coordinate and output
    ! the patch index in IndexPatchOut_D

    use BATL_lib,     ONLY: nDim, nI, nJ, nK, nBlock, x_, y_, z_, &
         iNode_B, Unused_B, iProc, MaxNode, &
         block_inside_regions, get_region_indexes, &
         Xyz_DGB
    use BATL_size,    ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK
    use ModAdvance,   ONLY: State_VGB, Bx_, By_, Bz_, Rho_
    use ModB0,        ONLY: B0_DGB
    use ModCurrent,   ONLY: get_current

    integer :: iBlock, i, j, k, iCriteria
    real :: CriteriaValue
    real, allocatable :: j_D(:)
    real :: jj, b
    logical:: DoTest

    character(len=*), parameter:: NameSub = 'calc_pic_criteria'
    !---------------------------------------------------------------。-----------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub,' is called'

    if(.not. allocated(J_D)) allocate(J_D(3))

    if(nCriteriaPic==0) then
       IsPicCrit_CB = iPicOn_             
       RETURN
    end if

    do iBlock=1,nBlock       

       if(Unused_B(iBlock)) CYCLE
       if(.not. IsPicNode_A(iNode_B(iBlock))) CYCLE

       do k=1,nK; do j=1,nJ; do i=1,nI

          call get_current(i, j, k, iBlock, j_D)

          jj = sqrt(j_D(1)**2 + j_D(2)**2 + j_D(3)**2)

          b = sqrt((State_VGB(Bx_,i,j,k,iBlock))**2+&
               (State_VGB(By_,i,j,k,iBlock))**2+&
               (State_VGB(Bz_,i,j,k,iBlock))**2)

          do iCriteria=1, nCriteriaPic

             select case(trim(NameCriteriaPic_I(iCriteria)))
             case('j/b')
                CriteriaValue = jj / b
             case('rho')
                CriteriaValue = State_VGB(Rho_,i,j,k,iBlock)
             end select

             if (CriteriaValue > CriteriaMinPic_I(iCriteria) .and. &
                  CriteriaValue < CriteriaMaxPic_I(iCriteria)) then
                IsPicCrit_CB(i,j,k,iBlock) = iPicOn_
             end if

          end do ! end loop criteria

       end do; end do; end do

    end do ! end loop blocks

    call test_stop(NameSub, DoTest)
  end subroutine calc_pic_criteria
  !============================================================================


end module ModPIC
!==============================================================================
