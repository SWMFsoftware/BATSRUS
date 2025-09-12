!  Copyright (C) 2002 Regents of the University of Michigan
!  Portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModPIC

  use BATL_lib, ONLY: &
       test_start, test_stop, iProc, iComm
  use ModBatsrusUtility, ONLY: stop_mpi
  use ModGridInfo, ONLY: iPicOn_, iPicOff_, &
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
  public:: pic_find_active_node
  public:: i_status_pic_region
  public:: i_status_pic_active
  public:: i_status_pic_criteria
  public:: pic_set_cell_status
  public:: mhd_to_pic_vec
  public:: is_inside_active_pic_region
  public:: calc_pic_criteria
  public:: write_pic_status_file
  public:: read_pic_status_file
  public:: calc_crit_jb
  public:: calc_crit_jbperp
  public:: calc_crit_entropy

  ! The PIC code that is coupled to MHD
  character(len=50), public:: NameVersionPic = 'none'

  logical, public:: UsePic = .false.

  ! If UseAdaptivePic is True, the coupler will calculate the status
  ! of the pic patch array and send to PIC. Adaptive pic only
  ! works for FLEKS so far.
  logical, public:: UseAdaptivePic = .false.

  logical, public:: DoRestartPicStatus = .false.

  real, allocatable, public :: &
       DivCurvature_CB(:,:,:,:), Curvature_DGB(:,:,:,:,:)
  real, allocatable, public :: &
       FullB_DGB(:,:,:,:,:), FullB_DG(:,:,:,:),&
       UnitBfield_DGB(:,:,:,:,:), &
       GradUnitBx_DGB(:,:,:,:,:), &
       GradUnitBy_DGB(:,:,:,:,:), &
       GradUnitBz_DGB(:,:,:,:,:)

  ! The viriables for adaptive PIC criterias
  integer, public :: nCriteriaPic=0
  character (len=10), public, allocatable :: NameCriteriaPic_I(:)
  real, public, allocatable :: &
       CriteriaMinPic_I(:), CriteriaMaxPic_I(:), &
       CriteriaMinPicDim_I(:), CriteriaMaxPicDim_I(:)
  real, public :: CriteriaB1=1.0, CriteriaB1Dim=1.0
  integer, public:: nPatchExtend_D(3)=0

  ! Description of the region where the pic region is fixed there
  character(len=200):: StringPicRegion = 'none'
  character(len=200):: StringPicRegionLimit = 'none'
  character(len=*), parameter :: NamePicStatusFile = "picstatus.rst"

  type(FreqType), public :: &
       AdaptPic = FreqType(.false.,100000,1e30,-1,-1.0)

  ! Load balance the blocks that are overlapped with the PIC grids.
  logical, public:: DoBalancePicBlock=.true.

  ! Load balance the blocks that are overlapped with the active PIC grids.
  logical, public:: DoBalanceActivePicBlock=.false.

  ! The cell status related to get PIC involved, -1 is the default
  integer, public, allocatable:: iStatusPicCrit_CB(:, :, :, :)

  ! Vars of regions defined with the #REGION commands
  integer, allocatable:: iRegionPic_I(:)
  integer, allocatable:: iRegionPicLimit_I(:)
  real, allocatable:: InsidePicRegion_C(:,:,:)
  real, allocatable:: InsidePicRegionLimit_C(:,:,:)

  ! Local variables

  ! Conversion to PIC units
  ! If UseSamePicUnit is true, use the same units for all PIC regions.
  logical, public:: UseSamePicUnit = .true.
  real, public :: xUnitPicSi = 1, uUnitPicSi = 1, MassUnitPicSi = 1
  real, public, allocatable  :: xUnitPicSi_I(:), uUnitPicSi_I(:), &
       MassUnitPicSi_I(:), ScalingFactor_I(:)

  ! File sent by the PIC code

  ! TODO: change the name nRegionPic to nGridPic ??
  ! PIC regions
  integer, public :: nRegionPic = 0

  ! R_DDI: mhd coordinates to pic coordinates.
  real, public, allocatable:: XyzMinPic_DI(:,:), XyzMaxPic_DI(:,:), &
       LenPic_DI(:,:), DxyzPic_DI(:,:), r_DDI(:,:,:)

  ! The number of patches of each region
  integer, public, allocatable:: nPatchCell_DI(:,:)

  ! Each patch uses 1 bit to record its status, on or off. The second
  ! index is the region number.
  integer, public, allocatable:: iPicStatus_I(:)
  integer, public, allocatable:: iPicStatusMin_I(:), iPicStatusMax_I(:)
  integer, public::  nSizeStatus

  integer, public:: nCellPerPatch = 4

  ! The PIC regon can rotate around x, y and z axis. The rotation rule is the
  ! same as the rotation for #REGION command.
  logical, public :: DoRotatePIC = .false.

  ! Is the node overlaped with PIC domain?
  logical, public,allocatable:: IsPicNode_A(:)

  ! Is the node overlaped with active PIC region?
  logical, public,allocatable:: IsActivePicNode_A(:)

  ! The last grid/decomposition when pic criteria were calculated.
  integer, public :: iPicGrid = -1, iPicDecomposition = -1

  logical :: IsPicRegionInitialized = .false.

contains
  !============================================================================
  subroutine pic_read_param(NameCommand)

    use ModReadParam, ONLY: read_var
    use BATL_lib, ONLY: x_, y_, z_, nI, nJ, nK, nDim, MaxBlock,&
         get_region_indexes
    use ModNumConst, ONLY: cDegToRad
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
    case ("#PICADAPT")
       call read_var('DoAdaptPic', AdaptPic % DoThis)
       if(AdaptPic % DoThis) then
          call read_var('DnAdaptPic', AdaptPic % Dn)
          call read_var('DtAdaptPic', AdaptPic % Dt)
          AdaptPic % nNext = AdaptPic % Dn
          AdaptPic % tNext = AdaptPic % Dt
       end if

    case ('#PICPATCH')
       call read_var('PatchSize', nCellPerPatch)

    case ('#PICCRITERIA')
       if(allocated(iStatusPicCrit_CB)) deallocate(iStatusPicCrit_CB)
       allocate(iStatusPicCrit_CB(nI,nJ,nK,MaxBlock))

       iStatusPicCrit_CB = iPicOff_

       call read_var('nCriteriaPic', nCriteriaPic)
       if(.not. allocated(NameCriteriaPic_I)) &
            allocate(NameCriteriaPic_I(nCriteriaPic))
       ! Criteria in IO units
       if(.not. allocated(CriteriaMaxPicDim_I)) &
            allocate(CriteriaMaxPicDim_I(nCriteriaPic))
       if(.not. allocated(CriteriaMinPicDim_I)) &
            allocate(CriteriaMinPicDim_I(nCriteriaPic))
       ! Criteria in normalized units
       if(.not. allocated(CriteriaMaxPic_I)) &
            allocate(CriteriaMaxPic_I(nCriteriaPic))
       if(.not. allocated(CriteriaMinPic_I)) &
            allocate(CriteriaMinPic_I(nCriteriaPic))
       do iCriteria=1, nCriteriaPic
          call read_var('NameCriteriaPic_I', NameCriteriaPic_I(iCriteria))
          call read_var('CriteriaMinPic_I', CriteriaMinPicDim_I(iCriteria))
          call read_var('CriteriaMaxPic_I', CriteriaMaxPicDim_I(iCriteria))
          if(NameCriteriaPic_I(iCriteria)=='j/b' .or.&
               NameCriteriaPic_I(iCriteria)=='j/bperp') then
             call read_var('CriteriaB1', CriteriaB1Dim)
          end if
       end do

    case("#PICPATCHEXTEND")
       do iDim = 1, nDim
          call read_var('nPatchExtend_D', nPatchExtend_D(iDim))
       end do

    case("#PICREGIONMAX")
       call read_var('StringPicRegionLimit', StringPicRegionLimit, &
            IsLowerCase=.true.)
       if (StringPicRegionLimit /= 'none') then
            call get_region_indexes(StringPicRegionLimit, &
            iRegionPicLimit_I)
            ! if PICREGIONMAX is presented, set PICADAPT=True
            AdaptPic % DoThis = .true.
       end if

    case("#PICREGIONMIN")
       call read_var('StringPicRegion', StringPicRegion, &
            IsLowerCase=.true.)
       if (StringPicRegion /= 'none') then
            call get_region_indexes(StringPicRegion, iRegionPic_I)
            ! if PICREGIONMIN is presented, set PICADAPT=True
            AdaptPic % DoThis = .true.
       end if

    case('#RESTARTPICSTATUS')
       call read_var('DoRestartPicStatus', DoRestartPicStatus)

    case("#PICUNIT")
       call read_var('xUnitPicSi', xUnitPicSi)
       call read_var('uUnitPicSi', uUnitPicSi)

    case("#PICGRIDUNIT")
       UseSamePicUnit = .false.
       call read_var('nPicRegion', nRegionPicTmp)

       if(nRegionPic > 0 .and. nRegionPicTmp /= nRegionPic ) then
          if(iProc==0) call stop_mpi(NameSub// &
               ': the input nPicRegion conflicts with the existing nPicRegion')
       endif
       nRegionPic = nRegionPicTmp

       if(allocated(xUnitPicSi_I)) deallocate( &
            xUnitPicSi_I, uUnitPicSi_I, MassUnitPicSi_I, ScalingFactor_I)
       allocate( &
            xUnitPicSi_I(nRegionPic),       &
            uUnitPicSi_I(nRegionPic),       &
            MassUnitPicSi_I(nRegionPic),       &
            ScalingFactor_I(nRegionPic)     )

       do iRegion = 1, nRegionPic
          call read_var('xUnitPicSi', xUnitPicSi_I(iRegion))
          call read_var('uUnitPicSi', uUnitPicSi_I(iRegion))
          call read_var('ScalingFactor', ScalingFactor_I(iRegion))
       enddo

    case("#PICBALANCE")
       call read_var('DoBalancePicBlock', DoBalancePicBlock)
       call read_var('DoBalanceActivePicBlock', DoBalanceActivePicBlock)
       if(DoBalanceActivePicBlock) DoBalancePicBlock = .false.
    case("#PICGRID")
       call read_var('nPicRegion', nRegionPicTmp)
       if(nRegionPic > 0 .and. nRegionPicTmp /= nRegionPic ) then
          if(iProc==0) call stop_mpi(NameSub// &
               ': the input nPicRegion conflicts with the existing nPicRegion')
       endif
       nRegionPic = nRegionPicTmp

       UsePic = nRegionPic > 0
       if(allocated(XyzMinPic_DI)) deallocate( &
            XyzMinPic_DI, XyzMaxPic_DI, DxyzPic_DI, nPatchCell_DI)
       allocate( &
            XyzMinPic_DI(nDim,nRegionPic), &
            XyzMaxPic_DI(nDim,nRegionPic), &
            LenPic_DI(nDim,nRegionPic), &
            r_DDI(3,3,nRegionPic),       &
            DxyzPic_DI(nDim,nRegionPic), &
            nPatchCell_DI(3, nRegionPic))
       nPatchCell_DI = 1
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

          do iDim = 1, nDim
             if (XyzMinPic_DI(iDim, iRegion) >= XyzMaxPic_DI(iDim, iRegion)) &
                  call stop_mpi(NameSub// &
                  ':MinPic should be smaller than MaxPic!')
             if(DxyzPic_DI(iDim, iRegion) < 0) call stop_mpi(NameSub//&
                  ':The PIC cell size should be a positive number!')
          enddo

          LenPic_DI(1:nDim, iRegion) = XyzMaxPic_DI(1:nDim,iRegion) - &
               XyzMinPic_DI(1:nDim,iRegion)
          r_DDI(:,:,iRegion) = 0
          r_DDI(1,1,iRegion) = 1
          r_DDI(2,2,iRegion) = 1
          r_DDI(3,3,iRegion) = 1

       end do

    case("#PICGRIDROTATE")
       DoRotatePIC = .true.

       call read_var('nPicRegion', nRegionPicTmp)
       if(nRegionPic > 0 .and. nRegionPicTmp /= nRegionPic ) then
          if(iProc==0) call stop_mpi(NameSub// &
               ': the input nPicRegion conflicts with the existing nPicRegion')
       endif
       nRegionPic = nRegionPicTmp

       UsePic = nRegionPic > 0
       if(allocated(XyzMinPic_DI)) deallocate( &
            XyzMinPic_DI, XyzMaxPic_DI, LenPic_DI, DxyzPic_DI)
       allocate( &
            XyzMinPic_DI(nDim,nRegionPic), &
            XyzMaxPic_DI(nDim,nRegionPic), &
            DxyzPic_DI(nDim,nRegionPic), &
            LenPic_DI(nDim,nRegionPic),  &
            r_DDI(3,3,nRegionPic))
       XyzMinPic_DI = 0
       XyzMaxPic_DI = 0
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

    use BATL_lib, ONLY: nDim, find_grid_block, MaxDim, &
         x_, y_, z_, nI, nJ, nK, Unset_
    use ModPhysics, ONLY: No2Si_V, UnitMass_, UnitCharge_
    use ModHallResist, ONLY: HallFactorMax, UseHallResist, &
         HallFactor_C, set_hall_factor_cell
    use ModPhysics, ONLY: IonMassPerCharge
    use ModMultiFluid, ONLY: nIonFLuid, MassIon_I
    use ModVarIndexes, ONLY: IsMhd
    use ModMpi, ONLY: MPI_REAL
    use ModMain, ONLY: iNewGrid, iNewDecomposition

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

    ! Normalizing the system so q/(mc) == 1 in PIC.
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

    ! PIC does not suppor multi-sessions now. If the pic region related
    ! parameters has been initialized, then skip this subroutine in
    ! the following sessions.
    if(IsPicRegionInitialized) RETURN
    IsPicRegionInitialized = .true.

    ! FLEKS requires UseAdaptivePic to be .true.
    UseAdaptivePic = NameVersionPic == "FLEKS"

    if(UseSamePicUnit) then
       if(allocated(xUnitPicSi_I)) deallocate( &
            xUnitPicSi_I, uUnitPicSi_I, MassUnitPicSi_I, ScalingFactor_I)
       allocate( &
            xUnitPicSi_I(nRegionPic),       &
            uUnitPicSi_I(nRegionPic),       &
            MassUnitPicSi_I(nRegionPic),       &
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
          if(UseAdaptivePic) then
             nPatchCell_DI(i,iRegion) = nCell/nCellPerPatch

             if(iProc == 0 .and. mod(nCell, nCellPerPatch) /= 0)&
                  call stop_mpi(&
                  'In all directions, the PIC grid cell number ' //  &
                  '(defined by #PICGRID) should be divisible by ' // &
                  'the patch size, which is defined by #PICPATCH.')

          endif
       end do
    end do

    if(.not. DoRestartPicStatus .and. UseAdaptivePic) then
       ! if DoRestartPicStatus, the arrays are allocated in restart part
       if(allocated(iPicStatusMin_I)) deallocate(iPicStatusMin_I)
       allocate(iPicStatusMin_I(nRegionPic))
       iPicStatusMin_I = 0

       if(allocated(iPicStatusMax_I)) deallocate(iPicStatusMax_I)
       allocate(iPicStatusMax_I(nRegionPic))
       iPicStatusMax_I = 0

       ! Calculate the size nSizeStatus and allocate integer array iPicStatus_I
       ! to store the status of the patches for all PIC grids.
       nSizeStatus = 0
       do iRegion = 1, nRegionPic
          iPicStatusMin_I(iRegion) = nSizeStatus + 1

          iPicStatusMax_I(iRegion) = iPicStatusMin_I(iRegion) -1 + &
               ceiling(real(product(nPatchCell_DI(1:nDim,iRegion))) &
               /storage_size(nSizeStatus))

          ! The number of integers needed to store the patch status information
          nSizeStatus = iPicStatusMax_I(iRegion)
       enddo

       if(allocated(iPicStatus_I)) deallocate(iPicStatus_I)
       allocate(iPicStatus_I(nSizeStatus))
       call set_status_all(iPicOff_)

    endif

    do iRegion = 1, nRegionPic
       XyzMaxPic_DI(:,iRegion) = XyzMinPic_DI(:,iRegion) + LenPic_DI(:,iRegion)

       if(UseHallResist) then
          PicMiddle_D = 0
          do i = 0, 2
             ! Part of the PIC grid may overlap with the MHD body, where the
             ! Hall factor is not defined. But it is not likely that the
             ! lower corner, the middle point, and the upper corner
             ! are all inside a body.
             PicMiddle_D(1:nDim) = XyzMinPic_DI(:,iRegion) + &
                  0.5*i*LenPic_DI(:,iRegion)
             call find_grid_block(PicMiddle_D, iProcPic,iBlockPic, &
                  iCellOut_D=iCell_D)
             if(iProcPic /= Unset_) EXIT
          enddo

          if(iProc == 0 .and. iProcPic == Unset_) call stop_mpi(&
               'Error: can not get the scaling factor for the PIC grid!')

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

       MassUnitPicSi = 1e7*xUnitPicSi_I(iRegion) * &
            (IonMassPerChargeSi*ScalingFactor_I(iRegion))**2
       MassUnitPicSi_I(iRegion) = MassUnitPicSi

       if(iProc==0)then
          write(*,*) NameSub,': iRegion            = ',iRegion
          write(*,*) NameSub,': IonMassPerChargeSi = ',IonMassPerChargeSi
          write(*,*) NameSub,': xUnitPicSi         = ',xUnitPicSi_I(iRegion)
          write(*,*) NameSub,': uUnitPicSi         = ',uUnitPicSi_I(iRegion)
          write(*,*) NameSub,': ScalingFactor      = ',ScalingFactor_I(iRegion)
          write(*,*) NameSub,': MassUnitPicSi      = ',MassUnitPicSi_I(iRegion)
       end if
    end do

    ! iPicGrid and iPicDecomposition should be initialized here
    ! in case #PICREGIONMIN/MAX is used but PICADAPT is set to false
    iPicGrid = iNewGrid
    iPicDecomposition = iNewDecomposition

    ! Set active PIC cells for adaptive PIC
    if(UseAdaptivePic) then

       ! Set PIC region limit
       allocate(InsidePicRegionLimit_C(1:nI,1:nJ,1:nK))
       InsidePicRegionLimit_C = 1.0 ! initialized as inside
       ! Set user defined fixed pic regions
       allocate(InsidePicRegion_C(1:nI,1:nJ,1:nK))
       InsidePicRegion_C = 1.0

       ! Calculate the pic region criteria
       if(.not. DoRestartPicStatus) then
          if(allocated(iStatusPicCrit_CB)) then
             call calc_pic_criteria
          end if
          call pic_set_cell_status
       end if

       if(iProc==0) then
          write(*,*)
          write(*,*) NameSub,': Adapt-PIC Initialization Finished'
          write(*,*)
       end if
    end if

    if(iProc == 0) then
       write(*,*) "Corrected PIC regions"
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
  subroutine write_pic_status_file

    use ModUtilities, ONLY: open_file, close_file
    use ModIoUnit, ONLY: UnitTmp_

    character(len=100) :: NameFile
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_pic_status_file'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! iPicStatus_I exists in every proc, use only the first one
    if(iProc /= 0) RETURN

    NameFile = trim('GM/restartOUT/')//NamePicStatusFile
    call open_file(FILE=NameFile, form='UNFORMATTED', NameCaller=NameSub)
    write(UnitTmp_) nSizeStatus
    write(UnitTmp_) iPicStatus_I
    write(UnitTmp_) nRegionPic
    write(UnitTmp_) iPicStatusMin_I
    write(UnitTmp_) iPicStatusMax_I

    call close_file

    call test_stop(NameSub, DoTest)
  end subroutine write_pic_status_file
  !============================================================================
  subroutine read_pic_status_file

    use ModUtilities, ONLY: open_file, close_file
    use ModIoUnit, ONLY: UnitTmp_

    integer:: iError

    character(len=100) :: NameFile
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_pic_status_file'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    NameFile = trim('GM/restartIN/')//NamePicStatusFile
    call open_file(FILE=NameFile, status='old', form='UNFORMATTED', &
         NameCaller=NameSub)
    read(UnitTmp_, iostat = iError) nSizeStatus
    if(allocated(iPicStatus_I)) deallocate(iPicStatus_I)
    allocate(iPicStatus_I(nSizeStatus))
    read(UnitTmp_, iostat = iError) iPicStatus_I
    read(UnitTmp_, iostat = iError) nRegionPic
    if(allocated(iPicStatusMin_I)) deallocate(iPicStatusMin_I)
    allocate(iPicStatusMin_I(nRegionPic))
    if(allocated(iPicStatusMax_I)) deallocate(iPicStatusMax_I)
    allocate(iPicStatusMax_I(nRegionPic))
    read(UnitTmp_, iostat = iError) iPicStatusMin_I
    read(UnitTmp_, iostat = iError) iPicStatusMax_I
    call close_file

    if(iError /= 0) call stop_mpi(NameSub// &
         ' could not read data from '//trim(NameFile))

    call test_stop(NameSub, DoTest)

  end subroutine read_pic_status_file
  !============================================================================
  subroutine pic_find_node

    ! Find blocks that overlap with PIC region(s).
    use BATL_lib, ONLY: nI, nJ, nK, nBlock, Unused_B, &
         iNode_B, MaxNode

    integer:: iBlock, i, j, k

    logical:: IsPicBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'pic_find_node'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub,' is called'

    if(.not.allocated(IsPicNode_A)) allocate(IsPicNode_A(MaxNode))
    IsPicNode_A = .false.

    do iBlock=1,nBlock
       if(Unused_B(iBlock)) CYCLE
       IsPicBlock = .false.
       ! Loop through all cells of a block, if any cell of this block
       ! is overlapped with the PIC grid, this block is considered as
       ! a PIC block/node.
       do i = 1, nI; do j = 1, nJ; do k = 1, nK
          if(.not. IsPicBlock .and. i_status_pic_region(iBlock,i,j,k) == 1) &
               IsPicBlock = .true.
       end do; end do; end do
       IsPicNode_A(iNode_B(iBlock)) = IsPicBlock
    end do

    if(DoTest) write(*,*)'IsPicNode= ', IsPicNode_A(:)
    call test_stop(NameSub, DoTest)

  end subroutine pic_find_node
  !============================================================================
  subroutine pic_find_active_node

    ! Find blocks that overlap with active PIC region(s).
    use BATL_lib, ONLY: nI, nJ, nK, nBlock, Unused_B, &
         iNode_B, MaxNode

    integer:: iBlock, i, j, k

    logical:: IsActivePicBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'pic_find_active_node'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub,' is called'

    if(.not.allocated(IsActivePicNode_A)) allocate(IsActivePicNode_A(MaxNode))
    IsActivePicNode_A = .false.

    if(IsPicRegionInitialized) then
       do iBlock=1,nBlock
          if(Unused_B(iBlock)) CYCLE
          IsActivePicBlock = .false.
          ! Loop through all cells of a block, if any cell of this block
          ! is overlapped with the PIC grid, this block is considered as
          ! a PIC block/node.
          do i = 1, nI; do j = 1, nJ; do k = 1, nK
             if(.not. IsActivePicBlock .and. &
                  i_status_pic_active(iBlock,i,j,k) == 1) &
                  IsActivePicBlock = .true.
          end do; end do; end do
          IsActivePicNode_A(iNode_B(iBlock)) = IsActivePicBlock
       end do
    else
       call pic_find_node
       IsActivePicNode_A = IsPicNode_A
    endif
    if(DoTest) write(*,*)'IsActivePicNode= ', IsActivePicNode_A(:)
    call test_stop(NameSub, DoTest)

  end subroutine pic_find_active_node
  !============================================================================
  subroutine pic_set_cell_status

    use BATL_lib, ONLY: &
         nDim, x_, y_, z_,&
         Xyz_DNB, nBlock, Unused_B,&
         CoordMin_DB, CoordMax_DB,&
         nIJK_D, IsCartesianGrid, xyz_to_coord, CellSize_DB
    use BATL_Region, ONLY: &
         is_point_inside_regions
    use ModMain
    use ModMpi

    integer:: iError
    integer:: nX, nY, nZ, i, j, k, iRegion, iDim, &
         iP, jP, kP, iPExt, jPExt, kPExt

    real:: XyzMinBlock_D(MaxDim), XyzMaxBlock_D(MaxDim),&
         XyzMinPic_D(nDim), XyzMaxPic_D(nDim)

    real:: XyzMhd_D(MaxDim)=0.0, XyzPatchMhd_D(MaxDim)=0.0, Coord_D(MaxDim)
    integer:: iBlock, iCell_D(MaxDim)
    integer:: iPatch_D(3) = 0, iPatchCell_D(3) = 0,&
         iPatchMin_D(3) = 0, iPatchMax_D(3) = 0

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'pic_set_cell_status'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub,' is called'

    call set_status_all(iPicOff_)

    ! although UseAdaptivePic is .true., adaptation is turned off
    ! if PICREGIONMAX and PICREGIONMIN are not defined
    ! directly turn on all PIC patches
    if(.not. AdaptPic % DoThis .and.&
         .not. allocated(iRegionPic_I) .and.&
         .not. allocated(iRegionPicLimit_I)) then
       call set_status_all(iPicOn_)
       RETURN
    end if

    do iRegion = 1, nRegionPic

       nX = nPatchCell_DI(x_, iRegion)
       nY = nPatchCell_DI(y_, iRegion)
       nZ = nPatchCell_DI(z_, iRegion)

       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE

          if(IsCartesianGrid) then
             XyzMinBlock_D = CoordMin_DB(:, iBlock)
             XyzMaxBlock_D = CoordMax_DB(:, iBlock)
          else
             ! In non-cartesian case, the min&max gen. coords
             ! are not neccessarily real min&max projected in
             ! cartesian grid, gathering all coordinates of nodes
             ! and take the min&max in cartesian
             do iDim=1, nDim
                XyzMinBlock_D(iDim) = minval(minval(minval(&
                     Xyz_DNB(iDim, :, :, :, iBlock),&
                     dim=1), dim=1), dim=1)
                XyzMaxBlock_D(iDim) = maxval(maxval(maxval(&
                     Xyz_DNB(iDim, :, :, :, iBlock),&
                     dim=1), dim=1), dim=1)
             end do
          end if

          ! Then convert to PIC xyz
          call mhd_to_pic_vec(iRegion, XyzMinBlock_D,&
               XyzMinPic_D(1:nDim))
          call mhd_to_pic_vec(iRegion, XyzMaxBlock_D,&
               XyzMaxPic_D(1:nDim))
          ! Then convert to patch indices
          call coord_to_patch_index(iRegion, XyzMinPic_D(1:nDim),&
               iPatchMin_D(1:nDim))
          call coord_to_patch_index(iRegion, XyzMaxPic_D(1:nDim),&
               iPatchMax_D(1:nDim))

          ! loop through FLEKS patches in this MHD block
          do iP = max(iPatchMin_D(x_), 0), min(iPatchMax_D(x_), nX - 1)
             do jP = max(iPatchMin_D(y_), 0), min(iPatchMax_D(y_), nY - 1)
                do kP = max(iPatchMin_D(z_), 0), min(iPatchMax_D(z_), nZ - 1)

                   ! current patch
                   iPatch_D = [iP, jP, kP]
                   call patch_index_to_coord(iRegion, iPatch_D, "Mhd",&
                        XyzPatchMhd_D)

                   ! loop through cells in this patch
                   do i=0,nCellPerPatch-1;do j=0,nCellPerPatch-1;&
                        do k=0,nCellPerPatch-1

                      ! cell coordinate of i,j,k
                      iPatchCell_D = [i, j, k]
                      ! get cell coordinate in the patch
                      XyzMhd_D(1:nDim) = XyzPatchMhd_D(1:nDim)+&
                           (iPatchCell_D(1:nDim)+0.5)*&
                           DxyzPic_DI(1:nDim, iRegion)

                      ! first check if #PICREGIONMIN is defined
                      ! and turn on cell inside it
                      if(allocated(iRegionPic_I)) then
                         if(is_point_inside_regions(iRegionPic_I,&
                              XyzMhd_D)) then
                            call set_point_status(iPicStatus_I(&
                                 iPicStatusMin_I(iRegion):&
                                 iPicStatusMax_I(iRegion)),&
                                 nX, nY, nZ, iP, jP, kP, iPicOn_)
                            CYCLE
                         endif
                      end if ! end check #PICREGIONMIN

                      if(allocated(iRegionPicLimit_I)) then
                         if(.not. is_point_inside_regions(iRegionPicLimit_I,&
                              XyzMhd_D)) CYCLE
                      end if

                      ! The non-cartersian case is considered in xyz_to_coord
                      call xyz_to_coord(XyzMhd_D, Coord_D)
                      iCell_D = 1 + (Coord_D - CoordMin_DB(:, iBlock))/&
                           CellSize_DB(:, iBlock)

                      ! in case point outside block
                      if( any(iCell_D>nIJK_D) .or. any(iCell_D<1) ) CYCLE

                      ! In case just use #PICREGIONMIN to control the PIC shape
                      if(.not. allocated(iStatusPicCrit_CB)) CYCLE

                      if(allocated(iStatusPicCrit_CB)) then
                         if(iStatusPicCrit_CB(&
                              iCell_D(x_),iCell_D(y_),iCell_D(z_),iBlock)&
                              /=iPicOn_) CYCLE
                      end if

                      ! Also switching on the surrounding patches.
                      do iPExt = max(iP - nPatchExtend_D(x_), 0), &
                           min(iP + nPatchExtend_D(x_), nX-1)
                         do jPExt = max(jP - nPatchExtend_D(y_), 0), &
                              min(jP + nPatchExtend_D(y_), nY-1)
                            do kPExt = max(kP - nPatchExtend_D(z_), 0), &
                                 min(kP + nPatchExtend_D(z_), nZ-1)

                               ! The patches switched on here may outside
                               ! the regions that are defined by #PICREGIONMAX.
                               ! So, correct_status is called below to ensure
                               ! all active patches are inside the regions of
                               ! #PICREGIONMAX.
                               call set_point_status(iPicStatus_I(&
                                    iPicStatusMin_I(iRegion):&
                                    iPicStatusMax_I(iRegion)),&
                                    nX, nY, nZ, iPExt, jPExt, kPExt, iPicOn_)

                            enddo
                         enddo
                      enddo
                   end do; end do; end do ! end looping cells
                end do
             end do
          end do

       end do ! end loop blocks
    end do ! end loop thorugh regions

    call correct_status

    ! Global MPI reduction for iPicStatus_I array
    call MPI_Allreduce(MPI_IN_PLACE, iPicStatus_I, nSizeStatus,&
         MPI_INT, MPI_BOR, iComm, iError)

  end subroutine pic_set_cell_status
  !============================================================================
  logical function is_inside_pic_grid(iPatchIn_D, iRegion)

    integer, intent(in) :: iPatchIn_D(3)
    integer, intent(in) :: iRegion

    !--------------------------------------------------------------------------
    if(any(iPatchIn_D < 0) .or. &
         any(iPatchIn_D >= nPatchCell_DI(:, iRegion))) then
       is_inside_pic_grid = .false.
    else
       is_inside_pic_grid = .true.
    end if

  end function is_inside_pic_grid
  !============================================================================
  subroutine correct_status

    ! This subroutine ensures all active patches are inside the regions
    ! that are defined by #PICREGIONMAX

    use BATL_lib, ONLY: x_, y_, z_, MaxDim, nDim
    use BATL_Region, ONLY: is_point_inside_regions

    integer :: iRegion, nX, nY, nZ, i, j, k
    integer :: iStatus
    integer:: iPatch_D(MaxDim)
    real:: XyzMhd_D(MaxDim)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'correct_status'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub,' is called'

    if(.not. allocated(iRegionPicLimit_I)) RETURN

    do iRegion = 1, nRegionPic
       nX = nPatchCell_DI(x_, iRegion)
       nY = nPatchCell_DI(y_, iRegion)
       nZ = nPatchCell_DI(z_, iRegion)
       do i = 0, nX-1; do j = 0, nY-1; do k = 0, nZ-1
          call get_point_status(iPicStatus_I(&
               iPicStatusMin_I(iRegion):iPicStatusMax_I(iRegion)),&
               nX, nY, nZ, i, j, k, iStatus)
          if(iStatus == iPicOn_) then
             iPatch_D = [i, j,k]
             call patch_index_to_coord(iRegion, iPatch_D(1:nDim), &
                  "Mhd", XyzMhd_D(1:nDim))

             if(.not. is_point_inside_regions(&
                  iRegionPicLimit_I, XyzMhd_D(1:nDim))) then
                call set_point_status(iPicStatus_I(&
                     iPicStatusMin_I(iRegion):iPicStatusMax_I(iRegion)),&
                     nX, nY, nZ, i, j, k, iPicOff_)
             endif

          endif
       enddo; enddo; enddo
    enddo
  end subroutine correct_status
  !============================================================================
  subroutine set_status_all(iStatusDest)

    ! The subroutine that set entire iPicStatus_I to iStatusDest

    use BATL_lib, ONLY: x_, y_, z_
    integer, intent(in) :: iStatusDest

    integer :: iRegion, nX, nY, nZ, i, j, k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_status_all'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub,' is called'

    iPicStatus_I = 0

    do iRegion = 1, nRegionPic
       nX = nPatchCell_DI(x_, iRegion)
       nY = nPatchCell_DI(y_, iRegion)
       nZ = nPatchCell_DI(z_, iRegion)
       do i = 0, nX-1; do j = 0, nY-1; do k = 0, nZ-1
          call set_point_status(iPicStatus_I(&
               iPicStatusMin_I(iRegion):iPicStatusMax_I(iRegion)),&
               nX, nY, nZ, i, j, k, iStatusDest)
       enddo; enddo; enddo
    enddo

  end subroutine set_status_all
  !============================================================================
  subroutine is_inside_active_pic_region(xyz_D, IsInside)

    ! It should be a function instead of a subroutine. --Yuxi

    use BATL_lib, ONLY: nDim, x_, y_, z_

    real, intent(in) :: Xyz_D(nDim)
    logical, intent(out):: IsInside

    integer:: iRegion, iStatus, nX, nY, nZ
    integer:: iCellInPatch_D(3) = 0

    !--------------------------------------------------------------------------
    IsInside = .false.

    do iRegion = 1, nRegionPic

       ! If Xyz_D is outside this PIC grid, then go to check the next PIC grid.
       if(any(Xyz_D < XyzMinPic_DI(1:nDim, iRegion))) CYCLE
       if(any(Xyz_D > XyzMaxPic_DI(1:nDim, iRegion))) CYCLE

       nX = nPatchCell_DI(x_, iRegion)
       nY = nPatchCell_DI(y_, iRegion)
       nZ = nPatchCell_DI(z_, iRegion)

       ! Patch cell index
       iCellInPatch_D(1:nDim) = floor((Xyz_D - &
            XyzMinPic_DI(1:nDim,iRegion))/ &
            (DxyzPic_DI(1:nDim,iRegion)*nCellPerPatch))

       call get_point_status(&
            iPicStatus_I(iPicStatusMin_I(iRegion):&
            iPicStatusMax_I(iRegion)),&
            nX, nY, nZ, iCellInPatch_D(x_), iCellInPatch_D(y_),&
            iCellInPatch_D(z_), iStatus)

       if(iStatus==iPicOff_) RETURN

       ! All the patches surround Xyz_D are actived.
       IsInside = .true.
       RETURN

    end do
  end subroutine is_inside_active_pic_region
  !============================================================================
  integer function i_status_pic_region(iBlock,i,j,k)

    ! If a cell is inside the PIC region, return 1;
    ! otherwise, return 0;

    use BATL_lib, ONLY: nDim, Xyz_DGB

    integer, intent(in) :: iBlock,i,j,k

    integer:: iStatus
    integer:: iRegion
    real:: Xyz_D(nDim), Pic_D(nDim)

    character(len=*), parameter:: NameSub = 'i_status_pic_region'
    !--------------------------------------------------------------------------
    Xyz_D = Xyz_DGB(1:nDim,i,j,k,iBlock)

    iStatus=0
    do iRegion = 1, nRegionPic
       call mhd_to_pic_vec(iRegion, Xyz_D, Pic_D)

       if(all(Pic_D > 0 ).and.&
            all(Pic_D < LenPic_DI(:,iRegion))) &
            iStatus = 1
    enddo

    i_status_pic_region=iStatus
  end function i_status_pic_region
  !============================================================================
  integer function i_status_pic_active(iBlock,i,j,k)

    ! If a cell is inside the PIC region, return 1;
    ! otherwise, return 0;

    use BATL_lib, ONLY: nDim, Xyz_DGB

    integer, intent(in) :: iBlock,i,j,k

    integer:: iStatus
    real:: Xyz_D(nDim)
    logical :: IsInside

    character(len=*), parameter:: NameSub = 'i_status_pic_active'
    !--------------------------------------------------------------------------

    iStatus=0

    Xyz_D = Xyz_DGB(1:nDim,i,j,k,iBlock)

    call is_inside_active_pic_region(Xyz_D, IsInside)

    if (IsInside) iStatus=1
    i_status_pic_active=iStatus

  end function i_status_pic_active
  !============================================================================
  integer function i_status_pic_criteria(iBlock,i,j,k)

    ! If a cell is inside the PIC region, return 1;
    ! otherwise, return 0;

    integer, intent(in) :: iBlock,i,j,k

    integer:: iStatus

    character(len=*), parameter:: NameSub = 'i_status_pic_criteria'
    !--------------------------------------------------------------------------

    iStatus=0

    if (iStatusPicCrit_CB(i, j, k, iBlock) >= iPicOn_) iStatus = 1

    i_status_pic_criteria=iStatus

  end function i_status_pic_criteria
  !============================================================================
  subroutine pic_to_mhd_vec(iRegion, CoordIn_D, CoordOut_D, OriginIn_D)

    ! Transfer PIC coordinates to Mhd coordinates. Origin_D
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
  subroutine coord_to_patch_index(iRegion, CoordPicIn_D, iPatchOut_D)

    ! This subroutine takes the PIC xyz coordinate and output
    ! the patch index in iPatchOut_D

    use BATL_lib, ONLY: nDim

    integer, intent(in) :: iRegion
    real, intent(in) :: CoordPicIn_D(nDim) ! xyz in PIC coordinates
    integer:: iPatchOut_D(nDim)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'coord_to_patch_index'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub,' is called'

    iPatchOut_D(1:nDim) = floor(&
         CoordPicIn_D/&
         (DxyzPic_DI(1:nDim, iRegion) * nCellPerPatch)&
         )

    call test_stop(NameSub, DoTest)
  end subroutine coord_to_patch_index
  !============================================================================
  subroutine patch_index_to_coord(iRegion, iPatchIn_D, NameCoord, &
       CoordOut_D)

    ! This subroutine takes the adaptive PIC patch index as input, and
    ! output the corresponding MHD or PIC coordinates depends on NameCoord

    use BATL_lib, ONLY: nDim

    integer, intent(in) :: iRegion
    integer, intent(in) :: iPatchIn_D(nDim)
    character(len=3), intent(in) :: NameCoord
    real, intent(out) :: CoordOut_D(nDim)

    real :: CoordPic_D(nDim), CoordMhd_D(nDim)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'patch_index_to_coord'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub,' is called'

    CoordMhd_D(1:nDim) = &
         iPatchIn_D(1:nDim)*(DxyzPic_DI(1:nDim,iRegion)*nCellPerPatch) &
         + XyzMinPic_DI(1:nDim,iRegion)

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
    ! the patch index in iPatchOut_D

    use BATL_lib, ONLY: nDim, nI, nJ, nK, nG, nBlock, MaxBlock, &
         x_, y_, z_, iNode_B, Unused_B, iProc, &
         message_pass_cell, CellSize_DB
    use BATL_size, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK
    use ModAdvance, ONLY: State_VGB, Bx_, Bz_, Rho_, &
         x_, y_, z_, Ux_, Uz_, p_, RhoUx_, RhoUz_
    use ModCurrent, ONLY: get_current
    use ModCellGradient, ONLY: calc_divergence, calc_gradient
    use ModB0, ONLY: UseB0, B0_DGB
    use ModPhysics, ONLY: Io2No_V, UnitX_, UnitB_, UnitU_, &
         UnitP_, UnitRho_, Gamma
    use ModMain, ONLY: iNewGrid, iNewDecomposition

    integer :: iBlock, i, j, k, iCriteria
    integer, allocatable :: iPicStatus_CBI(:,:,:,:,:)
    real :: CritJB, CritJBperp, CritEntropy, CriteriaValue
    real, allocatable :: Current_D(:), Ufield_DGB(:,:,:,:,:),&
         DivU_CB(:,:,:,:), CurrentCrossB_D(:)
    logical:: IsSatisfyAllCrit

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_pic_criteria'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub,' is called'

    iPicGrid = iNewGrid
    iPicDecomposition = iNewDecomposition

    if(.not. allocated(iStatusPicCrit_CB)) RETURN

    if(nCriteriaPic==0) then
       iStatusPicCrit_CB = iPicOn_
       RETURN
    end if

    call pic_find_node

    ! if pic criteria exists in PARAM.in
    if(.not. allocated(iPicStatus_CBI)) &
         allocate(iPicStatus_CBI(nI,nJ,nK,MaxBlock,nCriteriaPic))

    do iCriteria=1,nCriteriaPic
       select case(trim(NameCriteriaPic_I(iCriteria)))
       case('rho')
          CriteriaMinPic_I(iCriteria)=CriteriaMinPicDim_I(iCriteria)
          CriteriaMaxPic_I(iCriteria)=CriteriaMaxPicDim_I(iCriteria)
       case('j/b')
          CriteriaMinPic_I(iCriteria)=CriteriaMinPicDim_I(iCriteria)
          CriteriaMaxPic_I(iCriteria)=CriteriaMaxPicDim_I(iCriteria)
          CriteriaB1 = CriteriaB1Dim * Io2No_V(UnitB_)
       case('j/bperp')
          CriteriaMinPic_I(iCriteria)=CriteriaMinPicDim_I(iCriteria)
          CriteriaMaxPic_I(iCriteria)=CriteriaMaxPicDim_I(iCriteria)
          CriteriaB1 = CriteriaB1Dim * Io2No_V(UnitB_)
       case('divu')
          CriteriaMinPic_I(iCriteria)=CriteriaMinPicDim_I(iCriteria)&
               *Io2No_V(UnitU_)/Io2No_V(UnitX_)
          CriteriaMaxPic_I(iCriteria)=CriteriaMaxPicDim_I(iCriteria)&
               *Io2No_V(UnitU_)/Io2No_V(UnitX_)
       case('divcurv')
          CriteriaMinPic_I(iCriteria)=CriteriaMinPicDim_I(iCriteria)
          CriteriaMaxPic_I(iCriteria)=CriteriaMaxPicDim_I(iCriteria)
       case('beta')
          CriteriaMinPic_I(iCriteria)=CriteriaMinPicDim_I(iCriteria)
          CriteriaMaxPic_I(iCriteria)=CriteriaMaxPicDim_I(iCriteria)
       case('entropy')
          CriteriaMinPic_I(iCriteria)=CriteriaMinPicDim_I(iCriteria)&
               *Io2No_V(UnitP_)/(Io2No_V(UnitRho_)**Gamma)
          CriteriaMaxPic_I(iCriteria)=CriteriaMaxPicDim_I(iCriteria)&
               *Io2No_V(UnitP_)/(Io2No_V(UnitRho_)**Gamma)
       case('speed')
          CriteriaMinPic_I(iCriteria)=CriteriaMinPicDim_I(iCriteria)&
               *Io2No_V(UnitU_)
          CriteriaMaxPic_I(iCriteria)=CriteriaMaxPicDim_I(iCriteria)&
               *Io2No_V(UnitU_)
       case('jy')
          if(.not. allocated(Current_D)) allocate(Current_D(3))
          CriteriaMinPic_I(iCriteria)=CriteriaMinPicDim_I(iCriteria)
          CriteriaMaxPic_I(iCriteria)=CriteriaMaxPicDim_I(iCriteria)
       end select
    end do

    iStatusPicCrit_CB = iPicOff_
    iPicStatus_CBI = iPicOff_

    ! Full B Field is a useful variable
    allocate(FullB_DGB(3,minI:maxI,minJ:maxJ,minK:maxK,nBlock))
    allocate(FullB_DG(3,minI:maxI,minJ:maxJ,minK:maxK))

    do iBlock=1,nBlock
       if(Unused_B(iBlock)) CYCLE
       if(UseB0) then
          FullB_DGB(:,:,:,:,iBlock) = &
               State_VGB(Bx_:Bz_,:,:,:,iBlock) + B0_DGB(:,:,:,:,iBlock)
       else
          FullB_DGB(:,:,:,:,iBlock) = State_VGB(Bx_:Bz_,:,:,:,iBlock)
       end if
    end do

    ! loop through criterias and allocate variables
    ! also the unit transfer is done here
    do iCriteria=1, nCriteriaPic
       select case(trim(NameCriteriaPic_I(iCriteria)))
       case('divu')
          allocate(DivU_CB(minI:maxI,minJ:maxJ,minK:maxK,MaxBlock))
          allocate(Ufield_DGB(3,minI:maxI,minJ:maxJ,minK:maxK,MaxBlock))
       case('divcurv')
          allocate(UnitBfield_DGB(nDim,minI:maxI,minJ:maxJ,minK:maxK,MaxBlock))
          allocate(Curvature_DGB(nDim,minI:maxI,minJ:maxJ,minK:maxK,MaxBlock))
          if(.not. allocated(DivCurvature_CB)) then
             allocate(DivCurvature_CB(minI:maxI,minJ:maxJ,minK:maxK,MaxBlock))
             DivCurvature_CB = 0.0
          end if
          allocate(GradUnitBx_DGB(nDim,minI:maxI,minJ:maxJ,minK:maxK,MaxBlock))
          allocate(GradUnitBy_DGB(nDim,minI:maxI,minJ:maxJ,minK:maxK,MaxBlock))
          allocate(GradUnitBz_DGB(nDim,minI:maxI,minJ:maxJ,minK:maxK,MaxBlock))
          UnitBfield_DGB = 0.0
          Curvature_DGB = 0.0
          GradUnitBx_DGB = 0.0
          GradUnitBy_DGB = 0.0
          GradUnitBz_DGB = 0.0
       end select
    end do

    do iBlock=1,nBlock
       if(Unused_B(iBlock)) CYCLE
       if(.not. IsPicNode_A(iNode_B(iBlock))) CYCLE

       if(allocated(DivU_CB)) then
          Ufield_DGB(:,:,:,:,iBlock) = State_VGB(Ux_:Uz_,:,:,:,iBlock)
          call calc_divergence(iBlock, Ufield_DGB(:,:,:,:,iBlock), &
               0, DivU_CB(:,:,:,iBlock), UseBodyCellIn=.true.)
       end if

       if(allocated(DivCurvature_CB)) then

          do k=minK,maxK; do j=minJ,maxJ; do i=minI,maxI
             UnitBfield_DGB(1:nDim,i,j,k,iBlock) = &
                  FullB_DGB(1:nDim,i,j,k,iBlock) &
                  / max(norm2(FullB_DGB(1:nDim,i,j,k,iBlock)),1e-30)
          end do; end do; end do

          ! Notice that Gradient B field only have physical cell values
          call calc_gradient(iBlock, UnitBfield_DGB(x_,:,:,:,iBlock), &
               nG, GradUnitBx_DGB(:,:,:,:,iBlock), UseBodyCellIn=.true.)
          if(nJ>1) call calc_gradient(iBlock,&
               UnitBfield_DGB(y_,:,:,:,iBlock), &
               nG, GradUnitBy_DGB(:,:,:,:,iBlock), UseBodyCellIn=.true.)
          if(nK>1) call calc_gradient(iBlock,&
               UnitBfield_DGB(z_,:,:,:,iBlock), &
               nG, GradUnitBz_DGB(:,:,:,:,iBlock), UseBodyCellIn=.true.)

          do k=1,nK; do j=1,nJ; do i=1,nI
             ! calculate the b*grad part of curvature
             Curvature_DGB(x_,i,j,k,iBlock) = &
                  sum(UnitBfield_DGB(:,i,j,k,iBlock) &
                  *GradUnitBx_DGB(:,i,j,k,iBlock))
             if(nJ>1) Curvature_DGB(y_,i,j,k,iBlock) = &
                  sum(UnitBfield_DGB(:,i,j,k,iBlock) &
                  *GradUnitBy_DGB(:,i,j,k,iBlock))
             if(nK>1) Curvature_DGB(z_,i,j,k,iBlock) = &
                  sum(UnitBfield_DGB(:,i,j,k,iBlock)&
                  *GradUnitBz_DGB(:,i,j,k,iBlock))
          end do; end do; end do

       end if

    end do

    ! Fill the ghost cells for calculating divergence
    if(allocated(Curvature_DGB)) call message_pass_cell(nDim, Curvature_DGB)

    do iBlock=1,nBlock

       if(Unused_B(iBlock)) CYCLE
       if(.not. IsPicNode_A(iNode_B(iBlock))) CYCLE

       if(allocated(Curvature_DGB)) then
          call calc_divergence(iBlock, Curvature_DGB(:,:,:,:,iBlock), &
               nG, DivCurvature_CB(:,:,:,iBlock), UseBodyCellIn=.true.)
          DivCurvature_CB(:,:,:,iBlock) = DivCurvature_CB(:,:,:,iBlock)*&
               (CellSize_DB(1, iBlock)**2)
       end if

       FullB_DG = FullB_DGB(:,:,:,:,iBlock)

       do k=1,nK; do j=1,nJ; do i=1,nI
          do iCriteria=1, nCriteriaPic
             select case(trim(NameCriteriaPic_I(iCriteria)))
             case('j/bperp')
                call calc_crit_jbperp(i, j, k, iBlock, FullB_DG,&
                     CriteriaB1, CritJBperp)
                CriteriaValue = CritJBperp
             case('j/b')
                call calc_crit_jb(i, j, k, iBlock, FullB_DG,&
                     CriteriaB1, CritJB)
                CriteriaValue = CritJB
             case('rho')
                CriteriaValue = State_VGB(Rho_,i,j,k,iBlock)
             case('divu')
                CriteriaValue = DivU_CB(i,j,k,iBlock)
             case('divcurv')
                CriteriaValue = DivCurvature_CB(i,j,k,iBlock)
             case('beta')
                CriteriaValue = 2*State_VGB(p_,i,j,k,iBlock) &
                     /sum(FullB_DGB(:,i,j,k,iBlock)**2)
             case('entropy')
                call calc_crit_entropy(i, j, k, iBlock, State_VGB, CritEntropy)
                CriteriaValue = CritEntropy
             case('speed')
                CriteriaValue = norm2(State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) &
                     /State_VGB(Rho_,i,j,k,iBlock))
             case('jy')
                call get_current(i, j, k, iBlock, Current_D)
                CriteriaValue = Current_D(2)
             end select

             if (CriteriaValue > CriteriaMinPic_I(iCriteria) .and. &
                  CriteriaValue < CriteriaMaxPic_I(iCriteria)) then
                iPicStatus_CBI(i,j,k,iBlock,iCriteria) = iPicOn_
             end if

          end do ! end loop criteria
       end do; end do; end do
    end do ! end loop blocks

    ! collect pic crit info
    do k=1,nK; do j=1,nJ; do i=1,nI
       do iBlock=1,nBlock
          IsSatisfyAllCrit = .true.
          do iCriteria=1, nCriteriaPic
             if(iPicStatus_CBI(i,j,k,iBlock,iCriteria)==iPicOff_) &
                  IsSatisfyAllCrit = .false.
          end do
          if(IsSatisfyAllCrit) iStatusPicCrit_CB(i,j,k,iBlock) = iPicOn_
       end do
    end do; end do; end do

    if(iProc==0) write(*,*) "Cleaning temp arrays for PIC criteria..."
    ! Deallocate arrays
    if(allocated(Current_D))         deallocate(Current_D)
    if(allocated(CurrentCrossB_D))   deallocate(CurrentCrossB_D)
    if(allocated(DivU_CB))           deallocate(DivU_CB)
    if(allocated(Ufield_DGB))        deallocate(Ufield_DGB)
    if(allocated(Curvature_DGB))     deallocate(Curvature_DGB)
    if(allocated(DivCurvature_CB))   deallocate(DivCurvature_CB)
    if(allocated(FullB_DGB))         deallocate(FullB_DGB)
    if(allocated(FullB_DG))          deallocate(FullB_DG)
    if(allocated(UnitBfield_DGB))    deallocate(UnitBfield_DGB)
    if(allocated(GradUnitBx_DGB))    deallocate(GradUnitBx_DGB)
    if(allocated(GradUnitBy_DGB))    deallocate(GradUnitBy_DGB)
    if(allocated(GradUnitBz_DGB))    deallocate(GradUnitBz_DGB)

    call test_stop(NameSub, DoTest)
  end subroutine calc_pic_criteria
  !============================================================================
  subroutine calc_crit_jb(i, j, k, iBlock, FullB_DG, CriteriaB1, CritJB)

    use BATL_lib, ONLY: CellSize_DB, MinI, MaxI,&
         MinJ, MaxJ, MinK, MaxK
    use ModCurrent, ONLY: get_current

    integer, intent(in) :: i, j, k, iBlock
    real, intent(in) :: CriteriaB1
    real, intent(in):: FullB_DG(3,MinI:MaxI,MinJ:MaxJ,MinK:MaxK)

    real :: Current, Current_D(3)

    real, intent(out) :: CritJB

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_crit_jb'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub,' is called'

    call get_current(i, j, k, iBlock, Current_D)
    Current = norm2(Current_D)
    CritJB = Current/(norm2(FullB_DG(:,i,j,k))+CriteriaB1)*&
         CellSize_DB(1, iBlock)

  end subroutine calc_crit_jb
  !============================================================================
  subroutine calc_crit_jbperp( &
       i, j, k, iBlock, FullB_DG, CriteriaB1, CritJBperp)

    ! Calculate dx*j/Bperp = dx*j^2/|j x B|

    use BATL_lib, ONLY: CellSize_DB, MinI, MaxI,&
         MinJ, MaxJ, MinK, MaxK
    use ModCurrent, ONLY: get_current
    use ModCoordTransform, ONLY: cross_product

    integer, intent(in) :: i, j, k, iBlock
    real, intent(in) :: CriteriaB1
    real, intent(in):: FullB_DG(3,MinI:MaxI,MinJ:MaxJ,MinK:MaxK)

    real :: Current, Current_D(3), CurrentCrossB_D(3)

    real, intent(out) :: CritJBperp

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_crit_jbperp'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub,' is called'

    call get_current(i, j, k, iBlock, Current_D)
    Current = norm2(Current_D)
    CurrentCrossB_D = cross_product(Current_D, FullB_DG(:,i,j,k))
    CritJBperp = CellSize_DB(1, iBlock) * Current**2 &
         / ( norm2(CurrentCrossB_D) + (Current + 1e-30)*CriteriaB1)

  end subroutine calc_crit_jbperp
  !============================================================================
  subroutine calc_crit_entropy(i, j, k, iBlock, State_VGB, CritEntropy)

    use BATL_lib, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK, MaxBlock
    use ModAdvance, ONLY: nVar, Rho_, p_
    use ModPhysics, ONLY: Gamma

    integer, intent(in) :: i, j, k, iBlock
    real,    intent(in) :: State_VGB(nVar,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,&
         MaxBlock)

    real, intent(out) :: CritEntropy
    !--------------------------------------------------------------------------
    CritEntropy = State_VGB(p_,i,j,k,iBlock) &
         * State_VGB(Rho_,i,j,k,iBlock)**(-Gamma)

  end subroutine calc_crit_entropy
  !============================================================================

end module ModPIC
!==============================================================================
