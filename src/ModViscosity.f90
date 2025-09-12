!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModViscosity

  use BATL_lib, ONLY: &
       test_start, test_stop
  use ModBatsrusUtility, ONLY: stop_mpi
  use omp_lib

  implicit none

  SAVE

  private ! except

  public:: viscosity_read_param
  public:: viscosity_init
  public:: viscosity_clean
  public:: set_visco_factor_face
  public:: set_visco_factor_cell
  public:: get_viscosity_tensor

  ! Use viscosity or not
  logical, public :: UseViscosity=.false.
  !$acc declare create(UseViscosity)

  ! Visosity tensor for each fluid
  real, public, allocatable:: Visco_DDI(:,:,:)
  !$omp threadprivate( Visco_DDI )
  !$acc declare create(Visco_DDI)

  ! Viscosity factor on the faces and in the cell centers
  real, public, allocatable:: &
       ViscoFactor_DF(:,:,:,:), ViscoFactor_C(:,:,:)
  !$omp threadprivate( ViscoFactor_DF, ViscoFactor_C )
  !$acc declare create( ViscoFactor_DF, ViscoFactor_C )

  ! Local variables

  ! Velocity vector for each block and fluid
  real, allocatable:: u_DGI(:,:,:,:,:)
  !$omp threadprivate( u_DGI )

  ! Gradient of velocity centered for faces
  real, allocatable:: GradU_DDI(:,:,:)
  !$omp threadprivate( GradU_DDI )

  real :: ViscoCoeff=0.0, ViscoCoeffSi=0.0

  ! Name/shape of the regions where Visco effect is used
  character(len=20):: StringViscoRegion='none'

  ! Indexes of regions defined with the #REGION commands
  integer, allocatable:: iRegionVisco_I(:)
  !$omp threadprivate( iRegionVisco_I )

  ! Artificial viscosity.
  logical, public :: UseArtificialVisco=.false.
  real,    public :: AlphaVisco, BetaVisco

contains
  !============================================================================

  subroutine viscosity_read_param(NameCommand)

    use ModReadParam, ONLY: read_var
    character(len=*), intent(in):: NameCommand

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'viscosity_read_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(NameCommand)
    case('#VISCOSITY')
       call read_var('UseViscosity',      UseViscosity)
       if(UseViscosity) &
            call read_var('ViscosityCoeffSi',  ViscoCoeffSi)
    case('#ARTIFICIALVISCOSITY')
       call read_var('UseArtificialVisco', UseArtificialVisco)
       if(UseArtificialVisco)call read_var('AlphaVisco', AlphaVisco)
       if(UseArtificialVisco)call read_var('BetaVisco', BetaVisco)
    case('#VISCOSITYREGION')
       call read_var('StringViscosityRegion', StringViscoRegion, &
            IsLowerCase=.true.)
    case default
       call stop_mpi(NameSub//' unknown command='//NameCommand)
    end select

    call test_stop(NameSub, DoTest)
  end subroutine viscosity_read_param
  !============================================================================

  subroutine viscosity_init

    use BATL_lib, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK, nDim, &
         get_region_indexes
    use ModMultiFluid, ONLY: nFluid
    use ModPhysics, ONLY: Si2No_V, UnitX_, UnitT_
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'viscosity_init'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    !$acc update device(UseViscosity)
    if(.not.UseViscosity) RETURN

    ViscoCoeff = ViscoCoeffSi*Si2No_V(UnitX_)**2/Si2No_V(UnitT_)

    !$omp parallel
    if(allocated(u_DGI))     deallocate(u_DGI)
    if(allocated(GradU_DDI)) deallocate(GradU_DDI)
    if(allocated(Visco_DDI)) deallocate(Visco_DDI)

    allocate(u_DGI(nDim,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,nFluid))
    u_DGI = 0.0

    allocate(GradU_DDI(nDim,nDim,nFluid))
    GradU_DDI = 0.0

    allocate(Visco_DDI(nDim,nDim,nFluid))
    Visco_DDI = 0.0

    ! Get signed indexes for viscosity region(s)
    call get_region_indexes(StringViscoRegion, iRegionVisco_I)
    !$omp end parallel

    call test_stop(NameSub, DoTest)
  end subroutine viscosity_init
  !============================================================================

  subroutine viscosity_clean

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'viscosity_clean'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(.not.UseViscosity) RETURN

    !$omp parallel
    if(allocated(u_DGI))          deallocate(u_DGI)
    if(allocated(GradU_DDI))      deallocate(GradU_DDI)
    if(allocated(Visco_DDI))      deallocate(Visco_DDI)
    if(allocated(iRegionVisco_I)) deallocate(iRegionVisco_I)
    !$omp end parallel

    StringViscoRegion = 'none'

    UseViscosity = .false.

    call test_stop(NameSub, DoTest)
  end subroutine viscosity_clean
  !============================================================================

  subroutine set_visco_factor_cell(iBlock)

    use BATL_lib, ONLY: block_inside_regions, nI, nJ, nK

    integer, intent(in):: iBlock

    ! Set the viscosity factor for the cell centers of block iBlock
    ! Also set IsViscoBlock if any of the cells have a non-zero factor.
    logical :: IsViscoBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_visco_factor_cell'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    if(.not.allocated(ViscoFactor_C)) allocate(ViscoFactor_C(nI,nJ,nK))

    if(.not.allocated(iRegionVisco_I))then
       IsViscoBlock = .true.
       ViscoFactor_C = ViscoCoeff
    else
       call block_inside_regions(iRegionVisco_I, iBlock, &
            size(ViscoFactor_C), 'cells', IsViscoBlock, Value_I=ViscoFactor_C)

       ! Nothing to do if the block does not intersect with the Visco region
       if(.not.IsViscoBlock) RETURN

       ! Multiply by ViscoFactorMax
       if(ViscoCoeff /= 1) ViscoFactor_C = ViscoCoeff*ViscoFactor_C
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine set_visco_factor_cell
  !============================================================================
  subroutine set_visco_factor_face(iBlock)

    use BATL_lib, ONLY: block_inside_regions, nDim, nINode, nJNode, nKNode

    integer, intent(in):: iBlock

    ! Set the visco factor for the cell faces of block iBlock
    ! Also set IsViscoBlock if any of the faces have a non-zero factor
    logical :: IsViscoBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_visco_factor_face'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    if(.not.allocated(ViscoFactor_DF)) &
         allocate(ViscoFactor_DF(nDim,nINode,nJNode,nKNode))

    if(.not.allocated(iRegionVisco_I))then
       IsViscoBlock = .true.
       ViscoFactor_DF = ViscoCoeff
    else
       call block_inside_regions(iRegionVisco_I, iBlock, &
            size(ViscoFactor_DF), 'face', IsViscoBlock, Value_I=ViscoFactor_DF)

       ! Nothing to do if the block does not intersect with the Visco region
       if(.not.IsViscoBlock) RETURN

       ! Multiply by ViscoFactorMax
       if(ViscoCoeff /= 1) ViscoFactor_DF = ViscoCoeff*ViscoFactor_DF
    end if

    !$acc update device(ViscoFactor_DF)
    call test_stop(NameSub, DoTest, iBlock)
  end subroutine set_visco_factor_face
  !============================================================================

  subroutine get_viscosity_tensor(iDimFace, iFace, jFace, &
       kFace,iBlockFace,iFluidMin,iFluidMax,ViscoCoeff,IsNewBlockVisco)

    use ModAdvance, ONLY: State_VGB
    use BATL_lib, ONLY: nDim, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, x_, y_, z_
    use ModMultiFluid, ONLY: select_fluid, nFluid, iRho, iRhoUx
    use ModFaceGradient, ONLY: get_face_gradient_field
    use ModMain, ONLY: x_

    integer, intent(in) :: iDimFace, iFace, jFace,kFace,iBlockFace
    integer, intent(in) :: iFluidMin,iFluidMax
    real,    intent(in) :: ViscoCoeff
    logical, intent(inout):: IsNewBlockVisco

    real :: Diag
    real, parameter :: TraceCoeff = 2.0/3.0
    integer :: i,j,k, iFluid
    logical :: IsNewBlock

    character(len=*), parameter:: NameSub = 'get_viscosity_tensor'
    !--------------------------------------------------------------------------
    ! Get velocity vector for the block, only done once per block
    if(IsNewBlockVisco) then
       do iFluid = iFluidMin, iFluidMax
          if(nFluid>1) call select_fluid(iFluid)
          do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
             if(State_VGB(iRho,i,j,k,iBlockFace) > 0.0) then
                u_DGI(:,i,j,k,iFluid) = &
                     State_VGB(iRhoUx:iRhoUx+nDim-1,i,j,k,iBlockFace) / &
                     State_VGB(iRho,i,j,k,iBlockFace)
             else
                u_DGI(:,i,j,k,iFluid) = 0.0
             end if
          end do; end do; end do
       end do
    end if

    IsNewBlock = IsNewBlockVisco
    ! Set face gradient once per fluid
    do iFluid = iFluidMin, iFluidMax
       call get_face_gradient_field(iDimFace, iFace, jFace, kFace, &
            iBlockFace, nDim, IsNewBlockVisco, &
            u_DGI(:,:,:,:,iFluid), GradU_DDI(:,:,iFluid))
       IsNewBlock = IsNewBlockVisco
    end do
    IsNewBlockVisco = .false.

    ! Get the viscosity tensor
    do iFluid = 1, nFluid
       Diag              =        GradU_DDI(x_,x_,iFluid)
       if(nDim > 1) Diag = Diag + GradU_DDI(y_,y_,iFluid)
       if(nDim > 2) Diag = Diag + GradU_DDI(z_,z_,iFluid)
       Diag = -TraceCoeff*Diag

       ! Diagonal
       Visco_DDI(x_,x_,iFluid)             = 2*GradU_DDI(x_,x_,iFluid) + Diag
       if(nDim > 1)Visco_DDI(y_,y_,iFluid) = 2*GradU_DDI(y_,y_,iFluid) + Diag
       if(nDim > 2)Visco_DDI(z_,z_,iFluid) = 2*GradU_DDI(z_,z_,iFluid) + Diag

       ! Off-diagonal terms are symmetrized
       if(nDim > 1)then
          Visco_DDI(x_,y_,iFluid) = &
               GradU_DDI(x_,y_,iFluid) + GradU_DDI(y_,x_,iFluid)
          Visco_DDI(y_,x_,iFluid) = Visco_DDI(x_,y_,iFluid)
       end if
       if(nDim > 2)then
          Visco_DDI(x_,z_,iFluid) = &
               GradU_DDI(x_,z_,iFluid) + GradU_DDI(z_,x_,iFluid)
          Visco_DDI(z_,x_,iFluid) = Visco_DDI(x_,z_,iFluid)
          Visco_DDI(y_,z_,iFluid) = &
               GradU_DDI(y_,z_,iFluid) + GradU_DDI(z_,y_,iFluid)
          Visco_DDI(z_,y_,iFluid) = Visco_DDI(y_,z_,iFluid)
       end if

       Visco_DDI(:,:,iFluid) = ViscoCoeff*Visco_DDI(:,:,iFluid)
    end do

  end subroutine get_viscosity_tensor
  !============================================================================

end module ModViscosity
!==============================================================================
