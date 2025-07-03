!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModElectricField

  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest
  use ModBatsrusUtility, ONLY: stop_mpi

  ! Calculate electric field Efield_DGB from MHD quantities.
  !
  ! Calculate potential and inductive part of the electric field: Epot and Eind
  ! Efield = Epot + Eind
  !
  ! where curl(Epot) = 0 and div(Eind)=0.
  !
  ! This is similar to the projection scheme.
  ! NOTE: For convenience the sign of the potential is reversed.
  !
  ! Epot = grad(Potential), and Potential satisfies the Poisson equation
  !
  ! Laplace(Potential) = div(E)

  use BATL_lib, ONLY: nDim, nI, nJ, nK, nIJK, nG, &
       MinI, MaxI, MinJ, MaxJ, MinK, MaxK, &
       nBlock, MaxBlock, Unused_B, &
       iProc, iComm, message_pass_cell

  use ModAdvance,      ONLY: Efield_DGB
  use ModMain,         ONLY: nStep, UseB
  use ModGeometry,     ONLY: IsBoundary_B, Used_GB
  use ModCellBoundary, ONLY: set_cell_boundary, FloatBC_, GradPotBC_
  use ModCellGradient, ONLY: calc_gradient, calc_divergence
  use ModLinearSolver, ONLY: LinearSolverParamType, solve_linear_multiblock
  use ModVarIndexes,   ONLY: IsMhd
  use ModMultiFluid,   ONLY: UseMultiIon

  implicit none
  SAVE

  private ! except

  public:: get_electric_field           ! Calculate E
  public:: get_electric_field_block     ! Calculate E for 1 block
  public:: get_efield_in_comoving_frame ! Field in frame comoving with ions
  public:: correct_efield_block         ! Transform to global frame (add -UxB)
  public:: calc_div_e               ! Calculate div(E)
  public:: calc_inductive_e         ! Calculate Eind
  public:: get_num_electric_field   ! Calculate numerical E from fluxes

  ! Logical, which controls, if the electric field in the frame of reference
  ! comoving with an average ions, is calculated via the momentum flux (in a
  ! conservative manner) or as J x B force.
  logical, public:: UseJCrossBForce = UseB .and. .not.IsMhd
  !$acc declare create(UseJCrossBForce)

  ! Make Efield available through this module too
  public:: Efield_DGB

  ! Total, potential and inductive electric fields
  real, public, allocatable:: Epot_DGB(:,:,:,:,:)
  real, public, allocatable:: Eind_DGB(:,:,:,:,:)

  ! Electric potential
  real, public, allocatable:: Potential_GB(:,:,:,:)

  ! Divergence of the electric field
  real, public, allocatable:: DivE_CB(:,:,:,:)

  ! local variables
  real, allocatable:: Laplace_C(:,:,:)

  ! Default parameters for the linear solver
  type(LinearSolverParamType):: SolverParam = LinearSolverParamType( &
       .false.,     &! DoPrecond
       'left',      &! TypePrecondSide
       'BILU',      &! TypePrecond
       0.0,         &! PrecondParam (Gustafsson for MBILU)
       'GMRES',     &! TypeKrylov
       'rel',       &! TypeStop
       1e-3,        &! ErrorMax
       200,         &! MaxMatvec
       200,         &! nKrylovVector
       .false.,     &! UseInitialGuess
       -1.0, -1, -1) ! Error, nMatvec, iError (return values)

  ! number of blocks used
  integer:: nBlockUsed

  ! number of unknowns to solve for
  integer:: nVarAll

  ! indicate if we are in the linear stage or not
  logical:: IsLinear = .false.

contains
  !============================================================================

  subroutine get_electric_field

    ! Fill in all cells of Efield_DGB with electric field

    integer:: iBlock
    integer:: nStepLast = -1

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_electric_field'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(nStepLast == nStep) RETURN
    nStepLast = nStep

    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       call get_electric_field_block(iBlock)
    end do

    ! Fill in ghost cells
    call message_pass_cell(3, Efield_DGB)

    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE

       ! Fill outer ghost cells with floating values
       if(IsBoundary_B(iBlock)) &
            call set_cell_boundary(nG, iBlock, 3, Efield_DGB(:,:,:,:,iBlock),&
            iTypeBcIn = FloatBC_)
    end do

    call test_stop(NameSub, DoTest)
  end subroutine get_electric_field
  !============================================================================

  subroutine get_electric_field_block(iBlock, DoHallCurrentIn)

    ! Fill in all cells of Efield_DGB with electric field for block iBlock
    ! Assumes that ghost cells are set
    ! This will NOT work for Hall MHD

    use ModVarIndexes, ONLY: Bx_, Bz_, Ex_, Ez_
    use ModAdvance,    ONLY: State_VGB, UseEfield
    use ModB0,         ONLY: UseB0, B0_DGB
    use ModCoordTransform, ONLY: cross_product
    use ModMultiFluid,     ONLY: iRhoUxIon_I, iRhoUyIon_I, iRhoUzIon_I, &
         iRhoIon_I, ChargePerMass_I
    use ModCurrent,        ONLY: get_current
    use BATL_lib,          ONLY: x_, y_, z_

    integer, intent(in):: iBlock
    logical, optional, intent(in) :: DoHallCurrentIn

    integer :: i, j, k, iMin, iMax, jMin, jMax, kMin, kMax
    real    :: b_D(3), uPlus_D(3), uElec_D(3), Current_D(3), InvElectronDens
    logical :: DoHallCurrent
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_electric_field_block'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    if(.not.allocated(Efield_DGB))then
       allocate(Efield_DGB(3,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
       Efield_DGB = 0.0
    end if

    if(UseEfield)then
       Efield_DGB(:,:,:,:,iBlock) = State_VGB(Ex_:Ez_,:,:,:,iBlock)
       RETURN
    end if
    DoHallCurrent = .false.
    if (present(DoHallCurrentIn)) DoHallCurrent = DoHallCurrentIn

    if (DoHallCurrent) then
       ! get_current cannot be called in ghost cells
       ! Only apply DoHallCurrent in write_plot_common at this moment, which
       ! hopefully can give a better electric field for plotting.
       iMin=1;    iMax=nI;   jMin=1;    jMax=nJ;   kMin=1;    kMax=nK
    else
       ! some functions may still need the electric field in ghost cells
       iMin=MinI; iMax=MaxI; jMin=MinJ; jMax=MaxJ; kMin=MinK; kMax=MaxK

       ! set the current_D = 0.0 if not considering the Hall current
       Current_D = 0.0
    end if

    do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
       if(.not. Used_GB(i,j,k,iBlock))then
          Efield_DGB(:,i,j,k,iBlock) = 0.0
          CYCLE
       end if

       ! Ideal MHD case
       b_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
       if(UseB0) b_D = b_D + B0_DGB(:,i,j,k,iBlock)

       ! inv of electron charge density = 1/(e*ne)
       InvElectronDens = 1.0/ &
            sum(ChargePerMass_I*State_VGB(iRhoIon_I,i,j,k,iBlock))

       ! charge average ion velocity
       uPlus_D(x_) = InvElectronDens*sum(ChargePerMass_I &
            *State_VGB(iRhoUxIon_I,i,j,k,iBlock))
       uPlus_D(y_) = InvElectronDens*sum(ChargePerMass_I &
            *State_VGB(iRhoUyIon_I,i,j,k,iBlock))
       uPlus_D(z_) = InvElectronDens*sum(ChargePerMass_I &
            *State_VGB(iRhoUzIon_I,i,j,k,iBlock))

       if (DoHallCurrent) call get_current(i,j,k,iBlock,Current_D)

       uElec_D = uPlus_D - Current_D*InvElectronDens

       ! E = - ue x B
       Efield_DGB(:,i,j,k,iBlock) = -cross_product(uElec_D, b_D)

    end do; end do; end do
    call test_stop(NameSub, DoTest, iBlock)
  end subroutine get_electric_field_block
  !============================================================================
  subroutine get_efield_in_comoving_frame(iBlock)

    use ModAdvance, ONLY: MhdFlux_VX, MhdFlux_VY, MhdFlux_VZ, SourceMhd_VC, &
         State_VGB, bCrossArea_DX, bCrossArea_DY, bCrossArea_DZ
    use ModMain,    ONLY: MaxDim, UseB0
    use ModB0,      ONLY: B0_DGB, UseCurlB0, CurlB0_DC
    use ModCoordTransform, ONLY: cross_product
    use ModMultiFluid,     ONLY: iRhoIon_I, ChargePerMass_I, nIonFluid
    use BATL_lib, ONLY: CellVolume_GB
    use ModVarIndexes, ONLY: Bx_, Bz_, nVar

    integer, intent(in):: iBlock

    integer:: i, j, k
    real :: State_V(nVar), vInv
    real :: ChargeDens_I(nIonFluid)
    real, dimension(MaxDim) :: Force_D, FullB_D, Current_D
    real :: InvElectronDens
    logical :: DoTestCell

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_efield_in_comoving_frame'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    Efield_DGB(:,:,:,:,iBlock) = 0.0

    if(UseJCrossBForce)then
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE
          DoTestCell = DoTest .and. iTest==i .and. jTest==j .and. kTest==k

          vInv = 1/CellVolume_GB(i,j,k,iBlock)
          State_V = State_VGB(:,i,j,k,iBlock)
          ChargeDens_I = ChargePerMass_I*State_V(iRhoIon_I)
          if(sum(ChargeDens_I) <= 0.0) then
             write(*,*)'ChargePerMass_I='   , ChargePerMass_I
             write(*,*)'State_V(iRhoIon_I)=', State_V(iRhoIon_I)
             call stop_mpi('negative electron density')
          end if
          InvElectronDens = 1.0/sum(ChargeDens_I)

          ! Calculate Lorentz force = J x B
          Current_D = (bCrossArea_DX(:,i+1,j,k) - bCrossArea_DX(:,i,j,k))
          if(nJ>1) Current_D = Current_D + &
               (bCrossArea_DY(:,i,j+1,k) - bCrossArea_DY(:,i,j,k))
          if(nK>1) Current_D = Current_D + &
               (bCrossArea_DZ(:,i,j,k+1) - bCrossArea_DZ(:,i,j,k))
          Current_D = Current_D*vInv

          if(DoTestCell) then
             write(*,'(2a,15es16.8)') NameSub, ': Current_D    = ', Current_D
          end if

          if(UseCurlB0) Current_D = Current_D + CurlB0_DC(:,i,j,k)

          FullB_D = State_V(Bx_:Bz_)
          if(UseB0) FullB_D =  FullB_D + B0_DGB(:,i,j,k,iBlock)

          ! Lorentz force: J x B
          Force_D = cross_product(Current_D, FullB_D)

          if(DoTestCell) write(*,'(2a,15es16.8)') NameSub,': Force_D      =', &
               Force_D
          Force_D = Force_D &
               + vInv*(MhdFlux_VX(:,i,j,k)  - MhdFlux_VX(:,i+1,j,k))
          if(nDim > 1) Force_D = Force_D &
               + vInv*(MhdFlux_VY(:,i,j,k)  - MhdFlux_VY(:,i,j+1,k))
          if(nDim > 2) Force_D = Force_D &
               + vInv*(MhdFlux_VZ(:,i,j,k)  - MhdFlux_VZ(:,i,j,k+1))
          if(DoTestCell)write(*,'(2a,15es16.8)') &
               NameSub,': after grad Pwave, Force_D =', Force_D
          Efield_DGB(:,i,j,k,iBlock) = InvElectronDens*Force_D
       end do; end do; end do
    else
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not. Used_GB(i,j,k,iBlock))CYCLE
          Efield_DGB(:,i,j,k,iBlock) = SourceMhd_VC(:,i,j,k) +  &
               ( MhdFlux_VX(:,i,j,k)  - MhdFlux_VX(:,i+1,j,k)     &
               + MhdFlux_VY(:,i,j,k)  - MhdFlux_VY(:,i,j+1,k)     &
               + MhdFlux_VZ(:,i,j,k)  - MhdFlux_VZ(:,i,j,k+1)  )  &
               /CellVolume_GB(i,j,k,iBlock)
       end do; end do; end do
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine get_efield_in_comoving_frame
  !============================================================================
  subroutine correct_efield_block(iBlock)

    ! Recalculates electric field, which has been obtained
    ! with get_efield_in_comoving_frame, to a global frame
    ! of reference. To achive this, -v x B is added, v being a
    ! the ion velocity (averaged charge velocity).

    use ModVarIndexes, ONLY: Bx_, Bz_
    use ModAdvance,    ONLY: State_VGB
    use ModB0,         ONLY: UseB0, B0_DGB
    use ModCoordTransform, ONLY: cross_product
    use ModMultiFluid,     ONLY: iRhoUxIon_I, iRhoUyIon_I, iRhoUzIon_I, &
         iRhoIon_I, ChargePerMass_I
    use BATL_lib,          ONLY: x_, y_, z_

    integer, intent(in):: iBlock

    integer :: i, j, k
    real    :: b_D(3), uPlus_D(3), InvElectronDens
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'correct_efield_block'
    !--------------------------------------------------------------------------

    do k = 1, nK; do j = 1, nJ; do i = 1, nK
       if(.not. Used_GB(i,j,k,iBlock))CYCLE

       b_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
       if(UseB0) b_D = b_D + B0_DGB(:,i,j,k,iBlock)

       ! inv of electron charge density = 1/(e*ne)
       InvElectronDens = 1.0/ &
            sum(ChargePerMass_I*State_VGB(iRhoIon_I,i,j,k,iBlock))

       ! charge average ion velocity
       uPlus_D(x_) = InvElectronDens*sum(ChargePerMass_I &
            *State_VGB(iRhoUxIon_I,i,j,k,iBlock))
       uPlus_D(y_) = InvElectronDens*sum(ChargePerMass_I &
            *State_VGB(iRhoUyIon_I,i,j,k,iBlock))
       uPlus_D(z_) = InvElectronDens*sum(ChargePerMass_I &
            *State_VGB(iRhoUzIon_I,i,j,k,iBlock))

       ! E = E(in comoving frame)- u x B
       Efield_DGB(:,i,j,k,iBlock) = Efield_DGB(:,i,j,k,iBlock) &
            -cross_product(uPlus_D, b_D)

    end do; end do; end do
    call test_stop(NameSub, DoTest, iBlock)
  end subroutine correct_efield_block
  !============================================================================
  subroutine correct_efield
    integer :: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'correct_efield'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       call correct_efield_block(iBlock)
    end do
    ! Fill in ghost cells
    call message_pass_cell(3, Efield_DGB, nProlongOrderIn=1)
    call test_stop(NameSub, DoTest)
  end subroutine correct_efield
  !============================================================================
  subroutine calc_inductive_e

    ! Calculate the inductive part of the electric field Eind

    integer:: iBlock
    integer:: nStepLast = -1

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_inductive_e'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub, ' starting'

    if(nStepLast == nStep) RETURN
    nStepLast = nStep

    nBlockUsed = nBlock - count(Unused_B(1:nBlock))
    nVarAll    = nBlockUsed*nIJK

    call get_electric_field

    call calc_div_e

    ! Calculate Potential_GB from the Poisson equation
    ! fill ghost cells at the end
    call calc_potential

    if(.not.allocated(Eind_DGB)) &
         allocate(Eind_DGB(3,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))

    ! Calculate Epot
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE

       ! Set outer boundary ghost cells with total E field
       if(IsBoundary_B(iBlock)) &
            Epot_DGB(:,:,:,:,iBlock) = Efield_DGB(:,:,:,:,iBlock)

       ! Epot = grad(Potential)
       call calc_gradient(iBlock, Potential_GB(:,:,:,iBlock), &
            nG, Epot_DGB(:,:,:,:,iBlock), UseBodyCellIn=.true.)
    end do

    ! Fill in ghost cells
    call message_pass_cell(3,Epot_DGB)

    ! Calculate Epot and Eind
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       ! Eind = E - Epot
       Eind_DGB(:,:,:,:,iBlock) = Efield_DGB(:,:,:,:,iBlock) - &
            Epot_DGB(:,:,:,:,iBlock)
    end do

    if(DoTest)write(*,*) NameSub, ' finished'

    call test_stop(NameSub, DoTest)
  end subroutine calc_inductive_e
  !============================================================================
  subroutine calc_div_e

    integer:: iBlock
    integer:: nStepLast = -1

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_div_e'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(nStepLast == nStep) RETURN
    nStepLast = nStep

    if(allocated(DivE_CB)) deallocate(DivE_CB)
    allocate(DivE_CB(nI,nJ,nK,nBlock))

    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE

       ! Calculate DivE for this block (it has no ghost cells: nG=0)
       call calc_divergence(iBlock, Efield_DGB(:,:,:,:,iBlock), &
            0, DivE_CB(:,:,:,iBlock), UseBodyCellIn=.true.)
    end do

    call test_stop(NameSub, DoTest)
  end subroutine calc_div_e
  !============================================================================
  subroutine calc_potential

    real, allocatable:: Rhs_I(:), Potential_I(:)

    integer:: iBlock, i, j, k, n

    integer:: nStepLast = -1

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_potential'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub,' starting at nStep, nStepLast=', &
         nStep, nStepLast

    if(nStepLast == nStep) RETURN
    nStepLast = nStep

    if(.not.allocated(Potential_GB)) &
         allocate(Potential_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))

    if(.not.allocated(Epot_DGB)) &
         allocate(Epot_DGB(nDim,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))

    ! Make potential zero in unused cells
    Potential_GB(:,:,:,1:nBlock) = 0.0

    ! Make Epot = Efield in outer boundary ghost cells
    Epot_DGB(:,:,:,:,1:nBlock) = Efield_DGB(:,:,:,:,1:nBlock)

    allocate(Rhs_I(nVarAll), Potential_I(nVarAll))

    ! Substitute 0 into the Laplace operator WITH boundary conditions
    ! to get the RHS
    Potential_I = 0.0
    IsLinear = .false.
    call matvec_inductive_e(Potential_I, Rhs_I, nVarAll)
    Rhs_I = -Rhs_I

    if(DoTest)write(*,*) NameSub,' min,max(BC Rhs)=', &
         minval(Rhs_I), maxval(Rhs_I)

    ! Add div(E) to the RHS
    n = 0
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          n = n + 1
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE
          Rhs_I(n) = Rhs_I(n) + DivE_CB(i,j,k,iBlock)
       end do; end do; end do
    end do

    if(DoTest)write(*,*) NameSub,' min,max(Full Rhs)=', &
         minval(Rhs_I), maxval(Rhs_I)

    ! Start the linear solve stage
    IsLinear = .true.

    ! Make potential and electric field 0 in unused and boundary cells
    Potential_GB(:,:,:,1:nBlock) = 0.0
    Epot_DGB(:,:,:,:,1:nBlock)   = 0.0

    ! solve Poisson equation Laplace(Potential) = div(E)
    call solve_linear_multiblock( SolverParam, &
         1, nDim, nI, nJ, nK, 2*nDim+1, nBlockUsed, iComm, &
         matvec_inductive_e, Rhs_I, Potential_I, DoTest)

    if(SolverParam%iError /= 0 .and. iProc == 0) &
         write(*,*) NameSub,' failed in ModElectricField'

    if(DoTest)write(*,*) NameSub,' min,max(Potential_I)=', &
          minval(Potential_I), maxval(Potential_I)

    ! Put solution Potential_I into Potential_GB
    n = 0
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          n = n + 1
          if(Used_GB(i,j,k,iBlock)) &
               Potential_GB(i,j,k,iBlock) = Potential_I(n)
       end do; end do; end do
    end do

    ! Fill in ghost cells and boundary cells using true BCs
    IsLinear = .false.
    call bound_potential

    deallocate(Rhs_I, Potential_I)

    if(DoTest)write(*,*) NameSub,' min,max(Potential_GB)=', &
          minval(Potential_GB(:,:,:,1:nBlock)), &
          maxval(Potential_GB(:,:,:,1:nBlock))

    call test_stop(NameSub, DoTest)
  end subroutine calc_potential
  !============================================================================
  subroutine bound_potential

    use ModGeometry,   ONLY: IsBody_B, r_GB
    use ModPhysics,    ONLY: rBody
    use ModIeCoupling, ONLY: get_ie_potential
    use BATL_lib,      ONLY: Xyz_DGB

    ! Fill in ghost cells and boundary cells for the potential

    integer:: iBlock, i, j, k
    real:: rInside
    integer:: iTypeBc
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'bound_potential'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    ! Fill in ghost cells for Potential_GB
    call message_pass_cell(Potential_GB)

    if(IsLinear)then
       ! In the linear stage use float BC that corresponds to E_normal=0
       iTypeBc = FloatBC_
    else
       ! Apply gradient(potential) = Enormal BC
       iTypeBc = GradPotBC_
    end if

    ! Fill outer ghost cells
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE

       if(IsBoundary_B(iBlock)) &
            call set_cell_boundary(nG, iBlock, 1, Potential_GB(:,:,:,iBlock),&
            iTypeBcIn = iTypeBc)
    end do

    if(IsLinear) RETURN

    ! Fill in inner boundary cells with ionosphere potential
    rInside = 1.01
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       if(.not.IsBody_B(iBLOCK)) CYCLE
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          if(r_GB(i,j,k,iBlock) > rBody) CYCLE
          if(r_GB(i,j,k,iBlock) < rInside) CYCLE
          call get_ie_potential(Xyz_DGB(:,i,j,k,iBlock), &
               Potential_GB(i,j,k,iBlock))
       end do; end do; end do
    end do

    call test_stop(NameSub, DoTest)
  end subroutine bound_potential
  !============================================================================

  subroutine matvec_inductive_e(x_I, y_I, MaxN)

    ! Calculate y = Laplace(x)

    integer, intent(in):: MaxN
    real,    intent(in) :: x_I(MaxN)
    real,    intent(out):: y_I(MaxN)

    integer:: iBlock, i, j, k, n
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'matvec_inductive_e'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(.not.allocated(Laplace_C)) allocate(Laplace_C(nI,nJ,nK))

    ! x_I -> Potential_GB
    n = 0
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          n = n + 1
          if(Used_GB(i,j,k,iBlock)) Potential_GB(i,j,k,iBlock) = x_I(n)
       end do; end do; end do
    end do

    ! Boundary cells are filled in with zero
    call bound_potential

    ! Calculate gradient of potential
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       call calc_gradient(iBlock, Potential_GB(:,:,:,iBlock), &
            nG, Epot_DGB(:,:,:,:,iBlock), UseBodyCellIn=.true.)
    end do

    ! Fill in ghost cells
    call message_pass_cell(3,Epot_DGB)

    ! Calculate Laplace_C and copy it into y_I
    n = 0
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE

       call calc_divergence(iBlock, Epot_DGB(:,:,:,:,iBlock), 0, Laplace_C, &
            UseBodyCellIn=.true.)

       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          n = n + 1
          if(Used_GB(i,j,k,iBlock))then
             y_I(n) = Laplace_C(i,j,k)
          else
             y_I(n) = 0.0
          end if
       end do; end do; end do
    end do

    call test_stop(NameSub, DoTest)
  end subroutine matvec_inductive_e
  !============================================================================

  subroutine get_num_electric_field(iBlock)

    ! Calculate the total electric field which includes numerical resistivity
    ! This estimate averages the numerical fluxes to the cell centers
    ! for sake of simplicity.

    use ModSize,       ONLY: nI, nJ, nK
    use ModVarIndexes, ONLY: Bx_,By_,Bz_
    use ModAdvance,    ONLY: &
         Flux_VXI, Flux_VYI, Flux_VZI, ExNum_CB, EyNum_CB, EzNum_CB
    use BATL_lib,      ONLY: CellFace_DB

    integer, intent(in) :: iBlock
    integer:: iGang

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_num_electric_field'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    iGang = 1
    ! E_x=(fy+fy-fz-fz)/4
    ExNum_CB(:,:,:,iBlock) = - 0.25*(                                    &
         ( Flux_VYI(Bz_,1:nI,1:nJ  ,1:nK  ,iGang)                             &
         + Flux_VYI(Bz_,1:nI,2:nJ+1,1:nK  ,iGang) ) / CellFace_DB(2,iBlock) - &
         ( Flux_VZI(By_,1:nI,1:nJ  ,1:nK  ,iGang)                             &
         + Flux_VZI(By_,1:nI,1:nJ  ,2:nK+1,iGang) ) / CellFace_DB(3,iBlock) )

    ! E_y=(fz+fz-fx-fx)/4
    EyNum_CB(:,:,:,iBlock) = - 0.25*(                                    &
         ( Flux_VZI(Bx_,1:nI  ,1:nJ,1:nK  ,iGang)                             &
         + Flux_VZI(Bx_,1:nI  ,1:nJ,2:nK+1,iGang) ) / CellFace_DB(3,iBlock) - &
         ( Flux_VXI(Bz_,1:nI  ,1:nJ,1:nK  ,iGang)                             &
         + Flux_VXI(Bz_,2:nI+1,1:nJ,1:nK  ,iGang) ) / CellFace_DB(1,iBlock) )

    ! E_z=(fx+fx-fy-fy)/4
    EzNum_CB(:,:,:,iBlock) = - 0.25*(                                    &
         ( Flux_VXI(By_,1:nI  ,1:nJ  ,1:nK,iGang)                             &
         + Flux_VXI(By_,2:nI+1,1:nJ  ,1:nK,iGang) ) / CellFace_DB(1,iBlock) - &
         ( Flux_VYI(Bx_,1:nI  ,1:nJ  ,1:nK,iGang)                             &
         + Flux_VYI(Bx_,1:nI  ,2:nJ+1,1:nK,iGang) ) / CellFace_DB(2,iBlock))

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine get_num_electric_field
  !============================================================================

end module ModElectricField
!==============================================================================

