!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModSemiImplicit

  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest, &
       iBlockTest, iProcTest, iVarTest, iProc, iComm, nDim
  use BATL_size, ONLY: nGang
  use ModBatsrusUtility, ONLY: error_report, stop_mpi
  use ModSemiImplVar
  use ModImplicit, ONLY: nDiagSemi
  use ModConserveFlux, ONLY: DoConserveFlux
  use ModLinearSolver, ONLY: LinearSolverParamType
  use ModMain, ONLY: iNewDecomposition

  implicit none
  save

  private ! except

  public:: read_semi_impl_param  ! Read parameters
  public:: init_mod_semi_impl    ! Initialize variables
  public:: clean_mod_semi_impl   ! Deallocate variables
  public:: advance_semi_impl     ! Advance semi implicit operator

  ! The index of the variable currently being solved for
  integer, public:: iVarSemi

  ! Default parameters for the semi-implicit linear solver
  type(LinearSolverParamType), public:: SemiParam = LinearSolverParamType( &
       .true.,      &! DoPrecond
       'left',      &! TypePrecondSide
       'BILU',      &! TypePrecond
       0.0,         &! PrecondParam (Gustafsson for MBILU)
       'GMRES',     &! TypeKrylov
       'rel',       &! TypeStop
       1e-3,        &! ErrorMax
       100,         &! MaxMatvec
       100,         &! nKrylovVector
       .false.,     &! UseInitialGuess
       -1.0, -1, -1) ! Error, nMatvec, iError (return values)

  ! Linear arrays for RHS, unknowns, pointwise Jacobi preconditioner
  real, allocatable:: Rhs_I(:), x_I(:), JacobiPrec_I(:)
  !$acc declare create(Rhs_I, x_I)

  ! Index of the test block
  integer:: iBlockSemiTest = 1

contains
  !============================================================================
  subroutine read_semi_impl_param(NameCommand)

    use ModReadParam, ONLY: read_var
    use ModLinearSolver, ONLY: &
         Jacobi_, BlockJacobi_, GaussSeidel_, Dilu_, Bilu_, Bilu1_

    character(len=*), intent(in) :: NameCommand

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_semi_impl_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(NameCommand)
    case('#SEMIIMPLICIT', '#SEMIIMPL')
       call read_var('UseSemiImplicit', UseSemiImplicit)
       if(UseSemiImplicit) call read_var('TypeSemiImplicit', TypeSemiImplicit)
       !$acc update device(TypeSemiImplicit)

    case("#SEMICOEFF", "#SEMIIMPLCOEFF", "#SEMIIMPLICITCOEFF")
       call read_var('SemiImplCoeff', SemiImplCoeff)
       !$acc update device(SemiImplCoeff)

    case("#SEMIPRECONDITIONER", "#SEMIPRECOND")
       call read_var('DoPrecond',   SemiParam%DoPrecond)
       if(SemiParam%DoPrecond)then
          call read_var('TypePrecond', SemiParam%TypePrecond, &
               IsUpperCase=.true.)
          select case(SemiParam%TypePrecond)
          case('HYPRE')
          case('JACOBI')
             SemiParam%PrecondParam = Jacobi_
          case('BLOCKJACOBI')
             SemiParam%PrecondParam = BlockJacobi_
          case('GS')
             SemiParam%PrecondParam = GaussSeidel_
          case('DILU')
             SemiParam%PrecondParam = Dilu_
          case('BILU1')
             SemiParam%PrecondParam = Bilu1_
          case('BILU')
             SemiParam%PrecondParam = Bilu_
          case('MBILU')
             call read_var('GustafssonPar', SemiParam%PrecondParam)
             SemiParam%PrecondParam = -SemiParam%PrecondParam
          case default
             call stop_mpi(NameSub//' invalid TypePrecond='// &
                  SemiParam%TypePrecond)
          end select
       end if
    case("#SEMIKRYLOV")
       call read_var('TypeKrylov', SemiParam%TypeKrylov, IsUpperCase=.true.)
       call read_var('ErrorMax', SemiParam%ErrorMax)
       call read_var('MaxMatvec', SemiParam%MaxMatvec)
       SemiParam%nKrylovVector = SemiParam%MaxMatvec

    case('#SEMIKRYLOVSIZE')
       call read_var('nKrylovVector', SemiParam%nKrylovVector)
    case default
       call stop_mpi(NameSub//' invalid NameCommand='//NameCommand)
    end select

    call test_stop(NameSub, DoTest)
  end subroutine read_semi_impl_param
  !============================================================================
  subroutine init_mod_semi_impl

    use ModLinearSolver, ONLY: bilu_
    use BATL_lib, ONLY: MaxDim, nI, nJ, nK, nIJK, &
         MinI, MaxI, MinJ, MaxJ, MinK, MaxK, MaxBlock
    use ModVarIndexes, ONLY: nWave
    use ModResistivity, ONLY: BxImpl_, UseResistivity
    use ModHallResist, ONLY: UseHallResist

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_mod_semi_impl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(allocated(SemiAll_VCB)) RETURN

    allocate(iBlockFromSemi_B(MaxBlock))

    nVectorSemi = 0
    if(allocated(iVectorSemi_I)) deallocate(iVectorSemi_I)

    select case(TypeSemiImplicit)
    case('radiation')
       ! Radiative transfer with point implicit temperature: I_w
       nVarSemiAll = nWave
       if(nWave > 1) UseSplitSemiImplicit = .true.
    case('cond', 'parcond')
       ! Heat conduction: T
       nVarSemiAll = 1
    case('radcond')
       ! Radiative transfer and heat conduction: I_w and T
       nVarSemiAll = nWave + 1
       UseSplitSemiImplicit = .true.
    case('resistivity', 'resist', 'resisthall')
       ! (Hall) resistivity: magnetic field
       UseSemiHallResist  = UseHallResist .and.TypeSemiImplicit /= 'resist'
       UseSemiResistivity = UseResistivity.and.TypeSemiImplicit /= 'resisthall'
       nVarSemiAll = MaxDim
       nVectorSemi = 1
       allocate(iVectorSemi_I(nVectorSemi))
       iVectorSemi_I(1) = BxImpl_
    case default
       call stop_mpi(NameSub//': nVarSemiAll unknown for'//TypeSemiImplicit)
    end select

    if(UseSplitSemiImplicit)then
       ! Split semi-implicit scheme solves 1 implicit variable at a time
       nVarSemi = 1
    else
       ! Unsplit (semi-)implicit scheme solves all impl. variables together
       nVarSemi = nVarSemiAll
    end if
    ! Set range of (semi-)implicit variables for unsplit scheme.
    ! For split scheme this will be overwritten.
    iVarSemiMin = 1
    iVarSemiMax = nVarSemi

    ! Fix preconditioner type from DILU to BILU for a single variable
    ! because BILU is optimized for scalar equation.
    ! Fix from BILU1 to BILU for multiple variables,
    ! because BILU1 is only implemented for scalar equation.
    if( nVarSemi == 1 .and. SemiParam%TypePrecond == 'DILU' .or. &
         nVarSemi > 1 .and. SemiParam%TypePrecond == 'BILU1')then
       SemiParam%TypePrecond  = 'BILU'
       SemiParam%PrecondParam = Bilu_
    end if

    ! Number of dimensions for the ILU preconditioner
    nDiagSemi = 2*nDim + 1
    if(SemiParam%TypePrecond == 'BILU1') nDiagSemi = 3

    if(nVarSemi > 1 .and. SemiParam%TypePrecond == 'HYPRE' .and. iProc==0) &
         call stop_mpi( &
         'HYPRE preconditioner requires split semi-implicit scheme!')

    allocate(SemiAll_VCB(nVarSemiAll,nI,nJ,nK,MaxBlock))
    allocate(NewSemiAll_VCB(nVarSemiAll,nI,nJ,nK,MaxBlock))
    allocate(DconsDsemiAll_VCB(nVarSemiAll,nI,nJ,nK,MaxBlock))

    allocate(SemiState_VGB(nVarSemi,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
    allocate(SemiStateTmp_VGI(nVarSemi,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,nGang))
    allocate(DcoordDxyz_DDFDI(MaxDim,MaxDim,1:nI+1,1:nJ+1,1:nK+1,MaxDim,nGang))
    allocate(TransGrad_DDGI(MaxDim,MaxDim,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,nGang))

    allocate(ResSemi_VCB(nVarSemi,nI,nJ,nK,MaxBlock))

    allocate(JacSemi_VVCIB(nVarSemi,nVarSemi,nI,nJ,nK,nDiagSemi,MaxBlock))

    allocate(DeltaSemiAll_VCB(nVarSemiAll,nI,nJ,nK,MaxBlock))

    ! Variables for the flux correction
    if( (TypeSemiImplicit(1:3) /= 'rad' .and. TypeSemiImplicit /= 'cond') &
         .or. UseAccurateRadiation)then
       allocate( &
            FluxImpl_VXB(nVarSemi,nJ,nK,2,MaxBlock), &
            FluxImpl_VYB(nVarSemi,nI,nK,2,MaxBlock), &
            FluxImpl_VZB(nVarSemi,nI,nJ,2,MaxBlock) )
       FluxImpl_VXB = 0.0
       FluxImpl_VYB = 0.0
       FluxImpl_VZB = 0.0
    end if

    ! Linear arrays
    allocate(Rhs_I(nVarSemi*nIJK*MaxBlock))
    allocate(x_I(nVarSemi*nIJK*MaxBlock))
    if(SemiParam%TypePrecond == 'JACOBI')then
       allocate(JacobiPrec_I(nVarSemi*nIJK*MaxBlock))
    else
       allocate(JacobiPrec_I(1))
    end if

    !$acc update device(iVarSemiMin, iVarSemiMax)
    !$acc update device(nVarSemiAll, nVarSemi, nDiagSemi)
    !$acc update device(UseSplitSemiImplicit)
    call test_stop(NameSub, DoTest)
  end subroutine init_mod_semi_impl
  !============================================================================
  subroutine clean_mod_semi_impl

    ! Deallocate all variables

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'clean_mod_semi_impl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(allocated(iBlockFromSemi_B))  deallocate(iBlockFromSemi_B)
    if(allocated(SemiAll_VCB))       deallocate(SemiAll_VCB)
    if(allocated(NewSemiAll_VCB))    deallocate(NewSemiAll_VCB)
    if(allocated(DeltaSemiAll_VCB))  deallocate(DeltaSemiAll_VCB)
    if(allocated(DconsDsemiAll_VCB)) deallocate(DconsDsemiAll_VCB)
    if(allocated(SemiState_VGB))     deallocate(SemiState_VGB)
    if(allocated(ResSemi_VCB))       deallocate(ResSemi_VCB)
    if(allocated(JacSemi_VVCIB))     deallocate(JacSemi_VVCIB)
    if(allocated(FluxImpl_VXB))      deallocate(FluxImpl_VXB)
    if(allocated(FluxImpl_VYB))      deallocate(FluxImpl_VYB)
    if(allocated(FluxImpl_VZB))      deallocate(FluxImpl_VZB)
    if(allocated(Rhs_I))             deallocate(Rhs_I)
    if(allocated(x_I))               deallocate(x_I)
    if(allocated(JacobiPrec_I))      deallocate(JacobiPrec_I)
    if(allocated(iVectorSemi_I))     deallocate(iVectorSemi_I)

    call test_stop(NameSub, DoTest)
  end subroutine clean_mod_semi_impl
  !============================================================================
  subroutine advance_semi_impl

    ! Advance semi-implicit terms

    use ModMain, ONLY: IsTimeAccurate
    use ModAdvance, ONLY: DoFixAxis, State_VGB, DtMax_CB
    use ModB0, ONLY: B0_DGB
    use ModCoarseAxis, ONLY: UseCoarseAxis, coarsen_axis_cells
    use ModImplHypre, ONLY: hypre_initialize, hypre_preconditioner
    use ModLinearSolver, ONLY: solve_linear_multiblock
    use ModMessagePass, ONLY: exchange_messages
    use ModRadDiffusion, ONLY: &
         get_impl_rad_diff_state, set_rad_diff_range, update_impl_rad_diff
    use ModHeatConduction, ONLY: &
         get_impl_heat_cond_state, update_impl_heat_cond
    use ModResistivity, ONLY: &
         get_impl_resistivity_state, update_impl_resistivity
    use ModFieldLineThread, ONLY: UseFieldLineThreads, advance_threads, Heat_
    use ModFixAxisCells, ONLY: fix_axis_cells
    use BATL_lib, ONLY: nDim, nI, nJ, nK, nBlock, Used_GB, Unused_B, nIJK
    use ModEnergy, ONLY: limit_pressure
    ! use omp_lib

    integer :: iBlockSemi, iBlock, iError1, i, j, k, iVar, n, n0

    logical :: DoTestKrylov

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'advance_semi_impl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    call test_start('krylov', DoTestKrylov)

    call timing_start(NameSub)

    if(DoTest) write(*,*)NameSub,' starting with test var, B0=', &
         State_VGB(iVarTest,iTest,jTest,kTest,iBlockTest), &
         B0_DGB(:,iTest,jTest,kTest,iBlockTest)

    if(TypeSemiImplicit(1:6) /= 'resist')then
       ! All used blocks are solved for with the semi-implicit scheme
       ! except for (Hall) resistivity
       nBlockSemi = 0
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          nBlockSemi = nBlockSemi + 1
          iBlockFromSemi_B(nBlockSemi) = iBlock
       end do
       !$acc update device(iBlockFromSemi_B, nBlockSemi)
    else
       ! For (Hall) resistivity the number of semi-implicit blocks will be
       ! set in get_impl_resistivity_state, so initialize up to nBlock
       DconsDsemiAll_VCB(:,:,:,:,1:nBlock) = 1.0
    end if

    ! Get current state and dCons/dSemi derivatives
    select case(TypeSemiImplicit)
    case('radiation', 'radcond', 'cond')
       call get_impl_rad_diff_state(SemiAll_VCB, DconsDsemiAll_VCB, &
            DeltaSemiAll_VCB=DeltaSemiAll_VCB)
    case('parcond')
       call get_impl_heat_cond_state
    case('resistivity','resist','resisthall')
       call get_impl_resistivity_state(SemiAll_VCB)
    case default
       call stop_mpi(NameSub//': no get_impl_state implemented for' &
            //TypeSemiImplicit)
    end select

    call check_nan_semi

    ! Set the test block
    if(iProc == iProcTest)then
       do iBlockSemi = 1, nBlockSemi
          if(iBlockTest == iBlockFromSemi_B(iBlockSemi))then
             iBlockSemiTest = iBlockSemi
             EXIT
          end if
       end do
    end if

    ! Re-initialize HYPRE preconditioner
    if(SemiParam%TypePrecond == 'HYPRE') call hypre_initialize

    call update_block_jacobian_face

    ! For nVarSemi = 1,  loop through all semi-implicit variables one-by-one
    ! For nVarSemi = nVarSemiAll, do all (semi-)implicit variables together
    do iVarSemi = 1, nVarSemiAll, nVarSemi

       if(UseSplitSemiImplicit)then
          iVarSemiMin = iVarSemi
          iVarSemiMax = iVarSemi
          call set_rad_diff_range(iVarSemi)
       end if

       ! Set right hand side
       call get_semi_impl_rhs(Rhs_I)

       ! Calculate Jacobian matrix if required
       if(SemiParam%DoPrecond)then
          call timing_start('impl_jacobian')
          call get_semi_impl_jacobian
          call timing_stop('impl_jacobian')
       endif

       call timing_start('solve_linear_multiblock')
       ! solve implicit system
       call solve_linear_multiblock( SemiParam, &
            nVarSemi, nDim, nI, nJ, nK, nDiagSemi, nBlockSemi, iComm, &
            semi_impl_matvec, Rhs_I, x_I, DoTestKrylov, &
            JacSemi_VVCIB, JacobiPrec_I, cg_precond, hypre_preconditioner)
       call timing_stop('solve_linear_multiblock')

       if(SemiParam%iError /= 0 .and. iProc == 0 .and. IsTimeAccurate) &
            call error_report(NameSub//': Krylov solver failed, Krylov error',&
            SemiParam%Error, iError1, .true.)

       !$omp parallel do private( n )
       !$acc parallel loop gang independent
       do iBlockSemi = 1, nBlockSemi
          n = (iBlockSemi-1)*nIJK*nVarSemi ! openmp testing
          !$acc loop vector independent collapse(3)
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             do iVar = iVarSemiMin, iVarSemiMax
                if(Used_GB(i,j,k,iBlockFromSemi_B(iBlockSemi)))then
                   n0 = n + nVarSemi*(i-1 + nI*(j-1 + nJ*(k-1))) + &
                        (iVar - iVarSemiMin + 1)
                   NewSemiAll_VCB(iVar,i,j,k,iBlockSemi) = &
                        SemiAll_VCB(iVar,i,j,k,iBlockSemi) + x_I(n0)
                else
                   NewSemiAll_VCB(iVar,i,j,k,iBlockSemi) = &
                        SemiAll_VCB(iVar,i,j,k,iBlockSemi)
                end if
             enddo
          enddo; enddo; enddo;
       enddo
       !$omp end parallel do
    end do ! Splitting

    call timing_start('update_impl')
    ! Put back semi-implicit result into the explicit code
    !$omp parallel do private(iBlock)
    !$acc parallel loop gang
    do iBlockSemi = 1, nBlockSemi
       iBlock = iBlockFromSemi_B(iBlockSemi)
       select case(TypeSemiImplicit)
       case('radiation', 'radcond', 'cond')
#ifndef _OPENACC
          call update_impl_rad_diff(iBlock, iBlockSemi, &
               NewSemiAll_VCB(:,:,:,:,iBlockSemi), &
               SemiAll_VCB(:,:,:,:,iBlockSemi), &
               DconsDsemiAll_VCB(:,:,:,:,iBlockSemi))
#endif
       case('parcond')
          call update_impl_heat_cond(iBlock, iBlockSemi, &
               NewSemiAll_VCB(:,:,:,:,iBlockSemi), &
               SemiAll_VCB(:,:,:,:,iBlockSemi), &
               DconsDsemiAll_VCB(:,:,:,:,iBlockSemi))
       case('resistivity','resist','resisthall')
#ifndef _OPENACC
          call update_impl_resistivity(iBlock, &
               NewSemiAll_VCB(:,:,:,:,iBlockSemi))
#endif
       case default
#ifndef _OPENACC
          call stop_mpi(NameSub//': no update_impl implemented for' &
               //TypeSemiImplicit)
#endif
       end select
       call limit_pressure(1, nI, 1, nJ, 1, nK, iBlock, 1, 1, State_VGB)
    end do
    !$omp end parallel do

    call timing_stop('update_impl')

    ! call cpu_time(finish)

    ! print '("Time = ",f6.3," seconds.")',finish-start

    if(DoTest) write(*,*)NameSub,' after update test var=', &
         State_VGB(iVarTest,iTest,jTest,kTest,iBlockTest)

    if(DoFixAxis)call fix_axis_cells
    if(UseCoarseAxis)call coarsen_axis_cells
    ! Exchange messages, so ghost cells of all blocks are updated
    if(UseFieldLineThreads) call advance_threads(Heat_)

    call exchange_messages

    if(DoTest) write(*,*)NameSub,' final test var, B0=', &
         State_VGB(iVarTest,iTest,jTest,kTest,iBlockTest), &
         B0_DGB(:,iTest,jTest,kTest,iBlockTest)

    call timing_stop(NameSub)

    call test_stop(NameSub, DoTest)
  end subroutine advance_semi_impl
  !============================================================================
  subroutine get_semi_impl_rhs_block(iBlock, SemiState_VG, RhsSemi_VC, &
       IsLinear)
    !$acc routine vector

    use BATL_lib, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK, nI, nJ, nK
    use ModHeatConduction, ONLY: get_heat_conduction_rhs

#ifndef _OPENACC
    use ModLinearSolver, ONLY: UsePDotADotP
    use ModRadDiffusion, ONLY: get_rad_diffusion_rhs
    use ModResistivity, ONLY: get_resistivity_rhs
#endif

    integer, intent(in):: iBlock
    real, intent(inout):: SemiState_VG(nVarSemi,MinI:MaxI,MinJ:MaxJ,MinK:MaxK)
    real, intent(out)  :: RhsSemi_VC(nVarSemi,nI,nJ,nK)
    logical, intent(in):: IsLinear

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_semi_impl_rhs_block'
    !--------------------------------------------------------------------------
#ifndef _OPENACC
    call test_start(NameSub, DoTest, iBlock)
#endif
    select case(TypeSemiImplicit)
    case('radiation', 'radcond', 'cond')
#ifndef _OPENACC
       if(.not.IsLinear) UsePDotADotP = .false.
       call get_rad_diffusion_rhs(iBlock, SemiState_VG, &
            RhsSemi_VC, IsLinear=IsLinear)
#endif
    case('parcond')
       call get_heat_conduction_rhs(iBlock, SemiState_VG, &
            RhsSemi_VC, IsLinear=IsLinear)
    case('resistivity','resist','resisthall')
#ifndef _OPENACC
       call get_resistivity_rhs(iBlock, SemiState_VG, &
            RhsSemi_VC, IsLinear=IsLinear)
#endif
    case default
#ifndef _OPENACC
       call stop_mpi(NameSub//': no get_rhs implemented for' &
            //TypeSemiImplicit)
#endif
    end select

#ifndef _OPENACC
    call test_stop(NameSub, DoTest, iBlock)
#endif
  end subroutine get_semi_impl_rhs_block
  !============================================================================
  subroutine get_semi_impl_rhs(RhsSemi_VCB)

    use ModGeometry, ONLY: IsBoundary_B
    use ModCellBoundary, ONLY: set_cell_boundary
    use BATL_lib, ONLY: nI, nJ, nK, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, &
         Used_GB, nBlock, Xyz_DGB, CellVolume_GB, &
         message_pass_cell, message_pass_face, apply_flux_correction_block
    use ModAdvance, ONLY: iTypeUpdate, UpdateFast_
    use ModUpdateStateFast, ONLY: set_cell_boundary_for_block

    real, intent(out):: RhsSemi_VCB(nVarSemi,nI,nJ,nK,nBlockSemi)

    integer :: iBlockSemi, iBlock, i, j, k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_semi_impl_rhs'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Initialize SemiState_VGB=0 so it can be message passed
    !$omp parallel do
    !$acc parallel loop gang independent
    do iBlock = 1, nBlock
       !$acc loop vector collapse(3) independent
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          SemiState_VGB(:,i,j,k,iBlock) = 0.0
       end do; end do; end do
    end do
    !$omp end parallel do

    ! Set SemiState_VGB from SemiAll_VCB in semi-implicit blocks
    !$omp parallel do private( iBlock )
    !$acc parallel loop gang independent
    do iBlockSemi = 1, nBlockSemi
       iBlock = iBlockFromSemi_B(iBlockSemi)
       !$acc loop vector collapse(3) independent
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          SemiState_VGB(:,i,j,k,iBlock) = &
               SemiAll_VCB(iVarSemiMin:iVarSemiMax,i,j,k,iBlockSemi)
       end do; end do; end do
    end do
    !$omp end parallel do

    ! Message pass to fill in ghost cells
    select case(TypeSemiImplicit)
    case('radiation', 'radcond', 'cond')
       if(UseAccurateRadiation)then
          call message_pass_cell(nVarSemi, SemiState_VGB, nWidthIn=2, &
               nProlongOrderIn=1, nCoarseLayerIn=2, &
               DoRestrictFaceIn = .true., &
               iDecomposition=iNewDecomposition)
       else
          call message_pass_cell(nVarSemi, SemiState_VGB, nWidthIn=1, &
               nProlongOrderIn=1, DoSendCornerIn=.false., &
               DoRestrictFaceIn=.true., &
               iDecomposition=iNewDecomposition)
       end if
    case('parcond','resistivity','resist','resisthall')
       call message_pass_cell(nVarSemi, SemiState_VGB, nWidthIn=2, &
            nProlongOrderIn=1, nCoarseLayerIn=2, DoRestrictFaceIn = .true.,&
            UseOpenACCIn=.true., iDecomposition=iNewDecomposition)
    case default
       call stop_mpi(NameSub//': no get_rhs message_pass implemented for' &
            //TypeSemiImplicit)
    end select

    !$acc parallel loop gang independent &
    !$acc present(RhsSemi_VCB)
    do iBlockSemi = 1, nBlockSemi
       iBlock = iBlockFromSemi_B(iBlockSemi)

       if(IsBoundary_B(iBlock)) then
          if(iTypeUpdate == UpdateFast_) then
             call set_cell_boundary_for_block(iBlock, nVarSemi, &
                  SemiState_VGB(:,:,:,:,iBlock), IsLinear=.false.)
          else
#ifndef _OPENACC
             ! Apply boundary conditions (1 layer of outer ghost cells)
             call set_cell_boundary(1, iBlock, nVarSemi, &
                  SemiState_VGB(:,:,:,:,iBlock), iBlockSemi, IsLinear=.false.)
#endif
          end if
       end if

       call get_semi_impl_rhs_block(iBlock, SemiState_VGB(:,:,:,:,iBlock), &
            RhsSemi_VCB(:,:,:,:,iBlockSemi), IsLinear=.false.)
    end do

#ifndef _OPENACC
    if( (DoConserveFlux .and. &
         TypeSemiImplicit(1:3) /= 'rad' .and. TypeSemiImplicit /= 'cond') &
         .or. UseAccurateRadiation)then
       call message_pass_face(nVarSemi, &
            FluxImpl_VXB, FluxImpl_VYB, FluxImpl_VZB)

       do iBlockSemi = 1, nBlockSemi
          iBlock = iBlockFromSemi_B(iBlockSemi)

          ! there are no ghost cells for RhsSemi_VCB
          call apply_flux_correction_block(iBlock, nVarSemi, 0, 0, &
               RhsSemi_VCB(:,:,:,:,iBlockSemi), &
               Flux_VXB=FluxImpl_VXB, Flux_VYB=FluxImpl_VYB, &
               Flux_VZB=FluxImpl_VZB)
       end do
    end if
#endif

    ! Multiply with cell volume (makes matrix symmetric)
    !$acc parallel loop gang independent
    do iBlockSemi = 1, nBlockSemi
       iBlock = iBlockFromSemi_B(iBlockSemi)
       !$acc loop vector collapse(3) independent
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          RhsSemi_VCB(:,i,j,k,iBlockSemi) = RhsSemi_VCB(:,i,j,k,iBlockSemi) &
               *CellVolume_GB(i,j,k,iBlock)
       end do; end do; end do
    end do

    if(DoTest) then
       do iBlockSemi = 1, nBlockSemi
          iBlock = iBlockFromSemi_B(iBlockSemi)
          if(iBlockSemi == iBlockSemiTest)then
             write(*,*) NameSub, &
                  ' iBlock, iBlockSemi, sum(Rhs**2)=', iBlock, iBlockSemi, &
                  sum(RhsSemi_VCB(:,:,:,:,iBlockSemi)**2)
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                write(*,*) NameSub, &
                     ' i,j,k,True,Xyz,Rhs=', &
                     i,j,k, Used_GB(i,j,k,iBlock), Xyz_DGB(:,i,j,k,iBlock), &
                     RhsSemi_VCB(:,i,j,k,iBlockSemi)
             end do; end do; end do
          end if
       end do
    end if

    call test_stop(NameSub, DoTest)
  end subroutine get_semi_impl_rhs
  !============================================================================
  subroutine semi_impl_matvec(x_I, y_I, MaxN)

    ! Calculate y_I = A.x_I where A is the linearized semi-implicit operator

    use ModAdvance, ONLY: DtMax_CB, iTypeUpdate, UpdateFast_
    use ModGeometry, ONLY: IsBoundary_B
    use ModMain, ONLY: Dt, IsTimeAccurate, Cfl, UseDtLimit
    use ModLinearSolver, ONLY: UsePDotADotP, pDotADotPPe
    use ModCellBoundary, ONLY: set_cell_boundary
    use ModUpdateStateFast, ONLY: set_cell_boundary_for_block
    use BATL_lib, ONLY: nI, nJ, nK, nIJK,  CellVolume_GB, &
         message_pass_cell, message_pass_face, &
         apply_flux_correction_block

    integer, intent(in) :: MaxN
    real,    intent(in) :: x_I(MaxN)
    real,    intent(out):: y_I(MaxN)

    integer :: iBlockSemi, iBlock, i, j, k, iVar, n, n0
    real :: Volume, DtLocal

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'semi_impl_matvec'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    call timing_start(NameSub)

    ! Fill in StateSemi so it can be message passed
    n = 0
    !$omp parallel do private( iBlock,n )
    !$acc parallel loop gang independent
    do iBlockSemi = 1, nBlockSemi
       iBlock = iBlockFromSemi_B(iBlockSemi)
       n = (iBlockSemi-1)*nIJK*nVarSemi
       !$acc loop vector collapse(4) independent
       do k = 1, nK; do j = 1, nJ; do i = 1, nI; do iVar = 1, nVarSemi
          n0 = n + iVar + nVarSemi*(i-1 + nI*(j-1 + nJ*(k-1)) )
          SemiState_VGB(iVar,i,j,k,iBlock) = x_I(n0)
       end do; end do; end do; end do
    end do
    !$omp end parallel do

    ! Message pass to fill in ghost cells
    select case(TypeSemiImplicit)
    case('radiation', 'radcond', 'cond')
#ifndef _OPENACC
       if(UseAccurateRadiation)then
          UsePDotADotP = .false.

          call message_pass_cell(nVarSemi, SemiState_VGB, nWidthIn=2, &
               nProlongOrderIn=1, nCoarseLayerIn=2, &
               DoRestrictFaceIn = .true., &
               iDecomposition=iNewDecomposition)
       else
          ! Initialize the computation of (p . A . P) form
          UsePDotADotP = SemiParam%TypeKrylov == 'CG'

          pDotADotPPe = 0.0

          call message_pass_cell(nVarSemi, SemiState_VGB, nWidthIn=1, &
               nProlongOrderIn=1, DoSendCornerIn=.false., &
               DoRestrictFaceIn=.true., &
               iDecomposition=iNewDecomposition)
       end if
#endif
    case('parcond','resistivity','resist','resisthall')
       call message_pass_cell(nVarSemi, SemiState_VGB, nWidthIn=2, &
            nProlongOrderIn=1, nCoarseLayerIn=2, DoRestrictFaceIn = .true.,&
            UseOpenACCIn=.true., iDecomposition=iNewDecomposition)
    case default
       call stop_mpi(NameSub//': no get_rhs message_pass implemented for' &
            //TypeSemiImplicit)
    end select

    n = 0
    !$omp parallel do private( iBlock,n,DtLocal,Volume )
    !$acc parallel loop gang independent
    do iBlockSemi = 1, nBlockSemi
       iBlock = iBlockFromSemi_B(iBlockSemi)

       if(IsBoundary_B(iBlock)) then
          if(iTypeUpdate == UpdateFast_) then
             call set_cell_boundary_for_block(iBlock, nVarSemi, &
                  SemiState_VGB(:,:,:,:,iBlock), IsLinear=.true.)
          else
#ifndef _OPENACC
             call set_cell_boundary( 1, iBlock, nVarSemi, &
                  SemiState_VGB(:,:,:,:,iBlock), iBlockSemi, IsLinear=.true.)
#endif
          end if
       end if

       call get_semi_impl_rhs_block(iBlock, SemiState_VGB(:,:,:,:,iBlock), &
            ResSemi_VCB(:,:,:,:,iBlockSemi), IsLinear=.true.)

#ifndef _OPENACC
       if(UsePDotADotP)then
          n = (iBlockSemi-1)*nIJK*nVarSemi ! openmp testing
          DtLocal = Dt
          if(UseSplitSemiImplicit)then
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                if(.not.IsTimeAccurate .or. UseDtLimit) &
                     DtLocal = max(1.0e-30,Cfl*DtMax_CB(i,j,k,iBlock))
                Volume = CellVolume_GB(i,j,k,iBlock)/(DtLocal*SemiImplCoeff)
                n = n + 1
                pDotADotPPe = pDotADotPPe +  &
                     Volume*x_I(n)**2 &
                     *DconsDsemiAll_VCB(iVarSemi,i,j,k,iBlockSemi)
             end do; enddo; enddo
          else
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                if(.not.IsTimeAccurate .or. UseDtLimit) &
                     DtLocal = max(1.0e-30,Cfl*DtMax_CB(i,j,k,iBlock))
                Volume = CellVolume_GB(i,j,k,iBlock)/(DtLocal*SemiImplCoeff)
                do iVar = 1, nVarSemi
                   n = n + 1
                   pDotADotPPe = pDotADotPPe + Volume*x_I(n)**2 &
                        *DconsDsemiAll_VCB(iVar,i,j,k,iBlockSemi)
                enddo
             enddo; enddo; enddo
          end if
       end if
#endif

    end do
    !$omp end parallel do

#ifndef _OPENACC
    if((DoConserveFlux .and.  &
         TypeSemiImplicit(1:3) /= 'rad' .and. TypeSemiImplicit /= 'cond') &
         .or. UseAccurateRadiation)then
       call message_pass_face(nVarSemi, &
            FluxImpl_VXB, FluxImpl_VYB, FluxImpl_VZB)

       !$omp parallel do private( iBlock )
       do iBlockSemi = 1, nBlockSemi
          iBlock = iBlockFromSemi_B(iBlockSemi)

          ! zero ghost cells for ResSemi_VCB
          call apply_flux_correction_block(iBlock, nVarSemi,0, 0, &
               ResSemi_VCB(:,:,:,:,iBlockSemi), &
               Flux_VXB=FluxImpl_VXB, Flux_VYB=FluxImpl_VYB, &
               Flux_VZB=FluxImpl_VZB)
       end do
       !$omp end parallel do
    end if
#endif

    n = 0
    if(UseSplitSemiImplicit)then
#ifndef _OPENACC
       !$omp parallel do private( iBlock, DtLocal, Volume, n )
       do iBlockSemi = 1, nBlockSemi
          iBlock = iBlockFromSemi_B(iBlockSemi)
          DtLocal = Dt
          n = (iBlockSemi-1)*nIJK*nVarSemi ! openmp testing

          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             if(.not.IsTimeAccurate .or. UseDtLimit) &
                  DtLocal = max(1.0e-30,Cfl*DtMax_CB(i,j,k,iBlock))
             Volume = CellVolume_GB(i,j,k,iBlock)
             n = n + 1
             y_I(n) = Volume* &
                  (x_I(n)*DconsDsemiAll_VCB(iVarSemi,i,j,k,iBlockSemi) &
                  /DtLocal - SemiImplCoeff*ResSemi_VCB(1,i,j,k,iBlockSemi))
          end do; enddo; enddo
       end do
       !$omp end parallel do
#endif
    else
       !$omp parallel do private( iBlock,DtLocal,n,Volume )
       !$acc parallel loop gang independent
       do iBlockSemi = 1, nBlockSemi
          iBlock = iBlockFromSemi_B(iBlockSemi)
          n = (iBlockSemi-1)*nIJK*nVarSemi ! openmp testing
          !$acc loop vector collapse(3) independent
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             if(.not.IsTimeAccurate .or. UseDtLimit) then
                DtLocal = max(1.0e-30,Cfl*DtMax_CB(i,j,k,iBlock))
             else
                DtLocal = Dt
             endif
             Volume = CellVolume_GB(i,j,k,iBlock)
             do iVar = 1, nVarSemi
                ! n = n + 1
                n0 = n + iVar + nVarSemi*(i-1 + nI*(j-1 + nJ*(k-1)) )
                y_I(n0) = Volume* &
                     (x_I(n0)*DconsDsemiAll_VCB(iVar,i,j,k,iBlockSemi)/DtLocal&
                     - SemiImplCoeff * ResSemi_VCB(iVar,i,j,k,iBlockSemi))
             enddo
          enddo; enddo; enddo
       end do
       !$omp end parallel do
    end if

    if(UsePDotADotP)then
       pDotADotPPe = pDotADotPPe * SemiImplCoeff
    else
       pDotADotPPe = 0.0
    end if

    ! Apply left preconditioning if required: y --> P_L.y
    if(SemiParam%DoPrecond .and. SemiParam%TypeKrylov /= 'CG') &
         call semi_precond(MaxN, y_I)

    call timing_stop(NameSub)

    call test_stop(NameSub, DoTest)
  end subroutine semi_impl_matvec
  !============================================================================
  subroutine cg_precond(Vec_I, PrecVec_I, MaxN)

    ! Set PrecVec = Prec.Vec where Prec is the preconditioner matrix.
    ! This routine is used by the Preconditioned Conjugate Gradient method

    integer, intent(in) :: MaxN
    real,    intent(in) :: Vec_I(MaxN)
    real,    intent(out):: PrecVec_I(MaxN)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'cg_precond'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    PrecVec_I = Vec_I
    call semi_precond(MaxN, PrecVec_I)

    call test_stop(NameSub, DoTest)
  end subroutine cg_precond
  !============================================================================
  subroutine semi_precond(MaxN, Vec_I)

    ! Multiply Vec with the preconditioner matrix.
    ! This routine is used by the Preconditioned Conjugate Gradient method

    use BATL_lib, ONLY: nDim, nI, nJ, nK
    use ModLinearSolver, ONLY: precond_left_multiblock
    use ModImplHypre, ONLY: hypre_preconditioner

    integer, intent(in)   :: MaxN
    real,    intent(inout):: Vec_I(MaxN)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'semi_precond'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(SemiParam%TypePrecond)
    case('HYPRE')
       call hypre_preconditioner(MaxN, Vec_I)
    case('JACOBI')
       Vec_I = JacobiPrec_I(1:MaxN)*Vec_I
    case default
       call precond_left_multiblock(SemiParam, &
            nVarSemi, nDim, nI, nJ, nK, nDiagSemi, nBlockSemi, &
            JacSemi_VVCIB, Vec_I)
    end select

    call test_stop(NameSub, DoTest)
  end subroutine semi_precond
  !============================================================================
  subroutine update_block_jacobian_face

    use ModGeometry, ONLY: IsBoundary_B
    use ModFieldLineThread, ONLY: UseFieldLineThreads
    use BATL_lib, ONLY: IsCartesianGrid
    use ModFaceGradient, ONLY: set_block_jacobian_face_simple
    use ModImplicit, ONLY: DcoordDxyz_DDFDI, TransGrad_DDGI
    use ModUtilities, ONLY: i_gang
    use ModMain, ONLY: iNewDecomposition, iNewGrid

    integer :: iGang, iBlock, iBlockSemi

    integer, save:: iGrid = -1, iDecomposition = -1

    logical :: UseFirstOrderBc
    !--------------------------------------------------------------------------
    if(IsCartesianGrid) RETURN

    if(iGrid == iNewGrid .and. iDecomposition == iNewDecomposition) RETURN
    iGrid = iNewGrid
    iDecomposition = iNewDecomposition

#ifdef _OPENACC
    !$acc parallel loop gang independent
    do iBlockSemi = 1, nBlockSemi
       iBlock = iBlockFromSemi_B(iBlockSemi)
       UseFirstOrderBc = UseFieldLineThreads.and.IsBoundary_B(iBlock)
       iGang = i_gang(iBlock)
       call set_block_jacobian_face_simple(iBlock, &
            DcoordDxyz_DDFDI(:,:,:,:,:,:,iGang), &
            TransGrad_DDGI(:,:,:,:,:,iGang), &
            UseFirstOrderBc)
    end do
#endif
  end subroutine update_block_jacobian_face
  !============================================================================
  subroutine test_semi_impl_jacobian

    ! Calculate the Jacobian Jac_VVI = d(RHS)/d(Var) for the test cell
    ! using numerical derivatives of the RHS obtained with
    ! get_semi_impl_rhs_block

    use BATL_lib, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK, nI, nJ, nK, &
         IsRotatedCartesian, rot_to_cart

    ! Local variables
    real, parameter:: RelEps = 1e-6, AbsEps = 1e-8

    real, allocatable:: SemiState_VG(:,:,:,:), SemiPert_VG(:,:,:,:), &
         Rhs_VC(:,:,:,:), RhsPert_VC(:,:,:,:), &
         JacAna_VVCI(:,:,:,:,:,:), JacNum_VVI(:,:,:), &
         JacAna_VV(:,:), JacNum_VV(:,:)

    real:: JacAna, JacNum

    integer:: i, j, k, iPert, jPert, kPert, iBlock, iStencil, iVar, jVar

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'test_semi_impl_jacobian'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    allocate( &
         SemiState_VG(nVarSemi, MinI:MaxI,MinJ:MaxJ,MinK:MaxK), &
         SemiPert_VG(nVarSemi,MinI:MaxI,MinJ:MaxJ,MinK:MaxK), &
         Rhs_VC(nVarSemi,nI,nJ,nK), RhsPert_VC(nVarSemi,nI,nJ,nK), &
         JacAna_VVCI(nVarSemi,nVarSemi,nI,nJ,nK,nDiagSemi), &
         JacNum_VVI(nVarSemi,nVarSemi,nDiagSemi), &
         JacAna_VV(nVarSemi,nVarSemi), JacNum_VV(nVarSemi,nVarSemi))

    ! For sake of simpler code
    iBlock = iBlockTest

    ! Get analytic Jacobian
    call get_semi_impl_jacobian_block(iBlock, JacAna_VVCI)

    ! Set semi-implicit state with nG ghost cells.
    ! The ghost cells are set but not used.
    SemiState_VG = 1.0
    SemiState_VG(:,1:nI,1:nJ,1:nK) = &
         SemiAll_VCB(iVarSemiMin:iVarSemiMax,:,:,:,iBlockSemiTest)

    ! Initialize perturbed state
    SemiPert_VG = SemiState_VG

    ! Get original RHS (use SemiPert to keep compiler happy)
    call get_semi_impl_rhs_block(iBlock, SemiPert_VG, Rhs_VC, IsLinear=.false.)

    ! Select test cell
    i = iTest; j = jTest; k = kTest

    ! Initialize numerical Jacobian
    JacNum_VVI = 0.0

    ! Loop over stencil
    do iStencil = 1, nDiagSemi

       ! Set indexes for perturbed cell
       iPert = i; jPert = j; kPert = k
       select case(iStencil)
       case(2)
          iPert = i - 1
          if(iPert == 0) CYCLE
       case(3)
          iPert = i + 1
          if(iPert > nI) CYCLE
       case(4)
          jPert = j - 1
          if(jPert == 0) CYCLE
       case(5)
          jPert = j + 1
          if(jPert > nJ) CYCLE
       case(6)
          kPert = k - 1
          if(kPert == 0) CYCLE
       case(7)
          kPert = k + 1
          if(kPert > nK) CYCLE
       end select

       ! Loop over variables to be perturbed
       do iVar = 1, nVarSemi

          ! Perturb the variable
          SemiPert_VG(iVar,iPert,jPert,kPert) &
               = SemiState_VG(iVar,iPert,jPert,kPert)*(1 + RelEps) + AbsEps

          ! Calculate perturbed RHS
          call get_semi_impl_rhs_block(iBlock, SemiPert_VG, RhsPert_VC, &
               IsLinear=.false.)

          ! Calculate Jacobian elements
          JacNum_VVI(:,iVar,iStencil) = &
               (RhsPert_VC(:,i,j,k) - Rhs_VC(:,i,j,k)) / &
               (  SemiPert_VG(iVar,iPert,jPert,kPert)    &
               - SemiState_VG(iVar,iPert,jPert,kPert) )

          ! Reset the variable to the original value
          SemiPert_VG(iVar,iPert,jPert,kPert) = &
               SemiState_VG(iVar,iPert,jPert,kPert)
       end do
    end do

    do iStencil = 1, nDiagSemi

       JacAna_VV = JacAna_VVCI(:,:,i,j,k,iStencil)
       JacNum_VV = JacNum_VVI(:,:,iStencil)

       if(IsRotatedCartesian .and. TypeSemiImplicit(1:6) == 'resist')then
          JacAna_VV = rot_to_cart(JacAna_VV)
          JacNum_VV = rot_to_cart(JacNum_VV)
       end if

       do jVar = 1, nVarSemi; do iVar = 1, nVarSemi
          JacAna = JacAna_VV(iVar,jVar)
          JacNum = JacNum_VV(iVar,jVar)
          write(*,'(a,3i2,a,4es13.5)') &
               NameSub//': iVar,jVar,iStencil=', iVar, jVar, iStencil, &
               ' JacAna, JacNum, AbsDiff, RelDiff=', &
               JacAna, JacNum, abs(JacAna - JacNum), &
               2*abs(JacAna - JacNum)/(abs(JacAna) + abs(JacNum) + 1e-30)
       end do; end do
    end do

    deallocate(SemiState_VG, SemiPert_VG, Rhs_VC, RhsPert_VC, &
         JacAna_VVCI, JacNum_VVI, JacAna_VV, JacNum_VV)

    call test_stop(NameSub, DoTest)
  end subroutine test_semi_impl_jacobian
  !============================================================================
  subroutine get_semi_impl_jacobian_block(iBlock, JacSemi_VVCI)
    !$acc routine vector

    use BATL_lib, ONLY: nI, nJ, nK
    use ModHeatConduction, ONLY: add_jacobian_heat_cond
#ifndef _OPENACC
    use ModRadDiffusion, ONLY: add_jacobian_rad_diff
    use ModResistivity, ONLY: add_jacobian_resistivity, &
         add_jacobian_hall_resist
#endif

    integer, intent(in) :: iBlock
    real,    intent(out):: JacSemi_VVCI(nVarSemi,nVarSemi,nI,nJ,nK,nDiagSemi)

    ! All elements have to be set
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_semi_impl_jacobian_block'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    JacSemi_VVCI = 0.0

    select case(TypeSemiImplicit)
    case('radiation', 'radcond', 'cond')
#ifndef _OPENACC
       call add_jacobian_rad_diff(iBlock, nVarSemi, JacSemi_VVCI)
#endif
    case('parcond')
       call add_jacobian_heat_cond(iBlock, nVarSemi, JacSemi_VVCI)
    case('resistivity','resist','resisthall')
#ifndef _OPENACC
       if(UseSemiResistivity) &
            call add_jacobian_resistivity(iBlock, nVarSemi, JacSemi_VVCI)
       if(UseSemiHallResist) &
            call add_jacobian_hall_resist(iBlock, nVarSemi, JacSemi_VVCI)
#endif
    case default
#ifndef _OPENACC
       call stop_mpi(NameSub//': no add_jacobian implemented for' &
            //TypeSemiImplicit)
#endif
    end select

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine get_semi_impl_jacobian_block
  !============================================================================
  subroutine get_semi_impl_jacobian

    use ModAdvance, ONLY: DtMax_CB
    use ModMain, ONLY: Dt, IsTimeAccurate, Cfl, UseDtLimit
    use ModImplicit, ONLY: UseNoOverlap
    use ModImplHypre, ONLY: hypre_set_matrix_block, hypre_set_matrix
    use BATL_lib, ONLY: nI, nJ, nK, Used_GB, CellVolume_GB

    integer :: iBlockSemi, iBlock, i, j, k, iStencil, iVar
    real    :: Coeff, DtLocal

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_semi_impl_jacobian'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(DoTest) call test_semi_impl_jacobian

    ! The HYPRE AMG preconditioner requires the overlap between blocks
    ! For all other preconditioners it is better to avoid the overpap
    if(SemiParam%TypePrecond=='HYPRE') UseNoOverlap = .false.

    !$omp parallel do private( iBlock, Coeff, DtLocal )
    !$acc parallel loop gang independent
    do iBlockSemi = 1, nBlockSemi
       iBlock = iBlockFromSemi_B(iBlockSemi)

       ! Get dR/dU
       call get_semi_impl_jacobian_block(iBlock, &
            JacSemi_VVCIB(:,:,:,:,:,:,iBlockSemi))

       ! Form A = Volume*(1/dt - SemiImplCoeff*dR/dU)
       !    symmetrized for sake of CG
       !$acc loop vector collapse(4) independent
       do iStencil = 1, nDiagSemi; do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE
          Coeff = -SemiImplCoeff*CellVolume_GB(i,j,k,iBlock)
          JacSemi_VVCIB(:,:,i,j,k,iStencil,iBlockSemi) = &
               Coeff * JacSemi_VVCIB(:,:,i,j,k,iStencil,iBlockSemi)
       end do; end do; end do; end do
       DtLocal = Dt
       !$acc loop vector collapse(3) independent
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not.IsTimeAccurate .or. UseDtLimit) &
               DtLocal = max(1.0e-30, Cfl*DtMax_CB(i,j,k,iBlock))
          Coeff = CellVolume_GB(i,j,k,iBlock)/DtLocal
          if(UseSplitSemiImplicit)then
             JacSemi_VVCIB(1,1,i,j,k,1,iBlockSemi) = &
                  Coeff*DconsDsemiAll_VCB(iVarSemi,i,j,k,iBlockSemi) &
                  + JacSemi_VVCIB(1,1,i,j,k,1,iBlockSemi)
          else
             do iVar = 1, nVarSemi
                JacSemi_VVCIB(iVar,iVar,i,j,k,1,iBlockSemi) = &
                     Coeff*DconsDsemiAll_VCB(iVar,i,j,k,iBlockSemi) &
                     + JacSemi_VVCIB(iVar,iVar,i,j,k,1,iBlockSemi)
             end do
          end if
       end do; end do; end do

#ifndef _OPENACC
       if(SemiParam%TypePrecond == 'HYPRE') &
            call hypre_set_matrix_block(iBlockSemi, &
            JacSemi_VVCIB(1,1,1,1,1,1,iBlockSemi))
#endif
    end do
    !$omp end parallel do

    if(SemiParam%TypePrecond == 'HYPRE') call hypre_set_matrix(.true.)

    UseNoOverlap = .true.

    call test_stop(NameSub, DoTest)
  end subroutine get_semi_impl_jacobian
  !============================================================================
  subroutine check_nan_semi
#ifndef _OPENACC
    use BATL_lib, ONLY: nI, nJ, nK, Xyz_DGB, iProc
    use ModAdvance, ONLY: State_VGB
    use, intrinsic :: ieee_arithmetic

    integer:: iVar, iBlockSemi, iBlock, i, j, k
    real:: Value1, Value2

    character(len=*), parameter:: NameSub = 'check_nan_semi'
    !--------------------------------------------------------------------------
    do iBlockSemi = 1, nBlockSemi
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          do iVar = 1, nVarSemiAll
             Value1 = SemiAll_VCB(iVar,i,j,k,iBlockSemi)
             Value2 = DconsDsemiAll_VCB(iVar,i,j,k,iBlockSemi)
             if (ieee_is_nan(Value1) .or. ieee_is_nan(Value2)) then
                write(*,*) 'iProc=', iProc, &
                     ': NaN in SemiAll_V=', SemiAll_VCB(:,i,j,k,iBlockSemi)
                write(*,*) 'iProc=', iProc, ': NaN DconsDsemiAll_V=', &
                     DconsDsemiAll_VCB(:,i,j,k,iBlockSemi)
                write(*,*) 'iProc=', iProc, ': NaN State_V=', &
                     State_VGB(:,i,j,k,iBlock)
                iBlock = iBlockFromSemi_B(iBlockSemi)
                write(*,*) 'iProc=', iProc, &
                     ': NaN at i,j,k,iBlockSemi, iBlock= ', &
                     i, j, k, iBlockSemi, iBlock, &
                     ', x,y,z= ', Xyz_DGB(:,i,j,k,iBlock)

                call stop_mpi(NameSub//': NaN in SemiAll or DconsDsemiAll')
             end if
          end do
       end do; end do; end do
    end do
#endif
  end subroutine check_nan_semi
  !============================================================================
end module ModSemiImplicit
!==============================================================================

