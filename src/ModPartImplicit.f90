!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModPartImplicit

  use BATL_lib, ONLY: &
       test_start, test_stop, StringTest, iTest, jTest, kTest, iBlockTest, &
       iVarTest, iProc, nProc, iComm
  use ModBatsrusUtility, ONLY: error_report, stop_mpi

  use ModImplicit

  implicit none
  private ! except

  public:: read_part_impl_param
  public:: init_mod_part_impl
  public:: clean_mod_part_impl
  public:: advance_part_impl

  ! Local variables

  ! Maximum number of implicit blocks
  integer:: MaxBlockImpl = 0

  ! Use energy or pressure as the implicit variable
  logical :: UseImplicitEnergy = .true.

  ! Maximum number of unknowns per processor
  integer:: MaxImplVar

  real, parameter:: SmallDouble=1.E-30, BigDouble=1.E+30

  ! Actual number of implicitly treated blocks for a processor
  integer :: nBlockImpl=0

  ! Actual number of implicit variables (unknowns) per processor, total
  integer :: nImpl=0
  real :: NimplTotal=0.0 ! define real to avoid overfloating

  ! Test variables
  integer :: iBlockImplTest=1, nTest=1

  ! Indirect index array from implicit block index to general block index
  integer, allocatable :: iBlockFromImpl_B(:)

  ! Parameters for the implicit techniques

  ! Implicit scheme parameters
  real   :: CflImpl=100.0, ImplCoeff0=1.0, ImplCoeff=1.0
  logical:: UseImplSource = .false.

  ! Newton iteration parameters
  logical :: UseNewton = .false.
  ! Do a conservative update at the end of the newton iteration?
  logical :: UseConservativeImplicit = .false.
  integer :: MaxIterNewton = 1
  real    :: NewtonErrorMax = 0.001

  ! Real number accuracy for numerical derivatives in the Jacobian
  real:: JacobianEps = 1.E-12

  ! Default parameters for the implicit linear solver
  type(LinearSolverParamType):: ImplParam = LinearSolverParamType( &
       .true.,      &! DoPrecond
       'symmetric', &! TypePrecondSide
       'MBILU',     &! TypePrecond
       -0.5,        &! PrecondParam (Gustafsson for MBILU)
       'GMRES',     &! TypeKrylov
       'rel',       &! TypeStop
       1e-3,        &! ErrorMax
       100,         &! MaxMatvec
       100,         &! nKrylovVector
       .false.,     &! UseInitialGuess
       -1.0, -1, -1) ! Error, nMatvec, iError (return values)

  ! Additional Krylov scheme parameters
  character(len=10):: TypeKrylovInit ='nul'

  ! Spatial order of the implicit part of the residual in dR/dU
  integer:: nOrderImpl = 1

  ! Previous time step, explicit time step, ratio of explicit and implicit dt
  real:: DtExpl = -1.0, DtCoeff = -1.0

  ! Second norm of the variables and the residual
  real:: Norm_V(nVar)

  ! The k-th Newton iterate, one layer of ghost cells for the Jacobian
  real, allocatable :: Impl_VGB(:,:,:,:,:)

  ! low and high order residuals
  real, allocatable :: ResImpl_VCB(:,:,:,:,:)
  real, allocatable :: ResExpl_VCB(:,:,:,:,:)

  real, allocatable :: JacImpl_VVCIB(:,:,:,:,:,:,:)

  ! Right hand side for UseNewton, right hand side, dw=w_n+1 - w_n
  real, allocatable :: Rhs0_I(:)
  real, allocatable :: Rhs_I(:)
  real, allocatable :: x_I(:)

  ! Implicit cell or not.
  logical, allocatable :: IsImplCell_CB(:,:,:,:)

  ! Update check
  real :: &
       RejectStepLevel   = 0.3, RejectStepFactor   = 0.50, &
       ReduceStepLevel   = 0.6, ReduceStepFactor   = 0.95, &
       IncreaseStepLevel = 0.8, IncreaseStepFactor = 1.05

contains
  !============================================================================
  subroutine read_part_impl_param(NameCommand)

    use ModReadParam,     ONLY: read_var
    use ModPointImplicit, ONLY: UsePointImplicit
    use ModLinearSolver,  ONLY: &
         BlockJacobi_, GaussSeidel_, Bilu_
    use ModAdvance,       ONLY: UseEfield

    character(len=*), intent(in) :: NameCommand

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_part_impl_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(NameCommand)
    case("#GRIDBLOCKIMPL", "#GRIDBLOCKIMPLALL")
       call read_var('MaxBlockImpl', MaxBlockImpl)
       if(NameCommand == "#GRIDBLOCKIMPLALL") &
            MaxBlockImpl = 1 + (MaxBlockImpl-1)/nProc

    case('#IMPLICIT')
       call read_var('UsePointImplicit', UsePointImplicit)
       call read_var('UsePartImplicit',  UsePartImplicit)
       call read_var('UseFullImplicit',  UseFullImplicit)

       UseImplicit = UseFullImplicit .or. UsePartImplicit

       if(UsePartImplicit .and. UseFullImplicit) call stop_mpi(&
            'Only one of UsePartImplicit and UseFullImplicit can be true')

       if(UseImplicit)call read_var('CflImpl',CflImpl)

    case('#IMPLCRITERIA', '#IMPLICITCRITERIA', '#STEPPINGCRITERIA')
       call read_var('TypeImplCrit', TypeImplCrit, IsLowerCase=.true.)
       select case(TypeImplCrit)
       case('r')
          call read_var('rImplicit', rImplicit)
       case('test','dt')
       case default
          if(iProc==0)then
             write(*,'(a)')NameSub// &
                  ' WARNING: invalid TypeImplCrit='//trim(TypeImplCrit)// &
                  ' !!!'
             write(*,*)NameSub//' Setting TypeImplCrit=dt'
          end if
          TypeImplCrit = 'dt'
       end select

    case('#PARTIMPL', '#PARTIMPLICIT')
       call read_var('UsePartImplicit2', UsePartImplicit2)

    case('#IMPLENERGY', '#IMPLICITENERGY')
       call read_var('UseImplicitEnergy', UseImplicitEnergy)

    case('#IMPLSCHEME', '#IMPLICITSCHEME')
       call read_var('nOrderImpl', nOrderImpl)
       call read_var('TypeFluxImpl', TypeFluxImpl, IsUpperCase=.true.)
       ! For 5-moment equation all schemes are equivalent with Rusanov
       if(UseEField) TypeFluxImpl = 'RUSANOV'

    case('#IMPLSTEP', '#IMPLICITSTEP')
       call read_var('ImplCoeff ', ImplCoeff0)
       call read_var('UseBDF2   ', UseBdf2)
       call read_var('UseImplSource', UseImplSource)

    case('#IMPLCHECK', '#IMPLICITCHECK')
       call read_var('RejectStepLevel' ,   RejectStepLevel)
       call read_var('RejectStepFactor',   RejectStepFactor)
       call read_var('ReduceStepLevel' ,   ReduceStepLevel)
       call read_var('ReduceStepFactor',   ReduceStepFactor)
       call read_var('IncreaseStepLevel' , IncreaseStepLevel)
       call read_var('IncreaseStepFactor', IncreaseStepFactor)

    case('#NEWTON')
       call read_var('UseNewton',UseNewton)
       if(UseNewton)then
          call read_var('UseConservativeImplicit', UseConservativeImplicit)
          call read_var('MaxIterNewton', MaxIterNewton)
       end if

    case('#JACOBIAN')
       call read_var('DoPrecond', ImplParam%DoPrecond)
       call read_var('JacobianEps', JacobianEps)

    case('#PRECONDITIONER')
       call read_var('TypePrecondSide', ImplParam%TypePrecondSide, &
            IsLowerCase=.true.)
       select case(ImplParam%TypePrecondSide)
       case('left','right','symmetric')
       case default
          ImplParam%TypePrecondSide = 'left'
       end select
       call read_var('TypePrecond', ImplParam%TypePrecond, IsUpperCase=.true.)
       select case(ImplParam%TypePrecond)
       case('BLOCKJACOBI')
          ImplParam%PrecondParam = BlockJacobi_
          ImplParam%TypePrecondSide  = 'left'
       case('GS')
          ImplParam%PrecondParam = GaussSeidel_
       case('BILU')
          ImplParam%PrecondParam = Bilu_
       case('MBILU')
          call read_var('GustafssonPar', ImplParam%PrecondParam)
          ImplParam%PrecondParam = -ImplParam%PrecondParam
       case default
          call stop_mpi(NameSub// &
               ': invalid TypePrecond='//ImplParam%TypePrecond)
       end select

    case('#KRYLOV')
       call read_var('TypeKrylov',ImplParam%TypeKrylov, IsUpperCase=.true.)
       call read_var('TypeKrylovInit', TypeKrylovInit, IsLowerCase=.true.)
       select case(TypeKrylovInit)
       case('explicit', 'scaled', 'nul')
       case default
          TypeKrylovInit = 'nul'
       end select
       call read_var('ErrorMaxKrylov', ImplParam%ErrorMax)
       ! Use this error for the Newton iteration in case it is used
       NewtonErrorMax = ImplParam%ErrorMax
       call read_var('MaxMatvec', ImplParam%MaxMatvec)
       ImplParam%nKrylovVector   = ImplParam%MaxMatvec
       ImplParam%UseInitialGuess = TypeKrylovInit /= 'nul'

    case('#KRYLOVSIZE')
       call read_var('nKrylovVector', ImplParam%nKrylovVector)

    case default
       call stop_mpi(NameSub//' invalid NameCommand='//NameCommand)
    end select

    call test_stop(NameSub, DoTest)
  end subroutine read_part_impl_param
  !============================================================================
  subroutine init_mod_part_impl

    use ModSize, ONLY: MaxBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_mod_part_impl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(allocated(Impl_VGB)) RETURN

    ! Set default for MaxBlockImpl
    if(MaxBlockImpl < 1) MaxBlockImpl = MaxBlock

    ! Cannot exceed MaxBlock
    MaxBlockImpl = min(MaxBlockImpl, MaxBlock)

    ! Solve for all variables implicitly
    MaxImplVar = nVar*nIJK*MaxBlockImpl

    ! Arrays for all implicit variables
    allocate(Impl_VGB(nVar,0:nI+1,j0_:nJp1_,k0_:nKp1_,MaxBlockImpl))
    allocate(ImplOld_VCB(nVar,nI,nJ,nK,MaxBlock))

    ! Arrays for split implicit variables
    allocate(ResExpl_VCB(nVar,nI,nJ,nK,MaxBlockImpl))
    allocate(ResImpl_VCB(nVar,nI,nJ,nK,MaxBlockImpl))

    allocate(JacImpl_VVCIB(nVar,nVar,nI,nJ,nK,nStencil,MaxBlockImpl))
    allocate(Rhs0_I(MaxImplVar))
    allocate(Rhs_I(MaxImplVar))
    allocate(x_I(MaxImplVar))

    allocate(IsImplCell_CB(nI,nJ,nK,MaxBlock))

    ! Conversion of block index
    allocate(iBlockFromImpl_B(MaxBlockImpl))

    if(iProc==0)then
       call write_prefix
       write(iUnitOut,'(a)') 'init_mod_implicit allocated arrays'
    end if

    call test_stop(NameSub, DoTest)
  end subroutine init_mod_part_impl
  !============================================================================
  subroutine clean_mod_part_impl

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'clean_mod_part_impl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(.not.allocated(Impl_VGB)) RETURN
    deallocate(Impl_VGB)
    deallocate(ImplOld_VCB)
    deallocate(ResImpl_VCB)
    deallocate(ResExpl_VCB)
    deallocate(JacImpl_VVCIB)
    deallocate(Rhs0_I)
    deallocate(Rhs_I)
    deallocate(x_I)
    deallocate(IsImplCell_CB)
    deallocate(iBlockFromImpl_B)

    if(iProc==0)then
       call write_prefix
       write(iUnitOut,'(a)') NameSub//' deallocated arrays'
    end if

    call test_stop(NameSub, DoTest)
  end subroutine clean_mod_part_impl
  !============================================================================
  subroutine advance_part_impl

    ! The implicit schemes used in this module are described in detail in
    !
    ! G. Toth, D. L. De Zeeuw, T. I. Gombosi, K. G. Powell, 2006,
    !  Journal of Computational Physics, 217, 722-758,
    !  doi:10.1016/j.jcp.2006.01.029
    !
    ! and
    !
    ! Keppens, Toth, Botchev, van der Ploeg,
    ! J. Int. Num. Methods in Fluids, 30, 335-352, 1999
    !
    ! Equation numbers below refer to the latter paper unless stated otherwise.
    !
    ! We solve the MHD equation written as
    !
    !    dw/dt = R(t)                                                 (1)
    !
    ! by one of the following implicit schemes:
    !
    ! If UseBDF2 is false (and in any case for the 1st time step):
    !
    !    w^n+1 = w^n + dt^n*[R^n + ImplCoeff*(R_low^n+1 - R_low^n)]   (4)
    !
    ! where ImplCoeff is a fixed parameter in the [0,1] range.
    ! Here R is high order while R_imp is possibly low order discretization.
    ! The low order scheme is typically the first order Rusanov scheme.
    !
    ! If UseBDF2 is true (except for the 1st time step):
    !
    !    w^n+1 = w^n + dt^n*[ ImplCoeff*(R^n + R_low^n+1 - R_low^n)
    !
    !                        + (1-ImplCoeff)*(w^n - w^n-1)/dt^n-1]    (8)
    !
    ! where
    !
    !    ImplCoeff = (dt^n + dt^n-1)/(2*dt^n + dt^n-1)
    !
    ! provides second order time accuracy.
    !
    ! A Newton iteration is used to solve the discrete equations (4) or (8):
    !
    !    w^(k=0) = w^n
    !
    ! Solve
    !
    !    (I - dt*ImplCoeff*dR_low/dw).dw = ImplCoeff*dt*R(w^n)
    !
    !            + (1-ImplCoeff)*(w^n - w^n-1)*dt/dt^n-1
    !
    !            + ImplCoeff*dt*[R_low(w^k) - R_low^n] + (w^n - w^k)
    !
    ! for the increment dw.
    ! Terms in the second line are only included for BDF2,
    ! while terms in the third line are zero for the first k=0 iteration.
    !
    ! In each iteration update the iterate as
    !
    !    w^k+1 = w^k + dw
    !
    ! At most MaxIterNewton iterations are done. When the Newton iteration
    ! is finished, the solution is updated as
    !
    !    w^n+1 = w^k
    !
    ! In each iteration the linear problem is solved by a Krylov type
    ! iterative method.
    !
    ! We use get_residual(.false.,...) to calculate R_expl
    ! and    get_residual(.true.,....) to calculate R_impl

    use ModMain, ONLY: nBlockMax, nBlockExplAll, IsTimeAccurate, &
         nStep, tSimulation, dt, UseDtFixed, DtFixed, DtFixedOrig, Cfl, &
         iNewDecomposition
    use ModVarIndexes, ONLY: Rho_
    use ModAdvance, ONLY : State_VGB, StateOld_VGB, &
         DtMax_CB, Tmp1_GB, iTypeAdvance_B, iTypeAdvance_BP, &
         SkippedBlock_, ExplBlock_, ImplBlock_, UseUpdateCheck, DoFixAxis
    use ModAdvanceExplicit, ONLY: advance_explicit
    use ModCoarseAxis, ONLY:UseCoarseAxis, coarsen_axis_cells
    use ModPhysics, ONLY : No2Si_V, UnitT_
    use ModPointImplicit, ONLY: UsePointImplicit
    use ModLinearSolver, ONLY: solve_linear_multiblock
    use ModEnergy, ONLY: pressure_to_energy_block, energy_to_pressure_cell
    use ModMessagePass, ONLY: exchange_messages
    use ModResistivity, ONLY: UseResistivity, init_impl_resistivity, &
         init_impl_hall_resist
    use ModHallResist, ONLY: UseHallResist, HallCmaxFactor
    use ModFixAxisCells, ONLY: fix_axis_cells

    use BATL_lib, ONLY: Unused_B, Unused_BP, Xyz_DGB, minval_grid
    use BATL_size, ONLY: j0_, nJp1_, k0_, nKp1_
    use ModMpi

    integer :: i, j, k, iVar, iBlock, iBlockImpl
    integer :: nIterNewton
    integer :: iError, iError1
    real    :: NormX, NormLocal_V(nVar)

    logical :: IsConverged

    real :: TimeSimulationOrig, HallCmaxFactorOrig

    logical :: DoTestKrylov

    logical :: UseUpdateCheckOrig, UsePointImplicitOrig, &
         DoFixAxisOrig, UseCoarseAxisOrig

    real    :: pRhoRelativeMin

    integer :: iLoc_I(5)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'advance_part_impl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(DoTest) write(*,*)NameSub,' starting at step=',nStep

    call timing_start(NameSub)

    ! Initialize some variables in ModImplicit
    call implicit_init

    ! Get initial iterate from current state
    call explicit2implicit(0,nI+1,j0_,nJp1_,k0_,nKp1_,Impl_VGB)

    if(DoTest)write(*,*)NameSub,': nBlockImpl=',nBlockImpl
    if(DoTest.and.nBlockImpl>0)write(*,*)NameSub,': Impl_VGB=',&
         Impl_VGB(:,iTest,jTest,kTest,iBlockImplTest)

    call MPI_allreduce(real(nImpl), NimplTotal, 1, MPI_REAL, MPI_SUM, &
         iComm, iError)
    Norm_V = -1.0
    ! Global norm of current w_(k=0) = w_n
    do iVar = 1, nVar
       NormLocal_V(iVar) = sum(Impl_VGB(iVar,1:nI,1:nJ,1:nK,1:nBlockImpl)**2)
    end do
    call MPI_allreduce(NormLocal_V, Norm_V, nVar, MPI_REAL, MPI_SUM, &
         iComm,iError)

    Norm_V = sqrt(Norm_V/(NimplTotal/nVar))
    where(Norm_V < SmallDouble) Norm_V = 1.0

    if(DoTest)write(*,*)NameSub,': nImpltot, Norm_V=',NimplTotal,Norm_V

    TimeSimulationOrig = tSimulation
    UseUpdateCheckOrig = UseUpdateCheck
    UseUpdateCheck     = .false.

    ! Advance explicitly treated blocks if any
    if(UsePartImplicit .and. nBlockExplALL > 0)then
       call timing_start('part_expl')
       if(DoTest)write(*,*)NameSub,': advance explicit blocks'

       if(UseBDF2)then

          ! Save the current state into the previous state for BDF2 scheme
          ! This is needed for explicit blocks, because they may become
          ! implicit in the next time step...
          !$omp parallel do
          do iBlock = 1, nBlock
             if(iTypeAdvance_B(iBlock) /= ExplBlock_)CYCLE
             ImplOld_VCB(:,:,:,:,iBlock) = State_VGB(:,1:nI,1:nJ,1:nK,iBlock)

             if(UseImplicitEnergy) call pressure_to_energy_block( &
                  ImplOld_VCB(:,:,:,:,iBlock), 1, nI, 1, nJ, 1, nK, iBlock)
          end do
          !$omp end parallel do
       end if

       if(.not.UsePartImplicit2)then
          ! Select Unused_B = not explicit blocks
          iNewDecomposition = mod(iNewDecomposition+1, 10000)
          Unused_BP(1:nBlockMax,:) = &
               iTypeAdvance_BP(1:nBlockMax,:) /= ExplBlock_
          Unused_B(1:nBlockMax) = Unused_BP(1:nBlockMax,iProc)
       end if

       ! advance explicit blocks, calc timestep
       if(.not.UseDtFixed) cfl = ExplCfl

       ! Make sure that full whistler speed is used in the explicit update
       if(UseHallResist)then
          HallCmaxFactorOrig = HallCmaxFactor
          HallCmaxFactor = 1.0
       endif

       call advance_explicit(DoCalcTimestep=.true.)

       if(UseHallResist) HallCmaxFactor = HallCmaxFactorOrig

       if(.not.UsePartImplicit2)then
          ! update ghost cells for the implicit blocks to time level n+1
          iNewDecomposition = mod(iNewDecomposition+1, 10000)
          Unused_BP(1:nBlockMax,:) = &
               iTypeAdvance_BP(1:nBlockMax,:) == SkippedBlock_
          Unused_B(1:nBlockMax) = Unused_BP(1:nBlockMax,iProc)
       end if

       call exchange_messages

       ! The implicit scheme is only applied on implicit blocks
       iNewDecomposition = mod(iNewDecomposition+1, 10000)
       Unused_BP(1:nBlockMax,:) = &
            iTypeAdvance_BP(1:nBlockMax,:) /= ImplBlock_
       Unused_B(1:nBlockMax) = Unused_BP(1:nBlockMax,iProc)

       call timing_stop('part_expl')
    end if

    ! Advance implicitly treated blocks

    ! Let other parts of the code know that we are inside the implicit update
    IsImplicitUpdate = .true.

    ! Switch off point implicit scheme while advancing the implicit blocks
    UsePointImplicitOrig = UsePointImplicit
    UsePointImplicit = .false.

    ! Switch off merging the cells around the poles during the implicit solve
    DoFixAxisOrig      = DoFixAxis
    DoFixAxis          = .false.
    UseCoarseAxisOrig  = UseCoarseAxis
    UseCoarseAxis      = .false.
    ! Use implicit time step
    if(.not.UseDtFixed) Cfl = CflImpl

    if(UseDtFixed)then
       if(DoTest)write(*,*)NameSub,': call get_dt_courant'
       call get_dt_courant(DtExpl)
       DtExpl = 0.5*DtExpl
       DtCoeff = dt/DtExpl
    else
       if(DoTest)write(*,*)NameSub,': no call of get_dt_courant'
       DtCoeff = CflImpl/0.5
    endif

    if(UseBDF2.and.nStep==nStepPrev+1)then
       ! For 3 level BDF2 scheme set beta=ImplCoeff if previous state is known
       ImplCoeff = (dt+DtPrev)/(2*dt+DtPrev)
    else
       ImplCoeff = ImplCoeff0
    end if

    ! Advance time to level n+1 in case there is explicit time dependence:
    !   R(U^n+1,t^n+1) = R(U^n,t^n+1) + dR/dU(U^n,t^n+1).(U^n+1 - U^n)
    ! so the Jacobian should be evaluated at t^n+1

    tSimulation = TimeSimulationOrig + Dt*No2Si_V(UnitT_)

    if(DoTest.and.IsTimeAccurate)&
         write(*,*)NameSub,': DtCoeff,DtExpl,dt=',DtCoeff,DtExpl,dt
    if(DoTest.and.UseBDF2)write(*,*)NameSub,': nStepPrev,DtPrev,ImplCoeff=',&
         nStepPrev,DtPrev,ImplCoeff

    if(.not.UseBDF2)then
       ! Save the current state into ImplOld_VCB so that StateOld_VGB
       ! can be restored.
       ! The implicit blocks haven't been updated, so save current state
       !$omp parallel do private( iBlock )
       do iBlockImpl = 1, nBlockImpl
          iBlock = iBlockFromImpl_B(iBlockImpl)
          ImplOld_VCB(:,:,:,:,iBlock) = Impl_VGB(:,1:nI,1:nJ,1:nK,iBlockImpl)
       end do
       !$omp end parallel do
    end if

    ! Initialize right hand side and x_I. Uses ImplOld_VCB for BDF2 scheme.
    call impl_newton_init

    ! Save previous timestep for 3 level scheme
    if(UseBDF2)then
       nStepPrev  = nStep
       DtPrev = dt

       ! Save the current state into ImplOld_VCB so that StateOld_VGB
       ! can be restored.
       ! The implicit blocks haven't been updated, so save current state
       !$omp parallel do private( iBlock )
       do iBlockImpl = 1, nBlockImpl
          iBlock = iBlockFromImpl_B(iBlockImpl)
          ImplOld_VCB(:,:,:,:,iBlock) = Impl_VGB(:,1:nI,1:nJ,1:nK,iBlockImpl)
       end do
       !$omp end parallel do
    endif

    ! Newton-Raphson iteration and iterative linear solver
    NormX = BigDouble
    nIterNewton = 0
    do
       nIterNewton = nIterNewton + 1
       if(DoTest)write(*,*)NameSub,': nIterNewton=',nIterNewton
       if(nIterNewton > MaxIterNewton)then
          write(*,*)'Newton-Raphson failed to converge nIterNewton=', &
               nIterNewton
          if(IsTimeAccurate)call stop_mpi('Newton-Raphson failed to converge')
          EXIT
       endif

       ! Calculate Jacobian matrix if required
       if(ImplParam%DoPrecond)then

          if(nIterNewton > 1)then
             ! Update ghost cells for Impl_VGB,
             ! because it is needed by impl_jacobian
             call implicit2explicit(Impl_VGB(:,1:nI,1:nJ,1:nK,:))
             call exchange_messages
             call explicit2implicit(0,nI+1,j0_,nJp1_,k0_,nKp1_,Impl_VGB)
          end if

          call timing_start('impl_jacobian')

          ! Initialize variables for preconditioner calculation
          if(UseResistivity .and. .not. UseSemiResistivity) &
               call init_impl_resistivity
          if(UseHallResist .and. .not. UseSemiHallResist) &
               call init_impl_hall_resist

          ! Calculate approximate dR/dU matrix
          !$omp parallel do
          do iBlockImpl = 1, nBlockImpl
             call impl_jacobian(iBlockImpl, &
                  JacImpl_VVCIB(:,:,:,:,:,:,iBlockImpl))
          end do
          !$omp end parallel do
          call timing_stop('impl_jacobian')
       endif

       ! Update rhs and initial x_I if required
       if(nIterNewton > 1) call impl_newton_loop

       if(DoTest.and.nBlockImpl>0)write(*,*)NameSub,&
            ': initial x_I(test), rhs(test)=',x_I(nTest),Rhs_I(nTest)

       ! solve implicit system

       ! For Newton solver the outer loop has to converge,
       ! the inner loop only needs to reduce the error somewhat.
       if(UseNewton) ImplParam%ErrorMax = 0.1

       call timing_start('krylov')
       call test_start('krylov', DoTestKrylov)
       call solve_linear_multiblock(ImplParam, &
            nVar, nDim, nI, nJ, nK, 2*nDim+1, nBlockImpl, iComm, &
            impl_matvec, Rhs_I, x_I, DoTestKrylov, JacImpl_VVCIB)
       call test_stop('krylov', DoTestKrylov)
       call timing_stop('krylov')

       if(DoTest .and. nBlockImpl>0)&
            write(*,*)NameSub,': final     x_I(test)=',x_I(nTest)

       if(ImplParam%iError /= 0 .and. iProc == 0 .and. IsTimeAccurate) &
            call error_report(NameSub// &
            ': Krylov solver failure, Krylov error', &
            ImplParam%Error, iError1, .true.)

       ! Update w: Impl_VGB(k+1) = Impl_VGB(k) + x_I
       ! with coeff=1 or coeff<1 from backtracking (for steady state only)
       ! based on reducing the residual
       ! ||ResExpl_VCB(Impl_VGB+1)|| <= ||ResExpl_VCB(Impl_VGB)||.
       ! Also calculates ResImpl_VCB=DtExpl*R_loImpl_VGB+1
       ! and logical IsConverged.
       call impl_newton_update(NormX, IsConverged)

       if(DoTest.and.UseNewton) &
            write(*,*)NameSub,': NormX, converged=',NormX, IsConverged

       if(IsConverged) EXIT
    enddo ! Newton iteration

    ! Make the update conservative
    if(UseConservativeImplicit)call impl_newton_conserve

    ! Put back implicit result into the explicit code
    call implicit2explicit(Impl_VGB(:,1:nI,1:nJ,1:nK,:))

    if(DoFixAxisOrig)call fix_axis_cells
    if(UseCoarseAxisOrig)call coarsen_axis_cells
    ! Make explicit part available again for partially explicit scheme
    if(UsePartImplicit)then
       ! Restore Unused_B
       if(.not.UsePartImplicit2)then
          iNewDecomposition=mod(iNewDecomposition-3, 10000)
       else
          iNewDecomposition=mod(iNewDecomposition-1, 10000)
       end if
       Unused_BP(1:nBlockMax,:) = &
            iTypeAdvance_BP(1:nBlockMax,:) == SkippedBlock_
       Unused_B(1:nBlockMax) = Unused_BP(1:nBlockMax,iProc)
    endif

    ! Exchange messages, so ghost cells of all blocks are updated
    call exchange_messages

    if(DoTest.and.nBlockImpl>0)write(*,*)NameSub,': new w=',&
         Impl_VGB(iVarTest,iTest,jTest,kTest,iBlockImplTest)
    if(UseNewton.and.DoTest)write(*,*)NameSub,': final nIterNewton, NormX=',&
         nIterNewton, NormX

    ! Restore StateOld in the implicit blocks
    !$omp parallel do private( iBlock )
    do iBlockImpl = 1, nBlockImpl
       iBlock = iBlockFromImpl_B(iBlockImpl)
       StateOld_VGB(:,1:nI,1:nJ,1:nK,iBlock) = ImplOld_VCB(:,:,:,:,iBlock)

       if(UseImplicitEnergy) &
            call energy_to_pressure_cell(iBlock, StateOld_VGB)
    end do
    !$omp end parallel do

    if(UseUpdateCheckOrig .and. IsTimeAccurate .and. UseDtFixed)then

       ! Calculate the largest relative drop in density or pressure
       !$omp parallel do
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          ! Check p and rho
          Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) =&
               min(State_VGB(P_,1:nI,1:nJ,1:nK,iBlock) / &
               StateOld_VGB(P_,1:nI,1:nJ,1:nK,iBlock), &
               State_VGB(Rho_,1:nI,1:nJ,1:nK,iBlock) / &
               StateOld_VGB(Rho_,1:nI,1:nJ,1:nK,iBlock) )
       end do
       !$omp end parallel do

       if(index(StringTest, 'updatecheck') > 0)then
          pRhoRelativeMin = minval_grid(Tmp1_GB, iLoc_I=iLoc_I)
          if(iLoc_I(5) == iProc)then
             i = iLoc_I(1); j = iLoc_I(2); k = iLoc_I(3); iBlock = iLoc_I(4)
             write(*,*) 'pRhoRelativeMin is at i,j,k,iBlock,iProc = ',iLoc_I
             write(*,*) 'x,y,z =', Xyz_DGB(:,i,j,k,iBlock)
             write(*,*) 'RhoOld,pOld=', StateOld_VGB([Rho_,P_],i,j,k,iBlock)
             write(*,*) 'RhoNew,pNew=', State_VGB([Rho_,P_],i,j,k,iBlock)
             write(*,*) 'pRhoRelativeMin=', pRhoRelativeMin
          end if
       else
          pRhoRelativeMin = minval_grid(Tmp1_GB)
       end if
       if(pRhoRelativeMin < RejectStepLevel .or. ImplParam%iError /= 0)then
          ! Redo step if pressure decreased below RejectStepLevel
          ! or the Krylov iteration failed.
          Dt = 0.0
          ! Do not use previous step in BDF2 scheme
          nStepPrev = -1
          ! Reset the state variable, the energy and set DtMax_CB variable to 0
          !$omp parallel do
          do iBlock = 1,nBlock
             if(Unused_B(iBlock)) CYCLE
             State_VGB(:,1:nI,1:nJ,1:nK,iBlock) &
                  = StateOld_VGB(:,1:nI,1:nJ,1:nK,iBlock)
             DtMax_CB(1:nI,1:nJ,1:nK,iBlock)    = 0.0
          end do
          !$omp end parallel do
          ! Reduce next time step
          DtFixed = RejectStepFactor*DtFixed
          if(index(StringTest, 'updatecheck') > 0) write(*,*) NameSub, &
               ': RejectStepLevel, iError, DtFixed=', &
               RejectStepLevel, ImplParam%iError, DtFixed
       elseif(pRhoRelativeMin < ReduceStepLevel)then
          ! Reduce next time step if pressure is reduced below ReduceStepLevel
          DtFixed = ReduceStepFactor*DtFixed
          if(index(StringTest, 'updatecheck') > 0) write(*,*) NameSub, &
               ': ReduceStepLevel, DtFixed=', ReduceStepLevel, DtFixed
       elseif(pRhoRelativeMin > IncreaseStepLevel .and. Dt == DtFixed)then
          ! Increase next time step if change is above IncreaseStepLevel
          ! and the last step was taken with DtFixed. Do not exceed DtFixedOrig
          DtFixed = min(DtFixedOrig, DtFixed*IncreaseStepFactor)
          if(index(StringTest, 'updatecheck') > 0) write(*,*) NameSub, &
               ': IncreaseStepLevel, DtFixed=', IncreaseStepLevel, DtFixed
       end if

       if(DoTest) write(*,*) NameSub,': pRelMin,Dt,DtFixed=',&
            pRhoRelativeMin,Dt*No2Si_V(UnitT_), DtFixed*No2Si_V(UnitT_)
    endif

    ! Advance time by Dt
    tSimulation = TimeSimulationOrig + Dt*No2Si_V(UnitT_)

    ! Restore logicals
    UseUpdateCheck   = UseUpdateCheckOrig
    UsePointImplicit = UsePointImplicitOrig
    DoFixAxis        = DoFixAxisOrig
    UseCoarseAxis    = UseCoarseAxisOrig

    ! Done with implicit update
    IsImplicitUpdate = .false.

    call timing_stop(NameSub)

    call test_stop(NameSub, DoTest)
  end subroutine advance_part_impl
  !============================================================================
  subroutine impl_newton_init

    ! Initialization for NR

    use ModMain, ONLY : nStep,dt,nOrder, &
         UseRadDiffusion
    use ModAdvance, ONLY : TypeFlux
    use ModMpi
    use ModRadDiffusion, ONLY: IsNewTimestepRadDiffusion

    integer :: i, j, k, n, iVar, iBlockImpl, iBlock, iError
    real :: Coef1, Coef2, q1, q2, q3

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'impl_newton_init'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Calculate high and low order residuals
    ! ResExpl_VCB= DtExpl * R

    if(UseRadDiffusion) IsNewTimestepRadDiffusion = .true.

    !                not low,  dt,  subtract
    call get_residual(.false.,.true.,.true., &
         Impl_VGB(:,1:nI,1:nJ,1:nK,:),ResExpl_VCB)

    if(UseRadDiffusion) IsNewTimestepRadDiffusion = .false.

    if (nOrder==nOrderImpl .and. TypeFlux==TypeFluxImpl) then
       ! If R_low=R then ResImpl_VCB = ResExpl_VCB
       ResImpl_VCB(:,:,:,:,1:nBlockImpl) = ResExpl_VCB(:,:,:,:,1:nBlockImpl)
    else
       ! ResImpl_VCB = DtExpl * R_low
       !                  low,  no dt, subtract
       call get_residual(.true.,.false.,.true., &
            Impl_VGB(:,1:nI,1:nJ,1:nK,:), ResImpl_VCB)
    endif

    if(DoTest.and.nBlockImpl>0)write(*,*)'ResExpl_VCB,ResImpl_VCB(test)=',&
         ResExpl_VCB(iVarTest,iTest,jTest,kTest,iBlockImplTest),&
         ResImpl_VCB(iVarTest,iTest,jTest,kTest,iBlockImplTest)

    ! Calculate rhs used for nIterNewton=1
    if(UseBDF2 .and. nStep == nStepPrev + 1)then
       ! Collect RHS terms from Eq 8 in Paper implvac
       ! Newton-Raphson iteration. The BDF2 scheme implies
       ! beta+alpha=1 and beta=(dt_n+dt_n-1)/(2*dt_n+dt_n-1)
       Coef1 = ImplCoeff*DtCoeff
       Coef2 = (1 - ImplCoeff)*Dt/DtPrev

       !$omp parallel do private(iBlock, n)
       do iBlockImpl = 1, nBlockImpl
          iBlock = iBlockFromImpl_B(iBlockImpl)
          n = (iBlockImpl-1)*nIJK*nVar
          do k = 1, nK; do j = 1, nJ; do i = 1, nI; do iVar = 1, nVar
             n = n + 1
             ! For 1st Newton iteration
             ! RHS = dt*(beta*R + alpha*(w_n-w_n-1)/dt_n-1)/Norm_V
             Rhs_I(n) = (Coef1*ResExpl_VCB(iVar,i,j,k,iBlockImpl) &
                  + Coef2*(Impl_VGB(iVar,i,j,k,iBlockImpl) &
                  -        ImplOld_VCB(iVar,i,j,k,iBlock)))/Norm_V(iVar)
          end do; end do; enddo; enddo
       enddo
       !$omp end parallel do
    else
       !$omp parallel do private( n )
       do iBlockImpl = 1, nBlockImpl
          n = (iBlockImpl-1)*nIJK*nVar
          do k = 1, nK; do j = 1, nJ; do i = 1, nI; do iVar = 1, nVar
             n = n + 1
             ! RHS = dt*R/Norm_V for the first iteration
             Rhs_I(n) = ResExpl_VCB(iVar,i,j,k,iBlockImpl)*DtCoeff/Norm_V(iVar)
          end do; enddo; enddo; enddo
       end do
       !$omp end parallel do
    endif

    !$omp parallel do private( n )
    do iBlockImpl = 1, nBlockImpl
       n = (iBlockImpl-1)*nIJK*nVar
       do k = 1, nK; do j = 1, nJ; do i = 1, nI; do iVar = 1, nVar
          n = n + 1
          if(.not. IsImplCell_CB(i,j,k,iBlockFromImpl_B(iBlockImpl))) then
             Rhs_I(n) = 0 ! Do the same thing for Rhs0_I ?
          endif
       end do; enddo; enddo; enddo
    end do
    !$omp end parallel do

    if(UseNewton .or. UseConservativeImplicit)then
       ! Calculate RHS0 used for RHS when nIterNewton > 1
       !$omp parallel do private( n )
       do iBlockImpl = 1, nBlockImpl
          n = (iBlockImpl-1)*nIJK*nVar
          do k = 1, nK; do j = 1, nJ; do i = 1, nI; do iVar = 1, nVar
             n = n + 1
             ! RHS0 = [dt*(R - beta*R_low) + w_n]/Norm_V
             !     = RHS + [-beta*dt*R_low + w_n]/Norm_V
             Rhs0_I(n) = Rhs_I(n) &
                  + (-ImplCoeff*DtCoeff*ResImpl_VCB(iVar,i,j,k,iBlockImpl)&
                  + Impl_VGB(iVar,i,j,k,iBlockImpl))/Norm_V(iVar)
          end do; end do; enddo; enddo
       enddo
       !$omp end parallel do
    endif

    if(DoTest)then
       call MPI_allreduce(sum(ResImpl_VCB(:,:,:,:,1:nBlockImpl)**2),q1,&
            1,MPI_REAL,MPI_SUM,iComm,iError)
       call MPI_allreduce(sum(ResExpl_VCB(:,:,:,:,1:nBlockImpl)**2),q2,&
            1,MPI_REAL,MPI_SUM,iComm,iError)
       call MPI_allreduce(sum(Rhs_I(1:nImpl)**2),q3,&
            1,MPI_REAL,MPI_SUM,iComm,iError)

       if(DoTest)write(*,*)'Sum ResExpl_VCB**2,ResImpl_VCB**2,rhs**2:', &
            q1, q2, q3
    end if

    ! Initial guess for x_I = w_n+1 - w_n
    select case(TypeKrylovInit)
    case('explicit')
       ! w_n+1-w_n = dt * R_n
       x_I(1:nImpl) = Rhs_I(1:nImpl)
    case('scaled')
       ! Like explicit, but amplitude reduced
       ! w_n+1-w_n = DtExpl * R_n
       x_I(1:nImpl) = Rhs_I(1:nImpl)/DtCoeff
    case('nul')
       ! w_n+1-w_n = 0
       x_I(1:nImpl) = 0.0
    case('old')
    case default
       call stop_mpi(NameSub// &
            ': unknown type for TypeKrylovInit='//TypeKrylovInit)
    end select

    call test_stop(NameSub, DoTest)
  end subroutine impl_newton_init
  !============================================================================
  subroutine impl_newton_loop

    use ModMpi

    integer :: i,j,k,iVar,iBlockImpl,n, iError
    real    :: q1

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'impl_newton_loop'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Caculate RHS for 2nd or later Newton iteration
    !$omp parallel do private( n )
    do iBlockImpl = 1, nBlockImpl
       n = (iBlockImpl-1)*nIJK*nVar
       do k = 1, nK; do j = 1, nJ; do i = 1, nI; do iVar = 1, nVar
       n = n + 1
       ! RHS = (dt*R_n - beta*dt*R_n_low
       !       + w_n + beta*dt*R_k_low - Impl_VGB)/Norm
       ! use: RHS0 and ResImpl_VCB = DtExpl * R_k_low
       Rhs_I(n) = Rhs0_I(n) &
            + (ImplCoeff*DtCoeff*ResImpl_VCB(iVar,i,j,k,iBlockImpl) &
            - Impl_VGB(iVar,i,j,k,iBlockImpl))/Norm_V(iVar)
       enddo; enddo; enddo; enddo
    enddo
    !$omp end parallel do

    if(DoTest)then
       call MPI_allreduce(sum(Rhs_I(1:nImpl)**2),q1,1,MPI_REAL,MPI_SUM,&
            iComm,iError)
       if(DoTest)then
          write(*,*)'norm of rhs:',sqrt(q1/NimplTotal)
          if(nBlockImpl>0)write(*,*)'rhs,rhs0,ResImpl_VCB,Impl_VGB(test)=',&
               Rhs_I(nTest),Rhs0_I(nTest),               &
               ResImpl_VCB(kTest,iVarTest,iTest,jTest,iBlockImplTest),  &
               Impl_VGB(iVarTest,iTest,jTest,kTest,iBlockImplTest)
       end if
    end if

    ! Initial guess for x_I is always zero in later NR iterations
    x_I(1:nImpl) = 0.0

    call test_stop(NameSub, DoTest)
  end subroutine impl_newton_loop
  !============================================================================
  subroutine impl_newton_update(NormX, IsConverged)

    ! Update Impl_VGB(k+1) = Impl_VGB(k) + x_I

    use ModMpi

    real,    intent(out):: NormX
    logical, intent(out):: IsConverged

    integer:: i, j, k, iVar, iBlockImpl, n, iError
    real:: NormXLocal

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'impl_newton_update'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(UseNewton)then
       ! Calculate progress in NR scheme to set linear solver accuracy
       ! NormX = ||Impl_VGB(k+1) - Impl_VGB(k)||/||w_n||
       NormXLocal = sum(x_I(1:nImpl)**2)
       call MPI_allreduce(NormXLocal, NormX, 1, MPI_REAL, MPI_SUM, &
            iComm, iError)
       NormX = sqrt(NormX/NimplTotal)
       IsConverged = NormX < NewtonErrorMax
       if(DoTest)write(*,*)'NormX:',NormX
    else
       IsConverged = .true.
    endif

    ! w=w+x_I for all true cells
    !$omp parallel do private( n )
    do iBlockImpl = 1, nBlockImpl
       n = (iBlockImpl-1)*nIJK*nVar
       do k = 1, nK; do j = 1, nJ; do i = 1, nI; do iVar = 1, nVar
          n = n + 1
          if(IsImplCell_CB(i,j,k,iBlockFromImpl_B(iBlockImpl)))&
               Impl_VGB(iVar,i,j,k,iBlockImpl) = &
               Impl_VGB(iVar,i,j,k,iBlockImpl) &
               + x_I(n)*Norm_V(iVar)
       enddo; enddo; enddo; enddo
    enddo
    !$omp end parallel do

    if(UseConservativeImplicit .or. .not.IsConverged) then
       ! calculate low order residual ResImpl_VCB = DtExpl*RES_low(k+1)
       !                  low,   no dt, subtract
       call get_residual(.true., .false., .true., &
            Impl_VGB(:,1:nI,1:nJ,1:nK,:), ResImpl_VCB)
    end if

    call test_stop(NameSub, DoTest)
  end subroutine impl_newton_update
  !============================================================================
  subroutine impl_newton_conserve

    ! Replace the final Newton iterate Impl_VGB with a flux based
    ! conservative update

    integer :: i, j, k, iVar, n, iBlockImpl, iBlock
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'impl_newton_conserve'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! w = Rhs0*Norm_V + ImplCoeff*DtCoeff*ResImpl
    !
    ! Rhs0 is the normalized iteration independent part of the right hand side,
    ! which is calculated in impl_newton_init.
    !
    ! Norm_V converts each variable from the normalized (second norm=1)
    ! units to the units used in the explicit part BATSRUS
    !
    ! ResImpl is the (low order) residual obtained from the final newton
    ! iterate with a DtExpl time step. It is calculated in impl_newton_update.
    !
    ! DtCoeff = Dt/DtExpl is used to convert to the implicit time step

    !$omp parallel do private( iBlock,n )
    do iBlockImpl = 1, nBlockImpl
       iBlock = iBlockFromImpl_B(iBlockImpl)
       n = (iBlockImpl-1)*nIJK*nVar
       do k = 1, nK; do j = 1, nJ; do i = 1, nI; do iVar = 1, nVar
          n = n + 1
          if(IsImplCell_CB(i,j,k,iBlock)) &
               Impl_VGB(iVar,i,j,k,iBlockImpl) = &
               Rhs0_I(n)*Norm_V(iVar) &
               + ImplCoeff*DtCoeff*ResImpl_VCB(iVar,i,j,k,iBlockImpl)
       enddo; enddo; enddo; enddo
    enddo
    !$omp end parallel do

    call test_stop(NameSub, DoTest)
  end subroutine impl_newton_conserve
  !============================================================================
  subroutine impl_matvec(x_I, y_I, n)

    ! Calculate y=P_L.A.P_R.x for the iterative solver, where
    ! P_L and P_R are the left and right preconditioner matrices,
    ! A = I - beta*dt*dR/dw, and R is the residual from dw/dt = R(w).
    !
    ! The multiplication by A is done in a matrix free fashion.

    use ModLinearSolver, ONLY: &
         precond_left_multiblock, precond_right_multiblock

    integer, intent(in):: n
    real, intent(in)   :: x_I(n)
    real, intent(out)  :: y_I(n)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'impl_matvec'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(DoTest)write(*,*)NameSub,': initial n,sum(x**2)=', n, sum(x_I**2)

    if(ImplParam%DoPrecond)then

       ! y = P_R.x, where P_R = I, U^{-1}, or U^{-1}L^{-1}
       ! for left, symmetric and right preconditioning, respectively
       y_I = x_I
       call precond_right_multiblock(ImplParam, &
            nVar, nDim, nI, nJ, nK, nStencil, nBlockImpl, JacImpl_VVCIB, y_I)

       ! y = A.y
       call impl_matvec_free(y_I, y_I)

       ! y = P_L.y, where P_L==U^{-1}.L^{-1}, L^{-1}, or I
       ! for left, symmetric, and right preconditioning, respectively
       call precond_left_multiblock(ImplParam, &
            nVar, nDim, nI, nJ, nK, nStencil, nBlockImpl, JacImpl_VVCIB, y_I)
    else
       ! y = A.y
       call impl_matvec_free(x_I, y_I)
    end if

    if(DoTest)write(*,*) NameSub,': final n, sum(y**2)=', n, sum(y_I**2)

    call test_stop(NameSub, DoTest)
  end subroutine impl_matvec
  !============================================================================
  subroutine impl_matvec_free(x_I, y_I)

    ! Calculate y=L.x for the iterative solver, matrix-free
    ! where L= I - beta*dt*dR/dw   (dt=dt_implicit)
    !
    ! One sided derivative:

    ! ImplEps_VGB = Impl_VGB + eps*x            ! perturbation
    !
    ! ImplEps_VGB'=ImplEps_VGB + R(ImplEps_VGB,DtExpl)  ! advance ImplEps_VGB
    !
    ! dR/dw.x = (R(w+eps*x)-R(w))/eps
    !          = [(ImplEps_VGB'-ImplEps_VGB) - (Impl_VGB'-Impl_VGB)]/eps/DtExpl
    !          = (ImplEps_VGB'-Impl_VGB')/eps/DtExpl - x/DtExpl
    !
    ! L.x = dx - beta*dt*dR/dw.x
    !      = (1 + beta*DtCoeff)*x - beta*DtCoeff*(ImplEps_VGB' - w')/eps
    !
    ! where w=Impl_VGB, w'=w+R_low, beta=ImplCoeff, eps=sqrt(JacobianEps)/||x||
    ! instead of eps=(JacobianEps)^(1/2)*(Impl_VGB.x)/(x.x) suggested by Keyes

    use ModMpi

    real, intent(in)   :: x_I(nImpl)
    ! Sometimes this subroutine called with the same array in both arguments
    ! that's why the intent of y cannot be set to out.
    real, intent(inout):: y_I(nImpl)

    real, allocatable, save:: ImplEps_VCB(:,:,:,:,:)
    integer:: n, i, j, k, iVar, iBlock, iError
    real:: Eps, xNorm, xNormTotal, Coef1, Coef2

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'impl_matvec_free'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    call timing_start(NameSub)

    if(.not.allocated(ImplEps_VCB)) &
         allocate(ImplEps_VCB(nVar,nI,nJ,nK,MaxBlockImpl))

    xNorm = sum(x_I**2)
    call MPI_allreduce(xNorm, xNormTotal, 1, MPI_REAL, MPI_SUM,iComm,iError)

    if(DoTest)write(*,*) NameSub,': initial n,sum(x**2),xNormTotal=', &
         nImpl, xNorm, xNormTotal

    xNorm = sqrt(xNormTotal/NimplTotal)

    if(xNorm < SmallDouble) xNorm = 1.0

    Eps = sqrt(JacobianEps)/xNorm

    !$omp parallel do private( n )
    do iBlock = 1, nBlockImpl
       n = (iBlock-1)*nIJK*nVar
       do k = 1, nK; do j = 1, nJ; do i = 1, nI; do iVar = 1, nVar
          n = n + 1
          ImplEps_VCB(iVar,i,j,k,iBlock) = Impl_VGB(iVar,i,j,k,iBlock) &
               + Eps*x_I(n)*Norm_V(iVar)
       enddo; enddo; enddo; enddo
    enddo
    !$omp end parallel do

    ! Advance ImplEps_VCB:low order,  no dt, don't subtract
    call get_residual(.true., .false., .false., ImplEps_VCB, ImplEps_VCB)

    ! Calculate y = L.x = (1 + beta*DtCoeff)*x
    !                       - beta*DtCoeff*(ImplEps_VCB' - Impl_VGB')/eps
    ! where ImplEps_VCB  = Impl_VGB + eps*x,
    !       ImplEps_VCB' = ImplEps_VCB + dt*R(ImplEps_VCB) and
    !       Impl_VGB'    = Impl_VGB + dt*R(w)
    ! y = x + beta*DtCoeff*x
    !       - beta*DtCoeff*(Impl_VGB + eps*x + R(ImplEps_VCB) - w - R(w))/eps
    !   = x - beta*DtCoeff*(R(ImplEps_VCB)-R(Impl_VGB))/eps
    !   = x - beta*dt*dR/dU*x

    Coef1 = 1 + ImplCoeff*DtCoeff
    Coef2 = ImplCoeff*DtCoeff/Eps

    if(DoTest)write(*,*)'DtCoeff,ImplCoeff,Coef1,Coef2=', &
         DtCoeff, ImplCoeff, Coef1, Coef2

    !$omp parallel do private( n )
    do iBlock = 1, nBlockImpl
       n = (iBlock-1)*nIJK*nVar
       do k = 1, nK; do j = 1, nJ; do i = 1, nI; do iVar = 1, nVar
          n = n + 1
          y_I(n) = Coef1*x_I(n) - Coef2*(ImplEps_VCB(iVar,i,j,k,iBlock) &
               - Impl_VGB(iVar,i,j,k,iBlock) &
               - ResImpl_VCB(iVar,i,j,k,iBlock))/Norm_V(iVar)
          if(.not. IsImplCell_CB(i,j,k,iBlockFromImpl_B(iBlock))) then
             y_I(n) = Rhs_I(n)
          endif
       enddo; enddo; enddo; enddo
    enddo
    !$omp end parallel do

    call timing_stop(NameSub)

    if(DoTest)write(*,*) NameSub,': final n,sum(y**2)=', nImpl, sum(y_I**2)

    call test_stop(NameSub, DoTest)
  end subroutine impl_matvec_free
  !============================================================================
  subroutine impl_jacobian(iBlockImpl, Jac_VVCI)

    ! Calculate Jacobian matrix for block iBlockImpl:
    !
    !    JAC = I - dt*beta*dR/dw
    !
    ! using 1st order Rusanov scheme for the residual RES:
    !
    !    R_i = 1./Dx*[                 -(Fx_i+1/2 - Fx_i-1/2 )
    !                + 0.5*cmax_i+1/2 * (W_i+1    - W_i      )
    !                + 0.5*cmax_i-1/2 * (W_i-1    - W_i      )
    !                + 0.5*Q_i        * (Bx_i+1   - Bx_i-1   ) ]
    !         +1./Dy*[...]
    !         +1./Dz*[...]
    !         +S
    !
    ! where W contains the conservative variables,
    ! Fx, Fy, Fz are the fluxes, cmax is the maximum speed,
    ! Q are the coefficients B, U and U.B in the Powell source terms, and
    ! S are the local source terms.
    !
    !    Fx_i+1/2 = 0.5*(FxL_i+1/2 + FxR_i+1/2)
    !
    ! For first order scheme
    !
    !    FxL_i+1/2 = Fx[ W_i  , B0_i+1/2 ]
    !    FxR_i+1/2 = Fx[ W_i+1, B0_i+1/2 ]
    !
    ! We neglect terms containing d(cmax)/dW, and obtain a generalized eq.18:
    !
    ! Main diagonal stencil==1:
    !    dR_i/dW_i   = 0.5/Dx*[ (dFxR/dW-cmax)_i-1/2 - (dFxL/dW+cmax)_i+1/2 ]
    !                + 0.5/Dy*[ (dFyR/dW-cmax)_j-1/2 - (dFyL/dW+cmax)_j+1/2 ]
    !                + 0.5/Dz*[ (dFzR/dW-cmax)_k-1/2 - (dFzL/dW+cmax)_k+1/2 ]
    !                + dQ/dW_i*divB
    !                + dS/dW
    !
    ! Subdiagonal stencil==2:
    !    dR_i/dW_i-1 = 0.5/Dx* [ (dFxL/dW+cmax)_i-1/2
    !                           - Q_i*dBx_i-1/dW_i-1 ]
    ! Superdiagonal stencil==3:
    !    dR_i/dW_i+1 = 0.5/Dx* [-(dFxR/dW-cmax)_i+1/2
    !                           + Q_i*dBx_i+1/dW_i+1 ]
    !
    ! and similar terms for stencil=4,5,6,7.
    !
    ! The partial derivatives are calculated numerically
    ! (except for the trivial dQ/dW and dB/dW terms):
    !
    !  dF_iw/dW_jw = [F_iw(W + eps*W_jw) - F_iw(W)] / eps
    !  dS_iw/dW_jw = [S_iw(W + eps*W_jw) - S_iw(W)] / eps

    use ModMain
    use ModNumConst, ONLY: i_DD
    use ModvarIndexes
    use ModAdvance, ONLY: DtMax_CB
    use ModB0, ONLY: B0_DX, B0_DY, B0_DZ, set_b0_face
    use ModRadDiffusion, ONLY: add_jacobian_rad_diff
    use ModResistivity, ONLY: UseResistivity, add_jacobian_resistivity, &
         add_jacobian_hall_resist
    use ModHallResist, ONLY: UseHallResist, HallFactor_C, HallJ_CD, &
         set_hall_factor_cell
    use BATL_lib, ONLY: IsCartesianGrid, IsRzGeometry, &
         FaceNormal_DDFB, CellSize_DB, CellVolume_GB

    integer, intent(in) :: iBlockImpl
    real,    intent(out):: Jac_VVCI(nVar,nVar,nI,nJ,nK,nStencil)

    integer :: iBlock
    real, dimension(nVar,nI,nJ,nK)  :: Impl_VC, ImplEps_VC
    real, dimension(nI+1,nJ+1,nK+1) :: dFdWLeft_F, dFdWRight_F
    real, dimension(nVar,nI,nJ,nK)  :: s_VC, sEps_VC, sPowell_VC
    real:: DivB_C(nI,nJ,nK)
    real:: B0_DFD(MaxDim,nI+1,nJ+1,nK+1,MaxDim), Cmax_DF(MaxDim,nI+1,nJ+1,nK+1)

    real   :: Eps, Coeff
    integer:: i, j, k, i1, i2, i3, j1, j2, j3, k1, k2, k3
    integer:: iStencil, iVar, jVar, kVar, iDim

    real :: Dxyz_D(MaxDim)
    real :: FluxLeft_VFD(nVar,nI+1,nJ+1,nK+1,MaxDim) ! Unperturbed left flux
    real :: FluxRight_VFD(nVar,nI,nJ,nK,MaxDim)      ! Unperturbed right flux
    real :: FluxEpsLeft_VF(nVar,nI+1,nJ+1,nK+1)    ! Perturbed left flux
    real :: FluxEpsRight_VF(nVar,nI,nJ,nK)         ! Perturbed right flux
    real :: FaceArea_F(nI, nJ, nK)               ! Only the inner faces
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'impl_jacobian'
    !--------------------------------------------------------------------------
    iBlock = iBlockFromImpl_B(iBlockImpl)
    call test_start(NameSub, DoTest, iBlock)

    Eps = sqrt(JacobianEps)

    ! Extract state for this block
    Impl_VC = Impl_VGB(1:nVar,1:nI,1:nJ,1:nK,iBlockImpl)
    Dxyz_D = CellSize_DB(:,iBlock)
    if(UseB0)then
       call set_b0_face(iBlock)
       B0_DFD(:,1:nI+1,1:nJ  ,1:nK  ,x_) = B0_DX(:,1:nI+1,1:nJ,1:nK)
       B0_DFD(:,1:nI  ,1:nJ+1,1:nK  ,y_) = B0_DY(:,1:nI,1:nJ+1,1:nK)
       B0_DFD(:,1:nI  ,1:nJ  ,1:nK+1,z_) = B0_DZ(:,1:nI,1:nJ,1:nK+1)
    else
       B0_DFD = 0.0
    end if

    if(UseHallResist)call impl_init_hall

    ! Initialize matrix to zero (to be safe)
    Jac_VVCI = 0.0

    ! Initialize reference flux and the cmax array
    do iDim = 1, nDim

       i1 = 1+i_DD(1,iDim); j1= 1+i_DD(2,iDim); k1= 1+i_DD(3,iDim)
       i2 =nI+i_DD(1,iDim); j2=nJ+i_DD(2,iDim); k2=nK+i_DD(3,iDim)

       call get_face_flux(Impl_VC,B0_DFD(:,i1:i2,j1:j2,k1:k2,iDim),&
            nI, nJ, nK, iDim, iBlock, FluxLeft_VFD(:,i1:i2,j1:j2,k1:k2,iDim))

       call get_face_flux(Impl_VC,B0_DFD(:,1:nI,1:nJ,1:nK,iDim),&
            nI, nJ, nK, iDim, iBlock, FluxRight_VFD(:,:,:,:,iDim))

       ! Average w for each cell interface into ImplEps_VC
       i1 = 1-i_DD(1,iDim); j1 = 1-i_DD(2,iDim); k1 = 1-i_DD(3,iDim)

       ! Calculate orthogonal cmax for each interface in advance
       call get_cmax_face(                            &
            0.5*(Impl_VGB(1:nVar,1:i2,1:j2,1:k2,iBlockImpl)+ &
            Impl_VGB(1:nVar,i1:nI,j1:nJ,k1:nK,iBlockImpl)),     &
            B0_DFD(:,1:i2,1:j2,1:k2,iDim),       &
            i2,j2,k2,iDim,iBlock,Cmax_DF(iDim,1:i2,1:j2,1:k2))

       ! cmax always occurs as -ImplCoeff*0.5/dx*cmax
       Coeff = -0.5
       Cmax_DF(iDim,1:i2,1:j2,1:k2) = Coeff*Cmax_DF(iDim,1:i2,1:j2,1:k2)
    enddo

    ! Initialize divB and sPowell_VC arrays
    if(UseB .and. UseDivBSource) call impl_divbsrc_init

    ! Set s_VC=S(Impl_VC)
    if(UseImplSource) call get_impl_source(iBlock, Impl_VC, s_VC)

    ! The w to be perturbed and jVar is the index for the perturbed variable
    ImplEps_VC = Impl_VC

    do jVar = 1, nVar
       ! Remove perturbation from previous jVar if there was a previous one
       if(jVar>1) ImplEps_VC(jVar-1,:,:,:) = Impl_VC(jVar-1,:,:,:)

       ! Perturb new jVar variable
       Coeff = Eps*Norm_V(jVar)
       ImplEps_VC(jVar,:,:,:) = Impl_VC(jVar,:,:,:) + Coeff

       do iDim = 1, nDim
          ! Index limits for faces and shifted centers
          i1 = 1+i_DD(1,iDim); j1= 1+i_DD(2,iDim); k1= 1+i_DD(3,iDim)
          i2 =nI+i_DD(1,iDim); j2=nJ+i_DD(2,iDim); k2=nK+i_DD(3,iDim);
          i3 =nI-i_DD(1,iDim); j3=nJ-i_DD(2,iDim); k3=nK-i_DD(3,iDim);

          call get_face_flux(ImplEps_VC,B0_DFD(:,i1:i2,j1:j2,k1:k2,iDim),&
               nI,nJ,nK,iDim,iBlock,FluxEpsLeft_VF(:,i1:i2,j1:j2,k1:k2))

          call get_face_flux(ImplEps_VC,B0_DFD(:,1:nI,1:nJ,1:nK,iDim),&
               nI,nJ,nK,iDim,iBlock,FluxEpsRight_VF)

          ! Calculate dfdw=(feps-f0)/eps for each iVar variable and both
          ! left and right sides
          do iVar = 1, nVar

             ! call getflux(ImplEps_VC,B0_DFD(:,i1:i2,j1:j2,k1:k2,iDim),&
             !     nI,nJ,nK,iVar,iDim,iBlockImpl,fepsLface(i1:i2,j1:j2,k1:k2))

             ! call getflux(ImplEps_VC,B0_DFD(:,1:nI,1:nJ,1:nK,iDim),&
             !     nI,nJ,nK,iVar,iDim,iBlockImpl,fepsRface(1:nI,1:nJ,1:nK))

             ! dfdw = F_iw(W + eps*W_jVar) - F_iw(W)] / eps is multiplied by
             ! -0.5 in all formulae
             Coeff = -0.5/(Eps*Norm_V(jVar))

             dFdWLeft_F(i1:i2,j1:j2,k1:k2) = Coeff*&
                  (FluxEpsLeft_VF(iVar,i1:i2,j1:j2,k1:k2) &
                  -  FluxLeft_VFD(iVar,i1:i2,j1:j2,k1:k2,iDim))
             dFdWRight_F( 1:nI, 1:nJ, 1:nK) = Coeff*&
                  (FluxEpsRight_VF(iVar,1:nI,1:nJ,1:nK) &
                  -  FluxRight_VFD(iVar,1:nI,1:nJ,1:nK,iDim))

             if(DoTest)write(*,'(a,i1,i2,6(f15.8))') &
                  'iVar,jVar,f0L,fepsL,dfdwL,R:', &
                  iVar,jVar,&
                  FluxLeft_VFD(iVar,iTest,jTest,kTest,iDim),&
                  FluxEpsLeft_VF(iVar,iTest,jTest,kTest),&
                  dFdWLeft_F(iTest,jTest,kTest),&
                  FluxRight_VFD(iVar,iTest,jTest,kTest,iDim),&
                  FluxEpsRight_VF(iVar,iTest,jTest,kTest),&
                  dFdWRight_F(iTest,jTest,kTest)

             ! Add contribution of cmax to dfdwL and dfdwR
             if(iVar == jVar)then
                ! FxL_i-1/2 <-- (FxL + cmax)_i-1/2
                dFdWLeft_F(i1:i2,j1:j2,k1:k2) = dFdWLeft_F(i1:i2,j1:j2,k1:k2)&
                     + Cmax_DF(iDim,i1:i2,j1:j2,k1:k2)
                ! FxR_i+1/2 <-- (FxR - cmax)_i+1/2
                dFdWRight_F(1:nI,1:nJ,1:nK) = dFdWRight_F(1:nI,1:nJ,1:nK) &
                     - Cmax_DF(iDim,1:nI,1:nJ,1:nK)
             endif

             ! Divide flux*area by volume
             dFdWLeft_F(i1:i2,j1:j2,k1:k2) = dFdWLeft_F(i1:i2,j1:j2,k1:k2) &
                  /CellVolume_GB(1:nI,1:nJ,1:nK,iBlock)
             dFdWRight_F(1:nI,1:nJ,1:nK) = dFdWRight_F(1:nI,1:nJ,1:nK) &
                  /CellVolume_GB(1:nI,1:nJ,1:nK,iBlock)

             ! Contribution of fluxes to main diagonal (middle cell)
             ! dR_i/dW_i = 0.5/Dx*[(dFxR/dW-cmax)_i-1/2 - (dFxL/dW+cmax)_i+1/2]

             Jac_VVCI(iVar,jVar,:,:,:,1) = Jac_VVCI(iVar,jVar,:,:,:,1) &
                  - dFdWRight_F( 1:nI, 1:nJ, 1:nK)     &
                  + dFdWLeft_F(i1:i2,j1:j2,k1:k2)

             ! Add Q*dB/dw to dfdwL and dfdwR for upper and lower diagonals
             ! These diagonals are non-zero for the inside interfaces only
             ! which corresponds to the range i1:nI,j1:nJ,k1:nK.
             if(UseB    .and. UseDivBSource .and. &
                  (     (iVar >= RhoUx_ .and. iVar <= RhoUz_) &
                  .or.  (iVar >= Bx_    .and. iVar <= Bz_   ) &
                  .or.  (iVar == E_ .and. UseImplicitEnergy)  &
                  ) )then
                if(.not.IsCartesianGrid &
                     .and. jVar>=Bx_ .and. jVar<=B_+nDim)then
                   ! The source terms are always multiplied by Coeff
                   Coeff=-0.5
                   ! Get the corresponding face area
                   FaceArea_F(i1:nI,j1:nJ,k1:nK) = &
                        FaceNormal_DDFB(jVar-B_,iDim,i1:nI,j1:nJ,k1:nK,iBlock)

                   ! Relative to the right face flux Q is shifted to the left
                   dFdWLeft_F(i1:nI,j1:nJ,k1:nK) =                 &
                        dFdWLeft_F(i1:nI,j1:nJ,k1:nK)              &
                        + Coeff*sPowell_VC(iVar,i1:nI,j1:nJ,k1:nK) &
                        *FaceArea_F(i1:nI,j1:nJ,k1:nK)             &
                        /CellVolume_GB(i1:nI,j1:nJ,k1:nK,iBlock)

                   dFdWRight_F(i1:nI,j1:nJ,k1:nK) =             &
                        dFdWRight_F(i1:nI,j1:nJ,k1:nK)          &
                        + Coeff*sPowell_VC(iVar,1:i3,1:j3,1:k3) &
                        *FaceArea_F(i1:nI,j1:nJ,k1:nK)          &
                        /CellVolume_GB(1:i3,1:j3,1:k3,iBlock)

                elseif(jVar==B_+iDim)then
                   ! The source terms are always multiplied by Coeff
                   Coeff=-0.5/Dxyz_D(iDim)

                   ! Relative to the right face flux Q is shifted to the left
                   dFdWLeft_F(i1:nI,j1:nJ,k1:nK) =              &
                        dFdWLeft_F(i1:nI,j1:nJ,k1:nK)           &
                        + Coeff*sPowell_VC(iVar,i1:nI,j1:nJ,k1:nK)

                   dFdWRight_F(i1:nI,j1:nJ,k1:nK) =             &
                        dFdWRight_F(i1:nI,j1:nJ,k1:nK)          &
                        + Coeff*sPowell_VC(iVar,1:i3,1:j3,1:k3)
                end if
             end if
             Jac_VVCI(iVar,jVar,i1:nI,j1:nJ,k1:nK,2*iDim  ) = &
                  -dFdWLeft_F(i1:nI,j1:nJ,k1:nK)
             Jac_VVCI(iVar,jVar, 1:i3, 1:j3, 1:k3,2*iDim+1) = &
                  +dFdWRight_F(i1:nI,j1:nJ,k1:nK)
          enddo ! iVar
       enddo ! iDim

       if(DoTest)then
          write(*,*)'After fluxes jVar=',jVar,' stencil, row, JAC'
          do iStencil = 1, nStencil
             do kVar = 1, nVar
                write(*,'(i1,a,i1,a,20(f9.5))')iStencil,',',kVar,':',&
                     Jac_VVCI(:,kVar,iTest,jTest,kTest,iStencil)
             end do
          enddo
       endif

       ! Derivatives of local source terms
       if(UseImplSource)then

          ! w2=S(Impl_VC+eps*W_jVar)
          call get_impl_source(iBlock, ImplEps_VC, sEps_VC)

          Coeff = 1.0/(Eps*Norm_V(jVar))
          do iVar = 1, nVar
             ! JAC(..1) += dS/dW_jVar
             Jac_VVCI(iVar,jVar,:,:,:,1) = Jac_VVCI(iVar,jVar,:,:,:,1)&
                  + Coeff*(sEps_VC(iVar,:,:,:) - s_VC(iVar,:,:,:))
          enddo
       endif
    enddo

    if(DoTest) &
         write(*,*)'After fluxes and sources:  Jac_VVCI(...,1):', &
         Jac_VVCI(1:nVar,1:nVar,iTest,jTest,kTest,1)

    ! Contribution of middle to Powell's source terms
    if(UseB .and. UseDivBSource)then
       ! JAC(...1) += d(Q/divB)/dW*divB
       call impl_divbsrc_middle

       if(DoTest)then
          write(*,*)'After divb sources: row, JAC(...,1):'
          do kVar = 1, nVar
             write(*,'(i1,a,20(f9.5))')kVar,':',&
                  Jac_VVCI(:,kVar,iTest,jTest,kTest,1)
          end do
       end if
    end if

    ! Add extra terms for regular and Hall resistivity
    if(UseResistivity .and. .not.UseSemiResistivity) &
         call add_jacobian_resistivity(iBlock, nVar, Jac_VVCI)
    if(UseHallResist .and. .not.UseSemiHallResist) &
         call add_jacobian_hall_resist(iBlock, nVar, Jac_VVCI)

    ! Add extra terms for radiative diffusion
    if(UseRadDiffusion) &
         call add_jacobian_rad_diff(iBlock, nVar, Jac_VVCI)

    ! Multiply JAC by the implicit timestep dt, ImplCoeff, Norm_V, and -1
    if(IsTimeAccurate)then
       do iStencil = 1, nStencil; do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(IsImplCell_CB(i,j,k,iBlock))then
             do jVar = 1, nVar; do iVar = 1, nVar
                Jac_VVCI(iVar,jVar,i,j,k,iStencil) = &
                     -Jac_VVCI(iVar,jVar,i,j,k,iStencil) &
                     *dt*ImplCoeff*Norm_V(jVar)/Norm_V(iVar)
             end do; end do
          else
             ! Set JAC = 0.0 inside body
             Jac_VVCI(:,:,i,j,k,iStencil) = 0.0
          end if
       end do; end do; end do; end do
    else
       ! Local time stepping has DtMax_CB=0.0 inside the body
       do iStencil = 1, nStencil; do k = 1, nK; do j = 1, nJ; do i = 1, nI
          do jVar = 1, nVar; do iVar = 1, nVar
             Jac_VVCI(iVar,jVar,i,j,k,iStencil) = &
                  -Jac_VVCI(iVar,jVar,i,j,k,iStencil) &
                  *DtMax_CB(i,j,k,iBlock)*CflImpl*ImplCoeff &
                  *Norm_V(jVar)/Norm_V(iVar)
          end do; end do;
       end do; end do; end do; end do
    endif

    if(DoTest)then
       write(*,*)'After boundary correction and *dt: row, JAC(...,1):'
       do kVar = 1, nVar
          write(*,'(i1,a,20(f9.5))')kVar,':',&
               Jac_VVCI(:,kVar,iTest,jTest,kTest,1)
       end do
    end if

    ! Add unit matrix to main diagonal
    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       do iVar = 1, nVar
          Jac_VVCI(iVar,iVar,i,j,k,1) = Jac_VVCI(iVar,iVar,i,j,k,1) + 1.0
       end do
    end do; end do; end do

    if(DoTest)then
       write(*,*)'After adding I: row, JAC(...,1):'
       do kVar = 1, nVar
          write(*,'(i1,a,20(f9.5))')kVar,':',&
               Jac_VVCI(:,kVar,iTest,jTest,kTest,1)
       end do
    end if

    call test_stop(NameSub, DoTest, iBlock)
  contains
    !==========================================================================
    subroutine impl_divbsrc_init

      ! Calculate div B for middle cell contribution to Powell's source terms
      !------------------------------------------------------------------------
      if(IsCartesianGrid)then
         if(IsRzGeometry)call stop_mpi('impl_divbsrc_init not working for RZ')

         DivB_C = &
              ( Impl_VGB(Bx_,2:nI+1,1:nJ,1:nK,iBlockImpl)           &
              - Impl_VGB(Bx_,0:nI-1,1:nJ,1:nK,iBlockImpl))/Dxyz_D(x_)
         if(nJ>1) DivB_C = DivB_C &
              +(Impl_VGB(By_,1:nI,2:nJ+1,1:nK,iBlockImpl)           &
              - Impl_VGB(By_,1:nI,0:nJ-1,1:nK,iBlockImpl))/Dxyz_D(y_)
         if(nK>1) DivB_C = DivB_C &
              +(Impl_VGB(Bz_,1:nI,1:nJ,2:nK+1,iBlockImpl)           &
              - Impl_VGB(Bz_,1:nI,1:nJ,0:nK-1,iBlockImpl))/Dxyz_D(z_)
         DivB_C = 0.5*DivB_C

      else
         do k = 1, nK; do j = 1, nJ; do i = 1, nI
            DivB_C(i,j,k) = &
                 sum (Impl_VGB(Bx_:B_+nDim,i+1,j,k,iBlockImpl) &
                 *    FaceNormal_DDFB(:,1,i+1,j,k,iBlock))&
                 -sum(Impl_VGB(Bx_:B_+nDim,i-1,j,k,iBlockImpl) &
                 *    FaceNormal_DDFB(:,1,i,j,k,iBlock))  &
                 +sum(Impl_VGB(Bx_:B_+nDim,i,j+1,k,iBlockImpl) &
                 *    FaceNormal_DDFB(:,2,i,j+1,k,iBlock))&
                 -sum(Impl_VGB(Bx_:B_+nDim,i,j-1,k,iBlockImpl) &
                 *    FaceNormal_DDFB(:,2,i,j,k,iBlock))

            if(nK>1) DivB_C(i,j,k) = DivB_C(i,j,k) &
                 +sum(Impl_VGB(Bx_:B_+nDim,i,j,k+1,iBlockImpl) &
                 *    FaceNormal_DDFB(:,3,i,j,k+1,iBlock))&
                 -sum(Impl_VGB(Bx_:B_+nDim,i,j,k-1,iBlockImpl) &
                 *    FaceNormal_DDFB(:,3,i,j,k,iBlock))

            DivB_C(i,j,k) = 0.5/CellVolume_GB(i,j,k,iBlock)*DivB_C(i,j,k)

         end do; end do; end do
      end if

      ! Make sure that sPowell_VC is defined for all indexes
      sPowell_VC = 0.0
      do k = 1, nK; do j = 1, nJ; do i = 1, nI
         ! Calculate coefficients Q that multiply div B in Powell source terms
         ! Q(rhoU)= B
         sPowell_VC(RhoUx_:RhoUz_,i,j,k) = Impl_VC(Bx_:Bz_,i,j,k)

         ! Q(B)   = U
         sPowell_VC(Bx_:Bz_,i,j,k) = Impl_VC(RhoUx_:RhoUz_,i,j,k) &
              /Impl_VC(Rho_,i,j,k)

         if(.not. UseImplicitEnergy) CYCLE
         ! Q(E)   = U.B
         sPowell_VC(E_,i,j,k) = &
              sum(Impl_VC(Bx_:Bz_,i,j,k)*Impl_VC(RhoUx_:RhoUz_,i,j,k)) &
              /Impl_VC(Rho_,i,j,k)
      end do; end do; end do

    end subroutine impl_divbsrc_init
    !==========================================================================
    subroutine impl_divbsrc_middle

      integer:: i,j,k

      ! JAC(...1) += dQ/dW_i*divB

      ! Q(rhoU)= -divB*B
      ! dQ(rhoU)/dB = -divB
      !------------------------------------------------------------------------
      do k = 1, nK; do j = 1, nJ; do i = 1, nI
         Jac_VVCI(rhoUx_,Bx_,i,j,k,1)=Jac_VVCI(rhoUx_,Bx_,i,j,k,1)&
              - DivB_C(i,j,k)
         Jac_VVCI(rhoUy_,By_,i,j,k,1)=Jac_VVCI(rhoUy_,By_,i,j,k,1)&
              - DivB_C(i,j,k)
         Jac_VVCI(rhoUz_,Bz_,i,j,k,1)=Jac_VVCI(rhoUz_,Bz_,i,j,k,1)&
              - DivB_C(i,j,k)

         ! Q(B)= -divB*rhoU/rho
         ! dQ(B)/drho = +divB*rhoU/rho**2
         Jac_VVCI(Bx_,rho_,i,j,k,1)=Jac_VVCI(Bx_,rho_,i,j,k,1) &
              + DivB_C(i,j,k) &
              *Impl_VC(rhoUx_,i,j,k)/Impl_VC(rho_,i,j,k)**2
         Jac_VVCI(By_,rho_,i,j,k,1)=Jac_VVCI(By_,rho_,i,j,k,1) &
              + DivB_C(i,j,k) &
              *Impl_VC(rhoUy_,i,j,k)/Impl_VC(rho_,i,j,k)**2
         Jac_VVCI(Bz_,rho_,i,j,k,1)=Jac_VVCI(Bz_,rho_,i,j,k,1) &
              + DivB_C(i,j,k) &
              *Impl_VC(rhoUz_,i,j,k)/Impl_VC(rho_,i,j,k)**2

         ! dQ(B)/drhoU= -divB/rho
         Jac_VVCI(Bx_,rhoUx_,i,j,k,1)=Jac_VVCI(Bx_,rhoUx_,i,j,k,1)&
              - DivB_C(i,j,k)/Impl_VC(rho_,i,j,k)
         Jac_VVCI(By_,rhoUy_,i,j,k,1)=Jac_VVCI(By_,rhoUy_,i,j,k,1)&
              - DivB_C(i,j,k)/Impl_VC(rho_,i,j,k)
         Jac_VVCI(Bz_,rhoUz_,i,j,k,1)=Jac_VVCI(Bz_,rhoUz_,i,j,k,1)&
              - DivB_C(i,j,k)/Impl_VC(rho_,i,j,k)

         if(.not.UseImplicitEnergy) CYCLE

         ! Q(E)= -divB*rhoU.B/rho
         ! dQ(E)/drho = +divB*rhoU.B/rho**2
         Jac_VVCI(E_,rho_,i,j,k,1)=Jac_VVCI(E_,rho_,i,j,k,1)&
              + DivB_C(i,j,k)*&
              (Impl_VC(rhoUx_,i,j,k)*Impl_VC(Bx_,i,j,k)&
              +Impl_VC(rhoUy_,i,j,k)*Impl_VC(By_,i,j,k)&
              +Impl_VC(rhoUz_,i,j,k)*Impl_VC(Bz_,i,j,k))&
              /Impl_VC(rho_,i,j,k)**2

         ! dQ(E)/drhoU = -divB*B/rho
         Jac_VVCI(E_,rhoUx_,i,j,k,1)=Jac_VVCI(E_,rhoUx_,i,j,k,1) &
              - DivB_C(i,j,k) &
              *Impl_VC(Bx_,i,j,k)/Impl_VC(rho_,i,j,k)
         Jac_VVCI(E_,rhoUy_,i,j,k,1)=Jac_VVCI(E_,rhoUy_,i,j,k,1) &
              - DivB_C(i,j,k) &
              *Impl_VC(By_,i,j,k)/Impl_VC(rho_,i,j,k)
         Jac_VVCI(E_,rhoUz_,i,j,k,1)=Jac_VVCI(E_,rhoUz_,i,j,k,1) &
              - DivB_C(i,j,k) &
              *Impl_VC(Bz_,i,j,k)/Impl_VC(rho_,i,j,k)

         ! dQ(E)/dB = -divB*rhoU/rho
         Jac_VVCI(E_,Bx_,i,j,k,1)=Jac_VVCI(E_,Bx_,i,j,k,1) &
              - DivB_C(i,j,k) &
              *Impl_VC(rhoUx_,i,j,k)/Impl_VC(rho_,i,j,k)
         Jac_VVCI(E_,By_,i,j,k,1)=Jac_VVCI(E_,By_,i,j,k,1) &
              - DivB_C(i,j,k) &
              *Impl_VC(rhoUy_,i,j,k)/Impl_VC(rho_,i,j,k)
         Jac_VVCI(E_,Bz_,i,j,k,1)=Jac_VVCI(E_,Bz_,i,j,k,1) &
              - DivB_C(i,j,k) &
              *Impl_VC(rhoUz_,i,j,k)/Impl_VC(rho_,i,j,k)
      end do; end do; end do

    end subroutine impl_divbsrc_middle
    !==========================================================================
    subroutine impl_init_hall

      ! Calculate cell centered currents to be used by getflux

      use ModGeometry, ONLY: DgenDxyz_DDC, set_block_jacobian_cell

      real :: DbDgen_DD(3,3)

      real :: InvDx2, InvDy2, InvDz2

      logical:: DoTest
      character(len=*), parameter:: NameSub = 'impl_init_hall'
      !------------------------------------------------------------------------
      call test_start(NameSub, DoTest, iBlock)

      call set_hall_factor_cell(iBlock)

      InvDx2 = 0.5/Dxyz_D(x_); InvDy2 = 0.5/Dxyz_D(y_); InvDz2 = 0.5/Dxyz_D(z_)

      if(IsCartesianGrid)then

         do k = 1, nK; do j = 1, nJ; do i = 1, nI
            ! Jx = dBz/dy - dBy/dz
            if(nJ>1) HallJ_CD(i,j,k,x_) =                    &
                 +InvDy2*(Impl_VGB(Bz_,i,j+1,k,iBlockImpl)      &
                 -        Impl_VGB(Bz_,i,j-1,k,iBlockImpl))
            if(nK>1) HallJ_CD(i,j,k,x_) = HallJ_CD(i,j,k,x_) &
                 -InvDz2*(Impl_VGB(By_,i,j,k+1,iBlockImpl)      &
                 -        Impl_VGB(By_,i,j,k-1,iBlockImpl))
         end do; end do; end do

         do k = 1, nK; do j = 1, nJ; do i = 1, nI
            ! Jy = dBx/dz - dBz/dx
            HallJ_CD(i,j,k,y_) = &
                 -InvDx2*(Impl_VGB(Bz_,i+1,j,k,iBlockImpl)      &
                 -        Impl_VGB(Bz_,i-1,j,k,iBlockImpl))
            if(nK>1) HallJ_CD(i,j,k,y_) = HallJ_CD(i,j,k,y_) &
                 +InvDz2*(Impl_VGB(Bx_,i,j,k+1,iBlockImpl)      &
                 -        Impl_VGB(Bx_,i,j,k-1,iBlockImpl))
         end do; end do; end do

         do k = 1, nK; do j = 1, nJ; do i = 1, nI
            ! Jz = dBy/dx - dBx/dy
            HallJ_CD(i,j,k,z_) = &
                 +InvDx2*(Impl_VGB(By_,i+1,j,k,iBlockImpl)      &
                 -        Impl_VGB(By_,i-1,j,k,iBlockImpl))
            if(nJ>1) HallJ_CD(i,j,k,z_) = HallJ_CD(i,j,k,z_) &
                 -InvDy2*(Impl_VGB(Bx_,i,j+1,k,iBlockImpl)      &
                 -        Impl_VGB(Bx_,i,j-1,k,iBlockImpl))
         end do; end do; end do

      else

         call set_block_jacobian_cell(iBlock)

         DbDgen_DD = 0.0 !!! make it MaxDim*nDim and use Dim1_, Dim2_, Dim3_

         do k = 1, nK; do j = 1, nJ; do i = 1, nI
            DbDgen_DD(:,1) = InvDx2*&
                 (Impl_VGB(Bx_:Bz_,i+1,j,k,iBlockImpl) &
                 -Impl_VGB(Bx_:Bz_,i-1,j,k,iBlockImpl))
            if(nJ>1) DbDgen_DD(:,2) = InvDy2* &
                 (Impl_VGB(Bx_:Bz_,i,j+1,k,iBlockImpl) &
                 -Impl_VGB(Bx_:Bz_,i,j-1,k,iBlockImpl))
            if(nK>1) DbDgen_DD(:,3) = InvDz2* &
                 (Impl_VGB(Bx_:Bz_,i,j,k+1,iBlockImpl) &
                 -Impl_VGB(Bx_:Bz_,i,j,k-1,iBlockImpl))

            ! Jx = dBz/dy - dBy/dz
            if(nJ>1) HallJ_CD(i,j,k,x_) = &
                 + sum(DbDgen_DD(z_,:)*DgenDxyz_DDC(:,y_,i,j,k))
            if(nK>1) HallJ_CD(i,j,k,x_) = HallJ_CD(i,j,k,x_) &
                 - sum(DbDgen_DD(y_,:)*DgenDxyz_DDC(:,z_,i,j,k))

            ! Jy = dBx/dz - dBz/dx
            HallJ_CD(i,j,k,y_) = &
                 - sum(DbDgen_DD(z_,:)*DgenDxyz_DDC(:,x_,i,j,k))
            if(nK>1)HallJ_CD(i,j,k,y_) = HallJ_CD(i,j,k,y_) &
                 + sum(DbDgen_DD(x_,:)*DgenDxyz_DDC(:,z_,i,j,k))

            ! Jz = dBy/dx - dBx/dy
            HallJ_CD(i,j,k,z_) = &
                 + sum(DbDgen_DD(y_,:)*DgenDxyz_DDC(:,x_,i,j,k))
            if(nJ>1) HallJ_CD(i,j,k,z_) = HallJ_CD(i,j,k,z_) &
                 - sum(DbDgen_DD(x_,:)*DgenDxyz_DDC(:,y_,i,j,k))

         end do; end do; end do

      end if
      if(DoTest) write(*,*) NameSub, &
           ' HallJ_CD=',HallJ_CD(iTest,jTest,kTest,:)

      do k = 1, nK; do j = 1, nJ; do i = 1, nI
         HallJ_CD(i,j,k,:) = HallFactor_C(i,j,k)*HallJ_CD(i,j,k,:)
      end do; end do; end do

      call test_stop(NameSub, DoTest, iBlock)
    end subroutine impl_init_hall
    !==========================================================================
  end subroutine impl_jacobian
  !============================================================================
  subroutine implicit_init

    ! Set number of implicit blocks and variables,
    ! and conversion array between explicit and implicit block indices
    ! The implicit blocks are contiguous (all used) from 1 ... nBlockImpl

    use ModMain
    use ModAdvance, ONLY: iTypeAdvance_B, ImplBlock_
    use ModGeometry, ONLY: r_GB
    use BATL_lib,  ONLY: Used_GB

    integer :: iBlock, iBlockImpl
    integer :: i, j, k
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'implicit_init'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Fix parameters if needed
    if(ImplParam%nKrylovVector > ImplParam%MaxMatvec) &
         ImplParam%nKrylovVector = ImplParam%MaxMatvec

    ! Newton iterations will use zero initial guess (for simplicity)
    if(UseNewton)then
       TypeKrylovInit = 'nul'
       ImplParam%UseInitialGuess = .false.
    end if

    nBlockImpl = count(iTypeAdvance_B(1:nBlock) == ImplBlock_)

    ! Check for too many implicit blocks
    if(nBlockImpl > MaxBlockImpl)then
       write(*,*)'ERROR: Too many implicit blocks!'
       write(*,*)'MaxBlockImpl < nBlockImpl :',MaxBlockImpl,nBlockImpl
       call stop_mpi( &
            'Change number of processors,'// &
            ' reduce number of implicit blocks,'// &
            ' or increase MaxBlockImpl in ModSize.f90 !')
    end if

    ! Number of implicit variables
    nImpl = nBlockImpl*nVar*nIJK

    ! Create conversion array and find the test block
    iBlockImplTest = 1
    iBlockImpl = 0
    do iBlock = 1, nBlock
       if (iTypeAdvance_B(iBlock) == ImplBlock_) then
          iBlockImpl = iBlockImpl + 1
          iBlockFromImpl_B(iBlockImpl) = iBlock
          if(iBlock == iBlockTest) iBlockImplTest = iBlockImpl
       endif
    end do

    IsImplCell_CB = .false.

    !$omp parallel do private(iBlock)
    do iBlockImpl = 1, nBlockImpl
       iBlock = iBlockFromImpl_B(iBlockImpl)
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          IsImplCell_CB(i,j,k,iBlock) = Used_GB(i,j,k,iBlock)
       enddo; enddo; enddo

       if(UseResistivePlanet)then
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             if(r_GB(i,j,k,iBlock) < 1.0) &
                  IsImplCell_CB(i,j,k,iBlock) = .false.
          enddo; enddo; enddo
       endif
    enddo

    ! The index of the test variable in the linear array
    nTest = &
         iVarTest + &
         nVar*(iTest-1+nI*(jTest-1+nJ*(kTest-1+nK*(iBlockImplTest-1))))

    call test_stop(NameSub, DoTest)
  end subroutine implicit_init
  !============================================================================
  subroutine explicit2implicit(iMin, iMax, jMin, jMax, kMin, kMax, Var_VGB)

    ! Convert data structure Var_VGB of the implicit code to the explicit code

    use ModMain
    use ModEnergy,  ONLY: pressure_to_energy_block
    use ModAdvance, ONLY: State_VGB
    use ModSize, ONLY: nG, nI, nJ, nK

    integer,intent(in):: iMin,iMax,jMin,jMax,kMin,kMax
    real,  intent(out):: &
         Var_VGB(nVar,iMin:iMax,jMin:jMax,kMin:kMax,MaxBlockImpl)

    integer :: iBlockImpl, iBlock, i, j, k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'explicit2implicit'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*)'Starting explicit2implicit: ',&
         'iMin,iMax,jMin,jMax,kMin,kMax=', iMin, iMax, jMin, jMax, kMin, kMax

    call timing_start('expl2impl')

    !$omp parallel do private( iBlock )
    do iBlockImpl = 1, nBlockImpl
       iBlock = iBlockFromImpl_B(iBlockImpl)
       Var_VGB(:,:,:,:,iBlockImpl) = &
            State_VGB(:,iMin:iMax,jMin:jMax,kMin:kMax,iBlock)

       if(UseImplicitEnergy) call pressure_to_energy_block( &
            Var_VGB(:,:,:,:,iBlockImpl), iMin, iMax, jMin, jMax, kMin, kMax, &
            iBlock)

       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          ! Max velocity at each face is calculated later in get_cmax_face(),
          ! which is calculated block-by-block. If a cell is not implicit
          ! and is not a 'ghost' cell of an implicit cell, then set the
          ! density to 1 and all the other variables to 0, so that the maximum
          ! velocities at these faces are 0.
          if(.not. any(IsImplCell_CB(max(1,i-nG):min(nI,i+nG),&
               max(1,j-nG):min(nJ,j+nG),max(1,k-nG):min(nK,k+nG),iBlock))) then
             Var_VGB(:,i,j,k,iBlockImpl) = 0
             Var_VGB(iRho_I,i,j,k,iBlockImpl) = 1.0
          endif
       enddo; enddo; enddo
    end do
    !$omp end parallel do

    call timing_stop('expl2impl')

    if(DoTest .and. nBlockImpl > 0) &
         write(*,*)NameSub, ': finished with Var_VGB=',&
         Var_VGB(:,iTest,jTest,kTest,iBlockImplTest)

    call test_stop(NameSub, DoTest)
  end subroutine explicit2implicit
  !============================================================================
  subroutine impl2expl(Var_VC, iBlock)

    ! Convert the implicit block Var_VC to block iBlock of the explicit code

    use ModSize,       ONLY: nI, nJ, nK
    use ModEnergy,     ONLY: energy_to_pressure_cell
    use ModAdvance,    ONLY: nVar, State_VGB
    use ModMultiFluid, ONLY: nFluid, iRho, iRho_I
    use ModPhysics,    ONLY: RhoMin_I

    real,    intent(in) :: Var_VC(:,:,:,:) ! dimension(nVar,nI,nJ,nK)
    integer, intent(in) :: iBlock
    integer :: i,j,k, iFluid

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'impl2expl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    call timing_start('impl2expl')

    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if(.not.IsImplCell_CB(i,j,k,iBlock)) CYCLE
       State_VGB(1:nVar,i,j,k,iBlock) = Var_VC(1:nVar,i,j,k)
    end do; end do; end do

    do iFluid = 1, nFluid
       if(RhoMin_I(iFluid) < 0) CYCLE
       iRho = iRho_I(iFluid)
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          State_VGB(iRho,i,j,k,iBlock) = max(RhoMin_I(iFluid), &
               State_VGB(iRho,i,j,k,iBlock))
       end do; end do; end do
    end do

    if(UseImplicitEnergy) &
         call energy_to_pressure_cell(iBlock, State_VGB)

    call timing_stop('impl2expl')

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine impl2expl
  !============================================================================
  subroutine implicit2explicit(Var_VCB)

    use ModAdvance, ONLY: State_VGB

    real :: Var_VCB(:,:,:,:,:)   ! dimension(nVar,nI,nJ,nK,MaxBlockImpl)
    integer :: iBlockImpl, iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'implicit2explicit'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    !$omp parallel do private( iBlock )
    do iBlockImpl = 1, nBlockImpl
       iBlock = iBlockFromImpl_B(iBlockImpl)
       call impl2expl(Var_VCB(:,:,:,:,iBlockImpl),iBlock)
    end do
    !$omp end parallel do

    if(DoTest)write(*,*) NameSub,': State_VGB=',&
         State_VGB(:,iTest,jTest,kTest,iBlockTest)

    call test_stop(NameSub, DoTest)
  end subroutine implicit2explicit
  !============================================================================
  subroutine get_residual(IsLowOrder, DoCalcTimestep, DoSubtract, Var_VCB, &
       Res_VCB)

    ! If IsLowOrder is true apply low  order scheme
    ! otherwise             apply high order scheme
    !
    ! If DoCalcTimestep is true calculate time step based on CFL condition
    !
    ! If DoSubtract is true return  Res_VCB = Var_VCB(t+DtExpl)-Var_VCB(t)
    ! otherwise return              Res_VCB = Var_VCB(t+DtExpl)

    use ModMain
    use ModAdvance, ONLY : TypeFlux, DtMax_CB
    use ModAdvanceExplicit, ONLY: advance_explicit
    use ModMessagePass, ONLY: exchange_messages
    use ModMpi

    logical, intent(in) :: IsLowOrder, DoCalcTimestep, DoSubtract
    real, intent(in)    :: Var_VCB(:,:,:,:,:)
    ! The actual Var_VCB and Res_VCB arguments may be the same array:
    ! intent(inout)
    real, intent(inout) :: Res_VCB(:,:,:,:,:)

    real    :: CflTmp
    integer :: nOrderTmp, nStageTmp, iBlockImpl, iBlock
    character(len=10) :: TypeFluxTmp

    integer :: i, j, k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_residual'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    call timing_start('get_residual')

    if(DoTest .and. nBlockImpl>0)&
         write(*,*)'get_residual DoSubtract,IsLowOrder,Var_VCB=',&
         DoSubtract, IsLowOrder, &
         Var_VCB(kTest,iVarTest,iTest,jTest,iBlockImplTest)

    nStageTmp       = nStage
    nStage          = 1
    if(IsLowOrder)then
       nOrderTmp    = nOrder
       nOrder       = nOrderImpl
       TypeFluxTmp  = TypeFlux
       TypeFlux     = TypeFluxImpl
    endif
    if(UseDtFixed)then
       !$omp parallel do private( iBlock )
       do iBlockImpl = 1, nBlockImpl
          iBlock = iBlockFromImpl_B(iBlockImpl)
          DtMax_CB(:,:,:,iBlock) = 0.0
          where(IsImplCell_CB(1:nI,1:nJ,1:nK,iBlock)) &
               DtMax_CB(1:nI,1:nJ,1:nK,iBlock) = DtExpl
       end do
       !$omp end parallel do
    else
       CflTmp = Cfl
       Cfl    = 0.5
    end if

    ! Res_VCB = Var_VCB(t+dt)
    call implicit2explicit(Var_VCB)

    call exchange_messages

    call advance_explicit(DoCalcTimestep)

    call explicit2implicit(1, nI, 1, nJ, 1, nK, Res_VCB)

    if(DoSubtract) Res_VCB(:,:,:,:,1:nBlockImpl) = &
         Res_VCB(:,:,:,:,1:nBlockImpl) - Var_VCB(:,:,:,:,1:nBlockImpl)

    if(DoTest .and. nBlockImpl > 0)write(*,*)'get_residual Res_VCB:',&
         Res_VCB(iVarTest,iTest,jTest,kTest,iBlockImplTest)

    !$omp parallel do private( iBlock )
    do iBlockImpl = 1, nBlockImpl; do k = 1, nK; do j = 1, nJ; do i = 1, nI
       iBlock = iBlockFromImpl_B(iBlockImpl)
       if(.not. IsImplCell_CB(i,j,k,iBlock)) then
          Res_VCB(:,i,j,k,iBlockImpl) = 0;
       endif
    enddo; enddo; enddo; enddo
    !$omp end parallel do

    ! Restore global variables
    nStage      = nStageTmp
    if(IsLowOrder)then
       nOrder   = nOrderTmp
       TypeFlux = TypeFluxTmp
    end if
    if(.not.UseDtFixed) Cfl = CflTmp

    call timing_stop('get_residual')

    call test_stop(NameSub, DoTest)
  end subroutine get_residual
  !============================================================================
  subroutine get_impl_source(iBlock, Var_VC, SourceImpl_VC)

    ! Get sources for block iBlock using implicit data Var_VC

    use ModMain
    use ModVarIndexes
    use ModAdvance, ONLY : Source_VC  ! To communicate to calc_source
    use ModCalcSource, ONLY: calc_source

    integer, intent(in) :: iBlock
    real, intent(in)    :: Var_VC(nVar,nI,nJ,nK)
    real, intent(out)   :: SourceImpl_VC(nVar,nI,nJ,nK)

    logical :: UseDivbSourceOrig
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_impl_source'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    call timing_start(NameSub)

    UseDivbSourceOrig = UseDivbSource
    UseDivbSource     = .false.

    call impl2expl(Var_VC, iBlock)

    call calc_source(iBlock)

    SourceImpl_VC = Source_VC(1:nVar,:,:,:)

    if(UseImplicitEnergy)then
       ! Overwrite pressure source terms with energy source term
       SourceImpl_VC(iP_I,:,:,:) = &
            Source_VC(Energy_:Energy_+nFluid-1,:,:,:)
    end if

    UseDivbSource = UseDivbSourceOrig
    call timing_stop(NameSub)

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine get_impl_source
  !============================================================================
  subroutine get_face_flux(StateCons_VC,B0_DC,nI,nJ,nK,iDim,iBlock,Flux_VC)

    ! We need the cell centered physical flux function, but to keep
    ! the implicit scheme general for all equations, we reuse
    ! subroutine get_physical_flux from ModFaceFlux.

    use ModVarIndexes, ONLY: nVar, Energy_
    use ModMain,     ONLY: x_, y_, z_
    use ModAdvance,  ONLY: nFlux
#ifndef _OPENACC
    use ModFaceFlux, ONLY: &
         set_block_values, set_cell_values, get_physical_flux, &
         iFace, jFace, kFace, DoTestCell, &
         HallJx, HallJy, HallJz, UseHallGradPe, B0x, B0y, B0z, Area
#endif
    use ModHallResist, ONLY: UseHallResist, HallJ_CD
    use ModMultiFluid, ONLY: nFluid, iP_I

    integer, intent(in):: nI, nJ, nK, iDim, iBlock
    real, intent(in)   :: StateCons_VC(:,:,:,:) ! dimension(nVar,nI,nJ,nK)
    real, intent(in)   :: B0_DC(:,:,:,:)        ! dimension(MaxDim,nI,nJ,nK)
    real, intent(out)  :: Flux_VC(:,:,:,:)      ! dimension(nVar,nI,nJ,nK)
#ifndef _OPENACC

    real :: Primitive_V(nVar), Conservative_V(nFlux), Flux_V(nFlux)

    real :: Un_I(nFluid+1), En
    integer :: i, j, k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_face_flux'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    call set_block_values(iBlock, iDim)
    ! Set iFace=i, jFace=j, kFace=k so that
    ! call set_cell_values and call get_physical_flux work
    ! This is not quite right but good enough for the preconditioner
    do k = 1, nK; kFace=k; do j = 1, nJ; jFace=j; do i = 1, nI; iFace=i

       DoTestCell = DoTest .and. &
            i==iTest .and. j==jTest .and. k==kTest

       Primitive_V = StateCons_VC( :,i, j, k)
       call conservative_to_primitive(Primitive_V)

       if(UseHallResist)then
          HallJx = HallJ_CD(i, j, k, x_)
          HallJy = HallJ_CD(i, j, k, y_)
          HallJz = HallJ_CD(i, j, k, z_)
       end if

       call set_cell_values

       ! Ignore gradient of electron pressure in the preconditioner
       UseHallGradPe = .false.

       B0x = B0_DC(x_, i, j, k)
       B0y = B0_DC(y_, i, j, k)
       B0z = B0_DC(z_, i, j, k)

       call get_physical_flux(Primitive_V, Conservative_V, Flux_V, Un_I, En)

       Flux_VC(1:nVar,i,j,k)= Flux_V(1:nVar)*Area

       if(UseImplicitEnergy)then
          ! Replace pressure flux with energy flux
          Flux_VC(iP_I,i,j,k) = Flux_V(Energy_:Energy_+nFluid-1)*Area
       end if

    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
#endif
  end subroutine get_face_flux
  !============================================================================
  subroutine get_cmax_face(Var_VF, B0_DF, nFaceI, nFaceJ, nFaceK, &
       iDim, iBlock,Cmax_F)

    use ModMain,     ONLY: x_, y_, z_
    use ModFaceFlux, ONLY: &
         set_block_values, set_cell_values, get_speed_max
#ifndef _OPENACC
    use ModFaceFlux, ONLY: B0x, B0y, B0z, CmaxDt, Area, DoTestCell,&
         iFace, jFace, kFace, UnLeft_I, UnRight_I
#endif
    use ModAdvance,  ONLY: eFluid_
    use ModMultiFluid, ONLY: nFluid

    integer, intent(in) :: nFaceI,nFaceJ,nFaceK,iDim,iBlock
    real,    intent(in) :: Var_VF(:,:,:,:) ! (nVar,nFaceI,nFaceJ,nFaceK)
    real,    intent(in) :: B0_DF(:,:,:,:)  ! (MaxDim,nFaceI,nFaceJ,nFaceK)
    real,    intent(out):: Cmax_F(:,:,:)   ! (nFaceI,nFaceJ,nFaceK)

    real :: Primitive_V(nVar), Cmax_I(nFluid)
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_cmax_face'
    !--------------------------------------------------------------------------
#ifndef _OPENACC
    call test_start(NameSub, DoTest, iBlock)

    ! The electron speed is set to zero (I can't remember why)
    UnLeft_I(eFluid_)  = 0.0
    UnRight_I(eFluid_) = 0.0

    call set_block_values(iBlock, iDim)

    do kFace = 1, nFaceK; do jFace = 1, nFaceJ; do iFace = 1, nFaceI

       DoTestCell = DoTest .and. &
            iFace==iTest .and. jFace==jTest .and. kFace==kTest

       Primitive_V = Var_VF(:,iFace, jFace, kFace)

       call conservative_to_primitive(Primitive_V)

       call set_cell_values

       ! This is inconsistent for hd with Sokolov scheme,
       ! because originally the maximum speed from Rusanov scheme is applied!
       B0x = B0_DF( x_,iFace, jFace, kFace)
       B0y = B0_DF( y_,iFace, jFace, kFace)
       B0z = B0_DF( z_,iFace, jFace, kFace)

       CmaxDt = 0.0 ! initialize to avoid floating point exception
       call get_speed_max(Primitive_V, cmax_I = Cmax_I)

       Cmax_F(iFace, jFace, kFace) = maxval(Cmax_I)*Area

    end do; end do; end do

    if(DoTest)write(*,*) NameSub,': Area, cmax=', &
         Area, Cmax_F(iTest, jTest, kTest)

    call test_stop(NameSub, DoTest, iBlock)
#endif
  end subroutine get_cmax_face
  !============================================================================
  subroutine conservative_to_primitive(State_V)

    use ModVarIndexes, ONLY: Bx_, Bz_, IsMhd, nFluid
    use ModMultiFluid, ONLY: select_fluid, &
         iRho, iRhoUx, iUx, iRhoUz, iUz, iP, &
         iRho_I, iUx_I, iUy_I, iUz_I, iRhoUx_I, iRhoUy_I, iRhoUz_I
    use ModPhysics, ONLY: GammaMinus1_I, GammaMinus1

    real, intent(inout):: State_V(nVar)
    real :: InvRho, InvRho_I(nFluid)
    integer :: iFluid
    character(len=*), parameter:: NameSub = 'conservative_to_primitive'
    !--------------------------------------------------------------------------
    if(UseImplicitEnergy)then
       do iFluid = 1, nFluid
          if(nFluid > 1) call select_fluid(iFluid)

          InvRho = 1.0/State_V(iRho)

          State_V(iP) = GammaMinus1_I(iFluid)*(State_V(iP) - &
               0.5*sum(State_V(iRhoUx:iRhoUz)**2)*InvRho)

          if(iFluid == 1 .and. IsMhd) &
               State_V(iP) = State_V(iP) - &
               0.5*GammaMinus1*sum(State_V(Bx_:Bz_)**2)

          State_V(iUx:iUz) = InvRho*State_V(iRhoUx:iRhoUz)
       end do
    else
       InvRho_I = 1.0/State_V(iRho_I)
       State_V(iUx_I) = InvRho_I*State_V(iRhoUx_I)
       State_V(iUy_I) = InvRho_I*State_V(iRhoUy_I)
       State_V(iUz_I) = InvRho_I*State_V(iRhoUz_I)
    end if

  end subroutine conservative_to_primitive
  !============================================================================
  subroutine get_dt_courant(DtOut)

    use ModMain
    use ModB0,  ONLY: B0_DGB
    use ModGeometry, ONLY: IsNoBody_B
    use ModMpi
    use ModHallResist, ONLY: UseHallResist, set_hall_factor_face
    use BATL_lib, ONLY: CellVolume_GB

    real, intent(out) :: DtOut

    real :: Cmax_C(nI,nJ,nK), B0_DC(MaxDim,nI,nJ,nK), DtLocal
    integer :: iDim, iBlockImpl, iBlock, iError

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_dt_courant'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! First calculate max(cmax/dx) for each cell and dimension
    DtLocal = 0.0
    !$omp parallel do private( iBlock, B0_DC, Cmax_C ) &
    !$omp reduction(max:DtLocal)
    do iBlockImpl = 1, nBlockImpl
       iBlock = iBlockFromImpl_B(iBlockImpl)

       if(UseB0)then
          B0_DC = B0_DGB(:,1:nI,1:nJ,1:nK,iBlock)
       else
          B0_DC = 0.0
       end if

       ! Since we use get_cmax_face to calculate the cell centered speed,
       ! the Hall factor is needed on the face.
       ! Also note that if HallCmaxFactor is zero, then the whistler speed
       ! gets ignored. This may or may not be intentional !!!
       if(UseHallResist) call set_hall_factor_face(iBlock)

       do iDim = 1, nDim
          call get_cmax_face(Impl_VGB(1:nVar,1:nI,1:nJ,1:nK,iBlockImpl), &
               B0_DC, nI, nJ, nK, iDim, iBlock, Cmax_C)

          if(.not.IsNoBody_B(iBlock))then
             where(.not.IsImplCell_CB(1:nI,1:nJ,1:nK,iBlock)) Cmax_C = 0.0
          end if

          DtLocal = max(DtLocal, &
               maxval(Cmax_C/CellVolume_GB(1:nI,1:nJ,1:nK,iBlock)))
       end do
    end do
    !$omp end parallel do

    ! Take global maximum
    call MPI_allreduce(DtLocal, DtOut, 1, MPI_REAL, MPI_MAX, iComm, iError)

    if(DoTest)write(*,*)'1/dt_local,1/dt=', DtLocal, DtOut

    ! Take inverse, and reduce so it is OK for 3D calculation
    DtOut = 0.3/DtOut

    if(DoTest)write(*,*) NameSub, ': final dt=',DtOut

    call test_stop(NameSub, DoTest)
  end subroutine get_dt_courant
  !============================================================================
end module ModPartImplicit
!==============================================================================
