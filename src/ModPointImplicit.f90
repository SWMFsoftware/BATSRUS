!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModPointImplicit

  use BATL_lib, ONLY: &
       test_start, test_stop, StringTest, iTest, jTest, kTest, iBlockTest, &
       iProc
  use ModBatsrusUtility, ONLY: stop_mpi

  ! This module implements a point implicit scheme for the implicit
  ! part of the right hand side Rimp = R - Rexp that contributes to the
  ! the implicitly treated variables Uimp, a subset of U=(Uexp, Uimp).
  ! Rimp should depend on the local cell values only: no spatial derivatives!
  !
  ! For a one stage scheme the variables are updated with the following
  ! first order scheme
  !
  ! Uexp^n+1 = Uexp^n + Dt*Rexp(U^n)
  ! Uimp^n+1 = Uimp^n + Dt*Rimp(U^n+1)
  !
  ! For the two stage scheme the following scheme is applied
  !
  ! Uexp^n+1/2 = Uexp^n + Dt/2 * Rexp(U^n)
  ! Uimp^n+1/2 = Uimp^n + Dt/2 * Rimp(U^n+1/2)
  !
  ! Uexp^n+1   = Uexp^n + Dt*Rexp(U^n+1/2)
  ! Uimp^n+1   = Uimp^n + Dt*beta*Rimp(U^n+1) + Dt*(1-beta)*Rimp(U^n)
  !
  ! where beta is in the range 0.5 and 1.0.
  ! The scheme is second order accurate in time for beta = 0.5.
  !
  ! For the general case Rimp is non-linear, and it is linearized as
  !
  ! Rimp(U^n+1/2) = Rimp(Uexp^n+1/2,Uimp^n) + dRimp/dUimp*(Uimp^n+1/2 - Uimp^n)
  ! Rimp(U^n+1)   = Rimp(Uexp^n+1,  Uimp^n) + dRimp/dUimp*(Uimp^n+1   - Uimp^n)
  !
  ! Note that the Jacobian dRimp/dUimp is evaluated at the partially advanced
  ! states (Uexp^n+1/2,Uimp^n) and (Uexp^n+1,Uimp^n) respectively.
  ! If Rimp is linear, the linearization is exact.
  !
  ! Substituting the linearization back into the one-stage and two-stage
  ! schemes yields a linear equation for the differences
  ! (Uimp^n+1/2 - Uimp^n) and (Uimp^n+1 - Uimp^n), respectively.
  ! Since Rimp depends on the local cell values only, the linear equations
  ! can be solved point-wise for every cell.
  !
  ! The Jacobian can be given analytically by the subroutine passed to
  ! update_point_implicit, or it can be obtained by taking numerical
  ! derivatives of Rimp:
  !
  ! dRimp/dU^w = ((Rimp(Uexp^n+1,Uimp^n+eps^w) - Rimp(Uexp^n+1,Uimp^n))/eps^w
  !
  ! where eps^w is a small perturbation in the w-th component of Uimp.
  use ModSize, ONLY: MaxBlock
  use ModMultiFluid, ONLY: UseMultiIon
  use ModAdvance, ONLY: UseEfield
  use omp_lib
  implicit none

  save

  private ! except

  public:: init_mod_point_impl       ! initialize module
  public:: clean_mod_point_impl      ! clean module
  public:: update_point_implicit     ! do update with point implicit scheme
  public:: read_point_implicit_param ! read parameters
  public:: init_point_implicit_num   ! initialize solver
  public:: linear_equation_solver    ! solve a linear problem

  ! Default is true for multi-ion and ion-electron equations
  logical, public:: UsePointImplicit = UseMultiIon .or. UseEfield
  ! Allows the user to specify the blocks to use the point implicit scheme
  ! individually
  logical, public, allocatable:: UseUserPointImplicit_B(:)

  ! balance point implicit blocks once or multiple times?
  logical, public:: DoBalancePointImplicit = .false.
  logical, public, protected:: IsDynamicPointImplicit = .false.
  ! Indexes of point implicit variables
  integer, public, allocatable :: iVarPointImpl_I(:)
  logical, public :: IsPointImplMatrixSet=.false.! Is dS/dU matrix analytic?
  logical, public :: IsPointImplPerturbed=.false.! Is the state perturbed?
  !$omp threadprivate( IsPointImplMatrixSet )
  !$omp threadprivate( IsPointImplPerturbed )

  real, allocatable :: Matrix_II(:,:), Rhs_I(:)
  !$omp threadprivate( Matrix_II, Rhs_I )

  real, public, allocatable :: &
       DsDu_VVC(:,:,:,:,:), &     ! dS/dU derivative matrix
       EpsPointImpl_V(:)          ! absolute perturbation per variable
  real, public    :: EpsPointImpl ! relative perturbation
  !$omp threadprivate( DsDu_VVC )

  ! Local variables
  ! Number of point implicit variables
  integer :: nVarPointImpl = 0

  ! Number and indexes of variables with numerical derivatives
  integer :: nVarPointImplNum = 0
  integer, allocatable :: iVarPointImplNum_I(:)

  ! Coeff. of implicit part: beta=0.5 second order, beta=1.0 first order
  real:: BetaPointImpl = 1.0

  ! Use asymmetric derivative in numerical Jacobian calculation
  logical:: IsAsymmetric = .true.

  ! Normalize variables cell-by-cell or per block
  logical:: DoNormalizeCell = .false.

contains
  !============================================================================
  subroutine init_mod_point_impl(init_point_implicit)

    use ModKind, ONLY: nByteReal
    use ModMain, ONLY: nI, nJ, nK, nVar

    optional :: init_point_implicit

    interface
       subroutine init_point_implicit
       end subroutine init_point_implicit
    end interface

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_mod_point_impl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(allocated(UseUserPointImplicit_B)) RETURN

    allocate(UseUserPointImplicit_B(MaxBlock))
    ! Default is true for multi-ion and electron-ion equations
    UseUserPointImplicit_B = UsePointImplicit

    ! Set default perturbation parameters
    !
    ! Perturbation_V = abs(State_V)*EpsPointImpl + EpsPointImpl_V

    allocate(EpsPointImpl_V(nVar))
    if(nByteReal == 8)then
       ! Precision of 8-byte arithmetic is roughly P = 1e-12
       if(IsAsymmetric)then
          ! Optimal value is the square root of P for 1-sided derivative
          EpsPointImpl   = 1.e-6
       else
          ! Optimal value is the 2/3 power of P for 1-sided derivative
          EpsPointImpl   = 1.e-9
       end if
    else
       ! Precision of 4-byte arithmetic is roughly P = 1e-6
       if(IsAsymmetric)then
          ! Optimal value is the square root of P for 1-sided derivative
          EpsPointImpl   = 1.e-3
       else
          ! Optimal value is the 2/3 power of P for 1-sided derivative
          EpsPointImpl   = 1.e-4
       end if
    end if
    ! Set the smallest value for the perturbation. This divides the
    ! difference of the source terms Spert - Sorig, so it cannot be
    ! too small otherwise the error in the source term becomes large.
    EpsPointImpl_V = EpsPointImpl

    ! This call should allocate and set the iVarPointImpl_I index array.
    ! If IsPointImplMatrixSet=T is set then the dS/dU matrix is analytic.
    ! If IsPointImplMatrixSet is not true, then nVarPointImplNum and
    ! iVarPointImplNum_I index array may be set with the number and indexes
    ! of point implicit variables that require numerical derivatives
    ! for setting dS/dU. This is a subset of the iVarPointImpl_I variables.
    ! Finally, init_point_implicit may also modify the EpsPointImpl and
    ! EpsPointImpl_V parameters.

    nVarPointImplNum = 0
    call init_point_implicit

    if(.not.allocated(iVarPointImpl_I)) call stop_mpi( NameSub // &
       ': init_point_implicit did not set iVarPointImpl_I')

    ! If IsPointImplMatrixSet is false and the iVarPointImplNum_I array is
    ! not set, then all variables require numerical derivatives,
    ! so we simply copy iVarPointImpl_I into iVarPointImplNum_I
    if(.not.IsPointImplMatrixSet .and. nVarPointImplNum == 0) &
       call init_point_implicit_num

    nVarPointImpl = size(iVarPointImpl_I)

    !$omp parallel
    allocate( &
       DsDu_VVC(nVar,nVar,nI,nJ,nK), &
       Matrix_II(nVarPointImpl,nVarPointImpl), &
       Rhs_I(nVarPointImpl))
    !$omp end parallel

    if(iProc==0 .and. index(StringTest, NameSub)>0)then
       write(*,*)NameSub,' allocated arrays'
       write(*,*)NameSub,': nVarPointImpl  =', nVarPointImpl
       write(*,*)NameSub,': iVarPointImpl_I=', iVarPointImpl_I
       write(*,*)NameSub,': IsPointImplMatrixSet=', IsPointImplMatrixSet
       if(.not.IsPointImplMatrixSet)then
          write(*,*)NameSub,': nVarPointImplNum  =', nVarPointImplNum
          write(*,*)NameSub,': iVarPointImplNum_I=', iVarPointImplNum_I
          write(*,*)NameSub,': EpsPointImpl  =', EpsPointImpl
          write(*,*)NameSub,': EpsPointImpl_V=', EpsPointImpl_V
       end if
    end if

    call test_stop(NameSub, DoTest)
  end subroutine init_mod_point_impl
  !============================================================================
  subroutine init_point_implicit_num

    ! Set iVarPointImplicitNum_I, the list of variables to be perturbed
    ! by copying iVarPointImplicit_I

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_point_implicit_num'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(allocated(iVarPointImplNum_I)) deallocate(iVarPointImplNum_I)
    nVarPointImplNum = size(iVarPointImpl_I)
    allocate(iVarPointImplNum_I(nVarPointImplNum))
    iVarPointImplNum_I = iVarPointImpl_I

    call test_stop(NameSub, DoTest)
  end subroutine init_point_implicit_num
  !============================================================================
  subroutine clean_mod_point_impl

    !--------------------------------------------------------------------------
    if(.not.allocated(UseUserPointImplicit_B)) RETURN
    deallocate(UseUserPointImplicit_B)
    if(allocated(iVarPointImpl_I)) deallocate(iVarPointImpl_I)
    if(allocated(iVarPointImplNum_I)) deallocate(iVarPointImplNum_I)
    if(allocated(EpsPointImpl_V)) deallocate(EpsPointImpl_V)
    !$omp parallel
    if(allocated(DsDu_VVC)) deallocate(DsDu_VVC)
    if(allocated(Rhs_I)) deallocate(Rhs_I)
    if(allocated(Matrix_II)) deallocate(Matrix_II)
    !$omp end parallel

  end subroutine clean_mod_point_impl
  !============================================================================
  subroutine read_point_implicit_param
    use ModReadParam, ONLY: read_var

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_point_implicit_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    call read_var('UsePointImplicit', UsePointImplicit)

    if(UsePointImplicit) then
       call read_var('BetaPointImplicit', BetaPointImpl)
       call read_var('IsAsymmetric',      IsAsymmetric)
       call read_var('DoNormalizeCell',   DoNormalizeCell)
    end if

    call test_stop(NameSub, DoTest)
  end subroutine read_point_implicit_param
  !============================================================================
  subroutine update_point_implicit(iBlock, calc_point_impl_source)

    use ModMain, ONLY: &
         nI, nJ, nK, nIJK, Cfl, iStage, nStage, IsTimeAccurate
    use ModAdvance, ONLY: nVar, State_VGB, StateOld_VGB, Source_VC, DtMax_CB, &
         DoReplaceDensity, UseMultiSpecies
    use ModMultiFluid, ONLY: iRho_I, nFluid
    use ModGeometry, ONLY: IsNoBody_B
    use BATL_lib, ONLY: Used_GB
    use ModVarIndexes, ONLY: SpeciesFirst_, SpeciesLast_, &
         Rho_, DefaultState_V, NameVar_V
    use ModPhysics, ONLY: RhoMin_I

    integer, intent(in) :: iBlock
    interface
       subroutine calc_point_impl_source(iBlock)
         integer, intent(in) :: iBlock
       end subroutine calc_point_impl_source
    end interface

    integer :: i, j, k, iVar, jVar, iIVar, iJVar, iFluid
    real :: DtCell, BetaStage, Norm_C(nI,nJ,nK), Epsilon_C(nI,nJ,nK)
    real :: StateExpl_VC(nVar,nI,nJ,nK)
    real :: Source0_VC(nVar,nI,nJ,nK), Source1_VC(nVar,nI,nJ,nK)
    real :: State0_C(nI,nJ,nK)

    logical :: DoTestCell

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'update_point_implicit'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    call timing_start(NameSub)

    ! The beta parameter is always one in the first stage
    if(iStage == 1 .or. .not. IsTimeAccurate)then
       BetaStage = 1.0
    else
       BetaStage = BetaPointImpl
    end if

    ! call timing_start('pointimplinit')

    ! Store explicit update
    StateExpl_VC = State_VGB(:,1:nI,1:nJ,1:nK,iBlock)

    ! Put back old values into the implicit variables
    ! so the update is relative to the time level n
    ! (this is a steady state preserving scheme).
    do k=1,nK; do j=1,nJ; do i=1,nI; do iIVar=1,nVarPointImpl
       iVar = iVarPointImpl_I(iIvar)
       State_VGB(iVar,i,j,k,iBlock) = StateOld_VGB(iVar,i,j,k,iBlock)
    end do; end do; end do; end do

    ! Calculate unperturbed source for right hand side
    ! and possibly also set analytic Jacobean matrix elements.
    ! Multi-ion may set its elements while the user uses numerical Jacobean.
    Source_VC = 0.0
    DsDu_VVC  = 0.0

    call calc_point_impl_source(iBlock)

    ! Calculate (part of) Jacobian numerically if necessary
    if(.not.IsPointImplMatrixSet)then

       ! Let the source subroutine know that the state is perturbed
       IsPointImplPerturbed = .true.

       ! Save unperturbed source
       Source0_VC = Source_VC(1:nVar,:,:,:)

       ! Perturb all point implicit variables one by one
       do iIVar = 1,nVarPointImplNum
          iVar = iVarPointImplNum_I(iIvar)

          ! Store unperturbed state
          State0_C = State_VGB(iVar,1:nI,1:nJ,1:nK,iBlock)

          ! Get perturbation based on first norm of state in the block
          if(DoNormalizeCell)then
             Norm_C = abs(State0_C)
          elseif(IsNoBody_B(iBlock))then
             Norm_C = sum(abs(State0_C))/nIJK
          else
             Norm_C = sum(abs(State0_C),MASK=Used_GB(1:nI,1:nJ,1:nK,iBlock))&
                  /max(count(Used_GB(1:nI,1:nJ,1:nK,iBlock)),1)
          end if

          Epsilon_C = EpsPointImpl*Norm_C + EpsPointImpl_V(iVar)

          if(DefaultState_V(iVar) > 0.5 .and. .not. IsAsymmetric)then
             if(DoNormalizeCell)then
                Epsilon_C = min(Epsilon_C, max(1e-30, 0.5*State0_C))
             else
                Epsilon_C = min(Epsilon_C, max(1e-30, 0.5*minval(State0_C)))
             end if
          end if
          ! Perturb the state
          State_VGB(iVar,1:nI,1:nJ,1:nK,iBlock) = State0_C + Epsilon_C

          ! Calculate perturbed source
          Source_VC = 0.0
          call calc_point_impl_source(iBlock)

          if(IsAsymmetric)then
             ! Calculate dS/dU matrix elements
             do iJVar=1,nVarPointImplNum; jVar=iVarPointImplNum_I(iJVar)
                DsDu_VVC(jVar,iVar,:,:,:) = DsDu_VVC(jVar,iVar,:,:,:) + &
                     (Source_VC(jVar,:,:,:) - Source0_VC(jVar,:,:,:))/Epsilon_C
             end do
          else
             ! Store perturbed source corresponding to +Epsilon_C perturbation
             Source1_VC = Source_VC(1:nVar,:,:,:)

             ! Perturb the state in opposite direction
             State_VGB(iVar,1:nI,1:nJ,1:nK,iBlock) = State0_C - Epsilon_C

             ! Calculate perturbed source
             Source_VC = 0.0
             call calc_point_impl_source(iBlock)

             ! Calculate dS/dU matrix elements with symmetric differencing
             do iJVar=1,nVarPointImplNum; jVar=iVarPointImplNum_I(iJVar)
                DsDu_VVC(jVar,iVar,:,:,:) = DsDu_VVC(jVar,iVar,:,:,:) + &
                     0.5*(Source1_VC(jVar,:,:,:) - Source_VC(jVar,:,:,:)) &
                     /Epsilon_C
             end do
          end if

          ! Restore unperturbed state
          State_VGB(iVar,1:nI,1:nJ,1:nK,iBlock) = State0_C
       end do

       ! Restore unperturbed source
       Source_VC(1:nVar,:,:,:) = Source0_VC

       IsPointImplPerturbed = .false.

    end if

    if(DoTest)then
       do iIVar=1,nVarPointImpl
          iVar = iVarPointImpl_I(iIVar)
          write(*,'(a,a,i5,a,100es15.6)')NameSub,': DsDu(',iVar,',:)=',  &
               (DsDu_VVC(iVar,iVarPointImpl_I(iJVar),iTest,jTest,kTest), &
               iJVar = 1, nVarPointImpl)
       end do
    end if

    ! Do the implicit update
    do k=1,nK; do j=1,nJ; do i=1,nI

       DoTestCell = DoTest .and. i==iTest .and. j==jTest .and. k==kTest

       ! Do not update body cells
       if(.not.Used_GB(i,j,k,iBlock)) CYCLE

       ! call timing_start('pointimplmatrix')
       DtCell = Cfl*DtMax_CB(i,j,k,iBlock)*iStage/real(nStage)

       ! The right hand side is Uexpl - Uold + Sold
       do iIVar = 1, nVarPointImpl
          iVar = iVarPointImpl_I(iIVar)
          Rhs_I(iIVar) = StateExpl_VC(iVar,i,j,k) &
               - StateOld_VGB(iVar,i,j,k,iBlock) &
               + DtCell * Source_VC(iVar,i,j,k)
       end do

       ! The matrix to be solved for is A = (I - beta*Dt*dS/dU)
       do iIVar = 1, nVarPointImpl
          iVar = iVarPointImpl_I(iIVar)
          do iJVar = 1, nVarPointImpl
             jVar = iVarPointImpl_I(iJVar)
             Matrix_II(iIVar,iJVar) = - BetaStage*DtCell* &
                  DsDu_VVC(iVar,jVar,i,j,k)
          end do
          ! Add unit matrix
          Matrix_II(iIVar,iIVar) = Matrix_II(iIVar,iIVar) + 1.0

       end do
       ! call timing_stop('pointimplmatrix')

       if (DoTestCell) then
          write(*,*) NameSub,' DtCell  =', DtCell
          write(*,*) NameSub,&
               ' StateExpl_VC, StateOld_VGB, Source_VC, initial Rhs_I      ='
          do iIVar = 1, nVarPointImpl
             iVar = iVarPointImpl_I(iIVar)
             write(*,'(a,100es15.6)') NameVar_V(iVar),                    &
             StateExpl_VC(iVar,iTest,jTest,kTest),                        &
                  StateOld_VGB(iVar,iTest,jTest,kTest,iBlockTest),           &
                  Source_VC(iVar,iTest,jTest,kTest),                      &
                  Rhs_I(iIvar)
          end do
          write(*,*) NameSub,' initial Matrix_II  ='
          do iIVar = 1, nVarPointImpl
             write(*,'(100es15.6)') Matrix_II(:,iIvar)
          end do
       end if

       ! Solve the A.dU = RHS equation
       ! call timing_start('pointimplsolve')
       call linear_equation_solver(nVarPointImpl, Matrix_II, Rhs_I)
       ! call timing_stop('pointimplsolve')

       if (DoTestCell) then
          write(*,*) NameSub,': Rhs_I  ='
          do iIVar = 1, nVarPointImpl
             iVar = iVarPointImpl_I(iIVar)
             write(*,'(a, 100es15.6)') NameVar_V(iVar),Rhs_I(iIvar)
          end do
       end if

       ! call timing_start('pointimplupdate')

       ! Update: U^n+1 = U^n + dU
       do iIVar = 1, nVarPointImpl
          iVar = iVarPointImpl_I(iIVar)
          State_VGB(iVar,i,j,k,iBlock) =&
               StateOld_VGB(iVar,i,j,k,iBlock) + Rhs_I(iIVar)
       end do

       ! Set minimum density.
       do iFluid = 1, nFluid
          if(RhoMin_I(iFluid) < 0) CYCLE
          iVar = iRho_I(iFluid)
          State_VGB(iVar,i,j,k,iBlock) = max(RhoMin_I(iFluid), &
               State_VGB(iVar,i,j,k,iBlock))
       end do

       if(UseMultispecies)then
          ! Fix negative species densities
          State_VGB(SpeciesFirst_:SpeciesLast_,i,j,k,iBlock) = &
               max(0.0, State_VGB(SpeciesFirst_:SpeciesLast_,i,j,k,iBlock))

          ! Add up species densities to total density
          if(DoReplaceDensity)State_VGB(Rho_,i,j,k,iBlock) = &
               sum(State_VGB(SpeciesFirst_:SpeciesLast_,i,j,k,iBlock))
       end if

       ! call timing_stop('pointimplupdate')

    end do; end do; end do

    if(DoTest)then
       write(*,*) NameSub, ':'
       write(*,*) &
            'NameVar, StateOld,     StateExp,      StateNew='
       do iIVar = 1, nVarPointImpl
          iVar = iVarPointImpl_I(iIVar)
          write(*,'(a,3es15.6, f9.3)') NameVar_V(iVar),           &
               StateOld_VGB(iVar,iTest,jTest,kTest,iBlock), &
               StateExpl_VC(iVar,iTest,jTest,kTest),        &
               State_VGB(iVar,iTest,jTest,kTest,iBlock)
       end do
    end if

    call timing_stop(NameSub)

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine update_point_implicit
  !============================================================================
  subroutine linear_equation_solver(nVar, Matrix_VV, Rhs_V, DoLuIn)

    integer, intent(in) :: nVar
    real, intent(inout) :: Matrix_VV(nVar, nVar)
    real, intent(inout) :: Rhs_V(nVar)
    logical, intent(in), optional:: DoLuIn

    ! This routine solves the system of Nvar linear equations:
    !
    !               Matrix_VV*X = Rhs_V.
    !
    ! The result is returned in Rhs_V, the matrix is overwritten
    ! with the LU decomposition.
    !
    ! 1. The routine performs a lower-upper (LU) decomposition of the
    ! square matrix Matrix_VV of rank Nvar unless DoLuIn is present and false.
    ! Crout's method with partial implicit pivoting is used to perform
    ! the decompostion.
    !
    ! 2. Apply forward and backward substitution to obtain the solution
    ! vector X returned into Rhs_V.

    logical:: DoLu
    integer, parameter :: MaxVar = 100
    real, parameter :: cTiny=1.0E-20

    integer :: i_I(MaxVar) = 0            ! pivoting, saved from LU
    integer :: iL, iI, iLMax, jL, kL, lL
    real    :: Scaling_I(MaxVar), LhsMax, LhsTemp, TotalSum

    character(len=*), parameter:: NameSub = 'linear_equation_solver'
    !--------------------------------------------------------------------------
    if(nVar > MaxVar) call stop_mpi(&
         'ERROR in ModPointImplicit linear solver: MaxVar is too small')

    DoLu = .true.
    if(present(DoLuIn)) DoLU = DoLuIn

    if(DoLu)then
       ! Loop through each row to get implicit Scaling_I information.
       do iL = 1, nVar
          LhsMax = 0.0
          do jL = 1, nVar
             if (abs(Matrix_VV(iL,jL)) > LhsMax) LhsMax = abs(Matrix_VV(iL,jL))
          end do
          Scaling_I(iL) = 1.0/LhsMax
       end do

       ! Peform the LU decompostion using Crout's method.
       do jL = 1, nVar
          do iL = 1, jL-1
             TotalSum = Matrix_VV(iL,jL)
             do kL = 1, iL-1
                TotalSum = TotalSum-Matrix_VV(iL,kL)*Matrix_VV(kL,jL)
             end do
             Matrix_VV(iL,jL) = TotalSum
          end do
          LhsMax = 0.0
          do iL = jL, nVar
             TotalSum = Matrix_VV(iL,jL)
             do kL = 1, jL-1
                TotalSum = TotalSum - Matrix_VV(iL,kL)*Matrix_VV(kL,jL)
             end do
             Matrix_VV(iL,jL) = TotalSum
             LhsTemp = Scaling_I(iL)*abs(TotalSum)
             if (LhsTemp >=  LhsMax) then
                iLMax = iL
                LhsMax = LhsTemp
             end if
          end do
          if (jL /=  iLMax) then
             do kL = 1, nVar
                LhsTemp = Matrix_VV(iLMax,kL)
                Matrix_VV(iLMax,kL) = Matrix_VV(jL,kL)
                Matrix_VV(jL,kL) = LhsTemp
             end do
             Scaling_I(iLMax) = Scaling_I(jL)
          end if
          i_I(jL) = iLMax
          if (abs(Matrix_VV(jL,jL)) == 0.0) Matrix_VV(jL,jL) = cTiny
          if (jL /=  nVar) then
             LhsTemp = 1.0/Matrix_VV(jL,jL)
             do iL = jL+1, nVar
                Matrix_VV(iL,jL) = Matrix_VV(iL,jL)*LhsTemp
             end do
          end if
       end do

    end if ! DoLu

    ! Peform forward and back substitution to obtain the solution vector.
    iI = 0
    do iL = 1, nVar
       lL = i_I(iL)
       TotalSum = Rhs_V(lL)
       Rhs_V(lL) = Rhs_V(iL)
       if (iI /=  0) then
          do jL = iI, iL-1
             TotalSum = TotalSum - Matrix_VV(iL,jL)*Rhs_V(jL)
          end do
       else if (TotalSum /=  0.0) then
          iI = iL
       end if
       Rhs_V(iL) = TotalSum
    end do
    do iL = nVar, 1, -1
       TotalSum = Rhs_V(iL)
       do jL = iL+1, nVar
          TotalSum = TotalSum - Matrix_VV(iL,jL)*Rhs_V(jL)
       end do
       Rhs_V(iL) = TotalSum/Matrix_VV(iL,iL)
    end do

  end subroutine linear_equation_solver
  !============================================================================
end module ModPointImplicit
!==============================================================================
