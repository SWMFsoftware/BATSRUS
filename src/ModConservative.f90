!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModConservative

  ! Determine if a cell should be updated with the conservative
  ! energy equation or the non-conservative pressure equation

  use BATL_lib, ONLY: &
       iProc, iTest, jTest, kTest, iBlockTest, iProcTest, &
       test_start, test_stop
  use ModBatsrusUtility, ONLY: stop_mpi

  implicit none

  SAVE

  private ! except

  public:: clean_mod_conservative ! set fully conservative scheme
  public:: set_non_conservative   ! set fully non-conservative scheme
  public:: select_conservative    ! select cells to use conservative update
  public:: read_conservative_param
  public:: is_conserv             ! return true for conservative cell

  ! Conservative/Non-conservative parameters
  logical, public :: UseNonConservative = .false.
  !$acc declare create(UseNonConservative)

  ! True if any physics based criterion is used
  logical, public :: IsDynamicConservCrit = .false.

  ! True if only geometric based criteria are used
  logical, public :: IsStaticConservCrit = .false.

  ! Number and type of criteria
  integer, public :: nConservCrit = 0
  !$acc declare create(nConservCrit)

  ! Cells selected to be updated with conservative equations
  logical, allocatable, public :: IsConserv_CB(:,:,:,:)
  !$acc declare create(IsConserv_CB)

  ! Local variables
  character(len=10), allocatable :: TypeConservCrit_I(:)

  ! Geometrical parameters
  real    :: rConserv = -1.0, xParabolaConserv, yParabolaConserv

  ! Physics based parameters (to locate shocks)
  real    :: pCoeffConserv, GradPCoeffConserv

contains
  !============================================================================
  subroutine clean_mod_conservative

    ! Clean module and reset to fully conservative scheme (default)
    !--------------------------------------------------------------------------
    UseNonConservative   = .false.
    IsDynamicConservCrit = .false.
    IsStaticConservCrit  = .false.
    nConservCrit         = 0

    if(allocated(TypeConservCrit_I)) deallocate(TypeConservCrit_I)
    if(allocated(IsConserv_CB))      deallocate(IsConserv_CB)

  end subroutine clean_mod_conservative
  !============================================================================
  subroutine set_non_conservative

    ! Set fully non-conservative scheme
    !--------------------------------------------------------------------------
    call clean_mod_conservative
    UseNonConservative = .true.

  end subroutine set_non_conservative
  !============================================================================
  subroutine read_conservative_param(NameCommand)

    use ModReadParam, ONLY: read_var
    use ModMain, ONLY: UseStrict

    character(len=*), intent(in):: NameCommand

    integer:: i

    character(len=*), parameter:: NameSub = 'read_conservative_param'
    !--------------------------------------------------------------------------
    select case(NameCommand)
    case("#NONCONSERVATIVE")
       call read_var('UseNonConservative',UseNonConservative)
       if(.not.UseNonConservative)then
          nConservCrit = 0
          if(allocated(TypeConservCrit_I)) deallocate(TypeConservCrit_I)
       end if

    case("#CONSERVATIVECRITERIA")
       IsStaticConservCrit  = .false.
       IsDynamicConservCrit = .false.
       call read_var('nConservCrit', nConservCrit)
       if(nConservCrit > 0) then
          UseNonConservative = .true.
          if(allocated(TypeConservCrit_I)) deallocate(TypeConservCrit_I)
          allocate( TypeConservCrit_I(nConservCrit) )
          do i = 1, nConservCrit
             call read_var('TypeConservCrit',TypeConservCrit_I(i),&
                  IsLowerCase=.true.)
             select case(TypeConservCrit_I(i))
             case('r','radius')
                IsStaticConservCrit  = .true.
                !    non-conservative scheme is used for r < rConserv
                TypeConservCrit_I(i) = 'r'
                call read_var('rConserv',rConserv)
             case('parabola','paraboloid')
                IsStaticConservCrit  = .true.
                !    non-conservative scheme is used for
                !    x < xParabolaConserv - (y**2+z**2)/yParabolaConserv
                TypeConservCrit_I(i) = 'parabola'
                call read_var('xParabolaConserv',xParabolaConserv)
                call read_var('yParabolaConserv',yParabolaConserv)
                ! Physics based criteria
             case('p')
                IsDynamicConservCrit = .true.
                ! Balsara/Ryu switch 1
                call read_var('pCoeffConserv',pCoeffConserv)
             case('gradp','jumpp')
                IsDynamicConservCrit = .true.
                ! Balsara/Ryu switch 2
                call read_var('GradPCoeffConserv',GradPCoeffConserv)
             case default
                if(UseStrict)then
                   call stop_mpi(NameSub//&
                        ' ERROR: unknown TypeConservCrit=' &
                        //TypeConservCrit_I(i))
                else
                   if(iProc==0)write(*,'(a)') NameSub // &
                        ' WARNING: ignoring unknown TypeConservCrit=',&
                        trim(TypeConservCrit_I(i))//' !!!'
                end if
             end select
             ! Check if the same criterion has been used before
             if(any(TypeConservCrit_I(1:i-1)==TypeConservCrit_I(i)))then
                if(iProc==0)write(*,'(a)')NameSub // &
                     ' WARNING: multiple use of criterion ',&
                     trim(TypeConservCrit_I(i))
             end if
          end do
          ! If there is any dynamic criteria then it needs evaluation
          ! every time step
          if(IsDynamicConservCrit) IsStaticConservCrit = .false.
       end if
    case default
       call stop_mpi(NameSub//': invalid NameCommand='//NameCommand)
    end select

  end subroutine read_conservative_param
  !============================================================================
  logical function is_conserv(i, j, k, iBlock, iFluid)

    use ModMultiFluid, ONLY: nIonFluid, UseNeutralFluid, DoConserveNeutrals

    ! Return true if cell is conservative for fluid iFluid (default is 1)

    integer, intent(in):: i, j, k, iBlock
    integer, intent(in), optional:: iFluid
    !--------------------------------------------------------------------------
    ! Check for neutral fluid first
    if(UseNeutralFluid .and. present(iFluid))then
       if(iFluid > nIonFluid)then
          is_conserv = DoConserveNeutrals
          RETURN
       end if
    end if
    if(nConservCrit < 1)then
       ! pure conservative or non-conservative
       is_conserv = .not.UseNonConservative
    else
       ! mixed conservative / non-conservative
       is_conserv = IsConserv_CB(i,j,k,iBlock)
    end if

  end function is_conserv
  !============================================================================
  subroutine select_conservative

    ! Set the global variable IsConserv_CB true for conservative grid cells

    use ModMain, ONLY: UseB0
    use ModVarIndexes, ONLY: Rho_, p_, pe_, RhoUx_, RhoUz_, Bx_, Bz_
    use ModAdvance, ONLY: State_VGB, UseElectronPressure
    use ModB0, ONLY: B0_DGB
    use ModGeometry, ONLY: r_GB
    use ModPhysics, ONLY: InvGammaMinus1
    use BATL_lib, ONLY: &
         MinI, MaxI, MinJ, MaxJ, MinK, MaxK, MaxBlock, &
         nI, nJ, nK, nBlock, Unused_B, Xyz_DGB, x_, y_, z_

    integer :: iBlock, iCrit, i, j, k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'select_conservative'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    call timing_start('nonconservative')

    if(DoTest)write(*,*) NameSub,': starting with ',&
         'UseNonConservative, nConservCrit=',UseNonConservative, nConservCrit

    if(.not.allocated(IsConserv_CB))then
       allocate(IsConserv_CB(nI,nJ,nK,MaxBlock))
       if(DoTest)write(*,*) NameSub,': allocated IsConserv_CB'
    end if

    if(nConservCrit < 1)then
       ! There are no criteria so use fully non-conservative
       if(DoTest)write(*,*) NameSub,': set_non_conservative'
       call set_non_conservative
       RETURN
    endif

    if(IsDynamicConservCrit)then

       if(DoTest)write(*,*) NameSub, ': Apply physics based criteria'

       ! These all have to be true to use non-conservative,
       ! so any of the criteria can switch to conservative
       IsConserv_CB(:,:,:,1:nBlock) = .false.

       do iBlock = 1, nBlock
          if( Unused_B(iBlock) ) CYCLE

          if(UseElectronPressure)then
             do k = MinK,MaxK; do j = MinJ,MaxJ; do i = MinI,MaxI
                State_VGB(p_,i,j,k,iBlock) = State_VGB(p_,i,j,k,iBlock) &
                     + State_VGB(Pe_,i,j,k,iBlock)
             end do; end do; end do
          end if

          do iCrit = 1, nConservCrit
             select case(TypeConservCrit_I(iCrit))
             case('p')
                ! Check if pressure is > pCoeffConserv*energy_density
                if(UseB0)then
                   do k=1,nK; do j=1,nJ; do i=1,nI
                      IsConserv_CB(i,j,k,iBlock) = &
                           IsConserv_CB(i,j,k,iBlock) .or. &
                           State_VGB(P_,i,j,k,iBlock) > pCoeffConserv *   &
                           ( InvGammaMinus1*State_VGB(P_,i,j,k,iBlock)    &
                           + 0.5*sum(State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)**2)&
                           / State_VGB(Rho_,i,j,k,iBlock)                 &
                           + 0.5*sum((State_VGB(Bx_:Bz_,i,j,k,iBlock)     &
                           +          B0_DGB(:,i,j,k,iBlock))**2))
                   end do;end do;end do
                else
                   do k=1,nK; do j=1,nJ; do i=1,nI
                      IsConserv_CB(i,j,k,iBlock) = &
                           IsConserv_CB(i,j,k,iBlock) .or. &
                           State_VGB(P_,i,j,k,iBlock) > pCoeffConserv *   &
                           ( InvGammaMinus1*State_VGB(P_,i,j,k,iBlock)    &
                           + 0.5*sum(State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)**2)&
                           / State_VGB(Rho_,i,j,k,iBlock)                 &
                           + 0.5*sum(State_VGB(Bx_:Bz_,i,j,k,iBlock)**2))
                   end do;end do;end do
                end if
             case('gradp')
                ! Switch to conservative if gradient of pressure is large
                do k=1,nK; do j=1,nJ; do i=1,nI
                   IsConserv_CB(i,j,k,iBlock) = IsConserv_CB(i,j,k,iBlock) &
                        .or. &
                        (abs(State_VGB(P_,i+1,j,k,iBlock) &
                        -    State_VGB(P_,i-1,j,k,iBlock))  &
                        +abs(State_VGB(P_,i,j+1,k,iBlock) &
                        -    State_VGB(P_,i,j-1,k,iBlock))  &
                        +abs(State_VGB(P_,i,j,k+1,iBlock) &
                        -    State_VGB(P_,i,j,k-1,iBlock))) &
                        > GradPCoeffConserv * min(    &
                        State_VGB(P_,i,j,k,iBlock),   &
                        State_VGB(P_,i+1,j,k,iBlock), &
                        State_VGB(P_,i-1,j,k,iBlock), &
                        State_VGB(P_,i,j+1,k,iBlock), &
                        State_VGB(P_,i,j-1,k,iBlock), &
                        State_VGB(P_,i,j,k+1,iBlock), &
                        State_VGB(P_,i,j,k-1,iBlock))
                end do; end do; end do
             case('jumpp')
                ! Switch to conservative if pressure jump is large
                do k=1,nK; do j=1,nJ; do i=1,nI
                   IsConserv_CB(i,j,k,iBlock) = IsConserv_CB(i,j,k,iBlock) &
                        .or. &
                        maxval(State_VGB(P_,i-2:i+2,j,k,iBlock)) &
                        > GradPCoeffConserv* &
                        minval(State_VGB(P_,i-2:i+2,j,k,iBlock)) .or. &
                        nJ > 1 .and. &
                        maxval(State_VGB(P_,i,j-2:j+2,k,iBlock)) &
                        > GradPCoeffConserv* &
                        minval(State_VGB(P_,i:i,j-2:j+2,k,iBlock)) .or. &
                        nK > 1 .and. &
                        maxval(State_VGB(P_,i,j,k-2:k+2,iBlock)) &
                        > GradPCoeffConserv* &
                        minval(State_VGB(P_,i:i,j,k-2:k+2,iBlock))
                end do; end do; end do
             case default
                CYCLE
             end select

             if(DoTest .and. iBlock==iBlockTest)&
                  write(*,*) NameSub, ': TypeCrit, IsConserv=',&
                  TypeConservCrit_I(iCrit), &
                  IsConserv_CB(iTest,jTest,kTest,iBlock)

          end do ! iCrit

          if(UseElectronPressure)then
             do k = MinK,MaxK; do j = MinJ,MaxJ; do i = MinI,MaxI
                State_VGB(p_,i,j,k,iBlock) = State_VGB(p_,i,j,k,iBlock) &
                     - State_VGB(Pe_,i,j,k,iBlock)
             end do; end do; end do
          end if

       end do ! iBlock

    else
       ! If there are no physics based criteria we start from
       ! the assumption of conservative everywhere
       IsConserv_CB(:,:,:,1:nBlock) = .true.

       if(DoTest .and. iProc==iProcTest)&
            write(*,*) NameSub, ': default IsConserv is true'
    endif

    do iBlock=1,nBlock
       if( Unused_B(iBlock) ) CYCLE

       ! Apply geometry based criteria
       ! Any of these can switch from conservative to non-conservative
       do iCrit=1,nConservCrit
          select case(TypeConservCrit_I(iCrit))
          case('r')
             ! Switch to non-conservative inside radius rConserv
             IsConserv_CB(:,:,:,iBlock) = IsConserv_CB(:,:,:,iBlock) .and. &
                  r_GB(1:nI,1:nJ,1:nK,iBlock) > rConserv
          case('parabola')
             ! Switch to non-conservative behind parabola inside the bow shock
             IsConserv_CB(:,:,:,iBlock) = IsConserv_CB(:,:,:,iBlock) .and. &
                  Xyz_DGB(x_,1:nI,1:nJ,1:nK,iBlock) > xParabolaConserv - &
                  ( Xyz_DGB(y_,1:nI,1:nJ,1:nK,iBlock)**2 &
                  + Xyz_DGB(z_,1:nI,1:nJ,1:nK,iBlock)**2 ) / yParabolaConserv
          case default
             CYCLE
          end select
          if(DoTest.and.iBlock==iBlockTest)&
               write(*,*) NameSub, ': TypeCrit, IsConserv=',&
               TypeConservCrit_I(iCrit), IsConserv_CB(iTest,jTest,kTest,iBlock)
       end do
    end do

    !$acc update device(IsConserv_CB)

    call timing_stop('nonconservative')

    call test_stop(NameSub, DoTest)

  end subroutine select_conservative
  !============================================================================
end module ModConservative
!==============================================================================
