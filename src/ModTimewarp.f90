module ModTimewarp

  ! Change time variable from t to t' = t + r/uWarp
  ! uWarp must be faster than the fastest wave speed in the radial direction
  ! All characteristics need to point in the positive r direction

  use ModAdvance, ONLY: StateOld_VGB, StateOld_VG, State_VGB
  use ModGeometry, ONLY: r_GB
  use ModVarIndexes, ONLY: nVar, Rho_, RhoU_, RhoUx_, RhoUz_, p_
  use ModBatsrusUtility, ONLY: stop_mpi
  use BATL_lib, ONLY: Used_GB, Xyz_DGB, nI, nJ, nK, iProc

  implicit none

  SAVE

  private ! except

  public:: read_timewarp_param ! read parameters
  public:: state_to_warp_cell  ! Convert one cell to time warped variables
  public:: state_to_warp       ! Convert a block to time warped variables
  public:: warp_to_state       ! Convert a block from warp to state variables

  ! Time warp parameters
  logical, public:: UseTimeWarp = .false. ! Use time warp scheme
  logical, public:: UseWarpCmax = .true.  ! Use warp numerical diffusion
  integer, public:: iDimWarp = 0          ! 0 is for radial direction
  real, public::    uWarpDim = 0.0        ! Dimensional warp speed
  real, public::    uWarp = 0.0           ! Normalized warp speed

  ! Local variables
  real:: TempWarp = -1.0 ! dimensionless temperature of isothermal hydro

  ! Iterative scheme
  logical:: UseIteration = .true.
  integer:: MaxIteration = 20
  real:: Tolerance = 1e-8

contains
  !============================================================================
  subroutine read_timewarp_param(NameCommand)

    use ModReadParam, ONLY: read_var

    character(len=*), intent(in):: NameCommand

    character(len=*), parameter:: NameSub = 'read_timewarp_param'
    !--------------------------------------------------------------------------
    select case(NameCommand)
    case("#TIMEWARP")
       call read_var("UseTimeWarp", UseTimeWarp)
       if(UseTimeWarp)then
          call read_var("uWarpDim", uWarpDim)
          uWarp = -1.0 ! force recalculation of uWarp
       end if
    case("#WARPDIM")
       call read_var("iDimWarp", iDimWarp)
    case("#WARPCMAX")
       call read_var("UseWarpCmax", UseWarpCmax)
    case("#WARPSCHEME")
       call read_var("UseIteration", UseIteration)
       if(UseIteration)then
          call read_var("Tolerance", Tolerance)
          call read_var("MaxIteration", MaxIteration)
       end if
    case default
       call stop_mpi(NameSub//': unknown command='//NameCommand)
    end select

  end subroutine read_timewarp_param
  !============================================================================
  subroutine state_to_warp(iBlock)

    ! Convert from State to State - Flux_r/uWarp
    ! HD only for now

    integer, intent(in):: iBlock

    integer:: i, j, k

    character(len=*), parameter:: NameSub = 'state_to_warp'
    !--------------------------------------------------------------------------
    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if(.not.Used_GB(i,j,k,iBlock)) CYCLE
       call state_to_warp_cell(StateOld_VGB(:,i,j,k,iBlock), i, j, k, iBlock)
       call state_to_warp_cell(State_VGB(:,i,j,k,iBlock), i, j, k, iBlock)
    end do; end do; end do

  end subroutine state_to_warp
  !============================================================================
  subroutine state_to_warp_cell(State_V, i, j, k, iBlock, IsConservIn)

    use ModConservative, ONLY: UseNonConservative, nConservCrit, IsConserv_CB
    use ModPhysics, ONLY: Io2No_V, UnitU_

    integer:: e_ = p_

    real, intent(inout):: State_V(nVar)
    integer, intent(in):: i, j, k, iBlock
    logical, optional, intent(in):: IsConservIn

    real:: Flux_V(nVar), Norm_D(3), InvRho, RhoUr, Ur, p
    logical:: IsConserv
    !--------------------------------------------------------------------------
    if(.not.UseIteration)then
       ! Store temperature for isothermal hydro with analytic warp_to_state
       if(TempWarp < 0) TempWarp = State_V(p_)/State_V(Rho_)
    end if
    ! Store normalized warp speed
    if(uWarp < 0) uWarp = uWarpDim*Io2No_V(UnitU_)

    if(present(IsConservIn))then
       IsConserv = IsConservIn
    else
       IsConserv = .not.UseNonConservative
       if(UseNonConservative .and. nConservCrit > 0)then
          IsConserv = IsConserv_CB(i,j,k,iBlock)
       end if
    end if
    ! To be replaced with call energy_to_pressure1(Prim_V, i, j, k, iBlock)
    InvRho = 1/State_V(Rho_)
    if(IsConserv)then
       p = State_V(e_) - 0.5*InvRho*sum(State_V(RhoUx_:RhoUz_)**2)
    else
       p = State_V(p_)
    end if
    ! Get the warp direction
    if(iDimWarp == 0)then
       ! radial direction is warped
       Norm_D = Xyz_DGB(:,i,j,k,iBlock)/r_GB(i,j,k,iBlock)
    else
       ! iDimWarp direction is warped
       Norm_D = 0.0
       Norm_D(iDimWarp) = 1.0
    end if
    ! To be replaced with call get_physical_flux(State_V, Norm_V, ... Flux_V)
    RhoUr = sum(State_V(RhoUx_:RhoUz_)*Norm_D)
    Ur = InvRho*RhoUr
    ! Density flux
    Flux_V(Rho_) = RhoUr
    ! Momentum flux
    Flux_V(RhoUx_:RhoUz_) = Ur*State_V(RhoUx_:RhoUz_) + p*Norm_D
    if(IsConserv)then
       ! Energy flux
       Flux_V(e_) = Ur*(p + State_V(e_))
    else
       ! Pressure flux
       Flux_V(p_) = Ur*p
    end if

    ! Convert to warp variables
    State_V = State_V - Flux_V/uWarp

  end subroutine state_to_warp_cell
  !============================================================================
  subroutine warp_to_state(iBlock)

    ! Convert from State - uWarp*Flux_x back to State
    ! Isothermal HD only

    integer, intent(in):: iBlock

    integer:: i, j, k
    character(len=*), parameter:: NameSub = 'warp_to_state'
    !--------------------------------------------------------------------------
    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if(.not.Used_GB(i,j,k,iBlock)) CYCLE
       if(UseIteration)then
          call warp_to_state_cell1(State_VGB(:,i,j,k,iBlock), i, j, k, iBlock)
       else
          call warp_to_state_cell2(State_VGB(:,i,j,k,iBlock), i, j, k, iBlock)
       end if
    end do; end do; end do

  contains
    !==========================================================================
    subroutine warp_to_state_cell1(Warp_V, i, j, k, iBlock)

      ! Iterative conversion from warp state to regular state

      real, intent(inout):: Warp_V(nVar)
      integer, intent(in):: i, j, k, iBlock

      real:: Jac_VV(nVar,nVar)

      integer:: iIter
      real:: StateIter_V(nVar), WarpIter_V(nVar)
      !------------------------------------------------------------------------
      StateIter_V = StateOld_VG(:,i,j,k) ! initial guess
      ! Iterate to obtain the solution
      do iIter = 1, MaxIteration
         ! Convert StateIter_V to WarpIter_V = StateIter_V - F_r/uWarp
         WarpIter_V = StateIter_V
         call state_to_warp_cell(WarpIter_V, i, j, k, iBlock)
         ! Check if we reached the desired accuracy
         if(all(abs(Warp_V - WarpIter_V) <= &
              Tolerance*(abs(Warp_V) + abs(WarpIter_V)))) EXIT

         call get_jacobian(StateIter_V, WarpIter_V, Jac_VV)

         ! Nudge the solution based on the error
         StateIter_V = StateIter_V + matmul(Jac_VV, Warp_V - WarpIter_V)
      end do
      if(iIter > MaxIteration .and. MaxIteration > 1)then
         write(*,*) NameSub,': Warning, iteration did not succeed at'
         write(*,*) NameSub,': i,j,k,iBlock,iProc=', i, j, k, iBlock, iProc
         write(*,*) NameSub,': Xyz_D=', Xyz_DGB(:,i,j,k,iBlock)
         write(*,*) NameSub,': StateOld_V =', StateOld_VG(:,i,j,k)
         write(*,*) NameSub,': StateIter_V=', StateIter_V
         write(*,*) NameSub,': Warp_V     =', Warp_V
         write(*,*) NameSub,': WarpIter_V =', WarpIter_V
      end if

      ! Return the iterative solution
      Warp_V = StateIter_V

    end subroutine warp_to_state_cell1
    !==========================================================================
    subroutine get_jacobian(State_V, Warp_V, Jac_VV)

      ! Calculate dU/dW as the inverse of dW/dU

      use ModCoordTransform, ONLY: inverse_matrix
      use ModNumConst, ONLY: i_DD

      real, intent(in):: State_V(nVar), Warp_V(nVar)
      real, intent(out):: Jac_VV(nVar,nVar)

      real:: StateEps_V(nVar), WarpEps_V(nVar)
      real, parameter:: Eps = 1e-6

      integer:: iVar
      !------------------------------------------------------------------------
      ! Calculate dW/dU numerically
      do iVar = 1, nVar
         StateEps_V = State_V
         StateEps_V(iVar) = StateEps_V(iVar) + Eps
         WarpEps_V = StateEps_V
         call state_to_warp_cell(WarpEps_V, i, j, k, iBlock)
         Jac_VV(:,iVar) = (WarpEps_V - Warp_V)/Eps
      end do
      ! dU/dW = inverse(dW/dU)
      Jac_VV = inverse_matrix(nVar, Jac_VV)

    end subroutine get_jacobian
    !==========================================================================
    subroutine warp_to_state_cell2(State_V, i, j, k, iBlock)

      ! Analytic conversion for isothermal hydrodynamics

      real, intent(inout):: State_V(nVar)
      integer, intent(in):: i, j, k, iBlock

      real:: RhoWarp, Norm_D(3), RhoUWarp, a, b, c, d, Rho
      !------------------------------------------------------------------------
      RhoWarp   = State_V(Rho_)
      if(iDimWarp == 0)then
         Norm_D = Xyz_DGB(:,i,j,k,iBlock)/r_GB(i,j,k,iBlock)
         RhoUWarp = sum(State_V(RhoUx_:RhoUz_)*Norm_D)
      else
         RhoUWarp = State_V(RhoU_+iDimWarp)
      end if
      ! Coefficients of quadratic equation a*Rho^2 + b*rho + c = 0
      a = TempWarp/uWarp**2
      b = -(RhoWarp - RhoUWarp/uWarp)
      c = RhoWarp**2
      ! Determinant
      d = b**2 - 4*a*c
      if(d < 0)then
         write(*,*) NameSub,': i,j,k,iBlock=', i, j, k, iBlock
         write(*,*) NameSub,': Xyz_D=', Xyz_DGB(:,i,j,k,iBlock)
         write(*,*) NameSub,': State_V=', State_V
         write(*,*) NameSub,': RhoWarp,RhoUWarp,TempWarp=', &
              RhoWarp, RhoUWarp, TempWarp
         write(*,*) NameSub,': a,b,c,d=', a, b, c, d
         if(d < 0) call stop_mpi(NameSub//': negative determinant')
      end if
      ! We take the larger root (to be checked)
      Rho = (-b - sqrt(b**2 - 4*a*c))/(2*a)
      State_V(Rho_) = Rho
      if(iDimWarp == 0)then
         State_V(RhoUx_:RhoUz_) = uWarp*(Rho - RhoWarp)*Norm_D
      else
         State_V(RhoU_+iDimWarp) = uWarp*(Rho - RhoWarp)
      end if
      State_V(p_) = TempWarp*Rho

    end subroutine warp_to_state_cell2
    !==========================================================================
  end subroutine warp_to_state
  !============================================================================
end module ModTimewarp
!==============================================================================
