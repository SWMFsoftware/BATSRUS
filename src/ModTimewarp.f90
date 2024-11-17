module ModTimeWarp

  ! Chang time variable from t to t' = t + r/uWarp
  ! uWarp must be faster than the fastest wave speed in the radial direction
  ! All characteristics need to point in the positive r direction

  use ModAdvance, ONLY: StateOld_VGB, State_VGB
  use ModGeometry, ONLY: r_GB
  use ModVarIndexes, ONLY: nVar, Rho_, RhoU_, RhoUx_, RhoUz_, p_
  use ModBatsrusUtility, ONLY: stop_mpi
  use BATL_lib, ONLY: Used_GB, Xyz_DGB, nI, nJ, nK

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
          uWarp = -1.0
       end if
    case("#WARPDIM")
       call read_var("iDimWarp", iDimWarp)
    case("#WARPCMAX")
       call read_var("UseWarpCmax", UseWarpCmax)
    case default
       call stop_mpi(NameSub//': unknown command='//NameCommand)
    end select
    
  end subroutine read_timewarp_param
  !============================================================================
  subroutine state_to_warp(iBlock)

    ! Convert from State to State - Flux_x/uWarp
    ! Isothermal HD only

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
  !==========================================================================
  subroutine state_to_warp_cell(State_V, i, j, k, iBlock)

    use ModPhysics, ONLY: Io2No_V, UnitU_

    real, intent(inout):: State_V(nVar)
    integer, intent(in):: i, j, k, iBlock

    real:: Flux_V(nVar), Norm_D(3), RhoUr
    !------------------------------------------------------------------------
    ! Store temperature
    !--------------------------------------------------------------------------
    if(TempWarp < 0) TempWarp = State_V(p_)/State_V(Rho_)
    ! Store normalized warp speed
    if(uWarp < 0) uWarp = uWarpDim*Io2No_V(UnitU_)
    ! Calculate flux
    Flux_V = 0.0
    if(iDimWarp == 0)then
       ! radial direction is warped
       Norm_D = Xyz_DGB(:,i,j,k,iBlock)/r_GB(i,j,k,iBlock)
       RhoUr = sum(State_V(RhoUx_:RhoUz_)*Norm_D)
       Flux_V(Rho_) = RhoUr
       Flux_V(RhoUx_:RhoUz_) = RhoUr*State_V(RhoUx_:RhoUz_)/State_V(Rho_) &
            + State_V(p_)*Norm_D
       Flux_V(p_) = RhoUr*TempWarp
    else
       ! X direction is warped
       Flux_V(Rho_) = State_V(RhoU_+iDimWarp)
       Flux_V(RhoU_+iDimWarp) = State_V(RhoU_+iDimWarp)**2/State_V(Rho_) &
            + State_V(p_)
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
       ! StateOld could be stored instead of recalculated !!!
       call warp_to_state_cell(StateOld_VGB(:,i,j,k,iBlock), i, j, k, iBlock)
       call warp_to_state_cell(State_VGB(:,i,j,k,iBlock), i, j, k, iBlock)

    end do; end do; end do

  contains
    !==========================================================================
    subroutine warp_to_state_cell(State_V, i, j, k, iBlock)

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

    end subroutine warp_to_state_cell
    !==========================================================================
  end subroutine warp_to_state
  !============================================================================
end module ModTimeWarp
