module ModTimewarp

  ! Change time variable from t to t' = t + r/uWarp
  ! uWarp must be faster than the fastest wave speed in the radial direction
  ! All characteristics need to point in the positive r direction

  use ModAdvance, ONLY: StateOld_VGB, StateOld_VG, State_VGB, nFlux
  use ModGeometry, ONLY: r_GB
  use ModVarIndexes, ONLY: nVar, nIonFluid, nFluid
  use ModMultiFluid, ONLY: iRho_I, iRhoUx_I, iRhoUz_I, iUx_I, iUz_I, iP_I
  use ModConservative, ONLY: is_conserv
  use ModEnergy, ONLY: energy_i, pressure_i
  use ModPhysics, ONLY: Io2No_V, UnitU_
  use ModPhysicalFLux, ONLY: Normal_D, get_physical_flux
  use ModBatsrusUtility, ONLY: stop_mpi, error_report
  use BATL_lib, ONLY: Used_GB, Xyz_DGB, nI, nJ, nK, test_start, test_stop

  implicit none

  SAVE

  private ! except

  public:: read_timewarp_param ! read parameters
  public:: init_mod_timewarp   ! initialized module
  public:: state_to_warp_cell  ! Convert one cell to time warped variables
  public:: state_to_warp       ! Convert a block to time warped variables
  public:: warp_to_state       ! Convert a block from warp to state variables

  ! Time warp parameters
  logical, public:: UseTimeWarp = .false. ! Use time warp scheme
  logical, public:: UseWarpCmax = .true.  ! Use warp numerical diffusion
  integer, public:: iDimWarp = 0          ! 0 is for radial direction
  real, public::    uWarpDim = 0.0        ! Dimensional warp speed
  real, public::    uWarp = 0.0           ! Normalized warp speed

  ! Iterative scheme
  integer:: MaxIteration = 20, DnJacobian = 1
  real:: Tolerance = 1e-10
  real:: EpsRel = 1e-6, EpsAbs = 1e-10

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
       call read_var("Tolerance", Tolerance)
       call read_var("MaxIteration", MaxIteration)
       call read_var("DnJacobian", DnJacobian)
       call read_var("EpsRel", EpsRel)
       call read_var("EpsAbs", EpsAbs)
    case default
       call stop_mpi(NameSub//': unknown command='//NameCommand)
    end select

  end subroutine read_timewarp_param
  !============================================================================
  subroutine init_mod_timewarp

    ! Store normalized warp speed
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_mod_timewarp'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    uWarp = uWarpDim*Io2No_V(UnitU_)
    if(DoTest) write(*,*) NameSub,': uWarp, uWarpDim=', uWarp, uWarpDim
    call test_stop(NameSub, DoTest)

  end subroutine init_mod_timewarp
  !============================================================================
  subroutine state_to_warp(iBlock, DoStateOldOnly)

    ! Convert from StateOld to StateOld - Flux_r/uWarp
    ! If DoStateOldOnly is false, then also do State_VGB

    integer, intent(in):: iBlock
    logical, intent(in):: DoStateOldOnly

    integer:: i, j, k

    character(len=*), parameter:: NameSub = 'state_to_warp'
    !--------------------------------------------------------------------------
    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if(.not.Used_GB(i,j,k,iBlock)) CYCLE
       call state_to_warp_cell( &
            nVar, StateOld_VGB(:,i,j,k,iBlock), i, j, k, iBlock)
       if(.not.DoStateOldOnly) call state_to_warp_cell( &
            nVar, State_VGB(:,i,j,k,iBlock), i, j, k, iBlock)
    end do; end do; end do

  end subroutine state_to_warp
  !============================================================================
  subroutine state_to_warp_cell(nVarState, State_V, i, j, k, iBlock)

    ! Convert state into warp variable for pressure and/or energy.
    ! When nVarState = nFlux, convert both, when nVarState = nVar,
    ! then convert either pressures or energies.

    integer, intent(in):: nVarState
    real, intent(inout):: State_V(nVarState)
    integer, intent(in):: i, j, k, iBlock

    integer:: iFluid
    real:: InvRho, Prim_V(nVar), Flux_V(nFlux)
    ! for now. Should become optional arguments
    real:: Un_I(nFluid+1), En, StateCons_V(nFlux), NormalOld_D(3)

    ! Convert to primitive variables
    !--------------------------------------------------------------------------
    Prim_V = State_V(1:nVar)
    do iFluid = 1, nFluid
       ! Convert momenta to velocities
       InvRho = 1/State_V(iRho_I(iFluid))
       Prim_V(iUx_I(iFluid):iUz_I(iFluid)) = &
            InvRho*State_V(iRhoUx_I(iFluid):iRhoUz_I(iFluid))
       ! Check if both pressures and energies are present
       if(nVarState == nFlux) CYCLE
       ! Convert energy to pressure if necessary
       if(is_conserv(i, j, k, iBlock, iFluid)) &
            Prim_V(iP_I(iFluid)) = pressure_i(State_V, iFluid)
    end do

    ! Store current Normal_D (why??)
    NormalOld_D = Normal_D
    ! Get the warp direction
    if(iDimWarp == 0)then
       ! radial direction is warped
       Normal_D = Xyz_DGB(:,i,j,k,iBlock)/r_GB(i,j,k,iBlock)
    else
       ! iDimWarp direction is warped
       Normal_D = 0.0
       Normal_D(iDimWarp) = 1.0
    end if
    ! Get the flux in the warp direction
    call get_physical_flux(Prim_V, StateCons_V, Flux_V, Un_I, En)

    ! Recover Normal_D ??
    Normal_D = NormalOld_D

    ! Put energy fluxes into pressure fluxes if necessary
    if(nVarState == nVar)then
       do iFluid = 1, nFluid
          if(is_conserv(i, j, k, iBlock, iFluid)) &
               Flux_V(iP_I(iFluid)) = Flux_V(nVar+iFluid)
       end do
    end if
    ! Convert to warp variables
    State_V = State_V - Flux_V(1:nVarState)/uWarp

  end subroutine state_to_warp_cell
  !============================================================================
  subroutine warp_to_state(iBlock)

    ! Convert from State - uWarp*Flux_x back to State
    ! in State_VGB((:,1:nI,1:nJ,1:nK,nIiBlock)

    integer, intent(in):: iBlock

    integer:: i, j, k
    character(len=*), parameter:: NameSub = 'warp_to_state'
    !--------------------------------------------------------------------------
    call timing_start(NameSub)
    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if(.not.Used_GB(i,j,k,iBlock)) CYCLE
       call warp_to_state_cell(State_VGB(:,i,j,k,iBlock), i, j, k, iBlock)
    end do; end do; end do
    call timing_stop(NameSub)

  contains
    !==========================================================================
    subroutine warp_to_state_cell(Warp_V, i, j, k, iBlock)

      ! Iterative conversion from warp state to regular state

      use ModPointImplicit, ONLY: linear_equation_solver

      real, intent(inout):: Warp_V(nVar)
      integer, intent(in):: i, j, k, iBlock

      ! Jacobian matric dW/dU
      real:: Jac_VV(nVar,nVar)

      integer:: iFluid, iIter, iError = -1
      real:: StateIter_V(nVar), WarpIter_V(nVar), dIter_V(nVar), Error
      logical:: DoLu
      !------------------------------------------------------------------------
      ! StateOld_VG has pressures
      StateIter_V = StateOld_VG(:,i,j,k)

      ! Convert pressures to energies to simplify iterative scheme
      ! Entropies and Boris not handled yet !!!
      do iFluid = 1, nFluid
         if(is_conserv(i, j, k, iBlock, iFluid)) &
              StateIter_V(iP_I(iFluid)) = energy_i(StateIter_V, iFluid)
      end do

      ! Iterate to obtain the solution
      do iIter = 1, MaxIteration
         ! Convert StateIter_V to WarpIter_V = StateIter_V - F_r/uWarp
         WarpIter_V = StateIter_V
         call state_to_warp_cell(nVar, WarpIter_V, i, j, k, iBlock)

         ! Check if we reached the desired accuracy
         if(all(abs(Warp_V - WarpIter_V) <= &
              Tolerance*(abs(Warp_V) + abs(WarpIter_V)))) EXIT

         ! Get dW/dU matrix
         if(modulo(iIter - 1, DnJacobian) == 0)then
            call get_jacobian(StateIter_V, WarpIter_V, Jac_VV)
            DoLu = .true.
         else
            DoLu = .false.
         end if

         ! Newton iteration: U_(k+1) = U_k + (dW/dU)^(-1).(W - W_k)
         ! Find solution to Jac_VV*dU = dW
         dIter_V = Warp_V - WarpIter_V
         call linear_equation_solver(nVar, Jac_VV, dIter_V, DoLuIn=DoLu)

         ! Update iteration
         StateIter_V = StateIter_V + dIter_V

      end do
      if(iIter > MaxIteration .and. MaxIteration > 1)then
         ! Check if we reached a reasonable accuracy
         Error = maxval(abs(Warp_V - WarpIter_V) &
              /        (abs(Warp_V) + abs(WarpIter_V) + 1e-30))
         if(Error > 1e-6)then
            ! If not, report the error
            if(iError < 0)then
               ! Write out details for the first time when iError is not set
               write(*,*) NameSub,': i,j,k,iBlock=', i, j, k, iBlock
               write(*,*) NameSub,': Xyz_D=', Xyz_DGB(:,i,j,k,iBlock)
               write(*,*) NameSub,': StateOld_V =', StateOld_VG(:,i,j,k)
               write(*,*) NameSub,': StateIter_V=', StateIter_V
               write(*,*) NameSub,': Warp_V     =', Warp_V
               write(*,*) NameSub,': WarpIter_V =', WarpIter_V
            end if
            call error_report(NameSub// &
                 ": Timewarp iteration failed, relative error", Error, &
                 iError, .true.)
         end if
      end if

      ! Return the iterative solution
      Warp_V = StateIter_V

    end subroutine warp_to_state_cell
    !==========================================================================
    subroutine get_jacobian(State_V, Warp_V, Jac_VV)

      ! Calculate dW/dU matrix Jac_VV for U=State_V and W=Warp_V.

      real, intent(in):: State_V(nVar), Warp_V(nVar)
      real, intent(out):: Jac_VV(nVar,nVar)

      real:: StateEps_V(nVar), WarpEps_V(nVar)
      real:: Eps

      integer:: iVar
      !------------------------------------------------------------------------
      do iVar = 1, nVar
         ! Perturb iVar in State_V
         StateEps_V = State_V
         Eps = abs(State_V(iVar))*EpsRel + EpsAbs
         StateEps_V(iVar) = StateEps_V(iVar) + Eps
         ! Calculate perturbed warped variable
         WarpEps_V = StateEps_V
         call state_to_warp_cell(nVar, WarpEps_V, i, j, k, iBlock)
         ! Calculate derivative
         Jac_VV(:,iVar) = (WarpEps_V - Warp_V)/Eps
      end do

    end subroutine get_jacobian
    !==========================================================================
  end subroutine warp_to_state
  !============================================================================
end module ModTimewarp
!==============================================================================
