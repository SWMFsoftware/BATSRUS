!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModUser

  use BATL_lib, ONLY: &
       test_start, test_stop, iProc, iTest, jTest, kTest

  use ModUserEmpty,                              &
       IMPLEMENTED1 => user_read_inputs,         &
       IMPLEMENTED2 => user_set_ics,             &
       IMPLEMENTED6 => user_set_cell_boundary

  use ModSize, ONLY: x_, y_, z_
  use ModVarIndexes
  use ModNumConst, ONLY: cTwoPi, cRadToDeg

  include 'user_module.h' ! list of public methods

  ! user module info
  character(len=*), parameter :: NameUserFile = "ModUserSwitchback.f90"
  character(len=*), parameter :: NameUserModule = "Switchback"

  real :: Phase, LambdaX, LambdaY
  real :: WidthBy, WidthUx, AmplBy, AmplUx
  integer :: iPowerUx
  integer:: iVarSwitchBack
  logical:: IsSubAlfvenic

contains
  !============================================================================
  subroutine user_read_inputs

    use ModReadParam

    character(len=100) :: NameCommand
    character(len=20)  :: NameVar
    logical:: DoTest

    character(len=*), parameter:: NameSub = 'user_read_inputs'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    UseUserIcs = .true.

    do
       if(.not.read_line() ) EXIT
       if(.not.read_command(NameCommand)) CYCLE
       select case(NameCommand)

       case('#SWITCHBACK')
          call read_var('Bperp',     AmplBy)
          call read_var('LambdaR',   LambdaX)
          call read_var('WidthBphi', WidthBy)
          call read_var('AmplitudeV',AmplUx)
          call read_var('LambdaY',   LambdaY)
          call read_var('WidthY',    WidthUx)
          call read_var('iPower',    iPowerUx)
          call read_var('NameVar',   NameVar)
          call get_iVar(NameVar, iVarSwitchBack)
          call read_var('IsSubAlfvenic', IsSubAlfvenic)
       case("#USERINPUTEND")
          EXIT
       case default
          if(iProc==0) call stop_mpi( &
               'read_inputs: unrecognized command: '//NameCommand)
       end select
    end do
    call test_stop(NameSub, DoTest)

  end subroutine user_read_inputs
  !============================================================================
  subroutine user_set_ics(iBlock)

    use ModGeometry, ONLY: &
         RadiusMin, r_GB
    use ModAdvance, ONLY: &
         State_VGB, RhoUx_, RhoUy_, RhoUz_, Ux_, &
         Bx_, By_, Bz_, rho_, p_
    use ModMultiFluid, ONLY:
    use ModPhysics, ONLY: &
         ShockLeft_V, &
         Io2No_V, &
         UnitU_, UnitB_, &
         Gamma
    use ModSize, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK
    use BATL_lib, ONLY: &
         Xyz_DGB
    use ModIonElectron, ONLY:

    integer, intent(in) :: iBlock

    real:: x, y, z, r, r2
    integer:: i, j, k
    real :: Rho
    real:: dB, dUr, dVwave, Valfven

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_ics'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    dB  = AmplBy*Io2No_V(UnitB_)
    dUr = AmplUx*Io2No_V(UnitU_)
    Valfven = abs(ShockLeft_V(Bx_))/sqrt(ShockLeft_V(Rho_))

    do k = MinK,MaxK; do j = MinJ,MaxJ; do i = MinI,MaxI
       r = r_GB(i,j,k,iBlock)
       x = Xyz_DGB(x_,i,j,k,iBlock)/r  ! x/r for X components
       y = Xyz_DGB(y_,i,j,k,iBlock)/r
       z = Xyz_DGB(z_,i,j,k,iBlock)/r
       r = r/RadiusMin ! normalize to inner boundary RadiusMin
       r2= r**2
       Rho = ShockLeft_V(Rho_)/r2
       State_VGB(Rho_,i,j,k,iBlock)    = Rho
       State_VGB(RhoUx_,i,j,k,iBlock)  = x*ShockLeft_V(Ux_)*Rho
       State_VGB(RhoUy_,i,j,k,iBlock)  = y*ShockLeft_V(Ux_)*Rho
       State_VGB(RhoUz_,i,j,k,iBlock)  = z*ShockLeft_V(Ux_)*Rho
       State_VGB(Bx_,i,j,k,iBlock)     = x*ShockLeft_V(Bx_)/r2
       State_VGB(By_,i,j,k,iBlock)     = y*ShockLeft_V(Bx_)/r2
       State_VGB(Bz_,i,j,k,iBlock)     = z*ShockLeft_V(Bx_)/r2
       State_VGB(p_,i,j,k,iBlock)      = ShockLeft_V(p_)/r2**Gamma
       if(r > 1 .and. (WidthBy < 0 .or. &
            r-1 < WidthBy/RadiusMin))then
          ! Circularly polarized Alfven waves from
          ! r=RadiusMin to RadiusMin + WidthBphi
          Phase = cTwoPi*(r-1)*RadiusMin/LambdaX
          State_VGB(Bx_,i,j,k,iBlock) = State_VGB(Bx_,i,j,k,iBlock) &
               - dB*sin(Phase)*y/r
          State_VGB(By_,i,j,k,iBlock) = State_VGB(By_,i,j,k,iBlock) &
               + dB*sin(Phase)*x/r
          State_VGB(Bz_,i,j,k,iBlock) = State_VGB(Bz_,i,j,k,iBlock) &
               + dB*cos(Phase)/r
          State_VGB(RhoUx_,i,j,k,iBlock) = State_VGB(RhoUx_,i,j,k,iBlock) &
               - dB*sin(Phase)*sqrt(Rho)*y/r
          State_VGB(RhoUy_,i,j,k,iBlock) = State_VGB(RhoUy_,i,j,k,iBlock) &
               + dB*sin(Phase)*sqrt(Rho)*x/r
          State_VGB(RhoUz_,i,j,k,iBlock) = State_VGB(RhoUz_,i,j,k,iBlock) &
               + dB*cos(Phase)*sqrt(Rho)/r
       end if

       ! Wave velocity perturbation
       if(r > 1 .and. &
            (WidthUx < 0 .or. abs(y)*RadiusMin < WidthUx)) then
          Phase = cRadToDeg*asin(y)/LambdaY
          dVwave = dUr*sin(cTwoPi*Phase)**iPowerUx
          select case(iVarSwitchBack)
          case(Ux_)
             State_VGB(RhoUx_,i,j,k,iBlock) = &
                  State_VGB(RhoUx_,i,j,k,iBlock) + Rho*dVwave*x
             State_VGB(RhoUy_,i,j,k,iBlock) = &
                  State_VGB(RhoUy_,i,j,k,iBlock) + Rho*dVwave*y
          case(Bx_)
             State_VGB(Bx_,i,j,k,iBlock) = &
                  State_VGB(Bx_,i,j,k,iBlock) + sqrt(Rho)*dVwave*x/r
             State_VGB(By_,i,j,k,iBlock) = &
                  State_VGB(By_,i,j,k,iBlock) + sqrt(Rho)*dVwave*y/r
          case(Rho_)
             State_VGB(Rho_,i,j,k,iBlock) = &
                  State_VGB(Rho_,i,j,k,iBlock) + Rho*2*dVWave/Valfven
          case default
             call stop_mpi(NameSub//': cannot perturbed this variable')
          end select
       end if
    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)

  end subroutine user_set_ics
  !============================================================================
  subroutine user_set_cell_boundary(iBlock, iSide, TypeBc, IsFound)

    ! set Alfven wave conditions at r minimum boundary

    use ModImplicit, ONLY: StateSemi_VGB
    use ModSize, ONLY: x_, y_, z_
    use ModPhysics, ONLY:&
         Io2No_V, UnitU_, UnitB_, &
         ShockLeft_V
    use ModMain, ONLY: tSimulation
    use ModAdvance, ONLY: Rho_, Ux_, RhoUx_, RhoUz_, State_VGB,p_
    use ModGeometry, ONLY: &
         r_GB
    use ModVarIndexes
    use BATL_lib, ONLY: Xyz_DGB

    integer, intent(in)          :: iBlock, iSide
    character(len=*), intent(in) :: TypeBc
    logical, intent(out)         :: IsFound

    integer :: i, j, k
    real    :: x, y, z, r

    real:: Rho, Br, Ur, dB, dUr, SqrtRho, dVwave, Valfven

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_cell_boundary'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    IsFound = .false.
    if(iSide /= 1) RETURN
    IsFound = .true.

    if(DoTest)write(*,*) NameSub,' ShockLeft_V=', ShockLeft_V

    Rho = ShockLeft_V(Rho_)
    SqrtRho = sqrt(Rho)
    Br  = ShockLeft_V(Bx_)
    Ur  = ShockLeft_V(Ux_)

    dB  = AmplBy*Io2No_V(UnitB_) ! Alfven wave amplitude
    dUr = AmplUx*Io2No_V(UnitU_) ! Velocity perturbation
    Valfven = abs(Br)/SqrtRho

    if(DoTest)write(*,*) NameSub,': Rho, Br, Ur, dB, dUr=', &
         Rho, Br, Ur, dB, dUr

    do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, 0
       r = r_GB(i,j,k,iBlock)
       x = Xyz_DGB(x_,i,j,k,iBlock)/r  ! x/r for X components
       y = Xyz_DGB(y_,i,j,k,iBlock)/r  ! y/r for Y components
       z = Xyz_DGB(z_,i,j,k,iBlock)/r  ! z/r for Z components

       ! Phase = t*omega = t*kx*(Ur+Valfven)
       !       = t*2*pi/LambdaX*(Ur+Valfven)
       Phase = -tSimulation*cTwoPi/LambdaX*(Ur + Valfven)

       ! Density
       State_VGB(Rho_,i,j,k,iBlock) = Rho

       ! Momentum: radial flow + Alven wave + velocity perturbation
       State_VGB(RhoUx_,i,j,k,iBlock) = x*Ur*Rho - dB*sin(Phase)*SqrtRho*y
       State_VGB(RhoUy_,i,j,k,iBlock) = y*Ur*Rho + dB*sin(Phase)*SqrtRho*x
       State_VGB(RhoUz_,i,j,k,iBlock) = z*Ur*Rho + dB*cos(Phase)*SqrtRho

       ! Magnetic field: radial field + Alfven wave
       State_VGB(Bx_,i,j,k,iBlock) = x*Br - dB*sin(Phase)*y
       if(IsSubAlfvenic)then
          ! floating (allow for incoming or outgoing waves)
          State_VGB(By_:Bz_,i,j,k,iBlock) = State_VGB(By_:Bz_,1,j,k,iBlock)
       else
          ! outgoing circularly polarized Alfven waves
          State_VGB(By_,i,j,k,iBlock) = y*Br + dB*sin(Phase)*x
          State_VGB(Bz_,i,j,k,iBlock) = z*Br + dB*cos(Phase)
       end if

       ! Pressure
       State_VGB(p_,i,j,k,iBlock) = ShockLeft_V(P_)

       ! Add Y dependent wave perturbation
       Phase = cRadToDeg*asin(y)/LambdaY
       dVwave = dUr*sin(cTwoPi*Phase)**iPowerUx
       select case(iVarSwitchBack)
       case(Ux_)
          State_VGB(RhoUx_,i,j,k,iBlock) = &
               State_VGB(RhoUx_,i,j,k,iBlock) + Rho*dVwave*x
          State_VGB(RhoUy_,i,j,k,iBlock) = &
               State_VGB(RhoUy_,i,j,k,iBlock) + Rho*dVwave*y
       case(Bx_)
          State_VGB(Bx_,i,j,k,iBlock) = &
               State_VGB(Bx_,i,j,k,iBlock) + sqrt(Rho)*dVwave*x
          State_VGB(By_,i,j,k,iBlock) = &
               State_VGB(By_,i,j,k,iBlock) + sqrt(Rho)*dVwave*y
       case(Rho_)
          State_VGB(Rho_,i,j,k,iBlock) = &
               State_VGB(Rho_,i,j,k,iBlock) + Rho*2*dVWave/Valfven
       case default
          call stop_mpi(NameSub//': cannot perturbed this variable')
       end select

    end do; end do; end do

    if(DoTest)write(*,*) NameSub,': final State_V=', &
         State_VGB(:,iTest,jTest,kTest,iBlock)

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_cell_boundary
  !============================================================================
end module ModUser
!==============================================================================
