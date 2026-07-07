!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

!  Kelvin-Helmholtz instability setup following the Athena test suite
!  (Stone et al. 2008, ApJS).  Three-layer slab geometry with shear flow
!  in x and density contrast between the inner slab and outer regions.
!
!  Test description: https://www.astro.princeton.edu/~jstone/Athena/tests/kh/kh.html
!
!  Perturbation modes:
!    'random'     – Athena-style random noise on vx and vy (default)
!    'sinusoidal' – single-mode sin perturbation on vy only
!                   (for linear growth-rate comparisons)
!
!  Setup types (selected via #SETUP in PARAM.in):
!    '3layer' – Athena-style symmetric three-layer slab (default).
!               Shear interfaces at |y| = yShear.  All boundaries periodic.
!    '2layer' – Asymmetric two-layer setup (Venus-like ionosphere).
!               Interface at y = yShear.  Upper layer (y > yShear) is
!               less dense, fast, magnetized; lower layer (y <= yShear)
!               is dense, slow, unmagnetized.  Top/bottom boundaries
!               should use outflow (or fixed), left/right periodic.
!
!  Works for HD, MHD, and MultiIon equations.  The magnetic field is set
!  uniformly via #UNIFORMSTATE in the PARAM.in file.  For MultiIon, the
!  total density (rhoInner/rhoOuter) is split equally among all ion fluids;
!  each fluid receives the same velocity profile and perturbation.

module ModUser

  use BATL_lib, ONLY: &
       test_start, test_stop, iProcTest, iProc

  use ModUserEmpty,               &
       IMPLEMENTED1 => user_read_inputs,     &
       IMPLEMENTED2 => user_initial_perturbation

  include 'user_module.h' ! list of public methods

  character (len=*), parameter :: NameUserFile = "ModUserKelvinHelmholtz.f90"
  character (len=*), parameter :: NameUserModule = &
       'KELVIN-HELMHOLTZ INSTABILITY'

  ! Setup type: '3layer' (Athena-style, symmetric) or '2layer' (Venus-like)
  character(len=20), save :: TypeSetup = '3layer'

  ! KHI parameters (shared by both setups)
  !   3-layer: shear interfaces at |y| = yShear, inner slab has rhoInner
  !   2-layer: interface at y = yShear, lower layer has rhoInner
  real, save :: &
       yShear   = 0.25, &  ! Shear interface y-position
       ySmooth  = 0.0,  &  ! Smoothing width (0 = sharp jump)
       rhoInner = 2.0,  &  ! Density in inner/lower layer
       rhoOuter = 1.0,  &  ! Density in outer/upper layer
       dvx      = 0.5,  &  ! Shear velocity amplitude (upper/inner layer)
       pertAmp  = 0.01, &  ! Perturbation amplitude (peak-to-peak for random)
       nWaveX   = 1.0,  &  ! Number of wavelengths in x (sinusoidal mode)
       pertWidth = 0.0, &  ! Gaussian envelope width in y (0 = global; sinusoidal only)
       SeedPert = 1.0      ! Random seed for perturbation

  ! Type of perturbation: 'random' (Athena-style noise on vx,vy)
  ! or 'sinusoidal' (single-mode sin on vy only)
  character(len=20), save :: TypePerturbation = 'random'

  ! 2-layer setup parameters (Venus-like, used only when TypeSetup='2layer')
  real, save :: &
       BzUpper  = 0.0, &  ! Bz in upper layer (2-layer setup only)
       UxLower  = 0.0, &  ! Vx in lower layer (2-layer setup only)
       PUpper   = 0.0     ! Thermal pressure in upper layer (2-layer setup only)

contains
  !============================================================================
  subroutine user_read_inputs
    use ModReadParam

    character (len=100) :: NameCommand
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_read_inputs'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    do
       if(.not.read_line() ) EXIT
       if(.not.read_command(NameCommand)) CYCLE
       select case(NameCommand)

       case('#SETUP')
          call read_var('TypeSetup', TypeSetup)
          select case(TypeSetup)
          case('3layer', '2layer')
             ! valid
          case default
             if(iProc==0) call stop_mpi( &
                  NameSub//': unknown TypeSetup='//trim(TypeSetup))
          end select
          if(TypeSetup == '2layer')then
             call read_var('BzUpper', BzUpper)
             call read_var('UxLower', UxLower)
             call read_var('PUpper',  PUpper)
          end if

       case('#PERTURBATION')
          call read_var('yShear', yShear)
          call read_var('ySmooth', ySmooth)
          call read_var('rhoInner', rhoInner)
          call read_var('rhoOuter', rhoOuter)
          call read_var('dvx', dvx)
          call read_var('pertAmp', pertAmp)
          call read_var('nWaveX', nWaveX)
          call read_var('pertWidth', pertWidth)
          call read_var('SeedPert', SeedPert)
          call read_var('TypePerturbation', TypePerturbation)

       case('#USERINPUTEND')
          if(iProc==0) write(*,*)'USERINPUTEND'
          EXIT
       case default
          if(iProc==0) call stop_mpi( &
               'read_inputs: unrecognized command: '//NameCommand)
       end select
    end do
    call test_stop(NameSub, DoTest)
  end subroutine user_read_inputs
  !============================================================================

  subroutine user_initial_perturbation

    use ModMain, ONLY: nBlock, Unused_B, nI, nJ, nK, UseB
    use ModAdvance, ONLY: State_VGB
    use ModMultiFluid, ONLY: nFluid, select_fluid, iRho, iRhoUx, iRhoUy, iP
    use ModVarIndexes, ONLY: Bz_
    use ModGeometry, ONLY: Xyz_DGB, xMinBox, xMaxBox
    use ModNumConst, ONLY: cTwoPi
    use ModPhysics, ONLY: Io2No_V, UnitX_, UnitU_, UnitRho_, UnitB_, UnitP_

    integer :: iBlock, i, j, k, iFluid
    real :: Lx, RandomVal, PertUx, PertUy, RhoFluidInner, RhoFluidOuter
    real :: Trans, PLower
    integer :: SeedSize, iSeed
    integer, allocatable :: Seed_I(:)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_initial_perturbation'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(iProc==iProcTest)then
       write(*,*)'Initializing Kelvin-Helmholtz problem'
       write(*,*)'TypeSetup =', TypeSetup
       write(*,*)'Parameters:'
       write(*,*)'yShear   =',yShear,  ' ySmooth  =',ySmooth
       write(*,*)'rhoInner =',rhoInner,' rhoOuter =',rhoOuter
       write(*,*)'dvx      =',dvx,     ' pertAmp  =',pertAmp
       write(*,*)'nWaveX   =',nWaveX,  ' pertWidth=',pertWidth, &
            ' Seed =',SeedPert
       write(*,*)'TypePerturbation = ',TypePerturbation
       write(*,*)'nFluid   =',nFluid
       if(TypeSetup == '2layer')then
          write(*,*)'BzUpper  =',BzUpper, ' UxLower  =',UxLower
          write(*,*)'PUpper   =',PUpper
       end if
    else
       DoTest=.false.; DoTest=.false.
    end if

    ! Convert I/O parameters to normalized (code) units.
    ! This makes the module work with any TypeIoUnit / TypeNormalization.
    yShear  = yShear  * Io2No_V(UnitX_)
    ySmooth = ySmooth * Io2No_V(UnitX_)
    dvx       = dvx       * Io2No_V(UnitU_)
    pertAmp   = pertAmp   * Io2No_V(UnitU_)
    pertWidth = pertWidth * Io2No_V(UnitX_)
    if(TypeSetup == '2layer')then
       UxLower = UxLower * Io2No_V(UnitU_)
       BzUpper = BzUpper * Io2No_V(UnitB_)
       PUpper  = PUpper  * Io2No_V(UnitP_)
    end if

    ! Seed the random number generator for reproducibility
    if(TypePerturbation == 'random')then
       call random_seed(size=SeedSize)
       allocate(Seed_I(SeedSize))
       do iSeed = 1, SeedSize
          Seed_I(iSeed) = int(SeedPert) + iProc*SeedSize + iSeed
       end do
       call random_seed(put=Seed_I)
       deallocate(Seed_I)
    end if

    Lx = xMaxBox - xMinBox

    ! Per-fluid density: split total density equally among all fluids
    ! (convert from I/O to normalized units)
    RhoFluidInner = rhoInner * Io2No_V(UnitRho_) / nFluid
    RhoFluidOuter = rhoOuter * Io2No_V(UnitRho_) / nFluid

    do iBlock = 1, nBlock
       if (Unused_B(iBlock)) CYCLE

       select case(TypeSetup)
       case('3layer')
          ! Three-layer symmetric slab (Athena-style)
          ! Shear interfaces at |y| = yShear, flow in x-direction
          do iFluid = 1, nFluid
             if(nFluid > 1) call select_fluid(iFluid)

             if(ySmooth > 0.0)then
                ! Smooth tanh transition at |y| = yShear
                State_VGB(iRho,:,:,:,iBlock) = &
                     (RhoFluidInner + RhoFluidOuter)/2.0 - &
                     (RhoFluidInner - RhoFluidOuter)/2.0 * &
                     tanh((abs(Xyz_DGB(y_,:,:,:,iBlock)) - yShear)/ySmooth)

                ! Velocity: +dvx inside, -dvx outside (momentum = rho * vx)
                State_VGB(iRhoUx,:,:,:,iBlock) = State_VGB(iRho,:,:,:,iBlock) * &
                     dvx * tanh((yShear - abs(Xyz_DGB(y_,:,:,:,iBlock)))/ySmooth)
             else
                ! Sharp jump at |y| = yShear
                where(abs(Xyz_DGB(y_,:,:,:,iBlock)) <= yShear)
                   State_VGB(iRho,:,:,:,iBlock) = RhoFluidInner
                   State_VGB(iRhoUx,:,:,:,iBlock) = RhoFluidInner * dvx
                elsewhere
                   State_VGB(iRho,:,:,:,iBlock) = RhoFluidOuter
                   State_VGB(iRhoUx,:,:,:,iBlock) = RhoFluidOuter * (-dvx)
                end where
             end if
          end do  ! iFluid

       case('2layer')
          ! Two-layer asymmetric setup (Venus-like ionosphere)
          ! Lower layer (y <= yShear): dense, slow, unmagnetized
          ! Upper layer (y > yShear):  less dense, fast, magnetized
          ! The lower layer state comes from #UNIFORMSTATE; the user module
          ! overwrites the upper layer and explicitly sets the lower layer.
          do iFluid = 1, nFluid
             if(nFluid > 1) call select_fluid(iFluid)

             if(ySmooth > 0.0)then
                ! Smooth tanh transition at y = yShear
                ! trans = 0 below yShear (lower), 1 above (upper)
                Trans = 0.0  ! unused, array form used below
                ! Read lower-layer pressure from state (uniform via UNIFORMSTATE)
                PLower = State_VGB(iP,1,1,1,iBlock)

                State_VGB(iRho,:,:,:,iBlock) = &
                     RhoFluidInner + (RhoFluidOuter - RhoFluidInner) * &
                     0.5*(1.0 + tanh((Xyz_DGB(y_,:,:,:,iBlock) - yShear)/ySmooth))

                State_VGB(iRhoUx,:,:,:,iBlock) = State_VGB(iRho,:,:,:,iBlock) * &
                     (UxLower + (dvx - UxLower) * &
                     0.5*(1.0 + tanh((Xyz_DGB(y_,:,:,:,iBlock) - yShear)/ySmooth)))

                State_VGB(iP,:,:,:,iBlock) = &
                     PLower + (PUpper/nFluid - PLower) * &
                     0.5*(1.0 + tanh((Xyz_DGB(y_,:,:,:,iBlock) - yShear)/ySmooth))
             else
                ! Sharp jump at y = yShear
                where(Xyz_DGB(y_,:,:,:,iBlock) > yShear)
                   ! Upper layer
                   State_VGB(iRho,:,:,:,iBlock)   = RhoFluidOuter
                   State_VGB(iRhoUx,:,:,:,iBlock) = RhoFluidOuter * dvx
                   State_VGB(iP,:,:,:,iBlock)     = PUpper / nFluid
                elsewhere
                   ! Lower layer (set explicitly, matching UNIFORMSTATE)
                   State_VGB(iRho,:,:,:,iBlock)   = RhoFluidInner
                   State_VGB(iRhoUx,:,:,:,iBlock) = RhoFluidInner * UxLower
                   ! Pressure unchanged from UNIFORMSTATE (lower layer value)
                end where
             end if
          end do  ! iFluid

          ! Set Bz for the 2-layer case (shared variable, not per-fluid)
          if(UseB)then
             if(ySmooth > 0.0)then
                State_VGB(Bz_,:,:,:,iBlock) = BzUpper * &
                     0.5*(1.0 + tanh((Xyz_DGB(y_,:,:,:,iBlock) - yShear)/ySmooth))
             else
                where(Xyz_DGB(y_,:,:,:,iBlock) > yShear)
                   State_VGB(Bz_,:,:,:,iBlock) = BzUpper
                elsewhere
                   State_VGB(Bz_,:,:,:,iBlock) = 0.0
                end where
             end if
          end if

       case default
          if(iProc==0) call stop_mpi( &
               NameSub//': unknown TypeSetup='//trim(TypeSetup))
       end select

       ! Apply perturbation to all fluids (same for both setups)
       select case(TypePerturbation)
       case('random')
          ! Athena-style: random noise on both vx and vy
          ! peak-to-peak amplitude = pertAmp, so range = [-pertAmp/2, +pertAmp/2]
          ! The same velocity perturbation is applied to all fluids.
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             call random_number(RandomVal)
             PertUx = (RandomVal - 0.5) * pertAmp
             call random_number(RandomVal)
             PertUy = (RandomVal - 0.5) * pertAmp

             do iFluid = 1, nFluid
                if(nFluid > 1) call select_fluid(iFluid)
                State_VGB(iRhoUx,i,j,k,iBlock) = State_VGB(iRhoUx,i,j,k,iBlock) &
                     + State_VGB(iRho,i,j,k,iBlock) * PertUx
                State_VGB(iRhoUy,i,j,k,iBlock) = State_VGB(iRho,i,j,k,iBlock) &
                     * PertUy
             end do
          end do; end do; end do

       case('sinusoidal')
          ! Single-mode sinusoidal perturbation in v_y only
          ! delta_vy = pertAmp * sin(2*pi*nWaveX*x/Lx)
          ! When pertWidth > 0, a Gaussian envelope exp(-((y-yShear)/pertWidth)^2)
          ! localizes the perturbation near the shear layer (recommended for
          ! setups with outflow boundaries to avoid spurious boundary effects).
          do iFluid = 1, nFluid
             if(nFluid > 1) call select_fluid(iFluid)
             if(pertWidth > 0.0)then
                State_VGB(iRhoUy,:,:,:,iBlock) = State_VGB(iRho,:,:,:,iBlock) &
                     * pertAmp * sin(cTwoPi * nWaveX * Xyz_DGB(x_,:,:,:,iBlock) / Lx) &
                     * exp( -((Xyz_DGB(y_,:,:,:,iBlock) - yShear)/pertWidth)**2 )
             else
                State_VGB(iRhoUy,:,:,:,iBlock) = State_VGB(iRho,:,:,:,iBlock) * &
                     pertAmp * sin(cTwoPi * nWaveX * Xyz_DGB(x_,:,:,:,iBlock) / Lx)
             end if
          end do

       case default
          if(iProc==0) call stop_mpi( &
               NameSub//': unknown TypePerturbation='//trim(TypePerturbation))
       end select

    end do

    call test_stop(NameSub, DoTest)
  end subroutine user_initial_perturbation
  !============================================================================

end module ModUser
!==============================================================================
