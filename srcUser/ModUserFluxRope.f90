!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModUser

  use BATL_lib, ONLY: &
       test_start, test_stop, iProc

  use ModUserEmpty,               &
       IMPLEMENTED1 => user_read_inputs,                &
       IMPLEMENTED2 => user_set_ics

  include 'user_module.h' ! list of public methods

  ! Here you must define a user routine Version number and a
  ! descriptive string.
  character (len=*), parameter :: NameUserFile = "ModUserFluxRope.f90"
  character (len=*), parameter :: NameUserModule = 'Flux rope model'

  integer :: nFluxRopes = 0
  real, allocatable :: Center_II(:,:)
  real, allocatable :: Radius_I(:), Amplitude_I(:)

  ! Parameters for island coalescence problem
  logical :: UseIsland = .false.
  real :: B0, Pert, Ti, Te, Rho0, Rhob
  real :: Lambda, Epsilon

contains
  !============================================================================
  subroutine user_read_inputs

    use ModReadParam

    integer :: iFluxRope

    character(len=100) :: NameCommand
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_read_inputs'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    do
       if(.not.read_line() ) EXIT
       if(.not.read_command(NameCommand)) CYCLE
       select case(NameCommand)
       case('#FLUXROPES')
          call read_var('nFluxRopes', nFluxRopes)
          if(nFluxRopes > 0) then
             if(allocated(Center_II)) &
                  deallocate(Center_II, Radius_I, Amplitude_I)
             allocate(Center_II(2, nFluxRopes))
             allocate(Radius_I(nFluxRopes))
             allocate(Amplitude_I(nFluxRopes))
          endif
          do iFluxRope = 1, nFluxRopes
             call read_var('xCenter', Center_II(1,iFluxRope))
             call read_var('yCenter', Center_II(2,iFluxRope))
             call read_var('Radius' , Radius_I(iFluxRope))
             call read_var('Amplitude', Amplitude_I(iFluxRope))
          enddo

       case('#ISLANDCOALESCENCE')
          call read_var('UseIsland', UseIsland)
          if(UseIsland) then
             call read_var('Lambda', Lambda)
             call read_var('Epsilon', Epsilon)
             call read_var('Rho0',Rho0)
             call read_var('Rhob',Rhob)
             call read_var('Ti',Ti)
             call read_var('Te',Te)
             call read_var('B0', B0)
             call read_var('Pert',Pert)
          endif

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
  subroutine user_set_ics(iBlock)

    integer, intent(in) :: iBlock
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_ics'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(nFluxRopes>0) call user_set_ics_flux_rope(iBlock)

    if(UseIsland) call user_set_ics_island_coalescence(iBlock)

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_ics
  !============================================================================
  subroutine user_set_ics_island_coalescence(iBlock)
    use ModGeometry, ONLY: Xyz_DGB
    use ModNumConst, ONLY: cPi, cTwoPi
    use ModAdvance, ONLY: State_VGB, Rho_, Bx_, By_,  p_, Pe_
    use ModSize, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK, nI, nJ, nK

    integer, intent(in) :: iBlock

    integer :: i, j, k
    real :: x1, y1
    real :: Sinhy, Coshy, Cosx, Sinx
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_ics_island_coalescence'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! The B field initialization follows the paper of
    ! Jonathan+(DOI:10.1063/1.4935302):

    ! Az = -Lambada*B0*ln[cosh(y/Lambda) + Epsilon*cos(x/Lambda)]
    ! Bx = B0*sinh(y/Lambda)/[cosh(y/Lambda) + Epsilon*cos(x/Lambda)]
    ! By = B0*Epsilon*sin(x/Lambda)/[cosh(y/Lambda) + Epsilon*cos(x/Lambda)]
    ! n = n0*(1-Epsilon^2)/[cosh(y/Lambda) + Epsilon*cos(x/Lambda)]^2 + nb

    ! Assume Ti = Te = T, and 2*n0*T = B0^2, then grad(P_total) = J x B.

    ! The perturbation is adopted from Daughton+ (DOI:10.1063/1.3191718)

    do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
       x1 = Xyz_DGB(x_,i,j,k,iBlock)/Lambda
       y1 = Xyz_DGB(y_,i,j,k,iBlock)/Lambda
       Sinhy = sinh(y1)
       Coshy = cosh(y1)
       Sinx = sin(x1)
       Cosx = cos(x1)

       State_VGB(:,i,j,k,iBlock) = 0

       State_VGB(Rho_,i,j,k,iBlock) = Rhob + &
            Rho0*(1-Epsilon**2)/(Coshy + Epsilon*Cosx)**2

       State_VGB(p_,i,j,k,iBlock) = State_VGB(Rho_,i,j,k,iBlock)*Te

       State_VGB(Pe_,i,j,k,iBlock) = State_VGB(Rho_,i,j,k,iBlock)*Te

       State_VGB(Bx_,i,j,k,iBlock) = B0*Sinhy/(Coshy + Epsilon*Cosx)

       State_VGB(By_,i,j,k,iBlock) = B0*Epsilon*Sinx/(Coshy + Epsilon*Cosx)

       ! Assume Lx/Ly = 2
       State_VGB(Bx_,i,j,k,iBlock) = State_VGB(Bx_,i,j,k,iBlock) + &
            -B0*Pert*cos(0.5*x1-cPi)*sin(0.5*y1)

       State_VGB(By_,i,j,k,iBlock) = State_VGB(By_,i,j,k,iBlock) + &
            B0*Pert*sin(0.5*x1-cPi)*cos(0.5*y1)

    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)

  end subroutine user_set_ics_island_coalescence
  !============================================================================
  subroutine user_set_ics_flux_rope(iBlock)
    use ModGeometry, ONLY: Xyz_DGB
    use ModAdvance, ONLY: State_VGB, Bx_, By_, Bz_
    use ModSize, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK, nI, nJ, nK

    integer, intent(in) :: iBlock

    integer :: iFluxRope, i, j, k
    real :: x, y, r, Phi
    real :: InvRadius, InvRadius2, InvRadius4, InvRadius6, InvRadius8, Radius2
    real :: BPhi, Bz0
    real :: Ampl

    real, parameter :: c0 = 47./360, c1 = 0.5, c2 = 0.75, c3 = 5./9, &
         c4 = 5./24, c5 = 1./30, c6 = 1./6
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_ics_flux_rope'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    do iFluxRope = 1, nFluxRopes
       ! The force free flux rope model is adpoted from
       ! Stanier et al. PoP (2013).
       Radius2    = Radius_I(iFluxRope)**2
       InvRadius  = 1.0/Radius_I(iFluxRope)
       InvRadius2 = InvRadius**2
       InvRadius4 = InvRadius2**2
       InvRadius6 = InvRadius2*InvRadius4
       InvRadius8 = InvRadius4**2

       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          x = Xyz_DGB(x_,i,j,k,iBlock)
          y = Xyz_DGB(y_,i,j,k,iBlock)
          Phi = atan2(y - Center_II(y_, iFluxRope), &
               x - Center_II(x_, iFluxRope))
          r = sqrt( &
               (x - Center_II(x_, iFluxRope) )**2 +  &
               (y - Center_II(y_, iFluxRope) )**2 )
          Ampl = Amplitude_I(iFluxRope)
          if(r < Radius_I(iFluxRope)) then
             Bz0 = State_VGB(Bz_,i,j,k,iBlock)
             ! See Stanier(2013) for the axial current, which
             ! is 'B_T' in that paper.
             State_VGB(Bz_,i,j,k,iBlock) = Ampl*sqrt( (Bz0/Ampl)**2 + &
                  c0*Radius2 - c1*r**2 + c2*InvRadius2*r**4 - &
                  c3*InvRadius4*r**6   + c4*InvRadius6*r**8 - &
                  c5*InvRadius8*r**10 )

             ! B_phi = Ampl*(r/2 + r**5/(6*w**4) - r**3/(2*w**2)) when r <= w
             BPhi = Ampl*( &
                  0.5*r + c6*InvRadius4*r**5 - 0.5*InvRadius2*r**3)
          else
             ! B_phi = B_phi(r=w)*w/r when r > w
             BPhi = Ampl*c6*Radius2/r
          endif

          State_VGB(Bx_,i,j,k,iBlock) = &
               State_VGB(Bx_,i,j,k,iBlock) - BPhi*sin(Phi)
          State_VGB(By_,i,j,k,iBlock) = &
               State_VGB(By_,i,j,k,iBlock) + BPhi*cos(Phi)
       enddo; enddo; enddo
    enddo

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_ics_flux_rope
  !============================================================================
end module ModUser
!==============================================================================
