!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModUser

  use BATL_lib, ONLY: &
       test_start, test_stop, iProc

  use ModUserEmpty,               &
       IMPLEMENTED1 => user_read_inputs,                &
       IMPLEMENTED2 => user_init_session,               &
       IMPLEMENTED3 => user_set_ics,                    &
       IMPLEMENTED4 => user_amr_criteria

  include 'user_module.h'

  character (len=*), parameter :: NameUserFile = "ModUserWaveReflection.f90"
  character (len=*), parameter :: NameUserModule = &
       'WAVE REFLECTION, Gamma. Toth'

  logical :: IsSmooth = .false.
  real :: xLeft0 = 25.0, xRight0 = 30.0, cSoundX0 = 1.0
  real :: pPerturb = 1.1, bPerturb = 0.0
  real :: DistanceMin = 1.0
  real :: xLeft, xRight, cSoundX, Ux, CosSlope, SinSlope

contains
  !============================================================================
  subroutine user_read_inputs
    use ModMain
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
       case("#DISTANCE")
          call read_var('DistanceMin',DistanceMin)
       case("#PERTURBATION")
          call read_var('xLeft0'  ,xLeft0)
          call read_var('xRight0' ,xRight0)
          call read_var('pPerturb',pPerturb)
          call read_var('bPerturb',bPerturb)
          call read_var('IsSmooth',IsSmooth)
       case('#USERINPUTEND')
          EXIT
       case default
          if(iProc==0) call stop_mpi( &
               'user_read_inputs: unrecognized command: '//NameCommand)
       end select
    end do
    call test_stop(NameSub, DoTest)
  end subroutine user_read_inputs
  !============================================================================

  subroutine user_init_session

    use ModAdvance, ONLY: Ux_
    use ModPhysics, ONLY: ShockSlope, ShockLeft_V
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_init_session'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    ! Calculate and store the cos and sin of the shock slope
    CosSlope = cos(atan(ShockSlope))
    SinSlope = sin(atan(ShockSlope))

    ! Rotate pressure perturbation parameters
    xLeft   = xLeft0 /CosSlope
    xRight  = xRight0/CosSlope

    ! Project the sound and bulk speeds to the X axis
    cSoundX = cSoundX0/CosSlope
    Ux      = ShockLeft_V(Ux_)/CosSlope

    if(iProc==0)then
       write(*,*)'user_init_session: ShockSlope  =',ShockSlope
       write(*,*)'user_init_session: xLeft,xRight=',xLeft,xRight
       write(*,*)'user_init_session: cSoundX,  Ux=',cSoundX,Ux
    endif

    call test_stop(NameSub, DoTest)
  end subroutine user_init_session
  !============================================================================

  subroutine user_set_ics(iBlock)

    use ModMain, ONLY: nI, nJ, nK, nBlock
    use ModGeometry, ONLY: Xyz_DGB, CellSize_DB
    use ModAdvance, ONLY: State_VGB, &
         Rho_, RhoUx_, RhoUy_, RhoUz_, Ux_, Uy_, Uz_, P_, Bx_, By_, Bz_
    use ModPhysics, ONLY: ShockSlope, ShockLeft_V, ShockRight_V, Gamma
    use ModNumConst, ONLY: cPi

    integer, intent(in) :: iBlock

    real :: Potential_G(MinI:MaxI,MinJ:MaxJ)
    real :: xRot_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)
    real :: Perturb_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)
    real :: pTotal
    integer :: i, j, k
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_ics'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! Redo rotated state so that div B = 0 if necessary
    if(ShockSlope /= 0.0 .and. .not. IsSmooth)then
       ! Store total pressure
       pTotal = State_VGB(P_,1,1,1,iBlock) + &
            0.5*(State_VGB(Bx_,1,1,1,iBlock)**2 &
            +    State_VGB(By_,1,1,1,iBlock)**2 )

       ! Calculate rotated magnetic field from rotated vector potential
       ! Original vector potential: A_z = Bx*y - By*x (both for x<0 and x>0)
       ! Rotated  vector potential: A_z = Bx*(Cos*y-Sin*x)-By*(Cos*x+Sin*y)

       Potential_G = &
            ShockLeft_V(Bx_)* &
            (CosSlope*Xyz_DGB(Y_,:,:,1,iBlock)-SinSlope*Xyz_DGB(x_,:,:,1,iBlock)) &
            -ShockLeft_V(By_)*min(0., &
            CosSlope*Xyz_DGB(x_,:,:,1,iBlock) + SinSlope*Xyz_DGB(Y_,:,:,1,iBlock)) &
            -ShockRight_V(By_)*max(0., &
            CosSlope*Xyz_DGB(x_,:,:,1,iBlock) + SinSlope*Xyz_DGB(Y_,:,:,1,iBlock))

       ! B = curl A so Bx = dA_z/dy and By = -dAz/dx
       do j=1,nJ; do i=1,nI
          State_VGB(Bx_,i,j,:,iBlock) = &
               +(Potential_G(i,j+1)-Potential_G(i,j-1)) / (2*CellSize_DB(y_,iBlock))
          State_VGB(By_,i,j,:,iBlock) = &
               -(Potential_G(i+1,j)-Potential_G(i-1,j)) / (2*CellSize_DB(x_,iBlock))
       end do; end do

       ! Recalculate pressure
       State_VGB(P_,1:nI,1:nJ,1:nK,iBlock) = pTotal - &
            0.5*(State_VGB(Bx_,1:nI,1:nJ,1:nK,iBlock)**2 &
            +    State_VGB(By_,1:nI,1:nJ,1:nK,iBlock)**2 )

       ! Recalculate momentum and density
       do k=1,nK; do j=1,nJ; do i=1,nI
          State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) = &
               State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) * &
               Gamma * State_VGB(P_,i,j,k,iBlock) / State_VGB(Rho_,i,j,k,iBlock)

          State_VGB(Rho_,i,j,k,iBlock) = Gamma * State_VGB(P_,i,j,k,iBlock)
       end do; end do; end do
    end if

    ! Apply perturbation
    xRot_G = Xyz_DGB(x_,:,:,:,iBlock)+ShockSlope*Xyz_DGB(Y_,:,:,:,iBlock)

    if(IsSmooth)then
       ! Smooth perturbation with sin^2 profile starting at xRot = xLeft
       Perturb_G = sin( cPi*(xRot_G - xLeft) / (xRight-xLeft))**2

       where(xRot_G  >= xLeft .and. xRot_G <= xRight)

          ! Perturb By, Bz and P with a smooth perturbation
          State_VGB(By_,:,:,:,iBlock) = State_VGB(By_,:,:,:,iBlock) &
               + bPerturb * Perturb_G

          State_VGB(Bz_,:,:,:,iBlock) = State_VGB(Bz_,:,:,:,iBlock) &
               + bPerturb * Perturb_G

          State_VGB(P_,:,:,:,iBlock) = State_VGB(P_,:,:,:,iBlock) &
               + pPerturb * Perturb_G

          ! Compress density adiabatically. Keep velocities constant.
          State_VGB(Ux_,:,:,:,iBlock)= &
                  State_VGB(RhoUx_,:,:,:,iBlock)/State_VGB(Rho_,:,:,:,iBlock)
          State_VGB(Uy_,:,:,:,iBlock)= &
                  State_VGB(RhoUy_,:,:,:,iBlock)/State_VGB(Rho_,:,:,:,iBlock)
          State_VGB(Uz_,:,:,:,iBlock)= &
                  State_VGB(RhoUz_,:,:,:,iBlock)/State_VGB(Rho_,:,:,:,iBlock)

          State_VGB(Rho_,:,:,:,iBlock)=ShockRight_V(Rho_)* &
                  (State_VGB(P_,:,:,:,iBlock)/ShockRight_V(p_))**(1./Gamma)

          State_VGB(RhoUx_,:,:,:,iBlock)= &
               State_VGB(Ux_,:,:,:,iBlock)*State_VGB(Rho_,:,:,:,iBlock)
          State_VGB(RhoUy_,:,:,:,iBlock)= &
               State_VGB(Uy_,:,:,:,iBlock)*State_VGB(Rho_,:,:,:,iBlock)
          State_VGB(RhoUz_,:,:,:,iBlock)= &
               State_VGB(Uz_,:,:,:,iBlock)*State_VGB(Rho_,:,:,:,iBlock)
       end where
    else
       ! Apply a discontinuous perturbation to pressure
       where(xRot_G  >= xLeft .and. xRot_G <= xRight)
          State_VGB(P_,:,:,:,iBlock) = pPerturb*State_VGB(P_,:,:,:,iBlock)
       end where
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_ics
  !============================================================================

  subroutine user_amr_criteria(iBlock, UserCriteria, TypeCriteria, IsFound)

    use ModSize, ONLY: nI, nJ, nK
    use ModGeometry, ONLY: Xyz_DGB, CellSize_DB
    use ModPhysics, ONLY: ShockSlope
    use ModMain, ONLY: tSimulation

    ! Variables required by this user subroutine
    character (len=*),intent(in) :: TypeCriteria
    integer, intent(in)          :: iBlock
    real, intent(out)            :: UserCriteria
    logical ,intent(inout)       :: IsFound

    real :: xCenter, yCenter, xShifted, x_I(5), Distance
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_amr_criteria'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    xCenter = 0.5*(Xyz_DGB(x_,nI,nJ,nK,iBlock)+Xyz_DGB(x_,1,1,1,iBlock))
    yCenter = 0.5*(Xyz_DGB(y_,nI,nJ,nK,iBlock)+Xyz_DGB(y_,1,1,1,iBlock))
    xShifted = xCenter + ShockSlope*yCenter

    ! write(*,*)'xLeft,xRight,Ux,cSoundX=',xLeft,xRight,Ux,cSoundX
    ! call stop_mpi('Debug')

    ! Location of sound wave edges and the tangential discontinuity
    x_I(1) = xLeft  + (Ux-cSoundX)*tSimulation
    x_I(2) = xLeft  + (Ux+cSoundx)*tSimulation
    x_I(3) = xRight + (Ux-cSoundX)*tSimulation
    x_I(4) = xRight + (Ux+cSoundX)*tSimulation
    x_I(5) =           Ux         *tSimulation

    ! Reflect left going sound wave edges
    if(x_I(1) < x_I(5)) x_I(1) = 2*x_I(5) - x_I(1)
    if(x_I(3) < x_I(5)) x_I(3) = 2*x_I(5) - x_I(3)

    Distance = minval(abs(x_I - xShifted))

    if(Distance <= nI/2*CellSize_DB(x_,iBlock) + DistanceMin)then
       UserCriteria = 1.0
    else
       UserCriteria = 0.0
    endif

    IsFound = .true.

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_amr_criteria
  !============================================================================

end module ModUser
!==============================================================================
