!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModUpdateStateGpu

  use ModVarIndexes
  use ModFaceFlux, ONLY: print_face_values
  use ModMain, ONLY: iStage, Cfl, Dt
  use ModAdvance, ONLY: nFlux, State_VGB, Energy_GBI
!!!  time_BLK, StateOld_VGB, State_VGB, EnergyOld_CBI, Energy_GBI
  use BATL_lib, ONLY: nDim, nI, nJ, nK, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, &
       nBlock, Unused_B, x_, y_, z_, CellVolume_B, CellFace_DB, &
       test_start, test_stop, iTest, jTest, kTest, iBlockTest
  use ModPhysics, ONLY: Gamma, InvGammaMinus1, GammaMinus1_I
  use ModMain, ONLY: SpeedHyp

  implicit none

  private ! except

  public:: update_state_gpu
  public:: update_state_gpu_v1
  
  logical:: DoTestCell= .false.

contains
  !============================================================================
  subroutine update_state_gpu

    integer:: i, j, k, iBlock

    real:: DtPerDv, Change_V(nFlux)
    real:: Flux_VX(nFlux,nI+1,nJ,nK)
    real:: Flux_VY(nFlux,nI,nJ+1,nK)
    real:: Flux_VZ(nFlux,nI,nJ,nK+1)

    !$acc declare create (DtPerDv, Change_V, Flux_VX, Flux_VY, Flux_VZ)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'update_state_gpu'
    !--------------------------------------------------------------------------
#ifndef OPENACC
    call test_start(NameSub, DoTest)
#endif

    !$acc parallel
    !$acc loop gang private(Flux_VX, Flux_VY, Flux_VZ) independent
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE

       !$acc loop vector collapse(3) private(Change_V) independent
       do k = 1, nK; do j = 1, nJ; do i = 1, nI+1
#ifndef OPENACC
          DoTestCell = DoTest .and. (i == iTest .or. i == iTest+1) .and. &
               j == jTest .and. k == kTest .and. iBlock == iBlockTest
#endif
          call get_flux_x(i, j, k, iBlock, Flux_VX(:,i,j,k))

       end do; end do; end do

       if(nDim > 1)then
          !$acc loop vector collapse(3) private(Change_V) independent
          do k = 1, nK; do j = 1, nJ+1; do i = 1, nI
#ifndef OPENACC
             DoTestCell = DoTest .and. i == iTest .and. &
                  (j == jTest .or. j==jTest+1) .and. k == kTest .and. iBlock == iBlockTest
#endif
             call get_flux_y(i, j, k, iBlock, Flux_VY(:,i,j,k))
             
          end do; end do; end do
       end if
       
       if(nDim > 2)then
          !$acc loop vector collapse(3) private(Change_V) independent
          do k = 1, nK+1; do j = 1, nJ; do i = 1, nI
#ifndef OPENACC
             DoTestCell = DoTest .and. i == iTest .and. &
                  j == jTest .and. (k == kTest .or. k == kTest+1) .and. iBlock == iBlockTest
#endif
             call get_flux_z(i, j, k, iBlock, Flux_VZ(:,i,j,k))
             
          end do; end do; end do
       end if

       !$acc loop vector collapse(3) private(DtPerDv, Change_V) independent
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          
          Change_V              =            Flux_VX(:,i,j,k) - Flux_VX(:,i+1,j,k)
          if(nDim > 1) Change_V = Change_V + Flux_VY(:,i,j,k) - Flux_VY(:,i,j+1,k)
          if(nDim > 2) Change_V = Change_V + Flux_VZ(:,i,j,k) - Flux_VZ(:,i,j,k+1) 

          ! Time step divided by cell volume
!!!       DtPerDv = Cfl*time_BLK(i,j,k,iBlock)/CellVolume_GB(i,j,k,iBlock)
          DtPerDv = Dt/CellVolume_B(iBlock)
          
          ! Update state
          State_VGB(:,i,j,k,iBlock) = State_VGB(:,i,j,k,iBlock) &
               + DtPerDv*Change_V(1:nVar)
          Energy_GBI(i,j,k,iBlock,1) = Energy_GBI(i,j,k,iBlock,1) &
               + DtPerDv*Change_V(nVar+1)

          ! Calculate pressure from energy
          State_VGB(p_,i,j,k,iBlock) = &
               GammaMinus1_I(1)*( Energy_GBI(i,j,k,iBlock,1) - &
               0.5*( &
               ( State_VGB(RhoUx_,i,j,k,iBlock)**2 &
               + State_VGB(RhoUy_,i,j,k,iBlock)**2 &
               + State_VGB(RhoUz_,i,j,k,iBlock)**2 &
               )/State_VGB(Rho_,i,j,k,iBlock)      &
               + State_VGB(Bx_,i,j,k,iBlock)**2    &
               + State_VGB(By_,i,j,k,iBlock)**2    &
               + State_VGB(Bz_,i,j,k,iBlock)**2    &
               )          )

#ifndef OPENACC
          DoTestCell = DoTest .and. i==iTest .and. j==jTest .and. k==kTest &
               .and. iBlock == iBlockTest
          if(DoTestCell)then
             write(*,*)'State_VGB =', State_VGB(:,i,j,k,iBlock)
             write(*,*)'Energy_GBI=', Energy_GBI(i,j,k,iBlock,:)
             write(*,*)'Change_V  =', Change_V
             write(*,*)'DtPerDv   =', DtPerDv
          end if
#endif

       enddo; enddo; enddo

    end do
    !$acc end parallel

#ifndef OPENACC
    call test_stop(NameSub, DoTest, iBlock)
#endif

  end subroutine update_state_gpu
  !=======================================
  subroutine get_flux_x(i, j,  k, iBlock, Flux_V)
    !$acc routine seq
    
    integer, intent(in):: i, j, k, iBlock
    real,    intent(out):: Flux_V(nFlux)

    real :: Area, NormalX, NormalY, NormalZ, Un, Cmax
    real :: StateLeft_V(nVar), StateRight_V(nVar), State_V(nVar)
    real :: StateLeftCons_V(nFlux), StateRightCons_V(nFlux)
    real :: FluxLeft_V(nFlux), FluxRight_V(nFlux)
    !--------------------------------------
    ! if(IsCartesianGrid)then
    Area = CellFace_DB(x_,iBlock)
    NormalX = 1.0
    NormalY = 0.0
    NormalZ = 0.0
    ! end if

    ! First order left state of primitive variables
    StateLeft_V  = State_VGB(:,i-1,j,k,iBlock)
    StateLeft_V(Ux_:Uz_) = StateLeft_V(Ux_:Uz_)/StateLeft_V(Rho_)

    ! First order right state of primitive variables
    StateRight_V  = State_VGB(:,i,j,k,iBlock)
    StateRight_V(Ux_:Uz_) = StateRight_V(Ux_:Uz_)/StateRight_V(Rho_)

    ! average state
    State_V = 0.5*(StateLeft_V + StateRight_V)

    call get_speed_max(State_V, NormalX, NormalY, NormalZ, &
         Un, Cmax)
    call get_physical_flux(StateLeft_V, NormalX, NormalY, NormalZ, &
         StateLeftCons_V, FluxLeft_V)
    call get_physical_flux(StateRight_V, NormalX, NormalY, NormalZ, &
         StateRightCons_V, FluxRight_V)

    ! Rusanov flux
    Flux_V = Area*0.5*((FluxLeft_V + FluxRight_V) &
         +             Cmax*(StateLeftCons_V - StateRightCons_V))

  end subroutine get_flux_x
  !=======================================
  subroutine get_flux_y(i, j, k, iBlock, Flux_V)
    !$acc routine seq
    
    integer, intent(in):: i, j, k, iBlock
    real,    intent(out):: Flux_V(nFlux)

    real :: Area, NormalX, NormalY, NormalZ, Un, Cmax
    real :: StateLeft_V(nVar), StateRight_V(nVar), State_V(nVar)
    real :: StateLeftCons_V(nFlux), StateRightCons_V(nFlux)
    real :: FluxLeft_V(nFlux), FluxRight_V(nFlux)
    !--------------------------------------
    ! if(IsCartesianGrid)then
    Area = CellFace_DB(y_,iBlock)
    NormalX = 0.0
    NormalY = 1.0
    NormalZ = 0.0
    ! end if

    ! First order left state of primitive variables
    StateLeft_V  = State_VGB(:,i,j-1,k,iBlock)
    StateLeft_V(Ux_:Uz_) = StateLeft_V(Ux_:Uz_)/StateLeft_V(Rho_)

    ! First order right state of primitive variables
    StateRight_V  = State_VGB(:,i,j,k,iBlock)
    StateRight_V(Ux_:Uz_) = StateRight_V(Ux_:Uz_)/StateRight_V(Rho_)

    ! average state
    State_V = 0.5*(StateLeft_V + StateRight_V)

    call get_speed_max(State_V, NormalX, NormalY, NormalZ, &
         Un, Cmax)
    call get_physical_flux(StateLeft_V, NormalX, NormalY, NormalZ, &
         StateLeftCons_V, FluxLeft_V)
    call get_physical_flux(StateRight_V, NormalX, NormalY, NormalZ, &
         StateRightCons_V, FluxRight_V)

    ! Rusanov flux
    Flux_V = Area*0.5*((FluxLeft_V + FluxRight_V) &
         +             Cmax*(StateLeftCons_V - StateRightCons_V))

  end subroutine get_flux_y
  !=======================================
  subroutine get_flux_z(i, j, k, iBlock, Flux_V)
    !$acc routine seq
    
    integer, intent(in):: i, j, k, iBlock
    real,    intent(out):: Flux_V(nFlux)

    real :: Area, NormalX, NormalY, NormalZ, Un, Cmax
    real :: StateLeft_V(nVar), StateRight_V(nVar), State_V(nVar)
    real :: StateLeftCons_V(nFlux), StateRightCons_V(nFlux)
    real :: FluxLeft_V(nFlux), FluxRight_V(nFlux)
    !--------------------------------------
    ! if(IsCartesianGrid)then
    Area = CellFace_DB(z_,iBlock)
    NormalX = 0.0
    NormalY = 0.0
    NormalZ = 1.0
    ! end if

    ! First order left state of primitive variables
    StateLeft_V  = State_VGB(:,i,j,k-1,iBlock)
    StateLeft_V(Ux_:Uz_) = StateLeft_V(Ux_:Uz_)/StateLeft_V(Rho_)

    ! First order right state of primitive variables
    StateRight_V  = State_VGB(:,i,j,k,iBlock)
    StateRight_V(Ux_:Uz_) = StateRight_V(Ux_:Uz_)/StateRight_V(Rho_)

    ! average state
    State_V = 0.5*(StateLeft_V + StateRight_V)

    call get_speed_max(State_V, NormalX, NormalY, NormalZ, &
         Un, Cmax)
    call get_physical_flux(StateLeft_V, NormalX, NormalY, NormalZ, &
         StateLeftCons_V, FluxLeft_V)
    call get_physical_flux(StateRight_V, NormalX, NormalY, NormalZ, &
         StateRightCons_V, FluxRight_V)

    ! Rusanov flux
    Flux_V = Area*0.5*((FluxLeft_V + FluxRight_V) &
         +             Cmax*(StateLeftCons_V - StateRightCons_V))

  end subroutine get_flux_z
  !============================================================================
  subroutine update_state_gpu_v1

    integer:: i, j, k, iBlock

    real:: Change_V(nFlux), Change_VC(nFlux,nI,nJ,nK), DtPerDv
    !$acc declare create (Change_V, Change_VC, DtPerDv)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'update_state_gpu'
    !--------------------------------------------------------------------------
#ifndef OPENACC
    call test_start(NameSub, DoTest)
#endif

    !$acc parallel
    !$acc loop gang private(Change_VC) independent
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE

       !$acc loop vector collapse(3) private(Change_V) independent
       do k = 1, nK; do j = 1, nJ; do i = 1, nI

#ifndef OPENACC
          DoTestCell = DoTest .and. i==iTest .and. j==jTest .and. k==kTest &
               .and. iBlock == iBlockTest
#endif

          ! Initialize change in State_VGB
          Change_V = 0.0

          call do_face(1, i, j, k, iBlock, Change_V)
          call do_face(2, i, j, k, iBlock, Change_V)

          if(nDim > 1)then
             call do_face(3, i, j, k, iBlock, Change_V)
             call do_face(4, i, j, k, iBlock, Change_V)
          end if

          if(nDim > 2)then
             call do_face(5, i, j, k, iBlock, Change_V)
             call do_face(6, i, j, k, iBlock, Change_V)
          end if

          Change_VC(:,i,j,k) = Change_V

       enddo; enddo; enddo

       !$acc loop vector collapse(3) private(DtPerDv) independent
       do k = 1, nK; do j = 1, nJ; do i = 1, nI

          ! Time step divided by cell volume
!!!       DtPerDv = Cfl*time_BLK(i,j,k,iBlock)/CellVolume_GB(i,j,k,iBlock)
          DtPerDv = Dt/CellVolume_B(iBlock)

          ! Update state
          State_VGB(:,i,j,k,iBlock) = State_VGB(:,i,j,k,iBlock) &
               + DtPerDv*Change_VC(1:nVar,i,j,k)
          Energy_GBI(i,j,k,iBlock,1) = Energy_GBI(i,j,k,iBlock,1) &
               + DtPerDv*Change_VC(nVar+1,i,j,k)

          ! Calculate pressure from energy
          State_VGB(p_,i,j,k,iBlock) = &
               GammaMinus1_I(1)*( Energy_GBI(i,j,k,iBlock,1) - &
               0.5*( &
               ( State_VGB(RhoUx_,i,j,k,iBlock)**2 &
               + State_VGB(RhoUy_,i,j,k,iBlock)**2 &
               + State_VGB(RhoUz_,i,j,k,iBlock)**2 &
               )/State_VGB(Rho_,i,j,k,iBlock)      &
               + State_VGB(Bx_,i,j,k,iBlock)**2    &
               + State_VGB(By_,i,j,k,iBlock)**2    &
               + State_VGB(Bz_,i,j,k,iBlock)**2    &
               )          )

#ifndef OPENACC
          DoTestCell = DoTest .and. i==iTest .and. j==jTest .and. k==kTest &
               .and. iBlock == iBlockTest
          if(DoTestCell)then
             write(*,*)'State_VGB =', State_VGB(:,i,j,k,iBlock)
             write(*,*)'Energy_GBI=', Energy_GBI(i,j,k,iBlock,:)
             write(*,*)'Change_V  =', Change_VC(:,i,j,k)
             write(*,*)'DtPerDv   =', DtPerDv
          end if
#endif

       enddo; enddo; enddo

    end do
    !$acc end parallel

#ifndef OPENACC
    call test_stop(NameSub, DoTest, iBlock)
#endif

  end subroutine update_state_gpu_v1
  !============================================================================
  subroutine do_face(iFace, i, j, k, iBlock, Change_V)
    !$acc routine seq

    integer, intent(in):: iFace, i, j, k, iBlock
    real, intent(inout):: Change_V(nFlux)

    real :: Area, NormalX, NormalY, NormalZ, Un, Cmax
    real :: StateLeft_V(nVar), StateRight_V(nVar), State_V(nVar)
    real :: StateLeftCons_V(nFlux), StateRightCons_V(nFlux)
    real :: FluxLeft_V(nFlux), FluxRight_V(nFlux), Flux_V(nFlux)
    !--------------------------------------------------------------------------
    select case(iFace)
    case(1)
       Area = CellFace_DB(x_,iBlock)
       NormalX = 1.0
       NormalY = 0.0
       NormalZ = 0.0
       StateLeft_V           = State_VGB(:,i-1,j,k,iBlock)
       StateLeft_V(Ux_:Uz_)  = StateLeft_V(Ux_:Uz_)/StateLeft_V(Rho_)
       StateRight_V          = State_VGB(:,i,j,k,iBlock)
       StateRight_V(Ux_:Uz_) = StateRight_V(Ux_:Uz_)/StateRight_V(Rho_)
    case(2)
       Area = -CellFace_DB(x_,iBlock)
       NormalX = 1.0
       NormalY = 0.0
       NormalZ = 0.0
       StateLeft_V           = State_VGB(:,i,j,k,iBlock)
       StateLeft_V(Ux_:Uz_)  = StateLeft_V(Ux_:Uz_)/StateLeft_V(Rho_)
       StateRight_V          = State_VGB(:,i+1,j,k,iBlock)
       StateRight_V(Ux_:Uz_) = StateRight_V(Ux_:Uz_)/StateRight_V(Rho_)
    case(3)
       Area = CellFace_DB(y_,iBlock)
       NormalX = 0.0
       NormalY = 1.0
       NormalZ = 0.0
       StateLeft_V           = State_VGB(:,i,j-1,k,iBlock)
       StateLeft_V(Ux_:Uz_)  = StateLeft_V(Ux_:Uz_)/StateLeft_V(Rho_)
       StateRight_V          = State_VGB(:,i,j,k,iBlock)
       StateRight_V(Ux_:Uz_) = StateRight_V(Ux_:Uz_)/StateRight_V(Rho_)
    case(4)
       Area = -CellFace_DB(y_,iBlock)
       NormalX = 0.0
       NormalY = 1.0
       NormalZ = 0.0
       StateLeft_V           = State_VGB(:,i,j,k,iBlock)
       StateLeft_V(Ux_:Uz_)  = StateLeft_V(Ux_:Uz_)/StateLeft_V(Rho_)
       StateRight_V          = State_VGB(:,i,j+1,k,iBlock)
       StateRight_V(Ux_:Uz_) = StateRight_V(Ux_:Uz_)/StateRight_V(Rho_)
    case(5)
       Area = CellFace_DB(z_,iBlock)
       NormalX = 0.0
       NormalY = 0.0
       NormalZ = 1.0
       StateLeft_V           = State_VGB(:,i,j,k-1,iBlock)
       StateLeft_V(Ux_:Uz_)  = StateLeft_V(Ux_:Uz_)/StateLeft_V(Rho_)
       StateRight_V          = State_VGB(:,i,j,k,iBlock)
       StateRight_V(Ux_:Uz_) = StateRight_V(Ux_:Uz_)/StateRight_V(Rho_)
    case(6)
       Area = -CellFace_DB(z_,iBlock)
       NormalX = 0.0
       NormalY = 0.0
       NormalZ = 1.0
       ! This could be call get_face(iFace,i,j,k)
       StateLeft_V           = State_VGB(:,i,j,k,iBlock)
       StateLeft_V(Ux_:Uz_)  = StateLeft_V(Ux_:Uz_)/StateLeft_V(Rho_)
       StateRight_V          = State_VGB(:,i,j,k+1,iBlock)
       StateRight_V(Ux_:Uz_) = StateRight_V(Ux_:Uz_)/StateRight_V(Rho_)
    end select

    ! average state
    State_V = 0.5*(StateLeft_V + StateRight_V)

    ! Calculate the Rusanov flux
    call get_speed_max(State_V, NormalX, NormalY, NormalZ, &
         Un, Cmax)
    call get_physical_flux(StateLeft_V, NormalX, NormalY, NormalZ, &
         StateLeftCons_V, FluxLeft_V)
    call get_physical_flux(StateRight_V, NormalX, NormalY, NormalZ, &
         StateRightCons_V, FluxRight_V)

    ! Rusanov flux
    Flux_V = Area*(0.5*(FluxLeft_V + FluxRight_V) &
         + 0.5*Cmax*(StateLeftCons_V - StateRightCons_V))

    Change_V = Change_V + Flux_V

  end subroutine do_face
  !============================================================================
  subroutine get_physical_flux(State_V, NormalX, NormalY, NormalZ, &
       StateCons_V, Flux_V)
    !$acc routine seq

    real, intent(in) :: State_V(nVar)      ! primitive state vector
    real, intent(in) :: NormalX, NormalY, NormalZ ! face normal
    real, intent(out):: StateCons_V(nFlux) ! conservative state vector
    real, intent(out):: Flux_V(nFlux)      ! conservative flux

    real:: Rho, Ux, Uy, Uz, Bx, By, Bz, Hyp, p, e, Un, Bn, pB, pTot
    !--------------------------------------------------------------------------
    Rho = State_V(Rho_)
    Ux  = State_V(Ux_)
    Uy  = State_V(Uy_)
    Uz  = State_V(Uz_)
    Bx  = State_V(Bx_)
    By  = State_V(By_)
    Bz  = State_V(Bz_)
    if(Hyp_ > 1) Hyp = State_V(Hyp_)
    p   = State_V(p_)

    ! Convenient variables
    Un  = Ux*NormalX + Uy*NormalY + Uz*NormalZ
    Bn  = Bx*NormalX + By*NormalY + Bz*NormalZ
    pB  = 0.5*(Bx**2 + By**2 + Bz**2)

    ! Hydro energy density
    e   = InvGammaMinus1*p + 0.5*Rho*(Ux**2 + Uy**2 + Uz**2)

    ! Conservative state for the Rusanov solver
    StateCons_V(Rho_)    = Rho
    StateCons_V(RhoUx_)  = Rho*Ux
    StateCons_V(RhoUy_)  = Rho*Uy
    StateCons_V(RhoUz_)  = Rho*Uz
    StateCons_V(Bx_)     = Bx
    StateCons_V(By_)     = By
    StateCons_V(Bz_)     = Bz
    if(Hyp_ > 1) StateCons_V(Hyp_) = Hyp
    StateCons_V(p_)      = p
    StateCons_V(Energy_) = e + pB  ! Add magnetic energy density

    ! Physical flux
    pTot = p + pB
    Flux_V(Rho_)    = Rho*Un
    Flux_V(RhoUx_)  = Un*Rho*Ux - Bn*Bx + NormalX*pTot
    Flux_V(RhoUy_)  = Un*Rho*Uy - Bn*By + NormalY*pTot
    Flux_V(RhoUz_)  = Un*Rho*Uz - Bn*Bz + NormalZ*pTot
    if(Hyp_ > 1)then
       Flux_V(Bx_)  =  Un*Bx - Ux*Bn + NormalX*SpeedHyp*Hyp
       Flux_V(By_)  =  Un*By - Uy*Bn + NormalY*SpeedHyp*Hyp
       Flux_V(Bz_)  =  Un*Bz - Uz*Bn + NormalZ*SpeedHyp*Hyp
       Flux_V(Hyp_) =  SpeedHyp*Bn
    else
       Flux_V(Bx_)  =  Un*Bx - Ux*Bn
       Flux_V(By_)  =  Un*By - Uy*Bn
       Flux_V(Bz_)  =  Un*Bz - Uz*Bn
    end if
    Flux_V(p_)      =  Un*p
    Flux_V(Energy_) =  Un*(e + p) &
         + Flux_V(Bx_)*Bx + Flux_V(By_)*By + Flux_V(Bz_)*Bz ! Poynting flux

  end subroutine get_physical_flux
  !============================================================================
  subroutine get_speed_max(State_V, NormalX, NormalY, NormalZ, Un, Cmax)
    !$acc routine seq

    real, intent(in):: State_V(nVar), NormalX, NormalY, NormalZ
    real, intent(out):: Un, Cmax

    real:: InvRho, p, Bx, By, Bz, Bn, B2
    real:: Sound2, Alfven2, Alfven2Normal, Fast2, Discr, Fast
    !--------------------------------------------------------------------------

    InvRho = 1.0/State_V(Rho_)
    Bx  = State_V(Bx_)
    By  = State_V(By_)
    Bz  = State_V(Bz_)
    p   = State_V(p_)
    Bn  = Bx*NormalX + By*NormalY + Bz*NormalZ
    B2  = Bx**2 + By**2 + Bz**2

    Sound2        = InvRho*p*Gamma
    Alfven2       = InvRho*B2
    Alfven2Normal = InvRho*Bn**2
    Fast2 = Sound2 + Alfven2
    Discr = sqrt(max(0.0, Fast2**2 - 4*Sound2*Alfven2Normal))
    Fast  = sqrt( 0.5*(Fast2 + Discr) )

    Un   = State_V(Ux_)*NormalX + State_V(Uy_)*NormalY + State_V(Uz_)*NormalZ
    Cmax = abs(Un) + Fast

#ifndef OPENACC
    if(DoTestCell)then
       write(*,*) &
            ' iFluid, rho, p(face)   =', 1, State_V(Rho_), State_V(p_)
       ! if(UseAnisoPressure) write(*,*) &
       !     ' Ppar, Perp             =', Ppar, Pperp
       ! if(UseElectronPressure) write(*,*) &
       !     ' State_V(Pe_)           =', State_V(Pe_)
       ! if(UseAnisoPe) write(*,*) &
       !     ' State_V(Pepar_)        =', State_V(Pepar_)
       ! if(UseWavePressure) write(*,*) &
       !     ' GammaWave, State(Waves)=', &
       !     GammaWave, State_V(WaveFirst_:WaveLast_)
       write(*,*) &
            ' Fast2, Discr          =', Fast2, Discr
       write(*,*) &
            ' Sound2, Alfven2       =', Sound2, Alfven2
       write(*,*) &
            ' FullBn, Alfven2Normal =', Bn, Alfven2Normal
       write(*,*) &
            ' FullBx, FullBy, FullBz=', Bx, By, Bz
    end if
#endif

  end subroutine get_speed_max
  !============================================================================

end module ModUpdateStateGpu
!==============================================================================
