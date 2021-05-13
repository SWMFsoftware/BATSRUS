!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModUpdateStateFast

  ! Calculate each face twice

  use ModOptimizeParam, ONLY: &
       DoLf, LimiterBeta, nStage, iStage, nOrder, &
       IsCartesian, IsCartesianGrid, UseNonConservative, nConservCrit, &
       UseDivbSource, UseHyperbolicDivB, IsTimeAccurate, UseDtFixed, UseB0
  use ModVarIndexes
  use ModMultiFluid, ONLY: iUx_I, iUy_I, iUz_I, iP_I
  use ModAdvance, ONLY: nFlux, State_VGB, StateOld_VGB, &
       Flux_VXI, Flux_VYI, Flux_VZI, nFaceValue, UnFirst_, Bn_ => BnL_, &
       Primitive_VGI, IsConserv_CB, &
       DtMax_CB => time_BLK, Vdt_
  use BATL_lib, ONLY: nDim, nI, nJ, nK, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, &
       nBlock, Unused_B, x_, y_, z_, CellVolume_B, CellFace_DB, &
       test_start, test_stop, iTest, jTest, kTest, iBlockTest
  use BATL_lib, ONLY: CellVolume_GB, CellFace_DFB, FaceNormal_DDFB, Xyz_DGB
  use ModPhysics, ONLY: Gamma, GammaMinus1, InvGammaMinus1, &
       GammaMinus1_I, InvGammaMinus1_I, FaceState_VI
  use ModMain, ONLY: UseB, SpeedHyp, Dt, Cfl, body1_
  use ModNumConst, ONLY: cUnit_DD
  use ModTimeStepControl, ONLY: calc_timestep
  use ModB0, ONLY: B0_DXB, B0_DYB, B0_DZB, B0_DGB
  use ModGeometry, ONLY: true_cell,  Body_BLK
  use ModBoundaryGeometry, ONLY: iBoundary_GB,  domain_
  use ModConst, ONLY: cTiny

  implicit none

  private ! except

  public:: update_state_cpu      ! save flux, recalculate primitive vars
  public:: update_state_gpu      ! recalculate flux and primitive vars
  public:: update_state_cpu_prim ! save flux and primitive vars
  public:: update_state_gpu_prim ! recalculate flux, save primitive vars

  logical:: DoTestCell= .false.

contains
  !============================================================================
  subroutine update_state_cpu

    ! optimal for CPU (face value and face flux calculated only once)

    integer:: i, j, k, iBlock, iGang, iFluid, iP, iUn

    real:: DtPerDv, DivU, DivB, InvRho, Change_V(nFlux)
    !$acc declare create (Change_V)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'update_state_cpu'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    !$acc parallel
    !$acc loop gang private(iGang) independent
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE

#ifdef OPENACC
       iGang = iBlock
#else
       iGang = 1
#endif

       if(iStage == 1 .and. nStage == 2) call set_old_state(iBlock)

       !$acc loop vector collapse(3) independent
       do k = 1, nK; do j = 1, nJ; do i = 1, nI+1

          if(  .not. true_cell(i-1,j,k,iBlock) .and. &
               .not. true_cell(i,j,k,iBlock)) then
             Flux_VXI(UnFirst_:Vdt_,i,j,k,iGang) = 0.0
             CYCLE
          endif

          call get_flux_x(i, j, k, iBlock)
       end do; end do; end do

       if(nDim > 1)then
          !$acc loop vector collapse(3) independent
          do k = 1, nK; do j = 1, nJ+1; do i = 1, nI

             if(  .not. true_cell(i,j-1,k,iBlock) .and. &
                  .not. true_cell(i,j,k,iBlock)) then
                Flux_VYI(UnFirst_:Vdt_,i,j,k,iGang) = 0.0
                CYCLE
             endif

             call get_flux_y(i, j, k, iBlock)
          end do; end do; end do
       end if

       if(nDim > 2)then
          !$acc loop vector collapse(3) independent
          do k = 1, nK+1; do j = 1, nJ; do i = 1, nI

             if(  .not. true_cell(i,j,k-1,iBlock) .and. &
                  .not. true_cell(i,j,k,iBlock)) then
                Flux_VZI(UnFirst_:Vdt_,i,j,k,iGang) = 0.0
                CYCLE
             endif

             call get_flux_z(i, j, k, iBlock)
          end do; end do; end do
       end if

       if(.not.IsTimeAccurate .and. iStage==1) call calc_timestep(iBlock)
       
       ! Update
       !$acc loop vector collapse(3) private(Change_V, DtPerDv) independent
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not. true_cell(i,j,k,iBlock)) CYCLE

#ifndef OPENACC
          DoTestCell = DoTest .and. i==iTest .and. j==jTest .and. k==kTest &
               .and. iBlock == iBlockTest
#endif

          Change_V =  Flux_VXI(1:nFlux,i,j,k,iGang) &
               -      Flux_VXI(1:nFlux,i+1,j,k,iGang)
          if(nDim > 1) Change_V = Change_V + Flux_VYI(1:nFlux,i,j,k,iGang) &
               -                             Flux_VYI(1:nFlux,i,j+1,k,iGang)
          if(nDim > 2) Change_V = Change_V + Flux_VZI(1:nFlux,i,j,k,iGang) &
               -                             Flux_VZI(1:nFlux,i,j,k+1,iGang)

          if(UseB .and. UseDivbSource)then
             DivB = Flux_VXI(Bn_,i+1,j,k,iGang) - Flux_VXI(Bn_,i,j,k,iGang)
             if(nJ > 1) DivB = DivB + &
                  Flux_VYI(Bn_,i,j+1,k,iGang) - Flux_VYI(Bn_,i,j,k,iGang)
             if(nK > 1) DivB = DivB + &
                  Flux_VZI(Bn_,i,j,k+1,iGang) - Flux_VZI(Bn_,i,j,k,iGang)
             InvRho = 1/State_VGB(Rho_,i,j,k,iBlock)
             Change_V(RhoUx_:RhoUz_) = Change_V(RhoUx_:RhoUz_) &
                  - DivB*State_VGB(Bx_:Bz_,i,j,k,iBlock)
             Change_V(Bx_:Bz_) = Change_V(Bx_:Bz_) &
                  - DivB*InvRho*State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)
             Change_V(Energy_) = Change_V(Energy_) &
                  - DivB*InvRho*sum(State_VGB(Bx_:Bz_,i,j,k,iBlock) &
                  *                 State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock))

             if(UseB0) then
                Change_V(RhoUx_:RhoUz_) = Change_V(RhoUx_:RhoUz_) &
                     - DivB*B0_DGB(:,i,j,k,iBlock)
             endif
          end if

          if(UseNonConservative)then
             ! Add -(g-1)*p*div(u) source term
             do iFluid = 1, nFluid
                iP  = iP_I(iFluid)
                iUn = UnFirst_ + iFluid - 1
                DivU = Flux_VXI(iUn,i+1,j,k,iGang) - Flux_VXI(iUn,i,j,k,iGang)
                if(nJ > 1) DivU = DivU &
                     + Flux_VYI(iUn,i,j+1,k,iGang) - Flux_VYI(iUn,i,j,k,iGang)
                if(nK > 1) DivU = DivU &
                     + Flux_VZI(iUn,i,j,k+1,iGang) - Flux_VZI(iUn,i,j,k,iGang)
                Change_V(iP) = Change_V(iP) &
                     - GammaMinus1_I(iFluid)*State_VGB(iP,i,j,k,iBlock)*DivU
             end do
          end if

          ! Time step divided by cell volume
          if(IsTimeAccurate)then
             DtPerDv = iStage*Dt
          else
             DtPerDv = iStage*Cfl*DtMax_CB(i,j,k,iBlock)
          end if
          if(IsCartesian)then
             DtPerDv = DtPerDv/(nStage*CellVolume_B(iBlock))
          else
             DtPerDv = DtPerDv/(nStage*CellVolume_GB(i,j,k,iBlock))
          end if

          ! Update state
          if(iStage == 1)then
             if(.not.UseNonConservative)then
                ! Overwrite pressure and change with energy
                call pressure_to_energy(State_VGB(:,i,j,k,iBlock))
                Change_V(iP_I) = Change_V(Energy_:)
             end if
             State_VGB(:,i,j,k,iBlock) = State_VGB(:,i,j,k,iBlock) &
                  + DtPerDv*Change_V(1:nVar)
          else
             if(.not.UseNonConservative)then
                ! Overwrite old pressure and change with energy
                call pressure_to_energy(StateOld_VGB(:,i,j,k,iBlock))
                Change_V(iP_I) = Change_V(Energy_:)
             end if
             State_VGB(:,i,j,k,iBlock) = StateOld_VGB(:,i,j,k,iBlock) &
                  + DtPerDv*Change_V(1:nVar)
          end if
          ! Convert energy back to pressure
          if(.not.UseNonConservative) &
               call energy_to_pressure(State_VGB(:,i,j,k,iBlock))

#ifndef OPENACC
          if(DoTestCell)then
             write(*,*)'State_VGB =', State_VGB(:,i,j,k,iBlock)
             write(*,*)'Change_V  =', Change_V
             write(*,*)'DtPerDv   =', DtPerDv
          end if
#endif

       enddo; enddo; enddo

       if(IsTimeAccurate .and. .not.UseDtFixed) call calc_timestep(iBlock)

    end do
    !$acc end parallel

    call test_stop(NameSub, DoTest, iBlock)

  end subroutine update_state_cpu
  !============================================================================
  subroutine get_flux_x(i, j,  k, iBlock)
    !$acc routine seq

    integer, intent(in):: i, j, k, iBlock

    real :: Area, Normal_D(3), B0_D(3)
    real :: StateLeft_V(nVar), StateRight_V(nVar)
    integer:: iGang
    !--------------------------------------------------------------------------
#ifdef OPENACC
    iGang = iBlock
#else
    iGang = 1
#endif
    call get_normal(1, i, j, k, iBlock, Normal_D, Area)

    call get_face_x(i, j, k, iBlock, StateLeft_V, StateRight_V)

    B0_D = 0
    if(UseB0) B0_D = B0_DXB(:,i,j,k,iBlock)

    if(Body_BLK(iBlock)) then
       if (true_cell(i-1,j,k,iBlock) .and. &
            iBoundary_GB(i,j,k,iBlock) /= domain_) then
          call set_face(StateLeft_V, StateRight_V,&
               0.5*(Xyz_DGB(:,i-1,j,k,iBlock) + Xyz_DGB(:,i,j,k,iBlock)))
       endif

       if (true_cell(i,j,k,iBlock) .and. &
            iBoundary_GB(i-1,j,k,iBlock) /= domain_) then
          call set_face(StateRight_V, StateLeft_V,&
               0.5*(Xyz_DGB(:,i-1,j,k,iBlock) + Xyz_DGB(:,i,j,k,iBlock)))
       endif
    endif

    call get_numerical_flux(Normal_D, Area, &
         StateLeft_V, StateRight_V, Flux_VXI(:,i,j,k,iGang), B0_D)

  end subroutine get_flux_x
  !============================================================================
  subroutine get_flux_y(i, j, k, iBlock)
    !$acc routine seq

    integer, intent(in):: i, j, k, iBlock

    real :: Area, Normal_D(3), B0_D(3)
    real :: StateLeft_V(nVar), StateRight_V(nVar)
    integer:: iGang
    !--------------------------------------------------------------------------
#ifdef OPENACC
    iGang = iBlock
#else
    iGang = 1
#endif
    call get_normal(2, i, j, k, iBlock, Normal_D, Area)

    call get_face_y(i, j, k, iBlock, StateLeft_V, StateRight_V)

    B0_D = 0
    if(UseB0) B0_D = B0_DYB(:,i,j,k,iBlock)

    if(Body_BLK(iBlock)) then
       if (true_cell(i,j-1,k,iBlock) .and. &
            iBoundary_GB(i,j,k,iBlock) /= domain_) then
          call set_face(StateLeft_V, StateRight_V,&
               0.5*(Xyz_DGB(:,i,j-1,k,iBlock) + Xyz_DGB(:,i,j,k,iBlock)))
       endif

       if (true_cell(i,j,k,iBlock) .and. &
            iBoundary_GB(i,j-1,k,iBlock) /= domain_) then
          call set_face(StateRight_V, StateLeft_V,&
               0.5*(Xyz_DGB(:,i,j-1,k,iBlock) + Xyz_DGB(:,i,j,k,iBlock)))
       endif
    endif

    call get_numerical_flux(Normal_D, Area, &
         StateLeft_V, StateRight_V, Flux_VYI(:,i,j,k,iGang), B0_D)

  end subroutine get_flux_y
  !============================================================================
  subroutine get_flux_z(i, j, k, iBlock)
    !$acc routine seq

    integer, intent(in):: i, j, k, iBlock

    real :: Area, Normal_D(3), B0_D(3)
    real :: StateLeft_V(nVar), StateRight_V(nVar)
    integer:: iGang
    !--------------------------------------------------------------------------
#ifdef OPENACC
    iGang = iBlock
#else
    iGang = 1
#endif
    call get_normal(3, i, j, k, iBlock, Normal_D, Area)

    call get_face_z(i, j, k, iBlock, StateLeft_V, StateRight_V)

    B0_D = 0
    if(UseB0) B0_D = B0_DZB(:,i,j,k,iBlock)

    if(Body_BLK(iBlock)) then
       if (true_cell(i,j,k-1,iBlock) .and. &
            iBoundary_GB(i,j,k,iBlock) /= domain_) then
          call set_face(StateLeft_V, StateRight_V,&
               0.5*(Xyz_DGB(:,i,j,k-1,iBlock) + Xyz_DGB(:,i,j,k,iBlock)))
       endif

       if (true_cell(i,j,k,iBlock) .and. &
            iBoundary_GB(i,j,k-1,iBlock) /= domain_) then
          call set_face(StateRight_V, StateLeft_V,&
               0.5*(Xyz_DGB(:,i,j,k-1,iBlock) + Xyz_DGB(:,i,j,k,iBlock)))
       endif
    endif

    call get_numerical_flux(Normal_D, Area, &
         StateLeft_V, StateRight_V, Flux_VZI(:,i,j,k,iGang), B0_D)

  end subroutine get_flux_z
  !============================================================================
  subroutine update_state_gpu

    ! optimal for GPU, but also works with CPU

    integer:: i, j, k, iBlock

    logical:: IsConserv
    real:: CellVolume, DtPerDv
    real:: Change_V(nFlux+nDim), Change_VC(nFlux+1,nI,nJ,nK)
    !$acc declare create (Change_V, Change_VC)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'update_state_gpu'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    !$acc parallel
    !$acc loop gang private(Change_VC) independent
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE

       if(iStage == 1 .and. nStage == 2) call set_old_state(iBlock)

       !$acc loop vector collapse(3) private(Change_V) independent
       do k = 1, nK; do j = 1, nJ; do i = 1, nI

#ifndef OPENACC
          DoTestCell = DoTest .and. i==iTest .and. j==jTest .and. k==kTest &
               .and. iBlock == iBlockTest
#endif

          ! Set StateOld here?

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

          Change_VC(1:nFlux,i,j,k) = Change_V(1:nFlux)
          ! Calculate 1/sum(area*cmax)
          if(.not.UseDtFixed) Change_VC(nFlux+1,i,j,k) &
               = 1.0/sum(Change_V(nFlux+1:nFlux+nDim))

       enddo; enddo; enddo

       ! Update state
       if(iStage == 1)then
          !$acc loop vector collapse(3) private(DtPerDv) independent
          do k = 1, nK; do j = 1, nJ; do i = 1, nI

             if(.not.IsTimeAccurate)then
                ! Local time stepping: cell volume cancels out
                ! Dt/Volume = (Cfl/nStage*Volume/Vdt)/Volume
                DtPerDv = Cfl*Change_VC(nFlux+1,i,j,k)/nStage
             else
                if(IsCartesian)then
                   CellVolume = CellVolume_B(iBlock)
                else
                   CellVolume = CellVolume_GB(i,j,k,iBlock)
                end if
                DtPerDv = Dt/(nStage*CellVolume)
                ! For next time step
                if(.not.UseDtFixed .and. nStage==1) DtMax_CB(i,j,k,iBlock) = &
                     CellVolume*Change_VC(nFlux+1,i,j,k)
             end if
             IsConserv = .not.UseNonConservative
             if(nConservCrit > 0) IsConserv = IsConserv_CB(i,j,k,iBlock)
             if(IsConserv)then
                ! Overwrite pressure and change with energy
                call pressure_to_energy(State_VGB(:,i,j,k,iBlock))
                Change_VC(iP_I,i,j,k) = Change_VC(Energy_:nFlux,i,j,k)
             end if
             ! Update
             State_VGB(:,i,j,k,iBlock) = State_VGB(:,i,j,k,iBlock) &
                  + DtPerDv*Change_VC(1:nVar,i,j,k)

             ! Convert energy back to pressure
             if(IsConserv) call energy_to_pressure(State_VGB(:,i,j,k,iBlock))

#ifndef OPENACC
             DoTestCell = DoTest .and. i==iTest .and. j==jTest .and. k==kTest &
                  .and. iBlock == iBlockTest
             if(DoTestCell)then
                write(*,*)'iStage    =', iStage
                write(*,*)'State_VGB =', State_VGB(:,i,j,k,iBlock)
                write(*,*)'Change_V  =', Change_VC(:,i,j,k)
                write(*,*)'DtPerDv   =', DtPerDv
             end if
#endif

          enddo; enddo; enddo
       else
          !$acc loop vector collapse(3) private(DtPerDv) independent
          do k = 1, nK; do j = 1, nJ; do i = 1, nI

             if(.not.IsTimeAccurate)then
                ! Local time stepping: cell volume cancels out
                ! Dt/Volume = (Cfl/nStage*Volume/Vdt)/Volume
                DtPerDv = Cfl*Change_VC(nFlux+1,i,j,k)
             else
                if(IsCartesian)then
                   CellVolume = CellVolume_B(iBlock)
                else
                   CellVolume = CellVolume_GB(i,j,k,iBlock)
                end if
                DtPerDv = Dt/CellVolume
                ! For next time step (this is the 2nd and last stage)
                if(.not.UseDtFixed) DtMax_CB(i,j,k,iBlock) = &
                     CellVolume*Change_VC(nFlux+1,i,j,k)
             end if

             IsConserv = .not.UseNonConservative
             if(nConservCrit > 0) IsConserv = IsConserv_CB(i,j,k,iBlock)
             if(IsConserv)then
                ! Overwrite pressure and change with energy
                call pressure_to_energy(StateOld_VGB(:,i,j,k,iBlock))
                Change_VC(iP_I,i,j,k) = Change_VC(Energy_:nFlux,i,j,k)
             end if
             ! Update state
             State_VGB(:,i,j,k,iBlock) = StateOld_VGB(:,i,j,k,iBlock) &
                  + DtPerDv*Change_VC(1:nVar,i,j,k)
             ! Convert energy back to pressure
             if(IsConserv) call energy_to_pressure(State_VGB(:,i,j,k,iBlock))

#ifndef OPENACC
             DoTestCell = DoTest .and. i==iTest .and. j==jTest .and. k==kTest &
                  .and. iBlock == iBlockTest
             if(DoTestCell)then
                write(*,*)'iStage    =', iStage
                write(*,*)'State_VGB =', State_VGB(:,i,j,k,iBlock)
                write(*,*)'Change_V  =', Change_VC(:,i,j,k)
                write(*,*)'DtPerDv   =', DtPerDv
             end if
#endif
          enddo; enddo; enddo
       end if

       ! Flux_V is a local variable so generic calc_timestep does not work
       ! call calc_timestep(iBlock)

    end do
    !$acc end parallel

    call test_stop(NameSub, DoTest, iBlock)

  end subroutine update_state_gpu
  !============================================================================

  subroutine set_face(VarsTrueFace_V, VarsGhostFace_V, FaceCoords_D)
    !$acc routine seq
    real, intent(in)   :: VarsTrueFace_V(nVar)
    real, intent(out)  :: VarsGhostFace_V(nVar)
    real, intent(in)   :: FaceCoords_D(3)

    real, parameter:: DensityJumpLimit=0.1
    !--------------------------------------------------------------------------

    if(.true.) then 
       ! 'ionosphere' type BC
       
       VarsGhostFace_V        =  VarsTrueFace_V

       ! Use body densities but limit jump
       ! Pressure gets set too (! ). It will be overwritten below
       where(DefaultState_V(1:nVar) > cTiny)
          VarsGhostFace_V = VarsTrueFace_V + &
               sign(1.0, FaceState_VI(:,body1_) - VarsTrueFace_V)*   &
               min( abs(FaceState_VI(:,body1_) - VarsTrueFace_V)     &
               ,    DensityJumpLimit*VarsTrueFace_V   )
       end where
       
       ! Set pressures, including electron pressure, to float.
       VarsGhostFace_V(iP_I) = VarsTrueFace_V(iP_I)

       ! Change sign for velocities (plasma frozen into dipole field)
       VarsGhostFace_V(iUx_I) = -VarsTrueFace_V(iUx_I)
       VarsGhostFace_V(iUy_I) = -VarsTrueFace_V(iUy_I)
       VarsGhostFace_V(iUz_I) = -VarsTrueFace_V(iUz_I)


       ! Change B_ghost so that the radial component of Bghost + Btrue = 0
       ! Brefl = 2*r*(B.r)/r^2, so Bghost = Btrue - Brefl
       VarsGhostFace_V(Bx_:Bz_) = VarsTrueFace_V(Bx_:Bz_) - &
            2*FaceCoords_D*sum(VarsTrueFace_V(Bx_:Bz_)*FaceCoords_D)& ! Brefl
            /sum(FaceCoords_D**2)                                     ! Brefl
    else 
       ! 'ionospherefloat'

       VarsGhostFace_V        =  VarsTrueFace_V
       VarsGhostFace_V(iUx_I) = -VarsTrueFace_V(iUx_I)
       VarsGhostFace_V(iUy_I) = -VarsTrueFace_V(iUy_I)
       VarsGhostFace_V(iUz_I) = -VarsTrueFace_V(iUz_I)
    endif

  end subroutine set_face
  !============================================================================

  subroutine do_face(iFace, i, j, k, iBlock, Change_V)
    !$acc routine seq

    integer, intent(in):: iFace, i, j, k, iBlock
    real, intent(inout):: Change_V(nFlux+nDim)

    integer:: iDim
    real:: Area, Normal_D(3), InvRho, B0_D(3)
    real:: StateLeft_V(nVar), StateRight_V(nVar), Flux_V(nFaceValue)
    !--------------------------------------------------------------------------
    B0_D = 0
    select case(iFace)
    case(1)
       call get_normal(1, i, j, k, iBlock, Normal_D, Area)
       call get_face_x(   i, j, k, iBlock, StateLeft_V, StateRight_V)
       if(UseB0) B0_D = B0_DXB(:,i,j,k,iBlock)
    case(2)
       call get_normal(1, i+1, j, k, iBlock, Normal_D, Area)
       Area = -Area
       call get_face_x(   i+1, j, k, iBlock, StateLeft_V, StateRight_V)
       if(UseB0) B0_D = B0_DXB(:,i+1,j,k,iBlock)
    case(3)
       call get_normal(2, i, j, k, iBlock, Normal_D, Area)
       call get_face_y(   i, j, k, iBlock, StateLeft_V, StateRight_V)
       if(UseB0) B0_D = B0_DYB(:,i,j,k,iBlock)
    case(4)
       call get_normal(2, i, j+1, k, iBlock, Normal_D, Area)
       Area = -Area
       call get_face_y(   i, j+1, k, iBlock, StateLeft_V, StateRight_V)
       if(UseB0) B0_D = B0_DYB(:,i,j+1,k,iBlock)
    case(5)
       call get_normal(3, i, j, k, iBlock, Normal_D, Area)
       call get_face_z(   i, j, k, iBlock, StateLeft_V, StateRight_V)
       if(UseB0) B0_D = B0_DZB(:,i,j,k,iBlock)
    case(6)
       call get_normal(3, i, j, k+1, iBlock, Normal_D, Area)
       Area = -Area
       call get_face_z(   i, j, k+1, iBlock, StateLeft_V, StateRight_V)
       if(UseB0) B0_D = B0_DZB(:,i,j,k+1,iBlock)
    end select

    call get_numerical_flux(Normal_D, &
         Area, StateLeft_V, StateRight_V, Flux_V, B0_D)

    ! Change due to fluxes through this face
    Change_V(1:nFlux) = Change_V(1:nFlux) + Flux_V(1:nFlux)

    ! Change due to -(gama-1)*divU source terms
    if(nFluid == 1)then
       if(UseNonConservative) Change_V(p_) = Change_V(p_) &
            + GammaMinus1*State_VGB(p_,i,j,k,iBlock)*Flux_V(UnFirst_)
    else
       if(UseNonConservative) Change_V(iP_I) = Change_V(iP_I) &
            + GammaMinus1_I*State_VGB(iP_I,i,j,k,iBlock) &
            *Flux_V(UnFirst_:UnFirst_+nFluid-1)
    end if

    ! Change due to 8-wave source terms
    if(UseB .and. UseDivbSource)then
       InvRho = 1/State_VGB(Rho_,i,j,k,iBlock)
       Change_V(RhoUx_:RhoUz_) = Change_V(RhoUx_:RhoUz_) &
            + Flux_V(Bn_)*State_VGB(Bx_:Bz_,i,j,k,iBlock)
       Change_V(Bx_:Bz_) = Change_V(Bx_:Bz_) &
            + Flux_V(Bn_)*InvRho*State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)
       Change_V(Energy_) = Change_V(Energy_) &
            + Flux_V(Bn_)*InvRho*sum(State_VGB(Bx_:Bz_,i,j,k,iBlock) &
            *                        State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock))
    end if

    ! Calculate maximum of Cmax*Area from the faces per dimension
    if(.not.UseDtFixed)then
       iDim = (iFace+1)/2
       Change_V(nFlux+iDim) = max(Change_V(nFlux+iDim), Flux_V(Vdt_))
    end if

  end subroutine do_face
  !============================================================================
  subroutine set_old_state(iBlock)
    !$acc routine vector

    integer, intent(in):: iBlock

    integer:: i, j, k

    !$acc loop vector collapse(3) independent
    !--------------------------------------------------------------------------
    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       StateOld_VGB(:,i,j,k,iBlock)  = State_VGB(:,i,j,k,iBlock)
    end do; end do; end do

  end subroutine set_old_state
  !============================================================================

  subroutine get_physical_flux(State_V, Normal_D, StateCons_V, Flux_V, B0_D)
    !$acc routine seq

    real, intent(in) :: State_V(nVar)      ! primitive state vector
    real, intent(in) :: Normal_D(3)        ! face normal
    real, intent(out):: StateCons_V(nFlux) ! conservative state vector
    real, intent(out):: Flux_V(nFlux)      ! conservative flux
    real, intent(in) :: B0_D(3)

    real:: Rho, Un, Bn, pB, e
    real:: B0B1, FullB_D(3), B0n,  FullBn
    ! Convenient variables
    !--------------------------------------------------------------------------
    Rho = State_V(Rho_)
    Un  = sum(State_V(Ux_:Uz_)*Normal_D)

    FullB_D = State_V(Bx_:Bz_) + B0_D

    Bn  = sum(State_V(Bx_:Bz_)*Normal_D)
    B0n = sum(B0_D*Normal_D)
    FullBn = Bn + B0n

    B0B1= sum(State_V(Bx_:Bz_)*B0_D)
    pB  = 0.5*sum(State_V(Bx_:Bz_)**2)

    e   = InvGammaMinus1*State_V(p_) + 0.5*Rho*sum(State_V(Ux_:Uz_)**2)

    ! Conservative state for the Rusanov solver
    StateCons_V(1:nVar) = State_V
    StateCons_V(RhoUx_:RhoUz_) = State_V(Rho_)*State_V(Ux_:Uz_)
    StateCons_V(Energy_) = e + pB ! Add magnetic energy density

    ! Physical flux
    Flux_V(Rho_) = Rho*Un
    Flux_V(RhoUx_:RhoUz_) = Un*Rho*State_V(Ux_:Uz_) + Normal_D*State_V(p_) &
         -Bn*FullB_D - B0n*State_V(Bx_:Bz_) + (pB + B0B1)*Normal_D
    if(Hyp_ > 1)then
       Flux_V(Bx_:Bz_) = Un*FullB_D - State_V(Ux_:Uz_)*FullBn &
            + Normal_D*SpeedHyp*State_V(Hyp_)
       Flux_V(Hyp_) = SpeedHyp*Bn
    else
       Flux_V(Bx_:Bz_) = Un*FullB_D - State_V(Ux_:Uz_)*FullBn
    end if
    Flux_V(p_)      =  Un*State_V(p_)
    Flux_V(Energy_) =  Un*(e + State_V(p_)) &
         + sum(Flux_V(Bx_:Bz_)*State_V(Bx_:Bz_)) ! Poynting flux

  end subroutine get_physical_flux
  !============================================================================
  subroutine get_speed_max(State_V, Normal_D, &
       Un, B0_D, Cmax, Cleft, Cright)
    !$acc routine seq

    ! Using primitive variable State_V and normal direction get
    ! normal velocity and wave speeds.

    real, intent(in) :: State_V(nVar), Normal_D(3)
    real, intent(out):: Un              ! normal velocity (signed)
    real, intent(in) :: B0_D(3)
    real, intent(out), optional:: Cmax  ! maximum speed (positive)
    real, intent(out), optional:: Cleft ! fastest left wave (usually negative)
    real, intent(out), optional:: Cright! fastest right wave (usually positive)

    real:: InvRho, Bn, B2
    real:: Sound2, Fast2, Discr, Fast
    !--------------------------------------------------------------------------
    InvRho = 1.0/State_V(Rho_)
    Bn  = sum((State_V(Bx_:Bz_)+B0_D)*Normal_D)
    B2  = sum((State_V(Bx_:Bz_)+B0_D)**2)

    Sound2= InvRho*State_V(p_)*Gamma
    Fast2 = Sound2 + InvRho*B2
    Discr = sqrt(max(0.0, Fast2**2 - 4*Sound2*InvRho*Bn**2))
    Fast  = sqrt( 0.5*(Fast2 + Discr) )

    Un = sum(State_V(Ux_:Uz_)*Normal_D)
    if(present(Cmax))   Cmax   = abs(Un) + Fast
    if(present(Cleft))  Cleft  = Un - Fast
    if(present(Cright)) Cright = Un + Fast

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
            ' Sound2, Alfven2       =', Sound2, InvRho*B2
       write(*,*) &
            ' FullBn, Alfven2Normal =', Bn, InvRho*Bn**2
       write(*,*) &
            ' FullBx, FullBy, FullBz=', State_V(Bx_:Bz_)+B0_D
    end if
#endif

  end subroutine get_speed_max
  !============================================================================
  subroutine get_normal(iDir, i, j, k, iBlock, Normal_D, Area)
    !$acc routine seq
    integer, intent(in) :: i, j, k, iBlock, iDir
    real,    intent(out):: Normal_D(3), Area
    !--------------------------------------------------------------------------
    if(IsCartesian)then
       Area = CellFace_DB(iDir,iBlock)
    else
       Area = CellFace_DFB(iDir,i,j,k,iBlock)
       if(Area == 0.0)then
          Normal_D = [1.0, 0.0, 0.0]
          RETURN
       end if
    end if
    if(IsCartesianGrid)then
       Normal_D = cUnit_DD(:,iDir)
    else
       Normal_D = FaceNormal_DDFB(:,iDir,i,j,k,iBlock)/Area
    end if

  end subroutine get_normal
  !============================================================================
  subroutine get_face_x(i, j, k, iBlock, StateLeft_V, StateRight_V)
    !$acc routine seq

    integer, intent(in) :: i, j, k, iBlock
    real,    intent(out):: StateLeft_V(nVar), StateRight_V(nVar)

    integer:: iVar
    !--------------------------------------------------------------------------
    if(nOrder == 1)then
       call get_primitive(State_VGB(:,i-1,j,k,iBlock), StateLeft_V)
       call get_primitive(State_VGB(:,i,j,k,iBlock),   StateRight_V)
    else
       ! Do it per variable to reduce memory use
       do iVar = 1, nVar
          ! Single fluid conversion to primitive variables
          if(iVar < Ux_ .or. iVar > Uz_)then
             call limiter2( &
                  State_VGB(iVar,i-2,j,k,iBlock), &
                  State_VGB(iVar,i-1,j,k,iBlock), &
                  State_VGB(iVar,  i,j,k,iBlock), &
                  State_VGB(iVar,i+1,j,k,iBlock), &
                  StateLeft_V(iVar), StateRight_V(iVar))
          else
             call limiter2( &
                  State_VGB(iVar,i-2,j,k,iBlock)/ &
                  State_VGB(Rho_,i-2,j,k,iBlock), &
                  State_VGB(iVar,i-1,j,k,iBlock)/ &
                  State_VGB(Rho_,i-1,j,k,iBlock), &
                  State_VGB(iVar,  i,j,k,iBlock)/ &
                  State_VGB(Rho_,  i,j,k,iBlock), &
                  State_VGB(iVar,i+1,j,k,iBlock)/ &
                  State_VGB(Rho_,i+1,j,k,iBlock), &
                  StateLeft_V(iVar), StateRight_V(iVar))
          end if
       end do

       ! Return to 1st order for the faces that need body cells to
       ! calculate 2nd order face values.
       ! This is equivalent to limiter_body in ModFaceValue.f90
       if(any(.not.true_cell(i-2:i,j,k,iBlock) )) &
            call get_primitive(State_VGB(:,i-1,j,k,iBlock), StateLeft_V)

       if(any(.not.true_cell(i-1:i+1,j,k,iBlock) )) &
            call get_primitive(State_VGB(:,i,j,k,iBlock),   StateRight_V)
    end if

  end subroutine get_face_x
  !============================================================================
  subroutine get_face_y(i, j, k, iBlock, StateLeft_V, StateRight_V)
    !$acc routine seq

    integer, intent(in) :: i, j, k, iBlock
    real,    intent(out):: StateLeft_V(nVar), StateRight_V(nVar)

    integer:: iVar
    !--------------------------------------------------------------------------
    if(nOrder == 1)then
       call get_primitive(State_VGB(:,i,j-1,k,iBlock), StateLeft_V)
       call get_primitive(State_VGB(:,i,j,k,iBlock),   StateRight_V)
    else
       ! Do it per variable to reduce memory use
       do iVar = 1, nVar
          ! Single fluid conversion to primitive variables
          if(iVar < Ux_ .or. iVar > Uz_)then
             call limiter2( &
                  State_VGB(iVar,i,j-2,k,iBlock), &
                  State_VGB(iVar,i,j-1,k,iBlock), &
                  State_VGB(iVar,i,j  ,k,iBlock), &
                  State_VGB(iVar,i,j+1,k,iBlock), &
                  StateLeft_V(iVar), StateRight_V(iVar))
          else
             call limiter2( &
                  State_VGB(iVar,i,j-2,k,iBlock)/ &
                  State_VGB(Rho_,i,j-2,k,iBlock), &
                  State_VGB(iVar,i,j-1,k,iBlock)/ &
                  State_VGB(Rho_,i,j-1,k,iBlock), &
                  State_VGB(iVar,i,j  ,k,iBlock)/ &
                  State_VGB(Rho_,i,j  ,k,iBlock), &
                  State_VGB(iVar,i,j+1,k,iBlock)/ &
                  State_VGB(Rho_,i,j+1,k,iBlock), &
                  StateLeft_V(iVar), StateRight_V(iVar))
          end if
       end do

       if(any(.not.true_cell(i,j-2:j,k,iBlock) )) &
            call get_primitive(State_VGB(:,i,j-1,k,iBlock), StateLeft_V)

       if(any(.not.true_cell(i,j-1:j+1,k,iBlock) )) &
            call get_primitive(State_VGB(:,i,j,k,iBlock),   StateRight_V)
    end if
  end subroutine get_face_y
  !============================================================================
  subroutine get_face_z(i, j, k, iBlock, StateLeft_V, StateRight_V)
    !$acc routine seq

    integer, intent(in) :: i, j, k, iBlock
    real,    intent(out):: StateLeft_V(nVar), StateRight_V(nVar)

    integer:: iVar
    !--------------------------------------------------------------------------
    if(nOrder == 1)then
       call get_primitive(State_VGB(:,i,j,k-1,iBlock), StateLeft_V)
       call get_primitive(State_VGB(:,i,j,k,iBlock),   StateRight_V)
    else
       ! Do it per variable to reduce memory use
       do iVar = 1, nVar
          ! Single fluid conversion to primitive variables
          if(iVar < Ux_ .or. iVar > Uz_)then
             call limiter2( &
                  State_VGB(iVar,i,j,k-2,iBlock), &
                  State_VGB(iVar,i,j,k-1,iBlock), &
                  State_VGB(iVar,i,j,k  ,iBlock), &
                  State_VGB(iVar,i,j,k+1,iBlock), &
                  StateLeft_V(iVar), StateRight_V(iVar))
          else
             call limiter2( &
                  State_VGB(iVar,i,j,k-2,iBlock)/ &
                  State_VGB(Rho_,i,j,k-2,iBlock), &
                  State_VGB(iVar,i,j,k-1,iBlock)/ &
                  State_VGB(Rho_,i,j,k-1,iBlock), &
                  State_VGB(iVar,i,j,k  ,iBlock)/ &
                  State_VGB(Rho_,i,j,k  ,iBlock), &
                  State_VGB(iVar,i,j,k+1,iBlock)/ &
                  State_VGB(Rho_,i,j,k+1,iBlock), &
                  StateLeft_V(iVar), StateRight_V(iVar))
          end if
       end do

       if(any(.not.true_cell(i,j,k-2:k,iBlock) )) &
            call get_primitive(State_VGB(:,i,j,k-1,iBlock), StateLeft_V)

       if(any(.not.true_cell(i,j,k-1:k+1,iBlock) )) &
            call get_primitive(State_VGB(:,i,j,k,iBlock),   StateRight_V)
    end if

  end subroutine get_face_z
  !============================================================================
  subroutine get_numerical_flux(Normal_D, &
       Area,  StateLeft_V, StateRight_V, Flux_V, B0_D)
    !$acc routine seq

    real, intent(in)   :: Normal_D(3), Area
    real, intent(inout):: StateLeft_V(nVar), StateRight_V(nVar)
    real, intent(out)  :: Flux_V(nFaceValue)
    real, intent(in)   :: B0_D(3)

    ! Average state
    real:: State_V(nVar)

    ! Conservative variables
    real :: StateLeftCons_V(nFlux), StateRightCons_V(nFlux)

    ! Left and right fluxes
    real :: FluxLeft_V(nFlux), FluxRight_V(nFlux)

    ! Left, right and maximum speeds, normal velocity, jump in Bn
    real :: Cleft, Cright, Cmax, Un, DiffBn, CleftAverage, CrightAverage

    real :: AreaInvCdiff, Cproduct, Bn
    !--------------------------------------------------------------------------
    if(DoLf)then
       ! Rusanov scheme

       ! average state
       State_V = 0.5*(StateLeft_V + StateRight_V)

       call get_speed_max(State_V, Normal_D, Un, B0_D, Cmax)
       call get_physical_flux(StateLeft_V, Normal_D, &
            StateLeftCons_V, FluxLeft_V, B0_D)
       call get_physical_flux(StateRight_V, Normal_D, &
            StateRightCons_V, FluxRight_V, B0_D)

       ! Lax-Friedrichs flux
       Flux_V(1:nFlux) = Area*0.5*((FluxLeft_V + FluxRight_V) &
            +                      Cmax*(StateLeftCons_V - StateRightCons_V))

       if(nFluid == 1)then
          if(UseNonConservative) Flux_V(UnFirst_) = Area*0.5* &
               sum((StateLeft_V(Ux_:Uz_) + StateRight_V(Ux_:Uz_))*Normal_D)
       else
          if(UseNonConservative) Flux_V(UnFirst_:UnFirst_+nFluid-1) = &
               Area*0.5* &
               ( (StateLeft_V(iUx_I) + StateRight_V(iUx_I))*Normal_D(1) &
               + (StateLeft_V(iUy_I) + StateRight_V(iUy_I))*Normal_D(2) &
               + (StateLeft_V(iUz_I) + StateRight_V(iUz_I))*Normal_D(3) )
       end if

       ! Store Bnormal
       if(UseB .and. UseDivbSource) Flux_V(Bn_) = 0.5*Area* &
            sum((StateLeft_V(Bx_:Bz_) + StateRight_V(Bx_:Bz_))*Normal_D)

    else
       ! Linde scheme
       if(UseB)then
          ! Sokolov's algorithm
          ! Calculate the jump in the normal magnetic field vector
          DiffBn = &
               0.5*sum(Normal_D*(StateRight_V(Bx_:Bz_) - StateLeft_V(Bx_:Bz_)))

          ! Remove the jump in the normal magnetic field
          StateLeft_V(Bx_:Bz_)  = StateLeft_V(Bx_:Bz_)  + DiffBn*Normal_D
          StateRight_V(Bx_:Bz_) = StateRight_V(Bx_:Bz_) - DiffBn*Normal_D
       end if

       ! This implementation is for non-relativistic MHD only
       ! Left speed of left state
       call get_speed_max(StateLeft_V, Normal_D, Un, B0_D, Cleft=Cleft)

       ! Right speed of right state
       call get_speed_max(StateRight_V, Normal_D, Un, B0_D, Cright=Cright)

       ! Speeds of average state
       State_V = 0.5*(StateLeft_V + StateRight_V)
       call get_speed_max(State_V, Normal_D, &
            Un, B0_D, Cmax, CleftAverage, CrightAverage)

       ! Limited left and right speeds
       Cleft  = min(0.0, Cleft,  CleftAverage)
       Cright = max(0.0, Cright, CrightAverage)

       ! Physical flux
       call get_physical_flux(StateLeft_V, Normal_D, &
            StateLeftCons_V, FluxLeft_V, B0_D)
       call get_physical_flux(StateRight_V, Normal_D, &
            StateRightCons_V, FluxRight_V, B0_D)

       Cproduct     = Cright*Cleft
       AreaInvCdiff = Area/(Cright - Cleft)
       ! HLLE flux
       Flux_V(1:nFlux) = AreaInvCdiff *&
            ( Cright*FluxLeft_V - Cleft*FluxRight_V  &
            + Cproduct*(StateRightCons_V - StateLeftCons_V) )

       if(nFluid == 1)then
          if(UseNonConservative)Flux_V(UnFirst_) = AreaInvCDiff* &
               ( (Cright*StateLeft_V(Ux_) &
               -  Cleft*StateRight_V(Ux_))*Normal_D(1) &
               + (Cright*StateLeft_V(Uy_) &
               -  Cleft*StateRight_V(Uy_))*Normal_D(2) &
               + (Cright*StateLeft_V(Uz_) &
               -  Cleft*StateRight_V(Uz_))*Normal_D(3) )
       else
          if(UseNonConservative) Flux_V(UnFirst_:UnFirst_+nFluid-1) = &
               AreaInvCdiff*  &
               ( (Cright*StateLeft_V(iUx_I)              &
               -  Cleft*StateRight_V(iUx_I))*Normal_D(1) &
               + (Cright*StateLeft_V(iUy_I)              &
               -  Cleft*StateRight_V(iUy_I))*Normal_D(2) &
               + (Cright*StateLeft_V(iUz_I)              &
               -  Cleft*StateRight_V(iUz_I))*Normal_D(3) )
       end if

       if(UseB)then
          if(Hyp_ > 1 .and. UseHyperbolicDivb) then
             ! Overwrite the flux of the Hyp field with the Lax-Friedrichs flux
             Cmax = max(Cmax, SpeedHyp)
             Flux_V(Hyp_) = 0.5*Area*(FluxLeft_V(Hyp_) + FluxRight_V(Hyp_) &
                  - Cmax*(StateRight_V(Hyp_) - StateLeft_V(Hyp_)))
          end if

          ! Linde scheme: use Lax-Friedrichs flux for Bn
          ! The original jump was removed, now we add it with Cmax
          Flux_V(Bx_:Bz_) = Flux_V(Bx_:Bz_) - Area*Cmax*DiffBn*Normal_D

          ! Fix the energy diffusion
          ! The energy jump is also modified by
          ! 1/2(Br^2 - Bl^2) = 1/2(Br-Bl)*(Br+Bl)
          ! Note that BnLeft = BnRight, no need to average
          Bn = sum(Normal_D*StateLeft_V(Bx_:Bz_))
          Flux_V(Energy_) = Flux_V(Energy_) - Area*Cmax*DiffBn*Bn

          ! Store Bnormal
          if(UseDivbSource) Flux_V(Bn_) = Area*Bn

       end if
    end if

    ! Store time step constraint (to be generalized for multifluid)
    Flux_V(Vdt_) = abs(Area)*Cmax

  end subroutine get_numerical_flux
  !============================================================================
  subroutine energy_to_pressure(State_V)
    !$acc routine seq

    ! Calculate pressure from energy density
    real, intent(inout):: State_V(nVar)
    !--------------------------------------------------------------------------

    ! Subtract magnetic energy from the first fluid for MHD
    if(IsMhd) State_V(p_) = State_V(p_) -  0.5*sum(State_V(Bx_:Bz_)**2)

    ! Convert hydro energy density to pressure
    State_V(iP_I) = GammaMinus1_I*( State_V(iP_I) - 0.5* &
         ( State_V(iRhoUx_I)**2 &
         + State_V(iRhoUy_I)**2 &
         + State_V(iRhoUz_I)**2 ) / State_V(iRho_I) )

  end subroutine energy_to_pressure
  !============================================================================
  subroutine pressure_to_energy(State_V)
    !$acc routine seq

    ! Calculate energy density from pressure
    real, intent(inout):: State_V(nVar)
    !--------------------------------------------------------------------------

    ! Calculy hydro energy density
    State_V(iP_I) = State_V(iP_I)*InvGammaMinus1_I + 0.5* &
         ( State_V(iRhoUx_I)**2 &
         + State_V(iRhoUy_I)**2 &
         + State_V(iRhoUz_I)**2 ) / State_V(iRho_I)

    ! Add magnetic energy to first fluid for MHD
    if(IsMhd) State_V(p_) = State_V(p_) + 0.5*sum(State_V(Bx_:Bz_)**2)

  end subroutine pressure_to_energy
  !============================================================================
  subroutine get_primitive(State_V, Primitive_V)
    !$acc routine seq

    real, intent(in) :: State_V(nVar)
    real, intent(out):: Primitive_V(nVar)
    !--------------------------------------------------------------------------
    Primitive_V(1:Ux_-1)    = State_V(1:RhoUx_-1)
    Primitive_V(Ux_:Uz_)    = State_V(RhoUx_:RhoUz_)/State_V(Rho_)
    Primitive_V(Uz_+1:nVar) = State_V(RhoUz_+1:nVar)

  end subroutine get_primitive
  !============================================================================
  subroutine limiter2(Var1, Var2, Var3, Var4, VarLeft, VarRight)
    !$acc routine seq

    ! Second order limiter on a 4 point stencil
    real, intent(in) :: Var1, Var2, Var3, Var4  ! cell center values at i=1..4
    real, intent(out):: VarLeft, VarRight       ! face values at i=2.5

    real, parameter:: cThird = 1./3.
    real:: Slope21, Slope32, Slope43
    !--------------------------------------------------------------------------
    Slope21 = LimiterBeta*(Var2 - Var1)
    Slope32 = LimiterBeta*(Var3 - Var2)
    Slope43 = LimiterBeta*(Var4 - Var3)

    VarLeft  = Var2 + (sign(0.25,Slope32) + sign(0.25,Slope21))*&
         min(abs(Slope32), abs(Slope21), cThird*abs(2*Var3-Var1-Var2))
    VarRight = Var3 - (sign(0.25,Slope32) + sign(0.25,Slope43))*&
         min(abs(Slope32), abs(Slope43), cThird*abs(Var3+Var4-2*Var2))

  end subroutine limiter2
  !============================================================================
  subroutine update_state_cpu_prim

    ! optimal for CPU (face value and face flux calculated only once)

    integer:: i, j, k, iBlock, iGang, iFluid, iP, iUn

    real:: DivU, DivB, InvRho, DtPerDv, Change_V(nFlux)
    !$acc declare create (Change_V)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'update_state_cpu_prim'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    !$acc parallel
    !$acc loop gang private(iGang) independent
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE

#ifdef OPENACC
       iGang = iBlock
#else
       iGang = 1
#endif

       if(iStage == 1 .and. nStage == 2) call set_old_state(iBlock)

       ! Calculate the primitive variables
       !$acc loop vector collapse(3) independent
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          call get_primitive(State_VGB(:,i,j,k,iBlock), Primitive_VGI(:,i,j,k,iGang))
       end do; end do; end do

       !$acc loop vector collapse(3) independent
       do k = 1, nK; do j = 1, nJ; do i = 1, nI+1

          ! DoTestCell = DoTest .and. (i == iTest .or. i == iTest+1) .and. &
          !     j == jTest .and. k == kTest .and. iBlock == iBlockTest

          call get_flux_x_prim(i, j, k, iBlock, Flux_VXI(:,i,j,k,iGang))

       end do; end do; end do

       if(nDim > 1)then
          !$acc loop vector collapse(3) independent
          do k = 1, nK; do j = 1, nJ+1; do i = 1, nI

             ! DoTestCell = DoTest .and. iBlock==iBlockTest .and. i==iTest &
             !     .and. (j==jTest .or. j==jTest+1) .and. k==kTest

             call get_flux_y_prim(i, j, k, iBlock, Flux_VYI(:,i,j,k,iGang))

          end do; end do; end do
       end if

       if(nDim > 2)then
          !$acc loop vector collapse(3) independent
          do k = 1, nK+1; do j = 1, nJ; do i = 1, nI

             ! DoTestCell = DoTest .and. iBlock==iBlockTest .and. i==iTest &
             !     .and. j==jTest .and. (k==kTest .or. k==kTest+1)

             call get_flux_z_prim(i, j, k, iBlock, Flux_VZI(:,i,j,k,iGang))

          end do; end do; end do
       end if

       ! Update
       !$acc loop vector collapse(3) private(Change_V, DtPerDv) independent
       do k = 1, nK; do j = 1, nJ; do i = 1, nI

          Change_V =  Flux_VXI(1:nFlux,i,j,k,iGang) &
               -      Flux_VXI(1:nFlux,i+1,j,k,iGang)
          if(nDim > 1) Change_V = Change_V + Flux_VYI(1:nFlux,i,j,k,iGang) &
               -                             Flux_VYI(1:nFlux,i,j+1,k,iGang)
          if(nDim > 2) Change_V = Change_V + Flux_VZI(1:nFlux,i,j,k,iGang) &
               -                             Flux_VZI(1:nFlux,i,j,k+1,iGang)

          if(UseB .and. UseDivbSource)then
             DivB = Flux_VXI(Bn_,i+1,j,k,iGang) - Flux_VXI(Bn_,i,j,k,iGang)
             if(nJ > 1) DivB = DivB + &
                  Flux_VYI(Bn_,i,j+1,k,iGang) - Flux_VYI(Bn_,i,j,k,iGang)
             if(nK > 1) DivB = DivB + &
                  Flux_VZI(Bn_,i,j,k+1,iGang) - Flux_VZI(Bn_,i,j,k,iGang)
             InvRho = 1/State_VGB(Rho_,i,j,k,iBlock)
             Change_V(RhoUx_:RhoUz_) = Change_V(RhoUx_:RhoUz_) - &
                  DivB*State_VGB(Bx_:Bz_,i,j,k,iBlock)
             Change_V(Bx_:Bz_) = Change_V(Bx_:Bz_) - &
                  DivB*InvRho*State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)
             Change_V(Energy_) = Change_V(Energy_) - &
                  DivB*InvRho*sum(State_VGB(Bx_:Bz_,i,j,k,iBlock) &
                  *               State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock))
          end if

          if(UseNonConservative)then
             ! Add -(g-1)*p*div(u) source term
             do iFluid = 1, nFluid
                iP = iP_I(iFluid)
                iUn = UnFirst_ + iFluid - 1
                DivU = Flux_VXI(iUn,i+1,j,k,iGang) - Flux_VXI(iUn,i,j,k,iGang)
                if(nJ > 1) DivU = DivU &
                     + Flux_VYI(iUn,i,j+1,k,iGang) - Flux_VYI(iUn,i,j,k,iGang)
                if(nK > 1) DivU = DivU &
                     + Flux_VZI(iUn,i,j,k+1,iGang) - Flux_VZI(iUn,i,j,k,iGang)
                Change_V(iP) = Change_V(iP) &
                     - GammaMinus1_I(iFluid)*State_VGB(iP,i,j,k,iBlock)*DivU
             end do
          end if

          ! Time step divided by cell volume
          if(IsTimeAccurate)then
             DtPerDv = iStage*Dt
          else
             DtPerDv = iStage*Cfl*DtMax_CB(i,j,k,iBlock)
          end if
          if(IsCartesian)then
             DtPerDv = DtPerDv/(nStage*CellVolume_B(iBlock))
          else
             DtPerDv = DtPerDv/(nStage*CellVolume_GB(i,j,k,iBlock))
          end if

          ! Update state
          if(iStage == 1)then
             if(.not.UseNonConservative)then
                ! Overwrite pressure and change with energy
                call pressure_to_energy(State_VGB(:,i,j,k,iBlock))
                Change_V(iP_I) = Change_V(Energy_:)
             end if
             State_VGB(:,i,j,k,iBlock) = State_VGB(:,i,j,k,iBlock) &
                  + DtPerDv*Change_V(1:nVar)
          else
             if(.not.UseNonConservative)then
                ! Overwrite old pressure and change with energy
                call pressure_to_energy(StateOld_VGB(:,i,j,k,iBlock))
                Change_V(iP_I) = Change_V(Energy_:)
             end if
             State_VGB(:,i,j,k,iBlock) = StateOld_VGB(:,i,j,k,iBlock) &
                  + DtPerDv*Change_V(1:nVar)
          end if
          ! Convert energy back to pressure
          if(.not.UseNonConservative) &
               call energy_to_pressure(State_VGB(:,i,j,k,iBlock))

#ifndef OPENACC
          DoTestCell = DoTest .and. i==iTest .and. j==jTest .and. k==kTest &
               .and. iBlock == iBlockTest
          if(DoTestCell)then
             write(*,*)'State_VGB =', State_VGB(:,i,j,k,iBlock)
             write(*,*)'Change_V  =', Change_V
             write(*,*)'DtPerDv   =', DtPerDv
          end if
#endif
       enddo; enddo; enddo

       if(.not.UseDtFixed) call calc_timestep(iBlock)

    end do
    !$acc end parallel

    call test_stop(NameSub, DoTest, iBlock)

  end subroutine update_state_cpu_prim
  !============================================================================
  subroutine get_flux_x_prim(i, j,  k, iBlock, Flux_V)
    !$acc routine seq

    integer, intent(in) :: i, j, k, iBlock
    real,    intent(out):: Flux_V(nFaceValue)

    real :: Area, Normal_D(3), B0_D(3)
    real :: StateLeft_V(nVar), StateRight_V(nVar)
    !--------------------------------------------------------------------------
    call get_normal(1, i, j, k, iBlock, Normal_D, Area)

    call get_face_x_prim(i, j, k, iBlock, StateLeft_V, StateRight_V)

    B0_D = 0
    if(UseB0) B0_D = B0_DXB(:,i,j,k,iBlock)

    call get_numerical_flux(Normal_D, Area, &
         StateLeft_V, StateRight_V, Flux_V, B0_D)

  end subroutine get_flux_x_prim
  !============================================================================
  subroutine get_flux_y_prim(i, j, k, iBlock, Flux_V)
    !$acc routine seq

    integer, intent(in) :: i, j, k, iBlock
    real,    intent(out):: Flux_V(nFaceValue)

    real :: Area, Normal_D(3), B0_D(3)
    real :: StateLeft_V(nVar), StateRight_V(nVar)
    !--------------------------------------------------------------------------
    call get_normal(2, i, j, k, iBlock, Normal_D, Area)

    call get_face_y_prim(i, j, k, iBlock, StateLeft_V, StateRight_V)

    B0_D = 0
    if(UseB0) B0_D = B0_DYB(:,i,j,k,iBlock)

    call get_numerical_flux(Normal_D, Area, &
         StateLeft_V, StateRight_V, Flux_V, B0_D)

  end subroutine get_flux_y_prim
  !============================================================================
  subroutine get_flux_z_prim(i, j, k, iBlock, Flux_V)
    !$acc routine seq

    integer, intent(in) :: i, j, k, iBlock
    real,    intent(out):: Flux_V(nFaceValue)

    real :: Area, Normal_D(3), B0_D(3)
    real :: StateLeft_V(nVar), StateRight_V(nVar)
    !--------------------------------------------------------------------------
    call get_normal(3, i, j, k, iBlock, Normal_D, Area)

    call get_face_z_prim(i, j, k, iBlock, StateLeft_V, StateRight_V)

    B0_D = 0
    if(UseB0) B0_D = B0_DZB(:,i,j,k,iBlock)

    call get_numerical_flux(Normal_D, Area, &
         StateLeft_V, StateRight_V, Flux_V, B0_D)

  end subroutine get_flux_z_prim
  !============================================================================
  subroutine update_state_gpu_prim

    ! optimal for GPU, store primitive variables

    integer:: i, j, k, iBlock, iGang

    real:: Change_V(nFlux), DtPerDv
    !$acc declare create (Change_V)

#ifndef OPENACC
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'update_state_gpu_prim'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
#endif

    !$acc parallel
    !$acc loop gang independent
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE

#ifdef OPENACC
       iGang = iBlock
#else
       iGang = 1
#endif
       if(iStage == 1 .and. nStage == 2) call set_old_state(iBlock)

       !$acc loop vector collapse(3) independent
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          call get_primitive(State_VGB(:,i,j,k,iBlock), Primitive_VGI(:,i,j,k,iGang))
       end do; end do; end do

       !$acc loop vector collapse(3) private(Change_V, DtPerDv) independent
       do k = 1, nK; do j = 1, nJ; do i = 1, nI

          ! DoTestCell = DoTest .and. i==iTest .and. j==jTest .and. k==kTest &
          !     .and. iBlock == iBlockTest

          ! Initialize change in State_VGB
          Change_V = 0.0

          call do_face_prim(1, i, j, k, iBlock, Change_V)
          call do_face_prim(2, i, j, k, iBlock, Change_V)

          if(nDim > 1)then
             call do_face_prim(3, i, j, k, iBlock, Change_V)
             call do_face_prim(4, i, j, k, iBlock, Change_V)
          end if

          if(nDim > 2)then
             call do_face_prim(5, i, j, k, iBlock, Change_V)
             call do_face_prim(6, i, j, k, iBlock, Change_V)
          end if

          ! Time step divided by cell volume
          if(IsTimeAccurate)then
             DtPerDv = iStage*Dt
          else
             DtPerDv = iStage*Cfl*DtMax_CB(i,j,k,iBlock)
          end if
          if(IsCartesian)then
             DtPerDv = DtPerDv/(nStage*CellVolume_B(iBlock))
          else
             DtPerDv = DtPerDv/(nStage*CellVolume_GB(i,j,k,iBlock))
          end if

          ! Update state
          if(iStage == 1)then
             if(.not.UseNonConservative)then
                ! Overwrite pressure and change with energy
                call pressure_to_energy(State_VGB(:,i,j,k,iBlock))
                Change_V(iP_I) = Change_V(Energy_:)
             end if
             State_VGB(:,i,j,k,iBlock) = State_VGB(:,i,j,k,iBlock) &
                  + DtPerDv*Change_V(1:nVar)
          else
             if(.not.UseNonConservative)then
                ! Overwrite old pressure and change with energy
                call pressure_to_energy(StateOld_VGB(:,i,j,k,iBlock))
                Change_V(iP_I) = Change_V(Energy_:)
             end if
             State_VGB(:,i,j,k,iBlock) = StateOld_VGB(:,i,j,k,iBlock) &
                  + DtPerDv*Change_V(1:nVar)
          end if
          ! Convert energy back to pressure
          if(.not.UseNonConservative) &
               call energy_to_pressure(State_VGB(:,i,j,k,iBlock))

#ifndef OPENACC
          ! DoTestCell = DoTest .and. i==iTest .and. j==jTest .and. k==kTest &
          !     .and. iBlock == iBlockTest
          ! if(DoTestCell)then
          !   write(*,*)'State_VGB =', State_VGB(:,i,j,k,iBlock)
          !   write(*,*)'Change_V  =', Change_V
          !   write(*,*)'DtPerDv   =', DtPerDv
          ! end if
#endif
       enddo; enddo; enddo

       ! Flux_V is a local variable so generic calc_timestep does not work
       ! call calc_timestep(iBlock)

    end do
    !$acc end parallel

#ifndef OPENACC
    call test_stop(NameSub, DoTest, iBlock)
#endif
  end subroutine update_state_gpu_prim
  !============================================================================
  subroutine do_face_prim(iFace, i, j, k, iBlock, Change_V)
    !$acc routine seq

    integer, intent(in):: iFace, i, j, k, iBlock
    real, intent(inout):: Change_V(nFlux)

    real:: Area, Normal_D(3), InvRho, B0_D(3)
    real:: StateLeft_V(nVar), StateRight_V(nVar), Flux_V(nFaceValue)
    !--------------------------------------------------------------------------
    B0_D = 0
    select case(iFace)
    case(1)
       call get_normal(1, i, j, k, iBlock, Normal_D, Area)
       call get_face_x_prim(i, j, k, iBlock, StateLeft_V, StateRight_V)
       if(UseB0) B0_D = B0_DXB(:,i,j,k,iBlock)
    case(2)
       call get_normal(1, i+1, j, k, iBlock, Normal_D, Area)
       Area = -Area
       call get_face_x_prim(i+1, j, k, iBlock, StateLeft_V, StateRight_V)
       if(UseB0) B0_D = B0_DXB(:,i+1,j,k,iBlock)
    case(3)
       call get_normal(2, i, j, k, iBlock, Normal_D, Area)
       call get_face_y_prim(i, j, k, iBlock, StateLeft_V, StateRight_V)
       if(UseB0) B0_D = B0_DYB(:,i,j,k,iBlock)
    case(4)
       call get_normal(2, i, j+1, k, iBlock, Normal_D, Area)
       Area = -Area
       call get_face_y_prim(i, j+1, k, iBlock, StateLeft_V, StateRight_V)
       if(UseB0) B0_D = B0_DYB(:,i,j+1,k,iBlock)
    case(5)
       call get_normal(3, i, j, k, iBlock, Normal_D, Area)
       call get_face_z_prim(   i, j, k, iBlock, StateLeft_V, StateRight_V)
       if(UseB0) B0_D = B0_DZB(:,i,j,k,iBlock)
    case(6)
       call get_normal(3, i, j, k+1, iBlock, Normal_D, Area)
       Area = -Area
       call get_face_z_prim(i, j, k+1, iBlock, StateLeft_V, StateRight_V)
       if(UseB0) B0_D = B0_DZB(:,i,j,k+1,iBlock)
    end select

    call get_numerical_flux(Normal_D, Area, StateLeft_V, StateRight_V, &
         Flux_V, B0_D)

    ! Change due to fluxes through this face
    Change_V = Change_V + Flux_V(1:nFlux)

    ! Change due to -(gama-1)*divU source terms
    if(nFluid == 1)then
       if(UseNonConservative) Change_V(p_) = Change_V(p_) &
            + GammaMinus1*State_VGB(p_,i,j,k,iBlock)*Flux_V(UnFirst_)
    else
       if(UseNonConservative) Change_V(iP_I) = Change_V(iP_I) &
            + GammaMinus1_I*State_VGB(iP_I,i,j,k,iBlock)&
            *Flux_V(UnFirst_:UnFirst_+nFluid-1)
    end if

    ! Change due to 8-wave source terms
    if(UseB .and. UseDivbSource)then
       InvRho = 1/State_VGB(Rho_,i,j,k,iBlock)
       Change_V(RhoUx_:RhoUz_) = Change_V(RhoUx_:RhoUz_) &
            + Flux_V(Bn_)*State_VGB(Bx_:Bz_,i,j,k,iBlock)
       Change_V(Bx_:Bz_) = Change_V(Bx_:Bz_) &
            + Flux_V(Bn_)*InvRho*State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)
       Change_V(Energy_) = Change_V(Energy_) &
            + Flux_V(Bn_)*InvRho*sum(State_VGB(Bx_:Bz_,i,j,k,iBlock) &
            *                        State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock))
    end if

  end subroutine do_face_prim
  !============================================================================
  subroutine get_face_x_prim(i, j, k, iBlock, StateLeft_V, StateRight_V)
    !$acc routine seq

    integer, intent(in) :: i, j, k, iBlock
    real,    intent(out):: StateLeft_V(nVar), StateRight_V(nVar)

    integer:: iVar, iGang
    !--------------------------------------------------------------------------
#ifdef OPENACC
    iGang = iBlock
#else
    iGang = 1
#endif

    if(nOrder == 1)then
       StateLeft_V  = Primitive_VGI(:,i-1,j,k,iGang)
       StateRight_V = Primitive_VGI(:,i  ,j,k,iGang)
    else
       ! Do it per variable to reduce memory use
       do iVar = 1, nVar
          ! Single fluid conversion to primitive variables
          call limiter2( &
               Primitive_VGI(iVar,i-2,j,k,iGang), &
               Primitive_VGI(iVar,i-1,j,k,iGang), &
               Primitive_VGI(iVar,  i,j,k,iGang), &
               Primitive_VGI(iVar,i+1,j,k,iGang), &
               StateLeft_V(iVar), StateRight_V(iVar))
       end do
    end if

  end subroutine get_face_x_prim
  !============================================================================
  subroutine get_face_y_prim(i, j, k, iBlock, StateLeft_V, StateRight_V)
    !$acc routine seq

    integer, intent(in) :: i, j, k, iBlock
    real,    intent(out):: StateLeft_V(nVar), StateRight_V(nVar)

    integer:: iVar, iGang
    !--------------------------------------------------------------------------
#ifdef OPENACC
    iGang = iBlock
#else
    iGang = 1
#endif

    if(nOrder == 1)then
       StateLeft_V  = Primitive_VGI(:,i,j-1,k,iGang)
       StateRight_V = Primitive_VGI(:,i,j  ,k,iGang)
    else
       ! Do it per variable to reduce memory use
       do iVar = 1, nVar
          ! Single fluid conversion to primitive variables
          call limiter2( &
               Primitive_VGI(iVar,i,j-2,k,iGang), &
               Primitive_VGI(iVar,i,j-1,k,iGang), &
               Primitive_VGI(iVar,i,j  ,k,iGang), &
               Primitive_VGI(iVar,i,j+1,k,iGang), &
               StateLeft_V(iVar), StateRight_V(iVar))
       end do
    end if

  end subroutine get_face_y_prim
  !============================================================================
  subroutine get_face_z_prim(i, j, k, iBlock, StateLeft_V, StateRight_V)
    !$acc routine seq

    integer, intent(in) :: i, j, k, iBlock
    real,    intent(out):: StateLeft_V(nVar), StateRight_V(nVar)

    integer:: iVar, iGang
    !--------------------------------------------------------------------------
#ifdef OPENACC
    iGang = iBlock
#else
    iGang = 1
#endif

    if(nOrder == 1)then
       StateLeft_V   = Primitive_VGI(:,i,j,k-1,iGang)
       StateRight_V  = Primitive_VGI(:,i,j,k  ,iGang)
    else
       ! Do it per variable to reduce memory use
       do iVar = 1, nVar
          ! Single fluid conversion to primitive variables
          call limiter2( &
               Primitive_VGI(iVar,i,j,k-2,iGang), &
               Primitive_VGI(iVar,i,j,k-1,iGang), &
               Primitive_VGI(iVar,i,j,k  ,iGang), &
               Primitive_VGI(iVar,i,j,k+1,iGang), &
               StateLeft_V(iVar), StateRight_V(iVar))
       end do
    end if

  end subroutine get_face_z_prim
  !============================================================================
end module ModUpdateStateFast
!==============================================================================
