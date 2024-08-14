!  Copyright (C) 2001 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModUpdateStateFast

  ! Calculate each face twice

  use ModOptimizeParam, ONLY: &
       DoLf, LimiterBeta, nStage, iStage, nOrder, &
       IsCartesian, IsCartesianGrid, UseNonConservative, nConservCrit, &
       UseDivbSource, UseHyperbolicDivB, IsTimeAccurate, UseDtFixed, UseB0, &
       UseBody, UseBorisCorrection, ClightFactor, UseRhoMin, UsePMin, &
       UseElectronEntropy
  use ModFaceBoundary, ONLY: B1rCoef
  use ModVarIndexes
  use ModMultiFluid, ONLY: iUx_I, iUy_I, iUz_I, iP_I, iRhoIon_I, nIonFluid, &
       ChargePerMass_I
  use ModAdvance, ONLY: nFlux, State_VGB, StateOld_VGB, &
       Flux_VXI, Flux_VYI, Flux_VZI, &
       nFaceValue, UnFirst_, UnLast_, Bn_ => BnL_, En_ => BnR_, &
       DtMax_CB, Vdt_, iTypeUpdate, UpdateFast_, UseRotatingFrame, &
       UseElectronPressure
  use ModCellBoundary, ONLY: FloatBC_, VaryBC_, InFlowBC_, FixedBC_
  use ModConservative, ONLY: IsConserv_CB
  use BATL_lib, ONLY: nDim, nI, nJ, nK, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, &
       nBlock, Unused_B, x_, y_, z_, CellVolume_B, CellFace_DB, &
       CellVolume_GB, CellFace_DFB, FaceNormal_DDFB, Xyz_DGB, Used_GB, &
       Unset_, test_start, test_stop, iTest, jTest, kTest, iBlockTest, &
       iVarTest, iDimTest, iSideTest
  use ModParallel, ONLY: DiLevel_EB
  use ModPhysics, ONLY: Gamma, GammaMinus1, InvGammaMinus1, &
       GammaMinus1_I, InvGammaMinus1_I, FaceState_VI, CellState_VI, &
       C2light, InvClight, InvClight2, RhoMin_I, pMin_I, &
       OmegaBody_D, set_dipole, Gbody, OmegaBody, GammaWave, &
       GammaElectronMinus1, GammaElectron
  use ModMain, ONLY: Dt, DtMax_B, Cfl, tSimulation, &
       iTypeCellBc_I, body1_, UseRotatingBc, UseB, SpeedHyp, UseIe, &
       UseGravity, nStep
  use ModImplicit, ONLY: iVarSemiMin, iVarSemiMax
#ifdef _OPENACC
  use ModMain, ONLY: nStep
#endif
  use ModB0, ONLY: B0_DGB, get_b0_dipole
  use ModNumConst, ONLY: cUnit_DD
  use ModTimeStepControl, ONLY: calc_timestep
  use ModGeometry, ONLY: IsBody_B, IsNoBody_B, IsBoundary_B, xMaxBox, r_GB
  use ModSolarWind, ONLY: get_solar_wind_point
  use ModIeCoupling, ONLY: UseCpcpBc, RhoCpcp_I
  use ModWaves, ONLY: AlfvenPlusFirst_, AlfvenPlusLast_, AlfvenMinusFirst_, &
       AlfvenMinusLast_
  use ModBatsrusUtility, ONLY: stop_mpi

  implicit none

  private ! except

  public:: sync_cpu_gpu          ! Synchronize variables between CPU and GPU
  public:: update_state_fast     ! Fast update of State_VGB
  public:: update_b0_fast        ! Fast update of B0
  public:: set_boundary_fast     ! set cell based boundary for State_VGB
  public:: set_cell_boundary_for_block ! set cell bounary for a block

  logical, parameter:: UseAlfvenWaves  = WaveFirst_ > 1

  logical:: DoTestUpdate, DoTestFlux, DoTestSource, DoTestAny
  !$acc declare create (DoTestUpdate, DoTestFlux, DoTestSource, DoTestAny)

contains
  !============================================================================
  include 'vector_functions.h'
  subroutine sync_cpu_gpu(String, NameCaller, State_VGB, B0_DGB, Trace_DICB)

    character(len=*), intent(in):: String
    character(len=*), optional, intent(in):: NameCaller
    real, optional:: State_VGB(:,:,:,:,:)
    real, optional:: B0_DGB(:,:,:,:,:)
    real, optional:: Trace_DICB(:,:,:,:,:,:)

    ! Ensure that variables are in sync between CPU and GPU.
    ! Only perform the acc update if the status index of a variable
    ! is larger on the source than the targer device (DiVAR is 1 or -1).

    ! By default update the variables passed from the CPU to the GPU.
    ! If string contains "CPU", then update from GPU to CPU
    ! If string contains "change", then set DiVar=-1 for GPU or DiVar=1 for CPU
    !
    ! Examples:
    ! call sync_cpu_gpu("update on GPU", NameSub, State_VGB, B0_DGB)
    ! call sync_cpu_gpu("update on CPU", NameSub, B0_DGB=B0_DGB)
    ! call sync_cpu_gpu("change on GPU", Trace_DICB=Trace_DICB)
    !
    ! The optional NameCaller string is used for testing purposes

    integer:: DiState=0, DiB0 = 0, DiTrace = 0
    logical:: DoChange, IsCpu

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'sync_cpu_gpu'
    !--------------------------------------------------------------------------
    if(iTypeUpdate < UpdateFast_) RETURN

    call test_start(NameSub, DoTest)

    DoChange = index(String, 'change') > 0
    IsCpu    = index(String, 'CPU') > 0

    if(DoTest)then
       write(*,*) NameSub,': String, DoChange, IsCpu, DiState=', &
            String, DoChange, IsCpu, DiState
       if(present(NameCaller)) write(*,*) NameSub,' called by ',NameCaller
    end if

    if(present(State_VGB))then
       if(DoChange)then
          if(IsCpu)then
             DiState = 1
          else
             DiState = -1
          endif
       else
          if(IsCpu .and. DiState < 0)then
             if(DoTest)write(*,*) NameSub,': acc update State to CPU'
             call timing_start("sync_state")
             !$acc update host (State_VGB)
             call timing_stop("sync_state")
             DiState = 0
          elseif(.not.IsCpu .and. DiState > 0)then
             call timing_start("sync_state")
             if(DoTest)write(*,*) NameSub,': acc update State to GPU'
             !$acc update device(State_VGB)
             call timing_stop("sync_state")
             DiState = 0
          endif
       endif
    endif
    if(UseB0 .and. present(B0_DGB)) then
       if(DoChange)then
          if(IsCpu)then
             DiB0 = 1
          else
             DiB0 = -1
          endif
       else
          if(IsCpu .and. DiB0 < 0)then
             if(DoTest)write(*,*) NameSub,': acc update B0 to CPU'
             call timing_start("sync_b0")
             !$acc update host (B0_DGB)
             call timing_stop("sync_b0")
             DiB0 = 0
          elseif(.not.IsCpu .and. DiB0 > 0)then
             if(DoTest)write(*,*) NameSub,': acc update B0 to GPU'
             call timing_start("sync_b0")
             !$acc update device(B0_DGB)
             call timing_stop("sync_b0")
             DiB0 = 0
          endif
       endif
    endif
    if(present(Trace_DICB))then
       if(DoChange)then
          if(IsCpu)then
             DiTrace = 1
          else
             DiTrace = -1
          endif
       else
          if(IsCpu .and. DiTrace < 0)then
             if(DoTest)write(*,*) NameSub,': acc update Trace to CPU'
             call timing_start("sync_trace")
             !$acc update host (Trace_DICB)
             call timing_stop("sync_trace")
             DiTrace = 0
          elseif(.not.IsCpu .and. DiTrace > 0)then
             if(DoTest)write(*,*) NameSub,': acc update Trace to GPU'
             call timing_start("sync_trace")
             !$acc update device(Trace_DICB)
             call timing_stop("sync_trace")
             DiTrace = 0
          endif
       endif

    end if
    if(DoTest)write(*,*) NameSub,': DiState, DiB0, DiTrace=', &
         DiState, DiB0, DiTrace

    call test_stop(NameSub, DoTest)

  end subroutine sync_cpu_gpu
  !============================================================================
  subroutine update_state_fast

    ! Apply cell boundary conditions and update one stage
    integer:: i, j, k, iBlock, iGang, iVar
    logical:: IsBodyBlock

    character(len=*), parameter:: NameSub = 'update_state_fast'
    !--------------------------------------------------------------------------
    call timing_start(NameSub)

#ifndef SCALAR
    call test_start('update_state',   DoTestUpdate)
    call test_start('calc_face_flux', DoTestFlux)
    call test_start('calc_source',    DoTestSource)

    DoTestAny = DoTestUpdate .or. DoTestFlux .or. DoTestSource

    !$acc update device(DoTestUpdate, DoTestFlux, DoTestSource, DoTestAny)
    !$acc update device(nStep)

    call sync_cpu_gpu('update on GPU', NameSub, State_VGB, B0_DGB)
    call sync_cpu_gpu('change on GPU', NameSub, State_VGB)

    if(DoTestAny)then
       write(*,*)'==========================================================='
       write(*,*) NameSub, ' started with DoResChangeOnly=F of course'
    end if

    !$acc parallel
    !$acc loop gang private(iGang, IsBodyBlock) independent
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE

#ifdef _OPENACC
       iGang = iBlock
#else
       iGang = 1
#endif

       if(iStage == 1 .and. nStage == 2) call set_old_state(iBlock)

       if(UseBody) IsBodyBlock = IsBody_B(iBlock)

       if(UseBody .and. IsBodyBlock)then
          !$acc loop vector collapse(3) independent
          do k = 1, nK; do j = 1, nJ; do i = 1, nI+1

             if(  .not. Used_GB(i-1,j,k,iBlock) .and. &
                  .not. Used_GB(i,j,k,iBlock)) then
                Flux_VXI(UnFirst_:Vdt_,i,j,k,iGang) = 0.0
             else
                call get_flux_x(i, j, k, iBlock, IsBodyBlock)
             end if
          end do; end do; end do
          if(nDim > 1)then
             !$acc loop vector collapse(3) independent
             do k = 1, nK; do j = 1, nJ+1; do i = 1, nI
                if(  .not. Used_GB(i,j-1,k,iBlock) .and. &
                     .not. Used_GB(i,j,k,iBlock)) then
                   Flux_VYI(UnFirst_:Vdt_,i,j,k,iGang) = 0.0
                else
                   call get_flux_y(i, j, k, iBlock, IsBodyBlock)
                end if
             end do; end do; end do
          end if
          if(nDim > 2)then
             !$acc loop vector collapse(3) independent
             do k = 1, nK+1; do j = 1, nJ; do i = 1, nI
                if(  .not. Used_GB(i,j,k-1,iBlock) .and. &
                     .not. Used_GB(i,j,k,iBlock)) then
                   Flux_VZI(UnFirst_:Vdt_,i,j,k,iGang) = 0.0
                else
                   call get_flux_z(i, j, k, iBlock, IsBodyBlock)
                end if
             end do; end do; end do
          end if
       else ! Not a body block
          !$acc loop vector collapse(3) independent
          do k = 1, nK; do j = 1, nJ; do i = 1, nI+1
             call get_flux_x(i, j, k, iBlock)
          end do; end do; end do
          if(nDim > 1)then
             !$acc loop vector collapse(3) independent
             do k = 1, nK; do j = 1, nJ+1; do i = 1, nI
                call get_flux_y(i, j, k, iBlock)
             end do; end do; end do
          end if
          if(nDim > 2)then
             !$acc loop vector collapse(3) independent
             do k = 1, nK+1; do j = 1, nJ; do i = 1, nI
                call get_flux_z(i, j, k, iBlock)
             end do; end do; end do
          end if
       end if
       if(.not.IsTimeAccurate .and. iStage==1) call calc_timestep(iBlock)

       ! Update
#ifdef TESTACC
       if(DoTestUpdate .and. iBlock==iBlockTest)then
          write(*,*)NameSub,' nStep=', nStep,' iStage=', iStage,     &
               ' dt=',DtMax_CB(iTest,jTest,kTest,iBlock)*Cfl
          if(nConservCrit > 0) write(*,*)NameSub,' IsConserv=', &
               IsConserv_CB(iTest,jTest,kTest,iBlock)
          do iVar = 1, nVar
             write(*,*)NameVar_V(iVar), '(TestCell)  =',&
                  State_VGB(iVar,iTest,jTest,kTest,iBlockTest)
          end do
          ! switch on to test first ghost cell in the i direction
          ! do iVar=1,nVar
          !    write(*,*)' ',NameVar_V(iVar), '(RightCell)  =',&
          !         State_VGB(iVar,iTest+1,jTest,kTest,iBlockTest)
          ! end do
          ! do iVar=1,nVar
          !    write(*,*)' ',NameVar_V(iVar), '(LeftCell)  =',&
          !         State_VGB(iVar,iTest-1,jTest,kTest,iBlockTest)
          ! end do
       end if
#endif

       !$acc loop vector collapse(3) independent
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          call update_cell(i, j, k, iBlock, iGang, IsBodyBlock)
       enddo; enddo; enddo

       if(IsTimeAccurate .and. .not.UseDtFixed .and. iStage==nStage) &
            call calc_timestep(iBlock)

    end do
    !$acc end parallel

    if(DoTestAny)write(*,*) &
         '==========================================================='
#endif
    call timing_stop(NameSub)

  end subroutine update_state_fast
  !============================================================================
#ifndef SCALAR
  subroutine update_cell(i, j, k, iBlock, iGang, IsBodyBlock)
    !$acc routine seq

    ! compute source terms and update cell

    integer, intent(in):: i, j, k, iBlock, iGang
    logical, intent(in):: IsBodyBlock

    integer:: iFluid, iP, iUn, iUx, iUy, iUz, iRho, iEnergy, iVar
    real:: DivU, DivB, DivE, DivF, DtLocal, Change_V(nFlux), ForcePerRho_D(3)

    logical:: IsConserv

    character(len=*), parameter:: NameSub = 'update_cell'
    !--------------------------------------------------------------------------
    if(UseBody .and. IsBodyBlock) then
       if(.not. Used_GB(i,j,k,iBlock)) RETURN
    end if

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
       Change_V(RhoUx_:RhoUz_) = Change_V(RhoUx_:RhoUz_) &
            - DivB*State_VGB(Bx_:Bz_,i,j,k,iBlock)
       if(UseB0) Change_V(RhoUx_:RhoUz_) = Change_V(RhoUx_:RhoUz_) &
            - DivB*B0_DGB(:,i,j,k,iBlock)

       ! Divide by density to account for Rho in momentum
       DivB = DivB/State_VGB(Rho_,i,j,k,iBlock)
       Change_V(Bx_:Bz_) = Change_V(Bx_:Bz_) &
            - DivB*State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)
       Change_V(Energy_) = Change_V(Energy_) &
            - DivB*sum(State_VGB(Bx_:Bz_,i,j,k,iBlock) &
            *          State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock))
    end if

    if(UseBorisCorrection .and. ClightFactor /= 1.0)then
       ! Calculate Boris source term
       DivE = Flux_VXI(En_,i+1,j,k,iGang) - Flux_VXI(En_,i,j,k,iGang)
       if(nJ > 1) DivE = DivE + &
            Flux_VYI(En_,i,j+1,k,iGang) - Flux_VYI(En_,i,j,k,iGang)
       if(nK > 1) DivE = DivE + &
            Flux_VZI(En_,i,j,k+1,iGang) - Flux_VZI(En_,i,j,k,iGang)
       ! Apply coefficients and divide by density for E=(B x RhoU)/Rho
       DivE = DivE*(ClightFactor**2 - 1)*InvClight2 &
            /State_VGB(Rho_,i,j,k,iBlock)
       Change_V(RhoUx_:RhoUz_) = Change_V(RhoUx_:RhoUz_) &
            + DivE*cross_prod( &
            State_VGB(Bx_:Bz_,i,j,k,iBlock), &
            State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock))
       if(UseB0) Change_V(RhoUx_:RhoUz_) = Change_V(RhoUx_:RhoUz_) &
            + DivE*cross_prod( &
            B0_DGB(:,i,j,k,iBlock), &
            State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock))

#ifdef TESTACC
       if(DoTestSource .and. i==iTest .and. j==jTest .and. k==kTest &
            .and. iBlock == iBlockTest)then
          write(*,*) 'Enx =', &
               Flux_VXI(En_,i,j,k,iGang), Flux_VXI(En_,i+1,j,k,iGang)
          write(*,*) 'Eny =', &
               Flux_VYI(En_,i,j,k,iGang), Flux_VYI(En_,i,j+1,k,iGang)
          write(*,*) 'Enz =', &
               Flux_VZI(En_,i,j,k,iGang), Flux_VZI(En_,i,j,k+1,iGang)
          divE = divE/CellVolume_GB(i,j,k,iBlock) &
               *State_VGB(Rho_,i,j,k,iBlock)
          write(*,*)'Coef   =', (ClightFactor**2 - 1)*InvClight2
          write(*,*)'divE*Coef  =', divE
          ! if(UseB0)then
          !   write(*,*) '!!! e_D=', cross_prod( &
          !        B0_DGB(:,i,j,k,iBlock) &
          !        + State_VGB(Bx_:Bz_,i,j,k,iBlock), &
          !        State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)) &
          !        /State_VGB(Rho_,i,j,k,iBlock)
          ! else
          !   write(*,*) '!!! e_D=', DivE*cross_prod( &
          !        State_VGB(Bx_:Bz_,i,j,k,iBlock), &
          !        State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)) &
          !        /State_VGB(Rho_,i,j,k,iBlock)
          ! end if
       end if
#endif

    end if
    if(UseNonConservative)then
       ! Add -(g-1)*p*div(u) source term
       DivU = Flux_VXI(UnFirst_,i+1,j,k,iGang) - Flux_VXI(UnFirst_,i,j,k,iGang)
       if(nJ > 1) DivU = DivU &
            + Flux_VYI(UnFirst_,i,j+1,k,iGang) - Flux_VYI(UnFirst_,i,j,k,iGang)
       if(nK > 1) DivU = DivU &
            + Flux_VZI(UnFirst_,i,j,k+1,iGang) - Flux_VZI(UnFirst_,i,j,k,iGang)
       Change_V(p_) = Change_V(p_) &
            - GammaMinus1*State_VGB(p_,i,j,k,iBlock)*DivU

       do iFluid = 2, nFluid
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

    if(UseElectronPressure .and. .not.UseElectronEntropy)then
       ! Calculate DivU = div(U_e)
       DivU =                   Flux_VXI(UnLast_,i+1,j,k,iGang) &
            -                   Flux_VXI(UnLast_,i,j,k,iGang)
       if(nJ > 1) DivU = DivU + Flux_VYI(UnLast_,i,j+1,k,iGang) &
            -                   Flux_VYI(UnLast_,i,j,k,iGang)
       if(nK > 1) DivU = DivU + Flux_VZI(UnLast_,i,j,k+1,iGang) &
            -                   Flux_VZI(UnLast_,i,j,k,iGang)

       ! Adiabatic heating for electron pressure: -(g-1)*Pe*Div(U)
       Change_V(Pe_) = Change_V(Pe_) &
            - GammaElectronMinus1*State_VGB(Pe_,i,j,k,iBlock)*DivU
    end if

    if(UseAlfvenWaves)then
       DivU =                   Flux_VXI(UnFirst_,i+1,j,k,iGang) &
            -                   Flux_VXI(UnFirst_,i,j,k,iGang)
       if(nJ > 1) DivU = DivU + Flux_VYI(UnFirst_,i,j+1,k,iGang) &
            -                   Flux_VYI(UnFirst_,i,j,k,iGang)
       if(nK > 1) DivU = DivU + Flux_VZI(UnFirst_,i,j,k+1,iGang) &
            -                   Flux_VZI(UnFirst_,i,j,k,iGang)
       do iVar = WaveFirst_, WaveLast_
          Change_V(iVar) = Change_V(iVar) &
               - (GammaWave - 1)*State_VGB(iVar,i,j,k,iBlock)*DivU
       end do
       ! The energy equation contains the work of the wave pressure
       ! -u.grad Pwave = -div(u Pwave) + Pwave div(u)
       ! The -div(u Pwave) is implemented as a flux.
       ! Here we add the Pwave div(u) source term
       Change_V(Energy_) = Change_V(Energy_) + (GammaWave - 1) &
            *sum(State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock))*DivU
    end if

    ! Below we add sources that do not need to be divided by cell volume
    if(IsCartesian)then
       Change_V = Change_V/CellVolume_B(iBlock)
    else
       Change_V = Change_V/CellVolume_GB(i,j,k,iBlock)
    end if

#ifdef TESTACC
    if(DoTestUpdate .and. i==iTest .and. j==jTest .and. k==kTest &
         .and. iBlock == iBlockTest)then
       write(*,*)'Change_V after divided by V', Change_V(iVarTest)
    end if
#endif

    if(UseGravity .or. UseRotatingFrame)then

       if(UseGravity)then
          ForcePerRho_D = &
               Gbody*Xyz_DGB(:,i,j,k,iBlock)/r_GB(i,j,k,iBlock)**3
          Change_V(Ux_:Uz_) = Change_V(Ux_:Uz_) &
               + State_VGB(Rho_,i,j,k,iBlock)*ForcePerRho_D
          Change_V(Energy_) = Change_V(Energy_) &
               + sum(State_VGB(Ux_:Uz_,i,j,k,iBlock)*ForcePerRho_D)
       end if

       if(UseRotatingFrame)then
          Change_V(Ux_) = Change_V(Ux_) &
               + 2*OmegaBody*State_VGB(Uy_,i,j,k,iBlock) &
               + State_VGB(Rho_,i,j,k,iBlock) &
               *OmegaBody**2 * Xyz_DGB(x_,i,j,k,iBlock)
          Change_V(Uy_) = Change_V(Uy_) &
               - 2*OmegaBody*State_VGB(Ux_,i,j,k,iBlock) &
               + State_VGB(Rho_,i,j,k,iBlock) &
               *OmegaBody**2 * Xyz_DGB(y_,i,j,k,iBlock)
          Change_V(Energy_) = Change_V(Energy_) &
               + OmegaBody**2 * sum(State_VGB(Ux_:Uy_,i,j,k,iBlock) &
               *Xyz_DGB(x_:y_,i,j,k,iBlock))
       end if

       do iFluid = 2, nFluid
          iRho = iRho_I(iFluid)
          iUx = iUx_I(iFluid)
          iEnergy = nVar + iFluid

          if(UseGravity)then
             iUz = iUz_I(iFluid)

             Change_V(iUx:iUz) = Change_V(iUx:iUz) &
                  + State_VGB(iRho,i,j,k,iBlock)*ForcePerRho_D
             Change_V(iEnergy) = Change_V(iEnergy) &
                  + sum(State_VGB(iUx:iUz,i,j,k,iBlock)*ForcePerRho_D)
          end if

#ifdef TESTACC
          if(DoTestUpdate .and. i==iTest .and. j==jTest .and. k==kTest &
               .and. iBlock == iBlockTest)then
             write(*,*)'Change_V after gravity', Change_V(iVarTest)
          end if
#endif

          if(UseRotatingFrame)then
             iUy = iUy_I(iFluid)

             Change_V(iUx) = Change_V(iUx) &
                  + 2*OmegaBody*State_VGB(iUy,i,j,k,iBlock) &
                  + State_VGB(iRho,i,j,k,iBlock) &
                  *OmegaBody**2 * Xyz_DGB(x_,i,j,k,iBlock)
             Change_V(iUy) = Change_V(iUy) &
                  - 2*OmegaBody*State_VGB(iUx,i,j,k,iBlock) &
                  + State_VGB(iRho,i,j,k,iBlock) &
                  *OmegaBody**2 * Xyz_DGB(y_,i,j,k,iBlock)
             Change_V(iEnergy) = Change_V(iEnergy) &
                  + OmegaBody**2 * sum(State_VGB(iUx:iUy,i,j,k,iBlock) &
                  *Xyz_DGB(x_:y_,i,j,k,iBlock))
          end if
       end do
    end if

#ifdef TESTACC
    if(DoTestUpdate .and. i==iTest .and. j==jTest .and. k==kTest &
         .and. iBlock == iBlockTest)then
       write(*,*)'Change_V after rotating frame', Change_V(iVarTest)
    end if
#endif

    ! Time step for iStage
    if(IsTimeAccurate)then
       DtLocal = iStage*Dt/nStage
    else
       DtLocal = iStage*Cfl*DtMax_CB(i,j,k,iBlock)/nStage
    end if

#ifdef TESTACC
    if(DoTestUpdate .and. i==iTest .and. j==jTest .and. k==kTest &
         .and. iBlock == iBlockTest)then
       write(*,*)'DtLocal, Dt, iStage', DtLocal, Dt, iStage
    end if
#endif

    ! Update state
    if(nConservCrit > 0) IsConserv = IsConserv_CB(i,j,k,iBlock)
    if(iStage == 1)then
       if(.not.UseNonConservative .or. nConservCrit>0.and.IsConserv)then
          ! Overwrite pressure and change with energy
          call pressure_to_energy(State_VGB(:,i,j,k,iBlock))
          do iFluid=1, nFluid
             Change_V(iP_I(iFluid)) = Change_V(Energy_+iFluid-1)
          end do
       end if
       if(UseBorisCorrection) call mhd_to_boris( &
            State_VGB(:,i,j,k,iBlock), B0_DGB(:,i,j,k,iBlock), &
            IsConserv)

       ! Convert electron pressure to entropy.
       if(UseElectronPressure .and. UseElectronEntropy) &
            State_VGB(Pe_,i,j,k,iBlock) = &
            State_VGB(Pe_,i,j,k,iBlock)*&
            sum(State_VGB(iRhoIon_I,i,j,k,iBlock)*ChargePerMass_I) &
            **(-GammaElectronMinus1)

       State_VGB(:,i,j,k,iBlock) = State_VGB(:,i,j,k,iBlock) &
            + DtLocal*Change_V(1:nVar)
    else
       if(.not.UseNonConservative .or. nConservCrit>0.and.IsConserv)then
          ! Overwrite old pressure and change with energy
          call pressure_to_energy(StateOld_VGB(:,i,j,k,iBlock))
          do iFluid=1, nFluid
             Change_V(iP_I(iFluid)) = Change_V(Energy_+iFluid-1)
          end do
       end if
       if(UseBorisCorrection) call mhd_to_boris( &
            StateOld_VGB(:,i,j,k,iBlock), B0_DGB(:,i,j,k,iBlock), &
            IsConserv)

       ! Convert electron pressure to entropy
       if(UseElectronPressure .and. UseElectronEntropy) &
            StateOld_VGB(Pe_,i,j,k,iBlock) = &
            StateOld_VGB(Pe_,i,j,k,iBlock)*&
            sum(StateOld_VGB(iRhoIon_I,i,j,k,iBlock)*ChargePerMass_I) &
            **(-GammaElectronMinus1)

       State_VGB(:,i,j,k,iBlock) = StateOld_VGB(:,i,j,k,iBlock) &
            + DtLocal*Change_V(1:nVar)
    end if
    ! Maybe we should put State_VGB(:,i,j,k) and B0_DGB(:,i,j,k) into
    ! local private arrays...
    if(UseBorisCorrection) call boris_to_mhd( &
         State_VGB(:,i,j,k,iBlock), B0_DGB(:,i,j,k,iBlock), IsConserv)

    ! Convert entropy to pressure
    if(UseElectronPressure .and. UseElectronEntropy) &
         State_VGB(Pe_,i,j,k,iBlock)=State_VGB(Pe_,i,j,k,iBlock) &
         *sum(State_VGB(iRhoIon_I,i,j,k,iBlock)*ChargePerMass_I) &
         **GammaElectronMinus1

    ! Check minimum density
    if(UseRhoMin)then
       do iFluid = 1, nFluid
          State_VGB(iRho_I(iFluid),i,j,k,iBlock) = max(RhoMin_I(iFluid),&
               State_VGB(iRho_I(iFluid),i,j,k,iBlock))
       end do
    end if

    ! Convert energy back to pressure
    if(.not.UseNonConservative .or. nConservCrit>0.and.IsConserv) &
         call energy_to_pressure(State_VGB(:,i,j,k,iBlock))

#ifdef TESTACC
    if(DoTestUpdate .and. i==iTest .and. j==jTest .and. k==kTest &
         .and. iBlock == iBlockTest)then
       DivF = Flux_VXI(iVarTest,iTest,jTest,kTest,iGang)    &
            - Flux_VXI(iVarTest,iTest+1,jTest,kTest,iGang)
       if(nDim > 1) DivF = DivF  &
            +Flux_VYI(iVarTest,iTest,jTest,kTest,iGang)     &
            -Flux_VYI(iVarTest,iTest,jTest+1,kTest,iGang)
       if(nDim > 2) DivF = DivF  &
            +Flux_VZI(iVarTest,iTest,jTest,kTest,iGang)     &
            -Flux_VZI(iVarTest,iTest,jTest,kTest+1,iGang)
       DivF = DivF/CellVolume_GB(iTest,jTest,kTest,iBlockTest)
       write(*,*)'Fluxes and sources for ', NameVar_V(iVarTest)
       !#ifdef _OPENACC
       write(*,*) &
            'X fluxes L,R =',Flux_VXI(iVarTest,iTest,jTest,kTest,iGang),&
            Flux_VXI(iVarTest,iTest+1,jTest,kTest,iGang)
       write(*,*) &
            'Y fluxes L,R =',Flux_VYI(iVarTest,iTest,jTest,kTest,iGang),&
            Flux_VYI(iVarTest,iTest,jTest+1,kTest,iGang)
       write(*,*) &
            'Z fluxes L,R =',Flux_VZI(iVarTest,iTest,jTest,kTest,iGang),&
            Flux_VZI(iVarTest,iTest,jTest,kTest+1,iGang)
       write(*,*)'DtLocal=', DtLocal
       write(*,*)'Change_V=', Change_V(iVarTest)
       write(*,*)'CellVolume=', CellVolume_GB(iTest,jTest,kTest,iBlockTest)
       write(*,*)'source=', Change_V(iVarTest) - DivF
       write(*,*)'fluxes=', DivF
       !#else
       ! write(*,'(2x,a,2es23.15)') &
       !     'X fluxes L,R =',Flux_VXI(iVarTest,iTest,jTest,kTest,iGang),&
       !     Flux_VXI(iVarTest,iTest+1,jTest,kTest,iGang)
       ! write(*,'(2x,a,2es23.15)') &
       !     'Y fluxes L,R =',Flux_VYI(iVarTest,iTest,jTest,kTest,iGang),&
       !     Flux_VYI(iVarTest,iTest,jTest+1,kTest,iGang)
       ! write(*,'(2x,a,2es23.15)') &
       !     'Z fluxes L,R =',Flux_VZI(iVarTest,iTest,jTest,kTest,iGang),&
       !     Flux_VZI(iVarTest,iTest,jTest,kTest+1,iGang)
       ! write(*,'(2x,a,es23.15)')'source=', Change_V(iVarTest) &
       !     /CellVolume_GB(iTest,jTest,kTest,iBlockTest) - DivF
       ! write(*,'(2x,a,es23.15)')'fluxes=', DivF
       !#endif
       write(*,*)
       write(*,*)NameSub,' final for nStep=', nStep
       do iVar=1,nVar
          !#ifdef _OPENACC
          write(*,*) ' ', NameVar_V(iVar), '(TestCell)  =',&
               State_VGB(iVar,iTest,jTest,kTest,iBlockTest)
          !#else
          ! write(*,'(2x,2a,es23.15)')NameVar_V(iVar), '(TestCell)  =',&
          !     State_VGB(iVar,iTest,jTest,kTest,iBlockTest)
          !#endif
       end do
       write(*,*) NameSub,' is finished for iProc, iBlock=', 0, iBlock
       if(UseDivbSource)      write(*,*)'divB =', divB
       if(UseNonConservative) write(*,*)'divU =', divU
    end if
#endif

  end subroutine update_cell
  !============================================================================
  subroutine get_flux_x(i, j,  k, iBlock, IsBodyBlock)
    !$acc routine seq

    integer, intent(in):: i, j, k, iBlock
    logical, intent(in), optional:: IsBodyBlock

    real :: Area, Normal_D(3), B0_D(3)
    real :: StateLeft_V(nVar), StateRight_V(nVar)
    integer:: iGang, iVar
    logical:: DoTestSide
#ifndef _OPENACC
    !--------------------------------------------------------------------------
    iGang = 1
#else
    iGang = iBlock
#endif
    call get_normal(1, i, j, k, iBlock, Normal_D, Area)

    call get_face_x(i, j, k, iBlock, StateLeft_V, StateRight_V, IsBodyBlock)

    if(UseB0) call get_b0_face(B0_D,i,j,k,iBlock,x_)

#ifdef TESTACC
    DoTestSide = .false.
    if(DoTestFlux .and. (iDimTest == 0 .or. iDimTest == 1) .and. &
         j==jTest .and. k==kTest .and. iBlock == iBlockTest)then
       if(  (i == iTest+1 .and. iSideTest >= 0) .or. &
            (i == iTest .and. iSideTest <= 0)) DoTestSide = .true.
    end if

    if(DoTestSide)then
       write(*,*)'Calc_facefluxes, left and right states at i-1/2 and i+1/2:'
       do iVar = 1, nVar
          !#ifdef _OPENACC
          write(*,*)NameVar_V(iVar),'=',&
               StateLeft_V(iVar), StateRight_V(iVar), iSideTest
          !#else
          !          write(*,'(2a,2es13.5,i3)')NameVar_V(iVar),'=',&
          !               StateLeft_V(iVar), StateRight_V(iVar), iSideTest
          !#endif
       end do
       if(UseB0)then
          !#ifdef _OPENACC
          write(*,*)'B0x:', B0_D(1), iSideTest
          write(*,*)'B0y:', B0_D(2), iSideTest
          write(*,*)'B0z:', B0_D(3), iSideTest
          !#else
          !          write(*,'(a,es13.5,i3)')'B0x:', B0_D(1), iSideTest
          !          write(*,'(a,es13.5,i3)')'B0y:', B0_D(2), iSideTest
          !          write(*,'(a,es13.5,i3)')'B0z:', B0_D(3), iSideTest
          !#endif
       end if
    end if
#endif

    call get_numerical_flux(Normal_D, Area, &
         StateLeft_V, StateRight_V, Flux_VXI(:,i,j,k,iGang), B0_D, DoTestSide)

  end subroutine get_flux_x
  !============================================================================
  subroutine get_flux_y(i, j,  k, iBlock, IsBodyBlock)
    !$acc routine seq

    integer, intent(in):: i, j, k, iBlock
    logical, intent(in), optional:: IsBodyBlock

    real :: Area, Normal_D(3), B0_D(3)
    real :: StateLeft_V(nVar), StateRight_V(nVar)
    integer:: iGang, iVar
    logical:: DoTestSide
#ifndef _OPENACC
    !--------------------------------------------------------------------------
    iGang = 1
#else
    iGang = iBlock
#endif
    call get_normal(2, i, j, k, iBlock, Normal_D, Area)

    call get_face_y(i, j, k, iBlock, StateLeft_V, StateRight_V, IsBodyBlock)

    if(UseB0) call get_b0_face(B0_D, i, j, k, iBlock, y_)

#ifdef TESTACC
    DoTestSide = .false.
    if(DoTestFlux .and. (iDimTest == 0 .or. iDimTest == 2) .and. &
         i==iTest .and. k==kTest .and. iBlock == iBlockTest)then
       if(  (j == jTest+1 .and. iSideTest >= 0) .or. &
            (j == jTest .and. iSideTest <= 0)) DoTestSide = .true.
    end if

    if(DoTestSide)then
       write(*,*)'Calc_facefluxes, left and right states at j-1/2 and j+1/2:'
       do iVar = 1, nVar
          !#ifdef _OPENACC
          write(*,*)NameVar_V(iVar),'=',&
               StateLeft_V(iVar), StateRight_V(iVar), iSideTest
          !#else
          !          write(*,'(2a,2es13.5,i3)')NameVar_V(iVar),'=',&
          !               StateLeft_V(iVar), StateRight_V(iVar), iSideTest
          !#endif
       end do
       if(UseB0)then
          !#ifdef _OPENACC
          write(*,*)'B0x:', B0_D(1), iSideTest
          write(*,*)'B0y:', B0_D(2), iSideTest
          write(*,*)'B0z:', B0_D(3), iSideTest
          !#else
          !          write(*,'(a,es13.5,i3)')'B0x:', B0_D(1), iSideTest
          !          write(*,'(a,es13.5,i3)')'B0y:', B0_D(2), iSideTest
          !          write(*,'(a,es13.5,i3)')'B0z:', B0_D(3), iSideTest
          !#endif
       end if
    end if
#endif

    call get_numerical_flux(Normal_D, Area, &
         StateLeft_V, StateRight_V, Flux_VYI(:,i,j,k,iGang), B0_D, DoTestSide)

  end subroutine get_flux_y
  !============================================================================
  subroutine get_flux_z(i, j, k, iBlock, IsBodyBlock)
    !$acc routine seq

    integer, intent(in):: i, j, k, iBlock
    logical, intent(in), optional:: IsBodyBlock

    real :: Area, Normal_D(3), B0_D(3)
    real :: StateLeft_V(nVar), StateRight_V(nVar)
    integer:: iGang, iVar
    logical:: DoTestSide
#ifndef _OPENACC
    !--------------------------------------------------------------------------
    iGang = 1
#else
    iGang = iBlock
#endif
    call get_normal(3, i, j, k, iBlock, Normal_D, Area)

    call get_face_z(i, j, k, iBlock, StateLeft_V, StateRight_V, IsBodyBlock)

    if(UseB0) call get_b0_face(B0_D,i,j,k,iBlock,z_)

#ifdef TESTACC
    DoTestSide = .false.
    if(DoTestFlux .and. (iDimTest == 0 .or. iDimTest == 3) .and. &
         i==iTest .and. j==jTest .and. iBlock == iBlockTest)then
       if(  (k == kTest+1 .and. iSideTest >= 0) .or. &
            (k == kTest .and. iSideTest <= 0)) DoTestSide = .true.
    end if

    if (DoTestSide)then
       write(*,*)'Calc_facefluxes, left and right states at k-1/2 and k+1/2:'
       do iVar = 1, nVar
          !#ifdef _OPENACC
          write(*,*)NameVar_V(iVar),'=',&
               StateLeft_V(iVar), StateRight_V(iVar), iSideTest
          !#else
          !          write(*,'(2a,2es13.5,i3)')NameVar_V(iVar),'=',&
          !               StateLeft_V(iVar), StateRight_V(iVar), iSideTest
          !#endif
       end do
       if(UseB0)then
          !#ifdef _OPENACC
          write(*,*)'B0x:', B0_D(1), iSideTest
          write(*,*)'B0y:', B0_D(2), iSideTest
          write(*,*)'B0z:', B0_D(3), iSideTest
          !#else
          !          write(*,'(a,es13.5,i3)')'B0x:', B0_D(1), iSideTest
          !          write(*,'(a,es13.5,i3)')'B0y:', B0_D(2), iSideTest
          !          write(*,'(a,es13.5,i3)')'B0z:', B0_D(3), iSideTest
          !#endif
       end if
    end if
#endif

    call get_numerical_flux(Normal_D, Area, &
         StateLeft_V, StateRight_V, Flux_VZI(:,i,j,k,iGang), B0_D, DoTestSide)

  end subroutine get_flux_z
  !============================================================================
  subroutine get_face_x(i, j, k, iBlock, StateLeft_V, StateRight_V, &
       IsBodyBlock)
    !$acc routine seq

    integer, intent(in) :: i, j, k, iBlock
    real,    intent(out):: StateLeft_V(nVar), StateRight_V(nVar)
    logical, intent(in), optional:: IsBodyBlock

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
    end if
    if(UseBody .and. present(IsBodyBlock)) then
       ! Use first order if stencil intersects the body
       if(nOrder == 2)then
          if(.not.all(Used_GB(i-2:i,j,k,iBlock))) &
               call get_primitive(State_VGB(:,i-1,j,k,iBlock), StateLeft_V)
          if(.not.all(Used_GB(i-1:i+1,j,k,iBlock))) &
               call get_primitive(State_VGB(:,i,j,k,iBlock),   StateRight_V)
       end if
       ! Apply face boundary condition
       if(Used_GB(i-1,j,k,iBlock) .and. .not. Used_GB(i,j,k,iBlock)) then
          call set_face(StateLeft_V, StateRight_V, i-1, j, k, i, j, k, iBlock)
       elseif(Used_GB(i,j,k,iBlock) .and. .not. Used_GB(i-1,j,k,iBlock)) then
          call set_face(StateRight_V, StateLeft_V, i, j, k, i-1, j, k, iBlock)
       endif
    endif

  end subroutine get_face_x
  !============================================================================
  subroutine get_face_y(i, j, k, iBlock, StateLeft_V, StateRight_V, &
       IsBodyBlock)
    !$acc routine seq

    integer, intent(in) :: i, j, k, iBlock
    real,    intent(out):: StateLeft_V(nVar), StateRight_V(nVar)
    logical, intent(in), optional :: IsBodyBlock

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
    end if
    if(UseBody .and. present(IsBodyBlock)) then
       if(nOrder == 2)then
          if(.not.all(Used_GB(i,j-2:j,k,iBlock))) &
               call get_primitive(State_VGB(:,i,j-1,k,iBlock), StateLeft_V)
          if(.not.all(Used_GB(i,j-1:j+1,k,iBlock))) &
               call get_primitive(State_VGB(:,i,j,k,iBlock),   StateRight_V)
       endif
       if(Used_GB(i,j-1,k,iBlock) .and. .not. Used_GB(i,j,k,iBlock)) then
          call set_face(StateLeft_V, StateRight_V, i, j-1, k, i, j, k, iBlock)
       else if(Used_GB(i,j,k,iBlock) .and. .not. Used_GB(i,j-1,k,iBlock)) then
          call set_face(StateRight_V, StateLeft_V, i, j, k, i, j-1, k, iBlock)
       end if
    end if

  end subroutine get_face_y
  !============================================================================
  subroutine get_face_z(i, j, k, iBlock, StateLeft_V, StateRight_V, &
       IsBodyBlock)
    !$acc routine seq

    integer, intent(in) :: i, j, k, iBlock
    real,    intent(out):: StateLeft_V(nVar), StateRight_V(nVar)
    logical, intent(in), optional :: IsBodyBlock

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
    end if
    if(UseBody .and. present(IsBodyBlock)) then
       if (nOrder == 2) then
          if(.not.all(Used_GB(i,j,k-2:k,iBlock))) &
               call get_primitive(State_VGB(:,i,j,k-1,iBlock), StateLeft_V)
          if(.not.all(Used_GB(i,j,k-1:k+1,iBlock))) &
               call get_primitive(State_VGB(:,i,j,k,iBlock),   StateRight_V)
       endif
       if (Used_GB(i,j,k-1,iBlock) .and. .not. Used_GB(i,j,k,iBlock)) then
          call set_face(StateLeft_V, StateRight_V, i, j, k-1, i, j, k, iBlock)
       else if (Used_GB(i,j,k,iBlock) .and. .not. Used_GB(i,j,k-1,iBlock)) then
          call set_face(StateRight_V, StateLeft_V, i, j, k, i, j, k-1, iBlock)
       end if
    end if

  end subroutine get_face_z
  !============================================================================
  subroutine set_old_state(iBlock)
    !$acc routine vector

    ! Copy the current state into StateOld_VGB for 2-stage scheme:
    ! Storage:           StateOld = State
    ! Stage 1:           State'   = State    + dt/2*R(State)
    ! Stage 2:           State"   = StateOld +   dt*R(State')
    integer, intent(in):: iBlock

    integer:: i, j, k
    !--------------------------------------------------------------------------
    !$acc loop vector collapse(3) independent
    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       StateOld_VGB(:,i,j,k,iBlock)  = State_VGB(:,i,j,k,iBlock)
    end do; end do; end do

  end subroutine set_old_state
  !============================================================================
  subroutine get_normal(iDir, i, j, k, iBlock, Normal_D, Area)
    !$acc routine seq

    ! Calculate normal direction and Area for the face in diretion iDir
    ! of cell i, j, k in block iBlcok

    integer, intent(in) :: i, j, k, iBlock, iDir
    real,    intent(out):: Normal_D(3), Area
    !--------------------------------------------------------------------------
    if(IsCartesian)then
       Area = CellFace_DB(iDir,iBlock)
    else
       Area = CellFace_DFB(iDir,i,j,k,iBlock)
       if(Area < 1e-15)then
          Area = 0
          Normal_D = [1.0, 0.0, 0.0]
          RETURN
       end if
    end if
    if(IsCartesianGrid)then
       Normal_D = cUnit_DD(:,iDir)
    else
       Normal_D = 0.0
       Normal_D(1:nDim) = FaceNormal_DDFB(:,iDir,i,j,k,iBlock)/Area
    end if

  end subroutine get_normal
  !============================================================================
  subroutine get_primitive(State_V, Primitive_V)
    !$acc routine seq

    ! Convert from momentum to velocity

    real, intent(in) :: State_V(nVar)
    real, intent(out):: Primitive_V(nVar)

    integer:: iFluid
    real:: InvRho
    !--------------------------------------------------------------------------
    InvRho = 1/State_V(Rho_)
    Primitive_V(1:Ux_-1)    = State_V(1:RhoUx_-1)
    Primitive_V(Ux_:Uz_)    = State_V(RhoUx_:RhoUz_)*InvRho
    Primitive_V(Uz_+1:nVar) = State_V(RhoUz_+1:nVar)

    ! Do rest of the fluid velocities
    do iFluid = 2, nFluid
       InvRho = 1/State_V(iRho_I(iFluid))
       Primitive_V(iUx_I(iFluid)) = InvRho*State_V(iRhoUx_I(iFluid))
       Primitive_V(iUy_I(iFluid)) = InvRho*State_V(iRhoUy_I(iFluid))
       Primitive_V(iUz_I(iFluid)) = InvRho*State_V(iRhoUz_I(iFluid))
    end do

  end subroutine get_primitive
  !============================================================================
  subroutine limiter2(Var1, Var2, Var3, Var4, VarLeft, VarRight)
    !$acc routine seq

    ! Second order Koren limiter on a 4 point stencil

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
#endif
  subroutine set_boundary_fast

    ! Set cell boundaries for State_VGB

    integer:: iBlock
    !--------------------------------------------------------------------------
#ifndef SCALAR
    if (IsTimeAccurate .and. iTypeCellBc_I(2) == VaryBC_)then
       call get_solar_wind_point(tSimulation, [xMaxBox, 0., 0.], &
            CellState_VI(:,2))
       ! Convert velocity to momentum
       CellState_VI(RhoUx_:RhoUz_,2) = &
            CellState_VI(RhoUx_:RhoUz_,2)*CellState_VI(Rho_,2)
       !$acc update device(CellState_VI)
    endif

    !$acc parallel loop gang independent
    do iBlock = 1, nBlock
       call set_cell_boundary_for_block(iBlock, nVar, &
            State_VGB(:,:,:,:,iBlock))
    end do
#endif
  end subroutine set_boundary_fast
  !============================================================================
#ifndef SCALAR
  subroutine set_cell_boundary_for_block(iBlock, nVarState, State_VG, &
       IsLinearIn)
    !$acc routine vector

    integer, intent(in) :: iBlock, nVarState
    real, intent(inout):: State_VG(nVarState,MinI:MaxI,MinJ:MaxJ,MinK:MaxK)

    ! Present only for semi-implicit scheme
    logical, intent(in), optional:: IsLinearIn

    integer :: i, j, k, iSide
    logical :: IsLinear
    !--------------------------------------------------------------------------
    if(Unused_B(iBlock)) RETURN
    if(.not.IsBoundary_B(iBlock)) RETURN

    ! Default is false, because explicit scheme does not use linear BC
    IsLinear = .false.
    if(present(IsLinearIn)) IsLinear = IsLinearIn

    ! x left
    iSide = 1
    if(DiLevel_EB(iSide,iBlock) == Unset_) then
       !$acc loop vector collapse(3) independent
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, 0
          call set_boundary_for_cell(iSide, i, j, k, 1, j, k, &
               iBlock, iTypeCellBc_I(iSide), IsLinear, nVarState, State_VG)
       end do; end do; end do
    end if

    ! x right
    iSide = 2
    if(DiLevel_EB(iSide,iBlock) == Unset_) then
       !$acc loop vector collapse(3) independent
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = nI+1, MaxI
          call set_boundary_for_cell(iSide, i, j, k, nI, j, k, &
               iBlock, iTypeCellBc_I(iSide), IsLinear, nVarState, State_VG)
       end do; end do; end do
    end if

    ! y left
    iSide = 3
    if(DiLevel_EB(iSide,iBlock) == Unset_ .and. nDim >= 2) then
       !$acc loop vector collapse(3) independent
       do k = MinK, MaxK; do j = MinJ, 0; do i = MinI, MaxI
          call set_boundary_for_cell(iSide, i, j, k, i, 1, k, &
               iBlock, iTypeCellBc_I(iSide), IsLinear, nVarState, State_VG)
       end do; end do; end do
    end if

    ! y right
    iSide = 4
    if(DiLevel_EB(iSide,iBlock) == Unset_ .and. nDim >= 2) then
       !$acc loop vector collapse(3) independent
       do k = MinK, MaxK; do j = nJ+1, MaxJ; do i = MinI, MaxI
          call set_boundary_for_cell(iSide, i, j, k, i, nJ, k, &
               iBlock, iTypeCellBc_I(iSide), IsLinear, nVarState, State_VG)
       end do; end do; end do
    end if

    ! z left
    iSide = 5
    if(DiLevel_EB(iSide,iBlock) == Unset_ .and. nDim == 3) then
       !$acc loop vector collapse(3) independent
       do k = MinK, 0; do j = MinJ, MaxJ; do i = MinI, MaxI
          call set_boundary_for_cell(iSide, i, j, k, i, j, 1, &
               iBlock, iTypeCellBc_I(iSide), IsLinear, nVarState, State_VG)
       end do; end do; end do
    end if

    ! z right
    iSide = 6
    if(DiLevel_EB(iSide,iBlock) == Unset_ .and. nDim == 3) then
       !$acc loop vector collapse(3) independent
       do k = nK+1, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          call set_boundary_for_cell(iSide, i, j, k, i, j, nK, &
               iBlock, iTypeCellBc_I(iSide), IsLinear, nVarState, State_VG)
       end do; end do; end do
    end if
  end subroutine set_cell_boundary_for_block
  !============================================================================

  subroutine set_boundary_for_cell(iSide, i, j, k, iSend, jSend, kSend, &
       iBlock, iTypeBC, IsLinear, nVarState, State_VG)
    !$acc routine seq

    integer, intent(in):: iSide, i, j, k, iSend, jSend, kSend, iBlock
    integer, intent(in):: iTypeBC, nVarState
    logical, intent(in):: IsLinear
    real, intent(inout):: State_VG(nVarState,MinI:MaxI,MinJ:MaxJ,MinK:MaxK)
    !--------------------------------------------------------------------------
    if(IsLinear)then
       State_VG(iVarSemiMin:iVarSemiMax,i,j,k) = 0.0
    elseif(iTypeBC == FloatBc_)then
       State_VG(:,i,j,k) = State_VG(:,iSend,jSend,kSend)
    elseif(iTypeBC == VaryBC_ .or. &
         iTypeBC == FixedBC_ .or. &
         iTypeBC == InFlowBC_) then
       State_VG(:,i,j,k) = CellState_VI(:,iSide)
       if(UseB0) State_VG(Bx_:Bz_,i,j,k) = &
            State_VG(Bx_:Bz_,i,j,k) - B0_DGB(:,i,j,k,iBlock)
    else
       call stop_mpi('set_boundary_for_cell: Unimplemented boundary type')
    end if

  end subroutine set_boundary_for_cell
  !============================================================================
  subroutine set_face(VarsTrueFace_V, VarsGhostFace_V, &
       i, j, k, iBody, jBody, kBody, iBlock)
    !$acc routine seq

    ! Set boundary conditions on the face between the physical
    ! cells i,j,k,iBlock and body cell iBody,jBody,kBody,iBlock.

    real, intent(in)   :: VarsTrueFace_V(nVar)
    real, intent(out)  :: VarsGhostFace_V(nVar)
    integer, intent(in):: i, j, k, iBody, jBody, kBody, iBlock

    real:: Coef

    real:: XyzFace_D(3), u_D(3), b_D(3)

    real, parameter:: DensityJumpLimit=0.1
    !--------------------------------------------------------------------------
    if(B1rCoef /= 1.0 .or. UseRotatingBc) XyzFace_D &
         = 0.5*(Xyz_DGB(:,i,j,k,iBlock) + Xyz_DGB(:,iBody,jBody,kBody,iBlock))

    if(.true.) then
       ! 'ionosphere' type BC

       VarsGhostFace_V        =  VarsTrueFace_V

       ! Use body densities but limit jump
       ! Pressure gets set too (! ). It will be overwritten below
       where(DefaultState_V(1:nVar) > 0.0)
          VarsGhostFace_V = VarsTrueFace_V + &
               sign(1.0, FaceState_VI(:,body1_) - VarsTrueFace_V)*   &
               min( abs(FaceState_VI(:,body1_) - VarsTrueFace_V)     &
               ,    DensityJumpLimit*VarsTrueFace_V   )
       end where
       ! where(DefaultState_V(1:nVar) > 0.0)
       !   VarsGhostFace_V = FaceState_VI(:,body1_)
       ! endwheree

       ! Apply CPCP boundary condition
       if(UseCpcpBc .and. UseIe) &
            VarsGhostFace_V(iRhoIon_I) = RhoCpcp_I(1:nIonFluid)

       ! Set pressures, including electron pressure, to float.
       VarsGhostFace_V(iP_I) = VarsTrueFace_V(iP_I)

       ! Change sign for velocities (plasma frozen into dipole field)
       VarsGhostFace_V(iUx_I) = -VarsTrueFace_V(iUx_I)
       VarsGhostFace_V(iUy_I) = -VarsTrueFace_V(iUy_I)
       VarsGhostFace_V(iUz_I) = -VarsTrueFace_V(iUz_I)

       if(B1rCoef /= 1.0)then
          ! Fully or partially reflect B1r:
          ! Brefl = (1-B1rCoef)*r*(B.r)/r^2 and Bghost = Btrue - Brefl
          Coef = (1-B1rCoef)/sum(XyzFace_D**2)
          VarsGhostFace_V(Bx_:Bz_) = VarsTrueFace_V(Bx_:Bz_) - &
               Coef*XyzFace_D*sum(VarsTrueFace_V(Bx_:Bz_)*XyzFace_D)
       end if
    else
       ! 'ionospherefloat'

       VarsGhostFace_V        =  VarsTrueFace_V
       VarsGhostFace_V(iUx_I) = -VarsTrueFace_V(iUx_I)
       VarsGhostFace_V(iUy_I) = -VarsTrueFace_V(iUy_I)
       VarsGhostFace_V(iUz_I) = -VarsTrueFace_V(iUz_I)
    endif

    if (UseIe) then
       ! Apply E x B / B^2 drift velocity
       b_D = VarsTrueFace_V(Bx_:Bz_) + &
            0.5*(B0_DGB(:,i,j,k,iBlock) + B0_DGB(:,iBody,jBody,kBody,iBlock))

       ! Get the E x B / B^2 velocity
       call calc_inner_bc_velocity(tSimulation, XyzFace_D, b_D, u_D)

       ! Subtract the radial component of the velocity (no outflow/inflow)
       u_D = u_D &
            - XyzFace_D * sum(XyzFace_D*u_D) / sum(XyzFace_D**2)

       VarsGhostFace_V(iUx_I) = 2*u_D(x_) + VarsGhostFace_V(iUx_I)
       VarsGhostFace_V(iUy_I) = 2*u_D(y_) + VarsGhostFace_V(iUy_I)
       VarsGhostFace_V(iUz_I) = 2*u_D(z_) + VarsGhostFace_V(iUz_I)
    end if

    if (UseRotatingBc) then
       ! The corotation velocity is u = Omega x R
       u_D = cross_prod(OmegaBody_D, XyzFace_D)

       ! Apply corotation for the following BC:  'reflect','linetied', &
       ! 'ionosphere','ionospherefloat','polarwind','ionosphereoutflow'
       VarsGhostFace_V(iUx_I) = 2*u_D(x_) + VarsGhostFace_V(iUx_I)
       VarsGhostFace_V(iUy_I) = 2*u_D(y_) + VarsGhostFace_V(iUy_I)
       VarsGhostFace_V(iUz_I) = 2*u_D(z_) + VarsGhostFace_V(iUz_I)
    end if

  end subroutine set_face
  !============================================================================
  subroutine get_b0_face(B0_D, i, j, k, iBlock, iDir)
    !$acc routine seq

    ! Return B0 at the face in direction iDir relative to i, j, k cell center

    real, intent(out)   :: B0_D(3)
    integer, intent(in) :: i,j,k,iBlock,iDir
    !--------------------------------------------------------------------------
    B0_D = 0

    if(iDir > nDim) RETURN

    select case(iDir)
    case(x_)
       B0_D = 0.5*(B0_DGB(:,i-1,j,k,iBlock) + B0_DGB(:,i,j,K,iBlock))
    case(y_)
       B0_D = 0.5*(B0_DGB(:,i,j-1,k,iBlock) + B0_DGB(:,i,j,k,iBlock))
    case(z_)
       B0_D = 0.5*(B0_DGB(:,i,j,k-1,iBlock) + B0_DGB(:,i,j,k,iBlock))
    end select

  end subroutine get_b0_face
  !============================================================================
  subroutine calc_block_dt(iBlock)
    !$acc routine vector

    ! Compute maximum stable time step for this solution block

    integer, intent(in):: iBlock

    real:: DtMin
    integer:: i, j, k
    !--------------------------------------------------------------------------
    if(IsNoBody_B(iBlock)) then
       DtMin = DtMax_CB(1,1,1,iBlock)
       !$acc loop vector independent collapse(3) reduction(min:DtMin)
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          DtMin = min(DtMin, DtMax_CB(i,j,k,iBlock))
       end do; end do; end do
       DtMax_B(iBlock) = DtMin
    else
       ! If the block has no true cells, set DtMax_B=1.0E20
       DtMin = 1e20
       !$acc loop vector independent collapse(3) reduction(min:DtMin)
       do k=1,nK; do j=1,nJ; do i=1,nI
          if (Used_GB(i,j,k,iBlock)) DtMin = min(DtMin, DtMax_CB(i,j,k,iBlock))
       end do; end do; end do
       DtMax_B(iBlock) = DtMin
    end if

  end subroutine calc_block_dt
  !============================================================================
  subroutine get_physical_flux(State_V, Normal_D, StateCons_V, Flux_V, B0_D)
    !$acc routine seq

    ! Calculate physical flux at a face with normal vector Normal_D
    ! based on the primitive state State_V and B0 field B0_D.
    ! Also return the conservative state vector StateCons_V.

    real, intent(in) :: State_V(nVar)      ! primitive state vector
    real, intent(in) :: Normal_D(3)        ! face normal
    real, intent(out):: StateCons_V(nFlux) ! conservative state vector
    real, intent(out):: Flux_V(nFlux+1)    ! conservative flux and Enormal
    real, intent(in) :: B0_D(3)

    ! Convenient variables
    integer:: iVar
    real:: Un, Bn, pB, e
    real:: FullB_D(3), B0n,  FullBn
    real:: AlfvenSpeed, pExtra
    !--------------------------------------------------------------------------
    ! For sake of simplicity, to be optimized
    if(UseBorisCorrection)then
       call get_boris_flux(State_V, Normal_D, StateCons_V, Flux_V, B0_D)
       RETURN
    end if
    Un  = sum(State_V(Ux_:Uz_)*Normal_D)

    FullB_D = State_V(Bx_:Bz_)
    Bn  = sum(FullB_D*Normal_D)
    pB  = 0.5*sum(FullB_D**2)
    FullBn = Bn

    e = InvGammaMinus1*State_V(p_) + 0.5*State_V(Rho_)*sum(State_V(Ux_:Uz_)**2)

    if(UseB0) then
       B0n     = sum(B0_D*Normal_D)
       FullB_D = FullB_D + B0_D
       FullBn  = FullBn + B0n
    endif

    ! Conservative state for the Rusanov solver
    StateCons_V(1:nVar) = State_V
    StateCons_V(RhoUx_:RhoUz_) = State_V(Rho_)*State_V(Ux_:Uz_)
    StateCons_V(Energy_) = e + pB ! Add magnetic energy density

    pExtra = 0.0
    if(UseElectronPressure) pExtra = State_V(Pe_)
    if(UseAlfvenWaves) &
         pExtra = pExtra + (GammaWave-1)*sum(State_V(WaveFirst_:WaveLast_))

    ! Physical flux
    Flux_V(Rho_) = State_V(Rho_)*Un
    Flux_V(RhoUx_:RhoUz_) = Flux_V(Rho_)*State_V(Ux_:Uz_) - Bn*FullB_D &
         + (pB + State_V(p_) + pExtra)*Normal_D
    if(UseB0) then
       Flux_V(RhoUx_:RhoUz_) = Flux_V(RhoUx_:RhoUz_) &
            - B0n*State_V(Bx_:Bz_) + sum(State_V(Bx_:Bz_)*B0_D)*Normal_D
    endif

    if(Hyp_ > 1)then
       Flux_V(Bx_:Bz_) = Un*FullB_D - State_V(Ux_:Uz_)*FullBn &
            + Normal_D*SpeedHyp*State_V(Hyp_)
       Flux_V(Hyp_) = SpeedHyp*Bn
    else
       Flux_V(Bx_:Bz_) = Un*FullB_D - State_V(Ux_:Uz_)*FullBn
    end if
    Flux_V(p_)      =  Un*State_V(p_)
    Flux_V(Energy_) =  Un*(e + State_V(p_) + pExtra) &
         + sum(Flux_V(Bx_:Bz_)*State_V(Bx_:Bz_)) ! Poynting flux

    if(UseElectronPressure)then
       if(UseElectronEntropy) StateCons_V(Pe_) = &
            State_V(Pe_)*sum(State_V(iRhoIon_I)*ChargePerMass_I) &
            **(-GammaElectronMinus1)

       Flux_V(Pe_) = Un*StateCons_V(Pe_)
    end if

    if(Ehot_ > 1) Flux_V(Ehot_) = Un*State_V(Ehot_)

    if(UseAlfvenWaves)then
       AlfvenSpeed = FullBn/sqrt(State_V(Rho_))

       do iVar = AlfvenPlusFirst_, AlfvenPlusLast_
          Flux_V(iVar) = (Un + AlfvenSpeed)*State_V(iVar)
       end do

       do iVar = AlfvenMinusFirst_, AlfvenMinusLast_
          Flux_V(iVar) = (Un - AlfvenSpeed)*State_V(iVar)
       end do
    end if

  end subroutine get_physical_flux
  !============================================================================
  subroutine get_boris_flux(State_V, Normal_D, StateCons_V, Flux_V, B0_D)
    !$acc routine seq

    real, intent(in) :: State_V(nVar)      ! primitive state vector
    real, intent(in) :: Normal_D(3)        ! face normal
    real, intent(out):: StateCons_V(nFlux) ! conservative state vector
    real, intent(out):: Flux_V(nFlux+1)    ! conservative flux + Enormal
    real, intent(in) :: B0_D(3)

    ! Variables for conservative state and flux calculation
    real :: Rho, p, e
    real :: B2, FullB2, pTotal, pTotal2, uDotB ! , DpPerB
    real :: u_D(3), FullB_D(3), e_D(3), E2Half, Un, En, FullBn, Bn
    !--------------------------------------------------------------------------
    Rho     = State_V(Rho_)
    u_D     = State_V(Ux_:Uz_)
    p       = State_V(p_)
    FullB_D = State_V(Bx_:Bz_)
    if(UseB0) FullB_D = FullB_D + B0_D
    Bn      = sum(State_V(Bx_:Bz_)*Normal_D)
    FullBn  = sum(FullB_D*Normal_D)

    ! For isotropic Pe, Pe contributes the ion momentum eqn, while for
    ! anisotropic Pe, Peperp contributes
    ! if (UseElectronPressure .and. .not. UseAnisoPe) then
    !   PeAdd = State_V(Pe_)
    ! else if (UseAnisoPe) then
    !   ! Peperp = (3*pe - Pepar)/2
    !   PeAdd = (3*State_V(Pe_) - State_V(Pepar_))/2.0
    ! end if

    B2 = sum(State_V(Bx_:Bz_)**2)

    ! Electric field divided by speed of light:
    ! E= - U x B / c = (B x U)/c
    e_D = InvClight*cross_prod(FullB_D, u_D)

    ! Electric field squared/c^2
    E2Half  = 0.5*sum(e_D**2)

    ! Calculate energy and total pressure
    e = InvGammaMinus1*p + 0.5*(Rho*sum(u_D**2) + B2)

    pTotal  = 0.5*B2
    if(UseB0) pTotal = pTotal + sum(B0_D*State_V(Bx_:Bz_))

    ! if(UseElectronPressure) pTotal = pTotal + PeAdd

    ! if(UseWavePressure)then
    !    if(UseWavePressureLtd)then
    !       pTotal = pTotal + (GammaWave-1)*State_V(Ew_)
    !    else
    !       pTotal = pTotal + (GammaWave-1)*sum(State_V(WaveFirst_:WaveLast_))
    !    end if
    ! end if

    ! pTotal = pperp + bb/2 = 3/2*p - 1/2*ppar + bb/2
    !        = p + bb/2 + (p - ppar)/2
    ! if(UseAnisoPressure) pTotal = pTotal + 0.5*(p - State_V(Ppar_))

    pTotal2 = pTotal + E2Half

    ! The full momentum contains the ExB/c^2 term:
    ! rhoU_Boris = rhoU - ((U x B) x B)/c^2 = rhoU + (U B^2 - B U.B)/c^2
    uDotB   = sum(u_D*FullB_D)
    FullB2  = sum(FullB_D**2)

    StateCons_V(1:nVar) = State_V
    StateCons_V(RhoUx_:RhoUz_)  = &
         Rho*u_D + (u_D*FullB2 - FullB_D*uDotB)*InvClight2

    ! The full energy contains the electric field energy
    StateCons_V(Energy_) = e + E2Half

    ! Normal component
    Un = sum(u_D*Normal_D)
    En = sum(e_D*Normal_D)

    ! Store it into Flux_V for Boris source term
    if(ClightFactor /= 1.0) Flux_V(nFlux+1) = En

    ! f_i[rho] = rho*u_i
    Flux_V(Rho_)   = Rho*Un

    ! f_i[rhou_k] = u_i*u_k*rho - b_k*b_i - B0_k*b_i - B0_i*b_k - E_i*E_k
    !          +n_i*[p + B0_j*b_j + 0.5*(b_j*b_j + E_j*E_j)]
    Flux_V(RhoUx_:RhoUz_) = Un*Rho*State_V(Ux_:Uz_) + p*Normal_D &
         - Bn*FullB_D - En*E_D + pTotal2*Normal_D

    if(UseB0) Flux_V(RhoUx_:RhoUz_) = Flux_V(RhoUx_:RhoUz_) &
         - sum(B0_D*Normal_D)*State_V(Bx_:Bz_)

    pTotal = p + pTotal
    ! f_i[b_k]=u_i*(b_k+B0_k) - u_k*(b_i+B0_i)
    Flux_V(Bx_:Bz_) = Un*FullB_D - u_D*FullBn

    ! f_i[p]=u_i*p
    Flux_V(p_) = Un*p

    ! f_i[e]=(u_i*(ptotal+e+(b_k*B0_k))-(b_i+B0_i)*(b_k*u_k))
    Flux_V(Energy_) = &
         Un*(pTotal + e) - FullBn*sum(u_D*State_V(Bx_:Bz_))

    ! if(UseAnisoPressure)then
    !    ! f_i[rhou_k] = f_i[rho_k] + (ppar - pperp)bb for anisopressure
    !    ! ppar - pperp = ppar - (3*p - ppar)/2 = 3/2*(ppar - p)
    !    if (.not. UseAnisoPe) then
    !       ! In isotropic electron case, no electron contributions
    !       DpPerB = 1.5*(State_V(Ppar_) - p)*FullBn/max(1e-30, FullB2)
    !    else
    !       ! In anisotropic electron case, only (Pepar - Pperp) contributes
    !       DpPerB = 1.5*(State_V(Ppar_) + State_V(Pepar_) &
    !            - p - State_V(Pe_))*FullBn/max(1e-30, FullB2)
    !    end if
    !    Flux_V(RhoUx_:RhoUz_) = Flux_V(RhoUx_:RhoUz_) + FullB_D*DpPerB
    !    ! f_i[Ppar] = u_i*Ppar
    !    Flux_V(Ppar_)  = Un*State_V(Ppar_)
    !    Flux_V(Energy_) = Flux_V(Energy_) + DpPerB*sum(u_D*FullB_D)
    ! end if

    ! For electron pressure equation
    ! HallUn = Un

  end subroutine get_boris_flux
  !============================================================================
  subroutine get_speed_max(DoTestSide, State_V, Normal_D, &
       Un, B0_D, Cmax, Cleft, Cright)
    !$acc routine seq

    ! Using primitive variable State_V and normal direction get
    ! normal velocity and wave speeds.

    logical, intent(in):: DoTestSide
    real, intent(in) :: State_V(nVar), Normal_D(3)
    real, intent(out):: Un              ! normal velocity (signed)
    real, intent(in) :: B0_D(3)         ! B0 field on the face
    real, intent(out), optional:: Cmax  ! maximum speed (positive)
    real, intent(out), optional:: Cleft ! fastest left wave (usually negative)
    real, intent(out), optional:: Cright! fastest right wave (usually positive)

    real:: InvRho, Bn, B2
    real:: Sound2, Fast2, Discr, Fast
    real:: GammaP
    !--------------------------------------------------------------------------
    if(UseBorisCorrection)then
       call get_boris_speed(DoTestSide, State_V, Normal_D, Un, B0_D, &
            Cmax, Cleft, Cright)
       RETURN
    end if

    InvRho = 1.0/State_V(Rho_)
    if(UseB0)then
       Bn  = sum((State_V(Bx_:Bz_) + B0_D)*Normal_D)
       B2  = sum((State_V(Bx_:Bz_) + B0_D)**2)
    else
       Bn  = sum(State_V(Bx_:Bz_)*Normal_D)
       B2  = sum(State_V(Bx_:Bz_)**2)
    end if

    GammaP = State_V(p_)*Gamma

    if(UseElectronPressure) GammaP = GammaP + GammaElectron*State_V(Pe_)

    if(UseAlfvenWaves) GammaP = GammaP &
         + GammaWave*(GammaWave - 1)*sum(State_V(WaveFirst_:WaveLast_))

#ifdef TESTACC
    if(DoTestSide) then
       write(*,*)&
            'Sound2 uninitialized= ', Sound2, iSideTest
       write(*,*)&
            'GammaP, InvRho= ', GammaP, InvRho
    end if
#endif

    Sound2=GammaP*InvRho

#ifdef TESTACC
    if(DoTestSide) then
       write(*,*)&
            'Sound2 updated ', Sound2, iSideTest
    end if
#endif

    Fast2 = Sound2 + InvRho*B2
    Discr = sqrt(max(0.0, Fast2**2 - 4*Sound2*InvRho*Bn**2))
    Fast  = sqrt( 0.5*(Fast2 + Discr) )

    Un = sum(State_V(Ux_:Uz_)*Normal_D)
    if(present(Cmax))   Cmax   = abs(Un) + Fast
    if(present(Cleft))  Cleft  = Un - Fast
    if(present(Cright)) Cright = Un + Fast

#ifdef TESTACC
    if(DoTestSide)then
       write(*,*) ' iFluid, rho, p(face)   =', &
            1, State_V(Rho_), State_V(p_), iSideTest
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
            ' Fast2, Discr          =', Fast2, Discr, iSideTest
       write(*,*) &
            ' GammaP, InvRho         =', GammaP, InvRho, iSideTest
       write(*,*) &
            ' Sound2, Alfven2       =', Sound2, InvRho*B2, iSideTest
       write(*,*) &
            ' FullBn, Alfven2Normal =', Bn, InvRho*Bn**2, iSideTest
       if(UseB0)then
          write(*,*) ' FullB=', State_V(Bx_) + B0_D(1), &
               State_V(By_) + B0_D(2),  State_V(Bz_) + B0_D(3), iSideTest
       else
          write(*,*) ' B=', State_V(Bx_), State_V(By_), State_V(Bz_), iSideTest
       end if

    end if
#endif

  end subroutine get_speed_max
  !============================================================================
  subroutine get_boris_speed(DoTestSide, State_V, Normal_D, Un, B0_D, &
       Cmax, Cleft, Cright)
    !$acc routine seq

    ! Using primitive variable State_V and normal direction get
    ! normal velocity and wave speeds with semi-relativistic Boris correction

    logical, intent(in):: DoTestSide ! side of cell being tested
    real, intent(in) :: State_V(nVar), Normal_D(3)
    real, intent(out):: Un              ! normal velocity (signed)
    real, intent(in) :: B0_D(3)         ! B0 field on the face
    real, intent(out), optional:: Cmax  ! maximum speed (positive)
    real, intent(out), optional:: Cleft ! fastest left wave (usually negative)
    real, intent(out), optional:: Cright! fastest right wave (usually positive)

    real :: InvRho, Sound2, FullB_D(3), FullBn, FullB2
    real :: p  ! , Ppar, Pperp, BnInvB2, GammaPe
    real :: Alfven2, Alfven2Normal, Fast2, Discr, Fast, Slow
    real :: GammaA2, GammaU2
    real :: UnBoris, Sound2Boris, Alfven2Boris, Alfven2NormalBoris
    ! No explicit formula for multi-ion fluids
    ! if (nTrueIon > 1) call stop_mpi &
    !     ('get_boris_speed should not be called with multi-ion fluids')

    !--------------------------------------------------------------------------
    InvRho = 1.0/State_V(Rho_)
    ! iPIon_I = p_ for single ion MHD case. iPIon_I is need to add the
    ! electron pressure(s) for single ion five- and six-moment case.
    p = State_V(p_) ! sum(State_V(iPIon_I))
    FullB_D = State_V(Bx_:Bz_)
    if(UseB0) FullB_D = FullB_D + B0_D
    FullB2 = sum(FullB_D**2)
    FullBn = sum(Normal_D*FullB_D)

    ! Calculate sound speed squared
    ! if(UseAnisoPressure .and. FullB2 > 0 .and. .not. UseAnisoPe)then
    !   ! iPparIon_I = Ppar_ for single ion MHD case. iPparIon_I is need to
    !   ! add the electron pressure(s) for single ion six-moment case.
    !   Ppar  = sum(State_V(iPparIon_I))
    !   Pperp = (3*p - Ppar)/2.
    !   BnInvB2 = FullBn**2/FullB2
    !   Sound2 = InvRho*(2*Pperp + (2*Ppar - Pperp)*BnInvB2)
    ! else if (UseAnisoPressure .and. FullB2 > 0 .and. useAnisoPe)then
    !   ! In the anisotropic electron pressure case, Pe is added to the
    !   ! total pressure while Pepar is added to the total Ppar.
    !   p     = p + State_V(Pe_)
    !   Ppar  = Ppar + State_V(Pepar_)
    !   Pperp = (3*p - Ppar)/2.
    !   BnInvB2 = FullBn**2/FullB2
    !   Sound2 = InvRho*(2*Pperp + (2*Ppar - Pperp)*BnInvB2)
    ! else
    Sound2 = InvRho*Gamma*p
    ! end if

    ! Add contribution of electron pressure for the isotropic Pe case.
    ! if(UseElectronPressure .and. .not. UseAnisoPe)then
    !    GammaPe = GammaElectron*State_V(Pe_)
    !    Sound2  = Sound2 + InvRho*GammaPe
    ! else
    !    ! For five- and six-moment, Pe should be 0 because electron pressure
    !    ! has already been added.
    !    GammaPe = 0.0
    ! end if

    ! Wave pressure = (GammaWave - 1)*WaveEnergy
    ! if(UseWavePressure) Sound2 = Sound2 + InvRho*GammaWave &
    !      * (GammaWave - 1)*sum(State_V(WaveFirst_:WaveLast_))

    Alfven2       = InvRho*FullB2
    Alfven2Normal = InvRho*FullBn**2

    Un = sum(State_V(Ux_:Uz_)*Normal_D)

    ! "Alfven Lorentz" factor
    GammaA2 = 1.0/(1.0 + Alfven2*InvClight2)

    ! 1-gA^2*Un^2/c^2
    GammaU2 = max(0.0, 1.0 - GammaA2*Un**2*InvClight2)

    ! Modified speeds
    Sound2Boris        = Sound2*GammaA2*(1+Alfven2Normal*InvClight2)
    Alfven2Boris       = Alfven2*GammaA2*GammaU2
    Alfven2NormalBoris = Alfven2Normal*GammaA2*GammaU2

    ! Approximate slow and fast wave speeds
    Fast2 = Sound2Boris + Alfven2Boris

    ! if(UseAnisoPressure .and. FullB2 > 0)then
    !    Discr = sqrt(max(0.0, Fast2**2  &
    !         + 4*((Pperp*InvRho)**2*BnInvB2*(1 - BnInvB2) &
    !         - 3*Ppar*Pperp*InvRho**2*BnInvB2*(2 - BnInvB2) &
    !         + 3*Ppar*Ppar*(InvRho*BnInvB2)**2 &
    !         - (3*Ppar + GammaPe)*InvRho*Alfven2NormalBoris &
    !         + GammaPe*InvRho**2*(4*Ppar*BnInvB2 &
    !         - 3*Ppar - Pperp*BnInvB2)*BnInvB2)))
    !
    ! else
    Discr = sqrt(max(0.0, Fast2**2 - 4.0*Sound2*Alfven2NormalBoris))
    ! end if

    ! Get fast and slow speeds multiplied with the face area
    Fast = sqrt( 0.5*(          Fast2 + Discr) )
    Slow = sqrt( 0.5*( max(0.0, Fast2 - Discr) ) )

    ! In extreme cases "slow" wave can be faster than "fast" wave
    ! so take the maximum of the two

    UnBoris            = Un*GammaA2
    if(present(Cmax))   Cmax   = max(abs(UnBoris) + Fast, abs(Un) + Slow)
    if(present(Cleft))  Cleft  = min(UnBoris - Fast, Un - Slow)
    if(present(Cright)) Cright = max(UnBoris + Fast, Un + Slow)

#ifdef TESTACC
    if(DoTestSide)then
       write(*,*) ' InvRho, p      =', InvRho, p, iSideTest
       write(*,*) ' FullB, FullBn  =', FullB_D(1), FullB_D(2), FullB_D(3), &
            FullBn, iSideTest
       write(*,*) ' Sound2,Alfven2 =', Sound2, Alfven2, iSideTest
       write(*,*) ' GammaA2,GammaU2=', GammaA2, GammaU2, iSideTest
       write(*,*) ' Sound2Boris,Alfven2Boris,Normal=', &
            Sound2Boris, Alfven2Boris, Alfven2NormalBoris, iSideTest
       write(*,*) ' Fast2, Discr   =', Fast2, Discr, iSideTest
       write(*,*) ' Fast, Slow     =', Fast, Slow, iSideTest
       write(*,*) ' Un, UnBoris    =', Un, UnBoris, iSideTest
    end if
#endif

  end subroutine get_boris_speed
  !============================================================================
  subroutine get_numerical_flux(Normal_D, Area, &
       StateLeft_V, StateRight_V, Flux_V, B0_D, DoTestSide)
    !$acc routine seq

    ! Calculate numerical flux Flux_V based on the left and right states
    ! and the B0 field along the normal direction Normal_D. The flux
    ! returns the Area that may be negative for certain update schemes.

    real, intent(in)   :: Normal_D(3), Area
    real, intent(inout):: StateLeft_V(nVar), StateRight_V(nVar)
    real, intent(out)  :: Flux_V(nFaceValue)
    real, intent(in)   :: B0_D(3)
    logical, intent(in):: DoTestSide

    ! Average state
    real:: State_V(nVar)

    ! Conservative variables
    real :: StateLeftCons_V(nFlux), StateRightCons_V(nFlux)

    ! Left and right fluxes
    real :: FluxLeft_V(nFlux+1), FluxRight_V(nFlux+1)

    ! Left, right and maximum speeds, normal velocity, jump in Bn
    real :: Cleft, Cright, Cmax, Un, DiffBn, CleftAverage, CrightAverage

    real :: AreaInvCdiff, Cproduct, Bn

    integer :: iVar
    !--------------------------------------------------------------------------
    if(DoLf)then
       ! Rusanov scheme

       ! average state
       State_V = 0.5*(StateLeft_V + StateRight_V)

       call get_speed_max(DoTestSide, State_V, Normal_D, Un, B0_D, Cmax)
       call get_physical_flux(StateLeft_V, Normal_D, &
            StateLeftCons_V, FluxLeft_V, B0_D)
       call get_physical_flux(StateRight_V, Normal_D, &
            StateRightCons_V, FluxRight_V, B0_D)

       ! Lax-Friedrichs flux
       Flux_V(1:nFlux) = &
            Area*0.5* (FluxLeft_V(1:nFlux) + FluxRight_V(1:nFlux) &
            +          Cmax*(StateLeftCons_V - StateRightCons_V))

       if(nFluid == 1)then
          if(UseNonConservative .or. UseAlfvenWaves) &
               Flux_V(UnFirst_) = Area*0.5* &
               sum((StateLeft_V(Ux_:Uz_) + StateRight_V(Ux_:Uz_))*Normal_D)
       else
          if(UseNonConservative) Flux_V(UnFirst_:UnFirst_+nFluid-1) = &
               Area*0.5* &
               ( (StateLeft_V(iUx_I) + StateRight_V(iUx_I))*Normal_D(1) &
               + (StateLeft_V(iUy_I) + StateRight_V(iUy_I))*Normal_D(2) &
               + (StateLeft_V(iUz_I) + StateRight_V(iUz_I))*Normal_D(3) )
       end if
       if(UseElectronPressure) Flux_V(UnLast_) = Area*0.5* &
            sum((StateLeft_V(Ux_:Uz_) + StateRight_V(Ux_:Uz_))*Normal_D)

       ! Store Bnormal
       if(UseB .and. UseDivbSource) Flux_V(Bn_) = Area*0.5* &
            sum((StateLeft_V(Bx_:Bz_) + StateRight_V(Bx_:Bz_))*Normal_D)

       ! Store Enormal
       if(UseBorisCorrection .and. ClightFactor /= 1.0) &
            Flux_V(En_) = Area*0.5*(FluxLeft_V(nFlux+1) + FluxRight_V(nFlux+1))
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
       call get_speed_max(DoTestSide, StateLeft_V, Normal_D, Un, B0_D, &
            Cleft=Cleft)

       ! Right speed of right state
       call get_speed_max(DoTestSide, StateRight_V, Normal_D, Un, B0_D, &
            Cright=Cright)

       ! Speeds of average state
       State_V = 0.5*(StateLeft_V + StateRight_V)
       call get_speed_max(DoTestSide, State_V, Normal_D, &
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
            ( Cright*FluxLeft_V(1:nFlux) - Cleft*FluxRight_V(1:nFlux)  &
            + Cproduct*(StateRightCons_V - StateLeftCons_V) )

       if(nFluid == 1)then
          if(UseNonConservative .or. UseAlfvenWaves) &
               Flux_V(UnFirst_) = AreaInvCDiff* &
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
       if(UseElectronPressure) Flux_V(UnLast_) = AreaInvCDiff* &
            ( (Cright*StateLeft_V(Ux_) &
            -  Cleft*StateRight_V(Ux_))*Normal_D(1) &
            + (Cright*StateLeft_V(Uy_) &
            -  Cleft*StateRight_V(Uy_))*Normal_D(2) &
            + (Cright*StateLeft_V(Uz_) &
            -  Cleft*StateRight_V(Uz_))*Normal_D(3) )

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

          ! Store Enormal
          if(UseBorisCorrection .and. ClightFactor /= 1.0) &
               Flux_V(En_) = AreaInvCdiff* &
               (Cright*FluxLeft_V(nFlux+1) - Cleft*FluxRight_V(nFlux+1))

       end if
    end if

    ! Store time step constraint (to be generalized for multifluid)
    Flux_V(Vdt_) = abs(Area)*Cmax

#ifdef TESTACC
    if(DoTestSide)then
       write(*,*)'Hat state for Normal_D=', &
            Normal_D(1), Normal_D(2), Normal_D(3), iSideTest
       write(*,*)'rho=',0.5*(StateLeft_V(Rho_)+StateRight_V(Rho_)), iSideTest
       write(*,*)'Un =',Un, iSideTest
       write(*,*)'P  =',0.5*(StateLeft_V(P_)+StateRight_V(P_)), iSideTest
       if(UseB)then
          if(UseB0)then
             write(*,*)'B  =', &
                  0.5*(StateLeft_V(Bx_)+ StateRight_V(Bx_)) + B0_D(1),  &
                  0.5*(StateLeft_V(By_)+ StateRight_V(By_)) + B0_D(2),  &
                  0.5*(StateLeft_V(Bz_)+ StateRight_V(Bz_)) + B0_D(3),  &
                  iSideTest
          else
             write(*,*)'B  =', &
                  0.5*(StateLeft_V(Bx_)+ StateRight_V(Bx_)),  &
                  0.5*(StateLeft_V(By_)+ StateRight_V(By_)),  &
                  0.5*(StateLeft_V(Bz_)+ StateRight_V(Bz_)),  &
                  iSideTest
          end if
          write(*,*)'BB =', &
               sum((0.5*(StateLeft_V(Bx_:Bz_) + StateRight_V(Bx_:Bz_)) + B0_D &
               )**2), iSideTest
       end if
       write(*,*)
       write(*,*) 'Area=', Area, iSideTest
       write(*,*)'Eigenvalue_maxabs=', Cmax, iSideTest
       write(*,*)'CmaxDt           =', Cmax, iSideTest
       do iVar = 1, nFlux
          !#ifdef _OPENACC
          write(*,*) 'Var,F,F_L,F_R,dU,c*dU/2=', &
               NameVar_V(iVar),&
               Flux_V(iVar), FluxLeft_V(iVar)*Area, FluxRight_V(iVar)*Area, &
               StateRightCons_V(iVar)-StateLeftCons_V(iVar), &
               0.5*Cmax*(StateRightCons_V(iVar)-StateLeftCons_V(iVar))*Area, &
               iSideTest
          !#else
          ! write(*,'(a,a8,5es13.5,i3)') 'Var,F,F_L,F_R,dU,c*dU/2=', &
          !      NameVar_V(iVar),&
          !      Flux_V(iVar), FluxLeft_V(iVar)*Area, FluxRight_V(iVar)*Area,&
          !      StateRightCons_V(iVar)-StateLeftCons_V(iVar),&
          !      0.5*Cmax*(StateRightCons_V(iVar)-StateLeftCons_V(iVar))*Area,&
          !      iSideTest
          !#endif
       end do
    end if
#endif

  end subroutine get_numerical_flux
  !============================================================================
  subroutine boris_to_mhd(State_V, B0_D, IsConserv)
    !$acc routine seq

    real, intent(inout):: State_V(nVar)
    real,    intent(in):: B0_D(3)
    logical, intent(in):: IsConserv

    ! Replace semi-relativistic momentum with classical momentum in State_V.
    ! Replace semi-relativistic energy density Energy with classical value.
    ! for conservative scheme.
    ! Use B0=B0_D in the total magnetic field if present.

    real:: RhoC2, b_D(3), RhoUBoris_D(3), u_D(3)
    !--------------------------------------------------------------------------
    b_D = State_V(Bx_:Bz_)
    if(UseB0) b_D = b_D + B0_D

    RhoC2       = State_V(Rho_)*C2Light
    RhoUBoris_D = State_V(RhoUx_:RhoUz_)

    ! Gombosi et al. 2001, eq(16) with vA^2 = B^2/Rho, gA^2=1/(1+vA^2/c^2)
    !
    ! RhoU = [RhoUBoris + B*B.RhoUBoris/(Rho c^2)]/(1+B^2/Rho c^2)
    !      = (RhoUBoris*Rho*c^2 + B*B.RhoUBoris)/(Rho*c^2 + B^2)

    State_V(RhoUx_:RhoUz_) =(RhoC2*RhoUBoris_D + b_D*sum(b_D*RhoUBoris_D)) &
         /(RhoC2 + sum(b_D**2))

    ! No need to set energy for non-conservative scheme
    if(UseNonConservative)then
       if(nConservCrit == 0) RETURN
       if(.not.IsConserv) RETURN
    end if
    ! e = e_boris - (U x B)^2/(2 c^2)   eq 92
    u_D = State_V(RhoUx_:RhoUz_)/State_V(Rho_)
    State_V(p_) = State_V(p_) &
         - 0.5*sum(cross_prod(u_D, b_D)**2)*InvClight2

  end subroutine boris_to_mhd
  !============================================================================
  subroutine mhd_to_boris(State_V, B0_D, IsConserv)
    !$acc routine seq

    ! Replace classical momentum with  semi-relativistic momentum in State_V.
    ! Replace classical energy density with semi-relativistic value for
    ! conservative scheme.
    ! Use B0=B0_D in the total magnetic field if present.

    real, intent(inout):: State_V(nVar)
    real,    intent(in):: B0_D(3)
    logical, intent(in):: IsConserv

    real:: Rho, b_D(3), u_D(3)
    !--------------------------------------------------------------------------
    b_D = State_V(Bx_:Bz_)
    if(UseB0) b_D = b_D + B0_D

    Rho = State_V(Rho_)
    u_D = State_V(RhoUx_:RhoUz_)/Rho

    ! Gombosi et al. 2001, eq(12) with vA^2 = B^2/Rho
    !
    ! RhoUBoris = RhoU + (RhoU B^2 - B RhoU.B)/(Rho c^2)
    !           = U*(Rho + B^2/c^2 - B U.B/c^2
    State_V(RhoUx_:RhoUz_) = u_D*(Rho + sum(b_D**2)*InvClight2) &
         - b_D*sum(u_D*b_D)*InvClight2

    ! No need to set energy for non-conservative scheme
    if(UseNonConservative)then
       if(nConservCrit == 0) RETURN
       if(.not.IsConserv) RETURN
    end if
    ! e_Boris = e + (UxB)^2/(2 c^2)   eq 92
    State_V(p_) = State_V(p_) &
         + 0.5*sum(cross_prod(u_D, b_D)**2)*InvClight2

  end subroutine mhd_to_boris
  !============================================================================
  subroutine limit_pressure(State_V)
    !$acc routine seq
    real, intent(inout):: State_V(nVar)

    integer:: iFluid
    !--------------------------------------------------------------------------
    do iFluid = 1, nFluid
       State_V(iP_I(iFluid)) = max(pMin_I(iFluid), State_V(iP_I(iFluid)))
    end do

  end subroutine limit_pressure
  !============================================================================
  subroutine energy_to_pressure(State_V)
    !$acc routine seq

    ! Calculate pressure from energy density
    real, intent(inout):: State_V(nVar)

    integer:: iFluid

    ! Subtract magnetic energy from the first fluid for MHD
    !--------------------------------------------------------------------------
    if(IsMhd) State_V(p_) = State_V(p_) -  0.5*sum(State_V(Bx_:Bz_)**2)

    ! Convert hydro energy density to pressure
    State_V(p_) = GammaMinus1*( State_V(p_) &
         - 0.5*sum(State_V(RhoUx_:RhoUz_)**2)/State_V(Rho_) )

    ! Deal with other fluids
    do iFluid = 2, nFluid
       State_V(iP_I(iFluid)) = GammaMinus1_I(iFluid)*( State_V(iP_I(iFluid)) &
            - 0.5* &
            ( State_V(iRhoUx_I(iFluid))**2 &
            + State_V(iRhoUy_I(iFluid))**2 &
            + State_V(iRhoUz_I(iFluid))**2 ) / State_V(iRho_I(iFluid)) )
    end do

    if(UsePmin) call limit_pressure(State_V)

  end subroutine energy_to_pressure
  !============================================================================
  subroutine pressure_to_energy(State_V)
    !$acc routine seq

    ! Calculate energy density from pressure
    real, intent(inout):: State_V(nVar)

    integer:: iFluid
    !--------------------------------------------------------------------------
    if(UsePmin) call limit_pressure(State_V)

    ! Calculate hydro energy density
    State_V(p_) = State_V(p_)*InvGammaMinus1 &
         + 0.5*sum(State_V(RhoUx_:RhoUz_)**2)/State_V(Rho_)
    do iFluid = 2, nFluid
       State_V(iP_I(iFluid)) = State_V(iP_I(iFluid))*InvGammaMinus1_I(iFluid) &
            + 0.5* &
            ( State_V(iRhoUx_I(iFluid))**2 &
            + State_V(iRhoUy_I(iFluid))**2 &
            + State_V(iRhoUz_I(iFluid))**2 ) / State_V(iRho_I(iFluid))
    end do
    ! Add magnetic energy to first fluid for MHD
    if(IsMhd) State_V(p_) = State_V(p_) + 0.5*sum(State_V(Bx_:Bz_)**2)

  end subroutine pressure_to_energy
  !============================================================================
#endif
  subroutine update_b0_fast

    ! Update B0 due to the rotation of the dipole

    integer:: i, j, k, iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'update_b0_fast'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    call timing_start(NameSub)

    call sync_cpu_gpu('update on GPU', NameSub, State_VGB, B0_DGB)
    call sync_cpu_gpu('change on GPU', NameSub, B0_DGB=B0_DGB)

    call set_dipole

    !$acc parallel loop gang independent
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE

       !$acc loop vector collapse(3) independent
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          State_VGB(Bx_:Bz_,i,j,k,iBlock) = &
               State_VGB(Bx_:Bz_,i,j,k,iBlock) + B0_DGB(:,i,j,k,iBlock)
          call get_b0_dipole(Xyz_DGB(:,i,j,k,iBlock), B0_DGB(:,i,j,k,iBlock))
          if(Used_GB(i,j,k,iBlock))then
             State_VGB(Bx_:Bz_,i,j,k,iBlock) = &
                  State_VGB(Bx_:Bz_,i,j,k,iBlock) - B0_DGB(:,i,j,k,iBlock)
          else
             State_VGB(Bx_:Bz_,i,j,k,iBlock) = 0.0
          end if
       end do; end do; end do
    end do

    ! messsage pass to fix B1 in the ghost cells ?!
    ! set B0 at reschange ?!

    call timing_stop(NameSub)
    call test_stop(NameSub, DoTest)

  end subroutine update_b0_fast
  !============================================================================
  subroutine calc_inner_bc_velocity(tSimulation, Xyz_D, b_D, u_D)
    !$acc routine seq

    use ModIeCoupling, ONLY: dIonoPotential_DII, rIonosphere, &
         dThetaIono, dPhiIono
    use ModCoordTransform, ONLY: xyz_to_dir
    use CON_planet_field,  ONLY: map_planet_field_fast

    real, intent(in)    :: tSimulation      ! Simulation time
    real, intent(in)    :: Xyz_D(3)    ! Position vector
    real, intent(in)    :: b_D(3)      ! Magnetic field

    real, intent(out)   :: u_D(3)      ! Velocity vector

    real :: XyzIono_D(3)    ! Mapped point on the ionosphere
    real :: Theta, Phi           ! Mapped point colatitude, longitude
    real :: ThetaNorm, PhiNorm   ! Normalized colatitude, longitude
    real :: Dist1, Dist2         ! Distance from ionosphere grid point

    real :: dPotential_D(2)      ! Gradient of potential at the mapped position
    real :: DdirDxyz_DD(2,3)     ! Jacobian matrix between Theta, Phi and Xyz_D
    real :: eField_D(3)     ! Electric field
    real :: B2                   ! Magnetic field squared

    integer :: iTheta, iPhi, iHemisphere

    ! Map down to the ionosphere at radius rIonosphere. Result is in SMG.
    ! Also obtain the Jacobian matrix between Theta,Phi and Xyz_D
    character(len=*), parameter:: NameSub = 'calc_inner_bc_velocity'
    !--------------------------------------------------------------------------
    call map_planet_field_fast(Xyz_D, rIonosphere, XyzIono_D, &
         iHemisphere, DdirDxyz_DD, UseGsmIn=.true., DoNotConvertBack=.true.)

    ! Calculate angular coordinates
    call xyz_to_dir(XyzIono_D, Theta, Phi)

    ThetaNorm = Theta / dThetaIono
    PhiNorm   = Phi   / dPhiIono

    iTheta    = floor(ThetaNorm) + 1
    iPhi      = floor(PhiNorm)   + 1

    Dist1     = ThetaNorm - (iTheta - 1)
    Dist2     = PhiNorm   - (iPhi   - 1)

    dPotential_D = &
         (1 - Dist1)*( (1-Dist2) * dIonoPotential_DII(:, iTheta  , iPhi  )  &
         +             Dist2     * dIonoPotential_DII(:, iTheta  , iPhi+1)) &
         + Dist1    *( (1-Dist2) * dIonoPotential_DII(:, iTheta+1, iPhi  )  &
         +             Dist2     * dIonoPotential_DII(:, iTheta+1, iPhi+1))

    ! E = -grad(Potential) = - dPotential/d(Theta,Phi) * d(Theta,Phi)/d(x,y,z)
    eField_D = - matmul23_right( dPotential_D, DdirDxyz_DD)

    ! Magnetic field
    B2  = sum(b_D**2)

    ! U = (E x B) / B^2
    u_D = cross_prod(eField_D, b_D) / B2
  end subroutine calc_inner_bc_velocity
  !============================================================================
end module ModUpdateStateFast
!==============================================================================
