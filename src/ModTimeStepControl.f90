!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModTimeStepControl

  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest, iBlockTest, &
       iProc, nProc, iComm
  use ModBatsrusUtility, ONLY: stop_mpi

  implicit none

  SAVE

  private ! except

  ! Public methods
  public:: read_time_step_control_param
  public:: calc_timestep
  public:: set_global_timestep
  public:: control_time_step

  logical, public:: UseTimeStepControl  = .false.
  real,    public:: TimeStepControlInit = 1.0
  logical, public:: UseMaxTimeStep      = .false. ! calculate max time step
  real,    public:: DtMin = -1.0, DtMax = -1.0    ! value of min/max time steps

  ! Local variables
  integer             :: nVarControl = -1
  integer, allocatable:: iVarControl_I(:)
  real,    allocatable:: VarRatio_I(:)

  real :: &
       RejectStepLevel1   = 0.3, &
       RejectStepLevel2   = 3.0, &
       RejectStepFactor   = 0.50, &
       ReduceStepLevel1   = 0.6, &
       ReduceStepLevel2   = 1.5, &
       ReduceStepFactor   = 0.95, &
       IncreaseStepLevel1 = 0.8, &
       IncreaseStepLevel2 = 1.2, &
       IncreaseStepFactor = 1.05

  ! Minimum time step checking
  logical, public:: DoCheckTimeStep = .false.
  integer, public:: DnCheckTimeStep = 1, iCheckTimeStep = 0
  real,    public:: TimeStepMin = 0.0
  real,    public:: TimeSimulationOldCheck

contains
  !============================================================================
  subroutine read_time_step_control_param(NameCommand)

    use ModMain,       ONLY: NameVarLower_V
    use ModReadParam
    use ModVarIndexes, ONLY: nVar
    use ModUtilities,  ONLY: split_string

    character(len=*), intent(in):: NameCommand

    integer :: iControl, iVar
    character(len=100):: NameVarControl
    character(len=20) :: NameVarControl_I(nVar)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_time_step_control_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(NameCommand)
    case("#CONTROLTIMESTEP", "#TIMESTEPCONTROL")
       call read_var('UseTimeStepControl', UseTimeStepControl)
    case("#CONTROLINIT")
       call read_var('TimeStepControlInit', TimeStepControlInit)
    case("#CONTROLDECREASE")
       call read_var('RejectStepLevel1' ,  RejectStepLevel1)
       call read_var('ReduceStepLevel1' ,  ReduceStepLevel1)
       call read_var('IncreaseStepLevel1', IncreaseStepLevel1)
    case("#CONTROLINCREASE")
       call read_var('RejectStepLevel2' ,  RejectStepLevel2)
       call read_var('ReduceStepLevel2' ,  ReduceStepLevel2)
       call read_var('IncreaseStepLevel2', IncreaseStepLevel2)
    case("#CONTROLFACTOR")
       call read_var('RejectStepFactor',   RejectStepFactor)
       call read_var('ReduceStepFactor',   ReduceStepFactor)
       call read_var('IncreaseStepFactor', IncreaseStepFactor)
    case("#CONTROLVAR")
       call read_var('NameVarControl', NameVarControl, IsLowerCase=.true.)
       ! Split list of variables and match with NameVar_V
       call split_string(NameVarControl, nVar, NameVarControl_I, nVarControl)
       if(allocated(iVarControl_I)) deallocate(iVarControl_I, VarRatio_I)
       allocate( iVarControl_I(nVarControl), VarRatio_I(nVarControl) )
       do iControl=1, nVarControl
          do iVar = 1, nVar
             if(NameVarLower_V(iVar) == NameVarControl_I(iControl)) EXIT
          end do
          if(iVar > nVar)call stop_mpi(NameSub//' invalid NameVarControl='//&
               NameVarControl_I(iControl))
          iVarControl_I(iControl) = iVar
       end do
    case("#CHECKTIMESTEP")
       call read_var('DoCheckTimeStep', DoCheckTimeStep)
       call read_var('DnCheckTimeStep', DnCheckTimeStep)
       call read_var('TimeStepMin'   ,  TimeStepMin)
    case default
       call stop_mpi(NameSub//' invalid NameCommand='//NameCommand)
    end select

    call test_stop(NameSub, DoTest)
  end subroutine read_time_step_control_param
  !============================================================================
  subroutine calc_timestep(iBlock, IsPartLocal)
    !$acc routine vector

    use ModVarIndexes, ONLY: WaveFirst_, WaveLast_
    use ModSize, ONLY: nI, nJ, nK
    use ModMain, ONLY: UseDtFixed, Dt, DtFixed, DtMax_B, Cfl, &
         UseDtLimit, DtLimit, rLocalTimeStep, UseUserTimeStep
    use ModAdvance, ONLY : DtMax_CB, Flux_VXI, Flux_VYI, Flux_VZI, Vdt_, &
         DoFixAxis, rFixAxis, r2FixAxis, State_VGB, DoUpdate_V
    use ModGeometry, ONLY: Used_GB, IsNoBody_B, rMin_B, r_GB
    use ModCoronalHeating, ONLY: get_cell_heating
    use ModTurbulence, ONLY: UseAlfvenWaveDissipation
    use BATL_lib, ONLY: CellVolume_GB, CoordMin_DB, CoordMax_DB, &
         IsCylindricalAxis, IsLatitudeAxis, r_, Lat_
    use ModNumConst, ONLY: cHalfPi
    use ModCoarseAxis, ONLY: UseCoarseAxis, calc_coarse_axis_timestep,&
         NorthHemiSph_, SouthHemiSph_
    use ModUtilities, ONLY: i_gang

    ! Calculate stable time step DtMax_CB for each cell in block iBlock.
    ! If IsPartLocal is present, limit DtMax_CB with Dt/Cfl.

    integer, intent(in) :: iBlock
    logical, optional, intent(in):: IsPartLocal

    integer:: i, j, k, Di, Dk, iGang
    real:: Vdt, DtBlock

    ! Variables for time step control due to loss terms
    real :: DtLoss, WaveDissipationRate_V(WaveFirst_:WaveLast_), CoronalHeating

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_timestep'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    iGang = i_gang(iBlock)

    if(DoTest)write(*,*) NameSub,' starting, IsNoBody_B=', IsNoBody_B(iBlock)

    ! Calculate time step limit based on maximum speeds across 6 faces
    !$acc loop vector collapse(3)
    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if(.not. Used_GB(i,j,k,iBlock)) then
          DtMax_CB(i,j,k,iBlock) = 0
       else
          Vdt =  max(Flux_VXI(Vdt_,i,j,k,iGang), Flux_VXI(Vdt_,i+1,j,k,iGang))
          if(nJ > 1) Vdt = Vdt &
               + max(Flux_VYI(Vdt_,i,j,k,iGang), Flux_VYI(Vdt_,i,j+1,k,iGang))
          if(nK > 1) Vdt = Vdt &
               + max(Flux_VZI(Vdt_,i,j,k,iGang), Flux_VZI(Vdt_,i,j,k+1,iGang))
          DtMax_CB(i,j,k,iBlock) = CellVolume_GB(i,j,k,iBlock) / Vdt
       end if
    end do; end do; end do

    if(UseCoarseAxis)then
       if(CoordMax_DB(Lat_,iBlock) > cHalfPi-1e-8)then
          call calc_coarse_axis_timestep(iBlock,NorthHemiSph_)
       elseif(CoordMin_DB(Lat_,iBlock) < -cHalfPi+1e-8)then
          call calc_coarse_axis_timestep(iBlock,SouthHemiSph_)
       end if
#ifndef _OPENACC
    elseif(DoFixAxis)then
       ! Use the time step in the super cell of the cell just outside.
       ! In time accurate this removes the time step constraints from supercell
       ! In local time stepping mode it increases the time step
       if(IsCylindricalAxis)then
          if(CoordMin_DB(r_,iBlock) <= 0.0)then
             Di = 1; if(r2FixAxis > 0.0) Di = 2
             do j = 1, nJ; do i = 1, nI
                DtMax_CB(1:Di,j,k,iBlock) = DtMax_CB(Di+1,j,k,iBlock)
             end do; end do
          end if
       elseif(IsLatitudeAxis .and. rMin_B(iBlock) < rFixAxis)then
          Dk = 1; if(rMin_B(iBlock) < r2FixAxis) Dk = 2
          if(CoordMax_DB(Lat_,iBlock) > cHalfPi-1e-8)then
             do k = nK+1-Dk, nK
                DtMax_CB(1:nI,1:nJ,k,iBlock) = DtMax_CB(1:nI,1:nJ,nK-Dk,iBlock)
             end do
          end if
          if(CoordMin_DB(Lat_,iBlock) < -cHalfPi+1e-8)then
             do k = 1, Dk
                DtMax_CB(1:nI,1:nJ,k,iBlock) = DtMax_CB(1:nI,1:nJ,Dk+1,iBlock)
             end do
          end if
       end if
#endif
    end if

    ! Time step restriction due to point-wise loss terms
    ! (only explicit source terms)
    if(UseAlfvenWaveDissipation .and. DoUpdate_V(WaveFirst_) )then
       !$acc loop vector collapse(3) private(WaveDissipationRate_V, DtLoss) &
       !$acc    independent
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not. Used_GB(i,j,k,iBlock)) CYCLE

          if(all(State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock) > 0.0))then
             call get_cell_heating(i, j, k, iBlock, &
                     WaveDissipationRate_V, CoronalHeating)
             DtLoss = 1 / maxval(WaveDissipationRate_V)
             ! The following prevents the wave energies from becoming
             ! negative due to too large loss terms.
             DtMax_CB(i,j,k,iBlock) = DtMax_CB(i,j,k,iBlock)*DtLoss/&
                  (DtMax_CB(i,j,k,iBlock) + DtLoss)
          end if
       end do; end do; end do
    end if

#ifndef _OPENACC
    if(UseUserTimeStep) call user_calc_timestep(iBlock)
    if(DoTest)then
       write(*,*)NameSub,' Vdt_X(iTest:iTest+1)=', &
            Flux_VXI(Vdt_,iTest:iTest+1,jTest,kTest,iGang)
       if(nJ>1) write(*,*) NameSub,' Vdt_Y(jTest:jTest+1)=', &
            Flux_VYI(Vdt_,iTest,jTest:jTest+1,kTest,iGang)
       if(nK>1) write(*,*) NameSub,' Vdt_Z(kTest:kTest+1)=', &
            Flux_VZI(Vdt_,iTest,jTest,kTest:kTest+1,iGang)
       write(*,*) NameSub,' DtMax_CB=',DtMax_CB(iTest,jTest,kTest,iBlock)
    end if
#endif

    ! Compute maximum stable time step for this solution block
    if(IsNoBody_B(iBlock) .and. rLocalTimeStep < 0) then
       DtBlock = DtMax_CB(1,1,1,iBlock)
       !$acc loop vector independent collapse(3) reduction(min:DtBlock)
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          DtBlock = min(DtBlock, DtMax_CB(i,j,k,iBlock))
       end do; end do; end do
       DtMax_B(iBlock) = DtBlock
    else
       ! If the block has unused cells, initialize DtMax_B=1.0E20
       DtBlock = 1e20
       !$acc loop vector independent collapse(3) reduction(min:DtBlock)
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(Used_GB(i,j,k,iBlock) .and. r_GB(i,j,k,iBlock) > rLocalTimeStep) &
               DtBlock = min(DtBlock, DtMax_CB(i,j,k,iBlock))
       end do; end do; end do
       DtMax_B(iBlock) = DtBlock

       if(DoTest)write(*,*) NameSub,' minval(DtMax_CB,MASK), DtMax_B=',&
            minval(DtMax_CB(:,:,:,iBlock), &
            MASK=Used_GB(1:nI,1:nJ,1:nK,iBlock)), DtMax_B(iBlock)
    end if

#ifndef _OPENACC
    if(DoTest)write(*,*)NameSub,' DtMax_B, loc=',DtMax_B(iBlock),&
         minloc(DtMax_CB(:,:,:,iBlock),&
         MASK=Used_GB(1:nI,1:nJ,1:nK,iBlock))
#endif

    ! Reset DtMax_CB for fixed time step (but DtMax_B is kept! )
    if(UseDtFixed) then
       !$acc loop vector collapse(3)
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          DtMax_CB(i,j,k,iBlock) = DtFixed
       end do; end do; end do
    endif

    ! Limit local time step so that Cfl*DtMax_CB <= DtLimit,
    if(UseDtLimit)then
       !$acc loop vector collapse(3)
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          DtMax_CB(i,j,k,iBlock) = min(DtLimit/Cfl, DtMax_CB(i,j,k,iBlock))
       end do; end do; end do
    end if

    ! Limit local time step by global time step (we could limit this to
    if(present(IsPartLocal) .and. Dt > 0)then
       !$acc loop vector collapse(3)
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          DtMax_CB(i,j,k,iBlock) = min(Dt/Cfl, DtMax_CB(i,j,k,iBlock))
       end do; end do; end do
    end if

#ifndef _OPENACC
    if(DoTest .and. UseDtFixed) &
         write(*,*) NameSub,' after UseDtFixed, DtMax_CB =', &
         DtMax_CB(iTest,jTest,kTest,iBlock)

    if(DoTest .and. UseDtLimit) &
         write(*,*) NameSub,' after limiting, DtMax_CB =', &
         DtMax_CB(iTest,jTest,kTest,iBlock)

    ! Set time step to zero inside body.
    if(.not.IsNoBody_B(iBlock)) then
       where (.not.Used_GB(1:nI,1:nJ,1:nK,iBlock))&
            DtMax_CB(:,:,:,iBlock) = 0.0
    end if
#endif
    call test_stop(NameSub, DoTest, iBlock)
  end subroutine calc_timestep
  !============================================================================
  subroutine set_global_timestep(TimeSimulationLimit)

    use ModMain
    use ModAdvance,  ONLY: DtMax_CB, State_VGB, rho_, Bx_, Bz_, P_, &
         iTypeAdvance_B, ExplBlock_
    use ModB0,       ONLY: B0_DGB
    use ModGeometry, ONLY: Used_GB, IsNoBody_B
    use ModImplicit, ONLY: UsePartImplicit
    use ModPhysics,  ONLY: No2Si_V, Si2No_V, No2Io_V, &
         UnitX_, UnitU_, UnitT_, UnitB_, UnitRho_, UnitP_, Gamma
    use ModNumConst
    use ModMpi
    use BATL_lib,    ONLY: Xyz_DGB, CellSize_DB, &
         MaxNode, nNode, iNode_B, iTimeLevel_A, nTimeLevel

    real, intent(in):: TimeSimulationLimit ! Simulation time not to be exceeded

    integer :: iBlock
    integer :: iError, Ijk_D(3), i, j, k
    real    :: DtMinPe, Cmax, Cmax_C(nI,nJ,nK)

    ! DtMax_CB is already set in calc_timestep,
    ! and Dt=DtLimit is set in set_parameters
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_global_timestep'
    !--------------------------------------------------------------------------
    if(UseDtLimit) RETURN
    call test_start(NameSub, DoTest)

    if(DoTest)write(*,*) NameSub, &
         ' starting with TimeSimulationLimit, DtMax_B, DtMax_CB=', &
         TimeSimulationLimit, DtMax_B(iBlockTest)*No2Io_V(UnitT_), &
         DtMax_CB(iTest,jTest,kTest,iBlockTest)*No2Io_V(UnitT_)

    if(UseMaxTimeStep)then
       if(.not.allocated(iTimeLevel_A)) allocate(iTimeLevel_A(MaxNode))
       iTimeLevel_A(1:nNode) = 0
       nTimeLevel = 0
    end if

    if(UseDtFixed)then
       Dt = DtFixed
    elseif(nStep < 1 .or. nStep == 1 .and. TimeSimulationLimit > 0.0)then
       Dt    = 0.0
       DtMin = 0.0
       DtMax = 0.0
    else
       ! Impose global time step for time-accurate calculations as required
       if(UsePartImplicit)then
          ! Implicit blocks are not taken into account for partially implicit
          ! run
          DtMinPE = huge(DtMax_B(1))
          !$acc parallel loop independent reduction(min:DtMinPE)
          do iBlock = 1, nBlock
             if (iTypeAdvance_B(iBlock) == ExplBlock_) &
                  DtMinPE = min(DtMinPE, DtMax_B(iBlock))
          end do
          if(UseMaxTimeStep) then
             DtMax = -huge(DtMax_B(1))
             !$acc parallel loop independent reduction(max:DtMax)
             do iBlock = 1, nBlock
                if (iTypeAdvance_B(iBlock) == ExplBlock_) &
                     DtMax = max(DtMax, DtMax_B(iBlock))
             end do
          end if
       else
          DtMinPE = huge(DtMax_B(1))
          !$acc parallel loop independent reduction(min:DtMinPE)
          do iBlock = 1, nBlock
             if (.not.Unused_B(iBlock)) &
                  DtMinPE = min(DtMinPE, DtMax_B(iBlock))
          end do

          if(UseMaxTimeStep) then
             DtMax = -huge(DtMax_B(1))
             !$acc parallel loop independent reduction(max:DtMax)
             do iBlock = 1, nBlock
                if (.not.Unused_B(iBlock)) DtMax = max(DtMax, DtMax_B(iBlock))
             end do
             DtMax = min(DtMax, DtLimit/Cfl)
          end if
       endif

       ! Set Dt to minimum time step over all the PE-s
       ! Multi-GPU runs are the same as multi-CPU runs. Dt is updated after
       ! this call.
       call MPI_allreduce(DtMinPE, DtMin, 1, MPI_REAL, MPI_MIN, iComm, iError)

       Dt = DtMin

#ifndef _OPENACC
       if(DoTest .and. DtMinPE == Dt)then
          do iBlock = 1, nBlock
             if(Unused_B(iBlock)) CYCLE
             if(DtMax_B(iBlock) /= Dt) CYCLE
             write(*,*) NameSub, ' Dt=',Dt,'=', Dt*No2Si_V(UnitT_),&
                  ' s  is controlled by block with ',&
                  'iProc, iBlock, IsNoBody_B, Used_GB =', &
                  iProc, iBlock, IsNoBody_B(iBlock), &
                  all(Used_GB(1:nI,1:nJ,1:nK,iBlock)), &
		  any(Used_GB(1:nI,1:nJ,1:nK,iBlock))
             write(*,*) NameSub, ' X,Y,Z coordinates of (1,1,1) cell center=',&
                  Xyz_DGB(:,1,1,1,iBlock)
             write(*,*) NameSub, ' cell size in normalized and SI units:', &
                  CellSize_DB(:,iBlock), ', ', &
                  CellSize_DB(:,iBlock)*No2Si_V(UnitX_),' m'
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                Cmax_C(i,j,k) = Gamma*State_VGB(P_,i,j,k,iBlock)
                if(UseB)then
                   if(UseB0)then
                      Cmax_C(i,j,k) = Cmax_C(i,j,k) +           &
                           sum((State_VGB(Bx_:Bz_,i,j,k,iBlock) &
                           +    B0_DGB(:,i,j,k,iBlock))**2    )
                   else
                      Cmax_C(i,j,k) = Cmax_C(i,j,k) + &
                           sum(State_VGB(Bx_:Bz_,i,j,k,iBlock)**2)
                   end if
                end if
                CMax_C(i,j,k) = Cmax_C(i,j,k)/State_VGB(rho_,i,j,k,iBlock)
             end do; end do; end do
             Ijk_D = maxloc(Cmax_C, MASK=Used_GB(1:nI,1:nJ,1:nK,iBlock))
             i=Ijk_D(1); j=Ijk_D(2); k=Ijk_D(3)
             Cmax = sqrt(Cmax_C(i,j,k))
             write(*,*) NameSub, ' Cmax=',Cmax*No2Si_V(UnitU_),&
                  ' m/s is reached at X,Y,Z=', Xyz_DGB(:,i,j,k,iBlock)
             if(UseB0)write(*,*) NameSub,' B0=',&
                  B0_DGB(:,i,j,k,iBlock)*No2Si_V(UnitB_), ' T'
             if(UseB)write(*,*) NameSub, ' B1=',&
                  State_VGB(Bx_:Bz_,i,j,k,iBlock)*No2Si_V(UnitB_), ' T'
             write(*,*) NameSub,' Rho=',&
                  State_VGB(Rho_,i,j,k,iBlock)*No2Si_V(UnitRho_), ' kg/m3'
             write(*,*) NameSub,' p=',&
                  State_VGB(p_,i,j,k,iBlock)*No2Si_V(UnitP_), ' Pa'
             EXIT
          end do
       end if

       if(UseMaxTimeStep)then
          ! Calculate largest time step
          if(nProc > 1) call MPI_allreduce(&
               MPI_IN_PLACE, DtMax, 1, MPI_REAL, MPI_MAX, iComm, iError)

          if(DoTest) write(*,*)NameSub,' DtMin, DtMax=', DtMin, DtMax

          ! Make DtMax a power of 2 multiple of DtMin
          nTimeLevel = floor(log(DtMax/DtMin)/log(2.0))
          DtMax = DtMin * 2**nTimeLevel

          if(DoTest)write(*,*)NameSub,' DtMax after rounding=',DtMax

          Dt = DtMax
       end if
#endif
    end if

    ! Limit Dt such that the simulation time cannot exceed TimeSimulationLimit.
    ! If statement avoids real overflow when TimeSimulationLimit = Huge(0.0)
    if(TimeSimulationLimit > 0.0 .and. &
         tSimulation + Cfl*Dt*No2Si_V(UnitT_) > TimeSimulationLimit)then
       Dt = (TimeSimulationLimit - tSimulation)*Si2No_V(UnitT_)/Cfl
       if(UseMaxTimeStep)then
          DtMax = Dt
          DtMin = min(DtMin, Dt)
          nTimeLevel = ceiling(log(DtMax/DtMin)/log(2.0))
          DtMin = DtMax/2**nTimeLevel
       end if
    end if

    !$acc parallel loop gang
    !$omp parallel do
    do iBlock = 1, nBlock
       if (Unused_B(iBlock)) CYCLE

       if(UseMaxTimeStep .and. DtMax_B(iBlock) > 0)then
          ! Make block time step power of 2 multiple of DtMin that is < DtMax_B
          ! Limit by DtMax in case DtMax was limited by TimeSimulationLimit
          if(DtMax_B(iBlock) >= DtMax)then
             DtMax_B(iBlock) = DtMax
          else
             ! Time level of this block
             iTimeLevel_A(iNode_B(iBlock)) = &
                  ceiling(log(DtMax/DtMax_B(iBlock))/log(2.0))
             ! Time step rounded to power of 2 fraction of DtMax
             DtMax_B(iBlock) = DtMax / 2**iTimeLevel_A(iNode_B(iBlock))
          end if
          DtMax_CB(:,:,:,iBlock) = DtMax_B(iBlock)
       else
          ! Set each cell to global time step
          !$acc loop collapse(3)
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             DtMax_CB(i,j,k,iBlock) = Dt
          end do; end do; end do
       end if

       ! Reset time step to zero inside body.
       if(.not.IsNoBody_B(iBlock))then
          where(.not.Used_GB(1:nI,1:nJ,1:nK,iBlock)) &
               DtMax_CB(:,:,:,iBlock) = 0.0
       end if

    end do
    !$omp end parallel do

#ifndef _OPENACC
    ! Collect time level information from all processors
    if(UseMaxTimeStep .and. nProc > 1) call MPI_allreduce(MPI_IN_PLACE, &
         iTimeLevel_A, nNode, MPI_INTEGER, MPI_SUM, iComm, iError)
#endif

    ! Set global time step to the actual time step used
    Dt = Cfl*Dt
    !$acc update device(Dt)

    if(DoTest)write(*,*) NameSub,' finished with Dt, DtMax_B, DtMax_CB=', &
         Dt*No2Io_V(UnitT_), DtMax_B(iBlockTest)*No2Io_V(UnitT_), &
         DtMax_CB(iTest,jTest,kTest,iBlockTest)*No2Io_V(UnitT_)

    call test_stop(NameSub, DoTest)
  end subroutine set_global_timestep
  !============================================================================
  subroutine control_time_step

    use ModMain,     ONLY: nBlock, nI, nJ, nK, Unused_B, Dt, Cfl, CflOrig, &
         DtFixed, DtFixedOrig, UseDtFixed, tSimulation, &
         DtLimit, DtLimitOrig, UseDtLimit, UseLocalTimeStep
    use ModAdvance,  ONLY: Rho_, p_, &
         State_VGB, StateOld_VGB, DtMax_CB
    use ModPhysics,  ONLY: No2Si_V, UnitT_
    use ModGeometry, ONLY: Used_GB
    use ModMpi

    integer:: iBlock, i, j, k, iError
    real   :: RelativeChangeMin,  RelativeChangeMax, Tmp, Factor

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'control_time_step'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Use density and pressure as control variables by default
    if(nVarControl < 0)then
       nVarControl = 2
       allocate( iVarControl_I(nVarControl), VarRatio_I(nVarControl) )
       iVarControl_I = [Rho_, p_]
    end if

    ! Calculate the largest relative drop in the control variables
    RelativeChangeMin = 1e+6
    RelativeChangeMax = 1e-6

    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(Used_GB(i,j,k,iBlock))then
             VarRatio_I = State_VGB(iVarControl_I,i,j,k,iBlock) &
                  /    StateOld_VGB(iVarControl_I,i,j,k,iBlock)
             RelativeChangeMin = min(RelativeChangeMin, minval(VarRatio_I))
             RelativeChangeMax = max(RelativeChangeMax, maxval(VarRatio_I))
          end if
       end do; end do; end do
    end do

    if(nProc > 1)then
       ! Take minimum and maximum over all the processors
       Tmp = RelativeChangeMin
       call MPI_allreduce(Tmp, RelativeChangeMin, 1, MPI_REAL, MPI_MIN, &
            iComm, iError)
       Tmp = RelativeChangeMax
       call MPI_allreduce(Tmp, RelativeChangeMax, 1, MPI_REAL, MPI_MAX, &
            iComm, iError)
    end if

    ! Figure out time step reduction factor
    if(       RelativeChangeMin < RejectStepLevel1  &
         .or. RelativeChangeMax > RejectStepLevel2 )then
       ! Redo step if change is outside [RejectStepLevel1, RejectStepLevel2]
       tSimulation = tSimulation - Dt*No2Si_V(UnitT_)
       Dt = 0.0
       ! Do not use previous step in BDF2 scheme
       ! !! nStepPrev = -1
       ! Reset the state variable, the energy and set DtMax_CB variable to 0
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          State_VGB(:,1:nI,1:nJ,1:nK,iBlock)  &
               = StateOld_VGB(:,1:nI,1:nJ,1:nK,iBlock)
          DtMax_CB(1:nI,1:nJ,1:nK,iBlock)     = 0.0
       end do
       ! Reduce next time step
       Factor = RejectStepFactor

       ! if (RelativeChangeMax > 100.0) then
       !   Factor = 0.1
       ! endif

    elseif(   RelativeChangeMin < ReduceStepLevel1 &
         .or. RelativeChangeMax > ReduceStepLevel2 )then
       ! Reduce next time step if change exceeded ReduceStepLevel
       Factor = ReduceStepFactor

    elseif(    RelativeChangeMin > IncreaseStepLevel1  &
         .and. RelativeChangeMax < IncreaseStepLevel2 )then
       ! Increase next time step if change is within IncreaseStepLevel
       Factor = IncreaseStepFactor
    else
       Factor = 1.0
    end if

    if(UseDtFixed)then
       ! Do not exceed DtFixedOrig
       DtFixed = min(DtFixedOrig, DtFixed*Factor)
    elseif(UseDtLimit .or. UseLocalTimeStep)then
       ! Do not exceed DtLimitOrig
       DtLimit = min(DtLimitOrig, DtLimit*Factor)
       Cfl     = min(CflOrig, Cfl*Factor)
    else
       ! Do not exceed CflOrig
       Cfl     = min(CflOrig, Cfl*Factor)
    end if

    if(DoTest)then
       write(*,*) NameSub,': RelativeChangeMin,Max,Factor=', &
            RelativeChangeMin, RelativeChangeMax, Factor
       if(UseDtFixed)then
          write(*,*) NameSub,': Dt, DtFixed, Cfl=',&
               Dt*No2Si_V(UnitT_), DtFixed*No2Si_V(UnitT_), Cfl
       elseif(UseDtLimit .or. UseLocalTimeStep)then
          write(*,*) NameSub,': Dt, DtLimit, Cfl=',&
               Dt*No2Si_V(UnitT_), DtLimit*No2Si_V(UnitT_), Cfl
       else
          write(*,*) NameSub,': Dt, Cfl=', Dt*No2Si_V(UnitT_), Cfl
       end if
    end if

    call test_stop(NameSub, DoTest)
  end subroutine control_time_step
  !============================================================================
end module ModTimeStepControl
!==============================================================================
