!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModLocalTimeStep

  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest, iBlockTest, iVarTest,&
       iComm
  use ModBatsrusUtility, ONLY: stop_mpi

  use ModSize, ONLY: nDim, nBlock, MaxBlock, nI, nJ, nK, nG, x_, y_, z_
  use ModMain, ONLY: UseLocalTimeStep, UseLocalTimeStepNew, DtLimitDim
  use ModTimeStepControl, ONLY: UseMaxTimeStep
  use BATL_lib, ONLY: UseTimeLevel

  implicit none

  private ! except

  public:: advance_localstep    ! time accurate mode with subcycling
  public:: read_localstep_param ! read parameters for algorithm

  ! Local variables
  real:: DtMinSi, DtMaxSi

contains
  !============================================================================
  subroutine read_localstep_param(NameCommand, iSession)

    use ModReadParam, ONLY: read_var

    character(len=*), intent(in):: NameCommand
    integer, intent(in):: iSession

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_localstep_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(NameCommand)
    case('#LOCALTIMESTEP', '#SUBCYCLING')
       ! Check if we had it on already
       UseLocalTimeStepNew = .not.UseLocalTimeStep .and. iSession > 1

       call read_var('UseSubcycling', UseLocalTimeStep)
       if(UseLocalTimeStep)then
          call read_var('UseMaxTimeStep',   UseMaxTimeStep)
          UseTimeLevel = UseMaxTimeStep
          call read_var('DtLimitDim',       DtLimitDim)
       end if

       ! Check if the local time stepping was just switched on
       UseLocalTimeStepNew = UseLocalTimeStepNew .and. UseLocalTimeStep

    case default
       call stop_mpi(NameSub//': unknown command='//NameCommand)
    end select

    call test_stop(NameSub, DoTest)
  end subroutine read_localstep_param
  !============================================================================
  subroutine advance_localstep(TimeSimulationLimit)

    use ModMain, ONLY: tSimulation, Dt, DtMax_B, Cfl, iStage, nStage,&
         nOrder, nOrderProlong
    use ModFaceFlux, ONLY: calc_face_flux
    use ModFaceValue, ONLY: calc_face_value
    use ModAdvance, ONLY: nFluid, nVar, State_VGB, &
         Flux_VXI, Flux_VYI, Flux_VZI
    use ModB0, ONLY: set_b0_face
    use ModConserveFlux, ONLY: DoConserveFlux
    use ModGeometry, ONLY: IsBody_B, IsBoundary_B
    use ModFaceBoundary, ONLY: set_face_boundary
    use ModCellBoundary, ONLY: set_cell_boundary
    use ModPhysics, ONLY: No2Si_V, Si2No_V, UnitT_
    use ModCalcSource, ONLY: calc_source
    use ModTimeStepControl, ONLY: calc_timestep, DtMin, DtMax
    use ModBlockData, ONLY: set_block_data
    use ModCoronalHeating, ONLY: get_coronal_heat_factor, UseUnsignedFluxModel
    use ModResistivity, ONLY: set_resistivity, UseResistivity
    use ModCoarseAxis, ONLY: UseCoarseAxis, coarsen_axis_cells
    use ModUpdateState, ONLY: update_state
    use BATL_lib, ONLY: Unused_B, min_tree_level,  &
         message_pass_cell, store_face_flux, apply_flux_correction

    real, intent(in) :: TimeSimulationLimit

    ! Advance the solution by one time step. After the time step
    ! the simulation time should not exceed TimeSimulationLimit.
    ! Each block is advanced with local time steps proportional
    ! to the block level.
    ! For now a 2-stage scheme is applied per local time step,
    ! so it is a temporally 2nd order accurate scheme.

    ! Number of fluxes including the energy variables
    integer, parameter:: nFlux = nVar + nFluid
    real, allocatable:: Flux_VFD(:,:,:,:,:), &
         Flux_VXB(:,:,:,:,:), Flux_VYB(:,:,:,:,:), Flux_VZB(:,:,:,:,:)

    ! time and stage (1 or 2) info per block
    real   :: Time_B(MaxBlock), TimeOld_B(MaxBlock)
    integer:: iStage_B(MaxBlock)

    real   :: TimeStage, TimeEnd, DtSiTiny
    integer:: nTimeStage, iStageLocal, iLevelMin, iBlock

    integer:: iGang

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'advance_localstep'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    iGang = 1
    if(DoTest)write(*,*)NameSub, &
         ' starting with TimeSimulationLimit=', TimeSimulationLimit

    call timing_start(NameSub)

    if(UseMaxTimeStep)then
       ! DtMin and DtMax are set by ModTimeStepControl::set_global_timestep
       DtMinSi = DtMin*No2Si_V(UnitT_)*Cfl
       DtMaxSi = DtMax*No2Si_V(UnitT_)*Cfl

       if(DoTest)write(*,*) NameSub,' DtMinSi, DtMaxSi=', DtMinSi, DtMaxSi

    else
       ! set global and local time steps
       call set_local_time_step(TimeSimulationLimit)
       ! The total time step is the largest
       Dt = DtMaxSi*Si2No_V(UnitT_)
    end if

    if(DoConserveFlux)then
       if(.not. allocated(Flux_VFD)) allocate(&
            Flux_VFD(nFlux,nI+1,nJ+1,nK+1,nDim), &
            Flux_VXB(nFlux,nJ,nK,2,MaxBlock), & ! Two sides of the block
            Flux_VYB(nFlux,nI,nK,2,MaxBlock), &
            Flux_VZB(nFlux,nI,nJ,2,MaxBlock))

       ! Initialize fluxes for flux correction scheme
       Flux_VFD = 0.0
       Flux_VXB = 0.0
       Flux_VYB = 0.0
       Flux_VZB = 0.0

       if(DoTest)write(*,*) NameSub,' initialized conservative flux arrays'

    end if

    ! Initialize time and stage info for all blocks
    Time_B(1:nBlock)    = tSimulation
    TimeOld_B(1:nBlock) = tSimulation
    iStage_B(1:nBlock)  = 1

    ! A small fraction of the smallest time step in SI units
    DtSiTiny = 1e-6*DtMinSi

    ! Initialize time of current stage
    TimeStage = tSimulation

    ! Final simulation time
    TimeEnd = tSimulation + DtMaxSi

    ! Number of stages
    nTimeStage = nint(DtMaxSi/DtMinSi)

    ! Loop over stages (nStage stages per local time step)
    do iStageLocal = 1, nStage*nTimeStage

       ! Number of grid levels involved in this stage
       iLevelMin = min_tree_level(iStageLocal)
       if(DoTest)write(*,*)'iStageLocal, iLevelMin=', iStageLocal, iLevelMin

       ! write(*,*)'!!! left TimeOld, Time=', TimeOld_B(13), Time_B(13)
       ! write(*,*)'!!! rite TimeOld, Time=', TimeOld_B(14), Time_B(14)
       ! write(*,*)'!!! Bef left Rho( 3:6)=', State_VGB(3, 3:6,1,1,13)
       ! write(*,*)'!!! Bef rite Rho(-1:2)=', State_VGB(3,-1:2,1,1,14)

       call timing_start('message_pass_cell')
       if(nOrder > 1 .and. nOrderProlong == 2)then
          call message_pass_cell(nVar, State_VGB, &
               TimeOld_B=TimeOld_B, Time_B=Time_B, iLevelMin=iLevelMin)
       else
          call message_pass_cell(nVar, State_VGB, &
               DoSendCornerIn=.true., &
               nProlongOrderIn=1, &
               DoRestrictFaceIn=.true., &
               TimeOld_B=TimeOld_B, Time_B=Time_B, iLevelMin=iLevelMin)
       end if
       call timing_stop('message_pass_cell')

       ! write(*,*)'!!! Aft left Rho( 3:6)=', State_VGB(3, 3:6,1,1,13)
       ! write(*,*)'!!! Aft rite Rho(-1:2)=', State_VGB(3,-1:2,1,1,14)

       ! Calculate coronal heat factor
       ! This routine involves MPI_allreduce, so it cannot be done per block
       if(UseUnsignedFluxModel) call get_coronal_heat_factor

       ! Calculate resistivity
       ! This routine is usually local, but may involve message passing
       ! if DoMessagePassResistivity is true.
       ! A block-wise resistivity calculation should be implemented.
       if(UseResistivity) call set_resistivity

       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE

          ! Calculate interface values for L/R states of each face
          !   and apply BCs for interface states as needed.
          call set_b0_face(iBlock)

          ! Skip block that already exceeded the current time of the stage
          if(Time_B(iBlock) > TimeStage + DtSiTiny) CYCLE

          ! stage for block in the nStage stage update
          iStage = iStage_B(iBlock)

          ! Update cell boundaries
          if (IsBoundary_B(iBlock)) call set_cell_boundary( &
               nG, iBlock, nVar, State_VGB(:,:,:,:,iBlock))

          call timing_start('calc_facevalues')
          call calc_face_value(iBlock, DoResChangeOnly=.false.)
          call timing_stop('calc_facevalues')

          ! Update face boundaries
          if(IsBody_B(iBlock)) &
               call set_face_boundary(iBlock, Time_B(iBlock), .false.)

          ! Calculate fluxes
          call timing_start('calc_fluxes')
          call calc_face_flux(.false., iBlock)
          call timing_stop('calc_fluxes')

          ! Calculate sources
          call calc_source(iBlock)

          ! Update state
          call timing_start('update_state')
          call update_state(iBlock)
          call timing_stop('update_state')

          ! Need something like update_check in the future.

          if(DoConserveFlux .and. iStage == nStage)then
             ! Store fluxes for flux correction
             Flux_VFD(1:nFlux,1:nI+1,1:nJ,1:nK,x_) = &
                  Flux_VXI(1:nFlux,1:nI+1,1:nJ,1:nK,iGang)
             if(nJ>1) & ! 2D
                  Flux_VFD(1:nFlux,1:nI,1:nJ+1,1:nK,y_) = &
                  Flux_VYI(1:nFlux,1:nI,1:nJ+1,1:nK,iGang)
             if(nK>1) & ! 3D
                  Flux_VFD(1:nFlux,1:nI,1:nJ,1:nK+1,z_) = &
                  Flux_VZI(1:nFlux,1:nI,1:nJ,1:nK+1,iGang)

             call store_face_flux(iBlock, nFlux, &
                  Flux_VFD, Flux_VXB, Flux_VYB, Flux_VZB, &
                  DtIn = DtMax_B(iBlock)*Cfl, DoStoreCoarseFluxIn = .true.,&
                  DoReschangeOnlyIn = .not.UseMaxTimeStep)

             if(DoTest .and. iBlock == iBlockTest) &
                  write(*,*) NameSub,' stored conservative flux'

          end if

          ! Update block time
          TimeOld_B(iBlock) = Time_B(iBlock)
          Time_B(iBlock)    = Time_B(iBlock) &
               + Cfl*DtMax_B(iBlock)*No2Si_V(UnitT_)/nStage

          ! Swap between iStage = 1 and 2 for this block
          if(nStage==2) iStage_B(iBlock) = 3 - iStage

          ! Calculate time step limit for next time step
          ! if the block has reached the end of the time step
          if(Time_B(iBlock) >= TimeEnd - DtSiTiny) call calc_timestep(iBlock)

          ! At this point the user has surely set all "block data"
          ! NOTE: The user has the option of calling set_block_data directly.
          call set_block_data(iBlock)

       enddo ! iBlock

       ! Apply flux correction at the end of full steps
       if(DoConserveFlux .and. &
            (modulo(iStageLocal,2*nStage) == 0 .or. iStageLocal == nStage))then

          if(DoTest)write(*,*) NameSub, &
               ' before  conservative flux, test var=',&
               State_VGB(iVarTest,iTest,jTest,kTest,iBlockTest)

          call apply_flux_correction( &
               nVar, nFluid, State_VGB, &
               Flux_VXB, Flux_VYB, Flux_VZB, iStageIn=iStageLocal/nStage, &
               DoReschangeOnlyIn=.not.UseMaxTimeStep, DoTestIn=DoTest)

          if(DoTest)write(*,*) NameSub, &
               ' applied conservative flux, test var=',&
               State_VGB(iVarTest,iTest,jTest,kTest,iBlockTest)
       end if

       ! Coarsen the axis cells if requested
       if(UseCoarseAxis)call coarsen_axis_cells

       ! Update time for the stage
       TimeStage = TimeStage + DtMinSi/nStage

    enddo ! iStageLocal

    tSimulation = tSimulation + DtMaxSi

    if(DoTest)write(*,*)NameSub,' finished'

    call timing_stop(NameSub)

    call test_stop(NameSub, DoTest)
  end subroutine advance_localstep
  !============================================================================

  subroutine set_local_time_step(TimeSimulationLimit)

    use ModGeometry, ONLY: IsNoBody_B, CellSize1Min, CellSize1Max
    use ModMain, ONLY: tSimulation, Cfl, DtMax_B
    use ModAdvance, ONLY: DtMax_CB
    use BATL_lib, ONLY: CellSize_DB, Unused_B, Used_GB
    use ModPhysics, ONLY: No2Si_V, UnitT_, Si2No_V
    use ModMpi

    real, intent(in) :: TimeSimulationLimit

    real   :: DtDxMinPe, DtDxMin
    integer:: iError
    integer:: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_local_time_step'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(DoTest)write(*,*) NameSub,' starting with TimeSimulationLimit=', &
         TimeSimulationLimit

    ! Find the smallest value of Dt_stable/Dx in the whole domain
    DtDxMinPe = minval(DtMax_B(1:nBlock)/CellSize_DB(1,1:nBlock), &
         MASK=.not.Unused_B(1:nBlock))

    call MPI_allreduce(DtDxMinPe, DtDxMin, 1, MPI_REAL, MPI_MIN, iComm, iError)

    ! Smallest and largest time steps in the domain (includes Cfl number)
    DtMinSi = Cfl*CellSize1Min*DtDxMin*No2Si_V(UnitT_)
    DtMaxSi = Cfl*CellSize1Max*DtDxMin*No2Si_V(UnitT_)

    if(DoTest)write(*,*) NameSub,': original DtDxMin, DtMinSi, DtMaxSi=', &
          DtDxMin, DtMinSi, DtMaxSi

    ! Check if we reached the final time
    if(tSimulation + DtMaxSi > TimeSimulationLimit) then
       ! For the last few time steps use uniform time steps
       DtMinSi = min(DtMinSi, TimeSimulationLimit - tSimulation)
       DtMaxSi = DtMinSi
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          DtMax_B(iBlock) = DtMinSi/Cfl * Si2No_V(UnitT_)
       enddo
    else
       ! Set the block time step (without Cfl) proportional to refinement level
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          DtMax_B(iBlock) = CellSize_DB(1,iBlock)*DtDxMin
       enddo
    end if

    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       DtMax_CB(:,:,:,iBlock) = DtMax_B(iBlock)

       ! Reset time step to zero inside body.
       if(.not.IsNoBody_B(iBlock))then
          where(.not.Used_GB(1:nI,1:nJ,1:nK,iBlock)) &
               DtMax_CB(:,:,:,iBlock) = 0.0
       end if
    enddo

    if(DoTest)write(*,*) NameSub,' finished with DtMinSi, DtMaxSi=', &
         DtMinSi, DtMaxSi

    call test_stop(NameSub, DoTest)
  end subroutine set_local_time_step
  !============================================================================

end module ModLocalTimeStep
!==============================================================================
