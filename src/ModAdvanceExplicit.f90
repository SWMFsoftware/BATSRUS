!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModAdvanceExplicit

  use BATL_lib, ONLY: &
       test_start, test_stop
  use ModBatsrusUtility, ONLY: barrier_mpi2, stop_mpi

  implicit none
  private ! except

  public:: advance_explicit ! advance state variables with explicit method
  public:: update_secondbody
contains
  !============================================================================
  subroutine advance_explicit(DoCalcTimestep)

    use ModMain
    use ModFaceBoundary, ONLY: set_face_boundary
    use ModFaceFlux, ONLY: calc_face_flux
    use ModFaceValue, ONLY: calc_face_value, calc_cell_norm_velocity, &
         set_low_order_face
    use ModAdvance, ONLY: UseUpdateCheck, DoFixAxis, DoCalcElectricField, &
         UseAdaptiveLowOrder, UseMhdMomentumFlux, iTypeUpdate, UpdateFast_,  &
         LeftState_VX, RightState_VX, nFaceValue, DtMax_CB
    use ModCoarseAxis, ONLY: UseCoarseAxis, coarsen_axis_cells
    use ModB0, ONLY: set_b0_face
    use ModParallel, ONLY: DiLevel_EB
    use ModGeometry, ONLY: IsBody_B, rMin_B
    use ModBlockData, ONLY: set_block_data
    use ModPhysics, ONLY: No2Si_V, UnitT_, &
         update_angular_velocity, UseBody2Orbit
    use ModCalcSource, ONLY: calc_source
    use ModConserveFlux, ONLY: DoConserveFlux, &
         CorrectedFlux_VXB, CorrectedFlux_VYB, CorrectedFlux_VZB, &
         save_cons_flux, apply_cons_flux
    use ModCoronalHeating, ONLY: get_coronal_heat_factor,&
         UseUnsignedFluxModel
    use ModWaves, ONLY: UseAwRepresentative
    use ModTurbulence, ONLY:           &
         wave_energy_to_representative, representative_to_wave_energy
    use ModMessagePass, ONLY: exchange_messages
    use ModTimeStepControl, ONLY: calc_timestep, enforce_cfl, DoEnforceCfl
    use BATL_lib, ONLY: message_pass_face, IsAnyAxis
    use ModResistivity, ONLY: set_resistivity, UseResistivity
    use ModFieldLineThread, ONLY: &
         UseFieldLineThreads, advance_threads, Enthalpy_, is_threaded_block, &
         advance_threaded_block_expl
    use ModUpdateStateFast, ONLY: update_state_fast
    use ModUpdateState, ONLY: update_check, update_state, check_nan
    use ModConstrainDivB, ONLY: &
         bface_to_bcenter, get_vxb, bound_vxb, constrain_b
    use ModFixAxisCells, ONLY: fix_axis_cells
    use ModElectricField, ONLY: get_num_electric_field, correct_efield_block
    use ModParticleMover, ONLY: UseChargedParticles=>UseParticles, &
         UseHybrid, trace_particles, get_state_from_vdf, advance_ion_current
    use ModReverseField, ONLY: DoReverseField, DoReverseField_B, &
         do_reverse_block, reverse_field, set_sign_field
    use ModViscosity, ONLY: UseArtificialVisco
    use omp_lib

    logical, intent(in) :: DoCalcTimestep

    integer:: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'advance_explicit'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Perform multi-stage update of solution for this time (iteration) step
    call timing_start(NameSub)

    if(UseBody2Orbit) call update_secondbody
    if(UseAwRepresentative)call wave_energy_to_representative
    STAGELOOP: do iStage = 1, nStage
       !$acc update device(iStage)
       if(DoTest) write(*,*)NameSub,' starting stage=',iStage

       ! If heating model depends on global B topology, update here
       if(UseUnsignedFluxModel) call get_coronal_heat_factor

       if(.not.UseOptimizeMpi) call barrier_mpi2('expl1')

       if(UseResistivity) call set_resistivity

       if(UseRotatingBc) call update_angular_velocity

       if(iStage==1)then
          if(UseArtificialVisco .or. UseAdaptiveLowOrder) &
               call calc_cell_norm_velocity
          call set_low_order_face
       endif

       if(DoConserveFlux)then
          !$omp parallel do
          do iBlock = 1, nBlock
             if(Unused_B(iBlock)) CYCLE
             if(all(DiLevel_EB(:,iBlock) /= 1)) CYCLE
             ! Calculate interface values for L/R states of each
             ! fine grid cell face at block edges with resolution changes
             ! and apply BCs for interface states as needed.
             call set_b0_face(iBlock)
             call timing_start('calc_face_bfo')
             call calc_face_value(iBlock, DoResChangeOnly=.true.)
             call timing_stop('calc_face_bfo')

             if(IsBody_B(iBlock)) &
                  call set_face_boundary(iBlock, tSimulation, .true.)

             ! Compute interface fluxes for each fine grid cell face at
             ! block edges with resolution changes.

             call timing_start('calc_fluxes_bfo')
             call calc_face_flux(.true., iBlock)
             call timing_stop('calc_fluxes_bfo')

             ! Save conservative flux correction for this solution block
             call save_cons_flux(iBlock)
          end do
          !$omp end parallel do

          if(DoTest)write(*,*)NameSub,' done res change only'

          ! Message pass conservative flux corrections.
          call timing_start('send_cons_flux')
          call message_pass_face(nFaceValue, CorrectedFlux_VXB, &
               CorrectedFlux_VYB, CorrectedFlux_VZB, DoSubtractIn=.false.)

          call timing_stop('send_cons_flux')

          if(DoTest)write(*,*)NameSub,' done message pass'

       endif

       if(iTypeUpdate == UpdateFast_)then
          call update_state_fast
       else
          ! CPU compatible code
          !$omp parallel do
          do iBlock = 1, nBlock

             if(Unused_B(iBlock)) CYCLE

             ! Calculate interface values for L/R states of each face
             ! and apply BCs for interface states as needed.
             if(DoReverseField)then
                if(iStage == 1) &
                     DoReverseField_B(iBlock) = do_reverse_block(iBlock)
                if(DoReverseField_B(iBlock)) call reverse_field(iBlock)
             end if
             call set_b0_face(iBlock)

             call timing_start('calc_facevalues')
             call calc_face_value(iBlock, DoResChangeOnly=.false.)
             call timing_stop('calc_facevalues')
             if(IsBody_B(iBlock)) &
                  call set_face_boundary(iBlock, tSimulation,.false.)
             if(UseFieldLineThreads.and.is_threaded_block(iBlock))then
                if(IsTimeAccurate)then
                   call advance_threaded_block_expl(iBlock, iStage, &
                        RightState_VX(:,1, 1:nJ, 1:nK),       &
                        LeftState_VX(:,1, 1:nJ, 1:nK),        &
                        DtIn = Dt*No2Si_V(UnitT_))
                ! elseif(iStage > 1)then
                !   call advance_threaded_block_expl(iBlock, iStage, &
                !        RightState_VX(:, 1, 1:nJ, 1:nK),       &
                !        LeftState_VX(:, 1, 1:nJ, 1:nK) ,       &
                !        Dt_II = Cfl*DtMax_CB(1,1:nJ,1:nK,iBlock)*&
                !        No2Si_V(UnitT_))
                else
                   call advance_threaded_block_expl(iBlock, iStage, &
                        RightState_VX(:, 1, 1:nJ, 1:nK),       &
                        LeftState_VX(:, 1, 1:nJ, 1:nK))
                end if
             end if
             ! Compute interface fluxes for each cell.
             call timing_start('calc_fluxes')
             call calc_face_flux(.false., iBlock)
             call timing_stop('calc_fluxes')

             ! Enforce flux conservation by applying corrected fluxes
             ! to each coarse grid cell face at block edges with
             ! resolution changes.
             if(DoConserveFlux) call apply_cons_flux(iBlock)

             ! Compute source terms for each cell.
             call timing_start('calc_sources')
             call calc_source(iBlock)
             call timing_stop('calc_sources')

             ! With known magnetic field and electric field in the
             ! comoving frame update ion velocities at the half time-step
             if(UseFlic .and. iStage >= 2)call advance_ion_current(iBlock)

             ! Electric field in the comoving frame is calculated
             ! and, probably used, in calc_sorces. Add -UxB, to get field
             ! in the global coordinate frame
             if(UseChargedParticles .and. UseMhdMomentumFlux) &
                  call correct_efield_block(iBlock)

             ! In the second stage in the FLIC scheme nothing is updated
             ! except for the (multi)ion currents in advance_ion_current.
             if(UseFlic .and. iStage==2) CYCLE

             if(DoCalcTimestep .and. iStage == 1)then
                ! Calculate time step (both local and global
                ! for the block) used in multi-stage update
                ! for steady state calculations.
                ! Also calculate time step when UseDtLimit is true.
                if(.not.IsTimeAccurate .or. UseDtLimit) &
                     call calc_timestep(iBlock)
                if(IsTimeAccurate)then
                   ! Set local time step inside rLocalTimeStep
                   if(rMin_B(iBlock) < rLocalTimeStep)then
                      call calc_timestep(iBlock, IsPartLocal=.true.)
                   elseif(DoEnforceCfl)then
                      ! The local increase in temperature, for example
                      ! in the semi-implicit heat conduction solver may
                      ! break the CFL condition locally. Modify the time
                      ! step in these occurrences, to enforce
                      ! the Courant-Friedrichs-Levi condition
                      call enforce_cfl(iBlock)
                   end if
                end if
             end if
             ! Update solution state in each cell.
             call timing_start('update_state')
             call update_state(iBlock)
             call timing_stop('update_state')

             if(DoCalcElectricField .and. iStage == nStage) &
                  call get_num_electric_field(iBlock)

             if(UseConstrainB .and. iStage == nStage)then
                call timing_start('constrain_b')
                call get_vxb(iBlock)
                call bound_vxb(iBlock)
                call timing_stop('constrain_b')
             end if

             ! Calculate time step (both local and global
             ! for the block) used in multi-stage update
             ! for time accurate calculations.
             ! For time accurate with UseDtLimit, do not
             ! calculate time step.
             if( IsTimeAccurate .and. .not.UseDtLimit .and. &
                  iStage == nStage .and. DoCalcTimestep)&
                  call calc_timestep(iBlock)

             ! At this point the user has surely set all "block data"
             ! NOTE: The user has the option of calling set_block_data directly
             call set_block_data(iBlock)
             if(DoReverseField)then
                if(DoReverseField_B(iBlock))then
                   call reverse_field(iBlock, DoStateOld=iStage==nStage)
                else
                   call set_sign_field(iBlock)
                end if
             end if

          end do ! Multi-block solution update loop.
          !$omp end parallel do
       end if

       if(DoTest)write(*,*)NameSub,' done update blocks'

       if(.not.UseOptimizeMpi) call barrier_mpi2('expl2')

       if(IsAnyAxis .and. DoFixAxis) call fix_axis_cells
       if(UseCoarseAxis) call coarsen_axis_cells

       ! Check for NaN-s unless running on GPU
       if(iTypeUpdate < UpdateFast_) call check_nan(NameSub)

       ! Check for allowable update percentage change.
       if(UseUpdateCheck)then
          call timing_start('update_check')
          call update_check
          call timing_stop('update_check')

          if(DoTest)write(*,*)NameSub,' done update check'
       end if

       if(UseConstrainB .and. iStage==nStage)then
          call timing_start('constrain_b')
          ! Correct for consistency at resolution changes
          ! call correct_VxB

          ! Update face centered and cell centered magnetic fields
          do iBlock = 1, nBlock
             if(Unused_B(iBlock))CYCLE
             call constrain_b(iBlock)
             call bface_to_bcenter(iBlock)
          end do
          call timing_stop('constrain_b')
          if(DoTest)write(*,*)NameSub,' done constrain B'
       end if

       if(DoCalcTimeStep)then
          ! Update check only works for half step 2 stages time integration.
          ! The final Dt is determined by the second stage if Dt changed by
          ! update_check subroutine.
          if(UseUpdateCheck .and. iStage==1) &
               tSimulationOld = tSimulation
          if(UseFlic)then
             ! Staging Dt/2; Dt/2; Dt
             if(iStage/=2)tSimulation = tSimulation + &
                  Dt*No2Si_V(UnitT_)/2
          else
             tSimulation = tSimulation + Dt*No2Si_V(UnitT_)/nStage
          end if
          if(UseUpdateCheck .and. iStage==nStage) &
               tSimulation = tSimulationOld + Dt*No2Si_V(UnitT_)
       endif
       !$acc update device(tSimulation)

       ! If we have particle to move in the electromagnetic field,
       ! test or hybrid ones
       if(UseChargedParticles)then
          if(iStage==1)then
             ! Ballistically (with no change in particle velocity)
             ! propagate particles for a half time-step
             ! This step corresponds to Equation 16 of Moschou, Sokolov et al.
             ! 2019 Hybrid paper of ASTRONUM
             call trace_particles(Dt=Dt, DoBorisStepIn=.false.)
             if(UseHybrid)call get_state_from_vdf
          end if
          ! Calculate acceleration by the electromagnetic force.
          ! This step corresponds to Equation 7, 21 and Stage 2 in Figure 2
          ! of the Moschou, Sokolov et al. 2019 Hybrid paper of ASTRONUM
          ! Then ballistically (with no change in particle velocity)
          ! propagate them for a half time-step
          ! This step corresponds to Equation 16 of Moschou, Sokolov+ 2019
          ! Hybrid paper of ASTRONUM
          if(UseFlic)then
             if(iStage==2)call trace_particles(&
                  Dt=Dt, DoBorisStepIn=.true.)
             if(iStage==3.and.UseHybrid)call get_state_from_vdf
          else
             if(iStage==nStage)call trace_particles(&
                  Dt=Dt, DoBorisStepIn=.true.)
          end if
       end if

       if(iStage < nStage)then
          if(UseFieldLineThreads) call advance_threads(Enthalpy_)
          call exchange_messages
       end if

       if(DoTest)write(*,*)NameSub,' finished stage=',istage

    end do STAGELOOP  ! Multi-stage solution update loop.
    iStage = nStage
    if(UseAwRepresentative)call representative_to_wave_energy
    call timing_stop(NameSub)

    if(DoTest)write(*,*)NameSub,' finished'

    call test_stop(NameSub, DoTest)

  end subroutine advance_explicit
  !============================================================================
  subroutine update_secondbody
    use ModMain, ONLY:  nBlock, Unused_B, body2_, iNewGrid
    use ModPhysics, ONLY: rBody2, xBody2, yBody2, zBody2, &
         set_second_body_coord
    use ModMessagePass, ONLY: exchange_messages
    use ModBoundaryGeometry, ONLY: iBoundary_GB, domain_, &
         fix_boundary_ghost_cells
    use ModGeometry, ONLY: Xyz_DGB, rBody2_GB, rMinBody2_B, &
         IsNoBody_B
    use ModSize, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK, MaxBlock
    use ModAdvance, ONLY: State_VGB, nVar
    use BATL_lib, ONLY: nI, nJ, nK, Used_GB

    integer :: i,j,k
    integer :: iCounter, iNei, jNei, kNei
    integer :: iBlock
    real    :: StateCounter_V(nVar)
    logical, allocatable:: IsBody2Old_GB(:,:,:,:)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'update_secondbody'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(.not.allocated(IsBody2Old_GB))&
         allocate(IsBody2Old_GB(MinK:MaxK, MinJ:MaxJ, MinI:MaxI, MaxBlock))

    ! Checking which cells are currently body cells  (before update)
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE
       ! Flag old body2 cells
       IsBody2Old_GB(:,:,:,iBlock)= (iBoundary_GB(:,:,:,iBlock)==body2_)
    enddo

    ! Update second body coordinates using orbit elemnts from CON_planet
    call set_second_body_coord

    ! Updating the grid structure for the new second body position
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE
       ! Update rBody2_GB array (the distance from the second body center)
       ! with new second body location
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          rBody2_GB(i,j,k,iBlock) = norm2( Xyz_DGB(:,i,j,k,iBlock) - &
               [xBody2, yBody2, zBody2])
       end do; end do; end do
       rMinBody2_B(iBlock) = minval(rBody2_GB(:,:,:,iBlock))
       ! Reset true cells. "True" are old true cells or old body2 cells
       where(IsBody2Old_GB(:,:,:,iBlock)) &
            iBoundary_GB(:,:,:,iBlock)=domain_

       ! Set iBoundary_GB for body2
       where( rBody2_GB(:,:,:,iBlock) < rbody2) &
            iBoundary_GB(:,:,:,iBlock) = body2_

       Used_GB(:,:,:,iBlock) = (iBoundary_GB(:,:,:,iBlock)==domain_)

       ! IsNoBody_B: if all cells EXCLUDING ghost cells are outside body(ies)
       IsNoBody_B(iBlock) = all(Used_GB(1:nI,1:nJ,1:nK,iBlock))
    enddo
    iNewGrid = mod(iNewGrid + 1, 10000)
    call fix_boundary_ghost_cells

    do iBlock=1, nBlock
       if(Unused_B(iBlock))CYCLE
       ! Loop over physicall cells (the ghost cells will be fixed with
       ! exchange_messages)
       do k = 1,nK ; do j = 1,nJ ; do i = 1,nI
          ! Check if the cell flagged as the second body cell
          ! (hence, filled in with some garbage) becomes a physical cell
          if(Used_GB(i,j,k,iBlock).and.IsBody2Old_GB(i,j,k, iBlock)) then
             ! New true cell, which was inside the second body before.
             ! Needs to be filled in from good cells
             ! Nullify counters
             iCounter = 0
             StateCounter_V = 0.0
             ! Check if there are good neighboring cells with good state
             ! Loop through neighboring cells,
             do kNei = -1,1
                do jNei = -1,1
                   NEI: do iNei = -1,1
                      ! Skip cells which were inside second body
                      if(IsBody2Old_GB(i+iNei,j+jNei,k+kNei,iBlock)) CYCLE NEI
                      ! Skip other non-true cells
                      if(.not.Used_GB(i+iNei,j+jNei,k+kNei,iBlock)) CYCLE NEI
                      ! Collect good state to the counter
                      StateCounter_V = StateCounter_V + &
                           State_VGB(:,i+iNei,j+jNei,k+kNei,iBlock)
                      iCounter = iCounter + 1
                   end do NEI
                end do
             end do

             ! Recover the state from the counter:
             if(iCounter==0) call stop_mpi(&
                  'No good neighbors for the cell near the second body')
             ! Average the state vector over good neighboring cells
             State_VGB(:,i,j,k,iBlock) =  StateCounter_V/iCounter
          end if
       enddo; enddo; enddo
    enddo
    ! Update ghostcells which is needed if the new physical cells are
    ! near the block boundary
    call exchange_messages

    call test_stop(NameSub, DoTest)
  end subroutine update_secondbody
  !============================================================================
end module ModAdvanceExplicit
!==============================================================================
