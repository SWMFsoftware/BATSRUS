!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModUserEmpty

  ! This module contains empty user routines. They should be "used"
  ! (included) in the srcUser/ModUser*.f90 files for routines that the user
  ! does not wish to implement.

  ! These constants are provided for convenience
  use BATL_lib, ONLY: iProc, x_, y_, z_, MinI, MaxI, MinJ, MaxJ, MinK, MaxK
  use ModBatsrusUtility, ONLY: stop_mpi, get_iVar
  use ModMain, ONLY: &
       UseUserInitSession, UseUserPerturbation, UseUserICs, UseUserB0, &
       UseUserSourceExpl, UseUserSourceImpl, UseUserUpdateStates,      &
       UseUserTimeStep
  implicit none

  private :: stop_user

contains
  !============================================================================
  subroutine user_set_boundary_cells(iBlock)

    ! Set "false" cells that are surrounded by the "extra" face boundary
 
    integer,intent(in)::iBlock

    character(len=*), parameter:: NameSub = 'user_set_boundary_cells'
    !--------------------------------------------------------------------------
    call stop_user(NameSub)

    ! where(....) iBoundary_GB(:,:,:,iBlock) = ExtraBc_
    
  end subroutine user_set_boundary_cells
  !============================================================================
  subroutine user_set_face_boundary(FBC)

    ! Apply user defined face boundary condition

    use ModMain, ONLY: FaceBCType
    type(FaceBCType) :: FBC

    character(len=*), parameter:: NameSub = 'user_set_face_boundary'
    !--------------------------------------------------------------------------
    FBC%VarsGhostFace_V = 0.0

    call stop_user(NameSub)

  end subroutine user_set_face_boundary
  !============================================================================
  subroutine user_set_cell_boundary(iBlock, iSide, TypeBc, IsFound)
    !$acc routine vector

    ! Apply user defined ghost cell boundary condition
    
    integer,          intent(in)  :: iBlock, iSide
    character(len=*), intent(in)  :: TypeBc
    logical,          intent(out) :: IsFound

    character(len=*), parameter:: NameSub = 'user_set_cell_boundary'
    !--------------------------------------------------------------------------
    IsFound = .false.
    call stop_user(NameSub)
    
  end subroutine user_set_cell_boundary
  !============================================================================
  subroutine user_initial_perturbation
    
    ! Apply initial perturbation. 
    ! The routine is called once and should be applied for all blocks, the
    ! do-loop should be present. Another distinction from user_set_ics is that
    ! user_initial_perturbation can be applied after restart, while
    ! user_set_ICs cannot.

    character(len=*), parameter:: NameSub = 'user_initial_perturbation'
    !--------------------------------------------------------------------------
    UseUserPerturbation = .false. ! if not implemented
    
    !do iBlock = 1, nBlockMax
    !   State_VGB... = 
    !end do

  end subroutine user_initial_perturbation
  !============================================================================
  subroutine user_set_ics(iBlock)

    ! Set user defined initial state
    
    integer, intent(in) :: iBlock

    character(len=*), parameter:: NameSub = 'user_set_ics'
    !--------------------------------------------------------------------------
    UseUserICs = .false.  ! if not implemented

    ! State_VGB(...,iBlock) = ...
    
  end subroutine user_set_ics
  !============================================================================
  subroutine user_init_session

    ! Initialize the user module. This routine is called in every session
    ! by default.

    character(len=*), parameter:: NameSub = 'user_init_session'
    !--------------------------------------------------------------------------
    UseUserInitSession = .false. ! if not implemented
    
  end subroutine user_init_session
  !============================================================================
  subroutine user_action(NameAction)

    ! This routine gets called multiple times during the run and provides
    ! an opportunity to do something.
    ! NameAction identifies where the call is from.

    character(len=*), intent(in):: NameAction

    ! select case(NameAction)
    ! case('initialize module')
    !
    ! case('clean module')
    !  
    ! case('reading restart files')
    !
    ! case('initial condition done')
    !
    ! case('load balance done')
    !
    ! case('write progress')
    !
    ! end select

    character(len=*), parameter:: NameSub = 'user_action'
    !--------------------------------------------------------------------------
  end subroutine user_action
  !============================================================================
  subroutine user_specify_region(iArea, iBlock, nValue, NameLocation, &
       IsInside, IsInside_I, Value_I)

    ! user specified geometric criteria

    integer,   intent(in):: iArea        ! area index in BATL_region
    integer,   intent(in):: iBlock       ! block index
    integer,   intent(in):: nValue       ! number of output values
    character, intent(in):: NameLocation ! c, g, x, y, z, or n

    logical, optional, intent(out) :: IsInside
    logical, optional, intent(out) :: IsInside_I(nValue)
    real,    optional, intent(out) :: Value_I(nValue)

    character(len=*), parameter:: NameSub = 'user_specify_region'
    !--------------------------------------------------------------------------
    call stop_user(NameSub)

  end subroutine user_specify_region
  !============================================================================
  subroutine user_amr_criteria(iBlock, UserCriteria, TypeCriteria, IsFound)

    ! User defined AMR criteria. Set the value of UserCriteria for
    ! for block iBlock. Multiple user criteria can implemented, distinguished
    ! by TypeCriteria (starting with 'user'). IsFound should be set to .true.
    ! if TypeCriteria is recognized.
    
    integer, intent(in)          :: iBlock
    real, intent(out)            :: UserCriteria
    character(len=*), intent(in) :: TypeCriteria
    logical, intent(inout)       :: IsFound

    character(len=*), parameter:: NameSub = 'user_amr_criteria'
    !--------------------------------------------------------------------------
    UserCriteria = 0.0
    IsFound = .false.

    call stop_user(NameSub//'(TypeCrit='//TypeCriteria//')')

  end subroutine user_amr_criteria
  !============================================================================
  subroutine user_read_inputs

    character(len=*), parameter:: NameSub = 'user_read_inputs'
    !--------------------------------------------------------------------------
    call stop_user(NameSub)

  end subroutine user_read_inputs
  !============================================================================
  subroutine user_get_log_var(VarValue, NameVar, Radius)

    ! Based on NameVar set log variable VarValue. Radius parameter can
    ! be used to define the log variable (e.g. integral of some flux at
    ! a given Radius)

    real, intent(out)            :: VarValue
    character(len=*), intent(in) :: NameVar
    real, intent(in), optional   :: Radius
    logical :: DoWarn = .true.

    character(len=*), parameter:: NameSub = 'user_get_log_var'
    !--------------------------------------------------------------------------
    if(DoWarn .and. iProc==0) write(*,*) '!!! WARNING: ',NameSub, &
         ' is not implemented. NameVar=', NameVar
    DoWarn = .false.
    VarValue = -7777.0
    
  end subroutine user_get_log_var
  !============================================================================
  subroutine user_set_plot_var(iBlock, NameVar, IsDimensional, &
       PlotVar_G, PlotVarBody, UsePlotVarBody, &
       NameTecVar, NameTecUnit, NameIdlUnit, IsFound)

    ! Based on NameVar set PlotVar_G and related variables for block iBlock
    
    integer,          intent(in)   :: iBlock
    character(len=*), intent(in)   :: NameVar
    logical,          intent(in)   :: IsDimensional
    real,             intent(inout):: PlotVar_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)
    real,             intent(out)  :: PlotVarBody
    logical,          intent(out)  :: UsePlotVarBody
    character(len=*), intent(inout):: NameTecVar
    character(len=*), intent(inout):: NameTecUnit
    character(len=*), intent(inout):: NameIdlUnit
    logical,          intent(out)  :: IsFound

    character(len=*), parameter:: NameSub = 'user_set_plot_var'
    !--------------------------------------------------------------------------
    PlotVar_G = 0.0
    PlotVarBody = 0.0
    UsePlotVarBody = .false.
    IsFound = .false.

    call stop_user(NameSub//'(NameVar='//NameVar//')')

  end subroutine user_set_plot_var
  !============================================================================
  subroutine user_calc_sources_expl(iBlock)

    ! Set explicit source terms for block iBlock

    integer, intent(in) :: iBlock

    character(len=*), parameter:: NameSub = 'user_calc_sources'
    !--------------------------------------------------------------------------
    UseUserSourceExpl = .false. ! if not implemented
    ! Source_VC(...,iBlock) = ...
    
  end subroutine user_calc_sources_expl
  !============================================================================
  subroutine user_calc_sources_impl(iBlock)

    ! Set point implicit source terms for block iBlock

    integer, intent(in) :: iBlock

    character(len=*), parameter:: NameSub = 'user_calc_sources_impl'
    !--------------------------------------------------------------------------
    UseUserSourceImpl = .false. ! if not implemented
    ! Source_VC(...,iBlock) = ... 
    
  end subroutine user_calc_sources_impl
  !============================================================================
  subroutine user_init_point_implicit

    ! Set list of point-implicit variables and initialize other parameters
    ! for point-implicit evaluation of source terms in user_calc_sources_impl
    
    character(len=*), parameter:: NameSub = 'user_init_point_implicit'
    !--------------------------------------------------------------------------
    UseUserSourceImpl = .false. ! if not implemented

    ! allocate and set iVarPointImpl_I and IsPointImplMatrixSet
    ! can also set UseUserPointImplicit_B and IsDynamicPointImplicit

  end subroutine user_init_point_implicit
  !============================================================================
  subroutine user_get_b0(x, y, z, B0_D)

    ! Set or modify B0_D for location x, y, z
    
    real, intent(in)   :: x, y, z
    real, intent(inout):: B0_D(3)

    character(len=*), parameter:: NameSub = 'user_get_b0'
    !--------------------------------------------------------------------------
    UseUserB0 = .false. ! if not implemented

  end subroutine user_get_b0
  !============================================================================
  subroutine user_update_states(iBlock)

    ! Update State_VGB with a user defined method.
    ! It may call update_state_normal and add some extra steps
    
    integer,intent(in):: iBlock

    character(len=*), parameter:: NameSub = 'user_update_states'
    !--------------------------------------------------------------------------
    UseUserUpdateStates = .false. ! if not implemented

  end subroutine user_update_states
  !============================================================================
  subroutine user_calc_timestep(iBlock)

    ! Reduce the time step if it is affected by the user added processes
    integer,intent(in):: iBlock

    character(len=*), parameter:: NameSub = 'user_calc_timestep'
    !--------------------------------------------------------------------------
    UseUserTimeStep = .false. ! if not implemented
    
    ! DtMax_CB(1:nI,1:nJ,1:nK,iBlock) = &
    !       min(DtMax_CB(1:nI,1:nJ,1:nK,iBlock),...)
    
  end subroutine user_calc_timestep
  !============================================================================
  subroutine user_normalization

    ! Set user specific normalization. 
    
    use ModPhysics

    character(len=*), parameter:: NameSub = 'user_normalization'
    !--------------------------------------------------------------------------
    call stop_user(NameSub)

    ! No2Si_V(UnitX_)  = ...
    ! No2Si_V(UnitU_)  = ...
    ! No2Si_V(UnitRho_)= ...

  end subroutine user_normalization
  !============================================================================
  subroutine user_io_units

    ! Set user defined I/O units

    use ModPhysics

    character(len=*), parameter:: NameSub = 'user_io_units'
    !--------------------------------------------------------------------------
    call stop_user(NameSub)

    ! Io2Si_V(UnitX_)           = ...
    ! Io2Si_V(UnitRho_)         = ...
    ! Io2Si_V(UnitN_)           = ...
    ! Io2Si_V(UnitU_)           = ...
    ! Io2Si_V(UnitP_)           = ...
    ! Io2Si_V(UnitB_)           = ...
    ! Io2Si_V(UnitRhoU_)        = ...
    ! Io2Si_V(UnitEnergydens_)  = ...
    ! Io2Si_V(UnitJ_)           = ...
    ! Io2Si_V(UnitDivB_)        = ...
    ! Io2Si_V(UnitAngle_)       = ...
    ! NameTecUnit_V(UnitX_)     = ...
    ! ...
    ! NameIdlUnit_V(UnitX_)     = ...

  end subroutine user_io_units
  !============================================================================
  subroutine user_set_resistivity(iBlock, Eta_G)
    !$acc routine vector

    ! Set Eta_G for block iBlock

    integer, intent(in) :: iBlock
    real,    intent(out):: Eta_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)

    character(len=*), parameter:: NameSub = 'user_set_resistivity'
    !--------------------------------------------------------------------------
    Eta_G = 0.0

    call stop_user(NameSub)

  end subroutine user_set_resistivity
  !============================================================================
  subroutine user_material_properties(State_V, i, j, k, iBlock, iDir, &
       EinternalIn, TeIn, NatomicOut, AverageIonChargeOut, &
       EinternalOut, TeOut, PressureOut, &
       CvOut, GammaOut, HeatCondOut, IonHeatCondOut, TeTiRelaxOut, &
       OpacityPlanckOut_W, OpacityEmissionOut_W, OpacityRosselandOut_W, &
       PlanckOut_W)

    ! The State_V vector is in normalized units, all other physical
    ! quantities are in SI.
    !
    ! If the electron energy is used, then EinternalIn, EinternalOut,
    ! PressureOut, CvOut refer to the electron internal energies,
    ! electron pressure, and electron specific heat, respectively.
    ! Otherwise they refer to the total (electron + ion) internal energies,
    ! total (electron + ion) pressure, and the total specific heat.

    use ModAdvance,    ONLY: nWave
    use ModVarIndexes, ONLY: nVar

    real, intent(in) :: State_V(nVar)
    integer, optional, intent(in):: i, j, k, iBlock, iDir  ! cell/face index
    real, optional, intent(in)  :: EinternalIn             ! [J/m^3]
    real, optional, intent(in)  :: TeIn                    ! [K]
    real, optional, intent(out) :: NatomicOut              ! [1/m^3]
    real, optional, intent(out) :: AverageIonChargeOut     ! dimensionless
    real, optional, intent(out) :: EinternalOut            ! [J/m^3]
    real, optional, intent(out) :: TeOut                   ! [K]
    real, optional, intent(out) :: PressureOut             ! [Pa]
    real, optional, intent(out) :: CvOut                   ! [J/(K*m^3)]
    real, optional, intent(out) :: GammaOut                ! dimensionless
    real, optional, intent(out) :: HeatCondOut             ! [J/(m*K*s)]
    real, optional, intent(out) :: IonHeatCondOut          ! [J/(m*K*s)]
    real, optional, intent(out) :: TeTiRelaxOut            ! [1/s]
    real, optional, intent(out) :: &
         OpacityPlanckOut_W(nWave)                         ! [1/m]
    real, optional, intent(out) :: &
         OpacityEmissionOut_W(nWave)                       ! [1/m]
    real, optional, intent(out) :: &
         OpacityRosselandOut_W(nWave)                      ! [1/m]

    ! Multi-group specific interface. The variables are respectively:
    ! Group Planckian spectral energy density
    real, optional, intent(out) :: PlanckOut_W(nWave)      ! [J/m^3]

    character(len=*), parameter:: NameSub = 'user_material_properties'
    !--------------------------------------------------------------------------
    call stop_user(NameSub)

  end subroutine user_material_properties
  !============================================================================
  integer function i_type_block_user(iBlock)

    ! Define block type for load balancing

    integer, intent(in), optional:: iBlock
    !--------------------------------------------------------------------------
    i_type_block_user = 0

  end function i_type_block_user
  !============================================================================
  subroutine stop_user(NameSub)

    ! Note that this routine is not a user routine but just a routine
    ! which warns the user if they try to use an unimplemented user routine.

    character(len=*), intent(in) :: NameSub
    !--------------------------------------------------------------------------
#ifndef _OPENACC    
    call stop_mpi('You are trying to call the empty user routine '//   &
         NameSub//'. Please implement the routine in src/ModUser.f90')
#endif    

  end subroutine stop_user
  !============================================================================
end module ModUserEmpty
!==============================================================================
