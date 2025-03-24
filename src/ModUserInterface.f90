!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModUserInterface

  implicit none

  ! Provide the interface information for user routines in user_interface.f90
  ! This is required if the routine has optional arguments.
  ! The external can be called instead of calling the routines directly
  ! from ModUser to avoid circular dependencies. This module can be used
  ! instead of ModUser to check/provide the subroutine interface.
  ! To avoid circular dependencies at the level of files,
  ! this module cannot be in the same file as user_interface.f90.

  interface

     subroutine set_user_version

     end subroutine set_user_version

     subroutine user_set_boundary_cells(iBlock)

       integer,intent(in)::iBlock

     end subroutine user_set_boundary_cells

     subroutine user_set_face_boundary(FBC)

       use ModMain, ONLY: FaceBCType

       type(FaceBCType) :: FBC

     end subroutine user_set_face_boundary

     subroutine user_set_cell_boundary(iBlock, iSide, TypeBc, IsFound)
       !$acc routine vector
       integer,          intent(in)  :: iBlock, iSide
       character(len=*), intent(in)  :: TypeBc
       logical,          intent(out) :: IsFound

     end subroutine user_set_cell_boundary

     subroutine user_initial_perturbation

     end subroutine user_initial_perturbation

     subroutine user_set_ics(iBlock)

       integer, intent(in) :: iBlock

     end subroutine user_set_ics

     subroutine user_init_session

     end subroutine user_init_session

     subroutine user_action(NameAction)

       character(len=*), intent(in):: NameAction

     end subroutine user_action

     subroutine user_specify_region(iArea, iBlock, nValue, NameLocation, &
          IsInside, IsInside_I, Value_I)

       integer,   intent(in):: iArea        ! area index in BATL_region
       integer,   intent(in):: iBlock       ! block index
       integer,   intent(in):: nValue       ! number of output values
       character, intent(in):: NameLocation ! c, g, x, y, z, or n

       logical, optional, intent(out) :: IsInside
       logical, optional, intent(out) :: IsInside_I(nValue)
       real,    optional, intent(out) :: Value_I(nValue)

     end subroutine user_specify_region

     subroutine user_amr_criteria(iBlock, UserCriteria, TypeCriteria, IsFound)

       integer, intent(in)          :: iBlock
       real, intent(out)            :: UserCriteria
       character(len=*), intent(in) :: TypeCriteria
       logical, intent(inout)       :: IsFound

     end subroutine user_amr_criteria

     subroutine user_read_inputs

     end subroutine user_read_inputs

     subroutine user_get_log_var(VarValue, TypeVar, Radius)

       real, intent(out)           :: VarValue
       character(len=*), intent(in):: TypeVar
       real, optional, intent(in)  :: Radius

     end subroutine user_get_log_var

     subroutine user_set_plot_var(iBlock, NameVar, IsDimensional, &
          PlotVar_G, PlotVarBody, UsePlotVarBody, &
          NameTecVar, NameTecUnit, NameIdlUnit, IsFound)

       use ModSize, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK

       integer,          intent(in)   :: iBlock
       character(len=*), intent(in)   :: NameVar
       logical,          intent(in)   :: IsDimensional
       real,             intent(inout):: &
            PlotVar_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)
       real,             intent(out)  :: PlotVarBody
       logical,          intent(out)  :: UsePlotVarBody
       character(len=*), intent(inout):: NameTecVar
       character(len=*), intent(inout):: NameTecUnit
       character(len=*), intent(inout):: NameIdlUnit
       logical,          intent(out)  :: IsFound

     end subroutine user_set_plot_var

     subroutine user_calc_sources_expl(iBlock)

       integer, intent(in) :: iBlock
       ! Change Source_VC here
     end subroutine user_calc_sources_expl

     subroutine user_calc_sources_impl(iBlock)

       integer, intent(in) :: iBlock
       ! Change Source_VC here
     end subroutine user_calc_sources_impl

     subroutine user_init_point_implicit

     end subroutine user_init_point_implicit

     subroutine user_get_b0(x, y, z, B0_D)

       real, intent(in)   :: x, y, z
       real, intent(inout):: B0_D(3)

     end subroutine user_get_b0

     subroutine user_update_states(iBlock)

       integer,intent(in):: iBlock

     end subroutine user_update_states

     subroutine user_normalization

     end subroutine user_normalization

     subroutine user_io_units

     end subroutine user_io_units

     subroutine user_set_resistivity(iBlock, Eta_G)
      !$acc routine vector

       use ModSize, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK

       integer, intent(in) :: iBlock
       real,    intent(out):: Eta_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)

     end subroutine user_set_resistivity

     subroutine user_material_properties(State_V, i, j, k, iBlock, iDir, &
          EinternalIn, TeIn, NatomicOut, AverageIonChargeOut, &
          EinternalOut, TeOut, PressureOut, &
          CvOut, GammaOut, HeatCondOut, IonHeatCondOut, TeTiRelaxOut, &
          OpacityPlanckOut_W, OpacityEmissionOut_W, OpacityRosselandOut_W, &
          PlanckOut_W)

       use ModAdvance,    ONLY: nWave
       use ModVarIndexes, ONLY: nVar
       implicit none
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
       real, optional, intent(out) :: PlanckOut_W(nWave)      ! [J/m^3]

     end subroutine user_material_properties

     integer function i_type_block_user(iBlock)

       integer, intent(in), optional:: iBlock
     end function i_type_block_user

  end interface

end module ModUserInterface
!==============================================================================
