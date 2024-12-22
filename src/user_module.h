!  Copyright (C) 2002 Regents of the University of Michigan, 
!  portions used with permission 
!  For more information, see http://csem.engin.umich.edu/tools/swmf

  use ModUtilities, ONLY: CON_stop

  implicit none  

  SAVE ! save all module variables

  private ! except  
  public :: NameUserFile
  public :: NameUserModule
  public :: user_set_boundary_cells
  public :: user_set_face_boundary
  public :: user_set_cell_boundary
  public :: user_initial_perturbation
  public :: user_set_ics
  public :: user_init_session
  public :: user_specify_region
  public :: user_amr_criteria
  public :: user_read_inputs
  public :: user_get_log_var
  public :: user_set_plot_var
  public :: user_calc_sources_expl
  public :: user_calc_sources_impl
  public :: user_init_point_implicit
  public :: user_get_b0
  public :: user_update_states
  public :: user_calc_timestep
  public :: user_io_units
  public :: user_normalization
  public :: user_set_resistivity
  public :: user_material_properties
  public :: user_action
  public :: i_type_block_user
