!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModParticleMover
  use BATL_lib, ONLY: &
       test_start, test_stop
  use BATL_lib, ONLY: nDim, nI, nJ, nK, nIJK, nG, &
       MinI, MaxI, MinJ, MaxJ, MinK, MaxK, &
       nBlock, MaxBlock, Unused_B, &
       iProc, iComm, message_pass_cell

  use ModAdvance,      ONLY: Efield_DGB
  use ModMain,         ONLY: n_step
  use ModGeometry,     ONLY: far_field_bcs_blk, true_cell
  use ModCellBoundary, ONLY: set_cell_boundary
contains
  subroutine trace_charged_particles(iSort)
    integer, intent(in) :: iSort
    !=======================PARTICLE MOVER=========================!
    !Advance the particles in one timestep; calculate cell-centered
    !number density and velocity moments
  end subroutine trace_charged_particles
end module ModParticleMover
