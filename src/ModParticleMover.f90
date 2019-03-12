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
  use ModParticles,    ONLY: allocate_particles, message_pass_particles
  use BATL_particles,  ONLY: Particle_I, remove_undefined_particles, &
       mark_undefined, check_particle_location, put_particles, trace_particles
  !\
  !Parameters
  !/
  ! number of particles with different charge-to-mass ratios
  integer :: nKindChargedParticles = 0 
  !\
  ! The order number of this kind of particle in the whole 
  ! BATL_particle repository
  !/
  integer, allocatable :: iKindParticle_I(:)
  !\
  !Charge to mass ratio, same for all particles of a given 
  !kind
  !/
  real,    allocatable :: Charge2Mass_I(:)
  !\
  ! Named indexes
  !/
  integer, parameter :: nVar = 2*nDim +1, x_ = 1, y_ = 2, z_=nDim,&
       Ux_ = nDim + x_, Uy_= nDim + y_, Uz_ = 2*nDim, Mass = Uz_ +1
contains
  subroutine allocate_charged_particles
  end subroutine allocate_charged_particles
  !==================
  subroutine trace_charged_particles(iSort)
    integer, intent(in) :: iSort
    !=======================PARTICLE MOVER=========================!
    !Advance the particles in one timestep; calculate cell-centered
    !number density and velocity moments
  end subroutine trace_charged_particles
end module ModParticleMover
