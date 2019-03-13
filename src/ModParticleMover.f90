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
  logical :: DoInit = .true.
contains
  !====================================================
  !=============read_param=============================!
  !subroutine reads the following paramaters from the PARAM.in file:
  !#CHARGEDPARTICLES
  !3                      nKindChargedParticles
  !4.0                    Mass
  !2.0                    Charge
  !1000000                nParticleMax
  !16.0                   Mass
  !6.0                    Charge
  !500000                 nParticleMax
  !16.0                   Mass
  !7.0                    Charge
  !500000                 nParticleMax
  !--------------------------------------------------------------------------
  subroutine read_charged_particle_param(NameCommand)

    use ModMain,      ONLY: NameThisComp
    use ModReadParam, ONLY: read_var

    character(len=*), intent(in) :: NameCommand

    logical:: DoTest, UseParticles = .false.
    character(len=*), parameter:: NameSub = 'read_charged_particle_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(NameCommand)
    case("#CHARGEDPARTICLES")
       call read_var('UseParticles', UseParticles)
       if(UseParticles)then
          call read_var('nKindChargedParticles', nKindChargedParticles)
          if(nKindChargedParticles<= 0) call stop_mpi(&
               NameThisComp//':'//NameSub//&
               ': invalid number of charged particle kinds')
          call read_var('Mass_I', Mass_I)
          if(Mass_I<= 0) call stop_mpi(&
               NameThisComp//':'//NameSub//&
               ': invalid mass of charged particle kind')
          call read_var('Charge_I', Charge_I)
          if(Charge_I<= 0) call stop_mpi(&
               NameThisComp//':'//NameSub//&
               ': invalid charge of charged particle kind')
          call read_var('nParticleMax_I', nParticleMax_I)
          if(nParticleMax_I<= 0) call stop_mpi(&
               NameThisComp//':'//NameSub//&
               ': invalid number of charged particles for ikind')
       end if 
    end select
    call test_stop(NameSub, DoTest)
  end subroutine read_charged_particle_param 
  !====================================================
  subroutine allocate_charged_particles(Mass_I, Charge_I, nParticleMax_I)
    real,    intent(in)    :: Mass_I(:), Charge_I(:)
    integer, intent(in)    :: nParticleMax_I(:)
    integer :: iKind
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'allocate_charged_particles'
    !-----------------------------!
    call test_start(NameSub, DoTest)
    if(.not.DoInit) RETURN
    DoInit = .false.
    !Allocate enough arrays for all Charged Particle Types
    nKindChargedParticles = size(nParticleMax_I)
    allocate(iKindParticle_I(nKindChargedParticles)) 
    allocate(Charge2Mass_I(nKindChargedParticles))
    Charge2Mass_I = Charge_I/Mass_I
    iKindParticle_I = -1
    do iKind = 1, nKindChargedParticles 
       call allocate_particles(&
            iKindParticle = iKindParticle_I(iKind), &
            nVar          = nVar    , &
            nIndex        = 1  , &
            nParticleMax  = nParticleMax_I(iKind)    )
    end do
    call test_stop(NameSub, DoTest)
  end subroutine allocate_charged_particles
  !====================================================
  subroutine trace_charged_particles(iSort, Xyz_DI, iIndex_II, UseInputInGenCoord)
    integer, intent(in) :: iSort
    ! trace particle locations starting at Xyz_DI;
    real,            intent(in) ::Xyz_DI(:, :)
    ! initial particle indices for starting particles
    integer,optional,intent(in) :: iIndex_II(:,:)

    ! An input can be in generalized coordinates
    logical, optional,intent(in) :: UseInputInGenCoord
    integer :: nParticleOld ! number of already existing charged particles
    integer :: nParticleAll ! number of all particles on all PEs

    !=======================PARTICLE MOVER=========================!
    !Advance the particles in one timestep; calculate cell-centered
    !number density and velocity moments
  end subroutine trace_charged_particles
end module ModParticleMover
