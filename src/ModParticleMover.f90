!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModParticleMover
  use BATL_lib, ONLY: &
       test_start, test_stop
  use BATL_lib, ONLY: nDim,  MaxDim, nBlock, MaxBlock, iProc, iComm

  use ModMain,         ONLY: NameThisComp
  use ModAdvance,      ONLY: UseEfield, Efield_DGB, State_VGB
  use ModVarIndexes,   ONLY: Bx_, Bz_
  use ModParticles,    ONLY: gen_allocate_particles=>allocate_particles, &
       gen_deallocate_particles=>deallocate_particles
  use BATL_particles,  ONLY: Particle_I, &
       mark_undefined, check_particle_location, &
       set_pointer_to_particles
  use ModNumConst
  implicit none
  SAVE
  !\
  ! If true, the particles are allocated and may be traced
  !/
  logical :: UseParticles = .false.
  !\
  !Parameters
  !/
  !\
  ! number of particles with different charge-to-mass ratios
  integer :: nKindParticles = 0 
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
  !\
  ! Indexes in coordinate-velocity-mass array
  !/
  integer, parameter :: x_ = 1, y_ = 2, z_=3, U_ = nDim,       &
       Ux_ = U_ + x_, Uy_= U_ + y_, Uz_ = U_ + z_, Mass_= Uz_ +1,&
       nVar = Mass_
  !\
  ! Indexes in the index array
  !/
  integer, parameter :: &
       Status_ = 1, DoAll_ = 1, DoCollect_ = 2, Done_= 3 
  logical :: DoInit = .true.
  !\
  ! For conveniently address to coordinates and 
  ! indexes of a particular sort of particle
  !/
  real,    pointer :: Coord_DI(:,:)
  integer, pointer :: Index_II(:,:)
  !\
  ! Indexes in the array to collect particle VDF moments
  !/
  integer, parameter :: Rho_ = 1, RhoU_ = 1, RhoUx_ = RhoU_ + x_, &
       RhoUy_ = RhoU_ + y_, RhoUz_ = RhoU_ + z_
  !\
  ! Moments of the particle VDFs
  !/
  real,    allocatable :: MomentsMinus_DGBI(:,:,:,:,:,:)
  real,    allocatable :: MomentsPlus_DGBI(:,:,:,:,:,:)
  real,    allocatable :: Moments_DGBI(:,:,:,:,:,:)

  !\
  ! Global variables to be shared by more than one routine
  real    :: Dt          !Time step
  integer :: iKind       !Sort of particles
  !\
  ! (Dt/2)*Zq/(Am_p), to convert field to force
  !/
  real:: QDtPerM      !For a given sort of particles
  character(len=*), parameter:: NameMod = 'ModParticleMover:' 
contains
  !=============read_param=============================!
  !subroutine read the following paramaters from the PARAM.in file:
  !#CHARGEDPARTICLES
  !3                      nKindParticles
  !4.0                    Mass
  !2.0                    Charge
  !1000000                nParticleMax
  !16.0                   Mass
  !6.0                    Charge
  !500000                 nParticleMax
  !16.0                   Mass
  !7.0                    Charge
  !500000                 nParticleMax
  !and allocate particle arrays immediately.
  !To reallocate, first, use command
  !#CHARGEDPARTICLES
  !0                      nKindParticles
  !to deallocate arrays.
  !--------------------------------------------------------------------------
  subroutine read_param(NameCommand)

    use ModReadParam, ONLY: read_var

    character(len=*), intent(in) :: NameCommand

    logical:: DoTest
    integer :: iLoop
    !\
    !Arrays to read A and Z for different ion particles
    !/
    real, allocatable :: Mass_I(:), Charge_I(:)
    !\
    ! array to read maximum number of particles for different
    ! sorts of ions
    !/
    integer, allocatable :: nParticleMax_I(:)
    character(len=*), parameter:: NameSub = NameMod//'read_param'
    !--------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(NameCommand)

    case("#CHARGEDPARTICLES")
       call read_var('nKindParticles', nKindParticles)
       if(nKindParticles<= 0) then
          !\
          ! Clean particle arrays
          !/
          if(UseParticles)call deallocate_particles
          UseParticles = .false.
          RETURN
       end if
       !\
       ! The particle arrays can be reset, however,
       ! first nKindParticle should be set to zero
       ! to deallocate available arrays
       !/
       if(UseParticles)call stop_mpi(NameThisComp//':'//NameSub//&
               ': To reset particle arrays first use '//&
               '#CHARGEDPARTICLES command with nKindParticles=0')
       UseParticles = .true.
       allocate(  Mass_I(nKindParticles))
       allocate(Charge_I(nKindParticles))
       allocate(nParticleMax_I(nKindParticles))
       do iLoop = 1, nKindParticles 
          call read_var('Mass_I', Mass_I(iLoop))
          if(Mass_I(iLoop)<= 0) call stop_mpi(&
               NameThisComp//':'//NameSub//&
               ': invalid mass of charged particle kind')
          call read_var('Charge_I', Charge_I(iLoop))
          call read_var('nParticleMax_I', nParticleMax_I(iLoop))
          if(nParticleMax_I(iLoop)<= 0) call stop_mpi(&
               NameThisComp//':'//NameSub//&
               ': invalid number of charged particles for ikind')
       end do
    case default
       call stop_mpi(&
            NameThisComp//':'//NameSub//&
            ': Unknown command '//NameCommand//' in PARAM.in')
    end select
    call allocate_particles(Mass_I, Charge_I, nParticleMax_I)
    deallocate(Mass_I, Charge_I, nParticleMax_I)
    call test_stop(NameSub, DoTest)
  end subroutine read_param
  !===================== DEALLOCATE====================
  subroutine deallocate_particles
    integer :: iLoop
    !----------------
    nullify(Coord_DI)
    nullify(Index_II)
    !\
    ! Deallocate particle arrays
    !/
    do iLoop = 1, size(iKindParticle_I)
       call gen_deallocate_particles(iKindParticle_I(iLoop))
    end do
    deallocate(iKindParticle_I, Charge2Mass_I, MomentsMinus_DGBI, &
            MomentsPlus_DGBI, Moments_DGBI)
  end subroutine deallocate_particles
  !====================================================
  subroutine allocate_particles(Mass_I, Charge_I, nParticleMax_I)
    use BATL_lib, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK
    real,    intent(in)    :: Mass_I(nKindParticles) 
    real,    intent(in)    :: Charge_I(nKindParticles)
    integer, intent(in)    :: nParticleMax_I(nKindParticles)
    integer :: iKind
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'allocate_charged_particles'
    !-----------------------------!
    call test_start(NameSub, DoTest)

    if(.not.DoInit) RETURN
    DoInit = .false.

    !Allocate enough arrays for all Charged Particle Types
    allocate(iKindParticle_I(nKindParticles)) 
    allocate(Charge2Mass_I(nKindParticles))

    Charge2Mass_I = Charge_I/Mass_I
    
    iKindParticle_I = -1
    do iKind = 1, nKindParticles 
       call gen_allocate_particles(&
            iKindParticle = iKindParticle_I(iKind), &
            nVar          = nVar     , &
            nIndex        = Status_  , &
            nParticleMax  = nParticleMax_I(iKind)    )
    end do
    allocate(MomentsMinus_DGBI(Rho_:RhoUz_,&
         MinI:MaxI,  MinJ:MaxJ, MinK:MaxK, MaxBlock, &
         nKindParticles))
    allocate(MomentsPlus_DGBI(Rho_:RhoUz_,&
         MinI:MaxI,  MinJ:MaxJ, MinK:MaxK, MaxBlock, &
         nKindParticles))
    allocate(Moments_DGBI(Rho_:RhoUz_,&
         MinI:MaxI,  MinJ:MaxJ, MinK:MaxK, MaxBlock, &
         nKindParticles))

    call test_stop(NameSub, DoTest)
  end subroutine allocate_particles
  !===============================================!
  subroutine trace_particles(DtIn)
    use BATL_particles, ONLY: &
         batl_trace_particles=>trace_particles
    real, intent(in) :: DtIn
    integer :: iLoop, nParticle
    !----------------------
    Dt = DtIn
    MomentsMinus_DGBI = 0.0 !Prepare storage for the VDF moments
    MomentsPlus_DGBI = 0.0 !Prepare storage for the VDF moments
    Moments_DGBI = 0.0 !Prepare storage for the VDF moments
    do iLoop = 1, nKindParticles
       iKind = iKindParticle_I(iLoop)
       call set_pointer_to_particles(&
            iKind, Coord_DI, Index_II, &
            nParticle=nParticle)
       !\
       ! (Dt/2)*Zq/(Am_p), to convert field to force
       !/
       QDtPerM = cHalf * Dt * Charge2Mass_I(iKind)
       Index_II(Status_,1:nParticle) = DoAll_
       call batl_trace_particles(iKind, boris_scheme, check_done)
    end do

  end subroutine trace_particles
  !=====================================
  subroutine boris_scheme(iParticle, EndOfSegment)
    !\
    ! In this routine we follow the formulation described in the Book: 
    ! Plasma Physics via Computer Simulation, 
    ! Editors: Birdsall, C. K.; Langdon, A. B., 
    ! The Adam Hilger Series on Plasma Physics, 1991.
    ! A time-centered difference method for solving the Newton-Lorentz 
    ! equations of motion is detailed in Subestion 4.3, Eq. (3). 
    ! This method makes use of A) the Buneman 1967 method, through Eq. (4)-(6),
    ! in order to simplify Eq. (3) by subtracting the drift velocity from v, and
    ! B) the Boris Scheme, which separates the electric and magnetic fields , 
    ! through Eq. (7) - (9).
    !/
    !\
    ! WITHIN THE FRAMEWORK OF HYBRYD SCHEME:
    ! At the beginning of the Boris Scheme the 
    ! known vector quantities are: x(N+1/2), u(N).
    ! At the end of the time-step trac_particles has:
    ! 1. Advanced the velocity and location vectors: u(N+1), x(N+3/2)
    ! 2. Uses the E, B-fields: E(N+1/2), B(N+1/2), interpolated to
    !    the particle location at x(N+1/2).
    !    This algorithm corresponds to Step 2(a) (p.109)of the CAM Algorithm
    !     in Matthews, 1993, J. Comput. Phys, v. 112, pp. 102-116.
    ! 3. Collected the current and charge densities at two different points:
    !     a. \rho_c(x(N+1/2),u(N+1)), J(x(N+1/2),u(N+1)) : MomentsMinus_DGBI
    !     b. \rho_c(x(N+3/2),u(N+1)), J(x(N+3/2),u(N+1)) : MomentsPlus_DGBI
    ! This algorithm corresponds to Step 2(b) of the CAM Algorithm in
    ! Matthews, 1993, J. Comput. Phys, v. 112, pp. 102-116.
    ! They are collected for each species separately, as described on page
    ! 110, item 2(b)(i)
    !/
    use ModBatlInterface, ONLY: interpolate_grid_amr_gc
    use ModMain, ONLY: UseB0
    use ModB0, ONLY: get_b0
    use ModCoordTransform, ONLY: cross_product

    integer, intent(in)  :: iParticle
    logical, intent(out) :: EndOfSegment
    !\
    ! Coords
    !/
    real :: Xyz_D(MaxDim)
    !\
    ! Velocity vectors at different stages
    !/
    real,dimension(x_:z_) :: U_D, U12_D 
    !\
    ! Magnetic and electric field vectors
    !/
    real,dimension(x_:z_) :: B_D, E_D
    !\
    ! Fields multiplied by (Dt/2)*(Charge/Mass)
    !/
    real,dimension(x_:z_) :: EForce_D, BForce_D
    !\
    ! Block in which the interpolation is possible
    integer  :: iBlock
    !\
    ! interpolation data: number of cells, cell indices, weights
    !/
    integer:: nCell, iCell_II(0:nDim, 2**nDim)
    real   :: Weight_I(2**nDim) 
    !\
    ! Particle mass and momentum
    !/
    real   :: Mass, Moments_V(Rho_:RhoUz_)
    !Misc
    integer:: iCell ! loop variable
    integer:: i_D(MaxDim)
    !\
    ! Outputs from check_location routine
    !/
    logical :: IsGone, DoMove
    character(len=*), parameter:: NameSub = 'interpolate_Bfield'
    !---------------------
    !\
    ! Routine either completes advance or needs to move the particle
    ! In both situation the external batl_trace_particles routine
    ! does not need to call routine boris_scheme again immediately. 
    ! Therefore,
    EndOfSegment = .true.
    if(Index_II(Status_, iParticle) == Done_ )RETURN
    if(Index_II(Status_, iParticle) == DoAll_)then
       !\
       ! Interpolate particle coordinates and velocities into BATSRUS grid
       !/
       ! Coordinates and block #
       Xyz_D   = 0.0
       Xyz_D(1:nDim)   = Coord_DI(x_:nDim, iParticle)
       iBlock          = Index_II(0,iParticle)
       call interpolate_grid_amr_gc(&
            Xyz_D, iBlock, nCell, iCell_II, Weight_I)
       ! Coordinates and block #
       U_D =  Coord_DI(Ux_:Uz_, iParticle)
       !\
       ! Interpolate fields with obtained weight coefficients
       !/
       B_D = 0.0;   E_D = 0.0
       ! get potential part of the magnetic field at current coordinates 
       if(UseB0)call get_b0(Xyz_D, B_D)
       !\
       ! Interpolate electric field and the remaining non-potential part 
       ! of the magnetic field
       !/ 
       do iCell = 1, nCell
          i_D = 1
          i_D(1:nDim) = iCell_II(1:nDim, iCell)
          B_D = B_D + &
               State_VGB(Bx_:Bz_,i_D(1),i_D(2),i_D(3),iBlock)*Weight_I(iCell)
          E_D = E_D + &
               Efield_DGB(:,i_D(1),i_D(2),i_D(3),iBlock)*Weight_I(iCell)
       end do
       !\
       ! Calculate individual contributions from E, B field on  velocity 
       !/
       !Electric field force, divided by particle mass
       !and multiplied by \Delta t/2
       Eforce_D = QDtPerM*E_D 
       ! Get the velocity, add
       ! acceleration from the electric field, for the
       ! first half of the time step:
       U_D = U_D  + Eforce_D
       !Get magnetic force divided by particle mass
       !and multiplied by \Delta t/2
       BForce_D = QDtPerM*B_D
       !Add a half of the magnetic rotation:
       U12_D = U_D + cross_product(U_D,BForce_D)
       !Multiply the magnetic force by 2 to take a whole
       !rotation and reduce its magnitude not to perturb energy
       BForce_D = (2.0/(1.0 + sum(BForce_D**2))) * BForce_D
       !\
       ! Update velocity
       !/
       U_D = U_D + cross_product(U12_D,BForce_D) + Eforce_D
       !\
       ! Collect the contribution to moments of VDF, 
       ! from a given particle with coordinates at half time step
       ! before velocity
       !/
       Moments_V(Rho_) = Mass
       Moments_V(RhoUx_:RhoUz_) = Mass*U_D
       do iCell = 1, nCell
          i_D = 1
          i_D(1:nDim) = iCell_II(1:nDim, iCell)
          MomentsMinus_DGBI(:,i_D(1),i_D(2),i_D(3),iBlock,iKind) = &
               MomentsMinus_DGBI(:,i_D(1),i_D(2),i_D(3),iBlock,iKind) + &
               Moments_V*Weight_I(iCell)
       end do
       !\
       ! Save coordinate array
       !/
       Coord_DI(Ux_:Uz_,iParticle) = U_D
       !\
       ! Update coordinates
       !/
       Coord_DI(x_:nDim, iParticle) = Coord_DI(x_:nDim, iParticle) + &
            Dt*Coord_DI(Ux_:U_+nDim,iParticle)
       ! check location, schedule for message pass, if needed
       call check_particle_location(       &
            iKindParticle = iKind         ,&
            iParticle     = iParticle,     &
            DoMove        = DoMove        ,&
            IsGone        = IsGone    )
       if(IsGone)then
          !\
          ! Particle left the computational domain, schedule for 
          ! removal
          !/
          call mark_undefined(iKind, iParticle)
          !\
          ! Mark as done for not calling the routine for 
          ! this particle any longer
          !/
          Index_II(Status_,iParticle) = Done_
          RETURN
       elseif(DoMove)then
          !\
          ! Particle will be send to other processor, the current
          ! will be collected there
          !/
          Index_II(Status_,iParticle) = DoCollect_
          RETURN
       end if
    end if
    ! Coordinates and block #
    Xyz_D   = 0.0
    Xyz_D(1:nDim)   = Coord_DI(x_:nDim, iParticle)
    iBlock          = Index_II(0,iParticle)
    call interpolate_grid_amr_gc(&
         Xyz_D, iBlock, nCell, iCell_II, Weight_I)
    !\
    ! Get the contribution to moments of VDF, 
    ! from a given particle with coordinates at half time step
    ! after velocity.
     !/
    Moments_V(Rho_)          =  Coord_DI(Mass_, iParticle)
    Moments_V(RhoUx_:RhoUz_) = Moments_V(Rho_)*&
         Coord_DI(Ux_:Uz_, iParticle)
    !\
    ! Collect Contribution with updated weight coefficients
    !/
    do iCell = 1, nCell
       i_D = 1
       i_D(1:nDim) = iCell_II(1:nDim, iCell)
       MomentsPlus_DGBI(:,i_D(1),i_D(2),i_D(3),iBlock,iKind) = &
            MomentsPlus_DGBI(:,i_D(1),i_D(2),i_D(3),iBlock,iKind) + &
            Moments_V*Weight_I(iCell)
    end do
    Index_II(Status_, iParticle) = Done_
  end subroutine boris_scheme
  !==========================
  subroutine check_done(Done)
    ! check whether all paritcles have been advanced through 
    ! full time step
    logical, intent(out):: Done
    !--------------------------------------------------------------------------
    Done = all(Index_II(Status_,1:Particle_I(iKind)%nParticle)==Done_)
  end subroutine check_done
  !==========================
end module ModParticleMover
