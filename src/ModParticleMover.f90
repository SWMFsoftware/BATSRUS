!  Copyright (C) 2002 Regents of theUniversity of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModParticleMover
  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest
  use BATL_lib, ONLY: nDim,  MaxDim, nBlock, MaxBlock, iProc, iComm, &
       nI, nJ, nK, Unused_B, CoordMin_DB, CellSize_DB, put_particles,& 
       coord_to_xyz, nProc
  use BATL_lib, ONLY: CellVolume_GB

  use ModMain,         ONLY: NameThisComp
  use ModAdvance,      ONLY: UseEfield, Efield_DGB, State_VGB
  use ModVarIndexes,   ONLY: Bx_, Bz_
  use ModMultiFluid,   ONLY: nTrueIon, &
          iRhoIon_I, iPIon_I, iRhoUxIon_I, iRhoUyIon_I, iRhoUzIon_I 
  use ModParticles,    ONLY: gen_allocate_particles=>allocate_particles, &
       gen_deallocate_particles=>deallocate_particles
  use BATL_particles,  ONLY: Particle_I, &
       mark_undefined, check_particle_location, &
       set_pointer_to_particles
  use ModGeometry, ONLY: true_cell
  use ModNumConst
  implicit none
  SAVE
  PRIVATE! Except
  !\
  ! If true, the particles are allocated and may be traced
  !/
  logical, public         :: UseParticles = .false. 
  logical :: DoNormalize = .false.
  !\
  ! If true, one or more ion fluids are solved using hybrid scheme
  logical, public         :: UseHybrid = .false.
  !\
  ! Public members
  !/
  public :: read_param         !Particle parameters
  public :: normalize_param    !To use Batsrus unit for charge
  public :: trace_particles    !Particle mover
  public :: get_state_from_vdf !Uses the moments to get the MHD state
  
  !\
  !Parameters
  !/
  integer :: nHybridParticleSort = 0 
  !\
  ! number of all sorts of charged particles including hybrid and test 
  ! particles: nParticleSort = nHybridParticleSort + nTestParticles
  !/
  integer :: nParticleSort = 0
  !\
  ! The order number of this sort of particle in the whole 
  ! BATL_particle repository. Allocated in the range 1:nParticleSort
  ! Set by a generic procedure ModParticles:allocate_particles. By 
  ! allocating a structure for each new sort of charged particles, the
  ! order number of this structure in the array BATL_particles:Particles_I
  ! is stored in iKindParticle_I. When addressing to this structure in 
  ! setting the pointer to the coordinate array for particles of iSort, or
  ! using any other method from ModParticles or BATL_particles, the array 
  ! is used as follows:
  ! iKind = iKindParticle_I(iSort)
  ! call set_pointer_to_particles(iKind,....) 
  !/
  integer, allocatable :: iKindParticle_I(:)


  integer, allocatable :: iHybridIon_I(:)

  ! Maximum number of particles to be considered for each ion.
  integer, allocatable :: nParticleMax_I(:)
  ! Number of particles in each cell to be considered for each hybrid fluid.
  integer, allocatable :: nHybridParticlePerCell_I(:)
  !\
  !Charge to mass ratio, same for all particles of a given sort.
  !Allocated with the  index range 1:nParticleSort
  !
  !/
  real,    allocatable :: Charge2Mass_I(:)
  !\
  ! Named indexes
  !/
  !\
  ! Indexes in coordinate-velocity-mass array
  !/
  integer, parameter :: x_ = 1, y_ = 2, z_=3, U_ = nDim,       &
       Ux_= U_ + x_, Uy_= U_ + y_, Uz_ = U_ + z_, Mass_= Uz_ +1,&
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
       RhoUy_ = RhoU_ + y_, RhoUz_ = RhoU_ + z_, P_ = RhoUz_, &
       Px_ = P_ + x_, Py_ = P_ + y_, Pz_ = P_ + z_, &
       P12_ = Pz_ + 1, P13_ = Pz_ + 2, P23_  = Pz_ + 3 
  !\
  ! Moments of the particle VDFs
  !/
  real,    allocatable :: Moments_VGBI(:,:,:,:,:,:)
  !\
  ! May be redundant?
  !/
  !\
  ! Charge and current densities
  !/
  real,    allocatable :: DensityAndCurrent_VCB(:,:,:,:,:)
  !\
  ! \Lambda and \Gamma coefficients used by the Current Advance Method (CAM)
  ! as described in Matthews 1993 paper 
  !/
  real,    allocatable :: CAMCoef_VCB( :,:,:,:,:)
  !\
  ! Indexes in the array to collect current and charge densities 
  !/
  integer, parameter :: RhoC_ = 1, J_ = 1, Jx_ = J_ + x_, &
       Jy_ = J_ + y_, Jz_ = J_ + z_, Lambda_ = 1, &
       Gamma_ = Lambda_, GammaX_ = Gamma_ + x_, &
       GammaY_ = Gamma_ + y_, GammaZ_ = Gamma_ + z_
  !\
  ! Global variables to be shared by more than one routine
  !/
  real    :: Dt          !Time step
  integer :: iKind       !Sort of particles
  !True if we make boris algorithm and then advance coordinate 
  !through a time halfstep. False, if we only advance coordinates 
  logical :: DoBorisStep 
  !\
  ! (Dt/2)*Zq/(Am_p), to convert field to force
  !/
  real:: QDtPerM      !For a given sort of particles
  character(len=*), parameter:: NameMod = 'ModParticleMover:' 
contains
  !=============read_param=============================!
  !subroutine read the following paramaters from the PARAM.in file:
  ! #CHARGEDPARTICLES
  ! 3                      nHybridParticleSort
  ! 2                      iHybridIon_I(1) 
  ! 1000000                nHybridParticleMax_I(1)
  ! 5                      nHybridParticlePerCell_I(1)
  ! 3                      iHybridIon_I(2) 
  ! 50000                  nHybridParticleMax_I(2)
  ! 5                      nHybridParticlePerCell_I(2)
  ! 4                      iHybridIon_I(3) 
  ! 150000                 nHybridParticleMax_I(3)
  ! 5                      nHybridParticlePerCell_I(3)
  ! 1                      nTestParticles
  ! 4.0                    Mass_I
  ! 2.0                    Charge_I
  !--------------------------------------------------------------------------
  subroutine read_param(NameCommand)

    use ModReadParam, ONLY: read_var

    character(len=*), intent(in) :: NameCommand

    logical :: DoTest
    integer :: iLoop, nCell
    integer :: nTestParticles = 1
    !\
    !Arrays to read A and Z for different ion particles
    !/
    real, allocatable :: Mass_I(:)
    real, allocatable :: Charge_I(:)
    !\
    ! array to read maximum number of particles for different
    ! sorts of ions
    !/
    integer, allocatable :: nHybridParticleMax_I(:)
    character(len=*), parameter:: NameSub = NameMod//'read_param'
    !--------------------------------------------------------------------
    if(UseParticles) RETURN !Do not reset already set particle arrays
    call test_start(NameSub, DoTest)
    select case(NameCommand)

    case("#CHARGEDPARTICLES")
       !\
       ! Read the total number of hybrid particle sorts from PARAM.in
       !/
       call read_var('nHybridParticleSort', nHybridParticleSort)
       if(nHybridParticleSort>0)then
          UseParticles = .true.
          UseHybrid    = .true.
          allocate(iHybridIon_I(nHybridParticleSort))
          allocate(nHybridParticleMax_I(nHybridParticleSort))
          allocate(nHybridParticlePerCell_I(nHybridParticleSort))
          do iLoop = 1, nHybridParticleSort 
             !\
             ! Read which of the BATSRUS ion fluids (enumerated from 1 to
             ! nTrueIon) will be treaded as hybrid fluids and sampled with  
             ! the charged particles in this module. 
             !/
             call read_var('iHybridIon_I', iHybridIon_I(iLoop))
             if(iHybridIon_I(iLoop)<= 0.or.iHybridIon_I(iLoop)>nTrueIon)&
                  call stop_mpi(NameThisComp//':'//NameSub//&
                  ': invalid type of hybrid ion fluid')
             !\
             ! Read the number of particles of the specific hybrid particle 
             ! sort to be allocated at the beginning of the simulation.
             !/
             call read_var('nHybridParticleMax_I', nHybridParticleMax_I(iLoop))
             if(nHybridParticleMax_I(iLoop)<= 0) call stop_mpi(&
                  NameThisComp//':'//NameSub//&
                  ': invalid number of charged particles for ikind')
             !\
             ! Read the number of particles of the specific hybrid particle sort
             ! to be allocated at each cell of the computational domain.
             !/
             call read_var('nHybridParticlePerCell_I', &
                     nHybridParticlePerCell_I(iLoop))
             if(nHybridParticlePerCell_I(iLoop)<= 0) call stop_mpi(&
                  NameThisComp//':'//NameSub//&
                  ': invalid number of charged particles per cell for ikind')
          end do
       else
          nHybridParticleSort = 0
          UseHybrid  = .false.
       end if
       call read_var('nTestParticles', nTestParticles)
       ! Define the total number of ion sort to be considered    
       nParticleSort = nHybridParticleSort + max(nTestParticles,0)
       if(nParticleSort == 0) RETURN
       UseParticles = .true.
       ! Allocate arrays for all particles: test + hybrid
       allocate(iKindParticle_I(nParticleSort))
       allocate(Mass_I(nParticleSort)); Mass_I = 1.0
       allocate(Charge_I(nParticleSort)); Charge_I = 1.0
       allocate(nParticleMax_I(nParticleSort))
       !\
       ! Store nParticleMax for hybrid particles
       !/
       if(nHybridParticleSort > 0)then
          nParticleMax_I(1:nHybridParticleSort) = nHybridParticleMax_I
          deallocate(nHybridParticleMax_I)
       end if
       !\
       ! The first part of the particle arrays 
       ! Mass_I, Charge_I and nParticleMax_I
       ! are populated by the hybrid particles (1:nHybridParticleSort),
       ! while the second part is populated by the test particles
       ! (nHybridParticleSort+1:nParticleSort).
       !/
       do iLoop = nHybridParticleSort + 1, nParticleSort
          call read_var('MassTest_I', Mass_I(iLoop))
          if(Mass_I(iLoop)<= 0) call stop_mpi(&
               NameThisComp//':'//NameSub//&
               ': invalid mass of charged particle kind')
          call read_var('ChargeTest_I', Charge_I(iLoop))
          call read_var('nTestMax_I', nParticleMax_I(iLoop))
          if(nParticleMax_I(iLoop)<= 0) call stop_mpi(&
               NameThisComp//':'//NameSub//&
               ': invalid number of test particles for ikind')
       end do
    case default
       call stop_mpi(&
            NameThisComp//':'//NameSub//&
            ': Unknown command '//NameCommand//' in PARAM.in')
    end select
    !\
    ! Allocate all sort of particles with 
    ! characteristics: Mass_I, Charge_I, nParticleMax_I.
    ! All sort of particles, charged or otherwise, are allocated through the 
    ! subroutine allocate_particles.
    ! Each sort of particles has a characteristic number and in order to 
    ! access a specific sort of particles this number NEEDS to be stored
    ! in an array for all sort of particles.
    !/
    call allocate_particles(Mass_I, Charge_I, nParticleMax_I)
    !\
    ! Deallocate all the allocated arays.
    !/
    deallocate(Mass_I, Charge_I, nParticleMax_I)
    call test_stop(NameSub, DoTest)
  end subroutine read_param
  !===================== NORMALIZE ====================
  subroutine normalize_param
    use ModPhysics,    ONLY: ElectronCharge
    use ModMultiFluid, ONLY: ChargePerMass_I
    integer :: iLoop
    if(.not.DoNormalize)RETURN
    DoNormalize = .false.   !Do not repeat normalization
    !\
    ! For hybrid particle take charge to mass ratio from
    ! parameters for BATSRUS ion fluids:
    !/
    if(nHybridParticleSort >0 )&
         Charge2Mass_I(1:nHybridParticleSort) = &
         ChargePerMass_I(iHybridIon_I)
    !\
    ! For test particles the Charge ans mass numbers, Z,A
    ! as written from the PARAM.in file. Their ratio should
    ! be normalized in multiplying by a proper factor.
    !/ 
    if(nParticleSort > nHybridParticleSort)&
         Charge2Mass_I(1+nHybridParticleSort:nParticleSort) =&
         Charge2Mass_I(1+nHybridParticleSort:nParticleSort)* &
         ElectronCharge
  end subroutine normalize_param
  !===================== DEALLOCATE====================
  !Not used and probably not usable
  !subroutine deallocate_particles
  !  integer :: iLoop
  !  !----------------
  !  nullify(Coord_DI)
  !  nullify(Index_II)
  !  !\
  !  ! Deallocate hybrid particle arrays
  !  !/
  !  do iLoop = 1, size(iKindParticle_I)
  !     call gen_deallocate_particles(iKindParticle_I(iLoop))
  !  end do
  !  deallocate(iHybridIon_I, Charge2Mass_I, &
  !          Moments_VGBI, DensityAndCurrent_VCB, &
  !          CAMCoef_VCB, iKindParticle_I)
  !end subroutine deallocate_particles
  !====================================================
  subroutine allocate_particles(Mass_I, Charge_I, nParticleMax_I)
    use BATL_lib, ONLY: jDim_, kDim_
    !\
    ! Redefine MinI:...:MaxK for a single ghost cell layer
    ! We need this to properly apply message pass routines to
    ! the moments array
    !/ 
    integer, parameter:: nG = 1, MinI = 1-nG, MaxI = nI+nG, &
         MinJ = 1-nG*jDim_, MaxJ = nJ+nG*jDim_,             &
         MinK = 1-nG*kDim_, MaxK = nK+nG*kDim_
    real,    intent(in)    :: Mass_I(nParticleSort) 
    real,    intent(in)    :: Charge_I(nParticleSort)
    integer, intent(in)    :: nParticleMax_I(nParticleSort)
    integer :: iLoop
    logical :: DoTest
    character(len=*), parameter:: NameSub = 'allocate_charged_particles'
    !-----------------------------!
    call test_start(NameSub, DoTest)

    if(.not.DoInit) RETURN
    DoInit = .false.

    !Allocate enough arrays for all Charged Particle Types
    allocate(Charge2Mass_I(nParticleSort))

    Charge2Mass_I = Charge_I/Mass_I
    !\
    ! The newly introduced charge-to-mass ratios for particles should be
    !  normalized.
    !/
    DoNormalize = .true.

    iKindParticle_I = -1
    do iLoop = 1, nParticleSort 
       call gen_allocate_particles(&
            iKindParticle = iKindParticle_I(iLoop), &
            nVar          = nVar     , &
            nIndex        = Status_  , &
            nParticleMax  = nParticleMax_I(iLoop)    )
    end do
    allocate(Moments_VGBI(Rho_:P23_,&
         MinI:MaxI,  MinJ:MaxJ, MinK:MaxK, MaxBlock, &
         iKindParticle_I(1):iKindParticle_I(nParticleSort)))
    allocate(DensityAndCurrent_VCB(RhoC_:Jz_,&
            1:nI,  1:nJ, 1:nK, MaxBlock))
    allocate(CAMCoef_VCB(Lambda_:GammaZ_,&
            1:nI,  1:nJ, 1:nK, MaxBlock))
    call test_stop(NameSub, DoTest)
  end subroutine allocate_particles
  !===============================================!
  subroutine trace_particles(Dt, DoBorisStepIn)
    use ModMain,    ONLY: nI, nJ, nK 
    use BATL_particles, ONLY: &
         batl_trace_particles=>trace_particles
    use BATL_pass_face_field, ONLY: add_ghost_cell_field
    real,    intent(in) :: Dt            !Time step
    logical, intent(in) :: DoBorisStepIn !If velocity is advanced or not

    integer :: iLoop, nParticle, iBlock, i, j, k
    real, dimension(3) :: FullB_D, u_D
    !----------------------
    DoBorisStep = DoBorisStepIn

    Moments_VGBI = 0.0            !Prepare storage for the VDF moments
    DensityAndCurrent_VCB   = 0.0 !Same for charge and current densities 
    CAMCoef_VCB       = 0.0       !Same for \Lambda and \Gamma coeffs 

    do iLoop = 1, nParticleSort
       iKind = iKindParticle_I(iLoop)
       call set_pointer_to_particles(&
            iKind, Coord_DI, Index_II, nParticle=nParticle)
       !\
       ! (Dt/2)*Zq/(Am_p), to convert field to force
       !/
       QDtPerM = cHalf*Dt*Charge2Mass_I(iLoop)
       Index_II(Status_,1:nParticle) = DoAll_

       call batl_trace_particles(iKind, boris_scheme, check_done)
       !\
       ! For particles near the block boundary, contributions 
       ! may be assigned to ghost cells 
       call add_ghost_cell_field(P23_, 1, &
            Moments_VGBI(:,:,:,:,:,iKind))
    end do
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE
       do iLoop = 1, nHybridParticleSort
          iKind = iKindParticle_I(iLoop)
          !\
          ! Sum up contributions to charge and current densities 
          ! from  the moments of hybrid ions (first  
          ! nHybridParticleSort of all charged particles)
          !/
          DensityAndCurrent_VCB(RhoC_:Jz_,:,:,:,iBlock) = &
               DensityAndCurrent_VCB(RhoC_:Jz_,:,:,:,iBlock) + &
               Charge2Mass_I(iLoop)* &
               Moments_VGBI(Rho_:RhoUz_,1:nI,1:nJ,1:nK,iBlock,iKind)
          if(.not.DoBorisStep)&
               !\
               ! The mover with DoBorisStep =.false. is called in the
               ! beginning of the time step. After this step, in addition
               ! to the ion current and charge density, one needs to collect
               ! \Lambda and \Gamma coefficients as described in 
               ! Eq. (24a) and (24b) of the CAM algorithm in the paper
               ! Matthews, 1993, J. Comput. Phys, v. 112, pp. 102-116.
               ! These coefficients are used to advance the current 
               ! using Eq. (23) 
               !/
               CAMcoef_VCB(Lambda_:GammaZ_,:,:,:,iBlock) = &
               CAMcoef_VCB(Lambda_:GammaZ_,:,:,:,iBlock) + &
               Charge2Mass_I(iLoop)**2 * &
               Moments_VGBI(Rho_:RhoUz_,1:nI,1:nJ,1:nK,iBlock,iKind)
       end do
    end do
  end subroutine trace_particles
  !=====================================
  subroutine boris_scheme(iParticle, EndOfSegment)
    !\
    ! In this routine we follow the formulation described in the  
    ! Book: Plasma Physics via Computer Simulation, 
    ! Editors: Birdsall, C. K.; Langdon, A. B., 
    ! The Adam Hilger Series on Plasma Physics, 1991.
    ! A time-centered difference method for solving the Newton-Lorentz 
    ! equations of motion is detailed in Subestion 4.3, Eq. (3). 
    ! This method makes use of A) the Buneman 1967 method, through 
    ! Eq. (4)-(6), in order to simplify Eq. (3) by subtracting the 
    ! drift velocity from v, and B) the Boris Scheme, which separates 
    ! the electric and magnetic fields ,  through Eq. (7) - (9).
    !/
    !\
    ! WITHIN THE FRAMEWORK OF HYBRYD SCHEME:
    ! At the beginning of the Boris Scheme the 
    ! known vector quantities are: x(N+1/2), u(N).
    ! At the end of the time-step trac_particles has:
    ! 1. Advanced the velocity and location vectors: u(N+1), x(N+1)
    !    The displacement advancement takes place in two Steps. 
    !    a. First by a half time step to x(N+1) and then 
    ! 2. Uses the E, B-fields: E(N+1/2), B(N+1/2), interpolated to
    !    the particle location at x(N+1/2).
    !    This algorithm corresponds to Step 2(a) (p.109) of the CAM 
    !    Algorithm in Matthews, 1993, J.Comput.Phys, v.112, pp.102-116.
    ! 3. Collected the current and charge densities:
    !     a. \rho_c(x(N+1),u(N+1)), J(x(N+1),u(N+1))
    ! To make the full Boris step the routine is called with 
    ! DoBorisStep = .true.
    !/
    !\
    ! 4. At the beginning of the (next) time step only the particle
    !     coordinates are advanced from x(N+1) to x(N+3/2). We then  
    !     collect 
    !     b. \rho_c(x(N+3/2),u(N+1)), J(x(N+3/2),u(N+1))
    ! To do this, the routine is called with DoBorisStep = .false.
    !/
    ! This algorithm corresponds to Step 2(b) of the CAM Algorithm in
    ! Matthews, 1993, J. Comput. Phys, v. 112, pp. 102-116
    ! with a modification to split the displacement into two parts, 
    ! following Holmstrom, arXiv:0911.4435, 2009. 
    ! They are collected for each species separately, as described 
    ! on page 110, item 2(b)(i)
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
    real   :: Mass, Moments_V(Rho_:P23_)
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
       if(DoBorisStep)then
          !\
          ! Interpolate particle coordinates into BATSRUS grid
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
          ! get potential part of the magnetic field at given location 
          if(UseB0)call get_b0(Xyz_D, B_D)
          !\
          ! Interpolate electric field and the remaining non-potential 
          ! part  of the magnetic field
          !/ 
          do iCell = 1, nCell
             i_D = 1
             i_D(1:nDim) = iCell_II(1:nDim, iCell)
             B_D = B_D + Weight_I(iCell)*&
                  State_VGB(Bx_:Bz_,i_D(1),i_D(2),i_D(3),iBlock)
             E_D = E_D + Weight_I(iCell)*&
                  Efield_DGB(:,i_D(1),i_D(2),i_D(3),iBlock)
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
          ! Save updated velocity (tree components
          !/
          Coord_DI(Ux_:Uz_,iParticle) = U_D
       end if    !End of Boris step advancing the velocity
       !\
       ! Advance coordinates through a half step
       !/
       Coord_DI(x_:nDim, iParticle) = Coord_DI(x_:nDim, iParticle) + &
            0.5*Dt*Coord_DI(Ux_:U_+nDim,iParticle)
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
       end if !Particle stays at the same PE
    end if    !All operations preceeding the current collection are done
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
    !Zero moment
    Moments_V(Rho_)          =  Coord_DI(Mass_, iParticle)
    !First moments
    U_D = Coord_DI(Ux_:Uz_,iParticle)
    Moments_V(RhoUx_:RhoUz_) = Moments_V(Rho_)*U_D(x_:z_)
    !Second moments: diagonal
    Moments_V(Px_:Pz_)   =  Moments_V(Rho_)*U_D(x_:z_)**2
    !Second moments: off-diagonal
    Moments_V(P12_:P13_) =  Moments_V(Rho_)*U_D(x_)*U_D(y_:z_)
    Moments_V(P23_)      =  Moments_V(Rho_)*U_D(y_)*U_D(z_)
    !\
    ! Collect Contribution with updated weight coefficients
    ! Note, that the last index of array is not iLoop (since it
    ! is not a global variable), but iKind
    !/
    do iCell = 1, nCell
       i_D = 1
       i_D(1:nDim) = iCell_II(1:nDim, iCell)
       Moments_VGBI(:,i_D(1),i_D(2),i_D(3),iBlock,iKind) = &
            Moments_VGBI(:,i_D(1),i_D(2),i_D(3),iBlock,iKind) + &
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
    Done = &
         all(Index_II(Status_,1:Particle_I(iKind)%nParticle)==Done_)
  end subroutine check_done
  !==========================
  subroutine get_state_from_vdf_block(iBlock)
    integer, intent(in) :: iBlock

    logical :: DoTest, DoTestCell
    integer :: i, j, k, iIon, iKind, iLoop
    real :: vInv
    character(len=*), parameter:: NameSub = &
         'get_state_from_vdf_block'
    !--------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    vInv = 0.0
    !\
    ! Transform VDF moments to State Vector Variables
    !/
    do k=1,nK; do j=1,nJ; do i=1,nI
       ! Skip not true cells 
       if(.not.true_cell(i,j,k,iBlock)) CYCLE
       
       DoTestCell = DoTest .and. i==iTest .and. j==jTest .and. k==kTest
       
       !\
       ! From VDF moments update fluid components for each hybrid fluid
       ! For the update we use the moments calculated at the  time-step 
       ! when velocity and displacement are synchronized (x(N+1), U(N+1)).
       !/
       vInv = 1.0/CellVolume_GB(i,j,k,iBlock)
       do iLoop = 1, nHybridParticleSort
          iKind = iKindParticle_I(iLoop); iIon = iHybridIon_I(iLoop)
          !\
          ! Density of hybrid fluid iIon : Mass / Control Volume
          !/
          State_VGB(iRhoIon_I(iIon),i,j,k,iBlock) = &
                  State_VGB(iRhoIon_I(iIon),i,j,k,iBlock) + &
               Moments_VGBI(Rho_,i,j,k,iBlock,iKind)*vInv
          !\
          ! Momentum density of hybrid fluid iIon : Momentum / Control Volume
          !/
          State_VGB(iRhoUxIon_I(iIon):iRhoUzIon_I(iIon),i,j,k,iBlock) = &
                  State_VGB(iRhoUxIon_I(iIon):iRhoUzIon_I(iIon),i,j,k,iBlock) + &
               Moments_VGBI(RhoUx_:RhoUz_,i,j,k,iBlock,iKind)*vInv
          !\
          ! Pressure of hybrid fluid iIon : P = Trace(P_tensor) / 3 
          ! Mass * (Ux^2+Uy^2+Uz^2) / Control Volume / 3
          !/
          State_VGB(iPIon_I(iIon),i,j,k,iBlock) = &
                  State_VGB(iPIon_I(iIon),i,j,k,iBlock) + &
               (  Moments_VGBI(Px_,i,j,k,iBlock,iKind)  &
               +  Moments_VGBI(Py_,i,j,k,iBlock,iKind)  &
               +  Moments_VGBI(Pz_,i,j,k,iBlock,iKind))*vInv/3.0
       end do
    end do; end do; end do
    call test_stop(NameSub, DoTest)
  end subroutine get_state_from_vdf_block
  !==========================
  subroutine get_state_from_vdf
    integer :: iBlock
    logical :: DoTest
    character(len=*), parameter:: NameSub = &
         'get_state_from_vdf'
    !--------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE
       call get_state_from_vdf_block(iBlock)
    end do
    call test_stop(NameSub, DoTest)
    
  end subroutine get_state_from_vdf
  !==========================
  !\
  ! In this subroutine we obtain the VDF for each particle species
  ! starting from the state vector variables.
  ! More specifically, we calculate the thermal velocity uThermal_I
  ! 
  !/
  subroutine get_vdf_from_state(iBlock, DoOnScratch)
    use ModMpi
    use ModRandomNumber, ONLY: random_real
    use ModBatlInterface, ONLY: interpolate_grid_amr_gc
    use BATL_lib, ONLY: iNode_B, IsCartesianGrid
!    use BATL_pass_face_field, ONLY: add_ghost_cell_field

    integer, intent(in) :: iBlock
    !If the distribution function is initialized for the
    !first time, one can miss the check, if there are already
    !some particles in the block to generate the VDF.
    logical, optional, intent(in):: DoOnScratch
    !\
    ! seed for random number generator
    integer:: iSeed
    !/
    !Total number of particles of the current sort
    integer :: nParticle, nParticleMax
    integer :: nPPerCell !Number of particles per cell
    !Particle mass
    real :: Mass
    !Sorts of ions and particles
    integer :: iIon, iKind 
    ! loop variables
    integer:: iLoop, iParticle, iCell, i, j, k, iDim 
    !Cartesian and generalized coordinates
    real :: Xyz_D(MaxDim), Coord_D(MaxDim)
    !\
    !Loop indexes gathered to an array
    !/
    integer:: Ijk_D(MaxDim) ![i,j,k] 
    !\
    !The cell index for interpolation stencil point
    !/`
    integer:: i_D(MaxDim)   
    !\
    ! Parameters of the interpolation procedure
    !/
    integer:: nCell, iCell_II(0:nDim, 2**nDim)
    real   :: Weight_I(2**nDim)
    !\
    ! Interpolated MHD parameters at the particle location
    !/
    real :: P, Rho, RhoU_D(MaxDim), InvRho, uThermal2
    !Random numbers
    real :: RndUnif, RndUnif1, RndUnif2
    !Parameters of the Maxwellian distribution
    real :: Energy, MomentumAvr
    logical :: DoTest
    character(len=*), parameter:: NameSub = &
     'get_vdf_from_state'
    !--------------------------------------------------------------------
    call test_start(NameSub, DoTest) 
    !\
    ! Transform State Vector Variables to VDFs
    !/
    !\
    ! Initialize random number generator with the global block
    ! number, so that initializetion does not depend on how the blocks
    ! are distributed over processors
    !/
    iSeed = iNode_B(iBlock); Coord_D = 0.0; Xyz_D = 0.0

    SORTS:do iLoop = 1, nHybridParticleSort
       iKind = iKindParticle_I(iLoop); iIon = iHybridIon_I(iLoop) 
       call set_pointer_to_particles(&
         iKind, Coord_DI, Index_II, &
         nParticle=nParticle, nParticleMax=nParticleMax)
       !\
       ! Number of particles per Block = nHybridParticlePerCell_I
       !/
       nPPerCell = nHybridParticlePerCell_I(iLoop)
       if(.not.DoOnScratch.and.nParticle>0) then 
          !\
          !The particles already present in the block need to be 
          !removed, not to contribute to the VDF moments
          !/
          where(Index_II(0,1:nParticle)==iBlock)&
               Index_II(0,1:nParticle) = -Index_II(0,1:nParticle)
       end if
       !\
       ! Loop over physical cells:
       !/
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          ! Skip not true cells 
          if(.not.true_cell(i,j,k,iBlock)) CYCLE
         !\
          ! Loop over particles to scatter within a given cell
          !/
          if(nParticle + nPPerCell > nParticleMax)&
               call stop_mpi('Insufficient number of particles is allocated')
          Ijk_D = [i,j,k] 
          !\
          ! Mass of particles
          !/
          Mass = &
               State_VGB(iRhoIon_I(iIon),i,j,k,iBlock)*&
               CellVolume_GB(i,j,k,iBlock)/nPPerCell

          PARTICLES:do iParticle = nParticle + 1, nParticle + nPPerCell
             !\
             ! Assign indexes
             !/
             Index_II(0:Status_, iParticle)   = [iBlock,Done_]
             Coord_DI(Mass_,iParticle)    = Mass
             !\
             ! Use random generator to assign coordinates for particles in 
             ! each block.
             !/
             do iDim = 1, nDim
                RndUnif = random_real(iSeed)
                Coord_D(iDim) = CellSize_DB(iDim,iBlock)*&
                     (Ijk_D(iDim) - 1 + RndUnif) + CoordMin_DB(iDim,iBlock)
             end do
             if(IsCartesianGrid)then
                Xyz_D = Coord_D
             else
                call coord_to_xyz(Coord_D, Xyz_D)
             end if
             Coord_DI(x_:nDim,iParticle) = Xyz_D(x_:nDim)
             !\
             ! If the thermal velocity is a positive value, "thermalize"
             ! i.e. generate a Gaussian distribution from the state vector
             ! variable and obtain the VDFs and velocity coordinates.
             !/
             call interpolate_grid_amr_gc(&
               Xyz_D, iBlock, nCell, iCell_II, Weight_I)

             Rho = 0.0; P = 0.0; RhoU_D = 0.0 

             do iCell = 1, nCell
                i_D = 1
                i_D(1:nDim) = iCell_II(1:nDim, iCell)
                !\
                ! Interpolate State_VGB with obtained weight coefficients
                !/
                Rho = Rho + Weight_I(iCell)*&
                  State_VGB(iRhoIon_I(iIon),i_D(1),i_D(2),i_D(3),iBlock)
                P = P + Weight_I(iCell)*&
                  State_VGB(iPIon_I(iIon),i_D(1),i_D(2),i_D(3),iBlock)
                RhoU_D = RhoU_D + Weight_I(iCell)*&
                  State_VGB(iRhoUxIon_I(iIon):iRhoUzIon_I(iIon),&
                  i_D(1),i_D(2),i_D(3),iBlock) 
             end do
             !\
             ! Calculate 1.0 / Rho once to reduce computational time
             !/
             InvRho = 1.0/Rho
             !\
             ! The total velocity of each macroparticle will be
             ! \vec{v_total} = \vec{uBulk} + uThermal * \vec{unit vector}
             ! The bulk velocity can be calculated as the ratio of the 
             ! momentum to density, i.e. ubulk = rho * u / rho
             !/
             Coord_DI(Ux_:Uz_,iParticle)=  RhoU_D*InvRho
             !\
             ! The thermal velocity in the normalization used here is:
             ! uThermal = sqrt(P_I/Rho_I)
             !/
             uThermal2 = P*InvRho 
             !\
             ! If the thermal velocity is zero then set velocity three vector 
             ! to zero
             !/
             if(uThermal2==0.0)CYCLE
             do iDim = 1, MaxDim
                RndUnif1  = random_real(iSeed)
                RndUnif2  = random_real(iSeed)
                ! The kinetic energy is calculated next for each particle
                Energy    = -uThermal2 *log(RndUnif1)
                ! The average momentum is calculated next for each particle
                MomentumAvr = sqrt(2.0*Energy)
                ! The random velocity  is added
                Coord_DI(U_+iDim,iParticle) = Coord_DI(U_+iDim,iParticle) +&
                     MomentumAvr*cos(cTwoPi*RndUnif2)
             end do
             !\
             ! The thing to do: add a parallel component of random velocity
             !/
          end do PARTICLES
          nParticle = nParticle + nPPerCell
       end do; end do; end do
       !\
       ! Collect particles of Sort per Block
       !/
       Particle_I(iKind)%nParticle = nParticle
    end do SORTS
    call test_stop(NameSub, DoTest)
  end subroutine get_vdf_from_state
!====================================
end module ModParticleMover
