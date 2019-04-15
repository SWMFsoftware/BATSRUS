!  Copyright (C) 2002 Regents of theUniversity of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModParticleMover
  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest
  use BATL_lib, ONLY: nDim,  MaxDim, nBlock, MaxBlock, iProc, iComm,&
       nI, nJ, nK, jDim_, kDim_, Unused_B
  use BATL_lib, ONLY: CellVolume_GB

  use ModMain,         ONLY: NameThisComp
  use ModAdvance,      ONLY: UseEfield, Efield_DGB, State_VGB
  use ModVarIndexes,   ONLY: Bx_, Bz_
  use ModMultiFluid,   ONLY: IonFirst_, iRho, iUx, iUz, iUx_I, iUz_I, &
          MassFluid_I, iRhoIon_I, iPIon_I, iUxIon_I, iUyIon_I, iUzIon_I 
  use ModParticles,    ONLY: gen_allocate_particles=>allocate_particles, &
       gen_deallocate_particles=>deallocate_particles
  use BATL_particles,  ONLY: Particle_I, &
       mark_undefined, check_particle_location, &
       set_pointer_to_particles
  use ModGeometry, ONLY: true_cell
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
  ! number of hybrid particles w/ different (predefined) charge-to-mass ratios
  integer :: nHybridParticleSort = 0 
  !\
  ! number of test particles with different charge-to-mass ratios
  integer :: nTestParticles= 0 
  !\
  ! The order number of this kind of particle in the whole 
  ! BATL_particle repository
  !/
  integer, allocatable :: iKindHybridParticle_I(:)
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
       RhoUy_ = RhoU_ + y_, RhoUz_ = RhoU_ + z_, P_ = RhoUz_, &
       Px_ = P_ + x_, Py_ = P_ + y_, Pz_ = P_ + z_, &
       P12_ = Pz_ + 1, P13_ = Pz_ + 2, P23_  = Pz_ + 3 
  !\
  ! Moments of the particle VDFs
  !/
  real,    allocatable :: MomentsMinus_DGBI(:,:,:,:,:,:)
  real,    allocatable :: MomentsPlus_DGBI( :,:,:,:,:,:)
  !\
  ! Charge and current densities
  !/
  real,    allocatable :: DensityMinus_VCB(:,:,:,:,:)
  real,    allocatable :: DensityPlus_VCB( :,:,:,:,:)
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
  ! 2                      HybridFluid_I(1) 
  ! 1000000                nHybridParticleMax_I(1)
  ! 3                      HybridFluid_I(2) 
  ! 50000                  nHybridParticleMax_I(2)
  ! 4                      HybridFluid_I(3) 
  ! 150000                 nHybridParticleMax_I(3)
  ! 1                      nTestParticles
  ! 4.0                    Mass_I
  ! 2.0                    Charge_I
  ! and allocate particle arrays immediately.
  ! To reallocate, first, use command
  ! #CHARGEDPARTICLES
  ! 0                      nHybridParticleSort
  ! to deallocate arrays.
  !--------------------------------------------------------------------------
  subroutine read_param(NameCommand)

    use ModReadParam, ONLY: read_var

    character(len=*), intent(in) :: NameCommand

    logical:: DoTest
    integer :: iLoop, nKindParticleSort
    !\
    !Arrays to read A and Z for different ion particles
    !/
    real, allocatable :: Mass_I(:), MassHybrid_I(:)
    real, allocatable :: Charge_I(:), ChargeHybrid_I(:)
    !\
    ! array to read maximum number of particles for different
    ! sorts of ions
    !/
    integer, allocatable :: HybridFluid_I(:)
    integer, allocatable :: nParticleMax_I(:), nHybridParticleMax_I(:)
    character(len=*), parameter:: NameSub = NameMod//'read_param'
    !--------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(NameCommand)

    case("#CHARGEDPARTICLES")
       call read_var('nHybridParticleSort', nHybridParticleSort)
       if(nHybridParticleSort<= 0) then
          !\
          ! Clean particle arrays
          !/
          if(UseParticles)call deallocate_particles
          UseParticles = .false.
          RETURN
       end if
       !\
       ! The particle arrays can be reset, however,
       ! first nKindHybridParticle should be set to zero
       ! to deallocate available arrays
       !/
       if(UseParticles)call stop_mpi(NameThisComp//':'//NameSub//&
               ': To reset particle arrays first use '//&
               '#CHARGEDPARTICLES command with nHybridParticleSort=0')
       UseParticles = .true.
       allocate(HybridFluid_I(nHybridParticleSort))
       allocate(nHybridParticleMax_I(nHybridParticleSort))
       allocate(MassHybrid_I(nHybridParticleSort))
       allocate(ChargeHybrid_I(nHybridParticleSort))
       do iLoop = 1, nHybridParticleSort 
          call read_var('HybridFluid_I', HybridFluid_I(iLoop))
          if(HybridFluid_I(iLoop)<= 0) call stop_mpi(&
               NameThisComp//':'//NameSub//&
               ': invalid type of ion kind')
          call read_var('nHybridParticleMax_I', nHybridParticleMax_I(iLoop))
          if(nHybridParticleMax_I(iLoop)<= 0) call stop_mpi(&
               NameThisComp//':'//NameSub//&
               ': invalid number of charged particles for ikind')
          HybridFluid_I = HybridFluid_I + IonFirst_ - 1
          Mass_I(1:nHybridParticleSort) = MassFluid_I(HybridFluid_I)
       end do
       !\
       ! The particle arrays can be reset, however,
       ! first nTestParticles should be set to zero
       ! to deallocate available arrays
       !/
       call read_var('nTestParticles', nTestParticles)
       if(nTestParticles<= 0) then
          !\
          ! Clean particle arrays
          !/
          if(UseParticles)call deallocate_particles
          UseParticles = .false.
          RETURN
       end if
       !\
       ! The particle arrays can be reset, however,
       allocate(nParticleMax_I(nTestParticles))
       allocate(  Mass_I(nTestParticles))
       allocate(Charge_I(nTestParticles))
       do iLoop = 1, nTestParticles
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
    call allocate_particles(MassHybrid_I, ChargeHybrid_I, nHybridParticleMax_I)
    deallocate(MassHybrid_I, ChargeHybrid_I, nHybridParticleMax_I)
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
    do iLoop = 1, size(iKindHybridParticle_I)
       call gen_deallocate_particles(iKindHybridParticle_I(iLoop))
    end do
    deallocate(iKindHybridParticle_I, Charge2Mass_I, MomentsMinus_DGBI, &
            MomentsPlus_DGBI, DensityMinus_VCB, DensityPlus_VCB, &
            CAMCoef_VCB)
  end subroutine deallocate_particles
  !====================================================
  subroutine allocate_particles(Mass_I, Charge_I, nParticleMax_I) 
    !\
    ! Redefine MinI:...:MaxK for a single ghost cell layer
    !/ 
    integer, parameter:: nG = 1, MinI = 1-nG, MaxI = nI+nG, &
         MinJ = 1-nG*jDim_, MaxJ = nJ+nG*jDim_,             &
         MinK = 1-nG*kDim_, MaxK = nK+nG*kDim_
    real,    intent(in)    :: Mass_I(nHybridParticleSort) 
    real,    intent(in)    :: Charge_I(nHybridParticleSort)
    integer, intent(in)    :: nParticleMax_I(nHybridParticleSort)
    integer :: iKind
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'allocate_charged_particles'
    !-----------------------------!
    call test_start(NameSub, DoTest)

    if(.not.DoInit) RETURN
    DoInit = .false.

    !Allocate enough arrays for all Charged Particle Types
    allocate(iKindHybridParticle_I(nHybridParticleSort)) 
    allocate(Charge2Mass_I(nHybridParticleSort))

    Charge2Mass_I = Charge_I/Mass_I
    
    iKindHybridParticle_I = -1
    do iKind = 1, nHybridParticleSort 
       call gen_allocate_particles(&
            iKindParticle = iKindHybridParticle_I(iKind), &
            nVar          = nVar     , &
            nIndex        = Status_  , &
            nParticleMax  = nParticleMax_I(iKind)    )
    end do
    allocate(MomentsMinus_DGBI(Rho_:P23_,&
         MinI:MaxI,  MinJ:MaxJ, MinK:MaxK, MaxBlock, &
         nHybridParticleSort))
    allocate(MomentsPlus_DGBI(Rho_:P23_,&
         MinI:MaxI,  MinJ:MaxJ, MinK:MaxK, MaxBlock, &
         nHybridParticleSort))
    allocate(DensityPlus_VCB(RhoC_:Jz_,&
            1:nI,  1:nJ, 1:nK, MaxBlock))
    allocate(DensityMinus_VCB(RhoC_:Jz_,&
            1:nI,  1:nJ, 1:nK, MaxBlock))
    allocate(CAMCoef_VCB(Lambda_:GammaZ_,&
            1:nI,  1:nJ, 1:nK, MaxBlock))

    call test_stop(NameSub, DoTest)
  end subroutine allocate_particles
  !===============================================!
  subroutine trace_particles(DtIn)
    use ModMain,    ONLY: nI, nJ, nK 
    use BATL_particles, ONLY: &
         batl_trace_particles=>trace_particles
    use BATL_pass_face_field, ONLY: add_ghost_cell_field
    real, intent(in) :: DtIn

    logical :: DoTest, DoTestCell
    integer :: iLoop, nParticle, iBlock
    integer :: i, j, k, iIon, jIon, iRhoUx, iRhoUz, iP, iEnergy
    real :: InvElectronDens
    real :: State_V(nVar)
    real, dimension(3) :: FullB_D, uIon_D, uIon2_D, u_D, uPlus_D
    real, dimension(nHybridParticleSort) :: &
     NumDens_I, ChargeDens_I, Rho_I, InvRho_I
    !----------------------
    Dt = DtIn
    MomentsMinus_DGBI = 0.0 !Prepare storage for the VDF moments
    MomentsPlus_DGBI  = 0.0 !Prepare storage for the VDF moments
    DensityMinus_VCB  = 0.0 !Same for the charge and current densities 
    DensityPlus_VCB   = 0.0 !Same for the charge and current densities 
    CAMCoef_VCB       = 0.0 !Same for the \Lambda and \Gamma coefficients 

    DoTestCell = .false.

    do iLoop = 1, nHybridParticleSort
       iKind = iKindHybridParticle_I(iLoop)
       call set_pointer_to_particles(&
            iKind, Coord_DI, Index_II, &
            nParticle=nParticle)
       !\
       ! (Dt/2)*Zq/(Am_p), to convert field to force
       !/
       QDtPerM = cHalf * Dt * Charge2Mass_I(iKind)
       Index_II(Status_,1:nParticle) = DoAll_
       call batl_trace_particles(iKind, boris_scheme, check_done)
       !\
       ! For particles near the block boundary, contributions are
       ! may be assigned to ghost cells 
       call add_ghost_cell_field(RhoUz_, 1, &
            MomentsMinus_DGBI(:,:,:,:,:,iKind))
       call add_ghost_cell_field(RhoUz_, 1, &
            MomentsPlus_DGBI( :,:,:,:,:,iKind))
    end do
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE
       !\
       ! Moments Minus (for x(N+1), U(N+1) ) arrays to get
       ! moments at mid point - see Step 3 in Matthews 
       ! algorithm on page 109. 
       !/
       do iLoop = 1, nHybridParticleSort
          iKind = iKindHybridParticle_I(iLoop)
          !\
          ! Sum up contributions to charge and current densities from 
          ! the moments
          !/
          DensityMinus_VCB(RhoC_:Jz_,:,:,:,iBlock) = &
               DensityMinus_VCB(RhoC_:Jz_,:,:,:,iBlock) + &
               Charge2Mass_I(iKind) * &
               MomentsMinus_DGBI(Rho_:RhoUz_,1:nI,1:nJ,1:nK,iBlock,iKind)
          ! Moments Plus (for x(N+3/2), U(N+1) ) 
          DensityPlus_VCB( RhoC_:Jz_,:,:,:,iBlock) = &
               DensityPlus_VCB( RhoC_:Jz_,:,:,:,iBlock) + &
               Charge2Mass_I(iKind) * &
               MomentsPlus_DGBI( Rho_:RhoUz_,1:nI,1:nJ,1:nK,iBlock,iKind)
          !\
          ! \Lambda and \Gamma coefficients as described in 
          ! Eq. (24a) and (24b) of the CAM algorithm in the paper
          ! Matthews, 1993, J. Comput. Phys, v. 112, pp. 102-116.
          ! These coefficients are used to advance the current using Eq. (23) 
          !/
          CAMcoef_VCB(Lambda_:GammaZ_,:,:,:,iBlock) = &
               CAMcoef_VCB(Lambda_:GammaZ_,:,:,:,iBlock) + &
               Charge2Mass_I(iKind)**2 * &
               MomentsPlus_DGBI(Rho_:RhoUz_,1:nI,1:nJ,1:nK,iBlock,iKind)
       end do
       !\
       ! Transform VDF moments to State Vector Variables
       !/
       do k=1,nK; do j=1,nJ; do i=1,nI
       if(.not.true_cell(i,j,k,iBlock)) CYCLE

       DoTestCell = DoTest .and. i==iTest .and. j==jTest .and. k==kTest

       State_V = State_VGB(:,i,j,k,iBlock)
       ChargeDens_I = Charge2Mass_I*State_V(iRhoIon_I)

       Rho_I    = State_V(iRhoIon_I)
       InvRho_I = 1.0/Rho_I

          do iIon = 1, nHybridParticleSort 
             State_VGB(iRhoIon_I(iIon),i,j,k,iBlock) = &
                     MomentsMinus_DGBI(Rho_,i,j,k,iBlock,iIon)/&
                  CellVolume_GB(i,j,k,iBlock)
             State_VGB(iUxIon_I(iIon):iUzIon_I(iIon),i,j,k,iBlock) = &
                     MomentsMinus_DGBI(RhoUx_:RhoUz_,i,j,k,iBlock,iIon)/&
                  CellVolume_GB(i,j,k,iBlock)*InvRho_I
             State_VGB(iPIon_I(iIon),i,j,k,iBlock) = &
                     (MomentsMinus_DGBI(Px_,i,j,k,iBlock,iIon)  &
                   +  MomentsMinus_DGBI(Py_,i,j,k,iBlock,iIon)  &
                   +  MomentsMinus_DGBI(Pz_,i,j,k,iBlock,iIon))/ &
                  CellVolume_GB(i,j,k,iBlock)/3.0
          end do
       end do; end do; end do
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
    !    The displacement advancement takes place in two Steps. 
    !    a. First by a half time step to x(N+1) and then
    !    b. by another half time step to x(N+3/2), 
    !    following Holmstrom, arXiv:0911.4435, 2009. 
    ! 2. Uses the E, B-fields: E(N+1/2), B(N+1/2), interpolated to
    !    the particle location at x(N+1/2).
    !    This algorithm corresponds to Step 2(a) (p.109)of the CAM Algorithm
    !     in Matthews, 1993, J. Comput. Phys, v. 112, pp. 102-116.
    ! 3. Collected the current and charge densities at two different points:
    !     a. \rho_c(x(N+1),u(N+1)), J(x(N+1),u(N+1)) : MomentsMinus_DGBI
    !     b. \rho_c(x(N+3/2),u(N+1)), J(x(N+3/2),u(N+1)) : MomentsPlus_DGBI
    ! This algorithm corresponds to Step 2(b) of the CAM Algorithm in
    ! Matthews, 1993, J. Comput. Phys, v. 112, pp. 102-116
    ! with a modification to split the displacement into two parts, following
    ! Holmstrom, arXiv:0911.4435, 2009. 
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
       ! Synchronize velocity and displacement by calculating displacement
       ! in mid-point: X(N)=0.5*(X(N-1/2)+X(N+1/2))
       !/
       Coord_DI(x_:nDim, iParticle) = Coord_DI(x_:nDim, iParticle) + &
            0.5*Dt*Coord_DI(Ux_:U_+nDim,iParticle)
       !\
       ! Collect the contribution to moments of VDF, 
       ! from a given particle with coordinates at half time step
       ! before velocity
       !/
       Moments_V(Rho_) = Mass
       Moments_V(RhoUx_:RhoUz_) = Mass*U_D
       Moments_V(Px_:Pz_) = Mass*U_D**2
       Moments_V(P12_) = Mass*U_D(1)*U_D(2)
       Moments_V(P13_) = Mass*U_D(1)*U_D(3)
       Moments_V(P23_) = Mass*U_D(2)*U_D(3)
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
    Moments_V(Px_:Pz_) = Moments_V(Rho_)*&
         Coord_DI(Ux_:Uz_, iParticle)**2
    Moments_V(P12_:P13_) =  Moments_V(Rho_)*&
         Coord_DI(Ux_, iParticle)*Coord_DI(Uy_:Uz_, iParticle)
    Moments_V(P23_) = Moments_V(Rho_)*&
         Coord_DI(Uy_, iParticle)*Coord_DI(Uz_, iParticle)
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
