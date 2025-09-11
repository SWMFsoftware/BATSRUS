!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModParticleFieldLine

  use BATL_lib, ONLY: &
       test_start, test_stop, iProc, nProc, iComm
  use ModBatsrusUtility, ONLY: get_time_string, stop_mpi
#ifdef _OPENACC
  use ModUtilities, ONLY: norm2
#endif

  ! This module contains subroutine for extracting magnetic field lines
  ! for passing to other codes;
  ! field line intergration is performed with use of BATL library including
  ! - continuous AMR interpolation
  ! - particle methods

  use BATL_lib, ONLY: &
       MaxDim, nDim,     &
       CellVolume_GB, &
       Particle_I, trace_particles,                           &
       mark_undefined, check_particle_location, put_particles
  use ModParticles, ONLY: allocate_particles
  use ModBatlInterface, ONLY: interpolate_grid_amr_gc
  use ModAdvance, ONLY: State_VGB
  use ModVarIndexes, ONLY: Rho_, RhoUx_, RhoUz_, Bx_, Bz_
  use ModMain, ONLY: NameThisComp

  implicit none

  SAVE
  private ! except

  public:: read_particle_line_param
  public:: init_particle_line
  public:: extract_particle_line
  public:: advect_particle_line
  public:: get_particle_data
  public:: write_plot_particle

  ! Local variables ----------------------
  ! use particles in the simulation
  logical, public :: UseParticles = .false.
  ! kinds of particles used to generate a magnetic field line
  integer :: iKindEnd = -1, iKindReg = -1

  ! variable in the state vector of a particle
  integer, parameter:: &
       ! coordinates of a particle
       x_    = 1, y_    = 2, z_    = 3, &
       ! storage for particle's position at the beginning of the step
       XOld_ = 4, YOld_ = 5, ZOld_ = 6, &
       ! stepsize during line extraction
       Ds_   = 7, &
       ! previous direction
       DirX_ = 8, DirZ_ = 10

  ! indices of a particle
  integer, parameter:: &
       ! field line this particle lays on
       fl_        = 1, &
       ! index of the particle along this field line
       id_        = 2, &
       ! indicator for message passing in trace_particle_line
       Pass_      = 3, &
       ! alignment of particle numbering with direction of B field:
       ! ( -1 -> reversed, +1 -> aligned); important for radial ordering
       ! when magnetic field general direction may be inward
       Alignment_ = 4

  integer, parameter::    &
       Normal_ = 0       ,&
       HalfStep_ = 1     ,&
       DoneFromScratch_ = 2

  ! data that can be requested and returned to outside this module
  ! ORDER MUST CORRESPOND TO INDICES ABOVE
  integer, parameter:: nVarAvail = 3, nIndexAvail = 2, nDataMax = &
       nVarAvail + nIndexAvail +1
  character(len=2), parameter:: NameVarAvail_V(nVarAvail) = &
       ['xx', 'yy', 'zz']
  character(len=2), parameter:: NameIndexAvail_I(0:nIndexAvail) = &
       ['bk','fl', 'id']

  ! spatial step limits at the field line extraction
  real   :: SpaceStepMin = 0.0
  real   :: SpaceStepMax = HUGE(SpaceStepMin)

  ! number of variables in the state vector
  integer, parameter:: nVarParticleReg = 6
  integer, parameter:: nVarParticleEnd = 10

  ! number of indices
  integer, parameter:: nIndexParticleReg = 3
  integer, parameter:: nIndexParticleEnd = 4

  ! maximum allowed number of field lines
  integer :: nFieldLineMax

  ! approximate number of particles per field line
  integer :: nParticlePerLine

  ! number of active field lines
  integer:: nFieldLine = 0

  ! soft radial boundary that may be set only once during the run;
  ! "soft" means that particles are allowed beyond this boundary BUT:
  ! - during extraction first particle that crosses it is kept but extraction
  !   of this line is stopped
  real:: RSoftBoundary = -1.0

  ! field line random walk (FLRW)
  logical :: UseFLRW = .false.
  real    :: DeltaPhiFLRW = 0.0

  ! initialization related info
  integer :: nLineInit
  real, allocatable:: XyzLineInit_DI(:,:)
  ! Shared variavles
  ! direction of tracing: -1 -> backward, +1 -> forward
  integer :: iDirTrace
  ! Parameters of a tool preventing the magnetic field line from
  ! reconnection and returning to the Sun. The tool limits the angle
  ! bewtween the magnetic field line and radial direction
  ! (if UseBRAlignment = .true.), or between the magnetic field line
  ! and streamline in corotating frame (if UseBUAlignment = .true.),
  ! which in the steady state should be parallel or antiparallel.
  logical :: UseBRAlignment = .false., UseBUAlignment = .false.
  real    :: CosBRAngleMax = 0.10, CosBUAngleMax = 0.85

  ! state and indexes for particles:
  real,    pointer::  StateEnd_VI(:,:), StateReg_VI(:,:)
  integer, pointer:: iIndexEnd_II(:,:),iIndexReg_II(:,:)
  public :: iKindReg, x_,y_, z_, fl_, id_, RSoftBoundary

contains
  !============================================================================
  subroutine read_particle_line_param(NameCommand)

    use ModReadParam, ONLY: read_var
    use ModNumConst, ONLY: cPi

    character(len=*), intent(in) :: NameCommand

    character(len=100) :: StringInitMode
    integer:: iLine, iDim ! loop variables

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_particle_line_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(NameCommand)
    case("#PARTICLELINE")
       call read_var('UseParticles', UseParticles)
       if(.not.UseParticles)then
          call test_stop(NameSub, DoTest)
          RETURN
       end if
       ! read info on size of the arrays to be allocated:
       ! max total number of field lines (on all procs)
       call read_var('nFieldLineMax', nFieldLineMax)
       ! number of particles per field line (average)
       call read_var('nParticlePerLine', nParticlePerLine)
       ! check correctness
       if(nFieldLineMax <= 0) call stop_mpi(&
            NameThisComp//':'//NameSub//&
            ': invalid max number of field lines')
       if(nParticlePerLine <= 0) call stop_mpi(&
            NameThisComp//':'//NameSub//&
            ': invalid number of particles per field lines')
       !--------------------------------------------------------------
       ! read min and max values for space step
       ! based on their values space step may be
       ! - both negative => automatic (defined by grid resolution)
       ! - otherwise     => restricted by min and/or max
       !                    whichever is positive
       call read_var('SpaceStepMin', SpaceStepMin)
       call read_var('SpaceStepMax', SpaceStepMax)
       ! negative value means "undefined"
       if(SpaceStepMin < 0) SpaceStepMin = 0
       if(SpaceStepMax < 0) SpaceStepMax = HUGE(SpaceStepMin)
       !--------------------------------------------------------------
       call read_var('InitMode', StringInitMode, IsLowerCase=.true.)
       ! Initialization modes:
       ! - preset: starting points are set from PARAM.in file
       if(index(StringInitMode, 'preset') > 0)then
          call read_var('nLineInit', nLineInit)
          if(nLineInit <= 0)&
               call stop_mpi(&
               NameThisComp//':'//NameSub // &
               ": invalid number of initialized particle lines")
          allocate(XyzLineInit_DI(MaxDim, nLineInit))
          do iLine = 1, nLineInit; do iDim = 1, MaxDim
             call read_var('XyzLineInit_DI', XyzLineInit_DI(iDim, iLine))
          end do; end do
       elseif(index(StringInitMode, 'import') > 0)then
          ! do nothing: particles are imported from elsewhere
       else
          call stop_mpi(&
               NameThisComp//':'//NameSub //": unknown initialization mode")
       end if
       call read_var('UseBRAlignment', UseBRAlignment)
       if(UseBRAlignment)call read_var('CosBRAngleMax', CosBRAngleMax)
       call read_var('UseBRAlignment', UseBUAlignment)
       if(UseBUAlignment)call read_var('CosBUAngleMax', CosBUAngleMax)
    case("#PARTICLELINERANDOMWALK")
       ! field line random walk may be enabled
       call read_var('UseFLRW', UseFLRW)
       if(UseFLRW)then
          ! read root-mean-square angle of field line diffusion coefficient
          call read_var('DeltaPhiFLRW [degree]', DeltaPhiFLRW)
          DeltaPhiFLRW = DeltaPhiFLRW*cPi/180
       end if
    end select
    call test_stop(NameSub, DoTest)
  end subroutine read_particle_line_param
  !============================================================================

  subroutine init_particle_line
    ! allocate containers for particles
    logical, save:: DoInit = .true.
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_particle_line'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(.not.DoInit) RETURN
    DoInit = .false.
    call allocate_particles(&
         iKindParticle = iKindReg,  &
         nVar   = nVarParticleReg,  &
         nIndex = nIndexParticleReg,&
         nParticleMax = nParticlePerLine * nFieldLineMax)
    call allocate_particles(&
         iKindParticle = iKindEnd  ,&
         nVar   = nVarParticleEnd  ,&
         nIndex = nIndexParticleEnd,&
         nParticleMax = nFieldLineMax)
    ! set pointers to parameters of end particles
    StateEnd_VI  => Particle_I(iKindEnd)%State_VI
    iIndexEnd_II => Particle_I(iKindEnd)%iIndex_II
    StateReg_VI  => Particle_I(iKindReg)%State_VI
    iIndexReg_II => Particle_I(iKindReg)%iIndex_II
    if(nLineInit > 0)&
         ! extract initial field lines
         call extract_particle_line(XyzLineInit_DI)
    call test_stop(NameSub, DoTest)
  end subroutine init_particle_line
  !============================================================================
  subroutine extract_particle_line(Xyz_DI, iTraceModeIn, &
       iIndex_II, UseInputInGenCoord)
    use ModMpi
    integer:: iError
    ! extract magnetic field lines starting at Xyz_DI;
    ! the whole field lines are extracted, i.e. they are traced forward
    ! and backward up until it reaches boundaries of the domain;
    ! requested coordinates may be different for different processor,
    ! if a certain line can't be started on a given processor, it is
    ! ignored, thus duplicates are avoided
    real,            intent(in) :: Xyz_DI(:, :)
    ! mode of tracing (forward, backward or both ways)
    integer,optional,intent(in) :: iTraceModeIn
    ! initial particle indices for starting particles
    integer,optional,intent(in) :: iIndex_II(:,:)

    ! An input can be in generalized coordinates
    logical, optional,intent(in) :: UseInputInGenCoord
    integer :: nLineThisProc! number of new field lines initialized locally
    integer :: nLineAllProc ! number of new field lines initialized on all PEs
    integer :: nParticleOld ! number of already existing regular particles

    ! mode of tracing (see description below)
    integer:: iTraceMode

    ! Loop variable for particles
    integer :: iParticle

    ! Magnetic field direction vector
    real   :: DirB_D(MaxDim)

    ! initialize field lines
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'extract_particle_line'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    call  put_particles(&
         iKindParticle      = iKindEnd,          &
         StateIn_VI         = Xyz_DI,            &
         iLastIdIn          = nFieldLine,        &
         iIndexIn_II        = iIndex_II,         &
         UseInputInGenCoord = UseInputInGenCoord,&
         DoReplace          = .true.,            &
         nParticlePE        = nLineThisProc)

    ! how many lines have been started on all processors
    if(nProc>1)then
       call MPI_Allreduce(nLineThisProc, nLineAllProc, 1, &
            MPI_INTEGER, MPI_SUM, iComm, iError)
    else
       nLineAllProc = nLineThisProc
    end if
    nFieldLine    = nFieldLine + nLineAllProc
    if(nFieldLine > nFieldLineMax) call stop_mpi(NameThisComp//':'//NameSub//&
         ': Limit for number of particle field lines exceeded')

    ! Save number of regular particles
    nParticleOld  = Particle_I(iKindReg)%nParticle

    ! Copy end points to regular points. Note, that new regular points
    ! have numbers nParticleOld+1:nParticleOld+nLineThisProc
    call copy_end_to_regular

    ! Trace field lines
    ! check if trace mode is specified
    if(present(iTraceModeIn))then
       if(abs(iTraceModeIn) > 1)call stop_mpi(&
            NameThisComp//':'//NameSub//': incorrect tracing mode')
       iTraceMode = iTraceModeIn
    else
       iTraceMode = 0
    end if

    ! tracing modes are the following:,
    ! -1 -> trace lines backward only
    !       do iDirTrace = -1, -1, 2
    !  0 -> trace lines in both directions
    !       do iDirTrace = -1,  1, 2
    ! +1 -> trace lines forward only
    !       do iDirTracs =  1,  1, 2
    do iDirTrace = 2*max(iTraceMode,0)-1, 2*min(iTraceMode,0)+1, 2

       ! fix alignment of particle indexing with B field direction
       ! and pass mode
       do iParticle = 1, Particle_I(iKindEnd)%nParticle
          iIndexEnd_II(Pass_, iParticle) = DoneFromScratch_
          call get_b_dir(iParticle, DirB_D)
          iIndexEnd_II(Alignment_, iParticle) = &
               nint( SIGN(1.0, sum(DirB_D*StateEnd_VI(x_:z_, iParticle)) ))
       end do

       call trace_particles(iKindEnd, particle_line)

       ! if just finished backward tracing and need to trace in both dirs
       ! => return to initial particles
       if(iDirTrace < 0 .and. iTraceMode == 0)then
          ! copy initial points to iKindEnd:
          ! the initial particles are currently right after the particles,
          ! that were in the list before the start of this subroutine,
          ! i.e. occupy positions from (nParticleOld+1)
          StateEnd_VI(x_:z_, 1:nLineThisProc) = &
               StateReg_VI(x_:z_, nParticleOld+1:nParticleOld+nLineThisProc)
          iIndexEnd_II(0:id_,1:nLineThisProc) = iIndexReg_II(&
               0:id_,nParticleOld+1:nParticleOld+nLineThisProc)
          Particle_I(iKindEnd)%nParticle = nLineThisProc
       end if
    end do
    ! Offset in id_
    call offset_id(nFieldLine - nLineAllProc + 1, nFieldLine)
    call test_stop(NameSub, DoTest)
  contains
    !==========================================================================
    subroutine offset_id(iLineStart,iLineEnd)
      use ModMpi
      integer, intent(in) :: iLineStart, iLineEnd
      integer:: iParticle, nParticle, iLine, iError
      integer, allocatable:: iOffsetLocal_I(:), iOffset_I(:)
      !------------------------------------------------------------------------
      allocate(iOffsetLocal_I(1:nFieldLineMax))
      allocate(iOffset_I(     1:nFieldLineMax))
      iOffsetLocal_I = 0; iOffset_I = 0

      ! set a pointer to parameters of regular particles
      nParticle = Particle_I(iKindReg)%nParticle
      do iParticle = 1, nParticle
         iLine = iIndexReg_II(fl_,iParticle)

         ! Reset offset, if id_ is less than one
         iOffsetLocal_I(iLine) = max(iOffsetLocal_I(iLine),&
              1 - iIndexReg_II(id_,iParticle))
      end do
      ! find maximim offset from all processors
      if(nProc > 1) then
         call MPI_Allreduce(iOffsetLocal_I(iLineStart), &
              iOffset_I(iLineStart), 1 + iLineEnd - iLineStart, &
              MPI_INTEGER, MPI_MAX, iComm, iError)
      else
         iOffset_I(iLineStart:iLineEnd) = &
              iOffsetLocal_I(iLineStart:iLineEnd)
      end if
      do iParticle = 1, nParticle
         iLine = iIndexReg_II(fl_,iParticle)

         ! Apply offset
         iIndexReg_II(id_,iParticle) = iIndexReg_II(id_,iParticle) + &
              iOffset_I(iLine)
      end do
      deallocate(iOffsetLocal_I, iOffset_I)
    end subroutine offset_id
    !==========================================================================
  end subroutine extract_particle_line
  !============================================================================
  subroutine particle_line(iParticle, IsEndOfSegment)
    ! the subroutine is the tracing loop;
    ! tracing starts at iKindEnd particles and proceeds in a given direction

    integer, intent(in) :: iParticle
    logical, intent(out):: IsEndOfSegment

    ! cosine of angle between direction of field and radial direction

    ! whether to schedule a particle for message pass
    logical:: DoMove, IsGone

    ! direction of the magnetic field
    real :: DirB_D(MaxDim)
    ! direction of the velocity in corotating coordinate system
    real :: DirU_D(MaxDim)
    ! radial direction
    real :: DirR_D(MaxDim)
    ! direction of the tangent to the line: may be parallel or
    ! antiparallel to DirB. The direction may be corrected if needed
    real :: DirLine_D(MaxDim)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'particle_line'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(iIndexEnd_II(Pass_,iParticle))
    case(HalfStep_)

       ! change Pass_ field for particle passed after first stage
       iIndexEnd_II(Pass_,iParticle) = Normal_
       ! will continue tracing, can't be the end of segment
       IsEndOfSegment = .false.
       ! do not call the first stage, proceed to the second one

    case(DoneFromScratch_)

       ! Initialize the radial direction that corresponds
       ! to the previous step
       StateEnd_VI(DirX_:DirZ_,iParticle) = iDirTrace* &
            StateEnd_VI(x_:z_,iParticle) / &
            norm2(StateEnd_VI(x_:z_, iParticle))
       iIndexEnd_II(Pass_, iParticle) = Normal_

       call stage1

    case(Normal_)

       ! increase particle index & copy to regular
       iIndexEnd_II(id_,iParticle) = iIndexEnd_II(id_,iParticle) + iDirTrace
       call copy_end_to_regular(iParticle)

       call stage1

    case default
       call stop_mpi(NameThisComp//':'//NameSub//': Unknown stage ID')
    end select

    if(IsEndOfSegment) RETURN
    call stage2
    call test_stop(NameSub, DoTest)
 contains
    !==========================================================================
    subroutine stage1
      !------------------------------------------------------------------------
      if(RSoftBoundary > 0.0)then
         if(norm2(StateEnd_VI(1:nDim,iParticle)) &
              > RSoftBoundary)then
            call mark_undefined(iKindEnd, iParticle)
            IsEndOfSegment = .true.
            RETURN
         end if
      end if
      ! First stage of RK2 method
      ! copy last known coordinates to old coords
      StateEnd_VI(XOld_:ZOld_,iParticle) = StateEnd_VI(x_:z_, iParticle)
      ! get the direction of the magnetic field at original location
      ! get and save gradient of cosine of angle between field and
      ! radial direction
      ! get and save step size
      call get_b_dir(iParticle, DirB_D         ,&
           DirU_D  = DirU_D                    ,&
           StepSize= StateEnd_VI(Ds_,iParticle),&
           IsBody  = IsEndOfSegment)
      ! particle's location may be inside central body
      if(IsEndOfSegment)RETURN

      ! Limit the interpolated time step
      StateEnd_VI(Ds_, iParticle) = &
           MIN(SpaceStepMax, MAX(SpaceStepMin, StateEnd_VI(Ds_,iParticle)))

      ! get line direction
      DirLine_D = DirB_D*iDirTrace*iIndexEnd_II(Alignment_,iParticle)
      ! get radial direction
      DirR_D    = StateEnd_VI(x_:z_,iParticle) / &
           norm2( StateEnd_VI(x_:z_,iParticle))
      ! correct the direction to prevent the line from closing
      if(UseBUAlignment) &
           call correct(DirLine_D, iDirTrace*DirU_D, CosBUAngleMax)
      if(UseBRAlignment) &
           call correct(DirLine_D, iDirTrace*DirR_D, CosBRAngleMax)

      ! get middle location
      StateEnd_VI(x_:z_,iParticle) = StateEnd_VI(x_:z_,iParticle) + &
           0.50*StateEnd_VI(Ds_,iParticle)*DirLine_D

      ! check location, schedule for message pass, if needed
      call check_particle_location(  &
           iKindParticle = iKindEnd ,&
           iParticle     = iParticle,&
           DoMove        = DoMove   ,&
           IsGone        = IsGone    )

      ! Particle may come beyond the boundary of computational
      ! domain. Otherwise, it may need to be marked as passed
      ! the first stage and be moved to different PE
      IsEndOfSegment = IsGone.or.DoMove
      if(DoMove) iIndexEnd_II(Pass_, iParticle) = HalfStep_
    end subroutine stage1
    !==========================================================================
    subroutine stage2
      ! radii and vector of radial direction
      real :: DirR_D(MaxDim)
      !------------------------------------------------------------------------
      ! get the direction of the magnetic field in the middle
      call get_b_dir(iParticle, DirB_D, IsBody = IsEndOfSegment)
      if(IsEndOfSegment) RETURN
      ! direction at the 2nd stage
      DirLine_D = DirB_D*iDirTrace*iIndexEnd_II(Alignment_,iParticle)
      ! get radial direction
      DirR_D    = StateEnd_VI(x_:z_,iParticle) / &
           norm2( StateEnd_VI(x_:z_,iParticle))
      ! correct the direction to prevent the line from closing
      if(UseBUAlignment) &
           call correct(DirLine_D, iDirTrace*DirU_D, CosBUAngleMax)
      if(UseBRAlignment) &
           call correct(DirLine_D, iDirTrace*DirR_D, CosBRAngleMax)

      ! check whether direction reverses in a sharp turn:
      ! this is an indicator of a problem, break tracing of the line
      if(sum(StateEnd_VI(DirX_:DirZ_,iParticle)*DirLine_D) < 0)then
         call mark_undefined(iKindEnd, iParticle)
         IsEndOfSegment = .true.
         RETURN
      end if

      ! save the direction to correct the next step
      StateEnd_VI(DirX_:DirZ_,iParticle) = DirLine_D
      ! get final location
      StateEnd_VI(x_:z_,iParticle) = StateEnd_VI(XOld_:ZOld_,iParticle) + &
           StateEnd_VI(Ds_, iParticle)*DirLine_D

      if(UseFLRW) call apply_random_walk(DirLine_D)

      ! update location and schedule for message pass
      call check_particle_location(  &
           iKindParticle = iKindEnd ,&
           iParticle     = iParticle,&
           DoMove        = DoMove   ,&
           IsGone        = IsGone    )
      ! Particle may come beyond the boundary of
      ! computational domain or may need to be
      ! moved to different PE
      IsEndOfSegment = IsGone.or.DoMove
    end subroutine stage2
    !==========================================================================
    subroutine correct(Dir_D, DirLim_D, CosAngleMax)
      ! corrects the direction to prevent the line from closing:
      ! if sum(Dir_D*DirLim_D) < CosMax, modify vector Dir_D to
      ! achieve that sum(Dir_D*DirLim_D) = CosMax

      ! current direction
      real, intent(inout):: Dir_D(MaxDim)
      ! direction with which the angle should be limited
      real, intent(in)   :: DirLim_D(MaxDim)
      ! minimal allowed cosine angle between Dir_D and DirLim_D
      real, intent(in)   :: CosAngleMax
      ! actual angle between Dir_D and DirLim_D
      real :: CosAngle
      ! weight coefficients
      real :: Weight, WeightLim
      ! actual angle between Dir_D and DirLim_D
      !------------------------------------------------------------------------
      CosAngle = sum(Dir_D*DirLim_D)
      if(CosAngle >=  CosAngleMax)RETURN
      if(CosAngle <= -CosAngleMax)then
         ! Flip the sign of projection onto DirLim_D
         Dir_D = Dir_D - 2*CosAngle*DirLim_D
         RETURN
      end if
      ! Corrected direction vector is Weight*Dir_D + WeightLim*DirLim_D
      ! Solve the weight coefficients, such that the resulting vector
      ! has a unity length and cosine of its angle with DirLim_D is CosMax:
      Weight = sqrt( (1 - CosAngleMax**2)/(1 - CosAngle**2) )
      WeightLim = CosAngleMax - CosAngle*Weight
      Dir_D = Weight*Dir_D + WeightLim*DirLim_D
    end subroutine correct
    !==========================================================================
    subroutine apply_random_walk(Dir_D)
      use ModCoordTransform, ONLY: cross_product
      use ModRandomNumber, ONLY: random_real
      use ModNumConst, ONLY: cTwoPi
      ! add components perpendicular to the current direction of the line
      ! to achieve the effect of field line random walk
      real, intent(in) :: Dir_D(MaxDim)

      ! current radial distance
      real:: Radius

      ! 2 perpendicular directions to Dir_D
      real:: DirPerp1_D(MaxDim), DirPerp2_D(MaxDim)

      ! random numbers: 2 uniform and 2 normal (see Box-Muller algorithm)
      real:: RndUnif1, RndUnif2, RndGauss1, RndGauss2
      ! seed for random number generator
      integer, save:: iSeed=0
      ! first, find perpendicular directions
      !------------------------------------------------------------------------
      DirPerp1_D = cross_product([1.0,0.0,0.0],Dir_D)
      if(all(DirPerp1_D == 0.0))&
           DirPerp1_D = cross_product([0.0,1.0,0.0],Dir_D)
      DirPerp1_D = DirPerp1_D / norm2(DirPerp1_D)
      DirPerp2_D = cross_product(DirPerp1_D, Dir_D )

      ! find 2D gaussian
      RndUnif1  = random_real(iSeed)
      RndUnif2  = random_real(iSeed)
      RndGauss1 = sqrt(-2*log(RndUnif1)) * cos(cTwoPi*RndUnif2)
      RndGauss2 = sqrt(-2*log(RndUnif1)) * sin(cTwoPi*RndUnif2)

      ! displace the particle
      Radius = norm2(StateEnd_VI(x_:z_,iParticle))
      ! see Laitinen et al. (2016), doi:10.1051/0004-6361/201527801
      StateEnd_VI(x_:z_,iParticle) = StateEnd_VI(x_:z_,iParticle) + &
                  DeltaPhiFLRW * sqrt(Radius * StateEnd_VI(Ds_, iParticle)) * &
                  (DirPerp1_D * RndGauss1 + DirPerp2_D * RndGauss2)
    end subroutine apply_random_walk
    !==========================================================================
  end subroutine particle_line
  !============================================================================
  subroutine copy_end_to_regular(iParticleIn)
    integer, optional, intent(in) :: iParticleIn
    ! copies indicated variables of known end particles to regular particles
    integer, parameter:: iVarCopy_I(3)   = [x_, y_, z_]
    integer, parameter:: iIndexCopy_I(3) = [0, fl_, id_]
    integer:: iParticle, iParticleStart, iParticleEnd
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'copy_end_to_regular'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(present(iParticleIn))then
       iParticleStart = iParticleIn
       iParticleEnd   = iParticleIn
    else
       iParticleStart = 1
       iParticleEnd   = Particle_I(iKindEnd)%nParticle
    end if
    do iParticle = iParticleStart, iParticleEnd
       if(Particle_I(iKindReg)%nParticle == nParticlePerLine*nFieldLineMax)&
            call stop_mpi(&
            NameThisComp//':'//NameSub//': max number of particles exceeded')
       Particle_I(iKindReg)%nParticle = Particle_I(iKindReg)%nParticle + 1
       StateReg_VI(iVarCopy_I, Particle_I(iKindReg)%nParticle) =&
            StateEnd_VI(iVarCopy_I, iParticle)
       iIndexReg_II(iIndexCopy_I, Particle_I(iKindReg)%nParticle) =&
            iIndexEnd_II(iIndexCopy_I, iParticle)
    end do
    call test_stop(NameSub, DoTest)
  end subroutine copy_end_to_regular
  !============================================================================
  subroutine get_b_dir(iParticle, DirB_D, DirU_D, StepSize, IsBody)
    use ModMain, ONLY: UseB0, UseRotatingFrame
    use ModB0, ONLY: get_b0
    use ModPhysics, ONLY: OmegaBody
    ! returns the direction of magnetic field for a given particle
    integer, intent(in):: iParticle
    real,    intent(out):: DirB_D(MaxDim)
    real,    optional, intent(out):: DirU_D(MaxDim)
    real,    optional, intent(out):: StepSize
    logical, optional, intent(out):: IsBody

    ! Coordinates and block #
    real     :: Xyz_D(MaxDim)
    integer  :: iBlock

    ! magnetic field
    real   :: B_D(MaxDim) = 0.0, U_D(MaxDim) = 0.0
    ! interpolation data: number of cells, cell indices, weights
    integer:: nCell, iCell_II(0:nDim, 2**nDim)
    real   :: Weight_I(2**nDim)
    integer:: iCell ! loop variable
    integer:: i_D(MaxDim)
    character(len=200):: StringError

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_b_dir'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    DirB_D = 0; B_D = 0; U_D = 0
    if(present(StepSize))StepSize = 0.0
    ! Coordinates and block #
    Xyz_D   = StateEnd_VI(x_:z_, iParticle)
    iBlock  = iIndexEnd_II(0,iParticle)
    call interpolate_grid_amr_gc(Xyz_D, iBlock, nCell, iCell_II, Weight_I,&
         IsBody)
    if(present(IsBody))then
       if(IsBody)then
          call mark_undefined(iKindEnd,iParticle);RETURN
       end if
    end if
    ! get potential part of the magnetic field at the current location
    if(UseB0)call get_b0(Xyz_D, B_D)
    ! get the remaining part of the magnetic field
    do iCell = 1, nCell
       i_D = 1
       i_D(1:nDim) = iCell_II(1:nDim, iCell)
       B_D = B_D + &
            State_VGB(Bx_:Bz_,i_D(1),i_D(2),i_D(3),iBlock)*Weight_I(iCell)
       U_D = U_D + &
            State_VGB(RhoUx_:RhoUz_,i_D(1),i_D(2),i_D(3),iBlock)/&
            State_VGB(Rho_,i_D(1),i_D(2),i_D(3),iBlock)*Weight_I(iCell)
    end do

    if(all(B_D==0))then
       write(StringError,'(a,es15.6,a,es15.6,a,es15.6)') &
            NameThisComp//':'//NameSub//&
            ': trying to extract line at region with zero magnetic field'//&
            ' at location X = ', &
            Xyz_D(1), ' Y = ', Xyz_D(2), ' Z = ', Xyz_D(3)
       call stop_mpi(StringError)
    end if
    ! normalize vector to unity
    DirB_D(1:nDim) = B_D(1:nDim) / norm2(B_D)
    if(present(StepSize))then
       ! interpolate cell sizes. For non-cartesian grids
       ! the metic tensor is used
       do iCell = 1, nCell
          i_D = 1
          i_D(1:nDim) = iCell_II(1:nDim, iCell)
          StepSize = StepSize + &
               CellVolume_GB(i_D(1),i_D(2),i_D(3),iBlock)*Weight_I(iCell)
       end do
       ! take a fraction of cubic root of inteprolated cell volume as step size
       StepSize = 0.1*StepSize**(1.0/3.0)
    end if
    if(present(DirU_D))then
       if(.not.UseRotatingFrame)then
          ! In the inertial frame the rotational velocity, \omega\times\vec{R}
          ! is added to the velocity in corotating frame. Hence, add
          ! -\omega\times\vec{R} to get the velocity in corotating frame,
          ! which may be aligned with magnetic field
          U_D(x_) = U_D(x_) + OmegaBody*Xyz_D(y_)
          U_D(y_) = U_D(y_) - OmegaBody*Xyz_D(x_)
       end if
       if(all(U_D==0))then
          write(StringError,'(a,es15.6,a,es15.6,a,es15.6)') &
               NameThisComp//':'//NameSub//&
               ': trying to use streamline at region with zero velocity'//&
               ' at location X = ', &
               Xyz_D(1), ' Y = ', Xyz_D(2), ' Z = ', Xyz_D(3)
          call stop_mpi(StringError)
       end if
       DirU_D = 0; DirU_D(1:nDim) = U_D(1:nDim)/norm2(U_D)
    end if
    call test_stop(NameSub, DoTest)
  end subroutine get_b_dir
  !============================================================================

  subroutine advect_particle_line
    ! advect particles with the local plasma velocity
    use ModMain, ONLY: IsTimeAccurate

    ! particles can be advected only in the time accurate run
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'advect_particle_line'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(.not.IsTimeAccurate) &
         RETURN

    ! reset particle's stage
    iIndexReg_II(Pass_,1:Particle_I(iKindReg)%nParticle) = DoneFromScratch_

    call trace_particles(iKindReg, advect_particle, check_done_advect)
    call test_stop(NameSub, DoTest)
  end subroutine advect_particle_line
  !============================================================================

  subroutine advect_particle(iParticle, IsEndOfSegment)
    use ModMain, ONLY: Dt
    ! advect an indiviudal particle using 2-stage integration

    integer, intent(in) :: iParticle
    logical, intent(out):: IsEndOfSegment

    ! whether to schedule a particle for message pass
    logical:: DoMove, IsGone

    ! local plasma velocity
    real :: V_D(MaxDim)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'advect_particle'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(iIndexReg_II(Pass_,iParticle))
    case(DoneFromScratch_)
       ! begin particle's advection
       ! copy last known coordinates to old coords
       StateReg_VI(XOld_:ZOld_,iParticle) = &
            StateReg_VI(x_:z_, iParticle)

       ! get the local plasma velocity
       call get_v
       if(IsEndOfSegment) RETURN
       ! get next location
       StateReg_VI(x_:z_,iParticle) = StateReg_VI(x_:z_,iParticle) + 0.5*Dt*V_D

       ! check location, schedule for message pass, if needed
       call check_particle_location(  &
            iKindParticle = iKindReg ,&
            iParticle     = iParticle,&
            DoMove        = DoMove   ,&
            IsGone        = IsGone    )

       ! Particle may be beyond the boundary of computational domain
       ! or need to move particle to different PE
       IsEndOfSegment = IsGone .or. DoMove

       ! the first stage is done
       iIndexReg_II(Pass_, iParticle) = HalfStep_
    case(HalfStep_)
       ! stage 1 has been finished, proceed to the next one
       ! get the local plasma velocity
       call get_v
       if(IsEndOfSegment)RETURN
       ! get final location
       StateReg_VI(x_:z_,iParticle) = &
            StateReg_VI(XOld_:ZOld_,iParticle) + Dt * V_D

       ! update location and schedule for message pass
       call check_particle_location( &
            iKindParticle = iKindReg,&
            iParticle     = iParticle)
       ! Integration is done
       !
       iIndexReg_II(Pass_, iParticle) = Normal_
       IsEndOfSegment = .true.
    case(Normal_)
       ! do nothing, full time step has been finished
       IsEndOfSegment = .true.
    case default
       call stop_mpi(NameThisComp//':'//NameSub//': Unknown stage ID')
    end select
    call test_stop(NameSub, DoTest)
  contains
    !==========================================================================
    subroutine get_v
      ! get local plasma velocity
      ! Coordinates and block #
      real     :: Xyz_D(MaxDim)
      integer  :: iBlock

      ! variables for AMR interpolation
      integer:: nCell, iCell_II(0:nDim, 2**nDim)
      real   :: Weight_I(2**nDim)
      integer:: iCell ! loop variable
      integer:: i_D(MaxDim)
      !------------------------------------------------------------------------
      ! Coordinates and block #
      Xyz_D   = StateReg_VI(x_:z_, iParticle)
      iBlock  = iIndexReg_II(0,    iParticle)
      ! reset the interpoalted values
      V_D = 0
      ! get the velocity
      call interpolate_grid_amr_gc(Xyz_D, iBlock, nCell, iCell_II, Weight_I, &
           IsEndOfSegment)
      if(IsEndOfSegment)then
         call mark_undefined(iKindReg,iParticle);RETURN
      end if
      ! interpolate the local density and momentum
      do iCell = 1, nCell
         i_D = 1
         i_D(1:nDim) = iCell_II(1:nDim, iCell)
         if(State_VGB(Rho_,i_D(1),i_D(2),i_D(3),iBlock)*Weight_I(iCell) <= 0)&
              call stop_mpi(&
              NameThisComp//':'//NameSub//": zero or negative plasma density")
         ! convert momentum to velocity
         V_D = V_D + &
              State_VGB(RhoUx_:RhoUz_,i_D(1),i_D(2),i_D(3),iBlock) / &
              State_VGB(Rho_,         i_D(1),i_D(2),i_D(3),iBlock) * &
              Weight_I(iCell)
      end do
    end subroutine get_v
    !==========================================================================
  end subroutine advect_particle
  !============================================================================
  subroutine check_done_advect(Done)
    ! check whether all paritcles have been advected full time step
    logical, intent(out):: Done
    !--------------------------------------------------------------------------
    Done = all(iIndexReg_II(Pass_,1:Particle_I(iKindReg)%nParticle)==Normal_)
  end subroutine check_done_advect
  !============================================================================
  !===================ACCESS TO THE PARTICLE DATA==============================
  subroutine get_particle_data(nSize, NameVar, DataOut_VI)

    use ModUtilities, ONLY: split=>split_string

    ! the subroutine gets variables specified in the string StringVar
    ! and writes them into DataOut_VI; data is for all particles otherwise

    integer, intent(in) :: nSize
    real,    intent(out):: DataOut_VI(nSize,Particle_I(iKindReg)%nParticle)
    character(len=*),intent(in) :: NameVar

    ! order of requested variables/indexes
    integer:: iOrderVar_V(nVarAvail), iOrderIndex_I(nIndexAvail + 1)
    ! number of variables/indices found in the request string
    integer:: nVarOut, nIndexOut
    ! order of data in the request
    integer:: iOrder_I(nDataMax)

    character(len=20)            :: Name_I(  nDataMax)
    integer                      :: iVar, iIndex, iOutput, nData

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_particle_data'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    nVarOut = 0  ; nIndexOut = 0
    call split(NameVar,  Name_I, nData)
    ! determine which variables are requested
    VAR:do iVar = 1, nVarAvail
       do iOutput = 1, nData
          if(index(Name_I(iOutput),NameVarAvail_V(iVar)) == 1)then
             ! The variable #iVar is present in the request at #iOutput
             nVarOut              = nVarOut + 1
             iOrderVar_V(nVarOut) = iVar
             iOrder_I(   nVarOut) = iOutput
             CYCLE VAR
          end if
       end do
    end do VAR
    IND:do iIndex = 0, nIndexAvail
       do iOutput = 1, nData
          if(index(Name_I(iOutput),NameIndexAvail_I(iIndex)) == 1)then
            ! The index #iIndex is present in the request at #iOutput
             nIndexOut                     = nIndexOut + 1
             iOrderIndex_I(nIndexOut)      = iIndex
             iOrder_I(nVarOut + nIndexOut) = iOutput
             CYCLE IND
          end if
       end do
    end do IND
    if(nData /= nVarOut + nIndexOut)call stop_mpi(&
         'Unrecognized some of the names: '//NameVar)
    DataOut_VI = 0
    ! store variables
    if(nVarOut   > 0) DataOut_VI(iOrder_I(1:nVarOut),:)                   = &
         StateReg_VI(iOrderVar_V(   1:nVarOut  ),&
         1:Particle_I(iKindReg)%nParticle)
    ! store indexes
    if(nIndexOut > 0) DataOut_VI(iOrder_I(nVarOut+1:nVarOut+nIndexOut),:) = &
         iIndexReg_II(iOrderIndex_I(1:nIndexOut),&
         1:Particle_I(iKindReg)%nParticle)
    call test_stop(NameSub, DoTest)
  end subroutine get_particle_data
  !============================================================================
  subroutine write_plot_particle(iFile)

    ! Save particle data

    use ModMain, ONLY: nStep, IsTimeAccurate, tSimulation
    use ModIO, ONLY: &
         StringDateOrTime, NamePlotDir, &
         TypeFile_I, TypePlot_I, TypePlotFormat_I, Plot_
    use ModPlotFile, ONLY: save_plot_file

    integer, intent(in):: iFile

    character(len=100) :: NameFile, NameStart, NameVar
    character (len=80) :: NameProc
    ! container for data
    real, allocatable:: PlotVar_VI(:,:)

    logical:: IsIdl
    integer:: iPlotFile

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_plot_particle'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    iPlotFile = iFile - Plot_

    ! Set the name of the variables based on plot form
    select case(TypePlotFormat_I(iFile))
    case('tec')
       IsIdl = .false.
       NameVar = '"X", "Y", "Z"'
       NameVar = trim(NameVar)//', "FieldLine"'
    case default
       call stop_mpi(&
            NameThisComp//':'//NameSub//' ERROR invalid plot form='//&
            TypePlotFormat_I(iFile))
    end select

    ! name of output files
    if(iPlotFile < 10)then
       write(NameStart,'(a,i1,a)') &
            trim(NamePlotDir)//trim(TypePlot_I(iFile))//'_',iPlotFile
    else
       write(NameStart,'(a,i2,a)') &
            trim(NamePlotDir)//trim(TypePlot_I(iFile))//'_',iPlotFile
    end if

    NameFile = NameStart

    if(IsTimeAccurate)then
       call get_time_string
       NameFile = trim(NameFile)// "_t"//StringDateOrTime
    end if
    write(NameFile,'(a,i7.7,a)') trim(NameFile) // '_n',nStep

    ! String containing the processor index
    if(nProc < 10000) then
       write(NameProc, '(a,i4.4,a)') "_pe", iProc
    elseif(nProc < 100000) then
       write(NameProc, '(a,i5.5,a)') "_pe", iProc
    else
       write(NameProc, '(a,i6.6,a)') "_pe", iProc
    end if

    NameFile = trim(NameFile) // trim(NameProc)

    if(IsIdl)then
       NameFile = trim(NameFile) // '.out'
    else
       NameFile = trim(NameFile) // '.dat'
    end if

    ! get the data on this processor
    if(allocated(PlotVar_VI)) deallocate(PlotVar_VI)
    allocate(PlotVar_VI(5,Particle_I(iKindReg)%nParticle))
    call get_particle_data(5, 'xx yy zz fl id', PlotVar_VI)

    call save_plot_file(&
         NameFile, &
         TypeFileIn     = TypeFile_I(iFile), &
         StringHeaderIn = "TEMPORARY", &
         TimeIn         = tSimulation, &
         nStepIn        = nStep, &
         NameVarIn      = 'X Y Z FieldLine Index', &
         IsCartesianIn  = .false., &
         CoordIn_I      = PlotVar_VI(1, :), &
         VarIn_VI       = PlotVar_VI(2:,:))

    deallocate(PlotVar_VI)

    call test_stop(NameSub, DoTest)
  end subroutine write_plot_particle
  !============================================================================

end module ModParticleFieldLine
!==============================================================================
