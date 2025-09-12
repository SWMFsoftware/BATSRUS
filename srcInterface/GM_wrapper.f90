!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module GM_wrapper

  ! Wrapper for BATSRUS Global Magnetosphere (GM) component

  use GM_couple_pt          !^CMP IF PT
  use GM_couple_ie          !^CMP IF IE
  use GM_couple_im          !^CMP IF IM
  use GM_couple_ps          !^CMP IF PS
  use GM_couple_ih          !^CMP IF IH
  use GM_couple_rb          !^CMP IF RB
  use GM_couple_pw          !^CMP IF PW
  use GM_couple_pc          !^CMP IF PC
  use GM_couple_ua          !^CMP IF UA

  use BATL_lib, ONLY: iProc, nProc, iComm
  use ModBatsrusMethods, ONLY: &
       BATS_init_session, BATS_setup, BATS_advance, BATS_save_files, &
       BATS_finalize
  use ModUtilities, ONLY: CON_set_do_test, CON_stop

  implicit none

  private ! except

  ! CON wrapper
  public:: GM_set_param
  public:: GM_init_session
  public:: GM_run
  public:: GM_save_restart
  public:: GM_finalize

  ! Coupling toolkit
  public:: GM_synchronize_refinement

  ! Point coupling
  public:: GM_get_grid_info
  public:: GM_find_points

  ! Pointer coupling
  public:: GM_use_pointer

  !^CMP IF IE BEGIN
  public:: GM_get_for_ie
  public:: GM_put_from_ie
  public:: GM_get_info_for_ie
  !^CMP END IE

  !^CMP IF IH BEGIN
  public:: GM_is_right_boundary_d
  public:: GM_put_from_mh           ! coupling toolkit based coupler
  public:: GM_put_from_ih_buffer    ! buffer grid based coupler
  public:: GM_get_for_global_buffer ! buffer grid based coupler
  !^CMP END IH

  !^CMP IF IM BEGIN
  public:: GM_get_for_im_trace_crcm ! for CRCM
  public:: GM_get_for_im_crcm       ! for CRCM
  public:: GM_get_sat_for_im_crcm   ! for CRCM
  public:: GM_get_for_im_trace      ! for RAM
  public:: GM_get_for_im_line       ! for RAM
  public:: GM_get_for_im            ! for RCM
  public:: GM_satinit_for_im        ! initialize satellite
  public:: GM_get_sat_for_im        ! get satellite info
  public:: GM_put_from_im           ! from IM
  public:: GM_put_from_im_cimi      ! from MI
  !^CMP END IM

  !^CMP IF PS BEGIN
  public:: GM_put_from_ps
  !^CMP END PS

  !^CMP IF PC BEGIN
  public:: GM_get_for_pc_dt
  public:: GM_get_for_pc_init
  public:: GM_get_for_pc
  public:: GM_get_for_pc_grid_info
  public:: GM_put_from_pc
  !^CMP END PC

  !^CMP IF PT BEGIN
  public:: GM_get_for_pt
  !^CMP END PT

  !^CMP IF PW BEGIN
  public:: GM_get_for_pw
  public:: GM_put_from_pw
  !^CMP END PW

  !^CMP IF RB BEGIN
  public:: GM_get_for_rb_trace
  public:: GM_get_for_rb
  public:: GM_satinit_for_rb
  public:: GM_get_sat_for_rb
  !^CMP END RB

  !^CMP IF UA BEGIN
  public:: GM_put_from_ua
  !^CMP END UA

contains
  !============================================================================

  subroutine GM_set_param(CompInfo, TypeAction)

    use CON_comp_info
    use BATL_lib, ONLY: nThread
    use ModIO, ONLY: iUnitOut, StringPrefix, STDOUT_, NamePlotDir
    use ModSetParameters, ONLY: set_parameters
    use ModRestartFile, ONLY: NameRestartInDir, NameRestartOutDir
    use ModMain, ONLY: NameThisComp, &
         IsTimeAccurate, tSimulation, StartTime, iStartTime_I, UseRotatingBc
    use ModB0, ONLY: DtUpdateB0, DoUpdateB0
    use CON_physics, ONLY: get_time, get_planet
    use ModTimeConvert, ONLY: time_real_to_int

    ! Arguments
    type(CompInfoType), intent(inout) :: CompInfo   ! Information for this comp
    character (len=*), intent(in)     :: TypeAction ! What to do

    logical :: DoTest,DoTestMe
    character(len=*), parameter:: NameSub = 'GM_set_param'
    !--------------------------------------------------------------------------
    call CON_set_do_test(NameSub, DoTest, DoTestMe)

    if(DoTest)write(*,*)NameSub,' called with TypeAction,iProc=', &
         TypeAction, iProc

    select case(TypeAction)
    case('VERSION')
       call put(CompInfo, Use=.true., NameVersion='BATSRUS')
    case('MPI')
       call get(CompInfo, iComm=iComm, iProc=iProc, nProc=nProc, &
            nThread=nThread, Name=NameThisComp)

       NamePlotDir(1:2)       = NameThisComp
       NameRestartInDir(1:2)  = NameThisComp
       NameRestartOutDir(1:2) = NameThisComp
    case('READ')
       call set_parameters('READ')
    case('CHECK')
       call get_time( &
            DoTimeAccurateOut = IsTimeAccurate, &
            tSimulationOut    = tSimulation, &
            tStartOut         = StartTime)
       call get_planet( &
            DtUpdateB0Out  = DtUpdateB0,    &
            DoUpdateB0Out  = DoUpdateB0,    &
            UseRotationOut = UseRotatingBc)
       call time_real_to_int(StartTime,iStartTime_I)

       call set_parameters('CHECK')
    case('STDOUT')
       iUnitOut=STDOUT_
       if(iProc==0)then
          StringPrefix = NameThisComp//':'
       else
          write(StringPrefix,'(a,i4.4,a)')NameThisComp,iProc,':'
       end if
    case('FILEOUT')
       call get(CompInfo,iUnitOut=iUnitOut)
       StringPrefix=''
    case('GRID')
       call GM_set_grid
    case default
       call CON_stop(NameSub//' SWMF_ERROR: invalid TypeAction='//TypeAction)
    end select

  end subroutine GM_set_param
  !============================================================================
  ! ROUTINE: GM_set_grid - intialize, set and broadcast adaptive block grid
  subroutine GM_set_grid
    use MH_domain_decomposition
    use CON_coupler
    use ModMain, ONLY: TypeCoordSystem, NameVarCouple
    use ModPhysics, ONLY: No2Si_V, UnitX_
    use ModVarIndexes, ONLY: nVar
    use ModAdvance, ONLY: State_VGB
    use CON_comp_param, ONLY: GM_
    use ModGeometry, ONLY: TypeGeometry, RadiusMin, RadiusMax
    use BATL_lib, ONLY: CoordMin_D, CoordMax_D, Particle_I
    use ModParticleFieldLine, ONLY: iKindReg, UseParticles

    logical:: UseParticleLine = .false.
    integer:: nParticle = 0, iError = 0

    logical:: DoTest, DoTestMe
    ! revision history:
    ! 23Aug03 I.Sokolov <igorsok@umich.edu> - initial prototype/prolog/code
    ! 03Sep03 G.Toth    <gtoth@umich.edu> - removed test_message_pass
    !                                      call synchronize_refinement directly

    character(len=*), parameter:: NameSub = 'GM_set_grid'
    !--------------------------------------------------------------------------
    DoTest=.false.; DoTestMe=.false.

    if(done_dd_init(GM_))RETURN
    call init_decomposition(GM_,GM_,3,.true.)

    ! Note: for Cartesian grid RadiusMin=xMin and RadiusMax=xMax
    call set_coord_system(GM_, &
         TypeCoord = TypeCoordSystem, &
         UnitX     = No2Si_V(UnitX_), &
         nVar      = nVar, &
         NameVar   = NameVarCouple, &
         TypeGeometry = TypeGeometry, &
         Coord1_I     = [ RadiusMin, RadiusMax ], &
         Coord2_I     = [ CoordMin_D(2), CoordMax_D(2) ], &
         Coord3_I     = [ CoordMin_D(3), CoordMax_D(3) ]  )

    if(is_proc(GM_)) Grid_C(GM_)%State_VGB => State_VGB

    if(is_proc(GM_))then
       call init_decomposition(&
            MH_Domain,GM_,3,.true.)
       call MH_get_root_decomposition(MH_Domain)
       call MH_update_local_decomposition(MH_Domain)
       MH_Domain%IsLocal=.true.
    end if
    call CON_set_do_test('test_grids', DoTest, DoTestMe)

    if(is_proc0(GM_))call MH_get_root_decomposition(GM_)

    call bcast_decomposition(GM_)

    call synchronize_refinement(GM_,MH_Domain)
    ! Check if the decomosition for lines is needed
    if(is_proc0(GM_))UseParticleLine = UseParticles
    call MPI_bcast(UseParticleLine,1,MPI_LOGICAL,&
         i_proc0(GM_),i_comm(),iError)
    if(UseParticleLine)then
       call init_decomposition_dd(&
       MH_LineDecomposition, GM_, nDim=1)
       if(is_proc0(GM_))then
          nParticle = Particle_I(iKindReg)%nParticle
          call get_root_decomposition_dd(&
               MH_LineDecomposition, &
               [n_proc(GM_)],      &
               [0.50],              &
               [real(n_proc(GM_)*nParticle) + 0.50], &
               [nParticle])
       end if
       call bcast_decomposition_dd(MH_LineDecomposition)
       if(DoTest.and.is_proc0(GM_))call show_domain_decomp(&
            MH_LineDecomposition)
    end if
  end subroutine GM_set_grid
  !============================================================================
  ! ROUTINE: GM_synchronize_refinement - synchronize global grid for GM_
  subroutine GM_synchronize_refinement(iProc0,iCommUnion)

    use MH_domain_decomposition
    use CON_comp_param, ONLY:GM_
    integer,intent(in) :: iProc0,iCommUnion
    ! revision history:
    ! 23AUG03  I.Sokolov <igorsok@umich.edu> - initial prototype/code/prolog
    ! 03SEP03  G.Toth    <gtoth@umich.edu> - arguments are not optional now

    !--------------------------------------------------------------------------
    if(is_proc(GM_)) &
         call MH_update_local_decomposition(MH_Domain)

    call synchronize_refinement(&
         GM_,MH_Domain,iProc0,iCommUnion)

  end subroutine GM_synchronize_refinement
  !============================================================================
  subroutine GM_get_grid_info(nDimOut, iGridOut, iDecompOut)

    use BATL_lib, ONLY: nDim
    use ModMain, ONLY: iNewGrid, iNewDecomposition

    integer, intent(out):: nDimOut    ! grid dimensionality
    integer, intent(out):: iGridOut   ! grid index (increases with AMR)
    integer, intent(out):: iDecompOut ! decomposition index

    ! Return basic grid information useful for model coupling.
    ! The decomposition index increases with load balance and AMR.

    logical:: DoTest, DoTestMe
    character(len=*), parameter:: NameSub = 'GM_get_grid_info'
    !--------------------------------------------------------------------------
    call CON_set_do_test(NameSub, DoTest, DoTestMe)

    nDimOut    = nDim
    iGridOut   = iNewGrid
    iDecompOut = iNewDecomposition

    if(DoTestMe)write(*,*) NameSub,': nDimOut, iGridOut, iDecompOut=', &
         nDimOut, iGridOut, iDecompOut

  end subroutine GM_get_grid_info
  !============================================================================
  subroutine GM_find_points(nDimIn, nPoint, Xyz_DI, iProc_I)

    use BATL_lib, ONLY: MaxDim, find_grid_block
    use ModPhysics, ONLY: Si2No_V, UnitX_

    integer, intent(in) :: nDimIn ! dimension of position vectors
    integer, intent(in) :: nPoint ! number of positions
    real,    intent(in) :: Xyz_DI(nDimIn,nPoint) ! positions
    integer, intent(out):: iProc_I(nPoint)       ! processor owning position

    ! Find array of points and return processor indexes owning them
    ! Could be generalized to return multiple processors...

    real:: Xyz_D(MaxDim) = 0.0
    integer:: iPoint, iBlock

    character(len=*), parameter:: NameSub = 'GM_find_points'
    !--------------------------------------------------------------------------
    do iPoint = 1, nPoint
       Xyz_D(1:nDimIn) = Xyz_DI(:,iPoint)*Si2No_V(UnitX_)
       call find_grid_block(Xyz_D, iProc_I(iPoint), iBlock)
    end do

  end subroutine GM_find_points
  !============================================================================

  subroutine GM_init_session(iSession, TimeSimulation)

    use ModMain, ONLY: UseIe, UsePw, TypeCellBc_I, TypeFaceBc_I, body1_
    use ModMain, ONLY: UseIM
    use ModPIC, ONLY: NameVersionPic
    use CON_coupler, ONLY: Couple_CC, IE_, IM_, GM_, IH_, PW_, PS_, PC_, SC_
    use CON_world, ONLY: get_comp_info

    integer,  intent(in) :: iSession         ! session number (starting from 1)
    real,     intent(in) :: TimeSimulation   ! seconds from start time

    logical :: IsUninitialized = .true.
    logical :: DoTest, DoTestMe
    character(len=*), parameter:: NameSub = 'GM_init_session'
    !--------------------------------------------------------------------------
    call CON_set_do_test(NameSub, DoTest, DoTestMe)

    UseIm = Couple_CC(IM_,GM_) % DoThis .or. Couple_CC(PS_,GM_) % DoThis
    UsePw = Couple_CC(PW_,GM_) % DoThis
    UseIe = Couple_CC(IE_,GM_) % DoThis

    ! Check if the boundary condition is properly set
    if(UsePw) TypeFaceBc_I(body1_) = 'polarwind'

    if(Couple_CC(GM_, PC_) % DoThis) &
         call get_comp_info(PC_, NameVersion=NameVersionPic)

    if((Couple_CC(IH_,GM_) % DoThis.or.Couple_CC(SC_,GM_) % DoThis) &
         .neqv. (TypeCellBc_I(2)=='coupled'))then
       if(Couple_CC(IH_,GM_) % DoThis.or.Couple_CC(SC_,GM_) % DoThis) then
          TypeCellBc_I(2)='coupled'
       else
          if(iProc==0)write(*,*)NameSub, &
               ' WARNING: GM is not coupled to SC nor to IH, ',&
               ' changing west boundary type from "coupled" to "vary"'
          TypeCellBc_I(2)='vary'
       end if
    end if

    if(IsUninitialized)then
       call BATS_setup
       IsUninitialized = .false.
    end if
    call BATS_init_session

    if(DoTest)write(*,*)NameSub,' finished for session ',iSession

    !$acc update device(UseIe)
  end subroutine GM_init_session
  !============================================================================

  subroutine GM_finalize(TimeSimulation)

    use ModMain, ONLY: IsTimeLoop

    real,     intent(in) :: TimeSimulation   ! seconds from start time

    character(len=*), parameter:: NameSub = 'GM_finalize'
    !--------------------------------------------------------------------------
    ! We are not advancing in time any longer
    IsTimeLoop = .false.

    call BATS_save_files('FINAL')

    call BATS_finalize

  end subroutine GM_finalize
  !============================================================================

  subroutine GM_save_restart(TimeSimulation)

    use CON_coupler, ONLY: NameRestartOutDirComp
    use ModRestartFile, ONLY: NameRestartOutDir

    real,     intent(in) :: TimeSimulation   ! seconds from start time

    character(len=*), parameter:: NameSub = 'GM_save_restart'
    !--------------------------------------------------------------------------
    if( NameRestartOutDirComp /= '') NameRestartOutDir = NameRestartOutDirComp

    call BATS_save_files('RESTART')

  end subroutine GM_save_restart
  !============================================================================

  subroutine GM_run(TimeSimulation,TimeSimulationLimit)

    use ModMain, ONLY: tSimulation

    real, intent(inout) :: TimeSimulation   ! current time of component

    real, intent(in):: TimeSimulationLimit ! simulation time not to be exceeded

    logical :: DoTest, DoTestMe
    character(len=*), parameter:: NameSub = 'GM_run'
    !--------------------------------------------------------------------------
    call CON_set_do_test(NameSub, DoTest, DoTestMe)

    if(DoTestMe)write(*,*)NameSub,' called with tSim, tSimLimit=',&
         TimeSimulation, TimeSimulationLimit

    if(abs(tSimulation - TimeSimulation)>0.0001) then
       write(*,*)NameSub,' GM time=', tSimulation, &
            ' SWMF time=', TimeSimulation
       call CON_stop(NameSub// &
            ' SWMF_ERROR: GM and SWMF simulation times differ')
    end if

    call BATS_advance(TimeSimulationLimit)

    ! Return time after the time step
    TimeSimulation = tSimulation

    if(DoTestMe)write(*,*)NameSub,' finished with tSim=', TimeSimulation

  end subroutine GM_run
  !============================================================================

  subroutine GM_use_pointer(iComp, tSimulation)

    use CON_coupler, ONLY: NameComp_I, Grid_C
    use ModMain, ONLY: nVarComp2, NameVarComp2, StateComp2_VGB

    integer, intent(in):: iComp
    real,    intent(in):: tSimulation

    logical:: DoTest, DoTestMe
    character(len=*), parameter:: NameSub = 'GM_use_pointer'
    !--------------------------------------------------------------------------
    call CON_set_do_test(NameSub, DoTest, DoTestMe)

    nVarComp2      =  Grid_C(iComp)%nVar
    NameVarComp2   =  Grid_C(iComp)%NameVar
    StateComp2_VGB => Grid_C(iComp)%State_VGB

    if(DoTestMe)then
       write(*,*) NameSub,' called from component    =', NameComp_I(iComp)
       write(*,*) NameSub,' nVarComp2, NameVarComp2  =', &
            nVarComp2, trim(NameVarComp2)
       ! write(*,*) NameSub,' StateComp2_VGB(:,1,1,1,1)=', &
       !      StateComp2_VGB(:,1,1,1,1)
    end if

    call user_action('POINTERCOUPLING_'//NameComp_I(iComp))

  end subroutine GM_use_pointer
  !============================================================================
  function GM_is_right_boundary_d(iBlock) RESULT(IsRightBoundary_D)
    use ModParallel, ONLY: Unset_, DiLevel_EB
    use BATL_size, ONLY: nDim
    integer, intent(in) :: iBlock
    logical :: IsRightBoundary_D(nDim)
    integer:: iDir
    character(len=*), parameter:: NameSub = 'GM_is_right_boundary_d'
    !--------------------------------------------------------------------------
    do iDir = 1, nDim
       IsRightBoundary_D(iDir) = DiLevel_EB(2*iDir,iBlock) == Unset_
    end do
  end function GM_is_right_boundary_d
  !============================================================================
end module GM_wrapper
!==============================================================================
