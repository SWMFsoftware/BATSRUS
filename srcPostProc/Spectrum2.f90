! This as a testing and stand alone Spectrum main program.
program spectrum
  use BATL_size, ONLY: &
       nDim, MaxBlock, nBlock, MinI, MaxI, MinJ, MaxJ, MinK, MaxK
  use BATL_lib, ONLY: &
       test_start, test_stop, iProc, nProc, iComm, init_mpi, clean_mpi
  use BATL_test, ONLY: StringTest
  use ModReadParam
  use ModNodes, ONLY: clean_mod_nodes
  use ModMpi
  use ModAdvance, ONLY: State_VGB, iTypeAdvance_BP, iTypeAdvance_B

  ! use ModSpectrum
  use ModSpectrumLos, ONLY: init_los_spectrum, get_los_data_cube
  use ModSpectrum, ONLY: init_spectrum, spectrum_calc_flux

  implicit none

  !----------------------------------------------------------------------------
  character(len=200) NameFileRoot, NameSpectrumOut
  character(len=200) TypeDataFile, NameUnitInput

  integer :: nRootRead_D(3)
  character(len=20):: TypeGeometryBatl
  character(len=20) :: TypeCellBc_I(6) = 'none'
  integer :: nStep
  real :: tStep

  integer :: nSpectralLine = 0
  real :: ProtonElectronRatio

  ! (1,:) For each pixel, the index of the processor that is going to store the
  ! state vectors and calculate spectrum flux along LOS. Because any one
  ! processor only stores a part of the image pixels (with corresponding LOS
  ! segments), it would be beneficial to know a pixel is going to be stored in
  ! which processor. Currently we are using cyclic parallelization, so this
  ! array seems redundant. But later if you decide to implement a better
  ! algorithm to redistribute work, this array would be useful.
  ! (2,:) Location of this pixel in the processor array (which stores only a
  ! part of the image). The interpolated state variables are then stored on
  ! iProc = Info(1,iPix), in sub-array State(:,:,Info(2,iPix))
  integer, allocatable :: iPixelProcInfo_II(:,:)  ! size is [2,nPixelA*nPixelB]

  ! Number of pixels stored in this processor, approx nPixelA*nPIxelB/nProc
  integer :: nPixelProc
  integer :: MaxLosSeg   ! maximum number of LOS segments, can be dynamic
  integer :: nLosSegInc   ! increment of array size if nLOS is too small
  ! Number of LOS segments at a pixel, size is nPixelProc
  integer, allocatable :: nLosSeg_I(:)
  ! State variables stored on this processor. [nVar+1, MaxLosSeg, nPixelProc]
  real, allocatable :: StatePixelSegProc_VII(:,:,:)
  real, allocatable :: Emis_IGB(:,:,:,:,:), EmisPixelSegProc_III(:,:,:)

  integer :: nPixel_I(2)
  real :: ImageCenter_I(2), ImageSize_I(2), ObsPos_D(3)
  real, allocatable :: PixelPosA_I(:), PixelPosB_I(:)
  real :: rInner, rOuter

  logical, allocatable :: UsedBatl_B(:)

  integer :: iError

  logical :: DoTestOutputLosSegments

  ! ------------------------------

  ! StringTest = 'get_los_data_cube'

  call init_mpi
  call init_defaults

  call timing_comp_proc("  ",iProc)
  if (iProc == 0) call timing_active(.true.)
  call timing_step(0)
  call timing_depth(3)
  call timing_report_style('tree')
  call timing_start('Spectrum')

  call init_program_spectrum

  if (iProc == 0) write(*,*) 'Init los.'
  allocate(iPixelProcInfo_II(2,nPixel_I(1)*nPixel_I(2)))
  allocate(PixelPosA_I(nPixel_I(1)),PixelPosB_I(nPixel_I(2)))
  call init_los_spectrum(rInner, rOuter, nPixel_I, ImageCenter_I, ImageSize_I, &
       ObsPos_D, PixelPosA_I, PixelPosB_I, nPixelProc, iPixelProcInfo_II, &
       nLosSeg_I, StatePixelSegProc_VII)

  if (iProc == 0) write(*,*) 'Get Los data cube.'
  call get_los_data_cube(iPixelProcInfo_II, StatePixelSegProc_VII, nLosSeg_I, State_VGB)

  ! convert unit from normalized to SI
  if (iProc == 0) write(*,*) 'Get Los data cube for Mhd variables.'
  call convert_units

  if (DoTestOutputLosSegments) then
     if (iProc == 0) write(*,*) 'Collect Los segments.'
     call collect_los_segments('IO2/1.vtk',DoTestIn=.false.)
  endif

  if (iProc == 0) write(*,*) 'Init Spectrum.'
  call init_spectrum(StatePixelSegProc_VII, nLosSeg_I, nSpectralLine, ProtonElectronRatio)

  call clean_mod_nodes
  deallocate(State_VGB,iTypeAdvance_BP,iTypeAdvance_B)

  if (iProc == 0) write(*,*) 'Spectrum calculate flux.'
  call spectrum_calc_flux(StatePixelSegProc_VII, nLosSeg_I)

  deallocate(StatePixelSegProc_VII)

  if (iProc == 0) write(*,*) 'Collect Spectrum results.'
  call collect_save_spectrum(NameSpectrumOut)

  if (nProc>1) call mpi_barrier(iComm,iError)
  call timing_stop('Spectrum')
  call timing_report_total
  call clean_mpi

contains
  !============================================================================

  subroutine init_defaults

    use ModVarIndexes, ONLY: &
         NameVar_V, nVar, nFluid, nWave, WaveFirst_, WaveLast_, IonFirst_
    use ModMain, ONLY: NameThisComp, NameVarLower_V
    use ModUtilities, ONLY: lower_case
    use ModPhysics, ONLY: TypeNormalization, TypeIoUnit, BodyTDim_I, BodyNDim_I
    use ModConst, ONLY: cTiny

    integer :: iVar, iWave
    character(len=3) :: NameWave

    ! Fix the NameVar_V string for waves
    !--------------------------------------------------------------------------
    if(WaveLast_ > 1)then
       do iWave = 1, nWave
          write(NameWave,'(a,i2.2)') 'I',iWave
          NameVar_V(WaveFirst_+iWave-1) = NameWave
       enddo
    endif

    do iVar = 1, nVar+nFluid
       NameVarLower_V(iVar) = NameVar_V(iVar)
       call lower_case(NameVarLower_V(iVar))
    end do

    NameThisComp = 'SC'
    TypeNormalization = "SOLARWIND"
    TypeIoUnit = "HELIOSPHERIC"

    BodyTDim_I            = 2.85E06    ! K
    BodyNDim_I(IonFirst_) = 1.50E8     ! /cc  protons
    BodyNDim_I(IonFirst_+1:nFluid) = BodyNDim_I(IonFirst_)*cTiny

  end subroutine init_defaults
  !============================================================================

  subroutine init_program_spectrum

    use BATL_tree, ONLY: read_tree_file, get_tree_position, distribute_tree, &
         show_tree, adapt_tree, iTree_IA, iNode_B, iStatusNew_A, &
         Unset_, Unused_, Used_, Status_, Coarsen_
    use BATL_lib, ONLY: init_grid_batl, init_batl, CoordMin_D, CoordMax_D, &
         Unused_B, Xyz_DGB
    use BATL_geometry, ONLY: gen_to_radius, LogRgen_I, IsLogRadius, &
         IsGenRadius, IsSpherical, IsRLonLat
    use ModBatlInterface, ONLY: set_batsrus_grid
    use ModParallel, ONLY: init_mod_parallel
    use ModPhysics, ONLY: &
         init_vector_variables, set_physics_constants, iVectorVar_I
    use ModMain, ONLY: init_mod_main
    use ModAdvance, ONLY: &
         init_mod_advance, clean_mod_advance, State_VGB, iTypeAdvance_BP, &
         iTypeAdvance_B
    use ModBoundaryGeometry, ONLY: init_mod_boundary_cells
    use ModVarIndexes, ONLY: NameVar_V, nVar
    use ModNodes, ONLY: init_mod_nodes
    use ModGeometry, ONLY: init_mod_geometry, XyzMin_D, XyzMax_D, TypeGeometry

    integer :: iBlock, iNode, iError
    real :: rMin, rMax, PositionMin_D(3), PositionMax_D(3), rSize
    !--------------------------------------------------------------------------
    character(len=200) NameInfoFile, NameTreeFile, NameDataFile

    rInner = 1.001
    rOuter = 5.
    ImageSize_I = [2.6, 2.6]
    nPixel_I = [0, 0]

    ObsPos_D = [218.5,0.001,0.001]
    DoTestOutputLosSegments = .false.

    call read_param_file

    NameInfoFile = trim(NameFileRoot)//'.info'
    call read_info_file(NameInfoFile)

    ! from set_parameters
    call init_vector_variables

    call init_batl(XyzMin_D(1:nDim), XyzMax_D(1:nDim), MaxBlock, &
         TypeGeometryBatl, TypeCellBc_I(1:2*nDim-1:2) == 'periodic', &
         nRootRead_D(1:nDim), UseRadiusIn=.true., UseDegreeIn=.false.,&
         RgenIn_I = exp(LogRgen_I), UseUniformAxisIn=.true.,&
         UseFDFaceFluxIn=.false., iVectorVarIn_I=iVectorVar_I)
    if(IsLogRadius .or. IsGenRadius)then
       XyzMin_D(1) = CoordMin_D(1)
       XyzMax_D(1) = CoordMax_D(1)
    end if

    ! from BatsrusMethods
    call init_mod_parallel
    call init_grid_batl
    call set_batsrus_grid

    call init_mod_main
    allocate(State_VGB(nVar,MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
    allocate(iTypeAdvance_BP(MaxBlock,0:nProc-1))
    allocate(iTypeAdvance_B(MaxBlock))
    iTypeAdvance_BP = 0
    iTypeAdvance_B = 0

    call init_mod_geometry
    call init_mod_boundary_cells
    call init_mod_nodes

    ! StringTest = 'set_physics_constants'
    call set_physics_constants

    if (iProc == 0) write(*,*) 'Read tree.'
    NameTreeFile = trim(NameFileRoot)//'.tree'
    call read_tree_file(NameTreeFile)
    call init_grid_batl
    call set_batsrus_grid

    if (iProc == 0) write(*,*) 'Read data.'
    NameDataFile = trim(NameFileRoot)//'.out'
    call read_data_file(NameDataFile)

  end subroutine init_program_spectrum
  !============================================================================

  subroutine read_param_file
    use ModReadParam, ONLY: &
         lStringLine, read_file, read_init, read_line, read_command, &
         read_echo_set

    character(len=*), parameter :: NameParamFile = 'SPECTRUM.in'
    character(len=lStringLine) :: &
         StringLine, NameCommand, NameFileHead, NameVarInput
    logical :: DoEcho = .true.
    !--------------------------------------------------------------------------
    character(len=10) TypeObsCoord
    logical :: DoObsRotate, DoObsRotTangent
    real :: ObsRotAngle

    call read_file(NameParamFile,IsVerbose=.false.)
    call read_echo_set(.false.)
    call read_init()

    READPARAM: do
       if(.not.read_line(StringLine) ) EXIT READPARAM
       if(.not.read_command(NameCommand)) CYCLE READPARAM

       select case(NameCommand)
       case("#ECHO")
          call read_var('DoEcho',DoEcho)
          DoEcho = DoEcho .and. iProc==0
          if (iProc ==0) then
             write(*,'(A)') trim(NameCommand)
             write(*,'(L1)') DoEcho
             write(*,*)
          endif

       case("#GRIDBLOCK", "#GRIDBLOCKALL")
          if (DoEcho) write(*,'(A)') trim(NameCommand)
          call read_echo_set(DoEcho)
          call read_var('MaxBlock', MaxBlock)
          if(NameCommand == "#GRIDBLOCKALL") MaxBlock = 1 + (MaxBlock-1)/nProc
          call read_echo_set(.false.)
          if (DoEcho) write(*,*)

       case('#DATAFILE')
          if (DoEcho) write(*,'(A)') trim(NameCommand)
          call read_echo_set(DoEcho)
          call read_var('NameFileRoot', NameFileRoot)
          call read_echo_set(.false.)
          if (DoEcho) write(*,*)

       case('#OUTFILE')
          if (DoEcho) write(*,'(A)') trim(NameCommand)
          call read_echo_set(DoEcho)
          call read_var('NameSpectrumOut', NameSpectrumOut)
          call read_echo_set(.false.)
          if (DoEcho) write(*,*)

       case('#LIMITRADIUS')
          if (DoEcho) write(*,'(A)') trim(NameCommand)
          call read_echo_set(DoEcho)
          call read_var('rInner', rInner)
          call read_var('rOuter', rOuter)
          call read_echo_set(.false.)
          if (DoEcho) write(*,*)

       case('#IMAGE')
          if (DoEcho) write(*,'(A)') trim(NameCommand)
          call read_echo_set(DoEcho)
          call read_var('nPixelA',nPixel_I(1))
          call read_var('nPixelB',nPixel_I(2))
          call read_var('ImageCenterX',ImageCenter_I(1))
          call read_var('ImageCenterY',ImageCenter_I(2))
          call read_var('ImageSizeX',ImageSize_I(1))
          call read_var('ImageSizeY',ImageSize_I(2))
          call read_echo_set(.false.)
          if (DoEcho) write(*,*)

       case('#OBSERVER')
          if (DoEcho) write(*,'(A)') trim(NameCommand)
          call read_echo_set(DoEcho)
          call read_var('TypeObsCoord', TypeObsCoord)
          select case(TypeObsCoord)
          case('HGR')
             call read_var('ObsPosX', ObsPos_D(1))
             call read_var('ObsPosY', ObsPos_D(2))
             call read_var('ObsPosZ', ObsPos_D(3))
          case('HGI')
          case('SAT')
          case default
             call stop_mpi('Observer coordinate system '//trim(TypeObsCoord)//&
                  ' not supported.')
          end select
          call read_var('DoObsRotate', DoObsRotate)
          if (DoObsRotate) then
             call read_var('DoObsRotTangent', DoObsRotTangent)
             if (.not. DoObsRotTangent) then
                call read_var('ObsRotAngle', ObsRotAngle)
             endif
          endif
          call read_echo_set(.false.)
          if (DoEcho) write(*,*)

       case default
       end select
    end do READPARAM

    if (any(nPixel_I <= 0)) then
       call stop_mpi('Number of image pixels less than zero!')
    endif
    if (any(ImageSize_I <= 0)) then
       call stop_mpi('Size of image less than zero!')
    endif

  end subroutine read_param_file
  !============================================================================

  ! read in 3D idl file information, refer to ModSetParameters and PostIDL
  subroutine read_info_file(NameInfoFile)
    use BATL_geometry, ONLY: nRgen, LogRgen_I
    use BATL_size, ONLY: nIJK, nIJK_D, MaxBlock
    use BATL_geometry, ONLY: gen_to_radius
    use ModReadParam, ONLY: &
         lStringLine, read_file, read_init, read_line, read_command, &
         read_echo_set
    use ModVarIndexes, ONLY: nVar
    use ModGeometry, ONLY: XyzMin_D, XyzMax_D, TypeGeometry

    character(len=200), intent(in) :: NameInfoFile

    character(len=lStringLine) :: &
         StringLine, NameCommand, NameFileHead, NameVarInput
    integer :: nProcOld, nDimSim, nIJKRead_D(3), i, nCellPlot, nPlotVar
    logical :: IsLogRadius, IsGenRadius

    character(len=*), parameter:: NameSub = 'read_info_file'
    !--------------------------------------------------------------------------
    call read_file(NameInfoFile,IsVerbose=.false.)
    call read_echo_set(.false.)
    call read_init()

    READPARAM: do
       if(.not.read_line(StringLine) ) EXIT READPARAM
       if(.not.read_command(NameCommand)) CYCLE READPARAM

       select case(NameCommand)
       case('#NDIM')
          call read_var('nDimSim', nDimSim)
          if (nDimSim /= 3) then
             call stop_mpi(NameSub//': Needs a 3D simulation data file')
          endif

       case('#GRIDBLOCKSIZE')
          call read_var('BlockSize1', nIJKRead_D(1))
          call read_var('BlockSize2', nIJKRead_D(2))
          call read_var('BlockSize3', nIJKRead_D(3))
          if(any(nIJK_D/=nIJKRead_D).and.iProc==0)then
             write(*,*)'Code is compiled with nI,nJ,nK=',nIJK_D
             call stop_mpi('Change nI,nJ,nK with Config.pl -g and recompile!')
          end if

       case('#ROOTBLOCK')
          call read_var('nRootBlock1', nRootRead_D(1))
          call read_var('nRootBlock2', nRootRead_D(2))
          call read_var('nRootBlock3', nRootRead_D(3))
          !   if(product(nRootRead_D) > MaxBlock*nProc .and. iProc==0)then
          !     write(*,*)'Not enough grid blocks allocated for root blocks'
          !     write(*,*)'Number of root blocks=',product(nRootRead_D)
          !     write(*,*)'MaxBlock, nProc, MaxBlock*nProc=', MaxBlock, nProc, MaxBlock*nProc
          !     call stop_mpi(NameSub//': insufficient number of grid blocks')
          !   endif

       case('#OUTPUTFORMAT')
          call read_var('TypeOutPutFormat', TypeDataFile)

       case('#NSTEP')
          call read_var('nStep', nStep)

       case('#TIMESIMULATION')
          call read_var('TimeSimulation', tStep)

       case('#PLOTRANGE')
          XyzMin_D = 0; XyzMax_D = 0
          do i = 1, nDimSim
             call read_var('CoordMin', XyzMin_D(i))
             call read_var('CoordMax', XyzMax_D(i))
          enddo

          ! case('#PLOTRESOLUTION')
          !   CellSizePlot_D = 1
          !   do i = 1, nDimSim
          !      call read_var('CellSizePlot', CellSizePlot_D(i))
          !   enddo

          ! case('#CELLSIZE')
          !   dCoordMin_D = 1
          !   do i = 1, nDimSim
          !      call read_var('CellSizeMin', dCoordMin_D(i))
          !   enddo

       case('#NCELL')
          call read_var('nCellPlot', nCellPlot)
          if(nCellPlot/nIJK > MaxBlock*nProc .and. iProc==0)then
             write(*,*)'Not enough grid blocks allocated for root blocks'
             write(*,*)'nCellPlot, nIJK, nBlock =', &
                  nCellPlot, nIJK, nCellPlot/nIJK
             write(*,*)'MaxBlock, nProc, MaxBlock*nProc=', &
                  MaxBlock, nProc, MaxBlock*nProc
             call stop_mpi(NameSub//': insufficient number of grid blocks')
          end if

          ! case('#PLOTVARIABLE')
          !   call read_var('nPlotVar', nPlotVar)
          !   call read_var('NameVar',  NameVar)
          !   call read_var('NameUnit', NameUnit)

          ! case('#SCALARPARAM')
          !   call read_var('nParam', nParamPlot)
          !   allocate(PlotParam_I(nParamPlot))
          !   do i = 1, nParamPlot
          !      call read_var('Param',PlotParam_I(i))
          !   enddo

       case('#GRIDGEOMETRYLIMIT')
          call read_var('TypeGeometry', TypeGeometry)
          IsLogRadius = index(TypeGeometry,'lnr')  > 0
          IsGenRadius = index(TypeGeometry,'genr') > 0
          if(IsGenRadius)then
             call read_var('nRgen', nRGen)
             allocate(LogRgen_I(nRgen))
             do i = 1, nRgen
                call read_var('LogRGen', LogRgen_I(i))
             end do
          end if
          ! if(TypeGeometry == 'roundcube')then
          !   call read_var('rRound0', rRound0)
          !   call read_var('rRound1', rRound1)
          !   call read_var('SqrtNDim',SqrtNDim)
          ! endif
          if(TypeGeometry(1:9)=='spherical') then
             TypeGeometryBatl = 'rlonlat'//TypeGeometry(10:20)
          else
             TypeGeometryBatl = TypeGeometry
          endif

          ! case('#PERIODIC')
          !  do i = 1, nDimSim
          !    call read_var('IsPeriodic',IsPeriodic_D(i))
          !  enddo

          ! case('#TECPLOTCONVERT')
          !  call read_var('DoReadTecplot', DoReadTecplot)
          !  call read_var('nHeaderTec', nHeaderTec)
          !  call read_var('nNodeTec',  nNodeTec)
          !  call read_var('nCellTec',  nCellTec)

       case('#PLOTVARIABLE')
          call read_var('nPlotVar', nPlotVar)
          if (nPlotVar /= nVar) then
             call stop_mpi(NameSub//': number of plot variables incorrect;'// &
                  ' configure SPECTRUM environment the same as BATSRUS/AWSOM.')
          endif
          call read_var('',NameVarInput)
          call read_var('',NameUnitInput)

       case default
          ! write(*,*) 'Ignored command'
       end select
    end do READPARAM

    if (index(TypeGeometry,'genr') > 0) then
       call gen_to_radius(XyzMin_D(1))
       call gen_to_radius(XyzMax_D(1))
    endif

  end subroutine read_info_file
  !============================================================================

  ! read in 3d data file (.out), refer to select_snapshot
  subroutine read_data_file(NameDataFile)
    use BATL_tree, ONLY: iTree_IA, Proc_, Block_
    use BATL_size, ONLY: nI, nJ, nK, nIJK, nG
    use BATL_lib, ONLY: Xyz_DGB, Unused_B
    use BATL_grid, ONLY: find_grid_block
    use BATL_pass_cell, ONLY: message_pass_cell
    use ModConst, ONLY: cTiny
    use ModVarIndexes, ONLY: nVar
    use ModCoordTransform, ONLY: xyz_to_rlonlat
    use ModPlotFile, ONLY: read_plot_file
    use ModIoUnit, ONLY: io_unit_new
    use ModAdvance, ONLY: State_VGB

    character(len=200), intent(in) :: NameDataFile

    integer :: iUnit, nDimOld, nVarOld, nParam, n_D(5), nStep
    real :: Time, Param_I(100)
    real, allocatable :: Coord_DI(:,:), Var_II(:,:)
    integer, allocatable :: iVar_I(:)
    real :: State_V(nVar), Xyz_D(3)
    character(len=200) :: StringHeader, NameVar
    integer :: iNode, iBlock, jBlock, jProc, iRec, iIjk, i, j, k, &
         ii, iCell, iProcFound, iCell_D(3)
    real :: XyzDiff_D(3), rLonLat_D(3)

    character(len=*), parameter:: NameSub = 'read_data_file'
    !--------------------------------------------------------------------------
    iUnit = io_unit_new()

    call read_plot_file(NameDataFile, iUnit, TypeDataFile, &
         StringHeader, nStep, Time, nDimOld, nParamOut=nParam, &
         nVarOut=nVarOld, nOut_D=n_D, ParamOut_I=Param_I, NameVarOut=NameVar)

    if (nDimOld /= 3 .or. n_D(2) /= 1 .or. n_D(3) /= 1) &
         call stop_mpi('Currently only support 3d unstructured grid.')

    allocate(iVar_I(nVar))

    ! Determine the shape of arrays from the header
    if (allocated(Coord_DI)) deallocate(Coord_DI)
    if (allocated(Var_II)) deallocate(Var_II)
    allocate( Coord_DI(nDimOld,n_D(1)), Var_II(nVarOld,n_D(1)))

    ! figure out which input variables are the ones we use from State_VGB
    iVar_I(1:nVar) = [(i, i=1, nVar)]

    ! Read the coord and var arrays
    call read_plot_file(NameDataFile, iUnit, TypeDataFile, &
         CoordOut_DI=Coord_DI, VarOut_VI=Var_II)

    ! from BATL/ModReadAmr
    do iCell = 1, n_D(1)
       Xyz_D = Coord_DI(:,iCell)
       State_V = 0.
       do ii = 1, nVar
          if (iVar_I(ii) > 0) State_V(ii) = Var_II(iVar_I(ii),iCell)
       enddo
       call find_grid_block(Xyz_D, iProcFound, iBlock, iCell_D)
       if(iBlock <= 0)then
          CYCLE
          write(*,*) 'ERROR for iCell, Xyz_D=', iCell, Xyz_D
          call stop_mpi(NameSub//': could not find cell on the grid')
       end if
       if (iProcFound /= iProc) CYCLE
       i = iCell_D(1)
       j = iCell_D(2)
       k = iCell_D(3)
       if (any(abs(Xyz_DGB(:,i,j,k,iBlock)-Xyz_D) > 1e-5)) then
          write(*,*)NameSub,' ERROR at iCell,i,j,k,iBlock,iProc=', &
               iCell, i, j, k, iBlock, iProc
          write(*,*)NameSub,' Xyz_D  =', Xyz_D
          write(*,*)NameSub,' Xyz_DGB=', Xyz_DGB(:,i,j,k,iBlock)
          call stop_mpi(NameSub//': incorrect coordinates')
       endif
       State_VGB(:,i,j,k,iBlock) = State_V
    enddo

    deallocate(Coord_DI, Var_II)
    close(iUnit)

    call message_pass_cell(nVar, State_VGB, nWidthIn=nG)

  end subroutine read_data_file
  !============================================================================

  subroutine write_3d_plot()

    use ModIO, ONLY: &
         nFile, nPlotFile, Plot_, TypeSatPos_I, plot_dx, plot_form, &
         TypeFile_I, plot_vars, plot_dimensional, plot_pars, plot_type, &
         plot_range
    use ModVarIndexes, ONLY: nVar, NameVar_V
    use ModUtilities, ONLY: join_string
    use ModWritePlot, ONLY: write_plot
    use BATL_lib, ONLY: CoordMin_D, CoordMax_D
    use BATL_test, ONLY: StringTest

    integer :: iFile
    !--------------------------------------------------------------------------
    nPlotFile  = 1
    nFile = max(nFile, Plot_ + nPlotFile)
    TypeSatPos_I = 'none'
    iFile = Plot_ + 1
    plot_form(iFile) = 'idl'
    plot_dx(:,iFile) = -1.
    TypeFile_I(iFile) = 'real4'
    plot_dimensional(iFile) = .false.
    call join_string(nVar, NameVar_V(1:nVar), plot_vars(iFile))
    plot_pars(iFile) = 'g c th p1 p2 p3 NX NY NZ R'
    plot_type(iFile) = '3d_out'
    plot_range(1:5:2, iFile) = CoordMin_D
    plot_range(2:6:2, iFile) = CoordMax_D

    StringTest = 'write_plot write_plot_idl'

    call write_plot(iFile)

  end subroutine write_3d_plot
  !============================================================================

  ! Collectes los array from all processors, only useful for testing
  subroutine collect_los_segments(NameOutputFile, DoTestIn)

    use ModIoUnit, ONLY: UnitTmp_
    use ModSpectrumLos, ONLY: LosX_, LosY_, LosZ_, Ds_
    use ModVarIndexes, ONLY: NameVar_V, nVar, Rho_, p_, Pe_
    use ModUtilities, ONLY: join_string

    character(len=*), intent(in), optional :: NameOutputFile
    logical, intent(in), optional :: DoTestIn

    integer :: nState, nPixelAll, nPixelProc_P(0:nProc-1)
    integer :: i, j, iPixelProc, nSeg, iError
    ! only useful on proc 0
    integer :: nLosSegMax, nPoint, iStatus_I(mpi_status_size) 
    real, allocatable :: StatePixSegAll_VII(:,:,:)
    integer, allocatable :: nLosSegAll_I(:), nTmpLosSeg_I(:)
    character(1) :: c
    !--------------------------------------------------------------------------
    character(len=200) NameTmp
    logical :: DoTest
    character(len=*), parameter :: NameSub = 'collect_los_segments'

    call timing_start(NameSub)

    if (present(DoTestIn)) then
       DoTest = DoTestIn
    else
       DoTest = .false.
    endif

    ! get number of pixels stored on other processors
    nState = size(StatePixelSegProc_VII,1)
    nPixelAll = size(iPixelProcInfo_II,2)
    do i = 0, nProc-1
       nPixelProc_P(i) = count(iPixelProcInfo_II(1,:) == i)
    enddo

    ! collect number of los segments on other processors
    if (iProc == 0) then
       allocate(nLosSegAll_I(nPixelAll))
       do i = 0, nProc-1
          allocate(nTmpLosSeg_I(nPixelProc_P(i)))
          if (i /= 0) then
             call mpi_recv(nTmpLosSeg_I, nPixelProc_P(i), MPI_INTEGER, i, &
                  0, iComm, iStatus_I, iError)
          else
             nTmpLosSeg_I = nLosSeg_I
          endif
          do j = 1, nPixelAll
             if (iPixelProcInfo_II(1,j) /= i) CYCLE
             nLosSegAll_I(j) = nTmpLosSeg_I(iPixelProcInfo_II(2,j))
          enddo
          deallocate(nTmpLosSeg_I)
       enddo
    else
       call mpi_send(nLosSeg_I,nPixelProc,MPI_INTEGER,0,0,iComm,iError)
    endif

    ! collect data cube stored on other processors
    if (iProc == 0) then
       nLosSegMax = maxval(nLosSegAll_I)
       allocate(StatePixSegAll_VII(nState,nLosSegMax,nPixelAll))
    endif
    do i = 1, nPixelAll
       ! local pixel position within processor
       iPixelProc = iPixelProcInfo_II(2,i)   
       if (iProc == 0) then
          nSeg = nLosSegAll_I(i)
          if (iPixelProcInfo_II(1,i) == 0) then
             StatePixSegAll_VII(:,1:nSeg,i) = &
                  StatePixelSegProc_VII(:,1:nSeg,iPixelProc)
          else
             call mpi_recv(StatePixSegAll_VII(:,1:nSeg,i), nState*nSeg, &
                  MPI_REAL, iPixelProcInfo_II(1,i), 0, iComm, &
                  iStatus_I, iError)
          endif
       else
          if (iPixelProcInfo_II(1,i) /= iProc) CYCLE
          nSeg = nLosSeg_I(iPixelProc)
          call mpi_send(StatePixelSegProc_VII(:,1:nSeg,iPixelProc), &
               nState*nSeg, MPI_REAL, 0, 0, iComm, iError)
       endif
    enddo

    if (DoTest .and. iProc == 0) then
       write(*,'(2A3,3A8,4A10)') 'i','j','x','y','z','ds','Rho','p','Pe'
       do i = 1, nPixelAll; do j = 1, nLosSegAll_I(i)
          write(*,'(2I3,3F8.4,4E10.3)') i, j, &
               StatePixSegAll_VII([LosX_,LosY_,LosZ_,Ds_,Rho_,p_,Pe_],j,i)
       enddo; enddo
    endif

    ! output los segments for testing
    ! Binary VTK file, about half the of ASCII and faster to read
    if (iProc == 0 .and. present(NameOutputFile)) then
       nPoint = sum(nLosSegAll_I)
       open(unit=UnitTmp_, file=NameOutputFile, status='replace', &
            access='stream', form='unformatted', convert='big_endian')
       write(UnitTmp_) '# vtk DataFile Version 2.0', new_line(c)
       write(UnitTmp_) 'LOS segments', new_line(c)
       write(UnitTmp_) 'BINARY', new_line(c)
       write(UnitTmp_) 'DATASET UNSTRUCTURED_GRID', new_line(c)
       write(NameTmp,'(A,I0,A)') 'POINTS ',nPoint,' double'
       write(UnitTmp_) trim(NameTmp), new_line(c)
       do i = 1, nPixelAll
          do j = 1, nLosSegAll_I(i)
             write(UnitTmp_) StatePixSegAll_VII(LosX_:LosZ_,j,i)
          enddo
       enddo
       write(NameTmp,'(A,I0," ",I0)') 'CELLS ',nPoint,2*nPoint
       write(UnitTmp_) NameTmp, new_line(c)
       j = 1
       do i = 0, nPoint-1
          write(UnitTmp_) j, i
       enddo
       write(NameTmp,'(A,I0)') 'CELL_TYPES ',nPoint
       write(UnitTmp_) NameTmp, new_line(c)
       do i = 0, nPoint-1
          write(UnitTmp_) j
       enddo
       ! Ideally I should output also the vectors. But these are unstructured
       ! points not even in correct order (with respect to los length), I will
       ! not be able to plot anything useful without first do a triangulation
       ! anyways. Can include it later if needed.
       close(UnitTmp_)
    endif

    ! clean up
    if (iProc == 0) then
       deallocate(nLosSegAll_I, StatePixSegAll_VII)
    endif

    call timing_stop(NameSub)
  end subroutine collect_los_segments
  !============================================================================

  subroutine convert_units
    use ModPhysics, ONLY: nIoUnit, No2Si_V, Io2Si_V, &
         UnitX_, UnitRho_, UnitRhoU_, UnitB_, UnitP_, UnitEnergyDens_
    use ModVarIndexes, ONLY: NameVar_V, Rho_, RhoUx_, RhoUz_, Bx_, Bz_, &
         p_, Pe_, Ppar_, WaveFirst_, WaveLast_
    use ModSpectrumLos, ONLY: Ds_
    real :: Input2Si_V(nIoUnit)

    !--------------------------------------------------------------------------
    if (trim(NameUnitInput) == 'normalized variables') then
       Input2Si_V = No2Si_V
    else
       Input2Si_V = Io2Si_V
    endif

    StatePixelSegProc_VII(Rho_,:,:) = &
         StatePixelSegProc_VII(Rho_,:,:)*Input2Si_V(UnitRho_)
    StatePixelSegProc_VII(RhoUx_:RhoUz_,:,:) = &
         StatePixelSegProc_VII(RhoUx_:RhoUz_,:,:)*Input2Si_V(UnitRhoU_)
    StatePixelSegProc_VII(Bx_:Bz_,:,:) = &
         StatePixelSegProc_VII(Bx_:Bz_,:,:)*Input2Si_V(UnitB_)
    StatePixelSegProc_VII(p_,:,:) = &
         StatePixelSegProc_VII(p_,:,:)*Input2Si_V(UnitP_)
    if (NameVar_V(Pe_) == 'Pe') &
         StatePixelSegProc_VII(Pe_,:,:) = &
         StatePixelSegProc_VII(Pe_,:,:)*Input2Si_V(UnitP_)
    if (NameVar_V(Ppar_) == 'Ppar') &
         StatePixelSegProc_VII(Ppar_,:,:) = &
         StatePixelSegProc_VII(Ppar_,:,:)*Input2Si_V(UnitP_)
    if (NameVar_V(WaveFirst_) == 'I01') &
         StatePixelSegProc_VII(WaveFirst_:WaveLast_,:,:) = &
         StatePixelSegProc_VII(WaveFirst_:WaveLast_,:,:) &
         *Input2Si_V(UnitEnergyDens_)
    StatePixelSegProc_VII(Ds_,:,:) = &
         StatePixelSegProc_VII(Ds_,:,:)*Input2Si_V(UnitX_)

  end subroutine convert_units
  !============================================================================

  subroutine collect_save_spectrum(NameOutputFile)
    use ModSpectrum, ONLY: SpectrumTable_I
    use ModPlotFile, ONLY: save_plot_file

    character(len=*), intent(in) :: NameOutputFile

    ! changed from real4 for testing purposes
    character(len=5) :: TypeFileSpectrum = 'ascii' 
    character(len=200) :: StringHeaderSpectrum = '[A] [erg sr^-1 cm^-2 A^-1]'
    real, allocatable :: Intensity_III(:,:,:), CoordWave_I(:)
    integer :: iWvlIntv, nWvlIntv, iWave, nWaveBin, nWaveAll
    integer :: iPix, jPix, iPixelAll, iPixelProc, iProcSend
    integer :: iStatus_I(mpi_status_size)
    character(len=*), parameter:: NameSub = 'collect_save_spectrum'
    !--------------------------------------------------------------------------

    call timing_start(NameSub)

    nWvlIntv = size(SpectrumTable_I)

    ! allocate array for all pixels
    if (iProc == 0) then
       nWaveAll = 0
       do iWvlIntv = 1,nWvlIntv
          nWaveAll = nWaveAll + SpectrumTable_I(iWvlIntv)%nBin
       end do
       allocate( &
            Intensity_III(nWaveAll,nPixel_I(1),nPixel_I(2)), &
            CoordWave_I(nWaveAll))
    endif

    ! message pass to collect Spectrum results
    iWave = 0
    do iWvlIntv = 1, nWvlIntv
       nWaveBin = SpectrumTable_I(iWvlIntv)%nBin
       if (iProc == 0) then
          CoordWave_I(iWave+1:iWave+nWaveBin) = &
               SpectrumTable_I(iWvlIntv)%SpectrumGrid_I(1:nWaveBin)
       endif

       iPixelAll = 0
       do jPix = 1, nPixel_I(2); do iPix = 1, nPixel_I(1)
          iPixelAll = iPixelAll + 1
          iProcSend = iPixelProcInfo_II(1,iPixelAll)
          iPixelProc = iPixelProcInfo_II(2,iPixelAll)

          if (iProc == 0) then
             if (iProcSend == 0) then
                Intensity_III(iWave+1:iWave+nWaveBin,iPix,jPix) = &
                     SpectrumTable_I(iWvlIntv)%Spectrum_II(iPixelProc,:)
             else
                call mpi_recv(Intensity_III(iWave+1:iWave+nWaveBin,iPix,jPix),&
                     nWaveBin, MPI_REAL, iProcSend, 0, iComm, &
                     iStatus_I, iError)
             endif
          else
             if (iProcSend /= iProc) CYCLE
             call mpi_send( &
                  SpectrumTable_I(iWvlIntv)%Spectrum_II(iPixelProc,:), &
                  nWaveBin, MPI_REAL, 0, 0, iComm, iError)
          endif
       enddo; enddo

       iWave = iWave + nWaveBin
    enddo

    if (iProc == 0) then
       ! for testing purpose create 1d output
       if (nPixel_I(1)*nPixel_I(2) == 1) then
          call save_plot_file(NameFile = NameOutputFile, &
               TypeFileIn     = TypeFileSpectrum,        &
               StringHeaderIn = StringHeaderSpectrum,    &
               NameVarIn      = "wavelength flux",       &
               Coord1In_I     = CoordWave_I,             &
               VarIn_I        = Intensity_III(:,1,1))
          deallocate(Intensity_III, CoordWave_I)
       else
          call save_plot_file(NameFile = NameOutputFile, &
               TypeFileIn     = TypeFileSpectrum,        &
               StringHeaderIn = StringHeaderSpectrum,    &
               NameVarIn      = "wavelength x y flux",   &
               Coord1In_I     = CoordWave_I,             &
               Coord2In_I     = PixelPosA_I,             &
               Coord3In_I     = PixelPosB_I,             &
               VarIn_III      = Intensity_III)
          deallocate(Intensity_III, CoordWave_I)
       endif
    endif

    call timing_stop(NameSub)
  end subroutine collect_save_spectrum
  !============================================================================

end program spectrum
!==============================================================================
! The following subroutines are here for compilation of the stand alone code.
! The subroutines and functions below are defined in srcInterface for SWMF,
! but they still need to get compiled in stand-alone mode.
subroutine get_from_spher_buffer_grid(Xyz_D,nVar,State_V)
  implicit none
  real,dimension(3),intent(in)::Xyz_D
  integer,intent(in)::nVar
  real,dimension(nVar)::State_V
  !----------------------------------------------------------------------------
  call stop_mpi( &
       'ERROR: get_from_spher_buffer_grid is for SWMF')
end subroutine get_from_spher_buffer_grid
!==============================================================================
subroutine plot_buffer(iFile)
  implicit none
  integer, intent(in)::iFile
  !----------------------------------------------------------------------------
  call stop_mpi( &
       'ERROR: plot_buffer is for SWMF')
end subroutine plot_buffer
!==============================================================================
subroutine read_ih_buffer(y,z,State_V)
  real :: y, z, State_V(8)
  !----------------------------------------------------------------------------
  call stop_mpi('ERROR: read_ih_buffer is for SWMF')
end subroutine read_ih_buffer
!==============================================================================
subroutine read_pw_buffer(FaceCoords_D,nVar,FaceState_V)
  real, intent(in) :: FaceCoords_D(3)
  integer, intent(in) :: nVar
  real, intent(inout) :: FaceState_V(nVar)
  !----------------------------------------------------------------------------
  call stop_mpi('ERROR: read_pw_buffer is for SWMF')
end subroutine read_pw_buffer
!==============================================================================

