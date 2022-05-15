!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModWritePlotLos
#ifdef _OPENACC
  use ModUtilities, ONLY: norm2
#endif
  use BATL_lib, ONLY: &
       test_start, test_stop, iProc, nProc, iComm
  use ModBatsrusUtility, ONLY: &
       barrier_mpi, stop_mpi, get_date_time, get_time_string

  implicit none

  private ! except

  public:: write_plot_los

contains
  !============================================================================
  subroutine write_plot_los(iFile)

    ! Integrate some quantities along several lines of sight and
    ! create a 2D image of the integrated quantities.
    ! The viewing point can be inifinitely far or at a finite distance.
    ! Applications include integrating density,
    ! creating a synthetic coronagraph image of Thomson scattered
    ! white light by integrating light scattered in the line of sight.
    !
    ! The algorithm loops over all blocks per processor (in parallel)
    ! and over lines of sight and then the results obtained on the
    ! processors are added up.
    !
    ! Written by Chip Manchester, KC Hansen
    !         some improvements by Gabor Toth
    !         some changes by Noe Lugaz
    !
    ! July     2001
    ! January  2002 modified for improved image plane
    ! December 2003 fixed sign error in scattering coefficient bLos
    ! January  2004 fix to accept 0 in LOS vector
    ! January  2004 fixed declaration for norm_los(3), XyzPix_D(3)
    ! February 2004 fix integration and make 2nd order accurate
    !               fix save_file in main.f90 update ghost cells
    !               include forgotten StringPlotParam=StringPlotParam_I(iFile)
    !               flags for PB and LW and limb darkening parameter
    !               improved block-line distance calculation
    !               fix IDL output and plot filenames
    !               use dynamic arrays and contained subroutines
    !               exclude whole block if outside LOS image
    !               simplify block-line distance calculation
    !               simplify body-line distance calculation
    !               moved plot variable loop inside line integration
    !               dimensionalize some of the plot variables
    ! January  2006 compatibility with framework
    !               rotation of the coordinates to HGI
    !               wide-angle line-of-sight (for STEREO)
    !               change in the parameters: satellite_position,
    !               OffsetAngle_I
    !               Cartesian grid and circular image centered
    !               at the Sun (no offset angle)
    ! March    2009 Allow integration of functions different from rho
    ! Sept     2009 Edit by Cooper Downs: Added integration method
    !               for spherical geometry,
    !               also added EUV (3-filters)
    !               and Soft-Xray synthesis capability

    use ModMain,    ONLY : nI, nJ, nK, nStep, tSimulation, Unused_B, &
         IsTimeAccurate, nBlock, NameThisComp, TypeCoordSystem,            &
         UseBody, StartTime, iStartTime_I, rLowerModel, rUpperModel
    use ModGeometry, ONLY: &
         Coord111_DB, nMirror_D, RadiusMin, rMin_B
    use ModPhysics, ONLY : No2Io_V, UnitX_, No2Si_V, UnitN_, rBody, &
         UnitTemperature_
    use ModIO
    use ModIoUnit, ONLY: UnitTmp_
    use ModAdvance, ONLY : State_VGB
    use ModNumConst, ONLY : cTiny, cUnit_DD, cTolerance
    use ModMpi
    use CON_axes, ONLY : transform_matrix
    use ModCoordTransform, ONLY : rot_matrix_z, cross_product
    use ModUtilities, ONLY: lower_case, split_string, join_string, &
         open_file, close_file
    use ModPlotFile, ONLY: save_plot_file
    use ModWritePlot, ONLY: set_plot_scalars
    use ModLookupTable, ONLY: i_lookup_table, interpolate_lookup_table, Table_I
    use BATL_lib, ONLY: Xyz_DGB, CellSize_DB, find_grid_block, &
         IsCartesianGrid, IsCartesian, IsRzGeometry
    use ModSatelliteFile, ONLY: nSatellite, NameSat_I, XyzSat_DI,  &
         set_satellite_positions
    use ModSpectrum, ONLY : spectrum_read_table, spectrum_calc_flux, &
         clean_mod_spectrum

    ! Arguments

    integer, intent(in) :: iFile
    ! Misc: for using MPI
    integer :: iError

    ! File specific parameters
    integer :: nPix_D(2) ! Narrowband images have nPix*nPix pixels
    ! Spectrum images are rectangular
    real    :: aOffset, bOffset, rSizeImage, rSizeImage2, &
         HalfSizeImage_D(2),rOccult, rOccult2, OffsetAngle

    ! Plot variables
    integer, parameter :: MaxParam=10
    real, allocatable :: ImagePe_VIII(:,:,:,:), Image_VIII(:,:,:,:)

    real :: Param_I(MaxParam)
    character (len=20) :: NameParam_I(MaxParam)
    character (len=20) :: NamePlotVar_V(MaxPlotvarLos)
    character (len=20) :: NameVar

    integer :: nEqpar, nPlotVar
    integer :: iPix, jPix             ! indexes of the pixel
    real    :: aPix, bPix             ! coordinates of pixel in the image frae
    real    :: ImageCenter_D(3)       ! 3D coordinates of the center of image
    real    :: aUnit_D(3), bUnit_D(3) ! unit vectors for the image coordinates
    real    :: LosPix_D(3)            ! unit vector from observer to pixel
    real    :: XyzPix_D(3)            ! pixel location in 3D
    real    :: rBlockSize, rBlockCenter
    real    :: SizePix_D(2), r2Pix
    real    :: BlockDistance, ObsDistance, Ratio
    real    :: XyzBlockCenter_D(3), CellSize_D(3), aBlockCenter, bBlockCenter
    real    :: XyzBlockStart_D(3), XyzBlockSign_D(3)=1.0, CoordMinBlock_D(3)

    real    :: FromObs_DD(3,3)
    real    :: Los_D(3), ObsPos_D(3)

    ! rInner in IH and rOuter in SC should be identical!
    ! rInner in SC should be smaller than the occulting radius
    ! rInner in IH should be larger than the inner boundary radius
    ! rOuter in SC should be smaller than the size of the domain
    real :: rInner, rInner2, rOuter, rOuter2

    character(len=500) :: NameAllVar, StringHeadLine
    character(len=500) :: StringUnitTec, StringUnitIdl
    character(len=5)   :: StringExtension
    character(len=40)  :: StringFormat

    ! extra variables needed for auxiliarry data writing with tec output
    ! (style copied from write_plot_tec)
    character(len=23) :: StringDateTime0, StringDateTime
    character(len=80) :: StringFormatTime
    character(len=80) :: StringTmp
    integer           :: iTime_I(7)

    ! block and variable Indices
    integer :: iBlock, iMirror, jMirror, kMirror, iVar

    logical :: DoTiming = .false., DoCheckBlock
    logical :: UseScattering, UseRho

    ! variables added for sph geometry
    logical :: UseEuv,UseSxr

    ! XyzBlockcenter changes, so need fixed one
    real :: FixedXyzBlockCenter_D(3)

    logical :: IsAlignedZ = .false.

    integer :: iTableEUV = -1, iTableSXR= -1

    ! variables for reading in a generalized table
    logical :: UseTableGen = .false.
    integer :: iTableGen = -1
    character (len=20) :: NameTableVar_V(MaxPlotvarLos)
    integer :: nTableVar
    real, allocatable :: InterpValues_I(:)

    integer :: iSat, iSatLoop
    integer:: iProcFound

    ! SPECTRUM - DEM/EM calculation
    logical            :: UseDEM = .false., UseSpm = .false.
    integer, parameter :: DEM_ = 1, EM_ = 2
    integer            :: iTe = 1, nLogTeDEM = 1

    ! SPECTRUM - flux/nbi calculation
    logical            :: UseFlux = .false., UseNbi = .false.
    integer            :: nLambda = 1
    real               :: LosDir_D(3)
    real,allocatable   :: Spectrum_I(:)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_plot_los'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Initialize stuff

    call timing_start(NameSub)

    ! Set rInner and rOuter depending on component
    select case(NameThisComp)
    case('SC')
       rInner = max(rBody, RadiusMin)
       rOuter = rUpperModel
    case('IH')                  ! To split the intergral span between SC and IH
       rInner = max(rBody, rLowerModel)
       rOuter = 1000.0
    case('GM')
       rInner = 0.0 ! needed for comet applications
       rOuter = 1e30
    end select
    rInner2 = rInner**2
    rOuter2 = rOuter**2

    ! Do we calculate SPECTRUM-DEM/EM or flux or narroband image?
    UseSpm = index(TypePlot_I(iFile),'spm')>0
    UseDEM = index(TypePlot_I(iFile),'dem')>0
    UseFlux = index(TypePlot_I(iFile),'fux')>0
    UseNbi = index(TypePlot_I(iFile),'nbi')>0

    if(NameThisComp == 'GM')then
       ! Do not convert to HGI
       FromObs_DD = cUnit_DD
    else
       if(UseSpm)then
          ! Get coordinate transformation matrix:
          FromObs_DD = transform_matrix(tSimulation, &
               TypeCoordPlot_I(iFile), TypeCoordSystem)
       else
          ! Convert to HGI
          FromObs_DD = transform_matrix(tSimulation,'HGI', TypeCoordSystem)
       end if
    end if
    iSat = 0

    if(UseFlux .or. UseNbi)call spectrum_read_table(iFile,UseNbi)

    if(UseSpm)then
       nPix_D          = nint(PlotRange_EI(1:2,iFile)/PlotDx_DI(1:2,iFile))+1
       aOffset         = xOffset_I(iFile)
       bOffset         = yOffset_I(iFile)
       rSizeImage      = 0
       rSizeImage2     = 0
       HalfSizeImage_D = 0.5*PlotRange_EI(1:2,iFile)
       rOccult         = 0
       rOccult2        = rOccult**2
       OffsetAngle     = 0
    else
       ! Set file specific parameters
       nPix_D          = nPixel_I(iFile)
       aOffset         = xOffset_I(iFile)
       bOffset         = yOffset_I(iFile)
       rSizeImage      = rSizeImage_I(iFile)
       rSizeImage2     = rSizeImage**2
       HalfSizeImage_D = rSizeImage
       rOccult         = rOccult_I(iFile)
       rOccult2        = rOccult**2
       OffsetAngle     = OffsetAngle_I(iFile)
    end if

    select case(TypeSatPos_I(iFile))
    case('sta', 'stb', 'earth')

       ! determine the index of the sta/stb satellite file
       do iSatLoop = 1, nSatellite
          if ( NameSat_I(iSatLoop) /= TypeSatPos_I(iFile)) CYCLE
          iSat = iSatLoop
          EXIT
       end do

       ! obtain the current location based on the simulation time
       call set_satellite_positions(iSat)
       ObsPos_DI(:, iFile) = XyzSat_DI(:, iSat)

       ! XyzSat_DI is in the current coordinate system, need to convert
       ! back to HGI
       ObsPos_DI(:, iFile) = matmul(                                   &
            transform_matrix(tSimulation, TypeCoordSystem, 'HGI'), &
            ObsPos_DI(:,iFile) )

    case('none')
       ! do nothing
    case default
       call stop_mpi(NameSub//': unknown TypeSatPos: '//TypeSatPos_I(iFile))
    end select

    ! Rotate observation point from HGI system to the current coordinate system
    ObsPos_D    = matmul(FromObs_DD, ObsPos_DI(:,iFile))
    ObsDistance = norm2(ObsPos_D)
    ! Normalize line of sight vector pointing towards the origin
    Los_D       = -ObsPos_D/ObsDistance

    ! Rotation with offset angle
    Los_D = matmul( rot_matrix_z(OffsetAngle), Los_D)

    ! Observer distance from image plane
    ObsDistance = abs(sum(ObsPos_D*Los_D))

    ! Make zero components slightly different from zero
    where(abs(Los_D) < cTiny) Los_D = sign(cTiny, Los_D)

    ! Pixel size for node based pixel grid
    SizePix_D = 2*HalfSizeImage_D/(nPix_D - 1)

    if(DoTest) then
       write(*,*) 'ObsPos         =', ObsPos_D
       write(*,*) 'Los_D          =', Los_D
       write(*,*) 'HalfSizeImage_D =', HalfSizeImage_D
       write(*,*) 'aOffset,bOffset =', aOffset, bOffset
       write(*,*) 'SizePix_D        =', SizePix_D
       write(*,*) 'nPix_D           =', nPix_D
    end if

    StringUnitTec = ''
    StringUnitIdl = ''

    TypePlot=TypePlot_I(iFile)
    StringPlotVar=StringPlotVar_I(iFile)

    if(DoTest)write(*,*)'iFile=',iFile,' TypePlot_I=',TypePlot, &
         ' form = ',TypePlotFormat_I(iFile)

    call lower_case(StringPlotVar)
    call split_string(StringPlotVar, MaxPlotvarLos, NamePlotVar_V, nPlotVar)
    call set_plot_scalars(iFile,MaxParam, nEqpar,NameParam_I, Param_I)
    ! Initialize table IDs. In this case automatically
    ! UseTableGen = (iTableGen >=0) and analogously for other table
    ! related logicals.
    iTableEuv = -1; iTableSxr = -1; iTableGen =  -1
    ! For generalized Los Table check NamePlotVar_V for string 'tbl'
    UseTableGen = any(NamePlotVar_V(1:nPlotVar)== 'tbl')

    if(UseTableGen) then
       iTableGen = i_lookup_table(trim(NameLosTable_I(iFile)))
       if (iTableGen <=0) &
            call stop_mpi('Need to load #LOOKUPTABLE for TBL response!')
       ! split the variable list string read in the table
       call split_string(Table_I(iTableGen)%NameVar, MaxPlotvarLos, &
            NameTableVar_V, nTableVar)
       ! don't count the x and y table labels as plot variables
       nPlotVar=nTableVar-2
       NamePlotVar_V(1:nPlotVar)=NameTableVar_V(3:nTableVar)

       ! redefine StringPlotVar with correct table info
       call join_string(nPlotVar, NamePlotVar_V, StringPlotVar)

       if(DoTest .and. iProc==0) then
          write(*,*) 'plot variables, UseRho=', trim(StringPlotVar), UseRho
          write(*,*) 'nPlotVar, PlotVarNames_V=', &
               nPlotVar,NamePlotVar_V(1:nPlotVar)
       end if

       ! allocate the vector that will contain the interpolated values
       if(.not.allocated(InterpValues_I)) &
            allocate(InterpValues_I(nPlotVar))

       if(DoTest .and. iProc==0) write(*,*) 'NameVar: ', &
            Table_I(iTableGen)%NameVar
    endif

    if(UseDEM)then
       NameAllVar='x y logTe'
    elseif(UseFlux)then
       NameAllVar='x y lambda'
    elseif(UseNbi)then
       NameAllVar='x y intensity'
    else
       NameAllVar='x y'
    end if
    NameAllVar = trim(NameAllVar)//' '//trim(StringPlotVar)//' ' &
         //StringPlotParam_I(iFile)

    if(DoTest .and. iProc==0) write(*,*) 'NameAllVar: ', NameAllVar

    if(DoTest .and. iProc==0) then
       write(*,*) 'plot variables, UseRho=', StringPlotVar, UseRho
       write(*,*) 'nPlotVar, PlotVarNames_V=', &
            nPlotVar,NamePlotVar_V(1:nPlotVar)
    end if

    ! Get the headers that contain variables names and units
    select case(TypePlotFormat_I(iFile))
    case('tec')
       call get_los_variable_tec(iFile,nPlotVar,NamePlotVar_V,StringUnitTec)
       if(DoTest .and. iProc==0) write(*,*)StringUnitTec
    case('idl')
       call get_los_unit_idl(iFile, nPlotVar, NamePlotVar_V, StringUnitIdl, &
            .false.)
       if(DoTest .and. iProc==0) write(*,*)StringUnitIdl
    case('hdf')
       call get_los_unit_idl(iFile,nPlotVar,NamePlotVar_V,StringUnitIdl, &
            .true.)
       if(DoTest .and. iProc==0) write(*,*)StringUnitIdl
    end select

    if(UseTableGen) then
       StringUnitTec = 'VARIABLES = "X", "Y"'
       do iVar=1, nPlotVar
          StringUnitTec = trim(StringUnitTec) &
               //', "'//trim(NamePlotVar_V(iVar))//'"'
       enddo
       if(DoTest .and. iProc==0) write(*,*)'StringUnitTec: ',StringUnitTec

       if(TypePlotFormat_I(iFile) /= 'hdf') then
          call join_string(nPlotVar, NamePlotVar_V, StringPlotVar)
          StringUnitIdl = 'x y '//StringPlotVar
          if(DoTest .and. iProc==0) write(*,*)'StringUnitIdl: ',StringUnitIdl
       end if
    endif

    ! Create unit vectors aUnit_D and bUnit_D orthogonal to the
    ! central line of sight to setup the coordinate system in the viewing plane
    ! We use cross products of the LOS vector with one of the principal
    ! directions (0,0,1) or (0,1,0) to make sure that the viewing plane is
    ! aligned with the original Cartesian coordinates. In case the viewing
    ! is roughly along the X or Y axis, we want bUnit_D to point along +Z,
    ! for viewing along the Z axis, we want bUnit_D to point along +Y:
    ! a = LOS x (0,0,1), b = a x LOS ensures that b is roughly aligned with +Z
    ! a = LOS x (0,1,0), b = a x LOS ensures that b is roughly aligned with +Y
    if(abs(Los_D(3)) < maxval(abs(Los_D(1:2))))then
       aUnit_D = cross_product(Los_D, [0.,0.,1.])
       IsAlignedZ = .true.
    else
       ! Viewing along the Z axis more or less
       aUnit_D = cross_product(Los_D, [0.,1.,0.])
    end if
    aUnit_D = aUnit_D/norm2(aUnit_D)
    bUnit_D = cross_product(aUnit_D, Los_D)
    bUnit_D = bUnit_D/norm2(bUnit_D)

    ! 3D vector pointing from the origin to the image center
    ImageCenter_D = ObsPos_D + ObsDistance*Los_D &
         + aOffset*aUnit_D + bOffset*bUnit_D

    ! Make offset to be relative to the Sun (and not the projected observer)
    aOffset = dot_product(ImageCenter_D, aUnit_D)
    bOffset = dot_product(ImageCenter_D, bUnit_D)

    ! !! aOffset = aOffset + dot_product(ObsPos_D, aUnit_D)
    if(UseDEM)then
       nLogTeDEM = &
            nint((LogTeMaxDEM_I(iFile)-LogTeMinDEM_I(iFile))/&
            DLogTeDEM_I(IFile))+1
       allocate( &
            ImagePe_VIII(nPlotVar,nPix_D(1),nPix_D(2),nLogTeDEM), &
            Image_VIII(nPlotVar,nPix_D(1),nPix_D(2),nLogTeDEM))
    elseif(UseFlux)then
       nLambda = &
            nint((LambdaMax_I(iFile)-LambdaMin_I(iFile))/DLambda_I(IFile))+1
       allocate( &
            ImagePe_VIII(nPlotVar,nPix_D(1),nPix_D(2),nLambda), &
            Image_VIII(nPlotVar,nPix_D(1),nPix_D(2),nLambda), &
            Spectrum_I(nLambda))
    else
       nLambda = 1
       allocate( &
            ImagePe_VIII(nPlotVar,nPix_D(1),nPix_D(2),1), &
            Image_VIII(nPlotVar,nPix_D(1),nPix_D(2),1))
       if(UseNbi)allocate(Spectrum_I(nLambda))
    end if

    ImagePe_VIII = 0.0

    ! Do we need to apply scattering
    UseScattering = any(NamePlotVar_V(1:nPlotVar) == 'wl') &
         .or.       any(NamePlotVar_V(1:nPlotVar) == 'pb')

    ! Do we need to calc EUV response?
    UseEuv = any(NamePlotVar_V(1:nPlotVar) == 'euv171') &
         .or.       any(NamePlotVar_V(1:nPlotVar) == 'euv195') &
         .or.       any(NamePlotVar_V(1:nPlotVar) == 'euv284')

    ! Do we need to calc Soft X-Trace_DSNB response?
    UseSxr = any(NamePlotVar_V(1:nPlotVar) == 'sxr')

    ! if EUV or SXR calc, then get lookup table info
    if (UseEuv) iTableEUV  = i_lookup_table('euv')
    if (UseSxr) iTableSXR  = i_lookup_table('sxr')

    ! Do we need to calculate density (also for white light and polarization)
    UseRho = UseScattering .or. any(NamePlotVar_V(1:nPlotVar) == 'rho') &
         .or. UseEuv .or. UseSxr .or. UseTableGen .or. UseSpm

    if(DoTiming)call timing_start('los_block_loop')

    if(UseLosSimple .or. .not.IsCartesianGrid)then
       call integrate_image
    else
       ! loop over blocks
       do iBlock = 1, nBlock

          if (Unused_B(iBlock)) CYCLE

          CellSize_D = CellSize_DB(:,iBlock)

          do iMirror = 1, nMirror_D(1)
             XyzBlockSign_D(1) = 3 - 2*iMirror
             do jMirror = 1, nMirror_D(2)
                XyzBlockSign_D(2) = 3 - 2*jMirror
                do kMirror = 1, nMirror_D(3)
                   XyzBlockSign_D(3) = 3 - 2*kMirror

                   call integrate_block

                end do    ! kMirror
             end do    ! jMirror
          end do    ! iMirror
       end do       ! iBlock loop
    end if

    if(DoTiming)call timing_stop('los_block_loop')
    !   if(TypePlotFormat_I(iFile) /= 'hdf') then
    !       ! add up the pixels from all PE-s to root proc
    if(nProc > 1)then
       if(UseDEM)then
          call MPI_REDUCE(ImagePe_VIII, Image_VIII, &
               nPix_D(1)*nPix_D(2)*nPlotVar*nLogTeDEM, &
               MPI_REAL, MPI_SUM, 0, iComm, iError)
       elseif(UseFlux)then
          call MPI_REDUCE(ImagePe_VIII, Image_VIII, &
               nPix_D(1)*nPix_D(2)*nPlotVar*nLambda, &
               MPI_REAL, MPI_SUM, 0, iComm, iError)
          call clean_mod_spectrum
       else
          call MPI_REDUCE(ImagePe_VIII, Image_VIII, nPix_D(1)*nPix_D(2)*&
               nPlotVar, MPI_REAL, MPI_SUM, 0, iComm, iError)
       end if
    else
       Image_VIII = ImagePe_VIII
    end if

    if(iProc==0) then

       if(IsDimensionalPlot_I(iFile)) call dimensionalize_plotvar_los

       if(DoTiming)call timing_start('los_save_plot')

       select case(TypePlotFormat_I(iFile))
       case('tec')
          StringExtension='.dat'
       case('idl')
          StringExtension='.out'
       case('hdf')
          StringExtension='.batl'
       end select

       if (iFile-plot_ > 9) then
          StringFormat='("' // trim(NamePlotDir) // '",a,i2,a,i7.7,a)'
       else
          StringFormat='("' // trim(NamePlotDir) // '",a,i1,a,i7.7,a)'
       end if

       ! the plot time is stored in the hdf5 files and displayed in VisIt.
       ! if you don not include it in the NameFile VisIt will automacially
       ! group all the los files.
       if(IsTimeAccurate .and. TypePlotFormat_I(iFile) /= 'hdf')then
          call get_time_string
          write(NameFile,StringFormat) &
               trim(TypePlot)//"_",&
               iFile-plot_,"_t"//trim(StringDateOrTime)//"_n",nStep,&
               StringExtension
       else
          write(NameFile,StringFormat) &
               trim(TypePlot)//"_",&
               iFile-plot_,"_n",nStep,StringExtension
       end if

       ! write header file

       if(TypePlotFormat_I(iFile)=='tec') then
          call open_file(FILE=NameFile)

          write(UnitTmp_,*) 'TITLE="BATSRUS: Synthetic Image"'
          write(UnitTmp_,'(a)')trim(StringUnitTec)
          write(UnitTmp_,*) 'ZONE T="LOS Image"', &
               ', I=',nPix_D(1),', J=',nPix_D(2),', K=1, F=POINT'

          ! Write Auxilliary header info, which is useful for EUV images.
          ! Makes it easier to identify, and automatically process synthetic
          ! images from different instruments/locations
          StringFormatTime = &
               '(i4.4,"/",i2.2,"/",i2.2,"T",i2.2,":",i2.2,":",i2.2,".",i3.3)'
          call get_date_time(iTime_I)
          write(StringDateTime0,StringFormatTime) iStartTime_I
          write(StringDateTime ,StringFormatTime) iTime_I

          ! TIMEEVENT
          write(UnitTmp_,'(a,a,a)') &
               'AUXDATA TIMEEVENT="',trim(StringDateTime),'"'

          ! TIMEEVENTSTART
          write(UnitTmp_,'(a,a,a)') &
               'AUXDATA TIMEEVENTSTART="',trim(StringDateTime0),'"'

          ! TIMESECONDSABSOLUTE
          ! time in seconds since 1965 Jan 01 T00:00:00.000 UTC
          write(StringTmp,'(E20.13)')StartTime+tSimulation
          write(UnitTmp_,'(a,a,a)') &
               'AUXDATA TIMESECONDSABSOLUTE="',trim(adjustl(StringTmp)),'"'

          ! ITER
          write(StringTmp,'(i12)')nStep
          write(UnitTmp_,'(a,a,a)') &
               'AUXDATA ITER="',trim(adjustl(StringTmp)),'"'

          ! NAMELOSTABLE
          if (UseTableGen) write(UnitTmp_,'(a,a,a)') &
               'AUXDATA NAMELOSTABLE="',trim(NameLosTable_I(iFile)),'"'

          ! HGIXYZ
          write(StringTmp,'(3(E14.6))')ObsPos_DI(:,iFile)
          write(UnitTmp_,'(a,a,a)') &
               'AUXDATA HGIXYZ="',trim(adjustl(StringTmp)),'"'

          ! Write point values
          do iPix = 1, nPix_D(1)
             aPix = (iPix - 1) * SizePix_D(1) - HalfSizeImage_D(1)
             do jPix = 1, nPix_D(2)
                bPix = (jPix - 1) * SizePix_D(2) - HalfSizeImage_D(2)

                if (IsDimensionalPlot_I(iFile)) then
                   write(UnitTmp_,fmt="(30(E14.6))") &
                        aPix*No2Io_V(UnitX_), bPix*No2Io_V(UnitX_), &
                        Image_VIII(1:nPlotVar,iPix,jPix,1)
                else
                   write(UnitTmp_,fmt="(30(E14.6))") aPix, bPix, &
                        Image_VIII(1:nPlotVar,iPix,jPix,1)
                end if

             end do
          end do
          call close_file
       else
          ! description of file contains units, physics and dimension
          StringHeadLine = 'LOS integrals'
          ! Write Auxilliary header info, which is useful for EUV images.
          ! Makes it easier to identify, and automatically process synthetic
          ! images from different instruments/locations

          write(StringFormatTime,*)&
               '(i4.4,"/",i2.2,"/",i2.2,"T",i2.2,":",i2.2,":",i2.2,".",i3.3)'
          call get_date_time(iTime_I)
          write(StringDateTime0,StringFormatTime) iStartTime_I
          write(StringDateTime ,StringFormatTime) iTime_I

          ! Optimize the amount of information required in the header

          ! TIMEEVENT and TIMEEVENTSTART
          StringHeadLine = trim(StringHeadline)// &
               ' TIMEEVENT='//trim(StringDateTime)// &
               ' TIMEEVENTSTART='//StringDateTime0

          ! TIMESECONDSABSOLUTE
          ! time in seconds since 1965 Jan 01 T00:00:00.000 UTC
          ! write(StringTmp,'(E20.13)')StartTime+tSimulation
          ! StringHeadLine = trim(StringHeadLine)//&
          !      '_TIMESECONDSABSOLUTE='//adjustl(StringTmp)

          if (UseTableGen) then
             ! NAMELOSTABLE_I
             StringHeadLine = trim(StringHeadLine)//' NAMELOSTABLE='//&
                  NameLosTable_I(iFile)
          endif

          ! Set image size and dimensionalize if necessary
          aPix = HalfSizeImage_D(1)
          bPix = HalfSizeImage_D(2)
          if (IsDimensionalPlot_I(iFile)) then
             aPix = aPix * No2Io_V(UnitX_)
             bPix = bPix * No2Io_V(UnitX_)
             aOffset = aOffset*No2Io_V(UnitX_)
             bOffset = bOffset*No2Io_V(UnitX_)
          end if

          select case(TypePlotFormat_I(iFile))
          case('idl')
             if(UseDEM)then
                StringHeadLine= 'DEM integrals '// &
                     ' TIMEEVENT='//trim(StringDateTime)// &
                     ' TIMEEVENTSTART='//StringDateTime0// &
                     ' '//StringUnitIdl

                call save_plot_file(NameFile, &
                     TypeFileIn = TypeFile_I(iFile), &
                     StringHeaderIn = StringHeadLine, &
                     nStepIn = nStep, &
                     TimeIn = tSimulation, &
                     ParamIn_I = Param_I(1:neqpar), &
                     NameVarIn = NameAllVar, &
                     NameUnitsIn = StringUnitIdl,&
                     nDimIn = 3, &
                     CoordMinIn_D = &
                     [aOffset-aPix, bOffset-bPix, LogTeMinDEM_I(iFile)], &
                     CoordMaxIn_D = &
                     [aOffset+aPix, bOffset+bPix, LogTeMaxDEM_I(iFile)], &
                     VarIn_VIII = Image_VIII(:,:,:,:))
             elseif(UseNbi)then
                StringHeadLine= 'NBI integrals '// &
                     ' TIMEEVENT='//trim(StringDateTime)// &
                     ' TIMEEVENTSTART='//StringDateTime0// &
                     ' '//StringUnitIdl
                call save_plot_file(NameFile, &
                     TypeFileIn = TypeFile_I(iFile), &
                     StringHeaderIn = StringHeadLine, &
                     nStepIn = nStep, &
                     TimeIn = tSimulation, &
                     ParamIn_I = Param_I(1:neqpar), &
                     NameVarIn = NameAllVar, &
                     NameUnitsIn = StringUnitIdl,&
                     nDimIn = 2, &
                     CoordMinIn_D = &
                     [aOffset-aPix, bOffset-bPix], &
                     CoordMaxIn_D = &
                     [aOffset+aPix, bOffset+bPix], &
                     VarIn_VII = Image_VIII(:,:,:,1))
             elseif(UseFlux)then
                StringHeadLine= 'Spectrum flux '// &
                     ' TIMEEVENT='//trim(StringDateTime)// &
                     ' TIMEEVENTSTART='//StringDateTime0// &
                     ' '//StringUnitIdl
                call save_plot_file(NameFile, &
                     TypeFileIn = TypeFile_I(iFile), &
                     StringHeaderIn = StringHeadLine, &
                     nStepIn = nStep, &
                     TimeIn = tSimulation, &
                     ParamIn_I = Param_I(1:neqpar), &
                     NameVarIn = NameAllVar, &
                     NameUnitsIn = StringUnitIdl,&
                     nDimIn = 3, &
                     CoordMinIn_D = &
                     [aOffset-aPix, bOffset-bPix, LambdaMin_I(iFile)], &
                     CoordMaxIn_D = &
                     [aOffset+aPix, bOffset+bPix, LambdaMax_I(iFile)], &
                     VarIn_VIII = Image_VIII(:,:,:,:))
             else
                call save_plot_file(NameFile, &
                     TypeFileIn = TypeFile_I(iFile), &
                     StringHeaderIn = StringHeadLine, &
                     nStepIn = nStep, &
                     TimeIn = tSimulation, &
                     ParamIn_I = Param_I(1:neqpar), &
                     NameVarIn = NameAllVar, &
                     nDimIn = 2, &
                     CoordMinIn_D = [-aPix, -aPix], &
                     CoordMaxIn_D = [+aPix, +aPix], &
                     VarIn_VII = Image_VIII(:,:,:,1))
             endif
          case('hdf')
             call save_plot_file(NameFile, &
                  TypeFileIn = 'hdf5', &
                  StringHeaderIn = StringHeadLine, &
                  nStepIn = nStep, &
                  TimeIn = tSimulation, &
                  ParamIn_I = Param_I(1:neqpar), &
                  NameVarIn_I = NamePlotVar_V, &
                  NameUnitsIn = StringUnitIdl,&
                  nDimIn = 2, &
                  CoordMinIn_D = [-aPix, -aPix], &
                  CoordMaxIn_D = [+aPix, +aPix], &
                  VarIn_VII = Image_VIII(:,:,:,1))
          end select
       end if
    end if  ! iProc==0
    if(DoTiming)call timing_stop('los_save_plot')

    call barrier_mpi

    deallocate(ImagePe_VIII, Image_VIII)
    if(UseFlux .or. UseNbi)deallocate(Spectrum_I)
    if(UseTableGen) deallocate(InterpValues_I)

    if(DoTest)write(*,*) NameSub,' finished'

    call timing_stop(NameSub)

    call test_stop(NameSub, DoTest)
  contains
    !==========================================================================
    subroutine integrate_image
      use ModFieldLineThread, ONLY: UseFieldLineThreads, rChromo=>rBody,      &
           get_tr_los_image, DoPlotThreads, UseTRCorrection
      real:: Distance
      real:: d=0.0, dMirror= 0.0, dChromo = -1.0, LosDotXyzPix, XyzPix2, &
           Discriminant = -1.0, DiscrChromo = -1.0, SqrtDiscr
      real:: XyzIntersect_D(3), XyzTR_D(3)
      !------------------------------------------------------------------------

      ! Loop over pixels
      do jPix = 1, nPix_D(2)

         ! Y position of the pixel on the image plane
         bPix = (jPix - 1) * SizePix_D(2) - HalfSizeImage_D(2)

         do iPix = 1, nPix_D(1)

            ! X position of the pixel on the image plane
            aPix = (iPix - 1) * SizePix_D(1) - HalfSizeImage_D(1)

            r2Pix = (aPix + aOffset)**2 + (bPix + bOffset)**2
            ! Check if pixel is within occultation radius
            if( r2Pix  <= rOccult2 ) CYCLE

            r2Pix = aPix**2 + bPix**2
            ! Check if pixel is outside the circular region
            if( r2Pix > rSizeImage2 .and. .not. UseSpm )CYCLE

            ! Get the 3D location of the pixel
            XyzPix_D = ImageCenter_D + aPix*aUnit_D + bPix*bUnit_D

            ! Vector from pixel center to observer
            LosPix_D = - XyzPix_D + ObsPos_D
            Distance = norm2(LosPix_D)
            ! Unit vector pointing from pixel center to observer
            LosPix_D = LosPix_D/Distance
            LosDir_D = LosPix_D

            ! Calculate whether there are intersections with the rInner sphere
            ! The LOS line can be written as XyzLine_D = XyzPix_D + d*LosPix_D
            ! If the LOS line intersects with the sphere of radius
            ! rInner+cTiny, then
            ! (rInner+cTiny)^2 = (XyzPix_D + d*LosPix_D)^2
            !                  = XyzPix_D^2 + 2 d XyzPix_D.LosPix_D + d^2
            ! where we use that LosPix_D is a unit vector.
            ! This can be rearranged to
            ! 0 = d^2 + 2 p d + q
            ! solved for d = -p + sqrt(p^2 - q) where only the positive root
            ! is needed, as LosPix_D points toward the observer.
            ! If there is an intersection, we set the starting point to
            ! XyzIntersect_D = XyzPix_D + d*LosPix_D

            ! The discriminant of the equation
            LosDotXyzPix = sum(LosPix_D*XyzPix_D)
            XyzPix2 = sum(XyzPix_D**2)
            Discriminant = LosDotXyzPix**2 &
                 - XyzPix2  + (rInner + cTiny)**2

            if (Discriminant > 0) then
               ! Only consider the intersection facing the observer
               SqrtDiscr = sqrt(Discriminant)
               d = - LosDotXyzPix + SqrtDiscr
               XyzIntersect_D = XyzPix_D + d*LosPix_D
               ! Integrate from the intersection point to observer

               call integrate_line(XyzIntersect_D, Distance - d)

               if(UseFieldLineThreads)then
                  ! The discriminant controlling intersection with
                  ! the chromosphere
                  DiscrChromo = LosDotXyzPix**2 - XyzPix2 + rChromo**2
                  ! Integrate in the other direction too if no intesection
                  LosPix_D = -LosPix_D
                  if(DiscrChromo > 0)then
                     ! Intersection with chromosphere
                     ! facing the observer
                     SqrtDiscr = sqrt(DiscrChromo)
                     dChromo = - LosDotXyzPix + SqrtDiscr
                     call integrate_line(XyzIntersect_D, d - dChromo, &
                          UseThreads = DoPlotThreads)
                     ! LOS ntersection with the top of Transition Region
                     if(UseTRCorrection.and.DoPlotThreads.and.          &
                          (UseEuv .or. UseSxr .or. UseTableGen))then
                        XyzTR_D = XyzIntersect_D + (d - dChromo)*LosPix_D
                        call find_grid_block((rInner + cTiny)*XyzTR_D/  &
                             norm2(XyzTR_D),iProcFound, iBlock)
                        if( iProc==iProcFound)call get_tr_los_image(&
                             Xyz_D = XyzTR_D ,&
                             DirLos_D = LosPix_D,                       &
                             iBlock = iBlock,                           &
                             nPlotVar = nPlotVar,                       &
                             NamePlotVar_V = NamePlotVar_V(1:nPlotVar), &
                             iTableEuv = iTableEuv,                     &
                             iTableSxr = iTableSxr,                     &
                             iTableGen = iTableGen,                     &
                             PixIntensity_V  = ImagePe_VIII(1:nPlotVar,  &
                             iPix, jPix,1))
                     end if
                  else
                     ! Distance between two intersections with the low
                     ! boundary R=rInner: - LosDotXyzPix \pm SqrtDisc
                     dMirror = 2*SqrtDiscr
                     call integrate_line(XyzIntersect_D, dMirror, &
                          UseThreads = DoPlotThreads)
                     XyzIntersect_D = XyzIntersect_D + dMirror*LosPix_D
                     call integrate_line(XyzIntersect_D, 1e30)
                  end if
               end if
            else
               ! No intersection, integrate from pixel to observer
               call integrate_line(XyzPix_D, Distance)

               ! Integrate in the other direction too if no intesection
               LosPix_D = -LosPix_D
               call integrate_line(XyzPix_D, 1e30)
            end if

         end do ! jPix loop
      end do    ! iPix loop

    end subroutine integrate_image
    !==========================================================================
    subroutine integrate_line(XyzStartIn_D, LengthMax, UseThreads)

      ! Integrate variables from XyzStartIn_D in the direction LosPix_D

      use ModGeometry, ONLY: &
           xMinBox, xMaxBox, yMinBox, yMaxBox, zMinBox, zMaxBox
      use ModFieldLineThread, ONLY: &
           IsUniformGrid, dCoord1Uniform, rChromo=>rBody
      use BATL_lib,           ONLY: xyz_to_coord, &
           get_tree_position, CoordMin_D, CoordMax_D, nIJK_D

      real, intent(in):: XyzStartIn_D(3)
      real, intent(in):: LengthMax
      logical, optional, intent(in) :: UseThreads

      real:: Length          ! Total length of integral
      real:: Ds              ! Length of line segment

      real:: XyzLos_D(3)    ! Coordinate of center of line segment
      integer:: iNode, iDimMin = 1
      real, dimension(MaxDim):: XyzStart_D, &
           PositionMin_D, PositionMax_D, &
           CoordMaxBlock_D, CoordBlock_D, CoordSizeBlock_D, &
           CoordSize_D, CoordLos_D, &
           XyzLosNew_D, CoordLosNew_D, dCoord_D

      real, parameter:: StepMax = 1.0, StepMin = 0.5, StepGood = 0.75
      real:: Step, DsTiny
      logical:: IsEdge

      logical, parameter :: DoTest = .false.

      ! DoTest = iPix==200 .and. jPix==200

      character(len=*), parameter:: NameSub = 'integrate_line'
      !------------------------------------------------------------------------
      iDimMin = r_
      if(present(UseThreads))then
         ! Integration through the threaded gap
         ! The part of Trace_DSNB passing through the threaded gap does not
         ! contribute to the integral if UseThreads = .false.
         if(.not.UseThreads)RETURN
         ! In the threaded gap, the radial coordinate is allowed to go beyond
         ! the block boundary and the domain boundary. The criterion for the
         ! Trace_DSNB pass to a new block should ignore this coordinate.
         iDimMin = r_ + 1
      end if
      if(DoTest .and. iProc == 0) then
         write(*,'(2a, 3f10.7, a, f10.7)')NameSub, ' XyzStartIn_D=', &
              XyzStartIn_D, ', Distance = ', norm2(XyzStartIn_D)
         write(*,'(2a, 3f10.7, a, f10.7)')NameSub, ' End point coordinates=',&
              XyzStartIn_D + LengthMax*LosPix_D, ', Distance = ', norm2(&
              XyzStartIn_D + LengthMax*LosPix_D)
      end if

      CoordSize_D = CoordMax_D - CoordMin_D
      DsTiny = cTiny*(xMaxBox-xMinBox + yMaxBox - yMinBox + zMaxBox - zMinBox)

      XyzStart_D = XyzStartIn_D

      ! Initial length of segment
      Ds = DsTiny

      ! Initialize "new" position as the starting point
      XyzLosNew_D = XyzStart_D
      call xyz_to_coord(XyzLosNew_D, CoordLosNew_D)

      ! Initialize block boundaries so that point is surely outside
      CoordMinBlock_D = CoordMax_D
      CoordMaxBlock_D = CoordMin_D

      if(DoTest) write(*,*) NameSub,': initial Ds=',Ds

      Length = -Ds
      LOOPLINE: do
         ! Total length integrated so far
         Length  = Length + Ds

         ! Stop if reached maximum length
         if(Length > LengthMax) EXIT LOOPLINE

         ! Move to new position
         XyzLos_D   = XyzLosNew_D
         CoordLos_D = CoordLosNew_D
         if(.not.present(UseThreads)) then
            ! Stop integration if we reached the edge of the domain
            if(  any(CoordLosNew_D > CoordMax_D) .or. &
                 any(CoordLosNew_D < CoordMin_D)) EXIT LOOPLINE
         end if
         if(Ds <= 0.0)then
            ! To prevent intinite looping
            write(*,*)'ds=', Ds
            call stop_mpi(NameSub//&
                 ': Algorithm failed: zero integration step')
         end if
         if(DoTest)write(*,*) NameSub,' inside: Ds, Length=', Ds, Length

         ! Check if we are still in the same block or not
         if(  any(CoordLos_D(iDimMin:) < CoordMinBlock_D(iDimMin:)) .or. &
              any(CoordLos_D(iDimMin:) > CoordMaxBlock_D(iDimMin:)))then
            if(present(UseThreads))then
               ! Find new block/node, increase the radial coordinate to
               ! put the point above the inner boundary
               call find_grid_block((rInner + cTiny)*XyzLos_D/norm2(XyzLos_D),&
                    iProcFound, iBlock, iNodeOut=iNode)
            else
               ! Find new block/node, increase the radial coordinate to
               ! put the point above the inner boundary
               call find_grid_block(XyzLos_D,&
                    iProcFound, iBlock, iNodeOut=iNode)
            end if
            ! Set block coordinates and the cell size on all processors
            call get_tree_position(iNode, PositionMin_D, PositionMax_D)
            CoordMinBlock_D = CoordMin_D + CoordSize_D*PositionMin_D  ! Start
            CoordMaxBlock_D = CoordMin_D + CoordSize_D*PositionMax_D  ! End
            CoordBlock_D    = 0.5*(CoordMaxBlock_D + CoordMinBlock_D) ! Center
            CoordSizeBlock_D= CoordMaxBlock_D - CoordMinBlock_D    ! Block size
            CellSize_D      = CoordSizeBlock_D / nIjk_D            ! Cell size
            if(present(UseThreads))then
               if(IsUniformGrid)CellSize_D(r_) = dCoord1Uniform
            end if
            if(DoTest)then
               write(*,*)NameSub,': new iBlock=', iBlock
               write(*, '(A, 3E12.5)')NameSub//': CoordMin=', CoordMinBlock_D
               write(*, '(A, 3E12.5)')NameSub//': CoordMax=', CoordMaxBlock_D
            end if

         end if

         ! Check if mid point will be inside the block. If not, reduce Ds
         IsEdge = .false.
         do
            ! Move to the middle of the segment
            XyzLosNew_D = XyzLos_D + 0.5*Ds*LosPix_D
            call xyz_to_coord(XyzLosNew_D, CoordLosNew_D)

            ! Check if midpoint is inside block + 1 cell size
            dCoord_D = abs(CoordLosNew_D - CoordBlock_D)
            if(all(2*dCoord_D(iDimMin:) <= CoordSizeBlock_D(iDimMin:))) EXIT

            ! Reduce Ds but make sure that 2*Ds is still outside.
            Ds = Ds*0.5

            ! Don't integrate this segment if it is very small
            ! Since we took half of Ds, XyzLosNew is at the end
            if(Ds < DsTiny)CYCLE LOOPLINE

            ! Make sure we don't try to increase the step below
            IsEdge = .true.
         end do

         ! Check how big the largest change is in the generalized coordinates
         Step = maxval(abs(CoordLosNew_D - CoordLos_D)/CellSize_D)

         ! If change is too large or too small adjust the step size
         if(Step > StepMax .or. (Step < StepMin .and. .not. IsEdge))then
            ! New interval size corresponds to a StepGood
            ! in generalized coordinates instead of Step
            Ds = Ds*StepGood/Step

            ! Check if mid point will be inside the block. If not, reduce Ds
            do
               ! Move to the middle of the modified segment
               XyzLosNew_D = XyzLos_D + 0.5*Ds*LosPix_D
               call xyz_to_coord(XyzLosNew_D, CoordLosNew_D)

               ! Check if midpoint is inside block
               dCoord_D = abs(CoordLosNew_D - CoordBlock_D)
               if(all(2*dCoord_D(iDimMin:) <= CoordSizeBlock_D(iDimMin:)))EXIT

               ! Reduce Ds and try again
               Ds = Ds*0.5

               ! Don't integrate this segment if it is very small
               if(Ds < DsTiny)CYCLE LOOPLINE
            end do
         end if
         if(Length + Ds >= LengthMax)then
            ! Reduce the integration step newr the end of segment...
            if(iProc == iProcFound)&
                                ! Add contribution from this segment to the image
                 call add_segment(LengthMax - Length, XyzLosNew_D, UseThreads)
            RETURN
         else
            if(iProc == iProcFound)then
               ! Add contribution from this segment to the image
               call add_segment(Ds, XyzLosNew_D, UseThreads)
            end if

            ! Move XyzLosNew to the end of the segment
            XyzLosNew_D = XyzLos_D + Ds*LosPix_D
            call xyz_to_coord(XyzLosNew_D, CoordLosNew_D)
         end if
      end do LOOPLINE

    end subroutine integrate_line
    !==========================================================================
    subroutine add_segment(Ds, XyzLos_D, UseThreads)

      use ModMain,        ONLY: NameVarLower_V
      use ModAdvance,     ONLY: UseElectronPressure, UseIdealEos
      use ModInterpolate, ONLY: interpolate_vector, interpolate_scalar
      use ModFieldLineThread, ONLY: interpolate_thread_state, DoPlotThreads
      use ModMultifluid,  ONLY: UseMultiIon, MassIon_I, ChargeIon_I, &
           iRhoIon_I, iPIon_I
      use ModPhysics,     ONLY: AverageIonCharge, PePerPtotal
      use ModVarIndexes,  ONLY: nVar, Rho_, Pe_, p_, Bx_, Bz_
      use ModB0,          ONLY: UseB0, B0_DGB
      use BATL_lib,       ONLY: xyz_to_coord, MinIJK_D, MaxIJK_D, CoordMin_D
      use ModUserInterface ! user_set_plot_var

      real, intent(in):: Ds          ! Length of line segment
      real, intent(in):: XyzLos_D(3) ! location of center of line segment
      logical, optional, intent(in) :: UseThreads

      real :: x, y, z
      real :: aLos, bLos, cLos, dLos
      real :: SinOmega, CosOmega, Cos2Theta, Sin2Omega, Cos2Omega, Logarithm

      real :: xLos, yLos, zLos, rLos2 ! Coordinates and radial distance squared
      real :: CoordNorm_D(3) ! Normalized coordinates of current point

      real :: State_V(nVar)  ! State at the center of the segment center
      real :: Rho            ! Mass density at the point
      real :: Te, TeSi       ! Electron temperature
      real :: Ne             ! Electron number density
      real :: Value          ! Value of the LOS variable at the point

      ! Variables for user defined LOS variables
      integer :: iBlockLast = -1, iVarLast = -1
      logical :: IsFound, UseBody
      character(len=1):: NameTecVar, NameTecUnit, NameIdlUnit
      real    :: ValueBody
      real, allocatable, save:: PlotVar_GV(:,:,:,:)
      logical :: DoneStateInterpolate = .false.
      integer :: jVar

      ! Added for EUV synth and sph geometry
      real :: GenLos_D(3)
      real :: ResponseFactor, EuvResponse_W(3), SxrResponse_W(2)

      ! parameters for temperature cuttoff of EUV/SXR response
      ! idea is to neglect most of the broadened transition region
      ! since broadening introduces unphysical column depth (by orders of
      ! magnitude) which can cause it to be large enough to produce an
      ! unwanted contribution
      real :: TeCutSi = 4.0e+5
      real :: DeltaTeCutSi = 3.0e+4
      real :: FractionTrue

      ! DEM/EM calculation
      real :: LogTeSi

      !------------------------------------------------------------------------
      rLos2= sum(XyzLos_D**2)
      xLos = XyzLos_D(1)
      yLos = XyzLos_D(2)
      zLos = XyzLos_D(3)

      if(UseScattering .and. rLos2 > 1.0)then
         ! See Hundhausen, A.J. 1993, JGR, 98(A8), 13177, doi:10.1029/93JA00157
         ! Equations for A, B, C, D on page 13,190

         ! This calculation is useful for light scattering in SC and IH
         ! as it assumes that the radiation comes from a central
         ! body with radius 1. Normally setting rOccult > 1 ensures rLos2 > 1.

         Sin2Omega = 1.0/rLos2
         SinOmega  = sqrt(Sin2Omega)
         Cos2Omega = 1 - Sin2Omega
         CosOmega = sqrt(Cos2Omega)
         Logarithm = log((1.0 + SinOmega)/CosOmega)

         ! omega and functions of omega are unique to a given line of sight
         aLos = CosOmega*Sin2Omega
         bLos = -0.125*( 1.0 - 3.0*Sin2Omega - (Cos2Omega/SinOmega)* &
              (1.0 + 3.0*Sin2Omega)*Logarithm )
         cLos = 4.0/3.0 - CosOmega - (1.0/3.0)*CosOmega*Cos2Omega
         dLos = 0.125*( 5.0 + Sin2Omega - (Cos2omega/SinOmega) * &
              (5.0 - Sin2Omega)*Logarithm )

         z =   (LosPix_D(1)**2 + LosPix_D(2)**2)*zLos            &
              - LosPix_D(3)*(LosPix_D(1)*xLos + LosPix_D(2)*yLos)
         x = xLos + (LosPix_D(1)/LosPix_D(3)) * (z - zLos)
         y = yLos + (LosPix_D(2)/LosPix_D(3)) * (z - zLos)
         Cos2Theta = (x**2 + y**2 + z**2)/rLos2
      end if

      ! Calculate normalized position
      if(IsRzGeometry)then
         ! Radial distance is sqrt(yLos**2+zLos**2)
         CoordNorm_D(1:2) = &
              ( [xLos*XyzBlockSign_D(1), sqrt(yLos**2+zLos**2) ] &
              - CoordMinBlock_D(1:2) )/CellSize_D(1:2) + 0.5
         CoordNorm_D(3) = 0.0
      elseif(IsCartesian)then
         CoordNorm_D = &
              (XyzBlockSign_D*XyzLos_D - CoordMinBlock_D)/CellSize_D + 0.5
      else
         ! get gen StringCoord of center point
         call xyz_to_coord(XyzBlockSign_D*XyzLos_D, GenLos_D)

         ! Normalized coordinates (to cell index)
         CoordNorm_D = (GenLos_D - CoordMinBlock_D)/CellSize_D + 0.5
      end if

      ! Interpolate state if it is needed by any of the plot variables
      DoneStateInterpolate = .false.
      if(UseRho .or. UseEuv .or. UseSxr .or. UseTableGen)then
         !`Interpolate state vector in the point with gen coords
         ! equal to GenLos_D
         if(present(UseThreads)  & ! This point is in the threaded gap OR the
              .or.(DoPlotThreads.and.& ! gap is used AND point is close to it
              GenLos_D(1) < CoordMin_D(1) + &
              0.50*CellSize_D(1) ))then
            ! Interpolate within the threaded gap
            call interpolate_thread_state(GenLos_D, iBlock, State_V)
         else
            ! Interpolate in the physical domain
            State_V = interpolate_vector(State_VGB(:,:,:,:,iBlock), &
                 nVar, nDim, MinIJK_D, MaxIJK_D, CoordNorm_D)
            if(UseB0 .and. UseFlux)State_V(Bx_:Bz_) = State_V(Bx_:Bz_) &
                 + interpolate_vector(B0_DGB(:,:,:,:,iBlock), &
                 3, nDim, MinIJK_D, MaxIJK_D, CoordNorm_D)
         end if
         DoneStateInterpolate = .true.
         Rho = State_V(Rho_)
      end if

      if(UseFlux .or. UseNbi)then
         Spectrum_I=0.
         call spectrum_calc_flux(iFile, State_V, Ds, nLambda, LosDir_D, &
              UseNbi, Spectrum_I(:))
         ImagePe_VIII(1,iPix,jPix,:)=ImagePe_VIII(1,iPix,jPix,:)+Spectrum_I(:)
         RETURN
      end if

      if(UseEuv .or. UseSxr .or. UseTableGen .or. UseDEM)then
         if(UseMultiIon)then
            Ne = sum(ChargeIon_I*State_V(iRhoIon_I)/MassIon_I)
         elseif(UseIdealEos)then
            Ne = Rho*AverageIonCharge/MassIon_I(1)
         end if
         if(UseElectronPressure)then
            Te = State_V(Pe_)/Ne
         elseif(UseMultiIon)then
            Te = sum(State_V(iPIon_I))*PePerPtotal/Ne
         else
            Te = State_V(p_)*PePerPtotal/Ne
         end if

         ! !! So minimum temperature is cTolerance in SI units???
         TeSi = max(Te*No2Si_V(UnitTemperature_), cTolerance)
         LogTeSi = log10(TeSi)

         ! Here 1e-6 is to convert to CGS
         Ne = 1.0e-6*Ne*No2Si_V(UnitN_)

         if(UseDEM)then
            ! Find temperature bin
            iTe = int((LogTeSi - LogTeMinDEM_I(iFile))/DLogTeDEM_I(iFile))&
                 + 1

            if(LogTeSi < LogTeMinDEM_I(iFile) .or. &
                 iTe > nLogTeDEM)RETURN

            ! Integrate DEM and EM values
            ImagePe_VIII(DEM_,iPix,jPix,iTe) = &
                 ImagePe_VIII(DEM_,iPix,jPix,iTe) + Ne**2 * Ds&
                 * (1.0e2*No2Si_V(UnitX_)) / (DLogTeDEM_I(iFile)*TeSi*log(10.))
            ImagePe_VIII(EM_,iPix,jPix,iTe) = ImagePe_VIII(EM_,iPix,jPix,iTe)&
                 + Ne**2 * Ds&
                 * SizePix_D(1)*SizePix_D(2) * (1.0e2*No2Si_V(UnitX_))**3
            RETURN
         end if

         !  ResponseFactor is applied to the product of tabulated "response
         !  function" (which is provided in the tables without a scaling
         !  factor, 1e-26) by the element of dimensionless length, ds, which
         !  should be converted to cm by  multiplying it by UnitX (which gives
         !  meters) and by 100 cm/m. The dependence on density should be also
         !  accounted for by multiplying this by Ne**2, Ne being in psrticles
         !  per cm3
         ResponseFactor = Ne**2*1.0e-26*(1.0e2*No2Si_V(UnitX_))
         if(present(UseThreads))then
            ! The head conduction is not modified, the whole response is true:
            FractionTrue = 1.0
         else
            ! calculate temperature cutoff to neglect widened transition region
            FractionTrue = 0.5*(1.0 + tanh((TeSi - TeCutSi)/DeltaTeCutSi))
         end if
         ! !! There should be just one table, not three!!!
         if (UseEuv) then
            ! now interpolate EUV response values from a lookup table
            if (iTableEUV <=0) &
                 call stop_mpi('Need to load #LOOKUPTABLE for EUV response!')
            call interpolate_lookup_table(iTableEUV, TeSi, Ne, &
                 EuvResponse_W, DoExtrapolate=.true.)
            EuvResponse_W = EuvResponse_W * FractionTrue
         end if

         if (UseSxr) then
            ! now interpolate SXR response values from a lookup table
            if (iTableSXR <=0) &
                 call stop_mpi('Need to load #LOOKUPTABLE for SXR response!')
            call interpolate_lookup_table(iTableSXR, TeSi, Ne, &
                 SxrResponse_W, DoExtrapolate=.true.)
            SxrResponse_W = SxrResponse_W * FractionTrue
         end if

         if (UseTableGen) then
            if(iTableGen <= 0) &
                 call stop_mpi('Need to load #LOOKUPTABLE for ' &
                 //NameLosTable_I(iFile)//' response!')
            ! now interpolate the entire table
            call interpolate_lookup_table(iTableGen, TeSi, Ne, &
                 InterpValues_I, DoExtrapolate=.true.)
            InterpValues_I = InterpValues_I * FractionTrue

            ! if using a generalized table can do it vector style
            ImagePe_VIII(:,iPix,jPix,1) = ImagePe_VIII(:,iPix,jPix,1) + &
                 InterpValues_I*ResponseFactor*Ds

            RETURN
         endif
      end if

      do iVar = 1, nPlotVar
         Value = 0.0 ! initialize to 0 so that if statements below work right
         NameVar = NamePlotVar_V(iVar)
         select case(NameVar)
         case ('len')
            ! Integrate the length of the integration lines
            Value = 1.0

         case('wl')
            ! White light with limb darkening
            if(rLos2 > 1.0) Value = Rho*( &
                 (1 - MuLimbDarkening)*(2*cLos - aLos*Cos2Theta) &
                 + MuLimbDarkening*(2*dLos - bLos*Cos2Theta) )

         case('pb')
            ! Polarization brightness
            if(rLos2 > 1.0) Value = &
                 Rho*( (1.0 - MuLimbDarkening)*aLos &
                 + MuLimbDarkening*bLos)*Cos2Theta

         case('euv171')
            ! EUV 171
            Value = EuvResponse_W(1)*ResponseFactor

         case('euv195')
            ! EUV 195
            Value = EuvResponse_W(2)*ResponseFactor

         case('euv284')
            ! EUV 284
            Value = EuvResponse_W(3)*ResponseFactor

         case('sxr')
            ! Soft X-ray (Only one channel for now, can add others later)
            Value = SxrResponse_W(1)*ResponseFactor

         case('rho')
            ! Simple density integral
            Value = Rho

         case('sphere10')
            ! Sphere of radius 10 with 100-r^2 density profile
            Value = max(0.0, 100.0 - rLos2)

         case('cube10')
            ! 20x20x20 cube centered around X=Y=Z=10
            Value = product( 0.5 + sign(0.5, 10.0 - abs(XyzLos_D-10.0)) )

         case default

            ! check if the variable is standard state variable
            do jVar = 1, nVar
               if(NameVarLower_V(jVar) /= NameVar) CYCLE

               if (.not. DoneStateInterpolate) then
                  State_V = interpolate_vector(State_VGB(:,:,:,:,iBlock), &
                       nVar, nDim, MinIJK_D, MaxIJK_D, CoordNorm_D)
                  DoneStateInterpolate = .true.
               end if

               Value = State_V(jVar)

               EXIT
            end do

            ! if the name is not in state variables
            if (jVar > nVar) then
               ! Obtain user defined plot function for the whole block
               if(iBlock /= iBlockLast .or. iVar > iVarLast)then
                  iBlockLast = iBlock
                  iVarLast   = iVar
                  if(.not.allocated(PlotVar_GV)) allocate( &
                       PlotVar_GV(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,nPlotVar))
                  call user_set_plot_var(iBlock, NameVar, &
                       IsDimensionalPlot_I(iFile), PlotVar_GV(:,:,:,iVar), &
                       ValueBody, UseBody, NameTecVar, NameTecUnit, &
                       NameIdlUnit, IsFound)
                  if(.not. IsFound)then
                     PlotVar_GV(:,:,:,iVar)=-7777.
                     if(iProc==0.and.iBlock==1)write(*,*) &
                          NameSub, ' WARNING: unknown plotvarname=', NameVar
                  end if
               end if
               ! Interpolate value
               Value = interpolate_scalar(PlotVar_GV(:,:,:,iVar), &
                    nDim, MinIJK_D, MaxIJK_D, CoordNorm_D)
            end if
         end select

         ImagePe_VIII(iVar,iPix,jPix,1) = ImagePe_VIII(iVar,iPix,jPix,1) &
              + Value*Ds

      end do ! iVar

    end subroutine add_segment
    !==========================================================================
    subroutine integrate_block

      character(len=*), parameter:: NameSub = 'integrate_block'
      !------------------------------------------------------------------------
      if(IsRzGeometry)then
         ! Exclude blocks that do not intersect the Z=0 plane
         if(nK > 1)then
            if(.not.(Xyz_DGB(z_,1,1,0,iBlock)<0 &
                 .and. Xyz_DGB(z_,1,1,nK,iBlock)>0)) RETURN
         end if
         ! Exclude blocks below the Y=0 plane
         if(Xyz_DGB(y_,1,nJ,1,iBlock)<0) RETURN
      end if

      rBlockSize = 0.5*sqrt(&
           ((nI+1)*CellSize_DB(x_,iBlock))**2 + &
           ((nJ+1)*CellSize_DB(y_,iBlock))**2 + &
           ((nK+1)*CellSize_DB(z_,iBlock))**2)

      ! position of the block center
      XyzBlockCenter_D = 0.5*(Xyz_DGB(:,nI,nJ,nK,iBlock) &
           +                  Xyz_DGB(:,1,1,1,iBlock))

      if(iMirror == 2) XyzBlockCenter_D(1) = -XyzBlockCenter_D(1)
      if(jMirror == 2) XyzBlockCenter_D(2) = -XyzBlockCenter_D(2)
      if(kMirror == 2) XyzBlockCenter_D(3) = -XyzBlockCenter_D(3)

      rBlockCenter = norm2(XyzBlockCenter_D)

      if(.not.IsRzGeometry .and. (UseEuv .or. UseSxr .or. UseTableGen)) then
         ! in cartesian grid, the rBody boundary cuts through blocks and,
         ! since EUV plots are integrating to surface, need to make sure that
         ! interpolation does not interpolate to ghost cells filled with
         ! garbage body values. So make sure that rInner is equal to
         ! rBody + cell diagonal width.
         ! This way, 8 cells bounding a point along the los
         ! are guaranteed to be true_cells. Only do this for blocks on the
         ! body (doesn't affect others). Also, changing it within block loop
         ! means rInner depends on block resolution (which you want).

         rInner = max(rBody, RadiusMin)

         if(UseBody .and. rMin_B(iBlock) < rBody ) &
              rInner = rBody + norm2(CellSize_D)
      end if

      FixedXyzBlockCenter_D = XyzBlockCenter_D

      if(rBlockCenter < rInner - rBlockSize) RETURN

      if(rBlockCenter > rOuter + rBlockSize) RETURN

      if(IsRzGeometry)then
         ! There are no simple checks to exclude a block in R-Z geometry
         DoCheckBlock = .false.
      else
         ! distance of block center from the observer along the LOS
         BlockDistance = dot_product(Los_D, XyzBlockCenter_D - ObsPos_D)

         ! Only blocks towards the image can be checked for exclusion easily
         DoCheckBlock = BlockDistance > 0
      end if
      if(DoCheckBlock)then
         Ratio = ObsDistance/BlockDistance
         ! 3D vector from the image center to the projected block center
         XyzBlockCenter_D = Ratio*(XyzBlockCenter_D - ObsPos_D) + ObsPos_D &
              - ImageCenter_D
         aBlockCenter = dot_product(XyzBlockCenter_D, aUnit_D)
         bBlockCenter = dot_product(XyzBlockCenter_D, bUnit_D)

         ! Project block size
         rBlockSize = rBlockSize*Ratio

         ! Check if block is inside the LOS image
         if((rSizeImage + rBlockSize)**2 < aBlockCenter**2 + bBlockCenter**2) &
              RETURN
      end if

      ! Store cell 1,1,1 coordinates
      XyzBlockStart_D = Coord111_DB(:,iBlock)

      ! Loop over pixels
      do jPix = 1, nPix_D(2)

         ! Y position of the pixel on the image plane
         bPix = (jPix - 1) * SizePix_D(2) - HalfSizeImage_D(2)

         ! Check if block can intersect this pixel
         if(DoCheckBlock)then
            if(abs(bPix - bBlockCenter) > rBlockSize) CYCLE
         end if

         do iPix = 1, nPix_D(1)

            ! X position of the pixel on the image plane
            aPix = (iPix - 1) * SizePix_D(1) - HalfSizeImage_D(1)

            ! Check if block can intersect this pixel
            if(DoCheckBlock)then
               if( (aPix - aBlockCenter)**2 + (bPix - bBlockCenter)**2 > &
                    rBlockSize**2 ) CYCLE
            end if

            r2Pix = (aPix + aOffset)**2 + (bPix + bOffset)**2
            ! Check if pixel is within occultation radius
            if( r2Pix  <= rOccult2 ) CYCLE

            r2Pix = aPix**2 + bPix**2
            ! Check if pixel is outside the circular region
            if( r2Pix > rSizeImage2 ) CYCLE

            ! Get the 3D location of the pixel
            XyzPix_D = ImageCenter_D + aPix*aUnit_D + bPix*bUnit_D

            ! Unit vector pointing from observer to pixel center
            LosPix_D = ObsPos_D - XyzPix_D
            LosPix_D = LosPix_D/norm2(LosPix_D)

            ! Do not let LOS direction to be perfectly aligned with major axes
            where(LosPix_D == 0.0) LosPix_D = cTiny

            ! Calculate contribution of this block to this pixel
            if(IsRzGeometry)then
               call integrate_los_block_rz
            else
               call integrate_los_block
            end if

         end do ! jPix loop
      end do    ! iPix loop

    end subroutine integrate_block
    !==========================================================================
    subroutine integrate_los_block_rz

      ! 0. Set x_S to the left and right X faces,
      !    and r_S to the inner and outer Y faces of the block.
      !
      ! 1. Calculate the Y and Z coordinates of the LOS intersecting x_S
      !    xMinBox = xMin and xMaxBox = xMax and obtain the corresponding
      !    radial distances r1 = sqrt(y1^2+z1^2) and r2=sqrt(y^2 + z^2)
      !    and keep them if R1 or R2 is within the [rMin, rMax] interval.
      !
      ! 2. Calculate the intersection of the line with a circular ring of width
      !    yMax-yMin in the Y-Z plane and obtain the corresponding X values
      !    (up to 4) and keep the ones that are within [xMin, xMax].
      !    This requires the solution of 2 second order equations.
      !
      ! 3. Integrate along the 1 or 2 line segments
      !    (making sure that we integrate inside the ring! ) to get the length
      !    of the line segment.

      use ModSort, ONLY: sort_quick

      ! maximum number of intersections between LOS and
      ! the ring formed by rotating the block around the X axis
      ! There are six potential cross sections with the sides and inner and
      ! outer rings, but only at most 4 of these are on the block
      integer, parameter :: MaxIntersect = 6

      ! index and number of intersections
      integer :: iIntersect, nIntersect

      ! indexes for sorting by distance
      integer :: iSort_I(MaxIntersect)
      integer :: iSide, iSign

      ! the axial (X) and squared radial (Y**2) coordinates of the block faces
      real :: x_S(2), r2_S(2)
      real :: Ratio, UnitYZ_D(2), DistRmin, r2Min, r2, Dist2, Dist

      ! coordinates of the intersections
      real :: Intersect_D(3), Intersect2_D(3), Intersect_DI(3,MaxIntersect)

      ! distances of intersections from the center of the pixel
      real :: DistIntersect, DistIntersect_I(MaxIntersect)
      !------------------------------------------------------------------------
      ! Calculate the closest approach to the origin in the Y-Z plane
      ! Normalize the Y-Z components of the LOS vector to unity
      Ratio     = 1/sqrt(sum(LosPix_D(2:3)**2))
      UnitYZ_D  = Ratio*LosPix_D(2:3)

      ! Distance to the closest approach is the projection of the pixel
      ! location to the line pointing in the LOS direction
      DistRMin = -sum(UnitYZ_D*XyzPix_D(2:3))

      ! The min distance squared can be obtained from the Pythagorian theorem
      r2Min        = sum(XyzPix_D(2:3)**2) - DistRmin**2

      ! The radial distance of the outer face of the block
      r2_S(2) = (0.5*(Xyz_DGB(y_,1,nJ,1,iBlock) &
           +          Xyz_DGB(y_,1,nJ+1,1,iBlock)))**2

      ! Return if the outer radius is smaller than the closest approach
      if(r2_s(2) < r2Min) RETURN

      ! The radial distance of the inner face of the block
      r2_S(1)= (0.5*(Xyz_DGB(y_,1, 0,1,iBlock) &
           +         Xyz_DGB(y_,1,   1,1,iBlock)))**2

      ! The X positions of the left and right faces of the block
      if(iMirror == 1) then
         x_S(1)= 0.5*(Xyz_DGB(x_, 0,1,1,iBlock) + Xyz_DGB(x_,   1,1,1,iBlock))
         x_S(2)= 0.5*(Xyz_DGB(x_,nI,1,1,iBlock) + Xyz_DGB(x_,nI+1,1,1,iBlock))
      else
         ! Swap signs and order of faces for mirror images
         x_S(1)= -0.5*(Xyz_DGB(x_,nI,1,1,iBlock) + Xyz_DGB(x_,nI+1,1,1,iBlock))
         x_S(2)= -0.5*(Xyz_DGB(x_, 0,1,1,iBlock) + Xyz_DGB(x_,   1,1,1,iBlock))
      end if

      ! Initialize intersection arrays
      nIntersect = 0

      ! Calculate intersection positions for the R faces
      ! There can be two intersections for both faces
      do iSide = 1, 2
         ! Distance from the closest approach to the intersection with the face
         Dist2 = r2_S(iSide) - r2Min
         if(Dist2 < 0) CYCLE ! no intersection
         Dist = sqrt(Dist2)
         ! Obtain the 3D coordinates for the two intersection points
         do iSign = -1, 1, 2
            DistIntersect = Ratio*(DistRmin + iSign*Dist)
            Intersect_D = XyzPix_D + DistIntersect*LosPix_D
            if(Intersect_D(1) > x_S(1) .and. Intersect_D(1) < x_S(2))then
               nIntersect = nIntersect + 1
               Intersect_DI(:,nIntersect)  = Intersect_D
               DistIntersect_I(nIntersect) = DistIntersect
            end if
         end do
      end do

      ! Calculate intersection positions for the X faces
      do iSide = 1, 2
         ! Distance to the intersection
         DistIntersect = (x_S(iSide) - XyzPix_D(1))/LosPix_D(1)
         Intersect_D = XyzPix_D + DistIntersect*LosPix_D
         r2 = sum(Intersect_D(2:3)**2)
         if(r2 > r2_S(1) .and. r2 < r2_S(2))then
            nIntersect = nIntersect + 1
            Intersect_DI(:,nIntersect) = Intersect_D
            DistIntersect_I(nIntersect) = DistIntersect
         end if
      end do

      if(nIntersect < 2) RETURN

      if(nIntersect > 2)then
         ! Sort intersection points by distance from pixel
         call sort_quick(nIntersect, DistIntersect_I(1:nIntersect), &
              iSort_I(1:nIntersect))
      else
         ! No need to sort two points
         iSort_I(1:2) = [1,2]
      end if

      ! Loop through segments connecting the consecutive intersection points
      do iIntersect = 1, nIntersect-1
         Intersect_D  = Intersect_DI(:,iSort_I(iIntersect))
         Intersect2_D = Intersect_DI(:,iSort_I(iIntersect+1))

         ! check if the radius of the midpoint is inside the block,
         ! if not then CYCLE
         r2 = sum((0.5*(Intersect_D(2:3) + Intersect2_D(2:3)))**2)

         if(r2 < r2_S(1) .or. r2 > r2_S(2)) CYCLE

         call integrate_segment(Intersect_D, Intersect2_D)
      end do

    end subroutine integrate_los_block_rz
    !==========================================================================
    subroutine integrate_los_block

      ! Local variables
      integer :: i, j, k, nCount

      real :: IntersectXyz_SDD(2,3,3) ! intersection coords of LOS for 6 faces
      real :: FaceXyz_SD(2,3) ! locations of the faces of the block
      real :: x1, x2, y1, y2, z1, z2
      real :: Point1_D(3), Point2_D(3)
      real :: R2Point1, R2Point2, rLine_D(3), rLine2
      real :: Coef1, Coef2, Coef3 ! Coefficients of quadratic equation
      real :: Discr
      real :: Solution1, Solution1_D(3), Solution2, Solution2_D(3)
      logical :: IsOuter, IsGoodSolution1, IsGoodSolution2, IsAllBehind

      real :: Tmp
      ! if(DoTiming)call timing_start('los_set_plotvar')

      ! Determine the location of the block faces
      !------------------------------------------------------------------------
      x1 = 0.5*(Xyz_DGB(x_, 0, 0, 0,iBlock)+Xyz_DGB(x_,   1,   1  , 1,iBlock))
      x2 = 0.5*(Xyz_DGB(x_,nI,nJ,nK,iBlock)+Xyz_DGB(x_,nI+1,nJ+1,nK+1,iBlock))
      y1 = 0.5*(Xyz_DGB(y_, 0, 0, 0,iBlock)+Xyz_DGB(y_,   1,   1,   1,iBlock))
      y2 = 0.5*(Xyz_DGB(y_,nI,nJ,nK,iBlock)+Xyz_DGB(y_,nI+1,nJ+1,nK+1,iBlock))
      z1 = 0.5*(Xyz_DGB(z_, 0, 0, 0,iBlock)+Xyz_DGB(z_,   1,   1,   1,iBlock))
      z2 = 0.5*(Xyz_DGB(z_,nI,nJ,nK,iBlock)+Xyz_DGB(z_,nI+1,nJ+1,nK+1,iBlock))

      ! Swap signs and order of faces for mirror images
      if(iMirror == 2) then
         Tmp = x2; x2 = -x1; x1 = -Tmp
      end if
      if(jMirror == 2) then
         Tmp = y2; y2 = -y1; y1 = -Tmp
      end if
      if(kMirror == 2) then
         Tmp = z2; z2 = -z1; z1 = -Tmp
      end if

      FaceXyz_SD(1,1) = x1
      FaceXyz_SD(1,2) = y1
      FaceXyz_SD(1,3) = z1
      FaceXyz_SD(2,1) = x2
      FaceXyz_SD(2,2) = y2
      FaceXyz_SD(2,3) = z2

      ! Determine where the line of sight enters and exits the block.
      ! Loop over the number of block face pairs, face directions
      ! and coordinates
      do i = 1, 2       ! face loop
         IntersectXyz_SDD(i,1,1) = FaceXyz_SD(i,1)
         IntersectXyz_SDD(i,2,2) = FaceXyz_SD(i,2)
         IntersectXyz_SDD(i,3,3) = FaceXyz_SD(i,3)

         do j=1,3     ! direction loop
            do k=1,3   ! coordinate loop
               if (j /= k) then
                  IntersectXyz_SDD(i,j,k) = XyzPix_D(k) + &
                       (LosPix_D(k)/LosPix_D(j)) &
                       *(FaceXyz_SD(i,j) - XyzPix_D(j))
               end if
            end do
         end do
      end do

      ! which of the 6 points are on the block?
      nCount = 0
      CHECK: do i=1,2
         do j=1,3
            if(        IntersectXyz_SDD(i,j,1) >= x1 &
                 .and. IntersectXyz_SDD(i,j,1) <= x2 ) then
               if(        IntersectXyz_SDD(i,j,2) >= y1 &
                    .and. IntersectXyz_SDD(i,j,2) <= y2 ) then
                  if(        IntersectXyz_SDD(i,j,3) >= z1 &
                       .and. IntersectXyz_SDD(i,j,3) <= z2 ) then
                     nCount = nCount + 1
                     if(nCount == 1) Point1_D = IntersectXyz_SDD(i,j,:)
                     if(nCount == 2) then
                        Point2_D = IntersectXyz_SDD(i,j,:)
                        ! If point 2 is different from point 1, we are done
                        if(sum(abs(Point1_D - Point2_D)) > cTolerance) &
                             EXIT CHECK
                        ! Ignore the second point, keep checking
                        nCount = 1
                     end if
                  end if
               end if
            end if
         end do
      end do CHECK

      ! Check if the los cuts through the block
      if(nCount /= 2) RETURN

      R2Point1 = sum(Point1_D**2)
      R2Point2 = sum(Point2_D**2)

      ! Check if the whole segment is inside rInner
      if( R2Point1 <= rInner2 .and. R2Point2 <= rInner2) RETURN

      ! Check if the whole segment is outside rOuter
      rLine_D = XyzPix_D - LosPix_D*dot_product(LosPix_D, XyzPix_D)
      rLine2  = sum(rLine_D**2)
      if( rLine2 > rOuter2 ) RETURN

      ! Check if there is a need to calculate an intersection

      ! Do we intersect the outer sphere
      IsOuter = R2Point1 > rOuter2 .or. R2Point2 > rOuter2

      ! Do we intersect the inner or outer spheres
      if( IsOuter .or. &
           (rLine2 < rInner2 .and. rBlockCenter < rInner+rBlockSize) ) then

         Coef1 = sum((Point2_D - Point1_D)**2)
         Coef2 = 2*dot_product(Point1_D, Point2_D - Point1_D)

         if( IsOuter ) then
            Coef3 = R2Point1 - rOuter2
         else
            Coef3 = R2Point1 - rInner2
         end if

         Discr = Coef2**2 - 4*Coef1*Coef3

         if(Discr < 0.0)then
            write(*,*)'Warning: Discr=',Discr
            !   call stop_mpi("Negative discriminant")
            RETURN
         end if

         ! Line of sight tangent to the outer sphere
         if(IsOuter.AND.Discr==0.0)RETURN

         ! Find the two intersections (distance from point1 towards point2)
         Discr = sqrt(Discr)
         Solution1 = (-Coef2-Discr)/(2*Coef1)
         Solution2 = (-Coef2+Discr)/(2*Coef1)

         Solution1_D = Point1_D + (Point2_D - Point1_D) * Solution1
         Solution2_D = Point1_D + (Point2_D - Point1_D) * Solution2

         ! Check if the solutions are within the segment
         IsGoodSolution1 = (Solution1 >= 0.0 .and. Solution1 <= 1.0)
         IsGoodSolution2 = (Solution2 >= 0.0 .and. Solution2 <= 1.0)

         if(IsOuter)then
            ! For outer sphere replace
            ! outlying point1 with solution1 and
            ! outlying point2 with solution2
            if(R2Point1 > rOuter2) then
               if(IsGoodSolution1)then
                  Point1_D = Solution1_D
               else
                  RETURN
               end if
            end if
            if(R2Point2 > rOuter2) then
               if(IsGoodSolution2)then
                  Point2_D = Solution2_D
               else
                  RETURN
               end if
            end if
         else
            ! For inner sphere replace
            ! internal point1 with solution2 and
            ! internal point2 with solution1
            if(R2Point1 < rInner2) Point1_D = Solution2_D
            if(R2Point2 < rInner2) Point2_D = Solution1_D
            ! Weird case: the segment cuts the inner sphere
            if(IsGoodSolution1 .and. IsGoodSolution2)then
               ! Need to do two integrals:
               ! from point1 to solution1 and
               ! from point2 to solution2
               if(Discr > 0.0)then
                  if(Solution1 > cTiny) &
                       call integrate_segment(Point1_D, Solution1_D)
                  if(solution2< 1 - cTiny) &
                       call integrate_segment(Point2_D, Solution2_D)
                  RETURN
               end if
            end if

         end if
      end if

      ! remove backside of sun from EUV images
      if(UseEuv.or.UseSxr.or.UseTableGen) then
         call los_cut_backside(Point1_D, Point2_D,IsAllBehind)
         ! don't continue if all on backside
         if(IsAllBehind) RETURN
      endif

      call integrate_segment(Point1_D, Point2_D)

    end subroutine integrate_los_block
    !==========================================================================
    subroutine integrate_segment(XyzStart_D, XyzEnd_D)

      ! Integrate variables from XyzStart_D to XyzEnd_D
      ! The line is split into nSegment segments of length Ds

      use BATL_lib,       ONLY: CoordMin_DB

      real, intent(in) :: XyzStart_D(3), XyzEnd_D(3)

      integer :: iSegment, nSegment

      real :: Ds             ! Length of line segment
      real :: XyzLos_D(3)    ! Coordinate of center of line segment

      ! Number of segments for an accurate integral
      !------------------------------------------------------------------------
      if (IsRzGeometry) then
         ! In RZ geometry Delta Y is representative for the radial resolution
         nSegment = 1 + sum(abs(XyzEnd_D - XyzStart_D) &
              / [ CellSize_D(1), CellSize_D(2), CellSize_D(2) ] )
      else
         ! Measure distance in cell size units and add up dimensions
         nSegment = 1 + sum(abs(XyzEnd_D - XyzStart_D)/CellSize_D)
      end if

      ! Length of a segment
      Ds = sqrt(sum((XyzEnd_D - XyzStart_D)**2)) / nSegment

      CoordMinBlock_D = CoordMin_DB(:, iBlock)

      do iSegment = 1, nSegment
         XyzLos_D = XyzStart_D &
              + (iSegment - 0.5)/nSegment*(XyzEnd_D - XyzStart_D)

         call add_segment(Ds, XyzLos_D)

      end do ! line segment interation loop

    end subroutine integrate_segment
    !==========================================================================
    subroutine dimensionalize_plotvar_los

      use ModConst,   ONLY : cSigmaThomson
      use ModPhysics, ONLY : No2Si_V, UnitX_, UnitRho_
      !------------------------------------------------------------------------
      do iVar = 1, nPlotVar
         NameVar = NamePlotVar_V(iVar)

         select case(NameVar)
         case('len')
            Image_VIII(iVar,:,:,1) = Image_VIII(iVar,:,:,1)*No2Si_V(UnitX_)
         case('rho')
            Image_VIII(iVar,:,:,1) = Image_VIII(iVar,:,:,1) &
                 *No2Si_V(UnitRho_)*No2Si_V(UnitX_)
         case('wl','pb')
            ! The sigma in Hundhausen, A. J. (1993) should be the square of the
            ! electron radius according to Equation (4.54)
            ! in Altschuler, M.D. (1979),
            ! in Image Reconstruction from Projections,
            ! ed. G.T. Herman (Berlin:Springer), 105
            ! So we use cSigmaThomson*3.0/16.0 here.
            Image_VIII(iVar,:,:,1) = Image_VIII(iVar,:,:,1) &
                 *No2Si_V(UnitN_)*No2Si_V(UnitX_)*cSigmaThomson*3.0/16.0
         case('euv171','euv195','euv284','sxr')
            ! do nothing since already taken care of
         case default
            ! User defined functions are already dimensional, but integral
            ! requires a multiplication by length unit
            Image_VIII(iVar,:,:,1) = Image_VIII(iVar,:,:,1)*No2Si_V(UnitX_)
         end select

      end do ! iVar

    end subroutine dimensionalize_plotvar_los
    !==========================================================================
    subroutine los_cut_backside(Xyz1_D,Xyz2_D,IsAllBehind)

      real, dimension(3), intent(inOut) :: Xyz1_D,Xyz2_D
      logical, intent(out) :: IsAllBehind
      logical :: IsBehind1, IsBehind2
      !------------------------------------------------------------------------
      IsAllBehind = .false.

      ! check if pixel intersects the solar disk, if not then return
      if(r2Pix > rInner2) RETURN

      ! check if either are behind the sun (dot product will be negative if so)
      IsBehind1 = (sum(Xyz1_D * ObsPos_D) < 0.0)
      IsBehind2 = (sum(Xyz2_D * ObsPos_D) < 0.0)

      ! *** NOTE XyzPix_D is the 3D position along the pixel LOS that lies
      ! on the plane intersecting the sun center and perp to observer
      ! ---> perfect for trimming intersection to plane with the hemisphere
      ! towards the observer.

      if(IsBehind1) Xyz1_D = XyzPix_D

      if(IsBehind2) Xyz2_D = XyzPix_D

      ! if both are behind, will not need the LOS
      IsAllBehind = IsBehind1.and.IsBehind2

    end subroutine los_cut_backside
    !==========================================================================
  end subroutine write_plot_los
  !============================================================================
  subroutine get_los_variable_tec(iFile, nPlotVar, NamePlotVar_V, &
       StringUnitTec)

    ! Using plot var information set the units for Tecplot files

    use ModPhysics, ONLY : NameTecUnit_V, UnitX_, UnitU_
    use ModIO, ONLY: IsDimensionalPlot_I

    ! Arguments

    integer, intent(in) :: NPlotVar,iFile
    character (len=20), intent(in) :: NamePlotVar_V(NPlotVar)
    character (len=500), intent(out) :: StringUnitTec
    character (len=20) :: NamePlotVar

    integer :: iVar
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_los_variable_tec'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if (IsDimensionalPlot_I(iFile)) then
       write(StringUnitTec,'(a)') 'VARIABLES = '
       write(StringUnitTec,'(a)') trim(StringUnitTec)//'"X '//&
            trim(NameTecUnit_V(UnitX_))
       write(StringUnitTec,'(a)') trim(StringUnitTec)//'", "Y '//&
            trim(NameTecUnit_V(UnitX_))
    else
       write(StringUnitTec,'(a)') 'VARIABLES = "X", "Y'
    end if

    do iVar = 1, nPlotVar

       write(StringUnitTec,'(a)') trim(StringUnitTec)//'", "'

       NamePlotVar = NamePlotVar_V(iVar)

       if (IsDimensionalPlot_I(iFile)) then

          select case(NamePlotVar)
          case('len')
             write(StringUnitTec,'(a)') &
                  trim(StringUnitTec)//'len'//' '//&
                  trim(NameTecUnit_V(UnitX_))
          case('rho')
             write(StringUnitTec,'(a)') &
                  trim(StringUnitTec)//'`r [m^-^2]'
          case('vlos','Vlos','ulos','Ulos')
             write(StringUnitTec,'(a)') &
                  trim(StringUnitTec)//'u.s'//' '//&
                  trim(NameTecUnit_V(UnitU_))
          case('wl')
             write(StringUnitTec,'(a)') &
                  trim(StringUnitTec)//'`wl [m^-^2]'//' '
          case('pb')
             write(StringUnitTec,'(a)') &
                  trim(StringUnitTec)//'`pb [m^-^2]'//' '
          case('euv171')
             write(StringUnitTec,'(a)') &
                  trim(StringUnitTec)//'`euv171 [DN/S]'//' '
          case('euv195')
             write(StringUnitTec,'(a)') &
                  trim(StringUnitTec)//'`euv195 [DN/S]'//' '
          case('euv284')
             write(StringUnitTec,'(a)') &
                  trim(StringUnitTec)//'`euv284 [DN/S]'//' '
          case('sxr')
             write(StringUnitTec,'(a)') &
                  trim(StringUnitTec)//'`sxr [DN/S]'//' '

             ! DEFAULT FOR A BAD SELECTION
          case default
             write(StringUnitTec,'(a)') &
                  trim(StringUnitTec)//'Default'

          end select

       else ! normalized plot variables

          select case(NamePlotVar)
          case ('len')
             write(StringUnitTec,'(a)') &
                  trim(StringUnitTec)//'len'
          case('rho')
             write(StringUnitTec,'(a)') &
                  trim(StringUnitTec)//'`r'
          case('vlos','Vlos','ulos','Ulos')
             write(StringUnitTec,'(a)') &
                  trim(StringUnitTec)//'u.s'
          case('wl')
             write(StringUnitTec,'(a)') &
                  trim(StringUnitTec)//'wl'
          case('pb')
             write(StringUnitTec,'(a)') &
                  trim(StringUnitTec)//'pb'
          case('euv171')
             write(StringUnitTec,'(a)') &
                  trim(StringUnitTec)//'euv171'
          case('euv195')
             write(StringUnitTec,'(a)') &
                  trim(StringUnitTec)//'euv195'
          case('euv284')
             write(StringUnitTec,'(a)') &
                  trim(StringUnitTec)//'euv284'
          case('sxr')
             write(StringUnitTec,'(a)') &
                  trim(StringUnitTec)//'sxr'

             ! DEFAULT FOR A BAD SELECTION
          case default
             write(StringUnitTec,'(a)') &
                  trim(StringUnitTec)//'Default'

          end select

       end if

    end do

    write(StringUnitTec,'(a)') trim(StringUnitTec)//'"'

    call test_stop(NameSub, DoTest)
  end subroutine get_los_variable_tec
  !============================================================================
  subroutine get_los_unit_idl(iFile, nPlotVar, NamePlotVar_V, &
       StringUnitIdl, IsDimensional)

    ! Based on plot_var information set the header string with unit names

    use ModPhysics, ONLY : NameIdlUnit_V, UnitX_, UnitU_
    use ModIO, ONLY : IsDimensionalPlot_I, TypePlot_I

    ! Arguments

    integer, intent(in) :: iFile,NPlotVar
    logical, intent(in) :: IsDimensional
    character (len=20), intent(in) :: NamePlotVar_V(NPlotVar)
    character (len=79), intent(out) :: StringUnitIdl
    character (len=20) :: NamePlotVar

    integer :: iVar

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_los_unit_idl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if (IsDimensionalPlot_I(iFile)) then
       if(index(TypePlot_I(iFile),'dem')>0)then
          write(StringUnitIdl,'(a)') trim(NameIdlUnit_V(UnitX_))//' '//&
               trim(NameIdlUnit_V(UnitX_))//' logK'
       elseif(index(TypePlot_I(iFile),'fux')>0)then
          write(StringUnitIdl,'(a)') trim(NameIdlUnit_V(UnitX_))//' '//&
               trim(NameIdlUnit_V(UnitX_))//' A'
       else
          write(StringUnitIdl,'(a)') trim(NameIdlUnit_V(UnitX_))//' '//&
               trim(NameIdlUnit_V(UnitX_))//' '//&
               trim(NameIdlUnit_V(UnitX_))
       end if
    else
       if (IsDimensional) then
          do iVar = 1, nPlotVar
             write(StringUnitIdl,'(a)') trim(StringUnitIdl)//' '//'normalized'
          end do
          StringUnitIdl=adJustl(trim(StringUnitIdl))
       else
          write(StringUnitIdl,'(a)') 'normalized variables'
       end if

    end if

    if (IsDimensionalPlot_I(iFile)) then

       do iVar = 1, nPlotVar

          NamePlotVar = NamePlotVar_V(iVar)

          select case(NamePlotVar)
          case('len')
             write(StringUnitIdl,'(a)') &
                  trim(StringUnitIdl)//' '//&
                  trim(NameIdlUnit_V(UnitX_))
          case('rho')
             write(StringUnitIdl,'(a)') &
                  trim(StringUnitIdl)//' '//'[m^-^2]'
          case('vlos','Vlos','ulos','Ulos')
             write(StringUnitIdl,'(a)') &
                  trim(StringUnitIdl)//' '//&
                  trim(NameIdlUnit_V(UnitU_))
          case('wl')
             write(StringUnitIdl,'(a)') &
                  trim(StringUnitIdl)//' '//'[m^-^2]'
          case('pb')
             write(StringUnitIdl,'(a)') &
                  trim(StringUnitIdl)//' '//'[m^-^2]'
          case('euv171')
             write(StringUnitIdl,'(a)') &
                  trim(StringUnitIdl)//' '//'euv171 [DN/S]'
          case('euv195')
             write(StringUnitIdl,'(a)') &
                  trim(StringUnitIdl)//' '//'euv195 [dn/s]'
          case('euv284')
             write(StringUnitIdl,'(a)') &
                  trim(StringUnitIdl)//' '//'euv284 [DN/S]'
          case('sxr')
             write(StringUnitIdl,'(a)') &
                  trim(StringUnitIdl)//' '//'sxr [DN/S]'
          case('dem')
             write(StringUnitIdl,'(a)') &
                  trim(StringUnitIdl)//' '//'[cm^-5 K^-1]'
          case('em')
             write(StringUnitIdl,'(a)') &
                  trim(StringUnitIdl)//' '//'[cm^-3]'
          case('fux')
             write(StringUnitIdl,'(a)') &
                  trim(StringUnitIdl)//' '//'[erg sr^-1 cm^-2 A^-1]'
          case('nbi')
             write(StringUnitIdl,'(a)') &
                  trim(StringUnitIdl)//' '//'[DN/s]'
             ! DEFAULT FOR A BAD SELECTION
          case default
             write(StringUnitIdl,'(a)') &
                  trim(StringUnitIdl)//'" Dflt"'
          end select

       end do

    end if

    call test_stop(NameSub, DoTest)
  end subroutine get_los_unit_idl
  !============================================================================
end module ModWritePlotLos
!==============================================================================
