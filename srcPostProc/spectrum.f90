program spectrum

  use ModConst, ONLY: cLightSpeed, cBoltzmann, cProtonMass, cAU, cPi, rSun
  use ModUtilities, ONLY: CON_stop

  use ModMPI

  implicit none

  ! MPI variables
  integer                     :: iProc, nProc, iComm = MPI_COMM_WORLD

  ! Variables for DEM and EM calculation
  logical                     :: DoDEM = .false., DoSpectrum = .false.
  logical                     :: DoEM = .false.
  real,allocatable            :: DEM_I(:), Te_I(:), EM_I(:)
  real                        :: LogTeMinDEM, LogTeMaxDEM, DLogTeDEM
  character(len=200)          :: NameDEMFile = 'dem.out'
  character(len=200)          :: NameEMFile = 'em.out'

  ! Logical variables of running modes
  logical                     :: IsVerbose =  .false., IsDebug = .false.
  logical                     :: IsDataFile = .false. ! use input file or not
  logical                     :: IsLogTeMin = .false.

  integer                     :: iError

  real                        :: LogTeMin = 3.

  ! Variables for LOS image reconstruction
  logical                     :: IsResponseFunction = .false.
  character(len=200)          :: NameResponseFunctionFile = 'response.out'
  real, allocatable           :: ResponseLambda_I(:), ResponseFactor_I(:), &
       LOSimage_II(:,:)
  integer                     :: nResponseBin

  ! One line option
  logical                     :: IsOneLine = .false.
  real                        :: OneLineWavelength

  ! Variables for output file
  character(len=200)          :: NameSpectrumFile = 'spectrum.out'

  ! Variables for input files
  character(len=200)          :: StringLine
  character(len=200)          :: NameDataFile, NameTableFile
  character(len=200)          :: TypeDataFile

  ! Variables for instrument (if any)
  logical                     :: IsInstrument = .false., IsEIS = .false.
  character(len=200)          :: NameInstrument
  integer                     :: nPixel ! Number of pixels along slit
  integer                     :: nWavelengthBin, iWavelengthBin
  real                        :: SizeWavelengthBin ! Resolution in wavelength

  ! Variables for solar wind input data file (if any)
  logical                     :: IsDataBlock = .false. ! Overwrite data size
  logical                     :: IsOnePixel = .false. ! Select one pixel only
  logical                     :: IsPe = .false.
  logical                     :: IsPpar = .false., IsPperp = .false.
  logical                     :: IsNoAlfven = .false. ! Ignore Alfven waves
  logical                     :: IsDoppler = .false. ! Calculate Doppler shift
  integer                     :: n1Block, n2Block, n3Block ! Define data size
  integer                     :: iOnePixel, jOnePixel, kOnePixel
  ! Rotation angles for box variable rotation
  integer                     :: iDimLOS=1, iDimVertical=2, iDimHorizontal=3

  ! Variables to read solar wind data file in
  integer                     :: nVar   ! Number of variables
  integer                     :: nDim   ! Number of dimensions
  integer                     :: nParam ! Number of parameters
  integer                     :: n1, n2, n3 ! Data box size
  real                        :: CoordMin_D(3), CoordMax_D(3)
  real,allocatable            :: Var_VIII(:,:,:,:)
  ! For H:He 10:1 fully ionized plasma the proton:electron ratio is 1/(1+2*0.1)
  real                        :: ProtonElectronRatio = 0.83
  real                        :: dx = 1., dy = 1., dz = 1.
  !, dVperd2 = 1., dOmega= 1.

  ! Extend transition region
  logical :: DoExtendTransitionRegion = .false.
  real :: TeModSi = 3.0E+5    ! K
  real :: DeltaTeModSi = 1E+4 ! K

  ! Variables for uniform data
  logical                     :: IsUniData = .false. ! Overwrite data w/ const
  real                        :: RhoUni, UxUni, UyUni, UzUni, BxUni, ByUni
  real                        :: BzUni
  real                        :: TparUni, TperpUni, TeUni, I01Uni, I02Uni

  ! Indexes for the solar wind variables
  integer, parameter          :: &
       rho_  =  1, &
       ux_   =  2, &
       uy_   =  3, &
       uz_   =  4, &
       bx_   =  5, &
       by_   =  6, &
       bz_   =  7, &
       tpar_ =  8, &
       tperp_ =  9, &
       te_   = 10, &
       t_    = 11, &
       I01_  = 12, &
       I02_  = 13

  integer                     :: nLocalVar = 13 ! Number of used variables

  ! Variables for the wavelengths of interest
  logical                     :: IsAllLines = .false. ! Ignore unobserved lines
  integer                     :: iWavelengthInterval, nWavelengthInterval
  integer                     :: nMaxLine, nLineFound
  real, allocatable           :: WavelengthInterval_II(:,:)

  ! For Doppler-shift wavelength intervals are shifted by 10% of speed of light
  real, allocatable           :: WavelengthIntervalShifted_II(:,:)

  ! Temperature and density grid for contrbution function (G)
  real                        :: DLogN, DLogT
  real                        :: MinLogN, MinLogT, MaxLogN, MaxLogT

  ! Maximum index range in density and temperature
  integer                     :: MaxI, MinI, MaxJ, MinJ

  ! Variables for lines
  integer                     :: nBin ! Wavelength bin number
  integer                     :: i, iLine, jPixel, kPixel
  integer                     :: nLineAll ! All lines of interest

  ! Variables for using measured ion temperature
  logical                     :: UseTion = .false.
  real                        :: Tion

  ! Variables for using equilibrium ionization despite having charge states
  ! To use non-equilibrium ions set it to false
  logical                     :: UseIonFrac = .false.
  integer, parameter          :: nElement = 30
  character(len=2)            :: NameElement_I(1:nElement) = ['h ','he','li',&
       'be','b ','c ','n ','o ','f ','ne','na','mg','al','si','p ','s ','cl',&
       'ar','k ','ca','sc','ti','v ','cr','mn','fe','co','ni','cu','zn']
  integer                     :: nIonFracMax = 500

  !----------------------------------------------------------------------------
  type IonFracTableType
     character(len=6)         :: NameIonFrac
     real,allocatable         :: IonFraction_III(:,:,:)
  end type IonFracTableType

  type(IonFracTableType), allocatable :: IonFracTable_I(:)

  ! Derived type to read tabulated G values
  !----------------------------------------------------------------------------
  type LineTableType
     character(len=6)         :: NameIon
     real                     :: Aion
     integer                  :: nLevelFrom, nLevelTo ! Levels of transition
     ! Indexes on density-temperature grid
     ! i = density index, j = temperature index
     integer                  :: iMin, jMin, iMax, jMax
     real                     :: LineWavelength
     real, allocatable        :: g_II(:,:)
     real                     :: StartLogT
     real, allocatable        :: IonFrac_II(:,:)
  end type LineTableType

  type(LineTableType), allocatable :: LineTable_I(:)

  ! Derived type of output
  type SpectrumTableType
     integer                  :: nBin
     real,allocatable         :: Spectrum_III(:,:,:), SpectrumGrid_I(:)
  end type SpectrumTableType

  type(SpectrumTableType), allocatable :: SpectrumTable_I(:)

  ! For MPI_reduce in the main program

  integer :: iInterval

  character(len=*), parameter :: NameSub = 'spectrum.f90'

  !---------------------------------------------------------------------------
  call MPI_init(iError)
  call MPI_comm_rank(iComm, iProc, iError)
  call MPI_comm_size(iComm, nProc, iError)

  if(iProc==0)write(*,*)'Spectrum.exe starting'

  call read_param

  if(IsResponseFunction)then
     call read_responsefunction
  endif

  if(IsDataFile)then
     call read_data
  else
     call set_data_block
  endif

  if(DoDEM)then
     call calc_dem
     if(.not. DoSpectrum)then
        if(iProc==0)write(*,*)'Spectrum.exe ending with DEM calculation only.'
        STOP
     endif
  endif

  call read_table

  if(IsDoppler)deallocate(WavelengthIntervalShifted_II)

  nLineAll = min(nMaxLine,nLineFound)

  ! Loop over Chianti lines
  if(IsVerbose .and. iProc==0)write(*,*)'IsOneLine = ', IsOneLine
!!! ! nLineAll = 1.
  do iLine = 1,nLineAll
     if (.not. IsOneLine)call calc_flux
     if (IsOneLine .and.  &
          OneLineWavelength == LineTable_I(iLine)%LineWavelength)call calc_flux
  end do

  deallocate(Var_VIII, LineTable_I)

  if(IsResponseFunction)then
     call mpi_reduce_real_array(LOSimage_II, &
          size(LOSimage_II), MPI_SUM, 0, &
          iComm, iError)
  else
     do iInterval = 1, nWavelengthinterval
        call mpi_reduce_real_array(SpectrumTable_I(iInterval)%Spectrum_III, &
             size(SpectrumTable_I(iInterval)%Spectrum_III), MPI_SUM, 0, &
             iComm, iError)
     end do
  end if

  if(iProc==0)then
     call save_all
     if(IsVerbose .and. iProc==0 )write(*,*)'done with save_all'
  end if

  deallocate(WavelengthInterval_II, SpectrumTable_I)

  call MPI_finalize(iError)

  if(iProc==0)write(*,*)'Spectrum.exe ending'

contains
  !============================================================================

  subroutine read_responsefunction

    use ModPlotFile, ONLY: read_plot_file

    character(len=*), parameter:: NameSub = 'read_responsefunction'
    !--------------------------------------------------------------------------
    call read_plot_file(NameFile = NameResponseFunctionFile, &
         n1Out = nResponseBin, &
         iErrorOut = iError)

    if(iError /= 0) call CON_stop( &
         NameSub//' could not header from '//trim(NameResponseFunctionFile))

    allocate(ResponseLambda_I(nResponseBin), ResponseFactor_I(nResponseBin))

    call read_plot_file(NameFile = NameResponseFunctionFile, &
         TypeFileIn = 'ascii',                               &
         VarOut_I   = ResponseFactor_I,                      &
         CoordOut_I = ResponseLambda_I,                      &
         iErrorOut  = iError)

    if(iError /= 0) call CON_stop( &
         NameSub//' could not data from '//trim(NameResponseFunctionFile))

  end subroutine read_responsefunction
  !============================================================================

  subroutine calc_dem

    use ModPlotFile, ONLY: save_plot_file

    integer                        :: nLogTeDEM
    integer                        :: iTe, i, j, k
    real                           :: DxLocal, Ne

    ! Variables to save file.
    character(len=5)               :: TypeFileDEM = 'ascii'
    character(len=200)             :: StringHeaderDEM = &
         '[K] [cm^-5 K^-1]'
    character(len=200)             :: StringHeaderEM = &
         '[K] [cm^-3]'

    character(len=*), parameter:: NameSub = 'calc_dem'
    !--------------------------------------------------------------------------

    ! Set up temperature grid and allocate variables
    nLogTeDEM = nint((LogTeMaxDEM-LogTeMinDEM)/DLogTeDEM)

    allocate(Te_I(nLogTeDEM), DEM_I(nLogTeDEM+1), EM_I(nLogTeDEM+1))

    DEM_I = 0
    EM_I = 0

    do iTe=1,nLogTeDEM
       Te_I(iTe) = 10**(LogTeMinDEM+DLogTeDEM * iTe)
    end do

    do k = 1, n3
       do j = 1, n2
          do i=1,n1
             ! Cells inside or behind solar body
             if(all(Var_VIII(1:7,i,j,k)==0))CYCLE
             ! Cells off the Temperature grid stop calculation
             if(Var_VIII(te_,i,j,k)<10**LogTeMinDEM)CYCLE
             if(Var_VIII(te_,i,j,k)>10**LogTeMaxDEM)CYCLE
             ! Locate cell on Temperature grid
             do iTe=1,nLogTeDEM-1
                if(Var_VIII(te_,i,j,k)<Te_I(iTe+1)) EXIT
             end do
             DxLocal = dx
             if(DoExtendTransitionRegion) DxLocal = DxLocal &
                  /extension_factor(Var_VIII(te_,i,j,k))

             Ne = Var_VIII(rho_,i,j,k)/cProtonMass*1e-6/ProtonElectronRatio

             ! DEM value is Ne**2 *dh/dT in [cm^-5 K^-1]
             DEM_I(iTe) = DEM_I(iTe) + &
                  Ne**2 * DxLocal / DLogTeDEM / Var_VIII(te_,i,j,k)/log(10.)
             ! EM value is Ne**2 *dV in [cm^-3]
             EM_I(iTe) = EM_I(iTe) + &
                  Ne**2 * DxLocal * dy * dz
          end do
       end do
    end do

    do iTe=1,nLogTeDEM
       Te_I(iTe) = LogTeMinDEM+DLogTeDEM * iTe
    end do

    call save_plot_file(NameFile = NameDEMFile, &
         TypeFileIn     = TypeFileDEM,      &
         StringHeaderIn = StringHeaderDEM,  &
         NameVarIn      = "Temperature DEM",     &
         nDimIn         = 1,                     &
         Coord1In_I     = Te_I(1:nLogTeDEM-1),           &
         VarIn_I     = DEM_I(1:nLogTeDEM-1))

    call save_plot_file(NameFile = NameEMFile, &
         TypeFileIn     = TypeFileDEM,      &
         StringHeaderIn = StringHeaderEM,  &
         NameVarIn      = "Temperature EM",     &
         nDimIn         = 1,                     &
         Coord1In_I     = Te_I(1:nLogTeDEM-1),           &
         VarIn_I     = EM_I(1:nLogTeDEM-1))

    deallocate(Te_I,DEM_I, EM_I)

  end subroutine calc_dem
  !============================================================================
  subroutine set_data_block
    ! When no data file input is used set up uniform data values in defined box
    integer, parameter             :: iUnitOut = 18
    real                           :: MinWavelength, Maxwavelength
    character(len=*), parameter:: NameSub = 'set_data_block'
    !--------------------------------------------------------------------------

    n1 = n1Block
    n2 = n2Block
    n3 = n3Block
    allocate(Var_VIII(nLocalVar,n1,n2,n3))

    if (IsUniData) then
       ! cm^-3 --> kg/m^3
       Var_VIII(rho_,1:n1,1:n2,1:n3)  = RhoUni * 1e6 * cProtonMass
       ! km/s --> m/s
       Var_VIII(ux_,1:n1,1:n2,1:n3)   = UxUni * 1e3
       Var_VIII(uy_,1:n1,1:n2,1:n3)   = UyUni * 1e3
       Var_VIII(uz_,1:n1,1:n2,1:n3)   = UzUni * 1e3
       ! G --> T
       Var_VIII(bx_,1:n1,1:n2,1:n3)   = BxUni * 1e-4
       Var_VIII(by_,1:n1,1:n2,1:n3)   = ByUni * 1e-4
       Var_VIII(bz_,1:n1,1:n2,1:n3)   = BzUni * 1e-4
       ! K
       Var_VIII(tpar_,1:n1,1:n2,1:n3) = TparUni
       Var_VIII(tperp_,1:n1,1:n2,1:n3) = TperpUni
       Var_VIII(te_,1:n1,1:n2,1:n3)   = TeUni
       ! erg cm^-3 --> J m^-3
       Var_VIII(I01_,1:n1,1:n2,1:n3)  = I01Uni * 1e-1
       Var_VIII(I02_,1:n1,1:n2,1:n3)  = I02Uni * 1e-1

       !       dVperd2 = 1.0
    else
       call CON_stop( &
            NameSub//' need data input!!! ')
    end if

    ! Set up bins for Spectrum
    allocate(SpectrumTable_I(nWavelengthinterval))
    nBin = 0
    do iWavelengthInterval=1, nWavelengthInterval
       MinWavelength = WavelengthInterval_II(1,iWavelengthInterval)
       MaxWavelength = WavelengthInterval_II(2,iWavelengthInterval)
       nWavelengthBin = nint((MaxWavelength-MinWavelength)/SizeWavelengthBin)
       nBin = nWavelengthBin
       if(.not. IsResponseFunction)then
          allocate(SpectrumTable_I(iWavelengthinterval)%Spectrum_III(n2,n3, &
               nWavelengthBin))
          SpectrumTable_I(iWavelengthinterval)%Spectrum_III(:,:,:)=0.0
       end if
       allocate(SpectrumTable_I(iWavelengthinterval)%SpectrumGrid_I(&
            nWavelengthBin+1))

       SpectrumTable_I(iWavelengthinterval)%nBin=nBin
       do iWavelengthBin=1,nWavelengthBin+1
          ! Values of each wavelength bin correspond to the center of the bin
          SpectrumTable_I(iWavelengthinterval)%SpectrumGrid_I(iWavelengthBin)&
               = MinWavelength + (iWavelengthBin-.5)*SizeWavelengthBin
       end do
    end do

    if(IsResponseFunction)then
       allocate(LOSImage_II(n2,n3))
       LOSImage_II = 0.0
    endif

  end subroutine set_data_block
  !============================================================================
  subroutine save_all

    use ModPlotFile, ONLY: save_plot_file

    character                      :: Namefile

    ! Variables for output file
    character(len=5)     :: TypeFileSpectrum = 'ascii'
    character(len=200)   :: StringHeaderSpectrum = &
         '[A] [erg sr^-1 cm^-2 A^-1]'
    real,allocatable            :: &
         Intensity_VIII(:,:,:,:), CoordWave_I(:), CoordPixelJ_I(:),CoordPixelK_I(:)

    integer:: nWave, iWaveInterval, nWaveInterval, iWaveBin, iWave, &
         nWaveBin, nWaveAll, j, k
    real                        :: DWaveBin, WavelengthMin, WavelengthMax

    character(len=*), parameter:: NameSub = 'save_all'
    !--------------------------------------------------------------------------
    nWaveAll = 0
    nWaveInterval = nWavelengthInterval
    do iWavelengthInterval = 1,nWavelengthInterval
       nWaveAll = nWaveAll + SpectrumTable_I(iWavelengthInterval)%nBin
    end do

    allocate( &
         Intensity_VIII(1,nWaveAll,n2,n3), &
         CoordWave_I(nWaveAll), CoordPixelJ_I(n2), CoordPixelK_I(n3))

    if(IsDataBLock)then
       if(n3/=0)then
          do k = 1, n3
             CoordPixelK_I(k) = float(k)/float(n3)
          end do
       end if
       if(n2/=0)then
          do j = 1, n2
             CoordPixelJ_I(j) = float(j)/float(n2)
          end do
       end if
    end if

    if(IsResponseFunction)then
       call save_plot_file(NameFile = NameSpectrumFile, &
            TypeFileIn     = 'ascii',      &
            StringHeaderIn = '[DN/s]',  &
            NameVarIn      = "x y Intensity",     &
            nDimIn         = 2,                     &
            CoordMinIn_D   = CoordMin_D(2:3),            &
            CoordMaxIn_D   = CoordMax_D(2:3), &
            VarIn_II     = LOSImage_II)
       deallocate(ResponseLambda_I, ResponseFactor_I, LOSimage_II)
    else
       ! Number of wave length processed so far
       nWave = 0
       ! Loop over intervals
       if(IsVerbose .and. iProc==0) write(*,*)"  nWaveInterval = ",nWaveInterval
       do iWaveInterval = 1, nWaveInterval

          ! Number of wave length in this interval
          nWaveBin = SpectrumTable_I(iWaveInterval)%nBin
          WavelengthMin = WavelengthInterval_II(1,iWaveInterval)
          WavelengthMax = WavelengthInterval_II(2,iWaveInterval)
          DWaveBin = (WavelengthMax - WavelengthMin)/nWaveBin

          ! Loop over wave lengths in this interval
          do iWaveBin = 1, nWaveBin
             ! Global wave length index
             iWave = nWave + iWaveBin

             ! Save intensity and coordinate into global array
             Intensity_VIII(1,iWave,:,:) = &
                  SpectrumTable_I(iWaveInterval)%Spectrum_III(:,:,iWaveBin)
             CoordWave_I(iWave) = WavelengthMin + (iWaveBin - 0.5)*DWaveBin
          end do
          ! Finished processing this interval
          nWave = nWave + nWaveBin
       end do

       ! nDim = 1 if n2 = n3 = 1, nDim = 2 if n3 = 1 /= n2 otherwise nDim= 3
       if(IsDataBlock)then
          if(n3==1)then
             if(n2==1)then
                call save_plot_file(NameFile = NameSpectrumFile, &
                     TypeFileIn     = TypeFileSpectrum,      &
                     StringHeaderIn = StringHeaderSpectrum,  &
                     NameVarIn      = "wavelength flux",     &
                     Coord1In_I     = CoordWave_I,           &
                     VarIn_VI       = Intensity_VIII(:,:,1,1))
             else
                call save_plot_file(NameFile = NameSpectrumFile, &
                     TypeFileIn     = TypeFileSpectrum,      &
                     StringHeaderIn = StringHeaderSpectrum,  &
                     NameVarIn      = "wavelength y flux",   &
                     Coord1In_I     = CoordWave_I,           &
                     Coord2In_I     = CoordPixelJ_I,        &
                     VarIn_VII      = Intensity_VIII(:,:,:,1))
             endif
          else
             call save_plot_file(NameFile = NameSpectrumFile, &
                  TypeFileIn     = TypeFileSpectrum,      &
                  StringHeaderIn = StringHeaderSpectrum,  &
                  NameVarIn      = "wavelength x y flux", &
                  Coord1In_I     = CoordWave_I,           &
                  Coord2In_I     = CoordPixelJ_I,      &
                  Coord3In_I     = CoordPixelK_I,        &
                  VarIn_VIII      = Intensity_VIII)
          endif

       else
          if(n3==1)then
             if(n2==1)then
                call save_plot_file(NameFile = NameSpectrumFile, &
                     TypeFileIn     = TypeFileSpectrum,      &
                     StringHeaderIn = StringHeaderSpectrum,  &
                     NameVarIn      = "wavelength flux",     &
                     Coord1In_I     = CoordWave_I,           &
                     VarIn_VI       = Intensity_VIII(:,:,1,1))
             else
                call save_plot_file(NameFile = NameSpectrumFile, &
                     TypeFileIn     = TypeFileSpectrum,      &
                     StringHeaderIn = StringHeaderSpectrum,  &
                     NameVarIn      = "wavelength y flux",   &
                     CoordMinIn_D   = CoordMin_D,            &
                     CoordMaxIn_D   = CoordMax_D,            &
                     Coord1In_I     = CoordWave_I,           &
                     VarIn_VII      = Intensity_VIII(:,:,:,1))
             endif
          else
             call save_plot_file(NameFile = NameSpectrumFile, &
                  TypeFileIn     = TypeFileSpectrum,      &
                  StringHeaderIn = StringHeaderSpectrum,  &
                  NameVarIn      = "wavelength x y flux", &
                  CoordMinIn_D   = CoordMin_D,            &
                  CoordMaxIn_D   = CoordMax_D,            &
                  Coord1In_I     = CoordWave_I,           &
                  VarIn_VIII      = Intensity_VIII)
          endif
       end if

       deallocate(Intensity_VIII, CoordWave_I, CoordPixelJ_I, CoordPixelK_I)

    endif

  end subroutine save_all
  !============================================================================
  subroutine calc_flux

    use ModInterpolate, ONLY: bilinear

    integer                        :: i,iInterval, iBin, iCenter, iIon
    integer                        :: iNMin, jTMin, iNMax, jTMax

    real                           :: FluxMono
    real                           :: Lambda, LambdaSI, Lambda0SI, DLambdaSI
    real                           :: DLambda, DLambdaSI2, Lambda_shifted
    real                           :: DLambdaInstr2 = 0.0 !!! do it later
    real                           :: Zplus2, Zminus2, CosAlpha, SinAlpha
    real                           :: B_D(3), Bnorm_D(3)
    real                           :: Unth2, Uth2
    real                           :: Gint, LogNe, LogTe, Rho
    real                           :: Tlos
    real                           :: Aion
    real                           :: TShift, EquilIonFrac
    logical                        :: IsFound = .false.

    character(len=*), parameter:: NameSub = 'calc_flux'
    !--------------------------------------------------------------------------

    Aion     = LineTable_I(iLine)%Aion
    IsFound = .false.
    if(UseIonFrac)then
       do iIon=1,nIonFracMax
          if(LineTable_I(iLine)%NameIon==IonFracTable_I(iIon)%NameIonFrac)then
             IsFound = .true.
             if(IsVerbose .and. iProc ==0) then
                write(*,*)"  NON-EQUILIBRIUM IONIZATION USED for"
                write(*,*)"  Ion : ",LineTable_I(iLine)%NameIon
                write(*,*)" Line : ",LineTable_I(iLine)%LineWavelength
             end if
             EXIT
          end if
       enddo
    endif

    if(IsOnePixel .and. iProc==0)write(*,*)'n1 n2 n3 i j k =',n1,n2,n3,i,jPixel,kPixel

    do kPixel=1,n3
       do jPixel=1,n2
          ! Each pixel is done by one processor
          if(modulo(kPixel*n2+jPixel,nProc)/=iProc)CYCLE
          do i=1,n1
             if(IsOnePixel .and. (kPixel/=kOnePixel .or. &
                  jPixel/=jOnePixel .or. i/=iOnePixel))CYCLE
             ! Cells inside body or behind the solar disk
             if(all(Var_VIII(1:7,i,jPixel,kPixel)==0))CYCLE

             Lambda   = LineTable_I(iLine)%LineWavelength

             ! Doppler shift while x axis is oriented towards observer
             if(IsDoppler)then
                Lambda_shifted = (-Var_VIII(ux_,i,jPixel,kPixel)/cLightSpeed &
                     + 1 ) * Lambda
                Lambda = Lambda_shifted
             endif

             ! Find which interval wavelength belongs to
             iInterval = -1
             do iWavelengthInterval = 1, nWavelengthInterval
                if((Lambda <= WavelengthInterval_II(2,iWavelengthInterval)) &
                     .and.&
                     (Lambda >= WavelengthInterval_II(1,iWavelengthInterval)))&
                     then
                   iInterval = iWavelengthInterval
                   EXIT
                end if
             end do

             if(iInterval == -1)EXIT

             iBin =0
             do
                iBin=iBin+1
                if((Lambda > &
                     SpectrumTable_I(iInterval)%SpectrumGrid_I(iBin)) &
                     .and.&
                     (Lambda < &
                     SpectrumTable_I(iInterval)%SpectrumGrid_I(iBin+1)))&
                     then
                   iCenter=iBin
                   EXIT
                end if
                if(SpectrumTable_I(iInterval)%SpectrumGrid_I(iBin+1) > &
                     WavelengthInterval_II(2,iInterval))EXIT
                if (iBin==nBin)EXIT
             end do

             ! Calculate Elzasser variables
             Rho = Var_VIII(rho_,i,jPixel,kPixel)
             Zplus2   = Var_VIII(I01_,i,jPixel,kPixel) * 4.0 / Rho
             Zminus2  = Var_VIII(I02_,i,jPixel,kPixel) * 4.0 / Rho

             ! Calculate angle between LOS and B directions
             B_D      = Var_VIII(bx_:bz_,i,jPixel,kPixel)
             CosAlpha = abs(B_D(1)/sqrt(max(sum(B_D**2), 1e-30)))

             ! Calculate temperature relative to the LOS direction
             SinAlpha = sqrt(1 - CosAlpha**2)
             Tlos = SinAlpha**2 * Var_VIII(tperp_,i,jPixel,kPixel) &
                  + CosAlpha**2 * Var_VIII(tpar_,i,jPixel,kPixel)

             if(UseTion)Tlos = Tion

             ! Calculate thermal and non-thermal broadening
             Unth2    = 1.0/16.0 * (Zplus2 + Zminus2) * SinAlpha**2
             Uth2     = cBoltzmann * Tlos/(cProtonMass * Aion)

             ! Convert from kg m^-3 to kg cm^-3 (*1e-6)
             ! and divide by cProtonMass in kg so Ne is in cm^-3
             ! 1 : 0.83  electron to proton ratio is assumed
             LogNe = log10(Rho*1e-6/cProtonMass/ProtonElectronRatio)
             LogTe = log10(Var_VIII(te_,i,jPixel,kPixel))

             if(IsLogTeMin .and. (LogTe < LogTeMin))CYCLE

             ! Convert to SI
             LambdaSI = LineTable_I(iLine)%LineWavelength * 1e-10

             ! Add thermal and non-thermal broadening
             DLambdaSI2 = LambdaSI**2 * (Uth2 + Unth2)/cLightSpeed**2

             ! Add 70 mA for Hinode/EIS 2'' slit
             ! 70 mA is in FWHM, that's 7e-12 m
             ! FWHM = 2sqrt(2ln2)*sigma
             ! sigma = FWHM/(2sqrt(2ln2))
             ! sigma^2 = (7e-12)^2 /(4*2*ln2)
             if(IsEIS)DLambdaInstr2 = (7e-12)**2/(8*log(2.))

             ! Add instrumental broadening (if any)
             if(IsInstrument)DLambdaSI2 = DLambdaSI2 + DLambdaInstr2

             ! Convert [m] --> [A]
             DLambdaSI = sqrt(DLambdaSI2)
             DLambda   = DLambdaSI * 1e10

             ! Get the contribution function
             iNMin  = LineTable_I(iLine)%iMin
             jTMin  = LineTable_I(iLine)%jMin
             iNMax  = LineTable_I(iLine)%iMax
             jTMax  = LineTable_I(iLine)%jMax

             Gint = bilinear(LineTable_I(iLine)%g_II(:,:), &
                  iNMin, iNMax, jTMin, jTMax, &
                  [ LogNe/DLogN , LogTe/DLogT ],DoExtrapolate=.true.)

             if(UseIonFrac .and. IsFound)then
                EquilIonFrac = bilinear(LineTable_I(iLine)%IonFrac_II(:,:), &
                     iNMin, iNMax, jTMin, jTMax, &
                     [ LogNe/DLogN , LogTe/DLogT ],DoExtrapolate=.true.)
                Gint = Gint/EquilIonFrac * IonFracTable_I(iIon)%IonFraction_III(i,jPixel,kPixel)
             end if

             ! When Gint becomes negative due to extrapolation -> move to next
             if(Gint<=0)CYCLE

             ! Calculate flux and spread it on the Spectrum_II grids

             ! Constant multiplier converted from SI [m] to CGS units [cm]
             ! dV_cell / (1AU)^2 * 1e2
             ! dVperd2 = dx*dy*dz /(1.496e11)**2 * 1e2

             ! Solid angle obtained by the emitting surface at 1AU distance
             ! dOmega = dy*dz / (1.496e11)**2

             ! dVperd2/dOmega = dx * 1e2 or dx in CGS

             FluxMono = Gint * (10.0**LogNe)**2 / (4*cPi) * dx

             if(DoExtendTransitionRegion) FluxMono = FluxMono &
                  /extension_factor(Var_VIII(te_,i,jPixel,kPixel))

             if(IsDebug .and. iProc==0)then
                write(*,*)'                                                   '
                write(*,*)'      ',LineTable_I(iLine)%NameIon,'         '
                write(*,*)'LineWavelength = ',LineTable_I(iLine)%LineWavelength
                write(*,*)'Gint = ',log10(Gint),' FluxMono = ',FluxMono
                write(*,*)'Aion = ',Aion
                write(*,*)'LambdaSI =',LambdaSI
                write(*,*)'DLambdaSI, DLambda =', DLambdaSI, DLambda
                write(*,*)'Uth, Unth = ',sqrt(Uth2), sqrt(Unth2)
                write(*,*)'LogTlos = ',log10(Tlos)
                write(*,*)'iLine = ',iLine
                write(*,*)'iCenter , iInterval = ',iCenter, iInterval
                write(*,*)'jTMin, jTMax = ', jTMin, jTMax
                write(*,*)'iNMin, iNMax = ', iNMin, iNMax
                write(*,*)'LogNe,DLogN , LogTe,DLogT = ', &
                     LogNe,DLogN , LogTe,DLogT
                write(*,*)'LogNe/DLogN , LogTe/DLogT = ', &
                     LogNe/DLogN , LogTe/DLogT
                write(*,*)'Gint = ',Gint
                write(*,*)'                                                   '
             endif

             call disperse_line(iInterval, iCenter, Lambda, DLambda, FluxMono)
          end do
       end do
    end do

  end subroutine calc_flux
  !============================================================================

  real function extension_factor(TeSi)

    real, intent(in) :: TeSi

    real :: FractionSpitzer
    !--------------------------------------------------------------------------
    FractionSpitzer = 0.5*(1.0+tanh((TeSi-TeModSi)/DeltaTeModSi))

    extension_factor = FractionSpitzer + &
         (1.0 - FractionSpitzer)*(TeModSi/TeSi)**2.5
  end function extension_factor
  !============================================================================

  subroutine disperse_line(iInterval,iCenter,Lambda,DLambda,FluxMono)

    real, intent(in)            :: Lambda, DLambda, FluxMono
    integer, intent(in)         :: iInterval, iCenter

    integer                     :: iStep, iWave, nWaveBin
    integer                     :: iBin, iBegin, iEnd
    real                        :: Flux, Phi, InvNorm, InvSigma2
    real                        :: LambdaBin, LambdaBegin, LambdaEnd
    real                        :: LambdaDist

    integer                     :: jBin

    character(len=*), parameter:: NameSub = 'disperse_line'
    !--------------------------------------------------------------------------

    nWaveBin = SpectrumTable_I(iInterval)%nBin

    ! Beginning and end of gaussian truncated to +/-5 sigma in [A]
    LambdaBegin = (Lambda - 5*DLambda)
    LambdaEnd   = (Lambda + 5*DLambda)

    ! Find the corresponding wavelength bin for starting wavelength
    do iBegin = 1, nWaveBin - 1
       if (LambdaBegin < SpectrumTable_I(iInterval)%SpectrumGrid_I(iBegin+1))&
            EXIT
    end do

    ! Start at iBegin for efficiency and
    ! stop at nWaveBin-1 so iEnd = nWaveBin if no EXIT was performed
    do iEnd = iBegin, nWaveBin-1
       if (LambdaEnd < SpectrumTable_I(iInterval)%SpectrumGrid_I(iEnd)) EXIT
    end do

    InvNorm   = 1/(sqrt(2*cPi) * DLambda)
    InvSigma2 = 1/(2*DLambda**2)

    ! Update bins between begin and end indices by adding the Gaussian
    ! distribution

    do iBin = iBegin , iEnd
       ! Get wavelength from the center of the bin
       LambdaBin = SpectrumTable_I(iInterval)%SpectrumGrid_I(iBin)

       ! Get distance from peak wavelength in SI
       LambdaDist = Lambda - LambdaBin

       ! Calculate Gaussian of the line
       Phi = InvNorm * exp(-LambdaDist**2 * InvSigma2)

       ! Calculate total monochromatic flux
       Flux = FluxMono*Phi

       if(IsResponseFunction)then
          ! Calculate LOS image intensity in DN units
          do jBin = 1, nResponseBin
             if (LambdaBin < ResponseLambda_I(jBin)) EXIT
          end do
          LOSImage_II(jPixel,kPixel) = &
               LOSImage_II(jPixel,kPixel) + Flux * ResponseFactor_I(jBin) * &
               SizeWavelengthBin
       else

          ! Update bin with flux
          SpectrumTable_I(iInterval)%Spectrum_III(jPixel,kPixel,iBin) = &
               SpectrumTable_I(iInterval)%Spectrum_III(jPixel,kPixel,iBin) + &
               Flux
       end if
    end do

  end subroutine disperse_line
  !============================================================================

  subroutine read_param

    use ModMPI, ONLY: MPI_COMM_SELF
    use ModReadParam

    character(len=200)      :: NameCommand
    logical                     :: IsNoInstrument = .false., DoEcho = .false.
    integer                     :: iPixel
    character(len=*), parameter:: NameSub = 'read_param'
    !--------------------------------------------------------------------------
    call read_file('SPECTRUM.in')

    call read_init('  ')

    ! Read SPECTRUM.in

    READPARAM: do
       if(.not.read_line(StringLine) ) EXIT READPARAM

       if(.not.read_command(NameCommand)) CYCLE READPARAM

       select case(NameCommand)
       case("#ECHO")
          call read_var('DoEcho',DoEcho)
          if(iProc==0)call read_echo_set(DoEcho)

       case("#VERBOSE")
          call read_var('IsVerbose', IsVerbose)

       case("#DEBUG")
          call read_var('IsDebug', IsDebug)

       case("#OUTFILE")
          call read_var('NameSpectrumFile',NameSpectrumFile)

       case("#DATAFILE")
          IsDataFile = .true.
          call read_var('NameDataFile',NameDataFile)
          call read_var('TypeDataFile',TypeDataFile)

       case("#TABLEFILE")
          call read_var('NameTableFile',NameTableFile)
          call read_var('nMaxLine',nMaxLine)

       case("#WAVELENGTHINTERVAL")
          IsNoInstrument = .true.

          call read_var('nWavelengthInterval',nWavelengthInterval)
          if(IsInstrument)then
             deallocate(WavelengthInterval_II)
             if(iProc==0)write(*,*)'INSTRUMENT intervals changed to WAVELENGTHINTERVALS'
          endif
          allocate(WavelengthInterval_II(2,nWavelengthInterval))
          do iWavelengthInterval=1, nWavelengthInterval
             call read_var('IntervalMin', &
                  WavelengthInterval_II(1,iWavelengthInterval))
             call read_var('IntervalMax', &
                  WavelengthInterval_II(2,iWavelengthInterval))
          end do
          call read_var('SizeWavelengthBin',SizeWavelengthBin)

       case("#DATABLOCK")
          IsDataBlock = .true.
          call read_var('n1',n1Block)
          call read_var('n2',n2Block)
          call read_var('n3',n3Block)

       case("#INSTRUMENT")
          IsInstrument = .true.
          iPixel = 0
          nPixel = 0
          call read_var('NameInstrument',NameInstrument)

          select case(NameInstrument)
          case("EIS")
             IsEIS = .true.
             nWavelengthInterval = 2
             nPixel = 512

             SizeWavelengthBin = 0.0223
             if(IsNoInstrument)then
                if(iProc==0)write(*,*)'INSTRUMENT interval changed to WAVELENGTHINTERVALS'
             else
                allocate(WavelengthInterval_II(2,nWavelengthInterval))
                WavelengthInterval_II(:,1) = [ 170 ,210 ]
                WavelengthInterval_II(:,2) = [ 250 ,290 ]
             endif
          case default
             if(iProc==0)write(*,*) NameSub // ' WARNING: unknown #INSTRUMENT '
          end select

       case("#UNIFORMDATA")
          IsUniData = .true.
          call read_var('IsPpar',IsPpar)
          call read_var('IsPe',IsPe)
          call read_var('RhoUni',RhoUni)
          call read_var('UxUni',UxUni)
          call read_var('UyUni',UyUni)
          call read_var('UzUni',UzUni)
          call read_var('BxUni',BxUni)
          call read_var('ByUni',ByUni)
          call read_var('BzUni',BzUni)
          ! One temperature
          if(.not. IsPe .and. .not. IsPpar)then
             call read_var('TparUni',TparUni)
             TperpUni = TparUni
             TeUni = TparUni
             ! Proton +  electron temperatures
          elseif(IsPe .and. .not. IsPpar)then
             call read_var('TparUni',TparUni)
             TperpUni = TparUni
             call read_var('TeUni',TeUni)
             ! Electron + anisotropic proton temperatures
          elseif(IsPe .and. IsPpar)then
             IsPperp = .true.
             call read_var('TparUni',TparUni)
             call read_var('TperpUni',TperpUni)
             call read_var('TeUni',TeUni)
          else
             if(iProc==0)write(*,*) NameSub // ' WARNING: check temperature setting' // &
                  'Select 1, 2 (proton+electron), or 3 temperature' // &
                  '(electron+anisotropic proton) model!'
          endif
          call read_var('I01Uni',I01Uni)
          call read_var('I02Uni',I02Uni)

       case("#NOALFVEN")
          call read_var('IsNoAlfven',IsNoAlfven)

       case("#ISONEPIXEL")
          call read_var('IsOnePixel',IsOnePixel)
          call read_var('iOnePixel',iOnePixel)
          call read_var('jOnePixel',jOnePixel)
          call read_var('kOnePixel',kOnePixel)

       case("#ISALLLINES")
          call read_var('IsAllLines',IsAllLines) ! Unobserved lines included

       case("#ISDOPPLER")
          call read_var('IsDoppler',IsDoppler)

       case("#ONELINE")
          IsOneLine = .true.
          call read_var('OneLineWavelength',OneLineWavelength)

       case("#LOGTEMIN")
          IsLogTeMin = .true.
          call read_var('LogTeMin',LogTeMin)

       case("#TRANSITIONREGION")
          call read_var('DoExtendTransitionRegion',DoExtendTransitionRegion)
          if(DoExtendTransitionRegion)then
             call read_var('TeModSi',TeModSi)
             call read_var('DeltaTeModSi',DeltaTeModSi)
          end if

       case("#DEM")
          DoDEM = .true.
          DoEM = .true.
          call read_var('NameDEMFile',NameDEMFile)
          call read_var('NameEMFile',NameEMFile)
          call read_var('DoSpectrum',DoSpectrum)
          call read_var('LogTeMinDEM',LogTeMinDEM)
          call read_var('LogTeMaxDEM',LogTeMaxDEM)
          call read_var('DLogTeDEM',DLogTeDEM)

       case("#RESPONSEFUNCTION")
          IsResponseFunction = .true.
          call read_var('NameResponseFunctionFile', NameResponseFunctionFile)

       case("#PROTONELECTRONRATIO")
          call read_var('ProtonElectronRatio',ProtonElectronRatio)

       case("#IONTEMPERATURE")
          call read_var('UseTion',UseTion)
          call read_var('Tion',Tion)

       ! if false, then non-equilibrium charge states are used
       case("#IONFRAC")
          call read_var('UseIonFrac',UseIonFrac)

       case default
          if(iProc==0)write(*,*) NameSub // ' WARNING: unknown #COMMAND '

       end select
    end do READPARAM

    ! In case of Doppler shift applied some lines might appear that would not
    ! otherwise. For this reason we introduce the extended WaveLengthIntervals
    ! for the sake of searching of lines of interest, assuming plasma moves no
    ! faster than 10% of the speed of light.
    if(IsDoppler)then
       allocate(WavelengthIntervalShifted_II(2,nWavelengthInterval))
       WavelengthIntervalShifted_II(1,:) =  WavelengthInterval_II(1,:) * 0.9
       WavelengthIntervalShifted_II(2,:) =  WavelengthInterval_II(2,:) * 1.1
    endif

  end subroutine read_param
  !============================================================================

  subroutine read_data

    use ModPlotFile, ONLY: read_plot_file
    use ModUtilities, ONLY: split_string, lower_case
    use ModCoordTransform, ONLY: rot_matrix_x, rot_matrix_y, rot_matrix_z

    character(len=200)          :: StringHeader
    character(len=200)          :: NameVar

    integer, parameter          :: MaxNameVar = 100
    character(len=20)           :: NameVar_V(MaxNameVar)

    integer                     :: nStep, i, j, k
    integer                     :: k1, k2
    integer                     :: nVarName, iVar

    real                        :: MinWavelength, MaxWavelength
    real                        :: Rot_DD(3,3)
    real                        :: Param_I(9)
    real,allocatable            :: VarIn_VIII(:,:,:,:)
    real                        :: Coord, Dz1, Dz2

    integer                     :: iElement, iCharge, nCharge, iIonFrac = 0
    character(len=4)            :: NameChargestate
    character(len=6)            :: NameChiantiChargestate
    real                        :: IonFracSum
    logical                     :: IsFound = .false.

    character(len=*), parameter:: NameSub = 'read_data'
    !--------------------------------------------------------------------------
    call read_plot_file(NameFile=NameDataFile, &
         TypeFileIn=TypeDataFile,              &
         StringHeaderOut=StringHeader,         &
         nStepOut=nStep,                       &
         nDimOut=nDim,                         &
         nParamOut=nParam,                     &
         ParamOut_I = Param_I,                 &
         nVarOut=nVar,                         &
         n1Out=n1,                             &
         n2Out=n2,                             &
         n3Out=n3,                             &
         NameVarOut=NameVar,                   &
         iErrorOut = iError)

    if(iError /= 0) call CON_stop( &
         NameSub//' could not header from '//trim(NameDataFile))

    allocate(VarIn_VIII(nVar,n1,n2,n3))

    call read_plot_file(NameFile=NameDataFile, &
         TypeFileIn = TypeDataFile,            &
         VarOut_VIII = VarIn_VIII,             &
         CoordMinOut_D = CoordMin_D,           &
         CoordMaxOut_D = CoordMax_D,           &
         iErrorOut = iError)

    if(iError /= 0) call CON_stop( &
         NameSub//' could not read data from '//trim(NameDataFile))

    ! Assign angles of rotated box
    Rot_DD = reshape(Param_I(1:9), [ 3, 3 ])

    ! Assign var names to indexes, drop unused data, convert to SI
    call split_string(NameVar, MaxNameVar, NameVar_V, nVarName)

    if(IsDataBlock)then
       n1 = n1Block
       n2 = n2Block
       n3 = n3Block
       allocate(Var_VIII(nLocalVar,n1,n2,n3))
    else
       allocate(Var_VIII(nLocalVar,n1,n2,n3))
    endif

    ! Set up bins for Spectrum
    allocate(SpectrumTable_I(nWavelengthinterval))
    nBin = 0
    do iWavelengthInterval=1, nWavelengthInterval
       MinWavelength = WavelengthInterval_II(1,iWavelengthInterval)
       MaxWavelength = WavelengthInterval_II(2,iWavelengthInterval)
       nWavelengthBin = nint((MaxWavelength-MinWavelength)/SizeWavelengthBin)
       nBin = nWavelengthBin
       if(.not. IsResponseFunction)then
          allocate(SpectrumTable_I(iWavelengthinterval)%Spectrum_III(n2,n3, &
               nWavelengthBin))
          SpectrumTable_I(iWavelengthinterval)%Spectrum_III(:,:,:)=0.0
       end if
       allocate(SpectrumTable_I(iWavelengthinterval)%SpectrumGrid_I(&
            nWavelengthBin+1))

       SpectrumTable_I(iWavelengthinterval)%nBin=nBin
       do iWavelengthBin=1,nWavelengthBin+1
          ! Values of each wavelength bin correspond to the center of the bin
          SpectrumTable_I(iWavelengthinterval)%SpectrumGrid_I(iWavelengthBin)&
               = MinWavelength + (iWavelengthBin-.5)*SizeWavelengthBin
       end do
    end do

    if(IsResponseFunction)then
       allocate(LOSimage_II(n2,n3))
       LOSImage_II = 0.0
    end if

    if(UseIonFrac)allocate(IonFracTable_I(nIonFracMax))

    if (IsUniData) then
       ! cm^-3 --> kg/m^3
       Var_VIII(rho_,1:n1,1:n2,1:n3)   = rhoUni * 1e6 * cProtonMass
       ! km/s --> m/s
       Var_VIII(ux_,1:n1,1:n2,1:n3)    = uxUni * 1e3
       Var_VIII(uy_,1:n1,1:n2,1:n3)    = uyUni * 1e3
       Var_VIII(uz_,1:n1,1:n2,1:n3)    = uzUni * 1e3
       ! G --> T
       Var_VIII(bx_,1:n1,1:n2,1:n3)    = bxUni * 1e-4
       Var_VIII(by_,1:n1,1:n2,1:n3)    = byUni * 1e-4
       Var_VIII(bz_,1:n1,1:n2,1:n3)    = bzUni * 1e-4
       ! K
       Var_VIII(tpar_,1:n1,1:n2,1:n3)  = tparUni
       Var_VIII(tperp_,1:n1,1:n2,1:n3) = tperpUni
       Var_VIII(te_,1:n1,1:n2,1:n3)    = teUni
       ! erg cm^-3 --> J m^-3
       Var_VIII(I01_,1:n1,1:n2,1:n3)   = I01Uni * 1e-1
       Var_VIII(I02_,1:n1,1:n2,1:n3)   = I02Uni * 1e-1

    else
       do iVar=1, nVar
          if(IsVerbose .and. iProc==0)write(*,*)'NameVar_V(iVar+nDim) = ',&
               NameVar_V(iVar+nDim)

          call lower_case(NameVar_V(iVar+nDim))

          select case(NameVar_V(iVar+nDim))
          case('rho')
             ! g/cm3 --> kg/m3
             Var_VIII(rho_,1:n1,1:n2,1:n3)  = VarIn_VIII(iVar,1:n1,1:n2,1:n3) &
                  * 1e3
          case('ux')
             ! km/s --> m/s
             Var_VIII(ux_,1:n1,1:n2,1:n3)   = VarIn_VIII(iVar,1:n1,1:n2,1:n3) &
                  * 1e3
          case('uy')
             Var_VIII(uy_,1:n1,1:n2,1:n3)   = VarIn_VIII(iVar,1:n1,1:n2,1:n3) &
                  * 1e3
          case('uz')
             Var_VIII(uz_,1:n1,1:n2,1:n3)   = VarIn_VIII(iVar,1:n1,1:n2,1:n3) &
                  * 1e3
          case('bx')
             ! G --> T
             Var_VIII(bx_,1:n1,1:n2,1:n3)   = VarIn_VIII(iVar,1:n1,1:n2,1:n3) &
                  *1e-4
          case('by')
             Var_VIII(by_,1:n1,1:n2,1:n3)   = VarIn_VIII(iVar,1:n1,1:n2,1:n3) &
                  *1e-4
          case('bz')
             Var_VIII(bz_,1:n1,1:n2,1:n3)   = VarIn_VIII(iVar,1:n1,1:n2,1:n3) &
                  *1e-4
          case('ppar')
             ! dyne/cm2 --> N/m2
             ! T = p/rho * M / kB
             Var_VIII(tpar_,1:n1,1:n2,1:n3) = VarIn_VIII(iVar,1:n1,1:n2,1:n3) &
                  * 1e-1 / max(1e-30,Var_VIII(rho_,1:n1,1:n2,1:n3)) * &
                  cProtonMass &
                  / cBoltzmann
             IsPpar = .true.
          case('pperp')
             Var_VIII(tperp_,1:n1,1:n2,1:n3) = VarIn_VIII(iVar,1:n1,1:n2,1:n3)&
                  * 1e-1 / max(1e-30,Var_VIII(rho_,1:n1,1:n2,1:n3)) * &
                  cProtonMass &
                  / cBoltzmann
             IsPperp = .true.
          case('pe')
             Var_VIII(te_,1:n1,1:n2,1:n3)   = VarIn_VIII(iVar,1:n1,1:n2,1:n3) &
                  * 1e-1 / max(1e-30,Var_VIII(rho_,1:n1,1:n2,1:n3)) * &
                  cProtonMass &
                  / cBoltzmann
             IsPe = .true.
          case('p')
             Var_VIII(t_,1:n1,1:n2,1:n3) = VarIn_VIII(iVar,1:n1,1:n2,1:n3) &
                  * 1e-1 / max(1e-30,Var_VIII(rho_,1:n1,1:n2,1:n3)) * &
                  cProtonMass &
                  / cBoltzmann
          case('i01')
             ! erg/cm^3 --> J/m^3
             Var_VIII(I01_,1:n1,1:n2,1:n3)  = VarIn_VIII(iVar,1:n1,1:n2,1:n3) &
                  * 1e-1
          case('i02')
             Var_VIII(I02_,1:n1,1:n2,1:n3)  = VarIn_VIII(iVar,1:n1,1:n2,1:n3) &
                  * 1e-1

          case default
             IsFound = .false.
             ! In case charge states are used in the output file
             if(UseIonFrac)then
                iIonFrac = 0
                ELEMENTLOOP: do iElement = 1, nElement
                   nCharge = iElement+1
                   do iCharge = 1, nCharge
                      iIonFrac = iIonFrac+1
                      if(nCharge < 10) then
                         write(NameChargestate,'(a,i1.1)')&
                              trim(NameElement_I(iElement)),iCharge
                      else
                         write(NameChargestate,'(a,i2.2)')&
                              trim(NameElement_I(iElement)),iCharge
                      end if

                      if(NameVar_V(iVar+nDim) == NameChargestate)then
                         IonFracTable_I(iIonFrac)%NameIonFrac = &
                              NameChiantiChargestate
                         allocate(IonFracTable_I(iIonFrac)%IonFraction_III(1:n1,1:n2,1:n3))
                         IonFracTable_I(iIonFrac)%IonFraction_III(:,:,:)&
                              = VarIn_VIII(iVar,:,:,:)

                         if(iCharge < 10) then
                            write(NameChiantiChargestate,'(a,i1.1)')&
                                 trim(NameElement_I(iElement))//'_',iCharge
                         else
                            write(NameChiantiChargestate,'(a,i2.2)')&
                                 trim(NameElement_I(iElement))//'_',iCharge
                         end if

                         IsFound = .true.
                         EXIT ELEMENTLOOP
                      end if
                   enddo
                enddo ELEMENTLOOP
             endif

             if(.not.IsFound .and. iProc==0) write(*,*) NameSub // ' unused NameVar = '&
                  // NameVar_V(iVar+nDim)
          end select
       end do

       ! Normalize charge states to get fractions
       if(UseIonFrac)then
          iIonFrac = 1
          do iElement = 1, nElement
             nCharge = iElement+1
             if(allocated(IonFracTable_I(iIonFrac)%IonFraction_III))then
                do k = 1, n3 ; do j = 1, n2 ; do i = 1, n1
                   IonFracSum = 0
                   do iCharge = iIonFrac, iIonFrac+iElement
                      IonFracSum = IonFracSum + &
                           IonFracTable_I(iCharge)%IonFraction_III(i,j,k)
                   end do
                   IonFracSum=max(IonFracSum,1e-99)
                   do iCharge = iIonFrac, iIonFrac+iElement
                      IonFracTable_I(iCharge)%IonFraction_III(i,j,k)=&
                           IonFracTable_I(iCharge)%IonFraction_III(i,j,k)&
                           /IonFracSum
                   end do
                end do; end do; end do
             end if
             iIonFrac = iIonFrac + nCharge
          end do
       end if

       if(IsDebug .and. iProc==0)then
          write(*,*)'ux_,uz_,bx_,bz_=', ux_,uz_,bx_,bz_
          write(*,*)'Rot_DD=', Rot_DD
          write(*,*)'Before: u_D=', Var_VIII(ux_:uz_,1,1,1)
       endif

       do k = 1, n3; do j = 1, n2; do i = 1, n1
          Var_VIII(ux_:uz_,i,j,k) = matmul(Var_VIII(ux_:uz_,i,j,k), Rot_DD)
          Var_VIII(bx_:bz_,i,j,k) = matmul(Var_VIII(bx_:bz_,i,j,k), Rot_DD)
       end do; end do; end do

       if(IsDebug .and. iProc==0)write(*,*)'After: u_D=', Var_VIII(ux_:uz_,1,1,1)
    endif

    deallocate(VarIn_VIII)

    ! Copy temperatures if not all components are given
    ! tpar = tper
    if(.not. IsPpar .and. .not. IsPperp)then
       Var_VIII(tpar_,1:n1,1:n2,1:n3) = Var_VIII(t_,1:n1,1:n2,1:n3)
       Var_VIII(tperp_,1:n1,1:n2,1:n3) = Var_VIII(t_,1:n1,1:n2,1:n3)
       ! tperp = (3*t - tpar)/2
    elseif(IsPpar .and. .not. IsPperp)then
       Var_VIII(tperp_,1:n1,1:n2,1:n3) = (3*Var_VIII(t_,1:n1,1:n2,1:n3) &
            - Var_VIII(tpar_,1:n1,1:n2,1:n3))/2.0
       ! tpar = 3*t - 2*tperp
    elseif(.not. IsPpar .and.IsPperp)then
       Var_VIII(tpar_,1:n1,1:n2,1:n3) = 3*Var_VIII(t_,1:n1,1:n2,1:n3) &
            - 2*Var_VIII(tperp_,1:n1,1:n2,1:n3)
    endif
    ! te = t
    if(.not. IsPe)Var_VIII(te_,1:n1,1:n2,1:n3) = Var_VIII(t_,1:n1,1:n2,1:n3)

    if(IsInstrument .and. nPixel /= n3 .and. iProc==0)write(*,*) &
         '!!! nPixel= ',nPixel,' /= n3  = ',n3,' -> we use n3 '

    if(IsNoAlfven)then
       Var_VIII(I01_,1:n1,1:n2,1:n3) = 0
       Var_VIII(I02_,1:n1,1:n2,1:n3) = 0
       if(iProc==0)write(*,*)"IsNoAlfven ON !!!"
    endif

    ! Cells behind the solar disk
    do kPixel = 1, n3
       do jPixel = 1, n2
          do i = 1, n1
             if(CoordMin_D(iDimLOS) + &
                  (i-0.5) * (CoordMax_D(iDimLOS)-CoordMin_D(iDimLOS))/n1 < 0 .and. &
                  (CoordMin_D(iDimVertical) + (jPixel-0.5) * &
                  (CoordMax_D(iDimVertical)-CoordMin_D(iDimVertical))/n2)**2+&
                  (CoordMin_D(iDimHorizontal) + (kPixel-0.5) * &
                  (CoordMax_D(iDimHorizontal)-&
                  CoordMin_D(iDimHorizontal))/n3)**2 < 1 ) &
                  Var_VIII(:,i,jPixel,kPixel) = 0.
          end do
       end do
    end do

    dx = (CoordMax_D(iDimLOS)-CoordMin_D(iDimLOS))/n1 * rSun *1e2
    dy = (CoordMax_D(iDimVertical)-CoordMin_D(iDimVertical))/n2 * rSun *1e2
    dz = (CoordMax_D(iDimHorizontal)-CoordMin_D(iDimHorizontal))/n3 * rSun *1e2

  end subroutine read_data
  !============================================================================

  subroutine read_table

    use ModIoUnit, ONLY: UnitTmp_
    use ModUtilities, ONLY: open_file, close_file

    ! Data read from the file
    character(len=6)            :: NameIon
    integer                     :: nLevelFrom, nLevelTo
    integer                     :: nFirstLevelFrom, nFirstLevelTo
    real                        :: LineWavelength, FirstLineWavelength
    real                        :: LogN, LogT, LogG, Aion, LogIonFrac

    ! End of file indicated by iError /= 0
    integer                     :: iError

    ! Intensity table sized to the maximum
    real, allocatable           :: g_II(:,:), IonFrac_II(:,:)

    ! Size of table for a given wave
    integer                     :: iMin, jMin, iMax, jMax

    ! Density, temperature and wave indexes
    integer                     :: iN, iT, iLine

    ! Switches for header and information for a wavelength of interest
    logical                     :: IsHeader
    logical                     :: DoStore

    !  List of elements and their atomic weight of periodic table
    integer, parameter                   :: nElement = 30
    integer                              :: iTemp=0, jTemp = 0
    character(len=100)                   :: NameElement_I(1:nElement)=&
         ['h ','he','li','be','b ','c ',&
         'n ' ,'o ','f ','ne','na','mg','al', &
         'si','p ','s ','cl','ar','k ','ca','sc','ti','v ','cr','mn','fe', &
         'co','ni','cu','zn']

    real,parameter                     :: AElement_I(1:nElement)=&
         [1.008,  4.003,  6.94 ,  9.012, &
         10.81 , 12.011, 14.007, 15.999, &
         18.998, 20.18 , 22.99 , 24.305, 26.982, &
         28.085, 30.974, 32.06 , 35.45 , 39.948, 39.098, 40.078, 44.956, &
         47.867, 50.942, 51.996, 54.938 ,55.845, &
         58.933, 58.693, 63.546, 65.38]

    character(len=*), parameter:: NameSub = 'read_table'
    !--------------------------------------------------------------------------
    if(IsVerbose .and. iProc==0) write(*,*)'reading table file=', trim(NameTableFile)

    ! Read only wavelength of interest into a nice table
    allocate(LineTable_I(nMaxLine))
    nLineFound          = 0
    iLine               = 0
    nFirstLevelFrom     = -1
    nFirstLevelTo       = -1
    FirstLineWavelength = -1.0
    DoStore             = .false.
    IsHeader            = .true.

    ! Start to read data file
    call open_file(FILE=NameTableFile, STATUS='old')

    ! Read grid size information from header
    READGRID: do
       read(UnitTmp_,'(a)',iostat=iError) StringLine
       if(iError  /= 0)then
          if(iProc==0)write(*,*)'iError = ',iError
          call CON_stop('failed reading header of chianti table')
       end if
       if(StringLine == "#GRID")then
          read(UnitTmp_,*)MinLogN, MaxLogN, DLogN
          read(UnitTmp_,*)MinLogT, MaxLogT, DLogT
          if(IsVerbose .and. iProc==0)then
             write(*,*)'MinLogN, MaxLogN, DLogN = ',MinLogN, MaxLogN, DLogN
             write(*,*)'MinLogT, MaxLogT, DLogT = ',MinLogT, MaxLogT, DLogT
          end if
          EXIT READGRID
       end if
    end do READGRID

    ! Set up maximum size grid to store tabulated values by wavelength
    MinI = nint(MinLogN/DLogN)
    MaxI = nint(MaxLogN/DLogN)
    MinJ = nint(MinLogT/DLogT)
    MaxJ = nint(MaxLogT/DLogT)
    allocate(g_II(MinI:MaxI,MinJ:MaxJ),IonFrac_II(MinI:MaxI,MinJ:MaxJ))

    ! Read remaining portion of table file
    READLOOP: do
       ! Read remaining header lines of table file
       if(IsHeader)then
          read(UnitTmp_,'(a)',iostat=iError) StringLine
          if(iError  /= 0)then
             if(iProc==0)write(*,*)'iError = ',iError
             call CON_stop('failed reading remaining header of chianti table')
          end if
          if(IsVerbose .and. iProc==0) write(*,'(a)') StringLine
          if(StringLine == "#START") IsHeader = .false.
          CYCLE READLOOP
       end if

       ! Read data line
       read(UnitTmp_,*,iostat=iError) &
            NameIon, nLevelFrom, nLevelTo, LineWavelength, &
            LogN, LogT, LogG, LogIonFrac

       if(iError  /= 0 .and. iError /= -1)then
          if(iProc==0)write(*,*)'iError = ',iError
          if(iProc==0)write(*,*)'last line = ',NameIon, nLevelFrom, nLevelTo, &
               LineWavelength, LogN, LogT, LogG, LogIonFrac
          call CON_stop('failed reading chianti table')
       end if

       ! Unobserved lines are stored with negative wavelength
       ! If interested in unobserved lines, use absolute value of wavelength
       if(IsAllLines)then
          if(IsVerbose .and. iProc==0 .and. LineWavelength < 0)&
               write(*,*)'Unobserved line read : ',LineWavelength
          LineWavelength = abs(LineWavelength)
       endif

       ! Check if current line belongs to the same wavelength as previous one
       if(LineWavelength == FirstLineWavelength .and. &
            nLevelFrom == nFirstLevelFrom .and. &
            nLevelTo == nFirstLevelTo .and. &
            iError == 0) then
          ! Calculate index and store extra elements of LogG in oversized g_II
          iN = nint(LogN/DLogN)
          iT = nint(LogT/DLogT)
          g_II(iN,iT) = 10.0**LogG
          IonFrac_II(iN,iT) = 10.0**LogIonFrac
          CYCLE READLOOP
       end if

       if(DoStore)then
          ! Store last indexes as the maximum indexes for N and T
          iMax = iN
          jMax = iT
          LineTable_I(iLine)%iMax = iMax
          LineTable_I(iLine)%jMax = jMax

          ! Extract min indexes into scalars
          iMin = LineTable_I(iLine)%iMin
          jMin = LineTable_I(iLine)%jMin

          ! Create intensity table
          allocate(LineTable_I(iLine)%g_II(iMin:iMax,jMin:jMax))
          LineTable_I(iLine)%g_II = g_II(iMin:iMax,jMin:jMax)

          ! Create equilibrium ionization fractions table
          allocate(LineTable_I(iLine)%IonFrac_II(iMin:iMax,jMin:jMax))
          LineTable_I(iLine)%IonFrac_II = IonFrac_II(iMin:iMax,jMin:jMax)

          ! Storage is done
          DoStore = .false.
       end if

       ! When reached end of file, exit loop
       if(iError /= 0) EXIT READLOOP

       ! If wavelength is different than previous line
       ! pass only if wavelength is inside of wavelengthintervals of interest
       do iWavelengthInterval = 1, nWavelengthInterval
          if(IsDoppler)then
             ! If Doppler shift is calculated, use shifted intervals
             if(LineWavelength < &
                  WavelengthIntervalShifted_II(1,iWavelengthInterval))&
                  CYCLE
             if(LineWavelength > &
                  WavelengthIntervalShifted_II(2,iWavelengthInterval))&
                  CYCLE
          else
             ! If no Doppler shift is calculated, use original intervals
             if(LineWavelength < WavelengthInterval_II(1,iWavelengthInterval))&
                  CYCLE
             if(LineWavelength > WavelengthInterval_II(2,iWavelengthInterval))&
                  CYCLE
          endif

          ! New line of interest found, decide to store it
          iLine      = iLine + 1
          if(IsVerbose .and. iProc==0)write(*,*)iLine,NameIon,LineWavelength
          ! Check if there are too many lines already
          if(iLine > nMaxLine)&
               call CON_stop('Too many lines are found, increase MaxWave')

          ! Change reference line
          FirstLineWavelength = LineWavelength
          nFirstLevelFrom = nLevelFrom
          nFirstLEvelTo = nLevelTo
          DoStore    = .true.
          nLineFound = nLineFound + 1

          ! Store ion name and wavelength
          iTemp = index(NameIon,'_')
          LineTable_I(iLine)%NameIon = NameIon(1:iTemp-1)//NameIon(iTemp+1:5)
          do jTemp = 1, nElement
             ! Pair index of chianti table to state variable
             if(trim(LineTable_I(iLine)%NameIon(1:iTemp-1))==&
                  trim(NameElement_I(jTemp)))then
                Aion = AElement_I(jTemp)
                EXIT
             end if
          enddo

          LineTable_I(iLine)%Aion           = Aion
          LineTable_I(iLine)%nLevelFrom     = nLevelFrom
          LineTable_I(iLine)%nLevelTo       = nLevelTo
          LineTable_I(iLine)%LineWavelength = LineWavelength

          if(IsVerbose .and. iProc==0)write(*,*)'NameIon, LineWavelength = ',NameIon,LineWavelength

          ! Calculate indexes and store as the minimum indexes
          iN                      = nint(LogN/DLogN)
          iT                      = nint(LogT/DLogT)
          LineTable_I(iLine)%StartLogT = LogT
          LineTable_I(iLine)%iMin = iN
          LineTable_I(iLine)%jMin = iT

          ! Initially zero out the input array
          g_II = 0.0
          IonFrac_II = 0.0

          ! Store first element
          g_II(iN,iT) = 10.0**LogG
          IonFrac_II(iN,iT) = 10.0**LogIonFrac

          ! Go on reading the lines corresponding to the wavelength above
          EXIT

       end do

    end do READLOOP
    call close_file

    deallocate(g_II, IonFrac_II)

    if(IsVerbose .and. iProc==0)write(*,*)'nLineFound = ',nLineFound
    if(IsVerbose .and. iProc==0)write(*,*)'nMaxLine = ',nMaxLine

  end subroutine read_table
  !============================================================================
end program spectrum
!==============================================================================
