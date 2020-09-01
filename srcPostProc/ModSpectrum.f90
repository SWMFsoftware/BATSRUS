! The Spectrum code. Integrates the line of sight solar spectrum. Hey it's 
! spectroscopy, not the regular wide band intensity maps! Isn't that so cool?!

! Reference: (Szente et al. 2019 ApJS)
! Introduction:
!   High-resolution spectroscopy is the most accurate tool for measuring the 
!   properties of the solar corona. Synthetic spectra based on the Awsom model 
!   have been studied, showing good agreement with line widths and fluxes of a 
!   few spectral lines measured by the SOHO/SUMER. Here we have something even 
!   better: a full Spectrum suite that allows the user to select any spectral 
!   range and focus on any spectral line available in the CHIANTI database. 
!   Currently it is a postprocessing tool that can calculate the emission from 
!   the optically thin solar corona by combining 3D MHD simulation results with 
!   the CHIANTI database. Doppler-shifted, nonthermal line broadening due to 
!   low-frequency Alfvén waves and anisotropic proton and isotropic electron 
!   temperatures can be individually taken into account during calculations. 
!   Synthetic spectral calculations can then be used for model validation, for 
!   interpretation of solar observations, and for forward modeling purposes.

! Usage:
!   Call this module from Spectrum2 program. Somehow it should magically work, 
!   or not.

! Comments:
!   Hey look here's a message board!

! History:
!   Sep 2016, Judit Szente, Bart van der Holst, Gabor Toth
!   ... ...., fixed lots of bugs
!   May 2018, fully functional now

module ModSpectrum

  use ModConst, ONLY: cAU, cPi, rSun
  use BATL_lib, ONLY: iComm, iProc
  use ModUtilities, ONLY: CON_stop
  use ModMPI

  implicit none
  private
  save
  public:: init_spectrum, spectrum_calc_flux

  ! Variables for DEM and EM calculation
  logical                     :: DoDEM = .false., DoSpectrum = .false.
  logical                     :: DoEM = .false.
  real,allocatable            :: DEM_I(:), Te_I(:), EM_I(:)
  real                        :: LogTeMinDEM, LogTeMaxDEM, DLogTeDEM
  character(len=200)          :: NameDEMFile = 'dem.out'
  character(len=200)          :: NameEMFile = 'em.out'

  ! Logical variables of running modes
  logical                     :: IsVerbose =  .false., IsDebug = .false.
  logical                     :: IsDataFile = .false. ! Use input file or not
  logical                     :: IsLogTeMin = .false.

  integer                     :: iError 

  real                        :: LogTeMin = 3.

  ! Variables for LOS image reconstruction
  logical                     :: IsResponseFunction = .false.
  character(len=200)          :: NameResponseFunctionFile = 'response.out'
  real, allocatable           :: ResponseLambda_I(:), ResponseFactor_I(:), &
       LOSimage_II(:,:)
  integer                     :: nResponseBin

  !One line option
  logical                     :: IsOneLine = .false.
  real                        :: OneLineWavelength

  ! now user can select multiple lines of interest
  ! (without calculating all possible lines in the wavelength range)
  integer :: nSelectLines = 0
  real :: SelectLinesWvlnthRange = 0.6, SelectLinesWvlnthResolution = 0.005
  logical :: DoSelectLinesDopplerRange = .true.
  character(len=6), allocatable :: NameSelectLines_I(:)
  real, allocatable :: WvlnthSelectLines_I(:)

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
  logical                     :: IsDoppler = .false. ! Calculate Doppler shift
  integer                     :: n1Block, n2Block, n3Block ! Define data size
  integer                     :: iOnePixel, jOnePixel, kOnePixel
  ! Rotation angles for box variable rotation
  integer                     :: iDimLOS=1, iDimVertical=2, iDimHorizontal=3

  ! Variables to read solar wind data file in
  integer                     :: nVar   ! Number of variables   
  integer                     :: nDim   ! Number of dimensions  
  integer                     :: nParam ! Number of parameters  
  integer                     :: n1, nImagePixelJ, nImagePixelK ! Data box size
  real                        :: CoordMin_D(3), CoordMax_D(3)        
  real,allocatable            :: Var_VIII(:,:,:,:)
  ! For H:He 10:1 fully ionized plasma the proton:electron ratio is 1/(1+2*0.1)
  real                        :: ProtonElectronRatio = 0.83
  real                        :: dx = 1., dy = 1., dz = 1.
  !, dVperd2 = 1., dOmega= 1.

  ! Extend transition region
  logical :: DoExtendTransitionRegion = .false.
  real :: TeModSi = 3.0E+5    !K
  real :: DeltaTeModSi = 1E+4 !K

  ! Variables for uniform data
  logical                     :: IsUniData = .false. ! Overwrite data w/ const
  real                        :: RhoUni, UxUni, UyUni, UzUni, BxUni, ByUni
  real                        :: BzUni
  real                        :: TparUni, TperpUni, TeUni, I01Uni, I02Uni

  ! Indexes for the solar wind variables, for local variables
  integer, parameter          :: &
      Rho_  =  1, &              
      Ux_   =  2, &                  
      Uy_   =  3, &               
      Uz_   =  4, & 
      Bx_   =  5, &
      By_   =  6, &
      Bz_   =  7, &
      Tpar_ =  8, &
      Tperp_=  9, &
      Te_   = 10, &
      T_    = 11, &
      I01_  = 12, &
      I02_  = 13

  integer, parameter          :: nSpectrumVar = I02_ ! Number of used variables

  integer :: iVarMhd2Spec_I(nSpectrumVar), nPixelProc

  logical :: UseTAnisotropy = .false., UseAlfven = .false.


  ! Variables for the wavelengths of interest
  logical                     :: IsAllLines = .false. ! Ignore unobserved lines
  integer                     :: iWavelengthInterval, nWavelengthInterval = 0
  integer                     :: nMaxLine, nLineFound
  real, allocatable           :: WavelengthInterval_II(:,:) 

  ! For Doppler-shift wavelength intervals are shifted by 10% of speed of light
  real, allocatable           :: WavelengthIntervalShifted_II(:,:) 

  ! Temperature and density grid for contrbution function (G)
  real                        :: DLogN, DLogT
  real                        :: MinLogN, MinLogT, MaxLogN, MaxLogT
  integer :: iNMin, iNMax, jTMin, jTMax

  ! Variables for lines
  integer                     :: nBin ! Wavelength bin number  
  integer                     :: i, iLine
  integer                     :: nLineAll ! All lines of interest

  ! Derived type to read tabulated G values
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
  end type LineTableType

  type(LineTableType), allocatable :: LineTable_I(:)

  ! Derived type of output 
  type SpectrumTableType
     integer                  :: nBin     
     real,allocatable         :: Spectrum_II(:,:), SpectrumGrid_I(:)
  end type SpectrumTableType

  type(SpectrumTableType), allocatable, public :: SpectrumTable_I(:)

  ! For MPI_reduce in the main program 

  integer :: iInterval

  character(len=*), parameter :: NameSub = 'spectrum.f90'

  !---------------------------------------------------------------------------

contains

  subroutine init_spectrum(StatePixelSegProc_VII, nLosSeg_I)
    use ModVarIndexes, ONLY: NameVar_V, MhdRho_=>Rho_, MhdRhoU_=>RhoU_, &
        MhdB_=>B_, WaveFirst_, MhdPe_=>Pe_, MhdP_=>p_, MhdPpar_=>Ppar_
    use ModConst, ONLY: cLightSpeed

    real, intent(inout):: StatePixelSegProc_VII(:,:,:)
    integer, intent(in):: nLosSeg_I(:)
    real :: MinWavelength, MaxWavelength, u, uMax, uMin
    logical :: UsePe = .false.
    integer :: iPixel, jLos, i, j
    real :: TmpWvlnthSwap_I(2)
    real, allocatable :: TmpWvlnthIntvl_I(:,:)

    character(len=*), parameter :: NameSub = 'init_spectrum'

    call timing_start(NameSub)

    nPixelProc = size(nLosSeg_I)
    ! We should already have all the interpolated and rotated state variables.
    ! Check if we have temperature anisotropy or Alfven waves.
    if (trim(NameVar_V(MhdPe_)) == 'Pe') UsePe = .true.
    if (trim(NameVar_V(MhdPpar_)) == 'Ppar') UseTAnisotropy = .true.
    if (trim(NameVar_V(WaveFirst_)) == 'I01') UseAlfven = .true.

    call read_param

    ! make connections between Mhdrus state variables and Spectrum variables
    iVarMhd2Spec_I(Rho_) = MhdRho_
    iVarMhd2Spec_I(Ux_:Uz_) = MhdRhoU_ + [1,2,3]
    iVarMhd2Spec_I(Bx_:Bz_) = MhdB_ + [1,2,3]
    iVarMhd2Spec_I(Tpar_) = merge(MhdPpar_,MhdP_,UseTAnisotropy)
    iVarMhd2Spec_I(Tperp_) = 1
    iVarMhd2Spec_I(Te_) = merge(MhdPe_,MhdP_,UsePe)
    iVarMhd2Spec_I(T_) = MhdP_
    iVarMhd2Spec_I(I01_:I02_) = WaveFirst_ + [0,1]

    ! In case of Doppler shift applied some lines might appear that would not
    ! otherwise. We introduce the extended WaveLengthIntervals for the sake of 
    ! searching of lines of interest, estimate with largest los velocity.
    uMin = 0.
    uMax = 0.
    if (IsDoppler) then
      do iPixel = 1, nPixelProc; do jLos = 1, nLosSeg_I(iPixel)
        u = StatePixelSegProc_VII(MhdRhoU_+1,jLos,iPixel)/StatePixelSegProc_VII(MhdRho_,jLos,iPixel)
        if (u > uMax) uMax = u
        if (u < uMin) uMin = u
      enddo; enddo
    endif
    if (nWavelengthInterval>0) then
      allocate(WavelengthIntervalShifted_II(2,nWavelengthInterval))
      WavelengthIntervalShifted_II(1,:) = WavelengthInterval_II(1,:) * (1-uMax/cLightSpeed)
      WavelengthIntervalShifted_II(2,:) = WavelengthInterval_II(2,:) * (1-uMin/cLightSpeed)
    endif

    ! The response function thing seems to be similar to what is currently
    ! available in WritePlotLos for the EUV images. Need to check later what
    ! it actually does.
    if(IsResponseFunction)then
       call read_responsefunction
    endif

    call read_table
    ! This is only useful when reading Chianti table, not needed after here
    if (allocated(WavelengthIntervalShifted_II)) deallocate(WavelengthIntervalShifted_II)

    ! Currently nLineFound is always less than nMaxLine
    ! Potentially in future can automatically pick only the strongest nMaxLine lines
    nLineAll = min(nMaxLine,nLineFound)
    if (nLineAll==0) call stop_mpi('No Chianti lines found.')
    if (iProc==0) write(*,'(A,I0,A)') 'A total of ', nLineAll, ' Chianti lines found.'

    ! automatic wavelength interval selection based on selected lines
    if (nWavelengthinterval==0) then
      allocate(TmpWvlnthIntvl_I(2,nLineAll))
      do i = 1, nLineAll
        TmpWvlnthIntvl_I(1,i) = LineTable_I(i)%LineWavelength - SelectLinesWvlnthRange/2.
        TmpWvlnthIntvl_I(2,i) = LineTable_I(i)%LineWavelength + SelectLinesWvlnthRange/2.
        if (IsDoppler .and. DoSelectLinesDopplerRange) then
          TmpWvlnthIntvl_I(1,i) = TmpWvlnthIntvl_I(1,i) * (1-uMax/cLightSpeed)
          TmpWvlnthIntvl_I(2,i) = TmpWvlnthIntvl_I(2,i) * (1-uMin/cLightSpeed)
        endif
      enddo
      do i = 2, nLineAll
        do j = nLineAll, i, -1
          if (TmpWvlnthIntvl_I(1,j)<TmpWvlnthIntvl_I(1,j-1)) then
            TmpWvlnthSwap_I = TmpWvlnthIntvl_I(:,j)
            TmpWvlnthIntvl_I(:,j) = TmpWvlnthIntvl_I(:,j-1)
            TmpWvlnthIntvl_I(:,j-1) = TmpWvlnthSwap_I
          endif
        enddo
      enddo
      i = 1
      j = 2
      nWavelengthInterval = 1
      do while(j<=nLineAll)
        if (TmpWvlnthIntvl_I(2,i)+SelectLinesWvlnthResolution>=TmpWvlnthIntvl_I(1,j)) then
          TmpWvlnthIntvl_I(2,i) = max(TmpWvlnthIntvl_I(2,i),TmpWvlnthIntvl_I(2,j))
          TmpWvlnthIntvl_I(:,j) = -1
        else
          nWavelengthinterval = nWavelengthinterval + 1
          i = j
        endif
        j = j + 1
      enddo
      if (allocated(WavelengthInterval_II)) deallocate(WavelengthInterval_II)
      allocate(WavelengthInterval_II(2,nWavelengthInterval))
      j = 0
      do i = 1, nLineAll
        if (TmpWvlnthIntvl_I(1,i)<0) CYCLE
        j = j + 1
        WavelengthInterval_II(:,j) = TmpWvlnthIntvl_I(:,i)
      enddo
      deallocate(TmpWvlnthIntvl_I)
      SizeWavelengthBin = SelectLinesWvlnthResolution
      if (IsVerbose) then
        write(*,*)
        write(*,*) 'Automatically selected wavelength interval:'
        write(*,*)
        write(*,'(A)') '#WAVELENGTHINTERVAL'
        write(*,'(I12,A)') nWavelengthInterval, achar(9)//achar(9)//'nWavelengthInterval'
        do i = 1, nWavelengthInterval
          write(*,'(F12.4,A)') WavelengthInterval_II(1,i), achar(9)//achar(9)//'IntervalMin'
          write(*,'(F12.4,A)') WavelengthInterval_II(2,i), achar(9)//achar(9)//'IntervalMax'
        enddo
        write(*,'(F12.4,A)') SizeWavelengthBin, achar(9)//achar(9)//'SizeWavelengthBin'
        write(*,*)
      endif
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
          allocate(SpectrumTable_I(iWavelengthinterval)% &
            Spectrum_II(nPixelProc,nWavelengthBin))
          SpectrumTable_I(iWavelengthinterval)%Spectrum_II = 0.0
       end if
       allocate(SpectrumTable_I(iWavelengthinterval)%SpectrumGrid_I(&
            nWavelengthBin+1))

       SpectrumTable_I(iWavelengthinterval)%nBin = nBin
       do iWavelengthBin=1,nWavelengthBin+1
          ! Values of each wavelength bin correspond to the center of the bin
          SpectrumTable_I(iWavelengthinterval)%SpectrumGrid_I(iWavelengthBin)&
               = MinWavelength + (iWavelengthBin-.5)*SizeWavelengthBin
       end do
    end do

    call timing_stop(NameSub)

  end subroutine init_spectrum

  subroutine read_param
    use ModReadParam

    character(len=200)      :: NameCommand
    logical                     :: IsNoInstrument = .false., DoEcho = .false.
    logical :: UseAlfven2
    integer                     :: iPixel
    character(len=*), parameter :: NameSub='read_param'
    !------------------------------------------------------------------------
    
    call read_file('SPECTRUM.in')
    call read_init('  ') 

    ! Read SPECTRUM.in
    READPARAM: do
      if(.not.read_line(StringLine) ) EXIT READPARAM
      if(.not.read_command(NameCommand)) CYCLE READPARAM

      select case(NameCommand)
      case("#ECHO")
        call read_var('DoEcho',DoEcho)
        call read_echo_set(DoEcho .and. iProc==0)

      case("#VERBOSE")
        call read_var('IsVerbose', IsVerbose)
        IsVerbose = (iProc == 0) .and. IsVerbose

      case("#DEBUG")
        call read_var('IsDebug', IsDebug)
        IsDebug = IsDebug .and. (iProc == 0)

      case("#TABLEFILE")
        call read_var('NameTableFile',NameTableFile)
        call read_var('nMaxLine',nMaxLine)

      case("#WAVELENGTHINTERVAL")
        IsNoInstrument = .true.

        call read_var('nWavelengthInterval',nWavelengthInterval)
        if (nWavelengthInterval<=0) then
          call stop_mpi('Error: Please specify a wavelength interval; or use #INSTRUMENT; ' &
              // 'or use #SELECTLINES and drop this tag to automatically select interval.')
        endif
        if(IsInstrument)then
           deallocate(WavelengthInterval_II)
           write(*,*)'INSTRUMENT intervals changed to WAVELENGTHINTERVALS'
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
        call read_var('nImagePixelJ',n2Block)
        call read_var('nImagePixelK',n3Block)

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
              write(*,*)'INSTRUMENT interval changed to WAVELENGTHINTERVALS'
           else
              allocate(WavelengthInterval_II(2,nWavelengthInterval))
              WavelengthInterval_II(:,1) = (/ 170 ,210 /)
              WavelengthInterval_II(:,2) = (/ 250 ,290 /)
           endif
        case default
           write(*,*) NameSub // ' WARNING: unknown #INSTRUMENT '
        end select

      ! case("#UNIFORMDATA")
      !   IsUniData = .true.
      !   call read_var('IsPpar',IsPpar)
      !   call read_var('IsPe',IsPe)
      !   call read_var('RhoUni',RhoUni)
      !   call read_var('UxUni',UxUni)
      !   call read_var('UyUni',UyUni)
      !   call read_var('UzUni',UzUni)
      !   call read_var('BxUni',BxUni)
      !   call read_var('ByUni',ByUni)
      !   call read_var('BzUni',BzUni)
      !   ! One temperature
      !   if(.not. IsPe .and. .not. IsPpar)then
      !     call read_var('TparUni',TparUni)
      !     TperpUni = TparUni
      !     TeUni = TparUni
      !     ! Proton +  electron temperatures
      !   elseif(IsPe .and. .not. IsPpar)then
      !     call read_var('TparUni',TparUni)
      !     TperpUni = TparUni
      !     call read_var('TeUni',TeUni)
      !     ! Electron + anisotropic proton temperatures
      !   elseif(IsPe .and. IsPpar)then
      !     IsPperp = .true.
      !     call read_var('TparUni',TparUni)
      !     call read_var('TperpUni',TperpUni)
      !     call read_var('TeUni',TeUni)
      !   else 
      !     write(*,*) NameSub // ' WARNING: check temperature setting' // &
      !         'Select 1, 2 (proton+electron), or 3 temperature' // & 
      !         '(electron+anisotropic proton) model!' 
      !   endif
      !   call read_var('I01Uni',I01Uni)
      !   call read_var('I02Uni',I02Uni)

      case("#ALFVEN")
        call read_var('UseAlfven',UseAlfven2)
        if (.not.UseAlfven .and. UseAlfven2) then
          write(*,*) 'Warning: no IO1 variable in equation, switching off Alfven'
        endif
        UseAlfven = UseAlfven .and. UseAlfven2

      case("#ISONEPIXEL")
        call read_var('IsOnePixel',IsOnePixel)
        call read_var('iOnePixel',iOnePixel)
        call read_var('jOnePixel',jOnePixel)
        call read_var('kOnePixel',kOnePixel)

      case("#ISALLLINES")
        call read_var('IsAllLines',IsAllLines) ! Unobserved lines included

      case("#ISDOPPLER")
        call read_var('IsDoppler',IsDoppler)

      ! case("#ONELINE")
      !   IsOneLine = .true.
      !   call read_var('OneLineWavelength',OneLineWavelength)

      case("#SELECTLINES")
        call read_var('nSelectLines',nSelectLines)
        if (nSelectLines > 0) then
          call read_process_select_lines
        endif

      case("#SELECTLINESOPTION")
        call read_var('SelectLinesWvlnthRange',SelectLinesWvlnthRange)
        call read_var('SelectLinesWvlnthResolution',SelectLinesWvlnthResolution)
        call read_var('DoSelectLinesDopplerRange',DoSelectLinesDopplerRange)

      case("#LOGTEMIN")
        IsLogTeMin = .true.
        call read_var('LogTeMin',LogTeMin)

      case("#TRANSITIONREGION")
        call read_var('DoExtendTransitionRegion',DoExtendTransitionRegion)
        if(DoExtendTransitionRegion)then
           call read_var('TeModSi',TeModSi)
           call read_var('DeltaTeModSi',DeltaTeModSi)
        end if

      ! case("#DEM")
      !   DoDEM = .true.
      !   DoEM = .true.
      !   call read_var('NameDEMFile',NameDEMFile)
      !   call read_var('NameEMFile',NameEMFile)
      !   call read_var('DoSpectrum',DoSpectrum)
      !   call read_var('LogTeMinDEM',LogTeMinDEM)
      !   call read_var('LogTeMaxDEM',LogTeMaxDEM)
      !   call read_var('DLogTeDEM',DLogTeDEM)

      ! case("#RESPONSEFUNCTION")
      !   IsResponseFunction = .true.
      !   call read_var('NameResponseFunctionFile', NameResponseFunctionFile)

      case("#PROTONELECTRONRATIO")
        call read_var('ProtonElectronRatio',ProtonElectronRatio)

      case default
        ! write(*,*) NameSub // ' WARNING: unknown #COMMAND '

      end select
    end do READPARAM

  contains
    subroutine read_process_select_lines
      use ModUtilities, ONLY: split_string, lower_case
      integer :: i, j, nStrParts, iC, nStrLen
      logical :: IsUnderScore
      character(len=20) :: NameSelectLine, NameSelectLineTmp_I(2)
      allocate(NameSelectLines_I(nSelectLines))
      allocate(WvlnthSelectLines_I(nSelectLines))
      do i = 1, nSelectLines
        call read_var('NameSelectLine',NameSelectLine)
        call split_string(NameSelectLine,NameSelectLineTmp_I,nStrParts,' ')
        if (nStrParts<2) then
          call stop_mpi("Error: wrong input for select line: use format e.g., fe_8 186.598")
        endif
        call lower_case(NameSelectLineTmp_I(1))
        IsUnderScore = .false.
        nStrLen = len_trim(NameSelectLineTmp_I(1))
        do j = 1, nStrLen
          ! very basic processing, accepts input of ion name without underscore
          iC = ichar(NameSelectLineTmp_I(1)(j:j))
          if (iC==ichar('_')) then
            IsUnderScore = .true.
          else if (iC>=ichar('0') .and. iC<=ichar('9')) then
            if (j==1) then
              call stop_mpi("Error: need ion name in front of wavelength: use format e.g., fe_8 186.598")
            endif
            if (.not.IsUnderScore) then
              NameSelectLineTmp_I(1)(j+1:nStrLen+1) = NameSelectLineTmp_I(1)(j:nStrLen)
              NameSelectLineTmp_I(1)(j:j) = '_'
              EXIT
            endif
          endif
        enddo
        NameSelectLines_I(i) = trim(NameSelectLineTmp_I(1))
        read(NameSelectLineTmp_I(2), *) WvlnthSelectLines_I(i)
      enddo

    end subroutine read_process_select_lines

  end subroutine read_param


  !==========================================================================
  subroutine read_responsefunction

    use ModPlotFile,         ONLY: read_plot_file

    character(len=*), parameter    :: NameSub = 'read_response'
    !------------------------------------------------------------------------ 

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


  !==========================================================================
  subroutine read_table

    use ModIoUnit, ONLY: UnitTmp_
    use ModUtilities, ONLY: open_file, close_file

    ! Data read from the file
    character(len=6)            :: NameIon
    integer                     :: nLevelFrom, nLevelTo
    integer                     :: nFirstLevelFrom, nFirstLevelTo
    real                        :: LineWavelength, FirstLineWavelength
    real                        :: LogN, LogT, LogG, Aion 

    ! End of file indicated by iError /= 0
    integer                     :: iError

    ! Intensity table sized to the maximum
    real, allocatable           :: g_II(:,:)

    ! Size of table for a given wave
    integer                     :: iMin, jMin, iMax, jMax

    ! Density, temperature and wave indexes
    integer                     :: iN, iT, iLine, k

    ! Switches for header and information for a wavelength of interest
    logical                     :: IsHeader, IsFound, IsMatch
    logical                     :: DoStore  

    character(len=*), parameter :: NameSub='read_table'
    !------------------------------------------------------------------------
    call timing_start(NameSub)
    if(IsVerbose) write(*,*)'reading table file=', trim(NameTableFile)

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
        write(*,*)'iError = ',iError
        call CON_stop('failed reading header of chianti table')
      endif
      if(StringLine == "#GRID")then
        read(UnitTmp_,*)MinLogN, MaxLogN, DLogN
        read(UnitTmp_,*)MinLogT, MaxLogT, DLogT
        if(IsVerbose)then
          write(*,'(A,2F12.2,F12.4)') 'MinLogN, MaxLogN, DLogN = ',MinLogN, MaxLogN, DLogN 
          write(*,'(A,2F12.2,F12.4)') 'MinLogT, MaxLogT, DLogT = ',MinLogT, MaxLogT, DLogT
        endif
        EXIT READGRID
      end if
    end do READGRID

    ! Set up maximum size grid to store tabulated values by wavelength
    iNMin = nint(MinLogN/DLogN)
    iNMax = nint(MaxLogN/DLogN)
    jTMin = nint(MinLogT/DLogT)
    jTMax = nint(MaxLogT/DLogT)
    allocate(g_II(iNMin:iNMax,jTMin:jTMax))

    if (IsVerbose) then
      if (nWavelengthInterval>0) &
        write(*,*) 'Wavelength range (shifted if Doppler):'
      do iWavelengthInterval = 1, nWavelengthInterval
        write(*,'(2F12.3)',advance='no') WavelengthInterval_II(:,iWavelengthinterval)
        if (IsDoppler) then
          write(*,'(A,2F12.3)') '  =>  ', WavelengthIntervalShifted_II(:,iWavelengthinterval)
        else
          write(*,*)
        endif
      enddo
    endif

    ! Read remaining portion of table file
    READLOOP: do
       ! Read remaining header lines of table file
       if(IsHeader)then
          read(UnitTmp_,'(a)',iostat=iError) StringLine
          if(iError  /= 0)then
             write(*,*)'iError = ',iError
             call CON_stop('failed reading remaining header of chianti table')
          end if
          if(IsVerbose) write(*,'(a)') StringLine
          if(StringLine == "#START") IsHeader = .false.
          CYCLE READLOOP
       end if

       ! Read data line
       read(UnitTmp_,*,iostat=iError) &
            NameIon, Aion, nLevelFrom, nLevelTo, LineWavelength, &
            LogN, LogT, LogG

       if(iError  /= 0 .and. iError /= -1)then
          write(*,*)'iError = ',iError
          write(*,*)'last line = ',NameIon, Aion, nLevelFrom, nLevelTo, &
               LineWavelength, LogN, LogT, LogG
          call CON_stop('failed reading chianti table')
       end if

       ! Unobserved lines are stored with negative wavelength
       ! If interested in unobserved lines, use absolute value of wavelength
       if(IsAllLines)then
          if(IsVerbose .and. LineWavelength < 0)&
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

          ! Storage is done
          DoStore = .false.
       end if

       ! When reached end of file, exit loop
       if(iError /= 0) EXIT READLOOP

       IsFound = .false.
       if (nWavelengthInterval>0) then
         ! If wavelength is different than previous line
         ! pass only if wavelength is inside of wavelengthintervals of interest
         do iWavelengthInterval = 1, nWavelengthInterval
            if (LineWavelength<WavelengthIntervalShifted_II(1,iWavelengthInterval)) CYCLE
            if (LineWavelength>WavelengthIntervalShifted_II(2,iWavelengthInterval)) CYCLE
            ! if (IsOneLine .and. LineWavelength /= OneLineWavelength) CYCLE
            if (nSelectLines>0) then
              IsMatch = .false.
              do k = 1, nSelectLines
                if (trim(NameSelectLines_I(k))==trim(NameIon) .and. WvlnthSelectLines_I(k)==LineWavelength) then
                  IsMatch = .true.
                  EXIT
                endif
              enddo
              if (.not. IsMatch) CYCLE
            endif
            IsFound = .true.
            EXIT
         end do
       else
         do k = 1, nSelectLines
           if (trim(NameSelectLines_I(k))==trim(NameIon) .and. WvlnthSelectLines_I(k)==LineWavelength) then
             IsFound = .true.
             EXIT
           endif
         enddo
       endif

       if (IsFound) then
          ! New line of interest found, decide to store it
          iLine      = iLine + 1
          if (IsVerbose) write(*,'(I5,A7,F12.3)')iLine,NameIon,LineWavelength
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
          LineTable_I(iLine)%NameIon        = NameIon
          LineTable_I(iLine)%Aion           = Aion
          LineTable_I(iLine)%nLevelFrom     = nLevelFrom
          LineTable_I(iLine)%nLevelTo       = nLevelTo
          LineTable_I(iLine)%LineWavelength = LineWavelength

          ! Calculate indexes and store as the minimum indexes
          iN                      = nint(LogN/DLogN)
          iT                      = nint(LogT/DLogT)
          LineTable_I(iLine)%StartLogT = LogT
          LineTable_I(iLine)%iMin = iN
          LineTable_I(iLine)%jMin = iT

          ! Initially zero out the input array
          g_II = 0.0

          ! Store first element
          g_II(iN,iT) = 10.0**LogG

       endif

    end do READLOOP
    call close_file

    deallocate(g_II)

    if(IsVerbose)write(*,*)'nLineFound = ',nLineFound
    if(IsVerbose)write(*,*)'nMaxLine = ',nMaxLine
    call timing_stop(NameSub) 

  end subroutine read_table

  !==========================================================================
  subroutine spectrum_calc_flux(StatePixelSegProc_VII, nLosSeg_I)
    use ModConst, ONLY: cBoltzmann, cProtonMass, cLightSpeed, cTiny
    use ModInterpolate, ONLY: bilinear
    use ModUtilities, ONLY: norm2

    real, intent(in):: StatePixelSegProc_VII(:,:,:)
    integer, intent(in):: nLosSeg_I(:)

    integer                        :: iInterval, iBin, iCenter
    integer :: Ds_, iPixel, jLos
    real :: Var_I(nSpectrumVar), dLosLength, XyzSeg_D(3)
    real                           :: FluxMono
    real                           :: Lambda, LambdaSI, Lambda0SI, DLambdaSI
    real                           :: DLambda, DLambdaSI2, Lambda_shifted
    real                           :: DLambdaInstr2 = 0.0 !!! do it later
    real                           :: Zplus2, Zminus2, Cos2Alpha, Sin2Alpha
    real                           :: Unth2, Uth2
    real                           :: Gint, LogNe, LogTe, Rho
    real                           :: Tlos
    real                           :: Aion
    real                           :: TShift
    character(len=*), parameter    :: NameSub='spectrum_calc_flux'
    !------------------------------------------------------------------------
    call timing_start(NameSub)

    Ds_ = size(StatePixelSegProc_VII,1)

    do iLine = 1, nLineAll
      Aion     = LineTable_I(iLine)%Aion   
      Lambda   = LineTable_I(iLine)%LineWavelength
      ! if (IsOneLine .and. Lambda /= OneLineWavelength) CYCLE

      if (IsDebug) then
        write(*,*) '######============================######'
        write(*,'(A,F10.3,A,E10.3)') trim(LineTable_I(iLine)%NameIon), Lambda, ', Aion= ', Aion
        write(*,*)'jTMin, jTMax = ', jTMin, jTMax
        write(*,*)'iNMin, iNMax = ', iNMin, iNMax
        write(*,*)'DLogN, DLogT = ', DLogN, DLogT
        write(*,*) '+=-----------------------+'
        write(*,'(2A5,A8,2A12,3A7,5A10,A12)') 'i', 'j', 'lgG', 'Flux*ds', 'ds', 'LgNe', &
            'LgTe', 'lgTls', 'Uth_kms', 'Unth_kms', 'DLambda', 'Vx_kms', '|B|_G', 'Rs'
      endif

      do iPixel = 1, nPixelProc; do jLos = 1, nLosSeg_I(iPixel)
        dLosLength = StatePixelSegProc_VII(Ds_,jLos,iPixel)
        XyzSeg_D = StatePixelSegProc_VII(Ds_-3:Ds_-1,jLos,iPixel)
        Var_I = StatePixelSegProc_VII(iVarMhd2Spec_I,jLos,iPixel)
        ! convert Mhd variables to the ones spectrum use, eg, RhoUx->Ux, P->T
        Var_I(Ux_:Uz_) = Var_I(Ux_:Uz_)/Var_I(Rho_)
        Var_I(T_) = Var_I(T_)/Var_I(Rho_)*cProtonMass/cBoltzmann
        Var_I(Te_) = Var_I(Te_)/Var_I(Rho_)*cProtonMass/cBoltzmann
        if (UseTAnisotropy) then
          Var_I(Tpar_) = Var_I(Tpar_)/Var_I(Rho_)*cProtonMass/cBoltzmann
          Var_I(Tperp_) = (3*Var_I(T_)-Var_I(Tpar_))/2.
        endif

        ! Doppler shift while x axis is oriented towards observer
        if(IsDoppler)then
          Lambda = ( 1 - Var_I(Ux_)/cLightSpeed ) * Lambda
        endif

        ! Find which interval wavelength belongs to
        iInterval = -1
        do iWavelengthInterval = 1, nWavelengthInterval
          if ((Lambda <= WavelengthInterval_II(2,iWavelengthInterval)) .and. &
              (Lambda >= WavelengthInterval_II(1,iWavelengthInterval))) then
            iInterval = iWavelengthInterval
            EXIT
          end if
        end do

        ! after Doppler shift the line may be outside interested wavelength range
        ! not considering broadening here
        if (iInterval == -1) CYCLE

        ! reconsider this part later, shouldn't iBin be easily calculated without a loop?
        do iBin = 1, nBin
          if ((Lambda > SpectrumTable_I(iInterval)%SpectrumGrid_I(iBin)) .and. &
              (Lambda < SpectrumTable_I(iInterval)%SpectrumGrid_I(iBin+1))) then
            iCenter = iBin
            EXIT
          end if
          if (SpectrumTable_I(iInterval)%SpectrumGrid_I(iBin+1) > &
              WavelengthInterval_II(2,iInterval)) EXIT
        end do

        Rho = Var_I(Rho_)

        ! Convert from kg m^-3 to kg cm^-3 (*1e-6)
        ! and divide by cProtonMass in kg so Ne is in cm^-3
        ! 1 : 0.83  electron to proton ratio is assumed
        LogNe = log10(Rho*1e-6/cProtonMass/ProtonElectronRatio)
        LogTe = log10(Var_I(Te_))

        if(IsLogTeMin .and. (LogTe < LogTeMin)) CYCLE

        ! Get the contribution function
        Gint = bilinear(LineTable_I(iLine)%g_II(:,:), iNMin, iNMax, jTMin, jTMax, &
            [LogNe/DLogN,LogTe/DLogT], DoExtrapolate=.false.)
        ! When Gint becomes negative due to extrapolation -> move to next
        if (Gint<=0) CYCLE 

        ! Angle of magnetic field relative to X is Alpha. Cos(Alpha)=Bx/B
        Cos2Alpha = Var_I(Bx_)**2/max(sum(Var_I(Bx_:Bz_)**2), 1e-30)
        Sin2Alpha = 1 - Cos2Alpha
        
        if (UseTAnisotropy) then
          ! Calculate angle between LOS and B directions
          ! Calculate temperature relative to the LOS direction
          Tlos = Sin2Alpha * Var_I(Tperp_) + Cos2Alpha * Var_I(Tpar_)
        else
          Tlos = Var_I(T_)
        endif
        ! Calculate thermal broadening
        Uth2  = cBoltzmann * Tlos/(cProtonMass * Aion)  

        if (UseAlfven) then
          ! Calculate Elzasser variables
          Zplus2   = Var_I(I01_) * 4.0 / Rho
          Zminus2  = Var_I(I02_) * 4.0 / Rho
          ! non-thermal broadening
          Unth2 = 1.0/16.0 * (Zplus2 + Zminus2) * Sin2Alpha
        else
          Unth2 = 0.
        endif

        ! Convert to SI
        LambdaSI = LineTable_I(iLine)%LineWavelength * 1e-10 
        ! Add thermal and non-thermal broadening
        DLambdaSI2 = LambdaSI**2 * (Uth2 + Unth2)/cLightSpeed**2

        ! Add 70 mA for Hinode/EIS 2'' slit
        ! 70 mA is in FWHM, that's 7e-12 m
        ! FWHM = 2sqrt(2ln2)*sigma
        ! sigma = FWHM/(2sqrt(2ln2))
        ! sigma^2 = (7e-12)^2 /(4*2*ln2)
        if (IsEIS) DLambdaInstr2 = (7e-12)**2/(8*log(2.)) 
        ! Add instrumental broadening (if any)            
        if(IsInstrument)DLambdaSI2 = DLambdaSI2 + DLambdaInstr2

        ! Convert [m] --> [A]
        DLambdaSI = sqrt(DLambdaSI2)
        DLambda   = DLambdaSI * 1e10


        ! Calculate flux and spread it on the Spectrum_II grids

        ! Constant multiplier converted from SI [m] to CGS units [cm]
        ! dV_cell / (1AU)^2 * 1e2
        ! dVperd2 = dx*dy*dz /(1.496e11)**2 * 1e2

        ! Solid angle obtained by the emitting surface at 1AU distance
        ! dOmega = dy*dz / (1.496e11)**2

        ! dVperd2/dOmega = dx * 1e2 or dx in CGS

        FluxMono = Gint * (10.0**LogNe)**2 / (4*cPi) * dLosLength*1e2

        if (DoExtendTransitionRegion) then
          FluxMono = FluxMono / extension_factor(Var_I(te_))
        endif

        if(IsDebug)then
          write(*,'(2I5,F8.2,2E12.3,3F7.3,2F10.3,F10.5,2F10.3,F12.4)') &
              iPixel, jLos, log10(Gint), FluxMono, dLosLength, LogNe, LogTe, &
              log10(Tlos), sqrt(Uth2)/1e3, sqrt(Unth2)/1e3, DLambda, Var_I(Ux_)/1e3, &
              norm2(Var_I(Bx_:Bz_))*1e4, norm2(XyzSeg_D)
        endif

        ! disperse line and save to spectrum array
        call disperse_line

      enddo; enddo
    enddo

    call timing_stop(NameSub)

  contains

    real function extension_factor(TeSi)
      real, intent(in) :: TeSi
      real :: FractionSpitzer
      !------------------------------------------------------------------------
      FractionSpitzer = 0.5*(1.0+tanh((TeSi-TeModSi)/DeltaTeModSi))
      extension_factor = FractionSpitzer + &
           (1.0 - FractionSpitzer)*(TeModSi/TeSi)**2.5
    end function extension_factor

    !==========================================================================
    subroutine disperse_line
      integer                     :: iStep, iWave, nWaveBin    
      integer                     :: iBin, iBegin, iEnd, jBin
      real                        :: Flux, Phi, InvNorm, InvSigma2
      real                        :: LambdaBin, LambdaBegin, LambdaEnd
      real                        :: LambdaDist
      character(len=*), parameter :: NameSub='disperse_line'
      !------------------------------------------------------------------------

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

      ! Update bins between begin and end indices by adding the Gaussian distribution
      do iBin = iBegin , iEnd
        ! Get wavelength from the center of the bin
        LambdaBin = SpectrumTable_I(iInterval)%SpectrumGrid_I(iBin)
        ! Get distance from peak wavelength in SI
        LambdaDist = Lambda - LambdaBin 
        ! Calculate Gaussian of the line
        Phi = InvNorm * exp(-LambdaDist**2 * InvSigma2)
        ! Calculate total monochromatic flux 
        Flux = FluxMono*Phi
        ! Update bin with flux
        SpectrumTable_I(iInterval)%Spectrum_II(iPixel,iBin) = &
            SpectrumTable_I(iInterval)%Spectrum_II(iPixel,iBin) + Flux
      end do

    end subroutine disperse_line
    !==========================================================================

  end subroutine spectrum_calc_flux

end module ModSpectrum

