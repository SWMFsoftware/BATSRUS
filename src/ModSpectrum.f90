!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModSpectrum

  use ModConst,     ONLY: cLightSpeed, cBoltzmann, cProtonMass, cAU, cPi, rSun
  use ModUtilities,      ONLY: CON_stop
  use BATL_lib,          ONLY: iProc, nProc, iComm
  use ModBatsrusUtility, ONLY: stop_mpi

  implicit none
  SAVE

  private ! except

  public :: spectrum_read_table, spectrum_calc_flux, clean_mod_spectrum

  integer                     :: iError

  ! For H:He 10:1 fully ionized plasma the proton:electron ratio is 1/(1+2*0.1)
  real                        :: ProtonElectronRatio = 0.83

  integer                     :: nMaxLine = 10000, nLineFound

  ! Temperature and density grid for contribution function (G)
  real                        :: DLogN, DLogT
  real                        :: LogNMin, LogTMin, LogNMax, LogTMax

  ! Maximum index range in density and temperature
  integer                     :: MaxI, MinI, MaxJ, MinJ

  ! Variables for lines
  integer                     :: i, iLine
  integer                     :: nLineAll ! All lines of interest

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
     real, allocatable        :: LogG_II(:,:)
     real                     :: StartLogT
     real, allocatable        :: LogIonFrac_II(:,:)
  end type LineTableType

  type(LineTableType), allocatable :: LineTable_I(:)

contains
  !============================================================================
  !---------------------------------------------------------------------------
  subroutine spectrum_read_table(iFile)

    use ModIoUnit,    ONLY: UnitTmp_
    use ModUtilities, ONLY: open_file, close_file
    use ModIO,        ONLY: NameSpmTable_I, UseUnobserved_I, UseDoppler_I, &
         LambdaMin_I, LambdaMax_I

    integer, intent(in)         :: iFile

    ! Data read from the file
    character(len=200)          :: StringLine
    character(len=6)            :: NameIon
    integer                     :: nLevelFrom, nLevelTo
    integer                     :: nFirstLevelFrom, nFirstLevelTo
    real                        :: LineWavelength, FirstLineWavelength
    real                        :: LogN, LogT, LogG, Aion, LogIonFrac

    ! End of file indicated by iError /= 0
    integer                     :: iError

    ! Intensity table sized to the maximum
    real, allocatable           :: LogG_II(:,:), LogIonFrac_II(:,:)

    ! Size of table for a given wave
    integer                     :: iMin, jMin, iMax, jMax

    ! Density, temperature and wave indexes
    integer                     :: iN, iT, iLine

    ! Switches for header and information for a wavelength of interest
    logical                     :: IsHeader
    logical                     :: DoStore

    character(len=*), parameter:: NameSub = 'spectrum_read_table'
    !--------------------------------------------------------------------------
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
    call open_file(FILE=NameSpmTable_I(iFile), STATUS='old')

    ! Read grid size information from header
    READGRID: do
       read(UnitTmp_,'(a)',iostat=iError) StringLine
       if(iError  /= 0)then
          if(iProc==0)write(*,*)'iError = ',iError
          call stop_mpi(NameSub//' failed reading header of chianti table')
       end if
       if(StringLine == "#GRID")then
          read(UnitTmp_,*)LogNMin, LogNMax, DLogN
          read(UnitTmp_,*)LogTMin, LogTMax, DLogT
          EXIT READGRID
       end if
    end do READGRID

    ! Set up maximum size grid to store tabulated values by wavelength
    MinI = nint(LogNMin/DLogN)
    MaxI = nint(LogNMax/DLogN)
    MinJ = nint(LogTMin/DLogT)
    MaxJ = nint(LogTMax/DLogT)
    allocate(LogG_II(MinI:MaxI,MinJ:MaxJ),LogIonFrac_II(MinI:MaxI,MinJ:MaxJ))

    ! Read remaining portion of table file
    READLOOP: do
       ! Read remaining header lines of table file
       if(IsHeader)then
          read(UnitTmp_,'(a)',iostat=iError) StringLine
          if(iError  /= 0)then
             if(iProc==0)write(*,*)'iError = ',iError
             call stop_mpi(NameSub//' failed reading header of chianti table')
          end if
          if(StringLine == "#START") IsHeader = .false.
          CYCLE READLOOP
       end if

       ! Read data line
       read(UnitTmp_,*,iostat=iError) &
            NameIon, Aion, nLevelFrom, nLevelTo, LineWavelength, &
            LogN, LogT, LogG, LogIonFrac

       if(iError  /= 0 .and. iError /= -1)then
          if(iProc==0)write(*,*)'iError = ',iError
          if(iProc==0)write(*,*)'last line = ',NameIon, Aion, nLevelFrom, &
               nLevelTo, LineWavelength, LogN, LogT, LogG, LogIonFrac
          call stop_mpi(NameSub//' failed reading chianti table')
       end if

       ! Unobserved lines are stored with negative wavelength
       ! If interested in unobserved lines, use absolute value of wavelength
       if(UseUnobserved_I(iFile).and. LineWavelength < 0)&
            LineWavelength = abs(LineWavelength)

       ! Check if current line belongs to the same wavelength as previous one
       if(LineWavelength == FirstLineWavelength .and. &
            nLevelFrom == nFirstLevelFrom .and. &
            nLevelTo == nFirstLevelTo .and. &
            iError == 0) then
          ! Calculate index and store extra elements of LogG in LogG_II
          iN = nint(LogN/DLogN)
          iT = nint(LogT/DLogT)
          LogG_II(iN,iT) = LogG
          LogIonFrac_II(iN,iT) = LogIonFrac
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
          allocate(LineTable_I(iLine)%LogG_II(iMin:iMax,jMin:jMax))
          LineTable_I(iLine)%LogG_II = LogG_II(iMin:iMax,jMin:jMax)

          ! Create equilibrium ionization fractions table
          allocate(LineTable_I(iLine)%LogIonFrac_II(iMin:iMax,jMin:jMax))
          LineTable_I(iLine)%LogIonFrac_II = LogIonFrac_II(iMin:iMax,jMin:jMax)

          ! Storage is done
          DoStore = .false.
       end if

       ! When reached end of file, exit loop
       if(iError /= 0) EXIT READLOOP

       ! If wavelength is different than previous line
       ! pass only if wavelength is inside of wavelengthintervals of interest

       if(UseDoppler_I(iFile))then
          ! If Doppler shift is calculated, use shifted intervals
          if(LineWavelength < LambdaMin_I(iFile)*0.9)CYCLE
          if(LineWavelength > LambdaMax_I(iFile)*1.1)CYCLE
       else
          ! If no Doppler shift is calculated, use original intervals
          if(LineWavelength <  LambdaMin_I(iFile))CYCLE
          if(LineWavelength > LambdaMax_I(iFile))CYCLE
       endif

       ! New line of interest found, decide to store it
       iLine      = iLine + 1

       ! Check if there are too many lines already
       if(iLine > nMaxLine)&
            call stop_mpi(NameSub//' Too many lines are found')

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
       LogG_II = 0.0
       LogIonFrac_II = 0.0

       ! Store first element
       LogG_II(iN,iT) = LogG
       LogIonFrac_II(iN,iT) = LogIonFrac

    end do READLOOP
    call close_file

    deallocate(LogG_II, LogIonFrac_II)
    nLineAll = min(nMaxLine,nLineFound)

  end subroutine spectrum_read_table
  !============================================================================

  subroutine spectrum_calc_flux(iFile, State_V, Ds, nLambda, LosDir_D, &
       Spectrum_I)

    use ModInterpolate, ONLY: bilinear
    use ModVarIndexes, ONLY: nVar, Rho_, Ux_, Uy_, Uz_, Bx_, By_, Bz_, &
         WaveFirst_, WaveLast_, Pe_, Ppar_, p_
    use ModPhysics, ONLY: No2Si_V, UnitX_,UnitN_, UnitTemperature_, rBody, &
         UnitRho_, UnitEnergyDens_ ,UnitB_, UnitP_, UnitMass_,UnitAngle_,UnitU_
    use ModConst, ONLY: rSun, cProtonMass, cLightSpeed, cBoltzmann, cPi
    use ModIO, ONLY: DLambdaIns_I, DLambda_I,LambdaMin_I,LambdaMax_I, &
         UseDoppler_I

    integer, intent(in)   :: iFile, nLambda
    real, intent(in)      :: State_V(nVar), Ds, LosDir_D(3)
    real, intent(inout)   :: Spectrum_I(nLambda)

    integer                        :: i,iBin
    integer                        :: iNMin, jTMin, iNMax, jTMax

    real                           :: FluxMono
    real                           :: Lambda, LambdaSI, Lambda0SI, DLambdaSI
    real                           :: DLambda, DLambdaSI2
    real                           :: DLambdaInstr2 = 0.0 !!! do it later
    real                           :: Zplus2, Zminus2, CosAlpha, SinAlpha
    real                           :: B_D(3), Bnorm_D(3)
    real                           :: Unth2, Uth2
    real                           :: Gint, LogNe, LogTe, Rho
    real                           :: Tlos, Ulos
    real                           :: Aion
    real                           :: TShift
    logical                        :: IsFound = .false.

    ! From disperse_line
    integer                     :: iBegin, iEnd
    real                        :: Flux, Phi, InvNorm, InvSigma2
    real                        :: LambdaBin, LambdaBegin, LambdaEnd
    real                        :: LambdaDist

    integer                     :: jBin

    ! Calculate Elzasser variables
    character(len=*), parameter:: NameSub = 'spectrum_calc_flux'
    !--------------------------------------------------------------------------
    Rho = State_V(Rho_)*No2Si_V(UnitRho_)
    Zplus2   = State_V(WaveFirst_)*No2Si_V(UnitEnergyDens_) * 4.0 / Rho
    Zminus2  = State_V(WaveLast_)*No2Si_V(UnitEnergyDens_) * 4.0 / Rho

    ! Calculate angle between LOS and B directions
    B_D      = State_V(Bx_:Bz_)*No2Si_V(UnitB_)

    CosAlpha = sum(LosDir_D*B_D)/sqrt(max(sum(B_D**2),1e-30))

    ! Calculate temperature relative to the LOS direction
    SinAlpha = sqrt(1 - CosAlpha**2)
    ! tperp = (3*t - tpar)/2
    Tlos = (SinAlpha**2 * (3*State_V(p_)- State_V(Ppar_))/(2*State_V(Rho_))&
         + CosAlpha**2 * State_V(Ppar_)/State_V(Rho_))&
         * No2Si_V(UnitTemperature_)

    ! Calculate the non-thermal broadening
    Unth2    = 1.0/16.0 * (Zplus2 + Zminus2) * SinAlpha**2

    ! Convert from kg m^-3 to kg cm^-3 (*1e-6)
    ! and divide by cProtonMass in kg so Ne is in cm^-3
    ! 1 : 0.83  electron to proton ratio is assumed
    LogNe = log10(Rho*1e-6/cProtonMass/ProtonElectronRatio)
    LogTe = log10(State_V(Pe_)/State_V(Rho_)* No2Si_V(UnitTemperature_))

    Ulos = sum(State_V(Ux_:Uz_)*LosDir_D)/State_V(Rho_)*No2Si_V(UnitU_)

    do iLine = 1, nLineAll

       Aion     = LineTable_I(iLine)%Aion
       Lambda   = LineTable_I(iLine)%LineWavelength
       ! Calculate the thermal broadening
       Uth2     = cBoltzmann * Tlos/(cProtonMass * Aion)

       ! Doppler shift while x axis is oriented towards observer
       if(UseDoppler_I(iFile))Lambda = &
            (-Ulos/cLightSpeed+1)*Lambda

       ! Convert to SI
       LambdaSI = LineTable_I(iLine)%LineWavelength * 1e-10

       ! Add thermal and non-thermal broadening
       DLambdaSI2 = LambdaSI**2 * (Uth2 + Unth2)/cLightSpeed**2

       ! Add instrumental broadening (if any)
       DLambdaSI2 = DLambdaSI2 + (DLambdaIns_I(iFile))**2

       ! Convert [m] --> [A]
       DLambdaSI = sqrt(DLambdaSI2)
       DLambda   = DLambdaSI * 1e10

       ! Gaussian profile
       InvNorm   = 1/(sqrt(2*cPi) * DLambda)
       InvSigma2 = 1/(2*DLambda**2)

       ! Beginning and end of gaussian truncated to +/-5 sigma in [A]
       LambdaBegin = Lambda - 5*DLambda
       LambdaEnd   = Lambda + 5*DLambda

       ! Get the contribution function
       iNMin  = LineTable_I(iLine)%iMin
       jTMin  = LineTable_I(iLine)%jMin
       iNMax  = LineTable_I(iLine)%iMax
       jTMax  = LineTable_I(iLine)%jMax

       Gint = bilinear(LineTable_I(iLine)%LogG_II(:,:), &
            iNMin, iNMax, jTMin, jTMax, &
            [ LogNe/DLogN , LogTe/DLogT ],DoExtrapolate=.true.)
       Gint = 10.0**Gint

       ! When Gint becomes negative due to extrapolation -> move to next
       if(Gint<=0)CYCLE

       ! Calculate flux and spread it on the Spectrum_II grids

       ! Constant multiplier converted from SI [m] to CGS units [cm]
       ! dV_cell / (1AU)^2 * 1e2
       ! dVperd2 = Ds*dy*dz /(1.496e11)**2 * 1e2

       ! Solid angle obtained by the emitting surface at 1AU distance
       ! dOmega = dy*dz / (1.496e11)**2

       ! dVperd2/dOmega = Ds * 1e2 or dx in CGS

       FluxMono = Gint * (10.0**LogNe)**2 / (4*cPi) * Ds*No2Si_V(UnitX_)

       ! Disperse line onto lamba bins

       ! Find the starting/ending wavelength bins
       ! Find the corresponding wavelength bin for starting wavelength
       do iBegin = 1, nLambda - 1
          if (LambdaBegin < LambdaMin_I(iFile)+DLambda_I(iFile)*iBegin)&
            EXIT
       end do

       ! stop at nWaveBin-1 so iEnd = nWaveBin if no EXIT was performed
       do iEnd = iBegin, nLambda-1
          if (LambdaEnd > LambdaMin_I(iFile)+DLambda_I(iFile)*iEnd) EXIT
       end do
       iEnd = iEnd-1

       ! Update bins between begin and end indices by adding the Gaussian
       ! distribution

       do iBin = iBegin , iEnd
          ! Get wavelength from the center of the bin
          LambdaBin = LambdaMin_I(iFile)+DLambda_I(iFile)*iBin

          ! Get distance from peak wavelength in SI
          LambdaDist = Lambda - LambdaBin

          ! Calculate Gaussian of the line
          Phi = InvNorm * exp(-LambdaDist**2 * InvSigma2)

          ! Calculate total monochromatic flux
          Flux = FluxMono*Phi

          ! Update bin with flux
          Spectrum_I(iBin) = Spectrum_I(iBin) + Flux
       end do
    end do

  end subroutine spectrum_calc_flux
  !============================================================================

  subroutine clean_mod_spectrum
    character(len=*), parameter:: NameSub = 'clean_mod_spectrum'
    !--------------------------------------------------------------------------
    deallocate(LineTable_I)

  end subroutine clean_mod_spectrum
  !============================================================================
end module ModSpectrum
!==============================================================================

