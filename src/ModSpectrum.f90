!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModSpectrum

  use BATL_lib,          ONLY: iProc
  use ModBatsrusUtility, ONLY: stop_mpi

  implicit none
  SAVE

  private ! except

  public :: spectrum_read_table, spectrum_calc_flux, clean_mod_spectrum, &
       spectrum_calc_emission

  logical :: UseIonFrac

  ! Temperature and density grid for contribution function (G)
  real                        :: DLogN, DLogT
  real                        :: LogNMin, LogTMin, LogNMax, LogTMax

  integer                     :: nLineAll ! All lines of interest

  ! Derived type to read tabulated G values
  type LineTableType
     character(len=4)         :: NameIon
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

  ! Response function for NarrowBand Image
  real, allocatable           :: ResponseLambda_I(:), Response_I(:)
  integer                     :: nResponseBin

  ! Photoexcitation
  integer                     :: iTablePhx=0
contains
  !============================================================================
  subroutine spectrum_read_table(iFile, UseNbi, UsePhx)

    use ModIoUnit,      ONLY: io_unit_new
    use ModUtilities,   ONLY: open_file, close_file
    use ModIO,          ONLY: NameSpmTable_I, UseUnobserved_I, UseDoppler_I, &
         LambdaMin_I, LambdaMax_I, NameNbiTable_I, DLambda_I, &
         NamePhxTable_I, UseIonFrac_I
    ! response function
    use ModPlotFile,    ONLY: read_plot_file
    ! photoexcitation
    use ModLookupTable, ONLY: init_lookup_table, i_lookup_table, Table_I

    integer, intent(in)         :: iFile
    logical, intent(in)         :: UseNbi, UsePhx

    ! Data read from the file
    character(len=200)          :: StringLine
    character(len=5)            :: NameIon
    integer                     :: nLevelFrom, nLevelTo
    integer                     :: nFirstLevelFrom, nFirstLevelTo
    real                        :: LineWavelength, FirstLineWavelength
    real                        :: LogN, LogT, LogG, Aion, LogIonFrac
    integer                     :: iUnit

    ! End of file indicated by iError /= 0
    integer                     :: iError

    integer                     :: nMaxLine = 10000, nLineFound

    ! Maximum index range in density and temperature
    integer                     :: MaxI, MinI, MaxJ, MinJ

    ! Intensity table sized to the maximum
    real, allocatable           :: LogG_II(:,:), LogIonFrac_II(:,:)

    ! Size of table for a given wave
    integer                     :: iMin, jMin, iMax, jMax

    ! Density, temperature and wave indexes
    integer                     :: iN, iT, iLine

    ! Switches for header and information for a wavelength of interest
    logical                     :: IsHeader
    logical                     :: DoStore

    ! For non-equilibrium ionization
    integer                     :: iTemp

    integer                     :: iElement, iCharge

    !  List of elements and their atomic weight of periodic table
    integer, parameter                  :: nElement = 30
    integer                             :: jTemp = 0
    character(len=2), parameter         :: NameElement_I(1:nElement)=&
         ['h ','he','li','be','b ','c ',&
         'n ' ,'o ','f ','ne','na','mg','al', &
         'si','p ','s ','cl','ar','k ','ca','sc','ti','v ','cr','mn','fe', &
         'co','ni','cu','zn']

    real, parameter                     :: AElement_I(1:nElement)=&
         [1.008,  4.003,  6.94 ,  9.012, &
         10.81 , 12.011, 14.007, 15.999, &
         18.998, 20.18 , 22.99 , 24.305, 26.982, &
         28.085, 30.974, 32.06 , 35.45 , 39.948, 39.098, 40.078, 44.956, &
         47.867, 50.942, 51.996, 54.938 ,55.845, &
         58.933, 58.693, 63.546, 65.38]

    ! Start with response function if any as it gives the min and max for nbi
    character(len=*), parameter:: NameSub = 'spectrum_read_table'
    !--------------------------------------------------------------------------
    UseIonFrac = UseIonFrac_I(iFile)
    if(UsePhx)then
       if(i_lookup_table(NamePhxTable_I(iFile)) <= 0) &
            call init_lookup_table(NamePhxTable_I(iFile), "load", &
            NameFile = NamePhxTable_I(iFile), TypeFile = 'ascii')
       iTablePhx = i_lookup_table(NamePhxTable_I(iFile))
       if(iTablePhx <= 0) call stop_mpi('table required for photoexcitation')

       iElement = nint(Table_I(iTablePhx)%Param_I(1))
       iCharge  = nint(Table_I(iTablePhx)%Param_I(2))
       LineWavelength = Table_I(iTablePhx)%Param_I(3)

       nLineAll = 1
       allocate(LineTable_I(nLineAll))

       if(iElement < 9) then
          write(LineTable_I(1)%NameIon,'(a,i1.1)') &
               trim(NameElement_I(iElement)), iCharge
       else
          write(LineTable_I(1)%NameIon,'(a,i2.2)') &
               trim(NameElement_I(iElement)), iCharge
       end if
       LineTable_I(1)%Aion = AElement_I(iElement)
       LineTable_I(1)%LineWavelength = LineWavelength

       RETURN
    end if

    if(UseNbi)then
       call read_plot_file(NameFile = NameNbiTable_I(iFile), &
            n1Out = nResponseBin, &
            iErrorOut = iError)

       if(iError /= 0) call stop_mpi( &
            NameSub//' could not header from '//trim(NameNbiTable_I(iFile)))

       allocate(ResponseLambda_I(nResponseBin), Response_I(nResponseBin))

       call read_plot_file(NameFile = NameNbiTable_I(iFile), &
            TypeFileIn = 'ascii',                            &
            VarOut_I   = Response_I,                         &
            CoordOut_I = ResponseLambda_I,                   &
            iErrorOut  = iError)

       if(iError /= 0) call stop_mpi( &
            NameSub//' could not data from '//trim(NameNbiTable_I(iFile)))

       LambdaMin_I(iFile)=ResponseLambda_I(1)
       LambdaMax_I(iFile)=ResponseLambda_I(nResponseBin)
       DLambda_I(iFile)=ResponseLambda_I(2)-ResponseLambda_I(1)

    end if

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
    iUnit = io_unit_new()
    call open_file(iUnit,FILE=NameSpmTable_I(iFile), STATUS='old')

    ! Read grid size information from header
    READGRID: do
       read(iUnit,'(a)',iostat=iError) StringLine
       if(iError  /= 0)then
          if(iProc==0)write(*,*)'iError = ',iError
          call stop_mpi(NameSub//' failed reading header of chianti table')
       end if
       if(StringLine == "#GRID")then
          read(iUnit,*)LogNMin, LogNMax, DLogN
          read(iUnit,*)LogTMin, LogTMax, DLogT
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
          read(iUnit,'(a)',iostat=iError) StringLine
          if(iError  /= 0)then
             if(iProc==0)write(*,*)'iError = ',iError
             call stop_mpi(NameSub//' failed reading header of chianti table')
          end if
          if(StringLine == "#START") IsHeader = .false.
          CYCLE READLOOP
       end if

       ! Read data line
       read(iUnit,*,iostat=iError) &
            NameIon, nLevelFrom, nLevelTo, LineWavelength, &
            LogN, LogT, LogG, LogIonFrac

       if(iError  /= 0 .and. iError /= -1)then
          if(iProc==0)write(*,*)'iError = ',iError
          if(iProc==0)write(*,*)'last line = ',NameIon, nLevelFrom, &
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

       if(UseDoppler_I(iFile).and.LambdaMin_I(iFile) /= LambdaMax_I(iFile))then
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
       ! Rename Chianti elements to AWSoM Chargestate naming
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

       ! Calculate indexes and store as the minimum indexes
       iN                      = nint(LogN/DLogN)
       iT                      = nint(LogT/DLogT)
       LineTable_I(iLine)%StartLogT = LogT
       LineTable_I(iLine)%iMin = iN
       LineTable_I(iLine)%jMin = iT

       ! Initially zero out the input array
       LogG_II = -99.0
       LogIonFrac_II = -99.0

       ! Store first element
       LogG_II(iN,iT) = LogG
       LogIonFrac_II(iN,iT) = LogIonFrac

    end do READLOOP
    call close_file

    deallocate(LogG_II, LogIonFrac_II)
    nLineAll = min(nMaxLine,nLineFound)

  end subroutine spectrum_read_table
  !============================================================================
  subroutine spectrum_calc_flux(iFile, State_V, Ds, nLambda, LosDir_D, UseNbi,&
       Spectrum_I, r)

    use ModInterpolate, ONLY: bilinear
    use ModVarIndexes, ONLY: nVar, Rho_, Ux_, Uz_, Bx_, Bz_, &
         WaveFirst_, WaveLast_, Pe_, Ppar_, p_, nElement, ChargeStateFirst_, &
         ChargestateLast_, NameVar_V, nChargeState_I
    use ModPhysics, ONLY: No2Si_V, UnitX_, UnitTemperature_, &
         UnitRho_, UnitEnergyDens_ ,UnitB_, UnitU_
    use ModConst, ONLY: cProtonMass, cLightSpeed, cBoltzmann, cPi
    use ModIO, ONLY: DLambdaIns_I, DLambda_I, LambdaMin_I, &
         UseDoppler_I, UseAlfven_I, TempMin_I, LambdaMax_I
    use ModAdvance, ONLY: UseElectronPressure, UseAnisoPressure
    use ModLookupTable, ONLY: interpolate_lookup_table
    use ModTurbulence,  ONLY: IsOnAwRepresentative, PoyntingFluxPerB
    use ModChromosphere, ONLY: extension_factor, DoExtendTransitionRegion

    integer, intent(in)   :: iFile, nLambda
    real, intent(in)      :: State_V(nVar), Ds, LosDir_D(3)
    logical, intent(in)   :: UseNbi
    real, intent(inout)   :: Spectrum_I(:)
    real, intent(in), optional :: r

    integer                        :: iBin
    integer                        :: iNMin, jTMin, iNMax, jTMax
    integer                        :: iLine
    real                           :: FluxMono
    real                           :: Lambda, LambdaSI, DLambdaSI
    real                           :: DLambda, DLambdaSI2
    real                           :: Zplus2, Zminus2, CosAlpha, SinAlpha
    real                           :: B_D(3)
    real                           :: Unth2, Uth2
    real                           :: Gint, LogNe, LogTe, Rho, TeSi
    real                           :: Tlos, Ulos
    real                           :: Aion
    real                           :: LocalState_V(nVar)
    ! For H:He 10:1 fully ionized plasma the proton:electron ratio is
    ! 1/(1+2*0.1)
    real                        :: ProtonElectronRatio = 0.83

    integer                     :: iBegin, iEnd
    real                        :: Flux, Phi, InvNorm, InvSigma2
    real                        :: LambdaBin, LambdaBegin, LambdaEnd
    real                        :: LambdaDist

    ! Charge state variables
    real                        :: EquilIonFrac
    logical                     :: IsFound = .false.
    integer                     :: iVar, iVarIon, iElement, nCharge
    real                        :: LambdaMin
    real                        :: Value_I(2)

    character(len=*), parameter:: NameSub = 'spectrum_calc_flux'
    !--------------------------------------------------------------------------
    Rho = State_V(Rho_)*No2Si_V(UnitRho_)

    ! Calculate angle between LOS and B directions
    B_D      = State_V(Bx_:Bz_)*No2Si_V(UnitB_)
    CosAlpha = sum(LosDir_D*B_D)/sqrt(max(sum(B_D**2),1e-30))

    ! Calculate temperature relative to the LOS direction
    SinAlpha = sqrt(1 - CosAlpha**2)
    ! tperp = (3*t - tpar)/2
    if(UseAnisoPressure)then
       Tlos = (SinAlpha**2 * (3*State_V(p_)- State_V(Ppar_))/(2*State_V(Rho_))&
            + CosAlpha**2 * State_V(Ppar_)/State_V(Rho_))&
            * No2Si_V(UnitTemperature_)
    else
       Tlos = State_V(p_)/State_V(Rho_)* No2Si_V(UnitTemperature_)
    end if

    if(Tlos<TempMin_I(iFile))RETURN

    Unth2 = 0.0
    if(UseAlfven_I(iFile))then
       ! Calculate Elzasser variables
       Zplus2   = State_V(WaveFirst_)*No2Si_V(UnitEnergyDens_) * 4.0 / Rho
       Zminus2  = State_V(WaveLast_)*No2Si_V(UnitEnergyDens_) * 4.0 / Rho
       ! Calculate the non-thermal broadening
       Unth2    = 1.0/16.0 * (Zplus2 + Zminus2) * SinAlpha**2
       if(IsOnAwRepresentative)&
            Unth2 = Unth2*PoyntingFluxPerB*sqrt(State_V(Rho_))
    end if

    ! Convert from kg m^-3 to kg cm^-3 (*1e-6)
    ! and divide by cProtonMass in kg so Ne is in cm^-3
    ! 1 : 0.83  electron to proton ratio is assumed
    LogNe = log10(Rho*1e-6/cProtonMass/ProtonElectronRatio)
    if(UseElectronPressure)then
       TeSi = State_V(Pe_)/State_V(Rho_)*No2Si_V(UnitTemperature_)
    else
       TeSi = State_V(p_)/State_V(Rho_)*No2Si_V(UnitTemperature_)
    end if
    LogTe = log10(TeSi)
    Ulos = sum(State_V(Ux_:Uz_)*LosDir_D)/State_V(Rho_)*No2Si_V(UnitU_)

    if(UseIonFrac .and. ChargeStateFirst_>1)then
       ! Normalize charge states to get fractions
       LocalState_V = State_V
       iVar = ChargeStateFirst_
       do iElement = 1, nElement
          nCharge = nChargeState_I(iElement)-1
          LocalState_V(iVar:iVar+nCharge) = &
               State_V(iVar:iVar+nCharge) / &
               sum(State_V(iVar:iVar+nCharge))
          where(LocalState_V(iVar:iVar+nCharge)<1e-99)
             LocalState_V(iVar:iVar+nCharge) = 0.0
          end where
          LocalState_V(iVar:iVar+nCharge) = &
               LocalState_V(iVar:iVar+nCharge) / &
               sum(LocalState_V(iVar:iVar+nCharge))
          iVar = iVar + nCharge + 1
       end do
    end if

    do iLine = 1, nLineAll
       if(UseIonFrac .and. ChargeStateFirst_>1)then
          ! Pair index of chianti table to state variable
          IsFound = .false.
          do iVarIon=ChargestateFirst_,ChargestateLast_
             if(trim(LineTable_I(iLine)%NameIon)==trim(NameVar_V(iVarIon)))then
                IsFound = .true.
                EXIT
             end if
          enddo
       end if

       ! Calculate the thermal broadening
       Aion     = LineTable_I(iLine)%Aion
       Uth2     = cBoltzmann * Tlos/(cProtonMass * Aion)

       ! Convert resting wavelength to SI
       Lambda   = LineTable_I(iLine)%LineWavelength
       LambdaSI = Lambda * 1e-10

       ! Add thermal and non-thermal broadening
       DLambdaSI2 = LambdaSI**2 * (Uth2 + Unth2)/cLightSpeed**2

       ! Add instrumental broadening (if any)
       if(DLambdaIns_I(iFile) > 0)&
            DLambdaSI2 = DLambdaSI2 + (DLambdaIns_I(iFile)/1e10)**2

       ! Convert [m] --> [A]
       DLambdaSI = sqrt(DLambdaSI2)
       DLambda   = DLambdaSI * 1e10

       ! Gaussian profile
       InvNorm   = 1/(sqrt(2*cPi) * DLambda)
       InvSigma2 = 1/(2*DLambda**2)

       ! Doppler shift while x axis is oriented towards observer
       if(UseDoppler_I(iFile))Lambda = (-Ulos/cLightSpeed+1)*Lambda

       ! Gaussian truncated to +/-5 sigma in [A]
       LambdaBegin = Lambda - 5*DLambda
       LambdaEnd   = Lambda + 5*DLambda

       ! Photoexcitation
       if(present(r))then
          call interpolate_lookup_table(iTablePhx, LogTe, LogNe, r, &
               Value_I, DoExtrapolate = .false.)
          Gint = 10.0**Value_I(1)

          if(UseIonFrac .and. IsFound)then
             EquilIonFrac = 10.0**Value_I(2)
             Gint = Gint/EquilIonFrac * LocalState_V(iVarIon)
          end if
       else

          ! Get the contribution function
          iNMin  = LineTable_I(iLine)%iMin
          jTMin  = LineTable_I(iLine)%jMin
          iNMax  = LineTable_I(iLine)%iMax
          jTMax  = LineTable_I(iLine)%jMax

          Gint = bilinear(LineTable_I(iLine)%LogG_II(:,:), &
               iNMin, iNMax, jTMin, jTMax, &
               [ LogNe/DLogN , LogTe/DLogT ],DoExtrapolate=.true.)
          Gint = 10.0**Gint

          if(UseIonFrac .and. IsFound)then
             EquilIonFrac = bilinear(LineTable_I(iLine)%LogIonFrac_II(:,:), &
                  iNMin, iNMax, jTMin, jTMax, &
                  [ LogNe/DLogN , LogTe/DLogT ],DoExtrapolate=.true.)
             EquilIonFrac = 10.0**EquilIonFrac
             Gint = Gint/EquilIonFrac * LocalState_V(iVarIon)
          end if
       end if

       ! When Gint becomes negative due to extrapolation -> move to next
       if(Gint<=0)CYCLE

       ! Constant multiplier converted from SI [m] to CGS units [cm]
       ! dV_cell / (1AU)^2 * 1e2
       ! dVperd2 = Ds*dy*dz /(1.496e11)**2 * 1e2
       ! Solid angle obtained by the emitting surface at 1AU distance
       ! dOmega = dy*dz / (1.496e11)**2
       ! dVperd2/dOmega = Ds * 1e2 or dx in CGS

       FluxMono = Gint * (10.0**LogNe)**2 / (4*cPi) * Ds*No2Si_V(UnitX_) * 1e2

       if(DoExtendTransitionRegion) FluxMono = FluxMono/extension_factor(TeSi)

       ! Disperse line onto lamba bins
       ! Find the starting/ending wavelength bins
       ! Find the corresponding wavelength bin for starting wavelength
       if(LambdaMin_I(iFile)==LambdaMax_I(iFile))then
          LambdaMin=LambdaMin_I(iFile)-nLambda*0.5*DLambda_I(iFile)
       else
          LambdaMin=LambdaMin_I(iFile)
       end if
       iBegin = nint((LambdaBegin-LambdaMin)/DLambda_I(iFile))+1
       iEnd = nint((LambdaEnd-LambdaMin)/DLambda_I(iFile))+1

       ! Update bins between begin and end indices by adding the Gaussian
       ! distribution
       do iBin = max(1,iBegin), min(nLambda,iEnd)
          ! Get wavelength from the center of the bin
          LambdaBin = LambdaMin+DLambda_I(iFile)*(iBin-1)

          ! Get distance from peak wavelength in SI
          LambdaDist = Lambda - LambdaBin

          ! Calculate Gaussian of the line
          Phi = InvNorm * exp(-LambdaDist**2 * InvSigma2)

          ! Calculate total monochromatic flux
          Flux = FluxMono*Phi
          if(UseNbi)then
             Spectrum_I(1) = &
                  Spectrum_I(1) + Flux*Response_I(iBin)*DLambda_I(iFile)
          else
             ! Update bin with flux
             Spectrum_I(iBin) = Spectrum_I(iBin) + Flux
          end if
       end do
    end do

  end subroutine spectrum_calc_flux
  !============================================================================
  subroutine spectrum_calc_emission(iFile,State_V, UseNbi,Emission,nLambda,r)

    use ModConst, ONLY: cProtonMass, cLightSpeed, cBoltzmann, cPi
    use ModInterpolate, ONLY: bilinear
    use ModVarIndexes, ONLY: nVar, Rho_,Pe_, p_, nElement, &
         ChargeStateFirst_, ChargestateLast_, NameVar_V, nChargeState_I
    use ModPhysics, ONLY: No2Si_V, UnitTemperature_, UnitRho_
    use ModAdvance, ONLY: UseElectronPressure
    use ModLookupTable, ONLY: interpolate_lookup_table
    use ModIO, ONLY: DLambda_I, LambdaMin_I, LambdaMax_I
    use ModChromosphere, ONLY: extension_factor, DoExtendTransitionRegion

    real, intent(in):: State_V(nVar)
    integer,intent(in):: iFile, nLambda
    logical, intent(in):: UseNbi
    real, intent(out):: Emission
    real, intent(in), optional:: r

    integer:: iNMin, jTMin, iNMax, jTMax
    real   :: Gint, LogNe, LogTe, Rho, TeSi
    real   :: LocalState_V(nVar)
    ! For H:He 10:1 fully ionized plasma the proton:electron ratio is
    ! 1/(1+2*0.1)
    real  :: ProtonElectronRatio = 0.83

    ! Charge state variables
    real   :: EquilIonFrac
    logical:: IsFound = .false.
    integer:: iVar, iVarIon, iElement, nCharge
    integer:: iLine
    real   :: Value_I(2)

    ! NBI variables
    real:: Tlos, Aion, Uth2, LambdaBin, &
         Lambda, LambdaSI, DLambdaSI, LambdaBegin,&
         LambdaEnd, FluxMono, LambdaMin, LambdaDist, Phi, Flux, &
         DLambda, DLambdaSI2, InvNorm, InvSigma2
    integer:: iBegin, iEnd, iBin

    character(len=*), parameter:: NameSub = 'spectrum_calc_emission'
    !--------------------------------------------------------------------------
    Emission = 0
    Rho = State_V(Rho_)*No2Si_V(UnitRho_)

    LogNe = log10(Rho*1e-6/cProtonMass/ProtonElectronRatio)
    if(UseElectronPressure)then
       TeSi = State_V(Pe_)/State_V(Rho_)*No2Si_V(UnitTemperature_)
    else
       TeSi = State_V(p_)/State_V(Rho_)*No2Si_V(UnitTemperature_)
    end if
    LogTe = log10(TeSi)

    if(UseIonFrac .and. ChargeStateFirst_>1)then
       ! Normalize charge states to get fractions
       LocalState_V = State_V
       iVar = ChargeStateFirst_
       do iElement = 1, nElement
          nCharge = nChargeState_I(iElement)-1
          LocalState_V(iVar:iVar+nCharge) = &
               State_V(iVar:iVar+nCharge) / &
               sum(State_V(iVar:iVar+nCharge))
          where(LocalState_V(iVar:iVar+nCharge)<1e-99)
             LocalState_V(iVar:iVar+nCharge) = 0.0
          end where
          LocalState_V(iVar:iVar+nCharge) = &
               LocalState_V(iVar:iVar+nCharge) / &
               sum(LocalState_V(iVar:iVar+nCharge))
          iVar = iVar + nCharge + 1
       end do
    end if

    if(.not.UseNbi)nLineAll = 1

    do iLine = 1, nLineAll
       if(UseIonFrac .and. ChargeStateFirst_>1)then
          ! Pair index of chianti table to state variable
          IsFound = .false.
          do iVarIon=ChargestateFirst_,ChargestateLast_
             if(trim(LineTable_I(iLine)%NameIon)==trim(NameVar_V(iVarIon)))then
                IsFound = .true.
                EXIT
             end if
          enddo
       end if

       if(UseNbi)then
          Tlos = State_V(p_)/State_V(Rho_)* No2Si_V(UnitTemperature_)
          ! Calculate the thermal broadening
          Aion     = LineTable_I(iLine)%Aion
          Uth2     = cBoltzmann * Tlos/(cProtonMass * Aion)

          ! Convert resting wavelength to SI
          Lambda   = LineTable_I(iLine)%LineWavelength
          LambdaSI = Lambda * 1e-10

          ! Add thermal and non-thermal broadening
          DLambdaSI2 = LambdaSI**2 * Uth2/cLightSpeed**2

          ! Convert [m] --> [A]
          DLambdaSI = sqrt(DLambdaSI2)
          DLambda   = DLambdaSI * 1e10

          ! Gaussian profile
          InvNorm   = 1/(sqrt(2*cPi) * DLambda)
          InvSigma2 = 1/(2*DLambda**2)

          ! Gaussian truncated to +/-5 sigma in [A]
          LambdaBegin = Lambda - 5*DLambda
          LambdaEnd   = Lambda + 5*DLambda

       end if

       ! Photoexcitation
       if(present(r))then
          call interpolate_lookup_table(iTablePhx, LogTe, LogNe, r, &
               Value_I, DoExtrapolate = .false.)
          Gint = 10.0**Value_I(1)

          if(UseIonFrac .and. IsFound)then
             EquilIonFrac = 10.0**Value_I(2)
             Gint = Gint/EquilIonFrac * LocalState_V(iVarIon)
          end if
       else
          ! Get the contribution function
          iNMin  = LineTable_I(iLine)%iMin
          jTMin  = LineTable_I(iLine)%jMin
          iNMax  = LineTable_I(iLine)%iMax
          jTMax  = LineTable_I(iLine)%jMax

          Gint = bilinear(LineTable_I(iLine)%LogG_II(:,:), &
               iNMin, iNMax, jTMin, jTMax, &
               [ LogNe/DLogN , LogTe/DLogT ],DoExtrapolate=.true.)
          Gint = 10.0**Gint

          if(UseIonFrac .and. IsFound)then
             EquilIonFrac = bilinear(LineTable_I(iLine)%LogIonFrac_II(:,:), &
                  iNMin, iNMax, jTMin, jTMax, &
                  [ LogNe/DLogN , LogTe/DLogT ],DoExtrapolate=.true.)
             EquilIonFrac = 10.0**EquilIonFrac
             Gint = Gint/EquilIonFrac * LocalState_V(iVarIon)
          end if
       end if

       ! When Gint becomes negative due to extrapolation -> move to next
       if(Gint<=0)Gint = 0.0

       if(UseNbi)then
          FluxMono = Gint * (10.0**LogNe)**2

          if(DoExtendTransitionRegion) &
               FluxMono = FluxMono/extension_factor(TeSi)

          ! Disperse line onto lamba bins
          ! Find the starting/ending wavelength bins
          ! Find the corresponding wavelength bin for starting wavelength
          if(LambdaMin_I(iFile)==LambdaMax_I(iFile))then
             LambdaMin=LambdaMin_I(iFile)-nLambda*0.5*DLambda_I(iFile)
          else
             LambdaMin=LambdaMin_I(iFile)
          end if
          iBegin = nint((LambdaBegin-LambdaMin)/DLambda_I(iFile))+1
          iEnd = nint((LambdaEnd-LambdaMin)/DLambda_I(iFile))+1

          ! Update bins between begin and end indices by adding the Gaussian
          ! distribution
          do iBin = max(1,iBegin), min(nLambda,iEnd)
             ! Get wavelength from the center of the bin
             LambdaBin = LambdaMin+DLambda_I(iFile)*(iBin-1)

             ! Get distance from peak wavelength in SI
             LambdaDist = Lambda - LambdaBin

             ! Calculate Gaussian of the line
             Phi = InvNorm * exp(-LambdaDist**2 * InvSigma2)

             ! Calculate total monochromatic flux
             Flux = FluxMono*Phi
             Emission = Emission + Flux*Response_I(iBin)*DLambda_I(iFile)

          end do
       else
          Emission = Gint * (10.0**LogNe)**2 ![erg s-1 cm-3]
       endif
    end do

  end subroutine spectrum_calc_emission
  !============================================================================
  subroutine clean_mod_spectrum

    integer                     :: iLine = 0

    character(len=*), parameter:: NameSub = 'clean_mod_spectrum'
    !--------------------------------------------------------------------------
    if(allocated(LineTable_I))then
       do iLine = 1, nLineAll
          if(allocated(LineTable_I(iLine)%LogG_II)) &
	       deallocate(LineTable_I(iLine)%LogG_II)
          if(allocated(LineTable_I(iLine)%LogIonFrac_II)) &
               deallocate(LineTable_I(iLine)%LogIonFrac_II)
       end do
       deallocate(LineTable_I)
    end if
    if(allocated(ResponseLambda_I))deallocate(ResponseLambda_I)
    if(allocated(Response_I))deallocate(Response_I)

  end subroutine clean_mod_spectrum
  !============================================================================
end module ModSpectrum
!==============================================================================
