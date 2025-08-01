!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModSolarwind

  use BATL_lib, ONLY: &
       test_start, test_stop, lVerbose, iProc
  use ModBatsrusUtility, ONLY: stop_mpi

  use ModKind
  use ModVarIndexes

  implicit none

  SAVE

  private ! except
  public :: read_solar_wind_file
  public :: read_solar_wind_param
  public :: normalize_solar_wind_data
  public :: get_solar_wind_point

  logical, public :: UseSolarwindFile = .false.

  ! Local variables

  character(len=500):: NameSolarwindFile

  integer, parameter :: nTimeVar = 7

  ! Number of data points in the input file
  integer :: nData = 0

  ! Number of variables in the input file
  integer :: nVarInput

  ! Name of the input variables
  character(len=10) :: NameInputVar_I(nVar)

  ! Variable index corresponding to the i-th input variable
  integer :: iVarInput_V(max(nVar,8))

  ! true if the variable is read from the data file
  logical :: IsInput_V(nVar)

  ! true if number density is read instead of density
  logical :: UseNumberDensity = .true.

  ! true if temperature is read instead of pressure
  logical :: UseTemperature = .true., UseElectronTemperature = .true.

  ! Arrays of input data and time
  real,         allocatable :: Solarwind_VI(:, :)
  real(Real8_), allocatable :: Time_I(:)

  ! Coordinate system of input file
  character(len=3) :: NameInputCoord = 'GSM'

  ! Normal direction to the (tilted) plane of input data
  real :: Normal_D(3) = [ 1.0, 0.0, 0.0 ]

  ! Position of the satellite
  real :: SatelliteXyz_D(3)=0.
  !$omp threadprivate( SatelliteXyz_D )

  ! Shall we reread the file
  logical :: DoReadAgain = .false.

contains
  !============================================================================
  subroutine read_solar_wind_param(NameCommand)

    use ModReadParam, ONLY: read_var

    character(len=*), intent(in):: NameCommand

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_solar_wind_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(NameCommand)
    case("#SOLARWINDFILE", "#UPSTREAM_INPUT_FILE")
       call read_var('UseSolarWindFile', UseSolarwindFile)
       if (UseSolarwindFile) &
            call read_var('NameSolarWindFile', NameSolarWindFile)
    case("#REFRESHSOLARWINDFILE")
       call read_var('DoReadAgain', DoReadAgain)
    case default
       call stop_mpi(NameSub//': unknown NameCommand='//NameCommand)
    end select

    call test_stop(NameSub, DoTest)
  end subroutine read_solar_wind_param
  !============================================================================
  subroutine read_solar_wind_file

    use ModMain, ONLY: StartTime, tSimulation, &
         UseStrict, TypeCoordSystem, NameVarLower_V
    use ModPhysics, ONLY: SolarWindBxDim, SolarWindByDim, SolarWindBzDim, &
         SolarWindUxDim, SolarWindUyDim, SolarWindUzDim, SolarWindNDim, &
         SolarWindTempDim, nVectorVar, iVectorVar_I
    use ModAdvance, ONLY: UseMultiSpecies
    use ModIoUnit, ONLY: UnitTmp_
    use ModNumConst, ONLY: cDegToRad, cHalfPi
    use ModConst, ONLY: cDay => cSecondPerDay
    use CON_geopack, ONLY: geopack_recalc, HgiGse_DD, GsmGse_DD
    use CON_axes, ONLY: transform_matrix
    use ModIO, ONLY: iUnitOut, write_prefix
    use ModTimeConvert, ONLY: time_int_to_real
    use ModUtilities, ONLY: upper_case, lower_case, split_string, &
         open_file, close_file
    use ModMPI

    character(len=500):: StringInputVar

    integer :: iError, i , iVar, jVar, iYear, iVectorVar
    logical :: UseZeroBx

    ! One line of input
    character (len=100) :: StringLine
    character (len=20) :: String

    real    :: TmpData_V(nVar)
    integer :: iTime_I(nTimeVar)

    ! Tilt angle of the plane of the input data
    real :: PlaneAngleXY=0.0, PlaneAngleXZ=0.0

    real :: TimeDelay
    real(Real8_) :: Time, DtData1, DtData2
    real:: Transform_DD(3,3)
    real:: Solarwind_V(nVar)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_solar_wind_file'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Set defaults
    UseZeroBx = .false.
    TimeDelay = 0.0
    PlaneAngleXY = 0.0
    PlaneAngleXZ = 0.0

    nVarInput = 8
    iVarInput_V(1:8) = [ Bx_, By_, Bz_, Ux_, Uy_, Uz_, Rho_, p_ ]
    if(UseMultiSpecies) iVarInput_V(7) = SpeciesFirst_

    ! Read solar wind file on all processors in parallel
    call open_file(FILE=NameSolarwindFile, STATUS="old")

    if(lVerbose>0 .and. iProc==0)then
       call write_prefix
       write(iUnitOut,*) NameSub,' reading ',trim(NameSolarwindFile)
    end if

    ! Read header information
    do
       read(UnitTmp_,'(a)', iostat = iError ) StringLine
       if (iError /= 0) call stop_mpi(NameSub// &
            ': could not find #START in '//trim(NameSolarwindFile))

       if(index(StringLine,'#REREAD')>0) read(UnitTmp_,*) DoReadAgain

       if(index(StringLine,'#COOR')>0)then
          read(UnitTmp_,'(a)') NameInputCoord
          call upper_case(NameInputCoord)
       endif

       if(index(StringLine,'#PLANE')>0)then
          read(UnitTmp_,*) PlaneAngleXY
          read(UnitTmp_,*) PlaneAngleXZ
          PlaneAngleXY = PlaneAngleXY * cDegToRad
          PlaneAngleXZ = PlaneAngleXZ * cDegToRad

          ! Calculate normal vector
          if( abs(abs(PlaneAngleXY)-cHalfPi) < 1.0e-3 )then
             Normal_D = [ 0.0, sign(1.0,PlaneAngleXY), 0.0 ]
          else if ( abs(abs(PlaneAngleXZ)-cHalfPi) < 1.0e-3 )then
             Normal_D = [ 0.0, 0.0, sign(1.0, PlaneAngleXZ) ]
          else
             Normal_D(2) = tan(PlaneAngleXY)
             Normal_D(3) = tan(PlaneAngleXZ)
             Normal_D =  Normal_D / sqrt(sum(Normal_D**2))
          end if
          if(DoTest)write(*,*)'Normal propagation direction is',Normal_D
       endif

       if(index(StringLine,'#VAR')>0)then
          read(UnitTmp_,'(a)', iostat = iError) StringInputVar
          call split_string(StringInputVar, nVar, NameInputVar_I, nVarInput)
          UseNumberDensity = .false.
          UseTemperature   = .false.
          UseElectronTemperature = .false.
          iVarInput_V = 0
          if(DoTest)then
             write(*,*)'StringInputVar=', StringInputVar
             write(*,*)'NameInputVar_I=',NameInputVar_I
             write(*,*)'Max Number of input variables =',nVar
             write(*,*)'user input number of variables=',nVarInput
          end if

          do iVar= 1, nVarInput
             String = NameInputVar_I(iVar)
             call lower_case(String)
             select case (String)
             case ('n')
                iVarInput_V(iVar)  = rho_
                UseNumberDensity = .true.
             case ('ux')
                iVarInput_V(iVar) = Ux_
             case ('uy')
                iVarInput_V(iVar) = Uy_
             case ('uz')
                iVarInput_V(iVar) = Uz_
             case ('bx')
                iVarInput_V(iVar) = Bx_
             case ('by')
                iVarInput_V(iVar) = By_
             case ('bz')
                iVarInput_V(iVar) = Bz_
             case ('t')
                iVarInput_V(iVar) = p_
                UseTemperature  = .true.
             case ('te')
                iVarInput_V(iVar) = pe_
                UseElectronTemperature  = .true.
             case  default
                do jVar=1, nVar
                   if(NameVarLower_V(jVar) == String) EXIT
                end do
                if(jVar > nVar) call stop_mpi(NameSub// &
                     ': unknown solarwind variable='//trim(String))
                iVarInput_V(iVar) = jVar
             end select
          end do
       end if

       if(index(StringLine,'#POSITION')>0)then
          read(UnitTmp_,*) SatelliteXyz_D(2)
          read(UnitTmp_,*) SatelliteXyz_D(3)
       endif

       if(index(StringLine,'#SATELLITEXYZ')>0)then
          read(UnitTmp_,*) SatelliteXyz_D(1)
          read(UnitTmp_,*) SatelliteXyz_D(2)
          read(UnitTmp_,*) SatelliteXyz_D(3)
       endif

       if(index(StringLine,'#ZEROBX')>0)    read(UnitTmp_,*) UseZeroBx

       if(index(StringLine,'#TIMEDELAY')>0) read(UnitTmp_,*) TimeDelay

       if(index(StringLine,'#START')>0) EXIT
    end do

    ! Set logicals telling if a variable is read from the input file
    do iVar = 1, nVar
       IsInput_V(iVar) = any(iVarInput_V(1:nVarInput) == iVar)
    end do

    ! Get the number of input lines
    nData = 0
    do
       read(UnitTmp_, *, IOSTAT=iError) iTime_I, TmpData_V(1:nVarInput)
       if(iError /= 0) EXIT
       nData = nData + 1
    end do

    ! For reading a new input file
    if(allocated(Solarwind_VI)) deallocate(Solarwind_VI, Time_I)
    allocate(Solarwind_VI(nVar,nData), Time_I(nData))

    ! Initialize array so all elements are set
    Solarwind_VI = 0.0

    ! Rewind to the end of #START line
    Rewind UnitTmp_
    do
       read(UnitTmp_,'(a)', iostat = iError ) StringLine
       if(index(StringLine,'#START')>0) EXIT
    enddo

    ! Read the data
    nData = 0
    do
       read(UnitTmp_, *, IOSTAT=iError) iTime_I, TmpData_V(1:nVarInput)
       if(iError /= 0) EXIT

       nData = nData + 1

       do iVar = 1, nVarInput
          jVar = iVarInput_V(iVar)
          Solarwind_VI(jVar, nData) = TmpData_V(iVar)
       end do

       if(DoTest)then
          write(*,*)'nData=', nData
          write(*,*)'Solarwind_VI=', Solarwind_VI(:, nData)
       end if

       ! Fix 2 digit years
       iYear = iTime_I(1)
       if(iYear >= 65 .and. iYear < 100) iYear = iYear + 1900
       if(iYear < 65)                    iYear = iYear + 2000
       iTime_I(1) = iYear

       ! Convert integer time into double precision real time and apply delay
       call time_int_to_real(iTime_I, Time)
       Time_I(nData) = Time + TimeDelay

       if(UseZeroBx) Solarwind_VI(Bx_, nData) = 0.0

       if(NameInputCoord /= TypeCoordSystem) then
          ! Transform vector variables from input to model coordinates

          call geopack_recalc(iTime_I)

          if(TypeCoordSystem == 'GSM' .and. NameInputCoord == 'GSE') then
             ! GSE -> GSM
             Transform_DD = GsmGse_DD
          elseif(TypeCoordSystem == 'GSE' .and. NameInputCoord == 'GSM') then
             ! GSM -> GSE
             Transform_DD = transpose(GsmGse_DD)
          elseif(TypeCoordSystem == 'HGI' .and. NameInputCoord == 'GSM') then
             ! GSM -> GSE -> HGI
             Transform_DD = matmul(GsmGse_DD, HgiGse_DD)
          elseif(TypeCoordSystem == 'HGI' .and. NameInputCoord == 'GSE')then
             ! GSE -> HGI
             Transform_DD = HgiGse_DD
          else
             Transform_DD = transform_matrix( &
                  tSimulation, TypeCoordSystem, NameInputCoord)
          end if

          do iVectorVar = 1, nVectorVar
             iVar = iVectorVar_I(iVectorVar)
             Solarwind_VI(iVar:iVar+2,nData)=&
                  matmul(Transform_DD, Solarwind_VI(iVar:iVar+2,nData))
          end do
          ! Shouldn't we add the orbital velocity of the Earth for HGI?!?

       endif
    enddo

    call close_file

    ! Check if the start time is within 1 day of the input data
    if( StartTime + tSimulation < Time_I(1) - cDay .or. &
         StartTime > Time_I(nData)+cDay) then
       write(*,*) "**********************************************************"
       write(*,*) "*                                                        *"
       write(*,*) "*      Warning! Warning! Warning! Warning! Warning!      *"
       write(*,*) "*                                                        *"
       write(*,*) "*  Time dependent solar wind file disagrees with the     *"
       write(*,*) "*  starting time of the simulation by more than 24 hours.*"
       write(*,*) "*  This could cause results which you may not enjoy.     *"
       write(*,*) "*                                                        *"
       if (UseStrict) call stop_mpi('Correct PARAM.in or the INPUT data file')

       write(*,*) "*  I am assuming that you are smarter than I am.         *"
       write(*,*) "*  Continuing with simulation.                           *"
       write(*,*) "*                                                        *"
       write(*,*) "**********************************************************"
    endif

    if(lVerbose>0 .and. iProc==0)then
       call write_prefix; write(iUnitOut,*) NameSub, &
            ' read ',nData,' points from ',trim(NameSolarwindFile)
    end if

    ! This part is only needed for solar wind normalization based on
    ! the input file. This should be eliminated.
    if (SolarWindTempDim <= 0.0) then

       if (nData == 1) then
          Solarwind_V = Solarwind_VI(:, 1)
       else
          ! Find the index with time before start time
          i = 1

          do while ((i < nData).and.   &
               (Time_I(i) < StartTime))
             i = i + 1
          enddo

          if ((i == nData .and. &
               Time_I(i) <= StartTime).or.(i==1)) then
             Solarwind_V = Solarwind_VI(:, i)
          else
             DtData2 = (Time_I(i) - StartTime) / &
                  (Time_I(i) - Time_I(i-1) + 1.0e-6)
             DtData1 = 1.0 - DtData2

             Solarwind_V = DtData1*Solarwind_VI(:, i) &
                  +        DtData2*Solarwind_VI(:, i-1)
          endif

       endif

       ! add up species densities
       if(UseMultiSpecies) &
            Solarwind_V(Rho_) = sum(Solarwind_V(SpeciesFirst_:SpeciesLast_))

       ! These scalars should be removed eventually
       SolarWindBxDim   = Solarwind_V(Bx_)
       SolarWindByDim   = Solarwind_V(By_)
       SolarWindBzDim   = Solarwind_V(Bz_)
       SolarWindUxDim   = Solarwind_V(Ux_)
       SolarWindUyDim   = Solarwind_V(Uy_)
       SolarWindUzDim   = Solarwind_V(Uz_)
       SolarWindNDim    = Solarwind_V(rho_)
       SolarWindTempDim = Solarwind_V(p_)
    endif

    call test_stop(NameSub, DoTest)

  end subroutine read_solar_wind_file
  !============================================================================
  subroutine normalize_solar_wind_data

    use ModAdvance, ONLY: &
         UseElectronPressure, UseAnisoPressure, UseMultiSpecies, UseAnisoPe
    use ModPhysics, ONLY: &
         Io2No_V, UnitTemperature_, UnitN_, UnitRho_, UnitP_, UnitU_, UnitB_, &
         LowDensityRatio, ElectronPressureRatio, SwTMinDim
    use ModMultiFluid, ONLY: select_fluid, iRho, iUx, iUy, iUz, &
         iP, iPIon_I, MassIon_I, UseMultiIon
    use ModConst
    use ModMPI

    integer, parameter:: T_= p_, Te_ = Pe_

    integer:: iData, iFluid
    real :: Solarwind_V(nVar)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'normalize_solar_wind_data'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    do iData=1, nData

       ! Put this point into a small array
       Solarwind_V = Solarwind_VI(:,iData)

       ! normalize B and U
       Solarwind_V(Bx_:Bz_) = Solarwind_V(Bx_:Bz_)*Io2No_V(UnitB_)
       Solarwind_V(Ux_:Uz_) = Solarwind_V(Ux_:Uz_)*Io2No_V(UnitU_)

       if(UseMultiSpecies) then

          ! We assume that species densities are always given as number density
          where(IsInput_V(SpeciesFirst_:SpeciesLast_))
             ! Normalize species number densities
             Solarwind_V(SpeciesFirst_:SpeciesLast_) = &
                  Solarwind_V(SpeciesFirst_:SpeciesLast_)*Io2No_V(UnitN_)
          elsewhere
             ! Use very small value for densities not given
             Solarwind_V(SpeciesFirst_:SpeciesLast_) = &
                  LowDensityRatio * Solarwind_V(SpeciesFirst_)
          end where

          if(UseTemperature)then
             ! calculate normalized p = n*T
             Solarwind_V(p_) = sum(Solarwind_V(SpeciesFirst_:SpeciesLast_)) &
                  *max(Solarwind_V(T_), SwTMinDim)*Io2No_V(UnitTemperature_)
          else
             ! normalize pressure
             Solarwind_V(p_) = Solarwind_V(p_)*Io2No_V(UnitP_)
          end if

          ! calculate mass density of each ion species
          Solarwind_V(SpeciesFirst_:SpeciesLast_) = &
               Solarwind_V(SpeciesFirst_:SpeciesLast_)*MassSpecies_V
          ! add up species densities
          Solarwind_V(Rho_) = sum(Solarwind_V(SpeciesFirst_:SpeciesLast_))
       else
          ! Single species
          if(UseNumberDensity) then
             Solarwind_V(Rho_) = Solarwind_V(Rho_)*Io2No_V(UnitN_)*MassIon_I(1)
          else
             Solarwind_V(Rho_) = Solarwind_V(Rho_)*Io2No_V(UnitRho_)
          end if
          if(UseTemperature) then
             Solarwind_V(p_) = max(Solarwind_V(T_), SwTMinDim) &
                  *Io2No_V(UnitTemperature_)*Solarwind_V(Rho_)/MassIon_I(1)
          else
             Solarwind_V(p_) = Solarwind_V(p_)*Io2No_V(UnitP_)
          end if
       end if

       ! Modify total pressure with electron pressure
       if(UseTemperature .and. .not.UseElectronPressure) &
            Solarwind_V(p_) = Solarwind_V(p_) * (1.0 + ElectronPressureRatio)

       ! Set or normalize other fluids for multi-fluid equations
       do iFluid = 2, nFluid
          call select_fluid(iFluid)

          ! Set fluid density
          if(.not.IsInput_V(iRho)) then
             if(iFluid == 1)then
                ! By default the solar wind contains the first ion fluid
                Solarwind_V(iRho) = Solarwind_V(Rho_)* &
                     (1.0 - LowDensityRatio*(nFluid - 1))
             else
                ! Other ion fluids are set to a very small fraction
                Solarwind_V(iRho) = Solarwind_V(Rho_)*LowDensityRatio
             end if
          elseif(UseNumberDensity)then
             Solarwind_V(iRho) = Solarwind_V(iRho) &
                  * Io2No_V(UnitN_) * MassFluid_I(iFluid)
          else
             Solarwind_V(iRho) = Solarwind_V(iRho) * Io2No_V(UnitRho_)
          end if

          ! Set fluid pressure
          if(.not.IsInput_V(iP)) then
             ! By default all fluids have the same temperature as the first ion
             Solarwind_V(iP) = Solarwind_V(p_)/Solarwind_V(Rho_) &
                  *Solarwind_V(iRho)*MassIon_I(1)/MassFluid_I(iFluid)
          elseif(UseTemperature)then
             ! Calculate normalized fluid pressure from temperature
             Solarwind_V(iP) = max(Solarwind_V(iP), SwTMinDim) &
                  *Io2No_V(UnitTemperature_)                   &
                  *Solarwind_V(iRho)/MassFluid_I(iFluid)
          else
             ! Normalize fluid pressure
             Solarwind_V(iP) = Solarwind_V(iP) * Io2No_V(UnitP_)
          end if

          ! Normalize or set fluid velocities
          if(IsInput_V(iUx)) then
             Solarwind_V(iUx) = Solarwind_V(iUx) * Io2No_V(UnitU_)
          else
             Solarwind_V(iUx) = Solarwind_V(Ux_)
          end if
          if(IsInput_V(iUy)) then
             Solarwind_V(iUy) = Solarwind_V(iUy) * Io2No_V(UnitU_)
          else
             Solarwind_V(iUy) = Solarwind_V(Uy_)
          end if
          if(IsInput_V(iUz)) then
             Solarwind_V(iUz) = Solarwind_V(iUz) * Io2No_V(UnitU_)
          else
             Solarwind_V(iUz) = Solarwind_V(Uz_)
          end if

       end do

       ! Fix total pressure if necessary
       if(IsMhd .and. UseMultiIon)then
          Solarwind_V(p_) = sum(Solarwind_V(iPIon_I))
          if(.not.UseElectronPressure) &
               Solarwind_V(p_) = (1+ElectronPressureRatio)*Solarwind_V(p_)
       end if

       if(UseAnisoPressure .and. .not. IsInput_V(Ppar_)) &
            Solarwind_V(pPar_) = Solarwind_V(p_)

       if(UseElectronPressure .and. .not. IsInput_V(Pe_))then
          if(ElectronPressureRatio > 0.0)then
             Solarwind_V(Pe_) = Solarwind_V(p_)*ElectronPressureRatio
          else
             Solarwind_V(Pe_) = Solarwind_V(p_)
          end if
       elseif(UseElectronPressure .and. IsInput_V(Pe_))then
          if(UseElectronTemperature) then
              Solarwind_V(Pe_) = max(Solarwind_V(Te_), SwTMinDim) &
                   *Io2No_V(UnitTemperature_)*Solarwind_V(Rho_)
          else
            Solarwind_V(Pe_) = Solarwind_V(Pe_)*Io2No_V(UnitP_)
          end if
       end if
       if(UseAnisoPe .and. .not. IsInput_V(Pepar_)) &
            Solarwind_V(Pepar_) = Solarwind_V(Pe_)

       ! Put back results in big array
       Solarwind_VI(:,iData) = Solarwind_V

    end do ! iData

    if(DoTest)then
       write(*,*)'Io2No_V(UnitP_)',Io2No_V(UnitP_)
       write(*,*)'Io2No_V(UnitN_)*Io2No_V(UnitTemperature_)',&
            Io2No_V(UnitN_)*Io2No_V(UnitTemperature_)
       write(*,*)'After normalization, Solarwind_VI(:,1)=',Solarwind_VI(:,1)
       write(*,*)'After normalization, Solarwind_VI(:,2)=',Solarwind_VI(:,2)
       write(*,*)'After normalization, Solarwind_VI(:,3)=',Solarwind_VI(:,3)
    end if

    call test_stop(NameSub, DoTest)

  end subroutine normalize_solar_wind_data
  !============================================================================
  subroutine get_solar_wind_point(TimeSimulation, Xyz_D, SolarWind_V)

    ! Calculate the solar wind data SolarWind_V expected at time
    ! TimeSimulation and position Xyz_D.
    ! If there is no solar wind file specified, the values given
    ! in the #SOLARWIND command (stored in FaceState_VI) are returned.
    ! If the solar wind file consists of a single line, return
    ! the values in it.
    ! If the solar wind file consists of multiple lines, then first
    ! find the time preceeding TimeSimulation and get the
    ! solar wind velocity at this time.
    !
    ! If the absolute time Time based on TimeSimulation is before
    ! the starting time of the solar wind data,
    ! use the first value. If Time is after the last value, then
    ! either use the last value (if DoReadAgain is false), or wait until the
    ! solar wind file gets more information that covers TimeSimulation.
    ! This wait can be arbitrarily long. Touching the SWMF.KILL file in the
    ! run directory, however, will kill the code with MPI_abort.
    !
    ! If the satellite position (where the solar wind is measured)
    ! has StatelliteXyz_D(1)=0, then it is assumed to be located at the
    ! minimum or maximum X boundary (xMinBox or xMaxBox) depending on the sign
    ! of the solar wind velocity.
    !
    ! Next calculate how long it takes to propagate from the satellite
    ! position to Xyz_D using the solar wind velocity and the orientation
    ! of the normal vector Normal_D that is normal to the assumed plane of the
    ! solar wind. TimeSimulation is shifted by the propagation time to
    ! the "propagated time". Note that when Xyz_D is at the boundary
    ! (xMinBox or xMaxBox)
    ! and the normal vector points in the +X direction, there is no time shift.
    !
    ! Finally the solar wind data is interpolated to the propagated time.
    ! If the propagated time is before the first or after the last time read
    ! from the solar wind file, then use the first or last data values,
    ! respectively.

    use ModKind
    use ModMain, ONLY: xMinBc_, StartTime
    use ModPhysics, ONLY: FaceState_VI, UnitT_, No2Si_V
    use ModVarIndexes
    use ModNumConst, ONLY: cTiny
    use ModGeometry, ONLY: xMinBox, xMaxBox
    use ModUtilities, ONLY: sleep
    use ModInterpolate, ONLY: interpolate_vector

    real, intent(in)  :: TimeSimulation
    real, intent(in)  :: Xyz_D(3)
    real, intent(out) :: SolarWind_V(nVar)  ! Varying solar wind parameters

    integer :: iData
    real(Real8_) :: Time
    real         :: SatDistance, u

    ! Check if the run should be killed while waiting for solar wind file
    logical      :: DoKill

    logical, parameter:: DoTestCell = .false.
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_solar_wind_point'
    !--------------------------------------------------------------------------
    if(nData <= 0 .or. .not.UseSolarwindFile)then
       ! Use fixed boundary conditon if there is no input data
       SolarWind_V = FaceState_VI(:,xMinBc_)
       RETURN
    end if

    if(nData == 1) then
       Solarwind_V = Solarwind_VI(:,1)
       RETURN
    end if

    ! DoTestCell = maxval(abs(Xyz_D - [ xTest, yTest, zTest ])) < 0.1
    if(DoTestCell) write(*,*) NameSub, 'TimeSim, Xyz_D=',TimeSimulation, Xyz_D

    ! Set the X coordinate of the sattellite if not yet set.
    if(SatelliteXyz_D(1) == 0.0)then
       ! Assume that the satellite is at the east or west boundary
       !  Distinguish based on the sign of the first Ux component.
       if(Solarwind_VI(Ux_,1) > 0.0)then
          SatelliteXyz_D(1) = xMinBox
       else
          SatelliteXyz_D(1) = xMaxBox
       endif
    end if
    ! Calculate absolute time
    Time = StartTime + TimeSimulation

    ! Read the input file again if Time exceeds last time in the file
    if(DoReadAgain .and. Time > Time_I(nData))then
       if(iProc==0)write(*,*) NameSub,' waiting for new solar wind data...'
       do
          call read_solar_wind_file
          call normalize_solar_wind_data
          if(Time < Time_I(nData)) EXIT
          inquire(file='SWMF.KILL', exist=DoKill)
          if(DoKill) call stop_mpi(NameSub//': SWMF.KILL file found')
          call sleep(1.0) ! sleep for 1 second CPU time
       end do
    end if

    ! Find data point preceeding this time
    do iData = 1, nData-1
       if(Time_I(iData) >= Time) EXIT
    end do

    if(DoTestCell)then
       write(*,*)NameSub,' Time, Time_I(iData), iData=',&
            Time, Time_I(iData), iData
    end if

    SatDistance = dot_product(Xyz_D - SatelliteXyz_D, Normal_D)
    ! Take into account the propagation time from the satellite plane
    if(abs(SatDistance) > cTiny)then

       ! Get velocity for time shift.
       Solarwind_V = interpolate_vector(Solarwind_VI, nVar=nVar, nDim=1, &
            Min_D=[1], Max_D=[nData], x8_D=[Time], x8_I=Time_I, &
            DoExtrapolate=.false.)

       u = dot_product(SolarWind_V(Ux_:Uz_), Normal_D)
       if(DoTestCell) write(*,*)NameSub,' u_D, u =',Solarwind_V(Ux_:Uz_),u

       ! Shift Time with travel time from the satellite plane
       if(abs(u) > 1e-5)then

          ! Absolute time is in SI units
          Time = Time - SatDistance/u * No2Si_V(UnitT_)

          ! Find data point preceeding this time
          do iData = 1, nData-1
             if(Time_I(iData) >= Time) EXIT
          end do

          if(DoTestCell) write(*,*)NameSub,&
               ' corrected Time, Time_I(iData), iData=',&
               Time, Time_I(iData), iData
       end if
    end if

    ! Interpolate in time
    Solarwind_V = interpolate_vector(Solarwind_VI, nVar=nVar, nDim=1, &
         Min_D=[1], Max_D=[nData], x8_D=[Time], x8_I=Time_I, &
         DoExtrapolate=.false.)

    if(DoTestCell)write(*,*)NameSub,' SolarWind_V =',SolarWind_V

  end subroutine get_solar_wind_point
  !============================================================================
end module ModSolarwind
!==============================================================================

