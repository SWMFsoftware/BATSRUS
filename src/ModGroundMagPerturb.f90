!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModGroundMagPerturb

  use BATL_lib, ONLY: &
       test_start, test_stop, lVerbose, iProc, nProc, iComm

  use ModPlanetConst,    ONLY: rPlanet_I, Earth_
  use ModPhysics,        ONLY: rCurrents, No2Io_V, Si2No_V, UnitB_, UnitJ_
  use ModCoordTransform, ONLY: sph_to_xyz, rot_xyz_sph, cross_product
  use ModConst,          ONLY: cDegToRad

  implicit none
  save

  private ! except

  public:: read_magperturb_param
  public:: init_mod_magperturb
  public:: finalize_magnetometer
  public:: write_magnetometers
  public:: write_geoindices

  logical, public:: DoSaveMags = .false.
  logical, public:: DoSaveGridmag = .false.
  integer, public:: nMagTotal = 0

  real,    public:: Kp=0.0   ! Resulting indices

  ! Public geomagnetic indices variables
  logical, public :: DoWriteIndices=.false., DoCalcKp=.false., DoCalcAe=.false.
  logical, public           :: IsFirstCalc=.true., IsSecondCalc=.true.
  integer, public           :: iSizeKpWindow = 0 ! Size of MagHistory_II
  integer, public, parameter:: nKpMag = 24, nAeMag = 24
  real, public, allocatable :: MagHistory_DII(:,:,:)  ! Mag time history.

  ! Local variables ------

  real               :: AeIndex_I(4)
  integer            :: nMagnetometer=0
  real, allocatable  :: PosMagnetometer_II(:,:)
  character(len=100) :: MagInputFile
  character(len=3)   :: TypeCoordMag='MAG' ! coords for magnetometer list
  character(len=3)   :: TypeCoordGrid='SMG'! coords for magnetometer grid
  character(len=7)   :: TypeMagFileOut='single '

  ! Variables for grid of magnetometers:
  integer:: nGridMag = 0

  character(len=7):: TypeGridFileOut='single'

  ! Array for IE Hall & Pederson contribution (3 x 2 x nMags)
  real, allocatable:: IeMagPerturb_DII(:,:,:)

  ! Fast algorithms: 
  real, allocatable:: LineContrib_DII(:,:,:)
  logical:: UseSurfaceIntegral = .false.      ! true for fast surface integral
  logical:: UseFastFacIntegral = .false.      ! true for fast FAC integral
  character(len=3):: TypeCoordFacGrid = 'SMG' ! 'MAG' for fast integral

  logical:: DoReadMagnetometerFile = .false., IsInitialized = .false.
  integer          :: iUnitMag = -1, iUnitGrid = -1 ! To be removed !!!
  character(len=3), allocatable :: MagName_I(:)

  ! Description of the magnetometer grid
  integer:: nGridLon = 0, nGridLat = 0
  real   :: GridLatMax, GridLatMin, GridLonMin, GridLonMax

  ! Output for magnetometer grid
  real, allocatable:: MagOut_VII(:,:,:)

  ! Private geomagnetic indices variables:
  integer :: nIndexMag = 0  ! Total number of mags required by indices
  integer :: iUnitIndices   ! File IO unit for indices file
  real, parameter    :: KpLat = 60.0           ! Synthetic Kp geomag. latitude
  real, parameter    :: AeLat = 70.0           ! Synthetic AE geomag. latitude
  real               :: k9 = 600.0             ! Scaling of standard K
  real, allocatable  :: LatIndex_I(:), LonIndex_I(:) ! Lat/Lon of geoindex mags
  real               :: XyzKp_DI(3,nKpMag)     ! Locations of Kp stations
  real               :: XyzAe_DI(3,nAeMag)     ! Locations of AE stations
  character(len=3)   :: TypeCoordIndex = 'SMG' ! SMG or MAG system for stations
  integer            :: kIndex_I(nKpMag)       ! Local k-index

  ! K-index is evaluated over a rolling time window, typically three hours.
  ! It may be desirable to reduce this window, changing the Kp index so
  ! that it is more indicative of dB/dt.  This requires careful rescaling
  ! of the K-index conversion tables, however.  Units are left in minutes
  ! because the size of the window should be large, making seconds cumbersome.
  integer :: nKpMins = 180

  ! Output frequency.  The absense of a dn-type variable stems from how
  ! kp is calculated -- using a floating time window that is hard to define
  ! when kp is calculated on a constant iteration cadence instead of a
  ! constant time cadence.
  real :: dtWriteIndices

  ! K CONVERSION TABLES
  ! Conversion table for the standard station, Niemegk.
  real, parameter :: &
       Table50_I(9)=[5.,10.,20.,40.,70.,120.,200.,330.,500.]

  ! Headers for Geoindices output file:
  character(len=*), parameter :: NameKpVars = &
       'Kp K_12 K_13 K_14 K_15 K_16 K_17 K_18 K_19 K_20 K_21 K_22 K_23 '//&
       'K_00 K_01 K_02 K_03 K_04 K_05 K_06 K_07 K_08 K_09 K_10 K_11 '
  character(len=*), parameter :: NameAeVars = 'AL AU AE AO '

contains
  !============================================================================

  subroutine read_magperturb_param(NameCommand)
    ! Handle params for all magnetometer-related commands.

    use ModIO,        ONLY: magfile_, maggridfile_, indexfile_, &
         dn_output,dt_output
    use ModReadParam, ONLY: read_var

    character(len=*), intent(in) :: NameCommand

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_magperturb_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    select case(NameCommand)
    case("#MAGPERTURBINTEGRAL")
       call read_var('UseSurfaceIntegral', UseSurfaceIntegral)
       call read_var('UseFastFacIntegral', UseFastFacIntegral)
       call read_var('TypeCoordIndex',     TypeCoordIndex)
       if(TypeCoordIndex/='MAG' .and. TypeCoordIndex/='SMG') &
            call stop_mpi(NameSub// &
            ': incorrect TypeCoordIndex='//TypeCoordIndex)
       if(UseFastFacIntegral)then
          TypeCoordFacGrid = 'MAG'
       else
          ! Mostly for testing purposes allow setting MAG for slow algorithm
          call read_var('TypeCoordFacGrid', TypeCoordFacGrid)
          if(TypeCoordFacGrid/='MAG' .and. TypeCoordFacGrid/='SMG') &
               call stop_mpi(NameSub// &
               ': incorrect TypeCoordFacGrid='//TypeCoordFacGrid)
       end if

    case("#MAGNETOMETER")
       DoSaveMags = .true.
       call read_var('MagInputFile', MagInputFile)
       call read_var('TypeMagFileOut', TypeMagFileOut)
       call read_var('DnOutput', dn_output(magfile_))
       call read_var('DtOutput', dt_output(magfile_))
       DoReadMagnetometerFile = .true.

    case("#MAGNETOMETERGRID")
       DoSaveGridmag = .true.
       call read_var('TypeFileMagGrid', TypeGridFileOut)
       call read_var('TypeCoordMagGrid',TypeCoordGrid)
       call read_var('nLonMagGrid',     nGridLon)
       call read_var('nLatMagGrid',     nGridLat)
       call read_var('LonMinMagGrid',   GridLonMin)
       call read_var('LonMaxMagGrid',   GridLonMax)
       call read_var('LatMinMagGrid',   GridLatMin)
       call read_var('LatMaxMagGrid',   GridLatMax)
       call read_var('DnSaveMagGrid',   dn_output(maggridfile_))
       call read_var('DtSaveMagGrid',   dt_output(maggridfile_))

    case('#GEOMAGINDICES')
       DoWriteIndices = .true. ! Activiate geoindices output file.
       DoCalcKp = .true.       ! Kp calculated (no others available.)
       DoCalcAe = .true.       ! Ae calculated (always on with Kp).
       call read_var('nKpWindow', nKpMins)
       call read_var('DtOutput' , dtWriteIndices)
       dt_output(indexfile_) = dtWriteIndices
       dn_output(indexfile_) = -1  ! Indices are function of physical time.

    case default
       call stop_mpi(NameSub//': unknown NameCommand='//NameCommand)
    end select

    call test_stop(NameSub, DoTest)
  end subroutine read_magperturb_param
  !============================================================================

  subroutine init_mod_magperturb
    ! Set up the grid of magnetometers and the respective files (if single
    ! file format is selected).

    use ModNumConst, ONLY: cRadToDeg

    integer :: iLat, iLon, iMag
    real    :: dLat, dLon

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_mod_magperturb'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(.not.(DoSaveMags .or. DoSaveGridmag .or. DoWriteIndices)) RETURN

    ! Return if already called
    ! the current implementation does not work for multiple initialization
    if(IsInitialized) RETURN
    IsInitialized=.true.

    ! Initialize geomagnetic indices
    if(DoWriteIndices) call init_geoindices

    ! Check number of magnetometers in the magnetometer file
    if(DoReadMagnetometerFile) call check_mag_input_file

    ! Update total number of magnetometers (shared between GM and IE)
    nGridMag  = nGridLat * nGridLon
    nMagTotal = nMagnetometer + nGridMag + nIndexMag

    if(DoTest)then
       write(*,*) NameSub//'Number of magnetometers:'
       write(*,*) '     Stations: ', nMagnetometer
       write(*,*) '     GridMags: ', nGridMag
       write(*,*) '     IndexMags:', nIndexMag
       write(*,*) '     TOTAL:    ', nMagTotal
    end if

    ! This is just for safety
    if(nMagTotal == 0) RETURN

    ! Allocate/initialize arrays:
    allocate(MagName_I(nMagTotal))
    allocate(PosMagnetometer_II(2,nMagTotal), IeMagPerturb_DII(3,2,nMagTotal) )
    IeMagPerturb_DII = 0.0

    ! Load magnetometer stations, names, coord systems from file:
    if(DoReadMagnetometerFile)then
       call read_mag_input_file
       DoReadMagnetometerFile = .false.
    end if

    ! Calculate magnetometer grid spacing.
    if(DoSaveGridMag)then
       if( GridLonMin+360. == GridLonMax)then
          ! If spanning the globe, do not include both 0 and 360.
          dLon = (GridLonMax - GridLonMin)/max(1, nGridLon)
          GridLonMax = GridLonMax - dLon
       else
          ! If not spanning the globe, grid goes end-to-end
          dLon = (GridLonMax - GridLonMin)/max(1, nGridLon-1)
       endif
       dLat = (GridLatMax - GridLatMin)/max(1, nGridLat-1)

       if(DoTest) then
          write(*,*)NameSub//' nLon and nLat = ', nGridLon, nGridLat
          write(*,*)NameSub//' Lon and Lat spacing = ', dLon, dLat
       end if

       ! Set up the grid.
       iMag = nMagnetometer
       do iLat = 1, nGridLat
          do iLon = 1, nGridLon
             iMag = iMag + 1
             PosMagnetometer_II(1,iMag) = GridLatMin + (iLat-1)*dLat
             PosMagnetometer_II(2,iMag) = GridLonMin + (iLon-1)*dLon
             write(MagName_I(iMag), '(i3.3)')  iMag
             if(DoTest) write(*,*) 'Mag Num, lat, lon: ', &
                  iMag, PosMagnetometer_II(:,iMag)
          end do
       end do
    end if

    ! Add IndexMag info to share to IE:
    if(DoCalcKp)then
       iMag = nMagnetometer+nGridMag
       PosMagnetometer_II(1, iMag+1       :iMag+nKpMag)        = KpLat
       PosMagnetometer_II(1, iMag+nKpMag+1:iMag+nKpMag+nAeMag) = AeLat
       PosMagnetometer_II(2, iMag+1:iMag+nKpMag+nAeMag) = LonIndex_I*cRadToDeg
       MagName_I(iMag+1:iMag+nKpMag) = 'KP_'
    end if

    if(DoTest)then
       write(*,*) NameSub//'Magnetometer positions to send to IE: '
       do iMag=1,nMagTotal
          write(*,*)'  iMag, Lat, Lon =', iMag, PosMagnetometer_II(:,iMag)
       end do
    end if

    ! Open files:
    if (DoSaveMags .and. iProc == 0) &
         call open_magnetometer_output_file('stat')
    if (DoSaveGridMag .and. iProc == 0) &
         call open_magnetometer_output_file('grid')

    call test_stop(NameSub, DoTest)
  end subroutine init_mod_magperturb
  !============================================================================

  subroutine init_geoindices

    ! Initialize variables, arrays, and output file.
    use ModNumConst,  ONLY: cDegToRad, cTwoPi
    use ModUtilities, ONLY: flush_unit, open_file
    use ModMain,      ONLY: n_step
    use ModIoUnit,    ONLY: io_unit_new
    use ModIO,        ONLY: NamePlotDir, IsLogName_e

    integer            :: i, iTime_I(7)
    real               :: RadXY, Phi
    character(len=100) :: NameFile

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_geoindices'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Set number of shared magnetometers.
    if(DoCalcKp)  nIndexMag = nIndexMag + nKpMag
    if(DoCalcAe)  nIndexMag = nIndexMag + nAeMag
    ! if(DoCalcDst) nIndexMag = nIndexMag + nDstMag !Not yet implemented..
    ! ...etc.

    if(DoTest) write(*,*)'Number of IndexMags used: ', nIndexMag

    ! Allocate lat & lon arrays for index mags:
    if(.not. allocated(LatIndex_I)) &
       allocate(LatIndex_I(nIndexMag), LonIndex_I(nIndexMag))

    ! Initialize Kp grid in TypeCoordIndex='SMG' or 'MAG' system
    XyzKp_DI(3,:) = sin(KpLat * cDegToRad) ! Z for all stations.
    RadXY         = cos(KpLat * cDegToRad) ! radial dist. from z-axis.
    do i=1, nKpMag
       Phi = cTwoPi * (i-1)/24.0
       XyzKp_DI(1,i) = RadXY * cos(Phi)
       XyzKp_DI(2,i) = RadXY * sin(Phi)
       if(iProc==0 .and. DoTest) &
            write(*,'(a, 3(1x, e13.3))') 'Kp Coords = ', XyzKp_DI(:,i)
       LatIndex_I(i) = KpLat
       LonIndex_I(i) = Phi
    end do

    ! Initialize Ae grid and arrays, similar to above.
    XyzAe_DI(3,:) = sin(AeLat * cDegToRad) ! Z for all stations.
    RadXY         = cos(AeLat * cDegToRad) ! radial dist. from z-axis.
    do i=1, nAeMag
       Phi = cTwoPi * (i-1)/24.0
       XyzAe_DI(1,i) = RadXY * cos(Phi)
       XyzAe_DI(2,i) = RadXY * sin(Phi)
       if(iProc==0 .and. DoTest) &
            write(*,'(a, 3(1x, e13.3))') 'AE Coords = ', XyzAe_DI(:,i)
       LatIndex_I(i+nKpMag) = AeLat
       LonIndex_I(i+nKpMag) = Phi
    end do

    ! Allocate array to follow time history of magnetometer readings.
    iSizeKpWindow = int(nKpMins / (dtWriteIndices / 60.0))

    ! Allocate MagPerturb_DII, open index file, write header.
    if (iProc==0) then
       allocate(MagHistory_DII(2, nKpMag, iSizeKpWindow))
       MagHistory_DII = 0.0

       if(IsLogName_e)then
          ! Event date added to geoindex file name
          call get_date_time(iTime_I)
          write(NameFile, '(a, a, i4.4, 2i2.2, "-", 3i2.2, a)') &
               trim(NamePlotDir), 'geoindex_e', iTime_I(1:6), '.log'
       else
          write(NameFile, '(a, a, i8.8, a)') &
               trim(NamePlotDir), 'geoindex_n', n_step, '.log'
       end if
       iUnitIndices = io_unit_new()
       call open_file(iUnitIndices, file=NameFile, status='replace')

       write(iUnitIndices, '(2a,f8.2,a,i4.4)') &
            'Synthetic Geomagnetic Indices', &
            ' DtOutput=', DtWriteIndices, '   SizeKpWindow(Mins)=', &
            iSizeKpWindow
       write(iUnitIndices, '(a)', advance='NO') &
            'it year mo dy hr mn sc msc '
       if (DoCalcKp) write(iUnitIndices, '(a)', advance='NO') NameKpVars
       if (DoCalcAe) write(iUnitIndices, '(a)', advance='NO') NameAeVars
       write(iUnitIndices, '(a)') '' ! Close header line.
       call flush_unit(iUnitIndices)
    end if

    call test_stop(NameSub, DoTest)
  end subroutine init_geoindices
  !============================================================================

  subroutine ground_mag_perturb(nMag, Xyz_DI, MagPerturb_DI)

    ! This subroutine is used to calculate the 3D ground magnetic perturbations
    ! at a given set of points (Xyz_DI) for nMag different magnetometers,
    ! from currents in GM cells.  The result is returned as MagPerturb_DI.

    use ModSize,           ONLY: nI, nJ, nK
    use ModGeometry,       ONLY: R_BLK, x1, x2, y1, y2, z1, z2
    use ModMain,           ONLY: &
         Unused_B, nBlock, Time_Simulation, TypeCoordSystem
    use ModNumConst,       ONLY: cPi
    use ModCurrent,        ONLY: get_current
    use CON_axes,          ONLY: transform_matrix
    use BATL_lib,          ONLY: CellVolume_GB, Used_GB, Xyz_DGB, x_, y_, z_

    integer, intent(in)                    :: nMag
    real,    intent(in), dimension(3,nMag) :: Xyz_DI
    real,    intent(out),dimension(3,nMag) :: MagPerturb_DI
    integer  :: i,j,k,iBlock,iMag
    real     :: r3, GmtoSmg_DD(3,3)
    real, dimension(3):: Xyz_D, Current_D, MagPerturb_D

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'ground_mag_perturb'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    call timing_start(NameSub)

    !\
    ! Calculate the magnetic perturbations in cartesian coordinates
    !/

    GmtoSmg_DD = transform_matrix(Time_Simulation, TypeCoordSystem, 'SMG')

    do iMag = 1, nMag
       Xyz_D = Xyz_DI(:,iMag)
       MagPerturb_D = 0.0
       do iBlock=1, nBlock
          if (Unused_B(iBlock))CYCLE
          do k=1, nK; do j=1, nJ; do i=1, nI
             if(.not.Used_GB(i,j,k,iBlock)) CYCLE
             if(r_BLK(i,j,k,iBlock) < rCurrents   .or. &
                  Xyz_DGB(x_,i+1,j,k,iBlock) > x2 .or. &
                  Xyz_DGB(x_,i-1,j,k,iBlock) < x1 .or. &
                  Xyz_DGB(y_,i,j+1,k,iBlock) > y2 .or. &
                  Xyz_DGB(y_,i,j-1,k,iBlock) < y1 .or. &
                  Xyz_DGB(z_,i,j,k+1,iBlock) > z2 .or. &
                  Xyz_DGB(z_,i,j,k-1,iBlock) < z1)   CYCLE

             call get_current(i,j,k,iBlock,Current_D)

             r3 = (sqrt(sum((Xyz_D-Xyz_DGB(:,i,j,k,iBlock))**2)))**3

             ! Add up perturbation
             MagPerturb_D = MagPerturb_D + CellVolume_GB(i,j,k,iBlock)*&
                  cross_product(Current_D, Xyz_D-Xyz_DGB(:,i,j,k,iBlock)) &
                  /(4*cPi*r3)
          end do; end do; end do
       end do

       ! Convert to SMG coordinates
       MagPerturb_DI(:,iMag) = matmul(GmtoSmg_DD, MagPerturb_D)
    end do

    call timing_stop(NameSub)

    call test_stop(NameSub, DoTest)
  end subroutine ground_mag_perturb
  !============================================================================

  subroutine ground_mag_perturb_fac(NameGroup, nMag, Xyz_DI, &
       MagPerturbFac_DI, MagPerturbMhd_DI)

    ! For nMag magnetometers at locations Xyz_DI, calculate the 3-component
    ! pertubation of the magnetic field (returned as MagPerturbFac_DI) due
    ! to "gap region" field-aligned currents.  These are the FACs taken
    ! towards the inner boundary of BATS-R-US and mapped to ionospheric
    ! altitudes (sub-MHD locations, the so-called "gap region") along assumed
    ! dipole field lines.
    !
    ! If UseSurfaceIntegral is true, also calculate the MagPerturbMhd_DI based
    ! on the relationship between the volumetric Biot-Savart integral and the 
    ! following surface integral that gives B at the location x0 as
    !
    ! B(x0) = int_|x|=R [n.B(x) (x-x0) + n x B(x) x (x-x0)] / (4pi*|x-x0|^3)
    !
    ! where n = x/|x| is the radial unit vector and the integral is taken
    ! at the radius R (rCurrents). This was derived by Igor Sokolov. 
    ! For x0=0 we get int n.B*r + n x B x r/(4pir^3) = int B/(4pi r^2) = <B>
    ! i.e. the average of B over the surface.
    !
    ! NOTE: The surface integral includes the external (IMF) field as well. 

    use ModMain,           ONLY: Time_Simulation, n_Step
    use CON_planet_field,  ONLY: get_planet_field, map_planet_field
    use ModNumConst,       ONLY: cPi, cTwoPi
    use ModCurrent,        ONLY: calc_field_aligned_current
    use CON_axes,          ONLY: transform_matrix
    use ModMpi

    !use ModPlotFile, ONLY: save_plot_file
    
    character(len=*), intent(in):: NameGroup
    integer, intent(in) :: nMag
    real,    intent(in) :: Xyz_DI(3,nMag)
    real,    intent(out):: MagPerturbFac_DI(3,nMag)
    real,  intent(inout):: MagPerturbMhd_DI(3,nMag)

    real,    parameter:: rIonosphere = 1.01725 ! rEarth + iono_height
    integer, parameter:: nTheta =181, nPhi =180, nR = 30

    integer           :: k, iHemisphere, iError
    integer           :: iTheta,iPhi,iLine,iMag
    real              :: dR, r, Theta, Phi
    real              :: dTheta, dPhi, SinTheta, dSurface, dVol, dVolCoeff
    real              :: InvBr, FacRcurrents
    real, dimension(3):: Xyz_D, b_D, XyzRcurrents_D, XyzMid_D, Pert_D
    real, save        :: Fac_II(nTheta,nPhi)

    ! Variables for surface integral
    real, save:: b_DII(3,nTheta,nPhi)
    real:: rDotB, rCrossB_D(3)

    ! Avoid recalculating Fac_II and b_DII in the same time step
    integer:: nStepLast = -100

    ! Variables for fast FAC integration (using Igor Sokolov's idea)
    ! The Biot-Savart integral for a given magnetometer indexed by iMag
    ! depends linearly on the FAC(rCurrents,iTheta,iPhi), so the contribution
    ! can be precalculated, which eliminates the expensive part of the 
    ! calculation. Storage of the contribution factors is distributed among
    ! the processors.

    integer:: iLineProc, nLineProc    ! linear line index for Lon-Lat grid
    integer, parameter:: nGroup = 4   ! number of magnetometer groups
    integer:: iGroup = 1              ! group index
    integer:: iMag0                   ! magnetometer index shift

    ! becomes true once the first integration was done for the group
    logical:: UseFastFacIntegral_I(nGroup) = .false. 

    ! Coordinate system for the grid that collects the FAC
    logical         :: DoConvertCoord
    real            :: SmToFacGrid_DD(3,3)

    !real:: Surface, Volume, Height=1.0 !!! to test surface/volume integration
    
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'ground_mag_perturb_fac'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    call timing_start('ground_db_fac')

    DoConvertCoord = TypeCoordFacGrid == 'MAG'
    if(DoConvertCoord) &
         SmToFacGrid_DD   = transform_matrix(Time_Simulation, 'SMG', 'MAG')
    
    MagPerturbFac_DI= 0.0

    dTheta = cPi    / (nTheta-1)
    dPhi   = cTwoPi / nPhi
    dR     = (rCurrents - rIonosphere) / nR

    if(UseSurfaceIntegral) MagPerturbMhd_DI = 0.0
    
    if(n_Step /= nStepLast)then
       nStepLast = n_Step
       ! Get the radial component of the field aligned current
       ! and the magnetic field vector (for surface integral) at rCurrents
       call timing_start('ground_calc_fac')
       if(UseSurfaceIntegral)then
          call calc_field_aligned_current(nTheta,nPhi,rCurrents, &
               Fac_II, b_DII, TypeCoordFacGrid=TypeCoordFacGrid, &
               IsRadialAbs=.true., FacMin=1e-4/No2Io_V(UnitJ_))
          if(nProc > 1)&
               call MPI_Bcast(b_DII, 3*nTheta*nPhi, MPI_REAL, 0, iComm, iError)
       else
          call calc_field_aligned_current(nTheta,nPhi,rCurrents, &
               Fac_II, TypeCoordFacGrid=TypeCoordFacGrid, &
               IsRadialAbs=.true., FacMin=1e-4/No2Io_V(UnitJ_))
       end if
       if(nProc > 1) &
            call MPI_Bcast(Fac_II, nTheta*nPhi, MPI_REAL, 0, iComm, iError)
       call timing_stop('ground_calc_fac')
    end if


    !if(iProc==0) call save_plot_file('fac.out', NameVarIn = "Lon Lat Fac", &
    !     CoordMinIn_D = [1.0, -89.5], CoordMaxIn_D = [359., 89.5], &
    !     VarIn_II=transpose(Fac_II))

    iLine = -1

    if(UseFastFacIntegral)then

       ! Allocate storage for all magnetometers
       if(.not.allocated(LineContrib_DII))then
          nLineProc = 1 + (nTheta*nPhi-1)/nProc
          allocate(LineContrib_DII(3,nMagTotal,nLineProc))
          LineContrib_DII = 0.0
          if(iProc==0) write(*,*) NameSub, &
               ' allocated LineContrib_DII(3,',nMagTotal,',',nLineProc,')'
       end if

       ! Set group index iGroup and 
       ! starting magnetometer index iMag0 for LineContrib_DII
       select case(NameGroup)
       case('stat')
          iGroup = 1
          iMag0 = 0
       case('grid')
          iGroup = 2
          iMag0 = nMagnetometer
       case('kp')
          iGroup = 3
          iMag0 = nMagnetometer + nGridMag
       case('ae')
          iGroup = 4
          iMag0 = nMagnetometer + nGridMag
          if(DoCalcKp) iMag0 = iMag0 + nKpMag
       case default
          call stop_mpi(NameSub//': unknown NameGroup='//NameGroup)
       end select
    end if

    if(UseFastFacIntegral_I(iGroup))then
       call timing_start('ground_fast_int')
       iLineProc = 0
       do iTheta = 1, nTheta
          do iPhi = 1, nPhi
             iLine = iLine + 1
             if(mod(iLine, nProc) /= iProc)CYCLE

             iLineProc = iLineProc + 1

             FacRcurrents = Fac_II(iTheta,iPhi)
             if(FacRcurrents == 0.0) CYCLE

             do iMag = 1, nMag

                ! Add contribution from this line to this magnetometer
                MagPerturbFac_DI(:,iMag) = MagPerturbFac_DI(:,iMag) + &
                     FacRcurrents * LineContrib_DII(:,iMag+iMag0,iLineProc)
             end do
          end do
       end do
       call timing_stop('ground_fast_int')
    else
       call timing_start('ground_slow_int')
       if(UseFastFacIntegral)then
          iLineProc = 0
          ! Next time the integrals can be reused
          ! if the station group corotates with the dipole
          select case(iGroup)
          case(1)
             ! Stations
             UseFastFacIntegral_I(iGroup) = &
                  TypeCoordMag == 'MAG' .or. TypeCoordMag == 'GEO'
          case(2)
             ! grid
             UseFastFacIntegral_I(iGroup) = &
                  TypeCoordGrid == 'MAG' .or. TypeCoordGrid == 'GEO'
          case(3,4)
             ! Kp and Ae indexes
             UseFastFacIntegral_I(iGroup) = TypeCoordIndex == 'MAG'
          end select
          if(iProc==0) write(*,*) NameSub,': ',NameGroup, &
               ' UseFastFacIntegral=', UseFastFacIntegral_I(iGroup)

       end if

       ! CHECK
       !Volume = 0.; Surface = 0.
       
       do iTheta = 1, nTheta
          Theta = (iTheta-1) * dTheta
          ! At the poles sin(Theta)=0, but the area of the triangle 
          ! formed by the pole and the latitude segment at Theta=dTheta/2
          ! is approximately dTheta/4*dTheta/2, so the sin(theta) is
          ! replaced with dTheta/8.
          SinTheta = max(sin(Theta), dTheta/8)

          do iPhi = 1, nPhi

             Phi = (iPhi-1) * dPhi

             ! If the FAC is under certain threshold, do nothing
             ! This  should be commented out for testing the volume
             if (Fac_II(iTheta,iPhi) == 0.0 &
                  .and. .not.(UseFastFacIntegral.or.UseSurfaceIntegral)) CYCLE

             iLine = iLine + 1

             ! do parallel computation among the processors
             if(mod(iLine, nProc) /= iProc)CYCLE

             ! Count local line index
             if(UseFastFacIntegral) iLineProc = iLineProc + 1

             call sph_to_xyz(rCurrents, Theta, Phi, XyzRcurrents_D)

             ! the field aligned current and B field at r=rCurrents
             FacRcurrents = Fac_II(iTheta,iPhi)

             ! Calculate volume element
             dSurface  = rCurrents**2*SinTheta*dTheta*dPhi
             dVolCoeff = dR*dSurface

             if(UseSurfaceIntegral)then

                ! B(x0) = int_|x|=R
                !   [n.B(x) (x-x0) + n x B(x) x (x-x0)] / (4pi*|x-x0|^3)
                ! where x0 = Xyz_DI, x = XyzRcurrents_D, |r|=rCurrents

                b_D       = b_DII(:,iTheta,iPhi)
                rDotB     = sum(XyzRcurrents_D*b_D)/rCurrents
                rCrossB_D = cross_product(XyzRcurrents_D, b_D)/rCurrents

                ! CHECK
                !Surface = Surface + dSurface
                !if(iTheta==nTheta .and. iPhi==nPhi) &
                !   write(*,*)'!!! Surface=', Surface, 4*cPi*rCurrents**2

                do iMag = 1, nMag
                   Xyz_D = Xyz_DI(:,iMag)
                   ! This should be done in advance !!!
                   if(DoConvertCoord) Xyz_D = matmul(SmToFacGrid_DD, Xyz_D)

                   ! x-x0
                   Xyz_D = XyzRcurrents_D - Xyz_D

                   MagPerturbMhd_DI(:,iMag) = MagPerturbMhd_DI(:,iMag) &
                        + (rDotB*Xyz_D + cross_product(rCrossB_D, Xyz_D)) &
                        *dSurface/(4*cPi*sqrt(sum(Xyz_D**2))**3)
                end do
                
             end if

             do k = 1, nR

                ! Second order integration in radial direction
                r = rCurrents - dR*(k-0.5)

                ! get next position along the field line
                call map_planet_field(Time_Simulation, XyzRcurrents_D, &
                     TypeCoordFacGrid//' NORM', r, XyzMid_D, iHemisphere)

                ! get the B field at this position
                call get_planet_field(&
                     Time_Simulation, XyzMid_D, TypeCoordFacGrid//' NORM', b_D)
                b_D = b_D*Si2No_V(UnitB_)

                ! The volume element is proportional to 1/Br. The sign
		! should be preserved (not yet!!!),
                ! because the sign is also there in the radial
		! component of the field aligned current: Br/B*FAC.
		! In the end j_D = b_D/Br*[(Br/B)*(j.B)]_rcurr
                InvBr = r/abs(sum(b_D*XyzMid_D))
                dVol  = dVolCoeff*InvBr

                !! Check correctness of integration. Needs Br at rCurrents.
                !if(abs(XyzMid_D(3)) > rCurrents - Height) &
                !     Volume = Volume + abs(dVol &
                !     *sum(XyzRcurrents_D*b_DII(:,iTheta,iPhi))/rCurrents)
                
                do iMag = 1, nMag
                   Xyz_D = Xyz_DI(:,iMag)

                   if(DoConvertCoord) Xyz_D = matmul(SmToFacGrid_DD, Xyz_D)

                   ! Do Biot-Savart integral JxR/|R|^3 dV for all magnetometers
                   ! where the field aligned J is proportional to B
                   Pert_D = dVol*cross_product(b_D, Xyz_D-XyzMid_D) &
                        /(4*cPi*(sqrt(sum((XyzMid_D-Xyz_D)**2)))**3)

                   MagPerturbFac_DI(:,iMag) = MagPerturbFac_DI(:,iMag) + &
                        FacRcurrents*Pert_D

                   ! Store contribution for fast method
                   if(UseFastFacIntegral) &
                        LineContrib_DII(:,iMag+iMag0,iLineProc) &
                        = LineContrib_DII(:,iMag+iMag0,iLineProc) + Pert_D

                   !if(...)then
                   !   write(*,*)'Xyz_D   =', Xyz_D
                   !   write(*,*)'XyzMid_D=', XyzMid_D
                   !   write(*,*)'b_D, InvBr=', b_D, InvBr
                   !   write(*,*)'dVol, dVolCoeff, FacRcurrents=', &
                   !        dVol, dVolCoeff, FacRcurrents
                   !   write(*,*)'Pert_D      =', Pert_D
                   !   write(*,*)'MagPerturb_D=', MagPerturbFac_DI(:,iMag)
                   !
                   !   call stop_mpi('DEBUG')
                   !end if
                   
                end do ! iMag
             end do ! kr
          end do ! iPhi
       end do ! iTheta
       call timing_stop('ground_slow_int')
    end if
    !! Volume of the two spherical caps above Height (should be filled
    !! with field lines). See https://en.wikipedia.org/wiki/Spherical_cap
    !write(*,*)'!!! Volumes =', Volume, 2*cPi*Height**2*(rCurrents - Height/3)

    ! Convert
    if(DoConvertCoord)then
       do iMag = 1, nMag
          MagPerturbFac_DI(:,iMag) = &
               matmul(MagPerturbFac_DI(:,iMag), SmToFacGrid_DD)
       end do
    end if

    call timing_stop('ground_db_fac')

    call test_stop(NameSub, DoTest)
  end subroutine ground_mag_perturb_fac
  !============================================================================
  subroutine calc_kp

    use CON_axes,      ONLY: transform_matrix
    use ModPhysics,    ONLY: No2Io_V, UnitB_
    use ModMain,       ONLY: time_simulation,TypeCoordSystem
    use ModIeCoupling, ONLY: calc_ie_mag_perturb
    use ModMpi

    integer :: i, iError
    real, dimension(3,3):: IndexToGm_DD, IndexToSm_DD, XyzSph_DD, SmgNed_DD
    real, dimension(3,nKpMag):: &
         dBmag_DI, dBfac_DI, dBHall_DI, dBPedersen_DI, dBsum_DI, Xyz_DI

    real :: dB_I(2)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_kp'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    call timing_start(NameSub)

    ! Coordinate transformations
    IndexToGm_DD = transform_matrix(Time_simulation, &
         TypeCoordIndex, TypeCoordSystem)
    IndexToSm_DD = transform_matrix(Time_simulation, &
         TypeCoordIndex, 'SMG')

    ! Obtain geomagnetic pertubations dB*_DI in SMG coordinates

    ! Location for MHD contribution in GM coordinates
    if(.not.UseSurfaceIntegral)then
       Xyz_DI = matmul(IndexToGm_DD, XyzKp_DI)
       call ground_mag_perturb(nKpMag, Xyz_DI, dBmag_DI)
    end if

    ! Location for IE contributions are in SMG coordinates
    Xyz_DI = matmul(IndexToSm_DD, XyzKp_DI)
    call calc_ie_mag_perturb(nKpMag, Xyz_DI, dBHall_DI, dBPedersen_DI)

    ! Location for gap region contribution in SMG (should be in MAG)
    call ground_mag_perturb_fac('kp', nKpMag, Xyz_DI, dBfac_DI, dBmag_DI)

    ! Add up contributions and convert to IO units (nT)
    dBsum_DI = (dBmag_DI + dBfac_DI + dBHall_DI + dBPedersen_DI) &
         *No2Io_V(UnitB_)

    ! Convert from SMG components to North-East-Down components
    do i=1, nKpMag

       ! Rotation matrix from Cartesian to spherical coordinates
       ! Xyz_DI is now in SM coordinates (see above)
       XyzSph_DD = rot_xyz_sph(Xyz_DI(:,i))

       ! Rotation matrix from SMG to local North-East-Down components
       ! North = -Theta
       SmgNed_DD(:,1) = -XyzSph_DD(:,2)
       ! East = Phi
       SmgNed_DD(:,2) =  XyzSph_DD(:,3)
       ! Down = -R
       SmgNed_DD(:,3) = -XyzSph_DD(:,1)

       dBsum_DI(:,i)= matmul(dBsum_DI(:,i),  SmgNed_DD)
    end do

    ! MPI Reduce to head node.
    if(nProc>1) call MPI_reduce_real_array(dBsum_DI, size(dBsum_DI), &
         MPI_SUM, 0, iComm, iError)

    ! Head node calculates K-values and shares them with all other nodes.
    if(iProc==0)then
       ! Shift MagHistory to make room for new measurements.
       MagHistory_DII(:,:,1:iSizeKpWindow-1) = &
            MagHistory_DII(:,:,2:iSizeKpWindow)

       do i = 1, nKpMag
          ! Store North and East components
          if (IsFirstCalc .or. IsSecondCalc) then
             ! First or second calc, fill in whole array to initialize.
             MagHistory_DII(1,i,:) = dBsum_DI(1,i)
             MagHistory_DII(2,i,:) = dBsum_DI(2,i)
          else
             ! Later in simulation, fill in the most recent value only
             MagHistory_DII(:,i,iSizeKpWindow) = dBsum_DI(1:2,i)
          end if

          ! Calculate dB_I(x,y), convert to K index
          dB_I(1) = maxval(MagHistory_DII(1,i,:)) - &
               minval(MagHistory_DII(1,i,:))
          dB_I(2) = maxval(MagHistory_DII(2,i,:)) - &
               minval(MagHistory_DII(2,i,:))
          kIndex_I(i) = k_index(maxval(DB_I))
       end do

       ! Kp is average of Ks.
       Kp = sum(kIndex_I)/real(nKpMag)

       ! Quantize to 1/3 levels.
       Kp = nint(3*Kp)/3.0

    end if

    call MPI_Bcast(Kp, 1, MPI_REAL, 0, iComm, iError)
    
    call timing_stop(NameSub)

    call test_stop(NameSub, DoTest)
  end subroutine calc_kp
  !============================================================================

  subroutine calc_ae

    use CON_axes,      ONLY: transform_matrix
    use ModPhysics,    ONLY: No2Io_V, UnitB_
    use ModMain,       ONLY: time_simulation,TypeCoordSystem
    use ModIeCoupling, ONLY: calc_ie_mag_perturb
    use ModMpi

    integer :: i, iError
    real, dimension(3,3):: IndexToGm_DD, IndexToSm_DD, XyzSph_DD, SmgNed_DD
    real, dimension(3,nAeMag):: &
         dBmag_DI, dBfac_DI, dBHall_DI, dBPedersen_DI, dBsum_DI, Xyz_DI

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_ae'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    call timing_start(NameSub)
    
    ! Coordinate transformations
    IndexToGm_DD = transform_matrix(Time_simulation, &
         TypeCoordIndex, TypeCoordSystem)
    IndexToSm_DD = transform_matrix(Time_simulation, &
         TypeCoordIndex, 'SMG')
    
    ! Obtain geomagnetic pertubations dB*_DI in SMG coordinates

    if(.not.UseSurfaceIntegral)then
       ! Location for MHD contribution in GM coordinates
       Xyz_DI = matmul(IndexToGm_DD, XyzAe_DI)
       call ground_mag_perturb(nAeMag, Xyz_DI, dBmag_DI)
    endif

    ! Location for IE contributions are in SMG coordinates
    Xyz_DI = matmul(IndexToSm_DD, XyzAe_DI)
    call calc_ie_mag_perturb(nAeMag, Xyz_DI, dBHall_DI, dBPedersen_DI)

    ! Location for gap region contribution in SMG (should be in MAG)
    call ground_mag_perturb_fac('ae', nAeMag, Xyz_DI, dBfac_DI, dBmag_DI)

    ! Add up contributions and convert to IO units (nT)
    dBsum_DI = (dBmag_DI + dBfac_DI + dBHall_DI + dBPedersen_DI) &
         *No2Io_V(UnitB_)

    ! Convert from SMG components to North-East-Down components
    do i=1, nAeMag

       ! Rotation matrix from Cartesian to spherical coordinates
       XyzSph_DD = rot_xyz_sph(Xyz_DI(:,i))

       ! Rotation matrix from SMG to local North-East-Down components
       ! North = -Theta
       SmgNed_DD(:,1) = -XyzSph_DD(:,2)
       ! East = Phi
       SmgNed_DD(:,2) =  XyzSph_DD(:,3)
       ! Down = -R
       SmgNed_DD(:,3) = -XyzSph_DD(:,1)

       dBsum_DI(:,i)= matmul(dBsum_DI(:,i),  SmgNed_DD)
    end do

    ! MPI Reduce to head node.
    if(nProc>1) call MPI_reduce_real_array(dBsum_DI, size(dBsum_DI), &
         MPI_SUM, 0, iComm, iError)

    ! Now, calculate AE indices on head node only.
    if(iProc==0) then
       AeIndex_I(1) = minval(dBsum_DI(1,:))          ! AL Index
       AeIndex_I(2) = maxval(dBsum_DI(1,:))          ! AU Index
       AeIndex_I(3) = AeIndex_I(2) - AeIndex_I(1)    ! AE Index
       AeIndex_I(4) = (AeIndex_I(2)+AeIndex_I(1))/2. ! AO Index
    end if

    call timing_stop(NameSub)

    call test_stop(NameSub, DoTest)
  end subroutine calc_ae
  !============================================================================
  subroutine check_mag_input_file
    ! Set number of magnetometers listed in the input file:
    ! set nMagnetometer

    use ModIoUnit, ONLY: UnitTmp_
    use ModIO,     ONLY: iUnitOut, Write_prefix
    use ModUtilities, ONLY: open_file, close_file
    use ModMpi

    integer :: iError

    ! One line of input
    character (len=100) :: Line
    character(len=3) :: NameMag
    real             :: LatMag, LonMag

    integer          :: nMag
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'check_mag_input_file'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Read file on the root processor
    if (iProc == 0) then

       if(lVerbose>0)then
          call write_prefix; write(iUnitOut,*) NameSub, &
               " reading: ",trim(MagInputFile)
       end if

       call open_file(file=MagInputFile, status="old")

       nMag = 0

       ! Read the file: read #COORD TypeCoord, #START
       READFILE: do

          read(UnitTmp_,'(a)', iostat = iError ) Line

          if (iError /= 0) EXIT READFILE

          if(index(Line,'#COORD')>0) then
             read(UnitTmp_,'(a)') TypeCoordMag
             select case(TypeCoordMag)
             case('MAG','GEO','SMG')
                call write_prefix;
                write(iUnitOut,'(a)') 'Magnetometer Coordinates='//TypeCoordMag
             case default
                call stop_mpi(NameSub//' invalid TypeCoordMag='//TypeCoordMag)
             end select
          endif

          if(index(Line,'#START')>0)then
             READPOINTS: do
                read(UnitTmp_,*, iostat=iError) NameMag, LatMag, LonMag
                if (iError /= 0) EXIT READFILE

                ! Add new points
                nMag = nMag + 1

             end do READPOINTS

          end if

       end do READFILE

       call close_file

       if(DoTest)write(*,*) NameSub,': nMagnetometer=', nMag

       ! Number of magnetometers in the file
       nMagnetometer = nMag

    end if

    ! Tell the coordinates to the other processors
    call MPI_Bcast(TypeCoordMag, 3, MPI_CHARACTER, 0, iComm, iError)

    ! Tell the number of magnetometers to the other processors
    call MPI_Bcast(nMagnetometer, 1, MPI_INTEGER, 0, iComm, iError)

    call test_stop(NameSub, DoTest)
  end subroutine check_mag_input_file
  !============================================================================

  subroutine read_mag_input_file
    ! Read the magnetometer input file which governs the number of virtual
    ! magnetometers to be used and their location and coordinate systems.
    ! Input values read from file are saved in module-level variables.

    use ModIO, ONLY: iUnitOut, Write_prefix
    use ModIoUnit, ONLY: UnitTmp_
    use ModUtilities, ONLY: open_file, close_file

    use ModMpi

    integer :: iError

    ! One line of input
    character (len=100):: StringLine
    integer            :: iMag

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_mag_input_file'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Read file on the root processor
    if (iProc == 0) then

       if(lVerbose>0)then
          call write_prefix; write(iUnitOut,*) NameSub, &
               " reading: ",trim(MagInputFile)
       end if

       ! Read in magnetometer positions and names
       call open_file(file=MagInputFile, status="old")
       READFILE: do
          read(UnitTmp_,'(a)') StringLine
          if(index(StringLine, '#START') > 0)then
             do iMag = 1, nMagnetometer
                read(UnitTmp_,*) MagName_I(iMag), PosMagnetometer_II(1:2,iMag)
             end do
             EXIT READFILE
          end if
       end do READFILE
       call close_file
    end if

    ! Tell the magnetometer name to the other processors
    call MPI_Bcast(MagName_I, nMagnetometer*3, MPI_CHARACTER, 0, &
         iComm, iError)
    ! Tell the other processors the coordinates
    call MPI_Bcast(PosMagnetometer_II, 2*nMagnetometer, MPI_REAL, 0, &
         iComm, iError)

    call test_stop(NameSub, DoTest)
  end subroutine read_mag_input_file
  !============================================================================

  subroutine open_magnetometer_output_file(NameGroup)
    ! Open and initialize the magnetometer output file.  A new IO logical unit
    ! is created and saved for future writes to this file.

    use ModMain,   ONLY: n_step
    use ModIoUnit, ONLY: io_unit_new
    use ModIO,     ONLY: NamePlotDir, IsLogName_e
    use ModUtilities, ONLY: flush_unit, open_file

    character(len=4), intent(in) :: NameGroup

    character(len=100):: NameFile, StringPrefix, TypeFileNow
    integer :: iMag, iTime_I(7), iUnitNow, iEnd, iStart, nMagNow

    ! Open the output file
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'open_magnetometer_output_file'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Magnetometer grid file or regular file?
    if(NameGroup == 'stat')then
       StringPrefix = 'magnetometers'
       iStart       = 0
       iEnd         = nMagnetometer
       TypeFileNow = TypeMagFileOut
    else if(NameGroup == 'grid')then
       StringPrefix = 'gridMags'
       iStart       = nMagnetometer
       iEnd         = nMagnetometer+nGridMag
       TypeFileNow  = TypeGridFileOut
    else
       call stop_mpi('open_magnetometer_output_files: unrecognized ' // &
            'magnetometer group: '//NameGroup)
    end if

    ! Total number of magnetometers written out now:
    nMagNow = iEnd-iStart

    ! If writing new files every time, no initialization needed.
    if(TypeFileNow /= 'single') RETURN

    if(IsLogName_e)then
       ! Event date added to magnetic perturbation file name
       call get_date_time(iTime_I)
       write(NameFile, '(3a, i4.4, 2i2.2, "-", 3i2.2, a)') &
            trim(NamePlotDir), trim(StringPrefix),'_e', iTime_I(1:6), '.mag'
    else
       write(NameFile,'(3a, i8.8, a)') &
            trim(NamePlotDir), trim(StringPrefix), '_n', n_step, '.mag'
    end if
    if(DoTest) then
       write(*,*) 'open_magnetometer_output_files: NameFile:', NameFile
    end if

    iUnitNow= io_unit_new()
    call open_file(iUnitNow, FILE=NameFile)

    ! Write the header
    write(iUnitNow, '(i5,a)',ADVANCE="NO") nMagNow, ' magnetometers:'
    do iMag=1,nMagnow-1
       write(iUnitNow, '(1X,a)', ADVANCE='NO') MagName_I(iMag+iStart)
    end do
    write(iUnitNow, '(1X,a)') MagName_I(iEnd)
    write(iUnitNow, '(a)')  &
         'nstep year mo dy hr mn sc msc station X Y Z '// &
         'dBn dBe dBd dBnMhd dBeMhd dBdMhd dBnFac dBeFac dBdFac ' // &
         'dBnHal dBeHal dBdHal dBnPed dBePed dBdPed'

    ! Save file IO unit.
    if(NameGroup == 'stat')then
       iUnitMag=iUnitNow
    else if(NameGroup == 'grid')then
       iUnitGrid=iUnitNow
    end if

    call flush_unit(iUnitNow)

    call test_stop(NameSub, DoTest)
  end subroutine open_magnetometer_output_file
  !============================================================================

  subroutine write_geoindices

    use ModMain,  ONLY: n_step
    use ModUtilities, ONLY: flush_unit
    use ModMpi

    integer :: iTime_I(7)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_geoindices'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Calculate indices on all nodes.
    if(DoCalcKp) call calc_kp
    if(DoCalcAe) call calc_ae
    ! if(DoCalcDst) call calc_dst ...etc...

    if(iProc > 0) RETURN ! Write only on head node.

    ! Write date and time.
    call get_date_time(iTime_I)
    write(iUnitIndices, '(i7.7, i5.4, 5(i3.2), i4.3)', ADVANCE='NO') &
         n_step, iTime_I

    ! if(DoCalcDst) write(..., ADVANCE='NO') dst

    if(DoCalcKp) &
         write(iUnitIndices, '(f5.2,100i2)', ADVANCE='NO') Kp, kIndex_I
    if(DoCalcAe) &
         write(iUnitIndices, '(4(1x,f9.2))', ADVANCE='NO') AeIndex_I
    ! Add newline
    write(iUnitIndices, *)

    call flush_unit(iUnitIndices)

    ! Ensure two initialization steps are not repeated.
    if(.not. IsFirstCalc) IsSecondCalc=.false.
    IsFirstCalc=.false.

    call test_stop(NameSub, DoTest)
  end subroutine write_geoindices
  !============================================================================

  subroutine write_magnetometers(NameGroup)
    ! Write ground magnetometer field perturbations to file. Values, IO units,
    ! and other information is gathered from module level variables.

    use ModIeCoupling, ONLY: calc_ie_mag_perturb
    use CON_axes, ONLY: transform_matrix
    use ModMain,  ONLY: n_step, time_simulation, TypeCoordSystem
    use ModUtilities, ONLY: flush_unit
    use ModMpi

    ! NameGroup determines which group of magnetometers will be written
    ! to file: regular stations ('stat') or grid magnetometers ('grid').
    character(len=4), intent(in) :: NameGroup

    integer :: iMag, iError, iStart, iEnd, iUnitOut, nMagNow

    character(len=3):: TypeCoordNow
    character(len=6):: TypeFileNow

    real:: Xyz_D(3)
    real:: MagtoGm_DD(3,3), GmtoSm_DD(3,3), XyzSph_DD(3,3), SmgNed_DD(3,3)

    real, dimension(:,:), allocatable :: &
         MagGmXyz_DI, MagSmXyz_DI, &
         dBMhd_DI, dBFac_DI, dBHall_DI, dBPedersen_DI, dBTotal_DI

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_magnetometers'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Configure output to cover specific magnetometer group:
    if (NameGroup == 'stat') then
       iStart       = 0
       iEnd         = nMagnetometer
       iUnitOut     = iUnitMag
       TypeCoordNow = TypeCoordMag
       TypeFileNow  = TypeMagFileOut
    else if (NameGroup == 'grid') then
       iStart       = nMagnetometer
       iEnd         = nMagnetometer+nGridMag
       iUnitOut     = iUnitGrid
       TypeCoordNow = TypeCoordGrid
       TypeFileNow  = TypeGridFileOut  ! should be "2d"
    else
       call stop_mpi(NameSub// &
            ': Unrecognized magnetometer group: '//NameGroup)
    end if

    ! Total number of magnetometers written out now:
    nMagNow = iEnd - iStart

    if(DoTest) then
       write(*,*) 'WRITING MAGNETOMETERS: Type=', NameGroup
       write(*,'(a, 3i4, 1x, a)') 'iStart, iEnd, iUnitOut, TypeCoordNow = ', &
            iStart, iEnd, iUnitOut, TypeCoordNow
       write(*,*) 'nMagNow = ', nMagNow
    end if

    ! Allocate variables:
    allocate( &
         MagGmXyz_DI(3,nMagNow),  MagSmXyz_DI(3,nMagNow), &
         dBMhd_DI(3,nMagNow),  dBFac_DI(3,nMagNow), &
         dBHall_DI(3,nMagNow), dBPedersen_DI(3,nMagNow), &
         dBTotal_DI(3,nMagNow))

    ! Matrix between coordinate systems
    MagtoGm_DD = transform_matrix(Time_Simulation, &
         TypeCoordNow, TypeCoordSystem)
    GmtoSm_DD  = transform_matrix(Time_Simulation, &
         TypeCoordSystem, 'SMG')

    ! Transform the Radius position into cartesian coordinates.
    ! Transform the magnetometer position from MagInCoord to GM/SM

    do iMag = 1, nMagNow
       ! (360,360) is for the station at the center of the planet
       if ( nint(PosMagnetometer_II(1,iMag+iStart)) == 360 .and. &
            nint(PosMagnetometer_II(2,iMag+iStart)) == 360) then
          Xyz_D = 0.0
       else
          call  sph_to_xyz(1.0,                           &
               (90-PosMagnetometer_II(1,iMag+iStart))*cDegToRad, &
               PosMagnetometer_II(2,iMag+iStart)*cDegToRad, Xyz_D)
          Xyz_D = matmul(MagtoGm_DD, Xyz_D)
       end if

       MagGmXyz_DI(:,iMag) = Xyz_D
       MagSmXyz_DI(:,iMag) = matmul(GmtoSm_DD, Xyz_D)
    end do

    ! Obtain geomagnetic pertubations in SMG coordinates
    if(.not.UseSurfaceIntegral) &
         call ground_mag_perturb(nMagNow, MagGmXyz_DI, dBMhd_DI)
    call ground_mag_perturb_fac(NameGroup, nMagNow, MagSmXyz_DI, &
         dBFac_DI, dBMhd_DI)
    call calc_ie_mag_perturb(nMagNow, MagSmXyz_DI, dBHall_DI, dBPedersen_DI)

    ! Collect the variables from all the PEs
    ! Use dBTotal as a temporary variable
    if(nProc > 1)then
       call MPI_reduce_real_array(dBMhd_DI, size(dBMhd_DI), &
            MPI_SUM, 0, iComm, iError)

       call MPI_reduce_real_array(dBFac_DI, size(dBFac_DI), &
            MPI_SUM, 0, iComm, iError)

       call MPI_reduce_real_array(dBHall_DI, size(dBHall_DI), &
            MPI_SUM, 0, iComm, iError)

       call MPI_reduce_real_array(dBPedersen_DI, size(dBPedersen_DI), &
            MPI_SUM, 0, iComm, iError)
    end if

    ! convert perturbation into output format and write them out
    if(iProc==0)then
       do iMag = 1, nMagNow
          ! Convert from SMG components to North-East-Down components
          ! except for the "DST" station at the center of the Earth.
          if(any(MagSmXyz_DI(:,iMag) /= 0.0)) then

             ! Rotation matrix from Cartesian to spherical coordinates
             XyzSph_DD = rot_xyz_sph(MagSmXyz_DI(:,iMag))

             ! Rotation matrix from SMG to local North-East-Down components
             ! North = -Theta
             SmgNed_DD(:,1) = -XyzSph_DD(:,2)
             ! East = Phi
             SmgNed_DD(:,2) =  XyzSph_DD(:,3)
             ! Down = -R
             SmgNed_DD(:,3) = -XyzSph_DD(:,1)

             dBMhd_DI(:,iMag)     = matmul(dBMhd_DI(:,iMag),  SmgNed_DD)
             dBFac_DI(:,iMag)     = matmul(dBFac_DI(:,iMag),  SmgNed_DD)
             dBHall_DI(:,iMag)    = matmul(dBHall_DI(:,iMag), SmgNed_DD)
             dBPedersen_DI(:,iMag)= matmul(dBPedersen_DI(:,iMag), SmgNed_DD)
          end if

       end do

       ! convert the magnetic perturbations to I/O units
       dBMhd_DI      = No2Io_V(UnitB_)*dBMhd_DI
       dBFac_DI      = No2Io_V(UnitB_)*dBFac_DI
       dBHall_DI     = No2Io_V(UnitB_)*dBHall_DI
       dBPedersen_DI = No2Io_V(UnitB_)*dBPedersen_DI

       ! Get total perturbation:
       dBTotal_DI = dBMhd_DI + dBFac_DI + dBHall_DI + dBPedersen_DI

       select case(TypeFileNow)
       case('single')
          call write_mag_single
       case('step')
          call write_mag_step
       case('ascii', 'real4', 'real8', 'tec')
          call write_mag_2d
       case('station')
          call stop_mpi(NameSub//': separate mag files not implemented yet.')
       end select

    end if

    ! Release memory.
    deallocate(MagGmXyz_DI, MagSmXyz_DI, &
         dBMhd_DI, dBFac_DI, dBHall_DI, dBPedersen_DI, dBTotal_DI)

    call test_stop(NameSub, DoTest)
  contains
    !==========================================================================
    subroutine write_mag_2d

      use ModPlotFile, ONLY: save_plot_file
      use ModIO, ONLY: NamePlotDir, IsLogName_e

      integer, parameter:: nVar = 15
      character(len=*), parameter:: NameVar = &
           "Lon Lat dBn dBe dBd dBnMhd dBeMhd dBdMhd dBnFac dBeFac dBdFac "// &
           "dBnHal dBeHal dBdHal dBnPed dBePed dBdPed"

      integer ::  iTime_I(7), iLon, iLat, iMag

      character(len=100):: NameFile
      !------------------------------------------------------------------------
      if(allocated(MagOut_VII))then
         if(size(MagOut_VII) /= nGridMag*nVar) deallocate(MagOut_VII)
      end if
      if(.not.allocated(MagOut_VII)) &
           allocate(MagOut_VII(nVar,nGridLon,nGridLat))

      iMag = 0
      do iLat = 1, nGridLat
         do iLon = 1, nGridLon
            iMag = iMag + 1
            MagOut_VII( 1: 3,iLon,iLat) = dBTotal_DI(:,iMag)
            MagOut_VII( 4: 6,iLon,iLat) = dBMhd_DI(:,iMag)
            MagOut_VII( 7: 9,iLon,iLat) = dBFac_DI(:,iMag)
            MagOut_VII(10:12,iLon,iLat) = dBHall_DI(:,iMag)
            MagOut_VII(13:15,iLon,iLat) = dBPedersen_DI(:,iMag)
         end do
      end do

      if(IsLogName_e)then
         ! Event date added to magnetic perturbation grid file name
         call get_date_time(iTime_I)
         write(NameFile, '(a, i4.4, 2i2.2, "-", 3i2.2, a)') &
              trim(NamePlotDir)//'mag_grid_e', iTime_I(1:6), '.out'
      else
         write(NameFile,'(a, i8.8, a)') &
              trim(NamePlotDir)//'mag_grid_n', n_step, '.out'
      end if

      call save_plot_file(NameFile, TypeFileIn=TypeFileNow, &
           StringHeaderIn = "Magnetometer grid ("//TypeCoordNow//") [deg] "// &
           "dB (North-East-Down) [nT]", &
           TimeIn = time_simulation, &
           NameVarIn = NameVar, &
           CoordMinIn_D = [ GridLonMin, GridLatMin ], &
           CoordMaxIn_D = [ GridLonMax, GridLatMax ], &
           VarIn_VII=MagOut_VII)

    end subroutine write_mag_2d
    !==========================================================================
    subroutine write_mag_single
      ! For TypeMagFileOut == 'single', write a single record to the file.

      integer :: iTime_I(7)
      !------------------------------------------------------------------------
      ! Get current time.
      call get_date_time(iTime_I)

      ! Write data to file.
      do iMag=1, nMagNow
         ! Write time and magnetometer number to file:
         write(iUnitOut,'(i8)',ADVANCE='NO') n_step
         write(iUnitOut,'(i5,5(1X,i2.2),1X,i3.3)',ADVANCE='NO') iTime_I
         write(iUnitOut,'(1X,i4)', ADVANCE='NO')  iMag

         ! Write position of magnetometer and perturbation to file:
         write(iUnitOut,'(18es13.5)') &
              MagSmXyz_DI(:,iMag)*rPlanet_I(Earth_), &
              dBTotal_DI(:,iMag), &
              dBMhd_DI(:,iMag), dBFac_DI(:,iMag), &
              dBHall_DI(:,iMag), dBPedersen_DI(:,iMag)
      end do

      ! Flush file buffer.
      call flush_unit(iUnitOut)

    end subroutine write_mag_single
    !==========================================================================

    subroutine write_mag_step

      ! For TypeMagFileOut == 'step', write one file for every write step.

      use ModIoUnit,    ONLY: UnitTmp_
      use ModUtilities, ONLY: open_file, close_file
      use ModIO,        ONLY: NamePlotDir, IsLogName_e

      integer ::  iTime_I(7)

      character(len=13) :: StringPrefix
      character(len=100):: NameFile
      !------------------------------------------------------------------------
      call get_date_time(iTime_I)

      if(NameGroup == 'stat') then
         StringPrefix='magnetometers'
      else if (NameGroup == 'grid') then
         StringPrefix='gridMags'
      endif

      if(IsLogName_e)then
         ! Event date added to magnetic perturbation file name
         write(NameFile, '(3a, i4.4, 2i2.2, "-", 3i2.2, a)') &
              trim(NamePlotDir), trim(StringPrefix),'_e', iTime_I(1:6), '.mag'
      else
         write(NameFile,'(3a, i8.8, a)') &
              trim(NamePlotDir), trim(StringPrefix),'_n', n_step, '.mag'
      end if

      ! Open file for output:
      call open_file(file=NameFile)

      ! Write the header
      write(UnitTmp_, '(i5,a)',ADVANCE="NO") nMagNow, ' magnetometers:'
      do iMag=1,nMagNow-1
         write(UnitTmp_, '(1X,a)', ADVANCE='NO') MagName_I(iMag+iStart)
      end do
      write(UnitTmp_, '(1X,a)') MagName_I(iEnd)
      write(UnitTmp_, '(a)')  &
           'nstep year mo dy hr mn sc msc station X Y Z '// &
           'dBn dBe dBd dBnMhd dBeMhd dBdMhd dBnFac dBeFac dBdFac ' // &
           'dBnHal dBeHal dBdHal dBnPed dBePed dBdPed'

      ! Write data to file.
      do iMag=1, nMagNow
         ! Write time and magnetometer number to file:
         write(UnitTmp_,'(i8)',ADVANCE='NO') n_step
         write(UnitTmp_,'(i5,5(1X,i2.2),1X,i3.3)',ADVANCE='NO') iTime_I
         write(UnitTmp_,'(1X,i4)', ADVANCE='NO')  iMag

         ! Write position of magnetometer and perturbation to file:
         write(UnitTmp_,'(18es13.5)') &
              MagSmXyz_DI(:,iMag)*rPlanet_I(Earth_), &
              dBTotal_DI(:,iMag), &
              dBMhd_DI(:,iMag), dBFac_DI(:,iMag), &
              dBHall_DI(:,iMag), dBPedersen_DI(:,iMag)
      end do

      ! Close file:
      call close_file

    end subroutine write_mag_step
    !==========================================================================
  end subroutine write_magnetometers
  !============================================================================

  subroutine finalize_magnetometer

    ! Close the magnetometer output files (flush buffer, release IO unit).
    ! Deallocate arrays.

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'finalize_magnetometer'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(iProc==0 .and. DoSaveMags .and. TypeMagFileOut /= 'step') &
         close(iUnitMag)
    if(iProc==0 .and. DoWriteIndices) close(iUnitIndices)
    if (allocated(IeMagPerturb_DII)) deallocate(IeMagPerturb_DII)
    if (allocated(MagName_I)) deallocate(MagName_I)
    if (allocated(MagHistory_DII)) deallocate(MagHistory_DII)
    if (allocated(LineContrib_DII)) deallocate(LineContrib_DII)

    call test_stop(NameSub, DoTest)
  end subroutine finalize_magnetometer
  !============================================================================

  integer function k_index(DeltaB)

    ! Convert a deltaB value (max-min over window) to a K-value using given
    ! the standard conversion table that lists the upper limit for each K
    ! window.  For example, a K of 0 is given for a deltaB <= 5, table(1) = 5.
    ! This table is scaled by k9, or the k9 for the station where the
    ! measurement is actually taken.

    real, intent(in) :: DeltaB

    integer :: i

    !--------------------------------------------------------------------------
    do i = 1, 9
       if( DeltaB - Table50_I(i)*k9/Table50_I(9) < 0) then
          k_index = i - 1
          RETURN
       end if
    end do
    k_index = 9

  end function k_index
  !============================================================================

end module ModGroundMagPerturb
!==============================================================================
