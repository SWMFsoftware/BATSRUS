!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
!^CMP FILE iM

module GM_couple_im

  use BATL_lib, ONLY: iProc, iComm
  use ModMpi
  use ModNumConst, ONLY: cRadToDeg, cDegToRad, cHalfPi
  use CON_coupler, ONLY: Grid_C, ncell_id, &
       nVarBuffer_CC, iVarSource_VCC, GM_, IM_, lComp_I

  use ModMain, ONLY: nStep, &
       DoMultiFluidIMCoupling, DoAnisoPressureIMCoupling
  use ModPhysics, ONLY: No2Si_V, Si2No_V, &
       UnitP_, UnitRho_, UnitTemperature_, UnitB_, &
       Bdp, DipoleStrengthSi, rCurrents, rBody

  use ModImCoupling, ONLY: nVarCouple, iVarCouple_V
  use ModUtilities, ONLY: CON_stop

  implicit none

  private ! except

  public:: GM_get_for_im_trace_crcm ! for iM/CRCM
  public:: GM_get_for_im_crcm       ! for iM/CRCM
  public:: GM_get_sat_for_im_crcm   ! for iM/CRCM
  public:: GM_get_for_im_trace      ! for iM/RAM
  public:: GM_get_for_im_line       ! for iM/RAM
  public:: GM_get_for_im            ! for iM/RCM
  public:: GM_satinit_for_im        ! initialize satellite
  public:: GM_get_sat_for_im        ! get satellite info
  public:: GM_put_from_im           ! from iM
  public:: GM_put_from_im_cimi           ! from iM

  character(len=*), parameter :: NameMod='GM_couple_im'

  ! iM Grid size
  integer :: nCells_D(2), iSize,jSize

  ! Information about the iM grid: 2D non-uniform regular grid
  real, allocatable, dimension(:) :: ImLat_I, ImLon_I

  integer :: i,j, i0

  real, save, dimension(:), allocatable :: &
       MHD_lat_boundary
  real, save, dimension(:,:), allocatable :: &
       MHD_SUM_vol, MHD_tmp, &
       MHD_SUM_rho, MHD_HpRho, MHD_OpRho, &
       MHD_SUM_p, MHD_HpP, MHD_OpP, &
       MHD_Beq, &
       MHD_Xeq, &
       MHD_Yeq, &
       MHD_Fluxerror
  real, parameter :: noValue=-99999.
  real :: eqB
  real :: colat,Ci,Cs,FCiCs,factor,Vol,Ri,s2,s6,s8,factor1,factor2

  integer :: iError

  logical :: DoTestTec, DoTestIdl

  ! This is for the GM-iM/RAM coupling
  real, allocatable :: StateLine_VI(:,:)

contains
  !============================================================================
  ! FOR CRCM COUPLING
  subroutine GM_get_for_im_trace_crcm(iSizeIn, jSizeIn, nDensityIn, &
       NameVar, nVarLine, nPointLine)

    ! Do field tracing for iM.
    ! Provide total number of points along rays
    ! and the number of variables to pass to iM
    use ModFieldTrace, ONLY: DoExtractUnitSi, integrate_field_from_sphere
    use CON_line_extract, ONLY: line_get
    use CON_planet, ONLY: RadiusPlanet, IonosphereHeight

    integer, intent(in)           :: iSizeIn, jSizeIn, nDensityIn
    character (len=*), intent(in) :: NameVar
    integer, intent(out)          :: nVarLine, nPointLine
    real :: Radius

    character(len=*), parameter:: NameSub = 'GM_get_for_im_trace_crcm'
    !--------------------------------------------------------------------------
    ! Allocate arrays
    call allocate_gm_im(iSizeIn, jSizeIn)

    Radius = (RadiusPlanet + IonosphereHeight) / RadiusPlanet

    ! The CRCM ionosphere radius at 100km altitude in normalized units
    Radius = (6378.+100.)/6378.  !!! could be derived from Grid_C ?

    DoExtractUnitSi = .true.

    call integrate_field_from_sphere(&
         iSizeIn, jSizeIn, ImLat_I, ImLon_I, Radius, NameVar)

    call line_get(nVarLine, nPointLine)

    nVarLine = 4 ! We only pass line index, length, B and radial distance to iM

  end subroutine GM_get_for_im_trace_crcm
  !============================================================================

  subroutine GM_get_for_im_crcm(Buffer_IIV, KpOut, AeOut, iSizeIn, jSizeIn, &
       nDensityIn, nVarIn, BufferLine_VI, nVarLine, nPointLine, &
       BufferSolarWind_V, NameVar)

    use ModGeometry, ONLY: xMaxBox
    use ModIoUnit, ONLY: UNITTMP_
    use ModMain, ONLY: tSimulation, TypeCoordSystem
    use ModVarIndexes, ONLY: &
         Rho_, RhoUx_, RhoUy_, RhoUz_, Bx_, By_, Bz_, p_, Ppar_, &
         iRho_I, iP_I, MassFluid_I, IonFirst_, IonLast_, nVar
    use ModPhysics, ONLY: No2Si_V, &
         UnitN_, UnitU_, UnitB_, UnitP_, rBody
    use ModSolarwind, ONLY: get_solar_wind_point
    use ModConst, ONLY: cProtonMass
    use ModNumConst, ONLY: cRadToDeg
    use ModGroundMagPerturb, ONLY: DoCalcKp, kP, DoCalcAe, AeIndex_I
    use CON_planet_field,  ONLY: map_planet_field

    use CON_line_extract, ONLY: line_get, line_clean
    use CON_axes,         ONLY: transform_matrix
    use CON_planet,       ONLY: RadiusPlanet

    integer, intent(in) :: iSizeIn, jSizeIn, nDensityIn, nVarIn
    real,    intent(out):: Buffer_IIV(iSizeIn,jSizeIn,nVarIn), KpOut, AeOut

    integer, intent(in) :: nPointLine, nVarLine
    real, intent(out)   :: BufferLine_VI(nVarLine, nPointLine)

    ! solar wind values
    real,    intent(out):: BufferSolarWind_V(8)

    character (len=*), intent(in):: NameVar

    integer :: nVarExtract, nPoint, iPoint, iStartPoint
    real, allocatable :: Buffer_VI(:,:)

    integer :: iLat,iLon,iLine,iLocBmin,iIon
    real    :: SmGm_DD(3,3), XyzBminSm_D(3), XyzEndSm_D(3), XyzEndSmIono_D(3)
    real    :: SolarWind_V(nVar)
    character(len=100) :: NameOut

    real :: RadiusIono
    integer :: iHemisphere
    logical :: DoTest, DoTestMe
    character(len=*), parameter:: NameSub = 'GM_get_for_im_crcm'
    !--------------------------------------------------------------------------
    if(iProc /= 0) RETURN
    call CON_set_do_test(NameSub, DoTest, DoTestMe)

    ! The CRCM ionosphere radius in normalized units
    RadiusIono = (6378.+100.)/6378.  !!! could be derived from Grid_C ?

    ! If kP is being calculated, share it.  Otherwise, share -1.
    if(DoCalcKp) then
       KpOut = kP
    else
       KpOut = -1
    endif

    ! If Ae is being calculated, share it.  Otherwise, share -1.
    if(DoCalcAe) then
       AeOut = AeIndex_I(3)
    else
       AeOut = -1
    endif

    ! Set buffer variable indexes
    if(.not.allocated(iVarCouple_V)) call set_buffer_indexes(nVarIn-5, NameSub)

    ! Initialize buffer_iiv
    Buffer_IIV = 0.0

    ! Put the extracted data into BufferLine_I
    call line_get(nVarExtract, nPoint)
    if(nPoint /= nPointLine)call CON_stop(NameSub//': nPointLine error')
    if(nVarExtract < nVarLine)call CON_stop(NameSub//': nVarLine error')
    allocate(Buffer_VI(0:nVarExtract, nPoint))
    call line_get(nVarExtract, nPoint, Buffer_VI, DoSort=.true.)

    ! Transformation matrix between CRCM(SM) and GM coordinates
    SmGm_DD = transform_matrix(tSimulation,TypeCoordSystem,'SMG')

    ! The first field line starts from iPoint = 1
    iStartPoint = 1
    do iPoint = 1, nPoint

       iLine =  Buffer_VI(0,iPoint)     ! line index
       iLat = mod(iLine-1, iSizeIn) + 1
       iLon = (iLine-1)/iSizeIn + 1

       BufferLine_VI(1,iPoint) = iLine
       BufferLine_VI(2,iPoint) = Buffer_VI(1,iPoint)                 ! Length
       BufferLine_VI(3,iPoint) = sqrt(sum(Buffer_VI(2:4,iPoint)**2)) ! |r|
       BufferLine_VI(4,iPoint) = &
            sqrt(sum(Buffer_VI(4+Bx_:4+Bz_,iPoint)**2))       ! |B|

       ! Find the location of minimum B, Bmin, and other variables at Bmin
       ! for each field line
       if(Buffer_VI(0,min(nPoint,iPoint+1)) /= Buffer_VI(0,iPoint) &
            .or. iPoint == nPoint)then
          ! Exclude open field lines by checking the radial
          ! distance of the last point on a field line
          if(BufferLine_VI(3,iPoint) > 1.0001*rBody*RadiusPlanet)then
             ! set line index to -1. for non-closed field line
             ! It would be much better not to send the line at all !!!
             BufferLine_VI(1,iStartPoint:iPoint) = -1
             Buffer_IIV(iLat, iLon, :) = NoValue

          else
             ! For closed field lines
             ! Location of Bmin for this field line
             iLocBmin = minloc(BufferLine_VI(4,iStartPoint:iPoint), DIM=1) &
                  + iStartPoint - 1

             ! Convert location from GM to SMG coordinates
             XyzBminSm_D = matmul(SmGm_DD, Buffer_VI(2:4,iLocBmin))
             Buffer_IIV(iLat,iLon,1) = XyzBminSm_D(1)            ! x
             Buffer_IIV(iLat,iLon,2) = XyzBminSm_D(2)            ! y

             Buffer_IIV(iLat,iLon,3) = BufferLine_VI(4,iLocBmin) ! Bmin

             ! get the conjugage point in SMG coordinates and pass it
             XyzEndSm_D=matmul(SmGm_DD,Buffer_VI(2:4,iPoint))/RadiusPlanet
             call map_planet_field(tSimulation, XyzEndSm_D, 'SMG NORM', &
                  RadiusIono, XyzEndSmIono_D, iHemisphere)

             ! SmLat of conjugate point
             Buffer_IIV(iLat,iLon,4) = 90.0-acos(XyzEndSmIono_D(3)&
                  /sqrt(sum(XyzEndSmIono_D(:)**2)))*cRadToDeg

             ! SmLon of conjugate point
             Buffer_IIV(iLat,iLon,5) = atan2(XyzEndSm_D(2),XyzEndSmIono_D(1))*cRadToDeg

             ! Put coupled variables (densities and pressures) into buffer
             Buffer_IIV(iLat,iLon,6:) = Buffer_VI(4+iVarCouple_V,iLocBmin)

             ! write(*,*) 'iVarCouple_V in GM_copuple_im'
             ! write(*,*) iVarCouple_V

             ! call CON_stop(NameSub//' done')

          end if

          ! Set the start point for the next field line
          iStartPoint = iPoint +1
       end if
    end do

    deallocate(Buffer_VI)
    call line_clean

    ! Pass the solar wind values note that the first ion density is passed
    ! as the solar wind number density

    call get_solar_wind_point(tSimulation, [xMaxBox, 0.0, 0.0], SolarWind_V)

    BufferSolarWind_V(1) = SolarWind_V(Rho_)/MassFluid_I(IonFirst_)*No2Si_V(UnitN_)
    BufferSolarWind_V(2) = SolarWind_V(RhoUx_) * No2Si_V(UnitU_)
    BufferSolarWind_V(3) = SolarWind_V(RhoUy_) * No2Si_V(UnitU_)
    BufferSolarWind_V(4) = SolarWind_V(RhoUz_) * No2Si_V(UnitU_)
    BufferSolarWind_V(5) = SolarWind_V(Bx_) * No2Si_V(UnitB_)
    BufferSolarWind_V(6) = SolarWind_V(By_) * No2Si_V(UnitB_)
    BufferSolarWind_V(7) = SolarWind_V(Bz_) * No2Si_V(UnitB_)
    BufferSolarWind_V(8) = SolarWind_V(p_)  * No2Si_V(UnitP_)

    ! test only for anisop
    if(iProc == 0 .and. DoTest .and. DoAnisoPressureIMCoupling)then
       write(NameOut,"(a,f6.1)") 'GM_get_for_IM_t_', tSimulation
       open(UnitTmp_,FILE=NameOut)
       write(UnitTmp_,"(a)") &
            'GM_get_for_im_crcm, Buffer_IIV, last index 1:5 and 7 '
       write(UnitTmp_,"(a)") 'Xeq Yeq Beq rho p ppar'
       do iLat =iSizeIn,1,-1
          do iLon =1,jSizeIn
             write(UnitTmp_,"(100es18.10)") Buffer_IIV(iLat,iLon,1:5), &
                  Buffer_IIV(iLat,iLon,7)
          enddo
       enddo
       close(UnitTmp_)
    end if

  end subroutine GM_get_for_im_crcm
  !============================================================================

  subroutine GM_get_sat_for_im_crcm(Buffer_III, Name_I, nSats)

    ! Subroutine to update and collect satellite locations for iM tracing

    ! Modules
    use ModSatelliteFile, ONLY: FilenameSat_I, XyzSat_DI, gm_trace_sat
    use ModWriteLogSatFile, ONLY: collect_satellite_data
    use ModMain,          ONLY: UseB0, nBlock
    use ModPhysics,       ONLY: No2Si_V, UnitB_
    use ModVarIndexes,    ONLY: nVar, Bx_, Bz_
    use ModB0,            ONLY: get_b0
    use ModCurrent,       ONLY: get_point_data
    use ModMPI

    ! Arguments
    integer, intent(in)               :: nSats
    real, intent(out)                 :: Buffer_III(4,2,nSats)
    character (len=100), intent(out)  :: Name_I(nSats)

    ! Internal variables
    real ::SatRay_D(3)
    real :: StateSat_V(0:nVar+3), B0Sat_D(3)
    integer :: iSat

    character(len=*), parameter:: NameSub = 'GM_get_sat_for_im_crcm'
    !--------------------------------------------------------------------------
    ! Store satellite names in Buffer_I (known on all processors)
    Name_I = FilenameSat_I(1:nSats)

    do iSat = 1, nSats
       ! Update satellite position.
       call GM_trace_sat(XyzSat_DI(1:3,iSat), SatRay_D)

       call get_point_data( &
            0.0, XyzSat_DI(:,iSat), 1, nBlock, Bx_, Bz_, StateSat_V)
       call collect_satellite_data(XyzSat_DI(:,iSat), StateSat_V)

       ! Store results in Buffer_III from processor zero
       if (iProc == 0) then
          Buffer_III(1:3,1,iSat) = XyzSat_DI(1:3,iSat)
          Buffer_III(1:3,2,iSat) = SatRay_D

          ! Determine total magnetic field squared at satellite: B2 = (B0+B1)^2
          B0Sat_D = 0.0
          if(UseB0) call get_b0(XyzSat_DI(:,iSat), B0Sat_D)
          Buffer_III(4,2,iSat)   = &
               sum( (StateSat_V(Bx_:Bz_) + B0Sat_D)**2 ) * No2Si_V(UnitB_)**2
       end if
    end do

  end subroutine GM_get_sat_for_im_crcm
  !============================================================================

  subroutine GM_get_for_im_trace(nRadius, nLon, nVarLine, nPointLine, NameVar)

    ! Do Trace_DSNB tracing for iM/RAM_SCB.
    ! Provide total number of points along rays
    ! and the number of variables to pass to iM

    use ModMain,       ONLY: tSimulation, TypeCoordSystem
    use ModVarIndexes, ONLY: Bx_, Bz_, nVar
    use ModIO,         ONLY: NamePrimitiveVarOrig
    use CON_comp_param,   ONLY: lNameVersion
    use CON_world,        ONLY: get_comp_info
    use CON_line_extract, ONLY: line_get, line_clean
    use CON_coupler,      ONLY: Grid_C, IM_
    use CON_axes,         ONLY: transform_matrix
    use ModMultiFluid,    ONLY: nFluid, iUx_I, iUz_I
    use ModFieldTrace,    ONLY: DoExtractBGradB1, DoExtractEfield, &
         trace_field_equator

    integer, intent(in)           :: nRadius, nLon
    integer, intent(out)          :: nVarLine, nPointLine
    character (len=*), intent(out):: NameVar

    real, allocatable, save :: RadiusIm_I(:), LongitudeIm_I(:)

    integer :: nVarExtract, iPoint, iUx5, iUz5, iFluid
    real    :: SmGm_DD(3,3)

    character(len=lNameVersion) :: NameVersionIm
    character(len=*), parameter:: NameSub = 'GM_get_for_im_trace'
    !--------------------------------------------------------------------------

    if(.not.allocated(RadiusIm_I))then
       if(   nRadius /= Grid_C(IM_) % nCoord_D(1) .or. &
            nLon    /= Grid_C(IM_) % nCoord_D(2) )then
          write(*,*)NameSub//' grid sizes do not agree nRadius,nLon,nCells=',&
               nRadius, nLon, Grid_C(IM_) % nCoord_D(1:2)
          call CON_stop(NameSub//' ERROR')
       end if
       allocate(RadiusIm_I(nRadius), LongitudeIm_I(nLon))
       RadiusIm_I    = Grid_C(IM_) % Coord1_I
       LongitudeIm_I = Grid_C(IM_) % Coord2_I
    end if

    ! Trace field lines starting from iM equatorial grid (do message pass)
    ! Include BGradB1 only if needed:
    call get_comp_info(IM_, NameVersion=NameVersionIm)
    if(NameVersionIm(1:7) == 'RAM-SCB')then
       DoExtractBGradB1 = .false. ! RAM-SCB does not need BGradB1
       DoExtractEfield  = .false. ! RAM-SCB does not need Efield
    else
       DoExtractBGradB1 = .true.  ! HEIDI needs BGradB1
       DoExtractEfield  = .true.  ! HEIDI needs Efield
    end if

    ! The variables to be passed: line index, length along line,
    ! coordinatess and primitive variables. Total is 5 + nVar.
    NameVar = 'iLine Length x y z '//NamePrimitiveVarOrig
    if(DoExtractBGradB1) NameVar = trim(NameVar)//' bgradb1x bgradb1y bgradb1z'
    if(DoExtractEfield)  NameVar = trim(NameVar)//' Ex Ey Ez Epotx Epoty Epotz'

    call trace_field_equator(nRadius, nLon, RadiusIm_I, LongitudeIm_I, .true.)

    if(iProc /= 0) RETURN

    ! Put the extracted data into BufferLine_VI
    call line_get(nVarExtract, nPointLine)
    ! The +1 is for the line index
    nVarLine = nVarExtract + 1
    allocate(StateLine_VI(nVarLine, nPointLine))
    ! Get all the line data and clean up
    call line_get(nVarExtract, nPointLine, StateLine_VI, DoSort=.true.)
    call line_clean

    ! Transformation matrix between the SM and GM coordinates
    SmGm_DD = transform_matrix(tSimulation,TypeCoordSystem,'SMG')
    do iPoint = 1, nPointLine
       StateLine_VI(3:5,iPoint)  = &
            matmul(SmGm_DD, StateLine_VI(3:5,iPoint)) ! X,Y,Z
       do iFluid = 1, nFluid
          iUx5 = iUx_I(iFluid)+5; iUz5 = iUz_I(iFluid)+5
          StateLine_VI(iUx5:iUz5,iPoint)  = &
               matmul(SmGm_DD, StateLine_VI(iUx5:iUz5,iPoint)) ! velocity
       end do
       StateLine_VI(Bx_+5:Bz_+5,iPoint)= &
            matmul(SmGm_DD, StateLine_VI(Bx_+5:Bz_+5,iPoint)) ! mag field

       ! b.grad(B1)
       if(DoExtractBGradB1) &
            StateLine_VI(nVar+6:nVar+8,iPoint) = &
            matmul(SmGm_DD, StateLine_VI(nVar+6:nVar+8,iPoint))

       ! E and Epot
       if(DoExtractEfield)then
          StateLine_VI(nVar+9:nVar+11,iPoint) = &
               matmul(SmGm_DD, StateLine_VI(nVar+9:nVar+11,iPoint))
          StateLine_VI(nVar+12:nVar+14,iPoint) = &
               matmul(SmGm_DD, StateLine_VI(nVar+12:nVar+14,iPoint))
       end if

    end do

  end subroutine GM_get_for_im_trace
  !============================================================================

  subroutine GM_get_for_im_line(nRadius, nLon, MapOut_DSII, &
       nVarLine, nPointLine, BufferLine_VI)

    use ModFieldTrace, ONLY: RayMap_DSII

    integer, intent(in) :: nRadius, nLon
    real,    intent(out):: MapOut_DSII(3,2,nRadius,nLon)
    integer, intent(in) :: nPointLine, nVarLine
    real, intent(out)   :: BufferLine_VI(nVarLine, nPointLine)

    logical :: DoTest, DoTestMe

    ! This routine should be called from processor 0 only
    character(len=*), parameter:: NameSub = 'GM_get_for_im_line'
    !--------------------------------------------------------------------------
    if(iProc /= 0) RETURN

    call CON_set_do_test(NameSub, DoTest, DoTestMe)

    ! Transfer extracted rays
    BufferLine_VI = StateLine_VI

    ! Transfer mapping info
    MapOut_DSII = RayMap_DSII

    deallocate(RayMap_DSII, StateLine_VI)

  end subroutine GM_get_for_im_line
  !============================================================================

  subroutine GM_get_for_im(Buffer_IIV,KpOut,iSizeIn,jSizeIn,nVar,NameVar)

    use ModFieldTrace, ONLY: RayResult_VII, RayIntegral_VII, &
         InvB_, Z0x_, Z0y_, Z0b_, RhoInvB_, pInvB_,  &
         HpRhoInvB_, OpRhoInvB_, HpPInvB_, OpPInvB_, iXEnd, CLOSEDRAY, &
         integrate_field_from_sphere
    use ModGroundMagPerturb, ONLY: DoCalcKp, kP

    integer,          intent(in) :: iSizeIn, jSizeIn, nVar
    real,             intent(out):: Buffer_IIV(iSizeIn,jSizeIn,nVar), KpOut
    character(len=*), intent(in) :: NameVar

    real :: Radius

    logical :: DoTest, DoTestMe

    character(len=*), parameter:: NameSub = 'GM_get_for_im'
    !--------------------------------------------------------------------------
    if(DoMultiFluidIMCoupling)then
       if(NameVar /= 'vol:z0x:z0y:bmin:Hprho:Oprho:Hpp:Opp') &
            call CON_stop(NameSub//' invalid NameVar='//NameVar)
    else
       if(NameVar /= 'vol:z0x:z0y:bmin:rho:p') &
            call CON_stop(NameSub//' invalid NameVar='//NameVar)
    end if
    call CON_set_do_test(NameSub//'_tec', DoTestTec, DoTestMe)
    call CON_set_do_test(NameSub//'_idl', DoTestIdl, DoTestMe)
    call CON_set_do_test(NameSub, DoTest, DoTestMe)

    ! Allocate arrays
    call allocate_gm_im(iSizeIn, jSizeIn)

    ! The RCM ionosphere radius at 100km altitude in normalized units
    Radius = (6378.+100.)/6378.  !!! could be derived from Grid_C ?
    if(.not. DoMultiFluidIMCoupling)then
       call integrate_field_from_sphere(&
            iSizeIn, jSizeIn, ImLat_I, ImLon_I, Radius, &
            'InvB,RhoInvB,pInvB,Z0x,Z0y,Z0b')
    else
       call integrate_field_from_sphere(&
            iSizeIn, jSizeIn, ImLat_I, ImLon_I, Radius, &
            'InvB,RhoInvB,pInvB,Z0x,Z0y,Z0b,HpRhoInvB,OpRhoInvB,HppInvB,OppInvB')
       ! but not pass Rhoinvb, Pinvb to iM
    end if

    if(iProc==0)then
       ! Copy RayResult into small arrays
       MHD_SUM_vol = RayResult_VII(InvB_   ,:,:)
       MHD_Xeq     = RayResult_VII(Z0x_    ,:,:)
       MHD_Yeq     = RayResult_VII(Z0y_    ,:,:)
       MHD_Beq     = RayResult_VII(Z0b_    ,:,:)
       if(.not.DoMultiFluidIMCoupling)then
          MHD_SUM_rho = RayResult_VII(RhoInvB_,:,:)
          MHD_SUM_p   = RayResult_VII(pInvB_  ,:,:)
       else
          MHD_HpRho= RayResult_VII(HpRhoInvB_,:,:)
          MHD_OpRho= RayResult_VII(OpRhoInvB_,:,:)
          MHD_HpP= RayResult_VII(HpPInvB_,:,:)
          MHD_OpP= RayResult_VII(OpPInvB_,:,:)
       end if
       ! Put impossible values if the Trace_DSNB is not closed
       if(.not.DoMultiFluidIMCoupling)then
          where(RayResult_VII(iXEnd,:,:) <= CLOSEDRAY)
             MHD_Xeq     = NoValue
             MHD_Yeq     = NoValue
             MHD_SUM_vol = 0.0
             MHD_SUM_rho = 0.0
             MHD_SUM_p   = 0.0
             MHD_Beq     = NoValue
          end where
       else
          where(RayResult_VII(iXEnd,:,:) <= CLOSEDRAY)
             MHD_Xeq     = NoValue
             MHD_Yeq     = NoValue
             MHD_SUM_vol = 0.0
             MHD_Hprho = 0.0
             MHD_Oprho = 0.0
             MHD_HpP   = 0.0
             MHD_OpP   = 0.0
             MHD_Beq     = NoValue
          end where
       end if

    end if

    deallocate(RayIntegral_VII, RayResult_VII)

    ! If kP is being calculated, share it.  Otherwise, share -1.
    if(DoCalcKp) then
       KpOut = kP
    else
       KpOut = -1
    endif

    if (iProc == 0) then
       ! Output before processing
       if(DoTestTec)call write_integrated_data_tec
       if(DoTestIdl)call write_integrated_data_idl

       ! Convert the units of the MHD_variables to SI units!!
       call process_integrated_data

       ! Output after processing
       if(DoTestTec)call write_integrated_data_tec
       if(DoTestIdl)call write_integrated_data_idl

       ! Put results into output buffer
       Buffer_IIV(:,:,InvB_)    = MHD_SUM_vol
       Buffer_IIV(:,:,Z0x_)     = MHD_Xeq
       Buffer_IIV(:,:,Z0y_)     = MHD_Yeq
       Buffer_IIV(:,:,Z0b_)     = MHD_Beq

       if(.not.DoMultiFluidIMCoupling)then
          Buffer_IIV(:,:,RhoInvB_) = MHD_SUM_rho
          Buffer_IIV(:,:,pInvB_)   = MHD_SUM_p
       else
          ! the index is not continuous as in ModFieldTrace
          Buffer_IIV(:,:,HpRhoInvB_) = MHD_HpRho
          Buffer_IIV(:,:,OpRhoInvB_) = MHD_Oprho
          Buffer_IIV(:,:,HpPInvB_)   = MHD_HpP
          Buffer_IIV(:,:,OpPInvB_)   = MHD_OpP
       end if

    end if

  end subroutine GM_get_for_im
  !============================================================================

  subroutine GM_satinit_for_im(nSats)

    ! This subroutine collects the number of satellite files for use in
    ! SWMF GM and iM coupling.

    ! Module variables to use:
    use ModMain,   ONLY: DoImSatTrace
    use ModSatelliteFile, ONLY: nSatellite

    ! Subroutine Arguments:
    integer,           intent(out) :: nSats
    !--------------------------------------------------------------------------

    ! If iM sat tracing is on, collect the number of satellites to trace.
    ! If iM sat tracing is off, set nSats to zero.
    if (DoImSatTrace) then
       nSats = nSatellite
    else
       nSats = 0
    endif

  end subroutine GM_satinit_for_im
  !============================================================================

  subroutine GM_get_sat_for_im(Buffer_III, Name_I, nSats)

    ! Subroutine to update and collect satellite locations for iM tracing

    ! Modules
    use ModSatelliteFile, ONLY: NameSat_I, XyzSat_DI, &
         get_satellite_ray, set_satellite_flags
    use ModMPI

    ! Arguments
    integer, intent(in)               :: nSats
    real, intent(out)                 :: Buffer_III(3,2,nSats)
    character (len=100), intent(out)  :: Name_I(nSats)

    ! Internal variables

    real :: sat_RayVars(5), sat_RayVarsSum(5)

    integer :: iSat, iError
    character(len=*), parameter:: NameSub = 'GM_get_sat_for_im'
    !--------------------------------------------------------------------------
    ! Store satellite names in Buffer_I
    Name_I = NameSat_I(1:nSats)

    do iSat = 1, nSats
       ! Update satellite position.
       call set_satellite_flags(iSat)
       call get_satellite_ray(iSat, sat_RayVars)

       ! Reduce values from all
       call MPI_reduce(sat_RayVars, sat_RayVarsSum, 5, MPI_REAL, MPI_SUM, &
            0, iComm, iError)

       ! Store results in Buffer_III
       if (iProc == 0) then
          Buffer_III(:,1,iSat) = XyzSat_DI(:,iSat)
          Buffer_III(:,2,iSat) = sat_RayVarsSum(1:3)
       end if
    end do

  end subroutine GM_get_sat_for_im
  !============================================================================

  subroutine GM_put_from_im(Buffer_IIV,iSizeIn,jSizeIn,nVar,NameVar)

    use CON_coupler
    use CON_world,      ONLY: get_comp_info
    use CON_comp_param, ONLY: lNameVersion
    use ModImCoupling                              ! Storage for iM pressure
    use ModMain, ONLY : nStep,tSimulation
    use ModIoUnit, ONLY: UNITTMP_
    use ModFieldTrace, ONLY: UseAccurateTrace, DoMapEquatorRay
    use ModVarIndexes, ONLY: nFluid, iRho_I, iP_I, iPparIon_I,SpeciesFirst_, &
         SpeciesLast_
    use ModAdvance,    ONLY: UseMultiSpecies, nSpecies

    character(len=80):: NameFile

    integer, intent(in) :: iSizeIn,jSizeIn,nVar
    real, intent(in) :: Buffer_IIV(iSizeIn,jSizeIn,nVar)
    character(len=*), intent(in) :: NameVar
    character(len=lNameVersion) :: NameVersionIm
    integer :: nCells_D(2), iError, i,j
    integer, parameter :: pres_=1, dens_=2, parpres_=3, bmin_=4, &
         Hpres_=3,Opres_=4,Hdens_=5,Odens_=6

    logical :: DoTest, DoTestMe

    character(len=*), parameter:: NameSub = 'GM_put_from_im'
    !--------------------------------------------------------------------------
    call CON_set_do_test(NameSub, DoTest, DoTestMe)

    if(DoMultiFluidIMCoupling)then
       if(NameVar /= 'p:rho:Hpp:Opp:Hprho:Oprho') &
            call CON_stop(NameSub//' invalid NameVar='//NameVar)
    else if(DoAnisoPressureIMCoupling)then
       if(NameVar /= 'p:rho:ppar:bmin') &
            call CON_stop(NameSub//' invalid NameVar='//NameVar)
    else
       if(NameVar /= 'p:rho') &
            call CON_stop(NameSub//' invalid NameVar='//NameVar)
    end if

    nCells_D=ncell_id(IM_)
    if( iSizeIn /= nCells_D(1) .or. jSizeIn /= nCells_D(2) ) then

       write(*,*)NameSub//' grid sizes do not agree iSize,jSize,nCells=',&
            iSizeIn,jSizeIn,nCells_D(1:2)
       call CON_stop(NameSub//' SWMF_ERROR')
    end if

    if(.not.allocated(ImLat_I))then
       ! Allocate ImLat_I, ImLon_I, IM_p, IM_dens
       call im_pressure_init(iSizeIn, jSizeIn)
       ! Set up iM ionospheric grid and store.
       ! Latitude specification is module specific, we must set it up
       ! according to the iM module we have selected.
       ! Determine version of iM:
       call get_comp_info(IM_, NameVersion=NameVersionIm)
       if(NameVersionIm(1:3) == 'RAM')then
          ! HEIDI and RAM-SCB have similar equatorial grids.
          ImLat_I = Grid_C(IM_) % Coord1_I

          ! Coupling with RAM/HEIDI grid requires accurate raytrace
          ! that stops at the equator
          UseAccurateTrace= .true.
          DoMapEquatorRay = .true.
       else
          ! RCM uses a CoLat based grid, information is stored in
          ! module grid information.
          ImLat_I = (cHalfPi - Grid_C(IM_) % Coord1_I) * cRadToDeg
       end if
       ImLon_I = Grid_C(IM_)% Coord2_I * cRadToDeg
    end if

    ! initialize
    IsImRho_I(:)  = .false.
    IsImP_I(:)    = .false.
    IsImPpar_I(:) = .false.

    ! Store iM variable for internal use
    ImP_III     (:,:,1) = Buffer_IIV(:,:,pres_)
 !   IM_p    = Buffer_IIV(:,:,pres_)

    ImRho_III     (:,:,1) = Buffer_IIV(:,:,dens_)
!    IM_dens = Buffer_IIV(:,:,dens_)
    iNewPIm  = iNewPIm + 1

    IsImRho_I(1)  = .true.
    IsImP_I(1)    = .true.
    IsImPpar_I(1) = .false.

    ! for multifluid
    if(DoMultiFluidIMCoupling)then
       if (UseMultiSpecies) then
          ImRho_III(:,:,2)= Buffer_IIV(:,:,Hdens_)
          !       IM_Hpdens = Buffer_IIV(:,:,Hdens_)
          ImRho_III(:,:,3)= Buffer_IIV(:,:,Odens_)
          !       IM_Opdens = Buffer_IIV(:,:,Odens_)
          IsImRho_I(:)  = .true.
          ! IsImRho_I(1)  = .false.
       else
          ImP_III(:,:,1)= Buffer_IIV(:,:,Hpres_)
          !       IM_Hpp = Buffer_IIV(:,:,Hpres_)
          ImP_III(:,:,2)= Buffer_IIV(:,:,Opres_)
          !       IM_Opp = Buffer_IIV(:,:,Opres_)
          ImRho_III(:,:,1)= Buffer_IIV(:,:,Hdens_)
          !       IM_Hpdens = Buffer_IIV(:,:,Hdens_)
          ImRho_III(:,:,2)= Buffer_IIV(:,:,Odens_)
          !       IM_Opdens = Buffer_IIV(:,:,Odens_)
          IsImRho_I(:)  = .true.
          IsImP_I(:)    = .true.
          IsImPpar_I(:) = .false.
       endif
    endif

    ! for anisotropic pressure
    if(DoAnisoPressureIMCoupling)then
       ImPpar_III(:,:,1)= Buffer_IIV(:,:,Opres_)
!       IM_ppar = Buffer_IIV(:,:,parpres_)
       ImBmin_II = Buffer_IIV(:,:,bmin_)
       IsImPpar_I(1) = .true.
    end if

    !$acc update device(ImLat_I, ImLon_I)
    !$acc update device(ImP_III, ImRho_III, ImPpar_III)
    !$acc update device(ImBmin_II)
    !$acc update device(IsImRho_I, IsImP_I, IsImPpar_I)

!    if(DoTest)call write_IMvars_tec  ! TecPlot output
!    if(DoTest)call write_IMvars_idl  ! IDL     output

  contains
    !==========================================================================

!    !============================================================================
!    subroutine write_IMvars_tec
!      integer :: j2
!      real :: lonShift
!      !-------------------------------------------------------------------------
!      if(iProc /= 0)RETURN
!
!      ! write values to plot file
!      write(NameFile,'(a,i6.6,a)')"IMp_n=",nStep,".dat"
!      OPEN (UNIT=UNITTMP_, FILE=NameFile, STATUS='unknown')
!      write(UNITTMP_,'(a)') 'TITLE="Raytrace Values"'
!      if(DoMultiFluidIMCoupling)then
!         write(UNITTMP_,'(a)') 'VARIABLES="J", "I", "Lon", "Lat",&
!              &"iM pressure", "iM density", &
!              &"iM Hp pressure", "iM Hp density", &
!              &"iM Op pressure", "iM Op density"'
!      else
!         write(UNITTMP_,'(a)') 'VARIABLES="J", "I", "Lon", "Lat",&
!              &"iM pressure", "iM density"'
!      end if
!      write(UNITTMP_,'(a,i4,a,i4,a)') &
!           'ZONE T="iM Pressure", I=',jSizeIn+1,', J=',iSizeIn,', K=1, F=POINT'
!      do i=1,iSizeIn
!         do j2=1,jSizeIn+1
!            j=j2; if(j2==jSizeIn+1) j=1
!            lonShift=0.; if(j2==jSizeIn+1) lonShift=360.
!            if(DoMultiFluidIMCoupling)then
!               write(UNITTMP_,'(2i4,8G14.6)') j2,i,ImLon_I(j)+lonShift,ImLat_I(i), &
!                    IM_p(i,j),IM_dens(i,j), &
!                    IM_Hpp(i,j),IM_Hpdens(i,j),IM_Opp(i,j), IM_Opdens(i,j)
!            else
!               write(UNITTMP_,'(2i4,4G14.6)') j2,i,ImLon_I(j)+lonShift,ImLat_I(i), &
!                    IM_p(i,j),IM_dens(i,j)
!            endif
!         end do
!      end do
!      CLOSE(UNITTMP_)
!
!    end subroutine write_IMvars_tec
!
!    !==========================================================================
!    subroutine write_IMvars_idl
!      if(iProc /= 0)RETURN
!
!      ! write values to plot file
!      write(NameFile,'(a,i6.6,a)')"IMp_n=",nStep,".out"
!      OPEN (UNIT=UNITTMP_, FILE=NameFile, STATUS='unknown', &
!           iostat =iError)
!      if (iError /= 0) call CON_stop("Can not open file "//NameFile)
!      write(UNITTMP_,'(a79)')            'iM pressure'
!      write(UNITTMP_,'(i7,1pe13.5,3i3)') nStep,tSimulation,2,0,2
!      write(UNITTMP_,'(3i4)')            jSizeIn,iSizeIn
!      if(DoMultiFluidIMCoupling)then
!         write(UNITTMP_,'(a79)')'Lon Lat p rho Hpp Hprho Opp Oprho'
!      else
!         write(UNITTMP_,'(a79)')'Lon Lat p rho'
!      endif
!      do i=iSizeIn,1,-1
!         do j=1,jSizeIn
!            if(DoMultiFluidIMCoupling)then
!               write(UNITTMP_,'(100(1pe18.10))') &
!                    ImLon_I(j),ImLat_I(i),IM_p(i,j),IM_dens(i,j), &
!                    IM_Hpp(i,j), IM_Hpdens(i,j), IM_Opp(i,j), IM_Opdens(i,j)
!            else
!               write(UNITTMP_,'(100(1pe18.10))') &
!                    ImLon_I(j),ImLat_I(i),IM_p(i,j),IM_dens(i,j)
!            endif
!         end do
!      end do
!      CLOSE(UNITTMP_)
!
!    end subroutine write_IMvars_idl

  end subroutine GM_put_from_im
  !============================================================================

  subroutine GM_put_from_im_cimi(Buffer_IIV,iSizeIn,jSizeIn,nVarIm,NameVarIm)

    use CON_coupler
    use CON_world,      ONLY: get_comp_info
    use CON_comp_param, ONLY: lNameVersion
    use ModImCoupling                              ! Storage for iM pressure
    use ModMain, ONLY : nStep,tSimulation
    use ModIoUnit, ONLY: UNITTMP_
    use BATL_lib, ONLY: iProc
    use ModFieldTrace, ONLY: UseAccurateTrace, DoMapEquatorRay
    use ModVarIndexes, ONLY: nFluid, iRho_I, iP_I, iPparIon_I,SpeciesFirst_, &
         SpeciesLast_,NameVar_V
    use ModAdvance,    ONLY: UseMultiSpecies, nSpecies
    use ModUtilities,       ONLY: split_string
    character(len=80):: NameFile

    integer, intent(in) :: iSizeIn,jSizeIn,nVarIm
    real, intent(in) :: Buffer_IIV(iSizeIn,jSizeIn,nVarIm)

    integer :: iFluid, iVarIm
    ! list of first nVarIm-1 variables provided by iM
    character(len=*), intent(in) :: NameVarIm

    character(len=lNameVersion) :: NameVersionIm
    integer :: nCells_D(2), iError, i,j

    integer,allocatable, save :: iRhoIm_I(:),  iPIm_I(:),  iPparIm_I(:)

    character (len=15),allocatable :: NameVarIm_V(:)

    integer, save :: nDensity
    integer, allocatable, save :: iDens_I(:)
    integer :: iDensity
    logical,save :: IsFirstCall = .true.
    logical :: DoTest, DoTestMe
    character(len=*), parameter:: NameSub = 'GM_put_from_im_cimi'
    !--------------------------------------------------------------------------

    call CON_set_do_test(NameSub, DoTest, DoTestMe)

    nCells_D=ncell_id(IM_)
    if( iSizeIn /= nCells_D(1) .or. jSizeIn /= nCells_D(2) ) then

       write(*,*)NameSub//' grid sizes do not agree iSize,jSize,nCells=',&
            iSizeIn,jSizeIn,nCells_D(1:2)
       call CON_stop(NameSub//' SWMF_ERROR')
    end if

    if(.not.allocated(ImLat_I))then
       ! Allocate ImLat_I, ImLon_I, IM_p, IM_dens
       call im_pressure_init(iSizeIn, jSizeIn)
       ! Set up iM ionospheric grid and store.
       ! Latitude specification is module specific, we must set it up
       ! according to the iM module we have selected.
       ! Determine version of iM:
       call get_comp_info(IM_, NameVersion=NameVersionIm)
       if(NameVersionIm(1:3) == 'RAM')then
          ! HEIDI and RAM-SCB have similar equatorial grids.
          ImLat_I = Grid_C(IM_) % Coord1_I

          ! Coupling with RAM/HEIDI grid requires accurate raytrace
          ! that stops at the equator
          UseAccurateTrace= .true.
          DoMapEquatorRay = .true.
       else
          ! RCM uses a CoLat based grid, information is stored in
          ! module grid information.
          ImLat_I = (cHalfPi - Grid_C(IM_) % Coord1_I) * cRadToDeg
       end if
       ImLon_I = Grid_C(IM_)% Coord2_I * cRadToDeg
    end if

    allocate(NameVarIm_V(nVarIm-1))
    call split_string(NameVarIm, NameVarIm_V)

    ! loop over GM fluid densities to find iM buffer variable location
    ! only on first call
    if (IsFirstCall) then
       ! Set array of density indexes:
       ! 3 values for multispecies, nIons for multifluid
       if(UseMultiSpecies)then
          nDensity = nSpecies
          allocate(iDens_I(nDensity))
          do iDensity = 1,nDensity
             iDens_I(iDensity) = SpeciesFirst_+iDensity-1
          end do
       else
          nDensity = nFluid
          allocate(iDens_I(nDensity))
          iDens_I = iRho_I(1:nDensity)
       end if

       ! allocate location bufffer
       allocate(iRhoIm_I(nDensity))
       allocate(iPIm_I(nFluid))
       allocate(iPparIm_I(nFluid))

       ! Initialize values assuming not present
       iRhoIm_I(:) = -1
       IsImRho_I(:)=.false.
       iPIm_I(:)=-1
       IsImP_I(:)=.false.
       iPparIm_I(:)=-1
       IsImPpar_I(:)=.false.

       do iDensity = 1, nDensity
          do iVarIm = 1,nVarIM-1
             ! get Density index for buffer
             if(trim(string_to_lower(NameVar_V(iDens_I(iDensity)))) &
                  == trim(string_to_lower(NameVarIm_V(iVarIm)))) then
                ! found a match. set iM fluid index
                iRhoIm_I(iDensity)  = iVarIm
                IsImRho_I(iDensity) = .true.
             elseif(iVarIm == nVarIm) then
                ! no match found.
                iRhoIm_I(iDensity)  = -1
                IsImRho_I(iDensity) = .false.
             endif
          enddo
       enddo

       do iFluid = 1, nFluid
          do iVarIm = 1,nVarIM-1
             ! get Pressure index for Buffer
             if(trim(string_to_lower(NameVar_V(iP_I(iFluid)))) &
                  == trim(string_to_lower(NameVarIm_V(iVarIm)))) then
                ! found a match. set iM fluid index
                iPIm_I(iFluid)  = iVarIm
                IsImP_I(iFluid) = .true.
             elseif(iVarIm == nVarIm) then
                ! no match found.
                iPIm_I(iFluid)  = -1
                IsImP_I(iFluid) = .false.
             endif

             if (DoAnisoPressureIMCoupling) then
                ! get Par Pressure index for Buffer
                if(trim(string_to_lower(NameVar_V(iPparIon_I(iFluid)))) &
                     == trim(string_to_lower(NameVarIm_V(iVarIm)))) then
                   ! found a match. set iM fluid index
                   iPparIm_I(iFluid)  = iVarIm
                   IsImPpar_I(iFluid) = .true.
                elseif(iVarIm == nVarIm) then
                   ! no match found.
                   iPparIm_I(iFluid)  = -1
                   IsImPpar_I(iFluid) = .false.
                endif
             else
                iPparIm_I(iFluid)  = -1
                IsImPpar_I(iFluid) = .false.
             endif
          enddo
       enddo
       IsFirstCall = .false.
    endif

    do iDensity=1,nDensity
       if (IsImRho_I(iDensity)) then
          ImRho_III     (:,:,iDensity) = Buffer_IIV(:,:,iRhoIm_I(iDensity))
       else
          ImRho_III     (:,:,iDensity) = -1.0
       endif
    enddo

    do iFluid=1,nFluid
       if (IsImP_I(iFluid)) then
          ImP_III     (:,:,iFluid) = Buffer_IIV(:,:,iPIm_I(iFluid))
          iNewPIm  = iNewPIm + 1
       else
          ImP_III     (:,:,iFluid) = -1.0
       endif

       if (IsImPpar_I(iFluid) .and. DoAnisoPressureIMCoupling) then
          ImPpar_III     (:,:,iFluid) = Buffer_IIV(:,:,iPparIm_I(iFluid))
       else
          ImPpar_III     (:,:,iFluid) = -1.0
       endif

    enddo
    ! for anisotropic pressure
    if(DoAnisoPressureIMCoupling)then
       ImBmin_II = Buffer_IIV(:,:,nVarIm)
    end if
!    write(*,*) 'GM max min ppar',maxval(ImPpar_III),minval(ImPpar_III)
!    write(*,*) 'GM max min p',maxval(ImP_III),minval(ImP_III)

    if(DoTest)call write_IMvars_tec  ! TecPlot output
    if(DoTest)call write_IMvars_idl  ! IDL     output

  contains
    !==========================================================================
    function string_to_lower( string ) result (new)
      character(len=*)           :: string

      character(len=len(string)) :: new

      integer                    :: i
      integer                    :: k
      integer                    :: length
      !------------------------------------------------------------------------
      length = len(string)
      new    = string
      do i = 1,len(string)
         k = iachar(string(i:i))
         if ( k >= iachar('A') .and. k <= iachar('Z') ) then
            k = k + iachar('a') - iachar('A')
            new(i:i) = achar(k)
         endif
      enddo
    end function string_to_lower
    !==========================================================================

    subroutine write_IMvars_tec
      integer :: j2
      real :: lonShift
      !------------------------------------------------------------------------
      if(iProc /= 0)RETURN

      ! write values to plot file
      write(NameFile,'(a,i6.6,a)')"IMp_n=",nStep,".dat"
      OPEN (UNIT=UNITTMP_, FILE=NameFile, STATUS='unknown')
      write(UNITTMP_,'(a)') 'TITLE="Raytrace Values"'
      if(DoMultiFluidIMCoupling)then
         write(UNITTMP_,'(a)') 'VARIABLES="J", "I", "Lon", "Lat",&
              &"iM pressure", "iM density", &
              &"iM Hp pressure", "iM Hp density", &
              &"iM Op pressure", "iM Op density"'
      else
         write(UNITTMP_,'(a)') 'VARIABLES="J", "I", "Lon", "Lat",&
              &"iM pressure", "iM density"'
      end if
      write(UNITTMP_,'(a,i4,a,i4,a)') &
           'ZONE T="iM Pressure", I=',jSizeIn+1,', J=',iSizeIn,', K=1, F=POINT'
      do i=1,iSizeIn
         do j2=1,jSizeIn+1
            j=j2; if(j2==jSizeIn+1) j=1
            lonShift=0.; if(j2==jSizeIn+1) lonShift=360.
            if(DoMultiFluidIMCoupling)then
               write(UNITTMP_,'(2i4,8G14.6)') j2,i,ImLon_I(j)+lonShift,ImLat_I(i), &
                    ImP_III(i,j,1),ImRho_III(i,j,1), &
                    ImP_III(i,j,2),ImRho_III(i,j,2),ImP_III(i,j,3), ImRho_III(i,j,3)
            else
               write(UNITTMP_,'(2i4,4G14.6)') j2,i,ImLon_I(j)+lonShift,ImLat_I(i), &
                    ImP_III(i,j,1),ImRho_III(i,j,1)
            endif
         end do
      end do
      CLOSE(UNITTMP_)

    end subroutine write_IMvars_tec
    !==========================================================================

    subroutine write_IMvars_idl
      !------------------------------------------------------------------------
      if(iProc /= 0)RETURN

      ! write values to plot file
      write(NameFile,'(a,i6.6,a)')"IMp_n=",nStep,".out"
      OPEN (UNIT=UNITTMP_, FILE=NameFile, STATUS='unknown', &
           iostat =iError)
      if (iError /= 0) call CON_stop("Can not open file "//NameFile)
      write(UNITTMP_,'(a79)')            'iM pressure'
      write(UNITTMP_,'(i7,1pe13.5,3i3)') nStep,tSimulation,2,0,2
      write(UNITTMP_,'(3i4)')            jSizeIn,iSizeIn
      if(DoMultiFluidIMCoupling)then
         write(UNITTMP_,'(a79)')'Lon Lat p rho Hpp Hprho Opp Oprho'
      else
         write(UNITTMP_,'(a79)')'Lon Lat p rho'
      endif
      do i=iSizeIn,1,-1
         do j=1,jSizeIn
            if(DoMultiFluidIMCoupling)then
               write(UNITTMP_,'(100(1pe18.10))') &
                    ImLon_I(j),ImLat_I(i),ImP_III(i,j,1),ImRho_III(i,j,1), &
                    ImP_III(i,j,2), ImRho_III(i,j,2),ImP_III(i,j,3),ImRho_III(i,j,3)
            else
               write(UNITTMP_,'(100(1pe18.10))') &
                    ImLon_I(j),ImLat_I(i),ImP_III(i,j,1),ImRho_III(i,j,1)
            endif
         end do
      end do
      CLOSE(UNITTMP_)

    end subroutine write_IMvars_idl
    !==========================================================================

  end subroutine GM_put_from_im_cimi
  !============================================================================

  subroutine allocate_gm_im(iSizeIn,jSizeIn)
    use CON_comp_param, ONLY: IM_

    integer, intent(in) :: iSizeIn, jSizeIn

    character(len=*), parameter:: NameSub = 'allocate_gm_im'
    !--------------------------------------------------------------------------
    if(allocated(MHD_lat_boundary)) deallocate(MHD_lat_boundary)
    if(allocated(MHD_SUM_vol))      deallocate(MHD_SUM_vol)
    if(allocated(MHD_tmp))          deallocate(MHD_tmp)
    if(allocated(MHD_Beq))          deallocate(MHD_Beq)
    if(allocated(MHD_Xeq))          deallocate(MHD_Xeq)
    if(allocated(MHD_Yeq))          deallocate(MHD_Yeq)
    if(allocated(MHD_Fluxerror))    deallocate(MHD_Fluxerror)

    if(DoMultiFluidIMCoupling)then
       if(allocated(MHD_HpRho))        deallocate(MHD_HpRho)
       if(allocated(MHD_HpP))          deallocate(MHD_HpP)
       if(allocated(MHD_OpRho))        deallocate(MHD_OpRho)
       if(allocated(MHD_OpP))          deallocate(MHD_OpP)
    else
       if(allocated(MHD_SUM_rho))      deallocate(MHD_SUM_rho)
       if(allocated(MHD_SUM_p))        deallocate(MHD_SUM_p)
    end if

    iSize = iSizeIn
    jSize = jSizeIn

    if(.not.allocated(ImLat_I))then
       nCells_D=ncell_id(IM_)
       if(  iSize /= nCells_D(1) .or. &
            jSize /= nCells_D(2) ) then
          write(*,*)NameSub//' grid sizes do not agree iSize,jSize,nCells=',&
               iSize,jSize, nCells_D(1:2)
          call CON_stop(NameSub//' ERROR')
       end if
       allocate(ImLat_I(iSize), ImLon_I(jSize))
       ! Convert colat, lon to lat-lon in degrees
       ImLat_I = 90.0 - Grid_C(IM_) % Coord1_I * cRadToDeg
       ImLon_I =        Grid_C(IM_) % Coord2_I * cRadToDeg
    end if

    ! Arrays needed for the field line Integral_I
    allocate( MHD_SUM_vol(isize,jsize))
    MHD_SUM_vol = 0.

    if(DoMultiFluidIMCoupling)then
       allocate( MHD_Hprho(isize,jsize), MHD_Oprho(isize,jsize), &
            MHD_Hpp(isize,jsize), MHD_Opp(isize,jsize))
       MHD_Hprho = 0.
       MHD_Oprho = 0.
       MHD_Hpp = 0.
       MHD_Opp = 0.
    else
       allocate(MHD_SUM_rho(isize,jsize), MHD_SUM_p(isize,jsize))
       MHD_SUM_rho = 0.
       MHD_SUM_p = 0.
    end if

    allocate( MHD_Beq(isize,jsize))
    MHD_Beq = 0.

    allocate( MHD_Xeq(isize,jsize))
    MHD_Xeq = 0.

    allocate( MHD_Yeq(isize,jsize))
    MHD_Yeq = 0.

    allocate( MHD_tmp(isize,jsize))
    MHD_tmp = 0.

    allocate( MHD_Fluxerror(isize,jsize))
    MHD_Fluxerror = 0.

    allocate( MHD_lat_boundary(jsize))
    MHD_lat_boundary = 0

  end subroutine allocate_gm_im
  !============================================================================

  subroutine write_integrated_data_tec
    use ModIoUnit, ONLY: UNITTMP_
    CHARACTER (LEN=80) :: NameFile
    integer :: j2, nCall=0
    real :: tmpT, tmpV1,tmpV2, lonShift,tmpHpT,tmpOpT
    !--------------------------------------------------------------------------

    nCall=nCall+1

    ! write values to plot file
    write(NameFile,'(a,i6.6,a,i4.4,a)')"rayValues_n=",nStep,"_",nCall,".dat"

    OPEN (UNIT=UNITTMP_, FILE=NameFile, STATUS='unknown')
    write(UNITTMP_,'(a)') 'TITLE="Raytrace Values"'
    if(DoMultiFluidIMCoupling)then
       write(UNITTMP_,'(a)') 'VARIABLES="J", "I", "Lon", "Lat", "Lat Boundary (I)"', &
            ', "Xeq", "Yeq"', &
            ', "Volume", "Volume**(-2/3)"', &
            ', "MHD `r", "MHD p", "MHD T"', &
            ', "MHD Hp`r", "MHD Hp p", "MHD Hp T"', &
            ', "MHD Op`r", "MHD Op p", "MHD Op T", "Beq"', &
            ', "FluxError"'
    else
       write(UNITTMP_,'(a)') 'VARIABLES="J", "I", "Lon", "Lat", "Lat Boundary (I)"', &
            ', "Xeq", "Yeq"', &
            ', "Volume", "Volume**(-2/3)"', &
            ', "MHD `r", "MHD p", "MHD T", "Beq"', &
            ', "FluxError"'
    end if
    write(UNITTMP_,'(a,i3.3,a,i4,a,i4,a)') &
         'ZONE T="PE=',iProc,'", I=',jsize+1,', J=',isize,', K=1, F=POINT'
    do i=1,isize
       do j2=1,jsize+1
          j=j2; if(j2==jsize+1) j=1
          lonShift=0.; if(j2==jsize+1) lonShift=360.
          if(DoMultiFluidIMCoupling)then
             tmpHpT=-1.; if(MHD_Hprho(i,j)>0.) &
                  tmpHpT = ((MHD_Hpp(i,j)*Si2No_V(UnitP_))/(MHD_Hprho(i,j)*Si2No_V(UnitRho_))) &
                  * No2Si_V(UnitTemperature_)
             tmpOpT=-1.; if(MHD_Oprho(i,j)>0.) &
                  tmpOpT = ((MHD_Opp(i,j)*Si2No_V(UnitP_))/(MHD_Oprho(i,j)*Si2No_V(UnitRho_))) &
                  * No2Si_V(UnitTemperature_)
          else
             tmpT=-1.; if(MHD_SUM_rho(i,j)>0.) &
                  tmpT = ((MHD_SUM_p(i,j)*Si2No_V(UnitP_))/(MHD_SUM_rho(i,j)*Si2No_V(UnitRho_))) &
                  * No2Si_V(UnitTemperature_)
          end if
          tmpV1=0.; if(MHD_SUM_vol(i,j)>0.) &
               tmpV1 = (MHD_SUM_vol(i,j)/1.e9)
          tmpV2=0.; if(MHD_SUM_vol(i,j)>0.) &
               tmpV2 = (MHD_SUM_vol(i,j)/1.e9)**(-2./3.)
          if(DoMultiFluidIMCoupling)then
             write(UNITTMP_,'(2i4,18G14.6)') j2,i,ImLon_I(j)+lonShift,ImLat_I(i),&
                  MHD_lat_boundary(j), &
                  MHD_Xeq(i,j),MHD_Yeq(i,j), &
                  tmpV1,tmpV2, &
                  MHD_Hprho(i,j), MHD_Hpp(i,j), tmpHpT, &
                  MHD_Oprho(i,j), MHD_Opp(i,j), tmpOpT,MHD_Beq(i,j), &
                  MHD_Fluxerror(i,j)
          else
             write(UNITTMP_,'(2i4,12G14.6)') j2,i,ImLon_I(j)+lonShift,ImLat_I(i),&
                  MHD_lat_boundary(j), &
                  MHD_Xeq(i,j),MHD_Yeq(i,j), &
                  tmpV1,tmpV2, &
                  MHD_SUM_rho(i,j),MHD_SUM_p(i,j),tmpT,MHD_Beq(i,j), &
                  MHD_Fluxerror(i,j)
          end if
       end do
    end do
    CLOSE(UNITTMP_)

  end subroutine write_integrated_data_tec
  !============================================================================

  subroutine write_integrated_data_idl

    use ModIoUnit, ONLY: UNITTMP_
    use ModMain,   ONLY: tSimulation
    CHARACTER (LEN=100) :: NameFile
    integer :: nCall = 0
    !--------------------------------------------------------------------------

    ! write values to plot file
    nCall = nCall+1
    write(NameFile,'(a,i6.6,a,i4.4,a)')"rayValues_n=",nStep,"_",nCall,".out"

    OPEN (UNIT=UNITTMP_, FILE=NameFile, STATUS='unknown', &
         iostat =iError)
    if (iError /= 0) call CON_stop("Can not open raytrace File "//NameFile)
    write(UNITTMP_,'(a79)')            'Raytrace Values_var22'
    write(UNITTMP_,'(i7,1pe13.5,3i3)') nStep,tSimulation,2,1,7
    write(UNITTMP_,'(3i4)')            jSize+1,iSize
    write(UNITTMP_,'(100(1pe13.5))')   0.0
    write(UNITTMP_,'(a79)') 'Lon Lat Xeq Yeq vol rho p Beq FluxError nothing'
    do i=isize,1,-1
       do j=1,jsize
          write(UNITTMP_,'(100(1pe18.10))') &
               ImLon_I(j),       &
               ImLat_I(i),       &
               MHD_Xeq(i,j),     &
               MHD_Yeq(i,j),     &
               MHD_SUM_vol(i,j), &
               MHD_SUM_rho(i,j), &
               MHD_SUM_p(i,j),   &
               MHD_Beq(i,j),     &
               MHD_FluxError(i,j)
       end do
       write(UNITTMP_,'(100(1pe18.10))') &
            ImLon_I(1)+360.0, &
            ImLat_I(i),       &
            MHD_Xeq(i,1),     &
            MHD_Yeq(i,1),     &
            MHD_SUM_vol(i,1), &
            MHD_SUM_rho(i,1), &
            MHD_SUM_p(i,1),   &
            MHD_Beq(i,1),     &
            MHD_FluxError(i,1)
    end do
    CLOSE(UNITTMP_)

  end subroutine write_integrated_data_idl
  !============================================================================

  subroutine process_integrated_data

    integer :: iLoc_I(1),iEquator,iNorthPole

    !--------------------------------------------------------------------------
    if(DoMultiFluidIMCoupling)then
       where(MHD_SUM_vol>0.)
          MHD_Hprho = MHD_Hprho/MHD_SUM_vol
          MHD_Oprho = MHD_Oprho/MHD_SUM_vol
          MHD_Hpp   = MHD_Hpp/MHD_SUM_vol
          MHD_Opp   = MHD_Opp/MHD_SUM_vol
       end where
    else
       where(MHD_SUM_vol>0.)
          MHD_SUM_p   = MHD_SUM_p/MHD_SUM_vol
          MHD_SUM_rho = MHD_SUM_rho/MHD_SUM_vol
       end where
    end if

    ! Set volume floor
    MHD_SUM_vol = max(1.E-8,MHD_SUM_vol)

    ! If the field-line tracer returned a good value, we may need to
    ! add contribution to V from the part of the field line that
    ! goes inside the body. If the field-line tracer did not
    ! return a good value, we will compute total V assuming dipole.
    Ri=(6378.+100.)/6378.
    Factor = 2. * (Ri**4) / abs(Bdp)
    do i=1,isize
       Colat = (90.0 - ImLat_I(i))*cDegToRad
       s2    = sin(colat)**2
       s6    = s2**3
       s8    = s6*s2

       Ci=abs(cos(colat))

       if( s2 < Ri/Rbody )then
          ! Fieldline goes beyond Rbody, add piece of fieldline volume
          Cs=sqrt(1.-(Rbody/Ri)*s2)
          FCiCs = (Ci-Cs) - (Ci**3-Cs**3) + (3./5.)*(Ci**5-Cs**5) &
               - (1./7.)*(Ci**7-Cs**7)
          if (s8 /= 0.0) then
             Vol = Factor*FCiCs/s8
          else
             Vol = 0.0
          endif
          where(MHD_SUM_vol(i,:)>1.1E-8)
             MHD_SUM_vol(i,:)=MHD_SUM_vol(i,:) + Vol
          end where
       end if

       if( s2 > Ri/Rcurrents )then
          ! Fieldline stays inside of Rcurrents, recompute some values

          ! Compute the full analytic volume
          FCiCs = Ci - Ci**3 + (3./5.)*Ci**5 - (1./7.)*Ci**7
          if (s8 /= 0.0) then
             Vol = factor*FCiCs/s8
          else
             Vol = 0.0
          endif

          ! Compute equatorial B value for dipole at this latitude
          eqB = abs(Bdp)*s6/Ri**3

          if( s2 > Ri/Rbody )then
             ! Fieldline stays inside of Rbody

             ! Recompute exact volume
             MHD_SUM_vol(i,:)=Vol

             ! Fix the grid inside Rbody
             MHD_Xeq(i,:) = (Ri/s2)*cos(ImLon_I(:)*cDegToRad)
             MHD_Yeq(i,:) = (Ri/s2)*sin(ImLon_I(:)*cDegToRad)

             ! Fix the equatorial B value
             MHD_Beq(i,:) = eqB

          else
             ! Fieldline stays inside of Rcurrents but outside Rbody
             ! Weight analytic formula proportional to the distance
             ! of the field line from rBody within the rBody-rCurrents range

             Factor1= (Ri/Rbody - s2)/ (Ri/Rbody - Ri/Rcurrents)
             Factor2= 1.0 - Factor1

             ! Check if numerical volume exists
             where(MHD_SUM_vol(i,:)>1.1E-8)
                ! Blend numerical volume with exact volume
                MHD_SUM_vol(i,:) = Factor1*MHD_SUM_vol(i,:) + Factor2*Vol
             end where

          end if
       end if
    end do

    ! find index for latitude closest to equator
    iLoc_I   = minloc(abs(ImLat_I))
    iEquator = iLoc_I(1)

    ! find index for latitude closest to north pole
    iLoc_I = maxloc(ImLat_I)
    iNorthPole = iLoc_I(1)

    iEquator = iEquator + sign(1, iNorthPole - iEquator)
    ! set open fieldline values
    do j=1,jsize

       ! Initialize the index of the last closed field line to be at
       ! the highest latitude in the RCM grid
       i0 = iNorthPole

       do i = iEquator, iNorthPole, sign(1, iNorthPole - iEquator)

          if(MHD_SUM_vol(i,j) < 1.1E-8 .or. &
               abs(MHD_Xeq(i,j)) > 200.0 .or. &
               abs(MHD_Yeq(i,j)) > 200.0 ) then
             i0=i+1
             EXIT
          end if
       end do

       ! Save the index of the "last" closed field line into MHD_lat_boundary
       MHD_lat_boundary(j)=i0
       ! write(*,*) "finding closed field-line ",j,i0,MHD_lat_boundary(j)

    end do

    ! Set impossible values for open fieldlines
    ! except for Xeq and Yeq where the last closed values are used
    ! which is useful when the equatorial grid is plotted
    do j=1,jsize
       i = int(MHD_lat_boundary(j))
       MHD_Beq    (1:i-1,j) = -1.
       MHD_Xeq    (1:i-1,j) = MHD_Xeq(i,j)
       MHD_Yeq    (1:i-1,j) = MHD_Yeq(i,j)
       MHD_SUM_vol(1:i-1,j) = -1.
       if(DoMultiFluidIMCoupling)then
          MHD_HpRho(1:i-1,j) = -1.
          MHD_OpRho(1:i-1,j) = -1.
          MHD_HpP  (1:i-1,j) = -1.
          MHD_OpP  (1:i-1,j) = -1.
       else
          MHD_SUM_rho(1:i-1,j) = -1.
          MHD_SUM_p  (1:i-1,j) = -1.
       endif
    end do

    ! Dimensionalize values
    ! Note: dimensions of "MHD_sum_vol" is Distance/Magnetic field.
    ! The distance unit is planetary radius in GM, and the same is
    ! used in RCM, so only the magnetic unit is converted to SI units.
    ! Similarly Xeq and Yeq (equatorial crossing coords) remain in
    ! normalized units.
    if(DoMultiFluidIMCoupling)then
       where(MHD_SUM_vol > 0.)
          MHD_SUM_vol = MHD_SUM_vol / No2Si_V(UnitB_)
          MHD_HpRho = MHD_HpRho * No2Si_V(UnitRho_)
          MHD_OpRho = MHD_OpRho * No2Si_V(UnitRho_)
          MHD_HpP   = MHD_HpP * No2Si_V(UnitP_)
          MHD_OpP   = MHD_OpP * No2Si_V(UnitP_)
       elsewhere
          MHD_SUM_vol = -1.
          MHD_HpRho = -1.
          MHD_OpRho = -1.
          MHD_HpP   = -1.
          MHD_OpP   = -1.
       end where
    else
       where(MHD_SUM_vol > 0.)
          MHD_SUM_vol = MHD_SUM_vol / No2Si_V(UnitB_)
          MHD_SUM_rho = MHD_SUM_rho * No2Si_V(UnitRho_)
          MHD_SUM_p   = MHD_SUM_p   * No2Si_V(UnitP_)
       elsewhere
          MHD_SUM_vol = -1.
          MHD_SUM_rho = -1.
          MHD_SUM_p   = -1.
       end where
    end if

    where(MHD_Beq > 0.)
       MHD_Beq = MHD_Beq * No2Si_V(UnitB_)
    elsewhere
       MHD_Beq = -1.
    end where

    if(DoTestTec .or. DoTestIdl)then
       do j=1,jsize-1; do i=1,isize-1
          if ( MHD_SUM_vol(i  ,j) > 0.0 .AND. &
               MHD_SUM_vol(i+1,j) > 0.0 .AND. &
               MHD_SUM_vol(i,j+1) > 0.0 .AND. &
               MHD_SUM_vol(i+1,j+1)> 0.0) THEN
             MHD_Fluxerror(i,j) = &
                  0.25E+9*(MHD_Beq(i,j  ) + MHD_Beq(i+1,j  ) + &
                  MHD_Beq(i,j+1) + MHD_Beq(i+1,j+1)) * &
                  0.5*(ABS((MHD_Xeq(i,j+1)-MHD_Xeq(i,j))* &
                  (MHD_Yeq(i+1,j)-MHD_Yeq(i,j))  &
                  - (MHD_Yeq(i,j+1)-MHD_Yeq(i,j)) &
                  * (MHD_Xeq(i+1,j)-MHD_Xeq(i,j))) &
                  + ABS((MHD_Xeq(i+1,j)-MHD_Xeq(i+1,j+1)) &
                  *(MHD_Yeq(i,j+1)-MHD_Yeq(i+1,j+1)) &
                  -(MHD_Yeq(i+1,j)-MHD_Yeq(i+1,j+1)) &
                  *(MHD_Xeq(i,j+1)-MHD_Xeq(i+1,j+1))))/&
                  (ABS(DipoleStrengthSi)*(SIN(ImLat_I(i)*cDegToRad)**2 &
                  -SIN(ImLat_I(i+1)*cDegToRad)**2)* &
                  (ImLon_I(j+1)-ImLon_I(j))*cDegToRad )- 1.0
          ELSE
             MHD_Fluxerror (i,j) = 0.0
          END IF
       end do; end do
       MHD_fluxerror(:,jsize) = MHD_Fluxerror (:,1)
       MHD_fluxerror(isize,:) = MHD_FLuxerror(isize-1,:)
    end if

  end subroutine process_integrated_data
  !============================================================================
  subroutine set_buffer_indexes(nVarCheck, NameSub)

    integer, optional, intent(in):: nVarCheck
    character(len=*), optional, intent(in):: NameSub

    ! indexes of registered components
    integer :: iGm, iIm
    !--------------------------------------------------------------------------
    ! Store buffer variable indexes
    iGm = lComp_I(GM_)
    iIm = lComp_I(IM_)
    nVarCouple = nVarBuffer_CC(iGm,iIm)
    if(present(nVarCheck))then
       if(nVarCheck /= nVarCouple)then
          if(iProc==0) call CON_stop(NameSub// &
               ' ERROR, invalid nVarCheck, nVarCouple=', nVarCheck, nVarCouple)
       end if
    end if

    ! Assume GM -> iM direction in CON_couple_gm_im.f90
    allocate(iVarCouple_V(nVarCouple))
    iVarCouple_V = iVarSource_VCC(1:nVarCouple,iGm,iIm)
  end subroutine set_buffer_indexes
  !============================================================================

end module GM_couple_im
!==============================================================================
