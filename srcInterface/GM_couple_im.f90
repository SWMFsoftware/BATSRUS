!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
!^CMP FILE IM

module GM_couple_im

  ! Coupling with inner magnetosphere models (IM)

  use BATL_lib, ONLY: iProc, iComm
  use ModUtilities, ONLY: CON_set_do_test
  use ModMpi
  use ModNumConst, ONLY: cRadToDeg, cDegToRad, cHalfPi
  use CON_coupler, ONLY: Grid_C, ncell_id, &
       nVarBuffer_CC, iVarSource_VCC, GM_, IM_, lComp_I
  use CON_planet, ONLY: RadiusPlanet

  use ModMain, ONLY: nStep, &
       DoMultiFluidIMCoupling, DoAnisoPressureIMCoupling
  use ModPhysics, ONLY: No2Si_V, Si2No_V, &
       UnitP_, UnitRho_, UnitTemperature_, UnitB_, &
       Bdp, DipoleStrengthSi, rCurrents, rBody
  use ModAdvance, ONLY: State_VGB
  use ModB0, ONLY: B0_DGB
  use ModUpdateStateFast, ONLY: sync_cpu_gpu
  use ModImCoupling, ONLY: nVarCouple, iVarCouple_V
  use ModUtilities, ONLY: CON_stop
#ifdef _OPENACC
  use ModUtilities, ONLY: norm2
#endif

  implicit none

  private ! except

  public:: GM_get_for_im_trace_crcm ! for IM/CRCM
  public:: GM_get_for_im_crcm       ! for IM/CRCM
  public:: GM_get_sat_for_im_crcm   ! for IM/CRCM
  public:: GM_get_for_im_trace      ! for IM/RAM
  public:: GM_get_for_im_line       ! for IM/RAM
  public:: GM_get_for_im            ! for IM/RCM
  public:: GM_satinit_for_im        ! initialize satellite
  public:: GM_get_sat_for_im        ! get satellite info
  public:: GM_put_from_im           ! from IM
  public:: GM_put_from_im_cimi      ! from IM/CIMI

  character(len=*), parameter:: NameMod='GM_couple_im'

  ! For some reason the RCM and CRCM use 100km for ionosphere height
  real, parameter:: IonosphereHeightIm = 100e3

  ! IM grid size
  integer:: nCells_D(2), iSize,jSize

  ! Information about the IM grid: 2D non-uniform regular grid
  real, allocatable:: ImLat_I(:), ImLon_I(:)

  integer:: i, j, i0

  real, save, allocatable:: MhdLatBoundary_I(:)
  real, save, dimension(:,:), allocatable:: &
       MhdSumVol_II, MhdTmp_II, &
       MhdSumRho_II, MhdHpRho_II, MhdOpRho_II, &
       MhdSumPe_II, MhdSumP_II, MhdHpP_II, MhdOpP_II, &
       MhdBeq_II, MhdXeq_II, MhdYeq_II, MhdFluxError_II
  real, parameter:: NoValue=-99999.
  real:: Beq
  real:: Colat, Ci, Cs, FCiCs, Factor, Vol, Ri, s2, s6, s8, Factor1, Factor2

  integer:: iError

  logical:: DoTestTec, DoTestIdl

  ! This is for the GM-IM/RAM coupling
  real, allocatable:: StateLine_VI(:,:)

contains
  !============================================================================
  ! FOR CRCM COUPLING
  subroutine GM_get_for_im_trace_crcm(iSizeIn, jSizeIn, nDensityIn, &
       NameVar, nVarLine, nPointLine)

    ! Do field tracing for IM.
    ! Provide total number of points along rays
    ! and the number of variables to pass to IM

    use ModFieldTrace, ONLY: DoExtractUnitSi, integrate_field_from_sphere
    use CON_line_extract, ONLY: line_get

    integer, intent(in)         :: iSizeIn, jSizeIn, nDensityIn
    character(len=*), intent(in):: NameVar
    integer, intent(out)        :: nVarLine, nPointLine
    real:: Radius

    ! Allocate arrays
    character(len=*), parameter:: NameSub = 'GM_get_for_im_trace_crcm'
    !--------------------------------------------------------------------------
    call allocate_gm_im(iSizeIn, jSizeIn)

    ! The CRCM ionosphere radius at 100km altitude in normalized units
    Radius = (RadiusPlanet + IonosphereHeightIm)/RadiusPlanet

    DoExtractUnitSi = .true.

    call integrate_field_from_sphere( &
         iSizeIn, jSizeIn, ImLat_I, ImLon_I, Radius, NameVar)

    DoExtractUnitSi = .false.

    call line_get(nVarLine, nPointLine)

    ! We only pass line index, length, B and radial distance to IM
    nVarLine = 4

  end subroutine GM_get_for_im_trace_crcm
  !============================================================================
  subroutine GM_get_for_im_crcm(Buffer_IIV, KpOut, AeOut, iSizeIn, jSizeIn, &
       nDensityIn, nVarIn, BufferLine_VI, nVarLine, nPointLine, &
       BufferSolarWind_V, NameVar)

    use ModGeometry, ONLY: xMaxBox
    use ModIoUnit, ONLY: UnitTmp_
    use ModMain, ONLY: tSimulation, TypeCoordSystem
    use ModVarIndexes, ONLY: &
         Rho_, RhoUx_, RhoUy_, RhoUz_, Bx_, By_, Bz_, p_, &
         MassFluid_I, nVar
    use ModPhysics, ONLY: No2Si_V, &
         UnitN_, UnitU_, UnitB_, UnitP_, rBody
    use ModSolarwind, ONLY: get_solar_wind_point
    use ModNumConst, ONLY: cRadToDeg
    use ModGroundMagPerturb, ONLY: DoCalcKp, Kp, DoCalcAe, AeIndex_I
    use CON_planet_field, ONLY: map_planet_field

    use CON_line_extract, ONLY: line_get, line_clean
    use CON_axes, ONLY: transform_matrix

    integer, intent(in) :: iSizeIn, jSizeIn, nDensityIn, nVarIn
    real,    intent(out):: Buffer_IIV(iSizeIn,jSizeIn,nVarIn), KpOut, AeOut
    integer, intent(in) :: nPointLine, nVarLine
    real,    intent(out):: BufferLine_VI(nVarLine,nPointLine)
    real,    intent(out):: BufferSolarWind_V(8)
    character(len=*), intent(in):: NameVar

    integer:: nVarExtract, nPoint, iPoint, iStartPoint
    real, allocatable:: Buffer_VI(:,:)
    integer:: iLat,iLon,iLine,iLocBmin
    real:: SmGm_DD(3,3), XyzBminSm_D(3), XyzEndSm_D(3), XyzEndSmIono_D(3)
    real:: SolarWind_V(nVar)
    character(len=100):: NameOut

    real:: RadiusIono
    integer:: iHemisphere
    logical:: DoTest, DoTestMe
    character(len=*), parameter:: NameSub = 'GM_get_for_im_crcm'
    !--------------------------------------------------------------------------
    if(iProc /= 0) RETURN
    call CON_set_do_test(NameSub, DoTest, DoTestMe)

    ! The CRCM ionosphere radius in normalized units
    RadiusIono = (RadiusPlanet + IonosphereHeightIm)/RadiusPlanet

    ! If Kp is being calculated, share it.  Otherwise, share -1.
    if(DoCalcKp) then
       KpOut = Kp
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
    if(.not.allocated(iVarCouple_V)) &
         call set_buffer_indexes(nVarIn - 20, NameSub)

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
       BufferLine_VI(2,iPoint) = Buffer_VI(1,iPoint)                  ! Length
       BufferLine_VI(3,iPoint) = norm2(Buffer_VI(2:4,iPoint))         ! |r|
       BufferLine_VI(4,iPoint) = norm2(Buffer_VI(4+Bx_:4+Bz_,iPoint)) ! |B|

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
             Buffer_IIV(iLat,iLon,4) = 90.0 - acos(XyzEndSmIono_D(3)&
                  /norm2(XyzEndSmIono_D))*cRadToDeg

             ! SmLon of conjugate point
             Buffer_IIV(iLat,iLon,5) = &
                  atan2(XyzEndSm_D(2),XyzEndSmIono_D(1))*cRadToDeg

             ! pass 5 xyz points around minB for curvature calc in cimi
             Buffer_IIV(iLat,iLon,6:8)= Buffer_VI(2:4,iLocBmin-2)
             Buffer_IIV(iLat,iLon,9:11)= Buffer_VI(2:4,iLocBmin-1)
             Buffer_IIV(iLat,iLon,12:14)= Buffer_VI(2:4,iLocBmin)
             Buffer_IIV(iLat,iLon,15:17)= Buffer_VI(2:4,iLocBmin+1)
             Buffer_IIV(iLat,iLon,18:20)= Buffer_VI(2:4,iLocBmin+2)

             ! Put coupled variables (densities and pressures) into buffer
             Buffer_IIV(iLat,iLon,21:) = Buffer_VI(4+iVarCouple_V,iLocBmin)

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

    BufferSolarWind_V(1) = SolarWind_V(Rho_)/MassFluid_I(1) * No2Si_V(UnitN_)
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
  subroutine GM_get_sat_for_im_crcm(Buffer_III, Name_I, nSat)

    ! Subroutine to update and collect satellite locations for IM tracing

    ! Modules
    use ModSatelliteFile, ONLY: NameFileSat_I, XyzSat_DI, gm_trace_sat
    use ModWriteLogSatFile, ONLY: collect_satellite_data
    use ModMain, ONLY: UseB0, nBlock
    use ModPhysics, ONLY: No2Si_V, UnitB_
    use ModVarIndexes, ONLY: Bx_, Bz_
    use ModB0, ONLY: get_b0
    use ModCurrent, ONLY: get_point_data
    use ModMPI

    ! Arguments
    integer, intent(in):: nSat
    real,   intent(out):: Buffer_III(4,2,nSat)
    character(len=100), intent(out):: Name_I(nSat)

    ! Internal variables
    real:: SatRay_D(3)
    real:: StateSat_V(0:3), B0Sat_D(3)
    integer:: iSat

    character(len=*), parameter:: NameSub = 'GM_get_sat_for_im_crcm'
    !--------------------------------------------------------------------------
    ! Provide satellite names so they can be known on all processors
    Name_I = NameFileSat_I(1:nSat)

    call sync_cpu_gpu("update on CPU", NameSub, State_VGB, B0_DGB)

    do iSat = 1, nSat
       ! Update satellite position
       call GM_trace_sat(XyzSat_DI(1:3,iSat), SatRay_D)

       call get_point_data( &
            0.0, XyzSat_DI(:,iSat), 1, nBlock, Bx_, Bz_, StateSat_V)

       call collect_satellite_data(XyzSat_DI(:,iSat), 3, StateSat_V)

       ! Store results in Buffer_III from processor zero
       if (iProc == 0) then
          Buffer_III(1:3,1,iSat) = XyzSat_DI(1:3,iSat)
          Buffer_III(1:3,2,iSat) = SatRay_D

          ! Determine total magnetic field squared at satellite: B2 = (B0+B1)^2
          B0Sat_D = 0.0
          if(UseB0) call get_b0(XyzSat_DI(:,iSat), B0Sat_D)
          Buffer_III(4,2,iSat)   = &
               sum( (StateSat_V(1:3) + B0Sat_D)**2 ) * No2Si_V(UnitB_)**2
       end if
    end do

  end subroutine GM_get_sat_for_im_crcm
  !============================================================================
  subroutine GM_get_for_im_trace(nRadius, nLon, nVarLine, nPointLine, NameVar)

    ! Do field line tracing for IM/RAM_SCB or IM/HEIDI.
    ! Provide total number of points along rays
    ! and the number of variables to pass to IM

    use ModMain, ONLY: tSimulation, TypeCoordSystem
    use ModVarIndexes, ONLY: Bx_, Bz_, nVar
    use ModIO, ONLY: NamePrimitiveVarOrig
    use CON_comp_param, ONLY: lNameVersion
    use CON_world, ONLY: get_comp_info
    use CON_line_extract, ONLY: line_get, line_clean
    use CON_coupler, ONLY: Grid_C, IM_
    use CON_axes, ONLY: transform_matrix
    use ModMultiFluid, ONLY: nFluid, iUx_I, iUz_I
    use ModFieldTrace, ONLY: DoExtractBGradB1, DoExtractEfield, &
         trace_field_equator
    use ModImCoupling, ONLY: IsImHeidi

    integer, intent(in)         :: nRadius, nLon
    integer, intent(out)        :: nVarLine, nPointLine
    character(len=*), intent(out):: NameVar

    real, allocatable, save:: RadiusIm_I(:), LongitudeIm_I(:)

    integer:: nVarExtract, iPoint, iUx5, iUz5, iFluid
    real:: SmGm_DD(3,3)
    ! Reversion matrix of X and Y, for HEIDI
    real, parameter:: ReverseSm_DD(3,3) = &
         reshape([-1, 0, 0, 0, -1, 0, 0, 0, 1], [3,3])

    character(len=lNameVersion):: NameVersionIm
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

    ! Trace field lines starting from IM equatorial grid (do message pass)
    ! Include BGradB1 only if needed:
    call get_comp_info(IM_, NameVersion=NameVersionIm)
    if(NameVersionIm(1:7) == 'RAM-SCB')then
       DoExtractBGradB1 = .false. ! RAM-SCB does not need BGradB1
       DoExtractEfield  = .false. ! RAM-SCB does not need Efield
    else
       IsImHeidi = .true.
       DoExtractBGradB1 = .true.  ! HEIDI needs BGradB1
       DoExtractEfield  = .true.  ! HEIDI needs Efield
    end if

    ! The variables to be passed: line index, length along line,
    ! coordinatess and primitive variables. Total is 5 + nVar.
    NameVar = 'iLine Length x y z '//NamePrimitiveVarOrig
    if(DoExtractBGradB1) NameVar = trim(NameVar)//' bgradb1x bgradb1y bgradb1z'
    if(DoExtractEfield)  NameVar = trim(NameVar)//' Ex Ey Ez Epotx Epoty Epotz'

    call sync_cpu_gpu("update on CPU", NameSub, State_VGB, B0_DGB)

    call trace_field_equator(nRadius, nLon, RadiusIm_I, LongitudeIm_I, .true.)

    if(iProc == 0)then

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

       if(IsImHeidi) SmGm_DD = matmul(ReverseSm_DD, SmGm_DD)

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
       end do ! iPoint
    end if ! iProc == 0

    DoExtractBGradB1 = .false.
    DoExtractEfield  = .false.

  end subroutine GM_get_for_im_trace
  !============================================================================
  subroutine GM_get_for_im_line(nRadius, nLon, MapOut_DSII, &
       nVarLine, nPointLine, BufferLine_VI)

    use ModFieldTrace, ONLY: RayMap_DSII

    integer, intent(in):: nRadius, nLon
    real,    intent(out):: MapOut_DSII(3,2,nRadius,nLon)
    integer, intent(in):: nPointLine, nVarLine
    real, intent(out) :: BufferLine_VI(nVarLine, nPointLine)

    logical:: DoTest, DoTestMe

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
  subroutine GM_get_for_im(Buffer_IIV, KpOut, iSizeIn, jSizeIn, nVar, NameVar)

    use ModFieldTrace, ONLY: RayResult_VII, RayIntegral_VII, &
         InvB_, Z0x_, Z0y_, Z0b_, RhoInvB_, pInvB_, iPeInvB, &
         HpRhoInvB_, OpRhoInvB_, HpPInvB_, OpPInvB_, iXEnd, ClosedRay, &
         integrate_field_from_sphere
    use ModAdvance, ONLY: UseElectronPressure, UseMultiSpecies
    use ModGroundMagPerturb, ONLY: DoCalcKp, Kp
    use ModFaceBoundary, ONLY: RatioPe2P

    integer,          intent(in):: iSizeIn, jSizeIn, nVar
    real,             intent(out):: Buffer_IIV(iSizeIn,jSizeIn,nVar), KpOut
    character(len=*), intent(in):: NameVar

    real:: Radius

    logical:: DoTest, DoTestMe
    character(len=*), parameter:: NameSub = 'GM_get_for_im'
    !--------------------------------------------------------------------------
    if(DoMultiFluidIMCoupling)then
       if(NameVar /= 'vol:z0x:z0y:bmin:Hprho:Oprho:Hpp:Opp:pe') &
            call CON_stop(NameSub//' invalid NameVar='//NameVar)
    else
       if(NameVar /= 'vol:z0x:z0y:bmin:rho:p:pe') &
            call CON_stop(NameSub//' invalid NameVar='//NameVar)
    end if
    call CON_set_do_test(NameSub//'_tec', DoTestTec, DoTestMe)
    call CON_set_do_test(NameSub//'_idl', DoTestIdl, DoTestMe)
    call CON_set_do_test(NameSub, DoTest, DoTestMe)

    if(iPeInvB /= 7 .and. iPeInvB /= 9) &
      call CON_stop(NameSub//' invalid IPeInvB')

    ! Allocate arrays
    call allocate_gm_im(iSizeIn, jSizeIn)

    ! The RCM ionosphere radius at 100km altitude in normalized units
    Radius = (RadiusPlanet + IonosphereHeightIm)/RadiusPlanet
    if(.not. DoMultiFluidIMCoupling)then
       call integrate_field_from_sphere( &
            iSizeIn, jSizeIn, ImLat_I, ImLon_I, Radius, &
            'InvB,RhoInvB,pInvB,Z0x,Z0y,Z0b,PeInvB')
    else
       call integrate_field_from_sphere( &
            iSizeIn, jSizeIn, ImLat_I, ImLon_I, Radius, &
            'InvB,RhoInvB,pInvB,Z0x,Z0y,Z0b,HpRhoInvB,OpRhoInvB,&
            &HppInvB,OppInvB,PeInvB')
       ! but not pass Rhoinvb, Pinvb to IM
    end if

    if(iProc == 0)then
       ! Copy RayResult into small arrays
       MhdSumVol_II = RayResult_VII(InvB_   ,:,:)
       MhdXeq_II     = RayResult_VII(Z0x_    ,:,:)
       MhdYeq_II     = RayResult_VII(Z0y_    ,:,:)
       MhdBeq_II     = RayResult_VII(Z0b_    ,:,:)
       if(.not.DoMultiFluidIMCoupling)then
          MhdSumRho_II = RayResult_VII(RhoInvB_,:,:)
          MhdSumP_II   = RayResult_VII(pInvB_  ,:,:)
       else
          MhdHpRho_II= RayResult_VII(HpRhoInvB_,:,:)
          MhdOpRho_II= RayResult_VII(OpRhoInvB_,:,:)
          MhdHpP_II= RayResult_VII(HpPInvB_,:,:)
          MhdOpP_II= RayResult_VII(OpPInvB_,:,:)
       end if

       if(UseElectronPressure) then
          MhdSumPe_II= RayResult_VII(iPeInvB,:,:)
       elseif(DoMultiFluidIMCoupling .or. UseMultiSpecies)then
          ! Fraction of Pe relative to HpP (first fluid)
          MhdSumPe_II = RatioPe2P*MhdHpP_II
       else
          ! Split total pressure into ion and electron pressures
          MhdSumP_II = MhdSumP_II/(1 + RatioPe2P)
          MhdSumPe_II = MhdSumP_II*RatioPe2P ! notice MhdSumP_II has changed!
       end if

       ! Put impossible values if the field line is not closed
       if(.not.DoMultiFluidIMCoupling)then
          where(RayResult_VII(iXEnd,:,:) <= ClosedRay)
             MhdXeq_II     = NoValue
             MhdYeq_II     = NoValue
             MhdSumVol_II = 0.0
             MhdSumRho_II = 0.0
             MhdSumPe_II  = 0.0
             MhdSumP_II   = 0.0
             MhdBeq_II     = NoValue
          end where
       else
          where(RayResult_VII(iXEnd,:,:) <= ClosedRay)
             MhdXeq_II     = NoValue
             MhdYeq_II     = NoValue
             MhdSumVol_II = 0.0
             MhdHpRho_II = 0.0
             MhdOpRho_II = 0.0
             MhdSumPe_II = 0.0
             MhdHpP_II   = 0.0
             MhdOpP_II   = 0.0
             MhdBeq_II     = NoValue
          end where
       end if

    end if

    deallocate(RayIntegral_VII, RayResult_VII)

    ! If Kp is being calculated, share it.  Otherwise, share -1.
    if(DoCalcKp) then
       KpOut = Kp
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
       Buffer_IIV(:,:,InvB_)    = MhdSumVol_II
       Buffer_IIV(:,:,Z0x_)     = MhdXeq_II
       Buffer_IIV(:,:,Z0y_)     = MhdYeq_II
       Buffer_IIV(:,:,Z0b_)     = MhdBeq_II

       if(.not.DoMultiFluidIMCoupling)then
          Buffer_IIV(:,:,RhoInvB_) = MhdSumRho_II
          Buffer_IIV(:,:,pInvB_)   = MhdSumP_II
       else
          ! the index is not continuous as in ModFieldTrace
          Buffer_IIV(:,:,HpRhoInvB_) = MhdHpRho_II
          Buffer_IIV(:,:,OpRhoInvB_) = MhdOpRho_II
          Buffer_IIV(:,:,HpPInvB_)   = MhdHpP_II
          Buffer_IIV(:,:,OpPInvB_)   = MhdOpP_II
       end if
       Buffer_IIV(:,:,iPeInvB)       = MhdSumPe_II
    end if

  end subroutine GM_get_for_im
  !============================================================================
  subroutine GM_satinit_for_im(nSat)

    ! This subroutine collects the number of satellite files for use in
    ! SWMF GM and IM coupling.

    ! Module variables to use:
    use ModMain, ONLY: DoImSatTrace
    use ModSatelliteFile, ONLY: nSatellite

    ! Subroutine Arguments:
    integer,           intent(out):: nSat
    !--------------------------------------------------------------------------

    ! If IM sat tracing is on, collect the number of satellites to trace.
    ! If IM sat tracing is off, set nSat to zero.
    if (DoImSatTrace) then
       nSat = nSatellite
    else
       nSat = 0
    endif

  end subroutine GM_satinit_for_im
  !============================================================================
  subroutine GM_get_sat_for_im(Buffer_III, Name_I, nSat)

    ! Subroutine to update and collect satellite locations for IM tracing

    ! Modules
    use ModSatelliteFile, ONLY: NameFileSat_I, XyzSat_DI, &
         get_satellite_ray, set_satellite_flags
    use ModMPI

    ! Arguments
    integer, intent(in)             :: nSat
    real, intent(out)               :: Buffer_III(3,2,nSat)
    character(len=100), intent(out):: Name_I(nSat)

    ! Internal variables

    real:: SatTrace_I(5), SatTraceSum_I(5)

    integer:: iSat, iError
    character(len=*), parameter:: NameSub = 'GM_get_sat_for_im'
    !--------------------------------------------------------------------------
    ! Store satellite names in Buffer_I
    Name_I = NameFileSat_I(1:nSat)

    do iSat = 1, nSat
       ! Update satellite position.
       call set_satellite_flags(iSat)
       call get_satellite_ray(iSat, SatTrace_I)

       ! Reduce values from all
       call MPI_reduce(SatTrace_I, SatTraceSum_I, 5, MPI_REAL, MPI_SUM, &
            0, iComm, iError)

       ! Store results in Buffer_III
       if (iProc == 0) then
          Buffer_III(:,1,iSat) = XyzSat_DI(:,iSat)
          Buffer_III(:,2,iSat) = SatTraceSum_I(1:3)
       end if
    end do

  end subroutine GM_get_sat_for_im
  !============================================================================
  subroutine GM_put_from_im(Buffer_IIV, iSizeIn, jSizeIn, nVar, NameVar)

    use CON_coupler
    use CON_world, ONLY: get_comp_info
    use CON_comp_param, ONLY: lNameVersion
    use ModImCoupling  ! Storage for IM pressure
    use ModFieldTrace, ONLY: UseAccurateTrace, DoMapEquatorRay
    use ModVarIndexes, ONLY:
    use ModAdvance, ONLY: UseMultiSpecies, UseElectronPressure

    integer, intent(in):: iSizeIn,jSizeIn,nVar
    real, intent(in):: Buffer_IIV(iSizeIn,jSizeIn,nVar)
    character(len=*), intent(in):: NameVar
    character(len=lNameVersion):: NameVersionIm
    integer:: nCells_D(2)
    integer, parameter:: pe_=1, pres_=2, dens_=3, parpres_=4, bmin_=5, &
         Hpres_=4, Opres_=5, Hdens_=6, Odens_=7, &
         HeidiHpres_ = 1, HeidiHdens_ = 2, &
         HeidiOpres_=4, HeidiOdens_=6, HeidiNpres_=3, HeidiNdens_=5

    logical:: DoTest, DoTestMe

    character(len=*), parameter:: NameSub = 'GM_put_from_im'
    !--------------------------------------------------------------------------
    call CON_set_do_test(NameSub, DoTest, DoTestMe)

    if(DoMultiFluidIMCoupling)then
      if(IsImHeidi)then
         if(NameVar /= 'Hpp:Hprho:Npp:Opp:Nprho:Oprho') &
            call CON_stop(NameSub//' invalid NameVar='//NameVar)
      else
         if(NameVar /= 'pe:p:rho:Hpp:Opp:Hprho:Oprho') &
               call CON_stop(NameSub//' invalid NameVar='//NameVar)
      end if
    else if(DoAnisoPressureIMCoupling)then
       if(NameVar /= 'pe:p:rho:ppar:bmin') &
            call CON_stop(NameSub//' invalid NameVar='//NameVar)
    else
       if(NameVar /= 'pe:p:rho') &
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
       ! Set up IM ionospheric grid and store.
       ! Latitude specification is module specific, we must set it up
       ! according to the IM module we have selected.
       ! Determine version of IM:
       call get_comp_info(IM_, NameVersion=NameVersionIm)
       if(NameVersionIm(1:3) == 'RAM')then
          ! HEIDI and RAM-SCB have similar equatorial grids.
          ImLat_I = Grid_C(IM_) % Coord1_I

          ! Coupling with RAM/HEIDI grid requires accurate raytrace
          ! that stops at the equator
          UseAccurateTrace= .true.
          DoMapEquatorRay = .true.
       else
          ! RCM uses a Colat based grid, information is stored in
          ! module grid information.
          ImLat_I = (cHalfPi - Grid_C(IM_) % Coord1_I) * cRadToDeg
       end if
       ImLon_I = Grid_C(IM_)% Coord2_I * cRadToDeg
    end if

    ! initialize
    IsImRho_I  = .false.
    IsImP_I    = .false.
    IsImPpar_I = .false.

    ! Store IM variable for internal use
    ImP_III(:,:,1) = Buffer_IIV(:,:,pres_)
    if(UseElectronPressure)then
       ImPe_II = Buffer_IIV(:,:,pe_)
    elseif(.not.DoMultiFluidIMCoupling .or. UseMultiSpecies) then
       ! Add electron pressure to ion pressure if there is no electron pressure
       ! This should NOT be done for multifluid ???!!!
       ImP_III(:,:,1) = Buffer_IIV(:,:,pres_) + Buffer_IIV(:,:,pe_)
    end if

    ImRho_III(:,:,1) = Buffer_IIV(:,:,dens_)
    iNewPIm  = iNewPIm + 1

    IsImRho_I(1)  = .true.
    IsImP_I(1)    = .true.
    IsImPpar_I(1) = .false.

    ! for multifluid
    if(DoMultiFluidIMCoupling)then
       if (UseMultiSpecies) then
          ImRho_III(:,:,2) = Buffer_IIV(:,:,Hdens_)
          ImRho_III(:,:,3) = Buffer_IIV(:,:,Odens_)
          IsImRho_I        = .true.
       elseif(IsImHeidi)then
          ImP_III(:,:,1)   = Buffer_IIV(:,:,HeidiHpres_)
          ImP_III(:,:,2)   = Buffer_IIV(:,:,HeidiNpres_)
          ImP_III(:,:,3)   = Buffer_IIV(:,:,HeidiOpres_)
          ImRho_III(:,:,1) = Buffer_IIV(:,:,HeidiHdens_)
          ImRho_III(:,:,2) = Buffer_IIV(:,:,HeidiNdens_)
          ImRho_III(:,:,3) = Buffer_IIV(:,:,HeidiOdens_)
          IsImRho_I        = .true.
          IsImP_I          = .true.
          IsImPpar_I       = .false.
       else
          ! Overwriting ImP_III(:,:,1) !!!
          ImP_III(:,:,1)   = Buffer_IIV(:,:,Hpres_)
          ImP_III(:,:,2)   = Buffer_IIV(:,:,Opres_)
          ImRho_III(:,:,1) = Buffer_IIV(:,:,Hdens_)
          ImRho_III(:,:,2) = Buffer_IIV(:,:,Odens_)
          IsImRho_I        = .true.
          IsImP_I          = .true.
          IsImPpar_I       = .false.
       endif
    endif

    ! for anisotropic pressure
    if(DoAnisoPressureIMCoupling)then
       ImPpar_III(:,:,1)= Buffer_IIV(:,:,Opres_)
       ! IM_ppar = Buffer_IIV(:,:,parpres_)
       ImBmin_II = Buffer_IIV(:,:,bmin_)
       IsImPpar_I(1) = .true.
       !$acc update device(ImBmin_II, ImPpar_III)
    end if

    DoMapEquatorRay = .false.

    !$acc update device(ImLat_I, ImLon_I)
    !$acc update device(ImP_III, ImRho_III)
    !$acc update device(IsImRho_I, IsImP_I, IsImPpar_I)

    ! if(DoTest)call write_im_vars_tec  ! TecPlot output
    ! if(DoTest)call write_im_vars_idl  ! IDL     output

  end subroutine GM_put_from_im
  !============================================================================
  subroutine GM_put_from_im_cimi(Buffer_IIV,iSizeIn,jSizeIn,nVarIm,NameVarIm)

    use CON_coupler
    use CON_world, ONLY: get_comp_info
    use CON_comp_param, ONLY: lNameVersion
    use ModImCoupling                              ! Storage for IM pressure
    use ModMain, ONLY: nStep,tSimulation
    use ModIoUnit, ONLY: UnitTmp_
    use BATL_lib, ONLY: iProc
    use ModFieldTrace, ONLY: UseAccurateTrace, DoMapEquatorRay
    use ModVarIndexes, ONLY: nFluid, iRho_I, iP_I, iPparIon_I, Pe_, &
         SpeciesFirst_, NameVar_V
    use ModAdvance, ONLY: UseMultiSpecies, nSpecies, UseElectronPressure
    use ModUtilities, ONLY: split_string
    character(len=80):: NameFile

    integer, intent(in):: iSizeIn,jSizeIn,nVarIm
    real, intent(in):: Buffer_IIV(iSizeIn,jSizeIn,nVarIm)

    integer:: iFluid, iVarIm
    ! list of first nVarIm-1 variables provided by IM
    character(len=*), intent(in):: NameVarIm

    character(len=lNameVersion):: NameVersionIm
    integer:: nCells_D(2), iError, i, j

    integer, allocatable, save:: iRhoIm_I(:), iPIm_I(:), iPparIm_I(:), &
                                 iPeIm

    character(len=15), allocatable:: NameVarIm_V(:)

    integer, save:: nDensity
    integer, allocatable, save:: iDens_I(:)
    integer:: iDensity
    logical,save:: IsFirstCall = .true.
    logical:: DoTest, DoTestMe
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
       ! Set up IM ionospheric grid and store.
       ! Latitude specification is module specific, we must set it up
       ! according to the IM module we have selected.
       ! Determine version of IM:
       call get_comp_info(IM_, NameVersion=NameVersionIm)
       if(NameVersionIm(1:3) == 'RAM')then
          ! HEIDI and RAM-SCB have similar equatorial grids.
          ImLat_I = Grid_C(IM_) % Coord1_I

          ! Coupling with RAM/HEIDI grid requires accurate raytrace
          ! that stops at the equator
          UseAccurateTrace= .true.
          DoMapEquatorRay = .true.
       else
          ! RCM uses a Colat based grid, information is stored in
          ! module grid information.
          ImLat_I = (cHalfPi - Grid_C(IM_) % Coord1_I) * cRadToDeg
       end if
       ImLon_I = Grid_C(IM_)% Coord2_I * cRadToDeg
    end if

    allocate(NameVarIm_V(nVarIm-1))
    call split_string(NameVarIm, NameVarIm_V)

    ! loop over GM fluid densities to find IM buffer variable location
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
       allocate(iPeIm)

       ! Initialize values assuming not present
       iRhoIm_I   = -1
       IsImRho_I  = .false.
       iPIm_I     = -1
       IsImP_I    = .false.
       iPparIm_I  = -1
       IsImPpar_I = .false.
       iPeIm      = -1
       IsImPe     = .false.

       do iDensity = 1, nDensity
          do iVarIm = 1, nVarIM - 1
             ! get Density index for buffer
             if(string_to_lower(NameVar_V(iDens_I(iDensity))) &
                  == string_to_lower(NameVarIm_V(iVarIm)) ) then
                ! found a match. set IM fluid index
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
             if(string_to_lower(NameVar_V(iP_I(iFluid))) &
                  == string_to_lower(NameVarIm_V(iVarIm)) ) then
                ! found a match. set IM fluid index
                iPIm_I(iFluid)  = iVarIm
                IsImP_I(iFluid) = .true.
             elseif(iVarIm == nVarIm) then
                ! no match found.
                iPIm_I(iFluid)  = -1
                IsImP_I(iFluid) = .false.
             endif

             if (DoAnisoPressureIMCoupling) then
                ! get Par Pressure index for Buffer
                if(string_to_lower(NameVar_V(iPparIon_I(iFluid))) &
                     == string_to_lower(NameVarIm_V(iVarIm)) ) then
                   ! found a match. set IM fluid index
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

       ! Only ever 1 electron pressure, no need to check multiple times
       if (UseElectronPressure) then
           do iVarIm = 1,nVarIM-1
               ! get Electron Pressure index for Buffer
               if(string_to_lower(NameVar_V(Pe_)) &
                       == string_to_lower(NameVarIm_V(iVarIm)) ) then
                   ! found a match. set IM fluid index
                   iPeIm  = iVarIm
                   IsImPe = .true.
               elseif(iVarIm == nVarIm) then
                   ! no match found.
                   iPeIm  = -1
                   isImPe = .false.
               endif
           end do
       end if

       IsFirstCall = .false.
    endif

    do iDensity=1,nDensity
       if (IsImRho_I(iDensity)) then
          ImRho_III(:,:,iDensity) = Buffer_IIV(:,:,iRhoIm_I(iDensity))
       else
          ImRho_III(:,:,iDensity) = -1.0
       endif
    enddo

    do iFluid=1,nFluid
       if (IsImP_I(iFluid)) then
          ImP_III(:,:,iFluid) = Buffer_IIV(:,:,iPIm_I(iFluid))
          iNewPIm = iNewPIm + 1
       else
          ImP_III(:,:,iFluid) = -1.0
       endif

       if (IsImPpar_I(iFluid) .and. DoAnisoPressureIMCoupling) &
            ImPpar_III(:,:,iFluid) = Buffer_IIV(:,:,iPparIm_I(iFluid))
    enddo

    if (UseElectronPressure .and. IsImPe) then
        ImPe_II = Buffer_IIV(:,:,iPeIm)
    else if (UseElectronPressure) then
        ImPe_II = -1.0
    end if

    ! for anisotropic pressure
    if(DoAnisoPressureIMCoupling) &
         ImBmin_II = Buffer_IIV(:,:,nVarIm)

    if(DoTest)call write_im_vars_tec  ! TecPlot output
    if(DoTest)call write_im_vars_idl  ! IDL     output

  contains
    !==========================================================================
    function string_to_lower(String) result (StringNew)

      use ModUtilities, ONLY: lower_case
      character(len=*), intent(in):: String

      character(len(String)):: StringNew
      !------------------------------------------------------------------------
      StringNew = String
      call lower_case(StringNew)

    end function string_to_lower
    !==========================================================================
    subroutine write_im_vars_tec

      integer:: j2
      real:: LonShift
      !------------------------------------------------------------------------
      if(iProc /= 0)RETURN

      ! write values to plot file
      write(NameFile,'(a,i6.6,a)')"IMp_n=",nStep,".dat"
      open (UNIT=UnitTmp_, FILE=NameFile, STATUS='unknown')
      write(UnitTmp_,'(a)') 'TITLE="Raytrace Values"'
      if(DoMultiFluidIMCoupling)then
         write(UnitTmp_,'(a)') 'VARIABLES="J", "I", "Lon", "Lat",&
              &"IM pressure", "IM density", &
              &"IM Hp pressure", "IM Hp density", &
              &"IM Op pressure", "IM Op density"'
      else
         write(UnitTmp_,'(a)') 'VARIABLES="J", "I", "Lon", "Lat",&
              &"IM pressure", "IM density"'
      end if
      write(UnitTmp_,'(a,i4,a,i4,a)') &
           'ZONE T="IM Pressure", I=',jSizeIn+1,', J=',iSizeIn,', K=1, F=POINT'
      do i=1,iSizeIn
         do j2=1,jSizeIn+1
            j=j2; if(j2==jSizeIn+1) j=1
            LonShift=0.; if(j2==jSizeIn+1) LonShift=360.
            if(DoMultiFluidIMCoupling)then
               write(UnitTmp_,'(2i4,8G14.6)') &
                    j2, i, ImLon_I(j) + LonShift, ImLat_I(i), &
                    ImP_III(i,j,1), ImRho_III(i,j,1), &
                    ImP_III(i,j,2), ImRho_III(i,j,2), &
                    ImP_III(i,j,3), ImRho_III(i,j,3)
            else
               write(UnitTmp_,'(2i4,4G14.6)') &
                    j2, i, ImLon_I(j) + LonShift, ImLat_I(i), &
                    ImP_III(i,j,1),ImRho_III(i,j,1)
            endif
         end do
      end do
      close(UnitTmp_)

    end subroutine write_im_vars_tec
    !==========================================================================
    subroutine write_im_vars_idl
      !------------------------------------------------------------------------
      if(iProc /= 0)RETURN

      ! write values to plot file
      write(NameFile,'(a,i6.6,a)')"IMp_n=",nStep,".out"
      open (UNIT=UnitTmp_, FILE=NameFile, STATUS='unknown', &
           iostat =iError)
      if (iError /= 0) call CON_stop("Can not open file "//NameFile)
      write(UnitTmp_,'(a79)')            'IM pressure'
      write(UnitTmp_,'(i7,1pe13.5,3i3)') nStep,tSimulation,2,0,2
      write(UnitTmp_,'(3i4)')            jSizeIn,iSizeIn
      if(DoMultiFluidIMCoupling)then
         write(UnitTmp_,'(a79)')'Lon Lat p rho Hpp Hprho Opp Oprho'
      else
         write(UnitTmp_,'(a79)')'Lon Lat p rho'
      endif
      do i=iSizeIn,1,-1
         do j=1,jSizeIn
            if(DoMultiFluidIMCoupling)then
               write(UnitTmp_,'(100es18.10)') &
                    ImLon_I(j),ImLat_I(i), &
                    ImP_III(i,j,1), ImRho_III(i,j,1), &
                    ImP_III(i,j,2), ImRho_III(i,j,2), &
                    ImP_III(i,j,3), ImRho_III(i,j,3)
            else
               write(UnitTmp_,'(100(1pe18.10))') &
                    ImLon_I(j),ImLat_I(i),ImP_III(i,j,1),ImRho_III(i,j,1)
            endif
         end do
      end do
      close(UnitTmp_)

    end subroutine write_im_vars_idl
    !==========================================================================
  end subroutine GM_put_from_im_cimi
  !============================================================================
  subroutine allocate_gm_im(iSizeIn,jSizeIn)

    use CON_comp_param, ONLY: IM_

    integer, intent(in):: iSizeIn, jSizeIn

    character(len=*), parameter:: NameSub = 'allocate_gm_im'
    !--------------------------------------------------------------------------
    if(allocated(MhdLatBoundary_I)) deallocate(MhdLatBoundary_I)
    if(allocated(MhdSumVol_II))      deallocate(MhdSumVol_II)
    if(allocated(MhdTmp_II))          deallocate(MhdTmp_II)
    if(allocated(MhdBeq_II))          deallocate(MhdBeq_II)
    if(allocated(MhdXeq_II))          deallocate(MhdXeq_II)
    if(allocated(MhdYeq_II))          deallocate(MhdYeq_II)
    if(allocated(MhdFluxError_II))    deallocate(MhdFluxError_II)

    if(DoMultiFluidIMCoupling)then
       if(allocated(MhdHpRho_II))        deallocate(MhdHpRho_II)
       if(allocated(MhdHpP_II))          deallocate(MhdHpP_II)
       if(allocated(MhdOpRho_II))        deallocate(MhdOpRho_II)
       if(allocated(MhdOpP_II))          deallocate(MhdOpP_II)
    else
       if(allocated(MhdSumRho_II))      deallocate(MhdSumRho_II)
       if(allocated(MhdSumP_II))        deallocate(MhdSumP_II)
    end if
    if(allocated(MhdSumPe_II))          deallocate(MhdSumPe_II)

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
       ! Convert Colat, lon to lat-lon in degrees
       ImLat_I = 90.0 - Grid_C(IM_) % Coord1_I * cRadToDeg
       ImLon_I =        Grid_C(IM_) % Coord2_I * cRadToDeg
    end if

    ! Arrays needed for the field line Integral_I
    allocate( MhdSumVol_II(isize,jsize))
    MhdSumVol_II = 0.

    if(DoMultiFluidIMCoupling)then
       allocate( MhdHpRho_II(isize,jsize), MhdOpRho_II(isize,jsize), &
            MhdHpP_II(isize,jsize), MhdOpP_II(isize,jsize))
       MhdHpRho_II = 0.
       MhdOpRho_II = 0.
       MhdHpP_II = 0.
       MhdOpP_II = 0.
    else
       allocate(MhdSumRho_II(isize,jsize), MhdSumP_II(isize,jsize))
       MhdSumRho_II = 0.
       MhdSumP_II = 0.
    end if
    allocate(MhdSumPe_II(isize,jsize))
    MhdSumPe_II = 0.

    allocate( MhdBeq_II(isize,jsize))
    MhdBeq_II = 0.

    allocate( MhdXeq_II(isize,jsize))
    MhdXeq_II = 0.

    allocate( MhdYeq_II(isize,jsize))
    MhdYeq_II = 0.

    allocate( MhdTmp_II(isize,jsize))
    MhdTmp_II = 0.

    allocate( MhdFluxError_II(isize,jsize))
    MhdFluxError_II = 0.

    allocate( MhdLatBoundary_I(jsize))
    MhdLatBoundary_I = 0

  end subroutine allocate_gm_im
  !============================================================================
  subroutine write_integrated_data_tec

    use ModIoUnit, ONLY: UnitTmp_

    character(len=80):: NameFile
    integer:: j2, nCall=0
    real:: TmpT, TmpV1,TmpV2, LonShift, TmpHpT, TmpOpT
    !--------------------------------------------------------------------------
    nCall = nCall + 1

    ! write values to plot file
    write(NameFile,'(a,i6.6,a,i4.4,a)') &
         "rayValues_n=", nStep, "_", nCall, ".dat"

    OPEN (UNIT=UnitTmp_, FILE=NameFile, STATUS='unknown')
    write(UnitTmp_,'(10a)') 'TITLE="Raytrace Values"'
    if(DoMultiFluidIMCoupling)then
       write(UnitTmp_,'(a)') &
            'VARIABLES="J", "I", "Lon", "Lat", "Lat Boundary (I)"', &
            ', "Xeq", "Yeq"', &
            ', "Volume", "Volume**(-2/3)"', &
            ', "MHD `r", "MHD p", "MHD T"', &
            ', "MHD Hp`r", "MHD Hp p", "MHD Hp T"', &
            ', "MHD Op`r", "MHD Op p", "MHD Op T", "Beq"', &
            ', "FluxError"'
    else
       write(UnitTmp_,'(10a)') &
            'VARIABLES="J", "I", "Lon", "Lat", "Lat Boundary (I)"', &
            ', "Xeq", "Yeq"', &
            ', "Volume", "Volume**(-2/3)"', &
            ', "MHD `r", "MHD p", "MHD T", "Beq"', &
            ', "FluxError"'
    end if
    write(UnitTmp_,'(a,i3.3,a,i4,a,i4,a)') &
         'ZONE T="PE=',iProc,'", I=',jsize+1,', J=',isize,', K=1, F=POINT'
    do i=1,isize
       do j2=1,jsize+1
          j=j2; if(j2==jsize+1) j=1
          LonShift=0.; if(j2==jsize+1) LonShift=360.
          if(DoMultiFluidIMCoupling)then
             TmpHpT=-1.
             if(MhdHpRho_II(i,j)>0.) &
                  TmpHpT = MhdHpP_II(i,j)*Si2No_V(UnitP_) &
                  /(MhdHpRho_II(i,j)*Si2No_V(UnitRho_)) &
                  * No2Si_V(UnitTemperature_)
             TmpOpT=-1.
             if(MhdOpRho_II(i,j)>0.) &
                  TmpOpT = MhdOpP_II(i,j)*Si2No_V(UnitP_) &
                  /(MhdOpRho_II(i,j)*Si2No_V(UnitRho_)) &
                  * No2Si_V(UnitTemperature_)
          else
             TmpT=-1.
             if(MhdSumRho_II(i,j)>0.) &
                  TmpT = MhdSumP_II(i,j)*Si2No_V(UnitP_) &
                  /(MhdSumRho_II(i,j)*Si2No_V(UnitRho_)) &
                  * No2Si_V(UnitTemperature_)
          end if
          TmpV1=0.; if(MhdSumVol_II(i,j)>0.) &
               TmpV1 = (MhdSumVol_II(i,j)/1.e9)
          TmpV2=0.; if(MhdSumVol_II(i,j)>0.) &
               TmpV2 = (MhdSumVol_II(i,j)/1.e9)**(-2./3.)
          if(DoMultiFluidIMCoupling)then
             write(UnitTmp_,'(2i4,18G14.6)') &
                  j2, i, ImLon_I(j) + LonShift, ImLat_I(i),&
                  MhdLatBoundary_I(j), &
                  MhdXeq_II(i,j), MhdYeq_II(i,j), &
                  TmpV1, TmpV2, &
                  MhdHpRho_II(i,j), MhdHpP_II(i,j), TmpHpT, &
                  MhdOpRho_II(i,j), MhdOpP_II(i,j), TmpOpT, MhdBeq_II(i,j), &
                  MhdFluxError_II(i,j)
          else
             write(UnitTmp_,'(2i4,12G14.6)') &
                  j2, i, ImLon_I(j) + LonShift, ImLat_I(i),&
                  MhdLatBoundary_I(j), &
                  MhdXeq_II(i,j), MhdYeq_II(i,j), &
                  TmpV1, TmpV2, &
                  MhdSumRho_II(i,j),MhdSumP_II(i,j),TmpT, MhdBeq_II(i,j), &
                  MhdFluxError_II(i,j)
          end if
       end do
    end do
    close(UnitTmp_)

  end subroutine write_integrated_data_tec
  !============================================================================
  subroutine write_integrated_data_idl

    ! write values to plot file

    use ModIoUnit, ONLY: UnitTmp_
    use ModMain, ONLY: tSimulation
    character(len=100):: NameFile
    integer:: nCall = 0
    !--------------------------------------------------------------------------
    nCall = nCall + 1
    write(NameFile,'(a,i6.6,a,i4.4,a)')"rayValues_n=",nStep,"_",nCall,".out"

    open (UNIT=UnitTmp_, FILE=NameFile, STATUS='unknown', &
         iostat = iError)
    if (iError /= 0) call CON_stop("Can not open raytrace File "//NameFile)
    write(UnitTmp_,'(a79)')            'Raytrace Values_var22'
    write(UnitTmp_,'(i7,1pe13.5,3i3)') nStep,tSimulation,2,1,7
    write(UnitTmp_,'(3i4)')            jSize+1,iSize
    write(UnitTmp_,'(100(1pe13.5))')   0.0
    write(UnitTmp_,'(a79)') 'Lon Lat Xeq Yeq vol rho p Beq FluxError nothing'
    do i=isize,1,-1
       do j=1,jsize
          write(UnitTmp_,'(100(1pe18.10))') &
               ImLon_I(j),       &
               ImLat_I(i),       &
               MhdXeq_II(i,j),     &
               MhdYeq_II(i,j),     &
               MhdSumVol_II(i,j), &
               MhdSumRho_II(i,j), &
               MhdSumP_II(i,j),   &
               MhdBeq_II(i,j),     &
               MhdFluxError_II(i,j)
       end do
       write(UnitTmp_,'(100(1pe18.10))') &
            ImLon_I(1)+360.0, &
            ImLat_I(i),       &
            MhdXeq_II(i,1),     &
            MhdYeq_II(i,1),     &
            MhdSumVol_II(i,1), &
            MhdSumRho_II(i,1), &
            MhdSumP_II(i,1),   &
            MhdBeq_II(i,1),     &
            MhdFluxError_II(i,1)
    end do
    close(UnitTmp_)

  end subroutine write_integrated_data_idl
  !============================================================================
  subroutine process_integrated_data

    integer:: iLoc_I(1),iEquator,iNorthPole
    !--------------------------------------------------------------------------
    if(DoMultiFluidIMCoupling)then
       where(MhdSumVol_II>0.)
          MhdHpRho_II = MhdHpRho_II/MhdSumVol_II
          MhdOpRho_II = MhdOpRho_II/MhdSumVol_II
          MhdHpP_II   = MhdHpP_II/MhdSumVol_II
          MhdOpP_II   = MhdOpP_II/MhdSumVol_II
          MhdSumPe_II = MhdSumPe_II/MhdSumVol_II
       end where
    else
       where(MhdSumVol_II>0.)
          MhdSumP_II   = MhdSumP_II/MhdSumVol_II
          MhdSumRho_II = MhdSumRho_II/MhdSumVol_II
          MhdSumPe_II = MhdSumPe_II/MhdSumVol_II
       end where
    end if

    ! Set volume floor
    MhdSumVol_II = max(1.E-8,MhdSumVol_II)

    ! If the field-line tracer returned a good value, we may need to
    ! add contribution to V from the part of the field line that
    ! goes inside the body. If the field-line tracer did not
    ! return a good value, we will compute total V assuming dipole.
    Ri = (RadiusPlanet + IonosphereHeightIm)/RadiusPlanet
    Factor = 2. * (Ri**4) / abs(Bdp)
    do i=1,isize
       Colat = (90.0 - ImLat_I(i))*cDegToRad
       s2    = sin(Colat)**2
       s6    = s2**3
       s8    = s6*s2

       Ci=abs(cos(Colat))

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
          where(MhdSumVol_II(i,:)>1.1E-8)
             MhdSumVol_II(i,:)=MhdSumVol_II(i,:) + Vol
          end where
       end if

       if( s2 > Ri/Rcurrents )then
          ! Fieldline stays inside of Rcurrents, recompute some values

          ! Compute the full analytic volume
          FCiCs = Ci - Ci**3 + (3./5.)*Ci**5 - (1./7.)*Ci**7
          if (s8 /= 0.0) then
             Vol = Factor*FCiCs/s8
          else
             Vol = 0.0
          endif

          ! Compute equatorial B value for dipole at this latitude
          Beq = abs(Bdp)*s6/Ri**3

          if( s2 > Ri/Rbody )then
             ! Fieldline stays inside of Rbody

             ! Recompute exact volume
             MhdSumVol_II(i,:)=Vol

             ! Fix the grid inside Rbody
             MhdXeq_II(i,:) = (Ri/s2)*cos(ImLon_I*cDegToRad)
             MhdYeq_II(i,:) = (Ri/s2)*sin(ImLon_I*cDegToRad)

             ! Fix the equatorial B value
             MhdBeq_II(i,:) = Beq

          else
             ! Fieldline stays inside of Rcurrents but outside Rbody
             ! Weight analytic formula proportional to the distance
             ! of the field line from rBody within the rBody-rCurrents range

             Factor1= (Ri/Rbody - s2)/ (Ri/Rbody - Ri/Rcurrents)
             Factor2= 1.0 - Factor1

             ! Check if numerical volume exists
             where(MhdSumVol_II(i,:)>1.1E-8)
                ! Blend numerical volume with exact volume
                MhdSumVol_II(i,:) = Factor1*MhdSumVol_II(i,:) + Factor2*Vol
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

          if(MhdSumVol_II(i,j) < 1.1E-8 .or. &
               abs(MhdXeq_II(i,j)) > 200.0 .or. &
               abs(MhdYeq_II(i,j)) > 200.0 ) then
             i0 = i + 1
             EXIT
          end if
       end do

       ! Save the index of the "last" closed field line into MhdLatBoundary_I
       MhdLatBoundary_I(j)=i0
       ! write(*,*) "finding closed field-line ",j,i0,MhdLatBoundary_I(j)

    end do

    ! Set impossible values for open fieldlines
    ! except for Xeq and Yeq where the last closed values are used
    ! which is useful when the equatorial grid is plotted
    do j = 1, jSize
       i = int(MhdLatBoundary_I(j))
       MhdBeq_II(1:i-1,j) = -1.
       MhdXeq_II(1:i-1,j) = MhdXeq_II(i,j)
       MhdYeq_II(1:i-1,j) = MhdYeq_II(i,j)
       MhdSumVol_II(1:i-1,j) = -1.
       if(DoMultiFluidIMCoupling)then
          MhdHpRho_II(1:i-1,j) = -1.
          MhdOpRho_II(1:i-1,j) = -1.
          MhdHpP_II(1:i-1,j) = -1.
          MhdOpP_II(1:i-1,j) = -1.
       else
          MhdSumRho_II(1:i-1,j) = -1.
          MhdSumP_II(1:i-1,j) = -1.
       endif
       MhdSumPe_II(1:i-1,j) = -1.
    end do

    ! Dimensionalize values
    ! Note: dimensions of "MhdSumVol_II" is Distance/Magnetic field.
    ! The distance unit is planetary radius in GM, and the same is
    ! used in RCM, so only the magnetic unit is converted to SI units.
    ! Similarly Xeq and Yeq (equatorial crossing coords) remain in
    ! normalized units.
    if(DoMultiFluidIMCoupling)then
       where(MhdSumVol_II > 0.)
          MhdSumVol_II = MhdSumVol_II / No2Si_V(UnitB_)
          MhdHpRho_II = MhdHpRho_II * No2Si_V(UnitRho_)
          MhdOpRho_II = MhdOpRho_II * No2Si_V(UnitRho_)
          MhdHpP_II   = MhdHpP_II * No2Si_V(UnitP_)
          MhdOpP_II   = MhdOpP_II * No2Si_V(UnitP_)
          MhdSumPe_II = MhdSumPe_II * No2Si_V(UnitP_)
       elsewhere
          MhdSumVol_II = -1.
          MhdHpRho_II = -1.
          MhdOpRho_II = -1.
          MhdHpP_II   = -1.
          MhdOpP_II   = -1.
          MhdSumPe_II = -1.
       end where
    else
       where(MhdSumVol_II > 0.)
          MhdSumVol_II = MhdSumVol_II / No2Si_V(UnitB_)
          MhdSumRho_II = MhdSumRho_II * No2Si_V(UnitRho_)
          MhdSumP_II   = MhdSumP_II   * No2Si_V(UnitP_)
          MhdSumPe_II  = MhdSumPe_II  * No2Si_V(UnitP_)
       elsewhere
          MhdSumVol_II = -1.
          MhdSumRho_II = -1.
          MhdSumP_II   = -1.
          MhdSumPe_II  = -1.
       end where
    end if

    where(MhdBeq_II > 0.)
       MhdBeq_II = MhdBeq_II * No2Si_V(UnitB_)
    elsewhere
       MhdBeq_II = -1.
    end where

    if(DoTestTec .or. DoTestIdl)then
       do j=1,jsize-1; do i=1,isize-1
          if ( MhdSumVol_II(i  ,j) > 0.0 .AND. &
               MhdSumVol_II(i+1,j) > 0.0 .AND. &
               MhdSumVol_II(i,j+1) > 0.0 .AND. &
               MhdSumVol_II(i+1,j+1)> 0.0) THEN
             MhdFluxError_II(i,j) = &
                  0.25E+9*(MhdBeq_II(i,j  ) + MhdBeq_II(i+1,j  ) + &
                  MhdBeq_II(i,j+1) + MhdBeq_II(i+1,j+1)) * &
                  0.5*(ABS((MhdXeq_II(i,j+1)-MhdXeq_II(i,j))* &
                  (MhdYeq_II(i+1,j)-MhdYeq_II(i,j))  &
                  - (MhdYeq_II(i,j+1)-MhdYeq_II(i,j)) &
                  * (MhdXeq_II(i+1,j)-MhdXeq_II(i,j))) &
                  + ABS((MhdXeq_II(i+1,j)-MhdXeq_II(i+1,j+1)) &
                  *(MhdYeq_II(i,j+1)-MhdYeq_II(i+1,j+1)) &
                  -(MhdYeq_II(i+1,j)-MhdYeq_II(i+1,j+1)) &
                  *(MhdXeq_II(i,j+1)-MhdXeq_II(i+1,j+1))))/&
                  (ABS(DipoleStrengthSi)*(SIN(ImLat_I(i)*cDegToRad)**2 &
                  -SIN(ImLat_I(i+1)*cDegToRad)**2)* &
                  (ImLon_I(j+1)-ImLon_I(j))*cDegToRad )- 1.0
          ELSE
             MhdFluxError_II (i,j) = 0.0
          END IF
       end do; end do
       MhdFluxError_II(:,jsize) = MhdFluxError_II (:,1)
       MhdFluxError_II(isize,:) = MhdFluxError_II(isize-1,:)
    end if

  end subroutine process_integrated_data
  !============================================================================
  subroutine set_buffer_indexes(nVarCheck, NameSub)

    integer, optional, intent(in):: nVarCheck
    character(len=*), optional, intent(in):: NameSub

    ! indexes of registered components
    integer:: iGm, iIm
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

    ! Assume GM -> IM direction in CON_couple_gm_im.f90
    allocate(iVarCouple_V(nVarCouple))
    iVarCouple_V = iVarSource_VCC(1:nVarCouple,iGm,iIm)

  end subroutine set_buffer_indexes
  !============================================================================
end module GM_couple_im
!==============================================================================
