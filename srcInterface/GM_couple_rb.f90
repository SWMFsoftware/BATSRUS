!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
!^CMP FILE RB

module GM_couple_rb

  ! Coupling with Radiation Belt component
  use BATL_lib, ONLY: iProc
  use ModMpi
  use CON_coupler, ONLY: Grid_C, ncell_id, CON_set_do_test, CON_stop
  use ModMain, ONLY: nStep
  use ModPhysics, ONLY: No2Si_V, Si2No_V, UnitP_, UnitRho_, UnitTemperature_

  implicit none
  save

  private ! except

  public:: GM_get_for_rb_trace
  public:: GM_get_for_rb
  public:: GM_satinit_for_rb
  public:: GM_get_sat_for_rb

  ! Local variables

  character (len=*), parameter :: NameMod='GM_couple_rb'

  ! RB Grid size
  integer :: nCells_D(2), iSize,jSize

  ! Information about the RB grid ! 2D non-uniform regular grid only !!!
  real, allocatable:: LatRb_I(:), LonRb_I(:)

  integer :: i,j

  real, allocatable :: MhdLatBoundary_I(:)
  real, dimension(:,:), allocatable :: &
       MhdSumVol_II, &
       MhdSumRho_II, &
       MhdSumP_II, &
       MhdBeq_II, &
       MhdXeq_II, &
       MhdYeq_II

  real, parameter :: NoValue=-99999.

  integer :: iError

contains
  !============================================================================
  subroutine allocate_gm_rb(iSizeIn,jSizeIn)

    use CON_comp_param, ONLY: RB_

    integer, intent(in) :: iSizeIn, jSizeIn
    character(len=*), parameter:: NameSub = 'allocate_gm_rb'
    !--------------------------------------------------------------------------
    iSize = iSizeIn
    jSize = jSizeIn

    if(.not.allocated(LatRb_I))then
       nCells_D=ncell_id(RB_)
       if(  iSize /= nCells_D(1) .or. &
            jSize /= nCells_D(2) ) then
          write(*,*)NameSub//' grid sizes do not agree iSize,jSize,nCells=',&
               iSize,jSize, nCells_D(1:2)
          call CON_stop(NameSub//' ERROR')
       end if
       allocate(LatRb_I(iSize), LonRb_I(jSize))
       ! Convert colat, lon to lat-lon in degrees
       LatRb_I = Grid_C(RB_) % Coord1_I
       LonRb_I = Grid_C(RB_) % Coord2_I
    end if

    if(.not.allocated(MhdSumVol_II))then
       allocate( MhdSumVol_II(isize,jsize))
       MhdSumVol_II = 0.
    end if

    if(.not.allocated(MhdSumRho_II))then
       allocate( MhdSumRho_II(isize,jsize))
       MhdSumRho_II = 0.
    end if

    if(.not.allocated(MhdSumP_II))then
       allocate( MhdSumP_II(isize,jsize))
       MhdSumP_II = 0.
    end if

    if(.not.allocated(MhdBeq_II))then
       allocate( MhdBeq_II(isize,jsize))
       MhdBeq_II = 0.
    end if

    if(.not.allocated(MhdXeq_II))then
       allocate( MhdXeq_II(isize,jsize))
       MhdXeq_II = 0.
    end if

    if(.not.allocated(MhdYeq_II))then
       allocate( MhdYeq_II(isize,jsize))
       MhdYeq_II = 0.
    end if

    if(.not.allocated(MhdLatBoundary_I))then
       allocate( MhdLatBoundary_I(jsize))
       MhdLatBoundary_I = 0
    end if

  end subroutine allocate_gm_rb
  !============================================================================
  subroutine write_integrated_data_tec

    use ModIoUnit, ONLY: UnitTmp_

    character(LEN=80) :: NameFile
    integer :: j2, nCall = 0
    real :: TmpT, TmpV1, TmpV2, LonShift
    !--------------------------------------------------------------------------
    nCall = nCall + 1

    ! write values to plot file
    write(NameFile,'(a,i6.6,a,i4.4,a)')"RB/RbValues_n=",nStep,"_",nCall,".dat"

    OPEN (UNIT=UnitTmp_, FILE=NameFile, STATUS='unknown')
    write(UnitTmp_,'(a)') 'TITLE="Raytrace Values"'
    write(UnitTmp_,'(a)') &
         'VARIABLES="J", "I", "Lon", "Lat", "Lat Boundary (I)"', &
         ', "Xeq", "Yeq"', &
         ', "Volume", "Volume**(-2/3)"', &
         ', "MHD `r", "MHD p", "MHD T", "Beq"'
    write(UnitTmp_,'(a,i3.3,a,i4,a,i4,a)') &
         'ZONE T="PE=',iProc,'", I=',jsize+1,', J=',isize,', K=1, F=POINT'
    do i=1,isize
       do j2=1,jsize+1
          j=j2; if(j2==jsize+1) j=1
          LonShift=0.; if(j2==jsize+1) LonShift=360.
          TmpT=-1.; if(MhdSumRho_II(i,j)>0.) &
               TmpT = ((MhdSumP_II(i,j)*Si2No_V(UnitP_)) / &
               (MhdSumRho_II(i,j)*Si2No_V(UnitRho_))) &
               * No2Si_V(UnitTemperature_)
          TmpV1=0.; if(MhdSumVol_II(i,j)>0.) &
               TmpV1 = (MhdSumVol_II(i,j)/1.e9)
          TmpV2=0.; if(MhdSumVol_II(i,j)>0.) &
               TmpV2 = (MhdSumVol_II(i,j)/1.e9)**(-2./3.)
          write(UnitTmp_,'(2i4,12G14.6)') &
               j2, i, LonRb_I(j)+LonShift, LatRb_I(i), &
               MhdLatBoundary_I(j), &
               MhdXeq_II(i,j),MhdYeq_II(i,j), &
               TmpV1, TmpV2, &
               MhdSumRho_II(i,j),MhdSumP_II(i,j),TmpT,MhdBeq_II(i,j)
       end do
    end do
    CLOSE(UnitTmp_)

  end subroutine write_integrated_data_tec
  !============================================================================
  subroutine write_integrated_data_idl

    ! write values to plot file

    use ModPhysics, ONLY: No2Si_V, UnitB_
    use ModIoUnit, ONLY: UnitTmp_
    use ModMain, ONLY: tSimulation

    character(len=100) :: NameFile

    integer :: nCall = 0
    !--------------------------------------------------------------------------
    nCall = nCall+1
    write(NameFile,'(a,i6.6,a,i4.4,a)')"RB/RbValues_n=",nStep,"_",nCall,".out"

    open (UNIT=UnitTmp_, FILE=NameFile, STATUS='unknown', &
         iostat =iError)
    if (iError /= 0) call CON_stop("Can not open raytrace File "//NameFile)
    write(UnitTmp_,'(a79)')            'Raytrace Values_var22'
    write(UnitTmp_,'(i7,1pe13.5,3i3)') nStep,tSimulation,2,1,6
    write(UnitTmp_,'(3i4)')            jSize,iSize
    write(UnitTmp_,'(100(1pe13.5))')   0.0
    write(UnitTmp_,'(a79)')            'Lon Lat Xeq Yeq vol rho p Beq nothing'

    do i=isize,1,-1
       do j=1,jsize
          write(UnitTmp_,'(100(1pe18.10))') &
               modulo(LonRb_I(j)-180.0,360.0),LatRb_I(i), &
               MhdXeq_II(i,j),MhdYeq_II(i,j),&
               MhdSumVol_II(i,j), &
               MhdSumRho_II(i,j), MhdSumP_II(i,j), &
               MhdBeq_II(i,j)*No2Si_V(UnitB_)
       end do
    end do
    close(UnitTmp_)

  end subroutine write_integrated_data_idl
  !============================================================================
  subroutine GM_get_for_rb_trace(iSizeIn, jSizeIn, NameVar, nVarLine, &
       nPointLine)

    ! Do ray tracing for RB.
    ! Provide total number of points along rays
    ! and the number of variables to pass to RB
    use ModFieldTrace, ONLY: DoExtractUnitSi, integrate_field_from_sphere

    use CON_line_extract, ONLY: line_get

    integer, intent(in)           :: iSizeIn, jSizeIn
    character (len=*), intent(in) :: NameVar
    integer, intent(out)          :: nVarLine, nPointLine
    real :: Radius

    character(len=*), parameter:: NameSub = 'GM_get_for_rb_trace'
    !--------------------------------------------------------------------------
    if(NameVar /= 'x:y:bmin:I_I:S_I:R_I:B_I:rho:p') &
         call CON_stop(NameSub//' invalid NameVar='//NameVar)

    ! Allocate arrays
    call allocate_gm_rb(iSizeIn, jSizeIn)

    ! The RB ionosphere radius in normalized units
    Radius = (6378.+100.)/6378.  !!! could be derived from Grid_C ?
    DoExtractUnitSi = .true.
    call integrate_field_from_sphere(&
         iSizeIn, jSizeIn, LatRb_I, LonRb_I, Radius, NameVar)

    call line_get(nVarLine, nPointLine)

    nVarLine = 4 ! We only pass line index, length, B and radial distance to RB

  end subroutine GM_get_for_rb_trace
  !============================================================================
  subroutine GM_get_for_rb(Buffer_IIV, iSizeIn, jSizeIn, nVarIn, &
       BufferLine_VI, nVarLine, nPointLine, NameVar)

    use ModGeometry, ONLY: xMaxBox
    use ModMain, ONLY: tSimulation,TypeCoordSystem
    use ModVarIndexes, ONLY: Rho_, RhoUx_, RhoUy_, RhoUz_, Bx_, By_, Bz_, &
         p_, MassFluid_I, nVar

    use ModPhysics, ONLY: No2Si_V, UnitN_, UnitU_, UnitB_, UnitP_, rBody
    use ModSolarwind, ONLY: get_solar_wind_point
    use ModConst, ONLY: cProtonMass

    use CON_line_extract, ONLY: line_get, line_clean
    use CON_axes, ONLY: transform_matrix
    use CON_planet, ONLY: RadiusPlanet

    integer, intent(in) :: iSizeIn, jSizeIn, nVarIn
    real,    intent(out):: Buffer_IIV(iSizeIn,jSizeIn,nVarIn)

    integer, intent(in) :: nPointLine, nVarLine
    real, intent(out)   :: BufferLine_VI(nVarLine,nPointLine)
    character (len=*), intent(in):: NameVar

    integer :: nVarExtract, nPoint, iPoint, iStartPoint
    real, allocatable :: Buffer_VI(:,:)

    logical :: DoTestTec, DoTestIdl
    logical :: DoTest, DoTestMe

    integer :: iLat,iLon,iLine, iLocBmin
    real    :: SolarWind_V(nVar), SmGm_DD(3,3), XyzBminSm_D(3)
    character(len=*), parameter:: NameSub = 'GM_get_for_rb'
    !--------------------------------------------------------------------------
    if(iProc /= 0) RETURN

    if(NameVar /= 'x:y:bmin:I_I:S_I:R_I:B_I:rho:p') &
         call CON_stop(NameSub//' invalid NameVar='//NameVar)

    call CON_set_do_test(NameSub//'_tec', DoTestTec, DoTestMe)
    call CON_set_do_test(NameSub//'_idl', DoTestIdl, DoTestMe)
    call CON_set_do_test(NameSub, DoTest, DoTestMe)

    ! Initialize
    Buffer_IIV = 0.0

    ! Put the extracted data into BufferLine_VI
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
             Buffer_IIV(iLat, iLon, 1:2) = NoValue*1e6    ! x, y
             Buffer_IIV(iLat, iLon, 3) = NoValue       ! Bmin
             Buffer_IIV(iLat, iLon, 4:5) = 0.0         ! rho, p

          else
             ! For closed field lines
             ! Location of Bmin for this field line
             iLocBmin = minloc(BufferLine_VI(4,iStartPoint:iPoint), dim=1) &
                  + iStartPoint - 1
             Buffer_IIV(iLat, iLon, 3) = BufferLine_VI(4, iLocBmin)    ! Bmin

             ! Convert location from GM to SMG coordinates
             XyzBminSm_D = matmul(SmGm_DD, Buffer_VI(2:4,iLocBmin))
             Buffer_IIV(iLat, iLon, 1) = XyzBminSm_D(1) ! x
             Buffer_IIV(iLat, iLon, 2) = XyzBminSm_D(2) ! y
             Buffer_IIV(iLat, iLon, 4) = Buffer_VI(4+Rho_,iLocBmin) &
                  /cProtonMass                          ! rho in [#/m^3]
             Buffer_IIV(iLat, iLon, 5) = Buffer_VI(4+p_,  iLocBmin)    ! p
          end if

          ! Set the start point for the next field line
          iStartPoint = iPoint +1
       end if
    end do

    deallocate(Buffer_VI)
    call line_clean

    ! Output before processing
    if(DoTest .or. DoTestTec)call write_integrated_data_tec
    if(DoTest .or. DoTestIdl)call write_integrated_data_idl

    ! Send solar wind values in the array of the extra integral
    ! This is a temporary solution. RB should use MhdSumRho_II and MhdSumP_II

    call get_solar_wind_point(tSimulation, [xMaxBox, 0.0, 0.0], SolarWind_V)

    Buffer_IIV(1,:,6) = SolarWind_V(Rho_)/MassFluid_I(1) * No2Si_V(UnitN_)
    Buffer_IIV(2,:,6) = SolarWind_V(RhoUx_) * No2Si_V(UnitU_)
    Buffer_IIV(3,:,6) = SolarWind_V(RhoUy_) * No2Si_V(UnitU_)
    Buffer_IIV(4,:,6) = SolarWind_V(RhoUz_) * No2Si_V(UnitU_)
    Buffer_IIV(5,:,6) = SolarWind_V(Bx_) * No2Si_V(UnitB_)
    Buffer_IIV(6,:,6) = SolarWind_V(By_) * No2Si_V(UnitB_)
    Buffer_IIV(7,:,6) = SolarWind_V(Bz_) * No2Si_V(UnitB_)
    Buffer_IIV(8,:,6) = SolarWind_V(p_)  * No2Si_V(UnitP_)

  end subroutine GM_get_for_rb
  !============================================================================
  subroutine GM_satinit_for_rb(nSats)

    ! This subroutine collects the number of satellite files for use in
    ! SWMF GM and RB coupling.

    ! Module variables to use:
    use ModMain, ONLY: DoRbSatTrace
    use ModSatelliteFile, ONLY: nSatellite

    ! Subroutine Arguments:
    integer,           intent(out) :: nSats
    !--------------------------------------------------------------------------

    ! If RB sat tracing is on, collect the number of satellites to trace.
    ! If RB sat tracing is off, set nSats to zero.
    if (DoRbSatTrace) then
       nSats = nSatellite
    else
       nSats = 0
    endif

  end subroutine GM_satinit_for_rb
  !============================================================================
  subroutine GM_get_sat_for_rb(Buffer_III, NameBuffer_I, nSats)

    ! Subroutine to update and collect satellite locations for RB tracing

    use ModSatelliteFile, ONLY: NameFileSat_I, XyzSat_DI, gm_trace_sat
    use ModWriteLogSatFile, ONLY: collect_satellite_data
    use ModMain, ONLY: UseB0, nBlock
    use ModPhysics, ONLY: No2Si_V, UnitB_
    use ModVarIndexes, ONLY: nVar, Bx_, By_, Bz_
    use ModB0, ONLY: get_b0
    use ModCurrent, ONLY: get_point_data
    use ModMPI

    ! Arguments
    integer, intent(in)               :: nSats
    real, intent(out)                 :: Buffer_III(4,2,nSats)
    character (len=100), intent(out)  :: NameBuffer_I(nSats)

    ! Local variables

    real ::SatRay_D(3)

    real :: StateSat_V(0:3), B0Sat_D(3)
    integer :: iSat
    character(len=*), parameter:: NameSub = 'GM_get_sat_for_rb'
    !--------------------------------------------------------------------------
    ! Store satellite names in NameBuffer_I
    NameBuffer_I = NameFileSat_I(1:nSats)

    do iSat=1, nSats
       ! Update satellite position.
       call GM_trace_sat(XyzSat_DI(:,iSat), SatRay_D)

       call get_point_data(0.0, XyzSat_DI(:,iSat), 1, nBlock, Bx_, Bz_, &
            StateSat_V)
       call collect_satellite_data(XyzSat_DI(:,iSat), 3, StateSat_V)

       ! Store results in Buffer_III
       if (iProc == 0) then
          B0Sat_D = 0.0
          if(UseB0) call get_b0(XyzSat_DI(:,iSat), B0Sat_D)

          Buffer_III(1:3,1,iSat)  = XyzSat_DI(1:3,iSat)
          ! Buffer_III(1:3,2,iSat) = sat_RayVarsSum(1:3)
          Buffer_III(1:3,2,iSat)  = SatRay_D
          Buffer_III(4,2,iSat)    = &
               sum( (StateSat_V(1:3) + B0Sat_D)**2 ) * No2Si_V(UnitB_)**2
       end if
    end do

  end subroutine GM_get_sat_for_rb
  !============================================================================
end module GM_couple_rb
!==============================================================================
