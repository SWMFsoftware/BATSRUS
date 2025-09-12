!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModWriteLogSatFile

  use BATL_lib, ONLY: &
       test_start, test_stop, StringTest, XyzTestCell_D, &
       iTest, jTest, kTest, iBlockTest, iProcTest, xTest, yTest, zTest, &
       iProc, nProc, iComm
  use ModBatsrusUtility, ONLY: get_date_time, stop_mpi
  use ModUpdateState, ONLY: check_nan
  use, intrinsic:: ieee_arithmetic

#ifdef _OPENACC
  use ModUtilities, ONLY: norm2
#endif
  use ModUtilities, ONLY: i_gang

  implicit none

  private ! except

  integer:: iFluid ! to make iFluid local in this module

  public:: write_logfile          ! write one line into the log file
  public:: collect_satellite_data ! collect data from multiple processors
  public:: calc_sphere            ! calculate values on spherical surface

contains
  !============================================================================
  real function minmod(x, y)

    real, intent(in):: x, y
    !--------------------------------------------------------------------------
    minmod = max(0.0, min(abs(x), sign(1.0, x)*y))

  end function minmod
  !============================================================================
  real function maxmod(x, y)

    real, intent(in):: x, y
    !--------------------------------------------------------------------------
    maxmod = max(abs(x), abs(y))

  end function maxmod
  !============================================================================
  subroutine write_logfile(iSatIn, iFile, TimeSatHeaderIn)

    use ModMain
    use ModVarIndexes
    use ModAdvance, ONLY: State_VGB, Tmp1_GB
    use ModPhysics, ONLY: Si2Io_V, Si2No_V, UnitT_
    use ModIO
    use ModIoUnit, ONLY: io_unit_new
    use ModUtilities, ONLY: flush_unit, split_string, open_file
    use ModSatelliteFile, ONLY: NameFileSat_I, IsFirstWriteSat_I, iUnitSat_I, &
         TypeTimeSat_I, StringSatVar_I, DoTrackSatellite_I, XyzSat_DI
    use CON_axes, ONLY: transform_matrix
    use BATL_lib, ONLY: Xyz_DGB, UseTestXyz, maxval_grid, minval_grid
    use ModMpi

    ! Arguments

    integer, intent(in):: iFile

    ! iSatIn = 0 -> write logfile
    ! iSatIn >=1 -> write satellite output files (number = isat)
    integer, intent(in):: iSatIn

    real, optional, intent(in):: TimeSatHeaderIn

    ! Logfile variables
    integer, parameter:: MaxLogVar=200
    integer, parameter:: MaxLogR = 10
    real:: LogVar_I(MaxLogVar)
    character (len=lNameLogVar):: NameLogVar_I(MaxLogVar)
    character (len=lNameLogVar):: NameLogVar
    character (len=10):: NameLogR_I(MaxLogR)
    real:: LogR_I(MaxLogR)
    integer:: nLogVar, nLogR, nLogTot, i, j, iSat
    integer:: nFluxVar       ! number of flux variables used
    integer:: iUnit          ! local unit number
    real:: Xyz_D(3)
    character (LEN=500):: NameAll
    character (LEN=100):: StringTime

    logical:: DoWritePosition
    real:: pMin, pMax
    integer:: iLoc_I(5)
    integer:: iTime_I(7) ! integer time: year,month,day,hour,minute,sec,msec
    integer:: iError

    ! Coordinate system transformation
    real:: Convert_DD(3,3)
    integer:: iVar

    ! Event date for NameFile
    character(len=19):: StringDateTime

    ! Header for the sat file in time accurate
    real:: TimeSatHeader

    ! Parcel variables
    integer        :: iParcel, iUnitParcel_I(MaxParcel)=-1
    character(len=2):: StringIParcel

    ! NaN detection variables

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_logfile'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(present(TimeSatHeaderIn)) then
       TimeSatHeader = TimeSatHeaderIn
    else
       TimeSatHeader = tSimulation
    end if

    DoWritePosition = .false.

    if(iSatIn == 0 .and. ( index(StringTest,'show_pmin')>0 .or. &
         index(StringTest,'show_pmax')>0 ) ) then

       Tmp1_GB(1:nI,1:nJ,1:nK,1:nBlock) &
            = State_VGB(P_,1:nI,1:nJ,1:nK,1:nBlock)

       if(index(StringTest,'show_pmin')>0)then
          pMin = minval_grid(Tmp1_GB, iLoc_I=iLoc_I)
          if(iLoc_I(5)==iProc)write(*,*)'pMin, loc, x, y, z=', pMin, iLoc_I, &
               Xyz_DGB(:,iLoc_I(1),iLoc_I(2),iLoc_I(3),iLoc_I(4))
       end if

       if(index(StringTest,'show_pmax')>0)then
          pMax = maxval_grid(Tmp1_GB, iLoc_I=iLoc_I)
          if(iLoc_I(5)==iProc)write(*,*)'pMax, loc, x, y, z=', pMax, iLoc_I, &
               Xyz_DGB(:,iLoc_I(1),iLoc_I(2),iLoc_I(3),iLoc_I(4))

       end if
    end if

    nFluxVar = 0
    nLogR    = 0
    LogR_I   = 0.0
    if (iSatIn == 0) then
       call split_string(StringLogVar, MaxLogVar, NameLogVar_I, nLogVar, &
            UseArraySyntaxIn=.true.)
       do i=1,nLogVar
          if(index(NameLogVar_I(i),'flx')>0) nFluxVar = nFluxVar + 1
          if(index(NameLogVar_I(i),'pnt')>0 .or.  &
               index(NameLogVar_I(i),'test')>0)  DoWritePosition = .true.
       end do
       if (nFluxVar > 0) then
          call split_string(StringLogRadius, MaxLogR, NameLogR_I, nLogR)
          do i=1,nLogR
             read(NameLogR_I(i),*) LogR_I(i)
          end do
       end if
       StringTime = TypeLogTime
    elseif (iSatIn >= 1) then
       iSat = iSatIn
       if (.not. DoTrackSatellite_I(iSat)) RETURN
       call split_string(StringSatVar_I(iSat), MaxLogVar, &
            NameLogVar_I, nLogVar, UseArraySyntaxIn=.true.)
       StringTime = TypeTimeSat_I(iSat)
       DoWritePosition = .true.
    elseif (iSatIn<0) then
       if (IsTimeAccurate)then
          StringTime = 'step time'
       else
          StringTime = 'step'
       end if
       DoWritePosition = .true.
       call split_string(StringParcelVar, MaxLogVar, NameLogVar_I, nLogVar, &
            UseArraySyntaxIn=.true.)
    end if

    ! check to make sure that the total number of Logfile variables is smaller
    ! than MaxLogVar.  If it is not write a warning message and truncate the
    ! list.
    if (nLogVar + nFluxVar*nLogR > MaxLogVar) then
       write(*,*)'WARNING !!! ', NameSub, &
            ': Number of logfile variables exceeds MaxLogVar.'
       if (nLogVar >= MaxLogVar) then
          nLogVar = MaxLogVar
          nLogR = 1
       else
          nLogR = (MaxLogVar-nLogVar)/nFluxVar + 1
       end if
    end if
    nLogTot = nLogVar + nFluxVar*(nLogR-1)

    ! load the time variables to the list of output
    NameAll = ''
    if(index(StringTime,'none')<=0) then
       if(index(StringTime,'step')>0) NameAll = 'it'
       if(index(StringTime,'date')>0) NameAll = trim(NameAll)// &
            ' year mo dy hr mn sc msc'
       if(index(StringTime,'time')>0) NameAll = trim(NameAll)//' t'
    end if

    if (DoWritePosition) then
       NameAll = trim(NameAll)//' X Y Z'
       if (iSatIn == 0) then
          Xyz_D = XyzTestCell_D
       elseif (iSatIn >= 1) then
          Xyz_D = XyzSat_DI(:,iSat)
       elseif (iSatIn<0) then
          Xyz_D = Parcel_DI(:,-iSatIn)
       end if
    end if

    do i = 1, nLogVar
       if(index(NameLogVar_I(i),'flx')>0) then
          do j = 1, nLogR
             NameAll = trim(NameAll)//' '//trim(NameLogVar_I(i))//'_R='// &
                  NameLogR_I(j)
          end do
       else
          NameAll = trim(NameAll)//' '//NameLogVar_I(i)
       end if
    end do

    if(DoTest .and. nStep <= 1)then
       write(*,*)'nLogVar,nFluxVar,nLogR,nLogTot:',  &
            nLogVar,nFluxVar,nLogR,nLogTot
       write(*,*)'NameLogVar_I:',NameLogVar_I(1:nLogVar)
       write(*,*)'NameLogR_I:',NameLogR_I(1:nLogR)
       write(*,*)'LogR_I:',LogR_I(1:nLogR)
       write(*,*)'NameAll:',trim(NameAll)
    end if

    if(iProc==0) then
       if (iSatIn==0) then

          if(iUnitLogfile < 0)then
             iUnitLogfile = io_unit_new()
             NameFile = trim(NamePlotDir) // 'log'

             if(IsLogNameE)then
                ! Event date added to log file name
                call get_date_time(iTime_I)
                write(StringDateTime, '(i4.4,2i2.2,"-",3i2.2)') iTime_I(1:6)
                NameFile = trim(NameFile) // '_e' // trim(StringDateTime)
             else
                if(nStep < 1000000)then
                   write(NameFile,'(a,i6.6)') trim(NameFile)//'_n',nStep
                else
                   write(NameFile,'(a,i8.8)') trim(NameFile)//'_n',nStep
                end if
             end if

             NameFile = trim(NameFile) // '.log'

             call open_file(iUnitLogfile, FILE=NameFile)
             if (index(NameAll,'pnt')>0 .or. index(NameAll,'PNT')>0 &
                  .or. index(NameAll,'test')>0) then
                if (UseTestXyz) then
                   write(iUnitLogfile,'(a,3es13.5)')  &
                        'test point (X,Y,Z): ',xTest,yTest,zTest
                else
                   write(iUnitLogfile,'(a,5(i6))')  &
                        'test point(I,J,K,BLK,PROC): ', &
                        iTest,jTest,kTest,iBlockTest,iProcTest
                end if
             else
                write(iUnitLogfile,'(a)')'Volume averages, fluxes, etc'
             end if
             write(iUnitLogfile,'(a)') trim(NameAll)
             ! Add #START line if variables contain PNT (capitalized)
             ! so that the file can be read as IMF/satellite input file.
             if (index(NameAll,'PNT')>0) write(iUnitLogfile,'(a)') '#START'
          endif
          iUnit = iUnitLogfile
       elseif (iSatIn >= 1) then
          iUnit = iUnitSat_I(iSat)
          if (IsFirstWriteSat_I(iSat)) then
             if (IsTimeAccurate) then
                write(iUnit,'(a, es13.5)')  &
                     'Satellite data for Satellite: ' // &
                     trim(NameFileSat_I(isat))        // &
                     ' at simulation time =', TimeSatHeader
             else
                write(iUnit,'(a)')  &
                     'Satellite data for Satellite: '//  &
                     trim(NameFileSat_I(isat))
             end if
             write(iUnit,'(a)')trim(NameAll)
             IsFirstWriteSat_I(iSat)=.false.
          end if

       elseif (iSatIn < 0) then
          iParcel = -iSatIn
          iUnit = iUnitParcel_I(iParcel)
          if(iUnit < 0)then
             iUnitParcel_I(iParcel) = io_unit_new()
             iUnit = iUnitParcel_I(iParcel)
             write(StringIParcel, '(i2.2)') iParcel
             NameFile = trim(NamePlotDir) // 'pcl'//'_'//StringIParcel//'_'
             if (IsTimeAccurate) then
                call get_date_time(iTime_I)
                write(StringDateTime, '(i4.4,2i2.2,"-",3i2.2)') iTime_I(1:6)
                NameFile = trim(NameFile)//'_t' //trim(StringDateTime)
             else
                if(nStep < 1000000)then
                   write(NameFile,'(a,i6.6)') trim(NameFile)//'_n',nStep
                else
                   write(NameFile,'(a,i8.8)') trim(NameFile)//'_n',nStep
                end if
             end if
             NameFile = trim(NameFile) // '.pcl'

             call open_file(iUnit, FILE=NameFile)
             write(iUnit,'(a)')'Lagrangian parcel path'
             write(iUnit,'(a)') trim(NameAll)
          endif
       endif
    endif
    call set_logvar(nLogVar, NameLogVar_I, nLogR, LogR_I, nLogTot, LogVar_I, &
         iSatIn)

    ! this write statement seems to be necessary for
    ! NAG compiler in debugging mode
    if(DoTest) &
         write(*,*) NameSub, ' set_logvar finished'

    ! Collect LogVar_I from all processors
    if(nProc > 1) call MPI_allreduce(MPI_IN_PLACE, LogVar_I, nLogTot, &
         MPI_REAL, MPI_SUM, iComm, iError)

    ! Do a check if any variable is NaN and STOP with an error message.
    do iVar = 1, nLogTot
       if (ieee_is_nan(LogVar_I(iVar))) then
          call check_nan(NameSub)
          call MPI_barrier(iComm, iError)
          if(iProc == 0) call stop_mpi('ERROR: NaN in Log file. '//&
               'Code stopped with NaN in variable - '//NameLogVar_I(iVar))
       end if
    end do

    ! WRITE OUT THE LINE INTO THE LOGFILE OR THE SATELLITE FILE
    if(iProc==0) then

       if (IsDimensionalPlot_I(iFile)) call normalize_logvar( &
            nLogVar, NameLogVar_I, nLogR, LogR_I, nLogTot, LogVar_I)

       ! first output the appropriate time data
       if(index(StringTime,'none')>0) then
          ! do nothing
       else
          if(index(StringTime,'step')>0) &
               write(iUnit,'(i8)',ADVANCE='NO') nStep
          if(index(StringTime,'date')>0) then
             call get_date_time(iTime_I)
             write(iUnit,'(i5,5(1X,i2.2),1X,i3.3)', ADVANCE='NO') iTime_I
          end if
          if(index(StringTime,'time')>0) then
             ! note that tSimulation is in SI units.
             if(IsDimensionalPlot_I(iFile)) then
                write(iUnit,'(es13.5)',ADVANCE='NO') &
                     tSimulation*Si2Io_V(UnitT_)
             else
                write(iUnit,'(es13.5)',ADVANCE='NO') &
                     tSimulation*Si2No_V(UnitT_)
             end if
          end if
       end if

       if(TypeCoordPlot_I(iFile) /= 'SYS' &
            .and. TypeCoordPlot_I(iFile) /= TypeCoordSystem)then

          Convert_DD = transform_matrix(tSimulation, &
               TypeCoordSystem, TypeCoordPlot_I(iFile))

          if(DoWritePosition) Xyz_D = matmul(Convert_DD, Xyz_D)

          ! Try to recognize vectors by name and convert them
          do iVar = 1, nLogTot-2
             call normalize_name_log_var(NameLogVar_I(iVar), NameLogVar)
             select case(NameLogVar)
             case('ux', 'rhoux', 'bx', 'b0x', 'b1x', 'jx', &
                  'uxpnt', 'rhouxpnt', 'bxpnt', 'b1xpnt', 'jxpnt')
                LogVar_I(iVar:iVar+2) = &
                     matmul(Convert_DD, LogVar_I(iVar:iVar+2))
             end select
          end do

       end if

       ! Now write the position of the test point or satellite if desired
       if(DoWritePosition) &
            write(iUnit,'(3es13.5)',ADVANCE='NO') Xyz_D

       ! Finally write out the data variables
       write(iUnit,'(100es14.5e3)') LogVar_I(1:nLogTot)

       call flush_unit(iUnit)
    end if

    call test_stop(NameSub, DoTest)

  end subroutine write_logfile
  !============================================================================
  subroutine set_logvar( &
       nLogVar, NameLogVar_I, nLogR, LogR_I, nLogTot, LogVar_I, iSat)

    use ModNumConst, ONLY: cPi
    use ModMain, ONLY: nStep, Dt, Cfl, TypeMessagePass, &
         UseRotatingFrame,UseB0, NameVarLower_V
    use ModPhysics, ONLY: rCurrents, InvGammaMinus1_I, OmegaBody, &
         ElectronPressureRatio, InvGammaElectronMinus1
    use ModVarIndexes
    use ModAdvance, ONLY: Tmp1_GB, Tmp2_GB, State_VGB, StateOld_VGB, DivB1_GB
    use ModCurrent, ONLY: get_point_data
    use ModB0, ONLY: B0_DGB, get_b0
    use ModGeometry, ONLY: xMinBox, xMaxBox, yMinBox, yMaxBox, &
         zMinBox, zMaxBox, DomainVolume
    use ModFieldTrace, ONLY: Trace_DSNB
    use ModSatelliteFile, ONLY: get_satellite_ray
    use ModSatelliteFile, ONLY: XyzSat_DI
    use ModIO, ONLY: lNameLogVar,Parcel_DI
    use ModMultiFluid, ONLY: UseMultiIon, &
         iRho, iP, iPpar, iRhoUx, iRhoUy, iRhoUz, iRhoIon_I, MassIon_I
    use BATL_lib, ONLY: nI, nJ, nK, nBlock, Unused_B, x_, y_, &
         integrate_grid, maxval_grid, minval_grid, MinIJK_D, &
         MaxIJK_D, nDim, xyz_to_coord, CellVolume_GB
    use BATL_tree, ONLY: find_tree_cell, iTree_IA, Block_, Proc_
    use BATL_geometry, ONLY: CoordMin_D, DomainSize_D
    use ModInterpolate, ONLY: interpolate_vector
    use ModMPI

    integer, intent(in)                    :: nLogVar, nLogR, nLogTot, iSat
    character (len=lNameLogVar), intent(in):: NameLogVar_I(nLogVar)
    real, intent(in)                       :: LogR_I(nLogR)
    real, intent(out)                      :: LogVar_I(nLogTot)

    character (len=lNameLogVar):: NameLogVar

    real:: StateIntegral_V(nVar)
    real:: SatRayVar_I(5)

    integer:: iVar, iR, iVarTot, iBlock
    integer:: i, j, k
    integer:: iError
    real:: r

    ! StateSat_V contains the weight (0), the state (1:nVar),
    ! and the currents (nVar+1:nVar+3) at the position of the satellite
    ! B0Sat_D contains B0 at the satellite
    real:: StateSat_V(0:nVar+3), B0Sat_D(3)

    ! Parcel part
    integer:: iParcel, iParcelBlock, iParcelProc, iParcelNode
    integer:: iParcelCell_D(3)
    real   :: Coord_D(3),ParcelCellDistance_D(3)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_logvar'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(DoTest.and.nStep==1)then
       write(*,*)'nLogVar,nLogR,nLogTot:',nLogVar,nLogR,nLogTot
       write(*,*)'NameLogVar_I:',NameLogVar_I(1:nLogVar)
       write(*,*)'LogR_I:',LogR_I(1:nLogR)
    end if

    LogVar_I = 0.0
    if(abs(iSat)==0)  then
       Tmp1_GB = 1.0
       DomainVolume  =integrate_grid(Tmp1_GB, UseGlobal=.true.)
    end if

    ! Obtain data to calculate log variables
    if(iSat>=1)then
       ! Satellites need B0 and the state at the satellite position
       if(UseB0)then
          call get_b0(XyzSat_DI(:,iSat), B0Sat_D)
       else
          B0Sat_D = 0.0
       end if

       call get_point_data(0.0, XyzSat_DI(:,iSat), 1,nBlock, 1, nVar+3, &
            StateSat_V)
       call collect_satellite_data(XyzSat_DI(:,iSat), nVar+3, StateSat_V)

       if (UseRotatingFrame) then
          StateSat_V(rhoUx_)=StateSat_V(rhoUx_) &
               - StateSat_V(rho_)*OmegaBody*XyzSat_DI(y_,iSat)
          StateSat_V(rhoUy_)=StateSat_V(rhoUy_) &
               + StateSat_V(rho_)*OmegaBody*XyzSat_DI(x_,iSat)
       end if

       ! If any tracing satellite variables are present, collect trace data
       do iVar=1, nLogVar
          select case(NameLogVar_I(iVar))
          case('lon1', 'lon2', 'lat1', 'lat2', 'status', &
               'theta1', 'theta2', 'phi1', 'phi2') ! backward compatible
             call get_satellite_ray(iSat, SatRayVar_I)
             if(nProc > 1)call MPI_reduce_real_array(SatRayVar_I, 5, MPI_SUM, &
                  0, iComm, iError)
             EXIT
          end select
       enddo

    elseif(iSat==0)then
       ! The logfile may need the integral of conservative variables
       ! Also extract the pressure into a variable for pMin and pMax
       do iVar = 1, nLogVar
          call normalize_name_log_var(NameLogVar_I(iVar), NameLogVar)
          select case(NameLogVar)
          case('rho','rhoux','rhouy','rhouz','bx','by','bz','p','pmin','pmax')
             call integrate_domain(StateIntegral_V, Tmp2_GB)
             EXIT
          end select
       end do
    elseif(iSat<0)then
       iParcel = -iSat
       call xyz_to_coord(Parcel_DI(1:3,iParcel),Coord_D)

       ! Get State variables from that location
       ! Point coordinates are given in generalized coordinates normalized to
       ! the domain size: CoordIn_D = (CoordOrig_D - CoordMin_D)/DomainSize_D
       call find_tree_cell((Coord_D-CoordMin_D)/DomainSize_D,&
            iParcelNode, iParcelCell_D, ParcelCellDistance_D)

       iParcelBlock  = iTree_IA(Block_,iParcelNode)
       iParcelProc   = iTree_IA(Proc_,iParcelNode)

       StateSat_V = 0
       if(iProc == iParcelProc)then
          StateSat_V(1:nVar) = &
               interpolate_vector(State_VGB(:,:,:,:,iParcelBlock),nVar,nDim,&
               MinIJK_D,MaxIJK_D,ParcelCellDistance_D+iParcelCell_D)
       end if

       if(nProc > 1)call MPI_reduce_real_array(StateSat_V(1:), nVar, MPI_SUM, &
            0, iComm, iError)

       if(iProc == 0)then
          if(UseB0)then
             call get_b0(Parcel_DI(1:3,iParcel), B0Sat_D)
          else
             B0Sat_D = 0.0
          end if

          if (UseRotatingFrame) then
             StateSat_V(rhoUx_)=StateSat_V(rhoUx_) &
                  - StateSat_V(rho_)*OmegaBody*Parcel_DI(y_,iParcel)
             StateSat_V(rhoUy_)=StateSat_V(rhoUy_) &
                  + StateSat_V(rho_)*OmegaBody*Parcel_DI(x_,iParcel)
          end if
       end if
    end if

    iVarTot = 0
    do iVar = 1, nLogVar

       iVarTot = iVarTot + 1
       call normalize_name_log_var(NameLogVar_I(iVar), NameLogVar)

       ! If we are a satellite and not a logfile (iSat>=1) then we should
       ! do only satellite variables so append a 'sat' to the end of the
       ! variables
       if (abs(iSat) >= 1) then
          call set_sat_var
       else
          call set_log_var
       end if
    end do

    call test_stop(NameSub, DoTest)
  contains
    !==========================================================================
    subroutine set_log_var

      use ModMain, ONLY: x_, y_, z_, TypeCoordSystem, tSimulation
      use ModUtilities, ONLY: lower_case
      use ModCurrent, ONLY: get_current
      use ModEnergy, ONLY: get_fluid_energy_block, energy_i
      use ModWaves, ONLY: UseWavePressure
      use BATL_lib, ONLY: Xyz_DGB, message_pass_cell
      use ModGeometry, ONLY: r_GB
      use ModIO, ONLY: lNameLogVar
      use ModIeCoupling, ONLY: logvar_ionosphere
      use ModCoordTransform, ONLY: cross_product
      use CON_axes, ONLY: transform_matrix
      use ModUserInterface ! user_get_log_var
      use ModFaceBoundary, ONLY: ratioOH
      use ModPhysics, ONLY: Gbody
      use ModRadiativeCooling, ONLY: RadCooling_C, get_radiative_cooling
      use ModChromosphere, ONLY: get_tesi_c, TeSi_C

      ! Local variables
      real:: Bx, By, Bz, RhoUx, RhoUy, RhoUz, bDotB, bDotU, Value
      real:: Current_D(3)
      real:: FullB_DG(3,0:nI+1,0:nJ+1,0:nK+1)
      real:: Convert_DD(3,3)

      integer:: jVar, iGang
      character(len=lNameLogVar):: NameLogVarLower
      !------------------------------------------------------------------------
      select case(NameLogVar)
      case('volume')
         ! Total volume
         LogVar_I(iVarTot) = DomainVolume/nProc

         ! MHD variables averaged over the computational domain
      case('e')
         do iBlock = 1, nBlock
            if (Unused_B(iBlock)) CYCLE
            call get_fluid_energy_block(iBlock, iFluid, Tmp1_GB(:,:,:,iBlock))
         end do
         LogVar_I(iVarTot) = integrate_grid(Tmp1_GB)/DomainVolume
      case('pmin')
         ! Divide by nProc so that adding up the processors can work
         LogVar_I(iVarTot) = minval_grid(Tmp2_GB)/nProc
      case('pmax')
         ! Divide by nProc so that adding up the processors can work
         LogVar_I(iVarTot) = maxval_grid(Tmp2_GB)/nProc
      case('urmin')
         do iBlock = 1, nBlock
            if (Unused_B(iBlock)) CYCLE
            Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) = &
                 ( State_VGB(iRhoUx, 1:nI,1:nJ,1:nK,iBlock)   &
                 * Xyz_DGB(       x_,1:nI,1:nJ,1:nK,iBlock)   &
                 + State_VGB(iRhoUy, 1:nI,1:nJ,1:nK,iBlock)   &
                 * Xyz_DGB(       y_,1:nI,1:nJ,1:nK,iBlock)   &
                 + State_VGB(iRhoUz, 1:nI,1:nJ,1:nK,iBlock)   &
                 * Xyz_DGB(       z_,1:nI,1:nJ,1:nK,iBlock) ) &
                 / State_VGB(iRho,1:nI,1:nJ,1:nK,iBlock)      &
                 / r_GB(1:nI,1:nJ,1:nK,iBlock)
         end do
         ! Divide by nProc so that adding up the processors can work
         LogVar_I(iVarTot) = minval_grid(Tmp1_GB)/nProc
      case('urmax')
         do iBlock = 1, nBlock
            if (Unused_B(iBlock)) CYCLE
            Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) = &
                 ( State_VGB(iRhoUx, 1:nI,1:nJ,1:nK,iBlock)   &
                 * Xyz_DGB(       x_,1:nI,1:nJ,1:nK,iBlock)   &
                 + State_VGB(iRhoUy, 1:nI,1:nJ,1:nK,iBlock)   &
                 * Xyz_DGB(       y_,1:nI,1:nJ,1:nK,iBlock)   &
                 + State_VGB(iRhoUz, 1:nI,1:nJ,1:nK,iBlock)   &
                 * Xyz_DGB(       z_,1:nI,1:nJ,1:nK,iBlock) ) &
                 / State_VGB(iRho,1:nI,1:nJ,1:nK,iBlock)      &
                 / r_GB(1:nI,1:nJ,1:nK,iBlock)
         end do
         ! Divide by nProc so that adding up the processors can work
         LogVar_I(iVarTot) = maxval_grid(Tmp1_GB)/nProc
      case('ux')
         do iBlock = 1, nBlock
            if (Unused_B(iBlock)) CYCLE
            Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) = &
                 State_VGB(iRhoUx,1:nI,1:nJ,1:nK,iBlock) / &
                 State_VGB(iRho,1:nI,1:nJ,1:nK,iBlock)
         end do
         LogVar_I(iVarTot) = integrate_grid(Tmp1_GB)/DomainVolume
      case('uy')
         do iBlock = 1, nBlock
            if (Unused_B(iBlock)) CYCLE
            Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) = &
                 State_VGB(iRhoUy,1:nI,1:nJ,1:nK,iBlock) / &
                 State_VGB(iRho,1:nI,1:nJ,1:nK,iBlock)
         end do
         LogVar_I(iVarTot) = integrate_grid(Tmp1_GB)/DomainVolume
      case('uz')
         do iBlock = 1, nBlock
            if (Unused_B(iBlock)) CYCLE
            Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) = &
                 State_VGB(iRhoUz,1:nI,1:nJ,1:nK,iBlock) / &
                 State_VGB(iRho,1:nI,1:nJ,1:nK,iBlock)
         end do
         LogVar_I(iVarTot) = integrate_grid(Tmp1_GB)/DomainVolume
      case('pperp')
         do iBlock = 1, nBlock
            if (Unused_B(iBlock)) CYCLE
            Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) = &
                 ( 3*State_VGB(iP,1:nI,1:nJ,1:nK,iBlock) &
                 -State_VGB(iPpar,1:nI,1:nJ,1:nK,iBlock) )/2
         end do
         LogVar_I(iVarTot) = integrate_grid(Tmp1_GB)/DomainVolume
      case('ekinx')
         do iBlock = 1, nBlock
            if (Unused_B(iBlock)) CYCLE
            Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) = &
                 State_VGB(iRhoUx,1:nI,1:nJ,1:nK,iBlock)**2/&
                 State_VGB(iRho,1:nI,1:nJ,1:nK,iBlock)
         end do
         LogVar_I(iVarTot) = 0.5*integrate_grid(Tmp1_GB)/DomainVolume
      case('ekiny')
         do iBlock = 1, nBlock
            if (Unused_B(iBlock)) CYCLE
            Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) = &
                 State_VGB(iRhoUy,1:nI,1:nJ,1:nK,iBlock)**2/&
                 State_VGB(iRho,1:nI,1:nJ,1:nK,iBlock)
         end do
         LogVar_I(iVarTot) = 0.5*integrate_grid(Tmp1_GB)/DomainVolume
      case('ekinz')
         do iBlock = 1, nBlock
            if (Unused_B(iBlock)) CYCLE
            Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) = &
                 State_VGB(iRhoUz,1:nI,1:nJ,1:nK,iBlock)**2/&
                 State_VGB(iRho,1:nI,1:nJ,1:nK,iBlock)
         end do
         LogVar_I(iVarTot) = 0.5*integrate_grid(Tmp1_GB)/DomainVolume
      case('ekin')
         do iBlock = 1, nBlock
            if (Unused_B(iBlock)) CYCLE
            Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) = &
                 (State_VGB(iRhoUx,1:nI,1:nJ,1:nK,iBlock)**2+&
                 State_VGB(iRhoUy,1:nI,1:nJ,1:nK,iBlock)**2+&
                 State_VGB(iRhoUz,1:nI,1:nJ,1:nK,iBlock)**2)&
                 /State_VGB(iRho,1:nI,1:nJ,1:nK,iBlock)
         end do
         LogVar_I(iVarTot) = 0.5*integrate_grid(Tmp1_GB)/DomainVolume
      case('eth')
         do iBlock = 1, nBlock
            if (Unused_B(iBlock)) CYCLE
            Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) &
                 = State_VGB(iP,1:nI,1:nJ,1:nK,iBlock)
         end do
         LogVar_I(iVarTot) &
              = InvGammaMinus1_I(iFluid)*integrate_grid(Tmp1_GB)/DomainVolume
      case('eeth')
         do iBlock = 1, nBlock
            if (Unused_B(iBlock)) CYCLE
            Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) &
                 = State_VGB(Pe_,1:nI,1:nJ,1:nK,iBlock)
         end do
         LogVar_I(iVarTot) &
              = InvGammaElectronMinus1*integrate_grid(Tmp1_GB)/DomainVolume
      case('eb')
         do iBlock = 1, nBlock
            if (Unused_B(iBlock)) CYCLE
            if(UseB0)then
               do k = 1, nK; do j=1, nJ; do i=1, nI
                  Tmp1_GB(i,j,k,iBlock) = &
                    ((State_VGB(Bx_,i,j,k,iBlock) + B0_DGB(1,i,j,k,iBlock))**2&
                    +(State_VGB(By_,i,j,k,iBlock) + B0_DGB(2,i,j,k,iBlock))**2&
                    +(State_VGB(Bz_,i,j,k,iBlock) + B0_DGB(3,i,j,k,iBlock))**2)
               end do; end do; end do
            else
               do k = 1, nK; do j=1, nJ; do i=1, nI
                  Tmp1_GB(i,j,k,iBlock) = &
                       ( State_VGB(Bx_,i,j,k,iBlock)**2 &
                       + State_VGB(By_,i,j,k,iBlock)**2 &
                       + State_VGB(Bz_,i,j,k,iBlock)**2 )
               end do; end do; end do
            end if
         end do
         LogVar_I(iVarTot) = 0.5*integrate_grid(Tmp1_GB)/DomainVolume
      case('egrav')
         do iBlock = 1, nBlock
            if (Unused_B(iBlock)) CYCLE
            Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) &
                 = State_VGB(iRho,1:nI,1:nJ,1:nK,iBlock) &
                 *Gbody/r_GB(1:nI,1:nJ,1:nK,iBlock)
         end do
         LogVar_I(iVarTot) = integrate_grid(Tmp1_GB)/DomainVolume
      case('radcool')
         do iBlock = 1, nBlock
            if (UnUsed_B(iBlock))CYCLE

            iGang = i_gang(iBlock)

            call get_tesi_c(iBlock, TeSi_C)
            do k = 1, nK; do j=1,nJ; do i=1, nI
               call get_radiative_cooling(i, j, k, iBlock, &
                    TeSi_C(i,j,k), RadCooling_C(i,j,k))
               Tmp1_GB(i,j,k,iBlock) = RadCooling_C(i,j,k)
            end do; end do; end do
         end do
         LogVar_I(iVarTot) = integrate_grid(Tmp1_GB)/DomainVolume
      case('jin','jout','jinmax','joutmax')

         if(index(TypeMessagePass,'opt')>0) &
              call stop_mpi('Spherical integral of J requires '// &
              'message passing edges and corners. Fix PARAM.in!')

         ! calculate the total/maximum current either into or out of the body
         do iBlock = 1, nBlock
            if(Unused_B(iBlock))CYCLE

            do k=1,nK; do j=1,nJ; do i=1,nI
               ! Calculate radial current
               call get_current(i,j,k,iBlock,Current_D)
               Tmp1_GB(i,j,k,iBlock) = &
                    sum(Current_D*Xyz_DGB(:,i,j,k,iBlock))/r_GB(i,j,k,iBlock)
            end do; end do; end do
         end do

         call message_pass_cell(Tmp1_GB, nWidthIn=1)

         do iBlock = 1, nBlock
            if(Unused_B(iBlock))CYCLE
            ! now modify tmp1 according to the case we want
            select case(NameLogVar)
            case('jin')
               Tmp1_GB(:,:,:,iBlock) = min(Tmp1_GB(:,:,:,iBlock), 0.0)
            case('jout')
               Tmp1_GB(:,:,:,iBlock) = max(Tmp1_GB(:,:,:,iBlock), 0.0)
            end select
         end do
         select case(NameLogVar)
         case('jin','jout')
            LogVar_I(iVarTot) = &
                 calc_sphere('integrate', 180, rCurrents, Tmp1_GB) &
                 /(4*cPi*rCurrents**2)
         case('jinmax')
            Value = calc_sphere('minval',180,rCurrents,Tmp1_GB)
            if(nProc > 1)call MPI_allreduce(MPI_IN_PLACE, Value, 1, MPI_REAL, &
                 MPI_MIN, iComm, iError)
            ! Divide by nProc so that adding up the processors can work
            LogVar_I(iVarTot) = Value/nProc

         case('joutmax')
            Value = calc_sphere('maxval',180,rCurrents,Tmp1_GB)
            if(nProc > 1)call MPI_allreduce(MPI_IN_PLACE, Value, 1, MPI_REAL, &
                 MPI_MAX, iComm, iError)
            ! Divide by nProc so that adding up the processors can work
            LogVar_I(iVarTot) = Value/nProc

         end select

      case('dst')
         ! Calculate the Biot-Savart formula for the center of the Earth:
         ! B = (1/4 Pi)*integral( (curl B) x R / R**3 dV)
         ! where R is the vector FROM the CURRENT to the FIELD POINT!
         ! Since the field point is at the origin, R = (-x,-y,-z)
         ! Only the Z component is calculated here: (J x R)_z = -J_x*y + Jy*x

         ! Calculate
         do iBlock = 1, nBlock
            if(Unused_B(iBlock))CYCLE
            do k = 1, nK; do j = 1, nJ; do i = 1, nI
               if ( r_GB(i,j,k,iBlock) < rCurrents .or. &
                    Xyz_DGB(x_,i+1,j,k,iBlock) > xMaxBox .or.      &
                    Xyz_DGB(x_,i-1,j,k,iBlock) < xMinBox .or.      &
                    Xyz_DGB(y_,i,j+1,k,iBlock) > yMaxBox .or.      &
                    Xyz_DGB(y_,i,j-1,k,iBlock) < yMinBox .or.      &
                    Xyz_DGB(z_,i,j,k+1,iBlock) > zMaxBox .or.      &
                    Xyz_DGB(z_,i,j,k-1,iBlock) < zMinBox ) then
                  Tmp1_GB(i,j,k,iBlock) = 0.0
                  CYCLE
               end if
               call get_current(i,j,k,iBlock,Current_D)
               Tmp1_GB(i,j,k,iBlock) = ( &
                    -Current_D(x_)*Xyz_DGB(y_,i,j,k,iBlock) &
                    +Current_D(y_)*Xyz_DGB(x_,i,j,k,iBlock) ) &
                    / r_GB(i,j,k,iBlock)**3
            end do; end do; end do
         end do
         ! The /4pi is part of the Biot-Savart formula
         LogVar_I(iVarTot) = integrate_grid(Tmp1_GB) / (4*cPi)

      case('dst_sm')
         ! Calculate the Biot-Savart formula for the center of the Earth:
         ! B = (1/4 Pi)*integral( (curl B) x R / R**3 dV)
         ! where R is the vector FROM the CURRENT to the FIELD POINT!
         ! Since the field point is at the origin, R = (-x,-y,-z)
         ! Only the SM Z component is calculated here:
         !   (J x R)_z = Sum_i GMtoSM_z,i * (curl B x R)_i

         ! Conversion from GM coordinate system to SMG
         Convert_DD = transform_matrix(tSimulation, &
              TypeCoordSystem, 'SMG')

         ! Calculate
         do iBlock = 1, nBlock
            if(Unused_B(iBlock))CYCLE
            do k = 1, nK; do j = 1, nJ; do i = 1, nI
               if ( r_GB(i,j,k,iBlock) < rCurrents .or. &
                    Xyz_DGB(x_,i+1,j,k,iBlock) > xMaxBox .or.      &
                    Xyz_DGB(x_,i-1,j,k,iBlock) < xMinBox .or.      &
                    Xyz_DGB(y_,i,j+1,k,iBlock) > yMaxBox .or.      &
                    Xyz_DGB(y_,i,j-1,k,iBlock) < yMinBox .or.      &
                    Xyz_DGB(z_,i,j,k+1,iBlock) > zMaxBox .or.      &
                    Xyz_DGB(z_,i,j,k-1,iBlock) < zMinBox ) then
                  Tmp1_GB(i,j,k,iBlock)=0.0
                  CYCLE
               end if
               call get_current(i,j,k,iBlock,Current_D)
               Tmp1_GB(i,j,k,iBlock) = &
                    -sum(Convert_DD(3,:) &
                    *cross_product(Current_D, Xyz_DGB(:,i,j,k,iBlock))) &
                    / r_GB(i,j,k,iBlock)**3
            end do; end do; end do
         end do
         ! The /4pi is part of the Biot-Savart formula
         LogVar_I(iVarTot) = integrate_grid(Tmp1_GB) / (4*cPi)

      case('dstdivb')
         ! Calculate the contribution of Div B to the surface integral of DST
         ! Error = (1/4 Pi)*integral( (div B) R / R**3 dV)
         ! Since the field point is at the origin, R = (-x,-y,-z)
         ! Only the Z component is calculated here: z * div B/R**3

         ! Calculate
         do iBlock = 1, nBlock
            if(Unused_B(iBlock))CYCLE
            do k=0,nK+1; do j=0,nJ+1; do i=0,nI+1
               if ( r_GB(i,j,k,iBlock) < rCurrents .or. &
                    Xyz_DGB(x_,i+1,j,k,iBlock) > xMaxBox .or.      &
                    Xyz_DGB(x_,i-1,j,k,iBlock) < xMinBox .or.      &
                    Xyz_DGB(y_,i,j+1,k,iBlock) > yMaxBox .or.      &
                    Xyz_DGB(y_,i,j-1,k,iBlock) < yMinBox .or.      &
                    Xyz_DGB(z_,i,j,k+1,iBlock) > zMaxBox .or.      &
                    Xyz_DGB(z_,i,j,k-1,iBlock) < zMinBox ) then
                  Tmp1_GB(i,j,k,iBlock)=0.0
                  CYCLE
               end if
               Tmp1_GB(i,j,k,iBlock) = &
                    Xyz_DGB(z_,i,j,k,iBlock) * DivB1_GB(i,j,k,iBlock) &
                    / r_GB(i,j,k,iBlock)**3
            end do; end do; end do
         end do
         ! The 4*pi is part of the Biot-Savart formula
         LogVar_I(iVarTot) = integrate_grid(Tmp1_GB) / (4*cPi)

         ! MHD variables at iTest, jTest, kTest, iBlockTest, iProcTest
      case('rhopnt')
         if(iProc == iProcTest) &
              LogVar_I(iVarTot) = State_VGB(iRho,iTest,jTest,kTest,iBlockTest)
      case('rhouxpnt')
         if(iProc == iProcTest) LogVar_I(iVarTot) &
              = State_VGB(iRhoUx,iTest,jTest,kTest,iBlockTest)
      case('rhouypnt')
         if(iProc == iProcTest) LogVar_I(iVarTot) &
              = State_VGB(iRhoUy,iTest,jTest,kTest,iBlockTest)
      case('rhouzpnt')
         if(iProc == iProcTest) LogVar_I(iVarTot) &
              = State_VGB(iRhoUz,iTest,jTest,kTest,iBlockTest)
      case('b1xpnt')
         if(iProc == iProcTest) &
              LogVar_I(iVarTot) = State_VGB(Bx_,iTest,jTest,kTest,iBlockTest)
      case('b1ypnt')
         if(iProc == iProcTest) &
              LogVar_I(iVarTot) = State_VGB(By_,iTest,jTest,kTest,iBlockTest)
      case('b1zpnt')
         if(iProc == iProcTest) &
              LogVar_I(iVarTot) = State_VGB(Bz_,iTest,jTest,kTest,iBlockTest)
      case('bxpnt')
         if(iProc == iProcTest)then
            if(UseB0)then
               LogVar_I(iVarTot) = &
                    B0_DGB(x_,iTest,jTest,kTest,iBlockTest) + &
                    State_VGB(Bx_,iTest,jTest,kTest,iBlockTest)
            else
               LogVar_I(iVarTot) = &
                    State_VGB(Bx_,iTest,jTest,kTest,iBlockTest)
            end if
         end if
      case('bypnt')
         if(iProc == iProcTest) then
            if(UseB0)then
               LogVar_I(iVarTot) = &
                    B0_DGB(y_,iTest,jTest,kTest,iBlockTest) + &
                    State_VGB(By_,iTest,jTest,kTest,iBlockTest)
            else
               LogVar_I(iVarTot) = &
                    State_VGB(By_,iTest,jTest,kTest,iBlockTest)
            end if
         end if
      case('bzpnt')
         if(iProc == iProcTest) then
            if(UseB0)then
               LogVar_I(iVarTot) = &
                    B0_DGB(z_,iTest,jTest,kTest,iBlockTest) + &
                    State_VGB(Bz_,iTest,jTest,kTest,iBlockTest)
            else
               LogVar_I(iVarTot) = &
                    State_VGB(Bz_,iTest,jTest,kTest,iBlockTest)
            end if
         end if
      case('ppnt')
         if(iProc == iProcTest) &
              LogVar_I(iVarTot) = State_VGB(iP,iTest,jTest,kTest,iBlockTest)
      case('tpnt', 'temppnt')
         if(iProc == iProcTest) LogVar_I(iVarTot) = &
              State_VGB(iP,iTest,jTest,kTest,iBlockTest) &
              /(1 + ElectronPressureRatio) &
              *MassFluid_I(iFluid)/State_VGB(iRho,iTest,jTest,kTest,iBlockTest)
      case('epnt')
         if(iProc == iProcTest) LogVar_I(iVarTot) = &
              energy_i(State_VGB(:,iTest,jTest,kTest,iBlockTest), iFluid)
      case('uxpnt')
         if(iProc == iProcTest) LogVar_I(iVarTot) = &
              State_VGB(iRhoUx,iTest,jTest,kTest,iBlockTest) / &
              State_VGB(iRho,  iTest,jTest,kTest,iBlockTest)
      case('uypnt')
         if(iProc == iProcTest) LogVar_I(iVarTot) = &
              State_VGB(iRhoUy,iTest,jTest,kTest,iBlockTest) / &
              State_VGB(iRho,  iTest,jTest,kTest,iBlockTest)
      case('uzpnt')
         if(iProc == iProcTest) LogVar_I(iVarTot) = &
              State_VGB(iRhoUz,iTest,jTest,kTest,iBlockTest) / &
              State_VGB(iRho,  iTest,jTest,kTest,iBlockTest)
         ! RAYTRACE variables averaged over volume
      case('lon1', 'lon2', 'lat1', 'lat2', 'status', &
           'theta1', 'theta2', 'phi1', 'phi2') ! backward compatible
         select case(NameLogvar)
         case('lat1', 'theta1')
            i = 1; j = 1
         case('lat2', 'theta2')
            i = 1; j = 2
         case('lon1', 'phi1')
            i = 2; j = 1
         case('lon2', 'phi2')
            i = 2; j = 2
         case('status')
            i = 3; j = 1
         end select
         do iBlock = 1, nBlock
            if(Unused_B(iBlock)) CYCLE
            Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) &
                 = Trace_DSNB(i,j,1:nI,1:nJ,1:nK,iBlock)
         end do
         LogVar_I(iVarTot) = integrate_grid(Tmp1_GB)/DomainVolume
         ! RAYTRACE variables at iTest, jTest, kTest, iBlockTest, iProcTest
      case('lat1pnt', 'theta1pnt') ! theta1pnt is backward compatible
         if(iProc == iProcTest) &
              LogVar_I(iVarTot) = Trace_DSNB(1,1,iTest,jTest,kTest,iBlockTest)
      case('lat2pnt', 'theta2pnt')
         if(iProc == iProcTest) &
              LogVar_I(iVarTot) = Trace_DSNB(1,2,iTest,jTest,kTest,iBlockTest)
      case('lon1pnt', 'phi1pnt')
         if(iProc == iProcTest) &
              LogVar_I(iVarTot) = Trace_DSNB(2,1,iTest,jTest,kTest,iBlockTest)
      case('lon2pnt', 'phi2pnt')
         if(iProc == iProcTest) &
              LogVar_I(iVarTot) = Trace_DSNB(2,2,iTest,jTest,kTest,iBlockTest)
      case('statuspnt')
         if(iProc == iProcTest) &
              LogVar_I(iVarTot) = Trace_DSNB(3,1,iTest,jTest,kTest,iBlockTest)
      case('cpcpn','cpcp_n','cpcp_north','cpcpnorth',&
           'cpcps','cpcp_s','cpcp_south','cpcpsouth')
         ! It is sufficient to calculate it on processor 0
         if(iProc == 0) &
              LogVar_I(iVarTot) = logvar_ionosphere(NameLogVar)
      case('ratioyoung')
         if(iProc == 0) &
              LogVar_I(iVarTot) = ratioOH
         ! Flux values integrated over the surface of a sphere
      case('aflx')
         ! just to check that the area is being computed correctly
         iVarTot = iVarTot - 1
         do iR=1,nLogR
            iVarTot = iVarTot + 1
            r = LogR_I(iR)
            Tmp1_GB(0:nI+1,0:nJ+1,0:nK+1,1:nBlock) = 1.0
            LogVar_I(iVarTot) = calc_sphere('integrate',50, r, Tmp1_GB)
         end do
      case('rhoflx')
         ! rho*U_R
         iVarTot = iVarTot - 1
         do iR=1,nLogR
            iVarTot = iVarTot + 1
            r = LogR_I(iR)
            do iBlock = 1, nBlock
               if(Unused_B(iBlock)) CYCLE
               do k=0,nK+1; do j=0,nJ+1; do i=0,nI+1
                  Tmp1_GB(i,j,k,iBlock) = &
                       sum(State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock) &
                       *Xyz_DGB(:,i,j,k,iBlock)) &
                       /r_GB(i,j,k,iBlock)
               end do; end do; end do
            end do
            LogVar_I(iVarTot) = calc_sphere('integrate',360, r, Tmp1_GB)
         end do
      case('dstflx')
         ! B1z
         iVarTot = iVarTot - 1
         do iR=1,nLogR
            iVarTot = iVarTot + 1
            r = LogR_I(iR)
            Tmp1_GB(0:nI+1,0:nJ+1,0:nK+1,1:nBlock) = &
                 State_VGB(Bz_,0:nI+1,0:nJ+1,0:nK+1,1:nBlock)
            LogVar_I(iVarTot) = calc_sphere('integrate',180, r, Tmp1_GB) / &
                 (4*cPi*r**2)
         end do
      case('bflx')
         ! B_R
         iVarTot = iVarTot - 1
         do iR=1,nLogR
            iVarTot = iVarTot + 1
            r = LogR_I(iR)
            do iBlock = 1, nBlock
               if(Unused_B(iBlock)) CYCLE
               FullB_DG=State_VGB(Bx_:Bz_,0:nI+1,0:nJ+1,0:nK+1,iBlock)
               if(UseB0)FullB_DG = FullB_DG &
                    +B0_DGB(:,0:nI+1,0:nJ+1,0:nK+1,iBlock)
               do k=0,nK+1; do j=0,nJ+1; do i=0,nI+1
                  Tmp1_GB(i,j,k,iBlock) = &
                       sum(FullB_DG(:,i,j,k)*Xyz_DGB(:,i,j,k,iBlock)) &
                       / r_GB(i,j,k,iBlock)
               end do; end do; end do
            end do

            LogVar_I(iVarTot) = calc_sphere('integrate',360, r, Tmp1_GB)
         end do
      case('b2flx')
         ! B^2*u_R
         iVarTot = iVarTot - 1
         do iR=1,nLogR
            iVarTot = iVarTot + 1
            r = LogR_I(iR)
            do iBlock = 1, nBlock
               if(Unused_B(iBlock))CYCLE
               FullB_DG = State_VGB(Bx_:Bz_,0:nI+1,0:nJ+1,0:nK+1,iBlock)
               if(UseB0)FullB_DG = FullB_DG &
                    +B0_DGB(:,0:nI+1,0:nJ+1,0:nK+1,iBlock)
               do k=0,nK+1; do j=0,nJ+1; do i=0,nI+1
                  Tmp1_GB(i,j,k,iBlock) = &
                       sum(FullB_DG(:,i,j,k)**2)
                  Tmp1_GB(i,j,k,iBlock) = 0.5*Tmp1_GB(i,j,k,iBlock)* &
                       sum( State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock) &
                       *Xyz_DGB(:,i,j,k,iBlock)) &
                       / (State_VGB(iRho,i,j,k,iBlock)*r_GB(i,j,k,iBlock))
               end do; end do; end do
            end do
            LogVar_I(iVarTot) = calc_sphere('integrate',360, r,Tmp1_GB)
         end do
      case('pvecflx')
         iVarTot = iVarTot - 1
         do iR=1,nLogR
            iVarTot = iVarTot + 1
            r = LogR_I(iR)
            do iBlock = 1, nBlock
               if(Unused_B(iBlock))CYCLE
               FullB_DG=State_VGB(Bx_:Bz_,0:nI+1,0:nJ+1,0:nK+1,iBlock)
               if(UseB0)FullB_DG = FullB_DG &
                    +B0_DGB(:,0:nI+1,0:nJ+1,0:nK+1,iBlock)
               do k=0,nK+1; do j=0,nJ+1; do i=0,nI+1
                  bDotb = sum(FullB_DG(:,i,j,k)**2)
                  bDotU = sum(&
                       FullB_DG(:,i,j,k)*State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock))
                  Tmp1_GB(i,j,k,iBlock) = &
                       sum( (bDotb*State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock) &
                       -     bDotU*FullB_DG(:,i,j,k))*Xyz_DGB(:,i,j,k,iBlock) &
                       ) / (State_VGB(iRho,i,j,k,iBlock)*r_GB(i,j,k,iBlock))
               end do; end do; end do
            end do
            LogVar_I(iVarTot) = calc_sphere('integrate',360, r,Tmp1_GB)
         end do

         ! simple circular Integral_I
      case('e2dflx')
         ! this is the azimuthal component of the electric field
         ! integrated around a circle
         ! Ex*y - Ey*x
         iVarTot = iVarTot-1
         do iR=1,nLogR
            iVarTot = iVarTot+1
            r = LogR_I(iR)
            do iBlock = 1,nBlock
               if(Unused_B(iBlock))CYCLE
               FullB_DG = State_VGB(Bx_:Bz_,0:nI+1,0:nJ+1,0:nK+1,iBlock)
               if(UseB0) &
                    FullB_DG = FullB_DG + B0_DGB(:,0:nI+1,0:nJ+1,0:nK+1,iBlock)
               do k=0,nK+1; do j=0,nJ+1; do i=0,nI+1
                  Bx = FullB_DG(x_,i,j,k)
                  By = FullB_DG(y_,i,j,k)
                  Bz = FullB_DG(z_,i,j,k)
                  RhoUx = State_VGB(iRhoUx,i,j,k,iBlock)
                  RhoUy = State_VGB(iRhoUy,i,j,k,iBlock)
                  RhoUz = State_VGB(iRhoUz,i,j,k,iBlock)
                  Tmp1_GB(i,j,k,iBlock) = &
                       ( (RhoUy*Bz-RhoUz*By)*Xyz_DGB(y_,i,j,k,iBlock) &
                       - (RhoUz*Bx-RhoUx*Bz)*Xyz_DGB(x_,i,j,k,iBlock) &
                       ) / (State_VGB(iRho,i,j,k,iBlock)*r_GB(i,j,k,iBlock))
               end do; end do; end do
            end do
            LogVar_I(iVarTot) = integrate_circle(r, 0.0, Tmp1_GB)
         end do

         ! OTHER VALUES
      case('dt')
         if(iProc == 0)LogVar_I(iVarTot) = dt

      case('cfl')
         if(iProc == 0)LogVar_I(iVarTot) = Cfl

      case('ew', 'erad')
         if(Ew_ == 1)then
            if(UseWavePressure)then
               LogVar_I(iVarTot) = &
                    sum(StateIntegral_V(WaveFirst_:WaveLast_))/DomainVolume
            else
               LogVar_I(iVarTot) = 0.0
            end if
         else
            LogVar_I(iVarTot) = StateIntegral_V(Ew_)/DomainVolume
         end if

      case('totv')
         if(tSimulation <= 0.0)then
            LogVar_I(iVarTot) = 0.0
         else
            do iBlock = 1, nBlock
               if (Unused_B(iBlock)) CYCLE
               Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) = &
                    abs( State_VGB(Rho_,1:nI,1:nJ,1:nK,iBlock) &
                    - StateOld_VGB(Rho_,1:nI,1:nJ,1:nK,iBlock))/Dt
            end do
            LogVar_I(iVarTot) = integrate_grid(Tmp1_GB)
         endif
      case('tv')
         do iBlock = 1, nBlock
            if (Unused_B(iBlock)) CYCLE
            Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) = &
                 abs( State_VGB(Rho_,2:nI+1,1:nJ,1:nK,iBlock) &
                 - State_VGB(Rho_,1:nI,1:nJ,1:nK,iBlock))
            if(nDim > 1) Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) = &
                 Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) + &
                 abs( State_VGB(Rho_,1:nI,2:nJ+1,1:nK,iBlock) &
                 - State_VGB(Rho_,1:nI,1:nJ,1:nK,iBlock))
            if(nDim > 2) Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) = &
                 Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) + &
                 abs( State_VGB(Rho_,1:nI,1:nJ,2:nK+1,iBlock) &
                 - State_VGB(Rho_,1:nI,1:nJ,1:nK,iBlock))
            ! Cancel out the cell volume in the integral
            Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) = Tmp1_GB(1:nI,1:nJ,1:nK,iBlock) &
                 & /CellVolume_GB(1:nI,1:nJ,1:nK,iBlock)
         end do
         LogVar_I(iVarTot) = integrate_grid(Tmp1_GB)
      case default
         ! Check if the variable name is one of the state variables
         NameLogVarLower = NameLogVar_I(iVar)
         call lower_case(NameLogVarLower)

         do jVar = 1, nVar
            if(NameVarLower_V(jVar) /= NameLogVarLower) CYCLE
            LogVar_I(iVarTot) = StateIntegral_V(jVar)/DomainVolume
            RETURN
         end do

         ! Check the user
         if (index(NameLogVar,'flx')>0)then
            iVarTot = iVarTot - 1
            do iR=1,nLogR
               iVarTot = iVarTot + 1
               r = LogR_I(iR)
               call user_get_log_var(LogVar_I(iVarTot), NameLogVarLower, r)
            end do
         else
            call user_get_log_var(LogVar_I(iVarTot), NameLogVarLower)
         end if
      end select

    end subroutine set_log_var
    !==========================================================================
    subroutine set_sat_var

      use ModAdvance, ONLY: UseMultiSpecies
      use ModUtilities, ONLY: lower_case
      use ModIO, ONLY: lNameLogVar

      integer:: jVar
      character(len=lNameLogVar):: NameLogVarLower
      !------------------------------------------------------------------------
      if (iProc/=0) RETURN

      select case(NameLogVar)
      case('rho')
         LogVar_I(iVarTot) = StateSat_V(IRho)
      case('rhoux')
         LogVar_I(iVarTot) = StateSat_V(iRhoUx)
      case('rhouy')
         LogVar_I(iVarTot) = StateSat_V(iRhoUy)
      case('rhouz')
         LogVar_I(iVarTot) = StateSat_V(iRhoUz)
      case('bx')
         LogVar_I(iVarTot) = StateSat_V(Bx_) + B0Sat_D(1)
      case('by')
         LogVar_I(iVarTot) = StateSat_V(By_) + B0Sat_D(2)
      case('bz')
         LogVar_I(iVarTot) = StateSat_V(Bz_) + B0Sat_D(3)
      case('e')
         LogVar_I(iVarTot) = InvGammaMinus1_I(iFluid)*StateSat_V(iP) + 0.5*&
              sum(StateSat_V(iRhoUx:iRhoUz)**2)/StateSat_V(iRho)
         if(iFluid == 1 .and. IsMhd) &
              LogVar_I(iVarTot) = LogVar_I(iVarTot) &
              + 0.5*sum((StateSat_V(Bx_:Bz_) + B0Sat_D)**2)
      case('n','t','temp')
         ! Calculate the number density
         if(UseMultiSpecies)then
            LogVar_I(iVarTot)=0.0
            do jVar = SpeciesFirst_, SpeciesLast_
               LogVar_I(iVarTot) = LogVar_I(iVarTot) + &
                    StateSat_V(jVar)/MassSpecies_V(jVar)
            end do
         else if(UseMultiIon .and. IsMhd .and. iFluid==1)then
            LogVar_I(iVarTot) = sum(StateSat_V(iRhoIon_I)/MassIon_I)
         else
            LogVar_I(iVarTot) = StateSat_V(iRho)/MassFluid_I(iFluid)
         end if

         ! Calculate temperature from P = n*k*T + ne*k*Te = n*k*T*(1+ne/n*Te/T)
         if(NameLogVar /= 'n') LogVar_I(iVarTot) = &
              StateSat_V(iP)/(1 + ElectronPressureRatio)/LogVar_I(iVarTot)
      case('p')
         LogVar_I(iVarTot) = StateSat_V(iP)
      case('pperp')
         LogVar_I(iVarTot) = (3*StateSat_V(iP)-StateSat_V(iPpar))/2.0
      case('ux')
         LogVar_I(iVarTot) = StateSat_V(iRhoUx)/StateSat_V(iRho)
      case('uy')
         LogVar_I(iVarTot) = StateSat_V(iRhoUy)/StateSat_V(iRho)
      case('uz')
         LogVar_I(iVarTot) = StateSat_V(iRhoUz)/StateSat_V(iRho)
      case('b0x')
         LogVar_I(iVarTot) = B0Sat_D(1)
      case('b0y')
         LogVar_I(iVarTot) = B0Sat_D(2)
      case('b0z')
         LogVar_I(iVarTot) = B0Sat_D(3)
      case('b1x')
         LogVar_I(iVarTot) = StateSat_V(Bx_)
      case('b1y')
         LogVar_I(iVarTot) = StateSat_V(By_)
      case('b1z')
         LogVar_I(iVarTot) = StateSat_V(Bz_)
      case('jx')
         LogVar_I(iVarTot) = StateSat_V(nVar+1)
      case('jy')
         LogVar_I(iVarTot) = StateSat_V(nVar+2)
      case('jz')
         LogVar_I(iVarTot) = StateSat_V(nVar+3)
      case('weight')
         ! Total weight of a point (normall 1.0)
         LogVar_I(iVarTot) = StateSat_V(0)
      case('order')
         ! Order of accuracy (2 if total weight is 1.0)
         if(abs(StateSat_V(0)-1)<1.e-5)then
            LogVar_I(iVarTot) = 2
         else
            LogVar_I(iVarTot) = 1
         end if

         ! Raytracing footpoint values
      case('lat1', 'theta1') ! theta1 is backward compatible
         LogVar_I(iVarTot) = SatRayVar_I(1)
      case('lon1', 'phi1')
         LogVar_I(iVarTot) = SatRayVar_I(2)
      case('status')
         LogVar_I(iVarTot) = SatRayVar_I(3)
      case('lat2', 'theta2')
         LogVar_I(iVarTot) = SatRayVar_I(4)
      case('lon2', 'phi2')
         LogVar_I(iVarTot) = SatRayVar_I(5)

      case default
         ! Check if the variable name is one of the state variables
         NameLogVarLower = NameLogVar_I(iVar)
         call lower_case(NameLogVarLower)
         do jVar = 1, nVar
            if(NameVarLower_V(jVar) /= NameLogVarLower) CYCLE
            LogVar_I(iVarTot) = StateSat_V(jVar)
            RETURN
         end do
         LogVar_I(iVarTot) = -777.0
         if(iProc==0)write(*,*)'WARNING in var_sat: unknown variable ',&
              NameLogVar, ' for iSat = ', iSat
      end select

    end subroutine set_sat_var
    !==========================================================================
  end subroutine set_logvar
  !============================================================================
  subroutine normalize_logvar( &
       nLogVar, NameLogVar_I, nLogR, LogR_I, nLogTot, LogVar_I)

    use ModPhysics
    use ModIO, ONLY: lNameLogVar

    integer, intent(in):: nLogVar, nLogR, nLogTot
    character (LEN=lNameLogVar), intent(in):: NameLogVar_I(nLogVar)
    real, intent(inout):: LogVar_I(nLogTot)
    real, intent(in):: LogR_I(nLogR)

    character (len=lNameLogVar):: NameLogVar
    integer:: iVar, iVarTot, jVar

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'normalize_logvar'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    iVarTot = 0
    do iVar = 1, nLogVar

       iVarTot = iVarTot+1
       NameLogVar = NameLogVar_I(iVar)
       call normalize_name_log_var(NameLogVar_I(iVar), NameLogVar)

       select case(NameLogVar)

          ! BASIC MHD variables
       case('rho','rhopnt')
          LogVar_I(iVarTot)= LogVar_I(iVarTot)*No2Io_V(UnitRho_)
       case('rhoux','rhouy','rhouz', 'rhouxpnt','rhouypnt','rhouzpnt')
          LogVar_I(iVarTot)= LogVar_I(iVarTot)*No2Io_V(UnitRhoU_)
       case('bx','by','bz','bxpnt','bypnt','bzpnt','b1xpnt','b1ypnt','b1zpnt',&
            'b1x','b1y','b1z','b0x','b0y','b0z','dst','dstdivb','dst_sm')
          LogVar_I(iVarTot)= LogVar_I(iVarTot)*No2Io_V(UnitB_)
       case('e','epnt','ew','erad','ekinx','ekiny','ekinz','ekin','eth', &
            'eeth','eb','egrav')
          LogVar_I(iVarTot) = LogVar_I(iVarTot)*No2Io_V(UnitEnergyDens_)
       case('p','ppnt','pmin','pmax','pperp')
          LogVar_I(iVarTot) = LogVar_I(iVarTot)*No2Io_V(UnitP_)
       case('ux','uy','uz','uxpnt','uypnt','uzpnt','urmin','urmax')
          LogVar_I(iVarTot)= LogVar_I(iVarTot)*No2Io_V(UnitU_)
       case('jx','jy','jz','jxpnt','jypnt','jzpnt',&
            'jin','jout','jinmax','joutmax')
          LogVar_I(iVarTot)= LogVar_I(iVarTot)*No2Io_V(UnitJ_)
       case('n')
          LogVar_I(iVarTot)=LogVar_I(iVarTot)*No2Io_V(UnitN_)
       case('t','temp','tpnt','temppnt')
          LogVar_I(iVarTot)=LogVar_I(iVarTot)*No2Io_V(UnitTemperature_)
       case('radcool')
          LogVar_I(iVarTot)=LogVar_I(iVarTot)&
               *No2Io_V(UnitEnergyDens_)/No2Io_V(UnitT_)

          ! Ionosphere values

       case('cpcpn','cpcp_n','cpcp_north','cpcpnorth',&
            'cpcps','cpcp_s','cpcp_south','cpcpsouth')
          ! User unit is kV = 1000 V
          LogVar_I(iVarTot) = LogVar_I(iVarTot) &
               *(No2Si_V(UnitElectric_)*No2Si_V(UnitX_))/1000.0
          ! Flux values
       case('aflx')
          LogVar_I(iVarTot:iVarTot+nLogR-1)=LogVar_I(iVarTot:iVarTot+nLogR-1)&
               *No2Io_V(UnitX_)**2
          iVarTot = iVarTot+nLogR-1
       case('rhoflx')
          LogVar_I(iVarTot:iVarTot+nLogR-1)=LogVar_I(iVarTot:iVarTot+nLogR-1)&
               *(No2Si_V(UnitRho_)*No2Si_V(UnitU_)*No2Si_V(UnitX_)**2)
          iVarTot = iVarTot+nLogR-1
       case('dstflx')
          LogVar_I(iVarTot:iVarTot+nLogR-1)=LogVar_I(iVarTot:iVarTot+nLogR-1)&
               *No2Io_V(UnitB_)
          iVarTot = iVarTot+nLogR-1
       case('bflx')
          LogVar_I(iVarTot:iVarTot+nLogR-1)=LogVar_I(iVarTot:iVarTot+nLogR-1)&
               *(No2Si_V(UnitB_)*No2Si_V(UnitX_)**2)
          iVarTot = iVarTot+nLogR-1
       case('b2flx')
          LogVar_I(iVarTot:iVarTot+nLogR-1)=LogVar_I(iVarTot:iVarTot+nLogR-1)&
               *(No2Si_V(UnitPoynting_)*No2Si_V(UnitX_)**2)
          iVarTot = iVarTot+nLogR-1
       case('pvecflx')
          LogVar_I(iVarTot:iVarTot+nLogR-1)=LogVar_I(iVarTot:iVarTot+nLogR-1)&
               *(No2Si_V(UnitPoynting_)*No2Si_V(UnitX_)**2)
          iVarTot = iVarTot+nLogR-1
       case('e2dflx')
          ! circular integral of the azimuthal component of the electric field
          ! on the surface of a sphere.
          LogVar_I(iVarTot:iVarTot+nLogR-1) = &
               LogVar_I(iVarTot:iVarTot+nLogR-1)&
               *No2Si_V(UnitElectric_)*No2Si_V(UnitX_)
          iVarTot = iVarTot+nLogR-1

          ! OTHER VALUES
       case('dt')
          LogVar_I(iVarTot) = LogVar_I(iVarTot)*No2Io_V(UnitT_)

       case default
          do jVar = 1, nVar
             if(NameVarLower_V(jVar) /= NameLogVar) CYCLE
             LogVar_I(iVarTot)=LogVar_I(iVarTot)*UnitUser_V(jVar)
             EXIT
          end do
          ! no normalization
       end select
    end do ! iVar
    call test_stop(NameSub, DoTest)

  end subroutine normalize_logvar
  !============================================================================
  real function calc_sphere(TypeAction, nTheta, Radius, Array_GB)

    ! This function calculates the integral of the incomming variable Array_GB
    ! over the surface of a sphere centered at the origin radius Radius.
    ! The resolution in the colatitude is determined by the nTheta parameter.

    use ModMain, ONLY: TypeMessagePass
    use ModGeometry, ONLY: r_GB, Coord111_DB, TypeGeometry
    use BATL_lib, ONLY: nI, nJ, nK, Unused_B, &
         MinI, MaxI, MinJ, MaxJ, Mink, MaxK, nBlock, MaxBlock, &
         IsCartesianGrid, IsRLonLat, Xyz_DGB, x_, y_, z_, &
         CellSize_DB, Theta_, Phi_, j0_, k0_, nJp1_, nKp1_
    use ModNumConst, ONLY: cRadToDeg, cPi, cTwoPi
    use ModInterpolate, ONLY: trilinear

    ! Arguments

    character(len=*), intent(in):: TypeAction
    integer, intent(in):: nTheta
    real,    intent(in):: Radius
    real,    intent(in):: Array_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)

    ! Local variables
    real:: Result, Darea0, Darea, Average

    ! Indices and coordinates
    integer:: iBlock,i,j,k,i1,i2
    integer:: MaxPhi, nPhi
    real   :: x, y, z, InvDxyz_D(3)
    real   :: Dr
    real   :: xMin, xMax, yMin, yMax, zMin, zMax, rMin, rMax
    real   :: dTheta, dPhi, Phi, Theta, SinTheta

    real:: Array_G(0:nI+1,j0_:nJp1_,k0_:nKp1_)

    ! Store cartesian coordinates for sake of efficiency
    ! The x and y depend on iPhi,iTheta while z only depends on iTheta
    !  real, allocatable:: x_II(:,:), y_II(:,:), z_I(:), SinTheta_I(:)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_sphere'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    call timing_start('calc_sphere_'//TypeAction)

    ! Initialize result
    select case(TypeAction)
    case('integrate')
       Result = 0.0
    case('maxval')
       Result = -Huge(0.0)
    case('minval')
       Result = Huge(0.0)
    case default
       call stop_mpi('ERROR in calc_sphere: Invalid action='//TypeAction)
    end select

    if(IsRLonLat)then
       ! For spherical geometry it is sufficient to
       ! interpolate in the radial direction

       do iBlock = 1, nBlock
          if (Unused_B(iBlock)) CYCLE
          rMin = 0.5*(r_GB( 0,1,1,iBlock) + r_GB( 1, 1, 1,iBlock))
          if(rMin > Radius) CYCLE
          rMax = 0.5*(r_GB(nI,1,1,iBlock) + r_GB(nI+1,1,1,iBlock))
          if(rMax <= Radius) CYCLE

          ! Set temporary array
          Array_G = Array_GB(0:nI+1,j0_:nJp1_,k0_:nKp1_,iBlock)

          dTheta = CellSize_DB(Theta_,iBlock)
          dPhi   = CellSize_DB(Phi_,iBlock)
          dArea0 = Radius**2 *dPhi *dTheta

          ! Find the radial index just after Radius
          i2=0
          do while ( Radius > r_GB(i2,1,1,iBlock))
             i2 = i2 + 1
          end do
          i1 = i2 - 1

          Dr = (Radius - r_GB(i1, 1, 1, iBlock)) &
               /(r_GB(i2, 1, 1, iBlock) - r_GB(i1, 1, 1, iBlock))
          if(Dr<0.or.Dr>1)call stop_mpi('wrong index in calc_sphere')

          select case(TypeAction)
          case('integrate')
             ! Integrate in theta
             do k = 1, nK
                SinTheta = norm2(Xyz_DGB(x_:y_,1,1,k,iBlock)) &
                     / r_GB(1,1,k,iBlock)
                ! Integrate in Phi by adding up 1..nJ and interpolate in R
                Average = Dr * sum( Array_G(i2, 1:nJ, k)) + &
                     (1-Dr)  * sum( Array_G(i1, 1:nJ, k))
                Result = Result + dArea0*SinTheta * Average
             end do
          case('maxval')
             Average = maxval(   Dr * Array_G(i2, 1:nJ, 1:nK)  &
                  +           (1-Dr)* Array_G(i1, 1:nJ, 1:nK))
             Result = max(Average, Result)
          case('minval')
             Average = minval(   Dr * Array_G(i2, 1:nJ, 1:nK)  &
                  +           (1-Dr)* Array_G(i1, 1:nJ, 1:nK))
             Result = min(Average, Result)
          end select
       end do
    elseif(IsCartesianGrid)then
       ! Get the angular resolution from the input parameter nTheta
       MaxPhi = 2*nTheta
       dTheta = cPi/nTheta
       dArea0 = Radius**2 * 2 * sin(0.5*dTheta)

       if (DoTest) write(*,*) 'nTheta,MaxPhi,dTheta[deg]:',nTheta,MaxPhi,&
            dTheta*cRadToDeg

       ! Calculate sin(theta) and x,y,z in advance
       !  allocate( x_II(MaxPhi,nTheta), y_II(MaxPhi, nTheta), z_I(nTheta), &
       !       SinTheta_I(nTheta) )
       !
       !  do i = 1, nTheta
       !     Theta         = (i - 0.5)*dTheta
       !     SinTheta      = sin(Theta)
       !     SinTheta_I(i) = SinTheta
       !     z_I(i)        = Radius*cos(Theta)
       !
       !     ! Number of Phi coordinates is proportional to 2*nTheta*SinTheta
       !     ! This keeps the area of the spherical cells roughly constant
       !     ! and the shape roughly a square.
       !     ! Make sure that nPhi is a multiple of 4 to preserve symmetry
       !     nPhi  = min(MaxPhi, 4 * ceiling(cQuarter*MaxPhi*SinTheta))
       !     Dphi  = cTwoPi/nPhi
       !     do j = 1, nPhi
       !        Phi = j*dPhi
       !        ! Convert to Cartesian coordinates
       !        x_II(j,i) = Radius*SinTheta*cos(Phi)
       !        y_II(j,i) = Radius*SinTheta*sin(Phi)
       !     end do
       !  end do

       ! Sum all cells within range
       do iBlock = 1, nBlock

          if (Unused_B(iBlock)) CYCLE

          ! get the max and min radial distance for this block so that
          ! we can check whether or not this block contibutes to the sum.

          xMin = 0.5*(Xyz_DGB(x_, 0, 0, 0,iBlock) &
               +      Xyz_DGB(x_,   1,   1  , 1,iBlock))
          xMax = 0.5*(Xyz_DGB(x_,nI,nJ,nK,iBlock) &
               +      Xyz_DGB(x_,nI+1,nJ+1,nK+1,iBlock))
          yMin = 0.5*(Xyz_DGB(y_, 0, 0, 0,iBlock) &
               +      Xyz_DGB(y_,   1,   1,   1,iBlock))
          yMax = 0.5*(Xyz_DGB(y_,nI,nJ,nK,iBlock) &
               +      Xyz_DGB(y_,nI+1,nJ+1,nK+1,iBlock))
          zMin = 0.5*(Xyz_DGB(z_, 0, 0, 0,iBlock) &
               +      Xyz_DGB(z_,   1,   1,   1,iBlock))
          zMax = 0.5*(Xyz_DGB(z_,nI,nJ,nK,iBlock) &
               +      Xyz_DGB(z_,nI+1,nJ+1,nK+1,iBlock))

          if( minmod(xMin,xMax)**2+minmod(yMin,yMax)**2+minmod(zMin,zMax)**2 &
               > Radius**2) &
               CYCLE
          if( maxmod(xMin,xMax)**2+maxmod(yMin,yMax)**2+maxmod(zMin,zMax)**2 &
               < Radius**2) &
               CYCLE

          InvDxyz_D = 1 / CellSize_DB(:,iBlock)

          ! Set temporary array
          Array_G = Array_GB(0:nI+1,j0_:nJp1_,k0_:nKp1_,iBlock)

          ! Fill in edges and corners for the first layer so that bilinear
          ! interpolation can be used without message passing these values

          if(index(TypeMessagePass,'opt')>0) &
               call fill_edge_corner(Array_G)

          do i = 1, nTheta

             ! Check if z is inside the block
             Theta = dTheta*(i-0.5)
             z     = Radius*cos(Theta)
             if(z <  zMin) CYCLE
             if(z >= zMax) CYCLE

             SinTheta = sin(Theta)

             ! Number of Phi coordinates is proportional to 2*nTheta*SinTheta
             nPhi = min(MaxPhi, 4 * ceiling(0.25*MaxPhi*SinTheta))
             dPhi = cTwoPi/nPhi

             ! Area of the surface element
             dArea = dArea0 * SinTheta * dPhi

             do j = 1, nPhi
                Phi = j*dPhi

                ! Check if x and y are inside the block
                x = Radius * SinTheta * cos(Phi)
                if(x <  xMin) CYCLE
                if(x >= xMax) CYCLE

                y = Radius * SinTheta * sin(Phi)
                if(y <  yMin) CYCLE
                if(y >= yMax) CYCLE

                ! Compute the interpolated values at the current location.
                ! Coordinates are normalized so that index=coordinate.
                ! XyzStart corresponds to 1,1,1 so we have to add 1
                ! to the index.

                Average = trilinear( Array_G, 0,nI+1, 0,nJ+1, 0,nK+1, &
                     1 + InvDxyz_D*([ x, y, z ] - Coord111_DB(:,iBlock)) )

                select case(TypeAction)
                case('integrate')
                   Result = Result + dArea * Average
                case('maxval')
                   Result = max(Result, Average)
                case('minval')
                   Result = min(Result, Average)
                end select
                ! if(iBlock==222.and.i==22.and.j==76)then
                !   write(*,*)'j, Result=',j, Result
                !   write(*,*)'x,y,z=',x,y,z
                !   write(*,*)'Coord111_DB=',Coord111_DB(:,iBlock)
                !   write(*,*)'InvDxyz_D=',InvDxyz_D
                ! end if
             end do
          end do
       end do
    else
       call stop_mpi('ERROR in calc_sphere: Not implemented for geometry=' &
            //TypeGeometry)
    end if
    ! deallocate(x_II, y_II, z_I, SinTheta_I)

    calc_sphere = Result
    call timing_stop('calc_sphere_'//TypeAction)

    call test_stop(NameSub, DoTest)

  end function calc_sphere
  !============================================================================
  real function integrate_circle(Radius, z, Array_GB)

    ! This function calculates the integral of the incoming variable Array_GB
    ! over a cirle parallel to the equitorial plane.  The radius of the circle
    ! for the z axis is defined by the radius Radius and the z position is
    ! is given by z.

    use ModMain, ONLY: TypeMessagePass
    use ModGeometry, ONLY: Coord111_DB
    use ModNumConst, ONLY: cTwoPi
    use ModInterpolate, ONLY: trilinear
    use BATL_lib, ONLY: nI, nJ, nK, MinI, MaxI, MinJ, MaxJ, Mink, MaxK, &
         nBlock, MaxBlock, Unused_B, Xyz_DGB, x_, y_, z_, CellSize_DB, &
         j0_, nJp1_, k0_, nKp1_

    ! Arguments

    real, intent(in):: Array_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)
    real, intent(in):: Radius, z

    ! Local variables
    real:: Integral, Average

    ! Indices and coordinates
    integer:: iBlock,i
    integer:: nPhi
    real:: xMin, xMax, yMin, yMax, zMin, zMax
    real:: x, y, InvDxyz_D(3)
    real:: dPhi,Phi
    real:: Array_G(0:nI+1,j0_:nJp1_,k0_:nKp1_)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'integrate_circle'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    Integral = 0.0

    ! the angular resolution of the integral is hard coded
    nPhi = 720
    dPhi = cTwoPi/nPhi

    if (DoTest) write(*,*) 'nPhi,dPhi',nPhi,dPhi

    ! Sum all cells within range

    do iBlock = 1, nBlock

       if (Unused_B(iBlock)) CYCLE
       ! get the max and min radial (cylindrical) distance for this block so
       ! that we can check whether or not this block contibutes to the sum.

       zMin=0.5*(Xyz_DGB(z_, 0, 0, 0,iBlock)+Xyz_DGB(z_,   1,   1,   1,iBlock))
       zMax=0.5*(Xyz_DGB(z_,nI,nJ,nK,iBlock)+Xyz_DGB(z_,nI+1,nJ+1,nK+1,iBlock))

       if(z < zMin .or. z >= zMax ) CYCLE

       xMin=0.5*(Xyz_DGB(x_, 0, 0, 0,iBlock)+Xyz_DGB(x_,   1,   1  , 1,iBlock))
       xMax=0.5*(Xyz_DGB(x_,nI,nJ,nK,iBlock)+Xyz_DGB(x_,nI+1,nJ+1,nK+1,iBlock))
       yMin=0.5*(Xyz_DGB(y_, 0, 0, 0,iBlock)+Xyz_DGB(y_,   1,   1,   1,iBlock))
       yMax=0.5*(Xyz_DGB(y_,nI,nJ,nK,iBlock)+Xyz_DGB(y_,nI+1,nJ+1,nK+1,iBlock))

       if( minmod(xMin,xMax)**2 + minmod(yMin,yMax)**2 > Radius**2) CYCLE
       if( maxmod(xMin,xMax)**2 + maxmod(yMin,yMax)**2 < Radius**2) CYCLE

       Array_G = Array_GB(0:nI+1,j0_:nJp1_,k0_:nKp1_,iBlock)

       if(index(TypeMessagePass,'opt')>0) call fill_edge_corner(Array_G)

       InvDxyz_D = 1 / CellSize_DB(:,iBlock)

       do i = 1, nPhi
          Phi = (i-0.5)*dPhi

          ! get the xyz coordinates
          x = Radius*cos(Phi)
          if( x < xMin .or. x >= xMax) CYCLE

          y = Radius*sin(Phi)
          if( y < yMin .or. y >= yMax) CYCLE

          ! Compute the interpolated values at the current location.
          ! Coordinates are normalized so that index=coordinate.
          ! XyzStart corresponds to 1,1,1 so we have to add 1 to the index.

          Average = trilinear( Array_GB(:,:,:,iBlock),0,nI+1,0,nJ+1,0,nK+1, &
               1 + InvDxyz_D*([ x, y, z ] - Coord111_DB(:,iBlock)))

          Integral = Integral + Average
       end do
    end do

    ! Now multiply by the size of each interval in the integral.
    ! Since they are all the same we can do this after the fact
    ! and not inside the above loops

    integrate_circle = Integral*Radius*dPhi

    call test_stop(NameSub, DoTest)
  end function integrate_circle
  !============================================================================
  subroutine collect_satellite_data(Xyz_D, nVar, StateCurrent_V)

    use ModMpi

    real, intent(in):: Xyz_D(3) ! The position of the interpolated state

    ! On input StateCurrent_V contains the weight and the interpolated state
    ! on a given PE.
    ! On output only PE 0 contains new data.
    ! In the first 0th element the total weight is returned.
    ! If weight is 1.0 then the returned state is second order accurate
    ! If weight is positive but not 1.0 then the returned state is first order
    ! If weight is 0.0 (or less?) then the point was not found and the
    !     returned state is -777.0
    ! The rest of the elements contain the globally interpolated state.

    integer, intent(in):: nVar
    real, intent(inout):: StateCurrent_V(0:nVar)

    ! local variables

    real   :: Weight
    integer:: iError

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'collect_satellite_data'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    ! Collect contributions from all the processors to PE 0
    if(nProc > 1)call MPI_reduce_real_array(StateCurrent_V, nVar+1, MPI_SUM, &
         0, iComm, iError)

    ! Check total weight and divide by it if necessary
    if(iProc==0)then
       Weight = StateCurrent_V(0)
       if(Weight <= 0.0)then
          write(*,*) NameSub, ' WARNING total weight =', Weight, &
               ' at Xyz_D=',Xyz_D
          StateCurrent_V = -777.0
       elseif(abs(Weight - 1) > 1.e-5)then
          StateCurrent_V(1:nVar) = StateCurrent_V(1:nVar) / Weight
       end if
    end if

    call test_stop(NameSub, DoTest)

  end subroutine collect_satellite_data
  !============================================================================
  subroutine satellite_test

    use ModVarIndexes
    use ModAdvance, ONLY: State_VGB
    use ModCurrent, ONLY: get_point_data
    use BATL_lib, ONLY: Xyz_DGB, x_, y_, z_, nBlock, iProc
    real:: State_V(0:nVar+3)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'satellite_test'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    State_VGB(Bx_,:,:,:,:) = Xyz_DGB(y_,:,:,:,:)
    State_VGB(By_,:,:,:,:) = Xyz_DGB(z_,:,:,:,:)
    State_VGB(Bz_,:,:,:,:) = Xyz_DGB(x_,:,:,:,:)

    call get_point_data(0.0, [xTest,yTest,zTest], 1, nBlock, 1, nVar+3, &
         State_V)
    call collect_satellite_data([xTest,yTest,zTest], nVar+3, State_V)

    if(iProc==0)then
       if(max(abs(State_V(Bx_)-yTest),abs(State_V(By_)-zTest),&
            abs(State_V(Bz_)-xTest)) > 1e-7)then
          write(*,*)'Satellite state=',State_V(1:nVar)
          write(*,*)'Weight, Difference=', State_V(0), &
               State_V(Bx_)-yTest,State_V(By_)-zTest,State_V(Bz_)-xTest
       else
          write(*,*)'Satellite state is correct'
       end if
       if(maxval(abs(State_V(nVar+1:nVar+3)+1)) > 1e-7)then
          write(*,*)'Satellite current (should be -1)=',State_V(nVar+1:nVar+3)
       else
          write(*,*)'Satellite current is correct'
       end if
    end if

    call test_stop(NameSub, DoTest)

  end subroutine satellite_test
  !============================================================================
  subroutine normalize_name_log_var(NameIn, NameOut)

    ! Normalize the logvar name to lower case and
    ! replace alternative names with a unique name

    use ModUtilities, ONLY: lower_case
    use ModMultiFluid, ONLY: extract_fluid_name

    character(len=*), intent(in) :: NameIn
    character(len=*), intent(out):: NameOut
    integer:: l

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'normalize_name_log_var'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    NameOut = NameIn

    ! Switch to all lower case
    call lower_case(NameOut)

    call extract_fluid_name(NameOut,iFluid)

    ! Replace mx with rhoux, my with rhouy, mz with rhouz
    select case(NameOut(1:2))
    case('mx','my','mz')
       NameOut = 'rhou'//NameOut(2:len(NameOut)-3)
    end select

    ! Replace *test with *pnt
    l = len_trim(NameOut)
    if(l<4) RETURN
    if(NameOut(l-3:l) == 'test') NameOut(l-3:l)='pnt '

    call test_stop(NameSub, DoTest)
  end subroutine normalize_name_log_var
  !============================================================================
  subroutine integrate_domain(Sum_V, Pressure_GB)

    ! Since the State_VGB variable has the variables in the first index,
    ! it is not efficient to integrate the variables one by one.
    ! This subroutine integrates all the state variables at the same time
    ! for the processor only.

    use ModAdvance, ONLY: nVar, State_VGB, P_
    use ModGeometry, ONLY: IsNoBody_B
    use BATL_lib, ONLY: MinI, MaxI, MinJ, MaxJ, Mink, MaxK, &
         nI, nJ, nK, nBlock, MaxBlock, Unused_B, &
         IsCartesian, CellVolume_B, CellVolume_GB, Used_GB

    ! Arguments
    real, intent(out):: Sum_V(nVar)
    real, intent(out):: Pressure_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock)

    ! Local variables:
    integer:: i, j, k, iBlock, iVar
    real   :: CellVolume

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'integrate_domain'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    call timing_start(NameSub)

    Sum_V = 0.0

    if(IsCartesian)then
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          if(IsNoBody_B(iBlock)) then
             do iVar=1,nVar
                Sum_V(iVar) = Sum_V(iVar) + CellVolume_B(iBlock)* &
                     sum(State_VGB(iVar,1:nI,1:nJ,1:nK,iBlock))
             end do
          else
             do iVar=1,nVar
                Sum_V(iVar) = Sum_V(iVar) + CellVolume_B(iBlock)* &
                     sum(State_VGB(iVar,1:nI,1:nJ,1:nK,iBlock),&
                     MASK=Used_GB(1:nI,1:nJ,1:nK,iBlock))
             end do
          end if
          Pressure_GB(1:nI,1:nJ,1:nK,iBlock) = &
               State_VGB(P_,1:nI,1:nJ,1:nK,iBlock)
       end do
    else
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          if(IsNoBody_B(iBlock)) then
             do k=1,nK; do j=1,nJ; do i=1,nI
                CellVolume = CellVolume_GB(i,j,k,iBlock)
                Sum_V = Sum_V + CellVolume_GB(i,j,k,iBlock)* &
                     State_VGB(:,i,j,k,iBlock)
             end do; end do; end do
          else
             do k=1,nK; do j=1,nJ; do i=1,nI
                if(.not.Used_GB(i,j,k,iBlock))CYCLE
                Sum_V = Sum_V + CellVolume_GB(i,j,k,iBlock)* &
                     State_VGB(:,i,j,k,iBlock)
             end do; end do; end do
          end if
          Pressure_GB(1:nI,1:nJ,1:nK,iBlock) = &
               State_VGB(P_,1:nI,1:nJ,1:nK,iBlock)
       end do
    end if

    call timing_stop(NameSub)

    call test_stop(NameSub, DoTest)

  end subroutine integrate_domain
  !============================================================================
  subroutine fill_edge_corner(Array_G)

    ! Fill edges and corners for Array_G (e.g. for bilinear interpolation)

    use BATL_lib, ONLY: nDim, nI, nJ, nK, j0_, nJp1_,k0_, nKp1_

    real, intent(inout):: Array_G(0:nI+1,j0_:nJp1_,k0_:nKp1_)
    !--------------------------------------------------------------------------
    if(nDim == 1) RETURN

    ! Edges in the K direction
    Array_G(0,j0_,:) = &
         Array_G(1,j0_,:)     + Array_G(0,1,:)      - Array_G(1,1,:)
    Array_G(nI+1,j0_,:) = &
         Array_G(nI,j0_,:)    + Array_G(nI+1,1,:)   - Array_G(nI,1,:)
    Array_G(0,nJp1_,:) = &
         Array_G(0,nJ,:)      + Array_G(1,nJp1_,:)  - Array_G(1,nJ,:)
    Array_G(nI+1,nJp1_,:) = &
         Array_G(nI+1,nJ,:)   + Array_G(nI,nJp1_,:) - Array_G(nI,nJ,:)

    if(nDim == 2) RETURN

    ! Edges in the J direction
    Array_G(0,:,k0_) = &
         Array_G(1,:,k0_)     + Array_G(0,:,1)      - Array_G(1,:,1)
    Array_G(nI+1,:,k0_) = &
         Array_G(nI,:,k0_)    + Array_G(nI+1,:,1)   - Array_G(nI,:,1)
    Array_G(0,:,nKp1_) = &
         Array_G(1,:,nKp1_)   + Array_G(0,:,nK)     - Array_G(1,:,nK)
    Array_G(nI+1,:,nKp1_) = &
         Array_G(nI,:,nKp1_)  + Array_G(nI+1,:,nK)  - Array_G(nI,:,nK)

    ! Edges in the I direction including the corners
    Array_G(:,j0_,k0_) = &
         Array_G(:,1,k0_)     + Array_G(:,j0_,1)    - Array_G(:,1,1)
    Array_G(:,nJp1_,k0_) = &
         Array_G(:,nJ,k0_)    + Array_G(:,nJp1_,1)  - Array_G(:,nJ,1)
    Array_G(:,j0_,nKp1_)=&
         Array_G(:,1,nKp1_)   + Array_G(:,j0_,nK)   - Array_G(:,1,nK)
    Array_G(:,nJp1_,nKp1_) = &
         Array_G(:,nJ,nKp1_)  + Array_G(:,nJp1_,nK) - Array_G(:,nJ,nK)

  end subroutine fill_edge_corner
  !============================================================================
end module ModWriteLogSatFile
!==============================================================================
