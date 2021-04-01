!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModBuffer
  use ModNumConst,  ONLY: cHalfPi, cTwoPi
  use BATL_lib,     ONLY: MaxDim
  implicit none
  save
  ! Named indexes for the spherical buffer
  integer, parameter :: BuffR_  =1, BuffLon_ =  2, BuffLat_ =  3
  integer            :: nRBuff = 2, nLonBuff = 90, nLatBuff = 45
  ! Buffer grid with dimension(nVar, nRBuff, 0:nLonBuff+1, 0:nLatBuff+1)
  real,  allocatable :: BufferState_VG(:,:,:,:)

  real               :: dSphBuff_D(3)
  real               :: BufferMin_D(3) = [ 19.0,    0.0, -cHalfPi]
  real               :: BufferMax_D(3) = [ 21.0, cTwoPi,  cHalfPi]

  real               :: SourceTarget_DD(MaxDim, MaxDim)
  real               :: TimeSimulationLast = -1.0

  logical            :: DoRestartBuffer = .false.
  character (len=3)  :: TypeCoordSource='???'
contains
  !============================================================================
  subroutine read_buffer_grid_param(NameCommand)
    use ModReadParam
    use ModNumConst, ONLY: cDegToRad
    use ModMain,     ONLY: NameThisComp
    character(LEN=*), intent(in) :: NameCommand
    character(len=*), parameter:: NameSub = 'read_buffer_grid_param'
    !--------------------------------------------------------------------------
    select case(NameCommand)
    case("#HELIOBUFFERGRID")
       if(NameThisComp == 'SC') &
            call stop_mpi(NameSub//' ERROR:'// &
            ' #HELIOBUFFERGRID can be used in IH and OH components only')
       call read_var('rBuffMin',   BufferMin_D(BuffR_))
       call read_var('rBuffMax',   BufferMax_D(BuffR_))
       call read_var('nLonBuff',   nLonBuff)
       call read_var('nLatBuff',   nLatBuff)
       nRBuff = 2
       BufferMin_D(BuffLon_:BuffLat_) = [0.0   , -cHalfPi]
       BufferMax_D(BuffLon_:BuffLat_) = [cTwoPi,  cHalfPi]
       call set_buffer_grid
    case("#BUFFERGRID")
       call read_var('nRBuff'    ,  nRBuff)
       call read_var('nLonBuff'  ,  nLonBuff)
       call read_var('nLatBuff'  ,  nLatBuff)
       call read_var('rBuffMin'  ,  BufferMin_D(BuffR_))
       call read_var('rBuffMax'  ,  BufferMax_D(BuffR_))
       call read_var('LonBuffMin',  BufferMin_D(BuffLon_))
       call read_var('LonBuffMax',  BufferMax_D(BuffLon_))
       call read_var('LatBuffMin',  BufferMin_D(BuffLat_))
       call read_var('LatBuffMax',  BufferMax_D(BuffLat_))

       ! Convert degrees to radians, latitude to co-latitude
       BufferMin_D(BuffLon_:BuffLat_) = BufferMin_D(BuffLon_:BuffLat_)&
            *cDegToRad
       BufferMax_D(BuffLon_:BuffLat_) = BufferMax_D(BuffLon_:BuffLat_)&
            *cDegToRad
       call set_buffer_grid
    case("#RESTARTBUFFERGRID")
       call read_var('DoRestartBuffer', DoRestartBuffer)
    end select
  end subroutine read_buffer_grid_param
  !============================================================================
  subroutine set_buffer_grid
    use ModVarIndexes, ONLY: nVar
    integer  :: nCell_D(3)
    !--------------------------------------------------------------------------
    if(allocated(BufferState_VG))deallocate(BufferState_VG)
    allocate(BufferState_VG(nVar, nRBuff, 0:nLonBuff+1, 0:nLatBuff+1))
    ! Calculate grid spacing and save
    nCell_D = [nRBuff, nLonBuff, nLatBuff]
    dSphBuff_D = (BufferMax_D - BufferMin_D)/real(nCell_D)
    dSphBuff_D(BuffR_) = (BufferMax_D(BuffR_) - BufferMin_D(BuffR_)) &
         /real(nRBuff - 1)
  end subroutine set_buffer_grid
  !============================================================================
  subroutine get_from_spher_buffer_grid(XyzTarget_D, nVar, State_V)
    use ModMain,       ONLY: TypeCoordSystem, Time_Simulation, &
         DoThinCurrentSheet
    use CON_axes,      ONLY: transform_matrix, transform_velocity
    use ModAdvance,    ONLY: UseB
    use ModWaves,      ONLY: UseAlfvenWaves
    use ModPhysics,    ONLY: No2Si_V, Si2No_V, UnitU_, UnitX_
    use ModVarIndexes, ONLY: Ux_, Uz_, RhoUx_, RhoUz_, SignB_,  Rho_,&
         WaveFirst_, WaveLast_, Bx_, Bz_
    use ModCoordTransform, ONLY: xyz_to_rlonlat
    integer,intent(in) :: nVar
    real,   intent(in) :: XyzTarget_D(MaxDim)
    real,   intent(out):: State_V(nVar)

    real              :: Sph_D(MaxDim)
    real              :: Ewave
    real              :: XyzSource_D(MaxDim)

    !--------------------------------------------------------------------------
    if(TypeCoordSource /= TypeCoordSystem) then
       ! Convert target coordinates to the coordiante system of the model

       if(Time_Simulation > TimeSimulationLast)then
          SourceTarget_DD = transform_matrix(&
               Time_Simulation, TypeCoordSystem, TypeCoordSource)
          TimeSimulationLast = Time_Simulation
       end if
       XyzSource_D = matmul(SourceTarget_DD, XyzTarget_D)
    else
       XyzSource_D = XyzTarget_D
    end if

    call xyz_to_rlonlat(XyzSource_D, Sph_D(1), Sph_D(2), Sph_D(3))

    ! Get the target state from the spherical buffer grid
    call interpolate_from_global_buffer(Sph_D, nVar, State_V)
    ! Transform vector variables from SC to IH
    if(TypeCoordSource /= TypeCoordSystem)then
       State_V(Ux_:Uz_) = transform_velocity(Time_Simulation,&
            State_V(Ux_:Uz_)*No2Si_V(UnitU_), XyzSource_D * No2Si_V(UnitX_), &
            TypeCoordSource, TypeCoordSystem)*Si2No_V(UnitU_)
       if(UseB) State_V(Bx_:Bz_) = matmul( State_V(Bx_:Bz_), SourceTarget_DD)
    end if
    ! Transform to primitive variables
    State_V(Ux_:Uz_) = State_V(RhoUx_:RhoUz_)/State_V(Rho_)
    if(SignB_>1)then
       if(DoThinCurrentSheet)then
          ! In both IH and OH we have no B0, so we ignore that !
          if(sum(State_V(Bx_:Bz_)*XyzTarget_D) < 0.0)then
             State_V(Bx_:Bz_) = -State_V(Bx_:Bz_)
             if(WaveFirst_ > 1 .and. UseAlfvenWaves)then
                Ewave = State_V(WaveFirst_)
                State_V(WaveFirst_) = State_V(WaveLast_)
                State_V(WaveLast_) = Ewave
             end if

             State_V(SignB_)=-1.0
          else
             State_V(SignB_)= 1.0
          end if
       else
          State_V(SignB_) = 0.0
       end if
    end if
  end subroutine get_from_spher_buffer_grid
  !============================================================================
  subroutine interpolate_from_global_buffer(SphSource_D, nVar, Buffer_V)
    ! DESCRIPTION
    ! This subroutine is used to interpolate from  state variables defined on a
    ! spherical buffer grid into the input point SphSource_D.
    ! The buffer grid overlaps some part of the computational grid of a
    ! source component that is coupled to this component.
    ! The buffer grid  has the same coordinate system as the source component
    ! (but may have a different grid resolution).
    ! It is assumed that the buffer grid was filled with the state vector from
    ! the source component at some earlier stage.

    ! INPUT:
    ! SphSource_D is associated with a point in the target component, and it
    ! is assumed that is was already converted to the source coordinate system.

    ! nVar is the number of state variables used in coupling the two components.

    ! Implicit inputs to this subroutine are the buffer grid size, points
    ! and the state vector at each point (USEd from BATSRUS).

    ! OUTPUT:
    ! Buffer_V is the state vector resulting from the interpolation.

    use ModInterpolate, ONLY: trilinear
    ! Input and output variables
    real,intent(in)    :: SphSource_D(3)
    integer,intent(in) :: nVar
    real,intent(out)   :: Buffer_V(nVar)

    real               :: NormSph_D(3)

    ! Convert to normalized coordinates.
    ! Radial is node centered, theta and phi are cell centered.
    character(len=*), parameter:: NameSub = 'interpolate_from_global_buffer'
    !--------------------------------------------------------------------------
    NormSph_D = (SphSource_D - BufferMin_D)/dSphBuff_D + [ 1.0, 0.5, 0.5 ]

    Buffer_V = trilinear(BufferState_VG, nVar, 1, nRBuff,0, nLonBuff+1, &
         0, nLatBuff+1, NormSph_D, DoExtrapolate=.true.)

  end subroutine interpolate_from_global_buffer
  !============================================================================
  subroutine plot_buffer(iFile)
    use ModPlotFile,   ONLY: save_plot_file
    use ModNumConst,   ONLY: cRadToDeg
    use ModAdvance,    ONLY: UseElectronPressure, UseAnisoPressure
    use ModVarIndexes, ONLY: nVar, Rho_, Ux_, Uz_, RhoUx_, RhoUz_, Bx_, Bz_, &
         p_, WaveFirst_, WaveLast_, Pe_, Ppar_, Ehot_, ChargeStateFirst_,    &
         ChargeStateLast_
    use ModIO,            ONLY: NamePrimitiveVarOrig, NamePlotDir
    use ModTimeConvert,   ONLY: time_real_to_int
    use ModCoordTransform, ONLY: rlonlat_to_xyz
    use ModMain,          ONLY: StartTime, Time_Simulation, x_, z_, n_step
    use ModPhysics,       ONLY: No2Si_V, UnitRho_, UnitU_, UnitB_, UnitX_,   &
         UnitP_, UnitEnergyDens_
    use BATL_lib,     ONLY: iProc
    integer, intent(in):: iFile          ! Unused
    integer            :: iTimePlot_I(7) ! To shape the file name
    integer            :: iR, iLon, iLat ! Coordinate indexes
    real               :: R, Lat, Lon    ! Coords
    ! Xyz and state variables to plot
    real               :: State_VII(3 + nVar, nLonBuff, nLatBuff)
    ! Coords: Longitude, Latitude, in degrees
    real               :: Coord_DII(2, nLonBuff, nLatBuff)
    character(LEN=30)::NameFile
    !--------------------------------------------------------------------------
    if(iProc/=0)RETURN ! May be improved
    ! Convert time to integers:
    call time_real_to_int(StartTime + Time_Simulation, iTimePlot_I)
    ! Independing on nRBuff, plot only two 2D files for spherical surfaces
    ! of radius of BufferMin_D(BuffR_) and BufferMax_D(BuffR_)
    do iR = 1, nRBuff, nRBuff - 1
       R = BufferMin_D(BuffR_) + dSphBuff_D(BuffR_)*(iR - 1)
       ! Shape the file name
       write(NameFile,'(a,i2.2,a,i4.4,a,5(i2.2,a))')'R=',nint(R),'Rs_',&
            iTimePlot_I(1),'_',iTimePlot_I(2),'_',iTimePlot_I(3),'_',&
            iTimePlot_I(4),'_',iTimePlot_I(5),'_',iTimePlot_I(6),'.out'
       do iLat = 1, nLatBuff
          Lat = BufferMin_D(BuffLat_) + dSphBuff_D(BuffLat_)*(iLat - 0.50)
          Coord_DII(2,:,iLat) = Lat*cRadToDeg
          do iLon = 1, nLonBuff
             Lon = BufferMin_D(BuffLon_) + dSphBuff_D(BuffLon_)*(iLon  - 0.50)
             Coord_DII( 1,iLon,iLat) = Lon*cRadToDeg
             ! Save Cartesian coordinates
             call rlonlat_to_xyz(R, Lon, Lat, State_VII(x_:z_,iLon,iLat))
             ! Save Buffer state vector
             State_VII(z_+1:z_+nVar, iLon, iLat) = &
                  BufferState_VG(:, iR, iLon, iLat)
             ! Transform to primitive variables:
             State_VII(z_+Ux_:z_+Uz_, iLon, iLat) = &
                  State_VII(z_+RhoUx_:z_+RhoUz_, iLon, iLat)/&
                  State_VII(z_+Rho_, iLon, iLat)
          end do
       end do
       ! Convert from normalized units to SI
       State_VII(  x_:z_,:,:) = State_VII(  x_:z_,:,:)*No2Si_V(UnitX_  )
       State_VII(z_+rho_,:,:) = State_VII(z_+rho_,:,:)*No2Si_V(UnitRho_)
       State_VII(z_+Ux_:z_+Uz_,:,:)   = State_VII(z_+Ux_:z_+Uz_,:,:)*&
            No2Si_V(UnitU_)
       State_VII(z_+Bx_:z_+Bz_,:,:)   = State_VII(z_+Bx_:z_+Bz_,:,:)*&
            No2Si_V(UnitB_)

       if(WaveFirst_ > 1)State_VII(z_+WaveFirst_:z_+WaveLast_,:,:) = &
            State_VII(z_+WaveFirst_:z_+WaveLast_,:,:)* &
            No2Si_V(UnitEnergyDens_)

       if(ChargeStateFirst_ > 1)&
            State_VII(z_+ChargeStateFirst_:z_+ChargeStateLast_,:,:) = &
            State_VII(z_+ChargeStateFirst_:z_+ChargeStateLast_,:,:)* &
            No2Si_V(UnitRho_)

       State_VII(z_+p_,:,:)  = State_VII(z_+p_,:,:)*No2Si_V(UnitP_)
       if(UseElectronPressure)State_VII(z_+Pe_,:,:)  = &
            State_VII(z_+Pe_,:,:)*No2Si_V(UnitP_)

       if(UseAnisoPressure)State_VII(z_+Ppar_,:,:)  = &
            State_VII(z_+Ppar_,:,:)*No2Si_V(UnitP_)

       if(Ehot_>1)State_VII(z_+Ehot_,:,:) = &
            State_VII(z_+Ehot_,:,:)*No2Si_V(UnitEnergyDens_)

       call save_plot_file(trim(NamePlotDir)//NameFile,&
            StringHeaderIn=&
            'SC-IH interface: longitude and latitude are in deg, other in SI',&
            NameVarIn    = &
            'Long Lat x y z '//NamePrimitiveVarOrig//' R',&
            nDimIn=2,      &
            nStepIn=n_step, TimeIn=Time_Simulation,&
            ParamIn_I=[R*No2Si_V(UnitX_)], &
            CoordIn_DII=Coord_DII, &
            VarIn_VII=State_VII)
    end do
  end subroutine plot_buffer
  !============================================================================
  subroutine  save_buffer_restart
    use ModMain,       ONLY: NameThisComp
    use ModIoUnit,     ONLY: UnitTmp_
    use ModUtilities,  ONLY: open_file, close_file
    character(len=*), parameter:: NameSub = 'save_buffer_restart'
    !--------------------------------------------------------------------------
    call open_file(file=NameThisComp//'/restartOUT/buffer.dat', &
         form='UNFORMATTED',    &
         NameCaller=NameSub)
    write(UnitTmp_,*)BufferState_VG
    call close_file
    !--------------------------------------------------------------------------
  end subroutine save_buffer_restart
  !============================================================================
  subroutine  read_buffer_restart
    use ModMain,       ONLY: NameThisComp
    use ModIoUnit,     ONLY: UnitTmp_
    use ModUtilities,  ONLY: open_file, close_file
    character(len=*), parameter:: NameSub = 'read_buffer_restart'
    !--------------------------------------------------------------------------
    call open_file(file=NameThisComp//'/restartIN/buffer.dat', &
         status='old', form='UNFORMATTED', NameCaller=NameSub)
    read(UnitTmp_,*)BufferState_VG
    call close_file
    !--------------------------------------------------------------------------
  end subroutine read_buffer_restart
  !============================================================================
end module ModBuffer
!==============================================================================
