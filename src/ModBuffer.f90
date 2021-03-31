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
  character (len=3)  :: TypeCoordSource='???'
contains
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

    ! Convert to left handed spherical coordinates Sph_D=(/r,phi,theta/) !!!
    ! What a concept...
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
    use ModPlotFile, ONLY: save_plot_file
    use ModNumConst,   ONLY: cDegToRad
    use ModAdvance,    ONLY: UseElectronPressure, UseAnisoPressure
    use ModVarIndexes, ONLY: nVar, Rho_, Ux_, Uz_, Bx_, Bz_, p_,             &
         WaveFirst_, WaveLast_, Pe_, Ppar_, Ehot_, ChargeStateFirst_,        &
         ChargeStateLast_
    use ModIO,            ONLY: NamePrimitiveVarOrig
    use ModTimeConvert,   ONLY: time_real_to_int
    use ModMain,          ONLY: StartTime, Time_Simulation, x_, y_, z_, n_step
    use ModPhysics,    ONLY: No2Si_V, UnitRho_, UnitU_, UnitB_, UnitX_,      &
         UnitP_, UnitEnergyDens_
    use BATL_lib,     ONLY: iProc

    implicit none

    integer, intent(in)::iFile
    integer:: iTimePlot_I(7), iR, iLon, iLat
    real   :: R, Lat, Lon, CosLat, SinLat
    real, allocatable,dimension(:,:,:):: State_VII, Coord_DII
    character(LEN=30)::NameFile
    !--------------------------------------------------------------------------
    if(iProc/=0)RETURN ! May be improved.

    allocate(State_VII(3 + nVar, 0:nLonBuff, 0:nLatBuff))
    allocate(Coord_DII(2, 0:nLonBuff, 0:nLatBuff))
    call time_real_to_int(StartTime + Time_Simulation, iTimePlot_I)
    do iR = 1,2
       R = BufferMin_D(BuffR_)*(2 - iR) + BufferMax_D(BuffR_)*(iR - 1)
       write(NameFile,'(a,i2.2,a,i4.4,a,5(i2.2,a))')'R=',nint(R),'Rs_',&
            iTimePlot_I(1),'_',iTimePlot_I(2),'_',iTimePlot_I(3),'_',&
            iTimePlot_I(4),'_',iTimePlot_I(5),'_',iTimePlot_I(6),'.out'
       do iLat = 0, nLatBuff
          Lat = -89.999 + (179.999/nLatBuff)*iLat
          Coord_DII(2,:,iLat) = Lat
          CosLat = cos(Lat*cDegToRad)
          SinLat = sin(Lat*cDegToRad)
          do iLon = 0, nLonBuff
             Lon = (360.0/nLonBuff)*iLon
             Coord_DII( 1,iLon,iLat) = Lon
             State_VII(x_,iLon,iLat) = R*CosLat*cos(Lon*cDegToRad)
             State_VII(y_,iLon,iLat) = R*CosLat*sin(Lon*cDegToRad)
             State_VII(z_,iLon,iLat) = R*SinLat
             call get_from_spher_buffer_grid(       &
                  State_VII(x_:z_,iLon,iLat), nVar, &
                  State_VII(z_+1:z_+nVar,iLon,iLat))

          end do
       end do
       ! Convert from normalized units to SI
       State_VII(  x_:z_,:,:) = State_VII(  x_:z_,:,:)*No2Si_V(UnitX_)
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

       call save_plot_file(NameFile,&
            StringHeaderIn=&
            'SC-IH interface, longitude and latitude are in deg, other in SI',&
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
end module ModBuffer
