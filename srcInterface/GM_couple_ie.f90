!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
!^CMP FILE IE

module GM_couple_ie

  use BATL_lib, ONLY: iProc
  use ModUtilities, ONLY: CON_set_do_test
  use ModBatsrusUtility, ONLY: stop_mpi
  use ModIeCoupling, ONLY: rIonosphere, &
       nThetaIono, nPhiIono, ThetaIono_I, PhiIono_I,&
       IonoPotential_II, IonoJouleHeating_II, &
       SigmaHall_II, SigmaPedersen_II, HallJ_DII, PedersenJ_DII, &
       init_ie_grid, calc_grad_ie_potential, map_jouleheating_to_inner_bc

  implicit none
  save

  private ! except

  public:: GM_get_for_ie
  public:: GM_get_info_for_ie
  public:: GM_put_from_ie

contains
  !============================================================================
  subroutine GM_get_info_for_ie(nVarIeGm, nVarGmIe, NameVar_I)

    use ModMain, ONLY: TypeFaceBc_I, body1_
    use ModGroundMagPerturb, ONLY: nMagTotal
    use ModAdvance, ONLY: UseElectronPressure, UseAnisoPressure, &
                                   UseAnisoPe

    ! Pass number and names of variables requested from IE

    integer, intent(out) :: nVarIeGm
    integer, intent(out), optional :: nVarGmIe
    character(len=*), intent(out), optional :: NameVar_I(:)

    logical :: DoTest, DoTestMe
    character(len=*), parameter:: NameSub = 'GM_get_info_for_ie'
    !--------------------------------------------------------------------------
    call CON_set_do_test(NameSub, DoTest, DoTestMe)

    ! Always pass electric potential
    nVarIeGm = 1
    if(present(NameVar_I)) NameVar_I(nVarIeGm) = 'potential'

    ! Joule heating is needed for empirical inner boundary
    if(TypeFaceBc_I(body1_) == 'ionosphereoutflow')then
        nVarIeGm = nVarIeGm+1
       if(present(NameVar_I)) NameVar_I(nVarIeGm) = 'jouleheat'
    end if

    ! Conductances are needed for magnetometer calculations
    if(nMagTotal > 0)then
        nVarIeGm = nVarIeGm + 1
       if(present(NameVar_I)) NameVar_I(nVarIeGm) = 'sigmahall'
        nVarIeGm = nVarIeGm + 1
       if(present(NameVar_I)) NameVar_I(nVarIeGm) = 'sigmapedersen'
    end if

    ! Standard 6 variable tracing: FAC, 1/b, rho, p, dlat, dlon
    if(present(nVarGmIe)) then
        nVarGmIe = 6
        ! Add additional space for Pe, Ppar, Pepar if turned on
        if (UseElectronPressure) nVarGmIe = nVarGmIe + 1
        if (UseAnisoPressure) nVarGmIe = nVarGmIe + 1
        if (UseAnisoPe) nVarGmIe = nVarGmIe + 1
    end if

    if(DoTestMe)then
       write(*,*) NameSub//' set nVarIeGm=', nVarIeGm
       if(present(NameVar_I)) write(*,*) NameSub//' set NameVar_I=', &
            NameVar_I
    end if

  end subroutine GM_get_info_for_ie
  !============================================================================
  subroutine GM_get_for_ie(Buffer_IIV, iSize, jSize, nVar)

    ! Send the following information from GM to IE on the IE grid:
    !  1. radial component of the field-aligned-currents (FACs)
    !  2. field line tracing information if DoTraceIE is true

    use CON_axes, ONLY: transform_matrix
    use ModMain, ONLY: tSimulation, TypeCoordSystem
    use ModCurrent, ONLY: calc_field_aligned_current
    use ModFieldTrace, ONLY: DoTraceIE, RayResult_VII, RayIntegral_VII, &
         InvB_, RhoInvB_, pInvB_, iXEnd, iYEnd, iZEnd, iPeInvB, CLOSEDRAY, &
         GmSm_DD, integrate_field_from_sphere
    use ModNumConst, ONLY: cRadToDeg
    use ModPhysics, ONLY: No2Si_V, UnitX_, UnitP_, UnitRho_, UnitB_, UnitJ_
    use ModCoordTransform, ONLY: xyz_to_sph
    use ModAdvance, ONLY: State_VGB, UseElectronPressure
    use ModB0, ONLY: B0_DGB
    use ModUpdateStateFast, ONLY: sync_cpu_gpu
    use CON_coupler, ONLY: Grid_C, IE_

    integer, intent(in) :: iSize, jSize, nVar
    real,    intent(out):: Buffer_IIV(iSize,jSize,nVar)

    integer:: i, j, iVar
    real:: Radius
    real, allocatable:: FieldAlignedCurrent_II(:,:)
    real, allocatable:: IeLat_I(:), IeLon_I(:)
    real:: XyzIono_D(3), RtpIono_D(3), Lat,Lon, dLat,dLon
    logical:: DoTest, DoTestMe

    character(len=*), parameter:: NameSub = 'GM_get_for_ie'
    !--------------------------------------------------------------------------
    call CON_set_do_test(NameSub, DoTest, DoTestMe)
    if(DoTest)write(*,*)NameSub,': iSize,jSize,nVar,DoTraceIe=', &
         iSize, jSize, nVar, DoTraceIe

    call sync_cpu_gpu('update on GPU', NameSub, State_VGB, B0_DGB)

    if(nThetaIono < 1) call init_ie_grid( &
         Grid_C(IE_) % Coord1_I, &
         Grid_C(IE_) % Coord2_I, &
         Grid_C(IE_) % Coord3_I(1))

    allocate(FieldAlignedCurrent_II(iSize,jSize))

    ! Put the radial component of the field aligned currents
    ! into the first variable of the buffer
    call calc_field_aligned_current(nThetaIono, nPhiIono, rIonosphere, &
         FieldAlignedCurrent_II, &
         Theta_I=ThetaIono_I, Phi_I=PhiIono_I, IsRadial=.true.)
    if(iProc == 0)then
       ! initialize all elements to zero on proc 0, others should not use it
       Buffer_IIV = 0.0

       ! Save the latitude boundary information to the equator
       Buffer_IIV(:,:,1) = FieldAlignedCurrent_II(:,:)*No2Si_V(UnitJ_)
       if(DoTest)write(*,*)NameSub, ': sum(FAC**2)=', sum(Buffer_IIV(:,:,1)**2)
    end if

    deallocate(FieldAlignedCurrent_II)
    if(DoTraceIE) then
       allocate(IeLat_I(iSize), IeLon_I(jSize))

       ! Load grid and convert to lat-lon in degrees
       IeLat_I = 90.0 - cRadToDeg * ThetaIono_I(1:iSize)
       IeLon_I =        cRadToDeg * PhiIono_I
       Radius = (6378.+100.)/6378.
       if(UseElectronPressure) then
           call integrate_field_from_sphere( &
                iSize, jSize, IeLat_I, IeLon_I, Radius, &
                'InvB,RhoInvB,pInvB,Z0x,Z0y,Z0b,PeInvB')
       else
           call integrate_field_from_sphere( &
                iSize, jSize, IeLat_I, IeLon_I, Radius, &
                'InvB,RhoInvB,pInvB,Z0x,Z0y,Z0b')
       end if

       ! Only processor 0 has the resulting Integral_I,
       !   the others do not participate in the coupling
       if(iProc == 0)then
          where(RayResult_VII(InvB_,:,:) > 0.)
             RayResult_VII(RhoInvB_,:,:) = RayResult_VII(RhoInvB_,:,:) &
                  /RayResult_VII(InvB_,:,:)
             RayResult_VII(pInvB_,:,:)   = RayResult_VII(pInvB_,:,:) &
                  /RayResult_VII(InvB_,:,:)
          end where
          where(RayResult_VII(iXEnd,:,:) <= CLOSEDRAY)
             RayResult_VII(   InvB_,:,:) = -1.
             RayResult_VII(rhoInvB_,:,:) = 0.
             RayResult_VII(  pInvB_,:,:) = 0.
          end where
          Buffer_IIV(:,:,2) = RayResult_VII(   InvB_,:,:) &
               * No2Si_V(UnitX_)/No2Si_V(UnitB_)
          Buffer_IIV(:,:,3) = RayResult_VII(RhoInvB_,:,:) * No2Si_V(UnitRho_)
          Buffer_IIV(:,:,4) = RayResult_VII(  pInvB_,:,:) * No2Si_V(UnitP_)
          iVar = 7
          if(UseElectronPressure) then
              where(RayResult_VII(InvB_,:,:) > 0.)
                  RayResult_VII(iPeInvB,:,:)  = RayResult_VII(iPeInvB,:,:) &
                          /RayResult_VII(InvB_,:,:)
              end where
              where(RayResult_VII(iXEnd,:,:) <= CLOSEDRAY)
                  RayResult_VII(iPeInvB,:,:) = 0.
              end where
              Buffer_IIV(:,:,iVar) = RayResult_VII( iPeInvB,:,:) * &
                      No2Si_V(UnitP_)
              iVar = iVar + 1
          end if

          ! Transformation matrix from default (GM) to SM coordinates
          GmSm_DD = transform_matrix(tSimulation,TypeCoordSystem,'SMG')

          ! Loop to compute deltas
          do j = 1, jSize; do i = 1, iSize
             Lat = -IeLat_I(i)
             Lon =  IeLon_I(j)
             if(RayResult_VII(InvB_,i,j) > 1e-10)then
                XyzIono_D(1) = RayResult_VII(iXEnd,i,j)
                XyzIono_D(2) = RayResult_VII(iYEnd,i,j)
                XyzIono_D(3) = RayResult_VII(iZEnd,i,j)
                XyzIono_D = matmul(GmSm_DD,XyzIono_D)
                call xyz_to_sph(XyzIono_D, RtpIono_D)
                Lat = 90 - RtpIono_D(2)*cRadToDeg
                Lon =      RtpIono_D(3)*cRadToDeg
             end if
             ! Shift to closer to local value, even if outside 0-360
             if( (IeLon_I(j)-Lon) < -180) Lon = Lon - 360
             if( (IeLon_I(j)-Lon) >  180) Lon = Lon + 360

             ! Compute deltas
             dLat = abs(Lat) - abs(IeLat_I(i))
             dLon =     Lon  -     IeLon_I(j)

             ! Put into exchange buffer
             Buffer_IIV(i,j,5) = dLat
             Buffer_IIV(i,j,6) = dLon
          end do; end do
          if(DoTest)then
             iVar=7
             write(*,*)NameSub, ': sum(Buf2**2)=', sum(Buffer_IIV(:,:,2)**2)
             write(*,*)NameSub, ': sum(Buf3**2)=', sum(Buffer_IIV(:,:,3)**2)
             write(*,*)NameSub, ': sum(Buf4**2)=', sum(Buffer_IIV(:,:,4)**2)
             write(*,*)NameSub, ': sum(Buf5**2)=', sum(Buffer_IIV(:,:,5)**2)
             write(*,*)NameSub, ': sum(Buf6**2)=', sum(Buffer_IIV(:,:,6)**2)
             if(UseElectronPressure)then
                 write(*,*)NameSub, ': sum(Buf7**2)=', &
                         sum(Buffer_IIV(:,:,iVar)**2)
             end if
          end if
       end if
       deallocate(IeLat_I, IeLon_I, RayIntegral_VII, RayResult_VII)
    end if

    if(DoTest)write(*,*)NameSub,' finished'

  end subroutine GM_get_for_ie
  !============================================================================
  subroutine GM_put_from_ie(Buffer_IIV, iSize, jSize, nVar, NameVar_I)

    ! Receive nVar variables listed in NameVar_I from IE on the IE grid
    ! and store them into the allocatable arrays in ModIeCoupling

    use ModPhysics, ONLY: &
         Si2No_V, UnitX_, UnitElectric_, UnitPoynting_, UnitJ_
    use CON_coupler, ONLY: Grid_C, IE_

    integer, intent(in) :: iSize, jSize, nVar
    real,    intent(in) :: Buffer_IIV(iSize,jSize,nVar)
    character(len=*), intent(in):: NameVar_I(nVar)

    integer:: iVar

    logical :: DoTest, DoTestMe
    character(len=*), parameter:: NameSub = 'GM_put_from_ie'
    !--------------------------------------------------------------------------
    call CON_set_do_test(NameSub, DoTest, DoTestMe)
    if(DoTest)write(*,*)NameSub,': iSize,jSiz,nVar=', iSize, jSize, nVar

    if(nThetaIono < 1) call init_ie_grid( &
         Grid_C(IE_) % Coord1_I, &
         Grid_C(IE_) % Coord2_I, &
         Grid_C(IE_) % Coord3_I(1))

    do iVar = 1, nVAr
       select case(NameVar_I(iVar))
       case('potential')
          if(.not. allocated(IonoPotential_II)) &
               allocate(IonoPotential_II(nThetaIono, nPhiIono))
          ! Electric potential has units of [E]*[x] (in SI: V/m*m=V)
          IonoPotential_II = Buffer_IIV(:,:,iVar) &
               *Si2No_V(UnitElectric_)*Si2No_V(UnitX_)
          call calc_grad_ie_potential
       case('jouleheat')
          if (.not. allocated(IonoJouleHeating_II)) &
               allocate(IonoJouleHeating_II(nThetaIono, nPhiIono))
          ! Joule heating variable has units of Poynting flux
          IonoJouleHeating_II = Buffer_IIV(:,:,2) &
               *Si2No_V(UnitPoynting_)

          ! scale Joule heating from ionosphere to GM inner BC
          call map_jouleheating_to_inner_bc
       case('sigmahall')
          if(.not. allocated(SigmaHall_II)) &
               allocate(SigmaHall_II(nThetaIono,nPhiIono))

          ! Height integrated conductivty has units of [j]*[x]/[E] (in SI:A/V)
          SigmaHall_II = Buffer_IIV(:,:,iVar) &
               *Si2No_V(UnitJ_)*Si2No_V(UnitX_)/Si2No_V(UnitElectric_)

          ! The ionosphere currents will need recalculation, so deallocate them
          if(allocated(HallJ_DII)) deallocate(HallJ_DII)

       case('sigmapedersen')
          if(.not. allocated(SigmaPedersen_II)) &
               allocate(SigmaPedersen_II(nThetaIono,nPhiIono))

          ! Height integrated conductivty has units of [j]*[x]/[E] (in SI:A/V)
          SigmaPedersen_II = Buffer_IIV(:,:,iVar) &
               *Si2No_V(UnitJ_)*Si2No_V(UnitX_)/Si2No_V(UnitElectric_)

          ! The ionosphere currents will need recalculation, so deallocate them
          if(allocated(PedersenJ_DII)) deallocate(PedersenJ_DII)
       case default
          call stop_mpi(NameSub//' unknown variable='//NameVar_I(iVar))
       end select
    end do

    if(DoTest)write(*,*)NameSub,' finished'

  end subroutine GM_put_from_ie
  !============================================================================
end module GM_couple_ie
!==============================================================================
