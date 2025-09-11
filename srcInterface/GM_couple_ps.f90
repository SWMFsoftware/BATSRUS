module GM_couple_ps

  ! use ModMpi
  use ModNumConst, ONLY: cRadToDeg, cDegToRad
  use CON_coupler, ONLY: Grid_C, ncell_id

  ! use ModMain, ONLY: nStep
  ! use ModPhysics, ONLY: No2Si_V, Si2No_V, &
  !     UnitP_, UnitRho_, UnitTemperature_, UnitB_, &
  !     Bdp, DipoleStrengthSi, rCurrents, rBody

  implicit none

  private ! except

  ! public:: GM_get_for_ps
  public:: GM_put_from_ps

  logical :: IsInitialized = .false., DoMultiFluidPSCoupling = .false.

  character(len=*), parameter :: NameMod='GM_couple_ps'

contains
  !============================================================================

  subroutine GM_put_from_ps(Buffer_IIV, iSizeIn, jSizeIn, nVarPsGm, NameVarIn)

    use CON_coupler
    use ModConst, ONLY: cProtonMass
    use CON_world, ONLY: get_comp_info
    use ModProcessVarName, ONLY: process_var_name
    use ModFieldTrace, ONLY: UseAccurateTrace, DoMapEquatorRay
    use ModImCoupling       ! Storage for IM (and PS) pressure/density

    integer, intent(in) :: iSizeIn,jSizeIn,nVarPsGm
    real,    intent(in) :: Buffer_IIV(iSizeIn,jSizeIn,nVarPsGm)
    character(len=*), intent(in) :: NameVarIn

    integer :: nCells_D(2)
    integer :: nSpeedGm, nPGm, nPparGm, nWaveGm, nMaterialGm, nDensityGm, &
         nChargeStateAllGm
    integer, parameter :: pres_ =1, dens_=2, Hpres_=3, Opres_=4, &
         Hdens_=5, Odens_=6

    logical :: DoTest, DoTestMe
    character(len=*), parameter:: NameSub = 'GM_put_from_ps'
    !--------------------------------------------------------------------------
    call CON_set_do_test(NameSub, DoTest, DoTestMe)

    ! Ensure that grid sizes agree:
    nCells_D=ncell_id(PS_)
    if( iSizeIn /= nCells_D(1) .or. jSizeIn /= nCells_D(2) ) then
       write(*,*)NameSub//' grid sizes do not agree iSize,jSize,nCells=',&
            iSizeIn,jSizeIn,nCells_D(1:2)
       call CON_stop(NameSub//' SWMF_ERROR')
    end if

    ! Initialization step:
    if(.not. IsInitialized) then
       if(DoTestMe) write(*,*) NameSub//': Initializing PS-GM Coupling'

       ! Set multifluid coupling on/off based on number of densities:
       call process_var_name(Grid_C(GM_)%NameVar, nDensityGm, nSpeedGm, &
            nPGm, nPparGm, nWaveGm, nMaterialGm, nChargeStateAllGm)
       DoMultiFluidPSCoupling = nDensityGm > 1

       if(DoTestMe) write(*,*) NameSub// &
            ': Setting DoMultiFluidPsCoupling to ', DoMultiFluidPsCoupling
    end if

    ! Check NameVar:
    if(DoMultiFluidPSCoupling)then
       if(NameVarIn /= 'p:rho:Hpp:Opp:Hprho:Oprho') &
            call CON_stop(NameSub//' invalid NameVarIn='//NameVarIn)
    else
       if(NameVarIn /= 'p:rho') &
            call CON_stop(NameSub//' invalid NameVarIn='//NameVarIn)
    end if

    ! Initialize PS-GM coupling use IM infrastructure.
    if(.not.allocated(ImLat_I))then
       ! Allocate ImLat_I, ImLon_I, IM_p, IM_dens
       call im_pressure_init(iSizeIn, jSizeIn)
       ! Set up PS ionospheric grid and store.
       ! PS uses an equatorial cylindrical grid, similar to RAM-SCB:
       ImLat_I = Grid_C(PS_) % Coord1_I
       ImLon_I = Grid_C(PS_) % Coord2_I * cRadToDeg

       if(DoTestMe) then
          write(*,*)NameSub//': PS grid information:'
          write(*,*)'     isize, jsize = ', iSizeIn, jSizeIn
          write(*,*)'     PS_lat max/min = ', maxval(ImLat_I), minval(ImLat_I)
          write(*,*)'     PS_lon max/min = ', maxval(ImLon_I), minval(ImLon_I)
       end if

       ! Coupling requires accurate raytrace that stops at the equator
       UseAccurateTrace= .true.
       DoMapEquatorRay = .true.
    end if

    ! initialize
    IsImRho_I(:)  = .false.
    IsImP_I(:)    = .false.
    IsImPpar_I(:) = .false.

        ! Store IM variable for internal use
    ImP_III     (:,:,1) = Buffer_IIV(:,:,pres_)
    ImRho_III     (:,:,1) = Buffer_IIV(:,:,dens_)

    iNewPIm  = iNewPIm + 1

    IsImRho_I(1)  = .true.
    IsImP_I(1)    = .true.
    IsImPpar_I(1) = .false.

    ! Store PS variables as IM for internal use.
!    IM_p    = Buffer_IIV(:,:,pres_)
!    IM_dens = Buffer_IIV(:,:,dens_)
!    iNewPIm  = iNewPIm + 1

!    if(DoTestMe)then
!       write(*,*)NameSub//': Max/Min values received from PS:'
!       write(*,*)NameSub//': Negative values mean NO COUPLING.'
!       write(*,'(a,2e12.3)')'   Density [cm-3] = ', &
!            maxval(IM_dens)/1e6/cProtonMass, minval(IM_dens)/1e6/cProtonMass
!       write(*,'(a,2e12.3)')'   Pressure [Pa]  = ', &
!            maxval(IM_p), minval(IM_p)
!    end if

    ! for multifluid
    if(DoMultiFluidPsCoupling)then
       ImP_III(:,:,2)= Buffer_IIV(:,:,Hpres_)
!       IM_Hpp = Buffer_IIV(:,:,Hpres_)
       ImP_III(:,:,3)= Buffer_IIV(:,:,Opres_)
!       IM_Opp = Buffer_IIV(:,:,Opres_)
       ImRho_III(:,:,2)= Buffer_IIV(:,:,Hdens_)
!       IM_Hpdens = Buffer_IIV(:,:,Hdens_)
       ImRho_III(:,:,3)= Buffer_IIV(:,:,Odens_)
!       IM_Opdens = Buffer_IIV(:,:,Odens_)
       IsImRho_I(:)  = .true.
       IsImP_I(:)    = .true.
       IsImPpar_I(:) = .false.

       if(DoTestMe) then
          write(*,*)NameSub//': Max/Min values received from PS:'
          write(*,'(a,2e12.3)')'   Hp Density [cm-3] = ', &
               maxval(ImRho_III(:,:,2))/1e6/cProtonMass, &
               minval(ImRho_III(:,:,2))/1e6/cProtonMass
          write(*,'(a,2e12.3)')'   Hp Pressure [Pa]  = ', &
               maxval(ImP_III(:,:,2)), minval(ImP_III(:,:,2))
          write(*,*)NameSub//': Max/Min values received from PS:'
          write(*,'(a,2e12.3)')'   Op Density [cm-3] = ', &
               maxval(ImRho_III(:,:,3))/1e6/cProtonMass, &
               minval(ImRho_III(:,:,3))/1e6/cProtonMass
          write(*,'(a,2e12.3)')'   Op Pressure [Pa]  = ', &
               maxval(ImP_III(:,:,3)), minval(ImP_III(:,:,3))
       end if

    endif

  end subroutine GM_put_from_ps
  !============================================================================

end module GM_couple_ps
!==============================================================================
