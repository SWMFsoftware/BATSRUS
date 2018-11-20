module GM_couple_ps

  !use ModMpi
  !use ModNumConst, ONLY: cRadToDeg, cDegToRad
  use CON_coupler, ONLY: Grid_C, ncell_id

  !use ModProcMH
  !use ModMain, ONLY: n_step
  !use ModPhysics, ONLY: No2Si_V, Si2No_V, &
  !     UnitP_, UnitRho_, UnitTemperature_, UnitB_, &
  !     Bdp, DipoleStrengthSi, rCurrents, rBody

  implicit none

  private ! except

  !public:: GM_get_for_ps
  public:: GM_put_from_ps   

  logical :: IsInitialized = .false., DoMultiFluidPSCoupling = .false.
  
  character(len=*), parameter :: NameMod='GM_couple_ps'

contains
  
  !==========================================================================
  subroutine GM_put_from_ps(Buffer_IIV, iSizeIn, jSizeIn, nVarPsGm, NameVarIn)

    use CON_coupler
    use CON_world,          ONLY: get_comp_info
    use ModProcessVarName,  ONLY: process_var_name
    use ModFieldTrace,      ONLY: UseAccurateTrace, DoMapEquatorRay
    use ModImCoupling       ! Storage for IM (and PS) pressure/density

    integer, intent(in) :: iSizeIn,jSizeIn,nVarPsGm
    real,    intent(in) :: Buffer_IIV(iSizeIn,jSizeIn,nVarPsGm)
    character(len=*), intent(in) :: NameVarIn

    integer :: nCells_D(2)
    integer :: nSpeedGm, nPGm, nPparGm, nWaveGm, nMaterialGm, nDensityGm
    integer, parameter :: pres_ =1, dens_=2, Hpres_=3, Opres_=4, &
         Hdens_=5, Odens_=6

    logical :: DoTest, DoTestMe
    character(len=*), parameter :: NameSub='GM_put_from_ps'
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
            nPGm, nPparGm, nWaveGm, nMaterialGm)
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
    if(.not.allocated(IM_lat))then
       ! Allocate IM_lat, IM_lon, IM_p, IM_dens
       call im_pressure_init(iSizeIn, jSizeIn)
       ! Set up PS ionospheric grid and store.
       ! PS uses an equatorial cylindrical grid, similar to RAM-SCB:
       IM_lat = Grid_C(PS_) % Coord1_I
       IM_lon = Grid_C(PS_) % Coord2_I * cRadToDeg

       if(DoTestMe) then
          write(*,*)NameSub//': PS grid information:'
          write(*,*)'     isize, jsize = ', iSizeIn, jSizeIn
          write(*,*)'     PS_lat max/min = ', maxval(IM_lat), minval(IM_lat)
          write(*,*)'     PS_lon max/min = ', maxval(IM_lon), minval(IM_lon)
       end if
       
       ! Coupling requires accurate raytrace that stops at the equator
       UseAccurateTrace= .true.
       DoMapEquatorRay = .true.
    end if

    ! Store PS variables as IM for internal use.
    ! PS will know
    IM_p    = Buffer_IIV(:,:,pres_)
    IM_dens = Buffer_IIV(:,:,dens_)
    iNewPIm  = iNewPIm + 1

    if(DoTestMe)then
       write(*,*)NameSub//': Max/Min values received from PS:'
       write(*,*)NameSub//': Negative values mean NO COUPLING.'
       write(*,'(a,2e12.3)')'   Density [cm-3] = ', &
            maxval(IM_dens)/1e6, minval(IM_dens)/1e6
       write(*,'(a,2e12.3)')'   Pressure [Pa]  = ', &
            maxval(IM_p), minval(IM_p)
    end if
    
    ! for multifluid                                               
    if(DoMultiFluidPSCoupling)then
       IM_Hpp    = Buffer_IIV(:,:,Hpres_)
       IM_Opp    = Buffer_IIV(:,:,Opres_)
       IM_Hpdens = Buffer_IIV(:,:,Hdens_)
       IM_Opdens = Buffer_IIV(:,:,Odens_)

       if(DoTestMe) then
          write(*,*)NameSub//': Max/Min values received from PS:'
          write(*,'(a,2e12.3)')'   Hp Density [cm-3] = ', &
               maxval(IM_Hpdens)/1e6, minval(IM_Hpdens)/1e6
          write(*,'(a,2e12.3)')'   Hp Pressure [Pa]  = ', &
               maxval(IM_Hpp), minval(IM_Hpp)
          write(*,*)NameSub//': Max/Min values received from PS:'
          write(*,'(a,2e12.3)')'   Op Density [cm-3] = ', &
               maxval(IM_Opdens)/1e6, minval(IM_Opdens)/1e6
          write(*,'(a,2e12.3)')'   Op Pressure [Pa]  = ', &
               maxval(IM_Opp), minval(IM_Opp)
       end if
       
    endif

  end subroutine GM_put_from_ps

end module GM_couple_ps
