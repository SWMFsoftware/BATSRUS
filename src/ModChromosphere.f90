!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModChromosphere

  ! All parameters relating to chromosphere and tansition region

  use BATL_lib, ONLY: test_start, test_stop, nI, nJ, nK
  use ModBatsrusUtility, ONLY: stop_mpi
  use ModPhysics, ONLY: Si2No_V, UnitTemperature_

  implicit none

  SAVE

  ! The use of short-scale exponential heat function with
  ! the decay length = (30 m/K)*TeCromosphere SI
  logical:: UseChromosphereHeating    = .false.

  real   :: NumberDensChromosphereCgs = 2.0e+11 ! [cm^{-3}]
  real   :: TeChromosphereSi = 5.0e4            ! [K]
  real   :: TeChromosphere
  !$acc declare create(TeChromosphere)

  ! TRANSITION REGION

  logical :: DoExtendTransitionRegion = .false.
  !$acc declare create(DoExtendTransitionRegion)

  ! The following variables are meaningful if
  ! DoExtendTransitionRegion=.true.
  real :: TeModSi = 3.0E+5                ! K
  real :: DeltaTeModSi = 1E+4             ! K
  !$acc declare create(TeModSi, DeltaTeModSi)

  ! The following variable is meaningful if
  ! DoExtendTransitionRegion = .false. . As long as
  ! the unextended transition region cannot be resolved
  ! we set the 'corona base temperature' equal to the
  ! temperature at the top of the transition region and
  ! use the integral ralationship across the transition
  ! region to find the number density at the top of the
  ! transition region
  real :: TeTransitionRegionTopSi = 4.0e+5 ! [K]

  ! Electron temperature in K:
  real, allocatable :: TeSi_C(:,:,:)
  !$omp threadprivate( TeSi_C )

contains
  !============================================================================
  subroutine read_chromosphere_param

    use ModReadParam, ONLY: read_var
    !--------------------------------------------------------------------------
    call read_var('UseChromosphereHeating', UseChromosphereHeating)
    call read_var('NeChromosphereCgs',      NumberDensChromosphereCgs)
    call read_var('TeChromosphereSi',       TeChromosphereSi)

  end subroutine read_chromosphere_param
  !============================================================================
  subroutine init_chromosphere
    !--------------------------------------------------------------------------
    TeChromosphere = TeChromosphereSi*Si2No_V(UnitTemperature_)
    !$acc update device(TeChromosphere)

    if(.not.allocated(TeSi_C)) allocate(TeSi_C(nI,nJ,nK))

  end subroutine init_chromosphere
  !============================================================================
  real function extension_factor(TeSi)
    !$acc routine seq

    real, intent(in) :: TeSi    ! Dimensionless

    real :: FractionSpitzer
    character(len=*), parameter:: NameSub = 'extension_factor'
    !--------------------------------------------------------------------------
#ifndef _OPENACC
    if(TeSi < 10)then
       write(*,*)'TeSi input =', TeSi
       call stop_mpi('Incorrect input temperature in '//NameSub)
    end if
#endif
    FractionSpitzer = 0.5*(1 + tanh((TeSi - TeModSi)/DeltaTeModSi))

    extension_factor = FractionSpitzer + &
         (1 - FractionSpitzer)*sqrt((TeModSi/TeSi)**5)

  end function extension_factor
  !============================================================================
  subroutine get_tesi_c(iBlock, TeSi_C)
    !$acc routine vector

    use BATL_lib,      ONLY: Xyz_DGB
    use ModAdvance,    ONLY: UseElectronPressure, UseIdealEos
    use ModAdvance,    ONLY: State_VGB, p_, Pe_, Rho_
    use ModSize,       ONLY: nI, nJ, nK
    use ModPhysics,    ONLY: No2Si_V, UnitTemperature_, &
         AverageIonCharge, PePerPtotal
    use ModMultifluid, ONLY: UseMultiIon, MassIon_I, ChargeIon_I, iRhoIon_I
    use ModUserInterface
    use ModGeometry,   ONLY: r_GB

    integer, intent(in)  :: iBlock
    real,    intent(out) :: TeSi_C(nI,nJ,nK)

    integer:: i, j, k
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_tesi_c'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    if(UseMultiIon)then
       !$acc loop vector collapse(3) independent
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          TeSi_C(i,j,k) = State_VGB(Pe_,i,j,k,iBlock) &
               /sum(ChargeIon_I*State_VGB(iRhoIon_I,i,j,k,iBlock)/MassIon_I) &
               *No2Si_V(UnitTemperature_)
       end do; end do; end do
    elseif(UseIdealEos)then
       if(UseElectronPressure)then
          !$acc loop vector collapse(3) independent
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             TeSi_C(i,j,k) = State_VGB(Pe_,i,j,k,iBlock) &
                  /State_VGB(Rho_,i,j,k,iBlock)
             TeSi_C(i,j,k) = TeSi_C(i,j,k) * No2Si_V(UnitTemperature_) * &
                  MassIon_I(1)/AverageIonCharge
          end do; end do; end do
       else
          !$acc loop vector collapse(3) independent
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             TeSi_C(i,j,k) = State_VGB(p_,i,j,k,iBlock) &
                  /State_VGB(Rho_,i,j,k,iBlock)
             TeSi_C(i,j,k) = TeSi_C(i,j,k) * No2Si_V(UnitTemperature_) * &
                  MassIon_I(1)/AverageIonCharge * PePerPtotal
          end do; end do; end do
       end if
    else
#ifndef _OPENACC
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          call user_material_properties(State_VGB(:,i,j,k,iBlock), &
               TeOut=TeSi_C(i,j,k))
       end do; end do; end do
#endif
    end if

#ifndef _OPENACC
    if(any(TeSi_C < 0)) then
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if (TeSi_C(i,j,k) < 0) then
               write(*,*) NameSub, ' Te is negative at Xyz_DGB, r_GB =', &
               Xyz_DGB(:,i,j,k,iBlock), r_GB(i,j,k,iBlock)
               if(UseMultiIon) then
                  write(*,*) "UseMultiIon=T, Pe =", &
                       State_VGB(Pe_,i,j,k,iBlock), &
                       ", RhoI= ", State_VGB(iRhoIon_I,i,j,k,iBlock)
               elseif(UseIdealEos) then
                  write(*,*) "IdealEOS"
                  if(UseElectronPressure)then
                     write(*,*) "UseElectronPressure=T, Pe =", &
                          State_VGB(Pe_,i,j,k,iBlock), &
                          ", Rho =", State_VGB(Rho_,i,j,k,iBlock)
                  else
                     write(*,*) "UseElectronPressure=F, p =", &
                          State_VGB(p_,i,j,k,iBlock), &
                          ", Rho =",State_VGB(Rho_,i,j,k,iBlock)
                  endif
               else
                  write(*,*) "NonIdeal EOS"
               endif
               call stop_mpi('Te is negative!')
          endif
       end do; end do; end do
    endif
#endif
    call test_stop(NameSub, DoTest, iBlock)

  end subroutine get_tesi_c
  !============================================================================
end module ModChromosphere
!==============================================================================
