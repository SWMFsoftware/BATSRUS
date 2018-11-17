!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModMultiFluid

  use BATL_lib, ONLY: &
       test_start, test_stop

  use ModVarIndexes
  use omp_lib
  
  implicit none
  save

  ! Convenient parameters for the ion fluids
  logical, parameter:: UseMultiIon = IonLast_ > IonFirst_ .and. Ex_ == 1

  ! Logical for signaling neutral fluid(s)
  logical, parameter:: UseNeutralFluid = nFluid > IonLast_ .or. B_ == U_

  ! Index for the first neutral fluid (as long as it exists)
  integer, parameter:: NeutralFirst_ = min(IonLast_+1, 100*(B_-U_)+1)

  ! This has to be at least 1 even if there are no ion fluids
  integer, parameter:: nIonFluid = IonLast_ - IonFirst_ + 1

  ! The number of true ion fluids if UseEfield
  integer, parameter:: nTrueIon  = nIonFluid - nElectronFluid

  ! Index points to the first electron fluid if UseEfield
  integer, parameter:: ElectronFirst_ = min(nTrueIon+1,nIonFluid)

  integer, parameter:: iUx_I(nFluid) = iRhoUx_I(1:nFluid)
  integer, parameter:: iUy_I(nFluid) = iRhoUy_I(1:nFluid)
  integer, parameter:: iUz_I(nFluid) = iRhoUz_I(1:nFluid)

  integer, parameter:: iRhoIon_I(nIonFluid)   = iRho_I(IonFirst_:IonLast_)
  integer, parameter:: iRhoUxIon_I(nIonFluid) = iRhoUx_I(IonFirst_:IonLast_)
  integer, parameter:: iRhoUyIon_I(nIonFluid) = iRhoUy_I(IonFirst_:IonLast_)
  integer, parameter:: iRhoUzIon_I(nIonFluid) = iRhoUz_I(IonFirst_:IonLast_)
  integer, parameter:: iUxIon_I(nIonFluid)    = iRhoUx_I(IonFirst_:IonLast_)
  integer, parameter:: iUyIon_I(nIonFluid)    = iRhoUy_I(IonFirst_:IonLast_)
  integer, parameter:: iUzIon_I(nIonFluid)    = iRhoUz_I(IonFirst_:IonLast_)
  integer, parameter:: iPIon_I(nIonFluid)     = iP_I(IonFirst_:IonLast_)

  integer, private:: i_
  logical, parameter:: IsIon_I(nFluid) = &
       (/ (i_ >= IonFirst_ .and. i_ <=IonLast_, i_=1, nFluid) /)

  ! The ion masses (adjustable)
  real :: MassIon_I(nIonFluid)
  real :: ChargeIon_I(nIonFluid)=1.0
  real :: ChargePerMass_I(nIonFluid) = 1.0

  ! Allow using fully non-conservative scheme for the neutral fluids
  logical :: DoConserveNeutrals    = .true.

  ! Variables that are set for the selected fluid
  integer ::                           &
       iRho   = Rho_,                  &
       iRhoUx = RhoUx_, iUx = RhoUx_,  &
       iRhoUy = RhoUy_, iUy = RhoUy_,  &
       iRhoUz = RhoUz_, iUz = RhoUz_,  &
       iPpar  = iPparIon_I(IonFirst_), &
       iP     = P_,                    &
       iEnergy= nVar+1
  !$omp threadprivate( iRho, iRhoUx, iRhoUy, iRhoUz, iPpar, iP, iEnergy )
  !$omp threadprivate( iUx, iUy, iUz )
  
  character (len=20) :: NameFluid = ''
  !$omp threadprivate( NameFluid )

  ! Variables for setting fluid boundary condition separately from MHD
  ! variables.
  logical :: IsFullyCoupledfluid = .true. , DoOhNeutralBc = .false.
  real    :: RhoBcFactor_I(nFluid) = 1.0, uBcFactor_I(nFluid) = 1.

  real    :: RhoNeutralsISW=0.0, RhoNeutralsISW_dim=0.0 , &
       PNeutralsISW=0.0  , PNeutralsISW_dim=0.0  , &
       UxNeutralsISW=0.0 , UxNeutralsISW_dim=0.0 , &
       UyNeutralsISW=0.0 , UyNeutralsISW_dim=0.0 , &
       UzNeutralsISW=0.0 , UzNeutralsISW_dim=0.0 , &
       TNeutralsISW=0.0  , TNeutralsISW_dim=0.0  , &
       mProtonMass=1.0
  
contains
  !============================================================================

  subroutine select_fluid(iFluidIn)
    integer :: iFluidIn 
    !--------------------------------------------------------------------------
    
    if(nFluid == 1) return
    
    iRho   = iRho_I(iFluidIn)
    iRhoUx = iRhoUx_I(iFluidIn)
    iRhoUy = iRhoUy_I(iFluidIn)
    iRhoUz = iRhoUz_I(iFluidIn)
    iP     = iP_I(iFluidIn)

    if(IsIon_I(iFluidIn)) iPpar = iPparIon_I(iFluidIn)

    iEnergy= nVar + iFluidIn

    iUx    = iRhoUx
    iUy    = iRhoUy
    iUz    = iRhoUz

  end subroutine select_fluid
  !============================================================================
  subroutine extract_fluid_name(String,iFluidOut)

    ! Find fluid name in string, remove it from string
    ! and set iFluid to the corresponding fluid index

    use ModUtilities, ONLY: lower_case

    character(len=*), intent(inout) :: String
    integer, optional, intent(out) :: iFluidOut
    
    integer :: i, l, iFluid

    character(len=*), parameter:: NameSub = 'extract_fluid_name'
    !--------------------------------------------------------------------------
    ! Assume fluid 1 as the default
    iFluid = 1

    ! Check if the string starts with a fluid name
    do i = 1, nFluid
       NameFluid = NameFluid_I(i)
       call lower_case(NameFluid)
       l = len_trim(NameFluid)
       if(String(1:l) == NameFluid)then
          ! Found fluid name, remove it from s
          iFluid = i
          String = String(l+1:len(String))
          EXIT
       end if
    end do

    call select_fluid(iFluid)

    NameFluid = NameFluid_I(iFluid)

    if(present(iFluidOut)) iFluidOut = iFluid
    
  end subroutine extract_fluid_name
  !============================================================================

end module ModMultiFluid
!==============================================================================
