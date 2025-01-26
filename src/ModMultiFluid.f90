!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModMultiFluid

  ! Definitions related to multi-ion and multi-fluid features

  use BATL_lib, ONLY: &
       test_start, test_stop

  use ModVarIndexes
  use omp_lib

  implicit none
  save

  ! Convenient parameters for the ion fluids
  logical, parameter:: UseMultiIon = nIonFluid > 1 .and. Ex_ == 1

  ! Logical for signaling neutral fluid(s) beyond the ions
  logical, parameter:: UseNeutralFluid = nFluid > nIonFluid

  ! Index for the first neutral fluid (as long as it exists)
  integer, parameter:: NeutralFirst_ = min(nIonFluid+1, 100*(B_-U_)+1)

  ! The number of true ion fluids if UseEfield
  integer, parameter:: nTrueIon  = nIonFluid - nElectronFluid

  ! Index points to the first electron fluid if UseEfield
  integer, parameter:: ElectronFirst_ = min(nTrueIon+1, nIonFluid)

  ! Indexes for velocities (same as momentum)
  integer, parameter:: iUx_I(nFluid) = iRhoUx_I
  integer, parameter:: iUy_I(nFluid) = iRhoUy_I
  integer, parameter:: iUz_I(nFluid) = iRhoUz_I

  ! Indexes for ion fluids
  integer, parameter:: iRhoIon_I(nIonFluid)   = iRho_I(1:nIonFluid)
  integer, parameter:: iRhoUxIon_I(nIonFluid) = iRhoUx_I(1:nIonFluid)
  integer, parameter:: iRhoUyIon_I(nIonFluid) = iRhoUy_I(1:nIonFluid)
  integer, parameter:: iRhoUzIon_I(nIonFluid) = iRhoUz_I(1:nIonFluid)
  integer, parameter:: iUxIon_I(nIonFluid)    = iUx_I(1:nIonFluid)
  integer, parameter:: iUyIon_I(nIonFluid)    = iUy_I(1:nIonFluid)
  integer, parameter:: iUzIon_I(nIonFluid)    = iUz_I(1:nIonFluid)
  integer, parameter:: iPIon_I(nIonFluid)     = iP_I(1:nIonFluid)

  ! Definition for entropy indexes (same as pressures)
  ! Perpendicular entropy is used for the isotropic case
  integer, parameter:: iS_I(nFluid)           = iP_I
  integer, parameter:: iSperpIon_I(nIonFluid) = iPIon_I
  integer, parameter:: iSparIon_I(nIonFluid)  = iPparIon_I

  integer, private:: i
  logical, parameter:: IsIon_I(nFluid) = &
       [ (i >= 1 .and. i <= nIonFluid, i=1, nFluid) ]

  ! Inverse of fluid masses for number density calculation
  real:: InvMassFluid_I(nFluid) = 1.0
  !$acc declare create(InvMassFluid_I)

  ! The ion masses (adjustable)
  real:: MassIon_I(nIonFluid) = 1.0
  real:: InvMassIon_I(nIonFluid) = 1.0      ! to avoid division
  real:: ChargeIon_I(nIonFluid) = 1.0
  real:: ChargePerMass_I(nIonFluid) = 1.0   ! electric charge for Hall MHD
  real:: ElectronPerMass_I(nIonFluid) = 1.0 ! for electron number density
  !$acc declare create(MassIon_I, ChargeIon_I, ChargePerMass_I)
  !$acc declare create(InvMassIon_I, ElectronPerMass_I)

  ! Allow using fully non-conservative scheme for the neutral fluids
  logical:: DoConserveNeutrals = .true.
  !$acc declare create(DoConserveNeutrals)

  ! Variables that are set for the selected fluid
  integer::                           &
       iRho   = Rho_,                  &
       iRhoUx = RhoUx_, iUx = RhoUx_,  &
       iRhoUy = RhoUy_, iUy = RhoUy_,  &
       iRhoUz = RhoUz_, iUz = RhoUz_,  &
       iPpar  = iPparIon_I(1),         &
       iP     = P_,                    &
       iEnergy= nVar+1
  !$omp threadprivate( iRho, iRhoUx, iRhoUy, iRhoUz, iPpar, iP, iEnergy )
  !$omp threadprivate( iUx, iUy, iUz )
  !$acc declare create( iRho, iRhoUx, iRhoUy, iRhoUz, iPpar, iP, iEnergy )
  !$acc declare create( iUx, iUy, iUz )

  character(len=20):: NameFluid = ''
  !$omp threadprivate( NameFluid )

  ! Variables for setting fluid boundary condition separately from MHD
  ! variables.
  logical:: IsFullyCoupledfluid = .true. , DoOhNeutralBc = .false.
  real:: RhoBcFactor_I(nFluid) = 1.0, uBcFactor_I(nFluid) = 1.

  real:: RhoNeutralsISW = 0.0, RhoNeuWindDim = 0.0 , &
       PNeutralsISW=0.0  , pNeuWindDim=0.0  , &
       UxNeutralsISW=0.0 , UxNeuWindDim=0.0 , &
       UyNeutralsISW=0.0 , UyNeuWindDim=0.0 , &
       UzNeutralsISW=0.0 , UzNeuWindDim=0.0 , &
       TNeutralsISW=0.0  , TempNeuWindDim=0.0  , &
       MassNeutralDim=1.0

contains
  !============================================================================
  subroutine select_fluid(iFluidIn)

    integer:: iFluidIn
    !--------------------------------------------------------------------------
    if(nFluid == 1) RETURN

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

    character(len=*), intent(inout):: String
    integer, optional, intent(out):: iFluidOut

    integer:: i, l, iFluid
    character(:), allocatable:: StringEnd

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
          StringEnd = String(l+1:len(String))
          String = StringEnd
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
