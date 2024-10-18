!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModUser
  ! This is the user module for Mars
  use ModSize
  use ModVarIndexes, ONLY: rho_, Ux_, Uz_,p_,Bx_, Bz_, &
       SpeciesFirst_, SpeciesLast_, MassSpecies_V
  use ModAdvance,    ONLY: nSpecies
  use ModPhysics,    ONLY: BodyRhoSpecies_I
  use ModMain,  ONLY: UaState_VCB
  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest, iBlockTest, iProc

  use ModUserEmpty, &
       IMPLEMENTED1 => user_read_inputs,                &
       IMPLEMENTED2 => user_init_session,               &
       IMPLEMENTED3 => user_set_ics,                    &
       IMPLEMENTED4 => user_set_face_boundary,          &
       IMPLEMENTED5 => user_init_point_implicit,        &
       IMPLEMENTED6 => user_calc_sources_impl,          &
       IMPLEMENTED7 => user_get_b0,                     &
       IMPLEMENTED8 => user_get_log_var,                &
       IMPLEMENTED9 => user_set_boundary_cells,         &
       IMPLEMENTED10=> user_action

  include 'user_module.h' ! list of public methods

  ! user routine Version number and descriptive string.
  character (len=*), parameter :: NameUserFile = "ModUserMars.f90"
  ! added rotation of crustal magnetic field with time,
  ! MHD model can now use MSO coordinate if needed
  character(len=*), parameter :: NameUserModule = &
       'Mars 4 species MHD code, Yingjuan Ma'

  character(len=10) :: SolarCond='solarmax  '

  ! Radius within which the point implicit scheme should be used
  real :: rPointImplicit = 2.0

  ! Mars stuff
  integer, parameter :: MaxSpecies=4, MaxNuSpecies=8, MaxReactions=10
  real, allocatable :: nDenNuSpecies_CBI(:,:,:,:,:)
  ! real,  dimension(1:nI, 1:nJ, 1:nK, MaxBlock,MaxNuSpecies) :: &
  !      nDenNuSpecies_CBI    ! number density of neutral species
  real, allocatable :: TempNuSpecies_CBI(:,:,:,:)! tempature of neutral species
  ! production rate according to optical depth
  real, allocatable :: Productrate_CB(:,:,:,:)
  ! Ionization rate according to TGCM
  real, allocatable :: Ionizationrate_CBI(:,:,:,:,:)

  real, dimension(MaxReactions):: ReactionRate_I
  real, dimension(MaxReactions,MaxSpecies):: CoeffSpecies_II, &
       dSdRho_II !, dLdRho_II
  real, dimension(MaxSpecies):: LossSpecies_I, &
       SiSpecies_I,  LiSpecies_I,  PhoIon_I, Recb_I, ImpIon_I
  !        dStndRho_I,  dLtdRho_I,  dLtndNumRho_I

  !$omp threadprivate( ReactionRate_I, CoeffSpecies_II, dSdRho_II )
  !$omp threadprivate( LossSpecies_I, SiSpecies_I, LiSpecies_I )
  !$omp threadprivate( PhoIon_I, Recb_I, ImpIon_I )

  real :: totalNumRho, totalLossRho, totalSourceRho, totalLossNumRho, &
       totalSourceNumRho, totalLossx, totalLossNumx
  !$omp threadprivate( totalNumRho, totalLossRho, totalSourceRho )
  !$omp threadprivate( totalLossNumRho )
  !$omp threadprivate( totalSourceNumRho, totalLossx, totalLossNumx )

  real, allocatable :: MaxSiSpecies_CB(:,:,:,:)
  real, allocatable :: MaxLiSpecies_CB(:,:,:,:)
  real, allocatable :: MaxSLSpecies_CB(:,:,:,:)

  ! the reactions considered:(p means ion, em means electron)
  ! the prefered order of a reaction is ions, Nus, hv and electrons
  integer, parameter :: &! reaction number
       CO2_hv__CO2p_em_=1 ,&! CO2+hv-->CO2p+em
       O_hv__Op_em_    =2 ,&! O+hv-->Op+em
       CO2p_O__O2p_CO_ =3 ,&! CO2p+O-->O2p+CO
       Op_CO2__O2p_CO_ =4 ,&! Op+CO2-->O2p+CO
       CO2p_O__Op_CO2_ =5 ,&! CO2p+O-->Op+CO2
       O2p_em__O_O_    =6 ,&! O2p+em-->O+O
       CO2p_em__CO_O_  =7 ,&! CO2p+em-->CO+O
       Hp_O__Op_H_     =8 ,&! Hp+O-->Op+H
       Op_H__Hp_O_     =9 ,&! Op+H-->Hp+O
       H_hv__Hp_em_    =10  ! H+hv-->Hp+em

  real, dimension(MaxReactions) :: Rate_I
  real, dimension(MaxReactions) :: RateDim_I= &
       [2.47e-7, 8.89e-8, 1.64e-10, 1.1e-9, &
       9.60e-11, 7.38e-8, 3.1e-7, 5.084e-10, 6.4e-10, 5.58e-8 ]  ! cm^3 s^(-1)

  integer, parameter :: &! order of ion species
       Hp_  =1, &
       O2p_ =2, &
       Op_  =3, &
       CO2p_=4

  real, dimension(MaxSpecies), parameter::  &
       MassSpecies_I=[1., 32., 16., 44. ]  ! atm

  !  MassSpecies_I(Hp_)=1	 ! atm
  !  MassSpecies_I(CO2p_)=44     ! atm
  !  MassSpecies_I(O2p_)=32      ! atm
  !  MassSpecies_I(Op_)=16       ! atm

  integer, parameter :: & ! order of Neutral species
       CO2_=1 ,&
       O_=2   ,&
       H_=3, &
       Oh_=4   ,&
       Ohx_=5 , &
       Hx_=6, &
       Ox_=7 ,&
       CO2x_=8

  real, dimension(MaxNuSpecies):: CrossSection_I
  real, dimension(MaxNuSpecies), parameter:: CrossSectionDim_I=&
       [2.6e-17,1.5e-17,0.0,1.5e-17,1.5e-17,0.0,1.5e-17,2.6e-17]

  real, dimension(MaxNuSpecies), parameter:: &
       NuMassSpecies_I=[44,16,1,16,16,1,16, 44]
  !  NuMassSpecies_I(CO2_)=44	! atm
  !  NuMassSpecies_I(O_)=16	! atm

  real, dimension(MaxNuSpecies):: HNuSpecies_I=1.0, HNuSpeciesDim_I=1.0
  ! HNuSpecies_dim_I(CO2_)=6.7e3   ! m
  ! HNuSpecies_dim_I(O_)=18.4e3    ! m

  real, dimension(MaxNuSpecies):: BodynDenNuSpecies_I,&
       BodynDenNuSpDim_I=[1.1593e12, 3.2278e9, 1.1307e7, 1.951e4, &
       1.5248e3, 9.4936e5, 5.2695e8, 2.2258e11]

  integer, parameter :: & ! other numbers
       em_=-1 ,&
       hv_=-2

  real :: TNu_body_dim=300.0, kTn  ! neutral temperature
  real :: kTi0  ! ion temperature at the body
  real :: kTp0  ! dimensionless temperature
                ! of new created ions / plasma (Tp_body=2.0*Ti_body)
  real :: Te_new_dim=1000., KTe0, T300
  real, parameter :: T300_dim = 300.0, Ti_dim =300.

  real, allocatable :: nu_BLK (:,:,:,:)
  real, parameter :: nu0_dim=4.0e-10
  real :: nu0

  ! coefficient of Mars magnetic field
  real, dimension(0:61,0:61) :: cmars, dmars
  integer :: NNm
  ! real :: mars_eps=1e-4
  logical :: UseMarsB0 = .false., UseMso=.false.

  ! variables needed to be converted from MSO to GEO
  real :: RotAxisMso_D(3) ! rotation axis in MSO coordinate
  real :: u, v, w, uv, uvw, sint1, cost1, sint2, cost2

  real :: rot = 1.0, thetilt = 0.0
  logical :: UseHotO= .false.
  logical :: UseTempCont=.false.
  logical :: UseImpactIon=.false.
  logical :: UseChargeEx=.true.
  logical :: UseChapman=.false.

  integer, parameter:: NLong=73, NLat=36, MaxAlt=21
  real :: Long_I(NLong), Lat_I(NLat), Alt_I(MaxAlt)
  real :: Temp(NLong, NLat, MaxAlt)
  real :: Den_CO2(NLong, NLat, MaxAlt)!,Den_CO2_dim(NLong, NLat, NAlt)
  real :: Den_O(NLong, NLat, MaxAlt)!,Den_O_dim(NLong, NLat, NAlt)
  real :: ICO2p(NLong, NLat, MaxAlt)!,ICO2p_dim(NLong, NLat, NAlt)
  real :: IOp(NLong, NLat, MaxAlt)!,IOp_dim(NLong, NLat, NAlt)
  logical :: UseMarsAtm=.false.
  integer :: NAlt=21

  logical:: UseOldEnergy=.true.

contains
  !============================================================================
  subroutine user_action(NameAction)

    character(len=*), intent(in):: NameAction

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_action'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(iProc==0)write(*,*) NameSub,' called with action ',NameAction
    select case(NameAction)
    case('initialize module')
       if(.not.allocated(TempNuSpecies_CBI))then
          allocate(TempNuSpecies_CBI(nI,nJ,nK,MaxBlock))
          allocate(Productrate_CB(nI,nJ,nK,MaxBlock))
          allocate(Ionizationrate_CBI(nI,nJ,nK,MaxBlock,2))
          allocate(MaxSiSpecies_CB(nI,nJ,nK,MaxBlock))
          allocate(MaxLiSpecies_CB(nI,nJ,nK,MaxBlock))
          allocate(MaxSLSpecies_CB(nI,nJ,nK,MaxBlock))
          allocate(nu_BLK(1:nI,1:nJ,1:nK,MaxBlock))
       end if
    case('clean module')
       if(allocated(TempNuSpecies_CBI)) deallocate(TempNuSpecies_CBI)
       if(allocated(Productrate_CB)) deallocate(Productrate_CB)
       if(allocated(Ionizationrate_CBI)) deallocate(Ionizationrate_CBI)
       if(allocated(MaxSiSpecies_CB))  deallocate(MaxSiSpecies_CB)
       if(allocated(MaxLiSpecies_CB))  deallocate(MaxLiSpecies_CB)
       if(allocated(MaxSLSpecies_CB))  deallocate(MaxSLSpecies_CB)
       if(allocated(nu_BLK)) deallocate(nu_BLK)
    end select
    call test_stop(NameSub, DoTest)
  end subroutine user_action
  !============================================================================

  subroutine user_read_inputs
    use ModMain
    use ModReadParam

    character (len=100) :: NameCommand
    integer:: i, j, k, n, m
    character (len=60):: TGCMFilename
    character (len=100) :: line

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_read_inputs'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    do
       if(.not.read_line() ) EXIT
       if(.not.read_command(NameCommand)) CYCLE
       select case(NameCommand)

       case('#USERINPUTEND')
          if(iProc==0) write(*,*)'USERINPUTEND'
          EXIT

       case('#USEOLDENERGY')
          call read_var('UseOldEnergy',UseOldEnergy)
          if(.not.UseOldEnergy)then
             call read_var('Te_new_dim',Te_new_dim)
             ! change temperature from ev to k
             Te_new_dim = Te_new_dim * 11610.0
          end if

       case("#UseMarsB0")  ! if or not include crustal magnetic field of Mars
          call read_var('UseMarsB0',UseMarsB0)
          if(UseMarsB0) then
             call read_var('NNm', NNm)
             call read_var('rot',rot)
             call read_var('thetilt', thetilt)
             rot= rot/180.0*3.141592653589793238462643383279
             thetilt= thetilt/180.0*3.141592653589793238462643383279
             cmars = 0.0
             dmars = 0.0
             open(15,file='marsmgsp.txt')
             do i=0,NNm
                read(15,*)n,(cmars(n-1,m),m=0,n-1),(dmars(n-1,m),m=0,n-1)
             end do
             close(15)
          endif

       case("#UseMso") ! default is false
          call read_var('UseMso', UseMso)
          if(UseMso) then
             call read_var('RotAxisMso_D(1)',RotAxisMso_D(1))
             call read_var('RotAxisMso_D(2)',RotAxisMso_D(2))
             call read_var('RotAxisMso_D(3)',RotAxisMso_D(3))

             RotAxisMso_D = RotAxisMso_D/sqrt(sum(RotAxisMso_D**2))

             u = RotAxisMso_D(1)
             v = RotAxisMso_D(2)
             w = RotAxisMso_D(3)

             uv=sqrt(u**2+v**2)
             cost1=u/uv
             sint1=v/uv
             uvw=sqrt((u*w)**2+v**2)
             cost2=w*u/uvw
             sint2=v/uvw
          end if

       case("#SOLARCON") ! solar cycle condition
          call read_var('SolarCon',SolarCond)

       case("#UseHotO")  ! adding hot Oxygen or not
          call read_var('UseHotO',UseHotO)

       case("#UseTempCont") ! add hoc term of the energy source
          call read_var('UseTempCont',UseTempCont)

       case('#REACTIONS')
          call read_var('UseImpactIon',UseImpactIon)
          call read_var('UseChargeEx',UseChargeEx)
          ! open(15,file='read_in.dat')
          ! do i=1,32
          !   read(15,*)Temp_dim(i),Impact_ION_dim(i,Op_),Impact_ION_dim(i,Hp_)
          ! end do
          ! close(15)

       case('#USECHAPMAN')
          call read_var('UseChapman',UseChapman)

       case("#UseMarsAtm")
          call read_var('UseMarsAtm',UseMarsAtm)
          if(UseMarsAtm)then
             call read_var('TGCMFilename',TGCMFilename)
             call read_var('NAlt', Nalt)
             open(15,file=TGCMFilename,status="old")
             read(15,*)line
             write(*,*)line, Nalt
             do k = 1, NAlt
                do j=1, NLat
                   do i=1, NLong
                      read(15,*)Long_I(i),Lat_I(j),Alt_I(k),Temp(i,j,k),Den_CO2(i,j,k),&
                           Den_O(i,j,k),ICO2p(i,j,k),IOp(i,j,k)
                   end do
                end do
             end do
             close(15)
             write(*,*)Long_I(Nlong),Lat_I(NLat),Alt_I(Nalt)
             write(*,*)Long_I(1),Lat_I(1),Alt_I(1)
             write(*,*)'Den_O(i,j,k),ICO2p(i,j,k),IOp(i,j,k)=',&
                  Den_O(Nlong,Nlat,Nalt),ICO2p(Nlong,Nlat,Nalt),&
                  IOp(Nlong,Nlat,Nalt)

          end if
       case('#POINTIMPLICITREGION')
          call read_var('rPointImplicit',rPointImplicit)

       case default
          if(iProc==0) call stop_mpi( &
               'read_inputs: unrecognized command: '//NameCommand)
       end select
    end do
    call test_stop(NameSub, DoTest)
  end subroutine user_read_inputs
  !============================================================================

  subroutine user_init_point_implicit

    use ModMain, ONLY: nBlock, Unused_B
    use ModVarIndexes
    use ModPointImplicit, ONLY: iVarPointImpl_I, IsPointImplMatrixSet, &
       UseUserPointImplicit_B
    use ModPhysics, ONLY: rBody
    use ModGeometry, ONLY: r_GB

    ! Allocate and set iVarPointImpl_I
    integer :: iBlock
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_init_point_implicit'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    allocate(iVarPointImpl_I(8))

    iVarPointImpl_I = [RhoHp_, RhoO2p_, RhoOp_, RhoCO2p_, &
         RhoUx_, RhoUy_, RhoUz_, P_]

    ! Note that energy is not an independent variable for the
    ! point implicit scheme. The pressure is an independent variable,
    ! and in this example there is no implicit pressure source term.

    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       UseUserPointImplicit_B(iBlock) = &
          r_GB(1,1,1,iBlock) <= rPointImplicit .and. &
          r_GB(nI,1,1,iBlock) > rBody
    end do

    ! Tell the point implicit scheme if dS/dU will be set analytically
    ! If this is set to true the DsDu_VVC matrix has to be set below.
    IsPointImplMatrixSet = .false.

    call test_stop(NameSub, DoTest)
  end subroutine user_init_point_implicit
  !============================================================================
  subroutine user_calc_sources_impl(iBlock)

    use ModAdvance,  ONLY: Source_VC
    use ModMain, ONLY: iNewDecomposition

    integer, intent(in) :: iBlock

    integer :: iLastDecomposition=-100
    !$omp threadprivate( iLastDecomposition )

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_calc_sources_impl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if( nDenNuSpecies_CBI(1, 1, 1, iBlock, 1) < 0.0 &
         .or. iLastDecomposition /= iNewDecomposition)then

       call set_neutral_density(iBlock)

       if(iBlock == nBlock)then
          iLastDecomposition = iNewDecomposition
       end if
    end if

    call user_sources(iBlock, Source_VC)

    call test_stop(NameSub, DoTest)
  end subroutine user_calc_sources_impl
  !============================================================================
  subroutine user_sources(iBlock, Source_VC)
    ! This subroutine is used to calculate sources for the MHD equations. The
    ! routine is called for each block separately so that the user would
    ! typically need only to code the source term calculation for a single
    ! block (in other words inside the the k,j,i loop below). As with all user
    ! subroutines, the variables declared in ModUser are available here.
    !
    ! The user should load the variables:
    !   Srho_C,SrhoUx_C,SrhoUy_C,SrhoUz_C,SBx_C,SBy_C,SBz_C,SE_C,SP_C
    ! They are now stored as one variable: S_IC, where
    ! rho_ : rho, rho_+1:rho_+MaxSpecies: rhoSpecies
    ! rho_+MaxSpecies+1: rhoUx
    ! rho_+MaxSpecies+2: rhoUy
    ! rho_+MaxSpecies+3: rhoUz
    ! rho_+MaxSpecies+7: P
    ! rho_+MaxSpecies+8: Energy
    !
    ! Note that SE_C(energy) and SP_C(pressure) must both be loaded if the code
    ! is going to use both the primitive and the conservative MHD equation
    ! advance (see the USER MANUAL and the DESIGN document). If using only
    ! primitive SP must be loaded. If using only conservative SE must be
    ! loaded. The safe approach is to load both.

    ! Variable meanings:
    !   Srho_C: Source terms for the continuity equation
    !   SE_C,SP_C: Source terms for the energy (conservative) and pressure
    !          (primative) equations
    !   SrhoUx_C,SrhoUy_C,SrhoUz_C: Source terms for the momentum equation
    !   SBx_C,SBy_C,SBz_C: Source terms for the magnetic field equations

    use ModMain,    ONLY: nI, nJ, nK
    use ModAdvance,  ONLY: State_VGB, Flux_VXI, Flux_VYI, Flux_VZI, Vdt_
    use ModVarIndexes, ONLY: Rho_, Ux_, Uy_, Uz_, &
         RhoUx_, RhoUy_, RhoUz_, P_, Energy_, Bx_, By_, Bz_
    use ModGeometry, ONLY: r_GB
    use ModPhysics,  ONLY: Rbody, InvGammaMinus1, GammaMinus1,&
         No2Io_V, UnitT_, UnitN_
    use BATL_lib, ONLY: CellVolume_GB
    use ModPointImplicit, ONLY: UsePointImplicit

    integer, intent(in) :: iBlock
    real, intent(inout) :: Source_VC(:,:,:,:)

    ! Variables required by this user subroutine
    real, parameter :: Te_dim = 300.0
    real, parameter :: A0=-1143.33, A1=323.767, A2=-35.0431, A3=1.69073, &
         A4=-0.0306575
    real, parameter :: B0=-1233.29, B1=347.764, B2=-37.4128, B3=1.79337, &
         B4=-0.0322777
    real :: S_IC(MaxSpecies+9,nI,nJ,nK)
    real :: xMinBox, xMaxBox, X3, X4
    real :: totalIMPNumRho
    integer:: i,j,k,iSpecies
    real :: inv_rho, inv_rho2, uu2, Productrate, kTi, kTe
    real :: temp
    real :: totalPSNumRho, totalRLNumRhox, temps
    real :: SourceLossMax, vdtmin

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_sources'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    if(iBlock /= iBlockTest) DoTest = .false.

    S_IC = 0.0

    if(r_GB(1,1,1,iBlock) > 3.0*Rbody) RETURN

    do k=1,nK; do j=1,nJ; do i=1,nI

       if (r_GB(i,j,k,iBlock) < Rbody) CYCLE

       inv_rho = 1.00/State_VGB(rho_,i,j,k,iBlock)
       inv_rho2 = inv_rho*inv_rho
       uu2 =(State_VGB(Ux_,i,j,k,iBlock)*State_VGB(Ux_,i,j,k,iBlock)  &
            +State_VGB(Uy_,i,j,k,iBlock)*State_VGB(Uy_,i,j,k,iBlock)  &
            +State_VGB(Uz_,i,j,k,iBlock)*State_VGB(Uz_,i,j,k,iBlock)) &
            *inv_rho2

       ReactionRate_I  = 0.0
       CoeffSpecies_II = 0.0
       PhoIon_I        = 0.0
       Recb_I          = 0.0
       LossSpecies_I   = 0.0
       SiSpecies_I     = 0.0
       LiSpecies_I     = 0.0

       totalSourceRho    = 0.0
       totalLossRho      = 0.0
       totalLossNumRho   = 0.0
       totalSourceNumRho = 0.0
       totalLossx        = 0.0
       totalLossNumx     = 0.0
       totalPSNumRho     = 0.0
       totalRLNumRhox    = 0.0

       totalNumRho = sum(State_VGB(SpeciesFirst_:SpeciesLast_,i,j,k,iBlock) &
            /MassSpecies_I)
       MaxSLSpecies_CB(i,j,k,iBlock) = 1.0e-3

       Productrate = Productrate_CB(i,j,k,iBlock)

       !             ReactionRate_I(CO2_hv__CO2p_em_)= &
       !                  Rate_I(CO2_hv__CO2p_em_)&
       !                  *nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)
       !             PhoIon_I(CO2p_)=ReactionRate_I(CO2_hv__CO2p_em_) &
       !                  *Productrate
       !             ReactionRate_I(O_hv__Op_em_)= &
       !                  Rate_I(O_hv__Op_em_)&
       !                  *nDenNuSpecies_CBI(i,j,k,iBlock,O_)
       !             PhoIon_I(Op_)=ReactionRate_I(O_hv__Op_em_) &
       !                  *Productrate

       ReactionRate_I(H_hv__Hp_em_) = &
            Rate_I(H_hv__Hp_em_) * nDenNuSpecies_CBI(i,j,k,iBlock,H_)
       PhoIon_I(Hp_) = ReactionRate_I(H_hv__Hp_em_) * Productrate

       PhoIon_I(CO2p_) = Ionizationrate_CBI(i,j,k,iBlock,CO2_)
       PhoIon_I(Op_) = Ionizationrate_CBI(i,j,k,iBlock,O_)

       ! IMPACT Ionization
       ImpIon_I = 0.0
       if(UseImpactIon)then
          xMinBox = log(Te_dim)
          xMaxBox = xMinBox*xMinBox
          X3 = xMaxBox*xMinBox
          X4 = xMaxBox*xMaxBox
          ImpIOn_I(Hp_) = exp(A0+A1*xMinBox+A2*xMaxBox+A3*X3+A4*X4)&
               *No2Io_V(UnitT_)*No2Io_V(UnitN_)
          ImpIOn_I(Op_) = exp(B0+B1*xMinBox+B2*xMaxBox+B3*X3+B4*X4)&
               *No2Io_V(UnitT_)*No2Io_V(UnitN_)
          ImpIon_I(Hp_) = ImpIon_I(Hp_)*&
               nDenNuSpecies_CBI(i,j,k,iBlock,H_)
          ImpIon_I(Op_) = ImpIon_I(Op_)*&
               nDenNuSpecies_CBI(i,j,k,iBlock,O_)
          ImpIon_I      = ImpIon_I * totalNumRho
       endif

!!!              Alt = (r_GB(i,j,k,iBlock)-1.0)*6052.0
!!!              if (Alt < 200.0 )then
!!!                 Te_dim = 300.0 + (Alt - 140.0)*3.7e3/60.0
!!!              else if( Alt < 800.0)then
!!!                 Te_dim = 4.0e3 + (Alt - 200.0)*5.0
!!!              else
!!!                 Te_dim =7.0e3
!!!              end if
!!!          Te_dim = 300.0
!!!          Ti_dim= Te_dim

       ! charge exchange
       ReactionRate_I(CO2p_O__O2p_CO_) = &
            Rate_I(CO2p_O__O2p_CO_) &
            * nDenNuSpecies_CBI(i,j,k,iBlock,O_)
       CoeffSpecies_II(O2p_,CO2p_) = ReactionRate_I(CO2p_O__O2p_CO_)

       ReactionRate_I(Op_CO2__O2p_CO_) = &
            Rate_I(Op_CO2__O2p_CO_) &
            * nDenNuSpecies_CBI(i,j,k,iBlock,CO2_) &
            *exp(log(Tnu_body_dim/Ti_dim)*0.39)
       CoeffSpecies_II(O2p_, Op_) = ReactionRate_I(Op_CO2__O2p_CO_)

       ReactionRate_I(CO2p_O__Op_CO2_) = &
            Rate_I(CO2p_O__Op_CO2_) &
            * nDenNuSpecies_CBI(i,j,k,iBlock,O_)
       CoeffSpecies_II(Op_,CO2p_) = ReactionRate_I(CO2p_O__Op_CO2_)

!!!    ReactionRate_I(Hp_O__Op_H_)= &
!!!         Rate_I(Hp_O__Op_H_)* nDenNuSpecies_CBI(i,j,k,iBlock,O_)
!!!    CoeffSpecies_II(Op_,Hp_)=ReactionRate_I(Hp_O__Op_H_)
!!!
!!!    ReactionRate_I(Op_H__Hp_O_)= &
!!!         Rate_I(Op_H__Hp_O_)* nDenNuSpecies_CBI(i,j,k,iBlock,H_)
!!!    CoeffSpecies_II(Hp_,Op_)=ReactionRate_I(Op_H__Hp_O_)

       ! Recombination

       ! ReactionRate_I(O2p_em__O_O_)=Rate_I(O2p_em__O_O_)
       ! Recb_I(O2p_)=ReactionRate_I(O2p_em__O_O_)

       ! ReactionRate_I(CO2p_em__CO_O_)=Rate_I(CO2p_em__CO_O_)
       ! Recb_I(CO2p_)=ReactionRate_I(CO2p_em__CO_O_)
       ! Recombination

       kTi = State_VGB(P_,i,j,k,iBlock)/totalNumRho*0.5
       kTe = kTi

       ReactionRate_I(O2p_em__O_O_) = Rate_I(O2p_em__O_O_)
       ! Recb_I(O2p_)=ReactionRate_I(O2p_em__O_O_)*exp(log(TNu_body_dim/Te_dim)*0.56)
       Recb_I(O2p_)=ReactionRate_I(O2p_em__O_O_)*exp(log(kTn/kTi)*0.56)

       ReactionRate_I(CO2p_em__CO_O_)=Rate_I(CO2p_em__CO_O_)
       ! Recb_I(CO2p_)=ReactionRate_I(CO2p_em__CO_O_)*sqrt(TNu_body_dim/Te_dim)
       Recb_I(CO2p_)=ReactionRate_I(CO2p_em__CO_O_)*&
            sqrt(kTn/kTi)

       do iSpecies = 1, nSpecies
          LossSpecies_I = LossSpecies_I + CoeffSpecies_II(iSpecies, :)
          ! dStndRho_I=dStndRho_I  &
          !      + CoeffSpecies_II(iSpecies, :)/MassSpecies_I(:)
          dSdRho_II(1:nSpecies, iSpecies) = &
               CoeffSpecies_II(1:nSpecies, iSpecies) &
               *MassSpecies_I(1:nSpecies)/MassSpecies_I(iSpecies)
       enddo

!!!    do iSpecies=1, nSpecies
!!!       dLdRho_II(1:nSpecies, iSpecies)=Recb_I(1:nSpecies)&
!!!            *rhoSpecies_GBI(i,j,k,iBlock,1:nSpecies) &
!!!            /MassSpecies_I(iSpecies)
!!!       dLdRho_II(iSpecies, iSpecies)=  &
!!!            dLdRho_II(iSpecies, iSpecies) &
!!!            +LossSpecies_I(iSpecies)  &
!!!            +Recb_I(iSpecies)*totalNumRho
!!!    enddo
!!!    !              dSLdRho_II=dSdRho_II-dLdRho_II
!!!
!!!    do iSpecies=1, nSpecies
!!!       dLtdRho_I(:)=dLtdRho_I(:) +dLdRho_II(iSpecies,:)
!!!       dLtndNumRho_I(:)=dLtndNumRho_I(:) &
!!!            +dLdRho_II(iSpecies,:)*MassSpecies_I(:)/MassSpecies_I(iSpecies)
!!!    enddo

       SiSpecies_I(:) = (PhoIon_I(:)+ImpIon_I(:))*MassSpecies_I(:)

       do iSpecies = 1, nSpecies
          SiSpecies_I(1:nSpecies) = &
               SiSpecies_I(1:nSpecies) &
               +dSdRho_II(1:nSpecies, iSpecies) &
               *State_VGB(rho_+iSpecies, i,j,k, iBlock)
          LiSpecies_I(iSpecies)= &
               LiSpecies_I(iSpecies) &
               +(LossSpecies_I(iSpecies) +Recb_I(iSpecies)*totalNumRho)&
               *State_VGB(rho_+iSpecies, i,j,k, iBlock)
       enddo

       totalIMPNumRho = sum(ImpIon_I(:))

       totalSourceRho = sum(SiSpecies_I(1:nSpecies))
       totalLossRho = sum(LiSpecies_I(1:nSpecies))
       ! sum of the (Loss term) of all ion species
       totalLossNumRho = sum(LiSpecies_I(1:nSpecies) &
            /MassSpecies_I(1:nSpecies))
       ! sum of the (loss term/atom mass)
       totalSourceNumRho = sum(SiSpecies_I(1:nSpecies)&
            /MassSpecies_I(1:nSpecies))
       ! sum of the (Source term/atom mass)
       totalLossx    = totalLossRho*inv_rho
       totalLossNumx = totalLossNumRho/totalNumRho
       totalPSNumRho = sum(PhoIon_I(:))

       ! sum of the photonionization source/atom mass
       totalRLNumRhox=sum(Recb_I(:) &
            *State_VGB(rho_+1:rho_+nSpecies, i,j,k, iBlock)/MassSpecies_I(:))
       ! sum of the (loss term/atom mass) due to recombination

       MaxSLSpecies_CB(i,j,k,iBlock) = maxval(abs(SiSpecies_I(1:nSpecies) + &
            LiSpecies_I(1:nSpecies) ) / &
            (State_VGB(rho_+1:rho_+MaxSpecies, i,j,k, iBlock)+1e-20)) &
            *CellVolume_GB(i,j,k,iBlock)

       ! Limit timestep for explicit scheme
       if(.not.UsePointImplicit)then
          ! sum of the (loss term/atom mass) due to recombination
          SourceLossMax = 10.0*maxval(abs(SiSpecies_I(1:nSpecies)-&
               LiSpecies_I(1:nSpecies) ) /&
               (State_VGB(rho_+1:rho_+nSpecies, i,j,k, iBlock)+1e-20))&
               *CellVolume_GB(i,j,k,iBlock)
          vdtmin = min(Flux_VXI(Vdt_,i,j,k,1),Flux_VYI(Vdt_,i,j,k,1),Flux_VZI(Vdt_,i,j,k,1))
          if(SourceLossMax > Vdtmin) then
             Flux_VXI(Vdt_,i,j,k,1) = max(SourceLossMax, Flux_VXI(Vdt_,i,j,k,1))
             Flux_VYI(Vdt_,i,j,k,1) = max(SourceLossMax, Flux_VYI(Vdt_,i,j,k,1))
             Flux_VZI(Vdt_,i,j,k,1) = max(SourceLossMax, Flux_VZI(Vdt_,i,j,k,1))
          end if
       end if

       !          Flux_VXI(Vdt_,i,j,k,1) = max (MaxSLSpecies_CB(i,j,k,iBlock),&
       !               Flux_VXI(Vdt_,i,j,k,1) )
       !          Flux_VYI(Vdt_,i,j,k,1) = max (MaxSLSpecies_CB(i,j,k,iBlock),&
       !               Flux_VYI(Vdt_,i,j,k,1) )
       !          Flux_VZI(Vdt_,i,j,k,1) = max (MaxSLSpecies_CB(i,j,k,iBlock),&
       !               Flux_VZI(Vdt_,i,j,k,1) )

       S_IC(rho_+1:rho_+MaxSpecies,i,j,k) = S_IC(rho_+1:rho_+MaxSpecies,i,j,k)&
            +SiSpecies_I(1:nSpecies) &
            -LiSpecies_I(1:nSpecies)

       S_IC(rho_,i,j,k) = S_IC(rho_,i,j,k) &
            +sum(SiSpecies_I(1:MaxSpecies)) &
            -sum(LiSpecies_I(1:MaxSpecies))

       S_IC(rho_+MaxSpecies+1,i,j,k) = S_IC(rho_+MaxSpecies+1,i,j,k) &
            -State_VGB(Ux_,i,j,k,iBlock)*totalLossx &
            -nu_BLK(i,j,k,iBlock)*State_VGB(Ux_,i,j,k,iBlock)

       S_IC(rho_+MaxSpecies+2,i,j,k) = S_IC(rho_+MaxSpecies+2,i,j,k) &
            -State_VGB(Uy_,i,j,k,iBlock)*totalLossx &
            -nu_BLK(i,j,k,iBlock)*State_VGB(Uy_,i,j,k,iBlock)

       S_IC(rho_+MaxSpecies+3,i,j,k) = S_IC(rho_+MaxSpecies+3,i,j,k) &
            -State_VGB(Uz_,i,j,k,iBlock)*totalLossx &
            -nu_BLK(i,j,k,iBlock)*State_VGB(Uz_,i,j,k,iBlock)

       !----- pressure and energy source terms
       if(UseOldEnergy)then
          temp = (totalSourceNumRho*kTp0 + totalPSNumRho*T300*20.) &
               -(totalLossNumx+totalRLNumRhox)*State_VGB(P_,i,j,k,iBlock)

          S_IC(rho_+MaxSpecies+8,i,j,k) = S_IC(rho_+MaxSpecies+8,i,j,k) + &
               (InvGammaMinus1*temp - 0.50*uu2*(totalLossRho) ) - &
               0.5*State_VGB(rho_,i,j,k,iBlock)*uu2*nu_BLK(i,j,k,iBlock)
          S_IC(rho_+MaxSpecies+7,i,j,k) = S_IC(rho_+MaxSpecies+7,i,j,k) + &
               (temp + 0.50*uu2*(totalSourceRho)*GammaMinus1) + &
               GammaMinus1*0.5*State_VGB(rho_,i,j,k,iBlock)*uu2*&
               nu_BLK(i,j,k,iBlock)

          if(kTi > kTn)then
             S_IC(rho_+MaxSpecies+8,i,j,k) = S_IC(rho_+MaxSpecies+8,i,j,k) &
                  -nu_BLK(i,j,k,iBlock)*totalNumRho*InvGammaMinus1*(kTi-kTn)
             S_IC(rho_+MaxSpecies+7,i,j,k) = S_IC(rho_+MaxSpecies+7,i,j,k) &
                  -nu_BLK(i,j,k,iBlock)*totalNumRho*(kTi-kTn)
          end if
       else
          temps = totalSourceNumRho*kTn            &
               + totalNumRho*(kTn-KTi)*nu_BLK(i,j,k,iBlock) &
               + totalPSNumRho*kTe0                &
               - totalLossNumRho*kTi               &
               - totalRLNumRhox*totalNumRho*KTe

!!!          if(UseTempControl.and.kTi > kT300)&
!!!               temps = temps+totalNumRho*(kT1000-KTi)*Nu_C(i,j,k)*5.0

          S_IC(rho_+MaxSpecies+8,i,j,k) = S_IC(rho_+MaxSpecies+8,i,j,k) &
               - 0.5*State_VGB(rho_,i,j,k,iBlock)*uu2*&
               nu_BLK(i,j,k,iBlock)  &
               - 0.50*uu2*(totalLossRho) &
               + InvGammaMinus1*temps

          S_IC(rho_+MaxSpecies+7,i,j,k) = S_IC(rho_+MaxSpecies+7,i,j,k) &
               + 0.5*GammaMinus1*State_VGB(rho_,i,j,k,iBlock)*uu2*&
               nu_BLK(i,j,k,iBlock)  &
               + 0.50*(GammaMinus1)*uu2*(totalSourceRho) &
               + temps

       end if

    end do; end do; end do     ! end of the i,j,k loop

    Source_VC(rho_       ,:,:,:) = Source_VC(rho_,:,:,:) + &
         S_IC(rho_,:,:,:)
    Source_VC(rho_+1:rho_+MaxSpecies,:,:,:) = &
         Source_VC(rho_+1:rho_+MaxSpecies,:,:,:) + &
         S_IC(rho_+1:rho_+MaxSpecies,:,:,:)
    Source_VC(rhoUx_     ,:,:,:) = Source_VC(rhoUx_,:,:,:) + &
         S_IC(rho_+MaxSpecies+1,:,:,:)
    Source_VC(rhoUy_     ,:,:,:) = Source_VC(rhoUy_,:,:,:) + &
         S_IC(rho_+MaxSpecies+2,:,:,:)
    Source_VC(rhoUz_     ,:,:,:) = Source_VC(rhoUz_,:,:,:) + &
         S_IC(rho_+MaxSpecies+3,:,:,:)
    Source_VC(Bx_        ,:,:,:) = Source_VC(Bx_,:,:,:) + &
         S_IC(rho_+MaxSpecies+4,:,:,:)
    Source_VC(By_        ,:,:,:) = Source_VC(By_,:,:,:) + &
         S_IC(rho_+MaxSpecies+5,:,:,:)
    Source_VC(Bz_        ,:,:,:) = Source_VC(Bz_,:,:,:) + &
         S_IC(rho_+MaxSpecies+6,:,:,:)
    Source_VC(P_         ,:,:,:) = Source_VC(P_,:,:,:) + &
         S_IC(rho_+MaxSpecies+7,:,:,:)
    Source_VC(Energy_    ,:,:,:) = Source_VC(Energy_,:,:,:) + &
         S_IC(rho_+MaxSpecies+8,:,:,:)

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_sources
  !============================================================================

  subroutine user_init_session
    use ModMain
    use ModPhysics
    use ModVarIndexes

    integer:: iBoundary
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_init_session'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    ! For Outer Boundaries
    do iBoundary=xMinBc_,zMaxBc_
       FaceState_VI(rhoHp_,iBoundary)    = SolarWindRho
       FaceState_VI(rhoO2p_,iBoundary)   = 1e-10
       FaceState_VI(rhoOp_,iBoundary)    = 1e-10
       FaceState_VI(rhoCO2p_,iBoundary)  = 1e-10
    end do
    call set_multiSp_ICs
    !    Rbody = 1.0 + 140.0e3/Mars
    BodyRho_I(1) = sum(BodyRhoSpecies_I)
    BodyP_I(1)   = sum(BodyRhoSpecies_I/MassSpecies_I)*kTp0

    FaceState_VI(rho_,body1_)=BodyRho_I(1)
    FaceState_VI(rhoHp_:rhoCO2p_,body1_) = BodyRhoSpecies_I
    FaceState_VI(P_,body1_)=BodyP_I(1)

    CellState_VI(:,Coord1MinBc_:Coord3MaxBc_) = FaceState_VI(:,xMinBc_:zMaxBc_)

    UnitUser_V(rhoHp_:rhoCO2p_)   = No2Io_V(UnitRho_)/MassSpecies_V

    if(.not.allocated(nDenNuSpecies_CBI))then
       allocate(nDenNuSpecies_CBI(nI, nJ, nK, MaxBlock, MaxNuSpecies))
       nDenNuSpecies_CBI = -1.0
    end if

    call test_stop(NameSub, DoTest)
  end subroutine user_init_session
  !============================================================================

  subroutine user_set_ICs(iBlock)

    use ModMain
    use ModAdvance
    use ModGeometry, ONLY:Xyz_DGB,r_GB,Used_GB
    use ModPhysics
    use ModNumConst
    use ModMultiFluid

    integer, intent(in) :: iBlock

    real :: CosSZA
    integer :: i,j,k,iFluid

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_ICs'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(DoTest)then
       write(*,*)'BodynDenNuSpecies_I(:)=',&
            BodynDenNuSpecies_I(:)
       WRITE(*,*)''
       write(*,*)'HNuSpecies_I(1:nNuSpecies)=',HNuSpecies_I(:)
       WRITE(*,*)''
       write(*,*)'Rbody=', Rbody
       write(*,*)''
    end if

    call set_neutral_density(iBlock)

    if(DoTest)then
       write(*,*)'usehoto=',UseHotO
       write(*,*)'nDenNuSpecies_CBI(iTest,jTest,kTest,iBlockTest,:)=',&
            nDenNuSpecies_CBI(iTest,jTest,kTest,iBlockTest,:)
       WRITE(*,*)''
       write(*,*)'nu(testcell)=', nu_BLK(iTest,jTest,kTest,iBlockTest)
       WRITE(*,*)''
       write(*,*)'Ionizationrate_CBI(testcell,CO2_)=',&
            Ionizationrate_CBI(iTest,jTest,kTest,iBlockTest,CO2_)
       write(*,*)'Ionizationrate_CBI(testcell,O_)=',&
            Ionizationrate_CBI(iTest,jTest,kTest,iBlockTest,O_)
       write(*,*)''
    end if

    do k=MinK,MaxK;do j=MinJ,MaxJ; do i=MinI,MaxI
       if (r_GB(i,j,k,iBlock)< Rbody) then
          cosSZA=(0.5+sign(0.5,Xyz_DGB(x_,i,j,k,iBlock)))*&
               Xyz_DGB(x_,i,j,k,iBlock)/max(r_GB(i,j,k,iBlock),1.0e-3)+&
               1.0e-3

          State_VGB(:,i,j,k,iBlock) = FaceState_VI(:,body1_)
          ! Convert velocity to momentum
          do iFluid = 1, nFluid
             call select_fluid(iFluid)
             State_VGB(iRhoUx,i,j,k,iBlock) = &
                  FaceState_VI(iUx,body1_)*FaceState_VI(iRho,body1_)
             State_VGB(iRhoUy,i,j,k,iBlock) = &
                  FaceState_VI(iUy,body1_)*FaceState_VI(iRho,body1_)
             State_VGB(iRhoUz,i,j,k,iBlock) = &
                  FaceState_VI(iUz,body1_)*FaceState_VI(iRho,body1_)
          end do
          !           State_VGB(rhoOp_,i,j,k,iBlock)= 0.0
          !           State_VGB(rhoO2p_,i,j,k,iBlock)= 0.0
          !           State_VGB(rhoCO2p_,i,j,k,iBlock)= 0.0

          State_VGB(rhoOp_,i,j,k,iBlock)= &
               FaceState_VI(rhoOp_,body1_)*cosSZA
          State_VGB(rhoO2p_,i,j,k,iBlock)= &
               FaceState_VI(rhoO2p_,body1_)*sqrt(cosSZA)
          State_VGB(rhoCO2p_,i,j,k,iBlock)= &
               FaceState_VI(rhoCO2p_,body1_)*cosSZA
          State_VGB(rho_,i,j,k,iBlock)  = &
               sum( State_VGB(rho_+1:rho_+MaxSpecies,i,j,k,iBlock))
          State_VGB(P_,i,j,k,iBlock) = &
               max(SolarWindP, sum(State_VGB(rho_+1:rho_+MaxSpecies,i,j,k,iBlock)&
               /MassSpecies_I(1:MaxSpecies))*kTp0 )
       else
          State_VGB(:,i,j,k,iBlock) = CellState_VI(:,Coord1MinBc_)
          State_VGB(Ux_:bz_,i,j,k,iBlock) = 0.0
       end if
    end do;end do; end do;

    do k=1,nK; do j=1,nJ; do i=1,nI
       if (Used_GB(i,j,k,iBlock).and. &
            r_GB(i,j,k,iBlock)<1.5*Rbody) then

          cosSZA=(0.5+sign(0.5,Xyz_DGB(x_,i,j,k,iBlock)))*&
               Xyz_DGB(x_,i,j,k,iBlock)/max(r_GB(i,j,k,iBlock),1.0e-3)+&
               1.0e-3

          State_VGB(rhoCO2p_,i,j,k,iBlock)= &
               Ionizationrate_CBI(i,j,k,iBlock,CO2_) &
               /nDenNuSpecies_CBI(i,j,k,iBlock,O_)   &
               /(Rate_I(CO2p_O__O2p_CO_)+Rate_I(CO2p_O__Op_CO2_))

          State_VGB(rhoOp_,i,j,k,iBlock)= &
               (Ionizationrate_CBI(i,j,k,iBlock,O_) &
               +Rate_I(CO2p_O__Op_CO2_)                &
               *State_VGB(rhoCO2p_,i,j,k,iBlock)    &
               *nDenNuSpecies_CBI(i,j,k,iBlock,O_)) &
               /(nDenNuSpecies_CBI(i,j,k,iBlock, CO2_)+4.0e6)&
               /Rate_I(Op_CO2__O2p_CO_)

          State_VGB(rhoO2p_,i,j,k,iBlock)= &
               SQRT((nDenNuSpecies_CBI(i,j,k,iBlock,O_)*&
               State_VGB(rhoCO2p_,i,j,k,iBlock)*&
               Rate_I(CO2p_O__O2p_CO_)+&
               nDenNuSpecies_CBI(i,j,k,iBlock, CO2_)*&
               State_VGB(rhoOp_,i,j,k,iBlock)*&
               Rate_I(Op_CO2__O2p_CO_)+1e-10)/Rate_I(O2p_em__O_O_))

          State_VGB(rhoO2p_:rhoCO2p_,i,j,k,iBlock)=&
               State_VGB(rhoO2p_:rhoCO2p_,i,j,k,iBlock)*&
               MassSpecies_I(O2p_:CO2p_)

       end if !(Used_GB?)

    end do; end do; end do

    do k=1,nK; do j=1,nJ; do i=1,nI

       if(.not.Used_GB(i,j,k,iBlock))CYCLE
       State_VGB(rho_,i,j,k,iBlock)   =&
            sum(State_VGB(rho_+1:rho_+MaxSpecies,i,j,k,iBlock))
       State_VGB(P_,i,j,k,iBlock)= &
            max(SolarWindP, sum(State_VGB(rho_+1:rho_+MaxSpecies,i,j,k,iBlock)&
            /MassSpecies_I(1:MaxSpecies))*kTp0)
    end do; end do; end do

    DtMax_CB(:,:,:,iBlock) = 0.00

    if(DoTest)then
       write(*,*)'initial set up'
       write(*,*)'Rate_I=',Rate_I
       write(*,*)''
       write(*,*)'rhoSpecies_GBI(testcell,1:nSpecies)=',&
            State_VGB(rho_+1:rho_+MaxSpecies,iTest,jTest,kTest,iBlockTest)
       write(*,*)'p_BLK(iTest,jTest,kTest,iBlockTest)=',&
            State_VGB(P_,iTest,jTest,kTest,iBlockTest)
       write(*,*)''
       write(*,*)'rhoSpecies_GBI(testcell+1,1:nSpecies)=',&
            State_VGB(rho_+1:rho_+MaxSpecies,iTest+1,jTest,kTest,iBlockTest)
       write(*,*)'p_BLK(iTest+1,jTest,kTest,iBlockTest)=',&
            State_VGB(P_,iTest+1,jTest,kTest,iBlockTest)
       write(*,*)''
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_ICs
  !============================================================================
  subroutine set_multiSp_ICs

    ! calculate the scale height of ion and neutal species and
    ! intial boundary value of ion species

    use ModMain
    use ModConst
    use ModIO
    use ModPhysics

    real :: Productrate0, OptDep

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_multiSp_ICs'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)then
       write(*,*)'in set_multisp_ICs, No2Io_V(UnitN_),t=',&
            No2Io_V(UnitN_),No2Io_V(UnitT_)
       write(*,*)'No2Si_V(UnitX_), temperature=',&
            No2Si_V(UnitX_), No2Si_V(UnitTemperature_)
       write(*,*)'kTp=',SolarWindP*Tnu_body_dim*2.0/SolarWindTempDim, &
            2.0*Tnu_body_dim/No2Si_V(UnitTemperature_)
       write(*,*)'BodynDenNuSpecies_dim_I(:)',&
            BodynDenNuSpdim_I(:)
    end if

    select case(SolarCond)

    case('solarmax')
       Tnu_body_dim = 134.0      ! neutral temperature
       BodynDenNuSpDim_I(CO2_)= 4.435e12
       BodynDenNuSpDim_I(O_)= 8.0283e9
       BodynDenNuSpDim_I(H_)= 1.8374e6
       BodynDenNuSpDim_I(Oh_)= 6.3119e4
       BodynDenNuSpDim_I(Ohx_)= 3.9646e3
       BodynDenNuSpDim_I(Hx_)= 7.3638e4
       BodynDenNuSpDim_I(Ox_)= 5.1736e8
       BodynDenNuSpDim_I(CO2x_)= 8.0807e10

       HNuSpeciesDim_I(O_)=13.34 ! scale height in KM
       HNuSpeciesDim_I(Ox_)=50.025
       HNuSpeciesDim_I(Oh_)=290.5
       HNuSpeciesDim_I(Ohx_)=2436.6

       HNuSpeciesDim_I(CO2_)=6.5631
       HNuSpeciesDim_I(CO2x_)=17.064

       HNuSpeciesDim_I(H_)=13.133
       HNuSpeciesDim_I(Hx_)=610.0

       RateDim_I(CO2_hv__CO2p_em_)=7.3e-7
       RateDim_I(O_hv__Op_em_) = 2.734e-7
       RateDim_I(H_hv__Hp_em_) = 8.59e-8

    case('permax')  ! for solar maximum at perihelion
       Tnu_body_dim = 137.3      ! neutral temperature
       BodynDenNuSpDim_I(CO2_)= 5.658e12
       BodynDenNuSpDim_I(O_)= 9.472e9
       BodynDenNuSpDim_I(H_)= 1.321e6
       BodynDenNuSpDim_I(Oh_)= 7.811e4
       BodynDenNuSpDim_I(Ohx_)= 4.715e3
       BodynDenNuSpDim_I(Hx_)= 4.630e4
       BodynDenNuSpDim_I(Ox_)= 5.156e8
       BodynDenNuSpDim_I(CO2x_)= 6.723e10

       HNuSpeciesDim_I(O_)=14.19 ! scale height in KM
       HNuSpeciesDim_I(Ox_)=54.74
       HNuSpeciesDim_I(Oh_)=290.5
       HNuSpeciesDim_I(Ohx_)=2436.6

       HNuSpeciesDim_I(CO2_)=6.83
       HNuSpeciesDim_I(CO2x_)=18.63

       HNuSpeciesDim_I(H_)=13.133
       HNuSpeciesDim_I(Hx_)=614.3

       RateDim_I(CO2_hv__CO2p_em_)=8.89e-7
       RateDim_I(O_hv__Op_em_) = 3.35e-7
       RateDim_I(H_hv__Hp_em_) = 9.29e-8

    case('aphmax')  ! for solar maximum at aphelion
       Tnu_body_dim = 130.5      ! neutral temperature
       BodynDenNuSpDim_I(CO2_)= 3.422e12
       BodynDenNuSpDim_I(O_)= 6.732e9
       BodynDenNuSpDim_I(H_)= 2.610e6
       BodynDenNuSpDim_I(Oh_)= 5.031e4
       BodynDenNuSpDim_I(Ohx_)= 3.296e3
       BodynDenNuSpDim_I(Hx_)= 1.207e4
       BodynDenNuSpDim_I(Ox_)= 5.192e8
       BodynDenNuSpDim_I(CO2x_)= 9.828e10

       HNuSpeciesDim_I(O_)=12.49 ! scale height in KM
       HNuSpeciesDim_I(Ox_)=45.45
       HNuSpeciesDim_I(Oh_)=290.5
       HNuSpeciesDim_I(Ohx_)=2436.6

       HNuSpeciesDim_I(CO2_)=6.29
       HNuSpeciesDim_I(CO2x_)=15.55

       HNuSpeciesDim_I(H_)=13.133
       HNuSpeciesDim_I(Hx_)=605.4

       RateDim_I(CO2_hv__CO2p_em_)=5.92e-7
       RateDim_I(O_hv__Op_em_) = 2.20e-7
       RateDim_I(H_hv__Hp_em_) = 7.90e-8

    case('solarmin')  ! for solar min condition

       Tnu_body_dim = 117.0      ! neutral temperature

       BodynDenNuSpDim_I(CO2_)= 1.1593e12
       BodynDenNuSpDim_I(O_)= 3.2278e9
       BodynDenNuSpDim_I(H_)= 1.1307e7
       BodynDenNuSpDim_I(Oh_)= 1.951e4
       BodynDenNuSpDim_I(Ohx_)= 1.5248e3
       BodynDenNuSpDim_I(Hx_)= 9.4936e5
       BodynDenNuSpDim_I(Ox_)= 5.2695e8
       BodynDenNuSpDim_I(CO2x_)= 2.2258e11

       HNuSpeciesDim_I(O_)=9.486  ! scale height in km
       HNuSpeciesDim_I(Ox_)=30.45
       HNuSpeciesDim_I(Oh_)=290.5
       HNuSpeciesDim_I(Ohx_)=2436.6

       HNuSpeciesDim_I(CO2_)=5.2667
       HNuSpeciesDim_I(CO2x_)=10.533

       HNuSpeciesDim_I(H_)=13.133
       HNuSpeciesDim_I(Hx_)=586.6

       RateDim_I(CO2_hv__CO2p_em_)=2.47e-7
       RateDim_I(O_hv__Op_em_) = 8.89e-8
       RateDim_I(H_hv__Hp_em_) = 5.58e-8

    case('permin')  ! for solar minimum at perihelion

       Tnu_body_dim = 119.9      ! neutral temperature

       BodynDenNuSpDim_I(CO2_)= 1.479e12
       BodynDenNuSpDim_I(O_)= 3.808e9
       BodynDenNuSpDim_I(H_)= 1.81e6
       BodynDenNuSpDim_I(Oh_)= 2.41e4
       BodynDenNuSpDim_I(Ohx_)= 1.81e3
       BodynDenNuSpDim_I(Hx_)= 5.97e5
       BodynDenNuSpDim_I(Ox_)= 5.25e8
       BodynDenNuSpDim_I(CO2x_)= 1.85e11

       HNuSpeciesDim_I(O_)=10.09  ! scale height in km
       HNuSpeciesDim_I(Ox_)=33.32
       HNuSpeciesDim_I(Oh_)=290.5
       HNuSpeciesDim_I(Ohx_)=2436.6

       HNuSpeciesDim_I(CO2_)=5.48
       HNuSpeciesDim_I(CO2x_)=11.50

       HNuSpeciesDim_I(H_)=13.133
       HNuSpeciesDim_I(Hx_)=590.8

       RateDim_I(CO2_hv__CO2p_em_)=3.01e-7
       RateDim_I(O_hv__Op_em_) = 1.09e-7
       RateDim_I(H_hv__Hp_em_) = 6.03e-8

    case('aphmin')  ! for solar minimum at aphelion

       Tnu_body_dim = 114.0      ! neutral temperature

       BodynDenNuSpDim_I(CO2_)= 8.945e11
       BodynDenNuSpDim_I(O_)= 2.707e9
       BodynDenNuSpDim_I(H_)= 1.61e7
       BodynDenNuSpDim_I(Oh_)= 1.56e4
       BodynDenNuSpDim_I(Ohx_)= 1.27e3
       BodynDenNuSpDim_I(Hx_)= 1.56e6
       BodynDenNuSpDim_I(Ox_)= 5.29e8
       BodynDenNuSpDim_I(CO2x_)= 2.71e11

       HNuSpeciesDim_I(O_)=8.88  ! scale height in km
       HNuSpeciesDim_I(Ox_)=27.66
       HNuSpeciesDim_I(Oh_)=290.5
       HNuSpeciesDim_I(Ohx_)=2436.6

       HNuSpeciesDim_I(CO2_)=5.05
       HNuSpeciesDim_I(CO2x_)=9.60

       HNuSpeciesDim_I(H_)=13.133
       HNuSpeciesDim_I(Hx_)=582.2

       RateDim_I(CO2_hv__CO2p_em_)=2.00e-7
       RateDim_I(O_hv__Op_em_) = 7.16e-8
       RateDim_I(H_hv__Hp_em_) = 5.13e-8

    case('permaxex')  ! for solar maximum at perihelion
       ! Ionization rates increased by 3 orders of magnitude
       
       Tnu_body_dim = 137.3      ! neutral temperature
       BodynDenNuSpDim_I(CO2_)= 5.658e12
       BodynDenNuSpDim_I(O_)= 9.472e9
       BodynDenNuSpDim_I(H_)= 1.321e6
       BodynDenNuSpDim_I(Oh_)= 7.811e4
       BodynDenNuSpDim_I(Ohx_)= 4.715e3
       BodynDenNuSpDim_I(Hx_)= 4.630e4
       BodynDenNuSpDim_I(Ox_)= 5.156e8
       BodynDenNuSpDim_I(CO2x_)= 6.723e10

       HNuSpeciesDim_I(O_)=14.19 ! scale height in KM
       HNuSpeciesDim_I(Ox_)=54.74
       HNuSpeciesDim_I(Oh_)=290.5
       HNuSpeciesDim_I(Ohx_)=2436.6

       HNuSpeciesDim_I(CO2_)=6.83
       HNuSpeciesDim_I(CO2x_)=18.63

       HNuSpeciesDim_I(H_)=13.133
       HNuSpeciesDim_I(Hx_)=614.3

       RateDim_I(CO2_hv__CO2p_em_)=8.89e-3
       RateDim_I(O_hv__Op_em_) = 3.35e-3
       RateDim_I(H_hv__Hp_em_) = 9.29e-4

    case('aphminex')  ! for solar minimum at aphelion
       ! Ionization rates decreased by nine orders of magnitude

       Tnu_body_dim = 114.0      ! neutral temperature

       BodynDenNuSpDim_I(CO2_)= 8.945e11
       BodynDenNuSpDim_I(O_)= 2.707e9
       BodynDenNuSpDim_I(H_)= 1.61e7
       BodynDenNuSpDim_I(Oh_)= 1.56e4
       BodynDenNuSpDim_I(Ohx_)= 1.27e3
       BodynDenNuSpDim_I(Hx_)= 1.56e6
       BodynDenNuSpDim_I(Ox_)= 5.29e8
       BodynDenNuSpDim_I(CO2x_)= 2.71e11

       HNuSpeciesDim_I(O_)=8.88  ! scale height in km
       HNuSpeciesDim_I(Ox_)=27.66
       HNuSpeciesDim_I(Oh_)=290.5
       HNuSpeciesDim_I(Ohx_)=2436.6

       HNuSpeciesDim_I(CO2_)=5.05
       HNuSpeciesDim_I(CO2x_)=9.60

       HNuSpeciesDim_I(H_)=13.133
       HNuSpeciesDim_I(Hx_)=582.2

       RateDim_I(CO2_hv__CO2p_em_)=2.00e-16
       RateDim_I(O_hv__Op_em_) = 7.16e-17
       RateDim_I(H_hv__Hp_em_) = 5.13e-17
       
    case ('earlymars1')
       Tnu_body_dim = 180.0      ! neutral temperature
       ! increase to 1000k to exobase

       RateDim_I(CO2_hv__CO2p_em_)=6.6e-7*6.0/2.25
       RateDim_I(O_hv__Op_em_) = 2.0e-7*6.0/2.25
       RateDim_I(H_hv__Hp_em_) = 8.59e-8*6.0/2.25

       BodynDenNuSpDim_I(CO2_)= 5.1e11
       BodynDenNuSpDim_I(O_)= 3.8e10
       BodynDenNuSpDim_I(H_)= 2.3e5
       BodynDenNuSpDim_I(Hx_)= 5.0e4
       BodynDenNuSpDim_I(Ox_)= 2.2e9
       BodynDenNuSpDim_I(CO2x_)= 6.8e9

       HNuSpeciesDim_I(O_)=24.5  ! scale height in km
       HNuSpeciesDim_I(Ox_)=100.
       HNuSpeciesDim_I(CO2_)=11.0
       HNuSpeciesDim_I(CO2x_)=32.0
       HNuSpeciesDim_I(H_)=24.0
       HNuSpeciesDim_I(Hx_)=650.

    case ('earlymars2')
       Tnu_body_dim = 180.0      ! neutral temperature
       ! increase to 1000k to exobase

       RateDim_I(CO2_hv__CO2p_em_)=6.6e-7*6.0/2.25
       RateDim_I(O_hv__Op_em_) = 2.0e-7*6.0/2.25
       RateDim_I(H_hv__Hp_em_) = 8.59e-8*6.0/2.25

       BodynDenNuSpDim_I(CO2_)= 1.5e14 !/cc
       BodynDenNuSpDim_I(O_)= 1.8e12
       BodynDenNuSpDim_I(H_)= 2.5e5
       BodynDenNuSpDim_I(Hx_)= 2.0e1
       BodynDenNuSpDim_I(Ox_)= 1.3e9
       BodynDenNuSpDim_I(CO2x_)= 1.0e9

       HNuSpeciesDim_I(O_)=25.5  ! scale height in km
       HNuSpeciesDim_I(Ox_)=190.
       HNuSpeciesDim_I(CO2_)=11.3
       HNuSpeciesDim_I(CO2x_)=51.0
       HNuSpeciesDim_I(H_)=15.0
       HNuSpeciesDim_I(Hx_)=850.

    case default
       call stop_mpi('unknow solar condition '//SolarCond)
    end select

    kTn = TNu_body_dim*Si2No_V(UnitTemperature_)
    kTi0 = kTn
    kTp0 = 2.0*kTn

    kTe0=max(Te_new_dim, Tnu_body_dim)*Si2No_V(UnitTemperature_)

    T300 = T300_dim*Si2No_V(UnitTemperature_)

    if(DoTest)then
       write(*,*)'Tnu_body=',kTn, TNu_body_dim
       write(*,*)'T300=', T300, T300_dim
       write(*,*)'Tp_body=', kTp0
    end if

    nu0=nu0_dim*No2Io_V(UnitN_)*No2Io_V(UnitT_)
    BodynDenNuSpecies_I(:)=&
         BodynDenNuSpDim_I(:)*Io2No_V(UnitN_)
    HNuSpecies_I(:)=&
         HNuSpeciesDim_I(:)*1.0e3*Si2No_V(UnitX_)

    ! normlize the reaction rate
    Rate_I(CO2_hv__CO2p_em_)= &
         RateDim_I(CO2_hv__CO2p_em_)*No2Io_V(UnitT_)
    Rate_I(O_hv__Op_em_)=  &
         RateDim_I(O_hv__Op_em_)*No2Io_V(UnitT_)
    Rate_I(CO2p_O__O2p_CO_)=  &
         RateDim_I(CO2p_O__O2p_CO_)  &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)
    Rate_I(Op_CO2__O2p_CO_)=  &
         RateDim_I(Op_CO2__O2p_CO_)*exp(log(8.0/3.0*T300/kTn)*0.39) &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)
    Rate_I(CO2p_O__Op_CO2_)=  &
         RateDim_I(CO2p_O__Op_CO2_) &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)

    Rate_I(O2p_em__O_O_)=  &
         RateDim_I(O2p_em__O_O_)*exp(log(4.0*T300/kTn)*0.56)&
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)
    Rate_I(CO2p_em__CO_O_)=  &
         RateDim_I(CO2p_em__CO_O_)*sqrt(T300/kTn)&
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)

    Rate_I(H_hv__Hp_em_)=  &
         RateDim_I(H_hv__Hp_em_)*No2Io_V(UnitT_)
    Rate_I(Hp_O__Op_H_)=  &
         RateDim_I(Hp_O__Op_H_) &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)
    Rate_I(Op_H__Hp_O_)=  &
         RateDim_I(Op_H__Hp_O_) &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)

    ReactionRate_I(CO2_hv__CO2p_em_)= &
         Rate_I(CO2_hv__CO2p_em_)*BodynDenNuSpecies_I(CO2_)
    PhoIon_I(CO2p_)=ReactionRate_I(CO2_hv__CO2p_em_)

    ReactionRate_I(O_hv__Op_em_)= &
         Rate_I(O_hv__Op_em_)*BodynDenNuSpecies_I(O_)
    PhoIon_I(Op_)=ReactionRate_I(O_hv__Op_em_)

    ! charge exchange
    ReactionRate_I(CO2p_O__O2p_CO_)= &
         Rate_I(CO2p_O__O2p_CO_)* BodynDenNuSpecies_I(O_)
    CoeffSpecies_II(O2p_,CO2p_)=ReactionRate_I(CO2p_O__O2p_CO_)

    ReactionRate_I(Op_CO2__O2p_CO_)= &
         Rate_I(Op_CO2__O2p_CO_)* BodynDenNuSpecies_I(CO2_)
    CoeffSpecies_II(O2p_, Op_)=ReactionRate_I(Op_CO2__O2p_CO_)

    ReactionRate_I(CO2p_O__Op_CO2_)= &
         Rate_I(CO2p_O__Op_CO2_)* BodynDenNuSpecies_I(O_)
    CoeffSpecies_II(Op_,CO2p_)=ReactionRate_I(CO2p_O__Op_CO2_)

    CrossSection_I=CrossSectiondim_I*No2Io_V(unitN_)*No2Si_V(unitX_)*1.0e2

    Optdep = sum(BodynDenNuSpecies_I*CrossSection_I*HNuSpecies_I)
    Productrate0 = max(exp(-Optdep), 1.0e-5)

    if(DoTest)then
       write(*,*)'=======in set_multisp=============='
       write(*,*)'BodynDenNuSpecies_I=',BodynDenNuSpecies_I
       write(*,*)'HNuSpecies_I=',HNuSpecies_I
       write(*,*)'solar min, Procductrate=', productrate0, Optdep
       write(*,*)'CrossSection_dim_I*unitUSER_n*unitSI_x=',CrossSectiondim_I,&
            No2Io_V(unitN_),No2Si_V(unitX_)
       write(*,*)''
    end if

    ! ion density at the body
    BodyRhoSpecies_I(Hp_) = SolarWindRho*0.3

    BodyRhoSpecies_I(CO2p_) = Rate_I(CO2_hv__CO2p_em_)*Productrate0*&
         BodynDenNuSpecies_I(CO2_)/BodynDenNuSpecies_I(O_)/&
         (Rate_I(CO2p_O__O2p_CO_)+Rate_I(CO2p_O__Op_CO2_))
    BodyRhoSpecies_I(Op_) = (Rate_I(O_hv__Op_em_)*Productrate0+&
         Rate_I(CO2p_O__Op_CO2_)*BodyRhoSpecies_I(CO2p_))&
         *BodynDenNuSpecies_I(O_)/(BodynDenNuSpecies_I(CO2_)+3.0e5)/&
         Rate_I(Op_CO2__O2p_CO_)
    BodyRhoSpecies_I(O2p_) = SQRT((BodynDenNuSpecies_I(O_)*&
         BodyRhoSpecies_I(CO2p_)*Rate_I(CO2p_O__O2p_CO_)+ &
         BodynDenNuSpecies_I(CO2_)*BodyRhoSpecies_I(Op_)*&
         Rate_I(Op_CO2__O2p_CO_))/Rate_I(O2p_em__O_O_))

    BodyRhoSpecies_I = BodyRhoSpecies_I*MassSpecies_I

    if(DoTest)then
       write(*,*)' set parameters of Mars: BodyRhoSpecies_I(i)=',&
            BodyRhoSpecies_I
       write(*,*)'neutral density=', BodynDenNuSpecies_I
       write(*,*)'nu0=',nu0
       write(*,*)'Rate_I=', Rate_I
       write(*,*)'RateDim_I=', RateDim_I
       !       call stop_mpi('end')
    end if
    call test_stop(NameSub, DoTest)
  end subroutine set_multiSp_ICs
  !============================================================================

  subroutine user_set_face_boundary(FBC)

    use ModMain,       ONLY: UseRotatingBc, ExtraBc_,Body1_,xMinBc_, FaceBCType
    use ModVarIndexes, ONLY: nVar, RhoOp_, RhoO2p_, RhoCO2p_, RhoHp_
    use ModPhysics,    ONLY: SolarWindRho, FaceState_VI, OmegaBody_D
    use ModCoordTransform, ONLY: cross_product

    type(FaceBCType), intent(inout):: FBC

    real:: XFace,YFace,ZFace,rFace,rFace2
    real:: uRot_D(3)
    real:: cosSZA
    real:: uDotR
    character(len=*), parameter:: NameSub = 'user_set_face_boundary'
    !--------------------------------------------------------------------------
    associate( VarsGhostFace_V => FBC%VarsGhostFace_V, &
               VarsTrueFace_V => FBC%VarsTrueFace_V, &
               FaceCoords_D => FBC%FaceCoords_D, &
               iBoundary => FBC%iBoundary )

    if(iBoundary == ExtraBc_)then
       VarsGhostFace_V = FaceState_VI(:,xMinBc_)
       RETURN
    elseif(iBoundary /= Body1_)then
       call stop_mpi(NameSub//' invalid iBoundary value')
    end if

    XFace = FaceCoords_D(1)
    YFace = FaceCoords_D(2)
    ZFace = FaceCoords_D(3)

    rFace2 = XFace**2 + YFace**2 + ZFace**2
    rFace  = sqrt(rFace2)

    ! Apply boundary conditions
    cosSZA=(0.5+sign(0.5,XFace)) * XFace/max(RFace,1.0e-3) + 1.0e-3

    VarsGhostFace_V(rhoOp_)  = BodyRhoSpecies_I(Op_) *cosSZA

    VarsGhostFace_V(rhoO2p_) = BodyRhoSpecies_I(O2p_)*sqrt(cosSZA)

    VarsGhostFace_V(rhoCO2p_)= BodyRhoSpecies_I(CO2p_)*cosSZA

    VarsGhostFace_V(rhoHp_)  = SolarWindRho*0.3

    VarsGhostFace_V(rho_) = sum(VarsGhostFace_V(rho_+1:rho_+MaxSpecies))
    VarsGhostFace_V(P_)=sum(VarsGhostFace_V(rho_+1:rho_+MaxSpecies)&
         /MassSpecies_I)*kTp0

    ! Reflective in radial direction
    uDotR = sum(VarsTrueFace_V(Ux_:Uz_)*FaceCoords_D)/rFace2
    ! bDotR = sum(VarsTrueFace_V(Bx_:Bz_)*FaceCoords_D)/rFace2

    VarsGhostFace_V(Ux_:Uz_) = VarsTrueFace_V(Ux_:Uz_) - 2*uDotR*FaceCoords_D
    ! VarsGhostFace_V(Bx_:Bz_) = VarsTrueFace_V(Bx_:Bz_) - 2*bDotR*FaceCoords_D
    VarsGhostFace_V(Bx_:Bz_) = 0.0

    ! Apply corotation?
    if (UseRotatingBc) then
       uRot_D = cross_product(OmegaBody_D, FaceCoords_D)
       VarsGhostFace_V(Ux_:Uz_) = VarsGhostFace_V(Ux_:Uz_) + 2*uRot_D
    end if

    end associate
  end subroutine user_set_face_boundary
  !============================================================================

  subroutine user_set_boundary_cells(iBlock)

    use ModGeometry,         ONLY: ExtraBc_, Xyz_DGB, xMaxBox
    use ModBoundaryGeometry, ONLY: iBoundary_GB

    integer, intent(in):: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_boundary_cells'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    where (Xyz_DGB(x_,:,:,:,iBlock) > xMaxBox) &
         iBoundary_GB(:,:,:,iBlock) = ExtraBc_

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_boundary_cells
  !============================================================================

  subroutine user_get_b0(xMinBox,yMinBox,zMinBox,B1)
    use ModMain
    use ModPhysics
    use ModNumConst

    real, intent(in) :: xMinBox,yMinBox,zMinBox
    real, intent(out), dimension(3) :: B1

    real :: R0, theta, phi, rr, X0, Y0, Z0, delta
    real, dimension(3) :: bb, B0, B2
    real :: sint, sinp, cost, cosp
    real :: xMaxBox, yMaxBox, zMaxBox

    !--------------------------------------------------------------------------

    if(.not. UseMarsB0) then
       B1(:) = 0.0
       RETURN
    end if

    call timing_start('user_get_b0')

    if(UseMso)then  ! change location from MSO to GEO
       ! rotate around Z axis to make the axis in the XZ plane
       x0 = xMinBox*cost1 + yMinBox*sint1
       y0 = -xMinBox*sint1 + yMinBox*cost1
       ! rotate around Y axis
       xMaxBox = X0*w-zMinBox*uv
       yMaxBox = Y0
       zMaxBox = X0*uv+zMinBox*w
       ! rotate back around Z axis so that the subsolar point is along the x
       ! axis
       x0=xMaxBox*cost2-yMaxBox*sint2
       z0=zMaxBox
       y0=xMaxBox*sint2+yMaxBox*cost2

    else
       X0 = xMinBox*cos(thetilt)-zMinBox*sin(thetilt)
       Y0 = yMinBox
       Z0 = xMinBox*sin(thetilt)+zMinBox*cos(thetilt)
    end if

    R0 = sqrt(X0*X0 + Y0*Y0 + Z0*Z0)
    rr = max(R0, 1.00E-6)
    if(abs(X0) < 1e-6) then
       if(Y0 < 0) then
          phi=-cPi/2.
       else
          phi=cPi/2.
       endif
    else
       if(X0 > 0) then
          phi=atan(Y0/X0)
       else
          phi=cPi+atan(Y0/X0)
       endif
    endif

    ! RotPeriodSi=0.0 if not use rotation
    ! delta=rot-tSimulation*VRad  !(Vrad=cTwoPi/RotPeriodSi)
    delta = rot

    If(RotPeriodSi > 0.0) then
       delta=rot-tSimulation/RotPeriodSi*cTwoPi
    end If

    theta=acos(Z0/rr)

    call MarsB0(R0,theta, phi+delta, bb)

    sint=sin(theta)
    cost=cos(theta)
    sinp=sin(phi)
    cosp=cos(phi)

    B0(1) = bb(1)*sint*cosp+bb(2)*cost*cosp-bb(3)*sinp
    B0(2) = bb(1)*sint*sinp+bb(2)*cost*sinp+bb(3)*cosp
    B0(3) = bb(1)*cost-bb(2)*sint

    if(UseMso)then  ! change from GEO to MSO
       B1(1) = B0(1)*cost2+B0(2)*sint2
       B1(2) = -B0(1)*sint2+B0(2)*cost2
       B1(3) = B0(3)

       B2(1)=w*B1(1)+uv*B1(3)
       B2(2)=B1(2)
       B2(3)=-uv*B1(1)+w*B1(3)

       B1(1) = B2(1)*cost1-B2(2)*sint1
       B1(2) = B2(1)*sint1+B2(2)*cost1
       B1(3) = B2(3)

    else
       B1(1) = B0(1)*cos(thetilt)+B0(3)*sin(thetilt)
       B1(2) = B0(2)
       B1(3) = -B0(1)*sin(thetilt)+B0(3)*cos(thetilt)
    end if

    ! Normalize the crustal magnetic field
    B1(1)=B1(1)*Io2No_V(UnitB_)
    B1(2)=B1(2)*Io2No_V(UnitB_)
    B1(3)=B1(3)*Io2No_V(UnitB_)

    call timing_stop('user_get_b0')
  end subroutine user_get_b0
  !============================================================================
  subroutine MarsB0(r,theta, phi, bb)

    integer, parameter:: nMax=62
    real, intent(in) :: r, theta, phi
    real, dimension(1:3),intent(out) :: bb
    ! real :: Rlgndr, dRlgndr
    integer :: NN, n, m, im
    real :: dRnm, signsx, Rmm
    real :: xtcos,xtsin
    real, dimension(0:nMax-1) :: xpcos, xpsin
    real :: a, arr, arrn, arrm, somx2, fact, temp
    real,dimension(0:nMax,0:nMax) :: Rnm
    real,dimension(0:nMax), save  :: Factor1_I, Factor2_I, Factor3_I
    real,dimension(0:nMax,0:nMax), save :: Factor1_II, Factor2_II, Factor3_II
    logical :: DoSetFactor = .true.

    !$omp threadprivate( Factor1_I, Factor2_I, Factor3_I )
    !$omp threadprivate( Factor1_II, Factor2_II, Factor3_II )

    character(len=*), parameter:: NameSub = 'MarsB0'
    !--------------------------------------------------------------------------
    if(DoSetFactor)then
       DoSetFactor = .false.
       do m = 0, nMax
          Factor1_I(m) = sqrt((2.*m+2.)*(2.*m+1.))
          Factor2_I(m) = sqrt(4.*m+6.)
          Factor3_I(m) = sqrt(2.*m+5.)
          do n = m, nMax
             if(n>m+2)then
                temp= sqrt((n-1.-m)*(n+m+1.)/(2.*n+1.)/(n-m-2.))
                Factor1_II(n,m) = sqrt((2.*n-1.)/(n-m-2.))/temp
                Factor2_II(n,m) = sqrt((n+m)/(2.*n-3.))/temp
             end if
             Factor3_II(n,m) = sqrt((n-m)*(n+m+1.))
          end do
       end do
    end if

    a=1.035336
    arr=a/r

    ! NNm=8

    ! mars_eps=1e-3

    if(r < 1.0) then
       NN=0
    else
       NN=NNm-1
    endif

    xtcos=cos(theta)
    xtsin=sin(theta)

    do im=0,NN
       xpcos(im)=cos(im*phi)
       xpsin(im)=sin(im*phi)
    end do

    bb(1)=0.0
    bb(2)=0.0
    bb(3)=0.0

    !	    somx2=sqrt((1.-xtcos)*(1.+xtcos))
    somx2=abs(xtsin)
    signsx=sign(1., xtsin)

    fact=1.
    Rmm=1.
    Rnm(0,0)=sqrt(2.)
    Rnm(1,0)=xtcos*sqrt(3.)*Rnm(0,0)
    do n=2, NN
       Rnm(n, 0)=(xtcos*sqrt((2.*n-1.)*(2.*n+1.))*Rnm(n-1,0)-&
            (n-1)*sqrt((2.*n+1.)/(2.*n-3.))*Rnm(n-2, 0))/n

    enddo ! n

    arrm=1.0

    call timing_start('crustal')

    do m=0, NN

       Rmm=Rmm*fact*somx2/Factor1_I(m)

       Rnm(m+1,m+1)=Rmm*Factor2_I(m)
       Rnm(m, m+1)=0

       fact=fact+2.

       arrm=arr*arrm
       arrn=arrm

       do n=m,NN
          arrn=arr*arrn
          ! write(*,*) 'arrn=', arrn, ' n=', n
          if(n> (m+2)) then
             Rnm(n,m+1) = xtcos*Factor1_II(n,m)*Rnm(n-1,m+1)-&
                  Factor2_II(n,m)*Rnm(n-2,m+1)

          else if(n > (m+1)) then
             Rnm(n,m+1)=xtcos*Factor3_I(m)*Rnm(m+1,m+1)
          endif

          dRnm=m*xtcos*Rnm(n,m)/xtsin-Rnm(n, m+1)*signsx* Factor3_II(n,m)

          bb(1)=bb(1)+(n+1)*arrn*Rnm(n,m)*(cmars(n,m)*xpcos(m)&
               +dmars(n,m)*xpsin(m))
          bb(2)=bb(2)-arrn*dRnm*(cmars(n,m)*&
               xpcos(m)+dmars(n,m)*xpsin(m))
          if(xtsin <= 1e-6) then
             bb(3)=0.
          else
             bb(3)=bb(3)-arrn*Rnm(n,m)*m/xtsin*(-cmars(n,m)*xpsin(m)&
                  +dmars(n,m)*xpcos(m))
          endif
       end do ! n
    end do ! m

    call timing_stop('crustal')

  end subroutine MarsB0
  !============================================================================

  subroutine user_get_log_var(VarValue, NameVar, Radius)

    use ModGeometry,   ONLY: Xyz_DGB,r_GB
    use ModMain,       ONLY: Unused_B
    use ModVarIndexes, ONLY: &
         Rho_, rhoHp_, rhoO2p_, RhoOp_, RhoCO2p_, rhoUx_, rhoUz_
    use ModAdvance,    ONLY: State_VGB,Tmp1_GB
    use ModPhysics,    ONLY: No2Si_V, UnitN_, UnitX_, UnitU_
    use ModWriteLogSatFile, ONLY: calc_sphere

    real, intent(out)           :: VarValue
    character(len=*), intent(in):: NameVar
    real, intent(in), optional  :: Radius

    integer:: i, j, k, iBlock, iVar

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_get_log_var'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub,': NameVar=', NameVar
    select case(NameVar)
    case('hpflx')
       iVar = rhoHp_
    case('opflx')
       iVar = RhoOp_
    case('o2pflx')
       iVar = RhoO2p_
    case('co2pflx')
       iVar =  RhoCO2p_
    case default
       call stop_mpi(NameSub//': wrong NameVar='//NameVar)
    end select

    do iBlock = 1, nBlock
       if (Unused_B(iBlock)) CYCLE
       do k=0,nK+1; do j=0,nJ+1; do i=0,nI+1
          Tmp1_GB(i,j,k,iBlock) = State_VGB(iVar,i,j,k,iBlock)*&
               sum(State_VGB(rhoUx_:rhoUz_,i,j,k,iBlock) &
               *   Xyz_DGB(:,i,j,k,iBlock)) &
               /r_GB(i,j,k,iBlock)/State_VGB(rho_,i,j,k,iBlock)
       end do; end do; end do
    end do
    VarValue = calc_sphere('integrate', 360, Radius, Tmp1_GB) &
         /MassSpecies_V(iVar) &
         *No2Si_V(UnitN_)*No2Si_V(UnitX_)**2*No2Si_V(UnitU_)

    call test_stop(NameSub, DoTest)
  end subroutine user_get_log_var
  !============================================================================

  subroutine Mars_Input(iBlock)

    use ModMain
    use ModPhysics
    use ModConst
    use ModGeometry, ONLY:r_GB,CellSize_DB,&
         Coord111_DB,TypeGeometry

    integer, intent(in) :: iBlock

    real, parameter :: TINY=1.0E-12
    real :: hh, theta, phi, dR, dtheta, dphi, dH, Hscale, HCO2, HO, grav
    real :: tempICO2p, tempIOp
    real :: xLat, xLong,xAlt
    integer :: i,j,k
    integer:: iAlt, jLong, kLat, ip1,jp1,kp1
    !------ Interpolation/Expolation for Tn,nCO2,nO,PCO2p,POp -----

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'Mars_Input'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    dR=CellSize_DB(x_,iBlock)
    dPhi=CellSize_DB(y_,iBlock)
    dTheta=CellSize_DB(z_,iBlock)

    select case(TypeGeometry)
    case('cartesian')
       call stop_mpi('Unknown geometry type = '//TypeGeometry)

    case('spherical','spherical_lnr')
       ! at least part of the block is outside the body
       if (r_GB(nI,1,1,iBlock) >= Rbody) then

          do k = 1, nK
             Theta = (k-1)*dTheta  + Coord111_DB(Theta_,iBlock)
             Theta = Theta*cRadToDeg ! Convert to degrees
             kLat=int((theta+87.5)/5.0+1.0)
             kp1=min(kLat+1, NLat)
             kLat = max(kLat,1)

             do j = 1, nJ
                Phi = (j-1)*dPhi  + Coord111_DB(Phi_,iBlock)
                ! Shift to -pi to +pi range
                if(Phi > cPi) Phi = Phi - cTwoPi
                Phi = Phi*cRadToDeg ! Convert to degrees
                jLong = int((Phi+180)/5.0 + 1.0)
                jp1   = min(jLong+1, nLong)
                jLong = max(jLong, 1)

                do i = nI, 1, -1
                   hh = (r_GB(i,j,k,iBlock)-1.00)*3396.00
                   !                 write(*,*)'hh=', hh, i,j,k,iBlock
                   xLong=0.2*(Phi-Long_I(jLong))
                   xLat=0.2*(Theta-Lat_I(kLat))
                   if(hh <= 100.0)then  ! inside the body
                      tempNuSpecies_CBI(i,j,k,iBlock)= &
                           tempNuSpecies_CBI(i+1,j,k,iBlock)
                      nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)=&
                           nDenNuSpecies_CBI(i+1,j,k,iBlock,CO2_)
                      nDenNuSpecies_CBI(i,j,k,iBlock,O_)= &
                           nDenNuSpecies_CBI(i+1,j,k,iBlock,O_)

                      ! tempICO2p=max(tempICO2p,TINY)
                      ! tempIOP=max(tempIOp,TINY)
                      Ionizationrate_CBI(i,j,k,iBlock,CO2_)=&
                           Ionizationrate_CBI(i+1,j,k,iBlock,CO2_)
                      Ionizationrate_CBI(i,j,k,iBlock,O_)=&
                           Ionizationrate_CBI(i+1,j,k,iBlock,O_)
                   elseif(hh <= Alt_I(NAlt))then
                      iAlt=int((hh -100.0)/10.0+1.0)
                      ip1=min(iAlt+1,NAlt)
                      if(iAlt < 1)then
                         write(*,*)'wrong ialt',iAlt
                      end if
                      xalt=0.1*(hh-Alt_I(iAlt))
                      ! interpolate
                      tempNuSpecies_CBI(i,j,k,iBlock)=          &
                           ((Temp(jLong,kLat,iAlt)*(1-xLong)       &
                           + xLong*Temp(jp1, kLat, ialt))*(1-xLat) &
                           +(Temp(jLong,kp1,iAlt)*(1-xLong)        &
                           + xLong*Temp(jp1, kp1, ialt))*xLat)*(1-xAlt)&
                           +((Temp(jLong,kLat,ip1)*(1-xLong)       &
                           + xLong*Temp(jp1, kLat, ip1))*(1-xLat)   &
                           +(Temp(jLong,kp1,ip1)*(1-xLong)         &
                           + xLong*Temp(jp1, kp1, ip1))*xLat)*xAlt

                      nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)=&
                           ((Den_CO2(jLong,kLat,iAlt)*(1-xLong)+xLong*Den_CO2(jp1, kLat, ialt))*(1-xLat)+&
                           (Den_CO2(jLong,kp1,iAlt)*(1-xLong)+xLong*Den_CO2(jp1, kp1, ialt))*xLat)*(1-xAlt)+&
                           ((Den_CO2(jLong,kLat,ip1)*(1-xLong)+xLong*Den_CO2(jp1, kLat, ip1))*(1-xLat)+&
                           (Den_CO2(jLong,kp1,ip1)*(1-xLong)+xLong*Den_CO2(jp1, kp1, ip1))*xLat)*xAlt

                      nDenNuSpecies_CBI(i,j,k,iBlock,O_)=&
                           ((Den_O(jLong,kLat,iAlt)*(1-xLong)+xLong*Den_O(jp1, kLat, ialt))*(1-xLat)+&
                           (Den_O(jLong,kp1,iAlt)*(1-xLong)+xLong*Den_O(jp1, kp1, ialt))*xLat)*(1-xAlt)+&
                           ((Den_O(jLong,kLat,ip1)*(1-xLong)+xLong*Den_O(jp1, kLat, ip1))*(1-xLat)+&
                           (Den_O(jLong,kp1,ip1)*(1-xLong)+xLong*Den_O(jp1, kp1, ip1))*xLat)*xAlt

                      tempICO2p=&
                           ((ICO2p(jLong,kLat,iAlt)*(1-xLong)+xLong*ICO2p(jp1, kLat, ialt))*(1-xLat)+&
                           (ICO2p(jLong,kp1,iAlt)*(1-xLong)+xLong*ICO2p(jp1, kp1, ialt))*xLat)*(1-xAlt)+&
                           ((ICO2p(jLong,kLat,ip1)*(1-xLong)+xLong*ICO2p(jp1, kLat, ip1))*(1-xLat)+&
                           (ICO2p(jLong,kp1,ip1)*(1-xLong)+xLong*ICO2p(jp1, kp1, ip1))*xLat)*xAlt

                      tempIOP=&
                           ((IOp(jLong,kLat,iAlt)*(1-xLong)+xLong*IOp(jp1, kLat, ialt))*(1-xLat)+&
                           (IOp(jLong,kp1,iAlt)*(1-xLong)+xLong*IOp(jp1, kp1, ialt))*xLat)*(1-xAlt)+&
                           ((IOp(jLong,kLat,ip1)*(1-xLong)+xLong*IOp(jp1, kLat, ip1))*(1-xLat)+&
                           (IOp(jLong,kp1,ip1)*(1-xLong)+xLong*IOp(jp1, kp1, ip1))*xLat)*xAlt

                      tempICO2p=max(tempICO2p,TINY)
                      tempIOP=max(tempIOp,TINY)
                      ! Ionizationrate_CBI(i,j,k,iBlock,CO2_)=tempICO2p*nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)
                      ! Ionizationrate_CBI(i,j,k,iBlock,O_)=tempIOP*nDenNuSpecies_CBI(i,j,k,iBlock,O_)
                      Ionizationrate_CBI(i,j,k,iBlock,CO2_)=tempICO2p
                      Ionizationrate_CBI(i,j,k,iBlock,O_)=tempIOP
                   else  ! hh > Alt_I(NAlt)

                      dH= hh - Alt_I(NAlt)
                      tempNuSpecies_CBI(i,j,k,iBlock)= &
                           (Temp(jLong,kLat,NAlt)*(1-xLong)+xLong*Temp(jp1, kLat,NAlt))*(1-xLat)+&
                           (Temp(jLong,kp1,NAlt)*(1-xLong)+xLong*Temp(jp1,kp1,NAlt))*xLat

                      tempICO2p=&
                           (ICO2p(jLong,kLat,NAlt)*(1-xLong)+xLong*ICO2p(jp1, kLat, NAlt))*(1-xLat)+&
                           (ICO2p(jLong,kp1,NAlt)*(1-xLong)+xLong*ICO2p(jp1, kp1, NAlt))*xLat

                      tempIOP=&
                           (IOp(jLong,kLat,NAlt)*(1-xLong)+xLong*IOp(jp1, kLat, NAlt))*(1-xLat)+&
                           (IOp(jLong,kp1,NAlt)*(1-xLong)+xLong*IOp(jp1, kp1, NAlt))*xLat

                      ! grav=3.72/r_GB(i,j,k,iBlock)/r_GB(i,j,k,iBlock)
                      grav=3.72/(1.0+300.0/3396.0)/(1.0+300.0/3396.0)

                      Hscale=cBoltzmann*&
                           tempNuSpecies_CBI(i,j,k,iBlock)/grav/cProtonMass! in m unit

                      HCO2= Hscale/NuMassSpecies_I(CO2_)/1.0e3
                      HO= Hscale/NuMassSpecies_I(O_)/1.0e3

                      nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)=&
                           ((Den_CO2(jLong,kLat,NAlt)*(1-xLong)+xLong*Den_CO2(jp1, kLat,Nalt))*(1-xLat)+&
                           (Den_CO2(jLong,kp1,NAlt)*(1-xLong)+xLong*Den_CO2(jp1,kp1,Nalt))*xLat)&
                           *exp(-dH/HCO2)
                      nDenNuSpecies_CBI(i,j,k,iBlock,O_)=&
                           ((Den_O(jLong,kLat,NAlt)*(1-xLong)+xLong*Den_O(jp1, kLat,Nalt))*(1-xLat)+&
                           (Den_O(jLong,kp1,NAlt)*(1-xLong)+xLong*Den_O(jp1,kp1,Nalt))*xLat)&
                           *exp(-dH/HO)

                      tempICO2p=max(tempICO2p,TINY)
                      tempIOP=max(tempIOp,TINY)
                      ! Ionizationrate_CBI(i,j,k,iBlock,CO2_)=tempICO2p*nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)
                      ! Ionizationrate_CBI(i,j,k,iBlock,O_)=tempIOP*nDenNuSpecies_CBI(i,j,k,iBlock,O_)
                      Ionizationrate_CBI(i,j,k,iBlock,CO2_)=tempICO2p
                      Ionizationrate_CBI(i,j,k,iBlock,O_)=tempIOP

                   end if ! hh < or > 300km
                end do
             end do
          end do
       end if
    case default

       call stop_mpi('Unknown geometry type = '//TypeGeometry)

    end select
    if(DoTest)then
       write(*,*)'Mars input end', &
            dR,dPhi,dTheta, iBlock, &
            maxval(nDenNuSpecies_CBI(nI,:,:,iBlock,CO2_)),&
            minval(nDenNuSpecies_CBI(nI,:,:,iBlock,CO2_)),&
            maxval(r_GB(nI,:,:,iBlock)),&
            minval(r_GB(1,:,:,iBlock))
       write(*,*)'Mars input end2',&
            iBlock, maxval(nDenNuSpecies_CBI(nI,:,:,iBlock,O_)),&
            minval(nDenNuSpecies_CBI(nI,:,:,iBlock,O_)),&
            maxval(Ionizationrate_CBI(nI,:,:,iBlock,CO2_)),&
            minval(Ionizationrate_CBI(nI,:,:,iBlock,O_)),&
            maxval(r_GB(nI,:,:,iBlock)),&
            minval(r_GB(1,:,:,iBlock))
    end if
    call test_stop(NameSub, DoTest, iBlock)
  end subroutine Mars_input
  !============================================================================

  subroutine ua_input(iBlock)

    use ModGeometry, ONLY: TypeGeometry, r_GB
    use ModPhysics,  ONLY: rBody

    integer, intent(in) :: iBlock

    integer :: i, j, k
    real :: hh

    logical:: DoTest

    character(len=*), parameter:: NameSub = 'ua_input'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    select case(TypeGeometry)
    case('cartesian')
       call stop_mpi('Unknown geometry type = '//TypeGeometry)

    case('spherical','spherical_lnr')
       ! at least part of the block is outside the body
       if (r_GB(nI,1,1,iBlock) >= Rbody) then
          do k = 1, nK; do j = 1, nJ; do i = nI, 1, -1
             hh = (r_GB(i,j,k,iBlock)-1.00)*3396.00
             if(hh <= 100.0)then  ! inside the body
                tempNuSpecies_CBI(i,j,k,iBlock)= &
                     tempNuSpecies_CBI(i+1,j,k,iBlock)
                nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)=&
                     nDenNuSpecies_CBI(i+1,j,k,iBlock,CO2_)
                nDenNuSpecies_CBI(i,j,k,iBlock,O_)= &
                     nDenNuSpecies_CBI(i+1,j,k,iBlock,O_)
                Ionizationrate_CBI(i,j,k,iBlock,CO2_)=&
                     Ionizationrate_CBI(i+1,j,k,iBlock,CO2_)
                Ionizationrate_CBI(i,j,k,iBlock,O_)=&
                     Ionizationrate_CBI(i+1,j,k,iBlock,O_)
             elseif(r_GB(i,j,k,iBlock) <= 3.0*rBody)then
                tempNuSpecies_CBI(i,j,k,iBlock) = UaState_VCB(1,i,j,k,iBlock)
                nDenNuSpecies_CBI(i,j,k,iBlock,CO2_) = &
                     UaState_VCB(2,i,j,k,iBlock)
                nDenNuSpecies_CBI(i,j,k,iBlock,O_) = &
                     UaState_VCB(3,i,j,k,iBlock)
                Ionizationrate_CBI(i,j,k,iBlock,CO2_) = &
                     UaState_VCB(4,i,j,k,iBlock)
                Ionizationrate_CBI(i,j,k,iBlock,O_) = &
                     UaState_VCB(5,i,j,k,iBlock)
             end if
          end do; end do; end do
       end if

    case default
       call stop_mpi('Unknown geometry type = '//TypeGeometry)
    end select

  end subroutine ua_input
  !============================================================================

  subroutine set_neutral_density(iBlock)

    use ModMain
    use ModAdvance
    use ModGeometry, ONLY:Xyz_DGB,r_GB
    use ModPhysics
    use ModNumConst

    integer, intent(in):: iBlock

    real :: CosSZA, Optdep
    integer :: i, j, k

    ! Variables for chapman function
    real :: Xp, chap_y, chap, sinSZA

    logical:: DoTest, DoTestCell
    character(len=*), parameter:: NameSub = 'set_neutral_density'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! calculate neutral
    do k=1,nK; do j=1,nJ; do i=1,nI

       DoTestCell= DoTest .and. i==iTest .and. j==jTest .and. k==kTest

       if(r_GB(i,j,k,iBlock)<= Rbody)then
          nDenNuSpecies_CBI(i,j,k,iBlock,:)=&
               BodynDenNuSpecies_I(:)
       else if(r_GB(i,j,k,iBlock)< 3.0) then
          nDenNuSpecies_CBI(i,j,k,iBlock,:)=&
               BodynDenNuSpecies_I(:)* &
               exp(-(r_GB(i,j,k,iBlock)-Rbody)&
               /HNuSpecies_I(:))
       else
          nDenNuSpecies_CBI(i,j,k,iBlock,:)=0.0
       end if

    end do;end do;end do

    ! calculate optical depth and producation rate
    do k=1,nK; do j=1,nJ; do i=1,nI
       if(.not.UseChapman)then
          cosSZA=(0.5+sign(0.5,Xyz_DGB(x_,i,j,k,iBlock)))*&
               Xyz_DGB(x_,i,j,k,iBlock)/max(r_GB(i,j,k,iBlock),1.0e-3)&
               +5.0e-4
          Optdep =max( sum(nDenNuSpecies_CBI(i,j,k,iBlock,1:MaxNuSpecies)*&
               CrossSection_I(1:MaxNuSpecies)*HNuSpecies_I(1:MaxNuSpecies)),&
               6.0e-3)/cosSZA
          if( Optdep<11.5 .and. Xyz_DGB(x_,i,j,k,iBlock) > 0.0) then
             Productrate_CB(i,j,k,iBlock) = max(exp(-Optdep), 1.0e-5)
          else
             Productrate_CB(i,j,k,iBlock) = 1.0e-5
          end if
       else
          Optdep=HNuSpecies_I(CO2_)*nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)*&
               CrossSection_I(CO2_)
          cosSZA = Xyz_DGB(x_,i,j,k,iBlock) /r_GB(i,j,k,iBlock)
          if(Optdep > 13.8) then
             Productrate_CB(i,j,k,iBlock) = 1.0e-6*&
                  max(cosSZA, 1.0e-6)
          else
             Xp=r_GB(i,j,k,iBlock)/HNuSpecies_I(1)
             chap_y= sqrt(0.5*Xp)*abs(cosSZA)
             if(cosSZA > 0.0)then   ! SZA<90 deg (equation 13)
                if(chap_y<8.0)then
                   chap = sqrt(3.1415926/2.0*Xp)*&
                        (1.0606963+0.5564383*chap_y)/&
                        (1.0619896+1.7245609*chap_y+chap_y*chap_y)
                elseif(chap_y<100.0)then
                   chap = sqrt(3.1415926/2.0*Xp)*&
                        0.56498823/(0.6651874+chap_y)
                else
                   chap=0.0
                end if
             elseif(cosSZA > -0.5) then
                ! 120 >SZA > 90 deg (equation 15) Smith and Smith, 1972
                sinSZA = sqrt(1.0 - cosSZA**2)
                if(chap_y<8.0)then
                   chap =sqrt(2.0*3.1415926*Xp)* &
                        (sqrt(sinSZA)*exp(Xp*(1.0-sinSZA)) &
                        -0.5*(1.0606963+0.5564383*chap_y)/ &
                        (1.0619896+1.7245609*chap_y+chap_y*chap_y))
                elseif(chap_y<100.0)then
                   chap =sqrt(2.0*3.1415926*Xp)* &
                        (sqrt(sinSZA)*exp(Xp*(1.0-sinSZA)) &
                        -0.5*0.56498823/(0.6651874+chap_y))
                else
                   chap =0.0
                end if
             else
                chap =1.0e10
             end if

             Optdep=Optdep*chap
             Productrate_CB(i,j,k,iBlock) = max(exp(-Optdep), 1.0e-6)

          end if
       end if
    end do; end do; end do
    do k=1,nK; do j=1,nJ; do i=1,nI
       if(UseHotO) then
          nu_BLK(i,j,k,iBlock)=&
               sum(nDenNuSpecies_CBI(i,j,k,iBlock,:))*nu0

          nDenNuSpecies_CBI(i,j,k,iBlock,O_)= &
               nDenNuSpecies_CBI(i,j,k,iBlock,O_)+ &
               nDenNuSpecies_CBI(i,j,k,iBlock,Ox_)

          nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)= &
               nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)+ &
               nDenNuSpecies_CBI(i,j,k,iBlock,CO2x_)

          nDenNuSpecies_CBI(i,j,k,iBlock,O_)= &
               nDenNuSpecies_CBI(i,j,k,iBlock,O_)+ &
               nDenNuSpecies_CBI(i,j,k,iBlock,Oh_)+&
               nDenNuSpecies_CBI(i,j,k,iBlock,Ohx_)

          nDenNuSpecies_CBI(i,j,k,iBlock,H_)= &
               nDenNuSpecies_CBI(i,j,k,iBlock,H_)+ &
               nDenNuSpecies_CBI(i,j,k,iBlock,Hx_)

       else
          nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)= &
               nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)+ &
               nDenNuSpecies_CBI(i,j,k,iBlock,CO2x_)

          nDenNuSpecies_CBI(i,j,k,iBlock,O_)= &
               nDenNuSpecies_CBI(i,j,k,iBlock,O_)+ &
               nDenNuSpecies_CBI(i,j,k,iBlock,Ox_)

          nu_BLK(i,j,k,iBlock)=(nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)+&
               nDenNuSpecies_CBI(i,j,k,iBlock,O_))*nu0

          nDenNuSpecies_CBI(i,j,k,iBlock,H_)= 1.0e-5

       end if

    end do; end do; end do

    if(UseMarsAtm)then
       if(allocated(UaState_VCB))then
          call ua_input(iBlock)
       else
          if(maxval(r_GB(:,:,:,iBlock))<3.0*Rbody) call Mars_input(iBlock)
       end if

       do k=1,nK; do j=1,nJ; do i=1,nI
          if(UseHotO) then
             nDenNuSpecies_CBI(i,j,k,iBlock,Oh_)= &
                  nDenNuSpecies_CBI(i,j,k,iBlock,Oh_)+&
                  nDenNuSpecies_CBI(i,j,k,iBlock,Ohx_)

             nDenNuSpecies_CBI(i,j,k,iBlock,O_)= &
                  nDenNuSpecies_CBI(i,j,k,iBlock,O_)+ &
                  nDenNuSpecies_CBI(i,j,k,iBlock,Oh_)

             nu_BLK(i,j,k,iBlock)=(nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)+&
                  nDenNuSpecies_CBI(i,j,k,iBlock,O_)+&
                  nDenNuSpecies_CBI(i,j,k,iBlock,H_) )*nu0
          else
             nu_BLK(i,j,k,iBlock)=(nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)+&
                  nDenNuSpecies_CBI(i,j,k,iBlock,O_))*nu0

             nDenNuSpecies_CBI(i,j,k,iBlock,H_)= 1.0e-5

          end if

          Ionizationrate_CBI(i,j,k,iBlock,CO2_)=&
               Ionizationrate_CBI(i,j,k,iBlock,CO2_)*&
               nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)
          Ionizationrate_CBI(i,j,k,iBlock,O_)=&
               Ionizationrate_CBI(i,j,k,iBlock,O_)*&
               nDenNuSpecies_CBI(i,j,k,iBlock,O_)

       end do; end do; end do
    else
       do k=1,nK; do j=1,nJ; do i=1,nI
          Ionizationrate_CBI(i,j,k,iBlock,O_)= &
               Rate_I(O_hv__Op_em_)&
               *nDenNuSpecies_CBI(i,j,k,iBlock,O_)&
               *Productrate_CB(i,j,k,iBlock)

          Ionizationrate_CBI(i,j,k,iBlock,CO2_)= &
               Rate_I(CO2_hv__CO2p_em_)&
               *nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)&
               *Productrate_CB(i,j,k,iBlock)
       end do;end do; end do
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine set_neutral_density
  !============================================================================

end module ModUser
!==============================================================================
