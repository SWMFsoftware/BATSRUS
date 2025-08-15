!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModUser

  ! This is the user module for Venus

  use ModUserEmpty,                                     &
       IMPLEMENTED1 => user_read_inputs,                &
       IMPLEMENTED2 => user_init_session,               &
       IMPLEMENTED3 => user_set_ics,                    &
       IMPLEMENTED5 => user_set_face_boundary,          &
       IMPLEMENTED6 => user_calc_sources_impl,          &
       IMPLEMENTED7 => user_init_point_implicit,        &
       IMPLEMENTED8 => user_update_states,              &
       IMPLEMENTED9 => user_set_resistivity,            &
       IMPLEMENTED10=> user_get_log_var,                &
       IMPLEMENTED11=> user_action

  use ModPhysics, ONLY: BodyRhoSpecies_I
  use ModAdvance, ONLY: nSpecies
  use ModSize
  use ModVarIndexes, ONLY: rho_, Ux_, Uz_,p_,Bx_, Bz_, MassSpecies_V
  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest, iBlockTest, iProc

  include 'user_module.h' ! list of public methods

  ! Here you must define a user routine Version number and a
  ! descriptive string.
  character (len=*), parameter :: NameUserFile = "ModUserVenus.f90"
  character (len=*), parameter :: NameUserModule = &
       'Venus 4 species MHD code, Yingjuan Ma'

  ! Radius within which the point implicit scheme should be used
  real :: rPointImplicit = 2.0

  ! Venus stuff
  integer, parameter :: MaxSpecies=4, MaxNuSpecies=3, MaxReactions=10
  integer :: nNuSpecies=3
  ! number density of neutral Species
  real, allocatable :: nDenNuSpecies_CBI(:,:,:,:,:)

  ! production rate according to optical depth
  real, allocatable :: Productrate_CB(:,:,:,:)

  real, dimension(MaxReactions):: ReactionRate_I
  real, dimension(MaxReactions,MaxSpecies):: CoeffSpecies_II, &
       dSdRho_II ! , dLdRho_II
  real, dimension(MaxSpecies):: LossSpecies_I, &
       SiSpecies_I, LiSpecies_I, PhoIon_I, Recb_I
  !    dStndRho_I, dLtdRho_I, dLtndNumRho_I
  !$omp threadprivate( ReactionRate_I, CoeffSpecies_II, dSdRho_II )
  !$omp threadprivate( LossSpecies_I, SiSpecies_I, LiSpecies_I )
  !$omp threadprivate( PhoIon_I, Recb_I )

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
       O_hv__Op_em_=2     ,&! O+hv-->Op+em
       CO2p_O__O2p_CO_=3  ,&! CO2p+O-->O2p+CO
       Op_CO2__O2p_CO_=4  ,&! Op+CO2-->O2p+CO
       CO2p_O__Op_CO2_=5  ,&! CO2p+O-->Op+CO2
       O2p_em__O_O_=6     ,&! O2p+em-->O+O
       CO2p_em__CO_O_=7   ,&! CO2p+em-->CO+O
       Hp_O__Op_H_=8      ,&! Hp+O-->Op+H
       Op_H__Hp_O_=9      ,&! Op+H-->Hp+O
       H_hv__Hp_em_=10      ! H+hv-->Hp+em

  real, dimension(MaxReactions) :: Rate_I
  real, dimension(MaxReactions) :: &  ! for solar maximum condition, venus
       Ratedim_I=[3.270e-6,1.224e-6, 1.64e-10, 1.1e-9, &
       9.60e-11, 7.38e-8, 3.1e-7, 5.084e-10, 6.4e-10, 0.0 ]  ! cm^3 s^(-1)

  ! CO2: 1.695e-6/0.72^2=3.26968  ; 6.346/0.72^2 = 12.24
  integer, parameter :: &! order of ion species
       Hp_  =1, &
       O2p_ =2, &
       Op_  =3, &
       CO2p_=4

  real, dimension(MaxSpecies)::  &
       MassSpecies_I=[1., 32., 16., 44. ]  ! atm

  !  MassSpecies_I(Hp_)=1	 ! atm
  !  MassSpecies_I(CO2p_)=44     ! atm
  !  MassSpecies_I(O2p_)=32      ! atm
  !  MassSpecies_I(Op_)=16       ! atm

  integer, parameter :: & ! order of Neutral species
       CO2_=1 ,&
       O_  =2 ,&
       H_  =3

  real, dimension(MaxNuSpecies):: CrossSection_I
  real, dimension(MaxNuSpecies), parameter:: &
       CrossSectionDim_I=[2.6e-17,1.5e-17,0.0]

  !  NuMassSpecies_I(CO2_)=44	! atm
  !  NuMassSpecies_I(O_)=16	! atm

  real, dimension(MaxNuSpecies):: HNuSpecies_I,&
       HNuSpeciesDim_I=[7.1e3,19.1e3, 1000.e3]
  ! scale height corresponding to 100km density

  real, dimension(MaxNuSpecies):: BodynDenNuSpecies_I,&
       BodynDenNuSpDim_I=[2.5e12,7.0e10, 0.0] ! density at 100km

  ! altitude corresponding to neutral density
  real, parameter :: Altitude0=100.0e3

  !  real :: body_Tn_dim = 2000.0! neutral temperature at the body
  real :: kTn, kTi0, kTp0  ! dimensionless temperature of neutral, &
  ! new created ions, plasma at the body
  real :: Te_new_dim=3000., KTe0 ! temperature of new created electrons

  !  real :: XiT0, XiTx ! dimensionless temperature of new created ions
  !  real :: Ti_body_dim=300.0  ! ion temperature at the body
  !  real :: Ti_body_dim=1000.0  ! ion temperature at the body
  !  real :: Tnu_body_dim=1000.0, Tnu_body, Tnu, Tnu_dim ! neutral temperature
  real, parameter :: T300_dim = 300.0
  real :: T300
  real, allocatable :: nu_BLK(:,:,:,:)
  real :: nu0_dim=1.0e-9,nu0  ! value from Tanaka, 1997, JGR

  character (len=10) :: type_innerbcs='reflect'
  character (len=10) :: SolarCond='solarmax  '

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
       if(.not.allocated(nu_BLK))then
          allocate(nu_BLK(nI,nJ,nK,MaxBlock))
          allocate(nDenNuSpecies_CBI(nI,nJ,nK,MaxBlock,MaxNuSpecies))
          allocate(Productrate_CB(nI,nJ,nK,MaxBlock))
          allocate(MaxSiSpecies_CB(nI,nJ,nK,MaxBlock))
          allocate(MaxLiSpecies_CB(nI,nJ,nK,MaxBlock))
          allocate(MaxSLSpecies_CB(nI,nJ,nK,MaxBlock))
       end if
    case('clean module')
       if(allocated(nu_BLK)) &
            deallocate(nu_BLK)
       if(allocated(nDenNuSpecies_CBI)) &
            deallocate(nDenNuSpecies_CBI)
       if(allocated(Productrate_CB)) &
            deallocate(Productrate_CB)
       if(allocated(MaxSiSpecies_CB)) &
            deallocate(MaxSiSpecies_CB)
       if(allocated(MaxLiSpecies_CB)) &
            deallocate(MaxLiSpecies_CB)
       if(allocated(MaxSLSpecies_CB)) &
            deallocate(MaxSLSpecies_CB)
    end select
    call test_stop(NameSub, DoTest)
  end subroutine user_action
  !============================================================================

  subroutine user_read_inputs
    use ModMain
    use ModReadParam

    character (len=100) :: NameCommand
    integer:: number

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

       case("#SOLARCON") ! solar cycle condition
          call read_var('SolarCon',SolarCond)

       case('#IonNeuCollision')
          call read_var('nu0_dim',nu0_dim)

       case('#REACTION')
          call read_var('number',number)
          Ratedim_I(number)=0.0

       case('#INNERBCS')
          call read_var('type_innerbcs',type_innerbcs)

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

    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE
       UseUserPointImplicit_B(iBlock) = &
            r_GB(1,1,1,iBlock) <= rPointImplicit .and. &
            r_GB(nI,1,1,iBlock) > rBody
    end do

    ! Note that energy is not an independent variable for the
    ! point implicit scheme. The pressure is an independent variable,
    ! and in this example there is no implicit pressure source term.

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

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_calc_sources_impl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    call user_sources(iBlock, Source_VC)

    call test_stop(NameSub, DoTest)
  end subroutine user_calc_sources_impl
  !============================================================================

  subroutine user_sources(iBlock, Source_VC)

    use ModMain, ONLY: iNewGrid, iNewDecomposition, Unused_B, nBlock
    use ModAdvance,  ONLY: State_VGB,Flux_VXI,Flux_VYI,Flux_VZI, Vdt_
    use ModVarIndexes, ONLY: Rho_, Ux_, Uy_, Uz_, &
         RhoUx_, RhoUy_, RhoUz_, P_, Energy_, Bx_, By_, Bz_
    use ModGeometry, ONLY: Xyz_DGB, r_GB
    use ModPhysics,  ONLY: Rbody, InvGammaMinus1, GammaMinus1
    use ModPointImplicit, ONLY: UsePointImplicit
    use BATL_lib, ONLY: CellVolume_GB

    integer, intent(in) :: iBlock
    real, intent(inout) :: Source_VC(:,:,:,:)

    ! Variables required by this user subroutine
    real :: S_IC(MaxSpecies+9,nI,nJ,nK)
    integer:: i, j, k, iSpecies
    real :: inv_rho, inv_rho2, uu2, cosSZA, Productrate, kTi, kTe
    real :: totalPSNumRho, totalRLNumRhox, temps
    real :: SourceLossMax, vdtmin
    real :: OptDep

    integer :: iLastGrid=-1, iLastDecomposition=-1, iBlockLoop
    !$omp threadprivate( iLastGrid, iLastDecomposition )

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_sources'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    S_IC = 0.0

    if(iNewGrid /= iLastGrid .or. iNewDecomposition /= iLastDecomposition)then
       iLastGrid          = iNewGrid
       iLastDecomposition = iNewDecomposition

       if(iProc==0) write(*,*)'Calculating neutral density for Venus'

       do iBlockLoop = 1, nBlock
          if(Unused_B(iBlockLoop)) CYCLE
          do k=1,nK; do j=1,nJ; do i=1,nI
             if(r_GB(i,j,k,iBlockLoop)<= Rbody)then
                nDenNuSpecies_CBI(i,j,k,iBlockLoop,:) =&
                     BodynDenNuSpecies_I(:)
             else if(r_GB(i,j,k,iBlockLoop)< 2.0) then
                nDenNuSpecies_CBI(i,j,k,iBlockLoop,:) =&
                     BodynDenNuSpecies_I(:)* &
                     exp(-(r_GB(i,j,k,iBlockLoop)-Rbody)&
                     /HNuSpecies_I(:))
             else
                nDenNuSpecies_CBI(i,j,k,iBlockLoop,:) = 0.0
             end if
          end do; end do; end do

          do k=1,nK; do j=1,nJ; do i=1,nI
             nu_BLK(i,j,k,iBlockLoop) = &
                  sum(nDenNuSpecies_CBI(i,j,k,iBlockLoop,1:nNuSPecies))*nu0
          end do; end do; end do
          ! calculate optical depth and producation rate
          do k=1,nK; do j=1,nJ; do i=1,nI
             cosSZA = (0.5 + sign(0.5,Xyz_DGB(x_,i,j,k,iBlockLoop)))*&
                  Xyz_DGB(x_,i,j,k,iBlockLoop) / &
                  max(r_GB(i,j,k,iBlockLoop),1.0e-3) + 5.0e-4
             OptDep = max( &
                  sum(nDenNuSpecies_CBI(i,j,k,iBlockLoop,1:MaxNuSpecies)*&
                  CrossSection_I(1:MaxNuSpecies)*&
                  HNuSpecies_I(1:MaxNuSpecies)),6.0e-3)/cosSZA
             if(OptDep<11.5 .and. Xyz_DGB(x_,i,j,k,iBlockLoop) > 0.0) then
                Productrate_CB(i,j,k,iBlockLoop) = max(exp(-OptDep), 1.0e-5)
             else
                Productrate_CB(i,j,k,iBlockLoop) = 1.0e-5
             end if

          end do; end do; end do

       end do
    end if

    do k=1,nK; do j=1,nJ; do i=1,nI
       if(r_GB(i,j,k,iBlock) > Rbody .and. r_GB(i,j,k,iBlock) < 2.0)then
          inv_rho = 1.00/State_VGB(rho_,i,j,k,iBlock)
          inv_rho2 = inv_rho*inv_rho
          uu2 =(State_VGB(Ux_,i,j,k,iBlock)*State_VGB(Ux_,i,j,k,iBlock)  &
               +State_VGB(Uy_,i,j,k,iBlock)*State_VGB(Uy_,i,j,k,iBlock)  &
               +State_VGB(Uz_,i,j,k,iBlock)*State_VGB(Uz_,i,j,k,iBlock)) &
               *inv_rho2

          ReactionRate_I=0.0
          CoeffSpecies_II(:,:)=0.0
          PhoIon_I(:)=0.0
          Recb_I(:)=0.0
          LossSpecies_I=0.0
          ! totalNumRho=0.0
          SiSpecies_I(:)=0.0
          LiSpecies_I(:)=0.0

          totalLossRho=0.0
          totalLossNumRho=0.0
          totalSourceNumRho=0.0
          totalLossx=0.0
          totalLossNumx=0.0
          totalPSNumRho=0.0
          totalRLNumRhox=0.0

          totalNumRho = sum(State_VGB(rho_+1:rho_+nSpecies,i,j,k,iBlock) &
               /MassSpecies_I(1:nSpecies))
          MaxSLSpecies_CB(i,j,k,iBlock)=1.0e-3

          Productrate = Productrate_CB(i,j,k,iBlock)

          ReactionRate_I(CO2_hv__CO2p_em_) = &
               Rate_I(CO2_hv__CO2p_em_)&
               *nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)
          PhoIon_I(CO2p_) = ReactionRate_I(CO2_hv__CO2p_em_) &
               *Productrate
          ReactionRate_I(O_hv__Op_em_) = &
               Rate_I(O_hv__Op_em_)&
               *nDenNuSpecies_CBI(i,j,k,iBlock,O_)
          PhoIon_I(Op_) = ReactionRate_I(O_hv__Op_em_) &
               *Productrate

          kTi = State_VGB(p_,i,j,k,iBlock)/totalNumRho/2.0
          kTe = kTi

          ! charge exchange
          ReactionRate_I(CO2p_O__O2p_CO_) = &
               Rate_I(CO2p_O__O2p_CO_)&
               * nDenNuSpecies_CBI(i,j,k,iBlock,O_)
          CoeffSpecies_II(O2p_,CO2p_) = ReactionRate_I(CO2p_O__O2p_CO_)

          ReactionRate_I(Op_CO2__O2p_CO_) = &
               Rate_I(Op_CO2__O2p_CO_)&
               * nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)&
               *exp(log(kTn/kTi)*0.39)
          CoeffSpecies_II(O2p_, Op_)=ReactionRate_I(Op_CO2__O2p_CO_)

          ReactionRate_I(CO2p_O__Op_CO2_) = &
               Rate_I(CO2p_O__Op_CO2_)&
               * nDenNuSpecies_CBI(i,j,k,iBlock,O_)
          CoeffSpecies_II(Op_,CO2p_)=ReactionRate_I(CO2p_O__Op_CO2_)

          ReactionRate_I(O2p_em__O_O_) = Rate_I(O2p_em__O_O_)
          Recb_I(O2p_) = ReactionRate_I(O2p_em__O_O_)*&
               exp(log(kTn/kTe)*0.56)

          ReactionRate_I(CO2p_em__CO_O_) = Rate_I(CO2p_em__CO_O_)
          Recb_I(CO2p_) = ReactionRate_I(CO2p_em__CO_O_)*&
               sqrt(kTn/kTe)

          do iSpecies=1,nSpecies
             LossSpecies_I = LossSpecies_I + CoeffSpecies_II(iSpecies, :)
             !  dStndRho_I=dStndRho_I  &
             !       +CoeffSpecies_II(iSpecies, :)/MassSpecies_I(:)
             dSdRho_II(1:nSpecies, iSpecies) = &
                  CoeffSpecies_II(1:nSpecies, iSpecies)&
                  *MassSpecies_I(1:nSpecies)/MassSpecies_I(iSpecies)
          enddo

!!!              do iSpecies=1, nSpecies
!!!                 dLdRho_II(1:nSpecies, iSpecies)=Recb_I(1:nSpecies)&
!!!                      *rhoSpecies_GBI(i,j,k,iBlock,1:nSpecies) &
!!!                      /MassSpecies_I(iSpecies)
!!!                 dLdRho_II(iSpecies, iSpecies)=  &
!!!                      dLdRho_II(iSpecies, iSpecies) &
!!!                      +LossSpecies_I(iSpecies)  &
!!!                      +Recb_I(iSpecies)*totalNumRho
!!!              enddo
!!!              !              dSLdRho_II=dSdRho_II-dLdRho_II
!!!
!!!              do iSpecies=1, nSpecies
!!!                 dLtdRho_I(:)=dLtdRho_I(:) +dLdRho_II(iSpecies,:)
!!!                 dLtndNumRho_I(:)=dLtndNumRho_I(:) &
!!!                      +dLdRho_II(iSpecies,:)*MassSpecies_I(:)/MassSpecies_I(iSpecies)
!!!              enddo

          SiSpecies_I = PhoIon_I*MassSpecies_I

          do iSpecies=1,nSpecies
             SiSpecies_I(1:nSpecies) =&
                  SiSpecies_I(1:nSpecies)  &
                  +dSdRho_II(1:nSpecies,iSpecies) &
                  *State_VGB(rho_+iSpecies,i,j,k,iBlock)
             LiSpecies_I(iSpecies) = &
                  LiSpecies_I(iSpecies)  &
                  +(LossSpecies_I(iSpecies) + Recb_I(iSpecies)*totalNumRho)&
                  *State_VGB(rho_+iSpecies,i,j,k,iBlock)
          enddo

          totalSourceRho = sum(SiSpecies_I(1:nSpecies))
          totalLossRho = sum(LiSpecies_I(1:nSpecies))
          ! sum of the (Loss term) of all ion species
          totalLossNumRho = sum(LiSpecies_I(1:nSpecies)&
               /MassSpecies_I(1:nSpecies))
          ! sum of the (loss term/atom mass) of all ..
          totalSourceNumRho = sum(SiSpecies_I(1:nSpecies)&
               /MassSpecies_I(1:nSpecies))
          ! sum of the (Source term/atom mass) of all..
          totalLossx = totalLossRho*inv_rho
          totalLossNumx = totalLossNumRho/totalNumRho
          totalPSNumRho = sum(PhoIon_I(:))
          ! sum of the photonionziation source/atom mass) of all..
          totalRLNumRhox = sum(Recb_I(:) &
               *State_VGB(rho_+1:rho_+nSpecies,i,j,k,iBlock)/MassSpecies_I(:))
          ! sum of the (loss term/atom mass) due to recombination

          MaxSLSpecies_CB(i,j,k,iBlock) = maxval(abs(SiSpecies_I(1:nSpecies)+&
               LiSpecies_I(1:nSpecies) ) /&
               (State_VGB(rho_+1:rho_+MaxSpecies, i,j,k, iBlock)+1e-20))&
               *CellVolume_GB(i,j,k,iBlock)

          ! ! Limit timestep for explicit scheme
          if(.not.UsePointImplicit)then
             ! sum of the (loss term/atom mass) due to recombination
             SourceLossMax = 10.0*maxval(abs(SiSpecies_I(1:nSpecies)-&
                  LiSpecies_I(1:nSpecies) ) /&
                  (State_VGB(rho_+1:rho_+nSpecies, i,j,k, iBlock)+1e-20))&
                  *CellVolume_GB(i,j,k,iBlock)
             vdtmin = min(Flux_VXI(Vdt_,i,j,k,1),Flux_VYI(Vdt_,i,j,k,1),Flux_VZI(Vdt_,i,j,k,1))
             if(SourceLossMax > Vdtmin) then
                ! UsePointImplicit_B(iBlock)=.true.
                ! write(*,*)'should use Point-implicit or increase its region'
                Flux_VXI(Vdt_,i,j,k,1) = max (SourceLossMax, Flux_VXI(Vdt_,i,j,k,1) )
                Flux_VYI(Vdt_,i,j,k,1) = max (SourceLossMax, Flux_VYI(Vdt_,i,j,k,1) )
                Flux_VZI(Vdt_,i,j,k,1) = max (SourceLossMax, Flux_VZI(Vdt_,i,j,k,1) )
             end if
          end if

          S_IC(rho_+1:rho_+MaxSpecies,i,j,k) = &
               S_IC(rho_+1:rho_+MaxSpecies,i,j,k)&
               +SiSpecies_I(1:nSpecies) &
               -LiSpecies_I(1:nSpecies)

          S_IC(rho_,i,j,k) = S_IC(rho_,i,j,k)&
               +sum(SiSpecies_I(1:MaxSpecies))&
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

          kTi = State_VGB(p_,i,j,k,iBlock)/totalNumRho/2.0
          kTe = kTi

          !----- pressure and energy source terms, 7 terms each
          temps = totalSourceNumRho*kTn &
               +  totalPSNumRho*kTe0    &
               -  totalLossNumRho*kTi   &
               -  totalRLNumRhox*totalNumRho*KTe

          S_IC(rho_+MaxSpecies+8,i,j,k) = S_IC(rho_+MaxSpecies+8,i,j,k) + &
               (InvGammaMinus1*temps - 0.50*uu2*(totalLossRho) )
          S_IC(rho_+MaxSpecies+7,i,j,k) = S_IC(rho_+MaxSpecies+7,i,j,k) + &
               (temps + 0.50*uu2*(totalSourceRho)*GammaMinus1)

          ! energy or pressure change due to velocity differences
          ! between plasma and neutrals, different sign and coef.
          S_IC(rho_+MaxSpecies+8,i,j,k) = S_IC(rho_+MaxSpecies+8,i,j,k)  &
               -0.5*State_VGB(rho_,i,j,k,iBlock)*uu2*&
               nu_BLK(i,j,k,iBlock)
          S_IC(rho_+MaxSpecies+7,i,j,k) = S_IC(rho_+MaxSpecies+7,i,j,k)  &
               +GammaMinus1*0.5*State_VGB(rho_,i,j,k,iBlock)*uu2*&
               nu_BLK(i,j,k,iBlock)
          ! energy or pressure change due to temperature differences
          ! between plasma and neutrals, same sign but different coef.
          S_IC(rho_+MaxSpecies+8,i,j,k) = S_IC(rho_+MaxSpecies+8,i,j,k)  &
               +nu_BLK(i,j,k,iBlock)*totalNumRho*InvGammaMinus1*(kTn-KTi)
          S_IC(rho_+MaxSpecies+7,i,j,k) = S_IC(rho_+MaxSpecies+7,i,j,k)  &
               +nu_BLK(i,j,k,iBlock)*totalNumRho*(kTn-KTi)

       else

       endif ! r_GB(i,j,k,iBlock) >= Rbody?

    end do; end do; end do ! end of the i,j,k loop

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
    use ModSize, ONLY: nDim
    use ModMain
    use ModPhysics
    use ModVarIndexes

    integer :: iBoundary

    ! For Outer Boundaries
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_init_session'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    do iBoundary=xMinBc_, zMaxBc_
       FaceState_VI(rhoHp_,iBoundary)    = SolarWindRho
       FaceState_VI(rhoO2p_,iBoundary)   = 1e-10
       FaceState_VI(rhoOp_,iBoundary)    = 1e-10
       FaceState_VI(rhoCO2p_,iBoundary)  = 1e-10
    end do
    call set_multiSp_ICs
    !    Rbody = 1.0 + 140.0e3/Rvenus
    BodyRho_I(1) = sum(BodyRhoSpecies_I)
    BodyP_I(1)   = sum(BodyRhoSpecies_I/MassSpecies_I)*kTp0

    FaceState_VI(rho_,body1_)=BodyRho_I(1)
    FaceState_VI(rhoHp_:rhoCO2p_,body1_) = BodyRhoSpecies_I
    FaceState_VI(P_,body1_)=BodyP_I(1)

    CellState_VI = FaceState_VI(:,xMinBc_:zMaxBc_)
    ! Convert velocity to momentum
    do iBoundary=1,2*nDim
       CellState_VI(rhoUx_:rhoUz_,iBoundary) = &
            FaceState_VI(Ux_:Uz_,iBoundary)*FaceState_VI(rho_,iBoundary)
    end do

    UnitUser_V(rhoHp_:rhoCO2p_) = No2Io_V(UnitRho_)/MassSpecies_V

    call test_stop(NameSub, DoTest)
  end subroutine user_init_session
  !============================================================================

  subroutine user_set_ICs(iBlock)

    use ModMain
    use ModAdvance
    use ModGeometry, ONLY: r_GB
    use BATL_lib, ONLY: Xyz_DGB, Used_GB
    use ModIO, ONLY: IsRestart
    use ModPhysics
    use ModMultiFluid

    integer, intent(in) :: iBlock

    real :: CosSZA, OptDep
    integer :: i, j, k, iFluid

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

    ! calculate neutral density
    do k=1,nK; do j=1,nJ; do i=1,nI
       if(r_GB(i,j,k,iBlock)<= Rbody)then
          nDenNuSpecies_CBI(i,j,k,iBlock,:) = &
               BodynDenNuSpecies_I(:)
       else if(r_GB(i,j,k,iBlock)< 2.0) then
          nDenNuSpecies_CBI(i,j,k,iBlock,:) = &
               BodynDenNuSpecies_I(:)* &
               exp(-(r_GB(i,j,k,iBlock)-Rbody)&
               /HNuSpecies_I(:))
       else
          nDenNuSpecies_CBI(i,j,k,iBlock,:) = 0.0
       end if
    end do; end do; end do

    do k=1,nK; do j=1,nJ; do i=1,nI
       nu_BLK(i,j,k,iBlock) = &
            sum(nDenNuSpecies_CBI(i,j,k,iBlock,1:nNuSPecies))*nu0
    end do; end do; end do

    ! calculate optical depth and producation rate
    do k=1,nK; do j=1,nJ; do i=1,nI
       cosSZA = (0.5+sign(0.5,Xyz_DGB(x_,i,j,k,iBlock)))*&
            Xyz_DGB(x_,i,j,k,iBlock)/max(r_GB(i,j,k,iBlock),1.0e-3)&
            +5.0e-4
       OptDep = max( sum(nDenNuSpecies_CBI(i,j,k,iBlock,1:MaxNuSpecies)*&
            CrossSection_I(1:MaxNuSpecies)*HNuSpecies_I(1:MaxNuSpecies)),&
            6.0e-3)/cosSZA
       if(OptDep<11.5 .and. Xyz_DGB(x_,i,j,k,iBlock) > 0.0) then
          Productrate_CB(i,j,k,iBlock) = max(exp(-OptDep), 1.0e-5)
       else
          Productrate_CB(i,j,k,iBlock) = 1.0e-5
       end if

    end do; end do; end do

    if(.not.IsRestart)then

       do k=MinK,MaxK;do j=MinJ,MaxJ; do i=MinI,MaxI
          if (r_GB(i,j,k,iBlock)< 1.0) then
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
                  FaceState_VI(rhoOp_,body1_)*sqrt(cosSZA)
             State_VGB(rhoCO2p_,i,j,k,iBlock)= &
                  FaceState_VI(rhoOp_,body1_)*cosSZA
             State_VGB(rho_,i,j,k,iBlock)  = &
                  sum( State_VGB(rho_+1:rho_+MaxSpecies,i,j,k,iBlock))
             State_VGB(P_,i,j,k,iBlock) = max(SolarWindP, &
                  sum(State_VGB(rho_+1:rho_+MaxSpecies,i,j,k,iBlock) &
                  /MassSpecies_I(1:MaxSpecies))*kTp0 )

          else
             State_VGB(:,i,j,k,iBlock) = CellState_VI(:,Coord1MinBc_)
             State_VGB(Bx_:Bz_,i,j,k,iBlock) = 0.0
             State_VGB(Ux_:Uz_,i,j,k,iBlock) = 0.0

          end if
       end do; end do; end do;

       do k=1,nK; do j=1,nJ; do i=1,nI

          if (Used_GB(i,j,k,iBlock).and. &
               r_GB(i,j,k,iBlock)<1.2*Rbody) then

             cosSZA=(0.5+sign(0.5,Xyz_DGB(x_,i,j,k,iBlock)))*&
                  Xyz_DGB(x_,i,j,k,iBlock)/max(r_GB(i,j,k,iBlock),1.0e-3)+&
                  1.0e-3

             State_VGB(rhoCO2p_,i,j,k,iBlock)= Rate_I(CO2_hv__CO2p_em_)*&
                  cosSZA &
                  *nDenNuSpecies_CBI(i,j,k,iBlock, CO2_)/&
                  nDenNuSpecies_CBI(i,j,k,iBlock,O_)/&
                  (Rate_I(CO2p_O__O2p_CO_)+Rate_I(CO2p_O__Op_CO2_))
             State_VGB(rhoOp_,i,j,k,iBlock)= (Rate_I(O_hv__Op_em_)*&
                  cosSZA+&
                  Rate_I(CO2p_O__Op_CO2_)*&
                  State_VGB(rhoCO2p_,i,j,k,iBlock))&
                  *nDenNuSpecies_CBI(i,j,k,iBlock,O_)&
                  /(nDenNuSpecies_CBI(i,j,k,iBlock, CO2_)+3.0e5)&
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

          end if ! (Used_GB?)

       end do; end do; end do

       do k=1,nK; do j=1,nJ; do i=1,nI

          if(.not.Used_GB(i,j,k,iBlock))CYCLE
          State_VGB(rho_,i,j,k,iBlock)   =&
               sum(State_VGB(rho_+1:rho_+MaxSpecies,i,j,k,iBlock))
          State_VGB(P_,i,j,k,iBlock)= &
               max(SolarWindP, sum(State_VGB(rho_+1:rho_+MaxSpecies,i,j,k,iBlock)&
               /MassSpecies_I(1:MaxSpecies))*kTp0)
       end do; end do; end do

    end if
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

    ! Calculate the scale height of ion and neutal species and
    ! intial boundary value of ion species

    use ModMain
    use ModConst
    use ModIO
    use ModPhysics

    real :: OptDep, Productrate0, Productrate, alt0

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_multiSp_ICs'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)then
       write(*,*)'in set_multisp_ICs, No2Io_V(UnitN_),t=',&
            No2Io_V(UnitN_),No2Io_V(UnitT_)
       write(*,*)'No2Si_V(UnitX_), temperature=',&
            No2Si_V(UnitX_), No2Si_V(UnitTemperature_)
       write(*,*)'BodynDenNuSpecies_dim_I(:)',&
            BodynDenNuSpdim_I(:)
    end if
    select case(SolarCond)

    case('solarmax')

       RateDim_I(CO2_hv__CO2p_em_)=1.695e-6/0.723/0.723/2.5 ! scale to Venus
       RateDim_I(O_hv__Op_em_) = 6.346e-7/0.723/0.723
       BodynDenNuSpDim_I(CO2_)= 1.0e15
       BodynDenNuSpDim_I(O_  )= 2.0e11
       HNuSpeciesDim_I(CO2_)=5.5e3
       HNuSpeciesDim_I(O_  )=17.0e3

    case('solarmin')

       RateDim_I(CO2_hv__CO2p_em_)=6.696e-7/0.723/0.723/3.0 ! scale to Venus
       RateDim_I(O_hv__Op_em_) = 2.44e-7/0.723/0.723
       BodynDenNuSpDim_I(CO2_)=1.0e15 ! neutral density in cm^-3
       BodynDenNuSpDim_I(O_  )=1.3e11
       HNuSpeciesDim_I(CO2_)=5.07e3  ! scale height in meter
       HNuSpeciesDim_I(O_  )=15.5e3

    case default
       call stop_mpi('unknow solar condition='//SolarCond)
    end select

    ! normalized body neutral temperature
    KTn = bodyTDim_I(1)*Si2No_V(UnitTemperature_)
    ! normalized body ion temperature
    kTi0 = kTn
    ! normalized body plasma temperature
    kTp0 = 2.0*kTn
    ! normalized newly created electron temperature
    kTe0 = Te_new_dim*Si2No_V(UnitTemperature_)

    T300 = T300_dim*Si2No_V(UnitTemperature_)

    nu0 = nu0_dim*No2Io_V(UnitN_)*No2Io_V(UnitT_)

    BodynDenNuSpecies_I(1:nNuSpecies) = &
         BodynDenNuSpDim_I(1:nNuSpecies)/No2Io_V(UnitN_)
    HNuSpecies_I(1:nNuSpecies) = &
         HNuSpeciesDim_I(1:nNuSpecies)*Si2No_V(UnitX_)

    alt0 = Rbody-Altitude0*Si2No_V(UnitX_)-1.0

    BodynDenNuSpecies_I(1:nNuSpecies)=&
         BodynDenNuSpecies_I(1:nNuSpecies)* &
         exp(-alt0/HNuSpecies_I(1:nNuSpecies))

    ! normlize the reaction rate
    Rate_I(CO2_hv__CO2p_em_) = &
         Ratedim_I(CO2_hv__CO2p_em_)*No2Io_V(UnitT_)
    Rate_I(O_hv__Op_em_) = &
         Ratedim_I(O_hv__Op_em_)*No2Io_V(UnitT_)
    Rate_I(CO2p_O__O2p_CO_) = &
         Ratedim_I(CO2p_O__O2p_CO_)  &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)
    Rate_I(Op_CO2__O2p_CO_) = &
         Ratedim_I(Op_CO2__O2p_CO_)*exp(log(8.0/3.0*T300/kTn)*0.39) &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)
    Rate_I(CO2p_O__Op_CO2_) = &
         Ratedim_I(CO2p_O__Op_CO2_) &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)

    Rate_I(O2p_em__O_O_) = &
         Ratedim_I(O2p_em__O_O_)*exp(log(4.0*T300/kTn)*0.56)&
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)
    Rate_I(CO2p_em__CO_O_) = &
         Ratedim_I(CO2p_em__CO_O_)*sqrt(T300/kTn)&
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)

    Rate_I(H_hv__Hp_em_) = &
         Ratedim_I(H_hv__Hp_em_)*No2Io_V(UnitT_)
    Rate_I(Hp_O__Op_H_) = &
         Ratedim_I(Hp_O__Op_H_) &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)
    Rate_I(Op_H__Hp_O_) = &
         Ratedim_I(Op_H__Hp_O_) &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)

    ReactionRate_I(CO2_hv__CO2p_em_) = &
         Rate_I(CO2_hv__CO2p_em_)*BodynDenNuSpecies_I(CO2_)
    PhoIon_I(CO2p_)=ReactionRate_I(CO2_hv__CO2p_em_)

    ReactionRate_I(O_hv__Op_em_) = &
         Rate_I(O_hv__Op_em_)*BodynDenNuSpecies_I(O_)
    PhoIon_I(Op_)=ReactionRate_I(O_hv__Op_em_)

    ! charge exchange
    ReactionRate_I(CO2p_O__O2p_CO_) = &
         Rate_I(CO2p_O__O2p_CO_)* BodynDenNuSpecies_I(O_)
    CoeffSpecies_II(O2p_,CO2p_)=ReactionRate_I(CO2p_O__O2p_CO_)

    ReactionRate_I(Op_CO2__O2p_CO_) = &
         Rate_I(Op_CO2__O2p_CO_)* BodynDenNuSpecies_I(CO2_)
    CoeffSpecies_II(O2p_, Op_)=ReactionRate_I(Op_CO2__O2p_CO_)

    ReactionRate_I(CO2p_O__Op_CO2_) = &
         Rate_I(CO2p_O__Op_CO2_)* BodynDenNuSpecies_I(O_)
    CoeffSpecies_II(Op_,CO2p_)=ReactionRate_I(CO2p_O__Op_CO2_)

    ! ion density at the body
    CrossSection_I=CrossSectiondim_I*No2Io_V(unitN_)*No2Si_V(unitX_)*1.0e2

    OptDep = sum(BodynDenNuSpecies_I*CrossSection_I*HNuSpecies_I)
    Productrate0 = max(exp(-OptDep), 1.0e-5)
    Productrate = Productrate0

    BodyRhoSpecies_I(Hp_) = SolarWindRho*0.1

    BodyRhoSpecies_I(CO2p_) = Rate_I(CO2_hv__CO2p_em_)*Productrate*&
         BodynDenNuSpecies_I(CO2_)/BodynDenNuSpecies_I(O_)/&
         (Rate_I(CO2p_O__O2p_CO_)+Rate_I(CO2p_O__Op_CO2_))

    BodyRhoSpecies_I(Op_) = (Rate_I(O_hv__Op_em_)*Productrate+&
         Rate_I(CO2p_O__Op_CO2_)*BodyRhoSpecies_I(CO2p_))&
         *BodynDenNuSpecies_I(O_)/(BodynDenNuSpecies_I(CO2_)+3.0e5)/&
         Rate_I(Op_CO2__O2p_CO_)

    BodyRhoSpecies_I(O2p_) = sqrt((BodynDenNuSpecies_I(O_)*&
         BodyRhoSpecies_I(CO2p_)*Rate_I(CO2p_O__O2p_CO_)+ &
         BodynDenNuSpecies_I(CO2_)*BodyRhoSpecies_I(Op_)*&
         Rate_I(Op_CO2__O2p_CO_))/Rate_I(O2p_em__O_O_))

    BodyRhoSpecies_I = BodyRhoSpecies_I*MassSpecies_I

    if(DoTest.and.iProc==1)then
       write(*,*)'crosssection=	',CrossSection_I, 'optdep=', Optdep
       write(*,*)'producationrate0=', Productrate0
       write(*,*)'hnuspecies_I=',HNuSpecies_I(1:nNuSpecies)
       write(*,*)' set parameters of Mars: BodyRhoSpecies_I(i)=',&
            BodyRhoSpecies_I
       write(*,*)'neutral density=', BodynDenNuSpecies_I
       write(*,*)'nu0=',nu0
       write(*,*)'Rate_I=', Rate_I
       write(*,*)'Rate_dim_I=', Ratedim_I
!       call stop_mpi('end')
    end if

    call test_stop(NameSub, DoTest)
  end subroutine set_multiSp_ICs
  !============================================================================

  subroutine user_set_face_boundary(FBC)

    use ModMain, ONLY: FaceBCType
    use ModVarIndexes, ONLY: nVar, RhoOp_, RhoO2p_, RhoCO2p_, RhoHp_
    use ModPhysics,    ONLY: SolarWindRho

    type(FaceBCType), intent(inout):: FBC

    real:: xFace, yFace, zFace, rFace, rFace2
    real:: CosSZA
    real:: uDotR, bDotR

    character(len=*), parameter:: NameSub = 'user_set_face_boundary'
    !--------------------------------------------------------------------------
    associate( VarsGhostFace_V => FBC%VarsGhostFace_V, &
               VarsTrueFace_V => FBC%VarsTrueFace_V, &
               FaceCoords_D => FBC%FaceCoords_D )

    XFace = FaceCoords_D(1)
    YFace = FaceCoords_D(2)
    ZFace = FaceCoords_D(3)

    rFace2 = XFace**2 + YFace**2 + ZFace**2
    rFace  = sqrt(rFace2)

    ! Apply boundary conditions
    cosSZA = (0.5+sign(0.5,XFace)) * XFace/max(rFace,1.0e-3) + 1.0e-3

    VarsGhostFace_V(rhoOp_) =  BodyRhoSpecies_I(Op_) *cosSZA

    VarsGhostFace_V(rhoO2p_) = BodyRhoSpecies_I(O2p_)*sqrt(cosSZA)

    VarsGhostFace_V(rhoCO2p_)= BodyRhoSpecies_I(CO2p_)*cosSZA

    VarsGhostFace_V(rhoHp_)  = SolarWindRho*0.3

    VarsGhostFace_V(rho_) = sum(VarsGhostFace_V(rho_+1:rho_+MaxSpecies))
    VarsGhostFace_V(P_)=sum(VarsGhostFace_V(rho_+1:rho_+MaxSpecies)&
         /MassSpecies_I)*kTp0

    ! Reflective in radial direction
    uDotR = sum(VarsTrueFace_V(Ux_:Uz_)*FaceCoords_D)/rFace2
    bDotR = sum(VarsTrueFace_V(Bx_:Bz_)*FaceCoords_D)/rFace2

    select case (type_innerbcs)
    case('float')
       VarsGhostFace_V(Ux_:Uz_) = &
            VarsTrueFace_V(Ux_:Uz_)
       VarsGhostFace_V(Bx_:Bz_) = &
            VarsTrueFace_V(Bx_:Bz_)
    case('reflect')
       VarsGhostFace_V(Ux_:Uz_) = &
            VarsTrueFace_V(Ux_:Uz_) - 2*uDotR*FaceCoords_D
       VarsGhostFace_V(Bx_:Bz_) = &
            VarsTrueFace_V(Bx_:Bz_) - 2*bDotR*FaceCoords_D
    case('zeroB')
       VarsGhostFace_V(Ux_:Uz_) = &
            VarsTrueFace_V(Ux_:Uz_) - 2*uDotR*FaceCoords_D
       VarsGhostFace_V(Bx_:Bz_) = 0.0
    case('default')
       write(*,*)'unknown type of user inner bcs'
    end select

    end associate
  end subroutine user_set_face_boundary
  !============================================================================

  subroutine user_set_plot_var1(iBlock, NameVar, IsDimensional, &
       PlotVar_G, PlotVarBody, UsePlotVarBody, &
       NameTecVar, NameTecUnit, NameIdlUnit, IsFound)

    use ModPhysics,    ONLY: No2Io_V, UnitN_, NameTecUnit_V, NameIdlUnit_V
    use ModAdvance,    ONLY: State_VGB, Rho_

    ! Returns dimensional number density (/cc)

    integer,          intent(in) :: iBlock
    character(len=*), intent(in) :: NameVar
    logical,          intent(in) :: IsDimensional
    real,             intent(inout):: PlotVar_G(MinI:MaxI, MinJ:MaxJ, MinK:MaxK)
    real,             intent(out):: PlotVarBody
    logical,          intent(out):: UsePlotVarBody
    character(len=*), intent(out):: NameTecVar
    character(len=*), intent(out):: NameTecUnit
    character(len=*), intent(out):: NameIdlUnit
    logical,          intent(out):: IsFound

    integer :: iVar
    real :: Coeff

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_plot_var1'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    IsFound = .true.
    select case(NameVar)
    case('nhp')
       iVar = Hp_
       NameTecVar = 'H^+'
    case('nco2p')
       iVar = CO2p_
       NameTecVar = 'CO_2^+'
    case('no2p')
       iVar = O2p_
       NameTecVar = 'O_2^+'
    case('nop')
       iVar = Op_
       NameTecVar = 'O^+'
    case default
       IsFound = .false.
    end select
    NameTecUnit = NameTecUnit_V(UnitN_)
    NameIdlUnit = NameIdlUnit_V(UnitN_)

    Coeff          = No2Io_V(UnitN_)/MassSpecies_I(iVar)
    PlotVar_G      = Coeff*State_VGB(Rho_+iVar,:,:,:,iBlock)
    PlotVarBody    = BodyRhoSpecies_I(iVar)
    UsePlotVarBody = .True.

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_plot_var1
  !============================================================================

  real function neutral_density(R0,iNu)
    !  use ModUser, ONLY : BodynDenNuSpecies_I, HNuSpecies_I
    use ModPhysics, ONLY : Rbody

    real, intent(in) :: R0
    integer, intent(in) :: iNu

    !--------------------------------------------------------------------------
    neutral_density = 0.0
    if( R0 >= 0.9*Rbody .and. R0< 3.0*Rbody ) &
         neutral_density= exp(-(R0-Rbody)/HNuSpecies_I(iNu))

  end function neutral_density
  !============================================================================

  subroutine user_get_log_var(VarValue, NameVar, Radius)

    use ModGeometry,   ONLY: Xyz_DGB, r_GB
    use ModMain,       ONLY: Unused_B
    use ModVarIndexes, ONLY: &
         Rho_, rhoHp_, rhoO2p_, RhoOp_, RhoCO2p_, rhoUx_, rhoUz_
    use ModAdvance,    ONLY: State_VGB,Tmp1_GB
    use ModPhysics,    ONLY: No2Si_V, UnitN_, UnitX_, UnitU_
    use ModWriteLogSatFile, ONLY: calc_sphere

    real, intent(out)            :: VarValue
    character (len=*), intent(in):: NameVar
    real, intent(in), optional :: Radius

    integer:: i, j, k, iBlock, iVar

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_get_log_var'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(DoTest)write(*,*) NameSub, ': NameVar=', NameVar
    select case(NameVar)
    case('hpflx')
       iVar = RhoHp_
    case('opflx')
       iVar = RhoOp_
    case('o2pflx')
       iVar = RhoO2p_
    case('co2pflx')
       iVar = RhoCO2p_
    case default
       call stop_mpi(NameSub//': wrong NameVar='//NameVar)
    end select

    do iBlock = 1, nBlock
       if (Unused_B(iBlock)) CYCLE
       do k=0,nK+1; do j=0,nJ+1; do i=0,nI+1
          Tmp1_GB(i,j,k,iBlock) = State_VGB(iVar,i,j,k,iBlock)* &
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

  subroutine user_set_resistivity(iBlock, Eta_G)

    use ModPhysics,  ONLY: No2Si_V, Si2No_V, &
         UnitTemperature_, UnitX_,UnitT_, Rbody
    use ModAdvance,  ONLY: State_VGB
    use ModGeometry, ONLY: rMin_B
    use ModResistivity, ONLY: Eta0Si

    integer, intent(in) :: iBlock
    real,intent(out) :: Eta_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)

    real   :: Te_dim
    real   :: loc_c(3), NumDenNeutral_V(3), Eta0
    integer:: i, j, k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_resistivity'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    DoTest=.false.

    ! Dimensionalize:: Eta* is in units of [m^2/s]
    Eta_G = 0.0

    if (rMin_B(iBlock) > 2.0*Rbody) RETURN ! in Rbody unit

    Eta0 = Eta0Si * Si2No_V(UnitX_)**2/Si2No_V(UnitT_)

    do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI;
       totalNumRho=sum(State_VGB(rho_+1:rho_+MaxSpecies,i,j,k,iBlock) &
            /MassSpecies_V(rho_+1:rho_+MaxSpecies))

       Te_dim= State_VGB(p_,i,j,k,iBlock)/(totalNumRho+1.0e-8)&
            *No2Si_V(UnitTemperature_)/2

       loc_c(:)=4.243e-4* sqrt(Te_dim)
       ! mu_en=1.5e-17*N(cm^-3)*Te^(1/2)
       ! eta = me*mu_en/mu0/e^2/n
       ! 1.5e-17*me/mu0/e^2 = 4.243e-4
       ! eta = loc *N(cm^3)/n(cm^3)

       NumDenNeutral_V= nDenNuSpecies_CBI(i,j,k,iBlock,1:3)

       NumDenNeutral_V = max(0.0, NumDenNeutral_V)

       Eta_G(i,j,k) = Eta0* &
            sum(loc_c(:)*NumDenNeutral_V(1:3))/&
            (totalNumRho+1.0e-8)

       if(DoTest.and. &
            iTest==i.and.jTest==j.and.kTest==k)then
          write(*,*) NameSub,': loc_c=', loc_c
          write(*,*) NameSub,': Te_dim=', Te_dim
          write(*,*) NameSub,': TotalNumRho=',TotalNumRho
          write(*,*) NameSub,': NumDenNeutral=', NumDenNeutral_V
          write(*,*) NameSub,': Eta_G=',Eta_G(iTest,jTest,kTest)
          write(*,*) NameSub,': Eta0Si, Eta0=',Eta0Si, Eta0
       end if
    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_resistivity
  !============================================================================

  subroutine user_update_states(iBlock)

    use ModUpdateState, ONLY: update_state_normal
    use ModVarIndexes
    use ModSize
    use ModAdvance, ONLY: State_VGB
    use ModPhysics

    integer,intent(in):: iBlock
    integer:: i,j,k
    real :: kTe

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_update_states'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    call update_state_normal(iBlock)

    ! Begin update check of temperature::
    ! now check to see if the temperature is less than some
    ! prescribed minimum. If it is set it to the minimum value

    do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI;
       totalNumRho = sum(State_VGB(rho_+1:rho_+MaxSpecies,i,j,k,iBlock) &
            /MassSpecies_V(rho_+1:rho_+MaxSpecies))
       kTe = State_VGB(p_,i,j,k,iBlock)/(totalNumRho+1.0e-8)/2.0

       if(kTe < kTn)then
          State_VGB(p_,i,j,k,iBlock)= totalNumRho*kTn*2.0
       end if
    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_update_states
  !============================================================================

end module ModUser
!==============================================================================
