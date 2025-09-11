!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

! this file contains the ModUser for general Cometary MHD

module ModUser

  ! MassSpecies is defined at [SpeciesFirst:SpeciesLast]
  ! Last 01.15 committed in
  ! This 01.16 change Te calculation, add ini rates for solar max.
  ! This ModUser.f90 requires ModEquationMHDComet.f90
  ! jpattern = 0, 10, 11, 12, sets the electron impact ionization term, and
  ! the way electron temperature is treated for dissociative recombination
  ! processes[Gombosi96].

  ! This is the user module for Comets
  use ModUserEmpty,               &
       IMPLEMENTED1 => user_read_inputs,                &
       IMPLEMENTED2 => user_init_session,               &
       IMPLEMENTED3 => user_set_ics,                    &
       IMPLEMENTED4 => user_init_point_implicit,        &
       IMPLEMENTED5 => user_calc_sources_impl,          &
       IMPLEMENTED6 => user_update_states,              &
       IMPLEMENTED7 => user_action

  use BATL_lib, ONLY: test_start, test_stop, iProc
  use ModSize

  include 'user_module.h' ! list of public methods

  ! Here you must define a user routine Version number and a
  ! descriptive string.
  character (len=*), parameter :: NameUserFile = "ModUserComet6Sp.f90"
  character (len=*), parameter :: NameUserModule = &
       '6 Species Cometary MHD Module, Yingdong Jia 2008'

  real ::  Qprod=7.e-9, ionization_rate=1.e-6, Unr, Unr_km=1.0, kin, kin_cc=1.7e-9
  real ::  Tion, mbar, lambda
  integer :: jpattern

  integer ::  nSpecies=1
  integer, parameter :: MaxSpecies=6, MaxNuSpecies=6,  &
       MaxIni=11, MaxCx=13
  integer ::  nNuSpecies=6	! nNuSpecies<6 doesn't work for this version yet

  real, allocatable, public:: NNeu_BLK(:,:,:,:,:), UNeu_BLK(:,:,:,:,:)

  integer, parameter :: H2Op_ =1, Hp_ =2, H3Op_ =3, OHp_ =4, Op_ =5, COp_ =6

  integer, parameter :: H2O_=1, H_=2, OH_=3, O_=4, CO_=5, H2_=6

  integer, parameter :: &	! ionization reaction number
       H2O_H2Op_ = 1 ,&
       H2O_Hp_   = 2 ,&
       H_Hp_     = 3 ,&
       OH_Hp_    = 4 ,&
       H2O_OHp_  = 5 ,&
       OH_OHp_   = 6 ,&
       H2O_Op_   = 7 ,&
       OH_Op_    = 8 ,&
       O_Op_     = 9 ,&
       CO_Op_    = 10,&
       CO_COp_   = 11

  ! The ionization rates that put down here are midified in user_init by
  ! a factor (ionization_rate) that reflecting the solar distance.
  real, dimension(MaxIni) :: IoniRate_I=[5.4e-7, .2e-7, &
       1.2e-7, .6e-7, .9e-7, 3.94e-7, 9.5e-9, 5.3e-8, 3.45e-7, &
       .4e-7, 6.25e-7]  ! s^(-1), solar minimum conditions at 1AU

  !  real, dimension(MaxIni) :: IoniRate_I=(/5.4e-7, .2e-7, &
  !	1.2e-7, .6e-7, .9e-7, 3.94e-7, 9.5e-9, 5.3e-8, 3.45e-7, &
  !	.4e-7, 6.25e-7/)  ! s^(-1), solar maximum conditions at 1AU

  integer, parameter :: &	! CX reaction number
       H2Op_H2O__H3Op_OH_ = 1 ,&
       H2Op_CO__COp_H2O_  = 2 ,&
       H2Op_H2__H3Op_H_   = 3 ,&
       Hp_H2O__H2Op_H_    = 4 ,&
       Hp_OH__OHp_H_      = 5 ,&
       Hp_O__Op_H_        = 6 ,&
       OHp_H2O__H3Op_O_   = 7 ,&
       OHp_H2O__H2Op_OH_  = 8 ,&
       OHp_CO__COp_OH_    = 9 ,&
       Op_H2O__H2Op_O_    = 10,&
       Op_OH__OHp_O_      = 11,&
       COp_H2O__H2Op_CO_  = 12,&
       COp_OH__OHp_CO_    = 13

  real, dimension(MaxCx) :: CxRate_I = [2.e-15, 4.3e-16, 0.8e-15, 8.2e-15, &
       4.4e-15, 0.38e-15, 1.3e-15, 1.6e-15, 0.8e-15, 3.2e-15, 3.2e-15, &
       1.6e-15, 0.3e-15]  ! m^3 s^(-1), already in SI unit.

contains
  !============================================================================
  subroutine user_action(NameAction)

    character(len=*), intent(in):: NameAction

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_action'
    !--------------------------------------------------------------------------
    if(iProc==0)write(*,*) NameSub,' called with action ',NameAction

    select case(NameAction)
    case('initialize module')
       if(.not.allocated(NNeu_BLK))then
          allocate(NNeu_BLK(0:nI+1,0:nJ+1,0:nK+1,MaxBlock,MaxNuSpecies))
          NNeu_BLK = 1.
          allocate(UNeu_BLK(0:nI+1,0:nJ+1,0:nK+1,MaxBlock,3))
          UNeu_BLK = 0.
       end if
    case('clean module')
       if(allocated(NNeu_BLK)) deallocate(NNeu_BLK)
       if(allocated(UNeu_BLK)) deallocate(UNeu_BLK)
    end select

  end subroutine user_action
  !============================================================================
  subroutine user_read_inputs

    use ModMain
    use ModReadParam
    use ModIO, ONLY: write_prefix, write_myname, iUnitOut

    character (len=100) :: NameCommand
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_read_inputs'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(iProc==0.and.lVerbose > 0)then
       call write_prefix; write(iUnitOut,*)'User read_input COMET starts'
    endif
    do
       if(.not.read_line() ) EXIT
       if(.not.read_command(NameCommand)) CYCLE
       select case(NameCommand)
       case("#COMETPARAM")
          call read_var('Qprod' ,Qprod)
          call read_var('Unr_km' ,Unr_km)
          call read_var('mbar',mbar)
          call read_var('ionization_rate',ionization_rate)
          call read_var('kin_cc',kin_cc)
          call read_var('Tion', Tion)
          call read_var('jpattern' ,jpattern)

       case("#MultiSP")		! ini rates hardwired in parameters.
          call read_var('nSpecies'  , nSpecies  )
          call read_var('nNuSpecies', nNuSpecies)

       case('#USERINPUTEND')
          if(iProc==0.and.lVerbose > 0)then
             call write_prefix;
             write(iUnitOut,*)'User read_input COMET ends'
          endif
          EXIT
       case default
          if(iProc==0) then
             call write_myname; write(*,*) &
                  'ERROR: Invalid user defined #COMMAND in user_read_inputs. '
             write(*,*) '--Check user_read_inputs for errors'
             write(*,*) '--Check to make sure a #USERINPUTEND command was used'
             write(*,*) '  *Unrecognized command was: '//NameCommand
             call stop_mpi('ERROR: Correct PARAM.in or user_read_inputs!')
          end if
       end select
    end do

    !    call stop_user(Name)
    call test_stop(NameSub, DoTest)
  end subroutine user_read_inputs
  !============================================================================

  subroutine user_init_session		! called before reading the Param.in
    use ModMain
    use ModPhysics
    use ModVarIndexes

    integer::iBoundary
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_init_session'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    ! For Outer Boundaries
    do iBoundary=xMinBc_,zMaxBc_
       FaceState_VI(SpeciesFirst_:SpeciesLast_,iBoundary) = 1e-10
       FaceState_VI(rhoHp_,iBoundary) = SolarWindRho
    end do
    FaceState_VI(SpeciesFirst_:SpeciesLast_,body1_) = BodyRho_I(1)/100.
    FaceState_VI(rhoH2Op_,body1_) = BodyRho_I(H2Op_)
    FaceState_VI(rhoHp_  ,body1_) = 1e-6

    CellState_VI(SpeciesFirst_:SpeciesLast_,Coord1MinBc_:Coord3MaxBc_) = &
         FaceState_VI(SpeciesFirst_:SpeciesLast_,xMinBc_:zMaxBc_)
    ! Convert velocity to momentum
    do iBoundary=1,Coord3MaxBc_
       CellState_VI(SpeciesFirst_:SpeciesLast_,iBoundary) = &
            FaceState_VI(SpeciesFirst_:SpeciesLast_,iBoundary)&
            *FaceState_VI(SpeciesFirst_:SpeciesLast_,iBoundary)
    end do

    kin=kin_cc*1E-6
    Unr=Unr_km*1E3
    lambda = Unr/ionization_rate	! in meters
    ! should be lambda water(5.4e-7), but this is the same with 1sp model.

    ! convert to real solar distance value
    IoniRate_I = IoniRate_I*ionization_rate*1e6

    call test_stop(NameSub, DoTest)
  end subroutine user_init_session
  !============================================================================

  subroutine user_set_ICs(iBlock)

    use ModPhysics, ONLY: CellState_VI
    use ModAdvance, ONLY: State_VGB
    use ModVarIndexes, ONLY: nVar
    use ModIO, ONLY: IsRestart
    use ModGeometry, ONLY: Xyz_DGB,r_GB
    use ModNumConst
    use ModPhysics, ONLY: No2Si_V, UnitX_, UnitU_
    use ModMain, ONLY: Coord1MinBc_

    integer, intent(in) :: iBlock

    integer :: iVar
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_ICs'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(IsRestart)RETURN	! jet and Bx parameters has to be set in 1st session.

    if(.not.IsRestart)then
       do iVar=1,nVar
          State_VGB(iVar,:,:,:,iBlock)   = CellState_VI(iVar,Coord1MinBc_)
       end do
    end if

    ! define neutral speed array, necessary for special neutral distribution types only
    UNeu_BLK(:,:,:,iBlock,1) = Unr* &
         Xyz_DGB(x_,0:nI+1,0:nJ+1,0:nK+1,iBlock)/ &
         r_GB(0:nI+1,0:nJ+1,0:nK+1,iBlock)
    UNeu_BLK(:,:,:,iBlock,2) = Unr* &
         Xyz_DGB(y_,0:nI+1,0:nJ+1,0:nK+1,iBlock)/ &
         r_GB(0:nI+1,0:nJ+1,0:nK+1,iBlock)
    UNeu_BLK(:,:,:,iBlock,3) = Unr* &
         Xyz_DGB(z_,0:nI+1,0:nJ+1,0:nK+1,iBlock)/ &
         r_GB(0:nI+1,0:nJ+1,0:nK+1,iBlock)
    UNeu_BLK(:,:,:,iBlock,:) = &
         UNeu_BLK(:,:,:,iBlock,:)/No2Si_V(UnitU_)

    ! define neutral density array, necessary for special neutral distribution types only
    NNeu_BLK(:,:,:,iBlock,H2O_) = Qprod * &	!!! Number Density!!!
         exp(-r_GB(0:nI+1,0:nJ+1,0:nK+1,iBlock)*No2Si_V(UnitX_)/lambda)/ &
         (4*cPi*Unr*r_GB(0:nI+1,0:nJ+1,0:nK+1,iBlock)**2*No2Si_V(UnitX_)**2)
    ! minor species using estimation only [combi96,Haberli96], despite of chemistry/scale hight.
    NNeu_BLK(:,:,:,iBlock,OH_ ) = NNeu_BLK(:,:,:,iBlock,H2O_)/30.
    NNeu_BLK(:,:,:,iBlock,O_  ) = NNeu_BLK(:,:,:,iBlock,H2O_)/100.
    NNeu_BLK(:,:,:,iBlock,CO_ ) = NNeu_BLK(:,:,:,iBlock,H2O_)/6.
    NNeu_BLK(:,:,:,iBlock,H2_ ) = NNeu_BLK(:,:,:,iBlock,H2O_)/500.
    if ( jpattern == 22 ) then		! uniform estimated H density
       NNeu_BLK(:,:,:,iBlock,H_) = Qprod * &
            exp(-r_GB(0:nI+1,0:nJ+1,0:nK+1,iBlock)* &
            No2Si_V(UnitX_)/lambda/12.)/ &
            (4*cPi*Unr*r_GB(0:nI+1,0:nJ+1,0:nK+1,iBlock)**2* &
            No2Si_V(UnitX_)**2 * 6)
    else		! Haser model for H density, should be used for other species later, esp. O and OH.
       NNeu_BLK(:,:,:,iBlock,H_) = Qprod * 1.e7/(1.e5-1.e7)* &
            ( exp(-r_GB(0:nI+1,0:nJ+1,0:nK+1,iBlock)* &
            No2Si_V(UnitX_)/lambda*10.) - &
            exp(-r_GB(0:nI+1,0:nJ+1,0:nK+1,iBlock)* &
            No2Si_V(UnitX_)/lambda/12.) )/ &
            (4*cPi*Unr*r_GB(0:nI+1,0:nJ+1,0:nK+1,iBlock)**2* &
            No2Si_V(UnitX_)**2 * 6)
    endif
    if(nNuSpecies < 2) NNeu_BLK(:,:,:,iBlock,2:MaxNuSpecies) = 1e-6

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_ICs
  !============================================================================

  subroutine user_init_point_implicit

    use ModVarIndexes
    use ModPointImplicit, ONLY: iVarPointImpl_I, IsPointImplMatrixSet

    ! Allocate and set iVarPointImpl_I
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_init_point_implicit'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    allocate(iVarPointImpl_I(4+nSpecies))

    iVarPointImpl_I = [RhoUx_, RhoUy_, RhoUz_, p_, &
         rhoH2Op_, rhoHp_, rhoH3Op_, rhoOHp_, rhoOp_, rhoCOp_]

    ! Note that energy is not an independent variable for the
    ! point implicit scheme. The pressure is an independent variable,
    ! and in this example there is no implicit pressure source term.

    ! Tell the point implicit scheme if dS/dU will be set analytically
    ! If this is set to true the DsDu_VVC matrix has to be set below.
    ! Initialization for comet implicit sources: false => numerical ptimplicit.
    IsPointImplMatrixSet = .false.

    call test_stop(NameSub, DoTest)
  end subroutine user_init_point_implicit
  !============================================================================

  subroutine user_calc_sources_impl(iBlock)

    use ModAdvance, ONLY: UseMultiSpecies

    integer, intent(in) :: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_calc_sources_impl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(.not.UseMultiSpecies)then
       call user_impl_source_sgl(iBlock)
    else
       call user_impl_source_multi(iBlock)
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_calc_sources_impl
  !============================================================================
  subroutine user_impl_source_sgl(iBlock)

    integer, intent(in) :: iBlock

    !--------------------------------------------------------------------------
  end subroutine user_impl_source_sgl
  !============================================================================

  subroutine user_impl_source_multi(iBlock)

    ! This is a test and example for using point implicit source terms
    ! Note that the energy is a dependent variable in the
    ! point implicit scheme, so there is no energy source here.
    ! The pressure source is zero.

    use ModPointImplicit, ONLY:

    use ModMain, ONLY: nI, nJ, nK
    use ModAdvance, ONLY: State_VGB, Source_VC
    use ModGeometry, ONLY:r_GB
    use ModVarIndexes
    use ModPhysics

    integer, intent(in) :: iBlock

    integer :: i, j, k, m
    real :: usqr, Unsqr, totalNumRho, Te, alphaTe, fi, &
         totalSourceRho, totalLossRho, totalNumLoss, Rkm, logR
    real :: Source_H3Op, Loss_H3Op, NumLoss_H3Op, collisn	! H3Op special rxn
    real :: Un(3), U(3)
    real, dimension(MaxSpecies) :: Source_I, Loss_I, AlphaN_I, ni_I
    real, dimension(MaxNuSpecies) :: nn_I

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_impl_source_multi'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    do k=1,nK ;   do j=1,nJ ;    do i=1,nI

       State_VGB(rho_,i,j,k,iBlock) = &	! essential for pt-implicit
            sum( State_VGB(SpeciesFirst_:SpeciesLast_,i,j,k,iBlock) )

       nn_I(1:MaxSpecies) = NNeu_BLK(i,j,k,iBlock,1:MaxSpecies)*No2Si_V(UnitT_)
       ni_I(1:MaxSpecies) = State_VGB(SpeciesFirst_:SpeciesLast_,i,j,k,iBlock) &
            /MassSpecies_V(SpeciesFirst_:SpeciesLast_)
       ! use of nSpecies is an example of how to change this into
       ! nSpecies version instead of 6 species version
       totalNumRho = sum(ni_I(1:nSpecies))

       ! The following is for the special impact ionization and recombination Te [gombosi96].
       Rkm  = r_GB(i,j,k,iBlock)*No2Si_V(UnitX_)/1E3	! km unit
       logR = log10(Rkm)
       if (jpattern == 10 .or. jpattern == 11 ) then    ! jpattern= 0, regular comet[Gombosi97]
          if (rkm >= 5000. .and. rkm < 10000.) then     ! jpattern=10, Comet Halley[Gombosi96]
	     fi = 1.0+0.77*log(rkm/5000.)               ! jpattern=11, regular comet with electron impact ionization
          elseif (rkm >= 10000. .and. rkm < 50000.) then
             fi = 1.5-0.31067*log(rkm/10000.)           ! jpattern=12, regluar case with electron temperature
          else                                          !      profile measureed by Giotto
	     fi = 1.0
          endif
       else
	  fi = 1.0
       endif	! jpattern
       if (jpattern == 10 .or. jpattern == 12 ) then
	  if ( Rkm <= 1584.893 ) then
	     Te = 100.
          else if ( Rkm <= 6918.310 ) then
	     Te = 10.0**( 1.143*logR-1.667 )
          else if ( Rkm <= 1.e4 ) then
	     Te = 10.0**( 10.965*logR-39.3725 )
          else if ( Rkm <= 1.e5 ) then
	     Te = 10.0**( .5135*logR+2.4325 )
          else
	     Te = 1.e5
          endif
       else
	  Te = State_VGB(p_,i,j,k,iBlock)*No2Si_V(UnitP_) / &
               ( 2*No2Si_V(UnitN_)*cBoltzmann*totalNumRho )
       endif	! jpattern

       If(Te < 200.) then
	  alphaTe  = 7.E-7*sqrt(300./Te)
       else
	  alphaTe  = 2.342*7.E-7*Te**(0.2553-0.1633*log10(Te))
       endif

       ! define recombination terms ion by ion
       AlphaN_I(H2Op_) = alphaTe
       AlphaN_I(Hp_  ) = 0.0
       AlphaN_I(H3Op_) = alphaTe
       AlphaN_I(Op_  ) = 0.0
       AlphaN_I(OHp_ ) = 3.8E-8*sqrt(300./Te)
       AlphaN_I(COp_ ) = 1.E-7*(300./Te)**0.46
       ! normalize
       AlphaN_I = AlphaN_I*totalNumRho/1E6*No2Si_V(UnitN_)*No2Si_V(UnitT_)

!!!! calculate source terms case by case---!!!
       ! calculate ionization source terms
       Source_I(H2Op_) = IoniRate_I(H2O_H2Op_)*fi*nn_I(H2O_)
       Source_I(Hp_  ) = IoniRate_I(H2O_Hp_  )*nn_I(H2O_) + &
            IoniRate_I(H_Hp_  )*nn_I(H_  ) + &
            IoniRate_I(OH_Hp_ )*nn_I(OH_ )
       Source_I(H3Op_) = 0.0
       Source_I(OHp_ ) = IoniRate_I(H2O_OHp_ )*nn_I(H2O_) + &
            IoniRate_I(OH_OHp_)*nn_I(OH_ )
       Source_I(Op_  ) = IoniRate_I(H2O_Op_  )*nn_I(H2O_) + &
            IoniRate_I(OH_Op_ )*nn_I(OH_ ) + &
            IoniRate_I(O_Op_  )*nn_I(O_  ) + &
            IoniRate_I(CO_Op_ )*nn_I(CO_ )
       Source_I(COp_ ) = IoniRate_I(CO_COp_  )*nn_I(CO_)
       Source_I = Source_I / No2Si_V(UnitN_)	! normalize

       ! add CX source terms
       Source_I(H2Op_) = Source_I(H2Op_) + &
            ( ni_I(Hp_  )*CxRate_I(Hp_H2O__H2Op_H_   ) + &
            ni_I(OHp_ )*CxRate_I(OHp_H2O__H2Op_OH_ ) + &
            ni_I(Op_  )*CxRate_I(Op_H2O__H2Op_O_   ) + &
            ni_I(COp_ )*CxRate_I(COp_H2O__H2Op_CO_ ) )*nn_I(H2O_)
       Source_I(Hp_  ) = Source_I(Hp_  ) + &	! Temperarily using H2O mirror for H mirror
            ( ni_I(Hp_  )*CxRate_I(H2Op_H2O__H3Op_OH_) )*nn_I(H_  )
       Source_I(H3Op_) = Source_I(H3Op_) + &
            ( ni_I(H2Op_)*CxRate_I(H2Op_H2O__H3Op_OH_) + &
            ni_I(OHp_ )*CxRate_I(OHp_H2O__H3Op_O_  ) )*nn_I(H2O_) + &
            ni_I(H2Op_  )*CxRate_I(H2Op_H2__H3Op_H_  )  *nn_I(H2_ )
       Source_I(OHp_ ) = Source_I(OHp_ ) + &
            ( ni_I(Hp_  )*CxRate_I(Hp_OH__OHp_H_     ) + &
            ni_I(Op_  )*CxRate_I(Op_OH__OHp_O_     ) + &
            ni_I(COp_ )*CxRate_I(COp_OH__OHp_CO_   ) )*nn_I(OH_ )
       Source_I(Op_  ) = Source_I(Op_  ) + &
            ( ni_I(Hp_  )*CxRate_I(Hp_O__Op_H_       ) )*nn_I(O_  )
       Source_I(COp_ ) = Source_I(COp_ ) + &
            ( ni_I(H2Op_)*CxRate_I(H2Op_CO__COp_H2O_ ) + &
            ni_I(OHp_ )*CxRate_I(OHp_CO__COp_OH_   ) )*nn_I(CO_ )

       ! define special charge exchange loss term
       Source_H3Op = MassSpecies_V(RhoH3Op_) * &
            ( ni_I(H2Op_)*CxRate_I(H2Op_H2O__H3Op_OH_)  *nn_I(H2O_) + &
            ni_I(H2Op_)*CxRate_I(H2Op_H2__H3Op_H_  )  *nn_I(H2_ ) )

       Source_I = Source_I * MassSpecies_V	!*m_s

!!!! calculate loss terms case by case---!!!!
       ! calculate ionization source terms
       Loss_I(H2Op_) = CxRate_I(H2Op_H2O__H3Op_OH_)*nn_I(H2O_) + &
            CxRate_I(H2Op_CO__COp_H2O_)*nn_I(CO_ ) + &
            CxRate_I(H2Op_H2__H3Op_H_ )*nn_I(H2_ )
       Loss_I(Hp_  ) = CxRate_I(Hp_H2O__H2Op_H_   )*nn_I(H2O_) + &
            CxRate_I(H2Op_H2O__H3Op_OH_)*nn_I(H_ ) + &	! using H2O mirror for H mirror
            CxRate_I(Hp_OH__OHp_H_    )*nn_I(OH_ ) + &
            CxRate_I(Hp_O__Op_H_      )*nn_I(O_  )
       Loss_I(H3Op_) = 0.0
       Loss_I(OHp_ ) = CxRate_I(OHp_H2O__H3Op_O_  )*nn_I(H2O_) + &
            CxRate_I(OHp_H2O__H2Op_OH_)*nn_I(H2O_) + &
            CxRate_I(OHp_CO__COp_OH_  )*nn_I(CO_ )
       Loss_I(Op_  ) = CxRate_I(Op_H2O__H2Op_O_   )*nn_I(H2O_) + &
            CxRate_I(Op_OH__OHp_O_    )*nn_I(OH_ )
       Loss_I(COp_ ) = CxRate_I(COp_H2O__H2Op_CO_ )*nn_I(H2O_) + &
            CxRate_I(COp_OH__OHp_CO_  )*nn_I(OH_ )

       Loss_I = ( Loss_I + AlphaN_I ) * &		!*ms*ns
            State_VGB(SpeciesFirst_:SpeciesLast_,i,j,k, iBlock)

       ! define special charge exchange loss term
       Loss_H3Op = ( CxRate_I(H2Op_H2O__H3Op_OH_)*nn_I(H2O_) + &
            CxRate_I(H2Op_H2__H3Op_H_ )*nn_I(H2_ ) ) * &
            State_VGB(rhoH2Op_,i,j,k, iBlock)
       NumLoss_H3Op = ( CxRate_I(H2Op_H2O__H3Op_OH_)*nn_I(H2O_) + &
            CxRate_I(H2Op_H2__H3Op_H_ )*nn_I(H2_ ) ) * ni_I(H2Op_)

       ! calculate Source_VC
       totalSourceRho  = sum( Source_I )
       totalLossRho    = sum( Loss_I   )
       totalNumLoss    = sum( Loss_I / MassSpecies_V )
       collisn         = State_VGB(rho_,i,j,k, iBlock)*kin*nn_I(H2O_)

       Un   = UNeu_BLK(i,j,k,iBlock,1:3)
       U    = State_VGB(rhoUx_:rhoUz_,i,j,k,iBlock)/ &
            State_VGB(rho_,i,j,k,iBlock)
       unsqr= dot_product( Un, Un )
       usqr = dot_product( U , U  )

       Source_VC(SpeciesFirst_:SpeciesLast_,i,j,k) = &
            Source_VC(SpeciesFirst_:SpeciesLast_,i,j,k) + &
            Source_I - Loss_I

       do m = 1,3
	  Source_VC(rhoU_+m,i,j,k) = Source_VC(rhoU_+m,i,j,k) + &
               (totalSourceRho-Source_H3Op+collisn)*Un(m) - &
               (totalLossRho-Loss_H3Op+collisn)*U(m)
       enddo	! added H3O corrections and elastic collisions [ThesisJia07]

       Source_VC(p_,i,j,k) = Source_VC(p_,i,j,k) + &
            (1.0/3.0)*(totalSourceRho-Source_H3Op+collisn)* &
            dot_product( (Un-U) , (Un-U) ) - &
            0.5  *State_VGB(p_,i,j,k,iBlock)/totalNumRho* &
            ( (totalNumLoss-NumLoss_H3Op+collisn/mbar) + sum(AlphaN_I/MassSpecies_V* &
            State_VGB(SpeciesFirst_:SpeciesLast_,i,j,k, iBlock)) )
       Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) + &
            0.5*( (totalSourceRho-Source_H3Op+collisn)*Unsqr - &
            (totalLossRho-Loss_H3Op+collisn)*usqr - &
            InvGammaMinus1*State_VGB(p_,i,j,k,iBlock)/totalNumRho* &
            ((totalNumLoss-NumLoss_H3Op+collisn/mbar) + sum(AlphaN_I/MassSpecies_V* &
            State_VGB(SpeciesFirst_:SpeciesLast_,i,j,k, iBlock))) )

       ! source energy for explicit, p for implicit
    end do; enddo; enddo

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_impl_source_multi
  !============================================================================

  subroutine user_update_states(iBlock)

    use ModUpdateState, ONLY: update_state_normal
    use ModVarIndexes
    use ModSize
    use ModAdvance, ONLY: State_VGB, UseMultiSpecies
    use ModMain
    use ModPhysics, ONLY: cBoltzmann, No2Si_V, UnitN_, UnitP_
    use CON_planet, ONLY: NamePlanet

    integer,intent(in):: iBlock
    integer:: i,j,k
    real :: Pthmin_VC

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_update_states'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    call update_state_normal(iBlock)

    ! Begin update check of temperature::

    if (NamePlanet == 'HALLEY') then
       ! now check to see if the temperature is less than some
       ! prescribed minimum. If it is set it to the minimum value
       do k=1,nK ;   do j=1,nJ ;    do i=1,nI
          if ( UseMultiSpecies ) then
             Pthmin_VC = sum( &
                  State_VGB(SpeciesFirst_:SpeciesLast_,i,j,k,iBlock)/ &
                  MassSpecies_V(SpeciesFirst_:SpeciesLast_) )* &
                  No2Si_V(UnitN_)*cBoltzmann*Tion/No2Si_V(UnitP_)
          else
             Pthmin_VC = State_VGB(rho_,i,j,k,iBlock)/ &
                  mbar*No2Si_V(UnitN_)*cBoltzmann*Tion/No2Si_V(UnitP_)
          endif
          if( State_VGB(p_,i,j,k,iBlock) < Pthmin_VC ) &
               State_VGB(p_,i,j,k,iBlock) = Pthmin_VC
       enddo; enddo; enddo
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_update_states
  !============================================================================

end module ModUser
!==============================================================================
