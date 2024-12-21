!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModCalcSource

  use BATL_lib, ONLY: &
       test_start, test_stop, StringTest, iTest, jTest, kTest, &
       iBlockTest, iVarTest

  implicit none
  SAVE

  private ! except

  ! Public methods
  public :: calc_source
  public :: read_source_param

  ! Friction force relative to a (moving) background fluid
  real, public:: FrictionSi = 0.0, Friction = 0.0
  real, public:: FrictionUDim_D(3) = 0.0, FrictionU_D(3) = 0.0

  ! Advection source term
  logical:: UseAdvectionSource = .false.
  integer:: iVarAdvectFirst, iVarAdvectLast

contains
  !============================================================================
  subroutine read_source_param(NameCommand)

    use ModReadParam, ONLY: read_var
    use ModBatsrusUtility, ONLY: stop_mpi, get_ivar

    character(len=*), intent(in):: NameCommand

    character(len=20):: NameVar

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_source_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    select case(NameCommand)
    case("#ADVECTION")
       call read_var('UseAdvectionSource', UseAdvectionSource)
       if(UseAdvectionSource)then
          call read_var('NameVarAdvectFirst', NameVar)
          call get_ivar(NameVar, iVarAdvectFirst)
          call read_var('NameVarAdvectLast', NameVar)
          call get_ivar(NameVar, iVarAdvectLast)
       end if
    case("#FRICTION")
       call read_var('FrictionSi',    FrictionSi)
       call read_var('FrictionUxDim', FrictionUDim_D(1))
       call read_var('FrictionUyDim', FrictionUDim_D(2))
       call read_var('FrictionUzDim', FrictionUDim_D(3))
       Friction = 0.0 ! Make sure that normalization is done
    case default
       call stop_mpi(NameSub//': unknown command='//NameCommand)
    end select

    call test_stop(NameSub, DoTest)
  end subroutine read_source_param
  !============================================================================
  subroutine calc_source(iBlock)

    ! Calculate source terms Source_VC for block iBlock

    ! Notes by I. Sokolov on div B related source terms
    !
    ! Full list of source terms for different MHD models:
    ! 1. MHD with full field (Powell99)
    ! For UseB true and UseB0 false the sources are added if
    ! UseDivBSource is true
    !
    ! Source_V(Mx_:Mz_) = -B_D*div B
    ! Source_V(Bx_:Bz_) = -U_D*div B
    ! Source_V(Energy_) = -(B_D.U_D)*div B
    !
    ! Comments: 1a. divB is calculated in terms of the face values of B
    !           1b. other variables are cell-centered
    !
    ! 2. MHD with split field B1 + B0, B0 field is assumed to be potential and
    !    divergence free (Powell99). For  UseB true and UseB0 true, the sources
    !    proportional to div B1 are added if  UseDivBSource is true. The terms
    !    proportional to div B0 and curl B0 (unpublished, present in the code
    !    since 2001-2002) are added if UseB0Source is true:
    ! Source_V(Mx_:Mz_) = -B_D(Bx_:Bz_)*(div B1 + div B0) - B0*(div B1)- &
    !                      (curl B0) x B1
    ! Source_V(Bx_:Bz_) = -U_D*(div B1 + div B0)
    ! Source_V(Energy_) = -(B_D.U_D)*(div B1 + div B0)
    ! Comment: 2a. divB is calculated in terms of the face values of B1
    !          2b. B0*(div B1) is calculated together with (div B1), and
    !              different contrributions to (div B1) are multiplied by face-
    !              or cell-centered B0 field
    !          2c. Historically in Source_V(Bx_:Bz_) and in Source_V(Energy_)
    !              sources div B0 sources was omitted. To enable this, turrn on
    !              UseDivFullBSource. Depending on this switch, the divB source
    !              cleans either div B1 or dib (B1 + B0)
    !          2d. other variables are cell-centered
    ! 2. MHD with split field B1 + B0, B0 field is assumed to be potential and
    !    divergence free (Powell99). For  UseB true and UseB0 true, the sources
    !    proportional to div B1 are added if  UseDivBSource is true. The terms
    !    proportional to div B0 and curl B0 (unpublished, present in the code
    !    since 2001-2002) are added if UseB0Source is true:
    ! Source_V(Mx_:Mz_) = -B_D(Bx_:Bz_)*(div B1 + div B0) - B0*(div B1)- &
    !                      (curl B0) x B1
    ! Source_V(Bx_:Bz_) = -U_D*(div B1 + div B0)
    ! Source_V(Energy_) = -(B_D.U_D)*(div B1 + div B0)
    ! Comment: 2a. divB is calculated in terms of the face values of B1
    !          2b. B0*(div B1) is calculated together with (div B1), and
    !              different contrributions to (div B1) are multiplied by face-
    !              or cell-centered B0 field
    !          2c. Historically, in Source_V(Bx_:Bz_) and in Source_V(Energy_)
    !              div B0 sources were omitted. To enable them, turn on
    !              UseDivFullBSource. Depending on this switch, the divB source
    !              cleans the field by reducing either div B1 or dib (B1 + B0).
    !              In the momentum equation div B0 source is always present
    !          2d. other variables are cell-centered
    ! 3. MHD with split field B1 + B0, B0 field is NOT assumed to be potential
    !    and divergence free. For UseB true, UseB0 true, UseDivBSource true
    !    and UseB0Source =0, the extra sources are applied
    !    if UseeCurlB0 is true at R > rCurrrentFreeB0. The extra sources are:
    ! Source_V(Mx_:Mz_) = Source_V(Mx_:Mz_) + SM_D
    ! Source_V(Energy_) = Source_V(Energy_) + SM_D.U_D
    ! where SM_D = (curl B0) x B1 +
    !              div(B0 B0) - grad B0^2/2 - B0 div B0  ! If UseB0MomentumFlux
    !           or (curl B0) x B0                        ! Otherwise
    ! Comment: 2a. divB is calculated in terms of the face values of B1
    !          2b. B0*div B1 is calculated together with div B1, and
    !              different contrributions to div B1 are multiplied by face-
    !              or cell-centered B0 field
    !          2c. Historically in Source_V(Bx_:Bz_) and in Source_V(Energy_)
    !              sources div B0 sources were omitted. To enable then, turn on
    !              UseDivFullBSource. Depending on this switch, the divB source
    !              cleans either div B1 or dib (B1 + B0)
    !          2d. other variables are cell-centered

    use ModMain,          ONLY: iDirGravity, UseBody2, TypeCoordSystem, &
         UseB0, UseDivBsource, UseRadDiffusion, DoThinCurrentSheet, &
         UseUserSourceExpl, UseUserSourceImpl
    use ModAdvance
    use ModConservative,  ONLY: UseNonConservative
    use ModGeometry,      ONLY: r_GB, rBody2_GB, Used_GB
    use ModPhysics
    use ModCoordTransform
    use ModElectricField, ONLY: get_efield_in_comoving_frame
    use ModImplicit,      ONLY: UseFullImplicit
    use ModRadDiffusion,  ONLY: calc_source_rad_diffusion
    use ModMultiFluid
    use ModPointImplicit, ONLY: UsePointImplicit
    use ModMultiIon,      ONLY: multi_ion_source_expl, multi_ion_source_impl
    use ModIonElectron,   ONLY: ion_electron_source_impl
    use ModWaves,         ONLY: UseWavePressure, GammaWave, DivU_C
    use ModCoronalHeating, ONLY: UseCoronalHeating, get_block_heating
    use ModTurbulence,  ONLY: &
         CoronalHeating_C, UseAlfvenWaveDissipation, WaveDissipationRate_VC, &
         apportion_coronal_heating, UseTurbulentCascade, get_wave_reflection, &
         KarmanTaylorBeta2AlphaRatio, IsOnAwRepresentative, PoyntingFluxPerB, &
         UseReynoldsDecomposition, SigmaD, UseTransverseTurbulence, &
         rMinWaveReflection, AlfvenWaveVel_DC
    use ModRadiativeCooling, ONLY: RadCooling_C, UseRadCooling, &
         get_radiative_cooling, add_chromosphere_heating
    use ModChromosphere,  ONLY: DoExtendTransitionRegion,      &
         UseChromosphereHeating, get_tesi_c, TeSi_C
    use ModFaceFlux,      ONLY: Pe_G
    use ModHallResist,    ONLY: UseBiermannBattery, IonMassPerCharge_G
    use ModB0,            ONLY: set_b0_source, UseB0Source, UseCurlB0,    &
         rCurrentFreeB0, DivB0_C, CurlB0_DC, B0_DGB, B0_DX, B0_DY, B0_DZ, &
         UseDivFullBSource, B0MomentumSource_DC, &
         UseForceFreeB0, rMaxForceFreeB0
    use BATL_lib,         ONLY: IsCartesian, IsRzGeometry, &
         Xyz_DGB, CellSize_DB, CellVolume_GB, x_, y_, z_, Dim1_, Dim2_, Dim3_,&
         correct_face_value
    use ModViscosity,     ONLY: &
         UseViscosity, set_visco_factor_cell, ViscoFactor_C
    use ModBorisCorrection, ONLY: UseBorisCorrection, add_boris_source
    use ModPUI, ONLY: add_pui_source, DivUpui_C, Pu3_
    use ModUserInterface, ONLY: user_calc_sources_expl, user_calc_sources_impl

    integer, intent(in):: iBlock

    integer :: i, j, k, iVar, iFluid, iUn
    real :: Pe, Pwave, DivU

    ! Variable for B0 source term

    real :: CurlB0CrossB_D(3), DivFullB

    ! Variables needed for Boris source terms also used for div(u)
    real :: RhoInv
    ! Variables needed for anisotropic pressure
    real :: b_D(MaxDim), GradU_DD(nDim,MaxDim)

    ! Gravitational force towards body
    real :: ForcePerRho_D(3)

    ! Momentum index parallel with gravity direction
    integer:: iRhoUGrav

    ! For centrifugal force
    real :: Omega2

    ! Viscosity
    real, parameter:: cTwoThirds = 2.0/3.0
    real :: Visco, Tmp, ViscoCoeff

    ! Coronal Heating
    real :: QPerQtotal_I(nIonFluid)
    real :: QparPerQtotal_I(nIonFluid)
    real :: QePerQtotal

    ! Variables for multi-ion MHD
    real :: InvElectronDens, uPlus_D(3), u_D(3)

    ! Variables for Minimum radial speed
    real :: Ur, Rho, rUnit_D(3), Force_D(3)

    ! Variables needed for outer heliosphere turbulence
    real :: GradAlfven_DD(nDim,MaxDim)
    logical :: IsNewBlockAlfven

    ! Variables used to calculate sources for Reynolds decomposition:
    !
    ! Energy difference, in the standrad argo denoted as Z^2\sigma_D
    ! Can be eveluated from the total energy assuming the constant
    ! extent of energy difference, i. e. as constant \sigma_D multiplied
    ! by the total energy. Otherwise, a separate equation can be solved
    real :: wD
    !
    ! Mode interaction coefficients for conversion of
    ! W_+ and W_- to the energy difference, Z^2\sigma_D and vise versa
    real :: ModeConversionPlus, ModeConversionMinus

    ! To calculate above coefficients:
    real :: bDotbDotGradU, bDotGradVAlfven

    ! Normalization factor if the representative functions are used for
    ! Alfven waves
    real :: SqrtRho

    logical:: DoTestCell

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_source'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    do k = 1, nK; do j = 1, nJ; do i = 1, nI; do iVar = 1, nSource
       Source_VC(iVar,i,j,k) = 0
    end do; end do; end do; end do

    ! Contribution from the magnetic source is collected separately
    ! To be used in calculating electrric field for hybrid scheme
    do k = 1, nK; do j = 1, nJ; do i = 1, nI; do iVar = RhoUx_, RhoUz_
       SourceMhd_VC(iVar,i,j,k) = 0
    end do; end do; end do; end do

    ! Source term for continuity equation(s). Only single fluid case
    ! is considered. Impact on other equations is ignored.
    if(UseAdvectionSource)then
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE
          DivU = div_u(UnFirst_, i, j, k)
          Source_VC(iVarAdvectFirst:iVarAdvectLast,i,j,k) = &
               State_VGB(iVarAdvectFirst:iVarAdvectLast,i,j,k,iBlock)*DivU
       end do; end do; end do
    end if

    ! Calculate source terms for ion pressure
    if((UseNonconservative .or. UseAnisoPressure) .and. .not.UseEntropy)then
       do iFluid = 1, nFluid
          if(nFluid > 1) call select_fluid(iFluid)
          iUn = UnFirst_ + iFluid - 1

          if((UseAnisoPressure .and. IsIon_I(iFluid)) &
               .or. (UseViscosity .and. nFluid == 1))then

             if(UseViscosity)call set_visco_factor_cell(iBlock)

             ! Source terms for anisotropic pressure equations
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                DoTestCell = DoTest .and. i==iTest .and. &
                     j==jTest .and. k==kTest

                if(.not.Used_GB(i,j,k,iBlock)) CYCLE

                if(UseViscosity) then
                   ViscoCoeff = ViscoFactor_C(i,j,k)
                   if(.not. UseAnisoPressure .and. ViscoCoeff <= 0.0 ) CYCLE
                end if

                ! Calculate gradient tensor of velocity
                call calc_grad_u(GradU_DD, i, j, k, iBlock)

                if(UseAnisoPressure .and. IsIon_I(iFluid))then
                   ! Calculate bDotbDotGradU = b dot (b matmul GradU)

                   ! Calculate unit vector parallel with full B field
                   b_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
                   if(UseB0) b_D = b_D + B0_DGB(:,i,j,k,iBlock)
                   b_D = b_D/norm2(b_D)

                   ! Calculate b.grad u.b
                   bDotbDotGradU=dot_product(b_D, matmul(b_D(1:nDim),GradU_DD))

                   ! p parallel: -2*ppar*b.(b.(Grad U))
                   Source_VC(iPpar,i,j,k) = Source_VC(iPpar,i,j,k) &
                        - 2*State_VGB(iPpar,i,j,k,iBlock)*bDotbDotGradU

                   ! p : 2/3*(pperp - ppar)*b.(b.(GradU))
                   !     = (p - ppar)*b.(b.(GradU))
                   Source_VC(iP,i,j,k) = Source_VC(iP,i,j,k) &
                        + (State_VGB(iP,i,j,k,iBlock)  &
                        - State_VGB(iPpar,i,j,k,iBlock))*bDotbDotGradU
                end if

                if(UseViscosity) then

                   if(ViscoCoeff <= 0.0 ) CYCLE

                   ! Source(p) = (gamma - 1)*d_i u_j tau_ij
                   ! tau_ij = rho*nu*(d_i u_j + d_j u_i - 2/3 delta_ij div u)

                   ! Calculate first -2/3 (div u)^2
                   Visco              =         GradU_DD(Dim1_,1)
                   if(nDim > 1) Visco = Visco + GradU_DD(Dim2_,2)
                   if(nDim > 2) Visco = Visco + GradU_DD(Dim3_,3)
                   Visco = -cTwoThirds*Visco**2

                   ! Add 2*Sum_i (d_i u_i)^2
                   Visco              = Visco + 2.0*GradU_DD(Dim1_,1)**2
                   if(nDim > 1) Visco = Visco + 2.0*GradU_DD(Dim2_,2)**2
                   if(nDim > 2) Visco = Visco + 2.0*GradU_DD(Dim3_,3)**2

                   ! Add Sum_{i<j} (d_i u_j + d_j u_i)^2
                   Tmp              =       GradU_DD(Dim1_,2)
                   if(nDim > 1) Tmp = Tmp + GradU_DD(Dim2_,1)
                   Visco = Visco + Tmp**2

                   Tmp              =       GradU_DD(Dim1_,3)
                   if(nDim > 2) Tmp = Tmp + GradU_DD(Dim3_,1)
                   Visco = Visco + Tmp**2

                   if(nDim > 1)then
                      Tmp              =       GradU_DD(Dim2_,3)
                      if(nDim > 2) Tmp = Tmp + GradU_DD(Dim3_,2)
                      Visco = Visco + Tmp**2
                   end if

                   ! Source(p) = (gamma - 1)*tau:grad u
                   Source_VC(p_,i,j,k) = Source_VC(p_,i,j,k) + &
                        GammaMinus1*ViscoCoeff * &
                        State_VGB(Rho_,i,j,k,iBlock)*Visco
                end if
             end do; end do; end do

             if(DoTest .and. UseAnisoPressure .and. &
                  (iVarTest == iPparIon_I(1) .or. iVarTest == p_)) &
                  call write_source('After bDotbDotGradU')

          end if

          ! Adiabatic heating: -(g-1)*P*Div(U)
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             if(.not.Used_GB(i,j,k,iBlock)) CYCLE

             DivU = div_u(iUn, i, j, k)
             if(UseAnisoPressure .and. IsIon_I(iFluid))then
                Source_VC(iP,i,j,k) = Source_VC(iP,i,j,k) &
                     - (State_VGB(iP,i,j,k,iBlock) &
                     - State_VGB(iPpar,i,j,k,iBlock)/3.0)*DivU
             else
                Source_VC(iP,i,j,k) = Source_VC(iP,i,j,k) &
                     - GammaMinus1_I(iFluid)*State_VGB(iP,i,j,k,iBlock)*DivU
             end if
          end do; end do; end do

          if(DoTest .and. iVarTest==iP)call write_source('After p div U')

       end do ! iFluid
    end if ! (UseAnisoPressure.or.UseNonConservative) .and. .not.UseEntropy

    if(UseSpeedMin)then
       ! push radial ion speed above SpeedMin outside rSpeedMin
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(r_GB(i,j,k,iBlock) < rSpeedMin) CYCLE
          rUnit_D = Xyz_DGB(:,i,j,k,iBlock)/r_GB(i,j,k,iBlock)
          do iFluid = 1, nIonFluid
             if(nFluid > 1) call select_fluid(iFluid)
             Rho = State_VGB(iRho,i,j,k,iBlock)
             u_D = State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock)/Rho
             Ur =  sum(u_D *rUnit_D)
             if (Ur < SpeedMin) then
                Force_D = rUnit_D * Rho*(SpeedMin - Ur)/TauSpeedMin
                Source_VC(iRhoUx:iRhoUz,i,j,k) = &
                     Source_VC(iRhoUx:iRhoUz,i,j,k) + Force_D
                Source_VC(iEnergy,i,j,k) = Source_VC(iEnergy,i,j,k) &
                     + sum(Force_D * u_D)
             end if
          end do; end do; end do
       end do
       if(DoTest)call write_source('After UseSpeedMin')
    end if ! UseSpeedMin

    if(UseWavePressure .and. .not.IsOnAwRepresentative)then
       ! Back reaction of the Alfven wave pressure on
       ! the wave turbulence equations
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE

          if(UseMultiIon)then
             ! The following should be Div(Uplus). For zero Hall velocity
             ! this is the same as Div(Ue).
             DivU = div_u(UnLast_, i, j, k)
          else
             DivU = div_u(UnFirst_,i, j, k)
          end if

          ! Store div U so it can be used in ModWaves
          DivU_C(i,j,k) = DivU

          do iVar = WaveFirst_, WaveLast_
             Source_VC(iVar,i,j,k) = Source_VC(iVar,i,j,k) &
                  - DivU*(GammaWave - 1)*State_VGB(iVar,i,j,k,iBlock)
          end do
          if(WDiff_>1)Source_VC(WDiff_,i,j,k) = Source_VC(WDiff_,i,j,k) &
               - DivU*(GammaWave - 1)*State_VGB(WDiff_,i,j,k,iBlock)
       end do; end do; end do
       if(DoTest.and.UseMultiIon)call write_source('After UseWavePressure')
    end if ! UseAlfvenWavePressure
    if(UseWavePressure .and. .not.UseMultiIon)then
       ! Back reaction of the Alfven wave pressure on
       ! the wave turbulence equations as well as
       ! its contribution to the wave energy source
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE
          DivU = div_u(UnFirst_,i, j, k)
          Pwave = (GammaWave - 1) &
               *sum(State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock))
          if(IsOnAwRepresentative)Pwave = Pwave*PoyntingFluxPerB*&
               sqrt(State_VGB(Rho_,i,j,k,iBlock))
          ! The energy equation contains the work of the wave pressure
          ! -u.grad Pwave = -div(u Pwave) + Pwave div(u)
          ! The -div(u Pwave) is implemented as a flux in ModFaceFlux.
          ! Here we add the Pwave div(u) source term
          Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) + DivU*Pwave

          ! Add "geometrical source term" p/r to the radial momentum
          ! equation. The "radial" direction is along the Y axis
          ! NOTE: here we have to use signed radial distance!
          if(IsRzGeometry) Source_VC(RhoUy_,i,j,k) = &
               Source_VC(RhoUy_,i,j,k) + Pwave/Xyz_DGB(Dim2_,i,j,k,iBlock)
       end do; end do; end do
       if(DoTest)call write_source('After UseWavePressure')
    end if ! UseAlfvenWavePressure

    ! TeSi cell-centered values are calculated if needed
    if((UseCoronalHeating .or. UseAlfvenWaveDissipation) &
         .and. DoExtendTransitionRegion .or. UseRadCooling) &
         call get_tesi_c(iBlock, TeSi_C)

    if(UseCoronalHeating .or. UseAlfvenWaveDissipation)then
       ! Calculate heating functions and, if the AW turbulence
       ! is used, the wave dissipation rates
       call get_block_heating(iBlock)

       if(UseChromosphereHeating) call add_chromosphere_heating(TeSi_C, iBlock)

       if(UseReynoldsDecomposition)then
          DoTestCell = .false.
          IsNewBlockAlfven = .true.

          if(UseTransverseTurbulence)then
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                if(.not.Used_GB(i,j,k,iBlock)) CYCLE
                if(r_GB(i,j,k,iBlock) < rMinWaveReflection)CYCLE

                ! Calculate unit vector parallel with full B field
                b_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
                if(UseB0) b_D = b_D + B0_DGB(:,i,j,k,iBlock)
                b_D = b_D/norm2(b_D)
                ! Calculate gradient tensor of Alfven speed
                call calc_grad_alfven(GradAlfven_DD, i, j, k, iBlock, &
                     IsNewBlockAlfven)

                ! Calculate b.grad V_a
                bDotGradVAlfven  = &
                     dot_product(b_D, matmul(b_D(1:nDim), GradAlfven_DD))
                ! Calculate gradient tensor of velocity
                if(UseMultiIon)then
                   call calc_grad_uplus(GradU_DD, i, j, k, iBlock)
                else
                   call calc_grad_U(GradU_DD, i, j, k, iBlock)
                end if
                ! Calculate bb : grad u
                bDotbDotGradU = dot_product(b_D, matmul(b_D(1:nDim),GradU_DD))

                ModeConversionPlus  = 0.5*DivU_C(i,j,k) - bDotbDotGradU
                if(UseTurbulentCascade)then
                   bDotGradVAlfven = sign(min(abs(bDotGradVAlfven), 0.5*abs(&
                        WaveDissipationRate_VC(WaveFirst_,i,j,k) - &
                        WaveDissipationRate_VC(WaveLast_ ,i,j,k)) ),&
                        bDotGradVAlfven)
                   ModeConversionPlus = sign(min(abs(ModeConversionPlus), &
                        sqrt(bDotGradVAlfven**2 + product(&
                        WaveDissipationRate_VC(:,i,j,k)))), ModeConversionPlus)
                end if
                ModeConversionMinus = ModeConversionPlus
                ModeConversionPlus  = ModeConversionPlus  + bDotGradVAlfven
                ModeConversionMinus = ModeConversionMinus - bDotGradVAlfven

                if(WDiff_>1)then
                   wD = State_VGB(WDiff_,i,j,k,iBlock)
                else
                   wD = SigmaD*&
                        sum(State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock))
                end if

                Source_VC(WaveFirst_,i,j,k) = Source_VC(WaveFirst_,i,j,k) &
                     - 0.5*ModeConversionPlus *wD
                Source_VC(WaveLast_ ,i,j,k) = Source_VC(WaveLast_ ,i,j,k) &
                     - 0.5*ModeConversionMinus*wD

                if(.not.UseMultiIon)then
                   ! Energy source related to the Alfven wave source above
                   ! For multi ion it is done in ModMultiIon
                   if(IsOnAwRepresentative)then
                      Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) &
                           + 0.5*(ModeConversionPlus + ModeConversionMinus) &
                           *wD*PoyntingFluxPerB*State_VGB(Rho_,i,j,k,iBlock)
                   else
                      Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) &
                           + 0.5*(ModeConversionPlus + ModeConversionMinus)*wD
                   end if
                end if

                if(WDiff_>1) &
                     Source_VC(WDiff_,i,j,k) = Source_VC(WDiff_,i,j,k)    &
                     - ModeConversionMinus * &
                     State_VGB(WaveFirst_,i,j,k,iBlock) &
                     - ModeConversionPlus  * &
                     State_VGB(WaveLast_,i,j,k ,iBlock)
             end do; end do; end do
          else ! isotropic turbulence
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                if(.not.Used_GB(i,j,k,iBlock)) CYCLE
                ModeConversionPlus  = DivU_C(i,j,k)/6.0
                ModeConversionMinus = ModeConversionPlus

                ! Calculate gradient tensor of Alfven speed.
                ! The following is not the fastest way to calculate
                ! div u_A (or B \cdot grad rho), but the transverse turbulence
                ! is the more common turbulence to use, so speed is not
                ! an issue
                call calc_grad_alfven(GradAlfven_DD, i, j, k, iBlock, &
                     IsNewBlockAlfven)

                bDotGradVAlfven = &
                     sum([ (GradAlfven_DD(iVar,iVar), iVar=1, nDim) ])

                ModeConversionPlus  = ModeConversionPlus  + bDotGradVAlfven
                ModeConversionMinus = ModeConversionMinus - bDotGradVAlfven

                if(WDiff_>1)then
                   wD = State_VGB(WDiff_,i,j,k,iBlock)
                else
                   wD = SigmaD*&
                        sum(State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock))
                end if

                Source_VC(WaveFirst_,i,j,k) = Source_VC(WaveFirst_,i,j,k) &
                     - 0.5*ModeConversionPlus *wD
                Source_VC(WaveLast_, i,j,k) = Source_VC(WaveLast_ ,i,j,k) &
                     - 0.5*ModeConversionMinus*wD

                if(.not.UseMultiIon)then
                   ! Energy source related to the Alfven wave source above
                   ! For multi ion it is done in ModMultiIon
                   Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) &
                        + 0.5*(ModeConversionPlus + ModeConversionMinus)*wD
                end if

                if(WDiff_>1) &
                     Source_VC(WDiff_,i,j,k) = Source_VC(WDiff_,i,j,k) -&
                     Source_VC(WaveFirst_,i,j,k)*ModeConversionMinus   -&
                     Source_VC(WaveLast_ ,i,j,k)*ModeConversionPlus
             end do; end do; end do
          end if
       elseif(UseTurbulentCascade)then
          call get_wave_reflection(iBlock)
       end if
       if(UseAlfvenWaveDissipation)then
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             Source_VC(WaveFirst_:WaveLast_,i,j,k) = &
                  Source_VC(WaveFirst_:WaveLast_,i,j,k) &
                  - WaveDissipationRate_VC(:,i,j,k)*&
                  State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock)
             ! aritmetic average of cascade rates for w_D, if needed
             if(WDiff_>1)Source_VC(WDiff_,i,j,k) = Source_VC(WDiff_,i,j,k) &
                  - 0.50*sum(WaveDissipationRate_VC(:,i,j,k))*&
                  State_VGB(WDiff_,i,j,k,iBlock)
             ! Weighted average of cascade rates for Lperp_, if needed
             if(Lperp_ > 1)Source_VC(Lperp_,i,j,k) = Source_VC(Lperp_,i,j,k) +&
                  KarmanTaylorBeta2AlphaRatio*sum( &
                  WaveDissipationRate_VC(:,i,j,k)*  &
                  State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock)) / &
                  max(1e-30,sum(State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock)))&
                  *State_VGB(Lperp_,i,j,k,iBlock)
          end do; end do; end do
          if(DoTest)call write_source('After UseAlfvenWaveDissipation')
       end if ! UseAlfvenWaveDissipation

       if(UseCoronalHeating .and. DoUpdate_V(p_))then
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             if(UseElectronPressure)then
                call apportion_coronal_heating(i, j, k, iBlock, &
                     State_VGB(:,i,j,k,iBlock), &
                     WaveDissipationRate_VC(:,i,j,k), CoronalHeating_C(i,j,k),&
                     QPerQtotal_I, QparPerQtotal_I, QePerQtotal)

                Source_VC(Pe_,i,j,k) = Source_VC(Pe_,i,j,k) &
                     + CoronalHeating_C(i,j,k)*GammaElectronMinus1*QePerQtotal

                Source_VC(iPIon_I,i,j,k) = Source_VC(iPIon_I,i,j,k) &
                     + CoronalHeating_C(i,j,k)*QPerQtotal_I &
                     *GammaMinus1_I(1:nIonFluid)
                Source_VC(Energy_:Energy_-1+nIonFluid,i,j,k) = &
                     Source_VC(Energy_:Energy_-1+nIonFluid,i,j,k) &
                     + CoronalHeating_C(i,j,k)*QPerQtotal_I

                if(UseAnisoPressure)then
                   do iFluid = 1, nIonFluid
                      Source_VC(iPparIon_I(iFluid),i,j,k) = &
                           Source_VC(iPparIon_I(iFluid),i,j,k) &
                           + CoronalHeating_C(i,j,k)*QparPerQtotal_I(iFluid)*2
                   end do
                end if
             else
                Source_VC(p_,i,j,k) = Source_VC(p_,i,j,k) &
                     + CoronalHeating_C(i,j,k)*GammaMinus1
                Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) &
                     + CoronalHeating_C(i,j,k)
             end if
          end do; end do; end do
          if(DoTest)call write_source('After UseCoronalHeating')
       end if ! UseCoronalHeating
    end if
    if(IsOnAwRepresentative)then
       call vect_dot_grad_state(&
            Vector_DC= AlfvenWaveVel_DC, &
            iVar     = WaveFirst_,       &
            iBlock   = iBlock,           &
            DoLimitTimeStep = .false.)
       call vect_dot_grad_state(&
            Vector_DC= -AlfvenWaveVel_DC,&
            iVar     = WaveLast_,        &
            iBlock   = iBlock,           &
            DoLimitTimeStep = .true.)
    end if

    if(nPui > 1)then
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE

          DivUpui_C(i,j,k) = div_u(UnFirst_+Pu3_-1, i, j, k)

          Source_VC(PuiFirst_:PuiLast_,i,j,k) = &
               Source_VC(PuiFirst_:PuiLast_,i,j,k) &
               + State_VGB(PuiFirst_:PuiLast_,i,j,k,iBlock)*DivUpui_C(i,j,k)
       end do; end do; end do

       call add_pui_source(iBlock)
    end if

    if(UseRadCooling)then
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          call get_radiative_cooling(i, j, k, iBlock, TeSi_C(i,j,k), &
               RadCooling_C(i,j,k), NameCaller=NameSub, &
               Xyz_D=Xyz_DGB(:,i,j,k,iBlock) )

          if(UseElectronPressure)then
             Source_VC(Pe_,i,j,k) = Source_VC(Pe_,i,j,k) &
                  + RadCooling_C(i,j,k)*GammaElectronMinus1
             if(UseElectronEnergy) &
                  Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) &
                  + RadCooling_C(i,j,k)
          else
             Source_VC(p_,i,j,k)  = Source_VC(p_,i,j,k) &
                  + RadCooling_C(i,j,k)*GammaMinus1
             Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) &
                  + RadCooling_C(i,j,k)
          end if
       end do; end do; end do
       if(DoTest)call write_source('After UseRadCooling')
    end if

    if(UseElectronPressure .and. &
         .not.(UseElectronEntropy .and. UseMultiIon))then
       ! Calculate DivU = div(U_e)
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          DoTestCell = DoTest .and. i==iTest .and. j==jTest .and. k==kTest

          if(.not.Used_GB(i,j,k,iBlock)) CYCLE
          DivU = div_u(UnLast_, i, j, k)

          Pe = State_VGB(Pe_,i,j,k,iBlock)

          if (UseAnisoPe) then
             ! Calculate bDotbDotGradU = b dot (b matmul GradU)

             call calc_grad_uplus(GradU_DD, i, j, k, iBlock)

             ! Calculate unit vector parallel with full B field
             b_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
             if(UseB0) b_D = b_D + B0_DGB(:,i,j,k,iBlock)
             b_D = b_D/norm2(b_D)

             ! Calculate b.grad u.b
             bDotbDotGradU = dot_product(b_D, matmul(b_D(1:nDim),GradU_DD))

             ! p parallel: -2*ppar*b.(b.(Grad U))
             Source_VC(Pepar_,i,j,k) = Source_VC(Pepar_,i,j,k) &
                  - 2*State_VGB(Pepar_,i,j,k,iBlock)*bDotbDotGradU

             ! p : 2/3*(pperp - ppar)*b.(b.(GradU))
             !     = (p - ppar)*b.(b.(GradU))
             Source_VC(Pe_,i,j,k)    = Source_VC(Pe_,i,j,k)      &
                  + (State_VGB(Pe_,i,j,k,iBlock) -               &
                  State_VGB(Pepar_,i,j,k,iBlock))*bDotbDotGradU

             if(DoTestCell) write(*,*) ' GradU_DD=', GradU_DD

             if(DoTestCell .and. (iVarTest == Pepar_ .or. iVarTest == pe_)) &
                  call write_source('After bDotbDotGradUplus')
          end if

          ! For electron entropy equation there is no such term
          if(.not.UseElectronEntropy .and. .not. UseAnisoPe) then
             ! Adiabatic heating for electron pressure: -(g-1)*Pe*Div(U)
             Source_VC(Pe_,i,j,k) = &
                  Source_VC(Pe_,i,j,k) - GammaElectronMinus1*Pe*DivU
          else if(.not.UseElectronEntropy .and. UseAnisoPe) then
             Source_VC(Pe_,i,j,k) = Source_VC(Pe_,i,j,k) &
                  - (State_VGB(Pe_,i,j,k,iBlock)      &
                  - State_VGB(Pepar_,i,j,k,iBlock)/3.0)*DivU
          end if

          if(.not.UseMultiIon)then
             ! The energy equation contains the work of the electron pressure
             ! -u.grad Pe = -div(u Pe) + Pe div(u)
             ! The -div(u Pe) is implemented as a flux in ModFaceFlux.
             ! Here we add the Pe div(u_e) source term
             if(.not. UseElectronEnergy) &
                  Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) + Pe*DivU

             ! Add "geometrical source term" p/r to the radial momentum
             ! equation. The "radial" direction is along the Y axis
             ! NOTE: here we have to use signed radial distance!
             if(IsRzGeometry) Source_VC(RhoUy_,i,j,k) = &
                  Source_VC(RhoUy_,i,j,k) + Pe/Xyz_DGB(y_,i,j,k,iBlock)
          end if
       end do; end do; end do
       if(DoTest.and.iVarTest==Pe_)call write_source('After Pe div Ue')
    end if ! UseElectronPressure .and..not.(UseElectronEntropy.and.UseMultiIon)

    if(IsRzGeometry)then
       ! The following geometrical source terms are added for the MHD equations
       ! Source[mr]  =(p+B^2/2-Bphi**2+mphi**2/rho)/radius
       ! Source[mphi]=(-mphi*mr/rho+Bphi*Br)/radius(if no angular momentum fix)
       ! Source[Bphi]=((Bphi*mr-Br*mphi)/rho)/radius

       ! The cylindrical coordinates are renamed to X,Y,Z
       ! The cylindrical axis is along X
       ! The "radial" direction is along the Y axis
       ! The azimuthal direction is along the Z axis

       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE

          ! Source[mr] = (p+mphi**2/rho)/radius
          Source_VC(iRhoUy_I,i,j,k) = Source_VC(iRhoUy_I,i,j,k) &
               + (State_VGB(iP_I,i,j,k,iBlock) &
               +  State_VGB(iRhoUz_I,i,j,k,iBlock)**2 &
               /  State_VGB(iRho_I,i,j,k,iBlock)) &
               / Xyz_DGB(y_,i,j,k,iBlock)

          ! Source[mphi] = (-mphi*mr/rho)/radius
          Source_VC(iRhoUz_I,i,j,k) = Source_VC(iRhoUz_I,i,j,k) &
               - State_VGB(iRhoUz_I,i,j,k,iBlock) &
               * State_VGB(iRhoUy_I,i,j,k,iBlock) &
               /(State_VGB(iRho_I,i,j,k,iBlock)*Xyz_DGB(y_,i,j,k,iBlock))

          if(UseB)then
             ! Source[mr] = (B^2/2-Bphi**2)/radius
             Source_VC(RhoUy_,i,j,k) = Source_VC(RhoUy_,i,j,k) &
                  + (0.5*sum(State_VGB(Bx_:Bz_,i,j,k,iBlock)**2) &
                  -  State_VGB(Bz_,i,j,k,iBlock)**2) / Xyz_DGB(y_,i,j,k,iBlock)

             ! Source[mphi]=Bphi*Br/radius
             Source_VC(RhoUz_,i,j,k) = Source_VC(RhoUz_,i,j,k) &
                  + State_VGB(Bz_,i,j,k,iBlock)*State_VGB(By_,i,j,k,iBlock) &
                  / Xyz_DGB(y_,i,j,k,iBlock)

             ! Source[Bphi]=((Bphi*mr-Br*mphi)/rho)/radius
             Source_VC(Bz_,i,j,k) = Source_VC(Bz_,i,j,k) &
                  + (State_VGB(Bz_,i,j,k,iBlock) &
                  *   State_VGB(RhoUy_,i,j,k,iBlock) &
                  -  State_VGB(By_,i,j,k,iBlock) &
                  *   State_VGB(RhoUz_,i,j,k,iBlock))&
                  /State_VGB(Rho_,i,j,k,iBlock)/Xyz_DGB(y_,i,j,k,iBlock)
          end if
          if(UseB .and. UseB0)then
             ! Source[mr] = (B0.B1 - 2 B0phi * Bphi)/radius
             Source_VC(RhoUy_,i,j,k) = Source_VC(RhoUy_,i,j,k) &
                  + (sum(State_VGB(Bx_:Bz_,i,j,k,iBlock) &
                  *      B0_DGB(:,i,j,k,iBlock)) &
                  - 2.0*State_VGB(Bz_,i,j,k,iBlock)*B0_DGB(z_,i,j,k,iBlock)) &
                  / Xyz_DGB(y_,i,j,k,iBlock)

             ! Source[mphi] = (B0phi * Br + Bphi * B0r)/radius
             Source_VC(RhoUz_,i,j,k) = Source_VC(RhoUz_,i,j,k) &
                  + (B0_DGB(z_,i,j,k,iBlock)*State_VGB(By_,i,j,k,iBlock) &
                  +  B0_DGB(y_,i,j,k,iBlock)*State_VGB(Bz_,i,j,k,iBlock)) &
                  / Xyz_DGB(y_,i,j,k,iBlock)

             ! Source[Bphi]=((B0phi * mr - B0r * mphi)/rho)/radius
             Source_VC(Bz_,i,j,k) = Source_VC(Bz_,i,j,k) &
                  + (B0_DGB(z_,i,j,k,iBlock)*State_VGB(RhoUy_,i,j,k,iBlock) &
                  -  B0_DGB(y_,i,j,k,iBlock)*State_VGB(RhoUz_,i,j,k,iBlock))&
                  /State_VGB(Rho_,i,j,k,iBlock)/Xyz_DGB(y_,i,j,k,iBlock)
          end if
       end do; end do; end do

       ! For now, no Hall MHD implementation for rz-geometry
       if(UseB .and. UseBiermannBattery .and. &
            (UseElectronPressure .or. ElectronPressureRatio > 0.0 .or. &
            .not.UseIdealEos))then

          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             if(.not.Used_GB(i,j,k,iBlock)) CYCLE

             ! Source[Bphi] = [ 1/(q_e*n_e) * (dP_e/dZ) ] / radius
             Source_VC(Bz_,i,j,k) = Source_VC(Bz_,i,j,k) &
                  + IonMassPerCharge_G(i,j,k)/State_VGB(Rho_,i,j,k,iBlock) &
                  /Xyz_DGB(y_,i,j,k,iBlock) &
                  *0.5*(Pe_G(i+1,j,k) - Pe_G(i-1,j,k))/CellSize_DB(x_,iBlock)
          end do; end do; end do
       end if
       if(DoTest)call write_source('After UseRzGeometry')
    end if ! UseRzGeometry

    ! We consider two cases: curl(B0) is zero analytically or non-zero.
    ! These are distinguished by UseCurlB0 being true or false.
    !
    ! Momentum equation has the Lorentz force J x B
    !     = (curl B1) x (B1 + B0)     if curl B0 = 0
    !     = (curl B1+B0) x (B1 + B0)  if curl B0 is not 0
    !
    ! Conservative fluxes add the divergence of the Maxwell tensor
    !     div(B1^2 + B1.B0 - B1 B1 - B1 B0 - B0 B1)
    !
    ! Deviations between these two are
    !   -(B1 + B0) div(B1)- div B1 source. Added B0 div(B1) in calc_divb_source
    !   -B1 div(B0)       - div(B0) source
    !   -curl(B0) x B1    - remove this if curl B0 = 0
    !   +curl(B0) x B0    - add this if curl B0 is not 0

    ! Calculate the source terms formulated via B0
    if(UseB0) call set_b0_source(iBlock, DoSkipSetB0Face=.true.)

    if(UseB .and. UseDivbSource)then
       ! DivB = Flux_VXI(Bn_,i+1,j,k,iGang) - Flux_VXI(Bn_,i,j,k,iGang)
       ! if(nJ > 1) DivB = DivB + &
       !     Flux_VYI(Bn_,i,j+1,k,iGang) - Flux_VYI(Bn_,i,j,k,iGang)
       ! if(nK > 1) DivB = DivB + &
       !     Flux_VZI(Bn_,i,j,k+1,iGang) - Flux_VZI(Bn_,i,j,k,iGang)
       ! Change_V(RhoUx_:RhoUz_) = Change_V(RhoUx_:RhoUz_) &
       !     - DivB*State_VGB(Bx_:Bz_,i,j,k,iBlock)
       ! if(UseB0) Change_V(RhoUx_:RhoUz_) = Change_V(RhoUx_:RhoUz_) &
       !     - DivB*B0_DGB(:,i,j,k,iBlock)
       !
       ! Change_V(Bx_:Bz_) = Change_V(Bx_:Bz_) &
       !     - DivB*State_VGB(Ux_:Uz_,i,j,k,iBlock)
       ! Change_V(Energy_) = Change_V(Energy_) &
       !     - DivB*sum(State_VGB(Bx_:Bz_,i,j,k,iBlock) &
       !     *          State_VGB(Ux_:Uz_,i,j,k,iBlock))

       ! 8 wave scheme results in terms proportional to div B
       if(IsCartesian)then
          call calc_divb_source(iBlock)
       else
          ! Calculate div B1 via the face interpolation of the magnetic field
          ! In paarallel, the same source terms in the momentum equation
          ! calculated, which can be formally represented as -(B1 + B0) div B1.
          ! Although B1 and B0 contribution to this term look additive and
          ! identical, nevertheless their contributions are calculated
          ! differently and in different places. Historically, div B1 as
          ! calculated via the half of sum of internal and external face
          ! values of magnetic field, it may be also represented as the
          ! internal divergence, calculated in terms of the inner interpolated
          ! field values and the sum of contributions from jumps in the
          ! normal component, which may be thought of the magnetic charges
          ! associated with the given cell (the internal divergence) and
          ! the surface magnetic charges at the faces. Historically, the
          ! effect from B0 field, B0 div B1, is ALWAYS (well, if UseB0 is !
          ! true) is calculated via the decomposed magnetic charge, i.e.
          ! the internal divergence B1 is multiplied by the cell-centered
          ! value of B0 field, while the the face B0 field is applied to the
          ! surface magnetic charges at the faces. In contrast with this
          ! approach, the entire effect from B1 field, B1 div B1 was calculated
          ! by applying cell-centered B1 field to the total divergence (see
          ! below. Now, there is an option to set UseDivFullBSource=true
          ! so that the contribution from the source B1 div B1 is also
          ! calculated via the decomposed magnetic charges, hence, it is
          ! also calculated in the calc_divb_source_gencoord subroutine and
          ! is not calculated below.
          call calc_divb_source_gencoord
       end if

       if(DoTest)write(*,*)'divB=', DivB1_GB(iTest,jTest,kTest,iBlockTest)
#ifdef TESTACC
       if(DoTest)write(*,*)'divU=', div_u(UnFirst_, iTest, jTest, kTest)
#endif
       if(DoTest .and. iVarTest >= RhoUx_ .and. iVarTest <= RhoUz_)&
            call write_source('After B0B1 source')

       ! Add contributions to other source terms
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE

          if(UseMultiIon .or. UseEfield)then
             ! inv of electron charge density
             InvElectronDens = 1.0/sum(ChargePerMass_I(1:nTrueIon) &
                  *State_VGB(iRhoIon_I(1:nTrueIon),i,j,k,iBlock))

             ! charge average ion velocity
             uPlus_D(x_) = InvElectronDens*sum(ChargePerMass_I(1:nTrueIon) &
                  *State_VGB(iRhoUxIon_I(1:nTrueIon),i,j,k,iBlock))
             uPlus_D(y_) = InvElectronDens*sum(ChargePerMass_I(1:nTrueIon) &
                  *State_VGB(iRhoUyIon_I(1:nTrueIon),i,j,k,iBlock))
             uPlus_D(z_) = InvElectronDens*sum(ChargePerMass_I(1:nTrueIon) &
                  *State_VGB(iRhoUzIon_I(1:nTrueIon),i,j,k,iBlock))

             Source_VC(Bx_:Bz_,i,j,k) = Source_VC(Bx_:Bz_,i,j,k) &
                  - DivB1_GB(i,j,k,iBlock)*uPlus_D

             ! Total ion energy contains magnetic energy
             if(UseTotalIonEnergy) Source_VC(Energy_,i,j,k) = &
                  Source_VC(Energy_,i,j,k) - DivB1_GB(i,j,k,iBlock) &
                  *sum(State_VGB(Bx_:Bz_,i,j,k,iBlock)*uPlus_D)
          else
             RhoInv = 1.0/State_VGB(Rho_,i,j,k,iBlock)
             u_D = RhoInv*State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)
             DivFullB = DivB1_GB(i,j,k,iBlock)
             ! We can choose to adveect/clean div B1 or div (B1 + B0)
             if(UseDivFullBSource)DivFullB = DivFullB + DivB0_C(i,j,k)
             ! Add magnetic charge advection to the induction equuation
             Source_VC(Bx_:Bz_,i,j,k) = Source_VC(Bx_:Bz_,i,j,k) - DivFullB*u_D

             if(.not. UseMhdMomentumFlux) CYCLE

             ! -B1 div(B1)       - usual div B source
             ! if UseDivFullBSource=.true., the source term to the momentum
             ! equattion was added in calc_divb_source_gencoord and should not
             ! be added here.
             if(.not.UseDivFullBSource)&
                  SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                  SourceMhd_VC(RhoUx_:RhoUz_,i,j,k)  &
                  -DivB1_GB(i,j,k,iBlock)*State_VGB(Bx_:Bz_,i,j,k,iBlock)

             if(.not.IsMhd .and. .not.UseTotalIonEnergy) CYCLE
             Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) &
                  - DivFullB*sum(State_VGB(Bx_:Bz_,i,j,k,iBlock)*u_D)
          end if

       end do; end do; end do

       if(DoTest)call write_source('After divb source')

       if(UseB0Source .and. UseMhdMomentumFlux)then

          !   -B1 div(B0)     - div(B0) source
          ! -curl(B0) x B1    - remove this term (in case curl B0 should be 0)
          !                     have to undo this if curl B0 is actually not 0

          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             if(.not.Used_GB(i,j,k,iBlock)) CYCLE
             SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                  SourceMhd_VC(rhoUx_:rhoUz_,i,j,k) &
                  - State_VGB(Bx_:Bz_,i,j,k,iBlock)*DivB0_C(i,j,k) &
                  - cross_product( &
                  CurlB0_DC(:,i,j,k), State_VGB(Bx_:Bz_,i,j,k,iBlock))
          end do; end do; end do

          if(DoTest.and.iVarTest>=RhoUx_.and.iVarTest<=RhoUz_)then
             write(*,*)'DivB0_C  =',DivB0_C(iTest,jTest,kTest)
             write(*,*)'CurlB0_DC=',CurlB0_DC(:,iTest,jTest,kTest)
             call write_source('After B0 source')
          end if
       end if ! UseB0Source .and. UseMhdMomentumFlux

    else
       if(UseB)call calc_divb(iBlock)
    end if ! UseB .and. UseDivbSource

    if(UseB .and. UseCurlB0 .and. UseMhdMomentumFlux)then

       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE
          if(r_GB(i,j,k,iBlock) < rCurrentFreeB0)CYCLE

          ! +curl(B0) x B1    - undo source term above
          ! +curl(B0) x B1    - add this if B0MomentumFlux = .true.
          ! + div.(B0 B0 - I B0^2/2) - B0 div B0 -  or this, if it is .false.
          ! since curl B0 is not 0 and produces the force effect.

          ! Calculate curl(B0)xB1
          CurlB0CrossB_D = cross_product( CurlB0_DC(:,i,j,k),&
               State_VGB(Bx_:Bz_,i,j,k,iBlock))
          ! Add curl(B0)xB0 if necessary
          if(.not.UseForceFreeB0 .or. r_GB(i,j,k,iBlock) > rMaxForceFreeB0) &
               CurlB0CrossB_D = CurlB0CrossB_D + B0MomentumSource_DC(:,i,j,k)
          ! Add momentum source
          SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
               SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) + CurlB0CrossB_D
          ! Energy equation source term is (curl(B0)xB) . u
          Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) &
               + sum(CurlB0CrossB_D*State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock))&
               /State_VGB(rho_,i,j,k,iBlock)
       end do; end do; end do

       if(DoTest .and. (iVarTest == Energy_ &
            .or. iVarTest >= RhoUx_ .and. iVarTest<=RhoUz_)) &
            call write_source('After curl B0')
    end if ! UseB .and. UseCurlB0 .and. UseMhdMomentumFlux

    if(UseB .and. UseBorisCorrection &
         .and. ClightFactor < 0.9999 &
         .and. index(StringTest,'nodivE')<1)then
       call add_boris_source(iBlock)
       if(DoTest.and.iVarTest>=RhoUx_.and.iVarTest<=RhoUz_) &
            call write_source('After E div E')
    end if

    if(IsMhd) then
       do k = 1, nK; do j = 1, nJ; do i = 1, nI; do iVar = RhoUx_, RhoUz_
          Source_VC(iVar,i,j,k) = &
               Source_VC(iVar,i,j,k) + SourceMhd_VC(iVar,i,j,k)
       end do; end do; end do; end do
    endif

    ! The electric field in the comoving frame is needed
    if(UseMhdMomentumFlux)&
         call get_efield_in_comoving_frame(iBlock)

    ! These source terms apply to all the fluids
    do iFluid = 1, nFluid
       if(nFluid > 1) call select_fluid(iFluid)
       if(UseGravity)then
          ! Add gravitational force
          if(iDirGravity == 0)then
             ! Force is toward the body at the origin
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                if(.not.Used_GB(i,j,k,iBlock)) CYCLE
                ForcePerRho_D = &
                     Gbody*Xyz_DGB(:,i,j,k,iBlock)/r_GB(i,j,k,iBlock)**3
                Source_VC(iRhoUx:iRhoUz,i,j,k) =Source_VC(iRhoUx:iRhoUz,i,j,k)&
                     + State_VGB(iRho,i,j,k,iBlock)*ForcePerRho_D
                Source_VC(iEnergy,i,j,k) = Source_VC(iEnergy,i,j,k) + &
                     sum(State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock)*ForcePerRho_D)
             end do; end do; end do

             if(UseBody2)then
                do k=1,nK; do j=1,nJ; do i=1,nI
                   if(.not.Used_GB(i,j,k,iBlock)) CYCLE
                   ForcePerRho_D = Gbody2 &
                        * (Xyz_DGB(:,i,j,k,iBlock)-[xBody2,yBody2,zBody2]) &
                        / rBody2_GB(i,j,k,iBlock)**3
                   Source_VC(iRhoUx:iRhoUz,i,j,k) = &
                        Source_VC(iRhoUx:iRhoUz,i,j,k) &
                        + State_VGB(iRho,i,j,k,iBlock)*ForcePerRho_D
                   Source_VC(iEnergy,i,j,k) = Source_VC(iEnergy,i,j,k) + &
                        sum(State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock) &
                        *   ForcePerRho_D)
                end do; end do; end do
             end if
          else
             iRhoUGrav = iRhoUx - 1 + iDirGravity
             do k=1,nK; do j=1,nJ; do i=1,nI
                if(.not.Used_GB(i,j,k,iBlock)) CYCLE
                Source_VC(iRhoUGrav,i,j,k) = Source_VC(iRhoUGrav,i,j,k) &
                     + Gbody*State_VGB(iRho,i,j,k,iBlock)
                Source_VC(iEnergy,i,j,k) = Source_VC(iEnergy,i,j,k) &
                     + Gbody*State_VGB(iRhoUGrav,i,j,k,iBlock)
             end do; end do; end do
          end if
          if(DoTest .and. (iVarTest == Energy_ .or. &
               iVarTest >= iRhoUx .and. iVarTest <= iRhoUz))then
             call write_source('After gravity')
          end if
       end if

       ! Add Coriolis forces
       if(UseRotatingFrame)then
          ! Add centrifugal and Coriolis forces
          select case(TypeCoordSystem)
          case('HGC','HGR','hgc','hgr','GEO')
             ! This is a special case since Omega is parallel with the Z axis
             Omega2 = OmegaBody**2
             do k = 1, nK; do j = 1, nJ; do i = 1, nI
                if(.not.Used_GB(i,j,k,iBlock)) CYCLE
                Source_VC(iRhoUx,i,j,k) = Source_VC(iRhoUx,i,j,k) &
                     + 2*OmegaBody*State_VGB(iRhoUy,i,j,k,iBlock) &
                     + State_VGB(iRho,i,j,k,iBlock) &
                     *Omega2 * Xyz_DGB(x_,i,j,k,iBlock)

                Source_VC(iRhoUy,i,j,k) = Source_VC(iRhoUy,i,j,k) &
                     - 2*OmegaBody*State_VGB(iRhoUx,i,j,k,iBlock) &
                     + State_VGB(iRho,i,j,k,iBlock) &
                     *Omega2 * Xyz_DGB(y_,i,j,k,iBlock)

                Source_VC(iEnergy,i,j,k) = Source_VC(iEnergy,i,j,k) &
                     + Omega2 * sum(State_VGB(iRhoUx:iRhoUy,i,j,k,iBlock) &
                     *                         Xyz_DGB(x_:y_,i,j,k,iBlock))
             end do; end do; end do
          case default
             call stop_mpi(NameSub // &
                  ' Inertial forces are not implemented for'// &
                  ' TypeCoordSystem='//TypeCoordSystem)
          end select
          if(DoTest.and.iVarTest>=iRhoUx .and. iVarTest<=iRhoUy) &
               call write_source('After Coriolis')
       end if
    end do

    if(UseMultiIon)then
       ! Add momentum source terms containing the gradient of electron pressure
       call multi_ion_source_expl(iBlock)

       if(DoTest) call write_source('After MultiIon sources explicit')

       ! Add stiff momentum source terms (uPlus - Uion) and artificial friction
       ! Explicit evaluation of these source terms is for code development only
       if(.not.UsePointImplicit) call multi_ion_source_impl(iBlock)
    end if

    if(UseEfield)then
       ! Add total charge density source term for HypE scalar: c/eps0 = c^3
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE
          Source_VC(HypE_,i,j,k) = Clight*C2light * &
               sum(State_VGB(iRhoIon_I,i,j,k,iBlock)*ChargePerMass_I)
       end do; end do; end do
    end if

    if(UseEfield .and. .not.UsePointImplicit)then
       ! Explicit evaluation of these source terms is for code development only
       call ion_electron_source_impl(iBlock)
       if(DoTest) call write_source('After IonElectron sources implicit')
    end if

    if(UseB .and. .not.IsMhd .and. .not.(UseMultiIon .or. UseEfield))then
       ! Add JxB term for nonconservative MHD scheme (like LFM)
       call multi_ion_source_expl(iBlock)

       if(DoTest) call write_source('After JxB term')
    end if

    if(UseRadDiffusion .and. UseFullImplicit) &
         call calc_source_rad_diffusion(iBlock)

    if(FrictionSi > 0.0) call calc_friction(iBlock)

    if(SignB_>1 .and. DoThinCurrentSheet)then
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE

          ! Note that the velocity of the first (and only) fluid is used
          Source_VC(SignB_,i,j,k) = Source_VC(SignB_,i,j,k) &
               + State_VGB(SignB_,i,j,k,iBlock)*div_u(UnFirst_, i, j, k)
       end do; end do; end do
    end if

    if(UseUserSourceExpl)then
       call user_calc_sources_expl(iBlock)
       if(DoTest) call write_source('After explicit user sources')
    end if

    if(.not.UsePointImplicit .and. UseUserSourceImpl)then
       call user_calc_sources_impl(iBlock)
       if(DoTest) call write_source( &
            'After implicit user source evaluated explicitly')
    end if

    if(DoTest) call write_source('final')

    call test_stop(NameSub, DoTest, iBlock)
  contains
    !==========================================================================
    function div_u(iUn, i, j, k) result(DivU)

      ! Calculate div(u) for velocity indexed by iUn at cell i,j,k

      integer, intent(in):: iUn, i, j, k
      real:: DivU
      !------------------------------------------------------------------------
      DivU =                   Flux_VXI(iUn,i+1,j,k,1) &
           -                   Flux_VXI(iUn,i,j,k,1)
      if(nJ > 1) DivU = DivU + Flux_VYI(iUn,i,j+1,k,1) &
           -                   Flux_VYI(iUn,i,j,k,1)
      if(nK > 1) DivU = DivU + Flux_VZI(iUn,i,j,k+1,1) &
           -                   Flux_VZI(iUn,i,j,k,1)
      DivU = DivU/CellVolume_GB(i,j,k,iBlock)

    end function div_u
    !==========================================================================
    subroutine calc_grad_u(GradU_DD, i, j, k, iBlock)

      use BATL_lib, ONLY: FaceNormal_DDFB, CellVolume_GB, Dim1_, Dim2_, Dim3_

      integer, intent(in) :: i, j, k, iBlock
      real,   intent(out) :: GradU_DD(nDim,MaxDim)

      integer :: iDir

      character(len=*), parameter:: NameSub = 'calc_grad_u'
      !------------------------------------------------------------------------
      GradU_DD = 0.0

      if (DoTestCell) then
         write(*,*) 'iFluid =', iFluid
         write(*,*) 'ux_D =', LeftState_VX(iUx:iUz,i+1,j,  k)
         write(*,*) 'ux_D =', LeftState_VX(iUx:iUz,i,  j,  k)
         write(*,*) 'uy_D =', LeftState_VY(iUx:iUz,i,  j+1,k)
         write(*,*) 'uy_D =', LeftState_VY(iUx:iUz,i,  j,  k)
         write(*,*) 'uz_D =', LeftState_VZ(iUx:iUz,i,  j,  k+1)
         write(*,*) 'uz_D =', LeftState_VZ(iUx:iUz,i,  j,  k)
      end if

      ! Calculate gradient tensor of velocity
      if(IsCartesian) then
         GradU_DD(Dim1_,:) = &
              ( LeftState_VX(iUx:iUz,i+1,j,k)   &
              + RightState_VX(iUx:iUz,i+1,j,k)  &
              - LeftState_VX(iUx:iUz,i,j,k)     &
              - RightState_VX(iUx:iUz,i,j,k) )  &
              /(2*CellSize_DB(Dim1_,iBlock))

         if(nJ > 1) GradU_DD(Dim2_,:) = &
              ( LeftState_VY(iUx:iUz,i,j+1,k)   &
              + RightState_VY(iUx:iUz,i,j+1,k)  &
              - LeftState_VY(iUx:iUz,i,j,k)     &
              - RightState_VY(iUx:iUz,i,j,k) )  &
              /(2*CellSize_DB(Dim2_,iBlock))

         if(nK > 1) GradU_DD(Dim3_,:) = &
              ( LeftState_VZ(iUx:iUz,i,j,k+1)   &
              + RightState_VZ(iUx:iUz,i,j,k+1)  &
              - LeftState_VZ(iUx:iUz,i,j,k)     &
              - RightState_VZ(iUx:iUz,i,j,k) )  &
              /(2*CellSize_DB(Dim3_,iBlock))

      else if(IsRzGeometry) then
         call stop_mpi(NameSub//': RZ geometry to be implemented')
      else
         do iDir = 1, MaxDim
            iVar = iUx - 1 + iDir

            GradU_DD(:,iDir) = &
                 0.5*(LeftState_VX(iVar,i+1,j,k) &
                 + RightState_VX(iVar,i+1,j,k))* &
                 FaceNormal_DDFB(:,1,i+1,j,k,iBlock) &
                 - 0.5*(LeftState_VX(iVar,i,j,k) &
                 + RightState_VX(iVar,i,j,k))* &
                 FaceNormal_DDFB(:,1,i,j,k,iBlock)

            if(nJ == 1) CYCLE

            GradU_DD(:,iDir) = GradU_DD(:,iDir) + &
                 0.5*(LeftState_VY(iVar,i,j+1,k) &
                 + RightState_VY(iVar,i,j+1,k))* &
                 FaceNormal_DDFB(:,2,i,j+1,k,iBlock) &
                 - 0.5*(LeftState_VY(iVar,i,j,k) &
                 + RightState_VY(iVar,i,j,k))* &
                 FaceNormal_DDFB(:,2,i,j,k,iBlock)

            if(nK == 1) CYCLE

            GradU_DD(:,iDir) = GradU_DD(:,iDir) + &
                 0.5*(LeftState_VZ(iVar,i,j,k+1) &
                 + RightState_VZ(iVar,i,j,k+1))* &
                 FaceNormal_DDFB(:,3,i,j,k+1,iBlock) &
                 - 0.5*(LeftState_VZ(iVar,i,j,k) &
                 + RightState_VZ(iVar,i,j,k))* &
                 FaceNormal_DDFB(:,3,i,j,k,iBlock)
         end do

         GradU_DD = GradU_DD / CellVolume_GB(i,j,k,iBlock)

      end if

    end subroutine calc_grad_u
    !==========================================================================
    subroutine calc_grad_alfven(GradAlfven_DD, i, j, k, iBlock, IsNewBlock)

      use BATL_lib, ONLY: FaceNormal_DDFB, CellVolume_GB, Dim1_, Dim2_, Dim3_

      integer, intent(in) :: i, j, k, iBlock
      logical, intent(inout) :: IsNewBlock
      real,   intent(out) :: GradAlfven_DD(nDim,MaxDim)

      real, allocatable, save :: Alfven_VFD(:,:,:,:,:)
      integer :: iDir

      character(len=*), parameter:: NameSub = 'calc_grad_alfven'
      !------------------------------------------------------------------------
      if(.not.allocated(Alfven_VFD)) &
           allocate(Alfven_VFD(MaxDim,0:nI+1,j0_:nJp1_,k0_:nKp1_,nDim))

      if(IsNewBlock)then
         call get_alfven_speed(Alfven_VFD)
         IsNewBlock = .false.
      end if

      GradAlfven_DD = 0.0

      ! Calculate gradient tensor of velocity
      if(IsCartesian) then
         GradAlfven_DD(Dim1_,:) = &
              (Alfven_VFD(:,i+1,j,k,Dim1_) - Alfven_VFD(:,i,j,k,Dim1_)) &
              /CellSize_DB(Dim1_,iBlock)

         if(nJ > 1) GradAlfven_DD(Dim2_,:) = &
              (Alfven_VFD(:,i,j+1,k,Dim2_) - Alfven_VFD(:,i,j,k,Dim2_)) &
              /CellSize_DB(Dim2_,iBlock)

         if(nK > 1) GradAlfven_DD(Dim3_,:) = &
              (Alfven_VFD(:,i,j,k+1,Dim3_) - Alfven_VFD(:,i,j,k,Dim3_)) &
              /CellSize_DB(Dim3_,iBlock)

      else if(IsRzGeometry) then
         call stop_mpi(NameSub//': RZ geometry to be implemented')
      else
         do iDir = 1, MaxDim
            GradAlfven_DD(:,iDir) = &
                 Alfven_VFD(iDir,i+1,j,k,Dim1_) &
                 *FaceNormal_DDFB(:,Dim1_,i+1,j,k,iBlock) &
                 - Alfven_VFD(iDir,i,j,k,Dim1_) &
                 *FaceNormal_DDFB(:,Dim1_,i,j,k,iBlock)

            if(nJ == 1) CYCLE

            GradAlfven_DD(:,iDir) = GradAlfven_DD(:,iDir) + &
                 Alfven_VFD(iDir,i,j+1,k,Dim2_) &
                 *FaceNormal_DDFB(:,Dim2_,i,j+1,k,iBlock) &
                 - Alfven_VFD(iDir,i,j,k,Dim2_) &
                 *FaceNormal_DDFB(:,Dim2_,i,j,k,iBlock)

            if(nK == 1) CYCLE

            GradAlfven_DD(:,iDir) = GradAlfven_DD(:,iDir) + &
                 Alfven_VFD(iDir,i,j,k+1,Dim3_) &
                 *FaceNormal_DDFB(:,Dim3_,i,j,k+1,iBlock) &
                 - Alfven_VFD(iDir,i,j,k,Dim3_) &
                 *FaceNormal_DDFB(:,Dim3_,i,j,k,iBlock)
         end do

         GradAlfven_DD = GradAlfven_DD/CellVolume_GB(i,j,k,iBlock)

      end if

    end subroutine calc_grad_alfven
    !==========================================================================
    subroutine get_alfven_speed(Alfven_VFD)

      use ModAdvance, ONLY: &
           LeftState_VX, LeftState_VY, LeftState_VZ,  &
           RightState_VX, RightState_VY, RightState_VZ
      use ModB0, ONLY: B0_DX, B0_DY, B0_DZ
      use ModMain, ONLY: UseB0
      use ModVarIndexes, ONLY: Bx_, Bz_, Rho_

      real, intent(out) :: Alfven_VFD(MaxDim,0:nI+1,j0_:nJp1_,k0_:nKp1_,nDim)

      real :: FullB_D(MaxDim)
      integer :: i, j, k

      character(len=*), parameter:: NameSub = 'get_alfven_speed'
      !------------------------------------------------------------------------

      do k = 1, nK; do j = 1, nJ; do i = 1, nI+1
         FullB_D = 0.5*(LeftState_VX(Bx_:Bz_,i,j,k) &
              + RightState_VX(Bx_:Bz_,i,j,k))
         if(UseB0) FullB_D = FullB_D + B0_DX(:,i,j,k)
         Rho = 0.5*(LeftState_VX(Rho_,i,j,k) + RightState_VX(Rho_,i,j,k))
         Alfven_VFD(:,i,j,k,Dim1_) = FullB_D/sqrt(Rho)
      end do; end do; end do

      if(nJ > 1)then
         do k = 1, nK; do j = 1, nJ+1; do i = 1, nI
            FullB_D = 0.5*(LeftState_VY(Bx_:Bz_,i,j,k) &
                 + RightState_VY(Bx_:Bz_,i,j,k))
            if(UseB0) FullB_D = FullB_D + B0_DY(:,i,j,k)
            Rho = 0.5*(LeftState_VY(Rho_,i,j,k) + RightState_VY(Rho_,i,j,k))
            Alfven_VFD(:,i,j,k,Dim2_) = FullB_D/sqrt(Rho)
         end do; end do; end do
      end if

      if(nK > 1)then
         do k = 1, nK+1; do j = 1, nJ; do i = 1, nI
            FullB_D = 0.5*(LeftState_VZ(Bx_:Bz_,i,j,k) &
                 + RightState_VZ(Bx_:Bz_,i,j,k))
            if(UseB0) FullB_D = FullB_D + B0_DZ(:,i,j,k)
            Rho = 0.5*(LeftState_VZ(Rho_,i,j,k) + RightState_VZ(Rho_,i,j,k))
            Alfven_VFD(:,i,j,k,Dim3_) = FullB_D/sqrt(Rho)
         end do; end do; end do
      end if

    end subroutine get_alfven_speed
    !==========================================================================
    subroutine vect_dot_grad_state(Vector_DC, iVar, iBlock, DoLimitTimeStep)

      use BATL_lib, ONLY: FaceNormal_DDFB, CellVolume_GB, Dim1_, Dim2_, Dim3_,&
           CellFace_DB
      use ModAdvance, ONLY:  Flux_VXI, Flux_VYI, Flux_VZI, Vdt_
      real,    intent(in) :: Vector_DC(MaxDim,nI,nJ,nK)
      integer, intent(in) :: iVar, iBlock
      logical, optional, intent(in) :: DoLimitTimeStep
      real :: Source_C(nI,nJ,nK), VectorComp, VectDotArea
      integer :: i, j, k

      character(len=*), parameter:: NameSub = 'vect_dot_grad_state'
      !------------------------------------------------------------------------
      Source_C = 0.0
      if(IsCartesian) then
         do k = 1,nK; do j=1,nJ; do i=1,nI
            if(.not.Used_GB(i,j,k,iBlock)) CYCLE
            VectorComp = Vector_DC(x_,i,j,k)
            if(present(DoLimitTimeStep))then
               VectDotArea = abs(VectorComp)*CellFace_DB(x_,iBlock)
               Flux_VXI(Vdt_,i:i+1,j,k,1) = max(        &
                    Flux_VXI(Vdt_,i:i+1,j,k,1),         &
                    VectDotArea  +                          &
                    abs(Flux_VXI(UnFirst_,i:i+1,j,k,1)) )
            end if
            Source_C(i,j,k) = 0.5*(VectorComp*(&
                 LeftState_VX(iVar,i+1,j,k) + RightState_VX(iVar,i+1,j,k)&
                 -LeftState_VX(iVar,i,j,k) - RightState_VX(iVar,i,j,k))  &
                 + abs(VectorComp)*(&
                 LeftState_VX(iVar,i+1,j,k) - RightState_VX(iVar,i+1,j,k)&
                 -LeftState_VX(iVar,i,j,k) + RightState_VX(iVar,i,j,k)) )&
                 /CellSize_DB(Dim1_,iBlock)

            if(nJ > 1)then
               VectorComp = Vector_DC(y_,i,j,k)
               if(present(DoLimitTimeStep))then
                  VectDotArea = abs(VectorComp)*CellFace_DB(y_,iBlock)
                  Flux_VYI(Vdt_,i,j:j+1,k,1) = max(        &
                       Flux_VYI(Vdt_,i,j:j+1,k,1),         &
                       VectDotArea  +                          &
                       abs(Flux_VYI(UnFirst_,i,j:j+1,k,1)) )
               end if
               Source_C(i,j,k) = Source_C(i,j,k) + 0.5*(VectorComp*(&
                    LeftState_VY(iVar,i,j+1,k) + RightState_VY(iVar,i,j+1,k)&
                    -LeftState_VY(iVar,i,j,k) - RightState_VY(iVar,i,j,k))  &
                    + abs(VectorComp)*(&
                    LeftState_VY(iVar,i,j+1,k) - RightState_VY(iVar,i,j+1,k)&
                    -LeftState_VY(iVar,i,j,k) + RightState_VY(iVar,i,j,k)) )&
                    /CellSize_DB(Dim2_,iBlock)
            end if
            if(nK > 1)then
               VectorComp = Vector_DC(z_,i,j,k)
               if(present(DoLimitTimeStep))then
                  VectDotArea = abs(VectorComp)*CellFace_DB(z_,iBlock)
                  Flux_VZI(Vdt_,i,j,k:k+1,1) = max(        &
                       Flux_VZI(Vdt_,i,j,k:k+1,1),         &
                       VectDotArea +                           &
                       abs(Flux_VZI(UnFirst_,i,j,k:k+1,1)) )
               end if
               Source_C(i,j,k) = Source_C(i,j,k) + 0.5*(VectorComp*(&
                    LeftState_VZ(iVar,i,j,k+1) + RightState_VZ(iVar,i,j,k+1)&
                    -LeftState_VZ(iVar,i,j,k) - RightState_VZ(iVar,i,j,k))  &
                    + abs(VectorComp)*(&
                    LeftState_VZ(iVar,i,j,k+1) - RightState_VZ(iVar,i,j,k+1)&
                    -LeftState_VZ(iVar,i,j,k) + RightState_VZ(iVar,i,j,k)) )&
                    /CellSize_DB(Dim3_,iBlock)
            end if
         end do; end do; end do
      else if(IsRzGeometry) then
         call stop_mpi(NameSub//': RZ geometry to be implemented')
      else
         do k = 1,nK; do j=1,nJ; do i=1,nI
            ! Face X Right
            if(.not.Used_GB(i,j,k,iBlock)) CYCLE
            VectorComp = sum(Vector_DC(:,i,j,k)* &
                 FaceNormal_DDFB(:,Dim1_,i+1,j,k,iBlock))
            if(present(DoLimitTimeStep))then
               VectDotArea = abs(VectorComp)
               Flux_VXI(Vdt_,i+1,j,k,1) = max(             &
                    Flux_VXI(Vdt_,i+1,j,k,1),              &
                    VectDotArea +                              &
                    abs(Flux_VXI(UnFirst_,i+1,j,k,1))      )
            end if
            Source_C(i,j,k) = 0.5*(VectorComp*(&
                 LeftState_VX(iVar,i+1,j,k) + RightState_VX(iVar,i+1,j,k)) &
                 + abs(VectorComp)*(&
                 LeftState_VX(iVar,i+1,j,k) - RightState_VX(iVar,i+1,j,k)))
            ! Face X Left
            VectorComp = -sum(Vector_DC(:,i,j,k)* &
                 FaceNormal_DDFB(:,Dim1_,i,j,k,iBlock))
            if(present(DoLimitTimeStep))then
               VectDotArea = abs(VectorComp)
               Flux_VXI(Vdt_,i,j,k,1) = max(               &
                    Flux_VXI(Vdt_,i,j,k,1),                &
                    VectDotArea +                              &
                    abs(Flux_VXI(UnFirst_,i,j,k,1))        )
            end if
            Source_C(i,j,k) = Source_C(i,j,k) + 0.5*(VectorComp*(&
                 LeftState_VX(iVar,i,j,k) + RightState_VX(iVar,i,j,k)) &
                 + abs(VectorComp)*(&
                 -LeftState_VX(iVar,i,j,k) + RightState_VX(iVar,i,j,k)))
            if(nJ > 1)then
               ! Face Y Right
               VectorComp = sum(Vector_DC(:,i,j,k)* &
                    FaceNormal_DDFB(:,Dim2_,i,j+1,k,iBlock))
               if(present(DoLimitTimeStep))then
                  VectDotArea = abs(VectorComp)
                  Flux_VYI(Vdt_,i,j+1,k,1) = max(          &
                       Flux_VYI(Vdt_,i,j+1,k,1),           &
                       VectDotArea +                           &
                       Flux_VYI(UnFirst_,i,j+1,k,1))
               end if
               Source_C(i,j,k) = Source_C(i,j,k) + 0.5*(VectorComp*(&
                    LeftState_VY(iVar,i,j+1,k) + RightState_VY(iVar,i,j+1,k))&
                    + abs(VectorComp)*(&
                    LeftState_VY(iVar,i,j+1,k) - RightState_VY(iVar,i,j+1,k)))
               ! Face Y Left
               VectorComp = -sum(Vector_DC(:,i,j,k)* &
                    FaceNormal_DDFB(:,Dim2_,i,j,k,iBlock))
               if(present(DoLimitTimeStep))then
                  VectDotArea = abs(VectorComp)
                  Flux_VYI(Vdt_,i,j,k,1) = max(            &
                       Flux_VYI(Vdt_,i,j,k,1),             &
                       VectDotArea +                           &
                       Flux_VYI(UnFirst_,i,j,k,1))
               end if
               Source_C(i,j,k) = Source_C(i,j,k) + 0.5*(VectorComp*(&
                    LeftState_VY(iVar,i,j,k) + RightState_VY(iVar,i,j,k))&
                    + abs(VectorComp)*(&
                    -LeftState_VY(iVar,i,j,k) + RightState_VY(iVar,i,j,k)))
            end if
            if(nK > 1) then
               ! Face Z Right
               VectorComp = sum(Vector_DC(:,i,j,k)* &
                    FaceNormal_DDFB(:,Dim3_,i,j,k+1,iBlock))
               if(present(DoLimitTimeStep))then
                  VectDotArea = abs(VectorComp)
                  Flux_VZI(Vdt_,i,j,k+1,1) = max(          &
                       Flux_VZI(Vdt_,i,j,k+1,1),           &
                       VectDotArea +                           &
                       Flux_VZI(UnFirst_,i,j,k+1,1)            )
               end if
               Source_C(i,j,k) = Source_C(i,j,k) + 0.5*(VectorComp*(&
                    LeftState_VZ(iVar,i,j,k+1) + RightState_VZ(iVar,i,j,k+1))&
                    + abs(VectorComp)*(&
                    LeftState_VZ(iVar,i,j,k+1) - RightState_VZ(iVar,i,j,k+1)))
               ! Face Z Left
               VectorComp = -sum(Vector_DC(:,i,j,k)* &
                    FaceNormal_DDFB(:,Dim3_,i,j,k,iBlock))
               if(present(DoLimitTimeStep))then
                  VectDotArea = abs(VectorComp)
                  Flux_VZI(Vdt_,i,j,k,1) = max(            &
                       Flux_VZI(Vdt_,i,j,k,1),             &
                       VectDotArea +                           &
                       Flux_VZI(UnFirst_,i,j,k,1))
               end if
               Source_C(i,j,k) = Source_C(i,j,k) + 0.5*(VectorComp*(&
                    LeftState_VZ(iVar,i,j,k) + RightState_VZ(iVar,i,j,k))+&
                    abs(VectorComp)*(&
                    -LeftState_VZ(iVar,i,j,k) + RightState_VZ(iVar,i,j,k)))
            end if

            Source_C(i,j,k)  = Source_C(i,j,k)/CellVolume_GB(i,j,k,iBlock)
         end do; end do; end do
      end if
      Source_VC(iVar,:,:,:) = Source_VC(iVar,:,:,:) - Source_C(:,:,:)
    end subroutine vect_dot_grad_state
    !==========================================================================
    subroutine calc_grad_uplus(GradU_DD, i, j, k, iBlock)

      ! This routine calculates the gradient tensor of uPlus_D, which is used
      ! in anisotropic Pe.

      use BATL_lib, ONLY: FaceNormal_DDFB, CellVolume_GB, Dim1_, Dim2_, Dim3_

      integer, intent(in) :: i, j, k, iBlock
      real,   intent(out) :: GradU_DD(nDim,MaxDim)

      ! uPlus_D on the left and right faces
      real :: uPlusLeft_D(3),  uPlusRight_D(3)
      real :: uPlusLeft1_D(3), uPlusRight1_D(3)

      integer :: iDir
      real :: uPlusLeft_DD(3,nDim),  uPlusRight_DD(3,nDim)
      real :: uPlusLeft1_DD(3,nDim), uPlusRight1_DD(3,nDim)

      character(len=*), parameter:: NameSub = 'calc_grad_uplus'
      !------------------------------------------------------------------------
      GradU_DD = 0.0

      ! Calculate gradient tensor of u_plus
      if(IsCartesian) then
         ! Obtain the uPlus_D on the corresponding faces
         call get_uplus(LeftState_VX( :,i+1,j,k), uPlusLeft1_D )
         call get_uplus(LeftState_VX( :,i,  j,k), uPlusLeft_D  )
         call get_uplus(RightState_VX(:,i+1,j,k), uPlusRight1_D)
         call get_uplus(RightState_VX(:,i,  j,k), uPlusRight_D )

         GradU_DD(Dim1_,:) = &
              (uPlusLeft1_D + uPlusRight1_D - uPlusLeft_D - uPlusRight_D)  &
              /(2*CellSize_DB(Dim1_,iBlock))

         if(nJ > 1) then
            ! Obtain the uPlus_D on the corresponding faces
            call get_uplus(LeftState_VY( :,i,j+1,k), uPlusLeft1_D )
            call get_uplus(LeftState_VY( :,i,j,  k), uPlusLeft_D  )
            call get_uplus(RightState_VY(:,i,j+1,k), uPlusRight1_D)
            call get_uplus(RightState_VY(:,i,j,  k), uPlusRight_D )

            GradU_DD(Dim2_,:) = &
                 (uPlusLeft1_D + uPlusRight1_D - uPlusLeft_D - uPlusRight_D) &
                 /(2*CellSize_DB(Dim1_,iBlock))
         end if

         if(nK > 1) then
            ! Obtain the uPlus_D on the corresponding faces
            call get_uplus(LeftState_VZ( :,i,j,k+1), uPlusLeft1_D )
            call get_uplus(LeftState_VZ( :,i,j,k), uPlusLeft_D    )
            call get_uplus(RightState_VZ(:,i,j,k+1), uPlusRight1_D)
            call get_uplus(RightState_VZ(:,i,j,k), uPlusRight_D   )

            GradU_DD(Dim3_,:) = &
                 (uPlusLeft1_D + uPlusRight1_D - uPlusLeft_D - uPlusRight_D) &
                 /(2*CellSize_DB(Dim1_,iBlock))
         end if

      else if(IsRzGeometry) then
         call stop_mpi(NameSub//': RZ geometry to be implemented')
      else

         call get_uplus(LeftState_VX( :,i+1,j,k), uPlusLeft1_DD(:,Dim1_) )
         call get_uplus(LeftState_VX( :,i,  j,k), uPlusLeft_DD(:,Dim1_)  )
         call get_uplus(RightState_VX(:,i+1,j,k), uPlusRight1_DD(:,Dim1_))
         call get_uplus(RightState_VX(:,i,  j,k), uPlusRight_DD(:,Dim1_) )
         if(nJ > 1)then
            call get_uplus(LeftState_VY( :,i,j+1,k), uPlusLeft1_DD(:,Dim2_) )
            call get_uplus(LeftState_VY( :,i,j,  k), uPlusLeft_DD(:,Dim2_)  )
            call get_uplus(RightState_VY(:,i,j+1,k), uPlusRight1_DD(:,Dim2_))
            call get_uplus(RightState_VY(:,i,j,  k), uPlusRight_DD(:,Dim2_) )
         end if
         if(nK > 1)then
            call get_uplus(LeftState_VZ( :,i,j,k+1), uPlusLeft1_DD(:,Dim3_) )
            call get_uplus(LeftState_VZ( :,i,j,k), uPlusLeft_DD(:,Dim3_)  )
            call get_uplus(RightState_VZ(:,i,j,k+1), uPlusRight1_DD(:,Dim3_))
            call get_uplus(RightState_VZ(:,i,j,k), uPlusRight_DD(:,Dim3_)   )
         end if

         do iDir = 1, MaxDim
            GradU_DD(:,iDir) = &
                 0.5*(uPlusLeft1_DD(iDir,Dim1_)+uPlusRight1_DD(iDir,Dim1_))* &
                 FaceNormal_DDFB(:,Dim1_,i+1,j,k,iBlock) &
                 - 0.5*(uPlusLeft_DD(iDir,Dim1_)+uPlusRight_DD(iDir,Dim1_))* &
                 FaceNormal_DDFB(:,Dim1_,i,j,k,iBlock)

            if(nJ == 1) CYCLE

            GradU_DD(:,iDir) = GradU_DD(:,iDir) + &
                 0.5*(uPlusLeft1_DD(iDir,Dim2_)+uPlusRight1_DD(iDir,Dim2_))* &
                 FaceNormal_DDFB(:,Dim2_,i,j+1,k,iBlock) &
                 - 0.5*(uPlusLeft_DD(iDir,Dim2_)+uPlusRight_DD(iDir,Dim2_))* &
                 FaceNormal_DDFB(:,Dim2_,i,j,k,iBlock)

            if(nK == 1) CYCLE

            GradU_DD(:,iDir) = GradU_DD(:,iDir) + &
                 0.5*(uPlusLeft1_DD(iDir,Dim3_)+uPlusRight1_DD(iDir,Dim3_))* &
                 FaceNormal_DDFB(:,Dim3_,i,j,k+1,iBlock) &
                 - 0.5*(uPlusLeft_DD(iDir,Dim3_)+uPlusRight_DD(iDir,Dim3_))* &
                 FaceNormal_DDFB(:,Dim3_,i,j,k,iBlock)
         end do

         GradU_DD = GradU_DD / CellVolume_GB(i,j,k,iBlock)

      end if

    end subroutine calc_grad_uplus
    !==========================================================================
    subroutine get_uplus(StateIn_V, uPlus_D)

      ! This subroutine gets the uPlus_D at the corresponding face
      ! using the face state values StateIn_V

      use ModMultiFluid, ONLY: ChargeIon_I, MassIon_I, nIonFluid

      real,    intent(in)  :: StateIn_V(nVar)
      real,    intent(out) :: uPlus_D(3)

      real :: ChargeDens_I(nIonFluid)
      !------------------------------------------------------------------------
#ifndef SCALAR
      ChargeDens_I    = ChargeIon_I*StateIn_V(iRhoIon_I)/MassIon_I
      InvElectronDens = 1.0/sum(ChargeDens_I)

      uPlus_D(x_) = InvElectronDens*sum( ChargeDens_I*StateIn_V(iUxIon_I) )
      uPlus_D(y_) = InvElectronDens*sum( ChargeDens_I*StateIn_V(iUyIon_I) )
      uPlus_D(z_) = InvElectronDens*sum( ChargeDens_I*StateIn_V(iUzIon_I) )

      if (DoTestCell) write(*,*) 'uPlus_D =', uPlus_D
#endif
    end subroutine get_uplus
    !==========================================================================
    subroutine calc_divb_source(iBlock)

      integer, intent(in):: iBlock

      integer::  i, j, k

      ! Variables needed for div B source terms
      real:: DxInvHalf, DyInvHalf, DzInvHalf, DivBInternal_C(1:nI,1:nJ,1:nK)
      real:: dB1nFace1, dB1nFace2, dB1nFace3, dB1nFace4, dB1nFace5, dB1nFace6
      real, dimension(MaxDim) :: B1Face1_D, B1Face2_D, B1Face3_D, B1Face4_D,&
           B1Face5_D, B1Face6_D
      real:: BCorrect0, BCorrect1
      !------------------------------------------------------------------------
      DxInvHalf = 0.5/CellSize_DB(x_,iBlock)
      DyInvHalf = 0.5/CellSize_DB(y_,iBlock)
      DzInvHalf = 0.5/CellSize_DB(z_,iBlock)

      do k = 1, nK; do j = 1, nJ; do i = 1, nI
         if(.not.Used_GB(i,j,k,iBlock)) CYCLE

         if(UseMhdMomentumFlux.and.UseB0 .or. .not.DoCorrectFace) then

            dB1nFace1 = DxInvHalf*&
                 (RightState_VX(Bx_,i,j,k) - LeftState_VX(Bx_,i,j,k))
            B1Face1_D = RightState_VX(Bx_:Bz_,i,j,k)
            dB1nFace2 = DxInvHalf*&
                 (RightState_VX(Bx_,i+1,j,k) - LeftState_VX(Bx_,i+1,j,k))
            B1Face2_D = LeftState_VX(Bx_:Bz_,i+1,j,k)
            if(nJ > 1)then
               dB1nFace3 = DyInvHalf* &
                    (RightState_VY(By_,i,j,k) - LeftState_VY(By_,i,j,k))
               B1Face3_D = RightState_VY(Bx_:Bz_,i,j,k)
               dB1nFace4 = DyInvHalf* &
                    (RightState_VY(By_,i,j+1,k) - LeftState_VY(By_,i,j+1,k))
               B1Face4_D = LeftState_VY(Bx_:Bz_,i,j+1,k)
            end if

            if(nK > 1)then
               dB1nFace5 = DzInvHalf * &
                    (RightState_VZ(Bz_,i,j,k) - LeftState_VZ(Bz_,i,j,k))
               B1Face5_D = RightState_VZ(Bx_:Bz_,i,j,k)
               dB1nFace6 = DzInvHalf * &
                    (RightState_VZ(Bz_,i,j,k+1) - LeftState_VZ(Bz_,i,j,k+1))
               B1Face6_D = LeftState_VZ(Bx_:Bz_,i,j,k+1)
            end if

            DivBInternal_C(i,j,k) = &
                 2*DxInvHalf*(LeftState_VX(Bx_,i+1,j,k) -&
                 RightState_VX(Bx_,i,j,k))

            if(nJ > 1) DivBInternal_C(i,j,k) = DivBInternal_C(i,j,k) + &
                 2*DyInvHalf*(LeftState_VY(By_,i,j+1,k) -&
                 RightState_VY(By_,i,j,k))

            if(nK > 1) DivBInternal_C(i,j,k) = DivBInternal_C(i,j,k) + &
                 2*DzInvHalf*(LeftState_VZ(Bz_,i,j,k+1) -&
                 RightState_VZ(Bz_,i,j,k))

            ! Momentum source term from B0 only needed for div(B^2/2 - BB)
            ! discretization
            if(UseMhdMomentumFlux.and.UseB0) then
               if(iTypeUpdate == UpdateSlow_) then
                  ! Simple approach with no B0 face arrays
                  SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                       SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) &
                       -B0_DGB(:,i,j,k,iBlock)*dB1nFace1    &
                       -B0_DGB(:,i,j,k,iBlock)*dB1nFace2

                  if(nJ > 1) &
                       SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                       SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) &
                       -B0_DGB(:,i,j,k,iBlock)*dB1nFace3   &
                       -B0_DGB(:,i,j,k,iBlock)*dB1nFace4

                  if(nK > 1) &
                       SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                       SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) &
                       -B0_DGB(:,i,j,k,iBlock)*dB1nFace5     &
                       -B0_DGB(:,i,j,k,iBlock)*dB1nFace6
               elseif(UseDivFullBSource)then
                  ! The face surface magnetic charge is multiplied by
                  ! the full face field. Accordingly, -B1 div B1 source is
                  ! not added later if UseDivFullBSource=.true.
                  SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                       SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) &
                       -(B0_DX(:,i,j,k) + B1Face1_D)*dB1nFace1    &
                       -(B0_DX(:,i+1,j,k) + B1Face2_D)*dB1nFace2

                  if(nJ > 1) &
                       SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                       SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) &
                       -(B0_DY(:,i,j,k) + B1Face3_D)*dB1nFace3   &
                       -(B0_DY(:,i,j+1,k) + B1Face4_D)*dB1nFace4

                  if(nK > 1) &
                       SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                       SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) &
                       -(B0_DZ(:,i,j,k) + B1Face5_D)*dB1nFace5     &
                       -(B0_DZ(:,i,j,k+1) + B1Face6_D)*dB1nFace6
               else
                  SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                       SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) &
                       -B0_DX(:,i,j,k)*dB1nFace1    &
                       -B0_DX(:,i+1,j,k)*dB1nFace2

                  if(nJ > 1) &
                       SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                       SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) &
                       -B0_DY(:,i,j,k)*dB1nFace3   &
                       -B0_DY(:,i,j+1,k)*dB1nFace4

                  if(nK > 1) &
                       SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                       SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) &
                       -B0_DZ(:,i,j,k)*dB1nFace5     &
                       -B0_DZ(:,i,j,k+1)*dB1nFace6
               endif
            endif
         endif

         if(DoCorrectFace) then
            ! Correct the face value so that the first order derivate is
            ! high-order accurate.
            BCorrect0 = correct_face_value( &
                 0.5*(RightState_VX(Bx_,i,j,k) + LeftState_VX(Bx_,i,j,k)), &
                 State_VGB(Bx_,i-2:i+1,j,k,iBlock))

            BCorrect1 = correct_face_value( &
                 0.5*(LeftState_VX(Bx_,i+1,j,k) &
                 +    RightState_VX(Bx_,i+1,j,k)),&
                 State_VGB(Bx_,i-1:i+2,j,k,iBlock))

            DivB1_GB(i,j,k,iBlock) = 2*DxInvHalf*(BCorrect1 - BCorrect0)

            if(nJ>1) then
               BCorrect0 = correct_face_value( &
                    0.5*(RightState_VY(By_,i,j,k) + LeftState_VY(By_,i,j,k)), &
                    State_VGB(By_,i,j-2:j+1,k,iBlock))

               BCorrect1 = correct_face_value( &
                    0.5*(LeftState_VY(By_,i,j+1,k) &
                    +   RightState_VY(By_,i,j+1,k)),&
                    State_VGB(By_,i,j-1:j+2,k,iBlock))

               DivB1_GB(i,j,k,iBlock) = DivB1_GB(i,j,k,iBlock) + &
                    2*DyInvHalf*(BCorrect1 - BCorrect0)
            endif

            if(nK>1) then
               BCorrect0 = correct_face_value( &
                    0.5*(RightState_VZ(Bz_,i,j,k) + LeftState_VZ(Bz_,i,j,k)), &
                    State_VGB(Bz_,i,j,k-2:k+1,iBlock))

               BCorrect1 = correct_face_value( &
                    0.5*(LeftState_VZ(Bz_,i,j,k+1) &
                    +   RightState_VZ(Bz_,i,j,k+1)),&
                    State_VGB(Bz_,i,j,k-1:k+2,iBlock))

               DivB1_GB(i,j,k,iBlock) = DivB1_GB(i,j,k,iBlock) + &
                    2*DzInvHalf*(BCorrect1 - BCorrect0)
            endif
         else
            DivB1_GB(i,j,k,iBlock)  = DivBInternal_C(i,j,k) &
                 + dB1nFace1 + dB1nFace2

            if(nJ > 1) DivB1_GB(i,j,k,iBlock) = DivB1_GB(i,j,k,iBlock) &
                 + dB1nFace3 + dB1nFace4

            if(nK > 1) DivB1_GB(i,j,k,iBlock) = DivB1_GB(i,j,k,iBlock) &
                 + dB1nFace5 + dB1nFace6
         endif

      end do; end do; end do

      ! Momentum source term from B0 only needed for true MHD equations
      if(.not.(UseMhdMomentumFlux .and. UseB0)) RETURN
      if(UseDivFullBSource)then
         ! The magnetic charge in the cell is multiplied by
         ! the full cell-centered field. Accordingly, -B1 div B1 source is
         ! not added later if UseDivFullBSource=.true.
         do k = 1, nK; do j = 1, nJ; do i = 1, nI
            if(.not.Used_GB(i,j,k,iBlock)) CYCLE
            SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                 SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) &
                 - DivBInternal_C(i,j,k)*(B0_DGB(:,i,j,k,iBlock) + &
                 State_VGB(Bx_:Bz_,i,j,k,iBlock))
         end do; end do; end do
      else
         do k = 1, nK; do j = 1, nJ; do i = 1, nI
            if(.not.Used_GB(i,j,k,iBlock)) CYCLE
            SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                 SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) &
                 - DivBInternal_C(i,j,k)*B0_DGB(:,i,j,k,iBlock)
         end do; end do; end do
      end if

    end subroutine calc_divb_source
    !==========================================================================
    subroutine calc_divb_source_gencoord

      use BATL_lib, ONLY: FaceNormal_DDFB

      real :: FaceArea_D(nDim), vInvHalf
      real :: B1nJumpL, B1nJumpR, DivBInternal_C(1:nI,1:nJ,1:nK)
      integer :: i, j, k

      character(len=*), parameter:: NameSub = 'calc_divb_source_gencoord'
      !------------------------------------------------------------------------
      do k = 1, nK; do j = 1, nJ; do i = 1, nI
         if(.not.Used_GB(i,j,k,iBlock)) CYCLE

         VInvHalf = 0.5/CellVolume_GB(i,j,k,iBlock)
         FaceArea_D = FaceNormal_DDFB(:,1,i,j,k,iBlock)
         B1nJumpL =VInvHalf*&
              sum(FaceArea_D*(RightState_VX(Bx_:B_+nDim,i,j,k) &
              -               LeftState_VX(Bx_:B_+nDim,i,j,k)))
         DivBInternal_C(i,j,k) = &
              -sum(FaceArea_D*RightState_VX(Bx_:B_+nDim,i,j,k))

         FaceArea_D = FaceNormal_DDFB(:,1,i+1,j,k,iBlock)
         B1nJumpR =  VInvHalf*&
              sum(FaceArea_D*(RightState_VX(Bx_:B_+nDim,i+1,j,k) &
              -               LeftState_VX(Bx_:B_+nDim,i+1,j,k)))

         DivBInternal_C(i,j,k) = DivBInternal_C(i,j,k) &
              + sum(FaceArea_D*LeftState_VX(Bx_:B_+nDim,i+1,j,k))

         DivB1_GB(i,j,k,iBlock)  = B1nJumpL + B1nJumpR

         if(.not.(UseMhdMomentumFlux .and. UseB0)) CYCLE

         if(iTypeUpdate == UpdateSlow_) then
            SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                 SourceMhd_VC(RhoUx_:RhoUz_,i,j,k)&
                 -B0_DGB(:,i,j,k,iBlock)*B1nJumpL &
                 -B0_DGB(:,i,j,k,iBlock)*B1nJumpR
         else if(UseDivFullBSource)then
            ! The face surface magnetic charge is multiplied by
            ! the full face field. Accordingly, -B1 div B1 source is
            ! not added later if UseDivFullBSource=.true.
            SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) =    &
                 SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) &
                 - (B0_DX(:,i,j,k) + RightState_VX(Bx_:Bz_,i,j,k))*B1nJumpL &
                 - (B0_DX(:,i+1,j,k) + LeftState_VX(Bx_:Bz_,i+1,j,k))*B1nJumpR
         else
            SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                 SourceMhd_VC(RhoUx_:RhoUz_,i,j,k)&
                 - B0_DX(:,i,j,k)*B1nJumpL   &
                 - B0_DX(:,i+1,j,k)*B1nJumpR
         end if
      end do; end do; end do

      if(DoTest)write(*,*)NameSub,' after i divbint, divb1=', &
           DivBInternal_C(iTest,jTest,kTest), &
           DivB1_GB(iTest,jTest,kTest,iBlockTest)

      do k = 1, nK; do j = 1, nJ; do i = 1, nI
         if(.not.Used_GB(i,j,k,iBlock)) CYCLE

         VInvHalf = 0.5/CellVolume_GB(i,j,k,iBlock)
         FaceArea_D = FaceNormal_DDFB(:,2,i,j,k,iBlock)
         B1nJumpL = VInvHalf*&
              sum(FaceArea_D*(RightState_VY(Bx_:B_+nDim,i,j,k) &
              -               LeftState_VY(Bx_:B_+nDim,i,j,k)))
         DivBInternal_C(i,j,k) = DivBInternal_C(i,j,k) &
              - sum(FaceArea_D*RightState_VY(Bx_:B_+nDim,i,j,k))

         FaceArea_D =  FaceNormal_DDFB(:,2,i,j+1,k,iBlock)
         B1nJumpR = VInvHalf*&
              sum(FaceArea_D*(RightState_VY(Bx_:B_+nDim,i,j+1,k) &
              -               LeftState_VY(Bx_:B_+nDim,i,j+1,k)))

         DivBInternal_C(i,j,k) = DivBInternal_C(i,j,k) &
              + sum(FaceArea_D*LeftState_VY(Bx_:B_+nDim,i,j+1,k))

         DivB1_GB(i,j,k,iBlock)  = DivB1_GB(i,j,k,iBlock) &
              + B1nJumpL + B1nJumpR

         if(.not.(UseMhdMomentumFlux .and. UseB0)) CYCLE

         if(iTypeUpdate == UpdateSlow_) then
            SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                 SourceMhd_VC(RhoUx_:RhoUz_,i,j,k)&
                 -B0_DGB(:,i,j,k,iBlock)*B1nJumpL &
                 -B0_DGB(:,i,j,k,iBlock)*B1nJumpR
         else if(UseDivFullBSource)then
            ! The face surface magnetic charge is multiplied by
            ! the full face field. Accordingly, -B1 div B1 source is
            ! not added later if UseDivFullBSource=.true.
            SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                 SourceMhd_VC(RhoUx_:RhoUz_,i,j,k)&
                 -(B0_DY(:,i,j,k) + RightState_VY(Bx_:Bz_,i,j,k))*B1nJumpL &
                 -(B0_DY(:,i,j+1,k) + LeftState_VY(Bx_:Bz_,i,j+1,k))*B1nJumpR
         else
            SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                 SourceMhd_VC(RhoUx_:RhoUz_,i,j,k)&
                 -B0_DY(:,i,j,k)*B1nJumpL &
                 -B0_DY(:,i,j+1,k)*B1nJumpR
         end if
      end do; end do; end do

      if(DoTest)write(*,*)NameSub,' after j divbint, divb1=', &
           DivBInternal_C(iTest,jTest,kTest), &
           DivB1_GB(iTest,jTest,kTest,iBlockTest)

      if(nK > 1)then
         do k = 1, nK; do j = 1, nJ; do i = 1, nI
            if(.not.Used_GB(i,j,k,iBlock)) CYCLE

            VInvHalf = 0.5/CellVolume_GB(i,j,k,iBlock)
            FaceArea_D = FaceNormal_DDFB(:,3,i,j,k,iBlock)
            B1nJumpL = VInvHalf*&
                 sum(FaceArea_D*(RightState_VZ(Bx_:B_+nDim,i,j,k) &
                 -                LeftState_VZ(Bx_:B_+nDim,i,j,k)))

            DivBInternal_C(i,j,k) = DivBInternal_C(i,j,k) &
                 - sum(FaceArea_D*RightState_VZ(Bx_:B_+nDim,i,j,k))

            FaceArea_D = FaceNormal_DDFB(:,3,i,j,k+1,iBlock)
            B1nJumpR = VInvHalf*&
                 sum(FaceArea_D*(RightState_VZ(Bx_:B_+nDim,i,j,k+1) &
                 -               LeftState_VZ(Bx_:B_+nDim,i,j,k+1)))

            DivBInternal_C(i,j,k) = DivBInternal_C(i,j,k) + &
                 sum(FaceArea_D*LeftState_VZ(Bx_:B_+nDim,i,j,k+1))

            DivB1_GB(i,j,k,iBlock)  = DivB1_GB(i,j,k,iBlock) &
                 + B1nJumpL + B1nJumpR

            if(.not.(UseMhdMomentumFlux .and. UseB0)) CYCLE

         if(iTypeUpdate == UpdateSlow_) then
            SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                 SourceMhd_VC(RhoUx_:RhoUz_,i,j,k)&
                 -B0_DGB(:,i,j,k,iBlock)*B1nJumpL &
                 -B0_DGB(:,i,j,k,iBlock)*B1nJumpR
         else if(UseDivFullBSource)then
               ! The face surface magnetic charge is multiplied by
               ! the full face field. Accordingly, -B1 div B1 source is
               ! not added later if UseDivFullBSource=.true.
               SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                    SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) &
                    -(B0_DZ(:,i,j,k) + RightState_VZ(Bx_:Bz_,i,j,k))*B1nJumpL &
                    -(B0_DZ(:,i,j,k+1)+ LeftState_VZ(Bx_:Bz_,i,j,k+1))*B1nJumpR
            else
               SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                    SourceMhd_VC(RhoUx_:RhoUz_,i,j,k)&
                    -B0_DZ(:,i,j,k)*B1nJumpL &
                    -B0_DZ(:,i,j,k+1)*B1nJumpR
            end if
         end do; end do; end do
      end if

      if(DoTest)write(*,*)NameSub,' after k divbint, divb1=', &
           DivBInternal_C(iTest,jTest,kTest), &
           DivB1_GB(iTest,jTest,kTest,iBlockTest)

      do k = 1, nK; do j = 1, nJ; do i = 1, nI
         if(.not.Used_GB(i,j,k,iBlock)) CYCLE
         DivBInternal_C(i,j,k) = DivBInternal_C(i,j,k) &
              /CellVolume_GB(i,j,k,iBlock)
         DivB1_GB(i,j,k,iBlock) = DivB1_GB(i,j,k,iBlock) &
              + DivBInternal_C(i,j,k)
      end do; end do; end do

      if(DoTest)write(*,*)NameSub,' final divb1=', &
           DivB1_GB(iTest,jTest,kTest,iBlockTest)

      if(.not.(UseMhdMomentumFlux .and. UseB0)) RETURN
      if(UseDivFullBSource)then
         ! The magnetic charge in the cell is multiplied by
         ! the full cell-centered field. Accordingly, -B1 div B1 source is
         ! not added later if UseDivFullBSource=.true.
         do k = 1, nK; do j = 1, nJ; do i = 1, nI
            if(.not.Used_GB(i,j,k,iBlock)) CYCLE
            SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                 SourceMhd_VC(RhoUx_:RhoUz_,i,j,k)&
                 - DivBInternal_C(i,j,k)*(B0_DGB(:,i,j,k,iBlock) + &
                 State_VGB(Bx_:Bz_,i,j,k,iBlock))
         end do; end do; end do
      else
         do k = 1, nK; do j = 1, nJ; do i = 1, nI
            if(.not.Used_GB(i,j,k,iBlock)) CYCLE
            SourceMhd_VC(RhoUx_:RhoUz_,i,j,k) = &
                 SourceMhd_VC(RhoUx_:RhoUz_,i,j,k)&
                 - DivBInternal_C(i,j,k)*B0_DGB(:,i,j,k,iBlock)
         end do; end do; end do
      end if
    end subroutine calc_divb_source_gencoord
    !==========================================================================
    subroutine calc_friction(iBlock)

      ! Calculate friction force due to some background fluid
      integer, intent(in):: iBlock

      real:: Rho, u_D(3), Force_D(3)
      integer:: i, j, k
      !------------------------------------------------------------------------
      if(Friction == 0.0)then
         ! Calculate normalized quantities
         Friction    = FrictionSi     / Si2No_V(UnitT_)
         FrictionU_D = FrictionUDim_D * Io2No_V(UnitU_)
      end if
      do k = 1, nK; do j = 1, nJ; do i = 1, nI
         if(.not.Used_GB(i,j,k,iBlock)) CYCLE
         Rho = State_VGB(Rho_,i,j,k,iBlock)
         u_D = State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)/Rho
         Force_D = Friction*(FrictionU_D - u_D)*Rho
         Source_VC(RhoUx_:RhoUz_,i,j,k) = Source_VC(RhoUx_:RhoUz_,i,j,k) &
              + Force_D
         Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) + sum(Force_D*u_D)
      end do; end do; end do

    end subroutine calc_friction
    !==========================================================================
    subroutine write_source(String)
      character(len=*), intent(in) :: String
      !------------------------------------------------------------------------
      write(*,'(a,es13.5)') NameSub//": "//String//" S(iVarTest)=",&
           Source_VC(iVarTest,iTest,jTest,kTest)
    end subroutine write_source
    !==========================================================================
  end subroutine calc_source
  !============================================================================
  subroutine calc_divb(iBlock)

    ! Calculate div B for a block and store result into DivB1_GB
    ! Compute divB using averaged and conservatively corrected
    ! left and right values

    use BATL_lib,      ONLY: CellSize_DB, x_, y_, z_
    use ModMain,       ONLY: nI, nJ, nK
    use ModVarIndexes, ONLY: Bx_, By_, Bz_
    use ModAdvance,    ONLY: DivB1_GB, &
         LeftState_VX, RightState_VX, &
         LeftState_VY, RightState_VY, &
         LeftState_VZ, RightState_VZ

    integer, intent(in) :: iBlock

    integer:: i, j, k
    real   :: DivB, InvDx, InvDy, InvDz
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_divb'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    InvDx            = 1/CellSize_DB(x_,iBlock)
    if(nJ > 1) InvDy = 1/CellSize_DB(y_,iBlock)
    if(nK > 1) InvDz = 1/CellSize_DB(z_,iBlock)

    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       DivB = InvDx* &
            (  LeftState_VX(Bx_,i+1,j,k)  &
            + RightState_VX(Bx_,i+1,j,k)  &
            -  LeftState_VX(Bx_,i,j,k)    &
            - RightState_VX(Bx_,i,j,k) )

       if(nJ > 1) DivB = DivB + InvDy* &
            (  LeftState_VY(By_,i,j+1,k)   &
            + RightState_VY(By_,i,j+1,k)   &
            -  LeftState_VY(By_,i,j,k)     &
            - RightState_VY(By_,i,j,k) )

       if(nK > 1) DivB = DivB + InvDz* &
            (  LeftState_VZ(Bz_,i,j,k+1)    &
            + RightState_VZ(Bz_,i,j,k+1)    &
            -  LeftState_VZ(Bz_,i,j,k)      &
            - RightState_VZ(Bz_,i,j,k) )

       DivB1_GB(i,j,k,iBlock) = 0.5*DivB

    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine calc_divb
  !============================================================================
end module ModCalcSource
!==============================================================================
