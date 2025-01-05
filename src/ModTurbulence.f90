!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModTurbulence

  use BATL_lib, ONLY: test_start, test_stop, MaxDim
  use BATL_size, ONLY: nGang
  use ModBatsrusUtility, ONLY: stop_mpi
#ifdef _OPENACC
  use ModUtilities, ONLY: norm2
#endif
  use ModMain,       ONLY: nI, nJ, nK
  use ModReadParam,  ONLY: lStringLine
  use ModVarIndexes, ONLY: WaveFirst_, WaveLast_, WDiff_, Lperp_
  use ModMultiFluid, ONLY: nIonFluid
  use ModTransitionRegion, ONLY: PoyntingFluxPerBSi, LperpTimesSqrtBSi
  use ModWaves,      ONLY: UseAlfvenWaves, UseWavePressure, &
       UseAwRepresentative
  use ModUtilities,  ONLY: i_gang
  use omp_lib

  implicit none
  SAVE

  logical :: IsOnAwRepresentative = .false.
  !$acc declare create(IsOnAwRepresentative)

  logical :: UseTurbulentCascade = .false.
  !$acc declare create(UseTurbulentCascade)
  real    :: rMinWaveReflection = 0.0
  logical :: UseNewLimiter4Reflection

  ! The Poynting flux to magnetic field ratio (one of the input parameters
  ! dimensionless):
  real :: PoyntingFluxPerB
  real :: ImbalanceMax = 2.0, ImbalanceMax2 = 4.0

  ! Alfven wave dissipation
  logical :: UseAlfvenWaveDissipation = .false.
  real    :: LperpTimesSqrtB
  real :: Crefl = 0.04
  !$acc declare create(UseAlfvenWaveDissipation, LperpTimesSqrtB, Crefl)

  ! Arrays for the calculated heat function and dissipated wave energy
  real, public, allocatable :: CoronalHeating_CI(:,:,:,:)
  real, public, allocatable :: WaveDissipationRate_VCI(:,:,:,:,:)
  !$omp threadprivate( CoronalHeating_CI, WaveDissipationRate_VCI )
  !$acc declare create( CoronalHeating_CI, WaveDissipationRate_VCI )

  ! Alfven wave speed array, cell-centered
  real, public, allocatable :: AlfvenWaveVel_DC(:,:,:,:)
  !$omp threadprivate(AlfvenWaveVel_DC)

  character(len=lStringLine) :: TypeHeatPartitioning
  ! Use a lookup table for linear Landau and transit-time damping of KAWs
  integer :: iTableHeatPartition = -1

  ! Switch whether to use uniform heat partition
  logical :: UseUniformHeatPartition = .false.
  real :: QionRatio_I(nIonFluid) = 0.6
  real :: QionParRatio_I(nIonFluid) = 0.0
  real :: QeRatio = 0.4
  !$acc declare create(UseUniformHeatPartition)
  !$acc declare create(QionRatio_I, QionParRatio_I, QeRatio)

  ! Dimensionless parameters for stochastic heating
  logical :: UseStochasticHeating = .true.
  real :: StochasticExponent   = 0.21
  real :: StochasticAmplitude  = 0.18
  real :: StochasticExponent2  = 0.21
  real :: StochasticAmplitude2 = 0.0 ! 1.17
  !$acc declare create(UseStochasticHeating)
  !$acc declare create(StochasticExponent, StochasticAmplitude)
  !$acc declare create(StochasticExponent2, StochasticAmplitude2)

  logical :: UseNonlinearAwDissipation = .false.

  ! Switch whether or not to use Alignment angle between Zplus and Zminus
  ! Elsasser variables in the cascade rate
  logical :: UseAlignmentAngle = .false.
  real    :: Cdiss_C(1:nI,1:nJ,1:nk)
  !$omp threadprivate(Cdiss_C)

  ! The normalized energy difference:
  ! SigmaD = (kinetic - magnetic)/(kinetic + magnetic)
  logical :: UseReynoldsDecomposition = .false.
  !$acc declare create(UseReynoldsDecomposition)
  logical :: UseTransverseTurbulence = .true.
  real    :: SigmaD = -1.0/3.0
  real    :: KarmanTaylorAlpha = 1.0
  real    :: KarmanTaylorBeta2AlphaRatio = 0.5
  logical, private:: DoInit = .true.
contains
  !============================================================================
  subroutine read_turbulence_param(NameCommand)

    use ModAdvance,    ONLY: UseAnisoPressure, iTypeUpdate, UpdateOrig_
    use ModReadParam,  ONLY: read_var

    integer :: iFluid

    character(len=*), intent(in):: NameCommand
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_turbulence_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(NameCommand)
       ! Types of coronal heating
    case('alfvenwavedissipation')
       UseAlfvenWaves  = WaveFirst_ > 1
       UseWavePressure = WaveFirst_ > 1
       UseAlfvenWaveDissipation = .true.
       DoInit = .true.
       call read_var('LperpTimesSqrtBSi', LperpTimesSqrtBSi)
       call read_var('Crefl', Crefl)

       ! To do: UseWavePressure does not work with fast update yet.
       if(iTypeUpdate /= UpdateOrig_) UseWavePressure = .false.
    case('turbulentcascade')
       UseAlfvenWaves  = WaveFirst_ > 1
       UseWavePressure = WaveFirst_ > 1
       UseAlfvenWaveDissipation = .true.
       UseTurbulentCascade = .true.
       DoInit = .true.
       call read_var('LperpTimesSqrtBSi', LperpTimesSqrtBSi)
       call read_var('rMinWaveReflection', rMinWaveReflection)
       if(WDiff_>1)then
          call read_var(&
               'UseReynoldsDecomposition', UseReynoldsDecomposition)
       else
          call read_var(&
               'UseReynoldsDecomposition', UseNewLimiter4Reflection)
       end if
       !$acc update device(UseTurbulentCascade)
    case('usmanov')
       UseAlfvenWaves  = WaveFirst_ > 1
       UseWavePressure = WaveFirst_ > 1
       UseAlfvenWaveDissipation = .true.
       UseReynoldsDecomposition = .true.

       call read_var('UseTransverseTurbulence', UseTransverseTurbulence)
       call read_var('SigmaD', SigmaD)
       ! "historically" our  Lperp = Usmanov's \Lambda/KarmanTaylorAlpha
       ! Therefore, we solve an equation for Lperp introduced in this way,
       ! so that KarmanTaylorAlpha is not present in any equation ...
       call read_var('KarmanTaylorAlpha', KarmanTaylorAlpha)
       ! KarmanTaylorBeta is present in non-linear term in the evolution
       ! equation for Lperp via its ratio to KarmanTaylorAlpha ...
       call read_var('KarmanTaylorBeta2AlphaRatio',KarmanTaylorBeta2AlphaRatio)
    case('#LIMITIMBALANCE')
       call read_var('ImbalanceMax',ImbalanceMax)
       ImbalanceMax2 = ImbalanceMax**2
    case("#POYNTINGFLUX")
       DoInit = .true.
       call read_var('PoyntingFluxPerBSi', PoyntingFluxPerBSi)
    case("#HEATPARTITIONING")
       UseUniformHeatPartition = .false.
       UseStochasticHeating = .false.
       call read_var('TypeHeatPartitioning', TypeHeatPartitioning)
       select case(TypeHeatPartitioning)
       case('uniform')
          UseUniformHeatPartition = .true.
          do iFluid = 1, nIonFluid
             call read_var('QionRatio', QionRatio_I(iFluid))
          end do
          if(UseAnisoPressure)then
             do iFluid = 1, nIonFluid
                call read_var('QionParRatio', QionParRatio_I(iFluid))
             end do
          end if
          QeRatio = 1.0 - sum(QionRatio_I)
          !$acc update device(UseUniformHeatPartition)
          !$acc update device(QionRatio_I, QionParRatio_I, QeRatio)
       case('stochasticheating')
          UseStochasticHeating = .true.
          ! Stochastic heating when Beta_proton is below 1
          call read_var('StochasticExponent', StochasticExponent)
          call read_var('StochasticAmplitude', StochasticAmplitude)
       case default
          call stop_mpi(NameSub//': unknown TypeHeatPartitioning = '&
               // TypeHeatPartitioning)
       end select

    case("#HIGHBETASTOCHASTIC")
       ! Correction for stochastic heating when Beta_proton is between 1 and 30
       ! KAWs are non-propagating for Beta_proton > 30.
       call read_var('StochasticExponent2', StochasticExponent2)
       call read_var('StochasticAmplitude2', StochasticAmplitude2)

    case("#ALIGNMENTANGLE")
       call read_var('UseAlignmentAngle', UseAlignmentAngle)
    case("#NONLINAWDISSIPATION")
       call read_var('UseNonLinearAWDissipation',UseNonLinearAWDissipation)
    case default
       call stop_mpi(NameSub//': unknown command = ' &
            // NameCommand)
    end select

    !$acc update device(UseAlfvenWaveDissipation, LperpTimesSqrtB, Crefl)

    !$acc update device(UseStochasticHeating)
    !$acc update device(StochasticExponent, StochasticAmplitude)
    !$acc update device(StochasticExponent2, StochasticAmplitude2)

    !$acc update device(UseReynoldsDecomposition)

    call test_stop(NameSub, DoTest)
  end subroutine read_turbulence_param
  !============================================================================
  subroutine init_turbulence

    use ModPhysics,     ONLY: Si2No_V, UnitB_, UnitX_, UnitU_, UnitEnergyDens_
    use ModMultiFluid,  ONLY: UseMultiIon, nIonFluid
    use ModLookupTable, ONLY: i_lookup_table
    use ModVarIndexes,  ONLY: nChargeStateAll

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_turbulence'
    !--------------------------------------------------------------------------
    if(.not.allocated(CoronalHeating_CI)) &
         allocate(CoronalHeating_CI(1:nI,1:nJ,1:nK,nGang))

    if(.not.allocated(WaveDissipationRate_VCI)) &
         allocate(WaveDissipationRate_VCI(WaveFirst_:WaveLast_, &
         1:nI,1:nJ,1:nK,nGang))

    if(.not.UseAlfvenWaves) RETURN

    if(.not.DoInit)RETURN
    DoInit = .false.

    if(UseAlfvenWaves.and.                   &
         .not.allocated(AlfvenWaveVel_DC))  &
         allocate(AlfvenWaveVel_DC(MaxDim,nI,nJ,nK))

    if(UseAlfvenWaveDissipation)then
       LperpTimesSqrtB = LperpTimesSqrtBSi &
            *Si2No_V(UnitX_)*sqrt(Si2No_V(UnitB_))
    end if

    PoyntingFluxPerB = PoyntingFluxPerBSi &
         *Si2No_V(UnitEnergyDens_)*Si2No_V(UnitU_)/Si2No_V(UnitB_)

    ! if multi-ion, then use lookup table to determine the linear Landau
    ! and transit-time damping of kinetic Alfven waves
    if(UseMultiIon .and. UseStochasticHeating .and. nChargeStateAll==1)then
       iTableHeatPartition = i_lookup_table('heatpartition')
       if(.not. iTableHeatPartition > 0) &
            call stop_mpi('Heat partition table required for multi-ion')
       if(nIonFluid /= 2) &
            call stop_mpi('multi-ion heat partitioning only works for 2 ions')
    end if
  end subroutine init_turbulence
  !============================================================================
  subroutine set_alfven_wave_vel_vect(iBlock)
    use ModAdvance, ONLY: State_VGB
    use ModB0,      ONLY: B0_DGB
    use ModMain, ONLY: UseB0
    use ModVarIndexes, ONLY: Rho_, Bx_, Bz_
    integer, intent(in) :: iBlock
    real :: FullB_D(3)
    integer :: i, j, k
    character(len=*), parameter:: NameSub = 'set_alfven_wave_vel_vect'
    !--------------------------------------------------------------------------
    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if(UseB0)then
          FullB_D = B0_DGB(:,i,j,k,iBlock) + State_VGB(Bx_:Bz_,i,j,k,iBlock)
       else
          FullB_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
       end if
       AlfvenWaveVel_DC(:,i,j,k) = FullB_D/&
            sqrt(State_VGB(Rho_,i,j,k,iBlock))
    end do; end do; end do
  end subroutine set_alfven_wave_vel_vect
  !============================================================================
  subroutine calc_alfven_wave_dissipation(i, j, k, iBlock, &
       WaveDissipationRate_V, CoronalHeating)
    !$acc routine seq
    use ModAdvance, ONLY: State_VGB
    use ModB0,      ONLY: B0_DGB
    use ModMain, ONLY: UseB0
    use ModVarIndexes, ONLY: Rho_, Bx_, Bz_

    integer, intent(in) :: i, j, k, iBlock
    real, intent(out)   :: WaveDissipationRate_V(WaveFirst_:WaveLast_), &
         CoronalHeating

    real :: EwavePlus, EwaveMinus, FullB_D(3), FullB, Coef, SqrtRho
    character(len=*), parameter:: NameSub = 'calc_alfven_wave_dissipation'
    !--------------------------------------------------------------------------
    if(IsOnAwRepresentative)then
#ifndef _OPENACC
       Coef = 2*sqrt(PoyntingFluxPerB*norm2(AlfvenWaveVel_DC(:,i,j,k)))&
            /LperpTimesSqrtB
       SqrtRho = PoyntingFluxPerB*sqrt(State_VGB(Rho_,i,j,k,iBlock))
#endif
    else
       if(UseB0)then
          FullB_D = B0_DGB(:,i,j,k,iBlock) + State_VGB(Bx_:Bz_,i,j,k,iBlock)
       else
          FullB_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
       end if
       FullB = norm2(FullB_D)
       Coef = 2.0*sqrt(FullB/State_VGB(Rho_,i,j,k,iBlock))/LperpTimesSqrtB
       SqrtRho = 1.0
    end if
    EwavePlus  = State_VGB(WaveFirst_,i,j,k,iBlock)
    EwaveMinus = State_VGB(WaveLast_,i,j,k,iBlock)

    WaveDissipationRate_V(WaveFirst_) = Coef* &
         sqrt(max(EwaveMinus,Crefl**2*EwavePlus))

    WaveDissipationRate_V(WaveLast_) = Coef* &
         sqrt(max(EwavePlus,Crefl**2*EwaveMinus))

    CoronalHeating = SqrtRho*sum(&
         WaveDissipationRate_V*State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock))

  end subroutine calc_alfven_wave_dissipation
  !============================================================================
  subroutine turbulent_cascade(i, j, k, iBlock, WaveDissipationRate_V, &
       CoronalHeating)

    use ModAdvance, ONLY: State_VGB
    use ModB0, ONLY: B0_DGB
    use ModMain, ONLY: UseB0
    use ModVarIndexes, ONLY: Bx_, Bz_,Rho_,  Lperp_

    integer, intent(in) :: i, j, k, iBlock
    real, intent(out)   :: CoronalHeating, &
         WaveDissipationRate_V(WaveFirst_:WaveLast_)

    real :: FullB_D(3), FullB, Coef, SqrtRho
    real :: EwavePlus, EwaveMinus

    ! Low-frequency cascade due to small-scale nonlinearities

    character(len=*), parameter:: NameSub = 'turbulent_cascade'
    !--------------------------------------------------------------------------
    if(Lperp_ > 1 .and. .not.UseTurbulentCascade)then
       ! Usmanov's model for Lperp = \Lambda/KarmanTaylorAlpha
       ! Note that Lperp is multiplied with the density
       Coef = sqrt(State_VGB(Rho_,i,j,k,iBlock))*2.0 &
            /State_VGB(Lperp_,i,j,k,iBlock)
       SqrtRho = 1.0
    elseif(IsOnAwRepresentative)then
       Coef = 2*sqrt(PoyntingFluxPerB*norm2(AlfvenWaveVel_DC(:,i,j,k)))
       SqrtRho = PoyntingFluxPerB*sqrt(State_VGB(Rho_,i,j,k,iBlock))
    else
       if(UseB0)then
          FullB_D = B0_DGB(:,i,j,k,iBlock) + State_VGB(Bx_:Bz_,i,j,k,iBlock)
       else
          FullB_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
       end if
       if(UseNonLinearAWDissipation)then
          ! Account for a contribution from the wave field into their
          ! dissipation. A half of wave energy, w/2, is the magneic oscillation
          ! energy, deltaB^2/2. Hence, DeltaB+/-=sqrt(W+/-)
          FullB = sqrt(sum(FullB_D**2) + &
               sum(State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock)) )
       else
          FullB = norm2(FullB_D)
       end if
       Coef = 2.0*sqrt(FullB/State_VGB(Rho_,i,j,k,iBlock))
       if(Lperp_>1)then
          ! The model for SC and IH, Lperp_ state variable is Lperp*sqrt(B)
          Coef = Coef/State_VGB(Lperp_,i,j,k,iBlock)
       else
          ! Lperp*sqrt(B) is constant (Hollweg's model)
          Coef = Coef/LperpTimesSqrtB
       end if
       SqrtRho = 1.0
    end if

    EwavePlus  = State_VGB(WaveFirst_,i,j,k,iBlock)
    EwaveMinus = State_VGB(WaveLast_,i,j,k,iBlock)

    WaveDissipationRate_V(WaveFirst_) = Coef*sqrt(EwaveMinus)
    WaveDissipationRate_V(WaveLast_) = Coef*sqrt(EwavePlus)

    CoronalHeating = SqrtRho*sum(&
         WaveDissipationRate_V*State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock))
  end subroutine turbulent_cascade
  !============================================================================
  subroutine get_wave_reflection(iBlock, IsNewBlock)
    ! Use array WaveDissipationRate_VCI. With these regards
    ! the usual way to call this function is:
    !
    ! if(DoExtendTransitionRegion) &
    ! call get_tesi_c(iBlock, TeSi_CI(:,:,:,iGang))
    ! call get_block_heating(iBlock)
    ! call get_wave_reflection(iBlock, IsNewBlock)
    use BATL_size, ONLY: nDim, nI, nJ, nK
    use ModAdvance, ONLY: State_VGB, Source_VC
    use ModB0, ONLY: B0_DGB
    use ModGeometry, ONLY: Used_GB, r_GB
    use ModMain, ONLY: UseB0
    use ModVarIndexes, ONLY: Rho_, Bx_, Bz_

    integer, intent(in) :: iBlock
    logical, optional, intent(inout):: IsNewBlock

    integer :: i, j, k, iGang
    real :: GradLogAlfven_D(nDim), CurlU_D(3), b_D(3)
    real :: FullB_D(3), FullB, Rho, DissipationRateMax, ReflectionRate,  &
         DissipationRateDiff
    real :: EwavePlus, EwaveMinus
    real :: AlfvenGradRefl, ReflectionRateImb
    logical :: IsNewBlockAlfven

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_wave_reflection'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(present(IsNewBlock)) then
       IsNewBlockAlfven = IsNewBlock
    else
       IsNewBlockAlfven = .true.
    end if

    iGang = i_gang(iBlock)

    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if( (.not.Used_GB(i,j,k,iBlock)).or.&
            r_GB(i,j,k, iBlock) < rMinWaveReflection)CYCLE

       call get_grad_log_alfven_speed(i, j, k, iBlock, IsNewBlockAlfven, &
            GradLogAlfven_D)
       call get_curl_u(i, j, k, iBlock, CurlU_D)

       if(UseB0)then
          FullB_D = B0_DGB(:,i,j,k,iBlock) + State_VGB(Bx_:Bz_,i,j,k,iBlock)
       else
          FullB_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
       end if
       FullB = norm2(FullB_D)
       b_D = FullB_D/max(1e-15, FullB)

       Rho = State_VGB(Rho_,i,j,k,iBlock)

       EwavePlus  = State_VGB(WaveFirst_,i,j,k,iBlock)
       EwaveMinus = State_VGB(WaveLast_,i,j,k,iBlock)

       AlfvenGradRefl = (sum(FullB_D(:nDim)*GradLogAlfven_D))**2/Rho

       ReflectionRateImb = sqrt( (sum(b_D*CurlU_D))**2 + AlfvenGradRefl )
       if(UseNewLimiter4Reflection)then
          DissipationRateDiff =-0.50*(&
               WaveDissipationRate_VCI(WaveFirst_,i,j,k,iGang)&
               - WaveDissipationRate_VCI(WaveLast_,i,j,k,iGang))
          ReflectionRate = sign(min(ReflectionRateImb,&
               abs(DissipationRateDiff)), DissipationRateDiff)
       else
          DissipationRateMax  = maxval(WaveDissipationRate_VCI(:,i,j,k,iGang))
          ! Clip the reflection rate from above with maximum dissipation rate
          ReflectionRate = min(ReflectionRateImb, DissipationRateMax)

          ! No reflection when turbulence is balanced (waves are then
          ! assumed to be uncorrelated)
          if(ImbalanceMax2*EwaveMinus < EwavePlus)then
             ReflectionRate = ReflectionRate*&
                  (1.0 - ImbalanceMax*sqrt(EwaveMinus/EwavePlus))
          elseif(ImbalanceMax2*EwavePlus < EwaveMinus)then
             ReflectionRate = ReflectionRate*&
                  (ImbalanceMax*sqrt(EwavePlus/EwaveMinus)-1.0)
          else
             ReflectionRate = 0.0
          end if
       end if
       Source_VC(WaveFirst_,i,j,k) = Source_VC(WaveFirst_,i,j,k) &
            - ReflectionRate*sqrt(EwavePlus*EwaveMinus)
       Source_VC(WaveLast_,i,j,k) = Source_VC(WaveLast_,i,j,k) &
            + ReflectionRate*sqrt(EwavePlus*EwaveMinus)

       ! Calculate sin(theta), where theta is the angle between Zplus
       ! and Zminus at the outer Lperp scale
       if(UseAlignmentAngle)then
          Cdiss_C(i,j,k) = sqrt(1.0 - AlfvenGradRefl &
               *(ReflectionRate/ReflectionRateImb**2)**2)
          WaveDissipationRate_VCI(:,i,j,k,iGang) = &
               WaveDissipationRate_VCI(:,i,j,k,iGang)*Cdiss_C(i,j,k)
          CoronalHeating_CI(i,j,k,iGang) = &
               CoronalHeating_CI(i,j,k,iGang)*Cdiss_C(i,j,k)
       end if
    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine get_wave_reflection
  !============================================================================
  subroutine get_grad_log_alfven_speed(i, j, k, iBlock, IsNewBlockAlfven, &
       GradLogAlfven_D)

    use BATL_lib, ONLY: IsCartesianGrid, &
         CellSize_DB, FaceNormal_DDFB, CellVolume_GB, &
         x_, y_, z_, Dim1_, Dim2_, Dim3_
    use BATL_size, ONLY: nDim, nI, j0_, nJp1_, k0_, nKp1_

    integer, intent(in) :: i, j, k, iBlock
    logical, intent(inout) :: IsNewBlockAlfven
    real, intent(out) :: GradLogAlfven_D(nDim)

    real, save :: LogAlfven_FD(0:nI+1,j0_:nJp1_,k0_:nKp1_,nDim)
    !$omp threadprivate(LogAlfven_FD)

    character(len=*), parameter:: NameSub = 'get_grad_log_alfven_speed'
    !--------------------------------------------------------------------------
    if(IsNewBlockAlfven)then
       call get_log_alfven_speed
       IsNewBlockAlfven = .false.
    end if

    if(IsCartesianGrid)then
       GradLogAlfven_D(Dim1_) = 1.0/CellSize_DB(x_,iBlock) &
            *(LogAlfven_FD(i+1,j,k,Dim1_) - LogAlfven_FD(i,j,k,Dim1_))
       if(nJ > 1) then
          GradLogAlfven_D(Dim2_) = 1.0/CellSize_DB(y_,iBlock) &
               *(LogAlfven_FD(i,j+1,k,Dim2_) - LogAlfven_FD(i,j,k,Dim2_))
       end if
       if(nK > 1) then
          GradLogAlfven_D(Dim3_) = 1.0/CellSize_DB(z_,iBlock) &
               *(LogAlfven_FD(i,j,k+1,Dim3_) - LogAlfven_FD(i,j,k,Dim3_))
       end if
    else
       GradLogAlfven_D = &
            LogAlfven_FD(i+1,j,k,Dim1_) &
            *FaceNormal_DDFB(:,Dim1_,i+1,j,k,iBlock) &
            - LogAlfven_FD(i,j,k,Dim1_) &
            *FaceNormal_DDFB(:,Dim1_,i,j,k,iBlock)
       if(nJ > 1)then
          GradLogAlfven_D = GradLogAlfven_D + &
               LogAlfven_FD(i,j+1,k,Dim2_) &
               *FaceNormal_DDFB(:,Dim2_,i,j+1,k,iBlock) &
               - LogAlfven_FD(i,j,k,Dim2_) &
               *FaceNormal_DDFB(:,Dim2_,i,j,k,iBlock)
       end if
       if(nK > 1) then
          GradLogAlfven_D = GradLogAlfven_D + &
               LogAlfven_FD(i,j,k+1,Dim3_) &
               *FaceNormal_DDFB(:,Dim3_,i,j,k+1,iBlock) &
               - LogAlfven_FD(i,j,k,Dim3_) &
               *FaceNormal_DDFB(:,Dim3_,i,j,k,iBlock)
       end if

       GradLogAlfven_D = GradLogAlfven_D/CellVolume_GB(i,j,k,iBlock)
    end if

  contains
    !==========================================================================
    subroutine get_log_alfven_speed

      use ModAdvance, ONLY: &
           LeftState_VX, LeftState_VY, LeftState_VZ,  &
           RightState_VX, RightState_VY, RightState_VZ
      use ModB0, ONLY: B0_DX, B0_DY, B0_DZ
      use ModMain, ONLY: UseB0
      use ModVarIndexes, ONLY: Rho_, Bx_, Bz_

      integer :: i, j, k
      real :: Rho, FullB_D(3)
      !------------------------------------------------------------------------
      do k = 1, nK; do j = 1, nJ; do i = 1, nI+1
         FullB_D = 0.5*(LeftState_VX(Bx_:Bz_,i,j,k) &
              + RightState_VX(Bx_:Bz_,i,j,k))
         if(UseB0) FullB_D = FullB_D + B0_DX(:,i,j,k)
         Rho = 0.5*(LeftState_VX(Rho_,i,j,k) &
              +     RightState_VX(Rho_,i,j,k))
         LogAlfven_FD(i,j,k,x_) = 0.50*log(max(sum(FullB_D**2), 1e-30)/Rho)
      end do; end do; end do

      if(nJ > 1)then
         do k = 1, nK; do j = 1, nJ+1; do i = 1, nI
            FullB_D = 0.5*(LeftState_VY(Bx_:Bz_,i,j,k) &
                 + RightState_VY(Bx_:Bz_,i,j,k))
            if(UseB0) FullB_D = FullB_D + B0_DY(:,i,j,k)
            Rho = 0.5*(LeftState_VY(Rho_,i,j,k) &
                 +     RightState_VY(Rho_,i,j,k))
            LogAlfven_FD(i,j,k,Dim2_) = &
                 0.50*log(max(sum(FullB_D**2), 1e-30)/Rho)
         end do; end do; end do
      end if

      if(nK > 1)then
         do k = 1, nK+1; do j = 1, nJ; do i = 1, nI
            FullB_D = 0.5*(LeftState_VZ(Bx_:Bz_,i,j,k) &
                 + RightState_VZ(Bx_:Bz_,i,j,k))
            if(UseB0) FullB_D = FullB_D + B0_DZ(:,i,j,k)
            Rho = 0.5*(LeftState_VZ(Rho_,i,j,k) &
                 +     RightState_VZ(Rho_,i,j,k))
            LogAlfven_FD(i,j,k,Dim3_) = &
                 0.50*log(max(sum(FullB_D**2), 1e-30)/Rho)
         end do; end do; end do
      end if

    end subroutine get_log_alfven_speed
    !==========================================================================
  end subroutine get_grad_log_alfven_speed
  !============================================================================
  subroutine get_curl_u(i, j, k, iBlock, CurlU_D)

    use BATL_lib, ONLY: IsCartesianGrid, CellSize_DB, FaceNormal_DDFB, &
         CellVolume_GB, x_, y_, z_
    use ModAdvance, ONLY: &
         LeftState_VX, LeftState_VY, LeftState_VZ,  &
         RightState_VX, RightState_VY, RightState_VZ
    use ModCoordTransform, ONLY: cross_product
    use ModSize, ONLY: MaxDim
    use ModVarIndexes, ONLY: Ux_, Uy_, Uz_

    integer, intent(in) :: i, j, k, iBlock
    real, intent(out) :: CurlU_D(MaxDim)

    real :: DxInvHalf, DyInvHalf, DzInvHalf
    character(len=*), parameter:: NameSub = 'get_curl_u'
    !--------------------------------------------------------------------------
    if(IsCartesianGrid)then
       DxInvHalf = 0.5/CellSize_DB(x_,iBlock)
       DyInvHalf = 0.5/CellSize_DB(y_,iBlock)
       DzInvHalf = 0.5/CellSize_DB(z_,iBlock)

       CurlU_D(x_) = &
            DyInvHalf*(LeftState_VY(Uz_,i,j+1,k)    &
            +          RightState_VY(Uz_,i,j+1,k)   &
            -          LeftState_VY(Uz_,i,j,k)      &
            -          RightState_VY(Uz_,i,j,k))    &
            - DzInvHalf*(LeftState_VZ(Uy_,i,j,k+1)  &
            +            RightState_VZ(Uy_,i,j,k+1) &
            -            LeftState_VZ(Uy_,i,j,k)    &
            -            RightState_VZ(Uy_,i,j,k))

       CurlU_D(y_) = &
            DzInvHalf*(LeftState_VZ(Ux_,i,j,k+1)    &
            +          RightState_VZ(Ux_,i,j,k+1)   &
            -          LeftState_VZ(Ux_,i,j,k)      &
            -          RightState_VZ(Ux_,i,j,k))    &
            - DxInvHalf*(LeftState_VX(Uz_,i+1,j,k)  &
            +            RightState_VX(Uz_,i+1,j,k) &
            -            LeftState_VX(Uz_,i,j,k)    &
            -            RightState_VX(Uz_,i,j,k))

       CurlU_D(z_) = &
            DxInvHalf*(LeftState_VX(Uy_,i+1,j,k)    &
            +          RightState_VX(Uy_,i+1,j,k)   &
            -          LeftState_VX(Uy_,i,j,k)      &
            -          RightState_VX(Uy_,i,j,k))    &
            - DyInvHalf*(LeftState_VY(Ux_,i,j+1,k)  &
            +            RightState_VY(Ux_,i,j+1,k) &
            -            LeftState_VY(Ux_,i,j,k)    &
            -            RightState_VY(Ux_,i,j,k))
    else
       CurlU_D(:) = &
            + cross_product( FaceNormal_DDFB(:,1,i+1,j,k,iBlock),           &
            LeftState_VX(Ux_:Uz_,i+1,j,k) + RightState_VX(Ux_:Uz_,i+1,j,k)) &
            - cross_product( FaceNormal_DDFB(:,1,i  ,j,k,iBlock),           &
            LeftState_VX(Ux_:Uz_,i,j,k) + RightState_VX(Ux_:Uz_,i,j,k))     &
            + cross_product( FaceNormal_DDFB(:,2,i,j+1,k,iBlock),           &
            LeftState_VY(Ux_:Uz_,i,j+1,k) + RightState_VY(Ux_:Uz_,i,j+1,k)) &
            - cross_product( FaceNormal_DDFB(:,2,i,j  ,k,iBlock),           &
            LeftState_VY(Ux_:Uz_,i,j,k) + RightState_VY(Ux_:Uz_,i,j,k))     &
            + cross_product( FaceNormal_DDFB(:,3,i,j,k+1,iBlock),           &
            LeftState_VZ(Ux_:Uz_,i,j,k+1) + RightState_VZ(Ux_:Uz_,i,j,k+1)) &
            - cross_product( FaceNormal_DDFB(:,3,i,j,k  ,iBlock),           &
            LeftState_VZ(Ux_:Uz_,i,j,k) + RightState_VZ(Ux_:Uz_,i,j,k))

       CurlU_D(:) = 0.5*CurlU_D(:)/CellVolume_GB(i,j,k,iBlock)
    end if

  end subroutine get_curl_u
  !============================================================================
  subroutine apportion_coronal_heating(i, j, k, iBlock, &
       State_V, WaveDissipationRate_V, CoronalHeating, &
       QPerQtotal_I, QparPerQtotal_I, QePerQtotal)
    !$acc routine seq

    ! Apportion the coronal heating to the electrons and protons based on
    ! how the Alfven waves dissipate at length scales << Lperp

    use ModVarIndexes, ONLY: nVar, Lperp_, nChargeStateAll
    use ModMain, ONLY: UseB0
    use ModPhysics, ONLY: IonMassPerCharge, pMin_I, TMin_I
    use ModAdvance, ONLY: nVar, UseAnisoPressure, Bx_, Bz_, Pe_
    use ModB0, ONLY: B0_DGB
    use ModChromosphere,  ONLY: DoExtendTransitionRegion, extension_factor, &
         TeSi_CI
    use ModMultiFluid, ONLY: ChargeIon_I, MassIon_I, UseMultiIon, &
         nIonFluid, iRhoIon_I, iRhoUxIon_I, iRhoUzIon_I, iPIon_I, &
         iPparIon_I
    use ModLookupTable, ONLY: interpolate_lookup_table

    integer, intent(in) :: i, j, k, iBlock
    real, intent(in) :: State_V(nVar)
    real, intent(in) :: WaveDissipationRate_V(WaveFirst_:WaveLast_)
    real, intent(in) :: CoronalHeating
    real, intent(out) :: QPerQtotal_I(nIonFluid), &
         QparPerQtotal_I(nIonFluid), QePerQtotal

    integer :: iIon, iPrev
    real :: Qtotal, Udiff_D(3), Upar, Valfven, Vperp
    real :: B_D(3), B, B2, InvGyroRadius, DeltaU, Epsilon, DeltaB, Delta
    real :: TeByTp, BetaElectron, BetaProton, Pperp, LperpInvGyroRad
    real :: pMin, P_I(nIonFluid), Ppar_I(nIonFluid)
    real :: Wmajor, Wminor, Wplus, Wminus, WmajorGyro, WminorGyro, Wgyro
    real :: DampingElectron, DampingPar_I(nIonFluid)
    real :: DampingPerp_I(nIonFluid), DampingProton
    real :: RhoProton, Ppar, SignMajor
    real :: QratioProton, ExtensionCoef, Qmajor, Qminor
    real, dimension(nIonFluid) :: QminorFraction_I, QmajorFraction_I, &
         CascadeTimeMajor_I, CascadeTimeMinor_I, Qmajor_I, Qminor_I, &
         QperpPerQtotal_I, GyroRadiusTimesB_I
    real :: BetaParProton, Np, Na, Ne, Tp, Ta, Te, Pp
    real :: Value_I(6), SqrtRho

    integer :: iGang

#ifndef SCALAR
    character(len=*), parameter:: NameSub = 'apportion_coronal_heating'
    !--------------------------------------------------------------------------
    iGang = i_gang(iBlock)

    if(UseStochasticHeating)then

       if(DoExtendTransitionRegion)then
#ifndef _OPENACC
          ExtensionCoef = extension_factor(TeSi_CI(i,j,k,iGang))
#endif
       else
          ExtensionCoef = 1.0
       end if
       Qtotal = max(CoronalHeating*ExtensionCoef, 1e-30)

       if(UseB0) then
          B_D = B0_DGB(:,i,j,k,iBlock) + State_V(Bx_:Bz_)
       else
          B_D = State_V(Bx_:Bz_)
       end if
       B2 = max(sum(B_D**2), 1e-30)
       B = sqrt(B2)

       RhoProton = State_V(iRhoIon_I(1))

       Valfven = B/sqrt(RhoProton)

       do iIon = 1, nIonFluid
          pMin = 0.0
          if(Tmin_I(iIon) < 0.0)then
             if(pMin_I(iIon) >= 0.0) pMin = pMin_I(iIon)
          else
             pMin = State_V(iRhoIon_I(iIon))/MassIon_I(iIon)*Tmin_I(iIon)
             if(pMin_I(iIon) >= 0.0) pMin = max(pMin_I(iIon), pMin)
          end if
          pMin = max(pMin, 1e-30)

          P_I(iIon) = max(pMin, State_V(iPIon_I(iIon)))
          if(UseAnisoPressure)then
             Ppar_I(iIon) = min(max(pMin, &
                  State_V(iPparIon_I(iIon))), (3*P_I(iIon)-2*pMin))
          else
             Ppar_I(iIon) = P_I(iIon)
          end if
       end do

       BetaProton = 2.0*P_I(1)/B2

       Wplus  = State_V(WaveFirst_)
       Wminus = State_V(WaveLast_)
       if(IsOnAwRepresentative)then
#ifndef _OPENACC
          SqrtRho = PoyntingFluxPerB*sqrt(RhoProton)
          Wplus = WPlus*SqrtRho; Wminus = Wminus*SqrtRho
#endif
       end if

       Wmajor = max(Wplus, Wminus)
       Wminor = min(Wplus, Wminus)

       ! Sign of major wave
       SignMajor = sign(1.0, Wplus - Wminus)

       if(SignMajor > 0.0)then
          Qmajor = WaveDissipationRate_V(WaveFirst_)*Wplus*ExtensionCoef
          Qminor = WaveDissipationRate_V(WaveLast_)*Wminus*ExtensionCoef
       else
          Qmajor = WaveDissipationRate_V(WaveLast_)*Wminus*ExtensionCoef
          Qminor = WaveDissipationRate_V(WaveFirst_)*Wplus*ExtensionCoef
       end if

       ! Linear Landau damping and transit-time damping of kinetic Alfven
       ! waves contributes to electron and parallel ion heating
       ! No heavy ion effects in the Linear Landau damping and transit-time
       ! damping yet
       if(UseMultiIon .and. nChargeStateAll==1)then
#ifndef _OPENACC
          BetaParProton = 2.0*Ppar_I(1)/B2
          Np = RhoProton
          Na = State_V(iRhoIon_I(nIonFluid))/MassIon_I(nIonFluid)
          Ne = sum(State_V(iRhoIon_I)*ChargeIon_I/MassIon_I)
          Tp = P_I(1)/Np
          Ta = P_I(nIonFluid)/Na
          Te = State_V(Pe_)/Ne

          ! difference bulk speed between alphas and protons
          Udiff_D = State_V(iRhoUxIon_I(nIonFluid):iRhoUzIon_I(nIonFluid)) &
               /State_V(iRhoIon_I(nIonFluid)) &
               -State_V(iRhoUxIon_I(1):iRhoUzIon_I(1)) &
               /State_V(iRhoIon_I(1))
          Upar = sum(Udiff_D*B_D)/B

          ! The damping rates (divided by k_parallel V_Ap) in the lookup
          ! table are for both forward propagating Alfven modes (i.e. in
          ! same direction as the alpha-proton drift) as well as backward
          ! propagating modes. The sign in drift can break the symmetrical
          ! behavior of forward and backward modes. For steady state, the
          ! Alfven modes are mostly forward propagating.
          call interpolate_lookup_table(iTableHeatPartition, &
               BetaParProton, abs(Upar)/Valfven, Tp/Ta, Tp/Te, Na/Np, &
               Value_I, DoExtrapolate = .false.)

          if(SignMajor*Upar < 0.0)then
             ! Backward propagating
             DampingPar_I(1) = Value_I(4)
             DampingPar_I(nIonFluid) = Value_I(6)
             DampingElectron = Value_I(5)
          else
             ! Forward propagating
             DampingPar_I(1) = Value_I(1)
             DampingPar_I(nIonFluid) = Value_I(3)
             DampingElectron = Value_I(2)
          end if
#endif
       else
          Pp = P_I(1)
          TeByTp = State_V(Pe_)/Pp

          BetaElectron = 2.0*State_V(Pe_)/B2

          DampingElectron = 0.01*sqrt(TeByTp/BetaProton) &
               *(1.0 + 0.17*BetaProton**1.3) &
               /(1.0 +(2800.0*BetaElectron)**(-1.25))
          DampingPar_I(1) = 0.08*sqrt(sqrt(TeByTp))*BetaProton**0.7 &
               *exp(-1.3/BetaProton)
          if(UseMultiIon) DampingPar_I(2:) = 0.0
       end if

       ! Stochasting heating contributes to perpendicular ion heating.
       ! Loop in reverse order for cascade power subtraction.
       do iIon = nIonFluid, 1, -1

          Ppar = Ppar_I(iIon)
          Pperp = 0.5*(3*P_I(iIon) - Ppar)

          ! Perpendicular ion thermal speed
          Vperp = sqrt(2.0*Pperp/State_V(iRhoIon_I(iIon)))

          GyroRadiusTimesB_I(iIon) = Vperp &
               *IonMassPerCharge*MassIon_I(iIon)/ChargeIon_I(iIon)

          InvGyroRadius = B/GyroRadiusTimesB_I(iIon)

          if(Lperp_ > 1)then
             LperpInvGyroRad = InvGyroRadius*State_V(Lperp_)/RhoProton
          else
             LperpInvGyroRad = InvGyroRadius*LperpTimesSqrtB/sqrt(B)
          end if

          ! Current multi-ion implementation assumes either alpha particles
          ! without heavy ions or heavy ions without alpha particles
          if(UseMultiIon .and. nChargeStateAll > 1)then
             if(iIon > 1)then
                ! heavy ions
                Qmajor_I(iIon) = Qmajor
                Qminor_I(iIon) = Qminor
             else
                ! protons
                ! For safety we floor the heating
                Qmajor_I(1) = Qmajor*max(1.0 - sum(QmajorFraction_I(2:)),0.0)
                Qminor_I(1) = Qminor*max(1.0 - sum(QminorFraction_I(2:)),0.0)
             end if

             WmajorGyro = Wmajor/sqrt(LperpInvGyroRad)
             WminorGyro = Wminor/sqrt(LperpInvGyroRad)

          elseif(iIon == nIonFluid)then
             Qmajor_I(iIon) = Qmajor
             Qminor_I(iIon) = Qminor

             WmajorGyro = Wmajor/sqrt(LperpInvGyroRad)
             WminorGyro = Wminor/sqrt(LperpInvGyroRad)

          else
             iPrev = iIon + 1

             ! Subtract what was used for stochastic heating of alphas
             Qmajor_I(iIon) = &
                  Qmajor_I(iPrev)*(1.0 - QmajorFraction_I(iPrev))
             Qminor_I(iIon) = &
                  Qminor_I(iPrev)*(1.0 - QminorFraction_I(iPrev))

             ! Reduce similarly the cascade power and exploit non-alignment
             ! (Boldyrev, 2005) of small-scale fluctuations to arrive at
             ! Wmajor and Wminor at the proton gyro-radius
             WmajorGyro = WmajorGyro &
                  *( (1.0 - QmajorFraction_I(iPrev))**2 &
                  /(1.0 - QminorFraction_I(iPrev)) )**(2.0/3.0) &
                  *sqrt(GyroRadiusTimesB_I(iIon)/GyroRadiusTimesB_I(iPrev))
             WminorGyro = WminorGyro &
                  *( (1.0 - QminorFraction_I(iPrev))**2 &
                  /(1.0 - QmajorFraction_I(iPrev)) )**(2.0/3.0) &
                  *sqrt(GyroRadiusTimesB_I(iIon)/GyroRadiusTimesB_I(iPrev))
          end if

          Wgyro = WmajorGyro + WminorGyro

          ! Cascade timescale at the gyroscale
          CascadeTimeMajor_I(iIon) = WmajorGyro/max(Qmajor_I(iIon),1e-30)
          CascadeTimeMinor_I(iIon) = WminorGyro/max(Qminor_I(iIon),1e-30)

          ! For protons the following would be DeltaU and DeltaB at ion gyro
          ! radius, except that we assumed that the Alfven ratio is one.
          DeltaU = sqrt(Wgyro/RhoProton)
          DeltaB = sqrt(Wgyro)

          Epsilon = DeltaU/Vperp
          Delta = DeltaB/B

          ! Damping rate for stochastic heating.
          ! It interpolates between the beta<1 and 1<beta<30 version.
          ! This formula is at the moment only suitable for protons.
          DampingPerp_I(iIon) = (StochasticAmplitude &
               *exp(-StochasticExponent/max(Epsilon,1e-15)) &
               + StochasticAmplitude2*sqrt(BetaProton) &
               *exp(-StochasticExponent2/max(Delta,1e-15))) &
               *State_V(iRhoIon_I(iIon))*DeltaU**3 &
               *InvGyroRadius/max(Wgyro,1e-15)

          if(iIon == 1)then
             ! Set k_parallel*V_Alfven = 1/t_minor (critical balance)
             DampingElectron = DampingElectron/max(CascadeTimeMinor_I(1),1e-30)
             DampingPar_I = DampingPar_I/max(CascadeTimeMinor_I(1), 1e-30)

             ! Total damping rate around proton gyroscale
             DampingProton = DampingElectron + sum(DampingPar_I) &
                  + DampingPerp_I(1)

             QmajorFraction_I(1) = DampingProton*CascadeTimeMajor_I(1) &
                  /(1.0 + DampingProton*CascadeTimeMajor_I(1))
             QminorFraction_I(1) = DampingProton*CascadeTimeMinor_I(1) &
                  /(1.0 + DampingProton*CascadeTimeMinor_I(1))
          else
             QmajorFraction_I(iIon) = &
                  DampingPerp_I(iIon)*CascadeTimeMajor_I(iIon) &
                  /(1.0 + DampingPerp_I(iIon)*CascadeTimeMajor_I(iIon))
             QminorFraction_I(iIon) = &
                  DampingPerp_I(iIon)*CascadeTimeMinor_I(iIon) &
                  /(1.0 + DampingPerp_I(iIon)*CascadeTimeMinor_I(iIon))
          end if
       end do

       QratioProton = (QmajorFraction_I(1)*Qmajor_I(1) &
            + QminorFraction_I(1)*Qminor_I(1))/Qtotal

       QparPerQtotal_I = DampingPar_I/DampingProton*QratioProton

       QperpPerQtotal_I(1) = DampingPerp_I(1)/DampingProton*QratioProton

       if(nIonFluid > 1) QperpPerQtotal_I(2:) = &
            (QmajorFraction_I(2:)*Qmajor_I(2:) &
            + QminorFraction_I(2:)*Qminor_I(2:))/Qtotal

       QPerQtotal_I = QperpPerQtotal_I + QparPerQtotal_I

       QePerQtotal = (DampingElectron/DampingProton - 1)*QratioProton &
            + (Qmajor_I(1) + Qminor_I(1))/Qtotal

    elseif(UseUniformHeatPartition)then
       QPerQtotal_I = QionRatio_I
       QparPerQtotal_I = QionParRatio_I
       QePerQtotal = QeRatio

    else
       call stop_mpi(NameSub//' Unknown energy partitioning')
    end if
#endif
  end subroutine apportion_coronal_heating
  !============================================================================
  subroutine wave_energy_to_representative
    ! Convert Alfven wave turbulence energy densities to
    ! dimensionless representative functions. Switch the logical
    ! UseAwRepresentativeHere on.
    use BATL_lib,      ONLY: Unused_B, Used_GB, nBlock, &
         MinI, MaxI, MinJ, MaxJ, MinK, MaxK
    use ModAdvance,    ONLY: State_VGB
    use ModVarIndexes, ONLY: Rho_, WDiff_, WaveFirst_, WaveLast_

    integer :: iBlock, i, j, k
    real    :: InvSqrtRho
    !--------------------------------------------------------------------------
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          if(.not.Used_GB(i,j,k,iBlock))CYCLE
          InvSqrtRho = 1/(  PoyntingFluxPerB*&
               sqrt( State_VGB(Rho_,i,j,k,iBlock) )  )
          State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock) = &
               InvSqrtRho*State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock)
          if(WDiff_>1)State_VGB(WDiff_,i,j,k,iBlock) = &
               InvSqrtRho*State_VGB(WDiff_,i,j,k,iBlock)
       end do; end do; end do
    end do
    IsOnAwRepresentative = .true.
    !$acc update device(IsOnAwRepresentative)
  end subroutine wave_energy_to_representative
  !============================================================================
  subroutine representative_to_wave_energy
    ! Convert dimensionless representative functions to Alfven wave turbulence
    ! energy densities. Switch of the logical IsOnAwRepresentative
    use BATL_lib,      ONLY: Unused_B, Used_GB, nBlock
    use ModVarIndexes, ONLY: Rho_, WDiff_, WaveFirst_, WaveLast_
    use ModAdvance,    ONLY: State_VGB, StateOld_VGB

    integer :: iBlock, i, j, k
    real    :: SqrtRho
    !--------------------------------------------------------------------------
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(.not.Used_GB(i,j,k,iBlock))CYCLE
          SqrtRho = PoyntingFluxPerB*sqrt( &
               State_VGB(Rho_,i,j,k,iBlock) )
          State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock) = &
               SqrtRho*State_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock)
          StateOld_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock) = &
               SqrtRho*StateOld_VGB(WaveFirst_:WaveLast_,i,j,k,iBlock)
          if(WDiff_>1)then
             State_VGB(WDiff_,i,j,k,iBlock) = &
                  SqrtRho*State_VGB(WDiff_,i,j,k,iBlock)
             StateOld_VGB(WDiff_,i,j,k,iBlock) = &
                  SqrtRho*StateOld_VGB(WDiff_,i,j,k,iBlock)
          end if
       end do; end do; end do
    end do
    IsOnAwRepresentative = .false.
    !$acc update device(IsOnAwRepresentative)
  end subroutine representative_to_wave_energy
  !============================================================================
end module ModTurbulence
!==============================================================================
