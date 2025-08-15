!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used
!  with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModMultiIon

  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest, iVarTest, iProc
  use ModBatsrusUtility, ONLY: stop_mpi

  ! !! "resistive terms" = ion-electron collisions to be added
  ! !! RZ geometry terms are missing

  ! Calculate source terms for multi-ion MHD.
  ! For sake of numerical stability this should be done with
  ! a point-implicit scheme.
  ! Allow for extra point-implicit sources in ModUser

  use ModMultiFluid
  use ModMain, ONLY: UseUserSourceImpl

  use ModUserInterface ! user_calc_sources_impl, user_init_point_implicit

  implicit none

  private

  ! Public methods and variables
  public:: multi_ion_set_parameters
  public:: multi_ion_set_restrict
  public:: multi_ion_source_expl
  public:: multi_ion_source_impl
  public:: multi_ion_init_point_impl

  logical, public, protected   :: DoRestrictMultiIon = .false.

  ! Does the cell have significant amount of multiple ion fluids?
  logical, public, allocatable, protected :: IsMultiIon_CB(:,:,:,:)

  ! Local variables

  ! parameters for selecting the single ion region
  real :: MachNumberMultiIon = 0.0
  real :: ParabolaWidthMultiIon = 0.0

  ! collision coefficient
  real :: CollisionCoefDim = -1.0
  real :: CollisionCoef = -1.0

  ! artificial friction parameters
  real    :: uCutOffDim = 0.0     ! cut-off velocity
  real    :: TauCutOffDim = -1.0  ! cut-off time scale
  integer :: nPowerCutOff = 0     ! cut-off exponent

  ! calculate analytic Jacobian for point-implicit scheme
  logical, parameter:: IsAnalyticJacobian = .true.

  ! Minimum pressure ratio for a minor fluid (so it remains positive)
  real:: LowPressureRatio = 1e-10

contains
  !============================================================================
  subroutine multi_ion_set_parameters(NameCommand)

    use ModSize, ONLY: nI, nJ, nK, MaxBlock
    use ModPhysics, ONLY: LowDensityRatio
    use ModReadParam, ONLY: read_var
    use ModPointImplicit, ONLY: IsPointImplMatrixSet
    use ModAdvance, ONLY: UseSingleIonVelocity, UseSingleIonTemperature

    character(len=*), intent(in):: NameCommand
    !--------------------------------------------------------------------------
    select case(NameCommand)
    case("#MULTIION")
       call read_var('LowDensityRatio',    LowDensityRatio)
       call read_var('LowPressureRatio',   LowPressureRatio)
       call read_var('DoRestrictMultiIon', DoRestrictMultiIon)
       if(DoRestrictMultiIon)then
          call read_var('MachNumberMultiIon',    MachNumberMultiIon)
          call read_var('ParabolaWidthMultiIon', ParabolaWidthMultiIon)
       end if
       IsPointImplMatrixSet = IsAnalyticJacobian

    case("#MULTIIONSTATE")
       call read_var('UseSingleIonVelocity',    UseSingleIonVelocity)
       call read_var('UseSingleIonTemperature', UseSingleIonTemperature)

    case("#COLLISION")
       call read_var('CollisionCoefDim', CollisionCoefDim)
       call read_var('TauCutOff', TauCutOffDim)
       if(TauCutOffDim > 0.0)then
          call read_var('uCutOffDim', uCutOffDim)
          call read_var('nPowerCutOff', nPowerCutOff)
       end if
    end select

    if(DoRestrictMultiIon .and. .not.allocated(IsMultiIon_CB)) &
         allocate(IsMultiIon_CB(nI,nJ,nK,MaxBlock))

  end subroutine multi_ion_set_parameters
  !============================================================================
  subroutine multi_ion_set_restrict(iBlock)

    ! Identify regions where only one ion fluid is present.

    use ModSize,     ONLY: nI, nJ, nK, x_, y_
    use ModAdvance,  ONLY: State_VGB, Rho_, RhoUx_, p_
    use ModPhysics,  ONLY: Gamma
    use BATL_lib,    ONLY: Xyz_DGB, Used_GB

    integer, intent(in) :: iBlock

    real    :: Rho, p, RhoUx
    integer :: i, j, k
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'multi_ion_set_restrict'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    do k=1,nK; do j=1,nJ; do i=1,nI
       if(.not.Used_GB(i,j,k,iBlock)) CYCLE
       ! Check if we are in the solar wind
       Rho   = State_VGB(Rho_,i,j,k,iBlock)
       p     = State_VGB(p_,i,j,k,iBlock)
       RhoUx = State_VGB(RhoUx_,i,j,k,iBlock)

       IsMultiIon_CB(i,j,k,iBlock) = .not. &
            (RhoUx < 0.0 .and. RhoUx**2 > MachNumberMultiIon**2*Gamma*p*Rho &
            .and. sum(Xyz_DGB(x_:y_,i,j,k,iBlock)**2) > &
            -ParabolaWidthMultiIon * Xyz_DGB(x_,i,j,k,iBlock))

       if(DoTest .and. i == iTest .and. j == jTest .and. k == kTest) then
          write(*,*) NameSub,'Rho, p, RhoUx =',Rho, p, RhoUx
          write(*,*) NameSub,'RhoUx**2, MachNumberMultiIon*g*p*Rho=', &
               RhoUx**2, MachNumberMultiIon*Gamma*p*Rho
          write(*,*) NameSub,'y**2, z**2, -ParabolaWidthMultiIon*x=', &
               Xyz_DGB(x_:y_,i,j,k,iBlock)**2, &
                -ParabolaWidthMultiIon * Xyz_DGB(x_,i,j,k,iBlock)
          write(*,*) NameSub, ' IsMultiIon_CB=',  IsMultiIon_CB(i,j,k,iBlock)
       end if

    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine multi_ion_set_restrict
  !============================================================================
  subroutine multi_ion_source_expl(iBlock)

    ! Add non-stiff source terms specific to multi-ion MHD
    !
    !    d(rho u_s)/dt +=     (n_s/n_e)*E
    !    d(e_s)/dt     += u_s.(n_s/n_e)*E
    !
    !    where s is the index of the ion fluid, and E is the electric
    !    field in the comoving frame.

    use ModMain,    ONLY: MaxDim, nI, nJ, nK, x_, y_, z_, UseB0
    use ModBorisCorrection, ONLY: UseBorisCorrection, UseBorisSimple
    use ModAdvance, ONLY: State_VGB, Source_VC, Efield_DGB, UseTotalIonEnergy
    use ModB0,      ONLY: B0_DGB
    use ModPhysics, ONLY: InvClight2
    use BATL_lib,   ONLY: Used_GB

    integer, intent(in) :: iBlock

    real :: State_V(nVar)
    real, dimension(MaxDim)   :: FullB_D
    real, dimension(nIonFluid):: ForceX_I, ForceY_I, ForceZ_I, ChargeDens_I, &
         Work_I
    real :: InvElectronDens

    ! Alfven Lorentz factor for Boris correction
    real :: Ga2

    integer :: i, j, k, iIonFluid

    logical :: DoTestCell

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'multi_ion_source_expl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(DoTest)then
       write(*,'(2a,es16.8)') NameSub,': initial Source_VC=', &
            Source_VC(iVarTest,iTest,jTest,kTest)
    end if
    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       DoTestCell = DoTest .and. i == iTest .and. j == jTest .and. k == kTest
       if(.not.Used_GB(i,j,k,iBlock)) CYCLE
       State_V = State_VGB(:,i,j,k,iBlock)
       ChargeDens_I = ChargePerMass_I*State_V(iRhoIon_I)
       InvElectronDens = 1.0/sum(ChargeDens_I)

       if(UseBorisCorrection .or. UseBorisSimple)then
          ! Simplified Boris correction
          ! (see the ASTRONUM 2009 proceedings paper by Toth et al.)
          ! Divide the number density by
          !
          ! 1 + V_s^2/c^2 = 1 + B^2/(c^2*n*M_s/q_s)
          !
          ! where we used V_s^2 = (q_s*M/M_s)*B^2/rho = B^2/(n*M_s/q_s)
          ! and q_s is the charge of species s in units of electron charge.
          FullB_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
          if(UseB0) FullB_D =  FullB_D + B0_DGB(:,i,j,k,iBlock)
          Ga2 = sum(FullB_D**2)*InvClight2*InvElectronDens
          ChargeDens_I = ChargeDens_I/(1 + Ga2*ChargePerMass_I)
       end if

       ! Multiply by n_s/n_e for all ion fluids
       ForceX_I = ChargeDens_I*Efield_DGB(x_,i,j,k,iBlock)
       ForceY_I = ChargeDens_I*Efield_DGB(y_,i,j,k,iBlock)
       ForceZ_I = ChargeDens_I*Efield_DGB(z_,i,j,k,iBlock)

       if(DoTestCell)then
          do iIonFluid = 1,nIonFluid
             write(*,'(2a,i1,a,15es16.8)') &
                  Namesub,': iIonFluid =', iIonFluid, ', Force_D  =', &
                  ForceX_I(iIonFluid), ForceY_I(iIonFluid), ForceZ_I(iIonFluid)
          end do
          write(*,'(2a,15es16.8)')                          &
               Namesub,': InvElectronDens, ChargeDens_I =', &
               InvElectronDens, ChargeDens_I
       end if

       ! Store ion momentum sources
       Source_VC(iRhoUxIon_I,i,j,k) = Source_VC(iRhoUxIon_I,i,j,k) + ForceX_I
       Source_VC(iRhoUyIon_I,i,j,k) = Source_VC(iRhoUyIon_I,i,j,k) + ForceY_I
       Source_VC(iRhoUzIon_I,i,j,k) = Source_VC(iRhoUzIon_I,i,j,k) + ForceZ_I

       Work_I = ( State_V(iRhoUxIon_I)*ForceX_I &
            +     State_V(iRhoUyIon_I)*ForceY_I &
            +     State_V(iRhoUzIon_I)*ForceZ_I ) / State_V(iRhoIon_I)

       ! Calculate ion energy sources = u_s.Force_s
       Source_VC(nVar+1:nVar+nIonFluid,i,j,k) = &
            Source_VC(nVar+1:nVar+nIonFluid,i,j,k) + Work_I

       ! Work for total ion energy is already added in ModPhysicalFlux
       if(UseTotalIonEnergy) Source_VC(Energy_,i,j,k) = &
            Source_VC(Energy_,i,j,k) - sum(Work_I)

    end do; end do; end do

    if(DoTest)write(*,'(2a,15es16.8)')NameSub,': final Source_VC =',&
         Source_VC(iVarTest,iTest,jTest,kTest)

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine multi_ion_source_expl
  !============================================================================
  subroutine multi_ion_source_impl(iBlock)

    ! Add 'stiff' source terms specific to multi-ion MHD:
    !
    ! 1. d(rho u_s)/dt +=      n_s*(- u_+ - w_H + u_s )xB
    !    d(e_s)/dt     += u_s.[n_s*(- u_+ - w_H + u_s )xB]
    !    where s is the index for the ion fluid,
    !    u_+ is the charge density weighted average ion velocity,
    !    w_H = -J/(e n_e) is the Hall velocity,
    !    n_e is the electron number density, and
    !    e is the electron charge.
    !
    ! 2. ion-ion collisions if required.
    !
    ! 3. artificial friction term if required.
    !
    ! 4. user source terms if required.

    use ModPointImplicit, ONLY:  UsePointImplicit, UseUserPointImplicit_B, &
         IsPointImplPerturbed, DsDu_VVC
    use ModMain,    ONLY: nI, nJ, nK, UseB0, UseFlic
    use ModAdvance, ONLY: State_VGB, Source_VC
    use ModB0,      ONLY: B0_DGB
    use BATL_lib,   ONLY: Xyz_DGB
    use ModPhysics, ONLY: ElectronCharge, InvGammaMinus1_I, &
         InvClight2, Si2No_V, No2Si_V, Io2No_V, &
         UnitTemperature_, UnitT_, UnitU_
    use ModBorisCorrection, ONLY: UseBorisCorrection, UseBorisSimple

    use ModMain,    ONLY: x_, y_, z_
    use ModCoordTransform, ONLY: cross_product
    use ModNumConst,       ONLY: iLeviCivita_III
    use ModSize,           ONLY: MaxDim
    use BATL_lib, ONLY: Used_GB

    integer, intent(in) :: iBlock

    ! Variables for multi-ion MHD
    real:: InvElectronDens, State_V(nVar)
    real, dimension(3) :: FullB_D, uIon_D, uIon2_D, u_D, uPlus_D
    real, dimension(3) :: Force_D
    real, dimension(nIonFluid) :: &
         NumDens_I, ChargeDens_I, ChargeDensBoris_I, &
         Rho_I, InvRho_I, Ux_I, Uy_I, Uz_I, Temp_I

    ! Alfven Lorentz factor for Boris correction
    real :: Ga2

    integer :: i, j, k, iIon, jIon, iRhoUx, iRhoUz, iP, iEnergy
    real :: AverageTemp, TemperatureCoef
    real :: CollisionRate_II(nIonFluid, nIonFluid), CollisionRate

    ! Artificial friction
    real :: InvuCutOff2, InvTauCutOff

    logical :: DoTestCell

    ! Variables for analytic Jacobian
    integer :: iDim, jDim, kDim, iUi, iUk
    real    :: SignedB, ForceCoeff, Coeff, CoefJacobian, Du2
    real    :: Du_D(3)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'multi_ion_source_impl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    DoTestCell = .false.

    ! Add user defined point implicit source terms here
    ! Explicit user sources are added in calc_sources
    if(UsePointImplicit .and. UseUserSourceImpl)then
       if(UseUserPointImplicit_B(iBlock)) call user_calc_sources_impl(iBlock)
    end if

    ! Do not evaluate multi-ion sources in the numerical Jacobian calculation
    ! (needed for the user source terms)
    if(IsPointImplPerturbed .and. IsAnalyticJacobian) RETURN

    if(CollisionCoefDim > 0.0)then
       ! Rate = n*CoefDim / T^1.5 with T [K], n [/cc] and Rate [1/s]
       CollisionCoef = CollisionCoefDim &
            /No2Si_V(UnitTemperature_)**1.5/Si2No_V(UnitT_)

       do jIon = 1, nIonFluid
          do iIon = 1, nIonFluid
             CollisionRate_II(iIon, jIon) = CollisionCoef* &
                  MassIon_I(iIon)*MassIon_I(jIon) &
                  /(MassIon_I(iIon)+MassIon_I(jIon))
          end do
       end do
    end if

    do k = 1, nK; do j = 1, nJ; do i = 1, nI
        if(.not.Used_GB(i,j,k,iBlock)) CYCLE

       DoTestCell = DoTest .and. i==iTest .and. j==jTest .and. k==kTest

       if(DoTestCell)write(*,'(2a,15es16.8)') NameSub, ' initial source = ',&
            Source_VC(iVarTest,i,j,k)

       ! Extract conservative variables
       State_V = State_VGB(:,i,j,k,iBlock)

       ! Total magnetic field
       FullB_D = State_V(Bx_:Bz_)
       if(UseB0) FullB_D =  FullB_D + B0_DGB(:,i,j,k,iBlock)

       ! calculate number densities
       NumDens_I     = State_V(iRhoIon_I) / MassIon_I
       ChargeDens_I  = NumDens_I * ChargeIon_I
       InvElectronDens = 1.0/sum(ChargeDens_I)

       if(InvElectronDens < 0.0) &
            call stop_mpi(NameSub//': negative electron denisty')

       ChargeDensBoris_I = ChargeDens_I

       if(UseBorisCorrection .or. UseBorisSimple)then
          ! See the ASTRONUM 2009 proceedings paper by Toth et al.
          !
          ! Boris correction: divide the number density by
          !
          ! 1 + V_s^2/c^2 = 1 + B^2/(c^2*n*M_s/q_s)
          !
          ! where we used V_s^2 = (q_s*M/M_s)*B^2/rho = B^2/(n*M_s/q_s)

          Ga2 = sum(FullB_D**2)*InvClight2*InvElectronDens
          ChargeDensBoris_I = ChargeDens_I/(1 + Ga2*ChargeIon_I/MassIon_I)
       end if

       Temp_I     = State_V(iPIon_I)/NumDens_I
       AverageTemp= sum(State_V(iPIon_I))/sum(NumDens_I)

       if(AverageTemp <= 0.0)then
          write(*,*)'ERROR: AverageTemp =',AverageTemp
          write(*,*)'i,j,k,iBlock,iProc =',i,j,k,iBlock,iProc
          write(*,*)'x,y,z              =',Xyz_DGB(:,i,j,k,iBlock)
          write(*,*)'iRhoIon_I          =',iRhoIon_I
          write(*,*)'RhoIon_I           =',State_V(iRhoIon_I)
          write(*,*)'MassIon_I          =',MassIon_I
          write(*,*)'ChargeIon_I        =',ChargeIon_I
          write(*,*)'NumDens_I          =',NumDens_I
          write(*,*)'ChargeDens_I       =',ChargeDens_I
          write(*,*)'iPIon_I            =',iPIon_I
          write(*,*)'PIon_I             =',State_V(iPIon_I)
          write(*,*)'Temp_I             =',Temp_I
          call stop_mpi(NameSub//': non-positive average temperature')
       end if

       Rho_I    = State_V(iRhoIon_I)
       InvRho_I = 1.0/Rho_I
       Ux_I     = InvRho_I*State_V(iUxIon_I)
       Uy_I     = InvRho_I*State_V(iUyIon_I)
       Uz_I     = InvRho_I*State_V(iUzIon_I)

       ! calculate the average positive charge velocity
       uPlus_D(x_) = InvElectronDens * sum(ChargeDens_I*Ux_I)
       uPlus_D(y_) = InvElectronDens * sum(ChargeDens_I*Uy_I)
       uPlus_D(z_) = InvElectronDens * sum(ChargeDens_I*Uz_I)

       TemperatureCoef = 1.0/(AverageTemp*sqrt(AverageTemp))

       if(TauCutOffDim > 0)then
          InvTauCutOff = 1.0/(Io2No_V(UnitT_)*TauCutOffDim)
          InvuCutOff2  = 1.0/(Io2No_V(UnitU_)*uCutOffDim)**2
       end if
       if(DoTestCell)then
          if(UseBorisCorrection)write(*,'(2a,15es16.8)') NameSub,'Ga2=',Ga2
          write(*,'(2a,15es16.8)') NameSub,' FullB_D  =', FullB_D
          write(*,'(2a,15es16.8)') NameSub,' uPlus_D  =', uPlus_D
       end if

       ! Calculate the source term for all the ion fluids
       do iIon = 1, nIonFluid
          uIon_D = [ Ux_I(iIon),  Uy_I(iIon), Uz_I(iIon) ]
          u_D    = uIon_D - uPlus_D
          ForceCoeff = ElectronCharge*ChargeDensBoris_I(iIon)
          if(UseFlic)then
             ! UxB force is treated by advance_ion_current
             Force_D = 0
          else
             Force_D    = ForceCoeff * cross_product(u_D, FullB_D)

             if(DoTestCell)then
                write(*,'(2a,i2)') NameSub,' iIon            =', iIon
                write(*,'(2a,15es16.8)') NameSub,' uIon_D          =', uIon_D
                write(*,'(2a,15es16.8)') NameSub,' u_D             =', u_D
                write(*,'(2a,15es16.8)') NameSub,' ForceCoeff      =', &
                     ForceCoeff
                write(*,'(2a,15es16.8)') NameSub,' ChargeDensBoris =', &
                     ChargeDensBoris_I(iIon)
                write(*,'(2a,15es16.8)') NameSub,' Force_D         =', &
                     Force_D
             end if

             ! Set corresponding matrix element
             if (IsAnalyticJacobian .and. UsePointImplicit) then
                do kDim = 1, MaxDim
                   iUk = iUxIon_I(iIon) + kDim - 1
                   do iDim = 1, MaxDim
                      if(kDim == iDim) CYCLE
                      jDim = 6 - kDim - iDim
                      SignedB = iLeviCivita_III(iDim, jDim, kDim)*FullB_D(jDim)

                      ! Jacobian term occurs with respect to the same fluid
                      iUi = iUxIon_I(iIon) + iDim - 1
                      DsDu_VVC(iUk, iUi, i, j, k) = &
                           DsDu_VVC(iUk, iUi, i, j, k) &
                           + ForceCoeff*SignedB*InvRho_I(iIon)

                      Coeff = ForceCoeff*SignedB*InvElectronDens
                      ! This term is with respect to any fluid
                      do jIon = 1, nIonFluid
                         iUi = iUxIon_I(jIon) + iDim - 1
                         DsDu_VVC(iUk,iUi,i,j,k) = &
                              DsDu_VVC(iUk,iUi,i,j,k) - &
                              Coeff*ChargeIon_I(jIon)/MassIon_I(jIon)
                      end do
                   end do
                end do
             end if
          end if

          if(CollisionCoefDim > 0.0 .or. TauCutOffDim > 0.0)then
             do jIon = 1, nIonFluid
                if(jIon == iIon) CYCLE

                ! Add collisional terms
                uIon2_D = [ Ux_I(jIon),  Uy_I(jIon), Uz_I(jIon) ]

                ! Physical collision
                if(CollisionCoefDim > 0.0)then
                   CollisionRate = CollisionRate_II(iIon, jIon) * &
                        NumDens_I(iIon) * NumDens_I(jIon) &
                        * TemperatureCoef
                else
                   CollisionRate = 0.0
                end if

                ! Artificial friction to keep the velocity difference in check
                ! We take the smaller of the two densities so that the
                ! acceleration is independent of the density of
                ! the minor species and the restriction works in all regions.
                ! The min function is symmetric, so momentum is conserved.
                ! u_0 is the cut-off velocity, Tau gives the time rate, and
                ! the power determines how sharp the cut-off is.
                if(TauCutOffDim > 0.0)then
                   ! CollisionRate =
                   !  1/tau * min(rho^iIon, rho^jIon) * (du2/u_0^2)^n

                   if(uCutOffDim < -1.01)then
                      ! Use simple Alfven speed B/sqrt(rho1+rho2)
                      InvUCutOff2 = (Rho_I(iIon) + Rho_I(jIon))/sum(FullB_D**2)
                   elseif(uCutOffDim < 0.0)then
                      ! Use properly "averaged" Alfven speed
                      ! for the cut-off velocity based on
                      ! "On the physical realization of two-dimensional
                      ! turbulence fields in magnetized interplanetary plasmas"
                      ! A. Stockem et al., APJ 651, 584, (2006). Eq(29) has
                      !
                      ! Du_crit = V_A1*sqrt(1 + r_n)
                      !
                      ! where V_A1 = B/sqrt(rho1) from just above eq.29
                      ! and r_n = N1/N2 = rho1/rho2 defined after eq.14, so
                      !
                      ! Du_crit^2 = B^2/rho1 * (1 + rho1/rho2) = B^2 / rho12
                      !
                      ! with rho12 = rho1*rho2/(rho1 + rho2)

                      InvUCutOff2 = 1.0 / (sum(FullB_D**2) &
                           *(Rho_I(iIon) + Rho_I(jIon)) &
                           *InvRho_I(iIon)*InvRho_I(jIon))
                   end if

                   Du2 = sum( (uIon2_D - uIon_D)**2 )
                   CollisionRate = CollisionRate + &
                        InvTauCutOff * min(Rho_I(iIon), Rho_I(jIon)) &
                        * ( InvUCutOff2 * Du2 ) ** nPowerCutOff
                end if

                Force_D = Force_D + CollisionRate*(uIon2_D - uIon_D)

                if(DoTestCell)then
                   write(*,'(2a,es16.8)') &
                        NameSub,' CollisionCoef=', CollisionCoef
                   write(*,'(2a,es16.8)') &
                        NameSub,' AverageTemp  =',AverageTemp
                   write(*,'(2a,es16.8)') &
                        NameSub,' AverageTempDim=', &
                        AverageTemp*No2Si_V(UnitTemperature_)
                   write(*,'(2a,i2)') &
                        NameSub,' after collision, iIon            =', iIon
                   write(*,'(2a,3es16.8)') &
                        NameSub,' after collision, Force_D         =', Force_D
                end if

                ! !! No heating for now
                ! If heating is added as below,
                ! adjust update_states_MHD to make sure that
                ! the execution passes through here even
                ! if UseUniformIonVelocity is true
                ! (fluids can have different temperatures)
                ! Heating = Heating + CollisionRate* &
                !     ( 2*(Temp_I(jIon) - Temp_I(iIon)) &
                !     + gm1*sum((uIon2_D - uIon_D)**2) )

                ! Calculate corresponding matrix elements
                if (TauCutOffDim > 0.0 .and. IsAnalyticJacobian) then

                   ! du = u^iIon - u^jIon
                   Du_D = uIon_D - uIon2_D

                   ! Common coefficient: CoefJacobian =
                   !  1/tau * min(rho^iIon, rho^jIon) * (1/u_0)^2n * (du^2)^n-1
                   CoefJacobian = InvTauCutOff &
                        * min(Rho_I(iIon), Rho_I(jIon)) &
                        * InvUCutOff2 ** nPowerCutOff &
                        * Du2 ** (nPowerCutOff - 1)

                   ! Add dFriction/d(RhoU) elements to the Jacobian
                   do kDim = 1, MaxDim
                      ! k component of RhoU^iIon
                      iUk = iUxIon_I(iIon) + kDim - 1
                      do iDim = 1, MaxDim

                         ! dFriction^iIon_k/d(RhoU^iIon_i) = -CoefJacobian
                         !  *(2*n*du_i*du_k/rho^iIon + delta_ik*du^2/rho^iIon)

                         iUi = iUxIon_I(iIon) + iDim - 1
                         DsDu_VVC(iUk, iUi, i, j, k) = &
                              DsDu_VVC(iUk, iUi, i, j, k) &
                              - 2.0 * nPowerCutOff * InvRho_I(iIon) &
                              * Du_D(iDim) * Du_D(kDim) &
                              *  CoefJacobian
                         if (iDim == kDim) DsDu_VVC(iUk, iUi, i, j, k) = &
                              DsDu_VVC(iUk, iUi, i, j, k) &
                              - CoefJacobian * Du2 *InvRho_I(iIon)

                         ! dFriction^iIon_k/d(RhoU^jIon_i) = +CoefJacobian
                         !  *(2*n*du_i*du_k/rho^jIon + delta_ik*du^2/rho^jIon)

                         iUi = iUxIon_I(jIon) + iDim - 1
                         DsDu_VVC(iUk, iUi, i, j, k) = &
                              DsDu_VVC(iUk, iUi, i, j, k) &
                              + 2.0 * nPowerCutOff *InvRho_I(jIon) &
                              * Du_D(iDim) * Du_D(kDim) &
                              * CoefJacobian
                         if (iDim == kDim)DsDu_VVC(iUk, iUi, i, j, k)  = &
                              DsDu_VVC(iUk, iUi, i, j, k) &
                              + CoefJacobian * Du2 *InvRho_I(jIon)
                      end do
                   end do
                end if
             end do

             ! No heating
             ! iP = iPIon_I(iIon)
             ! Source_VC(iP,i,j,k) = Source_VC(iP,i,j,k) + Heating

          end if

          iRhoUx = iRhoUxIon_I(iIon); iRhoUz = iRhoUzIon_I(iIon)
          Source_VC(iRhoUx:iRhoUz,i,j,k) = Source_VC(iRhoUx:iRhoUz,i,j,k) &
               + Force_D

          iEnergy = Energy_ - 1 + iIon
          Source_VC(iEnergy,i,j,k) = Source_VC(iEnergy,i,j,k) &
               + sum(Force_D*uIon_D)

       end do

       if(DoTestCell)write(*,'(2a,15es16.8)')NameSub, ' final source = ',&
            Source_VC(iVarTest,i,j,k)

    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine multi_ion_source_impl
  !============================================================================
  subroutine multi_ion_init_point_impl

    ! Select variables for point implicit evaluation. This is the union
    ! of the ion momenta and the variables selected (if any) in
    ! ModUser::user_init_point_implicit

    use ModPointImplicit, ONLY: iVarPointImpl_I, IsPointImplMatrixSet, &
         init_point_implicit_num
    use ModUserInterface ! user_init_point_implicit

    logical :: IsPointImpl_V(nVar)
    integer :: iVar, iPointImplVar, nPointImplVar

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'multi_ion_init_point_impl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    IsPointImpl_V = .false.
    IsPointImplMatrixSet = IsAnalyticJacobian

    if(UseUserSourceImpl)then
       call user_init_point_implicit
       if(allocated(iVarPointImpl_I)) then
          IsPointImpl_V(iVarPointImpl_I) = .true.
          ! Set index array for variables to be perturbed
          if(.not.IsPointImplMatrixSet) call init_point_implicit_num
          deallocate(iVarPointImpl_I)
       end if
    end if

    ! All ion momenta are implicit
    IsPointImpl_V(iRhoUxIon_I) = .true.
    IsPointImpl_V(iRhoUyIon_I) = .true.
    IsPointImpl_V(iRhoUzIon_I) = .true.
    ! IsPointImpl_V(iPIon_I)   = .true. !!! No heating in artificial friction

    nPointImplVar = count(IsPointImpl_V)

    allocate(iVarPointImpl_I(nPointImplVar))

    iPointImplVar = 0
    do iVar = 1, nVar
       if(.not. IsPointImpl_V(iVar)) CYCLE
       iPointImplVar = iPointImplVar + 1
       iVarPointImpl_I(iPointImplVar) = iVar
    end do

    call test_stop(NameSub, DoTest)
  end subroutine multi_ion_init_point_impl
  !============================================================================
end module ModMultiIon
!==============================================================================
