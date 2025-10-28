module ModPhysicalFlux

  use ModVarIndexes
  use ModBatsrusUtility, ONLY: stop_mpi

  implicit none

  ! Calculate the physical flux from the input variables.
  ! This is a fully public module. The design is for efficiency.

  public:: get_physical_flux ! Flux for HD and extended MHD

  ! These variables are used or not depending on what equation is solved
  real:: B0x = 0.0, B0y = 0.0, B0z = 0.0
  !$omp threadprivate(B0x, B0y, B0z)

  ! Normal to the face (direction of flux)
  real :: Normal_D(3) = [1.0, 0.0, 0.0]
  !$omp threadprivate(Normal_D)

  ! Variables for normal resistivity
  real :: EtaJx = 0.0, EtaJy = 0.0, EtaJz = 0.0, Eta = 0.0
  !$omp threadprivate(EtaJx, EtaJy, EtaJz, Eta)

  ! Variables needed for Hall resistivity
  real :: HallCoeff = 0.0
  real :: HallJx = 0.0, HallJy = 0.0, HallJz = 0.0
  !$omp threadprivate(HallCoeff, HallJx, HallJy, HallJz)

  ! Variables needed for Biermann battery term
  logical:: UseHallGradPe = .false.
  real:: BiermannCoeff = 0.0
  real:: GradXPeNe = 0.0, GradYPeNe = 0.0, GradZPeNe = 0.0
  !$omp threadprivate(UseHallGradPe)
  !$omp threadprivate(BiermannCoeff, GradXPeNe, GradYPeNe, GradZPeNe)

  ! Variables for diffusion solvers (radiation diffusion, heat conduction)
  real :: DiffCoef = 0.0, EradFlux = 0.0, RadDiffCoef = 0.0
  real :: HeatFlux = 0.0, IonHeatFlux = 0.0, HeatCondCoefNormal = 0.0
  !$omp threadprivate(DiffCoef, EradFlux, RadDiffCoef, HeatFlux, IonHeatFlux)
  !$omp threadprivate(HeatCondCoefNormal)

  ! Variables needed by viscosity
  real :: ViscoCoeff = 0.0
  !$omp threadprivate(ViscoCoeff)

  ! Variables introduced for regional Boris correction
  real :: InvClightFace = 1.0, InvClight2Face = 1.0
  !$omp threadprivate(InvClightFace, InvClight2Face)

  ! Switch to true to show test output
  logical:: DoTestCell = .false.
  !$omp threadprivate(DoTestCell)

  ! Index range for fluids
  integer:: iFluidMin = 1, iFluidMax = nFluid
  !$omp threadprivate(iFluidMin, iFLuidMax)

  ! Indexes of the face
  integer:: iFace=1, jFace=1, kFace=1
  !$omp threadprivate(iFace, jFace, kFace)

  ! Indexes of cells in the negative and positive directions from face
  integer :: iLeft=1,  jLeft=1, kLeft=1
  integer :: iRight=1, jRight=1, kRight=1
  !$omp threadprivate(iLeft, jLeft, kLeft, iRight, jRight, kRight)

  ! Index of the block for this face, iBlockFace = iBlock
  integer :: iBlockFace = 1
  !$omp threadprivate(iBlockFace)

  ! 1D Burgers' equation, works for Hd equations
  logical:: DoBurgers = .false.
  !$acc declare create(DoBurgers)

  ! This is an output of get_physical_flux
  real :: MhdFlux_V(RhoUx_:RhoUz_)
  !$omp threadprivate(MhdFlux_V)

contains
  !============================================================================
  subroutine get_physical_flux(State_V, StateCons_V, Flux_V, Un_I, En)

    ! Calculate physical flux from State_V (contains primitive variables)

    use ModMain, ONLY: UseB, UseHyperbolicDivb, SpeedHyp, UseResistivePlanet
    use ModAdvance, ONLY: &
       nFlux, eFluid_, UseMhdMomentumFlux, UseElectronPressure,  UseEfield, &
       UseIonEntropy, UseElectronEntropy, UseElectronEnergy, &
       UseTotalIonEnergy, UseAnisoPe
    use ModBorisCorrection, ONLY: UseBorisSimple, UseBorisCorrection
    use ModHallResist, ONLY: UseHallResist
    use ModImplicit, ONLY: UseSemiHallResist
    use ModPhysics, ONLY: ElectronPressureRatio, GammaElectron, &
       GammaElectronMinus1, InvGammaElectronMinus1, GammaMinus1, &
       InvGammaMinus1, InvGammaMinus1_I, GammaMinus1_I, &
       C2light, Clight
    use ModViscosity, ONLY: Visco_DDI
    use ModWaves, ONLY: UseAlfvenWaves, AlfvenMinusFirst_, AlfvenMinusLast_, &
       AlfvenPlusFirst_, AlfvenPlusLast_, &
       GammaWave, UseWavePressure, UseWavePressureLtd
    use ModTurbulence, ONLY: IsOnAwRepresentative
    use ModMultiFluid, ONLY: &
         iRhoIon_I, iUxIon_I, iUyIon_I, iUzIon_I, iPIon_I, &
         iRho, iRhoUx, iRhoUy, iRhoUz, iUx, iUy, iUz, iEnergy, iP, &
         IsIon_I, nIonFluid, UseMultiIon, ElectronPerMass_I, select_fluid
    use ModGeometry, ONLY: r_GB
    use ModPUI, ONLY: Pu3_
    use BATL_lib, ONLY: nDim, x_, y_, z_

    real, intent(in) :: State_V(nVar)      ! input primitive state
    real, intent(out):: StateCons_V(nFlux) ! conservative state
    real, intent(out):: Flux_V(nFlux)      ! fluxes for all states
    real, intent(out):: Un_I(nFluid+1)     ! normal velocities
    real, intent(out):: En                 ! normal electric field

    real :: NormalX, NormalY, NormalZ
    real:: Hyp, Bx, By, Bz, FullBx, FullBy, FullBz, Bn, B0n, FullBn, Un, HallUn
    real:: FluxBx, FluxBy, FluxBz, AlfvenSpeed
    real:: FluxViscoX, FluxViscoY, FluxViscoZ
    real:: Pe

    integer:: iVar, iFluid

    ! Calculate conservative state
    character(len=*), parameter:: NameSub = 'get_physical_flux'
    !--------------------------------------------------------------------------
    StateCons_V(1:nVar)  = State_V
    NormalX = Normal_D(1); NormalY = Normal_D(2); NormalZ = Normal_D(3)

    ! Make sure normal electric field is initialized
    En = 0.0

    ! Set magnetic variables
    if(UseB)then
       Bx = State_V(Bx_)
       By = State_V(By_)
       Bz = State_V(Bz_)
       FullBx  = Bx + B0x
       FullBy  = By + B0y
       FullBz  = Bz + B0z
       Bn      = Bx*NormalX  + By*NormalY  + Bz*NormalZ
       B0n     = B0x*NormalX + B0y*NormalY + B0z*NormalZ
       FullBn  = B0n + Bn
    end if

    ! Make sure this is initialized
    HallUn = 0.0

    do iFluid = iFluidMin, iFluidMax

       if(iFluid == 1 .and. (IsMhd .or. UseTotalIonEnergy))then
          ! Calculate MHD flux for first fluid
          if(UseBorisCorrection)then
             call get_boris_flux
          else
             call get_mhd_flux
          end if
       elseif(DoBurgers) then
          call get_burgers_flux
       else
          ! If there is no MHD fluid, calculate fluxes for magnetic field
          ! (and E field) together with hydro fluxes for the first fluid
          if(iFluid == 1 .and. UseB)then
             if(UseEfield)then
                call get_electro_magnetic_flux
                ! Calculate HD flux for first ion fluid
                call select_fluid(1)
                call get_hd_flux
             else
                ! Momentum and energy fluxes now include the electric field
                ! They need to be reassigned to MhdFlux_V accordingly
                call get_mhd_flux
             end if
          else
             ! Calculate HD flux for individual ion and neutral fluids
             call select_fluid(iFluid)
             call get_hd_flux
          end if
       end if

       if(UseResistivePlanet .and. iFluid == 1)then
          ! Do not evolve magnetic field inside the body
          if(r_GB(iLeft,jLeft,kLeft,iBlockFace) < 1.0 .and. &
               r_GB(iRight,jRight,kRight,iBlockFace) < 1.0) &
               Flux_V(Bx_:Bz_) = 0.0
       end if

       ! Store normal velocity (needed for source terms with div U)
       Un_I(iFluid) = Un

    end do

    ! The extra fluxes should be added at the same time as fluid 1 fluxes
    if(iFluidMin /= 1) RETURN

    ! Scalars advect with the first fluid's velocity
    do iVar = ScalarFirst_, ScalarLast_
       Flux_V(iVar) = Un_I(1)*State_V(iVar)
    end do
    ! Overwrite Lperp_ for multi-ion
    if(Lperp_ > 1 .and. UseMultiIon) Flux_V(Lperp_) = HallUn*State_V(Lperp_)
    ! Overwrite levelHP flux for multi-ion
    if(LevelHP_ > 1 .and. UseMultiIon) &
         Flux_V(LevelHP_) = HallUn*State_V(LevelHP_)

    if(PuiFirst_ > 1)then
       ! PUI scalar advect with second fluid's velocity
       do iVar = PuiFirst_, PuiLast_
          Flux_V(iVar) = Un_I(Pu3_)*State_V(iVar)
       end do
    end if

    ! Set flux for electron pressure
    if(UseElectronPressure)then
       ! Set conservative variable to Se
       if(UseElectronEntropy) StateCons_V(Pe_) = &
            State_V(Pe_)*sum(State_V(iRhoIon_I)*ElectronPerMass_I) &
            **(-GammaElectronMinus1)
       ! This is valid both for Pe and Se
       Flux_V(Pe_) = HallUn*StateCons_V(Pe_)

       if (UseAnisoPe) Flux_V(Pepar_) = HallUn*State_V(Pepar_)
    elseif(UseMhdMomentumFlux .and. UseMultiIon &
         .and. ElectronPressureRatio > 0)then
       Pe = sum(State_V(iPIon_I))*ElectronPressureRatio
       MhdFlux_V(RhoUx_) = MhdFlux_V(RhoUx_) + Pe*NormalX
       MhdFlux_V(RhoUy_) = MhdFlux_V(RhoUy_) + Pe*NormalY
       MhdFlux_V(RhoUz_) = MhdFlux_V(RhoUz_) + Pe*NormalZ
    end if

    if(Ehot_ > 1) Flux_V(Ehot_) = HallUn*State_V(Ehot_)

    if(UseAlfvenWaves)then
       if(.not.IsOnAwRepresentative)then
          ! Flux contribution proportional to the Alfven wave speed
          ! is calculated
          AlfvenSpeed = FullBn/sqrt(State_V(iRhoIon_I(1)))

          if(UseMultiIon)then
             do iVar = AlfvenPlusFirst_, AlfvenPlusLast_
                Flux_V(iVar) = (HallUn + AlfvenSpeed)*State_V(iVar)
             end do

             do iVar = AlfvenMinusFirst_, AlfvenMinusLast_
                Flux_V(iVar) = (HallUn - AlfvenSpeed)*State_V(iVar)
             end do
          else
             do iVar = AlfvenPlusFirst_, AlfvenPlusLast_
                Flux_V(iVar) = (Un_I(1) + AlfvenSpeed)*State_V(iVar)
             end do

             do iVar = AlfvenMinusFirst_, AlfvenMinusLast_
                Flux_V(iVar) = (Un_I(1) - AlfvenSpeed)*State_V(iVar)
             end do
          end if
       else
          ! The "representatives" propagate with Un
          do iVar = AlfvenPlusFirst_, AlfvenMinusLast_
             Flux_V(iVar) = Un_I(1)*State_V(iVar)
          end do
       end if
    end if

    if(ViscoCoeff > 0.0)then
       do iFluid = 1, nFluid
          if(nFluid > 1) call select_fluid(iFluid)
          FluxViscoX     = sum(Normal_D(1:nDim)*Visco_DDI(:,x_,iFluid))
          Flux_V(iRhoUx) = Flux_V(iRhoUx) - State_V(iRho)*FluxViscoX
          Flux_V(Energy_)= Flux_V(Energy_) - &
               State_V(iRho)*State_V(iUx)*FluxViscoX
          if(nDim == 1) CYCLE
          FluxViscoY     = sum(Normal_D(1:nDim)*Visco_DDI(:,y_,iFluid))
          Flux_V(iRhoUy) = Flux_V(iRhoUy) - State_V(iRho)*FluxViscoY
          Flux_V(Energy_)= Flux_V(Energy_) - &
               State_V(iRho)*State_V(iUy)*FluxViscoY
          if(nDim == 2) CYCLE
          FluxViscoZ     = sum(Normal_D(1:nDim)*Visco_DDI(:,z_,iFluid))
          Flux_V(iRhoUz) = Flux_V(iRhoUz) - State_V(iRho)*FluxViscoZ
          Flux_V(Energy_)= Flux_V(Energy_) - &
               State_V(iRho)*State_V(iUz)*FluxViscoZ
       end do
    end if

    if(UseB) then
       ! These terms are common for the induction equation
       ! If the first fluid is the total fluid,
       ! the total energy density is also updated
       if(Eta > 0.0)then
          ! Add curl Eta.J to induction equation
          FluxBx = NormalY*EtaJz - NormalZ*EtaJy
          FluxBy = NormalZ*EtaJx - NormalX*EtaJz
          FluxBz = NormalX*EtaJy - NormalY*EtaJx

          Flux_V(Bx_) = Flux_V(Bx_) + FluxBx
          Flux_V(By_) = Flux_V(By_) + FluxBy
          Flux_V(Bz_) = Flux_V(Bz_) + FluxBz

          ! add B.dB/dt term to energy equation
          if(IsMhd .or. UseTotalIonEnergy) Flux_V(Energy_) = Flux_V(Energy_) &
               + Bx*FluxBx + By*FluxBy + Bz*FluxBz
       end if

       if(UseHallGradPe)then
          ! Add curl (-grad Pe/n e) to induction equation
          FluxBx = - (NormalY*GradZPeNe - NormalZ*GradYPeNe)
          FluxBy = - (NormalZ*GradXPeNe - NormalX*GradZPeNe)
          FluxBz = - (NormalX*GradYPeNe - NormalY*GradXPeNe)

          Flux_V(Bx_) = Flux_V(Bx_) + FluxBx
          Flux_V(By_) = Flux_V(By_) + FluxBy
          Flux_V(Bz_) = Flux_V(Bz_) + FluxBz

          ! add B.dB/dt term to energy equation
          if(IsMhd .or. UseTotalIonEnergy) Flux_V(Energy_) = Flux_V(Energy_) &
               + Bx*FluxBx + By*FluxBy + Bz*FluxBz
       end if

       if(UseHyperbolicDivb)then
          Hyp  = State_V(Hyp_)

          Flux_V(Bx_:Bz_) = Flux_V(Bx_:Bz_) + SpeedHyp*Normal_D*Hyp
          Flux_V(Hyp_)    = SpeedHyp*Bn

          if(IsMhd .or. UseTotalIonEnergy) &
               Flux_V(Energy_) = Flux_V(Energy_) + SpeedHyp*Bn*Hyp
       elseif(Hyp_ > 1)then
          Flux_V(Hyp_) = 0.0
       end if
    end if

    if(EradFlux /= 0) Flux_V(Erad_) = Flux_V(Erad_) + EradFlux
    if(HeatFlux /= 0)then
       if(UseElectronPressure)then
          if(UseElectronEntropy) call stop_mpi(NameSub// &
               ' heat conduction for electron entropy is not implemented')
          Flux_V(Pe_) = Flux_V(Pe_) + GammaElectronMinus1*HeatFlux
       else
          Flux_V(p_) = Flux_V(p_) + GammaMinus1*HeatFlux
          Flux_V(Energy_) = Flux_V(Energy_) + HeatFlux
       end if
    end if
    if(IonHeatFlux /= 0.0)then
       Flux_V(p_) = Flux_V(p_) + GammaMinus1*IonHeatFlux
       Flux_V(Energy_) = Flux_V(Energy_) + IonHeatFlux
    end if

    ! Set the normal electron velocity used for Hall MHD and/or
    ! the electron pressure source term
    Un_I(eFluid_) = HallUn

  contains
    !==========================================================================
    subroutine get_boris_flux

      use ModAdvance, ONLY: UseElectronPressure, UseAnisoPressure, UseAnisoPe

      ! Variables for conservative state and flux calculation
      real :: Rho, Ux, Uy, Uz, p, e, PeAdd
      real :: B2, FullB2, pTotal, pTotal2, uDotB, DpPerB
      real :: Ex, Ey, Ez, E2Half, InvClight2Face
      !------------------------------------------------------------------------
      Rho     = State_V(Rho_)
      Ux      = State_V(Ux_)
      Uy      = State_V(Uy_)
      Uz      = State_V(Uz_)
      p       = State_V(p_)
      InvClight2Face = InvClightFace**2

      ! For isotropic Pe, Pe contributes the ion momentum eqn, while for
      ! anisotropic Pe, Peperp contributes
      if (UseElectronPressure .and. .not. UseAnisoPe) then
         PeAdd = State_V(Pe_)
      else if (UseAnisoPe) then
         ! Peperp = (3*pe - Pepar)/2
         PeAdd = (3*State_V(Pe_) - State_V(Pepar_))/2.0
      end if

      B2      = Bx**2 + By**2 + Bz**2

      ! Electric field divided by speed of light:
      ! E= - U x B / c = (B x U)/c
      Ex      = (FullBy*Uz - FullBz*Uy) * InvClightFace
      Ey      = (FullBz*Ux - FullBx*Uz) * InvClightFace
      Ez      = (FullBx*Uy - FullBy*Ux) * InvClightFace

      ! Electric field squared/c^2
      E2Half  = 0.5*(Ex**2 + Ey**2 + Ez**2)

      ! Calculate energy and total pressure
      e = InvGammaMinus1*p + 0.5*(Rho*(Ux**2 + Uy**2 + Uz**2) + B2)

      pTotal  = 0.5*B2 + B0x*Bx + B0y*By + B0z*Bz

      if(UseElectronPressure) pTotal = pTotal + PeAdd

      if(UseWavePressure)then
         if(UseWavePressureLtd)then
            pTotal = pTotal + (GammaWave-1)*State_V(Ew_)
         else
            pTotal = pTotal + (GammaWave-1)*sum(State_V(WaveFirst_:WaveLast_))
         end if
      end if

      ! pTotal = pperp + bb/2 = 3/2*p - 1/2*ppar + bb/2
      !        = p + bb/2 + (p - ppar)/2
      if(UseAnisoPressure) pTotal = pTotal + 0.5*(p - State_V(Ppar_))

      pTotal2 = pTotal + E2Half

      ! The full momentum contains the ExB/c^2 term:
      ! rhoU_Boris = rhoU - ((U x B) x B)/c^2 = rhoU + (U B^2 - B U.B)/c^2
      uDotB   = Ux*FullBx + Uy*FullBy + Uz*FullBz
      FullB2  = FullBx**2 + FullBy**2 + FullBz**2
      StateCons_V(RhoUx_)  = Rho*Ux + (Ux*FullB2 - FullBx*uDotB)*InvClight2Face
      StateCons_V(RhoUy_)  = Rho*Uy + (Uy*FullB2 - FullBy*uDotB)*InvClight2Face
      StateCons_V(RhoUz_)  = Rho*Uz + (Uz*FullB2 - FullBz*uDotB)*InvClight2Face

      ! The full energy contains the electric field energy
      StateCons_V(Energy_) = e + E2Half

      ! Normal direction
      Un     = Ux*NormalX + Uy*NormalY + Uz*NormalZ
      En     = Ex*NormalX + Ey*NormalY + Ez*NormalZ

      ! f_i[rho] = rho*u_i
      Flux_V(Rho_)   = Rho*Un

      ! f_i[rhou_k] = u_i*u_k*rho - b_k*b_i - B0_k*b_i - B0_i*b_k - E_i*E_k
      !          +n_i*[p + B0_j*b_j + 0.5*(b_j*b_j + E_j*E_j)]
      Flux_V(RhoUx_) = Un*Rho*Ux + p*NormalX
      Flux_V(RhoUy_) = Un*Rho*Uy + p*NormalY
      Flux_V(RhoUz_) = Un*Rho*Uz + p*NormalZ

      MhdFlux_V(RhoUx_) = &
           - Bn*FullBx - B0n*Bx - En*Ex + pTotal2*Normalx
      MhdFlux_V(RhoUy_) = &
           - Bn*FullBy - B0n*By - En*Ey + pTotal2*Normaly
      MhdFlux_V(RhoUz_) = &
           - Bn*FullBz - B0n*Bz - En*Ez + pTotal2*Normalz
      Flux_V(RhoUx_:RhoUz_) = Flux_V(RhoUx_:RhoUz_) + MhdFlux_V

      pTotal = p + pTotal
      ! f_i[b_k]=u_i*(b_k+B0_k) - u_k*(b_i+B0_i)
      Flux_V(Bx_) = Un*FullBx - Ux*FullBn
      Flux_V(By_) = Un*FullBy - Uy*FullBn
      Flux_V(Bz_) = Un*FullBz - Uz*FullBn

      ! f_i[e]=(u_i*(ptotal+e+(b_k*B0_k))-(b_i+B0_i)*(b_k*u_k))
      Flux_V(Energy_) = &
           Un*(pTotal + e) - FullBn*(Ux*Bx + Uy*By + Uz*Bz)

      if(UseAnisoPressure)then
         ! f_i[rhou_k] = f_i[rho_k] + (ppar - pperp)bb for anisopressure
         ! ppar - pperp = ppar - (3*p - ppar)/2 = 3/2*(ppar - p)
         if (.not. UseAnisoPe) then
            ! In isotropic electron case, no electron contributions
            DpPerB = 1.5*(State_V(Ppar_) - p)*FullBn/max(1e-30, FullB2)
         else
            ! In anisotropic electron case, only (Pepar - Pperp) contributes
            DpPerB = 1.5*(State_V(Ppar_) + State_V(Pepar_) &
                 - p - State_V(Pe_))*FullBn/max(1e-30, FullB2)
         end if
         Flux_V(RhoUx_) = Flux_V(RhoUx_) + FullBx*DpPerB
         Flux_V(RhoUy_) = Flux_V(RhoUy_) + FullBy*DpPerB
         Flux_V(RhoUz_) = Flux_V(RhoUz_) + FullBz*DpPerB
         ! f_i[Ppar] = u_i*Ppar
         if(UseIonEntropy) then
            ! Parallel entropy Spar = Ppar * B^2/rho^2
            StateCons_V(Ppar_) = State_V(Ppar_)*FullB2/Rho**2
            ! Perpendicular entropy Sperp = Pperp/FullB
            StateCons_V(p_) = 0.5*(3*p - State_V(Ppar_)) &
                 /sqrt(max(1e-30, FullB2))
         end if
         Flux_V(Ppar_)   = Un*StateCons_V(Ppar_)
         Flux_V(p_)      = Un*StateCons_V(p_)
         Flux_V(Energy_) = Flux_V(Energy_) &
              + DpPerB*(Ux*FullBx + Uy*FullBy + Uz*FullBz)
      else if(UseIonEntropy) then
         ! s = p/rho^g-1
         StateCons_V(p_) = p*Rho**(-GammaMinus1)
         ! u_i * s
         Flux_V(p_)  = Un*StateCons_V(p_)
      else
         ! f_i[p]=u_i*p
         Flux_V(p_)  = Un*p
      end if

      HallUn = Un

    end subroutine get_boris_flux
    !==========================================================================
    subroutine get_magnetic_flux(State_V, Flux_V, &
         FullBx, FullBy, FullBz, FullBn, HallUn, UxPlus, UyPlus, UzPlus)

      real, intent(in) :: State_V(:)
      real, intent(inout) :: Flux_V(:)
      real, intent(in) :: FullBx, FullBy, FullBz, FullBn
      real, intent(inout) :: HallUn, UxPlus, UyPlus, UzPlus

      ! Calculate magnetic flux for multi-ion equations
      ! without a global ion fluid

      real :: ElectronDens_I(nIonFluid), InvElectronDens
      real :: UnPlus
      real :: HallUx, HallUy, HallUz, InvRho
      !------------------------------------------------------------------------
      if(UseMultiIon)then
         ! calculate number densities
         ElectronDens_I  = ElectronPerMass_I*State_V(iRhoIon_I)
         InvElectronDens = 1/sum(ElectronDens_I)

         ! calculate positive charge velocity
         UxPlus = InvElectronDens*sum(ElectronDens_I*State_V(iUxIon_I))
         UyPlus = InvElectronDens*sum(ElectronDens_I*State_V(iUyIon_I))
         UzPlus = InvElectronDens*sum(ElectronDens_I*State_V(iUzIon_I))
      else
         UxPlus = State_V(Ux_)
         UyPlus = State_V(Uy_)
         UzPlus = State_V(Uz_)
      end if

      UnPlus = UxPlus*NormalX + UyPlus*NormalY + UzPlus*NormalZ

      if(HallCoeff > 0.0)then
         ! The ion mass per charge that is contained in HallCoef
         ! (and HallJ*) is normalized to be divided with the total
         ! mass density.
         InvRho = 1/sum(State_V(iRhoIon_I))
         HallUx = UxPlus - HallJx*InvRho
         HallUy = UyPlus - HallJy*InvRho
         HallUz = UzPlus - HallJz*InvRho
         HallUn = NormalX*HallUx + NormalY*HallUy + NormalZ*HallUz
      else
         HallUn = UnPlus
      end if

      if(HallCoeff > 0.0 .and. UseHallResist .and. .not. UseSemiHallResist)then
         Flux_V(Bx_) = HallUn*FullBx - HallUx*FullBn
         Flux_V(By_) = HallUn*FullBy - HallUy*FullBn
         Flux_V(Bz_) = HallUn*FullBz - HallUz*FullBn
      else
         Flux_V(Bx_) = UnPlus*FullBx - UxPlus*FullBn
         Flux_V(By_) = UnPlus*FullBy - UyPlus*FullBn
         Flux_V(Bz_) = UnPlus*FullBz - UzPlus*FullBn
      end if

      if(DoTestCell)then
         write(*,*)'ElectronDens_I, InvElectronDens=', &
              ElectronDens_I, InvElectronDens
         write(*,*)'UxyzPlus  =',UxPlus, UyPlus, UzPlus
         if(HallCoeff > 0.0)then
            write(*,*)'InvRho    =', InvRho
            write(*,*)'HallUxyz  =', HallUx, HallUy, HallUz
         end if
         write(*,*)'FullBxyz  =', FullBx, FullBy, FullBz
         write(*,*)'B0x,y,z   =', B0x, B0y, B0z
         write(*,*)'Flux(Bxyz)=', Flux_V(Bx_:Bz_)
      end if

    end subroutine get_magnetic_flux
    !==========================================================================
    subroutine get_mhd_flux

      use ModElectricField, ONLY: UseJCrossBForce
      use ModAdvance, ONLY: UseElectronPressure, UseAnisoPressure, UseAnisoPe
      use ModTurbulence, ONLY: UseReynoldsDecomposition, SigmaD, &
           UseTransverseTurbulence,  PoyntingFluxPerB, IsOnAwRepresentative

      ! Variables for conservative state and flux calculation
      real :: Rho, Ux, Uy, Uz, p, Pe, e
      real :: pPerp    ! in anisotropic case is not the same as p
      real :: pWave    ! Contribution from waves to pressure
      real :: pExtra   ! Electrons and waves act on ions via electr.field
      real :: SqrtRho  ! Square root of density times PoyntingFluxPerB
      real :: B2, B0B1, FullB2, pTotal, DpPerB
      real :: Gamma2
      real :: UxPlus, UyPlus, UzPlus
      real, dimension(nIonFluid):: Ux_I, Uy_I, Uz_I, p_I, e_I

      ! Energy difference (in the standard argo, Rho*sigma_D*Z^2/2
      real :: wD

      real :: MagneticForce_D(RhoUx_:RhoUz_)
      ! Extract primitive variables
      !------------------------------------------------------------------------
      Rho     = State_V(Rho_)
      Ux      = State_V(Ux_)
      Uy      = State_V(Uy_)
      Uz      = State_V(Uz_)
      p       = State_V(p_)
      if(UseElectronPressure) Pe = State_V(Pe_)

      ! A factor to convert representative functions to a real wave energy
      if(IsOnAwRepresentative)SqrtRho = sqrt(Rho)*PoyntingFluxPerB
      ! Hydrodynamic part of fluxes

      ! Normal direction
      Un = Ux*NormalX  + Uy*NormalY  + Uz*NormalZ

      ! f_n[rho] = Rho*U_i
      Flux_V(Rho_) = Rho*Un

      ! pTotal = pperp + bb/2 = 3/2*p - 1/2*ppar + bb/2
      !        = p + bb/2 + (p - ppar)/2
      if(UseAnisoPressure)then
         ! Perp = (3*p - Ppar)/2
         pPerp = 1.5*p - 0.5*State_V(Ppar_)
      else
         ! Isotropic case
         pPerp = p
      end if

      call get_magnetic_flux(State_V, Flux_V, &
           FullBx, FullBy, FullBz, FullBn, HallUn, UxPlus, UyPlus, UzPlus)

      ! Calculate conservative state for momentum
      StateCons_V(RhoUx_)  = Rho*Ux
      StateCons_V(RhoUy_)  = Rho*Uy
      StateCons_V(RhoUz_)  = Rho*Uz

      ! Calculate momentum flux, starting from hydro part
      ! f_n[rhou_k] = u_n*u_k*rho - b_n*(b_k + B0_k) - B0_n*b_k
      !               + Ptotal*n_k
      Flux_V(RhoUx_) = Un*Rho*Ux + pPerp*NormalX
      Flux_V(RhoUy_) = Un*Rho*Uy + pPerp*NormalY
      Flux_V(RhoUz_) = Un*Rho*Uz + pPerp*NormalZ

      ! Calculate hydrodynamic energy density, including electron energy
      if(UseTotalIonEnergy)then
         Ux_I = State_V(iUxIon_I)
         Uy_I = State_V(iUyIon_I)
         Uz_I = State_V(iUzIon_I)
         p_I  = State_V(iPIon_I)
         ! Hydro energies
         e_I = InvGammaMinus1_I(1:nIonFluid)*p_I &
              + 0.5*State_V(iRhoIon_I)*(Ux_I**2 + Uy_I**2 + Uz_I**2)
         ! Total hydro energy
         e = sum(e_I)
      else
         e = InvGammaMinus1*p + 0.5*Rho*(Ux**2 + Uy**2 + Uz**2)
      end if

      ! Calculate hydro energy flux
      if(UseTotalIonEnergy)then
         ! Is this correct for anisotropic case???
         Flux_V(Energy_) = sum( &
              (Ux_I*NormalX + Uy_I*NormalY + Uz_I*NormalZ)*(e_I + p_I))
      else
         Flux_V(Energy_) = Un*(e + pPerp)
      end if

      ! Add electron energy if included
      if(UseElectronEnergy) e = e + InvGammaElectronMinus1*Pe
      StateCons_V(Energy_) = e

      ! Correct momentum and energy hydro fluxes for anisotroic pressure
      if(UseAnisoPressure)then
         if (DoTestCell) then
            write(*,*) NameSub, ' before aniso flux:'
            write(*,*) ' Flux_V(RhoUx_) =', Flux_V(RhoUx_)
            write(*,*) ' Flux_V(RhoUy_) =', Flux_V(RhoUy_)
            write(*,*) ' Flux_V(RhoUz_) =', Flux_V(RhoUz_)
         end if

         ! f_i[rhou_k] = f_i[rho_k] + (ppar - pperp)bb for anisopressure
         ! ppar - pperp = ppar - (3*p - ppar)/2 = 3/2*(ppar - p)
         FullB2 = FullBx**2 + FullBy**2 + FullBz**2
         DpPerB = 1.5*(State_V(Ppar_) - p)*FullBn/max(1e-30, FullB2)
         Flux_V(RhoUx_) = Flux_V(RhoUx_) + FullBx*DpPerB
         Flux_V(RhoUy_) = Flux_V(RhoUy_) + FullBy*DpPerB
         Flux_V(RhoUz_) = Flux_V(RhoUz_) + FullBz*DpPerB
         Flux_V(Energy_)= Flux_V(Energy_) &
              + DpPerB*(Ux*FullBx + Uy*FullBy + Uz*FullBz)
         if(UseIonEntropy)then
            ! Parallel entropy Spar = Ppar * B^2/rho^2
            StateCons_V(Ppar_) = State_V(Ppar_)*FullB2/Rho**2
            ! Perpendicular entropy Sperp = Pperp/FullB
            StateCons_V(p_) = 0.5*(3*p - State_V(Ppar_)) &
                 /sqrt(max(1e-30, FullB2))
         end if
         ! f_i[Ppar] = u_i*Ppar or f_i[Spar] = u_i*Spar
         Flux_V(Ppar_)  = Un*StateCons_V(Ppar_)
         ! f_i[p] = u_i*p or f_i[s] = u_i*s
         Flux_V(p_)     = Un*StateCons_V(p_)

         if(DoTestCell)then
            write(*,*) NameSub, ' after aniso flux:'
            write(*,*) 'DpPerB  =', DpPerB
            write(*,*) 'FullBx  =', FullBx
            write(*,*) 'FullBy  =', FullBy
            write(*,*) 'FullBz  =', FullBz
            write(*,*) 'Flux_V(RhoUx_) =', Flux_V(RhoUx_)
            write(*,*) 'Flux_V(RhoUy_) =', Flux_V(RhoUy_)
            write(*,*) 'Flux_V(RhoUz_) =', Flux_V(RhoUz_)
         end if
      else if(UseIonEntropy)then
         ! s = p/rho^(g-1)
         StateCons_V(p_) = p*Rho**(-GammaMinus1)
         ! u_i * s
         Flux_V(p_)  = Un*StateCons_V(p_)
      else
         ! f_n[p] = u_n*p
         Flux_V(p_) = Un*p
      end if
      ! MHD part
      ! Add contribution from magnetic stress, electron and wave pressure
      pExtra = 0.0
      ! For isotropic Pe, Pe contributes the ion momentum eqn, while for
      ! anisotropic Pe, Peperp contributes
      if (UseElectronPressure) then
         if (UseAnisoPe) then
            ! Peperp = (3*pe - Pepar)/2
            pExtra = pExtra + 1.5*State_V(Pe_) - 0.5*State_V(Pepar_)
         else
            pExtra = pExtra + Pe
         end if
      end if
      if(UseWavePressure)then
         if(UseWavePressureLtd)then
            pWave = (GammaWave-1)*State_V(Ew_)
         else
            pWave = (GammaWave-1)*sum(State_V(WaveFirst_:WaveLast_))
         end if
         if(UseReynoldsDecomposition)then
            if(WDiff_ > 1)then
               wD = State_V(WDiff_)
            else
               wD = SigmaD*sum(State_V(WaveFirst_:WaveLast_))
            end if
            if(UseTransverseTurbulence)then
               pWave = pWave + (GammaWave - 1)*wD
            else
               pWave = pWave + (GammaWave - 1)*wD/3
            end if
         end if
         ! Convert representative functions to a real wave pressure if needed
         if(IsOnAwRepresentative) pWave = SqrtRho*pWave
         pExtra = pExtra + pWave
      end if
      ! Calculate some intermediate values for flux calculations
      B2      = Bx*Bx + By*By + Bz*Bz
      B0B1    = B0x*Bx + B0y*By + B0z*Bz
      pTotal  = 0.5*B2 + B0B1
      ! Magnetic force
      MagneticForce_D(RhoUx_) =  -Bn*FullBx - B0n*Bx + pTotal*NormalX
      MagneticForce_D(RhoUy_) =  -Bn*FullBy - B0n*By + pTotal*NormalY
      MagneticForce_D(RhoUz_) =  -Bn*FullBz - B0n*Bz + pTotal*NormalZ
      ! Add a gradient of extra pressure to momentum flux
      MhdFlux_V(RhoUx_) = pExtra*NormalX
      MhdFlux_V(RhoUy_) = pExtra*NormalY
      MhdFlux_V(RhoUz_) = pExtra*NormalZ
      if(.not.UseJCrossBForce) MhdFlux_V = MhdFlux_V + MagneticForce_D
      ! Correction for anisotropic electron pressure
      if(UseAnisoPe)then
         if (DoTestCell) then
            write(*,*) NameSub, ' before anisoPe flux:'
            write(*,*) ' Flux_V(RhoUx_) =', MhdFlux_V(RhoUx_)
            write(*,*) ' Flux_V(RhoUy_) =', MhdFlux_V(RhoUy_)
            write(*,*) ' Flux_V(RhoUz_) =', MhdFlux_V(RhoUz_)
         end if

         ! f_i[rhou_k] = f_i[rho_k] + (ppar - pperp)bb for anisopressure
         ! ppar - pperp = ppar - (3*p - ppar)/2 = 3/2*(ppar - p)
         ! In anisotropic electron case, only (Pepar - Pperp) contributes
         DpPerB = 1.5*(State_V(Pepar_) - Pe)*FullBn&
              /max(1e-30, FullB2)

         MhdFlux_V(RhoUx_) = MhdFlux_V(RhoUx_) + FullBx*DpPerB
         MhdFlux_V(RhoUy_) = MhdFlux_V(RhoUy_) + FullBy*DpPerB
         MhdFlux_V(RhoUz_) = MhdFlux_V(RhoUz_) + FullBz*DpPerB
         if(IsMhd) Flux_V(Energy_) = Flux_V(Energy_) &
              + DpPerB*(Ux*FullBx + Uy*FullBy + Uz*FullBz)
         ! Don't we need Flux_V(PePar_)?
         if(DoTestCell)then
            write(*,*) NameSub, ' after anisoPe flux:'
            write(*,*) 'DpPerB(pe)  =', DpPerB
            write(*,*) 'FullBx      =', FullBx
            write(*,*) 'FullBy      =', FullBy
            write(*,*) 'FullBz      =', FullBz
            write(*,*) 'Flux_V(RhoUx_) =', MhdFlux_V(RhoUx_)
            write(*,*) 'Flux_V(RhoUy_) =', MhdFlux_V(RhoUy_)
            write(*,*) 'Flux_V(RhoUz_) =', MhdFlux_V(RhoUz_)
         end if
      end if

      if(UseWavePressure .and. UseReynoldsDecomposition &
           .and. UseTransverseTurbulence)then
         FullB2 = FullBx**2 + FullBy**2 + FullBz**2
         DpPerB = -wD*FullBn/max(1e-30, FullB2)
         ! Convert representative functions to a real wave pressure if needed
         if(IsOnAwRepresentative)DpPerB = DpPerB*SqrtRho
         MhdFlux_V(RhoUx_) = MhdFlux_V(RhoUx_) + FullBx*DpPerB
         MhdFlux_V(RhoUy_) = MhdFlux_V(RhoUy_) + FullBy*DpPerB
         MhdFlux_V(RhoUz_) = MhdFlux_V(RhoUz_) + FullBz*DpPerB
         if(IsMhd .or. UseTotalIonEnergy) Flux_V(Energy_)= Flux_V(Energy_) &
              + DpPerB*(UxPlus*FullBx + UyPlus*FullBy + UzPlus*FullBz)
      end if

      ! Check if magnetic force and energy should be included at all
      if(.not.IsMhd .and. .not.UseTotalIonEnergy) RETURN

      if(IsMhd)then
         ! This is only valid for single ion fluid
         Flux_V(RhoUx_:RhoUz_) = Flux_V(RhoUx_:RhoUz_) + MhdFlux_V
         if(UseJCrossBForce) Flux_V(RhoUx_:RhoUz_) = &
              Flux_V(RhoUx_:RhoUz_) + MagneticForce_D
      end if

      ! Add magnetic energy
      StateCons_V(Energy_) = e + 0.5*B2

      ! f_i[e]=(u_i*(ptotal + e + (n x E) . B) since div(E x B) = curl(E) . B
      Flux_V(Energy_) = Flux_V(Energy_) & ! hydro energy flux
           + Flux_V(Bx_)*Bx + Flux_V(By_)*By + Flux_V(Bz_)*Bz ! Poynting flux

      ! The contribution of electron and wave pressures to the energy flux
      if( (UseElectronPressure .or. UseWavePressure) .and. iFluid == 1) then
         ! The following is not OK for single ion with Hall term and turbulence
         Flux_V(Energy_) = Flux_V(Energy_) + HallUn*pExtra
         ! Add Ue*Pe/(ge-1) if electron energy is included
         if(UseElectronEnergy) Flux_V(Energy_) = Flux_V(Energy_) &
             + HallUn*Pe*InvGammaElectronMinus1
      end if

      if(UseBorisSimple)then
         ! Correct the momentum using the (1+VA2/c^2)
         Gamma2 = 1 + (FullBx**2 + FullBy**2 + FullBz**2)/Rho*InvClightFace**2
         StateCons_V(RhoUx_:RhoUz_) = StateCons_V(RhoUx_:RhoUz_)*Gamma2
         if(UseAlfvenWaves) StateCons_V(WaveFirst_:WaveLast_) = &
              StateCons_V(WaveFirst_:WaveLast_)*Gamma2
      end if

    end subroutine get_mhd_flux
    !==========================================================================
    subroutine get_electro_magnetic_flux

      real :: Ex, Ey, Ez
      !------------------------------------------------------------------------
      Ex = State_V(Ex_); Ey = State_V(Ey_); Ez = State_V(Ez_)

      ! dB/dt + div F = 0
      ! div F = curl E = sum(Normal x E)/Volume
      Flux_V(Bx_) = NormalY*Ez - NormalZ*Ey
      Flux_V(By_) = NormalZ*Ex - NormalX*Ez
      Flux_V(Bz_) = NormalX*Ey - NormalY*Ex

      ! dE/dt + c^2(J - curl B) = 0   (curl B0 is assumed to be zero for now)
      Flux_V(Ex_) = -C2light*(NormalY*Bz - NormalZ*By)
      Flux_V(Ey_) = -C2light*(NormalZ*Bx - NormalX*Bz)
      Flux_V(Ez_) = -C2light*(NormalX*By - NormalY*Bx)

      ! dE/dt + c*grad PhiE ...
      Flux_V(Ex_:Ez_) = Flux_V(Ex_:Ez_) + Clight*Normal_D*State_V(HypE_)

      ! Flux part of dPhiE/dt + c*(div E - chargedensity/eps0)
      Flux_V(HypE_) = Clight*(Ex*NormalX  + Ey*NormalY  + Ez*NormalZ)

      if(DoTestCell)then
         write(*,'(a,99es13.5)')'ElectronDens_I  =', &
              ElectronPerMass_I*State_V(iRhoIon_I)
         write(*,'(a,3es13.5)') 'Normal_D        =', Normal_D
         write(*,'(a,3es13.5)') 'Bx,By,Bz        =', Bx,By,Bz
         write(*,'(a,3es13.5)') 'Ex,Ey,Ez        =', Ex,Ey,Ez
         write(*,'(a,3es13.5)') 'Flux_V(Bx_:Bz_) =', Flux_V(Bx_:Bz_)
         write(*,'(a,3es13.5)') 'Flux_V(Ex_:Ez_) =', Flux_V(Ex_:Ez_)
         write(*,'(a, es13.5)') 'State_V(HypE_)  =', State_V(HypE_)
         write(*,'(a, es13.5)') 'Flux_V(HypE_)   =', Flux_V(HypE_)
      end if

    end subroutine get_electro_magnetic_flux
    !==========================================================================
    subroutine get_hd_flux

      use ModAdvance, ONLY: UseElectronPressure, UseAnisoPressure, UseAnisoPe
      use ModMultiFluid, ONLY: iPpar
      use ModTurbulence, ONLY: UseReynoldsDecomposition, &
           UseTransverseTurbulence, SigmaD, PoyntingFluxPerB

      ! Variables for conservative state and flux calculation
      real :: Rho, Ux, Uy, Uz, p, e, RhoUn, pTotal, PeAdd, pWave, wD, SqrtRho
      real :: DpPerB, FullB2
      ! Extract primitive variables
      !------------------------------------------------------------------------
      Rho = State_V(iRho)
      Ux  = State_V(iUx)
      Uy  = State_V(iUy)
      Uz  = State_V(iUz)
      p   = State_V(iP)
      ! A factor to convert representative functions to a real wave energy
      if(IsOnAwRepresentative)SqrtRho = sqrt(Rho)*PoyntingFluxPerB
      ! For isotropic Pe, Pe contributes the ion momentum eqn, while for
      ! anisotropic Pe, Peperp contributes
      if (UseElectronPressure .and. .not. UseAnisoPe) then
         PeAdd = State_V(Pe_)
      elseif (UseAnisoPe) then
         ! Peperp = (3*pe - Pepar)/2
         PeAdd = (3*Pe - State_V(Pepar_))/2.0
      end if

      ! Calculate energy
      e = InvGammaMinus1_I(iFluid)*p + 0.5*Rho*(Ux**2 + Uy**2 + Uz**2)

      pTotal = p

      if(nIonFluid == 1 .and. iFluid == 1)then
         if(UseElectronPressure) pTotal = pTotal + PeAdd

         if(UseWavePressure)then
            if(UseWavePressureLtd)then
               pWave = (GammaWave-1)*State_V(Ew_)
            else
               pWave = (GammaWave-1)*sum(State_V(WaveFirst_:WaveLast_))
            end if
            if(UseReynoldsDecomposition)then
               if(WDiff_ > 1)then
                  wD = State_V(WDiff_)
               else
                  wD = SigmaD*sum(State_V(WaveFirst_:WaveLast_))
               end if
               if(UseTransverseTurbulence)then
                  pWave = pWave + (GammaWave-1)*wD
               else
                  pWave = pWave + (GammaWave-1)*wD/3
               end if
            end if
            if(IsOnAwRepresentative)pWave = pWave*SqrtRho
            pTotal = pTotal + pWave
         end if
      end if

      ! pTotal = pperp = 3/2*p - 1/2*ppar = p + (p - ppar)/2
      ! This also works if UseAnisoPe = T because only pperp contributes.
      ! In multi-ion case, there should be some corrections to the source
      ! terms (ModMultiIon) due to anisotropic electron pressure.
      if(UseAnisoPressure .and. IsIon_I(iFluid)) &
           pTotal = pTotal + 0.5*(p - State_V(iPpar))

      ! Calculate conservative state
      StateCons_V(iRhoUx)  = Rho*Ux
      StateCons_V(iRhoUy)  = Rho*Uy
      StateCons_V(iRhoUz)  = Rho*Uz
      StateCons_V(iEnergy) = e

      ! Normal velocity
      Un    = Ux*NormalX  + Uy*NormalY  + Uz*NormalZ
      RhoUn = Rho*Un

      ! f_i[rho] = rho*u_i
      Flux_V(iRho) = RhoUn

      ! f_i[rhou_k] = u_i*rho*u_k + n_i*[ptotal]
      Flux_V(iRhoUx) = RhoUn*Ux + pTotal*NormalX
      Flux_V(iRhoUy) = RhoUn*Uy + pTotal*NormalY
      Flux_V(iRhoUz) = RhoUn*Uz + pTotal*NormalZ

      Flux_V(iEnergy) = Un*(pTotal + e)

      if(UseAnisoPressure .and. IsIon_I(iFluid))then
         if (DoTestCell) then
            write(*,*) NameSub, ' before aniso flux:'
            write(*,*) 'p, PeAdd, pTotal =', p, PeAdd, pTotal
            write(*,*) 'Flux_V(RhoUx_)   =', Flux_V(RhoUx_)
            write(*,*) 'Flux_V(RhoUy_)   =', Flux_V(RhoUy_)
            write(*,*) 'Flux_V(RhoUz_)   =', Flux_V(RhoUz_)
         end if

         ! f_i[rhou_k] = f_i[rho_k] + (ppar - pperp)bb for anisopressure
         ! ppar - pperp = ppar - (3*p - ppar)/2 = 3/2*(ppar - p)
         FullB2 = FullBx**2 + FullBy**2 + FullBz**2
         DpPerB = 1.5*(State_V(iPpar) - p)*FullBn/max(1e-30, FullB2)

         Flux_V(iRhoUx) = Flux_V(iRhoUx) + FullBx*DpPerB
         Flux_V(iRhoUy) = Flux_V(iRhoUy) + FullBy*DpPerB
         Flux_V(iRhoUz) = Flux_V(iRhoUz) + FullBz*DpPerB

         ! f_i[Ppar] = u_i*Ppar
         if(UseIonEntropy)then
            ! Parallel entropy Spar = Ppar * B^2/rho^2
            StateCons_V(iPpar) = State_V(iPpar)*FullB2/Rho**2
            ! Perpendicular entropy Sperp = Pperp/FullB
            StateCons_V(iP) = 0.5*(3*p - State_V(iPpar)) &
                 /sqrt(max(1e-30, FullB2))
         end if
         Flux_V(iPpar) = Un*StateCons_V(iPpar)
         Flux_V(iP)    = Un*StateCons_V(iP)

         Flux_V(iEnergy) = Flux_V(iEnergy) &
              + DpPerB*(Ux*FullBx + Uy*FullBy + Uz*FullBz)

         if (DoTestCell) then
            write(*,*) NameSub, ' after aniso flux:'
            write(*,*) 'DpPerB =', DpPerB
            write(*,*) 'FullBx =', FullBx*DpPerB
            write(*,*) 'FullBy =', FullBy*DpPerB
            write(*,*) 'FullBz =', FullBz*DpPerB
            write(*,*) 'Flux_V(RhoUx_) =', Flux_V(RhoUx_)
            write(*,*) 'Flux_V(RhoUy_) =', Flux_V(RhoUy_)
            write(*,*) 'Flux_V(RhoUz_) =', Flux_V(RhoUz_)
         end if
      elseif(UseIonEntropy .and. IsIon_I(iFluid))then
         ! s = p*rho^(g-1)
         StateCons_V(iP) = p*Rho**(-GammaMinus1_I(iFluid))
         ! u_i * s
         Flux_V(iP)  = Un*StateCons_V(iP)
      else
         ! f_i[p]=u_i*p
         Flux_V(iP)  = Un*p
      end if

      ! Needed for adiabatic source term for electron pressure
      if(iFluid == 1 .and. .not.UseB) HallUn = Un

    end subroutine get_hd_flux
    !==========================================================================
    subroutine get_burgers_flux
      !------------------------------------------------------------------------
      Un = sum(State_V(iUx:iUz)*Normal_D)
      Flux_V = 0.0
      Flux_V(iRho) = 0.5*State_V(iRho)**2 * Un

    end subroutine get_burgers_flux
    !==========================================================================
  end subroutine get_physical_flux
  !============================================================================
end module ModPhysicalFlux
!==============================================================================
