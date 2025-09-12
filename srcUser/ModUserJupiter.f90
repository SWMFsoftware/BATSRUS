!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModUser

  use BATL_lib, ONLY: iProc, iTest, jTest, kTest, iProcTest, iBlockTest, &
       test_start, test_stop, lVerbose

  use ModVarIndexes, ONLY: rho_, Ux_, Uy_, Uz_, p_, Bx_, By_, Bz_, Energy_, &
       rhoUx_, rhoUy_, rhoUz_

  use ModSize, ONLY: nI, nJ, nK, nBlock, MaxBlock
  use ModPhysics, ONLY: Si2No_V, No2Si_V, &
       UnitN_, UnitT_, UnitX_, UnitMass_, UnitRho_

  use ModUserEmpty,                               &
       IMPLEMENTED1 => user_read_inputs,           &
       IMPLEMENTED2 => user_action,                &
       IMPLEMENTED3 => user_calc_sources_expl,     &
       IMPLEMENTED4 => user_set_plot_var,          &
       IMPLEMENTED5 => user_get_log_var,           &
       IMPLEMENTED6 => user_initial_perturbation,  &
       IMPLEMENTED7 => user_set_face_boundary

  include 'user_module.h'

  character (len=*), parameter :: NameUserFile = "ModUserJupiter.f90"
  character (len=*), parameter :: &
       NameUserModule = 'Jupiter Magnetosphere, Xianzhe Jia and Yash Sarkango'

  ! User input variables
  logical :: UseMassLoading = .false.
  logical :: AccelerateMassLoading, UseHighEnergyPressure
  real :: NeutralProfileConst, ScaleHeight, AlphaRec
  real :: IonizationRate, SigmaCollision
  real :: TorusDensity, ScaleHeightAngle
  real :: DensityJumpLimit = 0.1

  ! Source terms
  real, allocatable :: rhosi(:, :, :, :) ! Ionization
  real, allocatable :: qsd(:, :, :, :)   ! Charge-exchange
  real, allocatable :: alpha(:, :, :, :) ! Recombination

contains
  !============================================================================
  subroutine user_read_inputs
    use ModReadParam, ONLY: read_line, read_command, read_var
    use ModIO, ONLY: write_prefix, write_myname, iUnitOut

    character (len=100) :: NameCommand
    !--------------------------------------------------------------------------
    if (iProc == 0 .and. lVerbose > 0) then
       call write_prefix
       write(iUnitOut, *) 'User read_input JUPITER starts'
    end if

    do
       if (.not. read_line()) EXIT
       if (.not. read_command(NameCommand)) CYCLE

       select case(NameCommand)
       case("#MASSLOADING")
          call read_var('UseMassLoading', UseMassLoading)
          if (UseMassLoading) then
             call read_var(  &
                  'DoAccelerateMassLoading', AccelerateMassLoading)
             call read_var(  &
                  'UseHighEnergyPressure', UseHighEnergyPressure)
             call read_var(  &
                  'NeutralProfileConst', NeutralProfileConst)
             call read_var(  &
                  'ScaleHeight (deg)', ScaleHeight)
             call read_var(  &
                  'RecombinateRate (1/s)', AlphaRec)
             call read_var(  &
                  'IonizationRate (1/s)', IonizationRate)
             call read_var(  &
                  'CollisionCrossSection (cm2)', SigmaCollision)
          end if

       case('#TORUSINNERBC')
          call read_var('TorusDensity', TorusDensity)
          call read_var('ScaleHeightAngle', ScaleHeightAngle)
          call read_var('DensityJumpLimit', DensityJumpLimit)

       case("#USERINPUTEND")
          if (iProc == 0 .and. lVerbose > 0) then
             call write_prefix
             write(iUnitOut, *) 'User read_input JUPITER ends'
          end if
          EXIT

       case default
          if (iProc == 0) then
             call write_myname
             write(*, *) 'ERROR: Invalid user defined #COMMAND'
             write(*, *) '--Check user_read_inputs for errors'
             write(*, *) '--Check to make sure #USERINPUTEND was used'
             write(*, *) ' *Unrecognized command: '//NameCommand
             call stop_mpi( &
                  'ERROR: Correct PARAM.in or user_read_inputs')
          end if
       end select

    end do

  end subroutine user_read_inputs
  !============================================================================
  subroutine user_action(NameAction)
    ! This routine gets called multiple times during the run and provides
    ! and opportunity to do something.
    ! NameAction identifies where the call is from.

    character(len=*), intent(in):: NameAction
    character(len=*), parameter:: NameSub = 'user_action'
    !--------------------------------------------------------------------------
    select case(NameAction)
    case('initialize module')

       if (.not. allocated(rhosi)) &
            allocate(rhosi(1:nI, 1:nJ, 1:nK, 1:MaxBlock))
       if (.not. allocated(qsd)) &
            allocate(  qsd(1:nI, 1:nJ, 1:nK, 1:MaxBlock))
       if (.not. allocated(alpha)) &
            allocate(alpha(1:nI, 1:nJ, 1:nK, 1:MaxBlock))

       rhosi = 0.; qsd = 0.; alpha = 0.

    case('clean module')
       if (allocated(rhosi)) deallocate(rhosi)
       if (allocated(qsd))   deallocate(qsd)
       if (allocated(alpha)) deallocate(alpha)

    end select

  end subroutine user_action
  !============================================================================
  subroutine user_calc_sources_expl(iBlock)

    use ModAdvance, ONLY: Source_VC, State_VGB
    use ModGeometry, ONLY: Xyz_DGB, r_GB, rMin_B
    use ModPhysics, ONLY: gBody
    use ModConst, ONLY: cProtonMass, cPi

    integer, intent(in) :: iBlock

    logical :: DoTest

    real, dimension(1:nI, 1:nJ, 1:nK) :: &
         Srho, SrhoUx, SrhoUy, SrhoUz, SBx, SBy, SBz, Sp, SE

    integer :: i, j, k

    real :: IonMass = 16.0
    real :: ux, uy, uz, u2, uRelative2
    real :: Rxy, uNeutral2, uNeutral, uNeutralx, uNeutraly, uNeutralz
    real :: cxsource, nNeutral
    real :: Tfrac = 1.0  ! Ratio of electron-ion temperature

    character(len=*), parameter:: NameSub = 'user_calc_sources_expl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! Set the source arrays for this block to zero
    Srho   = 0.0
    SrhoUx = 0.0
    SrhoUy = 0.0
    SrhoUz = 0.0
    SBx    = 0.0
    SBy    = 0.0
    SBz    = 0.0
    SP     = 0.0
    SE     = 0.0

    if (.not. UseMassLoading) RETURN
    if (rMin_B(iBlock) > 15.0) RETURN

    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if ((Xyz_DGB(Z_, i, j, k, iBlock) > 6.0 .or. &
            Xyz_DGB(Z_, i, j, k, iBlock) < -6.0) .or. &
            (r_GB(i, j, k, iBlock) > 15.0 .or. &
            r_GB(i, j, k, iBlock) < 3.0)) CYCLE

       ux = State_VGB(rhoUx_,i,j,k,iBlock) / State_VGB(rho_,i,j,k,iBlock)
       uy = State_VGB(rhoUy_,i,j,k,iBlock) / State_VGB(rho_,i,j,k,iBlock)
       uz = State_VGB(rhoUz_,i,j,k,iBlock) / State_VGB(rho_,i,j,k,iBlock)
       u2 = sqrt(ux**2 + uy**2 + uz**2)

       ! Cylindrical Radius
       Rxy = sqrt(Xyz_DGB(X_, i, j, k, iBlock)**2 + &
            Xyz_DGB(Y_, i, j, k, iBlock)**2)

       uNeutral2 = abs(gBody) / Rxy
       uNeutral  = sqrt(uNeutral2)
       uNeutralx = -uNeutral * Xyz_DGB(Y_, i, j, k, iBlock) / Rxy
       uNeutraly =  uNeutral * Xyz_DGB(X_, i, j, k, iBlock) / Rxy
       uNeutralz =  0.0

       uRelative2 = &
            (uNeutralx - ux)**2 + (uNeutraly - uy)**2 + (uNeutralz - uz)**2

       if (Rxy <= 5.71) then
          nNeutral = &
               NeutralProfileConst * 60.0 * exp((Rxy - 5.71) / 0.2067)
       else if (Rxy > 5.71 .and. Rxy <= 5.875) then
          nNeutral = &
               NeutralProfileConst * 60.0 * exp(-(Rxy - 5.685) / 0.1912)
       else
          nNeutral = &
               NeutralProfileConst * 19.9 * exp( &
               -(Rxy - 5.9455) / (0.053173 * Rxy + 0.55858))
       end if

       nNeutral = nNeutral * exp(-(Xyz_DGB(Z_, i, j, k, iBlock)**2 / &
            (tan(ScaleHeight * cPi / 180.) * Rxy)**2))

       ! Be careful of units -
       ! We want - (Fortunately, these are also normalized units)
       !     cxsource == [1/s]             == [1 / Norm(time)]
       !     rhosi    == [amu / cm^3 / s] == [Norm(density / time)]
       !     qsd      == [amu / cm^3 / s] == [Norm(density / time)]
       !     alpha    == [amu / cm^3 / s] == [Norm(density / time)]
       !
       ! SigmaCollision is input in [cm^-2], nNeutral has units of [cm^-3]
       ! and uRelative is normalized. We convert the product of the first
       ! two terms to [m] and then convert to normalized units to be
       ! compatible with uRelative. This is because normalized units of
       ! density is [amu/cm^3], while that of length is [rPlanet].
       !
       ! IonizationRate has units of [1/s] and IonMass of [amu].
       ! The normalized unit for density and time is [amu/cm^3] and [s],
       ! so we can directly use them. This is assuming that the
       ! normalization is PLANETARY.
       !
       ! Similarly, no calculation is needed for alpha, since AlphaRec
       ! has units of [1/s].

       cxsource = nNeutral &
            * SigmaCollision * sqrt(uRelative2) * 1e2 / Si2No_V(UnitX_)

       rhosi(i, j, k, iBlock) = &
            nNeutral * IonizationRate * IonMass

       qsd(i, j, k, iBlock) = &
            cxsource * State_VGB(rho_, i, j, k, iBlock)

       alpha(i, j, k, iBlock) = &
            AlphaRec * State_VGB(rho_, i, j, k, iBlock)

       if (AccelerateMassLoading) &
            rhosi(i, j, k, iBlock) = rhosi(i, j, k, iBlock) * 10.

       ! Load the source terms into the right hand side

       Srho(i, j, k) = Srho(i, j, k) &
            + (rhosi(i, j, k, iBlock) - alpha(i, j, k, iBlock))

       SrhoUx(i, j, k) = SrhoUx(i, j, k) &
            + (rhosi(i, j, k, iBlock) + qsd(i, j, k, iBlock)) * uNeutralx &
            - (cxsource + AlphaRec) * State_VGB(rhoUx_, i, j, k, iBlock)

       SrhoUy(i, j, k) = SrhoUy(i, j, k) &
            + (rhosi(i, j, k, iBlock) + qsd(i, j, k, iBlock)) * uNeutraly &
            - (cxsource + AlphaRec) * State_VGB(rhoUy_, i, j, k, iBlock)

       SrhoUz(i, j, k) = SrhoUz(i, j, k) &
            - (cxsource + AlphaRec) * State_VGB(rhoUz_, i, j, k, iBlock)

       SE(i, j, k) = SE(i, j, k) &
            + 0.5 * (rhosi(i, j, k, iBlock) &
            + qsd(i, j, k, iBlock)) * uNeutral2 &
            - 0.5 * (qsd(i, j, k, iBlock) + alpha(i, j, k, iBlock)) * u2 &
            - 1.5 * State_VGB(p_, i, j, k, iBlock) * AlphaRec &
            + 1.5 * cxsource * State_VGB(p_, i, j, k, iBlock) * Tfrac

       SP(i, j, k) = SP(i, j, k) &
            + 0.5 * (rhosi(i, j, k, iBlock) &
            + qsd(i, j, k, iBlock)) * uRelative2 &
            - 1.5 * cxsource * State_VGB(p_, i, j, k, iBlock) &
            * (1. - Tfrac) &
            - 1.5 * AlphaRec * State_VGB(p_, i, j, k, iBlock)

       ! High Energy Pressure code not necessary atm.

    end do; end do; end do

    Source_VC(rho_   ,:,:,:) = Srho   + Source_VC(rho_   ,:,:,:)
    Source_VC(rhoUx_ ,:,:,:) = SrhoUx + Source_VC(rhoUx_ ,:,:,:)
    Source_VC(rhoUy_ ,:,:,:) = SrhoUy + Source_VC(rhoUy_ ,:,:,:)
    Source_VC(rhoUz_ ,:,:,:) = SrhoUz + Source_VC(rhoUz_ ,:,:,:)
    Source_VC(Bx_    ,:,:,:) = SBx    + Source_VC(Bx_    ,:,:,:)
    Source_VC(By_    ,:,:,:) = SBy    + Source_VC(By_    ,:,:,:)
    Source_VC(Bz_    ,:,:,:) = SBz    + Source_VC(Bz_    ,:,:,:)
    Source_VC(P_     ,:,:,:) = SP     + Source_VC(P_     ,:,:,:)
    Source_VC(Energy_,:,:,:) = SE     + Source_VC(Energy_,:,:,:)

    call test_stop(NameSub,DoTest,iBlock)

  end subroutine user_calc_sources_expl
  !============================================================================
  subroutine user_set_plot_var(iBlock, NameVar, IsDimensional, &
       PlotVar_G, PlotVarBody, UsePlotVarBody, &
       NameTecVar, NameTecUnit, NameIdlUnit, IsFound)

    use ModGeometry, ONLY: r_GB
    use ModConst, ONLY: cProtonMass

    integer,          intent(in)   :: iBlock
    character(len=*), intent(in)   :: NameVar
    logical,          intent(in)   :: IsDimensional
    real,         intent(inout):: PlotVar_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)
    real,             intent(out)  :: PlotVarBody
    logical,          intent(out)  :: UsePlotVarBody
    character(len=*), intent(inout):: NameTecVar
    character(len=*), intent(inout):: NameTecUnit
    character(len=*), intent(inout):: NameIdlUnit
    logical,          intent(out)  :: IsFound

    integer :: i, j, k
    character(len=*), parameter:: NameSub = 'user_set_plot_var'
    !--------------------------------------------------------------------------
    UsePlotVarBody = .true.
    PlotVarBody = 0.0

    select case(NameVar)
    case('rhosi')
       NameTecVar = 'rhosi'
       NameTecUnit = '[amu/cm^3/s]'
       NameIdlUnit = 'amu/cm^3/s'
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          PlotVar_G(i, j, k) = rhosi(i, j, k, iBlock)
       end do; end do; end do
       IsFound = .true.

    case('qsd')
       NameTecVar = 'qsd'
       NameTecUnit = '[amu/cm^3/s]'
       NameIdlUnit = 'amu/cm^3/s'
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          PlotVar_G(i, j, k) = qsd(i, j, k, iBlock)
       end do; end do; end do
       IsFound = .true.

    case('alpha')
       NameTecVar = 'alpha'
       NameTecUnit = '[amu/cm^3/s]'
       NameIdlUnit = 'amu/cm^3/s'
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          PlotVar_G(i, j, k) = alpha(i, j, k, iBlock)
       end do; end do; end do
       IsFound = .true.

    case('isuservol')
       NameTecVar = 'isuservol'
       NameTecUnit = '[Y/N]'
       NameIdlUnit = 'Y/N'
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if (r_GB(i, j, k, iBlock) < 30. .and. &
               r_GB(i, j, k, iBlock) > 2.5) then
             PlotVar_G(i, j, k) = 1.0
          else
             PlotVar_G = 0.0
          end if
       end do; end do; end do
       IsFound = .true.

    case default
       IsFound = .false.
       call stop_mpi(NameSub//':unknown plot variable: '//Namevar)
    end select

  end subroutine user_set_plot_var
  !============================================================================
  subroutine user_get_log_var(VarValue, TypeVar, Radius)

    use ModAdvance, ONLY: State_VGB

    real, intent(out) :: VarValue
    character (len=*), intent(in) :: TypeVar
    real, intent(in), optional :: Radius

    logical :: DoTest = .false.
    integer :: i, j, k, iBlock

    real :: userLogNo2Si
    character(len=*), parameter:: NameSub = 'user_get_log_var'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    if (DoTest) write(*, *) NameSub, ':TypeVar=', TypeVar

    ! Note: UnitMass_ == [amu/cm^3 rPlanet^3]

    select case(TypeVar)
    case('rhosi_integrated')
       call integrate_shell_volume(VarValue, rhosi, 3., 30.)
       VarValue = VarValue * No2Si_V(UnitMass_) / No2Si_V(UnitT_)

    case('qsd_integrated')
       call integrate_shell_volume(VarValue, qsd, 3., 30.)
       VarValue = VarValue * No2Si_V(UnitMass_) / No2Si_V(UnitT_)

    case('alpha_integrated')
       call integrate_shell_volume(VarValue, alpha, 3., 30.)
       VarValue = VarValue * No2Si_V(UnitMass_) / No2Si_V(UnitT_)

    case('mass_user_region')
      call integrate_shell_volume(VarValue, State_VGB(rho_, :, :, :, :), 3., 30.)
      VarValue = VarValue * No2Si_V(UnitMass_)

    case('volume_user_region')
      call integrate_shell_volume( VarValue,  &
        State_VGB(rho_, :, :, :, :) / State_VGB(rho_, :, :, :, :), &
        3., 30.)

      VarValue = VarValue * No2Si_V(UnitX_)**3

    case default
       call stop_mpi(NameSub//': wrong logvarname='//TypeVar)
    end select

    if(VarValue /= VarValue) then
       write(*,*) "NaN detected in variable ",TypeVar
       call stop_mpi('ERROR: Tried to write NaN to log file!')
    end if

    call test_stop(NameSub,DoTest,iBlock)

  end subroutine user_get_log_var
  !============================================================================
  subroutine integrate_shell_volume(resultVolume, var, r1, r2)

    use ModGeometry, ONLY: r_GB, IsNoBody_B
    use BATL_lib, ONLY: CellVolume_GB, Used_GB, Unused_B

    real, intent(out) :: resultVolume
    real, intent(in) :: var(1:nI, 1:nJ, 1:nK, 1:nBlock)
    real, intent(in) :: r1
    real, intent(in) :: r2

    integer :: i, j, k, iBlock
    !--------------------------------------------------------------------------
    resultVolume = 0.0

    do iBlock = 1, nBlock

       if (Unused_B(iBlock)) CYCLE

       if (IsNoBody_B(iBlock)) then
          do k = 1, nK; do j = 1, nJ ; do i = 1, nI
             if (r_GB(i, j, k, iBlock) > r2) CYCLE
             if (r_GB(i, j, k, iBlock) < r1) CYCLE
             resultVolume = resultVolume + &
                  var(i,j,k,iBlock) * CellVolume_GB(i,j,k,iBlock)
          end do; end do; end do
       else
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             if (.not. Used_GB(i, j, k, iBlock)) CYCLE
             if (r_GB(i, j, k, iBlock) > r2) CYCLE
             if (r_GB(i, j, k, iBlock) < r1) CYCLE
             resultVolume = resultVolume + &
                  var(i,j,k,iBlock) * CellVolume_GB(i,j,k,iBlock)
          end do; end do; end do
       end if
    end do
  end subroutine integrate_shell_volume
  !============================================================================
  subroutine user_initial_perturbation
    use ModMain, ONLY: nBlockMax, Unused_B
    use ModGeometry, ONLY: r_GB, Xyz_DGB
    use ModAdvance, ONLY: State_VGB
    use ModPhysics, ONLY: Io2No_V, UnitRho_
    use ModSize, ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK
    use ModIO, ONLY: IsRestart
    use ModConst, ONLY: cPi

    integer :: i, j, k, iBlock
    real :: Hz, Rxy
    real :: uRot_D(3)
    character(len=*), parameter:: NameSub = 'user_initial_perturbation'
    !--------------------------------------------------------------------------
    if (IsRestart) RETURN

    do iBlock = 1, nBlock
       if (Unused_B(iBlock)) CYCLE
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          Rxy = sqrt(Xyz_DGB(X_,i,j,k,iBlock)**2 + &
               Xyz_DGB(Y_,i,j,k,iBlock)**2)

          if (Rxy > 40.0 .or. Rxy < 3.0) CYCLE

          Hz = tan(2.5 * cPi / 180) * Rxy

          if (Rxy < 5.0) then
             State_VGB(rho_, i, j, k, iBlock) = &
                  State_VGB(rho_, i, j, k, iBlock) &
                  + 1e6 * exp((r_GB(i, j, k, iBlock) - 6.) / 0.2067) &
                  * exp(-(Xyz_DGB(Z_, i, j, k, iBlock)**2)/Hz**2) &
                  * Io2No_V(UnitRho_) &
                  * 16.0

          else
             State_VGB(rho_, i, j, k, iBlock) = &
                  State_VGB(rho_, i, j, k, iBlock) &
                  + (1987   * (r_GB(i, j, k, iBlock) / 6.)**(-8.2) &
                  + 14   * (r_GB(i, j, k, iBlock) / 6.)**(-3.2) &
                  + 0.05 * (r_GB(i, j, k, iBlock) / 6.)**(-0.65)) &
                  * exp(-(Xyz_DGB(Z_, i, j, k, iBlock)**2) / Hz**2) &
                  * Io2No_V(UnitRho_) &
                  * 16.0
          end if

          ! Hard-set velocity equal to the corotation velocity
          ! In this case, momentum

          ! State_VGB(rhoUx_:rhoUz_,i,j,j,iBlock) = &
          !      State_VGB(rho_,i,j,k,iBlock)*uRot_D(:)

       end do; end do; end do

    end do

  end subroutine user_initial_perturbation
  !============================================================================
  subroutine user_set_face_boundary(FBC)

    ! Apply user defined face boundary condition

    use ModMain, ONLY: FaceBCType, UseIe, UseRotatingBc, Body1_
    use ModCoordTransform, ONLY: cross_product
    use ModIeCoupling, ONLY: calc_inner_bc_velocity
    use ModPhysics, ONLY: OmegaBody_D, BodyRho_I, Io2No_V, UnitRho_
    use ModVarIndexes, ONLY: DefaultState_V
    use ModConst, ONLY: cPi

    type(FaceBCType) :: FBC

    real :: uIono_D(3), uRot_D(3), theta, theta0

    real :: FaceDensity

    character(len=*), parameter:: NameSub = 'user_set_face_boundary'
    !--------------------------------------------------------------------------
    associate ( iBoundary => FBC%iBoundary, &
                TypeBc => FBC%TypeBc,       &
                iFace => FBC%iFace,         &
                jFace => FBC%jFace,         &
                kFace => FBC%kFace,         &
                TimeBc => FBC%TimeBc,       &
                iBlockBc => FBC%iBlockBc,   &
                iSide => FBC%iSide,         &
                VarsGhostFace_V => FBC%VarsGhostFace_V, &
                VarsTrueFace_V => FBC%VarsTrueFace_V,   &
                FaceCoords_D => FBC%FaceCoords_D,       &
                B0Face_D => FBC%B0Face_D )

    ! VarsGhostFace_V = 0.0

    ! Set all variables to float (default).
    VarsGhostFace_V = VarsTrueFace_V

    ! Set face velocity to zero.
    VarsGhostFace_V(Ux_:Uz_) = -VarsTrueFace_V(Ux_:Uz_)

    if (UseIe .and. iBoundary==Body1_) then

        ! Get ExB velocity
        call calc_inner_bc_velocity( &
             TimeBc, FaceCoords_D, &
             VarsTrueFace_V(Bx_:Bz_) + B0Face_D, uIono_D)

        ! Subtract radial component of velocity.
        uIono_D = uIono_D &
            -FaceCoords_D * sum(FaceCoords_D * uIono_D) / sum(FaceCoords_D**2)

        VarsGhostFace_V(Ux_:Uz_) = VarsGhostFace_V(Ux_:Uz_) + 2 * uIono_D

    end if

    if (UseRotatingBc .and. iBoundary==Body1_) then
        ! Get Omega x r (corotation) velocity.
        uRot_D = cross_product(OmegaBody_D, FaceCoords_D)
        VarsGhostFace_V(Ux_:Uz_) = VarsGhostFace_V(Ux_:Uz_) + 2 * uRot_D
    end if

    ! Specify density (Z-dependent)
    theta = atan2(FaceCoords_D(3), sqrt(sum(FaceCoords_D(1:2)**2)))
    theta0 = ScaleHeightAngle * cPi / 180

    FaceDensity = BodyRho_I(1) + TorusDensity * exp( - (theta / theta0)**2 ) * Io2No_V(UnitRho_)

    ! VarsGhostFace_V(rho_) = VarsTrueFace_V(rho_) + &
    !     sign(1.0, FaceDensity - VarsTrueFace_V(rho_)) *   &
    !     min(  abs(FaceDensity - VarsTrueFace_V(rho_))     &
    !        , DensityJumpLimit * VarsTrueFace_V(rho_))

    if (FaceDensity > VarsTrueFace_V(rho_)) then
        VarsGhostFace_V(rho_) = VarsTrueFace_V(rho_) + &
            min( abs(FaceDensity - VarsTrueFace_V(rho_)), &
                 DensityJumpLimit * VarsTrueFace_V(rho_))
    else
        VarsGhostFace_V(rho_) = VarsTrueFace_V(rho_)
    end if

    end associate
  end subroutine user_set_face_boundary
  !============================================================================
end module ModUser
!==============================================================================
