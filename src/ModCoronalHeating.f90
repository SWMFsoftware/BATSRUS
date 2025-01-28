!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModCoronalHeating

  use BATL_lib, ONLY: &
       test_start, test_stop
  use ModBatsrusUtility, ONLY: stop_mpi
#ifdef _OPENACC
  use ModUtilities, ONLY: norm2
#endif
  use ModMain,       ONLY: nI, nJ, nK
  use ModTurbulence
  use omp_lib

  implicit none
  SAVE

  PRIVATE  ! except

  public :: read_coronal_heating_param ! call read_turbulence, if needed
  public :: init_coronal_heating       ! call init_turbulence if needed
  public :: get_coronal_heat_factor
  public :: get_coronal_heating
  public :: get_block_heating
  public :: get_cell_heating

  logical, public :: UseCoronalHeating = .false.
  !$acc declare create(UseCoronalHeating)

  ! Exponential Model ---------
  ! Variables and parameters for various heating models

  ! quantitative parameters for exponential heating model
  real :: HeatingAmplitude, HeatingAmplitudeCgs = 6.07e-7
  real :: DecayLengthExp = 0.7  ! in Solar Radii units
  logical :: UseExponentialHeating = .false.

  ! parameters for high-B transition (in Gauss)
  ! idea is to grossly approx 'Active Region' heating values
  logical :: UseArComponent = .false.
  real :: ArHeatB0 = 30.0
  real :: DeltaArHeatB0 = 5.0
  real :: ArHeatFactorCgs = 4.03E-05  ! cgs energy density = [ergs cm-3 s-1]

  ! Abbett's model -------------

  ! Normalization constant for Abbett Model
  real :: HeatNormalization = 1.0

  ! long scale height heating (Ch = Coronal Hole)
  logical :: DoChHeat = .false.
  real :: HeatChCgs = 5.0e-7
  real :: DecayLengthCh = 0.7

  logical :: DoInit = .true.

  ! Bill Abbet's model, if .true.
  logical, public :: UseUnsignedFluxModel = .false.

  ! Normalized value of Heating constant
  real :: HeatFactor = 0.0

  ! Cgs value of total power input from coronal heating
  real :: TotalCoronalHeatingCgs = 1.0e+28

  ! Exponential Scale height to truncate heating function
  real :: DecayLength = 1.0
  real, public :: DtUpdateFlux = -1.0
  real, public :: UnsignedFluxHeight = -99999.0

contains
  !============================================================================
  subroutine read_coronal_heating_param(NameCommand)
    ! Read heating parameters, call read_turbulence_param if needed
    use ModReadParam,  ONLY: read_var, lStringLine

    character(len=*), intent(in):: NameCommand
    character(len=lStringLine) :: TypeCoronalHeating
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_coronal_heating_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    select case(NameCommand)
    case("#CORONALHEATING")
       call read_var('TypeCoronalHeating', TypeCoronalHeating)

       ! Initialize logicals
       UseCoronalHeating = .true.
       UseUnsignedFluxModel = .false.
       UseExponentialHeating= .false.
       UseAlfvenWaveDissipation = .false.
       UseTurbulentCascade = .false.
       UseReynoldsDecomposition = .false.

       select case(TypeCoronalHeating)
       case('F','none')
          UseCoronalHeating = .false.
       case('exponential')
          DoInit = .true.
          UseExponentialHeating = .true.
          call read_var('DecayLengthExp', DecayLengthExp)
          call read_var('HeatingAmplitudeCgs', HeatingAmplitudeCgs)
       case('unsignedflux','Abbett')
          UseUnsignedFluxModel = .true.
          call read_var('DecayLength', DecayLength)
          call read_var('HeatNormalization', HeatNormalization)
       case('alfvenwavedissipation', 'turbulentcascade', 'usmanov')
          call read_turbulence_param(TypeCoronalHeating)
       case default
          call stop_mpi(NameSub//': unknown TypeCoronalHeating = ' &
               // TypeCoronalHeating)
       end select
       !$acc update device(UseCoronalHeating)
    case("#ACTIVEREGIONHEATING")
       call read_var('UseArComponent', UseArComponent)
       if(UseArComponent) then
          call read_var('ArHeatFactorCgs', ArHeatFactorCgs)
          call read_var('ArHeatB0', ArHeatB0)
          call read_var('DeltaArHeatB0', DeltaArHeatB0)
       endif

    case("#LONGSCALEHEATING")
       call read_var('DoChHeat', DoChHeat)
       if(DoChHeat)then
          call read_var('HeatChCgs', HeatChCgs)
          call read_var('DecayLengthCh', DecayLengthCh)
       end if
    case default
       call stop_mpi(NameSub//': unknown command = ' &
            // NameCommand)
    end select

    call test_stop(NameSub, DoTest)
  end subroutine read_coronal_heating_param
  !============================================================================
  subroutine init_coronal_heating
    ! Convert dimensional heating parameters
    use ModPhysics,     ONLY: Si2No_V, UnitEnergyDens_, UnitT_

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_coronal_heating'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    call init_turbulence
    if(.not.DoInit)then
       call test_stop(NameSub, DoTest)
       RETURN
    end if
    DoInit = .false.

    if(UseExponentialHeating)then
       HeatingAmplitude =  HeatingAmplitudeCgs*0.1 &
            *Si2No_V(UnitEnergyDens_)/Si2No_V(UnitT_)
    end if

    call test_stop(NameSub, DoTest)
  end subroutine init_coronal_heating
  !============================================================================
  subroutine get_coronal_heat_factor

    ! Integrate heat function with a unit factor, then multiply by
    ! the total heating and divide by volume
    ! With thus calculated factor, the integral of coronal heating
    ! function equals the prescribed (for example, stemming from
    ! the unsigned flux) total heating

    use ModAdvance,     ONLY: State_VGB, Bz_
    use ModGeometry,    ONLY: IsNoBody_B, Used_GB, TypeGeometry
    use ModMagnetogram, ONLY: get_magnetogram_field
    use ModMain,        ONLY: nI, nJ, nK, nBlock, Unused_B, tSimulation,z_
    use ModMpi,         ONLY: MPI_REAL, MPI_SUM
    use ModNumConst,    ONLY: cHalfPi, cTwoPi
    use ModPhysics,     ONLY: Si2No_V, No2Si_V, UnitX_, UnitT_, &
         UnitEnergyDens_, rBody
    use BATL_lib,       ONLY: CellFace_DB, CellVolume_GB, nProc, iComm

    integer :: i, j, k, iBlock
    integer :: iTheta, iPhi, iError
    real :: UnsignedFluxCgs, dAreaCgs
    real :: HeatFunction, HeatFunctionVolume, HeatFunctionVolumePe
    real :: x, y, z, Theta, Phi, SinTheta, CosTheta, SinPhi, CosPhi
    real :: B0_D(3), BrSi, BrCgs, SumUnsignedBrCgs
    real :: BzCgs_II(1:nI,1:nJ), SumUnsignedBzCgs, UnsignedFluxCgsPe
    real    :: TotalCoronalHeating = -1.0, TimeUpdateLast = -1.0
    logical :: DoFirst = .true.
    !$omp threadprivate(TotalCoronalHeating, TimeUpdateLast, DoFirst)

    integer, parameter:: nTheta = 72, nPhi=90
    real, parameter:: dSinTheta = 2.0/nTheta, dPhi = cTwoPi/nPhi
    real, parameter:: HeatExponent = 1.1488, HeatCoef = 89.4

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_coronal_heat_factor'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(DoFirst .and. DtUpdateFlux <= 0.0)then

       ! uniform cell area on sphere
       dAreaCgs = rBody**2*dSinTheta*dPhi*No2Si_V(UnitX_)**2*1e4
       SumUnsignedBrCgs = 0.0

       do iTheta = 0, nTheta

          Theta = cHalfPi - asin((real(iTheta) + 0.5)*dSinTheta - 1.0)
          SinTheta = sin(Theta)
          CosTheta = cos(Theta)
          do iPhi = 1, nPhi
             Phi=(real(iPhi)-0.5)*dPhi
             SinPhi = sin(Phi)
             CosPhi = cos(Phi)

             x = rBody*SinTheta*CosPhi
             y = rBody*SinTheta*SinPhi
             z = rBody*CosTheta

             call get_magnetogram_field(x, y, z, B0_D)
             BrSi = (x*B0_D(1) + y*B0_D(2) + z*B0_D(3))/rBody
             BrCgs = BrSi*1e4
             SumUnsignedBrCgs = SumUnsignedBrCgs + abs(BrCgs)

          end do
       end do

       UnsignedFluxCgs = SumUnsignedBrCgs*dAreaCgs

       TotalCoronalHeatingCgs = HeatCoef*UnsignedFluxCgs**HeatExponent

       TotalCoronalHeating = TotalCoronalHeatingCgs*1e-7 &
            *Si2No_V(UnitEnergyDens_)*Si2No_V(UnitX_)**3/Si2No_V(UnitT_)

       DoFirst = .false.

    elseif( DtUpdateFlux > 0.0 .and. &
         tSimulation - TimeUpdateLast > DtUpdateFlux ) then

       UnsignedFluxCgs = 0.0
       if(TypeGeometry == 'spherical')then
          do iBlock = 1, nBlock
             if(Unused_B(iBlock)) CYCLE
             if(IsNoBody_B(iBlock)) then
                call get_photosphere_unsignedflux(iBlock, UnsignedFluxCgs)
             end if
          end do
       elseif(TypeGeometry == 'cartesian')then
          do iBlock = 1, nBlock
             if(Unused_B(iBlock)) CYCLE
             if(IsNoBody_B(iBlock)) then
                dAreaCgs = CellFace_DB(z_,iBlock)*No2Si_V(UnitX_)**2*1e4

                call get_photosphere_field(iBlock, &
                     State_VGB(Bz_,1:nI,1:nJ,0:nK+1,iBlock), BzCgs_II)

                SumUnsignedBzCgs = sum(abs(BzCgs_II))
                UnsignedFluxCgs = UnsignedFluxCgs +  SumUnsignedBzCgs*dAreaCgs
             end if
          end do
       else
          call stop_mpi(NameSub//': '//TypeGeometry// &
              ' geometry is not yet implemented')
       end if
       if(nProc>1)then
          UnsignedFluxCgsPe = UnsignedFluxCgs
          call MPI_allreduce(UnsignedFluxCgsPe, UnsignedFluxCgs, 1, &
               MPI_REAL, MPI_SUM, iComm, iError)
       end if
       TotalCoronalHeatingCgs = HeatCoef*UnsignedFluxCgs**HeatExponent

       TotalCoronalHeating = TotalCoronalHeatingCgs*1e-7 &
            *Si2No_V(UnitEnergyDens_)*Si2No_V(UnitX_)**3/Si2No_V(UnitT_)
       TimeUpdateLast = tSimulation

    end if

    HeatFunctionVolume = 0
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE

       if(IsNoBody_B(iBlock)) then
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             call get_heat_function(i, j, k, iBlock, HeatFunction)
             HeatFunctionVolume = HeatFunctionVolume &
                  + HeatFunction*CellVolume_GB(i,j,k,iBlock)
          end do; end do; end do
       else
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             if(Used_GB(i,j,k,iBlock))then
                call get_heat_function(i, j, k, iBlock, HeatFunction)
                HeatFunctionVolume = HeatFunctionVolume &
                     + HeatFunction*CellVolume_GB(i,j,k,iBlock)
             end if
          end do; end do; end do
       end if
    end do

    if(nProc>1)then
       HeatFunctionVolumePe = HeatFunctionVolume
       call MPI_allreduce(HeatFunctionVolumePe, HeatFunctionVolume, 1, &
            MPI_REAL, MPI_SUM, iComm, iError)
    end if

    HeatFactor = TotalCoronalHeating/HeatFunctionVolume

    call test_stop(NameSub, DoTest)
  end subroutine get_coronal_heat_factor
  !============================================================================
  subroutine get_heat_function(i, j, k, iBlock, HeatFunction)
    ! Is defined with some arbitrary factor, becomes the coronal
    ! heating once multiplied by the heat factor calculated above
    use ModMain, ONLY: UseB0, z_
    use ModAdvance, ONLY: State_VGB, Bx_, Bz_
    use ModB0, ONLY: B0_DGB
    use ModGeometry, ONLY: r_GB
    use BATL_lib, ONLY: Xyz_DGB, IsCartesian

    integer, intent(in) :: i, j, k, iBlock
    real, intent(out) :: HeatFunction

    real :: Bmagnitude, B_D(3)

    character(len=*), parameter:: NameSub = 'get_heat_function'
    !--------------------------------------------------------------------------
    if(UseB0) then
       B_D = B0_DGB(:,i,j,k,iBlock) + State_VGB(Bx_:Bz_,i,j,k,iBlock)
    else
       B_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
    end if

    Bmagnitude = norm2(B_D)

    if(DtUpdateFlux <= 0.0)then
       HeatFunction = Bmagnitude*exp(-(r_GB(i,j,k,iBlock)-1.0)/DecayLength)
    else
       if(IsCartesian)then
          if(Xyz_DGB(z_,i,j,k,iBlock)<UnsignedFluxHeight)then
             HeatFunction = 0.0
          else
             HeatFunction = Bmagnitude
          end if
       else
          if(r_GB(i,j,k,iBlock)<UnsignedFluxHeight)then
             HeatFunction = 0.0
          else
             HeatFunction = Bmagnitude &
                  *exp(-(r_GB(i,j,k,iBlock)-1.0)/DecayLength)
          end if
       end if
    end if

  end subroutine get_heat_function
  !============================================================================
  subroutine get_coronal_heating(i, j, k, iBlock, CoronalHeating)
    ! Calculate the coronal heating by applying a proper factor
    ! to a somewhat arbitrarily defind heating function
    integer, intent(in) :: i, j, k, iBlock
    real, intent(out) :: CoronalHeating

    real :: HeatFunction
    character(len=*), parameter:: NameSub = 'get_coronal_heating'
    !--------------------------------------------------------------------------
    call get_heat_function(i, j, k, iBlock, HeatFunction)

    CoronalHeating = HeatFactor*HeatFunction

  end subroutine get_coronal_heating
  !============================================================================
  subroutine get_photosphere_field(iBlock, Bz_C, BzCgs_II)
    ! Internal program used in calculating heat factor
    use ModMain,      ONLY: nI, nJ, nK, z_
    use ModInterpolate, ONLY: find_cell
    use ModPhysics,   ONLY: No2Si_V, UnitB_
    use BATL_lib,     ONLY: CoordMin_DB, CoordMax_DB, CellSize_DB

    integer, intent(in) :: iBlock
    real, intent(in)    :: Bz_C(1:nI, 1:nJ, 0:nK+1) ! temporary array created
    real, intent(out)   :: BzCgs_II(1:nI, 1:nJ)
    real :: zMin, zMax, DxLeft, z
    integer :: iLeft

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_photosphere_field'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    zMin = CoordMin_DB(z_,iBlock)
    zMax = CoordMax_DB(z_,iBlock)

    BzCgs_II = 0.0
    if((UnsignedFluxHeight > zMax) .or. (UnsignedFluxHeight < zMin)) RETURN

    z = (UnsignedFluxHeight - zMin)/CellSize_DB(z_,iBlock) + 0.5
    call find_cell(0, nK+1, z, iLeft, DxLeft)

    BzCgs_II = ((1.0 - DxLeft)*Bz_C(1:nI, 1:nJ, iLeft) + &
         DxLeft*Bz_C(1:nI, 1:nJ, iLeft+1))*No2Si_V(UnitB_)*1e4

    call test_stop(NameSub, DoTest, iBlock)

  end subroutine get_photosphere_field
  !============================================================================
  subroutine get_photosphere_unsignedflux(iBlock, UnsignedFluxCgs)
    ! Internal program used in calculating the heat factor
    use ModAdvance,     ONLY: State_VGB
    use ModGeometry,    ONLY: r_GB
    use ModMain,        ONLY: nJ, nK, r_
    use ModInterpolate, ONLY: find_cell
    use ModPhysics,     ONLY: No2Si_V, UnitB_, UnitX_
    use ModVarIndexes,  ONLY: Bx_, Bz_
    use BATL_lib,       ONLY: CoordMin_DB, CoordMax_DB, CellSize_DB, &
         CellFace_DFB, Xyz_DGB

    integer, intent(in) :: iBlock
    real, intent(inout) :: UnsignedFluxCgs

    real :: rMin, rMax, r, DrLeft, BrLeft, BrRight, BrCgs, DrL, dAreaCgs
    integer :: iLeft, j, k, iL

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_photosphere_unsignedflux'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    rMin = CoordMin_DB(r_,iBlock)
    rMax = CoordMax_DB(r_,iBlock)

    if((UnsignedFluxHeight > rMax) .or. (UnsignedFluxHeight < rMin)) RETURN

    ! Cells used to interpolate Br
    r = (UnsignedFluxHeight - rMin)/CellSize_DB(r_,iBlock) + 0.5
    call find_cell(0, nI+1, r, iLeft, DrLeft)

    ! Cells used to interpolate face area
    r = r + 0.5
    call find_cell(0, nI+1, r, iL, DrL)
    ! Make sure that CellFace_DFB is never out of index range
    if(iL == 0)then
       iL = 1; DrL = 0.0
    else if(iL == nI+1)then
       iL = nI; DrL = 1.0
    end if

    do k = 1, nK; do j = 1, nJ
       BrLeft = sum(Xyz_DGB(:,iLeft,j,k,iBlock) &
            *State_VGB(Bx_:Bz_,iLeft,j,k,iBlock))/r_GB(iLeft,j,k,iBlock)
       BrRight = sum(Xyz_DGB(:,iLeft+1,j,k,iBlock) &
            *State_VGB(Bx_:Bz_,iLeft+1,j,k,iBlock))/r_GB(iLeft+1,j,k,iBlock)

       BrCgs = ((1.0 - DrLeft)*BrLeft + DrLeft*BrRight)*No2Si_V(UnitB_)*1e4

       dAreaCgs = ((1.0-DrL)*CellFace_DFB(r_,iL,j,k,iBlock) &
            +            DrL*CellFace_DFB(r_,iL+1,j,k,iBlock)) &
            *No2Si_V(UnitX_)**2*1e4

       UnsignedFluxCgs = UnsignedFluxCgs + abs(BrCgs)*dAreaCgs
    end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine get_photosphere_unsignedflux
  !============================================================================
  subroutine get_cell_heating(i, j, k, iBlock, &
       WaveDissipationRate_V, CoronalHeating, TeSiOut)
    !$acc routine seq

    use ModAdvance, ONLY: State_VGB, Pe_, Rho_
    use ModPhysics, ONLY: AverageIonCharge, No2Si_V, UnitTemperature_
    use ModMultiFluid, ONLY: MassIon_I
    use ModChromosphere, ONLY: DoExtendTransitionRegion, extension_factor

    integer, intent(in):: i, j, k, iBlock
    real, intent(out):: CoronalHeating, &
         WaveDissipationRate_V(WaveFirst_:WaveLast_)
    real, intent(out), optional:: TeSiOut

    real:: TeSi, ExtensionFactorInv
    !--------------------------------------------------------------------------
    if(UseTurbulentCascade .or. UseReynoldsDecomposition)then
       call turbulent_cascade(i, j, k, iBlock, &
            WaveDissipationRate_V, CoronalHeating)
    else
       call calc_alfven_wave_dissipation(i, j, k, iBlock, &
            WaveDissipationRate_V, CoronalHeating)
    end if

    if(DoExtendTransitionRegion) then
       TeSi = State_VGB(Pe_,i,j,k,iBlock)/State_VGB(Rho_,i,j,k,iBlock)
       TeSi = TeSi * No2Si_V(UnitTemperature_) * &
            MassIon_I(1)/AverageIonCharge

       ExtensionFactorInv = 1/extension_factor(TeSi)
       WaveDissipationRate_V = ExtensionFactorInv*WaveDissipationRate_V
       CoronalHeating = ExtensionFactorInv*CoronalHeating
       if(present(TeSiOut)) TeSiOut = TeSi
    end if

  end subroutine get_cell_heating
  !============================================================================
  subroutine get_block_heating(iBlock)

    ! Calculate two arrays: CoronalHeating_C and WaveDissipationRate_VC
    ! If DoExtendTransitionRegion, then the extension factor is applied,
    ! so that in this case TeSi_C aarray should be set.
    ! The usual way to call this function is:
    !
    ! if(DoExtendTransitionRegion) call get_tesi_c(iBlock, TeSi_C)
    ! call get_block_heating(iBlock)

    use ModGeometry, ONLY: r_GB
    use ModPhysics, ONLY: Si2No_V, UnitEnergyDens_, UnitT_, No2Io_V, UnitB_
    use ModMain, ONLY: x_, z_, UseB0
    use ModVarIndexes, ONLY: Bx_, Bz_
    use ModAdvance, ONLY: State_VGB
    use ModB0, ONLY: B0_DGB
    use ModChromosphere, ONLY: DoExtendTransitionRegion, extension_factor, &
         TeSi_C

    integer, intent(in) :: iBlock

    integer:: i, j, k
    real:: HeatCh

    real:: B_D(3)

    ! local variables for ArHeating (Active Region Heating)
    real:: FractionB, Bcell

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_block_heating'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(UseAlfvenWaveDissipation)then
#ifndef _OPENACC
       if(IsOnAwRepresentative)call set_alfven_wave_vel_vect(iBlock)
#endif
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          call get_cell_heating(i, j, k, iBlock, &
               WaveDissipationRate_VC(:,i,j,k), &
               CoronalHeating_C(i,j,k))
       end do; end do; end do
#ifndef _OPENACC
    elseif(UseUnsignedFluxModel)then
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          call get_coronal_heating(i, j, k, iBlock, CoronalHeating_C(i,j,k))
          CoronalHeating_C(i,j,k) = CoronalHeating_C(i,j,k)*HeatNormalization
       end do; end do; end do
    elseif(UseExponentialHeating)then
       do k = 1,nK; do j = 1, nJ; do i = 1, nI
          CoronalHeating_C(i,j,k) = HeatingAmplitude &
               *exp(-max(r_GB(i,j,k,iBlock) - 1, 0.0) / DecayLengthExp)
       end do; end do; end do
#endif
    else
       CoronalHeating_C = 0.0
    end if

#ifndef _OPENACC
    if(DoChHeat) then
       HeatCh = HeatChCgs * 0.1 * Si2No_V(UnitEnergyDens_)/Si2No_V(UnitT_)
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          CoronalHeating_C(i,j,k) = CoronalHeating_C(i,j,k) &
               + HeatCh*exp(-max(r_GB(i,j,k,iBlock) - 1, 0.0)/DecayLengthCh)
       end do; end do; end do
    end if

    if(UseExponentialHeating .and. UseArComponent) then
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(UseB0)then
             B_D = State_VGB(Bx_:Bz_,i,j,k,iBlock) + B0_DGB(x_:z_,i,j,k,iBlock)
          else
             B_D = State_VGB(Bx_:Bz_,i,j,k,iBlock)
          end if

          Bcell = No2Io_V(UnitB_) * norm2(B_D)

          FractionB = 0.5*(1.0+tanh((Bcell - ArHeatB0)/DeltaArHeatB0))
          CoronalHeating_C(i,j,k) = max(CoronalHeating_C(i,j,k),&
               FractionB * ArHeatFactorCgs * Bcell &
               * 0.1 * Si2No_V(UnitEnergyDens_)/Si2No_V(UnitT_))
       end do; end do; end do
    endif
    if(DoExtendTransitionRegion .and. .not.UseAlfvenWaveDissipation)then
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          CoronalHeating_C(i,j,k) = CoronalHeating_C(i,j,k) &
               /extension_factor(TeSi_C(i,j,k))
       end do; end do; end do
    end if
#endif
    call test_stop(NameSub, DoTest, iBlock)

  end subroutine get_block_heating
  !============================================================================
end module ModCoronalHeating
!==============================================================================
