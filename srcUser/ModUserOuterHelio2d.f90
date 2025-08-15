!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModUser

  ! used for MSWIM2D Outer Heliosphere model
  ! reference paper DOI: 10.3847/1538-4365/ac67eb

  use ModSize, ONLY: nI, nJ, nK, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, nBlock
  use BATL_lib, ONLY: test_start, test_stop, iTest, jTest, kTest, iProcTest, &
       iBlockTest, iVarTest, iProc, IsCylindrical
  use ModMain, ONLY: body1_, nBlock, Unused_B
  use ModPhysics, ONLY: Gamma, GammaMinus1, UnitX_, Io2Si_V, Si2Io_V, No2Io_V,&
       No2Si_V, Io2No_V,  NameTecUnit_V, UnitAngle_, UnitDivB_, &
       UnitEnergyDens_, UnitJ_, UnitN_, UnitRho_, UnitU_, rBody, UnitB_, &
       UnitP_, UnitTemperature_, UnitT_, UnitRhoU_, Si2No_V
  use ModNumConst, ONLY: cRadToDeg, cPi, cTwoPi
  use ModConst, ONLY: cBoltzmann, cProtonMass
  use ModBatsrusUtility, ONLY: stop_mpi
  use ModAdvance, ONLY: State_VGB, Source_VC, ExtraSource_ICB, nFlux
  use ModGeometry, ONLY: Xyz_DGB, r_GB, Used_GB
  use ModVarIndexes, ONLY: nVar, Rho_, Ux_, Uy_, Uz_, RhoUx_, RhoUy_, RhoUz_, &
       Bx_, By_, Bz_, p_, Energy_, iRho_I, iRhoUx_I, iRhoUy_I, iRhoUz_I, iP_I,&
       nFluid, NameVar_V
  use ModMultiFluid, ONLY: UseNeutralFluid, RhoNeutralsISW, &
       RhoNeuWindDim, PNeutralsISW, &
       UxNeutralsISW, UyNeutralsISW, UzNeutralsISW, TempNeuWindDim, &
       UxNeuWindDim, UyNeuWindDim, UzNeuWindDim, &
       MassNeutralDim, iRho, iRhoUx, iRhoUy, iRhoUz, iP, iEnergy, &
       RhoBcFactor_I, uBcFactor_I, select_fluid
  use ModUserEmpty,                                     &
       IMPLEMENTED1 => user_read_inputs,                &
       IMPLEMENTED2 => user_set_face_boundary,          &
       IMPLEMENTED3 => user_set_cell_boundary,          &
       IMPLEMENTED4 => user_set_ics,                    &
       IMPLEMENTED5 => user_update_states,              &
       IMPLEMENTED6 => user_action,                     &
       IMPLEMENTED7 => user_calc_sources_expl,          &
       IMPLEMENTED8 => user_calc_sources_impl,          &
       IMPLEMENTED9 => user_init_session

  include 'user_module.h' ! list of public methods

  character (len=*), parameter :: NameUserFile = "ModUserOuterHelio2d.f90"
  character (len=*), parameter :: NameUserModule = "2D Outer Heliosphere"

  ! Variables used for multiflud neutrals
  ! that are not defined in ModEquationMhd.f90 used for K-MHD
  integer, parameter :: &
       NeuRho_    = min(nVar, 9), &
       NeuRhoUx_  = min(nVar-2, 10), NeuUx_ = NeuRhoUx_, &
       NeuRhoUy_  = min(nVar-1, 11), NeuUy_ = NeuRhoUy_, &
       NeuRhoUz_  = min(nVar  , 12), NeuUz_ = NeuRhoUz_, &
       NeuP_      = min(nVar  , 13), &
       Ne2Rho_    = min(nVar  , 14), &
       Ne2RhoUx_  = min(nVar-2, 15), Ne2Ux_ = Ne2RhoUx_, &
       Ne2RhoUy_  = min(nVar-1, 16), Ne2Uy_ = Ne2RhoUy_, &
       Ne2RhoUz_  = min(nVar  , 17), Ne2Uz_ = Ne2RhoUz_, &
       Ne2P_      = min(nVar  , 18), &
       Ne3Rho_    = min(nVar  , 19), &
       Ne3RhoUx_  = min(nVar-2, 20), Ne3Ux_ = Ne3RhoUx_, &
       Ne3RhoUy_  = min(nVar-1, 21), Ne3Uy_ = Ne3RhoUy_, &
       Ne3RhoUz_  = min(nVar  , 22), Ne3Uz_ = Ne3RhoUz_, &
       Ne3P_      = min(nVar  , 23), &
       Ne4Rho_    = min(nVar  , 24), &
       Ne4RhoUx_  = min(nVar-2, 25), Ne4Ux_ = Ne4RhoUx_, &
       Ne4RhoUy_  = min(nVar-1, 26), Ne4Uy_ = Ne4RhoUy_, &
       Ne4RhoUz_  = min(nVar  , 27), Ne4Uz_ = Ne4RhoUz_, &
       Ne4P_      = min(nVar  , 28), &
       NeuEnergy_ = min(nFlux , 30), &
       Ne4Energy_ = min(nFlux , 33)

  ! Named indexes for fluids
  integer, parameter :: Ion_ = 1, Neu_ = min(nFluid,2), &
       Ne2_ = min(nFluid,3), Ne3_ = min(nFluid,4), Ne4_= min(nFluid,5)

  logical :: UseSource_I(Ion_:Ne4_) = .true.

  real :: OmegaSun   = 0.0  ! normalized angular speed of Sun
  real :: ParkerTilt = 0.0  ! Bphi/Br at the equator at r=rBody

  ! SWH variables.
  real :: &
       SwhRho = 0.0, SwhRhoDim = 0.0, &
       SwhP   = 0.0, SwhTDim   = 0.0, &
       SwhUx  = 0.0, SwhUxDim  = 0.0, &
       SwhUy  = 0.0, SwhUyDim  = 0.0, &
       SwhUz  = 0.0, SwhUzDim  = 0.0, &
       SwhBx  = 0.0, SwhBxDim  = 0.0, &
       SwhBy  = 0.0, SwhByDim  = 0.0, &
       SwhBz  = 0.0, SwhBzDim  = 0.0

  ! VLISM variables.
  real :: VliswTDim=0.0, &
       VliswRho = 0.0, VliswRhoDim = 0.0, &
       VliswP   = 0.0, VliswPDim   = 0.0, &
       VliswUx  = 0.0, VliswUxDim  = 0.0, &
       VliswUy  = 0.0, VliswUyDim  = 0.0, &
       VliswUz  = 0.0, VliswUzDim  = 0.0, &
       VliswBx  = 0.0, VliswBxDim  = 0.0, &
       VliswBy  = 0.0, VliswByDim  = 0.0, &
       VliswBz  = 0.0, VliswBzDim  = 0.0

  ! neutrals variables
  real :: MassNeutral

  ! Velocity, temperature, Mach number and radius limits for the populations
  real :: TempPop1LimitDim = 1e5    ! [K]
  real :: uPop1LimitDim    = 100.0  ! [km/s]
  real :: MachPop2Limit    = 0.9
  real :: MachPop3Limit    = 1.5
  real :: MachPop4Limit    = 2.0
  real :: rPop3Limit       = 50.0   ! [AU] it is all Pop3 out to rPop3Limit

  integer :: iFluidProduced_C(nI, nJ, nK)
  !$omp threadprivate(iFluidProduced_C)

  ! Neutral initial and boundary states population 4
  real:: RhoNe4Dim, UxNe4Dim, UyNe4Dim, UzNe4Dim, pNe4Dim
  real:: RhoNe4, UxNe4, UyNe4, UzNe4, pNe4

contains
  !============================================================================
  subroutine user_read_inputs

    use ModReadParam

    character (len=100) :: NameCommand
    character (len=*), parameter :: Name='user_read_inputs'
    !--------------------------------------------------------------------------
    do
       if(.not.read_line() ) EXIT
       if(.not.read_command(NameCommand)) CYCLE
       select case(NameCommand)

       case('#USERINPUTEND')
          if(iProc==0) write(*,*)'USERINPUTEND'
          EXIT
       case("#SOLARWINDH")
          call read_var('SwhRhoDim',SwhRhoDim)
          call read_var('SwhTDim'  ,SwhTDim)
          call read_var('SwhUxDim' ,SwhUxDim)
          call read_var('SwhUyDim' ,SwhUyDim)
          call read_var('SwhUz_dNe4Uz Ne4Pim' ,SwhUzDim)
          call read_var('SwhBxDim' ,SwhBxDim)
          call read_var('SwhByDim' ,SwhByDim)
          call read_var('SwhBzDim' ,SwhBzDim)
       case("#VLISW")
          call read_var('VliswRhoDim' ,VliswRhoDim)
          call read_var('VliswTDim'  ,VliswTDim)
          call read_var('VliswUxDim' ,VliswUxDim)
          call read_var('VliswUyDim' ,VliswUyDim)
          call read_var('VliswUzDim' ,VliswUzDim)
          call read_var('VliswBxDim' ,VliswBxDim)
          call read_var('VliswByDim' ,VliswByDim)
          call read_var('VliswBzDim' ,VliswBzDim)
       case("#SOURCES")
          call read_var('UseIonSource', UseSource_I(Ion_))
          call read_var('UseNeuSource', UseSource_I(Neu_))
          call read_var('UseNe2Source', UseSource_I(Ne2_))
          call read_var('UseNe3Source', UseSource_I(Ne3_))
          call read_var('UseNe4Source', UseSource_I(Ne4_))
       case("#REGIONS")
          call read_var('TempPop1LimitDim', TempPop1LimitDim)
          call read_var('uPop1LimitDim',    uPop1LimitDim)
          call read_var('MachPop2Limit',    MachPop2Limit)
          call read_var('MachPop3Limit',    MachPop3Limit)
          call read_var('rPop3Limit',       rPop3Limit)
          call read_var('MachPop4Limit',    MachPop4Limit)
       case("#NEUTRAL4")
          call read_var('RhoNe4', RhoNe4Dim)
          call read_var('UxNe4',  UxNe4Dim)
          call read_var('UyNe4',  UyNe4Dim)
          call read_var('UzNe4',  UzNe4Dim)
          call read_var('pNe4',   pNe4Dim)
       case default
          if(iProc==0) call stop_mpi( &
               'read_inputs: unrecognized command: '//NameCommand)
       end select
    end do

  end subroutine user_read_inputs
  !============================================================================
  subroutine normalize_lookup_solar_wind(LookupIMF_V, VarsGhostFace_V)
    !
    ! Normalize the solar wind read from a lookup table.
    ! This follows the same structure as normalize_solar_wind_data from
    !   ModSolarWind file.
    ! Created 2020.06
    !
    use ModPhysics, ONLY: Io2No_V, UnitTemperature_, UnitN_, UnitRho_, &
         UnitP_, UnitU_, UnitB_, SwTMinDim, FaceState_VI
    use ModMultiFluid, ONLY: MassIon_I

    real, intent(in) :: LookupIMF_V(9)
    real, intent(out):: VarsGhostFace_V(8)
    logical :: UseNumberDensity = .true.
    logical :: UseTemperature = .true.

    logical :: DoTest
    character(len=*), parameter:: NameSub = 'normalize_lookup_solar_wind'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    VarsGhostFace_V(Bx_:Bz_) = LookupIMF_V(2:4)*Io2No_V(UnitB_)
    VarsGhostFace_V(Ux_:Uz_) = LookupIMF_V(5:7)*Io2No_V(UnitU_)
    if(UseNumberDensity) then
       VarsGhostFace_V(Rho_) = LookupIMF_V(8)*Io2No_V(UnitN_)*MassIon_I(1)
    else
       VarsGhostFace_V(Rho_) = LookupIMF_V(8)*Io2No_V(UnitRho_)
    endif
    if(UseTemperature) then
       VarsGhostFace_V(p_) = max(LookupIMF_V(9),SwTMinDim) &
            *Io2No_V(UnitTemperature_)*LookupIMF_V(8)/MassIon_I(1)
    else
       VarsGhostFace_V(p_) = LookupIMF_V(9)*Io2No_V(UnitP_)
    endif

    call test_stop(NameSub, DoTest)

  end subroutine normalize_lookup_solar_wind
  !============================================================================
  subroutine user_set_face_boundary(FBC)

    use ModMain, ONLY: tSimulation, StartTime, FaceBCType
    use ModSolarWind, ONLY: get_solar_wind_point
    use ModConst, ONLY: cSecondPerDay, cDegToRad, RotationPeriodSun, &
         CarringtonSynodicPeriod
    use ModCoordTransform, ONLY: rot_xyz_sph, show_rot_matrix
    use CON_axes, ONLY: OmegaOrbit, XyzPlanetHgi_D
    use ModLookupTable, ONLY: i_lookup_table, interpolate_lookup_table, &
         get_lookup_table

    type(FaceBCType), intent(inout):: FBC

    !! local variables
    real:: xFace, yFace, zFace
    real:: SinTheta
    real:: Bsph_D(3), Vsph_D(3)
    real :: pSolarWind, Pmag, PmagEquator
    real :: XyzSph_DD(3,3) ! rotation matrix Xyz_D = matmul(XyzSph_DD, Sph_D)

    !! For IMF files
    real :: VarsGhostFace2_V(nVar)
    real :: PhiEarth0 = -2000.0 ! Initial Phi coordinate of Earth
    real :: Phi, Time1Weight
    real :: Phi2, Time2Weight
    real :: ShiftedTime1, ShiftedTime2

    !! For lookup tables
    integer :: MaxLookupTable = 3
    !   allow up to 3 solar wind inputs (L1, STA, STB)
    integer, allocatable :: iLookupTable_I(:)
    real :: VarsSatR_V(8), VarsSatL_V(8)
    real :: LookupData_V(9)
    character(len=3):: NameTable
    integer         :: i
    real            :: PhiDiff, PhiFace, PhiSat
    real            :: PhiMinDiffR, PhiMinDiffL
    real            :: PhiDiffR, PhiDiffL
    integer         :: iTableMinPhiR = 1, iTableMinPhiL = 1
    real            :: SatR_V(9), SatL_V(9)
    real            :: SatRWeight, SatLWeight
    real            :: ShiftedTimeR, ShiftedTimeL
    real            :: TimeFirst_I(3), TimeLast_I(3)
    real            :: IndexMin_I(1), IndexMax_I(1)

    logical:: DoTest, DoTestCell
    logical:: DoDebug = .false.

    character(len=*), parameter:: NameSub = 'user_set_face_boundary'
    !--------------------------------------------------------------------------
    associate( VarsGhostFace_V => FBC%VarsGhostFace_V, &
         VarsTrueFace_V => FBC%VarsTrueFace_V, &
         FaceCoords_D => FBC%FaceCoords_D, &
         iBlockBc => FBC%iBlockBc, &
         iFace => FBC%iFace, jFace => FBC%jFace, kFace => FBC%kFace, &
         iBoundary => FBC%iBoundary )

      call test_start(NameSub, DoTest)

      if(iBoundary /= body1_ .and. iBoundary /= 1)then
         write(*,*) NameSub,': iBoundary=', iBoundary
         call stop_mpi(NameSub//' is not implemented for this boundary!')
      end if

      xFace = FaceCoords_D(1)
      yFace = FaceCoords_D(2)
      zFace = FaceCoords_D(3)

      ! The IMF file contains X, Y, Z coordinates,
      if(IsCylindrical)then
         ! Rotation is taken in the ecliptic plane
         XyzSph_DD = rot_xyz_sph(FaceCoords_D(1), FaceCoords_D(2), 0.0)
      else
         ! but we interpret them as R-Lon-Lat
         ! convert face coordinates from Car to sph
         XyzSph_DD = rot_xyz_sph(FaceCoords_D)
      end if

      ! Detect the number of solar wind lookup tables provided.
      do i = 1, MaxLookupTable
         write(NameTable,'(A,I1)') 'SW', i
         if(.not.allocated(iLookupTable_I) .and. &
              i_lookup_table(NameTable) > 0)then
            allocate(iLookupTable_I(MaxLookupTable))
         endif
         if(allocated(iLookupTable_I))then
            iLookupTable_I(i) = i_lookup_table(NameTable)
         endif
      enddo

      if(allocated(iLookupTable_I))then
         !----LOOKUP TABLE(S) PROVIDED----

         ! Determine which satellites are closest to the cell.
         ! Do this by getting data from all lookup tables.
         !  Note: NO TIME SHIFT YET!
         PhiMinDiffR = cTwoPi
         PhiMinDiffL = cTwoPi
         iTableMinPhiR = 1
         iTableMinPhiL = 1

         ! Find and store index max and min of each lookup table.
         do i=1,size(iLookupTable_I)
            if(iLookupTable_I(i)<0) CYCLE
            call get_lookup_table(iTable=iLookupTable_I(i), &
                 IndexMin_I=IndexMin_I, IndexMax_I=IndexMax_I)
            TimeFirst_I(i) = IndexMin_I(1)
            TimeLast_I(i) = IndexMax_I(1)
         enddo

         PhiFace = atan2(yFace, xFace)
         if(PhiFace > cPi) PhiFace = PhiFace - cTwoPi

         ! Find two satellites closest to cell on right and left.
         do i=1, size(iLookupTable_I)
            if(iLookupTable_I(i)<0) CYCLE
            CALL interpolate_lookup_table(iLookupTable_I(i), &
                 StartTime+tSimulation, LookupData_V, DoExtrapolate=.false.)

            ! Ignore values outside of lookup table.
            if(StartTime+tSimulation < TimeFirst_I(i) .or. &
                 StartTime+tSimulation > TimeLast_I(i)) CYCLE

            ! Compute angular locations in the interval (-Pi, Pi)
            PhiSat = LookupData_V(1)*cDegToRad
            if(PhiSat > cPi) PhiSat = PhiSat - cTwoPi

            ! Calculate both angular differences between satellite and cell
            !   Note: both differences are positive.
            PhiDiff = PhiFace - PhiSat
            if(PhiDiff < 0)then
               PhiDiffL = -PhiDiff
               PhiDiffR = cTwoPi - PhiDiffL
            else
               PhiDiffR = PhiDiff
               PhiDiffL = cTwoPi - PhiDiffR
            endif

            ! If differences are smallest, store as global minima.
            if(PhiDiffR < PhiMinDiffR)then
               PhiMinDiffR = PhiDiffR
               iTableMinPhiR = iLookupTable_I(i)
            endif
            if(PhiDiffL < PhiMinDiffL)then
               PhiMinDiffL = PhiDiffL
               iTableMinPhiL = iLookupTable_I(i)
            endif
         enddo

         ! Get shifted time of observation at cell
         ShiftedTimeR = StartTime + tSimulation &
              - PhiMinDiffR/cTwoPi*CarringtonSynodicPeriod
         ShiftedTimeL = StartTime + tSimulation &
              + PhiMinDiffL/cTwoPi*CarringtonSynodicPeriod

         ! Get IMF data from each closest satellite to the cell.
         call interpolate_lookup_table(iTableMinPhiR, ShiftedTimeR, &
              SatR_V, DoExtrapolate=.false.)
         call normalize_lookup_solar_wind(SatR_V, VarsSatR_V)
         call interpolate_lookup_table(iTableMinPhiL, ShiftedTimeL, &
              SatL_V, DoExtrapolate=.false.)
         call normalize_lookup_solar_wind(SatL_V, VarsSatL_V)

         ! Average observations from both satellites.
         !   -> smaller distance, larger weight
         SatRWeight = PhiMinDiffL/(PhiMinDiffR + PhiMinDiffL)
         SatLWeight = 1 - SatRWeight
         VarsGhostFace_V(Rho_:p_) = VarsSatR_V*SatRWeight &
              + VarsSatL_V*SatLWeight

      else
         !----IMF FILE PROVIDED----
         ! Allows only one input file.

         DoDebug = .false.
         ! Get initial Phi coordinate of Earth
         if(PhiEarth0 < -1000)then
            PhiEarth0 = atan2(XyzPlanetHgi_D(2), XyzPlanetHgi_D(1))
            if(DoDebug)then
               write(*,*) NameSub,': PhiEarth0[deg]=',PhiEarth0*cRadToDeg
               write(*,*) NameSub,': OmegaOrbit, TimeSim=', &
                    OmegaOrbit, tSimulation
               DoDebug = .false.
            end if
         end if
         DoDebug = .false.

         ! Phi angle relative to the Earth (or the OMNI observation).
         ! Include orbital motion of Earth (for long simulations)
         Phi = atan2(yFace, xFace) - PhiEarth0 - OmegaOrbit*tSimulation

         ! Center in a way that the discontinuity is away from Earth
         Phi = modulo(Phi, cTwoPi) ! make Phi < 2pi
         if(Phi > cPi) Phi = Phi - cTwoPi ! if Phi > pi: make Phi negative
         ! get second solar wind point to improve discontinuity
         ! this second point should be less than one full rotation from Earth
         if(Phi > 0)then
            Phi2 = Phi - cTwoPi
         else
            Phi2 = Phi + cTwoPi
         end if

         ! Shift time (assuming steady state in the corotating frame)
         ! Time and tSimulation are in seconds (SI units).
         ShiftedTime1 = tSimulation - Phi/cTwoPi*CarringtonSynodicPeriod
         ShiftedTime2 = tSimulation - Phi2/cTwoPi*CarringtonSynodicPeriod

         ! Time is shifted above, so 1,0,0 should be the same in the IMF file.
         ! Thus, no additional time shift is applied in get_solar_wind_point.
         ! This functionality is designed to propagate solar wind from L1,
         ! but is not needed.
         call get_solar_wind_point(ShiftedTime1, [1.0, 0.0, 0.0], &
              VarsGhostFace_V)
         call get_solar_wind_point(ShiftedTime2, [1.0, 0.0, 0.0], &
              VarsGhostFace2_V)

         ! Average observations from consecutive rotations to improve solution
         ! and continuity.
         ! Note that the unweighted average preserves a sharp discontinuity,
         ! whereas the weighted average smooths it completely.
         ! -> weighted average based on fractional angular distance from Earth
         Time2Weight = abs(Phi/cTwoPi)
         Time1Weight = 1 - Time2Weight
         VarsGhostFace_V = &
              VarsGhostFace_V*Time1Weight + VarsGhostFace2_V*Time2Weight

      endif

      ! In IMF file, X is R, Y is PHI, Z is THETA
      !   so here, we need (/ X , -Z , Y /) as R, THETA, PHI
      ! u_theta component ignored
      !   could be included if periodic in Latitude is required
      Vsph_D = [ VarsGhostFace_V(Ux_), 0.0, VarsGhostFace_V(Uy_) ]
      Bsph_D = [ VarsGhostFace_V(Bx_), &
           -VarsGhostFace_V(Bz_), VarsGhostFace_V(By_) ]

      ! Convert to X,Y,Z components
      VarsGhostFace_V(Ux_:Uz_) = matmul(XyzSph_DD, Vsph_D)
      VarsGhostFace_V(Bx_:Bz_) = matmul(XyzSph_DD, Bsph_D)

      DoDebug = .false.
      if(iProc == 0 .and. DoDebug) &
           write(*,*) NameSub, ' GhostFace_V=', VarsGhostFace_V(1:8)

      if(UseNeutralFluid)then

         VarsGhostFace_V(NeuRho_:Ne4P_) = VarsTrueFace_V(NeuRho_:Ne4P_)

         ! PopI/Neu (disturbed by bow shock)
         if(sum([UxNeutralsISW,UyNeutralsISW,UzNeutralsISW]*FaceCoords_D) &
              > 0.0)then
            VarsGhostFace_V(NeuRho_) = RhoNeutralsISW * RhoBcFactor_I(Neu_)
            VarsGhostFace_V(NeuUx_ ) = UxNeutralsISW  * uBcFactor_I(Neu_)
            VarsGhostFace_V(NeuUy_ ) = UyNeutralsISW  * uBcFactor_I(Neu_)
            VarsGhostFace_V(NeuUz_ ) = UzNeutralsISW  * uBcFactor_I(Neu_)
            VarsGhostFace_V(NeuP_)   = PNeutralsISW   * RhoBcFactor_I(Neu_)
         else
            VarsGhostFace_V(NeuRho_:NeuP_) = VarsTrueFace_V(NeuRho_:NeuP_)
         end if

         ! PopIV/Ne4 (undisturbed)
         if( sum([ UxNe4, UyNe4, UzNe4 ]*FaceCoords_D) > 0.0)then
            VarsGhostFace_V(Ne4Rho_) = RhoNe4 * RhoBcFactor_I(Ne4_)
            VarsGhostFace_V(Ne4Ux_ ) = UxNe4  * uBcFactor_I(Ne4_)
            VarsGhostFace_V(Ne4Uy_ ) = UyNe4  * uBcFactor_I(Ne4_)
            VarsGhostFace_V(Ne4Uz_ ) = UzNe4  * uBcFactor_I(Ne4_)
            VarsGhostFace_V(Ne4P_  ) = PNe4   * RhoBcFactor_I(Ne4_)
         else
            VarsGhostFace_V(Ne4Rho_:Ne4P_) = VarsTrueFace_V(Ne4Rho_:Ne4P_)
         end if

      endif

      call test_stop(NameSub, DoTest)

    end associate
  end subroutine user_set_face_boundary
  !============================================================================
  subroutine user_set_cell_boundary(iBlock, iSide, TypeBc, IsFound)

    ! The ISM enters at the east boundary (negative x)

    integer,          intent(in) :: iBlock, iSide
    character(len=*), intent(in) :: TypeBc
    logical,          intent(out):: IsFound

    integer :: i,j,k

    logical :: DoTest
    character(len=*), parameter:: NameSub = 'user_set_cell_boundary'
    !--------------------------------------------------------------------------

    call test_start(NameSub, DoTest, iProc)

    if(iSide /= 2)then
       IsFound = .false.
       RETURN
    endif
    IsFound = .true.

    do k = MinK, MaxK; do j = MinJ, MaxJ; do i = nI+1, MaxI
       State_VGB(:,i,j,k,iBlock) = State_VGB(:,nI,j,k,iBlock)
       if(UseNeutralFluid)then
          if(sum(Xyz_DGB(:,i,j,k,iBlock) &
               * State_VGB(NeuRhoUx_:NeuRhoUz_,nI,j,k,iBlock)) < 0)then
             State_VGB(NeuRho_  ,i,j,k,iBlock) = RhoNeutralsISW
             State_VGB(NeuRhoUx_,i,j,k,iBlock) = RhoNeutralsISW*UxNeutralsISW
             State_VGB(NeuRhoUy_,i,j,k,iBlock) = RhoNeutralsISW*UyNeutralsISW
             State_VGB(NeuRhoUz_,i,j,k,iBlock) = RhoNeutralsISW*UzNeutralsISW
             State_VGB(NeuP_    ,i,j,k,iBlock) = PNeutralsISW
          endif
          if(sum(Xyz_DGB(:,i,j,k,iBlock) &
               * State_VGB(Ne4RhoUx_:Ne4RhoUz_,nI,j,k,iBlock)) < 0)then
             State_VGB(Ne4Rho_   ,i,j,k,iBlock) = RhoNe4
             State_VGB(Ne4RhoUx_ ,i,j,k,iBlock) = RhoNe4 * UxNe4
             State_VGB(Ne4RhoUy_ ,i,j,k,iBlock) = RhoNe4 * UyNe4
             State_VGB(Ne4RhoUz_ ,i,j,k,iBlock) = RhoNe4 * UzNe4
             State_VGB(Ne4P_     ,i,j,k,iBlock) = PNe4
          endif
       endif
    end do; end do; end do

    call test_stop(NameSub, DoTest, iProc)

  end subroutine user_set_cell_boundary
  !============================================================================
  subroutine user_set_ics(iBlock)

    use ModPhysics,  ONLY: rBody
    use ModCoordTransform, ONLY: rot_xyz_sph

    integer, intent(in) :: iBlock

    integer :: i,j,k

    real :: x, y, z, r
    real :: b_D(3), v_D(3), bSph_D(3), vSph_D(3)
    real :: SinTheta, SignZ
    real :: XyzSph_DD(3,3) ! rotation matrix Xyz_D = matmul(XyzSph_DD, Sph_D)

    logical :: DoTest, DoTestCell
    character(len=*), parameter:: NameSub = 'user_set_ics'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Make sure that OmegaSun and ParkerTilt are set
    if(OmegaSun == 0.0) call set_omega_parker_tilt

    do i=MinI,MaxI; do j=MinJ,MaxJ; do k=MinK,MaxK

       if(.not. Used_GB(i,j,k,iBlock)) CYCLE

       DoTestCell = DoTest .and. i==iTest .and. j==jTest .and. k==kTest

       x = Xyz_DGB(x_,i,j,k,iBlock)
       y = Xyz_DGB(y_,i,j,k,iBlock)
       z = Xyz_DGB(z_,i,j,k,iBlock)

       if(IsCylindrical)then
          ! Rotation is taken in the ecliptic plane
          XyzSph_DD = rot_xyz_sph(x, y, 0.0)
          ! No dependence on theta
          r = sqrt(x**2 + y**2)
          SinTheta = 1.0
       else
          ! but we interpret them as R-Lon-Lat
          ! convert face coordinates from Car to sph
          XyzSph_DD = rot_xyz_sph(x, y, z)
          ! theta is the angle measured from the pole
          r = r_GB(i,j,k,iBlock)
          SinTheta = sqrt(x**2 + y**2)/r
       end if

       ! calculating the Parker B field spherical components Bsph_D
       Bsph_D(1) =  SwhBx*(rBody/r)**2                  ! Br
       Bsph_D(2) =  0.0                                 ! Btheta
       Bsph_D(3) = -SwhBx*SinTheta*ParkerTilt*(rBody/r) ! Bphi

       ! wrong:   Vphi = OmegaSun*(6.96E5)*sin_theta/No2Io_V(UnitU_)
       ! correct: Vphi = OmegaSun*sin_theta*rSun**2/r
       ! rSun must be in normalized units, of course
       ! Vphi is approximately 0. (30km/s at 1AU, 1km/s at 30AU)

       Vsph_D = [SwhUx, 0., 0.]

       ! magnetic field components in cartesian coordinates
       b_D = matmul(XyzSph_DD, Bsph_D)
       State_VGB(Bx_:Bz_,i,j,k,iBlock) = b_D

       ! velocity components in cartesian coordinates
       v_D = matmul(XyzSph_DD, Vsph_D)

       ! density and pressure
       State_VGB(Rho_,i,j,k,iBlock) = SwhRho * (rBody/r)**2
       State_VGB(P_,i,j,k,iBlock)   = SwhP   * (rBody/r)**(2*Gamma)

       ! momentum
       State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) = State_VGB(rho_,i,j,k,iBlock)*v_D

       if(UseNeutralFluid)then

          ! PopI
          State_VGB(NeuRho_,i,j,k,iBlock)   = RhoNeutralsISW
          State_VGB(NeuP_,i,j,k,iBlock)     = PNeutralsISW
          State_VGB(NeuRhoUx_,i,j,k,iBlock) = RhoNeutralsISW*UxNeutralsISW
          State_VGB(NeuRhoUy_,i,j,k,iBlock) = RhoNeutralsISW*UyNeutralsISW
          State_VGB(NeuRhoUz_,i,j,k,iBlock) = RhoNeutralsISW*UzNeutralsISW

          ! PopII - set to be about 100km/s radially.
          ! The temperature is set to be 10^5K for this population.
          ! v_D is the plasma velocity, we take one quarter of that.
          State_VGB(Ne2Rho_,i,j,k,iBlock) = 1e-3 * RhoNeutralsISW
          State_VGB(Ne2P_,i,j,k,iBlock)   = 1e-3 * PNeutralsISW
          State_VGB(Ne2RhoUx_:Ne2RhoUz_,i,j,k,iBlock) = &
               0.25*State_VGB(Ne2Rho_,i,j,k,iBlock)*v_D

          ! PopIII
          State_VGB(Ne3Rho_,i,j,k,iBlock) = 0.1 * State_VGB(Rho_,i,j,k,iBlock)
          State_VGB(Ne3P_,i,j,k,iBlock)   = 0.1 * State_VGB(p_,i,j,k,iBlock)
          State_VGB(Ne3RhoUx_:Ne3RhoUz_,i,j,k,iBlock) = &
               State_VGB(Ne3Rho_,i,j,k,iBlock)*v_D

          ! PopIV
          State_VGB(Ne4Rho_,i,j,k,iBlock)  = RhoNe4
          State_VGB(Ne4RhoUx_,i,j,k,iBlock)= RhoNe4*UxNe4
          State_VGB(Ne4RhoUy_,i,j,k,iBlock)= RhoNe4*UyNe4
          State_VGB(Ne4RhoUz_,i,j,k,iBlock)= RhoNe4*UzNe4
          State_VGB(Ne4P_,i,j,k,iBlock)    = PNe4

       endif

       if(DoTestCell)then
          write(*,*)NameSub,' x, y, z, r             =', x, y, z, r
          write(*,*)NameSub,' SignZ, SwhBx, SinTheta=',SignZ, SwhBx, SinTheta
          write(*,*)NameSub,' OmegaSun, rBody, SwhUx=',OmegaSun,rBody,SwhUx
          write(*,*)NameSub,' Vsph_D                 =',Vsph_D
          write(*,*)NameSub,' v_D                    =',v_D
          write(*,*)NameSub,' Bsph_D                 =',Bsph_D
          write(*,*)NameSub,' b_D    =',State_VGB(Bx_:Bz_,i,j,k,iBlock)
          write(*,*)NameSub,' RhoU_D =',State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)
          write(*,*)NameSub,' rho,   =',State_VGB(Rho_,i,j,k,iBlock)
          write(*,*)NameSub,' p      =',State_VGB(P_,i,j,k,iBlock)
          if(UseNeutralFluid)then
             write(*,*)NameSub,' NeuRho   =',State_VGB(NeuRho_,i,j,k,iBlock)
             write(*,*)NameSub,' NeuRhoUx =',State_VGB(NeuRhoUx_,i,j,k,iBlock)
             write(*,*)NameSub,' NeuRhoUy =',State_VGB(NeuRhoUy_,i,j,k,iBlock)
             write(*,*)NameSub,' NeuRhoUz =',State_VGB(NeuRhoUz_,i,j,k,iBlock)
             write(*,*)NameSub,' NeuP     =',State_VGB(NeuP_,i,j,k,iBlock)
          endif
       end if

    end do; end do; end do

    call test_stop(NameSub, DoTest)

  end subroutine user_set_ics
  !============================================================================
  subroutine user_action(NameAction)

    character(len=*), intent(in):: NameAction

    character(len=*), parameter:: StringFormat = '(10X,A19,F15.6,A11,F15.6)'

    logical :: DoTest
    character(len=*), parameter:: NameSub = 'user_action'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(NameAction /= 'write progress') RETURN

    write(*,StringFormat) 'SwhRhoDim [n/cc]:',SwhRhoDim,'SwhRho:',SwhRho

    write(*,StringFormat) 'SwhUxDim  [km/s]:',SwhUxDim,'SwhUx:',SwhUx
    write(*,StringFormat) 'SwhUyDim  [km/s]:',SwhUyDim,'SwhUy:',SwhUy
    write(*,StringFormat) 'SwhUzDim  [km/s]:',SwhUzDim,'SwhUz:',SwhUz
    write(*,StringFormat) 'SwhTDim   [   K]:',SwhTDim,'SwhP:',SwhP
    write(*,StringFormat) 'SwhBxDim  [  nT]:',SwhBxDim,'SwhBx:',SwhBx
    write(*,StringFormat) 'SwhByDim  [  nT]:',SwhByDim,'SwhBy:',SwhBy
    write(*,StringFormat) 'SwhBzDim  [  nT]:',SwhBzDim,'SwhBz:',SwhBz
    write(*,'(10X,A19,F15.6)') 'SwhTDim   [   K]:',SwhTDim
    ! fast solar wind
    write(*,*)
    write(*,*)
    write(*,StringFormat) 'VliswRhoDim[n/cc]:',VliswRhoDim,'VliswRho:',VliswRho
    write(*,StringFormat) 'VliswUxDim[km/s]: ',VliswUxDim,'VliswUx:',VliswUx
    write(*,StringFormat) 'VliswUyDim[km/s]: ',VliswUyDim,'VliswUy:',VliswUy
    write(*,StringFormat) 'VliswUzDim[km/s]: ',VliswUzDim,'VliswUz:',VliswUz
    write(*,StringFormat) 'VliswPDim [nPa]: ',VliswPDim,'VliswP:',VliswP
    write(*,StringFormat) 'VliswBxDim[nT]: ',VliswBxDim,'VliswBx:',VliswBx
    write(*,StringFormat) 'VliswByDim[nT]:',VliswByDim,'VliswBy:',VliswBy
    write(*,StringFormat) 'VliswBzDim[nT]:',VliswBzDim,'VliswBz:',VliswBz
    write(*,'(10X,A19,F15.6)') 'VliswTDim[K]: ',VliswTDim!
    if(UseNeutralFluid)then
       ! neutrals
       write(*,*)
       write(*,StringFormat) &
            'RhoNeuWindDim:',RhoNeuWindDim ,'RhoNeutralsISW:',RhoNeutralsISW
       write(*,StringFormat) &
            'UxNeuWindDim:',UxNeuWindDim,'UxNeutralsISW:',UxNeutralsISW
       write(*,StringFormat) &
            'UyNeuWindDim:',UyNeuWindDim,'UyNeutralsISW:',UyNeutralsISW
       write(*,StringFormat) &
            'UzNeuWindDim:',UzNeuWindDim,'UzNeutralsISW:',UzNeutralsISW
       write(*,'(10X,A19,F15.6)') 'TempNeuWindDim:',TempNeuWindDim
       write(*,*)
       write(*,StringFormat) 'RhoNe4Dim:',RhoNe4Dim ,'RhoNe4:',RhoNe4
       write(*,StringFormat) 'UxNe4Dim:',UxNe4Dim,'UxNe4:',UxNe4
       write(*,StringFormat) 'UyNe4Dim:',UyNe4Dim,'UyNe4:',UyNe4
       write(*,StringFormat) 'UzNe4Dim:',UzNe4Dim,'UzNe4:',UzNe4
       write(*,StringFormat) 'PNe4Dim:',PNe4Dim,'PNe4:',PNe4
    endif
    write(*,*)
    call test_stop(NameSub, DoTest)
  end subroutine user_action
  !============================================================================
  subroutine user_calc_sources_expl(iBlock)

    ! Calculates the charge exchange cross sections for the neutrals
    ! This subroutine calculates the charge exchange between the ionized
    ! component and the three population of neutrals, Neu, Ne2, Ne3
    ! following the notation of McNutt (1998)
    ! The three population of neutrals are the neutrals created by
    ! charge exchange in different regions of the outer heliosphere
    !
    ! Neu are the neutrals the comes from the interstellar medium  (PopI)
    ! Ne2 are created between the Termination Shock and Heliopause (PopII)
    ! Ne3 are the neutrals created inside the Termination Shock    (PopIII)
    !
    ! As an example for Neu the source terms inside the Termination Shock will
    ! take into account their creations and destruction;
    ! outside the Termination Shock they will be just destroyed
    ! (see more details below).
    !
    ! The _I(1) is the ionized fluid and _I(Neu_)-_I(Ne4_) are the neutral
    ! fluids.
    !
    ! Written by Merav Opher 04/21/2002; Help From Gabor Toth

    use ModNumConst

    integer, intent(in) :: iBlock

    character (len=*), parameter :: Name='user_calc_sources'

    real :: State_V(nVar)
    real :: Ux, Uy, Uz, U2

    real, dimension(nFluid) :: &
         Ux_I=0.0, Uy_I=0.0, Uz_I=0.0, U2_I=0.0, Temp_I=0.0, &
         UThS_I=0.0, URelS_I=0.0, URelSdim_I=0.0, UStar_I=0.0, &
         Sigma_I=0.0, Rate_I=0.0, UStarM_I=1.0, SigmaN_I=0.0, RateN_I=0.0, &
         I0xp_I=0.0, I0px_I=0.0, I2xp_I=0.0, I2px_I=0.0, &
         JxpUx_I=0.0, JxpUy_I=0.0, JxpUz_I=0.0, &
         JpxUx_I=0.0, JpxUy_I=0.0, JpxUz_I=0.0, &
         Kxp_I=0.0, Kpx_I=0.0, Qepx_I=0.0, &
         QmpxUx_I=0.0, QmpxUy_I=0.0, QmpxUz_I=0.0

    integer :: i, j, k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_calc_sources_expl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! Figure out which neutral population is produced at this point
    call select_region(iBlock)

    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       ! Extract conservative variables
       State_V = State_VGB(:, i, j, k, iBlock)

       ! Compute velocity components from momentum.
       Ux_I  = State_V(iRhoUx_I)/State_V(iRho_I)
       Uy_I  = State_V(iRhoUy_I)/State_V(iRho_I)
       Uz_I  = State_V(iRhoUz_I)/State_V(iRho_I)

       ! Velocity squared for the ionized and four population of neutrals.
       U2_I  = Ux_I**2 + Uy_I**2 + Uz_I**2

       ! Temperature for the ionized and four population of neutrals (K)
       ! T = p/rho
       ! since P_plasma=2T_proton*rho_ion; T_proton=0.5P_plasma/rho_ion
       Temp_I       = (State_V(iP_I)/State_V(iRho_I))*No2Si_V(UnitTemperature_)
       Temp_I(Ion_) = 0.5*Temp_I(Ion_)

       ! Thermal speed (squared) for ionized and four populations of neutrals
       ! UThS units are (m/s)^2

       ! Thermal speed squared
       UThS_I = Temp_I*2.0*cBoltzmann/MassNeutral

       ! Relative velocity between neutrals and ionized fluid squared
       URelS_I(1)    = 0.0
       URelS_I(Neu_) = (Ux_I(Neu_) - Ux_I(1))**2 &
            + (Uy_I(Neu_) - Uy_I(1))**2 &
            + (Uz_I(Neu_) - Uz_I(1))**2
       URelS_I(Ne4_) = (Ux_I(Ne4_) - Ux_I(1))**2 &
            + (Uy_I(Ne4_) - Uy_I(1))**2 &
            + (Uz_I(Ne4_) - Uz_I(1))**2

       ! Calculating Cross Section Sigma_I for the different neutrals
       !
       ! Incorporating units to calculate the charge exchange cross sections
       ! No2Si_V(UnitU_) has units of m/s like cstartT so UReldim and UStar
       ! has units of m/s
       !  URelSdim_I  = URelS_I * No2Si_V(UnitU_)**2
       URelSdim_I(1)    = 0.0
       URelSdim_I(Neu_) = URelS_I(Neu_) * No2Si_V(UnitU_)**2
       URelSdim_I(Ne4_) = URelS_I(Ne4_) * No2Si_V(UnitU_)**2

       ! UStar_I has units of m/s
       UStar_I(1)    = 0.0
       UStar_I(Neu_) &
            = sqrt(URelSdim_I(Neu_) + (4/cPi)*(UThS_I(Neu_) +UThS_I(1)))
       UStar_I(Ne4_) &
            = sqrt(URelSdim_I(Ne4_) + (4./cPi)*(UThS_I(Ne4_) +UThS_I(1)))

       ! UStarM_I has units of m/s
       !!   UStarM_I  = sqrt(URelSdim_I + (64./(9.*cPi))*(UThS_I +UThS_I(1)))

       UStarM_I(1)    = 1.0
       UStarM_I(Neu_) = &
            sqrt(URelSdim_I(Neu_) + (64./(9.*cPi))*(UThS_I(Neu_) +UThS_I(1)))
       UStarM_I(Ne4_) = &
            sqrt(URelSdim_I(Ne4_) + (64./(9.*cPi))*(UThS_I(Ne4_) +UThS_I(1)))

       ! Maher and Tinsley cross section Sigma
       ! UStar has to have units of cm/s so the factor 100 is to pass m to cm
       ! Sigma has units of units of m^2
       !    Sigma_I =((1.64E-7 - (6.95E-9)*log(UStarM_I*100.))**2)*(1.E-4)
       !    SigmaN_I =((1.64E-7 - (6.95E-9)*log(UStar_I*100.))**2)*(1.E-4)
       Sigma_I(1)     = 0.0
       SigmaN_I(1)    = 0.0
       Sigma_I(Neu_)  = &
            ((1.64E-7 - (6.95E-9)*log(UStarM_I(Neu_)*100.))**2)*(1.E-4)
       SigmaN_I(Neu_) = &
            ((1.64E-7 - (6.95E-9)*log(UStar_I(Neu_)*100.))**2)*(1.E-4)
       Sigma_I(Ne4_)  = &
            ((1.64E-7 - (6.95E-9)*log(UStarM_I(Ne4_)*100.))**2)*(1.E-4)
       SigmaN_I(Ne4_) = &
            ((1.64E-7 - (6.95E-9)*log(UStar_I(Ne4_)*100.))**2)*(1.E-4)

       ! Calculating Rate  = \nu * nH * mp where nH is the density of neutrals
       ! \nu = Sigma*np*u_star where np is the density of the ionized flow and
       ! For each population of neutrals there will be another rate
       ! The charge exhange cross section 100 to change ustar to cm/s
       ! Rate has no units (m^2*m/s*s*m-3 )
       !! Rate_I =Sigma_I*State_V(Rho_)*State_V(iRho_I)*UStarM_I  &
       !!      *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)
       Rate_I(1)    = 0.0
       Rate_I(Neu_) = &
            Sigma_I(Neu_)*State_V(Rho_)*State_V(iRho_I(Neu_))*UStarM_I(Neu_) &
            *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)
       Rate_I(Ne4_) = &
            Sigma_I(Ne4_)*State_V(Rho_)*State_V(iRho_I(Ne4_))*UStarM_I(Ne4_) &
            *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)

       RateN_I(1)    = 0.0
       RateN_I(Neu_) = &
            SigmaN_I(Neu_)*State_V(Rho_)*State_V(iRho_I(Neu_))*UStar_I(Neu_) &
            *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)
       RateN_I(Ne4_) = &
            SigmaN_I(Ne4_)*State_V(Rho_)*State_V(iRho_I(Ne4_))*UStar_I(Ne4_) &
            *No2Si_V(UnitRho_)*No2Si_V(UnitT_)*(1./cProtonMass)

       ! Calculating the terms that enter in the Source terms
       ! The expressions for I0, Jxp, Kxp, Qexp are taken from
       !      Zank et al. 1996,  https://doi.org/10.1029/96JA02127
       !      Equations layed out in Appendix A
       ! For example for population I; Neu:
       !
       ! Source(density) = I0xpNe2 + I0xpNe3 in region 1
       !                 = - I0pxNeu         otherwise
       !
       ! Source(momentum) = QmxpNeu + JxpNe2 + JxpNe3 in region 1
       !                  = - JpxNeu                  otherwise
       !
       ! Source(energy) = QexpNeu + KxpNe2 + KxpNe3 in region 1
       !                = -KpxNeu                   otherwise
       !
       ! 'xp' indicates neutrals->protons charge exchange rates and
       ! 'px' indicates protons->neutrals
       ! charge exchange
       ! For example:
       ! I0xpNe2 is the term of creation of Neu by charge exchange p-Ne2
       ! I0xpNe3 is the term of creation of Neu by charge exchange p-Ne3

       I0xp_I = RateN_I
       I0px_I = RateN_I

       I2xp_I = Rate_I*(UStar_I/UStarM_I)*UThS_I(1)/No2Si_V(UnitU_)**2
       I2px_I = Rate_I*(UStar_I/UStarM_I)*UThS_I/No2Si_V(UnitU_)**2
       I2xp_I = RateN_I*UThS_I(1)/No2Si_V(UnitU_)**2
       I2px_I = RateN_I*UThS_I/No2Si_V(UnitU_)**2
!       I2xp_I = 0.0 ! Excluding Population 2 -TBK
!       I2px_I = 0.0
       ! units are fine: (Uth2/ustar)*termxp is unitless as it should be

       JxpUx_I  = Ux_I(1)*Rate_I
       JxpUy_I  = Uy_I(1)*Rate_I
       JxpUz_I  = Uz_I(1)*Rate_I

       JpxUx_I  = Ux_I*Rate_I
       JpxUy_I  = Uy_I*Rate_I
       JpxUz_I  = Uz_I*Rate_I

       ! Zieger for SW or Ion
       QmpxUx_I(1) = 0.0
       QmpxUy_I(1) = 0.0
       QmpxUz_I(1) = 0.0

       QmpxUx_I(Neu_) = (Ux_I(Neu_) - Ux_I(1))*Rate_I(Neu_)
       QmpxUx_I(Ne4_) = (Ux_I(Ne4_) - Ux_I(1))*Rate_I(Ne4_)

       QmpxUy_I(Neu_) = (Uy_I(Neu_) - Uy_I(1))*Rate_I(Neu_)
       QmpxUy_I(Ne4_) = (Uy_I(Ne4_) - Uy_I(1))*Rate_I(Ne4_)

       QmpxUz_I(Neu_) = (Uz_I(Neu_) - Uz_I(1))*Rate_I(Neu_)
       QmpxUz_I(Ne4_) = (Uz_I(Ne4_) - Uz_I(1))*Rate_I(Ne4_)

       ! For SW or Ion
       Kxp_I = 0.5*U2_I(1)*Rate_I  + I2xp_I
       Kpx_I = 0.5*U2_I*Rate_I  + I2px_I
       Qepx_I = Kpx_I - Kxp_I

       ! Calculate the source terms for this cell
       call calc_source_cell

    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)

  contains
    !==========================================================================
    subroutine calc_source_cell

      use ModPhysics,   ONLY: GammaMinus1_I

      ! Calculate source terms for one cell. The pressures source is
      ! S(p) = (gamma-1)[S(e) - u.S(rhou) + 0.5 u**2 S(rho)]

      real    :: Source_V(nVar + nFluid)
      integer :: iVar, iFluid

      logical :: DoTest

      character(len=*), parameter:: NameSub = 'calc_source_cell'
      !------------------------------------------------------------------------
      if(i == iTest .and. j == jTest .and. k == kTest) &
           call test_start(NameSub, DoTest, iBlock)

      Source_V = 0.0
      do iFluid = Neu_, Ne4_
         if(.not.UseSource_I(iFluid)) CYCLE
         call select_fluid(iFluid)
         if (iFluid == iFluidProduced_C(i,j,k)) then
            Source_V(iRho)    = sum(I0xp_I(Neu_:Ne4_))  - I0xp_I(iFluid)
            Source_V(iRhoUx)  = sum(JxpUx_I(Neu_:Ne4_)) - JpxUx_I(iFluid)
            Source_V(iRhoUy)  = sum(JxpUy_I(Neu_:Ne4_)) - JpxUy_I(iFluid)
            Source_V(iRhoUz)  = sum(JxpUz_I(Neu_:Ne4_)) - JpxUz_I(iFluid)
            Source_V(iEnergy) = sum(Kxp_I(Neu_:Ne4_))   - Kpx_I(iFluid)
         else
            Source_V(iRho)    = - I0px_I(iFluid)
            Source_V(iRhoUx)  = - JpxUx_I(iFluid)
            Source_V(iRhoUy)  = - JpxUy_I(iFluid)
            Source_V(iRhoUz)  = - JpxUz_I(iFluid)
            Source_V(iEnergy) = - Kpx_I(iFluid)
         end if

         Source_V(iP) = GammaMinus1_I(iFluid)* ( Source_V(iEnergy) &
              - Ux_I(iFluid)*Source_V(iRhoUx) &
              - Uy_I(iFluid)*Source_V(iRhoUy) &
              - Uz_I(iFluid)*Source_V(iRhoUz) &
              + 0.5*U2_I(iFluid)*Source_V(iRho) )
      end do

      if(UseSource_I(Ion_))then
         ! Only consider fluids with source selected.
         do iFluid = Neu_, Ne4_
            if(.not.UseSource_I(iFluid)) CYCLE
            Source_V(RhoUx_) = Source_V(RhoUx_) + QmpxUx_I(iFluid)
            Source_V(RhoUy_) = Source_V(RhoUy_) + QmpxUy_I(iFluid)
            Source_V(RhoUz_) = Source_V(RhoUz_) + QmpxUz_I(iFluid)
            Source_V(Energy_)= Source_V(Energy_) + Qepx_I(iFluid)
         enddo

         Source_V(p_) = GammaMinus1* ( Source_V(Energy_) &
              - Ux_I(Ion_)*Source_V(RhoUx_) &
              - Uy_I(Ion_)*Source_V(RhoUy_) &
              - Uz_I(Ion_)*Source_V(RhoUz_) &
              + 0.5*U2_I(Ion_)*Source_V(Rho_) )
      end if

      Source_VC(:,i,j,k) = Source_VC(:,i,j,k) + Source_V

      if(DoTest .and. i==iTest .and. j==jTest .and. k==kTest)then

         Source_V = Source_VC(:,i,j,k)

         write(*,*) NameSub, ' iFluidProduced=', iFluidProduced_C(i,j,k)
         do iVar = 1, nVar + nFLuid
            write(*,*) ' Source(',NameVar_V(iVar),')=',Source_V(iVar)
         end do
         write(*,*) ' Temp_I    ', Temp_I
         write(*,*) ' Rate_I    ', Rate_I
         write(*,*) ' Sigma_I   ', Sigma_I
         write(*,*) ' UStar_I   ', UStar_I
         write(*,*) ' UStarM_I  ', UStarM_I
         write(*,*) ' Ux_I      ', Ux_I
         write(*,*) ' U2_I      ', U2_I
         write(*,*) ' UTh_I     ', sqrt(UThS_I)
         write(*,*) ' URelDim_I ', sqrt(URelSdim_I)
         write(*,*) ' uDim_I    ', sqrt(U2_I)*No2Io_V(UnitU_)
         write(*,*) ' I2xp_I    ', I2xp_I
         write(*,*) ' I2px_I    ', I2px_I
         write(*,*) ' JxpUx_I   ', JxpUx_I
         write(*,*) ' JpxUx_I   ', JpxUx_I
         write(*,*) ' QmpxUx_I  ', QmpxUx_I
         write(*,*) ' Kxp_I     ', Kxp_I
         write(*,*) ' Kpx_I     ', Kpx_I
         write(*,*) ' Qepx_I    ', Qepx_I
      end if

      if(i == iTest .and. j == jTest .and. k == kTest) &
           call test_stop(NameSub, DoTest, iBlock)

    end subroutine calc_source_cell
    !==========================================================================
  end subroutine user_calc_sources_expl
  !============================================================================
  subroutine user_calc_sources_impl(iBlock)

    integer, intent(in) :: iBlock

    real :: State_V(nVar)
    real :: Ux, Uy, Uz, U2
    integer :: i,j,k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_calc_sources_impl'
    !--------------------------------------------------------------------------
    ! updating sources from AMPS in PT-OH coupling
    ! might be unnecessary relic from 3D version

    call test_start(NameSub, DoTest)

    if(DoTest .and. UseNeutralFluid)write(*,*)"  No action required."

    if(.not.UseNeutralFluid)then

       if(DoTest)write(*,*)"  No neutrals."

       if(.not.allocated(ExtraSource_ICB)) &
            ! allocate(ExtraSource_ICB(5,nI,nJ,nK,nBlock)) ! 5 is nFluid
            call CON_stop(NameSub//': ExtraSource_ICB is not allocated')

       if(DoTest) write(*,*)"..."

       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          ! skipping the cells inside rBody
          if(.not.Used_GB(i,j,k,iBlock))CYCLE

          ! Extract conservative variables
          State_V = State_VGB(:,i,j,k,iBlock)

          Ux  = State_V(RhoUx_)/State_V(Rho_)
          Uy  = State_V(RhoUy_)/State_V(Rho_)
          Uz  = State_V(RhoUz_)/State_V(Rho_)

          U2  = Ux**2 + Uy**2 + Uz**2

          ! updating the source terms
          Source_VC(Rho_,i,j,k) = Source_VC(Rho_,i,j,k) &
               + ExtraSource_ICB(1,i,j,k,iBlock)
          Source_VC(RhoUx_,i,j,k) = Source_VC(RhoUx_,i,j,k) &
               + ExtraSource_ICB(2,i,j,k,iBlock)
          Source_VC(RhoUy_,i,j,k) = Source_VC(RhoUy_,i,j,k) &
               + ExtraSource_ICB(3,i,j,k,iBlock)
          Source_VC(RhoUz_,i,j,k) = Source_VC(RhoUz_,i,j,k) &
               + ExtraSource_ICB(4,i,j,k,iBlock)
          Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) &
               + ExtraSource_ICB(5,i,j,k,iBlock)
          Source_VC(p_,i,j,k) = Source_VC(p_,i,j,k) &
               + GammaMinus1*( Source_VC(Energy_,i,j,k) &
               - Ux*Source_VC(RhoUx_,i,j,k) &
               - Uy*Source_VC(RhoUy_,i,j,k) &
               - Uz*Source_VC(RhoUz_,i,j,k) &
               + 0.5*U2*Source_VC(Rho_,i,j,k) )

       end do; end do; end do
       RETURN
    end if

    call test_stop(NameSub, DoTest)

  end subroutine user_calc_sources_impl
  !============================================================================
  subroutine set_omega_parker_tilt

    ! Calculate angular velocity in normalized units
    ! Note: the rotation period is 25.38 days in ModConst.f90:
    ! OmegaSun = cTwoPi/(RotationPeriodSun*Si2No_V(UnitT_))

    logical :: DoTest

    character(len=*), parameter:: NameSub = 'set_omega_parker_tilt'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    OmegaSun   = cTwoPi/(26.0*24.0*3600.00*Si2No_V(UnitT_))
    ParkerTilt = OmegaSun*rBody/SwhUx

    call test_stop(NameSub, DoTest)

  end subroutine set_omega_parker_tilt
  !============================================================================
  subroutine select_region(iBlock)

    ! set the global variable iFluidProduced_C
    ! to select which neutral fluid is produced in each cell of the block

    integer, intent(in):: iBlock

    integer :: i, j, k
    real    :: InvRho, U2, p, Mach2, TempDim, U2Dim, B2, MachAlfven2
    real    :: MachMagneto2

    ! This subroutine is unnecessary when not using the 4 neutral fluids
    logical :: DoTest
    character(len=*), parameter:: NameSub = 'select_region'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(.not.UseNeutralFluid) call CON_stop(NameSub// &
         ': no neutral fluids present')

    ! Produce fluid3 at the inner boundary
    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       if(r_GB(i,j,k,iBlock) < rPop3Limit) then
          iFluidProduced_C(i,j,k) = Ne3_
          iFluidProduced_C(i,j,k) = 0
          CYCLE
       end if

       InvRho = 1.0/State_VGB(Rho_,i,j,k,iBlock)
       p      = State_VGB(p_,i,j,k,iBlock)
       U2     = sum(State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)**2)*InvRho**2
       U2Dim  = U2*No2Io_V(UnitU_)**2

       B2 = sum(State_VGB(Bx_:Bz_,i,j,k,iBlock)**2)
       ! Square of Alfven Mach Number
       MachAlfven2 = U2/(B2*InvRho + 1.E-30)
       MachMagneto2 = U2/(1.E-10 + Gamma*p*InvRho + B2*InvRho)

       ! Square of Mach number
       Mach2 = U2/(Gamma*p*InvRho)

       ! Temperature in Kelvin
       TempDim = InvRho*p*No2Si_V(UnitTemperature_)

       ! Apply full source except near the boundaries between regions
       if (MachPop4Limit**2 < Mach2 .and. uPop1LimitDim**2 > U2Dim) then
          ! Outside the bow shock
          iFluidProduced_C(i,j,k) = Ne4_
       elseif( TempPop1LimitDim > TempDim .and. uPop1LimitDim**2 > U2Dim)then
          ! Outside the heliopause
          iFluidProduced_C(i,j,k) = Neu_
       elseif( MachPop2Limit**2 > Mach2 )then
          ! Heliosheath
          iFluidProduced_C(i,j,k) = Ne2_
       elseif( Mach2 > MachPop3Limit**2 )then
          ! Inside termination shock
          iFluidProduced_C(i,j,k) = Ne3_
       else
          ! No neutrals are produced in this region (but they are destroyed)
          iFluidProduced_C(i,j,k) = 0
       end if
       iFluidProduced_C(i,j,k) = 0

    end do; end do; end do
    call test_stop(NameSub, DoTest)

  end subroutine select_region
  !============================================================================
  subroutine user_update_states(iBlock)

    use ModGeometry, ONLY: TypeGeometry
    integer,intent(in):: iBlock
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_update_states'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(DoTest)write(*,*) NameSub,': coordinate system ', &
         TypeGeometry

    if(TypeGeometry == "spherical_lnr" .or. &
       TypeGeometry == "spherical")then
       call user_update_states_sph(iBlock)
    else if(TypeGeometry == "cylindrical_lnr" .or. &
       TypeGeometry == "cylindrical")then
       call user_update_states_cyl(iBlock)
    else
       write(*,*) NameSub,': user_update_states not implemented for ', &
            TypeGeometry
       call stop_mpi(NameSub//' invalid grid geometry '//TypeGeometry)
    endif

    call test_stop(NameSub, DoTest)

  end subroutine user_update_states
  !============================================================================
  subroutine user_update_states_sph(iBlock)

    use ModUpdateState, ONLY: update_state_normal
    use ModAdvance,     ONLY: StateOld_VGB, State_VGB, &
         Flux_VXI, Flux_VYI, Flux_VZI, Source_VC, DtMax_CB
    use ModMain,        ONLY: Cfl, nStage, iStage
    use ModEnergy,      ONLY: energy_to_pressure, pressure_to_energy
    use BATL_lib,       ONLY: CellVolume_GB, CoordMin_DB, CellSize_DB, &
         IsLogRadius
    use ModMultiFluid,  ONLY: DoConserveNeutrals

    integer,intent(in):: iBlock
    integer:: i, j, k, iVar, iVarFlux, iFluid
    real:: DtFactor, DtLocal, InvVolume
    real:: rCell_I(nI), rFace_I(nI+1), FactorL, FactorR
    real:: UpdateFlux
    logical:: DoTest, DoTestCell

    character(len=*), parameter:: NameSub = 'user_update_states_sph'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(DoTest)then
       write(*,*)NameSub,': State_VGB(iVar) initial value: ', &
         State_VGB(iVarTest,iTest,jTest,kTest,iBlockTest)
    end if

    DtFactor = (Cfl*iStage)/nStage

    ! Precalculate radial distances of cell centers and faces.
    do i = 1, nI
       rCell_I(i) = CoordMin_DB(1,iBlock) + (i-0.5)*CellSize_DB(1,iBlock)
    end do
    do i = 1, nI+1
       rFace_I(i) = CoordMin_DB(1,iBlock) + (i-1)*CellSize_DB(1,iBlock)
    end do
    if(IsLogRadius)then
       rCell_I = exp(rCell_I)
       rFace_I = exp(rFace_I)
    end if

    if(DoTest)write(*,*)NameSub,': Source_VC(iVar) before normal update: ',&
         Source_VC(iVarTest,iTest,jTest,kTest)

    ! Perform normal update.
    call update_state_normal(iBlock)

    if(DoTest)write(*,*)NameSub,': Source_VC(iVar) after normal update: ',&
         Source_VC(iVarTest,iTest,jTest,kTest)

    if(DoTest)write(*,*)NameSub,': State_VGB(iVar) after normal update: ',&
         State_VGB(iVarTest,iTest,jTest,kTest,iBlockTest)

    if(DoConserveNeutrals)then
       call pressure_to_energy(iBlock, State_VGB)
       call pressure_to_energy(iBlock, StateOld_VGB)
       if(DoTest)write(*,*)NameSub, &
            ': State_VGB(iVar) after pressure_to_energy:', &
            State_VGB(iVarTest,iTest,jTest,kTest,iBlockTest)
    endif

    ! Account for geometric differences between spherical (ion)
    ! grid and cylindrical (neutrals) grid.
    do k = 1,nK; do j = 1,nJ; do i = 1,nI

       DtLocal = DtFactor*DtMax_CB(i,j,k,iBlock)
       InvVolume = 1/CellVolume_GB(i,j,k,iBlock)

       ! Ratios of face area to cell volumes at r1, r2 of cell
       FactorL = rCell_I(i)/rFace_I(i)
       FactorR = rCell_I(i)/rFace_I(i+1)

       ! Correct fluxes for neutral populations.
       do iVar = NeuRho_, Ne4P_
          if(iVar >= Ne2Rho_ .and. iVar <= Ne3P_)then
             CYCLE ! ignore Ne2 and Ne3; not used for MSWIM2D
          else
             ! Change from pressure var to energy var, if necessary.
             iVarFlux = iVar
             if(DoConserveNeutrals .and. iVar == NeuP_) iVarFlux = NeuEnergy_
             if(DoConserveNeutrals .and. iVar == Ne4P_) iVarFlux = Ne4Energy_

             if(DoTest .and. iVar == iVarTest .and. i == iTest .and. &
                  j == jTest .and. k == kTest) &
                  write(*,*)NameSub, &
                  ': State_VGB(iVar) before flux subtraction: ', &
                  State_VGB(iVarTest,iTest,jTest,kTest,iBlockTest)

             ! Remove flux added by update_state_normal
             State_VGB(iVar,i,j,k,iBlock) = &
                  State_VGB(iVar,i,j,k,iBlock) - &
                  DtLocal * InvVolume * &
                  ( Flux_VXI(iVarFlux,i,j,k,1) - Flux_VXI(iVarFlux,i+1,j,k,1) &
                  + Flux_VYI(iVarFlux,i,j,k,1) - Flux_VYI(iVarFlux,i,j+1,k,1) &
                  + Flux_VZI(iVarFlux,i,j,k,1) - Flux_VZI(iVarFlux,i,j,k+1,1))

             if(DoTest .and. iVar == iVarTest .and. i == iTest .and. &
                  j == jTest .and. k == kTest) &
                  write(*,*)NameSub, &
                  ': State_VGB(iVar) after flux subtraction: ', &
                  State_VGB(iVarTest,iTest,jTest,kTest,iBlockTest)

             ! No Z flux for Pop I and Pop IV (horizontal flow)
             ! The X flux is modified to compensate for the latitude width
             UpdateFlux = DtLocal * InvVolume * &
                  ( FactorL*Flux_VXI(iVarFlux,i,j,k,1) &
                  - FactorR*Flux_VXI(iVarFlux,i+1,j,k,1) &
                  + Flux_VYI(iVarFlux,i,j,k,1) - Flux_VYI(iVarFlux,i,j+1,k,1) )

             if(DoTest .and. iVar == iVarTest .and. i == iTest .and. &
                  j == jTest .and. k == kTest)write(*,*)NameSub, &
                  ': Flux Update: ', UpdateFlux

             ! Add updated flux back to state.
             State_VGB(iVar,i,j,k,iBlock) = State_VGB(iVar,i,j,k,iBlock) + &
                  UpdateFlux

             if(DoTest .and. iVar == iVarTest .and. i == iTest .and. &
                  j == jTest .and. k == kTest) &
                  write(*,*)NameSub, &
                  ': State_VGB(iVar) after flux addition: ', &
                  State_VGB(iVarTest,iTest,jTest,kTest,iBlockTest)

          end if
       end do ! iVar(Neu_:Ne4P_)

       ! Return Ne2 and Ne3 to previous states (freeze neutrals).
       State_VGB(Ne2Rho_:Ne3P_,i,j,k,iBlock) = &
            StateOld_VGB(Ne2Rho_:Ne3P_,i,j,k,iBlock)

    end do; end do; end do ! i,j,k

    if(DoConserveNeutrals)then
       call energy_to_pressure(iBlock, State_VGB)
       call energy_to_pressure(iBlock, StateOld_VGB)
    endif

    if(DoTest)write(*,*)NameSub,': State_VGB(iVar) after correction: ', &
         State_VGB(iVarTest,iTest,jTest,kTest,iBlockTest)

    call test_stop(NameSub, DoTest)

  end subroutine user_update_states_sph
  !============================================================================
  subroutine user_update_states_cyl(iBlock)

    use ModUpdateState, ONLY: update_state_normal
    use ModAdvance,     ONLY: StateOld_VGB, State_VGB, &
         Flux_VXI, Flux_VYI, Flux_VZI, Source_VC, DtMax_CB
    use ModMain,        ONLY: Cfl, nStage, iStage
    use ModEnergy,      ONLY: energy_to_pressure, pressure_to_energy
    use BATL_lib,       ONLY: CellVolume_GB, CoordMin_DB, CellSize_DB, &
         IsLogRadius
    use ModMultiFluid,  ONLY: DoConserveNeutrals

    integer,intent(in):: iBlock
    integer:: i, j, k, iVar, iVarFlux, iFluid
    real:: RhoInv, r2Inv
    real:: Xy_D(2)
    real:: DtFactor, DtLocal, InvVolume
    real:: rCell_I(nI), rFace_I(nI+1), FactorL, FactorR
    real:: UpdateFlux
    logical:: DoTest, DoTestCell

    character(len=*), parameter:: NameSub = 'user_update_states_cyl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(DoTest)then
       write(*,*)NameSub,': State_VGB(iVar) initial value: ', &
            State_VGB(iVarTest,iTest,jTest,kTest,iBlockTest)
    end if

    DtFactor = (Cfl*iStage)/nStage

    if(DoTest)write(*,*)NameSub, &
         ': Source_VC(iVar) before geometric correction: ', &
         Source_VC(iVarTest,iTest,jTest,kTest)

    ! Account for geometric differences between spherical (ion)
    ! grid and cylindrical (neutrals) grid.
    do k = 1,nK; do j = 1,nJ; do i = 1,nI

       ! The following geometrical source terms are added for the MHD equations
       ! Source[mr]  =(p+B^2/2-Bphi**2+mphi**2/rho)/radius
       ! Source[mphi]=(-mphi*mr/rho+Bphi*Br)/radius(if no angular momentum fix)
       ! Source[Bphi]=((Bphi*mr-Br*mphi)/rho)/radius
       ! These terms are converted to [x,y,z] for simplicity.

       RhoInv = 1/State_VGB(Rho_,i,j,k,iBlock)
       r2Inv = 1/r_GB(i,j,k,iBlock)**2
       Xy_D = Xyz_DGB(1:2,i,j,k,iBlock)

       ! Source[mr] = (p+mphi**2/rho)/radius
       ! Source[mx] = x/r * (p+mphi**2/rho)/r
       ! Source[my] = y/r * (p+mphi**2/rho)/r
       Source_VC(RhoUx_:RhoUy_,i,j,k) = Source_VC(RhoUx_:RhoUy_,i,j,k) &
            + (State_VGB(P_,i,j,k,iBlock) &
            + RhoInv*State_VGB(RhoUz_,i,j,k,iBlock)**2)*Xy_D*r2Inv

       ! Source[mphi] = (-mphi*mr/rho)/radius
       ! Source[mz] = (-mphi*(x*mx+y*my)/radius/rho)/radius
       Source_VC(RhoUz_,i,j,k) = Source_VC(RhoUz_,i,j,k) &
            - State_VGB(RhoUz_,i,j,k,iBlock) &
            * sum(Xy_D * State_VGB(RhoUx_:RhoUy_,i,j,k,iBlock))*RhoInv*r2Inv

       ! Source[mr] = (B^2/2-Bphi**2)/radius
       ! Source[mx] = x/r * (B^2/2-Bphi**2)/radius
       ! Source[my] = y/r * (B^2/2-Bphi**2)/radius
       Source_VC(RhoUx_:RhoUy_,i,j,k) = Source_VC(RhoUx_:RhoUy_,i,j,k) + &
            (0.5 * sum(State_VGB(Bx_:Bz_,i,j,k,iBlock)**2) &
            - State_VGB(Bz_,i,j,k,iBlock)**2) * Xy_D * r2Inv

       ! Source[mphi]=Bphi*Br/radius
       ! Source[mz]=Bz*sqrt(Bx**2+By**2)/radius
       Source_VC(RhoUz_,i,j,k) = Source_VC(RhoUz_,i,j,k) &
            + State_VGB(Bz_,i,j,k,iBlock) &
            * sum(Xy_D*State_VGB(Bx_:By_,i,j,k,iBlock))*r2Inv

       ! Source[Bphi]=((Bphi*mr-Br*mphi)/rho)/radius
       ! Source[Bz]=((Bz*(x*mx+y*my)/r - (x*Bx+y*By)*mz/r)/rho)/radius
       Source_VC(Bz_,i,j,k) = Source_VC(Bz_,i,j,k) &
            + (State_VGB(Bz_,i,j,k,iBlock) &
            * sum(Xy_D*State_VGB(RhoUx_:RhoUy_,i,j,k,iBlock)) &
            - sum(Xy_D*State_VGB(Bx_:By_,i,j,k,iBlock)) &
            * State_VGB(RhoUz_,i,j,k,iBlock)) &
            * RhoInv * r2Inv

    end do; end do; end do ! i,j,k

    if(DoTest)write(*,*)NameSub,': Source_VC(iVar) after flux correction: ',&
         Source_VC(iVarTest,iTest,jTest,kTest)

    ! Perform normal update.
    call update_state_normal(iBlock)

    if(DoTest)then
       write(*,*)NameSub,': Source_VC(iVar) after normal update: ',&
         Source_VC(iVarTest,iTest,jTest,kTest)
       write(*,*)NameSub,': State_VGB(iVar) after normal update: ', &
            State_VGB(iVarTest,iTest,jTest,kTest,iBlockTest)
    end if

    ! Precalculate radial distances of cell centers and faces.
    do i = 1, nI
       rCell_I(i) = CoordMin_DB(1,iBlock) + (i-0.5)*CellSize_DB(1,iBlock)
    end do
    do i = 1, nI+1
       rFace_I(i) = CoordMin_DB(1,iBlock) + (i-1)*CellSize_DB(1,iBlock)
    end do
    if(IsLogRadius)then
       rCell_I = exp(rCell_I)
       rFace_I = exp(rFace_I)
    end if

    ! Account for geometric differences between spherical (ion)
    ! grid and cylindrical (neutrals) grid.
    do k = 1,nK; do j = 1,nJ; do i = 1,nI
       DtLocal = DtFactor*DtMax_CB(i,j,k,iBlock)
       InvVolume = 1/CellVolume_GB(i,j,k,iBlock)

       ! Ratios of face area to cell volumes at r1, r2 of cell
       FactorL = rFace_I(i)/rCell_I(i)
       FactorR = rFace_I(i+1)/rCell_I(i)

       ! Correct fluxes for ion population.
       do iVar = Rho_, P_
          ! Change from pressure var to energy var, if necessary.
          iVarFlux = iVar

          ! Remove flux added by update_state_normal
          State_VGB(iVar,i,j,k,iBlock) = &
               State_VGB(iVar,i,j,k,iBlock) - &
               DtLocal * InvVolume * &
               ( Flux_VXI(iVarFlux,i,j,k,1) - Flux_VXI(iVarFlux,i+1,j,k,1) &
               + Flux_VYI(iVarFlux,i,j,k,1) - Flux_VYI(iVarFlux,i,j+1,k,1) &
               + Flux_VZI(iVarFlux,i,j,k,1) - Flux_VZI(iVarFlux,i,j,k+1,1))

          ! Z flux is accounted for by the source terms above.
          ! The X flux is modified to compensate for the latitude width
          ! The Y flux actually stays the same.
          UpdateFlux = DtLocal * InvVolume * &
               ( FactorL*Flux_VXI(iVarFlux,i,j,k,1) &
               - FactorR*Flux_VXI(iVarFlux,i+1,j,k,1) &
               + Flux_VYI(iVarFlux,i,j,k,1) - Flux_VYI(iVarFlux,i,j+1,k,1) )

          ! Add updated flux back to state.
          State_VGB(iVar,i,j,k,iBlock) = State_VGB(iVar,i,j,k,iBlock) + &
               UpdateFlux
       end do
    end do; end do; end do

    ! Set Ne2 and Ne3 back to previous state.
    State_VGB(Ne2Rho_:Ne3P_,:,:,:,iBlock) = &
         StateOld_VGB(Ne2Rho_:Ne3P_,:,:,:,iBlock)

    if(DoTest)write(*,*)NameSub,': State_VGB(iVar) after face correction', &
         State_VGB(iVarTest,iTest,jTest,kTest,iBlockTest)

    call test_stop(NameSub, DoTest)

  end subroutine user_update_states_cyl
  !============================================================================
  subroutine user_init_session

    logical :: DoTest
    character(len=*), parameter:: NameSub = 'user_init_session'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    VliswRho = VliswRhoDim*Io2No_V(UnitRho_)
    VliswP   = 2.*VliswTDim*Io2No_V(UnitTemperature_)*VliswRho

    VliswUx  = VliswUxDim*Io2No_V(UnitU_)
    VliswUy  = VliswUyDim*Io2No_V(UnitU_)
    VliswUz  = VliswUzDim*Io2No_V(UnitU_)
    VliswBx  = VliswBxDim*Io2No_V(UnitB_)
    VliswBy  = VliswByDim*Io2No_V(UnitB_)
    VliswBz  = VliswBzDim*Io2No_V(UnitB_)

    SwhRho = SwhRhoDim*Io2No_V(UnitRho_)
    ! Pressure of plasma = 2*T_ion*rho_ion
    SwhP   = 2.*SwhTDim*Io2No_V(UnitTemperature_)*SwhRho

    SwhUx  = SwhUxDim*Io2No_V(UnitU_)
    SwhUy  = SwhUyDim*Io2No_V(UnitU_)
    SwhUz  = SwhUzDim*Io2No_V(UnitU_)
    SwhBx  = SwhBxDim*Io2No_V(UnitB_)
    SwhBy  = SwhByDim*Io2No_V(UnitB_)
    SwhBz  = SwhBzDim*Io2No_V(UnitB_)

    ! The units of rhoDim are n/cc and unitUSER_Rho Gamma/cc

    if(UseNeutralFluid)then

       if(DoTest)write(*,*)" ",NameSub,": modifying neutral fluid..."

       RhoNeutralsISW = RhoNeuWindDim*Io2No_V(UnitRho_)

       if(DoTest)write(*,*)" ",NameSub,": RhoNeutralsISW = ",RhoNeutralsISW

       PNeutralsISW  = TempNeuWindDim*Io2No_V(UnitTemperature_)*RhoNeutralsISW
       UxNeutralsISW = UxNeuWindDim*Io2No_V(UnitU_)
       UyNeutralsISW = UyNeuWindDim*Io2No_V(UnitU_)
       UzNeutralsISW = UzNeuWindDim*Io2No_V(UnitU_)
       MassNeutral   = MassNeutralDim*cProtonMass

       RhoNe4 = RhoNe4Dim*Io2No_V(UnitRho_)
       UxNe4  = UxNe4Dim*Io2No_V(UnitU_)
       UyNe4  = UyNe4Dim*Io2No_V(UnitU_)
       UzNe4  = UzNe4Dim*Io2No_V(UnitU_)
       PNe4   = pNe4Dim*Io2No_V(UnitP_)

       if(DoTest)write(*,*)" ",NameSub,": RhoNe4 = ",RhoNe4

    endif

    call test_stop(NameSub, DoTest)

  end subroutine user_init_session
  !============================================================================
end module ModUser
!==============================================================================
