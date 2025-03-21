!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModSetInitialCondition

  use BATL_lib, ONLY: &
       test_start, test_stop, iBlockTest, iTest, jTest, kTest

  use ModVarIndexes, ONLY: nVar
  use ModMain, ONLY: NamePrimitive_V
  use ModPhysics, ONLY: &
       UseShocktube, ShockLeftDim_V, ShockRightDim_V, ShockLeft_V, &
       ShockRight_V, ShockPosition, ShockSlope, nVectorVar, iVectorVar_I, &
       iUnitPrim_V, Io2No_V, UseRadialState, AmplRDim_V, ExponentR_V

  use ModBatsrusUtility, ONLY: stop_mpi, get_ivar
  use ModNumConst, ONLY: cTwoPi, cDegToRad

  implicit none

  private ! except

  public:: read_initial_cond_param ! read parameters for initial condition
  public:: set_initial_condition   ! set initial condition for one block
  public:: add_rotational_velocity ! transform between rotating/inertial frames

  ! Local variables

  ! Initialize B1 instead of full B
  logical:: UseB1 = .false.

  ! Entropy constant for isentropic initial condition. Only used if positive.
  real :: EntropyConstant = -1.0

  ! Use 2D initial state algorithm
  logical:: UseInitialStateDefinition = .false.

  ! Wave/Tophat/Gaussian
  logical:: UseWave = .false.
  logical:: DoRotateWave = .true.

  ! Wave parameters
  real:: Width, Amplitude, Phase, LambdaX, LambdaY, LambdaZ
  real, dimension(nVar):: Width_V=0.0, Ampl_V=0.0, Phase_V=0.0, &
       x_V=0.0, y_V=0.0, z_V=0.0, KxWave_V=0.0, KyWave_V=0.0, KzWave_V=0.0
  integer:: iPower_V(nVar)=1

contains
  !============================================================================
  subroutine read_initial_cond_param(NameCommand)

    use ModReadParam, ONLY: read_var
    use ModUtilities, ONLY: join_string
    use ModInitialState, ONLY: init_initial_state, read_initial_state_param

    character(len=*), intent(in):: NameCommand

    character(len=500):: StringVar
    character(len=20):: NameVar

    integer:: iVar

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_initial_cond_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    select case(NameCommand)
    case("#STATEDEFINITION")
       UseInitialStateDefinition = .true.
       call join_string(nVar, NamePrimitive_V(1:nVar), StringVar)
       call init_initial_state(StringVar)
       call read_initial_state_param(NameCommand)

    case("#STATEINTERFACE")
       call read_initial_state_param(NameCommand)

    case("#UNIFORMSTATE", "#UNIFORMSTATE1")
       UseShockTube = .true.
       UseB1 = NameCommand == '#UNIFORMSTATE1'
       do iVar = 1, nVar
          call read_var(NamePrimitive_V(iVar), ShockLeftDim_V(iVar))
       end do
       ShockRightDim_V = ShockLeftDim_V

    case("#RADIALSTATE")
       UseRadialState = .true.
       do iVar = 1, nVar
          call read_var('Amplitude', AmplRDim_V(iVar))
          if(AmplRDim_V(iVar) /= 0.0) &
               call read_var('ExponentR', ExponentR_V(iVar))
       end do

    case("#SHOCKTUBE")
       UseShockTube = .true.
       do iVar = 1, nVar
          call read_var(NamePrimitive_V(iVar)//' left', ShockLeftDim_V(iVar))
       end do
       do iVar = 1, nVar
          call read_var(NamePrimitive_V(iVar)//' right', &
               ShockRightDim_V(iVar))
       end do

    case("#SHOCKPOSITION")
       call read_var('ShockPosition', ShockPosition)
       call read_var('ShockSlope', ShockSlope)

    case("#ENTROPYCONSTANT")
       call read_var('EntropyConstant', EntropyConstant)

    case("#WAVE", "#WAVE2", "#WAVE4", "#WAVE6")
       UseWave = .true.
       call read_var('NameVar', NameVar)
       call get_ivar(NameVar, iVar)
       call read_var('Width', Width)
       call read_var('Amplitude', Amplitude)
       call read_var('LambdaX', LambdaX)
       call read_var('LambdaY', LambdaY)
       call read_var('LambdaZ', LambdaZ)
       call read_var('Phase', Phase)
       Width_V(iVar) = Width
       Ampl_V(iVar)  = Amplitude
       Phase_V(iVar) = Phase*cDegToRad

       if(NameCommand == '#WAVE6')then
          iPower_V(iVar) = 6
       elseif(NameCommand == '#WAVE4')then
          iPower_V(iVar) = 4
       elseif(NameCommand == '#WAVE2')then
          iPower_V(iVar) = 2
       else
          iPower_V(iVar) = 1
       end if

       ! if wavelength is smaller than 0, then the wave number is set to 0
       KxWave_V(iVar) = max(0.0, cTwoPi/LambdaX)
       KyWave_V(iVar) = max(0.0, cTwoPi/LambdaY)
       KzWave_V(iVar) = max(0.0, cTwoPi/LambdaZ)

    case("#BUMP")
       UseWave = .true.
       call read_var('NameVar',   NameVar)
       call get_ivar(NameVar, iVar)
       call read_var('Amplitude', Ampl_V(iVar))
       call read_var('WidthX',    LambdaX)
       call read_var('WidthY',    LambdaY)
       call read_var('WidthZ',    LambdaZ)
       call read_var('CenterX',   x_V(iVar))
       call read_var('CenterY',   y_V(iVar))
       call read_var('CenterZ',   z_V(iVar))
       call read_var('nPower',    iPower_V(iVar))
       iPower_V(iVar) = -iPower_V(iVar)

       ! Negative width sets 0 for 1/width (unlimited in that direction)
       KxWave_V(iVar) = max(0.0, 1/LambdaX)
       KyWave_V(iVar) = max(0.0, 1/LambdaY)
       KzWave_V(iVar) = max(0.0, 1/LambdaZ)

    case default
       call stop_mpi(NameSub//': unknown command='//NameCommand)
    end select

    call test_stop(NameSub, DoTest)
  end subroutine read_initial_cond_param
  !============================================================================
  subroutine set_initial_condition(iBlock)

    use ModMain
    use ModAdvance
    use ModB0, ONLY: B0_DGB, set_b0_cell, subtract_b0
    use ModGeometry, ONLY: Used_GB
    use ModIO, ONLY: IsRestart
    use ModPhysics, ONLY: FaceState_VI, CellState_VI, Gamma_I, &
         set_radial_state
    use ModUserInterface ! user_set_ics
    use ModSaMhd,          ONLY: UseSaMhd, init_samhd
    use ModConstrainDivB, ONLY: constrain_ics
    use ModMultiFluid
    use ModRestartFile, ONLY: UseRestartWithFullB
    use ModBoundaryGeometry, ONLY: iBoundary_GB
    use ModInitialState, ONLY: get_initial_state
    use ModIonElectron,   ONLY: &
         correct_electronfluid_efield , DoCorrectElectronFluid, DoCorrectEfield
    use BATL_lib, ONLY: Xyz_DGB, IsPeriodic_D
    use ModUtilities, ONLY: i_gang

    integer, intent(in) :: iBlock

    real   :: SinSlope, CosSlope, Rot_II(2,2), x, y
    integer:: i, j, k, iVar, iBoundary, iFluid, iGang

    logical:: DoTestCell
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_initial_condition'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    iGang = i_gang(iBlock)

    DtMax_CB(:,:,:,iBlock) = 0.0

    Flux_VXI(:,:,:,:,iGang) = 0.0
    Flux_VYI(:,:,:,:,iGang) = 0.0
    Flux_VZI(:,:,:,:,iGang) = 0.0

    if(Unused_B(iBlock))then
       do iVar = 1, nVar
          State_VGB(iVar,:,:,:,iBlock) = DefaultState_V(iVar)
       end do
    else
       ! If used, initialize solution variables and parameters.
       if(UseB0) call set_b0_cell(iBlock)

       ! Subtract B0 from Full B0+B1 from restart to obtain B1
       if(UseB0 .and. IsRestart .and. UseRestartWithFullB) &
            call subtract_b0(iBlock)

       if(.not.IsRestart)then

          if(UseShockTube)then
             ! Calculate sin and cos from the tangent = ShockSlope
             CosSlope = 1/sqrt(1 + ShockSlope**2)
             SinSlope = ShockSlope*CosSlope

             ! Set rotational matrix
             Rot_II = reshape([CosSlope, SinSlope, -SinSlope, CosSlope],[2,2])

             if(ShockSlope /= 0.0 .and. UseWave .and. DoRotateWave) &
                  call rotate_wave

          end if  ! UseShockTube

          ! Loop through all the cells
          do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI

             DoTestCell = DoTest .and. i==iTest .and. j==jTest .and. k==kTest
             ! Default state
             State_VGB(:,i,j,k,iBlock) = CellState_VI(:,Coord1MaxBc_)

             if(DoTestCell)write(*,*) NameSub,': default state=', &
                  State_VGB(:,i,j,k,iBlock)

             if(.not.Used_GB(i,j,k,iBlock))then
                ! Cells outside the domain
                iBoundary = iBoundary_GB(i,j,k,iBlock)

                State_VGB(1:nVar,i,j,k,iBlock) = FaceState_VI(1:nVar,iBoundary)

                if(DoTestCell)write(*,*) NameSub,': face state=', &
                     State_VGB(:,i,j,k,iBlock)

                ! Convert velocity to momentum
                do iFluid = 1, nFluid
                   if(nFluid > 1) call select_fluid(iFluid)
                   State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock) = &
                        FaceState_VI(iUx:iUz,iBoundary) &
                        *FaceState_VI(iRho,iBoundary)
                end do

                if(DoTestCell)write(*,*) NameSub,': face cons.=', &
                     State_VGB(:,i,j,k,iBlock)

             elseif(UseInitialStateDefinition)then
                ! Cells defined by the #STATEDEFINITION command
                x = Xyz_DGB(x_,i,j,k,iBlock)
                y = Xyz_DGB(y_,i,j,k,iBlock)
                call get_initial_state( [x, y], State_VGB(:,i,j,k,iBlock) )

                if(DoTestCell)write(*,*) NameSub,': InitalState=', &
                     State_VGB(:,i,j,k,iBlock)
             elseif(UseShocktube .or. UseRadialState)then
                if(UseShockTube)then
                   if( (Xyz_DGB(x_,i,j,k,iBlock) - ShockPosition) &
                        < -ShockSlope*Xyz_DGB(y_,i,j,k,iBlock))then
                      ! Set all variables first
                      State_VGB(:,i,j,k,iBlock) = ShockLeft_V

                      if(DoTestCell)write(*,*) NameSub,': left State=', &
                           State_VGB(:,i,j,k,iBlock)
#ifndef SCALAR
                      ! Rotate vector variables
                      do iFluid = 1, nFluid
                         if(nFluid > 1) call select_fluid(iFluid)
                         State_VGB(iUx:iUy,i,j,k,iBlock) = &
                              matmul(Rot_II, ShockLeft_V(iUx:iUy))
                      end do
                      if(UseB) State_VGB(Bx_:By_,i,j,k,iBlock) = &
                           matmul(Rot_II, ShockLeft_V(Bx_:By_))
#endif
                      if(DoTestCell)write(*,*) NameSub, &
                           ': final left State=', State_VGB(:,i,j,k,iBlock)
                   else
                      ! Set all variables first
                      State_VGB(:,i,j,k,iBlock) = ShockRight_V
                      if(DoTestCell)write(*,*) NameSub,': right State=', &
                           State_VGB(:,i,j,k,iBlock)
#ifndef SCALAR
                      ! Set vector variables
                      do iFluid = 1, nFluid
                         if(nFluid > 1) call select_fluid(iFluid)
                         State_VGB(iUx:iUy,i,j,k,iBlock) = &
                              matmul(Rot_II, ShockRight_V(iUx:iUy))
                      end do
                      if(UseB) State_VGB(Bx_:By_,i,j,k,iBlock) = &
                           matmul(Rot_II, ShockRight_V(Bx_:By_))
#endif
                      if(DoTestCell)write(*,*) NameSub,&
                           ': final right State=', State_VGB(:,i,j,k,iBlock)
                   end if
                end if

                if(UseRadialState) call set_radial_state( &
                     Xyz_DGB(:,i,j,k,iBlock), State_VGB(:,i,j,k,iBlock))

                ! Apply "wave" perturbations
                if(UseWave)then
                   call apply_wave(i, j, k, iBlock)
                   if(DoTestCell)write(*,*) NameSub,': wave State=', &
                        State_VGB(:,i,j,k,iBlock)
                end if

#ifndef SCALAR
                ! Convert velocity to momentum
                do iFluid = 1, nFluid
                   if(nFluid > 1) call select_fluid(iFluid)
                   State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock) = &
                        State_VGB(iRho,i,j,k,iBlock) * &
                        State_VGB(iUx:iUz,i,j,k,iBlock)
                end do
#endif
                if(.not.UseB0 .or. UseB1)CYCLE
                ! Remove B0 from B (if any)
                State_VGB(Bx_:Bz_,i,j,k,iBlock) = &
                     State_VGB(Bx_:Bz_,i,j,k,iBlock) - B0_DGB(:,i,j,k,iBlock)
                if(DoTestCell)write(*,*) NameSub,': cons shock State=', &
                     State_VGB(:,i,j,k,iBlock)

                ! end if UseShockTube

             elseif(UseWave)then
                ! Apply waves/bump without shocktube
                call apply_wave(i, j, k, iBlock)
                if(DoTestCell)write(*,*) NameSub,': wave State=', &
                     State_VGB(:,i,j,k,iBlock)
#ifndef SCALAR
                ! Convert velocity to momentum
                do iFluid = 1, nFluid
                   if(nFluid > 1) call select_fluid(iFluid)
                   State_VGB(iRhoUx:iRhoUz,i,j,k,iBlock) = &
                        State_VGB(iRho,i,j,k,iBlock) * &
                        State_VGB(iUx:iUz,i,j,k,iBlock)
                end do
#endif
                if(.not.UseB0 .or. UseB1)CYCLE
                ! Remove B0 from B (if any)
                State_VGB(Bx_:Bz_,i,j,k,iBlock) = &
                     State_VGB(Bx_:Bz_,i,j,k,iBlock) - B0_DGB(:,i,j,k,iBlock)
                if(DoTestCell)write(*,*) NameSub,': cons wave State=', &
                     State_VGB(:,i,j,k,iBlock)

             end if

          end do; end do; end do

          if(EntropyConstant > 0.0)then
             do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
                State_VGB(iP_I,i,j,k,iBlock) = &
                     EntropyConstant*State_VGB(iRho_I,i,j,k,iBlock)**Gamma_I
             end do; end do; end do
             if(DoTest)write(*,*) NameSub,': entropy State=', &
                  State_VGB(:,iTest,jTest,kTest,iBlockTest)
          end if

          ! Correct electron fluid for wave perturbations
          if(UseEfield .and. DoCorrectElectronFluid .and. UseWave) then
             call correct_electronfluid_efield(State_VGB(:,:,:,:,iBlock), &
                  1, nI, 1, nJ, 1, nK, iBlock, DoHallCurrentIn=.true.,    &
                  DoGradPeIn=.false., DoCorrectEfieldIn=DoCorrectEfield)
             if(DoTest)write(*,*) NameSub,': electric State=', &
                  State_VGB(:,iTest,jTest,kTest,iBlockTest)
          end if
          if(UseConstrainB) call constrain_ics(iBlock)

          ! Apply user defined initial conditions
          if(UseUserICs)then
             call user_set_ics(iBlock)
             if(DoTest)write(*,*) NameSub,': user State=', &
                  State_VGB(:,iTest,jTest,kTest,iBlockTest)
          end if

          if(iSignRotationIC /= 0)then
             call add_rotational_velocity(iSignRotationIC, iBlock)
             if(DoTest)write(*,*) NameSub,': rot vel State=', &
                  State_VGB(:,iTest,jTest,kTest,iBlockTest)
          end if
          if(UseSaMhd)then
             call init_samhd(iBlock)
             if(DoTest)write(*,*) NameSub,': samhd State=', &
                  State_VGB(:,iTest,jTest,kTest,iBlockTest)
          end if
       end if ! not IsRestart

    end if ! Unused_B

    if(DoTest)write(*,*) &
         NameSub,': final State=',State_VGB(:,iTest,jTest,kTest,iBlockTest)

    call test_stop(NameSub, DoTest, iBlock)

  contains
    !==========================================================================
    subroutine rotate_wave

      ! Rotate wave parameters with the angle of the shock slope

      integer:: iVar, iVectorVar, iVarX, iVarY
      real:: x, y
      !------------------------------------------------------------------------
      DoRotateWave = .false.

      ! Rotate vector variables
      do iVectorVar = 1, nVectorVar
         ! X and Y indexes of the vector variables
         iVarX = iVectorVar_I(iVectorVar)
         iVarY = iVarX + 1

         ! Make sure that the X and Y components of vector variables
         ! have consistent wave parameters
         if(Width_V(iVarX) > 0.0 .and. Width_V(iVarY) == 0.0) &
              call copy_wave(iVarX, iVarY)
         if(Width_V(iVarY) > 0.0 .and. Width_V(iVarX) == 0.0) &
              call copy_wave(iVarY, iVarX)

         ! Rotate amplitudes with the angle of the shock slope
         x = Ampl_V(iVarX); y = Ampl_V(iVarY)
         Ampl_V(iVarX) = CosSlope*x - SinSlope*y
         Ampl_V(iVarY) = SinSlope*x + CosSlope*y
      end do

      ! Rotate wave vectors
      do iVar = 1, nVar
         x = KxWave_V(iVar); y = KyWave_V(iVar)
         KxWave_V(iVar) = CosSlope*x - SinSlope*y
         KyWave_V(iVar) = SinSlope*x + CosSlope*y
      end do

    end subroutine rotate_wave
    !==========================================================================
    subroutine apply_wave(i, j, k, iBlock)

      ! Apply wave/Gaussian/tophat perturbations at a given grid cell

      use ModGeometry, ONLY: &
           xMinBox, xMaxBox, yMinBox, yMaxBox, zMinBox, zMaxBox

      integer, intent(in):: i, j, k, iBlock

      integer:: iVar
      real:: x, y, z, r, Ampl
      character(len=*), parameter:: NameSub = 'apply_wave'
      !------------------------------------------------------------------------
      do iVar = 1, nVar

         ! Normalize amplitude
         Ampl = Ampl_V(iVar)*Io2No_V(iUnitPrim_V(iVar))

         if(Ampl == 0.0) CYCLE

         if(DoTestCell) &
              write(*,*) NameSub, 'iVar, Ampl, kX, kY, kZ, iPower=', &
              iVar, Ampl, KxWave_V(iVar), KyWave_V(iVar), KzWave_V(iVar), &
              iPower_V(iVar)

         if(iPower_V(iVar) <= 0)then
            ! Bump
            x = Xyz_DGB(x_,i,j,k,iBlock) - x_V(iVar)
            y = Xyz_DGB(y_,i,j,k,iBlock) - y_V(iVar)
            z = Xyz_DGB(z_,i,j,k,iBlock) - z_V(iVar)
            if(IsPeriodic_D(1))then
               if(x > +(xMaxBox-xMinBox)/2) x = x - (xMaxBox-xMinBox)
               if(x < -(xMaxBox-xMinBox)/2) x = x + (xMaxBox-xMinBox)
            end if
            if(IsPeriodic_D(2))then
               if(y > +(yMaxBox-yMinBox)/2) y = y - (yMaxBox-yMinBox)
               if(y < -(yMaxBox-yMinBox)/2) y = y + (yMaxBox-yMinBox)
            end if
            if(IsPeriodic_D(3))then
               if(z > +(zMaxBox-zMinBox)/2) z = z - (zMaxBox-zMinBox)
               if(z < -(zMaxBox-zMinBox)/2) z = z + (zMaxBox-zMinBox)
            end if
            ! normalize
            x = x*KxWave_V(iVar)
            y = y*KyWave_V(iVar)
            z = z*KzWave_V(iVar)
            r = sqrt(x**2 + y**2 + z**2)
            if(r < 0.5) State_VGB(iVar,i,j,k,iBlock) = &
                 State_VGB(iVar,i,j,k,iBlock) &
                 + Ampl * cos(cPi*r)**abs(iPower_V(iVar))
         else
            ! cos^n profile
            if(Width_V(iVar) > 0)then
               if(DoTestCell)write(*,*) NameSub,': Width=', Width_V(iVar)
               if(KxWave_V(iVar) > 0.0)then
                  if(abs(Xyz_DGB(x_,i,j,k,iBlock) &
                       + ShockSlope*Xyz_DGB(y_,i,j,k,iBlock)) &
                       > Width_V(iVar) ) CYCLE
               elseif(KyWave_V(iVar) > 0.0)then
                  if(abs(Xyz_DGB(y_,i,j,k,iBlock)) > Width_V(iVar) ) CYCLE
               elseif(KzWave_V(iVar) > 0.0)then
                  if(abs(Xyz_DGB(z_,i,j,k,iBlock)) > Width_V(iVar) ) CYCLE
               end if
            end if

            State_VGB(iVar,i,j,k,iBlock) =        &
                 State_VGB(iVar,i,j,k,iBlock)          &
                 + Ampl*cos(Phase_V(iVar)      &
                 + KxWave_V(iVar)*Xyz_DGB(x_,i,j,k,iBlock)  &
                 + KyWave_V(iVar)*Xyz_DGB(y_,i,j,k,iBlock)  &
                 + KzWave_V(iVar)*Xyz_DGB(z_,i,j,k,iBlock))**iPower_V(iVar)
         end if
      end do

    end subroutine apply_wave
    !==========================================================================
    subroutine copy_wave(iVar, jVar)

      ! Copy wave parameters from iVar to jVar for rotated problems

      integer, intent(in):: iVar, jVar
      logical:: DoTest
      character(len=*), parameter:: NameSub = 'copy_wave'
      !------------------------------------------------------------------------
      call test_start(NameSub, DoTest)
      Width_V(jVar)  = Width_V(iVar)
      KxWave_V(jVar) = KxWave_V(iVar)
      KyWave_V(jVar) = KyWave_V(iVar)
      KzWave_V(jVar) = KzWave_V(iVar)
      Phase_V(jVar)  = Phase_V(iVar)
      iPower_V(jVar) = iPower_V(iVar)

      call test_stop(NameSub, DoTest)
    end subroutine copy_wave
    !==========================================================================
  end subroutine set_initial_condition
  !============================================================================
  subroutine add_rotational_velocity(iSign, iBlockIn)

    use ModSize,     ONLY: MinI, MaxI, MinJ, MaxJ, MinK, MaxK, nBlock, x_, y_
    use ModMain,     ONLY: Unused_B, NameThisComp
    use ModAdvance,  ONLY: State_VGB
    use ModGeometry, ONLY: Used_GB
    use ModPhysics,  ONLY: OmegaBody
    use ModMultiFluid, ONLY: iRho_I, iRhoUx_I, iRhoUy_I
    use BATL_lib,    ONLY: Xyz_DGB, iProc

    integer, intent(in):: iSign
    integer, optional, intent(in):: iBlockIn

    ! Transform velocities between inertial and rotating frames
    ! where Omega is the angular velocity of the rotating frame
    ! Since Omega = (0,0,OmegaBody)
    ! ux = ux - iSign*OmegaBody*y
    ! uy = uy + iSign*OmegaBody*x
    ! iSign=+1: from rotating to inertial frame
    ! iSign=-1: from inertial to rotating frame
    !
    ! If iBlockIn is present, do that block, otherwise do all blocks.

    integer :: i, j, k, iBlock, iBlockFirst, iBlockLast
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'add_rotational_velocity'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(present(iBlockIn))then
       iBlockFirst = iBlockIn; iBlockLast = iBlockIn
    else
       iBlockFirst = 1; iBlockLast = nBlock
       if(iProc==0)write(*,'(a)')&
            NameThisComp//': add rotational velocity to convert coords'
    end if

    do iBlock = iBlockFirst, iBlockLast
       if(Unused_B(iBlock))CYCLE
       do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
          if(.not.Used_GB(i,j,k,iBlock)) CYCLE
          State_VGB(iRhoUx_I,i,j,k,iBlock) = State_VGB(iRhoUx_I,i,j,k,iBlock) &
               - iSign*State_VGB(iRho_I,i,j,k,iBlock) &
	       *OmegaBody*Xyz_DGB(y_,i,j,k,iBlock)

          State_VGB(iRhoUy_I,i,j,k,iBlock) = State_VGB(iRhoUy_I,i,j,k,iBlock) &
	       + iSign*State_VGB(iRho_I,i,j,k,iBlock) &
	       *OmegaBody*Xyz_DGB(x_,i,j,k,iBlock)

       end do; end do; end do
    end do

    call test_stop(NameSub, DoTest)
  end subroutine add_rotational_velocity
  !============================================================================
end module ModSetInitialCondition
!==============================================================================
