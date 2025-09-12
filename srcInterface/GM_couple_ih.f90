!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
!^CMP FILE IH

module GM_couple_ih

  ! Couple with IH component

  use ModUtilities, ONLY: CON_set_do_test, CON_stop

  implicit none
  save

  private ! except

  public:: GM_put_from_mh           ! coupling toolkit based coupler
  public:: GM_put_from_ih_buffer    ! buffer grid based coupler
  public:: GM_get_for_global_buffer ! buffer grid based coupler

  character(len=3),  public:: NameCoord
  integer,           public:: nY, nZ
  real,              public:: yMin, yMax, zMin, zMax
  real, allocatable, public:: State_VII(:,:,:)

contains
  !============================================================================
  subroutine GM_put_from_mh( &
       nPartial, iPutStart, Put, Weight, DoAdd, StateSI_V, nVar)

    ! transform and put the data got from IH_

    use CON_router, ONLY: IndexPtrType, WeightPtrType
    use ModAdvance, ONLY: State_VGB,rho_,rhoUx_,rhoUz_,Bx_,Bz_,P_
    use ModB0, ONLY: B0_DGB
    use ModPhysics, ONLY: Si2No_V, UnitRho_, UnitRhoU_, UnitP_, UnitB_

    integer,intent(in)::nPartial,iPutStart,nVar
    type(IndexPtrType),intent(in)::Put
    type(WeightPtrType),intent(in)::Weight
    logical,intent(in)::DoAdd
    real,dimension(nVar),intent(in)::StateSI_V

    real,dimension(nVar)::State_V
    integer:: i, j, k, iBlock

    ! The meaning of state intdex in buffer and in model can be
    ! different. Below are the conventions for buffer:
    integer,parameter::&
         BuffRho_  =1,&
         BuffRhoUx_=2,&
         BuffRhoUz_=4,&
         BuffBx_   =5,&
         BuffBy_   =6,&
         BuffBz_   =7,&
         BuffP_    =8

    character(len=*), parameter:: NameSub = 'GM_put_from_mh'
    !--------------------------------------------------------------------------
    State_V(BuffRho_)              = StateSI_V(BuffRho_)*Si2No_V(UnitRho_)
    State_V(BuffRhoUx_:BuffRhoUz_) = StateSI_V(BuffRhoUx_:BuffRhoUz_) &
         *Si2No_V(UnitRhoU_)
    State_V(BuffBx_:BuffBz_)       = StateSI_V(BuffBx_:BuffBz_)*Si2No_V(UnitB_)
    State_V(BuffP_)                = StateSI_V(BuffP_)         *Si2No_V(UnitP_)

    i      = Put%iCB_II(1,iPutStart)
    j      = Put%iCB_II(2,iPutStart)
    k      = Put%iCB_II(3,iPutStart)
    iBlock = Put%iCB_II(4,iPutStart)

    if(DoAdd)then
       State_VGB(rho_,i,j,k,iBlock) = State_VGB(rho_,i,j,k,iBlock) + &
            State_V(BuffRho_)
       State_VGB(rhoUx_:rhoUz_,i,j,k,iBlock) = &
            State_VGB(rhoUx_:rhoUz_,i,j,k,iBlock) + &
            State_V(BuffRhoUx_:BuffRhoUz_)
       State_VGB(Bx_:Bz_,i,j,k,iBlock) = &
            State_VGB(Bx_:Bz_,i,j,k,iBlock) + &
            State_V(BuffBx_:BuffBz_)
       State_VGB(P_,i,j,k,iBlock) = State_VGB(P_,i,j,k,iBlock) + &
            State_V(BuffP_)

    else
       State_VGB(rho_,i,j,k,iBlock)= State_V(BuffRho_)
       State_VGB(rhoUx_:rhoUz_,i,j,k,iBlock) =  State_V(BuffRhoUx_:BuffRhoUz_)
       State_VGB(Bx_:Bz_,i,j,k,iBlock) = State_V(BuffBx_:BuffBz_) - &
            B0_DGB(:,i,j,k,iBlock)
       State_VGB(P_,i,j,k,iBlock)  = State_V(BuffP_)
    end if

  end subroutine GM_put_from_mh
  !============================================================================
  subroutine GM_put_from_ih_buffer( &
       NameCoordIn, nYIn, nZIn, yMinIn, yMaxIn, zMinIn, zMaxIn, Buffer_VII)

    use ModVarIndexes
    use ModPhysics, ONLY: Si2No_V, UnitX_,UnitRho_,UnitU_,UnitB_,UnitP_
    use ModMain, ONLY: TypeCellBc_I

    character(len=*), intent(in) :: NameCoordIn
    integer,          intent(in) :: nYIn, nZIn
    real,             intent(in) :: yMinIn, yMaxIn, zMinIn, zMaxIn
    real,             intent(in) :: Buffer_VII(nVar, nYIn, nZIn)

    integer                      :: j, k
    character(len=*), parameter:: NameSub = 'GM_put_from_ih_buffer'
    !--------------------------------------------------------------------------
    if(.not.allocated(State_VII)) then
       ! Check coordinate system. Only GSM and GSE make sense.
       if(NameCoordIn /= 'GSM' .and. NameCoord /= 'GSE') &
            call CON_stop(NameSub//': cannot handle coord system=' &
            //NameCoordIn)

       ! Store grid information
       NameCoord = NameCoordIn
       yMin = yMinIn * Si2No_V(UnitX_)
       yMax = yMaxIn * Si2No_V(UnitX_)
       zMin = zMinIn * Si2No_V(UnitX_)
       zMax = zMaxIn * Si2No_V(UnitX_)
       nY   = nYIn
       nZ   = nZIn
       ! Allocate space
       allocate(State_VII(nVar,nY,nZ))

       ! Make sure that GM uses the IH buffer
       TypeCellBc_I(2) = 'ihbuffer'

       ! Debugging
       ! write(*,*)'!!! NameCoord, nY, nZ=',NameCoord,nY,nZ
       ! write(*,*)'!!! yMin, yMax, zMin, zMax=',yMin, yMax, zMin, zMax
    end if

    ! Store input data
    State_VII = Buffer_VII

    ! Convert units and velocity to momentum
    do k = 1, nZ; do j = 1, nY
       State_VII(Rho_,j,k)          = State_VII(Rho_,j,k)    *Si2No_V(UnitRho_)
       State_VII(RhoUx_:RhoUz_,j,k) = &
            State_VII(Rho_,j,k)*State_VII(Rhoux_:RhoUz_,j,k) *Si2No_V(UnitU_)
       State_VII(Bx_:Bz_,j,k)       = State_VII(Bx_:Bz_,j,k) *Si2No_V(UnitB_)
       State_VII(P_,j,k)            = State_VII(P_,j,k)      *Si2No_V(UnitP_)
    end do; end do

  end subroutine GM_put_from_ih_buffer
  !============================================================================
  subroutine GM_get_for_global_buffer( &
       nR, nLon, nLat, BufferMinMax_DI, Buffer_VG)

    ! This subroutines fills a buffer grid by interpolating from a source
    ! GM_BATSRUS grid using second-order trilinear interpolation.

    ! The buffer grid can be a spherical shell, or a segment of such a shell.

    ! All state variables in the source grid are interpolated, but only those
    ! needed for coupling (as determined by CON_coupler) are actually passed.

    ! The filled buffer state vector is converted to SI units and vector
    ! quantities are rotated to the target component coordinate system.

    ! INPUT:

    ! nR, nLon, nLat: grid spacing for the buffer grid
    ! BufferMinMAx_DI : Buffer grid minimum and maximum coordinates, in all
    ! dimensions.

    ! OUTPUT:

    ! Buffer_VG : defined for all coupling variables and all buffer grid points
    ! (including buffer ghost cells).

    use ModSize, ONLY: nI, nJ, nK, MinI, MaxI, MinJ, MaxJ, MinK, MaxK
    use ModMain, ONLY: UseB0, rUpperModel
    use ModAdvance, ONLY: State_VGB, UseElectronPressure
    use ModB0, ONLY: B0_DGB
    use ModUpdateStateFast, ONLY: sync_cpu_gpu
    use ModPhysics, ONLY: &
         No2Si_V, UnitRho_, UnitP_, UnitRhoU_, UnitB_, UnitEnergyDens_, UnitU_
    use ModVarIndexes, ONLY: &
         Rho_, RhoUx_, RhoUz_, Bx_, Bz_, P_, Pe_, &
         Ppar_, WaveFirst_, WaveLast_, Ehot_, nVar, &
         ChargeStateFirst_, ChargeStateLast_, SignB_
    use CON_coupler, ONLY: &
         RhoCouple_, RhoUxCouple_,&
         RhoUzCouple_, PCouple_, BxCouple_, BzCouple_,  &
         PeCouple_, PparCouple_, WaveFirstCouple_,  &
         WaveLastCouple_, Bfield_, Wave_, EhotCouple_, &
         AnisoPressure_, ElectronPressure_,&
         CollisionlessHeatFlux_, ChargeStateFirstCouple_, &
         ChargeStateLastCouple_, ChargeState_, iVar_V, &
         DoCoupleVar_V, nVarCouple, SaMhd_, SaMhdCouple_
    use ModCoordTransform, ONLY: rlonlat_to_xyz
    use ModInterpolate, ONLY: trilinear
    use BATL_lib, ONLY: iProc, &
         find_grid_block, xyz_to_coord, CoordMin_DB, CellSize_DB

    ! Buffer size and limits
    integer,intent(in):: nR, nLon, nLat
    real, intent(in):: BufferMinMax_DI(3,2)

    ! State variables to be fiiled in all buffer grid points
    real, intent(out):: Buffer_VG(nVarCouple,nR,nLon,nLat)

    ! variables for defining the buffer grid
    integer :: nCell_D(3)
    real    :: SphMin_D(3), SphMax_D(3), dSph_D(3), Sph_D(3)

    ! Variables for interpolating from a grid block to a buffer grid point

    ! Store complete interpolated state vector
    real :: StateInPoint_V(nVar)

    ! Store interpolated state variables needed for coupling
    real :: Buffer_V(nVarCouple), B0_D(3)

    ! Buffer grid cell center coordinates
    real :: CoordBuffer_D(3), XyzBuffer_D(3)

    ! Buffer grid cell center position  normalized by grid spacing
    ! (in GM_BATSRUS grid generalized coordinates)
    real :: BufferNorm_D(3)

    ! variable indices in buffer
    integer   :: &
         iRhoCouple,              &
         iRhoUxCouple,            &
         iRhoUzCouple,            &
         iPCouple,                &
         iPeCouple,               &
         iPparCouple,             &
         iBxCouple,               &
         iBzCouple,               &
         iWaveFirstCouple,        &
         iWaveLastCouple,         &
         iChargeStateFirstCouple, &
         iChargeStateLastCouple,  &
         iEhotCouple

    integer   :: iBlock, iPe, iR, iLon, iLat
    logical   :: DoTest, DoTestMe

    character(len=*), parameter:: NameSub = 'GM_get_for_global_buffer'
    !--------------------------------------------------------------------------
    call CON_set_do_test(NameSub,DoTest,DoTestMe)

    call sync_cpu_gpu('update on CPU', NameSub, State_VGB, B0_DGB)

    Buffer_VG = 0.0

    ! get variable indices in buffer
    iRhoCouple              = iVar_V(RhoCouple_)
    iRhoUxCouple            = iVar_V(RhoUxCouple_)
    iRhoUzCouple            = iVar_V(RhoUzCouple_)
    iPCouple                = iVar_V(PCouple_)
    iPeCouple               = iVar_V(PeCouple_)
    iPparCouple             = iVar_V(PparCouple_)
    iBxCouple               = iVar_V(BxCouple_)
    iBzCouple               = iVar_V(BzCouple_)
    iWaveFirstCouple        = iVar_V(WaveFirstCouple_)
    iWaveLastCouple         = iVar_V(WaveLastCouple_)
    iEhotCouple             = iVar_V(EhotCouple_)
    iChargeStateFirstCouple = iVar_V(ChargeStateFirstCouple_)
    iChargeStateLastCouple  = iVar_V(ChargeStateLastCouple_)

    ! Calculate buffer grid spacing
    nCell_D  = [nR, nLon, nLat]
    SphMin_D = BufferMinMax_DI(:,1)
    SphMax_D = BufferMinMax_DI(:,2)

    ! Save the upper boundary radius as the limit for LOS integration span
    rUpperModel = SphMax_D(1)

    dSph_D     = (SphMax_D - SphMin_D)/real(nCell_D)
    dSph_D(1) = (SphMax_D(1) - SphMin_D(1))/(nCell_D(1) - 1)

    ! Loop over buffer grid points
    do iLat = 1, nLat ; do iLon = 1, nLon ; do iR = 1, nR

       ! Find the coordinates of the current buffer grid point,
       Sph_D = SphMin_D + [real(iR - 1), real(iLon)-0.5, real(iLat)-0.5]*dSph_D
       ! Find Xyz coordinates of the grid point
       call rlonlat_to_xyz(Sph_D, XyzBuffer_D)

       ! Find the block and PE in the GM_BATSRUS grid
       call find_grid_block(XyzBuffer_D, iPe, iBlock)

       ! Check if this block belongs to this processor
       if (iProc /= iPe) CYCLE

       ! Convert buffer grid point Xyz to GM_BATSRUS generalized coords
       call xyz_to_coord(XyzBuffer_D, CoordBuffer_D)

       ! Buffer grid point gen coords normalized by the block grid spacing
       BufferNorm_D = (CoordBuffer_D - CoordMin_DB(:,iBlock)) &
            /CellSize_DB(:,iBlock) + 0.5

       ! Interpolate from the true solution block to the buffer grid point
       StateInPoint_V = trilinear(State_VGB(:,:,:,:,iBlock),      &
            nVar, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, BufferNorm_D)

       ! Fill in the coupled state variables, convert to SI units

       Buffer_V(iRhoCouple)= StateInPoint_V(rho_)*No2Si_V(UnitRho_)
       Buffer_V(iRhoUxCouple:iRhoUzCouple) = &
            StateInPoint_V(rhoUx_:rhoUz_)*No2Si_V(UnitRhoU_)

       if(DoCoupleVar_V(Bfield_)) then
          if(UseB0)then
             B0_D = &
                  trilinear(B0_DGB(:,:,:,:,iBlock), &
                  3, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, &
                  BufferNorm_D, DoExtrapolate = .TRUE.)
             Buffer_V(iBxCouple:iBzCouple) = &
                  (StateInPoint_V(Bx_:Bz_) + B0_D)*No2Si_V(UnitB_)
          else
             Buffer_V(iBxCouple:iBzCouple) = &
                  StateInPoint_V(Bx_:Bz_)*No2Si_V(UnitB_)
          end if
       end if

       Buffer_V(iPCouple)  = StateInPoint_V(p_)*No2Si_V(UnitP_)

       if(DoCoupleVar_V(Wave_)) &
            Buffer_V(iWaveFirstCouple:iWaveLastCouple) = &
            StateInPoint_V(WaveFirst_:WaveLast_)&
            * No2Si_V(UnitEnergyDens_)

       if(DoCoupleVar_V(ChargeState_)) &
            Buffer_V(iChargeStateFirstCouple:iChargeStateLastCouple) = &
            StateInPoint_V(ChargeStateFirst_:ChargeStateLast_)&
            * No2Si_V(UnitRho_)

       if(DoCoupleVar_V(ElectronPressure_))then
          Buffer_V(iPeCouple) = StateInPoint_V(Pe_)*No2Si_V(UnitP_)
       else if(UseElectronPressure)then
          Buffer_V(iPCouple) = Buffer_V(iPCouple) + StateInPoint_V(Pe_)&
               *No2Si_V(UnitP_)
       end if

       if(DoCoupleVar_V(AnisoPressure_)) Buffer_V(iPparCouple) = &
            StateInPoint_V(Ppar_)*No2Si_V(UnitP_)

       if(DoCoupleVar_V(CollisionlessHeatFlux_)) Buffer_V(iEhotCouple) = &
            StateInPoint_V(Ehot_)*No2Si_V(UnitEnergyDens_)
       if(DoCoupleVar_V(SaMhd_))Buffer_V(iVar_V(SaMhdCouple_)) = &
            StateInPoint_V(SignB_)*No2Si_V(UnitB_)/No2Si_V(UnitU_)

       ! DONE - fill the buffer grid
       Buffer_VG(:,iR, iLon,iLat) = Buffer_V

    end do; end do; end do

  end subroutine GM_get_for_global_buffer
  !============================================================================
end module GM_couple_ih
!==============================================================================
