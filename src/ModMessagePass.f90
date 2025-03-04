!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModMessagePass

  use BATL_lib, ONLY: &
       test_start, test_stop

  ! Message passing to fill in ghost cells.
  !
  ! Also methods related to the buffer grid that acts like
  ! ghost cells in a spherical shell.

  implicit none

  private ! except

  public:: exchange_messages   ! fill ghost cells and (re)calculate energies

  ! True if it is sufficient to fill in the fine ghost cells with a single
  ! layer of the coarse cell values. Set in ModSetParameters.
  logical, public:: DoOneCoarserLayer = .true.
  character(len=*), parameter:: NameMod = 'ModMessagePass'
contains
  !============================================================================
  subroutine exchange_messages(DoResChangeOnlyIn, UseOrder2In, UseBufferIn)

    use ModCellBoundary, ONLY: set_cell_boundary, set_edge_corner_ghost
    use ModBoundaryGeometry, ONLY: fix_boundary_ghost_cells
    use ModMain, ONLY : &
         IsTimeLoop, UseConstrainB, iNewDecomposition, &
         nOrder, nOrderProlong, TypeMessagePass, &
         UseHighResChange, UseBufferGrid, UseResistivePlanet
    use ModVarIndexes
    use ModAdvance,  ONLY: State_VGB, iTypeUpdate, UpdateOrig_, UpdateFast_
    use ModGeometry, ONLY: IsBoundary_B
    use ModPhysics,  ONLY: ShockSlope, nVectorVar, iVectorVar_I
    use ModFaceValue, ONLY: UseAccurateResChange
    use ModEnergy,   ONLY: limit_pressure
    use ModCoordTransform, ONLY: rot_xyz_sph
    use ModParticleMover, ONLY:  UseBoundaryVdf, set_boundary_vdf
    use ModBuffer,   ONLY: fill_in_from_buffer
    use ModUpdateStateFast, ONLY: set_boundary_fast, sync_cpu_gpu

    use BATL_lib, ONLY: message_pass_cell, DiLevelNei_IIIB, nG, &
         MinI, MaxI, MinJ, MaxJ, MinK, MaxK, nBlock, Unused_B, Xyz_DGB, &
         IsSpherical, IsRLonLat, IsPeriodic_D, IsPeriodicCoord_D
    use ModMpi

    ! Fill ghost cells at res. change only
    logical, optional, intent(in) :: DoResChangeOnlyIn

    ! Use 2nd order prolongation to fill
    logical, optional, intent(in) :: UseOrder2In

    ! If .true. apply solution on the buffer grid,
    ! to fill in the cells within the buffer grid ranges
    logical, optional, intent(in) :: UseBufferIn

    logical :: UseOrder2
    logical :: DoResChangeOnly
    logical :: UseBuffer

    logical :: DoRestrictFace, DoTwoCoarseLayers, DoSendCorner
    integer :: nWidth, nCoarseLayer

    integer :: iBlock

    logical :: UseHighResChangeNow

    logical :: IsFound

    character(len=30):: TypeBc

!!! TO BE GENERALIZED
    logical:: IsPeriodicWedge = .false.
    integer:: iVector, iVar, i, j, k
    real   :: XyzSph_DD(3,3)

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'exchange_messages'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! This way of doing periodic BC for wedge is not perfect.
    ! It does not work for AMR or semi-implicit scheme with vectors.
    ! But it works for a number of simple but useful applications.
    ! A periodic wedge BC is needed if there is a periodic boundary condition
    ! for a non-periodic angular coordinate.
    ! In this case the vector variables are converted to spherical components
    ! during the message passing.
    IsPeriodicWedge = (IsSpherical .or. IsRLonLat) .and. &
         (any(IsPeriodic_D(2:3) .and. .not. IsPeriodicCoord_D(2:3)))

    DoResChangeOnly = .false.
    if(present(DoResChangeOnlyIn)) DoResChangeOnly = DoResChangeOnlyIn

    UseOrder2 = .false.
    if(present(UseOrder2In)) UseOrder2 = UseOrder2In

    UseBuffer = IsTimeLoop.and.UseBufferGrid
    if(present(UseBufferIn)) UseBuffer = UseBufferIn

    DoRestrictFace = nOrderProlong==1
    if(UseConstrainB) DoRestrictFace = .false.

    DoTwoCoarseLayers = &
         nOrder > 1 .and. nOrderProlong == 1 .and. .not.DoOneCoarserLayer

    UseHighResChangeNow = nOrder == 5 .and. UseHighResChange

    if(DoTest)write(*,*) NameMod//':'//NameSub, &
         ': DoResChangeOnly, UseOrder2, DoRestrictFace, DoTwoCoarseLayers,'//&
         ' UseBuffer, nOrderProlong, TypeMessagePass=', &
         DoResChangeOnly, UseOrder2, DoRestrictFace, DoTwoCoarseLayers, &
         UseBuffer, nOrderProlong, trim(TypeMessagePass)

    call timing_start('exch_msgs')

    call sync_cpu_gpu('update on GPU', NameSub, State_VGB)

    ! Apply boundary conditions
    if(.not.DoResChangeOnly)then
       call timing_start('cell_bc')
       if(iTypeUpdate == UpdateFast_) then
          call set_boundary_fast
       else
          do iBlock = 1, nBlock
             if (Unused_B(iBlock)) CYCLE
             if (IsBoundary_B(iBlock) .and. &
                  (nOrderProlong==2 .or. UseHighResChangeNow)) then
                call set_cell_boundary(nG, iBlock, &
                     nVar, State_VGB(:,:,:,:,iBlock))
                if(UseHighResChangeNow)call set_edge_corner_ghost(nG, iBlock, &
                     nVar, State_VGB(:,:,:,:,iBlock))
             endif
          end do
       end if
       call timing_stop('cell_bc')
    end if

    if(IsPeriodicWedge)then
       do iBlock = 1, nBlock
          if (Unused_B(iBlock)) CYCLE
          ! Skip blocks not at the boundary !!!
          do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
             XyzSph_DD = rot_xyz_sph(Xyz_DGB(:,i,j,k,iBlock))
             do iVector = 1, nVectorVar
                iVar = iVectorVar_I(iVector)
                State_VGB(iVar:iVar+2,i,j,k,iBlock) = &
                     matmul(State_VGB(iVar:iVar+2,i,j,k,iBlock), XyzSph_DD)
             end do
          end do; end do; enddo
       end do
    end if

    if (UseOrder2 .or. nOrderProlong > 1) then
       call sync_cpu_gpu('change on GPU', NameSub, State_VGB)
       if(DoTest) write(*,*) NameSub,' call 1 of message_pass_cell'
       call message_pass_cell(nVar, State_VGB,&
            DoResChangeOnlyIn=DoResChangeOnlyIn,&
            UseOpenACCIn=.true., iDecomposition=iNewDecomposition)
    elseif (TypeMessagePass=='all') then
       ! If ShockSlope is not zero then even the first order scheme needs
       ! all ghost cell layers to fill in the corner cells at the sheared BCs.
       nWidth = nG; if(nOrder == 1 .and. ShockSlope == 0.0)  nWidth = 1
       nCoarseLayer = 1; if(DoTwoCoarseLayers) nCoarseLayer = 2
       call sync_cpu_gpu('change on GPU', NameSub, State_VGB)
       if(DoTest) write(*,*) NameSub,' call 2 of message_pass_cell'
       call message_pass_cell(nVar, State_VGB, &
            nWidthIn=nWidth, nProlongOrderIn=1, &
            nCoarseLayerIn=nCoarseLayer, DoRestrictFaceIn = DoRestrictFace,&
            DoResChangeOnlyIn=DoResChangeOnlyIn, &
            UseHighResChangeIn=UseHighResChangeNow,&
            DefaultState_V=DefaultState_V, &
            UseOpenACCIn=.true., &
            iDecomposition=iNewDecomposition)
    else
       ! Pass corners if necessary
       DoSendCorner = nOrder > 1 .and. UseAccurateResChange
       ! Pass one layer if possible
       nWidth = nG;      if(nOrder == 1)       nWidth = 1
       nCoarseLayer = 1; if(DoTwoCoarseLayers) nCoarseLayer = 2
       if(DoTest) write(*,*) NameSub,' call 3 of message_pass_cell'
       call message_pass_cell(nVar, State_VGB, &
            nWidthIn=nWidth, &
            nProlongOrderIn=1, &
            nCoarseLayerIn=nCoarseLayer,&
            DoSendCornerIn=DoSendCorner, &
            DoRestrictFaceIn=DoRestrictFace,&
            DoResChangeOnlyIn=DoResChangeOnlyIn,&
            UseHighResChangeIn=UseHighResChangeNow,&
            DefaultState_V=DefaultState_V, &
            UseOpenACCIn=.true., &
            iDecomposition=iNewDecomposition)
    end if
    ! If the grid changed, fix iBoundary_GB
    ! This could/should be done where the grid is actually being changed,
    ! for example in load_balance
    if(.not.DoResChangeOnly) call fix_boundary_ghost_cells

    if(IsPeriodicWedge)then
       do iBlock = 1, nBlock
          if (Unused_B(iBlock)) CYCLE
          ! Skip blocks not at the boundary !!!
          do k=MinK,MaxK; do j=MinJ,MaxJ; do i=MinI,MaxI
             XyzSph_DD = rot_xyz_sph(Xyz_DGB(:,i,j,k,iBlock))
             do iVector = 1, nVectorVar
                iVar = iVectorVar_I(iVector)
                State_VGB(iVar:iVar+2,i,j,k,iBlock) = &
                     matmul(XyzSph_DD, State_VGB(iVar:iVar+2,i,j,k,iBlock))
             end do
          end do; end do; enddo
       end do
    end if

    ! The corner ghost cells outside the domain are updated
    ! from the ghost cells inside the domain, so the outer
    ! boundary condition have to be reapplied.
    if(iTypeUpdate == UpdateFast_)then
       call set_boundary_fast(DoResChangeOnly, .true.)
    else
       call timing_start('cell_bc')
       !$omp parallel do
       do iBlock = 1, nBlock
          if (Unused_B(iBlock)) CYCLE
          if(.not.DoResChangeOnly &
               .or. any(abs(DiLevelNei_IIIB(:,:,:,iBlock)) == 1) )then
             if (IsBoundary_B(iBlock)) then
                call set_cell_boundary( &
                     nG, iBlock, nVar, State_VGB(:,:,:,:,iBlock))

                ! Fill in boundary cells with hybrid particles
                if(UseBoundaryVdf)call set_boundary_vdf(iBlock)

             end if

             if(UseBuffer)call fill_in_from_buffer(iBlock)

          end if

          ! Maybe only ghost cells at res changes need this
          call limit_pressure(MinI, MaxI, MinJ, MaxJ, MinK, MaxK, iBlock, &
               1, nFluid, State_VGB)

          if(UseResistivePlanet) then
             TypeBc = 'ResistivePlanet'
             call user_set_cell_boundary(iBlock,-1,TypeBc,IsFound)
          end if

       end do
       !$omp end parallel do

       if(.not.DoResChangeOnly)UseBoundaryVdf = .false.
       call timing_stop('cell_bc')
    end if
    call timing_stop('exch_msgs')
    if(DoTest)call timing_show('exch_msgs',1)

    if(DoTest)write(*,*) NameSub,' finished'

    call test_stop(NameSub, DoTest)
  end subroutine exchange_messages
  !============================================================================
end module ModMessagePass
!==============================================================================
