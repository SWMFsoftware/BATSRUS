!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModCoarseAxis

  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest, iBlockTest
  use ModBatsrusUtility, ONLY: stop_mpi

  implicit none
  SAVE
  logical:: UseCoarseAxis = .false.
  integer:: nCoarseLayer = 3
  !$acc declare create(UseCoarseAxis,nCoarseLayer)

  ! If nCoarseLayer=1, then each pair of the cells near the axis are merged
  !----------a x i s---------------
  ! |   |   |          first layer "|" denote the cell boundaries
  ! | | | | |          second layer
  ! ...
  ! In case nCoarseLayer=2, then each 4 cells are merged near the axis,
  ! each pair of cells is merged in the second layer.
  ! ---------a x i s---------------
  ! |       |          first layer
  ! |   |   |          second layer "|" denote the cell boundaries
  ! | | | | |          third layer
  ! ...
  ! In case nCoarseLayer=3, then each 8 cells are merged in the first layer,
  ! each 4 cells are merged in the second layer, each 2 in the third layer
  ! ---------a x i s---------------
  ! |               |  first layer
  ! |       |       |  second layer
  ! |   |   |   |   |  third layer "|" denote the cell boundaries
  ! | | | | | | | | |  fourth layer
  ! ..............
  !

  integer,parameter:: NorthHemiSph_ = 2, SouthHemiSph_ = 1

contains
  !============================================================================
  subroutine read_coarse_axis_param

    use ModReadParam, ONLY:read_var
    use ModSize, ONLY: nJ

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'read_coarse_axis_param'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    call read_var('UseCoarseAxis', UseCoarseAxis)
    call read_var('nCoarseLayer', nCoarseLayer)
    if( ( nJ/(2**nCoarseLayer) )*(2**nCoarseLayer)/=nJ)&
         call stop_mpi('nJ must be a multiple of 2^nCoarseLayer')
    call test_stop(NameSub, DoTest)

    !$acc update device(UseCoarseAxis,nCoarseLayer)

  end subroutine read_coarse_axis_param
  !============================================================================
  subroutine calc_coarse_axis_timestep(iBlock,iHemisphere)
    !$acc routine vector

    use ModSize, ONLY: nI, nJ, nK
    use ModAdvance,  ONLY: Flux_VXI, Flux_VYI, Flux_VZI, Vdt_, DtMax_CB
    use BATL_lib, ONLY: CellVolume_GB,Used_GB
    use ModUtilities, ONLY: i_gang

    integer, intent(in):: iBlock, iHemisphere

    ! Misc
    ! Loop variables
    integer :: i, j, k, jMerge, jStart, jLast, kLayer, kStride
    integer:: iGang
    real:: VDt
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_coarse_axis_timestep'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    iGang = i_gang(iBlock)

    select case(iHemisphere)
    case(NorthHemiSph_)
       k = nK - nCoarseLayer; kStride =  1; jMerge = 1
    case(SouthHemiSph_)
       k =  1 + nCoarseLayer; kStride = -1; jMerge = 1
    case default
       call stop_mpi('Algorithmic Error in'//NameSub)
    end select

    do kLayer = 1, nCoarseLayer
       k = k + kStride; jMerge = jMerge*2
       do j = 1, nJ/jMerge
          jStart = (j-1)*jMerge + 1; jLast = j*jMerge
          do i = 1, nI
             if(any(.not. Used_GB(i,jStart:jLast,k,iBlock))) then
                DtMax_CB(i,jStart:jLast,k,iBlock) = 0
             else
                Vdt =  sum( &
                     max(Flux_VXI(Vdt_,i,jStart:jLast,k,iGang), &
                     Flux_VXI(Vdt_,i+1,jStart:jLast,k,iGang)))&
                     + max(Flux_VYI(Vdt_,i,jStart,k,iGang), &
                     Flux_VYI(Vdt_,i,jLast+1,k,iGang)) &
                     + sum(max(Flux_VZI(Vdt_,i,jStart:jLast,k,iGang), &
                     Flux_VZI(Vdt_,i,jStart:jLast,k+1,iGang) ))

                DtMax_CB(i,jStart:jLast,k,iBlock) = &
                     jMerge*CellVolume_GB(i,jStart,k,iBlock) / Vdt
             end if
          end do
       end do
    end do

    call test_stop(NameSub, DoTest, iBlock)

  end subroutine calc_coarse_axis_timestep
  !============================================================================
  subroutine coarsen_axis_cells

    use ModMain, ONLY: nI, nJ, nK, nBlock, Unused_B
    use ModAdvance, ONLY: nVar, State_VGB
    use BATL_lib, ONLY: CoordMin_DB, CoordMax_DB, Lat_, IsRLonLat
    use ModNumConst, ONLY: cHalfPi

    integer :: i, j, k, iBlock,&
         jMerge, jStart, jLast, iVar, kLayer, kStride

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'coarsen_axis_cells'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(DoTest)then
       if(.not.Unused_B(iBlockTest)) &
            write(*,*) NameSub,' initial state, energy=', &
            State_VGB(:,iTest,jTest,kTest,iBlockTest)
    end if

    if(.not.IsRLonLat) &
         call stop_mpi(NameSub//': invalid geometry')

    !$acc parallel
    !$acc loop independent
    do iBlock = 1, nBlock
       if(Unused_B(iBlock))CYCLE

       if(CoordMax_DB(Lat_,iBlock) > cHalfPi - 1e-8)then
          k = nK - nCoarseLayer; kStride =  1; jMerge = 1
       elseif(CoordMin_DB(Lat_,iBlock) < -cHalfPi + 1e-8)then
          k =  1 + nCoarseLayer; kStride = -1; jMerge = 1
       else
          CYCLE
       end if

       !$acc loop
       do kLayer = 1, nCoarseLayer
          ! Should we try to parallelize the kLayer and j loops?
          ! k = k0 + kLayer*kStride; jMerge = 2**kLayer
          k = k + kStride; jMerge = jMerge*2

          !$acc loop
          do j = 1, nJ/jMerge
             jStart = (j-1)*jMerge + 1; jLast = j*jMerge
             !$acc loop vector collapse(2) independent
             do i = 1, nI
                do iVar = 1, nVar
                   State_VGB(iVar,i,jStart:jLast,k,iBlock) = &
                        sum(State_VGB(iVar,i,jStart:jLast,k,iBlock))/jMerge
                end do
             end do
          end do
       end do
    end do
    !$acc end parallel

    if(DoTest)then
       if(.not.Unused_B(iBlockTest)) &
            write(*,*) NameSub,' final state, energy=', &
            State_VGB(:,iTest,jTest,kTest,iBlockTest)
    end if

    call test_stop(NameSub, DoTest)

  end subroutine coarsen_axis_cells
  !============================================================================
end module ModCoarseAxis
!==============================================================================
