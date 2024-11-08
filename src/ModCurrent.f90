!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModCurrent

  use BATL_lib, ONLY: &
       test_start, test_stop
  use ModAdvance,        ONLY: iTypeUpdate, UpdateFast_
  use ModCoordTransform, ONLY: sph_to_xyz
  use CON_axes,          ONLY: transform_matrix
#ifdef _OPENACC
  use ModUtilities, ONLY: norm2
#endif

  implicit none

  private ! except

  public:: get_point_data
  public:: get_current
  public:: calc_field_aligned_current

contains
  !============================================================================
  include 'vector_functions.h'
  subroutine get_point_data_fast(Xyz_D, b_D, Current_D)
    !$acc routine seq

    ! Obtain B and J at location Xyz_D using ghost cells

    use ModAdvance, ONLY: State_VGB, Bx_, Bz_
    use BATL_lib, ONLY: find_grid_block, iProc

    real, intent(in):: Xyz_D(3)
    real, intent(out):: b_D(3), Current_D(3)

    integer:: iProcOut, iBlock, iCell_D(3)
    integer:: i, j, k, iLo, jLo, kLo, iHi, jHi, kHi
    real:: Dist_D(3), WeightX, WeightY, WeightZ, WeightXyz, CurrentIjk_D(3)
    !--------------------------------------------------------------------------
    call find_grid_block(Xyz_D, iProcOut, iBlock, iCell_D, Dist_D, &
         UseGhostCell=.true.)

    b_D = 0.0
    Current_D = 0.0

    if(iProc /= iProcOut) RETURN

    iLo = iCell_D(1); jLo = iCell_D(2); kLo = iCell_D(3)
    iHi = iLo + 1; jHi = jLo + 1; kHi = kLo + 1

    ! Simple trilinear interpolation. Could be optimized if needed.
    do k = kLo, kHi
       WeightZ = (k-kLo)*Dist_D(3) + (kHi-k)*(1-Dist_D(3))
       do j = jLo, jHi
          WeightY = (j-jLo)*Dist_D(2) + (jHi-j)*(1-Dist_D(2))
          do i = iLo, iHi
             WeightX = (i-iLo)*Dist_D(1) + (iHi-i)*(1-Dist_D(1))
             WeightXyz = WeightX*WeightY*WeightZ
             if(WeightXyz == 0.0) CYCLE

             b_D = b_D + WeightXyz * State_VGB(Bx_:Bz_,i,j,k,iBlock)

             call get_current(i, j, k, iBlock, CurrentIjk_D)
             Current_D = Current_D + WeightXyz*CurrentIjk_D
          end do
       end do
    end do

  end subroutine get_point_data_fast
  !============================================================================
  subroutine get_point_data(WeightOldState, XyzIn_D, iBlockMin, iBlockMax, &
       iVarMin, iVarMax, StateCurrent_V)
    !$acc routine seq

    ! Interpolate the (new and/or old) state vector from iVarMin to iVarMax and
    ! the current (if iVarMax=nVar+3) for input position
    ! XyzIn_D given in Cartesian coordinates. The interpolated state
    ! is second order accurate everywhere except where there is a
    ! resolution change in more than one direction for the cell centers
    ! surrounding the given position. In these exceptional cases the
    ! interpolated state is first order accurate. The interpolation algorithm
    ! is based on trilinear interpolation, but it is generalized for
    ! trapezoidal hexahedrons.

    use ModVarIndexes, ONLY: nVar
    use ModMain, ONLY: nI, nJ, nK, nIJK_D, Unused_B
    use ModAdvance, ONLY: State_VGB, StateOld_VGB
    use ModParallel, ONLY: DiLevel_EB
    use ModGeometry, ONLY: Coord111_DB
    use BATL_lib, ONLY: IsCartesianGrid, CellSize_DB, xyz_to_coord

    ! Weight of the old state in the interpolation
    real, intent(in)  :: WeightOldState

    ! Input position is in generalized coordinates
    real, intent(in)  :: XyzIn_D(3)

    ! Block index range (typically 1:nBlock or iBlock:iBlock)
    integer, intent(in) :: iBlockMin, iBlockMax

    ! Do we need to calculate currents
    integer, intent(in) :: iVarMin, iVarMax

    ! Weight and interpolated state at the input position
    real, intent(out) :: StateCurrent_V(0:iVarMax-iVarMin+1)

    ! Local variables

    ! Maximum index for state variable and number of state variables
    integer :: iStateMax, nState

    ! Position in generalized coordinates
    real :: Xyz_D(3)

    ! Cell size and buffer size for current block
    real,    dimension(3) :: Dxyz_D, DxyzInv_D, DxyzLo_D, DxyzHi_D

    ! Position of cell center to the lower index direction
    integer, dimension(3) :: IjkLo_D

    ! Position of point and current cell center
    real :: x, y, z, xI, yJ, zK

    ! Bilinear weights
    real    :: WeightX,WeightY,WeightZ,WeightXyz

    ! Dimension, cell, block index and MPI error code
    integer :: iDim,i,j,k,iLo,jLo,kLo,iHi,jHi,kHi,iBlock

    ! Current at the cell center
    real:: Current_D(3)

    ! Testing
    character(len=*), parameter:: NameSub = 'get_point_data'
    !--------------------------------------------------------------------------

    ! Calculate maximum index and the number of state variables
    iStateMax = min(iVarMax, nVar)
    nState    = iStateMax - iVarMin + 1

    ! Convert to generalized coordinates if necessary
    if(IsCartesianGrid)then
       Xyz_D = XyzIn_D
    else
       call xyz_to_coord(XyzIn_D, Xyz_D)
    end if

    ! Set state and weight to zero, so MPI_reduce will add it up right
    StateCurrent_V = 0.0

    ! Loop through all blocks
    BLOCK: do iBlock = iBlockMin, iBlockMax
       if(Unused_B(iBlock)) CYCLE

       ! Put cell size of current block into an array
       Dxyz_D = CellSize_DB(:,iBlock)

       ! Set buffer zone according to relative size of neighboring block
       do iDim = 1, 3
          ! Block at the lower index side
          select case(DiLevel_EB(2*iDim-1,iBlock))
          case(1)
             DxyzLo_D(iDim) = 1.5*Dxyz_D(iDim)
          case(-1)
             DxyzLo_D(iDim) = 0.75*Dxyz_D(iDim)
          case default
             DxyzLo_D(iDim) = Dxyz_D(iDim)
          end select
          ! Check if point is inside the buffer zone on the lower side
          if(Xyz_D(iDim) < Coord111_DB(iDim,iBlock) - DxyzLo_D(iDim)) &
               CYCLE BLOCK

          ! Block at the upper index side
          select case(DiLevel_EB(2*iDim,iBlock))
          case(1)
             DxyzHi_D(iDim) = 1.5*Dxyz_D(iDim)
          case(-1)
             DxyzHi_D(iDim) = 0.75*Dxyz_D(iDim)
          case default
             DxyzHi_D(iDim) = Dxyz_D(iDim)
          end select
          ! Check if point is inside the buffer zone on the upper side
          if(Xyz_D(iDim) > Coord111_DB(iDim,iBlock) + &
               (nIJK_D(iDim)-1)*Dxyz_D(iDim) + DxyzHi_D(iDim)) CYCLE BLOCK
       end do

       ! Find closest cell center indexes towards the lower index direction
       IjkLo_D = floor((Xyz_D - Coord111_DB(:,iBlock))/Dxyz_D)+1

       ! Set the size of the box for bilinear interpolation

       ! At resolution change the box size is the sum
       ! average of the cell size of the neighboring blocks

       ! Also make sure that IjkLo_D is not out of bounds
       do iDim = 1,3
          if(IjkLo_D(iDim) < 1)then
             IjkLo_D(iDim)   = 0
             DxyzInv_D(iDim) = 1/DxyzLo_D(iDim)
          elseif(IjkLo_D(iDim) >= nIJK_D(iDim))then
             IjkLo_D(iDim)   = nIJK_D(iDim)
             DxyzInv_D(iDim) = 1/DxyzHi_D(iDim)
          else
             DxyzInv_D(iDim) = 1/Dxyz_D(iDim)
          end if
       end do

       ! Set the index range for the physical cells
       iLo = max(IjkLo_D(1),1)
       jLo = max(IjkLo_D(2),1)
       kLo = max(IjkLo_D(3),1)
       iHi = min(IjkLo_D(1)+1,nI)
       jHi = min(IjkLo_D(2)+1,nJ)
       kHi = min(IjkLo_D(3)+1,nK)

       ! Put the point position into scalars
       x = Xyz_D(1); y = Xyz_D(2); z = Xyz_D(3)

       ! Loop through the physical cells to add up their contribution
       do k = kLo, kHi
          zK = Coord111_DB(3,iBlock) + (k-1)*Dxyz_D(3)
          WeightZ = 1 - DxyzInv_D(3)*abs(z-zK)
          do j = jLo, jHi
             yJ = Coord111_DB(2,iBlock) + (j-1)*Dxyz_D(2)
             WeightY = 1 - DxyzInv_D(2)*abs(y-yJ)
             do i = iLo, iHi
                xI = Coord111_DB(1,iBlock) + (i-1)*Dxyz_D(1)
                WeightX = 1 - DxyzInv_D(1)*abs(x-xI)

                WeightXyz = WeightX*WeightY*WeightZ

                if(WeightXyz > 0.0)then
                   ! Add up the weight
                   StateCurrent_V(0) = StateCurrent_V(0) + WeightXyz

                   ! Add contibutions from the old state if required
                   if(WeightOldState > 0.0) &
                        StateCurrent_V(1:nState) = StateCurrent_V(1:nState) &
                        + WeightXyz*WeightOldState &
                        *StateOld_VGB(iVarMin:iStateMax,i,j,k,iBlock)

                   ! Add contibutions from the current state if required
                   if(WeightOldState < 1.0) &
                        StateCurrent_V(1:nState) = StateCurrent_V(1:nState) &
                        + WeightXyz*(1 - WeightOldState) &
                        *State_VGB(iVarMin:iStateMax,i,j,k,iBlock)

                   ! The current is always based on the new state
                   if(iVarMax == nVar + 3)then
                      call get_current(i, j, k, iBlock, Current_D)
                      StateCurrent_V(nState+1:nState+3) = &
                           StateCurrent_V(nState+1:nState+3) &
                           + WeightXyz*Current_D
                   end if

                end if
             end do
          end do
       end do
    end do BLOCK

  end subroutine get_point_data
  !============================================================================
  subroutine get_current(i, j, k, iBlock, Current_D, nOrderResChange, &
       DoIgnoreBody)
    !$acc routine seq

    ! Calculate the current in a cell of a block
    ! Avoid using ghost cells at resolution changes.
    ! Avoid using cells inside the body.
    ! If the optional argument nOrderReschange is present and equals 1,
    ! then use first order scheme at resolution changes,
    ! otherwise use second order scheme when possible.

    use ModAdvance,  ONLY: State_VGB, Bx_, By_, Bz_
    use ModGeometry, ONLY: Used_GB, IsNoBody_B, r_GB
    use BATL_lib, ONLY: IsCartesianGrid, IsRzGeometry, Xyz_DGB, CellSize_DB, &
         nI, nJ, nK, x_, y_, z_
    use ModParallel, ONLY: DiLevel_EB
    use ModCoordTransform, ONLY: inverse_matrix
#ifndef _OPENACC
    use ModB0, ONLY: UseCurlB0, rCurrentFreeB0, set_b0_source, CurlB0_DC
    use omp_lib
#endif

    integer, intent(in) :: i, j, k, iBlock
    real,    intent(out):: Current_D(3)

    integer, optional, intent(in):: nOrderResChange

    logical, optional, intent(in):: DoIgnoreBody

    logical:: UseFirstOrder
    integer:: iL, iR, jL, jR, kL, kR
    real   :: Ax, Bx, Cx, Ay, By, Cy, Az, Bz, Cz
    real   :: InvDx2, InvDy2, InvDz2

    real :: DxyzDcoord_DD(3,3), DcoordDxyz_DD(3,3), DbDcoord_DD(3,3)

    integer:: iBlockLast = -1
    !$omp threadprivate( iBlockLast )

    ! Exclude body cells
    character(len=*), parameter:: NameSub = 'get_current'
    !--------------------------------------------------------------------------
    if(.not.Used_GB(i,j,k,iBlock) .and. .not.present(DoIgnoreBody))then
       Current_D = 0.0
       RETURN
    endif

    UseFirstOrder = .false.
    if(present(nOrderReschange)) UseFirstOrder = nOrderResChange == 1

    InvDx2 = 0.5/CellSize_DB(x_,iBlock)
    InvDy2 = 0.5/CellSize_DB(y_,iBlock)
    InvDz2 = 0.5/CellSize_DB(z_,iBlock)

    ! Central difference
    iR = i+1; iL = i-1;
    Ax = -InvDx2; Bx = 0.0; Cx = +InvDx2
    ! Avoid the ghost cells at resolution changes by using
    ! second-order one-sided difference
    if(i==1 .and. abs(DiLevel_EB(1,iBlock))==1)then
       if(UseFirstOrder)then
          iL = i; Ax = -2*InvDx2; Cx = 2*InvDx2
       else
          iL = i+1; iR = i+2; Ax = 4*InvDx2; Bx =-3*InvDx2; Cx =-InvDx2
       end if
    elseif(i==nI .and. abs(DiLevel_EB(2,iBlock))==1)then
       if(UseFirstOrder)then
          iR = i; Ax = -2*InvDx2; Cx = 2*InvDx2
       else
          iL = i-1; iR = i-2; Ax =-4*InvDx2; Bx = 3*InvDx2; Cx = InvDx2
       end if
    end if

    ! For y direction
    if(nJ == 1)then
       ! 1D
       jR = j; jL = j
       Ay = 0.0; By = 0.0; Cy = 0.0
    else
       jR = j+1; jL = j-1;
       Ay = -InvDy2; By = 0.0; Cy = +InvDy2
       if(j==1 .and. abs(DiLevel_EB(3,iBlock))==1)then
          if(UseFirstOrder)then
             jL = j; Ay = -2*InvDy2; Cy = 2*InvDy2
          else
             jL = j+1; jR = j+2; Ay = 4*InvDy2; By = -3*InvDy2; Cy = -InvDy2
          end if
       elseif(j==nJ .and. abs(DiLevel_EB(4,iBlock))==1)then
          if(UseFirstOrder)then
             jR = j; Ay = -2*InvDy2; Cy = 2*InvDy2
          else
             jL = j-1; jR = j-2; Ay = -4*InvDy2; By = 3*InvDy2; Cy = InvDy2
          end if
       end if
    end if

    ! For z direction
    if(nK == 1)then
       ! 1D or 2D
       kR = k; kL = k
       Az = 0.0; Bz = 0.0; Cz = 0.0
    else
       kR = k+1; kL = k-1
       Az = -InvDz2; Bz = 0.0; Cz = +InvDz2
       if(k==1 .and. abs(DiLevel_EB(5,iBlock))==1)then
          if(UseFirstOrder)then
             kL = k; Az = -2*InvDz2; Cz = 2*InvDz2
          else
             kL = k+1; kR = k+2; Az = 4*InvDz2; Bz =-3*InvDz2; Cz =-InvDz2
          end if
       elseif(k==nK .and. abs(DiLevel_EB(6,iBlock))==1)then
          if(UseFirstOrder)then
             kR = k; Az = -2*InvDz2; Cz = 2*InvDz2
          else
             kL = k-1; kR = k-2; Az = -4*InvDz2; Bz = 3*InvDz2; Cz = InvDz2
          end if
       end if
    end if

    ! Use first-order one-sided difference near the body if needed.
    ! If even first-order fails, then set the current to zero and exit.
    if(.not.IsNoBody_B(iBlock) .and. .not.present(DoIgnoreBody))then
       if(.not.Used_GB(iL,j,k,iBlock).and..not.Used_GB(iR,j,k,iBlock))then
          Current_D = 0.0
          RETURN
       elseif(.not.Used_GB(iL,j,k,iBlock))then
          Ax = 0.0
          if(iR==i+2)then
             Bx =-InvDx2; Cx = InvDx2
          elseif(iR==i-2)then
             Bx = InvDx2; Cx =-InvDx2
          else ! iR==i+1
             Bx =-2.0*InvDx2; Cx = 2.0*InvDx2
          end if
       elseif(.not.Used_GB(iR,j,k,iBlock))then
          Cx = 0.0
          if(iL==i+1)then
             Ax = 2.0*InvDx2; Bx =-2.0*InvDx2
          else ! iL==i-1
             Ax =-2.0*InvDx2; Bx = 2.0*InvDx2
          end if
       end if

       if(nJ > 1)then
          ! 2D or 3D
          if(  .not.Used_GB(i,jL,k,iBlock) .and. &
               .not.Used_GB(i,jR,k,iBlock))then
             Current_D = 0.0
             RETURN
          elseif(.not.Used_GB(i,jL,k,iBlock))then
             Ay = 0.0
             if(jR==j+2)then
                By =-InvDy2; Cy = InvDy2
             elseif(jR==j-2)then
                By = InvDy2; Cy =-InvDy2
             else ! jR==j+1
                By =-2.0*InvDy2; Cy = 2.0*InvDy2
             end if
          elseif(.not.Used_GB(i,jR,k,iBlock))then
             Cy = 0.0
             if(jL==j+1)then
                Ay = 2.0*InvDy2; By =-2.0*InvDy2
             else ! jL==j-1
                Ay =-2.0*InvDy2; By = 2.0*InvDy2
             end if
          end if
       end if

       if(nK > 1)then
          ! 3D
          if(  .not.Used_GB(i,j,kL,iBlock) .and. &
               .not.Used_GB(i,j,kR,iBlock))then
             Current_D = 0.0
             RETURN
          elseif(.not.Used_GB(i,j,kL,iBlock))then
             Az = 0.0
             if(kR==k+2)then
                Bz =-InvDz2; Cz = InvDz2
             elseif(kR==k-2)then
                Bz = InvDz2; Cz =-InvDz2
             else ! kR==k+1
                Bz =-2.0*InvDz2; Cz = 2.0*InvDz2
             end if
          elseif(.not.Used_GB(i,j,kR,iBlock))then
             Cz = 0.0
             if(kL==k+1)then
                Az = 2.0*InvDz2; Bz =-2.0*InvDz2
             else ! kL==k-1
                Az =-2.0*InvDz2; Bz = 2.0*InvDz2
             end if
          end if
       end if
    end if

    if(IsCartesianGrid)then
       Current_D(x_) = &
            + Ay*State_VGB(Bz_,i,jL,k ,iBlock) &
            + By*State_VGB(Bz_,i,j ,k ,iBlock) &
            + Cy*State_VGB(Bz_,i,jR,k ,iBlock) &
            - Az*State_VGB(By_,i,j ,kL,iBlock) &
            - Bz*State_VGB(By_,i,j ,k ,iBlock) &
            - Cz*State_VGB(By_,i,j ,kR,iBlock)

       Current_D(y_) = &
            + Az*State_VGB(Bx_,i ,j,kL,iBlock) &
            + Bz*State_VGB(Bx_,i ,j,k ,iBlock) &
            + Cz*State_VGB(Bx_,i ,j,kR,iBlock) &
            - Ax*State_VGB(Bz_,iL,j,k ,iBlock) &
            - Bx*State_VGB(Bz_,i ,j,k ,iBlock) &
            - Cx*State_VGB(Bz_,iR,j,k ,iBlock)

       Current_D(z_) = &
            + Ax*State_VGB(By_,iL,j ,k,iBlock) &
            + Bx*State_VGB(By_,i ,j ,k,iBlock) &
            + Cx*State_VGB(By_,iR,j ,k,iBlock) &
            - Ay*State_VGB(Bx_,i ,jL,k,iBlock) &
            - By*State_VGB(Bx_,i ,j ,k,iBlock) &
            - Cy*State_VGB(Bx_,i ,jR,k,iBlock)
#ifndef _OPENACC
       ! Correct current for rz-geometry: Jz = Jz + Bphi/radius
       if(IsRzGeometry) Current_D(x_) = Current_D(x_) &
            + State_VGB(Bz_,i,j,k,iBlock)/Xyz_DGB(y_,i,j,k,iBlock)
#endif
    else
       ! Get current in generalized coordinates
       DxyzDcoord_DD(:,1) = InvDx2 &
            *(Xyz_DGB(:,i+1,j,k,iBlock) - Xyz_DGB(:,i-1,j,k,iBlock))

       DxyzDcoord_DD(:,2) = InvDy2 &
            *(Xyz_DGB(:,i,j+1,k,iBlock) - Xyz_DGB(:,i,j-1,k,iBlock))

       if(nK > 1)then
          DxyzDcoord_DD(:,3) = InvDz2 &
               *(Xyz_DGB(:,i,j,k+1,iBlock) - Xyz_DGB(:,i,j,k-1,iBlock))
       else
          DxyzDcoord_DD(:,3) = [ 0., 0., 1. ]
       end if

       DcoordDxyz_DD = inverse_matrix(DxyzDcoord_DD, DoIgnoreSingular=.true.)

       ! Calculate the partial derivatives dB/dGencoord
       DbDcoord_DD(:,1) = &
            + Ax*State_VGB(Bx_:Bz_,iL,j,k,iBlock) &
            + Bx*State_VGB(Bx_:Bz_,i ,j,k,iBlock) &
            + Cx*State_VGB(Bx_:Bz_,iR,j,k,iBlock)

       DbDcoord_DD(:,2) = &
            + Ay*State_VGB(Bx_:Bz_,i,jL,k,iBlock) &
            + By*State_VGB(Bx_:Bz_,i,j ,k,iBlock) &
            + Cy*State_VGB(Bx_:Bz_,i,jR,k,iBlock)

       if(nK > 1)then
          DbDcoord_DD(:,3) = &
               + Az*State_VGB(Bx_:Bz_,i,j,kL,iBlock) &
               + Bz*State_VGB(Bx_:Bz_,i,j,k ,iBlock) &
               + Cz*State_VGB(Bx_:Bz_,i,j,kR,iBlock)
       else
          DbDcoord_DD(:,3) = 0.0
       end if

       ! Jx = Dbz/Dy - Dby/Dz = Dbz/Dcoord.Dcoord/Dy - DBy/Dcoord.Dccord/dz
       Current_D(x_) = &
            + sum(DbDcoord_DD(z_,:)*DcoordDxyz_DD(:,y_)) &
            - sum(DbDcoord_DD(y_,:)*DcoordDxyz_DD(:,z_))

       ! Jy = Dbx/Dz - Dbz/Dx
       Current_D(y_) = &
            + sum(DbDcoord_DD(x_,:)*DcoordDxyz_DD(:,z_)) &
            - sum(DbDcoord_DD(z_,:)*DcoordDxyz_DD(:,x_))

       ! Jz = Dby/Dx - Dbx/Dy
       Current_D(z_) = &
            + sum(DbDcoord_DD(y_,:)*DcoordDxyz_DD(:,x_)) &
            - sum(DbDcoord_DD(x_,:)*DcoordDxyz_DD(:,y_))
    end if

#ifndef _OPENACC
    ! Add curl B0 if necessary
    if(UseCurlB0)then
       ! Curl B0 is zero inside rCurrentFreeB0
       if(r_GB(i,j,k,iBlock) < rCurrentFreeB0) RETURN
       ! Curl B0 is only calculated for the physical cells
       if(i<1 .or. i>nI .or. j<1 .or. j>nJ .or. k<1 .or. k>nK)RETURN
       ! Optimize for multiple calls for the same block
       if(iBlock /= iBlockLast)call set_b0_source(iBlock)
       iBlockLast = iBlock
       ! Add curl(B0) to the current
       Current_D = Current_D + CurlB0_DC(:,i,j,k)
    end if
#endif

  end subroutine get_current
  !============================================================================
  subroutine calc_field_aligned_current(nTheta, nPhi, rIn, Fac_II, &
       Br_II, Bt_DII, b_DII, &
       Theta_I, Phi_I, TypeCoordFacGrid, IsRadial, IsRadialAbs, FacMin)

    use ModVarIndexes,     ONLY: Bx_, Bz_, nVar
    use ModMain,           ONLY: tSimulation, TypeCoordSystem, nBlock
    use ModPhysics,        ONLY: rCurrents, UnitB_, Si2No_V
#ifdef _OPENACC
    use ModPhysics,        ONLY: set_dipole
    use CON_planet_field,  ONLY: map_planet_field_fast
    use ModB0,             ONLY: get_b0_dipole
#else
    use CON_planet_field,  ONLY: map_planet_field, get_planet_field
#endif
    use ModNumConst,       ONLY: cTwoPi, cPi
    use ModMpi
    use BATL_lib,          ONLY: iProc, nProc, iComm

    ! Map the 2D spherical grid points from the rIn radius to rCurrents.
    ! The grid is in the TypeCoordFacGrid system (default is SMG).
    ! Calculate the field aligned currents there, use the ratio of the
    ! magnetic field strength. Calculate radial component if requested.
    ! The FAC is saved into Fac_II.
    ! The radial field into the optional Br_II.
    ! The tangential field (r x B/r) into Bt_DII in the FAC coords.
    ! Calculate the latitude boundary that still maps to rCurrents
    ! if requested.

    ! Size of spherical grid at rIn
    integer, intent(in) :: nTheta, nPhi

    ! Radius of spherical grid where FAC is needed
    real, intent(in) :: rIn

    ! Field aligned current at rIn
    real, intent(out):: Fac_II(nTheta,nPhi)
    !$acc declare create(Fac_II)

    ! Radial component of magnetic field at rIn
    real, intent(out), optional:: Br_II(nTheta,nPhi)
    !$acc declare create(Br_II)

    ! Tangential component (r x B/r) of field at rIn in FAC coordinates
    real, intent(out), optional:: Bt_DII(3,nTheta,nPhi)
    !$acc declare create(Bt_DII)

    ! Magnetic field at rIn in FAC coordinates
    real, intent(out), optional:: b_DII(3,nTheta,nPhi)
    !$acc declare create(b_DII)

    ! Coordinate arrays allow non-uniform grid
    real, intent(in), optional:: Theta_I(nTheta)
    real, intent(in), optional:: Phi_I(nPhi)
    !$acc declare create(Theta_I, Phi_I)

    ! Coordinate system of the Theta-Phi grid and Brin
    character(len=3), intent(in), optional:: TypeCoordFacGrid

    ! If present then calculate the radial component of the FAC with
    ! or with out sign of Br
    logical, intent(in), optional:: IsRadial
    logical, intent(in), optional:: IsRadialAbs

    ! If present, zero out Fac < FacMin (before taking radial part)
    real, intent(in), optional:: FacMin

    ! Local variables
    character(len=3):: TypeCoordFac

    ! Interpolation weight, interpolated agnetic field and current
    real, allocatable :: bCurrent_VII(:,:,:)
    !$acc declare create(bCurrent_VII)

    integer :: i, j, iHemisphere, iError
    real    :: Phi, Theta, Xyz_D(3),XyzIn_D(3), rUnit_D(3)
    real    :: b_D(3), bRcurrents, Fac, Current_D(3), bUnit_D(3), B0_D(3)
    real    :: bIn_D(3), bIn, Br
    real    :: GmFac_DD(3,3)
    !$acc declare create(GmFac_DD)
    real    :: State_V(Bx_-1:nVar+3)
    real    :: dPhi, dTheta
    logical :: DoMap

    logical :: UseGsm
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_field_aligned_current'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    TypeCoordFac = 'SMG'
    if(present(TypeCoordFacGrid)) TypeCoordFac = TypeCoordFacGrid
    UseGsm = TypeCoordFac == 'GSM'

    if(.not.allocated(bCurrent_VII)) allocate(bCurrent_VII(0:6,nTheta,nPhi))

    bCurrent_VII = 0.0

    Fac_II = 0.0

    GmFac_DD = transform_matrix(tSimulation, TypeCoordFac, TypeCoordSystem)
    !$acc update device(GmFac_DD)

    if (abs(rIn - rCurrents) < 1.0e-3)then
       DoMap = .false.
    else
       DoMap = .true.
    end if

    dPhi = cTwoPi/nPhi
    dTheta = cPi /(nTheta-1)

#ifdef _OPENACC
    call set_dipole

    if(present(Phi_I))then
       !$acc update device(Phi_I)
    end if

    if(present(Theta_I))then
       !$acc update device(Theta_I)
    end if
#endif

    !$acc parallel loop vector collapse(2) &
    !$acc private(XyzIn_D, Xyz_D, B0_D, b_D, Current_D, State_V)
    do j = 1, nPhi; do i = 1, nTheta

       if(present(Phi_I))then
          Phi = Phi_I(j)
       else
          Phi = (j-1)*dPhi
       end if

       if(present(Theta_I))then
          Theta = Theta_I(i)
       else
          Theta = (i-1)*dTheta
       end if

       call sph_to_xyz(rIn, Theta, Phi, XyzIn_D)

       if (DoMap)then
#ifdef _OPENACC
          call map_planet_field_fast(XyzIn_D, rCurrents, Xyz_D, &
               iHemisphere, UseGsmIn=UseGsm)
#else
          call map_planet_field(tSimulation, XyzIn_D, &
               TypeCoordFac//' NORM', rCurrents, Xyz_D, iHemisphere)
#endif

          if(iHemisphere == 0) then
             ! This point does not map
             ! Assign weight 1, magnetic field of 1,0,0 and current 0,0,0
             bCurrent_VII(:,i,j) = &
                  [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0]
             CYCLE
          end if
       else
          Xyz_D = XyzIn_D
       end if

       ! Convert to GM coordinates
       Xyz_D = matmul3_left(GmFac_DD, Xyz_D)
#ifdef _OPENACC
       call get_b0_dipole(Xyz_D, B0_D)
#else
       ! Get the B0 field at the mapped position
       call get_planet_field(tSimulation, Xyz_D, &
            TypeCoordSystem//' NORM', B0_D)
       B0_D = B0_D*Si2No_V(UnitB_)
#endif

       ! Extract currents and magnetic field for this position

!!! DEBUG
!       if(iTypeUpdate >= UpdateFast_)then
!          call get_point_data_fast(Xyz_D, b_D, Current_D)
!          bCurrent_VII(0,  i,j) = 1.0          ! Weight
!          bCurrent_VII(1:3,i,j) = b_D + B0_D   ! B1 and B0
!          bCurrent_VII(4:6,i,j) = Current_D    ! Currents
!       else
          call get_point_data(0.0, Xyz_D, 1, nBlock, Bx_, nVar+3, State_V)
          bCurrent_VII(0,  i,j) = State_V(Bx_-1)         ! Weight
          bCurrent_VII(1:3,i,j) = State_V(Bx_:Bz_) + &   ! B1 + Weight*B0
               State_V(Bx_-1)*B0_D
          bCurrent_VII(4:6,i,j) = State_V(nVar+1:nVar+3) ! Currents
!       end if

    end do; end do

    if(nProc > 1)then
       !$acc host_data use_device(bCurrent_VII)
       call MPI_reduce_real_array(bCurrent_VII, &
            size(bCurrent_VII), MPI_SUM, 0, iComm, iError)
       !$acc end host_data
    end if

    ! Map the field aligned current to rIn sphere
    if(iProc == 0)then
       !$acc parallel loop vector collapse(2) &
       !$acc private(b_D, Current_D, XyzIn_D, Xyz_D, bIn_D, rUnit_D, bUnit_D)
       do j = 1, nPhi; do i = 1, nTheta

          if(present(Phi_I))then
             Phi = Phi_I(j)
          else
             Phi = (j-1)*dPhi
          end if

          if(present(Theta_I))then
             Theta = Theta_I(i)
          else
             Theta = (i-1)*dTheta
          end if

          ! Divide MHD values by the total weight if it exceeds 1.0
          if(bCurrent_VII(0,i,j) > 1.0) bCurrent_VII(:,i,j) = &
               bCurrent_VII(:,i,j)/bCurrent_VII(0,i,j)

          ! Extract magnetic field and current
          b_D = bCurrent_VII(1:3,i,j)
          Current_D = bCurrent_VII(4:6,i,j)

          ! The strength of the field
          bRcurrents = norm2(b_D)

          ! Convert b_D into a unit vector
          bUnit_D = b_D / bRcurrents

          ! get the field aligned current
          Fac = sum(bUnit_D*Current_D)

          ! Get Cartesian coordinates
          call sph_to_xyz(rIn, Theta, Phi, Xyz_D)

          ! Convert to GM coordinates
          XyzIn_D = matmul3_left(GmFac_DD, Xyz_D)

          if(DoMap)then
             ! Calculate magnetic field strength at the rIn grid point

#ifdef _OPENACC
             call get_b0_dipole(XyzIn_D, bIn_D)
#else
             call get_planet_field(tSimulation, XyzIn_D, &
                  TypeCoordSystem//' NORM', bIn_D)

             ! Convert to normalized units
             bIn_D = bIn_D*Si2No_V(UnitB_)
#endif
             bIn   = norm2(bIn_D)

             ! Multiply by the ratio of the magnetic field strengths
             Fac = bIn / bRcurrents * Fac
          else
             bIn_D = b_D
             bIn   = norm2(bIn_D)
          end if

          ! Zero out small Fac if requested
          if(present(FacMin))then
             if(abs(Fac) < FacMin) Fac = 0.0
          end if

          rUnit_D = XyzIn_D / rIn
          Br      = sum(bIn_D*rUnit_D)

          ! Get radial component of FAC: FAC_r = FAC*Br/B
          if(present(IsRadial)) Fac = Fac * Br/bIn

          if(present(IsRadialAbs)) Fac = Fac * abs(Br) / bIn

          ! store the (radial component of the) field alinged current
          Fac_II(i,j) = Fac

          ! store the B field in the FAC coordinates
          if(present(b_DII)) b_DII(:,i,j) = matmul3_right(bIn_D, GmFac_DD)

          ! store radial component of B field
          if(present(Br_II)) Br_II(i,j) = Br

          ! store tangential B field vector in FAC coordinates
          if(present(Bt_DII))then
             b_D = cross_prod(rUnit_D, bIn_D)
             Bt_DII(:,i,j) = matmul3_right(b_D, GmFac_DD )
          end if
       end do; end do
    end if
    deallocate(bCurrent_VII)

#ifdef _OPENACC
    !$acc update host(Fac_II)
    if(present(Br_II))then
       !$acc update host(Br_II)
    end if

    if(present(Bt_DII))then
       !$acc update host(Bt_DII)
    end if

    if(present(b_DII))then
       !$acc update host(B_dII)
    end if
#endif

    call test_stop(NameSub, DoTest)
  end subroutine calc_field_aligned_current
  !============================================================================
end module ModCurrent
!==============================================================================
