!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
!^CMP FILE UA

module GM_couple_ua

  implicit none

  private ! except
  public:: GM_get_for_ua
  public:: GM_put_from_ua

contains
  !============================================================================

  subroutine GM_update_neutral_dens(IsNew, nDenNuSpecies_srcInterface_CBI)

    ! Get magnetic field data from GM to UA

    ! use ModProcMH,  ONLY: iProc
    use BATL_lib, ONLY: iProc
    use ModPhysics, ONLY: Si2No_V, No2Si_V, iUnitCons_V, UnitX_
    use ModAdvance, ONLY: State_VGB, nVar, Bx_, Bz_
    use ModVarIndexes, ONLY: nVar
    use ModB0,      ONLY: UseB0, get_b0
    use BATL_lib,   ONLY: MaxDim, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, find_grid_block
    use ModInterpolate, ONLY: trilinear

    logical,          intent(in):: IsNew   ! true for new point array

    real, intent(out):: nDenNuSpecies_srcInterface_CBI ! Data array

    character(len=*), parameter:: NameSub = 'GM_update_neutral_dens'
    !--------------------------------------------------------------------------

    ! We should have second order accurate magnetic field in the ghost cells
    write(*,*) "Calling GM_update_neutral_dens..."

  end subroutine GM_update_neutral_dens
  !============================================================================
  subroutine GM_get_for_ua(IsNew, NameVar, nVarIn, nDimIn, nPoint, Xyz_DI, &
       Data_VI)

    ! Get magnetic field data from GM to UA

    ! use ModProcMH,  ONLY: iProc
    use BATL_lib, ONLY: iProc
    use ModPhysics, ONLY: Si2No_V, No2Si_V, iUnitCons_V, UnitX_
    use ModAdvance, ONLY: State_VGB, nVar, Bx_, Bz_
    use ModVarIndexes, ONLY: nVar
    use ModB0,      ONLY: UseB0, get_b0
    use BATL_lib,   ONLY: MaxDim, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, find_grid_block
    use ModInterpolate, ONLY: trilinear

    logical,          intent(in):: IsNew   ! true for new point array
    character(len=*), intent(in):: NameVar ! List of variables
    integer,          intent(in):: nVarIn  ! Number of variables in Data_VI
    integer,          intent(in):: nDimIn  ! Dimensionality of positions
    integer,          intent(in):: nPoint  ! Number of points in Xyz_DI

    real, intent(in) :: Xyz_DI(nDimIn,nPoint)  ! Position vectors / Xyz_DI is the same as Pos_DI!!!
    real, intent(out):: Data_VI(nVarIn,nPoint) ! Data array

    real:: Xyz_D(MaxDim), b_D(MaxDim)
    real:: Dist_D(MaxDim), State_V(nVar)
    integer:: iCell_D(MaxDim)

    integer:: iPoint, iBlock, iProcFound

    ! We should have second order accurate magnetic field in the ghost cells
    character(len=*), parameter:: NameSub = 'GM_get_for_ua'
    !--------------------------------------------------------------------------
    write(*,*) "Calling GM_get_for_ua...", nPoint, nVarIn

    ! From gm_find_points:
    ! do iPoint = 1, nPoint
    !   Xyz_D(1:nDimIn) = Xyz_DI(:,iPoint)*Si2No_V(UnitX_)
    !   call find_grid_block(Xyz_D, iProc_I(iPoint), iBlock)
       ! write(*,*) 'iProc_I: ',Xyz_D,iProc_I(ipoint)
    ! end do

    do iPoint = 1, nPoint

       Xyz_D = Xyz_DI(:,iPoint)*Si2No_V(UnitX_)
       call find_grid_block(Xyz_D, iProcFound, iBlock, iCell_D, Dist_D, &
            UseGhostCell = .true.)
            ! write(*,*) "Proc and Block: ", iProcFound, iBlock

       if(iProcFound /= iProc)then
          write(*,*)NameSub,' ERROR: Xyz_D, iProcFound=', Xyz_D, iProcFound
          call stop_mpi(NameSub//' could not find position on this proc')
       end if
       write(*,*) "For trilinear1: ",MinI,MaxI,MinJ,MaxJ,MinK,MaxK
       write(*,*) "For trilinear2: ",iCell_D
       write(*,*) "For trilinear3: ",Dist_D
       write(*,*) "For trilinear4: ",size(State_VGB(:,:,:,:,iBlock))
       State_V = trilinear(State_VGB(:,:,:,:,iBlock), &
            nVar, MinI, MaxI, MinJ, MaxJ, MinK, MaxK, Xyz_D, &
            iCell_D = iCell_D, Dist_D = Dist_D)

       Data_VI(:,iPoint) = State_V*No2Si_V(iUnitCons_V)

    end do
    write(*,*) 'GM data in GM_get_for_ua: ',maxval(Data_VI)
    write(*,*) 'Finished GM_get_for_ua...'
  end subroutine GM_get_for_ua
  !============================================================================
  subroutine GM_get_ua_region(numcall,NameVar, nVar, nPoint, Pos_DI, Data_VI, iPoint_I)
    ! This function will be called 3 times :
    !
    ! 1) Count outer boundary ghost cells to be obtained from GM
    !
    ! 2) Return the Xyz_DGB coordinates of these cells
    !
    ! 3) Recieve Data_VI from GM and put them into State_VGB.
    !    The indexing array iPoint_I is needed to maintain the same order as
    !    the original position array Pos_DI was given in 2)

    use BATL_lib,     ONLY: Xyz_DGB, nBlock, Unused_B, &
         MinI, MaxI, MinJ, MaxJ, MinK, MaxK, &
         CoordMin_D, CoordMax_D, CoordMin_DB, CellSize_DB
    use ModGeometry,  ONLY: far_field_bcs_blk, r_BLK
    use ModPhysics,   ONLY: No2Si_V, UnitX_, Si2No_V, iUnitCons_V
    use ModMain,      ONLY: UseB0
    use ModB0,        ONLY: B0_DGB
    use ModAdvance,   ONLY: State_VGB, Bx_, Bz_
    use ModMultiFluid, ONLY: nIonFluid
    use CON_coupler,  ONLY: Grid_C, GM_, nVarBuffer, iVarTarget_V
!!!    use ModUaCoupling, ONLY: nDenNuSpecies_fromUA
    ! use CON_couple_gm_ua, ONLY: State_VGB_UA
    ! use ModUser,      ONLY: nDenNuSpecies_CBI
    character(len=*), intent(inout):: NameVar ! List of variables
    integer,          intent(inout):: nVar    ! Number of variables in Data_VI
    integer,          intent(inout):: nPoint  ! Number of points in Pos_DI
    integer,          intent(in)::numcall
    real, intent(inout), allocatable, optional :: Pos_DI(:,:)  ! Positions

    real,    intent(in), optional:: Data_VI(:,:)    ! Recv data array
    integer, intent(in), optional:: iPoint_I(nPoint)! Order of data

    ! real :: nDenNuSpecies_srcInterface_CBI

    logical :: DoCountOnly
    integer :: i, j, k, iBlock, iPoint, iVarBuffer, iVar
    integer :: iLons, iLats, iAlts
    real    :: Coord_D(3), Xyz_D(3)
    real    :: rMinGm, tempalt

    character(len=*), parameter:: NameSub = 'GM_get_ua_region'
    !--------------------------------------------------------------------------
    write(*,*) "-->Starting GM_get_ua_region: ",numcall,nPoint,nVar

    DoCountOnly = nPoint < 1

    rMinGm = Grid_C(GM_)%Coord1_I(1) ! R min

    ! Find ghost cells in the GM domain
    iPoint = 0
    ! do iBlock = 1, nBlock
    !    do iLons = 1, nLons
    !      do iLats = 1, nLats
    !        do iAlts = 1, nAlts
    !           Coord_D = Xyz_gitm(:,iLons,iLats,iAlts,iBlock)
    !           ! write(*,*) 'Coord_D in UA_wrapper: ',Coord_D
    !           !         ! Exclude points below the UA-GM bottom boundary (100 km)
    !           if(Altitude_GB(iLons,iLats,iAlts,iBlock)/1.e3 < 100) CYCLE
    !           !
    !           !         ! Exclude points above the UA-GM bottom boundary (300 km)
    !           if(Altitude_GB(iLons,iLats,iAlts,iBlock)/1.e3 > 300) CYCLE
    !
    !           iPoint = iPoint + 1
    !           if(DoCountOnly) CYCLE
    !
    !        end do
    !      end do
    !    end do
    !  end do
    !  write(*,*) 'iPoint in UA_wrapper: ',iPoint
    ! do iBlock = 1, nBlock
    !   do iLons = 1, nLons
    !     do iLats = 1, nLats
    !       do iAlts = 1, nAlts
    !         Coord_D(1) = Altitude_GB(iLons,iLats,iAlts,iBlock)/1.e3*sin(Longitude(iLons,iBlock))*cos(Latitude(iLats,iBlock))
    !         Coord_D(2) = Altitude_GB(iLons,iLats,iAlts,iBlock)/1.e3*sin(Longitude(iLons,iBlock))*sin(Latitude(iLats,iBlock))
    !         Coord_D(3) = Altitude_GB(iLons,iLats,iAlts,iBlock)/1.e3*cos(Longitude(iLons,iBlock))
    !         ! write(*,*) 'Coord_D: ',Coord_D
    !
    !         ! Exclude points below the UA-GM bottom boundary (100 km)
    !         if(Altitude_GB(iLons,iLats,iAlts,iBlock)/1.e3 < 100) CYCLE
    !
    !         ! Exclude points above the UA-GM bottom boundary (300 km)
    !         if(Altitude_GB(iLons,iLats,iAlts,iBlock)/1.e3 > 300) CYCLE
    !
    !         ! Found a point to be set by UA
    !         iPoint = iPoint + 1
    !         if(DoCountOnly) CYCLE
    !         ! write(*,*) 'Writing POS_DI for: ',iPoint,nPoint
    !         if(present(Data_VI))then
    !                   ! Put Data_VI obtained from GM into State_VGB
    !                   ! write(*,*) "nVarBuffer: ", nVarBuffer
    !                   ! write(*,*) 'Data_VI: ',Data_VI
    !                   ! do iVarBuffer = 1, nVarBuffer
    !                  !    iVar = iVarTarget_V(iVarBuffer)
    !                !      State_VGB(iVar,i,j,k,iBlock) = &
    !              !             Data_VI(iVarBuffer,iPoint_I(iPoint))! &
    !            !               !*Si2No_V(iUnitCons_V(iVar))
    !          !            ! write(*,*) "Data: ", State_VGB(iVar,i,j,k,iBlock)
    !        !           end do
    !                   ! Set variables not defined by coupling
    !                   ! if(UseElectronPressure .and. &
    !                   !     .not. DoCoupleVar_V(ElectronPressure_)then
    !                   ! if(UseB0) State_VGB(Bx_:Bz_,i,j,k,iBlock) = &
    !                  !      State_VGB(Bx_:Bz_,i,j,k,iBlock) - B0_DGB(:,i,j,k,iBlock)
    !                else
    !                   Pos_DI(:,iPoint) = Xyz_gitm(:,iLons,iLats,iAlts,iBlock)! Xyz_DGB(:,i,j,k,iBlock)*No2Si_V(UnitX_)
    !          end if
    !       end do
    !     end do
    !   end do
    ! end do

    write(*,*) 'maxnpoints: ',nBlock,MaxK-MinK,MaxJ-MinJ,MaxI-MinI
    do iBlock = 1, nBlock
       do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, MaxI
          ! Set generalized coordinate
          Coord_D = &
               CoordMin_DB(:,iBlock) + ([i,j,k]-0.5)*CellSize_DB(:,iBlock)
          ! write(*,*) 'Cell Size: ',CellSize_DB(:,iBlock)

               ! In BATL_grid
               ! Xyz_DGB(:,i,j,k,iBlock) = CoordMin_DB(:,iBlock) + &
                !    ( (/i, j, k/) - 0.5 ) * CellSize_DB(:,iBlock)

          !     write(*,*) 'Coord_D: ',CoordMin_DB(:,iBlock)

          ! Exclude points that are inside the domain
          ! write(*,*) "Exclude points inside domain1...",(R_Mars+100e3)*Si2No_V(UnitX_),(R_Mars+300e3)*Si2No_V(UnitX_)
          ! write(*,*) "Exclude points inside domain2...",CoordMin_D,CoordMax_D

          ! if(all(Coord_D > CoordMin_D) .and. all(Coord_D < CoordMax_D)) CYCLE
          tempalt = sqrt(Coord_D(1)*Coord_D(1) + Coord_D(2)*Coord_D(2) + Coord_D(3)*Coord_D(3)) - 1
          ! write(*,*) 'tempalt: ',tempalt
          ! if(all(Coord_D > (R_Mars+300e3)*Si2No_V(UnitX_)) .and. all(Coord_D < (R_Mars+100e3)*Si2No_V(UnitX_))) CYCLE
          if (tempalt < (100e3)*Si2No_V(UnitX_)) CYCLE
          if (tempalt > (300e3)*Si2No_V(UnitX_)) CYCLE

          ! Exclude points below the GM bottom boundary
          ! if(r_BLK(i,j,k,iBlock) < rMinGm) CYCLE

          ! Found a point to be set by GM
          iPoint = iPoint + 1
          if(DoCountOnly) CYCLE

          if(present(Data_VI))then
             ! Put Data_VI obtained from GM into State_VGB
             ! write(*,*) "nVarBuffer: ", nVar ,Data_VI(1,1),iVarTarget_V(1)
             ! do iVar = 1, nVarBuffer

                ! iVar = iVarTarget_V(iVarBuffer)
                ! write(*,*) 'iVar: ',Data_VI(iVar,iPoint)
                ! write(*,*) 'iVarTarget_V: ',iVarTarget_V
                ! State_VGB_UA(iVar,i,j,k,iBlock) = &
                !     Data_VI(iVar,iPoint) !&
                     !*Si2No_V(iUnitCons_V(iVar))
                ! write(*,*) "Data: ", State_VGB(iVar,i,j,k,iBlock)
             ! end do
             ! do iVarBuffer = 1, nVarBuffer

              !  iVar = iVarTarget_V(iVarBuffer)
              !  write(*,*) 'iVar: ',iVar
              !  write(*,*) 'iVarTarget_V: ',iVarTarget_V
              ! write(*,*) 'Data in GM: ',Data_VI(:,iPoint), size(Data_VI(:,iPoint))!,size(nDenNuSpecies_fromUA(i,j,k,iBlock,1))
!!!              nDenNuSpecies_fromUA(i,j,k,iBlock,:) = Data_VI(:,iPoint)  ! Incompatible ranks 0 and 1 in assignment at (1)???
              ! nDenNuSpecies_srcInterface_CBI = Data_VI(:,iPoint)
              !  State_VGB_GM(iVar,i,j,k,iBlock) = &
              !       Data_VI(iVarBuffer,iPoint_I(iPoint)) &
              !       *Si2No_V(iUnitCons_V(iVar))
                ! write(*,*) "Data: ", State_VGB(iVar,i,j,k,iBlock)
             ! end do
             ! Set variables not defined by coupling
             ! if(UseElectronPressure .and. &
             !     .not. DoCoupleVar_V(ElectronPressure_)then
             ! if(UseB0) State_VGB(Bx_:Bz_,i,j,k,iBlock) = &
            !      State_VGB(Bx_:Bz_,i,j,k,iBlock) - B0_DGB(:,i,j,k,iBlock)
          else
             ! Provide position to GM
             ! write(*,*) 'Xyz_DGB: ',Xyz_DGB(:,i,j,k,iBlock)
             Pos_DI(:,iPoint) = Xyz_DGB(:,i,j,k,iBlock)!*No2Si_V(UnitX_) ! Coord_D
             ! write(*,*) 'MinMax Pos_DI: ',minval(Xyz_DGB(:,i,j,k,iBlock)),maxval(Xyz_DGB(:,i,j,k,iBlock))
             ! write(*,*) 'Pos_DI new: ',Pos_DI(:,iPoint)
          end if

       end do; end do; end do
    end do

    if(DoCountOnly) nPoint = iPoint

  end subroutine GM_get_ua_region
  !============================================================================
  subroutine GM_put_from_ua(NameVar, nVar, nPoint, Data_VI, iPoint_I, Pos_DI)

    use BATL_size, only: nI,nJ,nK,MaxBlock
!!!    use ModUACoupling

    character(len=*), intent(inout):: NameVar ! List of variables
    integer,          intent(inout):: nVar    ! Number of variables in Data_VI
    integer,          intent(inout):: nPoint  ! Number of points in Pos_DI
    integer :: i

    real,    intent(in), optional:: Data_VI(:,:)           ! Recv data array
    integer, intent(in), optional:: iPoint_I(nPoint)       ! Order of data
    real, intent(out), allocatable, optional:: Pos_DI(:,:) ! Position vectors

    character(len=*), parameter:: NameSub = 'GM_put_from_ua'
    !--------------------------------------------------------------------------
     write(*,*) "-->Starting GM_put_from_ua..."
    ! ! call CON_set_do_test(NameSub, DoTest, DoTestMe)
    ! integer :: MaxNuSpecies =

     if(.not. present(Data_VI))then
        ! nPoint = 0
        ! get nPoint
        write(*,*) 'Before get_ua_region: ',NameVar
        write(*,*) 'Before get_ua_region: ',nVar,nPoint
        write(*,*) 'Before get_ua_region: ',Pos_DI
        call GM_get_ua_region(1,NameVar, nVar, nPoint, Pos_DI)
        write(*,*) "After get_ua_region: ",NameVar
        write(*,*) 'After get_ua_region: ',nVar,nPoint
        write(*,*) 'After get_ua_region: ',Pos_DI

!!!        write(*,*) 'nDimGITM: ',nDimGITM    ! nDimGITM is zero, check why...
        if(allocated(Pos_DI)) deallocate(Pos_DI)
        allocate(Pos_DI(3,nPoint))          ! Instead of 3, it should be nDimGITM
        write(*,*) 'Pos_DI(1,1): ',Pos_DI(1,1)
    !
    !    ! get Pos_DI
        call GM_get_ua_region(2,NameVar, nVar, nPoint, Pos_DI)
        write(*,*) 'In GM_get_ua_region: ',maxval(Pos_DI),minval(abs(Pos_DI))
        write(*,*) 'Positions: ',Pos_DI(:,10)

        RETURN
     end if
    !
!!!    if(.not.allocated(nDenNuSpecies_fromUA)) then
!!!      call gm_init_ua_array(nI,nJ,nK,MaxBlock,MaxNuSpecies_coupler)
!!!      write(*,*) 'Allocating nDen3...',size(nDenNuSpecies_fromUA)
!!!    end if

    ! ! set State variables
!!!     write(*,*) 'Calling 3...',dummyvar1
     call GM_get_ua_region(3,NameVar, nVar, nPoint, Pos_DI, Data_VI, iPoint_I)
!!!     if(dummyvar1 == 0) then
!!!       open(unit=12,file='testoutput1.txt',action="write",status="replace")
!!!       do i=1,nPoint
!!!         write(12,*) Data_VI(:,i)
!!!       end do
!!!       close(unit=12)
!!!       dummyvar1 = 1
!!!     end if
    write(*,*) "Data_GMUA: ", maxval(Data_VI)
    write(*,*) 'nPoint at the end of GM_put_from_ua: ',nPoint

  end subroutine GM_put_from_ua
  !============================================================================

end module GM_couple_ua
!==============================================================================
