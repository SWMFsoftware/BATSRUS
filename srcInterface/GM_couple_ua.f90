!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module GM_couple_ua

  implicit none

  private ! except

  ! GM-UA coupling
  public:: GM_put_from_ua

contains
  !============================================================================
  subroutine GM_get_ua_region(NameVar, nVar, nPoint, Pos_DI, Data_VI, iPoint_I)

    ! import temperature, neutral densities and ionization rates

    ! This function will be called 3 times:
    !
    ! 1) Count grid cells to be overwritten by UA
    !
    ! 2) Return the Xyz_DGB coordinates of these cells
    !
    ! 3) Receive Data_VI from GM and put them into State_VGB.
    !    The indexing array iPoint_I is needed to maintain the same order as
    !    the original position array Pos_DI was given in 2)

    use BATL_lib, ONLY: Xyz_DGB, nBlock, Unused_B, nI, nJ, nK, MaxBlock, &
         test_start ! , iTest, jTest, kTest, iBlockTest
    use ModMain, ONLY: UaState_VCB
    use ModGeometry, ONLY: r_GB
    use ModPhysics, ONLY: No2Si_V, UnitX_, rBody

    character(len=*), intent(inout):: NameVar ! List of variables
    integer,          intent(inout):: nVar    ! Number of variables in Data_VI
    integer,          intent(inout):: nPoint  ! Number of points in Pos_DI
    real, intent(inout), allocatable, optional :: Pos_DI(:,:)  ! Positions

    real,    intent(in), optional:: Data_VI(:,:)    ! Recv data array
    integer, intent(in), optional:: iPoint_I(nPoint)! Order of data

    logical :: DoCountOnly
    integer :: i, j, k, iBlock, iPoint

    logical:: DoTest ! DoTestCell
    character(len=*), parameter:: NameSub = 'GM_get_ua_region'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(.not.allocated(UaState_VCB)) allocate(UaState_VCB(5,nI,nJ,nK,MaxBlock))

    DoCountOnly = nPoint < 1

    ! Find GM cells located in the UA domain
    iPoint = 0
    do iBlock = 1, nBlock
       if(Unused_B(iBlock)) CYCLE

       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          if(r_GB(i,j,k,iBlock) > 3.0*rBody) CYCLE ! rMaxUA originally

          ! Found a point of GM to be set by UA
          iPoint = iPoint + 1
          if(DoCountOnly) CYCLE

          ! DoTestCell = DoTest .and. iBlock == iBlockTest .and. &
          !     i == iTest .and. j == jTest .and. k == kTest
          ! if(DoTestCell) write(*,*) NameSub,': iPoint=', iPoint

          if(present(Data_VI))then
             ! Put Data_VI obtained from UA into source terms of GM
             ! Temperature, N_CO2, N_O, EUVIonRate_CO2->CO2+, EUVIonRate_O->O+
             UaState_VCB(:5,i,j,k,iBlock) = Data_VI(:5,iPoint_I(iPoint))
             ! if(DoTestCell) write(*,*) NameSub, ': UaState=', &
             !     UaState_VCB(:5,i,j,k,iBlock)
          else
             ! Provide GM position to UA
             Pos_DI(:,iPoint) = Xyz_DGB(:,i,j,k,iBlock)*No2Si_V(UnitX_)
             ! if(DoTestCell) write(*,*) NameSub, ': Pos_D=', Pos_DI(:,iPoint)
          end if

       end do; end do; end do
    end do

    if(DoCountOnly) nPoint = iPoint

  end subroutine GM_get_ua_region
  !============================================================================
  subroutine GM_put_from_ua(NameVar, nVar, nPoint, Data_VI, iPoint_I, Pos_DI)

    use BATL_size, ONLY: nDim
    use ModUtilities, ONLY: CON_set_do_test
    use ModMain, ONLY: IsNewUaState

    character(len=*), intent(inout):: NameVar ! List of variables
    integer,          intent(inout):: nVar    ! Number of variables in Data_VI
    integer,          intent(inout):: nPoint  ! Number of points in Pos_DI

    real,    intent(in), optional:: Data_VI(:,:)           ! Recv data array
    integer, intent(in), optional:: iPoint_I(nPoint)       ! Order of data
    real, intent(out), allocatable, optional:: Pos_DI(:,:) ! Position vectors

    logical:: DoTest, DoTestMe
    character(len=*), parameter:: NameSub = 'GM_put_from_ua'
    !--------------------------------------------------------------------------
    call CON_set_do_test(NameSub, DoTest, DoTestMe)

    if(.not. present(Data_VI))then
        nPoint = 0
        ! get nPoint
        call GM_get_ua_region(NameVar, nVar, nPoint, Pos_DI)

        if(allocated(Pos_DI)) deallocate(Pos_DI)
        allocate(Pos_DI(nDim,nPoint))

        if(DoTestMe)write(*,*) NameSub, ': nPoint, shape(Pos_DI)=', &
             nPoint, shape(Pos_DI)

        ! get Pos_DI
        call GM_get_ua_region(NameVar, nVar, nPoint, Pos_DI)

        RETURN
     end if

     if(DoTestMe)then
        write(*,*) NameSub,' nVar, nPoint =', nVar, nPoint
        write(*,*) NameSub,' shape(Data_VI)=', shape(Data_VI)
        write(*,*) NameSub,' call GM_get_ua_region'
     end if

     ! set variables needed to construct source terms
     IsNewUaState = .true.
     call GM_get_ua_region(NameVar, nVar, nPoint, Pos_DI, Data_VI, iPoint_I)

  end subroutine GM_put_from_ua
  !============================================================================
end module GM_couple_ua
!==============================================================================
