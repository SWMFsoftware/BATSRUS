!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModConstrainDivB

  use BATL_lib, ONLY: &
       test_start, test_stop, StringTest, iProc, iTest,jTest,kTest, iBlockTest

  ! A flux averaged constrained transport scheme for block AMR grid. See
  !
  ! G. Toth, 2000, Journal of Computational Physics, 161, 605-652
  !
  ! G. Toth and P. L. Roe, 2002, Journal of Computational Phys, 180, 736-750
  !
  ! Since we switched to BATL, it only works on uniform grid.

  use ModSize
  use ModIO,     ONLY: iUnitOut, write_prefix
  use ModUtilities, ONLY: i_gang

  implicit none
  SAVE

  private ! except

  public:: init_mod_ct
  public:: clean_mod_ct
  public:: constrain_b
  public:: constrain_ics
  public:: bcenter_to_bface
  public:: bface_to_bcenter
  public:: get_vxb
  public:: bound_vxb
  public:: bound_bface

  ! Initialize?
  logical, public :: DoInitConstrainB = .true.

  ! Face centered magnetic field components, Bxface is centered on face X etc.
  real, public, allocatable :: BxFace_GB(:,:,:,:)
  real, public, allocatable :: ByFace_GB(:,:,:,:)
  real, public, allocatable :: BzFace_GB(:,:,:,:)

  ! Local variables --------------

  ! Face centered normal magnetic field from the 4 finer neighbors
  ! on the two sides of the block (1=east/south/bot, 2=west/north/top)

  real, allocatable :: BxFaceFine_IIQSB(:,:,:,:,:)
  real, allocatable :: ByFaceFine_IIQSB(:,:,:,:,:)
  real, allocatable :: BzFaceFine_IIQSB(:,:,:,:,:)

  ! VxB stored at edges
  real, allocatable :: VxBX_GB(:,:,:,:)
  real, allocatable :: VxBY_GB(:,:,:,:)
  real, allocatable :: VxBZ_GB(:,:,:,:)

contains
  !============================================================================
  subroutine init_mod_ct

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_mod_ct'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(allocated(BxFace_GB)) RETURN

    allocate(BxFace_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
    allocate(ByFace_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
    allocate(BzFace_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
    allocate(BxFaceFine_IIQSB(nJ,nK,4,2,MaxBlock))
    allocate(ByFaceFine_IIQSB(nI,nK,4,2,MaxBlock))
    allocate(BzFaceFine_IIQSB(nI,nJ,4,2,MaxBlock))
    allocate(VxBX_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
    allocate(VxBY_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
    allocate(VxBZ_GB(MinI:MaxI,MinJ:MaxJ,MinK:MaxK,MaxBlock))
    if(iProc==0)then
       call write_prefix
       write(iUnitOut,'(a)') 'init_mod_ct allocated arrays'
    end if

    call test_stop(NameSub, DoTest)
  end subroutine init_mod_ct
  !============================================================================
  subroutine clean_mod_ct

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'clean_mod_ct'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    if(.not.allocated(BxFace_GB)) RETURN

    deallocate(BxFace_GB)
    deallocate(ByFace_GB)
    deallocate(BzFace_GB)
    deallocate(BxFaceFine_IIQSB)
    deallocate(ByFaceFine_IIQSB)
    deallocate(BzFaceFine_IIQSB)
    deallocate(VxBX_GB)
    deallocate(VxBY_GB)
    deallocate(VxBZ_GB)

    if(iProc==0)then
       call write_prefix
       write(iUnitOut,'(a)') 'clean_mod_ct deallocated arrays'
    end if

    call test_stop(NameSub, DoTest)
  end subroutine clean_mod_ct
  !============================================================================
  subroutine get_vxb(iBlock)

    ! Calculate VxB from fluxes following Balsara and Spicer

    use ModMain, ONLY : nI,nJ,nK
    use ModVarIndexes, ONLY : Bx_,By_,Bz_
    use ModAdvance, ONLY : Flux_VXI,Flux_VYI,Flux_VZI
    use BATL_lib, ONLY: CellFace_DB

    integer, intent(in) :: iBlock
    integer:: iGang
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'get_vxb'
    !--------------------------------------------------------------------------
    iGang = i_gang(iBlock)

    call test_start(NameSub, DoTest, iBlock)
    if(iBlock==iBlockTest)then
    else
       DoTest=.false.; DoTest=.false.
    end if

    ! VxBX_GB=(fy+fy-fz-fz)/4
    VxBX_GB(1:nI,1:nJ+1,1:nK+1,iBlock)= 0.25*(                        &
         (Flux_VYI(Bz_,1:nI,1:nJ+1,0:nK  ,iGang)                         &
         +Flux_VYI(Bz_,1:nI,1:nJ+1,1:nK+1,iGang) )/CellFace_DB(2,iBlock) &
         -(Flux_VZI(By_,1:nI,0:nJ  ,1:nK+1,iGang)                         &
         +Flux_VZI(By_,1:nI,1:nJ+1,1:nK+1,iGang) )/CellFace_DB(3,iBlock))

    ! VxBY_GB=(fz+fz-fx-fx)/4
    VxBY_GB(1:nI+1,1:nJ,1:nK+1,iBlock)= 0.25*(                        &
         (Flux_VZI(Bx_,0:nI  ,1:nJ,1:nK+1,iGang)                         &
         +Flux_VZI(Bx_,1:nI+1,1:nJ,1:nK+1,iGang) )/CellFace_DB(3,iBlock) &
         -(Flux_VXI(Bz_,1:nI+1,1:nJ,0:nK  ,iGang)                         &
         +Flux_VXI(Bz_,1:nI+1,1:nJ,1:nK+1,iGang) )/CellFace_DB(1,iBlock))

    ! VxBZ_GB=(fx+fx-fy-fy)/4
    VxBZ_GB(1:nI+1,1:nJ+1,1:nK,iBlock)= 0.25*(                        &
         (Flux_VXI(By_,1:nI+1,0:nJ  ,1:nK,iGang)                         &
         +Flux_VXI(By_,1:nI+1,1:nJ+1,1:nK,iGang) )/CellFace_DB(1,iBlock) &
         -(Flux_VYI(Bx_,0:nI  ,1:nJ+1,1:nK,iGang)                         &
         +Flux_VYI(Bx_,1:nI+1,1:nJ+1,1:nK,iGang) )/CellFace_DB(2,iBlock))

    if(DoTest)then
       write(*,*)'get_vxb: final VxB (edge centered)'
       write(*,*)'VxBX_GBLL,LR,RL,RR=',&
            VxBX_GB(iTest,jTest:jTest+1,kTest:kTest+1,iBlockTest)
       write(*,*)'VxBY_GBLL,LR,RL,RR=',&
            VxBY_GB(iTest:iTest+1,jTest,kTest:kTest+1,iBlockTest)
       write(*,*)'VxBZ_GBLL,LR,RL,RR=',&
            VxBZ_GB(iTest:iTest+1,jTest:jTest+1,kTest,iBlockTest)
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine get_vxb
  !============================================================================
  subroutine bound_vxb(iBlock)

    ! Apply boundary conditions on VxB

    use ModSize
    use ModMain, ONLY : TypeCellBc_I, Coord1MaxBc_
    use ModVarIndexes, ONLY : Bx_,By_,Bz_
    use ModAdvance, ONLY : Flux_VXI,Flux_VYI,Flux_VZI
    use ModParallel, ONLY : Unset_, DiLevel_EB
    use ModGeometry, ONLY : IsBody_B
    use ModPhysics, ONLY: SolarWindUx, SolarWindUy, SolarWindUz, &
         SolarWindBx, SolarWindBy, SolarWindBz
    use BATL_lib, ONLY: CellFace_DB, Used_GB

    integer, intent(in) :: iBlock

    integer:: i,j,k
    integer:: iGang

    ! Apply continuous or fixed boundary conditions at outer boundaries
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'bound_vxb'
    !--------------------------------------------------------------------------
    iGang = i_gang(iBlock)

    call test_start(NameSub, DoTest, iBlock)
    if(DiLevel_EB(1,iBlock)==Unset_)then
       do k=1,nK+1; do j=1,nJ
          VxBY_GB(1,j,k,iBlock) = &
               +Flux_VZI(Bx_,1,j,k,iGang)/CellFace_DB(3,iBlock)
       end do; end do
       do k=1,nK; do j=1,nJ+1
          VxBZ_GB(1,j,k,iBlock) = &
               -Flux_VYI(Bx_,1,j,k,iGang)/CellFace_DB(2,iBlock)
       end do; end do
    end if
    if(DiLevel_EB(2,iBlock)==Unset_)then
       ! fixed inflow!
       ! VxBX_GB(nI,:,:,iBlock)=SolarWindUy*SolarWindBz-SolarWindUz*SolarWindUy
       select case(TypeCellBc_I(Coord1MaxBc_))
       case('inflow','vary','fixed')
          VxBY_GB(nI+1,:,:,iBlock) = &
               SolarWindUz*SolarWindBx - SolarWindUx*SolarWindBz
          VxBZ_GB(nI+1,:,:,iBlock) = &
               SolarWindUx*SolarWindBy - SolarWindUy*SolarWindBx
       case default
          ! continuous
          do k=1,nK+1; do j=1,nJ
             VxBY_GB(nI+1,j,k,iBlock) = &
                  +Flux_VZI(Bx_,nI,j,k,iGang)/CellFace_DB(3,iBlock)
          end do; end do
          do k=1,nK; do j=1,nJ+1
             VxBZ_GB(nI+1,j,k,iBlock) = &
                  -Flux_VYI(Bx_,nI,j,k,iGang)/CellFace_DB(2,iBlock)
          end do; end do
       end select
    end if
    if(DiLevel_EB(3,iBlock)==Unset_)then
       do k=1,nK+1; do i=1,nI
          VxBX_GB(i,1,k,iBlock) = &
               -Flux_VZI(By_,i,1,k,iGang)/CellFace_DB(3,iBlock)
       end do; end do
       do k=1,nK; do i=1,nI+1
          VxBZ_GB(i,1,k,iBlock) = &
               +Flux_VXI(By_,i,1,k,iGang)/CellFace_DB(1,iBlock)
       end do; end do
    end if
    if(DiLevel_EB(4,iBlock)==Unset_)then
       do k=1,nK+1; do i=1,nI
          VxBX_GB(i,nJ+1,k,iBlock) = &
               -Flux_VZI(By_,i,nJ,k,iGang) /CellFace_DB(3,iBlock)
       end do; end do
       do k=1,nK; do i=1,nI+1
          VxBZ_GB(i,nJ+1,k,iBlock) = &
               +Flux_VXI(By_,i,nJ,k,iGang) /CellFace_DB(1,iBlock)
       end do; end do
    end if
    if(DiLevel_EB(5,iBlock)==Unset_)then
       do j=1,nJ+1; do i=1,nI
          VxBX_GB(i,j,1,iBlock) = &
               +Flux_VYI(Bz_,i,j,1,iGang) /CellFace_DB(2,iBlock)
       end do; end do
       do j=1,nJ; do i=1,nI+1
          VxBY_GB(i,j,1,iBlock) = &
               -Flux_VXI(Bz_,i,j,1,iGang) /CellFace_DB(1,iBlock)
       end do; end do
    end if
    if(DiLevel_EB(6,iBlock)==Unset_)then
       do j=1,nJ+1; do i=1,nI
          VxBX_GB(i,j,nK+1,iBlock) = &
               +Flux_VYI(Bz_,i,j,nK,iGang) /CellFace_DB(2,iBlock)
       end do; end do
       do j=1,nJ; do i=1,nI+1
          VxBY_GB(i,j,nK+1,iBlock) = &
               -Flux_VXI(Bz_,i,j,nK,iGang) /CellFace_DB(1,iBlock)
       end do; end do
    end if

    ! Set VxB to zero on the cell edges of the body cells
    if(IsBody_B(iBlock))then
       ! Apply inner boundary condition on the electric field
       ! Make sure that edges belonging to body ghost cells are also corrected
       do k=0,nK+1; do j=0,nJ+1; do i=0,nI+1
          if(.not.Used_GB(i,j,k,iBlock))then
             VxBX_GB(i,j:j+1,k:k+1,iBlock) = 0.0
             VxBY_GB(i:i+1,j,k:k+1,iBlock) = 0.0
             VxBZ_GB(i:i+1,j:j+1,k,iBlock) = 0.0
          end if
       end do; end do; end do
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine bound_vxb
  !============================================================================
  subroutine constrain_b(iBlock)

    ! Use CT scheme for updating the B field so that div B is conserved

    use ModSize
    use ModMain, ONLY : Dt
    use ModGeometry, ONLY : CellSize_DB

    integer, intent(in) :: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'constrain_b'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    if(iBlock==iBlockTest)then
    else
       DoTest=.false.; DoTest=.false.
    end if

    if(DoTest)then
       write(*,*)'constrain_b: initial face centered B'
       write(*,*)'BxfaceL,R=',&
            BxFace_GB(iTest:iTest+1,jTest,kTest,iBlockTest)
       write(*,*)'ByfaceL,R=',&
            ByFace_GB(iTest,jTest:jTest+1,kTest,iBlockTest)
       write(*,*)'BzfaceL,BzfaceR=',&
            BzFace_GB(iTest,jTest,kTest:kTest+1,iBlockTest)
    end if

    ! dBx/dt=d(VxBZ_GB)/dy-d(VxBY_GB)/dz
    BxFace_GB(1:nI+1,1:nJ,1:nK,iBlock)=                &
         BxFace_GB(1:nI+1,1:nJ,1:nK,iBlock) + Dt*(     &
         +(VxBZ_GB(1:nI+1,2:nJ+1,1:nK  ,iBlock)           &
         -VxBZ_GB(1:nI+1,1:nJ  ,1:nK  ,iBlock))           &
         /CellSize_DB(y_,iBlock)  &
         -(VxBY_GB(1:nI+1,1:nJ  ,2:nK+1,iBlock)           &
         -VxBY_GB(1:nI+1,1:nJ  ,1:nK  ,iBlock))           &
         /CellSize_DB(z_,iBlock))
    ! dBy/dt=d(VxBX_GB)/dz-d(VxBZ_GB)/dx
    ByFace_GB(1:nI,1:nJ+1,1:nK,iBlock)=                &
         ByFace_GB(1:nI,1:nJ+1,1:nK,iBlock) + Dt*(     &
         +(VxBX_GB(1:nI  ,1:nJ+1,2:nK+1,iBlock)           &
         -VxBX_GB(1:nI  ,1:nJ+1,1:nK  ,iBlock))           &
         /CellSize_DB(z_,iBlock)  &
         -(VxBZ_GB(2:nI+1,1:nJ+1,1:nK  ,iBlock)           &
         -VxBZ_GB(1:nI  ,1:nJ+1,1:nK  ,iBlock))           &
         /CellSize_DB(x_,iBlock))

    ! dBz/dt=d(VxBY_GB)/dx-d(VxBX_GB)/dy
    BzFace_GB(1:nI,1:nJ,1:nK+1,iBlock)=                &
         BzFace_GB(1:nI,1:nJ,1:nK+1,iBlock) + Dt*(     &
         +(VxBY_GB(2:nI+1,1:nJ  ,1:nK+1,iBlock)           &
         -VxBY_GB(1:nI  ,1:nJ  ,1:nK+1,iBlock))           &
         /CellSize_DB(x_,iBlock)  &
         -(VxBX_GB(1:nI  ,2:nJ+1,1:nK+1,iBlock)           &
         -VxBX_GB(1:nI  ,1:nJ  ,1:nK+1,iBlock))           &
         /CellSize_DB(y_,iBlock))
    if(DoTest)then
       write(*,*)'constrain_b: final face centered B'
       write(*,*)'BxfaceL,R=',&
            BxFace_GB(iTest:iTest+1,jTest,kTest,iBlockTest)
       write(*,*)'ByfaceL,R=',&
            ByFace_GB(iTest,jTest:jTest+1,kTest,iBlockTest)
       write(*,*)'BzfaceL,BzfaceR=',&
            BzFace_GB(iTest,jTest,kTest:kTest+1,iBlockTest)
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine constrain_b
  !============================================================================
  subroutine bface_to_bcenter(iBlock)

    use ModSize
    use ModVarIndexes, ONLY : Bx_,By_,Bz_
    use ModAdvance, ONLY : State_VGB
    use ModGeometry, ONLY : IsBody_B
    use BATL_lib,  ONLY: Used_GB

    integer, intent(in) :: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'bface_to_bcenter'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    State_VGB(Bx_:Bz_,:,:,:,iBlock) = -777.0

    ! average in direction x (b->B)
    State_VGB(Bx_,1:nI,1:nJ,1:nK,iBlock)= 0.5*(      &
         BxFace_GB(1:nI  ,1:nJ,1:nK,iBlock)+ &
         BxFace_GB(2:nI+1,1:nJ,1:nK,iBlock))

    ! average in direction y (b->B)
    State_VGB(By_,1:nI,1:nJ,1:nK,iBlock)= 0.5*(      &
         ByFace_GB(1:nI,1:nJ  ,1:nK,iBlock)+ &
         ByFace_GB(1:nI,2:nJ+1,1:nK,iBlock))

    ! average in direction z (b->B)
    State_VGB(Bz_,1:nI,1:nJ,1:nK,iBlock)= 0.5*(      &
         BzFace_GB(1:nI,1:nJ,1:nK  ,iBlock)+ &
         BzFace_GB(1:nI,1:nJ,2:nK+1,iBlock))

    if(IsBody_B(iBlock))then
       where(.not.Used_GB(:,:,:,iBlock))
          State_VGB(Bx_,:,:,:,iBlock)=0.0
          State_VGB(By_,:,:,:,iBlock)=0.0
          State_VGB(Bz_,:,:,:,iBlock)=0.0
       end where
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine bface_to_bcenter
  !============================================================================
  subroutine bcenter_to_bface(iBlock)

    use ModSize
    use ModVarIndexes, ONLY : Bx_,By_,Bz_
    use ModAdvance, ONLY : State_VGB
    use ModMain, ONLY: UseConstrainB

    integer, intent(in) :: iBlock

    integer:: i,j,k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'bcenter_to_bface'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! Estimate BFace from Bcenter

    ! The conditional write statements avoid a compiler optimization bug
    ! in the NAGWare Fortran 95 compiler Release 4.0a(388).
    ! The condition is never true, because this routine is only called
    ! when UseConstrainB is true

    do k=1,nK; do j=1,nJ; do i=1,nI+1
       BxFace_GB(i,j,k,iBlock)= 0.5*( &
            State_VGB(Bx_,i-1,j,k,iBlock)+ &
            State_VGB(Bx_,i  ,j,k,iBlock))
       if(.not.UseConstrainB)write(*,*)'!!!'
    end do; end do; end do
    do k=1,nK; do j=1,nJ+1; do i=1,nI
       ByFace_GB(i,j,k,iBlock)= 0.5*( &
            State_VGB(By_,i,j-1,k,iBlock)+ &
            State_VGB(By_,i,j  ,k,iBlock))
       if(.not.UseConstrainB)write(*,*)'!!!'
    end do; end do; end do
    do k=1,nK+1; do j=1,nJ; do i=1,nI
       BzFace_GB(i,j,k,iBlock)= 0.5*( &
            State_VGB(Bz_,i,j,k-1,iBlock)+ &
            State_VGB(Bz_,i,j,k  ,iBlock))
       if(.not.UseConstrainB)write(*,*)'!!!'
    end do; end do; end do

    call bound_bface(iBlock)

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine bcenter_to_bface
  !============================================================================
  subroutine bound_bface(iBlock)

    ! Set Bface to zero on the cell faces of the body cells
    ! Make sure that ghost cells inside the body are taken into account
    ! This may have to be generalized later

    use ModSize
    use ModGeometry, ONLY: IsBody_B
    use BATL_lib,  ONLY: Used_GB

    integer, intent(in) :: iBlock

    integer :: i,j,k

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'bound_bface'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(iBlock==iBlockTest)then
    else
       DoTest=.false.; DoTest=.false.
    end if

    if(DoTest)write(*,*)'bound_bface, IsBody_B=',IsBody_B(iBlock)

    if(IsBody_B(iBlock))then
       do k=0,nK+1; do j=0,nJ+1; do i=0,nI+1
          if(.not.Used_GB(i,j,k,iBlock))then
             BxFace_GB(i:i+1,j,k,iBlock)=0.0
             ByFace_GB(i,j:j+1,k,iBlock)=0.0
             BzFace_GB(i,j,k:k+1,iBlock)=0.0
          end if
       end do; end do; end do
    end if

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine bound_bface
  !============================================================================
  subroutine constrain_ics(iBlock)

    ! Initialize B field, Bface, and Bcenter for Constrained Transport

    use ModMain
    use ModVarIndexes
    use ModAdvance, ONLY : State_VGB
    use ModGeometry, ONLY : IsBody_B
    use ModIO, ONLY : IsRestart
    use ModPhysics, ONLY : SolarWindBx,SolarWindBy,SolarWindBz
    use BATL_lib, ONLY: Xyz_DGB, Used_GB

    integer, intent(in) :: iBlock

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'constrain_ics'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    if(Unused_B(iBlock))then
       BxFace_GB(:,:,:,iBlock)=0.0
       ByFace_GB(:,:,:,iBlock)=0.0
       BzFace_GB(:,:,:,iBlock)=0.0
    else

       if(.not.IsRestart)then
          where(Xyz_DGB(x_,:,:,:,iBlock)<16.)
             ! Cancel B field at x<16Re to avoid non-zero initial divB
             ! x=16 is a good choice because it is a power of 2 so it is
             ! a block boundary for all block sizes.
             ! x=16 is larger than typical rBody.
             State_VGB(Bx_,:,:,:,iBlock)=0.0
             State_VGB(By_,:,:,:,iBlock)=0.0
             State_VGB(Bz_,:,:,:,iBlock)=0.0
             ! Balance total pressure
             State_VGB(P_,:,:,:,iBlock)=State_VGB(P_,:,:,:,iBlock)+ &
                  0.5*(SolarWindBx**2+SolarWindBy**2+SolarWindBz**2)
          elsewhere
             ! Use solar wind values ahead of the Earth
             State_VGB(Bx_,:,:,:,iBlock)=SolarWindBx
             State_VGB(By_,:,:,:,iBlock)=SolarWindBy
             State_VGB(Bz_,:,:,:,iBlock)=SolarWindBz
          end where
       end if

       if(index(StringTest,'testCTcoarse')>0)then
          State_VGB(Bx_,:,:,:,iBlock)=   Xyz_DGB(x_,:,:,:,iBlock)
          State_VGB(By_,:,:,:,iBlock)=   Xyz_DGB(y_,:,:,:,iBlock)
          State_VGB(Bz_,:,:,:,iBlock)=-2*Xyz_DGB(z_,:,:,:,iBlock)
          if(IsBody_B(iBlock))then
             where(.not.Used_GB(:,:,:,iBlock))
                State_VGB(Bx_,:,:,:,iBlock)=0.
                State_VGB(By_,:,:,:,iBlock)=0.
                State_VGB(Bz_,:,:,:,iBlock)=0.
             end where
          end if
       end if

    endif

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine constrain_ics
  !============================================================================
end module ModConstrainDivB
!==============================================================================
