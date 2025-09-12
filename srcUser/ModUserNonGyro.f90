!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModUser

  use BATL_lib, ONLY: &
       test_start, test_stop, iProc

  use ModSize, ONLY: nI,nJ,nK
  use ModPhysics, ONLY: ElectronCharge

  use ModUserEmpty, &
       IMPLEMENTED1 => user_calc_sources, &
       IMPLEMENTED2 => user_read_inputs

  include 'user_module.h' ! list of public methods

  character (len=*), parameter :: NameUserFile = "ModUserNonGyro.f90"
  character (len=*), parameter :: NameUserModule = &
       'Non-Gyrotropic Reconnection Model, Kuznetsova, Toth'

  real, parameter :: UnsetX_ = -100.0

  ! Reconnection region where source terms are applied
  real :: xRecoMin  = -99.0
  real :: xRecoMax  =   0.0

  ! Search region for reconnection line
  real :: xLineMin  = -87.0
  real :: xLineMax  =  -3.0 ! for first line
  real :: xLineMax1 =  -5.0 ! for second line

  real :: yLineMax = 23.38  ! maximum distance in Y direction
  real :: DyLine   = 0.125  ! Y resolution of reconnection line search
  integer :: nJJ   = 187    ! = floor(yLineMax/DyLine)

  integer, parameter :: MaxJJ=1000 ! should exceed nJJ

  ! By default assume hydrogene for the effective ion mass
  real :: IonMassReconnect = 1.0, IonMassPerCharge

  real, dimension(MaxJJ) :: YYR, &
       XX0, DWZ_0, DWX_0, EF0_0, BZPR_0, VXPR_0, Cos_Thet_0, Sin_Thet_0, &
       XX1, DWZ_1, DWX_1, EF0_1, BZPR_1, VXPR_1, Cos_Thet_1, Sin_Thet_1

  integer, parameter :: nRecParam=7, dBzDx_=1, dVxDx_=2, WidthX_=3, &
       Efield_=4, WidthZ_=5, CosTheta_=6, SinTheta_=7

  real :: RecParam_I(nRecParam)

contains
  !============================================================================
  subroutine user_read_inputs
    use ModReadParam

    character (len=100) :: NameCommand
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_read_inputs'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    do
       if(.not.read_line() ) EXIT
       if(.not.read_command(NameCommand)) CYCLE
       select case(NameCommand)
       case('#LIMITX')
          call read_var('xRecoMin',xRecoMin)
          call read_var('xRecoMax',xRecoMax)
          call read_var('xLineMin',xLineMin)
          call read_var('xLineMax',xLineMax)
          call read_var('xLineMax1',xLineMax1)

       case('#LIMITY')
          call read_var('yLineMax',yLineMax)
          call read_var('DyLine',DyLine)
          nJJ = nint(yLineMax / DyLine)

       case('#IONMASS')
          call read_var('IonMass', IonMassReconnect)

       case('#USERINPUTEND')
          if(iProc==0) write(*,*)'USERINPUTEND'
          EXIT

       case default
          if(iProc==0) call stop_mpi( &
               'user_read_inputs: unrecognized command: '//NameCommand)
       end select
    end do
    call test_stop(NameSub, DoTest)
  end subroutine user_read_inputs
  !============================================================================

  subroutine user_calc_sources(iBlock)

    use ModMain
    use ModVarIndexes
    use ModAdvance
    use ModGeometry
    use ModConst
    use ModNumConst
    use ModPhysics
    use ModPhysics
    use ModParallel

    integer, intent(in) :: iBlock

    ! User declared local variables go here
    integer :: i,j,k, jj
    real :: xt, yt, zt
    real :: xxx, zzz, yyy
    real :: InvWidthX, InvWidthZ, Ey, dBxDt, dBzDt

    integer :: nStepOld = -1

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_calc_sources'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    IonMassPerCharge = IonMassReconnect / ElectronCharge

    ! find reconnection lines at the beginning of the time step
    if(nStep /= nStepOld)then
       nStepOld = nStep
       call set_rec_line
       call set_rec_line1

       if (DoTest) then
          do jj = 1, nJJ/2+1, nJJ/4
             write(*,*)'jj=',jj,' Y=',YYR(jj)
             if(XX0(jj) <= UnsetX_) CYCLE
             write(*,*)'XX0=',XX0(jj), &
                  'BZPR=',BZPR_0(jj), 'VXPR=',VXPR_0(jj), &
                  'DWX=', DWX_0(jj), 'DWZ=',DWZ_0(jj), &
                  'EF0=', EF0_0(jj)
             !    'CosThet=',Cos_Thet_0(1), 'SinThet=', Sin_Thet_0(jj)

             if(XX1(jj) <= UnsetX_) CYCLE
             write(*,*)'XX1=',XX1(jj), &
                  'BZPR=',BZPR_1(jj), 'VXPR=',VXPR_1(jj), &
                  'DWX=', DWX_1(jj), 'DWZ=',DWZ_1(jj), &
                  'EF0=', EF0_1(jj)
             !    'CosThet=',Cos_Thet_1(1), 'SinThet=', Sin_Thet_1(jj)
          end do
       endif
    end if

    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       xt = Xyz_DGB(x_,i,j,k,iBlock)
       yt = abs(Xyz_DGB(y_,i,j,k,iBlock))
       zt = Xyz_DGB(z_,i,j,k,iBlock)

       if( yt > yLineMax) CYCLE

       jj = 1 + (yt - DyLine/2)/DyLine

       if(jj < 1 .or. jj > nJJ)then
          write(*,*)'ERROR: yt, jj, nJJ=',yt,jj,nJJ
          call stop_mpi('jj index out of 1..nJJ range')
       end if

       if(xt > xRecoMin .and. xt < xRecoMax .and. XX0(jj) > UnsetX_)then

          InvWidthX = 2/DWX_0(jj)
          InvWidthZ = 2/DWZ_0(jj)
          ! InvWidthZ = 2/(IonMassPerCharge*2.*sqrt(2.)) !!! Masha's experiment

          xxx = InvWidthX*(xt - XX0(jj))
          yyy = yt/yLineMax
          zzz = InvWidthZ*zt
          Ey  = EF0_0(jj)/(cosh(zzz)*cosh(xxx)*cosh(yyy))

          ! B' = B - curl Ey: Bx' = Bx + dEy/dZ  and Bz' = Bz - dEy/dX
          ! d(1/cosh(xxx))/dx = -sinh(xxx)/cosh(xxx)^2*InvWidthX

          dBxDt =  - Ey*tanh(zzz)*InvWidthZ
          dBzDt =  + Ey*tanh(xxx)*InvWidthX

          Source_VC(Bx_,i,j,k)     = Source_VC(Bx_,i,j,k) + dBxDt
          Source_VC(Bz_,i,j,k)     = Source_VC(Bz_,i,j,k) + dBzDt
          Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) &
               + State_VGB(Bx_,i,j,k,iBlock)*dBxDt &
               + State_VGB(Bz_,i,j,k,iBlock)*dBzDt

          ! Source_VC(Bx_,i,j,k) = Source_VC(Bx_,i,j,k) &
          !     - Ey*tanh(zzz)*InvWidthZ*Cos_Thet_0(jj)
          ! Source_VC(Bz_,i,j,k) = Source_VC(Bz_,i,j,k) &
          !     + Ey*tanh(xxx)*InvWidthX*Sin_Thet_0(jj)
       endif

       ! Add term from second reconnection site
       ! Make sure that the second line is separated from the first one
       if(xt > xRecoMin .and. xt < XX0(jj) .and. XX1(jj) > UnsetX_ &
            .and. XX1(jj) < XX0(jj)-DWX_0(jj)*0.5) then

          InvWidthX = 2/DWX_1(jj)
          InvWidthZ = 2/DWZ_1(jj)

          xxx = InvWidthX*(xt - XX1(jj))
          yyy = yt/yLineMax
          zzz = InvWidthZ*zt
          Ey  = EF0_1(jj)/(cosh(zzz)*cosh(xxx)*cosh(yyy))

          ! B' = B - curl Ey: Bx' = Bx + dEy/dZ  and Bz' = Bz - dEy/dX
          ! d(1/cosh(xxx))/dx = -sinh(xxx)/cosh(xxx)^2*InvWidthX

          dBxDt =  - Ey*tanh(zzz)*InvWidthZ
          dBzDt =  + Ey*tanh(xxx)*InvWidthX

          Source_VC(Bx_,i,j,k)     = Source_VC(Bx_,i,j,k) + dBxDt
          Source_VC(Bz_,i,j,k)     = Source_VC(Bz_,i,j,k) + dBzDt
          Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) &
               + State_VGB(Bx_,i,j,k,iBlock)*dBxDt &
               + State_VGB(Bz_,i,j,k,iBlock)*dBzDt

          ! Source_VC(Bx_,i,j,k) = Source_VC(Bx_,i,j,k) &
          !     - Ey*tanh(zzz)*InvWidthZ*Cos_Thet_1(jj)
          ! Source_VC(Bz_,i,j,k) = Source_VC(Bz_,i,j,k) &
          !     + Ey*tanh(xxx)*InvWidthX*Sin_Thet_1(jj)
       endif

    end do; end do; end do     ! end the k,j,i loops

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_calc_sources
  !============================================================================

  subroutine set_rec_line
    use ModMain
    use ModVarIndexes
    use ModSize
    use ModIO
    use ModPhysics
    use ModGeometry
    use ModAdvance
    use ModParallel
    use ModConst
    use ModMpi

    integer :: iPE, iPEmax, iBlock, iBLKmax

    integer :: i,j,k
    integer :: ii,jj
    integer :: j1, iX, jY
    integer :: iError
    real :: XX0max, XX0max_all, yj1

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_rec_line'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    do jj=1, njj
       !   YYR(jj)=0.03125+0.0625*(jj -1)
       YYR(jj) = DyLine/2 + (jj-1)*DyLine

       XX0(jj) = UnsetX_
       XX0max  = UnsetX_

       BZPR_0(jj) = -1.
       VXPR_0(jj) = 0.
       DWX_0(jj) = -1.
       DWZ_0(jj) = -1.
       EF0_0(jj) = 0.
       Cos_Thet_0(jj) = 1.
       Sin_Thet_0(jj) = 0.
       RecParam_I(1) = -1.
       RecParam_I(2) = 0.
       RecParam_I(3) = -1.
       RecParam_I(4) = 0.
       RecParam_I(5) = -1.
       RecParam_I(6) = 1.
       RecParam_I(7) = 0.
       !
       !
       !
       iPE = -1
       iPEmax = -1
       iBLKmax = -1

       BLOCKS: do iBlock = 1,nBlockMax

          if (Unused_B(iBlock)) CYCLE BLOCKS

          ! Check if block is within the search region
          if ( Xyz_DGB(x_,nI,1,1,iBlock) > xLineMin .and. &
               Xyz_DGB(x_,nI,1,1,iBlock) < xLineMax .and. &
               YYR(jj) >= Xyz_DGB(y_,1,1,1,iBlock)  - 0.5*CellSize_DB(y_,iBlock) .and. &
               YYR(jj) <  Xyz_DGB(y_,1,nJ,1,iBlock) + 0.5*CellSize_DB(y_,iBlock) .and. &
               0.25*CellSize_DB(z_,iBlock) >=Xyz_DGB(z_,1,1,1,iBlock)  - 0.5*CellSize_DB(z_,iBlock) .and.&
               0.25*CellSize_DB(z_,iBlock) < Xyz_DGB(z_,1,1,nK,iBlock) + 0.5*CellSize_DB(z_,iBlock) ) then

             ! Find the j index corresponding to YYR
             yj1 = (YYR(jj)-Xyz_DGB(y_,1,1,1,iBlock))/CellSize_DB(y_,iBlock)  + 1
             j1  = max(1,floor(yj1))

             do i=nI,1,-1
                ! Find reversal in Bz
                if ( B0_DGB(z_,i+1,j1,1,iBlock) + &
                     State_VGB(Bz_,i+1,j1,1,iBlock) > 0. .and. &
                     B0_DGB(z_,i-1,j1,1,iBlock) + &
                     State_VGB(Bz_,i-1,j1,1,iBlock) <=0.) then

                   XX0max  = Xyz_DGB(x_,i,j1,1,iBlock)
                   iX      = i
                   iBLKmax = iBlock
                   jY      = j1

                   EXIT BLOCKS
                endif
             enddo

          end if
       enddo BLOCKS

       ! Get the X coordinate of the first reconnection line
       call MPI_ALLREDUCE(XX0max, XX0max_all, 1, MPI_REAL,MPI_MAX,iComm,iError)
       XX0(jj) = XX0max_all

       ! Calculate the reconnection box size and electric field
       if (XX0max == XX0max_all .and. XX0max_all > xRecoMin) then
          iPE = iProc
          call set_rec_parameters(iX, jY, 1, iBlkMax)
       else
          iPE = -1
       endif

       ! Tell everyone which processor contains iBLKmax (if any)
       call MPI_ALLREDUCE(iPE, iPEmax, 1, MPI_INTEGER, MPI_MAX, iComm, iError)

       ! If there is a reconnection line, broadcast its parameters
       if (iPEmax >= 0) then
          call MPI_Bcast(RecParam_I,nRecParam,MPI_REAL,iPEmax,iComm,iError)
          BZPR_0(jj)     = RecParam_I(dBzDx_)
          VXPR_0(jj)     = RecParam_I(dVxDx_)
          DWX_0(jj)      = RecParam_I(WidthX_)
          EF0_0(jj)      = RecParam_I(Efield_) ! multiplied by 2 for run hr4
          DWZ_0(jj)      = RecParam_I(WidthZ_)
          Cos_Thet_0(jj) = RecParam_I(CosTheta_)
          Sin_Thet_0(jj) = RecParam_I(SinTheta_)
       endif

    enddo

    call test_stop(NameSub, DoTest)
  end subroutine set_rec_line
  !============================================================================

  subroutine set_rec_line1
    use ModMain
    use ModVarIndexes
    use ModSize
    use ModIO
    use ModPhysics
    use ModGeometry
    use ModAdvance
    use ModParallel
    use ModConst
    use ModMpi

    integer :: iPE, iPEmax, iBlock, iBLKtemp, iBLKmax
    integer :: iBLKtemp1, iBLKtemp2

    integer :: i,j,k
    integer :: ii,jj
    integer :: j1, iX, jY
    integer :: iError
    real    :: XX0max, XX0max_all, yj1
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_rec_line1'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    do jj=1, njj
       YYR(jj) = DyLine/2 + (jj-1)*DyLine
       XX1(jj) = UnsetX_
       XX0max = UnsetX_

       BZPR_1(jj) = -1.
       VXPR_1(jj) = 0.
       DWX_1(jj) = -1.
       DWZ_1(jj) = -1.
       EF0_1(jj) = 0.
       Cos_Thet_1(jj) = 1.
       Sin_Thet_1(jj) = 0.
       RecParam_I(1) = -1.
       RecParam_I(2) = 0.
       RecParam_I(3) = -1.
       RecParam_I(4) = 0.
       RecParam_I(5) = -1.
       RecParam_I(6) = 1.
       RecParam_I(7) = 0.
       !
       iPE = -1
       iPEmax = -1
       iBLKmax = -1

       BLOCKS: do iBlock = 1,nBlock

          if (Unused_B(iBlock)) CYCLE

          if ( Xyz_DGB(x_,nI,1,1,iBlock) > xLineMin .and. &
               Xyz_DGB(x_,nI,1,1,iBlock) < xLineMax1 .and. &
               Xyz_DGB(x_,nI,1,1,iBlock) < XX0(jj) - 2. .and. &
               YYR(jj) >= Xyz_DGB(y_,1,1,1,iBlock) - 0.5*CellSize_DB(y_,iBlock) .and. &
               YYR(jj) < Xyz_DGB(y_,1,nJ,1,iBlock) + 0.5*CellSize_DB(y_,iBlock) .and. &
               0.25*CellSize_DB(z_,iBlock) >=Xyz_DGB(z_,1,1,1,iBlock) - 0.5*CellSize_DB(z_,iBlock) .and. &
               0.25*CellSize_DB(z_,iBlock) < Xyz_DGB(z_,1,1,nK,iBlock) + 0.5*CellSize_DB(z_,iBlock) ) then

             yj1=(YYR(jj)-Xyz_DGB(y_,1,1,1,iBlock))/CellSize_DB(y_,iBlock)+1.
             j1=max(1,floor(yj1))

             do i=nI,1,-1
                !
                if ( B0_DGB(z_,i+1,j1,1,iBlock) + &
                     State_VGB(Bz_,i+1,j1,1,iBlock) > 0 .and. &
                     B0_DGB(z_,i-1,j1,1,iBlock) + &
                     State_VGB(Bz_,i-1,j1,1,iBlock) <=0) then

                   XX0max  = Xyz_DGB(x_,i,j1,1,iBlock)
                   iX      = i
                   iBLKmax = iBlock
                   jY      = j1
                   EXIT BLOCKS
                endif
             enddo

          end if
       enddo BLOCKS

       ! Get the X coordinate of the second reconnection line
       call MPI_ALLREDUCE(XX0max, XX0max_all, 1, MPI_REAL,MPI_MAX,iComm,iError)
       XX1(jj) = XX0max_all

       ! Calculate the reconnection box size and electric field
       if (XX0max == XX0max_all .and. XX0max_all > xRecoMin) then
          iPE = iproc
          call set_rec_parameters(iX, jY, 1, iBlkMax)
       else
          iPE = -1
       endif
       ! Tell everyone which processor contains iBLKmax (if any)
       call MPI_ALLREDUCE(iPE, iPEmax, 1, MPI_INTEGER, MPI_MAX, iComm,iError)

       ! If there is a reconnection line, broadcast its parameters
       if (iPEmax >= 0) then
          call MPI_Bcast(RecParam_I,nRecParam,MPI_REAL,iPEmax,iComm,iError)
          BZPR_1(jj)     = RecParam_I(dBzDx_)
          VXPR_1(jj)     = RecParam_I(dVxDx_)
          DWX_1(jj)      = RecParam_I(WidthX_)
          EF0_1(jj)      = RecParam_I(Efield_) ! multiplied by 2 for run hr4
          DWZ_1(jj)      = RecParam_I(WidthZ_)
          Cos_Thet_1(jj) = RecParam_I(CosTheta_)
          Sin_Thet_1(jj) = RecParam_I(SinTheta_)
       endif

    enddo

    call test_stop(NameSub, DoTest)
  end subroutine set_rec_line1
  !============================================================================

  subroutine set_rec_parameters(i, j, k, iBlock)

    use ModAdvance, ONLY: State_VGB, Bx_, Bz_, Rho_, RhoUx_, RhoUz_, p_, &
         B0_DGB
    use ModMain, ONLY:x_,y_,z_
    use ModGeometry, ONLY: CellSize_DB

    integer, intent(in) :: i, j, k, iBlock

    real:: dBzDx, dBxDz, dVxDx, WidthX, WidthZ, Efield, IonSpeed
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_rec_parameters'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    ! dBz/dx
    dBzDx = ((State_VGB(Bz_,i+1,j,k,iBlock) &
         +      B0_DGB(z_,i+1,j,k,iBlock))  &
         -   (State_VGB(Bz_,i-1,j,k,iBlock) &
         +      B0_DGB(z_,i-1,j,k,iBlock))) / (2*CellSize_DB(x_,iBlock))

    ! dBx/dz
    dBxDz = ((State_VGB(Bx_,i,j,k+1,iBlock) &
         +      B0_DGB(x_,i,j,k+1,iBlock))  &
         -   (State_VGB(Bx_,i,j,k-1,iBlock) &
         +      B0_DGB(x_,i,j,k-1,iBlock))) / (2*CellSize_DB(z_,iBlock))

    ! d(Vx)/dx
    dVxDx = (State_VGB(RhoUx_,i+1,j,1,iBlock) &
         /   State_VGB(Rho_  ,i+1,j,1,iBlock) &
         -   State_VGB(RhoUx_,i-1,j,1,iBlock) &
         /   State_VGB(Rho_  ,i-1,j,1,iBlock)) / (2*CellSize_DB(x_,iBlock))

    ! IonSpeed = sqrt(2*P/Rho)
    IonSpeed = sqrt(2*State_VGB(p_,i,j,k,iBlock)/State_VGB(Rho_,i,j,k,iBlock))

    Efield = IonMassPerCharge*IonSpeed*dVxDx

    ! WidthX = sqrt(IonMassPerCharge*sqrt(2*p/rho) / (dBz/dx))
    WidthX = 4*CellSize_DB(x_,iBlock)
    if (dBzDx > 0) WidthX = max(WidthX, sqrt(IonMassPerCharge*IonSpeed/dBzDx))

    ! WidthZ = max(4*Dz, sqrt(IonMassPerCharge/(dBx/dz) * sqrt(2*p/rho)))
    WidthZ = 4*CellSize_DB(z_,iBlock)
    if (dBxDz > 0) WidthZ = max(WidthZ, sqrt(IonMassPerCharge*IonSpeed/dBxDz))

    RecParam_I(dBzDx_)    = dBzDx
    RecParam_I(dVxDx_)    = dVxDx
    RecParam_I(Efield_)   = Efield
    RecParam_I(WidthX_)   = WidthX
    RecParam_I(WidthZ_)   = WidthZ
    RecParam_I(CosTheta_) = 1.0
    RecParam_I(SinTheta_) = 0.0

    ! The code below takes the tilt in the X-Y plane into account

    ! BX = State_VGB(Bx_,:,:,:,iBlock)+B0xCell_BLK(:,:,:,iBlock)
    ! BY = State_VGB(By_,:,:,:,iBlock)+B0yCell_BLK(:,:,:,iBlock)
    ! BZ = State_VGB(Bz_,:,:,:,iBlock)+B0zCell_BLK(:,:,:,iBlock)
    !        B2(:,:,:) = BX**2+BY**2+BZ**2
    ! Jyy=0.5*((BX(i,j,2)-BX(i,j,0))/CellSize_DB(z_,iBlock) - &
    !     (BZ(i+1,j,1)-BZ(i-1,j,1))/CellSize_DB(x_,iBlock))
    ! Jxx=0.5*((BZ(i,j+1,1)-BZ(i,j-1,1))/CellSize_DB(y_,iBlock)     - &
    !     (BY(i,j,2)-BY(i,j,0))/CellSize_DB(z_,iBlock))
    ! Jxy=sqrt(Jyy**2+Jxx**2)
    ! CosThet=Jyy/Jxy
    ! SinThet=Jxx/Jxy
    ! BzPr=(BZ(i+1,j,1)-BZ(i-1,j,1))/(2.*CellSize_DB(x_,iBlock))*CosThet-&
    !        (BZ(i,j+1,1)-BZ(i,j-1,1))/(2.*CellSize_DB(y_,iBlock))*SinThet
    !
    ! Rho3 = State_VGB(rho_,i,j-1,1,iBlock)
    ! Rho4 = State_VGB(rho_,i,j+1,1,iBlock)
    ! VYPRX=(State_VGB(rhoUy_,i+1,j,1,iBlock)/Rho2 - &
    !     State_VGB(rhoUy_,i-1,j,1,iBlock)/Rho1)/(2.*CellSize_DB(x_,iBlock))
    ! VXPRY=(State_VGB(rhoUx_,i,j+1,1,iBlock)/Rho4 - &
    !     State_VGB(rhoUx_,i,j-1,1,iBlock)/Rho3)/(2.*CellSize_DB(y_,iBlock))
    ! VYPRY=(State_VGB(rhoUy_,i,j+1,1,iBlock)/Rho4 - &
    !     State_VGB(rhoUy_,i,j-1,1,iBlock)/Rho3)/(2.*CellSize_DB(y_,iBlock))
    ! RecParam_I(2)=VXPRX*CosThet**2-CosThet*SinThet*(VYPRX+VXPRY) &
    !       +VYPRY*SinThet**2
    ! write(*,*)'VXPRX=',VXPRX,'VYPRX=',VYPRY,'VXPRY=',VXPRY, &
    !       'VYPRY=',VYPRY

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine set_rec_parameters
  !============================================================================

end module ModUser
!==============================================================================

