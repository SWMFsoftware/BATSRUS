!^CFG COPYRIGHT UM

subroutine calc_sources
  use ModProcMH
  use ModMain
  use ModVarIndexes
  use ModGeometry, ONLY : dx_BLK, dy_BLK, dz_BLK, R_BLK,&
       body_BLK, Rmin_BLK, vInv_CB
  use ModGeometry, ONLY : R2_BLK                        !^CFG IF SECONDBODY
  use ModAdvance
  use ModParallel, ONLY : NOBLK, neiLEV, &
       neiLtop,neiLbot,neiLeast,neiLwest,neiLnorth,neiLsouth
  use ModPhysics
  use ModNumConst
  use ModResistivity,ONLY : UseResistivity, Eta_GB   !^CFG IF DISSFLUX
  use ModUser,     ONLY : user_calc_sources
  use ModCoordTransform
  use ModHallResist, ONLY: &
       UseHallResist, HallHyperFactor, calc_hyper_resistivity 
  use ModMultiFluid

  use ModCovariant, ONLY: UseCovariant !^CFG IF COVARIANT

  implicit none

  integer :: i, j, k, iDim

  logical :: DoTest, DoTestMe

  real :: Coef

  ! Variable for div B diffusion
  real :: Dr

  real,dimension(3)::CurlB0CrossB_D

  ! Variables needed for Boris source terms also used for div(u)
  real :: FullBx, FullBy, FullBz, Ux, Uy, Uz, RhoInv
  real :: E_D(3), DivE

  ! Variables needed for Joule heating
  real :: Current_D(3)

  ! Variables for multi-ion MHD
  integer :: iFirstIons
  real    :: InvCharge, InvNumDens,  State_V(nVar)
  real, dimension(3) :: FullB_D, uIon_D, u_D, uPlus_D, uPlusHallU_D, Force_D
  real, dimension(nIonFluid) :: NumDens_I, InvRho_I, Ux_I, Uy_I, Uz_I
  !---------------------------------------------------------------------------
  if(iProc==PROCtest .and. globalBLK==BLKtest)then
     call set_oktest('calc_sources', DoTest, DoTestMe)
  else
     DoTest=.false.; DoTestMe=.false.
  end if

  Source_VC   = cZero

  ! Calculate source terms for ion pressure
  if(UseNonconservative)then
     do iFluid = 1, nFluid
        call select_fluid
        ! Adiabatic heating: -(g-1)*P*Div(U)
        do k=1,nK; do j=1,nJ; do i=1,nI
           Source_VC(iP,i,j,k) = -(g-1)*State_VGB(iP,i,j,k,globalBLK)*&
                vInv_CB(i,j,k,globalBLK)*&
                (uDotArea_XI(i+1,j,k,iFluid) - uDotArea_XI(i,j,k,iFluid) &
                +uDotArea_YI(i,j+1,k,iFluid) - uDotArea_YI(i,j,k,iFluid) &
                +uDotArea_ZI(i,j,k+1,iFluid) - uDotArea_ZI(i,j,k,iFluid))
        end do; end do; end do
     end do

     ! Joule heating: dP/dt += (gamma-1)*eta*j**2
     if(UseResistFlux .or. UseResistivity)then  !^CFG IF DISSFLUX BEGIN
        do k=1,nK; do j=1,nJ; do i=1,nI           
           call get_current(i,j,k,GlobalBlk,Current_D)
           Source_VC(P_,i,j,k) = Source_VC(P_,i,j,k) + &
                (g-1) * Eta_GB(i,j,k,GlobalBlk) * sum(Current_D**2)
        end do; end do; end do
     end if                 !^CFG END DISSFLUX
  end if

  if(UseElectronPressure)then
     ! Adiabatic heating for electron pressure: -(g-1)*Pe*Div(U)
     do k=1,nK; do j=1,nJ; do i=1,nI
        Source_VC(Pe_,i,j,k) = -(g-1)*State_VGB(Pe_,i,j,k,globalBLK)*&
             vInv_CB(i,j,k,globalBLK)*&
             (uDotArea_XI(i+1,j,k,eFluid_) - uDotArea_XI(i,j,k,eFluid_) &
             +uDotArea_YI(i,j+1,k,eFluid_) - uDotArea_YI(i,j,k,eFluid_) &
             +uDotArea_ZI(i,j,k+1,eFluid_) - uDotArea_ZI(i,j,k,eFluid_))
     end do; end do; end do
  end if


  if(UseDivbSource)then
     if(UseCovariant)then   !^CFG IF COVARIANT BEGIN
        call calc_divb_source_covar
     else                   !^CFG END COVARIANT
        call calc_divb_source
     end if                 !^CFG IF COVARIANT

     if(DoTestMe)write(*,*)'divb=',DivB1_GB(iTest,jTest,kTest,BlkTest)
     if(DoTestMe.and.VarTest>=RhoUx_.and.VarTest<=RhoUz_)&
          call write_source('After B0B1 source')

     ! Add contributions to other source terms
     do k=1,nK; do j=1,nJ; do i=1,nI
        RhoInv=cOne/State_VGB(rho_,i,j,k,globalBLK)
        Source_VC(Bx_:Bz_,i,j,k)    = Source_VC(Bx_:Bz_,i,j,k) &
             -DivB1_GB(i,j,k,globalBLK)* &
             State_VGB(rhoUx_:rhoUz_,i,j,k,globalBLK)*RhoInv

        if(TypeFluid_I(1) /= 'ion') CYCLE

        Source_VC(rhoUx_:rhoUz_,i,j,k) = Source_VC(rhoUx_:rhoUz_,i,j,k) -&
             DivB1_GB(i,j,k,globalBLK)* &
             State_VGB(Bx_:Bz_,i,j,k,globalBLK)
        Source_VC(Energy_,i,j,k)     = Source_VC(Energy_,i,j,k) &
             -DivB1_GB(i,j,k,globalBLK)* &
             sum(State_VGB(Bx_:Bz_,i,j,k,globalBLK)*&
             State_VGB(rhoUx_:rhoUz_,i,j,k,globalBLK))*RhoInv
     end do;end do;end do

     if (UseB0Source .and. TypeFluid_I(1) == 'ion') then
        do k=1,nK; do j=1,nJ; do i=1,nI
           Source_VC(rhoUx_:rhoUz_,i,j,k)=Source_VC(rhoUx_:rhoUz_,i,j,k) - &
                State_VGB(Bx_:Bz_,i,j,k,globalBLK)*DivB0_CB(i,j,k,globalBLK)+&
                cross_product(&
                State_VGB(Bx_:Bz_,i,j,k,globalBLK),&
                CurlB0_DCB(:,i,j,k,globalBLK))
        end do; end do; end do
     end if
  else
     call calc_divB
  end if

  if(UseCurlB0)then
     do k=1,nK; do j=1,nJ; do i=1,nI
        if(R_BLK(i,j,k,globalBLK)<rCurrentFreeB0)CYCLE
        CurlB0CrossB_D = cross_product(&
             CurlB0_DCB(:,i,j,k,globalBLK),&
             State_VGB(Bx_:Bz_,i,j,k,globalBLK)+(/&
             B0xCell_BLK(i,j,k,globalBLK),&
             B0yCell_BLK(i,j,k,globalBLK),&
             B0zCell_BLK(i,j,k,globalBLK)/))
        Source_VC(rhoUx_:rhoUz_,i,j,k)= Source_VC(rhoUx_:rhoUz_,i,j,k) +&
             CurlB0CrossB_D
        Source_VC(Energy_,i,j,k)     = Source_VC(Energy_,i,j,k)        +&
             sum(CurlB0CrossB_D*State_VGB(rhoUx_:rhoUz_,i,j,k,globalBLK))&
             /State_VGB(rho_,i,j,k,globalBLK)
     end do;end do;end do
  end if

  if(boris_correction &                             !^CFG IF BORISCORR BEGIN
       .and. boris_cLIGHT_factor < 0.9999 & 
       .and. index(test_string,'nodivE')<1) then

     coef= (boris_cLIGHT_factor**2 - 1.0)*inv_c2LIGHT
     do k=1,nK; do j=1,nJ; do i=1,nI
        FullBx = B0xCell_BLK(i,j,k,globalBLK)+State_VGB(Bx_,i,j,k,globalBLK)
        FullBy = B0yCell_BLK(i,j,k,globalBLK)+State_VGB(By_,i,j,k,globalBLK)
        FullBz = B0zCell_BLK(i,j,k,globalBLK)+State_VGB(Bz_,i,j,k,globalBLK)
        Ux = State_VGB(rhoUx_,i,j,k,globalBLK)/State_VGB(rho_,i,j,k,globalBLK)
        Uy = State_VGB(rhoUy_,i,j,k,globalBLK)/State_VGB(rho_,i,j,k,globalBLK)
        Uz = State_VGB(rhoUz_,i,j,k,globalBLK)/State_VGB(rho_,i,j,k,globalBLK)
        E_D(x_) = FullBy*Uz - FullBz*Uy
        E_D(y_) = FullBz*Ux - FullBx*Uz
        E_D(z_) = FullBx*Uy - FullBy*Ux

        ! Calculate divergence of electric field 
        DivE = vInv_CB(i,j,k,globalBLK)*&
             (EDotFA_X(i+1,j,k)-EDotFA_X(i,j,k)+&
             EDotFA_Y(i,j+1,k) -EDotFA_Y(i,j,k)+&
             EDotFA_Z(i,j,k+1) -EDotFA_Z(i,j,k))

        Source_VC(rhoUx_:rhoUz_,i,j,k) = Source_VC(rhoUx_:rhoUz_,i,j,k) &
             + Coef*DivE*E_D 
     end do; end do; end do
  end if                                                 !^CFG END BORISCORR

  ! These source terms apply to all the fluids
  do iFluid = 1, nFluid
     call select_fluid
     ! Add gravity and/or centrifugal force
     if(UseGravity.or.UseRotatingFrame) then
        Source_VC(iRhoUx,:,:,:) = Source_VC(iRhoUx,:,:,:) + &
             State_VGB(iRho,1:nI,1:nJ,1:nK,globalBLK)* &
             fbody_x_BLK(:,:,:,globalBLK)
        Source_VC(iRhoUy,:,:,:) = Source_VC(iRhoUy,:,:,:) + &
             State_VGB(iRho,1:nI,1:nJ,1:nK,globalBLK)* &
             fbody_y_BLK(:,:,:,globalBLK)
        Source_VC(iRhoUz,:,:,:) = Source_VC(iRhoUz,:,:,:) + &
             State_VGB(iRho,1:nI,1:nJ,1:nK,globalBLK)* &
             fbody_z_BLK(:,:,:,globalBLK)
        Source_VC(Energy_,:,:,:) = Source_VC(Energy_,:,:,:) + &
             (State_VGB(iRhoUx,1:nI,1:nJ,1:nK,globalBLK)* &
             fbody_x_BLK(:,:,:,globalBLK) + & 
             State_VGB(iRhoUy,1:nI,1:nJ,1:nK,globalBLK)* &
             fbody_y_BLK(:,:,:,globalBLK) + &
             State_VGB(iRhoUz,1:nI,1:nJ,1:nK,globalBLK)* &
             fbody_z_BLK(:,:,:,globalBLK)) 
     end if

     ! Add Coriolis forces
     if(UseRotatingFrame)then
        select case(TypeCoordSystem)
        case('HGC','HGR')
           ! This is a special case since Omega is parallel with the Z axis
           do k=1,nK; do j=1,nJ; do i=1,nI
              Source_VC(iRhoUx,i,j,k) = Source_VC(iRhoUx,i,j,k) + &
                   cTwo*OmegaBody*State_VGB(iRhoUy,i,j,k,globalBLK)
              Source_VC(iRhoUy,i,j,k) = Source_VC(iRhoUy,i,j,k) - &
                   cTwo*OmegaBody*State_VGB(iRhoUx,i,j,k,globalBLK)
           end do; end do; end do
        case default
           call stop_mpi('ERROR in calc_sources: '// &
                'Coriolis force is not implemented for '// &
                'TypeCoordSystem=',TypeCoordSystem)
        end select
     end if
  end do

  if(UseMultiIon .and. .not.UseUserSource)then

     ! Add source term n_s*(- u_+ - w_H + u_s )xB for multi-ions
     ! where u_+ is the number density weighted average ion velocity,
     ! and w_H = -J/(e n_e) is the Hall velocity. Here
     ! e is the electron charge and n_e is the electron number density.

     InvCharge = 1.0/ElectronCharge
     iFirstIons = 1
     if(TypeFluid_I(1) == 'ion')iFirstIons = 2

     do k=1,nK; do j=1,nJ; do i=1,nI
        ! Extract conservative variables
        State_V = State_VGB(:,i,j,k,globalBLK)

        if(TypeFluid_I(1) == 'ion')then
           State_V(Rho_) = State_V(Rho_) &
                - sum(State_V(iRhoIon_I(2:nIonFluid)))
           State_V(RhoUx_) = State_V(RhoUx_) &
                - sum(State_V(iRhoUxIon_I(2:nIonFluid)))
           State_V(RhoUy_) = State_V(RhoUy_) &
                - sum(State_V(iRhoUyIon_I(2:nIonFluid)))
           State_V(RhoUz_) = State_V(RhoUz_) &
                - sum(State_V(iRhoUzIon_I(2:nIonFluid)))
        end if

        ! Total magnetic field
        FullB_D = State_V(Bx_:Bz_) + (/ &
             B0xCell_BLK(i,j,k,globalBLK),&
             B0yCell_BLK(i,j,k,globalBLK),&
             B0zCell_BLK(i,j,k,globalBLK) /)

        ! calculate number densities
        NumDens_I  = State_V(iRhoIon_I) / MassFluid_I(1:nIonFluid)
        InvNumDens = 1.0/sum(NumDens_I)

        InvRho_I = 1.0/State_V(iRhoIon_I)
        Ux_I  = InvRho_I*State_V(iUxIon_I)
        Uy_I  = InvRho_I*State_V(iUyIon_I)
        Uz_I  = InvRho_I*State_V(iUzIon_I)

        ! calculate the average positive charge velocity
        uPlus_D(x_) = InvNumDens* sum(NumDens_I*Ux_I)
        uPlus_D(y_) = InvNumDens* sum(NumDens_I*Uy_I)
        uPlus_D(z_) = InvNumDens* sum(NumDens_I*Uz_I)

        ! Add the Hall velocity -J/(e n)
        if(index(Test_String,'newj') > 0)then
           Current_D = vInv_CB(i,j,k,globalBLK)*&
                ( bCrossArea_DX(:,i+1,j,k) - bCrossArea_DX(:,i,j,k) &
                + bCrossArea_DY(:,i,j+1,k) - bCrossArea_DY(:,i,j,k) &
                + bCrossArea_DZ(:,i,j,k+1) - bCrossArea_DZ(:,i,j,k))

           !if(i==iTest.and.j==jTest.and.k==kTest.and.GlobalBlk==BlkTest)then
           !   write(*,*)'Current_D=',Current_D
           !   write(*,*)'bCrossArea_DX(i  )=',bCrossArea_DX(:,i  ,j,k)
           !   write(*,*)'bCrossArea_DX(i+1)=',bCrossArea_DX(:,i+1,j,k)
           !   write(*,*)'bCrossArea_DY(j  )=',bCrossArea_DY(:,i,j  ,k)
           !   write(*,*)'bCrossArea_DY(j+1)=',bCrossArea_DY(:,i,j+1,k)
           !   write(*,*)'bCrossArea_DZ(k  )=',bCrossArea_DZ(:,i,j,k  )
           !   write(*,*)'bCrossArea_DZ(k+1)=',bCrossArea_DZ(:,i,j,k+1)
           !end if
        else
           call get_current(i,j,k,GlobalBlk,Current_D)
           !if(i==iTest.and.j==jTest.and.k==kTest.and.GlobalBlk==BlkTest) &
           !     write(*,*)'Current_D=',Current_D
        end if
        uPlusHallU_D = uPlus_D - InvNumDens*InvCharge*Current_D

        ! Calculate the source term for all the ion fluids
        do iFluid = iFirstIons, nIonFluid
           uIon_D = (/ Ux_I(iFLuid),  Uy_I(iFluid), Uz_I(iFluid) /)
           u_D    = uIon_D - uPlusHallU_D

           Force_D = &
                ElectronCharge*NumDens_I(iFluid)*cross_product(u_D, FullB_D) 

           !if(i==iTest.and.j==jTest.and.k==kTest.and.GlobalBlk==BlkTest)then
           !   write(*,*)'iFluid, uIon_D =',iFluid, uIon_D
           !   write(*,*)'iFluid, u_D    =',iFluid, u_D
           !   write(*,*)'iFluid, Force_D=',iFluid, Force_D
           !end if

           Source_VC(iRhoUx_I(iFluid):iRhoUz_I(iFluid),i,j,k) = &
                Source_VC(iRhoUx_I(iFluid):iRhoUz_I(iFluid),i,j,k) + Force_D

           Source_VC(Energy_-1+iFluid,i,j,k) = &
                Source_VC(Energy_-1+iFluid,i,j,k) + sum(Force_D*uIon_D)

        end do
     end do; end do; end do
  end if

  if(UseHallResist .and. HallHyperFactor > 0.0) &
       call calc_hyper_resistivity(globalBLK)

  if(UseUserSource) call user_calc_sources

contains
  !===========================================================================
  subroutine calc_divb_source

    ! Variables needed for div B source terms
    real:: DxInvHalf, DyInvHalf, DzInvHalf, DivBInternal
    real:: dB1nEast, dB1nWest, dB1nSouth, dB1nNorth, dB1nTop, dB1nBot

    DxInvHalf = 0.5/Dx_BLK(GlobalBlk)
    DyInvHalf = 0.5/Dy_BLK(GlobalBlk)
    DzInvHalf = 0.5/Dz_BLK(GlobalBlk)

    do k=1,nK; do j=1,nJ; do i=1,nI
       dB1nEast = DxInvHalf*&
            (RightState_VX(Bx_,i,j,k)-LeftState_VX(Bx_,i,j,k))

       dB1nWest = DxInvHalf*&
            (RightState_VX(Bx_,i+1,j,k)-LeftState_VX(Bx_,i+1,j,k))

       dB1nSouth = DyInvHalf* &
            (RightState_VY(By_,i,j,k)-LeftState_VY(By_,i,j,k))

       dB1nNorth = DyInvHalf* &
            (RightState_VY(By_,i,j+1,k)-LeftState_VY(By_,i,j+1,k))

       dB1nBot = DzInvHalf * &
            (RightState_VZ(Bz_,i,j,k)-LeftState_VZ(Bz_,i,j,k))

       dB1nTop = DzInvHalf * &
            (RightState_VZ(Bz_,i,j,k+1)-LeftState_VZ(Bz_,i,j,k+1))

       DivBInternal = 2*(&
            DxInvHalf*(LeftState_VX(Bx_,i+1,j,k) -RightState_VX(Bx_,i,j,k))+&
            DyInvHalf*(LeftState_VY(By_,i,j+1,k) -RightState_VY(By_,i,j,k))+&
            DzInvHalf*(LeftState_VZ(Bz_,i,j,k+1) -RightState_VZ(Bz_,i,j,k)))

       DivB1_GB(i,j,k,globalBLK)  = DivBInternal + &
            dB1nEast + dB1nWest + dB1nSouth + dB1nNorth + dB1nTop + dB1nBot

       if(TypeFluid_I(1) /= 'ion') CYCLE

       Source_VC(rhoUx_,i,j,k) = Source_VC(rhoUx_,i,j,k) &
            -B0xFace_x_BLK(i,j,k,globalBLK)*dB1nEast
       Source_VC(rhoUy_,i,j,k) = Source_VC(rhoUy_,i,j,k) &
            -B0yFace_x_BLK(i,j,k,globalBLK)*dB1nEast
       Source_VC(rhoUz_,i,j,k) = Source_VC(rhoUz_,i,j,k) &
            -B0zFace_x_BLK(i,j,k,globalBLK)*dB1nEast

       Source_VC(rhoUx_,i,j,k) = Source_VC(rhoUx_,i,j,k)&
            -B0xFace_x_BLK(i+1,j,k,globalBLK)*dB1nWest
       Source_VC(rhoUy_,i,j,k) = Source_VC(rhoUy_,i,j,k)&
            -B0yFace_x_BLK(i+1,j,k,globalBLK)*dB1nWest
       Source_VC(rhoUz_,i,j,k) = Source_VC(rhoUz_,i,j,k)&
            -B0zFace_x_BLK(i+1,j,k,globalBLK)*dB1nWest

       Source_VC(rhoUx_,i,j,k) = Source_VC(rhoUx_,i,j,k)&
            -B0xFace_y_BLK(i,j,k,globalBLK)*dB1nSouth
       Source_VC(rhoUy_,i,j,k) = Source_VC(rhoUy_,i,j,k)&
            -B0yFace_y_BLK(i,j,k,globalBLK)*dB1nSouth
       Source_VC(rhoUz_,i,j,k) = Source_VC(rhoUz_,i,j,k)&
            -B0zFace_y_BLK(i,j,k,globalBLK)*dB1nSouth

       Source_VC(rhoUx_,i,j,k) = Source_VC(rhoUx_,i,j,k)&
            -B0xFace_y_BLK(i,j+1,k,globalBLK)*dB1nNorth
       Source_VC(rhoUy_,i,j,k) = Source_VC(rhoUy_,i,j,k)&
            -B0yFace_y_BLK(i,j+1,k,globalBLK)*dB1nNorth
       Source_VC(rhoUz_,i,j,k) = Source_VC(rhoUz_,i,j,k)&
            -B0zFace_y_BLK(i,j+1,k,globalBLK)*dB1nNorth

       Source_VC(rhoUx_,i,j,k) = Source_VC(rhoUx_,i,j,k)&
            -B0xFace_z_BLK(i,j,k,globalBLK)*dB1nBot
       Source_VC(rhoUy_,i,j,k) = Source_VC(rhoUy_,i,j,k)&
            -B0yFace_z_BLK(i,j,k,globalBLK)*dB1nBot
       Source_VC(rhoUz_,i,j,k) = Source_VC(rhoUz_,i,j,k)&
            -B0zFace_z_BLK(i,j,k,globalBLK)*dB1nBot

       Source_VC(rhoUx_,i,j,k) = Source_VC(rhoUx_,i,j,k)&
            -B0xFace_z_BLK(i,j,k+1,globalBLK)*dB1nTop
       Source_VC(rhoUy_,i,j,k) = Source_VC(rhoUy_,i,j,k)&
            -B0yFace_z_BLK(i,j,k+1,globalBLK)*dB1nTop
       Source_VC(rhoUz_,i,j,k) = Source_VC(rhoUz_,i,j,k)&
            -B0zFace_z_BLK(i,j,k+1,globalBLK)*dB1nTop

       Source_VC(rhoUx_,i,j,k) = Source_VC(rhoUx_,i,j,k)-DivBInternal*&
            B0xCell_BLK(i,j,k,globalBLK)
       Source_VC(rhoUy_,i,j,k) = Source_VC(rhoUy_,i,j,k)-DivBInternal*&
            B0yCell_BLK(i,j,k,globalBLK)
       Source_VC(rhoUz_,i,j,k) = Source_VC(rhoUz_,i,j,k)-DivBInternal*&
            B0zCell_BLK(i,j,k,globalBLK)               

    end do; end do; end do

  end subroutine calc_divb_source
  !===========================================================================
  !^CFG IF COVARIANT BEGIN
  subroutine calc_divb_source_covar

    use ModCovariant, ONLY: FaceAreaI_DFB, FaceAreaJ_DFB, FaceAreaK_DFB
    real :: FaceArea_D(3), vInvHalf
    real :: B1nJumpL, B1nJumpR, DivBInternal_C(1:nI,1:nJ,1:nK)
    integer :: i,j,k

    do k=1,nK; do j=1,nJ; do i=1,nI
       VInvHalf=vInv_CB(i,j,k,globalBLK)*cHalf
       FaceArea_D=FaceAreaI_DFB(:,i,j,k,globalBLK)
       B1nJumpL =VInvHalf*&
            (FaceArea_D(1)*(RightState_VX(Bx_,i,j,k)-LeftState_VX(Bx_,i,j,k))+&
            FaceArea_D(2)*(RightState_VX(By_,i,j,k)-LeftState_VX(By_,i,j,k))+&
            FaceArea_D(3)*(RightState_VX(Bz_,i,j,k)-LeftState_VX(Bz_,i,j,k)))
       DivBInternal_C(i,j,k)=&
            -(FaceArea_D(1)*RightState_VX(Bx_,i,j,k)+&
            FaceArea_D(2)*RightState_VX(By_,i,j,k)+&
            FaceArea_D(3)*RightState_VX(Bz_,i,j,k))

       FaceArea_D=FaceAreaI_DFB(:,i+1,j,k,globalBLK)
       B1nJumpR =  VInvHalf*&
            (FaceArea_D(1)*(RightState_VX(Bx_,i+1,j,k)-LeftState_VX(Bx_,i+1,j,k))+&
            FaceArea_D(2)*(RightState_VX(By_,i+1,j,k)-LeftState_VX(By_,i+1,j,k))+&
            FaceArea_D(3)*(RightState_VX(Bz_,i+1,j,k)-LeftState_VX(Bz_,i+1,j,k)))
       DivBInternal_C(i,j,k)=DivBInternal_C(i,j,k)+&
            (FaceArea_D(1)*LeftState_VX(Bx_,i+1,j,k)+&
            FaceArea_D(2)*LeftState_VX(By_,i+1,j,k)+&
            FaceArea_D(3)*LeftState_VX(Bz_,i+1,j,k))

       DivB1_GB(i,j,k,globalBLK)  = B1nJumpL + B1nJumpR

       if(TypeFluid_I(1) /= 'ion') CYCLE

       Source_VC(rhoUx_,i,j,k) = Source_VC(rhoUx_,i,j,k) &
            -B0xFace_x_BLK(i,j,k,globalBLK)*B1nJumpL
       Source_VC(rhoUy_,i,j,k) = Source_VC(rhoUy_,i,j,k) &
            -B0yFace_x_BLK(i,j,k,globalBLK)*B1nJumpL
       Source_VC(rhoUz_,i,j,k) = Source_VC(rhoUz_,i,j,k) &
            -B0zFace_x_BLK(i,j,k,globalBLK)*B1nJumpL

       Source_VC(rhoUx_,i,j,k) = Source_VC(rhoUx_,i,j,k)&
            -B0xFace_x_BLK(i+1,j,k,globalBLK)*B1nJumpR
       Source_VC(rhoUy_,i,j,k) = Source_VC(rhoUy_,i,j,k)&
            -B0yFace_x_BLK(i+1,j,k,globalBLK)*B1nJumpR
       Source_VC(rhoUz_,i,j,k) = Source_VC(rhoUz_,i,j,k)&
            -B0zFace_x_BLK(i+1,j,k,globalBLK)*B1nJumpR
    end do; end do; end do

    do k=1,nK; do j=1,nJ; do i=1,nI 
       VInvHalf=vInv_CB(i,j,k,globalBLK)*cHalf
       FaceArea_D=FaceAreaJ_DFB(:,i,j,k,globalBLK)
       B1nJumpL = VInvHalf*&
            (FaceArea_D(1)*(RightState_VY(Bx_,i,j,k)-LeftState_VY(Bx_,i,j,k))+&
            FaceArea_D(2)*(RightState_VY(By_,i,j,k)-LeftState_VY(By_,i,j,k))+&
            FaceArea_D(3)*(RightState_VY(Bz_,i,j,k)-LeftState_VY(Bz_,i,j,k)))
       DivBInternal_C(i,j,k)=DivBInternal_C(i,j,k)-&
            (FaceArea_D(1)*RightState_VY(Bx_,i,j,k)+&
            FaceArea_D(2)*RightState_VY(By_,i,j,k)+&
            FaceArea_D(3)*RightState_VY(Bz_,i,j,k))

       FaceArea_D=FaceAreaJ_DFB(:,i,j+1,k,globalBLK)
       B1nJumpR = VInvHalf*&
            (FaceArea_D(1)*(RightState_VY(Bx_,i,j+1,k)-LeftState_VY(Bx_,i,j+1,k))+&
            FaceArea_D(2)*(RightState_VY(By_,i,j+1,k)-LeftState_VY(By_,i,j+1,k))+&
            FaceArea_D(3)*(RightState_VY(Bz_,i,j+1,k)-LeftState_VY(Bz_,i,j+1,k)))
       DivBInternal_C(i,j,k)=DivBInternal_C(i,j,k)+&
            (FaceArea_D(1)*LeftState_VY(Bx_,i,j+1,k)+&
            FaceArea_D(2)*LeftState_VY(By_,i,j+1,k)+&
            FaceArea_D(3)*LeftState_VY(Bz_,i,j+1,k))

       DivB1_GB(i,j,k,globalBLK)  = DivB1_GB(i,j,k,globalBLK) &
            + B1nJumpL + B1nJumpR

       if(TypeFluid_I(1) /= 'ion') CYCLE

       Source_VC(rhoUx_,i,j,k) = Source_VC(rhoUx_,i,j,k)&
            -B0xFace_y_BLK(i,j,k,globalBLK)*B1nJumpL
       Source_VC(rhoUy_,i,j,k) = Source_VC(rhoUy_,i,j,k)&
            -B0yFace_y_BLK(i,j,k,globalBLK)*B1nJumpL
       Source_VC(rhoUz_,i,j,k) = Source_VC(rhoUz_,i,j,k)&
            -B0zFace_y_BLK(i,j,k,globalBLK)*B1nJumpL

       Source_VC(rhoUx_,i,j,k) = Source_VC(rhoUx_,i,j,k)&
            -B0xFace_y_BLK(i,j+1,k,globalBLK)*B1nJumpR
       Source_VC(rhoUy_,i,j,k) = Source_VC(rhoUy_,i,j,k)&
            -B0yFace_y_BLK(i,j+1,k,globalBLK)*B1nJumpR
       Source_VC(rhoUz_,i,j,k) = Source_VC(rhoUz_,i,j,k)&
            -B0zFace_y_BLK(i,j+1,k,globalBLK)*B1nJumpR

    end do; end do; end do

    do k=1,nK; do j=1,nJ; do i=1,nI 
       VInvHalf=vInv_CB(i,j,k,globalBLK)*cHalf
       FaceArea_D=FaceAreaK_DFB(:,i,j,k,globalBLK)
       B1nJumpL = VInvHalf*&
            (FaceArea_D(1)*(RightState_VZ(Bx_,i,j,k)-LeftState_VZ(Bx_,i,j,k))+&
            FaceArea_D(2)*(RightState_VZ(By_,i,j,k)-LeftState_VZ(By_,i,j,k))+&
            FaceArea_D(3)*(RightState_VZ(Bz_,i,j,k)-LeftState_VZ(Bz_,i,j,k)))
       DivBInternal_C(i,j,k)=DivBInternal_C(i,j,k)-&
            (FaceArea_D(1)*RightState_VZ(Bx_,i,j,k)+&
            FaceArea_D(2)*RightState_VZ(By_,i,j,k)+&
            FaceArea_D(3)*RightState_VZ(Bz_,i,j,k))

       FaceArea_D=FaceAreaK_DFB(:,i,j,k+1,globalBLK)
       B1nJumpR = VInvHalf*&
            (FaceArea_D(1)*(RightState_VZ(Bx_,i,j,k+1)-LeftState_VZ(Bx_,i,j,k+1))+&
            FaceArea_D(2)*(RightState_VZ(By_,i,j,k+1)-LeftState_VZ(By_,i,j,k+1))+&
            FaceArea_D(3)*(RightState_VZ(Bz_,i,j,k+1)-LeftState_VZ(Bz_,i,j,k+1)))
       DivBInternal_C(i,j,k)=(DivBInternal_C(i,j,k)+&
            (FaceArea_D(1)*LeftState_VZ(Bx_,i,j,k+1)+&
            FaceArea_D(2)*LeftState_VZ(By_,i,j,k+1)+&
            FaceArea_D(3)*LeftState_VZ(Bz_,i,j,k+1)))*&
            vInv_CB(i,j,k,globalBLK)

       DivB1_GB(i,j,k,globalBLK)  = DivB1_GB(i,j,k,globalBLK) &
            + B1nJumpL + B1nJumpR

       if(TypeFluid_I(1) /= 'ion') CYCLE

       Source_VC(rhoUx_,i,j,k) = Source_VC(rhoUx_,i,j,k)&
            -B0xFace_z_BLK(i,j,k,globalBLK)*B1nJumpL
       Source_VC(rhoUy_,i,j,k) = Source_VC(rhoUy_,i,j,k)&
            -B0yFace_z_BLK(i,j,k,globalBLK)*B1nJumpL
       Source_VC(rhoUz_,i,j,k) = Source_VC(rhoUz_,i,j,k)&
            -B0zFace_z_BLK(i,j,k,globalBLK)*B1nJumpL

       Source_VC(rhoUx_,i,j,k) = Source_VC(rhoUx_,i,j,k)&
            -B0xFace_z_BLK(i,j,k+1,globalBLK)*B1nJumpR
       Source_VC(rhoUy_,i,j,k) = Source_VC(rhoUy_,i,j,k)&
            -B0yFace_z_BLK(i,j,k+1,globalBLK)*B1nJumpR
       Source_VC(rhoUz_,i,j,k) = Source_VC(rhoUz_,i,j,k)&
            -B0zFace_z_BLK(i,j,k+1,globalBLK)*B1nJumpR
    end do; end do; end do

    do k=1,nK; do j=1,nJ; do i=1,nI 
       DivB1_GB(i,j,k,globalBLK)  = DivB1_GB(i,j,k,globalBLK)+&
            DivBInternal_C(i,j,k)

       if(TypeFluid_I(1) /= 'ion') CYCLE

       Source_VC(rhoUx_,i,j,k) = Source_VC(rhoUx_,i,j,k) &
            -DivBInternal_C(i,j,k)*B0xCell_BLK(i,j,k,globalBLK)
       Source_VC(rhoUy_,i,j,k) = Source_VC(rhoUy_,i,j,k) &
            -DivBInternal_C(i,j,k)*B0yCell_BLK(i,j,k,globalBLK)
       Source_VC(rhoUz_,i,j,k) = Source_VC(rhoUz_,i,j,k) &
            -DivBInternal_C(i,j,k)*B0zCell_BLK(i,j,k,globalBLK)               
    end do; end do; end do

  end subroutine calc_divb_source_covar
  !===========================================================================
  !^CFG END COVARIANT
  subroutine write_source(String)
    character(len=*) :: String
    write(*,'(a,a,es13.5)',advance='no') &
         String," S=",Source_VC(VarTest,iTest,jTest,kTest) 
  end subroutine write_source

end subroutine calc_sources

!=============================================================================

subroutine calc_divb
  use ModMain, ONLY : &              
       UseDivbDiffusion,&            !^CFG IF DIVBDIFFUSE
       nI,nJ,nK,globalBLK,test_string
  use ModVarIndexes, ONLY : Bx_,By_,Bz_
  use ModAdvance, ONLY : DivB1_GB,State_VGB, &
       LeftState_VX,RightState_VX,&
       LeftState_VY,RightState_VY,&
       LeftState_VZ,RightState_VZ
  use ModNumConst
  use ModGeometry, ONLY : dx_BLK,dy_BLK,dz_BLK 
  implicit none

  !\
  ! Calculate div B for a block and store result into SdivB
  !/
  if(index(test_string,'DIVB_CD')>0)then
     ! Use central differencing if test string contains DIVB_CD
     DivB1_GB(1:nI,1:nJ,1:nK,globalBLK) = cHalf*(&
          (State_VGB(Bx_, 2:nI+1, 1:nJ  , 1:nK  ,globalBLK)- &
          State_VGB(Bx_, 0:nI-1, 1:nJ  , 1:nK  ,globalBLK))/dx_BLK(globalBLK)+&
          (State_VGB(By_, 1:nI  , 2:nJ+1, 1:nK  ,globalBLK)- &
          State_VGB(By_, 1:nI  , 0:nJ-1, 1:nK  ,globalBLK))/dy_BLK(globalBLK)+&
          (State_VGB(Bz_, 1:nI  , 1:nJ  , 2:nK+1,globalBLK)- &
          State_VGB(Bz_, 1:nI  , 1:nJ  , 0:nK-1,globalBLK))/dz_BLK(globalBLK))
  else
     ! Compute divB using averaged and conservatively corrected 
     ! left and right values
     
     DivB1_GB(1:nI,1:nJ,1:nK,globalBLK) = &
          cHalf *( &
          ((LeftState_VX(Bx_,2:nI+1,1:nJ,1:nK)+    &
            RightState_VX(Bx_,2:nI+1,1:nJ,1:nK))-   &
           (LeftState_VX(Bx_,1:nI,1:nJ,1:nK)+    &
            RightState_VX(Bx_,1:nI,1:nJ,1:nK))  )/dx_BLK(globalBLK)+  &
          ((LeftState_VY(By_,1:nI,2:nJ+1,1:nK)+    &
            RightState_VY(By_,1:nI,2:nJ+1,1:nK))-   &
           (LeftState_VY(By_,1:nI,1:nJ,1:nK)+    &
            RightState_VY(By_,1:nI,1:nJ,1:nK))  )/dy_BLK(globalBLK)+  &
          ((LeftState_VZ(Bz_,1:nI,1:nJ,2:nK+1)+    &
            RightState_VZ(Bz_,1:nI,1:nJ,2:nK+1))-   &
           (LeftState_VZ(Bz_,1:nI,1:nJ,1:nK)+    &
            RightState_VZ(Bz_,1:nI,1:nJ,1:nK))  )/dz_BLK(globalBLK) )
  endif                                         
end subroutine calc_divb

!==============================================================================

subroutine get_current(i,j,k,iBlock,Current_D)

  ! Calculate the current in a cell of a block

  use ModAdvance,  ONLY: State_VGB, Bx_, By_, Bz_
  use ModGeometry, ONLY: True_Cell, Dx_BLK, Dy_BLK, Dz_BLK
  use ModCovariant,ONLY: UseCovariant                !^CFG IF COVARIANT

  implicit none
  integer, intent(in) :: i,j,k,iBlock
  real,    intent(out):: Current_D(3)

  real :: DxInvHalf, DyInvHalf, DzInvHalf
  !----------------------------------------------------------------------------

  ! Exclude cells next to the body because they produce incorrect currents
  if(.not.all(True_Cell(i-1:i+1,j-1:j+1,k-1:k+1,iBlock)))then
     Current_D = 0.0
     RETURN
  endif

  if(UseCovariant)then                               !^CFG IF COVARIANT BEGIN
     call covariant_curlb(i,j,k,iBlock,Current_D,.true.)
     RETURN
  end if                                             !^CFG END COVARIANT

  DxInvHalf = 0.5/Dx_BLK(iBlock)
  DyInvHalf = 0.5/Dy_BLK(iBlock)
  DzInvHalf = 0.5/Dz_BLK(iBlock)

  Current_D(1) = &
       (State_VGB(Bz_,i,j+1,k,iBlock) &
       -State_VGB(Bz_,i,j-1,k,iBlock))*DyInvHalf - &
       (State_VGB(By_,i,j,k+1,iBlock) &
       -State_VGB(By_,i,j,k-1,iBlock))*DzInvHalf

  Current_D(2) = &
       (State_VGB(Bx_,i,j,k+1,iBlock) &
       -State_VGB(Bx_,i,j,k-1,iBlock))*DzInvHalf- &
       (State_VGB(Bz_,i+1,j,k,iBlock) &
       -State_VGB(Bz_,i-1,j,k,iBlock))*DxInvHalf

  Current_D(3) = &
       (State_VGB(By_,i+1,j,k,iBlock) &
       -State_VGB(By_,i-1,j,k,iBlock))*DxInvHalf- &
       (State_VGB(Bx_,i,j+1,k,iBlock) &
       -State_VGB(Bx_,i,j-1,k,iBlock))*DyInvHalf

end subroutine get_current
