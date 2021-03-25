!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModFaceFluxGpu

  use ModVarIndexes
  use ModFaceFlux, ONLY: print_face_values
  use ModAdvance, ONLY: nGang, nFlux, State_VGB, &
       Flux_VXI, VdtFace_XI, uDotArea_XII, &
       Flux_VYI, VdtFace_YI, uDotArea_YII, &
       Flux_VZI, VdtFace_ZI, uDotArea_ZII   
  use ModPhysics, ONLY: Gamma, InvGammaMinus1
  use ModMain, ONLY: SpeedHyp
  use BATL_lib, ONLY: test_start, iTest, jTest, kTest, &
       nDim, nI, nJ, nK, x_, y_, z_, CellFace_DB
  implicit none

contains
  !============================================================================
  subroutine calc_face_flux_gpu(DoResChangeOnly, iBlock)
    !$acc routine vector

    logical, intent(in) :: DoResChangeOnly
    integer, intent(in) :: iBlock

    real :: Area, NormalX, NormalY, NormalZ, Un, Cmax
    real :: StateLeft_V(nVar), StateRight_V(nVar), State_V(nVar)
    real :: StateLeftCons_V(nFlux), StateRightCons_V(nFlux)
    real :: FluxLeft_V(nFlux), FluxRight_V(nFlux)

    integer:: i, j, k, iGang

    logical:: DoTest, DoTestCell
    character(len=*), parameter:: NameSub = 'calc_face_flux_gpu'
    !--------------------------------------------------------------------------
    if(DoResChangeOnly) RETURN

#ifdef OPENACC
    iGang = iBlock
#else
    iGang = 1
    call test_start(NameSub, DoTest, iBlock)
    if(DoTest)call print_face_values
#endif

    !$acc loop vector collapse(3) &
    !$acc private(Area, NormalX, NormalY, NormalZ, Un, Cmax, &
    !$acc         StateLeft_V, StateRight_V, State_V, &
    !$acc         StateLeftCons_V, StateRightCons_V, FluxLeft_V, FluxRight_V)
    do k = 1, nK; do j = 1, nJ; do i = 1, nI+1

#ifndef OPENACC
       DoTestCell = DoTest .and. (i == iTest .or. i == iTest+1) .and. &
            j == jTest .and. k == kTest
#endif

       ! if(IsCartesianGrid)then
       Area = CellFace_DB(x_,iBlock)
       NormalX = 1.0
       NormalY = 0.0
       NormalZ = 0.0
       ! end if

       ! First order left state of primitive variables
       StateLeft_V  = State_VGB(:,i-1,j,k,iBlock)
       StateLeft_V(Ux_:Uz_) = StateLeft_V(Ux_:Uz_)/StateLeft_V(Rho_)

       ! First order right state of primitive variables
       StateRight_V  = State_VGB(:,i,j,k,iBlock)
       StateRight_V(Ux_:Uz_) = StateRight_V(Ux_:Uz_)/StateRight_V(Rho_)
       
       ! average state
       State_V = 0.5*(StateLeft_V + StateRight_V)

       call get_speed_max(State_V, NormalX, NormalY, NormalZ, &
            Un, Cmax)
       call get_physical_flux(StateLeft_V, NormalX, NormalY, NormalZ, &
            StateLeftCons_V, FluxLeft_V)
       call get_physical_flux(StateRight_V, NormalX, NormalY, NormalZ, &
            StateRightCons_V, FluxRight_V)

       ! Rusanov flux
       Flux_VXI(:,i,j,k,iGang) = &
            Area*(0.5*(FluxLeft_V + FluxRight_V) &
            + 0.5*Cmax*(StateLeftCons_V - StateRightCons_V))

       ! For time step
       VdtFace_XI(i,j,k,iGang) = Cmax*Area

       ! For div U source term
       uDotArea_XII(i,j,k,:,iGang) = Un*Area

#ifndef OPENACC
       if(DoTestCell) call write_test_info(1, Flux_VXI(:,i,j,k,1))
#endif

    end do; end do; end do

    if(nDim == 1) RETURN

    !$acc loop vector collapse(3) &
    !$acc private(Area, NormalX, NormalY, NormalZ, Un, Cmax, &
    !$acc         StateLeft_V, StateRight_V, State_V, &
    !$acc         StateLeftCons_V, StateRightCons_V, FluxLeft_V, FluxRight_V)
    do k = 1, nK; do j = 1, nJ+1; do i = 1, nI

#ifndef OPENACC
       DoTestCell = DoTest .and. i == iTest .and. &
            (j == jTest .or. j == jTest+1) .and. k == kTest
#endif

       ! if(IsCartesianGrid)then
       Area = CellFace_DB(y_,iBlock)
       NormalX = 0.0
       NormalY = 1.0
       NormalZ = 0.0
       ! end if

       ! First order left state of primitive variables
       StateLeft_V  = State_VGB(:,i,j-1,k,iBlock)
       StateLeft_V(Ux_:Uz_) = StateLeft_V(Ux_:Uz_)/StateLeft_V(Rho_)

       ! First order right state of primitive variables
       StateRight_V  = State_VGB(:,i,j,k,iBlock)
       StateRight_V(Ux_:Uz_) = StateRight_V(Ux_:Uz_)/StateRight_V(Rho_)

       ! Average state
       State_V = 0.5*(StateLeft_V + StateRight_V)

       call get_speed_max(State_V, NormalX, NormalY, NormalZ, &
            Un, Cmax)
       call get_physical_flux(StateLeft_V, NormalX, NormalY, NormalZ, &
            StateLeftCons_V, FluxLeft_V)
       call get_physical_flux(StateRight_V, NormalX, NormalY, NormalZ, &
            StateRightCons_V, FluxRight_V)

       ! Rusanov flux
       Flux_VYI(:,i,j,k,iGang) = &
            Area*(0.5*(FluxLeft_V + FluxRight_V) &
            + 0.5*Cmax*(StateLeftCons_V - StateRightCons_V))

       ! For time step
       VdtFace_YI(i,j,k,iGang) = Cmax*Area

       ! For div U source term
       uDotArea_YII(i,j,k,:,iGang) = Un*Area

#ifndef OPENACC
       if(DoTestCell) call write_test_info(2, Flux_VYI(:,i,j,k,1))
#endif

    end do; end do; end do

    if(nDim == 2) RETURN

    !$acc loop vector collapse(3) &
    !$acc private(Area, NormalX, NormalY, NormalZ, Un, Cmax, &
    !$acc         StateLeft_V, StateRight_V, State_V, &
    !$acc         StateLeftCons_V, StateRightCons_V, FluxLeft_V, FluxRight_V)
    do k = 1, nK+1; do j = 1, nJ; do i = 1, nI

#ifndef OPENACC
       DoTestCell = DoTest .and. i == iTest .and. &
            j == jTest .and. (k == kTest .or. k == kTest+1)
#endif

       ! if(IsCartesianGrid)then
       Area = CellFace_DB(z_,iBlock)
       NormalX = 0.0
       NormalY = 0.0
       NormalZ = 1.0
       ! end if

       ! First order left state of primitive variables
       StateLeft_V  = State_VGB(:,i,j,k-1,iBlock)
       StateLeft_V(Ux_:Uz_) = StateLeft_V(Ux_:Uz_)/StateLeft_V(Rho_)

       ! First order right state of primitive variables
       StateRight_V  = State_VGB(:,i,j,k,iBlock)
       StateRight_V(Ux_:Uz_) = StateRight_V(Ux_:Uz_)/StateRight_V(Rho_)
       
       ! average state
       State_V = 0.5*(StateLeft_V + StateRight_V)

       call get_speed_max(State_V, NormalX, NormalY, NormalZ, &
            Un, Cmax)
       call get_physical_flux(StateLeft_V, NormalX, NormalY, NormalZ, &
            StateLeftCons_V, FluxLeft_V)
       call get_physical_flux(StateRight_V, NormalX, NormalY, NormalZ, &
            StateRightCons_V, FluxRight_V)

       ! Rusanov flux
       Flux_VZI(:,i,j,k,iGang) = &
            Area*(0.5*(FluxLeft_V + FluxRight_V) &
            + 0.5*Cmax*(StateLeftCons_V - StateRightCons_V))

       ! For time step
       VdtFace_ZI(i,j,k,iGang) = Cmax*Area

       ! For div U source term
       uDotArea_ZII(i,j,k,:,iGang) = Un*Area

#ifndef OPENACC
       if(DoTest .and. i == iTest .and. j == jTest .and. &
            (k == kTest .or. k == kTest+1)) &
            call write_test_info(3, Flux_VZI(:,i,j,k,1))
#endif

    end do; end do; end do

  contains
    !==========================================================================

#ifndef OPENACC
    subroutine write_test_info(iDimFace, Flux_V)
      integer, intent(in):: iDimFace
      real,    intent(in):: Flux_V(nFlux)

      integer :: iVar
      !------------------------------------------------------------------------

      write(*,'(1x,4(a,i4))')'Hat state for dir=',iDimFace,&
           ' at I=',i,' J=',j,' K=',k
      write(*,*)'rho=', State_V(Rho_)
      write(*,*)'Un =', Un
      write(*,*)'P  =', State_V(p_)
      if(Bx_ > 1)then
         write(*,*)'B  =', State_V(Bx_:Bz_) ! + [B0x,B0y,B0z]
         write(*,*)'BB =', sum(State_V(Bx_:Bz_)**2)
      end if
      write(*,'(1x,4(a,i4))') 'Fluxes for dir    =',iDimFace,&
           ' at I=',i,' J=',j,' K=',k

      write(*,'(1x,4(a,i4),a,es13.5)') 'Flux*Area for dir =',iDimFace,&
           ' at I=',i,' J=',j,' K=',k,' Area=',Area

      write(*,*)'Eigenvalue_maxabs=', Cmax
      write(*,*)'CmaxDt           =', Cmax ! for now the same
      do iVar = 1, nVar + nFluid
         write(*,'(a,a8,5es13.5)') 'Var,F,F_L,F_R,dU,c*dU/2=',&
              NameVar_V(iVar),&
              Flux_V(iVar), &
              FluxLeft_V(iVar)*Area, &
              FluxRight_V(iVar)*Area,&
              StateRightCons_V(iVar)-StateLeftCons_V(iVar),&
              0.5*Cmax*(StateRightCons_V(iVar)-StateLeftCons_V(iVar))*Area
      end do

    end subroutine write_test_info
    !==========================================================================
#endif OPENACC
    subroutine get_physical_flux(State_V, NormalX, NormalY, NormalZ, &
         StateCons_V, Flux_V)
      !$acc routine seq

      real, intent(in) :: State_V(nVar)      ! primitive state vector
      real, intent(in) :: NormalX, NormalY, NormalZ ! face normal
      real, intent(out):: StateCons_V(nFlux) ! conservative state vector
      real, intent(out):: Flux_V(nFlux)      ! conservative flux

      real:: Rho, Ux, Uy, Uz, Bx, By, Bz, Hyp, p, e, Un, Bn, pB, pTot
      !------------------------------------------------------------------------
      Rho = State_V(Rho_)
      Ux  = State_V(Ux_)
      Uy  = State_V(Uy_)
      Uz  = State_V(Uz_)
      Bx  = State_V(Bx_)
      By  = State_V(By_)
      Bz  = State_V(Bz_)
      if(Hyp_ > 1) Hyp = State_V(Hyp_)
      p   = State_V(p_)

      ! Convenient variables
      Un  = Ux*NormalX + Uy*NormalY + Uz*NormalZ
      Bn  = Bx*NormalX + By*NormalY + Bz*NormalZ
      pB  = 0.5*(Bx**2 + By**2 + Bz**2)

      ! Hydro energy density
      e   = InvGammaMinus1*p + 0.5*Rho*(Ux**2 + Uy**2 + Uz**2)

      ! Conservative state for the Rusanov solver
      StateCons_V(Rho_)    = Rho
      StateCons_V(RhoUx_)  = Rho*Ux
      StateCons_V(RhoUy_)  = Rho*Uy
      StateCons_V(RhoUz_)  = Rho*Uz
      StateCons_V(Bx_)     = Bx
      StateCons_V(By_)     = By
      StateCons_V(Bz_)     = Bz
      if(Hyp_ > 1) StateCons_V(Hyp_) = Hyp
      StateCons_V(p_)      = p
      StateCons_V(Energy_) = e + pB  ! Add magnetic energy density

      ! Physical flux
      pTot = p + pB
      Flux_V(Rho_)    = Rho*Un
      Flux_V(RhoUx_)  = Un*Rho*Ux - Bn*Bx + NormalX*pTot
      Flux_V(RhoUy_)  = Un*Rho*Uy - Bn*By + NormalY*pTot
      Flux_V(RhoUz_)  = Un*Rho*Uz - Bn*Bz + NormalZ*pTot
      if(Hyp_ > 1)then
         Flux_V(Bx_)  =  Un*Bx - Ux*Bn + NormalX*SpeedHyp*Hyp
         Flux_V(By_)  =  Un*By - Uy*Bn + NormalY*SpeedHyp*Hyp
         Flux_V(Bz_)  =  Un*Bz - Uz*Bn + NormalZ*SpeedHyp*Hyp
         Flux_V(Hyp_) =  SpeedHyp*Bn
      else
         Flux_V(Bx_)  =  Un*Bx - Ux*Bn
         Flux_V(By_)  =  Un*By - Uy*Bn
         Flux_V(Bz_)  =  Un*Bz - Uz*Bn
      end if
      Flux_V(p_)      =  Un*p
      Flux_V(Energy_) =  Un*(e + p) &
           + Flux_V(Bx_)*Bx + Flux_V(By_)*By + Flux_V(Bz_)*Bz ! Poynting flux

    end subroutine get_physical_flux
    !==========================================================================
    subroutine get_speed_max(State_V, NormalX, NormalY, NormalZ, Un, Cmax)
      !$acc routine seq

      real, intent(in):: State_V(nVar), NormalX, NormalY, NormalZ
      real, intent(out):: Un, Cmax

      real:: InvRho, p, Bx, By, Bz, Bn, B2
      real:: Sound2, Alfven2, Alfven2Normal, Fast2, Discr, Fast
      !------------------------------------------------------------------------

      InvRho = 1.0/State_V(Rho_)
      Bx  = State_V(Bx_)
      By  = State_V(By_)
      Bz  = State_V(Bz_)
      p   = State_V(p_)
      Bn  = Bx*NormalX + By*NormalY + Bz*NormalZ
      B2  = Bx**2 + By**2 + Bz**2

      Sound2        = InvRho*p*Gamma
      Alfven2       = InvRho*B2
      Alfven2Normal = InvRho*Bn**2
      Fast2 = Sound2 + Alfven2
      Discr = sqrt(max(0.0, Fast2**2 - 4*Sound2*Alfven2Normal))
      Fast  = sqrt( 0.5*(Fast2 + Discr) )

      Un   = State_V(Ux_)*NormalX + State_V(Uy_)*NormalY + State_V(Uz_)*NormalZ
      Cmax = abs(Un) + Fast

#ifndef OPENACC
      if(DoTestCell)then
         write(*,*) &
              ' iFluid, rho, p(face)   =', 1, State_V(Rho_), State_V(p_)
         ! if(UseAnisoPressure) write(*,*) &
         !     ' Ppar, Perp             =', Ppar, Pperp
         ! if(UseElectronPressure) write(*,*) &
         !     ' State_V(Pe_)           =', State_V(Pe_)
         ! if(UseAnisoPe) write(*,*) &
         !     ' State_V(Pepar_)        =', State_V(Pepar_)
         ! if(UseWavePressure) write(*,*) &
         !     ' GammaWave, State(Waves)=', &
         !     GammaWave, State_V(WaveFirst_:WaveLast_)
         write(*,*) &
              ' Fast2, Discr          =', Fast2, Discr
         write(*,*) &
              ' Sound2, Alfven2       =', Sound2, Alfven2
         write(*,*) &
              ' FullBn, Alfven2Normal =', Bn, Alfven2Normal
         write(*,*) &
              ' FullBx, FullBy, FullBz=', Bx, By, Bz
      end if
#endif

    end subroutine get_speed_max
    !==========================================================================

  end subroutine calc_face_flux_gpu
  !============================================================================
end module ModFaceFluxGpu
!==============================================================================