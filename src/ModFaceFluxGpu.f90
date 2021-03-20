!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModFaceFluxGpu

  use ModVarIndexes
  use ModAdvance, ONLY: nGang, nFlux, &
       LeftState_VXI, RightState_VXI, Flux_VXI, VdtFace_XI, uDotArea_XII, &
       LeftState_VYI, RightState_VYI, Flux_VYI, VdtFace_YI, uDotArea_YII, &
       LeftState_VZI, RightState_VZI, Flux_VZI, VdtFace_ZI, uDotArea_ZII
  use ModPhysics, ONLY: Gamma, InvGammaMinus1
  use ModMain, ONLY: SpeedHyp
  use BATL_lib, ONLY: nDim, nI, nJ, nK, x_, y_, z_, CellFace_DB
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

    integer:: iFace, jFace, kFace, iGang

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_face_flux_gpu'
    !--------------------------------------------------------------------------
    if(DoResChangeOnly) RETURN

    iGang = min(iBlock, nGang)

    !$acc loop vector collapse(3) &
    !$acc private(Area, NormalX, NormalY, NormalZ, Un, Cmax, &
    !$acc         StateLeft_V, StateRight_V, State_V, &
    !$acc         StateLeftCons_V, StateRightCons_V, FluxLeft_V, FluxRight_V)
    do kFace = 1, nK; do jFace = 1, nJ; do iFace = 1, nI+1
       ! if(IsCartesianGrid)then
       Area = CellFace_DB(x_,iBlock)
       NormalX = 1.0
       NormalY = 0.0
       NormalZ = 0.0
       ! end if

       ! Primitive variables
       StateLeft_V  = LeftState_VXI(:,iFace,jFace,kFace,iGang)
       StateRight_V = RightState_VXI(:,iFace,jFace,kFace,iGang)
       State_V      = 0.5*(StateLeft_V + StateRight_V)  ! average state

       call get_speed_max(State_V, NormalX, NormalY, NormalZ, &
            Un, Cmax)
       call get_physical_flux(StateLeft_V, NormalX, NormalY, NormalZ, &
            StateLeftCons_V, FluxLeft_V)
       call get_physical_flux(StateRight_V, NormalX, NormalY, NormalZ, &
            StateRightCons_V, FluxRight_V)

       ! Rusanov flux
       Flux_VXI(:,iFace,jFace,kFace,iGang) = &
            Area*(0.5*(FluxLeft_V + FluxRight_V) &
            + 0.5*Cmax*(StateLeftCons_V - StateRightCons_V))

       ! For time step
       VdtFace_XI(iFace,jFace,kFace,iGang) = Cmax*Area

       ! For div U source term
       uDotArea_XII(iFace,jFace,kFace,:,iGang) = Un*Area

    end do; end do; end do

    if(nDim == 1) RETURN

    !$acc loop vector collapse(3) &
    !$acc private(Area, NormalX, NormalY, NormalZ, Un, Cmax, &
    !$acc         StateLeft_V, StateRight_V, State_V, &
    !$acc         StateLeftCons_V, StateRightCons_V, FluxLeft_V, FluxRight_V)
    do kFace = 1, nK; do jFace = 1, nJ+1; do iFace = 1, nI
       ! if(IsCartesianGrid)then
       Area = CellFace_DB(y_,iBlock)
       NormalX = 0.0
       NormalY = 1.0
       NormalZ = 0.0
       ! end if

       ! Primitive variables
       StateLeft_V  = LeftState_VYI(:,iFace,jFace,kFace,iGang)
       StateRight_V = RightState_VYI(:,iFace,jFace,kFace,iGang)
       State_V      = 0.5*(StateLeft_V + StateRight_V)  ! average state

       call get_speed_max(State_V, NormalX, NormalY, NormalZ, &
            Un, Cmax)
       call get_physical_flux(StateLeft_V, NormalX, NormalY, NormalZ, &
            StateLeftCons_V, FluxLeft_V)
       call get_physical_flux(StateRight_V, NormalX, NormalY, NormalZ, &
            StateRightCons_V, FluxRight_V)

       ! Rusanov flux
       Flux_VYI(:,iFace,jFace,kFace,iGang) = &
            Area*(0.5*(FluxLeft_V + FluxRight_V) &
            + 0.5*Cmax*(StateLeftCons_V - StateRightCons_V))

       ! For time step
       VdtFace_YI(iFace,jFace,kFace,iGang) = Cmax*Area

       ! For div U source term
       uDotArea_YII(iFace,jFace,kFace,:,iGang) = Un*Area

    end do; end do; end do

    if(nDim == 2) RETURN

    !$acc loop vector collapse(3) &
    !$acc private(Area, NormalX, NormalY, NormalZ, Un, Cmax, &
    !$acc         StateLeft_V, StateRight_V, State_V, &
    !$acc         StateLeftCons_V, StateRightCons_V, FluxLeft_V, FluxRight_V)
    do kFace = 1, nK+1; do jFace = 1, nJ; do iFace = 1, nI
       ! if(IsCartesianGrid)then
       Area = CellFace_DB(z_,iBlock)
       NormalX = 0.0
       NormalY = 0.0
       NormalZ = 1.0
       ! end if

       ! Primitive variables
       StateLeft_V  = LeftState_VZI(:,iFace,jFace,kFace,iGang)
       StateRight_V = RightState_VZI(:,iFace,jFace,kFace,iGang)
       ! average state
       State_V      = 0.5*(StateLeft_V + StateRight_V)

       call get_speed_max(State_V, NormalX, NormalY, NormalZ, &
            Un, Cmax)
       call get_physical_flux(StateLeft_V, NormalX, NormalY, NormalZ, &
            StateLeftCons_V, FluxLeft_V)
       call get_physical_flux(StateRight_V, NormalX, NormalY, NormalZ, &
            StateRightCons_V, FluxRight_V)

       ! Rusanov flux
       Flux_VZI(:,iFace,jFace,kFace,iGang) = &
            Area*(0.5*(FluxLeft_V + FluxRight_V) &
            + 0.5*Cmax*(StateLeftCons_V - StateRightCons_V))

       ! For time step
       VdtFace_ZI(iFace,jFace,kFace,iGang) = Cmax*Area

       ! For div U source term
       uDotArea_ZII(iFace,jFace,kFace,:,iGang) = Un*Area

    end do; end do; end do

  contains
    !==========================================================================
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
      !$acc seq
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
      Bn  = State_V(Bx_)*NormalX + State_V(By_)*NormalY + State_V(Bz_)*NormalZ
      B2  = Bx**2 + By**2 + Bz**2

      Sound2        = InvRho*p*Gamma
      Alfven2       = InvRho*B2
      Alfven2Normal = InvRho*Bn**2
      Fast2 = Sound2 + Alfven2
      Discr = sqrt(max(0.0, Fast2**2 - 4*Sound2*Alfven2Normal))
      Fast  = sqrt( 0.5*(Fast2 + Discr) )

      Un   = State_V(Ux_)*NormalX + State_V(Uy_)*NormalY + State_V(Uz_)*NormalZ
      Cmax = Un + Fast

    end subroutine get_speed_max
    !==========================================================================

  end subroutine calc_face_flux_gpu
  !============================================================================
end module ModFaceFluxGpu
!==============================================================================
