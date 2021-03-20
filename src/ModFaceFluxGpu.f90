!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModFaceFluxGpu

  use ModVarIndexes
  use ModAdvance, ONLY: nGang, nFlux, &
       LeftState_VXI, RightState_VXI,Flux_VXI, VdtFace_XI, uDotArea_XII
  use ModPhysics, ONLY: Gamma, InvGammaMinus1
  use ModMain, ONLY: SpeedHyp
  use BATL_lib, ONLY: nI, nJ, nK, x_, CellFace_DB  
  implicit none
  
contains
  !============================================================================
  subroutine calc_face_flux_gpu(DoResChangeOnly, iBlock)
    !$acc routine vector

    logical, intent(in) :: DoResChangeOnly
    integer, intent(in) :: iBlock

    real :: Area, Cmax
    real :: StateLeft_V(nVar), StateRight_V(nVar), State_V(nVar)
    real :: StateLeftCons_V(nFlux), StateRightCons_V(nFlux)
    real :: FluxLeft_V(nFlux), FluxRight_V(nFlux)

    integer:: iFace, jFace, kFace, iGang
    
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_face_flux'
    !--------------------------------------------------------------------------
    if(DoResChangeOnly) RETURN

    iGang = max(iBlock, nGang)
    
    !$acc loop vector collapse(3) &
    !$acc private(Area, Cmax, StateLeft_V, StateRight_V, State_V, &
    !$acc         StateLeftCons_V, StateRightCons_V, FluxLeft_V, FluxRight_V)
    do kFace = 1, nK; do jFace = 1, nJ; do iFace = 1, nI+1
       Area = CellFace_DB(x_,iBlock)

       ! Primitive variables
       StateLeft_V  = LeftState_VXI(:,iFace,jFace,kFace,iGang)
       StateRight_V = RightState_VXI(:,iFace,jFace,kFace,iGang)
       State_V      = 0.5*(StateLeft_V + StateRight_V)  ! average state

       call get_speed_max(State_V, Cmax)
       call get_physical_flux(StateLeft_V, StateLeftCons_V, FluxLeft_V)
       call get_physical_flux(StateRight_V, StateRightCons_V, FluxRight_V)

       ! Rusanov flux
       Flux_VXI(:,iFace,jFace,kFace,iGang) = &
            Area*(0.5*(FluxLeft_V + FluxRight_V) &
            + Cmax*(StateLeftCons_V - StateRightCons_V))

       ! For time step
       VdtFace_XI(iFace,jFace,kFace,iGang) = Cmax*Area
       ! For div U source term
       uDotArea_XII(iFace,jFace,kFace,:,iGang) = State_V(Ux_)*Area

    end do; end do; end do


  contains
    !==========================================================================
    subroutine get_physical_flux(State_V, StateCons_V, Flux_V)
      !$acc routine seq

      real, intent(in) :: State_V(nVar)      ! primitive state vector
      real, intent(out):: StateCons_V(nFlux) ! conservative state vector
      real, intent(out):: Flux_V(nFlux)      ! conservative flux

      real:: Rho, Ux, Uy, Uz, Bx, By, Bz, Hyp, p, e, Un, Bn, B2
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
      e   = InvGammaMinus1*p + 0.5*Rho*(Ux**2 + Uy**2 + Uz**2) + 0.5*B2

      ! Convenient variables for different directions (pass Normal_D?)
      Un  = Ux
      Bn  = Bx
      B2  = Bx**2 + By**2 + Bz**2

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
      StateCons_V(Energy_) = e

      ! Physical flux
      Flux_V(Rho_)    =  Rho*Un
      Flux_V(RhoUx_)  = -Bn*Bx + 0.5*B2
      Flux_V(RhoUy_)  = -Bn*By
      Flux_V(RhoUz_)  = -Bn*Bz
      if(Hyp_ > 1)then
         Flux_V(Bx_)  =  Un*Bx - Ux*Bn + SpeedHyp*Hyp
         Flux_V(By_)  =  Un*By - Uy*Bn
         Flux_V(Bz_)  =  Un*Bz - Uz*Bn
         Flux_V(Hyp_) =  SpeedHyp*Bn
      else
         Flux_V(Bx_)  =  Un*Bx - Ux*Bn
         Flux_V(By_)  =  Un*By - Uy*Bn
         Flux_V(Bz_)  =  Un*Bz - Uz*Bn
      end if
      Flux_V(p_)      =  Un*p
      Flux_V(Energy_) =  Un*(e + p) &
           + Flux_V(Bx_)*Bx + Flux_V(By_)*By + Flux_V(Bz_)*Bz

    end subroutine get_physical_flux
    !==========================================================================
    subroutine get_speed_max(State_V, Cmax)
      !$acc seq
      real, intent(in):: State_V(nVar)
      real, intent(out):: Cmax

      real:: InvRho, p, Un, Bx, By, Bz, Bn, B2
      real:: Sound2, Alfven2, Alfven2Normal, Fast2, Discr, Fast
      !------------------------------------------------------------------------

      InvRho = 1.0/State_V(Rho_)
      Bx  = State_V(Bx_)
      By  = State_V(By_)
      Bz  = State_V(Bz_)
      p   = State_V(p_)
      Un  = State_V(Ux_)
      Bn  = Bx
      B2  = Bx**2 + By**2 + Bz**2

      Sound2        = InvRho*p*Gamma
      Alfven2       = InvRho*B2
      Alfven2Normal = InvRho*Bn**2
      Fast2 = Sound2 + Alfven2
      Discr = sqrt(max(0.0, Fast2**2 - 4*Sound2*Alfven2Normal))
      Fast  = sqrt( 0.5*(Fast2 + Discr) )
      
      Cmax = Un + Fast
      
    end subroutine get_speed_max
  end subroutine calc_face_flux_gpu
  !============================================================================
end module ModFaceFluxGpu
!==============================================================================
