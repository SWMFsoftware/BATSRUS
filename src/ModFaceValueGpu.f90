!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModFaceValueGpu

  use BATL_lib, ONLY: nDim, nI, nJ, nK
  use ModVarIndexes
  use ModAdvance, ONLY: State_VGB, &
         LeftState_VXI, RightState_VXI, &
         LeftState_VYI, RightState_VYI, &
         LeftState_VZI, RightState_VZI

  implicit none

  private ! except

  public:: calc_face_value_gpu

contains
  !============================================================================

  subroutine calc_face_value_gpu(iBlock)
    !$acc routine vector

    integer, intent(in):: iBlock

    integer:: i, j, k, iGang

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_face_value_gpu'
    !--------------------------------------------------------------------------
#ifdef OPENACC
    iGang = iBlock
#else
    iGang = 1
#endif

    !$acc loop vector collapse(3) independent
    do k=1, nK; do j=1, nJ; do i=1,nI+1
       LeftState_VXI(Rho_,i,j,k,iGang)    = State_VGB(Rho_,i-1,j,k,iBlock)
       LeftState_VXI(Ux_:Uz_,i,j,k,iGang) = State_VGB(RhoUx_:RhoUz_,i-1,j,k,iBlock) &
            /State_VGB(Rho_,i-1,j,k,iBlock)
       LeftState_VXI(Uz_+1:,i,j,k,iGang)  = State_VGB(RhoUz_+1:,i-1,j,k,iBlock)

       RightState_VXI(Rho_,i,j,k,iGang)    = State_VGB(Rho_,i,j,k,iBlock)
       RightState_VXI(Ux_:Uz_,i,j,k,iGang) = State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) &
            /State_VGB(Rho_,i,j,k,iBlock)
       RightState_VXI(Uz_+1:,i,j,k,iGang)  = State_VGB(RhoUz_+1:,i,j,k,iBlock)
    end do; end do; end do

    if(nDim == 1) RETURN

    !$acc loop vector collapse(3) independent
    do k=1, nK; do j=1, nJ+1; do i=1,nI
       LeftState_VYI(Rho_,i,j,k,iGang)    = State_VGB(Rho_,i,j-1,k,iBlock)
       LeftState_VYI(Ux_:Uz_,i,j,k,iGang) = State_VGB(RhoUx_:RhoUz_,i,j-1,k,iBlock) &
            /State_VGB(Rho_,i,j-1,k,iBlock)
       LeftState_VYI(Uz_+1:,i,j,k,iGang)  = State_VGB(RhoUz_+1:,i,j-1,k,iBlock)

       RightState_VYI(Rho_,i,j,k,iGang)    = State_VGB(Rho_,i,j,k,iBlock)
       RightState_VYI(Ux_:Uz_,i,j,k,iGang) = State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) &
            /State_VGB(Rho_,i,j,k,iBlock)
       RightState_VYI(Uz_+1:,i,j,k,iGang)  = State_VGB(RhoUz_+1:,i,j,k,iBlock)
    end do; end do; end do

    if(nDim == 2) RETURN

    !$acc loop vector collapse(3) independent
    do k=1, nK+1; do j=1, nJ; do i=1,nI
       LeftState_VZI(Rho_,i,j,k,iGang)    = State_VGB(Rho_,i,j,k-1,iBlock)
       LeftState_VZI(Ux_:Uz_,i,j,k,iGang) = State_VGB(RhoUx_:RhoUz_,i,j,k-1,iBlock) &
            /State_VGB(Rho_,i,j,k-1,iBlock)
       LeftState_VZI(Uz_+1:,i,j,k,iGang)  = State_VGB(RhoUz_+1:,i,j,k-1,iBlock)

       RightState_VZI(Rho_,i,j,k,iGang)    = State_VGB(Rho_,i,j,k,iBlock)
       RightState_VZI(Ux_:Uz_,i,j,k,iGang) = State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) &
            /State_VGB(Rho_,i,j,k,iBlock)
       RightState_VZI(Uz_+1:,i,j,k,iGang)  = State_VGB(RhoUz_+1:,i,j,k,iBlock)
    end do; end do; end do

  end subroutine calc_face_value_gpu
  !============================================================================

end module ModFaceValueGpu
!==============================================================================
