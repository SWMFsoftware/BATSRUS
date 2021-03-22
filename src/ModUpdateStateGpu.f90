!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModUpdateStateGpu

  use ModMain, ONLY: iStage, Cfl
  use ModAdvance
  use BATL_lib, ONLY: nI, nJ, nK, CellVolume_GB
  use ModEnergy, ONLY: calc_energy_or_pressure
  use ModMultiFluid, ONLY: nFluid, IonLast_, &
       iRho, iRhoUx, iRhoUy, iRhoUz, iP, select_fluid
  use ModPhysics,    ONLY: GammaMinus1_I
  
  implicit none

contains
  !============================================================================
  subroutine update_state_gpu(iBlock)
    !$acc routine vector
    integer, intent(in):: iBlock

    integer:: iVar, iFluid, i, j, k, iGang
#ifdef OPENACC
    !--------------------------------------------------------------------------
      iGang = iBlock
#else
      iGang = 1
#endif

    ! Note must copy state to old state only if iStage is 1.
    if(iStage==1) then
       !$acc loop vector collapse(4)
       do k = 1,nK; do j = 1,nJ; do i = 1,nI; do iVar = 1, nVar
          StateOld_VGB(iVar,i,j,k,iBlock) = State_VGB(iVar,i,j,k,iBlock)
       enddo; enddo; enddo; enddo

       !$acc loop vector collapse(4)
       do iFluid = 1, nFluid; do k = 1,nK; do j = 1,nJ; do i = 1,nI
          EnergyOld_CBI(i,j,k,iBlock,iFluid) = Energy_GBI(i,j,k,iBlock,iFluid)
       enddo; enddo; enddo; enddo
    end if

    ! Add div(F) to the source terms
    !$acc loop vector collapse(4)
    do k = 1, nK; do j = 1, nJ; do i = 1, nI; do iVar = 1, nFlux
       Source_VCI(iVar,i,j,k,iGang) = Cfl*time_BLK(i,j,k,iBlock)* &
            (Source_VCI(iVar,i,j,k,iGang) + &
            ( Flux_VXI(iVar,i,j,k,iGang)  - Flux_VXI(iVar,i+1,j,k,iGang)  &
            + Flux_VYI(iVar,i,j,k,iGang)  - Flux_VYI(iVar,i,j+1,k,iGang)  &
            + Flux_VZI(iVar,i,j,k,iGang)  - Flux_VZI(iVar,i,j,k+1,iGang)  ) &
            /CellVolume_GB(i,j,k,iBlock) &
            )
    end do; end do; end do; end do

    ! Update State_VGB
    !$acc loop vector collapse(4)
    do k=1,nK; do j=1,nJ; do i=1,nI; do iVar = 1, nVar
       State_VGB(iVar,i,j,k,iBlock) = &
            StateOld_VGB(iVar,i,j,k,iBlock) + Source_VCI(iVar,i,j,k,iGang)
    end do; end do; end do; end do

    ! Update energy variables
    !$acc loop vector collapse(4)
    do iFluid=1,nFluid; do k=1,nK; do j=1,nJ; do i=1,nI
       Energy_GBI(i,j,k,iBlock,iFluid) = &
            EnergyOld_CBI(i,j,k,iBlock,iFluid) &
            + Source_VCI(nVar+iFluid,i,j,k,iGang)
    end do; end do; end do; end do

    ! Calculate pressure from energy
    do iFluid = 1, IonLast_
       if(nFluid > 1) call select_fluid(iFluid)
       !$acc loop vector collapse(3)
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          State_VGB(iP,i,j,k,iBlock) = &
               GammaMinus1_I(iFluid)*( Energy_GBI(i,j,k,iBlock,iFluid) - &
               0.5*(State_VGB(iRhoUx,i,j,k,iBlock)**2 + &
               State_VGB(iRhoUy,i,j,k,iBlock)**2 + &
               State_VGB(iRhoUz,i,j,k,iBlock)**2) &
               /State_VGB(iRho,i,j,k,iBlock) )
       end do; end do; end do

       if(iFluid > 1 .or. .not. IsMhd) CYCLE

       ! Subtract magnetic energy
       !$acc loop vector collapse(3)
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          State_VGB(iP,i,j,k,iBlock) = State_VGB(iP,i,j,k,iBlock) &
               - GammaMinus1_I(iFluid)*0.5* &
               (State_VGB(Bx_,i,j,k,iBlock)**2 + &
               State_VGB(By_,i,j,k,iBlock)**2 + &
               State_VGB(Bz_,i,j,k,iBlock)**2 )
       end do; end do; end do
    end do
    
  end subroutine update_state_gpu
  !============================================================================

end module ModUpdateStateGpu
!==============================================================================
