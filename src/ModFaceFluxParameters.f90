module ModFaceFluxParameters

  use ModSize,       ONLY: MaxDim
  use ModVarIndexes, ONLY: nVar, nFluid

  implicit none

  public :: init_face_flux_arrays

#ifdef _OPENACC
  integer, parameter :: nFFLogic = 10, nFFInt = 17, nFFReal = 53
#else
  integer, parameter :: nFFLogic = 1, nFFInt = 1, nFFReal = 1
#endif
  integer, parameter :: &
       IsNewBlockVisco_          = 1, &
       IsNewBlockGradPe_         = IsNewBlockVisco_ + 1, &
       IsNewBlockCurrent_        = IsNewBlockGradPe_ + 1, &
       IsNewBlockHeatCond_       = IsNewBlockCurrent_ + 1, &
       IsNewBlockIonHeatCond_    = IsNewBlockHeatCond_ + 1, &
       IsNewBlockRadDiffusion_   = IsNewBlockIonHeatCond_ + 1, &
       IsNewBlockAlfven_         = IsNewBlockRadDiffusion_ + 1, &
       UseHallGradPe_            = IsNewBlockAlfven_ + 1, &
       IsBoundary_               = UseHallGradPe_ + 1, &
       DoTestCell_               = IsBoundary_ + 1

  integer, parameter :: &
       iLeft_           = 1, &
       jLeft_           = iLeft_ + 1, &
       kleft_           = jleft_ + 1, &
       iRight_          = kleft_ + 1, &
       jRight_          = iRight_ + 1, &
       kRight_          = jRight_ + 1, &
       iBlockFace_      = kRight_ + 1, &
       iDimFace_        = iBlockFace_ + 1, &
       iFluidMin_       = iDimFace_ + 1, &
       iFluidMax_       = iFluidMin_ + 1, &
       iVarMin_         = iFluidMax_ + 1, &
       iVarMax_         = iVarMin_ + 1, &
       iEnergyMin_      = iVarMax_ + 1, &
       iEnergyMax_      = iEnergyMin_ + 1, &
       iFace_           = iEnergyMax_ + 1, &
       jFace_           = iFace_ + 1, &
       kFace_           = jFace_ + 1

  integer, parameter :: &
       CmaxDt_          = 1, &
       Area2_           = CmaxDt_ + 1, &
       AreaX_           = Area2_ + 1, &
       AreaY_           = AreaX_ + 1, &
       AreaZ_           = AreaY_ + 1, &
       Area_            = AreaZ_ + 1, &
       DeltaBnL_        = Area_ + 1, &
       DeltaBnR_        = DeltaBnL_ + 1, &
       DiffBb_          = DeltaBnR_ + 1, &
       NormalX_         = DiffBb_ + 1, &
       NormalY_         = NormalX_ + 1, &
       NormalZ_         = NormalY_ + 1, &
       B0n_             = NormalZ_ + 1, &
       B0t1_            = B0n_ + 1, &
       B0t2_            = B0t1_ + 1, &
       UnL_             = B0t2_ + 1, &
       Ut1L_            = UnL_ + 1, &
       Ut2L_            = Ut1L_ + 1, &
       B1nL_            = Ut2L_ + 1, &
       B1t1L_           = B1nL_ + 1, &
       B1t2L_           = B1t1L_ + 1, &
       UnR_             = B1t2L_ + 1, &
       Ut1R_            = UnR_ + 1, &
       Ut2R_            = Ut1R_ + 1, &
       B1nR_            = Ut2R_ + 1, &
       B1t1R_           = B1nR_ + 1, &
       B1t2R_           = B1t1R_ + 1, &
       Enormal_         = B1t2R_ + 1, &
       EtaJx_           = Enormal_ + 1, &
       EtaJy_           = EtaJx_ + 1, &
       EtaJz_           = EtaJy_ + 1, &
       EtaResist_       = EtaJz_ + 1, &
       InvDxyz_         = EtaResist_ + 1, &
       HallCoeff_       = InvDxyz_ + 1, &
       HallJx_          = HallCoeff_ + 1, &
       HallJy_          = HallJx_ + 1, &
       HallJz_          = HallJy_ + 1, &
       BiermannCoeff_   = HallJz_ + 1, &
       GradXPeNe_       = BiermannCoeff_ + 1, &
       GradYPeNe_       = GradXPeNe_ + 1, &
       GradZPeNe_       = GradYPeNe_ + 1, &
       DiffCoef_        = GradZPeNe_ + 1, &
       EradFlux_        = DiffCoef_ + 1, &
       RadDiffCoef_     = EradFlux_ + 1, &
       HeatFlux_        = RadDiffCoef_ + 1, &
       IonHeatFlux_     = HeatFlux_ + 1, &
       HeatCondCoefNormal_ = IonHeatFlux_ + 1, &
       B0x_             = HeatCondCoefNormal_ + 1, &
       B0y_             = B0x_ + 1, &
       B0z_             = B0y_ + 1, &
       ViscoCoeff_      = B0z_ + 1, &
       InvClightFace_   = ViscoCoeff_ + 1, &
       InvClight2Face_  = InvClightFace_ + 1

contains
  !============================================================================
  subroutine init_face_flux_arrays(IsFF_I, iFF_I, rFF_I, &
       Unormal_I, bCrossArea_D)

    logical, intent(inout):: IsFF_I(nFFLogic)
    integer, intent(inout):: iFF_I(nFFInt)
    real, intent(inout):: rFF_I(nFFReal)
    real, intent(inout):: Unormal_I(nFluid+1)
    real, intent(inout):: bCrossArea_D(MaxDim)

    ! When openacc creates a derived type on GPU, the variables are
    ! not correctly initialized. So, they are explicitly initialized
    ! here.

#ifdef _OPENACC
    !--------------------------------------------------------------------------
    iFF_I(iFluidMin_) = 1
    iFF_I(iFluidMax_) = nFluid
    iFF_I(iVarMin_) = 1
    iFF_I(iVarMax_) = nVar
    iFF_I(iEnergyMin_) = nVar + 1
    iFF_I(iEnergyMax_) = nVar + nFluid

    Unormal_I = 0.0
    rFF_I(EradFlux_) = 0.0
    bCrossArea_D = 0.0
    rFF_I(B0x_) = 0.0
    rFF_I(B0y_) = 0.0
    rFF_I(B0z_) = 0.0

    IsFF_I(UseHallGradPe_) = .false.

    IsFF_I(DoTestCell_) = .false.

    IsFF_I(IsNewBlockVisco_) = .true.
    IsFF_I(IsNewBlockGradPe_) = .true.
    IsFF_I(IsNewBlockCurrent_) = .true.
    IsFF_I(IsNewBlockHeatCond_) = .true.
    IsFF_I(IsNewBlockIonHeatCond_) = .true.
    IsFF_I(IsNewBlockRadDiffusion_) = .true.
    IsFF_I(IsNewBlockAlfven_) = .true.
#endif
  end subroutine init_face_flux_arrays
  !============================================================================

end module ModFaceFluxParameters
!==============================================================================
