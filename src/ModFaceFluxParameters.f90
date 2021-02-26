module ModFaceFluxParameters
  
  use ModSize,       ONLY: MaxDim
  use ModVarIndexes, ONLY: nVar, nFluid
  
  implicit none

  integer, parameter :: nFFRealArg = &
       2*nVar + 2*(nVar+nFluid) + 7*MaxDim + 3*(nFluid+1) & ! Arrays
       + 53 ! Scalars
  
  integer, parameter :: &
       StateLeft_       = 1, &
       StateRight_      = StateLeft_ + nVar, &
       FluxLeft_        = StateRight_ + nVar, &
       FLuxRight_       = FluxLeft_ + nVar + nFluid, &
       Normal_          = FLuxRight_ + nVar + nFluid, &
       Tangent1_        = Normal_ + MaxDim, &
       Tangent2_        = Tangent1_ + MaxDim, &
       MhdFlux_         = Tangent2_ + MaxDim, &
       MhdFluxLeft_     = MhdFlux_ + MaxDim, &
       MhdFluxRight_    = MhdFluxLeft_ + MaxDim, &
       Unormal_         = MhdFluxRight_ + MaxDim, &
       UnLeft_          = Unormal_ + nFluid + 1, &
       UnRight_         = UnLeft_ + nFluid + 1, &
       bCrossArea_      = UnRight_ + nFluid + 1,&
       CmaxDt_          = bCrossArea_ + MaxDim, &
       Area2_           = CmaxDt_ + 1, &
       AreaX_           = Area2_ + 1, &
       AreaY_           = AreaX_ + 1, &
       AreaZ_           = AreaY_ + 1, &
       Area_            = AreaZ_ + 1, &
       DeltaBnL_        = Area_ + 1, &
       DeltaBnR_        = DeltaBnL_ + 1, &
       DiffBb_         = DeltaBnR_ + 1, &
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
       Eta_             = EtaJz_ + 1, &
       InvDxyz_         = Eta_ + 1, &
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





end module ModFaceFluxParameters
