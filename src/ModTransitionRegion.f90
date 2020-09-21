!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModTransitionRegion

  use BATL_lib, ONLY: &
       test_start, test_stop
  implicit none
  ! Normalization as used in the radcool table
  real, private, parameter :: RadNorm = 1.0E+22
  ! Correction coefficient to transform the units as used in the radcool
  ! table to SI system.
  real, private, parameter :: Cgs2SiEnergyDens = &
       1.0e-7&   ! erg = 1e-7 J
       /1.0e-6    ! cm3 = 1e-6 m3
  ! To find the volumetric radiative cooling rate in J/(m^3 s)
  ! the value found from radcool table should be multiplied by
  ! RadcoolSi and by n_e[m^{-3}]*n_i[m^{-3}]
  real, parameter :: Radcool2Si = 1.0e-12 & ! (cm-3=>m-3)**2
                          *Cgs2SiEnergyDens/RadNorm
  ! A constant factor to calculate the electron heat conduction
  real :: HeatCondParSi

  ! Coulomb logarithm
  real, parameter  :: CoulombLog = 20.0

  ! Correspondent named indexes: meaning of the columns in the table
  integer,parameter:: LengthPAvrSi_ = 1, UHeat_ = 2
  integer,parameter:: HeatFluxLength_ = 3, DHeatFluxXOverU_ = 4
  integer,parameter:: LambdaSi_=5, DLogLambdaOverDLogT_ = 6
  ! Global arrays used in calculating the tables
  real,dimension(1:500):: TeSi_I, LambdaSi_I, &
       LPe_I, UHeat_I, dFluxXLengthOverDU_I, DLogLambdaOverDLogT_I
  ! Control parameter: minimum temerature
  real, public             :: TeSiMin = 5.0e4
  real             :: SqrtZ   = 1.0
  public :: init_tr
  ! Table numbers needed to use lookup table
  integer, private :: iTableRadCool = -1
  integer          :: iTableTR = -1
  ! Needed for initialization:
  logical, private :: DoInit = .true.
contains
  !============================================================================
  subroutine init_tr(Z, TeChromoSi)
    use ModConst,       ONLY: cBoltzmann, cElectronMass, &
         cEps, cElectronCharge, cTwoPi
    use ModLookupTable, ONLY: i_lookup_table
    use ModUtilities,   ONLY: CON_stop
    !    use BATL_lib,       ONLY: test_start, test_stop, iProc
    ! INPUTS
    ! Average ion charge
    real, intent(in) :: Z
    ! Temperature on top of chromosphere
    real, intent(in) :: TeChromoSi
    integer :: iTableAiaXrt
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'init_tr'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    !   call test_start(NameSub, DoTest)
    if(.not.DoInit)RETURN
    DoInit = .false.
    ! Set model parameters
    SqrtZ = sqrt(Z)
    TeSiMin = TeChromoSi

    ! electron heat conduct coefficient for single charged ions
    ! = 9.2e-12 W/(m*K^(7/2))
    HeatCondParSi = 3.2*3.0*cTwoPi/CoulombLog &
         *sqrt(cTwoPi*cBoltzmann/cElectronMass)*cBoltzmann &
         *((cEps/cElectronCharge)*(cBoltzmann/cElectronCharge))**2

    iTableTR = i_lookup_table('TR')
    if(iTableTR<=0)then
      iTableRadCool = i_lookup_table('radcool')
       if(iTableRadCool <=0 )&
            call CON_stop('To create TR table, the radcool table is needed')
       call check_tr_table
    end if
    !  To make a unit test:
    ! 1. Uncomment the lines below as well as the declaration and the use
    ! of DoTest, test_start and test_stop
    ! 2. Add init_tr identifier to the #TEST command for SC component in
    !    Param/PARAM.in.test.SCIH_threadbc
    !    Param/PARAM.in.test.restart.SCIH_threadbc
    ! 3. run test8
    ! In run_test there will be files test_tr.out and test_tr_AiaXrt.out
    ! if(DoTest.and.iProc==0)then
    !   ! Plot test solution
    !   iTableAiaXrt = i_lookup_table('AiaXrt')
    !   if(iTableAiaXrt>0)then
    !      call plot_tr(&
    !           NamePlotFile='test_tr_AiaXrt.out',&
    !           nGrid       = 100,         &
    !           TeSi        = 5.0e5,       &
    !           PeSi        = 5.0e-3,      &
    !           iTable      = iTableAiaXrt)
    !   else
    !      call plot_tr(&
    !           NamePlotFile='test_tr.out',&
    !           nGrid       = 100,         &
    !           TeSi        = 5.0e5,       &
    !           PeSi        = 5.0e-3       )
    !   end if
    ! end if
    ! call test_stop(NameSub,DoTest)
    call test_stop(NameSub, DoTest)
  end subroutine init_tr
  !============================================================================
  subroutine check_tr_table(TypeFileIn)
    use ModLookupTable, ONLY: i_lookup_table, &
         init_lookup_table, make_lookup_table_1d

    character(LEN=*),optional,intent(in)::TypeFileIn

    character(len=5)::TypeFile

    character(len=*), parameter:: NameSub = 'check_tr_table'
    !--------------------------------------------------------------------------
    if(present(TypeFileIn))then
       TypeFile = TypeFileIn
    else
       TypeFile = 'ascii'
    end if
    ! initialize the TR table
    call init_lookup_table(                                      &
         NameTable = 'TR',                                       &
         NameCommand = 'save',                                   &
         NameVar =                                               &
         'logTe LPe UHeat FluxXLength '//                        &
         'dFluxXLegthOverDU Lambda dLogLambdaOverDLogT ',        &
         nIndex_I = [500],                                       &
         IndexMin_I = [1.0e4],                                   &
         IndexMax_I = [1.0e8],                                   &
         NameFile = 'TR.dat',                                    &
         TypeFile = TypeFile,                                    &
         StringDescription =                                     &
         'Model for transition region: '//                       &
         '[K] [N/m] [m/s] [W/m] [1] [W*m3/(k_B2)] [1]')

    ! The table is now initialized.
    iTableTR = i_lookup_table('TR')
    ! Fill in the table
    call make_lookup_table_1d(iTableTR, calc_tr_table)
  end subroutine check_tr_table
  !============================================================================
  subroutine calc_tr_table(iTableIn, Arg1, Value_V)
    use ModLookupTable, ONLY: interpolate_lookup_table
    use ModConst,      ONLY: cBoltzmann
    integer, intent(in):: iTableIn
    real, intent(in)   :: Arg1
    real, intent(out)  :: Value_V(:)
    integer:: iTe
    real   :: DeltaLogTe, LambdaCgs_V(1)

    logical, save:: IsFirstCall = .true.
    ! at the moment, radcool not a function of Ne, but need a dummy 2nd
    ! index, and might want to include Ne dependence in table later.
    ! Table variable should be normalized to radloss_cgs * 10E+22
    ! since we don't want to deal with such tiny numbers
    ! real, parameter :: RadNorm = 1.0E+22
    ! real, parameter :: Cgs2SiEnergyDens = &
    !     1.0e-7&   ! erg = 1e-7 J
    !     /1.0e-6    ! cm3 = 1e-6 m3
    ! real, parameter :: Radcool2Si = 1.0e-12 & ! (cm-3=>m-3)**2
    !     /RadNorm*Cgs2SiEnergyDens

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'calc_tr_table'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    DeltaLogTe = log(1.0e8/1.0e4)/499 ! Log(MaxTe/MinTe)/(nPoint-1)
    if(IsFirstCall)then
       IsFirstCall=.false.

       TeSi_I(1) = 1.0e4; LPe_I(1) = 0.0; UHeat_I(1) = 0.0
       do iTe = 2,500
          TeSi_I(iTe) = TeSi_I(iTe-1)*exp(DeltaLogTe)
       end do

       do iTe = 1,500
          call interpolate_lookup_table(iTableRadCool,&
               TeSi_I(iTe), LambdaCgs_V)
          LambdaSi_I(iTe) = LambdaCgs_V(1)*Radcool2Si
          if(iTe==1)CYCLE
          ! Integrate \sqrt{2\int{\kappa_0\Lambda Te**1.5 d(log T)}}/k_B
          UHeat_I(iTe) = UHeat_I(iTe-1) + &
               (LambdaSi_I(iTe-1)*TeSi_I(iTe-1)**1.50 + &
               LambdaSi_I(iTe)*TeSi_I(iTe)**1.50)*DeltaLogTe
       end do
       UHeat_I = sqrt(HeatCondParSi*UHeat_I)/cBoltzmann

       ! Temporary fix to avoid a singularity in the first point
       UHeat_I(1) = UHeat_I(2)
       do iTe = 2,500
          ! Integrate \int{\kappa_0\Lambda Te**3.5 d(log T)/UHeat}
          LPe_I(iTe) = LPe_I(iTe-1) + 0.5*DeltaLogTe* &
               ( TeSi_I(iTe-1)**3.50/UHeat_I(iTe-1) + &
               TeSi_I(iTe)**3.50/UHeat_I(iTe) )
          ! Not multiplied by \lappa_0
       end do
       dFluxXLengthOverDU_I(1) = &
            (LPe_I(2)*UHeat_I(2) - LPe_I(1)*UHeat_I(1))/&
            (DeltaLogTe*TeSi_I(1)**3.50)
       do iTe = 2,499
          dFluxXLengthOverDU_I(iTe) = &
               (LPe_I(iTe+1)*UHeat_I(iTe+1) - LPe_I(iTe-1)*UHeat_I(iTe-1))/&
               (2*DeltaLogTe*TeSi_I(iTe)**3.50)
       end do
       dFluxXLengthOverDU_I(500) = &
            (LPe_I(500)*UHeat_I(500) - LPe_I(499)*UHeat_I(499))/&
            (DeltaLogTe*TeSi_I(500)**3.50)

       LPe_I = LPe_I*HeatCondParSi
       UHeat_I(1) = 0.0
       ! Calculate dLogLambda/DLogTe
       DLogLambdaOverDLogT_I(1) = log(LambdaSi_I(2)/LambdaSi_I(1))/&
            DeltaLogTe
       do iTe = 2,499
          DLogLambdaOverDLogT_I(iTe) = &
               log(LambdaSi_I(iTe+1)/LambdaSi_I(iTe-1))/(2*DeltaLogTe)
       end do
       DLogLambdaOverDLogT_I(500) = log(LambdaSi_I(500)/LambdaSi_I(499))/&
            DeltaLogTe
    end if
    iTe = 1 + nint(log(Arg1/1.0e4)/DeltaLogTe)
    Value_V(LengthPAvrSi_:DLogLambdaOverDLogT_) = [ LPe_I(iTe), UHeat_I(iTe), &
         LPe_I(iTe)*UHeat_I(iTe), dFluxXLengthOverDU_I(iTe), &
         LambdaSi_I(iTe)/cBoltzmann**2, DLogLambdaOverDLogT_I(iTe)]

    call test_stop(NameSub, DoTest)
  end subroutine calc_tr_table
  !============================================================================
  subroutine integrate_emission(TeSi, PeSi, iTable, nVar, Integral_V)
    use ModConst, ONLY: cBoltzmann
    use ModLookupTable,  ONLY: interpolate_lookup_table
    ! INPUTS:
    ! The plasma parameters on top of the transition region:
    real,    intent(in)  :: TeSi, PeSi
    integer, intent(in)  :: iTable, nVar
    real,    intent(out) :: Integral_V(nVar)
    ! The model is parameterized in terms of PAvr=sqrt(Pi*Pe) at Ti=Te
    ! In terms of SqrtZ: PAvr = Pi*SqrtZ = Pe/SqrtZ
    real    :: PAvrSi
    ! 1D Grid across the TR
    ! Number of uniform meshes in the range from TeSiMin to Te on the TR top
    integer, parameter :: nTRGrid = 20
    ! mesh-centered temperature and the spatial length of intervals, in cm!!
    real    :: TeAvrSi_I(nTRGrid + 1), DeltaLCgs_I(nTRGrid + 1)
    ! Tabulated analytical solution:
    real    :: TRTable_V(LengthPAvrSi_:DLogLambdaOverDLogT_)
    ! Gen table values:
    real    :: Value_VI(nVar, nTRGrid +1)
    real    :: DeltaTe      ! Mesh of a temperature
    real    :: LPAvrSi_I(nTRGrid + 1), TeSi_I(nTRGrid + 1)
    real    :: DeltaLPAvrSi_I(nTRGrid + 1)
    integer ::  i, iVar ! Loop variables
    ! Electron density in particles per cm3:
    real    :: NeCgs, NiCgs
    !--------------------------------------------------------------------------
    PAvrSi = PeSi/SqrtZ
    DeltaTe = (TeSi - TeSiMin)/nTRGrid
    TeSi_I(1) = TeSiMin
    call interpolate_lookup_table(iTableTR, TeSiMin, TRTable_V, &
         DoExtrapolate=.false.)
    ! First value is now the product of the thread length in meters times
    ! a geometric mean pressure, so that
    LPAvrSi_I(1) = TRTable_V(LengthPAvrSi_)
    do i = 1, nTRGrid
       TeSi_I(i +1) = TeSi_I(i) + DeltaTe
       call interpolate_lookup_table(iTableTR, TeSi_I(i + 1),   &
            TRTable_V, DoExtrapolate=.false.)
       LPAvrSi_I(i + 1) = TRTable_V(LengthPAvrSi_)
       DeltaLPAvrSi_I(i) = LPAvrSi_I(i + 1) - LPAvrSi_I(i)
       TeAvrSi_I(i) = (TeSi_I(i + 1) + TeSi_I(i))*0.50
    end do
    TeAvrSi_I(nTRGrid + 1) = TeSi
    DeltaLPAvrSi_I(nTRGrid + 1) = LPAvrSi_I(1) - LPAvrSi_I(nTRGrid + 1)
    ! Now, DeltaL_I is the length interval multiplied by PAvrSi.
    ! Get rid of the latter factor and convert from meters to cm:
    DeltaLCgs_I = DeltaLPAvrSi_I*100.0/PAvrSi
    do i = 1, nTRGrid + 1
       ! Calculate mesh-centered electron density:
       NeCgs = 1.0e-6*PAvrSi*SqrtZ/(cBoltzmann*TeAvrSi_I(i))
       NiCgs = 1.0e-6*PAvrSi/(cBoltzmann*TeAvrSi_I(i)*SqrtZ)
       call interpolate_lookup_table(iTable, TeAvrSi_I(i), NeCgs, &
            Value_VI(:,i), DoExtrapolate=.true.)
       ! Multiply by a scale factor 1e-26 of the table times Ne*Ni*DeltaL
       Value_VI(:,i) = Value_VI(:,i)*NeCgs*NiCgs*1.0e-26*&
            DeltaLCgs_I(i)
    end do
    ! Sum up the contributions from all temperature intervals:
    do iVar = 1, nVar
       Integral_V(iVar) = sum(Value_VI(iVar,:))
    end do
  end subroutine integrate_emission
  !============================================================================
  subroutine plot_tr(NamePlotFile, nGrid, TeSi, PeSi, iTable)
    use ModPlotFile,    ONLY: save_plot_file
    use ModLookupTable, ONLY: interpolate_lookup_table, get_lookup_table
    use ModConst,       ONLY: cBoltzmann
    use ModUtilities,   ONLY: split_string, join_string
    ! INPUTS:
    character(LEN=*), intent(in) :: NamePlotFile
    ! Number of grid points. The grid is uniform if Te, but not in X
    integer, intent(in)  :: nGrid
    ! The plasma parameters on top of the transition region:
    real,    intent(in)  :: TeSi, PeSi
    ! The TR is mostly used to account for the integral of the spectral
    ! intensity across the transition region, which is tabulated in the
    ! lookup table. In this routine we can visualize the integrand
    integer, optional, intent(in)  :: iTable
    ! The model is parameterized in terms of PAvr=sqrt(Pi*Pe) at Ti=Te
    ! In terms of SqrtZ: PAvr = Pi*SqrtZ = Pe/SqrtZ
    real    :: PAvrSi
    ! 1D Grid across the TR
    real :: LengthSi_I(nGrid)
    ! Tabulated analytical solution:
    real    :: TRTable_V(LengthPAvrSi_:DLogLambdaOverDLogT_)
    ! Plot variables: Electron temperature and density in particles per cm3
    integer, parameter :: TeSi_ = 1, NeCgs_ = 2
    real, allocatable  :: Value_VI(:,:)
    real               :: DeltaTe      ! Mesh of a temperature
    real               :: LPAvrSi_I(nGrid)
    integer            :: i             ! Loop variable
    ! Ion density in particles per cm3:
    real    :: NiCgs
    ! To work with the emissivity table, if any
    logical :: DoPlotEmissivity !=present(iTable)
    integer :: nValue, nVar
    ! Array for variable names
    integer, parameter:: MaxVar = 200
    character(len=20) :: NameVar_I(MaxVar)
    character(len=500):: NameVarTable, NameVarPlot, NameUnitPlot
    !--------------------------------------------------------------------------
    DoPlotEmissivity = present(iTable)
    if(DoPlotEmissivity)then
       call  get_lookup_table(&
            iTable=iTable,    &
            nValue=nValue,    &
            NameVar=NameVarTable)
       call split_string(NameVarTable, MaxVar, NameVar_I, nVar, &
         UseArraySyntaxIn=.true.)
       call join_string(NameVar_I(3:nValue+2),NameVarTable) ! only var names!
       NameVarPlot = 'Length Te Ne '//trim(NameVarTable)//' TeTop PeTop'
       NameVar_I(1:nValue) = 'Response/m'
       call join_string(NameVar_I(:nValue),NameUnitPlot)
       NameUnitPlot='m K 1/cm3 '//trim(NameUnitPlot)//' K N/m2'
       allocate(Value_VI(TeSi_:NeCgs_+nValue,1:nGrid))
    else
       NameVarPlot = 'Length Te Ne TeTop PeTop'
       NameUnitPlot= 'm K 1/cm3 K N/m2'
       allocate(Value_VI(TeSi_:NeCgs_,1:nGrid))
    end if
    PAvrSi = PeSi/SqrtZ
    DeltaTe = (TeSi - TeSiMin)/(nGrid - 1)
    Value_VI(TeSi_,1) = TeSiMin
    Value_VI(NeCgs_,1) = 1.0e-6*PeSi/(cBoltzmann*TeSiMin)
    call interpolate_lookup_table(iTableTR, TeSiMin, TRTable_V, &
         DoExtrapolate=.false.)
    ! First value is now the product of the thread length in meters times
    ! a geometric mean pressure, so that
    LPAvrSi_I(1) = TRTable_V(LengthPAvrSi_)
    do i = 2, nGrid
       Value_VI(TeSi_,i) = Value_VI(TeSi_,i-1) + DeltaTe
       call interpolate_lookup_table(iTableTR, Value_VI(TeSi_,i),   &
            TRTable_V, DoExtrapolate=.false.)
       LPAvrSi_I(i) = TRTable_V(LengthPAvrSi_)
       Value_VI(NeCgs_,i) = 1.0e-6*PeSi/(cBoltzmann*Value_VI(TeSi_,i))
    end do

    ! Now, LPAvrSi is the length interval multiplied by PAvrSi.
    ! Get rid of the latter factor and offset coordinate so that]
    ! LengthSi_I(1) = 0
    LengthSi_I = (LPAvrSi_I -  LPAvrSi_I(1))/PAvrSi
    if(DoPlotEmissivity)then
       do i = 1, nGrid
          call interpolate_lookup_table(iTable, Value_VI(TeSi_,i),   &
            Value_VI(NeCgs_,i), Value_VI(NeCgs_+1:NeCgs_+nValue,i),    &
            DoExtrapolate=.false.)
          ! Multiply response function by: (1) NeCgs*NiCgs;
          ! (2) factor 1e-26 for response functions in CHIANTI tables
          ! (3) factor 100 to convert response/(cm of LoS length) to that per m
          NiCgs = 1.0e-6*PAvrSi/(SqrtZ*cBoltzmann*Value_VI(TeSi_,i))
          Value_VI(NeCgs_+1:NeCgs_+nValue,i) =    &
               Value_VI(NeCgs_+1:NeCgs_+nValue,i)*&
               Value_VI(NeCgs_,i)*NiCgs*          &
               1.0e-26*                           & ! response function per cm
               1.0e2                                ! response function per m
       end do
    end if
    call save_plot_file(&
         NameFile=NamePlotFile,   &
         ParamIn_I = [TeSi, PeSi],&
         NameVarIn = NameVarPlot, &
         nDimIn=1,                &
         Coord1In_I = LengthSi_i, &
         VarIn_VI = Value_VI,     &
         StringHeaderIn = 'Analytical model for transition region: ['//&
         trim(NameUnitPlot)//']')
    deallocate(Value_VI)
  end subroutine plot_tr
  !============================================================================
end module ModTransitionRegion
!==============================================================================
