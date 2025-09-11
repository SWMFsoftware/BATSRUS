!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModCellBoundary

  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest, iDimTest
  use ModBatsrusUtility, ONLY: stop_mpi

  implicit none

  SAVE

  private ! except

  ! Public methods
  public :: set_cell_boundary, set_edge_corner_ghost

  ! How to add a boundary condition?
  ! 1) Add an index for this boundary.
  ! 2) Increase nTypeBC
  ! 3) Add the name of this boundary to NameCellBc_I.
  integer, public, parameter :: &
       UnknownBC_            = 0, &
       NoneBC_               = 1,  NoneSemiBC_              =  -1, &
       GradPotBC_            = 2,  GradPotSemiBC_           =  -2, &
       CoupledBC_            = 3,  CoupledSemiBC_           =  -3, &
       PeriodicBC_           = 4,  PeriodicSemiBC_          =  -4, &
       FloatBC_              = 5,  FloatSemiBC_             =  -5, &
       OutFlowBC_            = 6,  OutFlowSemiBC_           =  -6, &
       ReflectBC_            = 7,  ReflectSemiBC_           =  -7, &
       LinetiedBC_           = 8,  LinetiedSemiBC_          =  -8, &
       FixedBC_              = 9,  FixedSemiBC_             =  -9, &
       InFlowBC_             = 10, InFlowSemiBC_            =  -10, &
       VaryBC_               = 11, VarySemiBC_              =  -11, &
       IHBufferBC_           = 12, IHBufferSemiBC_          =  -12, &
       FixedB1BC_            = 13, FixedB1SemiBC_           =  -13, &
       ShearBC_              = 14, ShearSemiBC_             =  -14, &
       FieldLineThreadsBC_   = 15, FieldLineThreadsSemiBC_  =  -15, &
       UserBC_               = 16, UserSemiBC_              =  -16, &
       UserFixValueBC_       = 17,                                  &
       UserNoInflowBC_       = 18

  integer, public, parameter :: nTypeBC = 18

  character(len=20), public, parameter :: NameCellBc_I(1:nTypeBC) = [ &
       'none            ', &
       'gradpot         ', &
       'coupled         ', &
       'periodic        ', &
       'float           ', &
       'outflow         ', &
       'reflect         ', &
       'linetied        ', &
       'fixed           ', &
       'inflow          ', &
       'vary            ', &
       'ihbuffer        ', &
       'fixedb1         ', &
       'shear           ', &
       'fieldlinethreads', &
       'user            ', &
       'userfixvalue    ', &
       'usernoinflow    ']

  integer, public :: iMin, iMax, jMin, jMax, kMin, kMax
  !$omp threadprivate(iMin, iMax, jMin, jMax, kMin, kMax)
contains
  !============================================================================
  subroutine set_cell_boundary(nGhost, iBlock, nVarState, State_VG, &
       iImplBlock, IsLinear, iTypeBcIn, iSideIn)

    ! Set ghost cells values in State_VG based on iTypeCellBc_I.
    ! iTypeBcIn can override the boundary condition defined in iTypeCellBc_I

    use ModVarIndexes
    use BATL_size, ONLY: nI, nJ, nK, MaxDim, nDim
    use ModAdvance, ONLY: UseEfield
    use ModSize, ONLY: x_, y_, z_
    use ModMain, ONLY: NameThisComp, UseRadDiffusion, UseB, UseB0, &
         UseHyperbolicDivb, IsTimeAccurate, IsTimeLoop, &
         TypeCellBc_I, iTypeCellBc_I
    use ModParallel, ONLY: Unset_, DiLevel_EB
    use ModGeometry, ONLY: &
         IsBoundary_B, XyzMin_D
    use ModPhysics, ONLY: UseOutflowPressure, pOutFlow, CellState_VI, &
         nVectorVar,iVectorVar_I
    use ModSemiImplVar, ONLY: nVectorSemi, iVectorSemi_I
    use ModMultiFluid, ONLY: nFluid, iRhoUx_I, iRhoUy_I, iRhoUz_I
    use ModImplicit, ONLY: TypeSemiImplicit, iVarSemiMin, iVarSemiMax, &
         iErImplFirst, iErImplLast
    use ModResistivity, ONLY: BxImpl_, ByImpl_, BzImpl_
    use ModRadDiffusion, ONLY: set_rad_outflow_bc
    use BATL_lib, ONLY: IsRzGeometry, IsCylindricalAxis, IsSphericalAxis, &
         Theta_, nRoot_D, MinI, MaxI, MinJ, MaxJ, MinK, MaxK
    use ModUserInterface ! user_set_cell_boundary
    use ModThreadedLC, ONLY: set_field_line_thread_bc
    use BATL_lib, ONLY: IsCartesianGrid
    use ModSaMhd, ONLY: UseSaMhd

    integer, intent(in):: nGhost
    integer, intent(in):: iBlock
    integer, intent(in):: nVarState
    real, intent(inout):: State_VG(nVarState,MinI:MaxI,MinJ:MaxJ,MinK:MaxK)

    ! Optional arguments when called by semi-implicit scheme or something else
    integer, optional, intent(in):: iImplBlock
    logical, optional, intent(in):: IsLinear
    integer, optional, intent(in):: iTypeBcIn
    integer,          optional, intent(in):: iSideIn

    integer:: iVar, iFluid, iSide, iSideMin, iSideMax

    integer :: iTypeBC
    character(len=30):: TypeBc

    ! Coefficient +1 or -1 for symmetric vs. anti-symmetric BCs
    real:: SymmCoeff_V(nVarState)

    integer :: iGhost, jGhost, kGhost, iGhost2, jGhost2, kGhost2
    logical :: IsFound

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_cell_boundary'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    if(.not.IsBoundary_B(iBlock))then
       write(*,*) NameSub,' warning: iBlock=',iBlock,' is not far_field block'
       RETURN
    end if

    if(DoTest)write(*,*)NameSub,': iBlock, DiLevel_EB=',&
         iBlock, DiLevel_EB(:,iBlock)

    if(DoTest)then
       Ighost =iTest; Jghost=jTest;  Kghost=kTest
       Ighost2=iTest; Jghost2=jTest; Kghost2=kTest
       select case(iDimTest)
       case(x_)
          if(iTest== 1)then
             iGhost=0; iGhost2 = 1 - nGhost
          endif
          if(iTest==nI)then
             iGhost=nI+1; iGhost2 = nI + nGhost
          endif
       case(y_)
          if(jTest== 1)then
             jGhost=0; jGhost2 = 1 - nGhost
          endif
          if(jTest==nJ)then
             jGhost=nJ+1; jGhost2 = nJ + nGhost
          endif
       case(z_)
          if(kTest== 1)then
             kGhost=0; kGhost2 = 1 - nGhost
          endif
          if(kTest==nK)then
             kGhost=nK+1; kGhost2 = nK + nGhost
          endif
       end select

       write(*,*)'iTest,  jTest,  kTest   =',iTest,  jTest,  kTest
       write(*,*)'iGhost, jGhost, kGhost  =',iGhost, jGhost, kGhost
       write(*,*)'iGhost2,jGhost2,kGhost2 =',iGhost2,jGhost2,kGhost2
       do iVar = 1, nVarState
          if(.not.present(iImplBlock)) then
             write(*,*)'initial',NameVar_V(iVar),   'cell,ghost,ghost2=',&
                  State_VG(iVar,iTest,jTest,kTest),&
                  State_VG(iVar,Ighost,Jghost,Kghost), &
                  State_VG(iVar,Ighost2,Jghost2,Kghost2)
          else
             write(*,*)'initial impl var',iVar, 'cell,ghost,ghost2=',&
                  State_VG(iVar,iTest,jTest,kTest),&
                  State_VG(iVar,Ighost,Jghost,Kghost), &
                  State_VG(iVar,Ighost2,Jghost2,Kghost2)
          end if
       end do
    end if

    if(present(iSideIn)) then
       iSideMin = iSideIn
       iSideMax = iSideIn
    else
       iSideMin = 1
       iSideMax = 2*nDim
    end if

    ! Loop through all sides
    do iSide = iSideMin, iSideMax

       ! Check if this side of the block is indeed an outer boundary
       ! Also skips periodic boundaries
       if(DiLevel_EB(iSide,iBlock) /= Unset_) CYCLE

       ! Apply cell BC when TypeCellBc_I(1:6) is set
       if(iTypeCellBc_I(iSide) == NoneBC_) CYCLE

       ! Do not apply cell boundary conditions at the pole
       ! This is either handled by message passing or supercell
       if(IsCylindricalAxis .and. iSide == 1) CYCLE

       ! Skip spherical Theta/Latitude boundaries if they reach the poles
       if(IsSphericalAxis .and. (iSide + 1)/2 == Theta_) CYCLE

       ! Set index limits for the ghost cell range
       iMin = 1 - nGhost; iMax = nI + nGhost
       jMin = 1 - nGhost; jMax = nJ + nGhost
       kMin = 1 - nGhost; kMax = nK + nGhost
       select case(iSide)
       case(1)
          iMax = 0
          ! Avoid using cells that are potentially not yet set
          if(DiLevel_EB(3,iBlock) == Unset_) jMin = 1
          if(DiLevel_EB(4,iBlock) == Unset_) jMax = nJ
          if(DiLevel_EB(5,iBlock) == Unset_) kMin = 1
          if(DiLevel_EB(6,iBlock) == Unset_) kMax = nK
       case(2)
          iMin = nI + 1
          if(DiLevel_EB(3,iBlock) == Unset_) jMin = 1
          if(DiLevel_EB(4,iBlock) == Unset_) jMax = nJ
          if(DiLevel_EB(5,iBlock) == Unset_) kMin = 1
          if(DiLevel_EB(6,iBlock) == Unset_) kMax = nK
       case(3)
          jMax = 0
          if(DiLevel_EB(5,iBlock) == Unset_) kMin = 1
          if(DiLevel_EB(6,iBlock) == Unset_) kMax = nK
       case(4)
          jMin = nJ + 1
          if(DiLevel_EB(5,iBlock) == Unset_) kMin = 1
          if(DiLevel_EB(6,iBlock) == Unset_) kMax = nK
       case(5)
          kMax = 0
       case(6)
          kMin = nK + 1
       end select

       if(nJ==1) jMin = 1
       if(nJ==1) jMax = 1
       if(nK==1) kMin = 1
       if(nK==1) kMax = 1

       if(present(iTypeBcIn))then
          iTypeBC = iTypeBcIn
       else
          iTypeBc = iTypeCellBc_I(iSide)
          if(present(iImplBlock)) iTypeBc = - abs(iTypeBc)
          TypeBc = TypeCellBc_I(iSide)
          if(present(iImplBlock)) TypeBc = trim(TypeBc)//'_semi'
       end if

       if(DoTest) write(*,*) NameSub,' iSide, Type iMin,iMax...kMax=', &
            iSide, TypeBc, iMin, iMax, jMin, jMax, kMin, kMax

       select case(iTypeBc)

       case(GradPotBC_)
          ! set boundary condition for electric potential as
          ! grad(potential) = E
          if(nVarState /= 1) call stop_mpi(NameSub// &
               ': gradpot BC is for scalar variable only')
          call set_gradpot_bc

       case(CoupledBC_)
          ! For SC-IH coupling the extra wave energy variable needs a BC
          if(NameThisComp == 'SC') call set_float_bc(&
               ScalarFirst_, ScalarLast_, iSide, iMin, iMax, &
               jMin, jMax, kMin, kMax, nVarState, State_VG)

       case(PeriodicBC_, PeriodicSemiBC_)
          call stop_mpi('The neighbors are not defined at periodic boundaries')

       case(FloatBC_, OutFlowBC_)
          call set_float_bc(1, nVarState, iSide, iMin, iMax, &
               jMin, jMax, kMin, kMax, nVarState, State_VG)
          if(UseOutflowPressure .and. iTypeBc == OutFlowBC_) &
               call set_fixed_bc(p_, p_, [pOutflow], iMin, iMax, &
               jMin, jMax, kMin, kMax, nVarState, State_VG)
          if(UseHyperbolicDivb) &
               call set_fixed_bc(Hyp_, Hyp_, [0.0], iMin, iMax, &
               jMin, jMax, kMin, kMax,  nVarState, State_VG)
          if(UseSaMhd)           &
               call set_fixed_bc(SignB_, SignB_, [0.0], iMin, iMax, &
               jMin, jMax, kMin, kMax,  nVarState, State_VG)
          if(UseEfield)         &
               call set_fixed_bc(HypE_, HypE_, [0.0], iMin, iMax, &
               jMin, jMax, kMin, kMax, nVarState, State_VG)
          if(UseRadDiffusion)   &
               call set_radiation_outflow_bc(WaveFirst_, WaveLast_, iSide)
       case(FloatSemiBC_, OutFlowSemiBC_)
          do iVar = iVarSemiMin, iVarSemiMax
             if(iVar < iErImplFirst .or. iVar > iErImplLast)then
                ! For non-radiation variables
                if(IsLinear)then
                   State_VG(iVar,imin:imax,jmin:jmax,kmin:kmax) = 0.0
                else
                   call set_float_bc(iVar, iVar, iSide, iMin, iMax, &
                        jMin, jMax, kMin, kMax, nVarState, &
                        State_VG)
                end if
             elseif(iVar == iErImplFirst .or. nVarState == 1)then
                ! For radiation variables (only call once when unsplit)
                call set_rad_outflow_bc(iSide, iBlock, iImplBlock, &
                     State_VG, IsLinear)
             end if
          end do

       case(ReflectBC_)
          if(IsCartesianGrid)then
             ! Scalars are symmetric
             SymmCoeff_V = 1.0

             ! Normal vector components are mirror symmetric
             select case(iSide)
             case(1,2)
                SymmCoeff_V(iRhoUx_I(1:nFluid)) = -1.0
                if(UseB)SymmCoeff_V(Bx_) = -1.0
             case(3,4)
                SymmCoeff_V(iRhoUy_I(1:nFluid)) = -1.0
                if(UseB)SymmCoeff_V(By_) = -1.0
                ! For RZ geometry, mirror Z components too at the axis
                if(IsRzGeometry .and. XyzMin_D(2)==0.0 .and. iSide==3)then
                   SymmCoeff_V(iRhoUz_I(1:nFluid)) = -1.0
                   if(UseB)SymmCoeff_V(Bz_) = -1.0
                end if
             case(5,6)
                SymmCoeff_V(iRhoUz_I(1:nFluid)) = -1.0
                if(UseB)SymmCoeff_V(Bz_) = -1.0
             end select
             call set_symm_bc(1, nVarState, SymmCoeff_V)
          else
             call set_reflect_bc(nVectorVar, iVectorVar_I)
          end if
       case(ReflectSemiBC_)
          if(IsCartesianGrid .or. TypeSemiImplicit(1:6) /= 'resist')then
             ! Scalars are symmetric
             SymmCoeff_V = 1.0

             ! Semi-implicit scheme is mostly applied to scalars
             ! The only exception right now is the magnetic field
             if(TypeSemiImplicit(1:6) == 'resist')then
                select case(iSide)
                case(1,2)
                   SymmCoeff_V(BxImpl_) = -1.0
                case(3,4)
                   SymmCoeff_V(ByImpl_) = -1.0
                   ! For RZ geometry, mirror Z components too at the axis
                   if(IsRzGeometry .and. XyzMin_D(2)==0.0 .and. iSide==3)then
                      SymmCoeff_V(BzImpl_) = -1.0
                   end if
                case(5,6)
                   SymmCoeff_V(BzImpl_) = -1.0
                end select
             end if
             call set_symm_bc(1, nVarState, SymmCoeff_V)
          else
             call set_reflect_bc(nVectorSemi, iVectorSemi_I)
          end if
       case(LinetiedBC_)
          ! Most variables float
          call set_float_bc(1, nVarState, iSide, iMin, iMax, jMin, jMax, &
               kMin, kMax, nVarState, State_VG)

          if(.not.present(iImplBlock))then
             ! The density and momentum use symmetric/antisymmetric BCs
             SymmCoeff_V = 1.0
             do iFluid=1, nFluid
                SymmCoeff_V(iRhoUx_I(iFluid):iRhoUz_I(iFluid)) = -1.0
                call set_symm_bc(iRho_I(iFluid), iRhoUz_I(iFluid), SymmCoeff_V)
             end do
          end if

       case(LinetiedSemiBC_)
          ! For semi-implicit scheme all variables float
          call set_float_bc(1, nVarState, iSide, iMin, iMax, jMin, jMax, &
               kMin, kMax, nVarState, State_VG)

       case(FixedBC_, InFlowBC_, VaryBC_, IHBufferBC_)
          if(IsTimeAccurate &
               .and.(iTypeBc == VaryBC_ .or. iTypeBc == InFlowBC_))then
             call set_solar_wind_bc
          else if(iTypeBc == IHBufferBC_ .and. IsTimeLoop)then
             call set_solar_wind_bc_buffer
          else
             call set_fixed_bc(1, nVarState, CellState_VI(:,iSide), &
                  iMin, iMax, jMin, jMax, kMin, kMax, nVarState, State_VG)
             if(UseB0)call fix_b0(Bx_, Bz_, iBlock, iMin, iMax, &
                  jMin, jMax, kMin, kMax, nVarState, State_VG)
          end if
       case(FixedSemiBC_, InFlowSemiBC_, VarySemiBC_)
          if (IsLinear) then
             State_VG(:,iMin:iMax,jMin:jMax,kMin:kMax) = 0.0
          else
             if(IsTimeAccurate &
                  .and.(iTypeBc == VaryBC_ .or. iTypeBc == InFlowBC_))then
                call set_solar_wind_bc
             else
                call set_fixed_semi_bc
             end if
          end if
       case(FixedB1BC_)
          call set_fixed_bc(1, nVarState, CellState_VI(:,iSide), &
               iMin, iMax, jMin, jMax, kMin, kMax, nVarState, State_VG)
       case(FixedB1SemiBC_)
          State_VG(:,iMin:iMax,jMin:jMax,kMin:kMax) = 0.0
       case(ShearBC_, ShearSemiBC_)
          call set_shear_bc
       case(NoneBC_)
       case(NoneSemiBC_)
          if(IsLinear) State_VG(:,iMin:iMax,jMin:jMax,kMin:kMax) = 0.0
       case(FieldLineThreadsBC_)
          call set_field_line_thread_bc(nGhost, iBlock, nVarState, State_VG)
       case(FieldLineThreadsSemiBC_)
          if(IsLinear)then
             State_VG(:,0,jMin:jMax,kMin:kMax) = 0.0
          else
             call set_field_line_thread_bc( &
                  nGhost, iBlock, nVarState, State_VG, iImplBlock)
          end if
       case(UserSemiBC_)
          if(IsLinear)then
             State_VG(:,iMin:iMax,jMin:jMax,kMin:kMax) = 0.0
             TypeBc = 'usersemilinear'
             call user_set_cell_boundary(iBlock, iSide, TypeBc, IsFound)
          else
             IsFound = .false.
             TypeBc = 'usersemi'
             call user_set_cell_boundary(iBlock, iSide, TypeBc, IsFound)
             if(.not.IsFound) call stop_mpi(NameSub// &
                  ': usersemi boundary condition is not found in user module')
          end if
       case default
          IsFound=.false.
          if(TypeBc(1:4) == 'user')then
             if(present(IsLinear))then
                if(IsLinear)then
                   ! Linear semi-implicit BC. Default is zero
                   TypeBc = trim(TypeBc)//'linear'
                   State_VG(:,iMin:iMax,jMin:jMax,kMin:kMax) = 0.0
                end if
             end if
             call user_set_cell_boundary(iBlock, iSide, TypeBc, IsFound)
          end if
          if(.not. IsFound) call stop_mpi(NameSub// &
               ': unknown TypeBc='//TypeBc)
       end select
    end do

    if(DoTest)then
       do iVar = 1, nVarState
          if(.not.present(iImplBlock)) then
             write(*,*)'final',NameVar_V(iVar),   'cell,ghost,ghost2=',&
                  State_VG(iVar,iTest,jTest,kTest),&
                  State_VG(iVar,Ighost,Jghost,Kghost), &
                  State_VG(iVar,Ighost2,Jghost2,Kghost2)
          else
             write(*,*)'final impl var',iVar, 'cell,ghost,ghost2=',&
                  State_VG(iVar,iTest,jTest,kTest),&
                  State_VG(iVar,Ighost,Jghost,Kghost), &
                  State_VG(iVar,Ighost2,Jghost2,Kghost2)
          end if
       end do
    end if

    call test_stop(NameSub, DoTest, iBlock)
  contains
    !==========================================================================
    subroutine set_gradpot_bc

      ! Pot(ghost) = Pot(inside2) + (x_ghost - x_inside2).E_inside1

      use ModAdvance, ONLY: Efield_DGB
      use BATL_lib, ONLY: Xyz_DGB, j2_, k2_, nJm1_, nKm1_

      integer:: i, j, k
      !------------------------------------------------------------------------
      select case(iSide)
      case(1)
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            State_VG(1,i,j,k) = State_VG(1,2,j,k) + &
                 sum( (Xyz_DGB(:,i,j,k,iBlock) - Xyz_DGB(:,2,j,k,iBlock)) &
                 *Efield_DGB(:,1,j,k,iBlock))
         end do; end do; end do
      case(2)
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            State_VG(1,i,j,k) = State_VG(1,nI-1,j,k) + &
                 sum( (Xyz_DGB(:,i,j,k,iBlock) - Xyz_DGB(:,nI-1,j,k,iBlock)) &
                 *Efield_DGB(:,nI,j,k,iBlock))
         end do; end do; end do
      case(3)
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            State_VG(1,i,j,k) = State_VG(1,i,j2_,k) + &
                 sum( (Xyz_DGB(:,i,j,k,iBlock) - Xyz_DGB(:,i,j2_,k,iBlock)) &
                 *Efield_DGB(:,i,1,k,iBlock))
         end do; end do; end do
      case(4)
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            State_VG(1,i,j,k) = State_VG(1,i,nJm1_,k) + &
                 sum( (Xyz_DGB(:,i,j,k,iBlock) - Xyz_DGB(:,i,nJm1_,k,iBlock)) &
                 *Efield_DGB(:,i,nJ,k,iBlock))
         end do; end do; end do
      case(5)
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            State_VG(1,i,j,k) = State_VG(1,i,j,k2_) + &
                 sum( (Xyz_DGB(:,i,j,k,iBlock) - Xyz_DGB(:,i,j,k2_,iBlock)) &
                 *Efield_DGB(:,i,j,1,iBlock))
         end do; end do; end do
      case(6)
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            State_VG(1,i,j,k) = State_VG(1,i,j,nKm1_) + &
                 sum( (Xyz_DGB(:,i,j,k,iBlock) - Xyz_DGB(:,i,j,nKm1_,iBlock)) &
                 *Efield_DGB(:,i,j,nK,iBlock))
         end do; end do; end do
      end select

    end subroutine set_gradpot_bc
    !==========================================================================
    subroutine set_float_bc(iVarMin, iVarMax, iSide, iMin, iMax, &
         jMin, jMax, kMin, kMax, nVarState, State_VG)

      ! Continuous: ghost = phys1
      integer, intent(in) :: iVarMin, iVarMax, iSide, nVarState, &
           iMin, iMax, jMin, jMax, kMin, kMax
      real, intent(inout):: State_VG(nVarState,MinI:MaxI,MinJ:MaxJ,MinK:MaxK)
      integer:: i, j, k
      !------------------------------------------------------------------------
      select case(iSide)
      case(1)
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            State_VG(iVarMin:iVarMax,i,j,k) = &
                 State_VG(iVarMin:iVarMax,1,j,k)
         end do; end do; end do
      case(2)
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            State_VG(iVarMin:iVarMax,i,j,k) = &
                 State_VG(iVarMin:iVarMax,nI,j,k)
         end do; end do; end do
      case(3)
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            State_VG(iVarMin:iVarMax,i,j,k) = &
                 State_VG(iVarMin:iVarMax,i,1,k)
         end do; end do; end do
      case(4)
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            State_VG(iVarMin:iVarMax,i,j,k) = &
                 State_VG(iVarMin:iVarMax,i,nJ,k)
         end do; end do; end do
      case(5)
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            State_VG(iVarMin:iVarMax,i,j,k) = &
                 State_VG(iVarMin:iVarMax,i,j,1)
         end do; end do; end do
      case(6)
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            State_VG(iVarMin:iVarMax,i,j,k) = &
                 State_VG(iVarMin:iVarMax,i,j,nK)
         end do; end do; end do
      end select
    end subroutine set_float_bc
    !==========================================================================
    subroutine set_shear_bc

      use ModPhysics, ONLY: ShockSlope

      ! Shear: ghost = phys(shifted)

      integer:: i, j, k, Di, Dj

      character(len=*), parameter:: NameSub = 'set_shear_bc'
      !------------------------------------------------------------------------
      ! For the corners and boundaries 5 and 6 fill with unsheared data first
      call set_float_bc(1, nVarState, iSide, iMin, iMax, jMin, jMax, &
           kMin, kMax, nVarState, State_VG)

      ! If the shock is not tilted, there is nothing to do
      if(ShockSlope == 0.0) RETURN

      ! Shear according to ShockSlope
      if(ShockSlope < 0.0)then
         call stop_mpi('ShockSlope must be positive!')
      elseif(ShockSlope > 1.1)then

         if(nRoot_D(x_) > 1)call stop_mpi(NameSub// &
              ': shear boundary condition does not work with '// &
              'shockSlope > 1 and multiple blocks along X')

         Di = nint(ShockSlope)
         Dj = 1
         if(abs(Di - ShockSlope) > 1e-6)&
              call stop_mpi('ShockSlope > 1 should be a round number!')
      else
         ! ShockSlope < 1
         Di = 1
         Dj = nint(1/ShockSlope)
         if(abs(Dj - 1/ShockSlope) > 1e-6)call stop_mpi( &
              'ShockSlope < 1 should be the inverse of a round number!')
      end if

      select case(iSide)
      case(1)
         ! Shift by +Di,-Dj
         do k = kMin, kMax; do j = jMin + Dj, jMax; do i = iMax, iMin, -1
            State_VG(:,i,j,k) = State_VG(:,i+Di,j-Dj,k)
         end do; end do; end do
      case(2)
         ! Shift by -Di,+Dj
         do k = kMin, kMax; do j = jMax - Dj, jMin, -1; do i = iMin, iMax
            State_VG(:,i,j,k) = State_VG(:,i-Di,j+Dj,k)
         end do; end do; end do
      case(3)
         ! Shift by -Di,+Dj
         do k = kMin, kMax; do j = jMax, jMin, -1; do i = iMin + Di, iMax
            State_VG(:,i,j,k) = State_VG(:,i-Di,j+Dj,k)
         end do; end do; end do
      case(4)
         ! Shift by +Di,-Dj
         do k = kMin, kMax; do j = jMin, jMax; do i = iMax - Di, iMin, -1
            State_VG(:,i,j,k) = State_VG(:,i+Di,j-Dj,k)
         end do; end do; end do
      end select

    end subroutine set_shear_bc
    !==========================================================================
    subroutine set_symm_bc(iVarMin, iVarMax, Coeff_V)

      ! Symmetry with optional sign change: ghost_i = Coeff*phys_i

      integer, intent(in):: iVarMin, iVarMax
      real,    intent(in):: Coeff_V(iVarMin:iVarMax)

      integer:: i, j, k
      !------------------------------------------------------------------------
      select case(iSide)
      case(1)
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            State_VG(iVarMin:iVarMax,i,j,k) = &
                 Coeff_V*State_VG(iVarMin:iVarMax,1-i,j,k)
         end do; end do; end do
      case(2)
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            State_VG(iVarMin:iVarMax,i,j,k) = &
                 Coeff_V*State_VG(iVarMin:iVarMax,2*nI+1-i,j,k)
         end do; end do; end do
      case(3)
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            State_VG(iVarMin:iVarMax,i,j,k) = &
                 Coeff_V*State_VG(iVarMin:iVarMax,i,1-j,k)
         end do; end do; end do
      case(4)
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            State_VG(iVarMin:iVarMax,i,j,k) = &
                 Coeff_V*State_VG(iVarMin:iVarMax,i,2*nJ+1-j,k)
         end do; end do; end do
      case(5)
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            State_VG(iVarMin:iVarMax,i,j,k) = &
                 Coeff_V*State_VG(iVarMin:iVarMax,i,j,1-k)
         end do; end do; end do
      case(6)
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            State_VG(iVarMin:iVarMax,i,j,k) = &
                 Coeff_V*State_VG(iVarMin:iVarMax,i,j,2*nK+1-k)
         end do; end do; end do
      end select

    end subroutine set_symm_bc
    !==========================================================================
    subroutine set_reflect_bc(nVector, iVector_I)

      ! Copy variables in a symmetric fashion, and then
      ! reflect the normal component of all vector variables

      use BATL_lib, ONLY: Xyz_DGB

      integer, intent(in):: nVector
      integer, allocatable:: iVector_I(:)

      integer:: iVector, iVarX, iVarZ
      integer:: i, j, k, iL, iR, jL, jR, kL, kR
      real :: Normal_D(MaxDim), VarNormal_D(MaxDim), InvNormal2
      !------------------------------------------------------------------------
      ! Scalars are symmetric
      SymmCoeff_V = 1.0
      call set_symm_bc(1, nVarState, SymmCoeff_V)

      select case(iSide)
      case(1,2)
         if(iSide == 1)then
            iL = 0; iR = 1
         else
            iL = nI; iR = nI+1
         end if

         do k = kMin, kMax; do j = jMin, jMax
            ! Face normal is parallel with vector connecting the cell centers
            Normal_D = Xyz_DGB(:,iR,j,k,iBlock) - Xyz_DGB(:,iL,j,k,iBlock)
            InvNormal2 = 1/sum(Normal_D**2)
            do iVector = 1, nVector
               iVarX = iVector_I(iVector); iVarZ = iVarX + 2
               do i = iMin, iMax
                  VarNormal_D = InvNormal2* &
                       sum(Normal_D*State_VG(iVarX:iVarZ,i,j,k))*Normal_D
                  State_VG(iVarX:iVarZ,i,j,k) = &
                       State_VG(iVarX:iVarZ,i,j,k) - 2*VarNormal_D
               end do
            end do
         end do; end do
      case(3,4)
         if(iSide == 3)then
            jL = 0; jR = 1
         else
            jL = nJ; jR = nJ+1
         end if

         do k = kMin, kMax; do i = iMin, iMax
            ! Face normal is parallel with vector connecting the cell centers
            Normal_D = Xyz_DGB(:,i,jR,k,iBlock) - Xyz_DGB(:,i,jL,k,iBlock)
            InvNormal2 = 1/sum(Normal_D**2)
            do iVector = 1, nVector
               iVarX = iVector_I(iVector); iVarZ = iVarX + 2
               do j = jMin, jMax
                  VarNormal_D = InvNormal2* &
                       sum(Normal_D*State_VG(iVarX:iVarZ,i,j,k))*Normal_D
                  State_VG(iVarX:iVarZ,i,j,k) = &
                       State_VG(iVarX:iVarZ,i,j,k) - 2*VarNormal_D
               end do
            end do
         end do; end do
      case(5,6)
         if(iSide == 5)then
            kL = 0; kR = 1
         else
            kL = nK; kR = nK+1
         end if

         do j = jMin, jMax; do i = iMin, iMax
            ! Face normal is parallel with vector connecting the cell centers
            Normal_D = Xyz_DGB(:,i,j,kR,iBlock) - Xyz_DGB(:,i,j,kL,iBlock)
            InvNormal2 = 1/sum(Normal_D**2)
            do iVector = 1, nVector
               iVarX = iVector_I(iVector); iVarZ = iVarX + 2
               do k = kMin, kMax
                  VarNormal_D = InvNormal2* &
                       sum(Normal_D*State_VG(iVarX:iVarZ,i,j,k))*Normal_D
                  State_VG(iVarX:iVarZ,i,j,k) = &
                       State_VG(iVarX:iVarZ,i,j,k) - 2*VarNormal_D
               end do
            end do
         end do; end do
      end select

    end subroutine set_reflect_bc
    !==========================================================================
    subroutine set_fixed_bc(iVarMin, iVarMax, State_V, iMin, iMax, &
         jMin, jMax, kMin, kMax, nVarState, State_VG)

      ! ghost = State_V
      integer, intent(in):: iVarMin, iVarMax, iMin, iMax, &
           jMin, jMax, kMin, kMax
      real,    intent(in):: State_V(iVarMin:iVarMax)
      integer, intent(in):: nVarState
      real, intent(inout):: State_VG(nVarState,MinI:MaxI,MinJ:MaxJ,MinK:MaxK)

      integer:: i, j, k
      !------------------------------------------------------------------------
      do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
         State_VG(iVarMin:iVarMax,i,j,k) = State_V
      end do; end do; end do
    end subroutine set_fixed_bc
    !==========================================================================
    subroutine set_fixed_semi_bc

      use ModAdvance, ONLY: UseIdealEos, UseElectronPressure
      use ModMultiFluid, ONLY: iRhoIon_I, iPion_I, UseMultiIon, ChargeIon_I, &
           MassIon_I
      use ModSemiImplVar, ONLY: UseSemiHallResist, UseSemiResistivity, iTeImpl
      use ModPhysics, ONLY: AverageIonCharge, PePerPtotal, &
           ElectronPressureRatio

      integer :: i, j, k
      real :: Ne, Pe

      character(len=*), parameter:: NameSub = 'set_fixed_semi_bc'
      !------------------------------------------------------------------------
      if(UseSemiHallResist .or. UseSemiResistivity)then
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            State_VG(BxImpl_:BzImpl_,i,j,k) = CellState_VI(Bx_:Bz_,iSide)
         end do; end do; end do
         ! Subtract B0:   B1 = B - B0
         if(UseB0) &
              call fix_b0(BxImpl_, BzImpl_, iBlock, iMin, iMax, jMin, jMax, &
              kMin, kMax, nVarState, State_VG)
      elseif(iTeImpl > 0)then
         do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
            if(.not.UseIdealEos)then
               call stop_mpi(NameSub//': non-ideal not yet implemented')
            elseif(UseMultiIon)then
               Ne = sum(ChargeIon_I*CellState_VI(iRhoIon_I,iSide)/MassIon_I)
            else
               Ne = CellState_VI(Rho_,iSide)*AverageIonCharge/MassIon_I(1)
            end if
            if(.not.UseIdealEos .and. .not.UseElectronPressure)then
               call stop_mpi(NameSub//': non-ideal not yet implemented')
            elseif(UseElectronPressure)then
               Pe = CellState_VI(Pe_,iSide)
            elseif(IsMhd)then
               Pe = CellState_VI(p_,iSide)*PePerPtotal
            else
               Pe = sum(CellState_VI(iPIon_I,iSide))*ElectronPressureRatio
            end if
            State_VG(iTeImpl,i,j,k) = Pe/Ne
         end do; end do; end do
      else
         call stop_mpi(NameSub// &
              ': not working for TypeSemiImplicit='//TypeSemiImplicit)
      end if

    end subroutine set_fixed_semi_bc
    !==========================================================================
    subroutine fix_b0(iVarMin, iVarMax, iBlock, iMin, iMax, &
         jMin, jMax, kMin, kMax, nVarState, State_VG)

      use ModB0, ONLY: B0_DGB

      integer, intent(in) :: iVarMin, iVarMax, iBlock, nVarState, &
           iMin, iMax, jMin, jMax, kMin, kMax
      real, intent(inout):: State_VG(nVarState,MinI:MaxI,MinJ:MaxJ,MinK:MaxK)

      ! Set B = B - B0 in ghost cells

      integer:: i, j, k
      !------------------------------------------------------------------------
      do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
         State_VG(iVarMin:iVarMax,i,j,k) =  &
              State_VG(iVarMin:iVarMax,i,j,k) - B0_DGB(:,i,j,k,iBlock)
      end do; end do; end do

    end subroutine fix_b0
    !==========================================================================
    subroutine set_solar_wind_bc

      use ModAdvance, ONLY: nVar, UseIdealEos, UseElectronPressure
      use ModGeometry, ONLY: xMinBox, xMaxBox, &
           yMinBox, yMaxBox, zMinBox, zMaxBox
      use ModB0, ONLY: B0_DGB
      use ModMultiFluid, ONLY: iRho_I, iRhoIon_I, iUx_I, iUy_I, iUz_I, &
           iPion_I, UseMultiIon, ChargeIon_I, MassIon_I
      use ModSolarwind, ONLY: get_solar_wind_point
      use ModMain, ONLY: tSimulation
      use BATL_lib, ONLY: Xyz_DGB, IsCartesianGrid
      use ModSemiImplVar, ONLY: UseSemiHallResist, UseSemiResistivity, iTeImpl
      use ModPhysics, ONLY: AverageIonCharge, PePerPtotal, &
           ElectronPressureRatio

      ! index and location of a single point
      integer :: i, j, k
      real :: Xyz_D(MaxDim)

      ! Varying solar wind parameters
      real :: SolarWind_V(nVar)

      real :: Ne, Pe

      ! logical :: DoTest, DoTestMe
      ! call set_oktest(NameSub, DoTest, DoTestMe)

      character(len=*), parameter:: NameSub = 'set_solar_wind_bc'
      !------------------------------------------------------------------------
      do k = kMin, kMax; do j = jMin, jMax; do i = iMin, iMax
         Xyz_D =  Xyz_DGB(:,i,j,k,iBlock)
         if(IsCartesianGrid)then
            ! Put the solar wind to the edge of the computational domain
            select case(iSide)
            case(1)
               Xyz_D(x_) = xMinBox
            case(2)
               Xyz_D(x_) = xMaxBox
            case(3)
               Xyz_D(y_) = yMinBox
            case(4)
               Xyz_D(y_) = yMaxBox
            case(5)
               Xyz_D(z_) = zMinBox
            case(6)
               Xyz_D(z_) = zMaxBox
            end select
         end if
         call get_solar_wind_point(tSimulation, Xyz_D, SolarWind_V)

         if(present(iImplBlock))then
            if(UseSemiHallResist .or. UseSemiResistivity)then
               State_VG(BxImpl_:BzImpl_,i,j,k) = SolarWind_V(Bx_:Bz_)
               ! Subtract B0:   B1 = B - B0
               if(UseB0) State_VG(BxImpl_:BzImpl_,i,j,k) = &
                    State_VG(BxImpl_:BzImpl_,i,j,k) - B0_DGB(:,i,j,k,iBlock)
            elseif(iTeImpl > 0)then
               if(.not.UseIdealEos)then
                  call stop_mpi(NameSub//': non-ideal not yet supported')
               elseif(UseMultiIon)then
                  Ne = sum(ChargeIon_I*Solarwind_V(iRhoIon_I)/MassIon_I)
               else
                  Ne = Solarwind_V(Rho_)*AverageIonCharge/MassIon_I(1)
               end if
               if(.not.UseIdealEos .and. .not.UseElectronPressure)then
                  call stop_mpi(NameSub//': non-ideal not yet supported')
               elseif(UseElectronPressure)then
                  Pe = SolarWind_V(Pe_)
               elseif(IsMhd)then
                  Pe = SolarWind_V(p_)*PePerPtotal
               else
                  Pe = sum(SolarWind_V(iPIon_I))*ElectronPressureRatio
               end if
               State_VG(iTeImpl,i,j,k) = Pe/Ne
            end if
         else
            State_VG(:,i,j,k) = SolarWind_V

            ! Convert velocities to momenta
            State_VG(iRhoUx_I, i,j,k) = &
                 State_VG(iUx_I, i,j,k)*State_VG(iRho_I,i,j,k)
            State_VG(iRhoUy_I, i,j,k) = &
                 State_VG(iUy_I, i,j,k)*State_VG(iRho_I,i,j,k)
            State_VG(iRhoUz_I, i,j,k) = &
                 State_VG(iUz_I, i,j,k)*State_VG(iRho_I,i,j,k)

            ! Subtract B0:   B1 = B - B0
            if(UseB0) State_VG(Bx_:Bz_,i,j,k) = &
                 State_VG(Bx_:Bz_,i,j,k) - B0_DGB(:,i,j,k,iBlock)
         end if
      end do; end do; end do

    end subroutine set_solar_wind_bc
    !==========================================================================
    subroutine set_solar_wind_bc_buffer

      use ModVarIndexes, ONLY: Bx_, Bz_
      use ModB0, ONLY: B0_DGB
      use BATL_lib, ONLY: Xyz_DGB

      ! index and location of a single point
      integer :: i, j, k
      real    :: y, z
      !------------------------------------------------------------------------
      do k = kMin, kMax
         z = Xyz_DGB(z_,1,1,k,iBlock)
         do j = jMin, jMax
            y = Xyz_DGB(y_,1,j,1,iBlock)
            do i = iMin, iMax
               call read_ih_buffer(y,z,State_VG(:,i,j,k))   ! ^CMP IF IH
               ! Subtract B0
               State_VG(Bx_:Bz_,i,j,k) = State_VG(Bx_:Bz_,i,j,k) &
                    - B0_DGB(:,i,j,k,iBlock)
            end do
         end do
      end do

    end subroutine set_solar_wind_bc_buffer
    !==========================================================================
    subroutine set_radiation_outflow_bc(iVarMin, iVarMax, iSide)

      use ModAdvance, ONLY: nWave
      use ModPhysics, ONLY: Si2No_V, UnitX_
      use ModUserInterface ! user_material_properties
      use BATL_lib, ONLY: CellSize_DB

      integer, intent(in) :: iVarMin, iVarMax, iSide

      integer:: iVar, i, j, k, iWave
      integer:: i1G, j1G, k1G ! index for first ghost cell layer
      integer:: i2G, j2G, k2G ! index for last  ghost cell layer
      integer:: i1P, j1P, k1P ! index for first physical cell layer
      integer:: i2P, j2P, k2P ! index for second physical cell layer
      integer:: Di, Dj, Dk    ! +1 if last ghost cell layer has larger index
      !                         than first ghost cell layer and -1 otherwise

      real   :: OpacityRosselandSi_W(nWave), Coef
      !------------------------------------------------------------------------
      select case(iSide)
      case(1, 2)
         if(iSide == 1)then
            i1P = 1; i2P = 2; i1G = 0; i2G = iMin; Di = -1
         else
            i1P = nI; i2P = nI-1; i1G = nI+1; i2G = iMax; Di = +1
         end if
         do k = 1, nK; do j = 1, nJ
            call user_material_properties(State_VG(:,i1P,j,k), &
                 i1P, j, k, iBlock, OpacityRosselandOut_W=OpacityRosselandSi_W)

            do iVar = iVarMin, iVarMax
               iWave = iVar - iVarMin + 1
               Coef = 2/sqrt( (3*OpacityRosselandSi_W(iWave) &
                    /          Si2No_V(UnitX_) * CellSize_DB(x_,iBlock))**2 &
                    + ((State_VG(iVar,i2p,j,k) &
                    -   State_VG(iVar,i1p,j,k)) &
                    /  State_VG(iVar,i1p,j,k))**2)
               State_VG(iVar,i1G,j,k) = State_VG(iVar,i1P,j,k) &
                    *(Coef - 0.5)/(Coef + 0.5)
               ! Extrapolate linearly to the rest of the ghost cells
               do i = i1G + Di, i2G, Di
                  State_VG(iVar,i,j,k) &
                       = 2*State_VG(iVar,i-Di,j,k) &
                       -   State_VG(iVar,i-2*Di,j,k)
               end do
            end do
         end do; end do

      case(3, 4)
         if(iSide == 3)then
            j1P = 1; j2P = 2; j1G = 0; j2G = jMin; Dj = -1
         else
            j1P = nJ; j2P = nJ-1; j1G = nJ+1; j2G = jMax; Dj = +1
         end if
         do k = 1, nK; do i = 1, nI
            call user_material_properties(State_VG(:,i,j1p,k), &
                 i, j1p, k, iBlock, OpacityRosselandOut_W=OpacityRosselandSi_W)

            do iVar = iVarMin, iVarMax
               iWave = iVar - iVarMin + 1
               Coef = 2/sqrt( (3*OpacityRosselandSi_W(iWave) &
                    /          Si2No_V(UnitX_) * CellSize_DB(y_,iBlock))**2 &
                    + ((State_VG(iVar,i,j2p,k) &
                    -   State_VG(iVar,i,j1p,k)) &
                    /  State_VG(iVar,i,j1p,k))**2)
               State_VG(iVar,i,j1g,k) = State_VG(iVar,i,j1p,k)&
                    *(Coef - 0.5)/(Coef + 0.5)
               ! Extrapolate linearly to the rest of the ghost cells
               do j = j1G + Dj, j2G, Dj
                  State_VG(iVar,i,j,k) &
                       = 2*State_VG(iVar,i,j-Dj,k) &
                       -   State_VG(iVar,i,j-2*Dj,k)
               end do
            end do
         end do; end do

      case(5, 6)
         if(iSide == 5)then
            k1P = 1; k2P = 2; k1G = 0; k2G = kMin; Dk = -1
         else
            k1P = nK; k2P = nK-1; k1G = nK+1; k2G = kMax; Dk = +1
         end if
         do j = 1, nJ; do i = 1, nI
            call user_material_properties(State_VG(:,i,j,k1p), &
                 i, j, k1p, iBlock, OpacityRosselandOut_W=OpacityRosselandSi_W)

            do iVar = iVarMin, iVarMax
               iWave = iVar - iVarMin + 1
               Coef = 2/sqrt( (3*OpacityRosselandSi_W(iWave) &
                    /          Si2No_V(UnitX_) * CellSize_DB(z_,iBlock))**2 &
                    + ((State_VG(iVar,i,j,k2p) &
                    -   State_VG(iVar,i,j,k1p)) &
                    /  State_VG(iVar,i,j,k1p))**2)
               State_VG(iVar,i,j,k1g) = State_VG(iVar,i,j,k1p) &
                    *(Coef - 0.5)/(Coef + 0.5)
               ! Extrapolate linearly to the rest of the ghost cells
               do k = k1G + Dk, k2G, Dk
                  State_VG(iVar,i,j,k) &
                       = 2*State_VG(iVar,i,j,k-Dk) &
                       -   State_VG(iVar,i,j,k-2*Dk)
               end do
            end do
         end do; end do
      end select

    end subroutine set_radiation_outflow_bc
    !==========================================================================
  end subroutine set_cell_boundary
  !============================================================================
  subroutine set_edge_corner_ghost(nGhost,iBlock, nVarState, State_VG)
    ! The blocks near boundary may have problem with high order resolution
    ! change and high order AMR. These blocks' corner/edge ghost cells
    ! may out of boundary and can not receive data from any block. Set
    ! these ghost cells with the closest face ghost cell values.

    ! This approach is not accurate! If it is smooth near the resolution
    ! change boundary point, it will be fine.

    use BATL_lib, ONLY: MinI,MaxI,MinJ,MaxJ,MinK,MaxK,DiLevelNei_IIIB,Unset_
    use BATL_size, ONLY: nI,nJ,nK

    integer, intent(in):: nGhost
    integer, intent(in):: iBlock
    integer, intent(in):: nVarState
    real, intent(inout):: State_VG(nVarState,MinI:MaxI,MinJ:MaxJ,MinK:MaxK)

    integer:: i, j, k, iDir,jDir,kDir,kDirBegin,kDirEnd
    integer:: iBegin,iEnd,jBegin,jEnd,kBegin,kEnd,Di,Dj,Dk,i0,j0,k0

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_edge_corner_ghost'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    if(nJ == 1) RETURN
    if(nK == 1) then
       kDirBegin = 0; kDirEnd = 0
    else
       kDirBegin = -1; kDirEnd = 1
    endif

    do kDir = kDirBegin,kDirEnd; do jDir = -1, 1; do iDir = -1, 1
       if(abs(iDir) + abs(jDir) + abs(kDir) /=1) CYCLE
       if(DiLevelNei_IIIB(iDir,jDir,kDir,iBlock) /=Unset_) CYCLE

       if(iDir/=0) then
          if(iDir == 1) then
             iBegin = nI + 1; iEnd = nI + nGhost; Di = 1
          else
             iBegin = 0; iEnd = 1-nGhost; Di = -1
          endif

          do k = MinK, MaxK; do j = MinJ, MaxJ; do i = iBegin, iEnd, Di
             i0 = i; j0 = j; k0 = k
             if(j<1) then
                j0 = 1
             elseif(j>nJ) then
                j0 = nJ
             endif
             if(k <1) then
                k0 = 1
             elseif(k>nK) then
                k0 = nK
             endif
             if(abs(State_VG(1,i,j,k) - 777)<1e-10 .and. &
                  abs(State_VG(nVarState,i,j,k) - 777)<1e-10) &
                  State_VG(:,i,j,k) = State_VG(:,i0,j0,k0)
          enddo; enddo; enddo
       endif ! iDir /=0

       if(jDir /=0) then
          if(jDir == 1) then
             jBegin = nJ+1; jEnd = nJ+nGhost; Dj = 1
          else
             jBegin = 0; jEnd = 1-nGhost; Dj = -1
          endif

          do k = MinK, MaxK; do j = jBegin,jEnd,Dj; do i = MinI, MaxI
             i0 = i; j0=j; k0=k
             if(i <1)then
                i0 = 1
             elseif(i>nI) then
                i0 = nI
             endif
             if(k <1) then
                k0 = 1
             elseif(k>nK) then
                k0 = nK
             endif
             if(abs(State_VG(1,i,j,k) - 777)<1e-10 .and. &
                  abs(State_VG(nVarState,i,j,k) - 777)<1e-10) &
                  State_VG(:,i,j,k) = State_VG(:,i0,j0,k0)
          enddo; enddo; enddo
       endif ! jDir /=0

       if(kDir /=0) then
          if(kDir == 1) then
             kBegin = nK + 1; kEnd = nk + nGhost; Dk = 1
          else
             kBegin = 0; kEnd = 1 - nGhost; Dk = -1
          endif

          do k = kBegin, kEnd, Dk; do j = MinJ, MaxJ; do i = MinI, MaxI
             i0 = i; j0=j; k0=k
             if(i <1)then
                i0 = 1
             elseif(i>nI) then
                i0 = nI
             endif
             if(j<1) then
                j0 = 1
             elseif(j>nJ) then
                j0 = nJ
             endif
             if(abs(State_VG(1,i,j,k) - 777)<1e-10 .and. &
                  abs(State_VG(nVarState,i,j,k) - 777)<1e-10) &
                  State_VG(:,i,j,k) = State_VG(:,i0,j0,k0)
          enddo; enddo; enddo
       endif ! kDir /=0
    enddo; enddo; enddo

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine set_edge_corner_ghost
  !============================================================================
end module ModCellBoundary
!==============================================================================
