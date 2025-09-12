!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModSemiImplVar

  implicit none

  ! Semi-implicit variables that cannot be in ModSemiImplicit
  ! because it would create circular dependencies

  logical, public:: UseSemiImplicit = .false.
  logical, public:: UseSplitSemiImplicit = .false.
  !$acc declare create(UseSplitSemiImplicit)
  character(len=40), public:: TypeSemiImplicit = 'none'
  !$acc declare create(TypeSemiImplicit)

  ! Do semi-implicit Hall and/or regular resistivity
  logical:: UseSemiHallResist  = .false.
  logical:: UseSemiResistivity = .false.

  ! Do the semi-implicit blocks change dynamically?
  logical, public:: IsDynamicSemiImpl = .false.

  ! Number of all semi-implicit variables
  integer, public:: nVarSemiAll
  !$acc declare create(nVarSemiAll)

  ! Index range of semi-implicit variables solved together
  integer, public:: iVarSemiMin, iVarSemiMax
  !$acc declare create(iVarSemiMin, iVarSemiMax)

  ! Number of semi-implicit variables solved together
  integer, public:: nVarSemi
  !$acc declare create(nVarSemi)

  ! Number of vectors and indexes of first components among semi-impl vars
  integer, public:: nVectorSemi = 0
  integer, public, allocatable:: iVectorSemi_I(:)

  ! Named indices for semi-implicit variables
  integer, public :: iTeImpl=0, iErImplFirst=0, iErImplLast=0, iEradImpl=0
  !$acc declare create(iTeImpl)

  ! Number of semi-implicit grid blocks
  integer, public:: nBlockSemi = -1
  !$acc declare create(nBlockSemi)

  ! Conversion from compact block index iBlockSemi to normal index iBlock
  integer, public, allocatable:: iBlockFromSemi_B(:)
  !$acc declare create(iBlockFromSemi_B)

  ! Arrays for flux correction at resolution changes
  real, public, allocatable, dimension(:,:,:,:,:) :: &
       FluxImpl_VXB, FluxImpl_VYB, FluxImpl_VZB

  ! Increase accurary of radiation solver by switching to third order
  ! interpolation at the resolution change
  logical, public:: UseAccurateRadiation = .false.

  ! Coefficient of the implicit part: 1 - Backward Euler, 0.5 - trapezoidal
  real, public:: SemiImplCoeff = 1.0
  !$acc declare create(SemiImplCoeff)

  ! This array is indexed with normal block index and has nG ghost cells
  real, allocatable:: SemiState_VGB(:,:,:,:,:)  ! Semi-implicit vars
  !$acc declare create(SemiState_VGB)

  real, allocatable:: SemiStateTmp_VGI(:,:,:,:,:)
  !$acc declare create(SemiStateTmp_VGI)

  ! Jacobian matrix for general grid: Dgencoord/Dcartesian
  real, allocatable:: DcoordDxyz_DDFDI(:,:,:,:,:,:,:)
  !$acc declare create(DcoordDxyz_DDFDI)

  ! Transverse gradients
  real, allocatable:: TransGrad_DDGI(:,:,:,:,:,:)
  !$acc declare create(TransGrad_DDGI)

  ! These arrays with *Semi_* and *SemiAll_* are indexed by compact iBlockSemi
  ! and have a single ghost cell at most.
  ! The SemiAll_ variables are indexed from 1..nVarSemiAll
  real, allocatable:: DconsDsemiAll_VCB(:,:,:,:,:) ! dCons/dSemi derivatives
  !$acc declare create(DconsDsemiAll_VCB)
  real, allocatable:: SemiAll_VCB(:,:,:,:,:)       ! Semi-implicit vars
  !$acc declare create(SemiAll_VCB)
  real, allocatable:: NewSemiAll_VCB(:,:,:,:,:)    ! Updated semi-impl vars
  !$acc declare create(NewSemiAll_VCB)
  real, allocatable:: ResSemi_VCB(:,:,:,:,:)       ! Result of Matrix(Semi)
  !$acc declare create(ResSemi_VCB)
  real, allocatable:: JacSemi_VVCIB(:,:,:,:,:,:,:) ! Jacobian/preconditioner
  !$acc declare create(JacSemi_VVCIB)

  ! Store Difference of U^* (after explicit update)  and U^n.
  real, allocatable:: DeltaSemiAll_VCB(:,:,:,:,:)

end module ModSemiImplVar
!==============================================================================

module ModImplicit

  use ModSize
  use BATL_lib, ONLY: iProc, iComm, nDim
  use ModVarIndexes, ONLY: nVar, P_
  use ModIO, ONLY: iUnitOut, write_prefix

  use ModLinearSolver, ONLY: LinearSolverParamType

  ! for sake of fewer changes in the user modules
  use ModSemiImplVar, StateSemi_VGB => SemiState_VGB

  implicit none
  SAVE

  ! Number of diagonals for the semi-implicit scheme
  ! can be different from nStencil
  integer:: nDiagSemi = 2*nDim + 1
  !$acc declare create(nDiagSemi)
  ! Heptadiagonal Jacobian matrix
  integer, parameter:: nStencil = 2*nDim + 1
  integer, parameter:: Stencil1_ = 1, Stencil2_ = 2, Stencil3_ = 3, &
       Stencil4_ = min(4,nStencil), Stencil5_ = min(5,nStencil), &
       Stencil6_ = min(6,nStencil), Stencil7_ = min(7,nStencil)

  ! Number of variables and the index of energy (replacing pressure)
  integer, parameter :: E_ = P_

  ! Logicals determining if and what implcit scheme is used
  logical :: UseImplicit = .false.

  ! Use fully implicit scheme (all blocks)
  logical :: UseFullImplicit=.false.

  ! Use part implicit scheme (some blocks)
  logical :: UsePartImplicit=.false.

  ! Use temporally 2nd order part implicit by doing explicit predictor step
  ! in all the blocks to get second order flux at expl/impl interfaces
  logical :: UsePartImplicit2=.false.

  ! Shall we zero out contribution from ghost cells
  logical :: UseNoOverlap = .true.
  !$acc declare create(UseNoOverlap)

  ! True while doing the implicit update
  logical :: IsImplicitUpdate = .false.

  ! Parameters for selecting implicit blocks
  character (len=10) :: TypeImplCrit = 'dt'
  real               :: Rimplicit

  ! Second order in time implicit scheme (uses previous time step)
  logical:: UseBDF2 = .false.

  ! Time step when the previous state was stored
  integer :: nStepPrev=-100

  ! Previous (n-1) state
  real, allocatable :: ImplOld_VCB(:,:,:,:,:)

  ! Previous time step
  real ::  DtPrev = -1.0

  ! CFL number for explicit scheme
  real :: ExplCfl = 0.8

  ! Implicit scheme
  character(len=10) :: TypeFluxImpl = 'default' ! same as explicit

end module ModImplicit
!==============================================================================
