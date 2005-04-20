!^CFG COPYRIGHT UM
!^CFG FILE USERFILES
!========================================================================
!========================================================================
!  This file contains routines that let the user make customizations to
!  BATSRUS without having to make changes in the "kernel" of the code.
!  Each of the routines is accessed in a run but setting the appropriate
!  input flag in the PARAM.in file.  Also included here is a ModUser
!  module.  Variables that are needed in more than one user routine (in
!  otherwords, if they must be global) can be defined here and accessed
!  in the other user subroutines.
!
!  In each of the routines there are sections for the user to place 
!  variable definitions and to put code to compute the desired quantity.
!  Note that in many routines (user_set_inner_BCs for example) the 
!  solution to a problem will be greatly affected by changes the user
!  introduces.  Often we suggest using one of the BATSRUS defaults rather
!  then coding a unique option.  However, if the user chooses to create
!  a user routine, we suggest that you follow the format of the examples
!  give below very carefully.  At the very least, the user should take
!  care to set the global variables that the routine requires to be set
!  and no others.  This will happen if the user adhears to the examples 
!  given and does not modify the looping structure or the sections of the 
!  subroutine which are documented as parts not to change.
!========================================================================
!========================================================================

!========================================================================
!  MODULE ModUser
!========================================================================
!\
! This module is where the user should define any variables that are
! needed in more than a single routine.  In otherwords, variables which
! need to be global to all the subroutines in this file.  
!/
Module ModUser
  use ModNumConst, ONLY: cHalf,cTwo,cThree,&
       cFour,cE1,cHundred,cHundredth,cZero,&
       cOne
  use ModMain,     ONLY: UseUserB0,UseUserHeating
  use ModSize,     ONLY: nI,nJ,nK,gcn,nBLK
  implicit none

  SAVE
  !\
  ! PFSSM related variables::
  !/
  ! Maximum order of spherical harmonics
  integer, parameter:: N_PFSSM=90

  ! Weights of the spherical harmonics
  real, dimension(N_PFSSM+1,N_PFSSM+1):: g_nm,h_nm

  ! Temporary variable
  real, dimension(N_PFSSM+1):: FactRatio1

  ! Number of header lines in the file
  integer:: iHead_PFSSM

  ! Logical for Initialization 
  logical:: DoFirst=.true.

  ! Name of the input file
  character (LEN=32):: File_PFSSM

  ! Header string in the input file
  character (LEN=80):: Head_PFSSM

  ! Rotation angle around z-axis, in degrees,
  ! from the coordinate system of the component
  ! towards the coordinate system of the magnetic 
  ! map in the positive direction - hence, is positive. 
  ! Is set automatically to be equal to the 
  ! H(eliographic) L(ongitude) of C(entral) M(eridian)
  ! of the M(ap) minus 180 Deg, if in the input file 
  ! Phi_Shift is negative
  !
  real:: Phi_Shift

  ! Units of the magnetic field in the file including corrections
  ! relative to the magnetic units in the SC (Gauss)
  real :: UnitB


  ! Rs - radius of outer source surface where field is taken to be 0.
  ! Ro - radius of inner boundary for the potential
  ! H  - height of ??
  !  
  ! Typical values are Rs_PFSSM=31.0; Ro_PFSSM=1.0; H_PFSSM=0.1::
  real:: R_PFSSM,Rs_PFSSM,Ro_PFSSM,H_PFSSM

  !\
  ! Parameters related to the empirical heating::
  !/
  real:: InvH0,DegF_Ratio,Dens_Ratio
  real:: DegFrm1,Tnot=cOne
  real:: MaxB0_1,MaxB0_2,Bnot
  !\
  ! Gibson & Low 1998 related variables::
  !/
  logical:: DoFirst_GL=.true.
  real:: Mrope_GL98
  !\
  ! The following are needed in user_sources::
  !/
  real, dimension(1:nI,1:nJ,1:nK):: &
       Srho,SrhoUx,SrhoUy,SrhoUz,SBx,SBy,SBz,Sp,SEr,SE
end Module ModUser
!========================================================================
!  SUBROUTINE user_read_inputs
!========================================================================

!\
! This subroutine allows the user to add input commands to the PARAM.in
! file that are specific to an application.  Although the user does not
! have to read input in the same manner as BATSRUS (with a #COMMAND
! followed by the data), the following example is set up to do this.
! This method is encouraged because it allows flexibility and it also
! will echo PARAM.in to standard out the same as the rest of the input
! file.
!
! In the PARAM.in file user commands need to be enclosed in the
! #USERINPUTBEGIN and #USERINPUTEND commands as follows
!
! #USERINPUTBEGIN
!
! #USERSPECIFIEDCOMMANDS
! ...
!
! #USERINPUTEND
!
! The leading #USERINPUTBEGIN tells BATSRUS to call user_read_inputs.
! The user_read_inputs routine should return to the standard read_inputs
! on reading the command #USERINPUTEND.
!
! The reading routine read_var can be used by the user to read all data
! types. BATSRUS uses a fortran 90 interface to overload the function so
! that it can read real, integer, logical or character strings. The syntax
! is read_var('characterstring',variable).  Where characterstring is
! typically just the variable name or a description of the variable's
! meaning.  For example: read_var('A random character',RandomCharacter)
! The advantage of using read_var is that it advances the line (iline)
! automatically and it also echos the command correctly back to standard
! out the way all other commands are echoed in read_inputs.
!
! The user should declare their specific variables in ModUser above.
!
! As with other user subroutines DO NOT MODIFY ANY GLOBAL VARIABLE DEFINED
! IN THE MODULES INCLUDED IN THIS SUBROUTINE UNLESS SPECIFIED!!
subroutine user_read_inputs
  use ModMain
  use ModProcMH,    ONLY: iProc
  use ModReadParam
  use ModIO,        ONLY: write_prefix, write_myname, iUnitOut
  use ModUser
  implicit none

  integer:: i
  character (len=100) :: NameCommand
  !---------------------------------------------------------------------------

  if(iProc==0.and.lVerbose > 0)then
      call write_prefix; write(iUnitOut,*)'User read_input HELIOSPHERE starts'
  endif
  do
     if(.not.read_line() ) EXIT
     if(.not.read_command(NameCommand)) CYCLE
     select case(NameCommand)
     case("#USER_FLAGS")
        call read_var('UseUserInnerBCs'         ,UseUserInnerBCs)
        call read_var('UseUserSource'           ,UseUserSource)
        call read_var('UseUserPerturbation'     ,UseUserPerturbation)
        call read_var('UseUserOuterBcs'         ,UseUserOuterBcs)
        call read_var('UseUserICs'              ,UseUserICs)
        call read_var('UseUserSpecifyRefinement',UseUserSpecifyRefinement)
        call read_var('UseUserLogFiles'         ,UseUserLogFiles)
        call read_var('UseUserWritePlot'        ,UseUserWritePlot)
        call read_var('UseUserAMR'              ,UseUserAMR)
        call read_var('UseUserEchoInput'        ,UseUserEchoInput)
        call read_var('UseUserB0'               ,UseUserB0)
        call read_var('UseUserSetPhysConst'     ,UseUserSetPhysConst)
        call read_var('UseUserUpdateStates'     ,UseUserUpdateStates)
     case("#USEUSERHEATING")
	call read_var('UseUserHeating'          ,UseUserHeating)
     case("#PFSSM")
        call read_var('UseUserB0'  ,UseUserB0)
        if (UseUserB0) then
           call read_var('Ro_PFSSM'   ,Ro_PFSSM)
           call read_var('Rs_PFSSM'   ,Rs_PFSSM)
           call read_var('H_PFSSM'    ,H_PFSSM)
           call read_var('File_PFSSM' ,File_PFSSM)
           call read_var('iHead_PFSSM',iHead_PFSSM)
           call read_var('Phi_Shift'  ,Phi_Shift)
           call read_var('UnitB'      ,UnitB)
           call read_var('dt_UpdateB0',dt_UpdateB0)
        endif
     case("#AWHEAT")
        call read_var('Bnot        ',Bnot)
        call read_var('Tnot        ',Tnot)
        call read_var('DegFrm1     ',DegFrm1)
        call read_var('DegF_Ratio  ',DegF_Ratio)
        call read_var('Dens_Ratio  ',Dens_Ratio)
     case('#USERINPUTEND')
        if(iProc==0.and.lVerbose > 0)then
           call write_prefix;
           write(iUnitOut,*)'User read_input HELIOSPHERE ends'
        endif
        EXIT
     case default
        if(iProc==0) then
           call write_myname; write(*,*) &
                'ERROR: Invalid user defined #COMMAND in user_read_inputs. '
           write(*,*) '--Check user_read_inputs for errors'
           write(*,*) '--Check to make sure a #USERINPUTEND command was used'
           write(*,*) '  *Unrecognized command was: '//NameCommand
           call stop_mpi('ERROR: Correct PARAM.in or user_read_inputs!')
        end if
     end select
  end do
end subroutine user_read_inputs

subroutine user_heat_source
  use ModMain,     ONLY: globalBLK
  use ModAdvance,  ONLY: qheat_BLK
  use ModNumConst, ONLY: cZero
  implicit none
  qheat_BLK(:,:,:,globalBLK) = cZero
  call heat_source_averages
end subroutine user_heat_source

!========================================================================
!  set_extra_boundary_cells::
!  Allows to define boundary conditions at the user defined boundary.
!  SHOULD define IsBoundaryCell_GI(:,:,:,ExtraBc_) using a boundary
!  condition for iBLK block.
!  EXAMPLE: OUTER SPHERICAL BOUNDARY of radius of 100.
!========================================================================
subroutine set_extra_boundary_cells(iBLK)
  use ModGeometry
  use ModNumConst
  use ModParallel, ONLY: neiLEV,neiLtop, neiLbot,&
       neiLeast,neiLwest,neiLnorth,neiLsouth
  implicit none
  integer, intent(in):: iBLK
  integer:: i2,j2,k2,i,j,k
  real,parameter:: R=1.0,R2=R*R
  IsBoundaryCell_GI(:,:,:,ExtraBc_) = R_BLK(:,:,:,iBLK)<R
  !\
  ! Neighbor solution block refinement levels::
  ! ( 0=neighbors at same level, 
  !  -1=neighbors at lower level,
  !  +1=neighbors at higher level,
  !  NOBLK=no neighbors).
  !/
  if(all(neiLEV(:,iBLK)==0))return
  if(neiLeast(iBLK)==+1)then
     !Make all the ghostcells covered by a coarser cell become body cells,
     !if the coarser cell center is inside the body::
     i2=0
     do k2=2,nK,2
        do j2=2,nJ,2
           IsBoundaryCell_GI(i2-1:i2,j2-1:j2,k2-1:k2,ExtraBc_)=(    &
                (x_BLK(i2,j2,k2,iBLK)-dx_BLK(iBLK)*cHalf)**2+       &
                (y_BLK(i2,j2,k2,iBLK)-dy_BLK(iBLK)*cHalf)**2+       &
                (z_BLK(i2,j2,k2,iBLK)-dz_BLK(iBLK)*cHalf)**2)<R2
        end do
     end do
  elseif(neiLeast(iBLK)==-1)then
     !Make any ghostcell covered by finer cell become a body cell, if
     !any of the finer cell centers is inside the body::
     i=0
     do k=1,nK
        do j=1,nJ
           IsBoundaryCell_GI(i,j,k,ExtraBc_)=&
                (&
                (x_BLK(i,j,k,iBLK)+dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)-dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)-dz_BLK(iBLK)*cQuarter)**2)<R2.or. &
                (&
                (x_BLK(i,j,k,iBLK)+dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)-dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)+dz_BLK(iBLK)*cQuarter)**2)<R2.or. &
                (&
                (x_BLK(i,j,k,iBLK)+dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)+dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)-dz_BLK(iBLK)*cQuarter)**2)<R2.or. &
                (&
                (x_BLK(i,j,k,iBLK)+dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)+dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)+dz_BLK(iBLK)*cQuarter)**2)<R2
        end do
     end do
  end if

 if(neiLwest(iBLK)==+1)then
     !Make all the ghostcells covered by a coarser cell become body cells,
     !if the coarser cell center is inside the body::
     i2=nI+2
     do k2=2,nK,2
        do j2=2,nJ,2
           IsBoundaryCell_GI(i2-1:i2,j2-1:j2,k2-1:k2,ExtraBc_)=(    &
                (x_BLK(i2,j2,k2,iBLK)-dx_BLK(iBLK)*cHalf)**2+       &
                (y_BLK(i2,j2,k2,iBLK)-dy_BLK(iBLK)*cHalf)**2+       &
                (z_BLK(i2,j2,k2,iBLK)-dz_BLK(iBLK)*cHalf)**2)<R2
        end do
     end do
  elseif(neiLwest(iBLK)==-1)then
     !Make any ghostcell covered by finer cell become a body cell, if
     !any of the finer cell centers is inside the body::
     i=nI+1
     do k=1,nK
        do j=1,nJ
           IsBoundaryCell_GI(i,j,k,ExtraBc_)=&
                (&
                (x_BLK(i,j,k,iBLK)-dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)-dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)-dz_BLK(iBLK)*cQuarter)**2)<R2.or. &
                (&
                (x_BLK(i,j,k,iBLK)-dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)-dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)+dz_BLK(iBLK)*cQuarter)**2)<R2.or. &
                (&
                (x_BLK(i,j,k,iBLK)-dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)+dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)-dz_BLK(iBLK)*cQuarter)**2)<R2.or. &
                (&
                (x_BLK(i,j,k,iBLK)-dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)+dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)+dz_BLK(iBLK)*cQuarter)**2)<R2
        end do
     end do
  end if

 if(neiLnorth(iBLK)==+1)then
     !Make all the ghostcells covered by a coarser cell become body cells,
     !if the coarser cell center is inside the body::
     j2=0
     do k2=2,nK,2
        do i2=2,nI,2
           IsBoundaryCell_GI(i2-1:i2,j2-1:j2,k2-1:k2,ExtraBc_)=(    &
                (x_BLK(i2,j2,k2,iBLK)-dx_BLK(iBLK)*cHalf)**2+       &
                (y_BLK(i2,j2,k2,iBLK)-dy_BLK(iBLK)*cHalf)**2+       &
                (z_BLK(i2,j2,k2,iBLK)-dz_BLK(iBLK)*cHalf)**2)<R2
        end do
     end do
  elseif(neiLnorth(iBLK)==-1)then
     !Make any ghostcell covered by finer cell become a body cell, if
     !any of the finer cell centers is inside the body::

     !Make all the ghostcell covered by finer cell to be body cells, if
     !any of the finer cell centers is inside the body
     j=0
     do k=1,nK
        do i=1,nI
           IsBoundaryCell_GI(i,j,k,ExtraBc_)=&
                (&
                (x_BLK(i,j,k,iBLK)-dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)+dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)-dz_BLK(iBLK)*cQuarter)**2)<R2.or. &
                (&
                (x_BLK(i,j,k,iBLK)-dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)+dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)+dz_BLK(iBLK)*cQuarter)**2)<R2.or. &
                (&
                (x_BLK(i,j,k,iBLK)+dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)+dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)-dz_BLK(iBLK)*cQuarter)**2)<R2.or. &
                (&
                (x_BLK(i,j,k,iBLK)+dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)+dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)+dz_BLK(iBLK)*cQuarter)**2)<R2
        end do
     end do
  end if
 if(neiLsouth(iBLK)==+1)then
     !Make all the ghostcells covered by a coarser cell become body cells,
     !if the coarser cell center is inside the body::
     j2=nJ+2
     do k2=2,nK,2
        do i2=2,nI,2
           IsBoundaryCell_GI(i2-1:i2,j2-1:j2,k2-1:k2,ExtraBc_)=(    &
                (x_BLK(i2,j2,k2,iBLK)-dx_BLK(iBLK)*cHalf)**2+       &
                (y_BLK(i2,j2,k2,iBLK)-dy_BLK(iBLK)*cHalf)**2+       &
                (z_BLK(i2,j2,k2,iBLK)-dz_BLK(iBLK)*cHalf)**2)<R2
        end do
     end do
  elseif(neiLsouth(iBLK)==-1)then
     !Make all the ghostcell covered by finer cell to be body cells, if
     !any of the finer cell centers is inside the body
     j=nJ+1
     do k=1,nK
        do i=1,nI
           IsBoundaryCell_GI(i,j,k,ExtraBc_)=&
                (&
                (x_BLK(i,j,k,iBLK)-dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)-dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)-dz_BLK(iBLK)*cQuarter)**2)<R2.or. &
                (&
                (x_BLK(i,j,k,iBLK)-dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)-dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)+dz_BLK(iBLK)*cQuarter)**2)<R2.or. &
                (&
                (x_BLK(i,j,k,iBLK)+dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)-dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)-dz_BLK(iBLK)*cQuarter)**2)<R2.or. &
                (&
                (x_BLK(i,j,k,iBLK)+dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)-dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)+dz_BLK(iBLK)*cQuarter)**2)<R2
        end do
     end do
  end if
  
 if(neiLbot(iBLK)==+1)then
     !Make all the ghostcells covered by a coarser cell become body cells,
     !if the coarser cell center is inside the body::
     k2=0
     do j2=2,nJ,2
        do i2=2,nI,2
           IsBoundaryCell_GI(i2-1:i2,j2-1:j2,k2-1:k2,ExtraBc_)=(    &
                (x_BLK(i2,j2,k2,iBLK)-dx_BLK(iBLK)*cHalf)**2+       &
                (y_BLK(i2,j2,k2,iBLK)-dy_BLK(iBLK)*cHalf)**2+       &
                (z_BLK(i2,j2,k2,iBLK)-dz_BLK(iBLK)*cHalf)**2)<R2
        end do
     end do
  elseif(neiLbot(iBLK)==-1)then
     !Make any ghostcell covered by finer cell become a body cell, if
     !any of the finer cell centers is inside the body::
     k=0
     do j=1,nJ
        do i=1,nI
           IsBoundaryCell_GI(i,j,k,ExtraBc_)=&
                (&
                (x_BLK(i,j,k,iBLK)-dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)-dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)+dz_BLK(iBLK)*cQuarter)**2)<R2.or. &
                (&
                (x_BLK(i,j,k,iBLK)-dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)+dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)+dz_BLK(iBLK)*cQuarter)**2)<R2.or. &
                (&
                (x_BLK(i,j,k,iBLK)+dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)-dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)+dz_BLK(iBLK)*cQuarter)**2)<R2.or. &
                (&
                (x_BLK(i,j,k,iBLK)+dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)+dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)+dz_BLK(iBLK)*cQuarter)**2)<R2
        end do
     end do
  end if
 if(neiLtop(iBLK)==+1)then
     !Make all the ghostcells covered by a coarser cell become body cells,
     !if the coarser cell center is inside the body::
     k2=nK+2
     do j2=2,nJ,2
        do i2=2,nI,2
           IsBoundaryCell_GI(i2-1:i2,j2-1:j2,k2-1:k2,ExtraBc_)=(    &
                (x_BLK(i2,j2,k2,iBLK)-dx_BLK(iBLK)*cHalf)**2+       &
                (y_BLK(i2,j2,k2,iBLK)-dy_BLK(iBLK)*cHalf)**2+       &
                (z_BLK(i2,j2,k2,iBLK)-dz_BLK(iBLK)*cHalf)**2)<R2
        end do
     end do
  elseif(neiLtop(iBLK)==-1)then
     !Make any ghostcell covered by finer cell become a body cell, if
     !any of the finer cell centers is inside the body::
     k=nK+1
     do j=1,nJ
        do i=1,nI
           IsBoundaryCell_GI(i,j,k,ExtraBc_)=&
                (&
                (x_BLK(i,j,k,iBLK)-dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)-dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)-dz_BLK(iBLK)*cQuarter)**2)<R2.or. &
                (&
                (x_BLK(i,j,k,iBLK)-dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)+dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)-dz_BLK(iBLK)*cQuarter)**2)<R2.or. &
                (&
                (x_BLK(i,j,k,iBLK)+dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)-dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)-dz_BLK(iBLK)*cQuarter)**2)<R2.or. &
                (&
                (x_BLK(i,j,k,iBLK)+dx_BLK(iBLK)*cQuarter)**2+        &
                (y_BLK(i,j,k,iBLK)+dy_BLK(iBLK)*cQuarter)**2+        &
                (z_BLK(i,j,k,iBLK)-dz_BLK(iBLK)*cQuarter)**2)<R2
        end do
     end do
  end if

end subroutine set_extra_boundary_cells
!========================================================================

!========================================================================
!  SUBROUTINE USER_SET_INNER_BCS
!========================================================================
!\
! This subroutine allows the user to apply boundary conditions to the inner
! body which are problem specific and cannot be created using the predefined
! options in BATSRUS.  The available options in BATSRUS have been designed
! to be self consistent and reasonably robust.  We generally recommend that
! you use on of those or a variant that is very close to one of them.  They
! can be considered reasonably safe.
!
! An example of a reasonable variant would be to use a modification of the
! "ionosphere" boundary where the density is fixed at the boundary to a 
! value that is a function of latitude.
!
! This routine is called for a single inner boundary face.  Since BATSRUS is
! is block cartesian, the values inside the boundary face must be passed back
! in cartesian coordinates.  Values that must be set are:
!
!  RhoFaceInside, pFaceInside, VxFaceInside, VyFaceInside, VzFaceInside
!  BxFaceInside, ByFaceInside, BzFaceInside
!
! Typically the boundary conditions are applied for the spherical coordinates
! and then transformed to the cartesian ones.
!
! As with all user subroutines, the variables declared in ModUser are 
! available here.  Again, as with other user subroutines DO NOT MODIFY 
! ANY GLOBAL VARIABLE DEFINED IN THE MODULES INCLUDED IN THIS SUBROUTINE 
! UNLESS SPECIFIED!!
!/
subroutine user_face_bcs(iFace,jFace,kFace,iBlock,iSide,iBoundary, &
     iter,time_now,FaceCoords_D,VarsTrueFace_V,VarsGhostFace_V,    &
     B0Face_D,UseIonosphereHere,UseRotatingBcHere)
  use ModSize,       ONLY: nDim,East_,West_,South_,North_,Bot_,    &
       Top_
  use ModMain,       ONLY: time_accurate,UseUserHeating,x_,y_,z_,  &
       UseInertial
  use ModVarIndexes, ONLY: & 
 !       EnergyRL_,&     !^CFG UNCOMMENT IF ALWAVES
        rho_,Ux_,Uy_,Uz_,Bx_,By_,Bz_,P_  

  use ModGeometry,   ONLY: R_BLK
  use ModAdvance,    ONLY: nFaceValueVars,State_VGB
  use ModPhysics,    ONLY: CosThetaTilt,SinThetaTilt,g,inv_g,      &
       inv_gm1,OmegaBody,unitUSER_B
  use ModNumConst,   ONLY: cZero,cHalf,cOne,cTwo,cTolerance,cTiny
  use ModUser
  implicit none
  !\
  ! Variables required by this user subroutine
  !/
  integer, intent(in):: iFace,jFace,kFace,iBlock,iSide,&
       iBoundary,iter
  real, intent(in):: time_now
  real, dimension(nDim), intent(in):: FaceCoords_D,    &
       B0Face_D
  real, dimension(nFaceValueVars), intent(in)::        &
       VarsTrueFace_V
  logical, intent(in):: UseIonosphereHere,             &
       UseRotatingBcHere
  real, dimension(nFaceValueVars), intent(out)::       &
       VarsGhostFace_V
  !\
  ! User declared local variables go here::
  !/
  integer:: iCell,jCell,kCell
  real:: XFace,YFace,ZFace,RFace
  real:: VxFaceOutside,VyFaceOutside,VzFaceOutside
  real:: BxFaceOutside,ByFaceOutside,BzFaceOutside
  real:: VrFaceOutside,VthetaFaceOutside,VphiFaceOutside, &
         VrFaceInside,VthetaFaceInside,VphiFaceInside,    &
         BrFaceOutside,BthetaFaceOutside,BphiFaceOutside, &
         BrFaceInside,BthetaFaceInside,BphiFaceInside
  real:: CosTheta,SinTheta,CosPhi,SinPhi
  real, dimension(1:3):: location,v_phi
  real:: XFaceT,YFaceT,ZFaceT,sin2Theta_coronal_hole
  real:: CosThetaT,SinThetaT,CosPhiT,SinPhiT
  real:: DensCell,PresCell,GammaCell
  real:: BdotR,UdotR,SignB0n
  real, dimension(3):: RFace_D,B1_D,U_D,Ut_D,Un_D
  !
  !---------------------------------------------------------------------------
  !\
  ! Calculation of boundary conditions should start here::
  !/
  !---------------------------------------------------------------------------
  !
  XFace = FaceCoords_D(1)
  YFace = FaceCoords_D(2)
  ZFace = FaceCoords_D(3)
  RFace = sqrt(XFace**2+YFace**2+ZFace**2)
  if (.not.UseUserHeating) then
     !\
     ! Apply some tricks to incorporate velocity
     ! shear at the boundary::
     !/
     RFace_D(x_)  = XFace/RFace
     RFace_D(y_)  = YFace/RFace
     RFace_D(z_)  = ZFace/RFace
     U_D (x_:z_)  = VarsTrueFace_V(Ux_:Uz_)
     UdotR        = dot_product(RFace_D,U_D)
     Un_D(x_:z_)  = UdotR*RFace_D(x_:z_)
     Ut_D         = U_D-Un_D
     B1_D(x_:z_)  = VarsTrueFace_V(Bx_:Bz_)
     BdotR        = dot_product(RFace_D,B0Face_D)
     SignB0n      = sign(cOne,BdotR)
     !\
     ! Update BCs for velocity and induction field::
     !/
     VarsGhostFace_V(Ux_:Uz_) = -U_D(x_:z_)
     VarsGhostFace_V(Bx_:Bz_) = B1_D(x_:z_)
     !\
     ! Update BCs for the mass density, EnergyRL,
     ! and pressure::
     !/
     iCell = iFace; jCell = jFace; kCell = kFace
     select case(iSide)
     case(East_)
        iCell  = iFace
     case(West_)
        iCell  = iFace-1
     case(South_)
        jCell  = jFace
     case(North_)
        jCell  = jFace-1
     case(Bot_)
        kCell  = kFace
     case(Top_)
        kCell  = kFace-1
     case default
        call stop_mpi('user_face_bcs')
     end select
     call get_plasma_parameters_cell(iCell,jCell,kCell,iBlock,&
          DensCell,PresCell,GammaCell)
     VarsGhostFace_V(rho_    ) = -VarsTrueFace_V(rho_     )+&
          cTwo*DensCell
     VarsGhostFace_V(P_      ) = -VarsTrueFace_V(P_       )+&
          cTwo*PresCell
   !  VarsGhostFace_V(EnergyRL_) = -VarsTrueFace_V(EnergyRL_)+&  !^CFG UNCOMMENT IF ALWAVES
   !       cTwo*PresCell*(cOne/(GammaCell-cOne)-inv_gm1)         !^CFG UNCOMMENT IF ALWAVES
  else
     !\
     ! Rotate to spherical coordinates
     !/
     CosTheta = ZFace/RFace
     SinTheta = sqrt(XFace**2+YFace**2)/RFace
     CosPhi   = XFace/sqrt(XFace**2+YFace**2+cTolerance**2)
     SinPhi   = YFace/sqrt(XFace**2+YFace**2+cTolerance**2)
     VxFaceOutside = VarsTrueFace_V(Ux_)
     VyFaceOutside = VarsTrueFace_V(Uy_)
     VzFaceOutside = VarsTrueFace_V(Uz_)
     BxFaceOutside = VarsTrueFace_V(Bx_)
     ByFaceOutside = VarsTrueFace_V(By_)
     BzFaceOutside = VarsTrueFace_V(Bz_)
     VrFaceOutside = (VxFaceOutside*XFace      +&
          VyFaceOutside*YFace                  +&
          VzFaceOutside*ZFace)/RFace
     VthetaFaceOutside = ((VxFaceOutside*XFace +&
          VyFaceOutside*YFace)*ZFace           -&
          VzFaceOutside*(XFace**2+YFace**2))   /&
          (sqrt(XFace**2+YFace**2+cTolerance**2)*RFace)
     VphiFaceOutside = (VyFaceOutside*XFace    -&
          VxFaceOutside*YFace)*SinTheta        /&
          ((XFace**2+YFace**2+cTolerance**2)/RFace)
     BrFaceOutside = (BxFaceOutside*XFace      +&
          ByFaceOutside*YFace                  +&
          BzFaceOutside*ZFace)/RFace
     BthetaFaceOutside = ((BxFaceOutside*XFace +&
          ByFaceOutside*YFace)*ZFace           -&
          BzFaceOutside*(XFace**2+YFace**2))   /&
          (sqrt(XFace**2+YFace**2+cTolerance**2)*RFace)
     BphiFaceOutside = (ByFaceOutside*XFace    -&
          BxFaceOutside*YFace)*SinTheta        /&
          ((XFace**2+YFace**2+cTolerance**2)/RFace)
     RFace = cOne
     call coronal_hole_boundary(RFace,sin2Theta_coronal_hole)
     XFaceT    =  CosThetaTilt*XFace+SinThetaTilt*ZFace
     YFaceT    = YFace
     ZFaceT    = -SinThetaTilt*XFace+CosThetaTilt*ZFace
     CosThetaT = ZFaceT/RFace
     SinThetaT = sqrt(XFaceT**2+YFaceT**2)/RFace
     CosPhiT   = XFaceT/sqrt(XFaceT**2+YFaceT**2+cTolerance**2)
     SinPhiT   = YFaceT/sqrt(XFaceT**2+YFaceT**2+cTolerance**2)
     if (SinThetaT*SinThetaT > sin2Theta_coronal_hole) then
        !\
        ! At the base of closed field regions::
        !/
        VrFaceInside     = -VrFaceOutside
        VthetaFaceInside = -VthetaFaceOutside
        VphiFaceInside   = -VphiFaceOutside
        BrFaceInside     =  BrFaceOutside
        BthetaFaceInside =  BthetaFaceOutside
        BphiFaceInside   =  BphiFaceOutside
        VarsGhostFace_V(rho_     ) = VarsTrueFace_V(rho_     )
        VarsGhostFace_V(P_       ) = VarsTrueFace_V(P_       )
       ! VarsGhostFace_V(EnergyRL_) = VarsTrueFace_V(EnergyRL_)!^CFG UNCOMMENT IF ALWAVES
     else
        !\
        ! At the base of open field regions::
        !/
        VrFaceInside     = cZero
        VthetaFaceInside = cZero
        VphiFaceInside   = cZero
        BrFaceInside     = cZero
        BthetaFaceInside = cZero 
        BphiFaceInside   = cZero
        VarsGhostFace_V(rho_     ) = cOne
        VarsGhostFace_V(P_       ) = inv_g
       ! VarsGhostFace_V(EnergyRL_) = VarsTrueFace_V(EnergyRL_)!^CFG UNCOMMENT IF ALWAVES
     endif
     !\
     ! Rotate back to cartesian coordinates::
     !/
     VarsGhostFace_V(Ux_) = VrFaceInside*XFace/RFace+&
          VthetaFaceInside*CosTheta*CosPhi          -&
          VphiFaceInside*SinPhi 
     VarsGhostFace_V(Uy_) = VrFaceInside*YFace/RFace+&
          VthetaFaceInside*CosTheta*SinPhi          +&
          VphiFaceInside*CosPhi
     VarsGhostFace_V(Uz_) = VrFaceInside*ZFace/RFace-&
          VthetaFaceInside*SinTheta
     VarsGhostFace_V(Bx_) = BrFaceInside*XFace/RFace+&
          BthetaFaceInside*CosTheta*CosPhi          -&
          BphiFaceInside*SinPhi
     VarsGhostFace_V(By_) = BrFaceInside*YFace/RFace+&
          BthetaFaceInside*CosTheta*SinPhi          +&
          BphiFaceInside*CosPhi
     VarsGhostFace_V(Bz_) = BrFaceInside*ZFace/RFace-&
          BthetaFaceInside*SinTheta
  endif
  !\
  ! Apply corotation:: Currently works only for the first body.
  !/
  if (UseInertial) then
     !\
     ! The program is called which calculates the cartesian 
     ! corotation velocity::
     !/
     VarsGhostFace_V(Ux_) = VarsGhostFace_V(Ux_) -&
          cTwo*OmegaBody*YFace
     VarsGhostFace_V(Uy_) = VarsGhostFace_V(Uy_) +&
          cTwo*OmegaBody*XFace
  end if
end subroutine user_face_bcs

subroutine user_calc_sources
  use ModVarIndexes, ONLY:&
        !EnergyRL_,& !^CFG UNCOMMENT IF ALWAVES
         rho_,rhoUx_,rhoUy_,rhoUz_,Bx_,By_,Bz_,P_
  use ModAdvance,    ONLY: Source_VC,Energy_,     &
       Flux_VX,Flux_VY,Flux_VZ
  use ModUser,       ONLY: Srho,SrhoUx,SrhoUy,SrhoUz,&
       SBx,SBy,SBz,SP,SEr,SE
  use ModNumConst,   ONLY: cZero
  implicit none
  
  Srho   = cZero
  SrhoUx = cZero
  SrhoUy = cZero
  SrhoUz = cZero
  SBx    = cZero
  SBy    = cZero
  SBz    = cZero
  SP     = cZero
  SEr    = cZero
  SE     = cZero
  call user_sources
  Source_VC(rho_       ,:,:,:) = Srho  +Source_VC(rho_,:,:,:)
  Source_VC(rhoUx_     ,:,:,:) = SrhoUx+Source_VC(rhoUx_,:,:,:)
  Source_VC(rhoUy_     ,:,:,:) = SrhoUy+Source_VC(rhoUy_,:,:,:)
  Source_VC(rhoUz_     ,:,:,:) = SrhoUz+Source_VC(rhoUz_,:,:,:)
  Source_VC(Bx_        ,:,:,:) = SBx   +Source_VC(Bx_,:,:,:)
  Source_VC(By_        ,:,:,:) = SBy   +Source_VC(By_,:,:,:)
  Source_VC(Bz_        ,:,:,:) = SBz   +Source_VC(Bz_,:,:,:)
  Source_VC(P_     ,:,:,:) = SP        +Source_VC(P_,:,:,:)
  Source_VC(Energy_,:,:,:) = SE    +Source_VC(Energy_,:,:,:)
 ! Source_VC(EnergyRL_  ,:,:,:) = SEr   +Source_VC(EnergyRL_  ,:,:,:) !^CFG UNCOMMENT IF ALWAVES
end subroutine user_calc_sources
!========================================================================
!  SUBROUTINE user_sources
!========================================================================
!\
! This subroutine is used to calculate sources for the MHD equations.  The
! routine is called for each block separately so that the user would typically
! need only to code the source term calculation for a single block (in other
! words inside the the k,j,i loop below).  As with all user subroutines, the
! variables declared in ModUser are available here.  Again, as with other
! user subroutines DO NOT MODIFY ANY GLOBAL VARIABLE DEFINED IN THE MODULES
! INCLUDED IN THIS SUBROUTINE UNLESS SPECIFIED!!
!
! The user should load the global variables:
!      Srho,SrhoUx,SrhoUy,SrhoUz,SBx,SBy,SBz,SE,SP,SEr
!
! Note that SE (energy) and SP (pressure) must both be loaded if the code is 
! going to use both the primitive and the conservative MHD equation advance  
! (see the USER MANUAL and the DESIGN document).  If using only primitive SP 
! must be loaded.  If using only conservative SE must be loaded.  The safe
! approach is to load both.
!/
subroutine user_sources
  use ModMain,       ONLY: nI,nJ,nK,globalBLK,PROCtest,BLKtest,  &
       UseRotatingFrame,UseUserHeating,UseInertial,gcn
  use ModVarIndexes, ONLY: &
       !EnergyRL_, &    !^CFG UNCOMMENT IF ALWAVES
       rho_,rhoUx_,rhoUy_,rhoUz_,Bx_,By_,Bz_,P_
  use ModAdvance,    ONLY: State_VGB,StateOld_VCB,Source_VC,      &
       B0xCell_BLK,B0yCell_BLK,B0zCell_BLK,UDotFA_X,UDotFA_Y,    &
       UDotFA_Z,Flux_VX,Flux_VY,Flux_VZ,Theat0,qheat_BLK
  use ModGeometry,   ONLY: x_BLK,y_BLK,z_BLK,R_BLK,VolumeInverse_I
  use ModConst,      ONLY: cZero,cHalf,cOne,cTwo,cTolerance
  use ModPhysics,    ONLY: g,OmegaBody,CosThetaTilt,SinThetaTilt,&
       Theat
  use ModProcMH,     ONLY: iProc 
  use ModUser,       ONLY: SrhoUx,SrhoUy,SrhoUz,SBx,SBy,SBz,SP,SE
  implicit none
  !\
  ! Variables required by this user subroutine::
  !/
  integer:: i,j,k
  logical:: oktest,oktest_me
  !
  !---------------------------------------------------------------------------
  !\
  ! Variable meanings:
  !   Srho: Source terms for the continuity equation
  !   SE,SP: Source terms for the energy (conservative) and presure
  !          (primative) equations
  !   SrhoUx,SrhoUy,SrhoUz:  Source terms for the momentum equation
  !   SBx,SBy,SBz:  Souce terms for the magnetic field equations
  !   SEr: Source terms for the relaxation energy equation
  !/
  !---------------------------------------------------------------------------
  !
  if (iProc==PROCtest.and.globalBLK==BLKtest) then
     call set_oktest('user_sources',oktest,oktest_me)
  else
     oktest=.false.; oktest_me=.false.
  end if
  do k=1,nK; do j=1,nJ; do i=1,nI
     !\
     ! User coding of the source terms should be inside this loop
     ! Note that user source terms should be added to the the
     ! values already loaded in the arrays. For example::
     ! Srho(i,j,k) = Srho(i,j,k) + user source term for rho
     !/
     if (UseUserHeating) call calc_OLD_heating
     if (.not.UseInertial) then
        !\
        ! Coriolis forces
        !/
        SrhoUx(i,j,k) = SrhoUx(i,j,k) +&
             cTwo*OmegaBody*State_VGB(rhoUy_,i,j,k,globalBLK)
        SrhoUy(i,j,k) = SrhoUy(i,j,k) -&
             cTwo*OmegaBody*State_VGB(rhoUx_,i,j,k,globalBLK)
     endif
  end do; end do; end do
  
Contains

    subroutine calc_OLD_heating
    implicit none
    real:: CosTheta,SinTheta,CosPhi,SinPhi,&
         sin2Theta_coronal_hole,XT,YT,ZT
    !\
    ! Compute Heliosphere source terms::
    !/
    XT =  CosThetaTilt*x_BLK(i,j,k,globalBLK)+&
         SinThetaTilt*z_BLK(i,j,k,globalBLK)
    YT =  y_BLK(i,j,k,globalBLK)
    ZT = -SinThetaTilt*x_BLK(i,j,k,globalBLK)+&
          CosThetaTilt*z_BLK(i,j,k,globalBLK)
    CosTheta = ZT/(R_BLK(i,j,k,globalBLK)+cTolerance)
    SinTheta = sqrt(XT**2+YT**2)             /&
         (R_BLK(i,j,k,globalBLK)+cTolerance)
    CosPhi = XT/sqrt(XT**2+YT**2+cTolerance**2)
    SinPhi = YT/sqrt(XT**2+YT**2+cTolerance**2)
    call coronal_hole_boundary(R_BLK(i,j,k,globalBLK),&
         sin2Theta_coronal_hole)
    if (SinTheta*SinTheta < sin2Theta_coronal_hole) then
       Theat0(i,j,k) = Theat
    else
       Theat0(i,j,k) = cOne
    end if
    SP(i,j,k)  = SP(i,j,k)+&
        (g-cOne)*State_VGB(rho_,i,j,k,globalBLK)     *&
         qheat_BLK(i,j,k,globalBLK)*(Theat0(i,j,k)-g *&
         State_VGB(P_,i,j,k,globalBLK)           /&
         State_VGB(rho_,i,j,k,globalBLK))
    SE(i,j,k)  = SE(i,j,k)+&
         State_VGB(rho_,i,j,k,globalBLK)             *&
         qheat_BLK(i,j,k,globalBLK)*(Theat0(i,j,k)-g *&
         State_VGB(P_,i,j,k,globalBLK)           /&
         State_VGB(rho_,i,j,k,globalBLK))
  end subroutine calc_OLD_heating
  
end subroutine user_sources

subroutine get_plasma_parameters_cell(iCell,jCell,kCell,iBlock,&
     DensCell,PresCell,GammaCell)
  !---------------------------------------------------------------------------
  !
  ! This module computes the cell values for density and pressure assuming
  ! a politropic equation of state with variable gamma = [2+n(T)]/n(T),
  ! where n(T)=n0+n1*T^2.
  ! This subroutine is written by ILR on May 29, 2003.
  ! Last updated is made by IVS and ILR on Nov 2, 2004.
  !---------------------------------------------------------------------------
  !
  use ModVarIndexes, ONLY: Bx_,By_,Bz_
  use ModGeometry,   ONLY: x_BLK,y_BLK,z_BLK
  use ModNumConst,   ONLY: cHalf,cOne,cTwo,cThree,cFour,cTiny
  use ModAdvance,    ONLY: B0xCell_BLK,B0yCell_BLK,B0zCell_BLK,  &
       State_VGB
  use ModUser,       ONLY: DegFrm1,DegF_Ratio,Dens_Ratio,MaxB0_1,&
       MaxB0_2,Bnot
  use ModPhysics,    ONLY: g,inv_g,unitUSER_B
  implicit none
  !---------------------------------------------------------------------------
  integer, intent(in):: iCell,jCell,kCell,iBlock
  real, intent(out):: DensCell,PresCell,GammaCell
  !---------------------------------------------------------------------------
  real, parameter:: n0=cFour
  real:: AAc,BBc,Fn1,Fg1
  real:: BrCell,B2Cell
  real:: XCell,YCell,ZCell,RCell
  real:: Temp_Ratio,TempCell,DegFrmCell
  real:: DegF_Modulation,Dens_Modulation,Temp_Modulation
  !---------------------------------------------------------------------------
  !\
  ! Get cell coordinates and radial distance from the Sun::
  !/
  XCell = x_BLK(iCell,jCell,kCell,iBlock)
  YCell = y_BLK(iCell,jCell,kCell,iBlock)
  ZCell = z_BLK(iCell,jCell,kCell,iBlock)
  RCell = sqrt(XCell**2+YCell**2+ZCell**2)
  if (RCell>0.5) then
     !\
     ! Get the absolute value of the radial component of the magnetic filed::
     !/
     BrCell = abs(&
          (XCell*B0xCell_BLK(iCell,jCell,kCell,iBlock) +&
           YCell*B0yCell_BLK(iCell,jCell,kCell,iBlock) +&
           ZCell*B0zCell_BLK(iCell,jCell,kCell,iBlock))/&
           RCell)
     B2Cell = sqrt(&
           B0xCell_BLK(iCell,jCell,kCell,iBlock)**2+&
           B0yCell_BLK(iCell,jCell,kCell,iBlock)**2+&
           B0zCell_BLK(iCell,jCell,kCell,iBlock)**2)
     !\
     ! Modulate the degrees of freedom so that in the CS (slow wind) the
     ! number of degrees of freedom is LARGER (~27) than in the open field
     ! regions (~13; fast wind).  This will result in a more isothermal
     ! solution in the helmet streamer belt than in the open field regions.
     ! Therefore, the kinetic gas pressure at the Sun in open field regions
     ! is somewhat GREATER than that in the closed field regions.  However,
     ! by imposing the density and temperature modulation described below,
     ! we increase the kinetic gas pressure in closed field regions.  All
     ! this is done to achieve a good agreement with observations at 1AU....
     !/
     DegF_Modulation = DegF_Ratio/&
                      (cOne+min(cOne,BrCell*RCell**3/MaxB0_1)*&
                      (DegF_Ratio-cOne))
     !\
     ! Modulate the temperature and density in order to have higher
     ! temperature (~2.5) and lower density (~1/5) in open field regions
     ! [abs(Br)<6G] compared to closed ones [abs(Br)~0].  This is done
     ! to reproduce the observed bi-modal structure of the solar wind.
     !/
     Temp_Ratio = Dens_Ratio/cTwo
     Temp_Modulation = cOne+min(cOne,BrCell*RCell**3/MaxB0_1)*&
                      (Temp_Ratio-cOne)
     Dens_Modulation = cOne/&
                      (cOne+min(cOne,BrCell*RCell**3/MaxB0_1)*&
                      (Dens_Ratio-cOne))
     !\
     ! Increase artificially the mass density in regions where
     ! B2Cell>MaxB0_2 (~25G), so that to maintain a reasonable value
     ! of the Alfven speed for B2Cell>MaxB0_2.
     !/
     if (B2Cell>MaxB0_2) then
        Dens_Modulation = min(cTwo+cHalf,&
             (B2Cell/MaxB0_2)**2/Dens_Ratio)
        Temp_Modulation = cOne/Dens_Modulation
     end if
  else
     DegF_Modulation = cOne
     Temp_Modulation = cOne
     Dens_Modulation = cOne
  end if
  !\
  ! Obtain the temperature at the given cell::
  !/
  Fg1 = (cOne+cHalf*n0+cHalf*cHalf*DegFrm1*DegF_Modulation)/&
        (cOne+cHalf*n0+cHalf*cHalf*DegFrm1)
  Fn1 = DegFrm1*DegF_Modulation
  AAc = cFour*(cOne+cHalf*n0)/Fn1
  BBc = cFour*(cOne+cHalf*n0+cHalf*cHalf*Fn1)/Fn1/RCell
  TempCell = cTwo*BBc/(sqrt(AAc**2+cFour*BBc)+AAc) !=1 as long as RCell=1
  !\
  ! Here we assume a quadratic dependence of the degrees of freedom
  ! on the temperature.  Assign the value of GammaCell at RCell::
  !/
  DegFrmCell = n0+Fn1*TempCell**2
  GammaCell  = (DegFrmCell+cTwo)/DegFrmCell 
  !\
  ! Obtain the mass density and pressure in the given cell::
  !/
  PresCell  = inv_g*Dens_Modulation*Temp_Modulation/Fg1*&
       (TempCell**(cOne+cHalf*n0))*exp(-cHalf*Fn1*(cOne-TempCell))
  DensCell  = g*PresCell*Fg1/TempCell/Temp_Modulation
end subroutine get_plasma_parameters_cell

!========================================================================
!  SUBROUTINE user_initial_perturbation
!========================================================================
!\
! This subroutine allows the user to add a perturbation to a solutions
! read in from a restart file.  The idea is to allow the user to "start"
! some process on top of the already converged solution. The routine loops
! over all blocks, but only needs to load the perturbation where appropriate.
! As with all user subroutines, the variables declared in ModUser are 
! available here.  Again, as with other user subroutines DO NOT MODIFY ANY 
! GLOBAL VARIABLE DEFINED IN THE MODULES INCLUDED IN THIS SUBROUTINE UNLESS 
! SPECIFIED!!
!
! The user should load the global variables:
!   rho_BLK,rhoUx_BLk,rhoUy_BLK,rhoUz_BLK,p_BLK,E_BLK
!   Bx_BLK, By_BLK, Bz_BLK
!
! Note that in most cases E (energy) and P (pressure) should both be loaded.
!/
subroutine user_initial_perturbation
  use ModMain,      ONLY: nI,nJ,nK,nBLK,globalBLK,PROCtest,        &
       BLKtest,unusedBLK,UseUserHeating,UseUserB0,gcn,x_,y_,z_
  use ModIO,        ONLY: restart
  use ModVarIndexes,ONLY:&
        !EnergyRL_ ,&  !^CFG UNCOMMENT IF ALWAVES
        rho_,rhoUx_,rhoUy_,rhoUz_,Bx_,By_,Bz_,P_
  use ModAdvance,   ONLY: State_VGB,B0xCell_BLK,B0yCell_BLK,       &
       B0zCell_BLK,tmp1_BLK,tmp2_BLK
  use ModProcMH,    ONLY: iProc,nProc,iComm
  use ModNumConst,  ONLY: cZero,cQuarter,cHalf,cOne,cTwo,cE1,cE9,  &
       cTolerance,cThree
  use ModConst,     ONLY: Rsun,Msun,cGravitation
  use ModGeometry,  ONLY: x_BLK,y_BLK,z_BLK,R_BLK,cV_BLK,x2,y2,z2
  use ModPhysics,   ONLY: Gbody,g,inv_g,gm1,inv_gm1,ModulationP,   &
       ModulationRho,UseFluxRope,rot_period_dim,OmegaBody,Rbody,   &
       unitSI_U,unitSI_rho,unitSI_x,unitUSER_energydens,           &
       unitUSER_t,unitUSER_B,Body_rho_dim
  use ModUser
  implicit none
  !\
  ! Variables required by this user subroutine::
  !/
  integer:: i,j,k,iBLK,iError
  logical:: oktest,oktest_me
  real:: volume
  real:: xx,yy,zz,RR,ROne,Rmax
  real:: rho_GL98,p_GL98
  real:: Bx_GL98,By_GL98,Bz_GL98
  real:: Dens_BLK,Pres_BLK,Gamma_BLK
  real, external:: maxval_BLK
  real, dimension(3):: R_GL98_D,B_GL98_D
  !
  !---------------------------------------------------------------------------
  !\
  ! Variable meanings:
  !
  !
  !/
  !---------------------------------------------------------------------------
  !
  if (iProc==PROCtest.and.globalBLK==BLKtest) then
     call set_oktest('user_initial_perturbation',oktest,oktest_me)
  else
     oktest=.false.; oktest_me=.false.
  end if
  !\
  ! Initialize some auxilary variables::
  !/
  Rbody  = cOne
  Mrope_GL98 = cZero
  !\
  ! Set the value of MaxB0_1 and MaxB0_2::
  !/
  if (UseUserB0) then
     MaxB0_1 = Bnot/unitUSER_B
     MaxB0_2 = 2.00E+01/unitUSER_B
  else
     !\
     ! Find the maximum value of B0 at time zero::
     !/
     do iBLK=1,nBLK
        if (unusedBLK(iBLK)) CYCLE
        do k=1,nK; do j=1,nJ; do i=1,nI
           if (R_BLK(i,j,k,iBLK) >= cOne) then
              tmp1_BLK(i,j,k,iBLK) = sqrt(    &
                   B0xCell_BLK(i,j,k,iBLK)**2+&
                   B0yCell_BLK(i,j,k,iBLK)**2+&
                   B0zCell_BLK(i,j,k,iBLK)**2)
           else
              tmp1_BLK(i,j,k,iBLK) = cZero
           endif
        enddo; enddo; enddo
     end do
     MaxB0_1 = maxval_BLK(nProc,tmp1_BLK)
     MaxB0_2 = MaxB0_1
  endif
  InvH0 = cGravitation*Msun/Rsun/unitSI_U**2
  do iBLK=1,nBLK
     if (unusedBLK(iBLK)) CYCLE   
     do k=1,nK;do j=1,nJ; do i=1,nI
        if ((.not.UseUserHeating).and.(.not.restart)) then
           xx = x_BLK(i,j,k,iBLK)
           yy = y_BLK(i,j,k,iBLK)
           zz = z_BLK(i,j,k,iBLK)
           RR = sqrt(xx**2+yy**2+zz**2+cTolerance**2)
           ROne  = max(cOne,RR)
           Rmax  = max(2.1E+01,sqrt(x2**2+y2**2+z2**2))
           State_VGB(Bx_      ,i,j,k,iBLK) = cZero
           State_VGB(By_      ,i,j,k,iBLK) = cZero
           State_VGB(Bz_      ,i,j,k,iBLK) = cZero
           call get_plasma_parameters_cell(i,j,k,iBLK,&
                Dens_BLK,Pres_BLK,Gamma_BLK)
           State_VGB(rho_     ,i,j,k,iBLK) = Dens_BLK
           State_VGB(P_       ,i,j,k,iBLK) = Pres_BLK
           State_VGB(rhoUx_   ,i,j,k,iBLK) = Dens_BLK*&
                4.0E+01*((ROne-cOne)/(Rmax-cOne))*xx/RR
           State_VGB(rhoUy_   ,i,j,k,iBLK) = Dens_BLK*&
                4.0E+01*((ROne-cOne)/(Rmax-cOne))*yy/RR
           State_VGB(rhoUz_   ,i,j,k,iBLK) = Dens_BLK*&
                4.0E+01*((ROne-cOne)/(Rmax-cOne))*zz/RR
          ! State_VGB(EnergyRL_,i,j,k,iBLK) = Pres_BLK   *& !^CFG UNCOMMENT IF ALWAVES
          !      (cOne/(Gamma_BLK-cOne)-inv_gm1)            !^CFG UNCOMMENT IF ALWAVES
        endif
        if (UseFluxRope) then
           R_GL98_D(x_) = x_BLK(i,j,k,iBLK)
           R_GL98_D(y_) = y_BLK(i,j,k,iBLK)
           R_GL98_D(z_) = z_BLK(i,j,k,iBLK)
           call add_GL98_fluxrope(R_GL98_D,rho_GL98,p_GL98,&
                B_GL98_D)
           State_VGB(Bx_,i,j,k,iBLK)          = &
                State_VGB(Bx_,i,j,k,iBLK)+B_GL98_D(x_)
           State_VGB(By_,i,j,k,iBLK)          = &
                State_VGB(By_,i,j,k,iBLK)+B_GL98_D(y_)
           State_VGB(Bz_,i,j,k,iBLK)          = &
                State_VGB(Bz_,i,j,k,iBLK)+B_GL98_D(z_)
           !\
           ! Add just `ModulationRho' times of the CME mass
           ! to the mass density::
           !/
           if ((State_VGB(rho_,i,j,k,iBLK)+     &
                ModulationRho*rho_GL98) < cOne*&
                State_VGB(rho_,i,j,k,iBLK)) then
              State_VGB(rho_,i,j,k,iBLK)       = &
                   cOne*State_VGB(rho_,i,j,k,iBLK)
           else
              State_VGB(rho_,i,j,k,iBLK)       = &
                   State_VGB(rho_,i,j,k,iBLK)  + &
                   ModulationRho*rho_GL98
           endif
           !\
           ! Add just `ModulationP' times of the CME pressure
           ! to the kinetic pressure::
           !/
           if ((State_VGB(P_,i,j,k,iBLK)+&
                ModulationP*p_GL98) < cOne* &
                State_VGB(P_,i,j,k,iBLK)) then
              State_VGB(P_,i,j,k,iBLK)     = &
                   cOne*State_VGB(P_,i,j,k,iBLK)
           else 
              State_VGB(P_,i,j,k,iBLK)     = &
                   State_VGB(P_,i,j,k,iBLK)+ &
                   ModulationP*p_GL98
           endif
           !\
           ! Calculate the mass added to the flux rope::
           !/
           Mrope_GL98 = Mrope_GL98+ModulationRho*&
                rho_GL98*cV_BLK(iBLK)
        endif
     end do; end do; end do
     !\
     ! Update the total energy::
     !/
     globalBLK=iBLK
     call correctE
  end do
  !\
  ! Write out some statistics::
  !/
  call post_init_stat
  
end subroutine user_initial_perturbation

subroutine user_set_physics_constants
  use ModPhysics, ONLY: FaceState_VI,CellState_VI
end subroutine user_set_physics_constants

subroutine add_GL98_fluxrope(R_GL98_D,rho_GL98,p_GL98,B_GL98_D)
  !
  !---------------------------------------------------------------------------
  ! PARAMETER LIST: cme_a, cme_r1, cme_r0, cme_a1, cme_rho1, cme_rho2, B1_dim,
  !                  RHOsun,Vscl
  ! Definition of Parameters used for the initial state
  !   cme_a    = contraction distance as in   r --> r -a
  !   cme_r1   = distance of flux rope from sun center = 1.2
  !   cme_r0   = radius of flux rope
  !   cme_a1   = constant for setting pressure in flux rope
  !   Rscl     = 1.0  scaled radius of the sun
  !   RHOscl   = 1.0  scaled density of RHOsun
  !   SSPscl   = 1.0  scaled soundspeed of the sun
  !   rho1scl  = uniform backround density of the solution before contraction
  !   rho2scl  = background powerlaw density added to after contraction
  !   B1scl    = magnetic field strength parameter of the flux rope
  !   Gsun     = gravitational acceleration at sun's surface = 2.734e4 cm/s**2
  !   Vscl     = V/SSPsun     
  !\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\//////////////////////////////////
  !=====================================================================
  !\
  ! Calculates magnetic field, pressure and density for a coronal flux rope 
  ! capable of self-similar expansion and erupting in a CME.
  ! The analytical solution is taken from Gibson and Low 
  ! Astrophysical Journal, Vol 493, p. 460.
  !
  ! Written by Chip Manchester Jan 18 2001
  ! Rewritten by Chip Nov 29 2001 for flux rope injection
  !/
  !   Bug fixes
  !   March 18       dpres_1dr1 = cme_a1*(dA2dr*dr2dr1 + dA2dth*dth2dr1)
  !   change to..... dpres_1dr1 = a1scl*(dA2dr*dr2dr1 + dA2dth*dth2dr1)
  !   without above fix, same as used for runs 12, 13, 14
  !   Feb   07, 2002 Br1 is changed to Br1**2 in density calc thanks to Ilia
  !   Feb   12, 2002 expression for ga0r0 is fixed     
  !   Feb   22, 2002 derivative of B2*2/dr1 is fixed
  !   Feb   22, 2002 angles in 2nd coordinate system are redefined
  !
  !---------------------------------------------------------------------------
  !
  use ModMain,           ONLY: x_,y_,z_
  use ModNumConst,       ONLY: cZero,cOne,cTwo,cFour,cPi,&
       cDegToRad
  use ModProcMH,         ONLY: iProc
  use ModCoordTransform, ONLY: rot_matrix_x,rot_matrix_y,&
       rot_matrix_z
  use ModPhysics,        ONLY: Gbody,g,inv_g,gm1,inv_gm1,&
       cme_a,cme_r1,cme_r0,cme_a1,a1scl,rho1scl,rho2scl, &
       SSPscl,cme_B1_dim,cme_alpha,ModulationRho,        &
       ModulationP,OrientationGL98,LatitudeGL98,LongitudeGL98
  use ModUser,           ONLY: DoFirst_GL
  use ModIO,             ONLY: iUnitOut, write_prefix
  implicit none
  !  
  real, dimension(3), intent(in):: R_GL98_D
  real, dimension(3), intent(out):: B_GL98_D
  real, intent(out):: rho_GL98,p_GL98
  !\
  ! User declared local variables go here::
  !/
  real:: x,y,z,  x_1,y_1,z_1,  x_2,y_2,z_2
  real:: r,cos_theta,sin_theta,cos_phi,sin_phi
  real:: r_1,cos_theta1,sin_theta1,cos_phi1,sin_phi1,lambda
  real:: r_2,cos_theta2,sin_theta2,cos_phi2,sin_phi2
  real:: dr2dr1,dth2dr1,cos_thmax,sin_thmax,dsin_thmaxdr
  real:: Br,Btheta,Bphi
  real:: Br1,Btheta1,Bphi1 
  real:: Br2,Btheta2,Bphi2
  real:: Bx_1,By_1,Bz_1
  real:: Bx_2,By_2,Bz_2
  real:: Br_r0,Btheta_r0
  real:: dBr1dr,dBtheta1dr,dBphi1dr
  real:: dBr_r0dr,dBtheta_r0dr 
  real:: dBr2dr1,dBtheta2dr1,dBphi2dr1
  real:: dBr2dr2,dBth2dr2,dBphi2dr2
  real:: dBr2dth2,dBth2dth2,dBphi2dth2
  real:: A2,dA2dr,dA2dth, d2A2dr2,d2A2drdth,d2A2dth2
  real:: pres_1,dpres_1dr1,F_grav,alpha0,ga0r0,delta 
  real, dimension(3):: R1_GL98_D,B1_GL98_D
  real, dimension(3,3):: RotateGL98_DD
  !------------------------------------------------------------------------
  if (DoFirst_GL) then

     DoFirst_GL=.false.
     !\
     ! Construct the rotational matrix RotateGL98_DD,
     !/
     RotateGL98_DD  = matmul( &
          rot_matrix_z(OrientationGL98*cDegToRad),&
          rot_matrix_y((LatitudeGL98-90)*cDegToRad))
     RotateGL98_DD = matmul(RotateGL98_DD, &
          rot_matrix_z(-LongitudeGL98*cDegToRad))

     if(iProc==0)then
        call write_prefix; write(iUnitOut,*) ''
        call write_prefix; write(iUnitOut,*) &
             '>>>>>>>>>>>>>>>>>>>                  <<<<<<<<<<<<<<<<<<<<<'
        call write_prefix; write(iUnitOut,*) &
             '            Initial Perturbation Is Initiated!!!'
        call write_prefix; write(iUnitOut,*) &
             '>>>>>>>>>>>>>>>>>>>                  <<<<<<<<<<<<<<<<<<<<<'
        call write_prefix; write(iUnitOut,*) ''
        call write_prefix; write(iUnitOut,*) 'B1_dim = ',cme_B1_dim
        call write_prefix; write(iUnitOut,*) 'cme_a  = ',cme_a
        call write_prefix; write(iUnitOut,*) 'cme_r1 = ',cme_r1
        call write_prefix; write(iUnitOut,*) 'cme_r0 = ',cme_r0
        call write_prefix; write(iUnitOut,*) 'cme_a1 = ',cme_a1
        call write_prefix; write(iUnitOut,*) 'ModulationRho = ',ModulationRho
        call write_prefix; write(iUnitOut,*) 'ModulationP   = ',ModulationP
     end if
  end if
  !
  delta = cOne/(cTwo*(cOne+cFour))
  !\
  ! Compute R1_GL98_D::
  !/
  R1_GL98_D = matmul(RotateGL98_DD, R_GL98_D)
  !\
  ! CALCULATE CELL CENTER FOR GLOBAL CARTESIAN COORDINATES 
  !/
  x = R1_GL98_D(x_)
  y = R1_GL98_D(y_)
  z = R1_GL98_D(z_)
  !\
  ! CALCULATE CELL CENTER FOR GLOBAL SPHERICAL COORDINATES 
  !/
  r = sqrt(x**2 + y**2 + z**2)
  cos_theta = z/r
  sin_theta = sqrt(x**2 + y**2)/r
  cos_phi   = x/sqrt(x**2 + y**2)
  sin_phi   = y/sqrt(x**2 + y**2)
  if (r <= delta) then 
     r = delta
     x = delta*sin_theta*cos_phi
     y = delta*sin_theta*sin_phi
     z = delta*cos_theta
  end if
  !\
  ! CALCULATE CELL CENTER FOR TRANSFORMED SPHERICAL COORDINATES 
  ! stretching transformation of variables r --> r - a
  !/
  lambda = r + cme_a
  r_1    = lambda
  cos_theta1 = cos_theta
  sin_theta1 = sin_theta 
  cos_phi1   = cos_phi
  sin_phi1   = sin_phi
  !\
  ! CALCULATE CELL CENTER FOR TRANSFORMED CARTESIAN COORDINATES 
  !/
  x_1 = lambda*sin_theta1*cos_phi1
  y_1 = lambda*sin_theta1*sin_phi1
  z_1 = lambda*cos_theta1
  !---------------------------FLUX ROPE REGION---------------------
  ! CALCULATE CELL CENTER CARTESIAN COORDINATES for CME FLUX ROPE
  ! stretching transformation r = r --> r - a
  !----------------------------------------------------------------
  x_2 = x_1
  y_2 = y_1 
  z_2 = z_1 - cme_r1 
  !\
  ! CALCULATE CELL CENTER SPHERICAL COORDINATES for CME FLUX ROPE
  !/
  r_2 = sqrt(x_2**2 + y_2**2 + z_2**2)
  cos_theta2 = x_2/r_2
  sin_theta2 = sqrt(z_2**2 + y_2**2)/r_2
  cos_phi2   = y_2/sqrt(z_2**2 + y_2**2)
  sin_phi2   = z_2/sqrt(z_2**2 + y_2**2)
  if (r_2 <= delta) then
     r_2 = delta
     y_2 = delta*sin_theta2*cos_phi2
     z_2 = delta*sin_theta2*sin_phi2
     x_2 = delta*cos_theta2
  end if
  alpha0 = 5.763854/cme_r0
  ga0r0 = sin(alpha0*cme_r0)/(alpha0*cme_r0) - cos(alpha0*cme_r0)
  A2 = (cFour*cPi*a1scl/alpha0**2)*((cme_r0**2/ga0r0)             * &
       (sin(alpha0*r_2)/(alpha0*r_2) - cos(alpha0*r_2)) - r_2**2) * &
       sin_theta2**2
  dA2dr = ((cFour*cPi*a1scl/alpha0**2)*((cme_r0**2/ga0r0)         * & 
       (cos(alpha0*r_2)/r_2 - sin(alpha0*r_2)/(alpha0*r_2**2)     + & 
       alpha0*sin(alpha0*r_2)) - cTwo*r_2))*sin_theta2**2
  dA2dth = (8.0*cPi*a1scl/alpha0**2)*((cme_r0**2/ga0r0)           * & 
       (sin(alpha0*r_2)/(alpha0*r_2) - cos(alpha0*r_2)) - r_2**2) * &
       sin_theta2*cos_theta2 
  d2A2dr2 = (cFour*cPi*a1scl/alpha0**2)*sin_theta2**2             * &
       ( (cme_r0**2/ga0r0)*(cTwo*sin(alpha0*r_2)/(alpha0*r_2**3)  - &
       cTwo*cos(alpha0*r_2)/(r_2**2) - alpha0*sin(alpha0*r_2)/r_2 + &
       (alpha0**2)*cos(alpha0*r_2)) - cTwo)  
  d2A2drdth = (8.0*cPi*a1scl/alpha0**2)*sin_theta2*cos_theta2     * &
       ((cme_r0**2/ga0r0)*(cos(alpha0*r_2)/r_2                    - & 
       sin(alpha0*r_2)/(alpha0*r_2**2)                            + &
       alpha0*sin(alpha0*r_2)) - cTwo*r_2) 
  d2A2dth2 = (8.0*cPi*a1scl/alpha0**2)*((cme_r0**2/ga0r0)         * &
       (sin(alpha0*r_2)/(alpha0*r_2) - cos(alpha0*r_2)) - r_2**2) * &
       (cos_theta2**2 - sin_theta2**2)
  dr2dr1  =  (x_2/r_2)*sin_theta1*cos_phi1                        + &
       (y_2/r_2)*sin_theta1*sin_phi1                              + &
       (z_2/r_2)*cos_theta1
  dth2dr1 =  (cOne/r_2)*(-sin_theta2*sin_theta1*cos_phi1          + &
       cos_theta2*cos_phi2*sin_theta1*sin_phi1                    + &
       cos_theta2*sin_phi2*cos_theta1)
  !\
  ! Derivatives of field components in flux rope spherical coordinates
  !/
  dBr2dr2 = -cTwo*dA2dth/(sin_theta2*r_2**3)           + &
       d2A2drdth/(sin_theta2*r_2**2)
  dBr2dth2 = -cos_theta2*dA2dth/(r_2**2*sin_theta2**2) + & 
       d2A2dth2/(sin_theta2*r_2**2)
  dBth2dr2 = dA2dr/(sin_theta2*r_2**2)                 - &
       d2A2dr2/(r_2 *sin_theta2)
  dBth2dth2 = cos_theta2*dA2dr/(r_2*sin_theta2**2)     - &
       d2A2drdth/(r_2*sin_theta2)
  dBphi2dr2 = alpha0*dA2dr/(r_2*sin_theta2)            - &
       alpha0*A2 /(sin_theta2*r_2**2)
  dBphi2dth2 = alpha0*dA2dth/(r_2*sin_theta2)          - & 
       alpha0*cos_theta2*A2/(r_2*sin_theta2**2)
  !\
  ! Total derivative of the flux rope field components in terms of `r1'
  !/
  dBr2dr1     = dBr2dr2  *dr2dr1 + dBr2dth2  *dth2dr1
  dBtheta2dr1 = dBth2dr2 *dr2dr1 + dBth2dth2 *dth2dr1
  dBphi2dr1   = dBphi2dr2*dr2dr1 + dBphi2dth2*dth2dr1
  !\
  ! Magnetic field components in the flux rope spherical coordinates
  !/
  Br2     = dA2dth/(sin_theta2*r_2**2)
  Btheta2 = -dA2dr/(sin_theta2*r_2)
  Bphi2   = alpha0*A2/(sin_theta2*r_2)
  !\
  ! Magnetic field components in the second cartesian coordinates
  ! X-COMPONENT OF MAGNETIC FIELD
  !/
  Bx_2 = Br2*cos_theta2            - &
       Btheta2*sin_theta2
  ! Y-COMPONENT OF MAGNETIC FIELD
  By_2 = Br2*sin_theta2*cos_phi2   + &
       Btheta2*cos_theta2*cos_phi2 - &
       Bphi2*sin_phi2
  ! Z-COMPONENT OF MAGNETIC FIELD
  Bz_2 = Br2*sin_theta2*sin_phi2   + &
       Btheta2*cos_theta2*sin_phi2 + &
       Bphi2*cos_phi2
  !\
  ! Define the magnetic field in the global cartesian coordinates
  ! INSIDE THE MAGNETIC FLUX ROPE REGION
  !/
  if (sqrt(x_2**2 + y_2**2 + z_2**2) <= cme_r0) then
     Bx_1 = Bx_2 
     By_1 = By_2 
     Bz_1 = Bz_2 
     !\
     ! Magnetic field components in global sperical coordinates
     !/
     Br1     = Bx_1*sin_theta1*cos_phi1 + &
          By_1*sin_theta1*sin_phi1      + &
          Bz_1*cos_theta1
     Btheta1 = Bx_1*cos_theta1*cos_phi1 + &
          By_1*cos_theta1*sin_phi1      - &
          Bz_1*sin_theta1
     Bphi1   = Bx_1*(-cOne)*sin_phi1    + &
          By_1*cos_phi1
     !\
     ! Compute kinetic gas pressure
     !/
     pres_1     = inv_g*rho1scl*SSPscl**2 + a1scl*A2
     dpres_1dr1 = a1scl*(dA2dr*dr2dr1 + dA2dth*dth2dr1)
     !\
     ! MAGNETIC FIELD transformed with stretching transformation
     !/
     Br     = Br1    *(lambda/r)**2
     Btheta = Btheta1*(lambda/r)
     Bphi   = Bphi1  *(lambda/r)
     !\
     ! Magnetic field components in global cartesian coordinates
     !/
     B1_GL98_D(x_) = Br*sin_theta1*cos_phi1   + &
          Btheta*cos_theta1*cos_phi1    - &
          Bphi*sin_phi1
     !\
     ! Y-COMPONENT OF MAGNETIC FIELD:: (x,y,z)_BATSRUS -> (x,y,z)
     !/
     B1_GL98_D(y_) = Br*sin_theta1*sin_phi1   + &
          Btheta*cos_theta1*sin_phi1    + &
          Bphi*cos_phi1
     !\
     ! Z-COMPONENT OF MAGNETIC FIELD:: (x,y,z)_BATSRUS -> (x,y,z)
     !/
     B1_GL98_D(z_) = Br*cos_theta1-Btheta*sin_theta1
     !\
     ! Transform back to the original coordinates
     ! given by R_GL98_D:: 
     !/
     B_GL98_D  = matmul(B1_GL98_D, RotateGL98_DD)
     !\
     ! PLASMA DENSITY with stretching transformation
     !/
     F_grav   = (abs(Gbody)/(r**2) + cme_alpha*r)
     rho_GL98 = (cOne/ F_grav)*(((lambda/r)**2)                   * &
          ((lambda/r)**2 - cOne)*(dpres_1dr1 + (cOne/(cFour*cPi)) * &
          (Br2*dBr2dr1 + Btheta2*dBtheta2dr1 + Bphi2*dBphi2dr1))  + &
          cTwo*lambda*cme_a*pres_1/r**3 + cme_a*lambda            / &
          (cFour*cPi*r**3)*(cOne - cTwo*(lambda/r)**2)*Br1**2     + &
          ((lambda/r)**2)*((cme_a/r)**2 +cTwo*cme_a/r)*(Btheta1**2+ &
          Bphi1**2)/(cFour*cPi*lambda))
     !\
     ! Add background density
     !/
     rho_GL98 = rho_GL98 + rho2scl/r**3
     !\
     ! PLASMA PRESSURE with contraction transformation
     !/
     p_GL98 = pres_1*(lambda/r)**2                               - &
          (cOne/(cTwo*cFour*cPi))*((lambda/r)**2)*((lambda/r)**2 - &
          cOne)*Br1**2 
     !\
     ! Add background pressure
     !/
     p_GL98 = p_GL98 + abs(Gbody)*rho2scl/(cFour*r**4)
  else
     B_GL98_D = cZero; rho_GL98 = cZero; p_GL98 = cZero
  endif
  
end subroutine add_GL98_fluxrope

subroutine post_init_stat
  use ModAdvance,    ONLY: State_VGB,rho_,P_
  use ModProcMH,     ONLY: iProc
  use ModUser,       ONLY: Mrope_GL98,MaxB0_1,MaxB0_2,Tnot,Bnot
  use ModPhysics,    ONLY: Gbody
  use ModIO,         ONLY: iUnitOut, write_prefix
  implicit none
  real, external :: maxval_blk, minval_blk
  real :: pMin, pMax, RhoMin, RhoMax
  !---------------------------------------------------------------------
  !\
  ! Post-initialization statistics::
  !/
  !  call write_prefix; write(iUnitOut,*) 'Mass in the flux rope on processor::',&
  !       iProc,Mrope_GL98
  if (iProc==0) then
     call write_prefix; write(iUnitOut,*) ''
     call write_prefix; write(iUnitOut,*) &
          '>>>>>>>>>>>>>>>>>>> Pressure and Density Log <<<<<<<<<<<<<<<<<<<<<'
     call write_prefix; write(iUnitOut,*) 'At PE=0'
     call write_prefix; write(iUnitOut,*) 'The value of MaxB0_1 is :: ',MaxB0_1
     call write_prefix; write(iUnitOut,*) 'The value of MaxB0_2 is :: ',MaxB0_2
     call write_prefix; write(iUnitOut,*) 'The value of Bnot    is :: ',Bnot
     pMin = minval_BLK(1,State_VGB(P_,:,:,:,:))
     pMax = maxval_BLK(1,State_VGB(P_,:,:,:,:))
     call write_prefix; write(iUnitOut,*) 'The min,max P is        :: ',&
          pMin, pMax
     RhoMin = minval_BLK(1,State_VGB(Rho_,:,:,:,:))
     RhoMax = maxval_BLK(1,State_VGB(Rho_,:,:,:,:))
     call write_prefix; write(iUnitOut,*) 'The min,max Rho is      :: ',&
          RhoMin, RhoMax
     call write_prefix; write(iUnitOut,*) 'The value of Tnot  is   :: ',Tnot
     call write_prefix; write(iUnitOut,*) 'The value of Gbody is   :: ',Gbody
     call write_prefix; write(iUnitOut,*) ''
     call write_prefix; write(iUnitOut,*) &
          '>>>>>>>>>>>>>>>>>>>                          <<<<<<<<<<<<<<<<<<<<<'
     call write_prefix; write(iUnitOut,*) ''
  end if
end subroutine post_init_stat

subroutine get_atmosphere_orig(i,j,k,iBLK,Dens_BLK,Pres_BLK)
  !
  !---------------------------------------------------------------------------
  !
  ! This module computes the background atmosphere in the 
  ! presence of gravity for an isothermal plasma::
  ! The subroutine is written by ILR on Feb 3, 2003.
  !
  !---------------------------------------------------------------------------
  !
  use ModGeometry,   ONLY: x_BLK,y_BLK,z_BLK
  use ModNumConst,   ONLY: cHalf,cOne
  use ModPhysics,    ONLY: inv_g
  implicit none
  
  integer, intent(in):: i,j,k,iBLK
  real, intent(out):: Dens_BLK,Pres_BLK
  real, parameter:: Rho0=cOne
  real:: xx,yy,zz,RR,BBr
  !\
  ! Get the coordinates and radial distance from the Sun::
  !/
  xx = x_BLK(i,j,k,iBLK)
  yy = y_BLK(i,j,k,iBLK)
  zz = z_BLK(i,j,k,iBLK)
  RR = sqrt(xx**2+yy**2+zz**2)
  if (RR > cHalf) then
     Dens_BLK = Rho0/RR**2
     Pres_BLK = Rho0*inv_g/RR**2
  else
     Dens_BLK = Rho0/cHalf**2
     Pres_BLK = Rho0*inv_g/cHalf**2
  endif
  
end subroutine get_atmosphere_orig
!=================================================================
! SUBROUTINE get_hlcmm
! Read H(eliographic) L(ongitude) of the C(entral) M(eridian) of 
! the M(ap). Assign Phi_Shift=HLCMM-180
subroutine get_hlcmm
  use ModUser,ONLY:Head_PFSSM, Phi_Shift,File_PFSSM
  use ModProcMH,ONLY:iProc
  use ModIO,       ONLY: iUnitOut, write_prefix
!  use CON_geopack,ONLY:is_advanced_hgr,get_advanced_hgr_longitude
  use ModNumConst
  implicit none
  real::HLCMM     !Heliographic Longitudee of the central meridian of map
  integer::iHLCMM !The same, but HLCMM is integer at WSO magnetograms
  integer::iErrorRead,iPosition
     iPosition=index(Head_PFSSM,'Centered')	
     if (iPosition>0.and.Phi_Shift<cZero)then	
        Head_PFSSM(1:len(Head_PFSSM)-iPosition)=&
	          Head_PFSSM(iPosition+1:len(Head_PFSSM))
        iPosition=index(Head_PFSSM,':')
        Head_PFSSM(1:len(Head_PFSSM)-iPosition)=&
	          Head_PFSSM(iPosition+1:len(Head_PFSSM))
        iPosition=index(Head_PFSSM,':')
        Head_PFSSM(1:len(Head_PFSSM)-iPosition)=&
                  Head_PFSSM(iPosition+1:len(Head_PFSSM))
        read(Head_PFSSM,'(i3)',iostat=iErrorRead)iHLCMM
        if(iErrorRead>0)call stop_mpi(&
                 'Can nod find HLCMM, '//File_PFSSM//&
	         ' is not a true WSO magnetogram')
        Phi_Shift=real(iHLCMM)-180
!        if(is_advanced_hgr())call get_advanced_hgr_longitude(&
!	         Phi_Shift,Phi_Shift)
	if(Phi_Shift<cZero)Phi_Shift=Phi_Shift+360
        if(iProc==0)then
           call write_prefix;write(iUnitOut,*)'Phi_Shift=',Phi_Shift
        end if
        return
     end if
     iPosition=index(Head_PFSSM,'Central')
     if(iPosition>0.and.Phi_Shift<cZero)then
        Head_PFSSM(1:len(Head_PFSSM)-iPosition)=&
                 Head_PFSSM(iPosition+1:len(Head_PFSSM))
        iPosition=index(Head_PFSSM,':')
        Head_PFSSM(1:len(Head_PFSSM)-iPosition)=&
	         Head_PFSSM(iPosition+1:len(Head_PFSSM))
        read(Head_PFSSM,*,iostat=iErrorRead)HLCMM
        if(iErrorRead>0)call stop_mpi(&
                 'Can nod find HLCMM, '//File_PFSSM//&
	         ' is not a true MDI magnetogram')
        Phi_Shift=HLCMM-180
!        if(is_advanced_hgr())call get_advanced_hgr_longitude(&
!	         Phi_Shift,Phi_Shift)
        if(Phi_Shift<cZero)Phi_Shift=Phi_Shift+360
        if(iProc==0)then
           call write_prefix;write(iUnitOut,*)'Phi_Shift=',Phi_Shift
        end if
     end if
end subroutine get_hlcmm

!============================================================================
subroutine get_user_b0(xx,yy,zz,B0_PFSSM)
  !
  !---------------------------------------------------------------------------
  ! This subroutine computes PFSS (Potential Field Source Surface)
  ! field model components in spherical coordinates at user-specified
  ! r (solar radii units, r>1) and theta, phi (both radians).
  ! The subroutine requires a file containing the spherical
  ! harmonic coefficients g(n,m) & h(n.m) obtained from a separate analysis 
  ! of a photospheric synoptic map, in a standard ("radial")
  ! format and normalization used by Stanford.
  !
  ! The PFSS field model assumes no currents in the corona and
  ! a pure radial field at the source surface, here R=2.5 Rsun
  !
  ! Get solar coefficients from Todd Hoeksema's files:
  !    1. Go to http://solar.stanford.edu/~wso/forms/prgs.html
  !    2. Fill in name and email as required
  !    3. Chose Carrington rotation (leave default 180 center longitude)
  ! For most requests of integer CRs with order < 20, result will come back
  ! immediately on the web.
  !    4. Count header lines before 1st (0,0) coefficient -this will be asked!
  !---------------------------------------------------------------------------
  ! Notes:
  !
  ! In the calling routine you must initialize one variable: istart=0 (it is a 
  ! flag used to tell the subroutine to read the coefficient file the first 
  ! time only). The first time around (DoFirst=0), the subroutine will ask for
  ! the coefficient file name, the order of the expansion to use (N_PFSSM=40 or 
  ! less*, but the coeff file can contain more orders than you use), and the 
  ! number of lines in the coefficient file header. (*note computation time 
  ! increases greatly with order used).
  !
  ! The source surface surface radius has been set at Rs=2.5*Ro in the 
  ! subroutine. PFSS fields at R>Rs are radial.(br,bthet,bphi) are the resulting
  ! components. Note the units of the B fields will differ with observatory used
  ! for the coefficients. Here we assume use of the wso coefficients so units are
  ! microT. The computation of the B fields is taken mainly from Altschuler, 
  ! Levine, Stix, and Harvey, "High Resolutin Mapping of the Magnetic Field of
  ! the Solar Corona," Solar Physics 51 (1977) pp. 345-375. It uses Schmidt
  ! normalized Legendre polynomials and the normalization is explained in the 
  ! paper. The field expansion in terms of the Schmidt normalized Pnm and dPnm's
  ! is best taken from Todd Hoeksema's notes which can be downloaded from the Web
  ! http://quake.stanford.edu/~wso/Description.ps
  ! The expansions  used to get include radial factors to make the field become
  ! purely radial at the source surface. The expans. in Altschuler et al assumed
  ! that the the coefficient g(n,m) and h(n,m) were the LOS coefficients -- but 
  ! the g(n,m) and h(n,m) now available are radial (according to Janet Luhman). 
  ! Therefore, she performs an initial correction to the g(n,m) and h(n,m) to 
  ! make them consistent with the the expansion. There is no reference for this
  ! correction.
  !---------------------------------------------------------------------------

  use ModMain,     ONLY: Time_Simulation, TypeCoordSystem
  use ModNumConst, ONLY: cZero,cHalf,cOne,cTwo,cThree, &
       cE1,cE9,cTolerance,cTiny,cDegToRad,cPi
  use ModProcMH,   ONLY: iProc
  use ModUser,     ONLY: DoFirst,File_PFSSM,Head_PFSSM,&
       N_PFSSM,R_PFSSM,Rs_PFSSM,Ro_PFSSM,   &
       H_PFSSM,iHead_PFSSM,factRatio1,     &
       g_nm,h_nm,UnitB,Phi_Shift
  use ModPhysics,  ONLY: unitUSER_B,OmegaBody,unitUSER_t
  use ModIO,       ONLY: iUnitOut, write_prefix
  use ModIoUnit,   ONLY: io_unit_new
  use CON_axes,    ONLY: transform_matrix, XyzPlanetHgi_D

  implicit none

  real, intent(in):: xx,yy,zz
  real, intent(out), dimension(3):: B0_PFSSM
  integer:: iError
  integer:: i,n,m
  real:: gtemp,htemp
  real:: c_n
  real:: SinPhi,CosPhi
  real:: sinmPhi,cosmPhi
  real:: CosTheta,SinTheta
  real:: stuff1,stuff2,stuff3
  real:: SumR,SumT,SumP,SumPsi
  real:: Rin_PFSSM,Theta_PFSSM,Phi_PFSSM
  real:: Br_PFSSM,Btheta_PFSSM,Bphi_PFSSM,Psi_PFSSM
  real, dimension(N_PFSSM+1,N_PFSSM+1):: p_nm,dp_nm

  integer :: iUnit

  !\
  ! Optimization by G. Toth::
  !/
  integer, parameter:: MaxInt=10000
  real, save:: Sqrt_I(MaxInt)
  real:: Xy
  real:: SinThetaM, SinThetaM1
  integer:: delta_m0
  real, dimension(-1:N_PFSSM+2):: RoRsPower_I, RoRPower_I, rRsPower_I
  !--------------------------------------------------------------------------
  !\
  ! Calculate cell-centered spherical coordinates::
  !/
  Rin_PFSSM   = sqrt(xx**2+yy**2+zz**2)
  !\
  ! Avoid calculating B0 inside a critical radius = 0.5*Rsun
  !/
  if (Rin_PFSSM < 9.00E-01) then
     B0_PFSSM = cZero
     RETURN
  end if
  Theta_PFSSM = acos(zz/Rin_PFSSM)
  Xy          = sqrt(xx**2+yy**2+cTiny**2)
  Phi_PFSSM   = atan2(yy,xx)
  SinTheta    = Xy/Rin_PFSSM
  CosTheta    = zz/Rin_PFSSM
  SinPhi      = yy/Xy
  CosPhi      = xx/Xy
  !\
  ! Set the source surface radius::
  ! The inner boundary in the simulations starts at a height
  ! H_PFSSM above that of the magnetic field measurements!
  !/
  if ((Rin_PFSSM+H_PFSSM) > Rs_PFSSM) then 
     R_PFSSM = Rs_PFSSM
  else
     R_PFSSM = Rin_PFSSM+H_PFSSM 
  endif

  if (DoFirst) then
     !\
     ! Initialize once g(n+1,m+1) & h(n+1,m+1) by reading a file
     ! created from Web data::
     !/ 
     DoFirst=.false.
     if (iProc==0) then
        call write_prefix; write(iUnitOut,*) 'Norder = ',N_PFSSM
        call write_prefix; write(iUnitOut,*) 'Entered coefficient file name :: ',File_PFSSM
        call write_prefix; write(iUnitOut,*) 'Entered number of header lines:: ',iHead_PFSSM
     endif
     !\
     ! Formats adjusted for wso CR rad coeffs::
     !/
     iUnit = io_unit_new()
     open(iUnit,file=File_PFSSM,status='old',iostat=iError)
     if (iHead_PFSSM /= 0) then
        do i=1,iHead_PFSSM
           read(iUnit,'(a)') Head_PFSSM
           if(Phi_Shift<-cTiny)call get_hlcmm	
        enddo
     endif
     !\
     ! Initialize all coefficient arrays::
     !/
     g_nm(:,:) = cZero; h_nm(:,:)  = cZero
     p_nm(:,:) = cZero; dp_nm(:,:) = cZero
     !\
     ! Read file with coefficients, g_nm and h_nm::
     !/
     do
        read(iUnit,*,iostat=iError) n,m,gtemp,htemp
        if (iError /= 0) EXIT
        if (n > N_PFSSM .or. m > N_PFSSM) CYCLE
        g_nm(n+1,m+1) = gtemp
        h_nm(n+1,m+1) = htemp
     enddo
     close(iUnit)
     !\
     ! Add correction factor for radial, not LOS, coefficients::
     ! Note old "coefficients" file are LOS, all new coeffs and 
     ! files are radial)
     !/
     do n=0,N_PFSSM
        stuff1 = cOne/real(n+1+(n/(Rs_PFSSM**(2*n+1))))
        do m=0,n
           g_nm(n+1,m+1) = g_nm(n+1,m+1)*stuff1
           h_nm(n+1,m+1) = h_nm(n+1,m+1)*stuff1
        enddo
     enddo
     !\
     ! Calculate sqrt(integer) from 1 to 10000::
     !/
     do m=1,MaxInt
        Sqrt_I(m) = sqrt(real(m))
     end do
     !\
     ! Calculate the ratio sqrt(2m!)/(2^m*m!)::
     !/
     factRatio1(:) = cZero; factRatio1(1) = cOne
     do m=1,N_PFSSM
        factRatio1(m+1) = factRatio1(m)*Sqrt_I(2*m-1)/Sqrt_I(2*m)
     enddo

  end if
  !\
  ! Transform Phi_PFSSM from the component's frame to the magnetogram's frame.
  !/
  Phi_PFSSM = Phi_PFSSM - Phi_Shift*cDegToRad

  !\
  ! Calculate powers of the ratios of radii
  !/
  rRsPower_I(-1) = Rs_PFSSM/R_PFSSM ! This one can have negative power.
  rRsPower_I(0)  = cOne
  RoRsPower_I(0) = cOne
  RoRPower_I(0)  = cOne
  do m=1,N_PFSSM+2
     RoRsPower_I(m) = RoRsPower_I(m-1) * (Ro_PFSSM/Rs_PFSSM)
     RoRPower_I(m)  = RoRPower_I(m-1)  * (Ro_PFSSM/R_PFSSM)
     rRsPower_I(m)  = rRsPower_I(m-1)  * (R_PFSSM /Rs_PFSSM)
  end do
  !\
  ! Calculate polynomials with appropriate normalization
  ! for Theta_PFSSMa::
  !/
  SinThetaM  = cOne
  SinThetaM1 = cOne

  do m=0,N_PFSSM
     if (m == 0) then
        delta_m0 = 1
     else
        delta_m0 = 0
     endif
     !\
     ! Eq.(27) from Altschuler et al. 1976::
     !/
     p_nm(m+1,m+1) = factRatio1(m+1)*Sqrt_I((2-delta_m0)*(2*m+1))* &
          SinThetaM
     !\
     ! Eq.(28) from Altschuler et al. 1976::
     !/
     if (m < N_PFSSM) p_nm(m+2,m+1) = p_nm(m+1,m+1)*Sqrt_I(2*m+3)* &
          CosTheta
     !\
     ! Eq.(30) from Altschuler et al. 1976::
     !/
     dp_nm(m+1,m+1) = factRatio1(m+1)*Sqrt_I((2-delta_m0)*(2*m+1))*&
          m*CosTheta*SinThetaM1
     !\
     ! Eq.(31) from Altschuler et al. 1976::
     !/
     if (m < N_PFSSM) &
          dp_nm(m+2,m+1) = Sqrt_I(2*m+3)*(CosTheta*&
          dp_nm(m+1,m+1)-SinTheta*p_nm(m+1,m+1))

     SinThetaM1 = SinThetaM
     SinThetaM  = SinThetaM*SinTheta

  enddo
  do m=0,N_PFSSM-2; do n=m+2,N_PFSSM
     !\
     ! Eq.(29) from Altschuler et al. 1976::
     !/
     stuff1         = Sqrt_I(2*n+1)/Sqrt_I(n**2-m**2)
     stuff2         = Sqrt_I(2*n-1)
     stuff3         = Sqrt_I((n-1)**2-m**2)/Sqrt_I(2*n-3)
     p_nm(n+1,m+1)  = stuff1*(stuff2*CosTheta*p_nm(n,m+1)-  &
          stuff3*p_nm(n-1,m+1))
     !\
     ! Eq.(32) from Altschuler et al. 1976::
     !/
     dp_nm(n+1,m+1) = stuff1*(stuff2*(CosTheta*dp_nm(n,m+1)-&
          SinTheta*p_nm(n,m+1))-stuff3*dp_nm(n-1,m+1))
  enddo; enddo
  !\
  ! Apply Schmidt normalization::
  !/
  do m=0,N_PFSSM; do n=m,N_PFSSM
     !\
     ! Eq.(33) from Altschuler et al. 1976::
     !/
     stuff1 = cOne/Sqrt_I(2*n+1)
     !\
     ! Eq.(34) from Altschuler et al. 1976::
     !/
     p_nm(n+1,m+1)  = p_nm(n+1,m+1)*stuff1
     dp_nm(n+1,m+1) = dp_nm(n+1,m+1)*stuff1
  enddo; enddo
  !\
  ! Truncate the value of SinTheta::
  !/
  if (SinTheta == cZero) SinTheta = cOne/(cE9*cE1)
  !\
  ! Initialize the values of SumR,SumT,SumP, and SumPsi::
  !/
  SumR = cZero; SumT   = cZero
  SumP = cZero; SumPsi = cZero
  !\
  ! Leave out monopole (n=0) term::
  !/
  g_nm(1,1) = cZero
  !\
  ! Calculate B for (R_PFSSM,Phi_PFSSM)::
  ! Also calculate magnetic potential Psi_PFSSM
  !/
  do m=0,N_PFSSM
     cosmPhi  = cos(m*Phi_PFSSM)
     sinmPhi  = sin(m*Phi_PFSSM)
     do n=m,N_PFSSM
        !\
        ! c_n corresponds to Todd's c_l::
        !/
        c_n    = -RoRsPower_I(n+2)
        !\
        ! Br_PFSSM = -d(Psi_PFSSM)/dR_PFSSM::
        !/
        stuff1 = (n+1)*RoRPower_I(n+2)-c_n*n*rRsPower_I(n-1)
        stuff2 = g_nm(n+1,m+1)*cosmPhi+h_nm(n+1,m+1)*sinmPhi
        SumR   = SumR + p_nm(n+1,m+1)*stuff1*stuff2
        !\
        ! Bt_PFSSM = -(1/R_PFSSM)*d(Psi_PFSSM)/dTheta_PFSSM::
        !/
        stuff1 = RoRPower_I(n+2)+c_n*rRsPower_I(n-1)
        SumT   = SumT-dp_nm(n+1,m+1)*stuff1*stuff2
        !\
        ! Psi_PFSSM::
        !/
        SumPsi = SumPsi+R_PFSSM*p_nm(n+1,m+1)*stuff1*stuff2
        !\
        ! Bp_PFSSM = -(1/R_PFSSM)*d(Psi_PFSSM)/dPhi_PFSSM::
        !/
        stuff2 = g_nm(n+1,m+1)*sinmPhi-h_nm(n+1,m+1)*cosmPhi
        SumP   = SumP + p_nm(n+1,m+1)*m/SinTheta*stuff1*stuff2
     enddo
  enddo
  !\
  ! Compute (Br_PFSSM,Btheta_PFSSM,Bphi_PFSSM) and Psi_PFSSM::
  !/
  Psi_PFSSM    = SumPsi
  Br_PFSSM     = SumR
  if (Rin_PFSSM > Rs_PFSSM) Br_PFSSM = Br_PFSSM*(Rs_PFSSM/Rin_PFSSM)**2
  Btheta_PFSSM = SumT
  Bphi_PFSSM   = SumP
  !\
  ! Magnetic field components in global Cartesian coordinates::
  ! Set B0xCell_BLK::
  !/
  B0_PFSSM(1) = Br_PFSSM*SinTheta*CosPhi+    &
                Btheta_PFSSM*CosTheta*CosPhi-&
                Bphi_PFSSM*SinPhi
  !\
  ! Set B0yCell_BLK::
  !/
  B0_PFSSM(2) = Br_PFSSM*SinTheta*SinPhi+    &
                Btheta_PFSSM*CosTheta*SinPhi+&
                Bphi_PFSSM*CosPhi
  !\
  ! Set B0zCell_BLK::
  !/
  B0_PFSSM(3) = Br_PFSSM*CosTheta-           &
                Btheta_PFSSM*SinTheta
  !\
  ! Apply field strength normalization::
  ! UnitB contains the units of the CR file relative to 1 Gauss
  ! and a possible correction factor (e.g. 1.8 or 1.7).
  !/
  B0_PFSSM(1:3) = B0_PFSSM(1:3)*(UnitB/unitUSER_B)

end subroutine get_user_b0
!========================================================================
!========================================================================
!  SUBROUTINE USER_SET_OUTER_BCS 
!========================================================================
! This subroutine allows the user to apply boundary conditions to the outer
! body which are problem specific and cannot be created using the predefined
! options in BATSRUS.
! The variables specific to the problem are loaded from ModUser
! Any of the outer boundary specified in BATSRUS can be used here.
  subroutine user_set_outerBCs(iBlock,TypeBc,IsFound)
  use ModMain
  use ModPhysics
  use ModProcMH
  use ModAdvance 
  use ModGeometry
  use ModSetOuterBC
  use ModUser
  implicit none
  integer,intent(in)::iBlock
  ! Variables required by this user subroutine
  logical,intent(out) :: IsFound
  character (len=20),intent(in) :: TypeBc
end subroutine user_set_outerBCs
!========================================================================
!========================================================================
!  SUBROUTINE USER_SPECIFY_INITIAL_REFINEMENT
!========================================================================
!
!\
! This subroutine allows the user to add an initial refinement type
! based on a geometric criteria.  The `case' specified in the PARAM.in file
! will be read here.
! As with all user subroutines, the variables declared in ModUser are
! available here.  Again, as with other user subroutines DO NOT MODIFY ANY
! GLOBAL VARIABLE DEFINED IN THE MODULES INCLUDED IN THIS SUBROUTINE UNLESS
! SPECIFIED!!
!/
subroutine user_specify_initial_refinement(iBLK,RefineBlock,lev, &
     dxBlock,xCenter,yCenter,zCenter,rCenter,minx,miny,minz,minR,&
     maxx,maxy,maxz,maxR,IsFound)
  use ModSize,       ONLY: nI,nJ,nK,gcn
  use ModVarIndexes, ONLY: Bx_,By_,Bz_
  use ModAdvance,    ONLY: B0xCell_BLK,B0yCell_BLK,       &
       B0zCell_BLK,State_VGB
  use ModAMR,        ONLY: InitialRefineType
  use ModMain,       ONLY: x_,y_,z_,iteration_number,     &
       time_loop
  use ModGeometry,   ONLY: XyzMin_D,XyzMax_D,x_BLK,y_BLK, &
       z_BLK,R_BLK,dx_BLK,dy_BLK,dz_BLK
  use ModNumConst,   ONLY: cTiny,cHundredth,cEighth,cHalf,&
       cQuarter,cOne,cTwo,cFour,cE1,cE2
  use ModPhysics,    ONLY: unitUSER_B
  use ModUser
  implicit none
  !------------------------------------------------------------------------
  logical, intent(out):: RefineBlock,IsFound
  integer, intent(in):: lev
  real, intent(in):: dxBlock
  real, intent(in):: xCenter,yCenter,zCenter,rCenter
  real, intent(in):: minx,miny,minz,minR
  real, intent(in):: maxx,maxy,maxz,maxR
  integer, intent(in):: iBLK
  !------------------------------------------------------------------------
  logical:: IsInRangeCS
  real:: dsMin,dsNewMin
  real:: critx,critxCenter,R2Cell
  real, dimension(3):: RCell_D
  !------------------------------------------------------------------------
  integer:: i,j,k
  real:: XCell,YCell,ZCell,RCell,RCentre
  real:: B0xCell,B0yCell,B0zCell
  real:: BIxCell,BIyCell,BIzCell
  logical, dimension(3):: IsGhostCell_D
  real, dimension(1-gcn:nI+gcn,1-gcn:nJ+gcn,1-gcn:nK+gcn):: Br_D
  !------------------------------------------------------------------------
  dsMin = min(&
       dx_BLK(iBLK),dy_BLK(iBLK),dz_BLK(iBLK))
  RCell_D (x_) = xCenter
  RCell_D (y_) = yCenter
  RCell_D (z_) = zCenter
  R2Cell = sqrt(dot_product(RCell_D,RCell_D))
  RefineBlock = .false.
  IsInRangeCS = .false.
  !\
  ! If not in the time_loop, refine the current sheet over
  ! the course of 3 iterations::
  !/
  if (time_loop) then
     dsNewMin = (cOne+cQuarter)/cFour+cHundredth
     if (dsMin>dsNewMin) then
        do k=1-gcn,nK+gcn
           IsGhostCell_D(3)=k<1.or.k>nK
           do j=1-gcn,nJ+gcn
              IsGhostCell_D(2)=j<1.or.j>nJ           
              do i=1-gcn,nI+gcn
                 IsGhostCell_D(1)=i<1.or.i>nI
                 if (count(IsGhostCell_D)>1) then
                    Br_D(i,j,k) = huge(cOne)
                    CYCLE
                 end if
                 XCell = x_BLK(i,j,k,iBLK)
                 YCell = y_BLK(i,j,k,iBLK)
                 ZCell = z_BLK(i,j,k,iBLK)
                 RCell = sqrt(XCell**2+YCell**2+ZCell**2)
                 B0xCell = B0xCell_BLK(i,j,k,iBLK)
                 B0yCell = B0yCell_BLK(i,j,k,iBLK)
                 B0zCell = B0zCell_BLK(i,j,k,iBLK)
                 BIxCell = State_VGB(Bx_,i,j,k,iBLK)
                 BIyCell = State_VGB(By_,i,j,k,iBLK)
                 BIzCell = State_VGB(Bz_,i,j,k,iBLK)
                 Br_D(i,j,k) = abs(             &
                      (XCell*(B0xCell+BIxCell)+ &
                       YCell*(B0yCell+BIyCell)+ &
                       ZCell*(B0zCell+BIzCell))/&
                       RCell)
              end do
           end do
        end do
        !\
        ! Construct refinement criteria to refine blocks in the
        ! current sheet only (IsInRangeCS = .true.)::
        !/
        RCentre = cEighth*&
             (R_BLK( 1, 1, 1,iBLK)+R_BLK( 1, 1,nK,iBLK)+&
              R_BLK( 1,nJ, 1,iBLK)+R_BLK( 1,nJ,nK,iBLK)+&
              R_BLK(nI, 1, 1,iBLK)+R_BLK(nI, 1,nK,iBLK)+&
              R_BLK(nI,nJ, 1,iBLK)+R_BLK(nI,nJ,nK,iBLK))
        IsInRangeCS = (RCentre**3*minval(Br_D)<3.50E-01)
     end if
  end if

  select case (InitialRefineType)
  case('UserHELIO','UserHelio','userhelio')
     if (lev<3) then
        RefineBlock = .true.
     else if (lev<10) then
        critx=(XyzMax_D(1)-XyzMin_D(1))/(2.0**real(lev-2))
        if (rCenter<1.10+critx) then
           RefineBlock = .true.
        end if
     endif
     IsFound = .true.
  case('UserAR486','userar486','USERAR486')
     if (.not.time_loop) then
        if (lev<4) then
           RefineBlock = .true.
        else if (lev<10) then
           !Block is in the CS or intersects the body::
           RefineBlock = IsInRangeCS.or.(minR<=1.00)
        endif
        !Do not refine inside the body::
        RefineBlock = RefineBlock.and.(maxR>1.00)
     else
        !Block is in the CS::
        RefineBlock = IsInRangeCS
     end if
     IsFound = .true.
  endselect
end subroutine user_specify_initial_refinement
!========================================================================
!========================================================================
!  SUBROUTINE user_amr_criteria
!========================================================================
!
!\
! This subroutine allows the user to add a refinement type
! based on a geometric criteria or physical criteria.  The `case'
! specified in the #AMRCRITERIA file will be read here.
!/
subroutine user_amr_criteria(iBLK, userCriteria, TypeCriteria, IsFound)
  use ModMain
  use ModAdvance
  use ModGeometry, ONLY:x_BLK,y_BLK,z_BLK,R_BLK,&
       dx_BLK,dy_BLK,dz_BLK,true_cell
  use ModPhysics
  use ModConst
  use ModUser
  implicit none
  !\ 
  ! Variables required by this user subroutine::
  !/
  integer, intent(in):: iBLK
  logical, intent(out):: IsFound
  integer, intent(out):: userCriteria
  character (len=20),intent(in):: TypeCriteria
  !\
  ! Local variables::
  !/
  logical:: IsInRange
  integer:: i,j,k
  real:: dsMin,dsMax,dsTwo
  real:: XCell,YCell,ZCell,RCell,RCenter
  real:: B0xCell,B0yCell,B0zCell,MinBr,MaxBr
  real:: BIxCell,BIyCell,BIzCell
  real, dimension(1-gcn:nI+gcn,1-gcn:nJ+gcn,1-gcn:nK+gcn):: Br_D
  logical,dimension(3)::IsGhostCell_D
  !\
  ! Find the radial location of the center of the block and
  ! the min/max cell size::
  !/
  RCenter = cEighth*&
       (R_BLK( 1, 1, 1,iBLK)+R_BLK( 1, 1,nK,iBLK)+&
        R_BLK( 1,nJ, 1,iBLK)+R_BLK( 1,nJ,nK,iBLK)+&
        R_BLK(nI, 1, 1,iBLK)+R_BLK(nI, 1,nK,iBLK)+&
        R_BLK(nI,nJ, 1,iBLK)+R_BLK(nI,nJ,nK,iBLK))
  dsMin = min(dx_BLK(iBLK),dy_BLK(iBLK),dz_BLK(iBLK))
  dsMax = max(dx_BLK(iBLK),dy_BLK(iBLK),dz_BLK(iBLK))
  dsTwo = dsMin*dsMax
  
  select case (TypeCriteria)
  case('UserCS','USERCS','usercs')
     !\
     ! Get the radial magnetic field in iBLK::
     !/
     do k=1-gcn,nK+gcn
        IsGhostCell_D(3)=k<1.or.k>nK
        do j=1-gcn,nJ+gcn
           IsGhostCell_D(2)=j<1.or.j>nJ           
           do i=1-gcn,nI+gcn
              IsGhostCell_D(1)=i<1.or.i>nI
              if (count(IsGhostCell_D)>1) then
                 Br_D(i,j,k) = huge(cOne)
                 CYCLE
              end if
              XCell   = x_BLK(i,j,k,iBLK)
              YCell   = y_BLK(i,j,k,iBLK)
              ZCell   = z_BLK(i,j,k,iBLK)
              RCell   = R_BLK(i,j,k,iBLK)
              B0xCell = B0xCell_BLK(i,j,k,iBLK)
              B0yCell = B0yCell_BLK(i,j,k,iBLK)
              B0zCell = B0zCell_BLK(i,j,k,iBLK)
              BIxCell = State_VGB(Bx_,i,j,k,iBLK)
              BIyCell = State_VGB(By_,i,j,k,iBLK)
              BIzCell = State_VGB(Bz_,i,j,k,iBLK)
              Br_D(i,j,k) = abs(&
                   (XCell*(B0xCell+BIxCell)+ &
                    YCell*(B0yCell+BIyCell)+ &
                    ZCell*(B0zCell+BIzCell))/&
                    RCell)
           end do
        end do
     end do
     !\
     ! Find the minimum of abs(Br) in iBLK::
     !/
     MinBr = minval(Br_D)
     !\
     ! Construct refine criteria based on ds2, RCenter, and MinBr::
     !/
     IsInRange = (RCenter<1.50*Rs_PFSSM).and.&
                 (MinBr<3.0E-05) 
     if (IsInRange) then
        userCriteria = dsTwo*RCenter*exp(-MinBr)
     else
        userCriteria = dsTwo*RCenter*exp(-MinBr)/cE6
     end if
     IsFound = .true.
  endselect
end subroutine user_amr_criteria
!========================================================================
!========================================================================
!  SUBROUTINE user_set_ICs
! (It will include set_ICs_global.f90
!!\
! Calculates the initial conditions of the grid for the Global Heliosphere
!
! Written by Merav Opher Feb 14  2002
!/
! OmegaBody is the rotation frequency of the Sun
!========================================================================

! This subroutine allows the user to apply initial conditions to the domain
! which are problem specific and cannot be created using the predefined
! options in BATSRUS.
! The variables specific to the problem are loaded from ModUser
subroutine user_set_ICs
  use ModMain,     ONLY: nI,nJ,nK,gcn,globalBLK
  use ModAdvance
  use ModGeometry, ONLY: x2,y2,z2,x_BLK,y_BLK,z_BLK,R_BLK,true_cell
  use ModIO,       ONLY: restart
  use ModPhysics
  use ModUser
  implicit none
end subroutine user_set_ICs
!========================================================================
!========================================================================
subroutine user_update_states(iStage,iBlock)
  use ModVarIndexes
  use ModSize
  use ModAdvance, ONLY: State_VGB
  use ModMain,    ONLY: nStage
  use ModPhysics, ONLY: inv_gm1
  implicit none
  integer,intent(in):: iStage,iBlock
  integer:: i,j,k
  real:: DensCell,PresCell,GammaCell
  
  call update_states_MHD(iStage,iBlock)
  !\
  ! Begin update of pressure and relaxation energy::
  !/
  !  if (iStage/=nStage) return
  do k=1,nK; do j=1,nJ; do i=1,nI
     call get_plasma_parameters_cell(i,j,k,iBlock,&
          DensCell,PresCell,GammaCell)
     State_VGB(P_   ,i,j,k,iBlock)=           &
          (GammaCell-cOne)*                   &
          (inv_gm1*State_VGB(P_,i,j,k,iBlock)&
          !+State_VGB(EnergyRL_,i,j,k,iBlock)&  !^CFG UNCOMMENT IF ALWAVES
          )
    ! State_VGB(EnergyRL_,i,j,k,iBlock)=           &!^CFG UNCOMMENT IF ALWAVES
    !      State_VGB(P_,i,j,k,iBlock)*(cOne   /&    !^CFG UNCOMMENT IF ALWAVES
    !      (GammaCell-cOne)-inv_gm1)                !^CFG UNCOMMENT IF ALWAVES
  end do; end do; end do
  call correctE
  !\
  ! End update of pressure and relaxation energy::
  !/
end subroutine user_update_states
!========================================================================
!========================================================================
!  SUBROUTINE user_write_progress
!========================================================================

!
! This subroutine allows the user to write to the log files variables
! (normalized and not) which are problem specific.
!
! The variables specific to the problem are loaded from ModUser
subroutine user_write_progress
  use ModProcMH
  use ModMain
  use ModPhysics
  use ModUser
  implicit none
end subroutine user_write_progress
!========================================================================
!========================================================================
!  SUBROUTINE user_get_log_var
!========================================================================
!
! This subroutine allows the user to write to the log files variables
! (normalized and not) which are problem specific.
!
! The variables specific to the problem are loaded from ModUser.
!
!========================================================================
subroutine user_get_log_var(VarValue,TypeVar)
  use ModProcMH,     ONLY: nProc
  use ModIO,         ONLY: dn_output,logfile_,write_myname
  use ModMain,       ONLY: unusedBLK,nBLK,iteration_number,   &
       x_,y_,z_
  use ModVarIndexes, ONLY: &
 !       EnergyRL_,&     !^CFG UNCOMMENT IF ALWAVES
        Bx_,By_,Bz_,rho_,rhoUx_,rhoUy_,rhoUz_,P_ 
  use ModGeometry,   ONLY: R_BLK
  use ModAdvance,    ONLY: State_VGB,tmp1_BLK,B0xCell_BLK,    &
       B0yCell_BLK,B0zCell_BLK
  use ModPhysics,    ONLY: inv_gm1,unitSI_energydens,unitSI_x,&
       unitSI_U,unitSI_rho
  use ModNumConst,   ONLY: cOne,cHalf,cE1,cE3,cE6
  use ModUser
  implicit none
  real, intent(out):: VarValue
  character (LEN=10), intent(in):: TypeVar 
  !
  integer:: iBLK
  real:: unit_energy,unit_mass
  real, external:: integrate_BLK
  !--------------------------------------------------------------------------
  unit_energy = cE1*cE6*unitSI_energydens*unitSI_x**3
  unit_mass   = cE3*unitSI_rho*unitSI_x**3
  !\
  ! Define log variable to be saved::
  !/
  select case(TypeVar)
  case('em_t','Em_t','em_r','Em_r')
     do iBLK=1,nBLK
        if (unusedBLK(iBLK)) cycle
        tmp1_BLK(:,:,:,iBLK) = &
             (B0xcell_BLK(:,:,:,iBLK)+State_VGB(Bx_,:,:,:,iBLK))**2+&
             (B0ycell_BLK(:,:,:,iBLK)+State_VGB(By_,:,:,:,iBLK))**2+&
             (B0zcell_BLK(:,:,:,iBLK)+State_VGB(Bz_,:,:,:,iBLK))**2
     end do
     VarValue = unit_energy*cHalf*integrate_BLK(1,tmp1_BLK)
  case('ek_t','Ek_t','ek_r','Ek_r')
     do iBLK=1,nBLK
        if (unusedBLK(iBLK)) cycle
        tmp1_BLK(:,:,:,iBLK) = &
             (State_VGB(rhoUx_,:,:,:,iBLK)**2 +&
              State_VGB(rhoUy_,:,:,:,iBLK)**2 +&
              State_VGB(rhoUz_,:,:,:,iBLK)**2)/&
              State_VGB(rho_  ,:,:,:,iBLK)             
     end do
     VarValue = unit_energy*cHalf*integrate_BLK(1,tmp1_BLK)
  case('et_t','Et_t','et_r','Et_r')
     do iBLK=1,nBLK
        if (unusedBLK(iBLK)) cycle
        tmp1_BLK(:,:,:,iBLK) = State_VGB(P_,:,:,:,iBLK)
     end do
     VarValue = unit_energy*inv_gm1*integrate_BLK(1,tmp1_BLK)
  case('ew_t','Ew_t','ew_r','Ew_r')
     do iBLK=1,nBLK
        if (unusedBLK(iBLK)) cycle
      ! tmp1_BLK(:,:,:,iBLK) = & !^CFG UNCOMMENT IF ALWAVES
      !      State_VGB(EnergyRL_,:,:,:,iBLK) !^CFG UNCOMMENT IF ALWAVES
     end do
     VarValue = unit_energy*integrate_BLK(1,tmp1_BLK)
  case('ms_t','Ms_t')
     do iBLK=1,nBLK
        if (unusedBLK(iBLK)) cycle
        tmp1_BLK(:,:,:,iBLK) = &
             State_VGB(rho_,:,:,:,iBLK)/R_BLK(:,:,:,iBLK)
     end do
     VarValue = unit_mass*integrate_BLK(1,tmp1_BLK)
  case('vol','Vol')
     tmp1_BLK(:,:,:,iBLK) = cOne
     VarValue = integrate_BLK(1,tmp1_BLK)
  case default
     VarValue = -7777.
     call write_myname;
     write(*,*) 'Warning in set_user_logvar: unknown logvarname = ',TypeVar
  end select
end subroutine user_get_log_var
!========================================================================
