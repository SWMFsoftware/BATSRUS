!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module GM_couple_pw

  ! Coupling with the Polar Wind component.

  use ModUtilities, ONLY: CON_set_do_test, CON_stop
  use BATL_lib, ONLY: nProc, iComm
  use ModNumConst, ONLY: cDegToRad
  use ModMultiFluid, ONLY: nIonFluid

  implicit none
  save

  ! cannot use private as read_pw_buffer needs almost all the variables

  ! This is what the SWMF couplers use
  public:: GM_get_for_pw
  public:: GM_put_from_pw

  ! Local variables

  character (len=3) :: NamePwCoord = '???'
  integer, parameter :: nCoord=3

  ! StateGm_VI stores the PW variables that occur in GM too
  ! These are interpolated then to the GM grid
  real, allocatable :: CoordXyzPw_DI(:,:), CoordXyPw_DI(:,:), StateGm_VI(:,:),&
       CoordXyzPw1_DI(:,:), CoordXyzPw2_DI(:,:), &
       StateGm1_VI(:,:), StateGm2_VI(:,:)

  integer, allocatable :: iLst1_I(:), lPtr1_I(:), lEnd1_I(:),&
       iLst2_I(:), lPtr2_I(:), lEnd2_I(:)

  integer, parameter :: nOuterPoint=12
  real,    parameter :: dThetaOuter=5.0*cDegToRad

  integer :: iCoupleFluid_I(nIonFluid) = -1

  integer :: nVarPw=-1, nSpeciesPw=-1, nLinePw=-1, nPoint=-1, nTriangle=-1
  integer :: iRhoPwFirst=-1, iRhoPwLast=-1, iUPwFirst=-1, iUPwLast=-1
  integer :: iRhoGmFirst=-1, iRhoGmLast=-1, iUGmFirst=-1, iUGmLast=-1
  integer :: nPoint1=-1, nPoint2=-1,nTriangle1=-1,nTriangle2=-1
  integer :: nLinePw1 = -1, nLinePw2 = -1

contains
  !============================================================================
  subroutine GM_get_for_pw(nTotalLine, Buffer_I)

    use ModVarIndexes, ONLY: p_
    use ModMain, ONLY: nBlock
    use ModPhysics, ONLY: No2Si_V, UnitP_
    use ModCurrent, ONLY: get_point_data
    use ModAdvance, ONLY: State_VGB
    use ModB0, ONLY: B0_DGB
    use ModUpdateStateFast, ONLY: sync_cpu_gpu
    use ModMpi

    integer,intent(in)  :: nTotalLine
    real,   intent(out) :: Buffer_I(nTotalLine)
    real                :: WeightAndP_I(2)
    integer             :: iLine, iError

    character(len=*), parameter:: NameSub = 'GM_get_for_pw'
    !--------------------------------------------------------------------------
    call sync_cpu_gpu('update on CPU', NameSub, State_VGB, B0_DGB)

    ! Get the pressure at each field line location.
    do iLine = 1, nTotalLine
       call get_point_data(&
            0.0, CoordXyzPW_DI(:,iLine), 1, nBlock, p_, p_, WeightAndP_I)
       Buffer_I(iLine) = WeightAndP_I(2)
    enddo

    ! Sum contributions from all PE-s to processor 0
    if(nProc > 1) call MPI_reduce_real_array(Buffer_I, nTotalLine, &
         MPI_SUM, 0, iComm, iError)

    ! Convert pressure to SI units
    Buffer_I = Buffer_I * No2Si_V(UnitP_)

  end subroutine GM_get_for_pw
  !============================================================================
  subroutine GM_put_from_pw(Buffer_VI, nVar, nFieldLine, Name_V)

    use CON_coupler, ONLY: PW_, Grid_C
    use ModMain, ONLY: x_, y_,z_, TypeCoordSystem, tSimulation
    use ModVarIndexes, ONLY: SpeciesFirst_, SpeciesLast_, NameVar_V
    use ModAdvance, ONLY: UseMultiSpecies
    use ModMultiFluid, ONLY: UseMultiIon, nIonFluid
    use ModPhysics, ONLY: Si2No_V, UnitRho_, UnitU_, rCurrents
    use ModNumConst, ONLY: cTwoPi,cHalfPi
    use ModTriangulateSpherical, ONLY:trmesh, trplot
    use CON_axes, ONLY: transform_matrix
    use ModCoordTransform, ONLY: dir_to_xyz
    use CON_planet_field, ONLY: map_planet_field
    use ModIoUnit, ONLY: UnitTmp_
    use ModUtilities, ONLY: open_file, close_file

    integer, parameter :: Theta_=1, Phi_=2

    integer,          intent(in):: nVar, nFieldLine
    real,             intent(in):: Buffer_VI(nVar, nFieldLine)
    character(len=*), intent(in):: Name_V(nVar)

    logical, save :: DoInitialize = .true.

    integer :: i, iLine, iHemisphere, j
    real    :: GmPw_DD(3,3), Theta, Phi, XyzPw_D(3), &
         SinThetaOuter1,SinThetaOuter2,CosThetaOuter1,CosThetaOuter2,&
         Tmp1_I(nFieldLine)

    integer :: iError=0 ! error counter for triangulation subroutine

    logical :: DoTest, DoTestMe
    character(len=*), parameter:: NameSub = 'GM_put_from_pw'
    !--------------------------------------------------------------------------
    call CON_set_do_test(NameSub, DoTest, DoTestMe)

    if(DoTestMe)write(*,*) NameSub,' starting: nVar, nFieldLine, Name_V=', &
         nVar, nFieldLine, Name_V

    ! If only 2 variables are passed, they are the coordinates colatitude
    ! and longitude needed for the GM->PW pressure coupling.
    if(nVar == 2)then
       if(.not.allocated(CoordXyzPw_DI)) allocate(CoordXyzPw_DI(3, nFieldLine))

       ! Convert from spherical to Cartesian coordinates at radius rCurrents
       ! where the pressure is going to be taken from
       do iLine = 1, nFieldLine
          ! Map field line from ionosphere (taken at r=1) to rCurrents
          Theta = Buffer_VI(Theta_, iLine)
          Phi   = Buffer_VI(Phi_,   iLine)
          call dir_to_xyz(Theta, Phi, XyzPw_D)
          call map_planet_field(tSimulation, XyzPw_D, 'SMG NORM', &
               rCurrents,  CoordXyzPw_DI(:,iLine), iHemisphere)
          if(iHemisphere == 0) CoordXyzPw_DI(:,iLine) = 0.0
       end do

       ! Convert from PW to GM coordinates if necessary
       NamePwCoord = Grid_C(PW_) % TypeCoord
       if(TypeCoordSystem /= NamePwCoord) then
          GmPw_DD = &
               transform_matrix(tSimulation, NamePwCoord, TypeCoordSystem)
          do iLine = 1, nFieldLine
             CoordXyzPw_DI(:,iLine) = matmul( GmPw_DD, CoordXyzPw_DI(:,iLine))
          end do
       end if

       RETURN
    end if

    if(DoInitialize)then
       DoInitialize=.false.

       nVarPw  = nVar
       nLinePw = nFieldLine
       nPoint = nLinePw + nOuterPoint

       nLinePw1 = count(Buffer_VI(Theta_,:) < cHalfPi)
       nPoint1 = nLinePw1 + nOuterPoint

       nLinePw2 = nLinePw - nLinePw1
       nPoint2 = nLinePw2 + nOuterPoint

       if(DoTestMe)then
          write(*,*) NameSub,' nVarPw, nLinePw, nOuterPoint, nPoint=',&
               nVarPw, nLinePw, nOuterPoint, nPoint
          write(*,*) NameSub,' nLinePw1, nLinePw2, nPoint1, nPoint2=',&
               nLinePw1, nLinePw2, nPoint1, nPoint2
       end if

       ! Subtract 2 coords and half of the remaining variables are velocities
       nSpeciesPw = (nVar-2)/2
       ! First 2 variables are coordinates,
       ! then nSpeciesPw densities then nSpeciesPw velocities
       iRhoPwFirst = 3
       iRhoPwLast  = 2 + nSpeciesPw
       iUPwFirst   = iRhoPwLast + 1
       iUPwLast    = nVar

       if(UseMultiSpecies)then
          ! Species densities and total velocity
          iRhoGmFirst = 1
          iRhoGmLast  = SpeciesLast_-SpeciesFirst_+1
          iUGmFirst   = iRhoGmLast + 1
          iUGmLast    = iUGmFirst

          ! Limit Pw variable range to the number of GM species (He is dropped)
          iRhoPwLast = &
               min(iRhoPwLast, iRhoPwFirst + SpeciesLast_ - SpeciesFirst_)
          iUPwLast = &
               min(iUPwLast, iUPwFirst + SpeciesLast_ - SpeciesFirst_)
       elseif(UseMultiIon)then
          ! Fluid densities and velocities
          iRhoGmFirst = 1
          iRhoGmLast  = nIonFluid
          iUGmFirst   = nIonFluid + 1
          iUGmLast    = 2*nIonFluid

          ! Build iCoupleFluid_I, which connects PW fluids with GM fluids.
          ! Assumption: MHD is using STANDARD FLUID NAMES and PW only has
          ! Op, Hep, and Hp to share.
          j = 1
          do i = 1, size(NameVar_V)
             if(DoTestMe)write(*,*) 'Checking var ', trim(NameVar_V(i)),'.'
             select case( trim(NameVar_V(i)) )
             case('HpRho')
                iCoupleFluid_I(j) = iRhoPwFirst
             case('OpRho')
                iCoupleFluid_I(j) = iRhoPwFirst + 1
             case('HepRho')
                iCoupleFluid_I(j) = iRhoPwFirst + 2
             end select
             ! Keep track of which ion species we are on by counting the
             ! number of variables that have the name <Species>Rho.
             if (index(NameVar_V(i), 'Rho')>1) j = j + 1
          end do

          if(DoTestMe)write(*,*) 'nIonFluid = ', nIonFluid
          if(DoTestMe)write(*,*) 'iCoupleFluid_I = (',iCoupleFluid_I,')'

       else
          ! Total density and velocity
          iRhoGmFirst = 1
          iRhoGmLast  = 1
          iUGmFirst   = 2
          iUGmLast    = 2
       end if

       if(DoTestMe)then
          write(*,*) NameSub, &
               ': iRhoGmFirst, iRhoGmLast, iUGmFirst, iUGmLast=', &
               iRhoGmFirst, iRhoGmLast, iUGmFirst, iUGmLast
          write(*,*) NameSub, &
               ': iRhoPwFirst, iRhoPwLast, iUPwFirst, iUPwLast=', &
               iRhoPwFirst, iRhoPwLast, iUPwFirst, iUPwLast
       end if

       allocate( &
            StateGm1_VI(iUGmLast,nPoint), &
            CoordXyzPw1_DI(nCoord,nPoint), &
            StateGm2_VI(iUGmLast,nPoint), &
            CoordXyzPw2_DI(nCoord,nPoint), &
            iLst1_I(6*(nPoint1-2)), &
            lPtr1_I(6*(nPoint1-2)), &
            lEnd1_I(nPoint1))

       ! Initialize arrays to zero
       StateGm1_VI    = 0
       CoordXyzPw1_DI = 0
       StateGm2_VI    = 0
       CoordXyzPw2_DI = 0
       iLst1_I = 0
       lEnd1_I = 0
       lPtr1_I = 0

       if(nLinePw2 > 0) then
          ! Southern hemisphere
          allocate( &
               iLst2_I(6*(nPoint2-2)), &
               lPtr2_I(6*(nPoint2-2)), &
               lEnd2_I(nPoint2))
          iLst2_I = 0
          lEnd2_I = 0
          lPtr2_I = 0
       end if
    end if

    if(nLinePw1+nLinePw2 /= nFieldLine .or. nVarPw /= nVar)then
       write(*,*)NameSub,' ERROR: nLinePw=',nLinePw1,'/= nFieldLine=', &
            nFieldLine,' or nVarPw=',nVarPw,' /= nVar=',nVar
       call CON_stop(NameSub,' nFieldLine or nVar has changed')
    end if

    ! Convert to X, Y on a unit sphere (this will be used for triangulation)
    where(Buffer_VI(Theta_,:) > cHalfPi)
       CoordXyzPw2_DI(x_,1:nFieldline) =  &
            sin(Buffer_VI(Theta_,:)) * cos(Buffer_VI(Phi_,:))
       CoordXyzPw2_DI(y_,1:nFieldline) =  &
            sin(Buffer_VI(Theta_,:)) * sin(Buffer_VI(Phi_,:))
       CoordXyzPw2_DI(z_,1:nFieldline) =  &
            cos(Buffer_VI(Theta_,:))
    elsewhere
       CoordXyzPw1_DI(x_,1:nFieldline) =  &
            sin(Buffer_VI(Theta_,:)) * cos(Buffer_VI(Phi_,:))
       CoordXyzPw1_DI(y_,1:nFieldline) =  &
            sin(Buffer_VI(Theta_,:)) * sin(Buffer_VI(Phi_,:))
       CoordXyzPw1_DI(z_,1:nFieldline) =  &
            cos(Buffer_VI(Theta_,:))
    end where

    if(UseMultiIon)then
       do i = iRhoGmFirst, iRhoGmLast
          ! Set values based on if fluid is shared by PW & GM.
          if (iCoupleFluid_I(i)==-1) then ! Fluid NOT in PW.
             where(Buffer_VI(Theta_,:) < cHalfPi) ! Northern Hemisphere.
                StateGm1_VI(i,1:nFieldline) = 1.67E-24 * Si2No_V(UnitRho_)
                StateGm1_VI(i+nIonFluid, 1:nFieldLine) = 0.0
             elsewhere                           ! Southern Hemisphere
                StateGm2_VI(i,1:nFieldline) = 1.67E-24 * Si2No_V(UnitRho_)
                StateGm2_VI(i+nIonFluid, 1:nFieldLine) = 0.0
             end where
          else                          ! Fluid IS in PW.
             where(Buffer_VI(Theta_,:) < cHalfPi) ! Northern Hemisphere.
                StateGm1_VI(i,           1:nFieldLine) = &
                     Buffer_VI(iCoupleFluid_I(i),:) * Si2No_V(UnitRho_)
                StateGm1_VI(i+nIonFluid, 1:nFieldLine) = &
                     Buffer_VI(iCoupleFluid_I(i)+nSpeciesPw,:)*Si2No_V(UnitU_)
             elsewhere                           ! Southern Hemisphere
                StateGm2_VI(i,           1:nFieldLine) = &
                     Buffer_VI(iCoupleFluid_I(i),:) * Si2No_V(UnitRho_)
                StateGm2_VI(i+nIonFluid, 1:nFieldLine) = &
                     Buffer_VI(iCoupleFluid_I(i)+nSpeciesPw,:)*Si2No_V(UnitU_)
             end where
          end if
       end do

       if(DoTestMe) then
          write(*,*)'Max Hp Density:', maxval(Buffer_VI(3, :))
          write(*,*)'Max Op Density:', maxval(Buffer_VI(4, :))
          write(*,*)'Max He Density:', maxval(Buffer_VI(5, :))
          write(*,*)'Max Sw Density:', &
               maxval(StateGm1_VI(1, :))/Si2No_V(UnitRho_)

          write(*,*)'Max Hp Velocity:', maxval(Buffer_VI(6, :))
          write(*,*)'Max Op Velocity:', maxval(Buffer_VI(7, :))
          write(*,*)'Max He Velocity:', maxval(Buffer_VI(8, :))
          write(*,*)'Max Sw Velocity:', &
               maxval(StateGm1_VI(4, :))/Si2No_V(UnitU_)
       end if

    else
       ! Set total
       if(UseMultiSpecies)then
          do i=iRhoGmFirst, iRhoGmLast
             where(Buffer_VI(Theta_,:)< cHalfPi)
                StateGm1_VI(i, 1:nFieldline) &
                     = Buffer_VI(i+iRhoPwFirst-1,:) * Si2No_V(UnitRho_)
             elsewhere
                StateGm2_VI(i, 1:nFieldline) &
                     = Buffer_VI(i+iRhoPwFirst-1,:) * Si2No_V(UnitRho_)
             end where
          end do
       else
          ! Total density in normalized units
          where(Buffer_VI(Theta_,:)< cHalfPi)
             StateGm1_VI(iRhoGmFirst, 1:nFieldline) &
                  = sum(Buffer_VI(iRhoPwFirst:iRhoPwLast,:), DIM=1) &
                  *Si2No_V(UnitRho_)
          elsewhere
             StateGm2_VI(iRhoGmFirst, 1:nFieldline) &
                  = sum(Buffer_VI(iRhoPwFirst:iRhoPwLast,:), DIM=1) &
                  *Si2No_V(UnitRho_)
          end where
       end if

       ! Field aligned velocity = total moment/total density
       where(Buffer_VI(Theta_,:)< cHalfPi)
          StateGm1_VI(iUGmFirst,1:nFieldline) &
               = sum(Buffer_VI(iRhoPwFirst:iRhoPwLast,:) &
               *     Buffer_VI(iUPwFirst:iUPwLast,:), dim=1) &
               / sum(Buffer_VI(iRhoPwFirst:iRhoPwLast,:), dim=1) &
               * Si2No_V(UnitU_)
       elsewhere
          StateGm2_VI(iUGmFirst,1:nFieldline)&
               = sum(Buffer_VI(iRhoPwFirst:iRhoPwLast,:) &
               *     Buffer_VI(iUPwFirst:iUPwLast,:), dim=1) &
               / sum(Buffer_VI(iRhoPwFirst:iRhoPwLast,:), dim=1) &
               * Si2No_V(UnitU_)
       end where
    end if

    ! Reduce densities according to a 1/r3 law based on magnetic field strength
    ! and assuming that PWOM solves up to R=2.25
    ! Factor = (2.25/rBody)**3
    ! StateGm_VI(iRhoGmFirst:iRhoGmLast,1:nLinePw) = &
    !     StateGm_VI(iRhoGmFirst:iRhoGmLast,1:nLinePw)*Factor

    ! Set coordinates for the outer points
    Tmp1_I = cTwoPi
    where(Buffer_VI(Theta_,:)>cHalfPi)
       Tmp1_I = Buffer_VI(Theta_,:)
    end where
    SinThetaOuter2 = sin(minval(Tmp1_I)-dThetaOuter)
    CosThetaOuter2 = cos(minval(Tmp1_I)-dThetaOuter)

    Tmp1_I = 0.0
    where(Buffer_VI(Theta_,:)<cHalfPi)
       Tmp1_I = Buffer_VI(Theta_,:)
    end where
    SinThetaOuter1 = sin(maxval(Tmp1_I)+dThetaOuter)
    CosThetaOuter1 = cos(maxval(Tmp1_I)+dThetaOuter)

    do i = 1, nOuterPoint
       CoordXyzPw1_DI(x_,nLinePw1+i) = &
            SinThetaOuter1 * cos(i*cTwoPi/nOuterPoint)
       CoordXyzPw1_DI(y_,nLinePw1+i) = &
            SinThetaOuter1 * sin(i*cTwoPi/nOuterPoint)
       CoordXyzPw1_DI(z_,nLinePw1+i) = CosThetaOuter1
       if(nLinePw2 /=0)then
          CoordXyzPw2_DI(x_,nLinePw1+nLinePw2+i) = &
               SinThetaOuter2 * cos(i*cTwoPi/nOuterPoint)
          CoordXyzPw2_DI(y_,nLinePw1+nLinePw2+i) = &
               SinThetaOuter2 * sin(i*cTwoPi/nOuterPoint)
          CoordXyzPw2_DI(z_,nLinePw1+nLinePw2+i) = CosThetaOuter2
       end if
    end do

    !  Create the triangulation.
    call trmesh(nPoint1, CoordXyzPw1_DI(x_,1:nPoint1), &
         CoordXyzPW1_DI(y_,1:nPoint1), CoordXyzPW1_DI(z_,1:nPoint1), &
         iLst1_I, lPtr1_I, lEnd1_I, iError )
    if (iError == -2) then
       write(*,*)NameSub, &
            ' WARNING: Error in TRMESH, First three nodes are collinear'
       call CON_stop(NameSub,' Problem With Triangulation')
    else if (iError > 0) then
       write(*,*)NameSub, &
            ' ERROR: Error in TRMESH, Duplicate nodes encountered'
       call CON_stop(NameSub,' Problem With Triangulation')
    end if

    if(nLinePw2 > 0)then
       call trmesh(nPoint2, CoordXyzPw2_DI(x_,nLinePw1+1:nLinePw1+nPoint2), &
            CoordXyzPW2_DI(y_,nLinePw1+1:nLinePw1+nPoint2), &
            CoordXyzPW2_DI(z_,nLinePw1+1:nLinePw1+nPoint2), &
            iLst2_I, lPtr2_I, lEnd2_I, iError )
       if ( iError == -2 ) then
          write(*,*)NameSub, &
               ' WARNING: Error in TRMESH, First three nodes are collinear'
          call CON_stop(NameSub,' Problem With Triangulation')
       else if ( iError > 0 ) then
          write(*,*)NameSub, &
               ' ERROR: Error in TRMESH, Duplicate nodes encountered'
          call CON_stop(NameSub,' Problem With Triangulation')
       end if
    end if

    if(DoTestMe)then
       write(*,*)NameSub,': nVarPw, nLinePw, nSpeciesPw=',&
            nVarPw, nLinePw2, nSpeciesPw
       write(*,*)NameSub,': nPoint2, nTriangle2=',nPoint2, nTriangle2
       write(*,*)NameSub,': CoordXyzPw_DI, Southern Hemi:'
       do i=nLinePw1+1,nLinePw1+nPoint2
          write(*,*) i-nLinePw1, CoordXyzPw2_DI(:,i)
       end do
       write(*,*)NameSub,': CoordXyzPw_DI, Northern Hemi:'
       do i=1,nPoint1
          write(*,*) i, CoordXyzPw1_DI(:,i)
       end do

       write(*,*) NameSub,': Writing plot of triangulation'
       ! Northern Hemi
       call open_file(FILE='TestTriangulation_North.eps')
       call trplot(UnitTmp_, 7.5, 90.0, 0.0, 90.0, nPoint1, &
            CoordXyzPw1_DI(x_,1:nPoint1), &
            CoordXyzPw1_DI(y_,1:nPoint1), &
            CoordXyzPw1_DI(z_,1:nPoint1), iLst1_I, lPtr1_I, &
            lEnd1_I, 'test1 triangulation',.true., iError )
       call close_file
       if(nLinePw2 > 0)then
          ! Southern Hemi
          call open_file(FILE='TestTriangulation_South.eps')
          call trplot(UnitTmp_, 7.5, -90.0, 0.0, 90.0, nPoint2, &
               CoordXyzPw2_DI(x_,nLinePw1+1:nLinePw1+nPoint2),    &
               CoordXyzPw2_DI(y_,nLinePw1+1:nLinePw1+nPoint2),    &
               CoordXyzPw2_DI(z_,nLinePw1+1:nLinePw1+nPoint2),    &
               iLst2_I, lPtr2_I, lEnd2_I, 'test1 triangulation',  &
               .true., iError )
          call close_file
       end if
    end if

  end subroutine GM_put_from_pw
  !============================================================================
 end module GM_couple_pw
!==============================================================================
