!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

subroutine read_pw_buffer(CoordIn_D, nVarIn, State_V)

  ! For the location CoordIn_D of the inner face boundary of GM
  ! return the primitive State_V (densities and velocities)
  ! in normalized units.
  ! Interpolation is performed with spherical triangulation.
  ! Either use northern hemisphere only, or both hemispheres if available.
  ! If CoordIn_D is not covered by the triangulation then
  ! State_V (intent inout) is not modified.

  use GM_couple_pw
  use CON_coupler, ONLY: PW_, Grid_C
  use CON_axes, ONLY: transform_matrix
  use ModMain, ONLY: TypeCoordSystem, tSimulation
  use ModVarIndexes, ONLY: Rho_, Ux_, Uy_, Uz_, &
       SpeciesFirst_, SpeciesLast_, nIonFluid
  use ModAdvance, ONLY: UseMultiSpecies, nSpecies
  use ModMultiFluid, ONLY: UseMultiIon, nIonFluid, &
       iRhoIon_I, iUxIon_I, iUyIon_I, iUzIon_I
  use ModTriangulateSpherical, ONLY: find_triangle_sph
  use ModPhysics, ONLY: No2Io_V, UnitU_, UnitRho_, PolarRhoMin_I
  use ModB0, ONLY: get_b0
  use CON_planet_field, ONLY: map_planet_field

  implicit none

  real, intent(in)    :: CoordIn_D(3)
  integer, intent(in) :: nVarIn
  real, intent(inout) :: State_V(nVarIn)

  logical    :: IsTriangleFound
  integer    :: iNode1, iNode2, iNode3, iIon, iUGm
  real       :: Area1, Area2, Area3

  ! A short time relative to the rotation period of Earth
  real, parameter :: dTimeMax = 600.0 ! [s]
  real    :: B0_D(3), XyzPw_D(3), Xyz_D(3), XyzTmp_D(3)
  integer :: iPoint, iHemisphere
  logical :: DoInitialize = .true.

  real    :: TimeSimLast = -1.0 - dTimeMax
  real, save:: PwGm_DD(3,3)

  logical :: DoTest, DoTestMe
  character(len=*), parameter:: NameSub = 'read_pw_buffer'
  !----------------------------------------------------------------------------
  call CON_set_do_test(NameSub, DoTest, DoTestMe)

  if(DoTestMe) write(*,*) NameSub,': initial State_V=', State_V

  if(DoInitialize)then
     DoInitialize = .false.
     ! Fill in outer points with body values coming in via State_V
     ! For each hemisphere, outer points are after PW points.
     do iPoint = nLinePw1 + 1, nPoint1
        StateGm1_VI(iUGmFirst:iUGmLast, iPoint) = 0.0
        if(UseMultiSpecies)then
           StateGm1_VI(iRhoGmFirst:iRhoGmLast, iPoint) = &
                State_V(SpeciesFirst_:SpeciesLast_)
        elseif(UseMultiIon)then
           StateGm1_VI(iRhoGmFirst:iRhoGmLast, iPoint) = State_V(iRhoIon_I)
        else
           StateGm1_VI(iRhoGmFirst, iPoint) = State_V(Rho_)
        end if
     end do

     if (nLinePw2 /=0) then
        do iPoint = nLinePw + 1, nLinePw + nOuterPoint
           StateGm2_VI(iUGmFirst:iUGmLast, iPoint) = 0.0
           if(UseMultiSpecies)then
              StateGm2_VI(iRhoGmFirst:iRhoGmLast, iPoint) = &
                   State_V(SpeciesFirst_:SpeciesLast_)
           elseif(UseMultiIon)then
              StateGm2_VI(iRhoGmFirst:iRhoGmLast, iPoint) = State_V(iRhoIon_I)
           else
              StateGm2_VI(iRhoGmFirst, iPoint) = State_V(Rho_)
           end if
        end do
     end if

     NamePwCoord = Grid_C(PW_) % TypeCoord

     if(DoTestMe)then
        write(*,*) NameSub, &
             ': UseMultiIon, UseMultiSpecies, iRhoGmFirst, iRhoGmLast=', &
             UseMultiIon, UseMultiSpecies, iRhoGmFirst, iRhoGmLast
        write(*,*) NameSub, &
             ': nLinePw1, nLinePw2, nLinePw, nOuterPoint=', &
             nLinePw1, nLinePw2, nLinePw, nOuterPoint
     end if
  end if

  if(TypeCoordSystem == NamePwCoord)then
     XyzPw_D = CoordIn_D
  else
     if(abs(tSimulation - TimeSimLast) > dTimeMax)then
        ! Update GM to PW transformation if necessary
        PwGm_DD = &
             transform_matrix(tSimulation, TypeCoordSystem, NamePwCoord)
        TimeSimLast = tSimulation
     end if
     ! Convert from GM coordinates to buffer coordinates
     XyzPw_D = matmul( PwGm_DD, CoordIn_D)
  end if

  ! Map down to radius 1.0 where the PW fieldline positions are defined
  call map_planet_field(tSimulation, XyzPw_D, 'SMG NORM', &
       1.0, Xyz_D, iHemisphere)

  ! Find triangle containing point Xyz_D
  Area1=0
  Area2=0
  Area3=0

  !  write(*,*) 'Calling find_triangle_sph with Xyz_D =', Xyz_D
  !  write(*,*) 'nLinePw2', nLinePw2
  if(Xyz_D(3) < 0 .and. nLinePw2 > 0)then
     ! Southern hemisphere (z < 0) with southern PW field lines (nLinePw2 > 0)
     call find_triangle_sph(Xyz_D, nPoint2, &
          CoordXyzPw2_DI(:,nLinePw1+1:nLinePw1+nPoint2), &
          iLst2_I, lPtr2_I, lEnd2_I, Area1, Area2, Area3, IsTriangleFound, &
          iNode1,iNode2,iNode3)
  else
     ! Get weights if in northern hemisphere.
     ! If point is in southern, but no PwLines in south, then flip sign of z
     XyzTmp_D(1) = Xyz_D(1)
     XyzTmp_D(2) = Xyz_D(2)
     XyzTmp_D(3) = abs(Xyz_D(3))
     call find_triangle_sph(XyzTmp_D, nPoint1, &
          CoordXyzPw1_DI(:,1:nPoint1), &
          iLst1_I, lPtr1_I, lEnd1_I, Area1, Area2, Area3, IsTriangleFound,&
          iNode1,iNode2,iNode3)
  end if

  !  write(*,*) '! Point X, Y, Z =', Xyz_D, 'IsTriangleFound=', IsTriangleFound
  !  write(*,*) '! Areas:', Area1, Area2,Area3
  !  write(*,*) '! sum area=', Area1+Area2+Area3
  !  write(*,*) '! Test Position Vector Length', sqrt(sum(Xyz_D(:)**2))
  !  write(*,*) '  '

  !  call con_stop('')

  ! Point is not covered: leave the input state variables alone
  if (.not.IsTriangleFound) then
     if(DoTestMe) write(*,*) NameSub, &
          ': triangle not found CoordIn_D=', CoordIn_D, &
          ' XyzPw_D=', XyzPw_D, ' Xyz_D=', Xyz_D

     RETURN
  end if

  ! Calculate field aligned momentum vector
  call get_b0(CoordIn_D, B0_D)
  B0_D = B0_D / sqrt(sum(B0_D**2))

  ! Make sure unit vector is pointing outward
  if(sum(B0_D*CoordIn_D) < 0.)B0_D = -B0_D

  ! Put into a temporary array StateGm_VI
  if(Xyz_D(3) < 0 .and. nLinePw2 /=0) then
     allocate(StateGm_VI(1:iUGmLast, nPoint2))
     ! Northern polar cap
     StateGm_VI = StateGm2_VI(:,nLinePw1+1:nLinePw1+nPoint2)
  else
     allocate(StateGm_VI(1:iUGmLast, nPoint1))
     ! Southern polar cap
     StateGm_VI = StateGm1_VI(:,1:nPoint1)
  end if

  ! interpolate values
  if(UseMultiIon)then
     State_V(iRhoIon_I) = max(PolarRhoMin_I(1:nIonFluid), &
          ( Area1*StateGm_VI(iRhoGmFirst:iRhoGmLast,iNode1) &
          + Area2*StateGm_VI(iRhoGmFirst:iRhoGmLast,iNode2) &
          + Area3*StateGm_VI(iRhoGmFirst:iRhoGmLast,iNode3) ) )
     do iIon = 1, nIonFluid
        ! Calculate field aligned velocity vector per fluid
        iUGm = iUGmFirst + iIon - 1
        State_V(iUxIon_I(iIon):iUzIon_I(iIon)) = &
             State_V(iUxIon_I(iIon):iUzIon_I(iIon)) + 2*B0_D * &
             ( Area1*StateGm_VI(iUGm, iNode1) &
             + Area2*StateGm_VI(iUGm, iNode2) &
             + Area3*StateGm_VI(iUGm, iNode3))
     end do
  else
     if(UseMultiSpecies) then
        ! Interpolate species densities and put total into Rho_
        State_V(SpeciesFirst_:SpeciesLast_) = &
             max(PolarRhoMin_I(1:nSpecies), &
             ( Area1*StateGm_VI(iRhoGmFirst:iRhoGmLast,iNode1) &
             + Area2*StateGm_VI(iRhoGmFirst:iRhoGmLast,iNode2) &
             + Area3*StateGm_VI(iRhoGmFirst:iRhoGmLast,iNode3) ) )

        State_V(Rho_) = sum(State_V(SpeciesFirst_:SpeciesLast_))
     else
        ! For simple MHD the StateGm_VI contains the total density
        State_V(Rho_) = max(PolarRhoMin_I(1), &
             ( Area1*StateGm_VI(iRhoGmFirst,iNode1) &
             + Area2*StateGm_VI(iRhoGmFirst,iNode2) &
             + Area3*StateGm_VI(iRhoGmFirst,iNode3) ) )
     end if

     ! Add field aligned velocity to set (True+Ghost)/2.
     State_V(Ux_:Uz_) = State_V(Ux_:Uz_) + 2*B0_D * ( &
          Area1*StateGm_VI(iUGmFirst,iNode1) + &
          Area2*StateGm_VI(iUGmFirst,iNode2) + &
          Area3*StateGm_VI(iUGmFirst,iNode3))

  end if

  if(DoTestMe)then
     write(*,*)NameSub,' finished with'
     write(*,*)'CoordIn_D     =', CoordIn_D
     write(*,*)'XyzPw_D       =', XyzPw_D
     write(*,*)'XyzPw_D       =', Xyz_D
     write(*,*)'Area1,2,3     =', Area1, Area2, Area3
     write(*,*)'sum(Area1,2,3)=', Area1 + Area2 + Area3
     write(*,*)'State_V(Rho_) =', State_V(Rho_)*No2Io_V(UnitRho_)
     write(*,*)'B0_D          =', B0_D
     write(*,*)'State_V(Ux:Uz)=', State_V(Ux_:Uz_)*No2Io_V(UnitU_)
     write(*,*)'Density at iNode1, iNode2, and iNode3=', &
          StateGm_VI(iRhoGmFirst, iNode1)*No2Io_V(UnitRho_), &
          StateGm_VI(iRhoGmFirst, iNode2)*No2Io_V(UnitRho_),&
          StateGm_VI(iRhoGmFirst, iNode3)*No2Io_V(UnitRho_)

     write(*,*)'iNode1, iNode2, iNode3 = ', iNode1, iNode2, iNode3
  end if

  deallocate(StateGm_VI)

end subroutine read_pw_buffer
!==============================================================================
