!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf

module ModWriteProgress

  use BATL_lib, ONLY: test_start, test_stop
#ifdef _OPENACC
  use ModUtilities, ONLY: norm2
#endif

  implicit none

  private ! except

  public:: write_progress ! write initial and run time information to screen
  public:: write_runtime_values ! write session parameters to screen
  public:: write_timeaccurate   ! write info about simulation/wall clock time

contains
  !============================================================================
  subroutine write_progress

    use BATL_lib, ONLY: iProc, nProc, nThread
    use ModMain
    use ModIO, ONLY: iUnitOut, write_prefix
    use ModVarIndexes, ONLY: NameEquation, NameEquationFile
    use ModUserInterface ! set_user_version

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_progress'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if (iProc /= 0 .or. lVerbose<=0) RETURN

    write(iUnitOut,*)
    call write_prefix; write(iUnitOut,'(a)') &
         " BATSRUS: Block Adaptive Tree Solar-Wind Roe Upwind Scheme"
    call write_prefix; write(iUnitOut,'(a)') &
         "          for 3D Heliospheric Flows"
    call write_prefix; write(iUnitOut,'(a)') &
         " University of Michigan, 1995-2017"
    write(iUnitOut,*)
    if(IsStandAlone)then
       call write_prefix; write(iUnitOut,'(a,i6,a,i3,a)') &
            ' BATSRUS is running as '//NameThisComp//' on ', nProc, &
            ' PE(s) with up to', nThread, ' threads/PE'
       write(iUnitOut,*)
    end if
    call write_prefix; write(iUnitOut,'(a)') &
         ' EQUATIONS:     '//NameEquation
    call write_prefix; write(iUnitOut,'(a)') &
         ' EQUATION FILE: '//NameEquationFile
    call set_user_version
    call write_prefix; write(iUnitOut,'(a)') &
         ' USER MODULE:   '//trim(NameUserModule)
    call write_prefix; write(iUnitOut,'(a)') &
         ' USER FILE:     '//trim(NameUserFile)
    write(iUnitOut,*)

    call test_stop(NameSub, DoTest)
  end subroutine write_progress
  !============================================================================
  subroutine write_runtime_values

    use ModMain
    use ModPhysics
    use ModBorisCorrection, ONLY: UseBorisCorrection, UseBorisSimple
    use ModIO, ONLY: iUnitOut, write_prefix
    use ModFaceValue, ONLY: TypeLimiter, LimiterBeta
    use ModAdvance, ONLY: TypeFlux, UseEfield, iTypeUpdate
    use ModGeometry, ONLY: &
         xMinBox, xMaxBox, yMinBox, yMaxBox, zMinBox, zMaxBox, &
         CellSizeMin, CellSizeMax, nUsedCell, count_true_cells
    use ModImplicit, ONLY: UseImplicit, UseSemiImplicit, &
         UseSplitSemiImplicit, TypeSemiImplicit
    use ModPointImplicit, ONLY: UsePointImplicit
    use ModMultiFluid, ONLY: UseNeutralFluid
    use ModFaceFlux, ONLY: TypeFluxNeutral
    use CON_planet, ONLY: NamePlanet, IsPlanetModified, &
         Planet_, NewPlanet_, &
         RadiusPlanet, MassPlanet, TiltRotation, OmegaPlanet, OmegaOrbit, &
         IonosphereHeight
    use ModIonElectron, ONLY: iVarUseCmax_I
    use ModMpi
    use BATL_lib, ONLY: iProc, nProc, nThread, nNodeUsed, nRoot_D, nIJK_D, &
         nIJK, nLevelMin, nLevelMax, IsLogRadius, IsGenRadius
    use ModUserInterface ! user_action

    integer :: iFluid
    character(len=100):: String, StringFormat

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'write_runtime_values'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    ! Count number of true cells
    call count_true_cells

    if (iProc /= 0 .or. lVerbose <= 0) RETURN

    call write_prefix; write(iUnitOut,*)
    call write_prefix; write(iUnitOut,*)'   Begin Numerical Simulation'
    call write_prefix; write(iUnitOut,*)'   --------------------------'
    call write_prefix; write(iUnitOut,*)
    call write_prefix; write(iUnitOut,*)
    if (NamePlanet /= 'NONE' .and. UseBody .and. NameThisComp == 'GM') then
       call write_prefix; write(iUnitOut,*)'   Planetary Parameters'
       call write_prefix; write(iUnitOut,*)'   --------------------'
       call write_prefix; write(iUnitOut,*)
       String = ''
       if (IsPlanetModified .and. (Planet_ /= NewPlanet_)) &
            String = '( default values were modified! )'
       call write_prefix; write(iUnitOUT,'(10X,A,A,2x,A)')  &
            'Name:           ', trim(NamePlanet), trim(String)
       StringFormat = '(10X,A16,ES13.5)'
       call write_prefix; write(iUnitOUT,StringFormat) &
            'Radius:         ', RadiusPlanet
       call write_prefix; write(iUnitOUT,StringFormat) &
            'Mass:           ', MassPlanet
       String = ' Not Rotating'
       if (OmegaPlanet /= 0.0) write(String, '(ES13.5)') cTwoPi/OmegaPlanet
       call write_prefix; write(iUnitOUT,'(10X,A,A)')'Rotation Period:', &
            trim(String)
       call write_prefix; write(iUnitOUT,StringFormat) &
            'Rot. Tilt [deg]:', TiltRotation*cRadToDeg
       String = ' Not Orbiting'
       if (OmegaOrbit /= 0.0) write(String, '(ES13.5)') cTwoPi/OmegaOrbit
       call write_prefix; write(iUnitOUT,'(10X,A,A)')'Orbit Period:   ', &
            trim(String)
       call write_prefix; write(iUnitOUT,StringFormat) &
            'Iono Height:    ', IonosphereHeight
       call write_prefix; write(iUnitOut,*)
    end if
    call write_prefix; write(iUnitOut,*) '   Physical Parameters'
    call write_prefix; write(iUnitOut,*) '   -------------------'
    call write_prefix; write(iUnitOut,*)
    call write_prefix; write(iUnitOUT,'(10X,a)') &
         'I/O Unit type: '//trim(TypeIoUnit)//'            '// &
         'Normalization: '//trim(TypeNormalization)
    call write_prefix; write(iUnitOut,*)
    call write_prefix; write(iUnitOut,'(10X,a,10(f12.8))') &
         'Gamma:       ',Gamma_I
    call write_prefix; write(iUnitOut,*)
    if(UseBody)then
       call write_prefix; write(iUnitOut,'(10X,2(A13,ES13.5))') &
            'rBody:       ', rBody,      ', rPlanet:   ',rPlanetSi
       do iFluid = 1, nFluid
          call write_prefix; write(iUnitOut,'(10X,2(A13,ES13.5))') &
               'BodyNDim:    ',BodyNDim_I(iFluid), &
               ', BodyTDim:  ',BodyTDim_I(iFluid)
       end do
       call write_prefix; write(iUnitOut,'(10X,2(A13,ES13.5))') &
            'BdpDim:      ', Bdp*No2Io_V(UnitB_),', Tilt [deg]:', &
            ThetaTilt*cRadToDeg
       if(UseRotatingBc)then
          call write_prefix; write(iUnitOut,'(10X,a)') 'Corotation is used'
       end if
    else
       call write_prefix; write(iUnitOut,'(10X,''UseBody1: .false.'')')
    end if
    if(UseGravity)then
       call write_prefix; write(iUnitOut,'(10X,a,ES13.5)') &
            'Gravity is used, gBody=', gBody
    end if
    call write_prefix; write(iUnitOut,*)

    if (UseBody2) then
       call write_prefix
       write(iUnitOut,'(10X,''UseBody2: .true.'')')
       call write_prefix; write(iUnitOut,'(10X,2(A13,ES13.5))') &
            'rBody2:      ',RBody2   ,', BdpDimBody2: ',&
            norm2(BdpDimBody2_D)
       call write_prefix; write(iUnitOut,'(10X,2(A13,ES13.5))') &
            'xBody2:      ',xBody2   ,', Bdp2Body_x:    ',BdpBody2_D(1)
       call write_prefix; write(iUnitOut,'(10X,2(A13,ES13.5))') &
            'yBody2:      ',yBody2   ,', Bdp2Body_y:    ',BdpBody2_D(2)
       call write_prefix; write(iUnitOut,'(10X,2(A13,ES13.5))') &
            'zBody2:      ',zBody2   ,', Bdp2Body_z:    ',BdpBody2_D(3)
       call write_prefix
       write(iUnitOut,'(10X,2(A13,ES13.5))') &
            'Body2NDim: ',Body2NDim,', Body2TDim:   ',Body2TDim
       if (UseBody2Orbit) then
          call write_prefix; write(iUnitOut,'(10X,''UseBody2Orbit: .true.'')')
          call write_prefix; write(iUnitOut,'(10X,2(A13,ES13.5))') &
               'OrbitPeriod: ', cTwoPi/OmegaOrbit
       endif
    else
       call write_prefix
       write(iUnitOut,'(10X,''UseBody2: .false.'')')
    end if

    call write_prefix; write(iUnitOut,*)
    call write_prefix; write(iUnitOut,'(10X,2(A13,ES13.5))')&
         'ClightFactor:', ClightFactor,', Clight:    ',Clight
    call write_prefix; write(iUnitOut,*)

    if(NameThisComp == 'GM' .and. SolarWindN > 0.0)then
       call write_prefix; write(iUnitOut,*)
       StringFormat = '(10X,A17,"]: ",es15.6,A10,es15.6)'
       call write_prefix; write(iUnitOut,StringFormat) &
            'SolarWindNDim   ['//NameIdlUnit_V(UnitN_), &
            SolarWindNDim,  ', SolarWindN:  ', SolarWindN
       call write_prefix; write(iUnitOut,StringFormat) &
            'SolarWindRhoDim ['//NameIdlUnit_V(UnitRho_), &
            SolarWindRhoDim,', SolarWindRho:', SolarWindRho
       call write_prefix; write(iUnitOut,StringFormat) &
            'SolarWindUxDim  ['//NameIdlUnit_V(UnitU_), &
            SolarWindUxDim, ',  SolarWindUx: ', SolarWindUx
       call write_prefix; write(iUnitOut,StringFormat) &
            'SolarWindUyDim  ['//NameIdlUnit_V(UnitU_), &
            SolarWindUyDim, ',  SolarWindUy: ',SolarWindUy
       call write_prefix; write(iUnitOut,StringFormat) &
            'SolarWindUzDim  ['//NameIdlUnit_V(UnitU_), &
            SolarWindUzDim, ',  SolarWindUz: ',SolarWindUz
       call write_prefix; write(iUnitOut,StringFormat) &
            'SolarWindPDim   ['//NameIdlUnit_V(UnitP_), &
            SolarWindPDim,  ',  SolarWindP:  ',SolarWindP
       call write_prefix; write(iUnitOut,StringFormat) &
            'SolarWindBxDim  ['//NameIdlUnit_V(UnitB_), &
            SolarWindBxDim, ',  SolarWindBx: ',SolarWindBx
       call write_prefix; write(iUnitOut,StringFormat) &
            'SolarWindByDim  ['//NameIdlUnit_V(UnitB_), &
            SolarWindByDim, ',  SolarWindBy: ',SolarWindBy
       call write_prefix; write(iUnitOut,StringFormat) &
            'SolarWindBzDim  ['//NameIdlUnit_V(UnitB_), &
            SolarWindBzDim, ',  SolarWindBz: ',SolarWindBz
       StringFormat = '(10X,A17,"]: ",F15.6)'
       call write_prefix; write(iUnitOut,StringFormat) &
            'SolarWindTempDim   ['//NameIdlUnit_V(UnitTemperature_), &
            SolarWindTempDim
    end if
    call write_prefix; write(iUnitOut,*)
    call write_prefix; write(iUnitOut,*)'   MHD Numerical Solution Parameters'
    call write_prefix; write(iUnitOut,*)'   ---------------------------------'
    call write_prefix; write(iUnitOut,*)

    if(iTypeUpdate > 1)then
       call write_prefix
       write(iUnitOut,'(10X,a,i1)') 'iTypeUpdate=', iTypeUpdate
    end if

    call write_prefix
    select case (nOrder)
    case (1)
       write(iUnitOut,'(10X,a)') '1st-order scheme'
    case (2)
       write(iUnitOut,'(10X,a)') '2nd-order scheme with '//trim(TypeLimiter)//&
            ' limiter'
    case (4)
       write(iUnitOut,'(10X,a)') '4th-order scheme with '//trim(TypeLimiter)//&
            ' limiter'
    case (5)
       write(iUnitOut,'(10X,a)') '5th-order scheme with '//trim(TypeLimiter)//&
            ' limiter'
    end select
    if(nOrder > 1 .and. TypeLimiter /= 'minmod') then
       call write_prefix;
       write(iUnitOut,'(10x,a,f5.2)') '    LimiterBeta =', LimiterBeta
    end if

    call write_prefix
    write(iUnitOut,'(10X,a,a)') trim(TypeFlux),' flux function'
    if(UseNeutralFluid)then
       call write_prefix
       write(iUnitOut,'(10X,a,a)') trim(TypeFluxNeutral), &
            ' flux for neutral fluids'
    end if

    call write_prefix
    if (IsTimeAccurate) then
       write(iUnitOut,'(10X,a)') 'Time accurate calculation'
    else
       write(iUnitOut,'(10X,a)') 'Steady state calculation'
    end if

    call write_prefix
    if(.not.UseImplicit .and. .not.UseSemiImplicit) &
         write(iUnitOut,'(10X,a)') 'Explicit time stepping'
    if (UseImplicit) write(iUnitOut,'(10X,a)') 'Implicit time stepping'
    if (UseSemiImplicit)then
       if(UseSplitSemiImplicit)then
          write(iUnitOut,'(10X,a)') 'Split semi-implicit time stepping for '//&
               trim(TypeSemiImplicit)
       else
          write(iUnitOut,'(10X,a)') 'Semi-implicit time stepping for '// &
               trim(TypeSemiImplicit)
       end if
    endif
    if(UsePointImplicit)then
       call write_prefix
       write(iUnitOut,'(10X,a)') 'Point-implicit time stepping'
    end if

    call write_prefix; write(iUnitOut,'(10x,a,i1)')   '    nStage: ', nStage
    call write_prefix
    if(UseDtFixed)then
       write(iUnitOut,*) '             Dt:    ', Dt
    else
       write(iUnitOut,'(10x,a,f4.2)') '    Cfl:    ', Cfl
    end if
    if(UseBorisCorrection)then
       call write_prefix
       write(iUnitOut,'(10X,a,f10.4)') &
            "    with Boris correction, factor =", ClightFactor
    end if
    if(UseBorisSimple)then
       call write_prefix
       write(iUnitOut,'(10X,a,f10.4)') &
            "   with simple Boris correction, factor =", ClightFactor
    end if
    if(UseEfield)then
       call write_prefix
       write(iUnitOut,'(10x,100a)') &
            "UseEfield, vars that diffuse with Cmax: ",NameVar_V(iVarUseCmax_I)
    end if

    call write_prefix; write(iUnitOut,*)
    call write_prefix; write(iUnitOut,*)'   Other Runtime Parameters'
    call write_prefix; write(iUnitOut,*)'   ------------------------'
    call write_prefix; write(iUnitOut,*)
    call write_prefix; write(iUnitOut,*)'Available processors: nProc = ',nProc
    call write_prefix; write(iUnitOut,*)'Available threads   : nThread = ', &
         nThread
    call write_prefix; write(iUnitOut,*)
    call write_prefix; write(iUnitOut,*)'After initial grid setup:'
    call write_prefix; write(iUnitOut,*)'  nBlockMax and MaxBlock      = ', &
         nBlockMax, MaxBlock
    call write_prefix; write(iUnitOut,*)'  Total number of blocks used = ', &
         nNodeUsed
    call write_prefix; write(iUnitOut,*)'  Total number of cells       = ', &
         nNodeUsed*nIJK
    call write_prefix; write(iUnitOut,*)'  Total number of true cells  = ', &
         nUsedCell
    call write_prefix; write(iUnitOut,*)'  Min and max AMR levels      = ', &
         nLevelMin, nLevelMax
    if(IsLogRadius .or. IsGenRadius)then
       call write_prefix; write(iUnitOut,*)'  Min and max cell size in Phi= ',&
            CellSizeMin, CellSizeMax
    else
       call write_prefix; write(iUnitOut,*)'  Min and max cell size in x/r= ',&
            CellSizeMin, CellSizeMax
    endif

    ! Constrained transport is not implemented for AMR grids anymore...
    if(UseConstrainB .and. nLevelMin /= nLevelMax) &
         call stop_mpi('Constrained transport works on uniform grid only!')

    call write_prefix; write(iUnitOut,*)
    call write_prefix
    write(iUnitOut,'(1x,a,3i8)')    'root blocks: ', nRoot_D
    call write_prefix
    write(iUnitOut,'(1x,a,3i8)')    'nIJK_D:      ', nIJK_D
    call write_prefix
    write(iUnitOut,'(1x,a,2es16.8)') 'x:           ', xMinBox, xMaxBox
    call write_prefix
    write(iUnitOut,'(1x,a,2es16.8)') 'y:           ', yMinBox, yMaxBox
    call write_prefix
    write(iUnitOut,'(1x,a,2es16.8)') 'z:           ', zMinBox, zMaxBox
    call write_prefix; write(iUnitOut,*)

    if(UseUserWriteProgress) call user_action('write progress')

    call test_stop(NameSub, DoTest)
  end subroutine write_runtime_values
  !============================================================================
  subroutine write_timeaccurate

    use ModMain, ONLY: tSimulation
    !--------------------------------------------------------------------------
    write(*, '(a,e13.5,a,f12.6,a,f12.6,a)') &
         '   Simulated Time T = ',tSimulation, &
         ' (',tSimulation/60.00, &
         ' min, ',tSimulation/3600.00,' hrs)'

  end subroutine write_timeaccurate
  !============================================================================
end module ModWriteProgress
!==============================================================================
