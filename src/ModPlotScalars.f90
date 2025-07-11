!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModPlotScalars

  use BATL_lib, ONLY: &
       test_start, test_stop, iProc, nProc, iComm
  use ModBatsrusUtility, ONLY: stop_mpi

  use ModIO
  use ModNumConst,  ONLY: cRadtoDeg, cDegToRad

  implicit none

  SAVE

  private   ! except
  public:: set_plot_scalars
contains
    subroutine set_plot_scalars(iFile, MaxParam, nParam, NameParam_I, Param_I)

    ! For file iPlotFile set Param_I based on NameParam_I
    ! Extend array of scalar parameters with useful information

    use ModPhysics, ONLY: Gamma, Gamma_I, GammaElectron, &
         cLight, rBody, ThetaTilt, &
         No2Io_V, No2Si_V, Io2Si_V, UnitX_, UnitT_, UnitU_, UnitRho_
    use ModFieldTrace, ONLY: rTrace
    use ModResistivity, ONLY: Eta0Si
    use ModIO
    use ModMain, ONLY: Dt
    use ModMultiFluid, ONLY: nFluid, nIonFluid,MassFluid_I, ChargeIon_I
    use ModSatelliteFile, ONLY: nSatellite, NameSat_I, i_sat_for_name, &
         set_satellite_positions, XyzSat_DI
    use BATL_lib, ONLY: nRoot_D, nI, nJ, nK
    use ModUtilities, ONLY: split_string, lower_case

    integer,           intent(in):: iFile
    integer,           intent(in):: MaxParam
    integer,           intent(out):: nParam
    character(len=10), intent(out):: NameParam_I(MaxParam)
    real,              intent(out):: Param_I(MaxParam)

    character(len=500):: NameParam
    integer:: iPar, iSat, iDim
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'set_plot_scalars'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)
    NameParam = StringPlotParam_I(iFile)

    call lower_case(NameParam)

    ! Check if satellite Xyz are present among paramaters
    do iSat = 1, nSatellite
       if(index(NameParam, '_'//trim(NameSat_I(iSat))) > 0)&
            call set_satellite_positions(iSat)
    end do

    call split_string(NameParam, MaxParam, NameParam_I, nParam, &
         UseArraySyntaxIn=.true.)

    do iPar = 1, nParam
       if(index(NameParam_I(iPar),'_') == 2)then
          ! satellite coords x_satname/y_satname/z_satname
          select case(NameParam_I(iPar)(1:2))
          case('x_')
             iDim = 1
          case('y_')
             iDim = 2
          case('z_')
             iDim = 3
          case default
             Param_I(iPar) = -7777.
             if(iProc == 0)write(*,*) NameSub// &
                  ':error: unknown parameter name=', &
                  trim(NameParam_I(iPar)), ' for iFile=', iFile
          end select
          iSat = i_sat_for_name(NameParam_I(iPar)(3:))
          if(iSat > 0)then
             Param_I(iPar) = XyzSat_DI(iDim,iSat)
          else
             Param_I(iPar) = -7777.
             if(iProc == 0) write(*,*) NameSub// &
                  ':error: unknown satellite=', &
                  NameParam_I(iPar)(3:), ' for iFile=', iFile
          end if
          if(DoTest)then
             write(*,*) NameSub,': NameParam=',trim(NameParam_I(iPar))
             write(*,*) 'iDim, iSat, Xyz=', iDim, iSat, XyzSat_DI(:,iSat)
             write(*,*) 'iPar=', iPar, ' Param_I(iPar)=', Param_I(iPar)
          end if
          CYCLE
       end if
       select case(NameParam_I(iPar))
       case('g', 'g1', 'gamma')
          Param_I(iPar) = Gamma
       case('g2')
          Param_I(iPar) = Gamma_I(min(2, nFluid))
       case('g3')
          Param_I(iPar) = Gamma_I(min(3, nFluid))
       case('g4')
          Param_I(iPar) = Gamma_I(min(4, nFluid))
       case('g5')
          Param_I(iPar) = Gamma_I(min(5, nFluid))
       case('g6')
          Param_I(iPar) = Gamma_I(min(6, nFluid))
       case('g7')
          Param_I(iPar) = Gamma_I(min(7, nFluid))
       case('g8')
          Param_I(iPar) = Gamma_I(min(8, nFluid))
       case('g9')
          Param_I(iPar) = Gamma_I(min(9, nFluid))
       case('ge')
          Param_I(iPar) = GammaElectron
       case('c','clight')
          if(IsDimensionalPlot_I(iFile)) then
             Param_I(iPar) = Clight*No2Io_V(UnitU_)
          else
             Param_I(iPar) = Clight
          end if
       case('r','rbody')
          Param_I(iPar) = rBody
          if(IsDimensionalPlot_I(iFile)) &
               Param_I(iPar) = Param_I(iPar)*No2Io_V(UnitX_)
          ! BEGIN CCMC REQUESTED PARAMETERS to describe block structure
       case('p1')
          Param_I(iPar) = nRoot_D(1)
       case('p2')
          Param_I(iPar) = nRoot_D(2)
       case('p3')
          Param_I(iPar) = nRoot_D(3)
       case('nx')
          Param_I(iPar) = nI
       case('ny')
          Param_I(iPar) = nJ
       case('nz')
          Param_I(iPar) = nK
       case('th')
          ! CCMC needs the dipole tilt in radians
          Param_I(iPar) = ThetaTilt
          ! END OF CCMC requested parameters
       case('tilt')
          Param_I(iPar) = ThetaTilt*cRadToDeg
       case('eta')
          Param_I(iPar) = Eta0Si
       case('mu')
          Param_I(iPar) = MuLimbDarkening
       case('obsx')
          Param_I(iPar) = ObsPos_DI(1,iFile)
       case('obsy')
          Param_I(iPar) = ObsPos_DI(2,iFile)
       case('obsz')
          Param_I(iPar) = ObsPos_DI(3,iFile)
       case('rTrace')
          Param_I(iPar) = rTrace
       case('dt')
          if(IsDimensionalPlot_I(iFile))then
             Param_I(iPar) = Dt*No2Io_V(UnitT_)
          else
             Param_I(iPar) = Dt
          end if
       case('xsi')
          if(IsDimensionalPlot_I(iFile))then
             Param_I(iPar) = Io2Si_V(UnitX_)
          else
             Param_I(iPar) = No2Si_V(UnitX_)
          end if
       case('tsi')
          if(IsDimensionalPlot_I(iFile))then
             Param_I(iPar) = Io2Si_V(UnitT_)
          else
             Param_I(iPar) = No2Si_V(UnitT_)
          end if
       case('usi')
          if(IsDimensionalPlot_I(iFile))then
             Param_I(iPar) = Io2Si_V(UnitU_)
          else
             Param_I(iPar) = No2Si_V(UnitU_)
          end if
       case('rhosi')
          if(IsDimensionalPlot_I(iFile))then
             Param_I(iPar) = Io2Si_V(UnitRho_)
          else
             Param_I(iPar) = No2Si_V(UnitRho_)
          end if
       case('mi','m1')
          Param_I(iPar) = MassFluid_I(1)
       case('m2')
          Param_I(iPar) = MassFluid_I(min(2, nFluid))
       case('m3')
          Param_I(iPar) = MassFluid_I(min(3, nFluid))
       case('m4')
          Param_I(iPar) = MassFluid_I(min(4, nFluid))
       case('m5')
          Param_I(iPar) = MassFluid_I(min(5, nFluid))
       case('m6')
          Param_I(iPar) = MassFluid_I(min(6, nFluid))
       case('m7')
          Param_I(iPar) = MassFluid_I(min(7, nFluid))
       case('m8')
          Param_I(iPar) = MassFluid_I(min(8, nFluid))
       case('m9')
          Param_I(iPar) = MassFluid_I(min(9, nFluid))
       case('me')
          Param_I(iPar) = MassFluid_I(nIonFluid)
       case('q1')
          Param_I(iPar) = ChargeIon_I(1)
       case('q2')
          Param_I(iPar) = ChargeIon_I(min(2,nIonFluid))
       case('q3')
          Param_I(iPar) = ChargeIon_I(min(3,nIonFluid))
       case('q4')
          Param_I(iPar) = ChargeIon_I(min(4,nIonFluid))
       case('q5')
          Param_I(iPar) = ChargeIon_I(min(5,nIonFluid))
       case('q6')
          Param_I(iPar) = ChargeIon_I(min(6,nIonFluid))
       case('q7')
          Param_I(iPar) = ChargeIon_I(min(7,nIonFluid))
       case('q8')
          Param_I(iPar) = ChargeIon_I(min(8,nIonFluid))
       case('q9')
          Param_I(iPar) = ChargeIon_I(min(9,nIonFluid))
       case('qe')
          Param_I(iPar) = ChargeIon_I(nIonFluid)
       case default
          Param_I(iPar) = -7777.
          if(iProc==0)write(*,*) NameSub, ' Error: unknown parameter name=',&
               NameParam_I(iPar),' for iFile=',iFile
       end select
    end do

    call test_stop(NameSub, DoTest)
  end subroutine set_plot_scalars
  !============================================================================
end module ModPlotScalars
!==============================================================================
