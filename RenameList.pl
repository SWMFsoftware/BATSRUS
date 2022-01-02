#  Copyright (C) 2002 Regents of the University of Michigan, portions used with permission 
#  For more information, see http://csem.engin.umich.edu/tools/swmf
##################################################################
#          Add variable name replacement rules as                #
#                                                                #
#             oldname => newname,                                #
#                                                                #
#          One rule per line!                                    #
#                                                                #
#  This file should be named RenameList.pl and be in the same    #
#  directory as Rename.pl, or it should be explicitly given      #
#  with the -i=FileName switch of Rename.pl                      #
#                                                                #
#                                                                #
##################################################################

%newname=(
    ### ModMain
    #n_step => nStep,
    #iteration_number => nIteration,
    #Dt_BLK => DtMax_B,
    
    #time_accurate => IsTimeAccurate,
    #time_loop => IsTimeLoop,
    #Body1 => UseBody,
    #optimize_message_pass => TypeMessagePass,
    #GravityDir => iDirGravity,
    #TypeCoordSystemInt => iTypeCoordSystem,
    #okdebug => DoDebug,
    #ShowGhostCells => DoShowGhostCells,
    #Time_Simulation => tSimulation,
    #Time_SimulationOld => tSimulationOld,
    #dn_timing => DnTiming,
    #t_Max => tSimulationMax,
    #cputime_max => CpuTimeMax,
    #Check_Stopfile => DoCheckStopFile,

    ### ModGeometry
    #XyzStart_BLK => Coord111_DB,
    #Body_BLK => IsBody_B,
    #true_BLK => IsNoBody_B,
    #far_field_BCs_BLK => IsBoundary_B,
    #R_BLK => r_GB,
    #R2_BLK => rBody2_GB,
    #Rmin_BLK => rMin_B,
    #Rmin2_BLK => rMinBody2_B,
    #x1 => xMinBox,
    #x2 => xMaxBox,
    #y1 => yMinBox,
    #y2 => yMaxBox,
    #z1 => zMinBox,
    #z2 => zMaxBox,
    #nTrueCells => nUsedCell,
    #true_cell => Used_GB,

    ### ModParallel: Replace *east...*west with array vars
    #neiLEV => DiLevel_EB,
    #neiBLK => jBlock_IEB,
    #neiPE  => jProc_IEB,
    #NOBLK  => Unset_,

    ### ModAdvance
    #FluxType => TypeFlux,
    #percent_max_rho => PercentRhoLimit_I,
    #precent_max_p => PercentPLimit_I,
    #tmp1_BLK => Tmp1_GB,
    #tmp2_BLK => Tmp2_GB,
    #time_BLK => DtMax_CB

    ### ModBatsrusMethods
    #SaveThreads4Plot => DoPlotThread,
    #
    ### ModConstrainB
    #BxFace_BLK => BxFace_GB, 
    #ByFace_BLK => ByFace_GB, 
    #BzFace_BLK => BzFace_GB, 
    #get_VxB => get_vxb,
    #bound_VxB => bound_vxb,
    #constrain_B => constrain_b,
    #Bface2Bcenter => bface_to_bcenter,
    #Bcenter2Bface => bcenter_to_bface,
    #bound_Bface => bound_bface,
    #
    ### ModCoronalHeating
    #MaxImbalance => ImbalanceMax,
    #MaxImbalance2 => ImbalanceMax2,
    #
    ## ModFaceValue
    
    ip => iP,
    jp => jP,
    kp => kP,
    im => iM,
    jm => jM,
    km => kM,
    fp1 => fP1,
    fp2 => fP2,
    dp1 => dP1,
    dp2 => dP2,
    ap1 => aP1,
    ap2 => aP2,

    ### ModFieldTrace
    #xEnd_ => iXEnd,
    #yEnd_ => iYEnd,
    #zEnd_ => iZEnd,
    #PeInvB_ => iPeInvB,
    #Length_ => iLength,
    #LOOPRAY => LoopRay,
    #NORAY => NoRay,
    #ray_iono_ => RayIono_,
    #ray_equator_ => RayEquator_,
    #ray_block_ => RayBlock_,
    #ray_open_ => RayOpen_,
    #ray_loop_ => RayLoop_,
    #ray_body_ => RayBody_,
    #ray_out_ => RayOut_,
    #set_DoTestRay => set_dotestray,
    #IjkIni_D => IndIni_D,
    #IjkMid_D => IndMind_D,
    #IjkCur_D => IndCur_D,
    #Ijk_D => Ind_D,
    #dxRel => DxRel,
    #dxOpt => DxOpt,
    #Dl => Ds,
    #Dlp => Ds01,
    #dlNext => DsNext,
    #dlMax => DsMax,
    #dlMin => DsMin,
    #dlTiny => DsTiny,
    #dx1 => Dx1,
    #dy1 => Dy1,
    #dz1 => Dz1,
    #dx2 => Dx2,
    #dy2 => Dy2,
    #dz2 => Dz2,
    #IjkIn_D => IndIn_D,
    #IjkOut_D => IndOut_D,
    #FileName => NameFile,
    #Integrals => Integral_I,
    #MapDown => DoMapDown,
    #Map1 => DoMap1,
    #Map2 => DoMap2,
    #Odd => IsOdd,
    #Skip => DoSkip,
    #SaveIntegrals => DoSaveIntegral,
    #stmp => String,
    #coord => StringCoord,
    #NS => StringNorS,
    #OC => iMap,
    #IE_lat => IeLat_I,
    #IE_lon => IeLon_I,
    #follow_iono => do_follow_iono,
    #ray => Trace_DSNB,

    ### ModFieldTraceFast
    ii => i0,
    jj => j0,
    kk => k0,
    
    );
