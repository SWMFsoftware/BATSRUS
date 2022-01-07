#  Copyright (C) 2002 Regents of the University of Michigan,
#  portions used with permission 
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
    percent_max_p => PercentPLimit_I,
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
    
    #ip => iP,
    #jp => jP,
    #kp => kP,
    #im => iM,
    #jm => jM,
    #km => kM,
    #fp1 => fP1,
    #fp2 => fP2,
    #dp1 => dP1,
    #dp2 => dP2,
    #ap1 => aP1,
    #ap2 => aP2,

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
    #ii => i0,
    #jj => j0,
    #kk => k0,

    ### ModHdf5*
    #PlotVarIdx => PlotVar_CBV,
    #PlotXYZNodes => Xyz_DNB,
    #UsedBlocks => iBlockUsed_B,
    #usednodes => iNodeUsed_B,
    #BlocksPerProc => nBlock_P,
    #MinLogicalExtents => iCoord_DB,
    #unknownNameArray => NameUnknown_V,
    #fileID => FileID,
    #iPlotDim => iDimPlot_D,
    #nCellsPerBlock => nCellBlock_D,
    #nNodeCellsPerBlock => nNodeBlock_D,
    #iplotComm => iCommPlot,
    #nplotvar => nPlotVar,
    #nPlotProc => nProcPlot,
    #iPlotProc => iProcPlot,
    #isNonCartesian => IsNonCartesian,
    #NotACut => IsNotACut,
    #plotType => TypePlot,
    #ii => i2,
    #jj => j2,
    #kk => k2,
    #ii1 => i3,
    #jj1 => j3,
    #kk1 => k3,
    #H5Index => iH5Index,
    #PlotVar => PlotVar_GV,
    #H5advance => DoH5Advance,
    #filename => NameFile,
    #plotvarnames => NamePlotVar_V,
    #plotVarUnits => NamePlotUnit_V,
    #plot_dimensional => IsDimensionalPlot,
    #Error => iError,
    #nBlocksUsedMax => nBlockUsedMax,
    #labelLeng => lLabel,
    #Coordinates => Coord_DB,
    #BoundingBox => CoordLimit_SDB,
    #BlockDataExtents => DataLimit_SB,
    #procnum => iProcPlot_B,
    #GlobalVarMin => GlobalVarMin_V,
    #GlobalVarMax => GlobalVarMax_V,
    #DatasetNameTemp => NameDataTmp,
    #isCutFile => IsCutFile,
    #isXZero => IsXZero,
    #RealMetaData => MetaData_I,
    #IntegerMetaData => IntMetaData_I,
    #FFV => Ffv_,

    ### ModIO
    #nPlotvarLosMax => MaxPlotvarLos,
    #nPlotRfrFreqMax => MaxPlotRadioFreq,
    #nPlotvarMax => MaxPlotvar,
    #restart => IsRestart,
    #restart_Bface => DoRestartBface,
    #save_plots_amr => DoSavePlotsAmr,
    #save_logfile => DoSaveLogfile,
    #save_binary => DoSaveBinary,
    #unit_log => iUnitLogfile,
    #n_pix_r => nPixel_I,
    #r_size_image => rSizeImage_I,
    #xOffset => xOffset_I,
    #yOffset => yOffset_I,
    #radius_occult => rOccult_I,
    #mu_los => MuLimbDarkening,
    #offset_angle => OffsetAngle_I,
    #n_Pix_X => nPixelX_I,
    #n_Pix_Y => nPixelY_I,
    #X_Size_Image => xSizeImage_I,
    #Y_Size_Image => ySizeImage_I,
    #dn_output => DnOutput_I,
    #dt_output => DtOutput_I,
    #n_output_last => nStepOutputLast_I,
    #t_output_last => iTimeOutputLast_I,
    #dn_progress1 => DnProgressShort,
    #dn_progress2 => DnProgressLong,
    #plot_type => TypePlot_I,
    #plot_type1 => TypePlot,
    #plot_form => TypePlotFormat_I,
    #log_form => TypeLogFormat,
    #plot_range => PlotRange_EI,
    #plot_point => PlotPointXyz_DI,
    #plot_normal => PlotNormal_DI,
    #plot_dx => PlotDx_DI,
    #plot_vars => StringPlotVar_I,
    #plot_vars1 => StringPlotVar,
    #plot_pars => StringPlotParam_I,
    #plot_pars1 => StringPlotParam,
    #log_vars => StringLogVar,
    #log_R_str => StringLogRadius,
    #log_time => TypeLogTime,
    #plot_dimensional => IsDimensionalPlot_I,
    #IsPlotName_n => IsPlotNameN,
    #IsPlotName_t => IsPlotNameT,
    #IsPlotName_e => IsPlotNameE,
    #IsLogName_n => IsLogNameN,
    #IsLogName_e => IsLogNameE,
    #save_restart_file => DoSaveRestart,
    #NameLosTable => NameLosTable_I,

    ### ModIeCoupling
    #jHall_DII => HallJ_DII,
    #jPedersen_DII => PedersenJ_DII,
    #Xyz_tmp => Xyz_D,

    ### ModImCoupling
    #IM_lat => ImLat_I,
    #IM_lon => ImLon_I,
    #IM_bmin => ImBmin_II,
    #ImP_CV => ImP_III,
    #ImRho_CV => ImRho_III,
    #ImPpar_CV => ImPpar_III,

    ### ModImplicit
    #ImplCritType => TypeImplCrit,
    #n_prev => nStepPrev,
    #dt_prev => DtPrev,
    #FluxTypeImpl => TypeFluxImpl,
    
    ### ModMultiFluid
    #RhoNeutralsISW_dim => RhoNeuWindDim,
    #PNeutralsISW_dim => pNeuWindDim,
    #UxNeutralsISW_dim => UxNeuWindDim,
    #UyNeutralsISW_dim => UyNeuWindDim,
    #UzNeutralsISW_dim => UzNeuWindDim,
    #TNeutralsISW_dim => TempNeuWindDim,
    #mProtonMass => MassNeutralDim,
    #SubString => StringEnd,

    ### ModNodes
    #NodeNumberLocal_NB => iNodeLocal_NB,
    #NodeNumberGlobal_NB => iNodeGlobal_NB,
    #NodeUniqueGlobal_NB => IsNodeUnique_NB,

    ### ModParticleFieldLine
    #KindEnd_ => iKindEnd,
    #KindReg_ => iKindReg,
    
    ### ModParticleMover
    #Index_II => Int_II,

    ### ModPhysics
    #Pratio_lo  => pRatioLo,
    #Pratio_hi  => pRatioHi,
    #SW_T_dim   => SolarWindTempDim,
    #SW_rho     => SolarWindRho,
    #SW_rho_dim => SolarWindRhoDim,
    #SW_n       => SolarWindN,
    #SW_n_dim   => SolarWindNDim,
    #SW_p       => SolarWindP,
    #SW_p_dim   => SolarWindPDim,
    #SW_Ux      => SolarWindUx,
    #SW_Ux_dim  => SolarWindUxDim,
    #SW_Uy      => SolarWindUy,
    #SW_Uy_dim  => SolarWindUyDim,
    #SW_Uz      => SolarWindUz,
    #SW_Uz_dim  => SolarWindUzDim,
    #SW_Bx      => SolarWindBx,
    #SW_Bx_dim  => SolarWindBxDim,
    #SW_By      => SolarWindBy,
    #SW_By_dim  => SolarWindByDim,
    #SW_Bz      => SolarWindBz,
    #SW_Bz_dim  => SolarWindBzDim,

    ### ModPointImplicit
    #MAXVAR => MaxVar,
    #IL => iL,
    #II => iI,
    #ILMAX => iLMax,
    #JL => jL,
    #KL => kL,
    #LL => lL,
    #INDX => i_I,
    #SCALING => Scaling_I,
    #LHSMAX => LhsMax,
    #LHSTEMP => LhsTemp,
    #TOTALSUM => TotalSum,
    #TINY => cTiny,
    
    ### ModProject
    #proj_method => TypeProjectIter,
    #proj_typestop => TypeProjectStop,
    #proj_divbcoeff => RelativeLimit,
    #proj_divbconst => AbsoluteLimit,
    #proj_matvecmax => MaxMatvec,
    #divbmin => DivBTiny,
    #proj_divb => DivB_GB,
    #phi => Phi_GB,
    #info => iInfo,
    #ierror => iError,
    #nmatvec => nMatvec,
    #resid => Resid,
    #divbmax_now => DivBMaxNow,
    #pmin_old => pMinOld,
    #pmin_new => pMinNew,
    #loc => iLoc_I,
    #rhs => Rhs_GB,
    #typestop => TypeStop,
    #tolerance => Tolerance,
    #matvecmax => MaxMatvec,
    #laplace_phi => LaplacePhi_GB,
    #idim => iDim,
    #dphi => dPhi_GB,
    #ddphi => DdPhi_GB,
    #phiC => Phi_III,
    #Phi_G => Phi_GB,
    #qx => x_GB,
    #iter => nIter,
    #tol => Tol,
    #bicg_r1 => r1_GB,
    #bicg_u1 => u1_GB,
    #rwork   => Work_II,
    #nmv => nMv,
    #alpha => Alpha, beta => Beta, omega => Omega,
    #rho => Rho,
    #rho0 => Rho0, rho1 => Rho1, sigma => Sigma,
    #assumedzero => AssumedZero,
    #rnrm0 => rNorm0,
    #rnrm => rNorm,
    #rnrmMax0 => rNormMax0,
    #rnrmMax => rNormMax,
    #kappa0 => Kappa0,
    #kappa1 => Kappa1,
    #itr => iIter,
    #matv => nMatvec,
    #rhonew => RhoNew,
    #res => Res,
    #res0 => Res0,
    #bet => Bet,
    #alf => Alf,
    #GoOn => DoGoOn,
    #varrho => VarRho,
    #hatgamma => HatGamma,
    #kappal => KappaL,
    #set_BLK => set_block_scalar,
    #eq_BLK => set_block_array,
    #add_BLK => add_block,
    #sub_BLK => sub_block,
    #eq_plus_BLK => add2_block,
    #add_times_BLK => add_times_block,
    #eq_plus_times_BLK => add2_times_block,
    #dot_product_BLK => dot_product_block,
    #qproduct => BlockProduct,
    #sum_BLK => sum_block,
    #qsum => BlockSum,
    #qa => a_GB,
    #qb => b_GB,
    #qc => c_GB,
    #qd => d_GB,

    ### ModSatellite
    #FilenameSat_I => NameFileSat_I,
    #TimeSat_I => TypeTimeSat_I,
    #iCurrent_satellite_position => iPointCurrentSat_I,
    #satellite_var => NameSatVar,
    #set_NameFile => set_name_file,
    #FilenameOutSat => NameFileOutSat,
    #dtime => dTime,
    #Xvect => XyzSat_D,
    #RayVars => Trace_DSC,

    ### ModSetParameters
    #plot_string => StringPlot,
    #log_string => StringLog,
    #plot_area => TypePlotArea,
    #plot_var => TypePlotVar,
    #TimingDepth => nDepthTiming,
    #TimingStyle => TypeTiming,
    #iVarSmoothReal_V => RealIVarSmooth_V,

    ### ModThreadedLC
    #M_VVI => Main_VVI,
    #L_VVI => Lower_VVI,
    #U_VVI => Upper_VVI,
    #R_VI => Res_VI,
    #W_VI => Weight_VI,
    #Major_ => iMajor,
    #Minor_ => iMinor,
    #tridiag_3by3_block => tridiag_block33,

    ### ModUserEmpty
    #user_block_type => i_type_block_user,

    ### ModWaves: shorter index names
    #AlfvenWavePlusFirst_  => AlfvenPlusFirst_,
    #AlfvenWaveMinusFirst_ => AlfvenMinusFirst_,
    #AlfvenWavePlusLast_   => AlfvenPlusLast_,
    #AlfvenWaveMinusLast_  => AlfvenMinusLast_,

    ### ModWritePlot
    #PlotVar => PlotVar_GV,
    #PlotVarBlk => PlotVarTec_GV,
    #PlotVar_inBody => PlotVarBody_V,
    #PlotVar_useBody => UsePlotVarBody_V,
    #plotvarnames => NamePlotVar_V,
    #nplotvar => nPlotVar,
    #allnames => NameAllVar,
    #unitstr_TEC => StringUnitTec,
    #unitstr_IDL => StringUnitIdl,
    #filename_n => NameFileNorth,
    #filename_s => NameFileSouth,
    #filename_h => NameFileHeader,
    #H5Index => iH5Index,
    #xmin => Coord1Min,
    #xmax => Coord1Max,
    #ymin => Coord2Min,
    #ymax => Coord2Max,
    #zmin => Coord3Min,
    #zmax => Coord3Max,
    #dxblk => CellSize1,
    #dyblk => CellSize2,
    #dzblk => CellSize3,
    #dxglobalmin => CellSizeMin_D,
    #nPEcells => nCellProc,
    #nGLOBALcells => nCellAll,
    #NotACut => IsNotCut,
    #H5Advance => DoH5Advance,
    #ii => i2,
    #jj => j2,
    #kk => k2,
    #j_DC => Current_DC,
    #itmp => i3,
    #jtmp => j3,
    #nBLKcells => nCellBlock,

    ### ModWritePlotIdl
    #PlotVar => PlotVar_GV,
    #DxBlock => CellSize1,
    #DyBlock => CellSize2,
    #DzBlock => CellSize3,

    ### ModWritePlotLos
    #eqpar => Param_I,
    #eqparnames => NameParam_I,
    #plotvarnames => NamePlotVar_V,
    #allnames => NameAllVar,
    #unitstr_TEC => StringUnitTec,
    #unitstr_IDL => StringUnitIdl,
    #file_extension => StringExtension,
    #file_format => StringFormat,
    #TextDateTime0 => StringDateTime0,
    #TextDateTime => StringDateTime,
    #FormatTime => StringFormatTime,
    #AlignedZ => IsAlignedZ,
    #TableVarNames => NameTableVar_V,
    #a_los => aLos,
    #b_los => bLos,
    #c_los => cLos,
    #d_los => dLos,
    #StateInterpolateDone => DoneStateInterpolate,
    #EuvResponse => EuvResponse_W,
    #SxrResponse => SxrResponse_W,
    #counter => nCount,
    #intrsct => IntersectXyz_SDD,
    #face_location => FaceXyz_SD,
    #xx1 => x1,
    #xx2 => x2,
    #yy1 => y1,
    #yy2 => y2,
    #zz1 => z1,
    #zz2 => z2,
    #coeff1 => Coef1,
    #coeff2 => Coef2,
    #coeff3 => Coef3,
    #get_TEC_los_variables => get_los_variable_tec,
    #x_q => x,
    #y_q => y,
    #z_q => z,
    #get_IDL_los_units => get_los_unit_idl,
    #UnitForALlNvars => IsDimensional,
    #neqparmax => MaxParam,
    #ifile => iFile,
    
    ### ModWritTecplot
    #TextDateTime => StringDateTime,
    #textNandT => StringNandT,
    #TextDateTime0 => StringDateTime0,
    #real_date => StringDate,
    #real_time => StringTime,
    #str => String,
    #tmp => StringTmp,
    #ifile => iFile,
    #unitstr_TEC => StringUnitTec,
    #PlotVarBlk => PlotVarTec_GV,
    #xmin => xMin,
    #xmax => xMax,
    #ymin => yMin,
    #ymax => yMax,
    #zmin => zMin,
    #zmax => zMax,
    #cut1 => Ijk1,
    #cut2 => Ijk2,
    #factor1 => Factor1,
    #factor2 => Factor2,
    #nBlockCuts => nBlockCut,
    #BlockCut => iBlockCut_A,
    #nBlockALL => nNodeUsed,
    #formatdata => StringFormat,
    #ic1 => i1,
    #ic2 => i2, 
    #jc1 => j1,
    #jc2 => j2, 
    #kc1 => k1,
    #kc2 => k2,
    #ic  => i,
    #jc  => j,
    #kc  => k,
    #fill_nodeXYZ => fill_node_xyz,
    #iopt => iOption,
    #NodesPerBlock => nNodeBlock,
    #NodeOffset => nNodeOffset_P,
    #NodeOffsetMax => MaxNodeOffset_P,
    #iStatus => iStatus_I,
    
    );
