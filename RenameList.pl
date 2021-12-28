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
    #ModMain
    n_step => nStep,
    iteration_number => nIteration,
    Dt_BLK => Dt_B,
    time_accurate => IsTimeAccurate,
    time_loop => IsTimeLoop,
    Body1 => UseBody,
    optimize_message_pass => TypeMessagePass,
    GravityDir => iDirGravity,
    TypeCoordSystemInt => iTypeCoordSystem,
    okdebug => DoDebug,
    ShowGhostCells => DoShowGhostCells,
    Time_Simulation => tSimulation,
    Time_SimulationOld => tSimulationOld,
    dn_timing => DnTiming,
    t_Max => tSimulationMax,
    cputime_max => CpuTimeMax,
    Check_Stopfile => DoCheckStopFile
    
	  );
