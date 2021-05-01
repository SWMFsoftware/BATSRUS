#!/usr/bin/perl -s
#  Copyright (C) 2002 Regents of the University of Michigan,
#  portions used with permission 
#  For more information, see http://csem.engin.umich.edu/tools/swmf

my $DefaultExe = "BATSRUS.exe";
my $Help   = $h;
my $Verbose= $v+0;
my $RunDir = ($d or "run");
my $Norun  = $norun+0;
my $Execute= ($x or $DefaultExe);
my $Run    = ($r or $Run="mpirun -np 2");
my $Flags  = $f;
my $Sleep  = $s+0;

&print_help if $Help or $#ARGV != 0;

# Figure out directory for TestBatsrus.pl
my $Dir = $0; $Dir =~ s/TestSuite\.pl//; $Dir = './' unless $Dir;

my $testbatsrus = "$Dir"."TestBatsrus.pl ".
    "-v=$Verbose -norun=$Norun -d=$RunDir -r='$Run' -x=$Execute -f='$Flags' ".
    "-s=$Sleep";

my $Table  =$ARGV[0];

my $Empty = "-Planet=earth_simple -Grid=earth_211 -Res=1.0".
    " -Length=tiny_simple".
    " -B0source=empty -Cfl=empty -Conservative=r6".
    " -Corotation=empty -Divb=empty -Reschange=empty -Implicit=empty".
    " -Inner=ionosphere -Message=empty -Outer=inflowsidesfixed".
    " -Stage=empty -Time=empty -Timestep=empty -Upstream=empty";

###########################################################################
if($Table =~ /test/i){
###########################################################################

    &execute($testbatsrus,"-beta");

###########################################################################
}elsif($Table =~ /empty/i){
###########################################################################

    &execute($testbatsrus, $Empty);

###########################################################################
}elsif($Table =~ /func/i){
###########################################################################

    &execute($testbatsrus,"-beta -rusanov -idltecamr");
    &execute($testbatsrus,"-beta -linde");             
    &execute($testbatsrus,"-beta -sokolov -idltecamr");
    &execute($testbatsrus,"-mc -roe -Reschange=tvd");  
    &execute($testbatsrus,"-Conservative=r6 -rusanov");
    &execute($testbatsrus,"-Conservative=adapt -linde")
    &execute($testbatsrus,"-Conservative=r6 -sokolov");
    &execute($testbatsrus,"-Conservative=r6 -roeold");    
    &execute($testbatsrus,"-borisfull -rusanov");
    &execute($testbatsrus,"-borisfull -linde");  
    &execute($testbatsrus,"-borisfull -sokolov");
    &execute($testbatsrus,"-borissimple -rusanov");
    &execute($testbatsrus,"-borissimple -linde");  
    &execute($testbatsrus,"-ta -hlld -Conservative=r6 -idltecamr");
    &execute($testbatsrus,"-ta -Conservative=r6 -borisfull");
    &execute($testbatsrus,"-ta -Conservative=r6 -borissimple");
    &execute($testbatsrus,"-Inner=float"); 
    &execute($testbatsrus,"-Inner=reflect"); 
    &execute($testbatsrus,"-Limiter=mc -Resist=hall");
    &execute($testbatsrus,"-Limiter=mc3 -hlld");
    &execute($testbatsrus,"-ta -Stage=2 -partimpl05 -Limiter=mc3",
	     "-hall -logsatmove -Reschange=accurate -Length=tiny_ta");

    &execute($testbatsrus,"-ta -Stage=2 -constrain",                  
	     "-Grid=earth_uniform -Res=2.0 -Length=tiny_ta_noamr");

    &execute($testbatsrus,"-ta -Stage=2 -project -Length=tiny_ta");
    &execute($testbatsrus,"-ta -Stage=2 -diffuse -Length=tiny_ta");
    &execute($testbatsrus,"-ta -Stage=2 -partimpl05 -Length=tiny_ta");

    &execute($testbatsrus,"-Limiter=mc3 -hlld",
		"-Restart=saveone -Length=restartsave");

    &execute($testbatsrus,"-Limiter=mc3 -hlld",
		"-Restart=read -Length=restartread");

    &execute($testbatsrus,"-ta -Conservative=r6 -hlld",
		"-Restart=save -Length=restartsave");

    &execute($testbatsrus,"-ta -Conservative=r6 -hlld",
		"-Restart=read -Length=restartread");

    &execute($testbatsrus,"-ta -Corotation=ideal,updateb0 -Length=tiny_ta");
    &execute($testbatsrus,"-Plottype=ray -Logtype=ray -nsturning_1nT_tilt");
    &execute($testbatsrus,"-Plottype=sph -Logtype=logpntflx");
    &execute($testbatsrus,"-Plottype=los");
    &execute($testbatsrus, $Empty);
    &execute($testbatsrus,"-Plottype=raynew -Logtype=ray -nsturning_1nT_tilt");

###########################################################################
}elsif($Table =~ /robust/i){
###########################################################################

    &execute($testbatsrus,"-SSSS -N_5nT -Logs=10");
    &execute($testbatsrus,
             "-ta -Plots=3minute -Logs=10", 
             "-borisfull_0.005 -full_ionosphere -Length=hour_restart",
             "-Solver=rusanov",
             "-Upstream=nsturning_5nT");
    &execute($testbatsrus,
             "-ta -Plots=3minute -Logs=10", 
             "-borisfull_0.005 -full_ionosphere -Length=20minute_restart",
             "-Solver=rusanov",
             "-Upstream=ppulse_inc20,ppulse_dec10");
    &execute($testbatsrus,
             "-ta -Plots=3minute -Logs=10", 
             "-borisfull_0.005 -full_ionosphere -Length=hour_restart",
             "-Solver=sokolov",
             "-Upstream=nsturning_5nT");
    &execute($testbatsrus,
             "-ta -Plots=3minute -Logs=10", 
             "-borisfull_0.005 -full_ionosphere -Length=20minute_restart",
             "-Solver=sokolov",
             "-Upstream=ppulse_inc20,ppulse_dec10");
    &execute($testbatsrus,
             "-ta -Plots=3minute -Logs=10", 
             "-borisfull_0.005 -full_ionosphere -Length=hour_restart",
             "-Solver=linde",
             "-Upstream=nsturning_5nT");
    &execute($testbatsrus,
             "-ta -Plots=3minute -Logs=10", 
             "-borisfull_0.005 -full_ionosphere -Length=20minute_restart",
             "-Solver=linde",
             "-Upstream=ppulse_inc20,ppulse_dec10");
    &execute($testbatsrus,
             "-ta -Plots=minute -Logs=10", 
             "-borisfull_0.005 -full_ionosphere -Length=hour_restart",
             "-Solver=linde",
             "-Upstream=varying");
    &execute($testbatsrus,
             "-ta -Plots=3minute -Logs=10 -partimpl05", 
             "-borisfull -full_ionosphere -Length=20minute_restart",
             "-Solver=linde,",
             "-Upstream=nsturning_5nT");
    &execute($testbatsrus,
             "-ta -Plots=3minute -Logs=10", 
             "-noboris -full_ionosphere -Length=20minute_restart",
             "-Solver=roe",
             "-Upstream=nsturning_5nT");
}else{
    print "ERROR: unknown option for testsuite!\n";
    print_help;
}

##############################################################################

sub execute{

    $command = join(' ',@_);

    print "$command\n" if $Verbose;

    $result = `$command`;

    print $result;
}


##############################################################################
#BOP
#!ROUTINE: TestSuite.pl - test BATSRUS with a suite of tests
#!DESCRIPTION:
# This script can run a set of tests using TestBatsrus.pl.
#
#!REVISION HISTORY:
# 01/28/02 G.Toth and A.Ridley - initial version developed for BATSRUS
# 07/10/03 G.Toth              - adapted to SWMF
# 03/11/04 G.Toth              - moved back into BATSRUS but 
#                                it can be used in SWMF as well
#EOP

sub print_help{

    print 
#BOC
"

TestSuite.pl [-h] [-v] [-norun] [-d=DIR] [-s=SLEEP]     \\
             [-r=RUNCOMMAND] [-x=EXECUTABLE] [-f=FLAGS] \\
             test | func | robust

-h     print this help message

-v     verbose

-norun do not run $DefaultExe, just create test directories and parameter files

-s=SLEEP
       sleep SLEEP seconds between tests, e.g. use -s=10 for 10 seconds.

-d=DIR
       define the run directory. The default is -d=run.

-r=RUNCOMMAND
       define the runcommand to be executed. The default runcommand is 
       -r='mpirun -np 2'. You can specify a machinefile,
       or run on a different number of processors.

-x=COMMAND    
       executable. The default is $DefaultExe

-f=FLAGS
       command line flags for the executable. The default is no flags.

test   just do a single run

empty  test the default settings (almost empty PARAM.in file)

func   test the functionality (after syntactic change)

robust test robustness (of new algorithm)"

#EOC
,"\n\n";

    exit;
}
