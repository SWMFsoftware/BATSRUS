#!/usr/bin/perl
#  Copyright (C) 2002 Regents of the University of Michigan, 
#  portions used with permission 
#  For more information, see http://csem.engin.umich.edu/tools/swmf

# Allow in-place editing
$^I = "";

# Add local directory to search
push @INC, ".";

use strict;

our $Component = "GM";
our $Code = "BATSRUS";
our $MakefileDefOrig = 'src/Makefile.def';
our @Arguments = @ARGV;

# Figure out remote git server
my $remote = `git config remote.origin.url`; $remote =~ s/\/BATSRUS(.git)?\n//;

#print "remote=$remote SWMFsoftware=$SWMFsoftware\n";

my $config   = "share/Scripts/Config.pl";
my $gitclone = "share/Scripts/gitclone -s";
my $MakefileConf = 'Makefile.conf';

# Use ../../share and ../../Makefile.conf if present and needed
if (not -f $config and -f "../../$config"){
    $config = "../../$config";
    $gitclone = "../../$gitclone";
    $MakefileConf = "../../$MakefileConf";
}

my $result;
# Git clone missing directories as needed. Start with share/ to get $gitclone.
if (not -f $config){
    print "Cloning share\n";
    $result = `git clone $remote/share 2>&1`;
    die $result unless -f $config;
    if(not -d "util"){
	print "Cloning util\n";
	$result = `git clone $remote/util 2>&1`;
	die $result unless -d "util";
    }
}

# Install srcBATL if not present
my $SrcBatl = 'srcBATL';
# The component ID is hidden from Rename.pl
if ($Component eq "G"."M" and not -d $SrcBatl){
    print "$gitclone $SrcBatl\n";
    $result = `$gitclone $SrcBatl 2>&1`;
    die $result if not -d $SrcBatl;
}

require $config;

# Variables inherited from share/Scripts/Config.pl
our %Remaining; # Unprocessed arguments
our $ERROR;
our $WARNING;
our $Help;
our $Verbose;
our $Show;
our $ShowGridSize;
our $NewGridSize;
our $NewGhostCell;
our $Install;

&print_help if $Help;

# Force settings without checking
my $Force;

# Equation and user module variables
my $Src         = 'src';
my $SrcUser     = 'srcUser';
my $SrcUserExtra= 'srcUserExtra';
my $UserMod     = "$Src/ModUser.f90";
my $UserModSafe = "$Src/ModUser.f90.safe";
my $SrcEquation = 'srcEquation';
my $EquationMod = "$Src/ModEquation.f90";
my $EquationModSafe = "$Src/ModEquation.f90.safe";
my $Equation;
my $UserModule;

# Grid size variables
my $NameBatlFile = $SrcBatl.'/BATL_size.f90';
my $GridSize;
my $GhostCell;
my ($nI, $nJ, $nK, $nG);

# additional variable information
my $nWave;
my $nWaveNew;
my $nPui;
my $nPuiNew;
my $nMaterial;
my $nMaterialNew;
my $ChargeState;
my $ChargeStateNew;

# Settings for optimization
my $OptParam    = "ModOptimizeParam.f90";
my $OptFile     = "src/ModOptimizeParam.f90";
my $OptFileOrig = "src/ModOptimizeParam_orig.f90";
my $Opt;

# For BATSRUS clones src/ is created during configuration of SWMF
if(not -d $Src){exit 0};

# Attempt installing srcUserExtra (may or may not be available)
# The component ID is hidden from Rename.pl
if ($Install and $Component eq "G"."M" and not -d $SrcUserExtra){
    print "$gitclone $SrcUserExtra\n";
    `$gitclone $SrcUserExtra 2>&1`;
    print "Cannot clone $SrcUserExtra ...no access...\n"
	if not -d $SrcUserExtra;
}

# Read previous grid size, equation and user module
&get_settings;

foreach (@Arguments){
    if(/^-f$/)                    {$Force=1;                       next};
    if(/^-e$/)                    {$Equation=1;                    next};
    if(/^-u$/)                    {$UserModule=1;                  next};
    if(/^-e=(.*)$/)               {$Equation=$1;                   next};
    if(/^-u=(.*)$/)               {$UserModule=$1;                 next};
    if(/^-s$/)                    {$Show=1;                        next};
    if(/^-nWave=([1-9]\d*)$/i)    {$nWaveNew=$1;                   next};
    if(/^-nPui=([1-9]\d*)$/i)     {$nPuiNew=$1;                    next};
    if(/^-nMaterial=([1-9]\d*)$/i){$nMaterialNew=$1;               next};
    if(/^-cs=(.*)/)               {$ChargeStateNew .= "$1";        next};
    if(/^-ng$/i)                  {print "ng=$GhostCell\n";        next};
    if(/^-ng=(.*)$/i)             {$NewGhostCell=$1;               next};
    if(/^-opt$/)                  {$Opt="show";                    next};
    if(/^-opt=(.*)$/)             {$Opt=$1;                        next};
    warn "WARNING: Unknown flag $_\n" if $Remaining{$_};
}

&set_grid_size if ($NewGridSize  and $NewGridSize ne $GridSize)
    or            ($NewGhostCell and $NewGhostCell ne $GhostCell);

# Show grid size in a compact form if requested
print "Config.pl -g=$nI,$nJ,$nK\n",
    if $ShowGridSize and not $Show;

# Set or list the equations
&set_equation if $Equation;

# Set additional variable information
open(FILE, $EquationMod);
while(<FILE>){
    next if /^\s*!/; # skip commented out lines
    $nWave=$1        if /\bnWave\s*=\s*(\d+)/i;
    $nPui=$1         if /\bnPui\s*=\s*(\d+)/i;
    $nMaterial=$1    if /\bnMaterial\s*=\s*(\d+)/i;
    $ChargeState=$1  if /NameElement_I\(1:nElement\)\s*=\s*(.*)/;
}
$ChargeState =~ tr/,\[\]\' /+/d; # format charge state list as h+o
close FILE;

die "$ERROR nWave was not found in equation module\n"
    if $nWaveNew and not $nWave;
&set_nwave     if $nWaveNew and $nWaveNew ne $nWave;

die "$ERROR nPui was not found in equation module\n"
    if $nPuiNew and not $nPui;
&set_npui      if $nPuiNew and $nPuiNew ne $nPui;

die "$ERROR nMaterial was not found in equation module\n"
    if $nMaterialNew and not $nMaterial;
&set_nmaterial if $nMaterialNew and $nMaterialNew ne $nMaterial;

die "$ERROR ChargeState was not found in equation module\n"
    if $ChargeStateNew and not $ChargeState;
&set_charge_state if $ChargeStateNew and $ChargeStateNew ne $ChargeState;

# Set or list the user modules
&set_user_module if $UserModule;

my $Settings = &current_settings; print $Settings if $Show;

# (Re)Create Makefile.RULES file(s) based on current settings
&create_makefile_rules($Settings);

# Optimize settings in $OptFile
&set_optimization if $Opt;

exit 0;

#############################################################################
sub check_var{

    # modify the first argument "oldvalue" as needed

    my $oldvalue = $_[0];
    my $newvalue = $_[1];
    my $first    = $_[2];

    print "oldvalue=$oldvalue newvalue=$newvalue first=$first\n" if $Verbose;
    
    return if $oldvalue eq "any"; # already adjustable

    $newvalue =~ s/^\s+//;        # remove leading space
    $newvalue =~ s/\s+.*//;       # remove comment
    $newvalue =~ s/\n//;          # remove newline
    $newvalue =~ s/^T$/.true./;   # logical true
    $newvalue =~ s/^F$/.false./;  # logical false

    if($first or $oldvalue eq ""){
	$_[0] = $newvalue; # first use of new value, make it fixed
    }elsif($newvalue ne $oldvalue){
	$_[0] = "any"; # two different values, cannot be fixed
    }

    print "modified oldvalue=$_[0]\n" if $Verbose;

}
#############################################################################
sub set_optimization{

    # Read current settings
    my %Opt;
    # Make sure optimized parameter file exists and is up-to-date
    `cd src; make $OptParam`;
    open(FILE, $OptFile) or die "$ERROR could not open $OptFile\n";
    while(<FILE>){
	# Extract adjustables: NAME => ... lines without Orig
	$Opt{$1} = 'any' if /^\s+(\w+)\s*\=\>/ and not /Orig/;
	# Extract fixed params: parameter:: NAME = VALUE
	$Opt{$1} = $2    if /parameter\s*::\s*(\w+)\s*=\s*(.+)/;
	last if /^\s*contains\s*$/;
    }
    close(FILE);
    print "-" x 70, "\nCurrent optimization:\n";
    my $name;
    foreach $name (sort keys %Opt){
	printf "%-30s = %s\n", $name, $Opt{$name};
    }
    return if $Opt eq "show";

    # Process -opt=... argument in $Opt
    print "-" x 70, "\n-opt=$Opt\n";
    print "-" x 70, "\nNew optimization:\n";

    my $Change; # set to 1 is settings change

    if($Opt =~ /PARAM\.in/){

	# Set the component name used
	my $NameComp = $Component;
	open(FILE, $Opt) or die "$ERROR could not open $Opt\n";
	while(<FILE>){
	    # Read component name from #COMPNENT command
	    if(/^#COMPONENT\b/){
		my $name = <FILE>;
		my $ison = <FILE>;
		$NameComp = $name unless $ison =~ /^T|F/;
		last;
	    }
	}
	close(FILE);
	    
	
	# Read settings from some PARAM.in file
	# Default settings
	my %Set = (
	    ClightFactor        => 1,
            DoLf                => ".true.",
            IsCartesian         => ".true.",
            IsCartesianGrid     => ".true.",
	    IsTimeAccurate      => ".true.",
	    LimiterBeta         => 1,
	    UseB0               => "UseB"  ,
	    UseBody             => ".false.",
            UseBorisCorrection  => ".false.",
	    UseCoarseAxis       => ".false.",
	    UseDivbSource       => "UseB .and. nDim>1",
	    UseDtFixed          => ".false.",
	    UseElectronEntropy  => "UseElectronPressure",
	    UseGravity          => ".false.",
            UseHyperbolicDivB   => ".false.",
            UseNonConservative  => ".false.",
	    UsePMin             => ".false.",
	    UseRhoMin           => ".false.",
	    UseRotatingFrame    => ".false.",
            iStage              => 1,
            nStage              => 1,
	    nConservCrit        => 0,
	    nOrder              => 1,
	    );

	# Component dependent defaults (from ModSetParameters)
	$Set{"UseB0"}            = ".false." if $NameComp =~ /IH|OH/;
	$Set{"UseGravity"}       = ".true."  if $NameComp !~ /GM/;
	$Set{"UseRotatingFrame"} = ".true."  if $NameComp =~ /SC|EE/;

	print "processing parameter file $Opt\n";
	my $first = 1;  # true in the first session
	my $nstage = 1; # default value for nStage
	open(FILE, $Opt) or die "$ERROR could not open $Opt\n";
	while(<FILE>){
	    if(/^\#RUN\b/ or /^\#END\b/ or eof(FILE)){
		check_var($Set{"nStage"}, $nstage, $first);
		check_var($Set{"iStage"}, "any", $first) if $nstage != 1;
		last unless /^#RUN\b/;
		$first = 0; # end of first session
	    }
	    # Read settings from various commands
	    if(/^#TIMEACCURATE\b/){
		my $timeacc = <FILE>;
		check_var($Set{"IsTimeAccurate"}, $timeacc, $first);
	    }elsif(/^#TIMESTEPLIMIT\b/){
		my $do = <FILE>; # not time accurate if limiting time step
		check_var($Set{"IsTimeAccurate"},'F',$first) if $do =~ /^\s*T/;
	    }elsif(/^#(TIMESTEPPING|RUNGEKUTTA|RK)\b/){
		$nstage = <FILE>; $nstage =~ s/^\s*(\d+).*\n/$1/;
	    }elsif(/^#SCHEME\b/){
		my $norder = <FILE>;
		check_var($Set{"nOrder"}, $norder, $first);
		$nstage = $norder; # default nStage follows nOrder
		my $scheme = <FILE>;
		if($scheme =~ /^\s*Rusanov/i){
		    check_var($Set{"DoLf"}, "T", $first);
		}else{
		    check_var($Set{"DoLf"}, "F", $first);
		}
		if($norder > 1){
		    my $limiter = <FILE>;
		    if($limiter =~ /^\s*minmod/){
			check_var($Set{"LimiterBeta"}, "1.0", $first);
		    }else{
			my $beta = <FILE>;
			check_var($Set{"LimiterBeta"}, $beta, $first);
		    }
		}
	    }elsif(/^#(BODY|MAGNETOSPHERE)\b/){
		my $usebody = <FILE>;
		check_var($Set{"UseBody"}, $usebody, $first);
	    }elsif(/^#PLANET\b/){
		my $planet = <FILE>;
		check_var($Set{"UseBody"}, "F", $first) if $planet =~ /^NONE/;
		check_var($Set{"UseB0"}, "F", $first)
		    if $planet =~ /^\s*(NONE|VENUS)/i;
	    }elsif(/^#BORIS\b/){
		my $boris = <FILE>;
		check_var($Set{"UseBorisCorrection"}, $boris, $first);
		if($boris =~ /^T/){
		    my $borisfactor = <FILE>;
		    check_var($Set{"ClightFactor"}, $borisfactor, $first);
		}
	    }elsif(/^#GRIDGEOMETRY\b/){
		my $geometry = <FILE>;
		if($geometry =~ /^cartesian/){
		    check_var($Set{"IsCartesian"}, "T", $first);
		    check_var($Set{"IsCartesianGrid"}, "T", $first);
		}elsif($geometry =~ /^rz/){
		    check_var($Set{"IsCartesian"}, "T", $first);
		    check_var($Set{"IsCartesianGrid"}, "F", $first);
		}else{
		    check_var($Set{"IsCartesian"}, "F", $first);
		    check_var($Set{"IsCartesianGrid"}, "F", $first);
		}
	    }elsif(/^#DIVB\b/){
		my $use8wave = <FILE>;
		check_var($Set{"UseDivbSource"}, $use8wave, $first);
	    }elsif(/^#ELECTRONENTROPY\b/){
		my $useentropy = <FILE>;
		check_var($Set{"UseElectronEntropy"}, $useentropy, $first);
	    }elsif(/^#COARSEAXIS\b/){
                my $usecoarseaxis = <FILE>;
		check_var($Set{"UseCoarseAxis"}, $usecoarseaxis, $first);
	    }elsif(/^#HYPERBOLICDIVB\b/){
		my $usehyp = <FILE>;
		check_var($Set{"UseHyperbolicDivB"}, $usehyp, $first);
	    }elsif(/^#NONCONSERVATIVE\b/){
		my $noncons = <FILE>;
		check_var($Set{"UseNonConservative"}, $noncons, $first);
	    }elsif(/^#CONSERVATIVECRITERIA\b/){
		my $ncrit = <FILE>; $ncrit =~ s/^\s*(\d+).*\n/$1/;
		check_var($Set{"nConservCrit"}, $ncrit, $first);
		check_var($Set{"UseNonConservative"}, "T", $first) if $ncrit;
	    }elsif(/^#USEB0\b/){
		my $useb0 = <FILE>;
		check_var($Set{"UseB0"}, $useb0, $first);
	    }elsif(/^#MINIMUMPRESSURE\b/){
		check_var($Set{"UsePMin"}, "T", $first);
	    }elsif(/^#MINIMUMDENSITY\b/){
		check_var($Set{"UseRhoMin"}, "T", $first);		
	    }elsif(/^#FIXEDTIMESTEP\b/){
		my $usedtfixed = <FILE>;
		check_var($Set{"UseDtFixed"}, $usedtfixed, $first);
	    }elsif(/^#COORD(INATE)?SYSTEM\b/){
		my $coor = <FILE>;
		$Set{"UseRotatingFrame"} = ".false." if $coor =~ /^HGI/i;
		$Set{"UseRotatingFrame"} = ".true."  if $coor =~ /^GEO|HGC|HGR/i;
	    }elsif(/^#GRAVITY^/){
		my $usegrav = <FILE>;
		check_var($Set{"UseGravity"}, $usegrav, $first);
	    }
	}
	close(FILE);

	foreach $name (sort keys %Set){
	    if($Set{$name} ne $Opt{$name}){
		$Change = 1;
		last;
	    }
	}
	if($Change){
	    foreach $name (sort keys %Opt){
		if($Set{$name} eq $Opt{$name}){
		    printf("%-30s = %s\n", $name, $Opt{$name});
		}else{
		    printf("%-30s = %-17s -> %s\n", $name, $Opt{$name}, $Set{$name});
		    $Opt{$name} = $Set{$name};
		}
	    }
	}
    }else{
	# -opt=none means that nothing is fixed
	if($Opt =~ s/^(any|none),?//i){
	    foreach $name (sort keys %Opt){
		next if $Opt{$name} eq 'any';
		printf "%-30s = %-17s -> any\n", "$name:", $Opt{$name};
		$Opt{$name} = 'any';
		$Change = 1;
	    }
	}

	# replace =F and =T with =.false. and =.true.
	$Opt =~ s/=F/=.false./g;
	$Opt =~ s/=T/=.true./g;

	my @Opt = split(/,/, $Opt);
	foreach (@Opt){
	    my $newvalue = "any";
	    $newvalue = $1 if s/=(.*)//;
	    my $oldvalue = $Opt{$_};
	    if(not $oldvalue){
		print "$ERROR Invalid variable name=$_\n";
		next;
	    }
	    if($newvalue ne $oldvalue){
		$Change = 1;
		# Print out modified value
		printf "%-30s = %-17s -> %s\n", $_, $oldvalue, $newvalue;
		$Opt{$_} = $newvalue;
	    }
	}
    }
    if(not $Change){
	# Nothing else to be done
	print "No changes in $OptFile\n";
	return;
    }
    
    # Edit the file
    print "Modifying $OptFile\n";
    my $parameters; # true inside setting parameters part
    my $checks;     # true inside checking parameters part
    @ARGV = ($OptFile);

    while(<>){
	# Change the Name => ... and NameOrig => lines
	if(/(\w+)Orig\s+=>/){
	    $name=$1;
	    if($Opt{$name} eq "any"){s/Orig//};
	}elsif(/(\w+)\s+=>/){
	    my $name = $1;
	    if($Opt{$name} ne "any" and $Opt{$name} ne ""){s/\s+=>/Orig =>/};
	}
	$parameters = 0 if /^\s*contains/;
	next if $parameters; # remove original parameters
	if(/Fixed values/){
	    print;
	    $parameters = 1;
	    foreach my $name (sort keys %Opt){
		my $value = $Opt{$name};
		next if $value eq 'any';
		my $type = 'real';
		$type = 'logical' if $name =~ /^(Is|Use|Do|Done)[A-Z]/;
		$type = 'integer' if $name =~ /^([i-n]|Int[A-Z])/;
		print "  $type, parameter:: $name = $value\n";
	    }
	    print "\n";
	    next;
	}
	$checks = 0 if /^\s+end subroutine check/;
	next if $checks; # remove original checks
	if(/Check fixed/){
	    print;
	    $checks = 1;
	    foreach my $name (sort keys %Opt){
		my $value = $Opt{$name};
		next if $value eq 'any';
		if($name =~ /^(Is|Use|Do|Done)[A-Z]/){
		    if($value eq ".true."){
			print "    if(.not. ${name}Orig) ".
			    "call CON_stop(NameSub// &\n\t ': $name=F')\n"
		    }elsif($value eq ".false."){
			print "    if(${name}Orig) ".
			    "call CON_stop(NameSub// &\n\t ': $name=T')\n"
		    }else{
			print "    if(${name}Orig .neqv. $value) ".
			    "call CON_stop(NameSub// &\n\t ': $name=', ${name}Orig)\n"
		    }
		}elsif($name ne 'iStage'){  # iStage cannot be checked
		    $value .= ' .and. nOrder > 1' if $name eq 'LimiterBeta';
		    print "    if(${name}Orig /= $value) ".
			"call CON_stop(NameSub// &\n\t ': $name=', ${name}Orig)\n";
		}
	    }
	    print "\n";
	    next;
	}
	print;
    }
}


#############################################################################

sub get_settings{

    # Make sure that BATL_size.f90 is up-to-date
    `cd srcBATL; make BATL_size.f90`;
    open(FILE, $NameBatlFile) or die "$ERROR could not open $NameBatlFile\n";
    while(<FILE>){
        next if /^\s*!/; # skip commented out lines
        $nI=$1           if /\bnI\s*=\s*(\d+)/i;
	$nJ=$1           if /\bnJ\s*=\s*(\d+)/i;
	$nK=$1           if /\bnK\s*=\s*(\d+)/i;
	$GhostCell=$1    if /\bnG\s*=\s*(\d)/;
    }
    close FILE;

    die "$ERROR could not read nI from $NameBatlFile\n" unless length($nI);
    die "$ERROR could not read nJ from $NameBatlFile\n" unless length($nJ);
    die "$ERROR could not read nK from $NameBatlFile\n" unless length($nK);
    die "$ERROR could not read nG from $NameBatlFile\n" 
	unless length($GhostCell);

    $GridSize = "$nI,$nJ,$nK";

}

#############################################################################

sub set_grid_size{

    $GridSize = $NewGridSize if $NewGridSize;

    if($GridSize =~ /^[1-9]\d*,[1-9]\d*,[1-9]\d*$/){
	($nI,$nJ,$nK) = split(',', $GridSize);
    }elsif($GridSize){
	die "$ERROR -g=$GridSize should be 3".
	    " positive integers separated with commas\n";
    }

    $GhostCell = $NewGhostCell if $NewGhostCell;
    die "$ERROR -ng=$GhostCell must be 2,3,4 or 5\n" if $GhostCell!~/^[2345]$/;

    # Check the grid size (to be set)
    die "$ERROR nK=$nK must be 1 if nJ is 1\n"         if $nJ==1 and $nK>1;
    die "$ERROR nI=$nI must be an even integer\n"      if           $nI%2!=0;
    die "$ERROR nJ=$nJ must be 1 or an even integer\n" if $nJ>1 and $nJ%2!=0;
    die "$ERROR nK=$nK must be 1 or an even integer\n" if $nK>1 and $nK%2!=0;

    if(not $Force){
	die "$ERROR nI=$nI nJ=$nJ nK=$nK does not allow AMR\n" 
	    if $nI == 2 or $nJ == 2 or $nK==2;
	die "$ERROR -ng=$GhostCell should not exceed nI/2=$nI/2\n"
	    if $GhostCell > $nI/2;
	die "$ERROR -ng=$GhostCell should not exceed nJ/2=$nJ/2\n"
	    if $GhostCell > $nJ/2 and $nJ>1;
	die "$ERROR -ng=$GhostCell should not exceed nK/2=$nK/2\n"
	    if $GhostCell > $nK/2 and $nK>1;
    }


    print "Writing new grid size $GridSize and $GhostCell ghost cells into ".
	"$NameBatlFile...\n";

    @ARGV = ($NameBatlFile);
    while(<>){
	if(/^\s*!/){print; next} # Skip commented out lines
	s/\b(nI\s*=[^0-9]*)(\d+)/$1$nI/i;
	s/\b(nJ\s*=[^0-9]*)(\d+)/$1$nJ/i;
	s/\b(nK\s*=[^0-9]*)(\d+)/$1$nK/i;
	s/\b(nG\s*=[^0-9]*)\d/$1$GhostCell/i;
	print;
    }

}

##############################################################################

sub set_equation{

    if($Equation eq '1'){
	my @EquationModules;
	chdir $SrcEquation;
	@EquationModules = sort(glob("ModEquation?*.f90"));
	for (@EquationModules){s/^ModEquation//; s/\.f90$//;}
	print "Available Equations:\n   ",join("\n   ",@EquationModules),"\n";
	chdir "..";
	return;
    }

    my $File = "$SrcEquation/ModEquation$Equation.f90";
    die "$ERROR File $File does not exist!\n" unless -f $File;

    # Check if there is any change in the equation module
    if(-f $EquationMod){
	open(FILE1,$File); open(FILE2,$EquationMod);
	my $IsSame = 1; my $line1; my $line2;
	while($line1=<FILE1> and $line2=<FILE2>){
	    # Ignore the nWave, nPui, and nMaterial definitions
	    next if $line1=~/nMaterial|nWave|nPui/ and $line2=~/nMaterial|nWave|nPui/;
	    if($line1 ne $line2){
		$IsSame = 0;
		last;
	    }
	}
	close(FILE1); close(FILE2);

	# Do not overwrite the equation module if there are no differences
	return if $IsSame;
    }

    `cp $EquationMod $EquationModSafe` if -f $EquationMod; # save previous eq.
    print "cp $File $EquationMod\n" if $Verbose;
    `cp $File $EquationMod`;

    # Add -DSCALAR compiler flag for ModEquationScalar
    @ARGV = ($MakefileConf);
    while (<>){
	s/^(CFLAG *= *)-DSCALAR */$1/;
	s/^(CFLAG *=) */$1 -DSCALAR / if $Equation eq 'Scalar';
	print;
    }
}

##############################################################################

sub set_nwave{

    $nWave = $nWaveNew;

    print "Writing new nWave = $nWaveNew into $EquationMod...\n";

    my $nWaveTwo = sprintf("%02d", $nWaveNew);

    @ARGV = ($EquationMod);

    my $prev;
    while(<>){
        if(/^\s*!/){print; next} # Skip commented out lines
        if(m/\&\s*\n/){         # Concatenate continuation lines
            $prev .= $_;
            next;
        }
        $_ = $prev . $_;
	$prev = "";
        s/\b(nWave\s*=[^0-9]*)(\d+)/$1$nWaveNew/i;
        s/I\([^\)]+\)/I($nWaveTwo)/m if /NamePrimitiveVar\s*\=/;
        print;
    }
}

##############################################################################

sub set_npui{

    $nPui = $nPuiNew;

    print "Writing new nPui = $nPuiNew into $EquationMod...\n";

    @ARGV = ($EquationMod);

    my $prev;
    while(<>){
        if(/^\s*!/){print; next} # Skip commented out lines
        if(m/\&\s*\n/){         # Concatenate continuation lines
            $prev .= $_;
            next;
        }
        $_ = $prev . $_;
	$prev = "";
        s/\b(nPui\s*=[^0-9]*)(\d+)/$1$nPuiNew/i;
        s/F\([^\)]+\)/F($nPuiNew)/m if /NamePrimitiveVar\s*\=/;
        print;
    }
}

#############################################################################

sub set_nmaterial{

    $nMaterial = $nMaterialNew;

    print "Writing new nMaterial = $nMaterialNew into $EquationMod...\n";

    @ARGV = ($EquationMod);

    my $prev;
    while(<>){
        if(/^\s*!/){print; next} # Skip commented out lines
        if(m/\&\s*\n/){         # Concatenate continuation lines
            $prev .= $_;
            next;
        }
        $_ = $prev . $_;
	$prev = "";
        s/\b(nMaterial\s*=[^0-9]*)(\d+)/$1$nMaterialNew/i;
        s/M\([^\)]+\)/M($nMaterialNew)/m if /NamePrimitiveVar\s*\=/;
        print;
    }
}

##############################################################################

sub set_charge_state{

    my @ValidChargeState = ('h','he','li','be','b','c','n','o','f','ne','na',
			    'mg','al','si','p','s','cl','ar','k','ca','sc',
			    'ti','v','cr','mn','fe','co','ni','cu','zn');

    my @ValidChargeStateAll = (2..31);

    my @MassElement = (1.0, 4.003, 6.94, 9.012, 10.81, 12.011, 14.007, 15.999,
		       18.998, 20.18, 22.99, 24.305, 26.982, 28.085, 30.974,
		       32.06, 35.45, 39.948, 39.098, 40.078, 44.956, 47.867,
		       50.942, 51.996, 54.938, 55.845, 58.933, 58.693, 63.546,
		       65.38);

    # Separate input into array
    my @ChargeStateIn;
    @ChargeStateIn = split('\+', $ChargeStateNew);

    # Get valid element names in correct order
    my @Intersection = ();
    foreach my $elem_j (@ValidChargeState) {
	foreach my $elem_i (@ChargeStateIn) {
	    if($elem_i eq $elem_j){
		push @Intersection, $elem_i;
	    }
	}
    }
    
    # Get rid of duplicates
    my %seen = ();
    my @Element_I = ();
    foreach my $elem (@Intersection) {
	next if $seen{$elem}++;
	push @Element_I,$elem;
    }
    my $nElement = @Element_I;

    # Get Z+1 for each element
    my $cs = 1;
    my @nChargeState_I = ();
    my @MassElement_I = ();
    foreach my $elem_j (@ValidChargeState) {
	$cs = $cs+1;
	foreach my $elem_i (@Element_I) {
	    if($elem_i eq $elem_j){
		push @nChargeState_I, $cs;
		push @MassElement_I, $MassElement[$cs-2];
	    }
	}
    }

    # Sum of all charge states
    my $nChargeStateAll;
    foreach (@nChargeState_I){$nChargeStateAll += $_};

    # Convert array to string
    my $nChargeState_I = join(",", @nChargeState_I);
    $nChargeState_I = " \[$nChargeState_I\]";

    my $MassElement_I = join(",",@MassElement_I);
    $MassElement_I = " \[$MassElement_I\]";
    
    # Element names have length 2, convert array to string
    foreach (@Element_I){
	s/^(.)$/$1 /; $_ = "'$_'";
    }
    my $NameElement_I = join(",",@Element_I);
    $NameElement_I = " \[$NameElement_I\]";

    # Send variables to ModEquation file
    @ARGV = ($EquationMod);
    my $prev;
    while(<>){
        if(/^\s*!/){print; next} # Skip commented out lines
        if(m/\&\s*\n/){         # Concatenate continuation lines
            $prev .= $_;
            next;
        }
        $_ = $prev . $_;
	$prev = "";
        s/\b(nChargeStateAll\s*=[^0-9]*)(\d+)/$1$nChargeStateAll/i;
	s/\b(nElement\s*=[^0-9]*)(\d+)/$1$nElement/i;
	s/\b(nChargeState_I\(1:nElement\)\s*=)(.*)/$1$nChargeState_I/i;
	s/\b(NameElement_I\(1:nElement\)\s*=)(.*)/$1$NameElement_I/i;
	s/\b(MassElement_I\(1:nElement\)\s*=)(.*)/$1$MassElement_I/i;
        print;
    }
    print "Set charge state in $EquationMod\n" if $Verbose;
}   

#############################################################################

sub set_user_module{

    if($UserModule eq '1'){
	my @UserModules;
	chdir $SrcUser;
	@UserModules = glob("*.f90");
	chdir "..";
	if(-d $SrcUserExtra){
	    chdir $SrcUserExtra;
	    push @UserModules, glob("*.f90");
	    chdir "..";
	}
	@UserModules = ("Default", sort @UserModules);
	for (@UserModules){s/^ModUser//; s/\.f90$//;}
	print "Available User Modules:\n   ",join("\n   ",@UserModules),"\n";
	return;
    }

    my $File;
    if($UserModule eq "Default"){
	$File = "$Src/ModUserDefault.f90";
    }else{
	# Search in srcUser and srcUserExtra;
	$File = "$SrcUser/ModUser$UserModule.f90";
	$File = "$SrcUserExtra/ModUser$UserModule.f90" 
	    if -d $SrcUserExtra and not -f $File;
    }
    die "$ERROR File $File does not exist!\n" unless -f $File;
    return if -f $UserMod and not `diff $File $UserMod`;
    `cp $UserMod $UserModSafe` if -f $UserMod; # save previous user module
    print "cp $File $UserMod\n" if $Verbose;
    `cp $File $UserMod`;
}

#############################################################################

sub current_settings{

    $Settings  = "Number of cells in a block  : nI=$nI, nJ=$nJ, nK=$nK\n";
    $Settings .= "Number of ghost cell layers : nG=$GhostCell\n";

    open(FILE, $UserMod) or die "$ERROR Could not open $UserMod\n";
    my $Module='???';
    my $NameFile='???';
    while(<FILE>){
	if(/NameUserModule/){
	    $Module = $';
	    $Module = <FILE> if $Module =~ /\&\s*$/; # read continuation line
	    $Module =~ s/\s*=\s*//;   # remove equal sign
	    $Module =~ s/^\s*[\'\"]//;   # remove leading quotation mark
	    $Module =~ s/[\'\"].*\n//;  # remove trailing quotation marks
	}
        $NameFile = $1 if /NameUserFile\s*=.*ModUser(.*)\.f90/;
    }
    close(FILE);
    $Settings .= "UserModule = $NameFile: $Module\n";

    open(FILE, $EquationMod) or die "$ERROR Could not open $EquationMod\n";
    my $Equation='???';
    my $NameFile='???';
    my $prev;
    my %Value;

    while(<FILE>){
	next if /^\s*!/; # skip commented out lines
	if(s/\&\s*\n//){ # concatenate continuation lines
	    $prev .= $_;
	    next;
	}
	$_ = $prev . $_;
	$prev = "";

	# Extract variable settings: var = integer, var = othervar
	$Value{"$1"} = $2 while s/(\w+)\s*=\s*(\d+)//m;
	$Value{"$1"} = $Value{"$2"} while s/(\w+)\s*=\s*(\w+)//m;

	# Extract descriptions
        $NameFile = $1 if /NameEquationFile\s*=.*ModEquation(.*)\.f90/;
	$Equation = $1 if /NameEquation\s*=\s*[\'\"]([^\'\"]*)/;

	# Charge states element list
	$ChargeState = $1 if /NameElement_I\(1:nElement\)\s*=\s*(.*)/;	
    }
    # Convert from [' h', ' o'] to h+o
    $ChargeState =~ tr/,\[\]\' /+/d;

    if($Verbose){
	print "$_ = $Value{$_}\n" foreach (sort keys %Value);
    }

    $Settings .= "Equation   = $NameFile: $Equation\n";

    my $nVar     = $Value{"nVar"};
    my $nSpecies;
    $nSpecies = $Value{"SpeciesLast_"} - $Value{"SpeciesFirst_"} + 1
	if $Value{"SpeciesLast_"} and $Value{"SpeciesFirst_"};
    my $nIonFluid = ($Value{"nIonFluid"} or 1);
    my $nFluid    = ($Value{"nFluid"} or 1);
    my $nNeutralFluid = $nFluid - $nIonFluid;

    $Settings .= "Charge states of elements   : $ChargeState\n"
	if $ChargeState;
    $Settings .= "Number of state variables   : nVar=$nVar\n";
    $Settings .= "Number of species           : nSpecies=$nSpecies\n" 
	if $nSpecies;
    $Settings .= "Number of wave bins         : nWave=$nWave\n" if $nWave;
    $Settings .= "Number of PUI bins          : nPui=$nPui\n"   if $nPui;
    $Settings .= "Number of materials         : nMaterial=$nMaterial\n" 
	if $nMaterial;
    $Settings .= "Number of ion fluids        : nIonFluid=$nIonFluid\n"
	if $nIonFluid != 1;
    $Settings .= "Number of neutral fluids    : nNeutralFluid=$nNeutralFluid\n"
	if $nNeutralFluid;
    $Settings .= "Number of fluids            : nFluid=$nFluid\n"
	if $nFluid > 1;
    $Settings .= "Electron pressure           : UsePe=1\n" if $Value{"Pe_"}>1;
    $Settings .= "Parallel pressure           : UsePpar=1\n" if $Value{"Ppar_"}>1;
    
    $Settings; # return value
}

#############################################################################

sub print_help{

    print "
Additional options for BATSRUS/Config.pl:

-g=NI,NJ,NK     Set grid block size. NI, NJ and NK are the number of cells 
                in the I, J and K directions, respectively. 
                If NK = 1, the 3rd dimension is ignored: 2D grid.
                If NJ=NK=1, the 2nd and 3rd dimensions are ignored: 1D grid.
                In non-ignored dimensions NI, NJ, NK have to be even integers.
                To allow AMR, the number of cells has to be 4 or more in all 
                non-ignored directions. Use -f to set the value 2 (no AMR). 

-ng             Print current setting for number of ghost cell layers.

-ng=NG          Set number of ghost cell layers to NG around grid blocks. 
                The value of NG can be 2, 3, 4 or 5, but  
                not more than NI/2, NJ/2 (if NJ>1), and NK/2 (if NK>1).
                Default value is NG=2.

-f              Force the -g and -ng settings.

-e              List all available equation modules.

-e=EQUATION     Select equation EQUATION. 

-u              List all available user modules.

-u=USERMODULE   Select the user module USERMODULE. 

-opt            Show current parameter settings for optimized code.
                Variables without a value are adjustable from PARAM.in.

-opt=STRING     Set parameters for optimized code. STRING can be a parameter
		file name that is analyzed for settings. Otherwise STRING is a 
		comma separated list consisting of NAME or NAME=any elements 
		for adjustable parameters, or NAME=VALUE for fixed parameters.
		For logical variables, T and F can be used as VALUE. If 
		STRING starts with 'any' or 'none', all values are adjustable.

-nWave=NWAVE    Set the number of wave bins used for radiation or wave
                turbulence to NWAVE for the selected EQUATION module.

-nPui=NPUI      Set the number of PUI bins to NPUI for the selected
                EQUATION module.

-nMaterial=NM   Set the number of material levels to NM
                for the selected EQUATION module.

-cs=ELEMENT_I   Set the + delimited list of element names for charge 
                state calculation.
                Only the 30 first elements of the periodic table are possible.
                Works with equation module AwsomChargeState in IH and SC 
                components only.

Examples for BATSRUS/Config.pl:

List available options for equations and user modules and show optimizations:

    Config.pl -e -u -opt

Select the MHD equations, the Default user module and optimize some parameters:

    Config.pl -e=MHD -u=Default -opt=none,nOrder=2,nStage=2,IsCartesian=T

Select the CRASH equation and user modules and optimize for run/PARAM.in file.
Set the number of materials to 5 and number of radiation groups to 30:

    Config.pl -e=Crash -u=Crash -opt=run/PARAM.in -nMaterial=5 -nWave=30

Set list of elements for charge state calculation

    Config.pl -cs=o+c+fe+mg

Set block size to 8x8x8, number of blocks to 400 and implicit blocks to 100
and number of ghost cells to 2:

    Config.pl -g=8,8,8,400,100 -ng=2

Set number of blocks as a function of number of processors in Makefile.test

    Config.pl -g=8,8,8,3000/\${NP},1

Show settings for BATSRUS:

    Config.pl -s
\n";
    exit 0;


}

