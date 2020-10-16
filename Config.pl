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
my $remote = `git config remote.origin.url`; $remote =~ s/\/BATSRUS.git\n//;
my $umichgitlab = ($remote eq "git\@gitlab.umich.edu:swmf_software");

my $config   = "share/Scripts/Config.pl";
my $gitclone;
if($umichgitlab){
    $gitclone = "share/Scripts/gitclone -s";
}else{
    $gitclone = "share/Scripts/githubclone";
}

# Git clone missing directories as needed. Start with share/ to get $gitclone.
if (not -f $config and not -f "../../$config"){
    `git clone $remote/share; git clone $remote/util`;
}
# The component ID is hidden from Rename.pl
if ($Component eq "G"."M"){
    print "$remote\n";
    print "$gitclone\n";
    `$gitclone srcBATL` if not -d "srcBATL";
    `$gitclone srcUserExtra` if not -d "srcUserExtra" and $umichgitlab;
}

if(-f $config){
    require $config;
}else{
    require "../../$config";
}

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
my $NameBatlFile = "srcBATL/BATL_size.f90";
my $GridSize;
my $GhostCell;
my ($nI, $nJ, $nK, $nG);

# additional variable information
my $nWave;
my $nWaveNew;
my $nMaterial;
my $nMaterialNew;
my $ChargeState;
my $nChargeStateAll;

# For SC/BATSRUS and IH/BATSRUS src/ is created during configuration of SWMF
if(not -d $Src){exit 0};

# Read previous grid size, equation and user module
&get_settings;

foreach (@Arguments){
    if(/^-f$/)                {$Force=1;                       next};
    if(/^-e$/)                {$Equation=1;                    next};
    if(/^-u$/)                {$UserModule=1;                  next};
    if(/^-e=(.*)$/)           {$Equation=$1;                   next};
    if(/^-u=(.*)$/)           {$UserModule=$1;                 next};
    if(/^-s$/)                {$Show=1;                        next};
    if(/^-nWave=(.*)$/i)      {
	# Check the number of wave bins (to be set)
	die "$ERROR nWave=$1 must be 1 or more\n" if $1 < 1;
	$nWaveNew=$1;
	next};
    if(/^-nMaterial=(.*)$/i)  {
	# Check the number of material level indices (to be set)
	die "$ERROR nMaterial=$1 must be 1 or more\n" if $1 < 1;
	$nMaterialNew=$1;
	next};
    if(/^-cs=(.*)/)           {$ChargeState.="$1";            next};
    if(/^-ng$/i)              {print "ng=$GhostCell\n"; next};
    if(/^-ng=(.*)$/i)         {$NewGhostCell=$1; next};
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
    $nMaterial=$1    if /\bnMaterial\s*=\s*(\d+)/i;
    $nChargeStateAll=$1  if /\bnChargeStateAll\s*=\s*(\d+)/i;
}
close FILE;
die "$ERROR nWave was not found in equation module\n" if $nWaveNew and not $nWave;
&set_nwave     if $nWaveNew and $nWaveNew ne $nWave;
die "$ERROR nMaterial was not found in equation module\n" if $nMaterialNew and not $nMaterial;
&set_nmaterial if $nMaterialNew and $nMaterialNew ne $nMaterial;

die "$ERROR nChargeStateAll was not found in equation module\n" if $ChargeState and not $nChargeStateAll;
&set_charge_state if $ChargeState;

# Set or list the user modules
&set_user_module if $UserModule;

my $Settings = &current_settings; print $Settings if $Show;

# (Re)Create Makefile.RULES file(s) based on current settings
&create_makefile_rules($Settings);

exit 0;

#############################################################################

sub get_settings{

    # Make sure that BATL_size.f90 is up-to-date
    `make $NameBatlFile`;
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
    die "$ERROR -ng=$GhostCell must be 2,3,4 or 5\n" if $GhostCell!~/^[2345]/;

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
	    # Ignore the nWave and nMaterial definitions
	    next if $line1=~/nMaterial|nWave/ and $line2=~/nMaterial|nWave/;
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
    
    # Separate input into array
    my @ChargeStateIn;
    @ChargeStateIn = split(',',$ChargeState);

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
    foreach my $elem_j (@ValidChargeState) {
	$cs = $cs+1;
	foreach my $elem_i (@Element_I) {
	    if($elem_i eq $elem_j){
		push @nChargeState_I, $cs;
	    }
	}
    }

    # Sum of all charge states
    my $nChargeStateAll = 0;
    foreach (@nChargeState_I){
	$nChargeStateAll += $_;
    }

    # Convert array to string
    my $nChargeState_I=join(",",@nChargeState_I);
    $nChargeState_I = " \[$nChargeState_I\]";

    # Element names have length 2, convert array to string
    foreach my $elem_k (@Element_I){
	if(length($elem_k) == 1) {
	    $elem_k = $elem_k . " ";
	}
    }
    foreach (@Element_I) {$_ = "'$_'";}
    my $NameElement_I=join(",",@Element_I);
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
        print;
    }
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
    my $Version='???';
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
	$Version = $1 if /VersionUserModule\s*=\s*([\d\.]+)/;
    }
    close(FILE);
    $Settings .= "UserModule = $NameFile: $Module, ver $Version\n";

    open(FILE, $EquationMod) or die "$ERROR Could not open $EquationMod\n";
    my $Equation='???';
    my $NameFile='???';
    my $prev;
    my %Value;

    my $ChargeStateList;
    
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
	$ChargeStateList = $1 if /NameElement_I\(1:nElement\)\s*=\s*(.*)/;	
    }
    if($Verbose){
	print "$_ = $Value{$_}\n" foreach (sort keys %Value);
    }

    $Settings .= "Equation   = $NameFile: $Equation\n";

    my $nVar     = $Value{"nVar"};
    my $nSpecies;
    $nSpecies = $Value{"SpeciesLast_"} - $Value{"SpeciesFirst_"} + 1
	if $Value{"SpeciesLast_"} and $Value{"SpeciesFirst_"};
    my $nIonFluid = $Value{"IonLast_"} - $Value{"IonFirst_"} + 1;
    my $nFluid    = $Value{"nFluid"}   - $Value{"IonFirst_"} + 1;
    my $nNeutralFluid;
    $nNeutralFluid = $nFluid - $Value{"IonLast_"} if $Value{"IonLast_"};

    $Settings .= "Charge states of elements   : $ChargeStateList\n" if
	$nChargeStateAll;
    
    $Settings .= "Number of state variables   : nVar=$nVar\n";
    $Settings .= "Number of species           : nSpecies=$nSpecies\n" 
	if $nSpecies;
    $Settings .= "Number of wave bins         : nWave=$nWave\n" if $nWave;
    $Settings .= "Number of materials         : nMaterial=$nMaterial\n" 
	if $nMaterial;
    $Settings .= "Number of ion fluids        : nIonFluid=$nIonFluid\n"
	if $nIonFluid != 1;
    $Settings .= "Number of neutral fluids    : nNeutralFluid=$nNeutralFluid\n"
	if $nNeutralFluid;
    $Settings .= "Number of fluids            : nFluid=$nFluid\n"
	if $nFluid > 1;
    $Settings .= "Electron pressure           : UsePe=1\n" if $Value{"Pe_"}>1;
    
    $Settings; # return value
}

#############################################################################

sub print_help{

    print "
Additional options for BATSRUS/Config.pl:

-g=NI,NJ,NK
                Set grid size. NI, NJ and NK are the number of cells 
                in the I, J and K directions, respectively. 
                If NK = 1, the 3rd dimension is ignored: 2D grid.
                If NJ=NK=1, the 2nd and 3rd dimensions are ignored: 1D grid.
                In non-ignored dimensions NI, NJ, NK have to be even integers.
                To allow AMR, the number of cells has to be 4 or more in all 
                non-ignored directions. 

-ng             Print current setting for number of ghost cell layers.

-ng=GHOSTCELL   Set number of ghost cell layers around grid blocks. 
                The value GHOSTCELL has to be 2, 3, 4 or 5, but  
                not more than NI/2, NJ/2 (if NJ>1), and NK/2 (if NK>1).
                Default value is GHOSTCELL=2.

-f              Force the -g and -ng settings even if GHOSTCELL 
                exceeds NI/2, NJ/2 (if NJ>1), NK/2 (if NK>1) limits.

-e              List all available equation modules.

-e=EQUATION     Select equation EQUATION. 

-u              List all the available user modules.

-u=USERMODULE   Select the user module USERMODULE. 

-nWave=NWAVE
                Set the number of wave bins used for radiation or wave
                turbulence to NWAVE for the selected EQUATION module.

-nMaterial=NMATERIAL
                Set the number of material levels to NMATERIAL
                for the selected EQUATION module.

-cs=ELEMENT_I
                Set the comma delimited list of element names for charge 
                state calculation.
                Only the 30 first elements of the periodic table are possible.
                Works with equation module AwsomChargeState in IH and SC 
                components only.

Examples for BATSRUS/Config.pl:

List available options for equations and user modules:

    Config.pl -e -u

Select the MHD equations, the Default user module:

    Config.pl -e=MHD -u=Default

Select the CRASH equation and user modules and 
set the number of materials to 5 and number of radiation groups to 30:

    Config.pl -e=Crash -u=Crash -nMaterial=5 -nWave=30

Set list of elements for charge state calculation

    Config.pl -cs=o,c,fe,mg

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

