#
# List the default target first for stand alone mode
#
DEFAULT_TARGET = BATSRUS
DEFAULT_EXE    = ${DEFAULT_TARGET}.exe

default : ${DEFAULT_TARGET}

include Makefile.def
include Makefile.conf

# Nothing should be done parallel in this Makefile
.NOTPARALLEL:

#
# Menu of make options
#
help:
	@echo ' '
	@echo '  You can "make" the following:'
	@echo ' '
	@echo '    <default> ${DEFAULT_TARGET} in stand alone mode, help in SWMF'
	@echo ' '
	@echo '    help         (makefile option list)'
	@echo '    install      (install BATSRUS)'
	@#^CFG IF DOC BEGIN
	@#	^CFG IF NOT REMOVEDOCTEX BEGIN
	@echo ' '
	@echo '    PDF          (Make PDF version of the documentation)'
	@#	^CFG END REMOVEDOCTEX
	@#^CFG END DOC
	@#^CFG IF TESTING BEGIN
	@echo '    test         (run all tests for BATSRUS)'
	@echo '    test_help    (show all options for running the tests)'
	@#^CFG END TESTING
	@echo ' '
	@echo '    LIB     (Component library libGM for SWMF)'
	@echo '    BATSRUS (Block Adaptive Tree Solar-Wind Roe Upwind Scheme)'
	@echo '    CRASH   (Code for Radiative Shock Hydrodynamics)'
	@echo '    NOMPI   (NOMPI library for compilation without MPI)'
	@echo '    PIDL    (PostIDL.exe creates 1 .out file from local .idl files)'
	@echo '    INTERPOLATE (INTERPOLATE.exe interpolates points from *.outs movies)'
	@echo '    SNAPSHOT   (SNAPSHOT.exe extract snapshots from *.outs movies)'
	@echo '    SPECTRUM   (SPECTRUM.exe calculates spectra from solar corona output on regular grid)'
	@echo '    EARTH_TRAJ (EARTH_TRAJ.exe creates Earth trajectory file for heliosphere)'
	@echo '    TIME_CONV  (TIME_CONV.exe converts the Carrington time to the usual one)'
	@echo ' '
	@echo '    rundir      (create run directory for standalone or SWMF)'
	@echo '    rundir RUNDIR=run_test (create run directory run_test)'
	@echo ' '
	@echo "    serialrun    (make and run ${DEFAULT_EXE} on 1 PE)"
	@echo "    parallelrun  (make and ${MPIRUN} ${DEFAULT_EXE})"
	@echo "    parallelrun NP=7 RUNDIR=run_test (cd run_test; ${PARALLEL} ${NPFLAG} 7 ${DEFAULT_EXE})"
	@echo ' '	
	@echo '    clean     (remove temp files like: *~ *.o *.kmo *.mod *.T *.lst core)'
	@echo '    distclean (equivalent to ./Config.pl -uninstall)'
	@echo '    dist      (create source distribution tar file)'

info:
	@echo "Total lines of Fortran: `wc -l src*/*.f* share/Library/src/*.f* | tail -1`"

install: srcBATL/BATL_size.f90
	cd src; make install

srcBATL/BATL_size.f90: srcBATL/BATL_size_orig.f90
	cp -f srcBATL/BATL_size_orig.f90 srcBATL/BATL_size.f90

LIB:
	cd srcBATL; $(MAKE) LIB
	cd src; $(MAKE) LIB
	cd srcInterface; $(MAKE) LIB

GITINFO:
	@(if [ "${GITINFO}" != "NO" ]; then \
		${SCRIPTDIR}/gitall -r=f > src/show_git_info.h; \
	fi)

BATSRUS: GITINFO
	cd ${SHAREDIR}; $(MAKE) LIB
	cd ${TIMINGDIR}; $(MAKE) LIB
	if [[ "${MPILIB}" == *lNOMPI*  ]]; then make NOMPI; fi
	cd ${MAGNETOGRAMDIR}; $(MAKE) LIB
	cd ${DEMTDIR}; $(MAKE) LIB
	cd ${EMPIRICALEEDIR}; $(MAKE) LIB
	cd ${EMPIRICALSCDIR}; $(MAKE) LIB
	cd srcBATL; $(MAKE) LIB
	cd src; $(MAKE) LIB
	cd src; make BATSRUS

CRASH: GITINFO
	cd ${SHAREDIR}; $(MAKE) LIB
	cd ${TIMINGDIR}; $(MAKE) LIB
	cd ${MAGNETOGRAMDIR}; $(MAKE) LIB
	cd ${DEMTDIR}; $(MAKE) LIB
	cd ${EMPIRICALEEDIR}; $(MAKE) LIB
	cd ${EMPIRICALSCDIR}; $(MAKE) LIB
	cd ${CRASHDIR}; $(MAKE) LIB
	cd srcBATL; $(MAKE) LIB
	cd src; $(MAKE) LIB
	cd src; make CRASH

NOMPI:
	cd ${NOMPIDIR}; $(MAKE) LIB

PIDL:
	cd ${SHAREDIR}; $(MAKE) PIDL
	@echo ' '
	@echo Program PostIDL has been brought up to date.
	@echo ' '

INTERPOLATE:
	cd ${SHAREDIR}; $(MAKE) LIB
	cd srcPostProc; make INTERPOLATE
	@echo ' '
	@echo Program INTERPOLATE has been brought up to date.
	@echo ' '

SNAPSHOT:
	cd ${SHAREDIR}; $(MAKE) LIB
	cd srcPostProc; make SNAPSHOT
	@echo ' '
	@echo Program SNAPSHOT has been brought up to date.
	@echo ' '

SPECTRUM:
	cd ${SHAREDIR}; $(MAKE) LIB
	cd srcPostProc; make SPECTRUM
	@echo ' '
	@echo Program SPECTRUM has been brought up to date.
	@echo ' '

EARTH_TRAJ:
	cd srcPostProc; $(MAKE) EARTH_TRAJ
	@echo ' '
	@echo Program EARTH_TRAJ has been brought up to date.
	@echo ' '

TIME_CONV:
	cd srcPostProc; $(MAKE) TIME_CONV
	@echo ' '
	@echo Program TIME_CONV has been brought up to date.
	@echo ' '

# Default component
COMPONENT = GM

rundir:
	mkdir -p ${RUNDIR}/${COMPONENT}
	cd ${RUNDIR}/${COMPONENT}; \
		mkdir restartIN restartOUT IO2; \
		ln -s IO2 plots; \
		ln -s ${BINDIR}/PostIDL.exe .; \
		cp    ${SCRIPTDIR}/pIDL .; \
		cp    ${GMDIR}/Scripts/TEC/pTEC .; \
		ln -s ${GMDIR}/Param .; \
		ln -s ${GMDIR}/data/TRAJECTORY .
	@(if [ "$(STANDALONE)" != "NO" ]; then \
		touch ${DIR}/share/JobScripts/job._TMP_${MACHINE}; \
		touch ${DIR}/share/JobScripts/_TMP_.${MACHINE}.pl; \
		cp ${DIR}/share/JobScripts/job.*${MACHINE}* ${RUNDIR}/; \
		cp ${DIR}/share/JobScripts/*.${MACHINE}*.[ps][lh] ${RUNDIR}/; \
		rm -f ${RUNDIR}/*_TMP_* ${DIR}/share/JobScripts/*_TMP_*; \
		cp -f Param/EARTH/PARAM.in ${RUNDIR}/PARAM.in; \
		touch ${RUNDIR}/core; chmod 444 ${RUNDIR}/core; \
		cp ${SCRIPTDIR}/PostProc.pl ${RUNDIR}/; \
		cp ${SCRIPTDIR}/Restart.pl  ${RUNDIR}/; \
		cp ${SCRIPTDIR}/Resubmit.pl ${RUNDIR}/; \
		cd ${RUNDIR}; ln -s ${BINDIR}/${DEFAULT_EXE} .; \
		ln -s ${COMPONENT}/* .; \
		rm -f plots; ln -s ${COMPONENT}/IO2 plots; \
	fi);

rundir_rh:
	$(MAKE) rundir RUNDIR=run STANDALONE=YES GMDIR=`pwd`
	cd run; ln -s ${BINDIR}/CRASH.exe .; \
	cp -f Param/CRASH/PARAM.in .                          
#
#       Run the default code on NP processors
#

parallelrun: ${DEFAULT_TARGET}
	cd ${RUNDIR}; ${MPIRUN} ./${DEFAULT_EXE}

serialrun: ${DEFAULT_TARGET}
	cd ${RUNDIR}; ${SERIAL} ./${DEFAULT_EXE}

#					^CFG IF DOC BEGIN
#	Create the documentation files      ^CFG IF NOT REMOVEDOCTEX BEGIN
#	
PDF:
	@cd Doc/Tex; $(MAKE) cleanpdf; $(MAKE) PDF

CLEAN1 = cleanpdf #				^CFG IF NOT MAKEPDF

#					    ^CFG END REMOVEDOCTEX
#					^CFG END DOC

#
# Cleaning
#

clean:
	cd src; $(MAKE) clean
	cd srcBATL; $(MAKE) clean
	cd srcInterface; $(MAKE) clean
	cd srcPostProc;  $(MAKE) clean
	@(if [ -d util  ]; then cd util;  $(MAKE) clean; fi);
	@(if [ -d share ]; then cd share; $(MAKE) clean; fi);

distclean: 
	./Config.pl -uninstall

allclean:
	cd src; $(MAKE) distclean
	cd srcBATL; $(MAKE) distclean
	cd srcInterface; $(MAKE) distclean
	cd srcPostProc;  $(MAKE) distclean
	@				#^CFG IF DOC BEGIN
	@					#^CFG IF NOT REMOVEDOCTEX BEGIN
	cd Doc/Tex; $(MAKE) clean ${CLEAN1}
	@					#^CFG END REMOVEDOCTEX
	@				#^CFG END DOC

dist:
	./Config.pl -uninstall
	@echo ' '
	@echo ' NOTE: All "run" or other created directories not included!'
	@echo ' '
	tar -cf tmp.tar  Makefile Makefile.test
	tar -rf tmp.tar  README.md PARAM.XML LICENSE.txt
	tar -rf tmp.tar  Configure.options	#^CFG IF CONFIGURE
	tar -rf tmp.tar  Configure.pl		#^CFG IF CONFIGURE
	tar -rf tmp.tar  Test*.pl		#^CFG IF TESTING
	tar -rf tmp.tar  Doc			#^CFG IF DOC
	tar -rf tmp.tar  Config.pl
	tar -rf tmp.tar  Param
	tar -rf tmp.tar  Scripts
	tar -rf tmp.tar  src srcBATL srcUser srcEquation srcInterface srcPostProc
	@(if [ -d util  ]; then tar -rf tmp.tar util; fi);
	@(if [ -d share ]; then tar -rf tmp.tar share; fi);
	@echo ' '
	gzip tmp.tar
	mv tmp.tar.gz BATSRUS_v${VERSION}_`date +%Y%b%d_%H%M.tgz`
	@echo ' '
	@ls -l BATSRUS_v*.tgz

include Makefile.test #^CFG IF TESTING
