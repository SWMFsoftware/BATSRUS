!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
program earth_traj

  ! earth_traj
  !
  ! Generation of the earth trajectoy file for input by SWMF
  ! as a thajectory of a satellite in the inner heliosphere.
  !
  ! The tragectory comprises the earth locations in HAE, HGI,
  ! or HGR coordinate system.
  !
  ! The input parameters are read from EARTH_TRAJ.in, and the result is
  ! written into STDOUT.
  !

  use CON_geopack_internal
  use ModUT
  use ModNumConst
  use ModTimeConvert, ONLY: time_int_to_real, time_real_to_int
  use ModConst

  implicit none

  real, parameter :: cAU2rSun = cAU/Rsun

  real :: Xyz_D(3), Aux
  real :: StartTimeRel=0.0, EndTimeRel=0.0, TimeStep=0.0
  real :: StartTime, EndTime, CurrentTime
  character(len=32) :: NameCommand
  character(len=32) :: NameFile
  character(len=3)  :: CoordSys
  integer, dimension(7) :: iStartTime, iCT
  integer :: i, iFile
  integer :: oUnit
  !----------------------------------------------------------------------------

  !
  ! Parse the command line arguments
  !
  iFile = 10
  NameFile='EARTH_TRAJ.in'
  open(iFile,file=NameFile,status='old')
  READPARAM: do
     read(iFile,'(a)',end=9000,err=9000) NameCommand
     if(index(NameCommand,'#') /= 1)NameCommand=''
     select case(NameCommand)
     case("#COOR")
        read(iFile,*,end=9000,err=9000) CoordSys
     case("#INCLUDE")
        read(iFile,*,end=9000,err=9000) NameFile
        iFile = iFile + 1
        open(iFile,file=trim(NameFile),status='old')
     case("#DATE","#STARTTIME")
        do i = 1, 6
           read(iFile,*,end=9000,err=9000) iStartTime(i)
        end do
        read(iFile,*,end=9000,err=9000) Aux  ! Auxiliary
        iStartTime(7) = 0
     case("#TIMELOOP")
        read(iFile,*,end=9000,err=9000) StartTimeRel, EndTimeRel, TimeStep
     case("#START","#END")
        if(iFile>10) then
           close(iFile)
           iFile = iFile-1
        else
           EXIT READPARAM
        end if
     case default
        if (len_trim(NameCommand) /= 0) then  ! i.e., just skip an empty line
           write(*,*) &
                '+++ Wrong command "'//trim(NameCommand)//'" encountered +++'
           stop
        end if
     end select
  end do READPARAM
  close(10)

  !
  ! Check correctness of arguments
  !
  if ((CoordSys /= 'HAE') .and. (CoordSys /= 'HGI') .and. (CoordSys /= 'HGR')) &
       call CON_stop('+++ Unknown coordinate system "'//trim(CoordSys)//'" specified +++')
  if (StartTimeRel > EndTimeRel) &
       call CON_stop('+++ StartTime MUST be less than EndTime +++')
  if (TimeStep <= 0.0) &
       call CON_stop('+++ TimeStep MUST be positive +++')
  if (EndTimeRel > StartTimeRel + TimeStep*49999) &
       call CON_stop('+++ Output file cannot have more than 50000 lines +++')
  oUnit=6
  write(oUnit,'(a)') '#COOR'
  write(oUnit,'(a)') trim(CoordSys)
  write(oUnit,*)
  write(oUnit,'(a)') '#DATE'
  do i = 1, 7
     write(oUnit,*) iStartTime(i)
  end do
  write(oUnit,*)
  write(oUnit,'(a)') '#TIMELOOP'
  write(oUnit,'(3f12.3)') StartTimeRel, EndTimeRel, TimeStep
  write(oUnit,*)
  write(oUnit,'(a)') '#START'

  call time_int_to_real(iStartTime, StartTime)

  CurrentTime = StartTime + StartTimeRel
  EndTime = StartTime + EndTimeRel
  do while(CurrentTime <= EndTime)
     call time_real_to_int(CurrentTime, iCT)
     call CON_recalc(iCT(1), iCT(2), iCT(3), iCT(4), iCT(5), iCT(6))
     select case(CoordSys)
     case('HAE')
        write(oUnit,'(7i4,3f12.2)') iCT, &
             Sun2EarthDirHae_D*SunEMBDistance*cAU/Rsun
     case('HGI')
        write(oUnit,'(7i4,3f12.2)') iCT, &
             Sun2EarthDirHgi_D*SunEMBDistance*cAU/Rsun
     case('HGR')
        Xyz_D =  matmul(HgrHgi_DD,Sun2EarthDirHgi_D)
        write(oUnit,'(7i4,3f12.2)') iCT, &
             Xyz_D*SunEMBDistance*cAU/Rsun
     end select
     CurrentTime = CurrentTime + TimeStep
  end do

  stop

9000 continue
  call CON_stop('+++ Error opening file "'//trim(NameFile)//'" +++')
9010 continue
  call CON_stop('+++ Error in date format +++')
9020 continue
  call CON_stop('+++ Error in timeloop format +++')

end program earth_traj
!==============================================================================
