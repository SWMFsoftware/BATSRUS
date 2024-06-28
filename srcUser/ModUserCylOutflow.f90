!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModUser
  ! This is the default user module which contains empty methods defined
  ! in ModUserEmpty.f90

  use ModUserEmpty, &
       IMPLEMENTED1 => user_read_inputs, &
       IMPLEMENTED2 => user_set_ics,     &
       IMPLEMENTED6 => user_set_cell_boundary

  use ModMain, ONLY: tSimulation
  use ModAdvance, ONLY: Rho_, RhoUx_, RhoUy_, RhoUz_, p_, State_VGB
  use ModGeometry, ONLY: r_GB
  use ModBatsrusUtility, ONLY: stop_mpi
  use ModPlotFile, ONLY: read_plot_file
  use BATL_lib, ONLY: MinI, nI, nJ, Xyz_DGB, test_start, test_stop

  include 'user_module.h' ! list of public methods

  character(len=*), parameter:: NameUserFile = 'srcUser/ModUserCylOutflow.f90'
  character(len=*), parameter:: NameUserModule = 'Cylindrical Outflow'

  ! Initial filename
  character(len=*), parameter:: NameFileIni = &
       'Observation/intensities_t000.txt'

  ! Outflow parameters
  real:: u0 = 0.0, u1 = 1.0, Exponent = 1.0
  
contains
  !============================================================================
  subroutine user_read_inputs

    use ModReadParam
    character(len=100) :: NameCommand
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_read_inputs'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    UseUserIcs = .true.
    do
       if(.not.read_line() ) EXIT
       if(.not.read_command(NameCommand)) CYCLE
       select case(NameCommand)

       case('#OUTFLOW')
          call read_var('u0', u0)
          call read_var('u1', u1)
          call read_var('Exponent', Exponent)
       case('#USERINPUTEND')
          EXIT
       case default
          call stop_mpi( &
               NameSub//': unrecognized command: '//NameCommand)
       end select
    end do

  end subroutine user_read_inputs
  !============================================================================
  subroutine user_set_ics(iBlock)

    ! Set initial density from a file and initial velocity from u0 and u1

    integer, intent(in) :: iBlock

    integer:: i, j
    real:: r, Rho

    character(len=*), parameter:: NameSub = 'user_set_ics'
    !--------------------------------------------------------------------------
    if(iBlock > 1) call stop_mpi(NameSub//' only works for a single block')

    ! Read into pressure
    call read_plot_file(NameFileIni, VarOut_II=State_VGB(p_,MinI:nI,1:nJ,1,1))

    write(*,*) NameSub,' read p(1,1,1,1)=', State_VGB(p_,1,1,1,1)

    ! Set velocities to Ur = u0 + u1*r
    do j = 1, nJ; do i = MinI, nI
       r = r_GB(i,j,1,1) ! Radial distance
       Rho = State_VGB(p_,i,j,1,1)*r**Exponent
       State_VGB(Rho_,i,j,1,1) = Rho
       State_VGB(RhoUx_:RhoUy_,i,j,1,1) = &
            Rho*Xyz_DGB(x_:y_,i,j,1,1)*(u0/r + u1)
       State_VGB(RhoUz_,i,j,1,1) = 0.0
    end do; end do

  end subroutine user_set_ics
  !============================================================================
  subroutine user_set_cell_boundary(iBlock, iSide, TypeBc, IsFound)

    ! Set "false" cells that are surrounded by the "extra" face boundary
 
    integer, intent(in):: iBlock, iSide
    character(len=*), intent(in):: TypeBc
    logical, intent(out):: IsFound

    integer:: iPict, iPictLast = 0
    character(len(NameFileIni)):: NameFile
    character(len=3):: NamePict
    
    character(len=*), parameter:: NameSub = 'user_set_boundary_cells'
    !--------------------------------------------------------------------------
    if(iBlock /= 1) call stop_mpi(NameSub//' only works for 1 block')
    if(iSide /= 1) RETURN
    IsFound = .true.
    
    ! Increase iPict from 0 to 1 when tSimulation reaches 1/64
    iPict = ceiling(64*tSimulation)
    if(iPict > iPictLast)then
       iPictLast = iPict
       write(NamePict,'(i3.3)') iPict
       NameFile = NameFileIni(1:len(NameFile)-7)//NamePict//".txt"
       write(*,*) NameSub, ' reading file ', NameFile
       ! Read into pressure variable
       call read_plot_file(NameFile, VarOut_II=State_VGB(p_,MinI:nI,1:nJ,1,1))
       ! Copy ghost cells to density
       State_VGB(Rho_,MinI:0,1:nJ,1,1) = State_VGB(p_,MinI:0,1:nJ,1,1) &
            *r_GB(MinI:0,1:nJ,1,1)**Exponent
    end if

  end subroutine user_set_cell_boundary
  !============================================================================
end module ModUser
!==============================================================================
