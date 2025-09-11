!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModUser

  use BATL_lib, ONLY: &
       test_start, test_stop, iProcTest, iProc

  use ModUserEmpty,               &
       IMPLEMENTED1 => user_set_ics

  include 'user_module.h' ! list of public methods

  character (len=*), parameter :: NameUserFile = "ModUserStretchedDipole.f90"
  character (len=*), parameter :: NameUserModule = &
       'Stretched Dipole'

  real, parameter :: alpha = 2.0, beta = 3.0 ! Stretching factors

contains
  !============================================================================
  subroutine user_set_ics(iBlock)

    use ModMain, ONLY: nI, nJ, nK
    use ModAdvance, ONLY: State_VGB
    use ModGeometry, ONLY: Xyz_DGB
    use ModVarIndexes, ONLY: Bx_,By_,Bz_
    use ModPhysics, ONLY: Bdp
    use ModB0, ONLY: B0_DGB

    integer, intent(in) :: iBlock

    integer :: i, j, k
    real    :: x, y, z, a, b

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_set_ics'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)
    if(iProc==iProcTest)then
       write(*,*)'Initializing Stretched Dipole '
       write(*,*)'Parameters:'
       write(*,*)'alpha=', alpha
       write(*,*)'beta =', beta

    else
       DoTest=.false.; DoTest=.false.
    end if

    do k = 1, nK; do j = 1, nJ; do i = 1, nI
       x = Xyz_DGB(x_,i,j,k,iBlock)
       y = Xyz_DGB(y_,i,j,k,iBlock)
       z = Xyz_DGB(z_,i,j,k,iBlock)

       ! The dipole field is stretched by alpha factor in the z direction and
       ! beta in the y direction.

       a = (sqrt(x**2 + y**2 + z**2))**5
       b = (sqrt(x**2 + (y*beta)**2 + (alpha*z)**2))**5

        ! write(*,*) 'Bx', B0_DGB(1,i,j,k,iBlock), Bdp*(3*z * x)/a
        ! write(*,*) 'By', B0_DGB(2,i,j,k,iBlock), Bdp*(3*z * y)/a
        ! write(*,*) 'Bz', B0_DGB(3,i,j,k,iBlock), Bdp*(2*z**2  - x**2 - y**2)/a

       State_VGB(Bx_,i,j,k,iBlock) = Bdp*((3. * z * x * alpha)/b - (3. * z * x)/a)
       State_VGB(By_,i,j,k,iBlock) = Bdp*((3. * z * y * alpha)/b - (3. * z * y)/a)
       State_VGB(Bz_,i,j,k,iBlock) = &
            Bdp*((2. * (alpha*z)**2 - x**2 - (beta*y)**2)/(alpha*b) - &
            (  2.      *       z**2 - x**2 -        y**2)/a)

    end do; end do; end do

    call test_stop(NameSub, DoTest, iBlock)
  end subroutine user_set_ics
  !============================================================================

end module ModUser
!==============================================================================
