  ! These vector valued functions are to be included.
  ! Avoids nvfortran+OpenACC compiler bugs. Allows inlining.
  !============================================================================
  function matmul3_left(a_DD, b_D) result(c_D)
    !$acc routine seq

    ! Matrix-vector multiplication for 3x3 matrix,
    ! to avoid temporaries at the call site.
    ! Equivalent with c_D = matmul(a_DD, b_D)

    real, intent(in) :: a_DD(3,3), b_D(3)
    real :: c_D(3)
    !--------------------------------------------------------------------------
    c_D(1) = a_DD(1,1)*b_D(1) + a_DD(1,2)*b_D(2) + a_DD(1,3)*b_D(3)
    c_D(2) = a_DD(2,1)*b_D(1) + a_DD(2,2)*b_D(2) + a_DD(2,3)*b_D(3)
    c_D(3) = a_DD(3,1)*b_D(1) + a_DD(3,2)*b_D(2) + a_DD(3,3)*b_D(3)

  end function matmul3_left
  !============================================================================
  function matmul3_right(a_D, b_DD) result(c_D)
    !$acc routine seq

    ! Vector-Matrix multiplication for 3x3 matrix,
    ! to avoid temporaries at the call site.
    ! Equivalent with c_D = matmul(a_D, b_DD)

    real, intent(in) :: a_D(3), b_DD(3,3)
    real :: c_D(3)
    !--------------------------------------------------------------------------

    c_D(1) = a_D(1)*b_DD(1,1) + a_D(2)*b_DD(2,1) + a_D(3)*b_DD(3,1)
    c_D(2) = a_D(1)*b_DD(1,2) + a_D(2)*b_DD(2,2) + a_D(3)*b_DD(3,2)
    c_D(3) = a_D(1)*b_DD(1,3) + a_D(2)*b_DD(2,3) + a_D(3)*b_DD(3,3)

  end function matmul3_right
  !============================================================================
  function matmul23_right(a_D, b_DD) result(c_D)
    !$acc routine seq

    ! Vector-Matrix multiplication for 2x3 matrix,
    ! to avoid temporaries at the call site.
    ! Equivalent with c_D = matmul(a_D, b_DD)

    real, intent(in) :: a_D(2), b_DD(2,3)
    real :: c_D(3)
    !--------------------------------------------------------------------------
    c_D(1) = a_D(1)*b_DD(1,1) + a_D(2)*b_DD(2,1)
    c_D(2) = a_D(1)*b_DD(1,2) + a_D(2)*b_DD(2,2)
    c_D(3) = a_D(1)*b_DD(1,3) + a_D(2)*b_DD(2,3)

  end function matmul23_right
  !============================================================================
  function cross_prod(a_D, b_D) result(c_D)
    !$acc routine seq

    ! Return c = a x b

    real, intent(in) :: a_D(3), b_D(3)
    real             :: c_D(3)
    !--------------------------------------------------------------------------
    c_D(1) = a_D(2)*b_D(3) - a_D(3)*b_D(2)
    c_D(2) = a_D(3)*b_D(1) - a_D(1)*b_D(3)
    c_D(3) = a_D(1)*b_D(2) - a_D(2)*b_D(1)

  end function cross_prod
  !============================================================================
