! Because of the gfortran, the declaration of variables should divide into several parts

program Lab
    ! Q. 1
    implicit none

    ! Declare
    real(8) :: right
    real(8) :: left
    real(8) :: index
    real(8) :: result
    real(8) :: last_result
    real(8), parameter :: cons = 10**(-8)
    real(8) :: diff
    right = 1.
    left = 0.
    index = (left + right) / 2.

    ! Main
    ! 3 steps of Bisection Method
    print *, cons
    call bisection_mtd(left, right, index, result)
    print *, left, right, index, result
    call bisection_mtd(left, right, index, result)
    print *, left, right, index, result
    call bisection_mtd(left, right, index, result)
    print *, left, right, index, result

    ! Iterative Newton's Method
    diff = result
    do while(diff .GT. cons)
        last_result = result ! Update last_index
        call Newton_mtd(index, result, last_result) ! Update index
        diff = abs(result) - result
    end do
    print *, index, result
end program Lab


!________________________________________________________
! Function
subroutine fc(x, rs)
    implicit none
    real(8) :: x, rs

    rs = x**3 - cos(x)
end subroutine fc

! One Derivative of Function
subroutine deri(x, rs)
    implicit none
    real(8) :: x, rs

    rs = 3.*(x**2) + sin(x)
end subroutine deri

! Two Derivative of Function
! subroutine two_deri(x, result)
!     implicit none
!     real(8) :: x, result
!
!     result = 6.*x + cos(x)
! end subroutine two_deri

! Bisection Method
subroutine bisection_mtd(left, right, index, result)
    implicit none
    real(8) :: left, right
    real(8) :: index, result
    real(8) :: ival, lval, rval
    real(8) :: x, rs ! represent the x and result in other subroutines

    index = (left + right) / 2.

    x = index
    call fc(x, rs)
    ival = rs

    x = left
    call fc(x, rs)
    lval = rs

    x = right
    call fc(x, rs)
    rval = rs

    if((ival .GT. lval) .AND. (ival .GT. 0)) then
        right = index
    else if((ival .GT. lval) .AND. (ival .LT. 0)) then
        left = index
    end if

    result = ival
end subroutine bisection_mtd

! Newton's Method
subroutine Newton_mtd(index, result, last_result)
    implicit none

    ! Declare
    real(8) :: last_result, last_index, last_deri
    real(8) :: index, result
    real(8) :: x, rs

    last_index = index ! index and result will change

    x = last_index
    call fc(x, rs)
    last_result = rs

    x = last_index
    call deri(x, rs) ! calculate the derivative of the index last time
    last_deri = rs

    ! Main
    index = last_index - last_result/last_deri ! Update the index

    x = index
    call fc(x, rs)! Update the result
    result = rs
end subroutine Newton_mtd
