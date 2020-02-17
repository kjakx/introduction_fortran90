module ensyu3_31 
    implicit none 
contains 
    function determinant(A) result(det)
        double precision, intent(in) :: A(2, 2)
        double precision det
        det = A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1)
    end function determinant

    function outer_product_321(a, b) result(outer)
        integer i
        double precision, intent(in) :: a(3), b(3)
        double precision c(2, 3), d(2, 2), outer(3)
        c(1, :) = a(:)
        c(2, :) = b(:)
        do i = 1, 3
            d(:, :) = c(:, 2:3)
            outer(i) = determinant(d)
            c = cshift(c, (/1, 1/), 2)
        end do 
    end function outer_product_321 

    function cross_product_207(a, b) result(cross)
        double precision a(3), b(3), cross(3)
        integer :: i
        do i = 1, 3
            cross(i) = a(mod(i, 3) + 1) * b(mod(i + 1, 3) + 1) - a(mod(i + 1, 3) + 1) * b(mod(i, 3) + 1)
        end do
    end function cross_product_207

    subroutine print_rmatc(a, name) 
        character(*), intent(in) :: name
        double precision, intent(in) :: a(:, :)
        integer i, n, m
        n = size(a, 1)
        m = size(a, 2)
        write(*, *) name
        do i = 1, n 
            write(*, '(100e12.4)') a(i, 1:m)
        end do 
    end subroutine print_rmatc
end module ensyu3_31

program main
    use ensyu3_31 
    implicit none 
    double precision a(3), b(3)
    double precision outer(3), cross(3)
    a = (/ 1, 2, 3 /)
    b = (/ 4, 5, 6 /)
    outer(:) = outer_product_321(a, b)
    cross(:) = cross_product_207(a, b)
    write(*, *) 'outer :', outer 
    write(*, *) 'cross :', cross
end program main