module ensyu3_32
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
        double precision, intent(in) :: a(3), b(3)
        double precision cross(3)
        integer :: i
        do i = 1, 3
            cross(i) = a(mod(i, 3) + 1) * b(mod(i + 1, 3) + 1) - a(mod(i + 1, 3) + 1) * b(mod(i, 3) + 1)
        end do
    end function cross_product_207

    function scolar_triple_product(a, b, c) result(p)
        double precision, intent(in) :: a(3), b(3), c(3)
        double precision p 
        p = dot_product(a, cross_product_207(b, c))
    end function scolar_triple_product
end module ensyu3_32

program main
    use ensyu3_32 
    implicit none 
    double precision a(3), b(3), c(3)
    double precision p 
    a = (/ 1.d0, 2.d0, 3.d0 /)
    b = (/ 8.d0, 9.d0, 4.d0 /)
    c = (/ 7.d0, 6.d0, 5.d0 /)
    p = scolar_triple_product(a, b, c)
    write(*, *) 'p :', p
end program main