module ensyu3_21
    implicit none
contains 
    function determinant(A) result(det)
        double precision, intent(in) :: A(2, 2)
        double precision det
        det = A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1)
    end function determinant
end module ensyu3_21

program outer_product
    use ensyu3_21
    implicit none
    integer i
    double precision a(3), b(3), c(2, 3), d(2, 2), outer(3)
    a(:) = (/1, 2, 3/)
    b(:) = (/4, 5, 6/)
    c(1, :) = a(:)
    c(2, :) = b(:)
    write(*, *) "a :", a
    write(*, *) "b :", b
    write(*, *) "c :"
    do i = 1, 2
        write(*, *) c(i, :)
    end do
    do i = 1, 3
        d(:, :) = c(:, 2:3)
        outer(i) = determinant(d)
        c = cshift(c, (/1, 1/), 2)
    end do 
    write(*, *) "outer_product =", (outer(i), i = 1, 3)
end program outer_product
