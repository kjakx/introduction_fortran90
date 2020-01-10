program outer_product
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
        outer(i) = d(1, 1) * d(2, 2) - d(1, 2) * d(2, 1)
        c = cshift(c, (/1, 1/), 2)
    end do 
    write(*, *) "outer_product =", (outer(i), i = 1, 3)
end program outer_product
