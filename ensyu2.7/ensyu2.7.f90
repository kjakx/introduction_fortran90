program cross_product
    implicit none
    double precision u(3), v(3), uxv(3)
    integer :: i, fi = 10
    open(fi, file = 'mat.d')
    read(fi, *) u
    read(fi, *) v
    close(fi)
    do i = 1, 3
        uxv(i) = u(mod(i, 3) + 1) * v(mod(i + 1, 3) + 1) - u(mod(i + 1, 3) + 1) * v(mod(i, 3) + 1)
    end do
    write(*, *) 'uxv =', uxv
end program