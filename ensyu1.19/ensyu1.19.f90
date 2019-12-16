program hyperbolic
    implicit none
    double precision x, d
    integer :: i, n, fi = 10, fo = 11
    open(fi, file = 'input.d')
    open(fo, file = 'output.d')
    read(fi, *) n
    close(fi)
    write(fo, *) 'x sinh(x) cosh(x) tanh(x)'
    d = 2.0d0 / dble(n - 1)
    do i = 1, n
        x = -1.0d0 + d * dble(i - 1)
        write(fo, '(4e12.4)') x, sinh(x), cosh(x), tanh(x)
    enddo
    close(fo)
end program hyperbolic