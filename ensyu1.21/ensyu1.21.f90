program er_circ_poly_area
    implicit none
    double precision :: r, th, s_poly, s_circle, er, pi = 3.14159265 
    integer :: i, n, fi = 10, fo = 11
    open(fi, file = 'input.d')
    open(fo, file = 'output.d')
    read(fi, *) r, n
    close(fi)
    if (r <= 0) stop 'stop. r must be r > 0'
    if (n < 3) stop 'stop. n must be n >= 3'
    s_circle = r ** 2 * pi
    do i = 3, n
        th = 2.0d0 * pi / dble(i)
        s_poly = dble(i) * r * r * sin(th) / 2.0d0
        er = s_circle - s_poly
        write(fo, '(2e12.4)') dble(i), er
    enddo
    close(fo)
end program er_circ_poly_area