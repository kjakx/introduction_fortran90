program ensyu
    implicit none
    double precision :: r, th, d_th, pi = 3.14159265 
    integer :: i, n, fi = 10, fo = 11
    open(fi, file = 'input.d')
    open(fo, file = 'output.d')
    read(fi, *) r, n
    close(fi)
    if (r <= 0) stop 'stop. r must be r > 0'
    if (n < 3) stop 'stop. n must be n >= 3'
    d_th = 2.0d0 * pi / dble(n)
    do i = 1, n + 1
        th = d_th * dble(i - 1)
        write(fo, '(2e12.4)') r * cos(th), r * sin(th)
    enddo
    close(fo)
end program ensyu