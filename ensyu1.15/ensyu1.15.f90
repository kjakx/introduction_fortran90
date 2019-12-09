program newton_kroot
    implicit none
    real(8) :: x1, x2, a, er, er0 = 1.0d-6
    integer :: n, k, nmax = 100
    write(*, '(a\)') 'input k, a : '
    read(*, *) k, a
    if (k < 2) stop 'stop. k mus be k >= 2'
    if (a <= 0) stop 'stop. a must be a > 0'
    write(*, *) ! new line
    write(*, *) 'k =', k
    write(*, *) 'a =', a
    x1 = a
    do n = 1, nmax
        x2 = x1 - (x1 ** k - a) / (x1 ** (k - 1)) / k
        er = abs(x2 - x1)
        if (er < er0) exit
        x1 = x2
    enddo
    write (*, *) 'kai, n, er = ', x2, n, er
end program newton_kroot
