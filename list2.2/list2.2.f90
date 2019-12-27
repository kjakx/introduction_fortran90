program dotp2
    implicit none
    integer :: i, fi = 10 
    integer, parameter :: n = 2
    double precision u(n), v(n), dotp
    open(fi, file = 'mat.d')
    read(fi, *) u
    read(fi, *) v
    close(fi)
    dotp = 0.0d0
    do i = 1, n
        dotp = dotp + u(i) * v(i)
    end do
    write(*, *) 'dot product =', dotp
end program