program dotp1
    implicit none
    integer i
    double precision u(2), v(2), dotp
    u(1) = 1.2d0
    u(2) = 3.4d0
    v(1) = 4.1d0
    v(2) = 2.6d0
    write(*, *) (u(i), i = 1, 2)
    write(*, *) (v(i), i = 1, 2)
    dotp = 0.0d0
    do i = 1, 2
        dotp = dotp + u(i) * v(i)
    enddo
    write(*, *) 'dot product =', dotp
end program dotp1
