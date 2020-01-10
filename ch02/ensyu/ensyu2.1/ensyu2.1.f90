program dotp_dim3
    double precision :: u(3), v(3), dotp = 0.0d0
    integer :: fi = 10, i
    open(fi, file = 'mat.d')
    read(fi, *) (u(i), i = 1, 3)
    read(fi, *) (v(i), i = 1, 3)
    do i = 1, 3
        dotp = dotp + u(i) * v (i)
    enddo
    write(*, *) (u(i), i = 1, 3)
    write(*, *) (v(i), i = 1, 3)
    write(*, *) 'dotp =', dotp
end program dotp_dim3