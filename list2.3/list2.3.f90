program dotp3
    implicit none
    integer :: i, n = 2, fi = 10 
    double precision dotp
    double precision, allocatable :: u(:), v(:)
    open(fi, file = 'mat.d')
    read(fi, *) n
    allocate(u(n), v(n))
    read(fi, *) u
    read(fi, *) v
    close(fi)
    dotp = 0.0d0
    do i = 1, n
        dotp = dotp + u(i) * v(i)
    end do
    write(*, *) 'dot product =', dotp
end program