program dotp4
    implicit none
    double precision, allocatable :: u(:), v(:)
    integer :: n, fi = 10
    open(fi, file = "mat.d")
    read(fi, *) n
    allocate(u(n), v(n))
    read(fi, *) u
    read(fi, *) v
    close(fi)
    write(*, *) 'dp =', dot_product(u, v)
    deallocate(u, v)
end program 