program dotp4
    implicit none
    double precision, allocatable :: u(:), v(:)
    integer n
    write(*, *) 'input n:'
    read(*, *) n
    allocate(u(n), v(n))
    write(*, *) 'input u(1 : n) :'
    read(*, *) u
    write(*, *) 'input v(1 : n) :'
    read(*, *) v
    write(*, *) 'dp =', dot_product(u, v)
    deallocate(u, v)
end program 