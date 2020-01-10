program normalize_vec
    double precision u(3), u_norm(3), l
    integer :: i, fi = 10
    open(fi, file = 'vec.d')
    read(fi, *) u(:)
    close(fi)
    ! dot_product
    l = dot_product(u, u)
    l = sqrt(l)
    if (l == 0) stop 'stop. norm is 0'
    u_norm = u / l
    write(*, *) 'l =', l
    write(*, *) 'u(1:3) =', (u(i), i = 1, 3)
    write(*, *) 'u_norm(1:3) =', (u_norm(i), i = 1, 3)
end program normalize_vec