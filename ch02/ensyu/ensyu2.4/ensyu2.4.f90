program vec_norm
    double precision u(3), l1, l2
    integer :: i, fi = 10
    open(fi, file = 'vec.d')
    read(fi, *) u(:)
    close(fi)
    ! dot_product
    l1 = dot_product(u, u)
    l1 = sqrt(l1)
    ! do loop
    l2 = 0
    do i = 1, 3
        l2 = l2 + u(i) * u(i)
    end do 
    l2 = sqrt(l2)
    write(*, *) 'l1 =', l1
    write(*, *) 'l2 =', l2
end program vec_norm