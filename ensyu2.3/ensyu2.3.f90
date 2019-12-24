program vec_diff_sq
    double precision a(3), b(3), c1(3), c2(3) ! n = 3
    integer :: i, fi = 10
    open(fi, file = 'mat.d')
    read(fi, *) (a(i), i = 1, 3)
    read(fi, *) (b(i), i = 1, 3)
    close(fi)
    ! embedded operation
    c1(:) = (a(:) - b(:)) ** 2
    ! do loop
    do i = 1, 3
        c2(i) = a(i) - b(i)
        c2(i) = c2(i) ** 2
    end do
    write(*, *) 'c1(i):', (c1(i), i = 1, 3)
    write(*, *) 'c2(i):', (c2(i), i = 1, 3)
end program vec_diff_sq