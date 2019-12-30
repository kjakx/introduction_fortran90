program mat2
    implicit none
    integer :: n, i, fi = 10
    double precision, allocatable :: a(:, :)
    open(fi, file = "mat.d")
    read(fi, *) n
    allocate(a(n, n))
    do i = 1, n
        read(fi, *) a(i, 1:n)
    end do
    close(fi)
    do i = 1, n
        write(*, '(100e12.4)') a(i, 1:n)
    end do
    deallocate(a)
end program mat2