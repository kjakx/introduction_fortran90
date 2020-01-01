program grid
    implicit none
    integer :: m, i, j, n1, n2, fno = 10
    double precision dx1, dx2
    double precision, allocatable :: x(:, :, :)
    write(*, *) 'input n1, n2:'
    read(*, *) n1, n2
    allocate(x(2, n1, n2))
    open(fno, file = 'grid.d')
    dx1 = 1.0d0 / dble(n1 - 1)
    dx2 = 1.0d0 / dble(n2 - 1)
    ! set x(m, i, j)
    do i = 1, n1
        do j = 1, n2
            x(1, i, j) = dx1 * dble(i - 1)
            x(2, i, j) = dx2 * dble(j - 1)
        end do 
    end do 
    ! write x1, x2 on grid.d
    do i = 1, n1
        do j = 1, n2
            write(fno, *) (x(m, i, j), m = 1, 2)
        end do 
        write(fno, *)
    end do
    write(fno, *)
    do j = 1, n2
        do i = 1, n1
            write(fno, *) (x(m, i, j), m = 1, 2)
        end do 
        write(fno, *)
    end do
    write(fno, *)
    deallocate(x)
    close(fno)
end program grid