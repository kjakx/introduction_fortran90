program mat_vec_mul
    implicit none
    integer n, i, j
    double precision t1, t2
    double precision, allocatable :: x(:), y(:), a(:, :)
    !write(*, *) 'input n:'
    read(*, *) n
    allocate(x(n), y(n), a(n, n))
    call random_seed 
    call random_number(x(:))
    call random_number(a(:, :))
    call cpu_time(t1)
    do i = 1, n 
        y(i) = 0.0d0
        do j = 1, n 
            y(i) = y(i) + a(i, j) * x(j)
        end do 
    end do 
    call cpu_time(t2)
    !write(*, *) 'y =', y
    write(*, *) n, t2 - t1
    deallocate(x, y, a)
end program mat_vec_mul