program random_noise
    implicit none
    double precision, allocatable :: r(:), x(:), y(:)
    double precision dx
    integer :: n, i, fo = 10
    write(*, *) 'input n (n >= 1):'
    read(*, *) n
    if (n < 1) stop 'stop. n must be n >= 1'
    dx = 10.0d0 / dble(n - 1)
    allocate(x(n), y(n))
    do i = 1, n
        x(i) = dx * (i - 1)
    end do
    allocate(r(n))
    call random_seed
    call random_number(r(1:n))
    r(1:n) = 2.0d0 * r(1:n) - 1.0d0 
    y(:) = 2 * x(:) + 1 + r(:)
    open(fo, file = "output.d")
    do i = 1, n
        write(fo, *) x(i), y(i)
    end do 
    close(fo)
end program random_noise