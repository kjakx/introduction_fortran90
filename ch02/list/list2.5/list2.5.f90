program rnum
    implicit none
    double precision, allocatable :: r(:)
    integer n
    write(*, *) 'input n (n >= 1):'
    read(*, *) n
    if (n < 1) stop 'stop. n must be n >= 1'
    allocate(r(n))
    call random_seed
    call random_number(r(1:n))
    r(1:n) = 2.0d0 * r(1:n) - 1.0d0
    write(*, *) r(1:n)
end program rnum