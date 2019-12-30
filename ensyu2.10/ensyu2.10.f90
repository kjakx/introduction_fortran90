program rnum
    implicit none
    double precision, allocatable :: r(:)
    double precision :: mean = 0, stddev = 0
    integer n, i
    write(*, *) 'input n (n >= 1):'
    read(*, *) n
    if (n < 1) stop 'stop. n must be n >= 1'
    allocate(r(n))
    call random_seed
    call random_number(r(1:n))
    write(*, *) r(1:n)
    mean = sum(r)
    mean = mean / dble(n)
    do i = 1, n
        stddev = stddev + (r(i) - mean) ** 2
    end do
    stddev = stddev / dble(n)
    stddev = sqrt(stddev)
    write(*, *) 'mean =', mean
    write(*, *) 'stddev =', stddev
end program rnum