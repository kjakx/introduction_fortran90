program random_mat
    implicit none
    double precision, allocatable :: a(:, :)
    integer n, i 
    write(*, *) 'input n:'
    read(*, *) n 
    if (n < 1 .or. n > 100) stop 'stop. n must be 0 < n < 100'
    allocate(a(n, n))
    call random_number(a(:, :))
    do i = 1, n 
        write(*, '(100e12.4)') a(i, 1:n)
    end do 
end program random_mat