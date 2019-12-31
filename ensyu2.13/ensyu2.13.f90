program random_symmetry_mat
    implicit none
    double precision, allocatable :: a(:, :)
    integer n, i, j 
    write(*, *) "input n (1<=n<=100):"
    read(*, *) n 
    if (n < 1 .or. 100 < n) stop "stop. n is invalid"
    allocate(a(n, n))
    call random_seed
    do j = 1, n 
        call random_number(a(1:j, j))
        a(j, 1:j) = a(1:j, j)
    end do 
    do i = 1, n 
        write(*, '(100e12.4)') a(i, :)
    end do
end program random_symmetry_mat   