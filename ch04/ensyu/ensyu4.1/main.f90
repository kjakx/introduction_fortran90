program random_mat
    use interface_mod
    implicit none
    double precision, allocatable :: a(:, :)
    integer n
    write(*, *) 'input n:'
    read(*, *) n
    if (n < 1 .or. n > 100) stop 'n must be 0 < 101'
    call allocate_rmat(a, n)
    call print_mat(a, n)
end program random_mat