program random_mat
    use interface_mod
    implicit none
    double precision, allocatable :: a(:, :)
    call allocate_rmat(a)
    call print_mat(a)
end program random_mat