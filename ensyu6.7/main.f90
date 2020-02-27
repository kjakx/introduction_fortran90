program main
    use interface_mod
    implicit none
    double precision, allocatable :: a(:, :), b(:), x(:), r(:)
    integer :: n = 3
    double precision det_a
    call set_random_ab(a, b, x, n)
    call gauss_jordan_pv_det(a, x, b, n, det_a)
    allocate(r(n))
    r(:) = b(:) - matmul(a, x)
    write(*, *) 'Gaussian elimination error =', dot_product(r, r)
    deallocate(a, b, x)
    write(*, *) 'det_a :', det_a
end program main
