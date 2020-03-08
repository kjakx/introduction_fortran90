program main
    use interface_mod
    implicit none
    double precision, allocatable :: a(:, :), b(:), x(:), r(:)
    integer :: n = 3
    call set_random_ab(a, b, x, n)
    call gauss_jordan_pv(a, x, b, n)
    allocate(r(n))
    r(:) = b(:) - matmul(a, x)
    write(*, *) 'Gauss-Jordan(pv) error =', dot_product(r, r)
    deallocate(a, b, x)
end program main