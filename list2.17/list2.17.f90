program block_mat
    implicit none
    integer m, n, k
    ! 
    ! Firstly, n x n random matrixes a, b are set.
    ! c is a matrix product a x b calcurated by matmul(a, b).
    ! d is mathematically the same as c, but the calculation method is different: block matrix is used.
    !
    double precision, allocatable :: a(:, :), b(:, :), c(:, :), d(:, :)
    n = 100 ! matrix size n x n 
    m = 33  ! upper-left partial matrix size 
    allocate(a(n, n), b(n, n), c(n, n), d(n, n))
    call random_seed 
    call random_number(a(:, :))
    call random_number(b(:, :))
    c = matmul(a, b)
    ! product of block matrixes
    k = m + 1
    d(1:m, 1:m) = matmul(a(1:m, 1:m), b(1:m, 1:m)) + matmul(a(1:m, k:n), b(k:n, 1:m))
    d(1:m, k:n) = matmul(a(1:m, 1:m), b(1:m, k:n)) + matmul(a(1:m, k:n), b(k:n, k:n))
    d(k:n, 1:m) = matmul(a(k:n, 1:m), b(1:m, 1:m)) + matmul(a(k:n, k:n), b(k:n, 1:m))
    d(k:n, k:n) = matmul(a(k:n, 1:m), b(1:m, k:n)) + matmul(a(k:n, k:n), b(k:n, k:n))
    write(*, *) sum((c(:, :) - d(:, :)) ** 2)   ! c is precisely not the same as d because of some errors.
end program block_mat