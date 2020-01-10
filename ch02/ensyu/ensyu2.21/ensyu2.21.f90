program matmul_vs_do
    implicit none
    integer n, i, j, k 
    double precision t1, t2
    double precision, allocatable :: a(:, :), b(:, :), mul_ab(:, :), t_mul_ab(:, :), ta(:, :), tb(:, :), mul_tbta(:, :)
    write(*, *) 'input n:'
    read(*, *) n
    write(*, *) "n =", n
    allocate(a(n, n), b(n, n), mul_ab(n, n), t_mul_ab(n, n), ta(n, n), tb(n, n), mul_tbta(n, n))
    call random_seed 
    call random_number(a(:, :))
    call random_number(b(:, :))
    ! do loop
    call cpu_time(t1)
    do i = 1, n 
        do j = 1, n 
            mul_ab(i, j) = 0.0d0
            do k = 1, n
                mul_ab(i, j) = mul_ab(i, j) + a(i, k) * b(k, j)
            end do
        end do
    end do 
    do i = 1, n 
        do j = 1, n 
            ta(i, j) = a(j, i)
            tb(i, j) = b(j, i)
            t_mul_ab(i, j) = mul_ab(j, i)
        end do
    end do 
    do i = 1, n 
        do j = 1, n 
            mul_tbta(i, j) = 0.0d0
            do k = 1, n
                mul_tbta(i, j) = mul_tbta(i, j) + tb(i, k) * ta(k, j)
            end do
        end do 
    end do
    call cpu_time(t2)
    write(*, *) "result(do loop):", all(mul_tbta == t_mul_ab)
    write(*, *) "time:", t2 - t1
    ! matmul
    call cpu_time(t1)
    mul_ab = matmul(a, b)
    ta = transpose(a)
    tb = transpose(b)
    t_mul_ab = transpose(mul_ab)
    mul_tbta = matmul(tb, ta)
    call cpu_time(t2)
    write(*, *) "result(transpose):", all(mul_tbta == t_mul_ab)
    write(*, *) "time:", t2 - t1
    deallocate(a, b, mul_ab, t_mul_ab, ta, tb, mul_tbta)
end program matmul_vs_do