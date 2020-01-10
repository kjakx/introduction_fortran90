program matmul_vs_do
    implicit none
    integer n, i, j, k 
    double precision t1, t2
    double precision, allocatable :: a(:, :), b(:, :), c(:, :)
    !write(*, *) 'input n:'
    read(*, *) n
    write(*, *) "n =", n
    allocate(a(n, n), b(n, n), c(n, n))
    call random_seed 
    call random_number(a(:, :))
    call random_number(b(:, :))
    ! do loop
    call cpu_time(t1)
    do i = 1, n 
        do j = 1, n 
            c(i, j) = 0.0d0
            do k = 1, n
                c(i, j) = c(i, j) + a(i, k) * b(k, j)
            end do 
        end do
    end do 
    call cpu_time(t2)
    write(*, *) "c(do loop):", c
    write(*, *) "time:", t2 - t1
    ! matmul
    call cpu_time(t1)
    c = matmul(a, b)
    call cpu_time(t2)
    write(*, *) "c(matmul):", c
    write(*, *) "time:", t2 - t1
    deallocate(a, b, c)
end program matmul_vs_do