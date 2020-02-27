program main
    use interface_mod
    implicit none
    integer, parameter :: n = 3
    double precision AI(n, 2*n), inverse_a(n, n)
    integer i
    AI(:, :) = 0.0d0
    call random_number(AI(:, 1:n))
    do i = 1, n
        AI(i, n+i) = 1.0d0
    end do
    write(*, *) 'AI :'
    do i = 1, n
        write(*, *) AI(i, :)
    end do
    inverse_a = inverse_gj_pv(AI, n)
    write(*, *) 'inverse_a :'
    do i = 1, n
        write(*, *) inverse_a(i, :)
    end do
end program main