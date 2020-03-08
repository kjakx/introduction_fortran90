program main
    use interface_mod
    implicit none
    integer, parameter :: n = 3, m = 2
    double precision AB(n, n+m), B(n, m)
    integer i
    call random_number(AB(:, :))
    write(*, *) 'AB :'
    do i = 1, n
        write(*, *) AB(i, :)
    end do
    B = AB_gj_pv(AB, n, m)
    write(*, *) 'B :'
    do i = 1, n
        write(*, *) B(i, :)
    end do
end program main