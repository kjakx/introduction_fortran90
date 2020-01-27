module ensyu3_16
    implicit none
contains
    subroutine print_sym_mat(a)
        double precision, intent(in) :: a(:, :)
        double precision, allocatable :: a_t(:, :), a_sym(:, :), a_alt(:, :)
        integer n, i, j
        n = size(a, 1)
        allocate(a_t(n, n), a_sym(n, n), a_alt(n, n))
        a_t = transpose(a)
        a_sym = 0.5 * (a + a_t)
        a_alt = 0.5 * (a - a_t)
        write(*, *) 'a :'
        do i = 1, n 
            write(*, *) (a(i, j), j = 1, n)
        end do
        write(*, *) 'a_t :'
        do i = 1, n 
            write(*, *) (a_t(i, j), j = 1, n)
        end do
        write(*, *) 'a_sym :'
        do i = 1, n 
            write(*, *) (a_sym(i, j), j = 1, n)
        end do
        write(*, *) 'a_alt :'
        do i = 1, n 
            write(*, *) (a_alt(i, j), j = 1, n)
        end do
    end subroutine print_sym_mat
end module ensyu3_16 

program main
    use ensyu3_16
    implicit none
    double precision, allocatable :: a(:, :)
    integer n
    write(*, *) 'input n:'
    read(*, *) n
    write(*, *) 'n =', n
    allocate(a(n, n))
    call random_number(a)
    call print_sym_mat(a)
    deallocate(a)
end program main