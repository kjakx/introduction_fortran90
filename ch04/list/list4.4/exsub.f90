subroutine allocate_rmat(a)
    use interface_mod, only : print_mat
    implicit none
    double precision, allocatable, intent(out) :: a(:, :)
    integer n
    write(*, *) 'input n:'
    read(*, *) n
    if (n < 1 .or. n > 100) stop 'n must be 0 < 101'
    allocate(a(n, n))
    call random_number(a)
    call print_mat(a)
end subroutine allocate_rmat

subroutine print_mat(a)
    implicit none
    double precision, intent(in) :: a(:, :)
    integer i, n, m
    n = size(a, 1)
    m = size(a, 2)
    do i = 1, n
        write(*, '(100e12.4)') a(i, 1:m)
    end do
end subroutine print_mat