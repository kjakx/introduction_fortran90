subroutine allocate_rmat(a, n)
    implicit none
    integer, intent(in) :: n
    double precision, allocatable, intent(out) :: a(:, :)
    allocate(a(n, n))
    call random_number(a)
end subroutine allocate_rmat

subroutine print_mat(a, n)
    implicit none
    integer, intent(in) :: n
    double precision, intent(in) :: a(n, n)
    integer i, n_row, n_col
    n_row = size(a, 1)
    n_col = size(a, 2)
    do i = 1, n_row
        write(*, '(100e12.4)') a(i, 1:n_col)
    end do
end subroutine print_mat