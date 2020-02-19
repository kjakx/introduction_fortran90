subroutine set_dd_mat(a)
    implicit none
    double precision, intent(out) :: a(:, :)
    integer i, n, m
    n = size(a, 1)
    m = size(a, 2)
    if (n /= m) stop 'stop. n != m'
    call random_number(a)
    a = 2.d0 * a - 1.d0
    a = a / dble(n + 1)
    do i = 1, n
        a(i, i) = 1.d0
    end do
end subroutine set_dd_mat