module subprogs
    implicit none
contains
    function f(x) result(y)
        double precision, intent(in) :: x(:)
        double precision y(size(x, 1))
        y(:) = 0.1d0 * x(:) ** 3.0d0 + 0.2d0 * x(:) ** 2.0d0 + 0.5d0 * x(:) + 1.0d0
    end function f

    function linspace(low_lim, upp_lim, div_num) result(lin_vec)
        double precision, intent(in) :: low_lim, upp_lim
        integer, intent(in) :: div_num
        double precision lin_vec(div_num + 1)
        integer i
        double precision dx
        dx = (upp_lim - low_lim) / div_num
        do i = 1, div_num + 1
            lin_vec(i) = low_lim + dx * dble(i - 1)
        end do
    end function linspace

end module subprogs

program main
    use subprogs
    implicit none
    integer, parameter :: m = 100
    double precision x(m), r(m), y(m)
    integer :: i, fno = 10
    open(fno, file='points.d')
    x(:) = linspace(-10.0d0, 10.0d0, m - 1)
    call random_number(r(:))
    r(:) = 2.0d0 * r(:) - 1.0d0
    y(:) = f(x(:)) + r(:)
    do i = 1, m
        write(fno, '(2e12.4)') x(i), y(i)
    end do
    close(fno)
end program main

