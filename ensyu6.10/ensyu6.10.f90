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

    subroutine gaussian_elimination_pv(a0, x, b, n)
        ! --- gaussian elimination with partial pivoting ---
        integer, intent(in) :: n
        double precision, intent(in) :: a0(n, n), b(n)
        double precision, intent(out) :: x(n)
        integer i, j, k, m
        double precision ar, am, a(n, n), t, w(n)
        a(:, :) = a0(:, :)
        x(:) = b(:)

        ! --- upper trianglation ---
        do k = 1, n
            ! --- partial pivoting ---
            m = k
            am = abs(a(k, k))
            do i = k + 1, n
                if (abs(a(i, k)) > am) then
                    am = abs(a(i, k))
                    m = i
                end if
            end do
            if (am == 0.0d0) stop 'A is singular'
            if (k /= m) then
                w(k:n) = a(k, k:n)
                a(k, k:n) = a(m, k:n)
                a(m, k:n) = w(k:n)
                t = x(k)
                x(k) = x(m)
                x(m) = t
            end if
            ! --- end partial pivoting ---
            ar = 1.0d0 / a(k, k)
            a(k, k) = 1.0d0
            a(k, k+1:n) = ar * a(k, k+1:n)
            x(k) = ar * x(k)
            do i = k, n
                if (i /= k) then
                    a(i, k+1:n) = a(i, k+1:n) - a(i, k) * a(k, k+1:n)
                    x(i) = x(i) - x(k) * a(i, k)
                    a(i, k) = 0.0d0
                end if
            end do
        end do
        ! --- end upper trianglation ---

        ! --- check if matrix a is upper trianglated ---
        ! do i = 1, n
        !     write(*, *) a(i, :)
        ! end do
        ! --- end check

        ! --- backward substitution ---
        do k = 1, n
            i = n - k + 1   ! i is an order-reversed index.
            do j = i + 1, n
                x(i) = x(i) - a(i, j) * x(j)
            end do
        end do
        ! --- end backward substitution ---
    end subroutine gaussian_elimination_pv

    subroutine set_random_ab(a, b, x, n)
        integer, intent(in) :: n
        double precision, allocatable, intent(out) :: a(:, :), b(:), x(:)
        allocate(a(n, n), b(n), x(n))
        call random_number(a)
        call random_number(b)
    end subroutine set_random_ab

    subroutine least_squares(x, y, a, n)
        integer, intent(in) :: n
        double precision, intent(in) :: x(:), y(:)
        double precision, intent(out) :: a(0:n)
        double precision b(0:n), c(0:n, 0:n)
        integer m, i, j, k
        m = size(x, 1)
        b(:) = 0.0d0
        c(:, :) = 0.0d0
        do k = 0, n
            do i = 1, m
                b(k) = b(k) + y(i) * x(i) ** k
            end do
        end do
        do k = 0, n
            do j = 0, n
                do i = 1, m
                    c(k, j) = c(k, j) + x(i) ** (k + j)
                end do
            end do
        end do
        call gaussian_elimination_pv(c, a, b, n+1)
    end subroutine least_squares

    function polynomial(n, a) result(y)
        integer, intent(in) :: n
        integer, parameter :: m = 1000
        double precision, intent(in) :: a(0:n)
        double precision y(m), x(m)
        integer i
        x(:) = linspace(-10.0d0, 10.0d0, m - 1)
        y(:) = 0.0d0
        do i = 0, n
            y(:) = y(:) + a(i) * x(:) ** i
        end do
    end function polynomial  
end module subprogs

program main
    use subprogs
    implicit none
    integer, parameter :: n = 3, m = 100
    double precision a(0:n), x(m), y(m), x_fit(1000), y_fit(1000)
    integer :: i, fin = 10, fout = 11
    open(fin, file='points.d')
    do i = 1, m
        read(fin, *) x(i), y(i)
    end do
    close(fin)
    call random_number(a)
    call least_squares(x, y, a, n)
    x_fit(:) = linspace(-10.0d0, 10.0d0, 999)
    y_fit(:) = polynomial(n, a)
    open(fout, file='poly.d')
    do i = 1, 1000
        write(fout, '(2e12.4)') x_fit(i), y_fit(i)
    end do
    close(fout)
    do i = 0, n
        write(*, *) 'a', n-i, '=', a(n-i)
    end do
end program main

