module subprogs
    implicit none
contains
    subroutine jacobi(a, b, x, n, itrmax, er0)
        integer, intent(in) :: n, itrmax
        double precision, intent(in) :: a(n, n), b(n), er0
        double precision, intent(out) :: x(n)
        double precision s, er, rd(n), r(n), x_prev(n)
        integer i, itr
        do i = 1, n
            if (a(i, i) == 0.0d0) stop 'a(i, i) == 0.0d0'
            rd(i) = 1.0d0 / a(i, i)
        end do
        x(1:n) = 0.0d0
        do itr = 1, itrmax
            x_prev(:) = x(:)
            do i = 1, n
                s = dot_product(a(i, 1:i-1), x_prev(1:i-1))
                s = s + dot_product(a(i, i+1:n), x_prev(i+1:n))
                x(i) = rd(i) * (b(i) - s)
            end do
            r(1:n) = b(1:n) - matmul(a, x)
            er = dot_product(r, r)
            write(*, *) 'itr =', itr, 'err =', er
            if (er <= er0) then
                write(*, *) '# converged #'
                exit
            endif
        end do
    end subroutine jacobi

    subroutine gauss_seidel(a, b, x, n, itrmax, er0)
        integer, intent(in) :: n, itrmax
        double precision, intent(in) :: a(n, n), b(n), er0
        double precision, intent(out) :: x(n)
        double precision s, er, rd(n), r(n)
        integer i, itr
        do i = 1, n
            if (a(i, i) == 0.0d0) stop 'a(i, i) == 0.0d0'
            rd(i) = 1.0d0 / a(i, i)
        end do
        x(1:n) = 0.0d0
        do itr = 1, itrmax
            do i = 1, n
                s = dot_product(a(i, 1:i-1), x(1:i-1))
                s = s + dot_product(a(i, i+1:n), x(i+1:n))
                x(i) = rd(i) * (b(i) - s)
            end do
            r(1:n) = b(1:n) - matmul(a, x)
            er = dot_product(r, r)
            write(*, *) 'itr =', itr, 'err =', er
            if (er <= er0) then
                write(*, *) '# converged #'
                exit
            endif
        end do
    end subroutine gauss_seidel

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

    subroutine alloc_dd_mat(a, b, x, n)
        integer, intent(in) :: n
        double precision, allocatable, intent(out) :: a(:, :), b(:), x(:)
        allocate(a(n, n), b(n), x(n))
        call set_dd_mat(a)
        call random_number(b)
    end subroutine alloc_dd_mat
end module subprogs

program main
    use subprogs
    implicit none
    double precision, allocatable :: a(:, :), b(:), x(:)
    integer :: n = 3, itrmax = 100
    double precision :: er0 = 1.0d-6
    call alloc_dd_mat(a, b, x, n)
    write(*, *) '### Jacobi method ###'
    call jacobi(a, b, x, n, itrmax, er0)
    write(*, *) 'x :', x
    write(*, *)
    write(*, *) '### Gauss-Seidel method ###'
    call gauss_seidel(a, b, x, n, itrmax, er0)
    write(*, *) 'x :', x
end program main