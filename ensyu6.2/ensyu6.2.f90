module subprogs
    implicit none
contains
    subroutine gauss_jordan(a0, x, b, n)
        integer, intent(in) :: n
        double precision, intent(in) :: a0(n, n), b(n)
        double precision, intent(out) :: x(n)
        integer i, k
        double precision ar, a(n, n)
        a(:, :) = a0(:, :)
        x(:) = b(:)
        do k = 1, n
            if (a(k, k) == 0.0d0) stop 'pivot = 0'
            ar = 1.0d0 / a(k, k)
            a(k, k) = 1.0d0
            a(k, k+1:n) = ar * a(k, k+1:n)
            x(k) = ar * x(k)
            do i = 1, n
                if (i /= k) then
                    a(i, k+1:n) = a(i, k+1:n) - a(i, k) * a(k, k+1:n)
                    x(i) = x(i) - x(k) * a(i, k)
                    a(i, k) = 0.0d0
                end if
            end do
        end do
    end subroutine gauss_jordan

    subroutine gaussian_elimination(a0, x, b, n)
        integer, intent(in) :: n
        double precision, intent(in) :: a0(n, n), b(n)
        double precision, intent(out) :: x(n)
        integer i, j, k
        double precision ar, a(n, n)
        a(:, :) = a0(:, :)
        x(:) = b(:)

        ! --- upper trianglation ---
        do k = 1, n
            if (a(k, k) == 0.0d0) stop 'pivot = 0'
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
    end subroutine gaussian_elimination

    subroutine set_random_ab(a, b, x, n)
        integer, intent(in) :: n
        double precision, allocatable, intent(out) :: a(:, :), b(:), x(:)
        allocate(a(n, n), b(n), x(n))
        call random_number(a)
        call random_number(b)
    end subroutine set_random_ab
end module subprogs

program main
    use subprogs
    implicit none
    double precision, allocatable :: a(:, :), b(:), x(:)    ! r(:)
    integer n
    double precision t_start, t_end, t_gauss_jordan, t_gaussian_elimination
    read(*, *) n
    call set_random_ab(a, b, x, n)
    call cpu_time(t_start)
    call gauss_jordan(a, x, b, n)
    call cpu_time(t_end)
    t_gauss_jordan = t_end - t_start
    call cpu_time(t_start)
    call gaussian_elimination(a, x, b, n)
    call cpu_time(t_end)
    t_gaussian_elimination = t_end - t_start   
    write(*, *) n, t_gauss_jordan, t_gaussian_elimination
    deallocate(a, b, x)
end program main
