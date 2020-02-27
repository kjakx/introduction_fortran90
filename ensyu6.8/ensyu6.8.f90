module subprogs
    implicit none
contains
    subroutine gaussian_elimination_pv_det(a0, x, b, n, det_a)
        ! --- gaussian elimination with partial pivoting ---
        integer, intent(in) :: n
        double precision, intent(in) :: a0(n, n), b(n)
        double precision, intent(out) :: x(n), det_a
        integer i, j, k, m
        double precision ar, am, a(n, n), t, w(n), alpha, D
        a(:, :) = a0(:, :)
        x(:) = b(:)
        det_a = 0.0d0
        alpha = 1.0d0
        D = 1.0d0
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
                alpha = alpha * (-1.0d0)
            end if
            ! --- end partial pivoting ---
            ar = 1.0d0 / a(k, k)
            a(k, k) = 1.0d0
            a(k, k+1:n) = ar * a(k, k+1:n)
            x(k) = ar * x(k)
            alpha = alpha * ar
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
        do i = 1, n
            D = D * a(i, i)
        end do
        det_a = D * alpha
    end subroutine gaussian_elimination_pv_det

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
    double precision, allocatable :: a(:, :), b(:), x(:), r(:)
    integer :: n = 3
    double precision det_a
    call set_random_ab(a, b, x, n)
    call gaussian_elimination_pv_det(a, x, b, n, det_a)
    allocate(r(n))
    r(:) = b(:) - matmul(a, x)
    write(*, *) 'Gaussian elimination(pv) error =', dot_product(r, r)
    deallocate(a, b, x)
    write(*, *) 'det_a =', det_a
end program main
