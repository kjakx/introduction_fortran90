module subprogs
    implicit none
contains
    function phi_theory(x, n1, n2) result(phi)
        integer, intent(in) :: n1, n2
        double precision, intent(in) :: x(2, n1, n2)
        double precision phi(n1, n2)
        integer i, j
        double precision, parameter :: pi = acos(-1.0d0)
        do j = 1, n2
            do i = 1, n1
                phi(i, j) = sin(pi * x(1, i, j)) * sinh(pi * (1.0d0 - x(2, i, j))) / sinh(pi)
            end do
        end do
    end function phi_theory

    subroutine set_gridx(x, n1, n2)
        integer, intent(in) :: n1, n2 
        double precision, intent(out) :: x(:, :, :)
        integer i, j
        double precision dx1, dx2
        dx1 = 1.0d0 / dble(n1 - 1)
        dx2 = 1.0d0 / dble(n2 - 1)
        ! set x(m, i, j)
        do i = 1, n1
            do j = 1, n2
                x(1, i, j) = dx1 * dble(i - 1)
                x(2, i, j) = dx2 * dble(j - 1)
            end do 
        end do 
    end subroutine set_gridx

    subroutine set_dbc(phi, x, n1, n2)
        integer, intent(in) :: n1, n2
        double precision, intent(in) :: x(2, n1, n2)
        double precision, intent(out) :: phi(n1, n2)
        integer i, j
        double precision, parameter :: pi = acos(-1.0d0)
        do i = 1, n1
            phi(i, 1) = sin(pi * x(1, i, 1))
            phi(i, n2) = 0.0d0
        end do
        do j = 2, n2 - 1
            phi(1, j) = 0.0d0
            phi(n1, j) = 0.0d0
        end do
    end subroutine set_dbc

    subroutine set_nbc(phi, n1, n2)
        integer, intent(in) :: n1, n2
        double precision, intent(out) :: phi(n1, n2)
        integer i
        do i = 1, n1
            phi(i, n2) = phi(i, n2-1)
        end do
    end subroutine set_nbc

    function chk_err(phi, x, n1, n2) result(er)
        double precision, intent(in) :: phi(:, :), x(:, :, :)
        integer, intent(in) :: n1, n2
        double precision er, d_phi(n1, n2)
        d_phi(:, :) = phi_theory(x, n1, n2) - phi(:, :)
        er = sum(d_phi(:, :)) ** 2
        write(*, *) 'er :', er
    end function chk_err

    subroutine output(phi, x, n1, n2, filename)
        integer, intent(in) :: n1, n2
        double precision, intent(in) :: phi(:, :), x(:, :, :)
        character(*) :: filename
        integer i, j
        integer :: fno = 1
        open(fno, file=filename)
        do i = 1, n1
            do j = 1, n2
                write(fno, *) x(:, i, j), phi(i, j)
            end do
            write(fno, *)
        end do
        close(fno)
    end subroutine output

    subroutine time_dependent_dispersion_explicit(phi, x, n1, n2, dt, alpha, n_step, p_step, er0)
        integer, intent(in) :: n1, n2, n_step, p_step
        double precision, intent(out) :: phi(n1, n2), x(2, n1, n2)
        double precision, intent(in) :: dt, alpha, er0
        double precision dx1, dx2, d1, d2, er, phi_next(n1, n2)
        integer i, j, i_step
        integer :: p_count = 0
        character(10000) :: filename
        dx1 = 1.0d0 / dble(n1 - 1)
        dx2 = 1.0d0 / dble(n2 - 1)
        d1 = alpha * dt / dx1 ** 2
        d2 = alpha * dt / dx2 ** 2
        phi(2:n1-1, 2:n2-1) = 0.0d0
        call set_dbc(phi, x, n1, n2)
        ! call set_nbc(phi, n1, n2)
        phi_next(2:n1-1, 2:n2-1) = 0.0d0
        call set_dbc(phi_next, x, n1, n2)
        ! call set_nbc(phi_next, n1, n2)
        do i_step = 1, n_step
            do j = 2, n2 - 1
                do i = 2, n1 - 1
                    phi_next(i, j) = phi(i, j) &
                    + d1 * (phi(i-1, j) - 2.0d0 * phi(i, j) + phi(i+1, j)) &
                    + d2 * (phi(i, j-1) - 2.0d0 * phi(i, j) + phi(i, j+1))
                end do
            end do
            ! call set_nbc(phi_next, n1, n2)
            er = chk_err(phi_next, x, n1, n2)
            phi(:, :) = phi_next(:, :)
            if (er < er0) exit
        end do
    end subroutine time_dependent_dispersion_explicit

        subroutine time_dependent_dispersion_implicit(phi, x, n1, n2, dt, alpha, ohm, n_step, p_step, er0)
        integer, intent(in) :: n1, n2, n_step, p_step
        double precision, intent(out) :: phi(n1, n2), x(2, n1, n2)
        double precision, intent(in) :: dt, alpha, ohm, er0
        double precision dx1, dx2, d1, d2, gamma, e, f, rhs, er
        integer i, j, i_step
        integer :: p_count = 0
        character(10000) :: filename
        dx1 = 1.0d0 / dble(n1 - 1)
        dx2 = 1.0d0 / dble(n2 - 1)
        d1 = alpha * dt / dx1 ** 2
        d2 = alpha * dt / dx2 ** 2
        gamma = (1.0d0 + 2.0d0 * d1 + 2.0d0 * d2) ** (-1.0d0)
        e = - gamma * d1
        f = - gamma * d2
        phi(2:n1-1, 2:n2-1) = 0.0d0
        call set_dbc(phi, x, n1, n2)
        ! call set_nbc(phi, n1, n2)
        do i_step = 1, n_step
            do j = 2, n2 - 1
                do i = 2, n1 - 1
                    rhs = - e * (phi(i-1, j) + phi(i+1, j)) &
                          - f * (phi(i, j-1) + phi(i, j+1))
                    phi(i, j) = phi(i, j) + ohm * (rhs - (1.0d0 - gamma) * phi(i, j))
                end do
            end do
            ! call set_nbc(phi_prev, n1, n2)
            er = chk_err(phi, x, n1, n2)
            if (er < er0) exit
        end do
    end subroutine time_dependent_dispersion_implicit
end module subprogs

program main
    use subprogs
    implicit none
    integer, parameter :: n1 = 10, n2 = 10, n_step = 100, p_step = 1
    double precision :: phi(n1, n2), x(2, n1, n2)
    double precision, parameter :: pi = acos(-1.0d0)
    double precision, parameter :: er0 = 1.0e-2
    double precision :: alpha = 0.5d0, ohm = 2.0d0 / (1.0d0 + sin(pi / (n1 - 1)))
    double precision :: t_start, t_end, t_implicit, t_explicit
    call set_gridx(x, n1, n2)
    ! euler explicit method
    phi(:, :) = 0.0d0
    write(*, *) '### explicit method ###'
    call cpu_time(t_start)
    call time_dependent_dispersion_explicit(phi, x, n1, n2, dble(5.0e-4), alpha, n_step, p_step, er0)
    call cpu_time(t_end)
    t_explicit = t_end - t_start
    ! euler implicit method
    phi(:, :) = 0.0d0
    write(*, *) '### implicit method ###'
    call cpu_time(t_start)
    call time_dependent_dispersion_implicit(phi, x, n1, n2, dble(1.0e-0), alpha, ohm, n_step, p_step, er0)
    call cpu_time(t_end)
    t_implicit = t_end - t_start
    ! call output(phi, x, n1, n2, filename='theory')
    write(*, *) 'explicit method:', t_explicit
    write(*, *) 'implicit method:', t_implicit
end program main