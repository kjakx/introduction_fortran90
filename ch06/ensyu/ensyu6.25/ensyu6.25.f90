module subprogs
    implicit none
contains
    subroutine set_init(eta, x, dx, n1, n2, L, h0)
        integer, intent(in) :: n1, n2
        double precision, intent(in) :: L, h0
        double precision, intent(out) :: eta(:, :, :), x(:, :, :), dx(:)
        integer i, j
        double precision dh
        dh = 0.1d0 * h0
        dx(1) = L / dble(n1 - 1)
        dx(2) = L / dble(n2 - 1)
        do i = 1, n1
            do j = 1, n2
                x(1, i, j) = dx(1) * dble(i - 1)
                x(2, i, j) = dx(2) * dble(j - 1)
            end do
        end do
        eta(:, :, 1) = h0 + dh * exp(- ((x(1, :, :) - 0.5d0 * L) ** 2 + (x(2, :, :) - 0.5d0 * L) ** 2) / 1.0d2)
        eta(:, :, 2) = eta(:, :, 1) ! initial condition : eta_tt = 0 at t = 0
    end subroutine set_init

    subroutine output(eta, x, n1, n2, filename)
        integer, intent(in) :: n1, n2
        double precision, intent(in) :: eta(:, :), x(:, :, :)
        character(*) :: filename
        integer i, j
        integer :: fno = 1
        open(fno, file=filename)
        do i = 1, n1
            do j = 1, n2
                write(fno, *) x(:, i, j), eta(i, j)
            end do
            write(fno, *)
        end do
        close(fno)
    end subroutine output

    subroutine wave_equation_2d(eta, x, n1, n2, dx, dt, gr, h0, itrmax)
        integer, intent(in) :: n1, n2, itrmax
        double precision, intent(in) :: x(:, :, :), dx(:), dt, gr, h0
        double precision, intent(out) :: eta(:, :, :)
        integer itr, i, j
        double precision c(2), c0
        character(10) :: filename
        write(filename, '(i3.3)') 0
        call output(eta(:, :, 1), x, n1, n2, filename)
        c(1:2) = gr * h0 * dt ** 2 / dx(1:2) ** 2
        c0 = 2.0d0 * (1.0d0 - c(1) - c(2))
        do itr = 1, itrmax
            do j = 2, n2 - 1
                do i = 2, n1 - 1
                    eta(i, j, 3) = c0 * eta(i, j, 2) - eta(i, j, 1) &
                                + c(1) * (eta(i-1, j, 2) + eta(i+1, j, 2)) &
                                + c(2) * (eta(i, j-1, 2) + eta(i, j+1, 2))
                end do
            end do
            write(filename, '(i3.3)') itr
            call output(eta(:, :, 3 ), x, n1, n2, filename)
            eta(2:n1-1, 2:n2-1, 1) = eta(2:n1-1, 2:n2-1, 2)
            eta(2:n1-1, 2:n2-1, 2) = eta(2:n1-1, 2:n2-1, 3)
            ! boundary condition
            eta(1, :, :) = eta(2, :, :)
            eta(n1, :, :) = eta(n1-1, :, :)
            eta(:, 1, :) = eta(:, 2, :)
            eta(:, n2, :) = eta(:, n2-1, :)
        end do
    end subroutine wave_equation_2d
end module subprogs

program main
    use subprogs
    implicit none
    integer, parameter :: n1 = 101, n2 = 101, itrmax = 100
    double precision :: eta(n1, n2, 3), x(2, n1, n2), dx(2), dt = 0.1d0, gr = 9.8d0, h0 = 1.0d0, L = 100.0d0
    call set_init(eta, x, dx, n1, n2, L, h0)
    call wave_equation_2d(eta, x, n1, n2, dx, dt, gr, h0, itrmax)
end program main