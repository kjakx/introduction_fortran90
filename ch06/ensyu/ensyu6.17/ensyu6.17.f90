module subprogs
    implicit none
contains
    function phi_theory(x, i, j, n1, n2) result(phi)
        integer, intent(in) :: i, j, n1, n2
        double precision, intent(in) :: x(2, n1, n2)
        double precision phi
        double precision, parameter :: pi = acos(-1.0d0)
        phi = sin(pi * x(1, i, j)) * sinh(pi * (1.0d0 - x(2, i, j))) / sinh(pi)
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

    subroutine chk_err(phi, c, d, n1, n2, er)
        double precision, intent(in) :: phi(:, :), c, d
        integer, intent(in) :: n1, n2
        double precision, intent(out) :: er
        double precision rhs
        integer i, j
        er = 0.0d0
        do j = 2, n2 - 1
            do i = 2, n1 - 1
                rhs = - c * (phi(i-1, j) + phi(i+1, j)) - d * (phi(i, j-1) + phi(i, j+1))
                er = er + (rhs - phi(i, j)) ** 2
            end do
        end do
    end subroutine chk_err

    subroutine laplace_sor(phi, x, n1, n2, ohm, itrmax, er0)
        integer, intent(in) :: n1, n2, itrmax
        double precision, intent(out) :: phi(n1, n2), x(2, n1, n2)
        double precision, intent(in) :: ohm, er0
        double precision dx1, dx2, c, d, rhs, er
        integer itr, i, j
        dx1 = 1.0d0 / dble(n1 - 1)
        dx2 = 1.0d0 / dble(n2 - 1)
        c = - 0.5 * dx2 ** 2 / (dx1 ** 2 + dx2 ** 2)
        d = - 0.5 * dx1 ** 2 / (dx1 ** 2 + dx2 ** 2)
        call set_dbc(phi, x, n1, n2)
        do itr = 1, itrmax
            do j = 2, n2 - 1
                do i = 2, n1 - 1
                    rhs = - c * (phi(i-1, j) + phi(i+1, j)) - d * (phi(i, j-1) + phi(i, j+1))
                    phi(i, j) = phi(i, j) + ohm * (rhs - phi(i, j))
                end do
            end do
            call chk_err(phi, c, d, n1, n2, er)
            if (er < er0) then
                write(*, *) ohm, itr
                exit
            end if
        end do
    end subroutine laplace_sor
end module subprogs

program main
    use subprogs
    implicit none
    integer, parameter :: n1 = 6, n2 = 6, itrmax = 100
    double precision :: phi(n1, n2), x(2, n1, n2)
    double precision :: ohm = 1.0d0, ohm0
    double precision, parameter :: er0 = 1.0e-6
    double precision, parameter :: pi = acos(-1.0d0)
    integer i
    ohm0 = 2.0d0 / (1.0d0 + sin(pi / dble(n1 - 1)))
    call set_gridx(x, n1, n2)
    do i = 1, 10
        phi(:, :) = 0.0d0
        ohm = 1.0d0 + 0.10d0 * (i - 1)
        call laplace_sor(phi, x, n1, n2, ohm, itrmax, er0)
    end do
    phi(:, :) = 0.0d0
    call laplace_sor(phi, x, n1, n2, ohm0, itrmax, er0)
end program main