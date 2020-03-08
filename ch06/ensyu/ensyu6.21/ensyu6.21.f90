module global
    implicit none
    integer, parameter :: t_max = 20000
    double precision, parameter :: dt = 5.0e-4
    double precision, parameter :: kappa = 0.01d0, L = 1.0d0, pi = acos(-1.0d0)
end module global

module subprogs
    use global
    implicit none
contains
    function T_theory(x, m) result(Temp)
        ! theoretical distribution of Temperature
        integer, intent(in) :: m
        double precision, intent(in) :: x(m+1)
        double precision :: Temp(0:10, m+1), delta_Temp(m+1), lambda2
        integer, parameter :: n = 50 ! the number of terms of series sum
        double precision sign, coef, el2t, n_odd
        integer :: i_n, t
        Temp(:, :) = 0.0d0
        do t = 0, 10
            do i_n = 1, n
                n_odd = dble(2 * i_n - 1)
                sign = (-1.0d0) ** (i_n - 1)
                coef = 1.0d0 / (n_odd ** 2)
                lambda2 = kappa * ( n_odd * pi / L ) ** 2
                el2t = exp(-lambda2 * dble(t))
                delta_Temp(:) = sign * coef * el2t * sin(n_odd * pi * x(:) / L)
                Temp(t, :) = Temp(t, :) + delta_Temp(:)
            enddo
            Temp(t, :) = Temp(t, :) * 4.0d0 * L / (pi ** 2)
        enddo
    end function T_theory

    subroutine set_x(x, m)
        integer, intent(in) :: m
        double precision, intent(out) :: x(m+1)
        integer i
        double precision dx
        dx = L / dble(m)
        do i = 1, m+1
            x(i) = dx * dble(i - 1)
        end do 
    end subroutine set_x

    subroutine set_init_Temp(Temp, x, m)
        integer, intent(in) :: m
        double precision, intent(in) :: x(m+1)
        double precision, intent(out) :: Temp(0:t_max, m+1)
        integer i
        Temp(0, 1) = 0.0d0
        do i = 2, m/2
            Temp(0, i) = x(i)
        end do
        do i = m/2 + 1, m
            Temp(0, i) = L - x(i)
        end do
        Temp(0, m+1) = 0.0d0
    end subroutine set_init_Temp

    function chk_er(Temp) result(er)
        double precision, intent(in) :: Temp(:, :)
        double precision er, d_Temp(size(Temp, 2))
        integer t
        er = 0.0d0
        do t = 0, t_max - 1
            d_Temp(:) = Temp(t+1, :) - Temp(t, :)
            er = er + sum(d_Temp(:)) ** 2
        end do
        write(*, *) 'er :', er
    end function chk_er

    subroutine output(Temp_at_t, x, m, filename)
        integer, intent(in) :: m
        double precision, intent(in) :: Temp_at_t(m+1), x(m+1)
        character(*) :: filename
        integer i
        integer :: fno = 1
        open(fno, file=filename)
        do i = 1, m+1
            write(fno, *) x(i), Temp_at_t(i)
        end do
        close(fno)
    end subroutine output

    subroutine heat_conduction_sor(Temp, x, m, n_step, er0)
        integer, intent(in) :: m, n_step
        double precision, intent(out) :: Temp(0:t_max, m+1)
        double precision, intent(in) :: x(m+1), er0
        double precision dx, k, er, Temp_cp(0:t_max, m+1)
        integer t, i, i_step
        character(2) :: filename
        dx = L / dble(m)
        k = kappa * dt / dx ** 2
        write(*, *) 'k =', k
        Temp(:, :) = 0.0d0
        call set_init_Temp(Temp, x, m)
        Temp_cp(:, :) = Temp(:, :)
        do i_step = 1, n_step
            do i = 2, m
                do t = 0, t_max - 1
                    Temp(t+1, i) = Temp(t, i) &
                    + k * (Temp(t, i+1) - 2.0d0 * Temp(t, i) + Temp(t, i-1))
                end do
            end do
            !er = chk_er(Temp)
            ! if (er < er0) exit
        end do
        do t = 0, t_max
            if (mod(t, t_max / 10) == 0) then
                write(filename, '(i2.2)') t / (t_max / 10)
                call output(Temp(t, :), x, m, filename)
            end if
        end do
    end subroutine heat_conduction_sor
end module subprogs

program main
    use subprogs
    implicit none
    integer, parameter :: m = 20, n_step = 100
    double precision :: Temp(0:t_max, m+1), Temp_theory(0:10, m+1), x(m+1)
    double precision, parameter :: er0 = 1.0e-6
    integer t
    character(7) :: theory = 'theory_'
    character(3) :: t_str
    character(10) :: filename
    Temp(:, :) = 0.0d0
    call set_x(x, m)
    call heat_conduction_sor(Temp, x, m, n_step, er0)
    Temp_theory(:, :) = T_theory(x, m)
    do t = 0, 10
        write(t_str, '(i2.2)') t
        filename = theory // t_str
        call output(Temp_theory(t, :), x, m, filename)
    end do
end program main