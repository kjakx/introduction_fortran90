module interface_mod
    implicit none
    interface
        subroutine gauss_jordan_pv(a0, x, b, n)
            integer, intent(in) :: n
            double precision, intent(in) :: a0(n, n) , b(n)
            double precision, intent(out) :: x(n)
        end subroutine gauss_jordan_pv

        subroutine set_random_ab(a, b, x, n)
            integer, intent(in) :: n
            double precision, allocatable, intent(out) :: a(:, :), b(:), x(:)
        end subroutine set_random_ab
    end interface
end module interface_mod