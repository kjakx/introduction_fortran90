module interface_mod
    implicit none
    interface
        subroutine gauss_jordan_pv_det(a0, x, b, n, det_a)
            integer, intent(in) :: n
            double precision, intent(in) :: a0(n, n) , b(n)
            double precision, intent(out) :: x(n), det_a
        end subroutine gauss_jordan_pv_det

        subroutine set_random_ab(a, b, x, n)
            integer, intent(in) :: n
            double precision, allocatable, intent(out) :: a(:, :), b(:), x(:)
        end subroutine set_random_ab
    end interface
end module interface_mod