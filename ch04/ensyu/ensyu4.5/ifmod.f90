module interface_mod
    implicit none
    interface
        subroutine set_dd_mat(a)
            double precision, intent(out) :: a(:, :)
        end subroutine set_dd_mat
        function chk_dd_mat(a) result(is_dd)
            double precision, intent(in) :: a(:, :)
            integer :: is_dd
        end function chk_dd_mat
    end interface
end module interface_mod