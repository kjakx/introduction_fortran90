module interface_mod
    implicit none
    interface
        function inverse_gj_pv(AI0, n) result(AI)
            integer, intent(in) :: n
            double precision, intent(in) :: AI0(n, 2*n)
            double precision AI(n, 2*n)
        end function inverse_gj_pv
    end interface
end module interface_mod