module interface_mod
    implicit none
    interface
        function AB_gj_pv(AB0, n, m) result(B)
            integer, intent(in) :: n, m
            double precision, intent(in) :: AB0(n, n+m)
            double precision B(n, m)
        end function AB_gj_pv
    end interface
end module interface_mod