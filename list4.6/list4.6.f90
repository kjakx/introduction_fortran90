module interface_mod
    interface
        function normal_vec(v) result(nv)
            double precision, intent(in) :: v(:)
            double precision nv(size(v))
        end function normal_vec
    end interface
end module interface_mod