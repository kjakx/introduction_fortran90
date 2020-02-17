module interface_mod
    interface
        subroutine allocate_rmat(a, n)
            integer, intent(in) :: n
            double precision, allocatable, intent(out) :: a(:, :)
        end subroutine allocate_rmat

        subroutine print_mat(a, n)
            integer, intent(in) :: n
            double precision, intent(in) :: a(n, n)
        end subroutine print_mat
    end interface
end module interface_mod