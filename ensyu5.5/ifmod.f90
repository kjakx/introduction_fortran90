module ex_gname_interface
    interface print_mat
        subroutine print_rmatc(a, name) 
            character(*), intent(in) :: name
            double precision, intent(in) :: a(:, :)
        end subroutine print_rmatc

        subroutine print_rmatc_nm(a, n, m, name) 
            character(*), intent(in) :: name
            integer, intent(in) :: n, m
            double precision, intent(in) :: a(n, m)
        end subroutine print_rmatc_nm

        subroutine print_imatc(ia, name) 
            character(*), intent(in) :: name
            integer, intent(in) :: ia(:, :)
        end subroutine print_imatc

        subroutine print_imatc_nm(ia, n, m, name) 
            character(*), intent(in) :: name
            integer, intent(in) :: n, m
            integer, intent(in) :: ia(n, m)
        end subroutine print_imatc_nm
    end interface
end module ex_gname_interface