module interface_mod
    interface
        subroutine allocate_rmat(a)
            double precision, allocatable, intent(out) :: a(:, :)
        end subroutine allocate_rmat

        subroutine print_mat(a)
            double precision, intent(in) :: a(:, :)
        end subroutine print_mat
    end interface
end module interface_mod