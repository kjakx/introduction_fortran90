module mat_subprogs
    implicit none
contains 
    subroutine print_mat2(a) 
        double precision, intent(in) :: a(:, :)
        integer i, n, m
        n = size(a, 1)
        m = size(a, 2)
        do i = 1, n 
            write(*, '(100e12.4)') a(i, 1:m)
        end do 
    end subroutine print_mat2 
end module mat_subprogs 

program random_mat
    use mat_subprogs 
    implicit none
    double precision a(-2:2, -3:3)
    call random_number(a(:, :))
    call print_mat2(a)
end program random_mat