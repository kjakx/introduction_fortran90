module mat_subprogs
    implicit none
contains 
    subroutine allocate_rmat(a)
        double precision, allocatable, intent(out) :: a(:, :)
        integer n
        write(*, *) 'input n:'
        read(*, *) n
        if (n < 1 .or. n > 100) stop 'n must be 0 < n < 101'
        allocate(a(n, n))
        call random_number(a)
    end subroutine allocate_rmat 
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
    double precision, allocatable :: a(:, :)
    call allocate_rmat(a)
    call print_mat2(a)
end program random_mat