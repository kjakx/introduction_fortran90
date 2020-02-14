module mat_subprogs3
    implicit none
    double precision, allocatable, save :: a(:, :)
    integer, save :: n, m
contains
    subroutine allocate_rmat2
        write(*, *) 'input n, m :'
        read(*, *) n, m
        allocate(a(n, m))
        call random_number(a)
    end subroutine allocate_rmat2

    subroutine print_mat3
        integer i
        do i = 1, n
            write(*, '(100e12.4)') a(i, 1:m)
        end do
    end subroutine print_mat3
end module mat_subprogs3

program random_mod2
    use mat_subprogs3
    call allocate_rmat2
    call print_mat3
end program random_mod2