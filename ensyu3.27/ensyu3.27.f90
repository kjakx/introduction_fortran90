module mat_subprogs
    implicit none
contains 
    subroutine print_rmatc(a, name) 
        character(*), intent(in) :: name
        double precision, intent(in) :: a(:, :)
        integer i, n, m
        n = size(a, 1)
        m = size(a, 2)
        write(*, *) name
        do i = 1, n 
            write(*, '(100e12.4)') a(i, 1:m)
        end do 
    end subroutine print_rmatc
    subroutine print_imatc(ia, name) 
        character(*), intent(in) :: name
        integer, intent(in) :: ia(:, :)
        integer i, n, m
        n = size(ia, 1)
        m = size(ia, 2)
        write(*, *) name
        do i = 1, n 
            write(*, *) ia(i, 1:m)
        end do 
    end subroutine print_imatc
end module mat_subprogs 

program random_mat
    use mat_subprogs 
    implicit none
    double precision, allocatable :: a(:, :)
    integer, allocatable :: ia(:, :)
    integer n, m
    write(*, *) 'input n, m:'
    read(*, *) n, m 
    write(*, *) 'n =', n
    write(*, *) 'm =', m 
    if (n * m < 1 .or. n * m > 100) stop 'stop. n or m must be 0 < (n, m) < 100'
    allocate(a(n, m))
    call random_number(a(:, :))
    ia = int(a * 10. + 1.) ! random integer from 1 to 10
    call print_rmatc(a, 'matrix a')
    call print_imatc(ia, 'matrix ia')
end program random_mat