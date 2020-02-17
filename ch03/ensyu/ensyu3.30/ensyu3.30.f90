module ensyu3_30
    implicit none
contains 
    function t(A) result(A_t)
        double precision, intent(in) :: A(:, :)
        double precision, allocatable :: A_t(:, :)
        integer n, m, i, j
        n = size(A, 1)
        m = size(A, 2)
        if (n /= m) stop 'stop. A must be square matrix'
        allocate(A_t(n, n))
        do j = 1, n 
            do i = 1, n 
                A_t(i, j) = A(j, i)
            end do 
        end do 
    end function t

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
end module ensyu3_30 

program main 
    use ensyu3_30
    implicit none
    double precision, allocatable :: A(:, :)
    integer n 
    write(*, *) 'input n :'
    read(*, *) n 
    write(*, *) 'n =', n 
    allocate(A(n, n))
    call random_number(A)
    call print_rmatc(A, 'A :')
    call print_rmatc(t(A), 't(A) :')
    call print_rmatc(transpose(A), 'transpose(A) :')
end program main
    
