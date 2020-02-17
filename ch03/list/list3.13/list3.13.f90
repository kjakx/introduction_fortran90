module sub_mod
    implicit none
contains
    subroutine print_column(x, m)
        integer, intent(in) :: m, x(m)
        write(*, *) x(1:m)
    end subroutine print_column 
end module sub_mod 

program main
    use sub_mod 
    implicit none 
    integer a(3, 3)
    a(1, 1:3) = (/ 11, 12, 13/)
    a(2, 1:3) = (/ 21, 22, 23/)
    a(3, 1:3) = (/ 31, 32, 33/)
    call print_column(a(1, 1), 3)
    call print_column(a(1, 2), 3)
end program main