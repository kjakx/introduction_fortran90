module subprogs
    implicit none
contains 
    subroutine print_ivec(iv, m)
        integer, intent(in) :: m, iv(m)
        write(*, *) iv(1:m)
    end subroutine print_ivec 
end module subprogs 

program main
    use subprogs 
    implicit none
    integer i, j, ia(3, 3)
    do i = 1, 3 
        ia(i, 1:3) = (/ (10 * i + j, j = 1, 3) /)
    end do 
    call print_ivec(ia(1:1, 1:3), 3)
    call print_ivec(ia(1, 1:3), 3)
    call print_ivec(ia(1:3, 1), 3)
    call print_ivec(ia(1, 1), 3)
end program main