module subprogs
    implicit none 
contains
    subroutine print_imat(ia)
        integer, intent(in) :: ia(:, :)
        integer i
        do i = 1, size(ia, 1)
            write(*, *) ia(i, 1:size(ia, 2))
        end do
    end subroutine print_imat 
end module subprogs 

program main
    use subprogs 
    implicit none 
    integer i, j, ia(3, 3)
    do i = 1, 3
        ia(i, 1:3) = (/ (10 * i + j, j = 1, 3) /)
    end do 
    call print_imat(ia(:, :))
    call print_imat(ia(2:3, 2:3))
end program main 