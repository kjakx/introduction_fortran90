module list3_22
    implicit none
contains
    subroutine swapvec2(x, y, n)
        integer, intent(in) :: n 
        integer, intent(inout) :: x(n), y(n)
        integer tmp(n)
        if (n /= size(y)) stop 'stop. size(x) /= size(y)'
        tmp(1:n) = x(1:n)
        x(1:n) = y(1:n)
        y(1:n) = tmp(1:n)
    end subroutine swapvec2 
end module list3_22

program main
    use list3_22
    implicit none
    integer :: x(3) = (/1, 2, 3/)
    integer :: y(3) = (/4, 5, 6/)
    write(*, *) 'x :', x
    write(*, *) 'y :', y
    call swapvec2(x, y, 3)
    write(*, *) 'x :', x
    write(*, *) 'y :', y
end program main