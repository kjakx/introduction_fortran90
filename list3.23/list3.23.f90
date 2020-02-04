module params
    implicit none 
    integer :: n = 2
end module params

module list3_23
    implicit none
contains
    subroutine swapvec3(x, y)
        use params
        integer, intent(inout) :: x(n), y(n)
        integer tmp(n)
        if (n /= size(y)) stop 'stop. size(x) /= size(y)'
        tmp(1:n) = x(1:n)
        x(1:n) = y(1:n)
        y(1:n) = tmp(1:n)
    end subroutine swapvec3 
end module list3_23

program main
    use list3_23
    implicit none
    integer :: x(3) = (/1, 2, 3/)
    integer :: y(3) = (/4, 5, 6/)
    write(*, *) 'x :', x
    write(*, *) 'y :', y
    call swapvec3(x, y)
    write(*, *) 'x :', x
    write(*, *) 'y :', y
end program main