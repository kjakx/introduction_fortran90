module list3_21
    implicit none
contains
    subroutine swapvec(x, y)
        integer, intent(inout) :: x(:), y(:)
        integer, allocatable :: tmp(:)
        integer n 
        n = size(x)
        if (n /= size(y)) stop 'stop. size(x) /= size(y)'
        allocate(tmp(n))
        tmp(1:n) = x(1:n)
        x(1:n) = y(1:n)
        y(1:n) = tmp(1:n)
        deallocate(tmp)
    end subroutine swapvec 
end module list3_21

program main
    use list3_21
    implicit none
    integer :: x(3) = (/1, 2, 3/)
    integer :: y(3) = (/4, 5, 6/)
    write(*, *) 'x :', x
    write(*, *) 'y :', y
    call swapvec(x, y)
    write(*, *) 'x :', x
    write(*, *) 'y :', y
end program main