module subprog 
    implicit none 
contains
    subroutine swap(a, b)
        integer, intent(out) :: a, b 
        integer tmp
        tmp = a 
        a = b 
        b = tmp
    end subroutine swap 
end module subprog 

program exchange
    use subprog
    implicit none 
    integer :: x = 77, y = 9095, tmp = 0
    write(*, *) "x, y, tmp = ", x, y, tmp
    call swap(x, y)
    write(*, *) "x, y, tmp = ", x, y, tmp
end program exchange