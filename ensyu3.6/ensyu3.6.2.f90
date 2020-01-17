program main
    implicit none 
    integer :: i = 1, j = 2, k 
    call tashizan(i, j, k)
    write(*, *) "i, j, k =", i, j, k
end program main

subroutine tashizan(a, b, c)
    implicit none
    integer, intent(in) :: a, b
    integer, intent(out) :: c
    a = 10
    c = a + b
end subroutine tashizan