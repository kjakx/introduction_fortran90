module math_subprogs
    implicit none
contains
    recursive function factorial(n) result(m)
        integer, intent(in) :: n
        integer m
        if (n <= 1) then
            m = 1
        else
            m = n * factorial(n - 1)
        end if
    end function factorial
end module math_subprogs

program test_recursive
    use math_subprogs
    implicit none
    integer n, i, k
    write(*, *) 'input n(0 <= n <= 10) :'
    read(*, *) n
    if (n < 0 .or. n > 10) stop 'invalid n, bye'
    k = 1
    do i = 2, n
        k = k * i
    end do
    write(*, *) 'factorial =', k
    write(*, *) 'factorial =', factorial(n)
end program test_recursive