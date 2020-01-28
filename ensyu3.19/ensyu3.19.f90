module ensyu3_19
    implicit none
contains 
    function trace(A) result(tr)
        double precision, intent(in) :: A(:, :)
        double precision :: tr
        integer n, i
        n = size(A, 1)
        tr = 0
        do i = 1, n 
            tr = tr + A(i, i)
        end do 
    end function trace 
end module ensyu3_19 

program main 
    use ensyu3_19
    implicit none
    double precision, allocatable :: A(:, :)
    integer n, i
    write(*, *) 'input n:'
    read(*, *) n 
    write(*, *) 'n =', n 
    allocate(A(n, n))
    call random_number(A)
    write(*, *) 'A :'
    do i = 1, n 
        write(*, *) A(i, :)
    end do 
    write(*, *) 'tr(A) :', trace(A)
end program main