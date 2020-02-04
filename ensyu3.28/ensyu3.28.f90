module ensyu3_28
    implicit none 
contains
    function identity(A) result(I)
        double precision, intent(in) :: A(:, :)
        double precision, allocatable :: I(:, :)
        integer j, n
        n = size(A, 1)
        allocate(I(n, n))
        I = 0.
        do j = 1, n 
            I(j, j) = 1.
        end do 
    end function identity 
end module ensyu3_28 

program main
    use ensyu3_28
    implicit none 
    double precision A(2, 2), I(2, 2)
    A(1, :) = (/ 1, 2 /)
    A(2, :) = (/ 3, 4 /)
    I = identity(A)
    write(*, *) 'I :'
    write(*, *) I(1, :)
    write(*, *) I(2, :)
end program main