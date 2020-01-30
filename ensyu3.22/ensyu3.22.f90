module ensyu3_22
    implicit none
contains 
    function i_j_cofactor(A, i, j) result(cofactor)
        integer, intent(in) :: i, j
        double precision, intent(in) :: A(3, 3)
        double precision sub(2, 2)
        double precision det_sub, cofactor
        ! submatrix
        sub(1:i-1, 1:j-1) = A(1:i-1, 1:j-1)
        sub(1:i-1, j:2) = A(1:i-1, j+1:3)
        sub(i:2, 1:j-1) = A(i+1:3, 1:j-1)
        sub(i:2, j:2) = A(i+1:3, j+1:3)
        det_sub = sub(1, 1) * sub(2, 2) - sub(1, 2) * sub(2, 1)
        cofactor = (-1.0d0)**(i+j) * det_sub
    end function i_j_cofactor
end module ensyu3_22

program main
    use ensyu3_22
    integer i, j, x, y
    double precision a(3, 3)
    ! input i, j
    write(*, *) "input i, j:"
    read(*, *) i, j
    write(*, *) "i =", i 
    write(*, *) "j =", j 
    call random_number(a)
    write(*, *) "a :"
    do x = 1, 3
        write(*, *) (a(x, y), y = 1, 3)
    end do 
    write(*, *) "(i, j)cofactor:", i_j_cofactor(a, i, j) 
end program main