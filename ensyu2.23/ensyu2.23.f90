program i_j_cofactor
    integer i, j, x, y
    double precision det_b, cofactor
    double precision a(3, 3), b(2, 2)
    ! input i, j
    write(*, *) "input i, j:"
    read(*, *) i, j
    write(*, *) "i =", i 
    write(*, *) "j =", j 
    call random_number(a)
    ! submatrix
    b(1:i-1, 1:j-1) = a(1:i-1, 1:j-1)
    b(1:i-1, j:2) = a(1:i-1, j+1:3)
    b(i:2, 1:j-1) = a(i+1:3, 1:j-1)
    b(i:2, j:2) = a(i+1:3, j+1:3)
    write(*, *) "a :"
    do x = 1, 3
        write(*, *) (a(x, y), y = 1, 3)
    end do 
    write(*, *) "b :"
    do x = 1, 2
        write(*, *) (b(x, y), y = 1, 2)
    end do 
    det_b = b(1, 1) * b(2, 2) - b(1, 2) * b(2, 1)
    cofactor = (-1.0d0)**(i+j) * det_b
    write(*, *) "(i, j)cofactor:", cofactor 
end program i_j_cofactor