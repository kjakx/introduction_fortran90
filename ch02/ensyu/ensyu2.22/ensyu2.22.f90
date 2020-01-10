program submatrix
    integer n, i, j, x, y
    double precision, allocatable :: a(:, :), b(:, :)
    ! input n
    write(*, *) "input n:"
    read(*, *) n 
    write(*, *) "n =", n 
    ! input i, j
    write(*, *) "input i, j:"
    read(*, *) i, j
    write(*, *) "i =", i 
    write(*, *) "j =", j 
    allocate(a(n, n), b(n-1, n-1))
    call random_number(a)
    ! submatrix
    b(1:i-1, 1:j-1) = a(1:i-1, 1:j-1)
    b(1:i-1, j:n-1) = a(1:i-1, j+1:n)
    b(i:n-1, 1:j-1) = a(i+1:n, 1:j-1)
    b(i:n-1, j:n-1) = a(i+1:n, j+1:n)
    write(*, *) "a :"
    do x = 1, n
        write(*, *) (a(x, y), y = 1, n)
    end do 
    write(*, *) "b :"
    do x = 1, n-1
        write(*, *) (b(x, y), y = 1, n-1)
    end do 
end program submatrix