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
    double precision, allocatable :: A(:, :), B(:, :), AB(:, :), BA(:, :)
    integer n, i
    double precision tr_AB, tr_BA
    write(*, *) 'input n:'
    read(*, *) n 
    write(*, *) 'n =', n 
    allocate(A(n, n), B(n, n), AB(n, n), BA(n, n))
    call random_number(A)
    call random_number(B)
    write(*, *) 'A :'
    do i = 1, n 
        write(*, *) A(i, :)
    end do 
    write(*, *) 'tr(A) :', trace(A)
    write(*, *) 'B :'
    do i = 1, n 
        write(*, *) B(i, :)
    end do 
    write(*, *) 'tr(B) :', trace(B)
    AB = matmul(A, B)
    BA = matmul(B, A)
    tr_AB = trace(AB)
    tr_BA = trace(BA)
    write(*, *) 'tr(AB) :', tr_AB 
    write(*, *) 'tr(BA) :', tr_BA 
    write(*, *) 'tr(AB) == tr(BA) :', tr_AB == tr_BA
end program main