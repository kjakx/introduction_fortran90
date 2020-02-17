module ensyu3_24
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

    function determinant(A) result(det)
        double precision, intent(in) :: A(3, 3)
        double precision :: det
        integer i, k 
        do i = 1, 3
            det = 0
            do k = 1, 3 
                det = det + A(i, k) * i_j_cofactor(A, i, k)
            end do
        end do
    end function determinant 
end module ensyu3_24

program main
    use ensyu3_24
    integer i
    double precision a(3, 3), a_t(3, 3), a_tri(3, 3)
    double precision :: tr_a_tri = 1
    double precision det_a, det_a_t, det_a_tri
    call random_number(a)
    a_t = transpose(a)
    a_tri = 0.d0
    do i = 1, 3
        a_tri(i, i:3) = a(i, i:3)
        tr_a_tri = tr_a_tri * a_tri(i, i) 
    end do
    det_a = determinant(a)
    det_a_t = determinant(a_t)
    det_a_tri = determinant(a_tri)
    write(*, *) 'a :'
    do i = 1, 3
        write(*, *) a(i, :)
    end do 
    write(*, *) 'a_t :'
    do i = 1, 3
        write(*, *) a_t(i, :)
    end do 
    write(*, *) 'a_tri :'
    do i = 1, 3
        write(*, *) a_tri(i, :)
    end do 
    write(*, *) 'det(a) :', det_a 
    write(*, *) 'det(a_t) :', det_a_t
    write(*, *) 'det(a_tri) :', det_a_tri 
    write(*, *) 'tr(a_tri) :', tr_a_tri
end program main