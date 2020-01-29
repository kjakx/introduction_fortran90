module ensyu3_20
    implicit none
contains 
    function determinant(A) result(det)
        double precision, intent(in) :: A(2, 2)
        double precision det
        det = A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1)
    end function determinant
end module ensyu3_20

program main
    use ensyu3_20
    implicit none
    double precision A(2, 2), B(2, 2), AB(2, 2), det_A, det_B, det_AB
    call random_number(A)
    call random_number(B)
    AB = matmul(A, B)
    det_A = determinant(A)
    det_B = determinant(B)
    det_AB = determinant(AB)
    write(*, *) 'det_A * det_B =', det_A * det_B
    write(*, *) 'det_AB =', det_AB
end program main