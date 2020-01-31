module ensyu3_25
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
end module ensyu3_25

program main
    use ensyu3_25
    double precision a(3, 3), a_col_k(3, 3), a_row_k(3, 3), a_swap_2row(3, 3), a_k_sum(3, 3)
    double precision :: k = 2.0d0
    ! make random matrix a(3 x 3)
    call random_number(a)

    ! 2nd col is multiplied by k
    a_col_k = a 
    a_col_k(:, 2) = a_col_k(:, 2) * k
    
    ! 1st row is multiplied by k
    a_row_k = a 
    a_row_k(1, :) = a_row_k(1, :) * k 

    ! 1st and 2nd rows are swapped
    a_swap_2row = a 
    a_swap_2row(1, :) = a(2, :)
    a_swap_2row(2, :) = a(1, :)

    ! k multiplied 2nd row is added to 1st row
    a_k_sum = a
    a_k_sum(1, :) = a_k_sum(1, :) + a_k_sum(2, :) * k

    ! output
    write(*, *) 'det(a) :', determinant(a)
    write(*, *) 'det(a_col_k) :', determinant(a_col_k)
    write(*, *) 'det(a_row_k) :', determinant(a_row_k)
    write(*, *) 'det(a_swap_2row) :', determinant(a_swap_2row)
    write(*, *) 'det(a_k_sum) :', determinant(a_k_sum)

end program main