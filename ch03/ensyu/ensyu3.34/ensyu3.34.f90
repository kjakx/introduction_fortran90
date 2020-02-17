module ensyu3_34
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
        if (det == 0) stop 'stop. det(A) == 0'
    end function determinant

    function cofactor_matrix(A) result(A_cofac)
        double precision, intent(in) :: A(3, 3)
        double precision A_cofac(3, 3)
        integer i, j
        do j = 1, 3 
            do i = 1, 3 
                A_cofac(i, j) = i_j_cofactor(A, i, j)
            end do 
        end do
        A_cofac = transpose(A_cofac)
    end function cofactor_matrix 

    function inverse(A) result(A_inv)
        double precision, intent(in) :: A(3, 3)
        double precision A_inv(3, 3)
        A_inv = cofactor_matrix(A)
        A_inv = A_inv / determinant(A)
    end function inverse

    subroutine print_rmatc(a, name) 
        character(*), intent(in) :: name
        double precision, intent(in) :: a(:, :)
        integer i, n, m
        n = size(a, 1)
        m = size(a, 2)
        write(*, *) name
        do i = 1, n 
            write(*, '(100e12.4)') a(i, 1:m)
        end do 
    end subroutine print_rmatc
end module ensyu3_34

program main
    use ensyu3_34
    implicit none
    double precision A(3, 3), I(3, 3)
    call random_number(A)
    I = matmul(A, inverse(A))
    call print_rmatc(I, 'I :')
end program main