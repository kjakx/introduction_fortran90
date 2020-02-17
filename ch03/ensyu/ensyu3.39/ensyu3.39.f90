module ensyu3_39
    implicit none
contains
    function determinant(A) result(det)
        double precision, intent(in) :: A(2, 2)
        double precision det
        det = A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1)
    end function determinant

    function outer_product_321(a, b) result(outer)
        integer i
        double precision, intent(in) :: a(3), b(3)
        double precision c(2, 3), d(2, 2), outer(3)
        c(1, :) = a(:)
        c(2, :) = b(:)
        do i = 1, 3
            d(:, :) = c(:, 2:3)
            outer(i) = determinant(d)
            c = cshift(c, (/1, 1/), 2)
        end do 
    end function outer_product_321 

    function cross_product_207(a, b) result(cross)
        double precision, intent(in) :: a(3), b(3)
        double precision cross(3)
        integer :: i
        do i = 1, 3
            cross(i) = a(mod(i, 3) + 1) * b(mod(i + 1, 3) + 1) - a(mod(i + 1, 3) + 1) * b(mod(i, 3) + 1)
        end do
    end function cross_product_207

    function scolar_triple_product(a, b, c) result(p)
        double precision, intent(in) :: a(3), b(3), c(3)
        double precision p 
        p = dot_product(a, cross_product_207(b, c))
    end function scolar_triple_product

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

    function determinant_3d(A) result(det)
        double precision, intent(in) :: A(3, 3)
        double precision :: det
        integer i, k 
        do i = 1, 3
            det = 0
            do k = 1, 3 
                det = det + A(i, k) * i_j_cofactor(A, i, k)
            end do
        end do
    end function determinant_3d

    function normal_vec(v, n) result(nv)
        integer, intent(in) :: n
        real(8), intent(in) :: v(n)
        real(8) nv(n), vl
        vl = sqrt(dot_product(v, v))
        if (vl == 0.0d0) then
            nv(:) = 0.0d0 
        else
            nv(:) = v(:) / vl
        end if 
    end function normal_vec 

    function gs(a, n) result(e)
        integer, intent(in) :: n 
        double precision, intent(in) :: a(n, n)
        double precision e(n, n), dotp
        integer k, j
        e(1:n, 1) = normal_vec(a(1:n, 1:1), n)
        do k = 2, n
            e(1:n, k) = a(1:n, k)
            do j = 1, k - 1
                dotp = dot_product(a(1:n, k), e(1:n, j))
                e(1:n, k) = e(1:n, k) - dotp * e(1:n, j)
            end do
            e(1:n, k) = normal_vec(e(1:n, k:k), n)
        end do
    end function gs

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
end module ensyu3_39

program main
    use ensyu3_39
    implicit none
    double precision a(3, 3), b(3, 3), T(3, 3), c(3, 3)
    call random_number(a)
    call random_number(b)
    T = gs(b, 3)
    c = matmul(a, T)
    c = matmul(transpose(T), c)
    call print_rmatc(a, "a :")
    call print_rmatc(c, "c :")
    write(*, *) "det(A) :", determinant_3d(a)
    write(*, *) "det(C) :", determinant_3d(c)
end program main
