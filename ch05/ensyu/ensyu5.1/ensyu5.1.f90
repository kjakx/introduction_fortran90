module subprogs
    implicit none
contains
    function determinant(A) result(det)
        double precision, intent(in) :: A(2, 2)
        double precision det
        det = A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1)
    end function determinant

    function outer_product(a, b) result(outer)
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
    end function outer_product

    function outer_norm(a, b, alpha) result(outer)
        double precision, intent(in) :: a(:), b(:)
        double precision, intent(in), optional :: alpha
        double precision :: outer(size(a))
        double precision vecl, factor
        outer = outer_product(a, b)
        vecl = dot_product(outer, outer)
        if (present(alpha)) then
            factor = alpha
        else
            factor = 1.0d0
        end if
        if (vecl == 0.0d0) then
            outer(:) = vecl * outer(:)
        else
            outer(:) = outer(:) / sqrt(vecl)
            outer(:) = factor * outer(:)
        end if
    end function outer_norm
end module subprogs