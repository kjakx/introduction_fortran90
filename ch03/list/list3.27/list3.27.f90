module list3_27
    implicit none
contains 
    function eval2x2mat(a) result(eval)
        double precision, intent(in) :: a(:, :)
        complex(8) eval(2)
        double precision b, c, d, e
        if (size(a, 1) /= size(a, 2)) stop 'not square'
        if (size(a, 1) /= 2) stop 'not 2x2 matrix'
        b = -0.5d0 * (a(1, 1) + a(2, 2))
        c = a(1, 1) * a(2, 2) - a(1, 2) * a(2, 1)
        d = b ** 2 - c
        if (d < 0.0d0) then
            eval(1) = cmplx(-b, sqrt(-d), kind = 8)
            eval(2) = conjg(eval(1))
        else if (d > 0.0d0) then
            ! sign(real1, real2) is a function that returns real2-signed real1.
            ! To avoid a digit error, if b is positive, e is to be a sum of two negative values.
            ! If b is negative, sign(sqrt(d)) is a positive, and so e is a sum of two positives.
            e = -b + sign(sqrt(d), -b)
            eval(1) = cmplx(e, 0.0d0, kind = 8)
            ! use x2 = c / x1
            eval(2) = cmplx(c / e, 0.0d0, kind = 8)
        else
            eval(1) = cmplx(-b, 0.0d0, kind = 8)
            eval(2) = eval(1)
        end if
    end function eval2x2mat
end module list3_27

program main
    use list3_27
    implicit none
    double precision A(2, 2)
    complex(8) eval(2)
    A(1, :) = (/ -1,  1 /)
    A(2, :) = (/ -1, -1 /)
    eval = eval2x2mat(A)
    write(*, *) 'eval :', eval
end program main