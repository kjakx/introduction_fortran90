program quad_eq
    implicit none
    double precision a, b, c, D, x1_re, x1_im, x2_re, x2_im
    write(*, *) 'input a, b, c:'
    read(*, *) a, b, c
    D = b * b - 4.0d0 * a * c
    if (D >= 0) then
        if (abs(sqrt(D) - b) > abs(- sqrt(D) - b)) then
            x1_re = (- b + sqrt(D)) / (2.0d0 * a)
            x2_re = - (c / a) / x1_re
        else
            x2_re = (- b - sqrt(D)) / (2.0d0 * a)
            x1_re = - (c / a) / x2_re
        endif
    else ! (D < 0)
        x1_re = -b / (2.0d0 * a)
        x1_im = sqrt(-D) / (2.0d0 * a)
        x2_re = x1_re
        x2_im = -x1_im
    endif
    write(*, *) 'D =', D
    if (D < 0) then
        write(*, *) 'x1 =', cmplx(x1_re, x1_im, 8)
        write(*, *) 'x2 =', cmplx(x2_re, x2_im, 8)
    else
        write(*, *) 'x1 =', x1_re
        write(*, *) 'x2 =', x2_re    
    endif
end program quad_eq
