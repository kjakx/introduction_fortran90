program quad_eq
    implicit none
    double precision a, b, c, D
    double complex x1, x2
    write(*, *) 'input a, b, c:'
    read(*, *) a, b, c
    D = b * b - 4.0d0 * a * c
    if (abs(sqrt(D) - b) > abs(- sqrt(D) - b)) then
        x1 = (- b + sqrt(D)) / (2.0d0 * a)
        x2 = - (c / a) / x1
    else
        x2 = (- b - sqrt(D)) / (2.0d0 * a)
        x1 = - (c / a) / x2
    endif
    write(*, *) 'D =', D
    if (D < 0) then
        write(*, *) 'x1 =', x1
        write(*, *) 'x2 =', x2
    else
        write(*, *) 'x1 =', real(x1)
        write(*, *) 'x2 =', real(x2)    
    endif
end program quad_eq