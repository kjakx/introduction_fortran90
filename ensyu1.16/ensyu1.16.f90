program disp_rel
    real(8) :: L, T, h, g = 9.80665, pi = 3.14159265
    integer :: n, n_max = 100
    write(*, '(\a)') 'input T, h :'
    read(*, *) T, h
    if (T <= 0) stop 'stop. T must be T > 0'
    if (h <= 0) stop 'stop. h must be h > 0'
    L = h
    do n = 1, n_max
        L2 = T ** 2 * (g*L/(2*pi)*tanh(2*pi*h/L))
        L = sqrt(L2)
    enddo 
    write(*, *) 'L =', L
end program disp_rel
