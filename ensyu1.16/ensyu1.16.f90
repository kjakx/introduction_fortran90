program disp_rel
    real(8) :: L, T, h, g = 9.80665, pi = 3.14159265
    integer n, n_max = 100
    write(*, '(\a)') 'input L, h :'
    read(*, *) L, h
    if (L <= 0) stop 'stop. L must be L > 0'
    if (h <= 0) stop 'stop. h must be h > 0'
    L = h
    do n = 1, n_max
        