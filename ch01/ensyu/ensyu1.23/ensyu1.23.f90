program comp_elip_1st
    implicit none
    double precision :: K_n, K_prev, K_delta = 1.0d0, k , e , pi = acos(-1.0d0)
    integer :: i, n
    write(*, *) 'pi =', pi
    read(*, *) n, k
    if (n < 2) stop 'stop. n must be n >= 2'
    if (abs(k) >= 1) stop 'stop. k must be |k| < 1'
    K_prev = 1
    write(*, *) 'n, K(k), |e|'
    do i = 1, n - 1
        K_delta = K_delta * (dble(2 * i - 1) ** 2) / (dble(2 * i) ** 2) * k * k
        K_n = K_prev + K_delta
        e = pi / 2.0d0 * (K_n - K_prev)
        K_prev = K_n
        write (*, *) i + 1, (pi / 2.0d0 * K_n), abs(e)
    enddo
end program comp_elip_1st