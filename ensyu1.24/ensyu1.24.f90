program rod_temp_dist
    implicit none
    double precision :: Temp_t_x = 0.0d0, delta_Temp, x, delta_x, lambda2, kappa = 0.01, L = 1.0d0, pi = acos(-1.0d0)
    double precision sign, coef, el2t, n_odd
    character*12 t_ch
    integer :: i_n, i_x, t, n = 50, m = 20, fo = 11
    delta_x = L / dble(m)
    do t = 0, 10
        write(t_ch, '(i2)') t
        open(fo, file="Temp_dist_at_t="//t_ch)
        do i_x = 1, m + 1
            x = delta_x * dble(i_x - 1)
            do i_n = 1, n 
                n_odd = dble(2 * i_n - 1)
                sign = -1.0d0 ** (i_n - 1)
                coef = 1.0d0 / (n_odd ** 2)
                lambda2 = kappa * ( n_odd * pi / L ) ** 2
                el2t = exp(-lambda2 * dble(t))
                delta_Temp = sign * coef * el2t * sin(n_odd * pi * x / L)
                Temp_t_x = Temp_t_x + delta_Temp
            enddo
            Temp_t_x = Temp_t_x * 4.0d0 * L / (pi ** 2)
            write(fo, '(2e12.4)') x, Temp_t_x
            Temp_t_x = 0.0d0
        enddo
        close(fo)
    enddo
end program rod_temp_dist

