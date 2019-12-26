program direction_cos
    double precision u(3), unit_x(3), unit_y(3), unit_z(3)
    double precision norm_u, cos_alpha, cos_beta, cos_gamma
    integer :: f_u = 10
    open(f_u, file = 'vec_u.d')
    read(f_u, *) u
    close(f_u)
    norm_u = sqrt(dot_product(u, u))
    if (norm_u == 0) stop 'stop. u must not be zero vector'
    unit_x = (/ 1, 0, 0 /)
    unit_y = (/ 0, 1, 0 /)
    unit_z = (/ 0, 0, 1 /)
    cos_alpha = dot_product(u, unit_x) / norm_u
    cos_beta = dot_product(u, unit_y) / norm_u
    cos_gamma = dot_product(u, unit_z) / norm_u
    write(*, *) 'cos_alpha =', cos_alpha
    write(*, *) 'cos_beta =', cos_beta
    write(*, *) 'cos_gamma =', cos_gamma
    write(*, *) 'sq_sum =', cos_alpha**2 + cos_beta**2 + cos_gamma**2
end program