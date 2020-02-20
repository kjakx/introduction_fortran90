program main
    use subprogs
    implicit none
    double precision u(3), v(3), normal_outer(3)
    u = (/ 3.0d0, 0.0d0, 0.0d0 /)
    v = (/ 0.0d0, 0.0d0, -6.0d0 /)
    normal_outer = outer_norm(u, v)
    write(*, *) 'normal_uxv =', normal_outer
end program main