program main
    use subprogs
    implicit none
    double precision, allocatable :: x(:), u(:), v(:), w(:), h(:)
    double precision, allocatable :: p(:), q(:), pn(:), qn(:)
    double precision dt, xl, dx, fr, gr, h0, dh
    integer n, itr, itrmax, i, pintv
    call set_init(  n, dt, xl, dx, fr, gr, itrmax, pintv, h0, dh, &
                    x, u, v, w, h, p, q, pn, qn)
    call print_uh(x, h, u, gr, n, 1)
    do itr = 1, itrmax
        call chk_cno(n, dx, dt, v, w)
        do i = 2, n - 1
            call cm1d(pn, p, i, v, dt, dx)
            call cm1d(qn, q, i, w, dt, dx)
        end do
        call bc_thru(pn, qn, p, q, v, w, n, dt, dx)
        call pq2uhvw(pn, qn, gr, u, h, v, w, n)
        p(:) = pn(:)
        q(:) = qn(:)
        if (mod(itr, pintv) == 0) call print_uh(x, h, u, gr, n, 0)
    end do
    call print_uh(x, h, u, gr, n, -1)
    deallocate(x, u, v, w, h, p, q, pn, qn)
end program main