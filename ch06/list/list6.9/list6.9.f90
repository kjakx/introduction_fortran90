module subprogs
    implicit none
contains

    subroutine set_init(n, dt, xl, dx, fr, gr, itrmax, pintv, h0, dh, &
                        x, u, v, w, h, p, q, pn, qn)
        integer, intent(out) :: n, itrmax, pintv
        double precision, intent(out) :: dt, xl, dx, fr, gr, h0, dh
        double precision, allocatable, intent(out) :: x(n), u(n), v(n), w(n), h(n), p(n), q(n), pn(n), qn(n)
        open(10, file = 'data.d')
        read(10, *) n, itrmax, pintv
        read(10, *) dt, xl, fr, gr, h0
        close(10)
        dx = xl / dble(n - 1)
        dh = 0.1d0 * h0
        allocate(x(n), u(n), v(n), w(n), h(n), p(n), q(n), pn(n), qn(n))
        x(:) = (/ (dx * dble(i - 1) , i = 1, n) /)
        u(:) = sqrt(gr + h0) * fr
        h(:) = h0 + dh * exp(- (x(:) - 0.5d0 * xl) ** 2 / 1.0d2)
        call uh2pqvw(u, h, gr, p, q, v, w, n)
    end subroutine set_init

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