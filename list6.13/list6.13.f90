module subprogs
    implicit none
contains
    subroutine bc_thru(pn, qn, p, q, v, w, n, dt, dx)
        integer, intent(in) :: n
        double precision pn(:), qn(:), p(:), q(:), v(:), w(:), dt, dx
        integer i
        do i = 1, n, n - 1
            pn(i) = p(i)
            qn(i) = q(i)
            cno1 = v(i) * dt / dx
            cno2 = w(i) * dt / dx
            if (i == 1) then
                if (cno1 < 0.0d0) pn(i) = p(i) - cno1 * (p(i+1) - p(i))
                if (cno2 < 0.0d0) qn(i) = q(i) - cno2 * (q(i+1) - q(i))
            else if (i == n) then
                if (cno1 > 0.0d0) pn(i) = p(i) - cno1 * (p(i) - p(i-1))
                if (cno2 > 0.0d0) qn(i) = q(i) - cno2 * (q(i) - q(i-1))
            end if
        end do
    end subroutine bc_thru

    subroutine cm1d(pn, p, i, v, dt, dx)
        integer, intent(in) :: i
        double precision, intent(in) :: p(:), v(:), dx, dt
        double precision, intent(out) :: pn(:)
        double precision cno
        cno = v(i) * dt / dx
        if (cno >= 0.0d0) then
            pn(i) = p(i) - cno * (p(i) - p(i-1))
        else
            pn(i) = p(i) - cno * (p(i+1) - p(i))
        end if
    end subroutine cm1d

    subroutine chk_cno(cno, dx, dt, v, w)
        double precision, intent(in) :: v(:), w(:), dx, dt, cno
        double precision cno1, cno2
        cno1 = maxval(abs(v(:)) * dt / dx)
        cno2 = maxval(abs(w(:)) * dt / dx)
        cno = max(cno1, cno2)
        if (cno >= 1) then
            write(*, *) 'stop. cno >= 1, cno =', cno
            stop
        end if
    end subroutine chk_cno

    subroutine print_uh(x, h, u, gr, n, fopen)
        integer, intent(in) :: n, fopen
        double precision, intent(in) :: x(:), h(:), u(:), gr
        if (fopen == 1) then
            open(20, file = 'uh.d')
        else if (fopen == -1) then
            close(20)
            return
        end if
        do i = 1, n
            write(20, '(10e16.8)') x(i), h(i), u(i) / sqrt(gr * h(i))
        end do
        write(20, *) ''
    end subroutine print_uh

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