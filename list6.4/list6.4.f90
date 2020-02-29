subroutine bicgstab1d(a, b, x, n, itrmax, er0)
    integer, intent(in) :: n, itrmax
    double precision, intent(in) :: a(n, n), b(n), er0
    double precision, intent(inout) :: x(n)
    integer itr
    double precision alpha, beta, c1, c2, c3, ev, vv, rr
    double precision r(n), r0(n), p(n), y(n), e(n), v(n)
    x(:) = 0.0d0
    r(:) = b - matmul(a, x)
    c1 = dot_product(r, r)
    if (c1 < er0) return
    p(:) = r(:)
    r0 = r
    do itr = 1, itrmax
        y(:) = matmul(a, p)
        c2 = dot_product(r0, y)
        alpha = c1 / c2
        e(:) = r(:) - alpha * y(:)
        v(:) = matmul(a, e)
        ev = dot_product(e, v)
        vv = dot_product(v, v)
        c3 = ev / vv
        x(:) = x(:) + alpha * p(:) + c3 * e(:)
        r(:) = e(:) - c3 * v(:)
        rr = dot_product(r, r)
        write(*, *) 'itr, er =', itr, rr
        if (rr < er0) exit
        c1 = dot_product(r0, r)
        beta = c1 / (c2 * c3)
        p(:) = r(:) + beta * (p(:) - c3 * y(:))
    end do
end subroutine bicgstab1d