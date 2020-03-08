call set_dbc(phi, x, n1, n2)
do itr = 1, itrmax
    do j = 2, n2 - 1
        do i = 2, n1 - 1
            rhs = - c * (phi(i-1, j) + phi(i+1, j)) - d * (phi(i, j-1) + phi(i, j+1))
            phi(i, j) = phi(i, j) + omega * (rhs - phi(i, j))
        end do
    end do
    call chk_err(phi, c, d, n1, n2, er)
    write(*, *) 'itr, er =', itr, er
    if(er < er0) exit
end do