phi_prev(2:n1-1, 2:n2-1) = 0.0d0
call set_dbc(phi, n1, n2)
call set_nbc(phi, x, n1, n2)
do istep = 1, nstep
    do j = 2, n2 - 1
        do i = 2, n1 - 1
            phi(i, j) = phi_prev(i, j) &
                        + d1 * (phi(i-1, j) - 2.0d0 * phi(i, j) + phi(i+1, j)) &
                        + d2 * (phi(i, j-1) - 2.0d0 * phi(i, j) + phi(i, j+1))
        end do
    end do
    call set_nbc(phi, x, n1, n2)
    er = chk_steady(phi_prev, phi, n1, n2)
    phi_prev(:, :) = phi(:, :)
    if (mod(istep, pstep) == 0) call output(phi, x, n1, n2)
    if (er < er0) exit
end do