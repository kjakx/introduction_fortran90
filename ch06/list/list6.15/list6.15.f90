c(1:2) = gr * h0 * dt ** 2 / dx(1:2) ** 2
c0 = 2.0d0 * (1.0d0 - c(1) - c(2))
do itr = 1, itrmax
    do j = 2, n2 - 1
        do i = 2, n1 - 1
            dta(i, j, 3) = c0 * eta(i, j, 2) - eta(i, j, 1) &
                         + c(1) * (eta(i-1, j, 2) + eta(i+1, j, 2)) &
                         + c(2) * (eta(i, j-1, 2) + eta(i, j+1, 2))
        end do
    end do
    eta(2:n1-1, 2:n2-1, 1) = eta(2:n1-1, n:n2-1, 2)
    eta(2:n1-1, 2:n2-1, 2) = eta(2:n1-1, n:n2-1, 3)
    ! boundary condition
    eta(1, :, :) = eta(2, :, :)
    eta(n1, :, :) = eta(n1-1, :, :)
    eta(:, 1, :) = eta(:, 2, :)
    eta(:, n2, :) = eta(:, n2-1, :)
end do