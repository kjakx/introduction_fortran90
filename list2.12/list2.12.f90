program sample
    implicit none
    integer, parameter :: n = 100
    double precision wa, min, max, b(n, n)
    integer i, j, imin(2), imax(2)
    call random_seed 
    call random_number(b(:, :))
    wa = 0.0d0
    min = b(1, 1)
    max = min
    do j = 1, n
        do i = 1, n 
            wa = wa + b(i, j)
            if (b(i, j) < min) then
                min = b(i, j)
                imin = (/i, j/)
            end if
            if (b(i, j) > max) then
                max = b(i, j)
                imax = (/i, j/)
            end if
        end do
    end do
    write(*, *) min, max, wa, imin(:), imax(:)
    write(*, *) minval(b), maxval(b), sum(b), minloc(b), maxloc(b)
end program sample