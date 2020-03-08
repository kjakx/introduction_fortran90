function inverse_gj_pv(AI0, n) result(inverse_A)
    integer, intent(in) :: n
    double precision, intent(in) :: AI0(n, 2*n)
    double precision AI(n, 2*n)
    double precision inverse_A(n, n)
    integer i, k, i_pivot
    double precision ar, pivot, w(n)
    write(*, *) 'entering function inverse_gj_pv...'
    AI(:, :) = AI0(:, :)
    ! --- gauss-jordan ---
    do k = 1, n
        ! --- partial pivoting --
        i_pivot = k
        pivot = abs(AI(k, k))
        do i = k + 1, n
            if (abs(AI(i, k)) > pivot) then
                pivot = abs(AI(i, k))
                i_pivot = i
            end if
        end do
        if (pivot == 0.0d0) stop 'A is singular'
        if (k /= i_pivot) then
            w(k:2*n) = AI(k, k:2*n)
            AI(k, k:2*n) = AI(i_pivot, k:2*n)
            AI(i_pivot, k:2*n) = w(k:2*n)
        end if
        ! --- end partial pivoting
        ar = 1.0d0 / AI(k, k)
        AI(k, k) = 1.0d0
        AI(k, k+1:2*n) = ar * AI(k, k+1:2*n)
        do i = 1, n
            if (i /= k) then
                AI(i, k+1:2*n) = AI(i, k+1:2*n) - AI(i, k) * AI(k, k+1:2*n)
                AI(i, k) = 0.0d0
            end if
        end do
    end do
    ! --- end gauss-jordan --
    inverse_a = AI(:, n+1:2*n)
    write(*, *) 'AI :'
    do i = 1, n
        write(*, *) AI(i, :)
    end do
end function inverse_gj_pv