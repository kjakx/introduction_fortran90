function AB_gj_pv(AB0, n, m) result(B)
    integer, intent(in) :: n, m
    double precision, intent(in) :: AB0(n, n+m)
    double precision AB(n, n+m)
    double precision B(n, m)
    integer i, k, i_pivot
    double precision ar, pivot, w(n)
    write(*, *) 'entering function inverse_gj_pv...'
    AB(:, :) = AB0(:, :)
    ! --- gauss-jordan ---
    do k = 1, n
        ! --- partial pivoting --
        i_pivot = k
        pivot = abs(AB(k, k))
        do i = k + 1, n
            if (abs(AB(i, k)) > pivot) then
                pivot = abs(AB(i, k))
                i_pivot = i
            end if
        end do
        if (pivot == 0.0d0) stop 'A is singular'
        if (k /= i_pivot) then
            w(k:n+m) = AB(k, k:n+m)
            AB(k, k:n+m) = AB(i_pivot, k:n+m)
            AB(i_pivot, k:n+m) = w(k:n+m)
        end if
        ! --- end partial pivoting
        ar = 1.0d0 / AB(k, k)
        AB(k, k) = 1.0d0
        AB(k, k+1:n+m) = ar * AB(k, k+1:n+m)
        do i = 1, n
            if (i /= k) then
                AB(i, k+1:n+m) = AB(i, k+1:n+m) - AB(i, k) * AB(k, k+1:n+m)
                AB(i, k) = 0.0d0
            end if
        end do
    end do
    ! --- end gauss-jordan --
    B = AB(:, n+1:n+m)
    write(*, *) 'AB :'
    do i = 1, n
        write(*, *) AB(i, :)
    end do
end function AB_gj_pv