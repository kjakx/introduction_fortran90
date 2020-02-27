module mat_subprogs
    implicit none
contains
    recursive function det_mat(a, n) result(det)
        integer, intent(in) :: n
        double precision, intent(in) :: a(n, n)
        double precision det, b(n-1, n-1)
        integer j
        if (n > 1) then
            det = 0.0d0
            do j = 1, n
                b(1 : n-1, 1 : j-1) = a(2 : n, 1 : j-1)
                b(1 : n-1, j : n-1) = a(2 : n, j+1 : n)
                det = det + (-1.0d0) ** (1 + j) * a(1, j) * det_mat(b, n-1)
            end do
        else
            det = a(1, 1)
        end if
    end function det_mat
end module mat_subprogs

program cal_det
    use mat_subprogs
    implicit none
    integer, parameter :: n = 5
    double precision a(n, n)
    a(1, :) = (/ 1, 2, 3, 4, 5 /)
    a(2, :) = (/ 1, 2, 3, 4, 5 /)
    a(3, :) = (/ 1, 2, 3, 4, 5 /)
    a(4, :) = (/ 1, 2, 3, 4, 5 /)
    a(5, :) = (/ 1, 2, 3, 4, 5 /)
    write(*, *) 'det =', det_mat(a, n)
end program cal_det