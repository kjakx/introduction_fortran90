function chk_dd_mat(a) result(is_dd)
    implicit none
    double precision, intent(in) :: a(:, :)
    integer :: is_dd
    integer i, n, m
    double precision sum_row
    is_dd = 1
    n = size(a, 1)
    m = size(a, 2)
    if (n /= m) is_dd = 0
    do i = 1, n
        sum_row = sum(a(i, :))
        if (sum_row >= 2.0) then 
            is_dd = 0
            exit
        end if
    end do
end function chk_dd_mat