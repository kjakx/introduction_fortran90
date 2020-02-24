subroutine print_rmatc(a, name) 
    character(*), intent(in) :: name
    double precision, intent(in) :: a(:, :)
    integer i, n, m
    n = size(a, 1)
    m = size(a, 2)
    write(*, *) name
    do i = 1, n 
        write(*, '(100e12.4)') a(i, 1:m)
    end do 
end subroutine print_rmatc

subroutine print_rmatc_nm(a, n, m, name) 
    character(*), intent(in) :: name
    integer, intent(in) :: n, m
    double precision, intent(in) :: a(n, m)
    integer i
    write(*, *) name
    do i = 1, n 
        write(*, '(100e12.4)') a(i, 1:m)
    end do 
end subroutine print_rmatc_nm

subroutine print_imatc(ia, name) 
    character(*), intent(in) :: name
    integer, intent(in) :: ia(:, :)
    integer i, n, m
    n = size(ia, 1)
    m = size(ia, 2)
    write(*, *) name
    do i = 1, n 
        write(*, *) ia(i, 1:m)
    end do 
end subroutine print_imatc

subroutine print_imatc_nm(ia, n, m, name) 
    character(*), intent(in) :: name
    integer, intent(in) :: n, m
    integer, intent(in) :: ia(n, m)
    integer i
    write(*, *) name
    do i = 1, n 
        write(*, *) ia(i, 1:m)
    end do 
end subroutine print_imatc_nm