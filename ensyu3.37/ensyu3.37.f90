module ensyu3_37
    implicit none
contains
    function normal_vec(v, n) result(nv)
        integer, intent(in) :: n
        real(8), intent(in) :: v(n)
        real(8) nv(n), vl
        vl = sqrt(dot_product(v, v))
        if (vl == 0.0d0) then
            nv(:) = 0.0d0 
        else
            nv(:) = v(:) / vl
        end if 
    end function normal_vec 

    function gs(a, n) result(e)
        integer, intent(in) :: n 
        double precision, intent(in) :: a(n, n)
        double precision e(n, n), dotp
        integer k, j
        e(1:n, 1) = normal_vec(a(1:n, 1:1), n)
        do k = 2, n
            e(1:n, k) = a(1:n, k)
            do j = 1, k - 1
                dotp = dot_product(a(1:n, k), e(1:n, j))
                e(1:n, k) = e(1:n, k) - dotp * e(1:n, j)
            end do
            e(1:n, k) = normal_vec(e(1:n, k:k), n)
        end do
    end function gs

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
end module ensyu3_37

program main
    use ensyu3_37
    implicit none
    double precision a(3, 3), T(3, 3)
    call random_number(a)
    T = gs(a, 3)
    call print_rmatc(a, "a :")
    call print_rmatc(T, "T :")
    call print_rmatc(matmul(T, transpose(T)), "T*T^-1 :")
end program main
