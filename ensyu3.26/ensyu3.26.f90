module ensyu3_26
    implicit none
contains 
    function theory(x, i, j, n1, n2) result(phi)
        integer, intent(in) :: i, j, n1, n2
        double precision, intent(in) :: x(2, n1, n2)
        double precision phi
        double precision, parameter :: pi = acos(-1.0d0)
        phi = sin(pi * x(1, i, j)) * sinh(pi * (1.0d0 - x(2, i, j))) / sinh(pi)
    end function theory

    subroutine set_gridx(x, n1, n2)
        integer, intent(in) :: n1, n2 
        double precision, intent(out) :: x(:, :, :)
        integer i, j
        double precision dx1, dx2
        dx1 = 1.0d0 / dble(n1 - 1)
        dx2 = 1.0d0 / dble(n2 - 1)
        ! set x(m, i, j)
        do i = 1, n1
            do j = 1, n2
                x(1, i, j) = dx1 * dble(i - 1)
                x(2, i, j) = dx2 * dble(j - 1)
            end do 
        end do 
    end subroutine set_gridx
end module ensyu3_26

program phi_on_grid
    use ensyu3_26
    implicit none
    integer :: m, i, j, n1, n2, fno = 10
    double precision, allocatable :: x(:, :, :)
    write(*, *) 'input n1, n2:'
    read(*, *) n1, n2
    allocate(x(2, n1, n2))
    open(fno, file = 'grid.d')
    call set_gridx(x, n1, n2)
    ! write x1, x2 on grid.d
    do i = 1, n1
        do j = 1, n2
            write(fno, *) (x(m, i, j), m = 1, 2), theory(x, i, j, n1, n2)
        end do 
        write(fno, *)
    end do

    !do j = 1, n2
    !    do i = 1, n1
    !        write(fno, *) (x(m, i, j), m = 1, 2), theory(x, i, j, n1, n2)
    !    end do 
    !    write(fno, *)
    !end do
    deallocate(x)
    close(fno)
end program phi_on_grid