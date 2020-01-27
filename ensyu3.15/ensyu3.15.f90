module ensyu3_15
    implicit none
contains 
    subroutine print_det_inv(a)
        integer i
        double precision, intent(in) :: a(2, 2)
        double precision det_a, inv_a(2, 2), iden(2, 2)
        det_a = a(1, 1) * a(2, 2) - a(1, 2) * a(2, 1)
        if (det_a == 0) stop 'stop. det_a = 0: inv_a does not exist'
        inv_a(1, :) = (/ a(2, 2), -a(1, 2) /)
        inv_a(2, :) = (/ -a(2, 1), a(1, 1) /)
        inv_a = inv_a / det_a
        write(*, *) 'a:'
        do i = 1, 2
            write(*, *) a(i, :)
        end do 
        write(*, *) 'det_a =', det_a 
        write(*, *) 'inv_a:'
        do i = 1, 2
            write(*, *) inv_a(i, :)
        end do 
        iden = matmul(inv_a, a)
        write(*, *) 'identity as a_inv * a:'
        do i = 1, 2
            write(*, *) iden(i, :)
        end do
    end subroutine print_det_inv 
end module ensyu3_15

program main
    use ensyu3_15
    implicit none
    double precision a(2, 2)
    call random_number(a)
    call print_det_inv(a)
end program main