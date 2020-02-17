module ensyu3_17
    implicit none
contains 
    subroutine rotate_2dvec(x, theta)
        double precision, intent(in) :: x(2), theta
        double precision y(2), rot_op(2, 2)
        integer i 
        rot_op(1, :) = (/ cos(theta), -sin(theta) /)
        rot_op(2, :) = (/ sin(theta), cos(theta) /)
        y = matmul(rot_op, x)
        write(*, *) 'x :', (x(i), i = 1, 2)
        write(*, *) 'y :', (y(i), i = 1, 2)
    end subroutine rotate_2dvec 
end module ensyu3_17 

program main
    use ensyu3_17
    implicit none
    double precision x(2), theta
    read(*, *) x, theta
    write(*, *) 'x =', x 
    write(*, *) 'theta =', theta
    call rotate_2dvec(x, theta)
end program main