module subprogs
    implicit none 
contains 
    subroutine print_statistical_param(dv)
        integer n, i
        double precision :: sum_vec, mean, disp = 0, std_dev
        double precision, intent(in) :: dv(:)
        n = size(dv, 1)
        sum_vec = sum(dv)
        mean = sum_vec / dble(n)
        do i = 1, n 
            disp = disp + (dv(i) - mean) ** 2
        end do 
        disp = disp / dble(n)
        std_dev = sqrt(disp)
        write(*, *) 'x :', dv
        write(*, *) 'sum :', sum_vec 
        write(*, *) 'mean :', mean 
        write(*, *) 'disp :', disp
        write(*, *) 'std_dev:', std_dev
    end subroutine print_statistical_param
end module subprogs 

program main 
    use subprogs 
    implicit none
    integer n
    double precision, allocatable :: x(:)
    write(*, *) 'input n:'
    read(*, *) n
    write(*, *) 'n =', n
    allocate(x(n))
    call random_number(x)
    call print_statistical_param(x)
end program main