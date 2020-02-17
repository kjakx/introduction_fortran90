module vec_subprogs
    implicit none
contains
    subroutine allocate_rvec(x, m)
        integer, intent(in) :: m 
        double precision, allocatable, intent(out) :: x(:)
        allocate(x(m))
        call random_number(x)
        x = x * 2.0d0 - 1.0d0
    end subroutine allocate_rvec
    subroutine print_vec(x, m)
        integer, intent(in) :: m 
        double precision, intent(in) :: x(1:m)
        write(*, *) x(1:m)
    end subroutine print_vec
end module vec_subprogs

program random_vec 
    use vec_subprogs
    implicit none
    double precision, allocatable :: v(:)
    integer n 
    write(*, *) 'input n:'
    read(*, *) n
    call allocate_rvec(v, n)
    call print_vec(v, n)
end program random_vec