module vec_subprogs
    implicit none
contains
    subroutine print1(v)
        integer, intent(in) :: v(:)
        write(*, *) v(2)
    end subroutine print1

    subroutine print2(v)
        integer, intent(in) :: v(0:)
        write(*, *) v(2)
    end subroutine print2
end module vec_subprogs

program chk
    use vec_subprogs
    implicit none
    integer :: x(0:2) = (/ 1, 2, 3 /)
    write(*, *) x(2)
    call print1(x)
    call print2(x)
end program chk