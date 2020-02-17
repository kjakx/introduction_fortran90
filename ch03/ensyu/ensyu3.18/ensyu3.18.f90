module ensyu3_18
    implicit none
contains
    function norm(u) result(vl)
        double precision, intent(in) :: u(:)
        double precision vl
        vl = norm2(u)
    end function norm 
end module ensyu3_18

program main
    use ensyu3_18
    implicit none
    double precision, allocatable :: u(:)
    integer n 
    write(*, *) 'input n:'
    read(*, *) n
    write(*, *) 'n =', n 
    allocate(u(n))
    call random_number(u)
    write(*, *) 'u :', u
    write(*, *) 'norm(u) =', norm(u)
end program main