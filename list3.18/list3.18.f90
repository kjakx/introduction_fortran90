module ensyu3_18
    implicit none
contains
    function vec_cos(a, b) result(vcos)
        double precision, intent(in) :: a(:), b(:)
        double precision vcos 
        double precision ab
        if (size(a) /= size(b)) stop 'stop. size(a) /= size(b)'
        ab = dot_product(a, a) * dot_product(b, b)
        if (ab == 0.0d0) then
            vcos = 0.0d0
        else 
            vcos = dot_product(a, b) / sqrt(ab)
        end if
    end function vec_cos 
end module ensyu3_18 

program main
    use ensyu3_18
    implicit none
    double precision, allocatable :: a(:), b(:)
    integer n 
    write(*, *) 'input n:'
    read(*, *) n 
    write(*, *) 'n :', n 
    allocate(a(n), b(n))
    call random_number(a)
    call random_number(b)
    write(*, *) 'a :', a
    write(*, *) 'b :', b 
    write(*, *) 'vec_cos :', vec_cos(a, b)
end program main