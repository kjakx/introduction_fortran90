module subprogs
    implicit none
contains 
    function geom_seq(a, r, n) result(Sn)
        double precision, intent(in) :: a, r
        double precision Sn
        integer, intent(in) :: n
        Sn = a * (1 - r**n) / (1 - r)
    end function geom_seq 
end module subprogs

program main
    use subprogs
    implicit none
    double precision :: a1 = 16.0, r = 0.8
    integer :: n = 10
    write(*, *) 'sum =', geom_seq(a1, r, n)
end program main
