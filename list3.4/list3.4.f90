module subprogs
    implicit none
contains
    subroutine cone_vol(r, h, v)
        double precision, intent(in) :: r, h 
        double precision, intent(out) :: v 
        double precision s, pi 
        pi = 2.0d0 * acos(0.0d0)
        s = pi * r ** 2
        v = s * h / 3.0d0
    end subroutine cone_vol 
end module subprogs

program main
    use subprogs 
    implicit none
    double precision :: a = 1.5d0, l = 3.0d0, vol 
    call cone_vol(a, l, vol)
    write(*, *) "vol =", vol 
end program main