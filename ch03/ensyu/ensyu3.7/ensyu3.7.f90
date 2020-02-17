module subprogs
    implicit none
contains
    subroutine rect_surf_vol(x, y, h, s, v)
        double precision, intent(in) :: x, y, h 
        double precision, intent(out) :: s, v
        s = 2.0d0 * (x * y + y * h + h * x)
        v = x * y * h
    end subroutine rect_surf_vol 
end module subprogs

program main
    use subprogs 
    implicit none
    double precision :: x = 10.0d0, y = 10.0d0, h = 15.0d0, surf, vol 
    call rect_surf_vol(x, y, h, surf, vol)
    write(*, *) "surf =", surf
    write(*, *) "vol =", vol 
end program main