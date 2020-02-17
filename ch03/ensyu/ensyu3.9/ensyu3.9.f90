module subprogs
    implicit none
contains
    function daikei_sekibun_std_dist(a, b) result(s)
        double precision, parameter :: pi = 3.14159265
        double precision, intent(in) :: a, b
        double precision dx, x, y, s
        integer :: i, n = 100
        if (n < 1) stop 'stop, n < 1'
        if (a >= b) stop 'stop, a >= b'
        dx = (b - a) / dble(n)
        s = 0.0d0
        do i = 0, n
            x = dx * dble(i)
            y = exp(-1 * (a + x) ** 2 / 2)
            if (i == 0 .or. i == n) then
                s = s + 0.5d0 * y
            else
                s = s + y
            end if
        end do
        s = s * dx / sqrt(2*pi)
    end function daikei_sekibun_std_dist 
end module subprogs

program main
    use subprogs
    implicit none
    double precision a, b
    read(*, *) a, b
    write(*, *) 's = ', daikei_sekibun_std_dist(a, b)
end program main    
