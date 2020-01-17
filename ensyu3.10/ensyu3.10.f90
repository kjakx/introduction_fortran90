module subprogs
    implicit none
contains
    function comp_elip_1st(k, n) result(K_n)
        double precision, intent(in) :: k
        integer, intent(in) :: n
        double precision :: K_n, K_prev, K_delta = 1.0d0, e, pi = acos(-1.0d0)
        integer :: i
        if (n < 2) stop 'stop. n must be n >= 2'
        if (abs(k) >= 1) stop 'stop. k must be |k| < 1'
        K_prev = 1
        do i = 1, n - 1
            K_delta = K_delta * (dble(2 * i - 1) ** 2) / (dble(2 * i) ** 2) * k * k
            K_n = K_prev + K_delta
            e = pi / 2.0d0 * (K_n - K_prev)
            K_prev = K_n
        end do
        K_n = K_n * pi / 2.0d0
    end function comp_elip_1st
end module subprogs

program main
    use subprogs
    implicit none
    double precision k
    integer n
    read(*, *) k, n
    write(*, *) 'k =', k
    write(*, *) 'n =', n
    write(*, *) 'K_n =', comp_elip_1st(k, n)
end program main