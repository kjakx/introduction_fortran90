program geom_seq_out
    implicit none
    real(8) :: a, a1 = 16.0, r = 0.8, sum = 0, Sn
    integer :: n, fo = 11
    open(fo, file = 'output.d')
    Sn = a1 * (1 - r**10) / (1 - r)
    sum = sum + a1
    write(fo, '(2e12.4)') dble(1), a1
    do n = 1, 9
        a = a1 * r**(n)
        sum = sum + a
        write(fo, '(2e12.4)') dble(n + 1), a
    enddo
    ! write(*, *) 'sum =', sum, ', Sn =', Sn
    close(fo)
end program geom_seq_out