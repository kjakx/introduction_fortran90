program e_taylor
    real(8) :: e_approx = 1, e = exp(1.0d0), er
    integer :: n, fact_n = 1
    do n = 1, 10
        fact_n = fact_n * n
        e_approx = e_approx + (1.0d0 / fact_n)
        er = abs(e_approx - e)
        write(*, *) 'e_approx(', n, ') =', e_approx
        write(*, *) 'er(', n, ') =', er
    enddo
end program e_taylor