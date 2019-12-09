program e_taylor
    real(8) :: e_approx = 1, e = exp(1.0d0)
    integer :: n, fact_n = 1
    do n = 1, 10
        fact_n = fact_n * n
        e_approx = e_approx / fact_n
        er = abs(e_approx - er)
        write(*, *) 'e_approx(', n, ') =', e_approx
        write(*, *) 'er(', n, ') =', er 
    enddo
end program e_taylor