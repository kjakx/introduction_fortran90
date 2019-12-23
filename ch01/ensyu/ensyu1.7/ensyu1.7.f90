program combination
	implicit none
	Integer i, n, r, fact_n, fact_r, fact_n_r, nPr, nCr
	do
		write(*, *) ' input n, r (n must be n <= 0. If n or r < 0 then stop.) '
		read(*, *) n, r
		if (n > 10) then
			write(*, *) ' n must be equal or smaller than 10. '
		else if (n < r) then 
			write(*, *) ' n must be equal or greater than r. '
		else if (n < 0 .or. r < 0) then
			stop ' good bye ... '
		else
			fact_n = 1
			fact_r = 1
			fact_n_r = 1
			do i = 1, n
				fact_n = fact_n * i
			enddo
			do i = 1, r
				fact_r = fact_r * i
			enddo
			do i = 1, n - r
				fact_n_r = fact_n_r * i
			enddo
			nPr = fact_n / fact_n_r
			nCr = nPr / fact_r
			write(*, *) n, 'P', r, '= ', nPr
			write(*, *) n, 'C', r, '= ', nCr
		endif
	enddo
end program combination


