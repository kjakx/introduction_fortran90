program geom_seq
	implicit none
	real(8) :: a, a1 = 16.0, r = 0.8, sum = 0, Sn
	integer :: n
	Sn = a1 * (1 - r**10) / (1 - r)
	sum = sum + a1
	write(*, *) 'n =', 1, ', a', 1, '=', a1
	do n = 1, 9
		a = a1 * r**(n)
		sum = sum + a
		write(*, *) 'n =', n + 1, ', a', n + 1, '=', a
	enddo
	write(*, *) 'sum =', sum, ', Sn =', Sn
end program geom_seq
