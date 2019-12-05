program prime
	implicit none
	Integer i, n, is_prime
	do
		is_prime = 1
		write(*, *) ' Input an integer n > 1: ( n <= 1 for interrupt ) '
		read(*, *) n
		if (n == 1) then
			stop ' n must be larger than 1. '
		else if (n > 1000000) then
			write(*, *) ' n is too large! '
			cycle
		else
			do i = 2, floor(sqrt(real(n)))
				if (mod(n, i) == 0) then
					is_prime = 0
					exit
				endif
			enddo
			if (is_prime == 0) then
				write(*, *) n, 'is not a prime. It can be divided by', i
			else
				write(*, *) n, 'is a prime.'
			endif
		endif
	enddo
end program prime


