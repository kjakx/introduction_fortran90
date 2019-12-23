program factoring
	implicit none
	Integer i, n
	do
		write(*, *) ' Input an integer n > 1: ( n <= 1 for interrupt ) '
		read(*, *) n
		if (n == 1) then
			stop ' n must be larger than 1. '
		else if (n > 10000) then
			write(*, *) ' n is too large! '
			cycle
		else
			i = 2
			write(*, *) n, '='
			do
				if (n == i) then
					write(*, *) i
					exit
				else if (mod(n, i) == 0) then
					write(*, *) i, '*'
					n = n / i
					i = 2
				else
					i = i + 1
				endif
			enddo
		endif
	enddo
end program factoring


