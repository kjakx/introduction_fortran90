program getMod
	implicit none
	real(8) a, p
	integer div, modap
	write(*, '(a\)') 'input a, p(p <= 0 to quit)  : '
	read(*, *) a, p
	if (p <= 0) stop 'stop for p == 0'
	div = int(a / p)
	modap = a - p * div
	write(*, *) int(a), 'mod', int(p), '=', modap
end program getMod
