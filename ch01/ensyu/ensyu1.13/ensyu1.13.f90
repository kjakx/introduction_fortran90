program daikei_sekibun_std_dist
	implicit none
	real(8) :: pi = 3.14159265
	real(8) dx, x, y, s, a, b
	integer i, n
	write(*, '(a\)') 'input n : '
	read(*, *) n
	if (n < 1) stop 'stop, n < 1'
	write(*, '(a\)') 'input a, b : '
	read(*, *) a, b
	if (a >= b) stop 'stop, not a < b'
	dx = (b - a) / dble(n)
	s = 0.0d0
	do i = 0, n
		x = dx * dble(i)
		y = exp(-1 * (a + x) ** 2 / 2)
		if (i == 0 .or. i == n) then
			s = s + 0.5d0 * y
		else
			s = s + y
		endif
	enddo
	s = s * dx / sqrt(2*pi)
	write(*, *) 's = ', s
end program daikei_sekibun_std_dist
