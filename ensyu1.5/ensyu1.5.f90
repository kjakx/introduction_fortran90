program loop_mton
	implicit none
	integer wa, m, n, i
	do
		write(*, *) ' input m, n (if m or n <= 0 , stop) : '
		read(*, *) m, n
		if (m <= 0 .or. n <= 0) then
			stop 'good bye ... '
		else if (m > n) then 
			write(*, *) ' sorry, n must be equal to or larger than m (m <= n)... '
		else
			wa = 0
			do i = m, n
				wa = wa + i
			enddo
			write(*, *) 'wa = ', wa
		endif
	enddo
end program loop_mton
