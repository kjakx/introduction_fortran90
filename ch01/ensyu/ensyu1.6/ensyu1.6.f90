program loop_mton
	implicit none
	integer wa, m, n, i
	do
		write(*, *) ' input m, n (if m or n <= 0 , stop) : '
		read(*, *) m, n
		if (m <= 0 .or. n <= 0) then
			stop 'good bye ... '
		endif
		wa = 0	
		if (m > n) then 
			do i = n, m
				wa = wa + i
			enddo
		else
			do i = m, n
				wa = wa + i
			enddo
		endif
		write(*, *) 'wa = ', wa
	enddo
end program loop_mton
