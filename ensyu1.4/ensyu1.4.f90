program loop_step
	implicit none
	integer i
	write(*, *) '1 to 5 step 2'
	do i = 1, 5, 2
		write(*, *) i
	enddo
	write(*, *) '5 to 1 step -2'
	do i = 5, 1, -2
		write(*, *) i
	enddo
end program loop_step

