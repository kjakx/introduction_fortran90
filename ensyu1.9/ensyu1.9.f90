program gcd_euclid
	implicit none
	integer m, n, m_org, n_org, gcd
	do
		write(*, *) ' input m, n (if m or n <= 0 , stop) : '
		read(*, *) m, n
		if (m <= 0 .or. n <= 0) then
			stop 'good bye ... '
		else if (m > 10000)	then
			write(*, *) ' m is too large! '
			cycle
		else if (n > 10000) then
			write(*, *) ' n is too large! '
			cycle
		else
			m_org = m
			n_org = n
			do
				if (m > n) then
					if (mod(m, n) == 0) then
						gcd = n
						exit
					else
						m = mod(m, n)
					endif
				else
					if(mod(n, m) == 0) then
						gcd = m
						exit
					else
						n = mod(n, m)
					endif
				endif
			enddo
		endif
		write(*, *) 'GCD of', m_org, 'and', n_org, 'is', gcd
	enddo
end program gcd_euclid
