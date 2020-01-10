program soeji_chk
    implicit none
    integer :: a(3) = (/ 1, 2, 3 /), b(2), i 
    do i = 1, 3
        b(1:i-1) = a(1:i-1)
        b(i:2) = a(i+1:3)
        write(*, *) 'b =', b(1:2)
    end do
end program soeji_chk