program part_array
    integer ia1(4), ia2(4)
    integer i
    ! case 1 : do loop
    ia1(1:4) = (/ 1, 2, 3, 4 /)
    do i = 2, 4
        ia1(i) = ia1(i - 1)
        ! next ia1(i) is 1.
    enddo
    ! case 2 : partial array
    ia2(1:4) = (/ 1, 2, 3, 4/)
    ! ia2(1:3) are substituted by (/2, 3, 4/)
    ia2(1:3) = ia2(2:4)
    write(*, *) 'ia1:', (ia1(i), i = 1, 4)
    write(*, *) 'ia2:', (ia2(i), i = 1, 4)
end program part_array