program main
    implicit none 
    integer :: f1 = 11, f2 = 12, i, j, k 
    open(f1, file = "data1.d")
    open(f2, file = "data2.d")
    call read_file(f1, i)
    call read_file(f1, j)
    call read_file(f2, k)
    ! calculation
end program main

subroutine read_file(fno, n)
    implicit none
    integer fno, n 
    read(fno, *) n 
    write(*, *) n
    if (n < 0) then
        write(*, *) "error: negative value"
        stop 
    endif
end subroutine read_file