subroutine allocate_rmat
    use globals
    implicit none
    write(*, *) 'input n, m :'
    read(*, *) n, m
    allocate (a(n, m))
    call random_number(a)
end subroutine allocate_rmat

subroutine print_mat
    use globals
    implicit none
    integer i
    do i = 1, n
        write(*, '(100e12.4)') a(i, 1:m)
    end do
end subroutine print_mat  