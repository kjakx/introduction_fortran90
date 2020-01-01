program tetra
    implicit none
    integer :: m, n, fno = 10
    double precision p(3, 4)
    call random_seed
    call random_number(p(1:3, 1:4))
    open(fno, file = 'tetra.d')
    do m = 1, 3
        do n = m + 1, 4
            write(fno, *) p(1:3, m)
            write(fno, *) p(1:3, n)
            write(fno, *)
        end do 
    end do 
    close(fno)
end program tetra