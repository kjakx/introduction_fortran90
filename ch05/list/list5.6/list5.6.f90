program read_data
    use file_mod
    implicit none
    integer :: fno = 10, nstep, n1, n2
    double precision dt, grav
    open(fno, file='sample.d')
    call read_file_data(fno, 'nstep = ', nstep)
    call read_file_data(fno, 'n1, n2 = ', n1, n2)
    call read_file_data(fno, 'dt = ', dt)
    call read_file_data(fno, 'grav = ', grav)
end program read_data