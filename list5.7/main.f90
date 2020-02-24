program read_data
    use exsubp_gname_interface
    implicit none
    integer :: fno = 10, nstep, n1, n2
    double precision dt, grav
    open(fno, file='sample.d')
    call ex_read_file_data(fno, 'nstep = ', nstep)
    call ex_read_file_data(fno, 'n1, n2 = ', n1, n2)
    call ex_read_file_data(fno, 'dt = ', dt)
    call ex_read_file_data(fno, 'grav = ', grav)
end program read_data