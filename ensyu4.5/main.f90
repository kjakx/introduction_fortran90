program main
    use interface_mod
    implicit none
    integer n, is_dd_mat
    double precision, allocatable :: a(:, :)
    write(*, *) 'input n :'
    read(*, *) n
    allocate(a(n, n))
    call set_dd_mat(a)
    is_dd_mat = chk_dd_mat(a)
    if (is_dd_mat == 1) then 
        write(*, *) 'a is dd matrix'
    else 
        write(*, *) 'a is not dd matrix'
    end if
end program main