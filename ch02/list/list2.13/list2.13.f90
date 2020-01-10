program simple_sort
    implicit none
    double precision t1, t2
    integer, parameter :: n = 10000
    double precision a(n), min 
    integer i, imin 
    call random_seed 
    call random_number(a(:))
    call cpu_time(t1)
    do i = 1, n - 1
        min = minval(a(i+1:n))
        imin = minloc(a(i+1:n), 1) + i
        if (a(i) > a(imin)) then
            a(imin) = a(i)
            a(i) = min
        end if 
    end do 
    call cpu_time(t2)
    !write(*, *) a(:)
    write(*, *) 'time =', t2 - t1
end program simple_sort