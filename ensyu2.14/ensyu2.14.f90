program mat3d
    implicit none
    integer i, j, k, is, n
    double precision t1, t2 
    double precision, allocatable :: a(:, :, :)
    write(*, *) 'input n:'
    read(*, *) n
    allocate(a(n, n, n), stat = is)
    if(is /= 0) stop 'stop. cannot allocate (n is too large)'
    call cpu_time(t1)
    do k = 1, n 
        do j = 1, n
            do i = 1, n 
                a(i, j, k) = 0
            end do
        end do
    end do 
    call cpu_time(t2)
    write(*, *) 'cpu time =', t2 - t1
    call cpu_time(t1)
    do i = 1, n 
        do j = 1, n 
            do k = 1, n
                a(i, j, k) = 0
            end do
        end do 
    end do
    call cpu_time(t2)
    write(*, *) 'cpu time =', t2 - t1
    call cpu_time(t1)
    a(1:n, 1:n, 1:n) = 0
    call cpu_time(t2)
    write(*, *) 'cpu time =', t2 - t1
    call cpu_time(t1)
    a(:, :, :) = 0
    call cpu_time(t2)
    write(*, *) 'cpu time =', t2 - t1
    deallocate(a)
end program mat3d
