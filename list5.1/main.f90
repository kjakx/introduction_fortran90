program main
    use subprogs
    implicit none
    double precision u(3), v(3)
    call random_number(u)
    v = vnorm(vec = u)
    write(*, *) 'v :', v
end program main