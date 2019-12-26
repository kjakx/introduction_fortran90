program tri_surface
    implicit none
    double precision a(3), b(3), axb(3), c(3)
    double precision area1, area2, a_n, b_n, c_n, s
    integer :: i, fi = 10
    open(fi, file = 'mat.d')
    read(fi, *) a
    read(fi, *) b
    close(fi)
    c = b - a
    a_n = sqrt(norm2(a))
    b_n = sqrt(norm2(b))
    c_n = sqrt(norm2(c))
    s = (a_n + b_n + c_n) * 0.5
    do i = 1, 3
        axb(i) = a(mod(i, 3) + 1) * b(mod(i + 1, 3) + 1) - a(mod(i + 1, 3) + 1) * b(mod(i, 3) + 1)
    end do
    area1 = sqrt(norm2(axb)) * 0.5
    area2 = sqrt(s * (s - a_n) * (s - b_n) * (s - c_n))
    write(*, *) 'a_n =', a_n
    write(*, *) 'b_n =', b_n
    write(*, *) 'c_n =', c_n
    write(*, *) 's =', s
    write(*, *) 'area1(cross_product) =', area1
    write(*, *) ' area2(heron) =', area2 
end program