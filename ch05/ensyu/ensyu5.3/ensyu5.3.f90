module ensyu3_24
    implicit none
contains 
    recursive function det_mat(a, n) result(det)
        integer, intent(in) :: n
        double precision, intent(in) :: a(n, n)
        double precision det, b(n-1, n-1)
        integer i
        if (n > 1) then
            det = 0.0d0
            do i = 1, n
                b(1 : i-1, 1 : n-1) = a(1 : i-1, 2 : n)
                b(i : n-1, 1 : n-1) = a(i+1 : n, 2 : n)
                det = det + (-1.0d0) ** (i + 1) * a(i, 1) * det_mat(b, n-1)
            end do
        else
            det = a(1, 1)
        end if
    end function det_mat
end module ensyu3_24

program main
    use ensyu3_24
    integer i
    double precision a(3, 3), a_t(3, 3), a_tri(3, 3)
    double precision :: tr_a_tri = 1
    double precision det_a, det_a_t, det_a_tri
    call random_number(a)
    a_t = transpose(a)
    a_tri = 0.d0
    do i = 1, 3
        a_tri(i, i:3) = a(i, i:3)
        tr_a_tri = tr_a_tri * a_tri(i, i) 
    end do
    det_a = det_mat(a, 3)
    det_a_t = det_mat(a_t, 3)
    det_a_tri = det_mat(a_tri, 3)
    write(*, *) 'a :'
    do i = 1, 3
        write(*, *) a(i, :)
    end do 
    write(*, *) 'a_t :'
    do i = 1, 3
        write(*, *) a_t(i, :)
    end do 
    write(*, *) 'a_tri :'
    do i = 1, 3
        write(*, *) a_tri(i, :)
    end do 
    write(*, *) 'det(a) :', det_a 
    write(*, *) 'det(a_t) :', det_a_t
    write(*, *) 'det(a_tri) :', det_a_tri 
    write(*, *) 'tr(a_tri) :', tr_a_tri
end program main