program matmul_vs_product
    implicit none
    integer i, j
    double precision a(2, 2), b(2, 2), prod_ab(2, 2), matmul_ab(2, 2)
    a(1, :) = (/ 1, 2 /)
    a(2, :) = (/ 3, 4 /)
    b(1, :) = (/ 5, 6 /)
    b(2, :) = (/ 7, 8 /)
    prod_ab = a * b 
    matmul_ab = matmul(a, b)
    write(*, *) "prod_ab:"
    do i = 1, 2
        write(*, *) (prod_ab(i, j), j = 1, 2)
    end do
    write(*, *) "matmul_ab:"
    do i = 1, 2
        write(*, *) (matmul_ab(i, j), j = 1, 2)
    end do
end program matmul_vs_product