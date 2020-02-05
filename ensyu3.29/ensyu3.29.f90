module ensyu3_29
    implicit none 
contains
    function identity(A) result(I)
        double precision, intent(in) :: A(:, :)
        double precision, allocatable :: I(:, :)
        integer j, n, m
        n = size(A, 1)
        m = size(A, 2)
        allocate(I(n, m))
        I = 0.
        do j = 1, min(n, m)
            I(j, j) = 1.
        end do 
    end function identity 

    function Pci(c, i, n) result(P)
        double precision, allocatable :: P(:, :)
        integer, intent(in) :: i, n
        double precision, intent(in) :: c
        if (c == 0) stop 'stop. c must not be zero'
        if (i < 1) stop 'stop. i must be positive integer' 
        if (i > n) stop 'stop. i > n'
        allocate(P(n, n))
        P = identity(P)
        P(i, i) = c
    end function Pci 

    function Qcij(c, i, j, n) result(Q)
        double precision, allocatable :: Q(:, :)
        double precision, intent(in) :: c
        integer, intent(in) :: i, j, n
        if (i == j) stop 'stop. i must not be equal to j'
        if (i < 1) stop 'stop. i must be positive integer'
        if (j < 1) stop 'stop. j must be positive integer'  
        if (i > n) stop 'stop. i > n'
        if (j > n) stop 'stop. j > n'
        allocate(Q(n, n))
        Q = identity(Q)
        Q(j, i) = c 
    end function Qcij 

    function Rij(i, j, n) result(R)
        double precision, allocatable :: R(:, :)
        double precision, allocatable :: tmp(:)
        integer, intent(in) :: i, j, n
        if (i == j) stop 'stop. i must not be equal to j'
        if (i < 1) stop 'stop. i must be positive integer'
        if (j < 1) stop 'stop. j must be positive integer'  
        if (i > n) stop 'stop. i > n'
        if (j > n) stop 'stop. j > n'
        allocate(R(n, n), tmp(n))
        R = identity(R)
        tmp(:) = R(i, :)
        R(i, :) = R(j, :)
        R(j, :) = tmp(:)
    end function Rij

    subroutine print_rmatc(a, name) 
        character(*), intent(in) :: name
        double precision, intent(in) :: a(:, :)
        integer i, n, m
        n = size(a, 1)
        m = size(a, 2)
        write(*, *) name
        do i = 1, n 
            write(*, '(100e12.4)') a(i, 1:m)
        end do 
    end subroutine print_rmatc
end module ensyu3_29

program main
    use ensyu3_29
    implicit none 
    double precision A(2, 3), P(2, 3), Q(2, 3), R(2, 3)
    double precision PA(2, 3), QA(2, 3), RA(2, 3), AP(2, 3), AQ(2, 3), AR(2, 3) 
    A(1, :) = (/ 1.d0, 2.d0, 3.d0 /)
    A(2, :) = (/ 4.d0, 5.d0, 6.d0 /)
    PA = matmul(Pci(2.d0, 2, size(A, 1)), A)
    QA = matmul(Qcij(2.d0, 1, 2, size(A, 1)), A)
    RA = matmul(Rij(1, 2, size(A, 1)), A)
    AP = matmul(A, Pci(2.d0, 2, size(A, 2)))
    AQ = matmul(A, Qcij(2.d0, 1, 2, size(A, 2)))
    AR = matmul(A, Rij(1, 2, size(A, 2)))
    call print_rmatc(A, 'A :')
    call print_rmatc(PA, 'PA :')
    call print_rmatc(QA, 'QA :')
    call print_rmatc(RA, 'RA :')
    call print_rmatc(AP, 'AP :')
    call print_rmatc(AQ, 'AQ :')
    call print_rmatc(AR, 'AR :')
end program main