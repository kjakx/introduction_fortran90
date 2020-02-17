module subprogs
    use globals
    implicit none
    double precision d
contains
    subroutine enzan
        a = 1.6d0
        b = 4.8d0
        c(:) = a + b
        d = a
    end subroutine enzan
end module subprogs