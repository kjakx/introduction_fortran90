module sample_mod0
    implicit none
    integer, save :: ia = 1, ib = 2, ic = 3
contains
    subroutine sub
        integer, save :: id = 4
        write(*, *) id
    end subroutine sub
end module sample_mod0

program main
    use sample_mod0
    implicit none
    write(*, *) ia, ib, ic
    ia = 0
    ib = 0
    ic = 0
    write(*, *) ia, ib, ic
    ! id is not accesible.
    ! id = 0
    call sub
end program