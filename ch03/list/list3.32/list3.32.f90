module sample_mod
    implicit none
    private
    integer, save :: ia = 1, ib = 2, ic = 3
    public ib, ic, sub
contains
    subroutine sub
        integer, save :: id = 4
        write(*, *) id
    end subroutine sub
end module sample_mod

program main
    use sample_mod
    implicit none
    write(*, *) ib, ic
    ! ia is not accesible.
    ! ia = 0
    ib = 0
    ic = 0
    write(*, *) ib, ic
    ! id is not accesible.
    ! id = 0
    call sub
end program