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

program chk_module
    use sample_mod, only : ib, sub
    implicit none
    ! ia is not accesible
    write(*, *) ia
    write(*, *) ib
    ! ic is not accesible
    write(*, *) ic
    call sub
end program