module sample
    implicit none
contains
    function revchar(c) result(rc)
        character(*), intent(in) :: c
        character(len(c)) rc
        integer i
        do i = 1, len(c)
            rc(i : i) = c(len(c) + 1 - i : len(c) + 1 - i)
        end do
    end function revchar
end module sample

program chk
    use sample
    implicit none
    character(11) :: c = 'I prefer Pi'
    write(*, *) c 
    write(*, *) revchar(c)
end program chk