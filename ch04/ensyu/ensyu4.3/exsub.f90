function revchar(c) result(rc)
    implicit none
    character(*), intent(in) :: c
    character(len(c)) rc
    integer i
    do i = 1, len(c)
        rc(i : i) = c(len(c) + 1 - i : len(c) + 1 - i)
    end do
end function revchar