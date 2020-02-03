module sample 
    implicit none 
contains 
    subroutine error_stop(emes)
        character(*), intent(in) :: emes 
        write(*, *) emes
        stop
    end subroutine error_stop
end module sample

program moji
    use sample
    implicit none
    integer :: positive = -1
    character(23) :: emes = 'positive is negative !!'
    if (positive < 0) call error_stop(emes)
end program moji