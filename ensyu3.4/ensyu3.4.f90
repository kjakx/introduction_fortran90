module subprogs
    implicit none
contains
    subroutine write_num_to_file
        integer :: ic = 1, fo = 11
        logical, save :: first_call = .true.
        if (first_call) then
            open(fo, file="output.d")
        end if
        if (ic == 10) then 
            close(fo)
            stop "stop. ic = 10"
        end if
        write(fo, *) ic
        ic = ic + 1
    end subroutine write_num_to_file
end module subprogs

program main
    use subprogs
    implicit none
    integer i 
    do i = 1, 10
        call write_num_to_file
    end do 
end program main