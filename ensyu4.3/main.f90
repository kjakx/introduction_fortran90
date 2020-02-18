program chk
    use sample
    implicit none
    character(11) :: c = 'I prefer Pi'
    write(*, *) c 
    write(*, *) revchar(c)
end program chk