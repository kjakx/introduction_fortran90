subroutine read_file_idata(fno, title, ivar)
    implicit none
    character(*), intent(in) :: title
    integer, intent(in) :: fno
    integer, intent(out) :: ivar
    read(fno, *) ivar
    write(*, *) title, ivar
end subroutine read_file_idata

subroutine read_file_i2data(fno, title, ivar1, ivar2)
    implicit none
    character(*), intent(in) :: title
    integer, intent(in) :: fno
    integer, intent(out) :: ivar1, ivar2
    read(fno, *) ivar1, ivar2
    write(*, *) title, ivar1, ivar2
end subroutine read_file_i2data

subroutine read_file_ddata(fno, title, dvar)
    implicit none
    character(*), intent(in) :: title
    integer, intent(in) :: fno
    double precision, intent(out) :: dvar
    read(fno, *) dvar
    write(*, *) title, dvar
end subroutine read_file_ddata