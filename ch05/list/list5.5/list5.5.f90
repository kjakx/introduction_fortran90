module file_mod
    implicit none
    interface read_file_data
        module procedure &
            read_file_idata, &
            read_file_i2data, &
            read_file_ddata
    end interface
contains
    subroutine read_file_idata(fno, title, ivar)
        character(*), intent(in) :: title
        integer, intent(in) :: fno
        integer, intent(out) :: ivar
        read(fno, *) ivar
        write(*, *) title, ivar
    end subroutine read_file_idata

    subroutine read_file_i2data(fno, title, ivar1, ivar2)
        character(*), intent(in) :: title
        integer, intent(in) :: fno
        integer, intent(out) :: ivar1, ivar2
        read(fno, *) ivar1, ivar2
        write(*, *) title, ivar1, ivar2
    end subroutine read_file_i2data

    subroutine read_file_ddata(fno, title, dvar)
        character(*), intent(in) :: title
        integer, intent(in) :: fno
        double precision, intent(out) :: dvar
        read(fno, *) dvar
        write(*, *) title, dvar
    end subroutine read_file_ddata
end module file_mod