module exsubp_gname_interface
    interface ex_read_file_data
        subroutine read_file_idata(fno, title, ivar)
            character(*), intent(in) :: title
            integer, intent(in) :: fno
            integer, intent(out) :: ivar
        end subroutine read_file_idata

        subroutine read_file_i2data(fno, title, ivar1, ivar2)
            character(*), intent(in) :: title
            integer, intent(in) :: fno
            integer, intent(out) :: ivar1, ivar2
        end subroutine read_file_i2data

        subroutine read_file_ddata(fno, title, dvar)
            character(*), intent(in) :: title
            integer, intent(in) :: fno
            double precision, intent(out) :: dvar
        end subroutine read_file_ddata
    end interface
end module exsubp_gname_interface