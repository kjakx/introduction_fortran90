module interface_mod
    interface
        subroutine matnorm(smat, s_row, s_col)
            double precision, intent(in) :: smat(:, :)
            double precision, intent(out), optional :: s_row, s_col
        end subroutine matnorm
    end interface
end module interface_mod

subroutine matnorm(smat, s_row, s_col)
    implicit none
    double precision, intent(in) :: smat(:, :)
    double precision, intent(out), optional :: s_row, s_col
    double precision :: as_row(size(smat, 1)), as_col(size(smat, 2))
    integer i
    if (present(s_row)) then
        do i = 1, size(smat, 1)
            as_row(i) = sum(abs(smat(i, :)))
        end do
        s_row = maxval(as_row)
    end if
    if (present(s_col)) then
        do i = 1, size(smat, 1)
            as_col(i) = sum(abs(smat(:, i)))
        end do
        s_col = maxval(as_col)
    end if
end subroutine matnorm

program main
    use interface_mod
    implicit none
    double precision w_row, w_col, a(100, 100)
    call random_seed
    call random_number(a)
    call matnorm(a, w_row, w_col)
    write(*, *) 'w_row :', w_row
    write(*, *) 'w_col :', w_col
end program main