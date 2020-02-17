module vec_subprogs
    implicit none
contains
    function normal_vec(v) result(nv)
        real(8), intent(in) :: v(:)
        real(8) nv(size(v, 1)), vl
        vl = sqrt(dot_product(v, v))
        if (vl == 0.0d0) then
            nv(:) = 0.0d0 
        else
            nv(:) = v(:) / vl
        end if 
    end function normal_vec 
end module vec_subprogs 

program main 
    use vec_subprogs
    implicit none
    real(8) :: x(3) = (/1., 2., 3./)
    write(*, *) 'x :', x
    write(*, *) 'norm_x :', normal_vec(x)
end program