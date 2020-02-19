function normal_vec(v) result(nv)
    implicit none
    double precision, intent(in) :: v(:)
    double precision nv(size(v, 1)), vl
    vl = sqrt(dot_product(v, v))
    if (vl == 0.0d0) then
        nv(:) = 0.0d0
    else
        nv(:) = v(:) / vl
    end if
end function normal_vec