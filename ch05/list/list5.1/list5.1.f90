module subprogs
    implicit none
contains
    function vnorm(vec, norm) result(nvec)
        double precision, intent(in) :: vec(:)
        double precision, intent(in), optional :: norm
        double precision :: nvec(size(vec))
        double precision vecl, factor
        vecl = dot_product(vec, vec)
        if (present(norm)) then
            factor = norm
        else
            factor = 1.0d0
        end if
        if (vecl == 0.0d0) then
            nvec(:) = vecl * vec(:)
        else
            vecl = factor / sqrt(vecl)
            nvec(:) = vecl * vec(:)
        end if
    end function vnorm
end module subprogs