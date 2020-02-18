program main_nvec
    use interface_mod
    implicit none
    double precision v(10), w(10)
    call random_seed
    call random_number(v)
    w(:) = normal_vec(v)
    write(*, *) w, dot_product(w, w)
end program main_nvec