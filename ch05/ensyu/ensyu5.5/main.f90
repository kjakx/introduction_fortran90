program main
    use ex_gname_interface
    implicit none
    integer ia(3, 3)
    double precision id(3, 3)
    ia(1, :) = (/ 1, 2, 3 /)
    ia(2, :) = (/ 4, 5, 6 /)
    ia(3, :) = (/ 7, 8, 9 /)
    call random_number(id)
    call print_mat(ia, 'ia :')
    call print_mat(ia, size(ia, 1), size(ia, 2), 'ia :')
    call print_mat(id, 'id :')
    call print_mat(id, size(id, 1), size(id, 2), 'id :')
end program main