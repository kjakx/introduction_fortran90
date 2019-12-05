program fibonatch
  implicit none
  integer i, n, an, an_1, an_2
  n = 10
  an_1 = 1
  an_2 = 2 
  write(*, *) "a1 = ", an_1
  write(*, *) "a2 = ", an_2
  do i = 3, n
    an = an_1 + an_2
    write(*, *) "a", i, " = ", an
    an_1 = an_2
    an_2 = an
  enddo
end program fibonatch 
