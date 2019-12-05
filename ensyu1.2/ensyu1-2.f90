program loop_sum
  implicit none
  integer k, n, wa1, wa2, wa3, kou1, kou2, kou3
  n = 10
  wa1 = 0
  wa2 = 0
  wa3 = 0
  kou1 = n * (n + 1) / 2
  kou2 = n * (n + 1) * (2 * n + 1) / 6
  kou3 = n ** 2 * (n + 1) ** 2 / 4
  do k = 1, n
    wa1 = wa1 + k
    wa2 = wa2 + k ** 2
    wa3 = wa3 + k ** 3
  enddo
  write(*, *) "wa1 = ", wa1, "\n", &
              "wa2 = ", wa2, "\n", &
              "wa3 = ", wa3, "\n", &
              "kou1 = ", kou1, "\n", &
              "kou2 = ", kou2, "\n", &
              "kou3 = ", kou3, "\n"
end program loop_sum 
