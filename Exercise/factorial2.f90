program factorial2

  implicit none
  integer, parameter :: &
       dp = selected_int_kind(15)
  integer(dp) :: i,n,start,factorial

  print *, 'Enter an integer < 15 '
  read *, n

  if ( (n/2)*2 == n ) then
     start = 2 ! n is even
  else
     start = 1 ! n is odd
  endif
  factorial = 1_dp
  do i = start,n,2
     factorial = factorial * i
  end do
  write(*,'(i4,a,i15)') n,'!!=',factorial

end program factorial2
     
