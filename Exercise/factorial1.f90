program factorial1

  implicit none
  integer, parameter :: dp = selected_int_kind(15)
  integer(dp) :: i,n,factorial

  print *, 'Enter an integer < 15 '
  read *, n

  factorial = n
  do i = n-1,1,-1
     factorial = factorial * i
  end do
  write(*,'(i4,a,i15)') n,'!=',factorial

end program factorial1
     
