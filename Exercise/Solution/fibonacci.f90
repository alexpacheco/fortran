program fibonacci

  implicit none
  integer, parameter :: dp = selected_real_kind(15)
  integer :: i, n, fib0, fib1, fib

  print *, "Enter the Fibonacci number"
  read *, n

  fib0 = 0
  fib1 = 1

  print *, "n,  f(n)"
  ! 0 + 1 + 2 + ... + n


  do i = 2, n
     fib = fib1 + fib0
     print *, i, fib
     fib0 = fib1
     fib1 = fib
  end do

end program fibonacci



