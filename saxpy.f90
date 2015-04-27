program test
  
  implicit none
  integer, parameter :: n = 100
  real :: alpha, x(n), y(n)

  alpha = 2.0
  x = 1.0
  y = 2.0

  call saxpy(n,alpha,x,y)
  
end program test

subroutine saxpy(n, alpha, x, y)
  implicit none
  integer ::  n
  real :: alpha, x(*), y(*)
!
! Saxpy: Compute y := alpha*x + y,
! where x and y are vectors of length n (at least).
!
  y(1:n) = alpha*x(1:n) + y(1:n)
 
end subroutine saxpy
      
