program gcd_lcm

  implicit none
  integer :: a, b
  integer :: gcd, lcm, u, v, t

  print *, "Program to calculate GCD and LCM of two integers"
  print *, "Enter two integers"
  read *, a, b

  u = a ; v = b

  do !while ( v/= 0 )
     t = v
     v = mod(u,v)
     u = t
     if ( v == 0 ) exit
  end do

  gcd = abs(u)
  lcm = a*b/gcd

  print *, "GCD of ", a, " and ", b, " is ", gcd
  print *, "LCM of ", a, " and ", b, " is ", lcm

end program gcd_lcm
