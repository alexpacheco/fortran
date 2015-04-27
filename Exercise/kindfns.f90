program kind_function

  implicit none
  integer,parameter :: dp = selected_real_kind(15) 
  integer,parameter :: ip = selected_int_kind(15) 
  integer(kind=4) :: i
  integer(kind=8) :: j
  integer(ip) :: k
  real(kind=4) :: a
  real(kind=8) :: b
  real(dp) :: c

  print '(a,i2,a,i4)', 'Kind of i = ',kind(i), '  with range =', range(i)
  print '(a,i2,a,i4)', 'Kind of j = ',kind(j), '  with range =', range(j)
  print '(a,i2,a,i4)', 'Kind of k = ',kind(k), '  with range =', range(k)
  print '(a,i2,a,i2,a,i4)', 'Kind of real a = ',kind(a),&
       '  with precision = ', precision(a),&
       '  and range =', range(a)
  print '(a,i2,a,i2,a,i4)', 'Kind of real b = ',kind(b),&
       '  with precision = ', precision(b),&
       '  and range =', range(b)
  print '(a,i2,a,i2,a,i4)', 'Kind of real c = ',kind(c),&
       '  with precision = ', precision(c),&
       '  and range =', range(c)
 print *, huge(i),kind(i)
 print *, huge(j),kind(j)
 print *, huge(k),kind(k)

end program kind_function
