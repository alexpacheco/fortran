program temp

  implicit none
  real :: tempC, tempF

  ! Convert 10C to fahrenheit

  tempF = 9 / 5 * 10 + 32

  ! Convert 40F to celsius

  tempC = 5 / 9 * (40 - 32 ) 

  print *, '10C = ', tempF, 'F'
  print *, '40F = ', tempC, 'C'

end program temp
  
