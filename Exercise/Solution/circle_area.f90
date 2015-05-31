program circ_area

  implicit none
  integer, parameter :: dp = selected_real_kind(15)
  real(dp) :: radius, area, circum
  real(dp), parameter :: pi = atan(1.d0) * 4.d0

  print *, 'Enter the radius of the circle'
  read *, radius

  area = pi * radius ** 2
  circum = 2.d0 * pi * radius
  
  print *, "Area of Circle with radius", radius , " is ", area
  print *, "It's circumference is ", circum./a
  
end program circ_area
