program nested_doloop

  implicit none
  integer,parameter :: dp = selected_real_kind(15)
  integer :: i,j
  real(dp) :: x,y,z,pi

  pi = 4d0*atan(1.d0)

  outer: do i = 0,180,45
     inner: do j = 0,180,45
        x = real(i)*pi/180d0
        y = real(j)*pi/180d0
        if ( j == 90 ) cycle inner
        z = sin(x) / cos(y)
        print '(2i6,3f12.6)', i,j,x,y,z
     end do inner
  end do outer
end program nested_doloop
