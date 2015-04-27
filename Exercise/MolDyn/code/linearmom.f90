subroutine linearmom(vel)
  use precision
  use param, only : natom
  implicit none
  real(dp), dimension(:,:), intent(inout) :: vel
  integer(ip) :: i
  real(dp) :: vcm(3)

  ! First get center of mass velocity
  vcm = 0d0
  do i = 1, 3
     vcm(i) = sum(vel(:,i))
  end do
  vcm = vcm / real(natom,dp)
  
  ! Now remove center of mass velocity from all atoms
  do i = 1, natom
     vel(i,:) = vel(i,:) - vcm(:)
  end do

end subroutine linearmom

