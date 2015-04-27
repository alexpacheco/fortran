subroutine verlet(coord, coord_t0, vel, vel_t0, acc, acc_t0, force, pener)
  use precision
  use param, only : natom, mass, dt, boxl, pot
  implicit none
  real(dp), dimension(:,:), intent(in) :: coord_t0, vel_t0, acc_t0
  real(dp), dimension(:,:), intent(out) :: coord, vel, acc, force
  real(dp), intent(out) :: pener
  integer(ip) :: i
  
  interface
     subroutine get_pot_force(coord, force, pener)
       use precision
       implicit none
       real(dp), dimension(:,:), intent(in) :: coord
       real(dp), dimension(:,:), intent(out) :: force
       real(dp), intent(out) :: pener
     end subroutine get_pot_force
  end interface

  ! Set coordinates, velocity, acceleration and force at next time step to zero
  coord = 0d0 ; vel = 0d0 ; acc = 0d0 
  
  ! Get new atom positions from Velocity Verlet Algorithm  
  coord = coord_t0 + vel_t0 * dt + 0.5d0 * acc_t0 * dt ** 2
  do i = 1, natom
     ! Apply PBC to coordinates
     where ( coord(i,:) > boxl(:) )
        coord(i,:) = coord(i,:) - boxl(:)
     elsewhere ( coord(i,:) < 0d0 )
        coord(i,:) = coord(i,:) + boxl(:)
     end where
  end do
  
  ! Get Potential and force at new atom positions
  call get_pot_force(coord, force, pener)
  
  ! Calculate Acceleration and Velocity  at current time step
  acc = force / mass
  vel = vel_t0 + 0.5d0 * ( acc + acc_t0 ) * dt
  
end subroutine verlet

subroutine get_pot_force(coord, force, pener)
  use precision
  use potential
  use param, only : natom, boxl
  implicit none
  real(dp), dimension(:,:), intent(in) :: coord
  real(dp), dimension(:,:), intent(out) :: force
  real(dp), intent(out) :: pener
  integer(ip) :: i, j
  real(dp) :: epot
  real(dp) :: r(3), f(3)

  pener = 0d0
  force = 0d0
  do i = 1, natom - 1
     do j = i + 1, natom
        r(:) = coord(i,:) - coord(j,:)
        ! minimum image criterion
        r = r - nint( r / boxl ) * boxl
        select case(pot)
        case('mp')
           call morse( r, f, epot )
        case default
           call lennard_jones( r, f, epot )
        end select
        pener = pener + epot
        force(i,:) = force(i,:) + f(:)
        force(j,:) = force(j,:) - f(:)
     end do
  end do

end subroutine get_pot_force
