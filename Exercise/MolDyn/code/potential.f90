module potential
  use precision
  implicit none
  real(dp) :: r2, r6, d2, d
  real(dp), parameter :: de = 0.176d0, a = 1.4d0, re = 1d0
  real(dp) :: exparre
  
contains
  subroutine lennard_jones(r,f,p)
    ! Lennard Jones Potential
    ! V = 4 * epsilon * [ (sigma/r)**12 - (sigma/r)**6 ]
    !   = 4 * epsilon * (sigma/r)**6 * [ (sigma/r)**2 - 1 ]
    !   = 4 * r**(-6) * [ r**(-2) - 1 ] for epsilon=sigma=1
    ! F_i = 48 * epsilon * (sigma/r)**6 * (1/r**2) * [ ( sigma/r)** 6 - 0.5 ] * i where i = x,y,z
    !     = 48 * r**(-8) * [ r**(-6) - 0.5 ] * i  for epsilon=sigma=1    implicit none
    implicit none
    real(dp), dimension(:), intent(in) :: r
    real(dp), dimension(:), intent(out) :: f
    real(dp), intent(out) :: p

    r2 = 1.d0 / dot_product(r,r)
    r6 = r2 ** 3

    f = dvdr_lj(r2, r6) * r
    p = pot_lj(r2, r6)
  end subroutine lennard_jones

  subroutine morse(r,f,p)
    ! Morse Potential
    ! V = D * [ 1 - exp(-a*(r - re)) ]^2
    ! F_i = 2*D * [ 1 - exp(-a*(r - re)) ] * a exp(-a*(r-re)) * i / r  
    implicit none
    real(dp), dimension(:), intent(in) :: r
    real(dp), dimension(:), intent(out) :: f
    real(dp), intent(out) :: p

    d2 = dot_product(r,r)
    d = sqrt(d2)
    exparre = exp( -a * (d - re ))
    
    f = dvdr_mp(exparre) * r
    p = pot_mp(exparre)
  end subroutine morse

  function pot_lj(r2, r6)
    implicit none
    real(dp), intent(in) :: r2, r6
    real(dp) :: pot_lj
    pot_lj = 4d0 * r6 * ( r6 - 1.d0 )
  end function pot_lj
  function pot_mp(exparre)
    implicit none
    real(dp), intent(in) :: exparre
    real(dp) :: pot_mp
    pot_mp = de * ( 1d0 - exparre )**2
  end function pot_mp

  function dvdr_lj(r2,r6)
    implicit none
    real(dp), intent(in) :: r2, r6
    real(dp) :: dvdr_lj
    dvdr_lj = 48d0 * r2 * r6 * ( r6 - 0.5d0 ) 
  end function dvdr_lj
  function dvdr_mp(exparre)
    implicit none
    real(dp), intent(in) :: exparre
    real(dp) :: dvdr_mp
    dvdr_mp = 2d0 * de * a * (1d0 - exparre) * exparre
  end function dvdr_mp
end module potential

