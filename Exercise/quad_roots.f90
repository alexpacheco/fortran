program roots_of_quad_eqn

  implicit none

  real(kind=8) :: a,b,c
  real(kind=8) :: roots(2),d

  print *, '============================================'
  print *, ' Program to solve a quadratic equation'
  print *, '    ax^2 + bx + c = 0 '
  print *, ' If d = b^2 - 4ac >= 0 '
  print *, '   then solutions are: '
  print *, '     (-b +/- sqrt(d) )/2a '
  print *, '============================================'

  ! read in coefficients a, b, and c
  write(*,*) 'Enter coefficients a,b and c'
  read(*,*) a,b,c
  write(*,*)
  write(*,*) ' Quadratic equation to solve is: '
  write(*,fmt='(a,f6.3,a,f6.3,a,f6.3,a)') '   ',a,'x^2 + ',b,'x + ',c,' = 0'
  write(*,*)

  outer: if ( a == 0d0 ) then
     middle: if ( b == 0.d0 ) then
        inner: if ( c == 0.d0 ) then
           write(*,*) 'Input equation is 0 = 0'
        else
           write(*,*) 'Equation is unsolvable'
           write(*,fmt='(a,f5.3,a)') ' ',c,' = 0'
        end if inner
     else
        write(*,*) 'Input equation is a Linear equation with '
        write(*,fmt='(a,f6.3)') ' Solution: ', -c/b
     end if middle
  else
     d = b*b - 4d0*a*c
     dis0: if ( d > 0d0 ) then
        d = sqrt(d)
        roots(1) = -( b + d)/(2d0*a)
        roots(2) = -( b - d)/(2d0*a)
        write(*,fmt='(a,2f12.6)') 'Solution: ', roots(1),roots(2)
     else if ( d == 0.d0 ) then
        write(*,fmt='(a,f12.6)') 'Both solutions are equal: ', -b/(2d0*a)
     else
        write(*,*) 'Solution is not real'
        d = sqrt(abs(d))
        roots(1) = d/(2d0*a)
        roots(2) = -d/(2d0*a)
        write(*,fmt='(a,ss,f6.3,sp,f6.3,a2,a,ss,f6.3,sp,f6.3,a2)') &
             ' (',-b/(2d0*a),sign(roots(1),roots(1)),'i)',' and (',-b/(2d0*a),sign(roots(2),roots(2)),'i)'
     end if dis0
  end if outer

end program roots_of_quad_eqn
