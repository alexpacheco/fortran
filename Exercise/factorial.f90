program fact
  
  implicit none
  integer :: i
  print *, 'enter integer whose factorial you want to calculate'
  read *, i
  
  print '(i5,a,i20)', i, '! = ', factorial(i)
  
contains
  recursive function factorial(i) result(i_fact)
    integer, intent(in) :: i
    integer :: i_fact
    
    if ( i > 0 ) then
       i_fact = i * factorial(i - 1)
    else
       i_fact = 1
    end if
  end function factorial
  
end program fact
