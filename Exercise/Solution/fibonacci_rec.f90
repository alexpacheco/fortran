module fibonacci

contains
  recursive function fibr(i) result(fib)
    implicit none
    integer, intent(in) :: i
    integer :: fib

    select case(i)
    case(0)
       fib = 0
    case(1)
       fib = 1
    case default
       fib = fibr(i-1) + fibr(i-2)
    end select
  end function fibr

  
end module fibonacci

program fibrecur

  use fibonacci
  
  implicit none
  integer, parameter :: dp = selected_real_kind(15)
  integer :: i, n


  print *, "Enter the Fibonacci number"
  read *, n

  print *, "n,    f(n)"

  do i = 0, n
     print *, i, fibr(i)
  end do

end program fibrecur
