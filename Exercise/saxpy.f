C234567890123456789012345678901234567890123456789012345678901234567890
      program test
      integer n
      parameter(n=100)
      real alpha, x(n), y(n)

      alpha = 2.0
      do 10 i = 1,n
         x(i) = 1.0
         y(i) = 2.0
 10   continue

      call saxpy(n,alpha,x,y)

      return
      end

      subroutine saxpy(n, alpha, x, y)
      integer n
      real alpha, x(*), y(*)
c
c Saxpy: Compute y := alpha*x + y,
c where x and y are vectors of length n (at least).
c
      do 20 i = 1, n
         y(i) = alpha*x(i) + y(i)
 20   continue
      
      return
      end
      
