program problem_02
  implicit none
  real*8::fn,a,b,tol,c,gn,gx1=0,fx,gx
  integer::i=0
  tol=0.001
  a=0
  b=1
  c=(a+b)/2.0
  do while(.true.)
    i=i+1
    fx=fn(c)
    gx=gn(c)
    write(*,*)i,gx,gx1,fx
    if(abs(gx-gx1)<tol)then
      print*,c
      exit
    end if
    gx1=gx
    c=gx
  end do
end program

real*8 function fn(x)
  implicit none
  real*8::x
  fn=-2**(-x)+x**3-(1/2.0)*x**2+x
end function
real*8 function gn(x)
  implicit none
  real*8::x
  gn=2**(-x)-x**3+(1/2.0)*x**2
end function
