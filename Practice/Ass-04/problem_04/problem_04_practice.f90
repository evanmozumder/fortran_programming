program problem_04_practice
  implicit none
  real*8::a=0,b=1,tol=0.000001,c,f
  integer::i=0
  do while(.true.)
    i=i+1
    c=(a*f(b)-b*f(a))/(f(b)-f(a))
    if(f(a)*f(c)<0)then
      b=c
    else
      a=c
    end if
    write(*,*)i,a,f(a),b,f(b),c,f(c),abs(a-b)
    if(abs(a-b)<tol)then
      write(*,*)"Solution is: ",c
      exit
    end if
  end do
end program

real*8 function f(x)
  implicit none
  real*8::x
  f=230*x**4+18*x**3+9*x**2-221*x-9
end function
