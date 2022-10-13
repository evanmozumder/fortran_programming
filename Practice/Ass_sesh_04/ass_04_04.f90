program ass_04_04
  implicit none
  integer::it=1
  real::a=0,b=1,tol=0.000001,c
  do while(.true.)
    it=it+1
    c=(a*f(b)-b*f(a))/(f(b)-f(a))
    if(f(a)*f(c)<0)then
      b=c
    else
      a=c
    end if

  end do
end program

real function f(x)
  real::x
  f=230*(x**4)+18*(x**3)+9*(x**2)-221*x-9
end function
