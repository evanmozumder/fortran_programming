program ass_04_02
  implicit none
  real::f,a=0,b=1,tol=0.001,c
  c=(a+b)/2
  c=g(c)
end program

real function f(x)
  real::x
  f=-(2**(-x))+x**3-0.5*(x**2)+x
end function

real function g(x)
  real::x
  g=2**(-x)-x**3+0.5*(x**2)
end function
