program ass_06_05
  implicit none
  real::upper=7.5,lower=0.0

end program

real function f(x)
  real::x
  f=1.5*(x**3)-7*x-1-exp(x)
end function
