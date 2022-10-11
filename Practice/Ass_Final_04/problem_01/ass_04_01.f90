program ass_04_01
  implicit none
  real::tol,a,b,c,f,fpc=0
  integer::it=1
  open(1,file="output.txt")
!  real::f
  tol=0.000001
  a=-2;b=0
  !bisection method
  write(1,"(10X,A,15X,A,30X,A,40X,A,40X,A,//)",advance="no")"Iteration","a","b","Pn","(Pn-Pn-1)/Pn"
  6 if(f(a)*f(b)<0)then
    c=(a+b)/2.0
    if(f(a)*f(c)<0)then
      b=c
    else
      a=c
    end if
    write(1,"(15X,I0,15X,F15.10,15X,F15.10,15X,F15.10,15X,F15.10,/)",advance="no")it,a,b,f(c),(f(c)-fpc)/f(c)
    if(abs((f(c)-fpc)/f(c))<tol)then
      write(1,*)"Root is: ",c
    else
      fpc=f(c)
      it=it+1
      goto 6
    end if
  end if
end program ass_04_01

real function f(x)
  real::x
  f=1.5*(x**3)-7*x-1-exp(x)
end function
