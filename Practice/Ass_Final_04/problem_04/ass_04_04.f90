program ass_04_04
  implicit none
  real::a=0,b=1,tol=0.000001,c,f
  integer::it=1
  logical::cond=.true.
  write(*,"(10X,A,13X,A,17X,A,17X,A,17X,A,17X,A,17X,A,//)",advance="no")"Iteration","a","f(a)","b","f(b)","c","f(c)"
  do while(cond)
    c=a-((f(a)*(a-b))/(f(a)-f(b)))
    write(*,"(15X,I0,5X,F15.7,5X,F15.7,5X,F15.7,5X,F15.7,5X,F15.7,5X,F15.7,/)",advance="no")it,a,f(a),b,f(b),c,f(c)
    if(f(a)*f(c)<0)then
      b=c
    else
      a=c
    end if
    if(abs(a-b)<tol)then
      write(*,*)"root is: ",c
      cond=.false.
    end if
    it=it+1
  end do
end program

real function f(x)
  real::x
  f=230*(x**4)+18*(x**3)+9*(x**2)-221*x-9
end function
