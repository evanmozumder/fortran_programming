program problem_04
  implicit none
  real::a,b,tol,fn,c
  integer::i=0
  a=0
  b=1
  tol=0.000001
  write(*,'(7A)')"It","a","f(a)","b","f(b)","c","f(c)","abs(a-b)"
  do while(.true.)
    i=i+1
    c=(a*fn(b)-b*fn(a))/(fn(b)-fn(a))
    if(fn(a)*fn(c)<0)then
      b=c
    else
      a=c
    end if
    write(*,*)i,a,fn(a),b,fn(b),c,fn(c),abs(a-b)
    if(abs(a-b)<tol)then
      print*,c
      exit
    end if
  end do
end program

real function fn(x)
  implicit none
  real::x
  fn=230*(x**4)+18*(x**3)+9*(x**2)-(221*x)-9
end function
