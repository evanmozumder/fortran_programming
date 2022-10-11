program problem_04
  implicit none
  real*8::a,b,fa,fb,fn,c,fc,tol,fc1
  integer::i=0
  logical::check=.true.
  a=-2
  b=0
  tol=0.000001
  fa=fn(a)
  fb=fn(b)
  if(fa*fb<0)then
    print*,"Solution Exist"
    write(*,'(/,T12,A,T25,A,T50,A,T75,A,T97,A,/)',advance="no")"It","a","b","Pn","Pn-Pn-1/Pn"
    fc1=0
    do while(check)
      i=i+1
      fa=fn(a)
      fb=fn(b)
      c=(a+b)/2.0
      fc=fn(c)
      write(*,*)i,a,b,fc,abs(fc-fc1)/abs(fc)
      if(abs(fc-fc1)/abs(fc)<tol)then
        print*,c
        check=.false.
      end if
      fc1=fc
      if(fa*fc<0)then
        b=c
      else if(fb*fc<0)then
        a=c
      end if
    end do
  else if(fa*fb==0)then
    if(fa==0)then
      print*,"Solution is: ",a
    else
      print*,"Solution is: ",b
    end if
  else
    print*,"Solution doesn't exist between a and b"
  end if
end program

real*8 function fn(x)
  implicit none
  real*8::x
  fn=(1.5*x**3)-(7*x)-1-(2.71828**x)
end function
