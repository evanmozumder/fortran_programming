program problem_01
  implicit none
  real(kind=8):: fn,x,tol,a,b,c,fa,fb,fn1
  integer:: i
  fn=1.5*(x**3)-7*x-1-(2.71828**x)
  a=-2
  b=0
  tol=0.000001
  fa=1.5*(a**3)-7*a-1-(2.71828**a)
  fb=1.5*(b**3)-7*b-1-(2.71828**b)
  if(fa*fb>0)then
    write(*,*)'There is no solution between your given endpoints'
  else if(fa*fb<0)then
    write(*,*)'There is a solution between your given endpoints'
  else
    if(fa==0)then
      write(*,*)'The root is: ',a
    else
      write(*,*)'The root is: ',b
    end if
  end if
  c=(a+b)/2
  fn1=1.5*(c**3)-7*c-1-(2.71828**c)
  if(fa*fn1>0)then
    a=c
  else if(fa*fn1<0)then
    b=c
  end if
  write(*,'(T15,A,T30,A,T45,A,T60,A,T75,A)')'No','a','b','Pn','(Pn-P(n-1))/Pn'
  do i=1,1000
    c=(a+b)/2
    fn=1.5*(c**3)-7*c-1-(2.71828**c)
    fa=1.5*(a**3)-7*a-1-(2.71828**a)
    fb=1.5*(b**3)-7*b-1-(2.71828**b)
    write(*,*)i,a,b,fn,abs(fn-fn1)/abs(fn)
    if(fa*fn>0)then
      a=c
    else if(fa*fn<0)then
      b=c
    end if
    if(abs(fn-fn1)/abs(fn)<tol .or. fn==0)then
      write(*,*)c
      exit
    end if
    fn1=fn
  end do
end program
