program problem_02
  implicit none
  real(kind=8)::fn,a,b,gn,x,tol,fa,fb,c
  integer:: i
  fn=-(2**(-x))+x**3-(0.5*x**2)+x
  gn=2**(-x)-x**3+0.5*(x**2)
  a=0
  b=1
  tol=0.001
  fa=-(2**(-a))+a**3-(0.5*a**2)+a
  fb=2**(-b)-b**3+0.5*(b**2)
  if(fa*fb>0)then
    write(*,*)'There is no solution between your given end points'
  else if(fa*fb==0)then
    if(fa==0)then
      write(*,*)'Root is: ',a
    else
      write(*,*)'Root is: ',b
    end if
  else
    write(*,*)'There is a solution between your given endpoints'
  end if
  write(*,'(T10,A,T30,A,T50,A,T70,A)')'No','Pn1','Pn','f(Pn)'
  c=(a+b)/2
  do i=1,10000
    fn=-(2**(-c))+c**3-(0.5*c**2)+c
    gn=2**(-c)-c**3+0.5*(c**2)
    write(*,*)i,c,gn,fn
    if(abs(fn)<tol .or. fn==0)then
      write(*,*)c
      exit
    else
      c=gn
    end if
  end do
end program
