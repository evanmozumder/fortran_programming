program problem_03_practice
  implicit none
  real*8::x,f,df,p,tol=0.00001
  integer::i=0
  write(*,*)"enter initial guess"
  read(*,*)x
  do while(.true.)
    i=i+1
    if(df(x)==0)then
      write(*,*)"Method Fails"
      exit
    end if
    p=x-(f(x)/df(x))
    write(*,*)i,f(x),df(x),p,f(p)
    if(abs(p-x)<tol)then
      write(*,*)"Solution is: ",p
      exit
    end if
    x=p
  end do
end program

real*8 function f(x)
  implicit none
  real*8::x
  f=16*x**4+88*x**3+159*x**2+76*x-240
end function

real*8 function df(x)
  implicit none
  real*8::x
  df=64*x**3+264*x**2+318*x+76
end function
