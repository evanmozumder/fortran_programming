program problem_03_practice
  implicit none
  real*8::fx,dfx,x,tol,p
  integer::i=0
  read(*,*)x,tol
  do while(.true.)
    i=i+1
    if(dfx(x)==0)then
      print*,"Method Fails"
      exit
    end if
    p=x-(fx(x)/dfx(x))
    write(*,*)i,x,fx(x),dfx(x),p,fx(p)
    if(abs(p-x)<tol)then
      write(*,*)"The root is: ",p
      exit
    end if
    x=p
  end do
end program

real*8 function fx(x)
  implicit none
  real*8::x
  fx=16*(x**4)+88*(x**3)+159*(x**2)+76*x-240
end function

real*8 function dfx(x)
  implicit none
  real*8::x
  dfx=64*(x**3)+264*(x**2)+318*x+76
end function
