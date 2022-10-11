program ass_04_02
  implicit none
  real(kind=8)::a=0,b=1,f,g,tol=0.001,c,gx
  integer::it=1
  logical::cond=.true.
  if(f(a)*f(b)>0)then
    write(*,*)"No root exist between given interval"
  else if(f(a)*f(b)==0)then
    if(f(a)==0)then
      write(*,*)"root is: ",a
    else
      write(*,*)"root is: ",b
    end if
  else
    write(*,"(A,//)")"Root lies between given interval"
    write(*,"(10X,A,15X,A,15X,A,15X,A)",advance="no")"Iteration","Pn-1","Pn","f(Pn)"
    c=(a+b)/2.0
    do while(cond)
      gx=g(c)
      write(*,*)it,c,gx,f(gx)
      it=it+1
      if(abs(c-gx)<tol)then
        cond=.false.
        write(*,*)"Root is: ",gx
      end if
      c=gx
    end do
  end if
end program

real(kind=8) function f(x)
  real(kind=8)::x
  f=-(2**(-x))+x**3-0.5*(x**2)+x
end function

real(kind=8) function g(x)
  real(kind=8)::x
  g=2**(-x)-x**3+0.5*(x**2)
end function