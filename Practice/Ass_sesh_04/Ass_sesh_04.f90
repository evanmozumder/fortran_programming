program ass_04_01
  implicit none
  real::a=7.5,b=0.0,tol=0.000001,c
  logical::cond=.true.
  do while(cond)
    if(f(a)*f(b)>0)then
      write(*,*)"There is no solution"
    else if(f(a)*f(b)==0)then
      if(f(a)==0)then
        write(*,*)"Solution is: ",a
      else
        write(*,*)"Solution is: ",b
      end if
    end if
    c=(a+b)/2
    if(f(a)*f(c)<0)then
      b=c
    else
      a=c
    end if
    write(*,)
  end do
end program

real function f(x)
  implicit none
  real::x
  f=1.5*x**3-7*x-1-exp(x)
end function
