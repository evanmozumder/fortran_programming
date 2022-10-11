program problem_04
  implicit none
  real*8::a=-2,b=0,c,tol,fc=0,f
  integer::i=0
  logical::check=.true.
  tol=0.000001
  if(f(a)*f(b)>0)then
    write(*,*)"There is no solution exist between given end points"
  else if(f(a)*f(b)==0)then
    if(f(a)==0)then
      write(*,*)a
    else
      write(*,*)b
    end if
  else
!    c=(a+b)/2
!    fc=f(c)
!    write(*,*)tol
    do while(check)
      i=i+1
      c=(a+b)/2
      if(f(a)*f(c)<0)then
        b=c
      else
        a=c
      end if
      write(*,*)i,a,b,f(c),fc,abs(f(c)-fc)/abs(f(c))
      if((abs(f(c)-fc)/abs(f(c)))<tol .and. i/=1)then
        write(*,*)"Solution is: ",c
        check=.false.
      end if
      fc=f(c)

    end do
  end if
end program

real*8 function f(x)
  implicit none
  real*8::x
  f=1.5*x**3-7*x-1-2.71828**x
end function
