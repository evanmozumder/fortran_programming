program problem_02
  implicit none
  real*8::a=0,b=1,tol=0.001,c,f,g,p
  logical::check=.true.
  integer::i=0
  if(f(a)*f(b)>0)then
    write(*,*)"There no solution exist",f(a),f(b)
  else if(f(a)*f(b)==0)then
    if(f(a)==0)then
      write(*,*)"Solution is: ",a
    else
      write(*,*)"Solution is: ",b
    end if
  else
    write(*,*)"There is solution between given end points"
    c=(a+b)/2
    write(*,'(T10,A,T20,A,T50,A,T80,A,T100,A,/)',advance="no")"it","Pn-1","Pn","f(Pn)","Pn-Pn-1"
    do while(check)
      i=i+1
      p=g(c)
      write(*,*)i,c,p,f(p),abs(p-c)
      if(abs(p-c)<tol)then
        write(*,*)"Solution is: ",p
        check=.false.
        exit
      end if
      c=p
    end do
  end if
end program

real*8 function f(x)
  implicit none
  real*8::x
  f=-2**(-x)+(x**3)-0.5*(x**2)+x
end function

real*8 function g(x)
  implicit none
  real*8::x
  g=(2**(-x))-(x**3)+(0.5*(x**2))
end function
