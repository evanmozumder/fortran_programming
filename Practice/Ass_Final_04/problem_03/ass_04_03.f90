program ass_04_03
  implicit none
  real::a,tol=0.00001,p,f,df
  integer::it=1
  logical::cond=.true.
  6 write(*,*)"enter initial guess: "
  read(*,*)a
  write(*,"(10X,A,15X,A,20X,A,20X,A,20X,A,//)",advance="no")"Iteration","Pn-1","df(Pn-1)","Pn","f(Pn)"
  do while(cond)
    if(df(a)==0)then
      write(*,*)"Method Fails"
      cond=.false.
      exit
    end if
    p=a-(f(a)/df(a))
    write(*,"(15X,I0,10X,F15.7,10X,F15.7,10X,F15.7,10X,F15.7,/)",advance="no")it,a,df(a),p,f(p)
    it=it+1
    if(abs(p-a)<tol)then
      write(*,*)"Root is: ",p
      exit
    end if
    a=p
  end do
  go to 6
end program

real function f(x)
  real::x
  f=16*(x**4)+88*(x**3)+159*(x**2)+76*x-240
end function

real function df(x)
  real::x
  df=64*(x**3)+264*(x**2)+318*x+76
end function
