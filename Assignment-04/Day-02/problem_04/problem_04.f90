program problem_04
  implicit none
  real:: a,b,c,f,tol
  integer:: i=1
  logical:: check=.true.
  a=0.0
  b=1.0
  tol=.000001
  write(*,1)"iteration","a","f(a)","b","f(b)","c","f(c)"
  1 format(7(A,15X))
  do while(check)
    c=a-((f(a)*(a-b))/(f(a)-f(b)))
    if(f(a)*f(c)<0)then
      b=c
    else
      a=c
    end if
    write(*,2)i,a,f(a),b,f(b),c,f(c)
    2 format(5X,I0,6(3X,F15.8))
    if(abs(a-b).le.tol .or. f(c)==0)then
      write(*,*)'root is : ',c
      check=.false.
    end if
    i=i+1
  end do
end program

function f(x)
  f=230*(x**4)+18*(x**3)+9*(x**2)-(221*x)-9
end function
