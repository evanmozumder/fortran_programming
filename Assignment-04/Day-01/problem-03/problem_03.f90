program problem_03
  implicit none
  real::a,x,tol,p,f,df
  integer:: i
  write(*,*)'Enter the initial guess: '
  read(*,*)a
  write(*,*)'Enter tolerance'
  read(*,*)tol
  write(*,'(T10,A,T15,A,T50,A,T70,A,T90,A)')'No','Pn-1','df(Pn-1)','Pn','f(Pn)'
  do i=1,1000
    if(df(a)==0)then
      write(*,*)'Method Fails'
      exit
    end if
    p=a-(f(a)/df(a))
    write(*,'(10X,I0,F17.10,3X,F17.10,3X,F17.10,3X,F17.10)')i,a,df(a),p,f(p)
    if(abs(p-a)<tol)then
      write(*,*)'The root is: ',p
      exit
    end if
    a=p
  end do
end program

function f(x)
  f=16*(x**4)+88*(x**3)+159*(x**2)+76*x-240
end function

function df(x)
  df=64*(x**3)+264*(x**2)+318*x+76
end function
