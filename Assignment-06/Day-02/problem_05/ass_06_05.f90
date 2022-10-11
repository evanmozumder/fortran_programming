program ass_06_05
  implicit none
  real,allocatable::yt(:),xt(:)
  real::f,upper=7.5,lower=0.0,h,resultt
  integer::i,n
  write(*,*)"Enter the subintervals for trapezoidal: "
  read(*,*)n
  allocate(yt(n+1),xt(n+1))
  h=(upper-lower)/n
  xt(1)=lower;xt(n+1)=upper;yt(1)=f(xt(1));yt(n+1)=f(xt(n+1))
  do i=0,n
    xt(i)=i*h+lower
    yt(i)=f(xt(i))
  end do
  call trapizoidal(n,yt,h,resultt)
  print*,resultt
  write(*,*)"Enter the subintervals for simpsons1/3: "
  read(*,*)n
  deallocate(yt,xt)
  allocate(yt(n+1),xt(n+1))
  xt(1)=lower;xt(n+1)=upper;yt(1)=f(xt(1));yt(n+1)=f(xt(n+1))
  do i=0,n
    xt(i)=i*h+lower
    yt(i)=f(xt(i))
  end do
  call simpsons(n,yt,h,resultt)
  print*,resultt
end program

subroutine trapizoidal(n,yt,h,resultt)
  implicit none
  integer::i,n
  real::yt(n+1),resultt,h
  resultt=yt(0)+yt(n)
  do i=1,n-1
    resultt=resultt+(2*yt(i))
  end do
  resultt=resultt*(h/2.0)
end subroutine

real function f(x)
  implicit none
  real::x
  f=1.5*(x**3)-7*x-1.0-exp(x)
end function

subroutine simpsons(n,yt,h,resultt)
  implicit none
  integer::i,n
  real,dimension(n+1)::yt
  real::resultt,h,temp=0
  resultt=yt(0)+yt(n)
  do i=1,n-1
    if(mod(i,2)==0)then
      resultt=resultt+(2.0*yt(i))
    else
      resultt=resultt+(4.0*yt(i))
    end if
  end do
  resultt=resultt*(h/3.0)
end subroutine
