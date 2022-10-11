program ass_06_06
  implicit none
  real,allocatable::yt(:),xt(:)
  real::f,upper,lower,h,resultt,pi=3.14159
  integer::i,n
  write(*,*)"Enter the subintervals for simpsons3/8 : "
  read(*,*)n
  allocate(yt(n+1),xt(n+1))
  upper=pi/4;lower=0
  h=(upper-lower)/n
  do i=0,n
    xt(i)=i*h+lower
    yt(i)=f(xt(i))
  end do
  call simpsonsThreeByEight(n,yt,h,resultt)
  print*,resultt
end program

real function f(x)
  implicit none
  real::x
  f=exp(3.0*x)*sin(2.0*x)
end function

subroutine simpsonsThreeByEight(n,yt,h,resultt)
  implicit none
  integer::i,n
  real::yt(n+1),resultt,h
  resultt=yt(0)+yt(n)
  do i=1,n-1
    if(mod(i,3)==0)then
      resultt=resultt+(2*yt(i))
    else
      resultt=resultt+(3*yt(i))
    end if
  end do
  resultt=resultt*((3.0*h)/8.0)
end subroutine
