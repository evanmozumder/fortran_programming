program ass_06_07
  implicit none
  real,allocatable,dimension(:)::xt,yt
  real::upper,lower,h,resultt,f
  integer::n,i,m=5
  write(*,*)"Enter intervals for weddle's rule: "
  read(*,*)n
  allocate(xt(n+1),yt(n+1))
  lower=exp(1.0);upper=2.0*exp(1.0)
!  lower=2.718281828;upper=2*2.718281828
  h=(upper-lower)/n
  do i=0,n
    xt(i)=i*h+lower
    yt(i)=f(xt(i))
  end do
  call weddles(n,h,yt,resultt)
  print*,resultt



  resultt=0
  do i=0,n
    if(mod(i,2)==0)then
      if(mod(i,6)==0 .and. i/=0)then
        resultt=resultt+(2*yt(i))
        m=5
        write(*,100,advance="no")yt(i)
        100 format("2*",F10.8," + ")
      else
        resultt=resultt+yt(i)
        write(*,99,advance="no")yt(i)
        99 format(F10.8," + ")
      end if
    else
      resultt=resultt+(m*yt(i))
      write(*,98,advance="no")m,yt(i)
      98 format(I0,"*",F10.8," + ")
      if(m==5)then
        m=6
      else
        m=5
      end if
    end if
  end do
  write(*,"(/,/,/,F12.8)")resultt
end program

real function f(x)
  implicit none
  real::x
  f=1.0/(x*log(x))
end function

subroutine weddles(n,h,yt,resultt)
  implicit none
  integer::n,i
  real::yt(n+1),h,resultt,m
  resultt=0.0;m=5.0
  do i=0,n
    if(mod(i,2)==0)then
      if(mod(i,6)==0 .and. i/=0)then
        resultt=resultt+(2*yt(i))
        m=5
      else
        resultt=resultt+yt(i)
      end if
    else
      resultt=resultt+(m*yt(i))
      if(m==5)then
        m=6
      else
        m=5
      end if
    end if
  end do
  resultt=resultt*((3.0*h)/10.0)
end subroutine
