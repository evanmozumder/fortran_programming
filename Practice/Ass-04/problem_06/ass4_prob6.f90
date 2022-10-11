program problem_06
  implicit none
  real,allocatable::x(:),y(:),table(:,:)
  integer::n=8,i,j,f,fact,k
  real::a,h,u,ans,u_mult
  read(*,*)f
  allocate(x(n),y(n),table(n,n+1))
  x=(/20.0, 40.0, 60.0, 80.0, 100.0, 120.0, 140.0, 160.0 /)
  y=(/10.0, 50.0, 109.0, 180.0, 300.0, 420.0, 565.0, 771.0 /)
  do i=1,n
    table(i,1)=x(i)
    table(i,2)=y(i)
  end do

  do j=3,n+1
    do i=n,j-1,-1
      table(i,j)=table(i,j-1)-table(i-1,j-1)
    end do
  end do
  do i=1,n
    do j=1,n+1
      if(j>2 .and. i<j-1)cycle
      write(*,'(F12.6,5X)',advance="no")table(i,j)
    end do
    write(*,*)
  end do
  a=x(n)
  h=x(2)-x(1)
  u=(f-a)/h
  ans=y(n)
  k=1
!  write(*,*)fact(6)
  do i=3,n+1
!    print*,table(n,i)
    ans=ans+(u_mult(u,k)*table(n,i))/fact(k)
    k=k+1
  end do
  write(*,*)ans
end program

!recursive function fact(n) result(ans)
!  implicit none
!  integer::n,ans,temp
!  temp=1
!  if(n>0)then
!    temp=n*fact(n-1)
!  end if
!  ans=temp
!end function
integer function fact(n)
  implicit none
  integer::n,i,temp
  temp=1
  do i=1,n
    temp=temp*i
  end do
  fact=temp
end function

real function u_mult(u,k)
  implicit none
  real::temp,u
  integer::i,k
  temp=u
  do i=1,k-1
    temp=temp*(u-i)
  end do
  u_mult=temp
end function
