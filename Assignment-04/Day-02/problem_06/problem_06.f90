program problem_06
  implicit none
  integer:: n,i,j,k,fact
  real:: f,a,u,h,app,u_mult
  real,allocatable:: x(:),y(:),table(:,:)
  read(*,*)f
  n=8
  allocate(x(n),y(n),table(n,n+1))
  x=(/20.0, 40.0, 60.0, 80.0, 100.0, 120.0, 140.0, 160.0 /)
  y=(/10.0, 50.0, 109.0, 180.0, 300.0, 420.0, 565.0, 771.0 /)
  do i=1,n
    table(i,1)=x(i)
    table(i,2)=y(i)
  end do

  ! construct backward difference table
  do i=3,n+1
    do j=n,i-1,-1
      table(j,i)=table(j,i-1)-table(j-1,i-1)
    end do
  end do

  ! printing backward difference table
  write(*,1)"backward differece table : "
  1 FORMAT(A,/)

  do i=1,n
    do j=1,n+1
      if(j>2 .and. i<j-1)then
        continue
      else
        write(*,2,advance='no')table(i,j)
      end if

    end do
    write(*,*)
  end do

  2 FORMAT(F12.6, 5X)

  a=x(n)
  h=x(2)-x(1)
  u=(f-a)/h

  k=1
  app=table(n,2)
  do i=3,n+1
    app=app+(u_mult(u,k)*table(n,i))/fact(k)
    k=k+1
  end do
  write(*,*)'for your given y value: ',app
end program

integer function fact(n)
    implicit none
    integer, intent(in)::n
    integer temp, i
    temp = 1
    do i = 1, n
        temp = temp * i
    end do
    fact = temp
end function fact

real function u_mult(u,k)
    implicit none
    real, intent(in):: u
    integer, intent(in):: k
    real:: temp
    integer:: i
    temp = u
    do i = 1, k-1
        temp = temp * (u+i)
    end do
    u_mult = temp
end function


