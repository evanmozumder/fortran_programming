program problem_05
  implicit none
  integer:: n,i,j,k,fact
  real:: f_year,a,u,h,y_pop,u_mult
  real,allocatable:: year(:),pop(:),table(:,:)
  n=9
  f_year=2018.0
  allocate(year(n),pop(n),table(n,n+1))
  year=(/1860.0, 1880.0, 1900.0, 1920.0, 1940.0, 1960.0, 1980.0, 2000.0, 2020.0 /)
  pop=(/249.0, 277.0, 316.0, 350.0, 431.0, 539.0, 689.0, 833.0, 1014.0 /)
  do i=1,n
    table(i,1)=year(i)
    table(i,2)=pop(i)
  end do

  ! construct forward difference table
  do i=3,n+1
    do j=1,n-i+2
      table(j,i)=table(j+1,i-1)-table(j,i-1)
    end do
  end do

  ! printing forward difference table
  write(*,1)"forward differece table : "
  1 format(A,/)

  do i = 1,n
    do j = 1, 2
      write(*,2 , advance="NO")table(i,j)
    end do
    do j = 3, n+2-i
      write(*,2 , advance="NO")table(i,j)
    end do
    write(*,*)
  end do

  2 format(F12.6, 5X)

  a=year(1)
  h=year(2)-year(1)
  u=(f_year-a)/h

  k=1
  y_pop=table(1,2)
  do i=3,n+1
    y_pop=y_pop+(u_mult(u,k)*table(1,i))/fact(k)
    k=k+1
  end do
  write(*,*)'Population in 2018 is : ',y_pop
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
        temp = temp * (u-i)
    end do
    u_mult = temp
end function


