program practice_05
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

  ! construct the forward difference table
  do i=3,n+1
    do j=1,n-i+2
      table(j,i)=table(j+1,i-1)-table(j,i-1)
    end do
  end do


end program

integer function fact(n)
  integer:: temp=1,i,n
  do i=1,n
    temp=temp*i
  end do
  fact=temp
end function

real function u_mult(u,k)
  real:: u,temp
  integer:: k
  temp=u
  do i=1,k-1
    temp=temp*(u-1)
  end do
  u_mult=temp
end function
