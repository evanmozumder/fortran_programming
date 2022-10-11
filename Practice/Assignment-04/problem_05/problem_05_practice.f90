program problem_05_practice
  implicit none
  real,allocatable::year(:),pop(:),table(:,:)
  real::a,h,u,f_year=2018,y_pop
  integer::n,i,j,k
  n=9
  allocate(year(n),pop(n),table(n,n+1))
  year=(/1860,1880,1900,1920,1940,1960,1980,2000,2020/)
  pop=(/249,277,316,350,431,539,689,833,1014/)
  do i=1,n
    table(i,1)=year(i)
    table(i,2)=pop(i)
  end do
  do j=3,n+1
    do i=1,n-j+2
      table(i,j)=table(i+1,j-1)-table(i,j-1)
    end do
  end do
  write(*,'(A,/)')"Forward Difference Table: "
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
  y_pop=pop(1)
  do i=3,n+1
!    y_pop=y_pop+(u_mult(u,k)*table(1,i))/fact(k)
  end do
end program

real function u_mult(u,k)
  implicit none
  real::u
  integer::k
end function
