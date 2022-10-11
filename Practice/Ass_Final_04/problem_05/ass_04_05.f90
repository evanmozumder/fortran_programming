program ass_04_05
  implicit none
  real,allocatable::year(:),pop(:),table(:,:)
  real::f_y=2018,a,h,u,summ,u_mult
  integer::i,col,row,n=9,k=1,fact
  allocate(year(n),pop(n),table(n,n+1))
  year=(/1860,1880,1900,1920,1940,1960,1980,2000,2020/)
  pop=(/249,277,316,350,431,539,689,833,1014/)
  do i=1,n
    table(i,1)=year(i)
    table(i,2)=pop(i)
  end do
  !construct forward difference table
  do col=3,n+1
    do row=1,n-col+2
      table(row,col)=table(row+1,col-1)-table(row,col-1)
    end do
  end do
  ! printing forward difference table
  write(*,"(10X,A,//)")"Forward Difference Table: "
  do row=1,n
    do col=1,n-row+2
      write(*,"(F15.7,2X)",advance="no")table(row,col)
    end do
    write(*,*)
  end do
  h=year(2)-year(1)
  a=year(1)
  u=(f_y-a)/h
  summ=table(1,2)
  do col=3,n+1
    summ=summ+((u_mult(u,k)*table(1,col))/fact(k))
    k=k+1
  end do
  print*,summ
end program

real function u_mult(u,k)
  real::u,temp
  integer::k,i
  temp=u
  do i=1,k-1
    temp=temp*(u-i)
  end do
  u_mult=temp
end function

integer function fact(k)
  integer::k,mult
  mult=1
  do i=1,k
    mult=mult*i
  end do
  fact=mult
end function
