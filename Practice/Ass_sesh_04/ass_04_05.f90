program ass_04_05
  implicit none
  integer,parameter::n=9
  integer::i,j,k,fact
  real::year(n),pop(n),table(n,n+1),f_y=2018,summ,h,u,u_mult
  year=(/1860,1880,1900,1920,1940,1960,1980,2000,2020/)
  pop=(/249,277,316,350,431,539,689,833,1014/)
  do i=1,n
    table(i,1)=year(i)
    table(i,2)=pop(i)
  end do
  do j=3,n+1!col
    do i=1,n-j+2 !row
      table(i,j)=table(i+1,j-1)-table(i,j-1)
    end do
  end do
  do i=1,n
    do j=1,n-i+2
      write(*,"(F10.3)",advance="no")table(i,j)
    end do
    write(*,*)
  end do
  h=(year(2)-year(1))
  u=(f_y-year(1))/h
  summ=pop(1)
  k=1
  do i=3,n+1
    summ=summ+(u_mult(u,k)*table(1,i))/fact(k)
    k=k+1
  end do
  print*,summ
end program

real function u_mult(u,k)
  real::u,temp
  integer::k,i
  temp=u
  do i=1,k-1
    temp=temp*(u-k)
  end do
  u_mult=temp
end function

integer function fact(n)
  implicit none
  integer::n,i,temp
  temp=1
  do i=1,n
    temp=temp*i
  end do
  fact=temp
end function
