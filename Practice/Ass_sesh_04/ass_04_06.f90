program ass_04_06
  implicit none
  real,allocatable::x(:),y(:),table(:,:)
  integer::n=8,col,row,i,k,fact,j
  real::a,h,u,xf,summ,u_mult
  allocate(x(n),y(n),table(n,n+1))
  x=(/20,40,60,80,100,120,140,160/)
  y=(/10,50,109,180,300,420,565,771/)
  write(*,*)"Enter value: "
  read(*,*)xf
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
    do j=1,i+1
      write(*,"(F10.3,3X)",advance="no")table(i,j)
    end do
    write(*,*)
  end do
end program
