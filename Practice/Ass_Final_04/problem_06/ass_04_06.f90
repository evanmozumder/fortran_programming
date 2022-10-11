program ass_04_06
  implicit none
  real,allocatable::x(:),y(:),table(:,:)
  integer::n=8,col,row,i,k,fact
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
  !construct backward difference table
  do col=3,n+1
    do row=n,col-1,-1
      table(row,col)=table(row,col-1)-table(row-1,col-1)
    end do
  end do
  write(*,*)"Backward Difference Table: "
  do row=1,n
    do col=1,row+1
      write(*,"(F15.7,5X)",advance="no")table(row,col)
    end do
    write(*,*)
  end do
  a=x(n);h=x(2)-x(1);u=(xf-a)/h;k=1
  summ=y(n)
  do i=3,n+1
    summ=summ+(u_mult(u,k)*table(n,i))/fact(k)
    k=k+1
  end do
  print*,summ
end program

real function u_mult(u,k)
  real::u,temp
  integer::k,i
  temp=u
  do i=1,k-1
    temp=temp*(u+i)
  end do
  u_mult=temp
end function

integer function fact(k)
  integer::k,temp,i
  temp=1
  do i=1,k
    temp=temp*i
  end do
  fact=temp
end function
