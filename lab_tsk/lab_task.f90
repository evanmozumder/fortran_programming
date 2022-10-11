program lab_task
  implicit none
  integer:: i,j,k,l,n=4
  real::a(4,5),mult,x(4),temp(5),s
  open(1,file='equ.txt')
  do i=1,n
    do j=1,n+1
      read(1,*)a(i,j)
    end do
  end do
!  l=1
    do j=1,n+1
      if(a(1,j)==0)then
        temp(j)=a(1,j)
        a(1,j)=a(2,j)
        a(2,j)=temp(j)
      end if
    end do
!  write(*,'(5(I0,2X),/)',advance='no')((a(i,j),j=1,5),i=1,4)
  do k=1,n-1
    do i=1,n
      if(i==k)cycle
      mult = a(i,k)/a(k,k)
      do j=1,n+1
!        if(j==5)print*,a(1,k)
!        print*,a(k,j)*(a(i,k)/real(a(k,k)))
        a(i,j) = a(i,j)-(a(k,j)*mult)
!        if(j==5)print*,a(i,j)
      end do
    end do
  end do
!  write(*,'(5(F15.5,2X),/)',advance='no')((a(i,j),j=1,5),i=1,4)
!  x(4)=nint(a(4,5)/a(4,4))
!  x(3)=nint((a(3,5)-(a(3,4)*x(4)))/a(3,3))
!  x(2)=nint((a(2,5)-((a(2,4)*x(4))+(a(2,3)*x(3))))/a(2,2))
!  x(1)=nint((a(1,5)-((a(1,4)*x(4))+(a(1,3)*x(3))+(a(1,2)*x(2))))/a(1,1))

  do i=n,1,-1
    s=a(i,n+1)
    do j=i+1,n
      s=s-a(i,j)*x(j)
    end do
    x(i)=s/a(i,i)
  end do
  print*,'Solutions are :'
  write(*,'(F4.1,2X)')(x(i),i=1,4)
end program
