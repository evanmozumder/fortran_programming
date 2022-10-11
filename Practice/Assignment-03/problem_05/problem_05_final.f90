program problem_05_final
  implicit none
  real,allocatable,dimension(:,:)::mat,upt
  real::det,mult,dett,sol(4)
  integer::i,j,n,k,l
  n=4
  allocate(mat(n,n+1))
  allocate(upt(n,n+1))
  open(1,file="mat.txt")
  do i=1,n
    do j=1,n+1
      read(1,*)mat(i,j)
    end do
  end do
  do i=1,n
    do j=1,n+1
      write(*,'(F4.1,2X)',advance="no")mat(i,j)
    end do
    write(*,*)
  end do
  write(*,*)
!  write(*,*)det(mat,n)




!  k=1
!  l=2
!  do i=1,n-1
!    do l=i+1,n
!      mult=mat(l,k)/mat(i,k)
!      do j=1,n
!        mat(l,j)=mat(l,j)-(mat(i,j)*mult)
!      end do
!    end do
!    k=k+1
!  end do
!
!  dett=1
!  do i=1,n
!    dett=dett*mat(i,i)
!  end do
!  print*,dett

  upt=mat
  call upper_trian(mat,n,upt)
  write(*,*)"Upper triangular matrix: "
  do i=1,n
    do j=1,n+1
      write(*,'(F8.3,2X)',advance="no")upt(i,j)
    end do
    write(*,*)
  end do
!  write(*,*)det(mat,n)
  sol(4)=upt(4,5)/upt(4,4)
  sol(3)=(upt(3,5)-(sol(4)*upt(3,4)))/upt(3,3)
  sol(2)=(upt(2,5)-(sol(3)*upt(2,3))-(sol(4)*upt(2,4)))/upt(2,2)
  sol(1)=(upt(1,5)-(sol(2)*upt(1,2))-(sol(3)*upt(1,3))-(sol(4)*upt(1,4)))/upt(1,1)
  do i=1,4
    write(*,*)sol(i)
  end do
!  do i=n-1,1,-1
!    do j=n-1,1,-1
!
!    end do
!  end do
end program

subroutine upper_trian(a,n,upt)
  implicit none
  real,dimension(n,n+1)::upt,a
  integer,intent(in)::n
  integer::k,i,j,l
  real::mult
  k=1
  do i=1,n-1
    do l=i+1,n
      if(a(l,k)==0)cycle
      mult=upt(l,k)/upt(i,k)
!      write(*,'(I0,2X,I0,3X,F10.5)',advance="no")l,k,mult
!      write(*,*)
      do j=1,n+1
        upt(l,j)=upt(l,j)-(upt(i,j)*mult)
!        print*,upt(l,j)
      end do
    end do
!    if(l==n)exit
    k=k+1
!    l=l+1
  end do
!  det=1
!  do i=1,n
!    det=det*upt(i,i)
!  end do
end subroutine

subroutine lower_trian(upt,n,lwt)
  implicit none
  real,dimension(n,n+1)::upt,lwt
  integer::n,i,j,k,l
  real::mult

end subroutine
