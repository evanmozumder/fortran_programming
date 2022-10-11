program determinant
  implicit none
  real,allocatable,dimension(:,:)::mat
  real::det,mult,dett
  integer::i,j,n,k,l
  read(*,*)n
  allocate(mat(n,n))
  open(1,file="mat.txt")
  do i=1,n
    do j=1,n
      read(1,*)mat(i,j)
    end do
  end do
  do i=1,n
    do j=1,n
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



  write(*,*)"Upper triangular matrix: "
  do i=1,n
    do j=1,n
      write(*,'(F8.2,2X)',advance="no")mat(i,j)
    end do
    write(*,*)
  end do
  write(*,*)det(mat,n)
end program

real function det(a,n)
  implicit none
  real,dimension(n,n)::a
  integer,intent(in)::n
  integer::k,i,j,l
  real::mult
  k=1
  l=2
  do i=1,n-1
    do l=i+1,n
      mult=a(l,k)/a(i,k)
      if(a(i,k)==0)mult=0
      do j=1,n
        a(l,j)=a(l,j)-(a(i,j)*mult)
!        print*,mat(l,j)
      end do
    end do
!    if(l==n)exit
    k=k+1
!    l=l+1
  end do
  det=1
  do i=1,n
    det=det*a(i,i)
  end do
end function
