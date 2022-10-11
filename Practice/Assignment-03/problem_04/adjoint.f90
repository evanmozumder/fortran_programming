program adjoint
  implicit none
  real,allocatable,dimension(:,:)::mat,adj
  real::det
  integer::n,i,j
  open(1,file="mat.txt")
  write(*,*)"Enter the dimension of square matrix: "
  read(*,*)n
  allocate(mat(n,n))
  allocate(adj(n,n))
  do i=1,n
    do j=1,n
      read(1,*)mat(i,j)
    end do
  end do
  write(*,*)"Given matrix: "
  do i=1,n
    do j=1,n
      write(*,'(F5.2,2X)',advance="no")mat(i,j)
    end do
    write(*,*)
  end do
  write(*,*)
  call adjMat(mat,adj,n)
  do i=1,n
    do j=1,n
      write(*,'(F5.2,2X)',advance="no")adj(i,j)
    end do
    write(*,*)
  end do
end program

subroutine adjMat(a,adj,n)
  implicit none
  real,dimension(n,n)::a
  real,intent(out),dimension(n,n)::adj
  real,dimension(n,n)::cofactor
  integer,intent(in)::n
  real,dimension(n-1,n-1)::sl
  integer::i,j
  real::man
  do i=1,n
    do j=1,n
      call slicef(sl,a,n,i,j)
      cofactor(i,j)=(sl(1,1)*sl(2,2))-(sl(2,1)*sl(1,2))
      if(((-1)**(i+j))==-1)cofactor(i,j)=cofactor(i,j)*(-1)
    end do
  end do
!  cofactor(1,1) = +(a(2,2)*a(3,3)-a(2,3)*a(3,2))
!  cofactor(1,2) = -(a(2,1)*a(3,3)-a(2,3)*a(3,1))
!  cofactor(1,3) = +(a(2,1)*a(3,2)-a(2,2)*a(3,1))
!  cofactor(2,1) = -(a(1,2)*a(3,3)-a(1,3)*a(3,2))
!  cofactor(2,2) = +(a(1,1)*a(3,3)-a(1,3)*a(3,1))
!  cofactor(2,3) = -(a(1,1)*a(3,2)-a(1,2)*a(3,1))
!  cofactor(3,1) = +(a(1,2)*a(2,3)-a(1,3)*a(2,2))
!  cofactor(3,2) = -(a(1,1)*a(2,3)-a(1,3)*a(2,1))
!  cofactor(3,3) = +(a(1,1)*a(2,2)-a(1,2)*a(2,1))
  adj=transpose(cofactor)
end subroutine

subroutine slicef(sl,mat,n,row,column)
  implicit none
  integer,intent(in)::n,row,column
  real,dimension(n,n),intent(in)::mat
  real,dimension(n-1,n-1),intent(out)::sl
  logical,dimension(n,n)::mask
  mask=.true.
  mask(row,:)=.false.
  mask(:,column)=.false.
  sl=reshape(pack(mat,mask),(/n-1,n-1/))
end subroutine

