program ass_03_04
  implicit none
  real,allocatable,dimension(:,:)::mat,adj
  real::det,detf
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
  call show(mat,n)
  call adjMat(mat,adj,n)
  write(*,*)"Adjoint Matrix is: "
  call show(adj,n)
  det=detf(mat,n)
  write(*,*)"Determinant is : ",det
  if(det==0)then
    write(*,*)"Inverse not exist: "
  else
    write(*,*)"Inverse Matrix is: "
    do i=1,n
      do j=1,n
        write(*,'(F5.2,2X)',advance="no")adj(i,j)/det
      end do
      write(*,*)
    end do
  end if
end program

subroutine adjMat(a,adj,n)
  implicit none
  real,dimension(n,n)::a,adj
  real,dimension(n-1,n-1)::sl
  integer::n,i,j
  do i=1,n
    do j=1,n
      call slicef(a,n,i,j,sl)
      adj(i,j)=(sl(1,1)*sl(2,2))-(sl(1,2)*sl(2,1))
      if(((-1)**(i+j))==-1)adj(i,j)=adj(i,j)*(-1)
    end do
  end do
  adj=transpose(adj)
end subroutine

subroutine slicef(a,n,row,col,sl)
  implicit none
  real::a(n,n),sl(n-1,n-1)
  logical::mask(n,n)
  integer::n,row,col,i,j,k,l
  mask=.true.
  mask(row,:)=.false.
  mask(:,col)=.false.
  sl=reshape(pack(a,mask),(/2,2/))
end subroutine

real function detf(a,n)
  implicit none
  real::a(n,n)
  integer::n,i,j,k,l
  real::mult
  k=1
  do i=1,n-1
    do l=i+1,n
      mult=a(l,k)/a(i,k)
      if(a(l,k)==0)cycle
      do j=1,n
        a(l,j)=a(l,j)-(a(i,j)*mult)
      end do
    end do
    k=k+1
  end do
  detf=1
  do i=1,n
    detf=detf*a(i,i)
  end do
end function

subroutine show(mat,n)
  implicit none
  real,dimension(n,n)::mat
  integer::n,i,j
  do i=1,n
    do j=1,n
      write(*,'(F8.3,T3)',advance="no")mat(i,j)
    end do
    write(*,*)
  end do
end subroutine


