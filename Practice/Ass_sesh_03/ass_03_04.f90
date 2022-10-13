program ass_03_04
  implicit none
  real,allocatable::mat(:,:),adj(:,:)
  real::det,detf
  integer::n,i,j
  read(*,*)n
  open(1,file="input_03_04.txt")
  allocate(mat(n,n),adj(n,n))
  read(1,*)mat
  mat=transpose(reshape(mat,(/3,3/)))
  call show(mat,n)
  call adjmat(mat,n,adj)
  call show(adj,n)
  det=detf(mat,n)
  print*,det
  do i=1,n
    do j=1,n
      write(*,"(F10.2,3X)",advance="no")adj(i,j)/det
    end do
    write(*,*)
  end do
end program

subroutine adjmat(mat,n,adj)
  real::mat(n,n),sl(n-1,n-1),adj(n,n)
  integer::n,i,j
  do i=1,n
    do j=1,n
      call slicef(mat,sl,n,i,j)
      adj(i,j)=sl(1,1)*sl(2,2)-sl(1,2)*sl(2,1)
      if((-1)**(i+j)==-1)adj(i,j)=adj(i,j)*(-1)
    end do
  end do
  adj=transpose(adj)
end subroutine

recursive real function detf(mat,n) result(det)
  real::mat(n,n),sl(n-1,n-1)
  integer::n,i
  det=0
  if(n==1)then
    det=mat(1,1)
    return
  else
    do i=1,n
      call slicef(mat,sl,n,1,i)
      det=det+((-1.0)**(1+i))*mat(1,i)*detf(sl,n-1)
    end do
    return
  end if
end function

subroutine slicef(mat,sl,n,row,col)
  implicit none
  real::mat(n,n),sl(n-1,n-1)
  integer::n,row,col
  logical::mask(n,n)
  mask=.true.
  mask(row,:)=.false.
  mask(:,col)=.false.
  sl=reshape(pack(mat,mask),(/n-1,n-1/))
end subroutine

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
  write(*,*)
end subroutine
