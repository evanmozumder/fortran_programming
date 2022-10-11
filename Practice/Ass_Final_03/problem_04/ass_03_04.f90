program ass_03_04
  implicit none
  real,allocatable::mat(:,:),sl(:,:)
  real::det,detf
  integer::n,i,j
  write(*,*)"Enter the size of the matrix: "
  read(*,*)n
  allocate(mat(n,n),sl(n-1,n-1))
  open(1,file="input_03_04.txt")
  read(1,*)mat
  mat=transpose(mat)
  write(*,*)"Given Matrix: "
  call display(mat,n)
!  write(*,*)"Sliced Matrix: "
!  call slicef(sl,mat,n,1,1)
!  call display(sl,n-1)
  print*,detf(mat,n)
end program

subroutine display(mat,n)
  implicit none
  real::mat(n,n)
  integer::n,i,j
  do i=1,n
    do j=1,n
      write(*,"(F10.2,2X)",advance="no")mat(i,j)
    end do
    write(*,*)
  end do
end subroutine

recursive real function detf(mat,n) result(det)
  implicit none
  real::mat(n,n),sl(n-1,n-1)
  integer::n,i
  det=0
  if(n==1)then
    det=mat(1,1)
    return
  else
    do i=1,n
      call slicef(sl,mat,n,1,i)
      det=det+((-1.0)**(1+i))*mat(1,i)*detf(sl,n-1)
    end do
    return
  end if
end function

subroutine slicef(sl,mat,n,row,col)
  implicit none
  real::mat(n,n),sl(n-1,n-1)
  integer::n,row,col
  logical::mask(n,n)
  mask=.true.
  mask(row,:)=.false.
  mask(:,col)=.false.
  sl=reshape(pack(mat,mask),(/n-1,n-1/))
end subroutine
