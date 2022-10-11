program determinant
  implicit none
  integer,parameter::n=5
  integer::i,j
  real*8::mat(n,n),detf,det
  !mat=reshape((/1,0,0,2,-4,3,3,1,-1/),(/3,3/))
  mat=reshape((/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25/),(/5,5/))
  det=detf(mat,n)
  print*,"The matrix is: "
  do i=1,n
    do j=1,n
      write(*,"(F8.3,3X)",advance='no')mat(i,j)
    end do
    write(*,*)
  end do
  write(*,*)"The determinant is:",det
end program

recursive real*8 function detf(mat,n) result(det)
  implicit none
  integer::n,i
  real*8::mat(n,n),sl(n-1,n-1)
  det=0
  if(n==2)then
    det=mat(1,1)*mat(2,2)-mat(1,2)*mat(2,1)
    return
  else if(n==1)then
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
  integer::n,row,col
  real*8::mat(n,n),sl(n-1,n-1)
  logical::mask(n,n)
  mask=.true.
  mask(row,:)=.false.
  mask(:,col)=.false.
  sl=reshape(pack(mat,mask),(/n-1,n-1/))
end subroutine
