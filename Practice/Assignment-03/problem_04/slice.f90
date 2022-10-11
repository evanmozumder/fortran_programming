program sliced
  implicit none
  real,dimension(3,3)::mat
  real,dimension(2,2)::slice
  integer,parameter::n=3
  logical,dimension(3,3)::mask
  real,dimension(n-1,n-1)::sl
  integer::i=3,j=3,k,l
  mat=reshape((/1,2,3,4,5,6,7,8,9/),(/3,3/))
  slice=reshape(pack(mat,mask),(/2,2/))
  print*,"The matrix is: "
  do i=1,3
    do j=1,3
      write(*,'(F8.3,T3)',advance="no")mat(i,j)
    end do
    write(*,*)
  end do
  print*,"The Sliced Matrix is: "
  do i=1,3
    do j=1,3
      call slicef(sl,mat,n,i,j)
      do k=1,2
        do l=1,2
          write(*,'(F8.3,T3)',advance="no")sl(k,l)
        end do
        write(*,*)
      end do
      write(*,*)
!      write(*,'(F8.3,T3)',advance="no")sl
    end do
    write(*,*)
  end do
end program

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

