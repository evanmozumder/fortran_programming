program mat_slice
  implicit none
  real::mat(3,3),sl(2,2)
  logical::mask(3,3)
  integer::i=3,j=3,k,l
  mat=reshape((/1,2,3,4,5,6,7,8,9/),(/3,3/))
  mask=.true.
  mask(i,:)=.false.
  mask(:,j)=.false.
!  slice=reshape(pack(mat,mask),(/2,2/))
  do i=1,3
    do j=1,3
      write(*,'(F5.2,2X)',advance="no")mat(i,j)
    end do
    write(*,*)
  end do
  write(*,*)
  do i=1,3
    do j=1,3
      call cofactor(mat,3,i,j,sl)
      do k=1,2
        do l=1,2
          write(*,'(F5.2,2X)',advance="no")sl(k,l)
        end do
        write(*,*)
      end do
      write(*,'(/)')
    end do
  end do

end program

subroutine cofactor(mat,n,row,col,sl)
  implicit none
  real::mat(n,n),sl(n-1,n-1)
  integer::n,row,col
  logical::mask(3,3)
  mask=.true.
  mask(row,:)=.false.
  mask(:,col)=.false.
  sl=reshape(pack(mat,mask),(/2,2/))
end subroutine
