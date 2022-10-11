program complex_read
  implicit none
  complex,dimension(2,2)::comp
  integer::i,j
  open(1,file="comp.txt")
  do i=1,2
    do j=1,2
      read(1,*)comp(i,j)
    end do
  end do
  do i=1,2
    do j=1,2
      write(*,*)comp(i,j)
    end do
  end do
end program
