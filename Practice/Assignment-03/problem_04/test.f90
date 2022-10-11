program test
  implicit none
  integer:: i,j
  complex,dimension(2,2)::mat
  open(1,file='com.txt')
  do i=1,2
    do j=1,2
      read(1,*)mat(i,j)
    end do
  end do
  do i=1,2
    do j=1,2
      write(*,*)mat(i,j)
    end do
  end do
end program
