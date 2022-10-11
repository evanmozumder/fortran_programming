program problem_03
  implicit none
  integer::n
  integer,allocatable,dimension(:,:)::matM,matN
  read(*,*)n
  allocate(matM(n))
  allocate(matN(n))
  open(1,file="input.txt")
  do i=1,n
    do j=1,n
      read(*,*)matM(i,j)
    end do
  end do
  do i=1,n
    do j=1,n
      read(*,*)matN(i,j)
    end do
  end do

end program
