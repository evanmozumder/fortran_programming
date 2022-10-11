program ass_03_01
  implicit none
  integer::n,i,j
  real,allocatable::mat(:,:)
  logical::sym=.true.
  write(*,*)"Enter size of the matrix: "
  read(*,*)n
  allocate(mat(n,n))
  open(1,file="input_03_01.txt")
  do i=1,n
    do j=1,n
      read(1,*)mat(i,j)
    end do
  end do
  write(*,*)"Given Matrix: "
  do i=1,n
    do j=1,n
      write(*,"(F10.5,3X)",advance="no")mat(i,j)
      if(mat(i,j)/=mat(j,i) .and. sym)sym=.false.
    end do
    write(*,*)
  end do
  if(.not.sym)then
    write(*,*)"Not Symmetric"
  else
    write(*,*)"Symmetric"
  end if
end program
