program problem_01
  implicit none
  integer::n,i,j
  real,allocatable,dimension(:,:)::mat
  logical::is_sym=.true.
  read(*,*)n
  allocate(mat(n,n))
  open(1,file="input_mat.txt")
  ! reading and printing main matrix
  write(*,*)"printing main matrix: "
  do i=1,n
    do j=1,n
      read(1,*)mat(i,j)
      write(*,'(F5.2,2X)',advance="no")mat(i,j)
    end do
    write(*,*)
  end do
  write(*,'(/,A)')"Transpose Matrix: "
  do i=1,n
    do j=1,n
      write(*,'(F5.2,2X)',advance="no")mat(j,i)
      if(mat(i,j)/=mat(j,i) .and. is_sym)is_sym=.false.
    end do
    write(*,*)
  end do
  if(is_sym)then
    write(*,'(/,A)')"Matrix is Symmetric"
  else
    write(*,'(/,A)')"Matrix isn't symmetric"
  end if
end program


