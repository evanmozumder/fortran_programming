program problem_01
	implicit none
	integer:: n,i,j
	real:: num
	logical:: ans=.true.
	real,allocatable::mat(:,:),matT(:,:)
	open(1,file='input_mat.txt')
	write(*,*)'Enter a positive integer n >= 2'
	read(*,*)n
  allocate(mat(n,n))
  allocate(matT(n,n))

  ! taking matrix element
  do i=1,n
    do j=1,n
      read(1,*)mat(i,j)
      matT(j,i)=mat(i,j)
    end do
  end do

  ! printing main matrix
  print*,'Main Matrix: '
  do i=1,n
    do j=1,n
      write(*,'(F10.5,2X)',advance='no')mat(i,j)
    end do
    write(*,*)
  end do
  write(*,*)

  ! printing transpose matrix
  print*,'Transopose Matrix: '
  do i=1,n
    do j=1,n
      write(*,'(F10.5,2X)',advance='no')matT(i,j)
    end do
    write(*,*)
  end do

  ! checking whether the matrix is Symmetric matrix or not
  do i=1,n
    do j=1,n
      if(mat(i,j)/=matT(i,j))then
        ans = .false.
        exit
      end if
    end do
  end do
  if(ans)then
    print*,'Symmetric matrix'
  else
    print*,'Not a symmetric matrix'
  end if
end program
