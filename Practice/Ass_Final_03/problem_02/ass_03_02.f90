program ass_03_02
  implicit none
  integer::n,i,j
  real,allocatable::mat1(:,:),mat2(:,:),mat3(:,:)
  write(*,*)"Enter the size of the matrix: "
  read(*,*)n
  allocate(mat1(n,n),mat2(n,n),mat3(n,n))
  do i=1,n
    do j=1,n
      mat1(i,j)=i+j
      mat2(i,j)=i**(j-1)
      if(abs(i-j)>1)then
        mat3(i,j)=1
      else if(abs(i-j)<1)then
        mat3(i,j)=-1
      else
        mat3(i,j)=0
      end if
    end do
  end do
  write(*,"(A,//)")"First Matrix"
  call display(mat1,n)
  write(*,"(A,//)")"Second Matrix"
  call display(mat2,n)
  write(*,"(A,//)")"third Matrix"
  call display(mat3,n)
end program

subroutine display(mat,n)
  implicit none
  real::mat(n,n)
  integer::n,i,j
  do i=1,n
    do j=1,n
      write(*,"(F15.7,3x)",advance="no")mat(i,j)
    end do
    write(*,*)
  end do
end subroutine
