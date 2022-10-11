program problem_02
  implicit none
  integer,dimension(4,4)::matA,matB,matC
  integer::i,j,dt
  call find_mat(matA,matB,matC)
  write(*,*)"First Matrix: "
  write(*,'(4(4(I3,3X),/))')((matA(i,j),j=1,4),i=1,4)
  write(*,*)"Second Matrix: "
  write(*,'(4(4(I3,3X),/))')((matB(i,j),j=1,4),i=1,4)
  write(*,*)"Third Matrix: "
  write(*,'(4(4(I3,3X),/))')((matC(i,j),j=1,4),i=1,4)
end program

subroutine find_mat(matA,matB,matC)
  implicit none
  integer::i,j,dt
  integer,dimension(4,4)::matA,matB,matC
  do i=1,4
    do j=1,4
      matA(i,j)=i+j
      matB(i,j)=i**(j-1)
      matC(i,j)=0
      if(abs(i-j)>1)matC(i,j)=1
      if(abs(i-j)<1)matC(i,j)=-1
    end do
  end do
end subroutine
