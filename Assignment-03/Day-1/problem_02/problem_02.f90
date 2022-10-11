program problem_02
  implicit none
  integer::matA(4,4),matB(4,4),matC(4,4)
  integer:: i,j,v

  ! a,b,c
  call find_matrix(matA, matB, matC)

  write(*,'(A,/)')'Matrix A: '
  write(*,'(4(I0,2X),/)',advance='no')((matA(i,j),j=1,4),i=1,4)
  write(*,'(2/)')

  write(*,'(A,/)')'Matrix B: '
  write(*,'(4(I0,2X),/)',advance='no')((matB(i,j),j=1,4),i=1,4)
  write(*,'(2/)')

  write(*,'(A,/)')'Matrix C: '
  write(*,'(4(I0,2X),/)',advance='no')((matC(i,j),j=1,4),i=1,4)
end program

subroutine find_matrix(matA,matB,matC)
  implicit none
  integer, intent(out)::matA(4,4),matB(4,4),matC(4,4)
  integer:: i,j,v

  do i=1,4
    do j=1,4
      matA(i,j) = i+j
      matB(i,j) = i**(j-1)
      if(abs(i-j)>1)then
        v = 1
      else if(abs(i-j)<1)then
        v = -1
      else
        v = 0
      end if
      matC(i,j) = v
    end do
  end do
end subroutine
