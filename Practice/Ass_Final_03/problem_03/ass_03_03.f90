program ass_03_03
  implicit none
  integer::n,i,j,k
  logical::check=.true.
  real,allocatable,dimension(:,:)::matM,matN,matMN,matMNT,matNT,matMT,matNTMT
  write(*,*)"Enter size of the matrix: "
  read(*,*)n
  allocate(matM(n,n),matN(n,n),matMN(n,n),matMNT(n,n),matMT(n,n),matNT(n,n),matNTMT(n,n))
  open(1,file="input_03_03.txt")
  do i=1,n
    do j=1,n
      read(1,*)matM(i,j)
    end do
  end do
  do i=1,n
    do j=1,n
      read(1,*)matN(i,j)
    end do
  end do
  write(*,"(A,///)")"matrix M: "
  call display(matM,n)
  write(*,"(A,///)")"matrix N: "
  call display(matN,n)
  write(*,"(A,//)")"matrix MN: "
  call mat_mult(n,matM,matN,matMN)
  call display(matMN,n)
  write(*,"(A,//)")"M*N Transpose: "
  call mat_transpose(n,matMN,matMNT)
  call display(matMNT,n)
  write(*,"(A,//)")"M transpose: "
  call mat_transpose(n,matM,matMT)
  call display(matMT,n)
  write(*,"(A,//)")"N transpose: "
  call mat_transpose(n,matN,matNT)
  call display(matNT,n)
  write(*,"(A,//)")"NT*MT: "
  call mat_mult(n,matNT,matMT,matNTMT)
  call display(matNTMT,n)
  do i=1,n
    do j=1,n
      if(matMNT(i,j)/=matNTMT(i,j))then
        check=.false.
        exit
      end if
    end do
  end do
  if(check)then
    write(*,*)"Verified"
  else
    write(*,*)"Not Verified"
  end if
end program

subroutine display(mat,n)
  implicit none
  real::mat(n,n)
  integer::i,j,n
  do i=1,n
    do j=1,n
      write(*,"(F15.7,3X)",advance="no")mat(i,j)
    end do
    write(*,*)
  end do
end subroutine

subroutine mat_mult(n,matA,matB,matAB)
  implicit none
  real::matA(n,n),matB(n,n),matAB(n,n),temp
  integer::i,j,n,k
  do i=1,n
    do j=1,n
      matAB(i,j)=0
      do k=1,n
        matAB(i,j)=matAB(i,j)+(matA(i,k)*matB(k,j))
      end do
    end do
  end do
end subroutine

subroutine mat_transpose(n,mat,matT)
  implicit none
  integer::n,i,j
  real::mat(n,n),matT(n,n)
  do i=1,n
    do j=1,n
      matT(i,j)=mat(j,i)
    end do
  end do
end subroutine
