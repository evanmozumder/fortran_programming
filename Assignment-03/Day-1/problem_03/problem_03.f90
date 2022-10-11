program problem_03
  implicit none
  integer:: n,i,j
  integer, allocatable:: mMat(:,:),nMat(:,:),mT(:,:),nT(:,:),mnT(:,:),mn(:,:),nTmT(:,:),mnCheck(:,:)
  logical:: check=.true.
  open(1,file='input1.txt')
  open(2,file='output.txt')
  write(*,*)'enter the value of n >= 3:'
  read(*,*)n
  allocate(mMat(n,n))
  allocate(nMat(n,n))
  allocate(mT(n,n))
  allocate(nT(n,n))
  allocate(mnT(n,n))
  allocate(nTmT(n,n))
  allocate(mnCheck(n,n))


  ! reading mMat
  do i=1,n
    do j=1,n
      read(1,*)mMat(i,j)
    end do
  end do
  ! reading nMat
  do i=1,n
    do j=1,n
      read(1,*)nMat(i,j)
    end do
  end do

  ! multiplication of m and n matrix
  mn = matmul(mMat, nMat)
  call mat_multi(n,mMat,nMat,mnCheck)

  ! transpose
  call find_transpose(n,mMat,mT)
  call find_transpose(n,nMat,nT)
  call find_transpose(n,mn,mnT)

  ! multiplication of n-transpose and m-transpose
  nTmT = matmul(nT,mT)

  ! check whether the given equation verified or not
  do i=1,n
    do j=1,n
      if(mnT(i,j)/=nTmT(i,j))then
        check = .false.
        exit
      end if
    end do
  end do
  if(check)then
    write(2,*)'Equation Verified'
  else
    write(2,*)'Equation not verified'
  end if

  write(2,'(A,/)')'Main Matrix'
  do i=1,n
    do j=1,n
      write(2,'(I0,2X)',advance='no')nMat(i,j)
    end do
    write(2,*)
  end do
  write(2,*)

  write(2,'(A,/)')'Changed Matrix'
  do i=1,n
    do j=1,n
      write(2,'(I0,2X)',advance='no')mnCheck(i,j)
    end do
    write(2,*)
  end do

end program

subroutine find_transpose(n,m,mT)
  implicit none
  integer, intent(in):: n
  integer, intent(in):: m(n,n)
  integer, intent(out):: mT(n,n)
  integer:: i,j
  do i=1,n
    do j=1,n
      mT(i,j) = m(j,i)
    end do
  end do
end subroutine

subroutine mat_multi(n,matA,matB,ab)
  implicit none
  integer, intent(in):: n
  integer, intent(in):: matA(n,n), matB(n,n)
  integer, intent(out):: ab(n,n)
  integer:: i,j,k,l(2)
  do i=1,n
    do j=1,n
      ab(i,j)=0
      do k=1,n
        ab(i,j)=ab(i,j)+(matA(i,k)*matB(k,j))
      end do
    end do
  end do
end subroutine
