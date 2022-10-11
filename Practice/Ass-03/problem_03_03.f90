program problem_03_03
  implicit none
  integer::n,i,j
  integer,allocatable,dimension(:,:)::matM,matN,matMN,mT,nT,mTnT
  read(*,*)n
  allocate(matM(n,n))
  allocate(matN(n,n))
  allocate(matMN(n,n))
  allocate(mT(n,n))
  allocate(nT(n,n))
  allocate(mTnT(n,n))
  open(1,file="input.txt")
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
   call show(matM,n)
   call show(matN,n)
   call prod(matM,matN,n,matMN)
   call show(matMN,n)
   mT=transpose(matM)
   nT=transpose(matN)
   call prod(mT,nT,n,mTnT)
   call show(mTnT,n)
end program

subroutine prod(matM,matN,n,matMN)
  implicit none
  integer::n,i,j,k
  integer,dimension(n,n)::matM,matN,matMN
  do i=1,n
    do j=1,n
      matMN(i,j)=0
      do k=1,n
        matMN(i,j)=matMN(i,j)+(matM(i,k)*matN(k,j))
      end do
    end do
  end do
end subroutine

subroutine show(mat,n)
  implicit none
  integer::i,j,n
  integer::mat(n,n)
  do i=1,n
    do j=1,n
      write(*,'(I0,2X)',advance="no")mat(i,j)
    end do
    write(*,*)
  end do
  write(*,*)
end subroutine
