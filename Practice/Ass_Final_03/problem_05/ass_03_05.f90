program ass_03_05
  implicit none
  integer::n,i,j
  real,allocatable::augmat(:,:),gauss(:,:),root(:)
  write(*,*)"Enter the number of unknown: "
  read(*,*)n
  allocate(augmat(n,n+1),gauss(n,n+1),root(n))
  open(1,file="input_03_05.txt")
  do i=1,n
    do j=1,n+1
      read(1,*)augmat(i,j)
    end do
  end do
  write(*,*)"Given Matrix: "
  call display(augmat,n)
  call gauss_eli(augmat,n,gauss)
  write(*,*)"Eliminted Matrix: "
  call display(gauss,n)
  call sol(gauss,n,root)
  write(*,*)"Solution is: "
  write(*,"(F10.2,3X)",advance="no")(root(i),i=1,n)
end program

subroutine sol(mat,n,root)
  implicit none
  real::mat(n,n),root(n)
  integer::n,i,j
  root(n)=mat(n,n+1)/mat(n,n)
  do i=n-1,1,-1
    root(i)=mat(i,n+1)
    do j=n,i+1,-1
      root(i)=root(i)-(mat(i,j)*root(j))
    end do
    root(i)=root(i)/mat(i,i)
  end do
end subroutine

subroutine gauss_eli(mat,n,gauss)
  implicit none
  real::mat(n,n+1),gauss(n,n+1),mult
  integer::i,j,n,k
  do i=1,n !row
    do j=i+1,n !row
      mult=mat(j,i)/mat(i,i)
      do k=1,n+1 !traverse through current row
        mat(j,k)=mat(j,k)-(mult*mat(i,k))
      end do
    end do
  end do
  do i=1,n
    do j=1,n+1
      gauss(i,j)=mat(i,j)
    end do
  end do
end subroutine

subroutine display(mat,n)
  implicit none
  real::mat(n,n+1)
  integer::n,i,j
  do i=1,n
    do j=1,n+1
      write(*,"(F10.2,2X)",advance="no")mat(i,j)
    end do
    write(*,*)
  end do
end subroutine