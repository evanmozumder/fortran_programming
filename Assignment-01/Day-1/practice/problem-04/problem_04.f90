program problem_04
  complex,dimension(3,3)::mat,conjugate,prod_mat,selfadj
  complex:: summ, prod, temp
  real, dimension(3,3):: mod_mat, argu_mat
  integer:: i,j,check
  open(unit=1,file='InputFile.txt',status='old')
  do i=1,3
    do j=1,3
      read(1,*)mat(i,j)
    end do
  end do

  ! printing main matrix
  print*,'Main matrix: '
  do i=1,3
    do j=1,3
      write(*,10,advance='no')mat(i,j)
    end do
    write(*,*)
  end do
  10 format(F7.2,"+(",F6.2,")i",2x)

  ! (i)
  do i=1,3
    do j=1,3
      mod_mat(i,j)=cabs(mat(i,j))
      argu_mat(i,j)=argument(mat(i,j))
    end do
  end do
  print*, 'Modulus'
  do i=1,3
    do j=1,3
      write(*,'(F5.2,2X)',advance='no')mod_mat(i,j)
    end do
    write(*,*)
  end do
  print*, 'Argument'
  do i=1,3
    do j=1,3
      write(*,'(F7.2,2X)',advance='no')argu_mat(i,j)
    end do
    write(*,*)
  end do

  ! (ii)
  summ = (0,0)
  prod = (1,1)
  do i=1,3
    j=4-i
    summ = summ + mat(i,j)
    prod = prod * mat(i,j)
  end do
  write(*,11)summ,prod
  11 format('Sum of the minor diagonal: ',F6.2,"+(",F6.2,")i",/,&
  'product of the minor diagonal: ',F6.2,"+(",F6.2,")i")

  ! (iii)
  conjugate = conjg(mat)
  do i=1,3
    do j=1,3
      temp=(0,0)
      do k=1,3
        temp = temp + mat(i,k)*conjugate(k,j)
      end do
      prod_mat(i,j) = temp
    end do
  end do

  print*, 'product of M and complex conjugate: '
  do i=1,3
    do j=1,3
      write(*,10,advance='no')prod_mat(i,j)
    end do
    write(*,*)
  end do

  ! (iv)
  do i=1,3
    do j=1,3
      selfadj(j,i) = conjugate(i,j)
    end do
  end do
  print*,'self adjoint matrix: '
  check=0
  do i=1,3
    do j=1,3
      if(mat(i,j)/=selfadj(i,j))then
        check=1
      end if
      write(*,10,advance='no')selfadj(i,j)
    end do
    write(*,*)
  end do
  if(check/=0)then
    print*, 'matrix M is not a Hermitian matrix'
  else
    print*, 'matrix M is a Hermitian matrix'
  end if
end program

real function argument(z)
implicit none
complex, intent(in):: z
real:: theta
real, parameter:: pi=3.1459
theta=atan(imag(z)/real(z))
argument=(theta*180)/pi
end function
