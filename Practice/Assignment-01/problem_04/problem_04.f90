program problem_04
  complex,dimension(3,3)::mat,conjugate,prod_mat,conjugate_transpose
  integer::i,j,k,check=0
  complex::z,s=(0,0),prod=(1,1),temp
  open(1,file="input.txt")
  do i=1,3
    do j=1,3
      read(1,*)mat(i,j)
    end do
  end do
  write(*,'(A)')"Given Matrix: "
  do i=1,3
    do j=1,3
      write(*,'(F6.2," + i(",F5.2,")"),3X',advance="no")mat(i,j)
    end do
    write(*,*)
  end do

  ! (i)
  write(*,'(/,A)')"Modulus of every element: "
  do i=1,3
    do j=1,3
      write(*,'(F6.2,2X)',advance="no")cabs(mat(i,j))
    end do
    write(*,*)
  end do
  write(*,'(/,A)')"Argument of every element: "
  do i=1,3
    do j=1,3
      write(*,'(F6.2,2X)',advance="no")argument(mat(i,j))
    end do
    write(*,*)
  end do

  ! (ii)
  j=3
  do i=1,3
    s=s+mat(i,j)
    prod=prod*mat(i,j)
    j=j-1
  end do
  write(*,'(/,A,F6.2," + i(",F6.2,")")')"Sum of the elements of the minor diagonal: ",s
  write(*,'(A,F6.2," + i(",F6.2,")")')"Product of the elements of the minor diagonal: ",prod

  ! (iii)
  write(*,'(/,A)')"product of M and complex conjugate of M: "
  conjugate=conjg(mat)
  do i=1,3
    do j=1,3
      temp=(0,0)
      do k=1,3
        temp=temp+(mat(i,k)*conjugate(k,j))
      end do
      prod_mat(i,j)=temp
      write(*,'(F6.2," + i(",F6.2,")",3X)',advance="no")prod_mat(i,j)
    end do
    write(*,*)
  end do

  ! (iv)
  write(*,'(/,A)')"Conjugate Transpose of M"
  do i=1,3
    do j=1,3
      conjugate_transpose(j,i)=conjugate(i,j)
      write(*,'(F6.2," + i(",F6.2,")",3X)',advance="no")conjugate_transpose(j,i)
      if(conjugate_transpose(i,j)/=mat(i,j) .and. check/=1)then
        check=1
      end if
    end do
    write(*,*)
  end do
!  do i=1,3
!    do j=1,3
!      if(conjugate_transpose(i,j)/=mat(i,j))then
!        check=1
!        exit
!      end if
!    end do
!  end do
  if(check==0)then
    write(*,'(/,A)')"M is a Hermitian Matrix"
  else
    write(*,'(/,A)')"M is not a Hermitian Matrix"
  end if
end program

real function argument(z)
implicit none
real::theta
complex::z
theta=atan(imag(z)/real(z))
argument=theta
end function
