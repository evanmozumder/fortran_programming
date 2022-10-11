program problem_04
  complex,dimension(3,3)::mat,selfadj,conjugate
  real,dimension(3,3)::mod_mat,argu_mat,pro_mat
  complex::s,p,temp
  integer:: i,j,k,test
  open(unit=1,file='input_file.txt',status='old')
  open(2,file='output_file.txt')
  do i=1,3
    do j=1,3
      read(1,*)mat(i,j)
    end do
  end do

  ! printing main matrix
  write(*,*)'Main Matrix: '
  write(2,*)'Main Matrix: '
  do i=1,3
    do j=1,3
      write(*,10,advance='no')mat(i,j)
      write(2,10,advance='no')mat(i,j)
    end do
    write(*,*)
    write(2,*)
  end do
  10 format(F6.2,"+(",F5.2,")",2X)

  ! (i)
  do i=1,3
    do j=1,3
      mod_mat(i,j)=cabs(mat(i,j))
      argu_mat(i,j) = argument(mat(i,j))
    end do
  end do
  write(*,*)'Modulus: '
  write(2,*)'Modulus: '
  do i=1,3
    do j=1,3
      write(*,'(F6.2,2X)',advance='no')mod_mat(i,j)
      write(2,'(F6.2,2X)',advance='no')mod_mat(i,j)
    end do
    write(*,*)
    write(2,*)
  end do
  write(*,*)'Argument: '
  write(2,*)'Argument: '
  do i=1,3
    do j=1,3
      write(*,'(F7.2,X)',advance='no')argu_mat(i,j)
      write(2,'(F7.2,X)',advance='no')argu_mat(i,j)
    end do
    write(*,*)
    write(2,*)
  end do

  ! (ii)
  s=(0,0)
  p=(1,1)
  do i=1,3
    j=4-i
    s=s+mat(i,j)
    p=p*mat(i,j)
  end do
  write(*,11)s,p
  write(2,11)s,p
  11 format('sum of the elements of the minor diagonal: ',F6.2,"+(",F6.2,")i",/,&
  'product of the elements of the minor diagonal: ',F6.2,"+(",F6.2,")i")

  ! (iii)
  conjugate = conjg(mat)
  do i=1,3
    do j=1,3
      temp=(0,0)
      do k=1,3
        temp = temp + (mat(i,k)*conjugate(k,j))
      end do
      pro_mat(i,j)=temp
    end do
  end do

  do i=1,3
    do j=1,3
      write(*,'(F7.2,X)',advance='no')pro_mat(i,j)
      write(2,'(F7.2,X)',advance='no')pro_mat(i,j)
    end do
    write(*,*)
    write(2,*)
  end do

  ! (iv)
  do i=1,3
    do j=1,3
      selfadj(j,i)=conjugate(i,j)
    end do
  end do
  write(*,*)'Conjugate transpose of main matrix'
  write(2,*)'Conjugate transpose of main matrix'
  do i=1,3
    do j=1,3
      write(*,100,advance='no')selfadj(i,j)
      write(2,100,advance='no')selfadj(i,j)
    end do
    write(*,*)
    write(2,*)
  end do
  100 format(F6.2,"+(",F6.2,")i",1x)

  test=0
  do i=1,3
    do j=1,3
      if(mat(i,j)/=selfadj(i,j))then
        test=1
        exit
      end if
    end do
  end do
  if(test/=0)then
    write(*,'(/,A)')'Matrix M is not Hermitian matrix'
    write(2,'(/,A)')'Matrix M is not Hermitian matrix'
  else
    write(*,'(/,A)')'Matrix M is a Hermitian matrix'
    write(2,'(/,A)')'Matrix M is a Hermitian matrix'
  end if
end program

REAL FUNCTION argument(Z)
IMPLICIT NONE
REAL, PARAMETER::pi = 3.1459
REAL::theta
COMPLEX,INTENT(IN)::Z
theta = ATAN(IMAG(z)/REAL(z))
argument=theta
END FUNCTION
