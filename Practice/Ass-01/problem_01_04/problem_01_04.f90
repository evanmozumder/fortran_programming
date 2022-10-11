program problem_01_04
  implicit none
  complex,dimension(3,3)::mat,modumat,conjugate,prod_mat,trans
  complex::num
  complex::s,p
  integer::i,j,fm,k
  open(1,file="input.txt")
  do i=1,3
    do j=1,3
      read(1,'(F4.1,X,F4.1)')mat(i,j)
    end do
  end do
  write(*,*)"Given Matrix: "
  do i=1,3
    do j=1,3
      write(*,'(F4.1," + i(",F4.1,")",3X)',advance="no")mat(i,j)
    end do
    write(*,*)
  end do
  write(*,*)"Modulus of each element: "
!  cabs(mat)
  do i=1,3
    do j=1,3
      write(*,'(F10.3,3X)',advance="no")cabs(mat(i,j))
    end do
    write(*,*)
  end do
  s=(0,0)
  p=(1,1)
  j=3
  do i=1,3
    s=s+mat(i,j)
    p=p*mat(i,j)
    j=j-1
  end do
  write(*,*)"Sum is: ",s
  write(*,*)"Product is: ",p
  conjugate=conjg(mat)
  prod_mat=(0,0)
  p=(1,1)
  do i=1,3
    do j=1,3
!      prod_mat(i,j)=(0,0)
      do k=1,3
        prod_mat(i,j)=prod_mat(i,j)+mat(i,k)*conjugate(k,j)
      end do
    end do
  end do
  write(*,*)"product matrix is: "
  do i=1,3
    do j=1,3
      write(*,'(F14.5," + i(",F14.5,")",3X)',advance="no")prod_mat(i,j)
    end do
    write(*,*)
  end do
  write(*,*)"Transpose matrix is: "
  do i=1,3
    do j=1,3
      trans(i,j)=conjugate(j,i)
      write(*,'(F4.1," + i(",F4.1,")",3X)',advance="no")trans(i,j)
    end do
    write(*,*)
  end do
  do i=1,3
    do j=1,3
!      if(mat(i,j)/=trans(i,n))the
    end do
  end do
end program

