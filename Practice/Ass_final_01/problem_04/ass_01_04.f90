program ass_01_04
  implicit none
  complex,allocatable::mat(:,:),conjugate(:,:),mult(:,:)
  integer::n,i,j
  complex::summ=(0.0,0.0)
  write(*,*)"Enter n: "
  read(*,*)n
  allocate(mat(n,n),conjugate(n,n),mult(n,n))
  open(1,file="input_01_04.txt")
  do i=1,n
    do j=1,n
      read(1,*)mat(i,j)
    end do
  end do
  call display(mat,n)
  do i=1,n
    do j=1,n
      write(*,"(F10.2,2X)",advance="no")sqrt((real(mat(i,j)))**2+(aimag(mat(i,j)))**2)
    end do
    write(*,*)
  end do
  write(*,*)
  do i=1,n
    do j=1,n
      write(*,"(F10.4,2X)",advance="no")cabs(mat(i,j))
    end do
    write(*,*)
  end do
  j=n
  do i=1,n
    summ=summ+mat(i,j)
    j=j-1
  end do
  print*,summ
  do i=1,n
    do j=1,n
      conjugate(i,j)=conjg(mat(i,j))
    end do
  end do
  call display(conjugate,n)
  mult=(0.0,0.0)
  call mat_mult(mat,conjugate,mult,n)
  write(*,*)
  call display(mult,n)
end program

subroutine mat_mult(mat,conjugate,mult,n)
  implicit none
  complex::mat(n,n),conjugate(n,n),mult(n,n)
  integer::n,i,j,k
  do i=1,n
    do j=1,n
      do k=1,n
        mult(i,j)=mult(i,j)+mat(i,k)*conjugate(k,j)
      end do
    end do
  end do
end subroutine

subroutine display(mat,n)
  implicit none
  complex::mat(n,n)
  integer::n,i,j
  do i=1,n
    do j=1,n
      write(*,"(F7.2,'+i(',F7.2,')',4x)",advance="no")mat(i,j)
    end do
    write(*,*)
  end do
end subroutine
