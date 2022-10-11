program problem_01_05
  implicit none
  real,dimension(36)::mat,sorted
  real,dimension(6,6)::sq_mat
  integer::i,n,j
  do i=1,36
    n=ran(n)
    mat(i)=100.0+100.0*ran(n)
  end do
  sq_mat=reshape(mat,(/6,6/))
  write(*,*)"F "
  do i=1,6
    do j=1,6
      write(*,'(F15.8,2X)',advance="no")sq_mat(i,j)
    end do
    write(*,*)
  end do
  write(*,*)"E "
  do i=1,6
    do j=1,6
      write(*,'(E15.7,2X)',advance="no")sq_mat(i,j)
    end do
    write(*,*)
  end do
  write(*,*)"Es "
  do i=1,6
    do j=1,6
      write(*,'(ES15.7,2X)',advance="no")sq_mat(i,j)
    end do
    write(*,*)
  end do
  call bubble_sort(mat,sorted,36)
  write(*,*)"Sorted Array "
  write(*,'(F15.8)')(sorted(i),i=1,36)
end program

subroutine bubble_sort(a,sorted,n)
  real,dimension(n)::a,sorted
  integer,intent(in)::n
  integer::i,k,j
  real::temp
  k=n-1
  do while(k/=0)
    do i=1,k
      if(a(i)>a(i+1))then
        temp=a(i)
        a(i)=a(i+1)
        a(i+1)=temp
      end if
    end do
    k=k-1
  end do
  sorted=a
end subroutine
