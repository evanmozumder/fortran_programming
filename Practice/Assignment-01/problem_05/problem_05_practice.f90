program problem_05
  implicit none
  real,dimension(6,6)::sqr_mat
  real,dimension(36)::mat
  integer::i,j,n,k=1
  open(1,file="ot_a1q3_F.txt")
  open(2,file="ot_a1q3_E.txt")
  open(3,file="ot_a1q3_ES.txt")
  open(4,file="ot_a1q3_sorted.txt")
  do i=1,6
    do j=1,6
      n=ran(n)
      mat(k)=100.0+(100.0*ran(n))
      sqr_mat(i,j)=mat(k)
      k=k+1
      write(1,'(F15.8,3X)',advance="no")sqr_mat(i,j)
      write(2,'(E15.7,3X)',advance="no")sqr_mat(i,j)
      write(3,'(ES15.7,3X)',advance="no")sqr_mat(i,j)
    end do
    write(1,*)
    write(2,*)
    write(3,*)
  end do
  call buble_sort(mat)
  do i=1,36
    write(4,'(F15.8)')mat(i)
  end do
end program

subroutine buble_sort(a)
  real,dimension(36)::a
  real::temp
  integer::i,j=35
  do while(j/=0)
    do i=1,j
      if(a(i)>a(i+1))then
        temp=a(i)
        a(i)=a(i+1)
        a(i+1)=temp
      end if
    end do
    j=j-1
  end do
end subroutine
