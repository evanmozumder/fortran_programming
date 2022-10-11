program practice_05
  real,dimension(6,6)::arr
  real,dimension(36)::mat
  integer::i,j,n,k
  open(1,file='ot_a1q3_F.txt')
  open(2,file='ot_a1q3_E.txt')
  open(3,file='ot_a1q3_ES.txt')
  k=1
  do i=1,6
    do j=1,6
      n=ran(n)
      arr(i,j)=100.0+100.0*ran(n)
      mat(k)=arr(i,j)
      k=k+1
    end do
  end do
  write(1,10,advance='no')((arr(i,j),j=1,6),i=1,6)
  10 format(6(F15.8,3X),/)

  write(2,11,advance='no')((arr(i,j),j=1,6),i=1,6)
  11 format(6(E15.7,3X),/)

  write(3,12,advance='no')((arr(i,j),j=1,6),i=1,6)
  12 format(6(ES15.8,3X),/)

  call bubble_sort(mat)
end program

subroutine bubble_sort(a)
  implicit none
  real,intent(inout),dimension(36)::a
  integer::i,j,p
  real:: temp
  p=35
  do i=1,35
    do j=1,p
      if(a(j)>a(j+1))then
        temp=a(j)
        a(j)=a(j+1)
        a(j+1)=temp
      end if
    end do
    p=p-1
  end do
  open(4,file='output-1.txt')
  write(4,'(F15.8)')(a(i),i=1,36)
end subroutine
