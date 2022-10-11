program problem_05
  implicit none
  real, dimension(6,6)::arr
  real, dimension(36)::mat
  integer:: i,j,n,k
  open(unit=1,file='ot_a1q3_F.txt',status='replace')
  open(unit=2,file='ot_a1q3_E.txt',status='replace')
  open(unit=3,file='ot_a1q3_ES.txt',status='replace')
  k=1
  do i=1,6
    do j=1,6
      n=ran(n)
      arr(i,j)=100.0+100.0*ran(n)
      mat(k)=arr(i,j)
      k=k+1
    end do
  end do
  ! (i)
  write(1,10)((arr(i,j),j=1,6),i=1,6)
  10 format(6(F12.8,3X),/)

  ! (ii)
  write(2,11)((arr(i,j),j=1,6),i=1,6)
  11 format(6(E15.7,3X),/)

  ! (iii)
  write(3,12)((arr(i,j),j=1,6),i=1,6)
  12 format(6(ES15.7,3X),/)

  call bubble_sort(mat)
end program

SUBROUTINE bubble_sort(A)
  REAL,INTENT(INOUT),DIMENSION(36)::A
  REAL::temp
  INTEGER::i,j,p
  p=35
  DO i=1,35
      DO j=1,p
          IF(A(j)>A(j+1))THEN
              temp=A(j)
              A(j)=A(j+1)
              A(j+1)=temp
          END IF
      END DO
      p=p-1
  END DO
  OPEN(13,FILE='ot_a1q3_sorted.txt')
  WRITE(13,' (F20.8) ')(A(i),i=1,36)
END SUBROUTINE
