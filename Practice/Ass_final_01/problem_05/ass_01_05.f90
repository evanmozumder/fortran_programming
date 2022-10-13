program ass_01_05
  implicit none
  real::mat(36),sq_mat(6,6),sorted(36)
  integer::i,j,n=36,num
  open(1,file="ot_a1q3_F.txt")
  open(2,file="ot_a1q3_E.txt")
  open(3,file="ot_a1q3_ES.txt")
  do i=1,n
    num=ran(num)
    mat(i)=100.0+100.0*ran(num)
  end do
  sq_mat=reshape(mat,(/6,6/))
  write(1,*)"Square Matrix: "
  call display(mat,6)
  n=6
  do i=1,n
    do j=1,n
      write(2,"(E15.7)",advance="no")sq_mat(i,j)
    end do
    write(2,*)
  end do
  do i=1,n
    do j=1,n
      write(3,"(ES15.7)",advance="no")sq_mat(i,j)
    end do
    write(3,*)
  end do
  n=36
  sorted=mat
  call bubble_sort(sorted,n)
  write(*,"(F10.5)")(sorted(i),i=1,n)
end program

subroutine bubble_sort(mat,n)
  implicit none
  real::mat(n),temp
  integer::n,k,i
  k=n-1
  do while(k/=0)
    do i=1,k
      if(mat(i)>mat(i+1))then
        temp=mat(i)
        mat(i)=mat(i+1)
        mat(i+1)=temp
      end if
    end do
    k=k-1
  end do
end subroutine

subroutine display(mat,n)
  implicit none
  real::mat(n,n)
  integer::n,i,j
  do i=1,n
    do j=1,n
      write(1,"(F15.8)",advance="no")mat(i,j)
    end do
    write(1,*)
  end do
end subroutine
