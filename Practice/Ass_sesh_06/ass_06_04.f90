program ass_06_04
  implicit none
  real::w=1.2,tol=0.001
  real,allocatable::augmented(:,:),x0(:),x1(:),temp_norm(:)
  integer::i,j,n=3
  open(1,file="input_06_04.txt")
  allocate(augmented(n,n+1),x0(n),x1(n),temp_norm(n))
  do i=1,n
    do j=1,n+1
      read(1,*)augmented(i,j)
    end do
  end do
  print*,"Augmented Matrix is: "
  do i=1,n
    do j=1,n+1
      write(*,"(F5.1,2X)",advance="no")augmented(i,j)
    end do
    write(*,"(/)")
  end do
  x1=0.0;x0=0.0
  do while(cond)
    do i=1,n
      su=0
      do j=1,n
        if(i/j)su=su+(x1(j)*augmented(i,j))
      end do
      x0=x1
      x1(i)=(1-w)*x1(i)+w/augmented(i,i)*(augmented(i,n+1)-su)
      temp_norm(i)=x1(i)-x0(i)
    end do
    norm=maxval(temp_norm)
    if(norm<tol)exit
  end do
end program

