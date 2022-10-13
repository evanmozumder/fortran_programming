program ass_06_03
  implicit none
  integer::i,j,n,k=1
  real::norm,su,tol=0.01
  logical::cond=.true.
  real,allocatable::augmented(:,:),x0(:),x1(:),temp_norm(:)
  open(1,file="input_06_03.txt")
  print*,"enter the number of variables"
  read(*,*)n
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
  x0=0.0;x1=0.0
  do while(cond)
    do i=1,n
      su=0
      do j=1,n
        if(i/=j)su=su+x1(j)*augmented(i,j)
      end do
      x0=x1
      x1(i)=(augmented(i,n+1)-su)/augmented(i,i)
      temp_norm(i)=x1(i)-x0(i)
    end do
    norm=maxval(temp_norm)
    if(norm<tol)cond=.false.
    write(*,"(7X,I0,6X)",advance="no")k
    k=k+1
    do i=1,n
      write(*,'(11X,F10.5)',advance="no")x1(i)
    end do
    write(*,"(/)")
  end do
end program
