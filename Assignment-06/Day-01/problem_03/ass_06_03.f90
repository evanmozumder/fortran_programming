program ass_06_03
  implicit none
  real,allocatable::augmented(:,:),x0(:),x1(:),su,temp_norm(:),norm,tol
  logical::cond=.true.
  integer::n,i,j,k=1,l
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
  x1=0.0
  x0=0.0
  tol=10.0**(-2)
  write(*,"(5X,A)",advance="no")"Iteration"
  do i=1,n
    write(*,100,advance="no")i
  end do
  write(*,"(//)")
  100 format(16x,"x(",I0,")")
  do while(cond)
    do i=1,n
      su=0
      do j=1,n
        if(i/=j)su=su+x1(j)*augmented(i,j)
      end do
      do l=1,n
        x0(l)=x1(l)
      end do
      x1(i)=(1.0/augmented(i,i))*(augmented(i,n+1)-su)
      temp_norm(i)=abs(x1(i)-x0(i))
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
