program problem_02
  implicit none
  integer,allocatable,dimension(:)::a
  integer::n,i
  read(*,*)n
  allocate(a(n+1))
  write(*,'(T10A,T30,A)')"Value of n","n-th term"
  do i=0,n
    if(i==0 .or. i==1)then
      a(i)=i
    else if(mod(i,2)==0)then
      a(i)=a(i/2)
    else
      a(i)=a((i-1)/2)+a((i+1)/2)
    end if
    write(*,'(T15,I0,T35,I0)')i,a(i)
  end do
end program
