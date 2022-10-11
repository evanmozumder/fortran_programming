program problem_01
  implicit none
  integer:: l,n,i
  n=12
  do i=0,n
    write(*,'(I3,2X)',advance="no")i
  end do
  write(*,*)
  do i=0,n
    write(*,'(I3,2X)',advance="no")l(i)
  end do
end program

recursive function l(x) result(ans)
  implicit none
  integer:: ans,x
  if(x==0)then
    ans=2
  else if(x==1)then
    ans=1
  else
    ans=l(x-1)+l(x-2)
  end if
end function
