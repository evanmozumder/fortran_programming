program ass_05_04
  implicit none
  integer::n,res,i,cons
  read(*,*)n
  do i=1,n
    write(*,*)cons(i)
  end do
end program

recursive integer function cons(n) result(res)
  implicit none
  integer::n
  res=0
  if(n<3)then
    res=n+1
  else
    res=cons(n-1)+cons(n-2)
  end if
end function
