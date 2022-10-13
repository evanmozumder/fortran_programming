program ass_05_01
  implicit none
  integer::n,i,res,lucus
  read(*,*)n
  do i=0,n
    write(*,*)lucus(i)
  end do
end program

recursive integer function lucus(n) result(res)
  integer::n
  if(n==0)then
    res=2
  else if(n==1)then
    res=1
  else
    res=lucus(n-1)+lucus(n-2)
  end if
end function
