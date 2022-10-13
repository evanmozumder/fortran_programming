program ass_05_03
  implicit none
  integer::n,hanoi,conv=24*3600
  read(*,*)n
  print*,hanoi(n)/conv
end program

recursive integer function hanoi(n) result(res)
  implicit none
  integer::n
  if(n==1)then
    res=1
  else
    res=2*(hanoi(n-1))+1
  end if
end function


