program facto
  implicit none
  integer::n,fact
  read(*,*)n
  write(*,*)fact(n)
end program

recursive function fact(n) result(ans)
  implicit none
  integer::n,ans,temp
  temp=1
  if(n>1)then
    temp=n*fact(n-1)
  end if
  ans=temp
end function
