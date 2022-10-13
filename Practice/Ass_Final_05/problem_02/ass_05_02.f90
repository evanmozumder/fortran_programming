program ass_05_02
  implicit none
  integer::n,r,fact
  read(*,*)n,r
  write(*,*)"npr: ",fact(n)/fact(n-r)
  write(*,*)"ncr: ",fact(n)/(fact(r)*fact(n-r))
end program

recursive integer function fact(n) result(res)
  integer::n,temp
  temp=1
  if(n>0)then
    temp=n*fact(n-1)
  end if
  res=temp
end function
