program problem_02
  implicit none
  integer::npr,ncr,n,r,fact
  read(*,*)n,r
  npr=fact(n)/fact(n-r)
  ncr=fact(n)/(fact(r)*fact(n-r))
  write(*,*)npr,ncr
end program

recursive function fact(n) result(ans)
  implicit none
  integer::n,ans,temp
  temp=1
  if(n>0)then
    temp=temp*fact(n-1)
  end if
  ans=temp
end function
