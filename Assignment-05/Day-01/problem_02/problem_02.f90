program problem_02
  implicit none
  integer::npr,ncr,n,r,fact
  write(*,*)"Enter the value of n and r: "
  read(*,*)n,r
  npr=fact(n)/fact(n-r)
  ncr=fact(n)/(fact(r)*fact(n-r))
  write(*,*)"npr is : ",npr
  write(*,*)"ncr is : ",ncr
end program

recursive function fact(n) result(ans)
  implicit none
  integer::a,n,ans
  a=1
  if(n>0)then
    a=fact(n-1)*n
  end if
  ans=a
end function

