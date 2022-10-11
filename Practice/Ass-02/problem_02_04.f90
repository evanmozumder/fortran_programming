program problem_02_04
  implicit none
  integer::n,i,gcd
  read(*,*)n
  do i=1,n
    if(gcd(n,i)==1)write(*,'(I0,3X)')i
  end do
end program

recursive function gcd(a,b) result(ans)
  implicit none
  integer::a,b,ans
  if(mod(a,b)==0)then
    ans=b
  else
    ans=gcd(b,mod(a,b))
  end if
end function
