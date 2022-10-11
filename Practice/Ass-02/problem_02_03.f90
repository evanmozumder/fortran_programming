program problem_02_03
  implicit none
  integer::a,b,gcd
  read(*,*)a,b
  write(*,*)gcd(a,b)
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
