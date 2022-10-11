program problem_03
  implicit none
  integer:: calc,n,conv,t
  n=25
  t=1
  conv=24*60*60
  write(*,*)"days required",(calc(n)*t)/conv
end program

recursive function calc(h) result(ans)
  implicit none
  integer:: h,ans
  ans=1
  if(h>1)then
    ans=2*calc(h-1)+1
  end if
end function
