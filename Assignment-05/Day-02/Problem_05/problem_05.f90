program problem_05
  implicit none
  integer:: calc,x
  write(*,*)"Enter bit strings length: "
  read(*,*)x
  write(*,*)"number of bit-strings except two consecutive 0s: ",calc(x)
end program

recursive function calc(x) result(ans)
  implicit none
  integer:: ans,x
  if(x<3)then
    ans=x+1
  else
    ans=calc(x-1)+calc(x-2)
  end if
end function
