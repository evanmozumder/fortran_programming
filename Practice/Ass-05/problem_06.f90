program problem_06
  implicit none
  integer::i,j
  logical::p,q=.false.,r=.false.
  do i=1,8
    if(i<=4)then
      p=.true.
    else
      p=.false.
    end if
    if(mod(i,2)==1)then
      if(q)then
        q=.false.
      else
        q=.true.
      end if
    end if
    if(r)then
      r=.false.
    else
      r=.true.
    end if
    write(*,*)p,q,r
  end do
end program
