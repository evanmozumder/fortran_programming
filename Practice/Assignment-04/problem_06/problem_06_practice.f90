program problem_06
  implicit none
  integer::i,t=1
  logical::p,q=.false.,r=.false.
  write(*,'(A,T4,A,T8,A,T16,A,T25,A,T36,A,T50,A,T65,A,/)',advance="no")'p','q','r','q/\r','p\/q','p\/r','p\/(q/\r)','(p\/q)/\(p\/r)'
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
    write(*,'(8(L,3X),/)',advance="no")p,q,r,(q.and.r),(p.or.q),(p.or.r),(p.or.(q.and.r)),((p.or.q).and.(p.or.r))
  end do
end program
