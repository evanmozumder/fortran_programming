program ass_05_06
  implicit none
  logical::p,q=.true.,r=.true.
  integer::i
  write(*,"(3(9X,A),5X,A,5X,A,/)")"p","q","r","p\/(q/\r)","(p\/q)/\(p\/r)"
  do i=1,8
    if(i<=4)then
      p=.false.
    else
      p=.true.
    end if
    if(mod(i,2)/=0)then
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
    write(*,"(4L10,L15)")p,q,r,p.or.(q.and.r),(p.or.q).and.(p.or.r)
  end do
end program