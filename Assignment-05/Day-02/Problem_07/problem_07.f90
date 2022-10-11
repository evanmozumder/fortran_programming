program problem_07
  implicit none
  integer,dimension(8)::p,q,r,qandr,porq,porr,porbqandr,porqbandbporr
  integer::i
  p=(/ 0,0,0,0,1,1,1,1 /)
  q=(/ 0,0,1,1,0,0,1,1 /)
  r=(/ 0,1,0,1,0,1,0,1 /)
  qandr=andOP(q,r)
  porbqandr=orop(p,qandr)
  porq=orop(p,q)
  porr=orop(p,r)
  porqbandbporr=andop(porq,porr)

  write(*,1,advance='no')"p","q","r","q/\r","p\/q","p\/r","p\/(q/\r)","(p\/q)/\(p\/r)"
  1 format(8(5X,A))
  write(*,'(//)')
  do i=1,8
    write(*,2,advance='no')p(i),q(i),r(i),qandr(i),porq(i),porr(i),porbqandr(i),porqbandbporr(i)
    2 format(3(5X,I0),3(7X,I0),10X,I0,15X,I0)
    write(*,*)
  end do
  write(*,*)
  write(*,*)"It is seen from the table that they are logically equivalent"

  contains
  function andOp(a1,a2)
  implicit none
  integer,dimension(8)::a1,a2,andOP
  integer:: i
  do i=1,8
    if(a1(i)==1 .and. a2(i)==1)then
      andOP(i)=1
    else
      andOP(i)=0
    end if
  end do
  end function
  function orop(a1,a2)
  implicit none
  integer,dimension(8)::a1,a2,orop
  integer:: i
  do i=1,8
    if(a1(i)==1 .or. a2(i)==1)then
      orop(i)=1
    else
      orop(i)=0
    end if
  end do
  end function
end program


