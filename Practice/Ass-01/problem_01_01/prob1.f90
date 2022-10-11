program problem_01
  implicit none
  integer:: len1,len2,len3,len4
  open(1,file="inp1.txt")
  open(2,file="inp2.txt")
  open(3,file="inp3.txt")
  open(4,file="inp4.txt")
  open(5,file="out.txt")
  read(1,'(I0)')len1
  close(1)
  open(1,file="inp1.txt")
  do i=1,len1
    if(i==1)then
      read(1,'(2X,A)',advance="no")
    end if
  end do
end program
