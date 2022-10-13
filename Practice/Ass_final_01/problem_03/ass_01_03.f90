program ass_01_03
  implicit none
  integer,parameter::n=20
  integer::i
  do i=1,n
    if(i<10)then
      open(i,file="File_"//char(48)//char(48+i)//".txt")
      write(i,"(A,I0)")"This is file number 0",i
    else if(i<20)then
      open(i,file="File_"//char(49)//char(38+i)//".txt")
      write(i,"(A,I0)")"This is file number ",i
    else
      open(i,file="File_"//char(50)//char(48)//".txt")
      write(i,"(A,I0)")"This is file number ",i
    end if
  end do
end program
