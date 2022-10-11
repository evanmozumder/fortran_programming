program problem_03
  integer::i
  do i=1,20
    if(i<=9)then
      open(i,file="File_"//char(48)//char(48+i)//".txt")
      write(i,'(A,I2.2)',advance="no")"This is file number ",i
    else if(i<20)then
      open(i,file="File_"//char(49)//char(38+i)//".txt")
      write(i,'(A,I0)',advance="no")"This is file number ",i
    else
      open(i,file="File_"//char(50)//char(48)//".txt")
      write(i,'(A,I0)',advance="no")"This is file number ",i
    end if
  end do
end program
