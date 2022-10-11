program problem_03
  integer:: i
  do i=1,20
    if(i<=9)then
      open(unit=i,file='File_'//char(48)//char(48+i)//'.txt',status='old')
      write(i,'(A,I1)')'This is file number 0',i
    else if(i<20)then
      open(unit=i,file='File_'//char(49)//char(38+i)//'.txt',status='old')
      write(i,'(A,I2)')'This is file number ',i
    else
      open(unit=i,file='File_'//char(50)//char(48)//'.txt',status='old  ')
      write(i,'(A,I2)')'This is file number ',i
    end if
  end do
end program
