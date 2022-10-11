program problem_03
  integer:: N
  do N=1,20
    if(N<=9)then
      open(unit=N,file="File_"//char(48)//char(48+N)//".txt",status='replace')
    else if(N>9 .and. N<20)then
      open(unit=N,file="File_"//char(49)//char(38+N)//".txt",status='replace')
    else
      open(unit=N,file="File_"//char(50)//char(48)//".txt",status='replace')
    end if
    IF(N<=9)THEN
        WRITE(N,100)N
    ELSE
        WRITE(N,101)N
    END IF
    100 FORMAT("This is file number 0",I0)
    101 FORMAT("This is file number ",I0)
    CLOSE(UNIT= N)
  end do
end program
