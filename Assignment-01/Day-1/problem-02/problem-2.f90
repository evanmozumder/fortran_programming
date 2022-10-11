program problem_02
  character(len=50)::name
  integer:: an,length
  open(unit=1,file='name.txt',status='old')
  read(1,'(A)'),name
  write(*,'(A)')name

  ! (i)
  do i=1,len(name)
    if(name(i:i)==" ")cycle
    write(*,'(A)',advance='no')name(i:i)
  end do
  write(*,'(/)')

  ! (ii)
  DO i = 1 , LEN(name)
    IF( name(i:i)=="A" .OR. name(i:i)=="E" .OR. name(i:i)=="I" .OR. name(i:i)=="O" .OR. name(i:i)=="U") CYCLE
    IF(name(i:i)=="a" .OR. name(i:i)=="e" .OR. name(i:i)=="i" .OR. name(i:i)=="o" .OR. name(i:i)== "u") CYCLE
    WRITE(*,'(A)', ADVANCE = "NO")name(i:i)
  END DO
  write(*,'(/)')

  ! (iii)
  do i=1,len(name)
    an = iachar(name(i:i))
    if(an>=97)then
      an = an-32
      write(*,'(A)',advance='no')achar(an)
    else
      write(*,'(A)',advance='no')name(i:i)
    end if
  end do
  write(*,'(/)')

  ! (iv)
  do i=1,len(name)
    if(name(i:i)==' ')then
      if(name(i+1:i+1)==' ')then
        length=i-1
        exit
      end if
    end if
  end do
  do i=length,1,-1
    write(*,'(A)',advance='no')name(i:i)
  end do
end program
