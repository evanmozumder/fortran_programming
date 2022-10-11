program problem_01
  implicit none
  integer:: len1,len2,len3,len4,i,it
  character::dt
  real::rdt
  open(1,file="InputFile_1.txt")
  open(2,file="InputFile_2.txt")
  open(3,file="InputFile_3.txt")
  open(4,file="InputFile_4.txt")
  open(5,file="output.txt")

  ! reading, writing first file
  read(1,'(I1)')len1
  close(1)
  open(1,file="InputFile_1.txt")
  do i=1,len1
    if(i==1)then
      read(1,'(2X,A)',advance="no")dt
    else
      read(1,'(X,A)',advance="no")dt
    end if
    write(5,'(A,X)',advance="no")dt
  end do
  write(5,'(/)')

  ! reading, writing second file
  read(2,'(I1)')len2
  close(2)
  open(2,file="InputFile_2.txt")
  do i=1,len2
    if(i==1)then
      read(2,'(/,I2)',advance="no")it
    else if(i==4 .or. i==6)then
      read(2,'(X,I2)',advance="no")it
    else
      read(2,'(X,I1)',advance="no")it
    end if
    write(5,'(I0,X)',advance="no")it
  end do
  write(5,'(/)')

  ! reading and writing third file
  read(3,'(I1)')len3
  close(3)
  open(3,file="InputFile_3.txt")
  do i=1,len3
    if(i==1)then
      read(3,'(/,F4.1)')rdt
      write(5,'(F4.1)')rdt
    else if(i==4)then
      read(3,'(F4.2)')rdt
      write(5,'(F4.2)')rdt
    else
      read(3,'(F3.1)')rdt
      write(5,'(F3.1)')rdt
    end if
  end do
  write(5,'(/)')

  ! reading and writing fourth file
  read(4,'(I1)')len4
  close(4)
  open(4,file="InputFile_4.txt")
  do i=1,len4
    if(i==1)then
      read(4,'(/,F4.1)',advance="no")rdt
      write(5,'(F4.1,X)',advance="no")rdt
    else if(i==3 .or. i==5)then
      read(4,'(X,F3.1)')rdt
      write(5,'(F3.1)')rdt
    else if(i==4)then
      read(4,'(F4.2)',advance="no")rdt
      write(5,'(F4.2,X)',advance="no")rdt
    else if(i==6)then
      read(4,'(F3.1)',advance="no")rdt
      write(5,'(F3.1)',advance="no")rdt
    else
      read(4,'(X,F3.1)',advance="no")rdt
      write(5,'(F3.1,X)',advance="no")rdt
    end if
  end do
end program

