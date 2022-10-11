program problem_01
  character,allocatable :: first_array(:)
  integer,allocatable::second_array(:)
  real,allocatable:: third_array(:),fourth_array(:)
  integer:: len1,i,j,len2,len3,len4
  open(unit=1,file='InputFile_1.txt',status='old')
  open(unit=2,file='InputFile_2.txt',status='old')
  open(unit=3,file='InputFile_3.txt',status='old')
  open(unit=4,file='InputFile_4.txt',status='old')
  open(unit=5,file='OutputFile.txt',status='replace')

  ! reading and writing first file
  read(1,'(I1)')len1
  close(1)
  allocate(first_array(len1))
  open(unit=1,file='InputFile_1.txt',status='old')
  do i=1,len1
    if(i==1)then
      read(1,10,advance='no')first_array(i)
    else
      read(1,11,advance='no')first_array(i)
    end if
    write(5,'(A,X)',advance='no')first_array(i)
  end do
  10 format(2X,A)
  11 format(X,A)

  ! reading and writing second file
  read(2,'(I1)')len2
  close(2)
  allocate(second_array(len2))
  open(unit=2,file='InputFile_2.txt',status='old')
  do i=1,len2
    if(i==1)then
      read(2,12,advance='no')second_array(i)
      write(5,'(/,I2)',advance='no')second_array(i)
    else if(i==4 .or. i==6)then
      read(2,13,advance='no')second_array(i)
      write(5,'(X,I2)',advance='no')second_array(i)
    else
      read(2,14,advance='no')second_array(i)
      write(5,'(X,I1)',advance='no')second_array(i)
    end if
  end do
  12 format(/,I2)
  13 format(X,I2)
  14 format(X,I1)


  ! reading and writing third file
  read(3,'(I1)')len3
  close(3)
  allocate(third_array(len3))
  open(unit=3,file='InputFile_3.txt',status='old')
  do i=1,len3
    if(i==1)then
      read(3,'(/,F4.1)')third_array(i)
      write(5,'(/,F4.1)')third_array(i)
    else if(i==4)then
      read(3,'(F4.2)')third_array(i)
      write(5,'(F4.2)')third_array(i)
    else
      read(3,'(F3.1)')third_array(i)
      write(5,'(F3.1)')third_array(i)
    end if
  end do

  ! reading and writing fourth file
  read(4,'(I1)')len4
  close(4)
  allocate(fourth_array(len4))
  open(unit=4,file='InputFile_4.txt',status='old')
  do i=1,len4
    if(i==1)then
      read(4,'(/,F4.1)',advance='no')fourth_array(i)
      write(5,'(F4.1)',advance='no')fourth_array(i)
    else if(i==4)then
      read(4,'(F4.2)',advance='no')fourth_array(i)
      write(5,'(F4.2)',advance='no')fourth_array(i)
    else if(i==3 .or. i==5)then
      read(4,'(X,F3.1)')fourth_array(i)
      write(5,'(X,F3.1)')fourth_array(i)
    else if(i==6)then
      read(4,'(F3.1)',advance='no')fourth_array(i)
      write(5,'(F3.1)',advance='no')fourth_array(i)
    else
      read(4,'(X,F3.1)',advance='no')fourth_array(i)
      write(5,'(X,F3.1)',advance='no')fourth_array(i)
    end if
  end do
end program
