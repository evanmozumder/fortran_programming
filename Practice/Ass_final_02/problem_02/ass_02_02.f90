program ass_02_02
  implicit none
  integer::n,i
  integer,allocatable::ans(:)
  read(*,*)n
  allocate(ans(n+1))
  do i=0,n
    if(i<=1)then
      ans(i)=i
    else
      if(mod(i,2)==0)then
        ans(i)=ans(i/2)
      else
        ans(i)=ans((i-1)/2)+ans((i+1)/2)
      end if
    end if
    write(*,*)i,ans(i)
  end do
end program
