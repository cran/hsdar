subroutine localmaxima (n, y, locmax)
implicit none
integer, intent(in)            :: n         ! Length of spectra
real, intent(in)               :: y(n)      ! Reflectance value
integer, intent(out)           :: locmax(n) ! Index of Local Maxima

integer                        :: i

locmax(1)=1
locmax(n)=n
do i=2, n-1
  if (y(i).gt.y(i-1) .and. y(i).gt.y(i+1)) then
    locmax(i)=i
  endif
enddo
end subroutine localmaxima
