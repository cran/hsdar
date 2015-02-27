subroutine bdr (n, ny, lenval, y)
implicit none
integer, intent(in)            :: n                 ! Number of spectra
integer, intent(in)            :: ny
integer, intent(in)            :: lenval(n)         ! Length of features in spectra
double precision               :: y(ny)         ! Reflectance values     

integer(4) i, k, m
double precision maxy

k=1
do i=1, n
  maxy = y(k)
  do m=k+1, k+lenval(i)-1
    if (maxy .lt. y(m)) maxy = y(m)
  enddo
  do m=k, k+lenval(i)
    y(m) = y(m) / maxy
  enddo
  k = k + 1 + lenval(i)
enddo
return
end subroutine bdr
  
subroutine ndbi (n, ny, lenval, y)
implicit none
integer, intent(in)            :: n                 ! Number of spectra
integer, intent(in)            :: ny
integer, intent(in)            :: lenval(n)         ! Length of features in spectra
double precision               :: y(ny)         ! Reflectance values     

integer(4) i, k, m
double precision maxy

k=1
do i=1, n
  maxy = y(k)
  do m=k+1, k+lenval(i)-1
    if (maxy .lt. y(m)) maxy = y(m)
  enddo
  do m=k, k+lenval(i)
    y(m) = (y(m) - maxy) / (y(m) + maxy)
  enddo
  k = k + 1 + lenval(i)
enddo
return
end subroutine ndbi
  
subroutine bna (n, ny, lenval, y)
implicit none
integer, intent(in)            :: n                 ! Number of spectra
integer, intent(in)            :: ny
integer, intent(in)            :: lenval(n)         ! Length of features in spectra
double precision               :: y(ny)         ! Reflectance values     

integer(4) i, k, m
double precision area

k=1
do i=1, n
  area = 0.0
  do m=k, k+lenval(i)-1
    area = area + y(m)
  enddo
  do m=k, k+lenval(i)
    y(m) = y(m) / area
  enddo
  k = k + 1 + lenval(i)
enddo
return
end subroutine bna
