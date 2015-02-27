subroutine checkhull (ncp, n, pts, y, offset, res, hull)
integer, intent(in)            :: ncp         ! Number of continuum points
integer, intent(in)            :: n           ! Length of spectra
integer, intent(in)            :: pts(ncp)    ! Continuum points
double precision, intent(in)   :: y(n)        ! Reflectance values
integer, intent(in)            :: offset
integer, intent(out)           :: res(2)      ! Flag if hull matches data
double precision, intent(out)  :: hull(n)     ! Hull values

integer                        :: maxreswavelength

integer                        :: i, k, ptscon(ncp)
double precision               :: slope, intercept, gmval, tol

res=0
tol=-1.0e-7
hull=0.0

ptscon = pts-offset

hull(ptscon(1))=y(ptscon(1))

do i=2, ncp
  slope     = (y(ptscon(i))-y(ptscon(i-1)))/dble(ptscon(i)-ptscon(i-1))
  intercept = y(ptscon(i-1))-slope*dble(ptscon(i-1))
  
  hull(ptscon(i)) = dble(ptscon(i))*slope+intercept
  do k=ptscon(i-1)+1, ptscon(i)-1
    gmval = dble(k)*slope+intercept
    hull(k) = gmval
    if ((gmval-y(k)).lt.tol) then
      res(1)=k+offset
      res(2)=maxreswavelength(ptscon(i-1),ptscon(i),y(ptscon(i-1):ptscon(i)),&
                              slope,intercept) + offset
      return
    endif
  enddo
enddo

end subroutine checkhull

subroutine makehull (ncp, n, pts, y, offset, hull)
implicit none
integer, intent(in)            :: ncp         ! Number of continuum points
integer, intent(in)            :: n           ! Length of spectra
integer, intent(in)            :: pts(ncp)    ! Continuum points
double precision, intent(in)   :: y(n)        ! Reflectance values
integer, intent(in)            :: offset
double precision, intent(out)  :: hull(n)     ! Hull values

integer                        :: i, k, ptscon(ncp)
double precision               :: slope, intercept

hull=0.0

ptscon = pts-offset

hull(ptscon(1))=y(ptscon(1))

do i=2, ncp
  slope     = (y(ptscon(i))-y(ptscon(i-1)))/dble(ptscon(i)-ptscon(i-1))
  intercept = y(ptscon(i-1))-slope*dble(ptscon(i-1))
  
  hull(ptscon(i)) = dble(ptscon(i))*slope+intercept
  do k=ptscon(i-1)+1, ptscon(i)-1
    hull(k) = dble(k)*slope+intercept
  enddo
enddo

end subroutine makehull

integer function maxreswavelength(startval,stopval,y,slope,intercept)
implicit none
integer          :: startval, stopval, i, mv
double precision :: slope, intercept, diff, gmval
double precision :: y(stopval-startval+1)

diff = 0.0
mv   = 0

do i=1,stopval-startval+1
  gmval = dble(i+startval-1)*slope+intercept
  if ((y(i)-gmval).gt.diff) then
    diff = y(i)-gmval
    mv = i+startval-1
  endif
enddo

maxreswavelength = mv

return
end function maxreswavelength