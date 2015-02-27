subroutine suh (nlm, n, LMin, y, hull, ptscon)
implicit none
integer, intent(in)            :: nlm       ! Number of local maxima
integer, intent(in)            :: n         ! Length of spectra
integer, intent(in)            :: LMin(nlm) ! Local maximum array
real, intent(in)               :: y(n)      ! Reflectance value
real, intent(out)              :: hull(n)   ! Hull of spectrum
integer, intent(out)           :: ptscon(n) ! Continuum points

integer                        :: i, k, j, m, o, p
integer                        :: gm
logical                        :: addfound
real                           :: gmval
real                           :: slope
real                           :: intercept
real                           :: diff
integer, allocatable           :: lm(:)
integer, allocatable           :: addlm(:)

! Initialize
allocate(lm(nlm),addlm(nlm))
lm    = LMin
addlm = 0

! Global maximum
gm    = 0
gmval = -10000000000.0
do i=1, n
  if (gmval.lt.y(i)) then
    gm = i
    gmval = y(i)
  endif
enddo


! Make sure that the slope doesn't change direction
k=1
i=2

do while (lm(i).lt.gm)
  if (y(lm(i)).lt.y(lm(k))) then
    lm(i) = 0
  else
    k = i
  endif
  i = i + 1
enddo
j = i
if (j.lt.nlm) then
  i = nlm-1
  k = nlm
  do while (lm(i).gt.gm)
    if (y(lm(i)).lt.y(lm(k))) then
      lm(i) = 0
    else
      k = i
    endif
    i = i - 1
  enddo
endif


! Reformat local maxima vector
addlm = lm
deallocate(lm)
allocate(lm(n))
lm = 0
do i=1, nlm
  if (addlm(i).gt.0 .and. addlm(i).le.n) then
    lm(addlm(i))=addlm(i)
  endif
enddo
deallocate(addlm)

! Search for further Continuum Points

addfound = .true.
o=2
p=0
do while (o.gt.1)
!   Initialize
  k=1
  j=1
  m=1
  o=0
  p=p+1
  slope = 0.0
  intercept = y(1)
  diff = -1.0
  addfound = .false.
!   Loop over data range
  do i=1, n-1
!     Check if slope and intercept must be recalculated because of new hull - line
    if (j.le.i) then
      if (diff.gt.0.0) then
        lm(m)=m
      endif
      diff = -1.0
      m = i
      k = j
      j = j + 1
      do while (lm(j).eq.0)
        j = j + 1
        if (j.ge.n) goto 100
      enddo
100   slope     = (y(j)-y(k))/float(j-k)
      intercept = y(k)-slope*float(k)
    else
!       Calculate hull-value for x = i
      gmval = float(i)*slope+intercept
!       Check if hull-value is smaller than y-value
      if (gmval.lt.y(i)) then
        o = o + 1
!         Calculate maximum difference between hull- and y-value
        if (diff.lt.(y(i)-gmval)) then
          m = i
          diff = y(i)-gmval
        endif
      endif
    endif
!     Calculate hull
    hull(i) = float(i)*slope+intercept
  enddo
  if (p.gt.n) goto 101
enddo
101 hull(n)=y(n)
ptscon=lm
deallocate(lm)
end subroutine suh
