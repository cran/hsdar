subroutine differenciate (nwl,n,m,y,bandcenter,derivation)
implicit none
integer, intent(in)            :: nwl               ! Number of bands
integer, intent(in)            :: n                 ! Number of spectra
integer, intent(in)            :: m                 ! m th derivation 
double precision, intent(in)   :: y(n,nwl)          ! Reflectance value
double precision, intent(in)   :: bandcenter(nwl)   ! Width of bands       
double precision, intent(out)  :: derivation(n,nwl) ! Derivation

integer i, k, l

derivation = y

do i=1, n
  do l=1, m
    do k=1, nwl-l
      derivation(i,k) = (derivation(i,k+1)-derivation(i,k))/ &
                        (bandcenter(k+1)-bandcenter(k))
    enddo
    do k=nwl-l, nwl
      derivation(i,k) = 0.0
    enddo
  enddo
enddo
return
end subroutine