subroutine meanfilter (nwl,n,p,y,smoothedy)
implicit none
integer, intent(in)            :: nwl               ! Number of bands
integer, intent(in)            :: n                 ! Number of spectra
integer, intent(in)            :: p                 ! Window size
double precision, intent(in)   :: y(n,nwl)          ! Reflectance values     
double precision, intent(out)  :: smoothedy(n,nwl)  ! Smoothed reflectance values

integer           :: i, k, l, stop_index, start_index
double precision  :: su

do i=1, n
  do k=1, nwl
    start_index = max(1, k-p)
    stop_index = min(nwl, k+p)
    
    su = 0.0
    
    do l=start_index, stop_index
      su = su + y(i,l)
    enddo
    
    smoothedy(i, k) = su/dble(stop_index-start_index+1)
    
  enddo
  
enddo


return
end subroutine meanfilter

subroutine gliding_meanfilter (nwl,n,p,y,smoothedy)
implicit none
integer, intent(in)            :: nwl               ! Number of bands
integer, intent(in)            :: n                 ! Number of spectra
integer, intent(in)            :: p                 ! Window size
double precision, intent(in)   :: y(n,nwl)          ! Reflectance values     
double precision, intent(out)  :: smoothedy(n,nwl)  ! Smoothed reflectance values

integer           :: i, k, l
double precision  :: su

do i=1, n
  do k=1, p-1
    su = 0.0
    do l=1, k
      su = su + y(i,l)
    enddo
    smoothedy(i,k) = su / dble(k)
  enddo
  do k=p, nwl
    su = 0.0
    do l=k-p+1, k
      su = su + y(i,l)
    enddo
    smoothedy(i,k) = su / dble(p)
  enddo
enddo
return
end subroutine gliding_meanfilter
! 
! subroutine savitzky_golay (nwl,n,p,y,smoothedy)
! implicit none
! integer, intent(in)            :: nwl               ! Number of bands
! integer, intent(in)            :: n                 ! Number of spectra
! integer, intent(in)            :: p                 ! Window size
! double precision, intent(in)   :: y(n,nwl)          ! Reflectance values     
! double precision, intent(out)  :: smoothedy(n,nwl)  ! Smoothed reflectance values
! 
! do i=1, n
!   do k=1, p
!     smoothedy(i, k) = y(i, k)
!   enddo
!   
!   do k=p+1, nwl-p-1
!     start_index = k-p
!     stop_index = k+p
!     
!     su = 0.0
!     
!     do l=start_index, stop_index
!       su = su + y(i,l)
!     enddo
!     
!     smoothedy(i, k) = su/dble(stop_index-start_index+1)
!     
!   enddo
!   
!   do k=nwl-p, nwl
!     smoothedy(i, k) = y(i, k)
!   enddo
!   
! enddo
! 
! 
! return
! end subroutine savitzky_golay