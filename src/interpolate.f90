! code <- "
subroutine inter(b1, n_b1, b2, n_b2, refl_in, refl_out, nspectra)
implicit none
integer n_b1, n_b2, nspectra
integer indices(n_b2, 2)
double precision b1(n_b1), b2(n_b2), refl_in(nspectra, n_b1), refl_out(nspectra, n_b2)
double precision weights(n_b2)

integer i, k
! double precision tol

! tol = 1.0e-10

indices = 0
do i=1, n_b2
  k = 2
  do while ((k .le. n_b1) .and. (indices(i, 1)*indices(i, 2)) .eq. 0) 
    if (indices(i, 1) .eq. 0) then
      if (b2(i) .ge. b1(k-1) .and. b2(i) .le. b1(k)) then
        indices(i, 1) = k-1
      endif
    endif
    
    if (indices(i, 2) .eq. 0) then      
      if (b2(i) .le. b1(k) .and. b2(i) .gt. b1(k-1)) then
        indices(i, 2) = k
      endif
    endif
    k = k + 1
  enddo
enddo


do i=1, n_b2
  weights(i) = (b2(i) - b1(indices(i,1)))/(b1(indices(i,2))-b1(indices(i,1)))
enddo

! do i = 1, 5
!   print*, refl_in(1, i)
! enddo
! do i = 1, 5
!   print*, refl_in(i, 1)
! enddo

do i = 1, n_b2
  refl_out(:,i) = (1.0 - weights(i)) * refl_in(:, indices(i,1)) + weights(i) * refl_in(:, indices(i,2)) 
enddo

return

end subroutine
! "
