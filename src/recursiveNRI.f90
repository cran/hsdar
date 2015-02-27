subroutine recursive_nri (nwl, nspec, reflectance, nri, nri_length)
implicit none
integer, intent(in)            :: nwl                         ! Number of bands in data
integer, intent(in)            :: nspec                       ! Number of spectra
integer, intent(in)            :: nri_length
real, intent(in)               :: reflectance(nspec,nwl)      ! Reflectance values
real, intent(out)              :: nri(nri_length)             ! Normalized ratio index values

integer i, k, m, n

nri = 0.0
n = 0
do m=1, nspec
  do i=1, nwl-1
    do k=i+1, nwl    
      n = n + 1
      nri(n) = (reflectance(m,k)-reflectance(m,i)) / &
               (reflectance(m,k)+reflectance(m,i))
    enddo
  enddo
enddo
end subroutine recursive_nri
