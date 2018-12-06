subroutine recursive_sr (nwl, nspec, reflectance, sr, sr_length)
implicit none
integer, intent(in)            :: nwl                         ! Number of bands in data
integer, intent(in)            :: nspec                       ! Number of spectra
integer, intent(in)            :: sr_length
real, intent(in)               :: reflectance(nspec,nwl)      ! Reflectance values
real, intent(out)              :: sr(sr_length)             ! Normalized ratio index values

integer i, k, m, n

sr = 0.0
n = 0
do m=1, nspec
  do i=1, nwl-1
    do k=i+1, nwl    
      n = n + 1
      sr(n) = (reflectance(m,k)/reflectance(m,i))
    enddo
  enddo
enddo
end subroutine recursive_sr
