subroutine sam (nspec, nref, nbands, spec, specref, specang)
implicit none
integer, intent(in)            :: nspec  
integer, intent(in)            :: nref
integer, intent(in)            :: nbands
double precision, intent(in)   :: spec(nspec,nbands)
double precision, intent(in)   :: specref(nref,nbands)
double precision, intent(out)  :: specang(nspec,nref)


integer ispec, iband, iref
double precision ssspec, ssref, ssboth


do ispec=1, nspec
  do iref=1, nref
    ssspec = 0.0
    ssref  = 0.0
    ssboth = 0.0
    do iband=1, nbands
      ssspec = ssspec + spec(ispec,iband)**2
      ssref  = ssref  + specref(iref,iband)**2
      ssboth = ssboth + spec(ispec,iband) * &
               specref(iref,iband)
    enddo

    specang(ispec,iref) = dble(acos(ssboth/(ssspec**0.5 * ssref**0.5)))

  enddo
enddo

end subroutine sam
