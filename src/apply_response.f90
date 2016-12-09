subroutine apply_response (nwl, nspec, nband, &!wl,
                           spec, &!responsedim,&
                           response_transformed, integrated, no_data)
implicit none

integer, intent (in)           :: nwl, nspec, nband
! double precision, intent (in)  :: responsedim(3)
! double precision, intent (in)  :: wl(nwl)
double precision, intent (in)  :: spec(nspec,nwl), no_data
double precision, intent (in)  :: response_transformed(nwl,nband)
double precision, intent (out) :: integrated(nspec,nband)

double precision :: sum_weights, tol = 1.0e-6
integer          :: iband, ispec, iwl


integrated = 0.0
do iband=1, nband
  specloop: do ispec=1, nspec
    sum_weights = 0.0
    do iwl=1, nwl      
      if (response_transformed(iwl,iband) .gt. 0) then
        if (abs(spec(ispec,iwl) - no_data) .gt. tol) then
          integrated(ispec,iband) = integrated(ispec,iband) + &
              spec(ispec,iwl) * response_transformed(iwl,iband)
          sum_weights = sum_weights + response_transformed(iwl,iband)
        else 
          integrated(ispec,iband) = no_data
          cycle specloop
        endif
      endif
    enddo
    integrated(ispec,iband) = integrated(ispec,iband) / sum_weights
  enddo specloop
enddo

return
    
end subroutine

subroutine transform_response (nwl, nband, nwlresponse, responsedim, &
                               response, response_transformed, wl)
implicit none

integer, intent (in)           :: nwl, nwlresponse, nband
double precision, intent (in)  :: response(nwlresponse,nband)
double precision, intent (in)  :: responsedim(3)
double precision, intent (out) :: response_transformed(nwl,nband)
double precision, intent (in)  :: wl(nwl)

integer          :: i, k, iband
double precision :: slope, aspect

response_transformed = 0.0

do iband=1, nband
  if (wl(1) .lt. responsedim(1)) then
    k = 1
  else
    k = int((wl(1) - responsedim(1)) / responsedim(3))
  endif
  i = 1
  do while (k .le. nwlresponse .and. i .le. nwl .and. &
            wl(i) .le. responsedim(2))
    if (wl(i) .ge. responsedim(1) .and. wl(i) .le. responsedim(2)) then
      if (wl(i) .eq. (responsedim(1) - responsedim(3) + dble(k) * responsedim(3))) then
        response_transformed(i,iband) = response(k,iband)
        k = k + 1
      else
        do while (((responsedim(1)-responsedim(3)) + dble(k) * responsedim(3)) &
                  .lt. wl(i) .and. k .lt. nwlresponse) 
          k = k + 1
        enddo
        slope = (response(k,iband) - response(k-1,iband))/((responsedim(1)-responsedim(3) + k * &
                responsedim(3)) - (responsedim(1) - responsedim(3) + (k-1) * responsedim(3)))
        aspect = response(k,iband) - slope * (responsedim(1) - responsedim(3) + k * responsedim(3))
        response_transformed(i,iband) = slope * wl(i) + aspect
      endif
    endif
    
    i = i + 1
    
    if (i .gt. nwl)  goto 100
      
 enddo

100 enddo

return

end subroutine