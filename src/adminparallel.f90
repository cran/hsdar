subroutine adminparallel (flag, process)
implicit none
integer :: flag, process
integer, save :: process_backup

if (flag .eq. 0) then
  process_backup = -1
elseif (flag .eq. 1) then
  process = process_backup
  if (process_backup .eq. -1) then
    process_backup = 1
  endif  
endif

return
end subroutine