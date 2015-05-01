subroutine inv_gauss_fit (x, nx, a, y)
implicit none
integer nx, i
integer x(nx)
double precision a(3), z(nx), ez(nx), y(nx)

if (a(2) .ne. 0.0) then
  z = dble(x - 2800)/a(3)
  ez = exp(-1.0*(z**2)/2.0)  
else
  z = 100.
  ez = 0.0
endif

y = a(2)*ez + a(1)

return
end subroutine

subroutine smgm_lsa (x, y, nx, a, gridsize, rmse_val)
implicit none
integer nx, gridsize
integer x(nx)
double precision y(nx), y_est(nx), a(3), a_b(3), y_est_b(nx)
double precision tol, diff, rmse, rmse_val, rmse_val_b
integer max_run, n_run

max_run = 100
tol = 0.0001
diff = 1.0

y = -1.0*(y - 1.0)

a(1) = 0.0
a(2) = 1.0*y(nx)
a(3) = 0.0

a_b(1) = 0.0
a_b(2) = 1.0
a_b(3) = 1000

call inv_gauss_fit(x, nx, a, y_est)
rmse_val = rmse(y_est, y, nx) 
rmse_val_b = rmse_val * 2.0

n_run = 0
do while (diff .gt. tol) 
  n_run = n_run + 1
  
  call grid_search(x, nx, a, a_b, y, rmse_val, y_est, gridsize)

  diff = abs(rmse_val - rmse_val_b)
  rmse_val_b = rmse_val
  
  if (n_run .ge. max_run) diff = -1.0
end do

return
end subroutine

double precision function rmse (x, y, n)
implicit none
integer n, i
double precision x(n), y(n), m, xq(n)

m = 0.0
xq = (x-y)**2
do i = 1, n
  m = m + xq(i)
enddo
rmse = (m/dble(n))**0.5

return
end function



subroutine grid_search (x, nx, a_new, a_old, y, min_rmse, y_est, gridsize)
implicit none
integer gridsize
integer nx
integer x(nx)
double precision a_new(3), a_old(3), y(nx), a(3) 
double precision grid(gridsize+1,gridsize+1), a2(gridsize+1), a3(gridsize+1)
double precision y_est(nx), rmse, min_rmse

integer i, k, min_pos(2)

a2 = a_new(2)
a3 = a_new(3)

do i = 1, gridsize
  a2(i+1) = a_new(2) - (a_new(2) - a_old(2))/dble(gridsize)*dble(i)
  a3(i+1) = a_new(3) - (a_new(3) - a_old(3))/dble(gridsize)*dble(i)
enddo

do i = 1, gridsize + 1
  a(2) = a2(i)
  do k = 1, gridsize + 1
    a(3) = a3(k)
    call inv_gauss_fit(x, nx, a, y_est)
    grid(i,k) = rmse(y_est, y, nx)
    
  enddo
enddo

min_pos = 1
min_rmse = grid(1,1)
do i = 1, gridsize + 1
  do k = 1, gridsize + 1
    if (min_rmse .gt. grid(i,k)) then
      min_rmse = grid(i,k)
      min_pos(1) = i
      min_pos(2) = k
    endif
  enddo
enddo

a_new(2) = a2(min_pos(1))
a_new(3) = a3(min_pos(2))

min_rmse = 100000000000.0
do i = -1, 1
  do k = -1, 1
    if (.not.(i .eq. 0 .and. k .eq. 0)) then
      if ((min_pos(1)+i) .gt. 0 .and. (min_pos(1)+i) .lt. (gridsize+2)) then
        if ((min_pos(2)+k) .gt. 0 .and. (min_pos(2)+k) .lt. (gridsize+2)) then
          if (min_rmse .gt. grid(min_pos(1)+i,min_pos(2)+k)) then
            min_rmse = grid(min_pos(1)+i,min_pos(2)+k)
            a_old(2) = a2(min_pos(1)+i)
            a_old(3) = a3(min_pos(2)+k)
          endif
        endif
      endif
    endif
  enddo
enddo

call inv_gauss_fit(x, nx, a_new, y_est)

min_rmse = grid(min_pos(1),min_pos(2))
return
end subroutine