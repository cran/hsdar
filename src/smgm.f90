subroutine inv_gauss_fit (x, nx, a, y)
implicit none
integer, intent(in) :: nx
integer, intent(in) :: x(nx)
double precision, intent(in)  :: a(3)
double precision, intent(out) :: y(nx)

integer i
double precision :: z, ez

if (abs(a(2)) .gt. 0.000001) then
  do i = 1, nx
    z = dble(x(i) - 2800)/a(3)
    ez = exp(-1.0*(z * z)/2.0)  
    y(i) = a(2)*ez + a(1)
  enddo
else
  do i = 1, nx
    y(i) = a(1)
  enddo
endif

return
end subroutine

subroutine smgm_lsa (x, y, nx, a, gridsize, rmse_val)
implicit none
integer nx, gridsize
integer x(nx)
double precision y(nx), a(3), a_b(3), y_est(nx)
double precision tol, diff, rmse_val, rmse_val_b
integer max_run, n_run

max_run = 100
tol = 0.0001
diff = 1.0

do n_run = 1, nx
  y(n_run) = -1.0*(y(n_run) - 1.0)
enddo

a(1) = 0.0
a(2) = 1.0*y(nx)
a(3) = 0.0

a_b(1) = 0.0
a_b(2) = 1.0
a_b(3) = 1000

call inv_gauss_fit(x, nx, a, y_est)
call rmse_function(y_est, y, nx, rmse_val) 

rmse_val_b = rmse_val * 2.0

n_run = 0
do while (diff .gt. tol) 
  n_run = n_run + 1
  
  call grid_search(x, nx, a, a_b, y, rmse_val, gridsize)

  diff = abs(rmse_val - rmse_val_b)
  rmse_val_b = rmse_val
  
  if (n_run .ge. max_run) diff = -1.0
end do

return
end subroutine

subroutine grid_search (x, nx, a_new, a_old, y, min_rmse, gridsize)
implicit none
integer, intent(in)  :: gridsize
integer, intent(in)  :: nx
integer, intent(in)  :: x(nx)
double precision, intent(inout)  :: a_new(3), a_old(3), min_rmse
double precision, intent(in) :: y(nx)

integer :: i, k, min_pos(2)
double precision :: rmse_val

double precision, allocatable :: grid(:,:), y_est(:), a(:), a_nd(:), a_rd(:)

allocate(grid(gridsize+1,gridsize+1))
allocate(y_est(nx))
allocate(a(3))
allocate(a_nd(gridsize+1))
allocate(a_rd(gridsize+1))

a_nd(1) = a_new(2)
a_rd(1) = a_new(3)

do i = 1, nx
  y_est(i) = dble(i)
enddo

do i = 1, gridsize
  a_nd(i+1) = a_new(2) - (a_new(2) - a_old(2))/dble(gridsize)*dble(i)
  a_rd(i+1) = a_new(3) - (a_new(3) - a_old(3))/dble(gridsize)*dble(i)
enddo

rmse_val = 2.0
a(1) = 0.0

do i = 1, gridsize + 1
  a(2) = a_nd(i)
  do k = 1, gridsize + 1
    a(3) = a_rd(k)
    call inv_gauss_fit(x, nx, a, y_est)
    call rmse_function(y_est, y, nx, rmse_val)
    grid(i,k) = rmse_val
    
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

a_new(2) = a_nd(min_pos(1))
a_new(3) = a_rd(min_pos(2))

min_rmse = 100000000000.0
do i = -1, 1
  do k = -1, 1
    if (.not.(i .eq. 0 .and. k .eq. 0)) then
      if ((min_pos(1)+i) .gt. 0 .and. (min_pos(1)+i) .lt. (gridsize+2)) then
        if ((min_pos(2)+k) .gt. 0 .and. (min_pos(2)+k) .lt. (gridsize+2)) then
          if (min_rmse .gt. grid(min_pos(1)+i,min_pos(2)+k)) then
            min_rmse = grid(min_pos(1)+i,min_pos(2)+k)
            a_old(2) = a_nd(min_pos(1)+i)
            a_old(3) = a_rd(min_pos(2)+k)
          endif
        endif
      endif
    endif
  enddo
enddo

! call inv_gauss_fit(x, nx, a_new, y_est)

min_rmse = grid(min_pos(1),min_pos(2))

deallocate(grid)
deallocate(y_est)
deallocate(a)
deallocate(a_nd)
deallocate(a_rd)

! call intpr("c", 1, gridsize, 1)
return
end subroutine

subroutine rmse_function (x, y, n, rmse_val)
implicit none
integer, intent(in) :: n
double precision, intent(in)  :: x(n), y(n)
double precision, intent(out) :: rmse_val

integer :: i
double precision :: sum_m, xq

sum_m = 0.0
do i = 1, n
  xq = x(i)-y(i)
  xq = xq*xq
  sum_m = sum_m + xq
enddo
if (sum_m .gt. 0) then
  rmse_val = sqrt(sum_m/dble(n))
else
  rmse_val = 0.0
endif
return
end subroutine
