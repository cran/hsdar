! ********************************************************************************
! tav_abs.f90
! ********************************************************************************
! computation of the average transmittivity at the leaf surface within a given
! solid angle. teta is the incidence solid angle (in radian). The average angle
! that works in most cases is 40deg*pi/180. ref is the refaction index.
! ********************************************************************************
! Stern F. (1964), Transmission of isotropic radiation across an interface between
! two dielectrics, Applied Optics, 3:111-113.
! Allen W.A. (1973), Transmission of isotropic light across a dielectric surface in
! two and three dimensions, Journal of the Optical Society of America, 63:664-666.
! ********************************************************************************
! version 5.02 (25 July 2011)
! ********************************************************************************

subroutine tav_abs_D(theta,nr,tav)

use dataSpec_PDB
implicit none

real(8), intent(in) :: theta, nr(nw)
real(8), intent(out) :: tav(nw)

real(8) pi,rd
real(8) n2(nw),np(nw),nm(nw)
real(8) a(nw),k(nw),sa(nw),b1(nw),b2(nw),b3(nw),b(nw),a3(nw)
real(8) ts(nw),tp(nw),tp1(nw),tp2(nw),tp3(nw),tp4(nw),tp5(nw)


pi	= atan(1.)*4.
rd  = pi/180.
n2  = nr**2.
np  = n2+1.
nm  = n2-1.
a   = (nr+1)*(nr+1.)/2.
k   = -(n2-1)*(n2-1.)/4.
sa  = sin(theta*rd)

if (theta.eq.90.) then
	b1=0.
else
	b1  = dsqrt((sa**2-np/2)*(sa**2-np/2)+k)
endif

b2  = sa**2-np/2
b   = b1-b2
b3  = b**3
a3  = a**3
ts  = (k**2./(6*b3)+k/b-b/2)-(k**2./(6*a3)+k/a-a/2)

tp1 = -2*n2*(b-a)/(np**2)
tp2 = -2*n2*np*dlog(b/a)/(nm**2)
tp3 = n2*(1./b-1./a)/2
tp4 = 16*n2**2.*(n2**2+1)*dlog((2*np*b-nm**2)/(2*np*a-nm**2))/(np**3.*nm**2)
tp5 = 16*n2**3.*(1./(2*np*b-nm**2)-1./(2*np*a-nm**2))/(np**3)
tp  = tp1+tp2+tp3+tp4+tp5
tav = (ts+tp)/(2*sa**2)

return
end
