! ********************************************************************************
! main.f90
! ********************************************************************************
!
! Jean-Baptiste FERET
! 
! Department of Global Ecology / Carnegie Institution for Sciences
! 260 Panama Street
! Stanford, CA 94305, USA
! E-mail: jbferet@stanford.edu
!
! Stéphane JACQUEMOUD
!
! Université Paris Diderot / Institut de Physique du Globe de Paris
! 35 rue Hélène Brion
! 75013 Paris, France
! E-mail: jacquemoud@ipgp.fr
!
! http://teledetection.ipgp.fr/prosail/
!
! ********************************************************************************
! version 5.02 (26 July 2011)
! ********************************************************************************

subroutine prospect2r (N,Cab,Car,Cbrown,Cw,Cm,RT2R)

use dataSpec_P5B
implicit none

integer(4) :: ii
double precision :: N,Cab,Car,Cbrown,Cw,Cm
double precision, allocatable, save :: RT(:,:)
double precision, intent(out) :: RT2R(nw,2)


allocate (RT(nw,2))

! N      = 1.2		! structure coefficient
! Cab    = 30.		! chlorophyll content (µg.cm-2) 
! Car    = 10.		! carotenoid content (µg.cm-2)
! Cbrown = 0.0		! brown pigment content (arbitrary units)
! Cw     = 0.015	! EWT (cm)
! Cm     = 0.009	! LMA (g.cm-2)


call prospect_5B(N,Cab,Car,Cbrown,Cw,Cm,RT)

RT2R = RT
deallocate(RT)
return
end subroutine prospect2r
