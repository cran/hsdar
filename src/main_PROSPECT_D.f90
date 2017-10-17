! ********************************************************************************
! main.f90
! ********************************************************************************
! for any question or request, please contact: 
!
! Jean-Baptiste FERET
! UMR-TETIS, IRSTEA Montpellier
! Maison de la Télédétection
! 500 rue Jean-Fracois Breton
! 34093 Montpellier cedex 5
! E-mail: jb.feret@teledetection.fr
!
! Stéphane JACQUEMOUD
! Université Paris Diderot / Institut de Physique du Globe de Paris
! 35 rue Hélène Brion
! 75013 Paris, France
! E-mail: jacquemoud@ipgp.fr
!
! http://teledetection.ipgp.fr/prosail/
!
! ********************************************************************************
! version 6.0 (16 January 2017)
! ********************************************************************************

subroutine prospect2r_d (N,Cab,Car,Anth,Cbrown,Cw,Cm,RT2R)

use dataSpec_PDB
implicit none

integer(4) :: ii
double precision :: N,Cab,Car,Anth,Cbrown,Cw,Cm

double precision, allocatable, save :: RT(:,:)
double precision, intent(out) :: RT2R(nw,2)
character :: tab   = char(9)

allocate (RT(nw,2))

! N      = 1.2		! structure coefficient
! Cab    = 30.		! chlorophyll content (µg.cm-2) 
! Car    = 10.		! carotenoid content (µg.cm-2)
! Anth   = 1.0		! Anthocyanin content (µg.cm-2)
! Cbrown = 0.0		! brown pigment content (arbitrary units)
! Cw     = 0.015	! EWT (cm)
! Cm     = 0.009	! LMA (g.cm-2)

call prospect_DB(N,Cab,Car,Anth,Cbrown,Cw,Cm,RT)

RT2R = RT
deallocate(RT)
return
end
