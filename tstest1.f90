! ----------------------------------------------------------------
! file: tstest.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created March 16, 2002 by William A. Perkins
! Last Change: Sat Mar 23 14:49:25 2002 by William A. Perkins <perk@localhost>
! ----------------------------------------------------------------

PROGRAM tstest

  USE time_series

  IMPLICIT NONE

  CHARACTER (LEN=80), SAVE :: rcsid = "$Id$"
  
  TYPE (time_series_rec), POINTER :: ts
  CHARACTER (LEN=1024) :: dstr
  DOUBLE PRECISION :: t
  INTEGER :: i

  CALL time_series_module_init(10, 10, verb = .TRUE., limit = TS_LIMIT_NONE)

  ts => time_series_read('tstest1.dat', fields = 2)

  DO t = ts%series(1)%time, ts%series(ts%length)%time, 1.0/24.0
     CALL time_series_interp(ts, t)
     dstr = ''
     CALL date_format(t, dstr)
     WRITE(*,*) TRIM(dstr), (ts%current(i), i = 1, ts%fields)
  END DO
  CALL time_series_module_done()
  

END PROGRAM tstest
