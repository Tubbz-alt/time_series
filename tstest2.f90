! ----------------------------------------------------------------
! file: tstest2.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created October 24, 2002 by William A. Perkins
! Last Change: Thu Oct 24 11:11:27 2002 by William A. Perkins <perk@leechong.pnl.gov>
! ----------------------------------------------------------------

PROGRAM tstest

  USE time_series

  IMPLICIT NONE

  CHARACTER (LEN=80), SAVE :: rcsid = "$Id$"
  
  TYPE (time_series_rec), POINTER :: ts
  DOUBLE PRECISION :: t
  INTEGER :: i

  CALL time_series_module_init(10, 10, &
       &mode = TS_REAL_MODE, limit = TS_LIMIT_FLAT, debug = 15)

  ts => time_series_read('tstest2.dat', fields = 2)

  DO t = 10.0*DAYS, 15.0*DAYS + 0.1*SECONDS, 6.0*HOURS
     CALL time_series_interp(ts, t)
     WRITE(*,100) t, (ts%current(i), i = 1, ts%fields)
  END DO
  CALL time_series_module_done()
  
100 FORMAT(3(F10.3, 1X))
END PROGRAM tstest
