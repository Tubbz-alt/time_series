! ----------------------------------------------------------------
! file: tstest.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created March 16, 2002 by William A. Perkins
! Last Change: Thu Jan 23 12:55:57 2003 by William A. Perkins <perk@leechong.pnl.gov>
! ----------------------------------------------------------------

PROGRAM tstest

  USE time_series

  IMPLICIT NONE

  CHARACTER (LEN=80), SAVE :: rcsid = "$Id$"
  
  TYPE (time_series_rec), POINTER :: ts
  CHARACTER (LEN=1024) :: dstr
  DOUBLE PRECISION :: t
  INTEGER :: i

  CALL date_time_init()
  CALL time_series_module_init(debug=10, limit = TS_LIMIT_NONE)

  ts => time_series_read('tstest1.dat', fields = 2)

  DO t = ts%series(1)%time, ts%series(ts%length)%time, 15.0*SECONDS
     CALL time_series_interp(ts, t)
     dstr = ''
     CALL date_format(t, dstr)
     WRITE(*,'(A, 1X, 15F15.6)') TRIM(dstr), t, (ts%current(i), i = 1, ts%fields)
  END DO
  CALL time_series_module_done()
  

END PROGRAM tstest
