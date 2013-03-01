! ----------------------------------------------------------------
! file: mettest1.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created March  1, 2013 by William A. Perkins
! Last Change: Thu Jun  3 06:45:08 2010 by William A. Perkins <d3g096@PE10900.pnl.gov>
! ----------------------------------------------------------------
PROGRAM mettest1
  
  USE met_time_series

  IMPLICIT NONE

  CHARACTER (LEN=80), SAVE :: rcsid = "$Id: tstest1.f90 18 2003-04-14 17:48:14Z perk $"
  
  TYPE (met_time_series_rec), POINTER :: metts
  CHARACTER (LEN=1024) :: dstr
  DOUBLE PRECISION :: t, deltat = 0.25d0*HOURS
  INTEGER :: i, steps, step

  CALL date_time_flags()
  CALL time_series_module_init(debug=10, limit = TS_LIMIT_NONE)

  metts => met_time_series_read('mettest1.dat')

  t = metts%ts%series(metts%ts%length)%time - metts%ts%series(1)%time
  steps = INT(t/deltat + 0.5)

  DO step = 0, steps
     t = metts%ts%series(1)%time + step*deltat
     CALL met_time_series_update(metts, t)
     dstr = ''
     CALL date_format(t, dstr)
     WRITE(*,'(A, 1X, 15F17.8)') TRIM(dstr), &
          &(metts%current(i), i = 1, met_fields)
  END DO

  CALL met_time_series_destroy(metts)

  CALL time_series_module_done()

END PROGRAM mettest1
