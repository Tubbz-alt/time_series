! ----------------------------------------------------------------
! file: datetest2.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created March 31, 2003 by William A. Perkins
! Last Change: Mon Mar 31 07:06:47 2003 by William A. Perkins <perk@leechong.pnl.gov>
! ----------------------------------------------------------------
PROGRAM datetest2

  USE date_time

  IMPLICIT NONE

  DOUBLE PRECISION :: start, end, time, deltat = 18.0*HOURS
  INTEGER :: steps, i
  CHARACTER (LEN=80) :: buf

  CALL date_time_flags(ydigits=5, sdigits=1)

  start = date_to_decimal('01-01-1944', '00:00:00')
  end = date_to_decimal('12-31-13100', '00:00:00')
  steps = INT((end - start)/deltat)

  DO i = 1, steps
     time = start + deltat*DBLE(i)
     CALL date_format(time, buf)
     WRITE(*,*) TRIM(buf)
  END DO
END PROGRAM datetest2
