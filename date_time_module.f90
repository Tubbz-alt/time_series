!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME:  MASS2 module file
!
! VERSION and DATE: 0.22 4-17-98
!
! PURPOSE:  header file for MASS2 model
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS: sets up the bc's for each block. note that block connections are also included here.
!
! MOD HISTORY: 4-1-98 allocatable arrays, pointers
!
!
!***************************************************************
!
MODULE date_time

IMPLICIT NONE

TYPE datetime_struct

    CHARACTER (LEN=10) :: date_string
    CHARACTER (LEN=8)    :: time_string
    DOUBLE PRECISION :: time

END TYPE datetime_struct

DOUBLE PRECISION, PARAMETER, PUBLIC :: &
     &SECONDS = 1.0/86400.0, &
     &MINUTES = 1.0/1440.0, &
     &HOURS = 1.0/24.0, &
     &DAYS = 1.0

CHARACTER, PRIVATE :: dsep = '-', tsep = ':'
CHARACTER (LEN=80), PRIVATE :: dfmt, tfmt
LOGICAL, PRIVATE :: realsec = .FALSE.

!#########################################################################
CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE date_time_init
  ! ----------------------------------------------------------------
  SUBROUTINE date_time_init(datesep, timesep, ydigits, sdigits)

    IMPLICIT NONE

    CHARACTER, INTENT(IN), OPTIONAL :: datesep, timesep
    INTEGER, INTENT(IN), OPTIONAL :: ydigits, sdigits

    IF (PRESENT(datesep)) dsep = datesep
    IF (PRESENT(timesep)) tsep = timesep

    IF (PRESENT(ydigits)) THEN
       WRITE (dfmt, '(1HI,I1.1,1H.,I1.1)') ydigits, ydigits
    ELSE
       dfmt = "I4.4"
    END IF

    dfmt = '(I2.2,1H' // dsep // ',I2.2,1H' // dsep // ',' // TRIM(dfmt) // ')'

    IF (PRESENT(sdigits)) THEN
       WRITE (tfmt, '(3HF0., I1.1,)') sdigits
       realsec = .TRUE.
    ELSE
       tfmt = 'I2.2'
       realsec = .FALSE.
    END IF

    tfmt = '(I2.2,1H' //tsep // ',I2.2,1H' // tsep // "," // TRIM(tfmt) // ')'

  END SUBROUTINE date_time_init



!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME: date_to_decimal
!
! VERSION and DATE: MASS2 
!
! PURPOSE: coverts a date/time string to decimal julian days
!
! RETURNS: decimal date
!
! REQUIRED: IMSL routine NDAYS
!
! LOCAL VARIABLES:
!
! COMMENTS: some luking precision problems that round things off
!   to seconds if a non-rational time increment is used.
!   eg. 1.333333. may be related to interaction with C++B
!
!
!
! MOD HISTORY:
!
!
!***************************************************************
!

DOUBLE PRECISION FUNCTION date_to_decimal(date_string, time_string)

  USE JULIAN

  CHARACTER (LEN=*) :: date_string
  CHARACTER (LEN=*)  :: time_string

  INTEGER :: i0, i1, mon,dd,yr,hh,mm
  DOUBLE PRECISION :: ss
  CHARACTER (LEN=20) :: buf

  date_to_decimal = 0.0

  buf = date_string
  i1 = SCAN(buf, dsep)
  IF (i1 .EQ. 0) RETURN
  READ (buf(:i1-1), *, ERR=100) mon
  IF (mon .GT. 12 .OR. mon .LT. 1) RETURN

  buf = buf(i1+1:)
  i1 = SCAN(buf, dsep)
  IF (i1 .EQ. 0) RETURN
  READ (buf(:i1-1), *, ERR=100) dd
  IF (dd .GT. 31 .OR. dd .LT. 1) RETURN

  buf = buf(i1+1:)
  READ (buf, *, ERR=100) yr

  buf = time_string
  i1 = SCAN(buf, tsep)
  IF (i1 .EQ. 0) RETURN
  READ (time_string(:i1-1), *, ERR=100) hh
  IF (hh .GT. 23 .OR. hh .LT. 0) RETURN

                                ! seconds are optional, so handle the
                                ! case when the second delimiter is
                                ! not there
  buf = buf(i1+1:)
  i1 = SCAN(buf, tsep)
  IF (i1 .EQ. 0) THEN
     READ (buf, *, ERR=100) mm
     ss = 0.0
  ELSE
     READ (buf(:i1-1), *, ERR=100) mm
  
     buf = buf(i1+1:)
     READ (buf, *, ERR=100) ss
  END IF
  IF (mm .GT. 59 .OR. mm .LT. 0) RETURN
  IF (ss .GT. 60.0 .OR. ss .LT. 0.0) RETURN

  date_to_decimal = juldays(mon, dd, yr, hh, mm, DBLE(ss))

100 RETURN

END FUNCTION date_to_decimal


!***************************************************************
!            Pacific Northwest National Laboratory
!***************************************************************
!
! NAME: decimal_to_date
!
! VERSION and DATE: MASS2 
!
! PURPOSE: coverts a decimal date to a date/time string
!
! RETURNS: time_string & time_string
!
! REQUIRED: Uses IMSL routine NDYIN
!
! LOCAL VARIABLES:
!
! COMMENTS:
!
!
! MOD HISTORY:
!
!
!***************************************************************
!

SUBROUTINE decimal_to_date(decimal_date, date_string, time_string)

  USE JULIAN
  IMPLICIT NONE

  DOUBLE PRECISION, INTENT(IN) :: decimal_date
  CHARACTER (LEN=*), INTENT(INOUT) :: date_string
  CHARACTER (LEN=*), INTENT(INOUT) :: time_string

  DOUBLE PRECISION :: sec
  INTEGER :: mon, day, yr, hr, min
  CHARACTER (LEN=1024) :: fmt
  INTEGER :: ylen 

  CALL CALCDATE(decimal_date,mon,day,yr,hr,min,sec)
  
  WRITE(date_string, dfmt) mon, day, yr
  IF (realsec) THEN
     WRITE(time_string, tfmt) hr, min, sec
  ELSE 
     WRITE(time_string, tfmt) hr, min, INT(sec)
  END IF

END SUBROUTINE decimal_to_date

! ----------------------------------------------------------------
! SUBROUTINE date_format
! ----------------------------------------------------------------
SUBROUTINE date_format(time, s)

  USE JULIAN
  IMPLICIT NONE
  DOUBLE PRECISION, INTENT(IN) :: time
  CHARACTER (LEN=*), INTENT(OUT) :: s
  
  DOUBLE PRECISION :: sec
  INTEGER :: mon, day, yr, hr, min
  CHARACTER (LEN=20) :: ds, ts

  CALL decimal_to_date(time, ds, ts)

  WRITE(s, *) TRIM(ds) // " " // trim(ts)

END SUBROUTINE date_format


END MODULE date_time

