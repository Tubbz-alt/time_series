! ----------------------------------------------------------------
! file: time_series.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created January 21, 2002 by William A. Perkins
! Last Change: Sun Mar 24 08:29:06 2002 by William A. Perkins <perk@localhost>
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE time_series
! ----------------------------------------------------------------
MODULE time_series

  USE date_time

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  INTEGER, PRIVATE, PARAMETER :: max_fields = 10
  DOUBLE PRECISION, PRIVATE, PARAMETER :: bogus = -9999.0

  CHARACTER (LEN=1024), PRIVATE :: buf, buf1

  ! ----------------------------------------------------------------
  ! TYPE time_series_point
  ! stores a single time with multiple values
  ! ----------------------------------------------------------------
  TYPE time_series_point
     DOUBLE PRECISION :: time
     DOUBLE PRECISION, POINTER :: field(:)
  END TYPE time_series_point

  ! ----------------------------------------------------------------
  ! TYPE time_series_rec
  ! ----------------------------------------------------------------
  TYPE time_series_rec
     INTEGER :: id
     CHARACTER (LEN=1024) :: filename
     INTEGER :: fields, length
     INTEGER :: start
     TYPE (time_series_point), POINTER :: series(:)
     DOUBLE PRECISION, POINTER :: current(:)
  END TYPE time_series_rec

                                ! mode determines interpolated value
                                ! when outside the series range

  INTEGER, PUBLIC, PARAMETER :: &
       &TS_LIMIT_NONE = 0, &    ! do not allow time outside the series range
       &TS_LIMIT_FLAT = 1, &    ! flatline when outside the series range
       &TS_LIMIT_EXTRAP = 2     ! extrapolate when outside the series range

  INTEGER, PRIVATE :: limit_mode 

                                ! mode determines the form dates take
                                ! in the input file

  INTEGER, PUBLIC, PARAMETER :: &
       &TS_DATE_MODE = 0, &     ! dates are expected
       &TS_REAL_MODE = 1        ! real numbers are expected

  INTEGER, PRIVATE :: time_series_mode

                                ! each time series is assigned an
                                ! integer id
  INTEGER, PRIVATE :: nextid

                                ! the module can be initialized to
                                ! report error and status messages to
                                ! an open file unit

  LOGICAL, PRIVATE :: verbose
  INTEGER, PRIVATE :: logunit, errunit

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE time_series_error
  ! ----------------------------------------------------------------
  SUBROUTINE time_series_error(msg, fatal, ts)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN) :: msg
    LOGICAL, INTENT(IN), OPTIONAL :: fatal
    TYPE (time_series_rec), POINTER, OPTIONAL :: ts

    LOGICAL :: myfatal

    myfatal = .FALSE.
    IF (PRESENT(fatal)) myfatal = fatal

    IF (PRESENT(ts)) THEN
       WRITE (buf1, *) ts%id
       WRITE (buf, *) 'ERROR: time series ', TRIM(buf1), ' (', TRIM(ts%filename), &
            &'): ', TRIM(msg)
    ELSE
       WRITE (buf, *) 'ERROR: time series: ', TRIM(msg)
    END IF
    IF (myfatal) buf = 'FATAL ' // buf

    WRITE (*,*) TRIM(buf)
    IF (logunit .GT. 0)  WRITE (logunit,*) TRIM(buf)
    IF (errunit .GT. 0)  WRITE (errunit,*) TRIM(buf)

    IF (myfatal) CALL exit(10)

  END SUBROUTINE time_series_error
  
  ! ----------------------------------------------------------------
  ! SUBROUTINE time_series_log
  ! ----------------------------------------------------------------
  SUBROUTINE time_series_log(msg, ts)

    IMPLICIT NONE

    TYPE (time_series_rec), POINTER, OPTIONAL :: ts
    CHARACTER (LEN=*), INTENT(IN) :: msg

    IF (logunit .LE. 0) RETURN

    IF (PRESENT(ts)) THEN
       WRITE (buf1, *) ts%id
       WRITE (buf, *) 'STATUS: time series ', TRIM(buf1), ': ', TRIM(ts%filename), &
            &': ', TRIM(msg)
    ELSE
       WRITE (buf, *) 'STATUS: time series: ', TRIM(msg)
    END IF

    WRITE (logunit,*) TRIM(buf)

  END SUBROUTINE time_series_log

  ! ----------------------------------------------------------------
  ! SUBROUTINE time_series_module_init
  ! ----------------------------------------------------------------
  SUBROUTINE time_series_module_init(log, err, verb, limit, mode)

    IMPLICIT NONE

    INTEGER, OPTIONAL, INTENT(IN) :: log, err, limit, mode
    LOGICAL, OPTIONAL, INTENT(IN) :: verb

    logunit = -1
    errunit = -1
    verbose = .FALSE.
    limit_mode = TS_LIMIT_NONE
    time_series_mode = TS_DATE_MODE

    IF (PRESENT(log)) logunit = log
    IF (PRESENT(err)) errunit = err
    IF (PRESENT(verb))  verbose = verb

    IF (PRESENT(limit)) THEN
       SELECT CASE (limit)
       CASE (TS_LIMIT_NONE, TS_LIMIT_FLAT, TS_LIMIT_EXTRAP)
          limit_mode = limit
       CASE DEFAULT
          WRITE (buf, *) 'Invalid limit mode: ', limit
          CALL time_series_error(buf, fatal = .TRUE.)
       END SELECT
    END IF

    IF (PRESENT(mode)) THEN
       SELECT CASE (mode)
       CASE (TS_DATE_MODE, TS_REAL_MODE)
          time_series_mode = mode
       CASE DEFAULT
          WRITE (buf, *) 'Invalid date/time format mode: ', mode
          CALL time_series_error(buf, fatal = .TRUE.)
       END SELECT
    END IF

    nextid = 1

    CALL time_series_log("module initialized")
    IF (verbose) THEN
       buf = 'limit mode: '
       SELECT CASE (limit_mode)
       CASE (TS_LIMIT_NONE)
          buf = TRIM(buf) // 'Times not allowed outside series'
       CASE (TS_LIMIT_FLAT)
          buf = TRIM(buf) // 'Flat line for times outside series'
       CASE (TS_LIMIT_EXTRAP)
          buf = TRIM(buf) // 'Extrapolate for times outside series'
       CASE DEFAULT
          WRITE (buf, *) 'limit mode not understood ', limit_mode
       END SELECT
       CALL time_series_log(buf)

       buf = 'date/time format: '
       SELECT CASE (time_series_mode)
       CASE (TS_DATE_MODE)
          buf = TRIM(buf) // 'Calendar dates'
       CASE (TS_REAL_MODE)
          buf = TRIM(buf) // 'Real numbers'
       CASE DEFAULT
          WRITE (buf, *) 'date/time format mode not understood ', time_series_mode
       END SELECT
    END IF

  END SUBROUTINE time_series_module_init

  ! ----------------------------------------------------------------
  ! SUBROUTINE time_series_module_done
  ! clean up anything that needs to be cleaned up
  ! ----------------------------------------------------------------
  SUBROUTINE time_series_module_done()

    IMPLICIT NONE

    CALL time_series_log("module finished")

  END SUBROUTINE time_series_module_done


  ! ----------------------------------------------------------------
  ! SUBROUTINE time_series_point_init
  ! ----------------------------------------------------------------
  SUBROUTINE time_series_point_init(pt, fields)

    IMPLICIT NONE
    TYPE (time_series_point) :: pt
    INTEGER, INTENT(IN), OPTIONAL :: fields

    INTEGER :: nfld

    nfld = 1
    IF (PRESENT(fields)) nfld = fields

    pt%time = 0.0
    ALLOCATE(pt%field(fields))
    pt%field = bogus

  END SUBROUTINE time_series_point_init

  ! ----------------------------------------------------------------
  ! TYPE (TIME_SERIES_REC) FUNCTION time_series_alloc
  ! ----------------------------------------------------------------
  TYPE (TIME_SERIES_REC) FUNCTION time_series_alloc(id, fields, length) RESULT (ts)

    IMPLICIT NONE
    
    POINTER ts
    INTEGER, INTENT(IN) :: length, id, fields
    INTEGER :: i

    ALLOCATE(ts)
    ts%filename = ''
    ts%length = length
    ts%fields = fields
    ts%start = 1
    ts%id = id
    ALLOCATE(ts%current(fields))
    ts%current = 0.0
    ALLOCATE(ts%series(length))

    DO i = 1, length
       CALL time_series_point_init(ts%series(i), fields)
    END DO
       
    IF (verbose) THEN
       WRITE(buf, *) 'created with ', fields, ' fields and ', length, ' times'
    END IF
    
  END FUNCTION time_series_alloc

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION time_series_point_read
  ! This routine reads a single time series point from the specified
  ! file.  It returns the number of fields read, zero if end of file
  ! occurs, -1 if a date error occurs, or -2 if any other error occurs.
  ! ----------------------------------------------------------------
  INTEGER FUNCTION time_series_point_read(iounit, pt) RESULT (nfld)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iounit
    TYPE (time_series_point), INTENT(INOUT), OPTIONAL :: pt

    CHARACTER (LEN=10) :: sdate
    CHARACTER (LEN=8) :: stime
    DOUBLE PRECISION :: x(max_fields), datetime

    nfld = 0
    x = bogus
    READ(iounit, *, END=100, ERR=200) sdate, stime, x

                                ! convert date/time to number
                                ! check to make sure it's valid

    datetime =  date_to_decimal(sdate, stime)

    IF (datetime .EQ. 0.0) THEN
       nfld = -1 
       RETURN
    END IF
    
                                ! count the number of fields read, and
                                ! make sure it is correct

    DO WHILE (x(nfld+1) .NE. bogus) 
       nfld = nfld + 1
    END DO

                                ! if there is someplace to put it,
                                ! pass on the data read

    IF (PRESENT(pt)) THEN
       pt%time = datetime
       pt%field(1:nfld) = x(1:nfld)
    END IF
    RETURN
200 nfld = -2
100 RETURN
  END FUNCTION time_series_point_read

  
  ! ----------------------------------------------------------------
  ! TYPE (time_series_rec) FUNCTION read_time_series
  ! ----------------------------------------------------------------
  TYPE (time_series_rec)  FUNCTION time_series_read(filename, fields, id)

    IMPLICIT NONE

    POINTER time_series_read

    CHARACTER (LEN=*) :: filename
    INTEGER, INTENT(IN), OPTIONAL :: fields
    INTEGER, INTENT(IN), OPTIONAL :: id

    LOGICAL :: exists
    INTEGER :: myfld, myid, length, istat, iounit, ierr, nfld, i

    IF (.NOT. PRESENT(fields)) THEN
       myfld = 1
    ELSE
       myfld = fields
    END IF
    IF (.NOT. PRESENT(id)) THEN
       myid = nextid
       nextid = nextid + 1
    ELSE 
       myid = id
    END IF

    INQUIRE(FILE=filename, EXIST=exists)
    IF (.NOT. exists) THEN 
       WRITE (buf,*) TRIM(filename), ': file does not exist'
       CALL time_series_error(buf, fatal = .TRUE.)
    END IF

                                ! open the file and see how many
                                ! points are in it

    OPEN(UNIT=iounit, FILE=filename, STATUS='old', IOSTAT=istat, FORM='formatted')
    IF (istat .NE. 0) THEN
       WRITE (buf,*) 'cannot open file ', TRIM(filename)
       CALL time_series_error(buf, fatal = .TRUE.)
    END IF

    READ (iounit, *) buf       ! throw away first line in file

    length = 0
    ierr = 0
    i = 1
    DO WHILE (.TRUE.)
       nfld = time_series_point_read(iounit)
       i = i + 1
       IF (nfld .GE. fields) THEN
          length = length + 1
       ELSE IF (nfld .EQ. 0) THEN
          EXIT
       ELSE
          WRITE (buf, *) TRIM(filename), ': line ', i, &
               &': fields read = ', nfld, ' (expected ', fields, ')'
          CALL time_series_error(buf)
          ierr = ierr + 1
       END IF
    END DO

    IF (ierr .GT. 0) THEN
       WRITE (buf, *) TRIM(filename), ': too many errors'
       CALL time_series_error(buf, fatal = .TRUE.)
    END IF

                                ! now that we know how many points we
                                ! have, allocate the time series
                                ! record

    time_series_read => time_series_alloc(myid, myfld, length)
    time_series_read%filename = filename

    REWIND(iounit)

    READ (iounit, *) buf       ! throw away first line in file

    ierr = 0
    DO i = 1, time_series_read%length
       nfld = time_series_point_read(iounit, time_series_read%series(i))
       IF (nfld .LT. time_series_read%fields) THEN
          WRITE (buf, *) 'unexpected error, line ', i, '(', nfld, ')'
          CALL time_series_error(buf, ts = time_series_read, fatal = .TRUE.)
       END IF
    END DO

    CLOSE (iounit)
    WRITE (buf, *) 'successfully read ', time_series_read%length , ' points'
    CALL time_series_log(buf, ts = time_series_read)

    IF (verbose) THEN
       CALL date_format(time_series_read%series(1)%time, buf)
       WRITE(buf, *) 'start = ', TRIM(buf)
       CALL time_series_log(buf, ts = time_series_read)
       CALL date_format(time_series_read%series(time_series_read%length )%time, buf)
       WRITE(buf, *) 'end = ', TRIM(buf)
       CALL time_series_log(buf, ts = time_series_read)
    END IF
  END FUNCTION time_series_read

  ! ----------------------------------------------------------------
  ! SUBROUTINE time_series_interp
  ! ----------------------------------------------------------------
  SUBROUTINE time_series_interp(ts, datetime)

    IMPLICIT NONE

    TYPE (time_series_rec), POINTER :: ts
    DOUBLE PRECISION, INTENT(IN) :: datetime

    CHARACTER (LEN=1024) :: dstr
    INTEGER :: i
    DOUBLE PRECISION :: factor

    IF (datetime .LT. ts%series(1)%time) THEN
       SELECT CASE (limit_mode)
       CASE (TS_LIMIT_NONE)
          CALL date_format(datetime, dstr)
          WRITE (buf,*) 'date (', TRIM(dstr), ' out of range'
          CALL time_series_error(buf, ts = ts, fatal = .TRUE.)
       CASE (TS_LIMIT_FLAT)
          ts%current = ts%series(1)%field
          RETURN
       CASE (TS_LIMIT_EXTRAP)
          i = 1
       END SELECT
    ELSE IF (datetime .GT. ts%series(ts%length)%time) THEN
       SELECT CASE (limit_mode)
       CASE (TS_LIMIT_NONE)
          CALL date_format(datetime, dstr)
          WRITE (buf,*) 'date (', TRIM(dstr), ' out of range'
          CALL time_series_error(buf, ts = ts, fatal = .TRUE.)
       CASE (TS_LIMIT_FLAT)
          ts%current = ts%series(ts%length)%field
          RETURN
       CASE (TS_LIMIT_EXTRAP)
          i = ts%length - 1
       END SELECT
    ELSE
       i = ts%start
       DO i = ts%start, ts%length - 1
          IF (datetime .GE. ts%series(i)%time .AND.&
               &datetime .LE. ts%series(i+1)%time) EXIT
       END DO
       IF (i .GT. 1) ts%start = i - 1
    END IF

    factor = (datetime - ts%series(i)%time)/&
         &(ts%series(i + 1)%time - ts%series(i)%time)
    ts%current = factor*(ts%series(i+1)%field - ts%series(i)%field) +&
         &ts%series(i)%field

  END SUBROUTINE time_series_interp


END MODULE time_series
