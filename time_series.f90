! ----------------------------------------------------------------
! file: time_series.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created January 21, 2002 by William A. Perkins
! Last Change: Sun Feb  3 21:16:15 2002 by William A. Perkins <perk@localhost>
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

  ! ----------------------------------------------------------------
  ! TYPE time_series_point
  ! stores a single time with multiple values
  ! ----------------------------------------------------------------
  TYPE time_series_point
     TYPE (datetime_struct) :: datetime
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
     LOGICAL :: dodatetime
     TYPE (time_series_point), POINTER :: series(:)
     TYPE (time_series_rec), POINTER :: next
  END TYPE time_series_rec

                                ! all of the time series used in the
                                ! program are stored in a singly
                                ! linked list.  tshead points to the
                                ! head of the list; tslast points to
                                ! the end of the list

  TYPE (time_series_rec), POINTER, PRIVATE :: tshead, tslast

!!$                                ! to avoid repeatedly searching the
!!$                                ! time series linked list, this array is
!!$                                ! mapped into the list
!!$
!!$  TYPE (time_series_rec), POINTER, PRIVATE, ALLOCATABLE :: tslistmap(:)
  
                                ! each time series is assigned an
                                ! integer id
  INTEGER, PRIVATE :: nextid

                                ! some flags to define the mode of operation:

  LOGICAL, PRIVATE :: doextra   ! extrapolate: supply values outside the date range

  LOGICAL, PRIVATE :: doflat    ! if doextra, supply flat-lined values
                                ! outside the date range

                                ! the module can be initialized to
                                ! report error and status messages to
                                ! an open file unit

  LOGICAL, PRIVATE :: verbose
  INTEGER, PRIVATE :: logunit, errunit

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE time_series_init
  ! ----------------------------------------------------------------
  SUBROUTINE time_series_init(log, err, verb, extrap, flat)

    IMPLICIT NONE

    INTEGER, OPTIONAL, INTENT(IN) :: log, err
    LOGICAL, OPTIONAL, INTENT(IN) :: verb, extrap, flat

    logunit = -1
    errunit = -1
    verbose = .FALSE.
    doextra = .FALSE.
    doflat = .FALSE.

    IF (PRESENT(log)) logunit = log
    IF (PRESENT(err)) errunit = err
    IF (PRESENT(verb))  verbose = verb
    IF (PRESENT(extrap)) doextra = extrap
    IF (PRESENT(flat)) doflat = flat

    NULLIFY(tslast)
    NULLIFY(tshead)
    nextid = 1

    CALL time_series_log("module initialized")

  END SUBROUTINE time_series_init

  ! ----------------------------------------------------------------
  ! SUBROUTINE time_series_done
  ! ----------------------------------------------------------------
  SUBROUTINE time_series_done()

    IMPLICIT NONE

  

  END SUBROUTINE time_series_done


  ! ----------------------------------------------------------------
  ! SUBROUTINE time_series_error
  ! ----------------------------------------------------------------
  SUBROUTINE time_series_error(msg, fatal, ts)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN) :: msg
    LOGICAL, INTENT(IN), OPTIONAL :: fatal
    TYPE (time_series_rec), POINTER, OPTIONAL :: ts

    CHARACTER (LEN=1024) :: s,n
    LOGICAL :: myfatal

    myfatal = .FALSE.
    IF (PRESENT(fatal)) myfatal = fatal

    IF (PRESENT(ts)) THEN
       WRITE (n, *) ts%id
       WRITE (s, *) 'ERROR: time series ', TRIM(n), ' (', TRIM(ts%filename), &
            &'): ', TRIM(msg)
    ELSE
       WRITE (s, *) 'ERROR: time series: ', TRIM(msg)
    END IF
    IF (fatal) s = 'FATAL ' // s

    WRITE (*,*) TRIM(s)
    IF (logunit .GT. 0)  WRITE (logunit,*) TRIM(s)
    IF (errunit .GT. 0)  WRITE (errunit,*) TRIM(s)

    IF (fatal) CALL exit(10)

  END SUBROUTINE time_series_error
  
  ! ----------------------------------------------------------------
  ! SUBROUTINE time_series_log
  ! ----------------------------------------------------------------
  SUBROUTINE time_series_log(msg, ts)

    IMPLICIT NONE

    TYPE (time_series_rec), POINTER, OPTIONAL :: ts
    CHARACTER (LEN=*), INTENT(IN) :: msg

    CHARACTER (LEN=1024) :: s, n

    IF (logunit .LE. 0) RETURN

    IF (PRESENT(ts)) THEN
       WRITE (n, *) ts%id
       WRITE (s, *) 'STATUS: time series ', TRIM(n), ': ', TRIM(ts%filename), &
            &': ', TRIM(msg)
    ELSE
       WRITE (s, *) 'STATUS: time series: ', TRIM(msg)
    END IF

    WRITE (logunit,*) TRIM(s)

  END SUBROUTINE time_series_log
  

  ! ----------------------------------------------------------------
  ! SUBROUTINE init_time_series_point
  ! ----------------------------------------------------------------
  SUBROUTINE init_time_series_point(pt, fields)

    IMPLICIT NONE
    TYPE (time_series_point) :: pt
    INTEGER, INTENT(IN), OPTIONAL :: fields

    INTEGER :: nfld

    nfld = 1
    IF (PRESENT(fields)) nfld = fields

    pt%datetime%time = 0.0
    pt%datetime%date_string = ''
    pt%datetime%time_string = ''
    ALLOCATE(pt%field(fields))
    pt%field = bogus

  END SUBROUTINE init_time_series_point

  ! ----------------------------------------------------------------
  ! TYPE (TIME_SERIES_REC) FUNCTION alloc_time_series_rec
  ! ----------------------------------------------------------------
  TYPE (TIME_SERIES_REC) FUNCTION alloc_time_series_rec(id, fields, length)

    IMPLICIT NONE
    
    POINTER alloc_time_series_rec
    INTEGER, INTENT(IN) :: length, id, fields
    INTEGER :: i

    ALLOCATE(alloc_time_series_rec)
    alloc_time_series_rec%filename = ''
    alloc_time_series_rec%length = length
    alloc_time_series_rec%fields = fields
    alloc_time_series_rec%start = 1
    alloc_time_series_rec%id = id
    ALLOCATE(alloc_time_series_rec%series(length))
    NULLIFY(alloc_time_series_rec%next)

    DO i = 1, length
       CALL init_time_series_point(alloc_time_series_rec%series(i), fields)
    END DO
       
    
  END FUNCTION alloc_time_series_rec

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION add_time_series
  ! ----------------------------------------------------------------
  INTEGER FUNCTION add_time_series(length, fields, id)

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: length, id, fields

    IF (.NOT. ASSOCIATED(tshead)) THEN
       tshead = alloc_time_series_rec(id, fields, length)
       tslast = tshead
    ELSE 
       tslast%next = alloc_time_series_rec(id, fields, length)
       tslast = tslast%next
    END IF

    add_time_series = tslast%id
  END FUNCTION add_time_series

  ! ----------------------------------------------------------------
  ! TYPE (time_series_rec) FUNCTION time_series_by_id
  ! ----------------------------------------------------------------
  RECURSIVE FUNCTION time_series_by_id(id, list) RESULT (ts)

    IMPLICIT NONE

    TYPE (time_series_rec), POINTER :: ts
    INTEGER, INTENT(IN) :: id
    TYPE (time_series_rec), POINTER, OPTIONAL :: list
    
    TYPE (time_series_rec), POINTER :: l

    IF (PRESENT(list)) THEN
       l = list
    ELSE
       l = tshead
    END IF

    NULLIFY(ts)

    IF (id .EQ. l%id) THEN
       ts = l
    ELSE IF (ASSOCIATED(l%next)) THEN
       ts = time_series_by_id(id, l%next)
    END IF
  END FUNCTION time_series_by_id


  ! ----------------------------------------------------------------
  ! LOGICAL FUNCTION read_time_series_pt
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION read_time_series_pt(iounit, pt)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iounit
    TYPE (time_series_point), INTENT(INOUT), OPTIONAL :: pt

    CHARACTER (LEN=80) :: sdate, stime
    DOUBLE PRECISION :: x(max_fields), datetime
    
    read_time_series_pt = .FALSE.

    x = bogus
    READ(iounit, END=100) sdate, stime, x
                                ! convert date/time to number
                                ! check to make sure it's valid
    IF (PRESENT(pt)) THEN
    END IF
100 RETURN
  END FUNCTION read_time_series_pt

  
  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION read_time_series
  ! ----------------------------------------------------------------
  INTEGER FUNCTION read_time_series(filename, fields, id)

    IMPLICIT NONE

    CHARACTER (LEN=*) :: filename
    INTEGER, INTENT(IN), OPTIONAL :: fields
    INTEGER, INTENT(IN), OPTIONAL :: id

    LOGICAL :: exists
    INTEGER :: myfld, myid, length, istat, iounit
    CHARACTER (LEN=1024) :: msg

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
                                ! check to make sure id does not exist
    END IF

    INQUIRE(FILE=filename, EXIST=exists)
    IF (.NOT. exists) THEN 
       WRITE (msg,*) TRIM(filename), ': file does not exist'
       CALL time_series_error(msg, fatal = .TRUE.)
    END IF

                                ! open the file and see how many
                                ! points are in it

    OPEN(UNIT=iounit, FILE=filename, STATUS='old', IOSTAT=istat, FORM='formatted')
    IF (istat .NE. 0) THEN
       WRITE (msg,*) 'cannot open file ', TRIM(filename)
       CALL time_series_error(msg, fatal = .TRUE.)
    END IF

    length = 0
    DO WHILE (read_time_series_pt(iounit)) 
       length = length + 1
    END DO

    REWIND(iounit)


    read_time_series = myid
  END FUNCTION read_time_series

  ! ----------------------------------------------------------------
  ! SUBROUTINE time_series_interp
  ! ----------------------------------------------------------------
  SUBROUTINE time_series_interp(id, datetime, y)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: id
    DOUBLE PRECISION, INTENT(IN) :: datetime
    DOUBLE PRECISION, INTENT(OUT) :: y(:)

    TYPE (time_series_rec), POINTER :: ts
    CHARACTER (LEN=1024) :: msg
    INTEGER :: i

    ts = time_series_by_id(id)

    IF (.NOT. ASSOCIATED(ts)) THEN
       WRITE(msg,*) 'time_series_interp: unknown time series id: ', id
       CALL time_series_error(msg, fatal = .TRUE.)
    END IF
    
    i = ts%start
    DO i = ts%start, ts%length - 1
       IF (datetime .GE. ts%series(i)%datetime%time .AND.&
            &datetime .LE. ts%series(i+1)%datetime%time) EXIT
    END DO

  END SUBROUTINE time_series_interp


END MODULE time_series
