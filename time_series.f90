! ----------------------------------------------------------------
! file: time_series.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created January 21, 2002 by William A. Perkins
! Last Change: Sun Feb  3 07:53:23 2002 by William A. Perkins <perk@localhost>
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE time_series
! ----------------------------------------------------------------
MODULE time_series

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  INTEGER, PRIVATE, PARAMETER :: max_fields = 10
  DOUBLE PRECISION, PRIVATE, PARAMETER :: bogus = -9999.0

  ! ----------------------------------------------------------------
  ! TYPE time_series_point
  ! stores a single time with multiple values
  ! ----------------------------------------------------------------
  TYPE time_series_point
     DOUBLE PRECISION :: datetime
     CHARACTER (LEN=10) :: cdate
     CHARACTER (LEN=8) :: ctime
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

                                ! to avoid repeatedly searching the
                                ! time series linked list, this array is
                                ! mapped into the list

  TYPE (time_series_rec), PRIVATE, ALLOCATABLE :: tslistmap(:)
  
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
  ! SUBROUTINE time_series_init
  ! ----------------------------------------------------------------
  SUBROUTINE time_series_init(lu, eu, vrb)

    IMPLICIT NONE

    INTEGER, OPTIONAL, INTENT(IN) :: lu, eu
    LOGICAL, OPTIONAL, INTENT(IN) :: vrb

    INTEGER :: logunit, errunit
    LOGICAL :: verbose

    logunit = -1
    errunit = -1
    verbose = .FALSE.

    IF (PRESENT(lu)) logunit = lu
    IF (PRESENT(eu)) errunit = eu
    IF (PRESENT(vrb)) verbose = vrb

    NULLIFY(tslast)
    NULLIFY(tshead)
    nextid = 1

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
  SUBROUTINE time_series_error(msg, fatal)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN) :: msg
    LOGICAL, INTENT(IN), OPTIONAL :: fatal

    CHARACTER (LEN=1024) :: s
    LOGICAL :: myfatal

    myfatal = .FALSE.
    IF (PRESENT(fatal)) myfatal = fatal

    WRITE (s, *) 'ERROR: time series: ', TRIM(msg)
    IF (fatal) s = 'FATAL ' // s

    WRITE (*,*) TRIM(s), TRIM(msg)
    IF (logunit .GT. 0)  WRITE (logunit,*) TRIM(s)
    IF (errunit .GT. 0)  WRITE (errunit,*) TRIM(s)

    IF (fatal) CALL exit(10)

  END SUBROUTINE time_series_error
  
  ! ----------------------------------------------------------------
  ! SUBROUTINE time_series_log
  ! ----------------------------------------------------------------
  SUBROUTINE time_series_log(ts, msg)

    IMPLICIT NONE

    TYPE (time_series_rec), POINTER :: ts
    CHARACTER (LEN=*), INTENT(IN) :: msg

    CHARACTER (LEN=1024) :: s, n

    WRITE (n, *) ts%id
    WRITE (s, *) 'STATUS: time series ', TRIM(n), ': ', TRIM(ts%filename), &
         &': ', TRIM(msg)

    WRITE (*,*) TRIM(s), TRIM(msg)
    IF (logunit .GT. 0)  WRITE (logunit,*) TRIM(s), TRIM(msg)

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

    pt%datetime = 0.0
    pt%cdate = ''
    pt%ctime = ''
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
  ! LOGICAL FUNCTION read_time_series_pt
  ! ----------------------------------------------------------------
  LOGICAL FUNCTION read_time_series_pt(iounit)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: iounit
  
    read_time_series_pt = .FALSE.
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



END MODULE time_series
