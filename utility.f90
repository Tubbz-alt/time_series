! ----------------------------------------------------------------
! file: utility.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created October 21, 2002 by William A. Perkins
! Last Change: Fri Jun 10 15:46:28 2005 by William A. Perkins <perk@McPerk.pnl.gov>
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE utility
! ----------------------------------------------------------------
MODULE utility

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  INTEGER, PUBLIC :: utility_error_iounit=13, utility_status_iounit=14

CONTAINS 

  ! ----------------------------------------------------------------
  ! SUBROUTINE error_message
  ! ----------------------------------------------------------------
  SUBROUTINE error_message(msg, fatal)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN) :: msg
    LOGICAL, INTENT(IN), OPTIONAL :: fatal
    CHARACTER (LEN=1024) :: buffer

    LOGICAL :: die = .FALSE.

    IF (PRESENT(fatal)) die = fatal

    IF (die) THEN
       buffer = "FATAL ERROR: " // msg
    ELSE 
       buffer = "ERROR: " // msg
    END IF

    WRITE(utility_error_iounit, *) TRIM(buffer)
    WRITE(*,*) TRIM(buffer)

    IF (die) CALL EXIT(1)
  END SUBROUTINE error_message

  ! ----------------------------------------------------------------
  ! SUBROUTINE status_message
  ! ----------------------------------------------------------------
  SUBROUTINE status_message(msg)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN) :: msg

    WRITE(utility_status_iounit, *) TRIM(msg)

  END SUBROUTINE status_message



  ! ----------------------------------------------------------------
  ! SUBROUTINE open_existing
  ! ----------------------------------------------------------------
  SUBROUTINE open_existing(fname, iunit, fatal, result)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN) :: fname
    INTEGER, INTENT(IN) :: iunit
    LOGICAL, INTENT(IN), OPTIONAL :: fatal
    LOGICAL, INTENT(OUT), OPTIONAL :: result

    LOGICAL :: file_exist, myfatal = .TRUE., myresult = .FALSE.
    INTEGER :: status

    CHARACTER (LEN=1024) :: msg

    file_exist = .TRUE.

    IF (PRESENT(fatal)) myfatal = fatal

    INQUIRE(FILE=TRIM(fname), EXIST=file_exist)
    IF(file_exist)THEN
       OPEN(iunit, file=fname, action='READ', iostat=status)
       IF (status .EQ. 0) THEN
          CALL status_message('Opened ' // TRIM(fname) // ' for reading')
          IF (PRESENT(result)) result = .TRUE.
          RETURN
       END IF
       WRITE(msg, *) TRIM(fname), ': cannot open for reading: ', status
       CALL error_message(msg, fatal=myfatal)
    ELSE
       WRITE(msg, *) TRIM(fname), ': cannot open for reading: file does not exist'
       CALL error_message(msg, fatal=myfatal)
    ENDIF
    IF (PRESENT(result)) result = .FALSE.
  END SUBROUTINE open_existing

  ! ----------------------------------------------------------------
  ! SUBROUTINE open_new
  ! ----------------------------------------------------------------
  SUBROUTINE open_new(fname, iunit)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN) :: fname
    INTEGER, INTENT(IN) :: iunit

    INTEGER :: status

    OPEN(unit=iunit, file=fname, action='WRITE', iostat=status)
    IF (status .EQ. 0) THEN
       CALL status_message('Opened ' // TRIM(fname) // ' for writing')
       RETURN
    END IF
    CALL error_message(TRIM(fname) // ': cannot open for writing', fatal=.TRUE.)

  END SUBROUTINE open_new



END MODULE utility
