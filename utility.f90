! ----------------------------------------------------------------
! file: utility.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created October 21, 2002 by William A. Perkins
! Last Change: Tue Apr 15 09:58:38 2003 by William A. Perkins <perk@leechong.pnl.gov>
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE utility
! ----------------------------------------------------------------
MODULE utility

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  INTEGER, PARAMETER, PUBLIC :: utility_error_iounit=13, utility_status_iounit=14

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
  SUBROUTINE open_existing(fname, iunit)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN) :: fname
    INTEGER, INTENT(IN) :: iunit

    LOGICAL :: file_exist
    INTEGER :: status

    INQUIRE(FILE=fname, EXIST=file_exist)

    IF(file_exist)THEN
       OPEN(iunit, file=fname, action='READ', iostat=status)
       IF (status .EQ. 0) THEN
          CALL status_message('Opened ' // TRIM(fname) // ' for reading')
          RETURN
       END IF
    ENDIF
    CALL error_message(TRIM(fname) // ': cannot open for reading', fatal=.TRUE.)
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
