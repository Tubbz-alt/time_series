! ----------------------------------------------------------------
! file: utility.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created October 21, 2002 by William A. Perkins
! Last Change: Thu Jan 23 13:33:54 2003 by William A. Perkins <perk@leechong.pnl.gov>
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE utility
! ----------------------------------------------------------------
MODULE utility

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

  INTEGER, PARAMETER, PUBLIC :: error_iounit=13, status_iounit=14

CONTAINS 

  ! ----------------------------------------------------------------
  ! SUBROUTINE error_message
  ! ----------------------------------------------------------------
  SUBROUTINE error_message(msg, fatal)

    IMPLICIT NONE

    CHARACTER*(*), INTENT(IN) :: msg
    LOGICAL, INTENT(IN), OPTIONAL :: fatal

    LOGICAL :: die = .FALSE.

    IF (PRESENT(fatal)) die = fatal

    WRITE(*,*) TRIM(msg)
    WRITE(error_iounit, *) TRIM(msg)

    IF (die) CALL EXIT(1)

  END SUBROUTINE error_message

  ! ----------------------------------------------------------------
  ! SUBROUTINE status_message
  ! ----------------------------------------------------------------
  SUBROUTINE status_message(msg)

    IMPLICIT NONE

    CHARACTER*(*), INTENT(IN) :: msg

    WRITE(status_iounit, *) TRIM(msg)

  END SUBROUTINE status_message



  ! ----------------------------------------------------------------
  ! SUBROUTINE open_existing
  ! ----------------------------------------------------------------
  SUBROUTINE open_existing(fname, iunit)

    IMPLICIT NONE

    CHARACTER*(*), INTENT(IN) :: fname
    INTEGER, INTENT(IN) :: iunit

    LOGICAL :: file_exist

    INQUIRE(FILE=fname, EXIST=file_exist)

    IF(file_exist)THEN
       OPEN(iunit,file=fname)
       CALL status_message('Opened ' // TRIM(fname) // ' for reading')
    ELSE
       CALL error_message(TRIM(fname) // ': cannot open for reading', fatal=.TRUE.)
    ENDIF
  END SUBROUTINE open_existing


END MODULE utility
