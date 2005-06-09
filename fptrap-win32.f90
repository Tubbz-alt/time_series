! ----------------------------------------------------------------
! file: fptrap-win32.f90
!
! This module is used to turn on floating point exception handling
! when using the Digitial Visual Fortran Compiler.
!
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created April 14, 2004 by William A. Perkins
! Last Change: Fri Jun  4 12:54:13 2004 by William A. Perkins <d3g096@r101243.pnl.gov>
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL
! ----------------------------------------------------------------
! MODULE fptrap
! ----------------------------------------------------------------
MODULE fptrap

  USE dflib
  USE utility, ONLY: status_message

  IMPLICIT NONE

  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE fptrap_common
  ! ----------------------------------------------------------------
  SUBROUTINE fptrap_common()

    IMPLICIT NONE

    INTEGER(2) :: oldc, newc
    CHARACTER (LEN=1024) :: msg

    newc = #0000
    newc = IOR(newc, FPCW$INVALID)
    newc = IOR(newc, FPCW$ZERODIVIDE)
    newc = IOR(newc, FPCW$OVERFLOW)
    newc = NOT(newc)

    CALL getcontrolfpqq(oldc)

    WRITE (msg, 9000) oldc 
    CALL status_message(msg)

    oldc = IAND(oldc, newc)

    CALL setcontrolfpqq(oldc)

    CALL getcontrolfpqq(oldc)

    WRITE (msg, 9000) oldc 
    CALL status_message(msg)

9000 FORMAT ('fptrap_common (win32): current FP control word: ', Z4)
9001 FORMAT ('fptrap_common (win32): updated FP control word: ', Z4)
  END SUBROUTINE fptrap_common


END MODULE fptrap
