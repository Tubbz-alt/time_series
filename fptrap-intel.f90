! ----------------------------------------------------------------
! file: fptrap-intel.f90
!
! This module is used to turn on floating point exception handling
! when using the Intel Fortran Compiler (7.0+).  It is modeled after a
! message posted to the comp.lang.fortran newsgroup by Will Henney
! (w.henney@astrosmo.unam.mx) on 2003-11-24 13:22:17 PST.
!
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created April 14, 2004 by William A. Perkins
! Last Change: Wed Apr 14 08:32:56 2004 by William A. Perkins <perk@leechong.pnl.gov>
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL
! ----------------------------------------------------------------
! MODULE fptrap
! ----------------------------------------------------------------
MODULE fptrap

  USE iflport, ONLY: getcontrolfpqq, setcontrolfpqq, &
       &FPCW$INVALID, FPCW$ZERODIVIDE, FPCW$OVERFLOW
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

    newc = FPCW$INVALID
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

9000 FORMAT ('fptrap_common (intel): current FP control word: ', Z4)
9001 FORMAT ('fptrap_common (intel): updated FP control word: ', Z4)
  END SUBROUTINE fptrap_common


END MODULE fptrap
