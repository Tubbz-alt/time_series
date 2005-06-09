! ----------------------------------------------------------------
! file: fptraptest.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created April 14, 2004 by William A. Perkins
! Last Change: Wed Apr 14 08:45:07 2004 by William A. Perkins <perk@leechong.pnl.gov>
! ----------------------------------------------------------------

! RCS ID: $Id$ Battelle PNL

PROGRAM fptraptest
  
  USE fptrap

  IMPLICIT NONE 

  DOUBLE PRECISION :: x = 0.0, y

  CALL fptrap_common()

  y = 1.0/x

  WRITE (*, *) 'Should have crashed by now.'

END PROGRAM fptraptest

  
