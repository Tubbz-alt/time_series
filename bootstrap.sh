#! /bin/sh
# -------------------------------------------------------------
# file: bootstrap.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 26, 2003 by William A. Perkins
# Last Change: Wed Mar 26 08:42:07 2003 by William A. Perkins <perk@leechong.pnl.gov>
# -------------------------------------------------------------

aclocal -I . && automake --foreign --add-missing --copy && autoconf
