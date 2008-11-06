dnl -------------------------------------------------------------
dnl AC_PROG_F90
dnl -------------------------------------------------------------
AC_DEFUN(AC_PROG_F90,
[AC_REQUIRE([AC_CANONICAL_HOST])

AC_ARG_VAR(F90, [Fortran 90 compiler command])

AC_ARG_WITH(f90,
    [  --with-f90=cmd              specify the Fortran 90 compiler],
    F90="$withval")

if test -n "$F90"; then
    FC="$F90"
fi
if test -z "$FC"; then
    AC_MSG_CHECKING(Building Fortran 90 compiler list for $host)
    case $host in
        i?86*linux*)
            ac_f90_compilers="f95 ifc ifort lf95 f90"
            ;;
        x86_64*linux*)
            ac_f90_compilers="f95 f90 ifc ifort g95 gfortran"
            ;;
        alpha*linux*|alphaev*-dec-osf*)
            ac_f90_compilers="f90 f95 fort"
            ;;
        mips-*-irix*)
            ac_f90_compilers="f90"
            ;;
        *-apple-darwin*)
            ac_f90_compilers="f95 f90 xlf90 xlf95 g95 gfortran"
            ;;
        *)
            AC_MSG_ERROR(Cannot compile for unknown system $host)
    esac
    AC_MSG_RESULT($ac_f90_compilers)
    AC_CHECK_PROGS(FC, $ac_f90_compilers)
    test -z "$FC" && AC_MSG_ERROR([no acceptable Fortran 90 compiler found in \$PATH])
fi

dnl AC_PROG_F77($FC)
AC_PROG_FC($FC)

AC_PROG_F90_FLAGS

AC_LANG(Fortran)
AC_FC_SRCEXT(f90)

FCFLAGS="$FCFLAGS $FCFLAGS_f90"
AC_SUBST(FFLAGS)
FFLAGS="$FCFLAGS"

dnl AC_PROG_F77_WORKS

])

dnl -------------------------------------------------------------
dnl AC_PROG_F90_FLAGS
dnl This sets the following variables and AC_SUBSTs them:
dnl     F90FLAGS, F90OPTIMIZE, F90DEBUG, F90MODEXT, F90MODPATH, F90PROFILE
dnl     F90LDFLAGS, F90LIBS
dnl -------------------------------------------------------------
AC_DEFUN(AC_PROG_F90_FLAGS,
[ dnl AC_REQUIRE([AC_PROG_F90])
AC_SUBST(F90FLAGS)
AC_SUBST(F90LDFLAGS)
AC_SUBST(F90LIBS)
AC_SUBST(F90MODULEEXT)
AC_SUBST(F90DEBUG)
AC_SUBST(F90OPTIMIZE)
AC_SUBST(F90PROFILE)
AC_SUBST(F90MODPATH)
AC_SUBST(F90TRAPOBJ)

dnl Some command line options

AC_ARG_VAR(F90FLAGS, [Fortran 90 compiler flags])

AC_ARG_ENABLE(f90-optimize,
  [  --enable-f90-optimize        turn on optimization])

AC_ARG_ENABLE(f90-debug,
  [  --enable-f90-debug           turn on debugging])

AC_ARG_ENABLE(f90-profile,
  [  --enable-f90-profile         turn on profiling])

AC_ARG_WITH(extra-f90flags,
    [  --with-extra-f90flags=flags  use additional compiler flags ],
    F90FLAGS="$F90FLAGS $withval")

dnl some defaults

F90MODULEEXT="mod"
F90MODPATH="-I"
F90DEBUG="-g"
F90OPTIMIZE="-O"
F90TRAPOBJ="fptrap-null.o"

case $host in
    i?64*linux*)
        case $FC in
                                #  The Intel compiler (64-bit)
            efc)
                F90FLAGS="$F90FLAGS -static -Vaxlib -w"
                F90LDFLAGS=""
                F90LIBS="-lPEPCF90"
                F90MODULEEXT=mod
                F90OPTIMIZE="-O3"
                F90PROFILE="-p"
                ;;
            *)
                AC_MSG_WARN(Fortran 90 Compiler ($FC) Unknown on $host)
        esac
        ;;
    i?86*linux*)
        case $FC in
                                # The Absoft compiler
            f95|f90)
                F90FLAGS="$F90FLAGS -trap=INVALID,DIVBYZERO,OVERFLOW  -YEXT_SFX=_ -YEXT_NAMES=LCS -YCFRL=1"
                F90LDFLAGS=""
                F90LIBS="-lU77"
                F90MODULEEXT=mod
                F90DEBUG="-g"
                F90PROFILE="-g -P"
                F90MODPATH="-p"
                ;;
                                #  The Intel compiler
            ifc|ifort)
                F90FLAGS="$F90FLAGS -static -Vaxlib -w"
                F90LDFLAGS=""
                F90LIBS=""
                F90MODULEEXT=mod
                F90OPTIMIZE="-O3"
                F90PROFILE="-p"
                F90TRAPOBJ="fptrap-intel.o"
                ;;
                                # The Lahey compiler
            lf95)
                F90FLAGS="$F90FLAGS --nin --trap"
                F90LDFLAGS="--staticlink"
                F90LIBS=""
                F90MODULEEXT=mod
                F90PROFILE=""
                ;;
            *)
                AC_MSG_WARN(Fortran 90 Compiler ($FC) Unknown on $host)
        esac
        ;;
    x86_64*linux*)
        case $FC in
                                # The Absoft compiler
            f95|f90)
                F90FLAGS="$F90FLAGS -trap=INVALID,DIVBYZERO,OVERFLOW  -YEXT_SFX=_ -YEXT_NAMES=LCS -YCFRL=1"
                F90LDFLAGS=""
                F90LIBS="-lU77"
                F90MODULEEXT=mod
                F90DEBUG="-g"
                F90PROFILE="-g -P"
                F90MODPATH="-p"
                ;;
            ifc|ifort)
                F90FLAGS="$F90FLAGS -static -Vaxlib -w"
                F90LDFLAGS=""
                F90LIBS=""
                F90MODULEEXT=mod
                F90OPTIMIZE="-O3"
                F90PROFILE="-p"
                F90TRAPOBJ="fptrap-intel.o"
                ;;
            g95|gfortran)
                F90FLAGS="$F90FLAGS -fno-second-underscore"
                F90LDFLAGS=""
                F90LIBS=""
                F90MODULEEXT=mod
                F90DEBUG="-g"
                F90PROFILE="-g -P"
                F90MODPATH="-I"
                ;;
            *)
                AC_MSG_WARN(Fortran 90 Compiler ($FC) Unknown on $host)
        esac
        ;;
    alpha*linux*|alphaev*-dec-osf*)
                                # The DEC/Compaq/HP compiler on Alpha Platforms
        case $FC in
            f90|f95|fort)
                F90FLAGS="$F90FLAGS -fpe"
                F90LDFLAGS="-non_shared"
                F90LIBS=""
                F90MODULEEXT=mod
                F90OPTIMIZE="-O -fast"
                F90PROFILE="-pg"
                ;;
            *)
                AC_MSG_WARN(Fortran 90 Compiler ($FC) Unknown on $host)
        esac
        ;;
    mips-*-irix*)
        case $FC in
                                # The stock SGI compiler
            f90)
                F90FLAGS="$F90FLAGS -TARG:platform=IP30  -OPT:Olimit=0"
                F90LDFLAGS=""
                F90LIBS="-lfpe"
                F90PROFILE=""
                ;;
            *)
                AC_MSG_WARN(Fortran 90 Compiler ($FC) Unknown on $host)
        esac
        ;;
    *-apple-darwin*)
                                # The Absoft compiler
        case $FC in
            f90|f95)
                F90FLAGS="$F90FLAGS -trap=INVALID,DIVBYZERO,OVERFLOW  -YEXT_SFX=_ -YEXT_NAMES=LCS -YCFRL=1 -N11"
                F90LDFLAGS=""
                F90LIBS="-lU77"
                F90MODULEEXT=mod
                F90DEBUG="-g"
                F90PROFILE="-g -P"
                F90MODPATH="-p"
                ;;
            xlf90|xlf95|xlf)
                F90FLAGS="$F90FLAGS -qflttrap=ov:zero:inv:enable -qextname"
                F90LDFLAGS=""
                F90LIBS=""
                F90MODULEEXT=mod
                F90DEBUG="-g"
                F90PROFILE="-g -P"
                F90MODPATH="-I"
                ;;
            g95|gfortran)
                F90FLAGS="$F90FLAGS -fno-second-underscore"
                F90LDFLAGS=""
                F90LIBS=""
                F90MODULEEXT=mod
                F90DEBUG="-g"
                F90PROFILE="-g -P"
                F90MODPATH="-I"
                ;;
        esac
        ;;                
    *)
        
esac


FCFLAGS="$FCFLAGS $F90FLAGS"
if test "$enable_f90_optimize" = "yes"; then
    FCFLAGS="$FCFLAGS $F90OPTIMIZE"
fi
if test "$enable_f90_debug" = "yes"; then
    FCFLAGS="$FCFLAGS $F90DEBUG"
fi
if test "$enable_f90_profile" = "yes"; then
    FCFLAGS="$FCFLAGS $F90PROFILE"
fi

LDFLAGS="$LDFLAGS $F90LDFLAGS"
LIBS="$LIBS $F90LIBS"

])
