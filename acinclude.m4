dnl -------------------------------------------------------------
dnl AC_PROG_F90
dnl -------------------------------------------------------------
AC_DEFUN(AC_PROG_F90,
[AC_REQUIRE([AC_CANONICAL_HOST])
if test -n "$F90"; then
    F77="$F90"
fi
if test -z "$F77"; then
    AC_MSG_CHECKING(Building Fortran 90 compiler list for $host)
    case $host in
        i?86*linux*)
            ac_f90_compilers="f95 ifc lf95 f90"
            ;;
        alpha*linux*|alphaev*-dec-osf*)
            ac_f90_compilers="f90 f95 fort"
            ;;
        mips-*-irix*)
            ac_f90_compilers="f90"
            ;;
        *)
            AC_MSG_ERROR(Cannot compile for unknown system $host)
    esac
    AC_MSG_RESULT($ac_f90_compilers)
    # AC_PROG_F77
    AC_CHECK_PROGS(F77, $ac_f90_compilers)
    test -z "$F77" && AC_MSG_ERROR([no acceptable Fortran 90 compiler found in \$PATH])
fi

AC_PROG_F90_FLAGS

AC_PROG_F77_WORKS

])

dnl -------------------------------------------------------------
dnl AC_PROG_F90_FLAGS
dnl This sets the following variables and AC_SUBSTs them:
dnl     F90FLAGS, F90OPTIMIZE, F90DEBUG, F90MODEXT, F90MODPATH, F90PROFILE
dnl     F90LDFLAGS, F90LIBS
dnl -------------------------------------------------------------
AC_DEFUN(AC_PROG_F90_FLAGS,
[AC_REQUIRE([AC_PROG_F90])
AC_SUBST(F90FLAGS)
AC_SUBST(F90LDFLAGS)
AC_SUBST(F90LIBS)
AC_SUBST(F90MODULEEXT)
AC_SUBST(F90DEBUG)
AC_SUBST(F90OPTIMIZE)
AC_SUBST(F90PROFILE)
AC_SUBST(F90MODPATH)

dnl some defaults

F90MODULEEXT="mod"
F90MODPATH="-I"
F90DEBUG="-g"
F90OPTIMIZE="-O"

case $host in
    i?86*linux*)
        case $F77 in
                                # The Absoft compiler
            f95|f90)
                F90FLAGS='-trap=INVALID,DIVBYZERO,OVERFLOW  -YEXT_SFX=_ -YEXT_NAMES=LCS -YCFRL=1'
                F90LDFLAGS=''
                F90LIBS="-lU77"
                F90MODULEEXT=mod
                F90DEBUG="-g"
                F90PROFILE="-g -P"
                F90MODPATH="-p"
                ;;
                                #  The Intel compiler
            ifc)
                F90FLAGS='-static -Vaxlib -w'
                F90LDFLAGS=''
                F90LIBS="-lPEPCF90"
                F90MODULEEXT=d
                F90OPTIMIZE="-tpp7 -O3 -axW -xW"
                F90PROFILE="-p"
                ;;
                                # The Lahey compiler
            lf95)
                F90FLAGS='--nin --trap'
                F90LDFLAGS='--staticlink'
                F90LIBS=""
                F90MODULEEXT=mod
                F90PROFILE=""
                ;;
            *)
                AC_MSG_WARN(Fortran 90 Compiler ($F77) Unknown on $host)
        esac
        ;;
    alpha*linux*|alphaev*-dec-osf*)
                                # The DEC/Compaq/HP compiler on Alpha Platforms
        case $F77 in
            f90|f95|fort)
                F90FLAGS='-fpe'
                F90LDFLAGS='-non_shared'
                F90LIBS=""
                F90MODULEEXT=mod
                F90OPTIMIZE="-O -fast"
                F90PROFILE="-pg"
                ;;
            *)
                AC_MSG_WARN(Fortran 90 Compiler ($F77) Unknown on $host)
        esac
        ;;
    mips-*-irix*)
        case $F77 in
                                # The stock SGI compiler
            f90)
                F90FLAGS='-TARG:platform=IP30  -OPT:Olimit=0'
                F90LDFLAGS=''
                F90LIBS="-lfpe"
                F90PROFILE=""
                ;;
            *)
                AC_MSG_WARN(Fortran 90 Compiler ($F77) Unknown on $host)
        esac
        ;;
    *)
        
esac

])
