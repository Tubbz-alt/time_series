# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February  3, 2002 by William A. Perkins
# Last Change: Sun Mar 24 08:39:33 2002 by William A. Perkins <perk@localhost>
# -------------------------------------------------------------


DEBUG = -g
FLAGS = $(DEBUG)
F90 = f95
F90FLAGS = $(FLAGS) -trap=INVALID,DIVBYZERO,OVERFLOW  -B108 -YEXT_NAMES=LCS -YCFRL=1
MOD=mod

COMPILE.f90 = $(F90) $(F90FLAGS) -c $(DEBUG)
LINK.f90 = $(F90) $(LDFLAGS)
RANLIB = ranlib

INCLOC = 
FFLAGS = ${INCLOC} $(FLAGS)
LIBLOC = 
LDFLAGS = ${LIBLOC} $(FLAGS)
LDLIBS = -lU77

LIB = libts.a
SRCS = \
	time_series.f90 \
	julian.f90 \
	date_time_module.f90
OBJS = $(SRCS:%.f90=%.o)
LIBOBJS = ${OBJS:%.o=$(LIB)(%.o)}

${LIB}: ${LIBOBJS}
	-${RANLIB} ${LIB}

clean::
	rm -f ${LIB}
	rm -f *.mod

test: tstest1

tstest1: tstest1.o ${LIB}
	${LINK.f90} -o $@ tstest1.o ${LIB} ${LDLIBS}

clean::
	rm -f tstest1.o
	rm -f tstest1

# dependancies for individual object files

time_series.o: time_series.f90 date_time_module.o
date_time_module.o: date_time_module.f90 julian.o
julian.o: julian.f90

clean:: 
	rm -f $(OBJS)

%.o: %.f90
	${COMPILE.f90} $<


tags: TAGS
TAGS: $(SRCS)
	etags $(SRCS)
clean:: 
	rm -f TAGS

clean::
	rm -f *~ *% ,*

