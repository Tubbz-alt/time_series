# -------------------------------------------------------------
# file: Makefile
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February  3, 2002 by William A. Perkins
# Last Change: Sun Feb  3 21:13:27 2002 by William A. Perkins <perk@localhost>
# -------------------------------------------------------------


F90 = f95
COMPILE.f90 = $(F90) $(F90FLAGS) -c $(DEBUG)
LINK.f90 = $(F90) $(LDFLAGS)
RANLIB = ranlib
MOD=mod

FLAGS = 
INCLOC = 
CFLAGS = ${INCLOC} $(FLAGS)
LIBLOC = 
LDFLAGS = ${LIBLOC} $(FLAGS)


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

# dependancies for individual object files

time_series.o: time_series.f90 date_time_module.o
date_time_module.o: date_time_module.f90 julian.o
julian.o: julian.f90

%.o: %.f90
	${COMPILE.f90} $<


tags: TAGS
TAGS: $(SRCS)
	etags $(SRCS)

clean:: 
	rm -f TAGS

clean::
	rm -f *.o *~ *% ,*

